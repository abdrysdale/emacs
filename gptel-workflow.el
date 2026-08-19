;;; gptel-workflow.el --- Declarative tool orchestration for gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Alex Drysdale

;; Author: Alex Drysdale <alexander.drysdale@wales.nhs.uk>
;; Created: 19 Aug 2026
;; Version: 0.1
;; Keywords: ai gptel tools workflow orchestration
;; X-URL: https://github.com/abdrysdale/emacs

;;; Commentary:
;;
;; A `workflow' tool for gptel that lets the LLM specify a pipeline of
;; tool calls with conditional branching — without writing arbitrary
;; code.  The model writes a JSON spec; elisp interprets it.
;;
;; Security model:
;;
;; 1. The workflow can ONLY call tools that are in the buffer-local
;;    `gptel-tools' (the active tool list).  It resolves tool names the
;;    same way `gptel--handle-tool-use' does — by searching
;;    `gptel-tools' with `gptel-tool-name'.  A tool not in the active
;;    list is rejected.
;;
;; 2. Per-tool `:confirm' flags are RESPECTED, not bypassed.  If any
;;    step references a tool with a non-nil `:confirm' slot, the
;;    workflow REFUSES to execute that step and returns a warning.
;;    The model must call confirmed tools directly so the user sees
;;    each one individually.  This prevents a tool like `web-search'
;;    (which sends data to the network) from being hidden inside a
;;    large JSON pipeline that was approved as a single block.
;;
;; 3. No `eval', no arbitrary elisp, no shell injection.  The model
;;    controls only: which active tool to call, what args to pass,
;;    and which step to branch to next.
;;
;; The workflow tool itself is registered via `gptel-make-tool' and
;; added to `gptel-tools' like any other tool.

;;; Code:

(require 'gptel)
(require 'json)

;;; --- Internals ---

(defun gptel-workflow--resolve-tool (name)
  "Look up tool named NAME in the active `gptel-tools' list.
Returns the `gptel-tool' struct, or nil if not found.
This mirrors how `gptel--handle-tool-use' resolves tools:
  (cl-find-if (lambda (ts) (equal (gptel-tool-name ts) name))
              gptel-tools)"
  (cl-find-if (lambda (ts) (equal (gptel-tool-name ts) name))
              gptel-tools))

(defun gptel-workflow--check-confirm (tool-spec)
  "Check whether TOOL-SPEC requires individual confirmation.
Returns non-nil if the tool has a non-nil `:confirm' slot.
This means the workflow CANNOT execute it — the user must approve
it as a standalone tool call, not buried inside a workflow JSON."
  (gptel-tool-confirm tool-spec))

(defun gptel-workflow--run-step (step)
  "Execute a single workflow STEP.
STEP is a plist: (:id \"step_id\" :tool \"tool_name\" :args (:key val ...))
Returns a plist: (:successp BOOLEAN :output STRING :step-id STRING)"
  (let* ((step-id (plist-get step :id))
         (tool-name (plist-get step :tool))
         (args (plist-get step :args))
         (tool-spec (gptel-workflow--resolve-tool tool-name)))
    (cond
     ;; Tool not in active list — reject
     ((null tool-spec)
      (list :successp nil
            :output (format "Error: Tool '%s' is not available in the active tool set. Active tools: %s"
                            tool-name
                            (mapconcat #'gptel-tool-name gptel-tools ", "))
            :step-id step-id))
     ;; Tool requires individual confirmation — refuse
     ((gptel-workflow--check-confirm tool-spec)
      (list :successp nil
            :output (format "WARNING: Tool '%s' requires individual user confirmation (:confirm is set). "
                            tool-name
                            (format "It cannot be executed inside a workflow — call '%s' directly "
                                    tool-name)
                            "as a standalone tool call so the user can review it.")
            :step-id step-id))
     ;; Tool found and not confirmed — execute
     (t
      (let ((arg-values
             ;; Map keyword args to positional args (same as gptel does)
             ;; gptel--map-tool-args is in gptel-request.el
             (condition-case nil
                 (progn (require 'gptel-request)
                        (gptel--map-tool-args tool-spec args))
               ;; Fallback: if gptel--map-tool-args isn't available,
               ;; extract values in arg declaration order
               (error
                (let ((arg-specs (gptel-tool-args tool-spec))
                      (vals '()))
                  (dolist (spec arg-specs (nreverse vals))
                    (let ((arg-name (plist-get spec :name)))
                      (push (plist-get args (intern (concat ":" arg-name))) vals))))))))
        ;; Execute the tool function, catching errors
        (condition-case errdata
            (let ((result (apply (gptel-tool-function tool-spec) arg-values)))
              (list :successp t
                    :output (if (stringp result) result (prin1-to-string result))
                    :step-id step-id))
          (error
           (list :successp nil
                 :output (format "Error running tool '%s': %s"
                                 tool-name
                                 (error-message-string errdata))
                 :step-id step-id))))))))

(defun gptel-workflow--eval-condition (condition result)
  "Evaluate CONDITION against the RESULT of a step.
CONDITION is a plist:
  (:on_success \"step_id\" :on_failure \"step_id\")
or:
  (:field \"output\" :regex \"pattern\" :match \"step_id\" :no_match \"step_id\")
RESULT is a plist: (:successp BOOLEAN :output STRING :step-id STRING)
Returns the next step ID, or nil if the workflow should end."
  (cond
   ;; Branch on success/failure
   ((and (plist-get condition :on_success)
         (plist-get condition :on_failure))
    (if (plist-get result :successp)
        (plist-get condition :on_success)
      (plist-get condition :on_failure)))
   ;; Regex match on output field
   ((and (plist-get condition :regex)
         (plist-get condition :field))
    (let* ((field-val (pcase (plist-get condition :field)
                        ("output" (or (plist-get result :output) ""))
                        ("success" (if (plist-get result :successp) "true" "false"))
                        (_ (or (plist-get result :output) ""))))
           (pattern (plist-get condition :regex))
           (matched (condition-case nil
                       (string-match-p pattern field-val)
                     (invalid-regexp nil))))
      (if matched
          (plist-get condition :match)
        (plist-get condition :no_match))))
   ;; Single-branch conditions (only on_success or only on_failure)
   ((plist-get condition :on_success)
    (when (plist-get result :successp)
      (plist-get condition :on_success)))
   ((plist-get condition :on_failure)
    (when (not (plist-get result :successp))
      (plist-get condition :on_failure)))
   ;; No condition — end workflow
   (t nil)))

(defun gptel-workflow--execute (spec)
  "Execute a workflow SPEC (parsed JSON as a plist).
SPEC structure:
  (:steps [(:id \"s1\" :tool \"grep\" :args (:pattern \"TODO\")
            :on_success \"s2\" :on_failure \"s3\")
           (:id \"s2\" :tool \"shell\" :args (:command \"wc -l\")
            :condition (:field \"output\" :regex \"[0-9]+\" :match \"done\" :no_match \"s3\"))
           (:id \"s3\" :tool \"file-tree\" :args (:dir \"src/\"))
           (:id \"done\")])
Returns a summary string of all step results."
  (let* ((steps-raw (append (plist-get spec :steps) nil))  ; vector -> list
         (steps (mapcar (lambda (s) (append s nil)) steps-raw))  ; normalize plists
         (step-map (mapcar (lambda (s) (cons (plist-get s :id) s)) steps))
         (results nil)
         (current-id (and steps (plist-get (car steps) :id)))
         (max-steps (length steps))  ; safety: prevent infinite loops
         (warnings nil))
    ;; Pre-scan: check for any tools with :confirm and collect warnings
    (dolist (step steps)
      (let* ((tool-name (plist-get step :tool))
             (tool-spec (when tool-name (gptel-workflow--resolve-tool tool-name))))
        (when (and tool-spec (gptel-workflow--check-confirm tool-spec))
          (push (format "WARNING: Step '%s' uses tool '%s' which requires individual "
                        (or (plist-get step :id) "?") tool-name)
                warnings))))
    (when warnings
      (push (format "confirmation. These steps will FAIL when reached. Tools with :confirm: %s"
                    (mapconcat #'identity (nreverse warnings) "; "))
            results))
    ;; Execute steps
    (cl-loop repeat (+ max-steps 1)  ; allow one extra for terminal step
             while current-id
             for step = (alist-get current-id step-map nil nil #'equal)
             for iter from 1
             do
             (if (null step)
                 ;; Step references a non-existent ID — end
                 (progn
                   (push (format "Step '%s': not found -- workflow ended" current-id)
                         results)
                   (setq current-id nil))
               ;; Check if this is a terminal step (no :tool field)
               (if (null (plist-get step :tool))
                   (progn
                     (push (format "Step '%s': terminal -- workflow complete" current-id)
                           results)
                     (setq current-id nil))
                 ;; Execute the step
                 (let* ((result (gptel-workflow--run-step step))
                        (step-id (plist-get result :step-id))
                        (successp (plist-get result :successp))
                        (output (plist-get result :output))
                        (truncated (truncate-string-to-width output 500 0 nil t)))
                   ;; Log result
                   (push (format "Step '%s' (%s): %s\n  Output: %s"
                                 step-id (plist-get step :tool)
                                 (if successp "SUCCESS" "FAILED")
                                 truncated)
                         results)
                   ;; Determine next step
                   (let ((condition (plist-get step :condition))
                         (on-success (plist-get step :on_success))
                         (on-failure (plist-get step :on_failure)))
                     (setq current-id
                           (cond
                            ;; Explicit condition (regex matching)
                            ((and condition (plist-get condition :regex))
                             (gptel-workflow--eval-condition condition result))
                            ;; Simple on_success/on_failure branching
                            ((or on-success on-failure)
                             (gptel-workflow--eval-condition
                              `(:on_success ,on-success :on_failure ,on-failure)
                              result))
                            ;; No branching — end workflow
                            (t nil))))))))
    ;; Return results as a string (oldest first)
    (mapconcat #'identity (nreverse results) "\n"))

;;; --- The gptel tool ---

(defun gptel-workflow-run (steps-spec)
  "Run a declarative workflow of tool calls with conditional branching.

STEPS-SPEC is a JSON string (or already-parsed plist) describing the
pipeline.  Each step can reference any tool in the active `gptel-tools'
list — tools not in the active list are rejected.

IMPORTANT: Tools with individual `:confirm' flags (like web-search,
shell, python) CANNOT be executed inside a workflow.  If a step
references such a tool, the step will fail with a warning.  Call
confirmed tools directly as standalone tool calls instead.

Example JSON:
{
  \"steps\": [
    {\"id\": \"search\", \"tool\": \"grep\",
     \"args\": {\"pattern\": \"TODO\", \"dir\": \"src/\"},
     \"on_success\": \"count\", \"on_failure\": \"report\"},
    {\"id\": \"count\", \"tool\": \"file-tree\",
     \"args\": {\"dir\": \"src/\"},
     \"condition\": {\"field\": \"output\", \"regex\": \"[0-9]+\",
                    \"match\": \"done\", \"no_match\": \"report\"}},
    {\"id\": \"report\", \"tool\": \"ls\",
     \"args\": {\"dir\": \"src/\"}},
    {\"id\": \"done\"}
  ]
}

Each step must have:
- id: unique string identifier
- tool: name of a gptel tool in the active tool set

Optional:
- args: object mapping argument names to values
- on_success: step id to run if this step succeeds
- on_failure: step id to run if this step fails
- condition: object with field, regex, match, no_match for regex branching

A step with no :tool field is a terminal marker — the workflow ends."
  (let ((spec (if (stringp steps-spec)
                  (json-parse-string steps-spec
                                     :object-type 'plist
                                     :array-type 'vector
                                     :null-object nil
                                     :false-object nil)
                steps-spec)))
    (gptel-workflow--execute spec)))

;;; --- Registration ---

(gptel-make-tool
 :name "workflow"
 :function #'gptel-workflow-run
 :description
 "Run a multi-step workflow of tool calls with conditional branching.

Use this when you need to chain multiple tool calls where later calls
depend on the success or output of earlier ones.  This avoids multiple
round-trips — all steps execute in one call.

IMPORTANT CONSTRAINTS:
- The workflow can ONLY call tools that are currently active in your
  gptel session.  If a tool is not in the active tool list, the step
  will fail.
- Tools with individual confirmation (those that would prompt the user
  when called directly, e.g. shell, python, web-search) CANNOT be used
  inside a workflow.  If a step references such a tool, it will FAIL
  with a warning.  Call confirmed tools directly as standalone tool
  calls instead.  This is a security measure — it prevents sensitive
  tools from being hidden inside a large JSON pipeline.

STEP SPECIFICATION (JSON string):
{
  \"steps\": [
    {
      \"id\": \"unique_step_id\",
      \"tool\": \"tool_name\",
      \"args\": {\"arg_name\": value},
      \"on_success\": \"next_step_id\",
      \"on_failure\": \"next_step_id\",
      \"condition\": {
        \"field\": \"output\",
        \"regex\": \"pattern\",
        \"match\": \"step_id\",
        \"no_match\": \"step_id\"
      }
    },
    ...
  ]
}

BRANCHING RULES:
- If both on_success and on_failure are set, branch on success/failure.
- If condition is set, evaluate the regex against the specified field.
- on_success/on_failure and condition are mutually exclusive per step.
- A step with no branching fields ends the workflow.
- A step with no \"tool\" field is a terminal marker.

EXAMPLE — search for TODOs, if found list files, otherwise show git log:
{
  \"steps\": [
    {\"id\": \"search\", \"tool\": \"grep\",
     \"args\": {\"pattern\": \"TODO\"},
     \"on_success\": \"list\", \"on_failure\": \"log\"},
    {\"id\": \"list\", \"tool\": \"file-tree\",
     \"args\": {\"dir\": \"src/\"}},
    {\"id\": \"log\", \"tool\": \"git-log\",
     \"args\": {}}
  ]
}"
 :args (list '(:name "steps_spec"
               :type string
               :description "JSON string describing the workflow steps. See the tool description for the format specification."))
 :confirm t   ; always confirm the workflow itself — the model is orchestrating multiple tools
 :category "orchestration")

(provide 'gptel-workflow)
;;; gptel-workflow.el ends here