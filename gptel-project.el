;;; gptel-project.el --- Per-buffer project pin for gptel -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Alex Drysdale

;; Author: Alex Drysdale <alexander.drysdale@wales.nhs.uk>
;; Created: 23 Sep 2026
;; Version: 0.1
;; Keywords: ai gptel tools project
;; X-URL: https://github.com/abdrysdale/emacs

;;; Commentary:
;;
;; gptel executes tool functions in the chat buffer, not the buffer
;; the request was sent from (gptel--accept-tool-calls applies tool
;; functions with no buffer wrapper).  Chat buffers don't visit files,
;; so `project-current' returns nil there and every project-aware tool
;; in gptel-tools.el errors with "Not in a project".
;;
;; This file gives each gptel buffer an explicit, buffer-local project
;; pin:
;;
;;   `gptel-project'        pinned project root (directory string)
;;   `gptel-project-origin' where the pin came from:
;;                          'creation  captured automatically when the
;;                                     chat buffer was created, from the
;;                                     buffer gptel was started in
;;                          'manual    set with M-x gptel-set-project
;;                          'inherited auto-detected on first tool call
;;                          'unset     explicitly unpinned with
;;                                     M-x gptel-unset-project
;;
;; The pin is captured at chat-buffer creation time (from
;; `gptel-mode-hook'; the new buffer inherits `default-directory' from
;; the originating buffer) and then stays fixed for the life of the
;; buffer.  Re-toggling gptel-mode or re-running M-x gptel on the same
;; buffer never changes an existing pin.
;;
;; Commands:
;;   M-x gptel-set-project    pin this gptel buffer to a project
;;   M-x gptel-unset-project  remove the pin; tools error instead of
;;                            silently re-detecting
;;
;; Requires: gptel; project.el (built-in since Emacs 26.1).

;;; Code:

(require 'gptel)
(require 'project)

(defvar-local gptel-project nil
  "Project root pinned for this gptel buffer, or nil when unpinned.
A directory string with trailing slash.  When non-nil,
`gptel-tool-utils--get-project-root' returns this instead of
running `project-current' in the chat buffer.")

(defvar-local gptel-project-origin nil
  "Where this buffer's project pin came from.
One of the symbols `creation', `manual', `inherited', `unset', or
nil (never pinned).  `unset' suppresses the automatic
capture-on-creation and inherit-on-first-use behaviour until a new
pin is set.")

(defun gptel-project--detect ()
  "Return the project root for the current buffer's directory, or nil.
Uses `project-current', falling back to `project--find-in-directory'.
Never prompts and never signals."
  (let ((dir (expand-file-name default-directory)))
    (when (file-directory-p dir)
      (let ((project (ignore-errors
                       (or (project-current nil dir)
                           (project--find-in-directory dir)))))
        (and project (file-name-as-directory
                      (expand-file-name (project-root project))))))))

(defun gptel-project--pin-at-creation ()
  "Pin this buffer's project when the gptel buffer is first created.
Runs from `gptel-mode-hook' in the chat buffer, whose
`default-directory' is inherited from the buffer `gptel' was
invoked from.  Never re-pins a buffer that already has a pin."
  (when (and (null gptel-project)
             (null gptel-project-origin))
    (let ((root (gptel-project--detect)))
      (when root
        (setq gptel-project root
              gptel-project-origin 'creation)))))

(defun gptel-set-project (dir)
  "Pin this gptel buffer to project DIR.
Project-aware gptel tools (ls, grep, file-tree, shell, git, python,
project-context, ...) resolve their project root from this pin
until it is changed or removed."
  (interactive
   (list (read-directory-name "Pin this gptel buffer to project: ")))
  (setq gptel-project (file-name-as-directory (expand-file-name dir))
        gptel-project-origin 'manual)
  (message "GPTel project pinned to %s" gptel-project))

(defun gptel-unset-project ()
  "Remove this buffer's project pin.
Project-aware gptel tools will error with a message asking for a
new pin instead of silently re-detecting a project."
  (interactive)
  (if (null gptel-project)
      (user-error "This gptel buffer has no project pin")
    (setq gptel-project nil
          gptel-project-origin 'unset)
    (message "GPTel project pin removed; run M-x gptel-set-project to pin one")))

(defun gptel-show-project ()
  "Show the project pin and its origin for this gptel buffer."
  (interactive)
  (if gptel-project
      (message "GPTel project: %s (origin: %s)"
               gptel-project
               (pcase gptel-project-origin
                 ('creation "captured at buffer creation")
                 ('manual "set manually")
                 ('inherited "auto-detected on first use")
                 (_ (symbol-name gptel-project-origin))))
    (message "No project pinned in this gptel buffer; run M-x gptel-set-project")))

(defun gptel-project--root ()
  "Return the pinned project root for the current buffer.
Errors when no pin exists and no project is detectable, so tool
calls fail with an actionable message instead of operating in the
wrong directory."
  (cond
   ;; Existing pin (creation/manual/inherited): honour it.
   (gptel-project
    (let ((root (expand-file-name gptel-project)))
      (if (file-directory-p root)
          (file-name-as-directory root)
        (error "Pinned gptel project %s no longer exists; run M-x gptel-set-project to re-pin"
               root))))
   ;; Never pinned and never unset: auto-detect once, then keep fixed.
   ((null gptel-project-origin)
    (let ((root (gptel-project--detect)))
      (if root
          (progn (setq gptel-project root
                       gptel-project-origin 'inherited)
                 root)
        (error "No project pinned for this gptel buffer; run M-x gptel-set-project here"))))
   ;; Explicitly unset: stay unpinned until a new pin is set.
   (t
    (error "No project pinned for this gptel buffer; run M-x gptel-set-project here"))))

;; Capture the origin project at chat-buffer creation time.  The hook
;; runs in the chat buffer itself; `default-directory' there is
;; inherited from the buffer gptel was started from.
(add-hook 'gptel-mode-hook #'gptel-project--pin-at-creation)

(provide 'gptel-project)
;;; gptel-project.el ends here