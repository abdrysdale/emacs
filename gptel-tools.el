;;; gptel-tools.el --- Tools for use with gptel -*- lexical-binding: t -*-

;; Copyright (C) 2025 Alex Drysdale

;; Author: Alex Drysdale <alexander.drysdale@wales.nhs.uk>
;; Created: 25 Nov 2025
;; Version: 1.0
;; Keywords: ai gptel tools
;; X-URL: https://github.com/abdrysdale/emacs

;;; Commentary:

;;; Code:

(require 'gptel)

(defun gptel-tool-utils--get-project-root ()
  "Get the root for the currently active project."
  (let ((project (project-current)))
    (unless project
      (error "Not in a project.  Cannot list directory contents"))
    (file-name-as-directory (expand-file-name (project-root project)))))

;; Emacs ;;
(gptel-make-tool
 :name "read_buffer"
 :function (lambda (buffer)
             (unless (buffer-live-p (get-buffer buffer))
               (error "Error: buffer %s is not live" buffer))
             (with-current-buffer  buffer
               (buffer-substring-no-properties (point-min) (point-max))))
 :description "return the contents of an emacs buffer"
 :args (list '(:name "buffer"
               :type string
               :description "name of the buffer whose contents are to be retrieved"))
 :category "emacs")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Web ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun gptel-tool--fetch-rendered-url-content (url)
  "Fetch and render the HTML content of a URL to readable text."
  (condition-case error
      (let ((buffer (url-retrieve-synchronously url nil nil 30))) ; 30s timeout
        (if (not (and buffer (buffer-live-p buffer)))
            "Error: Unable to fetch URL"
          (unwind-protect
              (with-current-buffer buffer
                ;; Handle HTTP redirections and errors
                (goto-char (point-min))
                (if (looking-at "HTTP/[0-9.]+ \\([0-9]+\\)")
                    (let ((status (string-to-number (match-string 1))))
                      (unless (<= 200 status 299)
                        (error "HTTP error: %d" status)))
                  (error "Invalid HTTP response"))
                ;; Find end of headers
                (goto-char (point-min))
                (let ((header-end (or (re-search-forward "\r?\n\r?\n" nil t)
                                      (point-max))))
                  ;; Narrow to body only and decode
                  (narrow-to-region header-end (point-max))
                  (decode-coding-region (point-min) (point-max) 'utf-8)
                  ;; Render HTML to text using shr
                  (let ((shr-width 80))  ; Prevent long lines
                    (shr-render-buffer (current-buffer)))
                  ;; Extract text and sanitize for JSON
                  (let ((text (buffer-substring-no-properties (point-min) (point-max))))
                    ;; Remove control characters (NULL, bell, etc.) except \t, \n, \r
                    (setq text (replace-regexp-in-string "[\x00-\x08\x0b\x0c\x0e-\x1f\x7f]" "" text))
                    ;; Normalize line endings
                    (setq text (replace-regexp-in-string "\r\n" "\n" text))
                    ;; Remove null bytes explicitly
                    (setq text (replace-regexp-in-string "\0" "" text))
                    ;; Truncate if extremely long (protect token limits)
                    (if (> (length text) 80000)
                        (concat (substring text 0 80000)
                                "\n\n[Content truncated due to length...]")
                      text))))
            ;; Cleanup: ensure buffer is killed even if error occurs
            (when (buffer-live-p buffer)
              (kill-buffer buffer)))))
    (error (format "Error fetching URL %s: %s"
                   url (error-message-string error)))))

(gptel-make-tool
 :name "fetch-url"
 :function #'gptel-tool--fetch-rendered-url-content
 :description "Fetch the content of URL."
 :args (list '(:name "url"
               :type string
               :description "Address of the url to fetch."))
 :category "web")

(defun gptel-tool--fetch-search-engine-query (query)
  "Fetch the results returned by the default search engine for QUERY."
  (let* ((search-prefix eww-search-prefix)
         (search-term (replace-regexp-in-string " " "+" query))
         (url (concat search-prefix search-term)))
    (gptel-tool--fetch-rendered-url-content url)))

(gptel-make-tool
 :name "query-search-engine"
 :function #'gptel-tool--fetch-search-engine-query
 :description "Query a search engine and get the first page of results."
 :args (list `(:name "query"
               :type string
               :description ,(concat
                              "Query for the search engine"
                              " - typically used to get a list of relevant"
                              " urls before passing each url to 'fetch-url'")))
 :category "web")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; File System ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gptel-make-tool
 :name "ls"
 :function (lambda (&optional relative-dir)
             (let* ((root (gptel-tool-utils--get-project-root))
                    (dir (if relative-dir
                             (concat root relative-dir)
                           root)))
               (shell-command-to-string (concat "ls " (shell-quote-argument dir)))))
 :description "List the contents of a directory. Use this to explore the project structure before reading specific files."
 :args (list '( :name "relative-dir"
                :type string
                :description
                "Path of the directory relative to the project root."
                :optional t))
 :confirm t
 :category "filesystem")


(gptel-make-tool
 :name "add-file-to-context"
 :function (lambda (PATH)
             (let ((root (gptel-tool-utils--get-project-root)))
               (gptel-add-file (concat root PATH))))
 :description "Add a file to context, if PATH is a directory, recursively add all files to context. This does not return anything so do not expect a response but rather check the context. "
 :args (list '( :name "PATH"
                :type string
                :description "Path of the file to add to context relative to the project root."))
 :confirm t
 :category "filesystem")


(gptel-make-tool
 :name "create-file"
 :function (lambda (path filename content)
             (let ((root (gptel-tool-utils--get-project-root))
                   (full-path (expand-file-name filename (concat root path))))
               (with-temp-buffer
                 (insert content)
                 (write-file full-path))
               (format "Created file %s in %s" filename path)))
 :description "Create a new file with the specified content"
 :args (list '(:name "path"             ; a list of argument specifications
               :type string
               :description "The directory where to create the file relative to the project root")
             '(:name "filename"
                :type string
                :description "The name of the file to create")
             '(:name "content"
               :type string
               :description "The content to write to the file"))
 :confirm t
 :category "filesystem")

(gptel-make-tool
 :name "grep"
 :function (lambda (pattern &optional relative-dir)
             (let* ((root (gptel-tool-utils--get-project-root))
                    (dir (if relative-dir (concat root relative-dir) root)))
               (shell-command-to-string
                (format "cd %s && rg -n --no-heading %s"
                        (shell-quote-argument dir)
                        (shell-quote-argument pattern)))))
 :description "Search file contents recursively using ripgrep. USE THIS when you need to find where something is defined, referenced, or configured. Always search before reading files — it's faster than guessing which file to read. Returns matching lines with file:line prefixes."
 :args (list '(:name "pattern"
               :type string
               :description "Regular expression to search for in file contents")
             '(:name "relative-dir"
               :type string
               :description "Directory relative to project root (optional, defaults to root)"
               :optional t))
 :confirm t
 :category "filesystem")


;; Secret redaction for use with cloud models ;;
;; Inspired by Claude Code's PostToolUse hook pattern and dotenvx's ;;
;; value-based redaction. Two layers: file exclusion + output redaction. ;;

(defun gptel-tool--redact-secrets (text)
  "Redact credential-like patterns from TEXT.
Replaces values that match common secret patterns with [REDACTED].
Does not redact the key names — only the values — so the model
can still see WHERE secrets are configured without seeing WHAT they are."
  (let ((patterns
         '(;; Key=value pairs: password=foo, api_key: bar, "secret": "baz"
           ("\\(\\(?:api[_-]?key\\|apikey\\|api[_-]?secret\\|secret\\|password\\|passwd\\|pwd\\|token\\|bearer\\|access[_-]?key\\|private[_-]?key\\|client[_-]?secret\\)\\)[\"']\{0,1\}[:= ]+\\([A-Za-z0-9_\\-./+=]{8,}\\)"
            . "\\1: [REDACTED]")
           ;; AWS access keys
           ("AKIA[0-9A-Z]{16}" . "[REDACTED-AWS-KEY]")
           ;; AWS secret keys (40 char base64)
           ;; WARNING: this pattern is aggressive — it matches ANY 40-char
           ;; base64 string, not just AWS secrets. May over-redact base64
           ;; images, commit hashes, etc. Tune or remove if over-redacting.
           ("\\([A-Za-z0-9/+=]\\{40\\}\\)" . "[REDACTED-AWS-SECRET]")
           ;; PEM private key blocks
           ("-----BEGIN [A-Z ]*PRIVATE KEY-----[\\s\\S]*?-----END [A-Z ]*PRIVATE KEY-----"
            . "[REDACTED-PRIVATE-KEY]")
           ;; Connection strings with embedded credentials
           ("\\(\\(?:postgres\\|mongodb\\|redis\\|amqp\\|mysql\\|postgresql\\)://[^:]+:[^@]+@"
            . "\\1://[REDACTED]:[REDACTED]@")
           ;; JWT tokens (eyJ... header)
           ("eyJ[A-Za-z0-9_\\-]+\\.[A-Za-z0-9_\\-]+\\.[A-Za-z0-9_\\-]+" . "[REDACTED-JWT]")
           ;; GitHub tokens
           ("gh[pousr]_[A-Za-z0-9]{36,}" . "[REDACTED-GITHUB-TOKEN]")
           ;; Generic high-entropy hex strings (64+ chars, likely hashes/secrets)
           ("\\b[0-9a-f]\\{64,\\}\\b" . "[REDACTED-HASH]"))))
    (dolist (pair patterns text)
      (setq text (replace-regexp-in-string (car pair) (cdr pair) text))))
  text)

(defun gptel-tool--safe-rg (pattern &optional relative-dir)
  "Run ripgrep with secret file exclusion and output redaction.
Excludes .env, .pem, .key, credentials, and secret files.
Redacts credential-like patterns from the output."
  (let* ((root (gptel-tool-utils--get-project-root))
         (dir (if relative-dir (concat root relative-dir) root))
         (raw (shell-command-to-string
               (format
                "cd %s && rg -n --no-heading --glob '!*.env' --glob '!.env.*' --glob '!*.pem' --glob '!*.key' --glob '!*credentials*' --glob '!*secret*' --glob '!*.p12' --glob '!*.pfx' %s"
                (shell-quote-argument dir)
                (shell-quote-argument pattern)))))
    (gptel-tool--redact-secrets raw)))

(gptel-make-tool
 :name "safe-grep"
 :function #'gptel-tool--safe-rg
 :description "Search file contents with automatic secret redaction. USE THIS instead of grep when working with cloud models (Together, Gemini, Anthropic) to prevent credential leakage. Excludes .env, .pem, .key, credentials files. Redacts API keys, passwords, tokens, private keys, connection strings, and JWTs from results. For local models (Ollama), use 'grep' instead — no data leaves your machine."
 :args (list '(:name "pattern"
               :type string
               :description "Regular expression to search for in file contents")
             '(:name "relative-dir"
               :type string
               :description "Directory relative to project root (optional, defaults to root)"
               :optional t))
 :confirm t
 :category "filesystem")


(gptel-make-tool
 :name "file-tree"
 :function (lambda (&optional relative-dir)
             (let* ((root (gptel-tool-utils--get-project-root))
                    (dir (if relative-dir (concat root relative-dir) root)))
               (shell-command-to-string
                (format "cd %s && find . -type f -not -path '*/.git/*' -not -path '*/node_modules/*' -not -path '*/__pycache__/*' | head -200 | sort"
                        (shell-quote-argument dir)))))
 :description "List all files in the project (or subdirectory) as a flat tree. USE THIS FIRST to understand the project structure before exploring specific directories or files. Excludes .git, node_modules, __pycache__. Capped at 200 files."
 :args (list '(:name "relative-dir"
               :type string
               :description "Directory relative to project root (optional, defaults to root)"
               :optional t))
 :category "filesystem")




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Project ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gptel-tool--flymake-diagnostics (&optional all)
  "Collect flymake errors across all open buffers in the current project.

Errors with low severity are not collected.  With ALL, collect all
diagnostics."
  (let ((project (project-current)))
    (unless project
      (error "Not in a project.  Cannot collect flymake diagnostics"))
    (require 'flymake)
    (let ((results '()))
      (dolist (diag (flymake--project-diagnostics project))
        (let ((severity (flymake--diag-type diag)))
          (when (memq severity `(:error :warning ,@(and all '(:note))))
            (with-current-buffer (flymake-diagnostic-buffer diag)
              (let* ((beg (flymake--diag-beg diag))
                     (line-num (line-number-at-pos beg))
                     (line-text (buffer-substring-no-properties
                                 (line-beginning-position) (line-end-position))))
                (push (format "File: %s:%d\nSeverity: %s\nMessage: %s\n---\n%s"
                              (buffer-file-name)
                              line-num
                              severity
                              (flymake--diag-text diag)
                              line-text)
                      results))))))
      (string-join (nreverse results) "\n\n"))))

(gptel-make-tool
 :name "flymake-diagnostic"
 :description "Collect all code diagnostics with severity high/medium \
across all open buffers in the current project.

With optional argument `all`, collect notes and low-severity diagnostics
too."
 :function #'gptel-tool--flymake-diagnostics
 :args (list '( :name "all"
                :type boolean
                :description
                "Whether low-severity diagnostics (notes) should also be collected."
                :optional t))
 :category "project")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Git ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gptel-make-tool
 :name "status"
 :function (lambda () (let ((root (gptel-tool-utils--get-project-root)))
                   (shell-command-to-string
                    (concat "cd " (shell-quote-argument root) " && git status"))))
 :description "Git status at the project root"
 :category "git")

(gptel-make-tool
 :name "branch"
 :function (lambda () (let ((root (gptel-tool-utils--get-project-root)))
                   (shell-command-to-string
                    (concat "cd " (shell-quote-argument root) " && git branch"))))
 :description "Show git branches at the project root."
 :category "git")

(gptel-make-tool
 :name "log"
 :function (lambda () (let ((root (gptel-tool-utils--get-project-root)))
                   (shell-command-to-string
                    (concat "cd " (shell-quote-argument root) " && git log --oneline"))))
 :description "Git log at the project root"
 :category "git")

(gptel-make-tool
 :name "git-diff"
 :function (lambda (&optional ref)
             (let ((root (gptel-tool-utils--get-project-root)))
               (shell-command-to-string
                (concat "cd " (shell-quote-argument root)
                        " && git diff "
                        (if ref (shell-quote-argument ref) "")))))
 :description "Show git diff — uncommitted changes by default, or changes in a specific commit/branch. Use to understand what changed recently."
 :args (list '(:name "ref"
               :type string
               :description "Git ref (commit hash, branch name) to diff against. Omit for working tree changes."
               :optional t))
 :category "git")

(gptel-make-tool
 :name "git-show"
 :function (lambda (ref)
             (let ((root (gptel-tool-utils--get-project-root)))
               (shell-command-to-string
                (concat "cd " (shell-quote-argument root)
                        " && git show --stat "
                        (shell-quote-argument ref)))))
 :description "Show a specific commit: message + files changed + diff stats. Use when you need to understand what a specific commit did."
 :args (list '(:name "ref"
               :type string
               :description "Commit hash or ref to show"))
 :category "git")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Shell ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gptel-make-tool
 :name "shell"
 :function (lambda (command)
             (let ((root (gptel-tool-utils--get-project-root)))
               (shell-command-to-string
                (format "cd %s && %s"
                        (shell-quote-argument root)
                        command))))
 :description "Run an arbitrary shell command from the project root. Use for git operations, config inspection, make targets, docker commands, or anything not covered by other tools. Prefer specific tools (grep, file-tree, git-diff) when they apply."
 :args (list '(:name "command"
               :type string
               :description "Shell command to execute"))
 :confirm t
 :category "shell")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Python ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gptel-make-tool
 :name "python"
 :function (lambda (cmd)
             (let ((root (gptel-tool-utils--get-project-root)))
               (shell-command-to-string
                (format "cd %s && uv run python -c %s"
                        (shell-quote-argument root)
                        (shell-quote-argument cmd)))))
 :args (list '( :name "cmd"
                :type string
                :description "Python code to execute. Runs via 'uv run python -c' from the project root."))
 :description "Run arbitrary Python code from the project root. Use for data processing, analysis, or chaining multiple file reads into a single result."
 :category "python"
 :confirm t)

(gptel-make-tool
 :name "pytest"
 :function (lambda () (let ((root (gptel-tool-utils--get-project-root)))
                   (shell-command-to-string
                    (concat "uv run pytest " (shell-quote-argument root)))))
 :description "Run pytest at the project root."
 :category "python")

(gptel-make-tool
 :name "unittest"
 :function (lambda () (let ((root (gptel-tool-utils--get-project-root)))
                   (shell-command-to-string
                    (concat "uv run python -m unittest discover -s 'tests' "
                            (shell-quote-argument root)))))
 :description "Run python -m unittest at the project root."
 :category "python")

(gptel-make-tool
 :name "mypy"
 :function (lambda () (let ((root (gptel-tool-utils--get-project-root)))
                   (shell-command-to-string
                    (concat "uv run ty " (shell-quote-argument root)))))
 :description "Run mypy at the project root - this is slow so only run if ty doesn't work"
 :category "python")

(gptel-make-tool
 :name "ty"
 :function (lambda () (let ((root (gptel-tool-utils--get-project-root)))
                   (shell-command-to-string
                    (concat "uv run ty " (shell-quote-argument root)))))
 :description "Run ty (the type checker by astral) at the project root."
 :category "python")

(gptel-make-tool
 :name "python-docs-search"
 :function
 (lambda (query)
   "Search the official Python documentation for QUERY and return the most relevant section.
Uses Google to find the top docs.python.org result, fetches it, and extracts the first
relevant heading and its content. Query should be a natural language phrase, e.g.,
'when to use doctest'."
   (let* ((search-query (concat query " site:docs.python.org"))
          (search-results (funcall (gptel-get-tool-function 'query-search-engine) search-query))
          (url (car (gptel--extract-urls-from-html search-results)))
          (content (when url (funcall (gptel-get-tool-function 'fetch-url) url))))
     (if (not url)
         "No relevant Python documentation found. Try rephrasing your query."
       (let* ((cleaned-content (gptel--strip-html-tags content))
              (keywords (split-string query))
              (section-headings (gptel--find-section-headings cleaned-content keywords))
              (best-match (car section-headings)))
         (if best-match
             (format "Found in: %s\n\n%s" url best-match)
           "No clear section found in documentation. The page may not contain a direct answer.")))))
 :description "Search the official Python documentation for a natural language query by fetching and parsing the top result."
 :args (list '(:name "query"
               :type string
               :description "Natural language query to search in Python docs (e.g., 'when to use doctest')"))
 :category "python")

;; Helper: Extract URLs from HTML snippet (simple but effective)
(defun gptel--extract-urls-from-html (html)
  "Extract the first 5 URLs from HTML text."
  (let ((urls '())
        (count 0))
    (while (and (string-match "<a[^>]+href=\"\\([^\"]+\\)\"[^>]*>" html)
                (< count 5))
      (let ((url (match-string 1 html)))
        (when (string-match-p "^https?://docs\\.python\\.org" url)
          (push url urls)
          (setq count (1+ count)))
        (setq html (substring html (match-end 0)))))
    (nreverse urls)))

;; Helper: Strip HTML tags and return clean text
(defun gptel--strip-html-tags (html)
  "Strip all HTML tags from HTML string, preserving text content."
  (let ((text html))
    (while (string-match "<[^>]+>" text)
      (setq text (replace-match "" t t text)))
    (replace-regexp-in-string "[\n\t]+" " " text)))

;; Helper: Find first heading containing any keyword
(defun gptel--find-section-headings (text keywords)
  "Find the first section heading in TEXT that contain any keyword from KEYWORDS.
Assumes headings are prefixed with '## ' or '### ' or are on their own line with capitalization."
  (let ((headings '())
        (lines (split-string text "\n" t)))
    (dolist (line lines)
      (when (and (> (length line) 2)
                 (or (string-prefix-p "## " line)
                     (string-prefix-p "### " line)
                     (string-match-p "^[A-Z][^<]*[.!?]?$" line)))
        (let ((line-lower (downcase line)))
          (when (cl-some (lambda (k) (string-match-p k line-lower)) keywords)
            (push line headings)))))
    (nreverse headings)))

(provide 'gptel-tools)
;;; gptel-tools.el ends here
