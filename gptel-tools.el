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
    (file-name-as-directory (project-root project))))

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
  "Fetch and render the html content of a URL."
   (let ((buffer (url-retrieve-synchronously url)))
    (if  buffer
        (with-current-buffer buffer
          (message (format "fetched and rendered url %s" url))
          (shr-render-region (point-min) (point-max))
          (concat (buffer-substring-no-properties (point-min) (point-max))))
      "unable to load website")))

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
    (fetch-rendered-url-content url)))

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
               (shell-command-to-string (concat "ls " dir))))
 :description "List the contents of the project directory."
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
                    (concat "cd " root " && git status"))))
 :description "Git status at the project root"
 :category "git")

(gptel-make-tool
 :name "branch"
 :function (lambda () (let ((root (gptel-tool-utils--get-project-root)))
                   (shell-command-to-string
                    (concat "cd " root " && git branch"))))
 :description "Git branch at the project root"
 :category "git")

(gptel-make-tool
 :name "log"
 :function (lambda () (let ((root (gptel-tool-utils--get-project-root)))
                   (shell-command-to-string
                    (concat "cd " root " && git log --oneline"))))
 :description "Git branch at the project root"
 :category "git")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Python ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gptel-make-tool
 :name "python"
 :function (lambda (cmd)
             (shell-command-to-string (format "uv run python -c %s" cmd)))
 :args (list '( :name "cmd"
                :type string
                :string "Python command to be run in: uv run python -c %s"))
 :description "Run an arbitrary python command."
 :category "python"
 :confirm t)

(gptel-make-tool
 :name "pytest"
 :function (lambda () (let ((root (gptel-tool-utils--get-project-root)))
                   (shell-command-to-string
                    (concat "uv run pytest " root))))
 :description "Run pytest at the project root."
 :category "python")

(gptel-make-tool
 :name "unittest"
 :function (lambda () (let ((root (gptel-tool-utils--get-project-root)))
                   (shell-command-to-string
                    (concat "uv run python -m unittest discover -s 'tests' "
                            root))))
 :description "Run python -m unittest at the project root."
 :category "python")

(gptel-make-tool
 :name "mypy"
 :function (lambda () (let ((root (gptel-tool-utils--get-project-root)))
                   (shell-command-to-string
                    (concat "uv run ty " root))))
 :description "Run mypy at the project root - this is slow so only run if ty doesn't work"
 :category "python")

(gptel-make-tool
 :name "ty"
 :function (lambda () (let ((root (gptel-tool-utils--get-project-root)))
                   (shell-command-to-string
                    (concat "uv run ty " root))))
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
  (let ((urls '()))
    (while (string-match "<a[^>]+href=\"\\([^\"]+\\)\"[^>]*>" html)
      (let ((url (match-string 1 html)))
        (when (string-match-p "^https?://docs\\.python\\.org" url)
          (push url urls))
        (setq html (substring html (match-end 0))))
      (when (> (length urls) 4) (cl-return)))
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
