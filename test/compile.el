;;; compile.el --- byte-compile the repository's .el files  -*- lexical-binding: t -*-

;;; Commentary:

;; CI entry point.  The real packages are not installed: loading init.el
;; bootstraps straight.el and installs packages interactively, neither of
;; which works on a headless runner.  Instead, every package and feature
;; init.el asks for gets an empty stub on `load-path', so the `require'
;; forms the compiler evaluates succeed and the config's own forms are
;; checked.
;;
;; Warnings are reported but do not fail the build; errors do.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'use-package)

;; init.el sets `use-package-always-ensure' at compile time, which would
;; make the compiler install every package over the network.
(setq use-package-ensure-function #'ignore
      byte-compile-error-on-warn nil)

;; Two forms in init.el use straight.el's `:straight' keyword.  straight
;; registers it when it is bootstrapped; give the compiler the same hook
;; so the forms expand instead of failing to parse.
(add-to-list 'use-package-keywords :straight)
(defun use-package-normalize/:straight (_name _keyword args) args)
(defun use-package-handler/:straight (name _keyword _arg rest state)
  (use-package-process-keywords name rest state))

(defun my/compile--symbol (form)
  "Return FORM as a symbol, unwrapping a quote."
  (cond
   ((symbolp form) form)
   ((and (consp form) (eq (car form) 'quote) (symbolp (cadr form)))
    (cadr form))))

(defun my/compile--collect-names (form)
  "Return the package and feature names referred to by FORM."
  (cond
   ((not (consp form)) nil)
   ((eq (car form) 'use-package)
    (let ((name (my/compile--symbol (nth 1 form))))
      (append (when name (list name))
              (mapcan #'my/compile--collect-names (cddr form)))))
   ((eq (car form) 'require)
    (let ((name (my/compile--symbol (nth 1 form))))
      (when name (list name))))
   (t
    (append (my/compile--collect-names (car form))
            (my/compile--collect-names (cdr form))))))

(defun my/compile--names ()
  "Return every package and feature init.el refers to."
  (let (names)
    (with-temp-buffer
      (insert-file-contents "init.el")
      (goto-char (point-min))
      (condition-case nil
          (while t
            (push (my/compile--collect-names (read (current-buffer))) names))
        (end-of-file nil)))
    (delete-dups (apply #'append (nreverse names)))))

(defun my/compile--stub-directory (names)
  "Create a directory of empty stubs for NAMES that are not built in.
Return the directory."
  (let ((dir (make-temp-file "emacs-ci-stubs-" t)))
    (dolist (name names)
      (unless (locate-library (symbol-name name))
        (with-temp-file (expand-file-name (concat (symbol-name name) ".el") dir)
          (insert (format "(provide '%s)\n" name)))))
    dir))

(let ((stubs (my/compile--stub-directory (my/compile--names))))
  (push stubs load-path)
  (message "Stubbed %d features in %s"
           (length (directory-files stubs nil "\\.el\\'")) stubs))

(let ((compiled 0)
      (errored 0)
      (skipped 0))
  (dolist (file (directory-files default-directory t "\\.el\\'"))
    (when (file-regular-p file)
      (let ((base (file-name-nondirectory file)))
        (cond
         ((string-prefix-p "." base)
          (cl-incf skipped))
         ((string-match-p "-autoloads\\|-pkg\\'" base)
          (cl-incf skipped))
         (t
          (message "\n>>> %s" base)
          (condition-case err
              (if (byte-compile-file file)
                  (cl-incf compiled)
                (progn
                  (message "  FAILED")
                  (cl-incf errored)))
            (error
             (message "  FAILED %s" (error-message-string err))
             (cl-incf errored))))))))
  (message "\n%d compiled, %d errors, %d skipped" compiled errored skipped)
  (kill-emacs (if (> errored 0) 1 0)))
