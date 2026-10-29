;;; install.el --- install every package init.el asks for  -*- lexical-binding: t -*-

;;; Commentary:

;; Prepares a real package set for the load job: every `use-package' form
;; in init.el contributes its package name (or its `:ensure' value), except
;; forms with `:ensure nil' and forms managed by straight.el, which init.el
;; installs itself when it runs.
;;
;; Byte-compilation only.  Native compilation would tie the cache to the
;; exact Emacs build and makes a cold run several times slower.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(setq package-native-compile nil
      package-check-signature 'allow-unsigned)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Emacs 30 bundles old copies of compat and transient as built-in packages,
;; so `package-install' leaves them alone by default.  Upgrade them from the
;; archives, as a machine that has installed any modern package would.
(setq package-install-upgrade-built-in t)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Install compat first: transient's autoloads use its macros
;; (`static-when'), and package.el activates transient before compat if it
;; resolves the two as a dependency pair.
(package-install 'compat)

(defun my/install--forms ()
  "Return the top-level forms of init.el."
  (with-temp-buffer
    (insert-file-contents "init.el")
    (goto-char (point-min))
    (let (forms)
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil))
      (nreverse forms))))

(defun my/install--packages ()
  "Return the package names init.el needs installed."
  (let (packages)
    (dolist (form (my/install--forms))
      (when (and (consp form) (eq (car form) 'use-package))
        (unless (memq :straight form)
          (let ((name (cadr form))
                (ensures nil)
                (saw-ensure nil)
                (ensure-nil nil)
                (tail (cddr form)))
            (while tail
              (when (eq (car tail) :ensure)
                (setq saw-ensure t)
                (let ((value (cadr tail)))
                  (cond
                   ((null value) (setq ensure-nil t))   ; :ensure nil
                   ((eq value t) (push name ensures))   ; :ensure t
                   ((symbolp value) (push value ensures))
                   ((listp value) (setq ensures (append value ensures))))))
              (setq tail (cdr tail)))
            (cond
             ((and saw-ensure ensure-nil (null ensures)) nil) ; not ensured
             (ensures (setq packages (append ensures packages)))
             (t (push name packages)))))))
    (delete-dups (nreverse packages))))

(let ((failed nil))
  (dolist (package (sort (my/install--packages) #'string<))
    (cond
     ((package-installed-p package)
      (princ (format "installed: %s\n" package)))
     ((not (assq package package-archive-contents))
      (princ (format "SKIP (not in an archive): %s\n" package)))
     (t
      (princ (format "installing: %s\n" package))
      (condition-case err
          (package-install package)
        (error
         (setq failed t)
         (princ (format "FAILED: %s: %s\n" package
                        (error-message-string err))))))))
  ;; vterm ships C source and asks at first load whether to compile it.
  ;; Build it here so the load job never meets a prompt.
  (when (package-installed-p 'vterm)
    ;; `setq', not `let': the variable is not declared special until vterm
    ;; loads, and a lexical let-binding would clash with its defcustom.
    (setq vterm-always-compile-module t)
    (condition-case err
        (progn
          (require 'vterm)
          (princ "vterm module ready\n"))
      (error
       (setq failed t)
       (princ (format "FAILED: vterm module: %s\n"
                      (error-message-string err))))))
  (when failed
    (kill-emacs 1)))
