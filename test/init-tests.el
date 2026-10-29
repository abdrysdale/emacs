;;; init-tests.el --- ERT checks for the init.el prefix maps  -*- lexical-binding: t -*-

;;; Commentary:

;; Checks the `global-set-keys-to-prefix' declarations in init.el.  The
;; config itself is not loaded: CI runs a headless Emacs without the
;; packages init.el installs, and loading it bootstraps straight.el.
;;
;;   emacs --batch -l test/init-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst my/test-root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))
  "Repository root, derived from this file's location.")

(defconst my/test-init-file (expand-file-name "init.el" my/test-root))

(defun my/test-prefix-forms ()
  "Return the `global-set-keys-to-prefix' forms found in init.el."
  (let (forms)
    (with-temp-buffer
      (insert-file-contents my/test-init-file)
      (goto-char (point-min))
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (when (and (listp form)
                         (eq (car form) 'global-set-keys-to-prefix))
                (push form forms))))
        (end-of-file nil)))
    (nreverse forms)))

(defun my/test-prefix-declarations ()
  "Return (PREFIX . KEY) pairs from the `global-set-keys-to-prefix' forms."
  (let (decls)
    (dolist (form (my/test-prefix-forms))
      (let ((prefix (nth 1 form))
            (bindings (nth 2 form)))
        (when (and (stringp prefix) (eq (car-safe bindings) 'quote))
          (dolist (elm (cadr bindings))
            (push (cons prefix (car elm)) decls)))))
    (nreverse decls)))

(defun my/test-declared-prefixes ()
  "Return the distinct prefixes declared in init.el."
  (delete-dups (mapcar #'car (my/test-prefix-declarations))))

(ert-deftest my/prefix-keys-are-unique ()
  "No key is declared twice under the same prefix."
  (let ((seen (make-hash-table :test #'equal))
        (dups nil))
    (dolist (decl (my/test-prefix-declarations))
      (let ((key (format "%s %s" (car decl) (cdr decl))))
        (if (gethash key seen)
            (push key dups)
          (puthash key t seen))))
    (when dups
      (ert-fail (format "Duplicate prefix keys: %s"
                        (mapconcat #'identity (nreverse dups) ", "))))))

(ert-deftest my/prefixes-have-bindings ()
  "Every declared prefix has at least one binding."
  (dolist (form (my/test-prefix-forms))
    (let ((prefix (nth 1 form))
          (bindings (nth 2 form)))
      (when (stringp prefix)
        (should (consp (cadr bindings)))))))

(defun my/test-explicit-binding (map key)
  "Return the explicit binding of KEY in MAP, ignoring default bindings.
KEY is a key sequence as returned by `kbd'.  Returns t when a command
sits where a prefix was expected, and nil when the key is not bound."
  (let ((binding map))
    (catch 'done
      (dotimes (i (length key))
        (unless (keymapp binding)
          (throw 'done t))
        (let ((event (aref key i))
              (next nil))
          (map-keymap (lambda (ev b) (when (equal ev event) (setq next b)))
                      binding)
          (unless next
            (throw 'done nil))
          (setq binding next)))
      binding)))

(defconst my/test-allowed-prefix-shadows
  '((smerge-mode . "C-c d"))
  "Minor modes that intentionally shadow a global prefix.
`smerge-mode-map' binds `C-c d' as its command prefix, so the smerge
commands win in a conflict buffer.  Any other shadow is unintended.")

(ert-deftest my/no-builtin-mode-shadows-a-prefix ()
  "No built-in minor mode shadows a C-c prefix except the listed ones.
Only the modes loaded into this Emacs are checked; the point is to
catch a built-in claiming a prefix the config believes is its own."
  (require 'smerge-mode)
  (let ((problems nil))
    (dolist (entry minor-mode-map-alist)
      (let ((mode (car entry))
            (map (cdr entry)))
        (when (keymapp map)
          (dolist (prefix (my/test-declared-prefixes))
            (when (string-match-p "\\`C-c .\\'" prefix)
              (when (and (my/test-explicit-binding map (kbd prefix))
                         (not (member (cons mode prefix)
                                      my/test-allowed-prefix-shadows)))
                (push (format "%s shadows %s" mode prefix) problems)))))))
    (when problems
      (ert-fail (format "Unexpected prefix shadows:\n%s"
                        (mapconcat #'identity (nreverse problems) "\n"))))))

(provide 'init-tests)
;;; init-tests.el ends here
