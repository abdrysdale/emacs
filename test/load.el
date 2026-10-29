;;; load.el --- load init.el for real  -*- lexical-binding: t -*-

;;; Commentary:

;; CI entry point.  Unlike test/compile.el this does not stub anything: it
;; runs the configuration against a real package set, so an unbound variable,
;; a missing file or a failing require fails the job.  This is the check that
;; answers "will Emacs start on my machine?".

;;; Code:

(require 'subr-x)

(setq user-emacs-directory
      (file-name-as-directory (expand-file-name "~/.emacs.d")))
(setq debug-on-error t)

;; The package set is prepared by test/install.el.  If something is missing,
;; say so instead of letting use-package reach the network mid-load.
(defun my/load--ensure (name ensure _state)
  "Fail unless every package in ENSURE (from NAME's use-package form) exists."
  (dolist (package (ensure-list ensure))
    (unless (or (null package) (eq package t) (package-installed-p package))
      (error "Package `%s' (needed by %s) is not installed" package name))))

(setq use-package-ensure-function #'my/load--ensure)

;; On Emacs 31 a missing package would open an interactive review buffer
;; (`package-review-policy' is t in init.el) and hang the job.
(with-eval-after-load 'package
  (when (fboundp 'package-review)
    (advice-add 'package-review :override #'ignore)))

(load (expand-file-name "init.el" user-emacs-directory) nil 'nomessage)

(princ "init.el loaded\n")
