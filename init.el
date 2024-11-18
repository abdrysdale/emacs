;;; MyEmacsConfig --- a minimal cross platform config
;;
;; -*- lexical-binding: t -*-
;;
;;; Commentary:
;; It is meant to provide a decent working environment
;; that's fairly easy to manage.
;; A lot of this is based on my own personal config and
;; the excellent resource of Emacs ONBOARD
;; URL: https://github.com/monkeyjunglejuice/emacs.onboard
;; In general, I try to use as many of the builtin features available
;; and tend to only use external package when it's much more convenient.

;;; Code:
(require 'server)
(defvar init-script-initial-clients nil
    "Connected clients when init script was run.")
(setq init-script-initial-clients server-clients)
(server-start)

;;  **********************
;;; * Package Management *
;;  **********************

;; Ensure that Emacs can verify SSL/TLS certificates
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)
(setq straight-use-package-by-default t)

;; Package archives
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t)

;; Natively compile packages at first use or immediately after installation?
(setq package-native-compile t)

;; File modes ;;
;; No config needed - just needed for the file types.
(use-package csv-mode)
(use-package yaml-mode)
(use-package toml-mode)
(use-package json-mode)
(use-package markdown-mode)

;;  ********************
;;; * Usage Statistics *
;;  ********************

;; File: elisp.info,  Node: Garbage Collection, Up: GNU Emacs Internals

;; Set a high value of 1 GB to prevent frequent garbage collections
;; during initialization.
(setq gc-cons-threshold #x40000000)  ; default threshold is 800 KB

;; Prevent longer GC pauses and experience less mini-interruptions.
;; When idle for 15 sec, run the GC no matter what.
;; This hack was stolen from <https://akrl.sdf.org/>
(defmacro my-time (&rest body)
  "Measure and return the time it takes evaluating BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

(defvar my-gc-timer nil
  "Timer object for garbage collection monitoring.
The timer can be canceled with `my-cancel-gc-timer'.")

(defun my-start-gc-timer ()
  "Start the garbage collection timer."
  (interactive)
  (setq my-gc-timer
        (run-with-idle-timer 15 t
                             (lambda ()
                               (my-time (garbage-collect))))))

(defun my-cancel-gc-timer ()
  "Cancel the garbage collection timer."
  (interactive)
  (when (timerp my-gc-timer)
    (cancel-timer my-gc-timer)
    (setq my-gc-timer nil)))

;; Start the GC Timer
(my-start-gc-timer)

;; Show a message when garbage collection happens? Useful while tuning the GC
(setq garbage-collection-messages nil)

;; Diagnostics
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs started in %s with %d garbage collections."
                     (format "%.3f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; WakaTime
;; Requires wakatime-cli
(load (concat user-emacs-directory ".waka.el"))
(use-package wakatime-mode
  :straight nil)
(global-wakatime-mode)

;; Straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)
(setq straight-use-package-by-default t)

(require 'use-package)
(setq use-package-always-ensure t)

;; Natively compile packages at first use or immediately after installation?
(setq package-native-compile t)

;; Magit ;;
(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
(setenv "GIT_ASKPASS" "git-gui--askpass")
(global-set-key (kbd "C-c g g") #'magit)

;; Forge ;;
(use-package forge
  :after magit)

;; Git Time Machine ;;
(use-package git-timemachine)
(setq git-timemachine-abbreviation-length 6)
(global-set-key (kbd "C-c g t") #'git-timemachine)

;; File modes ;;
;; No config needed - just needed for the file types.
(use-package csv-mode)
(use-package yaml-mode)
(use-package toml-mode)
(use-package json-mode)
(use-package markdown-mode)

;;  **********
;;; * System *
;;  **********

;; Prevent stale elisp bytecode from shadowing more up-to-date source files
(setq load-prefer-newer t)

;; Increase warning threshold
(setq large-file-warning-threshold (* 64 1000000))

;; Set undo limit to 64 MB
(setq undo-outer-limit (* 64 1000000))
(global-set-key (kbd "M-r") #'undo-redo)
(global-set-key (kbd "M-u") #'undo-only)

;; Increase the amount of data which Emacs reads from subprocesses
(setq read-process-output-max (* 1024 1024))  ; 1 MB

;; Sets the default directory
(setq default-directory "~/")

;; Info
(defvar info-custom-dir (expand-file-name "~/.emacs.d/info/")
  "Location of custom info directory.")
(add-to-list 'Info-directory-list info-custom-dir)
(when (eq system-type 'windows-nt)
    (add-to-list 'Info-directory-list
                 (expand-file-name "~/scoop/apps/emacs/current/share/info")))
(defun info-custom-manuals (manual)
  (info (concat info-custom-dir manual ".info")))
(defun info-custom-python ()
  "Launch python info file."
  (interactive)
  (info-custom-manuals "python"))
(global-set-key (kbd "C-c h p") #'info-custom-python)

;; Sets the auth source (requires gpg!)
(setq auth-sources '("~/.authinfo.gpg")
      epa-gpg-program "gpg"         ;; Ensures GPG program is GPG
      epa-pinentry-mode 'loopback)  ;; Needed if GPG requires a password for decrypting keys

;; Saves session
;; Works with daemons but not remotely running Emacs sessions.
(desktop-save-mode 1)
(add-hook 'server-after-make-frame-hook #'desktop-read)
(setq desktop-load-locked-desktop 'check-pid)

;;  ******
;;; * UI *
;;  ******

;; Help
(setq help-at-pt-timer-delay 0
      help-at-pt-display-when-idle "always")

;; Tooltips
(setq tooltip-delay 0
      tooltip-short-delay 0
      use-system-tooltips nil)

;; Fireplace - a mission critical package;;
(use-package fireplace)

;; Dashboard ;;
(use-package page-break-lines)
(use-package dashboard
  :config
  (require 'page-break-lines))

;; Which-Key ;;
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.1))

;; Frame
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq server-after-make-frame-hook #'dashboard-open)

;; Basic
(setq inhibit-startup-message t
      visible-bell t
      confirm-kill-emacs 'yes-or-no-p
      global-tab-line-mode t
      truncate-lines t
      x-stretch-cursor t
      use-dialog-box nil)

(scroll-bar-mode -1)                ; Disable visible scrollbar
(tool-bar-mode -1)                  ; Disable the toolbar
(tooltip-mode -1)                   ; Disable tooltips
(set-fringe-mode 0)                 ; Removes fringes
(menu-bar-mode -1)                  ; Disable the menu bar
(global-visual-line-mode t)         ; Enables visual lines
(blink-cursor-mode t)               ; Blinks the cursor
(global-hl-line-mode t)             ; Hightlights current line

;; Modeline
(setq mode-line-compact t   ; Compresses repeating spaces to a single space
      display-time-24hr-format t
      display-battery-mode t)
(display-time-mode 1)       ; Displays the time.
(display-battery-mode 1)    ; Displays the battery.
(size-indication-mode 1)    ; Show the buffer size in the modeline
(column-number-mode 1)      ; Show column number along with line number in modeline

;; Tabs
(setq tab-bar-position t
      tab-bar-show 1
      tab-bar-close-button-show nil
      tab-bar-format '(tab-bar-format-history
                       tab-bar-format-tabs
                       tab-bar-separator)
      tab-bar-new-tab-choice #'scratch-buffer
      tab-bar-new-tab-to 'rightmost)

;; Programming UI
(add-hook 'prog-mode-hook #'whitespace-mode)
(setq display-raw-bytes-as-hex t)

;; Line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type t)

;; Use spaces not tabs - this is the hill I will die on.
(setq-default c-basic-offset 4
	      tab-width 4
	      indent-tabs-mode nil
          tab-always-indent t)

;; Electric indents reidents text lines on-the-fly.
;; I do not like this.
(electric-indent-mode 0)

;; Theme
(defvar me/dark-theme 'modus-vivendi "Default dark theme.")
(defvar me/light-theme 'modus-operandi "Default light theme.")

(defun load-theme-light ()
  "Load the light theme which is set by me/light-theme."
  (interactive)
  (load-theme me/light-theme t))
(defun load-theme-dark ()
  "Load the dark theme which is set by me/dark-theme."
  (interactive)
  (load-theme me/dark-theme t))

(load-theme me/dark-theme t)

;; Font - only use Cascadia on windows.
(if (eq system-type 'windows-nt)
    (add-to-list 'default-frame-alist
                 '(font . "Cascadia Mono-14"))
  (set-face-attribute 'default nil
                      :font "FreeMono-13"
                      :width 'expanded
                      :weight 'medium))

;; Highlighting changes
(setq highlight-changes-mode t)

(global-set-key (kbd "C-c c c") #'highlight-changes-mode)
(global-set-key (kbd "C-c c h") #'highlight-changes-remove-highlight)
(global-set-key (kbd "C-c c v") #'highlight-changes-visible-mode)
(global-set-key (kbd "C-c c n") #'highlight-changes-next-change)
(global-set-key (kbd "C-c c p") #'highlight-changes-previous-change)
(global-set-key (kbd "C-c c r") #'highlight-changes-rotate-faces)
(global-set-key (kbd "C-c c f") #'highlight-compare-with-file)
(global-set-key (kbd "C-c c b") #'highlight-compare-buffers)

;;  *****************
;;; * Basic Editing *
;;  *****************

(save-place-mode 1)
(setq save-interprogram-paste-before-kill t)
(setq yank-pop-change-selection t)

(global-set-key (kbd "C-x j") #'join-line)
(global-set-key (kbd "C-c r") #'replace-string)
(global-set-key (kbd "C-c q") #'query-replace)
(global-set-key (kbd "C-/") #'undo)

;; In general, I prefer to go forward to the beginning of a word
;; not the end, but I've provided a shortcut for both a bound
;; forward to the beginning of the word to M-f
(global-set-key (kbd "C-M-f") #'forward-word)
(global-set-key (kbd "M-f") #'forward-to-word)

;; Handy for scrolling through buffers.
;; <SPC>    :: scroll forward one windowful.
;; S-<SPC>  :: scroll backward one windowful.
;; s        :: start incremental search.
;; q        :: Quit and go back to buffer & position before view-mode.
;; e        :: Quit and stay in the same buffer & position.
(global-set-key (kbd "C-c v") #'view-mode)

;; Format whitespace visualisation
(setq whitespace-style '(face tabs spaces trailing lines space-before-tab
                              newline indentation empty space-after-tab
                              space-mark tab-mark missing-newline-at-eof))

;; Count the words
(global-set-key (kbd "C-c b c") #'count-words)

;;  ********************
;;; * Advanced Editing *
;;  ********************

;; Completion
(require 'icomplete)
(setq icomplete-mode t
      icomplete-in-buffer t
      icomplete-compute-delay 0.01
      icomplete-delay-completions-threshold 10000
      icomplete-show-matches-on-no-input t
      icomplete-hide-common-prefix nil
      read-buffer-completion-ignore-case t
      completion-ignore-case t
      completion-auto-help nil
      completion-styles '(basic partial-completion initials substring)) ;; Flex is too aggressive.
;; Used to use fido-vertical but that has difficulty when not selecting an item
;; from the completion list.  That is, when rather than selecting "fo" if "foo"
;; is present "foo" will always be selected.
;; Hence I switched to icomplete.
;; The vertical completion takes up more screen real estate so I ignored that.

;; For some reason icomplete doesn't always load after initialisation
(icomplete-mode)

;; Use the default `completion--in-region' function.
(setq completion-in-region-function
      (lambda (&rest args)
        (apply #'completion--in-region args)))


;; Enables advanced editing modes
(put 'set-goal-column 'disabled nil)    ;; Enables setting a goal column
(put 'narrow-to-region 'disabled nil)   ;; Enables narrowing
(put 'upcase-region 'disabled nil)      ;; Enables up case a regoin
(put 'downcase-region 'disabled nil)    ;; Enables down case a regoin

;; Multiple cursors ;;
(use-package multiple-cursors
  :straight nil
  :config
  (global-set-key (kbd "C->") #'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") #'mc/mark-previous-like-this)
  (global-set-key (kbd "C-x M-.") #'mark-pop)
  (global-set-key (kbd "C-x C-.") #'mc/edit-lines)
  (define-key mc/keymap (kbd "<return>") nil))

;; Expand region
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; iSpell ;;
(global-set-key (kbd "C-c d s") 'ispell)
(global-set-key (kbd "C-c d w") 'ispell-word)
(global-set-key (kbd "C-c d c") 'ispell-comments-and-strings)


;; Multi-file operations
(global-set-key (kbd "C-c f c") #'fileloop-continue)


;;  *************************
;;; * Programming Languages *
;;  *************************

;;;; The Grand Unified Debugger
(global-set-key (kbd "C-x C-a i") #'gud-goto-info)
(global-set-key (kbd "C-x C-a t") #'gud-tooltip-mode)

;;;; Python
(defun python-imenu-use-flat-index
    ()
  (setq imenu-create-index-function
        #'python-imenu-create-flat-index))

(add-hook 'python-mode-hook
          #'python-imenu-use-flat-index)

(global-set-key (kbd "C-c g d") #'pdb)
(setq gud-pdb-command-name "poetry run python -m pdb")
(setq python-shell-interpreter "python")

;;;; Perl
(add-to-list 'major-mode-remap-alist '(perl-mode . cperl-mode))

(setq cperl-invalid-face nil)
(setq cperl-electric-keywords t) ;; expands for keywords such as
                                     ;; foreach, while, etc...
;; Electric parentheses
(setq cperl-electric-parens t)

;; Help on idle after 1s
(setq cperl-lazy-help-time 1)

;;;; Latex
(global-prettify-symbols-mode)
(use-package tex
  ;; In general I prefer the AUCTeX modes over their builtin counter parts
  ;; That is, LaTeX-mode over latex-mode etc.
  :straight nil
  :ensure auctex
  :config
  (add-hook 'LaTeX-mode-hook #'TeX-fold-mode)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (setq flymake-compiler "pdflatex")
              (setq flymake-args '("-interaction=nonstopmode" "%f"))))
  (add-to-list 'auto-mode-alist '("\\.TeX$" . LaTeX-mode))
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-process-asynchronous t
        TeX-check-TeX nil
        TeX-electric-sub-and-superscript t
        font-latex-fontify-sectioning 'color  ;; Disable variable font for sections
        TeX-engine 'default))
;; Useful (AUC)TeX commands
;;
;; C-c C-s      ::  Insert section.
;; C-c C-e      ::  Insert environment.
;; C-c C-m      ::  Insert macro.

;; C-c C-f C-b  ::  Bold.
;; C-c C-f C-i  ::  Italics.
;; C-c C-f C-e  ::  Emphasized.

;; C-c C-o C-f  ::  Fold mode.
;; C-c ~        ::  Math mode.
;; C-x * e      ::  calc-embedded.


;;;; Emacs Speaks Statistics
(use-package ess
  :config (require 'ess-r-mode))
;; (well also Python but that's not important right now)
(setq ess-ask-for-ess-directory t) ;; I actually like this feature!
(setq ess-local-process-name "R")
(setq ess-eval-visibly-p 'nowait) ;; No waiting whilst ESS is evaluating

(define-key ess-r-mode-map (kbd "M-?") nil) ;; unbinds M-?

;;  **************
;;; * Navigation *
;;  **************

;; Outline
;; Mainly used text based files.
(with-eval-after-load "outline"
  (define-key outline-minor-mode-map (kbd "C-c o")
              (lookup-key outline-minor-mode-map (kbd "C-c @"))))
(add-hook 'outline-minor-mode-hook
          (lambda ()
            (define-key outline-minor-mode-map (kbd "<tab>") 'outline-cycle)
            (define-key outline-minor-mode-map (kbd "S-<iso-lefttab>")
              '(lambda()
                 (interactive)
                 (save-excursion
                   (goto-char (point-min))
                   (outline-show-all)
                   (outline-hide-leaves))))))
(add-hook 'text-mode-hook #'outline-minor-mode)

;; Hide-show
;; Mainly used programming based files.
(define-key hs-minor-mode-map (kbd "C-c o")
            (lookup-key hs-minor-mode-map (kbd "C-c @")))
(setq hs-isearch-open t)    ;; Unhides comments and code when using isearch
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; Semantic mode
;; Language aware editing commands for:
;; C, C++, HTML,Java, Javascript, Make, Python, Scheme, SRecode, and Texinfo
(semantic-mode)

;; Search
(setq isearch-repeat-on-direction-change t
      isearch-wrap-pause 'no)
(global-set-key (kbd "C-s") #'isearch-forward)
(global-set-key (kbd "C-r") #'isearch-backward)

;; imenu
(defun me/imenu-function ()
  "Function imenu."
  (interactive)
  (let ((unread-command-events  (listify-key-sequence "Function\n") ))
    (call-interactively 'imenu)))

(defun me/imenu-variable ()
  "Variable imenu."
  (interactive)
  (let ((unread-command-events  (listify-key-sequence "Variable\n") ))
    (call-interactively 'imenu)))

(defun me/imenu-class ()
  "Class imenu."
  (interactive)
  (let ((unread-command-events  (listify-key-sequence "Class\n") ))
    (call-interactively 'imenu)))

(defun me/imenu-method ()
  "Method imenu."
  (interactive)
  (let ((unread-command-events  (listify-key-sequence "Method\n") ))
    (call-interactively 'imenu)))

(add-hook 'prog-mode-hook (lambda () (local-set-key (kbd "M-g f") #'me/imenu-function)))
(add-hook 'prog-mode-hook (lambda () (local-set-key (kbd "M-g v") #'me/imenu-variable)))
(add-hook 'prog-mode-hook (lambda () (local-set-key (kbd "M-g c") #'me/imenu-class)))
(add-hook 'prog-mode-hook (lambda () (local-set-key (kbd "M-g m") #'me/imenu-method)))

;; File navigation
(global-set-key (kbd "C-c f ,") 'find-file-other-window)
(global-set-key (kbd "C-c f p") 'find-file-at-point)
(global-set-key (kbd "C-c f v") 'view-file)

(recentf-mode 1)
(global-set-key (kbd "C-c f r") 'recentf-open-files)

;; Projects
(defun edit-projects ()
  "Edit the list of projects."
  (interactive)
  (find-file project-list-file))
(global-set-key (kbd "C-x p a") #'edit-projects)
(global-set-key (kbd "C-x p s") 'project-search)

;; Buffers
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-c b k") #'kill-buffer-and-window)
(global-set-key (kbd "C-c b ,") #'switch-to-buffer-other-window)
(global-set-key (kbd "C-c b v") #'view-buffer-other-window)
(global-set-key (kbd "M-[") #'previous-buffer)
(global-set-key (kbd "M-]") #'next-buffer)
(global-set-key (kbd "C-c b a") #'append-to-buffer)

(defun kill-this-buffer-reliably () (interactive) (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") #'kill-this-buffer-reliably)

;; Scratch buffer
(setq initial-major-mode #'org-mode)
(setq initial-scratch-message "")
(global-set-key (kbd "C-c b s") #'scratch-buffer)

(global-auto-revert-mode 1)
(setq midnight-mode t)
(midnight-delay-set 'midnight-delay "02:00am")

;;  *********
;;; * Dired *
;;  *********

(setq dired-listing-switches "-alh")

(setq dired-kill-when-opening-new-dired-buffer t)

(defun dired-sort-criteria (criteria)
  "Sort-dired by different CRITERIA by Robert Gloeckner."
  (interactive
   (list
    (or (completing-read "criteria [name]: "
             '("size(S)" "extension(X)" "creation-time(ct)"
               "access-time(ut)" "time(t)" "name()"))
    "")))
  (string-match ".*(\\(.*\\))" criteria)
  (dired-sort-other
   (concat dired-listing-switches
       (match-string 1 criteria))))

(setq global-auto-revert-non-file-buffers t)

(global-set-key (kbd "C-c d d") 'dired-default-directory-on-left)
(global-set-key (kbd "C-c d p") 'dired-at-point)
(global-set-key (kbd "C-c d f") 'find-dired)
(global-set-key (kbd "C-c d n") 'find-name-dired)
(global-set-key (kbd "C-c d w") 'wdired-change-to-wdired-mode)
(global-set-key (kbd "C-c d ,") 'dired-other-window)

;;  ***************
;;; * Development *
;;  ***************

;; Python
(require 'flymake)
(add-hook 'prog-mode-hook #'flymake-mode)
(setq flymake-start-on-flymake-mode t)
(setq python-flymake-command '("ruff" "check" "--output-format"
                               "concise" "--quiet"
                               "--exit-zero" "--select" "ALL"
                               "--ignore" "D407"
                               "--stdin-filename=stdin" "-"))

(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)
(define-key flymake-mode-map (kbd "C-c l b") 'flymake-show-buffer-diagnostics)
(define-key flymake-mode-map (kbd "C-c l p") 'flymake-show-project-diagnostics)

;;  *********
;;; * Tools *
;;  *********

;; Shell/Eshell
(require 'em-banner)
(setq eshell-banner-message "")

(require 'em-dirs)
(setq  eshell-list-files-after-cd t)

(global-set-key (kbd "C-c e") #'eshell)
(global-set-key (kbd "C-c s") #'shell)

;; Start an LLM-chat shell
(defvar lchat-model "nvidia/Llama-3.1-Nemotron-70B-Instruct-HF"
  "Default model for the lchat shell.")

(defun lchat ()
  "Start an LLM chat shell."
  (interactive)
  (let ((eshell-buffer-name "lchat"))
    (if (get-buffer eshell-buffer-name)
        (switch-to-buffer eshell-buffer-name)
      (progn
        (eshell)
        (insert (format "lchat -m %s" lchat-model))
        (eshell-send-input)))))
(global-set-key (kbd "C-c m l") #'lchat)

;; Visit init file
(defun my-visit-user-init-file ()
  "Visit the init file."
  (interactive)
  (find-file user-init-file))
(global-set-key (kbd "C-c f i") #'my-visit-user-init-file)

;; Calculator
(defun calc-or-quick-calc (arg)
  "Run calc or quick calc if ARG."
  (interactive "P")
  (if arg
      (call-interactively #'quick-calc)
    (call-interactively #'calc)))
(global-set-key (kbd "C-x c") 'calc-or-quick-calc)

;; Farenheit to Celcius calculator
(defun farenheit-to-celcius (temp)
  "Convert TEMP from Farenheit to Celcius."
  (/ (- temp 32) 1.8))

(defun farenheit-to-celcius-at-point ()
  "Get the Farenheit temperature at point as Celcius."
  (interactive)
  (let* ((temp-F (number-at-point))
         (temp-C (farenheit-to-celcius temp-F))
         (bounds (bounds-of-thing-at-point 'number)))
    (when bounds
      (message (format "%fF = %fC" temp-F temp-C))
      (if buffer-read-only
          (kill-new (format "%f" temp-C))
        (progn
          (delete-region (car bounds) (cdr bounds))
          (insert (format "%f" temp-C)))))))

;; Doc view
(require 'doc-view)
(setq doc-view-resolution 200)
(add-hook 'doc-view-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'doc-view-mode-hook #'doc-view-fit-page-to-window)
(defun doc-view-other-frame-scroll-up ()
  "Equivalent of switching to other frame, pressing SPC and then switching back."
  (interactive)
  (other-frame 1)
  (doc-view-scroll-up-or-next-page)
  (other-frame 1))
(defun doc-view-other-frame-scroll-down ()
  "Equivalent of switching to other frame, pressing p and then switching back."
  (interactive)
  (other-frame 1)
  (doc-view-scroll-down-or-previous-page)
  (other-frame 1))
(global-set-key (kbd "C-c p n") #'doc-view-other-frame-scroll-up)
(global-set-key (kbd "C-c p p") #'doc-view-other-frame-scroll-down)

;; Browser
(setq shr-width 70
      shr-cookie-policy nil
      eww-retrieve-command nil
      eww-browse-url-new-window-is-tab nil
      browse-url-browser-function #'eww)

(defun eww-search-scholar ()
  (interactive)
  (let ((eww-search-prefix "https://scholar.google.co.uk/scholar?q="))
    (call-interactively #'eww)))

(global-set-key (kbd "C-c u s") #'eww-search-scholar)
(global-set-key (kbd "C-c u u") #'eww)

;; Newsticker (RSS)
(global-set-key (kbd "C-c m n") #'newsticker-show-news)
(setq newsticker-frontend #'newsticker-treeview
      newsticker-hide-old-items-in-newsticker-buffer t)
(setq newsticker-url-list
      '(("npj imaging" "https://www.nature.com/npjimaging.rss" nil 3600)
        ("npj ml" "https://www.nature.com/natmachintell.rss" nil 3600)
        ("arxiv medphys" "https://rss.arxiv.org/rss//physics.med-ph" nil 3600)
        ("mr in med" "https://onlinelibrary.wiley.com/action/showFeed?jc=15222594&type=etoc&feed=rss" nil 3600)))

;; IRC
(require 'erc)
(load "~/.irc-auth")

(setq erc-autojoin-channels-alist
      '(("Libera.Chat"
         "#emacs"
         "#python"
         "#fortran"
         "#nhs-dev"
         "#math"
         "#machinelearning")))

(setq erc-modules '(netsplit
                    fill
                    button
                    match
                    track
                    completion
                    readonly
                    networks
                    ring
                    autojoin
                    noncommands
                    irccontrols
                    move-to-prompt
                    stamp
                    menu
                    list
                    sasl))

(setq erc-nick "TactfulCitrus"
      erc-port "6667"
      erc-server "irc.libera.chat"
      erc-try-new-nick-p nil
      erc-warn-about-blank-lines t
      erc-sasl-user erc-nick
      erc-sasl-password libera-chat-pass)

(defun irc () (interactive) (erc))
(global-set-key (kbd "C-c m i") #'irc)

;; Music
(use-package emms
  :ensure t
  :config
  (require 'emms-setup)
  (require 'emms-player-mplayer)
  (require 'emms-player-mpv)
  (emms-all)
  (if (eq system-type 'windows-nt)
      (setq emms-player-list '(emms-player-mplayer))
    (setq emms-player-list '(emms-player-mpv)
          emms-info-functions '(emms-info-native)
          emms-player-mpv-parameters '("--no-video"))))

(defvar emms-content-classicfm
  "https://media-ice.musicradio.com/ClassicFMMP3"
  "URL for a classical music radio station.")
(defun emms-play-classicfm ()
  "Play the ClassicFM radio."
  (interactive)
  (emms-play-url emms-content-classicfm))

(defvar emms-content-lofi
  "https://lofi.stream.laut.fm/lofi"
  "URL for a lofi music radio station.")
(defun emms-play-lofi ()
  "Play a LoFi radio station."
  (interactive)
  (emms-play-url emms-content-lofi))


(global-set-key (kbd "C-c m r c") #'emms-play-classicfm)
(global-set-key (kbd "C-c m r l") #'emms-play-lofi)
(global-set-key (kbd "C-c m s") #'emms-stop)

;;  ********
;;; * Mail *
;;  ********

;; Notmuch ;;
(use-package notmuch
  :straight nil)
(setq-default notmuch-search-oldest-first nil)
(global-set-key (kbd "C-c m m") #'notmuch)

;; Sometimes notmuch renders html really badly for the current colour scheme.
;; To remedy this, it's handy to open up the current email in a side buffer
;; and use =eww= to render the raw html.
(add-to-list 'display-buffer-alist
             '("html" . (display-buffer-same-window)))

(defun notmuch-view-html-in-eww-other-window ()
  "Display the current mail buffer in another window."
  (interactive)
  (other-window-prefix)
  (notmuch-show-view-raw-message)
  (shr-render-buffer (current-buffer)))

(global-set-key (kbd "C-c m v") #'notmuch-view-html-in-eww-other-window)

;; Sending Mail ;;
(use-package smtpmail)
(setq mail-user-agent 'message-user-agent)

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "127.0.0.1"
      smtpmail-smtp-service 1025
      message-kill-buffer-on-exit t)

;;  ********
;;; * Ebib *
;;  ********

(use-package ebib
  :after org
  :config
  (require 'org-ebib)
  (global-set-key (kbd "C-c m b") #'ebib)
  (global-set-key (kbd "C-c i") #'ebib-insert-citation)
  (setq ebib-save-xrefs-first nil
        ebib-index-default-sort '("Year" . descend)
        ebib-file-associations nil
        ebib-notes-display-max-lines 30))


(defvar ebib-paper-dir (expand-file-name "~/Documents/resources/papers")
  "Path to downloaded papers.")
(setq ebib-notes-directory (expand-file-name "~/Documents/notes/paper-notes")
      ebib-reading-list-file (expand-file-name "~/Documents/notes/reading-list.org")
      ebib-preload-bib-files `(,(expand-file-name "~/Documents/notes/refs.bib")))
(setq ebib-file-search-dirs `(,ebib-paper-dir))


;; Tries to download a paper associated with the url
;; Supports:
;;  * arXiv (https://arxiv.org/)
;;  * lingBuzz (https://ling.auf.net/lingBuzz/)
;;  * JSTOR (https://www.jstor.org/)
(add-hook 'ebib-index-mode-hook (lambda () (local-set-key (kbd "D") #'ebib-download-url)))
(add-hook 'ebib-index-mode-hook (lambda () (local-set-key (kbd "F") #'ebib-import-file)))

;; Converts DOI to bibtex
(defun doi2bibtex (doi)
  "Convert a DOI to a bibtex entry."
  (interactive "sDOI: ")
  (let ((url-request-method "GET")
        (url-mime-accept-string "application/x-bibtex"))
    (with-current-buffer (url-retrieve-synchronously (concat "https://doi.org/" doi))
      (goto-char (point-min))
      (re-search-forward "^$")
      (delete-region (point) (point-min))
      (re-search-forward ",")
      (downcase-region (point) (point-min))
      (kill-new (buffer-string))
      (message "DOI copied!"))))
(add-hook 'ebib-index-mode-hook (lambda () (local-set-key (kbd "C-d") #'doi2bibtex)))

;;  ***********
;;; * Windows *
;;  ***********

(global-set-key (kbd "C-M-w") 'scroll-other-window-down)

;; Default window navigation – simply switch to the next window in order.
;; Added for convenience; the default keybinding is "C-x o"
;; Which we'll rebind to follow-mode
;; Dired uses "C-o" to open file in other window so we'll need to unset that.
(add-hook 'dired-mode-hook
          (lambda ()
            (local-unset-key (kbd "C-o"))))
(global-set-key (kbd "C-o") #'other-window)
(global-set-key (kbd "M-o") #'previous-window-any-frame)
(global-set-key (kbd "C-M-o") #'other-frame)

;; Follow mode - for long files.
(global-set-key (kbd "C-x o") #'follow-mode)

;; Winner mode is handy for undo window changes.
(setq winner-mode t)

;; Minibuffers
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(savehist-mode 1)   ; Saves minibuffer history between sessions

(setq enable-recursive-minibuffers t
      minibuffer-depth-indicate-mode t
      minibuffer-follows-selected-frame nil)


;; Side Windows ;;
;;     ___________________________________
;;    |     *Buffer List*/*Ibuffer*       |
;;    |___________________________________|
;;    |     |                       |     |
;;    |  *  |                       |  *  |
;;    |  d  |                       |  T  |
;;    |  i  |                       |  a  |
;;    |  r  |   Main Window Area    |  g  |
;;    |  e  |                       |  s  |
;;    |  d  |                       |  *  |
;;    |  *  |                       |     |
;;    |_____|_______________________|_____|
;;    | *help*/*grep*/  |  *shell*/       |
;;    | *Completions*/  |  *compilation*/ |
;;    | *Calendar*      |                 |
;;    |_________________|_________________|
;;    |             Echo Area             |
;;    |___________________________________|

(setq fit-window-to-buffer-horizontally t)
(setq window-resize-pixelwise t)

(setq display-buffer-alist
      `(("\\*\\(?:Buffer List\\|Ibuffer\\)\\*"
         display-buffer-in-side-window
         (side . top)
         (slot . 1)
         (window-parameters . ((window-height . fit-window-to-buffer)
                               (preserve-size . (nil . t))
                               (no-other-window . nil))))
        ("\\*Tags List\\*"
         display-buffer-in-side-window
         (side . right)
         (slot . 1)
         (window-parameters . ((window-height . fit-window-to-buffer)
                               (window-width . fit-window-to-buffer)
                               (preserve-size . (t . nil))
                               (no-other-window . nil))))
        ("\\*\\(?:help\\|grep\\|Completions\\|*Calendar*\\)\\*"
         display-buffer-in-side-window
         (side . bottom)
         (slot . -1)
         (window-parameters . ((preserve-size . (nil . t))
                               (no-other-window . nil))))
        ("\\*\\(?:shell\\|compilation\\|eshell\\|eat\\)\\*"
         display-buffer-in-side-window
         (side . bottom)
         (slot . 1)
         (window-parameters . ((preserve-size . (nil . t))
                               (no-other-window . nil))))))

;; As dired buffers have no fixed names
(defun dired-default-directory-on-left ()
  "Display `default-directory' in side window on left, hiding details."
  (interactive)
  (let ((buffer (dired-noselect default-directory)))
    (with-current-buffer buffer (dired-hide-details-mode t))
    (display-buffer-in-side-window
     buffer `((side . left)
              (slot . 0)
              (window-parameters . ((window-width . fit-window-to-buffer)
                                    (preserve-size . (t . nil))
                                    (no-other-window . nil)))))))

(global-set-key (kbd "C-c w t") 'window-toggle-side-windows)

(setq switch-to-buffer-in-dedicated-window "pop"
      switch-to-buffer-obey-display-actions t)

;;  ************
;;; * Org Mode *
;;  ************

(variable-pitch-mode 0)
(auto-fill-mode 0)
(visual-line-mode 1)

(global-set-key (kbd "C-c f a") #'org-cycle-agenda-files)

(setq org-return-follows-link  t
      org-hide-emphasis-markers t)

(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s!)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

(require 'ox-md)

;; Bullets ;;
(use-package org-bullets)
(add-hook 'org-mode-hook
          (lambda ()
            (if (display-graphic-p)
                (org-bullets-mode)
              (org-bullets-mode -1))))

;; Clock ;;
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-clock-clocktable-default-properties '(:maxlevel 2 :scope subtree))

(global-set-key (kbd "C-c c r") 'org-clock-report)
(global-set-key (kbd "C-c c o") 'org-clock-out)
(global-set-key (kbd "C-c c l") 'org-clock-in-last)
(global-set-key (kbd "C-c c i") 'org-clock-in)
(global-set-key (kbd "C-c c g") 'org-clock-goto)

(defun mark-org-todo-as-started-and-clock-in ()
  "Mark a todo as started and clock in."
  (interactive)
  (if (string= major-mode "org-agenda-mode")
      (progn (org-agenda-todo "STARTED")
             (org-agenda-clock-in))
    (org-todo "STARTED")
    (org-clock-in)))

(global-set-key (kbd "C-c c s") 'mark-org-todo-as-started-and-clock-in)

;; Links ;;
(defun org-mode-url-at-point ()
  "Extract URL from =org-mode= link at point."
  (interactive "P")
  (let* ((link (org-element-lineage (org-element-context) '(link) t))
          (type (org-element-property :type link))
          (url (org-element-property :path link))
          (url (concat type ":" url)))
    url))

(defun me/org-link-copy (&optional arg)
  "Extract URL from org-mode link and add it to kill ring with optional ARG."
  (interactive "P")
  (let ((url (org-mode-url-at-point)))
    (kill-new url)
    (message (concat "Copied URL: " url))))

(with-eval-after-load "org"
  (define-key org-mode-map (kbd "C-x C-l") #'me/org-link-copy))

;; Browser in external browser
(defun browser-url-at-point-with-external-browser (&optional ARG)
  "Browse url with default browser passing ARG to browser."
  (interactive)
  (let ((url (org-mode-url-at-point)))
    (if url 
        (browse-url-default-browser url ARG)
      (error "No URL found"))))

(define-key org-mode-map (kbd "C-c u t") #'org-toggle-link-display)
(define-key org-mode-map (kbd "C-c u b") #'browser-url-at-point-with-external-browser)

;; Agenda ;;
(global-set-key (kbd "C-c m a") #'org-agenda)
(global-set-key (kbd "C-c m t") #'org-todo-list)
(setq org-deadline-warning-days 60)

;; Capture ;;
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Documents/notes/agenda.org" "Inbox")
         "* TODO %?\n")
        ("n" "Note" entry (file+headline "~/Documents/notes/agenda.org" "Inbox")
         "* %?\n")
        ("c" "Context Todo" entry (file+headline "~/Documents/notes/agenda.org" "Inbox")
         "* TODO (%(buffer-name (plist-get org-capture-plist :original-buffer))) %?\n")
        ("i" "Interrupting task" entry (file+headline "~/Documents/notes/agenda.org" "Inbox")
         "* STARTED %^{Task}\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
         :clock-in :clock-resume
         :prepend t)))
(global-set-key (kbd "C-c m c") #'org-capture)

;; By default org refile but the variable org-refile-targets can change that.
;; In changing this is also handy to view the outline path too.
;; Source : https://orgmode.org/manual/Refile-and-Copy.html#index-C_002dc-C_002dw-1
(setq org-refile-targets '((nil . (:maxlevel . 10)))
      org-refile-use-outline-path t
      org-outline-path-complete-in-steps nil)

;; HTML syntax highlighting
(use-package htmlize)

;;  ****************
;;; * Abbreviation *
;;  ****************

(load (concat user-emacs-directory "abbrevs.el"))

(setq-default abbrev-mode t)
(setq save-abbrevs nil)
(setq abbrev-suggest-hint-threshold 0)
(setq abbrev-suggest t)

;; Key binding for unexpanding abbrevs
(global-set-key (kbd "C-x a u") 'unexpand-abbrev)

(setq dabbrev-limit nil)  ;; No limit on searching back
(setq dabbrev-check-all-buffers t)


;;  ***********
;;; * STARTUP *
;;  ***********

(defun startup ()
    "Startup process."
    (interactive)
    (unless init-script-initial-clients
      (progn
        (irc)
        (newsticker-start)))
    (dashboard-open))
(startup)

;;  ***************
;;; * CUSTOM VARS *
;;  ***************

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("~/Documents/notes/agenda.org" "~/Documents/notes/reading-list.org"))
 '(package-selected-packages
   '(coterm powershell markdown-mode csv-mode ebib magit alda-mode))
 '(safe-local-variable-values '((eval outline-hide-sublevels 4))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; outline-regexp: ";;;\\*+\\|\\`"
;; eval: (outline-minor-mode 1)
;; eval: (outline-hide-sublevels 4)
;; End:
(provide 'init)
;;; init.el ends here
