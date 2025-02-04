;;; MyEmacsConfig --- a minimal cross platform config
;;
;; -*- lexical-binding: t -*-
;;
;;; Commentary:
;; It is meant to provide a decent working environment
;; that's fairly easy to manage.
;; In general, I try to use as many of the builtin features available
;; and tend to only use external package when it's much more convenient.

;;; Code:

;;  *****************************************
;;; * Local (system specific) configuration *
;;  *****************************************

(defmacro setq-if-defined (var val &optional arg)
  "Set VAR to VAL if it is defined else warn if ARG else raise an error."
  (if (boundp var)
      (list 'setq var val)
    (let ((warn-msg "Variable %s is not defined"))
      (if arg
          (warn warn-msg var)
        (error warn-msg var)))))

(defmacro setq-if-not-defined (var val)
  "Set VAR to VAL if it is not defined."
  (when (boundp var)
      (list 'setq var val)))

(defvar local/home-dir "~" "Home directory.")
(defvar wakatime-cli-path-rel nil "Relative path to wakatime-cli executable.")

(let ((local-conf (concat user-emacs-directory "local.el")))
  (when (file-exists-p local-conf)
    (load local-conf)))

;; Function for allowing per system home-directory
(defun in-home-dir (file)
  "Return the path of a FILE in the home directory as an absolute path."
  (concat (file-name-as-directory local/home-dir) file))

;;  **********
;;; * Server *
;;  **********

(require 'server)
(defvar init-script-initial-clients nil
    "Connected clients when init script was run.")
(setq init-script-initial-clients server-clients)
(unless init-script-initial-clients
  (progn
    (server-force-delete)
    (server-start)))

;;  **********************
;;; * Package Management *
;;  **********************

;; Ensure that Emacs can verify SSL/TLS certificates
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Package archives
(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))


;; Natively compile packages immediately after installation
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
;; Requires wakatime-cli (https://wakatime.com/emacs)
;; The wakatime login file sets the wakatime-api-key variable to the api key.
;; wakatime-cli must be specified in the users' path.
(let ((waka-login-file (concat user-emacs-directory ".waka.el")))
  (if (file-exists-p waka-login-file)
      (use-package wakatime-mode
        :init
        (setq wakatime-cli-path (in-home-dir wakatime-cli-path-rel))
        (load waka-login-file)
        :config
        (global-wakatime-mode))
    (message
     (concat "WakaTime not loaded as credentials not found in "
             waka-login-file))))

;; Magit ;;
(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1))
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
(setq default-directory (in-home-dir nil))

;; Info
(defvar info-custom-dir (in-home-dir ".emacs.d/info/")
  "Location of custom info directory.")
(add-to-list 'Info-directory-list info-custom-dir)
(when (eq system-type 'windows-nt)
    (add-to-list 'Info-directory-list
                 (in-home-dir "scoop/apps/emacs/current/share/info")))
(defun info-custom-manuals (manual)
  "Load an info MANUAL from the custom info directory."
  (info (concat info-custom-dir manual ".info")))
(defun info-custom-python ()
  "Launch python info file."
  (interactive)
  (info-custom-manuals "python"))
(global-set-key (kbd "C-c h p") #'info-custom-python)

;; Sets the auth source (requires gpg!)
;; loopback is needed if GPG requires a password for decrypting keys
(setq auth-sources `(,(in-home-dir ".authinfo.gpg"))
      epa-gpg-program "gpg"
      epa-pinentry-mode 'loopback)

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
(setq frame-title-format "The Editor of Saint IGNUcius")

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
(setq-default header-line-format mode-line-format) ; Moves modeline to the top
(setq mode-line-format nil) ; Removes the modeline from the bottom
(display-time-mode 1)       ; Displays the time.
(display-battery-mode 1)    ; Displays the battery.
(size-indication-mode 1)    ; Show the buffer size in the modeline
(column-number-mode 1)      ; Show column number with line number in modeline

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
(defvar me/current-theme me/dark-theme "Current theme.")

(defun me/load-theme ()
  "Load the appropriate theme based on the current theme."
  (load-theme me/current-theme t))

(defun toggle-theme ()
  "Toggle the theme between dark and light."
  (interactive)
  (if (eq me/current-theme me/dark-theme)
        (setq me/current-theme me/light-theme)
      (setq me/current-theme me/dark-theme))
  (me/load-theme))
(me/load-theme)

;;  As a quick and convenient test, this line happens to be 79 characters long.
(add-to-list 'default-frame-alist '(font . "Monospace 13"))
(add-to-list 'default-frame-alist '(width . 79))

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

(delete-selection-mode 1) ;; Replace highlighted text rather than just inserted.

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

;; Revert the buffer
(global-set-key (kbd "C-c b r") #'revert-buffer)

;; Copy the current buffer's filename to the kill ring.
(defun copy-filename-to-kill ()
  "Copy the current buffer's filename to the kill ring."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (stringp file-name)
        (kill-new (buffer-file-name))
      (message "Current buffer is not a file."))))
(global-set-key (kbd "C-c f w") #'copy-filename-to-kill)


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
      completion-styles '(basic partial-completion initials substring))
;; Used to use fido-vertical but that has difficulty when not selecting an item
;; from the completion list.  That is, when rather than selecting "fo" if "foo"
;; is present "foo" will always be selected.
;; Hence I switched to icomplete.
;; Similarly, flex is too aggressive a completion match so I've removed that.
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
(global-set-key (kbd "C-c d s") #'ispell)
(global-set-key (kbd "C-c d w") #'ispell-word)
(global-set-key (kbd "C-c d c") #'ispell-comments-and-strings)
(global-set-key (kbd "C-c d b") #'ispell-buffer)

(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)


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
  "Use flat indexing for imenu."
  (setq imenu-create-index-function
        #'python-imenu-create-flat-index))

(add-hook 'python-mode-hook
          #'python-imenu-use-flat-index)

(global-set-key (kbd "C-c g d") #'pdb)
(setq gud-pdb-command-name "poetry run python -m pdb")
(setq python-shell-interpreter "ipython")

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
        ;; Disable variable font for sections
        font-latex-fontify-sectioning 'color
        TeX-engine 'default))

(setq org-latex-listings 'minted
    org-latex-packages-alist '(("newfloat" "minted"))
    org-latex-pdf-process
    '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
      "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
)

;; Useful (AUC)TeX commands
;;
;; C-c C-s      ::  Insert section.
;; C-c C-e      ::  Insert environment.
;; C-c C-m      ::  Insert macro.

;; C-c C-a      ::  Compile all.
;; C-c C-c      ::  Compile.
;; C-c C-b      ::  Compile buffer.
;; C-c C-z      ::  Compile section.
;; C-c C-r      ::  Compile region.

;; C-c C-f C-b  ::  Bold.
;; C-c C-f C-i  ::  Italics.
;; C-c C-f C-e  ::  Emphasized.

;; C-c C-o C-f  ::  Fold mode.
;; C-c ~        ::  Math mode.
;; C-x * e      ::  calc-embedded.

;; C-c C-p C-d  ::  Preview document.
;; C-c C-p C-p  ::  Preview at point.

;; For viewing latex commands outside of org-mode buffers,
;; it's useful to bind to the org-latex-preview function globally.
(global-set-key (kbd "C-c C-x C-l") #'org-latex-preview)


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
(require 'cedet)
(setq semantic-default-submodes
      '(;; Perform semantic actions during idle time
        global-semantic-idle-scheduler-mode
        ;; Use a database of parsed tags
        global-semanticdb-minor-mode
        ;; Decorate buffers with additional semantic information
        global-semantic-decoration-mode
        ;; Highlight the name of the function you're currently in
        global-semantic-highlight-func-mode
        ;; show the name of the function at the top in a sticky
        global-semantic-stickyfunc-mode
        ;; Generate a summary of the current tag when idle
        global-semantic-idle-summary-mode
        ;; Show a breadcrumb of location during idle time
        global-semantic-idle-breadcrumbs-mode
        ;; Switch to recently changed tags with `semantic-mrub-switch-tags',
        ;; or `C-x B'
        global-semantic-mru-bookmark-mode))
(add-hook 'emacs-lisp-mode-hook #'semantic-mode)
(add-hook 'python-mode-hook #'semantic-mode)
(add-hook 'html-mode-hook #'semantic-mode)

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

(add-hook 'prog-mode-hook
          (lambda () (local-set-key (kbd "M-g f") #'me/imenu-function)))
(add-hook 'prog-mode-hook
          (lambda () (local-set-key (kbd "M-g v") #'me/imenu-variable)))
(add-hook 'prog-mode-hook
          (lambda () (local-set-key (kbd "M-g c") #'me/imenu-class)))
(add-hook 'prog-mode-hook
          (lambda () (local-set-key (kbd "M-g m") #'me/imenu-method)))

;; File navigation
(global-set-key (kbd "C-c f ,") 'find-file-other-window)
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

(defun kill-this-buffer-reliably ()
  "Reliably kill this buffer."
  (interactive) (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") #'kill-this-buffer-reliably)

;; Scratch buffer
(defun note-buffer ()
  "Create a scratch buffer for 'org-mode' notes."
  (interactive)
  (let ((initial-major-mode #'org-mode)
        (initial-scratch-message "#+title: Notes\n"))
    (call-interactively #'scratch-buffer)))
(setq initial-major-mode #'emacs-lisp-mode)
(setq initial-scratch-message ";;; Scratch --- A Scratch Pad for Elisp Code\n")
(global-set-key (kbd "C-c b s") #'scratch-buffer)
(global-set-key (kbd "C-c b n") #'note-buffer)

(global-auto-revert-mode 1)
(setq midnight-mode t)
(midnight-delay-set 'midnight-delay "02:00am")

;;  *********
;;; * Dired *
;;  *********

;; Loads useful functions.
;; Came for dired-do-find-marked-files
;; Stayed for the file omition, virtual-dired buffers and dired-x-find-file
(with-eval-after-load 'dired
  (setq dired-x-hands-off-my-keys nil)
  (require 'dired-x)
  (dired-x-bind-find-file)
  (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1))))

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
(global-set-key (kbd "C-c d e") 'wdired-change-to-wdired-mode)
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
(defvar lchat-model nil "Default model for the lchat shell.")
(setq-if-not-defined lchat-model "deepseek-ai/DeepSeek-R1")

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
(global-set-key (kbd "C-c l l") #'lchat)

;; GPTel
(use-package gptel)
(setq gptel-model 'deepseek-r1:latest)
(setq gptel-backend (gptel-make-ollama
                     "Ollama"
                     :host "localhost:11434"
                     :stream t
                     :models`(,gptel-model)))
(global-set-key (kbd "C-c l g") #'gptel)
(global-set-key (kbd "C-c l s") #'gptel-send)

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
  "Same as switching to other frame, pressing SPC and then switching back."
  (interactive)
  (other-frame 1)
  (doc-view-scroll-up-or-next-page)
  (other-frame 1))
(defun doc-view-other-frame-scroll-down ()
  "Same as switching to other frame, pressing p and then switching back."
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

(defun eww-search-wiki ()
  "Search Wikipedia."
  (interactive)
  (let ((eww-search-prefix
         "https://en.wikipedia.org/wiki/Special:Search?go=Go&search="))
    (call-interactively #'eww)))

(defun eww-search-scholar ()
  "Search Google Scholar."
  (interactive)
  (let ((eww-search-prefix "https://scholar.google.co.uk/scholar?q="))
    (call-interactively #'eww)))

(defun eww-search-pypi ()
  "Search PyPi."
  (interactive)
  (let ((eww-search-prefix "https://pypi.org/search/?q="))
    (call-interactively #'eww)))

(global-set-key (kbd "C-c u w") #'eww-search-wiki)
(global-set-key (kbd "C-c u s") #'eww-search-scholar)
(global-set-key (kbd "C-c u p") #'eww-search-pypi)
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

(setq erc-autojoin-channels-alist
      '(("Libera.Chat"
         "#emacs"
         "#python"
         "#fortran"
         "##forth"
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
      erc-tls-verify t
      erc-try-new-nick-p nil
      erc-warn-about-blank-lines t
      erc-sasl-user erc-nick)

;; The irc-auth-file should look something like:
;; (setq libera-chat-pass my-libera-chat-passowrd)
(let ((irc-auth-file (concat user-emacs-directory ".irc-auth.gpg")))
  (when (file-exists-p irc-auth-file)
    (load irc-auth-file)
    (setq erc-sasl-password libera-chat-pass)))


(defun irc () "Connect to default IRC client." (interactive) (erc))
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

(defvar emms-content-classic
  "http://relax.stream.publicradio.org/relax.mp3"
  "URL for a classical music radio station.")
(defun emms-play-classic ()
  "Play the ClassicFM radio."
  (interactive)
  (emms-play-url emms-content-classic))

(defvar emms-content-lofi
  "https://lofi.stream.laut.fm/lofi"
  "URL for a lofi music radio station.")
(defun emms-play-lofi ()
  "Play a LoFi radio station."
  (interactive)
  (emms-play-url emms-content-lofi))


(defvar emms-content-bosa
  "https://centova5.transmissaodigital.com:20104/live"
  "URL for a bosa nova radio station.")
(defun emms-play-bosa ()
  "Play a Bosa Nova radio station."
  (interactive)
  (emms-play-url emms-content-bosa))

(defvar emms-content-smooth
  "http://playerservices.streamtheworld.com/pls/977_SMOOJAZZ.pls"
  "URL for a Smooth Jazz radio station.")
(defun emms-play-smooth ()
  "Play a smooth jazz radio station."
  (interactive)
  (emms-play-url emms-content-smooth))

(defvar emms-content-tradjazz
  "https://icecast.walmradio.com:8443/classic"
  "URL for a traditional Jazz radio station.")
(defun emms-play-tradjazz ()
  "Play a Traditional Jazz radio station."
  (interactive)
  (emms-play-url emms-content-tradjazz))

(defvar emms-content-jazz
  "https://icecast.walmradio.com:8443/jazz"
  "URL for a Jazz radio station.")
(defun emms-play-jazz ()
  "Play a Jazz radio station."
  (interactive)
  (emms-play-url emms-content-jazz))

(defvar emms-content-news
  "https://media-ssl.musicradio.com/LBCUKMP3"
  "URL for a news radio station.")
(defun emms-play-news ()
  "Play a news radio station."
  (interactive)
  (emms-play-url emms-content-news))

(defvar emms-content-funk
  "http://jazz-wr06.ice.infomaniak.ch/jazz-wr06-128.mp3"
  "URL for a funk radio station.")
(defun emms-play-funk ()
  "Play a funk radio station."
  (interactive)
  (emms-play-url emms-content-funk))



(global-set-key (kbd "C-c m r b") #'emms-play-bosa)
(global-set-key (kbd "C-c m r c") #'emms-play-classic)
(global-set-key (kbd "C-c m r f") #'emms-play-funk)
(global-set-key (kbd "C-c m r j") #'emms-play-jazz)
(global-set-key (kbd "C-c m r l") #'emms-play-lofi)
(global-set-key (kbd "C-c m r n") #'emms-play-news)
(global-set-key (kbd "C-c m r s") #'emms-play-smooth)
(global-set-key (kbd "C-c m r t") #'emms-play-tradjazz)

(global-set-key (kbd "C-c m s") #'emms-stop)

;;  ********
;;; * Mail *
;;  ********

;; Notmuch ;;
(use-package notmuch)
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


(defvar ebib-paper-dir (in-home-dir "Documents/resources/papers")
  "Path to downloaded papers.")
(setq ebib-notes-directory (in-home-dir "Documents/notes/paper-notes")
      ebib-reading-list-file (in-home-dir
                              "Documents/notes/reading-list.org")
      ebib-preload-bib-files `(,(in-home-dir
                                 "Documents/notes/refs.bib")))
(setq ebib-file-search-dirs `(,ebib-paper-dir))


;; Tries to download a paper associated with the url
;; Supports:
;;  * arXiv (https://arxiv.org/)
;;  * lingBuzz (https://ling.auf.net/lingBuzz/)
;;  * JSTOR (https://www.jstor.org/)
(add-hook 'ebib-index-mode-hook
          (lambda () (local-set-key (kbd "D") #'ebib-download-url)))
(add-hook 'ebib-index-mode-hook
          (lambda () (local-set-key (kbd "F") #'ebib-import-file)))

;; Copies the ebib key to the kill ring
(add-hook 'ebib-index-mode-hook
          (lambda () (local-set-key (kbd "C-w") #'ebib-copy-key-as-kill)))

;; Converts DOI to bibtex
(defun doi2bibtex (doi)
  "Convert a DOI to a bibtex entry."
  (interactive "sDOI: ")
  (let ((url-request-method "GET")
        (url-mime-accept-string "application/x-bibtex"))
    (with-current-buffer (url-retrieve-synchronously
                          (if (string-prefix-p "http" doi)
                              doi
                            (concat "https://doi.org/" doi)))
      (goto-char (point-min))
      (re-search-forward "^$")
      (delete-region (point) (point-min))
      (re-search-forward ",")
      (downcase-region (point) (point-min))
      (kill-new (buffer-string))
      (message "DOI copied!"))))
(add-hook 'ebib-index-mode-hook
          (lambda () (local-set-key (kbd "C-d") #'doi2bibtex)))

(defun ebib-insert-latex-ref-other-window ()
  "Insert the current item's reference and citation in the other window.

This is a very me specific function.  It basically, gets the current paper
in ebib and pastes the paper title followed by the latex citation in the
other window, switches back to the ebib window and goes to the next entry.

The idea is when you want to quickly add a lot of papers to a latex document
with some rough idea of what the papers were about."
  (interactive)
  (ebib-copy-reference-as-kill)
  (ebib-copy-key-as-kill)
  (other-window 1)
  (yank 2)
  (insert " ~\\cite{")
  (yank 0)
  (insert "}\n")
  (other-window I-1)
  (ebib-next-entry))
(add-hook 'ebib-index-mode-hook
          (lambda ()
            (local-set-key (kbd "C-y") #'ebib-insert-latex-ref-other-window)))

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
      org-hide-emphasis-markers t
      org-hide-leading-stars t
      org-indent-indentation-per-level 1
      org-latex-caption-above nil
      org-html-table-caption-above nil
      org-startup-indented t)

(setq org-todo-keywords
      '((sequence
         "TODO(t)" "STARTED(s!)" "WAITING(w@/!)"
         "|" "DONE(d!)" "CANCELLED(c@)")))

(require 'ox-md)

;; Publishing
(require 'ox-publish)
(use-package simple-httpd
  :ensure t)

;; Babel
(setq org-babel-load-languages '((emacs-lisp . t)
                                 (forth . t)
                                 (lisp . t)
                                 (makefile . t)
                                 (octave . t)
                                 (python . t)
                                 (R . t)
                                 (shell . t)
                                 (sql . t)
                                 (sqlite . t)))

(if (eq system-type 'windows-nt)
    (use-package ob-powershell
      :config
      ;; I would never have powershell or matlab installed on a linux system
      (add-to-list 'org-babel-load-languages '(powershell . t))
      (add-to-list 'org-babel-load-languages '(matlab . t)))
  (use-package ob-nix
    :config
    ;; Currently nix is only supported for unix systems
    (add-to-list 'org-babel-load-languages '(nix . t))))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)

;; Stops asking for confirmation for every source block execution
(setq org-confirm-babel-evaluate nil)


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
  "Extract URL from 'org-mode' link and add it to kill ring with optional ARG."
  (interactive "P")
  (let ((url (org-mode-url-at-point)))
    (kill-new url)
    (message (concat "Copied URL: " url))))

(with-eval-after-load "org"
  (define-key org-mode-map (kbd "C-c l l") #'me/org-link-copy))

;; Browser in external browser
(defun browser-url-at-point-with-external-browser (&optional ARG)
  "Browse url with default browser passing ARG to browser."
  (interactive)
  (let ((url (org-mode-url-at-point)))
    (if url
        (browse-url-default-browser url ARG)
      (error "No URL found"))))

(define-key org-mode-map (kbd "C-c u t") #'org-toggle-link-display)
(define-key org-mode-map (kbd "C-c u b")
            #'browser-url-at-point-with-external-browser)

;; Agenda ;;
(setq org-agenda-files
   `(,(in-home-dir "Documents/notes/agenda.org")
     ,(in-home-dir "Documents/notes/reading-list.org")))

(global-set-key (kbd "C-c m a") #'org-agenda)
(global-set-key (kbd "C-c m t") #'org-todo-list)
(setq org-deadline-warning-days 60)

;; Capture ;;
(setq org-capture-templates
      `(("t" "Todo" entry
         (file+headline ,(in-home-dir "Documents/notes/agenda.org") "Inbox")
         "* TODO %?\n")
        ("n" "Note" entry
         (file+headline ,(in-home-dir "Documents/notes/agenda.org") "Inbox")
         "* %?\n")
        ("c" "Context Todo" entry
         (file+headline ,(in-home-dir "Documents/notes/agenda.org") "Inbox")
         ,(concat
           "* TODO ("
           "%(buffer-name (plist-get org-capture-plist :original-buffer))"
           ") %?\n"))
        ("i" "Interrupting task" entry
         (file+headline ,(in-home-dir "Documents/notes/agenda.org") "Inbox")
         "* STARTED %^{Task}\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
         :clock-in :clock-resume
         :prepend t)
        ("r" "Reflection" entry
         (file+headline
          ,(in-home-dir "Documents/notes/agenda.org") "Reflections")
         ;; Uses the Driscoll Model:- one of the simplest models
         ;; and involves three stem questions which are;
         ;; what, so what and now what?
         ,(concat
           "* TODO %^{Title: }%?\n"
           "SCHEDULED: <%(org-read-date nil nil \"+83d\")> "
           "DEADLINE: <%(org-read-date nil nil \"+90d\")>\n"
           "*** /What?/\n%^{What: }\n"
           "*** /So What?/\n%^{So What: }\n"
           "*** /Now What?/\n%^{Now What: }\n"
           "*** /3 month update:/\n"))
        ("p" "Continuous Personal Development" entry
         (file+headline ,(in-home-dir "Documents/notes/agenda.org") "CPD")
         ,(concat
           "* %^{Title: }%?\t"
           "%(org-read-date nil nil \"+0d\")\t"
           ":%^{Tag: |work|professional|formal|self|other}:\n"
           "- /Benefit to me?/\n%^{Benefit to me: }\n"
           "- /Benefit to person/people/organisation?/\n%^{Benefit to org: }\n"
           "- /Where to find evidence?/\n%^{Where to find evidence: }\n"
           "- /Time spent (in hours)/ :: %^{Time spent: }\n"))))
(global-set-key (kbd "C-c m c") #'org-capture)

;; By default org refile but the variable org-refile-targets can change that.
;; In changing this is also handy to view the outline path too.
;; Source :
;; https://orgmode.org/manual/Refile-and-Copy.html#index-C_002dc-C_002dw-1
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

;;  *************
;;; * Templates *
;;  *************
(let ((templates-file (concat user-emacs-directory "templates.el")))
  (when (file-exists-p templates-file)

    (defmacro deftemplate (name content)
      """Macro for defining new templates."
      (list 'defun (intern (format "templates/%s" name)) ()
            (list 'interactive)
            (list 'kill-new content)
            (list 'message "Template copied")))

      (load templates-file)))


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
 '(package-selected-packages
   '(gptel yaml-mode which-key wakatime-mode toml-mode simple-httpd page-break-lines ob-powershell notmuch multiple-cursors json-mode htmlize git-timemachine forge fireplace expand-region ess emms ebib dashboard csv-mode auctex)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
(provide 'init)
;;; init.el ends here
