;;; MyEmacsConfig --- a minimal cross platform config -*- lexical-binding: t -*-
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
  (unless (boundp var)
      (list 'setq var val)))

(defmacro setv (var val &optional desc)
  "defvar VAR to VAL with DESC if not define - else setq"
  (if (boundp var)
      (list 'setq var val)
    (list 'defvar var val desc)))

(defmacro setq-if-nil (var val)
  "Set VAR to VAL if VAR is nil."
  `(unless ,var
     (setq ,var ,val)))

(defmacro global-set-keys-to-prefix (prefix keybindings)
  "Set functions in KEYBINDINGS to a PREFIX key."
  `(dolist (elm ,keybindings)
     (let ((key (format "%s %s" ,prefix (car elm)))
           (fn (cdr elm)))
       (global-set-key (kbd key) fn))))

(defmacro defun-surely (func)
  "Create a function NAME that run FUNC if the user is sure."
  `(defun ,(intern (format "%s-surely" func)) ()
     ,(format "Run %s when the user is sure." func)
     (interactive)
     (when (yes-or-no-p "Surely you can't be serious? ")
       (,func))))

(defun add-to-list-multiple (list to-add)
  "Add TO-ADD to LIST.
Allows for adding a sequence of items to the same list, rather
than having to call `add-to-list' multiple times."
  (interactive)
  (dolist (item to-add)
    (add-to-list list item)))

(defvar local/home-dir "~" "Home directory.")
(defvar wakatime-cli-path-rel nil "Relative path to wakatime-cli executable.")

(defvar local/habitica-uid nil "Habitica UID.")
(defvar local/habitica-token nil "Habitica Token.")

;; Important files that need to be present.
(defvar local/agenda-file nil "Local agenda file.")
(defvar local/reading-list nil "Local reading-list file.")
(defvar local/bib-file nil "Local bibliography file.")
(defvar local/paper-dir nil "Local directory for papers.")
(defvar local/paper-notes-dir  nil "Local paper directory for notes.")

(let ((local-conf (concat user-emacs-directory "local.el")))
  (when (file-exists-p local-conf)
    (load local-conf)))

;; Function for allowing per system home-directory
(defun in-home-dir (file)
  "Return the path of a FILE in the home directory as an absolute path."
  (concat (file-name-as-directory local/home-dir) file))

(setq-if-nil local/agenda-file (in-home-dir "Documents/notes/agenda.org"))
(setq-if-nil local/reading-list
             (in-home-dir "Documents/notes/reading-list.org"))
(setq-if-nil local/bib-file (in-home-dir "Documents/notes/refs.bib"))
(setq-if-nil local/paper-dir (in-home-dir "Documents/resources/papers"))
(setq-if-nil local/paper-notes-dir (in-home-dir "Documents/notes/paper-notes"))


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

(defmacro run-after-first-frame-connection (func)
  "Run FUNC after first frame is loaded."
  (let ((num-clients init-script-initial-clients)
        (*func-has-run* nil))
    `(lambda ()
       (message (format "%s has run? %s" ',func *func-has-run*))
       (if (and num-clients *func-has-run*)
           (progn
             (,func)
             (setq *func-has-run* t))))))


;;  **********************
;;; * Package Management *
;;  **********************

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

;; Ensure that Emacs can verify SSL/TLS certificates
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Package archives
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Manually update org-mode to the latest version
(package-upgrade 'org)

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


;;  ***************
;;; * File System *
;;  ***************

;; Tramp ;;
;; For now I'll use scp but after Emacs 30.2 or Tramp 2.7.3
;; then rsync will be fixed:
;; https://lists.gnu.org/r/tramp-devel/2025-06/msg00001.html
(setq vc-handled-backends '(Git)
      remote-file-name-inhibit-locks t
      tramp-use-scp-direct-remote-copying t
      remote-file-name-inhibit-auto-save-visited t
      tramp-copy-size-limit (* 1024 1024) ;; 1MB
      tramp-verbose 2
)

(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))

(connection-local-set-profiles
 '(:application tramp :protocol "scp")
 'remote-direct-async-process)

;; VC Mode ;;
;; More often than not I find myself returning to the built in features of Emacs
;; The most controversial is perhaps with using vc over magit.
;; I find Magit hangs more than vc-dir-mode and often fails more - with errors
;; such as waiting for Emacs to close the commit editor.
;; I also feel that Emacs' vc encourages me to make small commits on the file
;; regularly without getting in my way.
;; I digress.
(require 'vc-dir)
(setq vc-annotate-background-mode t)

(defun my/vc-diff-against-main ()
  "Diff current project against main branch."
  (interactive)
  (let ((default-branch "main")
        (current-branch (car (vc-git-branches))))
    (vc-root-version-diff t default-branch current-branch)))

; Conventional Commit Templates
(setv conventional-commit/types
  '("fix"
    "feat"
    "build"
    "chore"
    "ci"
    "docs"
    "style"
    "refactor"
    "perf"
    "test"
    "revert"
    "release"
    "deps"
    "security"
    "config"
    "data"
    "analysis")
  "Allowed conventional commit types.")

(defun conventional-commit/insert-template ()
  "Insert conventional commit message."
  (interactive)
  (let* ((type-possibly-breaking (completing-read
                                 "Type: " conventional-commit/types nil t))
         (is-breaking (yes-or-no-p "Breaking? "))
         (breaking-desc (if is-breaking
                            (format "\nBREAKING CHANGE: %s"
                                    (read-string
                                     "Breaking Change Description: "))
                          ""))
         (fixes (if (string= type-possibly-breaking "fix")
                    (format "\nFixes: %s" (read-string "Bug issue: "))
                  ""))
         (issue-closed? (read-string "Issue closed? "))
         (closes (if (string-empty-p issue-closed?)
                     ""
                   (format "\nCloses: %s" issue-closed?)))
         (related-to? (read-string "Related to: "))
         (related-to (if (string-empty-p related-to?)
                         ""
                       (format "\nRelated to %s" related-to?)))
         (footer (concat breaking-desc related-to fixes closes))
         (type (if is-breaking
                   (format "%s!" type-possibly-breaking)
                 type-possibly-breaking))
         (possible-scope (read-string "Scope (optional): "))
         (scope (if (string-empty-p possible-scope)
                    ""
                  (format "(%s)" possible-scope)))
         (description (read-string "Description: ")))
    (insert (format "%s%s: %s\n\n%s" type scope description footer)))
  ;; Moves to the correct position after template insertion
  (beginning-of-buffer)
  (next-line 2))
(require 'vc-git)
(define-key vc-git-log-edit-mode-map (kbd "C-c C-t")
            #'conventional-commit/insert-template)

;; smerge-mode ;;
;; The default smerge-mode prefix (C-c ^) is arthritic inducing
;; for big conflicts.
(setq smerge-command-prefix (kbd "C-c d"))

;; File modes ;;
;; No config needed - just needed for the file types.
(use-package csv-mode)
(use-package yaml-mode)
(use-package toml-mode)
(use-package json-mode)
(use-package markdown-mode)
(use-package dotenv-mode)


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
(require 'info)
(defvar info-custom-dir (in-home-dir ".emacs.d/info/")
  "Location of custom info directory.")
(add-to-list 'Info-directory-list info-custom-dir)
(when (eq system-type 'windows-nt)
    (add-to-list 'Info-directory-list
                 (in-home-dir "scoop/apps/emacs/current/share/info")))
(define-key Info-mode-map (kbd "M-p") #'Info-up)

;; Sets the auth source (requires gpg!)
;; loopback is needed if GPG requires a password for decrypting keys
(setq auth-sources `(,(in-home-dir ".authinfo.gpg"))
      epa-gpg-program "gpg"
      epa-pinentry-mode 'loopback)

;; Saves session
;; In general, I like option to load a previously saved session
;; in the event of crashes or when I need to jump right back into
;; where I was.
;; But I don't like this behavior by default as I feel it prevents me from
;; properly planning.
;; Hence, I'll disable desktop-save-mode but call it in the startup hook.
;; That way I get desktop auto-saving without loading previous desktops
;; by default.
;; Auto-save is turned on in the startup hook.
(desktop-save-mode nil)

;; Works with daemons but not remotely running Emacs sessions.
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

;; Which-Key ;;
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.1))

;; Frame
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq frame-title-format "The Holy Editor of Saint IGNUcius")

;; Basic
(setq inhibit-startup-message t
      visible-bell t
      confirm-kill-emacs nil
      global-tab-line-mode nil
      tab-line-close-button-show nil
      tab-line-tabs-function 'tab-line-tabs-mode-buffers
      truncate-lines t
      x-stretch-cursor t
      use-dialog-box nil)

(defun get-buffers-matching-current-mode ()
  "Return a list of buffers where their major-mode is equal to the current."
  (seq-filter (lambda (b) (derived-mode-p
                      (with-current-buffer b major-mode)))
              (buffer-list)))

(tab-line-mode -1)
(defun current-major-mode-tab-line-mode ()
  "Toggle 'tab-line-mode' for buffers with the same major mode as the current."
  (interactive)
  (let ((*buffers* (get-buffers-matching-current-mode))
        (*onoff* (if tab-line-mode -1 1)))
    (dolist (b *buffers*)
      (set-buffer b)
      (tab-line-mode *onoff*))
    (set-buffer (car *buffers*))))

(defun-surely save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") #'save-buffers-kill-terminal-surely)

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
(setq-default header-line-format nil)
(display-time-mode 1)       ; Displays the time.
(display-battery-mode 1)    ; Displays the battery.
(size-indication-mode 1)    ; Show the buffer size in the modeline
(column-number-mode 1)      ; Show column number with line number in modeline

;; Tab bar
(setq tab-bar-position t
      tab-bar-show 1
      tab-bar-close-button-show nil
      tab-bar-format '(tab-bar-format-history
                       tab-bar-format-tabs-groups
                       tab-bar-separator)
      tab-bar-new-tab-choice #'scratch-buffer
      tab-bar-new-tab-to 'rightmost)
(global-set-key (kbd "C-M-l") #'tab-previous)

;; Programming UI
(when (display-graphic-p)
    (add-hook 'prog-mode-hook #'whitespace-mode))
(setq display-raw-bytes-as-hex t)

;; Line numbers
(column-number-mode)
(setq display-line-numbers-type 'relative)
(setq-default display-line-numbers-width 4) ;; # lines > 9999 => rethink life.
(global-display-line-numbers-mode t)

;; Use spaces not tabs - this is the hill I will die on.
(setq-default c-basic-offset 4
	      tab-width 4
	      indent-tabs-mode nil
          tab-always-indent 'complete)
(add-hook 'org-mode #'(lambda () (setq-local tab-width 8)))

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
(if (eq system-type 'windows-nt)
    (set-face-attribute 'default nil
                        :family "Consolas" :height 110)
  (progn
    (add-to-list 'default-frame-alist '(font . "Monospace-10:light"))
    (add-to-list 'default-frame-alist '(width . 79))))

;; Highlighting changes
(setq highlight-changes-mode t)


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

;; Copy the current buffer's filename to the kill ring.
(defun copy-filename-to-kill ()
  "Copy the current buffer's filename to the kill ring."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (stringp file-name)
        (kill-new (buffer-file-name))
      (message "Current buffer is not a file."))))


;;  ********************
;;; * Advanced Editing *
;;  ********************

;; Completion
;; Used to use fido-vertical but that has difficulty when not selecting an item
;; from the completion list.  That is, when rather than selecting "fo" if "foo"
;; is present "foo" will always be selected.
;; Hence I switched to icomplete.
;; Similarly, flex is too aggressive a completion match so I've removed that.
;; The vertical completion takes up more screen real estate so I ignored that.
(require 'icomplete)
(setq icomplete-mode t
      icomplete-in-buffer t
      icomplete-compute-delay 0.01
      icomplete-delay-completions-threshold 10000
      icomplete-show-matches-on-no-input t
      icomplete-hide-common-prefix nil
      read-buffer-completion-ignore-case t
      completion-ignore-case t
      completion-auto-help t
      completion-styles '(basic
                          partial-completion
                          initials
                          substring
                          shorthand))

;; read-symbol-shorthands must only be set as a file local-variable
;; hence we define a function for quickly doing this.
(defun add-file-local-variable-shorthand ()
  "Add a file-local shorthand variable."
  (interactive)
  (let* ((shorthand (read-string "Shorthand: "))
         (longhand (read-string "Longhand: "))
         (current-shorthand-list (bound-and-true-p
                                  read-symbol-shorthands))
         (shorthand-list (if current-shorthand-list
                             current-shorthand-list
                           '())))
    (message "Added shorthand: %s -> %s" shorthand longhand)
    (add-to-list 'shorthand-list (cons shorthand longhand))
    (add-file-local-variable 'read-symbol-shorthands shorthand-list)
    (setq-local read-symbol-shorthands shorthand-list)))

;; For some reason icomplete doesn't always load after initialisation
(icomplete-mode)

;; Use the default `completion--in-region' function.
(setq completion-in-region-function
      (lambda (&rest args)
        (apply #'completion--in-region args)))

;; Enables advanced editing modes
(put 'set-goal-column 'disabled nil)    ;; Enables setting a goal column
(put 'narrow-to-region 'disabled nil)   ;; Enables narrowing
(put 'narrow-to-page 'disabled nil)
(add-hook 'prog-mode-hook #'(lambda () (setq-local page-delimiter "^\n\n")))

(put 'upcase-region 'disabled nil)      ;; Enables up case a region
(put 'downcase-region 'disabled nil)    ;; Enables down case a region
(defun titlecase-region (BEG END)
  "Convert a region to title case between BEG and END."
  (interactive "r")
  (downcase-region BEG END)
  (upcase-initials-region BEG END))

;; I use this more than insert-file
(global-set-key (kbd "C-x i") #'titlecase-region)
(global-set-key (kbd "C-x w r") #'write-region)

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
(setq ispell-dictionary "en_GB")
(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

;;  *************************
;;; * Programming Languages *
;;  *************************

;;;; The Grand Unified Debugger
(global-set-key (kbd "C-x C-a i") #'gud-goto-info)
(global-set-key (kbd "C-x C-a t") #'gud-tooltip-mode)

;;;; Compilation
(setq compilation-scroll-output 'first-error)
(setq compilation-python-type-check-cmd
      "uvx pyrefly check -j 0 --output-format min-text")

;;;; Python
(defun python-imenu-use-flat-index
    ()
  "Use flat indexing for imenu."
  (setq imenu-create-index-function
        #'python-imenu-create-flat-index))

(add-hook 'python-mode-hook
          #'python-imenu-use-flat-index)

(setq gud-pdb-command-name "uv run python -m pdb")
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

;;;; Powershel
(use-package powershell)

;;;; Nix
(use-package nix-mode)

;;;; Latex
(setq tectonic-compile-command "tectonic -X compile -f plain %T"
      tectonic-watch-command "tectonic -X watch")
(if (eq system-type 'windows-nt)
    (setq tectonic-compile-command "tectonic -X compile -f plain %T"
          tectonic-watch-command "tectonic -X watch")
  (setq tectonic-compile-command
        "nix-shell --command \"tectonic -X compile -f plain %T\""
        tectonic-watch-command
        "nix-shell --command \"tectonic -X watch\""))

(when (display-graphic-p)
  (global-prettify-symbols-mode))

(defun how-is-this-paper-looking? (BEG END)
  "Display number of todos and cites between BEG and END."
  (interactive "r")
  (message (format "[%s, %s] => %d cites; %d todos"
                   BEG END
                   (how-many "[\\]cite" BEG END)
                   (how-many "[\\]todo" BEG END))))

(defun latex-format-as-todo (BEG END)
  "Wrap the text between BEG and END in a todo."
  (interactive "r")
  (save-excursion
    (narrow-to-region BEG END)
    (set-mark nil)
    (goto-char (point-min))
    (insert "\\todo{")
    (goto-char (point-max))
    (insert "}")
    (widen)))

;;;; RefTeX
(require 'reftex)
;; Useful RefTeX commands
;;
;; C-c =    :: ToC.
;; C-c (    :: Create a label.
;; C-c )    :: Reference a label.
;;
;; C-c /    :: Create an index.
;; C-c <    :: Select an index.
;; C-c \    :: Create a special index.
;; C-c >    :: Display and edit the index.
;;
;; C-c &    :: Display cross-reference.
(defun ebib-latex-quick-cite ()
  "Insert the \cite{key} for an ebib key."
  (interactive)
  (let ((ebib-citation-commands '((LaTeX-mode
                                  (("cite" "\\cite{%(%K%,)}")))))
        (ebib-citation-insert-multiple current-prefix-arg))
    (ebib-insert-citation)))

(use-package latex
  ;; In general I prefer the AUCTeX modes over their builtin counter parts
  ;; That is, LaTeX-mode over latex-mode etc.
  :ensure auctex
  :ensure reftex
  :config
  (add-hook 'LaTeX-mode-hook #'TeX-fold-mode)
  (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (setq flymake-compiler "pdflatex")
              (setq flymake-args '("-interaction=nonstopmode" "%f"))))
  (add-to-list 'auto-mode-alist '("\\.TeX$" . LaTeX-mode))
  (setq TeX-auto-save t
        TeX-parse-self nil
        TeX-electric-sub-and-superscript t
        ;; Disable variable font for sections
        font-latex-fontify-sectioning 'color
        ;; The below variables set-up AucTeX to use tectonic
        TeX-engine-alist `((default
                          "Tectonic"
                          ,tectonic-compile-command
                          ,tectonic-watch-command
                          nil))
        LaTeX-command-style '(("" "%(latex)"))
        ;; Sets up reftex with AUCTeX
        reftex-plug-into-AUCTeX t
        TeX-check-TeX nil
        TeX-process-asynchronous t
        TeX-engine 'default)
  (setq-default TeX-master nil)
  :bind (:map reftex-mode-map ("C-c [" . nil))
  :bind (:map LaTeX-mode-map ("C-c [" . #'ebib-latex-quick-cite))
  :bind (:map LaTeX-mode-map ("C-c l t" . #'latex-format-as-todo)))

;; On windows, flyspell-mode causes .tex files to hang indefinitely.
;; This is due aspell on windows causing issues.
;; https://mail.gnu.org/archive/html/help-emacs-windows/2017-04/msg00017.html
(when (eq system-type 'windows-nt)
  (add-hook 'LaTeX-mode-hook (lambda () (flyspell-mode -1))))

;; TeX-command-list needs to be modified not pass extra metadata and options
(let ((tex-list (assoc "TeX" TeX-command-list))
      (latex-list (assoc "LaTeX" TeX-command-list)))
  (setf (cadr tex-list) "%(tex)"
        (cadr latex-list) "%l"))

;; Live PDF preview in tectonic projects
(add-hook 'after-change-major-mode-hook
          (lambda ()
            (when-let ((project (project-current))
                       (proot (project-root project)))
              (when (file-exists-p (expand-file-name "Tectonic.toml" proot))
                (setq-local TeX-output-dir
                            (expand-file-name "build/index" proot))))))

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
;; well also Python but that's not important right now
(setq ess-ask-for-ess-directory t) ;; I actually like this feature!
(setq ess-local-process-name "R")
(setq ess-eval-visibly-p 'nowait) ;; No waiting whilst ESS is evaluating

(define-key ess-r-mode-map (kbd "M-?") nil) ;; unbinds M-?

;;;; Rust
(use-package rust-mode)

;;;; F-sharp
(use-package fsharp-mode)

;;;; Clojure
(use-package cider)
(use-package paredit)

;;;; gleam
(use-package gleam-ts-mode
  :mode (rx ".gleam" eos))


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

;; hs-minor-mode not supported by gleam-ts-mode
(remove-hook 'gleam-ts-mode-hook #'hs-minor-mode)

;; Semantic mode
;; Language aware editing commands for:
;; C, C++, HTML,Java, Javascript, Make, Python, Scheme, SRecode, and Texinfo
(require 'cedet)
(require 'semantic)
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
(add-hook 'prog-mode-hook #'semantic-mode)
(define-key semantic-mode-map (kbd "M-/") #'completion-at-point)
(define-key semantic-mode-map (kbd "C-c , a")
            #'semantic-analyze-current-context)

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
(recentf-mode 1)
(ffap-bindings) ;; Better defaults for file finding
(setq read-file-name-completion-ignore-case t)

;; Projects
(defun edit-projects ()
  "Edit the list of projects."
  (interactive)
  (find-file project-list-file))
(global-set-keys-to-prefix "C-x p" '(("a" . edit-projects)
                                     ("s" . project-search)))

;; Buffers
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "M-[") (lambda () (interactive)
                              (if tab-line-mode (tab-line-switch-to-prev-tab)
                                (previous-buffer))))
(global-set-key (kbd "M-]") (lambda () (interactive)
                              (if tab-line-mode (tab-line-switch-to-next-tab)
                                (next-buffer))))

(defun kill-this-buffer-reliably ()
  "Reliably kill this buffer."
  (interactive) (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") #'kill-this-buffer-reliably)

;; Scratch buffer
(defun note-buffer ()
  "Create a scratch buffer for 'org-mode' notes."
  (interactive)
  (let* ((buffer-notes-name "*notes*")
         (buffer-notes-scratch (get-buffer buffer-notes-name)))
    (unless buffer-notes-scratch
      (switch-to-buffer (get-buffer-create buffer-notes-name))
      (org-mode)
      (insert "#+title: Notes\n"))
    (switch-to-buffer buffer-notes-name)))

(setq initial-major-mode #'emacs-lisp-mode)
(setq initial-scratch-message ";;; Scratch --- A Scratch Pad for Elisp Code\n")

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
  (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
  (add-hook 'dired-mode-hook
            (lambda () (local-set-key (kbd "M-p") #'dired-up-directory))))

(setq dired-listing-switches "-alh"
      dired-dwim-target #'dired-dwim-target-recent
      dired-kill-when-opening-new-dired-buffer t)

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

;; Provides dired-async-mode
;; I normally don't like using external packages but async
;; for dired is a must!
(use-package async)
(dired-async-mode t)


;;  ***************
;;; * Development *
;;  ***************

;; Python
(require 'flymake)
(add-hook 'prog-mode-hook #'flymake-mode)
(setq flymake-start-on-flymake-mode t)
(setq python-flymake-command '("uv run ruff" "check" "--output-format"
                               "concise" "--quiet"
                               "--exit-zero" "--select" "ALL"
                               "--stdin-filename=stdin" "-"))

(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)
(define-key flymake-mode-map (kbd "C-c l b") 'flymake-show-buffer-diagnostics)
(define-key flymake-mode-map (kbd "C-c l p") 'flymake-show-project-diagnostics)


;;  *********
;;; * Tools *
;;  *********

;; Habitica
(use-package habitica
  :config
  (setq habitica-show-streak t)
  (setq habitica-uid local/habitica-uid)
  (setq habitica-token local/habitica-token))

;; Shell/Eshell
(require 'em-banner)
(setq eshell-banner-message "")

(require 'em-dirs)
(setq  eshell-list-files-after-cd t)

(global-set-key (kbd "C-c e") #'eshell)
(global-set-key (kbd "C-c s") #'shell)
(global-set-key (kbd "C-x p C-s") #'project-shell)

;; GPTel
;;
;; |------------------------------------------------------------------------|
;; |                                        | $/1M Token  |     |    |      |
;; | Model Name                             | In   | Out  | TFJ | II |  CW  |
;; |----------------------------------------|------|------|-----|----|------|
;; | Llama-3.3-70B-Instruct-Turbo-Free      | 0.00 | 0.00 | -FJ | 28 |  131 |
;; | Llama-4-Maverick-17B-128E-Instruct-FP8 | 0.27 | 0.85 | -FJ | 36 | 1050 |
;; | moonshotai/Kimi-K2-Thinking            | 1.20 | 3.00 | TTJ | 67 |  262 |
;; |------------------------------------------------------------------------|
;;
;; FJ = Thinking, Function Calling, JSON Ouptut
;; II = Artificial Intelligence Index v2.2
;; CW = Context Window (kTokens)
;;
;; Purpose of each model?
;;  moonshotai/Kimi-K2-Thinking     :: Best
;;  Llama-4-Maverick                :: Huge context
;;  Llama-3.3                       :: Free
;;
(use-package gptel
  :config
  (when (eq system-type 'windows-nt)
    ;; Curl and gptel don't work well on windows
    ;; https://github.com/karthink/gptel/issues/251
    (setq gptel-use-curl nil
          gptel-stream nil))
  (setq gptel-backend (gptel-make-openai "TogetherAI"
                        :host "api.together.xyz"
                        :key together-ai-api-key
                        :stream t
                        :models
                        '(moonshotai/Kimi-K2-Thinking
                          meta-llama/Llama-4-Maverick-17B-128E-Instruct-FP8
                          meta-llama/Llama-3.3-70B-Instruct-Turbo-Free))
        gpt-model (car (gptel-openai-models gptel-backend))
        gptel-expert-commands t
        gptel-temperature 0.7
        gptel-default-mode 'org-mode
        ;; Responses seem to cut off for a lot of thinking models
        ;; and, come to think of it, a lot of other models too.
        ;; Hence I'll set a high upper limit and bring down
        ;; if I feel the response quality is degraded.
        ;; 2e14 seems like a reasonable for most model contexts.
        gptel-max-tokens 16384
        gptel-track-media t
        gptel-include-reasoning t)
  (load (concat user-emacs-directory "gptel-papers.el"))
  (load (concat user-emacs-directory "gptel-tools.el")))

(use-package gptel-fn-complete)

(setq gptel-commit--prompt
      "
Write a conventional commit message for the following diff making sure
to ONLY return the commit message!\n

The commit message should be structured as follows:

-----------------------------------------------------------------------
<type>[optional scope]: <description>

[optional body]

[optional footer(s)]
-----------------------------------------------------------------------

The commit contains the following structural elements, to communicate intent to the
consumers of your library:

1 fix: a commit of the type fix patches a bug in your codebase (this correlates
 with PATCH in Semantic Versioning).
2 feat: a commit of the type feat introduces a new feature to the codebase (this
 correlates with MINOR in Semantic Versioning).
3 BREAKING CHANGE: a commit that has a footer BREAKING CHANGE:, or
 appends a ! after the type/scope, introduces a breaking API change (correlating
 with MAJOR in Semantic Versioning). A BREAKING CHANGE can be part of
 commits of any type.
4 types other than fix: and feat: are allowed, for example
 @commitlint/config-conventional (based on the Angular convention)
 recommends build:, chore:, ci:, docs:, style:, refactor:, perf:, test:, and
 others.
5 footers other than BREAKING CHANGE: <description> may be provided and
 follow a convention similar to git trailer format.

Additional types are not mandated by the Conventional Commits specification, and
have no implicit effect in Semantic Versioning (unless they include a BREAKING
CHANGE). A scope may be provided to a commit’s type, to provide additional
contextual information and is contained within parenthesis, e.g., feat(parser):
add ability to parse arrays.

Examples

Commit message with description and breaking change footer

```
feat: allow provided config object to extend other configs

BREAKING CHANGE: `extends` key in config file is now used for extending other config files
```

Commit message with ! to draw attention to breaking change

```
feat!: send an email to the customer when a product is shipped
```

Commit message with scope and ! to draw attention to breaking change

```
feat(api)!: send an email to the customer when a product is shipped
```

Commit message with both ! and BREAKING CHANGE footer

```
chore!: drop support for Node 6

BREAKING CHANGE: use JavaScript features not available in Node 6.
```

Commit message with no body

```
docs: correct spelling of CHANGELOG
```

Commit message with scope

```
feat(lang): add Polish language
```

Commit message with multi-paragraph body and multiple footers

```
fix: prevent racing of requests

Introduce a request id and a reference to latest request. Dismiss
incoming responses other than from latest request.

Remove timeouts which were used to mitigate the racing issue but are
obsolete now.

Reviewed-by: Z
Refs: #123
```

Specification

The key words “MUST”, “MUST NOT”, “REQUIRED”, “SHALL”, “SHALL NOT”,
“SHOULD”, “SHOULD NOT”, “RECOMMENDED”, “MAY”, and “OPTIONAL” in this
document are to be interpreted as described in RFC 2119.

1 Commits MUST be prefixed with a type, which consists of a noun, feat, fix,
 etc., followed by the OPTIONAL scope, OPTIONAL !, and REQUIRED terminal
 colon and space.
2 The type feat MUST be used when a commit adds a new feature to your
 application or library.
3 The type fix MUST be used when a commit represents a bug fix for your
 application.
4 A scope MAY be provided after a type. A scope MUST consist of a noun
 describing a section of the codebase surrounded by parenthesis, e.g., fix
 (parser):
5 A description MUST immediately follow the colon and space after the
 type/scope prefix. The description is a short summary of the code changes,
 e.g., fix: array parsing issue when multiple spaces were contained in string.
6 A longer commit body MAY be provided after the short description, providing
 additional contextual information about the code changes. The body MUST
 begin one blank line after the description.
7 A commit body is free-form and MAY consist of any number of newline
 separated paragraphs.
8 One or more footers MAY be provided one blank line after the body. Each footer
 MUST consist of a word token, followed by either a :<space> or <space>#
 separator, followed by a string value (this is inspired by the git trailer
 convention).
9 A footer’s token MUST use - in place of whitespace characters, e.g., Acked-by
 (this helps differentiate the footer section from a multi-paragraph body). An
 exception is made for BREAKING CHANGE, which MAY also be used as a token.
10 A footer’s value MAY contain spaces and newlines, and parsing MUST
 terminate when the next valid footer token/separator pair is observed.
11 Breaking changes MUST be indicated in the type/scope prefix of a commit, or
 as an entry in the footer.
12 If included as a footer, a breaking change MUST consist of the uppercase text
 BREAKING CHANGE, followed by a colon, space, and description, e.g.,
 BREAKING CHANGE: environment variables now take precedence over
 config files.
13 If included in the type/scope prefix, breaking changes MUST be indicated by a !
 immediately before the :. If ! is used, BREAKING CHANGE: MAY be omitted from
 the footer section, and the commit description SHALL be used to describe the
 breaking change.
14 Types other than feat and fix MAY be used in your commit messages, e.g.,
 docs: update ref docs.
15 The units of information that make up Conventional Commits MUST NOT be
 treated as case sensitive by implementors, with the exception of BREAKING
 CHANGE which MUST be uppercase.
16 BREAKING-CHANGE MUST be synonymous with BREAKING CHANGE,
 when used as a token in a footer.

Here is the diff:\n\n
")

(setq gptel-commit-model 'meta-llama/Llama-4-Maverick-17B-128E-Instruct-FP8)
(defun gptel-commit ()
  "Write a commit message for the current diff."
  (interactive)
  (unless (or (eq major-mode 'vc-dir-mode)
              (eq major-mode 'diff-mode))
    (error "gptel-commit must be used in a vc-dir or diff buffer"))
  (unless (eq major-mode 'diff-mode)
    (vc-diff))
  (let ((diff (buffer-string))
        (gptel--system-message nil)
        (gptel-model gptel-commit-model))
    (gptel-request
        (concat gptel-commit--prompt  diff)
      :callback
      (lambda (response info)
        (kill-new response)
        (message "gptel-commit message in the kill ring!"))))
  (vc-next-action nil))

(add-hook 'vc-dir-mode-hook
          (lambda () (local-set-key (kbd "c") #'gptel-commit)))
(add-hook 'diff-mode-hook
          (lambda () (local-set-key (kbd "c") #'gptel-commit)))

(setq default-llm-system-prompt
"
You are Ceri, a large language model living inside Emacs.

`==Core Reasoning Protocol==`

- Solve problems using explicit, step-by-step reasoning that you document during reasoning.
- Decompose complex problems into logical phases (e.g., problem analysis, approach selection, execution, verification).
- After each major phase, pause to reflect on progress and quality. Use a simple status tag: `<status>`: on-track | uncertain | flawed
- If status is \"flawed,\" stop and backtrack. Explain why the approach failed, then try a different strategy.
- If status is \"uncertain,\" consider one alternative approach before proceeding.
- Be honest but not hyper-critical. Reserve \"flawed\" for genuine logical errors, not mere uncertainty.

`==Dynamic Step Budget==`

- Start with a soft budget of ~20 steps for the Reasoning section.
- A \"step\" is any meaningful reasoning action (e.g., \"Now I factor the quadratic,\" \"Let me reconsider the boundary conditions\").
- If you exceed 30 steps, pause and summarize your progress. Then ask the user: \"I've used 30 steps and haven't reached a final answer. Should I continue?\"
- For simple problems, ignore the budget entirely—just solve them directly.

`==Status System==`

- `<status>`: on-track (the current approach is sound, continue)
- `<status>`: uncertain (I have doubts, I will evaluate one alternative)
- `<status>`: flawed (this path is wrong, I must backtrack and explain why)

`==Formatting Guidelines==`

- **Answer section** must use only valid ASCII. Keep lines under 120 characters.
- Prefer bullet points and clear hierarchy for structuring information.
- Use tables sparingly—only when comparing multiple options side-by-side provides genuine clarity.
- For mathematical problems: show all work using LaTeX in the Reasoning section, but present the final result cleanly in the Answer section.

`==Backtracking Rules==`

- Backtrack only when status is \"flawed.\"
- When backtracking, write: `<backtrack>`: [brief explanation of what went wrong]
- Then start a new phase with a revised approach.
- Avoid backtracking more than twice on the same problem—this suggests you need to ask for clarification rather than guess.

`==Mathematical Problems==`

- In Reasoning: Provide detailed proofs, explore at least two solution paths when viable, and compare their tradeoffs.
- In Answer: Give the final result with key steps summarized, not the full proof unless requested.

`==Minimal Working Example==`

*User:* \"A startup has 6 engineers to allocate across three projects: Alpha (needs 2-3 engineers), Beta (needs 2-4 engineers), and Gamma (needs exactly 1 engineer). Due to skill constraints, Gamma cannot share engineers with Alpha. Beta must have at least as many engineers as Alpha. Find all valid allocations.\"

*Reasoning:*

-   `<step>`: Parse constraints formally:
    -   Alpha: 2 ≤ A ≤ 3
    -   Beta: 2 ≤ B ≤ 4
    -   Gamma: G = 1
    -   A + B + G = 6
    -   Gamma engineers are disjoint from Alpha
    -   B ≥ A
-   `<count>`: 19
-   `<status>`: on-track

-   `<step>`: Try approach 1: Allocate Gamma first (1 engineer), leaving 5 for Alpha/Beta.
    -   Since G=1, remaining: A + B = 5
    -   With B ≥ A and A ∈ {2,3}, test possibilities:
        -   If A=2 → B=3 (valid: B≥A, B≤4)
        -   If A=3 → B=2 (invalid: B≥A fails)
-   `<count>`: 18
-   `<status>`: uncertain (only one viable pair found, but need to check disjointness constraint)

-   `<step>`: Check disjointness for (A=2, B=3). This means 2 engineers on Alpha, 3 on Beta, 1 on Gamma, with Gamma's engineer not shared with Alpha's 2. This is possible since Alpha uses 2 of the remaining 5 engineers, Gamma uses 1 distinct one, leaving 3 for Beta.
-   `<count>`: 17
-   `<status>`: on-track

-   `<step>`: But wait—does \"Gamma cannot share engineers with Alpha\" mean the *individuals* are disjoint, or that the *count* is separate? Re-reading: \"cannot share engineers\" implies distinct people. My allocation works, but I should verify if other allocations exist by testing A=3 again with different reasoning.
-   `<count>`: 16
-   `<status>`: uncertain (maybe missed something)

-   `<step>`: Try A=3, B=2 again. B ≥ A fails (2 ≥ 3 is false). This is definitively invalid. So only A=2, B=3 works under current interpretation.
-   `<count>`: 15
-   `<status>`: on-track

-   `<step>`: Consider alternative interpretation: what if Gamma's engineer could be drawn from the pool but just not counted in Alpha's headcount? That violates \"cannot share engineers\" phrasing. Let me backtrack and re-express constraints.
-   `<count>`: 14
-   `<status>`: flawed (realized I might have misinterpreted the disjointness constraint's implications)

-   `<backtrack>`: The constraint \"Gamma cannot share engineers with Alpha\" means the 1 Gamma engineer must be distinct from Alpha's 2-3 engineers, but Beta can share with either. My original interpretation was correct. No need to change approach.

-   `<step>`: Final verification: A=2, B=3, G=1 uses 6 distinct engineers (2+3+1=6). Gamma's 1 is disjoint from Alpha's 2. Beta's 3 can overlap with either group. All constraints satisfied.
-   `<count>`: 13
-   `<status>`: on-track

*Answer:*

-   Only one valid allocation exists:
    -   Project Alpha: 2 engineers
    -   Project Beta: 3 engineers
    -   Project Gamma: 1 engineer (distinct from Alpha's team)
-   This uses all 6 engineers and satisfies:
    -   Alpha's 2-3 engineer requirement
    -   Beta's 2-4 engineer requirement with B ≥ A
    -   Gamma's exact 1-engineer requirement with skill separation
")
(setq gptel--system-message default-llm-system-prompt)

;; Aider.el
;; External dependencies :: aider
;; Emacs dependencies :: transient, magit, markdown-mode
(use-package aider
  :init
  (require 'transient)
  (require 'magit)
  (require 'markdown-mode)
  :config
  (setq aider-args '("--architect"
                     "--model" "together_ai/Qwen/Qwen3-Next-80B-A3B-Thinking"
                     "--reasoning-effort" "high"
                     "--no-auto-accept-architect"
                     "--editor-model" "together_ai/Qwen/Qwen3-Next-80B-A3B-Instruct"
                     "--weak-model" "together_ai/meta-llama/Llama-3.3-70B-Instruct-Turbo-Free"
                     "--show-diffs"
                     "--install-main-branch"))
  (setenv "TOGETHER_API_KEY" together-ai-api-key)
  (global-set-key (kbd "C-c a") 'aider-transient-menu))

;; Visit init file
(defun my-visit-user-init-file ()
  "Visit the init file."
  (interactive)
  (find-file user-init-file))

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

;; Starling.el
(use-package plz
  :straight t)
(use-package starling
  :straight `(starling :type git :host github :repo "abdrysdale/starling-el"))

(defun starling ()
  "Launch starling dashboard."
  (interactive)
  (starling-spaces)
  (delete-other-windows)
  (split-window-horizontally)
  (starling-insights)
  (split-window-vertically)
  (previous-window-any-frame))

;; Doc view
(require 'doc-view)
(setq doc-view-resolution 200
      doc-view-imenu-enabled t)
(add-hook 'doc-view-mode-hook (lambda () (display-line-numbers-mode -1)))

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

;; Browser
(require 'eww)
(setq eww-search-prefix "https://duckduckgo.com/html/?kd=-1&q="
      shr-width 70
      shr-cookie-policy nil
      shr-max-image-proportion 0.7
      shr-inhibit-images t
      shr-use-xwidgets-for-media t
      browse-url-browser-function #'eww
      eww-history-limit nil
      eww-retrieve-command 'sync
      eww-auto-rename-buffer 'title
      eww-browse-url-new-window-is-tab nil)

(defun eww-use-old-reddit (url)
  "Transform any www.reddit in URL to old.reddit."
  (replace-regexp-in-string "^h?t*p?s?:?/*www.reddit*"
                            "https://old.reddit" url))
(add-to-list 'eww-url-transformers #'eww-use-old-reddit)

(defmacro def-eww-with-search-prefix (name url)
  "Create a function eww-search- NAME URL as default search prefix."
  `(defun ,(intern (format "eww-search-%s" name)) ()
     ,(format "Launch eww with %s as the search prefix." url)
     (interactive)
     (let ((eww-search-prefix ,url))
       (call-interactively #'eww))))

(defvar eww-search-engine-list nil "List of search engines for eww.")
(setq eww-search-engine-list
      '(("wiki" "w" "https://en.wikipedia.org/wiki/Special:Search?go=Go&search=")
        ("scholar" "s" "https://scholar.google.co.uk/scholar?q=")
        ("pypi" "p" "https://pypi.org/search/?q=")))

(dolist (engine eww-search-engine-list)
  (let* ((name (car engine))
         (prefix (nth 1 engine))
         (url (nth 2 engine)))
    (eval `(global-set-key (kbd ,(format "C-c u %s" prefix))
                           (def-eww-with-search-prefix ,name ,url)))))

;; Newsticker (RSS)
(setq newsticker-frontend #'newsticker-treeview
      newsticker-automatically-mark-items-as-old nil
      newsticker-hide-old-items-in-newsticker-buffer t)

(setq newsticker-url-list
      ;; Scientific
      '(("npj imaging" "https://www.nature.com/npjimaging.rss")
        ("npj ml" "https://www.nature.com/natmachintell.rss")
        ("arxiv medphys" "https://rss.arxiv.org/rss//physics.med-ph")
        ("MHRA Device Safety Information"
         "https://www.gov.uk/drug-device-alerts.atom?alert_type%5B%5D=device-safety-information")
        ("mr in med" "https://onlinelibrary.wiley.com/action/showFeed?jc=15222594&type=etoc&feed=rss")
        ("Practical fMRI"
         "https://practicalfmri.blogspot.com/feeds/posts/default")
        ;; Personal
        ("Meadowhawk" "https://blog.meadowhawk.xyz/feeds/rss.xml")
        ("Ruslan" "https://codelearn.me/feed.xml")
        ("Reddit: Emacs" "https://www.reddit.com/r/emacs.rss")
        ("SachaChau" "https://sachachua.com/blog/feed/index.xml")
        ("Freya Vie" "https://freyavie.blog/feed/?type=rss")
        ("Marius Masalar" "https://marius.ink/feed.atom")
        ("Nothing is Simple"
         "https://nothingissimple.ablatedsprocket.com/rss.xml")
        ("Hoagie's corner of the internet"
         "https://site.sebasmonia.com/feed.xml")
        ("Yann Herklotz" "https://yannherklotz.com/index.xml")
        ("Steven Bonner" "https://sbonner0.github.io/feed.xml")
        ("Arjen Wiersma" "https://arjenwiersma.nl/posts/index.xml")
        ("abdrysdale" "https://abdrysdale.phd/feed.xml")))

;; IRC
(require 'erc)

(setq erc-autojoin-channels-alist
      '(("Libera.Chat"
         "#emacs"
         "#python"
         "#fortran"
         "##forth"
         "#commonlisp"
         "#lisp"
         "#indieweb"
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

(use-package transmission
  :config (setq transmission-timer 5))

;; Music
(use-package emms
  :ensure t
  :config
  (require 'emms-setup)
  (emms-all)
  (if (eq system-type 'windows-nt)
      (progn
        (require 'emms-player-mplayer)
        (setq emms-player-list '(emms-player-mplayer)))
    (progn
      (require 'emms-player-mpv)
      (setq emms-player-list '(emms-player-mpv)
            emms-info-functions '(emms-info-native)
            emms-player-mpv-parameters '("--no-video")))))

(setv emms-radio-addr
      '(
        ;; Classical
        ("Classic - Relax" . "http://relax.stream.publicradio.org/relax.mp3")
        ("ClassicFM" . "http://media-the.musicradio.com/ClassicFM")
        ("ClassicFM Movies" . "http://media-sov.musicradio.com/ClassicFM-M-Movies")
        ;; Jazz
        ("Jazz" . "https://icecast.walmradio.com:8443/jazz")
        ("Trad Jazz" . "https://icecast.walmradio.com:8443/classic")
        ("Bosa" . "https://centova5.transmissaodigital.com:20104/live")
        ("Smooth" . "http://playerservices.streamtheworld.com/pls/977_SMOOJAZZ.pls")
        ("Funk" . "http://jazz-wr06.ice.infomaniak.ch/jazz-wr06-128.mp3")
        ;; Atmospheric
        ("Lofi" . "https://lofi.stream.laut.fm/lofi")
        ("Ambient" . "https://isekoi-radio.com/listen/ambient/ambientradio.flac")
        ;; News
        ("Talk News" . "https://media-ssl.musicradio.com/LBCUKMP3")
        ("BBC World" . "https://stream.live.vc.bbcmedia.co.uk/bbc_world_service")
        )
      "A list of internet radio addresses.")

(defun emms-play-from-radio-addr ()
  "Play a url from 'emms-radio-addr'."
  (interactive)
  (let* ((radio (completing-read "Radio: " emms-radio-addr nil t))
         (addr (cdr (assoc radio emms-radio-addr))))
    (emms-play-url addr)))

;; Self Help
(setq sh/tasks
      '("Get tortoise out"
        "Make a cup of coffee"
        "Water the plants"
        "Check washing"
        "Check dishwasher"
        "Top up bird feeders"
        "Send someone a nice message"
        "Prayer/meditation"
        "Check bins"
        "Read a poem"
        "Shower"
        "Put clothes away"
        "Tidy a room"
        "Drink glass of water"
        "Set up toys"
        "Check house plants"))

(defun random-choice (list)
  "Return a random element from a LIST."
  (let* ((N (length list))
         (i (random N)))
    (nth i list)))

(defun print-center-of-frame (msg &optional x y)
  "Print MSG at width X and height Y or at the frame center."
  (let* ((lines (if (listp msg) msg (list msg)))
         (M (length lines))
         (idx 0)
         (H (if y y (/ (- (frame-height) M) 2))))
    (dolist (line lines)
      (let* ((N (length line))
             (W (if x x (/ (- (frame-width) N) 2))))
        (animate-string line (+ H idx) W)
        (setq idx (+ 1 idx))
        (sit-for 0.5)))))

(defun sh/code-red-buffer ()
  "Load the 'code-red' buffer."
  (interactive)
  (let ((name "*Code Red*"))
    (unless (get-buffer name)
      (tab-new))
    (switch-to-buffer (get-buffer-create name)))
  (text-mode)
  (erase-buffer)
  (sit-for 0)
  (delete-other-windows))

(define-derived-mode sh/code-red-mode org-mode "Code Red")
(defun sh/code-red-exit ()
  "Exit code red."
  (interactive)
  (kill-buffer) (tab-close))

(defvar ascii-tortoise nil "An ascii tortoise.")
(setq ascii-tortoise
"
   _____________
 /               \\
|  You got this!  |                     ________
 \\ ____________  /                 ___/ \\     / \\__
               \\/   ___           /      \\ _ /     \\
                   /   \\      __/  __          __    \\__
                  |  o  \\    /   /    \\      /    \\      \\
                  \\_ /    \\ /   |      |    |      |      \\
                    \\     |      \\ __ /      \\ __ /       /____/\\
                     \\___/   _                      _    /_____ /
                         \\_/   \\__________________/   \\_/
                          /    /                 /    /
                         /    / \\              /    / \\
                        |   /\\    \\           |   /\\    \\
                        |  \\   \\___|          |  \\   \\___|
                        \\___|   '''           \\___|   '''
                         '''                   '''
")

(defun sh/tortoise (&optional task)
  "Display a tortoise to help with TASK."
  (interactive)
  (sh/code-red-buffer)
  (when task
      (print-center-of-frame (concat "* " task " *") nil 10))
  (animate-string ascii-tortoise 15 15)
  (special-mode)
  (local-set-key (kbd "q") #'sh/code-red-exit))

(define-key sh/code-red-mode-map (kbd "t") #'sh/tortoise)

(defun sh/code-red (&optional arg input-task)
  "Opens a buffer in a new tab and displays a self help task.

If ARG then no new tasks are allowed.
IF INPUT-TASK then just display that task."
  (interactive)
  (sh/code-red-buffer)
  (let* ((task (if input-task input-task (random-choice sh/tasks)))
         (msg (concat "+ " task " +"))
         (another-msg "a => another")
         (stars (make-string (length msg) ?+)))
    (print-center-of-frame (list stars msg stars))
    (when arg
        (setq another-msg (concat "+" another-msg "+")))
    (animate-string (concat
                     another-msg
                     "\n\td => display tasks"
                     "\n\tt => tortoise"
                     "\n\tq => quit")
                    (- (frame-height) 6) 4)
    (sh/code-red-mode)
    (local-set-key (kbd "t") (lambda () (interactive) (sh/tortoise task))))
  (if arg
    (local-set-key (kbd "a")
                   (lambda () (interactive)
                     (message "It's best you just do this task.")))
    (local-set-key (kbd "a")
                   #'sh/code-red-again)))
(global-set-key (kbd "C-x C-r") #'sh/code-red)

(defun sh/code-red-again ()
  "Pick another task."
  (interactive)
  (sh/code-red-exit) (sh/code-red t))
(define-key sh/code-red-mode-map (kbd "a") #'sh/code-red-again)

(defun sh/code-red-display-tasks ()
  "Display all of the code-red tasks."
  (interactive)
  (sh/code-red-buffer)
  (let ((x (/ (- (frame-width) 10) 2))
        (tasks (mapcar (lambda (v) (concat "- " v ".")) sh/tasks)))
    (print-center-of-frame tasks x nil))
  (sh/code-red-mode)
  (sh/code-red t (completing-read "Select a task: " sh/tasks)))
(define-key sh/code-red-mode-map (kbd "d") #'sh/code-red-display-tasks)

;; Dice Rolls
(defun roll-dice (&optional sides)
  "Roll an number between [1, SIDES]."
  (interactive "nNumber of sides: ")
  (unless sides
    (error "roll-die: Sides argument is required in non-interactive calls"))
  (when (< sides 1)    ;; This also checks if the number is an integer!
    (error "roll-die: Sides argument must be a natural number"))
  (message (format "d%i rolled: %i" sides (1+ (random sides)))))


;;  ********
;;; * Mail *
;;  ********

;; Notmuch ;;
(use-package notmuch)
(setq-default notmuch-search-oldest-first nil)

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

(defun ebib-dependent-add-entry-and-next ()
  "Add the current entry as dependent entry and then move down."
  (interactive)
  (ebib-dependent-add-entry)
  (ebib-next-entry))

(use-package ebib
  ;; Occasionally launching ebib will return:
  ;; display-buffer-assq-regexp: Wrong type argument: stringp, nil
  ;; So far, this can be solved either by:
  ;;    - Restarting emacs.
  ;;    - Deleting and reinstalling ebib.
  :after org
  :config
  (require 'org-ebib)
  (setq ebib-save-xrefs-first nil
        ebib-index-default-sort '("Year" . descend)
        ebib-file-associations nil
        ebib-notes-display-max-lines 30)
  (add-to-list 'ebib-citation-commands
               '(LaTeX-mode
                 (("cite" "\\cite{%(%K%,)}")
                  ("cite+info" "\\cite%<[%A]%>[%A]{%(%K%,)}")
                  ("paren" "\\parencite%<[%A]%>[%A]{%(%K%,)}")
                  ("foot" "\\footcite%<[%A]%>[%A]{%(%K%,)}")
                  ("text" "\\textcite%<[%A]%>[%A]{%(%K%,)}")
                  ("smart" "\\smartcite%<[%A]%>[%A]{%(%K%,)}")
                  ("super" "\\supercite{%K}")
                  ("auto" "\\autocite%<[%A]%>[%A]{%(%K%,)}")
                  ("cites" "\\cites%<(%A)%>(%A)%(%<[%A]%>[%A]{%K}%)")
                  ("parens" "\\parencites%<(%A)%>(%A)%(%<[%A]%>[%A]{%K}%)")
                  ("foots" "\\footcites%<(%A)%>(%A)%(%<[%A]%>[%A]{%K}%)")
                  ("texts" "\\textcites%<(%A)%>(%A)%(%<[%A]%>[%A]{%K}%)")
                  ("smarts" "\\smartcites%<(%A)%>(%A)%(%<[%A]%>[%A]{%K}%)")
                  ("supers" "\\supercites%<(%A)%>(%A)%(%<[%A]%>[%A]{%K}%)")
                  ("autos" "\\autoscites%<(%A)%>(%A)%(%<[%A]%>[%A]{%K}%)")
                  ("author" "\\citeauthor%<[%A]%>[%A]{%(%K%,)}")
                  ("title" "\\citetitle%<[%A]%>[%A]{%(%K%,)}")
                  ("year" "\\citeyear%<[%A]%>[%A][%A]{%K}")
                  ("date" "\\citedate%<[%A]%>[%A]{%(%K%,)}")
                  ("full" "\\fullcite%<[%A]%>[%A]{%(%K%,)}"))))
  :bind (:map ebib-index-mode-map ("v" . #'ebib-dependent-add-entry-and-next))
  :bind (:map ebib-entry-mode-map ("C-x b" . nil)))

(defun ebib-create-key (key _db)
  "Return the KEY in DB."
  (format "%s" key))

(add-to-list 'ebib-reading-list-template-specifiers '(?k . ebib-create-key))
(setq ebib-notes-directory local/paper-notes-dir
      ebib-reading-list-file local/reading-list
      ebib-reading-list-template (concat
                                  "* %M [[ebib:%k][%T]] [/]\n"
                                  ":PROPERTIES:\n"
                                  "%K\n"
                                  ":FILE: %F\n"
                                  ":DOI: %D\n"
                                  ":URL: %U\n"
                                  ":END:\n"
                                  "** TODO Read Abstract\n"
                                  "** TODO Read Conclusion\n"
                                  "** TODO Thorough Read\n"
                                  "** TODO Critique\n")
      ebib-preload-bib-files `(,local/bib-file)
      ebib-file-search-dirs `(,local/paper-dir))

(setq ebib-reading-list-new-item-hook
          (lambda ()
                                                ;; Position in kill ring
            (ebib-copy-field-contents "title")  ;; 2nd
            (ebib-copy-citation-as-kill)        ;; 1st
            (ebib-copy-key-as-kill)             ;; 0th
            (org-capture nil "p")))

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

(defun field-from-bibtex (entry field)
  "Get the FIELD from a bibtex ENTRY."
  (interactive)
  (let*
      ((beg? (string-match field entry))
       (beg (+ 1 (string-match "{" entry beg?)))
       (end (string-match "}," entry beg))
       (contents (substring entry beg end)))
    (if beg? contents (format "No %s" field))))

(defmacro run-func-and-get-latest-kill (func)
  "Run FUNC and return the last item from the kill ring."
  `(progn
     (,func)
     (car kill-ring)))

(defun ebib-insert-latex-ref-other-window (N)
  "Insert the current N item's reference and citation in the other window.

This is a very specific function.  It basically, gets the current paper
in ebib and pastes the paper title followed by the latex citation in the
other window, switches back to the ebib window and goes to the next entry.

The idea is when you want to quickly add a lot of papers to a latex document
with some rough idea of what the papers were about."
  (interactive "p")
  (dotimes (i N)
    (let* ((key (run-func-and-get-latest-kill
                 ebib-copy-key-as-kill))
           (entry (run-func-and-get-latest-kill
                   ebib-copy-entry-as-kill))
           (title (field-from-bibtex entry "title"))
           (abstract (field-from-bibtex entry "abstract")))
      (other-window 1)
      (insert (format "\n%s~\\cite{%s}" title key))
      (insert " % ")
      (insert abstract)
      (other-window -1)
      (ebib-next-entry))))

(add-hook 'ebib-index-mode-hook
          (lambda ()
            (local-set-key (kbd "C-y") #'ebib-insert-latex-ref-other-window)))


;;  ***********
;;; * Windows *
;;  ***********

(global-set-key (kbd "C-M-w") 'scroll-other-window-down)

;; Default window navigation – simply switch to the next window in order.
;; This can become tedious without repeat-mode
(repeat-mode)
(global-set-key (kbd "C-x o") #'other-window)
(global-set-key (kbd "M-o") #'previous-window-any-frame)
(global-set-key (kbd "C-M-o") #'other-frame)

;; I regularly make use of follow mode

;; Move to window with arrow keys, swap windows with shift + arrow keys
(setq shift-select-mode nil)    ;; Shift + arrow keys overrides this anyway.
(windmove-default-keybindings 'none)
(windmove-swap-states-default-keybindings 'shift)

;; Winner mode is handy for undo window changes.
(winner-mode t)

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
;;    |      |                     |      |
;;    |  * * |                     |  * * |
;;    |  d v |                     |  T l |
;;    |  i c |                     |  a l |
;;    |  r d |  Main Window Area   |  g m |
;;    |  e i |                     |  s s |
;;    |  d r |                     |  * * |
;;    |  * * |                     |      |
;;    |______|_____________________|______|
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
        ("\\*\\(?:Tags List\\|Ceri\\|elysium\\)\\*"
         display-buffer-in-side-window
         (side . right)
         (slot . 1)
         (window-parameters . ((window-height . fit-window-to-buffer)
                               (window-width . fit-window-to-buffer)
                               (preserve-size . (t . nil))
                               (no-other-window . nil))))
        ("\\*\\(?:vc-dir\\|gud-run\\)\\*"
         display-buffer-in-side-window
         (side . left)
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

(setq switch-to-buffer-in-dedicated-window "pop"
      switch-to-buffer-obey-display-actions t)


;;  ************
;;; * Org Mode *
;;  ************

;; Useful keybindings:
;;
;; M-S-<RET>    :: org-insert-todo-heading

(variable-pitch-mode 0)
(auto-fill-mode 0)
(visual-line-mode 1)

(setq org-return-follows-link  t
      org-hide-emphasis-markers t
      org-hide-leading-stars t
      org-indent-indentation-per-level 1
      org-latex-caption-above nil
      org-html-table-caption-above nil
      org-agenda-skip-deadline-prewarning-if-scheduled t
      org-footnote-auto-adjust t
      org-footnote-auto-label 'confirm
      org-enforce-todo-dependencies t
      org-startup-indented t)

(setq org-todo-keywords
      '((sequence
         "TODO(t!)" "STARTED(s!)" "WAITING(w@/!)"
         "|" "DONE(d!)" "CANCELLED(c@)" "ABORT(a)")))

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
(setq org-clock-persist t
      org-clock-in-switch-to-state "STARTED"
      org-clock-into-drawer t
      org-clock-out-when-done t
      org-clock-report-include-clocking-task t
      org-pretty-entities t
      org-clock-clocktable-default-properties '(:maxlevel 2 :scope subtree))
(org-clock-persistence-insinuate)

(defun insert-time-rfc-822 ()
  "Insert the current RFC 822 time."
  (interactive)
  (insert
   (format-time-string "%a, %d %b %Y %T %Z" nil t)))

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
  (define-key org-mode-map (kbd "C-c l c") #'me/org-link-copy))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook
                      (lambda ()
                        (when (eq major-mode 'org-mode)
                          (org-babel-tangle)))
                      nil t)))

;; Browser in external browser
(defun browser-url-at-point-with-external-browser (&optional ARG)
  "Browse url with default browser passing ARG to browser."
  (interactive)
  (let ((url (org-mode-url-at-point)))
    (if url
        (browse-url-default-browser url ARG)
      (error "No URL found"))))

(define-key org-mode-map (kbd "C-c u t") #'org-toggle-link-display)

;; Agenda ;;
(setq org-agenda-files `(,local/agenda-file ,local/reading-list))

(setq org-deadline-warning-days 60)

(define-key org-mode-map (kbd "C-c p s") #'org-priority)

;; Capture ;;

;; Capture templates
(with-eval-after-load "org"
  (define-key org-mode-map (kbd "C-c n e") #'st/export-notes)
  (define-key org-mode-map (kbd "C-c n t") #'st/set-template))

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
        ("i" "Interrupting Task" entry
         (file+headline ,(in-home-dir "Documents/notes/agenda.org") "Inbox")
         "* STARTED %^{Task}\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
         :clock-in :clock-resume
         :prepend t)
        ("s" "Sub Task" entry
         (clock)
         "* STARTED %^{Task}\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
         :clock-in :clock-resume)
        ("p" "Paper" entry
         (file+headline ,(in-home-dir "Documents/notes/agenda.org") "Inbox")
         ,(concat
           "* TODO [[file:"
           ebib-reading-list-file
           "::#reading_%c][%(nth 2 kill-ring) (%(nth 1 kill-ring))]]\n"
           "- [[file:"
           (file-name-as-directory ebib-notes-directory)
           "%c.org][Notes]]")
         :immediate-finish t
         :jump-to-captured nil
         :kill-buffer t)
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
        ("d" "Continuous Personal Development" entry
         (file+headline ,(in-home-dir "Documents/notes/agenda.org") "CPD")
         ,(concat
           "* %^{Title: }%?\t"
           "%(org-read-date nil nil \"+0d\")\t"
           ":%^{Tag: |work|professional|formal|self|other}:\n"
           "- /Benefit to me?/\n%^{Benefit to me: }\n"
           "- /Benefit to person/people/organisation?/\n%^{Benefit to org: }\n"
           "- /Where to find evidence?/\n%^{Where to find evidence: }\n"
           "- /Time spent (in hours)/ :: %^{Time spent: }\n"))))

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

;; Source: https://www.emacswiki.org/emacs/AbbrevMode#h5o-10
(defun set-local-abbrevs (abbrevs)
  "Add ABBREVS to `local-abbrev-table' and make it buffer local.
ABBREVS should be a list of abbrevs as passed to `define-abbrev-table'.
The `local-abbrev-table' will be replaced by a copy with the new abbrevs added,
so that it is not the same as the abbrev table used in other buffers with the
same `major-mode'."
  (let* ((bufname (buffer-name))
         (prefix (substring (md5 bufname) 0 (length bufname)))
         (tblsym (intern (concat prefix "-abbrev-table"))))
    (set tblsym (copy-abbrev-table local-abbrev-table))
    (dolist (abbrev abbrevs)
      (define-abbrev (eval tblsym)
        (cl-first abbrev)
        (cl-second abbrev)
        (cl-third abbrev)))
    (setq-local local-abbrev-table (eval tblsym))))

;; Key binding for unexpanding abbrevs
(global-set-key (kbd "C-x a u") #'unexpand-abbrev)

(setq dabbrev-limit nil  ;; No limit on searching back
      dabbrev-check-all-buffers t
      dabbrev-case-fold-search t
      dabbrev-case-replace nil
      dabbrev-abbrev-skip-leading-regexp "\\$")
(global-set-key (kbd "C-M-/") #'dabbrev-expand)


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


;;  *******************
;;; * Niche Functions *
;;  *******************

;; Government Digital Skill Level
(defun skill-level-summary (BEG END)
  "Display the number of occurrences of each skill level between BEG and END."
  (interactive "r")
  (let ((levels '("Awareness" "Working" "Practitioner" "Expert"))
        (RSTART (if (= BEG END) nil BEG))
        (REND (if (= BEG END) nil END)))
    (message (mapconcat
              (lambda (level) (format "%s: %i "
                                 level
                                 (count-matches level RSTART REND t)))
              levels))))

;; Mostly MRI related...

(defconst gamma-bar 42.58e6 "Gyromagnetic Ratio in Hz/Tesla.")

(defun get-hz/px (bw px &optional half)
  "Get the Hz/Px from BW (kHz) and PX if HALF assumes bw is half bandwidth."
  (let* ((bandwidth (if half (* 2 bw) bw)))
         (/ (* 1000 bandwidth) px)))

(defun fat/water-shift (&optional T)
  "Get the fat-water shift - assumes 1.5T unless T is provided."
  (let* ((static-field (if T T 1.5)))
         (* static-field gamma-bar 3.5e-6)))

(defun bw-okay-ge? (bw px)
  "Check fat/water shift from BW and PX for 1.5T GE systems."
  (let* ((hz/px (get-hz/px bw px t))
         (delta_f (fat/water-shift))
         (fw-shift? (/ delta_f hz/px)))
    (message
     (format "FW Shift: %.2f (%.1f Hz/px %.1f Hz)" fw-shift? hz/px delta_f))))

(defun bw-okay-siemens? (bw px)
  "Check fat/water shift from BW and PX for 1.5T Siemens systems."
  (let* ((hz/px (get-hz/px bw px))
         (delta_f (fat/water-shift))
         (fw-shift? (/ delta_f hz/px)))
    (message
     (format "FW Shift: %.2f (%.1f Hz/px %.1f Hz)" fw-shift? hz/px delta_f))))

;;  ***********
;;; * STARTUP *
;;  ***********

(defun init-workers ()
  "Initialise workers."
    (interactive)
    (unless init-script-initial-clients
      (irc)
      (newsticker-start)))
(add-hook 'emacs-startup-hook #'init-workers)

(defun startup ()
    "Startup process."
    (interactive)
    (let ((buffer-notes "*notes*")
           (buffer-calendar "*Calendar*")
           (buffer-agenda "*Org Agenda*")
           (buffer-fireplace "*fireplace*")
           (buffer-scratch "*scratch*")
           (buffer-habitica "*habitica*")
           (habitica-width 87)
           (habitica-height 32))

      ;; Loads buffers
      (org-agenda-list)
      (calendar)
      (scratch-buffer)
      (note-buffer)
      (habitica-tasks)
      (if (get-buffer buffer-fireplace)
          (kill-buffer buffer-fireplace))

      ;; Calendar
      (switch-to-buffer buffer-calendar)
      (delete-other-windows)
      (split-window-horizontally)

      ;; Scratch
      (other-window 1)
      (split-window-vertically)
      (split-window-vertically)
      (other-window 1)
      (switch-to-buffer buffer-scratch)

      ;; Habitica
      (other-window 1)
      (split-window-horizontally)
      (switch-to-buffer buffer-habitica)
      (window-resize nil (- habitica-width (window-total-width nil)) t)
      (window-resize nil (- habitica-height (window-total-height nil)))

      ;; Fireplace
      (other-window 1)
      (fireplace)
      (switch-to-buffer (get-buffer buffer-fireplace))
      (display-line-numbers-mode -1)

      ;; Notes
      (other-window 1)
      (switch-to-buffer (get-buffer buffer-notes))
      (org-cycle-agenda-files)

      ;; Agenda
      (split-window-vertically)
      (other-window 1)
      (switch-to-buffer (get-buffer buffer-agenda))
      (org-agenda-redo-all)))

;;  **********************
;;; * Global Keybindings *
;;  **********************

(global-set-keys-to-prefix "C-c b" '(("," . switch-to-buffer-other-window)
                                     ("N" . note-buffer)
                                     ("a" . append-to-buffer)
                                     ("b" . startup)
                                     ("c" . count-words)
                                     ("d" . desktop-read)
                                     ("e" . (lambda () (interactive)
                                              (switch-to-buffer "*elysium*")))
                                     ("g" . (lambda () (interactive)
                                              (switch-to-buffer "*gud-run*")))
                                     ("i" . ebib-insert-citation)
                                     ("k" . kill-buffer-and-window)
                                     ("l" . (lambda () (interactive)
                                              (switch-to-buffer "*Ceri*")))
                                     ("m" . (lambda () (interactive)
                                              (switch-to-buffer "*Messages*")))
                                     ("n" . next-buffer)
                                     ("p" . previous-buffer)
                                     ("r" . revert-buffer)
                                     ("s" . scratch-buffer)
                                     ("v" . view-buffer-other-window)))

(global-set-keys-to-prefix "C-c c" `(("." . org-timer)
                                     ("," . insert-time-rfc-822)
                                     ("c" . compile)
                                     ("d" .(lambda () (interactive)
                                             (insert
                                              (format-time-string "%Y-%m-%d"))))
                                     ("g" . org-clock-goto)
                                     ("i" . org-clock-in)
                                     ("l" . org-clock-in-last)
                                     ("n" .  (lambda () (interactive)
                                               (insert
                                                (format-time-string "%H:%M"))))
                                     ("o" . org-clock-out)
                                     ("p" . org-timer-pause-or-continue)
                                     ("r" . org-clock-report)
                                     ("s" . org-timer-stop)
                                     ("t" . org-timer-set-timer)
                                     ("C-c" .
                                      (compile
                                       ,compilation-python-type-check-cmd))))

(global-set-keys-to-prefix "C-c d" '(("," . dired-other-window)
                                     ("A" . add-dir-local-variable)
                                     ("C" . copy-dir-locals-to-file-locals)
                                     ("D" . delete-dir-local-variable)
                                     ("b" . ispell-buffer)
                                     ("d" . dired-default-directory-on-left)
                                     ("c" . ispell-comments-and-strings)
                                     ("e" . wdired-change-to-wdired-mode)
                                     ("f" . find-dired)
                                     ("l" . dictionary-lookup-definition)
                                     ("m" . my/vc-diff-against-main)
                                     ("n" . find-name-dired)
                                     ("p" . dired-at-point)
                                     ("s" . ispell)
                                     ("w" . ispell-word)))

(global-set-keys-to-prefix "C-c f" '(("," . find-file-other-window)
                                     ("A" . add-file-local-variable)
                                     ("C" . copy-dir-locals-to-file-locals)
                                     ("D" . delete-file-local-variable)
                                     ("a" . org-cycle-agenda-files)
                                     ("c" . fileloop-continue)
                                     ("f" . follow-mode)
                                     ("i" . my-visit-user-init-file)
                                     ("r" . recentf-open-files)
                                     ("v" . view-file)
                                     ("w" . copy-filename-to-kill)))

(global-set-keys-to-prefix "C-c g" '(("d" . pdb)
                                     ("g" . grep)
                                     ("k" . kill-grep)
                                     ("l" . lgrep)
                                     ("r" . rgrep)
                                     ("z" . zgrep)))

(global-set-keys-to-prefix "C-c l" '(("a" . gptel-add)
                                     ("c" . gptel-fn-complete)
                                     ;; Sometimes just a dictionary is required
                                     ("d" . dictionary-lookup-definition)
                                     ("f" . gptel-add-file)
                                     ("g" . gptel)
                                     ("m" . gptel-menu)
                                     ("o" . gptel-papers-summarise)
                                     ("q" . gptel-context-quit)
                                     ("r" . gptel-rewrite)
                                     ("s" . gptel-send)
                                     ("C-a" . gptel-abort)))

(global-set-keys-to-prefix "C-c h" '(("b" . highlight-compare-buffers)
                                     ("c" . highlight-changes-mode)
                                     ("f" . highlight-compare-with-file)
                                     ("h" . highlight-changes-remove-highlight)
                                     ("n" . highlight-changes-next-change)
                                     ("p" . highlight-changes-previous-change)
                                     ("r" . highlight-changes-rotate-faces)
                                     ("v" . highlight-changes-visible-mode)))

(global-set-keys-to-prefix "C-c m" '(("A" . org-agenda)
                                     ("a" . org-agenda-list)
                                     ("b" . ebib)
                                     ("c" . org-capture)
                                     ("i" . irc)
                                     ("m" . notmuch)
                                     ("n" . newsticker-show-news)
                                     ("t" . org-todo-list)
                                     ("v" . notmuch-view-html-in-eww-other-window)))

(global-set-keys-to-prefix "C-c m r" '(("r" . emms-play-from-radio-addr)
                                       ("C-s" . emms-stop)))

(global-set-keys-to-prefix "C-c p"  '(("n" . doc-view-other-frame-scroll-up)
                                      ("p" . doc-view-other-frame-scroll-down)))

(global-set-keys-to-prefix "C-c t" '(("g" . global-tab-line-mode)
                                     ("l" . toggle-theme)
                                     ("n" . tab-line-switch-to-next-tab)
                                     ("p" . tab-line-switch-to-prev-tab)
                                     ("t" . current-major-mode-tab-line-mode)))

(global-set-keys-to-prefix "C-c u" '(("b" .
                                      browser-url-at-point-with-external-browser)
                                     ("u" . eww)
                                     ("C-b" . eww-list-bookmarks)))

(global-set-keys-to-prefix "C-c w" '(("m" . windmove-mode)
                                     ("p" #'how-is-this-paper-looking?)
                                     ("s" . window-swap-states)
                                     ("t" . window-toggle-side-windows)))


;;  *********************
;;; * AFTER FIRST FRAME *
;;  *********************

(setq server-after-make-frame-hook #'startup)
(when (<= (length (frame-list)) 1)
  (desktop-clear)
  (startup))


;;  ***************
;;; * CUSTOM VARS *
;;  ***************
;; Put custom variables elsewhere
(setq custom-file (concat user-emacs-directory "custom.el"))
(provide 'init)
;;; init.el ends here
