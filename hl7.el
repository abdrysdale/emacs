(require 'cl-lib)

(defgroup hl7 nil
  "Settings for HL7 modes."
  :group 'data)

;; --- Internal overlay functions ---

(defun hl7--apply-overlays ()
  "Apply HL7 visual overlays to format segments vertically."
  (save-excursion
    (goto-char (point-min))
    ;; Find lines that start with exactly 3 alphanumerics
    (while (re-search-forward "^[A-Z0-9]\\{3\\}" nil t)
      (let ((field-num 1)
            (limit (line-end-position)))
        ;; Search strictly for every pipe on the current line
        (while (search-forward "|" limit t)
          ;; (point) is now immediately AFTER the pipe.
          ;; We create an overlay over just the pipe character itself.
          (let ((ov (make-overlay (1- (point)) (point))))
            (overlay-put ov 'hl7-overlay 'field)
            (overlay-put ov 'display (format "\n%-4s|" (format "%d." field-num))))
          (setq field-num (1+ field-num)))))))

(defun hl7--remove-overlays ()
  "Remove all HL7 visual overlays, restoring the raw text view."
  (remove-overlays (point-min) (point-max) 'hl7-overlay 'field)
  (remove-overlays (point-min) (point-max) 'hl7-overlay 'fold))


;; --- Interactive commands ---

(defun hl7-toggle-fold ()
  "Toggle folding of the current HL7 segment."
  (interactive)
  (if (not hl7-visual-mode)
      (message "Enable hl7-visual-mode to fold segments.")
    (save-excursion
      (beginning-of-line)
      (if (looking-at "^[A-Z0-9]\\{3\\}")
          (let* ((start (+ (point) 3))
                 (end (line-end-position))
                 (existing-fold (cl-find-if (lambda (o) (eq (overlay-get o 'hl7-overlay) 'fold))
                                            (overlays-at start))))
            (if existing-fold
                (delete-overlay existing-fold)
              (let ((ov (make-overlay start end)))
                (overlay-put ov 'hl7-overlay 'fold)
                (overlay-put ov 'display "...")
                ;; Priority ensures the fold hides the field overlays
                (overlay-put ov 'priority 100))))
        (message "Not on an HL7 segment.")))))


;; --- The Minor Mode ---

(defvar hl7-visual-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'hl7-toggle-fold)
    map)
  "Keymap for `hl7-visual-mode`.")

(define-minor-mode hl7-visual-mode
  "Minor mode to visually format HL7 messages.
Toggles `view-mode` (read-only) and formats the text vertically."
  :init-value nil
  :lighter " HL7-Vis"
  :keymap hl7-visual-mode-map
  (if hl7-visual-mode
      (progn
        (hl7--apply-overlays)
        (view-mode 1)
        (message "HL7 Visual Mode ON (Read-Only)"))
    (progn
      (hl7--remove-overlays)
      (view-mode -1)
      (message "HL7 Visual Mode OFF (Edit Mode)"))))
