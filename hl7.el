;;; hl7.el --- Settings for HL7 modes -*- lexical-binding: t; -*-
;;; Code:
(require 'seq)

(defgroup hl7 nil
  "Settings for HL7 modes."
  :group 'data)

;; --- Internal overlay functions ---

(defun hl7--get-delimiter ()
  "Determine the HL7 field separator from the MSH segment.  Defaults to `|'."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^MSH\\(.\\)" nil t)
        (match-string 1)
      "|")))

(defun hl7--apply-overlays ()
  "Apply HL7 visual overlays to format segments vertically."
  ;; Clear existing overlays first to prevent stacking if called multiple times
  (hl7--remove-overlays)
  (save-excursion
    (goto-char (point-min))
    (let ((delim (hl7--get-delimiter)))
      ;; Find lines that start with exactly 3 alphanumerics
      (while (re-search-forward "^[A-Z0-9]\\{3\\}" nil t)
        (let ((field-num 1)
              (limit (line-end-position)))
          ;; Search strictly for the dynamic delimiter on the current line
          (while (search-forward delim limit t)
            (let* ((ov (make-overlay (1- (point)) (point)))
                   ;; Propertize the number to look distinctly different from the data
                   (display-str (propertize (format "\n%-4s%s"
                                                    (format "%d." field-num)
                                                    delim)
                                            'face 'shadow)))
              (overlay-put ov 'hl7-overlay 'field)
              (overlay-put ov 'display display-str))
            (setq field-num (1+ field-num))))))))

(defun hl7--remove-overlays ()
  "Remove all HL7 visual overlays, restoring the raw text view."
  (remove-overlays (point-min) (point-max) 'hl7-overlay 'field)
  (remove-overlays (point-min) (point-max) 'hl7-overlay 'fold))


;; --- Navigation ---

(defun hl7-next-segment (&optional arg)
  "Move point to the beginning of the next HL7 segment.
With prefix ARG, move forward ARG segments."
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (hl7-previous-segment (- arg))
    (dotimes (_ arg)
      (end-of-line)
      (if (re-search-forward "^[A-Z0-9]\\{3\\}" nil t)
          (beginning-of-line)
        (goto-char (point-max))
        (message "No more segments.")))))

(defun hl7-previous-segment (&optional arg)
  "Move point to the beginning of the previous HL7 segment.
With prefix ARG, move backward ARG segments."
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (hl7-next-segment (- arg))
    (dotimes (_ arg)
      (beginning-of-line)
      (if (re-search-backward "^[A-Z0-9]\\{3\\}" nil t)
          (beginning-of-line)
        (goto-char (point-min))
        (message "Beginning of segments.")))))


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
                 ;; Swapped cl-find-if for modern seq-find
                 (existing-fold (seq-find (lambda (o) (eq (overlay-get o 'hl7-overlay) 'fold))
                                          (overlays-in start end))))
            (if existing-fold
                (delete-overlay existing-fold)
              (let ((ov (make-overlay start end)))
                (overlay-put ov 'hl7-overlay 'fold)
                (overlay-put ov 'display (propertize "..." 'face 'shadow))
                (overlay-put ov 'priority 100))))
        (message "Not on an HL7 segment.")))))

;; --- The Minor Mode ---

(defvar hl7-visual-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'hl7-toggle-fold)
    (define-key map (kbd "M-n") 'hl7-next-segment)
    (define-key map (kbd "M-p") 'hl7-previous-segment)
    map)
  "Keymap for `hl7-visual-mode`.")

;;;###autoload
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

(provide 'hl7)
;;; hl7.el ends here
