;;; gptel-papers --- GPTEL based functions to summarise papers.
;;
;; -*- lexical-binding: t -*-
;;
;;; Commentary:
;; Wraps around gptel to summarise abstracts and papers.
;;; Code:

(require 'gptel)
(require 'markdown-mode)

(defvar paper-title nil "Title of the current paper title.")
(defvar paper-info nil "Extra information to consider about the paper.")
(defvar paper-topics nil "Topics of the paper.")
(defun gptel-papers-insert-local-variables ()
  "Insert paper related file local variables."
  (interactive)
  (add-file-local-variable 'paper-title (read-string "Title: "))
  (add-file-local-variable 'paper-info (read-string "Info: "))
  (add-file-local-variable 'paper-topics (read-string "Topics: ")))

(defvar gptel-papers-summary-models
  '(meta-llama/Llama-4-Maverick-17B-128E-Instruct-FP8
    Qwen/QwQ-32B)
  "Models to use for summarising papers.")

(defun gptel-papers-display-response (response buffer)
  "Display RESPONSE in a side window named BUFFER."
  (with-current-buffer (get-buffer-create buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert response))
    (markdown-mode)
    (display-buffer (current-buffer)
                    `((display-buffer-in-side-window)
                      (side . right)
                      (window-width . 0.25)))))

(defun gptel-papers-handle-response (response info buffer)
  "Handle RESPONSE from gptel and INFO about the request & display in BUFFER."
  (if (not response)
      (message "gptel-request failed with message: %s"
               (plist-get info :status))
    (gptel-papers-display-response response buffer)))

(defun gptel-papers-validate-response (input-content response-buffer)
  "Validate the response in RESPONSE-BUFFER against the INPUT-CONTENT."
  (let ((lines (split-string input-content "\n")))
    (with-current-buffer response-buffer
      (end-of-buffer)
      (insert "\n\n*Papers not found:*\n\n"))
    (dolist (line lines)
      (let* ((cite-start (string-match "[\\]cite{[[:alnum:]_-]*}" line))
             (cite-end (match-end 0))
             (key-start (+ 6 cite-start))
             (key-end (1- cite-end)))
        (with-current-buffer response-buffer
          (let ((inhibit-read-only t))
            (when cite-start
              (if (re-search-forward
                   (substring line key-start key-end) nil t)
                  (end-of-line)
                (end-of-buffer))
              (insert (concat "\n- " line))
              (beginning-of-buffer))))))))

(defun gptel-papers-improve-response
    (original-prompt original-response n buffer)
  "Improve ORIGINAL-RESPONSE based on ORIGINAL-PROMPT."
  (let ((improve-prompt
         (concat "An LLM was given this prompt:\n"
                 original-prompt
                 "\nand produced this response:\n"
                 original-response
                 "\nImprove the response by addressing any inaccuracies,"
                 " enhancing clarity,"
                 " and ensuring it aligns with the task requirements. "
                 " Provide a revised response."))
        (gptel-model (nth
                      (mod n (length gptel-papers-summary-models))
                      gptel-papers-summary-models))
        (gptel-include-reasoning nil))
    (message "Refining prompt with %s (attempt %d)" gptel-model n)
    (gptel-request improve-prompt
      :context (list buffer content)
      :callback (lambda (response info)
                  (let ((buffer (car (plist-get info :context)))
                        (content (last (plist-get info :context))))
                    (gptel-papers-handle-response
                     response info buffer)
                    (gptel-papers-validate-response content buffer))))))

(defun gptel-papers-request-output-in-side-window
    (prompt buffer content &optional n)
  "Send a PROMPT to gptel displaying the result in a side window.
The side window is named BUFFER the prompt is refined with N times.
Finally CONTENT is used to check all papers have been included."
  (let ((gptel-model (car gptel-papers-summary-models)))
    (gptel-request prompt
      :context (list buffer content prompt n)
      :callback (lambda (response info)
                  (let ((buffer (car (plist-get info :context)))
                        (content (nth 1 (plist-get info :context)))
                        (prompt (nth 2 (plist-get info :context)))
                        (n (nth 3 (plist-get info :context))))
                    (gptel-papers-handle-response response info buffer)
                    (gptel-papers-validate-response content buffer)
                    (when (not (zerop n))
                      (gptel-papers-improve-response
                       prompt response n buffer)))))))

(defun gptel-papers-summarise (BEG END n)
  "Summarise a citation(s) between BEG and END with N refinements."
  (interactive "r\np")
  (let* ((gptel-temperature 0.3)
         (out-buffer "*paper-summary*")
         (contents (buffer-substring BEG END))
         (title (if paper-title paper-title (read-string "Title: ")))
         (info (if paper-info paper-info (read-string "Info: ")))
         (topics (if paper-topics paper-topics (read-string "Topics: ")))
         (prompt (concat
                  "I'm writing a paper titled: " title "\n"
                  "Task: Process references into category-specific summaries"
                  " for my manuscript.\n"
                  "Brief Description of the paper: " info "\n\n"
                  "**Instructions**\n"
                  "1. For each reference:\n"
                  "\t- If abstract exists: write a 1-sentence summary"
                  " focusing methodological relevance to my manuscript."
                  "Prioritise summaries that either: "
                  " a) Critique methods used in comparable studies or"
                  " b) Identify gaps my work addresses or"
                  " c) Suggest validation approaches for my methods."
                  "\t- If no abstract, create a place-holder summary"
                  " using the reference title and mark with [REVIEW NEEDED]."
                  "\n\n"
                  "2. Group outputs into these categories with strict matching:"
                  topics
                  "\n\n"
                  "3. Include the LaTeX citation command `\cite{KEY}`"
                  " after each summary.\n"
                  contents)))
    (gptel-papers-request-output-in-side-window
     prompt out-buffer contents (or n 0))))
;;; gptel-papers.el ends here
