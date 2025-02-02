;;;;
;;; records-tex.el: functions for working with latex mode on records-mode 
;;;                 records
;;; $Id: records-tex.el,v 1.2 2001/04/11 18:14:12 ashvin Exp $
;;;
;;; Copyright (C) 2000 by Johan W. Klüwer
;;; Copyright (C) 2000 by Ashvin Goel
;;;
;;; This file is under the Gnu Public License.

(require 'records)
(require 'records-util)

(defun records-href-quote-string (str)
  "Quote a string so that the hyperref latex package doesn't barf on it. 
Right now only the character # is quoted."
  (let ((len (length str)) (i 0) (new-str ""))
    (while (< i len)
      (if (eq (elt str i) ?#)
          (progn (setq new-str (concat new-str "\\\\#")))
        (setq new-str (concat new-str (char-to-string (elt str i)))))
      (setq i (1+ i)))
    new-str))

(defun records-link-to-href ()
  "Replace <..link..> with \href{...link...}{...link...} 
in the entire buffer. Used by hyperref.sty latex style file."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\(link: \\|\\)<\\([^\n]+\\)>" nil t)
      ;; ignore link in the subject of the record
      (if (equal "" (buffer-substring-no-properties 
                     (match-beginning 1) (match-end 1)))
          (let* ((match (buffer-substring-no-properties 
                         (match-beginning 2) (match-end 2)))
                 (new-match (records-href-quote-string match)))
            (replace-match (concat "\\\\href{\\2}{" new-match "}")))
      ))))

(defun records-href-to-link ()
  "Replace \href{...link...}{...link...}  with <..link..>."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\\\href{\\([^\n]+\\)}{[^\n]+}" nil t)
      (replace-match "<\\1>"))))

(defun records-todo-text-to-environment ()
  "Replace the records todo start and end markers with latex todo environment."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward 
	    (concat records-todo-begin-move-regexp "\\(.*\\)") nil t)
      (replace-match "\\\\begin{todo}{\\1}" t nil))
    (goto-char (point-min))
    (while (re-search-forward records-todo-end-regexp nil t)
      ;; t 2nd for optional preserve case
      (replace-match "\\\\end{todo}" t nil))
    ))

(defun records-todo-environment-to-text ()
  "Replace the latex todo environment with todo start and end markers."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\\\begin{todo}{\\([^}]+\\)}" nil t)
      (replace-match (concat records-todo-begin-move-regexp "\\1") t nil))
    (while (re-search-forward "\\\\end{todo}" nil t)
      ;; t 2nd for optional preserve case
      (replace-match records-todo-end-regexp t nil)) 
    ))

;;;###autoload
(defun records-narrow-latex ()
  "Narrow records-file buffer to the current record and switch to latex-mode."
  (interactive)
  (save-excursion
    (let ((point-pair (records-record-region)))
      (narrow-to-region (first point-pair) (second point-pair))
      (latex-mode)
      (records-link-to-href)
      (records-todo-text-to-environment)
      ;; (x-symbol-mode)
      ;; (setq x-symbol-8bits t)
      (font-lock-mode 1)
      (font-lock-fontify-buffer)
      ))
  (message "Use M-x records-widen-latex to switch back to records mode."))

;;; TODO: should check that going to records-mode makes sense
;;; (possibly by checking the name of the file).
;;;###autoload
(defun records-widen-latex ()
  "Widen records-file buffer and switch from latex to records-mode."
  (interactive)
  (widen)
  ;; (x-symbol-encode)
  (records-mode)
  (records-href-to-link)
  (records-todo-environment-to-text)
  (font-lock-mode 1)
  (font-lock-fontify-buffer)
  )

;;;###autoload
(defun records-concatenate-records-latex (num)
  "Concatenate the current record with the records on the same subject written
in the last NUM days and output in latex format. Then run latex on the output.
Output these records in the records latex output buffer (see 
records-latex-output-buffer). Without prefix arg, prompts for number of days.
An empty string will output the current record only. A negative number
will output all the past records on the subject!"
  (interactive "P")
  (let ((reverse (y-or-n-p "Sort records in most-recent first order ")))
    (records-concatenate-records-1 'latex records-latex-output-buffer num
                                   reverse))
  (save-excursion
    (set-buffer (get-buffer records-latex-output-buffer)) ;set buffer
    (records-link-to-href)              ;links
    (records-todo-text-to-environment)  ;todo
    (goto-char (point-max))
    ;; make it latex mode
    (insert "\\end{document}\n%%% Local " "Variables: \n%%% mode: latex\n"
            "%%% End: \n")
    (goto-char (point-min))
    (if (file-exists-p records-tex-template-plain)
        (insert-file records-tex-template-plain)
      (error (concat "Template file does not exist. "
                     "Look at variable records-tex-template-plain.")))
    (let ((old-buf (get-file-buffer records-tex-temp-output-file)))
      (if old-buf (kill-buffer old-buf)))
    (write-file records-tex-temp-output-file)
    (shell-command (concat "latex " records-tex-temp-output-file))
    ))

;;;###autoload
(defun records-concatenate-record-files-latex (num)
  (error "Function is not yet implemented"))

(provide 'records-tex)
