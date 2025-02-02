;;;
;;; records-dindex.el
;;;
;;; $Id: records-dindex.el,v 1.4 2000/04/17 21:09:30 ashvin Exp $
;;;
;;; Copyright (C) 1996 by Ashvin Goel
;;;
;;; This file is under the Gnu Public License.

(defvar records-dindex-buffer nil
  "The name of the date-index buffer.
Initialized when the records date index file is loaded.")

(defun records-dindex-buffer (&optional modified)
  "Initialize the records date-index buffer from the date-index file.
If needed, create the date-index file.
If modified is t, check the file modification time since being visited."
  (if (and records-dindex-buffer
	   (get-buffer records-dindex-buffer))
      (if (and modified
	       (not (verify-visited-file-modtime 
		     (get-buffer records-dindex-buffer))))
	  ;; revert buffer since it has been modified under you.
	  (save-excursion
	    (set-buffer records-dindex-buffer)
	    (revert-buffer)))
    ;; get the dindex file
    (setq records-dindex-buffer (buffer-name 
                                 (find-file-noselect records-dindex-file)))
    (save-excursion 
      (set-buffer records-dindex-buffer)
      ;; add a newline at the end if needed
      (goto-char (point-max))
      (if (not (eq (char-before) ?\n))
          (progn (insert "\n") (records-dindex-save-buffer)))
      (setq buffer-read-only t))))

(defun records-dindex-save-buffer ()
  "Save the records date-index buffer."
  (records-index-save-buffer records-dindex-buffer))

(defun records-dindex-goto-date (date &optional no-error modified)
  "Goto the date in the date-index file.
If no-error is nil, raise error if date doesn't exist.
if no-error is t, return nil if (date, tag) doesn't exist and 
place point on the smallest date greater than the argument date."
  (records-dindex-buffer modified)
  (set-buffer records-dindex-buffer)
  (goto-char (1- (point-max)))
  ;; first check if date exists
  ;; start from end since we assume that
  ;; the last dates are most frequently used. 
  (if (re-search-backward (records-date-count-regexp date) (point-min) t)
      ;; found
      (progn
	(goto-char (match-beginning 0))
	(list 
	 ;; date
	 (buffer-substring-no-properties (match-beginning 1) (match-end 1))
	 ;; count
	 (buffer-substring-no-properties (match-beginning 2) (match-end 2)))
	)
    ;; date not found
    (if (null no-error)
	(error "records-dindex-goto-date: date " date " not found."))
    ;; search linearly and place point on next date
    ;; search is done from end because we assume that
    ;; the last dates are most frequently used. 
    (let ((ndate (records-normalize-date date))
          curr-date-count curr-ndate)
      (while ;; a do-while loop
	  (progn (setq curr-date-count (records-dindex-goto-prev-date))
		 (setq curr-ndate 
                       (if curr-date-count
                           (records-normalize-date (nth 0 curr-date-count))))
                 (and curr-date-count
                      (records-ndate-lessp ndate curr-ndate))))
      (if curr-date-count ;; go to next date if the previous date was found
          (records-dindex-goto-next-date))
      nil)))

(defun records-dindex-goto-prev-date (&optional arg)
  "Goto the arg^th previous date in the date-index buffer. 
Return the previous date or nil if the previous date doesn't exist."
  (if (re-search-backward (records-date-count-regexp) (point-min) t arg)
      ;; found
      (progn
	(list 
	 ;; date
	 (buffer-substring-no-properties (match-beginning 1) (match-end 1))
	 ;; count
	 (buffer-substring-no-properties (match-beginning 2) (match-end 2))))))

(defun records-dindex-goto-next-date (&optional arg)
  "Goto the arg^th next date in the date-index buffer. 
Return the next date or nil if the next date doesn't exist."
    ;; or else go to the next date
  (if (looking-at (records-date-count-regexp))
      (goto-char (match-end 0)))
  (if (re-search-forward (records-date-count-regexp) (point-max) t arg)
      (progn 
	(goto-char (match-beginning 0))
	(list 
	 ;; date
	 (buffer-substring-no-properties (match-beginning 1) (match-end 1))
	 ;; count
	 (buffer-substring-no-properties (match-beginning 2) (match-end 2))))))

(defun records-dindex-goto-relative-date(&optional arg date)
  "Invoke records-dindex-goto-prev-date or records-dindex-goto-next-date
depending on whether arg is negative or positive."
  (let ((date-count (if date (records-dindex-goto-date date t))))
    (cond ((null arg) date-count)
	  ((= arg 0) date-count)
	  ((< arg 0)
	   (records-dindex-goto-prev-date (- arg)))
	  ((> arg 0)
	   (records-dindex-goto-next-date arg)))))

(defun records-dindex-insert-record (date)
  "Insert a date into the records date-index."
  (save-excursion
    (let ((date-count (records-dindex-goto-date date t t)))
      (setq buffer-read-only nil)
      (if date-count
	  ;; date already exists, bump count
	  (progn
	    (goto-char (match-beginning 2))
	    (delete-region (match-beginning 2) (match-end 2))
	    (insert (int-to-string (1+ (string-to-int (nth 1 date-count))))))
	;; insert date + zero count
	(insert (concat date "#1 ")))
      (records-dindex-save-buffer)
      (setq buffer-read-only t))))

(defun records-dindex-delete-record (date)
  "Delete a date from the records date-index."
  (save-excursion
    (let* ((date-count (records-dindex-goto-date date nil t))
	   (count (string-to-int (nth 1 date-count))))
      (setq buffer-read-only nil)
      (if (> count 1)
	  ;; decrement count
	  (progn
	    (goto-char (match-beginning 2))
	    (delete-region (match-beginning 2) (match-end 2))
	    (insert (int-to-string (1- count))))
	;; remove date
	(delete-region (match-beginning 0) (match-end 0)))
      (records-dindex-save-buffer)
      (setq buffer-read-only t))))

(provide 'records-dindex)
