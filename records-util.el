;;;
;;; records-util.el
;;;
;;; $Id: records-util.el,v 1.7 1999/11/13 23:10:34 ashvin Exp $
;;;
;;; Copyright (C) 1996 by Ashvin Goel
;;;
;;; This file is under the Gnu Public License.

(defun records-create-todo ()
  "Create a records todo entry in the current record"
  (interactive)
  (save-excursion ;; make sure that we are on a subject
    (records-goto-subject))
  (if (not (bolp))
      (insert "\n"))
  (let (cur-point)
    (insert records-todo-begin-move-regexp "// created on " (records-todays-date) "\n")
    (setq cur-point (point))
    (insert "\n" records-todo-end-regexp)
    (goto-char cur-point)))

(defun records-get-todo (&optional date)
  "Insert the previous record files todo's into the date file.
See the records-todo-.*day variables on when it is automatically invoked."
  (interactive)
  (if date
      (records-goto-record nil date "" nil t nil nil nil)
    (setq date (records-file-to-date)))
  (save-excursion
    (let* ((new-buf (current-buffer))
	   (prev-date (records-goto-prev-record-file 1 t t))
	   (cur-buf (current-buffer))) ;; this is the prev-date buffer
      (if (null prev-date)
	  () ;; nothing to do
	(goto-char (point-min))
	(while (records-goto-down-record nil t) ;; record exists
	  ;; start the magic
	  (let* ((subject (nth 0 (records-subject-tag t))) ;; first record
                 (point-pair (records-record-region))
                 (bon-point (first point-pair))
                 (eon-point (second point-pair))
		 bt-point et-point move subject-inserted)
            (goto-char bon-point)
	    ;; process all the todo's in the current record
	    (while (re-search-forward records-todo-begin-regexp eon-point 'end)
	      ;; do the copy/move thing for the current todo
	      (setq bt-point (match-beginning 0))
	      (setq move (match-beginning 2))
	      ;; goto end of todo
	      (if (re-search-forward (concat "^" records-todo-end-regexp ".*\n") 
                                     eon-point 'end)
		  (setq et-point (match-end 0))
		(setq et-point (point)))
	      ;; for move, save the regions in the old file
	      (if move (setq records-todo-move-regions 
			     (cons (list bt-point et-point)
				   records-todo-move-regions)))
	      ;; now copy the region to the new file
	      (save-excursion
		(set-buffer new-buf)
		(goto-char (point-max))
		(if (not subject-inserted)
		    (progn (records-insert-record subject) 
			   (setq subject-inserted t)))
		(insert-buffer-substring cur-buf bt-point et-point)
		;; insert an extra newline - this is useful for empty records
		(insert "\n")))))
	;; end of record processing. 
        ;; for todo-move, remove regions from old file
	(let ((prev-modified (buffer-modified-p)))
	  (while records-todo-move-regions
	    (goto-char (car (car records-todo-move-regions)))
	    (apply 'delete-region (car records-todo-move-regions))
	    ;; do the records-todo-delete-empty-record
	    (if (and records-todo-delete-empty-record (records-body-empty-p))
		(records-delete-record nil t))
	    (setq records-todo-move-regions
		  (cdr records-todo-move-regions)))
	  (if (or prev-modified (not (buffer-modified-p)))
              () ;; don't do anything
            (save-buffer)
            (records-delete-empty-record-file)
            ))))))

(defun records-user-name ()
  "The user name of the records user."
  (eval-when-compile (load "mc-pgp"))
  (cond ((boundp 'mc-ripem-user-id)
	 mc-ripem-user-id)
	((boundp 'mc-pgp-user-id)
	 mc-pgp-user-id)
	(t (user-full-name))))

(defun records-encrypt-record (arg)
  "Encrypt the current record for the current user.
With prefix arg, start the encryption from point to the end of record.
Records encryption requires the mailcrypt and mc-pgp packages."
  (interactive "P")
  (if (not (fboundp 'mc-pgp-encrypt-region))
      (load "mc-pgp"))
  (save-excursion
    (let ((point-pair (records-record-region t))
          start)
      (if arg (setq start (point))
        (setq start (first point-pair)))
      (goto-char start)
      ;; sanity check
      (if (or (looking-at mc-pgp-msg-begin-line)
	      (looking-at mc-pgp-signed-begin-line))
	  (error "records-encrypt-record: record is already encrypted."))
      (mc-pgp-encrypt-region (list (records-user-name)) 
                             start 
                             (second point-pair)
                             (records-user-name) nil))))

(defun records-decrypt-record ()
  "Decrypt the current record.
Records decryption requires the mailcrypt and mc-pgp packages."
  (interactive)
  (if (not (fboundp 'mc-pgp-decrypt-region))
      (load "mc-pgp"))
  (save-excursion
    (let ((point-pair (records-record-region t)))
      (goto-char (first point-pair))
      (if (not (re-search-forward
                (concat "\\(" mc-pgp-msg-begin-line "\\|" 
                        mc-pgp-signed-begin-line "\\)") (mark) t))
          (error "records-decrypt-record: record is not encrypted."))
      (mc-pgp-decrypt-region (match-beginning 0) 
                             (second point-pair)
                             ))))

(defun records-concatenate-records (num)
  "Concatenate the current record with the records on the same subject written
in the last NUM days. Output these records in the records output buffer (see 
records-output-buffer). Without prefix arg, prompts for number of days.
An empty string will output the current record only. A negative number
will output all the past records on the subject!!"
  (interactive
   (list
    (if current-prefix-arg (int-to-string current-prefix-arg)
      (read-from-minibuffer "Concat records in last N days (default 1): "))))
  (let* ((date (records-file-to-date))
	 (subject-tag (records-subject-tag t))
	 (subject (nth 0 subject-tag))
	 (tag (nth 1 subject-tag))
	 (arg (string-to-int num))
	 (first-ndate (records-add-date (records-normalize-date date)
				      (if (= arg 0) -1 (- arg))))
	 cur-buf point-pair bon-point eon-point prev-date-tag)

    (if (< arg 0)
	(setq first-ndate '(0 0 0)))
    ;; erase output buffer if needed
    ;; print subject
    (save-excursion
      (set-buffer (get-buffer-create records-output-buffer))
      (if records-erase-output-buffer
	  (erase-buffer)
	(goto-char (point-max)))
      (insert (records-subject-on-concat subject)))
    ;; start with current record
    (save-excursion
      (while ;; do-while loop 
	  (progn
	    ;; get the current records's buffer, beg-point and end-point.
	    (setq point-pair (records-record-region t))
	    (setq cur-buf (buffer-name))
	    (setq bon-point (first point-pair))
	    (setq eon-point (second point-pair))
            (goto-char bon-point)
	    ;; insert the current record into records-output-buffer
	    (save-excursion
	      (set-buffer (get-buffer records-output-buffer))
	      (goto-char (point-max))
	      (insert (records-date-on-concat (concat date (records-tag tag))))
	      (insert-buffer-substring cur-buf bon-point eon-point))
	    ;; goto the previous record
	    (setq prev-date-tag (records-goto-prev-record 1 subject date tag t t))
	    (setq date (nth 0 prev-date-tag))
	    (setq tag (nth 1 prev-date-tag))
	    ;; check if this record should be copied
	    (and prev-date-tag 
		 (records-ndate-lessp first-ndate 
				    (records-normalize-date date))))))
    ;; display/select
    (if records-select-buffer-on-concat
	(pop-to-buffer (get-buffer records-output-buffer))
      (display-buffer (get-buffer records-output-buffer)))))
    
(defun records-concatenate-record-files (num)
  "Concatenate all the records in the records files of the last NUM days.
All the records of a subject are collected together. Output these records in the
records output buffer (see records-output-buffer). Without prefix arg, prompts
for number of days. An empty string will output the records of the current file."
  (interactive
   (list
    (if current-prefix-arg (int-to-string current-prefix-arg)
      (read-from-minibuffer "Concat records files in last N days (default 1): "
			    ))))
  (let* ((date (records-file-to-date))
	 (arg (string-to-int num))
	 (first-ndate (records-add-date (records-normalize-date date)
				      (if (= arg 0) -1 (- arg))))
	 records-subject-list)
    ;; erase output buffer if needed
    (save-excursion
      (set-buffer (get-buffer-create records-output-buffer))
      (if records-erase-output-buffer
	  (erase-buffer)
	(goto-char (point-max))))
    (save-excursion
      (while ;; loop thru. all files
	  (progn ;; do-while loop 
	    ;; goto the beginning of the file
	    (goto-char (point-min))
	    ;; loop thru. all records in a file
	    (while (records-goto-down-record nil t) 
	      (let* ((subject (nth 0 (records-subject-tag t)))
		     (tag  (nth 1 (records-subject-tag t)))
                     (point-pair (records-record-region t))
		     (bon-point (first point-pair))
		     (eon-point (second point-pair))
		     subject-mark omark record)
		;; get subject-mark
		(setq subject-mark (assoc subject records-subject-list))
		(if subject-mark
		    ()
		  (save-excursion
		    (set-buffer (get-buffer records-output-buffer))
		    ;; make a new marker
		    (setq omark (point-max-marker))
		    (goto-char omark)
		    ;; insert subject header 
		    (insert-before-markers (records-subject-on-concat subject))
		    (goto-char omark)
		    (insert "\n")) ;; this does a lot of the trick for markers
		  ;; add subject and new marker to list
		  (setq subject-mark (list subject omark))
		  (setq records-subject-list
			(append records-subject-list (list subject-mark))))
		(setq record (buffer-substring bon-point eon-point))
		(save-excursion
		  (set-buffer (get-buffer records-output-buffer))
		  (goto-char (nth 1 subject-mark))
		  (insert-before-markers 
		   (records-date-on-concat (concat date (records-tag tag))))
		  (insert-before-markers record))
		(goto-char eon-point)))
	    (setq date (records-goto-prev-record-file 1 t))
	    ;; check if this record should be copied
	    (and date (records-ndate-lessp first-ndate 
					 (records-normalize-date date))))))
    ;; clean up records-subject-list
    (while records-subject-list
      (set-marker (nth 1 (car records-subject-list)) nil)
      (setq records-subject-list (cdr records-subject-list)))
    ;; display/select
    (if records-select-buffer-on-concat
	(pop-to-buffer (get-buffer records-output-buffer))
      (display-buffer (get-buffer records-output-buffer)))))

(defun records-goto-calendar ()
  "Goto the calendar date in the current record file."
  (interactive)
  (let* ((date (records-file-to-date))
	 (ndate (records-normalize-date date))
	 ;; convert normalized date to calendar date
	 ;; the day and month are interchanged
	 (cdate (list (nth 1 ndate) (nth 0 ndate) (nth 2 ndate))))
    (eval-when-compile (require 'calendar))
    (calendar)
    (calendar-goto-date cdate)))

;;;###autoload
(defun records-calendar-to-record ()
  "Goto the record file corresponding to the calendar date."
  (interactive)
  (let* ((cdate (calendar-cursor-to-date))
	 (ndate (list (nth 1 cdate) (nth 0 cdate) (nth 2 cdate)))
	 (date (records-denormalize-date ndate)))
    (records-goto-record nil date nil nil 'other)))

;;;###autoload
(defun records-insert-record-region (beg end)
  "Insert the region in the current buffer into today's record.
Prompts for subject."
  (interactive "r")
  (let ((record-body (buffer-substring beg end)))
    (records-goto-today)
    (goto-char (point-max))
    (records-insert-record nil record-body)))

;;;###autoload
(defun records-insert-record-buffer ()
  "Insert the current buffer into today's record.
Prompts for subject."
  (interactive)
  (records-insert-record-region (point-min) (point-max)))

(provide 'records-util)
