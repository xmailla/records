;;;
;;; records-util.el
;;;
;;; $Id: records-util.el,v 1.15 2001/04/11 18:14:12 ashvin Exp $
;;;
;;; Copyright (C) 1996 by Ashvin Goel
;;;
;;; This file is under the Gnu Public License.

;;;###autoload
(defun records-create-todo ()
  "Create a records todo entry in the current record"
  (interactive)
  (save-excursion ;; make sure that we are on a subject
    (records-goto-subject))
  (if (not (bolp))
      (insert "\n"))
  (let (cur-point)
    (insert records-todo-begin-move-regexp "// created on " 
            (records-todays-date) "\n")
    (setq cur-point (point))
    (insert "\n" records-todo-end-regexp)
    (goto-char cur-point)))

;;;###autoload
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
	      (if (re-search-forward 
                   (concat "^" records-todo-end-regexp ".*\n") 
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

(autoload 'mc-encrypt-generic "mc-toplev" nil t)
(autoload 'mc-scheme-pgp   "mc-pgp"  nil t)
(autoload 'mc-scheme-pgp50 "mc-pgp5" nil t)
(autoload 'mc-scheme-gpg   "mc-gpg"  nil t)

(defun records-user-name ()
  "The user name of the records user."
  (if (not (boundp 'mc-default-scheme))
      (eval-when-compile (require 'mailcrypt)))
  (let ((user (cdr (assoc 'user-id (funcall mc-default-scheme)))))
    (cond ((not (null user)) user)
          (t (user-full-name)))))

;;;###autoload
(defun records-encrypt-record (arg)
  "Encrypt the current record for the current user.
With prefix arg, start the encryption from point to the end of record.
Records encryption requires the mailcrypt and mc-pgp (or mc-pgp5) packages."
  (interactive "P")
  (if (not (boundp 'mc-default-scheme))
      (eval-when-compile (require 'mailcrypt)))
  (save-excursion
    (let ((point-pair (records-record-region t))
          start end)
      (if arg (setq start (point))
        (setq start (first point-pair)))
      (setq end (second point-pair))
      (goto-char start)
      ;; sanity check
      (if (or (looking-at 
               (cdr (assoc 'msg-begin-line (funcall mc-default-scheme))))
	      (looking-at 
               (cdr (assoc 'signed-begin-line (funcall mc-default-scheme)))))
	  (error "records-encrypt-record: record is already encrypted."))
      (mc-encrypt-generic (records-user-name) nil 
                          start end (records-user-name) nil)
      )))

;;;###autoload
(defun records-decrypt-record ()
  "Decrypt the current record.
Records decryption requires the mailcrypt and mc-pgp (or mc-pgp5) packages."
  (interactive)
  (if (not (boundp 'mc-default-scheme))
      (eval-when-compile (require 'mailcrypt)))
  (save-excursion
    (let ((point-pair (records-record-region t)))
      (goto-char (first point-pair))
      (if (not (re-search-forward
                (concat "\\(" 
                        (cdr (assoc 'msg-begin-line
                                    (funcall mc-default-scheme)))
                        "\\|" 
                        (cdr (assoc 'signed-begin-line
                                    (funcall mc-default-scheme)))
                        "\\)") (mark) t))
          (error "records-decrypt-record: record is not encrypted."))
      (funcall (cdr (assoc 'decryption-func (funcall mc-default-scheme)))
               (match-beginning 0) (second point-pair)))))

;;;###autoload
(defun records-concatenate-records (num)
  "Concatenate the current record with the records on the same subject written
in the last NUM days. Output these records in the records output buffer (see 
records-output-buffer). Without prefix arg, prompts for number of days.
An empty string will output the current record only. A negative number
will output all the past records on the subject! Normally, the records are
output in most-recent first order. This function asks the user if the order
should be reversed."
  (interactive "P")
  (let ((reverse (y-or-n-p "Sort records in most-recent first order ")))
    (records-concatenate-records-1 nil records-output-buffer num reverse)))
  
(defun records-concatenate-records-1 (format output-buffer num reverse)
  "Concatenate the current record with the records on the same subject written
in the last NUM days. FORMAT is the formating of the buffer. 
Currently, plain and latex are supported. Output these records in BUFFER. 
An empty string will output the current record only. A negative number will 
output all the past records on the subject! Normally, the records are output in
most-recent first order. When the REVERSE argument is true, the order is 
reversed."
  (if (null num)
      (setq num (string-to-int
                 (read-from-minibuffer 
                  "Concat records in last N days (default 1): "))))
  (let* ((date (records-file-to-date))
	 (subject-tag (records-subject-tag t))
	 (subject (nth 0 subject-tag))
	 (tag (nth 1 subject-tag))
	 (first-ndate (records-add-date (records-normalize-date date)
				      (if (= num 0) -1 (- num))))
	 cur-buf point-pair bon-point eon-point prev-date-tag start-point)
         
    (if (< num 0)
	(setq first-ndate '(0 0 0)))
    ;; erase output buffer if needed
    (save-excursion
      (set-buffer (get-buffer-create output-buffer))
      (if records-erase-output-buffer
	  (erase-buffer)
	(goto-char (point-max)))
      ;; insert buffer header if needed
      (cond ((eq format nil)
             (insert (records-subject-on-concat subject)))
            ((eq format 'latex) t) ;; do nothing
            )
      (setq start-point (point)))
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
	    ;; insert the current record into output-buffer
	    (save-excursion
	      (set-buffer (get-buffer output-buffer))
	      (goto-char (if reverse start-point (point-max)))
              ;; insert record header
              (cond ((eq format nil)
                     (insert (records-date-on-concat 
                              (concat date (records-tag tag)))))
                    ((eq format 'latex)
                     (insert "\n\\begin{record}{" date "}{" subject "}\n")))
              ;; now insert the record
              (insert-buffer-substring cur-buf bon-point eon-point)
              ;; insert record tail
              (cond ((eq format nil) t)
                    ((eq format 'latex) (insert "\\end{record}\n\n")))
              )
	    ;; goto the previous record
	    (setq prev-date-tag (records-goto-prev-record 1 subject date tag
                                                          t t))
	    (setq date (nth 0 prev-date-tag))
	    (setq tag (nth 1 prev-date-tag))
	    ;; check if this record should be copied
	    (and prev-date-tag 
		 (records-ndate-lessp first-ndate 
				    (records-normalize-date date))))))
    ;; display/select
    (if records-select-buffer-on-concat
	(pop-to-buffer (get-buffer output-buffer))
      (display-buffer (get-buffer output-buffer)))))
    
;;;###autoload
(defun records-concatenate-record-files (num)
  "Concatenate all the records in the records files of the last NUM days. All
the records of a subject are collected together. Output these records in the
records output buffer (see records-output-buffer). Without prefix arg, prompts
for number of days. An empty string will output the records of the current
file."
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

;;;###autoload
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

;; bind the following to some simple key
;;      From Jody Klymak (jklymak@apl.washington.edu)
;; 01/10/2000 Support for url and gnus message id 
;;      From Rob Mihram 
;;;###autoload
(defun records-insert-link (&optional comment-string)
  "Writes the current buffer file name, url or message id
at the end of today's record and inserts a comment."
  (interactive "scomment: ")
  ;; should ideally do (eval-when-compile (require 'url)) but this 
  ;; package doesn't come with gnuemacs by default
  (require 'url)
  (save-excursion
    (let ((flink 
           (cond ((not (null buffer-file-name));; 1. normal file 
                  buffer-file-name)
                 ((not (null url-current-object));; 2. url page
                  (concat (aref url-current-object 0) "://" 
                          (aref url-current-object 3)
                          (aref url-current-object 5)))
                 ((eq major-mode 'gnus-summary-mode);; 3. gnus page
                  (progn
                    ;; silence byte compilation
                    (eval-when-compile (require 'mail-utils))
                    (eval-when-compile (require 'gnus))
                    (defvar gnus-current-headers)
                    (mail-strip-quoted-names 
                     (mail-header-message-id gnus-current-headers))))
                 (t (error "Couldn't create link.")))))
      (message "%s" flink)
       ;;; now we need to visit the buffer records-goto-today
      (if (one-window-p t) 
	  (split-window))
      (other-window 1)
      (records-goto-today)
      (goto-char (point-max))
      (insert "link: <" flink ">\n")
      (insert "" comment-string "\n")
      (other-window -1))))

(defun records-insert-template (&optional arg)
  "Insert a template into the current record based on the subject and the
association list records-template-alist (see the variable). Suitable for
calling from records-make-record-hook. Inserts template at end of record.
With non-null argument, inserts at beginning of record body.

Example hook:
 (add-hook 'records-make-record-hook 
          (function (lambda ()
                      (records-insert-template current-prefix-arg))))
"
  (interactive "P")
  (eval-when-compile (require 'tempo))
  (let ((subject nil)
        (tmpl nil)
        point-pair)
    (save-excursion
      (setq subject (records-goto-subject)))
    (setq tmpl (cdr (assoc subject records-template-alist)))
    (if tmpl
        (progn
          (setq point-pair (records-record-region t))
          (if arg
              (goto-char (first point-pair))
            (goto-char (second point-pair)))
          (tempo-insert-template 'tmpl nil)
          ))))

(defvar records-saved-subject-read-only nil)

(defun records-outline-mode (&optional arg)
  "Toggle outline minor mode for a records file.
With arg, turn outline minor mode on if arg is positive, off otherwise.
See the command `outline-mode' for more information on this mode."
  (interactive "P")
  (outline-minor-mode arg)
  (make-local-variable 'records-saved-subject-read-only)
  (if (eq outline-minor-mode t)
      ;; outline-minor-mode was turned on
      (if records-subject-read-only
          (progn 
            ;; make records-subject-read-only nil and remove the read-only
            ;; property so that subjects can be hidden
            (setq records-saved-subject-read-only t)
            (setq records-subject-read-only nil)
            (records-remove-read-only-property)
            ))
    ;; outline-minor-mode was turned off
    (setq records-subject-read-only records-saved-subject-read-only)
    (records-parse-buffer)
    ))

(provide 'records-util)
