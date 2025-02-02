;;;
;;; records-index.el
;;;
;;; $Id: records-index.el,v 1.14 2000/04/17 21:09:30 ashvin Exp $
;;;
;;; Copyright (C) 1996 by Ashvin Goel
;;;
;;; This file is under the Gnu Public License.

(defvar records-index-use-font-lock t
  "* Enable records index fontification.")

(defvar records-index-font-lock-keywords
  '(("^\\(.*\\): " 1 bold))
  "* Font-lock keywords for records index mode.")

(defvar records-index-buffer nil
  "The name of the index buffer.
Initialized when the records index file is loaded.")

(defvar records-index-mode-map nil
  "Key map for records index mode.")

(if records-index-mode-map
    nil
  (setq records-index-mode-map (make-sparse-keymap))
  (define-key records-index-mode-map "\r" 'records-index-goto-link)
  (define-key records-index-mode-map "\C-c\C-j" 'records-index-goto-link)
  (define-key records-index-mode-map [mouse-2] 
    'records-index-goto-mouse-link))

;;;###autoload
(defun records-index-mode ()
  "Records-index-mode with mouse support.
Key bindings are:
\\{records-index-mode-map}"
  (interactive)
  ;; make read only for user
  (setq buffer-read-only t)
  (records-index-parse-buffer)
  (setq major-mode 'records-index-mode mode-name "records-index")
  (use-local-map records-index-mode-map)
  (if records-index-use-font-lock
      (progn
	(eval-when-compile (require 'font-lock))
	;; (make-local-variable 'font-lock-no-comments)
	;; (setq font-lock-no-comments t)
	(make-local-variable 'font-lock-keywords)
	(setq font-lock-keywords records-index-font-lock-keywords)
	(font-lock-mode 1)))
  (run-hooks 'records-index-mode-hooks)
  )

(defmacro records-index-subject-regexp (&optional subject)
  "Regexp matching a subject in the records index file."
  ( `(if (, subject)
	 (concat "^\\(" (, subject) "\\): ")
       "^\\(.*\\): ")))

(defun records-index-parse-buffer ()
  "Parses the index buffer for records subject completion."
  (if (null records-index-buffer)
      (setq records-index-buffer (buffer-name (current-buffer))))
  (goto-char (point-min))
  ;; really a 'do' loop
  (while (progn
	   (forward-line 1)
	   (beginning-of-line)
	   (and (looking-at (records-index-subject-regexp))
		(intern (buffer-substring-no-properties
			 (match-beginning 1) (match-end 1))
			records-subject-table)
		))))

(defun records-index-buffer (&optional modified)
  "Initialize the records index buffer from the index file.
If needed, create the records directory and index file.
If modified is t, check the file modification time since being visited."
  (if (and records-index-buffer
	   (get-buffer records-index-buffer))
      (if (and modified
	       (not (verify-visited-file-modtime
		     (get-buffer records-index-buffer))))
	  ;; revert buffer since it has been modified under you.
	  (save-excursion
	    (set-buffer records-index-buffer)
	    (revert-buffer)))
    ;; get the index file
    (if (file-exists-p (expand-file-name records-index-file))
	()
      ;; see if records directory exists
      (if (not (file-directory-p (expand-file-name records-directory)))
	  ;; create records directory 
	  (make-directory (expand-file-name records-directory) t))
      ;; initialize records index file
      (write-region "-*- records-index -*-\n" nil records-index-file))
    ;; now get the index file
    (setq records-index-buffer (buffer-name 
			      (find-file-noselect records-index-file)))))

(defun records-index-save-buffer (&optional buf)
  "Save the records index buffer."
  ;; TODO: if this function is a no-op, things will still work.
  ;; the index buffers will be read-only, but not saved: modeline "--%*-" 
  ;; If indexes are changed frequently enough, this function ought to just
  ;; mark the index buffers so that they are eventually saved ...
  (let ((buf (if buf buf records-index-buffer)))
    (if (and buf
	     (get-buffer buf))
	(save-excursion
	  (set-buffer buf)
	  ;; no message would be better
	  (save-buffer)))))

(defun records-index-goto-subject (subject &optional switch no-error modified)
  "Goto the beginning of subject in the index buffer.
If switch is t, switch to the records index buffer.
If no-error is t and the subject is not found, then
place point at the beginning of the next subject."
  (records-index-buffer modified)
  (if switch
      (switch-to-buffer records-index-buffer t)
    (set-buffer records-index-buffer))
  (goto-char (point-min))
  (if (re-search-forward (records-index-subject-regexp subject) (point-max) t)
      (goto-char (match-beginning 0))
    (if (null no-error)
	(error (concat "records-index-goto-subject: subject " subject 
                       " not found.")))
    ;; search linearly until we get the next subject
    (while (let (match) ;; a do-while loop
	     (forward-line 1)
	     (beginning-of-line)
	     (setq match (looking-at (records-index-subject-regexp)))
	     (and match 
		  (string-lessp 
		  (buffer-substring-no-properties
		   (match-beginning 1)
		   (match-end 1)) subject))))))

(defun records-index-goto-date-tag (date tag &optional no-error)
  "Goto the (date, tag) in the index file.
Function assumes that point is at the beginning of the records index subject.
If no-error is nil, raise error if (date, tag) doesn't exist.
if no-error is t, return nil if (date, tag) doesn't exist and 
place point on the smallest (date, tag) pair greater than (date, tag)."
  ;; first check if (date, tag) exists
  (if (re-search-forward (concat date (records-tag tag)) (point-eoln) t)
      (progn ;; found
	(goto-char (match-beginning 0))
	(list date tag))
    ;; (date, tag) not found
    (if (null no-error)
	(error "records-index-goto-date-tag: " date " " tag " not found."))
    ;; search linearly and place point on next date
    (let ((ndate (records-normalize-date date)))
      (while ;; a do-while loop
	  (let* ((curr-date-tag (records-index-goto-next-date-tag))
		 (curr-ndate
		  (if curr-date-tag 
		      (records-normalize-date (nth 0 curr-date-tag)))))
	    (and curr-date-tag
		 (or (records-ndate-lessp curr-ndate ndate)
		     (and (records-ndate-equalp curr-ndate ndate)
			  (string-lessp (nth 1 curr-date-tag) tag))))
	    )))))

(defun records-index-goto-prev-date-tag (&optional arg)
  "Goto the arg^th previous (date, tag) in the index buffer. 
Return the previous (date, tag) 
or nil if the previous (date, tag) doesn't exist."
  (if (re-search-backward records-date-tag-regexp (point-boln) t arg)
      (progn
	(list 
	 ;; date
	 (buffer-substring-no-properties (match-beginning 1) (match-end 1))
	 (if (match-beginning 3)
	     ;; tag
	     (buffer-substring-no-properties (match-beginning 3) 
					     (match-end 3))
	   ;; empty tag
	   ""))
	)))

(defun records-index-goto-next-date-tag (&optional arg)
  "Goto the arg^th next (date, tag) in the index buffer.
Return the next (date, tag),
or nil if the next (date, tag) doesn't exist."
  (if (looking-at (records-index-subject-regexp))
      ;; go to the front of the subject 
      (goto-char (match-end 0))
    ;; or else go to the next date
    (if (looking-at records-date-tag-regexp)
	(goto-char (match-end 0))))
  (if (re-search-forward records-date-tag-regexp (point-eoln) t arg)
      (progn 
	(goto-char (match-beginning 0))
	(list 
	 ;; date
	 (buffer-substring-no-properties (match-beginning 1) (match-end 1))
	 (if (match-beginning 3)
	     ;; tag
	     (buffer-substring-no-properties (match-beginning 3) 
					     (match-end 3))
	   ;; empty tag
	   "")))))

(defun records-index-goto-relative-date-tag(&optional arg date tag)
  "Invoke records-index-goto-prev-date-tag or records-index-goto-next-date-tag
depending on whether arg is negative or positive.
Returns the new (date, tag)."
  (let ((date-tag (if date (records-index-goto-date-tag date tag t))))
    (cond ((null arg) date-tag)
	  ((= arg 0) date-tag)
	  ((< arg 0)
	   (records-index-goto-prev-date-tag (- arg)))
	  ((> arg 0)
	   (records-index-goto-next-date-tag arg)))))

(defun records-index-goto-link()
  "Go to the link under point in the records index file."
  (interactive)
  (save-excursion
    (skip-chars-backward "0-9#" (point-boln))
    (let (subject date tag)
    (if (not (looking-at records-date-tag-regexp))
	;; didn't find a date, tag
	(error "records-index-goto-link: invalid link.")
      ;; found date and tag. get subject
      (setq date (buffer-substring-no-properties (match-beginning 1)
						 (match-end 1)))
      (setq tag 
	    (if (match-beginning 3) 
		(buffer-substring-no-properties (match-beginning 3) 
						(match-end 3))
	      ""))
      (beginning-of-line)
      (if (not (looking-at (records-index-subject-regexp)))
	  (error "records-index-goto-link: subject not found."))
      (setq subject (buffer-substring-no-properties (match-beginning 1)
						    (match-end 1)))
      (records-goto-record subject date tag t)
      ))))

(defun records-index-goto-mouse-link(e)
  (interactive "e")
  (mouse-set-point e)
  (records-index-goto-link))

(defun records-index-insert-subject (subject)
  "Insert a record subject into the records index."
  (if (looking-at (records-index-subject-regexp subject))
      ;; no insertion required
      ()
    (save-excursion (insert (concat subject ": \n"))
		    (intern subject records-subject-table))))

(defun records-index-delete-subject (subject)
  (beginning-of-line)
  (if (records-index-goto-next-date-tag)
      ;; other guys exist. don't do anything
      ()
    ;; make sure that we are removing the correct subject
    (beginning-of-line)
    (if (not (looking-at (records-index-subject-regexp subject)))
	(error "records-index-delete-subject: invalide subject.")
      ;; ask for confirmation
      (if (y-or-n-p (concat "Delete subject: " subject " "))
	  ;; the 1+ is for the newline
	  (progn
	    (delete-region (point) (1+ (point-eoln)))
	    (unintern subject records-subject-table))))))

(defun records-index-insert-record (subject date tag)
  "Insert a record into the records index."
  (save-excursion
    (records-index-goto-subject subject nil t t)
    (setq buffer-read-only nil)
    (records-index-insert-subject subject)
    (if (records-index-goto-date-tag date tag t)
	(error (concat "records-index-insert-record: Record " date " " tag 
		       " exists already."))
      ;; now insert
      (insert (concat date (records-tag tag) " "))
      (records-index-save-buffer)
      (setq buffer-read-only t)
      )))

(defun records-index-delete-record (subject date tag)
  "Delete a record from the records index."
  (save-excursion
    (records-index-goto-subject subject nil nil t)
    (records-index-goto-date-tag date tag)
    ;; the 1+ is for the extra space
    (setq buffer-read-only nil)
    (delete-region (match-beginning 0) (1+ (match-end 0)))
    (records-index-delete-subject subject)
    (records-index-save-buffer)
    (setq buffer-read-only t)
    ))

(put 'records-index 'mode-class 'special)
  
(provide 'records-index)
(if (not (featurep 'records))
    (require 'records))
