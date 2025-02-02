;;
;;; records-extra.el
;;; 
;;; $Id: records-search.el,v 1.2 2000/04/17 21:09:30 ashvin Exp $
;;;
;;; Copyright (C) 1999 by Kaarthik Sivakumar
;;;
;;; This file is under the GNU Public License


;; The search function currently does the following:
;;
;; 1. Given a search string, the function will search records before or
;;    after the current record for a search string.
;;
;; 2. If search string is found, then that record will be displayed with
;;    point at the end of the string.
;;
;; 3. If search string is not found, then error.

;; Enhancements:
;; 1. Typing the search string in the minibuffer should not be needed. C-s
;;    doesn't seem to require that. It does incremental search as you keep
;;    typing.
;; 
;; 2. Redoing a search requires simply typing C-s
;; 
;; 3. Search wrap should be possible
;; 
;; 4. Reverse search should leave point at the beginning of the search string
;;    and forward search should leave point at the end of the search.

;;;###autoload
(defun records-search-forward (regexp)
  "Search forward for regular expression in current subject."
  (interactive "sEnter search string: ")
  (records-search-in-subject regexp)
)

;;;###autoload
(defun records-search-backward (regexp)
  "Search backward for regular expression in current subject."
  (interactive "sEnter search string: ")
  (records-search-in-subject regexp t)
)

(defun records-search-in-subject (string &optional reverse no-error)
  "Search for string in subject. If optional prefix arg is given,
search backward."
  (let (result)
    ;; The method is as follows.
    ;; 1. Check if there is any record before or after this record, depending
    ;; on the value of 'reverse'.
    ;; 2. If there is a record, then search in that record for the regexp.
    ;; 3. If the result is not found in that record, then go back to the while
    ;; and repeat the loop.
    ;; The loop will stop under the following conditions.
    ;; 1. 'records-search-get-next-record' returns nil, in which case 'result'
    ;; will also be nil.
    ;; 2. 'records-search-do-search' returns non-nil (which will be the
    ;; position of the regexp in the buffer), in which case 'result' will be
    ;; set to that value. So to find out why the loop was exited, just check
    ;; the value of 'result'.
    ;; Using that value, we know if the regexp was found or not.
    (while (and (records-search-get-next-record reverse)
		(not (setq result (records-search-do-search string)))))
    (if result ; If 'result' is non-nil, then regexp was found.
	(progn
	  (message "String found")
          ;; pop-to-buffer where the regexp was found.
	  (pop-to-buffer (current-buffer)))
      (if (null no-error) ; 'result' was nil. See if error has to be reported.
	  (error "String not found")
	nil))))

(defun records-search-get-next-record (reverse)
  " *Get the next or prev record, depending on the value of 'reverse'.
If 'reverse' is nil, then get the next forward, else get the prev record."
  ;; The following function sets the file containing the record as the current
  ;; buffer. So we can do all our operations on the current buffer itself.
  (records-goto-relative-record
   (if (not (null reverse)) -1 +1) nil nil nil t t))

(defun records-search-do-search (string)
  " *Internal function to search for string in record."
  (records-mark-record) ; mark record so that ...
  (narrow-to-region (mark) (point)) ; .. it can be narrowed down to.
  ;; go to beginning of buffer (which is beginning of the narrowed region).
  (beginning-of-buffer)
  (let (position)
    (if (re-search-forward string nil t) ; search forward for the regexp.
        ;; set 'position' to match-beginning
	(setq position (match-beginning 0)))
    (widen) ; widen the buffer.
    position)) ; return position.

(provide 'records-search)

;; Trying to use isearch functionality, 
;; but unfortunately, it is not easy to reuse isearch
;; Here is a start, but I am not sure if this is the best way to try to do it
;; Probably copying the relevant code from isearch is going to be easier
;; -Ashvin
;;

;(setq orig-search-function (symbol-function 'isearch-search))
;(setq records-search-in-progress nil)

;(defun records-search-wrapper ()
;  (records-search-in-subject isearch-string t)
;  (setq isearch-buffer (current-buffer)) ;; hack
;  )

;(defun records-isearch-setup ()
;  (if records-search-in-progress
;      (fset 'isearch-search 'records-search-wrapper)))

;(defun records-isearch-end-setup ()
;  (if records-search-in-progress
;      (progn
;        (fset 'isearch-search orig-search-function)
;        (setq records-search-in-progress nil))))

;(add-hook 'isearch-mode-hook 'records-isearch-setup)
;(add-hook 'isearch-mode-end-hook 'records-isearch-end-setup)

;(defun records-search-forward (&optional regexp)
;  "Search forward for regular expression in current subject."
;  (interactive "_P")
;  (setq records-search-in-progress t)
;  (isearch-mode t (not (null regexp)) nil (not (interactive-p)))
;  )

;(defun records-search-backward (&optional regexp)
;  "Search backward for regular expression in current subject."
;  (interactive "_P")
;  (setq records-search-in-progress t)
;  (isearch-mode nil (not (null regexp)) nil (not (interactive-p)))
;  )

