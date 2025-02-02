;;;
;;; records-vars.el
;;;
;;; $Id: records-vars.el,v 1.11 2001/04/11 18:14:12 ashvin Exp $
;;;
;;; Copyright (C) 1996 by Ashvin Goel
;;;
;;; This file is under the Gnu Public License.

;;;
;;; The next set of variables are accessed by recordsadmin.
;;; Do not set them explicitly since they are set in your
;;; records initilization file (see records-init-file) when recordsadmin 
;;; is run. Beware!
;;;

(defvar records-init-file (concat (getenv "HOME") "/.emacs-records")
  "* All the records initialization should be put in this file.
This file is also read by the perl index generator.
If you change this variable, you must change the perl scripts also.")

(defvar records-directory (concat (getenv "HOME") "/records")
  "* Directory under which all records are stored.")

(defvar records-index-file (concat records-directory "/index")
  "* File name in which records subject index is stored.")

(defvar records-dindex-file (concat records-directory "/dindex")
  "* File name in which records date index is stored.")

(defvar records-directory-structure 1
  "* The directory structure for records files. Its values can be 
0 => all records are stored in the variable records-directory. 
1 => records are stored by year.
2 => records are stored by year and month.")

(defvar records-day-order 0
  "* A records file name is composed of a day, month and year.
This variable determines the order of the day in date. 
Valid values are 0, 1 or 2 only.")

(defvar records-month-order 1
  "* A records file name is composed of a day, month and year.
This variable determines the order of the month in date. 
Valid values are 0, 1 or 2 only.")

(defvar records-year-order 2
  "* A records file name is composed of a day, month and year.
This variable determines the order of the month in date. 
Valid values are 0, 1 or 2 only.")

(defvar records-year-length 4
  "* The length of a records file year. Valid values are 2 or 4 only.")

;;;
;;; You are free to play with these variables.
;;; Use M-x set-variable records-.* (records- followed by completion) 
;;; to see all these variables.
;;;
(defcustom records-use-font-lock t 
  "* Enable fontification in records mode.
If font-lock-auto-fontify is t, records may be fontified even if this
value is nil. In that case, to turn off records fontification, add
\"records-mode\" as an element into the list called 
font-lock-mode-disable-list."
  :type 'boolean
  :group 'records
)

; silence font lock in emacs (xemacs doesn't seem to have this problem)
(defvar bold 'bold)

;; for compatibility with xemacs-20
;; (find-face 'font-lock-warning-face)
(if (memq 'font-lock-warning-face (face-list))
    ()
  (defface font-lock-warning-face
    '((((class color) (background light)) (:foreground "Red" :bold t))
      (((class color) (background dark)) (:foreground "Pink" :bold t))
      (t (:inverse-video t :bold t)))
    "Font Lock mode face used to highlight warnings."
    :group 'font-lock-faces)
)

;; TODO: hard to make it customizable: look at tag-table-alist in etags.el 
(defvar records-mode-font-lock-keywords
  '(
    ;; subject face: this regexp obtained from records-subject-regexp
    ("^\\* \\(.*\\)\n\\-\\-\\-+$" . bold)
    ("^\\(END_\\)?TODO:?" . font-lock-warning-face) ; todo
    ;; ("^link:\\ <\\(.*\\)>$" 1 font-lock-reference-face) ; link
    ("<\\([^\n]*\\)>" 1 font-lock-reference-face) ; link
    ("^TODO:\\ \\(//\\ .*$\\)" 1 font-lock-comment-face)) ; todo comment
  "* Font-lock keywords for records mode."
  )

(defcustom records-subject-read-only t
  "* If t, records subjects are made read-only.
This disables any accidental updates to a records subject. 
This variable has a local value for each records buffer."
  :type 'boolean
  :group 'records
  )

;; todo variables
(defcustom records-todo-prev-day nil
  "* If t, records-get-todo is invoked for a new file from 
records-goto-prev-day. A file is new if it does not have any records in it.
If nil, records-get-todo is not invoked.
If not nil and not t, user is asked whether records-get-todo should be 
invoked."
  :type '(choice (const nil)
                 (const t)
                 (const ask))
  :group 'records
  )

(defcustom records-todo-next-day nil
  "* If t, records-get-todo is invoked for a new file from 
records-goto-next-day.
If nil, records-get-todo is not invoked.
If not nil and not t, user is asked whether records-get-todo should be
invoked."
  :type '(choice (const nil)
                 (const t)
                 (const ask))
  :group 'records
)

(defcustom records-todo-today t
  "* If t, records-get-todo is invoked for a new file from records-goto-today.
If nil, records-get-todo is not invoked.
If not nil and not t, user is asked whether records-get-todo should be
invoked."
  :type '(choice (const nil)
                 (const t)
                 (const ask))
  :group 'records
)

(defcustom records-todo-begin-copy-regexp "CTODO: "
  "* The beginning of the copy todo is recognized by this variable. 
NOTE: this variable will be recognized at the beginning of a line."
  :type 'string
  :group 'records
)

(defcustom records-todo-begin-move-regexp "TODO: "
  "* The beginning of the move todo is recognized by this variable.
NOTE: this variable will be recognized at the beginning of a line."
  :type 'string
  :group 'records
)

(defcustom records-todo-end-regexp "END_TODO"
  "* The end of both the copy and move todo is recognized by this variable.
NOTE: this variable will be recognized at the beginning of a line."
  :type 'string
  :group 'records
)

(defcustom records-todo-delete-empty-record t
  "* If t, delete record if it is empty after a todo move.
If nil, don't delete record.
If not nil and not t, ask user about deleting the record."
  :type '(choice (const nil)
                 (const t)
                 (const ask))
  :group 'records  
)

(defcustom records-history-length 10
  "* The number of records that are stored in records-history."
  :type 'integer
  :group 'records  
  )

(defcustom records-output-buffer "*RECORDS-OUTPUT*"
  "* Contains the output of concatenating records."
  :type 'string
  :group 'records
  )

(defcustom records-subject-prefix-on-concat "--- "
  "* Prefix prepended to each subject on records concatenation. 
See \\[records-concatenate-records\], and \\[records-concatenate-record-files\]."
  :type 'string
  :group 'records
)

(defcustom records-subject-suffix-on-concat " ---"
  "* Suffix appended to each subject on records concatenation. 
See \\[records-concatenate-records\], and \\[records-concatenate-record-files\]."
  :type 'string
  :group 'records
)

(defcustom records-date-prefix-on-concat "* "
  "* Prefix prepended to each date on records concatenation. 
See \\[records-concatenate-records\], and \\[records-concatenate-record-files\]."
  :type 'string
  :group 'records
)

(defcustom records-date-suffix-on-concat ""
  "* Suffix appended to each date on records concatenation. 
See \\[records-concatenate-records\], and \\[records-concatenate-record-files\]."
  :type 'string
  :group 'records
)

(defcustom records-select-buffer-on-concat nil
  "* If non nil, the records-output-buffer is selected after records are
concatenated by \\[records-concatenate-records\].
If nil, the records-output-buffer is just displayed."
  :type 'boolean
  :group 'records
)

(defcustom records-erase-output-buffer nil
  "* If non nil, the records-output-buffer is erased, 
every time \\[records-concatenate-records\] is invoked.
If nil, the output is appended."
  :type 'boolean
  :group 'records
)

;; location of latex template for records.
(defcustom records-tex-directory 
  (concat records-directory "/tex/")
  "* Location of latex template for records. Create this directory yourself. 
It is not created by default."
  :type 'string
  :group 'records
  )

;; name of latex template
(defcustom records-tex-template-plain
  (concat records-tex-directory "records-templ.tex")
  "* Location of plain template for LaTeX'ing records. Make sure that
this file exists in the correct directory. It is not installed by default."
  :type 'string
  :group 'records
  )

(defcustom records-tex-temp-output-file
  (concat records-tex-directory "records-temp.tex")
  "* Output location for Latex runs."
  :type 'string
  :group 'records
  )

(defcustom records-latex-output-buffer "*RECORDS-LATEX-OUTPUT*"
  "* Contains the Latex output from concatenating records."
  :type 'string
  :group 'records
  )

(defcustom records-template-alist nil 
  "* List of templates associated with record subjects. 
Each value is a tempo template expression (not an already defined template, 
but the expression that would be passed to tempo-define-template).
This alist can be used to insert templates associated with each record
subject. See the function records-insert-template.

An example of this alist is
  ((\"Tennis\" . (\"Partner: \\nField no: \\nWho won: \\n\"))
   (\"Health\" . (\"Medication: \\nWeight: \\n\"))
   (\"Today\" . (\"Time: \" (current-time-string) \"\\n\"))

The third element inserts the current time when the Today record is inserted.
"
  :type '(repeat (cons :format "%v"
                       (choice :value "" (string :tag "Records Subject"))
                       (choice :value "" (sexp :tag "Records template"))))
  :group 'records
)

;;; records hooks
(defvar records-index-mode-hooks nil
  "* Hook functions that are run when the records index buffer is created.")

(defvar records-mode-hooks nil
  "* Hook functions that are run when any records file buffer is created.")

(defvar records-load-hooks nil
  "* Hook functions that are run when records mode is loaded.")

(defvar records-make-record-hook nil
  "* Hook functions that are run when a record is created. 
This hook can be used to create record templates.")

(if (boundp 'running-xemacs)
    ()
  (if (string-match "Lucid" (emacs-version))
      (setq running-xemacs t)
    (setq running-xemacs nil)
    ))

;;; Adding emacs compatibility code here, just so it is loaded first.
(if (not (fboundp 'first))
    (fset 'first 'car))

(if (not (fboundp 'second))
    (fset 'second 'cadr))

(provide 'records-vars)
