;;;
;;; records-autoloads.el
;;;
;;; $Id: records-autoloads.el,v 1.1 2000/04/17 21:09:30 ashvin Exp $
;;;
;;; Copyright (C) 1996-2000 by Ashvin Goel
;;;
;;; This file is under the Gnu Public License.

;;; For maintainers:
;;; Run the function update-autoloads-from-directory to update this file.
;;; This function will update the file only when the *.el files are 
;;; newer than this file.

;;; Local Variables:
;;; generated-autoload-file:"records-autoloads.el"
;;; End:

;;;### (autoloads (records-index-mode) "records-index" "records/records-index.el")

(autoload 'records-index-mode "records-index" "\
Records-index-mode with mouse support.
Key bindings are:
\\{records-index-mode-map}" t nil)

;;;***

;;;### (autoloads (records-search-backward records-search-forward) "records-search" "records/records-search.el")

(autoload 'records-search-forward "records-search" "\
Search forward for regular expression in current subject." t nil)

(autoload 'records-search-backward "records-search" "\
Search backward for regular expression in current subject." t nil)

;;;***

;;;### (autoloads (records-concatenate-record-files-latex records-concatenate-records-latex records-widen-latex records-narrow-latex) "records-tex" "records/records-tex.el")

(autoload 'records-narrow-latex "records-tex" "\
Narrow records-file buffer to the current record and switch to latex-mode." t nil)

(autoload 'records-widen-latex "records-tex" "\
Widen records-file buffer and switch from latex to records-mode." t nil)

(autoload 'records-concatenate-records-latex "records-tex" "\
Concatenate the current record with the records on the same subject written
in the last NUM days and output in latex format. Then run latex on the output.
Output these records in the records latex output buffer (see 
records-latex-output-buffer). Without prefix arg, prompts for number of days.
An empty string will output the current record only. A negative number
will output all the past records on the subject!" t nil)

(autoload 'records-concatenate-record-files-latex "records-tex" nil nil nil)

;;;***

;;;### (autoloads (records-insert-link records-insert-record-buffer records-insert-record-region records-calendar-to-record records-goto-calendar records-concatenate-record-files records-concatenate-records records-decrypt-record records-encrypt-record records-get-todo records-create-todo) "records-util" "records/records-util.el")

(autoload 'records-create-todo "records-util" "\
Create a records todo entry in the current record" t nil)

(autoload 'records-get-todo "records-util" "\
Insert the previous record files todo's into the date file.
See the records-todo-.*day variables on when it is automatically invoked." t nil)

(autoload 'records-encrypt-record "records-util" "\
Encrypt the current record for the current user.
With prefix arg, start the encryption from point to the end of record.
Records encryption requires the mailcrypt and mc-pgp (or mc-pgp5) packages." t nil)

(autoload 'records-decrypt-record "records-util" "\
Decrypt the current record.
Records decryption requires the mailcrypt and mc-pgp (or mc-pgp5) packages." t nil)

(autoload 'records-concatenate-records "records-util" "\
Concatenate the current record with the records on the same subject written
in the last NUM days. Output these records in the records output buffer (see 
records-output-buffer). Without prefix arg, prompts for number of days.
An empty string will output the current record only. A negative number
will output all the past records on the subject! Normally, the records are
output in most-recent first order. This function asks the user if the order
should be reversed." t nil)

(autoload 'records-concatenate-record-files "records-util" "\
Concatenate all the records in the records files of the last NUM days. All
the records of a subject are collected together. Output these records in the
records output buffer (see records-output-buffer). Without prefix arg, prompts
for number of days. An empty string will output the records of the current
file." t nil)

(autoload 'records-goto-calendar "records-util" "\
Goto the calendar date in the current record file." t nil)

(autoload 'records-calendar-to-record "records-util" "\
Goto the record file corresponding to the calendar date." t nil)

(autoload 'records-insert-record-region "records-util" "\
Insert the region in the current buffer into today's record.
Prompts for subject." t nil)

(autoload 'records-insert-record-buffer "records-util" "\
Insert the current buffer into today's record.
Prompts for subject." t nil)

(autoload 'records-insert-link "records-util" "\
Writes the current buffer file name, url or message id
at the end of today's record and inserts a comment." t nil)

;;;***

;;;### (autoloads (homepage-records-to-html batch-generate-homepage) "records-w3" "records/records-w3.el")

(autoload 'batch-generate-homepage "records-w3" nil nil nil)

(autoload 'homepage-records-to-html "records-w3" nil t nil)

;;;***

;;;### (autoloads (records-goto-today records-goto-index records-underline-line records-initialize) "records" "records/records.el")

(autoload 'records-initialize "records" "\
Reads the records init file and sets the records internal variables
like records-date, records-date-length, etc." t nil)

(autoload 'records-underline-line "records" "\
Underline the current line to the length of the line." t nil)

(autoload 'records-goto-index "records" "\
If arg is nil or zero, goto the index on date and tag.
With positive arg, go to the index arg-times next to date and tag.
With negative arg, go to the index arg-times previous to date and tag.
Returns the new (date, tag) if found." t nil)

(autoload 'records-goto-today "records" "\
Go to the records file of today." t nil)

;;;***

(provide 'records-autoloads)
