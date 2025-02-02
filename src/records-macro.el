;;; records-macro.el --- macros for records

;; Copyright (C) 2009  Xavier Maillard

;; Author: Xavier Maillard <xma@gnu.org>
;; Keywords: maint, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; In order to take advantage of this file, use this form:
;;
;; (eval-when-compile (require 'records-macro))

;;; Code:

(defmacro records-index-subject-regexp (&optional subject)
  "Regexp matching a subject in the records index file."
  `(if ,subject
       (concat "^\\(" ,subject "\\): ")
     "^\\(.*\\): "))

(defmacro records-date-count-regexp (&optional date)
  "Regexp matching a date in the records date-index file."
  `(if ,date
       (concat "\\(" ,date "\\)#\\([0-9]+\\) ")
     (concat records-date-regexp "#\\([0-9]+\\) ")))

(defmacro records-subject-regexp (&optional subject)
  "Regexp matching the beginning of a record."
  ;; TODO: the underline should be of length(subject) + 2
  ;; not easy to do when subject is nil
  `(if ,subject
       (concat "^\\* \\(" ,subject "\\)\n\\-\\-\\-+$")
     ;; "^\\* \\(.*\\)\n\\-+$"
     "^\\* \\(.*\\)\n\\-\\-\\-+$"
     ))

(defmacro records-subject-on-concat (subject)
  "Make subject for records concatenation."
  `(let ((sub (concat records-subject-prefix-on-concat ,subject
                      records-subject-suffix-on-concat)))
     (concat sub "\n" (make-string (length sub) ?-) "\n")))

(defmacro records-date-on-concat (date)
  "Make date for records concatenation."
  `(let ((d (concat records-date-prefix-on-concat ,date
                    records-date-suffix-on-concat)))
     (concat d "\n" (make-string (length d) ?-) "\n")))

(defmacro records-tag (tag)
  `(if (> (length ,tag) 0) (concat "#" ,tag) ""))

(provide 'records-macro)
;;; records-macro.el ends here
