(defmacro cons-unique (mem list)
  "Add mem to list if it does not exist in it."
  (` (setq (, list) 
	   (if (member (, mem) (, list)) list (cons (, mem) (, list))))))

(eval-when-compile (cons-unique (expand-file-name ".") load-path))
(eval-when-compile (cons-unique (expand-file-name "./mailcrypt") load-path))
