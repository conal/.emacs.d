;;; My version of \C-x - .  Helps you out if you really meant \C-x + .
;;; There is a bug somewhere that makes it look like you are at the beginning
;;; of the line when you are really not, after it says "Ok, I switched it".

(defun inverse-add-abbrev (table type arg)
  (let (name nameloc exp)
    (save-excursion
      (forward-word (- arg))
      (setq name (buffer-substring (point) (progn (forward-word 1)
						  (setq nameloc (point))))))
    (setq exp (read-string (format "%s expansion for \"%s\": "
				   type name)))
    (if (and (> (length name) (length exp))
	     (y-or-n-p (format "Do you really mean expand \"%s\" to \"%s\"?"
			       exp name)))
	(let ((tmp name))
	  (setq name exp)
	  (setq exp tmp)
	  (message "Ok, I switched it")))
    (if (or (not (abbrev-expansion name table))
	    (y-or-n-p (format "%s expands to \"%s\"; redefine? "
			      name (abbrev-expansion name table))))
	(progn
	  (define-abbrev table (downcase name) exp)
	  (save-excursion
	    (goto-char nameloc)
	    (expand-abbrev))))))

;;; Similarly for \C-x + when you meant \C-x - .

(defun add-abbrev (table type arg)
  (let ((exp (and (>= arg 0)
		  (buffer-substring
		   (point)
		   (if (= arg 0) (mark)
		     (save-excursion (forward-word (- arg)) (point))))))
	name)
    (setq name (read-string (format "%s abbrev for \"%s\": "
				    type exp)))
    (if (and (> (length name) (length exp))
	     (y-or-n-p (format "Do you really mean expand \"%s\" to \"%s\"?"
			       exp name)))
	(let ((tmp name))
	  (setq name exp)
	  (setq exp tmp)
	  (message "Ok, I switched it")))
    (if (or (null exp)
	    (not (abbrev-expansion name table))
	    (y-or-n-p (format "%s expands to \"%s\"; redefine? "
			      name (abbrev-expansion name table))))
	(define-abbrev table (downcase name) exp))))

;;; The "p" in (interactive...) was left out.


(defun expand-region-abbrevs (start end &optional noquery)
  "For abbrev occurrence in the region, offer to expand it.
The user is asked to type y or n for each occurrence.
A numeric argument means don't query; expand all abbrevs.
Calling from a program, arguments are START END &optional NOQUERY."
  (interactive "r
p")
  (save-excursion
    (goto-char (min start end))
    (let ((lim (- (point-max) (max start end))))
      (while (and (not (eobp))
		  (progn (forward-word 1)
			 (<= (point) (- (point-max) lim))))
	(let ((modp (buffer-modified-p)))
	  (if (expand-abbrev)
	      (progn
	       (set-buffer-modified-p modp)
	       (unexpand-abbrev)
	       (if (or noquery (y-or-n-p "Expand this? "))
		   (expand-abbrev)))))))))


(defun perhaps-expand-abbrev ()
  "Do expand-abbrev if abbrev-mode is true, but not if we're looking at a
letter.  This test should really use the same criterion as the normal automatic
abbreviation expansion mechanism."
  (interactive)
  (when (and abbrev-mode (not (looking-at "[a-zA-Z]")))
      (expand-abbrev)))
