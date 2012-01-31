;;; Stuff to maintain a string telling when you last have modified a file.
;;; Just have a string like "Last  Modified " (resettable) somewhere in the
;;; first 10 (resettable) lines of your file.  Whenever you write it out the
;;; date will automatically be updated there.

;;; All you have to do is add the following line to your .emacs file:
;;; (push-new 'fix-last-modified-string write-file-hooks)


;;; Make sure that this line is not within the first last-modified-line-max
;;; lines of this file.
(defvar last-modified-prefix "Last Modified:?"
  "*Regexp used by fix-last-modified-string to identify where the
date of last modification recorded in a file.")

(defvar last-modified-line-max 10
  "Maximum number of lines to look for last modification date.  Used by
fix-last-modified-line-max")

(defun insert-last-modified ()
  (interactive)
  (insert last-modified-prefix)
  (delete-backward-char 1)
  (insert " " (now-str) " by " (user-name)))

(defun now-str ()
  (format-time-string "%a %b %d, %Y %H:%M"))
  

(defun fix-last-modified-string ()
  "Find the date string following the value of last-modified-prefix within the
first last-modified-line-max lines, and replace it with the current date.  Then
return NIL.  Suitable for adding to write-file-hooks so that it is done
whenever such a file is saved."
  (save-excursion
    (goto-char (point-min))
    (forward-line last-modified-line-max)
    (let ((search-end (point))
	  (curr-point 0)
	  (eol 0)
	  (has-name nil))
      (goto-char (point-min))
      (let ((case-fold-search t))
	(if (re-search-forward last-modified-prefix search-end t)
	    (progn
	      (save-excursion
		(setq curr-point (point))
		(end-of-line)
		(setq eol (point))
		(goto-char curr-point)
		(setq has-name (search-forward " by " eol t)))
	      (skip-chars-forward " ")
	      ;; The following is equivalent to (kill-line), but it doesn't
	      ;; fill the yank buffer
	      (delete-char
	       (- (save-excursion
		    (end-of-line)
		    (point))
		  (point)))
	      (insert (now-str)
                      (if has-name (concat " by " (user-name)) ""))
              )))))
  nil)

(defun user-name ()
  (or user-full-name (user-original-login-name)))

(defvar user-full-name nil "name of user")

(provide 'last-modified)
