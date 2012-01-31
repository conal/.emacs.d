;; common definitions and functions
;; Copyright (C) 2004 Jose Antonio Ortega Ruiz

;; Author: Jose A Ortega Ruiz <jao@gnu.org>
;; Keywords: tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Aux functions used in other skeletons

;;; Code:

(require 'skeleton)

(defvar jao-company-name nil
  "Company name used in copyright notice")

(defvar jao-copyright-file nil
  "Path to the raw (uncommented) copyright file")

(defun jao-copyright-line (prefix &optional suffix)
  "Create a brief copyright notice with given PREFIX and SUFFIX"
  (concat prefix "Copyright (c) "
    (format-time-string "%Y")
    " by "
    (or jao-company-name (user-full-name))
    suffix))

(defun jao-date-line (prefix &optional suffix)
  "Create a start date line"
  (concat prefix
          "Start date: "
          (format-time-string "%a %b %d, %Y %H:%M")
          suffix))

(defun jao-svn-line (prefix &optional suffix)
  "Create a SVN ID line"
  (concat prefix
          "X-SVN: $" "Id$"
          suffix))

(defun jao-arch-line (&optional prefix suffix)
  "Create an arch-tag line"
  (let* ((prefix (or prefix (concat comment-start " ")))
         (suffix (or suffix
                     (and (> (length comment-end) 0) comment-end)
                     prefix)))
    (concat prefix
            "arch-tag: "
            (user-full-name)
            (format-time-string " %a %b %d %Y %H:%M:%S")
            " (" (file-name-nondirectory (buffer-file-name)) ")\n"
            (or suffix prefix))))

(defun jao-insert-arch-line ()
  (interactive)
  (insert (jao-arch-line)))

(defun jao-c&co-line (prefix &optional suffix)
  (let* ((none (lambda (p f) ""))
         (formats '(("arch" . jao-arch-line)
                    ("svn" . jao-svn-line)
                    ("none" . none)))
         (names (mapcar 'car formats))
         (prompt (concat "SCM (" (mapconcat 'identity names ", ") "): "))
         (sel (completing-read prompt formats nil 1))
         (fun (cdr (assoc sel formats))))
  (concat (funcall fun prefix suffix) "\n\n"
          (jao-copyright-line prefix suffix)
          "\n"
          (if (not (eq fun 'jao-arch-line))
              (jao-date-line prefix suffix)))))

;; aux functions ---------------------------------------------------------
(defun jao-basename ()
  "Get buffer file name without dir nor extension"
  (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))

(defun jao-basedir ()
  "Base directory"
  (file-name-nondirectory
   (substring (file-name-directory (buffer-file-name)) 0 -1)))

(defun jao-dir-level (l)
  (let ((elems (split-string
                (file-name-sans-extension (buffer-file-name)) "/")))
    (mapconcat 'identity (nthcdr (- (length elems) (+ 1 l)) elems) "/")))

(defun jao-extension ()
  "Find the extension of the currently visited file"
  (let ((elems (split-string (file-name-nondirectory (buffer-file-name))
           "\\.")))
    (nth (- (length elems) 1) elems)))


(defun jao-other-file-name (ext1 ext2)
  "Find the complimentary file name of header/source file"
  (let ((extension (jao-extension))
  (basename (jao-basename)))
    (if (string= extension ext1) (concat basename "." ext2)
      (concat basename "." ext1))))


(defun jao-read-commented-file (file-name prefix &optional suffix)
  "Read the given file appending PREFIX to each line"
  (let ((buff (find-file-noselect (expand-file-name file-name)))
	(result (if (null suffix) (concat prefix suffix "\n") "\n")))
    (if (> (buffer-size buff) 0)
	(save-current-buffer
	  (set-buffer buff)
	  (goto-char (point-min))
	  (while (< (point) (point-max))
	    (if (re-search-forward "^.*" nil t)
		(let ((match (match-string 0)))
		  (if (or (split-string match) (null suffix))
		      (setq result (concat result
					   prefix
					   match
					   suffix "\n"))
		    (setq result (concat result "\n"))))))))
    (kill-buffer buff)
    result))

(defun jao-read-copyright-file (prefix &optional suffix)
  "Read the predefined copyright file, commented with PREFIX and SUFFIX"
  (jao-read-commented-file jao-copyright-file prefix suffix))

(provide 'common-skel)

; arch-tag: Jose Antonio Ortega Ruiz Sat Feb 21 2004 14:52:38 (common-skel.el)
;