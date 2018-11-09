;;; haskell-skel.el --- skeleton for haskell source files
;; arch-tag: 73167147-2b73-42f6-a36f-0ab41dfc6603
;; Copyright (C) 2003, 2004 Jose A Ortega Ruiz

;; Author: Jose A Ortega Ruiz <jao@member.fsf.org>
;; Keywords: languages

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

;;

;;; Code:

(require 'my-mmm)
(require 'common-skel)

;;; Auxiliar
(defvar %jao-hsk-v1% "")
(defvar %jao-hsk-v2% "")

;; (defun jao-read-haskell-module ()
;;   (setq %jao-hsk-v1% (read-string
;;                       "Module prefix (empty for no module): "
;;                       %jao-hsk-v1%))
;;   (if (> (length %jao-hsk-v1%) 0)
;;       (concat "module " %jao-hsk-v1% "." (jao-basename)  " where\n\n") ""))

(defun jao-read-haskell-module ()
  (setq %jao-hsk-v1% (read-string
                      "Module prefix (empty for no prefix): "
                      %jao-hsk-v1%)))

(defun jao-haskell-module ()
  (concat %jao-hsk-v1%
          (if (> (length %jao-hsk-v1%) 0) "." "")
          (jao-basename)))

;; (defun jao-read-haskell-module ()
;;   (setq %jao-hsk-v1% (read-string
;;                       "Module prefix (empty for no prefix): "
;;                       %jao-hsk-v1%))
;;   (concat "module " 
;;           (if (> (length %jao-hsk-v1%) 0)
;;               (concat %jao-hsk-v1% ".")
;;             "")
;;           (jao-basename)  " where\n\n"))

(defvar jao-copyright-holder "<holder>")
(defvar jao-maintainer "<email address>")
(defvar jao-license "<license>") ;; "none" means no license line

;; (defun haskell-skel-tabula ()
;;   "Set up Haskell auto-insert for Tabula work."
;;   (interactive)
;;   (setq jao-copyright-holder "Tabula, Inc.")
;;   (setq jao-maintainer "conal@conal.net") ;; "conal@tabula.com"
;;   (setq jao-license "none"))

(defun haskell-skel-awake ()
  "Set up Haskell auto-insert for Awake work."
  (interactive)
  (setq jao-copyright-holder "Awake Networks.")
  (setq jao-maintainer "conal@awakenetworks.com")
  (setq jao-license "none"))

(defun haskell-skel-personal ()
  "Set up Haskell auto-insert for personal work."
  (interactive)
  (setq jao-copyright-holder "Conal Elliott")
  (setq jao-maintainer "conal@conal.net")
  (setq jao-license "BSD3"))

(haskell-skel-personal)
;; (haskell-skel-tabula)
;; (haskell-skel-awake)

;;; Skeletons
(define-skeleton jao-skel-haskell-file
  "Haskell hs file header"
  "Brief description: "
  '(jao-read-haskell-module)
  "-- {-# LANGUAGE #-}\n"
  "{-# OPTIONS_GHC -Wall #-}\n"
  "\n"
  "-- {-# OPTIONS_GHC -Wno-unused-imports #-} -- TEMP\n"
  "-- {-# OPTIONS_GHC -Wno-unused-binds   #-} -- TEMP\n"
  "\n"
  "----------------------------------------------------------------------\n"
  "-- |\n"
  "-- Module      :  " (jao-haskell-module) \n
  "-- Copyright   :  (c) " (format-time-string "%Y ") jao-copyright-holder "\n"
  (when (not (string-equal jao-license "none"))
    (concat "-- License     :  " jao-license "\n"))
  "--\n"
  "-- Maintainer  :  " jao-maintainer "\n"
  "-- Stability   :  experimental\n"
  "-- \n"
  "-- " str \n
  "----------------------------------------------------------------------\n\n"
  "module " (jao-haskell-module) " where\n\n"
  "-- TODO: explicit exports\n\n"
  _
  )


;; (define-skeleton jao-skel-lit-haskell-file
;;   "Haskell lhs file header"
;;   "Brief description: "
;;   "%% " (file-name-nondirectory (buffer-file-name)) " :  " str \n
;;   (jao-copyright-line "%% ")
;;   (jao-read-copyright-file "%% " "")
;;   "%% Created: " (format-time-string "%a %b %d, %Y %H:%M") \n
;;   ;; "$" "Id$"
;;   \n \n
;;   "\\begin{code}\n\n"
;;   (jao-read-haskell-module)
;;   _
;;   "\n\n\\end{code}\n"
;;   '(mmm-parse-buffer)
;;   )

(require 'last-modified)

;; I use "newline" rather than "\n" in case of longlines mode.

(define-skeleton jao-skel-latex-haskell-file
  "Haskell latex+lhs file header"
  "Brief description: "
  "%% " (file-name-nondirectory (buffer-file-name)) (newline) (newline)
  (jao-copyright-line "%% ") (newline)
  (jao-read-copyright-file "%% " "")
  "%% Created: " (format-time-string "%a %b %d, %Y %H:%M") (newline)
  "%% " (insert-last-modified) (newline)
  (newline) "\\section{" str "} \\label{sec:" 
  (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
  "}" (newline) (newline)
  "%if False" (newline)
  "\\begin{code}" (newline)
  (jao-read-haskell-module)
  _
  (newline) "\\end{code}" (newline) "%endif" (newline)
  '(mmm-parse-buffer)
  '(set-buffer-modified-p t) ; doesn't work.  why?
  )

(defun blog-fix-title (title)
  "Tweak TITLE for a blog post url"
  (replace-regexp-in-string "[^a-z0-9]+" "-" (downcase title)))

;;; Skeletons
(define-skeleton jao-skel-markdown-haskell-file
  "Haskell markdown+lhs file header. gitit-friendly."
  "Brief description: "
  '(jao-read-haskell-module)
  "---\n"
  "title: " str "\n"
  "tags: \n"
  "url: http://conal.net/blog/posts/" (blog-fix-title str)"/\n"
  "...\n\n"
  " <!--[ \n\n"
  "< {-# LANGUAGE #-}\n\n"
  "> {-# OPTIONS_GHC -Wall #-}\n\n"
  "< {-# OPTIONS_GHC -Wno-unused-imports #-} -- TEMP\n\n"
  "|\n"
  "Module      :  " (jao-haskell-module) "\n"
  "Copyright   :  (c) " (format-time-string "%Y ") jao-copyright-holder "\n"
  (when (not (string-equal jao-license "none"))
    (concat "-- License     :  " jao-license "\n"))
  "Maintainer  :  " jao-maintainer "\n"
  "Stability   :  experimental\n\n"
  str "\n\n"
  "> module " (jao-haskell-module) " where\n\n"
  " ]-->\n\n"
  " <!-- references -->\n"
  " <!-- -->\n\n"
  (markdown-mode)
  _
  )





;;; I don't know when to use \n vs "\n".  The former generates indentation
;;; errors sometimes, so I avoid them in the definition above.


(require 'autoinsert)

(add-to-list 'auto-insert-alist
             '("\\.hs\\'" . jao-skel-haskell-file))
(add-to-list 'auto-insert-alist
             ;; '("\\.lhs\\'" . jao-skel-latex-haskell-file)
             '("\\.lhs\\'" . jao-skel-markdown-haskell-file)
             )

(provide 'haskell-skel)

;;; haskell-skel.el ends here
