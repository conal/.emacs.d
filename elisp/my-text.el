;;; Text-mode enhancements

(defun match-angle ()
  "Make the syntax table treat < and > as matching delimiters"
  (interactive)
  (modify-syntax-entry ?\< "(>  ")      ; Make matchers
  (modify-syntax-entry ?\> ")<  "))

(defun no-match-angle ()
  "Make the syntax table not treat < and > as matching delimiters"
  (interactive)
  (modify-syntax-entry ?\< "_")         ; Make symbols
  (modify-syntax-entry ?\> "_"))

(defun continue-item ()
  "Copy indentation, a non-white-space group, and the following white-space
from the current line onto the next one."
  (interactive)
  (perhaps-expand-abbrev)
  (beginning-of-line nil)
  (let ((beg-point (point)))
    (back-to-indentation)
    (skip-chars-forward "^ \C-i")
    (skip-chars-forward " \C-i")
    (let ((init-str (buffer-substring (point) beg-point)))
      (end-of-line nil)
      (insert "\C-j" init-str))))

;;(global-set-key "\C-cc" 'continue-item) ; from my-text.el
;;(global-set-key "\e{" 'TeX-insert-braces)

(defconst indented-marker-regexp "\\(([a-zA-Z0-9])\\|[-o+]\\|<[a-z]+>\\|\\([0-9]+\\|[a-zA-Z]\\)\\.\\)[ \t]+"
  "* Regexp for detecting paragraph markers.  Default is some whitespace
preceded by either (a) a parenthesized letter, (b) -,o, or + ; (c) a number
and '.'; (d) a single letter and '.'")

;;; See my-text-adaptive-fill-function
(defconst indentation-regexp-with-marker
  (concat "[ \t]*\\("
          indented-marker-regexp
          "\\)?"))

(defconst lots-of-spaces
  (let ((some "                                         "))
    (concat some some some some)))

;;; Used in text modes
(defun my-text-adaptive-fill-function ()
  (let ((ind-column
         (if (looking-at indentation-regexp-with-marker)
             (progn (goto-char (match-end 0))
                    (current-column))
           0)))
    (substring lots-of-spaces 0 ind-column)))

;;; These tweaks need to be applied to the new version of
;;; fill-paragraph.  For now, unused.

(defun my-indented-fill-paragraph ()
  "My version of fill-paragraph for indented-text-mode.  Fill the current 
paragraph to have the same indentation as the current line.  Recognizes
paragraph markers, which are anti-indented strings of the form -,o, or +,
followed by a single space."
  (interactive)
  (perhaps-expand-abbrev)
  (if fill-prefix
      (fill-paragraph nil)
      (save-excursion
        (back-to-indentation)
        (if (looking-at "^$")   ; Skip empty lines
            (progn (skip-chars-forward "\C-j")
                   (back-to-indentation)))
        (if (looking-at indented-marker-regexp) ; Skip possible marker
            (goto-char (match-end 0)))
        (let ((fill-prefix "")
              (ind-column (current-column)) ; Set indentation
              (par-marker "")   ; Records deleted paragraph marker
              (par-marker-leng 0)) ; number of columns taken up.  Might
                                ; not be (length par-marker) because of
                                ; tabs.
          (mark-paragraph)
          ;; Delete marker, if there, before filling
          (next-line 1)
          (back-to-indentation)
          (if (looking-at indented-marker-regexp)
              (let ((bol-col (current-column))
                    (bol-char (point)))
                (goto-char (match-end 0))
                (setq par-marker (buffer-substring bol-char (match-end 0)))
                (setq par-marker-leng (- (current-column) bol-col))
                (backward-delete-char (- (point) bol-char))))
          (previous-line 1)
          ;; Get the whole paragraph lined up right
          (indent-rigidly (point) (mark) -100)
          (indent-rigidly (point) (mark) ind-column)
          ;; Now really fill it
          (next-line 1)
          (back-to-indentation)
          (set-fill-prefix)
          (fill-paragraph nil)
          ;; Replace the paragraph marker
          (back-to-indentation)
          (backward-delete-char-untabify par-marker-leng)
          (insert par-marker)
          ))))

(defconst before-marker-regexp "\C-j\C-j[ \t]*"
  "For text-insert-item")

(defun current-marker ()
  "Get the current marker"
  (save-excursion
    (re-search-backward (concat before-marker-regexp indented-marker-regexp))
    (buffer-substring (point) (match-end 0))))

(defun text-insert-item ()
  "Begin another marked paragraph.  Automagically changes (a) to (b), (2) to
 (3) etc."
  (interactive)
  (perhaps-expand-abbrev)
  ;;(delete-horizontal-space)
  (insert (current-marker))
  (increment-par-marker))

(defun increment-par-marker ()
  "Increment the marker at the beginning of the current line, i.e. change
a to b, 1 to 2, etc."
  (interactive)
  (save-excursion
    (back-to-indentation)
    (skip-chars-forward "(")
    (when (looking-at "\\([a-zA-Z]\\|[0-9]+\\)\\(\\.\\|)\\)")
      (goto-char (- (match-end 1) 1))
      (let ((new-char (+ 1 (char-after (point)))))
        (delete-char 1)
        ;; Special hack to make 9 roll over into 10.
        (insert (if (= new-char (+ ?9 1))
                    "10"
                    new-char))))))

(defun to-next-marker ()
  "Go to the next marked paragraph at the same level of enumeration."
  (interactive)
  (beginning-of-line 1)
  (let ((bol (point)))
    (back-to-indentation)
    (let ((white-space (buffer-substring bol (point))))
      (forward-line 1)
      (re-search-forward (concat "^" white-space indented-marker-regexp)))))

(defun fix-next-marker (count)
  "Fix the next COUNT (prefix arg) markers at this level of enumeration.
Useful after throwing off the numbering by deleting or inserting a marker."
  (interactive "p")
  (while (> count 0)
    (let ((cur-marker (current-marker)))
      (to-next-marker)
      (previous-line 2)
      (end-of-line)
      (insert cur-marker))
    (increment-par-marker)
    (let ((opoint (point)))
      (re-search-forward (concat before-marker-regexp indented-marker-regexp))
      (delete-region opoint (match-end 0)))
    (setq count (- count 1))))
        

(define-key text-mode-map "\C-ci" 'text-insert-item)
(define-key text-mode-map "\C-cI" 'fix-next-marker)

;;; Not used
(defun my-indent-relative (&optional unindented-ok)
  "Space out to under next indent point in previous nonblank line.
An indent point is a non-whitespace character following whitespace.
If the previous nonblank line has no indent points beyond
the column point starts at,  tab-to-tab-stop  is done instead.	This hacked
version also looks for a paragraph marker"
  (interactive "P")
  (if abbrev-mode (expand-abbrev))
  (let ((start-column (current-column))
	indent)
    (save-excursion
      (beginning-of-line)
      (if (re-search-backward "^[^\n]" nil t)
	  (let ((end (save-excursion (forward-line 1) (point))))
	    (move-to-column start-column)
	    ;; Is start-column inside a tab on this line?
	    (if (> (current-column) start-column)
		(backward-char 1))
	    (or (looking-at "[ \t]")
		unindented-ok
		(skip-chars-forward "^ \t" end))
	    (skip-chars-forward " \t" end)
	    (if (looking-at indented-marker-regexp) ; Skip possible marker
		(goto-char (match-end 0)))
	    (or (= (point) end) (setq indent (current-column))))))
    (if indent
	(let ((opoint (point-marker)))
	  (delete-region (point) (progn (skip-chars-backward " \t") (point)))
	  (indent-to indent 0)
	  (if (> opoint (point))
	      (goto-char opoint))
	  (move-marker opoint nil))
      (tab-to-tab-stop))))

(defvar my-up-list-regexp "]\\|)\\|}\\|\"\\|\\$")

(defun my-up-list ()
  "Very simple version of up-list.  First does expand-abbrev and then moves to
right after next closing delimiter (see variable my-up-list-regexp)"
  (interactive)
  (perhaps-expand-abbrev)
  (re-search-forward my-up-list-regexp))

(global-set-key "\C-c}" 'my-up-list)

(defun my-newline-and-indent ()
  "My alternative to newline-and-indent. expand-abbrev; newline; indent-for-tab-command."
  (interactive)
  (perhaps-expand-abbrev)
  ;; (newline-and-indent)
  (newline)
  (indent-for-tab-command)
  )

(global-set-key "\C-j" 'my-newline-and-indent)


(defun surround-sexp (count before &optional after)
  "Insert a BEFORE and AFTER around previous N sexp's, where N is the
prefix arg.  But if N is negative, instead starify the (-N)th previous sexp.
If AFTER is missing, it defaults to BEFORE."
  (expand-abbrev)
  (let ((negflag (< count 0))
        ;; After inserting, if at end of word, advance past before
        (after (or after before))
        (then-forward (if (looking-at "\\b")
                          (if (stringp after)
                              (length after)
                            1)
                        0)))
    (if negflag (setq count (- 0 count)))
    (save-excursion
      (backward-sexp count)
      (insert before)
      (forward-sexp (if negflag 1 count))
      (insert after))
    (forward-char then-forward)
    ;; (unless negflag (insert " ")) ; experiment
    ))

;;; For text, *foo* looks good, but Word autoformats *foo* to bold and
;;; _foo_ to italics

(defun starify-sexp (n)
  "Insert a pair of asterisks around previous N sexp's, where N is the
prefix arg.  But if N is negative, instead starify the (-N)th previous sexp."
  (interactive "p")
  (surround-sexp n ?\*))

(defun underscore-sexp (n)
  "Insert a pair of underscores around previous N sexp's, where N is the
prefix arg.  But if N is negative, instead starify the (-N)th previous sexp."
  (interactive "p")
  (surround-sexp n ?\_))

(defun quotify-sexp (n)
  "Insert a pair of back-ticks around previous N sexp's, where N is the
prefix arg.  But if N is negative, instead quotify the (-N)th previous
sexp.  Useful for markdown."
  (interactive "p")
  (surround-sexp n ?\"))

(defun backtickify-sexp (n)
  "Insert a pair of back-ticks around previous N sexp's, where N is the
prefix arg.  But if N is negative, instead quotify the (-N)th previous
sexp.  Useful for markdown."
  (interactive "p")
  (surround-sexp n ?\`))

(defun pipify-sexp (n)
  "Insert a pair of |'s around previous N sexp's, where N is the
prefix arg.  But if N is negative, instead quotify the (-N)th previous sexp."
  (interactive "p")
  (surround-sexp n ?\|)
  (mmm-parse-block 1)
  )

(global-set-key "\C-c*" 'starify-sexp)
(global-set-key [?\C-*] 'starify-sexp)
(global-set-key "\C-c_" 'underscore-sexp)
(global-set-key "\C-c\"" 'quotify-sexp)
(global-set-key [?\C-\"] 'quotify-sexp) ; "
(global-set-key "\C-c\`" 'backtickify-sexp)
(global-set-key "\C-c\|" 'pipify-sexp)
(global-set-key [?\C-|]  'pipify-sexp)


(defun insert-angles ()
  "Insert a pair of angle brackets and place point between them."
  (interactive)
  (insert "<>")
  (backward-char 1))

(global-set-key "\C-c<" 'insert-angles)

(defun insert-square-brackets ()
  "Insert a pair of square brackets and place point between them."
  (interactive)
  (insert "[]")
  (backward-char 1))

(global-set-key "\C-c[" 'insert-square-brackets)

(defun insert-quotes ()
  "Insert a pair of double quotes and place point between them."
  (interactive)
  (insert "\"\"")
  (backward-char 1))

(global-set-key "\e\"" 'insert-quotes)

(global-set-key "\C-?" 'backward-delete-char-untabify)


(defun insert-braces ()
  "Insert a pair of curly braces and place point between them."
  (interactive)
  (insert "{}")
  (backward-char 1))

(global-set-key (kbd "C-M-{") 'insert-braces)
(global-set-key (kbd "C-{")   'insert-braces)

(defun abbrev-expand-eol ()
  "Like end-of-line, but first do perhaps-expand-abbrev"
  (interactive)
  (perhaps-expand-abbrev)
  (end-of-line))

(global-set-key "\C-e" 'abbrev-expand-eol)

(defun abbrev-expand-end-of-visual-line ()
  "Like end-of-visual-line, but first do perhaps-expand-abbrev"
  (interactive)
  (perhaps-expand-abbrev)
  (end-of-visual-line))

;; (define-key visual-line-mode-map [remap move-end-of-line] 'abbrev-expand-end-of-visual-line)
(define-key visual-line-mode-map [remap abbrev-expand-eol] 'abbrev-expand-end-of-visual-line)


;;; "[.?!][]\"')}]*\\($\\|\t\\|  \\)[ \t\n]*"
;;; Took out one space, because some folks won't put in two!!
(setq sentence-end "[.?!][]\"')}]*\\($\\|\t\\| \\)[ \t\n]*")

(defun markdown-mmmify-lines (&optional whole-buffer)
  "mmm-ify 32 lines above & below the point, or the whole buffer if argument given"
  (interactive "P")
  ;; I was doing *either* buffer or block, but oddly, buffer leaves junk behind,
  ;; in the area of the block.
  (when whole-buffer (mmm-parse-buffer))
  (save-excursion (mmm-parse-block 32)))  ; save-excursion needed in cocoa emacs
]
;;; Handy everywhere
(global-set-key [?\C-,] 'markdown-mmmify-lines)

(defun current-line-as-region ()
  (save-excursion
    (beginning-of-line)
    (let ((bol (point)))
      (end-of-line)
      (list bol (point)))))

(defun current-region ()
  (list (region-beginning) (region-end)))

(defun current-region-or-line ()
  (if (and mark-active transient-mark-mode)
      (current-region)
    (current-line-as-region)))

;; (defun indent-line-by (n)
;;   (save-excursion
;;     (beginning-of-line)
;;     (let ((bol (point)))
;;       (end-of-line)
;;       (indent-rigidly bol (point) n))))

(defun indent-region-or-line-by (n)
  "Indent the line or region (if mark active) by the given amount."
  (interactive "p")
  (destructuring-bind (start end) (current-region-or-line)
    (let ((deactivate-mark nil))
      (indent-rigidly start end n))))

(defun indent-four-forward (&optional n)
  "Indent the line or region (if mark active) by four or prefix argument."
  (interactive "P")
  (indent-region-or-line-by (or n 4)))

(defun indent-four-backward (&optional n)
  "De-indent the line or region (if mark active) by four or prefix argument."
  (interactive "P")
  (indent-region-or-line-by (- (or n 4))))

(global-set-key [C-tab]   'indent-four-forward)
(global-set-key [backtab] 'indent-four-backward)
(global-set-key [S-tab]   'indent-four-backward)

;;; On my Mac shift-tab gives [S-tab], but on Linux it gives [backtab]

;; (defun add-starred-item ()
;;   (interactive)
;;   (insert "\n*   "))

;; TODO: Have add-starred-item match the indentation of the previous starred item.

(defun line-starts-with-regep (regexp)
  "Does the current line start with the given REGEXP?"
  (save-excursion
    (beginning-of-line)
    (looking-at regexp)))

(defun add-starred-item ()
  "Add a numbered or bulletted item.  Repeats the most recent item marker."
  (interactive)
  (perhaps-expand-abbrev)             ; in case we've just typed an abbrev
  ;; Copy initial sequence of asterisks or pound signs and final spaces.
  ;; Allow initial spaces for reuse in markdown-mode
  (let ((on-bulleted-line (save-excursion 
                            (beginning-of-line)
                            (looking-at "[ *]"))))
    (if (and on-bulleted-line
             (save-excursion (re-search-backward "^ *\\(\\*\\) *" nil t)))
        (insert "\n"
                ;; Four more spaces if immediatly after colon.
                ;; TODO: match number of spaces
                (if (eq (preceding-char) ?:) "    " "")
                (match-string 0))
      ;; If we're on a plain line or no bullet found, start at outermost
      ;; bullet level.
      (unless (save-excursion (beginning-of-line) (looking-at "$"))
        (newline))
      (insert "\n*   "))
    ))

(provide 'my-text)
