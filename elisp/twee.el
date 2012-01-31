;; Requirements
;; ------------

;; You'll need to have an emacs that has generic mode support (all modern
;; ones should) as well as longlines.el installed. You can grab longlines
;; online at:
;;
;; http://www.emacswiki.org/cgi-bin/wiki/longlines.el
;; 
;; Update: use visual-line-mode instead of longlines mode.

;; Installation
;; ------------

;; Place both longlines.el and twee.el into your load path -- if you're
;; not sure, it's probably best to put it into your site-lisp
;; directory. (If you don't know where that is, try locate site-lisp.)
;; Then add these lines to your .emacs file:

;; (autoload 'twee-mode "twee.el" "Mode for editing Twee files." t)
;; (add-to-list 'auto-mode-alist '("\\.tw\\'" . twee-mode))
;; (add-hook 'twee-mode-hook 'turn-on-font-lock)

;; Conal also recommends the line:
;; 
;; (add-hook 'longlines-mode-hook 'twee-dont-fill-paragraph-hook)


;; That's it. If all's right in the world, you'll pop into Twee mode
;; whenever you start editing a file whose name ends in .tw. 

;; -- Chris Klimas
;; klimas@gmail.com


;; Change History
;; --------------

;; :: October 18, 2006 [ConalElliott]
;; * syntax highlighting regexps simplified, improved, and commented 
;; * new highlighting: WikiWord, !!Subheading, {{{monospace}}}, /%comment%/
;; * key-bindings for markup insertion
;; 
;; :: December 19, 2006 [ConalElliott]
;; * Tweaked WikiWord regexp: added word boundaries ("\<...\>")
;; :: December 26, 2006 [ConalElliott]
;; * paragraph-start and paragraph-separate
;; :: December 27, 2006 [ConalElliott]
;; * twee-add-item (\C-ci): add a numbered or bulletted item.
;; :: January 3, 2007 [ConalElliott]
;; * twee-add-sublist (\C-cI): add a new sublist with NestedSlider.
;; :: January 4, 2007 [ConalElliott]
;; * page-delimiter
;; * twee-no-longlines, and uses

;; :: January 10, 2007 [ConalElliott]
;; * twee-expand-abbrev and its uses in twee-add-item & twee-add-sublist
;; * changed page-delimiter to match tiddler titles.  Not really what I
;;   want, since mark-page marks the tiddler body and the //next// tiddler
;;   title.  I don't know if it's possible to use mark-page to mark a
;;   tiddler.
;; :: January 12, 2007 [ConalElliott]
;; * twee-dont-fill-paragraph-hook.  See add-hook suggestion above.

;; Code
;; ----

;; (require 'longlines)



;; Basic mode definition
(define-generic-mode 'twee-mode
  nil   
  nil 
  '(("^::.*"                           ; :: Title [tags]
     . font-lock-function-name-face)
    ("^!.*" . font-lock-keyword-face)                    ; !!Subheading
    ("\<[A-Z][a-zA-Z_]*[a-z_][A-Z][a-zA-Z_]*\>"          ; WikiWord
     . font-lock-function-name-face)
    ;; conal: the regular expressions below are problematic.  For
    ;; instance, the italics pattern is a pair of slashes, followed by
    ;; non-slashes, followed by another pair of slashes.  So a *single*
    ;; slash mistakenly ends the italic formatting.  I don't know how to
    ;; write a regexp to describe a sequence of characters without two
    ;; consecutive slashes.  Moreover, a "http://" will trigger italic
    ;; mode.  I guess we need some context-sensitivity to do highlighting
    ;; robustly.
    ;; To do: try a face with actual bold & italics for the bold & italic
    ;; markup.
    ("''[^']*''" . font-lock-keyword-face)               ; ''bold''
    ("\\[\\[[^]]*\\]\\]" . font-lock-function-name-face) ; [[tag]]
    ("//[^/]*//" . font-lock-keyword-face)               ; //italics//
    ("<<[^>]*>>" . font-lock-warning-face)               ; <<macro>>
    ("{{{[^}]*}}}" . font-lock-constant-face)            ; {{{monospace}}}
    ("/%[^%]*%/" . font-lock-comment-face)               ; /%comment%/
    )
  '("\\.tw\\'")
  (list 'twee-mode-setup-function)
  "Major mode for editing Twee files.")  

(defvar twee-mode-hook nil)

(defun twee-mode-setup-function ()
  ;; (longlines-mode)
  (visual-line-mode)
  ;; Key bindings for easy insertion of formatting
  (local-set-key "\C-c[" (twee-delim-command "[[" "]]"))
  (local-set-key "\C-c<" (twee-delim-command "<<" ">>"))
  (local-set-key "\C-c{" (twee-delim-command "{{{" "}}}"))
  (local-set-key "\C-c'" (twee-delim-command "''" "''"))
  (local-set-key "\C-c/" (twee-delim-command "//" "//"))
  (local-set-key "\C-ci" 'twee-add-item)
  (local-set-key "\C-cI" 'twee-add-sublist)
  (set (make-local-variable 'paragraph-start) (concat "^$\\|" page-delimiter))
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  ;; Use tiddler name lines for page delimiter.  Not quite what I really
  ;; want.  I'd love to have a command like mark-page that grabs the
  ;; tiddler I'm editing (without name), so I can cut & paste.
  (set (make-local-variable 'page-delimiter) "^::.*")
  (run-hooks 'twee-mode-hook) ;;  turn on font-lock, abbrev-mode, etc
  )

(defun twee-insert-delimiters (begin end)
  "Insert a pair of delimiter strings and place point between them."
  (insert (concat begin end))
  (backward-char (length end)))

;; Command version.
(defun twee-delim-command (begin end)
  `(lambda () (interactive)
     (twee-insert-delimiters ,begin ,end)))

(defmacro twee-no-longlines (&rest body)
  "No-op replacement for a macro that avoided problems with longlines.
Use visual-line-mode instead."
  `(progn ,@body))

;; (defmacro twee-no-longlines (&rest body)
;;   "Do body while not in longlines-mode.  Switches back even if there's an
;; error.  Motivation: so that inserting a newline (\"\\n\") would really
;; insert a newline.  WARNING: switching modes invalidates info from the last
;; match, so be sure to save (match-string) result if you need it.  See
;; twee-add-item."
;;   ;; Strangely, (longlines-mode 0) messes up returns when not already in
;;   ;; longlines mode.
;;   `(let ((was-ll longlines-mode))
;;      (unwind-protect
;;          (progn
;;            (when was-ll (longlines-mode 0))
;;            ,@body)
;;        (when was-ll (longlines-mode 1)))))

(defun twee-expand-abbrev ()
  "If in abbrev-mode, do expand-abbrev."
  (when (and (boundp 'abbrev-mode) abbrev-mode)
    (expand-abbrev)))

(defun twee-add-item ()
  "Add a numbered or bulletted item.  Repeats the most recent item marker."
  (interactive)
  (twee-expand-abbrev) ; in case we've just typed an abbrev
  ;; Copy initial sequence of asterisks or pound signs and final spaces.
  ;; Allow initial spaces for reuse in markdown-mode
  (save-excursion (re-search-backward "^ *\\(\\*+\\|#+\\) *"))
  (let ((s (match-string 0)))
    (twee-no-longlines
     (insert "\n" s)))
  )

(defun twee-add-sublist ()
  "Start a sub-list, with surrounding NestedSlider.  Only works in a list,
since it searches for the most recent list item marker."
  (interactive)
  (twee-expand-abbrev)
  (save-excursion (re-search-backward "^\\(\\*+\\|#+\\) *"))
  (let* ((match     (match-string 0))
         (match-len (length match))
         (spaces    (make-string match-len ? )))
    (twee-no-longlines
     (insert " ++++\n*" spaces "\n" spaces "=== ")
     ;; Done inserting.  Move to first item in sublist
     (previous-line 1)
     (end-of-line))))


(defun twee-dont-fill-paragraph-in-longlines ()
  "Conal has a \\M-q (`fill-paragraph') habit from many years without
longlines.  Maybe you do also.  Here's a wake-up call.  The usual reasons
apply for wanting to leave behind unconscious and unproductive behaviors.
Also, `fill-paragraph' collapses multiple spaces, which messes up the visual
indentation that `twee-add-item' and `twee-add-sublist' create.

If, like Conal, you're addicted to \\M-q, it's not too late for you, and
it's okay to ask for help.  See `twee-dont-fill-paragraph-hook'."
  (interactive)
  (if longlines-mode
      (progn
        (beep)
        (message "I'm guessing you don't really want to use `fill-paragraph' in visual-line-mode."))
    (fill-paragraph nil)))

(defun twee-dont-fill-paragraph-hook ()
  "Bind \\M-q to invoke `twee-dont-fill-paragraph-in-longlines' instead of
`fill-paragraph'.  I recommend adding this line to your .emacs.

 (add-hook 'longlines-mode-hook 'twee-dont-fill-paragraph-hook)"
  (local-set-key "\M-q" 'twee-dont-fill-paragraph-in-longlines))

(provide 'twee-mode)
(provide 'twee)
