;;; Misc. TeX-related customizations

;; (require 'longlines)

(require 'my-text)

(defvar tex-mode-abbrev-table nil
  "Abbrev table used while in tex mode.")
(define-abbrev-table 'tex-mode-abbrev-table ())

(defun my-tex-insert-quote (count)
  "Just like tex-insert-quote, but calls expand-abbrev first if ending quote"
  (interactive "P")
  (if count
      (self-insert-command count)
      (progn
        (expand-abbrev)
        (insert
         (cond
          ((or (bobp)
               (save-excursion
                 (forward-char -1)
                 (looking-at "[ \t\n]\\|\\s(")))
           "``")
          ((= (preceding-char) ?\\)
           ?\")
          (t "''"))))))

(defun tex-math-display ()
  "Insert \" \[  \]\" and set point inside the brackets"
  (interactive)
  (insert " \\[  \\]")
  (backward-char 3))

(defun tex-math-inline ()
  "Insert \"$$\" and set point between the $'s"
  (interactive)
  (insert "$$")
  (backward-char 1))

(defvar tex-generic-section-newpage t
  "See tex-generic-section")

(defun tex-generic-section (section)
  "Insert \"\\xxx{}\" and set point between the braces.  If value of
tex-generic-section-newpage is true (the default), start with a ^L."
  (interactive)
  (if tex-generic-section-newpage
      (insert "\C-l"))
 (insert "\\" section "{}")
  (backward-char 1)
  (expand-abbrev))

(defun tex-mysection ()
  "Insert \"^L\\mysection{}\" and set point between the braces"
  (interactive)
  (tex-generic-section "mysection"))

(defun tex-subsection ()
  "Insert \"^L\\subsection{}\" and set point between the braces"
  (interactive)
  (tex-generic-section "mysubsection"))

(defun tex-subsubsection ()
  "Insert \"^L\\subsubsection{}\" and set point between the braces"
  (interactive)
  (tex-generic-section "mysubsubsection"))

(defvar default-tex-declaration-name "em"
  "*The default declaration name for tex-declaration")

(defun tex-declaration (decl)
  "Insert a declaration"
  (interactive (let ((decl (read-string
                            (concat "Name of declaration ("
                                    default-tex-declaration-name "): "))))
                 (list (if (string-equal decl "")
                           default-tex-declaration-name
                           decl))))
  (insert "{\\" decl " }")
  (backward-char 1)
  (setq default-tex-declaration-name decl))

(defun tex-specific-declaration (decl)
  "Insert a specific \"{\\DECL }\" without changing default-tex-declaration"
  (let ((default-tex-declaration-name)) ; Preserve
    (tex-declaration decl)))


(defvar default-tex-environment-name "bloogle"          ; default?
  "*The default environment name for tex-environment")

(defun tex-environment (env)
  "Insert an environment: \\begin{ENV} ... \\end{ENV}"
  (interactive (let ((env (read-string
                           (concat "Name of environment ("
                                   default-tex-environment-name "): "))))
                 (list (if (string-equal env "")
                           default-tex-environment-name
                         env))))
  (unless (bolp) (newline))
  (insert "\\begin{" env "}")(newline)(newline)
  (insert "\\end{" env "}")(newline)
  (previous-line 2)
  (setq default-tex-environment-name env))

(defun tex-specific-environment (env)
  "Insert a specific environment changing default-tex-environment-name"
  (let ((default-tex-environment-name)) ; Preserve
    (tex-environment env)))

(defun tex-itemize ()
  "Insert an itemize environment"
  (interactive)
  (tex-specific-environment "itemize")
  (latex-insert-item))

(defun tex-em ()
  "Insert \"{\\em }\""
  (interactive)
  (tex-specific-declaration "em"))

(defun tex-emph ()
  "Insert \"\\emph{}\""
  (interactive)
  (insert "\\emph{}")
  (backward-char 1))

(defun tex-emph-sexp (n)
  "Wrap \"\\emph{}\" around the previous N sexps, defaulting to 1, where N is the
prefix arg.  But if N is negative, instead starify the (-N)th previous sexp."
  (interactive "p")
  (surround-sexp n "\\emph{" "}"))

(defun tex-bold-sexp (n)
  "Wrap \"\\textbf{}\" around the previous N sexps, defaulting to 1, where N is the
prefix arg.  But if N is negative, instead starify the (-N)th previous sexp."
  (interactive "p")
  (surround-sexp n "\\textbf{" "}"))

(defun tex-math-sexp (n)
  "Wrap \"$$\" around the previous N sexps, defaulting to 1, where N is the
prefix arg.  But if N is negative, instead starify the (-N)th previous sexp."
  (interactive "p")
  (surround-sexp n "$" "$"))

(defun tex-AB-sexp (n)
  "Wrap \"\\AB{}\" (for Agda function) around the previous N sexps, defaulting to 1, where N is the
prefix arg.  But if N is negative, instead starify the (-N)th previous sexp."
  (interactive "p")
  (surround-sexp n "\\AB{" "}"))

(defun tex-AF-sexp (n)
  "Wrap \"\\AF{}\" (for Agda function) around the previous N sexps, defaulting to 1, where N is the
prefix arg.  But if N is negative, instead starify the (-N)th previous sexp."
  (interactive "p")
  (surround-sexp n "\\AF{" "}"))
 
(defun tex-AD-sexp (n)
  "Wrap \"\\AD{}\" (for Agda function) around the previous N sexps, defaulting to 1, where N is the
prefix arg.  But if N is negative, instead starify the (-N)th previous sexp."
  (interactive "p")
  (surround-sexp n "\\AD{" "}"))

(defun tex-AIC-sexp (n)
  "Wrap \"\\AIC{}\" (for Agda function) around the previous N sexps, defaulting to 1, where N is the
prefix arg.  But if N is negative, instead starify the (-N)th previous sexp."
  (interactive "p")
  (surround-sexp n "\\AIC{" "}"))

(defun tex-tt ()
  "Insert \"{\\tt }\""
  (interactive)
  (tex-specific-declaration "tt"))

(defun tex-texttt ()
  "Insert \"\\texttt{}\""
  (interactive)
  (insert "\\texttt{}")
  (backward-char 1))

(defun tex-hs ()
  "Insert \"\\hs{}\" for Mark Jones's HaskTeX"
  (interactive)
  (insert "\\hs{}")
  (backward-char 1))

(defun tex-verb ()
  "Insert \"\\verb||\""
  (interactive)
  (insert "\\verb||")
  (backward-char 1))

(defun tex-insert-item ()
  "Insert \"\\C-j\\\\C-j\\item \" to finish one item and stop another"
  ;; Doesn't quite work.  It should get the previously indented \item.
  (interactive)
  (expand-abbrev)
  ;; (newline)
  ;; (my-tex-newline-and-indent)
  ;; (insert "\\item ")
  (newline 2)
  (insert "  \\item ")
  )

(defun tex-footnote ()
  "Insert \"\\footnote{}"
  (interactive)
  (insert "\\footnote{}")
  (backward-char 1))

(defun tex-notefoot ()
  "Insert \"\\notefoot{}"
  (interactive)
  (insert "\\notefoot{}")
  (backward-char 1))

(defun tex-note ()
  "Insert \"\\note{}"
  (interactive)
  (insert "\\note{}")
  (backward-char 1))


(defun tex-rtn-shell ()
  "Send a \"\\n\" to the tex shell to continue after an error"
  (interactive)
  (send-string tex-shell-process "\n"))

(defun tex-psl-shell ()
  "Send a \"psl name\\n\" to the tex shell"
  (interactive)
  (save-some-buffers t)
  (visiblize-tex-shell)
  (send-string tex-shell-process
               (concat (tex-cd-string)
                       "psl "
                       (chop-tex (file-name-nondirectory buffer-file-name))
                       "\n")))

(defun tex-y-shell ()
  "Send a \"y\\n\" to the tex shell"
  (interactive)
  (send-string tex-shell-process "y\n"))

(defun tex-n-shell ()
  "Send a \"n\\n\" to the tex shell"
  (interactive)
  (send-string tex-shell-process "n\n"))

(defun my-tex-newline-and-indent ()
  "Insert a newline, then do indent-relative-maybe."
  ;; Modified from newline-and-indent in simple.el.  I don't want to set the
  ;; variable indent-line-function to 'indent-relative-maybe, because the
  ;; auto-filling uses it.  Takes indentation from the most recent indented
  ;; line, not necessarily the previous one.  First, do expand-abbrev
  (interactive)
  (expand-abbrev)
  (delete-region (point) (progn (skip-chars-backward " \t") (point)))
  (insert ?\n (previous-indentation)))

(defun previous-indentation ()
  "Return the indentation from the most recently indented line into the kill-buffer"
  (save-excursion
    (if (re-search-backward "^[ \t]" nil t)
        (let ((bol-point (point)))
          (back-to-indentation)
          (buffer-substring bol-point (point)))
        "")))

(defun my-tex-fill-paragraph ()
  "Do expand-abbrev, and then call Latex-fill-paragraph"
  (interactive)
  (expand-abbrev)
  (Latex-fill-paragraph)
  )

(defun tex-paragraph ()
  "tex the current paragraph.  Skip over initial \"\\item\""
  (interactive)
  (save-excursion
    (mark-paragraph)
    (if (looking-at "\C-j[ \t]*\\\\item ")
        (goto-char (match-end 0)))
    (tex-region-here (point) (mark))))

(defun my-tex-quotify-sexp (count)
  "Insert a pair of double quotes around previous N sexp's, where N is the prefix
arg.  But if N is negative, instead quotify the (-N)th previous sexp.  Modified
from quotify-sexp"
  (interactive "p")
  (let ((negflag (< count 0)))
    (if negflag (setq count (- 0 count)))
    (save-excursion
      (backward-sexp count)
      (my-tex-insert-quote nil)
      (forward-sexp (if negflag 1 count))
      (my-tex-insert-quote nil)))
)

(defun dollar-toggle ()
  "Toggle the syntax entry for $ between being a self-matching delimiter and 
being punctuation"
  (interactive)
  (message
   (concat "Dollar matching now "
           (if (= (char-syntax ?\$) ?\$)
               (progn (modify-syntax-entry ?\$ ".") "OFF")
               (progn (modify-syntax-entry ?\$ "$") "ON")))))

(defun my-tex-insert-quotes ()
  "Insert a pair of double quotes and place point between them."
  (interactive)
  (insert "``''")
  (backward-char 2))

(defun my-tex-insert-vbsize-verbatim ()
  "Insert  \" {\vbsize \begin{verbatim}  \end{verbatim}}\" and position inside."
  (interactive)
  (insert " {\\vbsize \\begin{verbatim}\n    \n\\end{verbatim}}")
  (previous-line 1)
  (end-of-line 1))


(defun my-tex-insert-haskell-brackets ()
  "Insert a \\< \\> pair and place point between them."
  (interactive)
  (insert "\\<\\>")
  (backward-char 2))

(defun add-spec (arg)
  "\\[surround-atsign] and \\[mmm-parse-block] (when \\[[mmm-mode]])"
  (interactive "p")
  (surround-pipes arg)
  (expand-abbrev)
  ;; (insert " ") ; experiment
  ;; starting in emacs 22.3.1, i need a save-excursion
  ;; (when mmm-mode (save-excursion (mmm-parse-block 1)))
  )

(defun shared-tex-lhs-init ()
  (local-set-key "\C-cb" 'tex-environment)
  (local-set-key "\C-c\C-b" 'tex-environment) ; was tex-buffer
  (local-set-key "\C-cI" 'tex-itemize) ; was tex-buffer
  (local-set-key "\C-c{" 'tex-declaration)
  (local-set-key "\C-ce" 'tex-emph)
  (local-set-key "\C-c*" 'tex-emph-sexp)
  (local-set-key (kbd "C-s-b") 'tex-bold-sexp)
  (local-set-key [?\C-*] 'tex-emph-sexp)
  (local-set-key [?\C-$] 'tex-math-sexp)
  (local-set-key "\C-ct" 'tex-texttt)
  (local-set-key "\C-ch" 'tex-hs)
  ;; (local-set-key "\C-c\C-v" 'tex-verb)
  (local-set-key "\C-cf" 'tex-footnote)
  (local-set-key "\C-c\C-f" 'tex-notefoot) ;; was tex-file
  (local-set-key "\C-c\C-n" 'tex-notefoot)
  ;; (local-unset-key "\C-c\C-p") ;; was tex-print
  ;; (local-unset-key "\C-c\C-n")
  (local-set-key "\C-cn" 'tex-note)
  (local-set-key "\e\"" 'my-tex-insert-quotes)
  ;; (local-set-key [?\C-,] 'markdown-mmmify-lines)
  ;; (local-set-key [?\C-'] 'add-code)
  ;; (local-set-key [?\C-|] 'add-spec)
  ;; (local-set-key [?\C-\\] 'add-spec) ; was toggle-input-method
  (local-set-key "\C-cv" 'do-make-noninteractive)
  ;; (local-set-key "\C-c\C-v" 'do-make-see-noninteractive)
  ;; There's also save-make-go ("\C-c\C-r")
  ;; tex-bibtex-file doesn't find the parent doc.
  (local-unset-key "\C-c\C-I")
  (auto-fill-mode 0)
  (visual-line-mode t)
  )

(defun tex-mode-hook-function ()
  ;;(setup-TeX-mode)
  (shared-tex-lhs-init)
  (local-set-key "\"" 'my-tex-insert-quote)
  ;;(local-set-key "\eq" 'my-tex-fill-paragraph)
  (local-set-key "\C-j" 'my-tex-newline-and-indent)
  (local-set-key "\t" 'indent-relative)
  (local-set-key "\C-c[" 'tex-math-display)
  (local-set-key "\C-c$" 'tex-math-inline)
  ;; (local-set-key "\C-cs" 'tex-mysection)
  ;; (local-set-key "\C-c\C-s" 'tex-subsection)
  ;; (local-set-key "\C-cS" 'tex-subsubsection)
  ;; (local-set-key "\C-ci" 'tex-insert-item)
  (local-set-key "\C-ci" 'latex-insert-item)
  (local-set-key [M-return] 'latex-insert-item)
  ;;(local-set-key "\C-cc" 'continue-item) ; from my-text.el
  (local-set-key "\C-cc" 'add-code-env)
  (local-set-key "\C-cs" 'add-spec-env)
  ;; I no longer use the tex-shell stuff.
  ;; (local-set-key "\C-cx" 'tex-x-shell)
  ;; (local-set-key "\C-c\C-m" 'tex-rtn-shell)
  ;; (local-set-key "\C-c\l" 'tex-psl-shell)
  ;; (local-set-key "\C-cy " 'tex-y-shell)
  ;; (local-set-key "\C-cn" 'tex-n-shell)
  ;; (local-set-key "\C-cH" 'tex-paragraph)
  (local-set-key "\C-c\"" 'my-tex-quotify-sexp)
  (local-set-key [?\C-\"] 'my-tex-quotify-sexp)
  ;; override some lhs-isms
  ;; (local-set-key "\C-c@" 'add-code)
  ;; (local-set-key [?\C-'] 'add-spec)
  ;;(local-set-key "\C-c<" 'my-tex-insert-haskell-brackets)
  ;;(local-set-key "\eV" 'my-tex-insert-vbsize-verbatim)
  ;; Sometimes I globally rebind these to the vi versions.
  (local-set-key "\e." 'find-tag)
  (local-set-key "\e," 'tags-loop-continue)
  ;;(modify-syntax-entry ?\\ "w")         ; word constituent
  (modify-syntax-entry ?_ "w")        ; for abbreviations
  (modify-syntax-entry ?\\ "w")        ; for avoiding abbreviations
  ;; enable the $-hack (see dollar-toggle)
  (modify-syntax-entry ?\$ "$")
  (modify-syntax-entry ?\| "$")        ; lhs2tex code fragment: self-matching 
  (no-match-angle)                     ; make angle brackets not match
  ;; Punt specialized versions of these.
  ;;(local-set-key "\C-c\C-f" nil)
  ;;(setq indent-line-function 'indent-to-left-margin)
  ;; Set in tex-mod.el, but somehow lost:
  ;;(setq paragraph-start "^[ \t]*$\\|^[\f\\\\%]")
  ;; Paragraph starts at either (a) empty line, (b) line starting with
  ;; whitespace, or (c) line starting with \item, \begin{, or \end{.
  ;; (setq paragraph-start
  ;;      "^$\\|^\\([ \t]+\\|%\\|> \\|\\\\\\(item\\|begin{\\|end{\\)\\)")
  ;; Simple
  (setq paragraph-start "^$")
  (setq paragraph-separate paragraph-start)
  (setq already-tex-hooked t)
  (setq comment-column 40)                             ; comments at column 40
  (setq comment-start "%"
        comment-end ""
        comment-start-skip "^%* *")
  (local-set-key "\C-c>" 'literate-haskell-mode)
  ;; (setq standard-latex-block-names
  ;;       (union standard-latex-block-names
  ;;              '("haskell" "haskell*")  ;; oops none for now
  ;;              :test 'string-equal))
  (setq tex-command "latex")
  (ispell-minor-mode)
  (setq local-abbrev-table tex-mode-abbrev-table)
  ;; When I use literate haskell files (which uses latex-mode via mmm),
  ;; hard breaks get inserted when reloading a file if the following line
  ;; is here.  If instead, I wait and do the longlines-mode explicitly,
  ;; all is fine.  I think.  Fixed in my tweaked mmm-mode.
  ;; 
  ;; (local-set-key "\C-c\C-r" 'my-tex-region)
  (local-set-key "\C-c\C-r" 'save-make-go)
  (local-set-key "\C-c\C-l" 'save-make-go)    ; was tex-recenter-output-buffer
  (local-unset-key "\C-c\C-j")       ; i want save-junk, not LaTeX-insert-item
  ;; (unless (string-equal (buffer-name) "Junk.lhs")
  ;;   (longlines-mode))
  ;; (TeX-fold-mode 1) ; present in carbon emacs

  (local-set-key (kbd "C-'")   'tex-AB-sexp)
  (local-set-key (kbd "C-M-'") 'tex-AF-sexp)
  (local-set-key (kbd "C-s-'") 'tex-AD-sexp)
  (local-set-key (kbd "C-M-s-'") 'tex-AIC-sexp)
  (toggle-input-method)
  )

;; (defun my-tex-region (start end)
;;   "Copy region to \"body.lhs\" and invoke make."
;;   (interactive "r")
;;   (save-some-buffers t)
;;   (write-region start end "body.lhs")
;;   (kill-compilation-or-not)  ;; from ~/.emacs
;;   (save-window-excursion
;;     (compile "make" t))
;;   )


;; "tex-mode-hook" used to work.  when did emacs get case-sensitivie?
(add-hook 'TeX-mode-hook 'tex-mode-hook-function)  ; for carbon emacs
(add-hook 'tex-mode-hook 'tex-mode-hook-function)  ; for cocoa  emacs

;;; The original version leaves a space before the second and following lines
;;; of a beginning-of-line comment.  I just changed "1+" to "+ 0" in the
;;; last line.
(defun tex-comment-indent ()
  (if (looking-at "%%%")
      (current-column)
      (skip-chars-backward " \t")
      (max (+ 0 (current-column)) comment-column)))


;;; Stuff for the newer tex support.

(setq tex-run-command "latex")
;;; There's a weirdness in yap that makes it try to read from stdin when
;;; starting up.  Without the "< nul", it waits for several <enter>'s.
(setq tex-dvi-view-command "< nul yap")
;; Just makes the .ps file.  How to actually print as well?
(setq tex-dvi-print-command "dvips")
(setq bibtex-command "bibtex")


;; Needed for tex-start-shell
;;(setq tex-shell-file-name "cmd95")


(provide 'my-tex)
