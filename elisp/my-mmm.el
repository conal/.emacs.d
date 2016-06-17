;;; For mmm-mode.  Thanks to Nils Anders Danielsson <nad@cs.chalmers.se>

(add-to-list 'load-path "~/gnu/mmm-mode-0.4.8/" t)

(require 'mmm-mode)

; The following expression was stolen from the Haskell Wiki, then
; modified extensively by me (NAD) .

;;; Be sure to customize mmm-mode-ext-classes-alist if you add a class

(mmm-add-classes
 '((twee
    :classes (twee-javascript
              twee-html
              twee-css
              literate-haskell-lhs2TeX-nobird
              haskellwiki-code
              haskellwiki-code-inline
              )
    )
   (twee-javascript
    :submode javascript-mode
    :front "\n//{{{$"
    :include-front t
    :back "^//}}}\n"
    :include-back t
    :insert ((?j insert-javascript-region
                 nil
                 @ "//{{{\n" @ _ @ "\n//}}}" @ "\n"
                 ;;'(mmm-parse-buffer)
             ))
    )
   (twee-html
    :submode html-mode
    :front "\n<!--{{{-->$"
    :include-front t
    :back "^<!--}}}-->\n"
    :include-back t
    :insert ((?h insert-html-region
                 nil
                 @ "<!--{{{-->\n" @ _ @ "\n<!--}}}-->}}}" @ "\n"
             ))
    )
   (twee-css
    :submode css-mode
    :front "\n/\\*{{{\\*/$"
    :include-front t
    :back "^/\\*}}}\\*/\n"
    :include-back t
    :insert ((?c insert-css-region
                 nil
                 @ "/*{{{*/\n" @ _ @ "\n/*}}}*/" @ "\n"
             ))
    )
   (markdown
    :classes (;; match blocks before inlines, for infix ops like "f `fmap` q"
              ;; markdown-delimited-code-block
              ;; markdown-code-block ; gets confused with text in lists
              literate-haskell-lhs2TeX-bird-code  ; experimental

              ;; literate-haskell-lhs2TeX-code   -- treats all code as comments

              ;; markdown-haskell-inline-double
              ;; haskell coloring spills into outer context
              ;; markdown-haskell-inline
              )
    )
   ;; Currently unused.
   ;; I want to replace by something smarter that grabs the language submode
   ;; from the {.LANG} following the twiddles.
   (markdown-delimited-code-block
    :submode haskell-mode
    :front "^~~~+.*\n"
    :include-front nil
    :back "^~~~+"
    :back-offset (beginning-of-line -1)
    ;; :include-back nil
    )

;;    (markdown-haskell-block
;;     :submode haskell-mode
;;     :front "^\n    [^\*#]"  ; not nested-list
;;     :include-front t
;;     :back "^[^ ]"
;;     :back-offset (beginning-of-line -1)
;;     ;; :include-back nil
;;     )
;;    (markdown-code-block
;;     :submode text-mode
;;     :front "^\n    [^\*#]"  ; not nested-list
;;     :front-offset 1
;;     :include-front t
;;     :back "^[^ ]"
;;     :back-offset (beginning-of-line -1)
;;     ;; :include-back nil
;;     )
;;    (markdown-haskell-inline-double
;;     :submode haskell-mode
;;     :front "\\(^\\|\\s \\|[(/\"-]\\)``"
;;     :back "``\\($\\|\\s \\|[\\\\,.;:/\"'?)-]\\)"
;;     :include-front nil
;;     :include-back nil
;;     ;; :back-offset -1
;;     )
   (markdown-haskell-inline
    :submode haskell-mode
    :front "\\(^\\|\\s \\|\\[\\|[(/\"-]\\)`"
    ;; :back "`\\($\\|\\s \\|[\\\\,.;:/\"'?)-]\\)"
    :back "`\\($\\|\\s \\|\\]\\|[,.;:/\"'?)-]\\)"
    :include-front nil
    :include-back nil
    ;; :back-offset -1
    )
   (literate-haskell-lhs2TeX-nobird
    :classes (literate-haskell-lhs2TeX-code
              literate-haskell-lhs2TeX-verb
              ;; literate-haskell-lhs2TeX-inline
              )
    )
   (literate-haskell-lhs2TeX
    :classes (literate-haskell-lhs2TeX-nobird
              literate-haskell-lhs2TeX-bird-code
              ;; literate-haskell-lhs2TeX-bird-spec
              )
    )
   (literate-haskell-lhs2TeX-code
    :submode haskell-mode
    :front "^\\\\begin{code}\\|^\\\\begin{spec}"
    ; :front-offset (end-of-line 1)
    :include-front nil
    :back "^\\\\end{code}\\|^\\\\end{spec}"
    ; :back-offset (beginning-of-line)  ;; -1 ?
    :include-back nil
    :insert ((?l insert-literate-haskell-laTeX-region
                 nil
                 @ "\\begin{code}\n" @ _ @ "\n\\end{code}" @ "\n"
             ))
    )
   (haskellwiki-code
    :submode haskell-mode
    :front "^<haskell>"
    ; :front-offset (end-of-line 1)
    :include-front nil
    :back "^</haskell>"
    ; :back-offset (beginning-of-line)  ;; -1 ?
    :include-back nil
    :insert ((?W insert-haskellwiki-haskell-code
                 nil
                 @ "<haskell>\n" @ _ @ "\n</haskell>" @ "\n"
             ))
    )
   (haskellwiki-code-inline
    :submode haskell-mode
    :front "<hask>"
    :back "</hask>"
    :include-front nil
    :include-back nil
    ;; :back-offset -1
    :insert ((?w insert-haskellwiki-haskell-code-inline
                 nil
                 @ "<hask>" @ _ @ "</hask>" @
             ))
    )
   (literate-haskell-lhs2TeX-bird-code
    :submode literate-haskell-mode
    :front "^\\(    \\)*[><] "
    :include-front true
    :back "^$" ; "^\\([^><]\\|$\\)" ; "^[^><]"
    :back-offset (beginning-of-line -1)
    :insert ((?b insert-literate-haskell-bird-region
                 nil
                 @ ">" @ " " _ @ "\n" @ "\n"))
    )
; literate-haskell-mode doesn't understand <...
; mmm-mode doesn't handle :back "$" :back-offset 0 very well. The
; keyboard bindings of the two modes aren't handled correctly.
   (literate-haskell-lhs2TeX-bird-spec
    :submode haskell-mode
    :front "^< "
    :back "^$" ; "^\\([^<]\\|$\\)"
    :back-offset -1
    )
   ;; @..@
   (literate-haskell-lhs2TeX-verb
    :submode haskell-mode
    :front "\\(^\\|\\s \\|[(/\"`-]\\)@"
    :back "@\\($\\|\\s \\|[\\\\,.;:/\"'?)-]\\)"
    :include-front nil
    :include-back nil
    ;; :back-offset -1
    )
   ;; |...|
   (literate-haskell-lhs2TeX-inline
    :submode haskell-mode
    :front "\\(^\\|\\s \\|[-(/\"`{]\\)|"
    :back "|\\($\\|\\s \\|[-\\\\,.;:/\"'?){}]\\)"
    :include-front nil
    :include-back nil
    ;; :back-offset -1
    )
   )
 )

;;; Obsolete

;; (literate-haskell
;;     :classes (literate-haskell-bird
;;               literate-haskell-laTeX
;;               )
;;     )
;;    (literate-haskell-bird
;;     :submode literate-haskell-mode
;;     :front "^>"
;;     :include-front t
;;     :back "^[^>]\\|\\'"
;;     :include-back nil
;;     :insert ((?b insert-literate-haskell-bird-region
;;                  nil
;;           y       @ ">" @ " " _ @ "\n" @ "\n"))
;;     )
;;    (literate-haskell-laTeX
;;     :submode haskell-mode
;;     :front "^\\\\begin{code}\n"
;;     :include-front nil
;;     :back "^\\\\end{code}"
;;     :include-back nil
;;     :insert ((?l insert-literate-haskell-laTeX-region
;;                  nil
;;                  @ "\\begin{code}\n" @ _ @ "\\end{code}" @ "\n"
;;                  '(mmm-parse-buffer)))
;;     )


(dolist (entry '((flyspell-prog-text-faces region)
                 (flyspell-generic-check-word-p region)
                 (haskell-literate region (haskell literate-haskell))
                 ))
  (add-to-list 'mmm-save-local-variables entry))

;; These variables are set by custom-set-variables:

;;   '(mmm-global-mode (quote maybe) nil (mmm-mode))
;;   '(mmm-mode-ext-classes-alist (quote ((latex-mode "\\.lhs$" literate-haskell-lhs2TeX) (text-mode "\\.lhs$" literate-haskell))) nil (mmm-mode))
;;   '(mmm-submode-decoration-level 2)



;; A hook named mmm-<major-mode>-hook is also run, if it exists. For
;; example, `mmm-html-mode-hook' is run whenever MMM Mode is entered with
;; HTML mode the dominant mode.

(defun mmm-latex-mode-hook ()
  (message "zonk mmm-latex-mode-hook"))

(defun mmm-literate-haskell-mode-hook ()
  (message "zonk mmm-literate-haskell-mode-hook"))

(provide 'my-mmm)
