;;; -*- Emacs-Lisp -*-

(setq debug-on-error t)

;;; https://blog.vifortech.com/posts/emacs-tls-fix/
(require 'gnutls)
(add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem")

(require 'package)
(add-to-list
  'package-archives
  '("melpa" . "http://melpa.org/packages/")
  '("melpa-stable" . "http://stable.melpa.org/packages/")
  )
(package-initialize)

;;; (package-refresh-contents)  ; Do occasionally

;; List from package-activated-list
;; https://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name
(let ((package-list
       '(attrap
         dante define-word elisp-slime-nav exec-path-from-shell f 
         flycheck-haskell frame-cmds idris-mode intero haskell-mode
         company flycheck lcr dash
         markdown-mode nlinum pkg-info epl popwin s seq
         use-package bind-key w3m yaml-mode zoom-frm
         pos-tip popup button-lock flycheck-color-mode-line
         ;; frame-functions
         ;; mmm-mode
         polymode poly-markdown
         )))
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package))))

(setq my-extra-load-path
      '(;;;"~/gnu"
        "~/.emacs.d/elisp"
        "~/git-repos/git-emacs"
        "~/git-repos/markdown-mode"  ; for https://github.com/jrblevin/markdown-mode/commit/62d5b
        "~/git-repos/glsl-mode"
        "~/git-repos/graphviz-dot-mode"
        "~/git-repos/flycheck-liquidhs.el"
        "~/git-repos/liquid-tip.el"
        ;; I'm getting haskell-mode and intero from melpa (not melpa-stable)
	;; "~/git-repos/haskell-mode"
        ;; "~/git-repos/intero"
        ))

(setq load-path
      (append (mapcar #'expand-file-name my-extra-load-path)
              load-path))

;; (require 'mmm-mode)

;;; Common Lisp conveniences
(load-library "cl")

;; maybe a temp thing until I'm on emacs 22
;; (setq user-emacs-directory (getenv "HOME"))
;; Default is "~/.emacs.d/". I want to move my customizations there.

(require 'graphviz-dot-mode)
(require 'titlecase)

(setq inhibit-startup-message t)

(defun swap-option-command ()
  "Swap option & command keys.  I also changed the binding of
screen-shots in the OS from command-# and command-$ to
command-option-3 and command-option-4, with control added for the
clipboard versions.  In other words, I replaced shift with
option, so as to avoid a clash with.  I also changed the
Spotlight binding from command-space to option-space."
  (interactive)
  (let ((option-was mac-option-modifier))
    (setq mac-option-modifier mac-command-modifier)
    (setq mac-command-modifier option-was)))

;;; default
;; (setq mac-option-modifier 'meta)
;; (setq mac-command-modifier 'super)

(when (eq system-type 'darwin)
  (swap-option-command))

;; (defun anygma ()
;;   "Insert a new Anygma journal entry."
;;   (interactive)
;;   (find-file "~/Journal/anygma.md")
;;   (goto-char (point-max))
;;   (unless (bolp) (newline)) ; start on fresh line
;;   (insert (format-time-string "### %A %d %B, %Y"))
;;   (newline 2)
;;   (open-line 1)
;;   ))

(defun replace-nth (n x l)
  "Replace the Nth element of L with X"
  (let ((copy (copy-list l)))
    (setf (nth n copy) x)
    copy))

(defun tabula-journal ()
  "gitit (wiki) page for Tabula journal. See `journal'."
  (interactive)
  (a-journal (concat tabula-dir "/Journal")))

(defun awake-journal ()
  "gitit (wiki) page for Awake journal. See `journal'."
  (interactive)
  (a-journal (concat "~/Awake/Journal")))

;; (defun personal-journal ()
;;   "gitit (wiki) page for personal journal. See `journal'."
;;   (interactive)
;;   (a-journal "~/Journal/wiki"))

(defun current-journal ()
  "gitit (wiki) page for my current journal. See `journal'."
  (interactive)
  (a-journal (concat "~/Journals/Current")))

(defalias 'journal 'current-journal)

(defun a-journal (dir)
  "gitit (wiki) journal page.  A directory for each year, and a page for each week, named for the date of the Sunday of that week."
  (interactive "DWiki directory: ")
  (let* ((now (current-time))
         (dec-now (decode-time now))
         (day (nth 3 dec-now)) ;; day of month
         (dow (nth 6 dec-now)) ;; day of week
         ;; back up day of month & week
         (sunday (apply #'encode-time (replace-nth 3 (- day dow) (replace-nth 6 0 dec-now))))
         (file-name (concat dir (format-time-string "/wikidata/%Y/%m-%d.page" sunday)))
         ;; e.g. "# Monday, September 13, "
         (entry-header (format-time-string "# %A, %B %d\n" now)))
    (make-directory (file-name-directory file-name) t) ;; make if doesn't exist
    (find-file file-name)
    (when (= (buffer-size) 0)
      ;; Header info. The space before html comments avoid confusion with
      ;; literate Haskell. Using %e causes the day # to be blank-padded
      ;; instead of zero-padded.
      ;; Or use %-e or %-d for no padding.
      (insert
       (format-time-string "---\ntitle: Notes for week of %B %e, %Y\n" sunday)
       "format: markdown\n"
       "autolink_bare_uris: true\n"
       "substMap: []\n"
       "---\n\n"
       )
      ;; See Journal 2016-07-07. Seems a bad idea, since the non-displayed
      ;; content is likely to get carried along when pasting HTML.
      ;; (insert " <!-- <style>.private { display: none; }</style> -->")
      ;; 2016-07-11: This HTML comment line leads to an mmm-mode error unless
      ;; preceded by another blank line. For now, just don't insert the
      ;; comments.
      ;; (insert " <!-- References -->\n\n <!-- -->\n")
      )
    ;; Insert entry header if not already present
    (let ((was-point (point)))
      (goto-char (point-min))
      (if (search-forward entry-header nil t)
          ;; (goto-char was-point) ;;  (1- (point-max))
          (goto-char (if (= was-point (point-min)) (point-max) was-point))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))                   ; start on blank line
        ;; Clear final newlines for uniform separation
        (looking-back "\n+" nil t)
        (delete-region (match-beginning 0) (match-end 0))
        (newline 4)                                     ; start on fresh line
        (previous-line)
        (insert entry-header "\n")
        ;;        (insert "## Hours\n\nxx hours: \n\n## ")
        ;;        (previous-line 2) (end-of-line)
        ;; (markdown-mode)                        ; cleans up lhs mode spill-over
        ))
    )
  ;; For convenience, rename the buffer. If there's already a different
  ;; journal file, rename it first.
  (let ((journal-name "journal")
        (buf (current-buffer)))
    (let ((current-journal (get-buffer journal-name)))
      (when (and current-journal (not (eq buf current-journal)))
        (message "renaming")
        (save-excursion
          (set-buffer current-journal)
          (rename-buffer (file-name-nondirectory (buffer-file-name)))))
      (rename-buffer journal-name)))
  )

(defun web-search (string)
  "Run a web-search search in a browser."
  (interactive "sSearch for: ")
  (browse-url (concat ;; "http://www.google.com/search?q="
                      "http://duckduckgo.com/?q="
                      string)))

(defun web-search-region (from to &optional quoted)
  "Run a web-search search on the contents of the region FROM/TO"
  (interactive "r\nP")
  ;; (message "web-search-region %d %d %s" from to quoted)
  (let ((str (buffer-substring from to)))
    (web-search (if quoted (concat "\"" str "\"") str))
    ))
(global-set-key "\C-cG" 'web-search-region)

(defun disqus-fix (start end)
  "Convert region markdown to HTML and fix up for use in a disqus comment.
Stash the result to the kill ring for pasting into a disqus comment box."
  (interactive "r")
  (save-window-excursion
    (shell-command-on-region start end "pandoc --smart --no-wrap")
    (switch-to-buffer "*Shell Command Output*")
    (cl-flet ((rr (from to) 
               (replace-regexp from to nil (point-min) (point-max))))
      (rr "</p><p>" "</p><br><p>")
      (rr "<blockquote>\n<p>" "<blockquote>")
      (rr "</blockquote>\n<p>" "</blockquote><p>")
      (rr "</p></blockquote>" "</blockquote>")
      )
    (kill-ring-save (point-min) (point-max))
    ))


(defun quotes ()
  "Open quotes file, and paste a quotation."
  (interactive)
  (find-file (expand-file-name "~/quotes/quotes.md"))
  (goto-char (point-max))
  ;; Clear final newlines for uniform separation
  (looking-back "\n+" nil t)
  (delete-region (match-beginning 0) (match-end 0))
  (newline 3)                     ; start on fresh line
  (previous-line)
  (yank)
  )

;; (require 'javascript-mode)
;; (require 'css-mode)

;; (require 'hoogle)

;;; My own stuff

;; (load "~/darcs-repos/haskellmode-emacs/haskell-site-file")

;; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; (setq my-extra-path
;;       (mapcar #'expand-file-name
;;               '("/usr/local/bin" "~/bin" "~/.cabal/bin")))

(load "my-abbrev")
(load "my-text")
(load "my-tex")
(load "my-modes")

;;(load "my-shell")

(global-set-key "\C-c\C-a" 'align-regexp)

(require 'last-modified)
(setq user-full-name "Conal Elliott")
(push 'fix-last-modified-string write-file-hooks)

(add-hook 'find-file-hooks 'auto-insert)

(setq default-major-mode 'indented-text-mode)

;;; I have my own simpler markdown mode

;; (autoload 'markdown-mode "markdown-mode.el"
;;       "Major mode for editing Markdown files" t)

(setq auto-mode-alist
      (append '(
                ("\\.bmp$"           . hexl-mode)
                ("\\.lhs$"           . markdown-mode) ; with mmm. happens in skel
                ("\\.css$"           . css-mode)
                ("\\.js$"            . javascript-mode)
                ("\\-make.inc"       . makefile-mode)
                ("[mM]akefile"       . makefile-mode)
                ("\\.txt$"           . indented-text-mode)
                ("\\.md$"            . markdown-mode)
                ("\\.markdown$"      . markdown-mode)
                ("\\.pcap$"          . hexl-mode)
                ("\\.pcapng$"        . hexl-mode)
                )
              auto-mode-alist))

;; Doesn't work.  Hm.
;; 
;; (push '("^Junk\\." auto-revert-tail-mode) auto-mode-alist)

;; ;; I like a big font.
;; (defun choose-big-font ()
;;   (interactive)
;;   (set-default-font "-outline-Courier New-bold-r-normal-normal-21-157-96-96-c-130-iso10646-1")
;;   )
;; ;; (choose-big-font)

;; Misc

;; Don't use.  The characters are all wrong and only kick in when I
;; explicitly font-lock-fontify-buffer.
;; (require 'pretty-lambda)

(global-set-key "\C-c#" 'what-line)

(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-c\C-t" 'transpose-paragraphs)

(global-set-key "\C-c\C-c" 'comment-region)

;;; For help on notating key sequences, see
;;; http://xahlee.org/emacs/keyboard_shortcuts.html

(defun new-entry (file-name intro-string)
  (find-file file-name)
  (end-of-buffer)
  (insert "\C-j\C-j")
  ;;(next-line 1)
  ;;(insert-time-stamp)
  (insert (current-time-string))
  (beginning-of-line)
  ;; delete time of day
  (forward-char 10)
  (delete-char 9)
  ;; put in asterisks and center
  (beginning-of-line)
  (insert "* * *  ")
  (end-of-line)
  (insert "  * * *")
  (center-line)
  (insert "\C-j\C-j" intro-string)
  (end-of-buffer)
  )

(defun was-new-day ()
  "Insert a new day header for my what-did file"
  (interactive)
  (new-entry "~/misc/what-did.txt" "Misc did:\C-j\C-j  - ")
  )

(defun nvc-journal ()
  "Insert a new day header for my nvc journal"
  (interactive)
  (new-entry "~/NVC/journal.txt" "")
  )

(defun save-replace (from to)
  (save-excursion
    (replace-regexp from to nil nil nil)))

(defun fix-quotes ()
  "Replace non-ascii quotes by ascii quotes"
  (interactive)
  ;; I guess there are a lot of unicode variations of quotation marks.
  ;; TODO: trade in these non-regexp replacements with regexp versions so that there's only one per result.
  (save-replace "[‘’′]" "'")
  (save-replace "[“”]" "\"")
  (save-replace " " " ")
  (save-replace " – " "---")
  (save-replace "" "--")
  (save-replace "½" "1/2")
  (save-replace "×" ":*")
  (save-replace "[—―]" "---")
  (save-replace "→" "->")
  (save-replace "◦" ".")
  (save-replace "…" "...")
  (save-replace "🙂" ":smiley:")
  (save-replace "😄" ":smiley:")
  (save-replace "😞" ":disappointed:")
  (save-replace "😊" ":smiley:")
  (save-replace "😃" ":smiley:")
  (save-replace "😉" ":wink:")
  (save-replace "😎" ":sunglasses:")
  (save-replace "🔥" ":fire:")
  (save-replace "👍" ":thumbsup:")
  (save-replace "⇒" "=>")
  (save-replace "∃" "exists ")
  (save-replace "ﬀ" "ff")
  (save-replace "ﬁ" "fi")
  (save-replace "􏰛" "fi")
  (save-replace "∀" "forall ")
  (save-replace "􏰡" "π")
  (save-replace "􏰠" "=~")
  (save-replace "\n\n \n\n" "\n\n")
  (save-replace "\n\n  \n\n" "\n\n")
  )

(defun fix-pdf ()
  "Replace non-ascii characters extracted from a pdf"
  (interactive)
  (save-replace "ﬀ" "ff")
  (save-replace "Pro ject" "Project")
  (save-replace "pro ject" "project")
  (save-replace "Ob ject" "Object")
  (save-replace "ob ject" "object")
  (save-replace "[“”]" "\"")
  (save-replace "[‘’]" "'")
  )

(defun fix-symbols ()
  "Replace ascii symbols with unicode ones for blog post"
  (interactive)
  (save-replace "\<forall\>" "∀")
  (save-replace "-->" "#-#>")   ; hack to hide "->".  better?
  (save-replace "->" "→")
  (save-replace "#-#>" "-->")   ; matching hack
  (save-replace "\\ " "λ")
  (save-replace "`lub`" "⊔")
  (save-replace " \. " " ∘ ")
  (save-replace "=>" "⇒")
  (save-replace "==" "≡")
  )


(require 'compile)
(setq compile-command "make ")

;; I don't remember what this next bit accomplishes.  Sat Nov  8 07:47:28 2008
;; (setq compilation-error-regexp-alist
;;       (cons '("\n\\([^( \t\n]+\\)[:(][ \t]*\\([0-9]+\\)\\([) \t]\\|:[^0-9\n]\\)" 1 2) compilation-error-regexp-alist))

(defun do-make ()
  (interactive)
  (save-some-buffers t)
  (call-interactively 'compile))

(global-set-key "\C-cm" 'do-make)

;; (defun do-make-noninteractive ()
;;   (interactive)
;;   (save-some-buffers t)
;;   (save-window-excursion
;;     (compile "make")))

;; (defun do-make-see-noninteractive ()
;;   (interactive)
;;   (save-some-buffers t)
;;   (save-window-excursion
;;     (compile "make see")))

(defun save-make-go (clean-first)
  "Save buffers and run make. If there's an argument, begin \"make clean\"."
  (interactive "P")
  (save-some-buffers t)
  ;; How to kill-compilation if it's running?
  ;; (catch 'error (kill-compilation))
  (kill-compilation-or-not)
  (sleep-for 0.1)
  (save-window-excursion
    (when clean-first (call-process "make" nil nil t "clean"))
    (compile "make" t)))

;; Can I use catch instead?  How does one catch an error in e-lisp?
(defun kill-compilation-or-not ()
  "Kill the process made by the \\[compile] or \\[grep] commands.  Like kill-compilation, but
doesn't error out when the process is not running."
  ;; (interactive)
  (let ((buffer (compilation-find-buffer)))
    (if (get-buffer-process buffer)
	(interrupt-process (get-buffer-process buffer))
      ;; (error "The %s process is not running" (downcase mode-name))
      )))

(global-set-key [f7] 'save-make-go)
(global-set-key [f8] 'save-make-go)

(setq abbrev-file-name (expand-file-name "~/.emacs.d/abbrev_defs"))
(read-abbrev-file abbrev-file-name)

(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;;; Load a partial-completion mechanism, which makes minibuffer completion
;;; search multiple words instead of just prefixes; for example, the command
;;; `M-x byte-compile-and-load-file RET' can be abbreviated as `M-x b-c-a RET'
;;; because there are no other commands whose first three words begin with
;;; the letters `b', `c', and `a' respectively.

;;; partial-completion mode is obsolete in emacs 24.

;; ;;(load-library "complete")
;; (partial-completion-mode t)
;; ;;(setq partial-completion-mode nil)
;; ;;(setq PC-meta-flag t)        ; meta not necessary

;; I like case-folding completion
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; (push ".DS_Store" PC-ignored-extensions)
;; (push "._.DS_Store" PC-ignored-extensions)

(add-to-list 'completion-ignored-extensions ".pdf")
(add-to-list 'completion-ignored-extensions ".agdai")

;; A Mac OS system file
(add-to-list 'completion-ignored-extensions ".DS_Store")
(add-to-list 'completion-ignored-extensions "._.DS_Store")

(add-to-list 'completion-ignored-extensions "Junk.hs")

(defun zap-matching ()
  "Zap away the current opening delimiter and its matching closer."
  (interactive)
  (let ((open (point)))
    (forward-sexp)
    (delete-char -1)
    (goto-char open)
    (delete-char 1)))

(global-set-key "\C-cz" 'zap-matching)
(global-set-key "\C-cZ" 'zap-matching)  ; ilisp uses \C-cz

(defun insert-time-stamp ()
  "Insert the current date and time in the buffer.  Useful for making change
logs, putting in a Last Modified in a new file, etc."
  (interactive)
  (insert (current-time-string)))

(global-set-key "\C-cT" 'insert-time-stamp)

(defun insert-change-log-item ()
  "Insert a ChangeLog item into the current buffer"
  (interactive)
  (beginning-of-buffer)
  (insert "\n\n** " (current-time-string) " by " (user-full-name) "\n")
  (insert "    "))

;; ;;; dynamic abbreviation
;; (setq dabbrev-case-fold-search 'case-fold-search)
;; (setq dabbrev-case-replace nil)         ; or 'case-replace

(setq explicit-shell-file-name nil)

;; Still useful?  Sat Nov  8 07:49:04 2008
;;
;; ;; From http://www.cs.washington.edu/homes/voelker/ntemacs.html
;; (setq explicit-shell-file-name "bash")  ;; or "sh"
;; (defun my-shell-setup ()
;;   "For bash under Emacs 20"
;;   (setq comint-scroll-show-maximum-output 'this)
;;   (setq comint-completion-addsuffix t)
;;   (setq comint-process-echoes nil) ; was nil
;;   (setq comint-eol-on-send t)
;;   (make-variable-buffer-local 'comint-completion-addsuffix))
;;
;; (setq shell-mode-hook 'my-shell-setup)
;; 
;; (setq process-coding-system-alist (cons '("bash" . raw-text-unix)
;; 					process-coding-system-alist))


(cd "~")

;; C-m maps to RET, and I don't want it to.
;; (global-unset-key "\C-m")

;; \C-M-% is hard for me
(global-set-key "\C-c%" 'query-replace-regexp)

;; instead of other-window-any-screen
(global-set-key "\M-o" 'my-other-window-or-frame)

(defun my-other-window-or-frame ()
  (interactive)
  (other-window-or-frame 1))

(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)

(global-unset-key "\C-z")

(defun expand-car (pair)
  (cons (concat "^" (expand-file-name (car pair))) (cdr pair)))

(defun insert-todo ()
  "Insert \"TODO: \", prefixed by the mode's comment chars."
  (interactive)
  (comment-dwim nil)
  (insert "TODO: "))

(global-set-key "\C-ct" 'insert-todo)

;; Normally meta-tab, which conflicts with window manager
(global-set-key [s-tab] 'complete-symbol)

;;; Windows stuff:
;; (require 'gnuserv)
;; (gnuserv-start)

(setenv "PAGER" "/bin/cat")
(setenv "EDITOR"
        (if (eq system-type 'darwin)
            (expand-file-name "~/bin/emacsclient-osx")
          "emacsclient"))

(require 'server)
(unless (server-running-p)
  (server-start))

;; (setq haskell-font-lock-symbols nil)

;;; SGML/HTML

;; Haskell "--" code comments look like html comments to emacs when
;; sgml-specials contains ?-.  So exclude here.  Must happen before
;; loading sgml-mode.
(setq sgml-specials '(?\"))

;; Instead of iconify
(global-set-key "\C-z" 'undo)
;; C-x C-z runs the command iconify-or-deiconify-frame
;; But I often hit that combo accidentally
(global-unset-key "\C-x\C-z")

(require 'avoid)
(mouse-avoidance-mode 'none) ;; or 'banish. 'animate appears to spaz on my Mac.

(setq darcs-command-prefix "\C-cd")
(require 'darcs)

(defun darcs-simple-record (label)
  (interactive "sLabel for patch to record: ")
  (darcs-run-command
   (concat "darcs record --all --skip-long-comment"
           " --name=\"" label "\"")))

(defun darcs-add (filename)
  "Add the current buffer's file to the repo"
  (interactive (list (darcs-current-file)))
  (darcs-maybe-save filename)
  (darcs-run-command (darcs-file-command "darcs add %s" (file-name-nondirectory filename))))


(defun darcs-push ()
  "Push to a darcs repo"
  (interactive)
  (let (compile-command)
    (compile "darcs push" nil)))

(define-key darcs-mode-commands-map "r" 'darcs-simple-record)
(define-key darcs-mode-commands-map "a" 'darcs-add)
(define-key darcs-mode-commands-map "p" 'darcs-push)

(defun scroll-cmd (n)
  `(lambda () (interactive) (scroll-up ,n)))

(global-set-key [?\C-\;] (scroll-cmd 1))
(global-set-key [?\C-\s-\;] (scroll-cmd 1)) ;; handy since \C-\; is flyspell-auto-correct-previous-word
(global-set-key [?\C-:]  (scroll-cmd -1))


(defvar save-junk-name "Junk"
  "Name of buffer used by \[save-junk]")

(defun save-junk (start end)
  "Save region to buffer named by \[save-junk-name], with current buffer's extension."
  (interactive "r")
  (let ((junk-file (concat save-junk-name "."
                           (file-name-extension (buffer-file-name)))))
    (write-region
     (format "\n%s From %s, line %d, %s:\n"
             comment-start
             (file-name-nondirectory (buffer-file-name))
             (count-lines 1 (point))
             (format-time-string "%x %X")
             )
     nil junk-file t)
    (append-to-file start end junk-file)))

(global-set-key "\C-c\C-j" 'save-junk)


(defun find-ancestor (dir-name)
  "Return an ancestor director with the given name.  Error if none."
  (interactive "sAncestor name: ")
  (let* ((start (buffer-file-name))
         (look #'(lambda (from)
                   ;; (message "at %s" from)
                   (if (string-equal (file-name-nondirectory from) dir-name)
                       from
                     (let ((next (directory-file-name (file-name-directory from))))
                       (if (string-equal from next)
                           (error "Didn't find %s above %s" dir-name start)
                         (funcall look next)))))))
    (funcall look start)))

(global-set-key [f9] 'next-error)
(global-set-key [f3] 'ispell-complete-word)   ; handier than esc-tab for multi-use

(define-key global-map [(meta control down-mouse-3)] 'imenu)

(defun today-irc-url (channel)
  "The URL for today's IRC log of a given channel"
  (format-time-string (concat "http://tunes.org/~nef/logs/" channel "/%y.%m.%d") nil))

(require 'http-get)

(defun fix-utf8 (string)
  "Fix utf8 chars in STRING"
  (interactive "sString: ")
  (save-window-excursion
    (switch-to-buffer "*fix-utf8 scratch buffer*" t)
    (set-language-environment 'utf-8)
    (delete-region (point-min) (point-max))
    (insert string)
    (decode-coding-region (point-min) (point-max) 'utf-8)
    (buffer-substring (point-min) (point-max))))

(defun irc-filter-pre-insert ()
  "For use as http-filter-pre-insert-hook"
  ;; (message "filter-irc-log-chunk")
  ;; Remove channel quit/join and @where ops reply
  (setq string (replace-regexp-in-string
                ;; 19:48:38 --- quit: lbc_ (Quit: lbc_)
                "^\\([0-9:]+ --- [a-z]+: \\|.*dibblego conal\\|.*##politics  invites you to join.*\\).*\n"
                ;; "\\|\*\*\*"  ;; what's this one?
                ""
                ;; (fix-utf8 string)
                string
                ))
  ;; avoid loss of newlines.
  ;; (longlines-mode 0)
  )

(defun irc-filter-post-insert ()
  "For use as http-filter-post-insert-hook"
  ;; To avoid requiring manual mode change after loading the whole buffer
  ;; what process to use here?
  ;; (decode-coding-region (process-mark) (point) 'utf-8)
  ;; (longlines-mode 1)
  )

(defun irc-log (url)
  "Show today's #haskell IRC log"
  (interactive
   (list (read-from-minibuffer "URL: " (today-irc-url "haskell"))))
  ;; http-filter-pre-insert-hook is buffer-local, so without the next
  ;; line, I was setting http-filter-pre-insert-hook in the wrong buffer.
  (switch-to-buffer (concat "*HTTP GET " url " *"))  ;; creates
  (pushnew 'irc-filter-pre-insert  http-filter-pre-insert-hook)
  (pushnew 'irc-filter-post-insert http-filter-post-insert-hook)
  ;; (set-language-environment 'utf-8)
  (http-get url)
  (text-mode)  ;; http-get changes the mode to fundamental
  ;; (view-mode 1) oops -- too soon.
  ;; oops.  too soon.
  ;; (decode-coding-region (point-min) (point-max) 'utf-8)
  )

(defun chat-save ()
  "Save the current buffer in my chat logs"
  (interactive)
  (let ((coding-system-for-write 'raw-text))
    (write-region (point-min) (point-max) 
                  (concat (format-time-string "~/chats/%Y/%m-%d ")
                          (buffer-name) ;; (remove ?# (buffer-name))
                          ".txt"))))

(defun elim-re (pattern)
  "Elminate the pattern throughout a buffer."
  (replace-regexp pattern "" nil (point-min) (point-max)))

(defun core-clean ()
  "Clean up ghc core output."
  (interactive)
  (replace-regexp "[A-Z][A-za-z]+\\." "" t (region-beginning) (region-end)))

(defun open (file)
  "The system \"open\" command"
  (interactive "fFile to open: ")
  (call-process "open" nil 0 nil (expand-file-name file)))

;;; next two definitions from Svein Ove Aas
(defun first-letter ()
  (move-beginning-of-line nil)
  (let ((beg (point)))
    (search-forward-regexp "[^[:space:]]")
    (- (- (point) beg) 1)))

(defun dewhere-fun ()
  (interactive)
  (save-excursion
    (move-beginning-of-line nil)
    (let ((beg (point)))
      ;; Skip where syntax
      (search-forward-regexp "[[:space:]^]+where[[:space:]]" nil 1)
      (if (< 1 (count-lines beg (point)))
        (goto-char beg)  ; No where, reset point
        (backward-char)) ; Move to just after the where
      ;; Find the function name
      (search-forward-regexp "[[:alnum:]]")
      (backward-char)
      (setq beg (point))
      ;; Find the first line with text at a column before the function name
      (move-beginning-of-line nil)
      (let* ((col (- beg (point)))
             (end (loop do (next-line) until (<= (first-letter) col) finally return (point))))
        (backward-char)
        (kill-region beg (point))))))



(defun pasteboard-insert-markdown ()
  "Extract html from Mac OS X pasteboard, convert to markdown, and paste/yank."
  (interactive)
  (call-process-shell-command "PasteMarkdown" nil t)
  (delete-backward-char 1) ; final newline
  )
(global-set-key "\C-\M-y" 'pasteboard-insert-markdown)


;;; On Emacs 24.5.1 (and possibly a few earlier versions) on Mac, the
;;; following setting leads to some display glitching. See
;;; http://stuff-things.net/2015/10/05/emacs-visible-bell-work-around-on-os-x-el-capitan/
;;; http://emacs.stackexchange.com/questions/20100/what-is-this-square-in-the-middle-of-the-emacs-gui

;; ;; Look for a more pleasant sound.
;; (setq visible-bell t)
(setq visible-bell nil) ;; The default
;; (setq ring-bell-function 'ignore)

(setq ring-bell-function
      (lambda ()
        (invert-face 'mode-line)
        (run-with-timer 0.1 nil 'invert-face 'mode-line)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(PC-meta-flag nil)
 '(agda-input-user-translations
   (quote
    (("r--|" "⟼")
     ("r|" "⤇")
     ("^-1" "⁻¹")
     ("u^" "˘")
     ("bij" "⤖")
     ("<x>" "⟨×⟩")
     ("map1" "map₁")
     ("map2" "map₂")
     ("mapi" "mapⁱ")
     ("mapi2" "mapⁱ₂")
     ("~<" "≈⟨ ? ⟩")
     ("purei" "pureⁱ")
     ("=?" "≟")
     ("=<" "≡⟨⟩")
     ("r]" "↦")
     ("~^" "≈˘⟨ ? ⟩")
     ("ex1" "∃¹")
     ("ex2" "∃²")
     ("ust" "꙳")
     ("la" "λ")
     ("del" "δ")
     ("begin" "\\begin")
     ("end" "\\end")
     ("<o>" "⥈"))))
 '(auto-save-interval 30)
 '(backup-by-copying t)
 '(c-style-variables-are-local-p nil)
 '(c-tab-always-indent t)
 '(column-number-mode t)
 '(comment-style (quote indent))
 '(dabbrev-case-fold-search (quote case-fold-search))
 '(dabbrev-case-replace (quote case-replace))
 '(default-frame-alist-qqq
    (quote
     ((height . 37)
      (width . 126)
      (font . "-outline-Courier
New-bold-r-normal-normal-19-142-96-96-c-110-iso10646-1")
      (tool-bar-lines . 0)
      (menu-bar-lines . 1))))
 '(default-input-method "TeX")
 '(delete-old-versions t)
 '(display-buffer-reuse-frames t)
 '(erc-autojoin-channels-alist
   (quote
    (("freenode.net" "#haskell-blah" "#haskell-iphone" "#haskell-ops" "#haskell-in-depth" "#ghc" "#haskell")
     (".*\\.freenode\\.net" "#haskell" "#ghc" "#haskell-in-depth" "#haskell-ops" "#haskell-blah" "#haskell-iphone"))))
 '(erc-away-nickname nil)
 '(erc-fill-column 100)
 '(erc-fill-mode nil)
 '(erc-mode-hook
   (quote
    (erc-munge-invisibility-spec pcomplete-erc-setup erc-button-add-keys
                                 (lambda nil
                                   (setq imenu-create-index-function
                                         (quote erc-create-imenu-index)))
                                 (lambda nil
                                   (abbrev-mode 1)))))
 '(erc-nick "conal")
 '(erc-nick-uniquifier "+")
 '(erc-prompt-for-password t)
 '(erc-user-full-name "Conal Elliott")
 '(erc-whowas-on-nosuchnick t)
 '(eval-expression-print-length 200)
 '(eval-expression-print-level 12)
 '(fill-column 80)
 '(flycheck-disabled-checkers (quote (haskell-stack-ghc)))
 '(flymake-no-changes-timeout 0.5)
 '(fringe-mode (quote (1 . 1)) nil (fringe))
 '(git-branch-buffer-closes-after-action nil)
 '(git-working-dir-change-behaviour (quote git-refresh-all-saved))
 '(graphviz-dot-preview-extension "pdf")
 '(graphviz-dot-view-command "view-dot %s")
 '(haskell-auto-insert-module-format-string
   "
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-imports #-} -- TEMP

-- | 

module %s where

" t)
 '(haskell-hoogle-command nil)
 '(haskell-indent-offset 2)
 '(haskell-process-args-cabal-repl (quote ("--ghc-option=-ferror-spans")))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-haskell-docs-imports nil)
 '(haskell-process-suggest-hoogle-imports t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-use-presentation-mode t)
 '(haskell-tags-on-save t)
 '(inferior-haskell-wait-and-jump t)
 '(ispell-program-name "aspell")
 '(ispell-silently-savep t)
 '(longlines-show-hard-newlines nil)
 '(longlines-wrap-follows-window-size t)
 '(mac-pass-command-to-system nil)
 '(markdown-asymmetric-header t)
 '(markdown-command "pandoc --toc --standalone --to html+smart")
 '(markdown-enable-html nil)
 '(markdown-enable-math nil)
 '(markdown-header-scaling t)
 '(markdown-hr-strings
   (quote
    ("* * * * * * * * * * * * * * * * * * * *" "---------------------------------------" "* * * * *" "---------" "* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *" "-------------------------------------------------------------------------------")))
 '(markdown-indent-on-enter t)
 '(markdown-unordered-list-item-prefix "
*   ")
 '(message-log-max 500)
 '(ns-use-native-fullscreen nil)
 '(package-selected-packages
   (quote
    (polymode-markdown poly-markdown polymode idris-mode flycheck-color-mode-line button-lock popup pos-tip attrap popwin use-package dante haskell-mode nlinum image+ company zoom-frm yaml-mode w3m mmm-mode markdown-mode flycheck-haskell exec-path-from-shell elisp-slime-nav define-word)))
 '(parens-require-spaces nil)
 '(pcomplete-ignore-case t)
 '(ps-font-size (quote (8 . 10)))
 '(read-buffer-completion-ignore-case t)
 '(safe-local-variable-values
   (quote
    ((flycheck-disabled-checkers quote
                                 (haskell-ghc haskell-stack-ghc))
     (flycheck-disabled-checkers quote
                                 (haskell-stack-ghc)))))
 '(scroll-conservatively 1000)
 '(scroll-margin 3)
 '(sentence-end-double-space nil)
 '(tags-case-fold-search nil)
 '(tex-shell-file-name "bash")
 '(tool-bar-mode nil)
 '(user-mail-address nil)
 '(vc-make-backup-files t)
 '(warning-suppress-types (quote ((undo discard-info)))))

 ;; '(mmm-global-mode (quote maybe) nil (mmm-mode))
 ;; '(mmm-idle-timer-delay 0.2)
 ;; '(mmm-parse-when-idle nil)


;;  '(user-mail-address "conal@conal.net")

;;; For git--config-get-email, overriding user-mail-address variable.
;;; Might not be the right thing for other uses of (user-mail-address).
(defun user-mail-address ()
  (let ((email (completing-read 
                "email: " '("Conal.Elliott@target.com")
                nil nil "conal@conal.net")))
    (git--config "user.email" email)
    (message "Repo user email set to %s" email)
    email))

;;; See http://www.emacswiki.org/emacs/EmacsClient#toc21
;;; 
;;;  '(server-done-hook (quote ((lambda nil (kill-buffer nil)) delete-frame)))
;;;  '(server-switch-hook (quote ((lambda nil (let (server-buf) (setq server-buf (current-buffer)) (bury-buffer) (switch-to-buffer-other-frame server-buf))))))

(put 'downcase-region 'disabled nil)
(put 'upcase-region   'disabled nil)

;; Keep dictionary with .emacs file, checked into git.
(setq ispell-personal-dictionary (expand-file-name "~/.emacs.d/.aspell.en.pws"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-doc-face ((t (:inherit font-lock-string-face :foreground "tomato4"))))
 '(markdown-blockquote-face ((t (:inherit font-lock-doc-face :foreground "#262"))))
 '(markdown-code-face ((t (:inherit default :foreground "DodgerBlue4"))))
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold :family "times"))))
 '(mmm-default-submode-face ((t (:background "lemon chiffon")))))

;; http://www.emacswiki.org/emacs/InteractiveSpell#toc1
(setq ispell-process-directory (expand-file-name "~/"))

;;; Some Linux customizations:
(setq x-select-enable-clipboard t)

;;; Turn off the tool bar.
(tool-bar-mode 0)

;;; For carbon emacs
(set-frame-parameter nil 'alpha 1.0)

;;; http://superuser.com/questions/256404/fullscreen-emacs-in-osx
(global-set-key [(control meta return)] 'toggle-frame-fullscreen)

;;; ;;; Save for insert item in latex and markdown modes
;;; (global-set-key [(meta return)] 'toggle-frame-fullscreen)

;;; (toggle-frame-fullscreen)

;;; Turn off some especially dangerous mac-style bindings for Cocoa Emacs
(global-unset-key [(super v)])
(global-unset-key [(super x)])
;; (global-unset-key [(super h)])
(global-unset-key [(super d)])
(global-unset-key [(super m)])  ;; minimizes to dock

(global-set-key [(super x)] 'execute-extended-command)

(global-set-key [(meta F11)] 'blort)

;; (autoload 'git-status "git-status" "Entry point into git-status mode." t)
(require 'git-status)  ; always
(autoload 'git-blame-mode "git-blame"
  "Minor mode for incremental blame for Git." t)

;keybindindings for git
(global-set-key [(meta super s)] 'git-status)

(defun blogify-region (from to &optional private)
  "Run blogify on the contents of the region bounded by FROM and TO and save the result in the inter-program copy buffer."
  (interactive "rP")
  (message "(%s)" (if private "private" "public"))
  (let ((blogify-program "blogify")) ; "blogify-was1"
    (save-window-excursion
      (shell-command-on-region from to (concat blogify-program (if private " --private" "")))
      (switch-to-buffer "*Shell Command Output*")
      (beginning-of-buffer)
      ;; Pandoc inserts annotations elements when generating MathML from LaTeX.
      ;; When I copying from the browser and paste into an email message, the annotations become visible.
      ;; To fix, we can remove the annotation or make it invisible with "display:none" CSS.
      (while (re-search-forward "<annotation .*?</annotation>" nil t)
        (replace-match "" nil nil))
      (kill-ring-save (point-min) (point-max))
      ;; (x-select-text (buffer-string))
      )))

(defun blogify-buffer (&optional private)
  "Run blogify on the contents of the current buffer and save the result in the inter-program copy buffer."
  (interactive "P")
  (blogify-region (point-min) (point-max) private))

(defun blogify-view-foo (&optional private)
  "'blogify-buffer' and browser-view the resulting foo.html."
  (interactive "P")
  (blogify-foo private)
  (browse-url "foo.html"))

(defun blogify-foo (&optional private)
  (interactive "P")
  (blogify-buffer private)
  (let ((title (save-excursion
                 (beginning-of-buffer)
                 (if (search-forward "\ntitle: " nil t)
                     (buffer-substring (match-end 0) (progn (end-of-line) (point)))
                   "No title"))))
    (save-window-excursion
      (switch-to-buffer "*Shell Command Output*")
      (beginning-of-buffer)
      (insert "<title>" title "</title>\n")
      (write-region (point-min) (point-max) "foo.html"))))

;;; To do: make blogify-foo *asynchronous*, to save waiting.

;; Experiment: set globally rather than just in markdown-mode, since I use mmm-mode.
(global-set-key "\C-cv" 'blogify-foo)
(global-set-key "\C-c\C-r" 'blogify-foo)
(global-set-key "\C-c\C-v" 'blogify-view-foo)

(autoload 'wikipedia-mode "wikipedia-mode.el"
  "Major mode for editing documents in Wikipedia markup." t)

(add-to-list 'auto-mode-alist '("\\.wiki\\'" . wikipedia-mode))

(require 'zoom-frm)
(global-set-key (if (boundp 'mouse-wheel-down-event) ; Emacs 22+
                    (vector (list 'control mouse-wheel-down-event))
                  [C-mouse-wheel])    ; Emacs 20, 21
                'zoom-in)
(when (boundp 'mouse-wheel-up-event) ; Emacs 22+
  (global-set-key (vector (list 'control mouse-wheel-up-event))
                  'zoom-out))

(global-set-key [s-up]   'zoom-in)
(global-set-key [s-down] 'zoom-out)

;; TODO: unify zoom bindings

;; Start with larger fonts
(when window-system
  (let ((frame-zoom-font-difference 10)) (zoom-frm-in)))

(global-set-key "\C-cR" 'rot13-region)

;; Get rid of `mouse-set-font' or `mouse-appearance-menu':
;; (global-set-key [S-down-mouse-1] nil)

;; On Linux, M-SPC is getting co-opted (for the window menu).
(when (eq system-type 'gnu/linux)
  (global-set-key [?\s- ] 'just-one-space))

(defvar growlnotify-command (executable-find "growlnotify") "The path to growlnotify")

(defun growl (title message)
  "Shows a message through the growl notification system using
 `growlnotify-command` as the program."
  (cl-flet ((encfn (s) (encode-coding-string s (keyboard-coding-system))) )
    (let* ((process (start-process "growlnotify" nil
                                   growlnotify-command
                                   (encfn title)
                                   "-a" "Emacs"
                                   "-n" "Emacs")))
      (process-send-string process (encfn message))
      (process-send-string process "\n")
      (process-send-eof process)))
  t)

(defun my-erc-hook (match-type nick message)
  "Shows a growl notification, when user's nick was mentioned. If the buffer is currently not visible, makes it sticky."
  (unless (posix-string-match "^\\** *Users on #" message)
    (growl
     (concat "ERC: name mentioned on: " (buffer-name (current-buffer)))
     message
     )))

(when (eq system-type 'darwin)
  (add-hook 'erc-text-matched-hook 'my-erc-hook))

(defun big ()
  "Big font in a big window"
  (interactive)
  (maximize-frame)
  (while (> (frame-width) 120)
    (enlarge-font 1)
    (maximize-frame)
    ))

(require 'linum) ; linum-mode

;; (journal)

(defun exercise-journal ()
  (interactive "")
  (switch-to-buffer "journal")
  (goto-char (point-max))
  (if (looking-back "\n+" nil t)
      (progn
        (delete-region (match-beginning 0) (match-end 0))
        (newline 2)
        (previous-line))
    (insert "\n"))
  (end-of-line)
  (insert "\n## Exercise\n\nMini-elliptical:  miles.\n")
  (backward-word 1) (backward-char 1))

(defun hipchat-trim (start end)
  "Tidy HipChat output after copy&paste."
  (interactive "r")
  ;; (message "hipchat-trim: %d %d" start end)
  (narrow-to-region start end)
  (save-excursion
    (while (re-search-forward "^\\[.*\\] \\([^ ]+\\)[^:]*:" nil t)
      (replace-match "*\\1:*" nil)))
  (save-excursion
    (while (re-search-forward "\\(.\\)\n" nil t)
      (replace-match "\\1  \n" nil)))
  (widen))

(defun slack-trim (start end)
  "Tidy Slack output after copy&paste."
  (interactive "r")
  ;; (message "slack-trim: %d %d" start end)
  (narrow-to-region start end)  ;; restore after testing
  ;; Usernames can be up to 21 characters long. They can contain lowercase
  ;; letters a to z (without accents), numbers 0 to 9, hyphens, periods, and
  ;; underscores.
  ;; https://get.slack.help/hc/en-us/articles/216360827-Change-your-username
  (save-excursion
    (goto-char start)
    (save-excursion
      (while (re-search-forward
              "^\n*\\([a-z0-9. _é-]+\\)  [:0-9 APM]*$"
              nil t)
        (replace-match "\n*\\1:*" nil)))
    ;; Same speaker continuing looks like blank lines and a bracketed date line.
    (save-excursion
      (while (re-search-forward "\n+[:0-9 APM]+ *\n" nil t)
        (replace-match "  \n" nil)))
    (save-excursion
      (while (search-forward ":slightly_smiling_face:" nil t)
        (replace-match ":smile:" nil)))
    (save-excursion
      (while (re-search-forward "\\(.\\) *\n" nil t)
        (replace-match "\\1  \n" nil)))
    (save-excursion
      (while (search-forward ":*  \n" nil t)
        (replace-match ":*\n" nil)))
    (save-excursion
      (while (re-search-forward "white_check_mark *\neyes *\nraised_hands *\n" nil t)
        (replace-match "" nil)))
    (save-excursion
      (while (re-search-forward "  +\n\n" nil t)
        (replace-match "\n\n" nil))))
    (save-excursion
      (while (re-search-forward "\n\n\n\n\nNew *\n\n" nil t)
        (replace-match "\n" nil)))
    (fix-quotes)
  (widen))

(defun messages-trim (start end)
  "Tidy Apple Messages output after copy&paste."
  (interactive "r")
  ;; (message "messages-trim: %d %d" start end)
  (narrow-to-region start end)  ;; restore after testing
  (when (eolp) (delete-char 1))
  (save-excursion
    (goto-char start)
    ;; Speaker lines end in a colon
    (save-excursion
      ;; (replace-regexp "\\([A-Za-z ]+\\):$" "\n*\\1:*"))
      (while (search-forward-regexp "^\\([A-Za-z ]+\\):$" nil t)
        (replace-match "\n*\\1:*")))
    ;; Content lines begin with a tab
    (save-excursion
      ;; (replace-regexp "^\t\\(.*\\)" "\\1  "))
      (while (search-forward-regexp "^\t\\(.*\\)" nil t)
        (replace-match "\\1  ")))
    (fix-quotes))
  (widen))

(defun hangouts-trim (start end)
  "Tidy Google Hangouts output after copy&paste."
  (interactive "r")
  ;; (message "hangouts-trim: %d %d" start end)
  (narrow-to-region start end)  ;; restore after testing
  (when (eolp) (delete-char 1))
  (save-excursion
    (goto-char start)
    ;; Speaker lines can be tricky to identify.
    ;; For now, two capitalized names.
    (save-excursion
      (while (search-forward-regexp "^\\([A-Z][a-z]+ [A-Z][a-z]+\\)$" nil t)
        (replace-match "\n_\\1:_")))
    ;; Remove date/time lines, e.g., "Thursday, August 23, 2018 8:54 AM"
    (save-excursion
      (while (search-forward-regexp "^[A-Z][a-z]+day,.*[AP]M\n" nil t)
        (replace-match "")))
    ;; add two spaces to the end of content lines (for line breaks)
    (save-excursion
      (while (search-forward-regexp "^\\(.+\\)" nil t)
        (replace-match "\\1  ")))
    (fix-quotes))
  (widen))

(defun irc-trim (start end)
  "Tidy IRC output after copy&paste."
  (interactive "r")
  ;; (message "irc-trim: %d %d" start end)
  (narrow-to-region start end) ;; restore after testing
  ;; Usernames can be up to 21 characters long. They can contain lowercase
  ;; letters a to z (without accents), numbers 0 to 9, hyphens, periods, and
  ;; underscores.
  ;; https://get.slack.help/hc/en-us/articles/216360827-Change-your-username
  (save-excursion
    (while (re-search-forward "^\\[[:0-9 APM]*\\] *<\\([^ ]+\\)>\t*\\(.*\\)" nil t)
      (replace-match "*\\1:* \\2\n" nil)))
  (save-excursion
    (while (re-search-forward "^.*) left IRC (.*\n" nil t)
      (replace-match "" nil)))
  (save-excursion
    (while (re-search-forward "^.*) joined the channel\n" nil t)
      (replace-match "" nil)))
  (fix-quotes)
  (widen))

(defun discord-trim (start end)
  "Tidy Discord Markdown after copy&paste."
  (interactive "r")
  (narrow-to-region start end) ;; restore after testing
  (save-excursion
    (goto-char start)
    (save-excursion
      (while (re-search-forward
              "^\\(.*\\)\\(To\\|Yester\\)day at [0-9]+:[0-9]+ [AP]M\n"
              nil t)
        (replace-match "\n*\\1:* " nil)))
    (save-excursion
      (while (re-search-forward "\\(.+\\)" nil t)
        (replace-match "\\1  " nil)))
    (save-excursion
      (while (search-forward "(edited)" nil t)
        (replace-match "" nil)))
    ;; (save-excursion
    ;;   (while (search-forward "\\[" nil t) (replace-match "" nil)))
    ;; (save-excursion
    ;;   (while (search-forward "\\]" nil t) (replace-match "" nil)))
    (save-excursion
      (while (re-search-forward "\n1\n" nil t) (replace-match "" nil)))
    (save-excursion
      (while (re-search-forward "\n__[0-9]+:[0-9]+ [AP]M__\n" nil t)
        (replace-match "" nil)))
    (save-excursion
      (while (re-search-forward " +\n\n" nil t)
        (replace-match "\n\n" nil)))
    )
  (fix-quotes)
  (widen))

(defun slack-markdown-trim (start end)
  "Tidy Slack Markdown after copy&paste."
  (interactive "r")
  (narrow-to-region start end) ;; restore after testing
  (save-excursion
    (goto-char start)
    (save-excursion
      ;; [1:10 PM](https://agda-categories.slack.com/archives/CQ547CJTG/p1601669447011700)  
      (while (re-search-forward
              "^\\[[0-9]+:[0-9]+\\](.*)\n"
              nil t)
        (replace-match "" nil)))
    (save-excursion
      ;; [Conal Elliott](/team/U01B8U851MG)[1:10 PM](https://agda-categories.slack.com/archives/CQ547CJTG/p1601669447011700)  
      (while (re-search-forward
              "^\\[\\(.*?\\)\\](/team/[A-Z0-9]*)\\(\\[[0-9]+:[0-9]+ [AP]M\\]\\)\\((.*?)\\)  \n"
              nil t)
        (replace-match
         ;; "*\\1* \\2\\3:"
         ;; "[\\1]\\3:"
         "*\\1*:"
         nil)))
    (save-excursion
      (while (re-search-forward "\n[0-9]+\n" nil t) (replace-match "" nil)))
    (save-excursion
      (while (search-forward "\n\n\n" nil t) (replace-match "\n\n" nil)))
    (save-excursion
      (while (re-search-forward "\\[\\(@.*?\\)\\](.*?)" nil t) (replace-match "*\\1*" nil)))
    (save-excursion
      (while (re-search-forward "\nGitHub\n\n\\[.*\\](.*)\n\n.*\n" nil t) (replace-match "" nil)))
    ;; If I decide to keep time stamps above, then drop the next rewrite.
    (save-excursion
      (while (re-search-forward "\nToday\n\n\\* \\* \\*\n" nil t) (replace-match "" nil)))
    )
  (fix-quotes)
  (widen))

(defun amazon-track-rename (&optional arg)
  "Keyboard macro."
  (interactive "p")
  (kmacro-exec-ring-item (quote ("\355mv \"\" \242\202\213 " 0 "%d")) arg))

(let ((tags-add-tables t))
  (mapc #'visit-tags-table
        '(
          ;; Find-tags favors later entries in this list
          ;; find . -name '*.*hs' | xargs hasktags -e
          )))

          ;; "~/git-repos/ghc/compiler/TAGS"

;;           "~/Haskell/circat/src/TAGS"
;;           "~/Haskell/shaped-types/src/TAGS"
;;           "~/Haskell/reification-rules/src/TAGS"


;;           "~/git-repos/kure/TAGS"
;;           "~/git-repos/ku-latest/hermit/src/TAGS"
;;           "~/Haskell/hermit-extras/src/TAGS"
;;           "~/Haskell/monomorph/src/TAGS"
;;           "~/Haskell/lambda-ccc/src/TAGS"

;;; I keep hitting this on accidentally (ns-print-buffer), when
;;; I've been interacting with terminal programs.
(global-unset-key [?\s-p])

;; (global-set-key "\C-cv" 'view-mode)

(defun git-push () (interactive) (git-cmd "push"))
(defun git-pull () (interactive) (git-cmd "pull"))

(global-set-key "\C-xgp" 'git-pull)
(global-set-key "\C-xgP" 'git-push)


;;; http://oremacs.com/2015/05/22/define-word/
;; (package-install 'define-word)
(global-set-key (kbd "C-c d") 'define-word-at-point)
(global-set-key (kbd "C-c D") 'define-word)

;;; mmm-mode stuff

;; (load "my-mmm") ; abandoning

;;; Fenced code in Markdown, thanks to http://jblevins.org/log/mmm

;; (defun my-mmm-markdown-auto-class (lang &optional submode)
;;   "Define a mmm-mode class for LANG in `markdown-mode' using SUBMODE.
;; If SUBMODE is not provided, use `LANG-mode' by default."
;;   (let ((class (intern (concat "markdown-" lang)))
;;         (submode (or submode (intern (concat lang "-mode"))))
;;         ;; (front (concat "^``` *" lang "[\n\r]+"))
;;         (front (concat "^ *``` *" lang "$"))
;;         (back "^ *```"))
;;     (mmm-add-classes (list (list class :submode submode :front front :back back :front-offset 1)))
;;     (mmm-add-mode-ext-class 'markdown-mode nil class)))

;; ;; Mode names that derive directly from the language name
;; (mapc 'my-mmm-markdown-auto-class
;;       '(
;;         "agda"
;;         "awk" "bibtex" "c" "cpp" "css" "html" "latex" "lisp" "makefile"
;;         "markdown" "python" "r" "ruby" "sql" "stata" "xml"
;;         "javascript" "haskell" "glsl" "verilog"
;;         "yaml"
;;         ))

;; ;; Mode names that differ from the language name
;; (my-mmm-markdown-auto-class "fortran" 'f90-mode)
;; (my-mmm-markdown-auto-class "perl" 'cperl-mode)
;; (my-mmm-markdown-auto-class "shell" 'shell-script-mode)
;; (my-mmm-markdown-auto-class "bash" 'shell-script-mode)
;; (my-mmm-markdown-auto-class "json" 'javascript-mode)

;; ;;; Careful with this one. I can lead to "Agda is busy with something in the
;; ;;; buffer #<killed buffer>" or just slowing down Emacs quite a lot.
;; ;; (my-mmm-markdown-auto-class "agda" 'agda2-mode)

;; ;; ;; Experimental alternative
;; ;; (my-mmm-markdown-auto-class "agda" 'fundamental-mode)

;; ;; Slows down scrolling quite a lot when point is in a dot region
;; (my-mmm-markdown-auto-class "dot" 'graphviz-dot-mode)
;; ;; (my-mmm-markdown-auto-class "dot" 'text-mode)

;; ;; (mmm-add-classes
;; ;;  '((markdown-haskell-birdtracks
;; ;;     :submode haskell-mode
;; ;;     :front "^> "
;; ;;     ;; :back "^$"
;; ;;     ;; :back "\n[^>]"
;; ;;     ;; :back "^\\($\\|[^>]\\)"
;; ;;     :back "$"         ; experiment
;; ;;     :include-front nil
;; ;;     :include-back  t  ; experiment
;; ;;     )))

;; ;; (mmm-add-classes
;; ;;  '((markdown-haskell-birdtracks
;; ;;     :submode haskell-mode
;; ;;     :front "^> "
;; ;;     ;; :back "^$" :back-offset -1
;; ;;     :back "$"
;; ;;     :include-front nil
;; ;;     )))
;; ;; (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-haskell-birdtracks)

;; ;; Alternatively,

;; ;; (mmm-add-classes
;; ;;  '((markdown-haskell-birdtracks
;; ;;     :submode literate-haskell-mode
;; ;;     :front "^> "
;; ;;     ;; :back "^$" :back-offset -1
;; ;;     :back "$"
;; ;;     :include-front nil
;; ;;     )))

;; ;; ;; Experiment
;; ;; (mmm-add-classes
;; ;;  '((markdown-haskell-inline
;; ;;     :submode haskell-mode
;; ;;     :front "\\( \\|^\\)`+"
;; ;;     :back "`+[ ,.\n]"
;; ;;     ;; :include-front t
;; ;;     )))
;; ;; (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-haskell-inline)

;; ;; (mmm-add-classes
;; ;;  '((markdown-fenced-plain
;; ;;     :submode fundamental-mode
;; ;;     ;; :front "^```[\n\r]+"
;; ;;     :front "^[\r\n]```[\r\n]+"
;; ;;     :back "[^\r\n][\r\n]```$"
;; ;;     :back-offset 1
;; ;;     ;; :include-front true
;; ;;     )))
;; ;; (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-fenced-plain)

;; ;;; I finally got the regexps right, but I think font-locking doesn't work in
;; ;;; fundamental-mode.

;; ;; (search-forward-regexp "[\r\n]```[\r\n]+")

;; ;; ;; Still experimenting with this one. When I decide, move it to customize
;; ;; (setq mmm-parse-when-idle t)

;; ;; markdown-mode binds mmm-parse-buffer to C-M-,

(require 'company)

(global-company-mode) ;; everywhere!
;; Enable dabbrev everywhere company-mode is on.
(add-to-list 'company-backends 'company-dabbrev)

;; Emacs Lisp code all effortlessly available via `M-.` (poppable via `M-*`).
(require 'elisp-slime-nav)
(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)

;;; Jump to elisp definitions.
;;; http://emacsredux.com/blog/2014/06/18/quickly-find-emacs-lisp-sources/
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)

;;; Elide uninteresting files in dired (including auto-save)
;;; https://www.emacswiki.org/emacs/DiredOmitMode
(require 'dired-x)
(setq-default dired-omit-files-p t) ; Buffer-local variable
;; (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
(setq dired-omit-files (concat dired-omit-files "\\|^\\.DS_Store$"))

(display-time-mode) ; show time in mode line

;; (require 'popwin)
;; (popwin-mode 1)

;;; https://github.com/nitros12/discord-emacs.el
(load-file "~/git-repos/discord-emacs.el/discord-emacs.el")

;;; https://wiki.portal.chalmers.se/agda/Docs/HowToSeeUnicode
;; (set-fontset-font "fontset-default" 'unicode "DejaVu Sans Mono")
;;; Probably move the next few forms elsewhere

(define-hostmode poly-latex-hostmode
    :mode 'latex-mode)

(define-innermode poly-agda-innermode
  :mode 'agda2-mode
  :head-matcher "\\\\begin{code}"
  :tail-matcher "\\\\end{code}"
  :head-mode 'host
  :tail-mode 'host)

(define-polymode poly-latex-mode
  :hostmode 'pm-host/latex
  :innermodes '(poly-agda-innermode))

(add-to-list 'auto-mode-alist '("\\.lagda\\'" . poly-latex-mode))
(add-to-list 'auto-mode-alist '("\\.md"       . poly-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.page"     . poly-markdown-mode))

;;; End of customizations
(setq debug-on-error nil)
