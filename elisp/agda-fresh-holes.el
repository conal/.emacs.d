;;; agda-fresh-holes.el --- force fresh elaboration of holey Agda buffers  -*- lexical-binding: t; -*-

;; Problem (agda/agda#2475, Icebox since 2017; #3392 dup): `agda2-load' of a
;; file whose interface (.agdai) is current REUSES the interface and skips
;; elaboration — but interaction points exist only as elaboration side
;; effects, so every `?'/`{!!}' silently reports as "0 goals".  The interface
;; gets written whenever the module is batch-checked as a DEPENDENCY (e.g. by
;; `make' of an importer, under --allow-unsolved-metas), so the trap arms
;; itself on any build-then-inspect workflow.
;;
;; Fix, scoped so hole-free modules keep instant interface reuse: before
;; `agda2-load', IF the buffer contains hole syntax, delete this module's own
;; .agdai (both layouts: beside-source and _build/<ver>/agda/<relpath>).
;; Dependencies' interfaces are untouched, so the forced re-elaboration costs
;; only this one module.  Mirrors ~/.claude/scripts/agda-goals.sh.

;;; Code:

(defun agda-fresh-holes--project-root (file)
  "Nearest ancestor of FILE containing _build, .git, or an .agda-lib file.
Falls back to FILE's directory."
  (let ((dir (file-name-directory file)))
    (or (locate-dominating-file
         dir (lambda (d)
               (or (file-directory-p (expand-file-name "_build" d))
                   (file-directory-p (expand-file-name ".git" d))
                   (directory-files d nil "\\.agda-lib\\'" t))))
        dir)))

(defun agda-fresh-holes--agdai-paths (file)
  "All existing .agdai paths for the Agda source FILE, in both layouts."
  (let* ((file (expand-file-name file))
         (root (expand-file-name (agda-fresh-holes--project-root file)))
         (rel  (file-relative-name file root))
         (rel-noext (file-name-sans-extension rel)))
    (append
     ;; beside-source layout (--local-interfaces)
     (let ((p (concat (file-name-sans-extension file) ".agdai")))
       (and (file-exists-p p) (list p)))
     ;; project-build layout: <root>/_build/<agda-version>/agda/<relpath>.agdai
     (file-expand-wildcards
      (concat root "/_build/*/agda/" rel-noext ".agdai")))))

(defun agda-fresh-holes--buffer-has-holes-p ()
  "Non-nil if the current buffer plausibly contains Agda holes.
Matches `{!' anywhere, or `?' standing alone between delimiters.
Overmatches a `?' in comments or strings; the only cost of a false
positive is one re-typecheck of this module."
  (save-excursion
    (goto-char (point-min))
    (or (search-forward "{!" nil t)
        (re-search-forward
         "\\(?:^\\|[][:space:](){};=,]\\)\\?\\(?:[][:space:](){};=,]\\|$\\)"
         nil t))))

(defun agda-fresh-holes-clear-interface (&rest _)
  "Delete this buffer's own .agdai when the buffer contains holes.
Intended as :before advice on `agda2-load', so a holey module is
always freshly elaborated (making its goals interactive) while
hole-free modules keep instant interface reuse."
  (when (and buffer-file-name
             (derived-mode-p 'agda2-mode)
             (agda-fresh-holes--buffer-has-holes-p))
    (dolist (agdai (agda-fresh-holes--agdai-paths buffer-file-name))
      (when (ignore-errors (delete-file agdai) t)
        (message "agda-fresh-holes: deleted %s (buffer has holes)" agdai)))))

(with-eval-after-load 'agda2-mode
  (advice-add 'agda2-load :before #'agda-fresh-holes-clear-interface))

(provide 'agda-fresh-holes)
;;; agda-fresh-holes.el ends here
