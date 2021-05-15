(defun perhaps-expand-abbrev ()
  "Do expand-abbrev if abbrev-mode is true, but not if we're inside a word."
  (interactive)
  (when (and abbrev-mode
             (looking-at "\\>")
             )
    (expand-abbrev))
  )
