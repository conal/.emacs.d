;;-*-coding: utf-8;-*-
(define-abbrev-table 'agda2-mode-abbrev-table
  '(
    ("?go?" "
    begin
      ?
    ≡⟨⟩
      ?
    ≈⟨ ? ⟩
      ?
    ≡⟨⟩
      ?
    ∎" nil :count 6)
   ))

(define-abbrev-table 'c++-mode-abbrev-table
  '(
    ("catch" "catch" c-electric-continued-statement :count 0)
    ("else" "else" c-electric-continued-statement :count 0)
    ("while" "while" c-electric-continued-statement :count 0)
   ))

(define-abbrev-table 'c-mode-abbrev-table
  '(
    ("else" "else" c-electric-continued-statement :count 0)
    ("while" "while" c-electric-continued-statement :count 0)
   ))

