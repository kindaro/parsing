
open Grammar

let cat_intermedial = Intermedial "cat"

let cat_terminal = Terminal "purr"

  let cat_rule_group: (string, string) context_free_rule_group =
  { source = cat_intermedial
      ; target =
      [ [Terminary cat_terminal]
        ; [ Intermediary cat_intermedial
; Intermediary cat_intermedial ] ] }

  let cat_grammar =
  let cat = cat_intermedial
  and purr = Terminary cat_terminal
  and meow = Terminary (Terminal "meow")
  in
  { rules = [ ]
      ; start =
      { source = cat
          ; target =
          [ [purr]
          ; [meow]
          ; [Intermediary cat; Intermediary cat] ] } }
