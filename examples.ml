open Core
open Grammar
open Chars
open Utilities
open Unicode

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

let cat_string = List.map (String.to_list "purry cat says cat cat") ~f: (function x -> Intermediary (Intermedial x))

let cat_rewrite: (char, char) rewrite_rule =
  { source =
      ( Intermediary (Intermedial 'c')
      , [ Intermediary (Intermedial 'a')
        ; Intermediary (Intermedial 't')
        ]
      )
  ; target =
      [ Intermediary (Intermedial 'p')
      ; Intermediary (Intermedial 'u')
      ;  Intermediary (Intermedial 'r')
      ;  Intermediary (Intermedial 'r')
      ]
  }

let cat_graphemes = sf "PURRY CAT SAYS CAT CAT"

let cat_rewrite_concise: (grapheme, grapheme) rewrite_rule = { source = uncons_exn (sf "CAT"); target = sf "purr" }
