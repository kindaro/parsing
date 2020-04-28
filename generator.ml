open Core
open List

open Grammar

let explode_sentential_form_with_rule ~rule sentential_form
    : (('a, 'b) symbol list) Set.Poly.t = sentential_form |> apply_rewrite_rule rule |> Set.Poly.of_list

let explode_sentential_form_with_grammar
    ~(grammar: ('a, 'b) grammar)
    (sentential_form: ('a, 'b) symbol list)
  : (('a, 'b) symbol list) Set.Poly.t =
  let rules = grammar |> expand_grammar |> fst
  in map rules ~f: (fun rule -> explode_sentential_form_with_rule ~rule: rule sentential_form) |> Set.Poly.union_list
