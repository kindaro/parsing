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

let generate (grammar: ('a, 'b) grammar): ('a, 'b) symbol list Set.Poly.t Sequence.t =
  let start_symbol = grammar |> expand_grammar |> snd
  and explode_set (sentential_forms: ('a, 'b) symbol list Set.Poly.t): ('a, 'b) symbol list Set.Poly.t =
    sentential_forms |> Set.Poly.to_list
    |> List.map ~f: (explode_sentential_form_with_grammar ~grammar: grammar) |> Set.Poly.union_list
  in let new_sentential_forms previous_sentential_forms =
       Set.Poly.diff (explode_set previous_sentential_forms) previous_sentential_forms
  and initial_sentence = Set.Poly.singleton [Intermediary start_symbol]
  in let step xs = let ys = new_sentential_forms xs in
       if Set.is_empty ys
       then None
       else Some (ys, Set.Poly.union xs ys)
  in Sequence.unfold ~init: initial_sentence ~f: step
