open Core
open Poly
open List
open Utilities

type 'a terminal = Terminal of 'a

type 'b intermedial = Intermedial of 'b

type ('a, 'b) symbol = Terminary of 'a terminal | Intermediary of 'b intermedial

type ('a, 'b) rewrite_rule =
  { source: ('a, 'b) symbol * ('a, 'b) symbol list
  ; target: ('a, 'b) symbol list }

type ('a, 'b) context_free_rule_group =
  { source: 'a intermedial
  ; target: ('a, 'b) symbol list list }

type ('a, 'b) grammar =
  { rules: ('a, 'b) rewrite_rule list
  ; start: ('a, 'b) context_free_rule_group }

(*

This way to define a grammar is isomorphic to the traditional quadruple ⟨non-terminals, terminals, rules, start symbol⟩ but enforces the invariants on the type level, at the cost of some processing complexity.

* I am calling non-terminals _«intermedials»_ to add perceptual distinctiveness.
* There are two kinds of rules: rewrite and context-free. While the former are more general, the latter necessarily occur even in context-sensitive grammars — as transformations of the start symbol.
* For a rewrite rule, the source is a non-empty list, as opposed to a simple list.
* The source of a context-free rule is always a single intermediary. Context-free rules with the same source must be grouped.
* A grammar definition has exactly one dedicated context-free rule that defines the start symbol.

The sets of terminals and intermediaries can be derived by scanning the rule set. The start symbol is defined by the dedicated context-free rule group. The rule should then be added to the other rules.

*)

let expand_context_free_rule_group (u: ('a, 'b) context_free_rule_group): ('a, 'b) rewrite_rule list =
  let f x: ('a, 'b) rewrite_rule  = {source = (Intermediary u.source, [ ]); target = x} in map u.target ~f: f

let expand_grammar (u: ('a, 'b) grammar): ('a, 'b) rewrite_rule list * 'b intermedial =
  let rules = append u.rules (expand_context_free_rule_group u.start) in (rules, u.start.source)

type 'a cut =
  { offset: int (* The cut remembers its location in the context. *)
  ; selection: 'a list (* Kept reversed. It is essentially a zipper. *)
  ; trailing: 'a list }

let apply_rewrite_rule (rule: ('a, 'b) rewrite_rule) sentential_form =
  let select pattern sentential_form =
    let initial_selections = map2_exn
        (range ~stop: `inclusive 0 (length sentential_form))
        (tails sentential_form)
        ~f: (fun offset xs -> { offset = offset ; selection = [ ] ; trailing = xs })
    and select_step (pattern, selections) = if is_empty selections
      then None
      else match pattern with
        | [ ] -> None
        | (u :: us) ->
          let maybe_admit_one u ~is_admissible = match u.trailing with
            | [ ] -> None
            | (x :: xs) -> if is_admissible x then Some {u with selection = x :: u.selection; trailing = xs} else None
          in Some (us, filter_map selections ~f: (maybe_admit_one ~is_admissible: (function x -> x = u)))
    in iterate (pattern, initial_selections) ~f: select_step
       |> last |> Option.value ~default: ([ ], [ ]) |> snd
  and replace sentential_form ~cut ~replacement = append (take sentential_form cut.offset) (append replacement cut.trailing)
  in let selections = select (fst rule.source :: snd rule.source) sentential_form in
  map selections ~f: (fun x -> replace sentential_form ~cut: x ~replacement: rule.target)

let is_free_of_intermediaries (sentential_form: ('a, 'b) symbol list)
  : bool = List.for_all ~f: (function x -> match x with Intermediary _ -> false | Terminary _ -> true) sentential_form
