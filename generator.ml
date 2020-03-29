open Core
open Utilities
open List
open Poly

open Grammar

type 'a cut =
  { offset: int (* The cut remembers its location in the context. *)
  ; selection: 'a list (* Kept reversed. It is essentially a zipper. *)
  ; trailing: 'a list }

let apply_rewrite_rule (rule: ('a, 'b) rewrite_rule) sentential_form =
  let select pattern sentential_form =
    let initial_selections = map2_exn
        (range 0 (length sentential_form))
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
