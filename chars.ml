open Core

open Utilities
open Grammar
open Unicode

let string_to_sentential_form (s: string): (grapheme, grapheme) symbol list =
  let convert_to_symbol (x: grapheme): (grapheme, grapheme) symbol =
    if is_any_upper_case x
    then Intermediary (Intermedial x)
    else Terminary (Terminal x)
  in s |> graphemes |> List.map ~f: convert_to_symbol

let sf = string_to_sentential_form

let sentential_form_to_string (xs: (grapheme, grapheme) symbol list): string =
  let convert_from_symbol (x: (grapheme, grapheme) symbol): grapheme = match x with
    | Intermediary (Intermedial y) -> y
    | Terminary (Terminal y) -> y
  in xs |> List.map ~f: convert_from_symbol |> List.map ~f: grapheme_to_string |> String.concat

let fs = sentential_form_to_string

let rule (source: string) (target: string): (grapheme, grapheme) rewrite_rule = {source = uncons_exn (sf source); target = sf target}

let char (source: string) = List.hd_exn (graphemes source)

let cf_rules (source: string) (targets: string list): ((grapheme, grapheme) context_free_rule_group) =
  {source = Intermedial (char source); target = List.map ~f: (fun s -> sf s) targets}
