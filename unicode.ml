open Core

type grapheme = Grapheme of Uchar.t list

let string_to_chars (s: string): Uchar.t list =
  let decoder = Uutf.decoder (`String s)
  in let rec decode_string (us: Uchar.t list): Uchar.t list =
       match Uutf.decode decoder with
       | `Uchar u -> decode_string (u :: us)
       | `End -> us
       | `Await -> failwith "Await is only emitted with Manual source."
       | `Malformed _ -> decode_string (Uutf.u_rep :: us)
  in List.rev (decode_string [ ])

let normalize_chars normalization_form (us: Uchar.t list): Uchar.t list =
  let transcoder = Uunf.create normalization_form
  in let rec roundtrip' (ws: Uchar.t list) (x: Uunf.ret): Uchar.t list =
       match Uunf.add transcoder x with
       | `Await | `End -> ws
       | `Uchar w -> roundtrip' (w :: ws) `Await
  in let roundtrip = roundtrip' [ ]
  in let rec transcode' (ws: Uchar.t list) (us: Uchar.t list): Uchar.t list =
       match us with
       | [ ] -> let ys = roundtrip `End in List.append ys ws
       | u :: us -> let ys = roundtrip (`Uchar u) in transcode' (List.append ys ws) us
  in let transcode = transcode' [ ]
  in List.rev (transcode us)

let chars_to_graphemes (us: Uchar.t list): (grapheme list * (Uchar.t list * Uchar.t list)) =
  let transcoder = Uuseg.create `Grapheme_cluster
  in let rec roundtrip'
      (just_started: bool)
      (*Whether to ignore the first grapheme boundary. Used to avoid an empty grapheme at the
        beginning of a string. *)
      (graphemes: grapheme list)
      (prefix: Uchar.t list)
      (remainder: Uchar.t list)
      (x)
    : (grapheme list * (Uchar.t list * Uchar.t list))
    (* Graphemes, then prefix and remainder of characters that do not form a complete grapheme.
       Prefix can only occur when just started. *)
    = match Uuseg.add transcoder x with
    | `Await | `End -> (graphemes, (prefix, remainder))
    | `Boundary -> if just_started
      then roundtrip' false graphemes remainder [ ] `Await
      else roundtrip' false (Grapheme (List.rev remainder) :: graphemes) prefix [ ] `Await
    | `Uchar w -> roundtrip' just_started graphemes prefix (w :: remainder) `Await
  in let roundtrip just_started = roundtrip' just_started [] [ ]
  in let rec transcode' just_started (xs: grapheme list)
      (prefix: Uchar.t list) (remainder: Uchar.t list) (us: Uchar.t list):
    (grapheme list * (Uchar.t list * Uchar.t list)) = match us with
      | [ ] -> let (ys, (prefix', remainder)) = roundtrip just_started remainder `End
        in if just_started
        then (List.rev (List.append ys xs), (List.rev prefix', List.rev remainder))
        else (List.rev (List.append ys xs), (List.rev prefix, List.rev remainder))
      | u :: us -> let (ys, (prefix', remainder)) = roundtrip just_started remainder (`Uchar u)
        in if just_started
        then transcode' false (List.append ys xs) prefix' remainder us
        else transcode' false (List.append ys xs) prefix remainder us
  in let transcode = transcode' true [ ] [ ] [ ]
  in transcode us

let chars_to_string (us: Uchar.t list): string =
  let temporary_buffer = Buffer.create 16
  in let encoder = Uutf.encoder (`UTF_8) (`Buffer temporary_buffer)
  in let push_one (x): unit =
       match Uutf.encode encoder x with
         | `Ok -> ()
         | `Partial -> failwith "Partial is only returned with Manual destination."
  in let rec encode_chars (chars: Uchar.t list): unit =
       match chars with
       | (u :: us) -> let () = push_one (`Uchar u) in encode_chars us
       | [ ] -> let () = push_one `End in ()
  in let () = encode_chars us in Buffer.contents temporary_buffer

let grapheme_to_string (Grapheme us): string = chars_to_string us
