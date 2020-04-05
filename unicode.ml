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

let transcode_chars normalization_form (us: Uchar.t list): Uchar.t list =
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
