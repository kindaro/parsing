open Core

type grapheme = Grapheme of Uchar.t list

let string_to_chars (s: string): Uchar.t list =
  let decoder = Uutf.decoder (`String s)
  in let rec decode_string (us: Uchar.t list): Uchar.t list =
       match Uutf.decode decoder with
       | `Uchar u -> decode_string (u :: us)
       | `End -> us
       | _ -> failwith "Some other constructor."
  in decode_string [ ]

let chars_to_string (us: Uchar.t list): string =
  let temporary_buffer = Buffer.create 16
  in let encoder = Uutf.encoder (`UTF_8) (`Buffer temporary_buffer)
  in let rec encode_chars (chars: Uchar.t list): unit =
       match chars with
       | [ ] -> let _ = Uutf.encode encoder `End in ()
       | (u :: us) -> let _ = Uutf.encode encoder (`Uchar u) in encode_chars us
  in let () = encode_chars (List.rev us) in Buffer.contents temporary_buffer
