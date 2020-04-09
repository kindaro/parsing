open Core
open List

let rec unfold x ~f = match f x with
  | None -> [ ]
  | Some (y, x') -> y :: unfold ~f: f x'

let rec iterate (x: 'a) ~(f): 'a list = match f x with
  | None -> [ ]
  | Some y -> y :: iterate ~f: f y

let unfold' f x = let g (_y, x') = f x' in let yxs = iterate ~f: g (Obj.magic (), x) in map yxs ~f: fst

let final ~(f: 'a -> ('b * 'a) option) (x: 'a): 'a option = Option.map ~f: fst (last (unfold x ~f: f))

let tails xs = xs :: iterate xs ~f: tl

let uncons_exn (xs: 'a list): ('a * 'a list) = match xs with
  | [ ] -> failwith "Cannot uncons an empty list."
  | (x :: xs) -> (x, xs)
