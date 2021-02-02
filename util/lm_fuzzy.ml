(*
 * Fuzzy string match
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2021 LdBeth
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 * Additional permission is given to link this library with the
 * OpenSSL project's "OpenSSL" library, and with the OCaml runtime,
 * and you may distribute the linked executables.  See the file
 * LICENSE.libmojave for more details.
 *
 * Author: LdBeth
 * ldbeth@sdf.org
 *)

let size = Lm_bitset.count

type kind = Alpha of int | Digit of int | Other

let encode c = match c with
   'A'..'Z' -> Alpha (Char.code c - 65)
 | 'a'..'z' -> Alpha (Char.code c - 97)
 | '0'..'9' -> Digit (628 + Char.code c) (* 676-48 *)
 | _ -> Other

let bigram s =
   let g = Lm_bitset.create 686 in (* 26*26+10 *)
   let l = String.length s - 2 in
      for i = 0 to l do
         match encode s.[i], encode s.[i+1] with
            Alpha a, Alpha b -> Lm_bitset.set g (26*a+b)
          | Digit a, Alpha _
          | Alpha _, Digit a -> Lm_bitset.set g a
          | Digit a, Digit b -> Lm_bitset.set g a; Lm_bitset.set g b
          | Other, _ | _, Other -> ()
      done;
      g

let dice_coeff a b =
   let bi_a, bi_b = bigram a, bigram b in
   let inter_s = size (Lm_bitset.inter bi_a bi_b) in
      (2.0 *. Float.of_int inter_s) /. Float.of_int ((size bi_a) + (size bi_b))

let levenshtein ?(subst=1) ?(insdel=1) a b =
   let len_a, len_b = String.length a, String.length b in
   let arr1 = Array.init (succ len_a) (fun x -> x * insdel) in
   let arr2 = Array.make (succ len_a) 0 in
   let rec aux n a1 a2 =
      if n < len_b then begin
         Array.set a2 0 (succ n * insdel);
         for i = 1 to len_a do
            let cost = if b.[n] = a.[i-1] then 0 else subst in
               Array.set a2 i (min (insdel + Array.get a2 (i-1))
                               (min (insdel + Array.get a1 i)
                                (cost + Array.get a1 (i-1))))
         done;
         aux (succ n) a2 a1
      end
      else Array.get a1 len_a
   in aux 0 arr1 arr2


(* test
let rate xs s n =
   List.map (fun x -> (dice_coeff s x, x)) xs
   |> List.sort (fun (a,_) (b,_) -> Int.neg (compare a b))
   |> Lm_list_util.firstn n *)

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)