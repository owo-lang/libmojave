(*
 * Bitset.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2000-2005 Mojave Group, Caltech
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
 * Author: Jason Hickey
 * jyh@cs.caltech.edu
 *)

(*
 * Packed boolean array.
 *)

let int_size = 62

type t = int array

let create len =
   Array.make ((len + int_size - 1) / int_size) 0

let copy = Array.copy

let set bits i =
   let index = i / int_size in
   let bit = i mod int_size in
      bits.(index) <- bits.(index) lor (1 lsl bit)

let reset bits i =
   let index = i / int_size in
   let bit = i mod int_size in
      bits.(index) <- bits.(index) land (lnot (1 lsl bit))

let get bits i =
   let index = i / int_size in
   let bit = i mod int_size in
      (bits.(index) land (1 lsl bit)) <> 0

let union a b =
   let alen, blen = Array.length a, Array.length b in
   let sl, bits = if alen > blen
                  then alen, Array.copy a
                  else blen, Array.copy b in
      for i = 0 to sl-1 do
         bits.(i) <- a.(i) lor b.(i)
      done;
      bits

let inter a b =
   let l_a, l_b = Array.length a, Array.length b in
   let max_s = max l_a l_b in
   let sl = min l_a l_b in
   let bits = Array.make max_s 0 in
      for i = 0 to sl-1 do
         bits.(i) <- a.(i) land b.(i)
      done;
      bits

(* binary search on integer
let fill_right_bits x = x lxor (x - 1)

let bit_mask n =
   let x = 1 lsl (pred n) in
      fill_right_bits x *)

let count = Array.fold_left (fun x y -> x + Lm_int_util.cnt y) 0

let rightmost_bit offset x = (offset * int_size) + Lm_int_util.ctz x
let rightmost_off x = x land (x - 1)

let iter f a =
   let b = Array.length a in
   let rec inner n x =
      if x > 0 then
         (f (rightmost_bit n x);
          inner n (rightmost_off x)) in
   let rec aux n =
      if n < b then
         (inner n a.(n);
          aux (succ n))
   in aux 0

let is_empty = Array.for_all (fun n -> n == 0)

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
