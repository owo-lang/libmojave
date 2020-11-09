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

(* binary search on integer
let fill_right_bits x = x lxor (x - 1)

let bit_mask n =
   let x = 1 lsl (pred n) in
      fill_right_bits x *)

let rightmost_bit offset x = offset + Lm_int_util.ctz x

let find_first a =
   let bound = Array.length a - 1 in
   let rec find n =
      if n > bound then raise Not_found else
      if a.(n) > 0
      then rightmost_bit (n * int_size) a.(n)
      else find (succ n)
   in find 0

let is_empty = Array.for_all (fun n -> n = 0)


(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
