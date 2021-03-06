(*
 * Some utility functions on integers.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
 *
 * Copyright (C) 1999-2005 PRL Group, Cornell University and Caltech
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
 * jyh@cs.cornell.edu
 *)

(*
 * Count trailing zeros. Undefined if argument is zero.
 *)
external ctz : int -> int = "lm_ctz_byte" "lm_ctz" [@@untagged] [@@noalloc]

(*
 * Compute log base 2, rounds down.
 * Return -1 if n <= 0.
 *)
external log2 : int -> int = "lm_ilog2_byte" "lm_ilog2" [@@untagged] [@@noalloc]

(* The slow algorithm
let log2 =
   let rec search j i =
      if 1 lsl j > i then
         pred j
      else
         search (succ j) i
   in
      search 0
 *)

(*
 * Count ones in bitset.
 *)
external cnt : int -> int = "lm_cnt_byte" "lm_cnt" [@@untagged] [@@noalloc]

(*
 * Integer square root. Unfdefined if argument is negative.
 *)
external sqrt : int -> int = "lm_sqrt_byte" "lm_sqrt" [@@untagged] [@@noalloc]

(*
 * Rounding up to next power of 2, minimal 1
 *)
let clp2 n = Int.shift_left 1 (succ (log2 (pred n)))

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
