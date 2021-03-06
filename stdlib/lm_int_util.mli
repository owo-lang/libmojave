(*
 * Some utility functions on integers.
 *
 * ----------------------------------------------------------------
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
 * Computing log base 2 of a number.
 * Rounds down.
 *)
external log2 : int -> int = "lm_ilog2_byte" "lm_ilog2" [@@untagged] [@@noalloc]

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
val clp2 : int -> int

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
