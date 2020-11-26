(*
 * Compatibility layer to Zarith library.
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
 * Copyright (C) 1998-2005 MetaPRL Group, Cornell University,
 * California Institute of Technology, and City University of New York
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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified by: Yegor Bryukhov <ybryukhov@gc.cuny.edu>
 * Modified by: Aleksey Nogin <nogin@cs.caltech.edu>
 *)

(* Uses Zarith *)

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

type num = Z.t

(************************************************************************
 * IMPLEMENTATIONS                                                      *
 ************************************************************************)

(*
 * Construction.
 *)
let zero_num = Z.zero
let one_num = Z.one
let minus_one_num = Z.minus_one

(*
 * Arithmetics.
 *)
let add_num = Z.add
let sub_num = Z.sub
let succ_num = Z.succ
let pred_num = Z.pred
let mult_num = Z.mul
let div_num = Z.div
let rem_num = Z.rem
let quo_num = div_num
let mod_num = rem_num

(*
 * Rounding towards -oo (floor), used by Itt_supinf.
 *)
let fdiv_num = Z.fdiv

(*
 * Euclidean division and remainder, used by Itt_omega.
 *)
let ediv_num = Z.ediv
let erem_num = Z.erem

let gcd_num = Z.gcd

(*
 * Power.
 *)
let power_num = Z.pow

(*
 * Absolute value.
 *)
let abs_num = Z.abs
let neg_num = Z.neg
let sign_num a = Z.of_int (Z.sign a)

(*
 * Equality.
 *)
let eq_num = Z.equal
let compare_num = Z.compare

let lt_num = Z.lt
let le_num = Z.leq
let gt_num = Z.gt
let ge_num = Z.geq

let is_zero n = Z.sign n == 0
let is_pos n = Z.sign n == 1
let is_neg n = Z.sign n == -1

(************************************************************************
 * CONVERSION                                                           *
 ************************************************************************)

(*
 * Integer conversions.
 *)
let is_integer_num = Z.fits_int
let integer_num = Z.to_int
let num_of_int = Z.of_int
let int_of_num = integer_num

(*
 * String conversions.
 *)
let to_string = Z.to_string
let of_string = Z.of_string

let string_of_num = to_string
let num_of_string = of_string

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
