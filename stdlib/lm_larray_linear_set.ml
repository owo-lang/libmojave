(*
 * This module provides a linearly ordered numbered set implementation
 * with lazy functions based on arrays
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
 * Copyright (C) 1998-2005 PRL Group, Cornell University and Caltech
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
 * Author: Alexey Nogin
 * Modified by: LdBeth
 *)
open Lm_linear_set_sig

type 'a linear_set = ('a Lazy.t) array

module Make (Type : TypeSig) =
struct
   type elt = Type.t
   type t = Type.t linear_set
   type index = int

   let empty = [||]
   let singleton x = [|Lazy.from_val x|]
   let length = Array.length
   let make i e = Array.make i (Lazy.from_val e)

   let force_f f = fun e -> f (Lazy.force_val e)
   let force_g g = fun e -> Lazy.from_val (g (Lazy.force_val e))
   let force_h h = fun a b -> let x, y = h a (Lazy.force_val b)
                              in x, Lazy.from_val y
   let lazy_f f = fun e -> lazy (f (Lazy.force_val e))

   let iter f = Array.iter (force_f f)
   let map g = Array.map (force_g g)
   let fold_map h = Lm_array_util.fold_map (force_h h)
   let of_list l = Array.map Lazy.from_val (Array.of_list l)
   let to_list a = Array.to_list (Array.map Lazy.force_val a)
   let lazy_apply f = Array.map (lazy_f f)

   let concat = Array.append

   let append a1 e a2 =
      let l1 = length a1 and l2 = length a2 in
      if l1 = 0 && l2 = 0 then singleton e else begin
         let r = make (succ l1 + l2) e in
         Array.blit a1 0 r 0 l1;
         Array.blit a2 0 r (succ l1) l2;
         r
      end

   let fold f x a =
      let r = ref x in
         for i = 0 to Array.length a - 1 do
            r := f !r i (Lazy.force_val (Array.unsafe_get a i))
         done;
         !r

   let get a n = Lazy.force_val (Array.get a n)

   let split t ind =
      Array.sub t 0 ind, Lazy.force_val t.(ind), Array.sub t (succ ind) (Array.length t - ind - 1)

   let drop ind t =
      Array.sub t ind (Array.length t - ind)

   let lazy_sub_map f = Lm_array_util.sub_map (lazy_f f)
   let append_list a l = Lm_array_util.append_list_array a (List.map Lazy.from_val l)
   let mapi g = Array.mapi (fun i e -> Lazy.from_val (g i (Lazy.force_val e)))
   let init i f = Array.init i (fun i -> Lazy.from_val (f i))
   let collect p = Lm_array_util.collect
                   (List.map (function
                       Lm_array_util.ArrayElement a -> Lm_array_util.ArrayElement (Lazy.from_val a)
                     | Lm_array_util.ArrayArray(b,n,m) -> Lm_array_util.ArrayArray(b,n,m)) p)

   let for_all f = Array.for_all (force_f f)
   let exists f = Array.exists (force_f f)
end
