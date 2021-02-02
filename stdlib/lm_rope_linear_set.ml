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
 *)
open Lm_linear_set_sig

type 'a rope =
	 Ept (* Empty *)
 | Sgt of 'a (* singleton *)
 | Sub of { string : 'a array; beg : int; len : int } (* substring *)
 | Cat of { size : int;
            left : 'a rope; llen : int; right : 'a rope }

type 'a linear_set = 'a rope

module Make (Type : TypeSig) =
struct
   type elt = Type.t
   type t = Type.t rope
   type index = int

   let empty = Ept
   let singleton x = Sgt x

   let length = function
      Ept -> 0
    | Sgt _ -> 1
    | Sub { len = n; _ } | Cat { size = n; _ } -> n

   let make i e = Sub { string = Array.make i e; beg = 0; len = i }

   let rec iter f = function
      Ept -> ()
    | Sgt e -> f e
    | Sub s -> Lm_array_util.sub_iter f s.string s.beg s.len
    | Cat c -> iter f c.left; iter f c.right

   let rec map (f : 'a -> 'b) = function
      Ept -> Ept
    | Sgt e -> Sgt (f e)
    | Sub ({ string = s;
             beg = i;
             len = len } as sub) ->
         Sub {sub with string = Lm_array_util.sub_map f s i len;
                       beg = 0 }
    | Cat c -> Cat {c with left = map f c.left; right = map f c.right }

   let fold_sub_map f x beg len a =
      match len with
         0 -> x, [||]
       | 1 -> let x, y = f x (Array.unsafe_get a beg) in
                 x, Array.make 1 y
       | l -> let x, y = f x (Array.unsafe_get a beg) in
              let r1, r2 = ref x, Array.make l y in
                 for i = succ beg to beg+l-1 do
                    let x, y = f !r1 (Array.unsafe_get a i) in
                       Array.unsafe_set r2 i y;
                       r1 := x
                 done;
                 !r1, r2

   let rec fold_map f x = function
      Ept -> x, Ept
    | Sgt e -> let a, b = f x e in a, Sgt b
    | Sub s -> let a, b = fold_sub_map f x s.beg s.len s.string in
                  a, Sub { s with string = b; beg = 0 }
    | Cat c -> let a1, b1 = fold_map f x c.left in
               let a2, b2 = fold_map f a1 c.right in
                  a2, Cat { c with left = b1; right = b2 }

   let of_list l =
      let a = Array.of_list l in
         Sub { string = a; beg = 0; len = Array.length a }

   let sub_to_list acc a beg len =
      let rec tolist i res =
         if i < beg then res else tolist (i - 1) (Array.unsafe_get a i :: res) in
         tolist (beg + len - 1) acc

   let to_list =
      let rec aux acc = function
         Ept -> acc
       | Sgt e -> e :: acc
       | Sub s -> sub_to_list acc s.string s.beg s.len
       | Cat c -> aux (aux acc c.right) c.left
      in aux []

   (* TODO: add lazy map *)
   let lazy_apply = map

   let concat a b =
      match a, b with
         Ept, _ -> b
       | _, Ept -> a
       | _ ->
            let len_a = length a in
            let len_b = length b in
               Cat { size = len_a + len_b; left = a; llen = len_a; right = b }

   let append a e b = concat (concat a (Sgt e)) b

   let fold_sub f x i a beg len =
      let r = ref x in
         for j = 0 to pred len do
            r := f !r (i + j) (Array.unsafe_get a (j + len))
         done;
         !r, len + i

   let fold f x a =
      let rec aux x i = function
         Ept -> x, i
       | Sgt e -> f x i e, succ i
       | Sub s -> fold_sub f x i s.string s.beg s.len
       | Cat c -> let x', i' = aux x i c.left in
                     aux x' i' c.right
      in aux x 0 a

   let get = Array.get

   let split t ind =
      Array.sub t 0 ind, t.(ind), Array.sub t (succ ind) (Array.length t - ind - 1)

   let rec drop i = function
      Sub ({ beg = b; len = l; _ } as s) when l > i ->
         Sub { s with beg = b + i; len = l - 1 }
    | Sub { len = l; _ } when l = i ->
         Ept
    | Cat ({ llen = l; left = lt; size = s; _ } as c) when l > i ->
         Cat { c with llen = l - i; left = drop i lt;
                      size = s - i }
    | Cat ({ llen = l; right = rt; _ } as c) when l = i ->
         rt
    | Cat { llen = l; size = s; right = rt; _ } when s > i ->
         drop (i - l) rt
    | Sgt _ when i = 0 -> Ept
    | _ -> invalid_arg "Lm_rope.drop"


(*   let lazy_sub_map = Lm_array_util.sub_map *)
   let append_list a l b = concat (concat a (of_list l)) b

   let mapi = Array.mapi

   let init i f =
      let a = Array.init i f in
         Sub { string = a; beg = 0; len = Array.length a }

   let collect l =
      let a = Lm_array_util.collect l in
         Sub { string = a; beg = 0; len = Array.length a }

   let for_sub p a beg len =
      let n = beg + len in
      let rec loop i =
         if i = n then true
         else if p (Array.unsafe_get a i) then loop (succ i)
         else false in
         loop beg

   let rec for_all f = function
      Ept -> true
    | Sgt e -> f e
    | Sub s -> for_sub f s.string s.beg s.len
    | Cat { left = lt; right = rt; _ } ->
         for_all f lt && for_all f rt

   (* let rec exists f = Array.exists *)
end
