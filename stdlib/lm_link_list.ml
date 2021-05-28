(*
 * Sequence with optimized insert & delete.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2020 LdBeth
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

type 'a llist =
   Node of { elt : 'a;
             mutable pred : 'a llist;
             mutable next : 'a llist
   }
 | Nil

let empty = Nil

let insert a = function
   (Node n) as p ->
       let node = Node { elt = a; pred = p; next = n.next } in
         n.next <- node;
         node
 | Nil ->
      Node { elt = a; pred = Nil; next = Nil }

let delete = function
   Node { pred = (Node n) as p; elt = a; next = next } ->
      n.next <- next;
      p, a
 | Node { pred = Nil; elt = a; next = next } ->
      next, a
 | Nil -> raise Not_found

let pred = function
   Node { pred = (Node _) as p; _ } -> p
 | _ -> raise Not_found

let next = function
   Node { next = (Node _) as n; _ } -> n
 | _ -> raise Not_found

let rec copy = function
   Node n ->
      Node { n with pred = copy n.pred; next = copy n.next }
 | Nil ->
      Nil

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)