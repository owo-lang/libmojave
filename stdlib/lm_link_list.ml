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
             mutable prev : 'a llist;
             mutable next : 'a llist
   }
 | Nil

let empty = Nil

(*
type 'a t =
   { mutable head : 'a llist;
     mutable node : 'a llist }

let create () =
   let nil = Nil in
      { head = nil; node = nil }
*)

let insert a = function
   (Node n) as p ->
      let node = Node { elt = a; prev = p; next = n.next } in
         begin
            match n.next with
               Node n'-> n'.prev <- node
             | _ -> raise (Failure "impossible")
         end;
         n.next <- node;
         node
 | Nil ->
      let node = Node { elt = a; prev = Nil; next = Nil } in
      (* make it circular *)
         match node with
            Node n ->
               n.prev <- node;
               n.next <- node;
               node
          | _ -> raise (Failure "impossible")

let is_single = function
   Node { prev = p; next = n; _ } when p == n ->
      true
 | Nil -> true
 | _ -> false

let delete = function
   (Node { prev = p; elt = a; _ } as self) when p == self ->
      Nil, a
 | Node { prev = (Node n) as p; elt = a; next = next } ->
      n.next <- next;
      p, a
 | _ -> raise Not_found

let top = function
   Node { elt = e; _ } -> e
 | _ -> raise Not_found

let prev = function
   Node { prev = (Node _) as p; _ } -> p
 | _ -> invalid_arg "list is not circular"

let next = function
   Node { next = (Node _) as n; _ } -> n
 | _ -> invalid_arg "list is not circular"

let copy a =
   match a with
      Nil -> Nil
    | Node n -> let init = Node { elt = n.elt; prev = Nil; next = Nil } in
                let rec aux prev node =
                   match prev, node with
                      Node prev', Node node' ->
                         if node == a then
                         begin
                            prev'.next <- init;
                            match init with
                               Node init' ->
                                  init'.prev <- prev
                             | _ -> raise (Failure "impossible")
                         end
                         else
                         begin
                            match node with
                               Node n ->
                                  let new_node = Node { n with prev = prev;
                                                               next = Nil }
                                  in prev'.next <- new_node;
                                     aux new_node (node'.next)
                             | Nil -> invalid_arg "list is not circular"
                         end
                    | _ -> raise (Failure "impossible")
                in aux init n.next;
                   init


(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)