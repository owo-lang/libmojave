(*
 * Functional queue.
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

type 'a t =
  | Empty
  | Single of 'a
  | Full of 'a list(*hd*) * 'a list(*tl*)

let empty = Empty

let add x = function
  | Empty -> Single x
  | Single y -> Full([y;x],[])
  | Full(hd,tl) -> Full(hd,x::tl)

let is_empty = function
  | Empty | Full([],[]) -> true
  | _ -> false

let peek = function
  | Empty -> failwith "empty queue"
  | Single a -> a
  | Full([],[]) -> failwith "empty queue"
  | Full(a::_,_) -> a
  | Full([],tl) ->
      List.nth tl (pred (List.length tl))

let take = function
  | Empty -> failwith "empty queue"
  | Single x -> (x,Empty)
  | Full(a::hd,tl) -> (a,(Full(hd,tl)))
  | Full([],tl) ->
      match tl with
      |	[] -> failwith "empty queue"
      |	[a] -> (a,Empty)
      |	[b;a] -> (a,(Single b))
      |	tl ->
	  let hd = List.rev tl in
	  (List.hd hd),(Full((List.tl hd),[]))

let iter f = function
  | Empty -> ()
  | Single a -> f a
  | Full(hd,tl) ->
      List.iter f hd;
      List.iter f (List.rev tl)

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)