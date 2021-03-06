(*
 * Extra functions on hashtables.
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
 * Add all the entries from the second table.
 * The original entries are removed.
 *)
let add_hashtbl dst src =
   Hashtbl.iter (fun key value ->
         Hashtbl.replace dst key value) src

(*
 * Update entry.
 *)
let update tbl key f d =
   match Hashtbl.find_opt tbl key with
      Some e -> Hashtbl.replace tbl key (f e)
    | None -> Hashtbl.add tbl key (f d)

let update_opt tbl key f =
   Hashtbl.replace tbl key (f (Hashtbl.find_opt tbl key))

let update_if tbl key f =
   match Hashtbl.find_opt tbl key with
      Some e -> Hashtbl.replace tbl key (f e)
    | None -> ()

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
