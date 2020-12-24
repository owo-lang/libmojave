(*
 * Stack with O(1) element access.
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

type 'a stack =
   {
      mutable old : 'a array;
      mutable cur : 'a array;
      mutable size : int;
      mutable i : int;
      mutable j : int
}

let make n e =
   let ns = Lm_int_util.clp2 n in
      { old = Array.make 0 e;
        cur = Array.make ns e;
        size = ns;
        i = 0;
        j = n }

let length s = s.j

let get s n =
   if n < s.i
   then Array.get s.old n
   else if n < s.j
   then Array.get s.cur n
   else invalid_arg "Lm_stack.get: out of bound"

let set s n e =
   if n < s.i
   then Array.set s.old n e
   else if n < s.j
   then Array.set s.cur n e
   else invalid_arg "Lm_stack.get: out of bound"

let push stack e =
   let { cur = ca; size = s;
         i = i; j = j; _ } = stack in
   let copy_old i =
      let n = pred i in
         Array.set stack.cur n (Array.get stack.old n);
         stack.i <- n
   in
      if j < s
      then begin
         Array.set ca j e;
         if i > 0 then copy_old i
      end
      else begin (* expand *)
         let ns = s * 2 in
            stack.old <- ca;
            stack.cur <- Array.make ns e;
            stack.size <- ns;
            copy_old j
      end;
      stack.j <- succ j;

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)