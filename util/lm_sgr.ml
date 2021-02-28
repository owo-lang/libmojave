(*
 * SELECT GRAPHIC RENDITION
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
 *)

type param = int * int (* on * off *)

type rendition =
   { mutable cache : int;
     mutable old : int list
  (* mutable fg : int;
     mutable bg : int *)
   }

let new_rend () = { cache = 0; old = [] }

let add (a, b) n = (lnot b) land (a lor n)

(*
 * Font toggles
 *)
let bold = 1 (* 1 *)
let dim = 2 (* 2 *)
let italic = 4 (* 3 *)
let underline = 8 (* 4 *)
(*
let blink = 16 ;; 5
let inverse = 32 ;; 7
let rm = 64
*)

(* let only x = x, lnot x *)
let on x = x, 0
let off x = 0, x

(* XXX: HACK: These features may not supported by
 * a particular terminal type.
 *)
let bb = on bold
let tt = off (italic + bold + dim)
let it = on italic
let rm = off 63
let sm = on dim
let ul = on underline

let code = [| '1' ; '2' ; '3'; '4' ; '5' ; '7' |]

let sgr =
   let buf = Buffer.create 19 in
   let () = Buffer.add_string buf "\027[0" in
   let rec aux i =
      if i > 0 then
      begin
         Buffer.add_char buf ';';
         Buffer.add_char buf (Array.get code (Lm_int_util.ctz i));
         aux (i land (i - 1)) (* rightmost bit off *)
      end
   in (fun i ->
            Buffer.truncate buf 3;
            aux i;
            Buffer.add_char buf 'm';
            Buffer.contents buf)

let sgr' i = Printf.sprintf "\027[%cm" (Array.get code (Lm_int_util.ctz i))

(* TODO: add TERM detection *)

let push p ({ cache = cache; old = old } as r) =
   r.old <- cache :: old;
   let nv = add p cache in
      r.cache <- nv;
      if cache == nv
      then None
      else Some (if nv == (nv lor cache)
                 then sgr' (nv - cache)
                 else sgr nv)

let pop ({ cache = cache; old = old } as r) =
   match old with
      a :: b -> r.cache <- a;
                r.old <- b;
                if a == cache
                then None
                else Some (sgr a)
    | _ -> invalid_arg "Lm_sgr.pop: underflow"

let bold_text t = Printf.sprintf "\027[0;1m%s\027[0m" t

let xterm_title t = Printf.sprintf "\027]0;%s\007" t

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)