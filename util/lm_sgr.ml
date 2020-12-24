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
let diff a b =
   let xor = a lxor b in
      a land xor, b land xor
      *)

(*
 * Font toggles
 *)
let bold = 1 (* 1 *)
let dim = 2 (* 2 *)
let italic = 4 (* 3 *)
let underline = 8 (* 4 *)
let blink = 16 (* 5 *)
let inverse = 32 (* 7 *)

let only x = x, lnot x
let on x = x, 0
let off x = 0, x

(* XXX: HACK: These features may not supported by
 * a particular terminal type.
 *)
let bf = only bold
let bb = on bold
let tt = off (italic + bold + dim)
let it = only italic
let em = on italic
let rm = off 63
let sm = on dim
let ul = on underline

(* let sgr l =
   let args = String.concat ";" (List.map Int.to_string l) in
   "\027[" ^ args ^"m"

  let sgr0 = "\027[m" *)

let get i f = i land f <> 0

let sgr i =
   "\027[0"
   ^ (if (get i bold) then ";1" else "")
   ^ (if (get i dim) then ";2" else "")
   ^ (if (get i italic) then ";3" else "")
   ^ (if (get i underline) then ";4" else "")
   ^ (if (get i blink) then ";5" else "")
   ^ (if (get i inverse) then ";7" else "")
   ^ "m"

(* TODO: add TERM detection *)

let push p ({ cache = cache; old = old } as r) =
   r.old <- cache :: old;
   let nv = add p cache in
      r.cache <- nv;
      Some (sgr nv)

let pop ({ cache = cache; old = old } as r) =
   match old with
      a :: b -> r.cache <- a;
                r.old <- b;
                Some (sgr a)
    | _ -> invalid_arg "Lm_sgr.pop: underflow"


(*  f oo *)