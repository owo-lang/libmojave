(*
 * Minimized Acylic FSM for spell checking
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
open Lm_bitset

(* The dictionary is a minimized graph represented as a compacted bitset,
 * with an array of hashtables for transition. The new added words are not
 * minimized, and for efficiency they are stored as is in a single hashtable.
 *)
type dict =
   { paths : (int, int) Hashtbl.t array;
     states : Lm_bitset.t;
     size : int;
     extra : (string, unit) Hashtbl.t (* Could be other set representaion *)
   }

let is_in_extra extra e =
   Hashtbl.mem extra e

let add_to_extra extra e =
   Hashtbl.replace extra e ()

(* temporarly add new word to dictionary *)
let add_word d w =
   let b = String.length w - 1 in
   let rec aux st n =
      let table = (Array.get d.paths (Char.code w.[n])) in
         match Hashtbl.find_opt table st with
            Some stn when (stn < d.size) ->
               if n < b then
                  aux stn (succ n)
               else (* eol of word, which means current word is a prefix of
                     * an existed word.
                     *)
                  Lm_bitset.set d.states stn
            (* add the word to extra table *)
          | Some _ -> add_to_extra d.extra w (* already a word with same prefix *)
          | None -> Hashtbl.add table st d.size;
                    add_to_extra d.extra w
   in aux 0 0

(* search word in dictionary *)
let check d w =
   let b = String.length w - 1 in
   let rec aux st n =
      let table = (Array.get d.paths (Char.code w.[n])) in
         match Hashtbl.find_opt table st with
            Some stn when (stn < d.size) ->
               if n < b then
                  aux stn (succ n)
               else
                  Lm_bitset.get d.states
          | Some _ -> is_in_extra d.extra w
          | None -> false
   in aux 0 0


let init () : unit -> automaton =
   { paths = Array.init 254 (fun _ -> Hashtbl.create 587); top = 0 }

let root = 0


val next : state * graph -> char -> state * bool

(* automaton *)
type a =
   { states : (int, (char * int) list) Hashtbl.t ;
     final : (int, unit) Hashtbl.t;
     reg : (int, unit) Hashtbl.t;
     mutable count : int
   }

let init_aut = { states = Hashtbl.create 7213;
                 final = Hashtbl.create 127;
                 reg = Hashtbl.create 127;
                 count = 0
               }

let aut_to_dict { states = s; final = f; count = i; _ } =
   let p = Array.init 254 (fun _ -> Hashtbl.create 587) in
   let b = Lm_bitset.create count in
   let set a _ = Lm_bitset.set b a in
   let path a b = List.map (fun (c, n) -> Hashtbl.add (Array.get p (Char.code x)) a n) b
   in Hashtbl.iter set f;
      Hashtbl.iter path s;
      { paths = p; states = b; size = succ i; extra = Hashtbl.make 13 }

let add_to_final a n =
   Hashtbl.add a.final n ()

let is_final a n =
   Hashtbl.mem a.final n

let add_to_reg a n =
   Hashtbl.add a.reg n ()

let is_in_reg a n =
   Hashtbl.mem a.reg n

let hash_children_last a st =
   match Hashtbl.find a.states st with
      [] -> None
    | a :: _ -> Some a

let find_common { states = s; _ } w =
   let b = String.length w in
   let rec aux n =
      match List.assoc_opt
      match match_state st n with
         Some (stn, true)
            string_cut (succ n), stn
       | Some (stn, false) ->
            if succ n >= b then
               string_cut (succ n) stn
            else aux stn (succ n)
       | None ->
            string_cut n, st
   in aux 0 0

let add_file =
   let register = ref [] in
   let add_word w =
      let (prefix, suffix), ls = find_common paths w in

(* foo *)