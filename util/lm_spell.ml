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
(* open Lm_bitset *)

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
                  Lm_bitset.get d.states n
          | Some _ -> is_in_extra d.extra w
          | None -> false
   in aux 0 0

(* automaton *)
type a =
   Node of (char * a ref) list
 | End of (char * a ref) list
(* | Root of a ref option array *)

type state = a ref

let zero = ref (Node [])

let theta state char =
   match !state with
      Node a
    | End a -> List.assoc_opt char a

let thetas state w =
   let b = String.length w in
   let rec aux s n =
      if n == b then s, n else
         match theta s w.[n] with
            Some s -> aux s (succ n)
          | None -> s, n in
   let s, st = aux state 0 in
   s, String.sub w st (b-st)

let has_children s =
   match !s with
      Node []
    | End [] -> false
    | _ -> true

let last_child s =
   match !s with
      Node l
    | End l -> List.hd l

let form_suffix sf l =
   match String.length sf with
      0 -> l
    | 1 -> (sf.[0], ref (End [])) :: l
    | x -> let rec aux n a =
              if n < 0 then a
              else aux (pred n) (sf.[n], ref (Node [a]))
           in (aux (x - 1) (sf.[0], ref (End []))) :: l

let equal_node a b =
   let sort = List.sort (fun a b -> compare (fst a) (fst b)) in
   let st_eq a b =
      (List.length a = List.length b)
      && List.for_all2
      (fun (a1, a2) (b1, b2) -> a1 = b1 && a2 == b2)
      (sort a) (sort b) in
      match a, b with
         Node a, Node b
       | End a, End b -> st_eq a b
       | _ -> false

let add_file zero filename =
   let register = ref [] in
   let rec replace_or_register (s : state) =
      let _, cs = last_child s in
         if has_children cs then
            replace_or_register cs;
         match List.find_opt (fun x -> equal_node x !cs) !register with
            Some q -> cs := q
          | None -> register := !cs :: !register in
   let add_word w =
      let ls, sf = thetas zero w in
         if has_children ls then replace_or_register ls;
         let sfx = form_suffix sf in
            ls := match !ls with
                     Node l -> Node (sfx l)
                   | End l -> End (sfx l)
   in
      try
         let inx = open_in filename in
         let count = ref 0 in
            try
               while true do
                  if !count > 100 then
                     (Lm_printf.eprintf "*"; count := 0);
                  add_word (input_line inx);
                  incr count;
               done
            with
               End_of_file -> close_in inx
             | x -> close_in inx;
                    raise x
      with
         Sys_error _ -> ()

(* foo *)