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

(* The dictionary is a minimized graph represented as a compacted bitset,
 * with an array of hashtables for transition. The new added words are not
 * minimized, and for efficiency they are stored as is in a single hashtable.
 *)
type dict =
   { paths : (char * int) list array;
     states : Lm_bitset.t;
     size : int;
     extra : (string, unit) Hashtbl.t (* Could be other set representaion *)
   }

let init_dict n =
   { paths = Array.make n [];
     states = Lm_bitset.create n; size = n;
     extra = Hashtbl.create 13 }

let is_in_extra extra e =
   Hashtbl.mem extra e

let add_to_extra extra e =
   Hashtbl.replace extra e ()

(* temporarly add new word to dictionary *)
let add_word d w =
   let b = String.length w - 1 in
   let rec aux st n =
      let pths = Array.get d.paths st in
      let c = w.[n] in
         match List.assoc_opt c pths with
            Some stn when (stn < d.size) ->
               if n < b then
                  aux stn (succ n)
               else (* eol of word, which means current word is a prefix of
                     * an existed word.
                     *)
                  Lm_bitset.set d.states stn
            (* add the word to extra table *)
          | Some _ -> add_to_extra d.extra w (* already a word with same prefix *)
          | None -> Array.set d.paths st ((c, d.size) :: pths); (* push new entry *)
                    add_to_extra d.extra w
   in aux 0 0

(* search word in dictionary *)
let check d w =
   let b = String.length w - 1 in
   let rec aux st n =
         match List.assoc_opt w.[n] (Array.get d.paths st) with
            Some stn when (stn < d.size) ->
               if n < b then
                  aux stn (succ n)
               else
                  Lm_bitset.get d.states stn
          | Some _ -> is_in_extra d.extra w
          | None -> false
   in aux 0 0

(*******
 * Build the acylic graph
 * See Incremental Construction of Minimal Acyclic Finite-State Automata, Jan Daciuk*
 *)
type node = { char : char; mutable next : state }

and state = { mutable id : int; term : bool; mutable children : node list }

let has_children = function
   { children = [] ; _ } -> false
 | _ -> true

let last_child s = List.hd s.children

let theta state char =
   match (List.find_opt (fun n -> n.char = char) state.children) with
      Some n -> Some n.next
    | None -> None

let thetas s w =
   let b = String.length w in
   let rec aux st n =
      if n = b then st, b else
         match theta st w.[n] with
            Some st -> aux st (succ n)
          | None -> st, n in
   let st, n = aux s 0 in
   let res = String.sub w n (b - n) (* drop n chars *)
   in st, res

let form_suffix sfx =
   let len = String.length sfx in
   let last = { char = sfx.[len - 1] ; next = { id = -1; term = true; children = [] } } in
   let rec aux n a =
      if n < 0 then a
      else aux (pred n) { char = sfx.[n];
                          next = { id = -1; term = false; children = [a] } }
   in aux (len - 2) last

let add_suffix last sfx =
   last.children <- (form_suffix sfx) :: last.children

module ChildrenHash =
struct
   type t = node list
   (* XXX: This assumes there will be only one sorted input file *)
   let equal = Lm_list_util.for_all2 (fun a b -> a.char = b.char && a.next == b.next)

   let hash = List.fold_left (fun a (b : node) -> a lxor Hashtbl.hash b) 0

end

module Register = Hashtbl.Make(ChildrenHash)

let build_from_file filename =
   (* a for terminal states, b for normal states *)
   let register_a, register_b = Register.create 30000, Register.create 150000 in
   let select b = if b then register_a else register_b in
   let find_in_register n = Register.find_opt (select n.term) n.children in
   let register n = Register.add (select n.term) n.children n in

   let rec replace_or_register s =
      let { next = next; _ } as child = last_child s in
         if has_children next then replace_or_register next;
         match find_in_register next with
            Some q -> child.next <- q
          | None -> register next in

   let root = { id = 0; term = false ; children = [] } in

   let add_word w =
      let last_state, current_suffix = thetas root w in
         if has_children last_state then
            replace_or_register last_state;
         add_suffix last_state current_suffix
   in
   let inx = open_in filename in
   begin
      try
         while true do
            add_word (input_line inx)
         done
      with
         End_of_file -> close_in inx
       | x -> close_in inx;
              raise x
   end;
   replace_or_register root;
   root, 1 + Register.length register_a + Register.length register_b

let compress_fsm root size =
   let count = ref 0 in
   let dict = init_dict size in
   let pths, ends, tmp = dict.paths, dict.states, Array.make size [] in
   let genid () = incr count; !count in
   let iter = List.map (fun { char = c; next = n } ->
                    if n.id < 0 then
                    begin
                       let id = genid() in
                          n.id <- id;
                          Array.set tmp id n.children;
                          if n.term then Lm_bitset.set ends id
                    end;
                    (c, n.id)) in
      Array.set tmp 0 root.children;
      for i = 0 to size - 1 do
         Array.set pths i (iter (Array.get tmp i));
      done;
      dict

let make_dict f =
   let r, s = build_from_file f in
      compress_fsm r s

let to_channel out d = Marshal.to_channel out (d.paths, d.states, d.size) []

let from_channel inx =
   let p, t, s = Marshal.from_channel inx in
      { paths = p; states = t; size = s ; extra = Hashtbl.create 13 }


(* foo *)