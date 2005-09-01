(*
 * Source locations.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2002-2005 Jason Hickey, Caltech
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Author: Jason Hickey
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Lm_symbol
open Lm_printf

(* XXX: TODO: we should switch to using MLast.loc instead *)

(*
 * A location is a character range
 *    filename, start_line, start_char, end_line, end_char
 *)
(* %%MAGICBEGIN%% *)
type loc = symbol * int * int * int * int
(* %%MAGICEND%% *)

(*
 * Comparison.
 *)
let compare (**)
       (v1, start_line1, start_char1, end_line1, end_char1)
       (v2, start_line2, start_char2, end_line2, end_char2) =
   let cmp = Lm_symbol.compare v1 v2 in
      if cmp = 0 then
         let cmp = start_line1 - start_line2 in
            if cmp = 0 then
               let cmp = start_char1 - start_char2 in
                  if cmp = 0 then
                     let cmp = end_line1 - end_line2 in
                        if cmp = 0 then
                           end_char1 - end_char2
                        else
                           cmp
                  else
                     cmp
            else
               cmp
      else
         cmp

(*
 * Source location if all else fails.
 *)
let bogus_loc name =
   Lm_symbol.add name, 0, 0, 0, 0

(*
 * Normal location.
 *)
let create_loc name start_line start_char end_line end_char =
   name, start_line, start_char, end_line, end_char

(*
 * For marshaling.
 *)
let dest_loc (name, start_line, start_char, end_line, end_char) =
   name, start_line, start_char, end_line, end_char

(*
 * Union of locations.
 *)
let union_loc loc1 loc2 =
   let file1, start_line1, start_char1, _, _ = loc1 in
   let file2, _, _, end_line2, end_char2 = loc2 in
      if file1 = file2 then
         (file1, start_line1, start_char1, end_line2, end_char2)
      else
         loc1

(*
 * Print a file location.
 *)
let pp_print_location buf (file, start_line, start_char, end_line, end_char) =
   fprintf buf "File %a: " output_symbol file;
   if start_line = end_line then
      fprintf buf "line %d, characters %d-%d" start_line start_char end_char
   else
      fprintf buf "lines %d:%d-%d:%d" start_line start_char end_line end_char

let string_of_location loc =
   pp_print_location stdstr loc;
   flush_stdstr ()

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
