(*
 * Lm_position informat for debugging.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2002 Jason Hickey, Caltech
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
open Format

open Lm_symbol
open Lm_location

(*
 * Lm_debug flags.
 *)
val debug_pos : bool ref
val trace_pos : bool ref

(*
 * Lm_position information.
 *)
type 'a pos

(*
 * Module for creating positions.
 * You have to specify the name of the module
 * where the exception are being created: use
 * MakePos in each file where Name.name is set
 * to the name of the module.
 *)
module type PosSig =
sig
   type t

   (* Creating positions *)
   val loc_exp_pos : loc -> t pos
   val loc_pos     : loc -> t pos -> t pos
   val base_pos    : t -> t pos
   val cons_pos    : t -> t pos -> t pos
   val pos_pos     : t pos -> t pos -> t pos
   val int_pos     : int -> t pos -> t pos
   val string_pos  : string -> t pos -> t pos
   val symbol_pos  : symbol -> t pos -> t pos
   val del_pos     : (formatter -> unit) -> loc -> t pos
   val del_exp_pos : (formatter -> unit) -> t pos -> t pos

   (* Utilities *)
   val loc_of_pos : t pos -> loc
   val pp_print_pos : formatter -> t pos -> unit
end

module type NameSig =
sig
   type t

   (* This is the name of the module where the position info is created *)
   val name : string

   (* Utilities for managing values *)
   val loc_of_value : t -> loc
   val pp_print_value  : formatter -> t -> unit
end

module MakePos (Name : NameSig) : PosSig with type t = Name.t

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
