(*
 * This is a generic printf builder.  We take "simple" printing
 * functions, and turn them into a general printf.
 *
 * Formatted printing.
 * Here are the format strings we handle.
 *    d or i: print an integer in decminal
 *    u: print an unsigned integer in decimal
 *    x: print an integer in unsigned hex in lowercase
 *    X: print an integer in unsigned hex in uppercase
 *    o: print an integer in unsigned octal
 *    s: print a string
 *    c: print a character
 *    f: print a float in decimal
 *    e,E: print a float in exponent notation
 *    g,G: print a float in best notation
 *    b: print a Boolean
 *    a: user-defined printer
 *    t: user-defined printer
 *    %: print the '%' char
 *
 * From the printf man page, each format specifier has
 *    1. 0 or more flags
 *       #: use alternate notation
 *       0: 0-pad the number
 *       '-': left-justify the field
 *       ' ': leave a space before the number
 *       '+': always print the sign of the number
 *    2. An optional field width in decimal
 *    3. An optional precision, specified as a '.' followed
 *       by a decimal number.
 *    4. A format specifier
 *
 * For Format:
 *    @]: close_box
 *    @,: print_cut
 *    @ : print_space
 *    @\n: force_newline
 *    @;: print_break
 *    @?: print_flush
 *    @.: print_newline
 *    @<n>: print_length
 *    @@: plain @ char
 *
 * This module now relies on CamlinternalFormat
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2001-2005 Mojave Group, Caltech
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
 * Original version author: Jason Hickey @email{jyh@cs.caltech.edu}
 * Updated by: LdBeth @email{ldbeth@sdf.org}
 * @end[license]
 *)

(*
 * Argument module.
 *)
module type PrintfArgsSig =
sig
   (* Some buffer type *)
   type t
   type result

   (* The printers *)
   val print_char : t -> char -> unit
   val print_string : t -> string -> unit

   (* Format functions *)
   val open_box : t -> int -> unit
   val open_hbox : t -> unit
   val open_vbox : t -> int -> unit
   val open_hvbox : t -> int -> unit
   val open_hovbox : t -> int -> unit
   val close_box : t -> unit

   val print_cut : t -> unit
   val print_space : t -> unit
   val force_newline : t -> unit
   val print_break : t -> int -> int -> unit
   val print_flush : t -> unit
   val print_newline : t -> unit

   val exit : t -> result
end

(*
 * What this module provides.
 *)
module type PrintfSig =
sig
   (* Some buffer type *)
   type t
   type result

   (* Lm_printf functions *)
   val fprintf : t -> ('a, t, result) format -> 'a
end

(*
 * Here's the actual printf module.
 *)
module MakePrintf (Args : PrintfArgsSig) =
struct
   (************************************************************************
    * TYPES
    ************************************************************************)

   type t = Args.t
   type result = Args.result

   open CamlinternalFormatBasics
   open CamlinternalFormat


   (************************************************************************
    * BASE PRINTERS
    ************************************************************************)

   let compute_tag acc =
      let buf = Buffer.create 16 in
      let rec p_acc b = function
         Acc_data_string (p, s)
       | Acc_string_literal (p, s) -> p_acc b p; Buffer.add_string b s
       | Acc_char_literal (p, c)
       | Acc_data_char (p, c) -> p_acc b p; Buffer.add_char b c
       | End_of_acc -> ()
       | _ -> raise (Invalid_argument "compute_tag: Unsupported print box tag foramt")
      in p_acc buf acc;
      let len = Buffer.length buf in
      if len < 2 then Buffer.contents buf
      else Buffer.sub buf 1 (len - 2)

   let rformat_open_box buf indent = function
      Pp_hbox -> Args.open_hbox buf
    | Pp_vbox -> Args.open_vbox buf indent
    | Pp_hvbox -> Args.open_hvbox buf indent
    | Pp_hovbox -> Args.open_hovbox buf indent
    | Pp_box -> Args.open_box buf indent
    | Pp_fits -> raise (Invalid_argument "rformat_open_box: Unsupported fits type")

   let format_formatting_lit o fmting_lit = match fmting_lit with
      Close_box                 -> Args.close_box o
    | Close_tag                 -> ()
    | Break ("@,", _, _)        -> Args.print_cut o
    | Break ("@ ", _, _)        -> Args.print_space o
    | Break (_, width, offset)  -> Args.print_break o width offset
    | FFlush                    -> Args.print_flush o
    | Force_newline             -> Args.force_newline o
    | Flush_newline             -> Args.print_newline o
    | Magic_size (_, _)         -> ()
    | Escaped_at                -> Args.print_char o '@'
    | Escaped_percent           -> Args.print_char o '%'
    | Scan_indic c              -> Args.print_char o '@'; Args.print_char o c

   let rec print_acc o acc = match acc with
      Acc_formatting_lit (p, f) ->
         print_acc o p; format_formatting_lit o f;
    | Acc_formatting_gen (p, Acc_open_tag acc') ->
         print_acc o p; Args.print_string o "@{"; print_acc o acc'; (* not implemented *)
    | Acc_formatting_gen (p, Acc_open_box acc') ->
         print_acc o p;
         let (indent, bty) = open_box_of_string (compute_tag acc') in
         rformat_open_box o indent bty;
    | Acc_string_literal (p, s)
    | Acc_data_string (p, s)   -> print_acc o p; Args.print_string o s
    | Acc_char_literal (p, c)
    | Acc_data_char (p, c)     -> print_acc o p; Args.print_char o c
    | Acc_delay (p, f)         -> print_acc o p; ignore (f o);
    | Acc_flush p              -> print_acc o p; Args.print_flush o
    | Acc_invalid_arg (p, msg) -> print_acc o p; invalid_arg msg;
    | End_of_acc               -> ()

   let fprintf o (Format (fmt, _)) =
      make_printf (fun acc -> print_acc o acc; Args.exit o) End_of_acc fmt
end

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
