(*
 * Simple NCurses interface.
 * Copyright (C) 2002 Justin David Smith, Caltech
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
 
 
(***  Types  ***)


(* window
   Type of all ncurses window interfaces.  This type is abstract in 
   OCaml but is a real pointer in the C code (interned by ncurses).  *)
type window


(* attr
   Attributes which can be passed to the various attr() functions.
   These control the display properties of the window.  *)
type attr =
   A_NORMAL
 | A_STANDOUT
 | A_UNDERLINE
 | A_REVERSE
 | A_BLINK
 | A_DIM
 | A_BOLD
 | A_PROTECT
 | A_INVIS 
 | A_ALTCHARSET
 | A_CHARTEXT


(***  C Interface Functions for Key Values  ***)


external caml_key_down  : unit -> int = "caml_key_down"  [@@noalloc]
external caml_key_up    : unit -> int = "caml_key_up"    [@@noalloc]
external caml_key_left  : unit -> int = "caml_key_left"  [@@noalloc]
external caml_key_right : unit -> int = "caml_key_right" [@@noalloc]
external caml_key_home  : unit -> int = "caml_key_home"  [@@noalloc]
external caml_key_end   : unit -> int = "caml_key_end"   [@@noalloc]
external caml_key_npage : unit -> int = "caml_key_npage" [@@noalloc]
external caml_key_ppage : unit -> int = "caml_key_ppage" [@@noalloc]
external caml_key_enter : unit -> int = "caml_key_enter" [@@noalloc]
external caml_key_cancel: unit -> int = "caml_key_cancel" [@@noalloc]


(***  Key Values  ***)


let key_down   = caml_key_down   ()
let key_up     = caml_key_up     ()
let key_left   = caml_key_left   ()
let key_right  = caml_key_right  ()
let key_home   = caml_key_home   ()
let key_end    = caml_key_end    ()
let key_npage  = caml_key_npage  ()
let key_ppage  = caml_key_ppage  ()
let key_enter  = caml_key_enter  ()
let key_cancel = caml_key_cancel ()

let key_err    = -1
let key_ctrla  = 1
let key_ctrld  = 4
let key_ctrle  = 5
let key_ctrlj  = 10
let key_ctrll  = 12
let key_ctrlm  = 13
let key_ctrlu  = 21
let key_ctrlv  = 22


(***  C Interface Functions for Curses  ***)


external caml_curses_enabled : unit -> bool = "caml_curses_enabled" [@@noalloc]
let curses_enabled = caml_curses_enabled ()

external initscr : unit -> unit = "caml_curses_initscr"
external endwin : unit -> unit = "caml_curses_endwin"
external newwin : int -> int -> int -> int -> window = "caml_curses_newwin"
external delwin : window -> unit = "caml_curses_delwin"
external waddch : window -> char -> unit = "caml_curses_waddch"
external waddstr : window -> string -> unit = "caml_curses_waddstr"
external wattron : window -> attr -> unit = "caml_curses_wattron"
external wattroff : window -> attr -> unit = "caml_curses_wattroff"
external wgetch : window -> int = "caml_curses_wgetch"
external wgetstr : window -> string = "caml_curses_wgetstr"
external wrefresh : window -> unit = "caml_curses_wrefresh"
external wnoutrefresh : window -> unit = "caml_curses_wnoutrefresh"
external doupdate : unit -> unit = "caml_curses_doupdate"
external refreshscreen : unit -> unit = "caml_curses_refreshscreen"
external werase : window -> unit = "caml_curses_werase"
external wclrtoeol : window -> unit = "caml_curses_wclrtoeol"
external wclrtobot : window -> unit = "caml_curses_wclrtobot"
external wmove : window -> int -> int -> unit = "caml_curses_wmove"
external getyx : window -> int * int = "caml_curses_getyx"
external getmaxyx : window -> int * int = "caml_curses_getmaxyx"
external scrollok : window -> bool -> unit = "caml_curses_scrollok"
external echook : bool -> unit = "caml_curses_echook"
external wscrl : window -> int -> unit = "caml_curses_wscrl"