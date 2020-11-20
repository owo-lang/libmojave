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


type window


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


val key_down   : int
val key_up     : int
val key_left   : int
val key_right  : int
val key_home   : int
val key_end    : int
val key_npage  : int
val key_ppage  : int
val key_enter  : int
val key_cancel : int

val key_err    : int
val key_ctrla  : int
val key_ctrld  : int
val key_ctrle  : int
val key_ctrlj  : int
val key_ctrll  : int
val key_ctrlm  : int
val key_ctrlu  : int
val key_ctrlv  : int


val curses_enabled : bool
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
