(*
 * Interface for SSL.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004 Mojave Group, Caltech
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
 * Author: Jason Hickey
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)

type t
type ssl_out
type ssl_in

exception SSLSigPipe

(*
 * SSL interface.
 *)
val enabled : bool
external socket       : string -> t                        = "lm_ssl_socket"
external bind         : t -> Unix.inet_addr -> int -> unit = "lm_ssl_bind"
external getsockname  : t -> Unix.inet_addr * int          = "lm_ssl_get_addr"
external listen       : t -> string -> int -> unit         = "lm_ssl_listen"
external accept       : t -> t                             = "lm_ssl_accept"
external connect      : t -> Unix.inet_addr -> int -> unit = "lm_ssl_connect"
external shutdown     : t -> unit                          = "lm_ssl_shutdown"
external close        : t -> unit                          = "lm_ssl_close"

(*
 * For restart.
 *)
external fd        : t -> int                           = "lm_ssl_fd"
external serve     : int -> string -> string -> t       = "lm_ssl_serve"

(*
 * Buffered output.
 *)
val out_channel_of_ssl : t -> ssl_out

val fprintf : ssl_out -> ('a, ssl_out, unit) format -> 'a

val output_char : ssl_out -> char -> unit
val output_string : ssl_out -> string -> unit
val output_buffer : ssl_out -> Buffer.t -> unit
val output : ssl_out -> string -> int -> int -> unit
val flush : ssl_out -> unit
val close_out : ssl_out -> unit

(*
 * Buffered input.
 *)
val in_channel_of_ssl : t -> ssl_in
val input_char : ssl_in -> char
val input_line : ssl_in -> string
val really_input : ssl_in -> bytes -> int -> int -> unit
val close_in : ssl_in -> unit

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
