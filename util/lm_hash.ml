(*
 * A "hash-cons" utility.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2005-2007 Mojave Group, Caltech and HRL Laboratories, LLC
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
 * Author: Jason Hickey @email{jyh@cs.caltech.edu}
 * Modified By: Aleksey Nogin @email{anogin@hrl.com} @email{nogin@metaprl.org}
 * @end[license]
 *)
open Lm_printf
open Lm_thread_core

open Lm_hash_sig

(************************************************************************
 * A generic hash module to make comparisons faster.
 * This version uses a state for hash-consing.
 *)
module MakeHash (Arg : HashArgSig)
: HashSig with type elt = Arg.t =
struct
   type elt = Arg.t

   (* %%MAGICBEGIN%% *)
   type t = int * elt
   (* %%MAGICEND%% *)

   let create x =
      Arg.hash x, x

   let get (_, x) =
      x

   let hash (i, _) =
      i

   let compare ((i1 : int), x1) ((i2 : int), x2) =
      if i1 = i2 then
         Arg.compare x1 x2
      else if i1 < i2 then
         -1
      else
         1
end

(************************************************************************
 * Table-based hashing.
 *)
module MakeHashCons (Arg : HashArgSig)
: HashConsSig
  with type elt = Arg.t
  with type hash = MakeHash(Arg).t =
struct
   (* %%MAGICBEGIN%% *)
   type elt = Arg.t
   type t = int

   module Key = MakeHash (Arg);;
   module KeyTable = Lm_map.LmMake (Key);;
   type hash = Key.t

   (*
    * We need both directions.
    *)
   type state =
      { mutable key_table : int KeyTable.t;
        mutable int_table : elt array
      }
   (* %%MAGICEND%% *)

   let create_state () =
      { key_table = KeyTable.empty;
        int_table = [||]
      }

   let length state =
      KeyTable.cardinal state.key_table

   let set state i x =
      let table = state.int_table in
      let len = Array.length table in
         if len = 0 then
            let table = Array.make 32 x in
               state.int_table <- table
         else if i = len then
            let table2 = Array.make (len * 2) x in
               Array.blit table 0 table2 0 len;
               state.int_table <- table2
         else
            table.(i) <- x

   let icreate state item =
      try KeyTable.find state.key_table item with
         Not_found ->
            let index = KeyTable.cardinal state.key_table in
               state.key_table <- KeyTable.add state.key_table item index;
               set state index (Key.get item);
               index

   let create state x =
      icreate state (Key.create x)

   let get state index =
      state.int_table.(index)

   let hash index =
      index

   let compare index1 index2 =
      index1 - index2

   let map_array f state =
      Array.mapi f (Array.sub state.int_table 0 (KeyTable.cardinal state.key_table))

   let fold f x state =
      let len = KeyTable.cardinal state.key_table in
      let rec fold i x =
         if i = len then
            x
         else
            fold (succ i) (f x i)
      in
         fold 0 x
end

(************************************************************************
 * Marshalable version.
 *
 * This takes a slightly different approach, wrapping the value in
 * a triple of a hash code and a dummy ref cell.  During marshaling,
 * the cell will point somewhere else, so we know that the value
 * must be rehashed.
 *)

(* %%MAGICBEGIN%% *)
type 'a hash_marshal_item =
   { mutable item_ref : unit ref;
     mutable item_val : 'a;
     item_hash        : int
   }
(* %%MAGICEND%% *)

(*
 * The reference in the current process.
 *)
let current_ref = ref ()

(*
 * Statistics.
 *)
type hash_stat =
   { hash_debug              : string;
     mutable hash_reintern   : int;
     mutable hash_compare    : int;
     mutable hash_collisions : int
   }

let hash_stats = ref []

let pp_print_stat buf stat =
   let { hash_debug      = debug;
         hash_reintern   = reintern;
         hash_compare    = compare;
         hash_collisions = collisions
       } = stat
   in
      fprintf buf "@[<hv 3>%s: reintern = %d, compare = %d, collisions = %d@]@\n" (**)
         debug reintern compare collisions

let pp_print_hash_stats buf =
   List.iter (pp_print_stat buf) !hash_stats

(*
 * let () =
 *    at_exit (fun () -> pp_print_hash_stats stderr)
 *)

(*
 * We need to work nicely with threads.
 *
 * Note that the reintern function may be recursive, so we need to account for cases,
 * where the current thread is already holding the lock.
 * Almost every accesses come from the main thread with very little if any contention from other
 * threads. This makes it more effiecient to use a single global lock (as opposed to having a
 * separate lock for each instance of the functor), so that mutually recursive reintern calls only
 * have to lock one lock, not all of them.
 *
 * Finally, we do not care about race conditions for the statistics
 *)
module Synchronize : sig
   val synchronize : ('a -> 'b) -> 'a -> 'b
end = struct
   let lock_mutex = MutexCore.create "Lm_hash.Synchronize"
   let lock_id = ref None

   let unsynchronize () =
      lock_id := None;
      MutexCore.unlock lock_mutex

   let synchronize f x =
      let id = ThreadCore.id (ThreadCore.self ()) in
         match !lock_id with
            Some id' when id = id' ->
               (*
                * We are already holding the lock. This means:
                *  - we do not have to do anything special
                *  - reading the shared lock_id ref could not have created a race condition
                *)
               f x
          | _ ->
               MutexCore.lock lock_mutex;
               lock_id := Some id;
               try
                  let res = f x in
                     unsynchronize();
                     res
               with exn ->
                  unsynchronize();
                  raise exn
end

let synchronize =
   if ThreadCore.enabled then
      Synchronize.synchronize
   else
      (fun f -> f)

(*
 * Make a hash item.
 *)
module MakeHashMarshal (Arg : HashMarshalArgSig) =
struct
   type elt = Arg.t
   type t = elt hash_marshal_item

   (* Keep a hash-cons table based on a weak comparison *)
   module WeakCompare =
   struct
      type t = elt hash_marshal_item

      let compare item1 item2 =
         let hash1 = item1.item_hash in
         let hash2 = item2.item_hash in
            if hash1 < hash2 then
               -1
            else if hash1 > hash2 then
               1
            else
               Arg.compare item1.item_val item2.item_val
   end;;

   module Table = Lm_map.LmMake (WeakCompare);;

   let table = ref Table.empty

   (*
    * Keep track of collisions for debugging.
    *)
   let stats =
      { hash_debug       = Arg.debug;
        hash_reintern    = 0;
        hash_compare     = 0;
        hash_collisions  = 0
      }

   let () =
      hash_stats := stats :: !hash_stats

   (*
    * When creating an item, look it up in the table.
    *)
   let create_core elt =
      let item =
         { item_ref  = current_ref;
           item_val  = elt;
           item_hash = Arg.hash elt
         }
      in
         try Table.find !table item with
            Not_found ->
               table := Table.add !table item item;
               item

   let create = synchronize create_core

   let intern elt =
      let item =
         { item_ref  = current_ref;
           item_val  = elt;
           item_hash = Arg.hash elt
         }
      in
         Table.find !table item

   (*
    * Reintern.  This will take an item that may-or-may-not be hashed
    * and produce a new one that is hashed.
    *)
   let reintern_core item1 =
      stats.hash_reintern <- succ stats.hash_reintern;
      try
         let item2 = Table.find !table item1 in
            if item2 != item1 then begin
               item1.item_val <- item2.item_val;
               item1.item_ref <- current_ref
            end;
            item2
      with
         Not_found ->
            item1.item_val <- Arg.reintern item1.item_val;
            item1.item_ref <- current_ref;
            table := Table.add !table item1 item1;
            item1

   let reintern = synchronize reintern_core

   (*
    * Access to the element.
    *)
   let get item =
      if item.item_ref == current_ref then
         item.item_val
      else
         (reintern item).item_val

   let hash item =
      item.item_hash

   (*
    * String pointer-based comparison.
    *)
   let compare item1 item2 =
      stats.hash_compare <- succ stats.hash_compare;
      let hash1 = item1.item_hash in
      let hash2 = item2.item_hash in
         if hash1 < hash2 then
            -1
         else if hash1 > hash2 then
            1
         else if item1.item_val == item2.item_val then
            0
         else
            let elt1 = get item1 in
            let elt2 = get item2 in
               if elt1 == elt2 then
                  0
               else begin
                  stats.hash_collisions <- succ stats.hash_collisions;
                  let cmp = Arg.compare elt1 elt2 in
                     if cmp = 0 then
                        invalid_arg "Lm_hash is broken@.";
                     cmp
               end

   let equal item1 item2 =
      (item1 == item2) || (item1.item_hash = item2.item_hash && get item1 == get item2)
end

(*
 * A version with two equalities.
 * The fine equality is used for cons-hashing, but the coarse
 * version is used for external comparisons.  The fine equality
 * must be a refinement of the coarse equality.
 *)
(* %%MAGICBEGIN%% *)
type 'a hash_marshal_eq_item = ('a * 'a hash_marshal_item) hash_marshal_item
(* %%MAGICEND%% *)

module MakeHashMarshalEq (Arg : HashMarshalEqArgSig) =
struct
   type elt = Arg.t
   type t = elt hash_marshal_eq_item

   module CoarseArg =
   struct
      type t     = Arg.t
      let debug  = Arg.debug ^ ":coarse"

      let hash     = Arg.coarse_hash
      let compare  = Arg.coarse_compare
      let reintern = Arg.reintern
   end;;

   module Coarse = MakeHashMarshal (CoarseArg);;

   (*
    * We fold the Coarse item into the fine
    * item only so we don't have to create three
    * modules (the final one being a pair of fine
    * and coarse).
    *)
   module FineArg =
   struct
      type t     = Arg.t * Coarse.t
      let debug  = Arg.debug ^ ":fine"

      (*
       * We're assuming that the fine hash is a
       * refinement of the coarse one.
       *)
      let hash (fine, _) =
         Arg.fine_hash fine

      let compare (fine1, _) (fine2, _) =
         Arg.fine_compare fine1 fine2

      let reintern ((fine, coarse) as item) =
         let fine' = Arg.reintern fine in
         let coarse' = Coarse.reintern coarse in
            if fine' == fine && coarse' == coarse then
               item
            else
               fine', coarse'
   end;;

   module Fine = MakeHashMarshal (FineArg);;

   let create x =
      Fine.create (x, Coarse.create x)

   let intern x =
      Fine.intern (x, Coarse.intern x)

   let get info =
      fst (Fine.get info)

   (*
    * The default functions are the coarse versions.
    *)
   let get_coarse info =
      snd (Fine.get info)

   let hash info =
      Coarse.hash (get_coarse info)

   let compare item1 item2 =
      Coarse.compare (get_coarse item1) (get_coarse item2)

   let equal item1 item2 =
      Coarse.equal (get_coarse item1) (get_coarse item2)

   (*
    * Also export the fine versions.
    *)
   let fine_hash = Fine.hash
   let fine_compare = Fine.compare
   let fine_equal = Fine.equal

   let reintern = Fine.reintern
end

(************************************************************************
 * Better-than usual hash functions.
 *
 * XXX: we use ranhash function, which simulates a enough random
 * sequence.
 *)

(* %%MAGICBEGIN%% *)
let digest_length = 16
external ranhash : int -> int = "lm_ranhash"
(* %%MAGICEND%% *)

(************************************************
 * Integer hashes.
 *)
(* %%MAGICBEGIN%% *)
module HashCode : HashCodeSig =
struct
   type t =
      { mutable hash_digest : int;
        mutable hash_code   : int
      }

   (*
    * New buffer.
    *)
   let create () =
      { hash_digest = 0;
        hash_code   = 0
      }

   (*
    * Add an integer.
    *)
   let add_truncated_bits buf i =
      let { hash_digest = digest;
            hash_code   = code
          } = buf
      in
      let code = code + i + 1 in
         buf.hash_digest <- (digest * 3) lxor (ranhash code);
         buf.hash_code <- code

   let add_bits buf i =
      add_truncated_bits buf (i land 0x7ff)

   (*
    * Add the characters in a string.
    *)
   let add_string buf s =
      for i = 0 to pred (String.length s) do
         add_truncated_bits buf (Char.code (String.unsafe_get s i))
      done

   (*
    * Numbers.
    *)
   let add_int buf i =
      add_bits buf i;
      add_bits buf (i lsr 11);
      add_bits buf (i lsr 22)

   let add_int32 buf i =
      add_bits buf (Int32.to_int (Int32.shift_right_logical i 16));
      add_bits buf (Int32.to_int i)

   let add_int64 buf i =
      add_int buf (Int64.to_int (Int64.shift_right_logical i 48));
      add_int buf (Int64.to_int (Int64.shift_right_logical i 24));
      add_int buf (Int64.to_int i)

   let add_nativeint buf i =
      add_int buf (Nativeint.to_int (Nativeint.shift_right_logical i 48));
      add_int buf (Nativeint.to_int (Nativeint.shift_right_logical i 24));
      add_int buf (Nativeint.to_int i)

   let add_float buf x =
      add_int64 buf (Int64.bits_of_float x)

   (*
    * Extract the digest.
    *)
   let code buf =
      buf.hash_digest
end
(* %%MAGICEND%% *)

(************************************************
 * Digest-based hashes.
 *)
(* %%MAGICBEGIN%% *)
module HashDigest : HashDigestSig =
struct
   type t =
      { hash_digest         : int array;
        mutable hash_code   : int
      }

   (*
    * New buffer.
    *)
   let create () =
      { hash_digest = Array.make digest_length 0;
        hash_code   = 0
      }

   (*
    * Add an integer.
    *)
   let add_truncated_bits buf i =
      let { hash_digest = digest;
            hash_code   = code
          } = buf
      in
      let code = code + digest_length + i in
         for i = 0 to digest_length - 1 do
            digest.(i) <- (digest.(i) * 3) lxor (ranhash (code + i))
         done;
         buf.hash_code <- code

   let add_bits buf i =
      add_truncated_bits buf (i land 0x7ff)

   (*
    * Add the characters in a string.
    *)
   let add_char buf c =
      add_truncated_bits buf (Char.code c)

   let add_string buf s =
      for i = 0 to pred (String.length s) do
         add_char buf (String.unsafe_get s i)
      done

   let add_substring buf s off len =
      if off < 0 || len < 0 || off + len > String.length s then
         raise (Invalid_argument "Lm_hash.add_substring");
      for i = off to pred (off + len) do
         add_char buf (String.unsafe_get s i)
      done

   (*
    * Numbers.
    *)
   let add_bool buf b =
      add_truncated_bits buf (if b then 1 else 0)

   let add_int buf i =
      add_bits buf i;
      add_bits buf (i lsr 11);
      add_bits buf (i lsr 22)

   let add_int32 buf i =
      add_bits buf (Int32.to_int (Int32.shift_right_logical i 16));
      add_bits buf (Int32.to_int i)

   let add_int64 buf i =
      add_int buf (Int64.to_int (Int64.shift_right_logical i 48));
      add_int buf (Int64.to_int (Int64.shift_right_logical i 24));
      add_int buf (Int64.to_int i)

   let add_nativeint buf i =
      add_int buf (Nativeint.to_int (Nativeint.shift_right_logical i 48));
      add_int buf (Nativeint.to_int (Nativeint.shift_right_logical i 24));
      add_int buf (Nativeint.to_int i)

   let add_float buf x =
      let i = Int64.bits_of_float x in
         add_int buf (Int64.to_int (Int64.shift_right i 48));
         add_int buf (Int64.to_int (Int64.shift_right i 24));
         add_int buf (Int64.to_int i)

   (*
    * Extract the digest.
    *)
   let digest buf =
      let digest = buf.hash_digest in
      let s = Bytes.create digest_length in
         for i = 0 to pred digest_length do
            Bytes.set s i (Char.chr (digest.(i) land 0xff))
         done;
         Bytes.to_string s
end;;
(* %%MAGICEND%% *)

(************************************************************************
 * Hash helper functions.
 *)

(*
 * The default function for combining hash values.
 *
 * XXX: The original one is discarded due to poor property of XOR
 * The new implementation is a variant of Bernstein's hash:

   seed <- 1009
   factor <- 9176
   hash <- seed * factor + i1
   hash <- hash * factor + i2

 * where the choice of seed and factor is not arbitrary and aimed
 * for less collisions, but better combination could exist.
 *)
let hash_combine i1 i2 =
   (0x8d4658 + i1) * 9176 + i2

let hash_seed = 1009

let hash_factor hash i =
   hash * 9176 + i

(*
 * Hash a list of integers.
 *)
let hash_int_list code l =
   let buf = HashCode.create () in
      HashCode.add_int buf code;
      List.iter (HashCode.add_int buf) l;
      HashCode.code buf

(*
 * Comparison utilities.
 *)
let rec compare_int_list (l1 : int list) (l2 : int list) =
   match l1, l2 with
      i1 :: l1, i2 :: l2 ->
         if i1 < i2 then
            -1
         else if i1 > i2 then
            1
         else
            compare_int_list l1 l2
    | [], _ ::_ ->
         -1
    | _ :: _, [] ->
         1
    | [], [] ->
         0

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
