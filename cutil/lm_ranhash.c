/*
 *
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
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
 * @email{ldbeth@sdf.org}
 * @end[license]
 */

#include <stdlib.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

static uint64_t ranhash(uint64_t v)
{
    v *= 3935559000370003845LL;
    v += 2691343689449507681LL;
    v ^= v >> 21; v ^= v << 37; v ^= v >> 4;
    v *= 4768777513237032717LL;
    v ^= v << 20; v ^= v >> 41; v ^= v << 5;
    return v;
}
/*
 * Truncated to 31-bit
 */
value lm_ranhash(value v_i)
{
    CAMLparam1(v_i);
    long int val = Long_val(v_i);
    int res = ranhash(val) & 0x7fffffff;

    CAMLreturn(Val_int(res));
}
