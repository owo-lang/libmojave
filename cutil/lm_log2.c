/*
   Fast integer log2.
   Copyright (C) 2020 LdBeth

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation,
   version 2.1 of the License.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

   Additional permission is given to link this library with the
   OpenSSL project's "OpenSSL" library, and with the OCaml runtime,
   and you may distribute the linked executables.  See the file
   LICENSE.libmojave for more details.
 */
#include <stdlib.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#ifdef __GNUC__ /* use GCC builtin function */

static int log2_64 (uint64_t value)
{
    return 31 - __builtin_clz(value);
}

static int ctz (unsigned value)
{
    return __builtin_ctz(value);
}

#else /* Portable fast log2 */
const int8_t tab64[64] = {
    63,  0, 58,  1, 59, 47, 53,  2,
    60, 39, 48, 27, 54, 33, 42,  3,
    61, 51, 37, 40, 49, 18, 28, 20,
    55, 30, 34, 11, 43, 14, 22,  4,
    62, 57, 46, 52, 38, 26, 32, 41,
    50, 36, 17, 19, 29, 10, 13, 21,
    56, 45, 25, 31, 35, 16,  9, 12,
    44, 24, 15,  8, 23,  7,  6,  5};

static int log2_64 (uint64_t value)
{
    value |= value >> 1;
    value |= value >> 2;
    value |= value >> 4;
    value |= value >> 8;
    value |= value >> 16;
    value |= value >> 32;
    return tab64[((uint64_t)((value - (value >> 1))*0x07EDD5E59A4E28C2)) >> 58];
}

static int ntz(unsigned x) {
    unsigned y;
    int n;

    if (x == 0) return 32;
    n = 31;
    y = x <<16; if (y != 0) {n = n-16; x = y;}
    y = x << 8; if (y != 0) {n = n-8; x = y;}
    y = x << 4; if (y != 0) {n = n-4; x = y;}
    y = x << 2; if (y != 0) {n = n-2; x = y;}
    y = x << 1; if (y != 0) {n = n-1;}
    return n;
}

#endif /* is GCC compatible? */

value lm_ilog2(value i)
{
    CAMLparam1(i);
    int val, res;

    val = Int_val(i);
    if (val > 0) {
        res = log2_64(val);
    } else {
        res = -1;
    }

    CAMLreturn(Val_int(res));
}

value lm_ctz(value i)
{
    CAMLparam1(i);
    int val;

    val = Int_val(i);
    if (val <= 0)
        caml_invalid_argument("ctz: argument cannot be negative or zero.");

    CAMLreturn(Val_int(ctz(val)));
}
