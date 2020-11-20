/*
   Fast integer log2, ctz.
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

#if defined __GNUC__ && defined __LP64__ /* use GCC builtin function */

static int log2_64 (uint64_t value)
{
    return 63 - __builtin_clzl(value);
}

static int ctz (unsigned long value)
{
    return __builtin_ctzl(value);
}

#else /* Portable fast log2 */
/* u64 version */
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

/* 32bit version
const int8_t tab32[32] = {
    0,  9,  1, 10, 13, 21,  2, 29,
    11, 14, 16, 18, 22, 25,  3, 30,
    8, 12, 20, 28, 15, 17, 24,  7,
    19, 27, 23,  6, 26,  5,  4, 31};

int log2_32 (uint32_t value)
{
    value |= value >> 1;
    value |= value >> 2;
    value |= value >> 4;
    value |= value >> 8;
    value |= value >> 16;
    return tab32[(uint32_t)(value*0x07C4ACDD) >> 27];
} */

static int ctz(unsigned long x)
{
    /* convert to rightmost 1 bit */
    return log2_64(x & -x);
}

#endif /* is GCC compatible? */

value lm_ilog2_byte(value i)
{
    /* CAMLparam1(i); */
    long int val;
    int res = -1;

    val = Long_val(i);
    if (val > 0)
        res = log2_64(val);

    return Val_int(res);
}

value lm_ctz_byte(value i)
{
    /* CAMLparam1(i); */
    long int val;

    val = Long_val(i);

    return Val_int(ctz(val));
}

intnat lm_ilog2(intnat i)
{
    intnat res = -1;

    if (i > 0)
      res = log2_64(i);

    return res;
}

intnat lm_ctz(intnat i)
{
    return ctz(i);
}
