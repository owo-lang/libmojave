/*
 * Provides function calculates actual string display width.
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
#include <wchar.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

/*
 * Some large buffer.
 */
#define BUFSIZE         (1 << 12)

/*
 * Safe wcswidth.
 */
static int s_wcswidth(const wchar_t *pwcs, size_t n)
{
    wchar_t wc;
    int l;
    int len = 0;

    while(n-- > 0 && (wc = *pwcs++) != L'\0'){
        l = wcwidth(wc);
        /* Treat any non printable wchar as width 0 */
        if(l < 0)
            l = 0;
        len += l ;
    }

    return len;
}

/*
 * Computes screen width of the string with mbsrtowcs and wcwidth.
 */
value display_width_of_string(value v_string)
{
    const char *str = String_val(v_string);

    wchar_t pwcs[BUFSIZE];
    int i = 0;

    while(str != NULL){
        if(mbsrtowcs(pwcs, &str, BUFSIZE, NULL) < 0)
            caml_failwith("display_width_of_string");

        i += s_wcswidth(pwcs, BUFSIZE);
    }

    return Val_int(i);
}
