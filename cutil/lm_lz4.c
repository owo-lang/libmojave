/*
 * Interface to LZ4 with Marshal support.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2021 LdBeth
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

#ifdef LZ4_ENABLED

/* XXX: This uses experimental interface */
#define CAML_INTERNALS

#include <memory.h>
#include <stdlib.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/intext.h>
#include <caml/io.h>
#include <caml/fail.h>

/************************************************************************
 * LZ4 routines.
 */

#include <lz4.h>

value lm_output_value_lz4(value vchannel, value v, value flags)
{
    CAMLparam3(vchannel, v, flags);
    char *buf;
    intnat len;

    caml_output_value_to_malloc(v, flags, &buf, &len);

    const int max_dst_size = LZ4_compressBound(len);
    char* compressed_data = malloc((size_t)max_dst_size);
    if (compressed_data == NULL)
        caml_raise_out_of_memory();
    const int compressed_data_size = LZ4_compress_default(buf, compressed_data, len, max_dst_size);
    free(buf);
    if (compressed_data_size <= 0) {
        free(compressed_data);
        caml_failwith("LZ4: compression failed.");
    }

    struct channel * channel = Channel(vchannel);

    Lock(channel);
    caml_putword(channel, compressed_data_size);
    caml_putword(channel, len);
    caml_really_putblock(channel, compressed_data, compressed_data_size);
    Unlock(channel);
    free(compressed_data);
    CAMLreturn(Val_unit);
}

value lm_input_value_lz4(value vchannel)
{
    CAMLparam1(vchannel);

    intnat compressed_data_size, data_size, len;

    struct channel * channel = Channel(vchannel);

    Lock(channel);
    compressed_data_size = caml_getword(channel);
    data_size = caml_getword(channel);
    char* compressed = malloc((size_t)compressed_data_size);
    if (compressed == NULL)
        caml_raise_out_of_memory();
    len = caml_really_getblock(channel, compressed, compressed_data_size);
    Unlock(channel);

    char* const regen_buffer = malloc(data_size);
    if (regen_buffer == NULL) {
        free(compressed);
        caml_raise_out_of_memory();
    }
    const int decompressed_size = LZ4_decompress_safe(compressed, regen_buffer, compressed_data_size, data_size);
    free(compressed);
    if (decompressed_size != data_size) {
        free(regen_buffer);
        caml_failwith("LZ4: compression failed.");
    }
    CAMLreturn(caml_input_value_from_malloc(regen_buffer, 0));
}
#else /* LZ4_ENABLED */

#include <stdlib.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/intext.h>
#include <caml/io.h>
#include <caml/fail.h>

value lm_output_value_lz4(value vchannel, value v, value flags)
{
    CAMLparam3(vchannel, v, flags);
    caml_failwith("LZ4 not supported!");
    CAMLreturn(Val_unit);
}

value lm_input_value_lz4(value vchannel)
{
    CAMLparam1(vchannel);
    caml_failwith("LZ4 not supported!");
    CAMLreturn(Val_unit);
}
#endif /* LZ4_ENABLED */
