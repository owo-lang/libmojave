/*
 * Dynamic linking hooks.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2005 Mojave Group, Caltech
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Author: Jason Hickey
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 */
#ifndef _LM_DLL_H
#define _LM_DLL_H

/*
 * The normal magic number.
 */
#define LM_DLL_MAGIC    0xabe5a34d

/*
 * Strings are named by identifiers.
 */
typedef short DllId;

typedef DllId DllString;
typedef DllId DllStringArray;
typedef DllId DllFields;

/*
 * Standard type names.  For values of any other type,
 * just define a string name for it.
 */
typedef DllString DllType;

/*
 * Callbacks are identified by number.
 */
typedef unsigned DllCallbackId;

/*
 * A description of a field in a struct.
 */
typedef struct {
    DllString name;
    DllString type;
} DllField;

/*
 * Arguments are also specified as fields.
 */
typedef DllId DllArgTypes;

/*
 * An object if a collection of fields.
 */
typedef struct {
    DllString name;
    DllFields fields;
} DllObject;

/*
 * Fields in enumerations.
 */
typedef struct {
    DllString name;
    int value;
} DllEnumField;

typedef struct {
    DllString name;
    DllEnumField *fields;
} DllEnumInfo;

/*
 * Type information for values that are exported.
 */
typedef value (*ValueFun)(value);

typedef struct {
    DllString name;
    ValueFun value;
    DllArgTypes arg_types;
    DllType result_type;
} DllValue;

/*
 * Hooks to provide OCaml functions.
 */
typedef struct {
    value (*alloc_hook)(mlsize_t, tag_t);
    value (*alloc_tuple_hook)(mlsize_t);
    value (*alloc_custom_hook)(struct custom_operations *ops, unsigned long size, mlsize_t mem, mlsize_t max);
    value (*copy_string_hook)(const char *s);
    value (*copy_string_array_hook)(const char **s);
    value (*copy_int32_hook)(int i);
    value (*copy_double_hook)(double x);
    value *(*named_value_hook)(char *name);
    value (*callback1_hook)(value, value);
    value (*callback2_hook)(value, value, value);
    value (*callback1_exn_hook)(value, value);
    value (*callback2_exn_hook)(value, value, value);
    void (*register_global_root_hook)(value *);
    void (*modify)(value *, value);
} DllHooks;

/*
 * Every library should define an export list
 * of all its functions.
 */
typedef struct {
    int magic;
    int version;
    value (*initialize_dll)(DllHooks *);
    void (*set_callback_handler)(value handler);
    char **strings;
    DllField **fields;
    DllObject *objects;
    DllEnumInfo *enums;
    DllValue *values;
} DllExport;

/*
 * Default definitions for the internal keywords.
 */
#define __dll_callback  static
#define __dll_hidden

#endif /* _LM_DLL_H */