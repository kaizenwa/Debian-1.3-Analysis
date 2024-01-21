/* Language specific definitions for GPC.
   Copyright (C) 1989 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/*
 * Specifies the initial value associated to the type in
 * Extended Pascal.
 * NOT used currently.
 * Also c-tree.h now defines the lang_type node
 * and since VALUE is not implemented yet, there is no reason
 * to alter the current definition.
 */
#if 0
struct lang_type {
    union tree_node *initial;
};
#endif

/* Language dependent node access.
 * Overload c-tree.h field for record_types
 */
#define PASCAL_CONF_NAMES(type) ((type)->elts[0])

/*
 * GPC Specific extensions to the tree_node DECL part.
 *
 * directive : IDENTIFIER_NODE, the directive
 * parms     : chain of formal parameters of the routine
 *	       (the FUNCTION_DECL contains only the types, not the argument
 *		names)
 */

struct lang_decl {
    union tree_node *directive;
    union tree_node *parms;
};
