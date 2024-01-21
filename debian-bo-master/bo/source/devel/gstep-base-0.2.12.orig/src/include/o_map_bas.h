/* Basic functions for map structures.
 * Copyright (C) 1995, 1996  Free Software Foundation, Inc.
 * 
 * Author: Albin L. Jones <Albin.L.Jones@Dartmouth.EDU>
 * Created: Mon Dec 11 01:24:48 EST 1995
 * Updated: Mon Mar 11 00:54:50 EST 1996
 * Serial: 96.03.11.03
 * 
 * This file is part of the GNUstep Base Library.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 * 
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. */ 

#ifndef __map_bas_h_OBJECTS_INCLUDE
#define __map_bas_h_OBJECTS_INCLUDE 1

/**** Included Headers *******************************************************/

#include <stdlib.h>
#include <Foundation/NSZone.h>
#include <Foundation/NSString.h>
#include <gnustep/base/numbers.h>

/**** Type, Constant, and Macro Definitions **********************************/

#define __map__ 1

/**** Function Implementations ***********************************************/

/** Magic numbers... **/

/* Returns XX's magic number. */
int
o_map_magic_number(o_map_t *xx);

/** Zones... **/

/* Returns the allocs used to create and maintain XX. */
NSZone *
o_map_zone(o_map_t *xx);

/** Names... **/

/* Returns the name that was given to XX. */
NSString *
o_map_name(o_map_t *xx);

/* Gives XX a name.  */
void
o_map_set_name(o_map_t *xx, NSString *name);

/* Takes away XX's name. */
void
o_map_unset_name(o_map_t *xx);

/** Serial numbers... **/

/* Returns the (process-wide) unique number given to the Libfn
 * structure XX.  See <gnustep/base/numbers.h> for more info. */
size_t
o_map_serial_number(o_map_t *xx);

/* Gives XX a new (process-wide) unique number.  Numbers are not
 * reused.  See <gnustep/base/numbers.h> for more info. */
size_t
_o_map_set_serial_number(o_map_t *xx);

/** Extras... **/

/* Sets the callbacks associated with XX's ``extra''.  NOTE: This must
 * be done before calling `o_map_set_extra()', as these callbacks
 * are used in that routine. */
o_callbacks_t
o_map_set_extra_callbacks(o_map_t *xx,
                                 o_callbacks_t callbacks);

/* Returns the callbacks associated with XX's ``extra''. */
o_callbacks_t
o_map_extra_callbacks(o_map_t *xx);

/* Returns XX's ``extra'', a little extra space that each
 * structure carries around with it.  Its use is
 * implementation-dependent. */
const void *
o_map_extra(o_map_t *xx);

/* Sets XX's ``extra'', a little extra space that each structure
 * carries around with it.  Its use is implementation-dependent. */
const void *
o_map_set_extra(o_map_t *xx, const void *extra);

/* Resets XX's ``extra''. */
void
o_map_unset_extra(o_map_t *xx);

/** Low-level Creation and Destruction **/

/* Handles the universal, low-level allocation of structures. */
o_map_t *
_o_map_alloc_with_zone(NSZone *zone);

/* Handles the universal, low-level deallocation of structures. */
void
_o_map_dealloc(o_map_t *xx);

/* Handles the low-level copying of structures. */
o_map_t *
_o_map_copy_with_zone(o_map_t *xx, NSZone *zone);

/* Returns a low-level description of XX. */
NSString *
_o_map_description(o_map_t *xx);

#endif /* __map_bas_h_OBJECTS_INCLUDE */

