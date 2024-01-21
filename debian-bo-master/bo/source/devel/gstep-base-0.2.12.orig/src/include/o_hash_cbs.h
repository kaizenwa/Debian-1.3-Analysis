/* Getting callbacks from hash structures.
 * Copyright (C) 1995, 1996  Free Software Foundation, Inc.
 * 
 * Author: Albin L. Jones <Albin.L.Jones@Dartmouth.EDU>
 * Created: Mon Dec 11 03:41:00 EST 1995
 * Updated: Mon Mar 11 00:54:20 EST 1996
 * Serial: 96.03.11.02
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

#ifndef __hash_cbs_h_OBJECTS_INCLUDE
#define __hash_cbs_h_OBJECTS_INCLUDE 1

/**** Included Headers *******************************************************/

#include <gnustep/base/o_cbs.h>
#include <gnustep/base/o_hash.h>

/**** Type, Constant, and Macro Definitions **********************************/

#define __hash__ 1

/**** Function Implementations ***********************************************/

#ifdef __map__

/** Callbacks **/

/* Returns the callbacks associated with YY's keys. */
o_callbacks_t o_hash_key_callbacks (o_hash_t *yy);

/* Returns the ``bogus'' marker associated with YY's keys. */
const void *o_hash_not_a_key_marker (o_hash_t *yy);

/* Returns the callbacks associated with YY's values. */
o_callbacks_t
o_hash_value_callbacks (o_hash_t *yy);

/* Returns the ``bogus'' marker associated with YY's values. */
const void *o_hash_not_a_value_marker (o_hash_t *yy);

#else /* !__map__ */

/** Callbacks **/

/* Returns the callbacks associated with YY's elements. */
o_callbacks_t o_hash_element_callbacks (o_hash_t *yy);

/* Returns the ``bogus'' marker associated with YY's elements. */
const void *o_hash_not_an_element_marker (o_hash_t *yy);

#endif /* __map__ */

#endif /* __hash_cbs_h_OBJECTS_INCLUDE */

