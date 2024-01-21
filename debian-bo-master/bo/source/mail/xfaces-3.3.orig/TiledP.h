/*                             -*- Mode: C++-C -*- 
 * 
 *		 Copyright 1994 Christopher B. Liebman
 *
 *     Permission to use, copy, modify, distribute, and sell this software
 *     and its documentation for any purpose is hereby granted without fee,
 *     provided that the above copyright notice appear in all copies and that
 *     both that copyright notice and this permission notice appear in
 *     supporting documentation, and that the name Christopher B. Liebman not
 *     be used in advertising or publicity pertaining to distribution of this
 *     software without specific, written prior permission.
 *
 *    THIS SOFTWARE IS PROVIDED `AS-IS'.  CHRISTOPHER B. LIEBMAN, DISCLAIMS
 *    ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 *    LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 *    PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL CHRISTOPHER
 *    B. LIEBMAN, BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING SPECIAL,
 *    INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA, OR
 *    PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 *    WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF
 *    OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author          : Chris Liebman
 * Created On      : 
 * Last Modified By: Chris Liebman
 * Last Modified On: 
 * Update Count    : 0
 * Status          : Released
 * 
 * HISTORY
 * 
 * PURPOSE
 * 	Simple tiled layout widget
 *
 * $Id: TiledP.h,v 1.2 1994/02/08 18:59:47 liebman Exp $
*/

#ifndef TiledP_h_
#define TiledP_h_

#include "Tiled.h"
#include <X11/IntrinsicP.h>
#include <X11/CompositeP.h>
#include <X11/StringDefs.h>

/*
 * New fields for the Tiled widget class record.
*/

typedef struct
{
    int foo;
} TiledClassPart;

/*
 * Full class record declaration.
*/

typedef struct _TiledClassRec
{
    CoreClassPart	core_class;
    CompositeClassPart	composite_class;
    TiledClassPart	tiled_class;
} TiledClassRec;

extern TiledClassRec tiledClassRec;


typedef struct
{
    /* Resources. */
    Dimension	    	vert_spacing;
    Dimension	    	horiz_spacing;
    Dimension	    	internal_width;
    Dimension	    	internal_height;
    Dimension		tile_width;	/* Width of tiles. */
    Dimension		tile_height;	/* Height of tiles. */
    Dimension		set_width;	/* Forced width in tiles. */
    Dimension		set_height;	/* Forced height in tiles. */
    Dimension		min_width;	/* In tiles. */
    Dimension		min_height;	/* In tiles. */
    Dimension		max_width;	/* In tiles. */
    Dimension		max_height;	/* In tiles. */
    Boolean		layout;
    
    /* Private state. */
    Dimension		tile_count;	/* Number of managed children. */
    Dimension		width_in_tiles;
    Dimension		height_in_tiles;
} TiledPart;

/* Full instance record declaration. */
typedef struct _TiledRec
{
    CorePart		core;
    CompositePart	composite;
    TiledPart		tiled;
} TiledRec;

#endif /* TiledP_h_ */

