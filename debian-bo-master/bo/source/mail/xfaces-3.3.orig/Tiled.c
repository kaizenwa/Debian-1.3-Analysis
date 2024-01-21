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
 * Last Modified On: Sun Feb 13 16:58:19 1994
 * Update Count    : 8
 * Status          : Released
 * 
 * HISTORY
 * 13-Feb-1994		Chris Liebman	
 *    Last Modified: Sat Feb  5 20:43:46 1994 #6 (Chris Liebman)
 *    Allow this file to compile with a non-ansi compiler.
 * 
 * PURPOSE
 * 	Simple tiled layout widget
*/


#ifndef lint
static char RCSid[] = "$Id: Tiled.c,v 1.5 1994/03/07 20:15:55 liebman Exp $";
#endif  /* lint */

#include "TiledP.h"
#include "X11/Composite.h"
#include "X11/CompositeP.h"
#include <X11/Xmu/CharSet.h>

#define superclass	    ((CompositeWidgetClass)&compositeClassRec)

#ifdef __STDC__
#define P_(x)	x
#else
#define	P_(x)	(/* x */)
#endif

/*.
 ***************************************************************************

   Functional Prototypes. 

 ***************************************************************************
 */

static Boolean		SetValues P_((Widget 			gcurrent,
				      Widget			grequest,
				      Widget			gnew,
				      ArgList 			args,
				      Cardinal 			*num_args));

static Boolean		AcceptFocus P_((Widget			w,
					Time			*time));

static void		Layout P_((TiledWidget			tw));

static XtGeometryResult	GeometryHandler P_((Widget			w,
					    XtWidgetGeometry	*request,
					    XtWidgetGeometry	*result));

static void		ChangeManaged P_((Widget 		widget));

static void		InsertChild P_((Widget			new));

static void		DeleteChild P_((Widget			old));

static Boolean defTrue = True;

#define offset(field) 			XtOffsetOf(TiledRec, field)

static XtResource resources[] =
{
    {
	XtNvertSpacing, XtCHeight, XtRDimension, sizeof(Dimension),
	offset(tiled.vert_spacing), XtRImmediate, (XtPointer) 0
    },
    {
	XtNhorizSpacing, XtCHeight, XtRDimension, sizeof(Dimension),
	offset(tiled.horiz_spacing), XtRImmediate, (XtPointer) 0
    },
    {
	XtNinternalWidth, XtCWidth, XtRDimension, sizeof(Dimension),
	offset(tiled.internal_width), XtRImmediate, (XtPointer) 0
    },
    {
	XtNinternalHeight, XtCHeight, XtRDimension, sizeof(Dimension),
	offset(tiled.internal_height), XtRImmediate, (XtPointer) 0
    },
    {
	XtNtileWidth, XtCWidth, XtRDimension, sizeof(Dimension),
	offset(tiled.tile_width), XtRImmediate, (XtPointer) 64
    },
    {
	XtNtileHeight, XtCHeight, XtRDimension, sizeof(Dimension),
	offset(tiled.tile_height), XtRImmediate, (XtPointer) 64
    },
    {
	XtNsetWidth, XtCWidth, XtRDimension, sizeof(Dimension),
	offset(tiled.set_width), XtRImmediate, (XtPointer) 0
    },
    {
	XtNsetHeight, XtCHeight, XtRDimension, sizeof(Dimension),
	offset(tiled.set_height), XtRImmediate, (XtPointer) 0
    },
    {
	XtNminWidth, XtCWidth, XtRDimension, sizeof(Dimension),
	offset(tiled.min_width), XtRImmediate, (XtPointer) 0
    },
    {
	XtNminHeight, XtCHeight, XtRDimension, sizeof(Dimension),
	offset(tiled.min_height), XtRImmediate, (XtPointer) 0
    },
    {
	XtNmaxWidth, XtCWidth, XtRDimension, sizeof(Dimension),
	offset(tiled.max_width), XtRImmediate, (XtPointer) 0
    },
    {
	XtNmaxHeight, XtCHeight, XtRDimension, sizeof(Dimension),
	offset(tiled.max_height), XtRImmediate, (XtPointer) 0
    },
    {
	XtNlayout, XtCLayout, XtRBoolean, sizeof(Boolean),
	offset(tiled.layout), XtRBoolean, (caddr_t) &defTrue
    }
};

#undef offset

static void
NewSize(tw)
TiledWidget	tw;
{
    int		internal_width  = tw->tiled.internal_width;
    int		internal_height = tw->tiled.internal_height;
    int		vert_spacing	= tw->tiled.vert_spacing;
    int		horiz_spacing	= tw->tiled.horiz_spacing;
    int		tile_width	= tw->tiled.tile_width;
    int		tile_height	= tw->tiled.tile_height;
    int		min_width	= tw->tiled.min_width;
    int		min_height	= tw->tiled.min_height;
    int		max_width	= tw->tiled.max_width;
    int		max_height	= tw->tiled.max_height;
    int		width_in_tiles	= tw->tiled.set_width;
    int		height_in_tiles	= tw->tiled.set_height;
    int		tile_count	= tw->tiled.tile_count;
    Dimension	width;
    Dimension	height;
    
    /*
     *  If there is no set width then calulate the preferred width in tiles;
    */
    
    if (width_in_tiles == 0)
    {
	width_in_tiles = tile_count;
	
	if ((max_width  > 0) && (width_in_tiles > max_width))
	{
	    width_in_tiles = max_width;
	}
	else if ((min_width > 0) && (width_in_tiles < min_width))
	{
	    width_in_tiles = min_width;
	}
    }
    
    /*
     *  If there is no set height then calulate the preferred height in tiles;
    */
    
    if (height_in_tiles == 0)
    {
	/*
	 *  Calculate what we want based upon the width in tiles and the
	 * tile count.
	*/
	
	height_in_tiles = (tile_count / width_in_tiles);

	if ((height_in_tiles * width_in_tiles) < tile_count)
	{
	    height_in_tiles += 1;
	}
	
	if ((max_height  > 0) && (height_in_tiles > max_height))
	{
	    height_in_tiles = max_height;
	}
	else if ((min_height > 0) && (height_in_tiles < min_height))
	{
	    height_in_tiles = min_height;
	}
    }
    
    /*
     *  Compute width and height in pixels.
    */
    
    width  = (2 * internal_width +
	      width_in_tiles * tile_width +
	      (width_in_tiles - 1) * horiz_spacing);
    height = (2 * internal_height +
	      height_in_tiles * tile_height +
	      (height_in_tiles - 1) * vert_spacing);

    /*
     *  If this is already our size then we can stop now!
    */

    if ((tw->core.width  == width) &&
	(tw->core.height == height))
    {
	return;
    }
    
    /*
     * Now ask our parrent for this size.
    */
    
    switch (XtMakeResizeRequest((Widget)tw, width, height,
			&width, &height))
    {
      case XtGeometryYes:
	break;
	
      case XtGeometryNo:
	/*
	 *   Said no!  Keep the current size!
	*/
	
	width  = tw->core.width;
	height = tw->core.height;
	break;
	
      case XtGeometryAlmost:
	/*
	 *  We take whatever we can get!  (parent must accept as was returned
	 * "almost")
	*/
	
	(void)XtMakeResizeRequest((Widget)tw, width, height, &width, &height);
	break;
	
      default:
	/*
	 *  Don't understand result!
	*/
	
	width  = tw->core.width;
	height = tw->core.height;
	break;
    }
    
    /*
     * recompute tile size based upon new width and height.
     *
     * First remove internal_width.
    */
    
    width -= (2 * internal_width);
    
    /*
     * Stick in an extra horiz_spacing to compensate for the extra one in the
     * next calculation.
    */
    
    width += horiz_spacing;
    
    /*
     *   Compute the new width in tiles.
    */
    
    width_in_tiles = width / (tile_width + horiz_spacing);
    
    if (width_in_tiles < 1)
    {
	width_in_tiles = 1;
    }
    
    tw->tiled.width_in_tiles = width_in_tiles;

    /*
     * Remove internal height.
    */
    
    height -= (2 * internal_height);

    /*
     * Stick in an extra horiz_spacing to compensate for the extra one in the
     * next calculation.
    */
    
    height += vert_spacing;
    
    /*
     *   Compute the new height in tiles.
    */
    
    height_in_tiles = height / (tile_height + vert_spacing);
    
    if (height_in_tiles < 1)
    {
	height_in_tiles = 1;
    }
    
    tw->tiled.height_in_tiles = height_in_tiles;
}

static void
Layout(tw)
TiledWidget tw;
{
    int		i, pos;
    Position	x, y;
    Widget	child;
    int		internal_width  = tw->tiled.internal_width;
    int		internal_height = tw->tiled.internal_height;
    int		vert_spacing	= tw->tiled.vert_spacing;
    int		horiz_spacing	= tw->tiled.horiz_spacing;
    int		tile_width	= tw->tiled.tile_width;
    int		tile_height	= tw->tiled.tile_height;
    int		width_in_tiles  = tw->tiled.width_in_tiles;
    Dimension	width, height;
    
    /*
     * Compute the new positions.
    */
    
    for ( i = 0, pos = 0; i < tw->composite.num_children; i++ )
    {
	child = tw->composite.children[i];
	
	/*
	 * Ignore unmanaged children!
	*/
	
	if (!XtIsManaged( child ))
	{
	    continue;
	}
	
	/*
	 * Compute the position for the child.
	*/
	
	x = (pos % width_in_tiles) * tile_width
	    + (pos % width_in_tiles) * horiz_spacing + internal_width;
	
	y = (pos / width_in_tiles) * tile_height
	    + (pos / width_in_tiles) * vert_spacing + internal_height;
	
	x -= child->core.border_width;
	y -= child->core.border_width;

	/*
	 * We allow children to be smaller than the tile size but *not*
	 * bigger.
	*/

	height = child->core.height;
	if (height > tile_height)
	{
	    height = tile_height;
	}

	width = child->core.width;
	if (width > tile_width)
	{
	    width = tile_width;
	}
	
	XtConfigureWidget(child, x, y, width, height,
			  child->core.border_width);
	
	++pos;
    }
    
    return;
}

static Boolean
SetValues(gcurrent, grequest, gnew, args, num_args)
Widget 		gcurrent;
Widget		grequest;
Widget		gnew;
ArgList 	args;
Cardinal	*num_args;
{
    TiledWidget 	current = (TiledWidget) gcurrent;
    TiledWidget 	new = (TiledWidget) gnew;
    
    /*
     * If the tile size or internal width/height or itemSpacing changes then
     * call Layout().
    */
    
    if ((current->tiled.vert_spacing    != new->tiled.vert_spacing) ||
	(current->tiled.horiz_spacing    != new->tiled.horiz_spacing) ||
	(current->tiled.internal_width  != new->tiled.internal_width) ||
	(current->tiled.internal_height != new->tiled.internal_height) ||
	(current->tiled.tile_width      != new->tiled.tile_width) ||
	(current->tiled.tile_height     != new->tiled.tile_height) ||
	(current->tiled.set_width       != new->tiled.set_width) ||
	(current->tiled.set_height      != new->tiled.set_height) ||
	(current->tiled.min_width       != new->tiled.min_width) ||
	(current->tiled.min_height      != new->tiled.min_height) ||
	(current->tiled.max_width       != new->tiled.max_width) ||
	(current->tiled.max_height      != new->tiled.max_height))
    {
	if (new->tiled.layout)
	{
	    NewSize(new);
	    Layout(new);
	}
    }
    else if (!current->tiled.layout && new->tiled.layout)
    {
	NewSize(new);
	Layout(new);
    }
    
    return( FALSE );
}

static Boolean
AcceptFocus(w, time)
Widget	w;
Time	*time;
{
    return( True );
}


/*
 * The geometry handler.
*/
static XtGeometryResult
GeometryHandler(w, request, result)
Widget			w;		/* Child widget. */
XtWidgetGeometry	*request;
XtWidgetGeometry	*result;
{
    if (!(request->request_mode & XtCWQueryOnly))
    {
	if (request->request_mode & CWX)
	{
	    w->core.x = request->x;
	}
	if (request->request_mode & CWY)
	{
	    w->core.y = request->y;
	}
	if (request->request_mode & CWWidth)
	{
	    w->core.width = request->width;
	}
	if (request->request_mode & CWHeight)
	{
	    w->core.height = request->height;
	}
	if (request->request_mode & CWBorderWidth)
	{
	    w->core.border_width = request->border_width;
	}
    }
    
    return( XtGeometryYes );
}

static void
ChangeManaged(widget)
Widget	widget;
{
    TiledWidget		tw = (TiledWidget)widget;
    int			count;
    int			i;
    Widget		child;
    
    for ( i = 0, count = 0; i < tw->composite.num_children; i++ )
    {
	child = tw->composite.children[i];
	
	if (XtIsManaged( child ))
	{
	    ++count;
	}
    }
    
    tw->tiled.tile_count = count;
    
    if (tw->tiled.layout)
    {
	NewSize(tw);
	Layout(tw);
    }
    
    return;
}

static void
InsertChild(new)
Widget new;
{
    /*
     * Insert the child widget in the composite children list with the
     * superclass insert_child routine.
    */
    
    (*superclass->composite_class.insert_child)(new);
    return;
}

static void
DeleteChild(old)
Widget old;
{
    TiledWidget		tw;
    
    tw = (TiledWidget)XtParent( old );
    
    if (tw->tiled.layout)
    {
	NewSize(tw);
	Layout(tw);
    }
    
    /*
     * Delete the child widget in the composite children list with the
     * superclass delete_child routine.
    */
    
    (*superclass->composite_class.delete_child)(old);
    
    return;
}


/*.
 ***************************************************************************

   Full class record constant.

 ***************************************************************************
 */

TiledClassRec tiledClassRec =
{
    {
	/* core_class fields */	
	(WidgetClass)superclass,	    /* superclass               */
	"Tiled",			    /* class_name               */
	sizeof(TiledRec),		    /* widget_size              */
	NULL,				    /* class_initialize         */
	NULL,				    /* class_part_initialize    */
	FALSE,				    /* class_inited             */
	NULL,				    /* initialize               */
	NULL,				    /* initialize_hook          */
	XtInheritRealize,		    /* realize                  */
	NULL,				    /* actions                  */
	0,				    /* num_actions              */
	resources,			    /* resources                */
	XtNumber(resources),		    /* num_resources            */
	NULLQUARK,			    /* xrm_class                */
	TRUE,				    /* compress_motion          */
	TRUE,				    /* compress_exposure        */
	TRUE,				    /* compress_enterleave      */
	FALSE,				    /* visible_interest         */
	NULL,				    /* destroy                  */
	XtInheritResize,		    /* resize                   */
	NULL,				    /* expose                   */
	SetValues,			    /* set_values               */
	NULL,				    /* set_values_hook          */
	XtInheritSetValuesAlmost,	    /* set_values_almost        */
	NULL,				    /* get_values_hook          */
	AcceptFocus,			    /* accept_focus             */
	XtVersion,			    /* version                  */
	NULL,				    /* callback_private         */
	NULL,				    /* tm_table                 */
	XtInheritQueryGeometry,		    /* query_geometry           */
	XtInheritDisplayAccelerator,	    /* display_accelerator      */
	NULL				    /* extension                */
    },
    {
	/**** CompositePart *****/
	GeometryHandler,		    /* geometry_handler     */
	ChangeManaged,			    /* change_managed       */
	InsertChild,			    /* insert_child         */
	DeleteChild,			    /* delete_child         */
	NULL				    /* extension            */
    },
    {
	0,				    /* unused. */
    }
    
};

WidgetClass tiledWidgetClass = (WidgetClass)&tiledClassRec;
