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
 * Created On      : Tue Jan 11 14:11:30 1994
 * Last Modified By: Chris Liebman
 * Last Modified On: Sun Feb 13 17:02:35 1994
 * Update Count    : 51
 * Status          : Released
 * 
 * HISTORY
 * 13-Feb-1994		Chris Liebman	
 *    Last Modified: Sat Feb 12 22:31:05 1994 #48 (Chris Liebman)
 *    Modified the way that annotations are handled.  Removed sounds from
 *    the face structure. Added a list of mail items using face to the
 *    face struct.
 *
 * 2-Feb-1994		Chris Liebman	
 *    Last Modified: Tue Feb  1 14:03:54 1994 #27 (Chris Liebman)
 *    Added Annotation support.
 *
 * 31-Jan-1994		Chris Liebman	
 *    Last Modified: Sat Jan 29 22:31:04 1994 #22 (Chris Liebman)
 *    Added support to keep the face order by deleting and recreating
 *    the widgets each check if the keepOrder resource is true.
 *    Modified the initial nomail image and sound calls in
 *    FaceDisplayInit().
 *
 * 24-Jan-1994		Chris Liebman	
 *    Last Modified: Sun Jan 23 17:26:17 1994 #13 (Chris Liebman)
 *    Changed the NoMail to NoMailWidget and exported.
 *
 * 20-Jan-1994		Chris Liebman	
 *    Last Modified: Tue Jan 18 20:48:03 1994 #5 (Chris Liebman)
 *    Fixed a problem where the pixmap variable in two functions was
 *    used before it was set and caused X error on some machines.
 * 
 * PURPOSE
 * 	Manage the face list.
*/

#ifndef lint
static char *RCSid = "$Id: face_display.c,v 1.15 1994/02/14 04:18:53 liebman Exp $";
#endif

#include "faces.h"

/*
 * The list of displayed faces.
*/

Face *TheFaceList;
Face *TheFaceListTail;

/*
 *   Count of faces displayed.
*/

static int TheFaceCount;

/*
 * This is the nomail widget.
*/

Widget		NoMailWidget;

static void
FaceMakeWidget(face)
Face*	face;
{
    if (face->pixmap != None)
    {
	face->widget = XtVaCreateWidget(face->label,
					labelWidgetClass,
					TheFrame,
					XtNbitmap, face->pixmap,
					NULL);
    }
    else
    {
	face->widget = XtVaCreateWidget(face->label,
					labelWidgetClass,
					TheFrame,
					NULL);
    }
    
    XtRealizeWidget(face->widget);
    XtManageChild(face->widget);
}

static void
FaceDestroyWidget(face)
Face* face;
{
    /*
     * If there is a widget here then destroy it.
    */
    
    if (face->widget != NULL)
    {
	XtUnmanageChild(face->widget);
	XtDestroyWidget(face->widget);
	face->widget = NULL;
    }
}

static void
FaceItemClear(face)
Face*		face;
{
    FaceItem* fi;
    FaceItem* next;
    
    for(fi = face->items; fi != NULL; fi = next)
    {
	next = fi->next;
	
	XtFree((char*)fi);
    }
    
    face->items = NULL;
}

static void
FaceItemAdd(face, item)
Face*		face;
MailItem*	item;
{
    FaceItem*	fi = XtNew(FaceItem);
    
    fi->item = item;
    fi->next = face->items;
    face->items = fi;
}

/*
 * Allocate a face entry.
*/

static Face *
FaceCreate(item)
MailItem*	item;
{
    Face	*face;
    
    face = (Face*) XtCalloc(1, sizeof(Face));
    face->pixmap = None;
    face->shape  = None;
    
    face->label  = XtNewString(item->label);
    
    /*
     *    Install and ref image.
    */
    
    face->image = item->image;
    FaceImageRef(face->image);
    
    FaceItemAdd(face, item);
    
    /*
     *  Put the new image on the list.
    */
    
    if (TheFaceList == NULL)
    {
	TheFaceList = face;
    }
    else
    {
	face->next = NULL;
	face->prev = TheFaceListTail;
	face->prev->next = face;
    }
    
    TheFaceListTail = face;
    
    /*
     * Bump list count (count of times image is in list.)
    */
    
    FaceImageCount(face->image);

    if (!TheFacesResources.keep_order)
    {
	/*
	 * Construct the widget.
	*/
	
	FaceMakeWidget(face);
	
	if (TheFaceCount == 0)
	{
	    XtUnmanageChild(NoMailWidget);
	}
    }
    
    /*
     *    Bump the face count.
    */
    
    TheFaceCount += 1;
    
    return (face);
}

/*
 * Free a face structure.
*/

static void
FaceFree(face)
Face	*face;
{
    /*
     * The previous face is now previous to the next face.
    */
    
    if (face->next)
    {
	face->next->prev = face->prev;
    }
    
    /*
     * The next face is now next from the previous face.
    */
    
    if (face->prev)
    {
	face->prev->next = face->next;
    }
    
    /*
     * If this was the first face then the next face is
     * first.
    */
    
    if (face == TheFaceList)
    {
	TheFaceList = face->next;
    }
    
    /*
     * If this was the last face then the prev face is
     * last.
    */
    
    if (face == TheFaceListTail)
    {
	TheFaceListTail = face->prev;
    }
    
    /*
     * Wipe out old links.
    */
    
    face->next = NULL;
    face->prev = NULL;
    
    /*
     * Decrement the face count.
    */
    
    --TheFaceCount;
    
    if (TheFaceCount == 0)
    {
	XtManageChild(NoMailWidget);
    }

    FaceItemClear(face);
    FaceDestroyWidget(face);
    
    /*
     *   Free any image.  Note that FaceImageFree() will decrement the 
     * refs count.
    */
    
    FaceImageDecount(face->image);
    FaceImageFree(face->image);
    
    /*
     *  Free up any annotation.
    */
    
    FaceAnnotateFree(face);
    XtFree(face->label);
    
    /*
     * Now free the face.
    */
    
    XtFree((void *)face);
}

/*
 * Add a new face image to the display list.
*/

Face*
FaceDisplay(item)
MailItem*	item;
{
    Face	*face;
    
    /*
     *   Find face in list.
    */
    
    for (face = TheFaceList; face != NULL; face = face->next)
    {
	/*
	 *   Does the labels match?
	*/
	
	if (strcmp(face->label, item->label) == 0)
	{
	    /*
	     *    Yep, if we are not compressing faces and this one is 
	     * already used then keep looking!
	    */
	    
	    if (!TheFacesResources.compress_images && (face->count > 0))
	    {
		continue;
	    }
	    
	    break;
	}
    }
    
    if (face == NULL)
    {
	face = FaceCreate(item);
    }
    else
    {
	FaceItemAdd(face, item);
    }
    
    face->count += 1;
    
    return face;
}

/*
 * Clear the count on all faces.
*/

void
FaceClear()
{
    Face *face;
    
    if (TheFacesResources.keep_order)
    {
	XtManageChild(NoMailWidget);
    }
    
    for (face = TheFaceList; face != NULL; face = face->next)
    {
        face->last_count = face->count;
	face->count = 0;
	
	FaceItemClear(face);
	
	if (TheFacesResources.keep_order)
	{
	    FaceDestroyWidget(face);
	}
    }
}

/*
 * Weed out any faces with zero counts.  This also re-annotates faces and
 * creates widgets if keep_order is set.
*/

void
FaceClean()
{
    Face *face;
    Face *next_face;
    
    for (face = TheFaceList; face != NULL; face = next_face)
    {
	next_face = face->next;
	
	/*
	 *  Get rid of any unused faces.
	*/
	
	if (face->count == 0)
	{
	    FaceFree(face);
	    continue;
	}
	
	/*
	 * If we need to preserve the order then we must re-create the widget.
	*/
	
	if (TheFacesResources.keep_order)
	{
	    FaceAnnotate(face);
	    FaceMakeWidget(face);
	    XtUnmanageChild(NoMailWidget);
	}
	
	/*
	 *  If the annotations were updated then change the image.
	*/
	
	if (FaceAnnotate(face))
	{
	    XtVaSetValues(face->widget, XtNbitmap, face->pixmap, NULL);
	}
    }
}

/*
 * Count active faces.
*/

int
FaceCount()
{
    Face *face;

    for (face = TheFaceList; face != NULL; face = face->next )
    {
	if ( face->count != face->last_count && face->count )
	{
            return( True );
	}
    }

    return( False );
}


/*
 * Initialize the faces display.
*/

void
FaceDisplayInit()
{
    Pixmap	pixmap = None;
    MailItem	item;
    
    if (FaceImageLoad(TheFacesResources.no_mail_image, &item, NULL))
    {
	NoMailImage = item.image;
	pixmap = FaceImagePixmap(NoMailImage);
    }
    
#ifdef SOUND
    if ((TheFacesResources.no_mail_sound != NULL) &&
	FaceSoundLoad(TheFacesResources.no_mail_sound, &item, NULL))
    {
	NoMailSound = item.sound;
    }
    else
    {
	NoMailSound = NULL;
    }
#endif /* SOUND */
    
    /*
     * Create the nomail widget.
    */
    
    if (pixmap != None)
    {
	NoMailWidget = XtVaCreateManagedWidget("nomail",
					       labelWidgetClass,
					       TheFrame,
					       XtNbitmap, pixmap,
					       NULL);
    }
    else
    {
	NoMailWidget = XtVaCreateManagedWidget("nomail",
					       labelWidgetClass,
					       TheFrame,
					       NULL);
    }
}
