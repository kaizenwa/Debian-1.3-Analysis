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
 * Created On      : Sun Jan 23 12:03:58 1994
 * Last Modified By: Chris Liebman
 * Last Modified On: Sat Jan 29 23:00:33 1994
 * Update Count    : 48
 * Status          : Released
 * 
 * HISTORY
 * 
 * PURPOSE
 * 	Handle X-Face header images.
*/

#ifndef lint
static char *RCSid = "$Id: face_search_xface.c,v 1.1 1994/02/01 03:53:20 liebman Exp $";
#endif

#include "faces.h"
#include "face_search.h"

static int
FaceSearchXFace(item, data)
MailItem*	item;
FaceSearchData*	data;
{
    MailHeader*		xface;

    /*
     * X-Face header is only valid for image!
    */
    
    if (data->format != FormatImage)
    {
	return 0;
    }
    
    xface = MailHeaderFind("X-Face:", item->headers);
    
    if (xface != NULL)
    {
	item->image = FaceImageXFaceCreate(xface->value);
	
	if (item->image)
	{
	    return 1;
	}
    }
    
    return 0;
}

static FaceSearchType xface =
{
    "x-face",
    FaceSearchXFace,
    NULL,
    NULL
};

void
FaceSearchXFaceInit()
{
    FaceSearchTypeRegister(&xface);
}
