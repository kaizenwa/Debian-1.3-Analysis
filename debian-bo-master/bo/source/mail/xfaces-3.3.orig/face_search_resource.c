/*                             -*- Mode: C++-C -*- 
 * 
 *  
 * 	     Copyright 1994 Christopher B. Liebman
 * 
 *  Permission to use, copy, modify, distribute, and sell this software
 *  and its documentation for any purpose is hereby granted without fee,
 *  provided that the above copyright notice appear in all copies and that
 *  both that copyright notice and this permission notice appear in
 *  supporting documentation, and that the name Christopher B. Liebman not
 *  be used in advertising or publicity pertaining to distribution of this
 *  software without specific, written prior permission.
 * 
 * THIS SOFTWARE IS PROVIDED `AS-IS'.  CHRISTOPHER B. LIEBMAN, DISCLAIMS
 * ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 * LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL CHRISTOPHER
 * B. LIEBMAN, BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING SPECIAL,
 * INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA, OR
 * PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 * WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 * 
 * 
 * 
 * Author          : Chris Liebman
 * Created On      : Sat Jan 29 15:17:46 1994
 * Last Modified By: Chris Liebman
 * Last Modified On: Sat Feb 19 22:55:49 1994
 * Update Count    : 60
 * Status          : Released
 * 
 * HISTORY
 * 13-Feb-1994		Chris Liebman	
 *    Last Modified: Sat Feb 12 23:28:52 1994 #42 (Chris Liebman)
 *    Added FormatCommand support.
 *
 * 
 * PURPOSE
 * 	Resource search.
*/

#ifndef lint
static char *RCSid = "$Id: face_search_resource.c,v 1.4 1994/03/07 20:26:32 liebman Exp $";
#endif

#include "faces.h"
#include "face_search.h"

static int
FaceSearchResourceWork(fullname, fullclass, item, data)
char*		fullname;
char*		fullclass;
MailItem*	item;
FaceSearchData*	data;
{
    int		ret = 0;
    XrmDatabase	db;
    String	type = NULL;
    XrmValue	value;

#ifdef RESOURCE_DEBUG
    fprintf(stderr, "FaceSearchResourceWork: looking for: <%s>/<%s>.\n",
	    fullname, fullclass);
#endif
    
#if (XtSpecificationRelease > 4)
    db = XtScreenDatabase(XtScreen(TheTopLevel));
#else
    db = XtDatabase(XtDisplay(TheTopLevel));
#endif
    
    if (XrmGetResource(db, fullname, fullclass, &type, &value) &&
	(strcmp(type, XtRString) == 0))
    {
	ret = FaceSearchLoad(value.addr, item, data);
    }
    
    return ret;
}

static int
FaceSearchResource(item, data)
MailItem*	item;
FaceSearchData*	data;
{
    char*	fullname;
    char*	fullclass;
    char*	appname = XtName(TheTopLevel);
    char* 	typename;
    char* 	typeclass;
    int		reallen = 0;
    
    switch(data->format)
    {
      case FormatImage:
	typename  = "image";
	typeclass = "Image";
	break;

      case FormatAudio:
	typename  = "sound";
	typeclass = "Sound";
	break;

      case FormatCommand:
	typename  = "command";
	typeclass = "Command";
	break;
	
      default:
	return 0;
    }
    
    /*
     *    Make a buffer for resource name construction.
    */
    
    if (item->realhost != NULL)
    {
	reallen = strlen(item->realhost);
    }
    
    fullname  = XtMalloc(strlen(appname) + strlen(typename) +
			 strlen(item->user)   + strlen(item->host) +
			 reallen + 9);
    fullclass = XtMalloc(strlen(XFACES_CLASS) + strlen(typeclass)  +
			 strlen("User@Host") + 9);
    
    /*
     *    Build a resource name for this sucker.
    */
    
    sprintf(fullname, "%s.%s.%s@%s",
	    appname, typename, item->user, item->host);
    sprintf(fullclass, "%s.%s.User@Host",
	    XFACES_CLASS, typeclass);
    
    /*
     *    Try to load the thing in.
    */
    
    if (FaceSearchResourceWork(fullname, fullclass, item, data))
    {
	XtFree(fullname);
	XtFree(fullclass);
	return 1;
    }

    /*
     *    try the real (looked up) hostname.
    */
    
    if (item->realhost != NULL)
    {
	
	sprintf(fullname, "%s.%s.%s@%s",
		appname, typename, item->user, item->realhost);
	
	/*
	 *    Try to load the thing in.
	*/
	
	if (FaceSearchResourceWork(fullname, fullclass, item, data))
	{
	    XtFree(fullname);
	    XtFree(fullclass);
	    return 1;
	}
    }
    
    /*
     *   Oh well.  Lets try for just the user name.
    */
    
    sprintf(fullname, "%s.%s.%s", appname, typename, item->user);
    sprintf(fullclass, "%s.%s.User", XFACES_CLASS, typeclass);
    
    if (FaceSearchResourceWork(fullname, fullclass, item, data))
    {
	XtFree(fullname);
	XtFree(fullclass);
	return 1;
    }
    
    /*
     *   Oh well.  Lets try for just the host name.
    */
    
    sprintf(fullname, "%s.%s.%s", appname, typename, item->host);
    sprintf(fullclass, "%s.%s.Host", XFACES_CLASS, typeclass);
    
    if (FaceSearchResourceWork(fullname, fullclass, item, data))
    {
	XtFree(fullname);
	XtFree(fullclass);
	return 1;
    }
    
    /*
     *   Oh well.  Lets try for just the  real (looked up) host name.
    */

    if (item->realhost != NULL)
    {
	sprintf(fullname, "%s.%s.%s", appname, typename, item->realhost);
	
	if (FaceSearchResourceWork(fullname, fullclass, item, data))
	{
	    XtFree(fullname);
	    XtFree(fullclass);
	    return 1;
	}
    }
    
    XtFree(fullname);
    XtFree(fullclass);
    
    /*
     *   Oh well...
    */
    
    return(0);
}

static FaceSearchType resource =
{
    "resource",
    FaceSearchResource,
    NULL,
    NULL,
};

void
FaceSearchResourceInit()
{
    FaceSearchTypeRegister(&resource);
}
