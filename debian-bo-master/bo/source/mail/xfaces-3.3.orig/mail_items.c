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
 * Last Modified On: Mon Mar  7 17:49:59 1994
 * Update Count    : 142
 * Status          : Released
 * 
 * HISTORY
 * 6-Mar-1994		Chris Liebman	
 *    Last Modified: Sun Mar  6 22:35:49 1994 #122 (Chris Liebman)
 *    Added customization for mail annotations.
 *
 * 13-Feb-1994		Chris Liebman	
 *    Last Modified: Sat Feb 12 23:17:13 1994 #51 (Chris Liebman)
 *    Added new mail annotation.
 *
 * 2-Feb-1994		Chris Liebman	
 *    Last Modified: Mon Jan 31 23:13:16 1994 #34 (Chris Liebman)
 *    Added annotation support.  This is only hooked up for scripts
 *    currently.
 *
 * 31-Jan-1994		Chris Liebman	
 *    Last Modified: Sat Jan 29 20:40:41 1994 #33 (Chris Liebman)
 *    Added new search support and a function to create an item that
 *    has no headers.
 *
 * 24-Jan-1994		Chris Liebman	
 *    Last Modified: Sun Jan 23 14:04:47 1994 #28 (Chris Liebman)
 *    Moved all of the sound / image searching to other files.
 *
 * 20-Jan-1994		Chris Liebman	
 *    Last Modified: Tue Jan 18 14:38:39 1994 #22 (Chris Liebman)
 *    Added new header parsing and bindings.
 *
 * 14-Jan-1994		Chris Liebman	
 *    Last Modified: Fri Jan 14 10:42:41 1994 #7 (Chris Liebman)
 *    Added new MailItemIgnore() function.
 *    MailItemCreate() now ignores items for which MailItemIgnore()
 *    returns true.
 *    Search the status header with bindings.
 *
 * PURPOSE
 * 	Routines to manage the list of mail items.
*/

#ifndef lint
static char *RCSid = "$Id: mail_items.c,v 1.20 1994/03/08 15:05:57 liebman Exp $";
#endif

#include "faces.h"
#ifdef LOOKUP_HOSTNAME
#include <netdb.h>
#endif

/*
 * The list of mail items from mail box.
*/

MailItem	*TheMailItems;
MailItem	*TheMailItemsTail;

/*
 *
*/

typedef enum mail_annotation_types
{
    MailAnnotateNone,
    MailAnnotateCount,
    MailAnnotateUser,
    MailAnnotateHost,
    MailAnnotateUserHost,
    MailAnnotateHeader
} MailAnnotationType;

typedef struct mail_annotation
{
    MailAnnotationType	type;
    char*		header;
} MailAnnotation;

static MailAnnotation*	mailAnnotations;
static int		annotationCount;
static MailAnnotation*	unknownMailAnnotations;
static int		unknownAnnotationCount;

static void
MailItemAnnotate(item, annotations)
MailItem*	item;
char**		annotations;
{
    int 		i;
    char		buffer[30];
    MailHeader*		header;
    int			count;
    MailAnnotation*	annos;
    char*		p;
    
    if (item->unknown)
    {
	count = unknownAnnotationCount;
	annos = unknownMailAnnotations;
#ifdef ITEM_DEBUG
	fprintf(stderr, "MailItemAnnotate(): using unknown annotations.\n");
#endif
    }
    else
    {
	count = annotationCount;
	annos = mailAnnotations;
#ifdef ITEM_DEBUG
	fprintf(stderr, "MailItemAnnotate(): using known annotations.\n");
#endif
    }
    
    if (item->annotations != NULL)
    {
	for (i = 0; item->annotations[i] != NULL; ++i)
	{
	    XtFree(item->annotations[i]);
	}
	
	XtFree((char*)item->annotations);
	item->annotations = NULL;
    }
    
    if (item->face == NULL)
    {
	return;
    }
    
    if (annotations == NULL)
    {
	item->annotations = (char**)XtCalloc(count + 1, sizeof(char*));
	
	for(i = 0; i < count; ++i)
	{
	    if (item->use_label == (i+1))
	    {
		item->annotations[i] = XtNewString(item->label);
	    }
	    else
	    {
		switch(annos[i].type)
		{
		  case MailAnnotateNone:
		    break;
		    
		  case MailAnnotateCount:
		    sprintf(buffer, "%d", item->face->count);
		    item->annotations[i] = XtNewString(buffer);
		    break;
		    
		  case MailAnnotateUser:
		    item->annotations[i] = XtNewString(item->user);
		    break;
		    
		  case MailAnnotateHost:
		    item->annotations[i] = XtNewString(item->host);
		    break;
		    
		  case MailAnnotateUserHost:
		    item->annotations[i] = XtMalloc(strlen(item->user) +
						    strlen(item->host) + 2);
		    sprintf(item->annotations[i], "%s@%s", item->user, item->host);
		    break;
		    
		  case MailAnnotateHeader:
		    header = MailHeaderFind(annos[i].header, item->headers);
		    if ((header != NULL) && (header->value != NULL))
		    {
			p = SkipChars(header->value, " \t");
			item->annotations[i] = XtNewString(p);
		    }
		    break;
		    
		  default:
		    fprintf(stderr, "Bad MailAnnotationType: %d\n",
			    annos[i].type);
		    break;
		}
	    }
	    
	    if (item->annotations[i] == NULL)
	    {
		item->annotations[i] = XtMalloc(1);
		*(item->annotations[i]) = '\0';
	    }
	    
#ifdef ITEM_DEBUG
	    fprintf(stderr, "item->annotation[%d]: <%s>\n",
		    i, item->annotations[i]);
#endif
	}
    }
    else
    {
	/*
	 * Count annotations.
	*/
	
	for(i = 0; annotations[i] != NULL; ++i);
	
	/*
	 * Copy annotations.
	*/
	
	item->annotations = (char**)XtCalloc(i+1, sizeof(char*));
	
	for(i = 0; annotations[i] != NULL; ++i)
	{
	    item->annotations[i] = XtNewString(annotations[i]);
	}
	
	item->annotations[i] = NULL;
    }
}

static void
MailItemFree(item)
MailItem	*item;
{
    if (item->next != NULL)
    {
	item->next->prev = item->prev;
    }
    
    if (item->prev != NULL)
    {
	item->prev->next = item->next;
    }
    
    if (TheMailItems == item)
    {
	TheMailItems = item->next;
    }
    
    if (TheMailItemsTail == item)
    {
	TheMailItemsTail = item->prev;
    }
    
    XtFree(item->user);
    XtFree(item->host);
    XtFree(item->realhost);
    XtFree(item->label);
    item->user  = NULL;
    item->host  = NULL;
    item->realhost  = NULL;
    item->label = NULL;
    
    /*
     * Free mail headers.
    */
    
    MailHeaderListFree(item->headers);
    item->headers = NULL;
    
    /*
     * Free annotations.
    */
    
    item->face = NULL;
    MailItemAnnotate(item, NULL);
    
    FaceImageFree(item->image);
    item->image = NULL;
    
#ifdef SOUND
    FaceSoundFree(item->sound);
#endif /* SOUND */
    item->sound = NULL;
    
    FaceCommandFree(item->command);
    item->command = NULL;
    
    XtFree((void *)item);
}

/*
 *    Use the ignoreMessage expression to see if this message should be
 * ignored.
*/

static int
MailItemIgnore(item)
MailItem	*item;
{
    FaceBinding	*binding;
    
    binding = FaceBindingCheck(item->headers,
			       TheFacesResources.ignore_message_bindings);
    
    if (binding != NULL)
    {
	return 1;
    }
    
    return(0);
}


void
MailBoxClear()
{
    MailItem	*item;
    
    for (item = TheMailItems; item != NULL; item = item->next)
    {
	item->in_use = 0;
    }
    
    FaceClear();
}

void
MailBoxUnClear()
{
    MailItem	*item;
    
    for (item = TheMailItems; item != NULL; item = item->next)
    {
	item->in_use = 1;
	item->face = FaceDisplay(item);
    }
}

void
MailBoxClean()
{
    MailItem *item;
    
    for (item = TheMailItems; item != NULL; item = item->next)
    {
	if (item->in_use == 0)
	{
	    MailItemFree(item);
	}
    }
    
    FaceClean();
}

static void
MailItemLabel(item)
MailItem*	item;
{
    if (item->label == NULL)
    {
	item->label = XtMalloc(strlen(item->user) + strlen(item->host) + 2);
	sprintf(item->label, "%s@%s", item->user, item->host);
	item->use_label = 0;
    }
}


/*
 *    Create a new mail item
*/

void
MailItemCreate(headers)
MailHeader*	headers;
{
    MailItem	*item;
    MailHeader	*from;
#ifdef LOOKUP_HOSTNAME
    struct hostent *host;
#endif
    
    /*
     *    Have we already seen this item?
    */
    
    for (item = TheMailItems; item != NULL; item = item->next)
    {
	if (item->in_use)
	{
	    continue;
	}
	
	if (MailHeaderListCompare(headers, item->headers))
	{
	    /*
	     * Yep!
	    */
	    
	    item->face = FaceDisplay(item);
	    item->in_use = 1;
	    
	    /*
	     * Setup annotations.
	    */
	    
	    MailItemAnnotate(item, NULL);

	    /*
	     * Don't need these headers!
	    */
	    
	    MailHeaderListFree(headers);
	    
	    return;
	}
    }
    
    item 	  = (MailItem *)XtCalloc(1, sizeof(MailItem));
    item->headers = headers;
    item->in_use  = 1;
    
    /*
     *    Now parse the from line into the user and host parts.
    */
    
    from = MailHeaderFind(TheFacesResources.from_field, headers);
    
    if (from == NULL)
    {
	from = MailHeaderFind("From:", headers);
    }
    
    if (from == NULL)
    {
	from = MailHeaderFind("From ", headers);
    }
    
    /*
     *   If we found no from line then we ignore this message.
    */
    
    if (from == NULL)
    {
	MailItemFree(item);
	return;
    }
    
    /*
     *   Parse the from address.
    */
    
    MailParseAddress(from->value, &(item->user), &(item->host));
    
#ifdef LOOKUP_HOSTNAME
    /*
     *  Lookup the host via gethostbyname if asked.
    */
    
    if (TheFacesResources.lookup_hostname)
    {
	host = gethostbyname(item->host);
	
	if (host != NULL)
	{
	    if (strcmp(item->host, host->h_name) != 0)
	    {
		item->realhost = XtNewString(host->h_name);
	    }
#ifdef LOOKUP_DEBUG
	    fprintf(stderr, "lookup: <%s> -> <%s>\n",
		    item->host, host->h_name);
#endif
	}
#ifdef LOOKUP_DEBUG
	else
	{
	    fprintf(stderr, "lookup: <%s> -> <>\n", item->host);
	}
#endif

    }
#endif /* LOOKUP_HOSTNAME */
    
    /*
     *   Now see if we should ignore this message.
    */
    
    if (MailItemIgnore(item))
    {
	/*
	 *   Yup!  Do not need this item.
	*/
	
	MailItemFree(item);
	return;
    }
    
    /*
     *  Locate any image and sound.
    */
    
    FaceImageFind(item);
#ifdef SOUND
    FaceSoundFind(item);
#endif
    FaceCommandFind(item);
    
    /*
     *  Compute the label (this is what is used to compress images.)
    */
    
    MailItemLabel(item);
    
    /*
     *    Add this new item to the tail of the list.
    */
    
    if (TheMailItems == NULL)
    {
	TheMailItems = TheMailItemsTail = item;
    }
    else
    {
	item->prev = TheMailItemsTail;
	item->prev->next = item;
	TheMailItemsTail = item;
    }
    
    item->face = FaceDisplay(item);
    
    /*
     * Setup annotations.
    */
    
    MailItemAnnotate(item, NULL);
    
#ifdef SOUND
    FaceSoundPlay(item->sound);
#endif /* SOUND */

    FaceCommandRun(item->command);
}

void
MailItemCreateNoHeaders(user, host, annotations)
char* user;
char* host;
char** annotations;
{
    MailItem	*item;

    if (annotations != NULL)
    {
	if (*annotations == NULL || **annotations == '\0')
	{
	    annotations = NULL;
	}
    }
    
    if (user == NULL)
    {
	user = "unknown";
    }
    
    if (host == NULL)
    {
	host = "LOCAL";
    }
    
    /*
     *    Have we already seen this item?
    */
    
    for (item = TheMailItems; item != NULL; item = item->next)
    {
	if (item->in_use)
	{
	    continue;
	}
	
	if (strcmp(user, item->user) == 0 &&
	    strcmp(host, item->host) == 0)
	{
#ifdef ITEM_DEBUG
	    fprintf(stderr,"MailItemCreateNoHeaders: reusing: <%s>:<%s>\n",
		    user, host);
#endif
	    /*
	     * Yep!
	    */
	    
	    item->face = FaceDisplay(item);

	    /*
	     * Annotations may have changed.
	    */
	    
	    MailItemAnnotate(item, annotations);
	    item->in_use = 1;
	    return;
	}
    }
    
#ifdef ITEM_DEBUG
	    fprintf(stderr,"MailItemCreateNoHeaders: creating: <%s>:<%s>\n",
		    user, host);
#endif
    
    item 	  = (MailItem *)XtCalloc(1, sizeof(MailItem));
    item->headers = NULL;
    item->user = XtNewString(user);
    item->host = XtNewString(host);
    item->in_use  = 1;
    
    /*
     *  Compute the label (this is what is used to compress images.)
    */
    
    MailItemLabel(item);
    
    /*
     *  Locate any image and sound.
    */
    
    FaceImageFind(item);
#ifdef SOUND
    FaceSoundFind(item);
#endif
    FaceCommandFind(item);
    
    /*
     *    Add this new item to the tail of the list.
    */
    
    if (TheMailItems == NULL)
    {
	TheMailItems = TheMailItemsTail = item;
    }
    else
    {
	item->prev = TheMailItemsTail;
	item->prev->next = item;
    }
    
    item->face = FaceDisplay(item);
    
    MailItemAnnotate(item, annotations);
    
#ifdef SOUND
    FaceSoundPlay(item->sound);
#endif /* SOUND */
    
    FaceCommandRun(item->command);
}

void
MailBoxEmpty()
{
    /*
     * There is no file, clear all counts and clean.  This
     * will cause all faces to be removed.
    */
    
    MailBoxClear();
    MailBoxClean();
}

void
MailItemInit()
{
    int		i;
    XrmDatabase	db;
    char*	appname = XtName(TheTopLevel);
    char*	fullname;
    char*	fullclass;
    String	type;
    XrmValue	value;
    
#if (XtSpecificationRelease > 4)
    db = XtScreenDatabase(XtScreen(TheTopLevel));
#else
    db = XtDatabase(XtDisplay(TheTopLevel));
#endif
    
    fullname = XtMalloc(strlen(appname) + 50);
    fullclass = XtMalloc(strlen(XFACES_CLASS) + 50);
    
    annotationCount = TheFacesResources.annotation_count;
    mailAnnotations = (MailAnnotation*) XtCalloc(annotationCount,
						 sizeof(MailAnnotation));
    
    for(i = 0; i < annotationCount; ++i)
    {
	type = NULL;
	mailAnnotations[i].type   = MailAnnotateNone;
	mailAnnotations[i].header = NULL;
	
	sprintf(fullname,  "%s.mail.annotation%d", appname, i+1);
	sprintf(fullclass, "%s.Mail.Annotation", XFACES_CLASS);

#ifdef ITEM_DEBUG
	fprintf(stderr, "looking for: %s/%s\n", fullname, fullclass);
#endif
	
	if (XrmGetResource(db, fullname, fullclass, &type, &value) &&
	    (strcmp(type, XtRString) == 0))
	{
#ifdef ITEM_DEBUG
	    fprintf(stderr, "found: <%s>\n", (char*)(value.addr));
#endif
	    if (strcmp(value.addr, "none") == 0)
	    {
		mailAnnotations[i].type = MailAnnotateNone;
	    }
	    else if (strcmp(value.addr, "count") == 0)
	    {
		mailAnnotations[i].type = MailAnnotateCount;
	    }
	    else if (strcmp(value.addr, "user") == 0)
	    {
		mailAnnotations[i].type = MailAnnotateUser;
	    }
	    else if (strcmp(value.addr, "host") == 0)
	    {
		mailAnnotations[i].type = MailAnnotateHost;
	    }
	    else if (strcmp(value.addr, "user@host") == 0)
	    {
		mailAnnotations[i].type = MailAnnotateUserHost;
	    }
	    else if (strncmp(value.addr, "*", 1) == 0)
	    {
		mailAnnotations[i].type   = MailAnnotateHeader;
		mailAnnotations[i].header = (char*)(value.addr) + 1;
	    }
	    
#ifdef ITEM_DEBUG
	    switch (mailAnnotations[i].type)
	    {
	      case MailAnnotateNone:
		fprintf(stderr,
			"mailAnnotations[%d].type: MailAnnotateNone\n", i);
		break;
		
	      case MailAnnotateCount:
		fprintf(stderr,
			"mailAnnotations[%d].type: MailAnnotateCount\n", i);
		break;
		
	      case MailAnnotateUser:
		fprintf(stderr,
			"mailAnnotations[%d].type: MailAnnotateUser\n", i);
		break;
		
	      case MailAnnotateHost:
		fprintf(stderr,
			"mailAnnotations[%d].type: MailAnnotateHost\n", i);
		break;
		
	      case MailAnnotateUserHost:
		fprintf(stderr,
			"mailAnnotations[%d].type: MailAnnotateUserHost\n", i);
		break;
		
	      case MailAnnotateHeader:
		fprintf(stderr,
			"mailAnnotations[%d].type: MailAnnotateHeader\n", i);
		fprintf(stderr,
			"mailAnnotations[%d].header: <%s>\n",
			i, mailAnnotations[i].header);
		break;
	    }
#endif
	}
    };
    
    unknownAnnotationCount = TheFacesResources.unknown_annotation_count;
    unknownMailAnnotations = (MailAnnotation*)XtCalloc(unknownAnnotationCount,
						       sizeof(MailAnnotation));
    
    for(i = 0; i < unknownAnnotationCount; ++i)
    {
	type = NULL;
	unknownMailAnnotations[i].type   = MailAnnotateNone;
	unknownMailAnnotations[i].header = NULL;
	
	sprintf(fullname,  "%s.mail.unknownAnnotation%d", appname, i+1);
	sprintf(fullclass, "%s.Mail.Annotation", XFACES_CLASS);
	
#ifdef ITEM_DEBUG
	fprintf(stderr, "looking for: %s/%s\n", fullname, fullclass);
#endif
	
	if (XrmGetResource(db, fullname, fullclass, &type, &value) &&
	    (strcmp(type, XtRString) == 0))
	{
#ifdef ITEM_DEBUG
	    fprintf(stderr, "found: <%s>\n", (char*)(value.addr));
#endif
	    if (strcmp(value.addr, "none") == 0)
	    {
		unknownMailAnnotations[i].type = MailAnnotateNone;
	    }
	    else if (strcmp(value.addr, "count") == 0)
	    {
		unknownMailAnnotations[i].type = MailAnnotateCount;
	    }
	    else if (strcmp(value.addr, "user") == 0)
	    {
		unknownMailAnnotations[i].type = MailAnnotateUser;
	    }
	    else if (strcmp(value.addr, "host") == 0)
	    {
		unknownMailAnnotations[i].type = MailAnnotateHost;
	    }
	    else if (strcmp(value.addr, "user@host") == 0)
	    {
		unknownMailAnnotations[i].type = MailAnnotateUserHost;
	    }
	    else if (strncmp(value.addr, "*", 1) == 0)
	    {
		unknownMailAnnotations[i].type   = MailAnnotateHeader;
		unknownMailAnnotations[i].header = (char*)(value.addr) + 1;
	    }
	    
#ifdef ITEM_DEBUG
	    switch (unknownMailAnnotations[i].type)
	    {
	      case MailAnnotateNone:
		fprintf(stderr,
			"unknownMailAnnotations[%d].type: MailAnnotateNone\n", i);
		break;
		
	      case MailAnnotateCount:
		fprintf(stderr,
			"unknownMailAnnotations[%d].type: MailAnnotateCount\n", i);
		break;
		
	      case MailAnnotateUser:
		fprintf(stderr,
			"unknownMailAnnotations[%d].type: MailAnnotateUser\n", i);
		break;
		
	      case MailAnnotateHost:
		fprintf(stderr,
			"unknownMailAnnotations[%d].type: MailAnnotateHost\n", i);
		break;
		
	      case MailAnnotateUserHost:
		fprintf(stderr,
			"unknownMailAnnotations[%d].type: MailAnnotateUserHost\n", i);
		break;
		
	      case MailAnnotateHeader:
		fprintf(stderr,
			"unknownMailAnnotations[%d].type: MailAnnotateHeader\n", i);
		fprintf(stderr,
			"unknownMailAnnotations[%d].header: <%s>\n",
			i, unknownMailAnnotations[i].header);
		break;
	    }
#endif
	}
    };
    
    XtFree(fullname);
    XtFree(fullclass);
}
