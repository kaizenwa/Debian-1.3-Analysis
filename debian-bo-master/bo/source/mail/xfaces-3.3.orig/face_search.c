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
 * Created On      : Thu Jan 27 09:53:07 1994
 * Last Modified By: Chris Liebman
 * Last Modified On: Sun Feb 13 17:07:36 1994
 * Update Count    : 65
 * Status          : Released
 * 
 * HISTORY
 * 13-Feb-1994		Chris Liebman	
 *    Last Modified: Sat Feb 12 22:04:13 1994 #64 (Chris Liebman)
 *    Added FormatCommand support.
 * 
 * PURPOSE
 * 	Handle searching for images/sounds.
*/

#ifndef lint
static char *RCSid = "$Id: face_search.c,v 1.5 1994/02/13 22:33:30 liebman Exp $";
#endif

#include "faces.h"
#include "face_search.h"

/*
 *  This is the master list of supported search types.
*/

static FaceSearchType*	AllSearchTypes = NULL;

/*
 *   Look up an search type by name.
*/

FaceSearchType*
FaceSearchTypeByName(name)
char* name;
{
    FaceSearchType* type;
    
    for (type = AllSearchTypes; type != NULL; type = type->next)
    {
	if (strcmp(name, type->name) == 0)
	{
	    return type;
	}
    }
    
    return NULL;
}

/*
 * Register a new search type.
*/

void
FaceSearchTypeRegister(type)
FaceSearchType* type;
{
    type->next = AllSearchTypes;
    AllSearchTypes = type;
}

/*
 *  Use the search data and search for something.
*/

int
FaceSearch(item, search_list)
MailItem*	item;
FaceSearchData*	search_list;
{
    FaceSearchData*	data;
    
#ifdef SEARCH_DEBUG
    fprintf(stderr, "FaceSearch failed.\n");
#endif
    
    /*
     *  Foreach search specifier we look for each type with each path.
    */
    
    for(data = search_list; data != NULL; data = data->next)
    {
#ifdef SEARCH_DEBUG
	fprintf(stderr, "Attempting search type: <%s>\n", data->search->name);
#endif
	if (data->search->search(item, data))
	{
#ifdef SEARCH_DEBUG
	    fprintf(stderr, "Succeded with search type: <%s>\n",
		    data->search->name);
#endif
	    return 1;
	}
    }
    
#ifdef SEARCH_DEBUG
    fprintf(stderr, "FaceSearch failed.\n");
#endif
    return 0;
}

/*
 * Load a sound/image
*/

int
FaceSearchLoad(name, item, data)
char*		name;
MailItem*	item;
FaceSearchData*	data;
{
    int ret = 0;
    
    switch(data->format)
    {
      case FormatImage:
	ret = FaceImageLoad(name, item, data);
	break;
	
#ifdef SOUND
      case FormatAudio:
	ret = FaceSoundLoad(name, item, data);
	break;
#endif
	
      case FormatCommand:
	ret = FaceCommandLoad(name, item, data);
	break;
	
      default:
	fprintf(stderr, "FaceSearchLoad: unknown format flag!\n");
	ret = 0;
	break;
    }
    
    return ret;
}

/*
 * Parse the search bindings.
*/

FaceSearchData*
FaceSearchParse(str, format)
char*		str;
FaceFormat	format;
{
    FaceSearchData*	head = NULL;
    FaceSearchData*	tail = NULL;
    FaceSearchData*	data;
    FaceSearchType*	search = NULL;
    FaceImageType**	itypes = NULL;
    char**		paths = NULL;
    char*		line;
    char*		value;
    
    if (str == NULL)
    {
	return(NULL);
    }
    
    while((line = ParseToken(&str, "\n")) != NULL)
    {
	/*
	 * Skip past any leading whitespace.
	*/
	
	line = SkipChars(line, "\t \n");
	
	/*
	 *  Get name of search type.
	*/
	
	value = ParseToken(&line, " \t\n");
	
	/*
	 * if there was no search name then skip this line.
	*/
	
	if ((value  == NULL) || ((search = FaceSearchTypeByName(value)) == NULL))
	{
	    if (value != NULL)
	    {
		fprintf(stderr, "FaceSearchParse: bad search type: <%s>\n", value);
	    }
	    
	    continue;
	}
	
	/*
	 *  Get the list of types.
	*/
	
	value = ParseToken(&line, " \t\n");
	
	if (value != NULL)
	{
	    if (format == FormatImage)
	    {
		itypes = FaceImageTypeListParse(value);
	    }
	    else
	    {
		/*
		 * We ignore the type list for sounds.
		*/
	    }
	}
	
	/*
	 *  Get the path list.
	*/
	
	value = ParseToken(&line, " \t\n");
	
	if (value != NULL)
	{
	    paths = PathParse(value);
	}
	
	/*
	 * Construct a new search data.
	*/
	
	data = XtNew(FaceSearchData);
	data->format = format;
	data->search = search;
	
	if (format == FormatImage)
	{
	    data->stypes = NULL;
	    data->itypes = itypes;
	}
	else
	{
	    data->itypes = NULL;
	    data->stypes = NULL;
	}
	
	data->paths = paths;
	data->next = NULL;
	
	/*
	 *   Construct list.
	*/
	
	if (!head)
	{
	    head = data;
	    tail = data;
	}
	else
	{
	    tail->next = data;
	    tail = data;
	}
    }
    
    return(head);
}
