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
 * Last Modified On: Sun Mar  6 22:49:00 1994
 * Update Count    : 61
 * Status          : Released
 * 
 * HISTORY
 * 13-Feb-1994		Chris Liebman	
 *    Last Modified: Mon Jan 31 22:39:36 1994 #39 (Chris Liebman)
 *    Added a label field to the bindings.
 *
 * 31-Jan-1994		Chris Liebman	
 *    Last Modified: Mon Jan 31 22:34:33 1994 #38 (Chris Liebman)
 *    Moved SkipChars() and ParseToken() to string.c
 *
 * 18-Jan-1994		Chris Liebman	
 *    Last Modified: Tue Jan 18 15:18:09 1994 #9 (Chris Liebman)
 *    Changed the binding parser to expect two and three tokens instead
 *    of one and two.  Now expects <field name> <pattern> [<file>].
 *
 * 14-Jan-1994		Chris Liebman	
 *    Last Modified: Tue Jan 11 14:16:14 1994 #1 (Chris Liebman)
 *    added flag to FaceBindingParse to allow the ignoreMessages
 *    bindings to be parsed also.
 * 
 * PURPOSE
 * 	   Parse the binding lines from the resource file.  Bindings contain a
 *	list of patterns then a file name for an image or sound if the pattern
 *	is matched.
*/

#ifndef lint
static char *RCSid = "$Id: face_binding.c,v 1.11 1994/03/07 20:24:10 liebman Exp $";
#endif

#include "faces.h"


/*
 *   Parse the bindings.
*/

FaceBinding *
FaceBindingParse(str, has_value, has_label)
String	str;
int	has_value;
int	has_label;
{
    FaceBinding	*fb;
    FaceBinding	*list = NULL;
    String	pair = NULL;
    String	name = NULL;
    String	pattern = NULL;
    String	value = NULL;
    String	label = NULL;
    String	annop;
    int		anno;
    char*	p;
    
    if (str == NULL)
    {
	return(NULL);
    }
    
    while((pair = ParseToken(&str, "\n")) != NULL)
    {
	anno = 1;
	pair = SkipChars(pair, "\t \n");
	
	/*
	 *  Field name to check.
	*/
	
	name = ParseToken(&pair, "\t \n");
	
	/*
	 *  Parse out the expression.
	*/
	
	pair = SkipChars(pair, "\t \n");
	
	if (!has_value)
	{
	    if (strlen(pair) == 0)
	    {
		continue;
	    }
	    pattern = pair;
	    value = NULL;
	}
	else
	{
	    if ((pattern = ParseToken(&pair, ":")) == NULL)
	    {
		continue;
	    }

	    if (!has_label)
	    {
		value = SkipChars(pair, "\t ");
	    }
	    else
	    {
		value = ParseToken(&pair, "\t \n");
		
		label = SkipChars(pair, "\t ");
		
		/*
		 * If there is an annotation number then grab it, otherwise it
		 * defaults to 1.
		*/
		
		annop = rindex(label, ':');
		
		if (annop != NULL)
		{
		    *annop++ = '\0';
		    anno = atoi(annop);
		}
		
		if (anno < 1 || anno > TheFacesResources.annotation_count)
		{
		    anno = 1;
		}
	    }
	}
	
	fb = XtNew(FaceBinding);
	
	/*
	 * If the field name starts with a '|' then we should be case
	 * insensitive.
	*/

	if (*name == '|')
	{
	    ++name;
	    fb->casesensitive = 0;
	    for(p = pattern; *p != '\0'; ++p)
	    {
		if (isupper(*p))
		{
		    *p = tolower(*p);
		}
	    }
	}
	else
	{
	    fb->casesensitive = 1;
	}
	
	/*
	 *  If the field name is "*" then all fields are searched. If the
	 * field name has a leading
	*/
	
	if (strcmp(name, "*") == 0)
	{
	    fb->name = NULL;
	}
	else
	{
	    fb->name    = XtNewString(name);
	}
	
	/*
	 *  Compile the regular expression.
	*/
	
	fb->patsrc  = XtNewString(pattern);
	fb->pattern = regcomp(pattern);
	
	/*
	 *  If we don't need a value then net the file to NULL.
	*/
	
	if (!has_value)
	{
	    fb->file = NULL;
	}
	else
	{
	    fb->file    = XtNewString(value);
	    fb->label	= XtNewString(label);
	    fb->anno	= anno;
	}
	
	/*
	 * place binding on list.
	*/
	
	fb->next    = list;
	list        = fb;
    }
    
    return(list);
}

/*
 *   Check a binding list agains a header list for a match.
*/

FaceBinding*
FaceBindingCheck(headers, bindings)
MailHeader*	headers;
FaceBinding*	bindings;
{
    MailHeader*		header;
    FaceBinding*	binding;
    char*		value;
    char*		p;
    int			result;
    
    for (binding = bindings; binding != NULL; binding = binding->next)
    {
#ifdef BINDING_DEBUG
	fprintf(stderr, "FaceBindingCheck: pattern: <%s>\n", binding->patsrc);
#endif
	/*
	 *  See if we only need to check one header for this binding.
	*/
	
	if (binding->name != NULL)
	{
#ifdef BINDING_DEBUG
	    fprintf(stderr, "FaceBindingCheck: check <%s> header.\n",
		    binding->name);
#endif
	    if ((header = MailHeaderFind(binding->name, headers)) != NULL)
	    {
#ifdef BINDING_DEBUG
		fprintf(stderr, "FaceBindingCheck: <%s> value: <%s>.\n",
			binding->name, header->value);
#endif

		/*
		 * if we are not case sensitive then copy the header value
		 * and make it lower case.
		*/

		if (!binding->casesensitive)
		{
		    value = XtNewString(header->value);

		    for(p = value; *p != '\0'; ++p)
		    {
			if (isupper(*p))
			{
			    *p = tolower(*p);
			}
		    }
#ifdef BINDING_DEBUG
		    fprintf(stderr,
			    "FaceBindingCheck: lowered value: <%s>\n", value);
#endif
		}
		else
		{
		    value = header->value;
		}
		
		result = regexec(binding->pattern, value);
		
		if (!binding->casesensitive)
		{
		    XtFree(value);
		    value = NULL;
		}
		
		if (result)
		{
#ifdef BINDING_DEBUG
		    fprintf(stderr, "FaceBindingCheck: matched!\n");
#endif
		    return binding;
		}
#ifdef BINDING_DEBUG
		fprintf(stderr, "FaceBindingCheck: no match.\n");
#endif
	    }
	}
	else
	{
#ifdef BINDING_DEBUG
	fprintf(stderr, "FaceBindingCheck: check all headers.\n");
#endif
	    /*
	     * Search all headers.
	    */
	    
	    for (header = headers; header != NULL; header = header->next)
	    {
#ifdef BINDING_DEBUG
		fprintf(stderr,
			"FaceBindingCheck: checking: <%s> value: <%s>.\n",
			header->name, header->value);
#endif
		
		/*
		 * if we are not case sensitive then copy the header value
		 * and make it lower case.
		*/
		
		if (!binding->casesensitive)
		{
		    value = XtNewString(header->value);

		    for(p = value; *p != '\0'; ++p)
		    {
			if (isupper(*p))
			{
			    *p = tolower(*p);
			}
		    }
#ifdef BINDING_DEBUG
		    fprintf(stderr,
			    "FaceBindingCheck: lowered value: <%s>\n", value);
#endif
		}
		else
		{
		    value = header->value;
		}
		
		result = regexec(binding->pattern, value);
		
		if (!binding->casesensitive)
		{
		    XtFree(value);
		    value = NULL;
		}
		
		if (result)
		{
#ifdef BINDING_DEBUG
		    fprintf(stderr, "FaceBindingCheck: matched!\n");
#endif
		    return binding;
		}
#ifdef BINDING_DEBUG
		fprintf(stderr, "FaceBindingCheck: no match.\n");
#endif
	    }
	}
    }
    
    return NULL;
}

