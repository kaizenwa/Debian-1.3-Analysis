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
 * Created On      : Mon Jan 31 22:31:39 1994
 * Last Modified By: Chris Liebman
 * Last Modified On: Mon Jan 31 23:22:48 1994
 * Update Count    : 5
 * Status          : Released
 * 
 * HISTORY
 * 
 * PURPOSE
 * 	Misc string functions.
*/

#ifndef lint
static char *RCSid = "$Id: string.c,v 1.2 1994/02/01 04:25:32 liebman Exp $";
#endif

#include "faces.h"

char**
StringParse(str, delims)
char* str;
char* delims;
{
    char*		name;
    int			count = 0;
    int			array_size = 0;
    char**	array = NULL;
    
    if (str == NULL)
    {
	return NULL;
    }
    
    array_size = 10;
    array = (char**)XtMalloc(array_size * sizeof(char*));
    
    while((name = ParseToken(&str, delims)) != NULL)
    {
	if (count >= array_size)
	{
	    array_size += 10;
	    array = (char**)XtRealloc((char*)array,
				       array_size * sizeof(char*));
	}
	
	array[count++] = name;
    }
    
    array[count] = NULL;
    
    return array;
}

String
SkipChars(str, delim)
String	str;
String	delim;
{
    while((*str != '\0') && (index(delim, *str) != NULL))
    {
	++str;
    }
    
    return(str);
}

String
ParseToken(str, delim)
String	*str;
String	delim;
{
    String	token;
    
    *str = SkipChars(*str, delim);
    
    if (**str == '\0')
    {
	return(NULL);
    }
    
    token = *str;
    
    while(index(delim, **str) == NULL)
    {
	++(*str);
    }
    
    /*
     *   We are now pointing at the first delim, change it to a NULL.
    */
    
    if (**str != '\0')
    {
	**str = '\0';
	++(*str);
    }
    
    return(token);
}
