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
 * Created On      : Sat Jan 29 16:45:48 1994
 * Last Modified By: Chris Liebman
 * Last Modified On: Mon Jan 31 22:32:22 1994
 * Update Count    : 16
 * Status          : Unknown, Use with caution!
 * 
 * HISTORY
 * 
 * PURPOSE
 * 	Path functions.
*/

#ifndef lint
static char *RCSid = "$Id: path.c,v 1.2 1994/02/23 13:17:02 liebman Exp $";
#endif

#include "faces.h"
#include <sys/stat.h>

char**
PathParse(str)
char* str;
{
    char** path;
    
    path = StringParse(str, ":");
    
    return path;
}

int
PathEnumerate(file, paths, func, data)
char*	file;
char**	paths;
int	(*func) P_((char* file, char* path, void* data));
void*	data;
{
    static char*	buffer = NULL;
    static int		buffer_size = 0;
    struct stat		buf;
    int ret = 0;
    int path_len;
    int file_len;
    int len;
    
    if (buffer == NULL)
    {
	buffer_size = 1024;
	buffer = XtMalloc(buffer_size);
    }
    
    if (paths == NULL)
    {
	fprintf(stderr, "PathEnumerate: NULL paths!\n");
	return 0;
    }
    
    file_len = strlen(file);
    
    while(!ret && *paths != NULL)
    {
	if (TheFacesResources.path_by_chdir)
	{
	    if (chdir(*paths) != -1)
	    {
		ret = func(file, *paths, data);
	    }
	}
	else
	{
	    if (stat(*paths, &buf) != -1 && (buf.st_mode & S_IFDIR))
	    {
		path_len = strlen(*paths);
		
		/*
		 *  We need path+/+file+NUL
		 */
		
		len = path_len + file_len + 2;
		
		if (len > buffer_size)
		{
		    buffer_size = len;
		    buffer = XtRealloc(buffer, buffer_size);
		}
		
		sprintf(buffer, "%s/%s", *paths, file);
		
		ret = func(buffer, *paths, data);
	    }
	}
	
	/*
	 * Go to the next path.
	*/
	
	++paths;
    }
    
    return ret;
}

