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
 * Created On      : Sat Feb 12 21:40:50 1994
 * Last Modified By: Chris Liebman
 * Last Modified On: Sun Feb 13 17:00:44 1994
 * Update Count    : 24
 * Status          : Released
 * 
 * HISTORY
 * 
 * PURPOSE
 * 	Handle commands.
*/

#ifndef lint
static char *RCSid = "$Id: face_command.c,v 1.1 1994/02/13 22:00:52 liebman Exp $";
#endif

#include "faces.h"
#include "face_command.h"
#include "face_search.h"

static FaceCommand* TheCommands;

/*
 *    Create face command data for face.
*/

FaceCommand *
FaceCommandCreate(file)
char	*file;
{
    FaceCommand	*fc;
    
    /*
     *    First see if we already have this command.
    */
    
    for (fc = TheCommands; fc != NULL; fc = fc->next)
    {
	if (strcmp(fc->command, file) == 0)
	{
	    /*
	     * Yep!
	    */
	    
	    fc->refs += 1;
	    
	    return(fc);
	}
    }
    
    /*
     *   Ok, create a face command struct.
    */
    
    fc = (FaceCommand *)XtCalloc(1, sizeof(FaceCommand));
    fc->command  = XtNewString(file);
    
    fc->refs  = 1;
    
    /*
     *  Put the new command on the list.
    */
    
    fc->next = TheCommands;
    fc->prev = NULL;
    
    if (fc->next != NULL)
    {
	fc->next->prev = fc;
    }
    
    TheCommands = fc;
    
    /*
     * and return it.
    */
    
    return(fc);
}

/*
 *    Free a command.
*/

void
FaceCommandFree(fc)
FaceCommand	*fc;
{
    if (!fc)
    {
	return;
    }
    
    /*
     *   First remove one reference.  If there are still more refs just
     * return.
    */
    
    fc->refs -= 1;
    if (fc->refs != 0)
    {
	return;
    }
    
    /*
     * The previous command is now previous to the next command.
    */
    
    if (fc->next != NULL)
    {
	fc->next->prev = fc->prev;
    }
    
    /*
     * The next face is now next from the previous face.
    */
    
    if (fc->prev != NULL)
    {
	fc->prev->next = fc->next;
    }
    
    /*
     * If this was the first command then the next command is
     * first.
    */
    
    if (fc == TheCommands)
    {
	TheCommands = fc->next;
    }
    
    /*
     *    Ok, free the name.
    */
    
    XtFree(fc->command);
    
    /*
     *    Free the struct.
    */
    
    XtFree((void *)fc);
}


int
FaceCommandLoad(file, item, data)
char*		file;
MailItem*	item;
FaceSearchData*	data;
{
    /*
     *   We always succeed!
    */
    
    item->command = FaceCommandCreate(file);
    
    return 1;
}

/*
 *  Execute a command.
*/

void
FaceCommandRun(fc)
FaceCommand	*fc;
{
    /*
     *  No command!
    */
    
    if (!fc)
    {
	return;
    }
    
    system(fc->command);
}

void
FaceCommandFind(item)
MailItem*	item;
{
    if (TheFacesResources.use_commands)
    {
	FaceSearch(item, TheFacesResources.command_search);
    }
}
