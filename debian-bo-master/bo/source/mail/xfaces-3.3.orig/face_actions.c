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
 * Last Modified On: Sun Feb 13 16:59:08 1994
 * Update Count    : 19
 * Status          : Released
 * 
 * HISTORY
 * 13-Feb-1994		Chris Liebman	
 *    Last Modified: Sat Feb 12 23:15:20 1994 #17 (Chris Liebman)
 *    Added RunCommands action.
 *
 * 31-Jan-1994		Chris Liebman	
 *    Last Modified: Tue Jan 11 14:16:36 1994 #1 (Chris Liebman)
 *    Now use CheckMailNow() to avoid duplicate code.
 * 
 * PURPOSE
 * 	Global Xt Actions for xfaces.
*/

#ifndef lint
static char *RCSid = "$Id: face_actions.c,v 1.8 1994/02/13 21:59:13 liebman Exp $";
#endif

#include "faces.h"

/*
 *    Replay sound action.
*/

static void
FacePlaySounds(w, event, params, num_params)
Widget	w;
XEvent	*event;
String	*params;
int	num_params;
{
#ifdef SOUND
    Face	*face;
    FaceItem	*item;
    
    /*
     *   If we do not want sounds then don't play any!
    */

    if (!TheFacesResources.use_sound)
    {
	return;
    }
    
    if (TheFaceList == NULL)
    {
	FaceSoundPlay(NoMailSound);
	return;
    }
    
    /*
     *    Find the face associated with widget.
    */
    
    for (face = TheFaceList; face != NULL; face = face->next)
    {
	if (face->widget == w)
	{
	    break;
	}
    }
    
    if (face == NULL)
    {
	return;
    }
    
    /*
     *  Play all attached sounds.
    */
    
    for(item = face->items; item != NULL; item = item->next)
    {
	if (item->item->sound != NULL)
	{
	    FaceSoundPlay(item->item->sound);
	}
    }
    
#endif /* SOUND */
}

/*
 *    Run commands action.
*/

static void
FaceRunCommands(w, event, params, num_params)
Widget	w;
XEvent	*event;
String	*params;
int	num_params;
{
    Face	*face;
    FaceItem	*item;
    
    /*
     *   If we do not want commands then don't run any.
    */
    
    if (!TheFacesResources.use_commands)
    {
	return;
    }
    
    /*
     *    Find the face associated with widget.
    */
    
    for (face = TheFaceList; face != NULL; face = face->next)
    {
	if (face->widget == w)
	{
	    break;
	}
    }
    
    if (face == NULL)
    {
	return;
    }
    
    /*
     *  Run all attached commands.
    */
    
    for(item = face->items; item != NULL; item = item->next)
    {
	if (item->item->command != NULL)
	{
	    FaceCommandRun(item->item->command);
	}
    }
}

static void
FaceCheckMail(w, event, params, num_params)
Widget	w;
XEvent	*event;
String	*params;
int	num_params;
{
    
    CheckMailNow();
    
#ifdef SOUND
    if (TheFaceList == NULL && TheFacesResources.use_sound)
    {
	FaceSoundPlay(NoMailSound);
	return;
    }
#endif /* SOUND */
}

static XtActionsRec	FaceActions[] =
{
    {"PlaySounds",	FacePlaySounds},
    {"RunCommands",	FaceRunCommands},
    {"CheckMail",	FaceCheckMail}
};

void
FaceActionsInit()
{
    XtAddActions(FaceActions, XtNumber(FaceActions));
}

