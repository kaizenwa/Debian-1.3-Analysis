/* ########################################################################

				macros.c

   File: macros.c
   Path: /home/fournigault/c/X11/xcoral-2.31/macros.c
   Description: 
   Created: Fri Jan 27 11:15:42 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:15:44 MET 1995
   Last maintained by: Lionel Fournigault

   RCS $Revision$ $State$
   

   ########################################################################

   Note: 

   ########################################################################

   Copyright (c) : Lionel Fournigault

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   ######################################################################## */


#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#ifdef __FreeBSD__
#include <stdlib.h>
#else /* !__FreeBSD__ */
#include <malloc.h>
#endif

#include "main_text.h"
#include "macros.h"
#include "warn_box.h"
#include "main_events.h"
#include "get_file.h"
#include "page.h"
#include "bm_search.h"
#include "chars_cmds.h"
#include "ie_func.h"

extern int last_key;

FCT (static Macro *, AllocMacro, () );
extern FCT(     char *, ie_call_function,(Text *, char *, int, int *)   );

Macro *macro = 0;
int macro_learning = False;
Text *macro_text = 0;

/*
**	Function name : StartMacro
**
**	Description : 
**	Input : 
**	Ouput :
*/
void StartMacro ( text ) 
    Text *text;
{
    if ( macro_learning ) {
	DisplayWMessage ( "Learn macro already run in other window", "Macro :", True );
	return;
    }
    if ( macro )
      ResetMacro ();
    ResetSearchString ();
    macro_learning = True;
    DisplayMessage ( text -> mwin, "Learn macro..." );
    macro_text = text;
    FreezeButtons ( text );
}

/*
**	Function name : EndMacro
**
**	Description : 
**	Input : 
**	Ouput :
*/
void EndMacro ( text ) 
    Text *text;
{
    if ( ! macro_learning )
      return;

    macro_learning = False;
    DisplayMessage ( text -> mwin, "End macro" );
    macro_text = 0;
    UnFreezeButtons ();
}

/*
**	Function name : InsideMacro
**
**	Description : 
**	Input : 
**	Ouput :
*/
int InsideMacro ( text )
    Text *text;
{
  if ( ! macro_learning )
      return False;
						 
  if ( text != macro_text ) {
    DisplayMessage ( text -> mwin, "Warning, macro definition is freezed" );
    return False;
  }
  return True;
}

/*
**	Function name : AllocMacro
**
**	Description : 
**	Input : 
**	Ouput :
*/
static Macro *AllocMacro ()
{
    Macro *tmp;
    
    if (( tmp = (Macro *) malloc ((unsigned) sizeof (Macro) + 2 )) == 0 )
      return 0;
    bzero ( (char *) tmp, sizeof (Macro) + 2);
    
    return (tmp);
}

/*
**	Function name : LoadMacro
**
**	Description : 
**	Input : 
**	Ouput :
*/
void LoadMacro ( text, func, arg, repeat )
    Text *text;
    FCT(void, (* func), ());
    void * arg;
    int repeat;
{
    Macro *tmp, *current = macro;

    if ( text != macro_text ) {
      DisplayWMessage ( "Learn macro already run in other window", "Macro :", True );
      return;
    }
    tmp = AllocMacro ();
    
#ifdef DEBUG
    (void) fprintf ( stderr, "LoadMacro\n" );
#endif /* DEBUG */  
    
    if (tmp) {
	DisplayMessage ( text -> mwin, "Learn macro..." );
	if (func == QuotedChar) {
	  tmp -> f = (void(*)()) f_impc;
	  /* previous char */
	  arg = (void *) ((int) *(text -> buf -> l_cur -1));
	}
	else
	  tmp -> f = func;
	if ( arg ) {
#ifdef DEBUG
	  (void) fprintf ( stderr, "LoadMacro c = %c %d\n",
			  (char) arg, (char) arg );
#endif /* DEBUG */  
	  tmp -> arg = arg;
	  tmp -> last_key = last_key;
	}
	else 
	  tmp -> arg = 0;
	tmp -> repeat = repeat;
	if ( current == 0 )
	  macro = tmp;
	else {
#ifdef DEBUG
	    (void) fprintf ( stderr, "LoadMacro link\n" );
#endif /* DEBUG */  
	    while ( current -> next )
	      current = current -> next;
	    current -> next = tmp;
	}
    }
    else
      DisplayMessage ( text -> mwin, "Alloc memory error" );
}

/*
**	Function name : ResetMacros
**
**	Description : 
**	Input : 
**	Ouput :
*/
void ResetMacro ()
{
    Macro *tmp, *current = macro;

#ifdef DEBUG
    (void) fprintf ( stderr, "ResetMacro\n" );
#endif /* DEBUG */  
    
    if ( current != 0 ) {
	do {
	    tmp = current -> next;
	    (void) free ( (char *) current );
	    current = tmp;
	} while ( current );
    }
    
    macro = 0;
    macro_learning = False;  
    UnFreezeButtons ();
}

/*
**	Function name : DeleteMacro
**
**	Description :
**	Input :
**	Output :
*/
void DeleteMacro ( text )
    Text *text;
{
  if ( text == macro_text )
    ResetMacro ();
}


/*
**	Function name : PlayMacros
**
**	Description : 
**	Input : 
**	Ouput :
*/
void RunMacro ( text, n )
    Text *text;
    int n;
{
    Macro *current = macro;

#ifdef DEBUG
    (void) fprintf ( stderr, "PlayMacro\n" );
#endif /* DEBUG */  
    
    if ( current == 0 )
      return;
    
    DisplayMessage ( text -> mwin, "Play macro..." );

    do {
	n--;
	current = macro;
	do {
	    repeat_num = current -> repeat;
	    if ( current -> arg == 0 )
	      (((FCT(void, (*),(Text *))) (current -> f))) ( text );
	    else if (current -> f == ((FCT(void, (*),())) ie_call_function)) {
	      int nw = text->win_id;
	      char * msg;

	      StorePosition ( text );
/*	      CheckModifAndFilename ( text ); */
	      last_key = current->last_key;
	      msg = (char*) ie_call_function(text, (char*) current->arg, 0, 0);
	      text = update_cwd(nw);
	      ie_redisplay( text );
	      if (msg != 0) {
		DisplayMessage ( text -> mwin, msg );
		repeat_num = 0;
		return;
	      }
	    }
	    else
	      (((FCT(void, (*),(Text *, int))) (current -> f))) ( text, (int) current -> arg );
	    current = current -> next;
	} while ( current );
    } while ( n );
    
    repeat_num = 0;
    DisplayMessage ( text -> mwin, " " );
}

