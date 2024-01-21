/* ########################################################################

			      bm_search.c

   File: bm_search.c
   Path: /home/fournigault/c/X11/xcoral-2.31/bm_search.c
   Description: 
   Created: Fri Jan 27 10:42:58 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 10:42:59 MET 1995
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
#include <string.h>

#ifndef apollo
#ifdef __FreeBSD__
#include <stdlib.h>
#else /* !__FreeBSD__ */
#include <malloc.h>
#endif
#endif

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <sys/types.h>
#define const
#include "regex.h"

#include "main_text.h"
#include "bm_search.h"
#include "page.h"
#include "text_cursor.h"
#include "mark_reg.h"
#include "input_str.h"
#include "warn_box.h"
#include "dial_box.h"

static char *str_save = 0;
static char *str_old = 0;
static char *str_new = 0;
static int abort_debug = False;

extern Display *dpy;

FCT (static void, GotoPos, (Text *text, char *start, int newpos, int dir) );
FCT (static int, SetReplaceString, (Text *text, int type, int from) );
FCT (static char *, SetString, (Text *text, char *prompt, int from) );

#define BYTEWIDTH 8

struct re_pattern_buffer RE_pattern_buffer;
char RE_fastmap[(1 << BYTEWIDTH)];
static struct re_registers RE_registers;

/*
**	Function name : ForwardSearch
**
**	Description : Prepare la recherche vers l'avant.
**
**	Input : Le text courant.
**	Ouput :
*/
void ForwardSearch ( text )
    Text *text;
{
    char *str;
    
    if ( (str = (char *) SetString ( text, "Forward search : ", S_KBD )) == 0 )  {
	DisplayMessage ( text -> mwin, "Abort" );
	return;
    }
    else
      DisplayMessage ( text -> mwin, "Search..." );
    
    if ( HandleForward ( str, text ) == 0 ) {
	DisplayMessage ( text -> mwin, "Search failed" );
	klaxon ();
    }
    
}


/*
**	Function name : HandleForward
**
**	Description : Recherche vers l'avant la chaine 'str'
**
**	Input : La chaine recherchee, le text courant.
**	Ouput :
*/
int HandleForward ( str, text )	
    char *str;
    Text *text;
{	
    int newpos;
    char *start,  *end;
    
    start = (char *) RightBuf ( text -> buf ); 
    end = (char *) BottomBuf ( text -> buf );
    
    if ( (newpos=BmSearch( start, str,  end -start, FORWARD )) != -1 ) {
	if ( newpos == 0 ) { 	/* On est pile dessus */
	    (void) MoveHole ( text -> buf, 1 );	/* Voyons plus loin */
	    start ++;
	    if ( (newpos=BmSearch( start, str,  end - start, FORWARD )) == -1 ) {
		(void) MoveHole ( text -> buf, -1 );
		return 0;
	    }
	}
	GotoPos ( text, start, newpos, FORWARD );
	return 1;
    }
    return 0;
}


/*
**	Function name : BackwardSearch
**
**	Description : Prepare la recherche vers l'arriere.
**
**	Input : Le text.
**	Ouput :
*/
void BackwardSearch ( text )
    Text *text;
{
    char *str;
    
    if ( (str = (char *) SetString ( text, "Backward search : ", S_KBD )) == 0 ) {
	DisplayMessage ( text -> mwin, "Abort" );
	return;
    }
    else
      DisplayMessage ( text -> mwin, "Search..." );	
    
    if ( HandleBackward ( str, text ) == 0 ) {
	DisplayMessage ( text -> mwin, "Search failed" );
	klaxon ();
    }
}


/*
**	Function name : HandleBackward
**
**	Description : Recherche vers l'arriere la chaine 'str'
**
**	Input :  La chaine recherchee, le text courant.
**	Ouput :
*/
int HandleBackward ( str, text )
    char *str;
    Text *text;
{
    int newpos, oldpos;
    char *start,  *end;
    int len;
    
    oldpos = GetNcFromRight ( text -> buf );
    start = (char *) GetCurrentLine ( text -> buf, &len );
    end = (char *) TopBuf ( text -> buf );
    
    start += ( len -1);	/* On part de la fin de la ligne */
    if ( (newpos=BmSearch( start, str, len-1, BACKWARD )) != -1 ) {
	if ( (newpos - oldpos) > 0 ) { /* On est a l'interieur */
	    (void) MoveHole ( text -> buf, -(newpos - oldpos) );
	    return 1;
	}
    }
    start = (char *) LeftBuf ( text -> buf ); /* Allons voir plus loin */

    if ( (newpos=BmSearch ( start, str,  start - end, BACKWARD )) == -1 )
      return 0;
    GotoPos ( text, start, newpos, BACKWARD );
    return 1;
}


/*
**	Function name : GlobalReplace
**
**	Description : Prepare une substitution globale.
**
**	Input : Le text courant.
**	Ouput :
*/
void GlobalReplace ( text )
    Text *text;
{
    if ( SetReplaceString ( text, GLOBAL, S_KBD ) == -1 )
      return;
    (void) HandleGlobal ( text );
}

/*
**	Function name : SetPatterns
**
**	Description : 
**	Input : 
**	Ouput :
*/
void SetPatterns ( old, new )
    char *old, *new;
{
    str_old = old;
    str_new = new;
}

/*
**	Function name : ResetPatterns
**
**	Description : 
**	Input : 
**	Ouput :
*/
void ResetPatterns ()
{
    str_old = 0;
    str_new = 0;
}

/*
**	Function name : HandleGlobal
**
**	Description : Substitution globale.
**
**	Input : Le text courant.
**	Ouput : Le nombre de substitutions.
*/
int HandleGlobal ( text )
    Text *text;
{
    int i, line, newpos, old_pos, modif = False;
    char *start,  *end;
    int f_query = False;
    
    line = text -> no_current_line;
    old_pos = GetNcFromLeft ( text -> buf );
    
    start = (char *) RightBuf ( text -> buf );
    end = (char *) BottomBuf ( text -> buf );
    
    i = 0;
    StorePosition(text);
    
    if ( (newpos=BmSearch( start, str_new,  end - start, FORWARD )) != -1 ) {
	/*
	 * Pour les cas irreversibles, on fait un 'query replace.'
	 */
        f_query = True;
	DisplayMessage ( text -> mwin, "done, but warning : query ..." );
	while ( (newpos=BmSearch( start, str_old,  end -start, FORWARD )) != -1 ) {
	    GotoPos ( text, start, newpos, FORWARD ); 
	    (void) MoveHole ( text -> buf, strlen (str_old)); /* On se place a la fin du mot */
	    StoreInUndo ( text, str_old, str_new, 1 ,0 , U_REP );

	    /* Pour la couleur */
	    if ( text -> current_ce != 0 )
	      (void) UpdateColorList (text, strlen(str_new)-strlen(str_old));

	    DeleteNchar ( text -> buf, strlen ( str_old) );
	    InsertNchar ( text -> buf, str_new, strlen ( str_new));
	    modif = True;
	    start = (char *) RightBuf ( text -> buf ); 
	    end = (char *) BottomBuf ( text -> buf );
	    i++;
	}
    }
    else {
	StoreInUndo ( text, str_old, str_new, 1 ,0 , U_GREP );

	/* Pour la couleur */
	if ( text -> current_ce != 0 )
	  (void) UpdateColorList (text, strlen(str_new)-strlen(str_old));

	while ( (newpos=BmSearch( start, str_old,  end - start, FORWARD )) != -1 ) {
	    (void) MoveHole ( text -> buf, newpos + strlen (str_old)); /* On se place a la fin du mot */
	    DeleteNchar ( text -> buf, strlen ( str_old) );
	    InsertNchar ( text -> buf, str_new, strlen ( str_new));
	    modif = True;
	    start = (char *) RightBuf ( text -> buf ); /* Position courante */
	    i++;
	}
    }
    ResetMark ( text );	
    HoleToLeft ( text -> buf );
    text -> no_current_line = 1;
    GotoLineNumber ( text, line );
    MoveHole ( text -> buf , old_pos );
    SetPosition(text);
    UpdatePage(text);
    
    if ( modif == True )
      SetTextModif ( text );
    return i;
}


/*
**	Function name : QueryReplace
**
**	Description : Substitution sur demande.
**
**	Input : Le text courant.
**	Ouput :
*/
void QueryReplace ( text )
    Text *text;
{
    int newpos;
    char *start, *end;
    char *reply;
    int abort, modif;
    char c = '\007'; /* ^G */
    
    if ( SetReplaceString ( text, QUERY, S_KBD ) == -1 ) return;
    
    start = (char *) RightBuf ( text -> buf ); 
    end = (char *) BottomBuf ( text -> buf );
    
    modif = abort = False;
    
    while ( (newpos=BmSearch( start, str_old,  end -start, FORWARD )) != -1 ) {
	GotoPos ( text, start, newpos, FORWARD ); 
	SetAndDisplayPage ( text );
	TextCursorOn ( text );
	(void) MoveHole ( text -> buf, strlen (str_old)); /* On se place a la fin du mot */
	reply = (char * ) GetString ( text, "Replace [y,n,q] : ", 1 );
	
	if ( (reply == 0) || (strncmp(reply, &c, 1)) == 0 || (strncmp(reply, "q", 1) == 0 )) {
	    abort = True;
	    break;
	}
	if ( strncmp ( reply, "y", 1 ) == 0 ) {
	    StoreInUndo ( text, str_old, str_new, 1 ,0 , U_REP );

	    /* Pour la couleur */
	    if ( text -> current_ce != 0 )
	      (void) UpdateColorList (text, strlen(str_new)-strlen(str_old));

	    UpdateMark ( text, - strlen ( str_old), 0 );
	    DeleteNchar ( text -> buf, strlen ( str_old) );
	    UpdateMark ( text,  strlen ( str_new), 0 );
	    InsertNchar ( text -> buf, str_new, strlen ( str_new));
	    modif = True;
	}
	start = (char *) RightBuf ( text -> buf ); 
	end = (char *) BottomBuf ( text -> buf );
    }
    
    if ( abort == True )
      DisplayMessage ( text -> mwin, "Abort" );
    else
      DisplayMessage ( text -> mwin, "Done" );
    
    /* On revient au debut du mot */
    
    SetAndDisplayPage ( text );
    if ( modif == True )
      SetTextModif ( text );
}


/*
**	Function name : MenuForwardSearch
**
**	Description : Prepare la recherche en avant,
**		 ( a partir du menu ).
**
**	Input : Le text courant.
**	Ouput :
*/
void MenuForwardSearch ( text )
    Text *text;
{
    char *str;
    
    (void) ResetSearchString ();
    if ( (str = (char *) SetString ( text, "Forward search : ", S_MENU )) == 0 )  {
	DisplayMessage ( text -> mwin, "Abort" );
	return;
    }
    if ( HandleForward ( str, text ) == 0 ) {
	DisplayMessage ( text -> mwin, "Search failed" );
	klaxon ();
    }
/*    (void) ResetSearchString (); */
}


/*
**	Function name : MenuBackwardSearch
**
**	Description : Prepare la recherche en arriere,
**		( a partir du Menu ).
**
**	Input : Le text.
**	Ouput :
*/
void MenuBackwardSearch ( text )
    Text *text;
{
    char *str;
    
    (void) ResetSearchString ();
    if ( (str = (char *) SetString ( text, "Backward search : ", S_MENU )) == 0 )  {
	DisplayMessage ( text -> mwin, "Abort" );
	return;
    }
    if ( HandleBackward ( str, text ) == 0 ) {
	DisplayMessage ( text -> mwin, "Search failed" );
	klaxon ();
    }
/*    (void) ResetSearchString (); */
}


/*
**	Function name : MenuQueryReplace
**
**	Description : Substitution a la demande,
**		 ( a partir du menu ).
**
**	Input : Le text.
**	Ouput :
*/
void MenuQueryReplace ( text )
    Text *text;
{
    if ( text )
      DisplayWMessage ( "Warning : Please use esc q", "MenuQueryReplace", True );
}


/*
**	Function name : MenuGlobalReplace
**
**	Description : Substitution globale,
**		( a partir du menu ).
**
**	Input : Le text courant.
**	Ouput :
*/
void MenuGlobalReplace ( text )
    Text *text;
{
    if ( SetReplaceString ( text, GLOBAL, S_MENU ) == -1 )
      return;
    (void) HandleGlobal ( text );
}


/*
**	Function name : BmSearch
**
**	Description : La recherche avec le fameux
**		algo Boyer-Moore.
**
**	Input : voir plus loin.
**	Ouput : La position relative du pattern recherche,
**		sinon -1.
*/
int BmSearch ( buf, str, buf_len, dir )
    char *buf;	/* Le buffer */
    char *str;	/* La string */
    int buf_len;	/* Longueur du buffer */
    int dir;	/* Forward (1), Backward (-1)*/
{
    int s_len = strlen ( str );
    int len = s_len -1;
    int i, j, k, t;
    int index [256];
 
    if((buf_len +1)< s_len) {
      return -1;
    }
    for ( i = 0 ; i < 256 ; i++ )  index [i] = s_len;	
    
    for ( i = 0; i < s_len; i++ ) {
	k = ( dir == FORWARD ) ? len - i : i;
	if ( index [(int) str [k]] == s_len ) 
	  index [(int) str [k]] = i;
    }
#ifdef DEBUG
    for ( i = 0 ; i < 256 ; i++ ) 
      if ( index[i] != s_len )
	fprintf ( stderr, "index %c %d = %d\n", i, index [i],  (char) i);
#endif
    for ( i = ( dir == FORWARD) ? len : 0, 
	 j = ( dir == FORWARD) ? len : 0;
	 (( dir == FORWARD) ? ( j>= 0) : (j <= len));
	 i -= ( dir == FORWARD) ? FORWARD : BACKWARD,
	 j -= ( dir == FORWARD) ? FORWARD : BACKWARD ) { 
#ifdef DEBUG
	(void) fprintf ( stderr, "i = %d, j = %d\n", i, j );
#endif
	while ( buf [i] != str [j] ) {
/*	    t = index [(int) buf [i] ]; */
	    t = index [(unsigned char) buf [i] ];
	    if ( dir == FORWARD ) {
#ifdef DEBUG 
		(void) fprintf ( stderr, "buf[%d] = %d str[%d] = %c t = %d\n",
				i, (unsigned char) buf[i], j, str[j], t );
#endif
		i += (( s_len - j ) > t) ? s_len - j : t;
		if ( i >= buf_len +1 ) return -1;
		j = len;
	    }
	    else {
#ifdef DEBUG 
		(void) fprintf ( stderr, "buf[%d] = %c str[%d] = %c t = %d\n", i, buf[i], j, str[j], t );
#endif
		i -= ( j > t) ? s_len : t; 
		
		if ( (buf_len +1 +i) < 0 ) return -1;
		j = 0; 
	    }
	}
    }
    return ( dir == FORWARD ) ? (i + 1) : (- ( i - s_len -1 ));
}

/*
**	Function name : SetString
**
**	Description : Positionne le pattern a rechercher.
**
**	Input : Le text courant, le prompt, clavier/menu
**
**	Ouput : Le pattern.
*/
static char *SetString ( text, prompt, from )
    Text *text;
    char *prompt;
    int from;
{
    char *str;
    char c = '\007'; /* ^G */
    int len;
    
    if ( (str_save == 0) || (strlen (str_save)) == 0 ) {
	if ( from == S_KBD )
	  str = (char *) GetString ( text, prompt, False );
	else
	  str = ( char *) GetStringFromDB ( prompt, False );
	
	if ( str == 0 )
	  return 0;
	if ( strncmp(str, &c, 1) == 0 ) {
	    abort_debug = True;
	    return 0;
	}
	if ( str_save != 0 )
	  (void) free ( str_save );
	len = strlen (str);
	str_save = (char *) malloc ( (unsigned) len + 1);
	(void) strncpy ( str_save, str,  len);
	*(str_save + len) = '\0';
    }
    return str_save;
}

/*
**	Function name : SetReplaceString
**
**	Description : Positionne les patterns utilises
**		lors des substitutions.
**
**	Input : Le text courant, Query/Global, menu/clavier.
**	Ouput : Ok 0, sinon -1.
*/
static int SetReplaceString ( text, type, from )
    Text *text;
    int type;
    int from;
{
    char *str;
    char buf [64], tmp[64];
    int len;
    
    if ( type == GLOBAL )
      (void) strcpy ( tmp, "Global replace," );
    else
      (void) strcpy ( tmp, "Query replace," );	
    (void) ResetSearchString ();
    (void) strcpy ( buf, tmp );
    (void) strcat ( buf, " Old String : " );
    if ( (str = (char *) SetString ( text, buf, from )) == 0 ) {
	DisplayMessage ( text -> mwin, "Abort" );
	return -1;
    }
    len = strlen (str);
    if ( str_old != 0 )
      (void) free ( str_old );
    str_old = (char * ) malloc ( (unsigned) len + 1);
    (void) strncpy ( str_old, str, len );
    *(str_old + len) = '\0';
    
    (void) ResetSearchString ();
    
    buf [0] = '\0';
    (void) strcpy ( buf, tmp );
    (void) strcat ( buf, " New String : " );
    
    if ( (str = (char *) SetString ( text, buf, from )) == 0 ) {
	if ( abort_debug == True ) {
	    DisplayMessage ( text -> mwin, "Abort" );
	    abort_debug = False;
	    return -1;
	}
	else {
	    (void) strcpy ( buf, "" );
	    str = buf;
	}
    }
    
    len = strlen (str);
    if ( str_new != 0 )
      (void) free ( str_new );
    str_new = (char *) malloc ( (unsigned) len + 1 );
    (void) strncpy ( str_new, str, len);
    *(str_new+ len) = '\0';
    (void) ResetSearchString ();
    return 0;
}


/*
**	Function name : GotoPos
**
**	Description : Va a la position relative a
**		partir de la position courante.
**
**	Input : Le text cournant, la position de depart,
**		nb octets, direction.
**
**	Ouput :
*/
static void GotoPos ( text, start, newpos, dir )
    Text *text;
    char *start;
    int newpos;
    int dir;
{
    int line;
    int x = ( dir == FORWARD ) ? newpos : -newpos;

    line = GetNewLine ( start, x );
    text -> no_current_line += (dir == FORWARD ) ? line : -line;
    
    (void) MoveHole ( text -> buf, x );	
    if ( line != 0 ) {
	if ( (line > (( dir == FORWARD) ? text -> n2 : text -> n1 )))
	  CurrentLineToMiddle ( text );
	else {
	    text -> n1 += (dir == FORWARD) ? line : -line;
	    text -> n2 -= (dir == FORWARD) ? line : -line;				
	    SetAndDisplayPage ( text );
	}
    }
}


/*
**	Function name : DeleteStrings
**
**	Description : 
**	Input : 
**	Ouput :
*/
void DeleteStrings ()
{
    if ( str_save != 0 )
      (void) free ( str_save );
    if ( str_old != 0 )   
      (void) free ( str_old );
    if ( str_new != 0 )
      (void) free ( str_new );
}


/*
**	Function name : ResetSearchString
**
**	Description : 
**	Input : 
**	Ouput :
*/
void ResetSearchString ()
{
    if ( str_save != 0 )
      (void) free ( str_save );
    str_save = 0;
}

/*
**	Function name : SearchInMan
**
**	Description : 
**	Input : 
**	Ouput :
*/
void SearchInMan ( text )
    Text *text;
{
    char *str;
    
    (void) ResetSearchString ();
    if ( (str = (char *) SetString ( text, "Manual Search : ", S_MENU )) == 0 )  {
	/* Abort */
	return;
    }
    
    if ( HandleForward ( str, text ) != 0 )
      return;
    else if ( HandleBackward ( str, text ) != 0 )
      return;
    else
      klaxon ();
}

/*===============
   REGEX SEARCH
================*/   
/*
**	Function name : UpddateRegs
**
**	Description : Juste pour ajouter la longueur de
**         la partie gauche de la bulle (dans le cas d'un
**         forward_search.
**         Ceci pour avoir des positions SMAC correctes
**         pour les re_sub_expressions.  
**	Input : La position courante a ajouter.
**	Output :
*/
void UpdateRegs (n)
{
    
    int i = RE_registers.num_regs;
    
    for (i=0;i<RE_registers.num_regs;i++) {
	RE_registers.start [i] += n;
	RE_registers.end [i] += n;
    }
}


/* #define DEBUG */
/*
**	Function name : RE_Match
**
**	Description : 
**	Input : le numeron du registre (0 et 1 correspondent
**         la premiere re_subexp. Un flag boleen pour le debut
**         ou la fin de la re_subexp courante.
**	Output : La position du debut ou de la fin de la re_subexp
**         courante.  
*/
int RE_Match (n, beginning )
    int n;
    int beginning;
{
  if ((n < 0) || (n >= RE_registers.num_regs)
      || (RE_registers.num_regs <= 0) || (RE_registers.start[n] < 0))
    return -1;

  if (beginning) 
    return (RE_registers.start[n]);
  else
    return (RE_registers.end[n]-1);
}

/*
**	Function name : RE_MenuForwardSearch
**
**	Description :
**	Input :
**	Output :
*/
void RE_MenuForwardSearch ( text )
    Text *text;
{
    int newpos, orig;
    char *str;
    
    (void) ResetSearchString ();
    if ( (str = (char *) SetString ( text, "Regex forward search : ", S_MENU )) == 0 )  {
	DisplayMessage ( text -> mwin, "Abort" );
	return;
    }
    else
      DisplayMessage ( text -> mwin, "Search..." );

    orig = ie_current_position (text);
    newpos = ie_re_search_forward ( text, str );

    if ((newpos == -1) || (newpos == -2) ){
      klaxon (); 
      return;
    }
    else {
	if ( orig == newpos ) {
	    /* On est pile dessus */
    	    (void) MoveHole ( text -> buf, 1 );	/* Voyons plus loin */
	    newpos = ie_re_search_forward ( text, str );
	    if ((newpos == -1) || (newpos == -2) ){
		(void) MoveHole ( text -> buf, -1 );
		klaxon (); 
		return;
	    }
	}
	(void) ie_goto_char (text, newpos);	
	ie_redisplay (text);
    }
}

/*
**	Function name : RE_MenuBackwardSearch
**
**	Description : Recherche en arriere d'une regex.
**         Si on est deja a l'interieur de la regex recherchee,
**         celle-ci n'esp pas vue.  
**	Input :
**	Output :
*/
void RE_MenuBackwardSearch ( text )
    Text *text;
{
    int newpos;
    char *str;
    
    (void) ResetSearchString ();
    if ( (str = (char *) SetString ( text, "Regex backward search : ", S_MENU )) == 0 )  {
	DisplayMessage ( text -> mwin, "Abort" );
	return;
    }
    else
      DisplayMessage ( text -> mwin, "Search..." );

    newpos = ie_re_search_backward ( text, str );

    if ((newpos == -1) || (newpos == -2) ){
	klaxon (); 
	return;
    }
    else {
      (void) ie_goto_char (text, newpos);
      ie_redisplay (text);
    }
}

/*
**	Function name : RE_Search
**
**	Description : Recherche d'une expression reguliere dans un buffer.
**         On considere que les frontieres sont valides (buffer + ou - size)
**	Input : L'expression reguliere, le buffer, la frontiere dans le 
**         buffer et la direction de recherche.
**	Output : la position dans le buffer, -1 sinon et -2 si error (compile).
*/
int RE_Search ( regex, buffer, size, direction)
    char *regex;
    char *buffer;
    int size;
    int direction;
{
    char *compile_result;
    static char *old_regex = 0;
    int search_result;
    
    int start = (direction == FORWARD) ? 0 : size-1;
    int range = (direction == FORWARD) ? size-1 : -size+1;
/*
    int start = (direction == FORWARD) ? 0 : size;
    int range = (direction == FORWARD) ? size : -size;
*/    
    if ((old_regex == 0) || (strcmp (old_regex, regex) != 0)) {
	/*
	   Si regex n'a pas change, c'est pas la peine de recompiler
	*/
	RE_pattern_buffer.fastmap = RE_fastmap;
	compile_result = re_compile_pattern (regex, strlen (regex), &RE_pattern_buffer);
	if (compile_result != (char *) 0)
	  return (-2);

	if (old_regex)
	  (void) free(old_regex);
	old_regex = (char*) malloc ((unsigned) strlen(regex) + 1);
	strcpy (old_regex, regex);
    }
#ifdef DEBUG
    (void) fprintf ( stderr, "size = %d range = %d\n reg = %s\n",
		    size, range, regex );
#endif DEBUG
    search_result = re_search ( &RE_pattern_buffer,
			       buffer, size, start, range, &RE_registers ); 

    return (search_result);
}

/*
**	Function name : RE_Replace
**
**	Description :
**	Input :
**	Output :
*/
void RE_MenuReplace ( text )
    Text *text;
{
    if ( SetReplaceString ( text, QUERY, S_MENU ) == -1 ) return;
    
    ie_re_replace ( text, str_old, str_new );
    ie_redisplay (text);
}

/*
**	Function name : RE_ReplaceMatch
**
**	Description : This function is derived from the emacs replace_match
**	Input :
**	Output : True if OK
*/
int RE_ReplaceMatch ( text, newstring )
    Text *text;
    char *newstring;
{
    int i,j,re_len, new_len = strlen(newstring);
    int nth_c, c;
    
    char *buffer;

    re_len = RE_registers.end[0] - RE_registers.start[0];
    
    /* newstring peut contenir \& ou \n avec 0<n<10.
       \& : substitution par la regex.
       \n : substitution par la substring n de la regex.
    */
    
    ie_goto_char(text, ie_current_position(text) + RE_registers.start[0]);
    buffer = RightBuf(text->buf); /* On est sur la regex */
    for (i=0;i<new_len;i++) {
	c = newstring[i];
	if (c == '\\') {
	    c = newstring[++i];
	    if (c == '&') {
		for(j=0;j<re_len;j++) {
		    nth_c = RE_registers.start[0]+j;
		    ie_insert_char (text, buffer[nth_c]);
		}
	    }
	    else if ((c >=1) && (c <= RE_registers.num_regs)) {
		re_len = RE_registers.end[c] - RE_registers.start[c];
		for(j=0;j<re_len;j++) {
		    nth_c = RE_registers.start[0]+j;
		    ie_insert_char (text, buffer[nth_c]);
		}
	    }
	    else {
		ie_insert_char (text, c);
	    }
	}
	else {
	    ie_insert_char (text,c);
	}
    }
    /* On vire la regex originale */
    for(j=0;j<re_len;j++) {
	ie_delete_char (text);
    }
    
    return (1);
}
