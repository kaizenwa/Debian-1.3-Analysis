/* ########################################################################

				 undo.c

   File: undo.c
   Path: /home/fournigault/c/X11/xcoral-2.31/undo.c
   Description: 
   Created: Fri Jan 27 11:38:09 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:38:11 MET 1995
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
#include <string.h>

#include "main_text.h"
#include "page.h"
#include "text_cursor.h"
#include "mark_reg.h"
#include "bm_search.h"
#include "chars_cmds.h"

FCT (static Undo *, AllocUndoBuf, () );
FCT (static void, CompactUndo, (Text *text) );
FCT (static void, Do_UAddChars, (Text *text) );
FCT (static void, Do_UAddOneChar, (Text *text) );
FCT (static void, Do_UDelChars, (Text *text) );
FCT (static void, Do_UDelOneChar, (Text *text) );
FCT (static void, Do_UReplace, (Text *text,int type, int old_line, int old_pos) );
FCT (static Undo *, GetCurrentUndo, (Text *text) );
FCT (static void, U_AddChars, (Text *text, int type, char *s, int len, int n) );
FCT (static void, U_DelChars, (Text *text, char *s, int len) );
FCT (static void, U_OneChar, (Text *text, int len, char *s, char *sr, int type) );
FCT (static void, U_RepAlloc, (char **p, char *str) );

static int flag_redo = 0;

extern Display *dpy;

/* #define U_TRACE */

FCT (void, DeleteUndo, (Text *text));
FCT (void, ResetUndo, (Text *text));
FCT (void, StoreInUndo, (Text *text, char *s, char *sr, int len, int n, int type));
FCT (void, DoUndo, (Text *text));
FCT (void, DoUndoUndo, (Text *text));

/*
**	Pour le buffer courant, les infos seront stokees dans
**	une liste chainee de structures Undo. Chaque element
**	de la liste correspond a une modification du buffer.
*/

/*
**	Function name : GetCurrentUndo
**
**	Description : Retourne les infos de l'element correspondant
**		a l'index courant.
**
**	Input : Le Text courant
**	Ouput : Un pointeur sur la structure Undo courante
*/
static Undo *GetCurrentUndo ( text )
    Text *text;
{
    int index = 0;
    Undo *tmp = text -> udi;
    
    while ( index != text -> u_index ) {
	tmp = tmp -> next;
	if ( tmp == 0 )
	  break;
	index ++;
    }
#ifdef U_TRACE
    (void) fprintf ( stderr, "GetCurrentUndo : index= %d\n", index );
#endif /* U_TRACE */
    return ( tmp );
}

/*
**	Function name : DeleteUndo
**
**	Description : Destruction complete de Undo.
**		Pour chaque element, on libere les buffers
**		alloues : buf, str_new et str_old et pour finir
**		l'element lui-meme.
**
**	Input : Le Text courant.
**	Ouput :
*/
void DeleteUndo ( text )
    Text *text;
{
    Undo *tmp, *current_u = text -> udi;
#ifdef U_TRACE
    int i =0;
#endif /* U_TRACE */
    
    if ( current_u == 0 ) {
#ifdef U_TRACE
	(void) fprintf ( stderr, "DeleteUndo : empty\n" );
#endif /* U_TRACE */
	return;
    }
    
    while ( True ) {
	if ( current_u -> buf  != 0 ) {
#ifdef U_TRACE
	    (void) fprintf ( stderr, "DeleteUndo : free (buf) %d\n",  
			    current_u -> buf );
#endif /* U_TRACE */
	    (void) free ( current_u -> buf );
	}
	if ( current_u -> str_old != 0 ) {
#ifdef U_TRACE
	    (void) fprintf ( stderr, "DeleteUndo : free (str_old) %d\n",
			    current_u -> str_old );
#endif /* U_TRACE */
	    (void) free ( current_u -> str_old );
	}
	if ( current_u -> str_new != 0 ) {
#ifdef U_TRACE
	    (void) fprintf ( stderr, "DeleteUndo : free (str_new) %d\n", 
			    current_u -> str_new );
#endif /* U_TRACE */
	    (void) free ( current_u -> str_new );
	}
	if ( current_u -> next == 0 ) {
#ifdef U_TRACE
	    (void) fprintf ( stderr, "DeleteUndo : free (end) %d\n",  current_u );
#endif /* U_TRACE */
	    (void) free ( (char *) current_u );		
#ifdef U_TRACE
	    i++;
#endif /* U_TRACE */
	    break;
	}
	else {
	    tmp = current_u -> next;
#ifdef U_TRACE
	    (void) fprintf ( stderr, "DeleteUndo : free %d\n",  current_u );
#endif /* U_TRACE */
	    (void) free ( (char *) current_u );
	    current_u = tmp;
	}
    }
#ifdef U_TRACE
    (void) fprintf ( stderr, "DeleteUndo : free %d undo elements %d\n", i );
#endif /* U_TRACE */
    
}

/*
**	Function name : AllocUndoInfos
**
**	Description : Alloue une structure Undo et mise a zero
**	Input : 
**	Ouput : Le pointeur sur la structure ou 0 si ya plus de memoire.
*/
static Undo *AllocUndoBuf ()
{
    Undo *tmp;
    
    if (( tmp = (Undo *) malloc ((unsigned) sizeof (Undo))) == 0 )
      return 0;
    
    bzero ( (char *) tmp, sizeof (Undo));
#ifdef U_TRACE
    (void) fprintf ( stderr, "AllocUndoBuf : %d\n", tmp );
#endif /* U_TRACE */
    
    return (tmp);
}

/*
**	Function name : ResetUndo
**
**	Description : Mise a zero de la liste.
**		
**	Input : 	Le Text courant
**	Ouput :
*/
void ResetUndo ( text )
    Text *text;
{
    if ( text -> udi == 0 ) {
#ifdef U_TRACE
	(void) fprintf ( stderr, "ResetUndo : empty\n" );
#endif /* U_TRACE */
	return;
    }
    
    DeleteUndo ( text );
    
    text -> udi = 0;
    text -> u_index = 0;
#ifdef U_TRACE
    (void) fprintf ( stderr, "ResetUndo\n" );
#endif /* U_TRACE */
}

/*
**	Function name : StoreInUndo
**
**	Description : Pour chaque modification du text-buffer, on
**		passe dans cette fonction.
**
**	Input : 
**		Le Text courant,
**		La chaine ajoutee ou enlevee,
**		La nouvelle chaine dans le cas d'une substitution,
**		La longueur de la chaine,
**		Le nombre de lignes dans la chaine,
**		Le type de l'operation.
**		
**	Ouput :
*/
void StoreInUndo ( text, s, sr, len ,n, type )
    Text *text;
    char *s, *sr;
    int len;
    int n;
    int type;
{
    Undo *new_udi;
    
    if ( text -> shell_id != 0 ) 
      return;
    
    if ( s == 0 ) {
#ifdef U_TRACE
	(void) fprintf ( stderr, "StoreInUndo : (null) string\n" );
#endif /* U_TRACE */
	return;
    }
    
    /*
       * Si des 'undo' ont ete effectues, alors on les vire.
    */
    if ( text -> u_index != 0 )
      CompactUndo ( text );
    
    if ( (new_udi = AllocUndoBuf ()) == 0 ) 
      (void) fprintf ( stderr, "Hum... AllocUndoBuf memory error\n" );
    
    if ( text -> udi != 0 ) {
#ifdef U_TRACE
	(void) fprintf ( stderr, "StoreInUndo : link new to current undo\n" );
#endif /* U_TRACE */
	/* Le chainage */
	new_udi -> next = text -> udi;
    }
    text -> udi = new_udi;
    
    if ( len == 1 || len == -1 ) {
	/* 
	   * on ajoute ou on enleve 1 seul caractere 
	   * si sr != 0 alors c'est une substitution.
	*/
	(void) U_OneChar ( text, len, s, sr, type );
    }
    
    else { 			
	if ( len > 0 ) {
	    /* On a ajoute plusieurs caracteres */
	    (void) U_AddChars ( text, type, s, len, n );
	}
	else {	
	    /* on enleve plusieurs caracteres */
	    (void ) U_DelChars ( text, s, len );
	}
	text -> udi -> one_char = False;
	text -> udi -> nb_lines = n;
    }
    text -> u_index = 0;
}

/*
**	Function name : U_DelChars
**
**	Description : Plusieurs caracteres ont ete effaces.
**		Avant de stoker les infos, on verifie si ya assez
**		de place pour ranger les caracteres effaces. 	
**
**	Input : Le Text courant, la chaine effacee et sa longueur.
**	Ouput :
*/
static void U_DelChars (text, s, len )
    Text *text;
    char *s;
    int len;
{
#ifdef U_TRACE
    (void) fprintf ( stderr, "U_DelChars\n" );
#endif /* U_TRACE */
    
    if ( (- len ) > text -> udi -> size ) {
#ifdef U_TRACE
	(void) fprintf ( stderr, "U_DelChars : len > buf_size \n" );
#endif /* U_TRACE */
	if ( text -> udi -> buf  != 0 ) {
#ifdef U_TRACE
	    (void) fprintf ( stderr, "U_DelChars : buf : free before malloc : %d\n",
			    text -> udi -> buf );
#endif /* U_TRACE */
	    (void) free ( text -> udi -> buf );
	}
	text -> udi -> buf = (char *) malloc ( (unsigned) (-len) + 1 );
#ifdef U_TRACE
	(void) fprintf ( stderr, "U_DelChars : buf : malloc : %d\n", text -> udi -> buf );
#endif /* U_TRACE */
	text -> udi -> size = (-len);
    }
    /*	(void) strncpy ( text -> udi -> buf, s, -len ); */
    (void) bcopy ( s, text -> udi -> buf, -len );
    
    text -> udi -> buf [-len] = 0;
    
    text -> udi -> pos = GetNcFromLeft ( text -> buf );
    text -> udi -> current_line = text -> no_current_line;
    text -> udi -> op = U_DEL;
    text -> udi -> type = U_STD;
}

/*
**	Function name : U_AddChars
**
**	Description : Plusieurs caracteres ont ete ajoutes.
**		Avant de stocker les infos, on verifie si ya assez
**		de place pour ranger les caracteres ajoutes. 	
**
**	Input : Le Text courant,
**		le type de l'operation effectuee ( U_INSERT, U_STD ),
**		la chaine et sa longueur,
**		le nombre de lignes.
**	Ouput :
*/
static void U_AddChars ( text,type, s, len, n )
    Text *text;
    int type;
    char *s;
    int len, n;
{
#ifdef U_TRACE
    (void) fprintf ( stderr, "U_AddChars\n" );
#endif /* U_TRACE */
    if ( len > text -> udi -> size ) {
#ifdef U_TRACE
	(void) fprintf ( stderr, "U_AddChars : len > buf_size\n" );
#endif /* U_TRACE */
	if ( text -> udi -> buf  != 0 ) {
#ifdef U_TRACE
	    (void) fprintf ( stderr, "U_AddChars : buf : buf free before malloc %d\n", text -> udi -> buf );
#endif /* U_TRACE */
	    (void) free ( text -> udi -> buf );
	}
	text -> udi -> buf = (char *) malloc ( (unsigned) len + 1 );
#ifdef U_TRACE
	(void) fprintf ( stderr, "U_AddChars : buf : malloc %d\n", text -> udi -> buf );
#endif /* U_TRACE */
	text -> udi -> size = len;
    }
    /*	(void) strncpy ( (char *) text -> udi -> buf, (char *) s, len ); */
    (void) bcopy ( (char *) s, (char *) text -> udi -> buf, len );
    text -> udi -> buf [len] = 0;
    switch  ( type ) {
    case U_INSERT:
#ifdef U_TRACE
      (void) fprintf ( stderr, "U_AddChars : U_INSERT\n" );
#endif /* U_TRACE */
      text -> udi -> pos = GetNcFromLeft ( text -> buf );
      text -> udi -> current_line = text -> no_current_line;
      break;
    case U_STD:
#ifdef U_TRACE
      (void) fprintf ( stderr, "U_AddChars : U_STD\n" );
#endif /* U_TRACE */
      MoveHole ( text -> buf, -len );
      text -> udi -> pos = GetNcFromLeft ( text -> buf );
      MoveHole ( text -> buf, len );
      text -> udi -> current_line = text -> no_current_line - n;
      break;
    }
    text -> udi -> op = U_ADD;
    text -> udi -> type = type;
}


/*
**	Function name : U_OneChar
**
**	Description : Un caractere a ete ajoute ou enleve.
**		On se sert aussi de cette fonction pour les
**		substitutions (global ou query).			
**
**	Input : Le Text courant,
**		La longeur : -1 ou +1 (enleve ou ajoute).
**		Le caractere ou l'ancienne chaine si substitution.
**		La nouvelle chaine si substitution.
**		Le type (U_INSERT,U_STD, U_REP et U_GREP).
**	Ouput :
*/
static void U_OneChar ( text, len, s, sr, type )
    Text *text;
    int len;
    char *s, *sr;
    int type;
{
    switch ( type ) {
    case U_INSERT:
#ifdef U_TRACE
      (void) fprintf ( stderr, "U_OneChar : U_INSERT\n" );
#endif /* U_TRACE */
      text -> udi -> pos = GetNcFromLeft ( text -> buf ) +1;
      text -> udi -> op = U_ADD;
      text -> udi -> c = *s;
      break;
    case U_STD:
#ifdef U_TRACE
      (void) fprintf ( stderr, "U_OneChar : U_STD\n" );
#endif /* U_TRACE */
      text -> udi -> c = *s;
      text -> udi -> pos = 
	(len == 1) ? GetNcFromLeft ( text -> buf ) : 1+GetNcFromLeft ( text -> buf );
      text -> udi -> op = (len == 1) ? U_ADD : U_DEL;
      break;
    case U_REP:
    case U_GREP:
#ifdef U_TRACE
      (void) fprintf ( stderr, "U_OneChar : U_REPLACE\n" );
#endif /* U_TRACE */
      text -> udi -> pos = GetNcFromLeft ( text -> buf );
      if ( type == U_REP ) {
#ifdef U_TRACE
	  (void) fprintf ( stderr, "U_OneChar : U_REP\n" );
#endif /* U_TRACE */
	  text -> udi -> pos -= strlen ( s );
      }
      U_RepAlloc ( &text -> udi -> str_new, sr );
      U_RepAlloc ( &text -> udi -> str_old, s );
      break;
    }
    text -> udi -> current_line = text -> no_current_line;
    text -> udi -> one_char = True; 
    text -> udi -> type = type;
    text -> udi -> nb_lines = 0;
    text -> udi -> size = 1;
}

/*
**	Function name : U_RepAlloc
**
**	Description : Allocation de memoire dans le cas d'une substitution et
**		copie de la chaine.
**
**	Input : Le buffer, la chaine a copier.
**	Ouput :
*/
static void U_RepAlloc ( p, str )
    char **p, *str;
{
    if ( *p == 0 ) {
	*p = (char *) malloc ( (unsigned) strlen (str) + 1);
#ifdef U_TRACE
	(void) fprintf ( stderr, "U_RepAlloc : p (null) : malloc %d\n", *p );
#endif /* U_TRACE */
    }
    else {
	if ( (strlen (str) +1) > strlen ( *p ) ) {
#ifdef U_TRACE
	    (void) fprintf ( stderr, "U_RepAlloc : p : free before malloc %d\n", *p );
#endif /* U_TRACE */
	    (void) free ( *p );
	    *p = (char *) malloc ( (unsigned) strlen (str) + 1);
#ifdef U_TRACE
	    (void) fprintf ( stderr, "U_RepAlloc : p : malloc %d\n", *p );
#endif /* U_TRACE */
	}
    }
    (void) strcpy ( *p, str );
}

/*
**	Function name : CompactUndo
**
**	Description : Vire les structures Undo qui ne sont plus
**		necessaires. C'est le cas lorsqu'on modifie le texte
**		apres un ou plusieurs 'undo'. Dans ce cas, on efface
**		depuis le debut jusqu'a l'index.
**
**	Input : Le Text courant.
**	Ouput :
*/
static void CompactUndo ( text )
    Text *text;
{
    Undo *tmp, *current_u = text -> udi;
#ifdef U_TRACE
    (void) fprintf ( stderr, "CompactUndo from index %d\n", text -> u_index );
#endif /* U_TRACE */
    
    while ( text -> u_index != 0 ) {
	if ( current_u -> buf  != 0 ) {
#ifdef U_TRACE
	    (void) fprintf ( stderr, "CompactUndo : free (buf) %d\n", current_u -> buf );
#endif /* U_TRACE */
	    (void) free ( current_u -> buf );
	}
	if ( current_u -> str_old != 0 ) {
#ifdef U_TRACE
	    (void) fprintf ( stderr, "CompactUndo : free (str_old) %d\n", current_u -> str_old);
#endif /* U_TRACE */
	    (void) free ( current_u -> str_old );
	}
	if ( current_u -> str_new != 0 ) {
#ifdef U_TRACE
	    (void) fprintf ( stderr, "CompactUndo : free (str_new) %d\n", current_u -> str_new );
#endif /* U_TRACE */
	    (void) free ( current_u -> str_new );
	}
	tmp = current_u  -> next;
#ifdef U_TRACE
	(void) fprintf ( stderr, "CompactUndo : free %d\n", tmp );
#endif /* U_TRACE */
	(void) free ( (char *) current_u );
	current_u = tmp;
	text -> u_index --;
    }
    text -> u_index = 0;
    text -> udi = tmp;
}

/*
**	Function name : DoUndo
**
**	Description : Point d'entree de l'operation 'undo'
**
**	Input : Le Text courant.
**	Ouput :
*/
void DoUndo ( text )
    Text *text;
{
    int old_line, old_pos;
    Mode *save_mode = text -> current_mode;
    Undo *tmp = GetCurrentUndo ( text );
    
    text -> current_mode = (Mode *) GetMode ("default");
#ifdef U_TRACE
    (void) fprintf ( stderr, "DoUndo\n" );
#endif /* U_TRACE */
    
    old_pos = 	GetNcFromLeft ( text -> buf );
    old_line = text -> no_current_line;
    
    if ( tmp == 0 ) {
#ifdef U_TRACE
	(void) fprintf ( stderr, "DoUndo : end\n" );
#endif /* U_TRACE */
	klaxon ();
	text -> current_mode = save_mode;
	return;
    }
    if ( text -> no_current_line != tmp -> current_line ) {
#ifdef U_TRACE
	(void) fprintf ( stderr, "DoUndo : gotoline before\n" );
#endif /* U_TRACE */
	StorePosition(text);
	GotoLineNumber ( text ,  tmp -> current_line );
	SetPosition(text);
	UpdatePage(text);

    }
    else {
#ifdef U_TRACE
	(void) fprintf ( stderr, "DoUndo : same line, go to beginning before\n" );
#endif /* U_TRACE */
	MoveToBline ( text );
    }
    
    if ( tmp -> one_char == True ) {
	switch ( tmp -> type ) {
	case U_STD:
	case U_INSERT:
	  if ( tmp -> op == U_ADD ) 
	    (void) Do_UDelOneChar ( text );
	  else 
	    (void) Do_UAddOneChar ( text );
	  break;
	case U_REP:
	case U_GREP:
	  (void) Do_UReplace ( text, tmp -> type, old_line, old_pos );
	  break;
	default :
	  break;
	}
    }
    else {
	if ( tmp -> op == U_ADD ) 
	  (void) Do_UDelChars ( text );
	else 
	  (void) Do_UAddChars ( text );
    }
    text -> u_index ++;
    text -> current_mode = save_mode;
    if ( (tmp -> next == 0) && (flag_redo == 0) ) {
	text -> modif = False;
	text -> mwin -> stat = False;
	RefreshWindowStatBuf ( text -> mwin );
    }
}

/*
**	Function name : Do_UReplace
**
**	Description : Undo dans le cas d'une substitution.
**
**	Input : Le Text courant.
**		le type (U_REP et U_GREP).
**		la position courante (utilisee dans le cas d'une
**		substitution globale).
**	Ouput :
*/
static void Do_UReplace ( text, type, old_line, old_pos )
    Text *text;
    int type;
    int old_line, old_pos;
{
    
    int newpos, modif = False;
    char *start, *end, *tmp;
    Undo *current_u = GetCurrentUndo ( text );
    char *str_old = current_u -> str_new;
    char *str_new = current_u -> str_old;
    
#ifdef U_TRACE
    (void) fprintf ( stderr, "Do_UReplace\n" );
#endif /* U_TRACE */
    
    TextCursorOff ( text );
    SetTextModif ( text );
    MoveHole ( text -> buf , current_u -> pos );
    
    start = (char *) RightBuf ( text -> buf );
    end = (char *) BottomBuf ( text -> buf );

    while ( (newpos=BmSearch( start, str_old,  end - start, 1 )) != -1 ) {
	/* On se place a la fin du mot */
	(void) MoveHole ( text -> buf, newpos + strlen (str_old)); 
	DeleteNchar ( text -> buf, strlen ( str_old) );
	InsertNchar ( text -> buf, str_new, strlen ( str_new));

	/* Pour la couleur */
	if ( text -> current_ce != 0 )
	  (void) UpdateColorList ( text, strlen(str_new)-strlen(str_old));
  
	modif = True;
	start = (char *) RightBuf ( text -> buf ); /* Position courante */
	if ( type == U_REP ) {
#ifdef U_TRACE
	    (void) fprintf ( stderr, "Do_UReplace : U_REP\n" );
#endif /* U_TRACE */
	    break;
	}
    }
    HoleToLeft ( text -> buf );
    text -> no_current_line = 1;
    
    if ( type == U_REP ) {
#ifdef U_TRACE
	(void) fprintf ( stderr, "Do_UReplace : U_REP\n" );
#endif /* U_TRACE */
	GotoLineNumber ( text, current_u -> current_line );
	MoveHole ( text -> buf , current_u -> pos );
    }
    else {
#ifdef U_TRACE
	(void) fprintf ( stderr, "Do_UReplace : U_GREP\n" );
#endif /* U_TRACE */
	GotoLineNumber ( text, old_line );
	MoveHole ( text -> buf , old_pos );
    }
    CurrentLineToMiddle ( text );
    if ( modif == True ) {
#ifdef U_TRACE
	(void) fprintf ( stderr, "Do_UReplace : modif\n" );
#endif /* U_TRACE */
	SetTextModif ( text );
    }
    
    tmp = (char *) malloc ( (unsigned) strlen ( str_old ) + 1 );
#ifdef U_TRACE
    (void) fprintf ( stderr, "Do_UReplace : malloc (tmp) %d\n", tmp );
#endif /* U_TRACE */
    (void) strcpy ( (char *) tmp, current_u  -> str_old );
    U_RepAlloc ( &current_u -> str_old, current_u -> str_new );
    U_RepAlloc ( &current_u -> str_new, tmp );
#ifdef U_TRACE
    (void) fprintf ( stderr, "Do_UReplace : free (tmp) %d\n", tmp );
#endif /* U_TRACE */
    (void) free ( tmp );
}

/*
**	Function name : Do_UAddChars
**
**	Description : Undo : ajoute plusieurs caracteres.
**
**	Input : Le Text courant.
**	Ouput :
*/
static void Do_UAddChars ( text )
    Text *text;
{
    Undo *current_u = GetCurrentUndo (text);
#ifdef U_TRACE
    (void) fprintf ( stderr, "Do_UAddChars \n" );
#endif /* U_TRACE */
    
    TextCursorOff ( text );
    SetTextModif ( text );
    MoveHole ( text -> buf, current_u -> pos );
    if ( current_u -> type == U_INSERT ) {
#ifdef U_TRACE
	(void) fprintf ( stderr, "Do_UAddChars : U_INSERT\n" );
#endif /* U_TRACE */
	SaveMark ( text );
	SetMark (text );
    }
    ClipOn ( text, 0 );
    InsertLines ( text, current_u -> buf, current_u -> size, current_u -> nb_lines );

    /* Pour la couleur */
    if ( text -> current_ce != 0 ) {
      (void) UpdateColorList ( text, current_u -> size );
      RefreshPage(text);
    }
    if ( current_u -> type == U_INSERT ) {
	ExchangePointMark ( text);
	RestoreMark (text);
#ifdef U_TRACE
	(void) fprintf ( stderr, "Do_UAddChars : restore mark\n" );
#endif /* U_TRACE */
    }
    TextCursorOn ( text );
    ClipOff ( text );
    current_u -> op = U_ADD;
}

/*
**	Function name : Do_UDelChars
**
**	Description : Undo : efface plusieurs caracteres.
**
**	Input : Le Text courant.
**	Ouput :
*/
static void Do_UDelChars ( text )
    Text *text;
{
    int lines;
    Undo *current_u = GetCurrentUndo ( text );	
#ifdef U_TRACE
    (void) fprintf ( stderr, "Do_UDelChars\n" );
#endif /* U_TRACE */
    
    TextCursorOff ( text );
    ClipOn ( text, 0 );
    
    MoveHole ( text -> buf, current_u -> pos );
    lines = GetNewLine (current_u -> buf, current_u -> size );
    (void) MoveHole ( text -> buf, current_u -> size );
    DeleteNchar ( text -> buf, current_u -> size);

    SetTextModif ( text );
    if ( lines == 0 ) {
#ifdef U_TRACE
	(void) fprintf ( stderr, "Do_UDelChars : lines (null)\n" );
#endif /* U_TRACE */
	ClipOn ( text, 0 );
	SetCurrentLine ( text );
    }
    else {
#ifdef U_TRACE
	(void) fprintf ( stderr, "Do_UDelChars : lines = %d\n", lines );
#endif /* U_TRACE */
	text -> lines_in_buf = GetNumberOfLineInBuf ( text -> buf );
	SetScrollLine ( text -> swin , text -> lines_in_buf );
	ShowScrollFrame ( dpy, text -> swin );
	SetLinesTable ( text );
	RefreshPage ( text );
	(void) MoveScrollBar ( dpy, text -> swin, 
			      CURRENT, text -> no_current_line - text -> n1 - 1 );
	RefreshScrollBar ( dpy, text -> swin );
    }
    /* Pour la couleur */
    if ( text -> current_ce != 0 ) {
      (void) UpdateColorList ( text, - current_u -> size );
      ClipOn ( text, 0 );
      RefreshPage ( text );
    }

    TextCursorOn ( text );
    ClipOff ( text );
    current_u -> op = U_DEL;
}

/*
**	Function name : Do_UDelOneChar
**
**	Description : Undo : efface un caractere.
**
**	Input : Le Text courant.
**	Ouput :
*/
static void Do_UDelOneChar ( text )
    Text *text;
{
    Undo *current_u = GetCurrentUndo ( text );
#ifdef U_TRACE
    (void) fprintf ( stderr, "Do_UDelOneChar\n" );
#endif /* U_TRACE */
    
    if ((current_u -> type == U_INSERT)
	&& (current_u -> c == '\n')) {
	StorePosition(text);
	GotoLineNumber ( text ,  current_u -> current_line + 1);
	SetPosition(text);
	UpdatePage(text);
#ifdef U_TRACE
	(void) fprintf ( stderr, "Do_UDelOneChar : U_INSERT\n" );
#endif /* U_TRACE */
    }
    else {
	MoveHole ( text -> buf, current_u -> pos );
#ifdef U_TRACE
	(void) fprintf ( stderr, "Do_UDelOneChar : type not U_INSERT\n" );
#endif /* U_TRACE */
    }
    text -> u_todo = False;
    f_delete ( text );
    text -> u_todo = True;
    /*
       * Attention si on fait undo sur un \n add, il faut mettre a jour
       * le numero de ligne ainsi que la position pour le redo eventuel.
    */
    if ( (current_u -> c == '\n')
	&& (current_u -> type != U_INSERT) ) {
	
#ifdef U_TRACE
	(void) fprintf ( stderr, "Do_UDelOneChar : c : return\n" );
#endif /* U_TRACE */
	
	current_u -> current_line --;
	current_u -> pos = 	
	  1 + GetNcFromLeft ( text -> buf );
    }
    current_u -> op = U_DEL;
}

/*
**	Function name : Do_UAddOneChar
**
**	Description : Undo : ajoute un caractere.
**
**	Input : Le Text courant.
**	Ouput :
*/
static void Do_UAddOneChar ( text )
    Text *text;
{
    Undo *current_u = GetCurrentUndo ( text );
#ifdef U_TRACE
    (void) fprintf ( stderr, "Do_UAddOneChar\n" );
#endif /* U_TRACE */
    
    MoveHole ( text -> buf, current_u -> pos -1);
    switch ( current_u -> c ) {
    case '\n':
#ifdef U_TRACE
      (void) fprintf ( stderr, "Do_UAddOneChar : case Return\n" );
#endif /* U_TRACE */
      text -> u_todo = False;
      f_return ( text );
      text -> u_todo = True;
      /*
	 * Attention si on fait undo sur un \n del, il faut mettre a jour
	 * le numero de ligne ainsi que la position pour le redo eventuel.
      */
      if ( current_u -> type == U_INSERT ) {
#ifdef U_TRACE
	  (void) fprintf ( stderr, "Do_UAddOneChar : Return, U_INSERT \n" );
#endif /* U_TRACE */
	  StorePosition(text);
	  GotoLineNumber ( text ,  current_u -> current_line);
	  MoveToEline ( text );
	  SetPosition(text);
	  UpdatePage(text);
      }
      else {
	  current_u -> current_line ++;
	  current_u -> pos = 	0;
#ifdef U_TRACE
	  (void) fprintf ( stderr, "Do_UAddOneChar : Return, other type\n" );
#endif /* U_TRACE */
      }
      
      break;
    case '\t':
#ifdef U_TRACE
      (void) fprintf ( stderr, "Do_UAddOneChar : Tab\n" );
#endif /* U_TRACE */
      text -> u_todo = False;
      f_tab ( text );
      text -> u_todo = True;
      if ( current_u -> type == U_INSERT ) {
	  TextCursorOff ( text );
	  BackwardChar ( text );
	  TextCursorOn ( text );
#ifdef U_TRACE
	  (void) fprintf ( stderr, "Do_UAddOneChar : Tab,  U_INSERT\n" );
#endif /* U_TRACE */
      }
      break;
    default:
#ifdef U_TRACE
      (void) fprintf ( stderr, "Do_UAddOneChar : Default\n" );
#endif /* U_TRACE */
      
      text -> u_todo = False;
      f_impc ( text, (int) current_u -> c );
      text -> u_todo = True;
      break;
    }
    current_u -> op = U_ADD;
}


/*
**	Function name : DoUndoUndo
**
**	Description : Comme son nom l'indique.
**
**	Input : Le Text courant.
**	Ouput :
*/
void DoUndoUndo ( text )
    Text *text;
{

#ifdef U_TRACE
    (void) fprintf ( stderr, "DoUndoUndo\n" );
#endif /* U_TRACE */
    
    if ( text -> u_index == 0 ) {
#ifdef U_TRACE
	(void) fprintf ( stderr, "DoUndoUndo : index (null)\n" );
#endif /* U_TRACE */
	
	klaxon ();
	return;
    }
    text -> u_index --;
    flag_redo = True;
    DoUndo ( text );
    flag_redo = False;
    text -> u_index --;
}

