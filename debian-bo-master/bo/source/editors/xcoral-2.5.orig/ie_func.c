/* ########################################################################

			       ie_func.c

   File: ie_func.c
   Path: /home/fournigault/c/X11/xcoral-2.31/ie_func.c
   Description: 
   Created: Fri Jan 27 11:07:28 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:07:29 MET 1995
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
#include <stdlib.h>
#include <string.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <sys/param.h>

#include "options.h"
#include "xcoral.h"
#include "bm_search.h"
#include "ie_func.h"
#include "page.h"
#include "text_cursor.h"
#include "new_window.h"
#include "fs_box.h"
#include "main_events.h"
#include "process.h"
#include "browser_init.h"
#include "get_file.h"
#include "mark_reg.h"
#include "chars_cmds.h"
#include "warn_box.h"

FCT (static void, MatchedAt, (Text *text, int c, int lines, int pos) );

/*
**	Function name : ie_current_position
**
**	Description : 
**	Input : 
**	Ouput : Le numero de la position courante (debut = 0)
*/
int  ie_current_position ( text ) 
    Text *text;
{
    return ( text -> buf -> l_cur - text -> buf -> top );
}

/*
**	Function name : ie_current_char
**
**	Description : 
**	Input : 
**	Ouput : Le caractere courant ou 0 si fin de fichier
*/
char ie_current_char ( text )
    Text *text;
{
    return ( ( (text -> buf -> r_cur) == text -> buf -> bottom ) ?
	    '\0' : *(text ->buf -> r_cur+1));
}

/*
**	Function name : ie_previous_char
**
**	Description : 
**	Input : 
**	Ouput : Le caractere precedent, 0 si debut de buffer.
*/
char ie_previous_char ( text )
    Text *text;
{
    return ( ( text -> buf -> l_cur <= text -> buf -> top ) ?
	    '\0' : *( text -> buf -> l_cur -1 ));
}

/*
**	Function name : ie_next_char
**
**	Description : 
**	Input : 
**	Ouput : le caractere suivant, 0 en fin de fichier
*/
char ie_next_char ( text )
    Text *text;
{
    return ( ( text -> buf -> r_cur + 1) == text -> buf -> bottom ) ?
      '\0' : *( text -> buf -> r_cur+2);
}

/*
**	Function name : ie_the_char
**
**	Description : 
**	Input : 
**	Ouput : le caractere correspondant a la position, 0 si erreur
*/
char ie_the_char (text, p)
    Text *text;
    int p;
{
    /* La longueur utile (sans la bulle) du buffer. */
    int b_len = ((text -> buf -> l_cur - text -> buf -> top) +
		 (text -> buf -> bottom - text -> buf -> r_cur));
    
    if ( (p >= 0) && ( p <= b_len )) {
	if ( p > (text -> buf -> l_cur - text -> buf -> top - 1)) {
	    /* Apres la bulle */
	    p -= text -> buf -> l_cur - text -> buf -> top;
	    return ( *( text -> buf -> r_cur + 1 + p));
	}
	else {
	    /* Avant la bulle */ 
	    return ( *(text -> buf -> top + p ) );
	}
    }
    else
      return '\0';
}

/*
**	Function name : ie_beginning_of_line
**
**	Description : 
**	Input : 
**	Ouput : la position du debut de ligne
*/
int ie_beginning_of_line( text )
    Text *text;
{
    int pos = ie_current_position ( text );
    
    if (pos)
      pos -= GetNcFromLeft ( text -> buf );
    
    return (pos);
/*    
    return ( (ie_current_position ( text ) == 0) ? 0 :
	    ie_current_position ( text ) - GetNcFromLeft ( text -> buf ));
*/	    
}

/*
**	Function name : ie_end_of_line
**
**	Description : 
**	Input :  
**	Ouput : la position de la fin de ligne
**         Position du return ou fin de fichier.
*/
int ie_end_of_line ( text )
    Text *text;
{
    if ( ie_current_char ( text ) == '\n' )
      return ( ie_current_position ( text ));
    else {
	return ( ie_current_position ( text ) 
		+ GetNcFromRight ( text -> buf ));
    }
}

/*
**	Function name : ie_at_end_of_file
**
**	Description : 
**	Input : 
**	Ouput :  True si fin de fichier.
*/
int ie_at_end_of_file ( text )
    Text *text;
{
    if ( ie_current_char ( text ) == 0 )
      return True;
    else
      return False;
}

/*
**	Function name : ie_end_of_file
**
**	Description : 
**	Input : 
**	Ouput :  la position de la fin de fichier
*/
int ie_end_of_file ( text )
    Text *text;
{
    return ( ( text -> buf -> l_cur - text -> buf -> top ) +
	    ( text -> buf -> bottom - text -> buf -> r_cur ));
}

/*
**	Function name : ie_goto_previous_char
**
**	Description : recule d'un caractere si possible
**	Input : 
**	Ouput :  
*/
void ie_goto_previous_char ( text )
    Text *text;
{
    char c;
    
    if ( ie_current_position ( text ) == 0 )
      return;
    (void) MoveHole ( text -> buf, -1 );	
    GetCurrentChar ( text -> buf, &c );
    if ( c == '\n' ) {
	text -> no_current_line --;
    }
    text -> ie_mark = True;
}

/*
**	Function name : ie_goto_next_char
**
**	Description :  avance d'un caractere si possible
**	Input : 
**	Ouput :
*/
void ie_goto_next_char ( text )
    Text *text;
{
    char c;
    
    if ( ie_at_end_of_file ( text ) == True )
      return;
    GetCurrentChar ( text -> buf, &c );
    if ( c == '\n' ) {
	if ( text -> no_current_line > text -> lines_in_buf )
	  return;
	text -> no_current_line ++;
    }
    (void) MoveHole ( text -> buf, 1 );
    text -> ie_mark = True;
}

/*
**	Function name : ie_goto_char
**
**	Description :  change de position si possible
**	Input : 
**	Ouput :
*/
void ie_goto_char ( text, p )
    Text *text;
    int p;
{
    int compteur = 0;
    int nb_lines = 0;
    int cur_pos = ie_current_position ( text );
    
    /* On vire d'abord les cas idiots. */
    if ( (p < 0) || (p > ie_end_of_file (text)) || (p == cur_pos))
      return;
    /*
       * De combien il faut deplacer la bulle et dans quelle
       * direction. De plus il faut savoir ou on en est au niveau
       * du numero de ligne.
    */
    if ( p < cur_pos ) {
	/* on va a gauche */
	char * left = text->buf->l_cur - 1;
	
	while (  compteur != (cur_pos - p)) {
	    if ( * ((char *) (left - compteur))  == '\n' ) {
		nb_lines ++;
	    }
	    compteur ++;
	}
	text -> no_current_line -= nb_lines;
    }
    else { 
	/* On va a droite du curseur */
	char *right = text -> buf -> r_cur + 1;
	
	while ( compteur < (p - cur_pos)) {
	    if ( * ((char *) (right + compteur))  == '\n' )
	      nb_lines ++;
	    compteur++ ;
	}
	text -> no_current_line += nb_lines;
    }
    MoveHole ( text -> buf, p - cur_pos );
    text -> ie_mark = True;
}

/*
**	Function name : ie_goto_beginning_of_line
**
**	Description : va en debut de ligne
**	Input : 
**	Ouput :
*/
void ie_goto_beginning_of_line ( text ) 
    Text *text;
{
    if ( text -> sl )
      text -> sl = 0;
    (void) MoveHole ( text -> buf, - GetNcFromLeft ( text -> buf ));
    text -> ie_mark = True;
}

/*
**	Function name : ie_goto_end_of_line
**
**	Description : va en fin de ligne
**	Input : 
**	Ouput :
*/
void ie_goto_end_of_line ( text ) 
    Text *text;
{
    MoveToEline ( text );
    text -> ie_mark = True;
}

/*
**	Function name : ie_goto_end_of_file
**
**	Description :  va a la fin du fichier
**	Input : 
**	Ouput :
*/
void ie_goto_end_of_file ( text )
    Text *text;
{
    if ( ie_at_end_of_file ( text ) == True )
      return;
    HoleToRight ( text -> buf );
    GotoLineNumber ( text, text -> lines_in_buf );
    text -> ie_mark = True;
    /*	GotoEnd ( text ); */
}

/*
**	Function name : ie_insert_char
**
**	Description : insere et avance
**	Input : 
**	Ouput :
*/
void ie_insert_char ( text, c )
    Text *text;
    int c;
{
    char the_c = (char) c;
    
    if ( text -> shell_id != 0) {
	if ((char) c == '\n') {
	    shell_return ( text );
	    return;
	}
	else {
	    SetPosInShell ( text );
	    SetTextModif ( text );
	}
    }
    if ( (char) c != '\n' )
      UpdateMark ( text, 1, 0 );
    else 
      UpdateMark ( text, 1, 1 );
    
    InsertNchar ( text -> buf, &the_c, 1 );
    
    if ( (char) c == '\n' ) {
	text ->  no_current_line ++;
	text -> lines_in_buf ++;
    }
    
    if ( text -> u_todo == True ) {
	(void) StoreInUndo ( text, &the_c, (char *) 0, 1, 0, U_STD );
	/* Pour la couleur */
	if ( text -> current_ce != 0 )
	  (void) UpdateColorList ( text, 1);
    }
    SetTextModif ( text);
    text -> ie_mark = True;
}

/*
**	Function name : ie_delete_char
**
**	Description :  detruit caractere (Controle D)
**	Input : 
**	Ouput :
*/
void ie_delete_char (text )
    Text *text;
{
    char c;
    int mark = 0;;
    
    if ( ie_at_end_of_file ( text ) == True )
      return;
    
    if ( ( c = ie_current_char ( text )) == '\n' ) {
	mark = UpdateMark ( text, -1, -1 );
	text -> no_current_line--;
	text -> lines_in_buf --;
    }
    else
      UpdateMark ( text, -1, 0 );
    
    ie_goto_next_char ( text );
    DeleteNchar ( text -> buf, 1 );
    
    if ( mark ) 
      UpdateMarkPos ( text );
    
    if ( text -> u_todo == True ) {
	(void) StoreInUndo ( text, &c, (char *) 0, -1, 0, U_STD );
	/* Pour la couleur */
	if ( text -> current_ce != 0 )
	  (void) UpdateColorList ( text, -1 );
    }
    SetTextModif ( text);
    text -> ie_mark = True;
}

/*
**	Function name : ie_replace_char
**
**	Description :  remplace sans avancer
**	Input : 
**	Ouput :
*/
void ie_replace_char ( text, c )
    Text *text;
    int c;
{
    if ( ie_at_end_of_file ( text ) == True ) {
	ie_insert_char ( text, c );
	ie_goto_previous_char ( text );
    }
    else {
	ie_insert_char ( text, c );
	ie_delete_char ( text );
	ie_goto_previous_char  (text);
    }
    SetTextModif ( text);
}

/*
**      Function name : ie_end_of_region
**
**      Description : 
**      Input : 
**      Ouput :
*/
int ie_end_of_region ( text )
    Text *text;
{
    return ( ie_current_position (text) -1);
}

/*
**	Function name : ie_kill_current_buffer
**
**	Description : 
**	Input : 
**	Ouput :
*/
void ie_kill_current_buffer ( text )
    Text *text;
{
    extern Display *dpy;
    
    KillText ( dpy, text );
    RefreshScroll ( dpy,  text -> swin, 
		   text -> width + GetScrollWidth () + W_SPACE + 1, text -> height, 0 );
    SetTextSave ( text );
    SetDirAndFilename ( text, "NoName" );
    XSetIconName ( dpy,  text ->  w_parent, "NoName" ); /* ca manquait */
    XStoreName ( dpy, text ->  w_parent, "untitled" ); 
/*    XStoreName ( dpy, text ->  w_parent, "NoName" ); */
    ResetMark ( text );
/*    text -> ie_mark = True; */ /* Pas necessaire */
}

/*
**	Function name : ie_mark_position
**
**	Description : 
**	Input : 
**	Ouput :
*/
int ie_mark_position ( text )
    Text *text;
{
    int pos, old_pos, old_line;
    
    if ( text -> markline == 0 ) /* Pas de marque */
      return -1;
    
    old_line = 	text -> no_current_line;
    old_pos = GetNcFromLeft ( text -> buf );
    
    if ( old_line != text -> markline ) {
	GotoLineNumber ( text, text -> markline );		
	if ( text -> markpos != 0 )
	  (void) MoveHole ( text -> buf, text -> markpos );
    }
    else {
	(void) MoveHole ( text -> buf,  text -> markpos - old_pos );
    }
    pos = ie_current_position ( text );
    
    if ( text -> no_current_line != old_line ) {
	GotoLineNumber ( text, old_line );		
	if ( old_pos != 0 )
	  (void) MoveHole ( text -> buf, old_pos );
    }
    else {
	(void) MoveHole ( text -> buf, old_pos - text -> markpos );
    }
    return pos;	
}

/*
**	Function name : ie_redisplay
**
**	Description : 
**	Input : 
**	Ouput :
*/
void ie_redisplay ( text )
    Text *text;
{
    extern Display *dpy;
    EdWin **t;
    
    for ( t = TWin; t < TWin + MAXWIN; t++ ) {
	if ( *t == 0 ) continue;
	if ( (( *t ) -> text -> ie_mark == True) || (( *t ) -> text == text) ) {
	    SetPosition ( ( *t ) -> text );
	    UpdatePage ( ( *t ) -> text );
	    ( *t ) -> text -> ie_mark = False;
	    XSync ( dpy, False );
	    SmallTime ( (long) 40000  );
	}
    }
}


/*
**	Function name : ie_blink
**
**	Description : 
**	Input : 
**	Ouput :
*/
void ie_blink ( text, pos )
    Text *text;
    int pos;
{
    int diff;
    int lines;
    extern Display *dpy;
    char c = ie_the_char ( text, pos );
    int current_pos = ie_current_position (text);
    
    TextCursorOn ( text );
    TextCursorOff ( text );
    
    if ( pos < 0 ) {
	ClearMessageWindow ( text -> mwin );
	DisplayMessage ( text -> mwin, "No Match" );
	return;
    }
    
    /* Est-ce qu'on sort de la page */
    if ( current_pos > pos ) {
	if ( (lines = GetNewLine ( text -> buf -> top + pos, current_pos - pos )) > text -> n1 ) {
	  MatchedAt ( text, (int) c, lines, pos );
	  return;
	}
	diff = -lines;
    }
    else {
	if ( (lines = GetNewLine ( text -> buf -> r_cur + 1, pos - current_pos )) > text -> n2 ) {
	  MatchedAt ( text, (int) c, lines, pos );
	  return;
	}
	diff = lines;
    }
    
    /* On va  a la position pos dans la page courante. */
    text -> no_current_line += diff;
    text -> n1 += diff;
    text -> n2 -= diff;
    (void) MoveHole ( text -> buf,  -(current_pos - pos) );
    SetAndDisplayPage ( text );
    
    /* On affiche le curseur pendant 400 ms puis on eteint */
    TextCursorOn ( text );
    XSync ( dpy, False );
    SmallTime ( (long) 400000  );
    TextCursorOff ( text );
    
    /* On revient a la position courante */
    text -> no_current_line -= diff;
    text -> n1 -= diff;
    text -> n2 += diff;
    (void) MoveHole ( text -> buf,  (current_pos - pos) );
    SetAndDisplayPage ( text );
}


/*
**	Function name : ie_cmd_shell
**
**	Description : 
**	Input : 
**	Ouput :
*/
int ie_cmd_shell ( text, str )
    Text *text;
    char *str;
{
#define BIG_LINE 2048
    
    FILE *fd;
    char tmp[BIG_LINE];
    int len;
    int modif_flag = False;
    
    if ( (fd = popen ( str,  "r")) == NULL ) {
	(void) fprintf ( stderr, "popen error\n" );
	return (-1);
    }

    while (fgets ( tmp,BIG_LINE, fd ) != 0 ) {
	len = strlen(tmp);
	modif_flag = True;
	UpdateMark ( text, len, 1 );
	InsertNchar ( text -> buf, tmp, len );
	if ( tmp[len-1] == '\n' ) {
	    text -> no_current_line ++;
	    text -> lines_in_buf ++;
	}
	(void) StoreInUndo ( text, tmp, (char *) 0, len ,1, U_STD );
	/* Pour la couleur */
	if ( text -> current_ce != 0 )
	  (void) UpdateColorList ( text, len );
    }
    if ( modif_flag )
      SetTextModif ( text );
    (void) pclose  (fd);
    return 0;
}

static void MatchedAt ( text, c, lines, pos )
    Text *text;
    int c;
    int lines;
    int pos;
{
    char tmp [64];
    
    ClearMessageWindow ( text -> mwin );
    (void) sprintf ( tmp, "Match '%c' at line %d pos %d", (char) c,
		    text -> no_current_line - lines, pos );
    DisplayMessage ( text -> mwin, tmp );
    return;
}

/*
**	Function name : ie_msearch
**
**	Description : 
**	Input : 
**	Ouput :
*/
int ie_msearch (text, chars, end, direction)
    Text * text;
    char * chars;
    int end;
    int direction;
{
    if (direction >= 0) {
	if (end > ie_end_of_file(text))
	  end = ie_end_of_file(text);
	while (ie_current_position(text) < end)
	  if (strchr(chars, ie_current_char(text)))
	    return 1;
	else
	  (void) ie_goto_next_char(text);
    }
    else {
	if (end < 0)
	  end = 0;
	while (ie_current_position(text) > end) {
	    (void) ie_goto_previous_char(text);
	    if (strchr(chars, ie_current_char(text)))
	      return 1;
	}
    }
    return 0;
}


/*
**	Function name : ie_create_mode
**
**	Description : 
**	Input : 
**	Ouput : Le nouveau mode sinon 0.
*/
Mode *ie_create_mode ( text, mode_name )
    Text *text;
    char *mode_name;
{
    if ( mode_name == 0 )
      return 0;
    else
      return ( (Mode *) CreateNewMode ( mode_name ));
}


/*
**	Function name : ie_set_mode_suffixes
**
**	Description : 
**	Input : 
**	Ouput :
*/
void ie_set_mode_suffixes ( text, mode_name, suf )
    Text *text;
    char *mode_name;
    char *suf;
{
    Mode *mode;
    
    if ( (mode_name == 0) || (suf == 0))
      return;
    
    if ( (mode = (Mode*) GetMode ( mode_name )) == 0 )
      return;
    
    SetModeSuffixe ( mode, suf );
}


/*
**	Function name : ie_set_mode_font
**
**	Description : 
**	Input : 
**	Ouput :
*/
void ie_set_mode_font ( text, mode_name, font_name )
    Text *text;
    char *mode_name;
    char *font_name;
{
    Mode *mode;

    if ( (mode_name == 0) || (font_name == 0) )
      return;
    
    if ( (mode = (Mode*) GetMode ( mode_name )) == 0 )
      return;
     
    SetModeFont ( mode, font_name );
}

/*
**	Function name : ie_key_def
**
**	Description : Pour les liasons de cles.
**             Pas triste la fonction.
**	Input : Le text courant, le mode, la liaison de cles
**         la fonction associee.
**	Ouput :
*/

int key_code( keys )
    char *keys;
{
  if (keys[0] == '^')
    switch (keys[1]) {
    case '[' :
      return 27;
    case ' ' :
    case '@' :
      return 0;
    case '^' :
      return '^';
    default:
      if ((keys[1] >= 'a') && (keys[1] <= 'z'))
	return keys[1] - 'a' + 1;
      if ((keys[1] >= 'A') && (keys[1] <= 'Z'))
	return keys[1] - 'A' + 1;
      return -1;
  }
  {
    int xk = Xk_Code(keys);
    
    return (xk) ? xk : keys[0];
  }
}

void ie_key_def ( text, mode_name, keys, func_name )
	Text *text;
	char *mode_name, *keys, *func_name;
{
    Mode *mode;
    int c;
    void (* f) ();

    if ( (mode_name == 0) || (keys == 0) )
      return;
    if ( (mode = (Mode*) GetMode ( mode_name )) == 0 )
      return;
    
    /* 
	keys : 1, 2, 3 ou 4 caracteres
	c pour un caractere simple;
	^ pour controle
	^[ pour escape
	^^ pour ^

	Les sequences possibles sont:
	
	c     --> Key (XK possible)
	^[c   --> Escape key. (XK possible)
	^[^[  --> Escape, Escape.
	^[^c  --> Escape, Controle key.
	^c    --> Controle key.
	^xc   --> Controle X, key (XK possible).
	^x^[  --> Controle X, Escape
	^x^c  --> Controle X, Controle key.
    */

    if ( keys [0] == '^' ) {
	if ( strlen (keys) < 2 )
	  return;
	switch ( keys [1] ) {
	case '[':
	  /* Escape .. */
	  if ( strlen (keys) < 3 )
	    return;
	  if ((c = key_code(&keys[2])) == -1)
	    return;
	  /* La fonction est-elle built_in ou extern ? */
	  if ( func_name == 0 ) {
	    mode -> esc_key [c].type = 0;
	    mode -> esc_key [c].func = 0;
	    break;
	  }
	  else if ( ( f = (void (*) ()) GetFuncNameFromString ( func_name )) == 0 ) {
	      mode -> esc_key [c].type = EXTERN_FUNC;
	      mode -> esc_key [c].func = (void (*) ()) func_name;
	  }
	  else {
	      mode -> esc_key [c].type = BUILTIN_FUNC;
	      mode -> esc_key [c].func = (void (*) ()) f;
	  }
	  break;
	case 'X':
	case 'x':
	  /* Controle X .. */
	  if ( strlen (keys) < 3 )
	    return;
	  if ((c = key_code(&keys[2])) == -1)
	    return;
	  /* La fonction est-elle built_in ou extern ? */
	  if ( func_name == 0 ) {
	    mode -> ctrX_key [c].type = 0;
	    mode -> ctrX_key [c].func = 0;
	    break;
	  }
	  else if ( ( f = (void (*) ()) GetFuncNameFromString ( func_name )) == 0 ) {
	      mode -> ctrX_key [c].type = EXTERN_FUNC;
	      mode -> ctrX_key [c].func = (void (*) ()) func_name;
	  }
	  else {
	      mode -> ctrX_key [c].type = BUILTIN_FUNC;
	      mode -> ctrX_key [c].func = (void (*) ()) f;
	  }
	  break;
	default:
	  /* ------Controle Key------ */
	  if ( strlen ( keys ) != 2 )
    	    return;
	  if ((c = key_code(&keys[0])) == -1)
	    return;
	  	  
	  /* La fonction est-elle built_in ou extern */
	  if ( func_name == 0 ) {
	    mode -> key [c].type = 0;
	    mode -> key [c].func = 0;
	    break;
	  }
	  else if ( ( f = (void (*) ()) GetFuncNameFromString ( func_name )) == 0 ) {
	      mode -> key [c].type = EXTERN_FUNC;
	      mode -> key [c].func = (void (*) ()) func_name;
	  }
	  else {
	      mode -> key [c].type = BUILTIN_FUNC;
	      mode -> key [c].func = (void (*) ()) f;
	  }
	  break;
	}
    }
    else {
	/* ------Caractere simple------ */
	if ( strlen ( keys ) != 1 ) {
	  if ((c = Xk_Code(keys)) == 0)
	    return;
	}
	else
	  c = (int) keys [0];
	
	/* La fonction est-elle built_in ou extern ? */
	if ( func_name == 0 ) {
	  mode -> key [c].type = 0;
	  mode -> key [c].func = 0;
	  return;
	}
	else if ( ( f = (void (*) ()) GetFuncNameFromString ( func_name )) == 0 ) {
	    mode -> key [c].type = EXTERN_FUNC;
	    mode -> key [c].func = (void (*) ()) func_name;
	}
	else {
	    mode -> key [c].type = BUILTIN_FUNC;
	    mode -> key [c].func = (void (*) ()) f;
	}
    }
}
    
int ie_last_key (text)
    Text *text;
{
    extern int last_key;
 
    if ( text != 0 )
      return (last_key);
    else
      return 0;
}

/*
**	Function name : ie_current_line_to_top
**
**	Description : 
**	Input : 
**	Ouput :
*/
void ie_current_line_to_top ( text )
    Text *text;
{
    text -> n1 = 0;
    text -> n2 = text -> lines_in_page - 1;
    StorePosition ( text );
    text -> ie_mark = True;
}

/*
**	Function name : ie_line_count
**
**	Description : 
**	Input : 
**	Ouput :
*/
int ie_line_count ( text )
    Text *text;
{
    return ( text -> lines_in_buf );
}


/*
**	Function name : ie_current_line
**
**	Description : 
**	Input : 
**	Ouput :
*/
int ie_current_line ( text )
    Text *text;
{
    return ( text -> no_current_line - 1 );
}

/*
**	Function name : ie_backward_search
**
**	Description : 
**	Input : 
**	Ouput :
*/
int ie_backward_search ( text, str )
    Text *text;
    char *str;
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
	    text -> ie_mark = True;
	    return True;
	}
    }
    start = (char *) LeftBuf ( text -> buf ); /* Allons voir plus loin */
    
    if ( (newpos=BmSearch ( start, str,  start - end, BACKWARD )) == -1 )
      return False;
    
    ie_goto_char ( text, ie_current_position ( text ) - newpos );

    text -> ie_mark = True;
    return True;
}

/*
**	Function name : ie_forward_search
**
**	Description : 
**	Input : 
**	Ouput :
*/
int ie_forward_search ( text, str )
    Text *text;
    char *str;
{
    int newpos;
    char *start, *end;
    
    start = (char *) RightBuf ( text -> buf ); 
    end = (char *) BottomBuf ( text -> buf );

    if ( (newpos=BmSearch( start, str,  end -start, FORWARD )) != -1 ) {
	if ( newpos == 0 ) { 	/* On est pile dessus */
	    (void) MoveHole ( text -> buf, 1 );	/* Voyons plus loin */
	    if ( ie_at_end_of_file(text)) {
		(void) MoveHole ( text -> buf, -1 );
		return False;
	    }
	    start ++;
	    if ( (newpos=BmSearch( start, str,  end - start, FORWARD )) == -1 ) {
		(void) MoveHole ( text -> buf, -1 );
		return False;
	    }
	}
	ie_goto_char ( text, ie_current_position ( text ) + newpos );
	text -> ie_mark = True;
	return True;
    }
    return False;
}

/*
**	Function name : ie_global_replace
**
**	Description : 
**	Input : 
**	Ouput : Le nombre de subtitutions.
*/
int ie_global_replace ( text, old, new )
    Text *text;
    char *old, *new;
{
    int i = 0;
    
    if ( (old != 0) && (new != 0) && (strlen(old) != 0)) {
      if (strchr(old, '\n') || strchr(new, '\n')) {
	/* galere */
	int pos = ie_current_position(text);
	int lgo = strlen(old);
	
	while (ie_forward_search(text, old)) {
	  int i;
	  char * n;
	  
	  for (i = 0; i != lgo; i += 1)
	    ie_delete_char(text);
	  for (n = new; *n; n += 1)
	    ie_insert_char(text, *n);
	}
	
	ie_goto_char(text, pos);
      }
      else {
	/* HandleGlobal sait le faire */
	text -> ie_mark = True;
	SetPatterns ( old, new );
	i = HandleGlobal ( text );
	ResetPatterns ();
      }
    }
    return i;
}

/*
**	Function name : ie_read_file
**
**	Description : 
**	Input : 
**	Ouput :
*/
int ie_read_file ( text, filename )
    Text *text;
    char *filename;
{
    extern Display *dpy;
    Text *result;

    ie_kill_current_buffer ( text );

    TextCursorOff ( text );
    if ( LoadFile ( text, filename, NEW ) != -1 ) {
	ShowScrollFrame ( dpy, text -> swin );
	FirstPage ( text );
	SetTextSave ( text );
	if ( IsAlreadyLoad ( text -> filename, text, &result ) > 1 ) {
	  XBell ( dpy, 10 );
	  KillBuffer ( text );
	  XMapRaised ( dpy, result -> w_parent );
	  TextCursorOn ( text );
	  text -> ie_mark = True;
	  return -1;
	}
	TextCursorOn ( text );
	text -> ie_mark = True;
	return 1;
    }
    else {
	TextCursorOn ( text );
	return 0;
    }
}

/*
**	Function name : ie_insert_file
**
**	Description : 
**	Input : 
**	Ouput :
*/
int ie_insert_file ( text, filename )
    Text *text;
    char *filename;
{
    int nbl, olen;
    extern Display *dpy;
    int x_len;
    
    TextCursorOff ( text );
    nbl = text -> lines_in_buf;
    olen = BottomBuf (text -> buf) - RightBuf (text -> buf);
    
    if ( LoadFile ( text, filename,  INSERT ) != -1 ) {
	SetTextModif ( text );		
	SetAndDisplayPage ( text ); 
	ShowScrollFrame ( dpy, text -> swin );
	(void) MoveScrollBar ( dpy, text -> swin, 
			      CURRENT, text -> no_current_line - text -> n1 - 1 );
	x_len = (BottomBuf (text -> buf) - RightBuf (text -> buf)) - olen;
	StoreInUndo ( text, RightBuf (text -> buf) , (char *) 0, x_len,
		     text -> lines_in_buf - nbl, U_INSERT );
    
	/* Pour la couleur */
	if ( text -> current_ce != 0 )
	  (void) UpdateColorList ( text, x_len );
	
	TextCursorOn ( text );
	text -> ie_mark = True;
	return 1;
    }
    else {
	TextCursorOn ( text );
	return 0;
    }
}


/*
**	Function name : ie_file_select
**
**	Description : 
**	Input : 
**	Ouput :
*/
char *ie_file_select ( text )
    Text *text;
{
    char *str;
    char c = '\007'; /* ^G */
    
    str = (char *) SelectFileFromBox ( "Select file from Smac." );
    if ( (str == 0) || (strncmp(str, &c, 1)) == 0 ) {
	if ( str != 0 )
	  (void) free ( str );
	return 0;
    }
    else {
	if ( str != 0 )
	  (void) free ( str );
	return str;
    }
}

/*
**	Function name : ie_write_file
**
**	Description : 
**	Input : 
**	Ouput :
*/
void ie_write_file ( text, filename )
    Text *text;
    char *filename;
{
    extern int write_flag;

    if ( (filename != 0) && (strlen (filename) != 0)){
	if ( *filename == '/' ) {
	    (void) strcpy ( text -> filename, filename );
	}
	else {
	    (void) strcpy ( text -> filename,  text -> current_dir );
	    (void) strcat ( text -> filename, "/" );
	    (void) strcat ( text -> filename, filename );
	}
	
	WriteFile ( text );
	if ( write_flag ) {
	  SetDirAndFilename ( text, filename );
	  (void) SetWindowName ( text );
	}
    }
}

/*
**	Function name : ie_save_file
**
**	Description : 
**	Input : 
**	Ouput :
*/
void ie_save_file ( text )
    Text *text;
{
    char *tmp;
    
    if (text -> filename) {
      tmp = (char *) malloc(MAXPATHLEN+2);
      (void) strcpy(tmp, text -> filename);
    }
    else
      return;
    
    (void) ie_write_file ( text, tmp );
    (void) free(tmp);
}

/*
**	Function name : ie_current_buffer_is_modified
**
**	Description : 
**	Input : 
**	Ouput :
*/
int ie_current_buffer_is_modified ( text )
    Text *text;
{
  return GetModif( text );
}


/*
**	Function name : ie_filename
**
**	Description : 
**	Input : 
**	Ouput :
*/
char *ie_filename ( text )
    Text *text;
{
    if ( strcmp ( text -> filename, "NoName" ) == 0 )
      return 0;
    else
      return ( text -> filename );
}


/*
**	Function name : ie_window_height
**
**	Description : 
**	Input : 
**	Ouput :
*/
int ie_window_height ( text )
    Text *text;
{
    return ( text -> lines_in_page );
}


/*
**	Function name : ie_window_width
**
**	Description : 
**	Input : 
**	Ouput :
*/
int ie_window_width ( text, ch )
    Text *text;
    int ch;
{
    char c = (char) ch;
    
    int width = XTextWidth ( text -> font, &c, 1 );
    
    if (width)
      return ( (text -> width - (2 * text -> x_or)) / width);
    
    return (- ((text -> width - (2 * text -> x_or))));
}

/*
**	Function name : ie_current_window
**
**	Description : 
**	Input : 
**	Ouput :
*/
int ie_current_window ( text )
    Text *text;
{
    return ( text -> win_id );
}

/*
**	Function name : ie_new_window
**
**	Description : 
**	Input : 
**	Ouput :
*/
int ie_new_window ( text )
    Text *text;
{
    EdWin *tmp;
    
    tmp = (EdWin *) CreateWindow ();
    
    if ( tmp != 0 ) {
	tmp -> text -> current_mode = (Mode *) GetMode ("default");
	tmp -> text -> mwin -> mode = tmp -> text -> current_mode;
	SetBrowserMode ( tmp -> text -> current_mode );
	tmp -> text -> ie_mark = True;
	(void) strcpy ( tmp -> text -> current_dir, text -> current_dir );
	
	WaitForMapped ( tmp -> text -> window, True );
	StorePosition ( tmp -> text );   
	
	return ( tmp -> text -> win_id );
    }
    else {
	(void) fprintf ( stderr, "Too many open window\n" );
	return -1;
    }
}

/*
**	Function name : ie_select_window
**
**	Description : 
**	Input : 
**	Ouput :
*/
Text *ie_select_window ( text, win_id )
    Text *text;
    int win_id;
{
    extern EdWin *TWin[];
    
    if ( (win_id <0) || (win_id > (MAXWIN - 1)))
      return 0;
    if ( TWin [win_id] != 0 ) {
	if ( TWin [win_id] -> text -> ie_mark == False ) {
	    TWin [win_id] -> text -> ie_mark = True;
	    StorePosition ( TWin [win_id] -> text );
	}
	chdir( TWin [win_id] -> text -> current_dir );
	return ( TWin [win_id] -> text );
    }
    return 0;
}

/*
**	Function name : ie_kill_window
**
**	Description : 
**
**	Input : 
**	Ouput : Un numero de fenetre utilisable, -1 sinon.
*/
int ie_kill_window ( text, num )
    Text *text;
    int num;
{
    EdWin **tw;
    int win_id = ie_current_window ( text );

    if ( (num <0) || (num > (MAXWIN - 1)))
      return (-1);
    
/*    WaitForXserver (); */
    
    if ( TWin [num] != 0 ) {
      SetTextSave ( TWin [num] -> text );
      WaitForXserver ();
      DeleteCurrentWindow ( TWin [num] -> text );
      WaitForXserver ();
      /*
	 Si c'etait la derniere fenetre ce qui suit n'a 
	 aucune importance.
      */
      if ( num != win_id )
	return win_id;
      
      /* Num etait celui de la fenetre courantre,
	 il faut retourner un num_id utilisable */
      for ( tw = TWin; tw < TWin + MAXWIN; tw++ ) {
	if ( *tw == 0 )
	  continue;
	else
	  return ((*tw) -> text -> win_id);
      }
    }
    return (-1);
}

/*
**	Function name : ie_lower_window
**
**	Description : 
**	Input : 
**	Ouput :
*/
void ie_lower_window ( text )
    Text *text;
{
    extern Display *dpy;
    int win_id = ie_current_window ( text );
    
    if ( TWin [win_id] != 0 )
      XLowerWindow ( dpy, text -> w_parent );
}

/*
**	Function name : ie_raise_window
**
**	Description : 
**	Input : 
**	Ouput :
*/
void ie_raise_window (text)
    Text *text;
{
    extern Display *dpy;
    
    int win_id = ie_current_window ( text );
    
    if ( TWin [win_id] != 0 )
      XMapRaised ( dpy, text -> w_parent );
}


/*
**	Function name : ie_set_mode
**
**	Description : 
**	Input : 
**	Ouput :
*/
void ie_set_mode ( text, name )
    Text *text;
    char *name;
{
    char *buf;
    Mode *new_mode = (Mode *) GetMode (name);
    
    if ( new_mode == 0 ) {
	buf = (char *) malloc ( (unsigned int) strlen (name) + 64 );
	(void) sprintf ( buf, "Warning : %s mode does not exist", name );
	DisplayMessage ( text -> mwin, buf );
	(void) free (buf);
	return;
    }
    
    if ( strcmp(text -> current_mode -> name, name) == 0 )
      return;
    
    if ( strcmp ( name, "Shell" ) == 0 ) {
	SetShellMode ( text );
	return;
    }
    
    if ( text -> shell_id != 0 )
      KillShell ( text );
    text -> current_mode = new_mode;
    
    SetCtrMode ( text -> mwin, text -> current_mode );
    RefreshWindowMode ( text -> mwin );
    if ( text -> current_mode -> font )
      RefreshWithNewFont ( text, text -> current_mode -> font );
}

/*
**	Function name : ie_set_font
**
**	Description :
**	Input :
**	Output :
*/
void ie_set_font ( text, font_name )
    Text *text;
    char *font_name;
{
    XFontStruct *font;
    
    if ((font = (XFontStruct *) XLoadQueryFont ( dpy, font_name )) == 0 ) {
      (void) DisplayWMessage ( "fontname unknown", "Change current font", True );
      return;
    }
    TextCursorOff ( text );
    (void) RefreshWithNewFont ( text, font );
}


/*
**	Function name : ie_re_search_forward
**
**	Description : Recherche en avant d'une expression reguliere dans
**         le buffer courant.  
**	Input : la RE
**	Output : -1 si failed -2 si error sinon la position du debut
**         de la RE dans le buffer courant.
*/
int ie_re_search_forward ( text, regex )
    Text *text;
    char *regex;
{
    int current_pos = ie_current_position(text);
    
    int result = RE_Search ( regex, RightBuf(text->buf),
			BottomBuf(text->buf) - RightBuf(text->buf) + 1, FORWARD);

    if ( result < 0 )
      return result;
    else {
	text -> ie_mark = True;
	UpdateRegs (current_pos);
	return (result + current_pos);
    }
}


/*
**	Function name : ie_re_search_backward
**
**	Description : Recherche en arriere d'une expression reguliere dans
**         le buffer courant.  
**	Input : la RE
**	Output : -1 si failed -2 si error sinon la position du debut
**         de la RE dans le buffer courant.
*/
int ie_re_search_backward ( text, regex )
    Text *text;
    char *regex;
{
    int result = RE_Search ( regex, TopBuf(text->buf),
			    LeftBuf(text->buf) - TopBuf(text->buf) + 1, BACKWARD);
  
    if ( result < 0 )
      return result;
    else {
	text -> ie_mark = True;
	return result;
    }
}

/*
**	Function name : ie_re_replace
**
**	Description : Remplacement d'une expression reguliere par une string
**         qui peut contenir :
**         \& : substitution par la regex.
**         \d : substitution par la substring d de la regex.  
**	Input : RE et newstring.
**	Output : True si Ok False sinon.
*/
int ie_re_replace ( text, regex, newstring )
    Text *text;
    char *regex, *newstring;
{
    int result;
    if ( ie_re_search_forward ( text, regex ) < 0 )
      return False;

    result = RE_ReplaceMatch ( text, newstring );
    text -> ie_mark = True;
    return result;
}

/*
**	Function name : ie_re_replace_match
**
**	Description :
**	Input :
**	Output :
*/
int ie_re_replace_match ( text, newstring )
    Text *text;
    char *newstring;
{
    int result = RE_ReplaceMatch ( text, newstring );
    text -> ie_mark = True;
    return result;
}

/*
**	Function name : ie_re_match_beginning
**
**	Description :
**	Input :
**	Output :
*/
int ie_re_match_beginning ( text, n)
    Text *text;
    int n;
{
    return RE_Match (n, 1);
}

/*
**	Function name : ie_re_match_end
**
**	Description :
**	Input :
**	Output :
*/
int ie_re_match_end ( text, n)
    Text *text;
    int n;
{
    return RE_Match (n, 0);
}

/*
**	Function name : ie_shell_to_string
**
**	Description : Execution d'une commande Shell.
**	Input : La commande shell.
**	Ouput : La resultat de la commande (sans le '\n') ou 0 si erreur.
**        le resultat doit etre libere par le client.  
*/
char *ie_shell_to_string ( text, str )
    Text *text;
    char *str;
{
    FILE *fd;
#define MAX_LINE 2048
    char tmp[MAX_LINE];
    int len, t_len = 0;
    char *pp, *ptr = 0;
    
    if ( (fd = popen ( str,  "r")) == NULL ) {
      (void) DisplayWMessage ( "Shell command", "Popen error", True );
      return 0;
    }
    else {
	bzero(tmp, MAX_LINE);
	while (fgets ( tmp,MAX_LINE, fd ) != 0 ) {
	    len = strlen(tmp);
	    pp = (char *) malloc ((unsigned) len+t_len+1);
	    bzero(pp, len+t_len+1);
	    if(ptr && t_len)
	      bcopy(ptr,pp,t_len);
	    bcopy ( tmp, pp + t_len, len );
	    t_len += len;
	    if (ptr)
	      (void) free(ptr);
	    ptr = pp;
	}
    }
    (void) pclose(fd);
    /* On vire le '\n' si il existe */
    if ((ptr != 0) && ptr[strlen(ptr)-1] == '\n')
      ptr[strlen(ptr)-1] = 0;
    
    return (ptr);
}

/*
**	Function name : ie_set_mark
**
**	Description :
**	Input :
**	Output :
*/
void ie_set_mark ( text, pos )
    Text *text;
    int pos;
{
    int current;
    
    if (pos <0)
      return;

    current = ie_current_position (text);
    
    (void) ie_goto_char(text,pos);
    text -> markline = text -> no_current_line;
    text -> markpos = GetNcFromLeft (text -> buf);
    (void) ie_goto_char(text,current);
}

/*
**	Function name : ie_goto_mark
**
**	Description :
**	Input :
**	Output :
*/
void ie_goto_mark ( text )
    Text *text;
{
    if ( text -> markline == 0 )
      return;

    if ( text -> no_current_line != text -> markline ) {
      (void) GotoLineNumber ( text, text -> markline );
      (void) ie_goto_char (text, ie_current_position(text) + text -> markpos);
    }
    else {
      (void) ie_goto_beginning_of_line (text);
      (void) ie_goto_char (text, ie_current_position(text) + text -> markpos);
    }

    text -> ie_mark = True;
}



/*
**	Function name : ie_reset_mark
**
**	Description :
**	Input :
**	Output :
*/
void ie_reset_mark ( text )
    Text *text;
{
    text -> markline = 0;
    text -> markpos = 0;
}

/*
**	Function name : ie_color_region
**
**	Description : 
**	Input :
**	Output :
*/
void ie_color_area ( text, start, end, colorname )
    Text *text;
    int start, end;
    char *colorname;
{
    extern Display *dpy;
    
    if ( DefaultDepth ( dpy, DefaultScreen ( dpy )) == 1 )
      return;
    (void) SetColorArea ( text, start, end, colorname );
}

/*
**	Function name : ie_past_region
**
**	Description :
**	Input :
**	Output :
*/
void ie_paste_region ( text )
    Text *text;
{
  
}

/*
**	Function name : ie_copy_region
**
**	Description :
**	Input :
**	Output :
*/
void ie_copy_region ( text )
    Text *text;
{
  
}

/*
**	Function name : ie_cut_region
**
**	Description :
**	Input :
**	Output :
*/
void ie_cut_region ( text )
    Text *text;
{
  
}

/*
**	Function name : ie_remove_colors
**
**	Description :
**	Input :
**	Output :
*/
void ie_remove_colors ( text )
    Text *text;
{
  (void) DeleteColorList (text);
}

/*
**	Function name : ie_watch_on
**
**	Description :
**	Input :
**	Output :
*/
void ie_watch_on(text)
    Text *text;
{
  WatchOn (text -> window);
}

/*
**	Function name : ie_watch_off
**
**	Description :
**	Input :
**	Output :
*/
void ie_watch_off(text)
    Text *text;
{
  WatchOff (text -> window);
}

/*
**	Function name : ie_usleep
**
**	Description :
**	Input :
**	Output :
*/
void ie_usleep(text, t)
    Text *text;
    int t;
{
    if (t <= 0 || t >= 1000000) {
	DisplayWMessage("usleep : bad argument", "usleep", 1);
	return;
    }
    (void) SmallTime((long) t);
}

/*
**	Function name : current_mode
**
**	Description :
**	Input :
**	Output : le nom du mode courrant. (doit etre libere)
*/
char *ie_current_mode(text)
    Text *text;
{
  if (text -> current_mode -> name)
    return ((char *) text -> current_mode -> name);
  else
    return 0;
}

/*
**	Function name : ie_monochrome
**
**	Description :
**	Input :
**	Output :
*/
int ie_monochrome(text)
	Text *text;
{
        extern Display *dpy;

	if ( DefaultDepth ( dpy, DefaultScreen ( dpy )) == 1 )
		return True;
	else
		return False;
}
