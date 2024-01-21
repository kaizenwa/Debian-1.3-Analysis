/* ########################################################################

				 mode.c

   File: mode.c
   Path: /home/fournigault/c/X11/xcoral-2.31/mode.c
   Description: 
   Created: Fri Jan 27 11:20:41 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:20:42 MET 1995
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
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#ifdef __FreeBSD__
#include <stdlib.h>
#else /* !__FreeBSD__ */
#include <malloc.h>
#endif

#include "xcoral.h"
#include "process.h"
#include "bm_search.h"
#include "text_cursor.h"
#include "mark_reg.h"
#include "page.h"
#include "chars_cmds.h"
#include "get_file.h"
#include "input_str.h"
#include "new_window.h"
#include "parse.h"
#include "config.h"
#include "options.h"

/* On va construire une liste chainee dont le premier
    element sera le mode par default */
  
Mode *m_default; 

FCT (void, ie_WR_delete, (Text *text) ); 

FCT (static Mode *, AllocMode, () );
FCT (static void, ResetMode, (Mode *mode) );
FCT (static void, SetDefaultModeValues, (Mode *mode) );
FCT (static void, SetModeName, (Mode *mode, char *mode_name) );

FCT (static void, ie_WR_setmark, (Text *text) );
FCT (static void, ie_WR_goto_beginning_of_line , (Text *text) );
FCT (static void, ie_WR_goto_previous_char, (Text *text) );
FCT (static void, ie_WR_delete_char, (Text *text) );
FCT (static void, ie_WR_goto_end_of_line, (Text *text) );
FCT (static void, ie_WR_goto_next_char, (Text *text) );
FCT (static void, ie_WR_abort, (Text *text) );
FCT (static void, ie_WR_return, (Text *text) );
FCT (static void, ie_WR_tab, (Text *text) );
FCT (static void, ie_WR_kill_lines, (Text *text) );
FCT (static void, ie_WR_redisplay, (Text *text) );
FCT (static void, ie_WR_cursor_down, (Text *text) );
FCT (static void, ie_WR_cursor_up, (Text *text) );
FCT (static void, ie_WR_open_space, (Text *text) );
FCT (static void, ie_WR_undo, (Text *text) );
FCT (static void, ie_WR_redo, (Text *text) );
FCT (static void, ie_WR_next_page, (Text *text) );
FCT (static void, ie_WR_kill_region, (Text *text) );
FCT (static void, ie_WR_paste_region, (Text *text) );
FCT (static void, ie_WR_eval_region, (Text *text) );
FCT (static void, ie_WR_indent_region, (Text *text) );
FCT (static void, ie_WR_current_line_to_top, (Text *text) );
FCT (static void, ie_WR_first_page, (Text *text) );
FCT (static void, ie_WR_last_page, (Text *text) );
FCT (static void, ie_WR_previous_page, (Text *text) );
FCT (static void, ie_WR_copy_region, (Text *text) );
FCT (static void, ie_WR_delete_window, (Text *text) );
FCT (static void, ie_WR_exchange_cursor_mark, (Text *text) );
FCT (static void, ie_WR_kill_current_buffer, (Text *text) );
FCT (static void, ie_WR_goto_mark, (Text *text) );
FCT (static void, ie_WR_new_window, (Text *text) );
FCT (static void, ie_WR_backward_search, (Text *text) );
FCT (static void, ie_WR_forward_search, (Text *text) );
FCT (static void, ie_WR_global_replace, (Text *text) );
FCT (static void, ie_WR_query_replace, (Text *text) );
FCT (static void, ie_WR_read_file, (Text *text) );
FCT (static void, ie_WR_save_file, (Text *text) );
FCT (static void, ie_WR_write_file, (Text *text) );
FCT (static void, ie_WR_insert_file, (Text *text) );
FCT (static void, ie_WR_goto_line, (Text *text) );
FCT (static void, ie_WR_eval_expression, (Text *text) );
FCT (static void, ie_WR_list_open_file, (Text *text) );
FCT (static void, ie_WR_str_return, (Text *text) );
FCT (static void, ie_WR_str_tab, (Text *text) );
FCT (static void, ie_WR_str_abort, (Text *text) );
FCT (static void, ie_WR_str_previous, (Text *text) );
FCT (static void, ie_WR_shell_return, (Text *text) );
FCT (static void, ie_WR_shell_CtrC, (Text *text) );
FCT (static void, ie_WR_shell_CtrD, (Text *text) );
FCT (static void, ie_WR_shell_key, (Text *text) );

/*
**	Function name : AllocMode
**
**	Description : Alloue la place pour un nouveau mode
**	Input : 
**	Ouput : un pointeur sur une structure Mode.
*/
static Mode *AllocMode ()
{
    Mode *tmp;
    
    if (( tmp = (Mode *) malloc ((unsigned) sizeof (Mode))) == 0 )
      return 0;
    
    bzero ( (char *) tmp, sizeof (Mode));
    return (tmp);
}


/*
**	Function name : InitMode
**
**	Description : Contruction du mode par defaut.
**	Input : 
**	Ouput :
*/
void InitMode ()
{
    /* Le mode par default */
    if ( (m_default = AllocMode ()) == 0 ) {
	(void) fprintf ( stderr, "Malloc error\n" );
	exit (1);
    }
    bzero ( (char *) m_default, sizeof (Mode));
    
    (void) SetModeName ( m_default, "default" );
    m_default -> suffixes = 0;
    m_default -> font = 0;
    (void) SetDefaultModeValues ( m_default );
/*    (void) SetModeFont ( m_default, TEXT_FONT ); */
    m_default -> font = GetOpFont ( OP_TEXT_FONT );
    
    /* Le mode C et C++ */
    if ( CreateNewMode ( "C-mode" ) == 0 )
      (void) fprintf ( stderr, "Create C mode error\n" );

    if ( CreateNewMode ( "C++mode" ) == 0 )
      (void) fprintf ( stderr, "Create C++ mode error\n" );
    
    if ( CreateNewMode ( "Latex" ) == 0 )
      (void) fprintf ( stderr, "Create Latex mode error\n" );
    
    if ( CreateNewMode ( "Html" ) == 0 )
      (void) fprintf ( stderr, "Create Html mode error\n" );
    
    if ( CreateNewMode ( "Ada" ) == 0 )
      (void) fprintf ( stderr, "Create Ada mode error\n" );
    
    if ( CreateNewMode ( "Perl" ) == 0 )
      (void) fprintf ( stderr, "Create Perl mode error\n" );
    
    if ( CreateNewMode ( "Fortran" ) == 0 )
      (void) fprintf ( stderr, "Create Fortran mode error\n" );
    
    if ( CreateNewMode ( "shell" ) == 0 )
      (void) fprintf ( stderr, "Create shell mode error\n" );

     if ( CreateNewMode ( "Edir" ) == 0 )
      (void) fprintf ( stderr, "Create Edir mode error\n" );

   /* Le mode pour input-string */
    if ( CreateNewMode ( "input_str" ) == 0 )
      (void) fprintf ( stderr, "Create input_mode error\n" );

    /* Le mode Shell */
    if ( CreateNewMode ( "Shell" ) == 0 )
      (void) fprintf ( stderr, "Create shell-mode error\n" );
    InitShellMode ();
}

/*
**	Function name : ResetMode
**
**	Description : 
**	Input : 
**	Ouput :
*/
static void ResetMode ( mode )
    Mode *mode;
{
    int i;
    
    for ( i=0; i< 512; i++ ) {
	mode -> key [i].func = 0;
	mode -> key [i].type = 0;
	mode -> ctrX_key [i].func = 0;
	mode -> ctrX_key [i].type = 0;
	mode -> esc_key [i].func = 0;
	mode -> esc_key [i].type = 0;
    }
}

/*
**	Function name : GetMode
**
**	Description : Retourne un pointeur sur le mode 
**          (recherche par le nom ).
**	Input : 
**	Ouput : un pointeur sur le mode sinon 0.
*/
Mode *GetMode ( name )
    char *name;
{
    Mode *tmp = m_default;
    int trouve = False;

    if ( strcmp ( name, m_default -> name ) == 0 )
      return (m_default);

    while ( tmp -> next != 0 ) {
	tmp = tmp -> next;
	if ( strcmp ( name, tmp -> name ) == 0 ) {
	    trouve = True;
	    break;
	}
    }
    return ( (trouve == True) ? tmp : (Mode*) 0);
}


/*
**	Function name : CreateNewMode
**
**	Description : 
**	Input : 
**	Ouput :
*/
Mode *CreateNewMode ( mode_name )
    char *mode_name;
{
    Mode *new_mode, *tmp = m_default;
 
    if ( (mode_name == 0) || (strlen (mode_name) == 0) )
      return 0;

    if ( (new_mode = GetMode ( mode_name )) != 0 ) {
	/* On reinitialise le mode */
	ResetMode ( new_mode );
	(void) SetDefaultModeValues ( new_mode );
	return (new_mode);
    }
	
    if ( (new_mode = AllocMode ()) == 0 ) {
	(void) fprintf ( stderr, "Malloc error\n" );
	return 0;
    }
    bzero ( (char *) new_mode, sizeof (Mode));

    /* Liaison avec le dernier element de la liste */
    while ( tmp -> next != 0 ) {
	tmp = tmp -> next;
    }
    tmp -> next = new_mode; /* et voila */
    
    (void) SetModeName ( new_mode, mode_name );
    new_mode -> font = m_default -> font;
    (void) SetDefaultModeValues ( new_mode );
    return (new_mode);
}
 

/*
**	Function name : SetModeName
**
**	Description : 
**	Input : 
**	Ouput :
*/
static void SetModeName ( mode , mode_name )
    Mode *mode;
    char *mode_name;
{
    mode -> name = (char *) malloc ((unsigned) (strlen(mode_name) + 2));

    if ( mode -> name == 0 ) {
	(void) fprintf ( stderr, "Malloc error\n" );
	exit (1);
    }

    (void) strcpy ( mode -> name, mode_name );
}


/*
**	Function name : SetModeSuffixe
**
**	Description : 
**	Input : 
**	Ouput :
*/
void SetModeSuffixe ( mode, suffixes )
    Mode *mode;
    char *suffixes;
{
    if ( suffixes == 0 ) {
	mode -> suffixes = 0;
      return;
    }
    else {
	mode -> suffixes = (char *) malloc ((unsigned) (strlen(suffixes) + 2));
	if ( mode -> name == 0 ) {
	    (void) fprintf ( stderr, "Malloc error\n" );
	    exit (1);
	}
	(void) strcpy ( mode -> suffixes, suffixes );
    }
}

/*
**	Function name : SetModeFont
**
**	Description : 
**	Input : 
**	Ouput :
*/
void SetModeFont ( mode, font_name )
    Mode *mode;
    char *font_name;
{
    extern Display *dpy;
    
    if ( font_name == 0 ) {
	mode -> font = 0;
      return;
    }
    else {
	mode -> font = (XFontStruct *) LoadFont ( dpy, font_name );
    }
}


/*
**	Function name : SearchMode
**
**	Description : Recherche d'un mode eventuel
**
**	Input : Un nom de fichier.
**	Ouput : Le mode associe, sinon 0
*/
Mode *SearchMode ( filename )
    char *filename;
{
    char *end_name;
    Mode *tmp = m_default;
    int trouve = False;

    end_name = (char * ) strrchr ( filename, '.' );
    if ( end_name == 0 )
      return m_default;

    if ( m_default -> suffixes != 0 ) {
      if ( BmSearch ( m_default -> suffixes, end_name,
		    strlen (m_default -> suffixes),1 ) != -1 )
	return m_default;
    }
    
    while ( tmp -> next != 0 ) {
	tmp = tmp -> next;
	if ( tmp -> suffixes != 0 ) {
	    if ( BmSearch ( tmp -> suffixes, end_name,
			   strlen (tmp -> suffixes),1 ) != -1 ) {
		trouve = True;
		break;
	    }
	}
    }
    return ( (trouve == True) ? tmp : (Mode*) m_default );
}


/*
    POUR LES MODES
*/
/*
    Definition des fonctions utilisables a partir de
    l'interpreteur.
    ATTENTION : Celles-ci raffraichissent la ou les pages courantes.
    d'ou le suffixe WR ( with refresh ).
*/ 
#define C_OFF TextCursorOff (text)
#define C_ON TextCursorOn (text)
  
static void ie_WR_setmark ( text ) Text *text; { C_OFF;SetMark ( text );C_ON;}
static void ie_WR_goto_beginning_of_line ( text ) Text *text; { C_OFF;MoveToBline ( text );C_ON;}
static void ie_WR_goto_previous_char ( text ) Text *text; { C_OFF;BackwardChar ( text );C_ON;}
static void ie_WR_delete_char ( text ) Text *text; { C_OFF;Control_D ( text );C_ON;}
static void ie_WR_goto_end_of_line ( text ) Text *text; { C_OFF;MoveToEline ( text );C_ON;}
static void ie_WR_goto_next_char ( text ) Text *text; { C_OFF;ForwardChar ( text );C_ON;}
static void ie_WR_abort ( text ) Text *text; { C_OFF;AbortCurrentCmd ( text );C_ON;}
static void ie_WR_return ( text ) Text *text; { f_return ( text ); }
static void ie_WR_tab ( text ) Text *text; { f_tab ( text ); }
static void ie_WR_kill_lines ( text ) Text *text; { C_OFF;KillLines ( text );C_ON;}
static void ie_WR_redisplay ( text ) Text *text; { C_OFF;UpdatePage ( text );C_ON;}
static void ie_WR_cursor_down ( text ) Text *text; { C_OFF;DownCursor (text);C_ON;}
static void ie_WR_cursor_up ( text ) Text *text; { C_OFF;UpCursor (text);C_ON;}
static void ie_WR_open_space ( text ) Text *text; { C_OFF;OpenSpace (text);C_ON;}
static void ie_WR_undo ( text ) Text *text; { C_OFF;DoUndo (text);C_ON;}
static void ie_WR_redo ( text ) Text *text; { C_OFF;DoUndoUndo (text);C_ON;}
static void ie_WR_kill_region ( text ) Text *text; { C_OFF;KillRegion (text);C_ON;}
static void ie_WR_paste_region ( text ) Text *text; { C_OFF;PasteRegion (text);C_ON;} 
static void ie_WR_eval_region ( text ) Text *text; { C_OFF;EvalRegion (text);C_ON;} 
static void ie_WR_indent_region ( text ) Text *text; { C_OFF;IndentRegion (text);C_ON;}
static void ie_WR_current_line_to_top ( text ) Text *text; { C_OFF;CurrentLineToTop (text);C_ON;}
static void ie_WR_first_page ( text ) Text *text;{ C_OFF; if (! IsFreezeMenus () == True )FirstPage (text); C_ON;}
static void ie_WR_last_page ( text ) Text *text; { C_OFF;if (! IsFreezeMenus () == True )GotoEndOfBuf (text);C_ON;}
static void ie_WR_previous_page ( text ) Text *text; { C_OFF;if (! IsFreezeMenus () == True )PreviousPage (text);C_ON;}
static void ie_WR_next_page ( text ) Text *text; { C_OFF;if (! IsFreezeMenus () == True )NextPage (text);C_ON;}
static void ie_WR_copy_region ( text ) Text *text; { C_OFF;CopyRegion (text);C_ON;}
static void ie_WR_delete_window ( text ) Text *text; { DeleteCurrentWindow (text);}
static void ie_WR_exchange_cursor_mark ( text ) Text *text; { ExchangePointMark (text);}
static void ie_WR_kill_current_buffer ( text ) Text *text; { KillBuffer (text);}
static void ie_WR_goto_mark ( text ) Text *text; { GotoTheMark (text);}
static void ie_WR_new_window ( text ) Text *text; { NewWindow (text);}

/*  Celles qui sont interactives (bottom mini-buffer) */

static void ie_WR_backward_search ( text ) Text *text; { C_OFF;BackwardSearch (text);C_ON;}
static void ie_WR_forward_search ( text ) Text *text; { C_OFF;ForwardSearch (text);C_ON;}
static void ie_WR_global_replace ( text ) Text *text; { C_OFF;GlobalReplace (text);C_ON;}
static void ie_WR_query_replace ( text ) Text *text; { C_OFF;QueryReplace (text);C_ON;}
static void ie_WR_read_file ( text ) Text *text; { KbdReadFile (text);}
static void ie_WR_save_file ( text ) Text *text; { KbdSaveFile (text);}
static void ie_WR_write_file ( text ) Text *text; { KbdWriteFile (text);}
static void ie_WR_insert_file ( text ) Text *text; { KbdInsertFile (text);}
static void ie_WR_goto_line ( text ) Text *text; { GotoLine (text);}
static void ie_WR_eval_expression ( text ) Text *text; { EvalExpressionFromKey (text);}

/*  Une  qui ouvre un fenetre */
static void ie_WR_list_open_file ( text ) Text *text; { DisplayOpenFiles (text);}

/* Les fonctions speciales pour le mini-buffer. */
static void ie_WR_str_return ( text ) Text *text; { InputStrReturn (text);}
static void ie_WR_str_tab ( text ) Text *text; { InputStrTab (text);}
static void ie_WR_str_abort ( text ) Text *text; { InputStrAbort (text);}
static void ie_WR_str_previous ( text ) Text *text; { InputStrPrevious (text);}

/* Les fonctions speciales pour le Shell */
static void ie_WR_shell_return ( text ) Text *text; { shell_return (text);}
static void ie_WR_shell_CtrC ( text ) Text *text; { shell_CtrC (text);}
static void ie_WR_shell_CtrD ( text ) Text *text; { shell_CtrD (text);}
static void ie_WR_shell_key ( text ) Text *text; { shell_key (text);}

/* Pour gerer les couples function_name <-> string */
typedef struct {
    void (* f) ();
    char *s;
} f2s;

#define NB_BUILTIN_FUNC 53

f2s func_string [NB_BUILTIN_FUNC] = {
    ie_WR_setmark, "setmark",
    ie_WR_goto_beginning_of_line, "goto_beginning_of_line", 
    ie_WR_goto_previous_char, "goto_previous_char", 
    ie_WR_delete_char, "delete_char",
    ie_WR_goto_end_of_line, "goto_end_of_line",
    ie_WR_goto_next_char, "goto_next_char",
    ie_WR_abort, "abort",
    ie_WR_return, "return",
    ie_WR_delete, "delete",
    ie_WR_tab, "tab",
    ie_WR_kill_lines, "kill_line", 
    ie_WR_redisplay, "redisplay",
    ie_WR_cursor_down, "cursor_down",
    ie_WR_cursor_up, "cursor_up",
    ie_WR_open_space, "open_space",
    ie_WR_undo, "undo",
    ie_WR_redo, "redo",
    ie_WR_next_page, "next_page",
    ie_WR_kill_region, "kill_region",
    ie_WR_paste_region, "paste_region",
    ie_WR_eval_region, "eval_region",
/*    ie_WR_indent_region, "c_indent_region", */
    ie_WR_current_line_to_top, "current_line_to_top",
    ie_WR_first_page, "first_page",
    ie_WR_last_page, "last_page",
    ie_WR_previous_page, "previous_page",
    ie_WR_copy_region, "copy_region",
    ie_WR_delete_window, "delete_window", 
    ie_WR_exchange_cursor_mark, "exchange_cursor_mark",
    ie_WR_kill_current_buffer, "kill_current_buffer",
    ie_WR_goto_mark, "goto_mark",
    ie_WR_new_window, "new_window",
    ie_WR_backward_search, "backward_search",
    ie_WR_forward_search, "forward_search",
    ie_WR_global_replace, "global_replace",
    ie_WR_query_replace, "query_replace",
    ie_WR_read_file, "read_file",
    ie_WR_save_file, "save_file",
    ie_WR_write_file, "write_file",
    ie_WR_insert_file, "insert_file",
    ie_WR_goto_line, "goto_line",
    ie_WR_eval_expression, "eval_expression",
    ie_WR_list_open_file, "list_open_file",
    ie_WR_str_return,"str_return",
    ie_WR_str_tab,"str_tab",
    ie_WR_str_abort,"str_abort",
    ie_WR_str_previous,"str_previous",    
    ie_WR_shell_return, "shell_return",
    ie_WR_shell_CtrC, "shell_CtrC",
    ie_WR_shell_CtrD, "shell_CtrD",
    ie_WR_shell_key, "shell_key",
    0, 0 
};


/*
**	Function name : GetFuncNameFromString
**
**	Description : Retourne  
**	Input : 
**	Ouput :
*/
#ifdef __alpha
long GetFuncNameFromString ( s )
#else
int GetFuncNameFromString ( s )
#endif    
    char *s;
{
    int i = 0;

    while ( func_string [i].f != 0 ) {
	if ( strcmp ( (char *) func_string [i].s, s ) == 0 )
#ifdef __alpha	  
	  return ((long) func_string [i].f);
#else	
	  return ((int) func_string [i].f);
#endif	  
	i++;
    }
    return (0);
}


/*
**	Function name : GetFuncStringFromName
**
**	Description : 
**	Input : 
**	Ouput :
*/
/*
static char *GetFuncStringFromName ( func )
    int (* func) ();
{
    int i = 0;
    
    while ( func_string [i].f != 0 ) {
	if ( (int) func_string [i].f == (int) func )
	  return ((char *) func_string [i].s);
	i++;
    }
    return ((char *) 0);
}
*/

/*
**	Function name : SetDefaultModeValues
**
**	Description : Charge le mode par default dans le mode courant.
**	Input : Le mode.
**	Ouput :
**
*/
static void SetDefaultModeValues ( mode )
    Mode *mode;
{
    extern void StartMacro (), EndMacro (), PlayMacro (), QuotedChar ();
    extern void DisplayLineNumber ();
    extern FCT(int, Xk_Code,(char *));

    /* Les touches, Controle + touche et Delete, XK */

    mode -> key [Ctr_sp].func = ie_WR_setmark;
    mode -> key [CtrA].func = ie_WR_goto_beginning_of_line;
    mode -> key [CtrB].func = ie_WR_goto_previous_char;
    mode -> key [CtrC].func = PlayMacro;
    mode -> key [CtrD].func = ie_WR_delete_char;
    mode -> key [CtrE].func = ie_WR_goto_end_of_line;
    mode -> key [CtrF].func = ie_WR_goto_next_char;
    mode -> key [CtrG].func = ie_WR_abort;
    mode -> key [CtrH].func = ie_WR_delete;
    mode -> key [CtrI].func = ie_WR_tab;
    mode -> key [CtrJ].func = ie_WR_return;
    mode -> key [CtrK].func = ie_WR_kill_lines;
    mode -> key [CtrL].func = ie_WR_redisplay; 
    mode -> key [CtrM].func = ie_WR_return;
    mode -> key [CtrN].func = ie_WR_cursor_down;
    mode -> key [CtrO].func = ie_WR_open_space;
    mode -> key [CtrP].func = ie_WR_cursor_up;
    mode -> key [CtrQ].func = QuotedChar;
    mode -> key [CtrR].func = ie_WR_backward_search;
    mode -> key [CtrS].func = ie_WR_forward_search;
    mode -> key [CtrT].func = ie_WR_redo;
    mode -> key [CtrU].func = ie_WR_undo;
    mode -> key [CtrV].func = ie_WR_next_page;
    mode -> key [CtrW].func = ie_WR_kill_region;
    mode -> key [CtrY].func = ie_WR_paste_region;
    mode -> key [DELETE].func = ie_WR_delete;

    mode -> key [Ctr_sp].type = BUILTIN_FUNC; 
    mode -> key [CtrA].type = BUILTIN_FUNC; 
    mode -> key [CtrB].type = BUILTIN_FUNC; 
    mode -> key [CtrC].type = BUILTIN_FUNC;     
    mode -> key [CtrD].type = BUILTIN_FUNC; 
    mode -> key [CtrE].type = BUILTIN_FUNC; 
    mode -> key [CtrF].type = BUILTIN_FUNC; 
    mode -> key [CtrG].type = BUILTIN_FUNC; 
    mode -> key [CtrH].type = BUILTIN_FUNC; 
    mode -> key [CtrI].type = BUILTIN_FUNC; 
    mode -> key [CtrJ].type = BUILTIN_FUNC; 
    mode -> key [CtrK].type = BUILTIN_FUNC; 
    mode -> key [CtrL].type = BUILTIN_FUNC; 
    mode -> key [CtrM].type = BUILTIN_FUNC; 
    mode -> key [CtrN].type = BUILTIN_FUNC; 
    mode -> key [CtrO].type = BUILTIN_FUNC; 
    mode -> key [CtrP].type = BUILTIN_FUNC;
    mode -> key [CtrQ].type = BUILTIN_FUNC;
    mode -> key [CtrR].type = BUILTIN_FUNC; 
    mode -> key [CtrS].type = BUILTIN_FUNC; 
    mode -> key [CtrT].type = BUILTIN_FUNC; 
    mode -> key [CtrU].type = BUILTIN_FUNC; 
    mode -> key [CtrV].type = BUILTIN_FUNC; 
    mode -> key [CtrW].type = BUILTIN_FUNC; 
    mode -> key [CtrY].type = BUILTIN_FUNC; 
    mode -> key [DELETE].type = BUILTIN_FUNC;

    {
      int code = Xk_Code("Right");
      
      mode -> key [code].type = BUILTIN_FUNC;
      mode -> key [code].func = ie_WR_goto_next_char;
    }
    {
      int code = Xk_Code("R12");
      
      mode -> key [code].type = BUILTIN_FUNC;
      mode -> key [code].func = ie_WR_goto_next_char;
    }    
    {
      int code = Xk_Code("Left");
      
      mode -> key [code].type = BUILTIN_FUNC;
      mode -> key [code].func = ie_WR_goto_previous_char;
    }
    {
      int code = Xk_Code("R10");
      
      mode -> key [code].type = BUILTIN_FUNC;
      mode -> key [code].func = ie_WR_goto_previous_char;
    }    
    {
      int code = Xk_Code("Up");
      
      mode -> key [code].type = BUILTIN_FUNC;
      mode -> key [code].func = ie_WR_cursor_up;
    }
    {
      int code = Xk_Code("R8");
      
      mode -> key [code].type = BUILTIN_FUNC;
      mode -> key [code].func = ie_WR_cursor_up;
    }    
    {
      int code = Xk_Code("Down");
      
      mode -> key [code].type = BUILTIN_FUNC;
      mode -> key [code].func = ie_WR_cursor_down;
    }
    {
      int code = Xk_Code("R14");
      
      mode -> key [code].type = BUILTIN_FUNC;
      mode -> key [code].func = ie_WR_cursor_down;
    }    
    {
      int code = Xk_Code("Home");
      
      mode -> key [code].type = BUILTIN_FUNC;
      mode -> key [code].func = ie_WR_first_page;
    }
    {
      int code = Xk_Code("R7");
      
      mode -> key [code].type = BUILTIN_FUNC;
      mode -> key [code].func = ie_WR_first_page;
    }
    {
      int code = Xk_Code("Next");
      
      mode -> key [code].type = BUILTIN_FUNC;
      mode -> key [code].func = ie_WR_next_page;
    }
    {
      int code = Xk_Code("R15");
      
      mode -> key [code].type = BUILTIN_FUNC;
      mode -> key [code].func = ie_WR_next_page;
    }    
    {
      int code = Xk_Code("Prior");
      
      mode -> key [code].type = BUILTIN_FUNC;
      mode -> key [code].func = ie_WR_previous_page;
    }
    {
      int code = Xk_Code("R9");
      
      mode -> key [code].type = BUILTIN_FUNC;
      mode -> key [code].func = ie_WR_previous_page;
    }    
    {
      int code = Xk_Code("End");
      
      mode -> key [code].type = BUILTIN_FUNC;
      mode -> key [code].func = ie_WR_last_page;
    }
    {
      int code = Xk_Code("R13");
      
      mode -> key [code].type = BUILTIN_FUNC;
      mode -> key [code].func = ie_WR_last_page;
    }
    
    /* Les sequences ESC key */

    mode -> esc_key [(int)'e'].func = ie_WR_eval_region;
    mode -> esc_key [(int)'i'].func = ie_WR_indent_region;
    mode -> esc_key [(int)'='].func = DisplayLineNumber;
    mode -> esc_key [(int)'t'].func = ie_WR_current_line_to_top;
    mode -> esc_key [(int)'q'].func = ie_WR_query_replace;
    mode -> esc_key [(int)'r'].func = ie_WR_global_replace;
    mode -> esc_key [(int)'v'].func = ie_WR_previous_page;
    mode -> esc_key [(int)'w'].func = ie_WR_copy_region;    
    mode -> esc_key [(int)'<'].func = ie_WR_first_page;
    mode -> esc_key [(int)'>'].func = ie_WR_last_page;
   
    mode -> esc_key [(int)'e'].type = BUILTIN_FUNC; 
    mode -> esc_key [(int)'i'].type = BUILTIN_FUNC; 
    mode -> esc_key [(int)'='].type = BUILTIN_FUNC; 
    mode -> esc_key [(int)'t'].type = BUILTIN_FUNC; 
    mode -> esc_key [(int)'q'].type = BUILTIN_FUNC; 
    mode -> esc_key [(int)'r'].type = BUILTIN_FUNC; 
    mode -> esc_key [(int)'v'].type = BUILTIN_FUNC; 
    mode -> esc_key [(int)'w'].type = BUILTIN_FUNC; 
    mode -> esc_key [(int)'<'].type = BUILTIN_FUNC; 
    mode -> esc_key [(int)'>'].type = BUILTIN_FUNC; 

    /* Les sequences Control_X + key */
    
    mode -> ctrX_key [CtrC].func = ie_WR_delete_window;
    mode -> ctrX_key [CtrE].func = ie_WR_eval_expression;
    mode -> ctrX_key [CtrF].func = ie_WR_read_file;
    mode -> ctrX_key [CtrS].func = ie_WR_save_file;
    mode -> ctrX_key [CtrW].func = ie_WR_write_file;
    mode -> ctrX_key [CtrX].func = ie_WR_exchange_cursor_mark;
    
    mode -> ctrX_key [(int) 'b'].func = ie_WR_list_open_file;
    mode -> ctrX_key [(int) 'k'].func = ie_WR_kill_current_buffer;
    mode -> ctrX_key [(int) 'l'].func = ie_WR_goto_line;
    mode -> ctrX_key [(int) 'm'].func = ie_WR_goto_mark;
    mode -> ctrX_key [(int) 'i'].func = ie_WR_insert_file; 
    mode -> ctrX_key [(int) 'n'].func = ie_WR_new_window;
    mode -> ctrX_key [(int) '('].func = StartMacro;
    mode -> ctrX_key [(int) ')'].func = EndMacro;
    
    
    mode -> ctrX_key [CtrC].type = BUILTIN_FUNC; 
    mode -> ctrX_key [CtrE].type = BUILTIN_FUNC; 
    mode -> ctrX_key [CtrF].type = BUILTIN_FUNC; 
    mode -> ctrX_key [CtrS].type = BUILTIN_FUNC; 
    mode -> ctrX_key [CtrW].type = BUILTIN_FUNC; 
    mode -> ctrX_key [CtrX].type = BUILTIN_FUNC; 
    
    mode -> ctrX_key [(int) 'b'].type = BUILTIN_FUNC; 
    mode -> ctrX_key [(int) 'k'].type = BUILTIN_FUNC; 
    mode -> ctrX_key [(int) 'l'].type = BUILTIN_FUNC; 
    mode -> ctrX_key [(int) 'm'].type = BUILTIN_FUNC; 
    mode -> ctrX_key [(int) 'i'].type = BUILTIN_FUNC; 
    mode -> ctrX_key [(int) 'n'].type = BUILTIN_FUNC; 
    mode -> ctrX_key [(int) '('].type = BUILTIN_FUNC;
    mode -> ctrX_key [(int) ')'].type = BUILTIN_FUNC;
}

