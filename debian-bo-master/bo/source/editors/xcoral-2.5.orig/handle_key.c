/* ########################################################################

			      handle_key.c

   File: handle_key.c
   Path: /home/fournigault/c/X11/xcoral-2.31/handle_key.c
   Description: 
   Created: Fri Jan 27 11:05:09 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:05:10 MET 1995
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
#include <X11/cursorfont.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <ctype.h>
#if defined(SYSV) || defined(UNIXWARE)
#include <string.h>
#else
#include <strings.h>
#endif

#include "xcoral.h"
#include "macros.h"
#include "page.h"
#include "mark_reg.h"
#include "text_cursor.h"
#include "chars_cmds.h"
#include "input_str.h"
#include "ie_func.h"
#include "bm_search.h"
#include "new_window.h"
#include "get_file.h"
#include "warn_box.h"
#include "main_events.h"

int last_key = 0;

extern Display *dpy;
extern FCT(     char *, ie_call_function,(Text *, char *, int, int *)   );

extern Trans st_initial[], st_control_x[], st_escape[], st_repeat[];

FCT (static void, GetDigit, (Text *text, InfosKey *infos) );
FCT (static void, GetInfosAsciiKey, (Text *text, int c, InfosKey *infos) );
FCT (static void, GetInfosKey, (Text *text, KeySym ksym, int c, InfosKey *infos) );
FCT (static void, f_ascii, (Text *text, InfosKey *infos) );
FCT (static void, f_ctr, (Text *text, InfosKey *infos) );
FCT (static void, f_ctrx, (Text *text, InfosKey *infos) );
FCT (static void, f_escape, (Text *text, InfosKey *infos) );
FCT (static void, f_esc_first_digit, (Text *text, InfosKey *infos) );
FCT (static void, f_esc_digit, (Text *text, InfosKey *infos) );
FCT (static void, f_nothing, (Text *text, InfosKey *infos) );
FCT (static void, f_special, (Text *text, InfosKey *infos) );

#ifndef XK_DRemove
#define XK_DRemove 0x1000FF00
#endif

XKKey XKInfos[] = {
  {"Multi_key", XK_Multi_key},
  
  {"Home", XK_Home},
  {"Left", XK_Left},
  {"Up", XK_Up},
  {"Right", XK_Right},
  {"Down", XK_Down},
  {"Prior", XK_Prior},
  {"Next", XK_Next},
  {"End", XK_End},
  {"Begin", XK_Begin},
  
  {"Select", XK_Select},
  {"Print", XK_Print},
  {"Execute", XK_Execute},
  {"Insert", XK_Insert},
  {"Remove", XK_DRemove},
  {"Undo", XK_Undo},
  {"Redo", XK_Redo},
  {"Menu", XK_Menu},
  {"Find", XK_Find},
  {"Cancel", XK_Cancel},
  {"Help", XK_Help},
  {"Break", XK_Break},
  {"Mode_switch", XK_Mode_switch},
  {"script_switch", XK_script_switch},
  {"Num_Lock", XK_Num_Lock},
  
  {"KP_Space", XK_KP_Space},
  {"KP_Tab", XK_KP_Tab},
  {"KP_Enter", XK_KP_Enter},
  {"KP_F1", XK_KP_F1},
  {"KP_F2", XK_KP_F2},
  {"KP_F3", XK_KP_F3},
  {"KP_F4", XK_KP_F4},
  {"KP_Equal", XK_KP_Equal},
  {"KP_Multiply", XK_KP_Multiply},
  {"KP_Add", XK_KP_Add},
  {"KP_Separator", XK_KP_Separator},
  {"KP_Subtract", XK_KP_Subtract},
  {"KP_Decimal", XK_KP_Decimal},
  {"KP_Divide", XK_KP_Divide},
  
  {"KP_0", XK_KP_0},
  {"KP_1", XK_KP_1},
  {"KP_2", XK_KP_2},
  {"KP_3", XK_KP_3},
  {"KP_4", XK_KP_4},
  {"KP_5", XK_KP_5},
  {"KP_6", XK_KP_6},
  {"KP_7", XK_KP_7},
  {"KP_8", XK_KP_8},
  {"KP_9", XK_KP_9},
  
  {"F1", XK_F1},
  {"F10", XK_F10},
  {"F11", XK_F11},
  {"F12", XK_F12},
  {"F13", XK_F13},
  {"F14", XK_F14},
  {"F15", XK_F15},
  {"F16", XK_F16},
  {"F17", XK_F17},
  {"F18", XK_F18},
  {"F19", XK_F19},
  {"F2", XK_F2},
  {"F20", XK_F20},
  {"F21", XK_F21},
  {"F22", XK_F22},
  {"F23", XK_F23},
  {"F24", XK_F24},
  {"F25", XK_F25},
  {"F26", XK_F26},
  {"F27", XK_F27},
  {"F28", XK_F28},
  {"F29", XK_F29},
  {"F3", XK_F3},
  {"F30", XK_F30},
  {"F31", XK_F31},
  {"F32", XK_F32},
  {"F33", XK_F33},
  {"F34", XK_F34},
  {"F35", XK_F35},
  {"F4", XK_F4},
  {"F5", XK_F5},
  {"F6", XK_F6},
  {"F7", XK_F7},
  {"F8", XK_F8},
  {"F9", XK_F9},
  {"L1", XK_L1},
  {"L10", XK_L10},
  {"L2", XK_L2},
  {"L3", XK_L3},
  {"L4", XK_L4},
  {"L5", XK_L5},
  {"L6", XK_L6},
  {"L7", XK_L7},
  {"L8", XK_L8},
  {"L9", XK_L9},
  {"R1", XK_R1},
  {"R10", XK_R10},
  {"R11", XK_R11},
  {"R12", XK_R12},
  {"R13", XK_R13},
  {"R14", XK_R14},
  {"R15", XK_R15},
  {"R2", XK_R2},
  {"R3", XK_R3},
  {"R4", XK_R4},
  {"R5", XK_R5},
  {"R6", XK_R6},
  {"R7", XK_R7},
  {"R8", XK_R8},
  {"R9", XK_R9},
  
  {0, 0}
};

Trans st_initial [] = {
        { KEY, f_ascii, st_initial },
        { DIGIT, f_ascii, st_initial },
	{ CONTROL, f_nothing, st_initial },
	{ CONTROL_AND_KEY, f_ctr, st_initial },
	{ CONTROL_AND_X, f_nothing, st_control_x },
	{ ESCAPE, f_nothing, st_escape },
	{ SPECIAL, f_special, st_initial },
	{ 0, 0, 0 }
};

Trans st_control_x [] = {
	{ KEY, f_ctrx, st_initial },
	{ DIGIT, f_ctrx, st_initial },
	{ CONTROL, f_nothing, st_control_x },
	{ CONTROL_AND_KEY, f_ctrx, st_initial },
	{ CONTROL_AND_X, f_ctrx, st_initial },
	{ ESCAPE, f_ctrx, st_initial },
	{ SPECIAL, f_special, st_control_x },
	{ 0, 0, 0 }
};

Trans st_escape [] = {
	{ KEY, f_escape, st_initial },
	{ DIGIT, f_esc_first_digit, st_repeat },
	{ CONTROL, f_nothing, st_escape },
	{ CONTROL_AND_KEY, f_escape, st_initial },
	{ CONTROL_AND_X, f_escape, st_initial },
	{ ESCAPE, f_escape, st_initial },
	{ SPECIAL, f_special, st_escape },
	{ 0, 0, 0 }
};

Trans st_repeat [] = {
        { KEY, f_ascii, st_initial },
	{ DIGIT, f_esc_digit, st_repeat },
	{ CONTROL, f_nothing, st_initial },
	{ CONTROL_AND_KEY, f_ctr, st_initial },
	{ CONTROL_AND_X, f_nothing, st_control_x },
	{ ESCAPE, f_escape, st_escape },
	{ SPECIAL, f_special, st_initial },
	{ 0, 0, 0 }
};

int 	repeat_num = 0;	/* Pour les repetitions		*/
static deletewindow = False;

static KeySym	ksym;
XComposeStatus	compose;
static int 	n_bytes;

int initial_stat_return;

/*
**	Function name : automate
**
**	Description : Un petit automate, pour s'amuser...
**		Il a trois etats possibles :
**		- initial ( etat par default )
**		- controle_x ( sequence Ctr X ... )
**		- escape ( Avec en plus le "repeat" )
**	Input : 
**	Ouput :
*/
ST *automate ( text, event, current_st )
    Text *text;
    XKeyEvent *event;
    ST *current_st;
{
    char buf [32];
    int i;
    InfosKey infos;
    
    bzero (buf, 32);
    n_bytes = XLookupString ( event, buf, 32, &ksym, &compose );
#ifdef DEBUG
    printf ( "n_bytes = %d %X\n", n_bytes, ksym );
#endif
    (void) GetInfosKey ( text, ksym, buf [0], &infos );

    deletewindow = False;
    initial_stat_return = False;
    
    for ( i=0; current_st -> trans [i].type != 0; i++ ){
	if ( current_st -> trans [i].type == infos.type ) {
	    if ( current_st -> trans [i].fnt == 0 || current_st -> trans [i].dest_stat == 0 ) {
		(void) fprintf ( stderr, "Error trans = %d\n", i );
		if ( deletewindow == False )
		  return ( (ST *) current_st );
		else
		  return ( (ST *) - 1 );	
	    }
	    current_st -> trans [i].fnt ( text, &infos );
	    if ( deletewindow == False ) {
		if ( initial_stat_return == True ) {
		    initial_stat_return = False;
		  return ( ( ST * ) -2 );
		}
		else
		  return ( ( ST * ) current_st -> trans [i].dest_stat );
	    }
	    else {
	      if ( initial_stat_return == True ) {
		/* Ya une nouvelle fenetre avec
		   changement de contexte */
		return ( ( ST * ) -2 );
	      }
	      else
		return ( (ST *) -1 );	
	    }
	}
    }
/*    (void) fprintf ( stderr, "Brrr... Automate error\n" ); */
    return ( (ST *) st_initial );
}


/*
**	Function name : GetInfosKey
**
**	Description : GetInfosKey remplie la structure InfosKey
**		en fonction du symbol relatif a l'evenement
**		touche enfoncee.
**		On cherche a savoir dans un premier temps si la
**		touche est de type controle, escape, ou fleche.
**	Input : Le symbole, le caractere, les infos.
**	Ouput :
*/
static void GetInfosKey ( text, ksym, c, infos )
    Text *text;
    KeySym ksym;
    int c;
    InfosKey *infos;
{
    switch ( ksym ) {
    case XK_Control_L :
    case XK_Control_R :
      infos -> type = CONTROL;
      infos -> ch = 0;
      ClearMessageWindow ( text -> mwin );
      break;
    case XK_Escape:
    case XK_Meta_L:
    case XK_Meta_R:
      infos -> type = ESCAPE;
      infos -> ch = c;
      ClearMessageWindow ( text -> mwin );
      break;
    default:
      if ((infos -> ch = XK_ksym_to_code(ksym)) != 0) {
	infos -> type = KEY;
	ClearMessageWindow ( text -> mwin );
      }
      else
	(void) GetInfosAsciiKey ( text, c, infos );
    }
}


/*
**	Function name : GetInfosAsciiKey
**
**	Description : GetInfosAsciiKey remplie la structure InfosKey
**		dans les cas suivants:
**		- Sequence Controle X
**		- Sequence Controle + Key
**		- Return, tab, delete, backspace space
**		- Caractere imprimable.
**	Input : Le symbole, le caractere et les infos.
**	Ouput :
*/
static void GetInfosAsciiKey ( text, c, infos )
    Text *text;
    int c;
    InfosKey *infos;
{
    
  if ( c >= 0 && c <= CtrZ && n_bytes != 0 ) {
    switch ( c ) {
	case CtrX	:
	  infos -> type = CONTROL_AND_X;
	  break;
	case RETURN 	:
	case LINEFEED	:
	case TAB 	:
	case BACKSPACE	:
	  infos -> type = KEY;
	  break;
	default:
	  infos -> type = CONTROL_AND_KEY;
	  break;
	}
	ClearMessageWindow ( text -> mwin );
	infos -> ch = c;
	return;
    }
    else if ( n_bytes == 1 ) {
	infos -> ch = c;
	infos -> type = ((c >= '0') && (c <= '9')) ? DIGIT : KEY;
    }
    else {
	infos -> type = SPECIAL; infos -> ch = 0;
    }
}

/*
**	Function name : f_ascii
**
**	Description : Traitement des caracteres ascii
**	Input : Le infos.
**	Ouput :
*/
static void f_ascii ( text, infos )
    Text *text;
    InfosKey *infos;	
{
    int type;
    void (* func) ();
    char *msg;

    type = text -> current_mode -> key [(int) infos -> ch].type;
    func = text -> current_mode -> key [(int) infos -> ch].func;

    last_key = infos -> ch;
    
    switch ( infos -> ch ) {
    case TAB :
    case DELETE:
    case BACKSPACE:
    case LINEFEED:
    case RETURN:
      if ( (type == BUILTIN_FUNC) && (func != 0 )) {
	  int repeat = repeat_num;
	  
	  (func) ( text );
	  if ( InsideMacro ( text ) == True )
	    LoadMacro ( text, (void(*)()) func, (void *) 0, repeat );
      }
      else {
	/* Pour l'interpreteur */
	StorePosition ( text );
	if ( (type == EXTERN_FUNC) && (func != 0 )) {
	  int nw = text->win_id;
	  
/*	  CheckModifAndFilename ( text ); */
	  if ( InsideMacro ( text ) == True )
	    LoadMacro ( text, (FCT(void,(*),(void *)))ie_call_function,
		       (void*) func, repeat_num );
	  msg = (char *) ie_call_function ( text, (char*) func , 0, 0) ;
	  text = update_cwd(nw);
	  if ( msg != 0 ) {
	    if ( InsideMacro ( text ) == True )
	      ResetMacro ();
	    DisplayMessage ( text -> mwin, msg );
	  }
	}
	ie_redisplay( text );
      }
      break;
    default :
      {
	if ( (type == BUILTIN_FUNC) && (func != 0 )) {
	  int repeat = repeat_num;
	  
	  (func) ( text );
	  if ( InsideMacro ( text ) == True )
	    LoadMacro ( text, func, (void *)infos->ch, repeat );
	}
	else if ( (type == EXTERN_FUNC) && (func != 0 )) {
	  /* Pour l'interpreteur */
	  StorePosition ( text );
	  if ( (type == EXTERN_FUNC) && (func != 0 )) {
	    int nw = text->win_id;
	    
/*	    CheckModifAndFilename ( text ); */
	    if ( InsideMacro ( text ) == True )
	      LoadMacro ( text, (FCT(void,(*),(void *)))ie_call_function,
			 (void*) func, repeat_num );
	    msg = (char *) ie_call_function ( text, (char *) func , 0, 0);
	    text = update_cwd(nw);
	    if ( msg != 0 ) {
	      if (InsideMacro (text ) == True )
		ResetMacro ();
	      DisplayMessage ( text -> mwin, msg );
	    }
	  }
	  ie_redisplay ( text );
	}
	else if ( ( infos -> ch < 256 /* pas XK */) && isprint ( infos -> ch ) ) {
	  int repeat = repeat_num;
	  
	  f_impc ( text, (int) infos -> ch );
	  if ( InsideMacro ( text ) == True )
	    LoadMacro ( text, (void(*)()) f_impc, (void *) infos -> ch, repeat );
	  SetTextModif ( text );	
	}
      }
      break;
    }
    repeat_num = 0;
}

/*
**	Function name : f_ctrx
**
**	Description : Traitement des sequences ^X ^key
**	Input : Les infos. 
**	Ouput :
*/
static void f_ctrx ( text, infos )
    Text *text;
    InfosKey *infos;
{
    int type;
    void (* func) ();
    char *msg;

    type = text -> current_mode -> ctrX_key [(int) infos -> ch].type;
    func = text -> current_mode -> ctrX_key [(int) infos -> ch].func;

    if ( InsideMacro ( text ) == True ) {
	if ( ((int) func == (int) PlayMacro)
	    || ((int) func == (int) StartMacro) ) {
	  (void) DisplayWMessage ( "A coffee will be useful for you", "Macro :", True );
	  ResetMacro ();
	  return;
	}
    }
    
    last_key = infos -> ch;
    
    if ( (type == BUILTIN_FUNC) && (func != 0) ) {
      int repeat = repeat_num;
      
      (func) ( text );
      if ( (InsideMacro ( text ) == True) &&  ((int) func != (int) StartMacro) )
	LoadMacro ( text, (void(*)()) func, (void *) infos -> ch, repeat );
    }
    else {
	/* Pour l'interpreteur */
	StorePosition ( text );
	if ( (type == EXTERN_FUNC) && (func != 0 )) {
	    int nw = text->win_id;
	    
/*	    CheckModifAndFilename ( text ); */
	    if ( InsideMacro ( text ) == True )
	      LoadMacro ( text, (FCT(void,(*),(void *)))ie_call_function,
			 (void*) func, repeat_num );
	    msg = (char *) ie_call_function ( text, (char *) func , 0, 0) ;
	    text = update_cwd(nw);
	    if ( msg != 0 ) {
	      if (InsideMacro (text ) == True )
		ResetMacro ();
	      DisplayMessage ( text -> mwin, msg );
	    }
	}
	ie_redisplay ( text );
    }
}

/*
**	Function name : f_ctr
**
**	Description : Traitement des sequences ^key
**	Input : Les infos.
**	Ouput :
*/
static void f_ctr ( text, infos )
    Text *text;
    InfosKey *infos;
{
    int type;
    void (* func) ();
    char *msg;

    type = text -> current_mode -> key [(int) infos -> ch].type;
    func = text -> current_mode -> key [(int) infos -> ch].func;

    if ( InsideMacro ( text ) == True ) {
	if ( (int) func == (int) PlayMacro ) {
	    klaxon ();
	    return;
	}
    }

    last_key = infos -> ch;
    
    if ( (type == BUILTIN_FUNC) && (func != 0) ) {
      int repeat = repeat_num;
      
      (func) ( text );
      if ( InsideMacro ( text ) == True  )
	LoadMacro ( text, (void (*)()) func, (void *) 0, repeat );
    }
    else {
	/* Pour l'interpreteur */
      StorePosition ( text );
      if ( (type == EXTERN_FUNC) && (func != 0 )) {
	int nw = text->win_id;
	
/*	CheckModifAndFilename ( text ); */
	if ( InsideMacro ( text ) == True )
	  LoadMacro ( text, (FCT(void,(*),(void *)))ie_call_function,
		     (void*) func, repeat_num );
	msg = (char *) ie_call_function ( text, (char *) func , 0, 0) ;
	text = update_cwd(nw);
	if ( msg != 0 ) {
	  if (InsideMacro (text ) == True )
	    ResetMacro ();
	  DisplayMessage ( text -> mwin, msg );
	}
      }
      ie_redisplay ( text );
    }
}

/*
**	Function name : f_escape
**
**	Description : Traitement des sequences escape.
**	Input : 
**	Ouput :
*/

static void f_escape ( text, infos )
    Text *text;
    InfosKey	*infos;
{
  int type;
  void (* func) ();
  char *msg;
  
  type = text -> current_mode -> esc_key [(int) infos -> ch].type;
  func = text -> current_mode -> esc_key [(int) infos -> ch].func;
  
  last_key = infos -> ch;
  
  if ( (type == BUILTIN_FUNC) && (func != 0) ) {
    int repeat = repeat_num;
    
    (func) ( text );
    if ( InsideMacro ( text ) == True )
      LoadMacro ( text, (void(*)()) func, (void *) infos -> ch, repeat );
  }	  
  else {
    /* Pour l'interpreteur */
    StorePosition ( text );
    if ( (type == EXTERN_FUNC) && (func != 0 )) {
      int nw = text->win_id;
      
/*      CheckModifAndFilename ( text ); */
      if ( InsideMacro ( text ) == True )
	LoadMacro ( text, (FCT(void,(*),(void *)))ie_call_function,
		   (void*) func, repeat_num );
      msg = (char *) ie_call_function ( text, (char *) func , 0, 0) ;
      text = update_cwd(nw);
      if ( msg != 0 ) {
	if (InsideMacro (text ) == True )
	  ResetMacro ();
	DisplayMessage ( text -> mwin, msg );
      }
    }
    ie_redisplay ( text );
  }
}

/*
**	Function name : f_first_digit, f_digit
**
**	Description : Traitement des sequences escape num
**	Input : 
**	Ouput :
*/

static void f_esc_first_digit ( text, infos )
    Text *text;
    InfosKey	*infos;
{
  repeat_num = 0;
  
  TextCursorOff ( text );
  GetDigit ( text, infos );
  TextCursorOn ( text );
}

static void f_esc_digit ( text, infos )
    Text *text;
    InfosKey	*infos;
{
  TextCursorOff ( text );
  GetDigit ( text, infos );
  TextCursorOn ( text );
}

/*
**	Function name : GetDigit
**
**	Description : Traitement la sequence escape num
**	Input : Les infos.
**	Ouput :
*/
static void GetDigit ( text, infos )
    Text *text;
    InfosKey *infos;
{
  char str [32];
  
  repeat_num = repeat_num * 10 + infos -> ch - '0';	
  if ( repeat_num > MAXREPEAT ) {
    DisplayMessage ( text -> mwin, "Too much...bye" );
    repeat_num = 0;
    return;
  }
  
  (void) sprintf ( str,"Repeat : %d", repeat_num );
  DisplayMessage ( text -> mwin,  str );
}


/*
**	Function name : DeleteCurrentWIndow
**
**	Description : 
**	Input : 
**	Ouput :
*/
void DeleteCurrentWindow ( text )
    Text *text;
{
    EdWin **t;

    if ( QLength ( dpy ) != 0 ) {
	XSync ( dpy, False );
	return;
    }

    for ( t = TWin; t < TWin + MAXWIN; t++ ) {
	if ( *t == 0 ) continue;
	if ( text == (*t) -> text ) {
	    if ( DeleteWindow ( text ) == 0 )
	      deletewindow = True;
	    break;
	}
    }
}

/*
**	Function name : GotoLine
**
**	Description : 
**	Input : 
**	Ouput :
*/
void GotoLine ( text )
    Text *text;
{
    char *str;
    char c = '\007';
    int n;
    
    TextCursorOff ( text );
    str = (char *) GetString ( text, "Goto Line : ", 0 );
    if ( (str == 0) || (strncmp (str, &c, 1 ) == 0) || ((n = atoi ( str )) == 0) )
      DisplayMessage ( text -> mwin, "Abort" );
    else {
	StorePosition(text);
	GotoLineNumber ( text, n );
	SetPosition(text);
	UpdatePage(text);
    }
    TextCursorOn ( text );
    repeat_num = 0;
}

/*
**	Function name : AbortCurrentCmd
**
**	Description : 
**	Input : 
**	Ouput :
*/
void AbortCurrentCmd ( text )
    Text *text;
{
    DisplayMessage ( text -> mwin, "Abort" );
    repeat_num = 0;
    ResetSearchString ();
    (void) UnmapWarningBox ();
}


/*
**	Function name : KillLines
**
**	Description : 
**	Input : 
**	Ouput :
*/
void KillLines ( text )
    Text *text;
{
    if ( repeat_num == 0 ) repeat_num++;
    Control_K ( text, repeat_num );
    repeat_num = 0;
}

/*
**	Function name : PlayMacro
**
**	Description : 
**	Input : 
**	Ouput :
*/
void PlayMacro ( text )
    Text *text;
{
    int n = ( repeat_num == 0 ) ? 1 : repeat_num;
    
    repeat_num = 0;
    RunMacro ( text, n );
}

static void f_nothing ( text, infos )
    Text *text;
    InfosKey *infos;    
{
/*    (void) fprintf ( stderr, "Nothing\n" ); */
}

static void f_special ( text, infos )
    Text *text;
    InfosKey *infos;
{
/*    (void) fprintf ( stderr, "Special\n" ); */
}



/*
**      Function name : DisplayLineNumber
**
**      Description :
**      Input :
**      Output :
*/
void DisplayLineNumber( text )
    Text *text;
{
    char tmp [64];
    int len, count, nc = GetNcFromLeft(text -> buf);
    char *cl = GetCurrentLine(text->buf, &len);
    int i;

    count = 0;
    i = 0;
    while (nc) {
      if (*(cl+i) == '\t') {
	count += (8 - (count % 8));
	count --;
      }
      count ++;
      i++;
      nc--;
    }
    sprintf ( tmp, "Line : %d column : %d",
	     text -> no_current_line, count );
    DisplayMessage ( text -> mwin, tmp );
}

/*
**      Function name : Xk_Code
**
**      Description : return a pseudo char code (>= 256) or 0
**      Input : the XK name
**      Output : the XK psuedo code (>= 256) or 0
*/
int Xk_Code( name )
    char * name;
{
  int i;
  
  for (i = 0; XKInfos[i].name; i += 1)
    if (! strcasecmp(XKInfos[i].name, name))
      /* appelle XK_ksym_to_code pour gerer les ksym egaux */
      return XK_ksym_to_code(XKInfos[i].xk_code);
    
  return 0;
}

int XK_ksym_to_code(ksym)
    KeySym ksym;
{
  int i;
  
  for (i = 0; XKInfos[i].name; i += 1)
    if (XKInfos[i].xk_code == ksym)
      return i + 256;
    
  return 0;
}
