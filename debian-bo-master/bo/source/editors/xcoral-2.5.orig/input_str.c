/* ########################################################################

			      input_str.c

   File: input_str.c
   Path: /home/fournigault/c/X11/xcoral-2.31/input_str.c
   Description: 
   Created: Fri Jan 27 11:09:50 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:09:51 MET 1995
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
#ifdef __NetBSD__
#include <pwd.h>
#include <dirent.h>
#else
#include <dirent.h>
#include <pwd.h>
#endif
#include <string.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <stdlib.h>
#include <sys/stat.h>

#include "main_text.h"
#include "handle_key.h"
#include "dial_box.h"
#include "ie_func.h"
#include "main_events.h"
#include "warn_box.h"
#include "text_cursor.h"
#include "kill_buf.h"
#include "chars_cmds.h"
#include "page.h"
#include "shadow.h"
#include "input_str.h"

Text *input_str;

ST *input_str_stat;
static int str_status;
static int input_toggle_del = True;
static int input_button = 0;
static int parent_y = 0;
static int p_width = 0;
static int mb_context = CTX_FILE;

extern Display *dpy;
extern Trans st_initial;
extern DBox dial_box;

#define STR_RETURN 1
#define STR_ABORT  2
#define MINIB_OK 3
#define MINIB_CANCEL 4

char *str_buf;
int str_buf_len;
#define STR_BUF_LEN 64

FCT (static void, CleanInputStr, () );
FCT (static int, CommonLength, (char *s, int ns, int r) );
FCT (static void, ConfigInputString, () );
FCT (static char *, ExpandFileName, (char *s, int *n, int *common) );
FCT (static void, ExposeInputString, () );
FCT (static void, InputStrSave, (char *s, int len) );
FCT (static void, InputStrUnmap, () );
FCT (static void, PrintMatchedNames, (char *s, int combien) );

/*
**	Function name : InitInputString
**
**	Description : 
**	Input : 
**	Ouput :
*/
void InitInputString ()
{
    XFontStruct *font;
    int i;
    extern void ie_WR_delete ();
    
    input_str = (Text *) MakeTextWindow ( dpy, DefaultRootWindow(dpy), 0, 0 );
    input_str -> mwin = 0;
    input_str -> swin = 0;
    input_str -> current_mode = (Mode *) GetMode ("input_str");
    input_str -> n1 = 0;
    input_str -> n2 = 0;
    input_str -> lines_in_page = 1;
    input_str -> y_or = 5;

    input_str -> buf = GetBuffer ( (unsigned) SIZEOF_BUFFER );
    input_str -> swin = 0;
    input_str -> mwin = 0;
    (void) strcpy ( input_str -> filename, "NoName" ); 
    
    if ( input_str -> current_mode == 0 )
      (void) fprintf (stderr, "Init mode error\n" );

    font = LoadFont ( dpy, "-adobe-courier-bold-r-normal--*-140-*-*-m-*-iso8859-1" );
    SetFontText ( dpy, input_str, font );
    input_str_stat = (ST *) &st_initial;
    
    XSelectInput ( dpy, input_str -> window, EnterWindowMask | LeaveWindowMask |
		  ExposureMask | KeyPressMask | VisibilityChangeMask |
		      KeyReleaseMask | ButtonPress | ButtonRelease );
   
    for ( i=0; i< 512; i++ ) {
	input_str -> current_mode -> key [i].func = 0;
	input_str -> current_mode -> key [i].type = 0;
	input_str -> current_mode -> ctrX_key [i].func = 0;
	input_str -> current_mode -> ctrX_key [i].type = 0;
	input_str -> current_mode -> esc_key [i].func = 0;
	input_str -> current_mode -> esc_key [i].type = 0;
    }
    
    ie_key_def ( input_str,"input_str" , "^A", "goto_beginning_of_line" );
    ie_key_def ( input_str,"input_str" , "^B", "goto_previous_char" );
    ie_key_def ( input_str,"input_str" , "^D", "delete_char" );
    ie_key_def ( input_str,"input_str" , "^E", "goto_end_of_line" );
    ie_key_def ( input_str,"input_str" , "^F", "goto_next_char" );
    ie_key_def ( input_str,"input_str" , "^G", "str_abort" );
    ie_key_def ( input_str,"input_str" , "^H", "delete" );
    ie_key_def ( input_str,"input_str" , "^K", "kill_line" );
    ie_key_def ( input_str,"input_str" , "^M", "str_return" );
    ie_key_def ( input_str,"input_str" , "^I", "str_tab" );
    ie_key_def ( input_str,"input_str" , "^P", "str_previous" );

    input_str -> current_mode -> key [DELETE].func = ie_WR_delete;
    
    input_str -> current_mode -> key [CtrA].type = BUILTIN_FUNC;
    input_str -> current_mode -> key [CtrB].type = BUILTIN_FUNC;
    input_str -> current_mode -> key [CtrD].type = BUILTIN_FUNC;    
    input_str -> current_mode -> key [CtrE].type = BUILTIN_FUNC;
    input_str -> current_mode -> key [CtrF].type = BUILTIN_FUNC;
    input_str -> current_mode -> key [CtrG].type = BUILTIN_FUNC;    
    input_str -> current_mode -> key [CtrH].type = BUILTIN_FUNC;
    input_str -> current_mode -> key [CtrK].type = BUILTIN_FUNC;
    input_str -> current_mode -> key [CtrM].type = BUILTIN_FUNC;    
    input_str -> current_mode -> key [CtrI].type = BUILTIN_FUNC;
    input_str -> current_mode -> key [CtrP].type = BUILTIN_FUNC;
    input_str -> current_mode -> key [DELETE].type = BUILTIN_FUNC;
    
    str_buf = (char *) malloc ( (unsigned) STR_BUF_LEN );
    
    str_buf_len = STR_BUF_LEN;
}

/*
**	Function name : InputString
**
**	Description : 
**	Input : 
**	Ouput :
*/
char *InputString ( parent, gc, font, prompt, reply )
    Window parent;
    GC gc;
    XFontStruct *font;
    char *prompt;
    int reply;
{
    XEvent ev;
    ST *automate (), *stat;
    XWindowAttributes att;
    int len;
    char *str, *s;
    int lines;
    int nbytes;
    extern XEvent event;
    static char c_g [2];
    
    FreezeMenus ();
    p_width = XTextWidth ( font, prompt, strlen (prompt));
    XReparentWindow ( dpy, input_str -> window, parent, p_width + 10, 1 );
    input_str -> w_parent = parent;
    XGetWindowAttributes ( dpy,parent , &att );
    XResizeWindow ( dpy, input_str -> window, att.width - (p_width + 11), att.height - 2 );
    input_str -> width = att.width - (p_width + 10);
    input_str -> height = att.height - 2;
    input_str -> y_or = ((att.height - input_str -> cursor_height ) / 2) - 1;
    input_str -> y_pos = input_str -> y_or;
    input_str -> x_pos = 10;
    input_str -> x_or = 10;
    parent_y = input_str -> y_or + 2;
    input_str -> width_relief = 0;
    input_button = 0;
    XMapWindow ( dpy, input_str -> window );
    
    for(;;) {
	if ( input_button == MINIB_OK ) {
	    len = 0;
	    str = (char *) GetCurrentLine ( input_str -> buf, &len );
	    if ( (str != 0) && (len != 0)) {
		(void) InputStrSave ( str, len );
		(void) CleanInputStr ();
		(void) InputStrUnmap ();
		UnmapWarningBox ();
		return str_buf;
	    }
	    else {
		(void) InputStrUnmap ();		  
		UnmapWarningBox ();
		return 0;
	    }
	}
	if ( input_button == MINIB_CANCEL ) {
	    (void) CleanInputStr ();
	    (void) InputStrUnmap ();
	    UnmapWarningBox ();
	    c_g [0] = '\007';
	    c_g [1] = '\0';
	    return ((char *) c_g );
	}
	XNextEvent ( dpy, &ev );
	switch ( ev.type ) {
	case Expose:
	  if ( ev.xany.window == parent ) {
	    XDrawImageString ( dpy, parent , gc, 10,
			      parent_y + input_str -> font -> ascent,
			      prompt, strlen (prompt));
	    XPutBackEvent ( dpy, &ev );
	    XNextEvent ( dpy, &event );
	    HandleExpose ();
	  }
	  else if ( ev.xany.window == input_str -> window ) {
	      (void) ExposeInputString ();
	      XDrawImageString ( dpy, parent , gc, 10,
				parent_y + input_str -> font -> ascent,
				prompt, strlen (prompt));
	  }
	  else {
	      XPutBackEvent ( dpy, &ev );
	      XNextEvent ( dpy, &event );
	      HandleExpose ();
	  }
	  break;
	case ConfigureNotify:
	      XPutBackEvent ( dpy, &ev );
	      XNextEvent ( dpy, &event );
	      HandleConfigure ();
	      (void) ConfigInputString ();
	  break;
	case ButtonPress:
	  if ( ev.xany.window != input_str -> window ) {
	      XPutBackEvent ( dpy, &ev );
	      XNextEvent ( dpy, &event );
	      HandleButtonPress ();
	  }
	  switch ( ev.xbutton.button ) {
	  case Button1:
	    if ( ev.xany.window == input_str -> window ) {
		TextCursorOff ( input_str );
		(void) MoveToXYinTextWindow ( input_str, ev.xbutton.x, ev.xbutton.y );
		TextCursorOn ( input_str );
	    }
	    break;
	  case Button2:
	    if ( ev.xany.window == input_str -> window ) {
		if ( ev.xbutton.state == ControlMask ) {
		    s = XFetchBuffer ( dpy, &nbytes, 0 );
		    if ( (s != 0) && (nbytes != 0 ) && (input_toggle_del == False) ) {
			if ( (RightBuf ( input_str -> buf ) + nbytes) > BottomBuf ( input_str -> buf ) ) {
			    nbytes = BottomBuf ( input_str -> buf ) - RightBuf ( input_str -> buf );
			}
			lines = GetNewLine ( RightBuf ( input_str -> buf ), nbytes );
			if ( lines == 0 )
			  DeleteBytesFromCutBuffer ( input_str );
			else 
			  klaxon ();
		    }
		    else
		      klaxon ();
		    input_toggle_del = True;
		}
		else {
		    s = XFetchBuffer ( dpy, &nbytes, 0 );
		    if ((s != 0) && (nbytes != 0 )) {
			lines = GetNewLine ( s, nbytes);
			if ( lines == 0 )
			  GetBytesFromCutBuffer ( input_str );
			else
			  klaxon ();
		    }
		    else
		      klaxon ();
		}
	    }
	    else {
/*		  klaxon (); */
	    }
	    break;
	  case Button3:
	    if ( ev.xany.window == input_str -> window ) {
		TextCursorOff ( input_str );
		XSync ( dpy, False );
		StoreBytesInCutBuffer ( input_str, ev.xbutton.x, ev.xbutton.y );
		input_toggle_del = False;
		TextCursorOn ( input_str );
	    }
	    break;
	  }
	  XDrawImageString ( dpy, parent, gc, 10,
			    parent_y + input_str -> font -> ascent,
			    prompt, strlen (prompt));
	  break;
	case EnterNotify:
	  if ( ev.xany.window == input_str -> window ) {
	      MouseIn ( input_str );
	      UnFreezeTextCursor ( input_str );
	      TextCursorOn ( input_str );
	  }
	  else {
	      XPutBackEvent ( dpy, &ev );
	      XNextEvent ( dpy, &event );
	      HandleEnter ();
	  }
	  XDrawImageString ( dpy, parent , gc, 10,
			    parent_y + input_str -> font -> ascent,
			    prompt, strlen (prompt));
	  break;
	case LeaveNotify:
	  if ( ev.xany.window == input_str -> window ) {
	      MouseOut( input_str );
	      TextCursorOff ( input_str );
	      FreezeTextCursor ( input_str );
	  }
	  else {
	      XPutBackEvent ( dpy, &ev );
	      XNextEvent ( dpy, &event );
	      HandleLeave ();
	  }
	  XDrawImageString ( dpy, parent , gc, 10,
			    parent_y + input_str -> font -> ascent,
			     prompt, strlen (prompt));
	  break;
	case KeyPress:
	  stat = (ST *) automate ( input_str, ( XKeyEvent *) &ev, input_str_stat );
	  if ( (int) stat != -1 )
	    input_str_stat = stat;
	  switch ( str_status ) {
	    case STR_RETURN:
	      len = 0;
	      str = (char *) GetCurrentLine ( input_str -> buf, &len );
	      if ( (str != 0) && (len != 0)) {
		  (void) InputStrSave ( str, len );
		  (void) CleanInputStr ();
		  (void) InputStrUnmap ();
		  UnmapWarningBox ();
		  return str_buf;
	      }
	      else {
		  (void) InputStrUnmap ();		  
		  UnmapWarningBox ();
		  return 0;
	      }
	      break;	      
	    case STR_ABORT:
	      (void) CleanInputStr ();
	      (void) InputStrUnmap ();
	      UnmapWarningBox ();
	      c_g [0] = '\007';
	      c_g [1] = '\0';
	      return ((char *) c_g );
	      break;
            default:
	      len = 0;
	      str = (char *) GetCurrentLine ( input_str -> buf, &len );
	      if ( (len == 1) && ( reply != 0 )) {
		  (void) InputStrSave ( str, len );
		(void) CleanInputStr ();
		(void) InputStrUnmap ();
		UnmapWarningBox ();		
		return str_buf;
	      }
	      break;
	  }
	  break;
	}
    }
}

/*
**	Function name : GetString
**
**	Description : 
**	Input : Le text courant, le prompt et le type de reponse.
**		En fait si 'reply est different de 0, alors on
**		retourne 1 caractere. C'est le cas pour les trucs
**		du genre 'answer [y/n]'.
**	Ouput : La chaine
*/
char *GetString ( text, prompt, reply )
    Text *text;
    char *prompt;
    int reply;
{
    int len = XTextWidth (input_str -> font,
			  prompt, strlen (prompt)) + 10;
    
    /* On verifie qu'on peut afficher le mini-buffer */
    if ( (text -> mwin -> twidth - len) < 100 ) {
        /* 100 pixels, c'est le minimum, 7,8,9 caracteres
	   suivant la fonte */
	return ( (char *) GetStringFromDB ( prompt, reply ));
    }
    else {
        /* Mini-Buffer */
	return ( (char *)
		InputString ( text -> mwin -> mess,
			     text -> mwin -> mess_gc,
			     text -> mwin -> font, prompt, reply ));
    }
}

/*
**	Function name : ConfigInputString
**
**	Description : 
**	Input : 
**	Ouput :
*/
static void ConfigInputString ()
{
    XWindowAttributes att;
    
    XGetWindowAttributes ( dpy, input_str -> w_parent , &att );
    if ( (att.width - (p_width + 11)) != input_str -> width ) {
	XResizeWindow ( dpy, input_str -> window, att.width - (p_width + 11), att.height - 2 );
	input_str -> width = att.width - (p_width + 10);
    }
    XMapWindow ( dpy, input_str -> window );
}

/*
**	Function name : CleanInputStr
**
**	Description : 
**	Input : 
**	Ouput :
*/
static void CleanInputStr ()
{
    ClearBuffer ( input_str -> buf );
    input_str -> lines_in_buf = 1; 
    input_str -> modif = False;
    input_str -> no_current_line = 1;
    (void) bzero ( (char *) input_str -> page.wline, 256 );
    (void) bzero ( (char *) input_str -> page.sline, 256 );
}
/*
**	Function name : EmptyMiniBuffer
**
**	Description :
**	Input :
**	Output :
*/
void EmptyMiniBuffer ()
{
    TextCursorOff ( input_str );
    CleanInputStr ();
    XClearWindow ( dpy, input_str -> window );
    TextCursorOn ( input_str );    
}



/*
**	Function name : InputStrUnmap
**
**	Description : 
**	Input : 
**	Ouput :
*/
static void InputStrUnmap ()
{
    MouseOut( input_str );
    TextCursorOff ( input_str );
    XUnmapWindow ( dpy, input_str -> window );
    XReparentWindow ( dpy, input_str -> window, DefaultRootWindow (dpy), 0, 0 );
    WaitForUnMapped ( input_str -> window );
    input_str_stat = (ST *) &st_initial;
    str_status = 0;
    UnFreezeMenus ();
}

/*
**	Function name : ExposeInputString
**
**	Description : 
**	Input : 
**	Ouput :
*/
static void ExposeInputString ()
{
    Display3D ( dpy, input_str -> window,
	       input_str -> top_sh,
	       input_str -> bot_sh,
	       input_str -> width_relief, DOWN );
    
    SetCurrentLine ( input_str );
    
    if ( input_str -> mouse_in == True ) 
      TextCursorOn ( input_str );
    else
      FreezeTextCursor ( input_str ); 
}


/*
**	Function name : InputStrSave
**
**	Description : 
**	Input : 
**	Ouput :
*/
static void InputStrSave ( s, len )
    char *s;
    int len;
{
    if ( len > (str_buf_len - 1) ) {
	(void) free ( str_buf );
	str_buf = (char *) malloc ( (unsigned) (len + 2));
	(void) strncpy ( str_buf, s, len );
	str_buf [len] = 0;
	str_buf_len = len + 2;
    }
    else {
	(void) strncpy ( str_buf, s, len );
	str_buf [len] = 0;
    }
}

/*
**	Function name : InputStrPrevious
**
**	Description : 
**	Input : 
**	Ouput :
*/
void InputStrPrevious ( text )
    Text *text;
{
    TextCursorOff ( input_str );
    (void) CleanInputStr ();
    XClearWindow ( dpy, input_str -> window );
    InsertNchar ( input_str -> buf, str_buf, strlen(str_buf) );
    input_str -> modif = True;
    SetCurrentLine ( input_str );
    TextCursorOn ( input_str );
}

/*
**	Function name : InputStrReturn
**
**	Description : 
**	Input : 
**	Ouput :
*/
void InputStrReturn ( text )
    Text *text;
{
    str_status = STR_RETURN;
}

/*
**	Function name : InputStrAbort
**
**	Description : 
**	Input : 
**	Ouput :
*/
void InputStrAbort ( text )
    Text *text;
{
    str_status = STR_ABORT;
}

/*
**	Function name : ExpandFileName
**
**	Description : 
**	Input : 
**	Ouput :
*/
static char * ExpandFileName (s, n, common)
    char * s;
    int * n;
    int * common;
{
    DIR * dp;
    struct dirent * entry;
    char * result = 0;
    int rlength = 0;
    char * current_dir;
    
    *n = 0;
    
    {
	char *tmp, *fsdirname, *newdir;
	char *slash;
	
	fsdirname = (char *) GetFsBoxInternalDir();
	slash = strrchr(s, '/');

	if (fsdirname) {
	    /* On est dans le file selecteur */
	    if(slash) {
		if (strlen(s)==1 && *s == '/') /* C'est la racine */
		  ;
		else
		  *slash = 0; /* Pour que s soit une directorie */
		if (*s != '/') {
		    /* on part de fsdirname */
		    newdir = (char *)malloc(strlen(fsdirname)+strlen(s)+2);
		    sprintf(newdir,"%s%s",fsdirname, s);
		    dp = opendir(newdir);
		    current_dir = (char *) stringdup(newdir);
		    free(newdir);
		}
		else {
		  dp = opendir(s);
		  current_dir = (char *) stringdup(s);
		}
		*slash = '/';
		s = slash + 1;
	    }
	    else {
		dp = opendir(fsdirname);
		current_dir = (char *) stringdup(fsdirname);
	    }
	}
	else {
	    if (slash) {
		*slash = 0;
		dp = opendir(s);
		current_dir = (char *) stringdup(s);
		*slash = '/';
		s = slash + 1;
	    }
	    else {
		dp = opendir(".");
		current_dir = (char *) stringdup(".");
	    }
	}
    }
    
    
    if (! dp)
      return 0;
    
    *common = strlen(s);
    
    while ((entry = readdir(dp)))
      if (strstr(entry->d_name, s) == entry->d_name) {
	char * nresult =
	  (char *) malloc((unsigned int) rlength + strlen(entry->d_name) + 1 + 1);
	
	if (! nresult) {
	    (void) closedir (dp);
	    free(current_dir);
	    return result;
	}
	
	bcopy(result, nresult, rlength);
	(void) strcpy(nresult + rlength, entry->d_name);
	rlength += strlen(entry->d_name) + 1;
	*n += 1;
	{
	  struct stat st;
	  char * f = malloc(strlen(current_dir) + 1 + strlen(entry->d_name) + 1);
	  
	  if (f) {
	    sprintf(f, "%s/%s", current_dir, entry->d_name);
	    (void) stat(f, &st);
	    if (S_ISDIR(st.st_mode)) {
	      nresult[rlength - 1] = '/';
	      nresult[rlength++] = 0;
	    }
	    free(f);
	  }
	}
	free(result);
	result = nresult;
    }
    
    (void) closedir (dp);
    free(current_dir);
    return result;
}


/*
**	Function name : CommonLength
**
**	Description : 
**	Input : 
**	Ouput :
*/
static int CommonLength(s, ns, r)
    char * s;
    int ns;
    int r;
{
    int c;
    
    r -= 1;
    
    while ((c = s[++r])) {
	char * p = s;
	int n = ns - 1;
	
	do
	  p += strlen(p) + 1;
	while (n-- && (p[r] == c));
	
	if (n != -1)
	  return r;
    }
    
    return r;
}

/*
**	Function name : PrintMatchedNames
**
**	Description : 
**	Input : 
**	Ouput :
*/
static void PrintMatchedNames(s, combien )
    char * s;
    int combien;
{
    char *tmp;
    char *str = s;
    int len, i;
    
    len = 0;
    for ( i=0; i<combien; i++ ) {
	len += strlen ( s );
	s += strlen ( s ) + 1;
    }
    tmp = (char *) malloc ( (unsigned int) len + combien + 2 );
    bzero ( tmp, len + combien + 2 );
    for ( i=0; i<combien; i++ ) {
	(void) strcat ( tmp, str );
	(void) strcat ( tmp, "\n" );
	str += strlen ( str ) + 1;
    }
    DisplayWMessage ( tmp, " Matched Names", True );
    (void) free ( tmp );
}

/*
**	Function name : InputStrTab
**
**	Description : 
**	Input : 
**	Ouput :
*/
void InputStrTab ( text )
    Text *text;
{
    char *s, *t_str;
    int len, combien;
/*
    if (input_str -> buf -> l_cur == input_str -> buf -> top) {
      klaxon ();
      return;
    }
*/
    s = (char *) GetCurrentLine ( input_str -> buf, &len );
    s[input_str -> buf -> l_cur - input_str -> buf -> top] = 0;
    
    {
      /* recherche le dernier mot */
      char * spacep = strrchr(s, ' ');

      if (spacep)
        s = spacep + 1;
    }
    TextCursorOff ( input_str );
    if (*s == '~') {
      char * expandtilde = (char *) ExpandTildeName (s);

      DeleteNchar(input_str -> buf, strlen(s));
      InsertNchar(input_str -> buf, expandtilde, strlen(expandtilde));
      s = expandtilde;
    }
    if ( mb_context == CTX_EVAL_EXP )
      t_str = (char *) ExpandSmacName ( s, &combien, &len);
    else
      t_str = (char *) ExpandFileName ( s, &combien, &len);
    if ( combien == 0 )
      klaxon ();
    else {
      if ( combien > 1 ) {
	  PrintMatchedNames(t_str, combien );
        /* prend les lettres communes */
	  InsertNchar ( input_str -> buf,
                     t_str + len,
                     CommonLength(t_str, combien, len) - len);
      } 
      else {
        InsertNchar ( input_str -> buf,
                     t_str + len,
                     strlen (t_str) - len);
      }
      free(t_str);
    }
    SetCurrentLine ( input_str ); 
    TextCursorOn ( input_str );
    
}

/*
**	Function name : ExpandTildeName
**
**	Description : 
**	Input : 
**	Ouput :
*/
char *ExpandTildeName ( name )
    char *name;
{
    struct passwd *pwd;
    char *home;
    char *p = name + 1;
    static char  buf [256];
    
    if ( name [1] == '/' || name [1] == '\0') {   /* c'est moi */
	if ( (home = (char *) getenv ( "HOME" )) == 0 )
	  return name;
	(void) strcpy ( buf, home );
	(void) strcat ( buf, name + 1 );
    }
    else { /* c'est un autre */
	int save;
	
	while ( *p && *p != '/' ) p++;
	save = *p;
	*p = 0;
	pwd = getpwnam ( name + 1 );
	*p = save;
	if (  pwd ) {
	    (void) strcpy ( buf, pwd ->  pw_dir);
	    (void) strcat ( buf, p );
	}
	else
	  return name;
    }
    return buf;
}

/*
**	Function name : FillMiniBuffer
**
**	Description : 
**	Input : 
**	Ouput :
*/
void FillMiniBuffer ( str )
    char *str;
{
    if ( (str == 0) || (strlen(str)== 0) )
      return;
    
    TextCursorOff ( input_str );
    (void) CleanInputStr ();
    XClearWindow ( dpy, input_str -> window );
    InsertNchar ( input_str -> buf, str, strlen (str) );
    input_str -> modif = True;
    TextCursorOn ( input_str );
    (void) ExposeInputString ();
}

/*
**	Function name : SetOKButton
**
**	Description : 
**	Input : 
**	Ouput :
*/
void SetOkButton ()
{
    input_button = MINIB_OK;
}

/*
**	Function name : SetCancelButton
**
**	Description : 
**	Input : 
**	Ouput :
*/
void SetCancelButton ()
{
    input_button = MINIB_CANCEL;
}

/*
**	Function name : SetMBcontext`
**
**	Description :
**	Input :
**	Output :
*/
void SetMBcontext ( cont )
    int cont;
{
    mb_context = cont;
}

