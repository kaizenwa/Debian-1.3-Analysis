/* ########################################################################

			      main_text.c

   File: main_text.c
   Path: /home/fournigault/c/X11/xcoral-2.31/main_text.c
   Description: 
   Created: Fri Jan 27 11:18:07 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:18:07 MET 1995
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
#include <sys/types.h>
#include <sys/time.h>
#include <signal.h>
#include <string.h>

#ifndef apollo
#ifdef __FreeBSD__
#include <stdlib.h>
#else /* !__FreeBSD__ */
#include <malloc.h>
#endif
#endif

#include "xcoral.h"
#include "main_text.h"
#include "chars_cmds.h"
#include "dial_box.h"
#include "process.h"
#include "text_cursor.h"
#include "page.h"
#include "browser_init.h"

static TextResources	tr;
extern char *getenv ();
extern void Display3D ();

FCT (static int, GetNbLinesInPage, (Text *text, XFontStruct *font) );
FCT (static void, Set_mode_internal, (Text *text, char *mode_name) );

void ie_WR_delete ( text ) Text *text; { f_delete ( text ); }

/*
**	Function name : InitTextRes
**
**	Description : Initialisations de resources communes
**		aux fenetres de texte : la fonte et les couleurs.
**
**	Input : Le display, la fonte, les couleur du devant, du fond,
**		du top_shadow et du buttom_shadow.
**	Ouput :
*/
void InitTextRes ( display, font, fg, bg, ts, bs )
    Display *display;
    XFontStruct	*font;
    unsigned long fg, bg, ts, bs;
{
    XGCValues	gcv;
    GC		gc;
    unsigned long 	gcm;
    
    gc = DefaultGC ( display, DefaultScreen ( display ) );
    
    tr.top_sh = ts;
    tr.bot_sh = bs;

    gcm = 0;
    gcm |= GCForeground;	gcv.foreground = fg;
    gcm |= GCBackground;	gcv.background = bg;
    gcm |= GCFont;		gcv.font = font -> fid;
    tr.cgc = XCreateGC ( display, DefaultRootWindow ( display ), gcm, &gcv );

    gcm = 0;
    gcm |= GCFunction;	gcv.function = GXxor;
    gcm |= GCPlaneMask;	gcv.plane_mask = fg ^ bg;
    gcm |= GCForeground;	gcv.foreground = fg ^ bg;
    gcm |= GCBackground;	gcv.background = bg;
    tr.igc = XCreateGC ( display, DefaultRootWindow ( display ), gcm, &gcv );
    tr.font = font;
    tr.fg = fg;
    tr.bg = bg;
    
#ifdef NOTDEF
    XCopyGC ( display, gc, (~0), tr.cgc );
    XCopyGC ( display, gc, (~0), tr.igc );
    
    gcm = 0;
    gcm |= GCForeground;	gcv.foreground = fg;
    gcm |= GCBackground;	gcv.background = bg;
    gcm |= GCFont;		gcv.font = font -> fid;
    
    XChangeGC ( display, tr.cgc, gcm, &gcv );
    
    gcm = 0;
    gcm |= GCFunction;	gcv.function = GXxor;
    gcm |= GCPlaneMask;	gcv.plane_mask = fg ^ bg;
    gcm |= GCForeground;	gcv.foreground = fg ^ bg;
    gcm |= GCBackground;	gcv.background = bg;
    
    XChangeGC ( display, tr.igc, gcm, &gcv );
#endif
}


/*
**	Function name : MakeTextWindow
**
**	Description : Creation d'un fenetre de texte.
**		
**	Input : Le display, la fenetre parent, la position par rapport
**		a celle-ci.
**	Ouput : La structure Text.
*/
Text *MakeTextWindow ( display, parent, x, y )
    Display	*display;
    Window	parent;
    int x, y;
{
    Text *text;
    XGCValues	gcv;
    int i;
    
    text = ( Text * ) malloc ( sizeof ( Text ));
    
    text -> width_relief = W_RELIEF;
    text -> w_parent = parent;
    
    if ( DefaultDepth ( display, DefaultScreen ( display )) == 1 ) 
      text -> width_relief = 1;	       
    
    text -> window = XCreateSimpleWindow (display, parent, x,
					  y, DEFAULT_SIZE, DEFAULT_SIZE, 0, tr.fg, tr.bg );
    
    XSelectInput ( display, text -> window,
		  ExposureMask | KeyPressMask | VisibilityChangeMask |
		  KeyReleaseMask | ButtonPress | ButtonRelease );
    
    gcv.foreground = tr.fg;
    gcv.background = tr.bg;
    gcv.font = tr.font -> fid;

    text -> Cgc = XCreateGC ( display, DefaultRootWindow ( display ),
	GCForeground | GCBackground | GCFont, &gcv );
    /*XCopyGC ( display, tr.cgc, (~0), text -> Cgc );*/
    
    text -> Igc = tr.igc;
    text -> top_sh = tr.top_sh;
    text -> bot_sh = tr.bot_sh;
    text -> lines_in_buf = 1;
    text -> no_current_line = 1;
    text -> n1 = 0;
    
    text -> fg = tr.fg;
    text -> bg = tr.bg;
    
    text -> x_or = MARGE;
    text -> y_or = MARGE;
    
    text -> x_pos = text -> x_or;
    text -> y_pos = text -> y_or;
    
    text -> visible = 0;
    
    SetFontText ( display, text, tr.font );
    
    text -> cursor_stat = OFF;
    text -> cursor_width = text -> char_width_ave;
    text -> cursor_height = text -> font_height;
    text -> sl = 0;
    *text -> filename = 0;
    text -> stat = 0;
    text -> modif = False;
    text -> mouse_in = False;
    
    text -> tab_width = TAB_WIDTH;

    for (i=0;i<256;i++) {
      text -> page.wline [i] = 0;
      text -> page.sline [i] = 0;
    }
    text -> page.hs_delta = 0;
    
    text -> markline = text -> markpos = 0;
    
    text -> udi = 0;
    text -> u_index = 0;
    text -> u_todo = True;
    text -> current_mode = (Mode *) GetMode ( "default" );
    text -> ie_mark = False;
    
    text -> shell_id = 0;
    text -> from_shell = 0;
    text -> to_shell = 0;
    text -> s_line = 0;
    text -> s_pos = 0;
    text -> current_ce = 0;
    text -> last_ce = 0;
    
    return text;
}

/*
**	Function name : LoadMode
**
**	Description : Charge le mode passe en argument.
**	Input : Le mode demande
**	Ouput :
*/
void LoadMode ( text )
    Text *text;
{
    Mode* tmp;
    char *buf;
    char *str;    
    char c = '\007'; /* ^G */
    extern Mode *m_default;
    Mode *mode = m_default;

    ClearListBox ();
    while(mode) {
	if ((strcmp("input_str",mode -> name) != 0)
	    && (strcmp("default",mode -> name) != 0)
	    && (strcmp("C-mode",mode -> name) != 0)
	    && (strcmp("C++mode",mode -> name) != 0)
	    && (strcmp("Shell",mode -> name) != 0)
	    && (strcmp("shell",mode -> name) != 0)
	    && (strcmp("Latex",mode -> name) != 0)
	    && (strcmp("Html",mode -> name) != 0)
	    && (strcmp("Ada",mode -> name) != 0)
	    && (strcmp("Fortran",mode -> name) != 0)
	    && (strcmp("Perl",mode -> name) != 0)
	    && (strcmp("Edir",mode -> name) != 0))
	    FillList ( mode -> name );
	mode = mode -> next;
    }    
    str = (char *) SelectFromListBox ( "Modes" );
/*    str = (char *) GetStringFromDB ( "Mode name : ", False ); */
    
    if ( (str == 0) || (strncmp(str, &c, 1)) == 0 ) {
	DisplayMessage ( text -> mwin, "Abort" );
	return;
    }
    tmp = (Mode *) GetMode ( str );
    if ( tmp != 0 ) {
	if ( text -> shell_id != 0 )
	  KillShell ( text );
	text -> current_mode = tmp;
    }
    else {
	buf = (char *) malloc ( (unsigned int) strlen (str) + 64 );
	(void) sprintf ( buf, "Warning : %s mode does not exist", str );
	DisplayMessage ( text -> mwin, buf );
	(void) free (buf);
	return;
    }
    SetCtrMode ( text -> mwin, text -> current_mode );
    RefreshWindowMode ( text -> mwin );
    if ( text -> current_mode -> font )
      (void) RefreshWithNewFont ( text, text -> current_mode -> font );
}


/*
**	Function name : NewMode
**
**	Description : Creer un nouveau mode sans suffixes
**         ni fonte.
**	Input : 
**	Ouput :
*/
void NewMode ( text )
    Text *text;
{
    Mode *new_mode;
    char *str;    
    char c = '\007'; /* ^G */
    
    str = (char *) GetStringFromDB ( "New mode name : ", False );
    if ( (str == 0) || (strncmp(str, &c, 1)) == 0 ) {
	DisplayMessage ( text -> mwin, "Abort" );
	return;
    }
    
    new_mode = (Mode *) CreateNewMode (str);
    new_mode -> suffixes = 0;
    new_mode -> font = 0;
}

/*
**	Function name : DeleteText
**
**	Description : Destruction d'une fenetre de texte.
**
**	Input : Le Display , la structure text.
**	Ouput :
*/
void DeleteText ( display, text )
    Display *display;
    Text *text;
{
    XFreeGC ( display, text -> Cgc );
    XDestroyWindow ( display, text -> window );
    if ( text != 0 ) 
      (void) free ( (char *) text );
}

/*
**	Function name : KillText
**
**	Description : Vire le texte d'une fenetre texte et met
**		a jours les infos.
**
**	Input : Le display, le text courant.
**	Ouput :
*/
void KillText ( display, text )
    Display *display;
    Text *text;
{
    /*
     * Affiche la premiere page pour positionner 
     * certaines variables.
     * Reset le buffer
     */
    FirstPage ( text );
    ClearBuffer ( text -> buf );
    
    /* 
     * Mis a jour des infos.
     */
    text -> lines_in_buf = 1; 
    text -> modif = False;
    text -> no_current_line = 1;
    (void) bzero ( (char *) text -> page.wline, 256 );
    (void) bzero ( (char *) text -> page.sline, 256 * 4 );
    text -> page.hs_delta = 0;
    DeleteColorList ( text );
    text -> last_ce = 0;
    text -> current_ce = 0;
    /* 
     * On nettoie bien
     */
    SetScrollLine ( text -> swin, 1 );
    XClearWindow ( display, text -> window );
    Display3D ( display, text -> window, text -> top_sh, text ->bot_sh, 2, 1 ); 
}


/*
**	Function name : TextInBuf
**
**	Description : Comme son nom l'indique
**	Input : Le text courant
**	Ouput : Vrai ou faux
*/
int TextInBuf ( text )
    Text *text;
{
    if(text->buf->l_cur == text->buf->top
       &&text->buf->r_cur == text->buf->bottom)
      return False;
    else
      return True;
/*       
    if ( TopBuf(text->buf) == BottomBuf(text->buf))
      return False;
    else
      return True;
*/      
}


/*
**	Function name : SetFontText
**
**	Description : Positionne la fonte pour le texte courant.
**
**	Input :  Le display, le text courant, la fonte.
**	Ouput :
*/
void SetFontText ( display, text, font )
    Display *display;
    Text *text;
    XFontStruct *font;
{
    int	min_width;

    text -> font = font;
    text -> font_height = (font -> max_bounds.ascent) + (font ->max_bounds.descent);
    text -> cursor_height = text -> font_height;
    
    min_width = font -> min_bounds.width;
    if (min_width == 0)
	min_width = font -> max_bounds.width;
    text -> cursor_width = ( min_width + font -> max_bounds.width ) / 2;
    text -> char_width_ave = ( min_width + font -> max_bounds.width ) / 2;
    text -> blanc_width = XTextWidth ( font, " ", 1 );
    
    XSetFont ( display, text -> Cgc, font -> fid );
}


/*
**	Function name : LoadFont
**
**	Description : Comme son nom l'indique.
**
**	Input : Le display, le nom de la fonte
**	Ouput : la structure
*/
XFontStruct *LoadFont ( dpy, str )
    Display *dpy;
    char    *str;
{
    XFontStruct     *font;
    extern void exit ();
    
    if ((font = XLoadQueryFont ( dpy, str )) == 0 ) {
	(void) fprintf ( stderr, "Fontname error : %s\n", str );
	if (( font = XLoadQueryFont ( dpy, "fixed" )) == 0 ) {
	    (void) fprintf ( stderr, "Can't load font : fixed\n" );
	    (void) exit (1);
	}
	( void ) fprintf ( stderr, "Use font : fixed\n" );
    }
    return font;
}


/*
**	Function name : ChangeTextFont
**
**	Description : Change la fonte courante.
**
**	Input : Le display, le text courant, le nom de la nouvelle fonte.
**	Ouput :
*/
void ChangeFont ( dpy, text, f )
    Display *dpy;
    Text *text;
    char *f;
{
    XFontStruct *font;
    
    TextCursorOff ( text );
    font = LoadFont ( dpy, f );
    (void) RefreshWithNewFont ( text, font );
}


/*
**	Function name : RefreshWithNewFont
**
**	Description : 
**	Input : 
**	Ouput :
*/
void RefreshWithNewFont ( text, font )
    Text *text;
    XFontStruct *font;
{
    extern Display *dpy;
    
    int i, n;
    XWindowAttributes att;
    
    SetFontText ( dpy, text, font );
    
    /* La fenetre est-elle deja mappee */
    XGetWindowAttributes ( dpy, text -> window, &att );
    if ( att.map_state == IsUnmapped )
      return;
    
    i = GetNbLinesInPage ( text, font );
    SetScrollFont ( text -> swin, font );
    SetScrollLinePage ( text -> swin, i ); 
    SetLineInPage ( text, i );
    SetScrollBarSize ( dpy, text -> swin );
    
    TextCursorOff ( text );
    ClearPage ( text );
    
    if ( text -> n1 < ( text -> lines_in_page - 1 )) {
	text -> n2 = text -> lines_in_page - text -> n1 - 1;
    }
    else {
	text -> n1 = text -> lines_in_page / 2;
	text -> n2 = text -> lines_in_page - text -> n1 - 1;
    }
    
    SetAndDisplayPage ( text );
    n = text -> no_current_line - text -> n1 - 1;
    if ( n != 0 )
      (void) MoveScrollBar ( dpy,  text -> swin, CURRENT, n );
    else
      (void) MoveScrollBar ( dpy, text -> swin, FIRST, 0 );
    TextCursorOn ( text );
}

/*
**	Function name : GetNbLinesInPage
**
**	Description : Retourne le nombre de lignes dans la page
**		courante pour une fonte donnee
**
**	Input : Le text courant, la fonte.
**	Ouput : le nombre de lignes.
*/
static int GetNbLinesInPage ( text, font )
    Text *text;
    XFontStruct *font;
{
    return ( (text -> height - (2*MARGE)) / 
	    (font -> max_bounds.ascent + font -> max_bounds.descent));
}


/*
**	Function name : MouseIn
**
**	Description : La pointeur est dans le text courant.
**
**	Input : le text courant
**	Ouput :
*/
void MouseIn ( text )
    Text *text;
{
#ifdef DEBUG
    (void) fprintf ( stderr, "Mouse in\n" );
#endif
    text -> mouse_in = True;
}


/*
**	Function name : MouseOut
**
**	Description : Le pointeur n'est plus dans le text courant
**
**	Input : Le text courant.
**	Ouput :
*/
void MouseOut ( text )
    Text *text;
{
#ifdef DEBUG
    (void) fprintf ( stderr, "Mouse out\n" );
#endif
    text -> mouse_in = False;
}


/*
**	Function name : ShowWindowText
**
**	Description : Affichage de la fenetre de texte.
**
**	Input : Le display, le text courant, la geometrie.
**	Ouput :
*/
void ShowWindowText ( display, text,width, height )
    Display	*display;
    Text	*text;
    int	width, height;
{
    int x;
#ifdef DEBUG
    (void) fprintf ( stderr, "ShowWindowText width = %d height = %d\n",
		    width, height );
#endif
    x = height - ( 2 * MARGE );
    text -> lines_in_page = (x / text -> font_height);
    text -> n2 = ( text -> lines_in_page - 1 ) - text -> n1;
    text -> width = width;
    text -> height = height;
    
    StorePosition ( text );
    
#ifdef DEBUG
    (void) fprintf ( stderr, "ShowWindowText text-height = %d\n",
		    text -> height );
    (void) fprintf ( stderr, "lineinpage = %d\n", text -> lines_in_page );
#endif
    XResizeWindow ( display, text -> window, text -> width, text -> height );
    XMapWindow ( display, text -> window );
}


/*
**	Function name : SetTextSave
**
**	Description : Mise a jour des infos apres une sauvegarde
**		du buffer courant,
**	Input : Le text
**	Ouput :
*/
void SetTextSave ( text )
    Text *text;
{
    text -> modif = False;
    if ( text -> mwin == 0 )
      return;
    text -> mwin -> stat = False;
    RefreshWindowStatBuf ( text -> mwin );
    (void) ResetUndo ( text );
}


/*
**	Function name : SetTextModif
**
**	Description : Mise a jour des infos apres une modification
**		du buffer courant.
**	Input : Le text
**	Ouput :
*/
void SetTextModif ( text) 
    Text *text;
{
    text -> modif = True;
    
    if ( text -> mwin == 0 )
      return;
    if ( text -> mwin -> stat != True ) {
	text -> mwin -> stat = True;
	RefreshWindowStatBuf ( text -> mwin );
    }
}

/*
**	Function name : Set_mode_internal
**
**	Description :
**	Input :
**	Output :
*/
static void Set_mode_internal(text, mode_name)
    Text *text;
    char *mode_name;
{
    if ( strcmp(text -> current_mode -> name, mode_name) != 0 ) {
	if ( text -> shell_id != 0 )
	  KillShell ( text );
	
	text -> current_mode = (Mode *) GetMode (mode_name);
	text -> mwin -> mode = text -> current_mode;
	
	RefreshWindowMode ( text -> mwin );
	
	if ( text -> current_mode -> font != text -> font )
	  (void) RefreshWithNewFont ( text, text -> current_mode -> font );
    }
}


/*
**	Function name : SetDefaultMode
**
**	Description :
**	Input : 
**	Ouput :
*/
void SetDefaultMode ( text )
    Text *text;
{
    Set_mode_internal(text, "default");
}

/*
**	Function name : SetLatexMode
**
**	Description :
**	Input :
**	Output :
*/
void SetLatexMode(text)
    Text *text;
{
    Set_mode_internal(text, "Latex");
}

/*
**	Function name : SetHtmlMode
**
**	Description :
**	Input :
**	Output :
*/
void SetHtmlMode(text)
    Text *text;
{
    Set_mode_internal(text, "Html");
}

/*
**	Function name : SetAdaMode
**
**	Description :
**	Input :
**	Output :
*/
void SetAdaMode(text)
    Text *text;
{
    Set_mode_internal(text, "Ada");
}

/*
**	Function name : SetPerlMode
**
**	Description :
**	Input :
**	Output :
*/
void SetPerlMode(text)
    Text *text;
{
    Set_mode_internal(text, "Perl");
}

/*
**	Function name : SetFortranMode
**
**	Description :
**	Input :
**	Output :
*/
void SetFortranMode(text)
    Text *text;
{
    Set_mode_internal(text, "Fortran");
}

/*
**	Function name : SetshellMode
**
**	Description :
**	Input :
**	Output :
*/
void SetshellMode(text)
    Text *text;
{
    Set_mode_internal(text, "shell");
}

/*
**	Function name : SetEdirMode
**
**	Description :
**	Input :
**	Output :
*/
void SetEdirMode(text)
    Text *text;
{
    Set_mode_internal(text, "Edir");
}


/*
**	Function name : SetCMode
**
**	Description : 
**	Input : 
**	Ouput :
*/
void SetCMode ( text )
    Text *text;
{
    if ( strcmp(text -> current_mode -> name, "C-mode") != 0 ) {
	if ( text -> shell_id != 0 )
	  KillShell ( text );
	
	text -> current_mode = (Mode *) GetMode ("C-mode");
	text -> mwin -> mode = text -> current_mode;
	RefreshWindowMode ( text -> mwin );
	if ( (text -> current_mode -> font != text -> font)
	    && (text -> current_mode -> font != 0) )
	  (void) RefreshWithNewFont ( text, text -> current_mode -> font );
    }
    if ( strcmp( "C-mode", (char *) GetBrowserModeName()) != 0 ) {
	SetBrowserMode ( text -> current_mode );
	RefreshBrowserControl ();
    }
}


/*
**	Function name : SetCCMode
**
**	Description : 
**	Input : 
**	Ouput :
*/
void SetCCMode ( text )
    Text *text; 
{
    if ( strcmp(text -> current_mode -> name, "C++mode") != 0 ) {
	if ( text -> shell_id != 0 )
	  KillShell ( text );
	
	text -> current_mode = (Mode *) GetMode ("C++mode");
	text -> mwin -> mode = text -> current_mode;
	RefreshWindowMode ( text -> mwin );
	if ( (text -> current_mode -> font != text -> font) 
	    && (text -> current_mode -> font != 0) )
	  (void) RefreshWithNewFont ( text, text -> current_mode -> font );
    }
    if ( strcmp( "C++mode", (char *) GetBrowserModeName()) != 0 ) {
	SetBrowserMode ( text -> current_mode );
	RefreshBrowserControl ();
    }
}


/*
**	Function name : ChangeDir
**
**	Description : Change de directorie courante.
**	Input : Le text courant.
**	Ouput :
*/
void ChangeDir ( text )
    Text *text;
{
    char *tmp;
    char buf [128];
    int len;
    
    (void) chdir ( text -> current_dir );
    
    /* 
       * Affichage du filename ou de la directorie.
    */
    if ( text -> filename == 0 )
      return;
    len = strlen ( text -> filename );
    if ( strcmp ( text -> filename + ( len - strlen ( "NoName" )), "NoName" ) == 0 ) {
	len = strlen ( text -> current_dir );
	if ( len > 20 ) {
/*	    (void) sprintf ( buf, "Dir : ...%s\n", */
   	    (void) sprintf ( buf, "Dir : ...%s", 
			    (char *) text -> current_dir + ( len - 20) );
	}
	else
/*	  (void) sprintf ( buf, "Dir : %s\n", text -> current_dir ); */
	  (void) sprintf ( buf, "Dir : %s", text -> current_dir );
	DisplayMessage ( text -> mwin, buf  );
	return;
    }
    tmp = strrchr ( text -> filename, '/' );
    if ( tmp != 0 )
      DisplayMessage ( text -> mwin, tmp + 1 );
}


/*
**	Function name : ExposeTextWindow
**
**	Description : Traitement d'un expose event dans une 
**		fenetre de texte.
**	Input : Le display, le text courant, la fenetre exposee.
**	Ouput :
*/
void ExposeTextWindow ( dpy, text, ev )
    Display *dpy;
    Text *text;
    XEvent *ev;
{
    XRectangle rec [2];
    Region region;
    XEvent tmp;
    
    Display3D ( dpy, text -> window,
	       text -> top_sh,
	       text -> bot_sh,
	       text -> width_relief, DOWN );
    
    region = XCreateRegion ();
    
    rec [0].x = ((XExposeEvent *) ev) -> x;
    rec [0].y = ((XExposeEvent *) ev) -> y;
    rec [0].width = ((XExposeEvent *) ev) -> width;
    rec [0].height = ((XExposeEvent *) ev) -> height;
    
    XUnionRectWithRegion ( rec, region, region );
    while ( XCheckTypedWindowEvent ( dpy, text -> window, Expose, &tmp )) {
	rec [0].x = tmp.xexpose.x;
	rec [0].y = tmp.xexpose.y;
	rec [0].width = tmp.xexpose.width;
	rec [0].height = tmp.xexpose.height;
	XUnionRectWithRegion ( rec, region, region );
    }
    XSetRegion ( dpy, text -> Cgc, region );
    ExposePage ( region, text );
    XSetClipMask ( dpy, text -> Cgc, None );
    XDestroyRegion ( region );
    SetCurrentLine ( text );
    if ( text -> mouse_in == True ) 
      TextCursorOn ( text );
    else
      FreezeTextCursor ( text ); 
    return;
}


/*
**	Function name : ConfigTextAndScroll
**
**	Description : 
**	Input : 
**	Ouput :
*/
void ConfigTextAndScroll ( text, width, height, space )
    Text *text;
    int width, height, space;
{
    extern Display *dpy;
    
    int i;
    
    i = ((height - ( 2 * space )) - (2 * text -> y_or)) / text -> font_height;
    
    SetScrollLinePage (  text -> swin, i ); 
    ShowWindowText ( dpy, text, 
		    width - ( 2 * space ) - GetScrollWidth () - 1, height - ( 2 * space ));
    i = text -> no_current_line - text -> n1 - 1;
    RefreshScroll ( dpy,  text -> swin, width - space, height - ( 2 * space ), i);
}





