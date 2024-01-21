/* ########################################################################

				xcoral.c

   File: xcoral.c
   Path: /home/fournigault/c/X11/xcoral-2.31/xcoral.c
   Description: 
   Created: Fri Jan 27 10:42:00 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 10:42:01 MET 1995
   Last maintained by: Lionel Fournigault

   RCS $Revision: 1.1 $ $State: Exp $
   

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
#include <signal.h>
#include <X11/Xlib.h>
#include <X11/cursorfont.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <sys/param.h>
#if defined(SVR4) || defined(UNIXWARE)
#include <string.h>
extern void bzero();
#else
#include <strings.h>
#endif

#include "options.h"
#include "xcoral.h"
#include "parse.h"
#include "shadow.h"
#include "main_events.h"
#include "browser_init.h"
#include "warn_box.h"
#include "list_box.h"
#include "dial_box.h"
#include "fs_box.h"
#include "man_box.h"
#include "input_str.h"
#include "get_file.h"

Display		*dpy;
EdWin		*TWin [MAXWIN], *edwin; /* La table des fenetres */
XContext	EdContext; /* Pour switcher */

FCT (static void, contHandler, () );
FCT (static void, stopHandler, () );


/*
**	Initialise les resources, les menus, les bouttons etc...
**	Creer la premiere fenetre de texte et entre dans
**	la boucle d'evenements.
*/
void main ( argc, argv ) 
    int argc;
    char **argv;
{
    EdWin *CreateWindow ();
    char pathname [MAXPATHLEN + 2];
    char *msg;
    char tmp[256];
    char *xcoralrc = 0;
    extern char *ie_load_file ();
    extern void exit ();
    extern char  *getcwd();
    extern FCT(void, error_ctrl_c,(int));
    extern char *getenv ();

#if defined(DEBUG) && defined(sparc)
    malloc_debug(2);
    malloc_verify();
#endif
    /*
     * Ou suis-je dans quelle etagere.
     */
    if ( getcwd ( (char *) pathname, MAXPATHLEN ) == 0 ) {
	(void) fprintf ( stderr, "Getwd error\n" );
	(void) exit ( 1 );
    }
    (void) bzero ( (char *) TWin, MAXWIN );
    
    /*
     * Initialisation du ressource manager, connexion avec le serveur,
     * creation d'un contexte graphique pour le top et bottom shadow.
     * Calcul des options ( parametres de la commande, .Xdefaults etc... )
     */
    XrmInitialize ();		
    ParseOpenDisp ( &argc, argv );
    CreateRGC ( dpy );
    GetUserDatabase ();
    MergeOptions ();
    
    EdContext = XUniqueContext ();
    
    InitMode ();
    
    /*
     * Initialisation des ressources pour les elements Text,
     * Menus et Panel de controle.
     * Les ressources sont :
     * 	une fonte et 4 couleurs (foreground, background,
     *	top_shadow et bottom shadow ).
     */
    InitTextRes ( dpy, GetOpFont ( OP_TEXT_FONT ), GetOpColor ( OP_TEXT_FG ),
		 GetOpColor ( OP_TEXT_BG ), GetOpColor ( OP_MENU_TS ),
		 GetOpColor ( OP_MENU_BS ));
    
    InitMenusRes ( dpy, GetOpFont ( OP_MENU_FONT ), GetOpColor ( OP_MENU_FG ),
		  GetOpColor ( OP_MENU_BG ), GetOpColor ( OP_MENU_TS ),
		  GetOpColor ( OP_MENU_BS ));
    
    InitControlRes ( GetOpColor ( OP_CTRL_FG ), GetOpColor ( OP_CTRL_BG ),
		    GetOpColor ( OP_CTRL_TS ),  GetOpColor ( OP_CTRL_BS ));
    
    /*
     * Encore quelques initialisations.
     */
    InitEvent ();
    InitScroll ( dpy );
    InitBrowser ();
    (void) InitWarningBox ();
    (void) InitListBox ();
    (void) InitDialogBox ();
    (void) InitFsBox ();
    (void) InitManBox ();
    SetBrowserMode ( (Mode *) GetMode ("C++mode") );
    SetBrowserDir ( (char *) pathname );
    init_smac(1024, 18);
    InitIconPixmap ();
    
    /* 
     * Chargement de '.xcoralrc'
     */
    bzero ( tmp, 256 );
    xcoralrc = (char *) getenv ("XCORALRC");
    if (!xcoralrc) {
	(void) sprintf ( tmp, (char *) getenv ( "HOME" ) );
	(void) strcat ( tmp, "/.xcoralrc" );
    }
    else {
      (void) strcpy ( tmp, xcoralrc );
    }
    msg = (char *) ie_load_file ( (Text *) 0, tmp ) ;
    chdir(pathname );
    

   if ( msg)
/*      (void) fprintf ( stderr, "Warning : .xcoralrc not found: %s\n", msg ); */
     (void) fprintf ( stderr, "%s\n", msg );    
    /*
     * Le mode 'input_str'
     */
    InitInputString ();
    
    /*
     * Allons-y pour les premieres fenetres d'edition. 
     */
    edwin = 0; /* Pour etre sur */
    if (! *(argv + 1)) {
      /* pas de nom de fichier */
      if ( (edwin = CreateWindow ()) == 0 ) { 
	( void ) fprintf ( stderr,"Create window error\n" );
	(void) exit (1);
      }
    
      SetCtrMode ( edwin -> text -> mwin, edwin -> text -> current_mode );
      (void) strcpy ( edwin -> text -> filename, (char *) GetOpFilename () );
      (void) strcpy ( edwin -> text -> current_dir, pathname );
    }
    else {
      while (*++argv) {
	EdWin * win;

	if (! (win  = CreateWindow ()))
	  /* trop de fenetre */
	  break;
	if ( ! edwin )
	  edwin = win;
	SetCtrMode ( win -> text -> mwin, win -> text -> current_mode );
	(void) strcpy ( win -> text -> filename, *argv );
	(void) strcpy ( win -> text -> current_dir, pathname );
	if ( LoadFile ( win -> text, (char *) *argv, NEW ) != -1 )
	  chdir(pathname);
	else
	    SetDirAndFilename ( win -> text, *argv );
      }
    }
	
    /*
     * On ignore les signaux habituels, et on attrape le stop et
     * le continue (pour des raisons tordues).
     */
    (void) signal ( SIGINT, error_ctrl_c );
#ifndef DEBUG
    (void) signal ( SIGQUIT, SIG_IGN );
#endif
/*
   (void) signal ( SIGTSTP, (void (*) ()) stopHandler );
   (void) signal ( SIGCONT, (void (*) ()) contHandler );
*/    
    /* 
     * Affichage des premieres fenetres et on attend que les Events y se
     * pointent.
     */
    {
      int i;

      for (i = 0; (i != MAXWIN) && TWin[i]; i += 1) {
	XMapWindow ( dpy, TWin[i]->w_frame );
	XFlush ( dpy );
      }
   }
    
    WaitForEvent ();
    
    /*NOTREACHED*/
}

/*
**	Name : stopHandler
**
**	Description : Attrape le signal 'stop'. S'il reste des requetes
**		ou des	evenements on ignore le signal.
**
*/
static void stopHandler ()
{
#ifdef DEBUG
    (void) fprintf ( stderr, "Stop\n" );
#endif
    (void) signal ( SIGTSTP, SIG_IGN );
    XSync ( dpy, False );
    if ( QLength ( dpy ) == 0 ) { /* On peut stopper */
	(void) signal ( SIGCONT, contHandler );
	(void) signal ( SIGTSTP, SIG_DFL );
	(void) kill ( getpid(), SIGTSTP );
    }
    else
      (void) signal ( SIGTSTP,  (void (*)()) stopHandler );
}


/*
**	Name : contHandler 
**
**	Description : Attrape le signal 'continue' pour virer tous
**		les evenements de type ButtonPress recus pendant que le
**		process etait stoppe. 
*/
static void contHandler ()
{
    XEvent event;
#ifdef DEBUG
    (void) fprintf ( stderr, "Continue\n" );
#endif
    (void) signal ( SIGCONT, SIG_IGN );
    
    XSync ( dpy, False );
    if ( QLength ( dpy ) != 0 ) 
      while ( XCheckMaskEvent ( dpy, ButtonPress, &event ));
    
    (void) signal ( SIGTSTP, (void (*) ()) stopHandler );
    (void) signal ( SIGCONT, SIG_DFL );
    (void) kill ( getpid (), SIGCONT );
    
    XFlush ( dpy ); 
}

#ifdef SYSV
#include <memory.h>
void bzero(char *dst, int len)
{
	memset((char *)dst, 0, len);
}
void bcopy(char *src, char *dst, int len)
{
/*	memcpy((char *)dst, (char *)src, len); */
  memmove((char *)dst, (char *)src, len);
}
#endif
