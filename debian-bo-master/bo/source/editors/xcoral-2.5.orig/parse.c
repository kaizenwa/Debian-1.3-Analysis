/* ########################################################################

				parse.c

   File: parse.c
   Path: /home/fournigault/c/X11/xcoral-2.31/parse.c
   Description: 
   Created: Fri Jan 27 11:24:00 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:24:01 MET 1995
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


/*
 * $Log: parse.c,v $
 * Revision 1.2  1993/12/24  14:21:33  klamer
 * Added support for geometry specification for browser and visit window.
 * For this file, that means:
 *  - add -browsergeometry / *browsergeometry in XrmOptionDescRec
 *  - add -visitgeometry / *visitgeometry in XrmOptionDescRec
 *  - update opTableEntries accordingly
 *  - Made SetGeometry SetGeometry2, which is called for the three window
 *    types: edit, browser and visit. Created a new function SetGeometry,
 *    which calls SetGeometry2 for these window types.
 *  - Extended function GetOpGeo() to handle OP_B_* and OP_V_* arguments.
 *
 * Besides, the following changes are made:
 *  - Server database now has precedence over .Xdefaults, as it should!
 *  - negative locations of windows now specify right / bottom.
 *
 * Added reading in getenv("XAPPLRESDIR")/Xcoral -- if this file exist.
 * This is according to standard X usage.
 */

#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xresource.h>
#include <X11/cursorfont.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <string.h>
#include <pwd.h>

#ifndef apollo
#ifdef __FreeBSD__
#include <stdlib.h>
#else /* !__FreeBSD__ */
#include <malloc.h>
#endif
#endif

#include "config.h"
#include "options.h"
#include "main_text.h"
#include "parse.h"
#include "shadow.h"

extern 	Display *dpy;
extern 	void exit ();
extern 	char *getenv ();

FCT (static char *, GetHomeDir, (char *dest) ); 
FCT (static void, SetDefaultBG, (unsigned long *bg, unsigned long *fg, char *bwbg, char *bwfg, char *colorbg, char *colorfg) ); 
FCT (static void, SetDialogBG, () ); 
FCT (static void, SetDialogFG, () ); 
FCT (static void, SetFont, () ); 
FCT (static void, SetGeometry, () ); 
FCT (static void, SetMenuBG, () ); 
FCT (static void, SetMenuFG, () ); 
FCT (static void, SetMenuFont, () ); 
FCT (static void, SetTextBG, () ); 
FCT (static void, SetTextFG, () ); 
FCT (static void, SetWarningMode, () ); 
FCT (static void, Usage, () ); 

static XrmOptionDescRec opTable [] = {
  
  {"=",		"*geometry",	XrmoptionIsArg,		(caddr_t) 0 },
  {"-browsergeometry",	"*browsergeometry",	XrmoptionSepArg, (caddr_t) 0 },
  {"-visitgeometry",	"*visitgeometry",	XrmoptionSepArg, (caddr_t) 0 },
  {"-d",	"*display",	XrmoptionSepArg,	(caddr_t) 0 },
  {"-display",	"*display",	XrmoptionSepArg,	(caddr_t) 0 },
  {"-fn",	"*font",	XrmoptionSepArg,	(caddr_t) 0 },
  {"-font",	"*font",	XrmoptionSepArg,	(caddr_t) 0 },
  {"-fg",	"*foreground",	XrmoptionSepArg,	(caddr_t) 0 },
  {"-foreground",	"*foreground",	XrmoptionSepArg,	(caddr_t) 0 },
  {"-bg",		"*background",	XrmoptionSepArg,	(caddr_t) 0 },
  {"-background",	"*background",	XrmoptionSepArg,	(caddr_t) 0 },
  {"-mfn",	"*mfont",	XrmoptionSepArg,	(caddr_t) 0 },
  {"-mfont",	"*mfont",	XrmoptionSepArg,	(caddr_t) 0 },
  {"-mfg",	"*mfg",		XrmoptionSepArg,	(caddr_t) 0 },
  {"-mforeground","*mforeground",	XrmoptionSepArg,	(caddr_t) 0 },
  {"-mbg",	"*mbg",		XrmoptionSepArg,	(caddr_t) 0 },
  {"-mbackground","*mbackground",	XrmoptionSepArg,	(caddr_t) 0 },
  {"-cbg",	"*cbg",		XrmoptionSepArg,	(caddr_t) 0 },
  {"-cbackground","*cbackground",	XrmoptionSepArg,	(caddr_t) 0 },
  {"-cfg",	"*cfg",		XrmoptionSepArg,	(caddr_t) 0 },
  {"-cforeground","*cforeground",	XrmoptionSepArg,	(caddr_t) 0 },
  {"-dw",	"*displaywarning",	XrmoptionIsArg,	(caddr_t) 0 },   
  {"-mono",	"*mono",	XrmoptionIsArg,	(caddr_t) 0 }   
};

static 	int opTableEntries = 23;
static 	XrmDatabase	commandlineDB, savecommandlineDB;
static	XrmDatabase	rDB;
static	Options		options;
static	int		IsColor;

#define Message(str) fprintf(stderr, "%s\n", str )
int UseColor () {return (IsColor);}

/*
**	Function name : ParseOpenDisp
**
**	Description : Parse la ligne de commande et ouvre le
**		display. 
**	Input : Le nombre d'arguments, la table des arguments.
**	Ouput :
*/
void ParseOpenDisp ( argc, argv )
    int *argc;
    char *argv [];
{
    XrmValue value;
    char *str_type [10];
    char *displayname;
    char buf [32];

    char *cmd_name = strrchr ( *argv, '/' );
    if ( cmd_name == 0 )
      cmd_name = argv [0];
    else
      cmd_name ++;
    
    /*
     * On peche les options de la ligne de commande	
     */
    XrmParseCommand ( &commandlineDB, opTable,
		     opTableEntries, cmd_name, argc, argv );
    
    /*
     * J'ai besoin des options de la ligne de commande pour plus tard.
     * En fait, c'est a cause de la fonction XMerge... qui est destructrice
     * Malheureusement il faut passer par un fichier intermediaire.
     */
    bzero ( buf, 32 );
    (void) sprintf ( buf, "/tmp/xcoral_db%d", getuid() );
    XrmPutFileDatabase ( commandlineDB, buf );
    if ( (savecommandlineDB = XrmGetFileDatabase (  buf )) != 0 ) 
      (void) unlink ( buf ) ;
    
    /*
     * Un nom de fichier peut etre...
     */
    if ( *argc == 2 )
      if ( argv[1][0] == '-' )
	(void) Usage ();
    else
      (void) strcpy ( options.filename, argv[1] );
    else 
      (void) strncpy ( options.filename, "NoName", 6 );
    
    /*
     * Avant d'ouvrir la connexion avec le serveur, on verifie
     * si la commande positionne le display. Sinon si displayname == 0
     * c'est la variable d'environnemt DISPLAY qui est utilisee.
     */
    displayname = 0;
    if ( XrmGetResource ( commandlineDB, "xcoral.display",
			 "xcoral.display",	str_type, &value ) == True ) {
	displayname = (char *) malloc ( (unsigned) (value.size + 1) );
	bzero ( displayname, (int) (value.size + 1));
	(void) strncpy ( displayname, value.addr, (int) value.size );
    }

    if ( ! (dpy = XOpenDisplay ( displayname ))) {
	(void) fprintf ( stderr, "%s : Cant'open display '%s'\n",
			argv [0], XDisplayName ( displayname ));
	(void) exit (1);
    }
    if ( displayname != 0 )
      (void) free (displayname); 
    
    if ( XrmGetResource ( commandlineDB, "xcoral.mono",
			 "xcoral.mono", str_type, &value ) == True ) {
      IsColor = False;
    }
    else {
      if ( DefaultDepth ( dpy, DefaultScreen ( dpy )) == 1 ) 
	IsColor = False;
      else
	IsColor = True;
    }
}


/*
**	Function name : GetUserDataBase
**
**	Description : Recherche des ressources dans la
**		hierarchie habituelle.
**	Input : 
**	Ouput :
*/
void GetUserDatabase ()
{
    XrmDatabase homeDB, serverDB, applicationDB;
    
    char 	filename [128];	
    char 	*environment;
    char 	name [256];
    char *str;
    
    (void)  sprintf ( name, "%s/%s", APP_DEFAULT, "Xcoral" );
    
    applicationDB = XrmGetFileDatabase ( name );
    (void) XrmMergeDatabases ( applicationDB, &rDB );
    str = getenv("XAPPLRESDIR");
    if (str != NULL)
    {
	(void) sprintf(name,"%s/%s", str, "Xcoral" );
	applicationDB = XrmGetFileDatabase ( name );
	(void) XrmMergeDatabases ( applicationDB, &rDB );
    }
    /*
       if ( (serverDB = XrmGetDatabase (dpy)) == 0 ) { 
       (void) GetHomeDir ( filename );
       (void) strcat ( filename, "/.Xdefaults" );
       serverDB = XrmGetFileDatabase ( filename );
       }
    */
    if ( (str = XResourceManagerString ( dpy )) != 0 ) 
      serverDB = XrmGetStringDatabase ( str );
    else {	
	(void) GetHomeDir ( filename );
	(void) strcat ( filename, "/.Xdefaults" );
	serverDB = XrmGetFileDatabase ( filename );
    }
    
    (void) XrmMergeDatabases ( serverDB, &rDB );
    
    /*
       Old behaviour was buggy: If XENVIROMENT was not set than .Xdefaults overided
       server database!
       
       if ((environment = getenv ( "XENVIRONEMENT" )) == 0 ) {
       environment = GetHomeDir ( filename );
       (void) strcat ( environment, "/.Xdefaults" );
       }
    */
    if ((environment = getenv ( "XENVIRONEMENT" )) != 0) {
      homeDB = XrmGetFileDatabase ( environment );
      (void) XrmMergeDatabases ( homeDB, &rDB );
    }
}


/*
**	Function name : MergeOptions
**
**	Description :  On ajoute les arguments de la ligne
**		de commande dans la base commune.
**	Input : 
**	Ouput :
*/
void MergeOptions ()
{
    XColor xcolor;
    
    XrmMergeDatabases ( commandlineDB, &rDB );
    
    /*
     * Enfin, on remplie la table des options.
     */ 
    (void) SetGeometry ();
    (void) SetFont (); 
    (void) SetTextFG ();
    (void) SetTextBG ();
    (void) SetMenuFont ();
    (void) SetMenuFG ();
    (void) SetMenuBG ();
    (void) SetDialogFG ();
    (void) SetDialogBG ();
    SetWarningMode();
    
    /*
     * Calcul des top et bottom shadow
     */
    xcolor.pixel = (unsigned long) options.mbg;
    XQueryColor ( dpy, DefaultColormap ( dpy, DefaultScreen ( dpy ) ),&xcolor ); 
    GetShadow ( dpy, &xcolor, &options.menu_top_shadow, &options.menu_bottom_shadow );
    
    xcolor.pixel = (unsigned long) options.cbg;
    XQueryColor ( dpy, DefaultColormap ( dpy, DefaultScreen ( dpy ) ), &xcolor ); 
    GetShadow ( dpy, &xcolor, &options.control_top_shadow, &options.control_bottom_shadow );
    
    xcolor.pixel = (unsigned long) options.bg;
    XQueryColor ( dpy, DefaultColormap ( dpy, DefaultScreen ( dpy ) ), &xcolor );
    GetShadow ( dpy, &xcolor, &options.text_top_shadow, &options.text_bottom_shadow );
    
    /* A partir de la, les ressources sont valides et utilisables par le programme */
}

/*
**	Function name : SetDefaultBG
**
**	Description :  Lorqu' une couleur de fond n'est pas precisee,
**		on utilise celle par defaut. Il faut verifier que cette
**		couleur n'est pas identique a celle de devant ( si fg
**		est une option de la ligne commande, ca peut etre le cas ).
**	Input :  Foreground, background, les valeurs par default.
**	Ouput :
*/
static void SetDefaultBG ( bg, fg, bwbg, bwfg, colorbg, colorfg)
    unsigned long *bg; /* RETURN */
    unsigned long *fg;
    char *bwbg, *bwfg, *colorbg, *colorfg; /*Default bw an color bg */
{	
    int result;
    
    if ( IsColor == False )  {
	*bg = PixelValue ( dpy, bwbg, &result );
	if ( *bg == *fg ) {
	    *bg = PixelValue ( dpy, bwfg, &result );
	}
    }
    else { /* color */
	*bg = PixelValue ( dpy, colorbg, &result );
	if ( *bg == *fg ) {
	    *bg = PixelValue ( dpy, colorfg, &result );
	}
    }
}


/*
**	Function name : SetDialogFG
**
**	Description : Calcul du foreground pour la fenetre 
**		de controle.
**	Input : 
**	Ouput :
*/
static void SetDialogFG ()
{
    XrmValue value;
    char *str_type[20];
    char *buf;
    int result;
    
    if ( XrmGetResource ( rDB, "xcoral.cfg",
			 "xcoral.cforeground", str_type,&value ) == True ) {
	
	buf = (char *) malloc ( (unsigned) (value.size + 1) );
	bzero ( buf, (int) (value.size + 1));
	(void) strncpy ( buf, value.addr, (int) value.size );
	
	if ( IsColor == False ) {
	    if ( (strcmp ( buf, "black" ) != 0 ) && (strcmp ( buf, "white" ) != 0 )) {
		options.cfg = PixelValue ( dpy, BW_CONTROL_FG, &result );
	    }
	    else {
		options.cfg = PixelValue ( dpy, buf, &result );
	    }
	}
	else { /* Color Display */
	    options.cfg = PixelValue ( dpy, buf, &result );
	    if ( result == False ) {
		options.cfg = PixelValue ( dpy, COLOR_CONTROL_FG, &result );
	    }
	}
	if ( buf != 0 )
	  (void) free ( buf );
    }
    else  {
	options.cfg = IsColor ? PixelValue ( dpy, COLOR_CONTROL_FG, &result ) :
	  PixelValue ( dpy, BW_CONTROL_FG, &result );
    }
}

/*
**	Function name : SetDialogBG
**
**	Description : Calcul du background pour la fenetre de
**		controle.
**	Input : 
**	Ouput :
*/
static void SetDialogBG ()
{
    XrmValue value;
    char *str_type[20];
    char *buf;
    int result;
    
    if ( XrmGetResource ( rDB, "xcoral.cbg",
			 "xcoral.cbackground", str_type,&value ) == True ) { 
	
	buf = (char *) malloc ( (unsigned) (value.size + 1) );
	bzero ( buf, (int) (value.size + 1));
	(void) strncpy ( buf, value.addr, (int) value.size );
	
	if ( IsColor == False ) {
	    if ( (strcmp ( buf, "black" ) != 0 ) && (strcmp ( buf, "white" ) != 0 )) 
	      options.cbg = PixelValue ( dpy, BW_CONTROL_BG, &result );
	    else  
	      options.cbg = PixelValue ( dpy, buf, &result );
	    if ( options.cbg == options.cfg ) {
		if ( XrmGetResource ( savecommandlineDB, "xcoral.cbg",
				     "xcoral.cbackground", str_type,&value ) == True ) {
		    if ( options.cbg == PixelValue ( dpy, BW_CONTROL_BG, &result ))
		      options.cfg = PixelValue ( dpy, BW_CONTROL_FG, &result );
		    else
		      options.cfg = PixelValue ( dpy, BW_CONTROL_BG, &result );
		}
		else {
		    if ( strcmp ( buf, BW_CONTROL_BG ) == 0 ) 
		      options.cfg = PixelValue ( dpy, BW_CONTROL_FG, &result );
		    else 
		      options.cfg = PixelValue ( dpy, BW_CONTROL_BG, &result );
		}
	    }
	}
	else { /* Color */
	    options.cbg = PixelValue ( dpy, buf, &result );
	    if ( result == False )
	      options.cbg = PixelValue ( dpy, COLOR_CONTROL_BG, &result );
	    if ( options.cbg == options.cfg ) {
		options.cbg = PixelValue ( dpy, COLOR_CONTROL_BG, &result );
		options.cfg = PixelValue ( dpy, COLOR_CONTROL_FG, &result );
	    }
	}
	if ( buf != 0 )
	  (void) free ( buf );
    }
    else 
      SetDefaultBG ( &options.cbg, &options.cfg, BW_CONTROL_BG, BW_CONTROL_FG,
		    COLOR_CONTROL_BG, COLOR_CONTROL_FG );
}

/*
**	Function name : SetMenuFG
**
**	Description : Calcul du foreground pour les menus.
**	Input : 
**	Ouput :
*/
static void SetMenuFG ()
{
    XrmValue value;
    char *str_type[20];
    char *buf;
    int result;
    
    if ( XrmGetResource ( rDB, "xcoral.mfg",
			 "xcoral.mforeground", str_type,&value ) == True ) {
	
	buf = (char *) malloc ( (unsigned) (value.size + 1) );
	bzero ( buf, (int) (value.size + 1));
	(void) strncpy ( buf, value.addr, (int) value.size );
	
	if ( IsColor == False ) {
	    if ( (strcmp ( buf, "black" ) != 0 ) && (strcmp ( buf, "white" ) != 0 ))
	      options.mfg = PixelValue ( dpy, BW_MENU_FG, &result );
	    else 
	      options.mfg = PixelValue ( dpy, buf, &result );
	}
	else { /* Color Display */
	    options.mfg = PixelValue ( dpy, buf, &result );
	    if ( result == False ) 
	      options.mfg = PixelValue ( dpy, COLOR_MENU_FG, &result );
	}
	if ( buf != 0 )
	  (void) free ( buf );
    }
    else 
      options.mfg = IsColor ? PixelValue ( dpy, COLOR_MENU_FG, &result ) :
	PixelValue ( dpy, BW_MENU_FG, &result );
}

/*
**	Function name : SetMenuBG
**
**	Description : Calcul du background pour les menus.
**	Input : 
**	Ouput :
*/
static void SetMenuBG ()
{
    XrmValue value;
    char *str_type[20];
    char *buf;
    int result;
    
    if ( XrmGetResource ( rDB, "xcoral.mbg",
			 "xcoral.mbackground", str_type,&value ) == True ) {
	buf = (char *) malloc ( (unsigned) (value.size + 1) );
	bzero ( buf, (int) (value.size + 1));
	(void) strncpy ( buf, value.addr, (int) value.size );
	
	if ( IsColor == False ) {
	    if ( (strcmp ( buf, "black" ) != 0 ) && (strcmp ( buf, "white" ) != 0 )) 
	      options.mbg = PixelValue ( dpy, BW_MENU_BG, &result );
	    else  
	      options.mbg = PixelValue ( dpy, buf, &result );
	    if ( options.mbg == options.mfg ) {
		if ( XrmGetResource ( savecommandlineDB, "xcoral.mbg",
				     "xcoral.mbackground", str_type,&value ) == True ) {
		    if ( options.mbg == PixelValue ( dpy, BW_MENU_BG, &result ))
		      options.mfg = PixelValue ( dpy, BW_MENU_FG, &result );
		    else
		      options.mfg = PixelValue ( dpy, BW_MENU_BG, &result );
		}
		else {
		    if ( strcmp ( buf, BW_MENU_BG ) == 0 ) 
		      options.mfg = PixelValue ( dpy, BW_MENU_FG, &result );
		    else 
		      options.mfg = PixelValue ( dpy, BW_MENU_BG, &result );
		}
	    }
	}
	else { /* Color */
	    options.mbg = PixelValue ( dpy, buf, &result );
	    if ( result == False )
	      options.mbg = PixelValue ( dpy, COLOR_MENU_BG, &result );
	    if ( options.mbg == options.mfg ) {
		options.mbg = PixelValue ( dpy, COLOR_MENU_BG, &result );
		options.mfg = PixelValue ( dpy, COLOR_MENU_FG, &result );
	    }
	}
	if ( buf != 0 )
	  (void) free ( buf );
    }
    else 
      SetDefaultBG ( &options.mbg, &options.mfg, BW_MENU_BG, BW_MENU_FG,
		    COLOR_MENU_BG, COLOR_MENU_FG );
}

/*
**	Function name : SetTextFG
**
**	Description : Calcul du foreground pour les fenetres de texte.
**	Input : 
**	Ouput :
*/
static void SetTextFG ()
{
    XrmValue value;
    char *str_type[20];
    char *buf;
    int result;
    
    if ( XrmGetResource ( rDB, "xcoral.fg",
			 "xcoral.foreground", str_type,&value ) == True ) {
	
	buf = (char *) malloc ( (unsigned) (value.size + 1) );
	bzero ( buf, (int) (value.size + 1));
	(void) strncpy ( buf, value.addr, (int) value.size );
	
	if ( IsColor == False ) {
	    if ( (strcmp ( buf, "black" ) != 0 ) && (strcmp ( buf, "white" ) != 0 ))
	      options.fg = PixelValue ( dpy, BW_TEXT_FG, &result );
	    else 
	      options.fg = PixelValue ( dpy, buf, &result );
	}
	else { /* Color Display */
	    options.fg = PixelValue ( dpy, buf, &result );
	    if ( result == False ) {
		options.fg = PixelValue ( dpy, COLOR_TEXT_FG, &result );
	    }
	}
	if ( buf != 0 )
	  (void) free ( buf );
    }
    else {
	options.fg = IsColor ? PixelValue ( dpy, COLOR_TEXT_FG, &result ) :
	  PixelValue ( dpy, BW_TEXT_FG, &result );
    }
}


static void SetWarningMode()
{
    XrmValue	value;
    char		*str_type[20];
    
    if ( XrmGetResource( rDB, "xcoral.displaywarning", 
			"xcoral.Displaywarning", str_type, &value ) == True ) {
	if ( (strcmp (value.addr, "True") == 0) 
	    || (strcmp (value.addr, "-dw") == 0) )
	  options.verb = True;
	else 
	  options.verb = False;
    }
    else 
      options.verb = False;
}

/*
**	Function name : SetTextBG
**
**	Description : Calcul du background pour les fenetres de texte.
**	Input : 
**	Ouput :
*/
static void SetTextBG ()
{
    XrmValue value;
    char *str_type[20];
    char *buf;
    XFontStruct *LoadFont ();
    int result;
    
    /* Le Foreground est deja connu */
    
    if ( XrmGetResource ( rDB, "xcoral.bg",
			 "xcoral.background", str_type,&value ) == True ) {
	/* 
	 * Le bg/background est defini dans les resources ou dans
	 * la ligne de commande
	 */
	buf = (char *) malloc ( (unsigned) (value.size + 1) );
	bzero ( buf, (int) (value.size + 1));
	(void) strncpy ( buf, value.addr, (int) value.size );
	
	if ( IsColor == False ) {
	    if ( (strcmp ( buf, "black" ) != 0 ) && (strcmp ( buf, "white" ) != 0 )) 
	      options.bg = PixelValue ( dpy, BW_TEXT_BG, &result );
	    else  
	      options.bg = PixelValue ( dpy, buf, &result );
	    if ( options.bg == options.fg ) {
		if ( XrmGetResource ( savecommandlineDB, "xcoral.bg",
				     "xcoral.background", str_type,&value ) == True ) {
		    /* L'option dans la ligne de commande est prioritaire */
		    if ( options.bg == PixelValue ( dpy, BW_TEXT_BG, &result ))
		      options.fg = PixelValue ( dpy, BW_TEXT_FG, &result );
		    else
		      options.fg = PixelValue ( dpy, BW_TEXT_BG, &result );
		}
		else {
		    if ( strcmp ( buf, BW_TEXT_BG ) == 0 ) 
		      options.fg = PixelValue ( dpy, BW_TEXT_FG, &result );
		    else 
		      options.fg = PixelValue ( dpy, BW_TEXT_BG, &result );
		}
	    }
	}
	else { /* Color */
	    options.bg = PixelValue ( dpy, buf, &result );
	    if ( result == False )
	      options.bg = PixelValue ( dpy, COLOR_TEXT_BG, &result );
	    if ( options.bg == options.fg ) {
		options.bg = PixelValue ( dpy, COLOR_TEXT_BG, &result );
		options.fg = PixelValue ( dpy, COLOR_TEXT_FG, &result );
	    }
	}
	if ( buf != 0 )
	  (void) free ( buf );
    }
    else {
	SetDefaultBG ( &options.bg, &options.fg, BW_TEXT_BG, BW_TEXT_FG,
		      COLOR_TEXT_BG, COLOR_TEXT_FG );
    }
}

/*
**	Function name : SetMenuFont
**
**	Description : La fonte pour les menus.
**	Input : 
**	Ouput :
*/
static void SetMenuFont ()
{
    XrmValue value;
    char *str_type[20];
    char *buf;
    XFontStruct *LoadFont ();
    
    if ( XrmGetResource ( rDB, "xcoral.mfn",
			 "xcoral.mfont", str_type,&value ) == True ) {
	
	buf = (char *) malloc ( (unsigned) (value.size + 1) );
	bzero ( buf, (int) (value.size + 1));
	(void) strncpy ( buf, value.addr, (int) value.size );
	
	options.menu_font = LoadFont ( dpy, buf );
	if ( buf != 0 )
	  (void) free ( buf );
    }
    else {
	options.menu_font = LoadFont ( dpy, MENU_FONT );
    }
}


/*
**	Function name : SetFont
**
**	Description : La fonte pour les fenetres texte.
**	Input : 
**	Ouput :
*/
static void SetFont ()
{
    XrmValue value;
    char *str_type[20];
    char *buf;
    XFontStruct *LoadFont ();
    
    if ( XrmGetResource ( rDB, "xcoral.fn",
			 "xcoral.font", str_type,&value ) == True ) {
	/*
	 * Ya des resssources
	 */
	buf = (char *) malloc ( (unsigned) (value.size + 1) );
	bzero ( buf, (int) (value.size + 1));
	(void) strncpy ( buf, value.addr, (int) value.size );
	options.text_font = LoadFont ( dpy, buf );
	if ( buf != 0 )
	  (void) free (buf);
    }
    else {
	options.text_font = LoadFont ( dpy, TEXT_FONT );
    }
}


/*
**	Function name : SetGeometry
**
**	Description : Initialisation de la geometrie de 
**		la feneter printcipale.
**	Input : 
**	Ouput :
*/

struct size {
  int x, y, width, height;
};

static void SetGeometry2 (opt,name,class)
    struct size *opt;
    char        *name, *class;
{
    XrmValue value;
    char *str_type[20];
    char *buf;
    int x, y;
    unsigned int width, height;
    long flags;
    
    if ( XrmGetResource ( rDB, name, /* "xcoral.geometry", */
			 class, /* "xcoral.Geometry", */ str_type,&value ) == True ) {
	/*
	 * Ya des resssources
	 */
	buf = (char *) malloc ( (unsigned) (value.size + 1) );
	(void) strncpy ( buf, value.addr, (int) value.size ); 
	
	flags = XParseGeometry ( buf,  &x, &y, &width, &height );
	if ( buf != 0 )
	  (void) free ( buf );
	
	if (!((WidthValue | HeightValue) & flags) )
	  (void) Usage ();
	
	if ( (width < ((DisplayWidth ( dpy, DefaultScreen (dpy )) * 2 )  / 5 ))
	    || width  > DisplayWidth ( dpy, DefaultScreen (dpy ) ) )
	  opt->width = (DisplayWidth ( dpy, DefaultScreen (dpy ) ) * 7) / 10;
	else
	  opt->width = width;
	
	if ( (height < ((DisplayHeight ( dpy, DefaultScreen (dpy )) * 1 ) /5 )) 
	    || height > DisplayHeight ( dpy, DefaultScreen (dpy ) ) )
	  opt->height = (DisplayHeight ( dpy, DefaultScreen (dpy ) ) * 6) / 10;
	else
	  opt->height = height;

	if ( XValue & flags ) {
	    if ( XNegative & flags )
	      x = DisplayWidth ( dpy, DefaultScreen ( dpy ) ) - width + x;
	    opt->x = x;
	}
	else
	  opt->x = -1;
	
	if ( YValue & flags ) {
	    if ( YNegative & flags )
	      y = DisplayHeight ( dpy, DefaultScreen (dpy ) ) - height + y;
	    opt->y = y;
	}
	else
	  opt->y = -1;
    }
    else {
	/*
	 *	Largeur par defaut =  13/20 de l'ecran.
	 *	Hauteur par defaut =  12/20 de l'ecran.
	 */
	opt->width = (DisplayWidth ( dpy, DefaultScreen (dpy ) ) * 13) / 20;
	opt->height = (DisplayHeight ( dpy, DefaultScreen (dpy ) ) * 12) / 20;
	opt->x = -1;
	opt->y = -1;
	
	if ( strcmp ( "xcoral.browsergeometry", name) == 0 ) {
	  opt->width = (DisplayWidth ( dpy, DefaultScreen (dpy ) ) * 6) / 11;
	  opt->height = (DisplayHeight ( dpy, DefaultScreen (dpy ) ) * 6) / 8;
	}
	if ( strcmp ( "xcoral.visitgeometry", name) == 0 ) {
	  opt->width = (DisplayWidth ( dpy, DefaultScreen (dpy ) ) * 10) / 20;
	  opt->height = (DisplayHeight ( dpy, DefaultScreen (dpy ) ) * 9) / 20;
	}
    }
}

static void SetGeometry()
{
  struct size  opt;
  
  SetGeometry2(&opt,"xcoral.geometry","xcoral.Geometry");
  options.x = opt.x;
  options.y = opt.y;
  options.width = opt.width;
  options.height = opt.height;
  SetGeometry2(&opt,"xcoral.browsergeometry","xcoral.BrowserGeometry");
  options.b_x = opt.x;
  options.b_y = opt.y;
  options.b_width = opt.width;
  options.b_height = opt.height;
  SetGeometry2(&opt,"xcoral.visitgeometry","xcoral.VisitGeometry");
  options.v_x = opt.x;
  options.v_y = opt.y;
  options.v_width = opt.width;
  options.v_height = opt.height;
}

/*
**	Function name : GetHomeDir
**
**	Description : Comme son nom l'indique.
**	Input : Le container.
**	Ouput :
*/
static char *GetHomeDir ( dest )
    char * dest;
{
    int uid;
    /*	struct passwd *getpwuid (); */
    struct passwd *pw;
    char *ptr;
    
    if ((ptr = getenv ("HOME")) != 0 ) {
	(void) strcpy ( dest, ptr );
    }
    else {
	if ((ptr = getenv ("USER")) != 0 ) {
	    pw = getpwnam (ptr);
	}
	else {
	    uid = getuid ();
	    pw = getpwuid ( uid );
	}
	if (pw) {
	    (void) strcpy ( (char *) dest, (char *) pw -> pw_dir );
	}
	else {
	    *dest = ' ';
	}
    }
    return dest;
}


/*
**	Function name : Usage
**
**	Description : 
**	Input : 
**	Ouput :
*/
static void Usage ()
{
    (void) fprintf ( stderr,"Usage : xcoral [-options...]\n");
    (void) fprintf ( stderr,"Option :\n");
    (void) fprintf ( stderr,
		    "\t[-d/-display]\t\t -Display\n" );
    (void) fprintf ( stderr,
		    "\t[=WidthxHeight+X+Y]\t -Window text geometry\n" );
    (void) fprintf ( stderr,
		    "\t[-browsergeometry WidthxHeight+X+Y]\t -Browser control geometry\n" );
    (void) fprintf ( stderr,
		    "\t[-visitgeometry WidthxHeight+X+Y]\t -Browser visit window geometry\n" );
    (void) fprintf ( stderr,
		    "\t[-fn/-font]\t\t -Font in text window\n" );
    (void) fprintf ( stderr,
		    "\t[-bg/-background]\t -Background color in text window\n" );
    (void) fprintf ( stderr,
		    "\t[-fg/-foreground]\t -Foreground color in text window\n" );
    (void) fprintf ( stderr,
		    "\t[-mfn/-mfont]\t\t -Font in menu\n" );
    (void) fprintf ( stderr,
		    "\t[-mbg/-mbackground]\t -Background color in menu\n" );
    (void) fprintf ( stderr,
		    "\t[-mfg/-mforeground]\t -Foreground color in menu\n" );
    (void) fprintf ( stderr,
		    "\t[-cbg/-cbackground]\t -Background color in control panel\n" );
    (void) fprintf ( stderr,
		    "\t[-cfg/-cforeground]\t -Foreground color in control panel\n" );
    (void) fprintf ( stderr,
		    "\t[-mono]\t\t -Force black and white colors\n" );
    (void) fprintf ( stderr,
		    "\t[-dw/-displaywarning]\t -Browser warning messages\n" );
    
    (void) exit (1);
}


/*
**	Function name : PixelValue
**
**	Description : 
**	Input : 
**	Ouput :
*/
unsigned long PixelValue ( display, str, result )
    Display *display;
    char *str;
    int *result;
{
    XColor  color, tcolor;
    int status;
    
    status = XAllocNamedColor ( display,
			       DefaultColormap ( display, DefaultScreen ( display ) ), 
			       str, &color, &tcolor );
    
    if ( status == True )
      *result = True;
    else {
	if ( IsColor == True ) {
	    (void) fprintf ( stderr, "Warning : can't alloc '%s' color.\n", str );
	    color.pixel = (unsigned long) BlackPixel(display,DefaultScreen ( display ));
	    *result = True;
/*	    (void) exit (1); */
	}
	else
	  *result = False;
    }
    return (color.pixel);
}


/*
**	Function name : GetOpFont
**
**	Description : Donne la fonte pour le type
**		d'objet passe en argument.
**	Input : Le type
**	Ouput : Un pointeur sur la fonte.
*/
XFontStruct *GetOpFont ( type )
    int type;
{
    XFontStruct *font;
    
    switch ( type ) {
    case OP_TEXT_FONT:
      font = options.text_font;
      break;
    case OP_MENU_FONT:
      font = options.menu_font;
      break;
    }
    return (font);
}


/*
**	Function name : GetOpColor
**
**	Description : Donne la couleur du type d'objet
**		passe en argument.
**	Input : Le type
**	Ouput : La valeur en pixel.
*/
unsigned long GetOpColor ( type )
    int type;
{
    unsigned long color;
    
    switch ( type ) {
    case OP_TEXT_FG:
      color = options.fg;
      break;
    case OP_TEXT_BG:
      color =  options.bg;
      break;
    case OP_TEXT_TS:
      color =  options.text_top_shadow;
      break;
    case OP_TEXT_BS:
      color = options.text_bottom_shadow;
      break;
    case OP_MENU_FG:
      color =  options.mfg;
      break;
    case OP_MENU_BG:
      color = options.mbg;
      break;
    case OP_MENU_TS:
      color =  options.menu_top_shadow;
      break;
    case OP_MENU_BS:
      color = options.menu_bottom_shadow;
      break;
    case OP_CTRL_FG:
      return options.cfg;
      break;
    case OP_CTRL_BG:
      return options.cbg;
      break;
    case OP_CTRL_TS:
      color = options.control_top_shadow;
      break;
    case OP_CTRL_BS:
      color = options.control_bottom_shadow;
      break;
    }
    return color;
}


/*
**	Function name : GetOpFilename
**
**	Description : Donne le filename de la ligne de commande.
**	Input : 
**	Ouput : Le filename.
*/
char *GetOpFilename ()
{
    return ( options.filename ); 
}


/*
   **	Function name : GetOpBW
   **
   **	Description : La Largeur du bord.
   **	Input : 
   **	Ouput :
*/
unsigned int GetOpBW ()
{
    return options.bw;
}


/*
**	Function name : GetOpBD
**
**	Description : Le border 
**	Input : 
**	Ouput :
*/
unsigned  long GetOpBD ()
{
    return options.bd;
}


/*
**	Function name : GetOpGeo
**
**	Description : Donne un element de la geometrie.
**	Input : Le type.
**	Ouput : Valeur en pixels.
*/
int GetOpGeo ( type )
    int type;
{
    int result;
    
    switch ( type ) {
    case OP_X:
      result = options.x;
      break;
    case OP_Y:
      result = options.y;
      break;
    case OP_WIDTH:
      result = options.width;
      break;
    case OP_HEIGHT:
      result = options.height;
      break;

    case OP_B_X:
      result = options.b_x;
      break;
    case OP_B_Y:
      result = options.b_y;
      break;
    case OP_B_WIDTH:
      result = options.b_width;
      break;
    case OP_B_HEIGHT:
      result = options.b_height;
      break;
   
    case OP_V_X:
      result = options.v_x;
      break;
    case OP_V_Y:
      result = options.v_y;
      break;
    case OP_V_WIDTH:
      result = options.v_width;
      break;
    case OP_V_HEIGHT:
      result = options.v_height;
      break;
    }
    return result;
}


/*
**	Function name : OpVerbose
**
**	Description : 
**	Input : 
**	Ouput :
*/
int OpVerbose ()
{
    return (options.verb);
}
