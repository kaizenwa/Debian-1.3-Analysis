/* ########################################################################

			       get_file.c

   File: get_file.c
   Path: /home/fournigault/c/X11/xcoral-2.31/get_file.c
   Description: 
   Created: Fri Jan 27 11:03:46 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:03:47 MET 1995
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
#ifdef UNIXWARE
#include <unistd.h>
#include <filehdr.h>
#endif 
#include <X11/Xlib.h>
#include <X11/cursorfont.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <sys/param.h>

#ifndef apollo
#ifdef __FreeBSD__
#include <stdlib.h>
#else /* !__FreeBSD__ */
#include <malloc.h>
#endif
#endif
#include <string.h> 
#ifdef SVR4
#include <unistd.h>
#include <sys/exechdr.h>
#endif

#include <errno.h>

#include "xcoral.h"
#include "browser_init.h"
#include "browser_eve.h"
#include "get_file.h"
#include "text_cursor.h"
#include "page.h"
#include "new_window.h"
#include "mark_reg.h"
#include "fs_box.h"
#include "input_str.h"
#include "dial_box.h"
#include "warn_box.h"

FCT (static int, CheckFile, (char *path, int *len, Text *text) );
FCT (static int, IsBinary, (FILE *fd, Text *text, int len) );
FCT (static void, KillCurrentBuffer, (Text *text, int from) );
     
#ifdef sun
#define _SUN_OS
#include <sys/exec.h>
#define ISMAG(x) (((x) == OMAGIC) || ((x) == NMAGIC) || ((x) == ZMAGIC))
#endif

#ifdef apollo
#define _APOLLO
#include <filehdr.h>
#define ISMAG(x) ISCOFF(x)
#endif

#if  (m68k && _AUX_SOURCE)
#define _APPLE_A_UX
#include <filehdr.h>
#define ISMAG(x) (((x)==MC68MAGIC) || ((x)==MC68TVMAGIC) || ((x)==M68MAGIC) || ((x)==M68TVMAGIC) \
|| ((x)==M68NSMAGIC))
#endif

#ifdef hpux
#define _HPUX
#include <filehdr.h>
#define ISMAG(x) (((x) == RELOC_MAGIC) || ((x) == EXEC_MAGIC) || ((x) == SHARE_MAGIC) \
	|| ((x) == DEMAND_MAGIC) || ((x) == DL_MAGIC) || ((x) == SHL_MAGIC))
#endif

#if ((__osf__) && (__alpha))
#define _DEC_OSF
#include <sys/exec.h>
#define ISMAG(x) (((x) == OMAGIC) || ((x) == NMAGIC) || ((x) == ZMAGIC))
#endif

#if ((ultrix) && (mips))
#define _DEC_ULTRIX
#include <sys/exec.h>
#define ISMAG(x) (((x) == OMAGIC) || ((x) == NMAGIC) || ((x) == ZMAGIC))
#endif

#ifdef _AIX
#include <filehdr.h>
#define ISMAG(x) (((x) == U802WRMAGIC) || ((x) == U802ROMAGIC) || \
	((x) == U802TOCMAGIC) || ((x) == U800WRMAGIC) || \
	((x) == U800ROMAGIC)  || ((x) == U800TOCMAGIC))
#endif

#if ((sgi) || (__sgi))
#define _SGI
#include <filehdr.h>
#define ISMAG(x) ISCOFF(x)
#endif

#ifdef linux
#define _LINUX
#include <unistd.h>
#include <a.out.h>
#define ISMAG(x) (((x)==OMAGIC)||((x)==NMAGIC)||((x)==ZMAGIC))
#endif

#if defined (__NetBSD__) || defined (__FreeBSD__)
#define _NETBSD
#include <a.out.h>
#define ISMAG(x) (((x) == OMAGIC) || ((x) == NMAGIC) || ((x) == ZMAGIC))
#endif

#ifdef __bsdi__
#define _BSDI
#include <a.out.h>
#define ISMAG(x) (((x) == OMAGIC) || ((x) == NMAGIC) || ((x) == ZMAGIC))
#endif

#ifdef UNIXWARE
#include <sys/x.out.h>
#define ISMAG(x) (((x) == OMAGIC) || ((x) == NMAGIC) || ((x) == ZMAGIC))
#endif

#ifndef ISMAG
#define _BAD_SYS
#define ISMAG(x) (((x) == OMAGIC) || ((x) == NMAGIC) || ((x) == ZMAGIC))
#endif

extern 	Display	*dpy;

#define MAX_NAME_LEN 20
int write_flag = True;

/*
**	Function name : KbdReadFile
**
**	Description : Commande 'read file' a partir du clavier.
**	Input : Le text courant.
**	Ouput :
*/
void KbdReadFile ( text )
    Text *text;
{
    char *str, *color; 
    Text *result;
    char c = '\007'; /* ^G */
    
    if ( GetModif ( text ) == True ) {
	if ( SaveCurrentBuffer ( text, F_KEY ) != True )
	  return;
    }
    ClearMessageWindow ( text -> mwin );
    
    str = GetString ( text, "Read File : ", 0 );
    
    if ( (str == 0) || (strncmp(str, &c, 1)) == 0 ) {
	DisplayMessage ( text -> mwin, "Abort" );
	return;
    }
    if ( *str == '~' )
      str = ExpandTildeName ( str );
    
    TextCursorOff ( text );
    if ( LoadFile ( text, str, NEW ) != -1 ) {
	ShowScrollFrame ( dpy, text -> swin );
	FirstPage ( text );
	SetTextSave ( text );
	if ( IsAlreadyLoad ( text -> filename, text, &result ) > 1) {
	    XBell ( dpy, 10 );
	    DisplayMessage ( text -> mwin, "Warning ...Already loaded" );
	    KillBuffer ( text );
	    XMapRaised ( dpy, result -> w_parent );
	}
    }
    TextCursorOn ( text );
    color = (char *) getenv("XCORAL_AUTO_HIGHLIGHT");
    if ( color != 0 && strcmp(color,"True")== 0)
      ColorBuffer(text);
 }


/*
**	Function name : KbdInsertFile
**
**	Description : Commande 'insert file' a partir du clavier.
**	Input : 
**	Ouput :
*/
void KbdInsertFile ( text )
    Text *text;
{
    char *str;
    char c = '\007'; /* ^G */
    int nbl, olen;
    int x_len;
    
    ClearMessageWindow ( text -> mwin );
    
    str = GetString ( text, "Insert File : ", 0 );
    
    if ( (str == 0) || (strncmp(str, &c, 1)) == 0 ) {
	DisplayMessage ( text -> mwin, "Abort" );
	return;
    }
    if ( *str == '~' )
      str = ExpandTildeName ( str );
    
    TextCursorOff ( text );
 
    nbl = text -> lines_in_buf;
    olen = BottomBuf (text -> buf) - RightBuf (text -> buf);
    
    if ( LoadFile ( text, str,  INSERT ) != -1 ) {
	SetTextModif ( text );		
	SetAndDisplayPage ( text ); 
	ShowScrollFrame ( dpy, text -> swin );
	(void) MoveScrollBar ( dpy, text -> swin, 
			      CURRENT, text -> no_current_line - text -> n1 - 1 );
	x_len = (BottomBuf (text -> buf) - RightBuf (text -> buf)) - olen;
	StoreInUndo ( text, RightBuf (text -> buf) , (char *) 0, x_len,
		     text -> lines_in_buf - nbl, U_INSERT );
    
	/* Pour la couleur */
	if ( text -> current_ce != 0 ) {
	  (void) UpdateColorList ( text, x_len );
	  ClipOn ( text, 0 );
	  RefreshPage ( text );
	  ClipOff ( text );
	}
    }
    TextCursorOn ( text );
}


/*
**	Function name : LoadFile
**
**	Description : Charge un fichier.
**	Input : Le text courant, le nom du fichier, type de
**		l'operation ( nouveau ou insert ).
**	Ouput : 0 si OK -1 sinon
*/
int LoadFile ( text, s, type )
    Text *text;
    char *s;
    int type;
{
    static int len;
    int n;
    Mode *mode;
    char *msg;
    FILE *fd;
    extern flag_visit;
    
#ifdef DEBUG
    (void) fprintf ( stderr, "filename = %s\n", s );
#endif
    if ( CheckFile ( s, &len, text ) == 0 ) {
	fd = fopen ( s, "r" );
	if ( fd == 0 ) {
	    (void) fprintf ( stderr, "Open error\n" );
	    return -1;
	}
	if ( IsBinary ( fd, text, len ) == True ) {
	    (void) fclose ( fd );
	    return -1;
	}
	rewind ( fd );
	switch ( type ) {
	case NEW:
	  (void) DeleteColorList ( text );
	  (void) LoadFileInBuffer ( text -> buf, fd, len, NEW );
	  SetDirAndFilename ( text, s );
	  SetWindowName ( text );
	  SetBrowserDir ( (char *) text -> current_dir );
	  mode =  (Mode *) SearchMode ( text -> filename );
	  if ( mode ) {
	      text -> current_mode = mode;
	      if ( flag_visit == False ) {
		  SetCtrMode ( text -> mwin, mode );
		  RefreshWindowMode ( text -> mwin );
	      }
	      if ( mode -> font )
		RefreshWithNewFont ( text, mode -> font );
	  }
	  break;
	case INSERT:
	  (void) LoadFileInBuffer ( text -> buf, fd, len,  INSERT );
	  break;
	}
	ResetMark ( text );
	n = GetNumberOfLineInBuf ( text -> buf );
#ifdef DEBUG
	(void) fprintf ( stderr, "n = %d\n", n );
#endif
	text -> lines_in_buf = n;
	SetScrollLine ( text -> swin, n );
	(void) fclose ( fd );

	return 0;
    }
    else
      return -1;
}

/*
**	Function name : SetDirFilename
**
**	Description : Positionne la directorie et le
**		pathname complet.
**	Input : Le text courant, le nom du fichier.
**	Ouput :
*/


void SetDirAndFilename ( text, name )
    Text *text;
    char *name;
{
    char *p;
    char *old_dir;
    char pathname [ MAXPATHLEN + 2];
    extern char  *getcwd();
    
    /*
     * Ici, name est un pathname valide. On doit utiliser le pathname
     * complet a cause du browser. name peut etre relatif ou absolu,
     * si name contient des .. il n'est pas possible de jouer seulement
     * sur le path pour les suprimer a` cause des liens symboliques.
     */
    old_dir = (char *) malloc ( (unsigned) strlen ( text -> current_dir ) + 1 );
    (void) strcpy ( old_dir, text -> current_dir );

    if (( strcmp ( name, "NoName") == 0 ) || (*name == '/'))
      (void) strcpy( text -> filename, name );
    else if ( strcmp ( text -> current_dir, "/" ) == 0 )
      (void) sprintf ( text -> filename,"/%s", name );
    else		
      (void) sprintf ( text -> filename,"%s/%s", text -> current_dir, name );
    
    if ( (p = strrchr ( name, '/' )) != 0 ) {
      /* Changement de repertoire */
      if (p == name) {		/* le cas particulier de la racine */
	(void) chdir ( "/" );
	(void) strcpy ( text -> current_dir, "/" );
	(void) sprintf ( text -> filename,"%s", name );
      }
      else {
	char * p1 = strrchr ( text -> filename, '/' );
	  
	*p1 = 0;
	chdir(text -> filename);
#ifdef linux
	if ( getwd ( pathname ) == 0 ) {
	    (void) fprintf ( stderr, "Can't getwd\n" );			
	}
#else	
	(void) getcwd ( pathname, MAXPATHLEN );
#endif
	(void) strcpy( text -> current_dir, pathname);
	if ( strcmp ( text -> current_dir, "/" ) == 0 )
	  (void) sprintf ( text -> filename, "/%s", p+1 );
	else		
	  (void) sprintf ( text -> filename, "%s/%s", text -> current_dir, p+1 );
      }
    }
    
    if ( strcmp ( old_dir, text -> current_dir ) != 0 ) {
	(void) chdir ( text -> current_dir );
	/*
	 * A cause de l'automount et des liens symboliques il faut
	 * de nouveau verifier le path.
	 */
#ifdef linux
	if ( getwd ( pathname ) == 0 ) {
	    (void) fprintf ( stderr, "Can't getwd\n" );			
	}
#else	
	(void) getcwd ( pathname, MAXPATHLEN );
#endif
	if ( strcmp ( pathname, text -> current_dir ) != 0 ) {
	  (void) strcpy ( text -> current_dir, pathname );
	  if (! strcmp(text -> current_dir, "/"))
	    (void) sprintf ( text -> filename, "/%s", p+1 );
	  else
	    (void) sprintf ( text -> filename, "%s/%s",
			    text -> current_dir, p+1);
	}
    }
#ifdef DEBUG      
    printf ( "dirname = %s\n", text -> current_dir );
    printf ( "filename = %s\n", text -> filename );
#endif
    (void) free ( old_dir );
    return;
}


/*
**	Function name : KbdSaveFile
**
**	Description : Commande 'save file' a partir du clavier.
**	Input : Le text courant.
**	Ouput :
*/
void KbdSaveFile ( text )
    Text *text;
{
    char   *name;
    char c = '\007';
    
    if ( strcmp (text -> filename, "NoName") == 0 ) {
	name = GetString ( text, "Write file : ", 0 );

	if ( (name == 0) || strncmp(name, &c,1) == 0 ) {
	    DisplayMessage ( text -> mwin, "Abort" );
	    return;
	}
	else {
	    if ( *name == '~' )
	      name = ExpandTildeName ( name );
	    
	    if ( *name == '/' ) {
		(void) strcpy ( text -> filename, name );
	    }
	    else {
		(void) strcpy ( text -> filename,  text -> current_dir );
		(void) strcat ( text -> filename, "/" );
		(void) strcat ( text -> filename, name );
	    }
	    (void) WriteFile ( text );
	    if ( write_flag )
	      SetWindowName ( text );
	}
    }
    else {
#ifdef DEBUG
	(void) sprintf ( buf, "cp %s '#%s'\n",
			text -> filename, text -> filename );
	system ( buf );
#endif
	if ( text -> modif == True )
	  (void) WriteFile ( text );
	else 
	  DisplayMessage ( text -> mwin, "No changes" );	
    }
}


/*
**	Function name : WriteFile
**
**	Description : Ecriture du buffer courant dans un fichier.
**	Input : Le text courant.
**	Ouput :
*/
void WriteFile ( text )
    Text *text;
{
    FILE *fd;
    int size;
#ifndef __FreeBSD__
    extern char *sys_errlist[];
#endif
    char buf [128];
    char *name;
    
    write_flag = True;
    if ( (fd = fopen ( text -> filename, "w" )) == 0 ) {
	DisplayMessage ( text -> mwin, sys_errlist[errno] );
	write_flag = False;
	return;
    }
    DisplayMessage ( text -> mwin, "Write ..." );
    XSync ( dpy, False );
    size = WriteCurrentFile ( text -> buf, fd );
    if ( size == -1 )  {
	(void) sprintf ( buf, "Write error" );
	DisplayMessage ( text -> mwin, buf );
	(void) fclose ( fd );
	write_flag = False;
	return;
    }
    else {
	name = 0;
	if ( (name = (char * ) strrchr ( text -> filename, '/' )) != 0 )
/*	  (void) sprintf ( buf, "Write %s, %d bytes.\n", ++name, size ); */
	  (void) sprintf ( buf, "Write %s, %d bytes.", ++name, size );
	else {
/*	    (void) sprintf ( buf, "Write %s, %d bytes.\n", text -> filename, size ); */
	  (void) sprintf ( buf, "Write %s, %d bytes.", text -> filename, size );
	}
	DisplayMessage ( text -> mwin, buf );
	SetTextSave ( text );
    }
    (void) fclose ( fd );
    if ( (text -> filename != 0) && strlen ( text -> filename) != 0 ) {
	if ( GoodSuffix ( text -> filename ) == True ) {
#ifdef DEBUG
	    (void) fprintf ( stderr, "touch %s\n", text -> filename ); 
#endif
	    parse_file ( text -> filename ); 
	    RefreshBrowserInfos ();
	    TextCursorOff ( text );
	    TextCursorOn ( text );
	}
    }
}


/*
**	Function name : KbdWriteFile
**
**	Description : Commande 'write file' a partir du clavier.
**	Input : Le text courant.
**	Ouput :
*/
void KbdWriteFile ( text )
    Text *text;
{
    char *s;
    char buf [128];
    char c = '\007';
    char *name;
    char tmp_name [1024];
    
#ifdef DEBUG
    (void) fprintf ( stderr, "Write File from key\n" );
#endif
    
    if ( (name = (char * ) strrchr ( text -> filename, '/' )) != 0 ) {
      name ++; /* On vire le slash */
      strcpy ( tmp_name, name );
    }
    else
      strcpy ( tmp_name, text -> filename );

    if ( strlen (tmp_name) > MAX_NAME_LEN ) 
      strcpy ( tmp_name + (MAX_NAME_LEN-4), "..." );
    (void) sprintf ( buf, "Write file [%s] : ", tmp_name );
    
    s = GetString ( text, buf, 0 );

    if ( (s == 0) || ((strncmp(s,"y",1) == 0) &&(strlen(s)==1)) ) {
	WriteFile ( text );
    }
    else if ( ((strncmp(s,"n",1 ) == 0)&&(strlen(s)==1)) || (strncmp(s,&c,1)==0) ) {
	DisplayMessage ( text -> mwin, "Abort" );
    }
    else {
	if ( *s == '~' )
	  s = ExpandTildeName ( s );
    
	if ( *s == '/' ) {
	    (void) strcpy ( text -> filename, s );
	}
	else {
	    (void) strcpy ( text -> filename,  text -> current_dir );
	    (void) strcat ( text -> filename, "/" );
	    (void) strcat ( text -> filename, s );
	}
	WriteFile ( text );
	if ( write_flag ) {
	  SetDirAndFilename ( text, s );
	  SetWindowName ( text );
	}
    }
}


/*
**	Function name : CheckFile
**
**	Description : Quelles les permissions sur le fichier.
**	Input : Le pathname, la longueur, le text courant.
**	Ouput : 0 Ok -1 sinon
*/
static int CheckFile ( path, len, text )
    char *path;
    int *len;	/* RETURN */
    Text *text;
{
    struct stat st;
    
    (void) stat ( path, &st );
    if ( access ( path, F_OK ) != 0 ) {
	DisplayMessage  ( text -> mwin, "File not found" );
	return (-1);
    }
    if ( access ( path, R_OK ) != 0 ) {
	DisplayMessage ( text -> mwin, "Permission denied" );
	return (-1);
    }
    if ( access ( path, W_OK ) != 0 ) {
	DisplayMessage ( text -> mwin, "Read only file" );
    }
    if ( ! S_ISREG(st.st_mode) ) {
	DisplayMessage  ( text -> mwin, "Not a regular file" );
	return (-1);
    }
    
    *len = (int) st.st_size;
    return 0;
}

/*
**	Function name : IsBinary
**
**	Description : On verifie que le fichier n'est pas
**		un binaire ou une archive.
**	Input : Le stream associe, le text courant, la longueur du fichier.
**	Ouput : Vrai si binaire Faux sinon.
*/
static int IsBinary ( fd, text, len )
    FILE *fd;
    Text *text;
    int len;
{
    unsigned short magic;
#ifdef _SUN_OS
    struct exec head;
#endif
#ifdef _HPUX
    struct header head;
#endif
#ifdef _DEC_OSF
    struct exec head;
#endif
#ifdef _DEC_ULTRIX
    struct exec head;
#endif
#ifdef _APPLE_A_UX
    struct filehdr head;
#endif
#ifdef _AIX
    struct filehdr head;
#endif
#ifdef _SGI
    struct filehdr head;
#endif
#ifdef _APOLLO
    struct filehdr head;
#endif
#ifdef _LINUX
    struct exec head;
#endif

#ifdef _NETBSD
        struct exec head;
#endif

#ifdef _BSDI
	struct exec head;
#endif
    
#ifdef _BAD_SYS
    struct exec head;
#endif

#ifdef UNIXWARE
    struct filehdr head;
#endif
    
    if ( len < sizeof (head) )	/* Un peu court ... */
      return False;
    if (fread((char *) &head, sizeof (head), 1, fd) != 1) {
	(void) fprintf ( stderr, "Header read error\n" );
	return False;
    }
#ifdef _SUN_OS
    magic = head.a_magic;
#endif
#ifdef _DEC_ULTRIX
    magic = head.ex_o.magic;
#endif
#ifdef _DEC_OSF
    magic = head.ex_o.magic;
#endif
#ifdef _HPUX
    magic = head.a_magic;
#endif
#ifdef _APPLE_A_UX
    magic = head.f_magic;
#endif
#ifdef _SGI
    magic = head.f_magic;
#endif
#ifdef _AIX
    magic = head.f_magic;
#endif
#ifdef _APOLLO
    magic = head.f_magic;
#endif
#ifdef _LINUX
    magic = (short) head.a_info;
#endif
#ifdef _BSDI
	magic = head.a_magic;
#endif
#ifdef _NETBSD
        magic = N_GETMAGIC (head);
#endif

    if ( ISMAG (magic) 
	|| (!strncmp ( (char *) &head, (char *) "!<arch>", 6)) ) {
	DisplayMessage  ( text -> mwin, "%@#7&^/_!...brrr..." );
	return True;
    }
    return False ;
}


/*
**	Function name : MenuReadFile
**
**	Description : Commande 'read file' a partir du menu.
**	Input : Le text courant.
**	Ouput :
*/
void MenuReadFile ( text )
    Text *text;
{
    char *str = 0;
    char *color;
    Text *result;
    char c = '\007'; /* ^G */
    
    if ( GetModif ( text ) == True ) {
	if ( SaveCurrentBuffer ( text, F_MENU ) != True )
	  return;
    }
    ClearMessageWindow ( text -> mwin );

    str = (char *) SelectFileFromBox ( "Read File" );
    
    if ( (str == 0) || (strncmp(str, &c, 1)) == 0 ) {
	DisplayMessage ( text -> mwin, "Abort" );
	return;
    }

    TextCursorOff ( text );

    if ( LoadFile ( text, str, NEW ) != -1 ) {
	ShowScrollFrame ( dpy, text -> swin );
	FirstPage ( text );
	SetTextSave ( text );
	if ( IsAlreadyLoad ( text -> filename, text, &result ) > 1 ) {
	    XBell ( dpy, 10 );
	    DisplayMessage ( text -> mwin, "Warning ...Already loaded" );
	    KillBuffer ( text );
	    XMapRaised ( dpy, result -> w_parent );
	}
    }
    TextCursorOn ( text );
    color = (char *) getenv("XCORAL_AUTO_HIGHLIGHT");
    if ( color != 0 && strcmp(color,"True")== 0)
      ColorBuffer(text);
}


/*
**	Function name : MenuInsertFile
**
**	Description : Commande 'insert file' a partir du menu.
**	Input : Le text courant.
**	Ouput :
*/
void MenuInsertFile ( text )
    Text *text;
{
    char *str = 0;
    char c = '\007'; /* ^G */
    int nbl, olen;
    int x_len;
    
    ClearMessageWindow ( text -> mwin );
    str = (char *) SelectFileFromBox ( "Insert File" );
    if ( (str == 0) || (strncmp(str, &c, 1)) == 0 ) {
	DisplayMessage ( text -> mwin, "Abort" );
	return;
    }
    TextCursorOff ( text );
    nbl = text -> lines_in_buf;
    olen = BottomBuf (text -> buf) - RightBuf (text -> buf);
    
    if ( LoadFile ( text, str,  INSERT ) != -1 ) {
	SetTextModif ( text );
	SetAndDisplayPage ( text );
	ShowScrollFrame ( dpy, text -> swin );
	(void) MoveScrollBar ( dpy, text -> swin, 
			      CURRENT, text -> no_current_line - text -> n1 - 1 );
	x_len = (BottomBuf (text -> buf) - RightBuf (text -> buf)) - olen;
	StoreInUndo ( text, RightBuf (text -> buf) , (char *) 0, x_len,
		     text -> lines_in_buf - nbl, U_INSERT );
	/* Pour la couleur */
	if ( text -> current_ce != 0 ) {
	  (void) UpdateColorList ( text, x_len );
	  ClipOn ( text, 0 );
	  RefreshPage ( text );
	  ClipOff ( text );
	}
    }
    TextCursorOn ( text );
}


/*
**	Function name : MenuSaveFile
**
**	Description : Commande 'save file' a partir du menu.
**	Input : Le text courant.
**	Ouput :
*/
void MenuSaveFile ( text )
    Text *text;
{
    char   *name;
    char c = '\007';
    
    if ( strcmp (text -> filename, "NoName") == 0 ) {
	name = (char *) GetStringFromDB ( "Write file : ", False );
	
	if ( (name == 0) || strncmp(name, &c,1) == 0 ) {
	    DisplayMessage ( text -> mwin, "Abort" );
	    return;
	}
	else {
	    if ( *name == '~' )
	      name = ExpandTildeName ( name );
	    
	    if ( *name == '/' ) {
		(void) strcpy ( text -> filename, name );
	    }
	    else {
		(void) strcpy ( text -> filename,  text -> current_dir );
		(void) strcat ( text -> filename, "/" );
		(void) strcat ( text -> filename, name );
	    }
	    WriteFile ( text );
	    if ( write_flag )
	      SetWindowName ( text );
	}
    }
    else {
#ifdef DEBUG
	(void) sprintf ( buf, "cp %s '#%s'\n",
			text -> filename, text -> filename );
	system ( buf );
#endif
	if ( text -> modif == True )
	  WriteFile ( text );
	else
	  DisplayMessage ( text -> mwin, "No changes" );
    }
}


/*
**	Function name : MenuWriteFile
**
**	Description : Commande 'write file' a partir du menu.
**	Input : Le text courant.
**	Ouput :
*/
void MenuWriteFile ( text )
    Text *text;
{
    char *s;
    char buf [128];
    char c = '\007';
    char *name;
    char tmp_name [1024];
    
#ifdef DEBUG
    (void) fprintf ( stderr, "Write File from menu\n" );
#endif
    
    if ( (name = (char * ) strrchr ( text -> filename, '/' )) != 0 ) {
      name ++; /* On vire le slash */
      strcpy ( tmp_name, name );
    }
    else
	strcpy ( tmp_name, text -> filename );

    if ( strlen (tmp_name) > MAX_NAME_LEN )
      strcpy ( tmp_name + (MAX_NAME_LEN-4), "..." );
    (void) sprintf ( buf, "Write file [%s] : ", tmp_name );
    
    s = GetStringFromDB ( buf, False );
    
    if ( (s == 0) || ((strncmp(s,"y",1) == 0)&&(strlen(s)==1)) ) {
	WriteFile ( text );
    }
    else if ( ((strncmp(s,"n",1 ) == 0)&&(strlen(s)==1)) || (strncmp(s,&c,1)== 0) ) {
	DisplayMessage ( text -> mwin, "Abort" );
    }
    else {
	if ( *s == '~' )
	  s = ExpandTildeName ( s );
	
	if ( *s == '/' ) {
	    (void) strcpy ( text -> filename, s );
	}
	else {
	    (void) strcpy ( text -> filename,  text -> current_dir );
	    (void) strcat ( text -> filename, "/" );
	    (void) strcat ( text -> filename, s );
	}
	WriteFile ( text );
	if ( write_flag ) {
	  SetDirAndFilename ( text, s );
	  SetWindowName ( text );
	}
    }
}


/*
**	Function name : SaveCurrentBuffer
**
**	Description : Sauve le buffer courant.
**	Input : Le text courant, clavier/menu.
**	Ouput :
*/
int SaveCurrentBuffer ( text, from )
    Text *text;
    int from;
{
    char *str = 0;
    char c = '\007'; /* ^G */
    char *buf, *name;
    char tmp_name [1024];
    extern char *sys_errlist[];
    
    buf = (char *) malloc ( (unsigned) strlen ( text -> filename ) + 64 );
    
    if ( (name = (char * ) strrchr ( text -> filename, '/' )) != 0 ) {
      name ++; /* on vire le slash */
      strcpy ( tmp_name, name );
    }
    else 
      strcpy ( tmp_name, text -> filename );

    if ( strlen (tmp_name) > MAX_NAME_LEN )
      strcpy ( tmp_name + (MAX_NAME_LEN-4), "..." );
    (void) sprintf ( buf, "Save buffer [%s] ? [y/n] : ", tmp_name );

    if ( from == F_KEY )
      str = (char * ) GetString ( text, buf, 0);
    else 
      str = (char * ) GetStringFromDB ( buf, False );

    while ( True ) {
	if ( (str == 0) || ((strncmp(str,"y",1) == 0)&&(strlen(str)==1)) ) {
	    WriteFile ( text );
	    if (write_flag != True) {
	      DisplayWMessage ( sys_errlist[errno], "Save current buffer", True );
	      if ( buf != 0 )
		(void) free ( buf );
	      return False;
	    }
	    break;
	}
	if ( strncmp(str,&c,1) == 0 ) {
	    DisplayMessage ( text -> mwin, "Abort" );
	    if ( buf != 0 )
	      (void) free ( buf );
	    return False;
	}
	if ( ((strncmp(str,"n",1) == 0)&&(strlen(str)==1)) ) {
	    break;
	}
        if ( from == F_KEY )
          str = (char *) GetString ( text, "Please answer y or n : ", 0 );
        else
          str = (char * ) GetStringFromDB ( "Please answer y or n : ", True  );
    }
    if ( buf != 0 )
      (void) free ( buf );
    return True;
}


/*
**	Function name : MenuGotoLine
**
**	Description : Va a la ligne n.
**	Input : Le text courant.
**	Ouput :
*/
void MenuGotoLine ( text )
    Text *text;
{
    int n;
    char *str;
    char c = '\007';
    
    str = (char * ) GetStringFromDB ( "Line number : ", False );
    if ( (str == 0) || (strncmp (str, &c, 1 ) == 0) || ((n = atoi ( str )) == 0) )
      DisplayMessage ( text -> mwin, "Abort" );
    else {
	TextCursorOff ( text );
	StorePosition(text);
	GotoLineNumber ( text, n );
	SetPosition(text);
	UpdatePage(text);
	TextCursorOn ( text );
    }
}


/*
**	Function name : KillCurrentBuffer
**
**	Description : Comme son nom l'indique.
**	Input : Le text courant.
**	Ouput :
*/
static void KillCurrentBuffer ( text, from )
    Text *text;
    int from;
{
    if ( GetModif ( text ) == True ) {
	if ( from == F_KEY ) {
	    if ( SaveCurrentBuffer ( text, F_KEY ) != True )
	      return;
	}
	else {
	    if ( SaveCurrentBuffer ( text, F_MENU ) != True )
	      return;
	}
    }
    KillText ( dpy, text );
    StorePosition ( text );
    RefreshScroll ( dpy,  text -> swin, 
		   text -> width + GetScrollWidth () + W_SPACE + 1, text -> height, 0 );
    SetTextSave ( text );
    SetDirAndFilename ( text, "NoName" );
    XSetIconName ( dpy,  text ->  w_parent, "NoName" );
    XStoreName ( dpy, text ->  w_parent, "NoName" );
    ResetMark ( text );
}


/*
**	Function name : KillBuffer
**
**	Description : 
**	Input : 
**	Ouput :
*/
void KillBuffer ( text )
    Text *text;
{
    TextCursorOff ( text );
    KillCurrentBuffer ( text, F_KEY );
    TextCursorOn ( text );
}

/*
**	Function name : MenuNewFile
**
**	Description : 
**	Input : 
**	Ouput :
*/
void MenuNewFile ( text )
    Text *text;
{
    TextCursorOff ( text );
    KillCurrentBuffer ( text, F_MENU );
    TextCursorOn ( text );
}


/*
**	Function name : LoadAndEvalFile
**
**	Description : 
**	Input : 
**	Ouput :
*/
void LoadAndEvalFile ( text )
    Text *text;
{
    char *msg = 0;
    char *s = 0;
    char c = '\007';
    char buf[128];
 
    StorePosition ( text );
    (void) sprintf ( buf, "Load and eval [.xcoralrc]  : " );
    
    s = SelectFileFromBox ( "Load and eval file" );
    if ( (s == 0) || (strncmp(s, &c, 1)) == 0 ) {
	DisplayMessage ( text -> mwin, "Abort" );
	return;
    }
    else {
      int nw = text->win_id;
      
      if ( *s == '~' )
	s = ExpandTildeName ( s );
	
      msg = (char * ) ie_load_file ( text, s );
      update_cwd(nw);
    }
    
    if ( msg != 0 )  {
      (void) DisplayWMessage ( msg , " load_file error : ", True );
      return;
    }
    ie_redisplay ( text );
}

/*
**	Function name : SetIconName
**
**	Description :
**	Input :
**	Output :
*/
void SetWindowName ( text )
    Text *text;
{
    char *icon_name;
    
    icon_name = strrchr ( text -> filename, '/' );
    if ( icon_name )
      icon_name ++;
    else
      icon_name = text -> filename;
    XSetIconName ( dpy,  text ->  w_parent, icon_name );
    XStoreName ( dpy, text ->  w_parent, text -> filename );
}

/*
**	Function name : update_cwd
**
**	Description :
**	Input :
**	Output :
*/
Text * update_cwd(win_id)
    int win_id;
{
  if ( TWin [win_id] == 0 )
    for (win_id = 0; ; win_id += 1)
      if (TWin [win_id] != 0 )
	break;
    
  chdir(TWin [win_id]->text->current_dir);
  return TWin [win_id]->text;
}

