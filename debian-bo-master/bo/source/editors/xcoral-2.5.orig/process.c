/* ########################################################################

			       process.c

   File: process.c
   Path: /home/fournigault/c/X11/xcoral-2.31/process.c
   Description: 
   Created: Fri Jan 27 11:25:54 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:25:55 MET 1995
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
#include <fcntl.h>

#ifndef apollo
#ifdef __FreeBSD__
#include <stdlib.h>
#else /* !__FreeBSD__ */
#include <malloc.h>
#endif
#endif

#include "xcoral.h"
#include "main_text.h"
#include "handle_key.h"
#include "process.h"
#include "text_cursor.h"
#include "page.h"
#include "main_events.h"
#include "chars_cmds.h"

FCT (static void, CleanFd, (Text *text) );
FCT (static void, GetExitShell, () );

/*
**	Function name : InitShellMode
**
**	Description : 
**	Input : 
**	Ouput :
*/
void InitShellMode ()
{
    extern void ie_WR_delete ();
    int i;
    Mode *shell_mode;
    
    shell_mode = (Mode *) GetMode ("Shell");
/*   for ( i=0; i< 512; i++ ) { */
    for ( i=32; i< 512; i++ ) { 
	shell_mode -> key [i].func = shell_key;
	shell_mode -> key [i].type = BUILTIN_FUNC;
/*	
	shell_mode -> ctrX_key [i].func = 0;
	shell_mode -> ctrX_key [i].type = 0;
	shell_mode -> esc_key [i].func = 0;
	shell_mode -> esc_key [i].type = 0;
*/	
    }
    shell_mode -> key [DELETE].func = ie_WR_delete;
    shell_mode -> key [DELETE].type = BUILTIN_FUNC;
    shell_mode -> key [CtrC].func = shell_CtrC;
    shell_mode -> key [CtrC].type = BUILTIN_FUNC;
    shell_mode -> key [CtrD].func = shell_CtrD;
    shell_mode -> key [CtrD].type = BUILTIN_FUNC;
    shell_mode -> key [CtrH].func = ie_WR_delete;
    shell_mode -> key [CtrH].type = BUILTIN_FUNC;
    shell_mode -> key [CtrM].func = shell_return;
    shell_mode -> key [CtrM].type = BUILTIN_FUNC;
}

/*
**	Function name : SetShellMode
**
**	Description : Passe le buffer courant en mode Shell si
**          il n'y est pas deja. Fork/exec et connection des
**          des entrees-sorties des 2 processes,
**
**	Input : Le Text courant.
**	Ouput :
*/
void SetShellMode ( text )
    Text *text;
{
    int pid, c_pid;
    char *shell;
    
    if ( strcmp(text -> current_mode -> name, "Shell") != 0 ) {
	text -> current_mode = (Mode *) GetMode ("Shell");
	text -> mwin -> mode = text -> current_mode;
	RefreshWindowMode ( text -> mwin );
	if ( (text -> current_mode -> font != text -> font)
	    && (text -> current_mode -> font != 0) )
	  RefreshWithNewFont ( text, text -> current_mode -> font );
	(void) strcpy ( text -> filename, "NoName");
    }
    else
      return;

    (void) pipe ( text -> p_shell );
    (void) pipe ( text -> p_xc );
    switch ( pid = fork() ) {
    case 0:
      (void) close (0);
      (void) dup ( text -> p_shell [0] ); /* Entree standard du shell */
      (void) close (1);
      (void) dup ( text -> p_xc [1] ); /* Sortie standard du shell */
      setbuf ( stdout, 0 ); /* Pas de bufferisation */
      (void) close (2);
      (void) dup ( text -> p_xc [1] ); /* Sortie d'erreur du shell */
      (void) close ( text -> p_shell [0] );
      (void) close ( text -> p_shell [1] );
      (void) close ( text -> p_xc [0] );
      (void) close ( text -> p_xc [1] );
      c_pid = getpid();
      (void) setpgrp ( c_pid, c_pid);
      shell = (char *) getenv("XCORAL_SHELL");
      if (shell == NULL) {
	shell = (char *) getenv("SHELL");
	if (shell == NULL) {
	  shell = (char *) malloc(strlen("/bin/csh")+2);
	  strcpy(shell,"/bin/csh");
	}
      }
#ifdef linux
	(void) execlp (shell, shell, "-i", (char *)0);
#else
      (void) execlp (shell, shell, "-i", 0);
#endif
      (void) fprintf ( stderr, "Can't exec shell\n" );
      break;
    case -1:
      perror ((char *)0);
      break;
    default:
      text -> shell_id = pid;
      text -> from_shell = text -> p_xc [0];
      text -> to_shell = text -> p_shell [1];
      (void) close ( text -> p_shell [0] );
      (void) close ( text -> p_xc [1] );
      break;
    }
    /* Pour la fonction select de la main_loop */
    SetFd ( text -> from_shell );
    text -> s_pos = 0;
    text -> s_line = 0;
}

/*
**	Function name : KillShell
**
**	Description : Cette fonction n'est appelable qu'a partir
**          du buffer courant. On peut donc tuer directement le
**          le shell associe si shell il y a.
**
**	Input : Le Text courant.
**	Ouput :
*/
void KillShell ( text )
    Text *text;
{
    if ( text -> shell_id == 0 )
	return;
    (void) kill ( text -> shell_id, SIGKILL );
    (void) GetExitShell ();
}


/*
**	Function name : GetExitShell
**
**	Description : Un shell associe a l'un des buffers
**          a ete tue soit par ^D soit sauvagement de l'exterieur.
**          Le wait doit rendre la main de suite (sinon on est mal)
**          et le zombie disparait.
**          Ensuite on cherche a savoir quel etait le buffer associe
**          a ce shell maintenant disparu et on fait le menage en
**          repassant en mode par default.
**          
**	Input : 
**	Ouput :
*/
static void GetExitShell ()
{
    int pid;
    int statusp;
    EdWin **t;
    Text *text = 0;

    pid = wait ( &statusp );
    
    for ( t = TWin; t < TWin + MAXWIN; t++ ) {
	if ( *t == 0 ) continue;
	if ( (*t) -> text -> shell_id == pid ) {
	    text = (*t) -> text;
	    break;
	}
    }
    if ( text == 0 ) {
	(void) fprintf ( stderr, "Exit shell error\n" );
	return;
    }
    
    (void) CleanFd ( text );
    SetDefaultMode ( text );
    TextCursorOff ( text );
    ClipOn ( text, 0 );
    InsertLines ( text , "Bye...\n",7 , 1);
    RefreshScrollBar ( dpy, text -> swin );
    TextCursorOn ( text );
    ClipOff ( text );
}


/*
**	Function name : CleanFd
**
**	Description : Mise a zero des descripteurs associes
**          au shell ( qui a disparu maintenant ).
**
**	Input : Le Text courant.
**	Ouput :
*/
static void CleanFd ( text )
    Text *text;
{
    ResetFd ( text -> from_shell );
    (void) close ( text -> to_shell );
    (void) close ( text -> from_shell );
    
    text -> shell_id = 0;
    text -> to_shell = 0;
    text -> from_shell = 0;
}

/*
**	Function name : shell_return
**
**	Description : Envoie d'un commande au shell
**	Input : Le Text courant.
**	Ouput :
*/
void shell_return ( text )
    Text *text;
{
    int len;
    char *s, *to_send;

    s = (char *) GetCurrentLine ( text -> buf, &len );

    if ( text -> s_line != text -> no_current_line ) {
	text -> s_line = text -> no_current_line;
	text -> s_pos = 0;
	f_return ( text );
	(void) write ( text -> to_shell, "\n", 1 );
	return;
    }

/*    if ( (s == 0) || (text -> s_pos == 0) || (len <= text -> s_pos)) {*/
    if ( (s == 0) || (len <= text -> s_pos)) {
	f_return ( text );
	(void) write ( text -> to_shell, "\n", 1 );
	return;
    }

    s += text -> s_pos;
    if ( s != 0 ) {
	to_send = (char *) malloc ( (unsigned) len + 2 );
	strncpy ( to_send, s, len );
	to_send [len] =  '\n';
	to_send [len+1] = '\0';
	f_return ( text );
	if ( len == 0 ) {
	    (void) write ( text -> to_shell, "\n", 1 );
	    return;
	}
	(void) write ( text -> to_shell, to_send, strlen(to_send));
	(void) free ( to_send );
    }
}

/*
**	Function name : SetPosInShell
**
**	Description : Recherche de la position de la commande
**          qui sera envoyee au Shell. Cette fonction permet
**          de s'affranchir du prompt.
**    
**	Input : Le Text courant.
**	Ouput :
*/
void SetPosInShell ( text )
    Text *text;
{
    if ( text -> s_line == 0 ) { /* Fisrt time */
	text -> s_line = text -> no_current_line;
	/* La longueur du prompt */
	text -> s_pos = GetNcFromLeft ( text -> buf ); 
    }
    else {
	if ( text -> s_line != text -> no_current_line ) {
	    text -> s_line = text -> no_current_line;
	    text -> s_pos = GetNcFromLeft ( text -> buf );
	}
    }
}

/*
**	Function name : Shell_key
**
**	Description : Cette fonction est liee a toutes les touches
**          caracteres du clavier.
**	Input : 
**	Ouput :
*/
void shell_key ( text )
    Text *text;
{
    extern int last_key;

    SetPosInShell ( text );
    f_impc ( text, last_key );
    SetTextModif ( text );
}

/*
**	Function name : shell_CtrC
**
**	Description : 
**	Input : 
**	Ouput :
*/
void shell_CtrC ( text )
    Text *text;
{
    
    TextCursorOff ( text );
    ClipOn ( text, 0 );
    InsertLines ( text , "^C", 2, 0);
    RefreshScrollBar ( dpy, text -> swin );
    TextCursorOn ( text );
    ClipOff ( text );
    
#if defined(SYSV) || defined(hpux)
    (void) kill( - getpgrp(text -> shell_id), SIGINT );
#else
#ifdef SVR4 /* also UNIXWARE */
	(void) sigsend(P_PGID, getpgrp(text -> shell_id), SIGINT );
#else
    (void) killpg ( getpgrp(text -> shell_id), SIGINT );
#endif
#endif
}

/*
**	Function name : shell_CtrD
**
**	Description : Envoi d'un ^D au Shell
**
**	Input : Le Text courant.
**	Ouput :
*/
void shell_CtrD ( text )
    Text *text;
{
    TextCursorOff ( text );
    ClipOn ( text, 0 );
    InsertLines ( text , "^D", 2, 0);
    RefreshScrollBar ( dpy, text -> swin );
    TextCursorOn ( text );
    ClipOff ( text );

    f_return ( text );
    (void) write ( text -> to_shell, "exit\n", 5 );
}

/*
**	Function name : ReadFromShell
**
**	Description : Lecture des caracteres envoyes par le
**          Shell. On essaye d'en lire le maximum d'un coup.
**
**	Input : Le Text courant.
**	Ouput :
*/
void ReadFromShell ( text )
    Text *text;
{
    int n, lines;
    char tmp [BUFSIZ];
    extern Display *dpy;
    
    (void) fcntl ( text -> from_shell, F_SETFL, O_NDELAY );
    n = read ( text -> from_shell, tmp,BUFSIZ );
    if ( n == 0 || n == -1) {
	/* Ya plus de shell */
	(void) GetExitShell ();
	return;
    }

    TextCursorOff ( text );
    ClipOn ( text, 0 );
    lines = GetNewLine ( tmp, n);
    SetTextModif ( text );
    if ( lines == 0 ) {
	InsertNchar ( text -> buf, tmp, n);
/*	ClearLine ( text, text -> n1 + 1 );*/
	SetCurrentLine ( text );
    }
    else {
	InsertLines ( text, tmp, n, lines );
	RefreshScrollBar ( dpy, text -> swin );
    }
    TextCursorOn ( text );
    ClipOff ( text );
}
