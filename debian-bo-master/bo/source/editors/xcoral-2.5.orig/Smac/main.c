/* ########################################################################

				 main.c

   File: main.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/main.c
   Description: 
   Created: Tue Feb 21 12:56:55 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:56:55 MET 1995
   Last maintained by: Bruno Pages

   RCS $Revision$ $State$
   

   ########################################################################

   Note: 

   ########################################################################

   Copyright (c) : Bruno Pages

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
#include <pwd.h>

#include "smac.h"
#include "error.h"

extern FILE * yyin;

char *ExpandTildeName ( name )
    char *name;
{
    struct passwd *pwd;
    char *home;
    char *p = name + 1;
    static char  buf [256];
    
    if ( name [1] == '/' ) {        /* c'est moi */
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

void main()
{
#ifdef RUNTIMECHECK
  char * m = init_smac(1024, 15);	/* taille pile = 1024,
					   tas = 2^15 octets */
  if (m) {
    fputs(m, stderr);

    exit(1);
  }
#else
  init_smac(1024);			/* taille pile */
#endif

  (void) signal(SIGINT, error_ctrl_c);
  
  /* Lecture de stdin jusqu'a la fin de fichier */

  for (;;) {
    if (! (setjmp(come_back))) {
      load_file();
      /* ya eu une erreur de lecture */
      if (*err_msg)
	Error(err_msg);
    }
    /* ya eu erreur */
    putchar('\n');
  }
}
