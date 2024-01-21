/* Getcwd.c - Get the working directory.
   Copyright (C) 1992, 1993, 1994, 1996 Malc Arnold.

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
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include <stdio.h>
#include <errno.h>

/****************************************************************************/
/* RCS info. */

#ifndef lint
static char *RcsId = "$Id: getcwd.c,v 1.3 1996/03/17 01:08:15 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/*LINTLIBRARY*/
/****************************************************************************/
/* Global function declarations */

extern FILE *popen();
extern int pclose();

/****************************************************************************/
/* Import the system error number */

extern int errno;

/****************************************************************************/
char *getcwd(buf, len)
char *buf;
int len;
{
	/* Put the current directory in buf and return status */

	FILE *fp;

	/* Open the pwd executable in a pipe */

	if ((fp = popen("pwd", "r")) == NULL) {
		return(NULL);
	}

	/* Read the output - the working directory */

	if (fgets(buf, len, fp) == NULL) {
		return(NULL);
	}

	/* Check for newline and trim it */

	if (buf[strlen(buf) - 1] != '\n') {
		errno = EIO;
		return(NULL);
	}
	buf[strlen(buf) - 1] = '\0';

	/* Close the pipe and return the buffer */

	(void) pclose(fp);
	return(buf);
}
/****************************************************************************/
