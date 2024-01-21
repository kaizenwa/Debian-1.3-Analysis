/* Io.h - Declarations for input redirection within af
   Copyright (C) 1995, 1996, 1997 Malc Arnold.

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


/****************************************************************************/
/* RCS info */

#define IOID	"$Id: io.h,v 1.4 1997/03/31 18:32:19 malc Exp $"

/****************************************************************************/
/* The possible header options when writing a message to a file */

#define HS_MBOX		0
#define HS_NONE		1
#define HS_FROM		2
#define HS_SHOW		3
#define HS_COPY		4
#define HS_ALL		5

/****************************************************************************/
/* The statuses returned by write-messages */

#define WM_OK		0
#define WM_RESYNC	1
#define WM_REREAD	2
#define WM_FAILED	3

/****************************************************************************/
/* The structure to hold the current input functions */

typedef struct iofunc {
	int (*key)();			/* Function to get a key */
	int (*type)();			/* Function to check for typeahead */
	char *(*line)();		/* Function to read a line */
	KEYSEQ *(*seq)();		/* Function to read a key sequence */
} IOFUNC;

/****************************************************************************/
