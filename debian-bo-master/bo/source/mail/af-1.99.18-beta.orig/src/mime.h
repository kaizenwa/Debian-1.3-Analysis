/* Mime.h - Declarations for af MIME header handling.
   Copyright (C) 1994, 1996 Malc Arnold.

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

#define	MIMEID		"$Id: mime.h,v 1.3 1996/03/17 01:10:47 malc Exp $"

/****************************************************************************/
/* The structures used to store a parsed Content-Type */

typedef struct mimeparam {
	char *name, *value;		/* Parameter name and value */
	struct mimeparam *next;		/* Pointer to next parameter */
} MIMEPARAM;

typedef struct {
	char *type, *subtype;		/* MIME type and subtype */
	MIMEPARAM *params;		/* Parameters */
} MIMETYPE;

/****************************************************************************/
/* Values used when checking a message is textual */

#define TEXTTYPE	"text"
#define TEXTSUBTYPE	"plain"
#define TEXTCHARSET	"charset"
#define ASCIICHARSET	"us-ascii"
#define TEXTENCODINGS	{ "7bit", "8bit", "binary", NULL }

/****************************************************************************/
