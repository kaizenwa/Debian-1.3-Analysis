/* Format.h - Declarations for af's display format handling.
   Copyright (C) 1992, 1993, 1994, 1995, 1996 Malc Arnold.

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

#ifndef lint
static char *FormatId = "$Id: format.h,v 1.9 1996/03/17 01:10:47 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* The functions used to add the contents of a conversion */

static char *m_arrow(), *m_chars(), *m_lines(), *m_date(), *m_timedate();
static char *m_encoding(), *m_line(), *m_name(), *m_addr(), *m_subject();
static char *m_tags(), *m_type(), *m_fulltype();
static char *b_flags(), *b_name(), *b_file(), *b_modes(), *b_lines();
static char *b_pos(), *p_percent(), *p_version();

/****************************************************************************/
/* The structure defining the available conversions */

typedef struct {
	char conv;			/* Conversion specifier */
	int width;			/* Default width */
	char scaled;			/* Scale the width? */
	char always;			/* Always printed? */
	char message;			/* Message-based? */
	char *(*func)();		/* Function to get contents */
} CONVERSION;

/****************************************************************************/
/* The structure used to store lengths of a format */

typedef struct {
	int length;					/* Length of format */
	int scaled;					/* Scaled length */
} FORMAT_LEN;

/****************************************************************************/
/* Characters output during screen handling */

#define P_LEN		3
#define P_ALL		"ALL"
#define P_TOP		"TOP"
#define P_BOT		"BOT"
#define P_MOR		"MOR"

#define BS_LEN		2

#define FILL_CHAR	' '
#define MODE_FILL	'='
#define CONT_CHAR	'$'

/****************************************************************************/
/* A macro to determine if a character starts a conversion */

#define ISCONV(x)	((x) == '%')

/* And one to determine if a character is considered whitespace */

#define ISFSPACE(x, m)	(isspace(x) || (m) && (x) == '=')

/****************************************************************************/
/* The buffer size for allocating converted lines */

#define LINEBUFSIZ	128

/****************************************************************************/
/* The list of available conversions*/

static CONVERSION conversions[] = {
	{ '*',	-2,	FALSE,	FALSE,	FALSE,	b_flags },
	{ '%',	-1,	FALSE,	TRUE,	TRUE,	p_percent },
	{ 'a',	-2,	FALSE,	TRUE,	FALSE,	m_arrow },
	{ 'b',	-20,	TRUE,	FALSE,	FALSE,	b_name },
	{ 'c',	5,	FALSE,	FALSE,	TRUE,	m_chars },
	{ 'C',	4,	FALSE,	FALSE,	TRUE,	m_lines },
	{ 'd',	-6,	FALSE,	FALSE,	TRUE,	m_date },
	{ 'D',	-12,	FALSE,	FALSE,	TRUE,	m_timedate },
	{ 'e',	-16,	TRUE,	FALSE,  TRUE,	m_encoding },
	{ 'f',	-20,	TRUE,	FALSE,	FALSE,	b_file },
	{ 'k',	-15,	TRUE,	FALSE,	TRUE,	m_type },
	{ 'K',	-15,	TRUE,	FALSE,	TRUE,	m_fulltype },
	{ 'l',	4,	FALSE,	FALSE,	TRUE,	m_line },
	{ 'm',	-12,	FALSE,	FALSE,	FALSE,	b_modes },
	{ 'n',	4,	FALSE,	FALSE,	FALSE,	b_lines },
	{ 'o',	-20,	TRUE,	FALSE,	TRUE,	m_name },
	{ 'O',	-20,	TRUE,	FALSE,	TRUE,	m_addr },
	{ 'p',	-3,	FALSE,	FALSE,	FALSE,	b_pos },
	{ 's',	-30,	TRUE,	FALSE,	TRUE,	m_subject },
	{ 't',	-6,	FALSE,	FALSE,	TRUE,	m_tags },
	{ 'v',	-4,	FALSE,	TRUE,	TRUE,	p_version },
	{ '\0',	0,	FALSE,	FALSE,	FALSE,	NULL }
};

/****************************************************************************/
