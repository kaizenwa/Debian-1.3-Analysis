/* gettext.c - GNU internationalization runtime support

   Copyright (C) 1994 Free Software Foundation, Inc.

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

   Written by Jim Meyering <meyering@gnu.ai.mit.edu>.  */

#if HAVE_CONFIG_H
# include <config.h>
#endif

/* How many bytes more to reallocate, each time needed.  */
#define REALLOC_STEP 500

#if __STDC__
# define voidstar void *
#else
# define voidstar char *
#endif

#ifdef STDC_HEADERS
# include <stdlib.h>
#else
char *getenv ();
voidstar malloc ();
#endif

#include <stdio.h>
#include <sys/types.h>

/* GNU small library functions.  */

#ifndef __P
# if PROTOTYPES
#  define __P(Args) Args
# else
#  define __P(Args) ()
# endif
#endif

#include <libintl.h>

voidstar xmalloc __P ((size_t));
voidstar xrealloc __P ((voidstar, size_t));
char *xstrdup __P ((const char *));

/* Debugging the memory allocator.  */

#if WITH_DMALLOC
# define MALLOC_FUNC_CHECK
# include <dmalloc.h>
#endif

/*---------------------------------------------------------------.
| Do backslashed sequence interpolation for BUFFER over itself.	 |
`---------------------------------------------------------------*/

static void
interpolate_escapes (char *buffer)
{
  char *cursor;

  /* Skip without copying everything before the first backslash.  */

  while (*buffer && *buffer != '\\')
    buffer++;

  /* Then edit the remainder of the buffer over itself.  */

  cursor = buffer;
  while (*cursor)
    if (*cursor == '\\')
      {
	cursor++;
	if (*cursor)
	  switch (*cursor)
	    {
	    default:
	      *buffer++ = *cursor++;
	      break;

	    case '\n':
	      /* Ignore newline escapes.  */
	      cursor++;
	      break;

	    case 'a':
	      *buffer++ = '\007';
	      cursor++;
	      break;

	    case 'b':
	      *buffer = '\b';
	      cursor++;
	      break;

	    case 'f':
	      *buffer++ = '\f';
	      cursor++;
	      break;

	    case 'n':
	      *buffer++ = '\n';
	      cursor++;
	      break;
	      
	    case 't':
	      *buffer++ = '\t';
	      cursor++;
	      break;
	    }
      }
    else
      *buffer++ = *cursor++;

  /* Ensure a buffer terminator.  */

  *buffer = '\0';
}

/*----------------------------------------------------------.
| Get all strings for product according to current locale.  |
`----------------------------------------------------------*/

/* Array of localized strings.  */
static const char **locale_string = NULL;

static void
get_all_strings (void)
{
  const struct _msg_ent *entry;	/* to scan message entries */
  const char *language;		/* value of LANG environment variable */
  char *name;			/* complete filename of message catalog */
  FILE *file;			/* bufferred file to read catalog */

  int code;			/* current code number being scanned */
  int value;			/* possible value of next code number */
  int character;		/* character under examination */
  char *buffer;			/* buffer to hold constructed strings */
  int length;			/* used length in buffer */
  int allocated;		/* allocated length of buffer */
  int scanned;			/* length scanned in buffer */
  char *cursor;			/* scanning cursor in buffer */

  /* Attempt allocating memory for the table of translated strings.  If
     no memory is available, return with locale_string still NULL.  This
     will be detected in gettext ().  Further, this routine will be tried
     again on the next message to translate.  */

  locale_string = (const char **) malloc (_msg_tbl_length * sizeof (char *));
  if (!locale_string)
    return;

  /* Initialize locale_string such that untranslated strings will yield
     themselves.  So if anything goes wrong later with memory allocation,
     at least the English version of diagnostics is available, and since
     locale_string is now set, this routine will not be called again.  */

  for (entry = _msg_tbl; entry < _msg_tbl + _msg_tbl_length; entry++)
    locale_string[entry->_msg_number - 1] = entry->_msg;

  /* Do nothing else if LANG environment variable is not set.  */

  language = getenv ("LANG");
  if (!language)
    return;

  /* Try to open the catalog, do nothing else if not found.  */

  name = (char *) xmalloc (sizeof (LOCALEDIR) + strlen (language)
			   + strlen (_msg_catalog_file_name) + 3);
  sprintf (name, "%s/%s/%s", LOCALEDIR, language, _msg_catalog_file_name);
  if (file = fopen (name, "r"), !file)
    {
      free (name);
      return;
    }

  /* Prepare to read the whole catalog.  */

  code = 0;
  length = 0;
  scanned = 0;
  allocated = REALLOC_STEP;
  buffer = (char *) xmalloc (allocated);

  while (character = getc (file), character != EOF)
    {

      /* Add the character to buffer, extending it as necessary.  */

      if (length == allocated)
	{
	  allocated += REALLOC_STEP;
	  buffer = (char *) xrealloc (buffer, allocated);
	}
      buffer[length++] = character;

      /* Scan each line separately.  */

      if (character == '\n')
	{

	  /* Reread the last line, checking it for `$ #[1-9][0-9]*'.  This
	     also has to be preceded by two newlines.  */

	  cursor = buffer + scanned;
	  if (scanned >= 2 && buffer[scanned - 2] == '\n'
	      && scanned + 3 < length && cursor[0] == '$' && cursor[1] == ' '
	      && cursor[2] == '#' && cursor[3] >= '1' && cursor[3] <= '9')
	    {

	      /* Decode the number value.  */

	      cursor += 3;
	      value = *cursor++ - '0';
	      while (*cursor >= '0' && *cursor <= '9')
		value = 10 * value + *cursor++ - '0';

	      /* Check if the number ends the line and has a legal value.  */

	      if (*cursor == '\n' && value <= _msg_tbl_length
		  && buffer[0] == '#' && buffer[1] == ' ')
		{

		  /* Cut the last line out of the string, and save the
		     buffer as a translation if a code number is defined.  */

		  if (code > 0)
		    {
		      buffer[scanned - 2] = '\0';
		      interpolate_escapes (buffer + 2);
		      locale_string[code - 1] = xstrdup (buffer + 2);
		    }

		  /* Initialize for reading a new translation.  */

		  code = value;
		  length = 0;
		}
	    }

	  /* Prepare for scanning a new line.  */

	  scanned = length;
	}
    }

  /* Ensure a string terminator, and save the buffer as a translation if
     a code number is defined.  */

  if (code > 0 && buffer[0] == '#' && buffer[1] == ' ')
    {
      if (scanned < length)
	{
	  if (length < allocated)
	    buffer = (char *) xrealloc (buffer, allocated + 1);
	  length++;
	}
      buffer[length - 1] = '\0';
      interpolate_escapes (buffer + 2);
      locale_string[code - 1] = xstrdup (buffer + 2);
    }

  free (buffer);
  fclose (file);
  free (name);
}

/*---------------------------------------------.
| Return the translation of a MESSAGE string.  |
`---------------------------------------------*/

const char *
__gettext (message)
     const char *message;
{
  const struct _msg_ent *entry;

  /* Read all strings if this has not been done yet.  */

  if (!locale_string)
    {
      get_all_strings ();

      /* If memory allocation problems, return the message untranslated.  */

      if (!locale_string)
	return message;
    }

  /* Do a linear search to original strings, and use the found index to pick
     the corresponding translation.  */

  for (entry = _msg_tbl; entry < _msg_tbl + _msg_tbl_length; entry++)
    if (strcmp (message, entry->_msg) == 0)
      return locale_string[entry->_msg_number - 1];

  /* If none found, return the message unchanged.  */

  return message;
}
