/* buf.c: This file contains the scratch-file buffer rountines for the
   ed line editor. */
/* ed line editor.
   Copyright (C) 1993, 1994 Andrew Moore, Talke Studio
   All Rights Reserved

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#ifndef lint
static char *rcsid = "@(#)$Id: buf.c,v 1.11 1994/11/13 04:25:44 alm Exp $";
#endif /* not lint */

#include "ed.h"

#include <sys/file.h>
#include <sys/stat.h>


FILE *sfp;			/* scratch file pointer */
off_t sfseek;			/* scratch file position */
int seek_write;			/* seek before writing */
line_t buffer_head;		/* incore buffer */

/* get_sbuf_line: get a line of text from the scratch file; return pointer
   to the text */
char *
get_sbuf_line (lp)
     line_t *lp;
{
  static char *sfbuf = NULL;	/* buffer */
  static int sfbufsz = 0;	/* buffer size */

  int len, ct;

  if (lp == &buffer_head)
    return NULL;
  seek_write = 1;		/* force seek on write */
  /* out of position */
  if (sfseek != lp->seek)
    {
      sfseek = lp->seek;
      if (fseek (sfp, sfseek, SEEK_SET) < 0)
	{
	  fprintf (stderr, "%s\n", strerror (errno));
	  sprintf (errmsg, "Cannot seek temp file");
	  return NULL;
	}
    }
  len = lp->len;
  REALLOC (sfbuf, sfbufsz, len + 1, NULL);
  if ((ct = fread (sfbuf, sizeof (char), len, sfp)) < 0 || ct != len)
    {
      fprintf (stderr, "%s\n", strerror (errno));
      sprintf (errmsg, "Cannot read temp file");
      return NULL;
    }
  sfseek += len;		/* update file position */
  sfbuf[len] = '\0';
  return sfbuf;
}


/* put_sbuf_line: write a line of text to the scratch file and add a line node
   to the editor buffer;  return a pointer to the end of the text */
char *
put_sbuf_line (cs)
     char *cs;
{
  line_t *lp;
  int len, ct;
  char *s;

  if ((lp = (line_t *) malloc (sizeof (line_t))) == NULL)
    {
      fprintf (stderr, "%s\n", strerror (errno));
      sprintf (errmsg, "Out of memory");
      return NULL;
    }
  /* assert: cs is '\n' terminated */
  for (s = cs; *s != '\n'; s++)
    ;
  if (s - cs >= LINECHARS)
    {
      sprintf (errmsg, "Line too long");
      return NULL;
    }
  len = s - cs;
  /* out of position */
  if (seek_write)
    {
      if (fseek (sfp, 0L, SEEK_END) < 0)
	{
	  fprintf (stderr, "%s\n", strerror (errno));
	  sprintf (errmsg, "Cannot seek temp file");
	  return NULL;
	}
      sfseek = ftell (sfp);
      seek_write = 0;
    }
  /* assert: SPL1() */
  if ((ct = fwrite (cs, sizeof (char), len, sfp)) < 0 || ct != len)
    {
      sfseek = -1;
      fprintf (stderr, "%s\n", strerror (errno));
      sprintf (errmsg, "Cannot write temp file");
      return NULL;
    }
  lp->len = len;
  lp->seek = sfseek;
  add_line_node (lp);
  sfseek += len;		/* update file position */
  return ++s;
}


/* add_line_node: add a line node in the editor buffer after the current line */
void
add_line_node (lp)
     line_t *lp;
{
  line_t *cp;

  cp = get_addressed_line_node (current_addr);	/* this get_addressed_line_node last! */
  INSQUE (lp, cp);
  addr_last++;
  current_addr++;
}


/* get_line_node_addr: return line number of pointer */
long
get_line_node_addr (lp)
     line_t *lp;
{
  line_t *cp = &buffer_head;
  long n = 0;

  while (cp != lp && (cp = cp->q_forw) != &buffer_head)
    n++;
  if (n && cp == &buffer_head)
    {
      sprintf (errmsg, "Invalid address");
      return ERR;
    }
  return n;
}


/* get_addressed_line_node: return pointer to a line node in the editor buffer */
line_t *
get_addressed_line_node (n)
     long n;
{
  static line_t *lp = &buffer_head;
  static long on = 0;

  SPL1 ();
  if (n > on)
    if (n <= (on + addr_last) >> 1)
      for (; on < n; on++)
	lp = lp->q_forw;
    else
      {
	lp = buffer_head.q_back;
	for (on = addr_last; on > n; on--)
	  lp = lp->q_back;
      }
  else if (n >= on >> 1)
    for (; on > n; on--)
      lp = lp->q_back;
  else
    {
      lp = &buffer_head;
      for (on = 0; on < n; on++)
	lp = lp->q_forw;
    }
  SPL0 ();
  return lp;
}


extern int newline_added;

char sfn[15] = "";		/* scratch file name */

/* open_sbuf: open scratch file */
int
open_sbuf ()
{
  char *mktemp ();
  int u;

  isbinary = newline_added = 0;
  u = umask(077);
  strcpy (sfn, "/tmp/ed.XXXXXX");
  if (mktemp (sfn) == NULL || (sfp = fopen (sfn, "w+")) == NULL)
    {
      fprintf (stderr, "%s: %s\n", sfn, strerror (errno));
      sprintf (errmsg, "Cannot open temp file");
      umask(u);
      return ERR;
    }
  umask(u);
  return 0;
}


/* close_sbuf: close scratch file */
int
close_sbuf ()
{
  if (sfp)
    {
      if (fclose (sfp) < 0)
	{
	  fprintf (stderr, "%s: %s\n", sfn, strerror (errno));
	  sprintf (errmsg, "Cannot close temp file");
	  return ERR;
	}
      sfp = NULL;
      unlink (sfn);
    }
  sfseek = seek_write = 0;
  return 0;
}


/* quit: remove_lines scratch file and exit */
void
quit (n)
     int n;
{
  if (sfp)
    {
      fclose (sfp);
      unlink (sfn);
    }
  exit (n);
}


extern line_t yank_buffer_head;
extern char *old_filename;
unsigned char ctab[256];	/* character translation table */

/* init_buffers: open scratch buffer; initialize line queue */
void
init_buffers ()
{
  int i = 0;

  /* Read stdin one character at a time to avoid i/o contention
     with shell escapes invoked by nonterminal input, e.g.,
     ed - <<EOF
     !cat
     hello, world
     EOF */
#ifdef HAVE_SETBUFFER
  setbuffer (stdin, stdinbuf, 1);
#else
  setvbuf (stdin, stdinbuf, _IONBF, 0);
#endif
  if ((errmsg = (char *) malloc (ERRSZ)) == NULL ||
      (old_filename = (char *) malloc (PATH_MAX + 1)) == NULL)
    {
      fprintf (stderr, "%s\n", strerror (errno));
      quit (2);
    }
  old_filename[0] = '\0';
  if (open_sbuf () < 0)
    quit (2);
  REQUE (&buffer_head, &buffer_head);
  REQUE (&yank_buffer_head, &yank_buffer_head);
  for (i = 0; i < 256; i++)
    ctab[i] = i;

}


/* translit_text: translate characters in a string */
char *
translit_text (s, len, from, to)
     char *s;
     int len;
     int from;
     int to;
{
  static int i = 0;

  unsigned char *us;

  ctab[i] = i;			/* restore table to initial state */
  ctab[i = from] = to;
  for (us = (unsigned char *) s; len-- > 0; us++)
    *us = ctab[*us];
  return s;
}
