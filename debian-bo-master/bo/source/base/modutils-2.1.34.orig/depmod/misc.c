/* Copyright 1996 Free Software Foundation, Inc.
   Contributed by Marcin Dalecki <dalecki@sub994.sub.uni-goettingen.de>

   This file is part of the Linux modutils.

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2 of the License, or (at your
   option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <syslog.h>
#include <ctype.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/utsname.h>

#include "util.h"
#include "misc.h"

/*=================================================================*/

int flag_debug = 0;

/*=================================================================*/

/*
 * Strip the directory and .o or .mod extension of a file name
 * Return a pointer to a malloc()ed buffer
 */
char *
strip_o (char *fname)
{
  char *pt = fname;

  while ((pt = strchr (fname, '/')))
    fname = pt + 1;
  if ((pt = strrchr (fname, '.'))
      && ((strcmp (pt, ".o") == 0) || (strcmp (pt, ".mod") == 0)))
    {
      char *leak = (char *) xmalloc (pt - fname + 1);
      strncpy (leak, fname, pt - fname);
      leak[pt - fname] = '\0';
      
      return leak;
    }
  return xstrdup (fname);
}

/*=================================================================*/

/*
 * Read and preprocess the contents of a configuration file.
 * Return the preprocessed contents on success and NULL otherwise.
 *
 * This function implements efficient in place reading of files,
 * the usual semantics of '#' comments and the arbitrary multiplicity of
 * white spaces as delimiters.
 */
char *
read_and_preprocess_file (const char *file)
{
  char *buf;

  char *tmp;
  char *pmt;
  char *cp;
  
  FILE *fp;
  struct stat statb;

  if (stat (file, &statb) == -1
      || (statb.st_mode & S_IFMT) != S_IFREG
      || !(fp = fopen (file, "r")))
    {
      return NULL;
    }

  /*
   * OK: Now first read all of the file into a dynamically allocated buffer.
   */
  buf = (char *) xmalloc ((unsigned) (statb.st_size + 2));
  if (!fread (buf, sizeof (char), statb.st_size + 1, fp))
      lprintf ("warning: file %s is empty!\n", file);
  buf[statb.st_size] = '\0';	/* be sure to NULL-terminate */
  fclose (fp);

  if (!strlen(buf))
  	return buf;
  /* 
   * Go through the whole buffer and remove comments by blanking them out.
   */
  for (cp = buf; (cp = strchr (cp, '#')); cp = tmp)
    {
      tmp = strchr (cp, '\n');
      if (!tmp)
	tmp = cp + strlen (cp);
      memset (cp, ' ', tmp - cp);
    }

  /*
   * Replace whitespaces other then '\n' by ' '
   */
  cp = buf;
  do
    {
      cp += strcspn (cp, "\t\f\v\r");
      if (*cp)
	*cp = ' ';
    }
  while (*cp);

  /*
   * Collapse all multiple white spaces into one
   */
  tmp = pmt = buf;
  do
    {
      tmp += strspn (tmp, " ");
      cp = strchr (tmp, ' ');
      if (!cp)
	cp = tmp + strlen (tmp);
      memmove (pmt, tmp, cp - tmp + 1);
      pmt += cp - tmp + 1;

      tmp = cp + 1;
    }
  while (*cp);
  
  /*
   * Remove any white space at line ends.
   */
  tmp = pmt = buf;  
  do
    {
       cp = strstr(tmp, " \n");
       if (!cp)
         cp = tmp + strlen (tmp);
       else
         *cp = '\n';
       memmove (pmt, tmp, cp - tmp + 1);
       pmt += cp - tmp + 1;
       
       tmp = cp + 2;
    }
  while (*cp);

  tmp = pmt = buf;  
  do
    {
      cp = strstr(tmp, "\n ");
      if (!cp)
        cp = tmp + strlen (tmp);
      memmove (pmt, tmp, cp - tmp + 1);
      pmt += cp - tmp + 1;
      
      tmp = cp + 2;
    }
  while(*cp);
  
  /*
   * Make sure that the last line is terminated by newline.
   */
  if ((cp > buf) && (*(cp - 1) != '\n'))
    {
      cp[0] = '\n';
      cp[1] = '\0';		/* no problem, we allocated this bit */
    }

  return buf;
}

#define RELEASE		0
#define UNKNOWN		-1

/* We check what kind command or pattern it tries to run. */
static int
what_command (const char *cmd)
{
  /* We first build the canonical form. */
  cmd = basename (cmd);
  if (strncmp (cmd, "uname", 5) == 0)
  {
    char *p;

    /* We check the argument. */
    p = strstr (cmd, "-r");
    if (p == NULL) return UNKNOWN;
    for (cmd += 5; cmd < p; cmd++)
    {
      if (!isspace (*cmd)) return UNKNOWN;
    }
    for (cmd += 2; *cmd; cmd++)
    {
      if (!isspace (*cmd)) return UNKNOWN;
    }

    return RELEASE;
  }
  return UNKNOWN;
}

/* We expand the command or pattern. */
static char *
expand_command (const char *prefix, const char *command)
{
  static struct utsname utsbuf; 
  char *tmp;

  switch (what_command (command))
  {
  case RELEASE:
    /* We need the kernel release number. */
    if (uname (&utsbuf) && errno != 0)
    {
      /* Bad release number. */
      tmp = "uname return error";
    }
    else
    {
      tmp = utsbuf.release;
    }
    break;

  default:
    /* We can add more later. For now, we just ignore it. */
    tmp = NULL;
    break;
  }

  return tmp;
}

/* We resolve the string for patterns and commands. */
char *
resolve_string (const char *str, char *buf, int size)
{
  const char *next, *left, *right;
  char *command, *tmp, *cp;
  int len;

  /* We parse the string for `foo`. */
  cp = buf;
  right = str;
  for (next = str; left = strchr (next, '`'); next = right + 1)
  {
    /* We found the first `. Copy the plain part. */
    len = left - next;
    size -= len;
    if (size <= 0)
    {
      goto error;
    }
    strncpy (cp, next, len);
    cp += len;

    right = strchr (left + 1, '`');
    if (right == NULL)
    {
      /* No second `. */
      break;
    }

    /* Now we get the second `. */
    len = right - left;
    command = alloca (len);
    strncpy (command, left + 1, len - 1);
    command [len - 1] = '\0';
    *cp = '\0';

    tmp = expand_command (buf, command);
    if (tmp)
    {
      len = strlen (tmp);
      size -= len;
      if (size <= 0)
      {
	goto error;
      }
      strncpy (cp, tmp, len);
      cp += len;
    }
    else
    {
      /* We don't know how to deal with it. Just copy it for now. */
      size -= len + 1;
      if (size <= 0)
      {
	goto error;
      }
      strncpy (cp, left, len + 1);
      cp += len + 1;
    }
  }

  if (right)
  {
    /* We are done. Copy the rest. */
   strncpy (cp, next, size - 1);
  }
  else
  {
    /* No second `. Copy the rest. */
    strncpy (cp, left, size - 1);
  }
  buf [size - 1] = '\0';
  
  return buf;

error:
  /* We don't do anything. */
  return (char *) str;
}

/*
 * This function concatenates lines terminated by '\\' and increases "lines"
 * accordingly. If "src" there is no source or the source is empty, we
 * return NULL. Otherwise we are returning the end of parsing.
 * The result is placed in "src"! lines is adjusted to the number of lines
 * read.
 */
char *
get_concat_line (char *src, int *lines)
{
  char *tmp;

  if (!src)			/* nothing to be processed */
    return NULL;

  tmp = src;

  if (!*tmp)
    return NULL;

  while (*src && (*src != '\n'))
    {
      if (src[0] == '\\' && src[1] == '\n')
	{
	  ++*lines;
	  ++src;
	  *src = '*';		/* fake it for the next loop */
	  --tmp;
	}
      else
	*tmp = *src;

      ++src;
      ++tmp;
    }
  *tmp = '\0';
  ++*lines;

  return src + 1;
}

/*=================================================================*/

/*
 * Error logging facilities.
 */

int log;			/* this is passed to insmod */
static int silent;

static int errors;
static const char *error_file;

void
error (const char *fmt,...)
{
  va_list args;

  if (silent)
    ;
  else if (log)
    {
      char buf[1024];
      int n;

      n = snprintf (buf, sizeof (buf), "%s: ",
		    error_file ? error_file : "insmod");
      va_start (args, fmt);
      vsnprintf (buf + n, sizeof (buf) - n, fmt, args);
      va_end (args);

      syslog (LOG_ERR, "%s", buf);
    }
  else
    {
      fprintf (stderr, "%s: ", error_file ? error_file : "modprobe");
      va_start (args, fmt);
      vfprintf (stderr, fmt, args);
      va_end (args);
      putc ('\n', stderr);
    }

  errors++;
}

void
lprintf (const char *fmt,...)
{
  va_list args;

  if (silent)
    ;
  else if (log)
    {
      char buf[1024];
      va_start (args, fmt);
      vsnprintf (buf, sizeof (buf), fmt, args);
      va_end (args);
      syslog (LOG_INFO, "%s", buf);
    }
  else
    {
      va_start (args, fmt);
      vfprintf (stdout, fmt, args);
      va_end (args);
      putchar ('\n');
    }
}

void
setsyslog (const char *program)
{
  openlog (program, LOG_CONS, LOG_DAEMON);
  log = 1;
}
