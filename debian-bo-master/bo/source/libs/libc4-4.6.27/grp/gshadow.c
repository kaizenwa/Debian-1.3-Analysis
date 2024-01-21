/*
 * Copyright 1990, 1991, John F. Haugh II
 * All rights reserved.
 *
 * Permission is granted to copy and create derivative works for any
 * non-commercial purpose, provided this copyright notice is preserved
 * in all copies of source code, or included in human readable form
 * and conspicuously displayed on all copies of object code or
 * distribution media.
 */

#include <ansidecl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <shadow.h>

#if 0
static char sccsid[] = "@(#)gshadow.c	3.7	08:45:58	9/12/91";
#endif

#define MAXMEM 1024
#define MAXBUF (BUFSIZ + MAXMEM * 9 + 1)

static FILE *shadow;
static char *sgrbuf = NULL;
static char **members;
static char **admins;
static struct sgrp sgroup;

#define FIELDS 4

static void
DEFUN_VOID(initmem)
{
  if (sgrbuf)
    return;

  sgrbuf = (char *) malloc (MAXBUF);
  members = (char **) malloc (sizeof(char *) * (MAXMEM + 1));
  admins = (char **) malloc (sizeof(char *) * (MAXMEM + 1));
}

static char *
DEFUN(fgetsx, (buf, cnt, f), char *buf AND int cnt AND FILE *f)
{
  char *cp = buf;
  char *ep;

  while (cnt > 0)
    {
      if (!fgets (cp, cnt, f))
	if (cp == buf)
	  return NULL;
	else
	  break;

      if ((ep = strrchr (cp, '\\')) && *(ep + 1) == '\n')
	{
	  if ((cnt -= ep - cp) > 0)
	    *(cp = ep) = '\0';
	}
      else
	break;
    }
  return buf;
}

static int
DEFUN(fputsx, (s, stream), char *s AND FILE *stream)
{
  int i;

  for (i = 0; *s; i++, s++)
    {
      if (putc (*s, stream) == EOF)
	return EOF;

      if (i > BUFSIZ / 2) {
	if (putc ('\\', stream) == EOF
	    || putc ('\n', stream) == EOF)
	  return EOF;

	i = 0;
      }
    }
  return 0;
}

static char **
DEFUN(list, (s, l), char *s AND char **l)
{
  int nmembers = 0;

  while (s && *s)
    {
      l[nmembers++] = s;
      if ((s = strchr (s, ',')))
	*s++ = '\0';
    }
  l[nmembers] = (char *) 0;
  return l;
}

void
DEFUN_VOID(setsgent)
{
  if (shadow)
    rewind (shadow);
  else
    shadow = fopen (GSHADOW, "r");
}

void
DEFUN_VOID(endsgent)
{
  if (shadow)
    {
      fclose (shadow);
      shadow = NULL;
    }
}

struct sgrp *
DEFUN(sgetsgent, (string), CONST char *string)
{
  char *fields[FIELDS];
  char *cp;
  int i;

  initmem();

  strncpy (sgrbuf, string, MAXBUF);
  sgrbuf[MAXBUF] = '\0';

  if ((cp = strrchr (sgrbuf, '\n')))
    *cp = '\0';

  for (cp = sgrbuf, i = 0; i < FIELDS && cp; i++)
    {
      fields[i] = cp;
      if ((cp = strchr (cp, ':')))
	*cp++ = '\0';
    }
  if ((cp && *cp) || i != FIELDS)
    return NULL;

  sgroup.sg_name = fields[0];
  sgroup.sg_passwd = fields[1];
  sgroup.sg_adm = list (fields[2], admins);
  sgroup.sg_mem = list (fields[3], members);

  return &sgroup;
}

struct sgrp *
DEFUN(fgetsgent, (fp), FILE *fp)
{
  char buf[sizeof sgrbuf];

  if (!fp)
    return NULL;

  if (fgetsx (buf, sizeof buf, fp))
    return NULL;

  return sgetsgent (buf);
}

struct sgrp *
DEFUN_VOID(getsgent)
{
  if (!shadow)
    setsgent ();

  return fgetsgent (shadow);
}

struct sgrp *
DEFUN(getsgnam, (name), CONST char *name)
{
  struct sgrp *sgrp;

  setsgent ();

  while ((sgrp = getsgent ()))
    {
      if (strcmp (name, sgrp->sg_name) == 0)
	return (sgrp);
    }
  return NULL;
}

int
DEFUN(putsgent, (sgrp, fp), CONST struct sgrp *sgrp AND FILE *fp)
{
  char buf[sizeof sgrbuf];
  char *cp = buf;
  int i;

  if (!fp || !sgrp)
    return -1;

  /*
   * Copy the group name and passwd.
   */

  strcpy (cp, sgrp->sg_name);
  cp += strlen (cp);
  *cp++ = ':';

  strcpy (cp, sgrp->sg_passwd);
  cp += strlen (cp);
  *cp++ = ':';

  /*
   * Copy the administrators, separating each from the other
   * with a ",".
   */

  for (i = 0; sgrp->sg_adm[i]; i++)
    {
      if (i > 0)
	*cp++ = ',';

      strcpy (cp, sgrp->sg_adm[i]);
      cp += strlen (cp);
    }
  *cp++ = ':';

  /*
   * Now do likewise with the group members.
   */

  for (i = 0; sgrp->sg_mem[i]; i++)
    {
      if (i > 0)
	*cp++ = ',';

      strcpy (cp, sgrp->sg_mem[i]);
      cp += strlen (cp);
    }
  *cp++ = '\n';
  *cp = '\0';

  /*
   * Output using the function which understands the line
   * continuation conventions.
   */

  return fputsx (buf, fp);
}
