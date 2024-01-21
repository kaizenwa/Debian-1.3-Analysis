/*
 * Copyright 1989, 1990, 1991, 1992, John F. Haugh II
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
#include "shadow.h"

#if 0
static char sccsid[] = "@(#)shadow.c	3.10	20:38:10	3/7/92";
#endif

static FILE *shadow;
static char *spwbuf = NULL;
static struct spwd spwd;

#define FIELDS 9
#define OFIELDS 5

#define MAXBUF BUFSIZ

static void
DEFUN_VOID(initmem)
{
  if (spwbuf)
    return;

  spwbuf = (char *) malloc (MAXBUF + 1);
}

void
DEFUN_VOID(setspent)
{
#undef TRUE
#undef FALSE
#define TRUE	1
#define FALSE	0
static char no_etc_shadow = FALSE;

  if (shadow)
    rewind (shadow);
  else
    if (!no_etc_shadow)
       no_etc_shadow = (shadow = fopen (SHADOW, "r")) ? FALSE : TRUE;
}

void
DEFUN_VOID(endspent)
{
  if (shadow)
    {
      fclose (shadow);
      shadow = NULL;
    }
}

struct spwd *
DEFUN(sgetspent, (string), CONST char *string)
{
  char *fields[FIELDS];
  char *cp;
  char *cpp;
  int i;

  initmem();

  strncpy (spwbuf, string, MAXBUF);
  spwbuf[MAXBUF] = '\0';

  if ((cp = strrchr (spwbuf, '\n')))
    *cp = '\0';

  for (cp = spwbuf, i = 0; *cp && i < FIELDS; i++)
    {
      fields[i] = cp;
      while (*cp && *cp != ':')
	cp++;

      if (*cp)
	*cp++ = '\0';
    }
  if (i == FIELDS - 1)
    fields[i++] = cp;

  if (*cp || (i != FIELDS && i != OFIELDS))
    return 0;

  spwd.sp_namp = fields[0];
  spwd.sp_pwdp = fields[1];

  if ((spwd.sp_lstchg = strtol (fields[2], &cpp, 10)) == 0 && *cpp)
    return 0;
  else if (fields[2][0] == '\0')
    spwd.sp_lstchg = -1;

  if ((spwd.sp_min = strtol (fields[3], &cpp, 10)) == 0 && *cpp)
    return 0;
  else if (fields[3][0] == '\0')
    spwd.sp_min = -1;

  if ((spwd.sp_max = strtol (fields[4], &cpp, 10)) == 0 && *cpp)
    return 0;
  else if (fields[4][0] == '\0')
    spwd.sp_max = -1;

  if (i == OFIELDS)
    {
      spwd.sp_warn = spwd.sp_inact = spwd.sp_expire =
	spwd.sp_flag = -1;

      return &spwd;
    }
  if ((spwd.sp_warn = strtol (fields[5], &cpp, 10)) == 0 && *cpp)
    return NULL;
  else if (fields[5][0] == '\0')
    spwd.sp_warn = -1;

  if ((spwd.sp_inact = strtol (fields[6], &cpp, 10)) == 0 && *cpp)
    return NULL;
  else if (fields[6][0] == '\0')
    spwd.sp_inact = -1;

  if ((spwd.sp_expire = strtol (fields[7], &cpp, 10)) == 0 && *cpp)
    return NULL;
  else if (fields[7][0] == '\0')
    spwd.sp_expire = -1;

  if ((spwd.sp_flag = strtol (fields[8], &cpp, 10)) == 0 && *cpp)
    return NULL;
  else if (fields[8][0] == '\0')
    spwd.sp_flag = -1;

  return &spwd;
}

struct spwd *
DEFUN(fgetspent, (fp), FILE *fp)
{
  char buf[BUFSIZ];

  if (!fp)
    return NULL;

  if (fgets (buf, BUFSIZ, fp) == NULL)
    return NULL;

  return sgetspent (buf);
}

struct spwd *
DEFUN_VOID(getspent)
{
  if (!shadow)
    setspent ();

  return fgetspent (shadow);
}

struct spwd *
DEFUN(getspnam, (name), CONST char *name)
{
  struct spwd *sp;

  setspent ();
  while ((sp = getspent ()) != NULL)
    {
      if (strcmp (name, sp->sp_namp) == 0)
	return sp;
    }
  return NULL;
}

int
DEFUN(putspent, (sp, fp), CONST struct spwd *sp AND FILE *fp)
{
  int errors = 0;

  if (!fp || !sp)
    return -1;

  if (fprintf (fp, "%s:%s:", sp->sp_namp, sp->sp_pwdp) < 0)
    errors++;

  if (sp->sp_lstchg != -1)
    {
      if (fprintf (fp, "%ld:", sp->sp_lstchg) < 0)
	errors++;
    }
  else if (putc (':', fp) == EOF)
    errors++;

  if (sp->sp_min != -1)
    {
      if (fprintf (fp, "%ld:", sp->sp_min) < 0)
	errors++;
    }
  else if (putc (':', fp) == EOF)
    errors++;

  if (sp->sp_max != -1)
    {
      if (fprintf (fp, "%ld", sp->sp_max) < 0)
	errors++;
    }

  /*
   * See if the structure has any of the SVR4 fields in
   * it.  If none of those fields have any data there is
   * no reason to write them out since they will be filled
   * in the same way when they are read back in.  Otherwise
   * there is at least one SVR4 field that must be output.
   */

  if (sp->sp_warn == -1 && sp->sp_inact == -1
      && sp->sp_expire == -1 && sp->sp_flag == -1)
    {
      if (putc ('\n', fp) == EOF || errors)
	return -1;
      else
	return 0;
    }
  else if (putc (':', fp) == EOF)
    errors++;

  if (sp->sp_warn != -1)
    {
      if (fprintf (fp, "%ld:", sp->sp_warn) < 0)
	errors++;
    }
  else if (putc (':', fp) == EOF)
    errors++;

  if (sp->sp_inact != -1)
    {
      if (fprintf (fp, "%ld:", sp->sp_inact) < 0)
	errors++;
    }
  else if (putc (':', fp) == EOF)
    errors++;

  if (sp->sp_expire != -1)
    {
      if (fprintf (fp, "%ld:", sp->sp_expire) < 0)
	errors++;
    }
  else if (putc (':', fp) == EOF)
    errors++;

  if (sp->sp_flag != -1)
    {
      if (fprintf (fp, "%ld", sp->sp_flag) < 0)
	errors++;
    }
  if (putc ('\n', fp) == EOF)
    errors++;

  if (errors)
    return -1;
  else
    return 0;
}
