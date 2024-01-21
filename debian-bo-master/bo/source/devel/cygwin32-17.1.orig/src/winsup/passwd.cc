/*  getpwnam () and friends 

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#include <pwd.h>
#include <stdio.h>
#include "winsup.h"

/* Remove a : teminated string from the buffer, and inc the pointer */

static char *
grab_string (char **p)
{
  char *src = *p;
  char *res = src;

  while (*src && *src != ':' && *src != '\n')
    src++;

  if (*src == ':')
    {
      *src = 0;
      src++;
    }
  *p = src;
  return res;
}

/* same, for ints */

static int
grab_int (char **p)
{
  char *src = *p;
  int val = atoi (src);
  while (*src && *src != ':' && *src != '\n')
    src++;
  if (*src == ':')
    src++;
  *p = src;
  return val;
}

static struct passwd *
parse (const char *buf)
{
  /* Allocate enough room for the passwd struct and all the strings
     in it in one go */
  static  struct passwd res;
  static char stat[100];
  size_t len = strlen (buf);
  char *mybuf;
  if (len > sizeof (stat))
    {
      set_errno (ENOMEM);
      return 0;
    }
  mybuf = stat;
  strcpy (mybuf, buf);
  res.pw_name = grab_string (&mybuf);
  res.pw_passwd = grab_string (&mybuf);
  res.pw_uid = grab_int (&mybuf);
  res.pw_gid = grab_int (&mybuf);
  res.pw_gecos = grab_string (&mybuf);
  res.pw_dir =  grab_string (&mybuf);
  res.pw_shell = grab_string (&mybuf);
  paranoid_printf ("passwd says %s:%d:%d\n",
		 res.pw_name,
		 res.pw_uid,
		 res.pw_gid);
  return &res;
}

static struct passwd *
search_for (uid_t uid, const char *name)
{
  struct passwd *res = 0;
  char buf[_PASSWORD_LEN];
  in ("search_for");
  FILE * f = fopen ("/etc/passwd", "rb");
  if (f) 
    {
      char *got;

      while ((got = fgets (buf, sizeof (buf), f)))
	{
	  paranoid_printf ("line from passwd file %s\n", got);
	  res = parse (got);
	  if (res) 
	    {
	      if (name) 
		{
		  if (strcmp (name, res->pw_name) == 0)
		    break;
		}
	      else
		{
		  if (uid == res->pw_uid)
		    break;
		}
	    }
	}
      fclose (f);
    }
  out ("search_for");
  return res;
}

struct passwd *
getpwuid (uid_t uid)
{
  return search_for (uid, 0);
}

struct passwd *
getpwnam (const char *name)
{
  return search_for (0,name);
}

static FILE *f; 

struct passwd *
getpwent (void)
{
  char buf[_PASSWORD_LEN];
  if (!f) 
    f = fopen (_PATH_PASSWD, "rb");
  if (f && fgets (buf, sizeof (buf), f))
    return parse (buf);
  return 0;
}

extern "C" 
struct passwd * 
getpwduid (uid_t uid)
{
  return NULL;
}

extern "C"
void
setpwend (void)
{
  if (f)
    fseek (f, 0, 0);
}

void
endpwent (void)
{
  if (f)
    fclose (f);
}

extern "C" int setpassent () { return 0; }
