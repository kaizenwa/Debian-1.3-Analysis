/*
Copyright (c) 1991, 1992, 1993 Xerox Corporation.  All Rights Reserved.  

Unlimited use, reproduction, and distribution of this software is
permitted.  Any copy of this software must include both the above
copyright notice of Xerox Corporation and this paragraph.  Any
distribution of this software must comply with all applicable United
States export control laws.  This software is made available AS IS,
and XEROX CORPORATION DISCLAIMS ALL WARRANTIES, EXPRESS OR IMPLIED,
INCLUDING WITHOUT LIMITATION THE IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS FOR A PARTICULAR PURPOSE, AND NOTWITHSTANDING ANY OTHER
PROVISION CONTAINED HEREIN, ANY LIABILITY FOR DAMAGES RESULTING FROM
THE SOFTWARE OR ITS USE IS EXPRESSLY DISCLAIMED, WHETHER ARISING IN
CONTRACT, TORT (INCLUDING NEGLIGENCE) OR STRICT LIABILITY, EVEN IF
XEROX CORPORATION IS ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.

$Id: util.c,v 1.12 1996/05/29 18:57:39 janssen Exp $
*/

#include <string.h>
#include <stdio.h>	/* for FILE */
#include <stdlib.h>	/* for getenv() */
#include <version.h>	/* for ILU_VERSION_STRING */

#include "iluptype.h"

#ifdef _IS_POSIX
#include <sys/types.h>
#include <unistd.h>
#endif /* _IS_POSIX */
#include <time.h>	/* for ctime() */

#ifndef MACOS
#include <sys/stat.h>	/* for stat() */

/* MED: Windows doesn't have pwd.h (or getpwuid()) */
#ifdef _IS_POSIX
#include <pwd.h>	/* for getpwuid() */
#endif

static char *ModTime (char *path)
{
  static char timebuf[30];
  struct stat statbuf;

  stat (path, &statbuf);
  strcpy (timebuf, (char *) ctime(&statbuf.st_mtime));
  timebuf[24] = '\0';
  return (timebuf);
}

static char *GoodGetLogin (void)
{
#ifdef _IS_POSIX
  char *name;
  struct passwd *pw;

  if ((name = (char *) getlogin()) == NULL
      && (name = (char *) (((pw = getpwuid(getuid())) == NULL) ? NULL : pw->pw_name)) == NULL
      && (name = (char *) getenv("USER")) == NULL)
    return "nobody";
  else
    return name;
#else
  return "nobody";
#endif
}

#else /* MACOS */
 
static char *ModTime (char *path)
{
return "Jan 15, 1692";
}
 
static char *GoodGetLogin (void)
{
return "Macintosh C++ user";
}
 
#endif /* MACOS */

typedef struct {
  FILE *file;
  char *prefix;
} PrintInfo;

static void PrintImportedFileInfo (Imported s, PrintInfo *info)
{
  Interface i = GetInterface (s->name, s->filename);

  if (i == NULL)
    {
      fprintf (stderr, "Couldn't find interface \"%s\".\n", s->name);
      exit(1);
    }
  else
    fprintf (info->file, ",\n%s and \"%s\" of %s", info->prefix, i->filename, ModTime(i->filename));
}

void iluparser_GenerateBoilerplate (FILE *file, Interface parse, char *programName, char *prefixes[2])
{
  PrintInfo info;
  time_t clock = time(0);
  char *now;

  info.file = file;
  info.prefix = prefixes[1];

  now = ilu_strdup(ctime(&clock));
  now[strlen(now) - 1] = '\0';
  
  fprintf (file, "%s This file was automatically generated with ILU (version %s) tools\n",
	   prefixes[0], ILU_VERSION_STRING);
  fprintf (file, "%s at %s by `%s'\n%s running \"%s\" of %s\n",
	   prefixes[1], now, GoodGetLogin(), prefixes[1], programName, ModTime(programName));
  fprintf (file, "%s on \"%s\" of %s", prefixes[1], parse->filename, ModTime(parse->filename));
  if (list_size(parse->imports) > 0)
    list_enumerate (parse->imports, (iluparser_EnumProc) PrintImportedFileInfo, &info);
  fprintf (file, ".\n%s\n%s ILU is Copyright 1991-1996 Xerox Corporation, All Rights Reserved.\n",
	   prefixes[1], prefixes[1]);
  fprintf (file, "%s ILU information:  ftp://ftp.parc.xerox.com/pub/ilu/ilu.html.\n",
	   prefixes[1]);
}

static void PrintInterfaceInfo (Interface parse, PrintInfo *info)
{
  fprintf (info->file, "%s on \"%s\" of %s", info->prefix, parse->filename, ModTime(parse->filename));
  if (list_size(parse->imports) > 0)
    list_enumerate (parse->imports, (iluparser_EnumProc) PrintImportedFileInfo, info);
  fprintf (info->file, "\n");
}

void iluparser_MultipleInterfaceBoilerplate (FILE *file, list interfaces, char *programName, char *prefixes[2])
{
  PrintInfo info;
  time_t clock = time(0);
  char *now;

  info.file = file;
  info.prefix = prefixes[1];

  now = ilu_strdup(ctime(&clock));
  now[strlen(now) - 1] = '\0';
  
  fprintf (file, "%s This file was automatically generated with ILU (version %s) tools\n",
	   prefixes[0], ILU_VERSION_STRING);
  fprintf (file, "%s at %s by `%s'\n%s running \"%s\" of %s\n",
	   prefixes[1], now, GoodGetLogin(), prefixes[1], programName, ModTime(programName));
  list_enumerate (interfaces, (iluparser_EnumProc) PrintInterfaceInfo, &info);
  fprintf (file, "%s\n%s ILU is Copyright 1991-1995 Xerox Corporation, All Rights Reserved.\n",
	   prefixes[1], prefixes[1]);
  fprintf (file, "%s ILU information:  ftp://ftp.parc.xerox.com/pub/ilu/ilu.html.\n",
	   prefixes[1]);
}

string iluparser_GetILUVersionString ()
{
  return (ILU_VERSION_STRING);
}
