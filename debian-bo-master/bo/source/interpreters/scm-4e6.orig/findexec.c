/* This file was part of DLD, a dynamic link/unlink editor for C.

   Copyright (C) 1990 by W. Wilson Ho.

   The author can be reached electronically by how@cs.ucdavis.edu or
   through physical mail at:

   W. Wilson Ho
   Division of Computer Science
   University of California at Davis
   Davis, CA 95616

Fri Sep 14 22:16:14 1990  Edgar Roeder  (edgar at megamaster)

	* added a separate DLDPATH environment variable in
	dld_find_executable so that users may specify a special path
	for object modules.

Thu Feb  3 01:46:16 1994  Aubrey Jaffer  (jaffer@jacal)

	* find_exec.c (dld_find_executable): added stat check for
	linux so that it doesn't think directories with the same name
	as the program are executable.

Wed Feb 21 23:06:35 1996  Aubrey Jaffer  <jaffer@jacal.bertronics>

	* find_exec.c: extracted for general use.  Generalized to
	MS-DOS.  */

/* This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 1, or (at your option) any
   later version. */

/* Given a filename, dld_find_executable searches the directories
   listed in the environment variable PATH for a file with that
   filename.  A new copy of the complete path name of that file is
   returned.  This new string may be disposed by free() later on.  */

#include <sys/file.h>
#include <sys/param.h>
#ifdef linux
# include <stdlib.h>
# include <sys/stat.h>
# include <unistd.h>     /* for X_OK define */
#endif
#ifdef __svr4__
# include <string.h>
# include <stdlib.h>
# include <sys/stat.h>
# include <unistd.h>     /* for X_OK define */
#else
# ifdef __sgi__
#  include <string.h>
#  include <stdlib.h>
#  include <sys/stat.h>
#  include <unistd.h>     /* for X_OK define */
# else
#  include <strings.h>
# endif
#endif
#ifndef __STDC__
# define const /**/
#endif

#ifndef DEFAULT_PATH
# define DEFAULT_PATH ".:~/bin::/usr/local/bin:/usr/new:/usr/ucb:/usr/bin:/bin:/usr/hosts"
#endif

static char *copy_of(s)
     register const char *s;
{
  register char *p = (char *) malloc(strlen(s)+1);
  if (!p) return 0;
  *p = 0;
  strcpy(p, s);
  return p;
}

/* ABSOLUTE_FILENAME_P(fname): True if fname is an absolute filename */
#ifdef atarist
# define ABSOLUTE_FILENAME_P(fname)	((fname[0] == '/') || \
	(fname[0] && (fname[1] == ':')))
#else
# define ABSOLUTE_FILENAME_P(fname)	(fname[0] == '/')
#endif /* atarist */

char *dld_find_executable(name)
     const char *name;
{
  char *search;
  register char *p;
  char tbuf[MAXPATHLEN];

  if (ABSOLUTE_FILENAME_P(name))
    return copy_of(name);

  if ((name[0] == '.') && (name[1] == '/')) {
    getcwd(tbuf, MAXPATHLEN);
    strcat(tbuf, name + 1);
    return copy_of(tbuf);
  }

  if (((search = (char *) getenv("DLDPATH")) == 0) &&
      ((search = (char *) getenv("PATH")) == 0))
    search = DEFAULT_PATH;

  p = search;

  while (*p) {
    register char *next = tbuf;

    if (p[0]=='~' && p[1]=='/' && getenv("HOME")) {
      strcpy(tbuf, (char *)getenv("HOME"));
      next = tbuf + strlen(tbuf);
      p++;
    }

    /* Copy directory name into [tbuf] */
    while (*p && *p != ':') *next++ = *p++;
    *next = 0;
    if (*p) p++;

    if (tbuf[0] == '.' && tbuf[1] == 0)
      getcwd(tbuf, MAXPATHLEN);	/* was getwd(tbuf); */
    else if (tbuf[0]=='~' && tbuf[1]==0 && getenv("HOME"))
      strcpy(tbuf, (char *)getenv("HOME"));

    strcat(tbuf, "/");
    strcat(tbuf, name);

    if (access(tbuf, X_OK) == 0) {
#ifndef hpux
# ifndef ultrix
      struct stat stat_temp;
      if (stat(tbuf,&stat_temp)) continue;
      if (S_IFREG != (S_IFMT & stat_temp.st_mode)) continue;
# endif/* ultrix */
#endif /* hpux */
      return copy_of(tbuf);
    }
  }

  return 0;
}
