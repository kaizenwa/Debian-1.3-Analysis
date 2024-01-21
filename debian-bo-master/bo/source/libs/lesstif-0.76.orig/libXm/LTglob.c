/**
 * $Id: LTglob.c,v 1.3 1996/11/08 04:46:28 miers Exp $
 *
 * derived from GNU libc glob.c
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1991, 1992, 1993 Free Software Foundation, Inc.
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/

static char rcsid[] = "$Id: LTglob.c,v 1.3 1996/11/08 04:46:28 miers Exp $";

#if defined (_AIX) && !defined (__GNUC__)
 #pragma alloca
#endif

#include <LTconfig.h>
#include <X11/Intrinsic.h>
#include <X11/Xfuncs.h>

#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>


#ifdef	STDC_HEADERS
#include <stddef.h>
#endif

#ifdef	HAVE_UNISTD_H
#include <unistd.h>
#ifndef POSIX
#ifdef	_POSIX_VERSION
#define	POSIX
#endif
#endif
#endif

#if !defined(__GNU_LIBRARY__) && !defined(STDC_HEADERS)
extern int errno;
#endif

#ifndef	NULL
#define	NULL	0
#endif


#if	defined (POSIX) || defined (HAVE_DIRENT_H) || defined (__GNU_LIBRARY__)
#include <dirent.h>
#ifndef	__GNU_LIBRARY__
#define D_NAMLEN(d) strlen((d)->d_name)
#else	/* GNU C library.  */
#define D_NAMLEN(d) ((d)->d_namlen)
#endif	/* Not GNU C library.  */
#else	/* Not POSIX or HAVE_DIRENT_H.  */
#define direct dirent
#define D_NAMLEN(d) ((d)->d_namlen)
#ifdef	HAVE_SYS_NDIR_H
#include <sys/ndir.h>
#endif	/* HAVE_SYS_NDIR_H */
#ifdef	HAVE_SYS_DIR_H
#include <sys/dir.h>
#endif	/* HAVE_SYS_DIR_H */
#ifdef HAVE_NDIR_H
#include <ndir.h>
#endif	/* HAVE_NDIR_H */
#endif	/* POSIX or HAVE_DIRENT_H or __GNU_LIBRARY__.  */

#if defined (POSIX) && !defined (__GNU_LIBRARY__)
/* Posix does not require that the d_ino field be present, and some
   systems do not provide it. */
#define REAL_DIR_ENTRY(dp) 1
#else
#define REAL_DIR_ENTRY(dp) (dp->d_ino != 0)
#endif /* POSIX */

#if	(defined (STDC_HEADERS) || defined (__GNU_LIBRARY__))
#include <stdlib.h>
#include <string.h>
#define	ANSI_STRING
#else	/* No standard headers.  */

#ifdef HAVE_STRING_H
#include <string.h>
#define	ANSI_STRING
#else
#include <strings.h>
#endif
#ifdef	HAVE_MEMORY_H
#include <memory.h>
#endif

extern void qsort ();
extern void abort (), exit ();

#endif	/* Standard headers.  */

#ifndef	HAVE_STRCOLL
#define	strcoll	strcmp
#endif


#ifndef	__GNU_LIBRARY__
#ifdef	__GNUC__
__inline
#endif
static void *
my_realloc (void *p, 
	    unsigned int n)
{
  /* These casts are the for sake of the broken Ultrix compiler,
     which warns of illegal pointer combinations otherwise.  */
  if (p == NULL)
    return (char *) XtMalloc (n);
  return (char *) XtRealloc (p, n);
}
#define	realloc	my_realloc
#endif


#if !defined(__alloca)
#ifdef	__GNUC__
#undef	alloca
#define	alloca(n)	__builtin_alloca (n)
#else	/* Not GCC.  */
#if	defined (sparc) || defined (HAVE_ALLOCA_H)
#include <alloca.h>
#else	/* Not sparc or HAVE_ALLOCA_H.  */
#ifndef	_AIX
extern char *alloca (size_t size_to_allocate);
#endif	/* Not _AIX.  */
#endif	/* sparc or HAVE_ALLOCA_H.  */
#endif	/* GCC.  */

#define	__alloca	alloca

#endif

#ifndef __GNU_LIBRARY__
#define __lstat lstat
#ifndef HAVE_LSTAT
#define lstat stat
#endif
#ifdef STAT_MACROS_BROKEN
#undef S_ISDIR
#endif
#ifndef S_ISDIR
#define S_ISDIR(mode) (((mode) & S_IFMT) == S_IFDIR)
#endif
#endif

#ifndef	STDC_HEADERS
#undef	size_t
#define	size_t	unsigned int
#endif

/* Some system header files erroneously define these.
   We want our own definitions from <fnmatch.h> to take precedence.  */
#undef	FNM_PATHNAME
#undef	FNM_NOESCAPE
#undef	FNM_PERIOD
#include <LTfnmatch.h>

/* Some system header files erroneously define these.
   We want our own definitions from <glob.h> to take precedence.  */
#undef	GLOB_ERR
#undef	GLOB_MARK
#undef	GLOB_NOSORT
#undef	GLOB_DOOFFS
#undef	GLOB_NOCHECK
#undef	GLOB_APPEND
#undef	GLOB_NOESCAPE
#undef	GLOB_PERIOD
#include <LTglob.h>

__ptr_t (*__glob_opendir_hook) __P ((const char *directory));
const char *(*__glob_readdir_hook) __P ((__ptr_t stream));
void (*__glob_closedir_hook) __P ((__ptr_t stream));

static int glob_pattern_p __P ((const char *pattern, int quote));
static int glob_in_dir __P ((const char *pattern, const char *directory,
			     int flags,
			     int (*errfunc) __P ((const char *, int)),
			     glob_t *pglob));
static int prefix_array __P ((const char *prefix, char **array, size_t n));
static int collated_compare __P ((const __ptr_t, const __ptr_t));

/* Do glob searching for PATTERN, placing results in PGLOB.
   The bits defined above may be set in FLAGS.
   If a directory cannot be opened or read and ERRFUNC is not nil,
   it is called with the pathname that caused the error, and the
   `errno' value from the failing call; if it returns non-zero
   `glob' returns GLOB_ABEND; if it returns zero, the error is ignored.
   If memory cannot be allocated for PGLOB, GLOB_NOSPACE is returned.
   Otherwise, `glob' returns zero.  */
int
_Lesstif_glob (
    const char *pattern,
    int flags,
    int (*errfunc) __P ((const char *, int)),
    glob_t *pglob)
{
  const char *filename;
  char *dirname;
  size_t dirlen;
  int status;
  int oldcount;

  if (pattern == NULL || pglob == NULL || (flags & ~__GLOB_FLAGS) != 0)
    {
      errno = EINVAL;
      return -1;
    }

  /* Find the filename.  */
  filename = strrchr (pattern, '/');
  if (filename == NULL)
    {
      filename = pattern;
      dirname = (char *) ".";
      dirlen = 0;
    }
  else if (filename == pattern)
    {
      /* "/pattern".  */
      dirname = (char *) "/";
      dirlen = 1;
      ++filename;
    }
  else
    {
      dirlen = filename - pattern;
      dirname = (char *) __alloca (dirlen + 1);
      bcopy(pattern, dirname, dirlen);
      dirname[dirlen] = '\0';
      ++filename;
    }

  if (filename[0] == '\0' && dirlen > 1)
    /* "pattern/".  Expand "pattern", appending slashes.  */
    {
      int val = _Lesstif_glob (dirname, flags | GLOB_MARK, errfunc, pglob);
      if (val == 0)
	pglob->gl_flags = (pglob->gl_flags & ~GLOB_MARK) | (flags & GLOB_MARK);
      return val;
    }

  if (!(flags & GLOB_APPEND))
    {
      pglob->gl_pathc = 0;
      pglob->gl_pathv = NULL;
    }

  oldcount = pglob->gl_pathc;

  if (glob_pattern_p (dirname, !(flags & GLOB_NOESCAPE)))
    {
      /* The directory name contains metacharacters, so we
	 have to glob for the directory, and then glob for
	 the pattern in each directory found.  */
      glob_t dirs;
      register int i;

      status = _Lesstif_glob (dirname,
		     ((flags & (GLOB_ERR | GLOB_NOCHECK | GLOB_NOESCAPE)) |
		      GLOB_NOSORT),
		     errfunc, &dirs);
      if (status != 0)
	return status;

      /* We have successfully globbed the preceding directory name.
	 For each name we found, call glob_in_dir on it and FILENAME,
	 appending the results to PGLOB.  */
      for (i = 0; i < dirs.gl_pathc; ++i)
	{
	  int oldcount;

#ifdef	SHELL
	  {
	    /* Make globbing interruptible in the bash shell. */
	    extern int interrupt_state;

	    if (interrupt_state)
	      {
		_Lesstif_globfree (&dirs);
		_Lesstif_globfree (&files);
		return GLOB_ABEND;
	      }
	  }
#endif /* SHELL.  */

	  oldcount = pglob->gl_pathc;
	  status = glob_in_dir (filename, dirs.gl_pathv[i],
				(flags | GLOB_APPEND) & ~GLOB_NOCHECK,
				errfunc, pglob);
	  if (status == GLOB_NOMATCH)
	    /* No matches in this directory.  Try the next.  */
	    continue;

	  if (status != 0)
	    {
	      _Lesstif_globfree (&dirs);
	      _Lesstif_globfree (pglob);
	      return status;
	    }

	  /* Stick the directory on the front of each name.  */
	  if (prefix_array (dirs.gl_pathv[i],
			    &pglob->gl_pathv[oldcount],
			    pglob->gl_pathc - oldcount))
	    {
	      _Lesstif_globfree (&dirs);
	      _Lesstif_globfree (pglob);
	      return GLOB_NOSPACE;
	    }
	}

      flags |= GLOB_MAGCHAR;

      if (pglob->gl_pathc == oldcount)
	/* No matches.  */
	if (flags & GLOB_NOCHECK)
	  {
	    size_t len = strlen (pattern) + 1;
	    char *patcopy = (char *) XtMalloc (len);
	    if (patcopy == NULL)
	      return GLOB_NOSPACE;
	    bcopy (pattern, patcopy, len);

	    pglob->gl_pathv
	      = (char **) XtRealloc ((char *)pglob->gl_pathv,
				   (pglob->gl_pathc +
				    ((flags & GLOB_DOOFFS) ?
				     pglob->gl_offs : 0) +
				    1 + 1) *
				   sizeof (char *));
	    if (pglob->gl_pathv == NULL)
	      {
		XtFree (patcopy);
		return GLOB_NOSPACE;
	      }

	    if (flags & GLOB_DOOFFS)
	      while (pglob->gl_pathc < pglob->gl_offs)
		pglob->gl_pathv[pglob->gl_pathc++] = NULL;

	    pglob->gl_pathv[pglob->gl_pathc++] = patcopy;
	    pglob->gl_pathv[pglob->gl_pathc] = NULL;
	    pglob->gl_flags = flags;
	  }
	else
	  return GLOB_NOMATCH;
    }
  else
    {
      status = glob_in_dir (filename, dirname, flags, errfunc, pglob);
      if (status != 0)
	return status;

      if (dirlen > 0)
	{
	  /* Stick the directory on the front of each name.  */
	  if (prefix_array (dirname,
			    &pglob->gl_pathv[oldcount],
			    pglob->gl_pathc - oldcount))
	    {
	      _Lesstif_globfree (pglob);
	      return GLOB_NOSPACE;
	    }
	}
    }

  if (flags & GLOB_MARK)
    {
      /* Append slashes to directory names.  glob_in_dir has already
	 allocated the extra character for us.  */
      int i;
      struct stat st;
      for (i = oldcount; i < pglob->gl_pathc; ++i)
	if (__lstat (pglob->gl_pathv[i], &st) == 0 &&
	    S_ISDIR (st.st_mode))
	  strcat (pglob->gl_pathv[i], "/");
    }

  if (!(flags & GLOB_NOSORT))
    /* Sort the vector.  */
    qsort ((__ptr_t) &pglob->gl_pathv[oldcount],
	   pglob->gl_pathc - oldcount,
	   sizeof (char *), collated_compare);

  return 0;
}


/* Free storage allocated in PGLOB by a previous `glob' call.  */
void
_Lesstif_globfree (
     register glob_t *pglob)
{
  if (pglob->gl_pathv != NULL)
    {
      register int i;
      for (i = 0; i < pglob->gl_pathc; ++i)
	if (pglob->gl_pathv[i] != NULL)
	  XtFree ((__ptr_t) pglob->gl_pathv[i]);
      XtFree ((__ptr_t) pglob->gl_pathv);
    }
}


/* Do a collated comparison of A and B.  */
static int
collated_compare (
     const __ptr_t a,
     const __ptr_t b)
{
  const char *const s1 = *(const char *const * const) a;
  const char *const s2 = *(const char *const * const) b;

  if (s1 == s2)
    return 0;
  if (s1 == NULL)
    return 1;
  if (s2 == NULL)
    return -1;
  return strcoll (s1, s2);
}


/* Prepend DIRNAME to each of N members of ARRAY, replacing ARRAY's
   elements in place.  Return nonzero if out of memory, zero if successful.
   A slash is inserted between DIRNAME and each elt of ARRAY,
   unless DIRNAME is just "/".  Each old element of ARRAY is freed.  */
static int
prefix_array (
     const char *dirname,
     char **array,
     size_t n)
{
  register size_t i;
  size_t dirlen = strlen (dirname);

  if (dirlen == 1 && dirname[0] == '/')
    /* DIRNAME is just "/", so normal prepending would get us "//foo".
       We want "/foo" instead, so don't prepend any chars from DIRNAME.  */
    dirlen = 0;

  for (i = 0; i < n; ++i)
    {
      size_t eltlen = strlen (array[i]) + 1;
      /*
       * MLM This is buggy.  This doesn't retain the extra byte allocated
       * by glob_in_dir if the element is a directory.  We make it so that
       * all elements get the extra byte.
       */
      char *new_w = (char *) XtMalloc (dirlen + 2 + eltlen);
      if (new_w == NULL)
	{
	  while (i > 0)
	    XtFree ((__ptr_t) array[--i]);
	  return 1;
	}

      bcopy (dirname, new_w, dirlen);
      new_w[dirlen] = '/';
      bcopy (array[i], &new_w[dirlen + 1], eltlen);
      XtFree ((__ptr_t) array[i]);
      array[i] = new_w;
    }

  return 0;
}


/* Return nonzero if PATTERN contains any metacharacters.
   Metacharacters can be quoted with backslashes if QUOTE is nonzero.  */
static int
glob_pattern_p (
     const char *pattern,
     int quote)
{
  register const char *p;
  int open = 0;

  for (p = pattern; *p != '\0'; ++p)
    switch (*p)
      {
      case '?':
      case '*':
	return 1;

      case '\\':
	if (quote)
	  ++p;
	break;

      case '[':
	open = 1;
	break;

      case ']':
	if (open)
	  return 1;
	break;
      }

  return 0;
}


/* Like `glob', but PATTERN is a final pathname component,
   and matches are searched for in DIRECTORY.
   The GLOB_NOSORT bit in FLAGS is ignored.  No sorting is ever done.
   The GLOB_APPEND flag is assumed to be set (always appends).  */
static int
glob_in_dir (
     const char *pattern,
     const char *directory,
     int flags,
     int (*errfunc) __P ((const char *, int)),
     glob_t *pglob)
{
  __ptr_t stream;

  struct globlink
    {
      struct globlink *next;
      char *name;
    };
  struct globlink *names = NULL, *tail = NULL;
  size_t nfound = 0;

  if (!glob_pattern_p (pattern, !(flags & GLOB_NOESCAPE)))
    {
      stream = NULL;
      flags |= GLOB_NOCHECK;
    }
  else
    {
      flags |= GLOB_MAGCHAR;

      stream = (__glob_opendir_hook ? (*__glob_opendir_hook) (directory)
		: (__ptr_t) opendir (directory));
      if (stream == NULL)
	{
	  if ((errfunc != NULL && (*errfunc) (directory, errno)) ||
	      (flags & GLOB_ERR))
	    return GLOB_ABEND;
	}
      else
	while (1)
	  {
	    const char *name;
	    size_t len;

	    if (__glob_readdir_hook)
	      {
		name = (*__glob_readdir_hook) (stream);
		if (name == NULL)
		  break;
		len = 0;
	      }
	    else
	      {
		struct dirent *d = readdir ((DIR *) stream);
		if (d == NULL)
		  break;
		if (! REAL_DIR_ENTRY (d))
		  continue;
		name = d->d_name;
#ifdef	HAVE_D_NAMLEN
		len = d->d_namlen;
#else
		len = 0;
#endif
	      }
		
	    if (_Lesstif_fnmatch (pattern, name,
				  (!(flags & GLOB_PERIOD) ? FNM_PERIOD : 0) |
				  ((flags & GLOB_NOESCAPE) ? FNM_NOESCAPE : 0)) == 0)
	      {
		struct globlink *new_w
		  = (struct globlink *) __alloca (sizeof (struct globlink));
		if (len == 0)
		  len = strlen (name);
		new_w->name
		  = (char *) XtMalloc (len + ((flags & GLOB_MARK) ? 1 : 0) + 1);
		if (new_w->name == NULL)
		  goto memory_error;
		bcopy (name, (__ptr_t) new_w->name, len);
		new_w->name[len] = '\0';
#ifdef READDIR_LEXICOGRAPHIC_ORDER
		if (names == NULL)
		  names = tail = new_w;
		else
		  {
		    tail->next = new_w;
		    tail = new_w;
		  }
		new_w->next = NULL;
#else
		new_w->next = names;
		names = new_w;
#endif
		++nfound;
	      }
	  }
    }

  if (nfound == 0 && (flags & GLOB_NOCHECK))
    {
      size_t len = strlen (pattern);
      nfound = 1;
      names = (struct globlink *) __alloca (sizeof (struct globlink));
      names->next = NULL;
      names->name = (char *) XtMalloc (len + 1);
      if (names->name == NULL)
	goto memory_error;
      bcopy (pattern, names->name, len);
      names->name[len] = '\0';
    }

  pglob->gl_pathv
    = (char **) XtRealloc ((char *)pglob->gl_pathv,
			 (pglob->gl_pathc +
			  ((flags & GLOB_DOOFFS) ? pglob->gl_offs : 0) +
			  nfound + 1) *
			 sizeof (char *));
  if (pglob->gl_pathv == NULL)
    goto memory_error;

  if (flags & GLOB_DOOFFS)
    while (pglob->gl_pathc < pglob->gl_offs)
      pglob->gl_pathv[pglob->gl_pathc++] = NULL;

  for (; names != NULL; names = names->next)
    pglob->gl_pathv[pglob->gl_pathc++] = names->name;
  pglob->gl_pathv[pglob->gl_pathc] = NULL;

  pglob->gl_flags = flags;

  if (stream != NULL)
    {
      int save = errno;
      if (__glob_closedir_hook)
	(*__glob_closedir_hook) (stream);
      else
	(void) closedir ((DIR *) stream);
      errno = save;
    }
  return nfound == 0 ? GLOB_NOMATCH : 0;

 memory_error:
  {
    int save = errno;
    if (__glob_closedir_hook)
      (*__glob_closedir_hook) (stream);
    else
      (void) closedir ((DIR *) stream);
    errno = save;
  }
  while (names != NULL)
    {
      if (names->name != NULL)
	XtFree ((__ptr_t) names->name);
      names = names->next;
    }
  return GLOB_NOSPACE;
}
