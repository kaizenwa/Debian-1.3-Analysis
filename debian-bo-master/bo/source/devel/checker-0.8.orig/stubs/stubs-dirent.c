/* Checker stubs for functions defined in dirent.h
   Copyright 1995, 1996 Tristan Gingold
		  Written December 1995 by Tristan Gingold

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License 
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

 The author may be reached by US/French mail:
		Tristan Gingold 
		8 rue Parmentier
		F-91120 PALAISEAU
		FRANCE
*/
#include "available-stubs.h"

#ifdef HAVE_DIRENT_H
#include <dirent.h>
#include "checker_api.h"

#undef HAVE_alphasort
#undef HAVE_getdirentries

#if 0
#define HAVE_opendir
#define HAVE_closedir
#define HAVE_readdir
#define HAVE_rewinddir
#define HAVE_seekdir
#define HAVE_telldir
#endif

#ifdef HAVE_chkr_func
void
stubs_chkr_check_struct_DIR (DIR *dir)
{
  /* Not very good... */
  stubs_chkr_check_addr (dir, sizeof (int), CHKR_TW, "dir");
}  

#define NAME_OFFSET(d) ((int) (d->d_name - (char *)d))
void
stubs_chkr_set_struct_dirent (struct dirent *dirent)
{
  stubs_chkr_set_right (&(dirent->d_ino), sizeof (long), CHKR_RW);
  stubs_chkr_set_right (&(dirent->d_off), sizeof (off_t), CHKR_RW);
  stubs_chkr_set_right (&(dirent->d_reclen), sizeof (unsigned short), CHKR_RW);
  stubs_chkr_set_right (&(dirent->d_name), dirent->d_reclen - NAME_OFFSET (dirent),
  				     CHKR_RW);
}  
#else
void stubs_chkr_check_struct_DIR (DIR *dir);
void stubs_chkr_set_struct_dirent (struct dirent *dirent);
#endif

/* compiled from: . */
#ifdef HAVE_opendir
/* From `/usr/include/dirent.h:63'.  */
DIR *
chkr$opendir (const char *path)
{
  DIR * res;
  stubs_chkr_check_str (path, CHKR_RO, "path");
  res = opendir (path);
  return res;
}
#endif /* HAVE_opendir */

#ifdef HAVE_closedir
/* From `/usr/include/dirent.h:67'.  */
int
chkr$closedir (DIR *dir)
{
  stubs_chkr_check_struct_DIR (dir);
#if USE_BI_JUMP
  __builtin_jump (closedir);
#else
  return closedir (dir);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_closedir */

#ifdef HAVE_readdir
/* From `/usr/include/dirent.h:73'.  */
struct dirent *
chkr$readdir (DIR *dir)
{
  struct dirent *res;
  stubs_chkr_check_struct_DIR (dir);
  res = readdir (dir);
  if (res)
    stubs_chkr_set_struct_dirent (res);
  return res;
}
#endif /* HAVE_readdir */

#ifdef HAVE_rewinddir
/* From `/usr/include/dirent.h:76'.  */
void
chkr$rewinddir (DIR *dir)
{
  stubs_chkr_check_struct_DIR (dir);
#if USE_BI_JUMP
  __builtin_jump (rewinddir);
#else
  rewinddir (dir);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_rewinddir */

#ifdef HAVE_seekdir
/* From `/usr/include/dirent.h:95'.  */
void
chkr$seekdir (DIR *dir, off_t off)
{
  stubs_chkr_check_struct_DIR (dir);
#if USE_BI_JUMP
  __builtin_jump (seekdir);
#else
  seekdir (dir, off);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_seekdir */

#ifdef HAVE_telldir
/* From `/usr/include/dirent.h:98'.  */
off_t
chkr$telldir (DIR *dir)
{
  stubs_chkr_check_struct_DIR (dir);
#if USE_BI_JUMP
  __builtin_jump (telldir);
#else
  return telldir (dir);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_telldir */

#ifdef HAVE_alphasort
/* From `/usr/include/dirent.h:120'.  */
int
chkr$alphasort (const struct dirent *const * arg0, const struct dirent *const * arg1)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (struct dirent *), CHKR_XX);
  stubs_chkr_check_addr (arg1, sizeof (struct dirent *), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (alphasort);
#else
  {
    int res;
    res = alphasort (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_alphasort */

#ifdef HAVE_getdirentries
/* From `/usr/include/dirent.h:130'.  */
__ssize_t
chkr$getdirentries (int arg0, char * arg1, size_t arg2, __off_t * arg3)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg1, sizeof (char), CHKR_XX);
  stubs_chkr_check_addr (arg3, sizeof (__off_t), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (getdirentries);
#else
  {
    __ssize_t res;
    res = getdirentries (arg0, arg1, arg2, arg3);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_getdirentries */

#ifdef HAVE_scandir
/* A real stubs for scandir is not efficient.  So I have modified scandir.
   If you really want a stub, see below.  */
#if 1
/* Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.
This file is part of the GNU C Library.

The GNU C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU C Library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

#include <errno.h>

int
chkr$scandir (const char *dir, struct dirent ***namelist,
	      int (*select)(struct dirent *),
	      int (*cmp)(const struct dirent * const *, const struct dirent * const *))
{
  DIR *dp;
  struct dirent **v = NULL;
  size_t vsize = 0, i;
  struct dirent *d;
  int save;

  stubs_chkr_check_str (dir, CHKR_RO, "dir");
  stubs_chkr_check_addr (namelist, sizeof (struct dirent **), CHKR_MW, "namelist");
  if (select)
    stubs_chkr_check_exec (select, "select");
  if (cmp)
    stubs_chkr_check_exec (cmp, "cmp");
    
  dp = opendir (dir);
  if (dp == NULL)
    return -1;

  save = errno;
  errno = 0;

  i = 0;
  while ((d = readdir (dp)) != NULL)
    {
      stubs_chkr_set_struct_dirent (d);
      if (select == NULL || (*select) (d))
        {
	  if (i == vsize)
	    {
	      struct dirent **new;
	      if (vsize == 0)
	        vsize = 10;
	      else
	        vsize *= 2;
	      new = (struct dirent **) realloc (v, vsize * sizeof (*v));
	      if (new == NULL)
	        {
	        lose:
		  closedir (dp);
		  while (i > 0)
		    free (v[--i]);
		  free (v);
		  errno = ENOMEM;
		  return -1;
	        }
	      v = new;
	    }

	  v[i] = (struct dirent *) malloc (sizeof (**v));
	  if (v[i] == NULL)
	    goto lose;

	  *v[i++] = *d;
        }
      }

  if (errno != 0)
    {
      save = errno;

      closedir (dp);

      /* Remember to free allocated memory upon error! */
      while (i > 0)
        free(v[--i]);
      free(v);

      errno = save;
      return -1;
    }

  closedir (dp);
  errno = save;

  *namelist = v;
  stubs_chkr_set_right (namelist, sizeof (struct dirent **), CHKR_RW);
  if (i)
    {
      int j;
      
      stubs_chkr_set_right (*namelist, i * sizeof (struct dirent *), CHKR_RW);
      for (j = 0; j < i; j++)
	stubs_chkr_set_struct_dirent (v[j]);
    }
    
  /* Sort the list if we have a comparison function to sort with.  */
  if (cmp != NULL)
    qsort (v, i, sizeof (*v), cmp);
  return i;
}
#else
/* A re-entrant stub can be done with local functions...  */
static int (*scandir_select_arg)(struct dirent *);
static int (*scandir_cmp_arg)(const struct dirent * const *, const struct dirent * const *);

static
int scandir_select (struct dirent *d)
{
  stubs_chkr_set_struct_dirent (d);
  return (*scandir_select_arg)(d);
}

static int
scandir_cmp (const struct dirent * const *d1, const struct dirent * const *d2)
{
  stubs_chkr_set_struct_dirent (d1);
  stubs_chkr_set_struct_dirent (d2);
  return (*scandir_cmp_arg)(d1, d2);
}

int
chkr$scandir (const char *dir, struct dirent ***namelist,
	      int (*select)(struct dirent *),
	      int (*cmp)(const struct dirent * const *, const struct dirent * const ))
{
  int res;
  
  stubs_chkr_check_str (dir, CHKR_RO, "dir");
  stubs_chkr_check_addr (namelist, sizeof (struct dirent **), CHKR_MW, "namelist");
  if (select)
    stubs_chkr_check_exec (select, "select");
  if (cmp)
    stubs_chkr_check_exec (cmp, "cmp");
  scandir_select_arg = select;
  scandir_cmp_arg = cmp;
  res = scandir (dir, namelist, scandir_select_arg, scandir_cmp_arg);
  stubs_chkr_set_right (namelist, sizeof (struct dirent **), CHKR_RW);
  if (res)
    {
      int j;
      
      stubs_chkr_set_right (*namelist, res * sizeof (struct dirent *), CHKR_RW);
      for (j = 0; j < res; j++)
	stubs_chkr_set_struct_dirent ((*namelist)[j]);
    }
  return res;
}
#endif
#endif /* HAVE_scandir */
  
#endif /* HAVE_DIRENT_H */
