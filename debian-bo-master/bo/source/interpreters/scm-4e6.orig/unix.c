/* Copyright (C) 1994, 1995 Free Software Foundation, Inc.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * As a special exception, the Free Software Foundation gives permission
 * for additional uses of the text contained in its release of GUILE.
 *
 * The exception is that, if you link the GUILE library with other files
 * to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the GUILE library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the
 * Free Software Foundation under the name GUILE.  If you copy
 * code from other Free Software Foundation releases into a copy of
 * GUILE, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for GUILE, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  
 */

/* "unix.c" functions only in Unix (unix).
   Author: Aubrey Jaffer */

#include "scm.h"

#include <pwd.h>
#include <sys/types.h>
/* #include <sys/wait.h> */
#include <sys/stat.h>

SCM	stat2scm P((struct stat *stat_temp));

#ifndef STDC_HEADERS
	void sync P((void));
	int symlink P((const char *oldpath, const char *newpath));
	int readlink P((const char *path, char *buf, sizet bufsiz));
	int acct P((const char *filename));
	int nice P((int inc));
#endif /* STDC_HEADERS */

   /* Only the superuser can successfully execute mknod and acct */
/* int mknod P((const char *path, mode_t mode, dev_t dev));
   should be in stat.h */
static char s_mknod[] = "mknod";
SCM l_mknod(path, mode, dev)
     SCM path, mode, dev;
{
  int val;
  ASSERT(NIMP(path) && STRINGP(path), path, ARG1, s_mknod);
  ASSERT(INUMP(mode), mode, ARG2, s_mknod);
  ASSERT(INUMP(dev), dev, ARG3, s_mknod);
  SYSCALL(val = mknod(CHARS(path), INUM(mode), INUM(dev)););
  return val ? BOOL_F : BOOL_T;
}
static char s_acct[] = "acct";
SCM l_acct(path)
     SCM path;
{
  int val;
  if FALSEP(path) {
    SYSCALL(val = acct(0););
    return val ? BOOL_F : BOOL_T;
  }
  ASSERT(NIMP(path) && STRINGP(path), path, ARG1, s_acct);
  SYSCALL(val = acct(CHARS(path)););
  return val ? BOOL_F : BOOL_T;
}

static char s_nice[] = "nice";
SCM l_nice(incr)
     SCM incr;
{
  ASSERT(INUMP(incr), incr, ARG1, s_nice);
  return nice(INUM(incr)) ? BOOL_F : BOOL_T;
}

SCM l_sync()
{
  sync();
  return UNSPECIFIED;
}

static char s_symlink[] = "symlink";
SCM l_symlink(oldpath, newpath)
     SCM oldpath, newpath;
{
  int val;
  ASSERT(NIMP(oldpath) && STRINGP(oldpath), oldpath, ARG1, s_symlink);
  ASSERT(NIMP(newpath) && STRINGP(newpath), newpath, ARG2, s_symlink);
  SYSCALL(val = symlink(CHARS(oldpath), CHARS(newpath)););
  return val ? BOOL_F : BOOL_T;
}
static char s_readlink[] = "readlink";
SCM l_readlink(path)
  SCM path;
{
  int i;
  char buf[1024];
  ASSERT(NIMP(path) && STRINGP(path), path, ARG1, s_readlink);
  SYSCALL(i = readlink(CHARS(path), buf, (sizet)sizeof(buf)););
  if (-1==i) return BOOL_F;
  return makfromstr(buf, (sizet)i);
}
static char s_lstat[] = "lstat";
SCM l_lstat(str)
  SCM str;
{
  int i;
  struct stat stat_temp;
  ASSERT(NIMP(str) && STRINGP(str), str, ARG1, s_lstat);
  SYSCALL(i = lstat(CHARS(str), &stat_temp););
  if (i) return BOOL_F;
  return stat2scm(&stat_temp);
}

static iproc subr1s[] = {
	{s_nice, l_nice},
	{s_acct, l_acct},
	{s_lstat, l_lstat},
	{s_readlink, l_readlink},
	{0, 0}};

void init_unix()
{
	make_subr("sync", tc7_subr_0, l_sync);
	init_iprocs(subr1s, tc7_subr_1);
	make_subr(s_symlink, tc7_subr_2, l_symlink);
	make_subr(s_mknod, tc7_subr_3, l_mknod);
	add_feature("unix");
}
