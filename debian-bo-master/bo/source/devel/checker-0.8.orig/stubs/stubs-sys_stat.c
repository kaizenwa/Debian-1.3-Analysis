/* Checker stubs for functions defined in sys/stat.h
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

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#include "checker_api.h"

#if 0
#define HAVE__xstat
#define HAVE_stat
#define HAVE_lstat
#define HAVE_fstat
#define HAVE_mknod
#define HAVE_umask
#define HAVE_mkfifo
#define HAVE_mkdir
#define HAVE_chmod
#define HAVE_fchmod
#endif

#ifdef HAVE_chkr_func
void
stubs_chkr_set_right_struct_stat (struct stat *buf)
{
  stubs_chkr_set_right (&(buf->st_dev), sizeof (dev_t), CHKR_RW);
  stubs_chkr_set_right (&(buf->st_ino), sizeof (ino_t), CHKR_RW);
  stubs_chkr_set_right (&(buf->st_mode), sizeof (mode_t), CHKR_RW);
  stubs_chkr_set_right (&(buf->st_nlink), sizeof (nlink_t), CHKR_RW);
  stubs_chkr_set_right (&(buf->st_uid), sizeof (uid_t), CHKR_RW);
  stubs_chkr_set_right (&(buf->st_gid), sizeof (gid_t), CHKR_RW);
  stubs_chkr_set_right (&(buf->st_rdev), sizeof (dev_t), CHKR_RW);
  stubs_chkr_set_right (&(buf->st_size), sizeof (off_t), CHKR_RW);
  stubs_chkr_set_right (&(buf->st_atime), sizeof (time_t), CHKR_RW);
  stubs_chkr_set_right (&(buf->st_mtime), sizeof (time_t), CHKR_RW);
  stubs_chkr_set_right (&(buf->st_ctime), sizeof (time_t), CHKR_RW);
  stubs_chkr_set_right (&(buf->st_blksize), sizeof (unsigned int), CHKR_RW);
  stubs_chkr_set_right (&(buf->st_blocks), sizeof (unsigned int), CHKR_RW);
}
#else
void stubs_chkr_set_right_struct_stat (struct stat *buf);
#endif

/* compiled from: . */
#ifdef HAVE__fxstat
int
chkr$_fxstat (int ver, int fd, struct stat *buf)
{
  int res;
  stubs_chkr_check_addr (buf, sizeof (struct stat), CHKR_MW, "buf");
  res = _fxstat (ver, fd, buf);
  if (res != -1)
    stubs_chkr_set_right_struct_stat (buf);
  return res;
}
#endif /* HAVE__fxstat */

#ifdef HAVE__xstat
int
chkr$_xstat (int ver, const char *name, struct stat *buf)
{
  int res;
  stubs_chkr_check_str (name, CHKR_RO, "name");
  stubs_chkr_check_addr (buf, sizeof (struct stat), CHKR_MW, "buf");
  res = _xstat (ver, name, buf);
  if (res != -1)
    stubs_chkr_set_right_struct_stat (buf);
  return res;
}
#endif /* HAVE__xstat */

#ifdef HAVE__lxstat
int
chkr$_lxstat (int ver, const char *name, struct stat *buf)
{
  int res;
  stubs_chkr_check_str (name, CHKR_RO, "name");
  stubs_chkr_check_addr (buf, sizeof (struct stat), CHKR_MW, "buf");
  res = _lxstat (ver, name, buf);
  if (res != -1)
    stubs_chkr_set_right_struct_stat (buf);
  return res;
}
#endif /* HAVE__lxstat */

#ifdef HAVE__xmknod
int
chkr$_xmknod (int ver, const char *path, mode_t mode, dev_t *dev)
{
  stubs_chkr_check_addr (dev, sizeof (dev_t), CHKR_RO, "dev");
  stubs_chkr_check_str (path, CHKR_RO, "path");
#if USE_BI_JUMP
  __builtin_jump (_xmknod);
#else
  return _xmknod (ver, path, mode, dev);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE__xmknod */

#ifdef HAVE_chmod
int
chkr$chmod (const char *path, mode_t mode)
{
  stubs_chkr_check_str (path, CHKR_RO, "path");
#if USE_BI_JUMP
  __builtin_jump (chmod);
#else
  return chmod (path, mode);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_chmod */

#ifdef HAVE_fchmod
int
chkr$fchmod (int fd, mode_t mode)
{
#if USE_BI_JUMP
  __builtin_jump (fchmod);
#else
  return fchmod (fd, mode);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_fchmod */

#ifdef HAVE_mkdir
int
chkr$mkdir (const char *path, mode_t mode)
{
  stubs_chkr_check_str (path, CHKR_RO, "path");
#if USE_BI_JUMP
  __builtin_jump (mkdir);
#else
  return mkdir (path, mode);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_mkdir */

#ifdef HAVE_mkfifo
int
chkr$mkfifo (const char *path, mode_t mode)
{
  stubs_chkr_check_str (path, CHKR_RO, "path");
#if USE_BI_JUMP
  __builtin_jump (mkfifo);
#else
  return mkfifo (path, mode);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_mkfifo */

#ifdef HAVE_umask
mode_t
chkr$umask (mode_t mask)
{
#if USE_BI_JUMP
  __builtin_jump (umask);
#else
  return umask (mask);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_umask */

#ifdef HAVE_stat
/* This was a static declaration.  */
int
chkr$stat (const char *name, struct stat *buf)
{
  int res;
  stubs_chkr_check_str (name, CHKR_RO, "name");
  stubs_chkr_check_addr (buf, sizeof (struct stat), CHKR_MW, "buf");
  res = stat (name, buf);
  if (res != -1)
    stubs_chkr_set_right_struct_stat (buf);
  return res;
}
#endif /* HAVE_stat */

#ifdef HAVE_lstat
int
chkr$lstat (const char *name, struct stat *buf)
{
  int res;
  stubs_chkr_check_str (name, CHKR_RO, "name");
  stubs_chkr_check_addr (buf, sizeof (struct stat), CHKR_MW, "buf");
  res = lstat (name, buf);
  if (res != -1)
    stubs_chkr_set_right_struct_stat (buf);
  return res;
}
#endif /* HAVE_lstat */

#ifdef HAVE_fstat
int
chkr$fstat (int fd, struct stat *buf)
{
  int res;
  stubs_chkr_check_addr (buf, sizeof (struct stat), CHKR_MW, "buf");
  res = fstat (fd, buf);
  if (res != -1)
    stubs_chkr_set_right_struct_stat (buf);
  return res;
}
#endif /* HAVE_fstat */

#ifdef HAVE_mknod
int
chkr$mknod (const char *path, mode_t mode, dev_t dev)
{
  stubs_chkr_check_str (path, CHKR_RO, "path");
#if USE_BI_JUMP
  __builtin_jump (mknod);
#else
  return mknod (path, mode, dev);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_mknod */

#endif /* HAVE_SYS_STAT_H */
