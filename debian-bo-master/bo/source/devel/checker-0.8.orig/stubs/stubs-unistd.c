/* Checker stubs for functions defined in unistd.h
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

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#include <sys/param.h>
#include <stdarg.h>
#include <limits.h>
#include "checker_api.h"

#undef HAVE_execle
#undef HAVE_cuserid
#undef HAVE_ctermid
#undef HAVE_setlogin
#undef HAVE_sethostname
#undef HAVE_chroot
#undef HAVE_swapon
#undef HAVE_getpass
#undef HAVE_brk
#undef HAVE_sbrk
#undef HAVE_crypt
#undef HAVE_encrypt
#undef HAVE_setkey
#undef HAVE_mkstemp
#undef HAVE_profil
#undef HAVE__xustat
#undef HAVE_swapoff
#undef HAVE_uselib
#undef HAVE_getdomainname
#undef HAVE_setdomainname
#undef HAVE_realpath

/* Pb with return.  */
#undef HAVE_llseek
#undef HAVE_sync
#undef HAVE_vhangup

#ifndef MAXARGS
#define MAXARGS _POSIX_ARG_MAX
#endif

/* compiled from: . */
#ifdef HAVE_access
int
chkr$access (const char *path, int mode)
{
  stubs_chkr_check_str (path, CHKR_RO, "path");
#if USE_BI_JUMP
  __builtin_jump (access);
#else
  return access (path, mode);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_access */

#ifdef HAVE_lseek
off_t
chkr$lseek (int fd, off_t offset, int whence)
{
  fd_used_by_prog (fd);
#if USE_BI_JUMP
  __builtin_jump (lseek);
#else
  return lseek (fd, offset, whence);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_lseek */

#ifdef HAVE_llseek
loff_t
chkr$llseek (int fd, loff_t offset, int whence)
{
  fd_used_by_prog (fd);
#if USE_BI_JUMP
  __builtin_jump (llseek);
#else
  return llseek (fd, offset, whence);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_llseek */

#ifdef HAVE_close
int
chkr$close (int fd)
{
  int res;
  
  fd_used_by_prog (fd);
  res = close (fd);
  if (res != -1)
    fd_closed (fd);
  return res;
}
#endif /* HAVE_close */

#ifdef HAVE_read
size_t
chkr$read (int fd, PTR buf, size_t n)
{
  size_t len;
  
  fd_used_by_prog (fd);
  if (n > 0)
    stubs_chkr_check_addr (buf, n, CHKR_MW, "buf");
  len = read (fd, buf, n);
  if (len > 0)
    stubs_chkr_set_right (buf, len, CHKR_RW);
  return len;
}
#endif /* HAVE_read */

#ifdef HAVE_write
size_t
chkr$write (int fd, const PTR buf, size_t n)
{
  fd_used_by_prog (fd);
  if (n > 0)
    stubs_chkr_check_addr (buf, n, CHKR_RO, "buf");
  return write (fd, buf, n);
}
#endif /* HAVE_write */

#ifdef HAVE_pipe
int
chkr$pipe (int fd[2])
{
  int res;
  
  stubs_chkr_check_addr (fd, 2 * sizeof (int), CHKR_MW, "fd");
  res = pipe (fd);
  if (res != -1)
    {
      stubs_chkr_set_right (fd, 2 * sizeof (int), CHKR_RW);
      fd_returned_by_system (fd[0]);
      fd_returned_by_system (fd[1]);
    }
  return res;
}
#endif /* HAVE_pipe */

#ifdef HAVE_alarm
unsigned int
chkr$alarm (unsigned int arg)
{
#if USE_BI_JUMP
  __builtin_jump (alarm);
#else
  return alarm (arg);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_alarm */

#ifdef HAVE_sleep
unsigned int
chkr$sleep (unsigned int arg)
{
#if USE_BI_JUMP
  __builtin_jump (sleep);
#else
  return sleep (arg);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_sleep */

#ifdef HAVE_pause
int
chkr$pause (void)
{
#if USE_BI_JUMP
  __builtin_jump (pause);
#else
  return pause ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_pause */

#ifdef HAVE_chown
int
chkr$chown (const char *path, uid_t uid, gid_t gid)
{
  stubs_chkr_check_str (path, CHKR_RO, "path");
#if USE_BI_JUMP
  __builtin_jump (chown);
#else
  return chown (path, uid, gid);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_chown */

#ifdef HAVE_fchown
int
chkr$fchown (int fd, uid_t uid, gid_t gid)
{
  fd_used_by_prog (fd);
#if USE_BI_JUMP
  __builtin_jump (fchown);
#else
  return fchown (fd, uid, gid);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_fchown */

#ifdef HAVE_fchdir
int
chkr$fchdir (int fd)
{
  fd_used_by_prog (fd);
#if USE_BI_JUMP
  __builtin_jump (fchdir);
#else
  return fchdir (fd);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_fchdir */

#ifdef HAVE_chdir
int
chkr$chdir (const char *path)
{
  stubs_chkr_check_str (path, CHKR_RO, "path");
#if USE_BI_JUMP
  __builtin_jump (chdir);
#else
  return chdir (path);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_chdir */

#ifdef HAVE_getcwd
char *
chkr$getcwd (char *buf, size_t size)
{
  char *res;
  
  if (buf)
    stubs_chkr_check_addr (buf, size, CHKR_MW, "buf");
  res = getcwd (buf, size);
  if (res)
    stubs_chkr_set_right (res, strlen (res) + 1, CHKR_RW);
  return res;
}
#endif /* HAVE_getcwd */

#ifdef HAVE_get_current_dir_name
char *
chkr$get_current_dir_name (void)
{
  char * res;
  res = get_current_dir_name ();
  if (res)
    stubs_chkr_set_right (res, strlen (res) + 1, CHKR_RW);
  return res;
}
#endif /* HAVE_get_current_dir_name */

#ifdef HAVE_getwd
#ifndef PATH_MAX
#define PATH_MAX MAXPATHLEN
#endif
char *
chkr$getwd (char *buf)
{
  char *res;
  stubs_chkr_check_addr (buf, PATH_MAX, CHKR_MW, "buf");
  res = getwd (buf);
  if (res)
    stubs_chkr_set_right (buf, strlen (buf) + 1, CHKR_RW);
  return res;
}
#endif /* HAVE_getwd */

#ifdef HAVE_dup
int
chkr$dup (int fd)
{
  int res;
  
  fd_used_by_prog (fd);
  res = dup (fd);
  if (res != -1)
    fd_duped (res, fd);
  return res;
}
#endif /* HAVE_dup */

#ifdef HAVE_dup2
int
chkr$dup2 (int fd1, int fd2)
{
  int res;
  
  fd_used_by_prog (fd1);
  fd_used_by_prog (fd2);
  res = dup2 (fd1, fd2);
  if (res != -1)
    fd_duped (res, fd1);
  return res;
}
#endif /* HAVE_dup2 */

#ifdef HAVE_execve
int
chkr$execve (const char *name, char *const argv[], char *const envp[])
{
  char **arg;
  
  stubs_chkr_check_str (name, CHKR_RO, "name");
  for (arg = (char **)argv; *arg; arg++)
    stubs_chkr_check_str (*arg, CHKR_RO, "argv[i]");
  stubs_chkr_check_addr (argv, arg + 1 - argv, CHKR_RO, "argv");
  for (arg = (char **)envp; *arg; arg++)
    stubs_chkr_check_str (*arg, CHKR_RO, "envp[i]");
  stubs_chkr_check_addr (envp, arg + 1 - envp, CHKR_RO, "envp");
  chkr_clean_before_exec ();
#if USE_BI_JUMP
  __builtin_jump (execve);
#else
  return execve (name, argv, envp);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_execve */

#ifdef HAVE_execv
int
chkr$execv (const char *path, char *const argv[])
{
  char * const *arg;
  
  stubs_chkr_check_str (path, CHKR_RO, "path");
  for (arg = (char **)argv; *arg; arg++)
    stubs_chkr_check_str (*arg, CHKR_RO, "argv[i]");
  stubs_chkr_check_addr (argv, arg + 1 - argv, CHKR_RO, "argv");
  chkr_clean_before_exec ();
#if USE_BI_JUMP
  __builtin_jump (execv);
#else
  return execv (path, argv);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_execv */

#ifdef HAVE_execle
int
chkr$execle (const char *path, const char *args, ... )
{
  va_list va;
  int arg;
  const char *array[MAXARGS];
  
  va_start (va, args);
  stubs_chkr_check_str (path, CHKR_RO, "path");
  array[0] = args;
  if (args)
    for (arg = 1; array[arg] = va_arg (va, char *); arg++)
      stubs_chkr_check_str (array[arg], CHKR_RO, "argv[i]");
  va_end (va);
  chkr_clean_before_exec ();
  return execve (path, (char *const *)array);
}
#endif /* HAVE_execle */

#ifdef HAVE_execl
int
chkr$execl (const char *path, const char *args, ... )
{
  va_list va;
  int arg;
  const char *array[MAXARGS];
  
  va_start (va, args);
  stubs_chkr_check_str (path, CHKR_RO, "path");
  array[0] = args;
  if (args)
    for (arg = 1; (array[arg] = va_arg (va, char *)); arg++)
      stubs_chkr_check_str (array[arg], CHKR_RO, "argv[i]");
  va_end (va);
  chkr_clean_before_exec ();
  return execv (path, (char *const *)array);
}
#endif /* HAVE_execl */

#ifdef HAVE_execvp
int
chkr$execvp (const char *path, char *const argv[])
{
  char **arg;
  
  stubs_chkr_check_str (path, CHKR_RO, "path");
  for (arg = (char **)argv; *arg; arg++)
    stubs_chkr_check_str (*arg, CHKR_RO, "argv[i]");
  stubs_chkr_check_addr (argv, arg + 1 - argv, CHKR_RO, "argv");
  chkr_clean_before_exec ();
#if USE_BI_JUMP
  __builtin_jump (execvp);
#else
  return execvp (path, argv);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_execvp */

#ifdef HAVE_execlp
int
chkr$execlp (const char *path, const char *args, ... )
{
  va_list va;
  int arg;
  const char *array[MAXARGS];
  
  va_start (va, args);
  stubs_chkr_check_str (path, CHKR_RO, "path");
  array[0] = args;
  if (args)
    for (arg = 1; array[arg] = va_arg (va, char *); arg++)
      stubs_chkr_check_str (array[arg], CHKR_RO, "argv[i]");
  va_end (va);
  chkr_clean_before_exec ();
  return execvp (path, (char *const *)array);
}
#endif /* HAVE_execlp */

#ifdef HAVE__exit
void
chkr$_exit (int status)
{
#if USE_BI_JUMP
  __builtin_jump (_exit);
#else
  _exit (status);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE__exit */

#ifdef HAVE_pathconf
long int
chkr$pathconf (const char *path, int name)
{
  stubs_chkr_check_str (path, CHKR_RO, "path");
#if USE_BI_JUMP
  __builtin_jump (pathconf);
#else
  return pathconf (path, name);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_pathconf */

#ifdef HAVE_fpathconf
long int
chkr$fpathconf (int fd, int name)
{
  fd_used_by_prog (fd);
#if USE_BI_JUMP
  __builtin_jump (fpathconf);
#else
  return fpathconf (fd, name);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_fpathconf */

#ifdef HAVE_sysconf
long int
chkr$sysconf (int name)
{
#if USE_BI_JUMP
  __builtin_jump (sysconf);
#else
  return sysconf (name);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_sysconf */

#ifdef HAVE_confstr
size_t
chkr$confstr (int name, char *buf, size_t len)
{
  size_t res;
  if (buf && len)
    stubs_chkr_check_addr (buf, len, CHKR_MW, "buf");
  res = confstr (name, buf, len);
  if (buf && len && res)
    stubs_chkr_set_right (buf, strlen (buf) + 1, CHKR_RW);
  return res;
}
#endif /* HAVE_confstr */

#ifdef HAVE_getpid
pid_t
chkr$getpid (void)
{
#if USE_BI_JUMP
  __builtin_jump (getpid);
#else
  return getpid ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_getpid */

#ifdef HAVE_getppid
pid_t
chkr$getppid (void)
{
#if USE_BI_JUMP
  __builtin_jump (getppid);
#else
  return getppid ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_getppid */

#ifdef HAVE_getpgid
pid_t
chkr$getpgid (pid_t pid)
{
#if USE_BI_JUMP
  __builtin_jump (getpgid);
#else
  return getpgid (pid);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_getpgid */

#ifdef HAVE_getpgrp
pid_t
chkr$getpgrp (void)
{
#if USE_BI_JUMP
  __builtin_jump (getpgrp);
#else
  return getpgrp ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_getpgrp */

#ifdef HAVE_setpgid
int
chkr$setpgid (pid_t pid, pid_t pgid)
{
#if USE_BI_JUMP
  __builtin_jump (setpgid);
#else
  return setpgid (pid, pgid);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_setpgid */

#ifdef HAVE_setpgrp
int
chkr$setpgrp (void)
{
#if USE_BI_JUMP
  __builtin_jump (setpgrp);
#else
  return setpgrp ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_setpgrp */

#ifdef HAVE_setsid
pid_t
chkr$setsid (void)
{
#if USE_BI_JUMP
  __builtin_jump (setsid);
#else
  return setsid ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_setsid */

#ifdef HAVE_getuid
uid_t
chkr$getuid (void)
{
#if USE_BI_JUMP
  __builtin_jump (getuid);
#else
  return getuid ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_getuid */

#ifdef HAVE_geteuid
uid_t
chkr$geteuid (void)
{
#if USE_BI_JUMP
  __builtin_jump (geteuid);
#else
  return geteuid ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_geteuid */

#ifdef HAVE_getgid
gid_t
chkr$getgid (void)
{
#if USE_BI_JUMP
  __builtin_jump (getgid);
#else
  return getgid ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_getgid */

#ifdef HAVE_getegid
gid_t
chkr$getegid (void)
{
#if USE_BI_JUMP
  __builtin_jump (getegid);
#else
  return getegid ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_getegid */

#ifdef HAVE_getgroups
int
chkr$getgroups (int size, gid_t *list)
{
  int res;
  stubs_chkr_check_addr (list, size * sizeof (gid_t), CHKR_MW, "list");
  res = getgroups (size, list);
  if (res > 0 && size > 0)
    stubs_chkr_set_right (list, res * sizeof (gid_t), CHKR_RW);  
  return res;
}
#endif /* HAVE_getgroups */

#ifdef HAVE_setuid
int
chkr$setuid (uid_t uid)
{
#if USE_BI_JUMP
  __builtin_jump (setuid);
#else
  return setuid (uid);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_setuid */

#ifdef HAVE_setreuid
int
chkr$setreuid (uid_t ruid, uid_t euid)
{
#if USE_BI_JUMP
  __builtin_jump (setreuid);
#else
  return setreuid (ruid, euid);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_setreuid */

#ifdef HAVE_seteuid
int
chkr$seteuid (uid_t euid)
{
#if USE_BI_JUMP
  __builtin_jump (seteuid);
#else
  return seteuid (euid);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_seteuid */

#ifdef HAVE_setgid
int
chkr$setgid (gid_t gid)
{
#if USE_BI_JUMP
  __builtin_jump (setgid);
#else
  return setgid (gid);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_setgid */

#ifdef HAVE_setregid
int
chkr$setregid (gid_t rgid, gid_t egid)
{
#if USE_BI_JUMP
  __builtin_jump (setregid);
#else
  return setregid (rgid, egid);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_setregid */

#ifdef HAVE_setegid
int
chkr$setegid (uid_t egid)
{
#if USE_BI_JUMP
  __builtin_jump (setegid);
#else
  return setegid (egid);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_setegid */

#ifdef HAVE_setfsuid
int
chkr$setfsuid (uid_t fsuid)
{
#if USE_BI_JUMP
  __builtin_jump (setfsuid);
#else
  return setfsuid (fsuid);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_setfsuid */

#ifdef HAVE_setfsgid
int
chkr$setfsgid (gid_t fsgid)
{
#if USE_BI_JUMP
  __builtin_jump (setfsgid);
#else
  return setfsgid (fsgid);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_setfsgid */

#ifdef HAVE_fork
pid_t
chkr$fork (void)
{
  pid_t res;
  res = fork ();
  if (res == 0)
    chkr_clean_after_fork ();
  return res;
}
#endif /* HAVE_fork */

#ifdef HAVE_vfork
pid_t
chkr$vfork (void)
{
  pid_t res;
  
  /* We can't use vfork because of chkr_clean_after_fork.  */
  res = fork ();
  if (res == 0)
    chkr_clean_after_fork ();
  return res;
}
#endif /* HAVE_vfork */

#ifdef HAVE_cuserid
char *
chkr$cuserid (char * arg0)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (cuserid);
#else
  {
    char * res;
    res = cuserid (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_cuserid */

#ifdef HAVE_ctermid
char *
chkr$ctermid (char * arg0)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (ctermid);
#else
  {
    char * res;
    res = ctermid (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_ctermid */

#ifdef HAVE_ttyname
char *
chkr$ttyname (int fd)
{
  char * res;
  
  fd_used_by_prog (fd);
  res = ttyname (fd);
  if (res)
    {
      int len = strlen (res);
      stubs_chkr_set_right (res, len + 1, CHKR_RW);
    }
  return res;
}
#endif /* HAVE_ttyname */

#ifdef HAVE_isatty
int
chkr$isatty (int fd)
{
  fd_used_by_prog (fd);
#if USE_BI_JUMP
  __builtin_jump (isatty);
#else
  return isatty (fd);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_isatty */

#ifdef HAVE_link
int
chkr$link (const char *oldname, const char *newname)
{
  stubs_chkr_check_str (oldname, CHKR_RO, "oldname");
  stubs_chkr_check_str (newname, CHKR_RO, "newname");
#if USE_BI_JUMP
  __builtin_jump (link);
#else
  return link (oldname, newname);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_link */

#ifdef HAVE_symlink
int
chkr$symlink (const char *oldname, const char *newname)
{
  stubs_chkr_check_str (oldname, CHKR_RO, "oldname");
  stubs_chkr_check_str (newname, CHKR_RO, "newname");
#if USE_BI_JUMP
  __builtin_jump (symlink);
#else
  return symlink (oldname, newname);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_symlink */

#ifdef HAVE_readlink
int
chkr$readlink (const char *path, char *buf, size_t len)
{
  size_t res;
  /* This function requires a stub */
  stubs_chkr_check_str (path, CHKR_RO, "path");
  stubs_chkr_check_addr (buf, len, CHKR_MW, "buf");
  res = readlink (path, buf, len);
  if (res != -1)
    stubs_chkr_set_right (buf, res, CHKR_RW);
  return res;
}
#endif /* HAVE_readlink */

#ifdef HAVE_unlink
int
chkr$unlink (const char *path)
{
  stubs_chkr_check_str (path, CHKR_RO, "path");
#if USE_BI_JUMP
  __builtin_jump (unlink);
#else
  return unlink (path);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_unlink */

#ifdef HAVE_rmdir
int
chkr$rmdir (const char *path)
{
  stubs_chkr_check_str (path, CHKR_RO, "path");
#if USE_BI_JUMP
  __builtin_jump (rmdir);
#else
  return rmdir (path);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_rmdir */

#ifdef HAVE_tcgetpgrp
pid_t
chkr$tcgetpgrp (int fd)
{
  fd_used_by_prog (fd);
#if USE_BI_JUMP
  __builtin_jump (tcgetpgrp);
#else
  return tcgetpgrp (fd);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_tcgetpgrp */

#ifdef HAVE_tcsetpgrp
int
chkr$tcsetpgrp (int fd, pid_t pid)
{
  fd_used_by_prog (fd);
#if USE_BI_JUMP
  __builtin_jump (tcsetpgrp);
#else
  return tcsetpgrp (fd, pid);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_tcsetpgrp */

#ifdef HAVE_getlogin
char *
chkr$getlogin (void)
{
  char * res;
  res = getlogin ();
  if (res)
    {
      int len;
      len = strlen (res);
      stubs_chkr_set_right (res, len + 1, CHKR_RW);
    }
  return res;
}
#endif /* HAVE_getlogin */

#ifdef HAVE_setlogin
int
chkr$setlogin (const char * arg0)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (setlogin);
#else
  {
    int res;
    res = setlogin (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_setlogin */

#ifdef HAVE_gethostname
int
chkr$gethostname (char *buf, size_t len)
{
  int res;
  
  stubs_chkr_check_addr (buf, len, CHKR_MW, "buf");
  res = gethostname (buf, len);
  if (res != -1)
    stubs_chkr_set_right (buf, strlen (buf) + 1, CHKR_RW);
  return res;
}
#endif /* HAVE_gethostname */

#ifdef HAVE_sethostname
int
chkr$sethostname (const char * arg0, size_t arg1)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (sethostname);
#else
  {
    int res;
    res = sethostname (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_sethostname */

#ifdef HAVE_gethostid
long int
chkr$gethostid (void)
{
#if USE_BI_JUMP
  __builtin_jump (gethostid);
#else
  return gethostid ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_gethostid */

#ifdef HAVE_sethostid
int
chkr$sethostid (long int hid)
{
#if USE_BI_JUMP
  __builtin_jump (sethostid);
#else
  return sethostid (hid);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_sethostid */

#ifdef HAVE_getpagesize
size_t
chkr$getpagesize (void)
{
#if USE_BI_JUMP
  __builtin_jump (getpagesize);
#else
  return getpagesize ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_getpagesize */

#ifdef HAVE_getdtablesize
int
chkr$getdtablesize (void)
{
#if USE_BI_JUMP
  __builtin_jump (getdtablesize);
#else
  return getdtablesize ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_getdtablesize */

#ifdef HAVE_fsync
int
chkr$fsync (int fd)
{
  fd_used_by_prog (fd);
#if USE_BI_JUMP
  __builtin_jump (fsync);
#else
  return fsync (fd);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_fsync */

#ifdef HAVE_sync
int
chkr$sync (void)
{
#if USE_BI_JUMP
  __builtin_jump (sync);
#else
  return sync ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_sync */

#ifdef HAVE_vhangup
int
chkr$vhangup (void)
{
#if USE_BI_JUMP
  __builtin_jump (vhangup);
#else
  return vhangup ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_vhangup */

#ifdef HAVE_acct
int
chkr$acct (const char *path)
{
  stubs_chkr_check_str (path, CHKR_RO, "path");
#if USE_BI_JUMP
  __builtin_jump (acct);
#else
  return acct (path);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_acct */

#ifdef HAVE_chroot
int
chkr$chroot (const char * arg0)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (chroot);
#else
  {
    int res;
    res = chroot (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_chroot */

#ifdef HAVE_swapon
int
chkr$swapon (const char * arg0)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (swapon);
#else
  {
    int res;
    res = swapon (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_swapon */

#ifdef HAVE_getusershell
char *
chkr$getusershell (void)
{
#if USE_BI_JUMP
  __builtin_jump (getusershell);
#else
  {
    char * res;
    res = getusershell ();
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_getusershell */

#ifdef HAVE_endusershell
void
chkr$endusershell (void )
{
#if USE_BI_JUMP
  __builtin_jump (endusershell);
#else
  endusershell ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_endusershell */

#ifdef HAVE_setusershell
void
chkr$setusershell (void )
{
#if USE_BI_JUMP
  __builtin_jump (setusershell);
#else
  setusershell ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_setusershell */

#ifdef HAVE_getpass
char *
chkr$getpass (const char * arg0)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (getpass);
#else
  {
    char * res;
    res = getpass (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_getpass */

#ifdef HAVE_brk
int
chkr$brk (void * arg0)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (void), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (brk);
#else
  {
    int res;
    res = brk (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_brk */

#ifdef HAVE_sbrk
void *
chkr$sbrk (ptrdiff_t arg0)
{
#if USE_BI_JUMP
  __builtin_jump (sbrk);
#else
  {
    void * res;
    res = sbrk (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_sbrk */

#ifdef HAVE_crypt
char *
chkr$crypt (const char * arg0, const char * arg1)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX);
  stubs_chkr_check_addr (arg1, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (crypt);
#else
  {
    char * res;
    res = crypt (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_crypt */

#ifdef HAVE_encrypt
void
chkr$encrypt (char * arg0, int arg1)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (encrypt);
#else
  encrypt (arg0, arg1);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_encrypt */

#ifdef HAVE_setkey
void
chkr$setkey (const char * arg0)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (setkey);
#else
  setkey (arg0);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_setkey */

#ifdef HAVE_ftruncate
int
chkr$ftruncate (int fd, size_t size)
{
  fd_used_by_prog (fd);
#if USE_BI_JUMP
  __builtin_jump (ftruncate);
#else
  return ftruncate (fd, size);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_ftruncate */

#ifdef HAVE_truncate
int
chkr$truncate (const char *path, size_t size)
{
  stubs_chkr_check_str (path, CHKR_RO, "path");
#if USE_BI_JUMP
  __builtin_jump (truncate);
#else
  return truncate (path, size);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_truncate */

#ifdef HAVE_ioperm
int
chkr$ioperm (long unsigned int arg0, long unsigned int arg1, int arg2)
{
#if USE_BI_JUMP
  __builtin_jump (ioperm);
#else
  {
    int res;
    res = ioperm (arg0, arg1, arg2);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_ioperm */

#ifdef HAVE_iopl
int
chkr$iopl (int arg0)
{
#if USE_BI_JUMP
  __builtin_jump (iopl);
#else
  {
    int res;
    res = iopl (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_iopl */

#ifdef HAVE_mktemp
char *
chkr$mktemp (char *file)
{
  stubs_chkr_check_str (file, CHKR_RW, "file");
#if USE_BI_JUMP
  __builtin_jump (mktemp);
#else
  return mktemp (file);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_mktemp */

#ifdef HAVE_mkstemp
int
chkr$mkstemp (char * arg0)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (mkstemp);
#else
  {
    int res;
    res = mkstemp (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_mkstemp */

#ifdef HAVE_nice
int
chkr$nice (int prio)
{
#if USE_BI_JUMP
  __builtin_jump (nice);
#else
  return nice (prio);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_nice */

#ifdef HAVE_profil
int
chkr$profil (char * arg0, int arg1, int arg2, int arg3)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (profil);
#else
  {
    int res;
    res = profil (arg0, arg1, arg2, arg3);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_profil */

#ifdef HAVE_usleep
void
chkr$usleep (long unsigned int usec)
{
#if USE_BI_JUMP
  __builtin_jump (usleep);
#else
  usleep (usec);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_usleep */

#ifdef HAVE__xustat
int
chkr$_xustat (int arg0, dev_t arg1, struct ustat * arg2)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg2, sizeof (struct ustat), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (_xustat);
#else
  {
    int res;
    res = _xustat (arg0, arg1, arg2);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE__xustat */

#ifdef HAVE_idle
int
chkr$idle (void)
{
#if USE_BI_JUMP
  __builtin_jump (idle);
#else
  {
    int res;
    res = idle ();
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_idle */

#ifdef HAVE_reboot
int
chkr$reboot (int arg0, int arg1, int arg2)
{
#if USE_BI_JUMP
  __builtin_jump (reboot);
#else
  {
    int res;
    res = reboot (arg0, arg1, arg2);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_reboot */

#ifdef HAVE_swapoff
int
chkr$swapoff (const char * arg0)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (swapoff);
#else
  {
    int res;
    res = swapoff (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_swapoff */

#ifdef HAVE_uselib
int
chkr$uselib (const char * arg0)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (uselib);
#else
  {
    int res;
    res = uselib (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_uselib */

#ifdef HAVE_getdomainname
int
chkr$getdomainname (char * arg0, size_t arg1)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (getdomainname);
#else
  {
    int res;
    res = getdomainname (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_getdomainname */

#ifdef HAVE_setdomainname
int
chkr$setdomainname (const char * arg0, size_t arg1)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (setdomainname);
#else
  {
    int res;
    res = setdomainname (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_setdomainname */

#ifdef HAVE_realpath
char *
chkr$realpath (const char * arg0, char * arg1)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX);
  stubs_chkr_check_addr (arg1, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (realpath);
#else
  {
    char * res;
    res = realpath (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_realpath */

#ifdef HAVE_lockf
int
chkr$lockf (int fd, int func, off_t size)
{
  fd_used_by_prog (fd);
#if USE_BI_JUMP
  __builtin_jump (lockf);
#else
  return lockf (fd, func, size);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_lockf */

#endif /* HAVE_UNISTD_H */
