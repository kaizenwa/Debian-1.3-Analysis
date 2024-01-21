/* Generic syscall for Checker.
   Copyright 1993, 1994, 1995 Tristan Gingold
		  Written August 1993 by Tristan Gingold

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

#include <errno.h>
#include <stddef.h>
#include <unistd.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/time.h>
#include <sys/mman.h>
#include <sys/stat.h>

#define PTR void *

static int saved_errno;
extern int chkr_errno;

int
chkr_read (int fd, PTR buf, size_t n)
{
  int res;
  saved_errno = errno;
  res = read (fd, buf, n);
  chkr_errno = errno;
  errno = saved_errno;
  return res;
}

int
chkr_write (int fd, const PTR buf, size_t n)
{
  int res;
  saved_errno = errno;
  res = write (fd, buf, n);
  chkr_errno = errno;
  errno = saved_errno;
  return res;
}

int
chkr_close (int fd)
{
  int res;
  saved_errno = errno;
  res = close (fd);
  chkr_errno = errno;
  errno = saved_errno;
  return res;
}

long
chkr_lseek (int fd, long offset, int whence)
{
  int res;
  saved_errno = errno;
  res = lseek (fd, offset, whence);
  chkr_errno = errno;
  errno = saved_errno;
  return res;
}

int
chkr_unlink (const char *name)
{
  int res;
  saved_errno = errno;
  res = unlink (name);
  chkr_errno = errno;
  errno = saved_errno;
  return res;
}

pid_t
chkr_getpid (void)
{
  return getpid ();
}

int
chkr_kill (pid_t pid, int sig)
{
  int res;
  saved_errno = errno;
  res = kill (pid, sig);
  chkr_errno = errno;
  errno = saved_errno;
  return res;
}

int
chkr_open (const char *filename, int flags, int mask)
{
  int res;
  saved_errno = errno;
  res = open (filename, flags, mask);
  chkr_errno = errno;
  errno = saved_errno;
  return res;
}

int
chkr_fcntl (int filedes, int cmd, void *arg)
{
  int res;
  saved_errno = errno;
  res = fcntl (filedes, cmd, arg);
  chkr_errno = errno;
  errno = saved_errno;
  return res;
}

int
chkr_access (const char *name, int type)
{
  int res;
  saved_errno = errno;
  res = access (name, type);
  chkr_errno = errno;
  errno = saved_errno;
  return res;
}

int
chkr_stat (const char *filename, struct stat *stat_buf)
{
  int res;
  saved_errno = errno;
  res = stat (filename, stat_buf);
  chkr_errno = errno;
  errno = saved_errno;
  return res;
}

int
chkr_lstat (const char *filename, struct stat *stat_buf)
{
  int res;
  saved_errno = errno;
  res = lstat (filename, stat_buf);
  chkr_errno = errno;
  errno = saved_errno;
  return res;
}

int
chkr_fstat (int fd, struct stat *stat_buf)
{
  int res;
  saved_errno = errno;
  res = fstat (fd, stat_buf);
  chkr_errno = errno;
  errno = saved_errno;
  return res;
}

int
chkr_sigaction (int sig, struct sigaction* oact, struct sigaction* nact)
{
  int res;
  saved_errno = errno;
  res = sigaction (sig, oact, nact);
  chkr_errno = errno;
  errno = saved_errno;
  return res;
}


void
chkr__exit (int status)
{
  _exit (status);
}

void*
chkr_sbrk (int increment)
{
  void * res;
  saved_errno = errno;
  res = sbrk (increment);
  chkr_errno = errno;
  errno = saved_errno;
  return res;
}

int
chkr_link (const char *filename, const char *to)
{
  int res;
  saved_errno = errno;
  res = link (filename, to);
  chkr_errno = errno;
  errno = saved_errno;
  return res;
}

int
chkr_dup2 (int fd1, int fd2)
{
  int res;
  saved_errno = errno;
  res = dup2 (fd1, fd2);
  chkr_errno = errno;
  errno = saved_errno;
  return res;
}

int
chkr_sigprocmask (int how, sigset_t *set, sigset_t *oldset)
{
  int res;
  saved_errno = errno;
  res = sigprocmask (how, set, oldset);
  chkr_errno = errno;
  errno = saved_errno;
  return res;
}

int
chkr_sigpending (sigset_t *set)
{
  int res;
  saved_errno = errno;
  res = sigpending (set);
  chkr_errno = errno;
  errno = saved_errno;
  return res;
}

int
chkr_ftruncate(int fd, off_t length)
{
  int res;
  saved_errno = errno;
  res = ftruncate (fd, length);
  chkr_errno = errno;
  errno = saved_errno;
  return res;
}

int
chkr_gettimeofday (struct timeval *tp, struct timezone *tz)
{
  int res;
  saved_errno = errno;
  res = gettimeofday (tp, tz);
  chkr_errno = errno;
  errno = saved_errno;
  return res;
}

long
chkr_sysconf (int arg)
{
  long res;
  saved_errno = errno;
  res = sysconf (arg);
  chkr_errno = errno;
  errno = saved_errno;
  return res;
}

caddr_t
chkr_mmap (caddr_t addr, size_t len, int prot, int flags, int fd, off_t off)
{
  caddr_t res;
  saved_errno = errno;
  res = mmap (addr, len, prot, flags, fd, off);
  chkr_errno = errno;
  errno = saved_errno;
  return res;
}

int
chkr_munmap (caddr_t addr, size_t len)
{
  int res;
  saved_errno = errno;
  res = munmap (addr, len);
  chkr_errno = errno;
  errno = saved_errno;
  return res;
}

void
chkr_init_sbrk (void)
{
}
