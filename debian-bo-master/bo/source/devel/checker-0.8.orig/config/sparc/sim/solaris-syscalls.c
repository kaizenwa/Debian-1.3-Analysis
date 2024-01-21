/* function called to check syscalls.
   Copyright 1995 Tristan Gingold
   From the linux-syscalls.c which was
		  Written September 1993 Tristan Gingold

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
#define NEED_MM
#include "checker.h"
#include "errlist.h"
#include "message.h"
#include "instr.h"
#include <stddef.h>
#include <unistd.h>
#include <utime.h>
#include <fcntl.h>
#include <grp.h>
#include <stdarg.h>
#include <termios.h>
#include <limits.h>
#include <sys/syscall.h>
#include <sys/stat.h>
#include <sys/vfs.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/signal.h>
#include <sys/times.h>
#include <sys/utsname.h>
#include <sys/mount.h>
#include <sys/mman.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/statvfs.h>
#include <netinet/in.h>
#include <sys/shm.h>
#include <sys/sem.h>
#include <sys/msg.h>
#include <sys/stropts.h>
#include <sys/ttold.h>
#include <sys/kstat.h>
#include <sys/acl.h>
#include <sys/filio.h>

extern void make_syscall (void);
extern void make_pid(void);
extern void skip_syscall (void);
extern void update_output_file(void);
extern void remove_mmap(PTR addr, uint len);
extern void new_segmmap(PTR addr, uint len, int prot, int flags, int filedes, uint off);
void seg_mprotect (PTR addr, uint len, int prot);
extern void new_segshm(int shmid, PTR addr, int flags);
extern void remove_shm(PTR addr);
void check_syscall (uint *regs);
int check_ioctl (int cmd, int arg, int *const res);
int check_ipc (ulong call, ulong first, ulong second, ulong third, void *ptr, int *res);
int check_socketcall (int call, unsigned long *args, unsigned int *const res);
int check_fcntl (int fd, int cmd, int arg, int *const res);
void sim_setcontext (ucontext_t *ucp);
void sim_getcontext (ucontext_t *ucp);
static int check_acl (int is_facl, uint *regs);
extern int do_not_update_pc_after_trap;

#define RES  (regs[O0])
#define CALLNBR (regs[G1])
#define ARG1 (regs[O0])
#define ARG2 (regs[O1])
#define ARG3 (regs[O2])
#define ARG4 (regs[O3])
#define ARG5 (regs[O4])
#define ARG6 (regs[O5])

#define RESPTR (&(regs[O0]))
#define PTR1 (PTR)ARG1
#define PTR2 (PTR)ARG2
#define PTR3 (PTR)ARG3
#define PTR4 (PTR)ARG4
#define PTR5 (PTR)ARG5
#define PTR6 (PTR)ARG6

/* This function is called by a stub to check a syscall.  All the registers 
 *  available through REGS and can be modified. */
void
check_syscall (uint *regs)
{
  int error = 0;
  switch (CALLNBR)
    {
    case SYS_exit:
      chkr_do_end ();
      break;
    case SYS_fork:
      make_syscall ();
      if (RES == 0)
        chkr_clean_after_fork ();
      break;
    case SYS_read:
      {
	char *ptr = PTR2;
	fd_used_by_prog (ARG1);
	if (ARG3 > 0)
	  chkr_check_addr (PTR2, ARG3, CHKR_TW);
	make_syscall ();
	if (RES > 0)
	  chkr_set_right (ptr, RES, CHKR_RW);
      }
      break;
    case SYS_write:
      fd_used_by_prog (ARG1);
      if (ARG3 > 0)
        chkr_check_addr (PTR2, ARG3, CHKR_RO);
      break;
    case SYS_open:
      chkr_check_str (PTR1, CHKR_RO);
      make_syscall ();
      if (RES != -1)
	fd_returned_by_system (RES);
      break;
    case SYS_close:
      if (!fd_used_by_prog (ARG1))
	{
	  skip_syscall ();
	  psr.c = 1;
	  RES = EBADF;
	}
      else
        fd_closed(ARG1);
      break;
    case SYS_wait:
      {
	PTR ptr = PTR1;
	chkr_check_addr(PTR1, sizeof (int), CHKR_TW);
	make_syscall();
	if (!psr.c)
	  chkr_set_right (ptr, sizeof (int), CHKR_RW);
      }
      break;
    case SYS_creat:
      chkr_check_str (PTR1, CHKR_RO);
      break;
    case SYS_link:
      chkr_check_str (PTR1, CHKR_RO);
      chkr_check_str (PTR2, CHKR_RO);
      break;
    case SYS_unlink:
      chkr_check_str (PTR1, CHKR_RO);
      break;
    case SYS_exec:	/* exec != execve ???? */
      error = 1;
      break;
    case SYS_chdir:
      chkr_check_str (PTR1, CHKR_RO);
      break;
    case SYS_time:
      if (ARG1)
	chkr_check_addr (PTR1, sizeof (time_t), CHKR_WO);
      break;
    case SYS_mknod:
      chkr_check_str (PTR1, CHKR_RO);
      break;
    case SYS_chmod:
      chkr_check_str (PTR1, CHKR_RO);
      break;
    case SYS_chown:
      chkr_check_str (PTR1, CHKR_RO);
      break;
    case SYS_brk:
      error = 1;
      break;
    case SYS_stat:
      chkr_check_str (PTR1, CHKR_RO);
      chkr_check_addr (PTR2, sizeof (struct stat), CHKR_WO);
      break;
    case SYS_lseek:
      fd_used_by_prog (ARG1);
      break;
    case SYS_getpid:
      break;
    case SYS_mount:
      chkr_check_str (PTR1, CHKR_RO);
      chkr_check_str (PTR2, CHKR_RO);
      chkr_check_str (PTR3, CHKR_RO);
      /* FIXME: options */
      break;
    case SYS_umount:
      chkr_check_str (PTR1, CHKR_RO);
      break;
    case SYS_setuid:
      break;
    case SYS_getuid:
      break;
    case SYS_stime:
      chkr_check_addr (PTR1, sizeof (time_t), CHKR_RO);
      break;
    case SYS_ptrace:
      /* FIXME: to do */
      error = 1;
      break;
    case SYS_alarm:
      break;
    case SYS_fstat:
      fd_used_by_prog (ARG1);
      chkr_check_addr (PTR2, sizeof (struct stat), CHKR_WO);
      break;
    case SYS_pause:
      break;
    case SYS_utime:
      if (PTR2)
        chkr_check_addr (PTR2, sizeof (struct utimbuf), CHKR_RW);
      chkr_check_str (PTR1, CHKR_RO);
      break;
    case SYS_stty:
      error = 1;
      break;			/* replaced by ioctl */
    case SYS_gtty:
      error = 1;
      break;			/* replaced by ioctl */
    case SYS_access:
      chkr_check_str (PTR1, CHKR_RO);
      break;
    case SYS_nice:
      error = 1;
      break;			/* replaced  by setpriority */
    case SYS_sync:
      break;
    case SYS_kill:
      break;
    case SYS_fstatfs:
      fd_used_by_prog (ARG1);
      chkr_check_addr (PTR2, sizeof (struct statvfs), CHKR_WO);
      break;
    case SYS_pgrpsys:
      break;
    case SYS_xenix:
      error = 1;
      break;
    case SYS_dup:
      {
	int fd = ARG1;
	fd_used_by_prog (ARG1);
	make_syscall ();
	fd_duped(RES, fd);
      }
      break;
    case SYS_pipe:
      chkr_check_addr (PTR1, 2 * sizeof (int), CHKR_WO);
      make_syscall ();
      if (RES == 0)
	{
	  fd_returned_by_system (((int *) PTR1)[0]);
	  fd_returned_by_system (((int *) PTR1)[1]);
	}
      break;
    case SYS_times:
      chkr_check_addr (PTR1, sizeof (struct tms), CHKR_WO);
      break;
    case SYS_profil:
      chkr_check_addr (PTR1, ARG1, CHKR_WO);
      break;
    case SYS_plock:
      error = 1;
      break;
    case SYS_setgid:
      break;
    case SYS_getgid:
      break;
    case SYS_signal:
      error = 1;
      break;
    case SYS_msgsys:
      error = 1;
      break;
    case SYS_syssun:
      error = 1;
      break;
    case SYS_acct:
      chkr_check_str (PTR1, CHKR_RO);
      break;
    case SYS_shmsys:
      error = 1;
      break;
    case SYS_semsys:
      error = 1;
      break;
    case SYS_ioctl:
      fd_used_by_prog (ARG1);
      error = check_ioctl (ARG2, ARG3, RESPTR);
      break;
    case SYS_uadmin:
      error = 1;
      break;
    case SYS_utssys:
      error = 1;
      break;
    case SYS_fdsync:
      error = 1;
      break;
    case SYS_execve:
      {
	char **ptr;

	/* check path */
	chkr_check_str (PTR1, CHKR_RO);

	/* check arguments */
	chkr_check_addr (PTR2, sizeof (char *), CHKR_RO);
	ptr = (char **) ARG2;
	while (*ptr != (char *) 0)
	  {
	    chkr_check_addr (ptr, sizeof (char *), CHKR_RO);
	    chkr_check_str (*ptr, CHKR_RO);
	    ptr++;
	  }
	chkr_check_addr (ptr, sizeof (char *), CHKR_RO);

	/* check environnement */
	chkr_check_addr (PTR3, sizeof (char *), CHKR_RO);
	ptr = (char **) ARG3;
	while (*ptr != (char *) 0)
	  {
	    chkr_check_addr (ptr, sizeof (char *), CHKR_RO);
	    chkr_check_str (*ptr, CHKR_RO);
	    ptr++;
	  }
	chkr_check_addr (ptr, sizeof (char *), CHKR_RO);
      }
      chkr_clean_before_exec ();
      break;
    case SYS_umask:
      break;
    case SYS_chroot:
      chkr_check_str (PTR1, CHKR_RO);
      break;
    case SYS_fcntl:
      fd_used_by_prog (ARG1);
      error = check_fcntl (ARG1, ARG2, ARG3, RESPTR);
      break;
    case SYS_ulimit:
      break;
    case SYS_rmdir:
      chkr_check_str (PTR1, CHKR_RO);
      break;
    case SYS_mkdir:
      chkr_check_str (PTR1, CHKR_RO);
      break;
    case SYS_getdents:
      {
	PTR ptr = PTR2;
	fd_used_by_prog(ARG1);
	if (ARG3 > 0)
	  chkr_check_addr(PTR2, ARG3, CHKR_TW);
	make_syscall ();
	if (RES > 0)
	  chkr_set_right(ptr, RES, CHKR_RW);
      }
      break;
    case SYS_sysfs:
      error = 1;
      break;
    case SYS_getmsg:
    case SYS_getpmsg:
      fd_used_by_prog (ARG1);
      if (PTR2)
        {
          chkr_check_addr (((struct strbuf*) PTR2) + offsetof (struct strbuf, maxlen), sizeof (int), CHKR_RO);
          chkr_check_addr (((struct strbuf*) PTR2) + offsetof (struct strbuf, len), sizeof (int), CHKR_WO);
          chkr_check_addr (((struct strbuf*) PTR2) + offsetof (struct strbuf, buf), sizeof (PTR), CHKR_RO);
          if (((struct strbuf*) PTR2)->maxlen > 0)
            chkr_check_addr (((struct strbuf*) PTR2)->buf, ((struct strbuf*) PTR2)->maxlen, CHKR_TW);
        }
      if (PTR3)
        {
          chkr_check_addr (((struct strbuf*) PTR3) + offsetof (struct strbuf, maxlen), sizeof (int), CHKR_RO);
          chkr_check_addr (((struct strbuf*) PTR3) + offsetof (struct strbuf, len), sizeof (int), CHKR_WO);
          chkr_check_addr (((struct strbuf*) PTR3) + offsetof (struct strbuf, buf), sizeof (PTR), CHKR_RO);
          if (((struct strbuf*) PTR3)->maxlen > 0)
            chkr_check_addr (((struct strbuf*) PTR3)->buf, ((struct strbuf*) PTR3)->maxlen, CHKR_TW);
        }
      make_syscall ();
      if (!psr.c && PTR2)
        {
          if (((struct strbuf*) PTR2)->len > 0)
            chkr_set_right (((struct strbuf*) PTR2)->buf, ((struct strbuf*) PTR2)->len, CHKR_RW);
        }
      if (!psr.c && PTR3)
        {
          if (((struct strbuf*) PTR3)->len > 0)
            chkr_set_right (((struct strbuf*) PTR3)->buf, ((struct strbuf*) PTR3)->len, CHKR_RW);
        }
      break;
    case SYS_putmsg:
    case SYS_putpmsg:
      fd_used_by_prog (ARG1);
      if (PTR2)
        {
          chkr_check_addr (((struct strbuf*) PTR2) + offsetof (struct strbuf, maxlen), sizeof (int), CHKR_RO);
          chkr_check_addr (((struct strbuf*) PTR2) + offsetof (struct strbuf, len), sizeof (int), CHKR_RO);
          chkr_check_addr (((struct strbuf*) PTR2) + offsetof (struct strbuf, buf), sizeof (PTR), CHKR_RO);
          if (((struct strbuf*) PTR2)->len > 0)
            chkr_check_addr (((struct strbuf*) PTR2)->buf, ((struct strbuf*) PTR2)->len, CHKR_RO);
        }
      if (PTR3)
        {
          chkr_check_addr (((struct strbuf*) PTR3) + offsetof (struct strbuf, maxlen), sizeof (int), CHKR_RO);
          chkr_check_addr (((struct strbuf*) PTR3) + offsetof (struct strbuf, len), sizeof (int), CHKR_RO);
          chkr_check_addr (((struct strbuf*) PTR3) + offsetof (struct strbuf, buf), sizeof (PTR), CHKR_RO);
          if (((struct strbuf*) PTR3)->len > 0)
            chkr_check_addr (((struct strbuf*) PTR3)->buf, ((struct strbuf*) PTR3)->len, CHKR_RO);
        }
      break;
    case SYS_poll:
      {
        int i;
        struct pollfd * pfd = (struct pollfd*) PTR1;
        for (i = 0; i < ARG2; i++)
          {
            chkr_check_addr (&pfd[i].fd, sizeof (int), CHKR_RO);
            fd_used_by_prog (pfd[i].fd);
            chkr_check_addr (&pfd[i].events, sizeof (short), CHKR_RO);
            chkr_check_addr (&pfd[i].revents, sizeof (short), CHKR_WO);
          }
      }
      break;
    case SYS_lstat:
      chkr_check_str (PTR1, CHKR_RO);
      chkr_check_addr (PTR2, sizeof (struct stat), CHKR_WO);
      break;
    case SYS_symlink:
      chkr_check_str (PTR1, CHKR_RO);
      chkr_check_str (PTR2, CHKR_RO);
      break;
    case SYS_readlink:
      {
	PTR ptr = PTR2;
	chkr_check_str (PTR1, CHKR_RO);
	if (ARG3)
	  chkr_check_addr (PTR2, ARG3, CHKR_TW);
	make_syscall ();
	if (RES > 0)
	  chkr_set_right (ptr, RES, CHKR_RW);
      }
      break;
    case SYS_setgroups:
      if (ARG1)
        chkr_check_addr (PTR2, ARG1 * sizeof (gid_t), CHKR_RO);
      break;
    case SYS_getgroups:
      {
	PTR ptr = PTR2;
	if (ARG1)
	  chkr_check_addr (PTR2, ARG1 * sizeof (gid_t), CHKR_TW);
	make_syscall ();
	if (RES > 0)
	  chkr_set_right (ptr, RES * sizeof (gid_t), CHKR_RW);
      }
      break;
    case SYS_fchmod:
      fd_used_by_prog (ARG1);
      break;
    case SYS_fchown:
      fd_used_by_prog (ARG1);
      break;
    case SYS_sigprocmask:
      if (ARG2)
	chkr_check_addr (PTR2, sizeof (sigset_t), CHKR_RO);
      if (ARG3)
	chkr_check_addr (PTR3, sizeof (sigset_t), CHKR_WO);
      break;
    case SYS_sigsuspend:
      chkr_check_addr (PTR1, sizeof (sigset_t), CHKR_RO);
      break;
    case SYS_sigaltstack:
      error = 1;
      break;
    case SYS_sigaction:
      skip_syscall();
      /* ACT is optional and is a pointer.  */
      if (PTR2)
        chkr_check_addr (PTR2, sizeof (struct sigaction), CHKR_RO);
      /* OLDACT is optional... */
      if (PTR3)
        chkr_check_addr (PTR3, sizeof (struct sigaction), CHKR_WO);
      RES = user_sigaction (ARG1, PTR2, PTR3);
      if (RES)
        psr.c = 1;
      else
        psr.c = 0;
      break;
    case SYS_sigpending:
      chkr_check_addr(PTR2, sizeof(sigset_t), CHKR_WO);
      break;
    case SYS_context:
      skip_syscall ();
      if (ARG1 == 0)
        {
          chkr_check_addr (PTR2, sizeof (ucontext_t), CHKR_RO);
          sim_getcontext ((ucontext_t*)ARG2);
        }
      else if (ARG1 == 1)
        {
          chkr_check_addr (PTR2, sizeof (ucontext_t), CHKR_WO);
          sim_setcontext ((ucontext_t*)ARG2);
          do_not_update_pc_after_trap = 1;
        }
      else error = 1;
      break;
    case SYS_evsys:
      error = 1;
      break;
    case SYS_evtrapret:
      error = 1;
      break;
    case SYS_statvfs:
      chkr_check_str (PTR1, CHKR_RO);
      chkr_check_addr (PTR2, sizeof (struct statvfs), CHKR_WO);
      break;
    case SYS_fstatvfs:
      fd_used_by_prog(ARG1);
      chkr_check_addr (PTR2, sizeof (struct statvfs), CHKR_WO);
      break;
    case SYS_nfssys:
      error = 1;
      break;
    case SYS_waitsys:
      chkr_check_addr (PTR3, sizeof (siginfo_t), CHKR_TW);
      make_syscall ();
      if (!psr.c)
        chkr_set_right (PTR3, sizeof (siginfo_t), CHKR_RW);
      break;
    case SYS_sigsendsys:
      error = 1;
      break;
    case SYS_hrtsys:
      error = 1;
      break;
    case SYS_acancel:
      error = 1;
      break;
    case SYS_async:
      error = 1;
      break;
    case SYS_priocntlsys:
      error = 1;
      break;
    case SYS_pathconf:
      error = 1;
      break;
    case SYS_mincore:
      error = 1;
      break;
    case SYS_mmap:	/* addr, size, prot, fl, fd, off */
      {
	daddr_t daddr = ARG1;
	int len = ARG2;
	int prot = ARG3;
	int flags = ARG4;
	int filedes = ARG5;
	off_t off = ARG6;

#ifdef MAP_ANONYMOUS
        if (!(flags & MAP_ANONYMOUS))
#endif
          fd_used_by_prog(filedes);
        if (daddr >= MM_LOW && daddr <= MM_HIGH)
          {
            if (flags & MAP_FIXED)
              {
                skip_syscall();	/* can't */
                psr.c = 1;
                RES = EINVAL;
                break;
              }
            ARG1 = 0;
          }
        make_syscall();
        if (psr.c)
          break;        /* fail */
        new_segmmap ((caddr_t) RES, len, prot, flags, filedes, off);
#if 0
        chkr_printf ("Mmap results: addr = 0x%08x, len = 0x%08x\n", RES, len);
#endif
      }
      break;
    case SYS_mprotect:
      {
        uint arg1 = ARG1;
        uint arg2 = ARG2;
        uint arg3 = ARG3;
        
        make_syscall ();
        if (!psr.c)
          seg_mprotect ((PTR)arg1, arg2, arg3);
      }
      break;
    case SYS_munmap:
      {
        uint arg2 = ARG2;
        PTR ptr1 = PTR1;

#if 0
        chkr_printf ("munmap: ptr = 0x%08x, MM_HIGH = 0x%08x, ptr + len = 0x%08x, MM_LOW = 0x%08x\n",
        		ARG1, MM_HIGH, ARG1 + ARG2, MM_LOW);
#endif
        if (ARG1 >= MM_HIGH || ARG1 + ARG2 <= MM_LOW)
          {
            make_syscall ();
            if (!psr.c)
              remove_mmap(ptr1, arg2);
          }
        else
          {
            skip_syscall();
            psr.c = 1;
            RES = EINVAL;
            break;
          }
      }
      break;
    case SYS_fpathconf:
      error = 1;
      break;
    case SYS_vfork:
      error = 1;
      break;
    case SYS_fchdir:
      fd_used_by_prog (ARG1);
      break;
    case SYS_readv:
      {
	struct iovec *iov = (struct iovec *) PTR2;
	int iocount = ARG3;
	int i;
	int len;

	fd_used_by_prog (ARG1);
	for (i = 0; i < iocount; i++)
	  {
	    chkr_check_addr (iov + i, sizeof (struct iovec), CHKR_RO);
	    if (iov[i].iov_len)
	      chkr_check_addr (iov[i].iov_base, iov[i].iov_len, CHKR_MW);
	  }
	if (ARG3 > 0)
	  chkr_check_addr (PTR2, ARG3, CHKR_TW);
	make_syscall ();
	if (RES > 0)
	  {
	    len = RES;
	    for (i = 0; i < iocount && len > 0; i++)
	      if (iov[i].iov_len <= len)
		{
		  chkr_set_right (iov[i].iov_base, iov[i].iov_len, CHKR_RW);
		  len -= iov[i].iov_len;
		}
	      else
		{
		  chkr_set_right (iov[i].iov_base, len, CHKR_RW);
		  len =0;
		}
	  }
      }
      break;
    case SYS_writev:
      {
	struct iovec *iov = PTR2;
	int i;
	int iovcount = ARG3;

	fd_used_by_prog (ARG1);
	for (i = 0; i < iovcount; i++)
	  {
	    chkr_check_addr (iov + i, sizeof (struct iovec), CHKR_RO);
	    chkr_check_addr (iov[i].iov_base, iov[i].iov_len, CHKR_RO);
	  }
      }
      break;
    case SYS_xstat:
      error = 1;
      break;
    case SYS_lxstat:
      error = 1;
      break;
    case SYS_fxstat:
      error = 1;
      break;
    case SYS_xmknod:
      chkr_check_str (PTR1, CHKR_RO);
      break;
    case SYS_clocal:
      error = 1;
      break;
    case SYS_setrlimit:
      chkr_check_addr (PTR2, sizeof (struct rlimit), CHKR_RO);
      break;
    case SYS_getrlimit:
      chkr_check_addr (PTR2, sizeof (struct rlimit), CHKR_WO);
      break;
    case SYS_lchown:
      chkr_check_str (PTR1, CHKR_RO);
      break;
    case SYS_memcntl:
      break;	/* Nothing to do */
/*    case SYS_getpmsg:
 *      See SYS_getmsg.
 */
/*    case SYS_putpmsg:
 *      See SYS_putmsg.
 */
    case SYS_rename:
      chkr_check_str (PTR1, CHKR_RO);
      chkr_check_str (PTR2, CHKR_RO);
      break;
    case SYS_uname:
      chkr_check_addr (PTR1, sizeof (struct utsname), CHKR_WO);
      break;
    case SYS_setegid:
      break;
    case SYS_sysconfig:
      break;
    case SYS_adjtime:
      error = 1;
      break;
    case SYS_systeminfo:
      /* FIXME: according to <sys/systeminfo.h>, if arg1 > 255, we set a
       * string.
       */
      if (ARG1 > 255)
        chkr_check_addr (PTR2, ARG3, CHKR_RO);
      else
        {
          PTR ptr = PTR2;
          chkr_check_addr (PTR2, ARG3, CHKR_TW);
          make_syscall ();
          chkr_set_right (ptr, RES, CHKR_RW);
        }
      break;
    case SYS_seteuid:
      break;
    case SYS_vtrace:
      error = 1;
      break;
    case SYS_fork1:
      error = 1;
      break;
    case SYS_sigtimedwait:
      error = 1;
      break;
    case SYS_lwp_info:
      error = 1;
      break;
    case SYS_yield:
      error = 1;
      break;
    case SYS_lwp_sema_wait:
      error = 1;
      break;
    case SYS_lwp_sema_post:
      error = 1;
      break;
    case SYS_modctl:
      error = 1;
      break;
    case SYS_fchroot:
      fd_used_by_prog (ARG1);
      break;
    case SYS_utimes:
      error = 1;
      break;
    case SYS_vhangup:
      break;
    case SYS_gettimeofday:
      if (ARG1)
        chkr_check_addr (PTR1, sizeof (struct timeval), CHKR_WO);
      if (ARG2)
        chkr_check_addr (PTR2, sizeof (struct timezone), CHKR_WO);
      break;
    case SYS_getitimer:
      chkr_check_addr (PTR2, sizeof (struct itimerval), CHKR_WO);
      break;
    case SYS_setitimer:
      chkr_check_addr (PTR2, sizeof (struct itimerval), CHKR_RO);
      if (ARG3)
	chkr_check_addr (PTR3, sizeof (struct itimerval), CHKR_WO);
      break;
    case SYS_lwp_create:
      error = 1;
      break;
    case SYS_lwp_exit:
      error = 1;
      break;
    case SYS_lwp_suspend:
      error = 1;
      break;
    case SYS_lwp_continue:
      error = 1;
      break;
    case SYS_lwp_kill:
      error = 1;
      break;
    case SYS_lwp_self:
      break;	/* Nothing to do. */
    case SYS_lwp_setprivate:
      error = 1;
      break;
    case SYS_lwp_getprivate:
      error = 1;
      break;
    case SYS_lwp_wait:
      error = 1;
      break;
    case SYS_lwp_mutex_unlock:
      error = 1;
      break;
    case SYS_lwp_mutex_lock:
      error = 1;
      break;
    case SYS_lwp_cond_wait:
      error = 1;
      break;
    case SYS_lwp_cond_signal:
      error = 1;
      break;
    case SYS_lwp_cond_broadcast:
      error = 1;
      break;
    case SYS_pread:
      error = 1;
      break;
    case SYS_pwrite:
      error = 1;
      break;
    case SYS_llseek:
      fd_used_by_prog(ARG1);
      chkr_check_addr(PTR4, sizeof(offset_t), CHKR_RO);
      break;
    case SYS_inst_sync:
      error = 1;
      break;
    case SYS_acl:
      check_acl (0, regs);
      break;
    case SYS_auditsys:
      error = 1;
      break;
    case SYS_processor_bind:
      error = 1;
      break;
    case SYS_processor_info:
      error = 1;
      break;
    case SYS_p_online:
      error = 1;
      break;
    case SYS_sigqueue:
      error = 1;
      break;
    case SYS_clock_gettime:
      error = 1;
      break;
    case SYS_clock_settime:
      error = 1;
      break;
    case SYS_clock_getres:
      error = 1;
      break;
    case SYS_timer_create:
      error = 1;
      break;
    case SYS_timer_delete:
      error = 1;
      break;
    case SYS_timer_settime:
      error = 1;
      break;
    case SYS_timer_gettime:
      error = 1;
      break;
    case SYS_timer_getoverrun:
      error = 1;
      break;
    case SYS_nanosleep:
      error = 1;
      break;
    case SYS_facl:
      check_acl (1, regs);
      break;
    case SYS_door:
      psr.c = 1;
      RES = EINVAL;
      break;      
    default:
      error = 1;
    }
  if (error)
    {
      chkr_perror (M_I_IES_SC_ET);
      chkr_printf ("Syscall# %04d\n", CALLNBR);
      chkr_abort ();
    }
}

/* The dates after the 'case' stmt are the last date when I (or somebody else)
 *  have checked the ioctl is correctly handled.
 * Mon Apr 4 1994: roughly tested, based on a fast look on the sources.
 * Sat Apr 16 1994: add ioctl tests based on a look on drivers/block/
 * Thu Apr 21 1994: add MTIOCTOP & MTIOCGET
 * Sat Apr 23 1994: add from vt.c
 * Sat Aug 13 1994: add SIO*
 * Sat Sep 17 1994: some SIO* added (tx to John Gardiner Myers <jgm+@CMU.EDU>)
 *
 * BTW, some ioctls (e.g. qic) are confused: same number than another iotcl.
 *  So, I comment them and indicate the conflict. Those with a ! are really
 *  incompatible...
 */
int
check_ioctl (int cmd, int arg, int *const res)
{
  switch (cmd)
    {
      /* tty_ioctl.c */
    case TCGETS:		/* Mon Apr 4 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct termios), CHKR_WO);
      break;
    case TCSETSW:		/* Mon Apr 4 1994 */
    case TCSETSF:		/* Mon Apr 4 1994 */
    case TCSETS:		/* Mon Apr 4 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct termios), CHKR_RO);
      break;
    case TCGETA:		/* Mon Apr 4 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct termios), CHKR_WO);
      break;
    case TCSETAW:		/* Mon Apr 4 1994 */
    case TCSETAF:		/* Mon Apr 4 1994 */
    case TCSETA:		/* Mon Apr 4 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct termios), CHKR_RO);
      break;
#if 0
    case TCXONC:		/* Mon Apr 4 1994 */
    case TCFLSH:		/* Mon Apr 4 1994 */
      chkr_check_addr ((PTR) arg, sizeof (int), CHKR_RO);
      break;
    case TIOCEXCL:		/* Mon Apr 4 1994 */
    case TIOCNXCL:		/* Mon Apr 4 1994 */
    case TIOCSCTTY:		/* Mon Apr 4 1994 */
      break;
#endif
    case TIOCGPGRP:		/* Mon Apr 4 1994 */
      chkr_check_addr ((PTR) arg, sizeof (pid_t), CHKR_WO);
      break;
    case TIOCSPGRP:		/* Mon Apr 4 1994 */
      chkr_check_addr ((PTR) arg, sizeof (pid_t), CHKR_RO);
      break;
#if 0
    case TIOCOUTQ:		/* Mon Apr 4 1994 */
      chkr_check_addr ((PTR) arg, sizeof (u_long), CHKR_WO);
      break;
#if TIOCINQ != FIONREAD
    case TIOCINQ:		/* Mon Apr 4 1994 */
      chkr_check_addr ((PTR) arg, sizeof (u_long), CHKR_WO);
      break;
#endif
    case TIOCSTI:		/* Mon Apr 4 1994 */
      chkr_check_addr ((PTR) arg, sizeof (char), CHKR_RO);
      break;
#endif
    case TIOCGWINSZ:		/* Mon Apr 4 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct winsize), CHKR_WO);
      break;
    case TIOCSWINSZ:		/* Mon Apr 4 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct winsize), CHKR_RO);
      break;
#if 0
/*	case TIOCLINUX: */
    case TIOCCONS:		/* Mon Apr 4 1994 */
      break;
    case TIOCNOTTY:		/* Mon Apr 4 1994 */
      break;
    case TIOCGETD:		/* Mon Apr 4 1994 */
      chkr_check_addr ((PTR) arg, sizeof (long), CHKR_WO);
      break;
    case TIOCSETD:		/* Mon Apr 4 1994 */
      chkr_check_addr ((PTR) arg, sizeof (long), CHKR_RO);
      break;
    case TIOCGLCKTRMIOS:	/* Mon Apr 4 1994 */
      chkr_check_addr ((PTR) arg, sizeof (PTR), CHKR_RO);
      chkr_check_addr (*((PTR *) arg), sizeof (struct termios), CHKR_WO);
      break;
    case TIOCSLCKTRMIOS:	/* Mon Apr 4 1994 */
      chkr_check_addr ((PTR) arg, sizeof (PTR), CHKR_RO);
      chkr_check_addr (*((PTR *) arg), sizeof (struct termios), CHKR_RO);
      break;
    case TIOCPKT:		/* Mon Apr 4 1994 */
      chkr_check_addr ((PTR) arg, sizeof (u_long), CHKR_RO);
      break;
      /* end of tty_ioctl.c */
    case TIOCSERCONFIG:	/* Mon Apr 4 1994 */
      break;
    case TIOCSERGWILD:		/* Mon Apr 4 1994 */
      chkr_check_addr ((PTR) arg, sizeof (long), CHKR_WO);
      break;
    case TIOCSERSWILD:		/* Mon Apr 4 1994 */
      chkr_check_addr ((PTR) arg, sizeof (long), CHKR_RO);
      break;
    case TIOCMGET:		/* Mon Apr 4 1994 */
      chkr_check_addr ((PTR) arg, sizeof (u_int), CHKR_WO);
      break;
    case TIOCMBIS:		/* Mon Apr 4 1994 */
    case TIOCMBIC:		/* Mon Apr 4 1994 */
    case TIOCMSET:		/* Mon Apr 4 1994 */
      chkr_check_addr ((PTR) arg, sizeof (long), CHKR_RO);
      break;
    case TIOCGSOFTCAR:		/* Mon Apr 4 1994 */
      chkr_check_addr ((PTR) arg, sizeof (long), CHKR_WO);
      break;
    case TIOCSSOFTCAR:		/* Mon Apr 4 1994 */
      chkr_check_addr ((PTR) arg, sizeof (long), CHKR_RO);
      break;
    case TIOCGSERIAL:		/* Mon Apr 4 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct serial_struct), CHKR_WO);
      break;
    case TIOCSSERIAL:		/* Mon Apr 4 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct serial_struct), CHKR_RO);
      break;
    case FIONCLEX:
    case FIOCLEX:
      break;			/* arg is not used */
    case TCSBRK:		/* Mon Apr 4 1994 */
    case TCSBRKP:		/* Mon Apr 4 1994 */
      chkr_check_addr ((PTR) arg, sizeof (long), CHKR_RO);
      break;
      /* vt */
    case KIOCSOUND:		/* Sat Apr 23 1994 */
    case KDMKTONE:		/* Sat Apr 23 1994 */
      break;
    case KDGKBTYPE:		/* Sat Apr 23 1994 */
      chkr_check_addr ((PTR) arg, sizeof (char), CHKR_WO);
      break;
    case KDADDIO:		/* Sat Apr 23 1994 */
    case KDDELIO:		/* Sat Apr 23 1994 */
    case KDENABIO:		/* Sat Apr 23 1994 */
    case KDDISABIO:		/* Sat Apr 23 1994 */
    case KDSETMODE:		/* Sat Apr 23 1994 */
      break;
    case KDGETMODE:		/* Sat Apr 23 1994 */
      chkr_check_addr ((PTR) arg, sizeof (unsigned long), CHKR_WO);
      break;
    case KDMAPDISP:		/* Sat Apr 23 1994 */
    case KDUNMAPDISP:		/* Sat Apr 23 1994 */
    case KDSKBMODE:		/* Sat Apr 23 1994 */
      break;
    case KDGKBMODE:		/* Sat Apr 23 1994 */
      chkr_check_addr ((PTR) arg, sizeof (unsigned long), CHKR_WO);
      break;
    case KDSKBMETA:		/* Sat Apr 23 1994 */
      break;
    case KDGKBMETA:		/* Sat Apr 23 1994 */
      chkr_check_addr ((PTR) arg, sizeof (unsigned long), CHKR_WO);
      break;
    case KDGKBENT:		/* Sat Apr 23 1994 */
      {
	struct kbentry *a = (struct kbentry *) arg;
	chkr_check_addr ((PTR) (&a->kb_index), sizeof (char), CHKR_RO);
	chkr_check_addr ((PTR) (&a->kb_table), sizeof (char), CHKR_RO);
	chkr_check_addr ((PTR) arg, sizeof (struct kbentry), CHKR_WO);
      }
      break;
    case KDSKBENT:		/* Sat Apr 23 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct kbentry), CHKR_RO);
      break;
    case KDGKBSENT:		/* Sat Apr 23 1994 */
      {
	struct kbsentry *a = (struct kbsentry *) arg;
	int len;
	chkr_check_addr ((PTR) (&a->kb_func), sizeof (char), CHKR_RO);
	chkr_check_addr ((PTR) (a->kb_string), sizeof (a->kb_string), CHKR_TW);
	make_syscall ();
	if (*res == 0)
	  {
	    len = strlen (a->kb_string);
	    chkr_set_right ((PTR) (a->kb_string), len + 1, CHKR_RW);
	  }
      }
      break;
    case KDSKBSENT:		/* Sat Apr 23 1994 */
      {
	struct kbsentry *a = (struct kbsentry *) arg;
	chkr_check_addr ((PTR) arg, sizeof (struct kbsentry), CHKR_RO);
	chkr_check_str ((PTR) (a->kb_string), CHKR_RO);
      }
      break;
    case KDGKBDIACR:		/* Sat Apr 23 1994 */
      {
	struct kbdiacrs *a = (struct kbdiacrs *) arg;
	chkr_check_addr ((PTR) arg, sizeof (struct kbdiacrs), CHKR_WO);
	make_syscall ();
	if (a->kb_cnt > 0)
	  chkr_set_right ((PTR) (a->kbdiacr), a->kb_cnt * sizeof (struct kbdiacr), CHKR_RW);
      }
      break;
    case KDSKBDIACR:		/* Sat Apr 23 1994 */
      {
	struct kbdiacrs *a = (struct kbdiacrs *) arg;
	chkr_check_addr ((PTR) (&a->kb_cnt), sizeof (a->kb_cnt), CHKR_RO);
	if (a->kb_cnt > 0)
	  chkr_check_addr ((PTR) (a->kbdiacr), a->kb_cnt * sizeof (struct kbdiacr), CHKR_RO);
      }
      break;
    case KDGETLED:		/* Sat Apr 23 1994 */
      chkr_check_addr ((PTR) arg, sizeof (char), CHKR_WO);
      break;
    case KDSETLED:		/* Sat Apr 23 1994 */
      break;
    case VT_SETMODE:		/* Sat Apr 23 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct vt_mode), CHKR_RO);
      break;
    case VT_GETMODE:		/* Sat Apr 23 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct vt_mode), CHKR_WO);
      break;
    case VT_GETSTATE:		/* Sat Apr 23 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct vt_stat), CHKR_WO);
      break;
    case VT_OPENQRY:		/* Sat Apr 23 1994 */
      chkr_check_addr ((PTR) arg, sizeof (long), CHKR_WO);
      break;
    case VT_ACTIVATE:		/* Sat Apr 23 1994 */
    case VT_WAITACTIVE:	/* Sat Apr 23 1994 */
    case VT_RELDISP:		/* Sat Apr 23 1994 */
      break;
    case PIO_FONT:		/* Sat Apr 23 1994 */
      chkr_check_addr ((PTR) arg, 8192, CHKR_RO);
      break;
    case GIO_FONT:		/* Sat Apr 23 1994 */
      chkr_check_addr ((PTR) arg, 8192, CHKR_WO);
      break;
    case PIO_SCRNMAP:		/* Sat Apr 23 1994 */
      chkr_check_addr ((PTR) arg, E_TABSZ, CHKR_RO);
      break;
    case GIO_SCRNMAP:		/* Sat Apr 23 1994 */
      chkr_check_addr ((PTR) arg, E_TABSZ, CHKR_WO);
      break;
      /* tpqic02: seem not to need arg */
/*	case MTRESET:-> FDCLRPRM */
/*	case MTFSF:  -> FDSETPRM ! */
/*	case MTBSF:  -> FDDEFPRM ! */
/*	case MTFSR:  -> FDGETPRM ! */
/*	case MTBSR:  -> FDMSGON */
/*	case MTWEOF: -> FDMSGOFF */
/*	case MTREW:  -> FDFMTBEG */
/*	case MTOFFL: -> FDFMTTRK ! */
/*	case MTNOP:  -> FDFMTEND */
    case MTRETEN:
/*	case MTBSFM: -> FDSETEMSGTRESH */
/*	case MTFSFM: -> FDFLUSH */
    case MTEOM:
    case MTERASE:
    case MTRAS1:
    case MTRAS2:
    case MTSEEK:
      break;
    case MTIOCGET:		/* Thu Apr 21 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct mtget), CHKR_WO);
      break;
    case MTIOCTOP:		/* Thu Apr 21 1994 */
      /* NB: there is a 2 spare bytes */
      chkr_check_addr ((PTR) arg, sizeof (short), CHKR_RO);
      chkr_check_addr ((PTR) (arg + 4), sizeof (int), CHKR_RO);
      break;
      /* floppy.c */
    case FDFMTBEG:		/* Sat Apr 16 1994 */
    case FDFMTEND:		/* Sat Apr 16 1994 */
      break;
    case FDGETPRM:		/* Sat Apr 16 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct floppy_struct), CHKR_WO);
      break;
    case FDFMTTRK:		/* Sat Apr 16 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct format_descr), CHKR_RO);
      break;
    case FDFLUSH:		/* Sat Apr 16 1994 */
      break;
    case FDCLRPRM:		/* Sat Apr 16 1994 */
      break;
    case FDSETPRM:		/* Sat Apr 16 1994 */
    case FDDEFPRM:		/* Sat Apr 16 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct floppy_struct), CHKR_RO);
      break;
    case FDMSGON:		/* Sat Apr 16 1994 */
    case FDMSGOFF:		/* Sat Apr 16 1994 */
    case FDSETEMSGTRESH:	/* Sat Apr 16 1994 */
      break;
      /* hd.c */
    case HDIO_GETGEO:		/* Sat Apr 16 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct hd_geometry), CHKR_WO);
      break;
    case BLKGETSIZE:		/* Sat Apr 16 1994 */
      chkr_check_addr ((PTR) arg, sizeof (long), CHKR_WO);
      break;
    case SIOCSIFENCAP:		/* Sat Sep 17 1994 */
      chkr_check_addr ((PTR) arg, sizeof (long), CHKR_RO);
      break;
    case SIOCGIFENCAP:		/* Sat Sep 17 1994 */
      chkr_check_addr ((PTR) arg, sizeof (unsigned long), CHKR_WO);
      break;
    case SIOCATMARK:		/* Sat Sep 17 1994 */
      chkr_check_addr ((PTR) arg, sizeof (unsigned long), CHKR_WO);
      break;
    case SIOCSPGRP:		/* Sat Aug 13 1994 */
      chkr_check_addr ((PTR) arg, sizeof (long), CHKR_RO);
      break;
    case SIOCGPGRP:		/* Sat Aug 13 1994 */
      chkr_check_addr ((PTR) arg, sizeof (long), CHKR_WO);
      break;
    case SIOCGSTAMP:		/* Sat Aug 13 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct timeval), CHKR_WO);
      break;
    case SIOCADDRT:		/* Sat Aug 13 1994 */
    case SIOCDELRT:		/* Sat Aug 13 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct rtentry), CHKR_RO);
      break;
    case SIOCDARP:		/* Sat Aug 13 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct arpreq), CHKR_RO);
      break;
    case SIOCGARP:		/* Sat Aug 13 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct arpreq), CHKR_WO);
      break;
    case SIOCSARP:		/* Sat Aug 13 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct arpreq), CHKR_RO);
      break;
    case SIOCDRARP:		/* Sat Aug 13 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct arpreq), CHKR_RO);
      break;
    case SIOCGRARP:		/* Sat Aug 13 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct arpreq), CHKR_WO);
      break;
    case SIOCSRARP:		/* Sat Aug 13 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct arpreq), CHKR_RO);
      break;
    case SIOCGIFFLAGS:		/* Sat Aug 13 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct ifreq), CHKR_WO);
      break;
    case SIOCSIFFLAGS:		/* Sat Aug 13 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct ifreq), CHKR_RO);
      break;
    case SIOCGIFADDR:		/* Sat Aug 13 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct ifreq), CHKR_WO);
      break;
    case SIOCSIFADDR:		/* Sat Aug 13 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct ifreq), CHKR_RO);
      break;
    case SIOCGIFBRDADDR:	/* Sat Aug 13 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct ifreq), CHKR_WO);
      break;
    case SIOCSIFBRDADDR:	/* Sat Aug 13 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct ifreq), CHKR_WO);
      break;
    case SIOCGIFDSTADDR:	/* Sat Aug 13 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct ifreq), CHKR_WO);
      break;
    case SIOCSIFDSTADDR:	/* Sat Aug 13 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct ifreq), CHKR_RO);
      break;
    case SIOCGIFNETMASK:	/* Sat Aug 13 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct ifreq), CHKR_WO);
      break;
    case SIOCSIFNETMASK:	/* Sat Aug 13 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct ifreq), CHKR_RO);
      break;
    case SIOCGIFMETRIC:		/* Sat Aug 13 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct ifreq), CHKR_WO);
      break;
    case SIOCSIFMETRIC:		/* Sat Aug 13 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct ifreq), CHKR_RO);
      break;
    case SIOCGIFMTU:		/* Sat Aug 13 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct ifreq), CHKR_WO);
      break;
    case SIOCSIFMTU:		/* Sat Aug 13 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct ifreq), CHKR_RO);
      break;
    case SIOCGIFHWADDR:		/* Sat Aug 13 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct ifreq), CHKR_WO);
      break;
    case SIOCSIFHWADDR:		/* Sat Aug 13 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct ifreq), CHKR_RO);
      break;
    case SIOCDEVPRIVATE:	/* Sat Aug 13 1994 */
      break;
    case SIOCGIFMAP:		/* Sat Aug 13 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct ifreq), CHKR_WO);
      break;
    case SIOCSIFMAP:		/* Sat Aug 13 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct ifreq), CHKR_RO);
      break;
    case SIOCGIFSLAVE:		/* Sat Aug 13 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct ifreq), CHKR_WO);
      break;
    case SIOCSIFSLAVE:		/* Sat Aug 13 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct ifreq), CHKR_RO);
      break;
    case SIOCGIFNAME:		/* Sat Sep 17 1994 */
      chkr_check_addr ((PTR) arg, 16, CHKR_WO);
      break;
    case SIOCGIFCONF:		/* Sat Sep 17 1994 */
      {
        struct ifconf *a = (struct ifconf *)arg;
        chkr_check_addr ((PTR) arg, sizeof (struct ifconf), CHKR_RW);
        chkr_check_addr ((PTR) a->ifc_buf, a->ifc_len, CHKR_TW);
        make_syscall();
        if (*res >= 0)
          chkr_set_right ((PTR) a->ifc_buf, a->ifc_len, CHKR_RW);
      }
      break;
    case SIOCGIFMEM:		/* Sat Sep 17 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct ifreq), CHKR_WO);
      break;
    case SIOCSIFMEM:		/* Sat Sep 17 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct ifreq), CHKR_RO);
      break;
    case SIOCSIFLINK:		/* Sat Aug 13 1994 */
    case BLKFLSBUF:		/* Sat Apr 16 1994 */
    case BLKRRPART:		/* Sat Apr 16 1994 */
    case TIOCLINUX:		/* I don't really know */
#endif

    case FIONREAD:		/* Mon Apr 4 1994 */
      chkr_check_addr ((PTR) arg, sizeof (int), CHKR_WO);
      break;
    case FIONBIO:
    case FIOASYNC:
      chkr_check_addr ((PTR) arg, sizeof (long), CHKR_RO);
      break;
    case FIOSETOWN:		/* Sat Sep 17 1994 */
      chkr_check_addr ((PTR) arg, sizeof (long), CHKR_RO);
      break;
    case FIOGETOWN:		/* Sat Sep 17 1994 */
      chkr_check_addr ((PTR) arg, sizeof (long), CHKR_WO);
      break;

    /* Streams.  Commented cases are currently not handled. */
    case I_PUSH:
      chkr_check_str ((PTR) arg, CHKR_RO);
      break;
    case I_POP:
      break;
    case I_LOOK:
      {
        PTR ptr = (PTR) arg;
        chkr_check_addr ((PTR) arg, FMNAMESZ + 1, CHKR_TW);
        make_syscall ();
        if (!psr.c)
          chkr_set_right (ptr, strlen (ptr) + 1, CHKR_RW);
      }
      break;
    case I_FLUSH:
      break;
    case I_FLUSHBAND:
      chkr_check_addr ((PTR) arg, sizeof (struct bandinfo), CHKR_RO);
      break;
    case I_SETSIG:
    case I_GETSIG:
      break;
    case I_FIND:
      chkr_check_str ((PTR) arg, CHKR_RO);
      break;
/*    case I_PEEK: */
    case I_SRDOPT:
      break;
    case I_GRDOPT:
    case I_NREAD:
      chkr_check_addr ((PTR) arg, sizeof (int), CHKR_WO);
      break;
/*    case I_FDINSERT: */
    case I_STR:
      {
        struct strioctl *strio = (struct strioctl *) arg;
        chkr_check_addr (&strio->ic_cmd, sizeof (int), CHKR_RO);
        chkr_check_addr (&strio->ic_timout, sizeof (int), CHKR_RO);
        chkr_check_addr (&strio->ic_len, sizeof (int), CHKR_RO);
        chkr_check_addr (&strio->ic_dp, sizeof (PTR), CHKR_RO);
        if (strio->ic_len > 0)
          chkr_check_addr (strio->ic_dp, strio->ic_len, CHKR_TW);
        make_syscall ();
        if (!psr.c && strio->ic_len > 0)
          chkr_set_right (strio->ic_dp, strio->ic_len, CHKR_RW);
      }
      break;      
    case I_SWROPT:
      break;
    case I_GWROPT:
      chkr_check_addr ((PTR) arg, sizeof (int), CHKR_WO);
      break;
    case I_SENDFD:
      fd_used_by_prog (arg);
      break;
    case I_RECVFD:
      chkr_check_addr ((PTR) arg, sizeof (int), CHKR_WO);
      break;
/*     case I_LIST: */
    case I_ATMARK:
      break;
    case I_CKBAND:
      break;
    case I_GETBAND:
      chkr_check_addr ((PTR) arg, sizeof (int), CHKR_WO);
      break;
    case I_CANPUT:
      break;
    case I_SETCLTIME:
      break;
    case I_GETCLTIME:
      chkr_check_addr ((PTR) arg, sizeof (int), CHKR_WO);
      break;
    case I_LINK:
      fd_used_by_prog (arg);
      break;
    case I_UNLINK:
      break;
    case I_PLINK:
    case I_PUNLINK:
      fd_used_by_prog (arg);
      break;
    
    /* ttcompat. */
    case TIOCGETP:
      chkr_check_addr ((PTR) arg, sizeof (struct sgttyb), CHKR_WO);
      break;
    
    /* kstat.  */
    case KSTAT_IOC_CHAIN_ID:
      break;
    case KSTAT_IOC_READ:
      chkr_check_addr ((PTR) arg, sizeof (kstat_t), CHKR_WO);
      break;
    case KSTAT_IOC_WRITE:
      chkr_check_addr ((PTR) arg, sizeof (kstat_t), CHKR_RO);
      break;
      
    default:			/* some other ioctl are not here */
      chkr_header (M_IOCTL_UNIMPLEMENT, cmd);
      chkr_printf (M_SEND_DESCRIPTION);
      make_syscall ();
      return 1;
    }
  return 0;
}

int
check_fcntl (int fd, int cmd, int arg, int *const res)
{
  switch (cmd)
    {
    case F_DUPFD:		/* arg is used as a value */
      make_syscall();
      fd_duped(*res, fd);
      break;
    case F_GETFD:
    case F_GETFL:
    case F_SETFD:		/* arg is used as a value */
    case F_SETFL:		/* arg is used as a value */
      break;
    case F_GETLK:
      chkr_check_addr ((PTR) arg, sizeof (struct flock), CHKR_WO);
      break;
    case F_SETLK:
    case F_SETLKW:	/* Wait */
      chkr_check_addr ((PTR) arg + offsetof(struct flock,l_pid), sizeof (int), CHKR_RO);
      break;
    case F_SETOWN:		/* arg is used as a value */
    case F_GETOWN:
      break;
    case F_FREESP:
      chkr_check_addr ((PTR) arg + offsetof(struct flock, l_whence), sizeof(((struct flock *)arg)->l_whence), CHKR_RO);
      chkr_check_addr ((PTR) arg + offsetof(struct flock, l_start), sizeof(((struct flock *)arg)->l_start), CHKR_RO);
      chkr_check_addr ((PTR) arg + offsetof(struct flock, l_len), sizeof(((struct flock *)arg)->l_len), CHKR_RO);
      break;
    default:
      chkr_header (M_FCNTL_UNIMPLEMENT, cmd);
      chkr_printf (M_SEND_DESCRIPTION);
      return 1;
    }
  return 0;
}

/* Check the address of socket, according to the protocol. */
static inline void
check_sockaddr(struct sockaddr *addr, int len)
{
    chkr_check_addr ((PTR) addr, sizeof(short), CHKR_RO);
    switch (addr->sa_family)
      {
    case AF_INET:
	if (len > sizeof(short)+sizeof(short)+sizeof(struct in_addr))
	  len = sizeof(short)+sizeof(short)+sizeof(struct in_addr);
	break;
    }
    chkr_check_addr ((PTR) addr, len, CHKR_RO);
}

#if 0
int
check_socketcall (int call, unsigned long *args, unsigned int *const res)
{
  /* check_socketcall assumes args[] is good */
  switch (call)
    {
    case SYS_ACCEPT:
      fd_used_by_prog (args[0]);
      chkr_check_addr ((PTR) args[2], sizeof (int), CHKR_RO);
      if (*(int *) args[2] > 0)
        chkr_check_addr ((PTR) args[1], *(int *) args[2], CHKR_TW);
      make_syscall ();
      if (*(int *) args[2] > 0)
	chkr_set_right ((PTR) args[1], *(int *) args[2], CHKR_RW);
      break;
    case SYS_BIND:
      fd_used_by_prog (args[0]);
      if (args[2] > 0)
        check_sockaddr((struct sockaddr*) args[1], args[2]);
      break;
    case SYS_CONNECT:
      fd_used_by_prog (args[0]);
      if (args[2] > 0)
        check_sockaddr((struct sockaddr*) args[1], args[2]);
      break;
    case SYS_GETPEERNAME:
      fd_used_by_prog (args[0]);
      chkr_check_addr ((PTR) args[2], sizeof (int), CHKR_RW);
      if (*(int*)args[2] > 0)
        chkr_check_addr ((PTR) args[1], *(int *) args[2], CHKR_TW);
      make_syscall ();
      if (*(int *) args[2] > 0)
	chkr_set_right ((PTR) args[1], *(int *) args[2], CHKR_RW);
      break;
    case SYS_GETSOCKNAME:
      fd_used_by_prog (args[0]);
      chkr_check_addr ((PTR) args[2], sizeof (int), CHKR_RW);
      if (*(int*)args[2] > 0)
        chkr_check_addr ((PTR) args[1], *(int *) args[2], CHKR_TW);
      make_syscall ();
      if (*(int *) args[2] > 0)
	chkr_set_right ((PTR) args[1], *(int *) args[2], CHKR_RW);
      break;
    case SYS_GETSOCKOPT:
      fd_used_by_prog (args[0]);
      chkr_check_addr ((PTR) args[4], sizeof (int), CHKR_RW);
      if (*(int*)args[4] > 0)
        chkr_check_addr ((PTR) args[3], *(int *) args[4], CHKR_TW);
      make_syscall ();
      if (*(int *) args[4] > 0)
	chkr_set_right ((PTR) args[3], *(int *) args[4], CHKR_RW);
      break;
    case SYS_LISTEN:
      fd_used_by_prog (args[0]);
      break;
    case SYS_RECV:
      fd_used_by_prog (args[0]);
      if (args[2] > 0)
        chkr_check_addr ((PTR) args[1], args[2], CHKR_TW);
      make_syscall ();
      if (*res > 0)
	chkr_set_right ((PTR) args[1], *res, CHKR_RW);
      break;
    case SYS_RECVFROM:
      fd_used_by_prog (args[0]);
      if (args[2] > 0)
        chkr_check_addr ((PTR) args[1], args[2], CHKR_TW);
      if (args[4])
	{
	  chkr_check_addr ((PTR) args[5], sizeof (int), CHKR_RO);
	  if (*(int*)args[5] > 0)
	    chkr_check_addr ((PTR) args[4], *(int *) args[5], CHKR_TW);
	}
      make_syscall ();
      if (*res > 0)
	{
	  if (args[2] > 0)
	    chkr_set_right ((PTR) args[1], args[2], CHKR_RW);
	  if (args[4] > 0)
	    chkr_set_right ((PTR) args[4], *(int *) args[5], CHKR_RW);
	}
      break;
    case SYS_SEND:
      fd_used_by_prog (args[0]);
      if (args[2] > 0)
        chkr_check_addr ((PTR) args[1], args[2], CHKR_RO);
      break;
    case SYS_SENDTO:
      fd_used_by_prog (args[0]);
      if (args[2] > 0)
        chkr_check_addr ((PTR) args[1], args[2], CHKR_RO);
      if (args[5] > 0)
        check_sockaddr((struct sockaddr*) args[4], args[5]);
      break;
    case SYS_SETSOCKOPT:
      fd_used_by_prog (args[0]);
      if (args[4] > 0)
        chkr_check_addr ((PTR) args[3], args[4], CHKR_RO);
      break;
    case SYS_SHUTDOWN:
      fd_used_by_prog (args[0]);
      break;
    case SYS_SOCKET:
      make_syscall ();
      if (*res != -1)
	fd_returned_by_system (*res);
      break;
    case SYS_SOCKETPAIR:
      chkr_check_addr ((PTR) args[3], 2 * sizeof (int), CHKR_WO);
      make_syscall ();
      if (*res != -1)
	{
	  fd_returned_by_system (((int *) args[3])[0]);
	  fd_returned_by_system (((int *) args[3])[1]);
	}
      break;
    default:
      chkr_header (M_SYSCALL_UNIMPLEMT, call);
      chkr_printf (M_SEND_DESCRIPTION);
      return 1;
    }
  return 0;
}

int
check_ipc (ulong call, ulong first, ulong second, ulong third, void *ptr, int *res)
{
  switch (call)
    {
    case SEMOP:
      if (second > 0)
        chkr_check_addr (ptr, second * sizeof (struct sembuf), CHKR_RW);
      break;
    case SEMGET:
      break;
    case SEMCTL:
      switch (second)
	{
	case IPC_INFO:		/* bug: semusz and semaem and not written */
	case SEM_INFO:		/* seems OK for SEM_INFO */
	  chkr_check_addr (ptr, sizeof (struct seminfo), CHKR_WO);
	  break;
	case IPC_STAT:
	  chkr_check_addr (ptr, sizeof (struct semid_ds), CHKR_WO);
	  break;
	case GETVAL:
	case GETPID:
	case GETNCNT:
	case GETZCNT:
	  break;
	case GETALL:
	  chkr_check_addr (ptr, 1 * sizeof (ushort), CHKR_WO);	/* FIXME */
	  break;
	case SETVAL:
	  break;
	case IPC_RMID:
	  break;
	case SETALL:
	  chkr_check_addr (ptr, 1 * sizeof (ushort), CHKR_RO);	/* FIXME */
	  break;
	case IPC_SET:
	  chkr_check_addr (ptr, sizeof (struct semid_ds), CHKR_WO);
	  break;
	default:
	  chkr_header (M_IPC_UNIMPLEMENT);
	  chkr_printf (M_SEND_DESCRIPTION);
	  return 1;		/* means error */
	}
      break;
    case MSGSND:
      if (second > 0)
        chkr_check_addr (ptr, second * sizeof (char) + sizeof (long), CHKR_RO);
      break;
    case MSGRCV:
      {
	struct ipc_kludge *tmp;
	chkr_check_addr (ptr, sizeof (struct ipc_kludge), CHKR_RO);
	tmp = (struct ipc_kludge *) ptr;
	if (second > 0)
	  chkr_check_addr (tmp->msgp, second * sizeof (char) + sizeof (long), CHKR_TW);
	make_syscall ();
	if (*res != 0)
	  chkr_check_addr (tmp->msgp, *res * sizeof (char) + sizeof (long), CHKR_TW);
      }
      break;
    case MSGGET:
      break;
    case MSGCTL:
      switch (second)
	{
	case IPC_INFO:		/* FIXME */
	case MSG_INFO:
	  chkr_check_addr (ptr, sizeof (struct msginfo), CHKR_WO);
	  break;
	case MSG_STAT:
	  chkr_check_addr (ptr, sizeof (struct msqid_ds), CHKR_WO);
	  break;
	case IPC_STAT:
	  chkr_check_addr (ptr, sizeof (struct msqid_ds), CHKR_WO);
	  break;
	case IPC_SET:
	  chkr_check_addr (ptr, sizeof (struct msqid_ds), CHKR_RO);
	  break;
	case IPC_RMID:
	  break;
	default:
	  chkr_header (M_IPC_UNIMPLEMENT);
	  chkr_printf (M_SEND_DESCRIPTION);
	  return 1;		/* means error */
	}
      break;
    case SHMAT:
      if (ptr >= (void*)MM_LOW && ptr <= (void*)MM_HIGH)
        {
          skip_syscall();
          *res = EINVAL;
        }
      make_syscall();
      chkr_set_right((void*)third, sizeof(void*), CHKR_RW);
      if (*res >= 0)
        new_segshm(first, *((void**)third), second);
      break;
    case SHMDT:
      make_syscall();
      if (*res >= 0)
        remove_shm(ptr);
      break;
    case SHMGET:
      break;
    case SHMCTL:
      switch (second)
	{
	case IPC_INFO:
	  chkr_check_addr (ptr, sizeof (struct shminfo), CHKR_WO);
	  break;
	case SHM_INFO:
	  chkr_check_addr (ptr, sizeof (struct shm_info), CHKR_WO);
	  break;
	case SHM_STAT:
	  chkr_check_addr (ptr, sizeof (struct shmid_ds), CHKR_WO);
	  break;
	case SHM_UNLOCK:
	case SHM_LOCK:
	  break;
	case IPC_STAT:
	  chkr_check_addr (ptr, sizeof (struct shmid_ds), CHKR_WO);
	  break;
	case IPC_SET:
	case IPC_RMID:
	  break;
	default:
	  chkr_header (M_IPC_UNIMPLEMENT);
	  chkr_printf (M_SEND_DESCRIPTION);
	  return 1;		/* means error */
	}
      break;
    default:
      chkr_header (M_IPC_UNIMPLEMENT, call);
      chkr_printf (M_SEND_DESCRIPTION);
      return 1;			/* means error */
    }
  return 0;
}
#endif

static int
check_acl (int is_facl, uint *regs)
{
  aclent_t *aclbuf = (aclent_t *)PTR4;
  int nentries = ARG3;
  int cmd = ARG2;
  int i;

  if (is_facl)
    fd_used_by_prog (ARG1);
  else
    chkr_check_str (PTR1, CHKR_RO);
  switch (cmd)
    {
    case GETACL:
      for (i = 0; i < nentries; i++)
	chkr_check_addr (aclbuf + i, sizeof (aclent_t), CHKR_MW);
      make_syscall ();
      if (RES > 0)
	for (i = 0; i < nentries && i < RES; i++)
	  chkr_check_addr (aclbuf + i, sizeof (aclent_t), CHKR_WO);
      break;
    case SETACL:
      for (i = 0; i < nentries; i++)
	chkr_check_addr (aclbuf + i, sizeof (aclent_t), CHKR_RO);
      break;
    case GETACLCNT:
      break;
    default:
      psr.c = 1;
      RES = EINVAL;
      break;
    }
  return 0;
}
