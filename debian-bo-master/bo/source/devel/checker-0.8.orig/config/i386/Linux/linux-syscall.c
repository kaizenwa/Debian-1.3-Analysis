/* function called by chkr_x_x_x_x_chkr, at syscalls.
   Copyright 1993, 1994, 1995 Tristan Gingold
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
#include <asm/vm86.h>
#include <sys/ioctl.h>
#include <sys/dirent.h>
#include <sys/socketcall.h>
#include <sys/mtio.h>
#include <netinet/in.h>
#define __KERNEL__		/* needed for msg, shm, sem */
#include <linux/linkage.h>
#include <sys/shm.h>
#include <sys/sem.h>
#include <sys/msg.h>
#undef __KERNEL__
#include <linux/timex.h>
#include <linux/fs.h>
#include <linux/tty.h>
#include <linux/hdreg.h>
#include <linux/fd.h>
#include <linux/kd.h>
#include <linux/vt.h>
#include <linux/kernel.h>
#include <linux/serial.h>
#include <linux/route.h>
#include <linux/netdevice.h>
#include <linux/if_arp.h>

#define NEED_MM
#include "checker.h"
#include "errlist.h"
#include "message.h"

struct reg
{
  unsigned int eax;
  unsigned int ecx;
  unsigned int edx;
  unsigned int ebx;
  unsigned int esp;
  unsigned int ebp;
  unsigned int esi;
  unsigned int edi;
  unsigned int eip;
  unsigned int flags;
};

extern void make_syscall (void);
extern void make_pid(void);
extern void skip_syscall (void);
extern void update_output_file(void);
int check_ioctl (int cmd, int arg, int *const res);
int check_ipc (ulong call, ulong first, ulong second, ulong third, void *ptr, int *res);
int check_socketcall (int call, unsigned long *args, unsigned int *const res);
int check_fcntl (int fd, int cmd, int arg, int *const res);

#define RES  ((signed long)(regs->eax))
#define CALLNBR RES
#define ARG1 (regs->ebx)
#define ARG2 (regs->ecx)
#define ARG3 (regs->edx)
#define ARG4 (regs->esi)
#define ARG5 (regs->edi)

#define RESPTR (&(regs->eax))
#define PTR1 (PTR)ARG1
#define PTR2 (PTR)ARG2
#define PTR3 (PTR)ARG3
#define PTR4 (PTR)ARG4
#define PTR5 (PTR)ARG5

/* This function is called by a stub to check a syscall.  All the registers 
 *  available through REGS and can be modified. */
void
check_syscall (struct reg *const regs)
{
  int error = 0;
  switch (CALLNBR)
    {
    case SYS_setup:
      error = 1;
      break;			/* must not be called */
    case SYS_exit:
      chkr_do_end ();
      break;
    case SYS_fork:
      make_syscall ();
      if (RES == 0)
        chkr_clean_after_fork ();
      break;
    case SYS_read:
      fd_used_by_prog (ARG1);
      if (ARG3 > 0)
        chkr_check_addr (PTR2, ARG3, CHKR_TW);
      make_syscall ();
      if (RES > 0)
	chkr_set_right (PTR2, RES, CHKR_RW);
      break;
    case SYS_write:
      fd_used_by_prog (ARG1);
      if (ARG3 > 0)
        chkr_check_addr (PTR2, ARG3, CHKR_RO);
      break;
    case SYS_open:
      chkr_check_str (PTR1, CHKR_RO);
      make_syscall ();
      if (RES >= 0)
	fd_returned_by_system (RES);
      break;
    case SYS_close:
      if (!fd_used_by_prog (ARG1))
	{
	  skip_syscall ();
	  RES = -EBADF;
	}
      else
        fd_closed(ARG1);
      break;
    case SYS_waitpid:
      error = 1;		/* replaced by wait4 */
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
    case SYS_chdir:
      chkr_check_str (PTR1, CHKR_RO);
      break;
    case SYS_time:
      if (ARG1)
	chkr_check_addr (PTR1, sizeof (time_t), CHKR_WO);
      break;
#ifdef SYS_prev_mknod
    case SYS_prev_mknod:
#else
    case SYS_mknod:
#endif
      chkr_check_str (PTR1, CHKR_RO);
      break;
    case SYS_chmod:
      chkr_check_str (PTR1, CHKR_RO);
      break;
    case SYS_chown:
      chkr_check_str (PTR1, CHKR_RO);
      break;
    case SYS_break:
      error = 1;
      break;
    case SYS_oldstat:
      error = 1;
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
    case SYS_oldfstat:
      error = 1;
      break;
    case SYS_pause:
      break;
    case SYS_utime:
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
    case SYS_ftime:
      break;
    case SYS_sync:
      break;
    case SYS_kill:
      break;
    case SYS_rename:
      chkr_check_str (PTR1, CHKR_RO);
      chkr_check_str (PTR2, CHKR_RO);
      break;
    case SYS_mkdir:
      chkr_check_str (PTR1, CHKR_RO);
      break;
    case SYS_rmdir:
      chkr_check_str (PTR1, CHKR_RO);
      break;
    case SYS_dup:
      fd_used_by_prog (ARG1);
      make_syscall ();
      fd_duped (RES, ARG1);
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
    case SYS_prof:
      error = 1;
      break;
    case SYS_brk:
      /* FIXME: call brk ? */
      error = 1;
      break;
    case SYS_setgid:
      break;
    case SYS_getgid:
      break;
    case SYS_signal:
      error = 1;
      break;			/* replaced by sigaction */
    case SYS_geteuid:
      break;
    case SYS_getegid:
      break;
    case SYS_acct:
      chkr_check_str (PTR1, CHKR_RO);
      break;
    case SYS_phys:
      error = 1;
      break;
    case SYS_lock:
      error = 1;
      break;
    case SYS_ioctl:
      fd_used_by_prog (ARG1);
      error = check_ioctl (ARG2, ARG3, RESPTR);
      break;
    case SYS_fcntl:
      fd_used_by_prog (ARG1);
      error = check_fcntl (ARG1, ARG2, ARG3, RESPTR);
      break;
    case SYS_mpx:
      error = 1;
      break;
    case SYS_setpgid:
      break;
    case SYS_ulimit:
      error = 1;
      break;			/* replaced by rlimit */
    case SYS_oldolduname:
      error = 1;
      break;
    case SYS_umask:
      break;
    case SYS_chroot:
      chkr_check_str (PTR1, CHKR_RO);
      break;
#ifdef SYS_prev_ustat
    case SYS_prev_ustat:
#else
    case SYS_ustat:
#endif
      chkr_check_addr (PTR2, sizeof (struct ustat), CHKR_WO);
      break;
    case SYS_dup2:
      if (!fd_used_by_prog (ARG1) || !fd_used_by_prog (ARG2))
	{
	  skip_syscall ();
	  RES = -EBADF;
	}
      else
        fd_duped(ARG1, ARG2);
      break;
    case SYS_getppid:
      break;
    case SYS_getpgrp:
      break;
    case SYS_setsid:
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
      break;
    case SYS_siggetmask:
      error = 1;		/* old syscall: sigprocmask */
      break;
    case SYS_sigsetmask:
      error = 1;		/* old syscall: sigprocmask */
      break;
    case SYS_setreuid:
      break;
    case SYS_setregid:
      break;
    case SYS_sigsuspend:
      break;
    case SYS_sigpending:
      break;
    case SYS_sethostname:
      if (ARG2)
        chkr_check_addr (PTR1, ARG2, CHKR_RO);
      break;
    case SYS_setrlimit:
      chkr_check_addr (PTR2, sizeof (struct rlimit), CHKR_RO);
      break;
    case SYS_getrlimit:
      chkr_check_addr (PTR2, sizeof (struct rlimit), CHKR_WO);
      break;
    case SYS_getrusage:
      chkr_check_addr (PTR2, sizeof (struct rusage), CHKR_WO);
      break;
    case SYS_gettimeofday:
      if (ARG1)
        chkr_check_addr (PTR1, sizeof (struct timeval), CHKR_WO);
      if (ARG2)
        chkr_check_addr (PTR2, sizeof (struct timezone), CHKR_WO);
      break;
    case SYS_settimeofday:
      chkr_check_addr (PTR1, sizeof (struct timeval), CHKR_RO);
      chkr_check_addr (PTR2, sizeof (struct timezone), CHKR_RO);
      break;
    case SYS_getgroups:
      if (ARG1)
        chkr_check_addr (PTR2, ARG1 * sizeof (__gid_t), CHKR_TW);
      make_syscall ();
      if (RES > 0)
	chkr_set_right (PTR2, RES * sizeof (__gid_t), CHKR_RW);
      break;
    case SYS_setgroups:
      if (ARG1)
        chkr_check_addr (PTR2, ARG1 * sizeof (__gid_t), CHKR_RO);
      break;
    case SYS_select:
      {
	char **ptr;
	fd_set fd_used;
	int i, j, nbr_fd;

	/* Only one warning by fd ! */
        FD_ZERO(&fd_used);
        
	chkr_check_addr (PTR1, 5 * sizeof (int *), CHKR_RO);
	
	/* Only the first NBR_FD are used */
	nbr_fd = ((int *)ARG1)[0];
	nbr_fd = nbr_fd > FD_SETSIZE ? FD_SETSIZE : nbr_fd;
	
	/* | could replace FD_SET, but the later one is more portable */
	ptr = (char **) PTR1;
	if (ptr[1])
	  {
	    chkr_check_addr (ptr[1], sizeof (fd_set), CHKR_RW);
	    for (i = 0; i < nbr_fd; i++)
	      if (FD_ISSET(i, ptr[1]))
	        FD_SET(i, &fd_used);
	  }
	if (ptr[2])
	  {
	    chkr_check_addr (ptr[2], sizeof (fd_set), CHKR_RW);
	    for (i = 0; i < nbr_fd; i++)
	      if (FD_ISSET(i, ptr[2]))
	        FD_SET(i, &fd_used);
	  }
	if (ptr[3])
	  {
	    chkr_check_addr (ptr[3], sizeof (fd_set), CHKR_RW);
	    for (i = 0; i < nbr_fd; i++)
	      if (FD_ISSET(i, ptr[3]))
	        FD_SET(i, &fd_used);
	  }
	if (ptr[4])
	  chkr_check_addr (ptr[4], sizeof (struct timeval), CHKR_RW);
	  
	/* Now we can check each fd */
	for (i = 0; i < nbr_fd; i++)
	  if (FD_ISSET(i, &fd_used))
	    if (!fd_used_by_prog(i))
	      {
	        /* This fd is reserved by the system, so remove it from 
	         * the lists. */
	        for (j = 1; j < 4; j++)
	          if (ptr[j])
	            FD_CLR(i, ptr[j]);
	      }
      }
      break;
    case SYS_symlink:
      chkr_check_str (PTR1, CHKR_RO);
      chkr_check_str (PTR2, CHKR_RO);
      break;
    case SYS_oldlstat:
      error = 1;
      break;
    case SYS_readlink:
      chkr_check_str (PTR1, CHKR_RO);
      if (ARG3)
        chkr_check_addr (PTR2, ARG3, CHKR_TW);
      make_syscall ();
      if (RES > 0)
	chkr_set_right (PTR2, RES, CHKR_RW);
      break;
    case SYS_uselib:
      error = 1;
      break;
    case SYS_swapon:
      chkr_check_str (PTR1, CHKR_RO);
      break;
    case SYS_reboot:
      break;
    case SYS_readdir:
      fd_used_by_prog (ARG1);
      chkr_check_addr (PTR2, sizeof (struct dirent), CHKR_WO);
      break;
    case SYS_mmap:
      {
        long *buffer = (long*)PTR1;
        chkr_check_addr(PTR1, 6 * sizeof(long int), CHKR_RO);
        if (!(buffer[3] & MAP_ANONYMOUS))
          fd_used_by_prog(buffer[4]);
        if (buffer[0] >= MM_LOW && buffer[0] <= MM_HIGH)
          {
            if (buffer[3] & MAP_FIXED)
              {
                skip_syscall();	/* can't */
                RES = -EINVAL;
                break;
              }
            buffer[0] = 0;
          }
        make_syscall();
        if (RES < 0 && RES > -1000)     /* up to 1000 errors ! FIXME */
          break;        /* fail */
        new_segmmap((void*)RES, buffer[1], buffer[2], buffer[3],
                    buffer[4], buffer[5]);
      }
      break;
    case SYS_munmap:
      if (ARG1 >= MM_LOW && ARG2 <= MM_HIGH)
        {
          skip_syscall();
          RES = -EINVAL;
          break;
        }
      make_syscall();
      if (RES == 0)
        remove_mmap(PTR1, ARG2);
      break;			/* not supported */
    case SYS_truncate:
      chkr_check_str (PTR1, CHKR_RO);
      break;
    case SYS_ftruncate:
      fd_used_by_prog (ARG1);
      break;
    case SYS_fchmod:
      fd_used_by_prog (ARG1);
      break;
    case SYS_fchown:
      fd_used_by_prog (ARG1);
      break;
    case SYS_getpriority:
      break;
    case SYS_setpriority:
      break;
    case SYS_profil:
      error = 1;
      break;			/* not supported */
    case SYS_statfs:
      chkr_check_str (PTR1, CHKR_RO);
      chkr_check_addr (PTR2, sizeof (struct statfs), CHKR_WO);
      break;
    case SYS_fstatfs:
      fd_used_by_prog (ARG1);
      chkr_check_addr (PTR2, sizeof (struct statfs), CHKR_WO);
      break;
    case SYS_ioperm:
      break;
    case SYS_socketcall:
      error = check_socketcall (ARG1, (unsigned long *) ARG2, RESPTR);
      break;
    case SYS_klog:
      switch (ARG1)
        {
          case 0:
          case 1:
          case 5:
          case 6:
          case 7:
          case 8:
            break;			/* nothing to check */
          case 2:
          case 3:
          case 4:
            if (ARG3)
              chkr_check_addr (PTR2, ARG3, CHKR_TW);
            make_syscall ();
            if (RES > 0)
              chkr_set_right (PTR2, RES, CHKR_RW);
            break;
          default:
            error = 1;
        }
      break;
    case SYS_setitimer:
      chkr_check_addr (PTR2, sizeof (struct itimerval), CHKR_RO);
      if (ARG3)
	chkr_check_addr (PTR3, sizeof (struct itimerval), CHKR_WO);
      break;
    case SYS_getitimer:
      chkr_check_addr (PTR2, sizeof (struct itimerval), CHKR_WO);
      break;
#ifdef SYS_prev_stat
    case SYS_prev_stat:
#else
    case SYS_stat:
#endif
      chkr_check_str (PTR1, CHKR_RO);
      chkr_check_addr (PTR2, sizeof (struct stat), CHKR_WO);
      break;
#ifdef SYS_prev_lstat
    case SYS_prev_lstat:
#else
    case SYS_lstat:
#endif
      chkr_check_str (PTR1, CHKR_RO);
      chkr_check_addr (PTR2, sizeof (struct stat), CHKR_WO);
      break;
#ifdef SYS_prev_fstat
    case SYS_prev_fstat:
#else
    case SYS_fstat:
#endif
      fd_used_by_prog (ARG1);
      chkr_check_addr (PTR2, sizeof (struct stat), CHKR_WO);
      break;
    case SYS_olduname:
      error = 1;
      break;
    case SYS_iopl:
      break;
    case SYS_vhangup:
      break;
    case SYS_idle:
      break;
    case SYS_vm86:
      chkr_check_addr (PTR1, sizeof (struct vm86_struct), CHKR_RW);
      break;
    case SYS_wait4:
      if (ARG2)
	chkr_check_addr (PTR2, sizeof (int *), CHKR_WO);
      if (ARG4)
	chkr_check_addr (PTR4, sizeof (struct rusage), CHKR_WO);
      break;
    case SYS_swapoff:
      chkr_check_str (PTR1, CHKR_RO);
      break;
    case SYS_sysinfo:
      /* 22 is due to the pad */
      chkr_check_addr (PTR1, sizeof (struct sysinfo) - 22, CHKR_WO);
      break;
    case SYS_ipc:
      error = check_ipc (ARG1, ARG2, ARG3, ARG4, PTR5, RESPTR);
      break;
    case SYS_fsync:
      fd_used_by_prog (ARG1);
      break;
    case SYS_sigreturn:
      break;
    case SYS_clone:
      /* Not supported because of the stack and recursion */
      error = 1;
      break;
    case SYS_setdomainname:
      if (ARG2)
        chkr_check_addr (PTR1, ARG2, CHKR_RO);
      break;
    case SYS_uname:
      chkr_check_addr (PTR1, sizeof (struct utsname), CHKR_WO);
      break;
    case SYS_modify_ldt:
      error = 1;
      break;
    case SYS_adjtimex:
      chkr_check_addr (PTR1, sizeof (struct timex), CHKR_RW);
      break;
    case SYS_mprotect:
      error = 1;		/* not yet supported */
      break;
    case SYS_sigprocmask:
      if (ARG2)
	chkr_check_addr (PTR2, sizeof (sigset_t), CHKR_RO);
      if (ARG3)
	chkr_check_addr (PTR3, sizeof (sigset_t), CHKR_WO);
      break;
    case SYS_create_module:
      error = 1;
      break;
    case SYS_init_module:
      error = 1;
      break;
    case SYS_delete_module:
      error = 1;
      break;
    case SYS_get_kernel_syms:
      error = 1;
      break;
    case SYS_quotactl:
      error = 1;
      break;
    case SYS_getpgid:
      break;
    case SYS_fchdir:
      break;
    case SYS_bdflush:
      if (ARG1 >= 2 && (ARG1 & 1) == 0)
        chkr_check_addr(PTR2, CHKR_WO, sizeof (int));
      break;
    case SYS_sysfs:
      /* Too weird */
      error = 1;
      break;
    case SYS_personality:
      error = 1;
      break;
    case SYS_afs_syscall:
      error = 1;
      break;
#if 0
    case SYS_setfsuid:
      break;
    case SYS_setfsgid:
      break;
    case SYS__llseek:
      fd_used_by_prog(ARG1);
      chkr_check_addr(PTR4, CHKR_WO, sizeof(loff_t));
      break;
#endif
    default:
      error = 1;
    }
  if (error)
    {
      chkr_perror (M_I_IES_SC_ET);
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
      chkr_check_addr ((PTR) arg, sizeof (struct termio), CHKR_WO);
      break;
    case TCSETAW:		/* Mon Apr 4 1994 */
    case TCSETAF:		/* Mon Apr 4 1994 */
    case TCSETA:		/* Mon Apr 4 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct termio), CHKR_RO);
      break;
    case TCXONC:		/* Mon Apr 4 1994 */
    case TCFLSH:		/* Mon Apr 4 1994 */
      chkr_check_addr ((PTR) arg, sizeof (int), CHKR_RO);
      break;
    case TIOCEXCL:		/* Mon Apr 4 1994 */
    case TIOCNXCL:		/* Mon Apr 4 1994 */
    case TIOCSCTTY:		/* Mon Apr 4 1994 */
      break;
    case TIOCGPGRP:		/* Mon Apr 4 1994 */
      chkr_check_addr ((PTR) arg, sizeof (pid_t), CHKR_WO);
      break;
    case TIOCSPGRP:		/* Mon Apr 4 1994 */
      chkr_check_addr ((PTR) arg, sizeof (pid_t), CHKR_RO);
      break;
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
    case TIOCGWINSZ:		/* Mon Apr 4 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct winsize), CHKR_WO);
      break;
    case TIOCSWINSZ:		/* Mon Apr 4 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct winsize), CHKR_RO);
      break;
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
    case FIONREAD:		/* Mon Apr 4 1994 */
      chkr_check_addr ((PTR) arg, sizeof (int), CHKR_WO);
      break;
    case TIOCGSERIAL:		/* Mon Apr 4 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct serial_struct), CHKR_WO);
      break;
    case TIOCSSERIAL:		/* Mon Apr 4 1994 */
      chkr_check_addr ((PTR) arg, sizeof (struct serial_struct), CHKR_RO);
      break;
    case FIONBIO:
    case FIOASYNC:
      chkr_check_addr ((PTR) arg, sizeof (long), CHKR_RO);
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
    case FIOSETOWN:		/* Sat Sep 17 1994 */
      chkr_check_addr ((__ptr_t) arg, sizeof (long), CHKR_RO);
      break;
    case FIOGETOWN:		/* Sat Sep 17 1994 */
      chkr_check_addr ((__ptr_t) arg, sizeof (long), CHKR_WO);
      break;
    case SIOCSIFENCAP:		/* Sat Sep 17 1994 */
      chkr_check_addr ((__ptr_t) arg, sizeof (long), CHKR_RO);
      break;
    case SIOCGIFENCAP:		/* Sat Sep 17 1994 */
      chkr_check_addr ((__ptr_t) arg, sizeof (unsigned long), CHKR_WO);
      break;
    case SIOCATMARK:		/* Sat Sep 17 1994 */
      chkr_check_addr ((__ptr_t) arg, sizeof (unsigned long), CHKR_WO);
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
      chkr_check_addr ((__ptr_t) arg, 16, CHKR_WO);
      break;
    case SIOCGIFCONF:		/* Sat Sep 17 1994 */
      {
        struct ifconf *a = (struct ifconf *)arg;
        chkr_check_addr ((__ptr_t) arg, sizeof (struct ifconf), CHKR_RW);
        chkr_check_addr ((__ptr_t) a->ifc_buf, a->ifc_len, CHKR_TW);
        make_syscall();
        if (*res >= 0)
          chkr_set_right ((__ptr_t) a->ifc_buf, a->ifc_len, CHKR_RW);
      }
      break;
    case SIOCGIFMEM:		/* Sat Sep 17 1994 */
      chkr_check_addr ((__ptr_t) arg, sizeof (struct ifreq), CHKR_WO);
      break;
    case SIOCSIFMEM:		/* Sat Sep 17 1994 */
      chkr_check_addr ((__ptr_t) arg, sizeof (struct ifreq), CHKR_RO);
      break;
    case SIOCSIFLINK:		/* Sat Aug 13 1994 */
    case BLKFLSBUF:		/* Sat Apr 16 1994 */
    case BLKRRPART:		/* Sat Apr 16 1994 */
    case TIOCLINUX:		/* I don't really know */
    default:			/* some other ioctl are not here */
      chkr_header (M_IOCTL_UNIMPLEMENT, cmd);
      chkr_printf (M_SEND_DESCRIPTION);
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
      chkr_check_addr ((PTR) arg, offsetof(struct flock,l_pid), CHKR_RO);
      break;
    case F_SETOWN:		/* arg is used as a value */
    case F_GETOWN:
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
    chkr_check_addr ((__ptr_t) addr, sizeof(short), CHKR_RO);
    switch (addr->sa_family)
      {
    case AF_INET:
	if (len > sizeof(short)+sizeof(short)+sizeof(struct in_addr))
	  len = sizeof(short)+sizeof(short)+sizeof(struct in_addr);
	break;
    }
    chkr_check_addr ((__ptr_t) addr, len, CHKR_RO);
}

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
          chkr_set_right ((PTR) args[1], *res, CHKR_RW);
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
          *res = -EINVAL;
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
