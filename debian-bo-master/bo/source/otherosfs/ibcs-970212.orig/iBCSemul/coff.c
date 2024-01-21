/*
 *  linux/abi/emulate.c
 *
 *  Copyright (C) 1993  Linus Torvalds
 *
 *   Modified by Eric Youngdale to include all ibcs syscalls.
 *   Re-written by Drew Sullivan to handle lots more of the syscalls correctly.
 *
 * $Id: coff.c,v 1.29 1996/08/07 15:01:40 mike Exp $
 * $Source: /usr/CVS/ibcs/iBCSemul/coff.c,v $
 */

#define __KERNEL__ 1
#include <linux/config.h>

#include <linux/module.h>
#include <linux/version.h>

#include <asm/segment.h>
#ifndef KERNEL_DS
#include <linux/segment.h>
#endif

#include <linux/types.h>
#include <linux/errno.h>
#include <linux/sched.h>
#include <linux/kernel.h>
#include <linux/mm.h>
#include <linux/stat.h>
#include <linux/stddef.h>
#include <linux/unistd.h>
#include <linux/ptrace.h>
#include <linux/fcntl.h>
#include <linux/time.h>

#include <asm/system.h>
#include <linux/fs.h>
#include <linux/sys.h>
#include <linux/malloc.h>

#include <ibcs/ibcs.h>

#ifdef __NR_getdents
#include <linux/dirent.h>
#endif

#ifdef IBCS_TRACE
#include <ibcs/trace.h>
#endif


int ibcs_brk(unsigned long newbrk)
{
	if (!newbrk)
		return current->MM(brk);
	if (newbrk != current->MM(brk) && (unsigned long)SYS(brk)(newbrk) != newbrk)
		return -ENOMEM;
	return 0;
}


#ifdef __sparc__
int ibcs_fork(struct pt_regs * regs) {
       /* No fork yet */
       printk ("ibcs2/sparc: No fork yet\n");
       send_sig(SIGSEGV, current, 1);
       return -1;
}

int ibcs_wait(struct pt_regs * regs) {
       /* No fork yet */
       printk ("ibcs2/sparc: No wait yet\n");
       send_sig(SIGSEGV, current, 1);
       return -1;
}

int ibcs_exec(struct pt_regs * regs) {

       /* No exec yet */
       printk ("ibcs2/sparc: No fork yet\n");
       send_sig(SIGSEGV, current, 1);
       return -1;
}

int ibcs_pipe(struct pt_regs * regs) {
       long filedes[2];
       int old_fs = get_fs();
       int rvalue;

       set_fs(get_ds());
       rvalue = SYS(pipe)(&filedes);
       set_fs(old_fs);
       if (rvalue == 0) {
               rvalue = filedes[0];
               regs->u_regs [UREG_I0] = filedes[1];
       }
       return rvalue;
}

/* note the double value return in eax and edx */
int ibcs_getpid(struct pt_regs * regs) {
       return current->pid;
}

/* note the double value return in eax and edx */
int ibcs_getuid(struct pt_regs * regs) {
       return current->uid;
}

/* note the double value return in eax and edx */
int ibcs_getgid(struct pt_regs * regs) {
       return current->gid;
}

#else /* __sparc__ */

int ibcs_lseek(int fd, unsigned long offset, int whence)
{
	int error;
	struct inode *ino;

	error = SYS(lseek)(fd, offset, whence);
	if (error != -ESPIPE || current->personality != PER_SCOSVR3)
		return error;

	ino = current->FD[fd]->f_inode;
	if (S_ISCHR(ino->i_mode) || S_ISBLK(ino->i_mode))
		return 0;

	return error;
}


int ibcs_fork(struct pt_regs * regs) {
	int rvalue;

	regs->eflags &= ~1; /* Clear carry flag */
	rvalue = SYS(fork)(regs->ebx, regs->ecx, 1,
		regs->esi, regs->edi, regs->ebp, regs->eax, regs->ds,
		regs->es, regs->fs, regs->gs, regs->orig_eax,
		regs->eip, regs->cs, regs->eflags, regs->esp, regs->ss);
	regs->edx = 0;
	return rvalue;
}

int ibcs_pipe(struct pt_regs * regs) {
	long filedes[2];
	int old_fs = get_fs();
	int rvalue;

	set_fs(get_ds());
	rvalue = SYS(pipe)(&filedes);
	set_fs(old_fs);
	if (rvalue == 0) {
		rvalue = filedes[0];
		regs->edx = filedes[1];
	}
	return rvalue;
}

/* note the double value return in eax and edx */
int ibcs_getpid(struct pt_regs * regs) {
	regs->edx = current->p_pptr->pid;

	return current->pid;
}

/* note the double value return in eax and edx */
int ibcs_getuid(struct pt_regs * regs) {
	regs->edx = current->euid;

	return current->uid;
}

/* note the double value return in eax and edx */
int ibcs_getgid(struct pt_regs * regs) {
	regs->edx = current->egid;

	return current->gid;
}

#define FLAG_ZF 0x0040
#define FLAG_PF 0x0004
#define FLAG_SF 0x0080
#define FLAG_OF 0x0800

#define MAGIC_WAITPID_FLAG (FLAG_ZF | FLAG_PF | FLAG_SF | FLAG_OF)

int ibcs_wait(struct pt_regs * regs) {
	long	result, kopt;
	int	pid, loc, opt;
	int	old_fs;

	/* xenix wait() puts status to edx and returns pid */
	if ((current->personality & PER_MASK) == (PER_XENIX & PER_MASK)) {
		old_fs = get_fs();
		set_fs (get_ds());
		result = SYS(wait4)(-1, &loc, 0, NULL);
		set_fs(old_fs);

		regs->edx = loc;
		return result;
	}
	/* if ZF,PF,SF,and OF are set then it is waitpid */
	if ((regs->eflags & MAGIC_WAITPID_FLAG) == MAGIC_WAITPID_FLAG) {
		pid = get_fs_long(((unsigned long *) regs->esp) + 1);
		loc = get_fs_long(((unsigned long *) regs->esp) + 2);
		opt = get_fs_long(((unsigned long *) regs->esp) + 3);

		/* Now translate the options from the SVr4 numbers */
		kopt = 0;
		if (opt & 0100) kopt |= WNOHANG;
		if (opt & 4) kopt |= WUNTRACED;

		result = SYS(wait4)(pid, loc, kopt, NULL);
	} else {
		loc = get_fs_long(((unsigned long *) regs->esp) + 1);
		result = SYS(wait4)(-1, loc, WUNTRACED, NULL);
	}
	if (result >= 0 && loc) {
		regs->edx = get_fs_long((unsigned long *) loc);
		if ((regs->edx & 0xff) == 0x7f) {
			int sig = (regs->edx >> 8) & 0xff;
			sig = current->exec_domain->signal_map[sig];
			regs->edx = (regs->edx & (~0xff00)) | (sig << 8);
			put_fs_long(regs->edx, (unsigned long *)loc);
		} else if (regs->edx && regs->edx == (regs->edx & 0xff)) {
			regs->edx = current->exec_domain->signal_map[regs->edx & 0x7f];
			put_fs_long(regs->edx, (unsigned long *)loc);
		}
	}
	return result;
}


/*
 * ibcs_exec() executes a new program.
 */
int ibcs_exec(struct pt_regs *regs)
{
	int error;
	char *pgm, **argv, **envp;
	char *filename;

	pgm = (char *)get_fs_long(((unsigned long *) regs->esp) + 1);
	argv = (char **)get_fs_long(((unsigned long *) regs->esp) + 2);
	envp = (char **)get_fs_long(((unsigned long *) regs->esp) + 3);
#ifdef IBCS_TRACE
	if ((ibcs_trace & TRACE_API) || ibcs_func_p->trace) {
		int i;
		char **v, *p, *q;

		if ((error = getname(pgm, &q))) {
			printk(KERN_DEBUG "iBCS:       pgm: 0x%lx bad pointer!\n",
				(unsigned long)pgm);
		} else {
			printk(KERN_DEBUG "iBCS        pgm: 0x%lx \"%s\"\n",
				(unsigned long)pgm, q);
			putname(q);
		}

		for (i=0,v=argv; v && i < 20; v++,i++) {
			if (!(p = (char *)get_fs_long(v)))
				break;
			if ((error = getname(p, &q))) {
				printk(KERN_DEBUG "iBCS        arg: 0x%lx bad pointer!\n",
					(unsigned long)p);
			} else {
				printk(KERN_DEBUG "iBCS:       arg: 0x%lx \"%s\"\n",
					(unsigned long)p, q);
				putname(q);
			}
		}
		if (v && p)
			printk(KERN_DEBUG "iBCS:       arg: ...\n");

		for (i=0,v=envp; v && i < 20; v++,i++) {
			if (!(p = (char *)get_fs_long(v)))
				break;
			if ((error = getname(p, &q))) {
				printk(KERN_DEBUG "iBCS        env: 0x%lx bad pointer!\n",
					(unsigned long)p);
			} else {
				printk(KERN_DEBUG "iBCS:       env: 0x%lx \"%s\"\n",
					(unsigned long)p, q);
				putname(q);
			}
		}
		if (v && p)
			printk(KERN_DEBUG "iBCS:       env: ...\n");
	}
#endif

	error = getname(pgm, &filename);
	if (error == 0) {
		/* if you get an error on this undefined, then remove the */
		/* 'static' declaration in /linux/fs/exec.c */
		error = do_execve(filename, argv, envp, regs);
		putname (filename);
        }
	return error;
}

int ibcs_procids(struct pt_regs * regs)
{
	int arg_offset;
	int op = get_fs_long(((unsigned long *)regs->esp)+1);

	/* Remap op codes for current personality if necessary. */
	switch (current->personality) {
		case PER_SVR3:
		case PER_SCOSVR3:
		case PER_WYSEV386:
		case PER_XENIX: {
			if (op < 0 || op > 5)
				return -EINVAL;
			op = "\000\001\005\003\377\377"[op];

			/* SCO at least uses an interesting library to
			 * syscall mapping that leaves an extra return
			 * address between the op code and the arguments.
			 */
			arg_offset = 1;
			break;
		}

		default:
			arg_offset = 0;
	}

	switch (op) {
		case 0: /* getpgrp */
			return current->pgrp;

		case 1: /* setpgrp */
			SYS(setpgid)(0, 0);
 			current->tty=NULL;
			return current->pgrp;

		case 2: { /* getsid */
			int pid = get_fs_long(((unsigned long *)regs->esp)
						+ 2 + arg_offset);
#if __NR_getsid
			return SYS(getsid)(pid);
#else
			if (!pid)
				return current->session;
			return -ESRCH;
#endif
		}

		case 3: /* setsid */
			return SYS(setsid)();

		case 4: { /* getpgid */
			int pid = get_fs_long(((unsigned long *)regs->esp)
						+ 2 + arg_offset);
			return SYS(getpgid)(pid);
		}

		case 5: { /* setpgid */
			int pid, pgid;

			pid = get_fs_long(((unsigned long *)regs->esp)
						+ 2 + arg_offset);
			pgid = get_fs_long(((unsigned long *)regs->esp)
						+ 3 + arg_offset);
			return SYS(setpgid)(pid, pgid);
		}
	}

	return -EINVAL;
}
#endif /* __sparc__ */

int ibcs_read(int fd, char *buf, int nbytes)
{
	int error, here, posn, reclen;
	struct file *file;
	struct dirent *d;
	int old_fs;

	error = SYS(read)(fd, buf, nbytes);
	if (error != -EISDIR)
		return error;

	/* Stupid bloody thing is trying to read a directory. Some old
	 * programs expect this to work. It works on SCO. To emulate it
	 * we have to map a dirent to a direct. This involves shrinking
	 * a long inode to a short. Fortunately nothing this archaic is
	 * likely to care about anything but the filenames of entries
	 * with non-zero inodes. We also assume that anything doing
	 * this doesn't understand names longer than 14 characters
	 * and quietly ignore anything longer.
	 */

	/* sys_read has already done a verify_area and checked the
	 * decriptor number.
	 */
	file = current->FD[fd];

	d = (struct dirent *)get_free_page(GFP_KERNEL);
	if (!d)
		return -ENOMEM;

	error = posn = reclen = 0;

	while (posn + reclen < nbytes) {
		/* Save the current position and get another dirent */
		here = file->f_pos;
		old_fs = get_fs();
		set_fs (get_ds());
		error = SYS(readdir)(fd, d, 1);
		set_fs(old_fs);
		if (error <= 0)
			break;

		/* If it'll fit in the buffer save it otherwise back up
		 * so it is read next time around.
		 * Oh, if we're at the beginning of the buffer there's
		 * no chance that this entry will ever fit so don't
		 * copy it and don't back off - we'll just pretend it
		 * isn't here...
		 */
		reclen = 16 * ((d->d_reclen + 13) / 14);
		if (posn + reclen <= nbytes) {
			/* SCO (at least) handles long filenames by breaking
			 * them up in to 14 character chunks of which all
			 * but the last have the inode set to 0xffff.
			 */
			char *p = d->d_name;

			/* Put all but the last chunk. */
			while (d->d_reclen > 14) {
				put_fs_word(0xffff, buf+posn);
				posn += 2;
				memcpy_tofs(buf+posn, p, 14);
				posn += 14;
				p += 14;
				d->d_reclen -= 14;
			}
			/* Put the last chunk. Note the we have to fold a
			 * long inode number down to a short. Hopefully
			 * nothing uses the inode number!
			 */
#if 0
			/* This appears to match what SCO does for
			 * reads on a directory with long inodes.
			 */
			if ((unsigned long)d->d_ino > 0xfffe)
				put_fs_word(0xfffe, buf+posn);
			else
				put_fs_word((short)d->d_ino, buf+posn);
#else
			/* This attempts to match the way stat and
			 * getdents fold long inodes to shorts.
			 */
			if ((unsigned long)d->d_ino & 0xffff)
				put_fs_word((unsigned long)d->d_ino & 0xffff,
					buf+posn);
			else
				put_fs_word(0xfffe, buf+posn);
#endif
			posn += 2;
			memcpy_tofs(buf+posn, p, d->d_reclen);

			/* Ensure that filenames that don't fill the array
			 * completely are null filled.
			 */
			for (; d->d_reclen < 14; d->d_reclen++)
				put_fs_byte('\0', buf+posn+d->d_reclen);

			posn += 14;
		} else if (posn) {
			SYS(lseek)(fd, here, 0);
		} /* else posn == 0 */
	}

	/* Lose the intermediate buffer. */
	free_page((unsigned long)d);

	/* If we've put something in the buffer return the byte count
	 * otherwise return the error status.
	 */
	return (posn ? posn : error);
}


#ifdef IBCS_TRACE
int ibcs_select(int n, void *rfds, void *wfds, void *efds, struct timeval *t)
{
	if ((ibcs_trace & TRACE_API) || ibcs_func_p->trace) {
		if (t) {
			int error = verify_area(VERIFY_READ, t, sizeof(*t));
			if (error)
				return error;
			printk(KERN_DEBUG "iBCS: select timeout in %lus, %luus\n",
				get_fs_long(&(t->tv_sec)),
				get_fs_long(&(t->tv_usec)));
		}
	}
	return SYS(_newselect)(n, rfds, wfds, efds, t);
}
#endif


int
ibcs_time(void)
{
	return SYS(time)(0);
}


#ifndef __NR_readv /* Around kernel 1.3.31 */

/* This is a rather simplistic implementation... */
int
ibcs_writev(int fd, struct ibcs_iovec *it, int n)
{
	unsigned long addr, len;
	int error = 1;
	int nbytes = 0;

	while (error > 0 && n--) {
		addr = get_fs_long(&it->addr);
		len = get_fs_long(&it->len);
#ifdef IBCS_TRACE
	if ((ibcs_trace & TRACE_API) || ibcs_func_p->trace) {
		printk(KERN_DEBUG "iBCS: buffer 0x%08lx, length 0x%08lx\n",
			addr, len);
	}
#endif
		error = SYS(write)(fd, addr, len);
		if (error > 0)
			nbytes += error;
		if ((unsigned long)error != len)
			error = 0;
		it++;
	}

	return (error < 0 ? error : nbytes);
}

#endif
