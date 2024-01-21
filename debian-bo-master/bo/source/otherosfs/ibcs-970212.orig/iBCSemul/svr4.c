/*
 *  linux/ibcs/svr4.c
 *
 *  Copyright (C) 1995  Mike Jagdis
 *
 * $Id: svr4.c,v 1.3 1995/11/29 13:05:01 mike Exp $
 * $Source: /usr/CVS/ibcs/iBCSemul/svr4.c,v $
 */

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
#include <ibcs/svr4.h>

#ifdef IBCS_TRACE
#include <ibcs/trace.h>
#endif


/* Interactive SVR4's /bin/sh calls access(... 011) but Linux returns
 * EINVAL if the access mode has any other bits than 007 set.
 */
int
svr4_access(char *path, int mode)
{
	return SYS(access)(path, mode & 007);
}


int svr4_getgroups(int n, unsigned long *buf)
{
	int i;

	if (n) {
		i = verify_area(VERIFY_WRITE, buf, sizeof(unsigned long) * n);
		if (i)
			return i;
	}
	for (i = 0 ; (i < NGROUPS) && (current->groups[i] != NOGROUP) ; i++) {
		if (!n)
			continue;
		if (i >= n)
			break;
		put_fs_long(current->groups[i], buf);
		buf++;
	}
	return(i);
}


int svr4_setgroups(int n, unsigned long *buf)
{
	int i;

	if (!suser())
		return -EPERM;
	if (n > NGROUPS)
		return -EINVAL;
	for (i = 0; i < n; i++, buf++) {
		current->groups[i] = get_fs_long(buf);
	}
	if (i < NGROUPS)
		current->groups[i] = NOGROUP;
	return 0;
}


int svr4_waitid(int idtype, int id, struct siginfo *infop, int options)
{
	long result, kopt;
	int old_fs, pid, status;

	switch (idtype) {
		case 0: /* P_PID */
			pid = id;
			break;

		case 1: /* P_PGID */
			pid = -id;
			break;

		case 7: /* P_ALL */
			pid = -1;
			break;

		default:
			return -EINVAL;
	}

	if (infop) {
		result = verify_area(VERIFY_WRITE, infop,
					sizeof(struct siginfo));
		if (result)
			return result;
	}

	kopt = 0;
	if (options & 0100) kopt |= WNOHANG;
	if (options & 4) kopt |= WUNTRACED;

	old_fs = get_fs();
	set_fs(get_ds());
	result = SYS(wait4)(pid, &status, kopt, NULL);
	set_fs(old_fs);
	if (result < 0)
		return result;

	if (infop) {
		unsigned long op, st;

		put_fs_long(current->exec_domain->signal_map[SIGCHLD],
			&infop->si_signo);
		put_fs_long(result,
			&infop->_data._proc._pid);

		if ((status & 0xff) == 0) {
			/* Normal exit. */
			op = CLD_EXITED;
			st = status >> 8;
		} else if ((status & 0xff) == 0x7f) {
			/* Stopped. */
			st = (status & 0xff00) >> 8;
			op = (st == SIGSTOP || st == SIGTSTP)
				? CLD_STOPPED
				: CLD_CONTINUED;
			st = current->exec_domain->signal_invmap[st];
		} else {
			st = (status & 0xff00) >> 8;
			op = (status & 0200)
				? CLD_DUMPED
				: CLD_KILLED;
			st = current->exec_domain->signal_invmap[st];
		}
		put_fs_long(op, &infop->si_code);
		put_fs_long(st, &infop->_data._proc._pdata._cld._status);
	}
	return 0;
}
