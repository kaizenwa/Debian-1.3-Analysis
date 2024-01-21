/*
 *  linux/ibcs/sysfs.c
 *
 *  Copyright (C) 1994  Mike Jagdis (jaggy@purplet.demon.co.uk)
 *
 * $Id: sysfs.c,v 1.10 1995/02/13 10:13:18 mike Exp $
 * $Source: /usr/CVS/ibcs/iBCSemul/sysfs.c,v $
 */

#include <linux/config.h>

#include <linux/module.h>
#include <linux/version.h>

#include <asm/segment.h>
#ifndef KERNEL_DS
#include <linux/segment.h>
#endif

#include <linux/errno.h>
#include <linux/sched.h>
#include <linux/kernel.h>
#include <linux/mm.h>
#include <linux/stddef.h>
#include <linux/unistd.h>
#include <linux/ptrace.h>
#include <linux/fcntl.h>

#include <asm/system.h>
#include <linux/fs.h>
#include <linux/sys.h>
#include <linux/string.h>

#include <ibcs/ibcs.h>

/* Kernels 1.1.10 and later have sysfs as a system call (checked by looking
 * for __NR_sysfs), kernels prior to 1.1.9 used a static array for the
 * filesystem table, 1.1.9 and later use a linked list (checked for by
 * looking for BUF_CLEAN from fs.h). We support them all...
 */

#ifndef __NR_sysfs

#ifdef IBCS_TRACE
#include <ibcs/trace.h>
#endif


/* Declared in linux/fs/super.c. */
#if BUF_CLEAN
extern struct file_system_type *file_systems;
#else
extern struct file_system_type file_systems[];
#endif


#define GETFSIND	1
#define GETFSTYP	2
#define GETNFSTYP	3


int ibcs_sysfs(struct pt_regs * regs) {
	int	cmd;
	struct file_system_type *fs;
	int error;
	int n_fs;

	cmd = get_fs_long(((unsigned long *) regs->esp) + 1);

	if (cmd == GETFSIND) {
		char *fsname, *kfsname;

		fsname = (char *)get_fs_long(((unsigned long *) regs->esp) + 2);
		error = getname(fsname, &kfsname);
		if (error)
			return (error);
		n_fs = 1;
#if BUF_CLEAN
		for (fs=file_systems; fs; fs=fs->next,n_fs++)
#else
		for (fs=file_systems; fs->name; fs++,n_fs++)
#endif
			if (!strcmp(fs->name, kfsname)) {
				putname(kfsname);
				return (n_fs);
			}
		putname(kfsname);
		return (-EINVAL);
	}

	n_fs = 0;
#if BUF_CLEAN
	for (fs=file_systems; fs; fs=fs->next,n_fs++);
#else
	for (fs=file_systems; fs->name; fs++,n_fs++);
#endif

	if (cmd == GETNFSTYP) {
		return (n_fs);
	}

	if (cmd == GETFSTYP) {
		int fs_ind;
		char *buf;
		char *p;

		fs_ind = get_fs_long(((unsigned long *) regs->esp) + 2) - 1;
#if BUF_CLEAN
		if (fs_ind < 0 || fs_ind >= n_fs)
			return (-EINVAL);
#else
		if (fs_ind < 0 || fs_ind >= n_fs
		|| !file_systems[fs_ind].name)
			return (-EINVAL);
#endif

#if BUF_CLEAN
		for (fs=file_systems; fs_ind && fs; fs=fs->next,fs_ind--);
		if (!fs)
			return (-EINVAL);
#else
		fs = &(file_systems[fs_ind]);
#endif

		buf = (char *)get_fs_long(((unsigned long *) regs->esp) + 3);

		error = verify_area(VERIFY_WRITE, buf, strlen(fs->name));
		if (error)
			return error;

		p = fs->name;
		do {
			put_fs_byte(*p, buf++);
		} while (*p++);

		return (0);
	}

#ifdef IBCS_TRACE
	if ((ibcs_trace & TRACE_API) || ibcs_func_p->trace) {
		printk(KERN_DEBUG "iBCS2 unsupported sysfs call %d\n", cmd);
	}
#endif

	return -EINVAL;
}
#endif /* __NR_sysfs */
