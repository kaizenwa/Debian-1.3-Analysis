/*
 *  linux/ibcs/syslocal.c
 *
 *  Copyright (C) 1994  Mike Jagdis (jaggy@purplet.demon.co.uk)
 *
 * $Id: syslocal.c,v 1.3 1996/07/26 12:03:39 mike Exp $
 * $Source: /usr/CVS/ibcs/iBCSemul/syslocal.c,v $
 */

#include <linux/config.h>

#include <linux/module.h>
#include <linux/version.h>

#include <asm/segment.h>
#ifndef KERNEL_DS
#include <linux/config.h>
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

#include <ibcs/ibcs.h>

#ifdef IBCS_TRACE
#include <ibcs/trace.h>
#endif


/* The syslocal() call is used for machine specific functions. For
 * instance on a Wyse 9000 it give information and control of the
 * available processors.
 */

#ifdef EMU_WYSEMP
#  define SL_ONLINE	0	/* Turn processor online */
#  define SL_OFFLINE	1	/* Turn processor offline */
#  define SL_QUERY	2	/* Query processor status */
#  define SL_NENG	3	/* Return No. of processors configured */
#  define SL_AFFINITY	4	/* processor binding */
#  define SL_CMC_STAT	7	/* gather CMC performance counters info */
#  define SL_KACC	8	/* make kernel data readable by user */
#  define SL_MACHTYPE	9	/* return machine type (MP/AT) */
#  define SL_BOOTNAME	10	/* return name of booted kernel */
#  define SL_BOOTDEV	11	/* return type of booted device */
#  define SL_UQUERY	12	/* query user status */

#  define SL_MACH_MP	0
#  define SL_MACH_AT	1
#  define SL_MACH_EISA	2
#  define SL_MACH_EMP	3
#endif


int
ibcs_syslocal(struct pt_regs * regs) {
	int	cmd;

	cmd = get_syscall_parameter (regs, 0);

	switch (cmd) {
#ifdef EMU_WYSEMP
		case SL_QUERY:
			return 0;

		case SL_NENG:
			return 1;

		case SL_MACHTYPE:
			return (EISA_bus ? SL_MACH_EISA : SL_MACH_AT);
#endif
	}

#ifdef IBCS_TRACE
	if ((ibcs_trace & TRACE_API) || ibcs_func_p->trace) {
		printk(KERN_DEBUG "iBCS2: unsupported syslocal call %d\n", cmd);
	}
#endif

	return -EINVAL;
}
