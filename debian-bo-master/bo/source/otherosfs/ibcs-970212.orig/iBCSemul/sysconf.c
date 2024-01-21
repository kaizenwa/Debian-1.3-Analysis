/*
 *  linux/ibcs/sysconf.c
 *
 *  Copyright (C) 1994  Mike Jagdis (jaggy@purplet.demon.co.uk)
 *
 * $Id: sysconf.c,v 1.7 1997/01/05 18:19:10 jaggy Exp $
 * $Source: /usr/CVS/ibcs/iBCSemul/sysconf.c,v $
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
#include <linux/limits.h>
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


/* The sysconf() call is supposed to give applications access to various
 * kernel parameters. According to SCO's man page this a POSIX mandated
 * function. Perhaps it should be moved across as a native Linux call?
 *
 * N.B. SCO only has sysconf in the Xenix group. Therefore this is based
 * on the Xenix spec. Is SVR4 the same? Wyse Unix V.3.2.1A doesn't have
 * sysconf documented at all.
 *
 * N.B. 0-7 are required (by who?). Other values may be defined for
 * various systems but there appears no guarantee that they match across
 * platforms. Thus, unless we can identify what system the executable
 * was compiled for, we probably prefer to have extensions fail. Hell,
 * nothing important is going to use this obscure stuff anyway...
 */
#define _SC_ARG_MAX	0
#define _SC_CHILD_MAX	1
#define _SC_CLK_TCK	2
#define _SC_NGROUPS_MAX	3
#define _SC_OPEN_MAX	4
#define _SC_JOB_CONTROL	5
#define _SC_SAVED_IDS	6
#define _SC_VERSION	7

#define _SC_PAGESIZE   11

int ibcs_sysconf(int name) {
	switch (name) {
		case _SC_ARG_MAX: {
			/* From limits.h */
			return (ARG_MAX);
		}

		case _SC_CHILD_MAX: {
			/* From limits.h */
			return (CHILD_MAX);
		}

		case _SC_CLK_TCK: {
			return (HZ);
		}

		case _SC_NGROUPS_MAX: {
			/* From limits.h */
			return (NGROUPS_MAX);
		}

		case _SC_OPEN_MAX: {
			/* From limits.h */
			return (OPEN_MAX);
		}

		case _SC_JOB_CONTROL: {
			return (1);
		}

		case _SC_SAVED_IDS: {
			return (1);
		}

		case _SC_PAGESIZE: {
		  return PAGE_SIZE;
		}

		case _SC_VERSION: {
			/* The version of the POSIX standard we conform
			 * to. SCO defines _POSIX_VERSION as 198808L
			 * sys/unistd.h. What are we?
			 */
			return (198808L);
		}
	}

#ifdef IBCS_TRACE
	if ((ibcs_trace & TRACE_API) || ibcs_func_p->trace) {
		printk(KERN_DEBUG "iBCS2 unsupported sysconf call %d\n", name);
	}
#endif

	return -EINVAL;
}
