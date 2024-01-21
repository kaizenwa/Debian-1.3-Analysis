/*
 *  linux/ibcs/ulimit.c
 *
 *  Copyright (C) 1993  Joe Portman (baron@hebron.connected.com)
 *	 First stab at ulimit
 *
 *  April 9 1994, corrected file size passed to/from setrlimit/getrlimit
 *    -- Graham Adams (gadams@ddrive.demon.co.uk)
 *
 * $Id: ulimit.c,v 1.5 1995/02/13 10:13:20 mike Exp $
 * $Source: /usr/CVS/ibcs/iBCSemul/ulimit.c,v $
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

#include <asm/system.h>
#include <linux/fs.h>
#include <linux/sys.h>
#include <linux/resource.h>

#include <ibcs/ibcs.h>
#include <ibcs/trace.h>

#define U_GETFSIZE 	(1)		  /* get max file size in blocks */
#define U_SETFSIZE 	(2)		  /* set max file size in blocks */
#define U_GETMEMLIM	(3)		  /* get process size limit */
#define U_GETMAXOPEN	(4)		  /* get max open files for this process */
#define U_GTXTOFF		(64)		  /* get text offset */
/*
 * Define nominal block size parameters.
 */
#define ULIM_BLOCKSIZE_BITS   9           /* block size = 512 */
#define ULIM_MAX_BLOCKSIZE (INT_MAX >> ULIM_BLOCKSIZE_BITS)

int
ibcs_ulimit (int cmd, int val)
{
	switch (cmd) {
		case U_GETFSIZE:
			return (current->rlim[RLIMIT_FSIZE].rlim_cur) >>
                                ULIM_BLOCKSIZE_BITS;

		case U_SETFSIZE:
			if ((val > ULIM_MAX_BLOCKSIZE) || (val < 0))
				return -ERANGE;
			val <<= ULIM_BLOCKSIZE_BITS;
			if (val > current->rlim[RLIMIT_FSIZE].rlim_max) {
				if (!suser())
					return -EPERM;
				else {
					current->rlim[RLIMIT_FSIZE].rlim_max = val;
				}
			}
			current->rlim[RLIMIT_FSIZE].rlim_cur = val;
			return 0;

		case U_GETMEMLIM:
			return current->rlim[RLIMIT_DATA].rlim_cur;

		case U_GETMAXOPEN:
			return NR_OPEN;

		default:
#ifdef IBCS_TRACE
			if ((ibcs_trace & TRACE_API) || ibcs_func_p->trace) {
				printk(KERN_DEBUG "iBCS2: unsupported ulimit call %d\n", cmd);
			}
#endif
			return -EINVAL;
	 }
}
