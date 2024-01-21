/*
 *  Function prototypes used for SVR4 emulation.
 *
 * $Id: svr4.h,v 1.1 1995/03/21 11:11:11 mike Exp $
 * $Source: /usr/CVS/ibcs/include/ibcs/svr4.h,v $
 */
#include <linux/ptrace.h>	/* for pt_regs */
#include <linux/sched.h>
#include <linux/signal.h>
#include <linux/unistd.h>

/* svr4.c */
extern int svr4_getgroups(int n, unsigned long *buf);
extern int svr4_setgroups(int n, unsigned long *buf);
extern int svr4_waitsys(struct pt_regs * regs);
