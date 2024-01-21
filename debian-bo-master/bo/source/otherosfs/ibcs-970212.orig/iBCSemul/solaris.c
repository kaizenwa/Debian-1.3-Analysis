/*
 *  linux/abi/solaris.c
 *
 *  Copyright (C) 1993  Linus Torvalds
 *   Modified by Eric Youngdale to include all ibcs syscalls.
 *   Re-written by Drew Sullivan to handle lots more of the syscalls correctly.
 *
 *   Jan 30 1994, Merged Joe Portman's code -- Drew
 *   Jan 31 1994, Merged Eric Yongdale's code for elf support -- Drew
 *
 *   Feb 4 1994
 *     Rebuilt with handling for multiple binary personalities
 *     -- Mike Jagdis (jaggy@purplet.demon.co.uk)
 *
 *  Feb 14 1994
 *     Dual mode. Compiled in if you say yes to the configure iBCS
 *     question during 'make config'. Loadable module with kernel
 *     hooks otherwise.
 *     -- Mike Jagdis (jaggy@purplet.demon.co.uk)
 *
 *  Feb 18 1994
 *     Added the /dev/socksys emulator. This allows applications which
 *     use the socket interface to Lachman streams based TCP/IP to use
 *     the Linux TCP/IP stack.
 *     -- Mike Jagdis (jaggy@purplet.demon.co.uk)
 *
 *  Jun, 1996
 *     Linux/SPARC Solaris emulation
 *     Separated from main program to avoid ifdef mania.
 *
 * $Id: solaris.c,v 1.2 1996/08/23 15:10:05 mike Exp $
 * $Source: /usr/CVS/ibcs/iBCSemul/solaris.c,v $
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
#include <linux/personality.h>
#include <linux/binfmts.h>

#include <ibcs/bsd.h>
#include <ibcs/ibcs.h>
#include <ibcs/abi4.h>
#include <ibcs/xnx.h>

#include <ibcs/svr4.h>
#if 0
/* Huh??? I can't find an asm/svr4.h anywhere. */
#include <asm/svr4.h>
#endif

#ifdef IBCS_TRACE
#include <ibcs/trace.h>
static void plist(int id, char *name, char *args, int *list);
static void fail(int id, long eax, char *name);
int ibcs_trace = 0;
int ibcs_id = 0;

int svr4_sigpending (int which_routine, svr4_sigset_t *set);
int svr4_context (struct pt_regs *regs);
int svr4_gettimeofday (void *x, void *y);
int do_sigsuspend (struct pt_regs *regs);

static char *sig_names[] = {
	"SIGHUP",	"SIGINT",	"SIGQUIT",	"SIGILL",
	"SIGTRAP",	"SIGABRT/SIGIOT","SIGUNUSED",	"SIGFPE",
	"SIGKILL",	"SIGUSR1",	"SIGSEGV",	"SIGUSR2",
	"SIGPIPE",	"SIGALRM",	"SIGTERM",	"SIGSTKFLT",
	"SIGCHLD",	"SIGCONT",	"SIGSTOP",	"SIGTSTP",
	"SIGTTIN",	"SIGTTOU",	"SIGIO/SIGPOLL/SIGURG",
	"SIGXCPU",	"SIGXFSZ",	"SIGVTALRM",	"SIGPROF",
	"SIGWINCH",	"SIGLOST",	"SIGPWR",	"SIG 31",
	"SIG 32"
};

#ifdef VERBOSE_ERRORS
#  include "maps/verberr.inc"
#endif /* VERBOSE_ERRORS */

#else /* IBCS_TRACE */

static void fail(long eax);

#endif /* IBCS_TRACE */

IBCS_func *ibcs_func_p;


/* This table contains the appropriate kernel routines that can be run
 * to perform the syscalls in question.  If an entry is 'Ukn' we don't
 * know how to handle it yet. (We also set trace on by default for these)
 * Spl means that we need to do special processing for this syscall
 *	(see ibcs_wait or ibcs_getpid)
 * Fast means that even the error return handling is done by the function call.
 */
#define ZERO	64	/* Um, magic zero for callmap. Don't ask :-). */
#define Spl	65	/* pass the regs structure down */
#define Ukn	66	/* no code to handle this case yet */
#define Fast	67	/* magic on return, return regs structure already set up */
#define Class	68	/* is a pointer to a subclass */
#define ISC	69	/* is a pointer to a sysisc subclass */
			/* subfun # is first argument on stack */

#define last(x)	((sizeof(x)/sizeof(*x))-1)

#include "maps/callsol.inc"

char kernel_version[] = UTS_RELEASE;

#define ELEMENTS(x) (sizeof (x) / sizeof (x[0]))
unsigned int the_pc;

static void
solaris_emulate(struct pt_regs * regs)
{
	int	i, syscall;
	int	args[8];
	int	rvalue;
	IBCS_func *p;
	void	*kfunc;
	short	nargs;
#ifdef IBCS_TRACE
	int	id = ++ibcs_id;
#endif

        syscall = regs->u_regs [UREG_G1];
#if 0
        printk ("Processing %d: ", syscall);
#endif
	if (syscall > ELEMENTS(Solaris_funcs)){
		send_sig(SIGSEGV, current, 1);
                return;
	}
#if 0
	printk ("%s\n", Solaris_funcs [syscall].name);
#endif	
	/* And finally we can get the function definition */
	p = ibcs_func_p = &Solaris_funcs [syscall];
	kfunc = Solaris_funcs [syscall].kfunc;
	nargs = Solaris_funcs [syscall].nargs;

	/* If the number of arguments is negative this is an unfudged
	 * system call function and we need to look up the real function
	 * address in the kernel's sys_call_table.
	 * Note that we never modify the callmap itself but do the lookup
	 * for each call. This allows modules that provide syscalls to
	 * be loaded and unloaded without any messy locking.
	 */
	if (nargs < 0) {
		kfunc = sys_call_table[(int)kfunc];

		/* Watch for a magic zero. This exists because we
		 * can't use -0 to represent a system call that
		 * takes no arguments.
		 */
		if (nargs == -ZERO)
			nargs = 0;
		else
			nargs = -nargs;
	}

	if (nargs <= (short)(sizeof(args)/sizeof(args[0])))
		for(i=0; i < nargs; i++)
			args[i] = get_syscall_parameter (regs, i);

#ifdef IBCS_TRACE
	if ((ibcs_trace & TRACE_API) || p->trace) {
		if (nargs == Spl) {
			for(i=0; i < (int)strlen(p->args); i++)
				args[i] = get_syscall_parameter (regs, i);
		}
	        the_pc = regs->u_regs [15];
		plist(id, p->name, p->args, args);
	}
#endif

	rvalue = -ENOSYS;
	if (kfunc) {
		switch(nargs) {
		case Fast:
			((sysfun_p)kfunc)(regs);
#if defined(IBCS_TRACE) && defined(__i386__)
			if ((ibcs_trace & (TRACE_API|TRACE_SIGNAL))
			&& (current->signal & (~current->blocked))) {
				unsigned long signr = current->signal & (~current->blocked);

				__asm__("bsf %1,%0\n\t"
					:"=r" (signr)
					:"0" (signr));
				printk(KERN_DEBUG "[%d]%d SIGNAL %lu <%s>\n",
					id, current->pid,
					signr+1, sig_names[signr]);
			}
#endif
			return;
		case Spl:
			rvalue = ((sysfun_p)kfunc)(regs);
		   #if 0
			printk ("Spl called, returning: %d\n", rvalue);
		   #endif
			break;
		case 0:
			rvalue = ((sysfun_p)kfunc)();
			break;
		case 1:
			rvalue = ((sysfun_p)kfunc)(args[0]);
			break;
		case 2:
			rvalue = ((sysfun_p)kfunc)(args[0], args[1]);
			break;
		case 3:
			rvalue = ((sysfun_p)kfunc)(args[0], args[1], args[2]);
			break;
		case 4:
			rvalue = ((sysfun_p)kfunc)(args[0], args[1], args[2], args[3]);
			break;
		case 5:
			rvalue = ((sysfun_p)kfunc)(args[0], args[1], args[2],
					     args[3], args[4]);
			break;
		case 6:
			rvalue = ((sysfun_p)kfunc)(args[0], args[1], args[2],
					     args[3], args[4], args[5]);
			break;
		default:
#ifdef IBCS_TRACE
			if ((ibcs_trace & TRACE_API) || p->trace)
				fail(id, get_result (regs), p->name);
#else
			fail(get_result (regs));
#endif
		}
	} else  {
#ifdef IBCS_TRACE
		if ((ibcs_trace & TRACE_API) || p->trace)
			fail(id, get_result (regs), p->name);
#else
		fail(get_restult (regs));
#endif
	}
#ifdef IBCS_TRACE
	printk(KERN_DEBUG "[%d]%d %s returns %d {%d}\n",
	       id, current->pid, p->name,
	       rvalue, rvalue);
#endif
        set_result (regs, rvalue);
}


#ifdef IBCS_TRACE
int ibcs_trace_set(int arg)
{
	if (arg != -1) {
		ibcs_trace = arg;
		printk(KERN_DEBUG "iBCS: trace code set to 0x%x\n", ibcs_trace);
	}

	return ibcs_trace;
}

int ibcs_trace_func(unsigned int per, int func, int val)
{
	unsigned int	i;
	IBCS_func *p;

	i = func;

        if (func > ELEMENTS(Solaris_funcs)){
	    return -EINVAL;
	}
	/* And finally we can get the function definition */
	p = &Solaris_funcs[i];

	/* If it is a subgroup move the pointer to the subgroup, roll
	 * the function number down a byte and try again.
	 */
	if (val != -1)
		p->trace = val;

        printk ("Setting debugging to: %x\n", p->trace);
	return p->trace;
}

/*
 * plist is used by the trace code to show the arg list
 */
static void plist(int id, char *name, char *args, int *list) {
	int	error;
	char	*tmp, *p, arg_buf[512];

	arg_buf[0] = '\0';
	p = arg_buf;
	while (*args) {
		switch(*args++) {
		case 'd': sprintf(p, "%d", *list++);		break;
		case 'o': sprintf(p, "0%o", *list++);		break;
		case 'p': sprintf(p, "0x%p", (void *)(*list++));	break;
		case '?': 
		case 'x': sprintf(p, "0x%x", *list++);		break;
		case 's': 
			error = getname((char *)(*list++),&tmp);
			if (!error) {
				/* we are debugging, we don't need to see it all */
				tmp[80] = '\0';
				sprintf(p, "\"%s\"", tmp);
				putname(tmp);
			}
			break;
		default:
			sprintf(p, "?%c%c?", '%', args[-1]);
			break;
		}
		while (*p) ++p;
		if (*args) {
			*p++ = ',';
			*p++ = ' ';
			*p = '\0';
		}
	}
	printk(KERN_DEBUG "[%d]%d %8.8x %s(%s)\n", 
		id, current->pid, the_pc, name, arg_buf);
}

static void fail(int id, long eax, char *name) {
	printk(KERN_ERR "[%d]%d Unsupported iBSC2 function 0x%lx(%s)\n",
		id, current->pid, eax, name);
}
#else /* IBCS_TRACE */
static void fail(long eax) {
	printk(KERN_ERR "Unsupported iBCS function 0x%lx\n", eax);
}
#endif /* IBCS_TRACE */


#include "maps/signal.inc"

struct exec_domain solaris_exec_domain = {
	"Solaris",
	solaris_emulate,
	1, 4,
	ibcs_to_linux_signals,
	linux_to_ibcs_signals,
	&mod_use_count_,
	NULL
};


int
init_module(void)
{
	register_symtab(0);

	/* Register the socksys socket interface to streams based TCP/IP. */
	init_socksys();
	
	register_exec_domain(&solaris_exec_domain);
	return 0;
}


void
cleanup_module(void)
{
	if (MOD_IN_USE)
		printk(KERN_INFO "iBCS: module is in use, remove delayed\n");

	/* Remove the COFF and ELF loaders */
	unregister_exec_domain (&solaris_exec_domain);
	/* Remove the socksys socket interface to streams based TCP/IP */
	cleanup_socksys();
}

int
svr4_sigpending (int which_routine, svr4_sigset_t *set)
{
	/* Solaris multiplexes on this one */
	/* Which routine has the actual routine that should be called */

	switch (which_routine){
	case 1:			/* sigpending */
		printk ("iBCS/Sparc: sigpending not implemented\n");
		return -EINVAL;
		
	case 2:			/* sigfillset */
		set->sigbits [0] = ~0;
		set->sigbits [1] = 0;
		set->sigbits [2] = 0;
		set->sigbits [3] = 0;
		return 0;
	}
	return -EINVAL;
}

extern int
svr4_setcontext (svr4_ucontext_t *c, struct pt_regs *regs);

extern int
svr4_getcontext (svr4_ucontext_t *c, struct pt_regs *regs);

int
svr4_context (struct pt_regs *regs)
{
	int context_fn = get_syscall_parameter (regs, 0);
	struct svr4_ucontext_t *uc = (void *) get_syscall_parameter (regs, 1);
        int fn;
   
	switch (context_fn){
	case 0: /* getcontext */
	        printk ("Getting context\n");
		fn = svr4_getcontext (uc, regs);
	        return fn;

	case 1: /* setcontext */
		printk ("Setting context\n");
		return svr4_setcontext (uc, regs);
	}
	return -EINVAL;
}

int svr4_gettimeofday (void *x, void *y)
{
    printk ("Inside gettimeofday\n");
    return ((sysfun_p)(sys_call_table [116])) (x, y);
}
