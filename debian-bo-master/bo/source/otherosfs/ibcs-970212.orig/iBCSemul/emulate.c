/*
 *  linux/abi/emulate.c
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
 * $Id: emulate.c,v 1.34 1996/08/23 15:10:00 mike Exp $
 * $Source: /usr/CVS/ibcs/iBCSemul/emulate.c,v $
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

#ifdef EMU_SVR4
#include <ibcs/svr4.h>
#endif

#ifdef IBCS_TRACE
#include <ibcs/trace.h>
static void plist(int id, char *name, char *args, int *list);
static void fail(int id, long eax, char *name);
int ibcs_trace = 0;
int ibcs_id = 0;

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

#include "maps/callmap.inc"

char kernel_version[] = UTS_RELEASE;


static void
iABI_emulate(struct pt_regs * regs)
{
	unsigned long	pers;
	int	i, j, k;
	int	of;
	int	args[8];
	int	rvalue;
	IBCS_func **group_map, *p;
	void	*kfunc;
	short	nargs;
#ifdef IBCS_TRACE
	int	id = ++ibcs_id;
#endif

	/* First decide which personality map we should be looking
	 * at by looking at the personality of this process.
	 */
	pers = current->personality & PER_MASK;
	if (pers > last(iBCS_personality_map)
	|| !(group_map = iBCS_personality_map[pers])) {
#ifdef IBCS_TRACE
		printk(KERN_DEBUG "[%d]%d iBCS: bad personality %lu\n",
			id, current->pid, pers);
#else
		printk(KERN_DEBUG "%d iBCS: bad personality %lu\n",
			current->pid, pers);
#endif
		regs->eflags |= 1; /* Set carry flag */
		regs->eax = iABI_errors(EINVAL);
		return;
	}

	i = regs->eax;
	of = 1;

do_subgroup:
	j = (i & 0xff) >> 3;
	k = i & 0x07;
	if (!group_map[j]) {
		/* This catches NULLs in group maps. It's better to
		 * point the block to the Unused set for tracing rather
		 * than using NULL but we'll trap it anyway.
		 */
#ifdef IBCS_TRACE
		printk(KERN_DEBUG "[%d]%d iBCS: function 0x%lx not supported under personality %ld\n",
			id, current->pid,
			(unsigned long)regs->eax, (unsigned long)pers);
#else
		printk(KERN_DEBUG "%d iBCS: function 0x%lx not supported under personality %lu\n",
			current->pid,
			(unsigned long)regs->eax, (unsigned long)pers);
#endif
		regs->eflags |= 1; /* Set carry flag */
		regs->eax = iABI_errors(EINVAL);
		return;
	}

	/* And finally we can get the function definition */
	p = &(group_map[j][k]);
	ibcs_func_p = p;
	kfunc = p->kfunc;
	nargs = p->nargs;

	/* If it is a subgroup move the pointer to the subgroup, roll
	 * the function number down a byte and try again.
	 */
	if (nargs == Class) {
		group_map = (IBCS_func **)kfunc;
		i >>= 8;
		goto do_subgroup;
	}
	if (nargs == ISC) {
		group_map = (IBCS_func **)kfunc;
		i = get_fs_long(((unsigned long *)regs->esp)+1);
		of = 2;
		goto do_subgroup;
	}

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
			args[i] = get_fs_long(((unsigned long *)regs->esp)+(i+of));

#ifdef IBCS_TRACE
	if ((ibcs_trace & TRACE_API) || p->trace) {
		if (nargs == Spl) {
			for(i=0; i < (int)strlen(p->args); i++)
				args[i] = get_fs_long(((unsigned long *) regs->esp) + (i+of));
		}
		plist(id, p->name, p->args, args);
	}
#endif

	rvalue = -ENOSYS;
	if (kfunc) {
		switch(nargs) {
		case Fast:
			((sysfun_p)kfunc)(regs);
#ifdef IBCS_TRACE
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
				fail(id, regs->eax, p->name);
#else
			fail(regs->eax);
#endif
		}
	} else  {
#ifdef IBCS_TRACE
		if ((ibcs_trace & TRACE_API) || p->trace)
			fail(id, regs->eax, p->name);
#else
		fail(regs->eax);
#endif
	}
	
	if (rvalue >= 0 || rvalue < -ENOIOCTLCMD) {
		regs->eflags &= ~1; /* Clear carry flag */
		regs->eax = rvalue;
#ifdef IBCS_TRACE
		if ((ibcs_trace & TRACE_API) || p->trace) {
			printk(KERN_DEBUG "[%d]%d %s returns %ld {%ld}\n",
				id, current->pid, p->name,
				regs->eax, regs->edx);
		}
#endif
	} else {
		regs->eflags |= 1; /* Set carry flag */
		regs->eax = iABI_errors(-rvalue);
#ifdef IBCS_TRACE
		if ((ibcs_trace & TRACE_API) || p->trace) {
			printk(KERN_DEBUG "[%d]%d %s error return "
#ifdef VERBOSE_ERRORS
				"linux=%d -> ibcs=%ld <%s>\n",
				id, current->pid, p->name,
				rvalue, regs->eax,
				-rvalue < (int)(sizeof(errmsg)/sizeof(errmsg[0]))
					? errmsg[-rvalue]
					: "unknown");
#else
				"linux=%d -> ibcs=%ld\n",
				id, current->pid, p->name,
				rvalue, regs->eax);
#endif
		}
#endif
	}
#ifdef IBCS_TRACE
	if ((ibcs_trace & (TRACE_API|TRACE_SIGNAL))
	&& (current->signal & (~current->blocked))) {
		unsigned long signr = current->signal & (~current->blocked);

		__asm__("bsf %1,%0\n\t"
			:"=r" (signr)
			:"0" (signr));
		printk(KERN_DEBUG "[%d]%d SIGNAL %lu <%s>, queued 0x%08lx\n",
			id, current->pid, signr+1, sig_names[signr], current->signal);
	}
#endif
}


int
ibcs_syscall(struct pt_regs *regs)
{
	regs->eax = get_fs_long(((unsigned long *) regs->esp) + 1);

	++regs->esp;
	iABI_emulate(regs);
	--regs->esp;

	return 0;
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
	unsigned int	i, j, k;
	IBCS_func **group_map, *p;

	if (per > last(iBCS_personality_map)
	|| !(group_map = iBCS_personality_map[per]))
		return -EINVAL;

	i = func;

do_subgroup:
	j = (i & 0xff) >> 3;
	k = i & 0x07;
	if (!group_map[j]) {
		return -EINVAL;
	}

	/* And finally we can get the function definition */
	p = &(group_map[j][k]);

	/* If it is a subgroup move the pointer to the subgroup, roll
	 * the function number down a byte and try again.
	 */
	if (p->nargs == Class) {
		group_map = (IBCS_func **)p->kfunc;
		i >>= 8;
		goto do_subgroup;
	}

	if (val != -1)
		p->trace = val;

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
	printk(KERN_DEBUG "[%d]%d %s(%s)\n",
		id, current->pid, name, arg_buf);
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


#ifdef EMU_BINFMT_OLDSCRIPT
extern struct linux_binfmt ibcs_script_format;
#endif
#ifdef EMU_BINFMT_ELF
extern struct linux_binfmt ibcs_elf_format;
#endif
#ifdef EMU_BINFMT_AOUT
extern struct linux_binfmt ibcs_aout_format;
#endif
#ifdef EMU_BINFMT_COFF
extern struct linux_binfmt coff_format;
#endif
#ifdef EMU_BINFMT_XOUT
extern struct linux_binfmt xout_format;
#endif


#include "maps/signal.inc"

struct exec_domain ibcs_exec_domain = {
	"iBCS2",
	iABI_emulate,
	1, 4,
	ibcs_to_linux_signals,
	linux_to_ibcs_signals,
	&mod_use_count_,
	NULL
};
struct exec_domain xnx_exec_domain = {
	"Xenix",
	iABI_emulate,
	7, 7,
	xnx_to_linux_signals,
	linux_to_xnx_signals,
	&mod_use_count_,
	NULL
};

#ifdef EMU_ISC
struct exec_domain isc_exec_domain = {
	"ISC",
	iABI_emulate,
	5, 5,
	isc_to_linux_signals,
	linux_to_isc_signals,
	&mod_use_count_,
	NULL
};
#endif

#ifdef EMU_BSD
struct exec_domain bsd_exec_domain = {
	"BSD",
	iABI_emulate,
	6, 6,
	bsd_to_linux_signals,
	linux_to_bsd_signals,
	&mod_use_count_,
	NULL
};
#endif


int
init_module(void)
{
#ifdef __NR_getdents
	register_symtab(0);
#endif

	/* Register the socksys socket interface to streams based TCP/IP. */
	init_socksys();
	
#ifdef EMU_BINFMT_OLDSCRIPT
	register_binfmt(&ibcs_script_format);
#endif
#ifdef EMU_BINFMT_ELF
	register_binfmt(&ibcs_elf_format);
#endif
#ifdef EMU_BINFMT_COFF
	register_binfmt(&coff_format);
#endif
#ifdef EMU_BINFMT_XOUT
	register_binfmt(&xout_format);
#endif
#ifdef EMU_BINFMT_AOUT
	register_binfmt(&ibcs_aout_format);
#endif
	register_exec_domain(&ibcs_exec_domain);
	register_exec_domain(&xnx_exec_domain);
#ifdef EMU_ISC
	register_exec_domain(&isc_exec_domain);
#endif
#ifdef EMU_BSD
	register_exec_domain(&bsd_exec_domain);
#endif
	return 0;
}


void
cleanup_module(void)
{
	if (MOD_IN_USE)
		printk(KERN_INFO "iBCS: module is in use, remove delayed\n");

#ifdef EMU_BINFMT_ELF
	unregister_binfmt(&ibcs_elf_format);
#endif
#ifdef EMU_BINFMT_COFF
	unregister_binfmt(&coff_format);
#endif
#ifdef EMU_BINFMT_XOUT
	unregister_binfmt(&xout_format);
#endif
#ifdef EMU_BINFMT_AOUT
	unregister_binfmt(&ibcs_aout_format);
#endif
#ifdef EMU_BINFMT_OLDSCRIPT
	unregister_binfmt(&ibcs_script_format);
#endif

	unregister_exec_domain(&ibcs_exec_domain);
	unregister_exec_domain(&xnx_exec_domain);
#ifdef EMU_ISC
	unregister_exec_domain(&isc_exec_domain);
#endif
#ifdef EMU_BSD
	unregister_exec_domain(&bsd_exec_domain);
#endif

	/* Remove the socksys socket interface to streams based TCP/IP */
	cleanup_socksys();
}
