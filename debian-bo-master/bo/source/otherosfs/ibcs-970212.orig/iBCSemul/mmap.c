/*
 *  linux/ibcs/mmap.c
 *
 *  Copyright (C) 1994 Eric Youngdale
 *
 */
#include <linux/config.h>

#include <linux/module.h>
#include <linux/version.h>

#include <linux/mm.h>
#include <linux/errno.h>
#include <linux/mman.h>
#include <linux/sched.h>
#include <linux/major.h>

#include <ibcs/ibcs.h>
#include <asm/unistd.h>

#ifdef IBCS_TRACE
#include <ibcs/trace.h>
#endif


int ibcs_mmap(unsigned int vaddr, unsigned int vsize, int prot, int flags,
	      int fd, unsigned int file_offset)
{
#ifdef __sparc__
	/* XXX: Why save personality and restore it? */
	int v, op = current->personality;
   
	v = sunos_mmap(vaddr, vsize, prot, flags, fd, file_offset);
#ifdef IBCS_TRACE
	if ((ibcs_trace & TRACE_API) || ibcs_func_p->trace) {
		printk(KERN_DEBUG "iBCS: sunos_mmap returns 0x%x\n", v);
	}
#endif
	current->personality = op;
	return v;
#else
	struct file * file = NULL;

	if (!(flags & MAP_ANONYMOUS)) {
		if (fd >= NR_OPEN || !(file = current->FD[fd]))
			return -EBADF;
	}
	if (current->personality == PER_SVR4
	&& !(flags & 0x80000000) && vaddr) {
		unsigned int ret;
		ret = do_mmap(file, vaddr, vsize, prot, flags | MAP_FIXED, file_offset);
		return (ret == vaddr ? 0 : ret);
	}

	return do_mmap(file, vaddr, vsize, prot, flags & 0x7fffffff, file_offset);
#endif
}
