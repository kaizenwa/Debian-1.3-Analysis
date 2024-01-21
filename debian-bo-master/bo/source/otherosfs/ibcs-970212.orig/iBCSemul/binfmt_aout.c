/*
 *  Copyright 1995-1996  Mike Jagdis (jaggy@purplet.demon.co.uk)
 *
 * $Id: binfmt_aout.c,v 1.3 1996/03/29 17:33:23 mike Exp $
 * $Source: /usr/CVS/ibcs/iBCSemul/binfmt_aout.c,v $
 *
 * Derived from original Linux code:
 *
 *    linux/fs/exec.c
 *
 *    Copyright (C) 1991, 1992  Linus Torvalds
 */
#include <linux/config.h>

#include <linux/module.h>
#include <linux/version.h>

#include <asm/segment.h>
#ifndef KERNEL_DS
#include <linux/segment.h>
#endif

#include <linux/fs.h>
#include <linux/sched.h>
#include <linux/kernel.h>
#include <linux/mm.h>
#include <linux/mman.h>
#include <linux/a.out.h>
#include <linux/errno.h>
#include <linux/signal.h>
#include <linux/string.h>
#include <linux/stat.h>
#include <linux/fcntl.h>
#include <linux/ptrace.h>
#include <linux/user.h>
#include <linux/malloc.h>
#include <linux/binfmts.h>
#include <linux/personality.h>
#include <linux/in.h> /* for ntohl() */

#include <asm/system.h>
#include <asm/pgtable.h>

#include <ibcs/ibcs.h>

#ifdef ELF_TRACE
#include <ibcs/trace.h>
#endif

#include "binfmt_lib.h"


static int load_aout_binary(struct linux_binprm *, struct pt_regs * regs);
static int load_aout_library(int fd);


#ifdef INIT_MM
struct linux_binfmt ibcs_aout_format = {
	NULL, &mod_use_count_, load_aout_binary, load_aout_library, NULL
};
#else
struct linux_binfmt ibcs_aout_format = {
	load_aout_binary, load_aout_library
};
#endif


static int load_aout_binary(struct linux_binprm * bprm, struct pt_regs * regs)
{
	struct exec ex;
	struct file * file;
	int fd, error;
	unsigned long p = bprm->p;
	unsigned long fd_offset, vm_offset;
	unsigned long pers;
	unsigned long rlim;

	MOD_INC_USE_COUNT;

	ex = *((struct exec *) bprm->buf);

	/* Basic sanity checks. There should be no relocation information
	 * and the size of the file must be greater than the total text
	 * plus data size.
	 */
	if (ex.a_trsize || ex.a_drsize
	|| ex.a_text + ex.a_data > bprm->inode->i_size) {
		MOD_DEC_USE_COUNT;
		return -ENOEXEC;
	}

	/* If the machine type is not 100 assume a BSD flavour, Linux uses
	 * M_386 (100) as the machine type except on very old binaries
	 * which can easily be updated using the lnxstamp program supplied
	 * as part of the iBCS/BSD emulator.
	 * If the machine type is 100 then we return ENOEXEC and let the
	 * kernel a.out loader handle it. This avoids us being marked
	 * in use for every Linux a.out program that is run (which makes
	 * unloading the iBCS module very difficult). This isn't perfect
	 * but should be good enough...
	 */
	if (N_MACHTYPE(ex) == 100) {
		MOD_DEC_USE_COUNT;
		return -ENOEXEC;
	}
	pers = PER_BSD;

	/* Set up the file and vm offsets based on the type of
	 * executable.
	 */
	switch (ex.a_info & 0xffff) {
		case ZMAGIC: /* demand paged executable */
			/* Alignment varies. If there is no symbol table
			 * we can work out the offset to the text start
			 * easily. If there is a symbol table we would
			 * have to walk it to find out the size of the
			 * string pool so in this case we guess at the
			 * "standard" Linux alignment. Alignment doesn't
			 * imply personality is known though...
			 */
			if (ex.a_text == 0) {
				/* Bill's 386bsd used a code-in-data kludge
				 * on the boot disk.
				 */
				fd_offset = 0;
			} else if (ex.a_syms == 0) {
				fd_offset = (bprm->inode->i_size
						- ex.a_text - ex.a_data)
					& (~15);
			} else {
				fd_offset = N_TXTOFF(ex);
			}
			vm_offset = 0;
			break;

		case QMAGIC: /* demand paged with page 0 unmapped */
			fd_offset = 0;
			vm_offset = PAGE_SIZE;
			break;

		case OMAGIC: /* impure executable */
			fd_offset = 32;
			vm_offset = 0;
			break;

		default:
			/* NetBSD puts the info field in network byte
			 * order instead of host order. Why? It also
			 * treats ZMAGIC and QMAGIC identically as
			 * regards leaving page 0 unmapped.
			 */
			switch (ntohl(ex.a_info) & 0xffff) {
				case ZMAGIC:
				case QMAGIC:
					fd_offset = 0;
					vm_offset = PAGE_SIZE;
					break;

				default:
					MOD_DEC_USE_COUNT;
					return -ENOEXEC;
			}
	}

	/* Check initial limits. This avoids letting people circumvent
	 * size limits imposed on them by creating programs with large
	 * arrays in the data or bss.
	 */
	rlim = current->rlim[RLIMIT_DATA].rlim_cur;
	if (rlim >= RLIM_INFINITY)
		rlim = ~0;
	if (ex.a_data + ex.a_bss > rlim) {
		MOD_DEC_USE_COUNT;
		return -ENOMEM;
	}

	/* OK, This is the point of no return */
	flush_old_exec(bprm);

	current->mm->brk = ex.a_bss +
		(current->mm->start_brk =
		(current->mm->end_data = ex.a_data +
		(current->mm->end_code = ex.a_text +
		(current->mm->start_code = vm_offset))));
	current->mm->rss = 0;
	current->mm->mmap = NULL;
 	current->suid = current->euid = current->fsuid = bprm->e_uid;
 	current->sgid = current->egid = current->fsgid = bprm->e_gid;
#ifdef PF_FORKNOEXEC
	current->flags &= ~PF_FORKNOEXEC;
#endif

	/* If the file offset is not on a filesystem block boundary treat
	 * the file as impure and slurp the lot in. Theory says we should
	 * able to slurp in the odd bits at the start and end of text and
	 * data segments and mmap the blocks in between. This might be useful
	 * for the cases where filesystems with differing block sizes are
	 * in use or the programmer is just thick. It isn't implemented
	 * yet however.
	 *   Also slurp the data if the filesystem doesn't support mmap.
	 */
	fd = open_inode(bprm->inode, O_RDONLY);
	if (fd < 0) {
		/* Too late! We're doomed... */
		send_sig(SIGSEGV, current, 0);
		MOD_DEC_USE_COUNT;
		return fd;
	}
	file = current->files->fd[fd];
	if (fd_offset % bprm->inode->i_sb->s_blocksize
	|| !file->f_op || !file->f_op->mmap) {
#ifndef STACK_TOP
		int old_fs;
#endif
		SYS(close)(fd);
		do_mmap(NULL, vm_offset, ex.a_text+ex.a_data,
			PROT_READ|PROT_WRITE|PROT_EXEC,
			MAP_FIXED|MAP_PRIVATE, 0);
#ifdef STACK_TOP
		read_exec(bprm->inode, fd_offset, (char *)vm_offset,
			ex.a_text+ex.a_data, 0);
#else
		old_fs = get_fs();
		set_fs(get_ds());
		read_exec(bprm->inode, fd_offset, (char *)vm_offset,
			ex.a_text+ex.a_data);
		set_fs(old_fs);
#endif
	} else {
		/* Don't forget that Bill's 386bsd data-in-text kludge
		 * won't have any text.
		 */
		if (ex.a_text) {
			error = do_mmap(file, vm_offset, ex.a_text,
				PROT_READ | PROT_EXEC,
				MAP_FIXED | MAP_PRIVATE | MAP_DENYWRITE | MAP_EXECUTABLE,
				fd_offset);
 
			if (error != vm_offset) {
				SYS(close)(fd);
				send_sig(SIGSEGV, current, 0);
				return -EINVAL;
			}
		}
		
		/* N.B. Linux makes data executable. BSD doesn't (unless
		 * using Bill's code-in-data kludge).
		 */
 		error = do_mmap(file, vm_offset + ex.a_text, ex.a_data,
				PROT_READ | PROT_WRITE | PROT_EXEC,
				MAP_FIXED | MAP_PRIVATE | MAP_DENYWRITE | MAP_EXECUTABLE,
				fd_offset + ex.a_text);
		SYS(close)(fd);
		if (error != vm_offset + ex.a_text) {
			send_sig(SIGSEGV, current, 0);
			MOD_DEC_USE_COUNT;
			return -EINVAL;
		}
	}

	if (PAGE_ALIGN(current->mm->brk) > PAGE_ALIGN(current->mm->start_brk))
		do_mmap(NULL, PAGE_ALIGN(current->mm->start_brk),
			PAGE_ALIGN(current->mm->brk)
				- PAGE_ALIGN(current->mm->start_brk),
			PROT_READ | PROT_WRITE | PROT_EXEC,
			MAP_FIXED | MAP_PRIVATE, 0);

	if (current->exec_domain && current->exec_domain->use_count)
		(*current->exec_domain->use_count)--;
	if (current->binfmt && current->binfmt->use_count)
		(*current->binfmt->use_count)--;
	current->personality = pers;
	current->exec_domain = lookup_exec_domain(current->personality);
	current->binfmt = &ibcs_aout_format;
	if (current->exec_domain && current->exec_domain->use_count)
		(*current->exec_domain->use_count)++;
	if (current->binfmt && current->binfmt->use_count)
		(*current->binfmt->use_count)++;

#if LINUX_VERSION_CODE < 66375
	p += setup_arg_pages(ex.a_text,bprm->page);
	p -= MAX_ARG_PAGES*PAGE_SIZE;
#ifdef __NR_getsid
	p = (unsigned long)create_tables((char *)p, bprm,
					current->personality != PER_LINUX);
#else
	p = (unsigned long)create_tables((char *)p,
					bprm->argc, bprm->envc,
					current->personality != PER_LINUX);
#endif
#else
	p = setup_arg_pages(p, bprm);
	p = (unsigned long)create_ibcs_tables((char *)p, bprm,
				current->personality != PER_LINUX);
#endif

	current->mm->start_stack = p;
	start_thread(regs, ex.a_entry, p);
	if (current->flags & PF_PTRACED)
		send_sig(SIGTRAP, current, 0);
	MOD_DEC_USE_COUNT;
	return 0;
}


static int load_aout_library(int fd)
{
        struct file * file;
	struct exec ex;
	struct  inode * inode;
	unsigned int len;
	unsigned int bss;
	unsigned int start_addr;
	int error;

	MOD_INC_USE_COUNT;

	if (SYS(lseek)(fd, 0, 0) != 0) {
		MOD_DEC_USE_COUNT;
		return -ENOEXEC;
	}

	file = current->files->fd[fd];
	inode = file->f_inode;
	
	set_fs(KERNEL_DS);
	error = file->f_op->read(inode, file, (char *) &ex, sizeof(ex));
	set_fs(USER_DS);
	if (error != sizeof(ex)) {
		MOD_DEC_USE_COUNT;
		return -ENOEXEC;
	}
	
	/* We come in here for the regular a.out style of shared libraries */
	if ((N_MAGIC(ex) != ZMAGIC && N_MAGIC(ex) != QMAGIC) || ex.a_trsize ||
	    ex.a_drsize || ((ex.a_entry & 0xfff) && N_MAGIC(ex) == ZMAGIC) ||
	    inode->i_size < ex.a_text+ex.a_data+ex.a_syms+N_TXTOFF(ex)) {
		MOD_DEC_USE_COUNT;
		return -ENOEXEC;
	}
	if (N_MAGIC(ex) == ZMAGIC && N_TXTOFF(ex) && 
	    (N_TXTOFF(ex) < inode->i_sb->s_blocksize)) {
		printk("N_TXTOFF < BLOCK_SIZE. Please convert library\n");
		MOD_DEC_USE_COUNT;
		return -ENOEXEC;
	}
	
	if (N_FLAGS(ex)) {
		MOD_DEC_USE_COUNT;
		return -ENOEXEC;
	}

	/* For  QMAGIC, the starting address is 0x20 into the page.  We mask
	   this off to get the starting address for the page */

	start_addr =  ex.a_entry & 0xfffff000;

	/* Now use mmap to map the library into memory. */
	error = do_mmap(file, start_addr, ex.a_text + ex.a_data,
			PROT_READ | PROT_WRITE | PROT_EXEC,
			MAP_FIXED | MAP_PRIVATE | MAP_DENYWRITE,
			N_TXTOFF(ex));
	if (error != start_addr) {
		MOD_DEC_USE_COUNT;
		return error;
	}
	len = PAGE_ALIGN(ex.a_text + ex.a_data);
	bss = ex.a_text + ex.a_data + ex.a_bss;
	if (bss > len) {
		error = do_mmap(NULL, start_addr + len, bss-len,
			PROT_READ|PROT_WRITE|PROT_EXEC,
			MAP_PRIVATE|MAP_FIXED, 0);
		if (error != start_addr + len) {
			MOD_DEC_USE_COUNT;
			return error;
		}
	}
	MOD_DEC_USE_COUNT;
	return 0;
}
