/*
 * linux/fs/binfmt_elf.c
 *
 * These are the functions used to load ELF format executables as used
 * on SVr4 machines.  Information on the format may be found in the book
 * "UNIX SYSTEM V RELEASE 4 Programmers Guide: Ansi C and Programming Support
 * Tools".
 *
 * Copyright 1993, 1994: Eric Youngdale (ericy@cais.com).
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
#include <linux/mm.h>
#include <linux/mman.h>
#include <linux/a.out.h>
#include <linux/errno.h>
#include <linux/signal.h>
#include <linux/binfmts.h>
#include <linux/string.h>
#include <linux/fcntl.h>
#include <linux/ptrace.h>
#include <linux/malloc.h>
#include <linux/shm.h>
#include <linux/personality.h>

#include <asm/pgtable.h>

#include <ibcs/ibcs.h>

#define DLINFO_ITEMS 8

#include <linux/elf.h>

#ifdef ELF_TRACE
#include <ibcs/trace.h>
#endif


#undef ELF_PARSE_COMMENTS


static int load_elf_binary(struct linux_binprm * bprm, struct pt_regs * regs);
static int load_elf_library(int fd);


#ifdef ELF_PARSE_COMMENTS
static int parse_comments(struct linux_binprm *exe_bprm, Elf32_Shdr * sect);

/*
 *  The following table gives clues to the "personality" of the executable
 *  which we hope to find in the .comment sections of the binary.
 *  The set here may not be comprehensive even for those systems listed.
 *  Use 'mcs -p' to list the .comments sections of a binary to see what
 *  clues might be there. Or use 'strings' if you don't have mcs.
 */
struct elf_clue {
    short len;		/* negative number uses strstr for lookup */
    char *text;
    unsigned long personality;
};

static struct elf_clue elf_clues[] = {
    /* SCO Unix OpenServer 5 */
    { 18, "@(#)SCO OpenServer",				PER_SCOSVR3 },
    { 22, "@(#) crt1.s.source 20.",			PER_SCOSVR3 },
#if 0
    /* Interactive SVR4 */
    { 33, "@(#)UNIX System V/386 Release 4.0",		PER_SVR4 },
#endif
#if 0
    /* UnixWare 1.x (at _end_ of string) */
    {  -7, " Univel",					PER_SVR4 },
#endif

    /* End of table */
    { 0, 0, 0 }
};
#endif /* ELF_PARSE_COMMENTS */


#ifdef INIT_MM
struct linux_binfmt ibcs_elf_format = {
	NULL, &mod_use_count_, load_elf_binary, load_elf_library, NULL
};
#else
struct linux_binfmt ibcs_elf_format = {
	load_elf_binary, load_elf_library
};
#endif


/* We need to explicitly zero any fractional pages
   after the data section (i.e. bss).  This would
   contain the junk from the file that should not
   be in memory */

static void
padzero(unsigned long elf_bss)
{
	unsigned int nbyte;
	char *fpnt;
  
	nbyte = elf_bss & (PAGE_SIZE-1);
	if (nbyte) {
		nbyte = PAGE_SIZE - nbyte;
		verify_area(VERIFY_WRITE, (void *) elf_bss, nbyte);
		fpnt = (char *)elf_bss;
		do {
			put_fs_byte(0, fpnt++);
		} while (--nbyte);
	}
}

unsigned long * create_elf_tables(char * p,int argc,int envc,struct elfhdr * exec, unsigned int load_addr, unsigned int interp_load_addr, int ibcs)
{
	unsigned long *argv,*envp, *dlinfo;
	unsigned long * sp;
#if LINUX_VERSION_CODE < 66375
	struct vm_area_struct *mpnt;

	mpnt = (struct vm_area_struct *)kmalloc(sizeof(*mpnt), GFP_KERNEL);
	if (mpnt) {
#ifdef __NR_getsid
		mpnt->vm_mm = current->mm;
#else
		mpnt->vm_task = current;
#endif
		mpnt->vm_start = PAGE_MASK & (unsigned long) p;
		mpnt->vm_end = TASK_SIZE;
		mpnt->vm_page_prot = PAGE_COPY;
#ifdef VM_STACK_FLAGS
		mpnt->vm_flags = VM_STACK_FLAGS;
		mpnt->vm_pte = 0;
#else
#  ifdef VM_GROWSDOWN
		mpnt->vm_flags = VM_GROWSDOWN;
#  endif
#endif
#ifndef MS_ASYNC
		mpnt->vm_share = NULL;
#endif
		mpnt->vm_inode = NULL;
		mpnt->vm_offset = 0;
		mpnt->vm_ops = NULL;
		insert_vm_struct(current->mm, mpnt);
#ifdef __NR_mlock
		current->MM(total_vm) += (mpnt->vm_end - mpnt->vm_start) >> PAGE_SHIFT;
#endif
#ifndef VM_GROWSDOWN
		current->MM(stk_vma) = mpnt;
#endif
	}
#endif
	sp = (unsigned long *) (0xfffffffc & (unsigned long) p);
	if(exec) sp -= DLINFO_ITEMS*2;
	dlinfo = sp;
	sp -= envc+1;
	envp = sp;
	sp -= argc+1;
	argv = sp;
	if (!ibcs) {
		put_fs_long((unsigned long)envp,--sp);
		put_fs_long((unsigned long)argv,--sp);
	}

	/* The constant numbers (0-9) that we are writing here are
	   described in the header file sys/auxv.h on at least
	   some versions of SVr4 */
	if(exec) { /* Put this here for an ELF program interpreter */
	  struct elf_phdr * eppnt;
	  eppnt = (struct elf_phdr *) exec->e_phoff;
	  put_fs_long(3,dlinfo++); put_fs_long(load_addr + exec->e_phoff,dlinfo++);
	  put_fs_long(4,dlinfo++); put_fs_long(sizeof(struct elf_phdr),dlinfo++);
	  put_fs_long(5,dlinfo++); put_fs_long(exec->e_phnum,dlinfo++);
	  put_fs_long(9,dlinfo++); put_fs_long((unsigned long) exec->e_entry,dlinfo++);
	  put_fs_long(7,dlinfo++); put_fs_long(interp_load_addr,dlinfo++);
	  put_fs_long(8,dlinfo++); put_fs_long(0,dlinfo++);
	  put_fs_long(6,dlinfo++); put_fs_long(PAGE_SIZE,dlinfo++);
	  put_fs_long(0,dlinfo++); put_fs_long(0,dlinfo++);
	}

	put_fs_long((unsigned long)argc,--sp);
	current->MM(arg_start) = (unsigned long) p;
	while (argc-->0) {
		put_fs_long((unsigned long) p,argv++);
		while (get_fs_byte(p++)) /* nothing */ ;
	}
	put_fs_long(0,argv);
	current->MM(arg_end) = current->MM(env_start) = (unsigned long) p;
	while (envc-->0) {
		put_fs_long((unsigned long) p,envp++);
		while (get_fs_byte(p++)) /* nothing */ ;
	}
	put_fs_long(0,envp);
	current->MM(env_end) = (unsigned long) p;
	return sp;
}


/* This is much more generalized than the library routine read function,
   so we keep this separate.  Technically the library read function
   is only provided so that we can read a.out libraries that have
   an ELF header */

static unsigned int load_elf_interp(struct elfhdr * interp_elf_ex,
			     struct inode * interpreter_inode, unsigned int *interp_load_addr)
{
        struct file * file;
	struct elf_phdr *elf_phdata  =  NULL;
	struct elf_phdr *eppnt;
	unsigned int len;
	unsigned int load_addr;
	int load_addr_set = 0;
	int elf_exec_fileno;
	int elf_bss;
#ifndef STACK_TOP
	int old_fs;
#endif
	int retval;
	unsigned int last_bss;
	int error;
	int i;
	unsigned int k;
	
	elf_bss = 0;
	last_bss = 0;
	error = load_addr = 0;
	
	/* First of all, some simple consistency checks */
	if((interp_elf_ex->e_type != ET_EXEC && 
	    interp_elf_ex->e_type != ET_DYN) || 
	   (interp_elf_ex->e_machine != EM_386 && interp_elf_ex->e_machine != EM_486) ||
	   (!interpreter_inode->i_op ||
	    !interpreter_inode->i_op->default_file_ops->mmap)){
		return 0xffffffff;
	}
	
	/* Now read in all of the header information */
	
	if(sizeof(struct elf_phdr) * interp_elf_ex->e_phnum > PAGE_SIZE) 
	    return 0xffffffff;
	
	elf_phdata =  (struct elf_phdr *) 
		kmalloc(sizeof(struct elf_phdr) * interp_elf_ex->e_phnum, GFP_KERNEL);
	if(!elf_phdata) return 0xffffffff;
	
#ifndef STACK_TOP
	old_fs = get_fs();
	set_fs(get_ds());
	retval = read_exec(interpreter_inode, interp_elf_ex->e_phoff, (char *) elf_phdata,
			   sizeof(struct elf_phdr) * interp_elf_ex->e_phnum);
	set_fs(old_fs);
#else
	retval = read_exec(interpreter_inode, interp_elf_ex->e_phoff, (char *) elf_phdata,
			   sizeof(struct elf_phdr) * interp_elf_ex->e_phnum, 1);
#endif
	
	elf_exec_fileno = open_inode(interpreter_inode, O_RDONLY);
	if (elf_exec_fileno < 0) return 0xffffffff;
	file = current->FD[elf_exec_fileno];

	eppnt = elf_phdata;
	for(i=0; i<interp_elf_ex->e_phnum; i++, eppnt++)
	  if(eppnt->p_type == PT_LOAD) {
#ifdef PF_R
		int elf_type = MAP_PRIVATE | MAP_DENYWRITE;
		int elf_prot = 0;
		unsigned long vaddr = 0;
	    if (eppnt->p_flags & PF_R) elf_prot = PROT_READ;
	    if (eppnt->p_flags & PF_W) elf_prot |= PROT_WRITE;
	    if (eppnt->p_flags & PF_X) elf_prot |= PROT_EXEC;
		if (interp_elf_ex->e_type == ET_EXEC || load_addr_set) {
			elf_type |= MAP_FIXED;
			vaddr = eppnt->p_vaddr;
	    }
	    error = do_mmap(file, 
			    load_addr + (vaddr & 0xfffff000),
			    eppnt->p_filesz + (vaddr & 0xfff),
				elf_prot,
				elf_type,
			    eppnt->p_offset & 0xfffff000);
#else
	    error = do_mmap(file, 
			    load_addr + (eppnt->p_vaddr & 0xfffff000),
			    eppnt->p_filesz + (eppnt->p_vaddr & 0xfff),
			    PROT_READ | PROT_WRITE | PROT_EXEC,
#ifdef MAP_EXECUTABLE
			    MAP_PRIVATE | MAP_DENYWRITE | MAP_EXECUTABLE | (interp_elf_ex->e_type == ET_EXEC ? MAP_FIXED : 0),
#else
#  ifdef MAP_DENYWRITE
			    MAP_PRIVATE | MAP_DENYWRITE |
			    ((interp_elf_ex->e_type == ET_EXEC || load_addr_set)
				? MAP_FIXED : 0),
#  else
			    MAP_PRIVATE | (interp_elf_ex->e_type == ET_EXEC ? MAP_FIXED : 0),
#  endif
#endif
			    eppnt->p_offset & 0xfffff000);
#endif /* PF_R */
	    
	    if (!load_addr_set && interp_elf_ex->e_type == ET_DYN) {
	      load_addr = error;
	      load_addr_set = 1;
	    }
	    k = load_addr + eppnt->p_vaddr + eppnt->p_filesz;
	    if(k > elf_bss) elf_bss = k;
	    if (error < 0 && error > -1024) break;  /* Real error */
	    k = load_addr + eppnt->p_memsz + eppnt->p_vaddr;
	    if(k > last_bss) last_bss = k;
	  }
	

	/* Now use mmap to map the library into memory. */

	SYS(close)(elf_exec_fileno);
	if(error < 0 && error > -1024) {
	        kfree(elf_phdata);
		return 0xffffffff;
	}

	padzero(elf_bss);
	len = (elf_bss + 0xfff) & 0xfffff000; /* What we have mapped so far */

	/* Map the last of the bss segment */
	if (last_bss > len)
	  do_mmap(NULL, len, last_bss-len,
		  PROT_READ|PROT_WRITE|PROT_EXEC,
		  MAP_FIXED|MAP_PRIVATE, 0);
	kfree(elf_phdata);

	*interp_load_addr = load_addr;
	return ((unsigned int) interp_elf_ex->e_entry) + load_addr;
}

static unsigned int load_aout_interp(struct exec * interp_ex,
			     struct inode * interpreter_inode)
{
  int retval;
  unsigned int elf_entry;

  current->MM(brk) = interp_ex->a_bss +
    (current->MM(end_data) = interp_ex->a_data +
     (current->MM(end_code) = interp_ex->a_text));
  elf_entry = interp_ex->a_entry;
  
  
  if (N_MAGIC(*interp_ex) == OMAGIC) {
    do_mmap(NULL, 0, interp_ex->a_text+interp_ex->a_data,
	    PROT_READ|PROT_WRITE|PROT_EXEC,
	    MAP_FIXED|MAP_PRIVATE, 0);
#ifdef STACK_TOP
    retval = read_exec(interpreter_inode, 32, (char *) 0, 
		       interp_ex->a_text+interp_ex->a_data, 0);
#else
    retval = read_exec(interpreter_inode, 32, (char *) 0, 
		       interp_ex->a_text+interp_ex->a_data);
#endif
  } else if (N_MAGIC(*interp_ex) == ZMAGIC || N_MAGIC(*interp_ex) == QMAGIC) {
    do_mmap(NULL, 0, interp_ex->a_text+interp_ex->a_data,
	    PROT_READ|PROT_WRITE|PROT_EXEC,
	    MAP_FIXED|MAP_PRIVATE, 0);
#ifdef STACK_TOP
    retval = read_exec(interpreter_inode,
		       N_TXTOFF(*interp_ex) ,
		       (char *) N_TXTADDR(*interp_ex),
		       interp_ex->a_text+interp_ex->a_data, 0);
#else
    retval = read_exec(interpreter_inode,
		       N_TXTOFF(*interp_ex) ,
		       (char *) N_TXTADDR(*interp_ex),
		       interp_ex->a_text+interp_ex->a_data);
#endif
  } else
    retval = -1;
  
  if(retval >= 0)
    do_mmap(NULL, (interp_ex->a_text + interp_ex->a_data + 0xfff) & 
	    0xfffff000, interp_ex->a_bss,
	    PROT_READ|PROT_WRITE|PROT_EXEC,
	    MAP_FIXED|MAP_PRIVATE, 0);
  if(retval < 0) return 0xffffffff;
  return elf_entry;
}

/*
 * These are the functions used to load ELF style executables and shared
 * libraries.  There is no binary dependent code anywhere else.
 */

#define INTERPRETER_NONE 0
#define INTERPRETER_AOUT 1
#define INTERPRETER_ELF 2

static int load_elf_binary(struct linux_binprm * bprm, struct pt_regs * regs)
{
	struct elfhdr elf_ex;
	struct elfhdr interp_elf_ex;
	struct file * file;
  	struct exec interp_ex;
	struct inode *interpreter_inode;
	unsigned int load_addr;
	int load_addr_set = 0;
	unsigned int interpreter_type = INTERPRETER_NONE;
	unsigned char ibcs2_interpreter;
	int i;
	int old_fs;
	int error;
	struct elf_phdr * elf_ppnt, *elf_phdata;
	int elf_exec_fileno;
	unsigned int elf_bss, k, elf_brk;
	int retval;
	char * elf_interpreter;
	unsigned int elf_entry, interp_load_addr = 0;
	int status;
	unsigned int start_code, end_code, end_data;
	unsigned int elf_stack;
	char passed_fileno[6];
	
	MOD_INC_USE_COUNT;

	ibcs2_interpreter = 0;
	status = 0;
	load_addr = 0;
	elf_ex = *((struct elfhdr *) bprm->buf);	  /* exec-header */
	
	if (elf_ex.e_ident[0] != 0x7f ||
	    strncmp(&elf_ex.e_ident[1], "ELF",3) != 0) {
		MOD_DEC_USE_COUNT;
		return  -ENOEXEC;
	}
	
	
	/* First of all, some simple consistency checks */
	if(elf_ex.e_type != ET_EXEC || 
	   (elf_ex.e_machine != EM_386 && elf_ex.e_machine != EM_486) ||
	   (!bprm->inode->i_op || !bprm->inode->i_op->default_file_ops ||
	    !bprm->inode->i_op->default_file_ops->mmap)){
		MOD_DEC_USE_COUNT;
		return -ENOEXEC;
	}
	
	current->personality = PER_LINUX;

#ifdef ELF_PARSE_COMMENTS
	/* First scan any sections for .comments which may have
	 * personality clues.
	 */
	if (elf_ex.e_shoff) {
		Elf32_Shdr * elf_spnt, *elf_shdata;

		elf_shdata = (Elf32_Shdr *)kmalloc(elf_ex.e_shentsize *
						   elf_ex.e_shnum, GFP_KERNEL);
#ifdef STACK_TOP
		retval = read_exec(bprm->inode, elf_ex.e_shoff,
				(char *) elf_shdata,
		 		elf_ex.e_shentsize * elf_ex.e_shnum, 1);
#else
		old_fs = get_fs();
		set_fs(get_ds());
		retval = read_exec(bprm->inode, elf_ex.e_shoff,
				(char *) elf_shdata,
				elf_ex.e_shentsize * elf_ex.e_shnum);
		set_fs(old_fs);
#endif
		if (retval < 0) {
	        	kfree(elf_shdata);
			MOD_DEC_USE_COUNT;
			return retval;
		}

		elf_spnt = elf_shdata;
		for(i=0; retval && i < elf_ex.e_shnum; i++){
			if (elf_spnt->sh_type == SHT_PROGBITS
			&& elf_spnt->sh_flags == 0
			&& (status = parse_comments(bprm, elf_spnt)) > 0)
				break;
			elf_spnt++;
			retval -= elf_ex.e_shentsize;
		}

		kfree(elf_shdata);
	}
#endif /* ELF_PARSE_COMMENTS */

#ifdef CONFIG_BINFMT_ELF
	/* If we didn't recognise it as being other than Linux return
	 * ENOEXEC and let the standard Linux ELF loader handle it.
	 * Otherwise we will save a reference to this loader for each
	 * Linux ELF binary which either risks a kernel crash if we
	 * unload iBCS or prevents us unloading iBCS entirely.
	 */
	if (current->personality == PER_LINUX) {
#ifdef ELF_TRACE
		if ((ibcs_trace & TRACE_ELF_LD)) {
			printk(KERN_DEBUG "ELF: Linux or generic SVR4"
					" - hand off to kernel loader.\n");
		}
#endif
		MOD_DEC_USE_COUNT;
		return -ENOEXEC;
	}
#endif

	/* Now read in all of the header information */
	
	elf_phdata = (struct elf_phdr *) kmalloc(elf_ex.e_phentsize * 
						 elf_ex.e_phnum, GFP_KERNEL);
	
#ifdef STACK_TOP
	retval = read_exec(bprm->inode, elf_ex.e_phoff, (char *) elf_phdata,
			   elf_ex.e_phentsize * elf_ex.e_phnum, 1);
#else
	old_fs = get_fs();
	set_fs(get_ds());
	retval = read_exec(bprm->inode, elf_ex.e_phoff, (char *) elf_phdata,
			   elf_ex.e_phentsize * elf_ex.e_phnum);
	set_fs(old_fs);
#endif
	if (retval < 0) {
	        kfree (elf_phdata);
		MOD_DEC_USE_COUNT;
		return retval;
	}
	
	elf_ppnt = elf_phdata;
	
	elf_bss = 0;
	elf_brk = 0;
	
	elf_exec_fileno = open_inode(bprm->inode, O_RDONLY);

	if (elf_exec_fileno < 0) {
	        kfree (elf_phdata);
		MOD_DEC_USE_COUNT;
		return elf_exec_fileno;
	}
	
	file = current->FD[elf_exec_fileno];
	
	elf_stack = 0xffffffff;
	elf_interpreter = NULL;
	start_code = 0xffffffff;
	end_code = 0;
	end_data = 0;
	
#ifndef STACK_TOP
	old_fs = get_fs();
	set_fs(get_ds());
#endif

	for(i=0;i < elf_ex.e_phnum; i++){
		if(elf_ppnt->p_type == PT_INTERP) {
			/* This is the program interpreter used for shared libraries - 
			   for now assume that this is an a.out format binary */
			
			elf_interpreter = (char *) kmalloc(elf_ppnt->p_filesz, 
							   GFP_KERNEL);
			if (elf_interpreter == NULL) {
#ifndef STACK_TOP
				set_fs(old_fs);
#endif
				kfree (elf_phdata);
				kfree(elf_interpreter);
				SYS(close)(elf_exec_fileno);
				MOD_DEC_USE_COUNT;
				return -ENOMEM;
			}
			
#ifdef STACK_TOP
			retval = read_exec(bprm->inode,elf_ppnt->p_offset,elf_interpreter,
					   elf_ppnt->p_filesz, 1);
#else
			retval = read_exec(bprm->inode,elf_ppnt->p_offset,elf_interpreter,
					   elf_ppnt->p_filesz);
#endif

			/* If the program interpreter is one of these two,
			   then assume an iBCS2 image. Otherwise assume
			   a native linux image. */
			if (strcmp(elf_interpreter,"/usr/lib/libc.so.1") == 0 ||
			    strcmp(elf_interpreter,"/usr/lib/ld.so.1") == 0)
			  ibcs2_interpreter = 1;
#if 0
			printk("Using ELF interpreter %s\n", elf_interpreter);
#endif
#ifdef STACK_TOP
			if(retval >= 0) {
				old_fs = get_fs();
				set_fs(get_ds());
				retval = namei(elf_interpreter, &interpreter_inode);
				set_fs(old_fs);
			}
			if(retval >= 0)
				retval = read_exec(interpreter_inode,0,bprm->buf,128, 1);
#else
			if(retval >= 0)
				retval = namei(elf_interpreter, &interpreter_inode);
			if(retval >= 0)
				retval = read_exec(interpreter_inode,0,bprm->buf,128);
#endif
			
			if(retval >= 0) {
				interp_ex = *((struct exec *) bprm->buf);		/* exec-header */
				interp_elf_ex = *((struct elfhdr *) bprm->buf);	  /* exec-header */
				
			}
			if(retval < 0) {
#ifndef STACK_TOP
				set_fs(old_fs);
#endif
				kfree (elf_phdata);
				kfree(elf_interpreter);
				SYS(close)(elf_exec_fileno);
				MOD_DEC_USE_COUNT;
				return retval;
			}
		}
		elf_ppnt++;
	}
	
#ifndef STACK_TOP
	set_fs(old_fs);
#endif
	
	/* Some simple consistency checks for the interpreter */
	if(elf_interpreter){
	        interpreter_type = INTERPRETER_ELF | INTERPRETER_AOUT;
		if(retval < 0) {
			kfree(elf_interpreter);
			kfree(elf_phdata);
			SYS(close)(elf_exec_fileno);
			MOD_DEC_USE_COUNT;
			return -ELIBACC;
		}
		/* Now figure out which format our binary is */
		if((N_MAGIC(interp_ex) != OMAGIC) && 
		   (N_MAGIC(interp_ex) != ZMAGIC) &&
		   (N_MAGIC(interp_ex) != QMAGIC)) 
		  interpreter_type = INTERPRETER_ELF;

		if (interp_elf_ex.e_ident[0] != 0x7f ||
		    strncmp(&interp_elf_ex.e_ident[1], "ELF",3) != 0)
		  interpreter_type &= ~INTERPRETER_ELF;

		if(!interpreter_type)
		  {
		    kfree(elf_interpreter);
		    kfree(elf_phdata);
		    SYS(close)(elf_exec_fileno);
		    MOD_DEC_USE_COUNT;
		    return -ELIBBAD;
		  }
	}
	
	/* OK, we are done with that, now set up the arg stuff,
	   and then start this sucker up */
	
	if (!bprm->sh_bang) {
		char * passed_p;
		
		if(interpreter_type == INTERPRETER_AOUT) {
		  sprintf(passed_fileno, "%d", elf_exec_fileno);
		  passed_p = passed_fileno;
		
		  if(elf_interpreter) {
		    bprm->p = copy_strings(1,&passed_p,bprm->page,bprm->p,2);
		    bprm->argc++;
		  }
		}
		if (!bprm->p) {
		        if(elf_interpreter) {
			      kfree(elf_interpreter);
			}
		        kfree (elf_phdata);
			SYS(close)(elf_exec_fileno);
			MOD_DEC_USE_COUNT;
			return -E2BIG;
		}
	}
	
	/* OK, This is the point of no return */
	flush_old_exec(bprm);

	current->MM(end_data) = 0;
	current->MM(end_code) = 0;
	current->MM(start_mmap) = ELF_START_MMAP;
	current->MM(mmap) = NULL;
	elf_entry = (unsigned int) elf_ex.e_entry;
	
	/* Do this so that we can load the interpreter, if need be.  We will
	   change some of these later */
	current->MM(rss) = 0;
#if LINUX_VERSION_CODE < 66375
#ifndef STACK_TOP
	bprm->p += setup_arg_pages(0, bprm->page);
#endif
#else
	bprm->p = setup_arg_pages(bprm->p, bprm);
#endif
	current->MM(start_stack) = bprm->p;
	
	/* Now we do a little grungy work by mmaping the ELF image into
	   the correct location in memory.  At this point, we assume that
	   the image should be loaded at fixed address, not at a variable
	   address. */
	
	old_fs = get_fs();
	set_fs(get_ds());
	
	elf_ppnt = elf_phdata;
	for(i=0;i < elf_ex.e_phnum; i++){
		
		if(elf_ppnt->p_type == PT_INTERP) {
			/* Set these up so that we are able to load the interpreter */
		  /* Now load the interpreter into user address space */
		  set_fs(old_fs);

		  if(interpreter_type & 1) elf_entry = 
		    load_aout_interp(&interp_ex, interpreter_inode);

		  if(interpreter_type & 2) elf_entry = 
		    load_elf_interp(&interp_elf_ex, interpreter_inode, &interp_load_addr);

		  old_fs = get_fs();
		  set_fs(get_ds());

		  iput(interpreter_inode);
		  kfree(elf_interpreter);
			
		  if(elf_entry == 0xffffffff) { 
		    printk("Unable to load interpreter\n");
		    kfree(elf_phdata);
		    send_sig(SIGSEGV, current, 0);
		    MOD_DEC_USE_COUNT;
		    return 0;
		  }
		}
		
		
		if(elf_ppnt->p_type == PT_LOAD) {
#ifdef PF_R
			int elf_prot = (elf_ppnt->p_flags & PF_R) ? PROT_READ : 0;
			if (elf_ppnt->p_flags & PF_W) elf_prot |= PROT_WRITE;
			if (elf_ppnt->p_flags & PF_X) elf_prot |= PROT_EXEC;
			error = do_mmap(file,
					elf_ppnt->p_vaddr & 0xfffff000,
					elf_ppnt->p_filesz + (elf_ppnt->p_vaddr & 0xfff),
					elf_prot,
					MAP_FIXED | MAP_PRIVATE | MAP_DENYWRITE | MAP_EXECUTABLE,
					elf_ppnt->p_offset & 0xfffff000);
#else
			error = do_mmap(file,
					elf_ppnt->p_vaddr & 0xfffff000,
					elf_ppnt->p_filesz + (elf_ppnt->p_vaddr & 0xfff),
					PROT_READ | PROT_WRITE | PROT_EXEC,
#ifdef MAP_EXECUTABLE
					MAP_FIXED | MAP_PRIVATE | MAP_DENYWRITE | MAP_EXECUTABLE,
#else
#  ifdef MAP_DENYWRITE
					MAP_FIXED | MAP_PRIVATE | MAP_DENYWRITE,
#  else
					MAP_FIXED | MAP_PRIVATE,
#  endif
#endif
					elf_ppnt->p_offset & 0xfffff000);
#endif /* PF_R */

#ifdef LOW_ELF_STACK
			if((elf_ppnt->p_vaddr & 0xfffff000) < elf_stack) 
				elf_stack = elf_ppnt->p_vaddr & 0xfffff000;
#endif
			
			if (!load_addr_set) {
			  load_addr = elf_ppnt->p_vaddr - elf_ppnt->p_offset;
			  load_addr_set = 1;
			}
			k = elf_ppnt->p_vaddr;
			if(k < start_code) start_code = k;
			k = elf_ppnt->p_vaddr + elf_ppnt->p_filesz;
			if(k > elf_bss) elf_bss = k;
#ifdef PF_X
			if((elf_ppnt->p_flags & PF_X) && end_code <  k)
#else
			if((elf_ppnt->p_flags | PROT_WRITE) && end_code <  k)
#endif
				end_code = k; 
			if(end_data < k) end_data = k; 
			k = elf_ppnt->p_vaddr + elf_ppnt->p_memsz;
			if(k > elf_brk) elf_brk = k;		     
		      }
		elf_ppnt++;
	}
	set_fs(old_fs);
	
	kfree(elf_phdata);
	
	if(interpreter_type != INTERPRETER_AOUT) SYS(close)(elf_exec_fileno);
	if (current->personality == PER_LINUX && ibcs2_interpreter)
		current->personality = PER_SVR4;
#ifdef INIT_MM
	if (current->exec_domain && current->exec_domain->use_count)
		(*current->exec_domain->use_count)--;
	if (current->binfmt && current->binfmt->use_count)
		(*current->binfmt->use_count)--;
	current->exec_domain = lookup_exec_domain(current->personality);
	current->binfmt = &ibcs_elf_format;
	if (current->exec_domain && current->exec_domain->use_count)
		(*current->exec_domain->use_count)++;
	if (current->binfmt && current->binfmt->use_count)
		(*current->binfmt->use_count)++;
#endif
#ifndef VM_STACK_FLAGS
	current->executable = bprm->inode;
	bprm->inode->i_count++;
#endif
#ifdef LOW_ELF_STACK
	current->start_stack = bprm->p = elf_stack - 4;
#endif
#if LINUX_VERSION_CODE < 66375
	bprm->p -= MAX_ARG_PAGES*PAGE_SIZE;
#endif
	bprm->p = (unsigned long) 
	  create_elf_tables((char *)bprm->p,
			bprm->argc,
			bprm->envc,
			(interpreter_type == INTERPRETER_ELF ? &elf_ex : NULL),
			load_addr,    
			interp_load_addr,
			(interpreter_type == INTERPRETER_AOUT ? 0 : 1));
	if(interpreter_type == INTERPRETER_AOUT)
	  current->MM(arg_start) += strlen(passed_fileno) + 1;
	current->MM(start_brk) = current->MM(brk) = elf_brk;
	current->MM(end_code) = end_code;
	current->MM(start_code) = start_code;
	current->MM(end_data) = end_data;
	current->MM(start_stack) = bprm->p;
#ifdef INIT_MM
	current->suid = current->euid = current->fsuid = bprm->e_uid;
	current->sgid = current->egid = current->fsgid = bprm->e_gid;
#else
	current->suid = current->euid = bprm->e_uid;
	current->sgid = current->egid = bprm->e_gid;
#endif
#ifdef PF_FORKNOEXEC
	current->flags &= ~PF_FORKNOEXEC;
#endif

	/* Calling sys_brk effectively mmaps the pages that we need for the bss and break
	   sections */
	current->MM(brk) = (elf_bss + 0xfff) & 0xfffff000;
	SYS(brk)((elf_brk + 0xfff) & 0xfffff000);

	padzero(elf_bss);

	if (current->personality != PER_LINUX) {
		/* Why this, you ask???  Well SVr4 maps page 0 as read-only,
		   and some applications "depend" upon this behavior.
		   Since we do not have the power to recompile these, we
		   emulate the SVr4 behavior.  Sigh.  */
		error = do_mmap(NULL, 0, 4096, PROT_READ | PROT_EXEC,
				MAP_FIXED | MAP_PRIVATE, 0);
	}

	/* SVR4/i386 ABI (pages 3-31, 3-32) says that when the program
	   starts %edx contains a pointer to a function which might be
	   registered using `atexit'.  This provides a mean for the
	   dynamic linker to call DT_FINI functions for shared libraries
	   that have been loaded before the code runs.

	   A value of 0 tells we have no such handler.  */
	regs->edx = 0;

	start_thread(regs, elf_entry, bprm->p);
	if (current->flags & PF_PTRACED)
		send_sig(SIGTRAP, current, 0);
	MOD_DEC_USE_COUNT;
	return 0;
}


#ifdef ELF_PARSE_COMMENTS
/*
 *  Parse a comments section looking for clues as to the system this
 *  was compiled on so we can get the system call interface right.
 */
static int
parse_comments(struct linux_binprm *exe_bprm, Elf32_Shdr * sect)
{
	unsigned long offset, nbytes;
	char *buffer;
	int old_fs;
	int status;

	/* Fetch the size of the section. There must be something in there
	 * or the section wouldn't exist at all. We only bother with the
	 * first 8192 characters though. There isn't any point getting too
	 * carried away!
	 */
	if ((nbytes = sect->sh_size) > 8192)
		nbytes = 8192;

	if (!(buffer = (char *)__get_free_page(GFP_KERNEL)))
		return 0;

	offset = sect->sh_offset;
	while (nbytes > 0) {
		char *p;
		unsigned long count, str_start;

#ifdef STACK_TOP
		status = read_exec(exe_bprm->inode, offset, buffer,
				nbytes > PAGE_SIZE ? PAGE_SIZE : nbytes, 1);
#else
		old_fs = get_fs();
		set_fs (get_ds());
		status = read_exec(exe_bprm->inode, offset, buffer,
				nbytes > PAGE_SIZE ? PAGE_SIZE : nbytes);
		set_fs(old_fs);
#endif
#if 0
		if (ibcs_trace & TRACE_ELF_LD)
			printk(KERN_DEBUG
				"ELF: read %d bytes, offset %d, got %d\n",
				nbytes, offset, status);
#endif

		if (status <= 0) {
			free_page((unsigned long)buffer);
			return 0;
		}

		p = buffer;
		str_start = 0;
		for (count=0; count<status; count++) {
			struct elf_clue *clue;
			char c = *(buffer + PAGE_SIZE - 1);
			*(buffer + PAGE_SIZE - 1) = '\0';
#if 0
			if (ibcs_trace & TRACE_ELF_LD)
				printk(KERN_DEBUG "ELF: testing %s\n", p);
#endif
			for (clue=elf_clues; clue->len; clue++) {
				if ((clue->len < 0 && strstr(p, clue->text))
				|| (clue->len > 0 && !strncmp(p, clue->text, clue->len))) {
#ifdef ELF_TRACE
					if (ibcs_trace & TRACE_ELF_LD) {
						printk(KERN_DEBUG
							"ELF: testing %s\n",
							p);
						printk(KERN_DEBUG
							"ELF:    with %s\n",
							clue->text);
						printk(KERN_DEBUG
							"ELF:  giving 0x%08lx\n",
							clue->personality);
					}
#endif
					current->personality = clue->personality;
					free_page((unsigned long)buffer);
					return (1);
				}
			}
			*(buffer + PAGE_SIZE - 1) = c;

			while (*p && count < status) p++,count++;
			if (count < status) {
				p++;
				count++;
				str_start = count;
			}
		}

		/* If we didn't find an end ofstring at all this page
		 * probably isn't useful string data.
		 */
		if (str_start == 0)
			str_start = status;

		nbytes -= str_start;
		offset += str_start;
	}

	free_page((unsigned long)buffer);
	return (0);
}
#endif /* ELF_PARSE_COMMENTS */


/* This is really simpleminded and specialized - we are loading an
   a.out library that is given an ELF header. */

static int load_elf_library(int fd){
        struct file * file;
	struct elfhdr elf_ex;
	struct elf_phdr *elf_phdata  =  NULL;
	struct  inode * inode;
	unsigned int len;
	int elf_bss;
#ifndef STACK_TOP
	int old_fs;
#endif
	int retval;
	unsigned int bss;
	int error;
	int i,j, k;

	MOD_INC_USE_COUNT;

	len = 0;
	file = current->FD[fd];
	inode = file->f_inode;
	elf_bss = 0;
	
	if (SYS(lseek)(fd, 0, 0) != 0) {
		MOD_DEC_USE_COUNT;
		return -ENOEXEC;
	}

	set_fs(KERNEL_DS);
	error = file->f_op->read(inode, file, (char *) &elf_ex, sizeof(elf_ex));
	set_fs(USER_DS);
	if (error != sizeof(elf_ex)) {
		MOD_DEC_USE_COUNT;
		return -ENOEXEC;
	}

	if (elf_ex.e_ident[0] != 0x7f ||
	    strncmp(&elf_ex.e_ident[1], "ELF",3) != 0) {
		MOD_DEC_USE_COUNT;
		return -ENOEXEC;
	}
	
	/* First of all, some simple consistency checks */
	if(elf_ex.e_type != ET_EXEC || elf_ex.e_phnum > 2 ||
	   (elf_ex.e_machine != EM_386 && elf_ex.e_machine != EM_486) ||
	   (!inode->i_op || !inode->i_op->default_file_ops->mmap)){
		MOD_DEC_USE_COUNT;
		return -ENOEXEC;
	}
	
	/* Now read in all of the header information */
	
	if(sizeof(struct elf_phdr) * elf_ex.e_phnum > PAGE_SIZE) {
		MOD_DEC_USE_COUNT;
		return -ENOEXEC;
	}
	
	elf_phdata =  (struct elf_phdr *) 
		kmalloc(sizeof(struct elf_phdr) * elf_ex.e_phnum, GFP_KERNEL);
	
#ifdef STACK_TOP
	retval = read_exec(inode, elf_ex.e_phoff, (char *) elf_phdata,
			   sizeof(struct elf_phdr) * elf_ex.e_phnum, 1);
#else
	old_fs = get_fs();
	set_fs(get_ds());
	retval = read_exec(inode, elf_ex.e_phoff, (char *) elf_phdata,
			   sizeof(struct elf_phdr) * elf_ex.e_phnum);
	set_fs(old_fs);
#endif
	
	j = 0;
	for(i=0; i<elf_ex.e_phnum; i++)
		if((elf_phdata + i)->p_type == PT_LOAD) j++;
	
	if(j != 1)  {
		kfree(elf_phdata);
		MOD_DEC_USE_COUNT;
		return -ENOEXEC;
	}
	
	while(elf_phdata->p_type != PT_LOAD) elf_phdata++;
	
	/* Now use mmap to map the library into memory. */
	error = do_mmap(file,
			elf_phdata->p_vaddr & 0xfffff000,
			elf_phdata->p_filesz + (elf_phdata->p_vaddr & 0xfff),
			PROT_READ | PROT_WRITE | PROT_EXEC,
#ifdef MAP_EXECUTABLE
			MAP_FIXED | MAP_PRIVATE | MAP_DENYWRITE | MAP_EXECUTABLE,
#else
#  ifdef MAP_DENYWRITE
			MAP_FIXED | MAP_PRIVATE | MAP_DENYWRITE,
#  else
			MAP_FIXED | MAP_PRIVATE,
#  endif
#endif
			elf_phdata->p_offset & 0xfffff000);

	k = elf_phdata->p_vaddr + elf_phdata->p_filesz;
	if(k > elf_bss) elf_bss = k;
	
	SYS(close)(fd);
	if (error != (elf_phdata->p_vaddr & 0xfffff000)) {
	        kfree(elf_phdata);
		MOD_DEC_USE_COUNT;
		return error;
	}

	padzero(elf_bss);

	len = (elf_phdata->p_filesz + elf_phdata->p_vaddr+ 0xfff) & 0xfffff000;
	bss = elf_phdata->p_memsz + elf_phdata->p_vaddr;
	if (bss > len)
	  do_mmap(NULL, len, bss-len,
		  PROT_READ|PROT_WRITE|PROT_EXEC,
		  MAP_FIXED|MAP_PRIVATE, 0);
	kfree(elf_phdata);
	MOD_DEC_USE_COUNT;
	return 0;
}
