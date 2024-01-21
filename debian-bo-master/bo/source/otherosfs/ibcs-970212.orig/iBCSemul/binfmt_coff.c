/*
 * These are the functions used to load COFF IBCS style executables.
 * Information on COFF format may be obtained in either the Intel Binary
 * Compatibility Specification 2 or O'Rilley's book on COFF. The shared
 * libraries are defined only the in the Intel book.
 *
 * This file is based upon code written by Eric Youngdale for the ELF object
 * file format.
 *
 * Author: Al Longyear (longyear@sii.com)
 *
 * Latest Revision:
 *    3 February 1994
 *      Al Longyear (longyear@sii.com)
 *      Cleared first page of bss section using put_fs_byte.
 *
 *     4 February 1994
 *       Mike Jagdis (jaggy@purplet.demon.co.uk)
 *       Added scanning of .comment sections for clues as to the
 *       "personality" of the executable. This is then used to emulate
 *       the right set of non-standard syscall extensions and bugs. :-)
 *
 *    18 February 1994
 *       Mike Jagdis (jaggy@purplet.demon.co.uk)
 *       If compile the module version of iBCS rather than the in-kernel
 *       version we must take care to signal when module code is in use
 *       or it may be freed from under us - Bad News :-).
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
#include <linux/binfmts.h>
#include <linux/string.h>
#include <linux/fcntl.h>
#include <linux/ptrace.h>
#include <linux/coff.h>
#include <linux/malloc.h>
#include <linux/personality.h>


#include <ibcs/ibcs.h>

#ifdef COFF_TRACE
#include <ibcs/trace.h>
#endif

#include "binfmt_lib.h"


static int load_coff_binary(struct linux_binprm *bprm, struct pt_regs *regs);
static int load_coff_library(int fd);

static int preload_library(struct linux_binprm *exe_bprm,
			    COFF_SCNHDR * sect,
			    struct file *fp);

static int load_object(struct linux_binprm *bprm,
			struct pt_regs *regs,
		 	int lib_ok);

static int parse_comments(struct linux_binprm *exe_bprm, COFF_SCNHDR * sect);

/*
 *  The following table gives clues to the "personality" of the executable
 *  which we hope to find in the .comment sections of the binary.
 *  The set here may not be comprehensive even for those systems listed.
 *  Use 'mcs -p' to list the .comments sections of a binary to see what
 *  clues might be there. Or use 'strings' if you don't have mcs.
 */
struct coff_clue {
    short len;		/* negative number uses strstr for lookup */
    char *text;
    unsigned long personality;
};

static struct coff_clue coff_clues[] = {
    /* Wyse Unix V/386 3.2.1[A]. */
    { 36, "@(#) UNIX System V/386 Release 3.2.1",	PER_WYSEV386 },

    /* SCO Unix V 3.2, 3.2.2, 3.2.4, 3.2.4.2 etc. */
    { 23, "@(#) crt1.s 1.8 89/05/30",			PER_SCOSVR3 },
    { 16, "@(#)SCO UNIX 3.2",				PER_SCOSVR3 },
    { 18, "\"@(#) SCO UNIX 3.2",			PER_SCOSVR3 },
    { 17, "@(#) SCO UNIX 3.2",				PER_SCOSVR3 },
    { 11, "@(#)SCO 3.2",				PER_SCOSVR3 },

    /* Interactive (ISC) 4.0 */
    { -1, "INTERACTIVE",				PER_ISCR4   },

    /* End of table */
    { 0, 0, 0 }
};


#ifdef INIT_MM
struct linux_binfmt coff_format = {
	NULL, &mod_use_count_, load_coff_binary, load_coff_library, NULL
};
#else
struct linux_binfmt coff_format = {
	load_coff_binary, load_coff_library
};
#endif


/*
 *  Small procedure to test for the proper file alignment.
 */

static inline int
is_properly_aligned (COFF_SCNHDR *sect)
{
    long scnptr = COFF_LONG (sect->s_scnptr);
    long vaddr  = COFF_LONG (sect->s_vaddr);
/*
 *  Print the section information if needed
 */

#ifdef COFF_TRACE
    if (ibcs_trace & TRACE_COFF_LD)
        printk (KERN_DEBUG "COFF: %s, scnptr = 0x%08lx, vaddr = 0x%08lx\n",
	        sect->s_name,
	        scnptr, vaddr);
#endif

/*
 *  Return the error code if the section is not properly aligned.
 */

#ifdef COFF_TRACE
    if (ibcs_trace & TRACE_COFF_LD)
        if (((vaddr - scnptr) & ~PAGE_MASK) != 0)
	    printk (KERN_DEBUG "COFF: bad alignment in %s\n", sect->s_name);
#endif
    return ((((vaddr - scnptr) & ~PAGE_MASK) != 0) ? -ENOEXEC : 0);
}

/*
 *    Clear the bytes in the last page of data.
 */

static
int clear_memory (unsigned long addr, unsigned long size)
{
    int status;

    size = (PAGE_SIZE - (addr & ~PAGE_MASK)) & ~PAGE_MASK;
    if (size == 0)
        status = 0;
    else {
      
#ifdef COFF_TRACE
	if (ibcs_trace & TRACE_COFF_LD)
            printk (KERN_DEBUG "COFF: un-initialized storage in last page %ld\n", size);
#endif

	status = verify_area (VERIFY_WRITE,
			      (void *) addr, size);
#ifdef COFF_TRACE
	if (ibcs_trace & TRACE_COFF_LD)
	    printk (KERN_DEBUG "COFF: result from verify_area = %d\n", status);
#endif

	if (status >= 0)
	    while (size-- != 0)
	        put_fs_byte (0, addr++);
    }
    return status;
}

/*
 *  Helper function to process the load operation.
 */

static int
load_object (struct linux_binprm * bprm, struct pt_regs *regs, int lib_ok)
{
    COFF_FILHDR  *coff_hdr = (COFF_FILHDR *) bprm->buf;	/* COFF Header */
    COFF_SCNHDR  *sect_bufr;	/* Pointer to section table            */
    COFF_SCNHDR  *text_sect;	/* Pointer to the text section         */
    COFF_SCNHDR  *data_sect;	/* Pointer to the data section         */
    COFF_SCNHDR  *bss_sect;	/* Pointer to the bss section          */
    int text_count;		/* Number of text sections             */
    int data_count;		/* Number of data sections             */
    int bss_count;		/* Number of bss sections              */
    int lib_count;		/* Number of lib sections              */
    unsigned int start_addr = 0;/* Starting location for program       */
    int status = 0;		/* Result status register              */
    int fd = -1;		/* Open file descriptor                */
    struct file *fp     = NULL;	/* Pointer to the file at "fd"         */
    short int sections  = 0;	/* Number of sections in the file      */
    short int aout_size = 0;	/* Size of the a.out header area       */
    short int flags;		/* Flag bits from the COFF header      */
    char not_pageable = 0;	/* Can we demand page the executable?  */
    unsigned long p = bprm->p;

#ifdef COFF_TRACE
    if (ibcs_trace & TRACE_COFF_LD)
        printk (KERN_DEBUG "COFF: exec %s\n", bprm->filename);
#endif

/*
 *  Validate the magic value for the object file.
 */
    do {
	if (COFF_I386BADMAG (*coff_hdr)) {
#ifdef COFF_TRACE
	    if (ibcs_trace & TRACE_COFF_LD)
	        printk (KERN_DEBUG "COFF: bad filehdr magic\n");
#endif
	    status = -ENOEXEC;
	    break;
	}
/*
 *  The object file should have 32 BIT little endian format. Do not allow
 *  it to have the 16 bit object file flag set as Linux is not able to run
 *  on the 80286/80186/8086.
 */
	flags = COFF_SHORT (coff_hdr->f_flags);
	if ((flags & (COFF_F_AR32WR | COFF_F_AR16WR)) != COFF_F_AR32WR) {
#ifdef COFF_TRACE
	    if (ibcs_trace & TRACE_COFF_LD)
	        printk (KERN_DEBUG "COFF: invalid f_flags bits\n");
#endif
	    status = -ENOEXEC;
	    break;
	}
/*
 *  Extract the header information which we need.
 */
	sections  = COFF_SHORT (coff_hdr->f_nscns);   /* Number of sections */
	aout_size = COFF_SHORT (coff_hdr->f_opthdr);  /* Size of opt. headr */
/*
 *  If the file is not executable then reject the execution. This means
 *  that there must not be external references.
 */
	if ((flags & COFF_F_EXEC) == 0) {
#ifdef COFF_TRACE
	    if (ibcs_trace & TRACE_COFF_LD)
	        printk (KERN_DEBUG "COFF: not executable bit\n");
#endif
	    status = -ENOEXEC;
	    break;
	}
/*
 *  There must be atleast one section.
 */
	if (sections == 0) {
#ifdef COFF_TRACE
	    if (ibcs_trace & TRACE_COFF_LD)
	        printk (KERN_DEBUG "COFF: no sections\n");
#endif
	    status = -ENOEXEC;
	    break;
	}
/*
 *  Do some additional consistency checks.
 *  The system requires mapping for this loader. If you try
 *  to use a file system with no mapping, the format is not valid.
 */
	if (!bprm->inode->i_op
	|| !bprm->inode->i_op->default_file_ops
	|| !bprm->inode->i_op->default_file_ops->mmap) {
#ifdef COFF_TRACE
	    if (ibcs_trace & TRACE_COFF_LD)
	        printk (KERN_DEBUG "COFF: no mmap in fs - demand paging disabled\n");
#endif
	    not_pageable = 1;
	}
    }
    while (0);
/*
 *  Allocate a buffer to hold the entire coff section list.
 */
    if (status >= 0) {
	int nbytes = sections * COFF_SCNHSZ;

	sect_bufr = (COFF_SCNHDR *) kmalloc (nbytes, GFP_KERNEL);
	if (0 == sect_bufr) {
	    printk (KERN_WARNING "COFF: kmalloc failed\n");
	    status = -ENOEXEC;
	}
/*
 *  Read the section list from the disk file.
 */
	else {
#ifdef STACK_TOP
	     status = read_exec (bprm->inode,
			    aout_size + COFF_FILHSZ,
			    (char *) sect_bufr,
			    nbytes, 1);
#else
	     int old_fs = get_fs ();
	     set_fs (get_ds ());  /* Make it point to the proper location    */
	     status = read_exec (bprm->inode,	     /* INODE for file       */
			    aout_size + COFF_FILHSZ, /* Offset in the file   */
			    (char *) sect_bufr,      /* Buffer for read      */
			    nbytes);                 /* Byte count reqd.     */
	     set_fs (old_fs);	                     /* Restore the selector */
#endif
#ifdef COFF_TRACE
	     if (status < 0 && (ibcs_trace & TRACE_COFF_LD))
	        printk (KERN_DEBUG "COFF: read aout hdr, status = %d\n", status);
#endif
	 }
    }
    else
	sect_bufr = NULL;	/* Errors do not have a section buffer */
/*
 *  Count the number of sections for the required types and store the location
 *  of the last section for the three primary types.
 */
    text_count = 0;
    data_count = 0;
    bss_count  = 0;
    lib_count  = 0;

    text_sect = NULL;
    data_sect = NULL;
    bss_sect  = NULL;
/*
 *  Loop through the sections and find the various types
 */
    if (status >= 0) {
	int nIndex;
	COFF_SCNHDR *sect_ptr = sect_bufr;

	for (nIndex = 0; nIndex < sections; ++nIndex) {
	    long int sect_flags = COFF_LONG (sect_ptr->s_flags);

	    switch (sect_flags) {
	    case COFF_STYP_TEXT:
		text_sect = sect_ptr;
		++text_count;
		status = is_properly_aligned (sect_ptr);
		break;

	    case COFF_STYP_DATA:
		data_sect = sect_ptr;
		++data_count;
		status = is_properly_aligned (sect_ptr);
		break;

	    case COFF_STYP_BSS:
		bss_sect = sect_ptr;
		++bss_count;
		break;

	    case COFF_STYP_LIB:
#ifdef COFF_TRACE
		if (ibcs_trace & TRACE_COFF_LD)
		    printk (KERN_DEBUG "COFF: .lib section found\n");
#endif
		++lib_count;
		break;

	    default:
		break;
	    }
	    sect_ptr = (COFF_SCNHDR *) & ((char *) sect_ptr)[COFF_SCNHSZ];
	}

/*
 * If any of the sections weren't properly aligned we aren't going to
 * be able to demand page this executable. Note that at this stage the
 * *only* excuse for having status <= 0 is if the alignment test failed.
 */
	if (status < 0) {
#ifdef COFF_TRACE
	    if (ibcs_trace & TRACE_COFF_LD)
	        printk (KERN_DEBUG "COFF: incorrectly aligned - demand paging disabled\n");
#endif
	    not_pageable = 1;
	    status = 0;
	}

/*
 *  Ensure that there are the required sections. There must be one text
 *  sections and one each of the data and bss sections for an executable.
 *  A library may or may not have a data / bss section.
 */
	if (text_count != 1) {
	    status = -ENOEXEC;
#ifdef COFF_TRACE
	    if (ibcs_trace & TRACE_COFF_LD)
	        printk (KERN_DEBUG "COFF: no text sections\n");
#endif
	}
	else {
	    if (lib_ok) {
		if (data_count != 1 || bss_count != 1) {
		    status = -ENOEXEC;
#ifdef COFF_TRACE
		    if (ibcs_trace & TRACE_COFF_LD)
		        printk (KERN_DEBUG "COFF: no .data nor .bss sections\n");
#endif
		}
	    }
	}
    }
/*
 *  If there is no additional header then assume the file starts at
 *  the first byte of the text section. This may not be the proper place,
 *  so the best solution is to include the optional header. A shared library
 *  __MUST__ have an optional header to indicate that it is a shared library.
 */
    if (status >= 0) {
	if (aout_size == 0) {
	    if (!lib_ok) {
		status = -ENOEXEC;
#ifdef COFF_TRACE
		if (ibcs_trace & TRACE_COFF_LD)
		    printk (KERN_DEBUG "COFF: no header in library\n");
#endif
	    }
	    start_addr = COFF_LONG (text_sect->s_vaddr);
	}
/*
 *  There is some header. Ensure that it is sufficient.
 */
	else {
	    if (aout_size < (short)COFF_AOUTSZ) {
		status = -ENOEXEC;
#ifdef COFF_TRACE
		if (ibcs_trace & TRACE_COFF_LD)
		    printk (KERN_DEBUG "COFF: header too small\n");
#endif
	    }
	    else {
		COFF_AOUTHDR *aout_hdr =	/* Pointer to a.out header */
		(COFF_AOUTHDR *) & ((char *) coff_hdr)[COFF_FILHSZ];
		short int aout_magic = COFF_SHORT (aout_hdr->magic); /* id */
/*
 *  Validate the magic number in the a.out header. If it is valid then
 *  update the starting symbol location. Do not accept these file formats
 *  when loading a shared library.
 */
		switch (aout_magic) {
		case COFF_OMAGIC:
		case COFF_ZMAGIC:
		case COFF_STMAGIC:
		    if (!lib_ok) {
			status = -ENOEXEC;
#ifdef COFF_TRACE
			if (ibcs_trace & TRACE_COFF_LD)
			    printk (KERN_DEBUG "COFF: wrong a.out header magic\n");
#endif
		    }
		    start_addr = (unsigned int) COFF_LONG (aout_hdr->entry);
		    break;
/*
 *  Magic value for a shared library. This is valid only when loading a
 *  shared library. (There is no need for a start_addr. It won't be used.)
 */
		case COFF_SHMAGIC:
		    if (lib_ok) {
#ifdef COFF_TRACE
			if (ibcs_trace & TRACE_COFF_LD)
			    printk (KERN_DEBUG "COFF: wrong a.out header magic\n");
#endif
			status = -ENOEXEC;
		    }
		    break;

		default:
#ifdef COFF_TRACE
		    if (ibcs_trace & TRACE_COFF_LD)
		        printk (KERN_DEBUG "COFF: wrong a.out header magic\n");
#endif
		    status = -ENOEXEC;
		    break;
		}
	    }
	}
    }
/*
 *  Fetch a file pointer to the executable.
 */
    if (status >= 0) {
	fd = open_inode (bprm->inode, O_RDONLY);
	if (fd < 0) {
#ifdef COFF_TRACE
	    if (ibcs_trace & TRACE_COFF_LD)
	        printk (KERN_DEBUG "COFF: can not open inode, result = %d\n", fd);
#endif
	    status = fd;
	}
	else
	    fp = current->FD[fd];
    }
    else
	fd = -1;		/* Invalidate the open file descriptor */
/*
 *  Generate the proper values for the text fields
 *
 *  THIS IS THE POINT OF NO RETURN. THE NEW PROCESS WILL TRAP OUT SHOULD
 *  SOMETHING FAIL IN THE LOAD SEQUENCE FROM THIS POINT ONWARD.
 */
    if (status >= 0) {
	long text_scnptr = COFF_LONG (text_sect->s_scnptr);
	long text_size   = COFF_LONG (text_sect->s_size);
	long text_vaddr  = COFF_LONG (text_sect->s_vaddr);

	long data_scnptr;
	long data_size;
	long data_vaddr;

	long bss_size;
	long bss_vaddr;
/*
 *  Generate the proper values for the data fields
 */
	if (data_sect != NULL) {
	    data_scnptr = COFF_LONG (data_sect->s_scnptr);
	    data_size   = COFF_LONG (data_sect->s_size);
	    data_vaddr  = COFF_LONG (data_sect->s_vaddr);
	}
	else {
	    data_scnptr = 0;
	    data_size   = 0;
	    data_vaddr  = 0;
	}
/*
 *  Generate the proper values for the bss fields
 */
	if (bss_sect != NULL) {
	    bss_size  = COFF_LONG (bss_sect->s_size);
	    bss_vaddr = COFF_LONG (bss_sect->s_vaddr);
	}
	else {
	    bss_size  = 0;
	    bss_vaddr = 0;
	}
/*
 *  Flush the executable from memory. At this point the executable is
 *  committed to being defined or a segmentation violation will occur.
 */
	if (lib_ok) {
#ifdef COFF_TRACE
	    if (ibcs_trace & TRACE_COFF_LD)
	        printk (KERN_DEBUG "COFF: flushing executable\n");
#endif
	    flush_old_exec (bprm);
/*
 *  Define the initial locations for the various items in the new process
 */
	    current->MM(mmap)        = NULL;
	    current->MM(rss)         = 0;
/*
 *  Construct the parameter and environment string table entries.
 */
#if LINUX_VERSION_CODE < 66375
#ifdef STACK_TOP
	    p += setup_arg_pages (0, bprm->page);
	    p -= MAX_ARG_PAGES*PAGE_SIZE;
	    p  = (unsigned long) create_tables ((char *) p, bprm, 1);
#else
	    p += change_ldt (0, bprm->page);
	    p -= MAX_ARG_PAGES*PAGE_SIZE;
	    p  = (unsigned long) create_tables ((char *) p,
						      bprm->argc,
						      bprm->envc,
						      1);
#endif
#else
	p = setup_arg_pages(p, bprm);
	p = (unsigned long)create_ibcs_tables((char *)p, bprm, 1);
#endif
/*
 *  Do the end processing once the stack has been constructed
 */
	    current->MM(start_code)  = text_vaddr & PAGE_MASK;
	    current->MM(end_code)    = text_vaddr + text_size;
	    current->MM(end_data)    = data_vaddr + data_size;
	    current->MM(start_brk)   =
	    current->MM(brk)         = bss_vaddr + bss_size;
	    current->MM(start_stack) = p;
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
#ifndef VM_STACK_FLAGS
	    current->executable  = bprm->inode; /* Store inode for file  */
	    ++bprm->inode->i_count;             /* Count the open inode  */
#endif
	    start_thread(regs, start_addr, current->MM(start_stack));
	}
/*
 *   Map the text pages
 */

#ifdef COFF_TRACE
	if (ibcs_trace & TRACE_COFF_LD)
	    printk (KERN_DEBUG "COFF: .text: vaddr = 0x%08lx, size = 0x%08lx, scnptr = 0x%08lx\n",
		     text_vaddr,
		     text_size,
		     text_scnptr);
#endif
	if (not_pageable)
		status = do_mmap (NULL,
			  text_vaddr & PAGE_MASK,
			  text_size + (text_vaddr & ~PAGE_MASK),
			  PROT_READ | PROT_EXEC,
			  MAP_FIXED | MAP_SHARED,
			  0);
	else
		status = do_mmap (fp,
			  text_vaddr & PAGE_MASK,
			  text_size + (text_vaddr & ~PAGE_MASK),
			  PROT_READ | PROT_EXEC,
#ifdef MAP_EXECUTABLE
			  MAP_FIXED | MAP_SHARED | MAP_DENYWRITE | MAP_EXECUTABLE,
#else
#  ifdef MAP_DENYWRITE
			  MAP_FIXED | MAP_SHARED | MAP_DENYWRITE,
#  else
			  MAP_FIXED | MAP_SHARED,
#  endif
#endif
			  text_scnptr & PAGE_MASK);

	status = (status == (int)(text_vaddr & PAGE_MASK)) ? 0 : -ENOEXEC;
/*
 *   Map the data pages
 */
	if (status >= 0 && data_size != 0) {
#ifdef COFF_TRACE
	    if (ibcs_trace & TRACE_COFF_LD)
	        printk (KERN_DEBUG "COFF: .data: vaddr = 0x%08lx, size = 0x%08lx, scnptr = 0x%08lx\n",
		         data_vaddr,
		         data_size,
		         data_scnptr);
#endif
	if (not_pageable)
	    	status = do_mmap (NULL,
			      data_vaddr & PAGE_MASK,
			      data_size + (data_vaddr & ~PAGE_MASK),
			      PROT_READ | PROT_WRITE | PROT_EXEC,
			      MAP_FIXED | MAP_PRIVATE,
			      0);
	else
	    	status = do_mmap (fp,
			      data_vaddr & PAGE_MASK,
			      data_size + (data_vaddr & ~PAGE_MASK),
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
			      data_scnptr & PAGE_MASK);

	    status = (status == (int)(data_vaddr & PAGE_MASK)) ? 0 : -ENOEXEC;
	}

	/* If we can't demand page it we have to read it in now. */
	if (not_pageable && status >= 0) {
#ifdef STACK_TOP
		status = read_exec(bprm->inode,
				text_scnptr,
				(char *)text_vaddr,
				text_size, 0);
#else
		status = read_exec(bprm->inode,
				text_scnptr,
				(char *)text_vaddr,
				text_size);
#endif
		if (status >= 0 && status != text_size)
			status = -ENOEXEC;

		if (status >= 0 && data_size) {
#ifdef STACK_TOP
			status = read_exec(bprm->inode,
					data_scnptr,
					(char *)data_vaddr,
					data_size, 0);
#else
			status = read_exec(bprm->inode,
					data_scnptr,
					(char *)data_vaddr,
					data_size);
#endif
			if (status >= 0 && status != data_size)
				status = -ENOEXEC;
		}
	}

/*
 *   Construct the bss data for the process. The bss ranges from the
 *   end of the data (which may not be on a page boundary) to the end
 *   of the bss section. Allocate any necessary pages for the data.
 */
	if (status >= 0 && bss_size != 0) {
#ifdef COFF_TRACE
	    if (ibcs_trace & TRACE_COFF_LD)
	        printk (KERN_DEBUG "COFF: .bss: vaddr = 0x%08lx, size = 0x%08lx\n",
		         bss_vaddr,
		         bss_size);
#endif
	    do_mmap(NULL, PAGE_ALIGN(bss_vaddr),
		bss_size + bss_vaddr - PAGE_ALIGN(bss_vaddr),
		PROT_READ | PROT_WRITE | PROT_EXEC,
		MAP_FIXED | MAP_PRIVATE, 0);

	    status = clear_memory (bss_vaddr, bss_size);
	}
/*
 *  Load any shared library for the executable.
 */
	if (status >= 0 && lib_ok && lib_count != 0) {
	    int nIndex;
	    COFF_SCNHDR *sect_ptr = sect_bufr;
/*
 *  Find the library sections. (There should be at least one. It was counted
 *  earlier.) This will eventually recurse to our code and load the shared
 *  library with our own procedures.
 */
	    for (nIndex = 0; nIndex < sections; ++nIndex) {
		long int sect_flags = COFF_LONG (sect_ptr->s_flags);
		if (sect_flags == COFF_STYP_LIB) {
		    status = preload_library (bprm, sect_ptr, fp);
		    if (status != 0)
			break;
		}
	    sect_ptr = (COFF_SCNHDR *) &((char *) sect_ptr) [COFF_SCNHSZ];
	    }
	}
/*
 *  Look for clues as to the system this binary was compiled on in the
 *  comments section(s). Only look at the main binary, not the shared
 *  libraries (or would it be better to prefer shared libraries over
 *  binaries? Or could they be different???)
 */
	current->personality = PER_SVR3;
	if (status >= 0 && lib_ok) {
	    int nIndex;
	    COFF_SCNHDR *sect_ptr = sect_bufr;

	    for (nIndex = 0; nIndex < sections; ++nIndex) {
		long int sect_flags = COFF_LONG (sect_ptr->s_flags);
		if (sect_flags == COFF_STYP_INFO
		&& (status = parse_comments(bprm, sect_ptr)) > 0)
			break;
	        sect_ptr = (COFF_SCNHDR *) &((char *) sect_ptr) [COFF_SCNHSZ];
	    }

	    /* If no .comments section was found there is no way to
	     * figure out the personality. Odds on it is SCO though...
	     */
	    if (nIndex == sections)
		current->personality = PER_SCOSVR3;
	}
#ifdef INIT_MM
	if (current->exec_domain && current->exec_domain->use_count)
		(*current->exec_domain->use_count)--;
	if (current->binfmt && current->binfmt->use_count)
		(*current->binfmt->use_count)--;
	current->exec_domain = lookup_exec_domain(current->personality);
	current->binfmt = &coff_format;
	if (current->exec_domain && current->exec_domain->use_count)
		(*current->exec_domain->use_count)++;
	if (current->binfmt && current->binfmt->use_count)
		(*current->binfmt->use_count)++;
#endif
/*
 *   Generate any needed trap for this process. If an error occured then
 *   generate a segmentation violation. If the process is being debugged
 *   then generate the load trap. (Note: If this is a library load then
 *   do not generate the trap here. Pass the error to the caller who
 *   will do it for the process in the outer lay of this procedure call.)
 */
	if (lib_ok) {
	    if (status < 0)
		send_sig (SIGSEGV, current, 0);	/* Generate the error trap  */
	    else {
		if (current->flags & PF_PTRACED)
		    send_sig (SIGTRAP, current, 0);
	    }
	    status = 0;		/* We are committed. It can't fail */
	}
    }
/*
 *  Do any cleanup processing
 */
    if (fd >= 0)
	SYS(close) (fd);		/* Close unused code file      */

    if (sect_bufr != NULL)
	kfree (sect_bufr);	/* Release section list buffer */
/*
 *  Return the completion status.
 */
#ifdef COFF_TRACE
    if (ibcs_trace & TRACE_COFF_LD)
        printk (KERN_DEBUG "COFF: binfmt_coff: result = %d\n", status);
#endif
    return (status);
}

/*
 *  This procedure will load the library listed in the file name given
 *  as the parameter. The result will be non-zero should something fail
 *  to load.
 */

static int
preload_this_library (struct linux_binprm *exe_bprm, char *lib_name)
{
    int   status;
    int   old_fs = get_fs();
/*
 *  If debugging then print "we have arrived"
 */
#ifdef COFF_TRACE
    if (ibcs_trace & TRACE_COFF_LD)
        printk (KERN_DEBUG "COFF: %s loading shared library %s\n",
	        exe_bprm->filename,
	        lib_name);
#endif
/*
 *  Change the FS register to the proper kernel address space and attempt
 *  to load the library. The library name is allocated from the kernel
 *  pool.
 */
    set_fs (get_ds ());
    status = SYS(uselib) (lib_name);
    set_fs (old_fs);
/*
 *  Return the success/failure to the caller.
 */
    return (status);
}

/*
 *  This procedure is called to load a library section. The various
 *  libraries are loaded from the list given in the section data.
 */

static int
preload_library (struct linux_binprm *exe_bprm,
		 COFF_SCNHDR * sect, struct file *fp)
{
    int status = 0;		/* Completion status                  */
    long nbytes;		/* Count of bytes in the header area  */
/*
 *  Fetch the size of the section. There must be enough room for at least
 *  one entry.
 */
    nbytes = (long)COFF_LONG (sect->s_size);
    if (nbytes < (long)COFF_SLIBSZ) {
	status = -ENOEXEC;
#ifdef COFF_TRACE
	if (ibcs_trace & TRACE_COFF_LD)
	    printk (KERN_DEBUG "COFF: library section too small\n");
#endif
    }
/*
 *  Allocate a buffer to hold the section data
 */
    else {
	COFF_SLIBHD *phdr;
	char *buffer = (char *) kmalloc (nbytes, GFP_KERNEL);

        if (0 == buffer) {
	    status = -ENOEXEC;
	    printk (KERN_WARNING "COFF: kmalloc failed\n");
	}
	else {
#ifdef STACK_TOP
	    status = read_exec (exe_bprm->inode,
			    COFF_LONG (sect->s_scnptr),
			    buffer,
			    nbytes, 1);
#else
	    int old_fs   = get_fs ();
	    set_fs (get_ds ());   /* Make it point to the proper location    */
	    status = read_exec (exe_bprm->inode,     /* INODE for file       */
			    COFF_LONG (sect->s_scnptr), /* Disk location     */
			    buffer,                     /* Buffer for read   */
			    nbytes);                    /* Byte count reqd.  */
	    set_fs (old_fs);                         /* Restore the selector */
#endif
	    if (status >= 0 && status != nbytes) {
#ifdef COFF_TRACE
		if (ibcs_trace & TRACE_COFF_LD)
		    printk (KERN_DEBUG "COFF: read of lib section was short\n");
#endif
		status = -ENOEXEC;
	    }
	}
/*
 *  At this point, go through the list of libraries in the data area.
 */
	phdr = (COFF_SLIBHD *) buffer;
	while (status >= 0 && nbytes > (long)COFF_SLIBSZ) {
	    int entry_size  = COFF_LONG (phdr->sl_entsz)   * sizeof (long);
	    int header_size = COFF_LONG (phdr->sl_pathndx) * sizeof (long);
/*
 *  Validate the sizes of the various items. I don't trust the linker!!
 */
	    if ((unsigned) header_size >= (unsigned) nbytes ||
		entry_size <= 0 ||
		(unsigned) entry_size <= (unsigned) header_size) {
		status = -ENOEXEC;
#ifdef COFF_TRACE
		if (ibcs_trace & TRACE_COFF_LD)
		    printk (KERN_DEBUG "COFF: header count is invalid\n");
#endif
	    }
/*
 *  Load the library. Stop the load process on the first error.
 */
	    else {
		status = preload_this_library (exe_bprm,
					       &((char *) phdr)[header_size]);
#ifdef COFF_TRACE
		if (ibcs_trace & TRACE_COFF_LD)
		    printk (KERN_DEBUG "COFF: preload_this_library result = %d\n", status);
#endif
	    }
/*
 *  Point to the next library in the section data.
 */
	    nbytes -= entry_size;
	    phdr = (COFF_SLIBHD *) &((char *) phdr)[entry_size];
	}
/*
 *  Release the space for the library list.
 */
	if (buffer != NULL)
	    kfree (buffer);
    }
/*
 *  Return the resulting status to the caller.
 */
    return (status);
}

/*
 *  Parse a comments section looking for clues as to the system this
 *  was compiled on so we can get the system call interface right.
 */

static int
parse_comments (struct linux_binprm *exe_bprm, COFF_SCNHDR * sect)
{
	unsigned long offset, nbytes;
	char *buffer;
	int status;

/*
 *  Fetch the size of the section. There must be something in there or the
 *  section wouldn't exist at all. We only bother with the first 8192
 *  characters though. There isn't any point getting too carried away!
 */
	if ((nbytes = COFF_LONG (sect->s_size)) > 8192)
		nbytes = 8192;

	if (!(buffer = (char *)__get_free_page(GFP_KERNEL)))
		return 0;

	offset = COFF_LONG(sect->s_scnptr);
	while (nbytes > 0) {
		char *p;
		unsigned long count, str_start;

#ifdef STACK_TOP
		status = read_exec(exe_bprm->inode, offset, buffer,
				nbytes > PAGE_SIZE ? PAGE_SIZE : nbytes, 1);
#else
		int old_fs = get_fs();
		set_fs(get_ds());
		status = read_exec(exe_bprm->inode, offset,
				buffer,
				nbytes > PAGE_SIZE ? PAGE_SIZE : nbytes);
		set_fs(old_fs);
#endif
#if 0
		if (ibcs_trace & TRACE_COFF_LD)
			printk(KERN_DEBUG
				"COFF: read %ld bytes, offset %ld, got %d\n",
				nbytes, offset, status);
#endif
		if (status <= 0) {
			free_page((unsigned long)buffer);
			return 0;
		}

		p = buffer;
		str_start = 0;
		for (count=0; count<status; count++) {
			struct coff_clue *clue;
			char c = *(buffer + PAGE_SIZE - 1);
			*(buffer + PAGE_SIZE - 1) = '\0';
#if 0
			if (ibcs_trace & TRACE_COFF_LD)
				printk(KERN_DEBUG "COFF: testing %s\n", p);
#endif
			for (clue=coff_clues; clue->len; clue++) {
				if ((clue->len < 0 && strstr(p, clue->text))
				|| (clue->len > 0 && !strncmp(p, clue->text, clue->len))) {
#ifdef COFF_TRACE
					if (ibcs_trace & TRACE_COFF_LD) {
						printk(KERN_DEBUG
							"COFF: testing %s\n",
							p);
						printk(KERN_DEBUG
							"COFF:    with %s\n",
							clue->text);
						printk(KERN_DEBUG
							"COFF:  giving 0x%08lx\n",
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


/*
 *  This procedure is called by the main load sequence. It will load
 *  the executable and prepare it for execution. It provides the additional
 *  parameters used by the recursive coff loader and tells the loader that
 *  this is the main executable. How simple it is . . . .
 */

static int
load_coff_binary (struct linux_binprm *bprm, struct pt_regs *regs)
{
	int ret;

	MOD_INC_USE_COUNT;
	ret = load_object (bprm, regs, 1);
	MOD_DEC_USE_COUNT;
	return ret;
}

/*
 *   Load the image for any shared library.
 *
 *   This is called when we need to load a library based upon a file name.
 */

static int
load_coff_library (int fd)
{
    struct linux_binprm *bprm;  /* Parameters for the load operation   */
    int    status;              /* Status of the request               */

	MOD_INC_USE_COUNT;

/*
 *  Read the first portion of the file.
 */
    bprm = (struct linux_binprm *) kmalloc (sizeof (struct linux_binprm),
					    GFP_KERNEL);
    if (0 == bprm) {
	printk (KERN_WARNING "COFF: kmalloc failed\n");
        status = -ENOEXEC;
    }
    else {
        struct file         *file;  /* Pointer to the file table           */
        struct pt_regs       regs;  /* Register work area                  */
#ifndef STACK_TOP
        int old_fs = get_fs ();     /* Previous FS register value          */
#endif

        memset (bprm, '\0', sizeof (struct linux_binprm));

	file           = current->FD[fd];
	bprm->inode    = file->f_inode;   /* The only item _really_ needed */
	bprm->filename = "";              /* Make it a legal string        */
#ifdef STACK_TOP
	status = read_exec (bprm->inode,
			    0L,
			    bprm->buf,
			    sizeof (bprm->buf), 1);
#else
	set_fs (get_ds ());   /* Make it point to the proper location    */
	status = read_exec (bprm->inode,	 /* INODE for file       */
			    0L,                  /* Offset in the file   */
			    bprm->buf,           /* Buffer for read      */
			    sizeof (bprm->buf)); /* Size of the buffer   */
	set_fs (old_fs);	                 /* Restore the selector */
#endif
/*
 *  Try to load the library.
 */
	status = load_object (bprm, &regs, 0);
/*
 *  Release the work buffer and return the result.
 */
	kfree (bprm);                 /* Release the buffer area */
    }

	MOD_DEC_USE_COUNT;

/*
 *  Return the result of the load operation
 */
    return (status);
}
