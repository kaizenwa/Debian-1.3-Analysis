/* Declarations for Checker and for the API.
   Copyright 1994, 1995 Tristan Gingold.
		  Written March 1994 by Tristan Gingold.

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License 
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

The author may be reached by US/French mail:
		Tristan Gingold 
		8 rue Parmentier
		F-91120 PALAISEAU
		FRANCE
*/

#ifndef _CHECKER_H
#define _CHECKER_H

/* Note:  This file is used for the internals and for the API.  The API
  consists of functions that can be called by your program (to do garbage
  detection for example) and by your debugger (even if you don't need the
  declarartion.
  The API file, checker_api.h is build from checker.h: only lines between
  / * API * / and / * NOT-API * / (they must appears *alone* in a line,
  without blanks between the slahes and the asterisks.  */
  
/* NOT-API */
#if 0
/* API */
#include <stddef.h>
#ifndef PTR
#define PTR void *
#endif
#ifdef __GNUC__
#define ATTRIBUTE(x) __attribute__(x)
#else
#define ATTRIBUTE (x)
#endif
/* NOT-API */
#endif

#include "define.h"
#include "chkrlib.h"
#include "machine.h"

/* Sizes of the red zones, in bytes.  They must be a multiple of 4 or 8.
   These red zones are used BEfore and AFter the blocks allocated by malloc 
   and co.  They are designed to catch bad memory accesses.  */
extern unsigned int be_red_zone;
extern unsigned int af_red_zone;

/* Number of bytes handled by a state in a bitmap.  One means an off-by-one
   byte-level detected errors.  */
#ifndef bytes_per_state
extern unsigned int bytes_per_state;
#endif

/* These structures contains info for each binary file (the executable and
   the shared libs).  This is needed for garbage detection.
   The first objects are (in this order): ".text", ".data & .bss".
   These objects must be defined.  */
struct object
  {
    char *path;		/* path */
    char *name;		/* name of the segment */
    uint org;
    uint end;		/* [ orig; end [ */
    uint rights;
    struct object *next;
  };
  
/* Ored values for rights.  */
/* This section can be considered as a .data section.  The garbage detector
   reckons that pointers found in this section are root one.  This value must
   be ored with OBJECT_READ and OBJECT_WRIT.  */
#define OBJECT_DATA 1

/* This section can be considred as a .text section.  Must be ored with
   OBJECT_READ and OBJECT_EXEC.  */
#define OBJECT_TEXT 2

/* This section is readable.  */
#define OBJECT_READ 4

/* This section is writable.  */
#define OBJECT_WRIT 8

/* This section is executable.  */
#define OBJECT_EXEC 16

/* The head of the list.  */
extern struct object *objects;

/* Used if the pagesize is only known at start time.  */
#ifdef INIT_PAGESIZE
/* The pagesize value.  */
extern unsigned int pagesize;

/* 2**log_pagesize == pagesize.  */
extern unsigned int log_pagesize;
#endif

/* Descriptor for /dev/zero used to allocate anonymous memory.  */
#ifndef HAVE_ANONYMOUS
extern int devzero_fd;
#endif

#ifdef CHKR_SAVESTACK
/* Used by fs, mmap and shm: number of frames saved for the history.  */
#define HISTORY_DEPTH 8

/* Defined in config/PROCESSOR/OS/xxx.c.  */
/* Save or display the current history.
   If PTR is not null, PTR should be a pointer on a zone of
   NUM * SIZEOF (char *) bytes.  The function fill this zone with frames, with
   a null at the end.  If NUM == -1, it should only put the current IP (this
   is used by GET_CURRENT_IP).
   If PTR is null, display the current history.
   FORGET is only used for aschecker on i486-linux.  */
void chkr_get_history (PTR *ptr, int forget, int num);

/* A configuration dependant function called during the initialisation.  */
void chkr_initialize (void);

/* Search pointers in the stack or in registers (This is for the garbage
   detector).  */
void search_stack (void);
void search_register (void);

/* Function called by search_* to 'register' a pointer found.  */
void chkr_address (PTR ptr, int sure);

/* Machine dependant stuff during initialisation.  */
extern void chkr_init_machine (int *argc, char *argv[], char *envp[]);

/* Get infos from the binary file and shared libraries, if any.  */
extern void init_main_object (int linked, char *argv0, int nlibs, char **libs);

/* The entry point of Checker, defined in parse-args.c  */
extern void ___chkr_init_chkr (int linked, int nlibs, char **libs, int argc, char *argv[], char *envp[]);

/* Initialize malloc and co.  */
extern void init_morecore (void);

/* Defined in find_exec.c.  */
/* Find the full name of the program.  */
char *chkr_find_executable (char *file);

/* Defined in symtab.c.  */
/* Load the symbol table in memory.  */
void chkr_load_symtab (void);

/* Print the function name, file and line corresponding to ptr, according to
   the symbol table which must have been loaded.  */
void chkr_show_addr (PTR ptr);

/* This function compares an history with the current one.  */
extern int same_history (char **funcs, int nbr_funcs);

/* The current stack pointer.  */
extern PTR known_stack_limit;
#endif /* CHKR_SAVESTACK */

#ifndef MDCHECKER

/* API */

/* These are the right in the bitmap.  A right is a 2 bits number.  */
/* At first the real right, which could be stored in a bitmap.  */
/* No rights:  can't neither be read nor written.  */
#define CHKR_UN	0
/* Read only: can only be read.  */
#define CHKR_RO	1
/* Write only: can only be written.  */
#define CHKR_WO	2
/* Read and write: can be read or written.  */
#define CHKR_RW 3

/* These are pseudo-right, used to test the bitmap.  See chkr_check_addr.  */
/* Test for write, but no evolution.  See chkr_check_addr.  */
#define CHKR_TW 6
/* Test for write and mark write-only.  */
#define CHKR_MW	10
/* Test for not a red zone.  */
#define CHKR_NOT_RZ 16

/* The real bits to consider.  */
#define CHKR_MASK 3

/* The exec right.  Never stored in a bitmap but in a descriptor.  */
#define CHKR_EXEC 4

/* NOT-API */

/* True if the text segment is writable.  */
extern int is_text_writable;

/* The size of the null zone.  */
extern unsigned int null_pointer_zone;

/* Standard functions to handle the bitmap.  Defined in maccess.c.  */
/* Set rights.  */
void chkr_set_right (const PTR ptr, int len, int right);

/* Check rights.  */
void chkr_check_addr (const PTR ptr, int len, int right);

/* Check the execution right.  */
void chkr_check_exec (const PTR ptr);

/* Check rights for a string.  */
void chkr_check_str (const PTR ptr, int right);

/* Check the alignment.  */
void chkr_check_align (const PTR ptr, int align, int size);

/* Copy the bitmap.  */
void chkr_copy_bitmap (PTR dest, PTR src, unsigned int len);

/* Register the current stack pointer.  */
void chkr_stack_pointer (PTR sp);

/* Check for not being a red zone.  */
void chkr_assert_not_red_zone (const PTR ptr, int len);

#if GCCCHECKER
/* API */
void stubs_chkr_set_right (const PTR ptr, int len, int right);
int stubs_chkr_check_addr (const PTR ptr, int len, int right, char *mes);
int stubs_chkr_check_exec (const PTR ptr, char *mes);
int stubs_chkr_copy_bitmap (PTR dest, PTR src, unsigned int len, char *mes);
int stubs_chkr_check_str (const PTR ptr, int right, char *mes);
void stubs_chkr_assert_not_red_zone (const PTR ptr, int len, char *mes);
/* NOT-API */
#endif
#endif /* !MDCHECKER */

#ifdef CHKR_PROFILE
/* Display profile informations.  */
void display_profile (void);

/* True if profile is enabled.  It doesn't spend time but will display
   informations before exiting.  */
extern int profile_flag;

/* Total time spend in the garbage detector.  */
extern struct timeval tv_garbage;
#endif

/* API */

/* These functions are defined in error.c.  */
/* Fonction called ito abort.  */
void chkr_abort (void) ATTRIBUTE((noreturn));

/* Report an error.  Disp the header and the call chain.  */
int chkr_report (int i);

/* Just print the header message for an error.  */
int chkr_perror (int e);

/* Print the current call chain (aka history).  */
int chkr_disp_call_chain (void);

/* NOT-API */

/* Convert an error name (3 letters) to a number.  */
int get_error_type_by_name (const char *name);

/* Parse a `--disable=' option.  */
int parse_disable (const char *str);

/* The function to call to disable a range of address.  */
int register_disable_range (uint low, uint high);

/* Convert a number to a error name.  */
char *get_error_name_by_number (int num);

/* Record an error suppression.  */
void record_suppress (int type, int n_funcs);
int parse_suppress (char *str);

/* Get for error TYPE the number of suppression.  */
int get_max_nbr_funcs_suppressed (int type);

/* Return true if the error is disabled.  */
int is_error_disable (int e);

/* Disable error E.  */
void disable_error (int e);

/* The errno for Checker.  Avoid confusion.  */
extern int chkr_errno;

/* Defined in find-exec.c.  Save in memory the pathname to the executable.  */
char *copy_of_exec_file (char *s);

/* Defined in parse-args.c.  */
/* True if Checker has already been initialized.  */
extern int chkr_is_init;

/* Function to call just before exiting.  */
void chkr_do_end (void);

/* Convert a string to a number.  */
int atod (const char *c);

/* Check the output descriptor.  */
void check_output_spec (void);

/* API */

/* Stupid function to call after each error reports.  To be breakpointed.  */
void __chkr_pause (void);

/* Cleaning functions after important syscalls.  They mainly update my_pid*
   and gather statistics.  */
void chkr_clean_after_fork (void);
void chkr_clean_before_exec (void);

/* NOT-API */

/* The pathname to the executable.  */
extern char *chkr_prog_path;

/* Number of --verbose in the option lines.  */
extern int flag_verbose;

/* If true, annotate messages in order to be read/analysed by a tool.  */
extern int flag_annotate;

/* If true, change the rights after a warning in order to disable following
   warnings.  */
extern int flag_warn_once;

/* These two addresses delimit the heap(s).  Used by the garbage detector:
   only the pointer on an address between these two value are valid.  */
extern PTR low_addr_heap;
extern PTR high_addr_heap;

/* Set malloc(0) behavior.  Defined in utils.c, called by parse-args.c.  */
void set_malloc0_behavior (const char *opt);

/* Defined in smalloc.c.  */
/* Malloc functions, to be used internally.  */
PTR sys_malloc (size_t real_size);
void sys_free (PTR ptr);
PTR sys_realloc (PTR ptr, size_t size);
PTR sys_dupalloc (PTR ptr);

/* Stuff about pid.  Defined in parse-args.c.  */
/* Cached value of getpid().  */
extern pid_t my_pid;

/* Ascii string of getpid().  */
extern char my_pid_c[6];

/* The size of the aged block queue.  */
extern uint aged_queue_length;

/* If true, each call to malloc, free... are displayed.  */
extern uint trace_malloc_flag;

#ifndef MDCHECKER
/* Defined in fd.c.  */
/* Initialize the fd manager.  */
void init_fds (void);

/* API */

/* Tell the manager that the program is using FD.  */
int fd_used_by_prog (int fd);

/* Tell the manager that the system has returned a new file descriptor FD.  */
int fd_returned_by_system (int fd);

/* Tell the manager that FD has been closed.  */
int fd_closed (int fd);

/* Tell the manager about dup() and dup2().  */
int fd_duped (int fdres, int fdd);

/* NOT-API */

/* Allocate from the manager a file descriptor.  */
int fd_alloc_reserved (char *comment);

/* Release a file descriptor.  */
void fd_free_reserved (int fd);
#endif /* !MDCHECKER */

/* Defined in end.c, begin.c, end-stubs.c, begin-stubs.c  */
/* Variables and functions to delimit the Checker code.  */
extern int checker_data_begin;
extern void checker_text_begin (void);
extern int checker_data_end;
extern void checker_text_end (void);
#ifdef GCCCHECKER
extern int checker_data_stubs_begin;
extern void checker_text_stubs_begin (void);
extern int checker_data_stubs_end;
extern void checker_text_stubs_end (void);
#endif

/* Defined in signal.c  */
#ifndef NO_SIGNALS
/* True if the CPU is executed Checker code.  */
extern int chkr_in_checker;

/* True if a signal is being delayed.  */
extern int chkr_sig_catched;

/* Function to send delayed signals.  */
extern void chkr_send_delayed_signals (void);

/* The new sigaction syscall called by stubs-syscall.c  */
extern int user_sigaction (int sig, const struct sigaction *act, struct sigaction *oldact);

/* Parse --Wsignal or --Wno-signal.  Defined in signal.c.  */
void parse_signal_warn (int flag, char *name);

/* Initialization of signal.c  */
void save_signals (void);
#endif /* !NO_SIGNALS */

/* time.c */
void disp_date (void);

/* API */

/* Defined in maccess.c, utils.c and garbage.c  */
struct mdesc;
int __chkr_read_bitmap (const PTR ptr, unsigned char *bm, int len);
int __chkr_write_bitmap (const PTR ptr, unsigned char *bm, int len);
void __chkr_disp_map (void);
void __chkr_disp_right (const PTR ptr, int len);
void __chkr_check_mdesc (struct mdesc *mdp);
void __chkr_check_intern (void);
void __chkr_disp_heaps (void);
void __chkr_disp_heapgraph (struct mdesc *mdp);
void __chkr_disp_statistics (struct mdesc *mdp);
void __chkr_dump_heap (struct mdesc *mdp);
void __chkr_dump_free (struct mdesc *mdp);
void __chkr_dump_block (PTR block);
void __chkr_disp_shlibs (void);
void __chkr_disp_siginfo (void);
void __chkr_disp_fd (void);
void __chkr_dump_symtab (void);
void __chkr_dump_sbusy (void);
void __chkr_garbage_detector (void);
void __chkr_disp_inuse (void);
void __chkr_evol_detector (void);
void __chkr_clear_leaks (void);
void __chkr_evol_inuse (void);
void __chkr_clear_inuse (void);
void __chkr_dump_suppress (void);

/* NOT-API */
extern void remove_mmap (PTR addr, uint len);
extern void new_segmmap (PTR addr, uint len, int prot, int flags, int filedes, uint off);
extern void seg_mprotect (PTR addr, uint len, int prot);
extern void new_segshm (int shmid, PTR addr, int flags);
extern void remove_shm (PTR addr);
char *find_bmbase (PTR base);

#if 0

/* API */

extern PTR malloc (size_t size);
extern PTR realloc (PTR ptr, size_t size);
extern PTR calloc (size_t nmemb, size_t size);
extern void free (PTR ptr);
extern PTR memalign (size_t alignment, size_t size);
extern PTR valloc (size_t size);
extern PTR sbrk (int incr);

void chkr_clean_after_fork (void);
void chkr_clean_before_exec (void);

void chkr_printf (const char *message, ...) __attribute__ ((format (printf, 1, 2)));
void chkr_header (const char *message, ...) __attribute__ ((format (printf, 1, 2)));
extern int flag_weak_check_copy;

/* NOT-API */

#endif

/* API */

#endif /* checker.h  */
