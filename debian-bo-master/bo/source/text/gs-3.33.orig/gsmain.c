/* Copyright (C) 1989, 1995 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* gsmain.c */
/* Common support for Ghostscript front ends */
#include "memory_.h"
#include "string_.h"
#include "ghost.h"
#include "gp.h"
#include "gsmatrix.h"			/* for gxdevice.h */
#include "gxdevice.h"
#include "errors.h"
#include "oper.h"
#include "idict.h"
#include "iname.h"			/* for name_init */
#include "dstack.h"
#include "estack.h"
#include "ostack.h"			/* put here for files.h */
#include "stream.h"			/* for files.h */
#include "files.h"
#include "ialloc.h"
#include "strimpl.h"		/* for sfilter.h */
#include "sfilter.h"		/* for iscan.h */
#include "iscan.h"
#include "main.h"
#include "store.h"
#include "isave.h"		/* for prototypes */
#include "interp.h"
#include "ivmspace.h"

/*
 * This is the only place in Ghostscript that calls 'exit'.  Including
 * <stdlib.h> is overkill, but that's where it's declared on ANSI systems.
 * We don't have any way of detecting whether we have a standard library
 * (some GNU compilers perversely define __STDC__ but don't provide
 * an ANSI-compliant library), so we check __PROTOTYPES__ and
 * hope for the best.
 */
#ifdef __PROTOTYPES__
#  include <stdlib.h>		/* for exit */
#else
extern void exit(P1(int));
#endif

/* ------ Exported data ------ */

/* The only reason we export gs_exit_status is so that window systems */
/* with alert boxes can know whether to pause before exiting if */
/* Ghostscript terminates with an error.  There must be a better way .... */
uint gs_memory_chunk_size = 20000;
int gs_exit_status;
int gs_user_errors;

/* File name search paths */
const char **gs_lib_paths;
private int gs_lib_count;
char *gs_lib_env_path;

/* Define whether or not to look in the current directory first. */
/* This is wrong by any reasonable criterion, but users insist on it. */
#ifndef SEARCH_HERE_FIRST
#  define SEARCH_HERE_FIRST 1
#endif

/* ------ Imported data ------ */

/* Configuration information imported from gconfig.c and iinit.c. */
extern gx_device *gx_device_list[];
extern const char *gs_lib_default_path;
extern const char *gs_init_file;
extern ref gs_init_file_array[];
extern ref gs_emulator_name_array[];

/* Imported from gsmisc.c */
extern FILE *gs_debug_out;

/* Imported from gsmemory.c */
void gs_malloc_init(P0());
void gs_malloc_release(P0());

/* ------ Forward references ------ */

private int gs_run_init_file(P2(int *, ref *));
int gs_run_file_open(P2(const char *, ref *));

/* ------ Initialization ------ */

/* Remember how much initialization has been done. */
private int init1_done, init2_done;

/* A handy way to declare and execute an initialization procedure: */
#define call_init(proc)\
{ extern void proc(P0()); proc(); }

/* Initialization to be done before anything else. */
void
gs_init0(FILE *in, FILE *out, FILE *err, int max_lib_paths)
{	/* Set the Ghostscript versions of stdin/out/err. */
	gs_stdin = in;
	gs_stdout = out;
	gs_stderr = err;
	gs_debug_out = gs_stdout;
	/* Do platform-dependent initialization. */
	/* We have to do this as the very first thing, */
	/* because it detects attempts to run 80N86 executables (N>0) */
	/* on incompatible processors. */
	gp_init();
	/* Initialize the file search paths */
	gs_malloc_init();
	gs_lib_env_path = 0;
	gs_lib_paths =
		(const char **)gs_malloc(max_lib_paths + 4, sizeof(char *),
					 "gs_lib_paths array");
	gs_lib_count = 0;
#if SEARCH_HERE_FIRST
	gs_add_lib_path(gp_current_directory_name);
#endif
	gs_user_errors = 1;
	gs_log_errors = 0;
	/* Reset debugging flags */
	memset(gs_debug, 0, 128);
	init1_done = init2_done = 0;
}

/* Initialization to be done before constructing any objects. */
void
gs_init1(void)
{	if ( !init1_done )
	{	{	extern bool gs_have_level2(P0());
			ialloc_init(&gs_memory_default, gs_memory_chunk_size,
				    gs_have_level2());
			alloc_save_init(idmemory);
		}
		name_init(imemory_system);
		call_init(obj_init)		/* requires name_init */
		call_init(scan_init)		/* ditto */
		init1_done = 1;
	}
}

/* Initialization to be done before running any files. */
private void
init2_make_string_array(ref *srefs, const char *aname)
{	ref *ifp = srefs;
	ref ifa;
	for ( ; ifp->value.bytes != 0; ifp++ )
	  r_set_size(ifp, strlen((const char *)ifp->value.bytes));
	make_tasv(&ifa, t_array, a_readonly | avm_foreign,
		  ifp - srefs, refs, srefs);
	initial_enter_name(aname, &ifa);
}
void
gs_init2(void)
{	gs_init1();
	if ( !init2_done )
	   {	int code, exit_code;
		ref error_object;
		call_init(igs_init)
		call_init(zop_init)
		{	extern void gs_iodev_init(P1(gs_memory_t *));
			gs_iodev_init(imemory);
		}
		call_init(op_init)	/* requires obj_init, scan_init */

		/* Set up the array of additional initialization files. */
		init2_make_string_array(gs_init_file_array, "INITFILES");
		/* Set up the array of emulator names. */
		init2_make_string_array(gs_emulator_name_array, "EMULATORS");

		/* Execute the standard initialization file. */
		code = gs_run_init_file(&exit_code, &error_object);
		if ( code < 0 )
		{	if ( code != e_Fatal )
				gs_debug_dump_stack(code, &error_object);
			gs_exit_with_code((exit_code ? exit_code : 2), code);
		}
		init2_done = 1;
	   }
   }

/* Add a library search path to the list. */
void
gs_add_lib_path(const char *lpath)
{	gs_lib_paths[gs_lib_count] = lpath;
	gs_lib_count++;
	gs_set_lib_paths();
}

/* ------ Execution ------ */

/* Complete the list of library search paths. */
void
gs_set_lib_paths(void)
{	const char **ppath = &gs_lib_paths[gs_lib_count];
	if ( gs_lib_env_path != 0 ) *ppath++ = gs_lib_env_path;
	if ( gs_lib_default_path != 0 ) *ppath++ = gs_lib_default_path;
	*ppath = 0;
}

/* Open a file, using the search paths. */
int
gs_lib_open(const char *file_name, ref *pfile)
{	/* This is a separate procedure only to avoid tying up */
	/* extra stack space while running the file. */
#define maxfn 200
	byte fn[maxfn];
	uint len;
	return lib_file_open(file_name, strlen(file_name), fn, maxfn,
			     &len, pfile);
}

/* Open and execute a file. */
int
gs_run_file(const char *file_name, int user_errors, int *pexit_code, ref *perror_object)
{	ref initial_file;
	int code = gs_run_file_open(file_name, &initial_file);
	if ( code < 0 ) return code;
	return gs_interpret(&initial_file, user_errors, pexit_code, perror_object);
}
int
gs_run_file_open(const char *file_name, ref *pfref)
{	gs_set_lib_paths();
	if ( gs_lib_open(file_name, pfref) < 0 )
	{	eprintf1("Can't find initialization file %s.\n", file_name);
		return_error(e_Fatal);
	}
	r_set_attrs(pfref, a_execute + a_executable);
	return 0;
}

/* Open and run the very first initialization file. */
private int
gs_run_init_file(int *pexit_code, ref *perror_object)
{	ref *prinit;
	ref ifile;
	ref first_token;
	int code;
	scanner_state state;
	/* We know that .initialfile is defined in systemdict */
	/* as an operator, and that it pushes a file on the o-stack. */
	dict_find_string(systemdict, ".initialfile", &prinit);
	gs_set_lib_paths();
	code = (*real_opproc(prinit))(osp);
	if ( code < 0 )
	  {	*pexit_code = 255;
		return code;
	  }
	ifile = *osp--;
	/* Check to make sure the first token is an integer */
	/* (for the version number check.) */
	scanner_state_init(&state, false);
	code = scan_token(ifile.value.pfile, &first_token, &state);
	if ( code != 0 || !r_has_type(&first_token, t_integer) )
	{	eprintf1("Initialization file %s does not begin with an integer.\n", gs_init_file);
		*pexit_code = 255;
		return_error(e_Fatal);
	}
	*++osp = first_token;
	r_set_attrs(&ifile, a_executable);
	return gs_interpret(&ifile, gs_user_errors, pexit_code, perror_object);
}
/* Operator to open and return the initial file. */
/* Precompiled initialization can redefine this. */
private int
zinitialfile(os_ptr op)
{	int code = gs_run_file_open(gs_init_file, (ref *)(op + 1));
	if ( code < 0 )
		return code;
	push(1);
	return 0;
}

/* Run a string. */
int
gs_run_string(const char *str, int user_errors,
  int *pexit_code, ref *perror_object)
{	return gs_run_string_with_length(str, (uint)strlen(str), user_errors,
					 pexit_code, perror_object);
}
int
gs_run_string_with_length(const char *str, uint length, int user_errors,
  int *pexit_code, ref *perror_object)
{	stream ss;
	ref fref;
	int code;

	gs_set_lib_paths();
	code = file_read_string((const byte *)str, length, &ss, &fref);
	if ( code < 0 ) return code;
	r_set_attrs(&fref, a_executable);
	code = gs_interpret(&fref, user_errors, pexit_code, perror_object);
	file_close(&fref);
	return code;
}

/* ------ Termination ------ */

/* Free all resources and exit. */
void
gs_finit(int exit_status, int code)
{	gx_device **pdev = gx_device_list;
	gs_exit_status = exit_status;	/* see above */
	fflush(stderr);			/* in case of error exit */
	for ( ; *pdev != 0; pdev++ )
	{	if ( (*pdev)->is_open )
			gs_closedevice(*pdev);
	}
	/* Do the equivalent of a restore "past the bottom". */
	/* This will release all memory, close all open files, etc. */
	if ( init1_done )
		alloc_restore_all(idmemory);
	/* Do platform-specific cleanup. */
	gp_exit(exit_status, code);
}
void
gs_exit_with_code(int exit_status, int code)
{	gs_finit(exit_status, code);
	/* Here is where we fix up the exit code if we are running on VMS. */
#if exit_OK != 0 || exit_FAILED != 1
	switch ( exit_status )
	  {
	  case 0:
	    exit(exit_OK);
	  case 1:
	    exit(exit_FAILED);
	  }
#endif
	exit(exit_status);
}
void
gs_exit(int exit_status)
{	gs_exit_with_code(exit_status, 0);
}

/* ------ Debugging ------ */

/* Debugging code */
extern void debug_print_ref(P1(const ref *));
extern void debug_dump_stack(P2(const ref_stack *, const char *));

/* Dump the stacks after interpretation */
void
gs_debug_dump_stack(int code, ref *perror_object)
{	zflush(osp);	/* force out buffered output */
	dprintf1("\nUnexpected interpreter error %d.\n", code);
	if ( perror_object != 0 )
	{	dputs("Error object: ");
		debug_print_ref(perror_object);
		dputc('\n');
	}
	debug_dump_stack(&o_stack, "Operand stack");
	debug_dump_stack(&e_stack, "Execution stack");
}

/* Log an error return.  We always include this, in case other */
/* modules were compiled with DEBUG set. */
#undef gs_log_error		/* in case DEBUG isn't set */
int
gs_log_error(int err, const char _ds *file, int line)
{	if ( gs_log_errors )
	  { if ( file == NULL )
	      dprintf1("Returning error %d.\n", err);
	    else
	      dprintf3("%s(%d): Returning error %d.\n",
		       (char *)file, line, err);
	  }
	return err;
}

/* ------ "Operator" definition ------ */

BEGIN_OP_DEFS(gsmain_op_defs) {
	{"0.initialfile", zinitialfile},
END_OP_DEFS(0) }
