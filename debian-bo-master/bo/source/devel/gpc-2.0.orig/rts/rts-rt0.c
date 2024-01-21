/* Copyright (C) 1991,1995 Free Software Foundation, Inc.

   This file is part of GNU Pascal Library.

   Top level entrypoint and RTS initialization. Interrupt handlers.

The GNU Pascal Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU Pascal Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU Pascal Library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

# include "rts.h"
# include <sys/stat.h>
# include "rts-fdr.h"

# define SIGTOMASK(n) (1<<((n) - 1))
# define NASSOC	32

int     Gpc_signal    = 0; /* Use Gpc_sigcause if this signal appears */
int     Gpc_sigcause  = 0; /* Number of error message to give if Gpc_signal */
int	Gpc_debug     = 0;
int	Gpc_warn      = 0; /* if nonzero output runtime warnings */
int	Gpc_DEVNULL;
int	Gpc_argc;
char	**Gpc_argv,**Gpc_envp;
char*	_p_GLOBAL[GLOBAL_SIZE];	/* this should be done otherwise */

/* A list of saved constructors to be run */
CONSTRUCTOR *Gpc_c_list = (CONSTRUCTOR *)NULL;

/* This is set nonzero when the constructors are collected */
int _p_collect_flag= 1;

/* Set to 1 if you want to use the EOLN hack by default.
 *
 * If 0: EOLN will validate the file buffer if tst_UND
 *
 * If 1, when EOLN is tested when all of the following TRUE
 *       1) tst_UND
 *	 2) tst_EMPTY (nothing has been read after reset)
 *	 3) tst_TXT
 *	 4) tst_TTY
 *	 5) tst_LGET
 *
 *       If these are TRUE, then the EOLN test return TRUE.
 *
 *       If the EOLN is *not tested* it is FALSE. This is
 *       to make the program with 'if eoln then readln;' in the
 *       very beginning work, they skip the eoln when they test
 *       it, if you don't test, you don't have to skip it.
 */
int _p_eoln_reset_hack = 0;

/* 1 if direct access routines should work only for direct
 * access files.
 */
int _p_force_direct_files = 0;

/* Program standard input and output */
struct Fdr _p_stdin;
struct Fdr _p_stdout;

FILE *current_stdin;

/* FDR list head pointers.
   Add an FDR to the list when reset/rewritten, remove it on close.
   List is needeed to flush buffered output to terminal
    1) when program crashes, dump everything before giving error message
    2) when something is read from terminal opened as a TEXT file
       (need to flush output from other TTY files before get)
 */

FDR LastFdr  = NULL;
FDR FirstFdr = NULL;

/* Prefix for the temporary file names */
char	*Gpc_tmpname = "/tmp/Gpc";

#ifdef BSD_RTS
static void	Gpc_handler();
static int	sigvecn();
#endif

extern char	*optarg;
extern int 	 optind;
extern int	 opterr;

assoc	*Gpc_assoc;

static void
clear_junk()
{
    _p_fflush(FALSE);
}

void
_p_initialize (argc,argv,envp)
int argc;
char **argv;
char **envp;
{
	int	c;
	int	eflag = 0;
	assoc	*ap;
	char	*p;
	int skip_args = 0;
	int noskip = 0;
	int no_constructors = 0;
	FILE  *StdFile = 0;    /* -i option strings written here, and given as */
			       /*  standard input for user program */	

#if 0
	malloc_init (0, _p_malloc_warning);
#endif
	_p_init_heap_alignment ();

	Gpc_assoc = (assoc*)_p_malloc(NASSOC * sizeof(assoc));
	ap = Gpc_assoc;
	ap->int_name = NULL;

	/* Simple options for the run time system from command line
	 * Since the normal use of the command line is pass args to
	 * the user program, passing args to the runtime system
	 * is made somewhat complicated:
	 * First arg has to be:
	 *
	 * -Grts
	 * 
	 * Other flags that the rts recognizes (if the first
	 * arg is -Grts are output with '-h' option (see below).
	 *
	 * --		   : indicates end of rts parameters.
	 */ 
	 
	opterr = 0;
	if (argc > 1 && strcmp (argv[1], "-Grts") == 0)
	  while ((c = getopt (argc,argv, "edwfsNhi:a:G:")) != EOF) {
	    skip_args++;
	    switch (c) {
	    case 'G':
	        if (skip_args != 1 || strcmp (optarg, "rts"))
		  {
		    /* Arg is not for us, so get out */
		    optind--;
		    goto wilderness;
		  }
		break;

	    case 's':
		noskip++;
		break;

	    case 'e':
		_p_eoln_reset_hack = !_p_eoln_reset_hack;
		D(1, fprintf(stderr,"Special EOLN handling after rset turned %s\n",
			     _p_eoln_reset_hack ? "ON" : "OFF"));
		break;

	    case 'f':
		_p_force_direct_files = !_p_force_direct_files;
		D(1, fprintf(stderr,"Forcing direct access files: %s\n",
			     _p_force_direct_files ? "ON" : "OFF"));
		break;

	    case 'N':
		no_constructors++;
		break;

	    case 'i':
		if (!StdFile)
		  {
		    char namebuf[ 100 ];

		    _p_makename(namebuf);

		    D(1, fprintf(stderr,"Opening file %s for -i options\n",
				 namebuf));
		    if (!(StdFile = fopen(namebuf, "w+"))) {
			_p_error(REPORT,
				"Can't open option file for writing (-i)");
			continue;
		    }
		    D(1, fprintf(stderr,"stdin will read from file %s\n",
				 namebuf));
		    unlink (namebuf);
		  }

		D(1,fprintf(stderr,"-i option line: '%s'\n", optarg));
		fputs(optarg, StdFile);
		fputs("\n", StdFile);
		break;

	    case 'd':
		Gpc_debug++;
		break;
	    case 'w':
		Gpc_warn++;
		break;
	    case 'a':
		if ((p = (char *)index(optarg,':'))) /* Assignment */
		  {
		    *p++ = '\0';
		    ap->int_name = optarg;
		    ap->ext_name = p;
		    D(1,fprintf(stderr,"Associated file: %s -> %s\n",
				ap->int_name, ap->ext_name));
		    ap++;
		    if (ap >= &Gpc_assoc[NASSOC])
			_p_error(REPORT, "Too many associated file names (-a)");
		    else
			ap->int_name = NULL;
		}
		break;
	    case 'h':
		eflag++;
		break;
	    default:
		optind--;
		goto wilderness;
	    }
	  }
      wilderness:;

#ifdef HAVE_SETLINEBUF
	setlinebuf(stderr);
#endif
	if (eflag)
	  {
	    _p_error(REPORT,"Allowed Gnu Pascal program command line options for run time system:");
	    _p_error(REPORT," -h : Give this help text and exit(1)");
	    _p_error(REPORT," -d : Debug flag (one or more) Internal RTS reports");
	    _p_error(REPORT," -w : Give runtime warning messages");
	    _p_error(REPORT," -e : toggle EOLN handling right after reset for TTY");
	    _p_error(REPORT," -f : toggle forcing direct access files");
	    _p_error(REPORT," -s : Let the program see RTS command line arguments");
	    _p_error(REPORT," -i : Each option makes one line to standard input");
	    _p_error(REPORT," -a : Associate file names. -a Pfile:extname");
	    _p_error(REPORT," -N : don't run the module initializer code (DEBUG)");
	    _p_error(REPORT," -- : Rest of the args are not for the run time system");
	    exit (1);
	  }

	Gpc_argc = argc;
	Gpc_envp = envp;
	Gpc_argv = argv;

	/* Make run time system args invisible to the program
	 *
	 * I am not certain that you want this.
	 * If you don't, give the '-s' parameter.
	 */
	if (skip_args && !noskip)
	  {
	    int i = 1;		/* leave arg 0 as it is */
	    int valid = optind;
	    
	    for (; i <= (Gpc_argc-optind); i++)
	      Gpc_argv[ i ] = Gpc_argv [ valid++ ];
	    
	    Gpc_argc = Gpc_argc - optind + 1;
	  }

	{
	    struct stat stinfo;
	    if (stat(NULL_DEVICE_NAME, &stinfo) == 0)
		Gpc_DEVNULL = stinfo.st_rdev;
	    else
		Gpc_DEVNULL = ILLINT;
	}
#ifdef BSD_RTS
	sigvecn(SIGTOMASK(SIGHUP)
		|SIGTOMASK(SIGINT)
		/* |SIGTOMASK(SIGQUIT) */
		|SIGTOMASK(SIGILL)
		|SIGTOMASK(SIGTRAP)
		|SIGTOMASK(SIGIOT)
		|SIGTOMASK(SIGEMT)
		|SIGTOMASK(SIGFPE)
		|SIGTOMASK(SIGBUS)
		|SIGTOMASK(SIGSEGV)
		|SIGTOMASK(SIGSYS)
		|SIGTOMASK(SIGPIPE)
		|SIGTOMASK(SIGTERM)
		|SIGTOMASK(SIGXCPU)
		|SIGTOMASK(SIGXFSZ),Gpc_handler);
#endif

	current_stdin = stdin;

	if (StdFile)
	  {
	    /* Rewind the -i ARG file */
	    rewind(StdFile);

	    /* Use this for standrad input */
	    current_stdin = StdFile;
	  }
	D(1, fprintf(stderr, "%s\n", gpc_rts_version));

	{
	  char	*p = _p_malloc(1);
	  GLOBAL_P(G_NPB) = p;
	  GLOBAL_P(G_NPE) = NULL;
	}

	/* Call an empty routine; user might want to execute some code here */
	_p_setup ();

	/* Initialize the standard input and output */
	_p_initialize_std_files ();

	/* Run possible pascal module initializers */
	if (Gpc_c_list && !no_constructors)
	  _p_run_constructors ();
}


/* One time routine that restores the
 * original standard input if we are not reading from there now.
 *
 * -i ARG uses this.
 */
int
_p_restore_stdin (File)
FDR File;
{
  if (current_stdin == stdin)
    return 0;


  m_FILNUM (File) = current_stdin = stdin;

  return 1;
}

/* This is called when program ends normally */
void
_p_finalize ()
{
  _p_fflush(FALSE);

  /* Call an empty routine, user might want to execute some code here */
  _p_final ();
  exit(0);
}

void
_p_fflush(only_ttys)
int only_ttys;	/* TRUE if flushing only terminals */
{
    FDR	scan;

    /* DON't flush stdin; SunOS starts to behave strangely :-) */
    fflush(stdout);
    fflush(stderr);

    /* flush buffers to synchronize output messages */
    for (scan = FirstFdr; scan; scan = m_NXTFDR(scan)) {
	if (m_STATUS(scan) & FiWRI && (!only_ttys || tst_TTY(scan))) {
	    fflush(m_FILNUM(scan));
	    D(3, fprintf(stderr, "Flushed output file %s\n",m_NAM(scan)));
	}
    }
}

#ifndef HAVE_STRDUP
char *
_p_strdup (p)
     char *p;
{
    char *ret;
    if (!p)
	return NULL;
    ret = _p_malloc (strlen(p)+1);

    (void) strcpy(ret, p);
    return ret;
}
#endif

void
_p_makename(str)
char *str;
{
    static int counter = 0;
    sprintf(str,"%s%05d.%05d",Gpc_tmpname,getpid(),counter++);
}

#ifdef BSD_RTS
int
sigvecn(sigs,handler)

int	sigs;
void	(*handler)();

{
	register int	i;
	int	omask;
	struct sigvec	sv,osv;
	int	error = 0;

	omask = sigsetmask(-1);

	sv.sv_handler = (void *)handler;
	sv.sv_mask = sigs;
	sv.sv_onstack = 0;

	for (i=0;i<32;i++) {
		if (sigs >> (i - 1) & 1) {
			if (sigvec(i,&sv,&osv)) error++;
		}
	}
	sigsetmask(omask);
	return(error);
}

struct sig_expl
{
	int	sig,how,code,gpcn;
	char	*expl;
}	expls[] = 
{
	{SIGHUP,  ABORT, NONE,		  UND, "hangup"},
	{SIGINT,  ABORT, NONE,		 FAST, "interrupt"},
	{SIGQUIT, ABORT, NONE,		  UND, "quit"},
#ifdef vax
	{SIGILL,  ABORT, ILL_RESAD_FAULT,  UND, "reserved adressing"},
	{SIGILL,  ABORT, ILL_PRIVIN_FAULT, UND, "privileged instruction"},
	{SIGILL,  ABORT, ILL_RESOP_FAULT,  507, "reserved operand"},
#else
	{SIGILL,  ABORT, NONE,		  UND, "illegal instruction"},
#endif
	{SIGTRAP, ABORT, NONE,		  UND, "trap"},
	{SIGIOT,  ABORT, NONE,		  UND, "iot"},
	{SIGEMT,  ABORT, NONE,		  UND, "emt"},
#ifdef vax
	{SIGFPE,  ABORT, FPE_INTOVF_TRAP,   47, "integer overflow trap"},
	{SIGFPE,  ABORT, FPE_INTDIV_TRAP,   45, "integer divide by zero trap"},
	{SIGFPE,  ABORT, FPE_FLTOVF_TRAP,  100, "floating overflow trap"},
	{SIGFPE,  ABORT, FPE_FLTDIV_TRAP,   44, "floating divide by zero trap"},
	{SIGFPE, REPORT, FPE_FLTUND_TRAP,  101, "floating underflow trap"},
	{SIGFPE,  ABORT, FPE_DECOVF_TRAP,  UND, "decimal overflow"},
	{SIGFPE,  ABORT, FPE_SUBRNG_TRAP,  UND, "subscript"},
	{SIGFPE,  ABORT, FPE_FLTOVF_FAULT, 100, "floating overflow fault"},
	{SIGFPE,  ABORT, FPE_FLTDIV_FAULT,  44,"floating divide by zero fault"},
	{SIGFPE, REPORT, FPE_FLTUND_FAULT, 101, "floating underflow fault"},
#else
	{SIGFPE,  ABORT, NONE, 		  UND, "floating point trap"},
#endif
	{SIGKILL, ABORT, NONE,		  UND, "kill"},
	{SIGBUS,  ABORT, NONE,		  UND, "bus error"},
	{SIGSEGV, ABORT, NONE,		  UND, "memory fault"},
	{SIGSYS,  ABORT, NONE,		  UND, "bad system call"},
	{SIGPIPE, ABORT, NONE,		  UND, "broken pipe"},
	{SIGALRM, ABORT, NONE,		  UND, "alarm"},
	{SIGTERM, ABORT, NONE,		  UND, "termination"},
	{SIGURG,  ABORT, NONE,		  UND, "urgent"},
	{-1,      ABORT, NONE,		  0,   0}
};

static void
Gpc_handler(sig,code,scp)
int	sig,code;
struct sigcontext	*scp;
{
	struct sig_expl	*p;
	int Warning;
	char *msg;

	for(p = expls; p->sig >= 0; p++)
	{
		if (p->sig == sig && p->code == code)
			break;
	}
	if (p->sig < 0) p = 0;
	if (p && p->how == IGNORE) {
		return;
	}
	Warning = p && p->how == REPORT;
	D(1, fprintf(stderr, "Gpc_handler: Warning= %d\n", Warning));
	if (Gpc_signal == sig) {
	    msg = _p_errmsg(Gpc_sigcause); /* Preset message number */
	    Gpc_signal = 0;
	} else				/* Try to give something out */
	    msg = (p ? ((msg = _p_errmsg(p->gpcn))?msg:p->expl) : 0);
	_p_prmessage(msg,
		    (p ? p->gpcn : UND),
		    Warning);
	if (Warning) {
		return;
	}
	if (p && p->gpcn >= UND)
	    fprintf(stderr,"Exiting (with core dump)\n");
	else
	    fprintf(stderr,"Exiting\n");
	clear_junk();
	_cleanup(); /* In libc -> flsbuf.c */      
	sigvecn(SIGTOMASK(SIGILL),SIG_DFL);
        if (p && p->gpcn >= UND) {
	    sigsetmask(0);
	    kill(getpid(), SIGILL);
	}
	exit(1);
}
#endif /* BSD_RTS */
