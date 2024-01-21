/* Parse arguments for Checker.
   Copyright 1993, 1994, 1995 Tristan Gingold
   Written October 1993 by Tristan Gingold

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

#include <fcntl.h>
#include <sys/param.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <signal.h>
#include <limits.h>
#include <unistd.h>
#include "checker.h"
#include "errlist.h"
#include "message.h"

#ifdef NEED_ATEXIT_FOR_CHKR_DO_END
#include <stdlib.h>		/* only for atexit().  */
#endif

/* Your default home, if $HOME is unreadable.  */
#ifndef DEFAULT_HOME
#define DEFAULT_HOME "/tmp"
#endif

/* The configuration file.  */
#ifndef OPTIONS_FILE
#define OPTIONS_FILE "/.checker"	/* Never omit the '/'.  */
#endif

/* The output file if the standard one was closed.  */
#ifndef DEFAULT_OUT_FILE
#define DEFAULT_OUT_FILE "/dev/tty"
#endif

/* If true, the initial message is not displayed.  */
static int flag_silent = 0;

/* If true, the program is stopped with kill just before main().  This is 
   helpful for debugging.  */
static int flag_stop;

/* If true, annotate messages in order to be read/analysed by a tool.  */
int flag_annotate;

/* If true, change the rights after a warning in order to disable following
   warnings.  */
int flag_warn_once;

/* A copy of argv[0].  */
static char *full_progname;

/* A pointer to the basename of argv[0] in full_progname.  */
static char *progname;

/* The full path of the program.  */
char *chkr_prog_path;

/* Fd where all error are written.  */
int chkr_out;

#ifdef CHECK_OUTPUT_FILE
/* Some infos of the output file.  These are used to check if the user has
   tried to handle the output file (ie messages file).  */
static dev_t st_dev;
static ino_t st_ino;
#endif

/* The name of the output file.  CHKR_OUT_FILE is made from CHKR_OUT_PROTO.
   Its opening mode is set by CHKR_OUT_APPEND.  */
static char *chkr_out_file;

/* True if the report must be appended to CHKR_OUT_FILE.  */
static int chkr_out_append;

/* The original name of the output file set by '--output'.  Can contains 
   %p or %n.  This is used when CHKR_OUT_FILE must be remade because %p 
   or %n has changed (fork).  */
static char *chkr_out_proto;

/* True if the messages must be appended to the file.  */
static int flag_append;

/* True if the verbose flag was set.  */
int flag_verbose = 0;

/* Disable the signal manager.  Only for maintainers.  See signal.c.  */
int flag_no_signals = 0;

/* True if --disp-args.  Checker displays the args of your program.  */
static int flag_disp_args;

/* If set, emit a warning if sbrk is called.  */
int flag_warn_sbrk = 1;

/* If set, emit a warning if free is called with a null argument.  */
int flag_warn_free_null = 0;

/* True if warns each time the alignment arg for memalign() is not a
   power of 2.  */
int flag_warn_memalign;

/* If set, trace calls to malloc, free...  */
uint trace_malloc_flag;

/* If set, the stubs of memcpy, bcopy, memmove don't check the source and
   the destination, but only copy the bitmap (the destination must be
   writable.  */
int flag_weak_check_copy;

/* Version and target of Checker.  See version.c.  */
extern char *version;
extern char *target;

/* Saved pid in numeric (my_pid) and in characteres (my_pid_c[]).  */
pid_t my_pid;
char my_pid_c[6];

/* Size of the NULL zone.  */
unsigned int null_pointer_zone = 0x04;

/* Info on the binaries files of this processus (usually only the binary
   file).  */
struct object *objects = (struct object *) 0;

#ifdef NEED_CHKR_LD_OPTIONS
/* The address of the symbol is set according to the options.  */
extern void *___chkr_ld_opts__;

/* The options.  */
#ifdef __GNUC__
int chkr_ld_options = (int) &___chkr_ld_opts__;
#else
int chkr_ld_options;
#endif
#endif

/* True if must run the garbage detector just before exiting.  */
static int do_detector_at_end;

/* True if __chkr_evol_inuse() must be call at the end.  */
static int do_inuse_at_end;

#ifdef CHKR_SAVESTACK
/* True if the user don't want to use the symbols table.  */
int flag_no_symtab;
#endif

#ifdef CHKR_PROFILE
/* True if profile infos are displayed at the end.  */
int profile_flag = 0;
#endif

/* True if Checker has already been initialized.  */
int chkr_is_init;

/* This is for parse_args().  */
#define isspace(c) ((c)==' ' || (c)=='\t')

/* The cached pagesize.  */
#ifdef INIT_PAGESIZE
unsigned int pagesize;
unsigned int log_pagesize;
#endif

/* A descriptor for /dev/zero, used by mmap to allocate anonymous memory.  */
#ifndef HAVE_ANONYMOUS
int devzero_fd;
#endif

/* See garbage.c  */
extern uint leak_size_threshold;

static int parse_args (char *input, char *ouput);
static void parse_opt_line (char *line);
static void print_message (char **message);
static void read_config_file (char *file);
static int chkr_ctod (const char c);
#ifdef CHECK_OUTPUT_FILE
static void store_output_spec (void);
#endif
void make_pid (void);
static void update_output_file (void);

#include "parse-args.mes"

/* ___chkr_init_chkr is called before main() and aims at initializing 
   Checker.  The arguments are those of main().
   NOTE: this function must not call malloc().  Only sys_malloc is OK.  */
void
___chkr_init_chkr (int linked, int nlibs, char **libs,
		   int argc, char *argv[], char *envp[])
{
  static int already_init = 0;
  char *args;
  char cffile[MAXPATHLEN];

  /* ARGC is nul only if this program was called by ldd(1).  Display the
     message and exit.  */
  if (!already_init && argc == 0)
    {
      print_message (init_message);
      _exit (0);
    }

  /* If ___chkr_init_chkr() was already called, something strange has
     happend.  */
  if (already_init)
    {
      chkr_printf (M_ALREADY_INIT);
      chkr_abort ();
    }
  else
    already_init = 1;

#ifdef INIT_PAGESIZE
  /* If the pagesize value could only be known by a syscall, call it for
     ever.  */
  pagesize = INIT_PAGESIZE;
  log_pagesize = 0;

  while ((1 << log_pagesize) != pagesize && log_pagesize < 30)
    log_pagesize++;
  if ((1 << log_pagesize) != pagesize)
    {
      chkr_printf ("Bad PAGESIZE: 0x%08x\n", pagesize);
      chkr_abort ();
    }
#endif /* INIT_PAGESIZE */

#if 0
  chkr_printf ("argc: %d argv[0]: %s\n", argc, argv[0]);
#endif

  /* Save the pid: set my_pid and my_pid_c.  */
  make_pid ();

#ifdef NEED_CHKR_LD_OPTIONS
#ifndef __GNUC__
  chkr_ld_options = (int) &___chkr_ld_opts__;
#endif
#endif

#ifndef HAVE_ANONYMOUS
  /* Open a descriptor for /dev/zero.
     Must be done very urly to allow sys_malloc to be used.  */
  devzero_fd = open ("/dev/zero", O_RDWR);
  if (devzero_fd == -1)
    {
      chkr_printf ("Can't open `/dev/zero'\n");
      chkr_abort ();
    }
#endif /* !HAVE_ANONYMOUS */

  /* Dup2 stderr for Checker.  */
  {
    int chkr_new_out;

#ifndef MDCHECKER
    /* Save fd state.  */
    init_fds ();
    chkr_new_out = fd_alloc_reserved ("chkr_out");
#else
    chkr_new_out = OPEN_MAX - 1;
#endif

    /* Dup the current output file.  */
    dup2 (2, chkr_new_out);	/* 2 is stderr.  */
    chkr_out = chkr_new_out;
  }

  /* Use a reserved fd for /dev/zero.  */
#ifndef HAVE_ANONYMOUS
#ifndef MDCHECKER
  {
    int tmp_fd = fd_alloc_reserved ("/dev/zero");
    dup2 (devzero_fd, tmp_fd);
    close (devzero_fd);
    devzero_fd = tmp_fd;
  }
#endif /* !MDCHECKER */
#endif /* !HAVE_ANONYMOUS */

  /* Set the output file name, where errors are send.  */
  chkr_out_file = sys_malloc (sizeof (DEFAULT_OUT_FILE) + 1);
  strcpy (chkr_out_file, DEFAULT_OUT_FILE);
  chkr_out_proto = sys_malloc (sizeof (DEFAULT_OUT_FILE) + 1);
  strcpy (chkr_out_proto, DEFAULT_OUT_FILE);

  /* Keep the progname name, if possible.  */
  if (argv && argv[0])
    {
      /* Find the full path of the program.  The full path is needed to read
         the symbols table.  */
#ifndef DONT_NEED_EXECUTABLE
      chkr_prog_path = chkr_find_executable (argv[0]);
#else
      chkr_prog_path = copy_of_exec_file (argv[0]);
#endif

      /* Save argv[0].  */
      full_progname = sys_malloc (strlen (argv[0]) + 1);
      strcpy (full_progname, argv[0]);

      /* Find the basename of argv[0].  */
      progname = full_progname + strlen (full_progname);
      while (progname != full_progname && progname[-1] != '/')
	progname--;
    }
  else
    {
      /* Use default values.  Note that the user will be able to set 
         chkr_prog_path with an option.  If chkr_prog_path is NULL, the OS
         could access to the symbol table if it provides a way to do this.
         This is OS-specific and usually based on the `/proc' file system.  */
      chkr_prog_path = NULL;
      full_progname = "<unknown>";
      progname = full_progname;
    }

  /* Parse each option of the environemnt variable CHECKEROPTS.  */
  args = chkr_getenv ("CHECKEROPTS");

  if (args != (char *) 0)
    parse_opt_line (args);

  /* Read `~/.checker' file and parse its options.  */
  args = chkr_getenv ("HOME");
  if (args != (char *) 0)
    strncpy (cffile, args, MAXPATHLEN - 10);
  else
    strcpy (cffile, DEFAULT_HOME);
  strcat (cffile, OPTIONS_FILE);
  read_config_file (cffile);

  /* Set objects.  */
  init_main_object (linked, argv[0], nlibs, libs);
  objects->path = chkr_prog_path ? chkr_prog_path : full_progname;

  /* Do machine dependant stuff.  */
  chkr_init_machine (&argc, argv, envp);

  /* Display the copyright and the initial message.  */
  if (!flag_silent)
    {
      chkr_printf (M_COPYRIGHT, version, target);
      print_message (init_message);
      chkr_header (M_IS_RUNNING, full_progname);
      disp_date ();
      chkr_printf ("\n\n");
    }

#ifdef NEED_CHKR_LD_OPTIONS
  /* If the program contains files not compiled with Checker, emit a warning.
   *  The linker ld know it and set ___chkr_ld_opts__ according to that.  */
  if (!flag_silent && (chkr_ld_options & 1))
    print_message (link_with_other_message);
#endif

  /* Some trivial checks.  ??_red_zone must be a multiple of BIGGEST_ALIGNMENT.
   */
  if (be_red_zone % BIGGEST_ALIGNMENT || af_red_zone % BIGGEST_ALIGNMENT)
    chkr_perror (M_I_BRS_MA_ET);

#ifndef MDCHECKER
  /* Initialize the bitmaps.  */
  known_stack_limit = (PTR) &argc;
  init_morecore ();
#endif

  /* If the full path of the program was not found, display a warning.
     This test must be done after parsing the options, since an option can
     set chkr_prog_path.  */
  if (chkr_prog_path == (char *) 0)
    {
      chkr_report (M_C_MES_CK_ET);
      chkr_printf (M_PRGNAME_NOT_FOUND);
      chkr_printf (M_ARGV0_IS, argv[0]);
    }

  /* Checker is now initialized.  */
  chkr_is_init = 1;

  /* Display the args.  */
  if (flag_disp_args)
    {
      int i;
      
      chkr_report (M_C_MES_CK_ET);
      chkr_puts ("argv: ");
      for (i = 0; i < argc; i++)
        chkr_printf ("%s%c", argv[i], i == argc - 1 ? '\n' : ' ');
      chkr_puts ("environment:\n");
      for (i = 0; envp[i]; i++)
        chkr_printf ("%s\n", envp[i]);
      chkr_puts ("\n");
    }
    
#if 1
  /* Disp libs if the verbose flag is set.  */
  if (flag_verbose)
    {
      __chkr_disp_shlibs ();
#ifndef MDCHECKER
      __chkr_disp_map ();
#endif
    }
#endif

  /* If the --stop option is set, stop now.  */
  if (flag_stop)
    {
      chkr_report (M_C_MES_CK_ET);
      chkr_printf (M_STOPPING_MYSELF, SIGCONT, my_pid);
      kill (my_pid, SIGSTOP);
    }

#ifdef CHECK_OUTPUT_FILE
  /* Save infos of the output file.  They will be used to check if the user
     has tried to handle the output file.  */
  store_output_spec ();
#endif

#ifndef NO_SIGNALS
  /* Save the signals handler/behavior so that they will be correctly 
     handled.  */
  save_signals ();
#endif

#ifdef NEED_ATEXIT_FOR_CHKR_DO_END
  /* Be sure chkr_do_end () is called before exiting.  Checker traps exit(2),
     so this is OK.  However with MDCHECKER or PLCHECKER, we must use atexit.
   */
  atexit (chkr_do_end);
#endif
}

/* Save the pid and fill my_pid_c.  */
void
make_pid (void)
{
  int i;
  pid_t pid;

  my_pid = pid = getpid ();
  my_pid_c[5] = '\0';
  pid %= 100000;
  for (i = 4; i >= 0; i--)
    {
      my_pid_c[i] = '0' + (pid % 10);
      pid /= 10;
    }
}

/* Extract the first field of INPUT and put it to OUTPUT.
   The metacharacters '"\ are recognized.
   Return the offset to the next field.
 */
static int
parse_args (char *input, char *output)
{
  char *buf;
  char *arg;
  int offset = 0;
  int squote = 0;		/* means ' */
  int dquote = 0;		/* means " */
  int bquote = 0;		/* means \ */

  arg = buf = alloca (strlen (input) + 1);

  /* Forget the first blanks.  */
  while (*input != '\0' && isspace (*input))
    {
      input++;
      offset++;
    }

  while (*input != '\0')
    {
      if (bquote)
	{
	  *arg++ = *input;
	  bquote = 0;
	}
      else if (*input == '\\')
	bquote = 1;
      else if (squote)
	{
	  if (*input == '\'')
	    squote = 0;
	  else
	    *arg++ = *input;
	}
      else if (dquote)
	{
	  if (*input == '\"')
	    dquote = 0;
	  else
	    *arg++ = *input;
	}
      else if (*input == '\'')
	squote = 1;
      else if (*input == '\"')
	dquote = 1;
      else if (isspace (*input))
	break;
      else
	*arg++ = *input;
      input++;
      offset++;
    }
  *arg = '\0';

  if (arg != buf)
    {
      strcpy (output, buf);
      return offset;
    }
  else
    return 0;
}

/* Array for the flags.  */
struct Opt
{
  char *long_flag;		/* long flag. (char*) 0 means end.  */
  short int short_flag;		/* short flag */
  char arg;			/* Is there an arg ? */
};

struct Opt options[] =
{
  {"silent", 's', 0},		/* no message at the begin */
  {"quiet", 'q', 0},		/* idem */
  {"help", 'h', 0},		/* display help */
  {"output", 'o', 1},		/* set the output file */
  {"image", 'i', 1},		/* set the image file */
  {"nosymtab", 'n', 0},		/* do not use symbol table */
  {"abort", 'a', 0},		/* abort */
  {"profile", 'p', 0},		/* display profile infos */
  {"NULLzone", 'N', 1},		/* set the size of the NULL zone */
  {"disable", 'd', 1},		/* disable an address */
  {"stop", 'S', 0},		/* stop before main, use SIGCONT to restart */
  {"detector", 'D', 1},		/* detector called at end */
  {"malloc0", 'm', 1},		/* malloc(0) behavior */
  {"Wmemalign", 'A', 0},	/* memalign align is a power of 2 */
  {"verbose", 'v', 0},		/* verbose */
  {"inuse", 'u', 1},		/* inuse called at end */
  {"Wsignal", 300, 1},		/* emit a warning when sig is received */
  {"Wno-signal", 301, 1},	/* != */
  {"aged-queue", 302, 1},	/* size of the age queue */
  {"leak-size-threshold", 303, 1},	/* minimum size of a leak to be displayed */
  {"bytes-per-state", 304, 1},	/* bytes per state */
  {"no-signals", 305, 0},	/* Disable the signal manager. */
  {"Wsbrk", 306, 0},		/* Enable sbrk warning. */
  {"Wno-sbrk", 307, 0},		/* Disable sbrk warning. */
  {"Wfree-null", 308, 0},	/* Warn for free(0).  */
  {"trace", 't', 0},		/* Trace calls to malloc, free... */
  {"warn-once", 309, 0},	/* Warn once by changing the rights.  */
  {"annotate", 310, 0},		/* Annotate the output messages.  */
  {"weak-check-copy", 'w', 0},	/* Weak check for memcpy, bcopy, memmove.  */
  {"disp-args", 311, 0},	/* Display args.  */
  {(char *) 0, 0, 0}};

/* Errors: */
#define GETOPT_END	-1	/* No more options */
#define GETOPT_NOPTION	-2	/* Not an option (doesn't begin with a '-') */
#define GETOPT_LNEEDARG	-3	/* There is no argument */
#define GETOPT_SNEEDARG -4	/* There is no argument */
#define GETOPT_LARG	-5	/* There is an argument */
#define GETOPT_SARG	-6	/* There is an argument */
#define GETOPT_UNKNOWN  -7	/* Option unknown */

/* Where the argument is written.  Enough space must be allocated.  */
static char *optarg;

/* The option line. */
static char *optline;

/* The flag (used when an error is returned).  Be careful, if the option was
   a short one, there is no NUL.  */
static char *my_optopt;

/* Parse option: returns the short flag and set OPTARG if necessary.  */
static int
my_getopt (void)
{
  struct Opt *tmp;

  /* Skip the spaces */
  while (*optline && isspace (*optline))
    optline++;
  if (*optline == '\0')
    return GETOPT_END;
  my_optopt = optline;
  if (*optline != '-')
    {
      optline += parse_args (optline, optarg);
      return GETOPT_NOPTION;
    }
  /* Skip the dash */
  optline++;
  if (*optline == '-')
    {
      /* There are two dashes.  This is a long flag */
      optline++;
      for (tmp = options; tmp->long_flag != (char *) 0; tmp++)
	{
	  if (strncmp (optline, tmp->long_flag, strlen (tmp->long_flag)) == 0)
	    {
	      char *p;
	      p = optline + strlen (tmp->long_flag);
	      if (*p != '=' && !isspace (*p) && *p)
		continue;
	      else
		optline = p;
/*             my_optopt = tmp->long_flag; */
	      if (tmp->arg)
		{
		  /* This option require an argument. */
		  if (*optline == '=')
		    {
		      optline++;
		      optline += parse_args (optline, optarg);
		    }
		  else
		    return GETOPT_LNEEDARG;
		  return tmp->short_flag;
		}
	      else
		{
		  /* This option doesn't allow an argument.  */
		  if (*optline && !isspace (*optline))
		    {
		      if (*optline == '=')
			{
			  optline += parse_args (optline, optarg);
			  return GETOPT_LARG;
			}
		      else
			{
			  optline += parse_args (optline, optarg);
			  return GETOPT_UNKNOWN;
			}
		    }
		  else
		    {
		      *optarg = '\0';
		      return tmp->short_flag;
		    }
		}
	    }
	}
    }
  else
    {
      /* There is only one dash.  This is a short flag.  */
      for (tmp = options; tmp->long_flag != (char *) 0; tmp++)
	{
	  if (*optline == tmp->short_flag)
	    {
	      optline++;
/*             my_optopt = tmp->long_flag; */
	      if (tmp->arg)
		{
		  /* This option require an argument.  */
		  if (*optline == '=')
		    {
		      optline += parse_args (++optline, optarg);
		      return tmp->short_flag;
		    }
		  else
		    {
		      optline += parse_args (optline, optarg);
		      return GETOPT_SNEEDARG;
		    }
		}
	      else
		{
		  /* This option doesn't allow an argument.  */
		  if (*optline && !isspace (*optline))
		    {
		      optline += parse_args (optline, optarg);
		      return GETOPT_SARG;
		    }
		  return tmp->short_flag;
		}
	    }
	}
    }
  optline += parse_args (optline, optarg);
  return GETOPT_UNKNOWN;
}

/* Handle an option line.  */
static void
parse_opt_line (char *line)
{
  char *buf;
  int opt;

  optline = line;
  buf = alloca (strlen (line) + 1);

  do
    {
      optarg = buf;
      opt = my_getopt ();
      /* No more option ? */
      if (opt == GETOPT_END)
	break;
      if (opt < 0)
	{
	  strcpy (buf, my_optopt);
	  buf[strlen (buf) - strlen (optline)] = '\0';
	  chkr_report (M_I_BOC_SG_ET);
	  switch (opt)
	    {
	    case GETOPT_LNEEDARG:
	      chkr_printf (M_LOPTION_NEED_ARG, buf);
	      break;
	    case GETOPT_SNEEDARG:
	      chkr_printf (M_SOPTION_NEED_ARG, buf);
	      break;
	    case GETOPT_LARG:
	      chkr_printf (M_LOPTION_NALLOW_ARG, buf);
	      break;
	    case GETOPT_SARG:
	      chkr_printf (M_SOPTION_NALLOW_ARG, buf);
	      break;
	    case GETOPT_NOPTION:
	      chkr_printf (M_OPTION_ILLEGAL, buf);
	      break;
	    case GETOPT_UNKNOWN:
	      chkr_printf (M_OPTION_ILLEGAL, buf);
	      break;
	    }
	  continue;
	}
      switch (opt)
	{
	case 'A':		/* memalign behavior */
	  flag_warn_memalign = 1;
	  break;
	case 'D':		/* detector */
	  if (strcmp (optarg, "end") == 0)
	    do_detector_at_end = 1;
	  break;
	case 'N':		/* NULL zone size */
	  null_pointer_zone = atod (optarg);
	  break;
	case 'S':		/* stop */
	  flag_stop = 1;
	  break;
	case 'a':		/* abort */
	  print_message (abort_message);
	  chkr_abort ();
	  break;
	case 'd':		/* disable */
	  if (!parse_disable (optarg))	/* see l-malloc/maccess.c */
	    {
	      chkr_report (M_I_BOC_SG_ET);
	      chkr_printf (M_UNKNOWN_DISABLE, optarg);
	    }
	  break;
	case 'h':		/* help */
	  print_message (init_message);
	  print_message (help_message);
	  flag_silent |= 2;	/* Do not redisplay the initial message */
	  break;
#if 1				/* FIXME : we must check */
	case 'i':		/* image: set the image file */
	  chkr_prog_path = copy_of_exec_file (optarg);
	  break;
#endif
	case 'm':		/* malloc(0) behavior */
	  set_malloc0_behavior (optarg);
	  break;
	case 'n':		/* no symtab */
#ifdef CHKR_SAVESTACK
	  flag_no_symtab = 1;
#endif
	  break;
	case 'o':		/* output: set the output file */
	  {
	    int append = flag_append;	/* Use the default mode */

	    if (*optarg == '+')
	      {
		/* If the FILE begins with a '+', the file must be appended. */
		append = 1;
		optarg++;	/* Skip the '+' */
	      }
	    else if (*optarg == '-')
	      {
		/* If the FILE begins with a '-', the file must be overwritten. */
		append = 0;
		optarg++;
	      }

	    /* If FILE is only '+' or '-', set the append mode and that's all */
	    if (*optarg == '\0')
	      {
		flag_append = append;
		break;
	      }

	    sys_free (chkr_out_proto);
	    chkr_out_proto = sys_malloc (strlen (optarg) + 1);
	    strcpy (chkr_out_proto, optarg);

	    chkr_out_append = append;
	    update_output_file ();

	    break;
	  }
	case 'p':		/* profile */
#ifdef CHKR_PROFILE
	  profile_flag = 1;
#endif
	  break;
	case 'q':		/* quiet & silent */
	case 's':
	  flag_silent = 1;
	  flag_verbose = 0;
	  break;
	case 't':		/* --trace */
	  trace_malloc_flag = 1;
	  break;
	case 'u':		/* detector */
	  if (strcmp (optarg, "end") == 0)
	    do_inuse_at_end = 1;
	  break;
	case 'v':
	  flag_verbose++;
	  break;
	case 'w':		/* --weak-check-copy  */
	  flag_weak_check_copy = 1;
	  break;
	case 300:		/* sig-warn */
#ifndef NO_SIGNALS
	  parse_signal_warn (1, optarg);
#endif
	  break;
	case 301:		/* sig-no-warn */
#ifndef NO_SIGNALS
	  parse_signal_warn (0, optarg);
#endif
	  break;
	case 302:		/* aged-queue */
	  aged_queue_length = atod (optarg);
	  break;
	case 303:		/* leak-size-threshold */
	  leak_size_threshold = atod (optarg);
	  break;
	case 304:		/* bytes per state */
#ifndef bytes_per_state
	  bytes_per_state = atod (optarg);
#endif
	  break;
	case 305:		/* --no-signals */
	  flag_no_signals = 1;
	  break;
	case 306:		/* --Wsbrk */
	  flag_warn_sbrk++;
	  break;
	case 307:		/* --Wno-sbrk */
	  flag_warn_sbrk = 0;
	  break;
	case 308:		/* --Wfree-null */
	  flag_warn_free_null = 1;
	  break;
	case 309:		/* --warn-once */
	  flag_warn_once = 1;
	  break;
	case 310:		/* --annotate */
	  flag_annotate = 1;
	  break;
	case 311:		/* --disp-args */
	  flag_disp_args = 1;
	  break;
	default:
	  strcpy (buf, my_optopt);
	  buf[strlen (buf) - strlen (optline)] = '\0';
	  chkr_report (M_I_BOC_SG_ET);
	  chkr_printf (M_UNKNOWN_OP, my_optopt);
	}
    }
  while (1);
}

/* Parse a line of configuration. This is used while parsing ~/.checker.  */
static void
parse_line (char *line)
{
  static int state = 0;		/* 1: options must be parsed   2: options not used */

  /* skip comment lines */
  if (line[0] == '#')
    return;

  if (line[0] == '\t' && state != 1)
    return;			/* These options are not for us.  */

  if (line[0] == '\0' || line[0] == ' ')
    {
      state = 0;
      return;
    }

  if (line[0] != '\0' && line[0] != '\t' && line[0] != ' ' && state == 0)
    {
      /* This is a program list, separed by '|'.  */
      char *e;
      char *b;
      char c;
      b = line;
      while (*b)
	{
	  e = b;
	  while (*e && *e != '|')
	    e++;
	  c = *e;
	  *e = '\0';
	  if ((chkr_prog_path && strcmp (b, chkr_prog_path) == 0)
	      || strcmp (b, progname) == 0
	      || strcmp (b, full_progname) == 0
	      || strcmp (b, "default") == 0)
	    {
	      state = 1;
	      return;
	    }
	  *e = c;
	  b = e + (c ? 1 : 0);
	}
      return;
    }

  if (line[0] == '\t' && state == 1)
    {
      line++;
      if (strncmp (line, "suppress", 8))
	parse_opt_line (line);
#ifndef MDCHECKER
      else
	parse_suppress (line + 9);
#endif
    }
}

#undef LINE_MAX
#ifdef _POSIX2_LINE_MAX
#define LINE_MAX _POSIX2_LINE_MAX
#else
#define LINE_MAX 2048
#endif

/* Parse the config file.  */
static void
read_config_file (char *file)
{
  int fd;
  int n;
  int offset;
  char *rc;
  char *line;
  int len;
  char buffer[LINE_MAX + 1];

  fd = open (file, O_RDONLY);
  if (fd == -1)
    return;
  offset = 0;
  do
    {
      n = read (fd, buffer + offset, LINE_MAX - offset);
      line = buffer;
      n += offset;
      len = n;
      if (n == 0)
	break;
      while ((rc = memchr (line, '\n', len)) != (char *) 0)
	{
	  *rc = '\0';
	  parse_line (line);
	  len -= rc - line + 1;
	  line = rc + 1;
	}
      strncpy (buffer, line, len);
      offset = len;
      if (offset == n)
	offset = 0;		/* line too long */
    }
  while (n == LINE_MAX);

  /* There is perhaps no return at the last line.  */
  if (len != 0)
    {
      line[len] = '\0';
      parse_line (line);
    }

  close (fd);
}

static void
print_message (char **message)
{
  while (*message != (char *) 0 && **message != 0)
    {
      write (chkr_out, *message, strlen (*message));
      message++;
    }
}

/* Convert a string to an int.
   Format are:
   0nnnnn...    base 8,
   0xnnnn...    base 16,
   0bnnnn...    base 2,
   nnnnnn...    base 10
   Stop when a bad caracter is found.
 */
int
atod (const char *c)
{
  int val = 0;
  int base = 10;
  int digit;

  if (*c == '0')
    {
      if (c[1] == 'x' || c[1] == 'X')
	{
	  base = 16;
	  c++;
	}
      else if (c[1] == 'b' || c[1] == 'B')
	{
	  base = 2;
	  c++;
	}
      else
	base = 8;
      c++;
    }
  while (*c != '\0')
    {
      digit = chkr_ctod (*c);
      c++;
      if (digit == -1 || digit >= base)
	break;
      val *= base;
      val += digit;
    }
  return val;
}

/* Convert a char to an int.  Used by atod().  */
static int
chkr_ctod (const char c)
{
  if (c >= '0' && c <= '9')
    return c - '0';
  if (c >= 'a' && c <= 'f')
    return c - 'a' + 10;
  if (c >= 'A' && c <= 'F')
    return c - 'A' + 10;
  return -1;
}

/* This function is called just before _exit() and does some cleanup.  */
void
chkr_do_end (void)
{
  if (do_detector_at_end)
    __chkr_garbage_detector ();
  if (do_inuse_at_end)
    __chkr_disp_inuse ();
#ifdef CHKR_PROFILE
  if (profile_flag)
    {
      chkr_report (M_I_PRF_MA_ET);
#ifndef MDCHECKER
      display_profile ();
#endif
      chkr_printf (M_TOTAL_GARBAGE_TIME, tv_garbage.tv_sec, tv_garbage.tv_usec);
    }
#endif
}

/* Clean up the child after a fork.  */
void
chkr_clean_after_fork (void)
{
  make_pid ();
  update_output_file ();

  /* If the --stop option is set, stop now.  */
  if (flag_stop)
    {
      chkr_report (M_C_MES_CK_ET);
      chkr_printf (M_STOPPING_MYSELF, SIGCONT, my_pid);
      kill (my_pid, SIGSTOP);
    }
}

/*  Clean up the processus before exec.  */
void
chkr_clean_before_exec (void)
{
  chkr_do_end ();
}

/* Build the name of the output file with chkr_out_proto.
 * It parse %p and %n.
 * The result is allocated by sys_malloc and must be freed by sys_free.
 */
static char *
build_name_output_file (void)
{
  int i, j, len;
  char *res;

  len = strlen (chkr_out_proto);

  /* If the proto is for a file descriptor, return NULL.  */
  if (strncmp ("fd=", chkr_out_proto, 3) == 0
      && len == 4
      && chkr_out_proto[3] >= '0' && chkr_out_proto[3] <= '9')
    return NULL;

  res = sys_malloc (len + 1);

  /* Parse '%p' and '%n': replace %p by the pid and %n by the basename.
   *  Of course, '%%' is replaced by '%'.  */
  j = 0;
  for (i = 0; i <= len; i++)
    {
      if (chkr_out_proto[i] != '%')
	{
	  res[j++] = chkr_out_proto[i];
	  continue;
	}
      switch (chkr_out_proto[++i])
	{
	case '%':
	  res[j++] = '%';
	  break;
	case 'n':		/* the basename */
	  res = sys_realloc (res, len + strlen (progname) + 1);
	  strcpy (&res[j], progname);
	  j += strlen (progname);
	  break;
	case 'p':		/* the pid */
	  res = sys_realloc (res, len + 5 + 1);
	  strcpy (&res[j], my_pid_c);
	  j += 5;
	  break;
	default:
	  break;
	}
    }
  return res;
}

/* Open the output file.  */
static void
open_output_file (void)
{
  int fd;

  fd = open (chkr_out_file,
	     O_WRONLY | O_CREAT | (chkr_out_append ? O_APPEND : O_TRUNC),
	     S_IRUSR | S_IWUSR);
  if (fd == -1)
    fd = 2;
  dup2 (fd, chkr_out);
  if (fd != 2)
    close (fd);
}

/* Re-open the output file if the name has changed.  */
static void
update_output_file (void)
{
  char *new_file;

  new_file = build_name_output_file ();

  /* If the file is for a file descriptor ('-o=fd=x'), use this fd.  */
  if (!new_file)
    {
      chkr_out = chkr_out_proto[3] - '0';
      chkr_out_file = NULL;
      return;
    }

  /* If the filename didn't change, there is nothing to do.  */
  if (strcmp (new_file, chkr_out_file) == 0)
    return

      sys_free (chkr_out_file);
  chkr_out_file = new_file;
  open_output_file ();
}

#ifdef CHECK_OUTPUT_FILE
/* Save some infos about the output file.  */
static void
store_output_spec (void)
{
  struct stat outstat;

  if (fstat (chkr_out, &outstat) != 0)
    st_dev = st_ino = 0;
  else
    {
      st_dev = outstat.st_dev;
      st_ino = outstat.st_ino;
    }
}

/* Check if the output file has been handled by the user. If yes, reopen it.
 */
void
check_output_spec (void)
{
  struct stat outstat;

  if (st_dev == 0 && st_ino == 0)
    return;

  if ((fstat (chkr_out, &outstat) != 0
       || st_dev != outstat.st_dev || st_ino != outstat.st_ino)
      && chkr_out_file)
    {
      int fd;
      /* Always use O_APPEND, since the file is RE-open. */
      fd = open (chkr_out_file, O_APPEND | O_WRONLY);
      if (fd == -1)
	fd = 2;
      dup2 (fd, chkr_out);
      if (fd != 2)
	close (fd);
      store_output_spec ();
    }
  return;
}
#endif /* CHECK_OUTPUT_FILE */
