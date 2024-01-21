/*
 * man.c: The manual pager
 *
 * Copyright (c) 1990, 1991, John W. Eaton.
 * Copyright (C), 1994, 1995 Graeme W. Wilford. (Wilf.)
 *
 * You may distribute under the terms of the GNU General Public
 * License as specified in the file COPYING that comes with this
 * distribution.
 *
 * John W. Eaton
 * jwe@che.utexas.edu
 * Department of Chemical Engineering
 * The University of Texas at Austin
 * Austin, Texas  78712
 *
 * Mostly written/re-written by Wilf, some routines by Markus Armbruster.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <assert.h>
#include <errno.h>

#if defined(STDC_HEADERS)
#  include <string.h>
#  include <stdlib.h>
#elif defined(HAVE_STRING_H)
#  include <string.h>
#elif defined(HAVE_STRINGS_H)
#  include <strings.h>
#else /* no string(s) header */
extern char *strchr(), *strcat();
#endif /* STDC_HEADERS */

#if defined(HAVE_UNISTD_H)
#  include <unistd.h>
#else 
extern uid_t getuid(), geteuid();
extern pid_t vfork();
#  define R_OK		4
#  define STDOUT_FILENO	1
#  define STDIN_FILENO	0
#endif /* HAVE_UNISTD_H */

#if defined(HAVE_LIMITS_H)
#  include <limits.h>
#elif defined(HAVE_SYS_PARAM_H)
#  include <sys/param.h>
#endif

#ifdef __profile__
#  ifndef PATH_MAX
#    ifdef _POSIX_VERSION
#      define PATH_MAX _POSIX_PATH_MAX
#    else /* !_POSIX_VERSION */
#      ifdef MAXPATHLEN
#        define PATH_MAX MAXPATHLEN
#      else /* !MAXPATHLEN */
#        define PATH_MAX 1024
#      endif /* MAXPATHLEN */
#    endif /* _POSIX_VERSION */
#  endif /* !PATH_MAX */
#endif /* __profile__ */

#ifndef PIPE_BUF
#  if defined(_POSIX_VERSION) && defined(_POSIX_PIPE_MAX)
#    define PIPE_MAX _POSIX_PIPE_MAX
#  else /* !_POSIX_PIPE_MAX */
#    if defined(PIPE_MAX) && (PIPE_MAX != INT_MAX)
#      define PIPE_BUF PIPE_MAX
#    else /* !PIPE_MAX */
#      define PIPE_BUF 512
#    endif /* PIPE_MAX */
#  endif /* _POSIX_PIPE_MAX */
#endif /* PIPE_BUF */

#ifdef HAVE_SYS_FILE
#  include <sys/file.h>
#endif /* HAVE_SYS_FILE */

#if HAVE_FCNTL_H
#  include <fcntl.h>
#endif

#include <ctype.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>

#if HAVE_SYS_WAIT_H
#  include <sys/wait.h>
#endif

#ifndef STDC_HEADERS
extern char *getenv();
extern int errno;
#endif

#ifdef HAVE_GETOPT_H
#  include <getopt.h>
#else /* !HAVE_GETOPT_H */
#  include "lib/getopt.h"
#endif /* HAVE_GETOPT_H */

#define NLS_SET	manSet
#include "nls/nls.h"

#ifdef NLS
nl_catd catfd;
#endif /* NLS */

#include "manconfig.h"
#include "libdb/mydbm.h"
#include "libdb/db_storage.h"
#include "lib/error.h"
#include "lib/cleanup.h"
#include "hashtable.h"
#include "ult_src.h"
#include "manp.h"
#include "man.h"

/* the magic cookie to request preprocessing */
#define PP_COOKIE "'\\\" "

/* the default preprocessor sequence */
#ifndef DEFAULT_MANROFFSEQ
#  define DEFAULT_MANROFFSEQ ""
#endif

/* Some systems lack these */
#ifndef STDIN_FILENO
#  define STDIN_FILENO 0
#endif
#ifndef STDOUT_FILENO
#  define STDOUT_FILENO 1
#endif
#ifndef STDERR_FILENO
#  define STDERR_FILENO 2
#endif

/* external formatter programs, one for use without -t, and one with -t */
#define NFMT_PROG "./mandb_nfmt"
#define TFMT_PROG "./mandb_tfmt"
#undef ALT_EXT_FORMAT	/* allow external formatters located in cat hierarchy */

static int global_manpath = -1;		/* global or user manual page hierarchy? */

#if defined _AIX || defined __sgi
char **global_argv;
#endif

#if defined(__ultrix) || defined(__alpha)
#define BROKEN_PCLOSE
#endif

#ifdef MAN_CATS
#  ifdef BROKEN_PCLOSE

/* Some pclose(), notably Ultrix's get confused if we use more than one
   concurrently, blech.  Define our own popen()/pclose() combo. */

static pid_t *fd2pid = NULL;	/* map fd to pid, unused entries are zero */
static int max_fd = -1;		/* max fd seen, fd2pid has max_fd+1 elements */


FILE *
popen (const char *cmd, const char *type)
{
	pid_t child;
	int for_reading;
	int pipe_fd[2];
	int fd;
	FILE *stream;

	/* check type */
	if (type && (type[0] == 'r')) {
		for_reading = 1;
	} else if (type && (type[0] == 'w')) {
		for_reading = 0;
	} else {
		return NULL;
	}

	if (pipe (pipe_fd)) {
		return NULL;
	}

	child = vfork ();
	if (child == -1) {
		close (pipe_fd[0]);
		close (pipe_fd[1]);
		return NULL;
	} else if (child == 0) {
		/* if for_reading connect the writing end of pipe to stdout
		   else the reading end to stdin */
		if (dup2 (pipe_fd[for_reading], for_reading) == -1) {
			_exit (127);
		}

		/* close pipe fds */
		close (pipe_fd[0]);
		close (pipe_fd[1]);

		/* exec cmd in a shell */
		execl ("/bin/sh", "sh", "-c", cmd, NULL);
		/* if we hit this, execl() failed */
		_exit (127);
	}

	/* if for_reading make a stream from the reading end of pipe
	   else from the writing end */
	fd = pipe_fd[!for_reading];
	close (pipe_fd[for_reading]);
	stream = fdopen (fd, type);

	/* extend fd2pid up to index fd if necessary */
	if (fd > max_fd) {
		pid_t *new = malloc ((fd+1)*sizeof(pid_t));
		if (new) {
			/* copy old entries */
			memcpy (new, fd2pid, (max_fd+1)*sizeof(pid_t));
			/* zero new entries */
			memset (new+max_fd+1, 0, (fd-max_fd)*sizeof(pid_t));
			fd2pid = new;
			max_fd = fd;
		}
	}

	/* if we didn't get the stream or couldn't extend fd2pid, clean up & fail */
	if (!stream || (fd > max_fd)) {
		int res;
		int save = errno;

		kill (child, SIGKILL);

		if (stream) {
			fclose (stream);
		} else {
			close (fd);
		}

		do {		/* cope with non-restarting system calls */
			res = waitpid (child, NULL, 0);
		} while ((res == -1) && (errno == EINTR));

		errno = save;
		return NULL;
	}

	fd2pid[fd] = child;		/* save pid for plcose() */
	return stream;
}


int
pclose (FILE *stream)
{
	int fd = fileno (stream);
	pid_t child;
	int status;
	int res;
	int save;

	if ((fd > max_fd) || !fd2pid[fd]) {
		return -1;
	}
	child = fd2pid[fd];
	fd2pid[fd] = 0;

	if (fclose (stream)) {
		return -1;
	}

	save = errno;
	do {			/* cope with non-restarting system calls */
		res = waitpid (child, &status, 0);
	} while ((res == -1) && (errno == EINTR));
	if (res != -1) errno = save;

	return status;
}

#endif /* BROKEN_PCLOSE  */


static FILE *checked_popen (char *command, char *type)
{
	FILE *stream;

#ifdef SECURE_MAN_UID
	if (global_manpath)
		drop_effective_privs();
#endif /* SECURE_MAN_UID */

	if (debug) {
		fprintf(stderr, "popen (\"%s\", \"%s\")\n",
			command, type);
		if (strchr (type, 'r'))
			stream = popen (CAT " </dev/null", type);
		else
			stream = popen (CAT " >/dev/null", type);
	} else
		stream = popen (command, type);

#ifdef SECURE_MAN_UID
	if (global_manpath)
		regain_effective_privs();
#endif /* SECURE_MAN_UID */

	if (stream == NULL)
		error (0, 0,
		       CATGETS(man_POPEN, "can't popen"));

	return stream;
}
#endif /* MAN_CATS */

static __inline__ void gripe_system (char *command, int status)
{
	error (CHILD_FAIL, 0,
	       CATGETS(man_SYSTEM, "command exited with status %d: %s"),
	       status, command);
}


static int checked_system (char *command)
{
	int status;

	status = do_system_drop_privs (command);
	if (status != 0)
		gripe_system (command, status);

	return status;
}


extern uid_t ruid; 		/* defined in security.c */
extern uid_t euid;		/* 	"	"	 */ 
extern char *manpathlist[];	/* defined in manp.c     */

/* globals */
#ifndef debug
int debug;
#endif

int quiet = 1;
char *program_name;
char *database;
MYDBM_FILE dbf; 
char *extension; /* global for globbing.c */

/* locals */
static char *alt_system_name;
static char **section_list;		
static char *section;
static char *colon_sep_section_list;
static char *preprocessors;
static char *pager;
static char *locale;
static char *prompt_string;
static char *std_sections[] = STD_SECTIONS;
static char *manp;
static char *external;

static int troff;
static char *roff_device = NULL;
static int print_where;
static int catman;
static int local_man_file;
static int findall;
static int update;

#ifdef MAN_DB_UPDATES
static int update_required = 1;	/* we haven't performed update */
#endif /* MAN_DB_UPDATES */

static int ascii;		/* insert tr in the output pipe */
static int save_cat; 		/* security breach? Can we save the cat? */

static int found_a_stray;		/* found a straycat */

#ifdef MAN_CATS
static char *tmp_cat_file;	/* for open_cat_stream(), close_cat_stream() */
static int cat_comp_pid;			/* dto. */
#endif

static const struct option long_options[] =
{
    {"local-file", no_argument, 	0, 'l'},
    {"manpath", required_argument, 	0, 'M'},
    {"pager", required_argument, 	0, 'P'},
    {"sections", required_argument, 	0, 'S'},
    {"all", no_argument, 		0, 'a'},
    {"debug", no_argument, 		0, 'd'},
    {"whatis", no_argument, 		0, 'f'},
    {"help", no_argument, 		0, 'h'},
    {"apropos", no_argument, 		0, 'k'},
    {"version", no_argument, 		0, 'V'},
    {"systems", required_argument, 	0, 'm'},
    {"preprocessor", required_argument,	0, 'p'},
    {"location", no_argument, 		0, 'w'},
    {"where", no_argument,		0, 'w'},
    {"locale", required_argument,	0, 'L'},
    {"extension", required_argument,	0, 'e'},
    {"update", no_argument, 		0, 'u'},
    {"prompt", required_argument,	0, 'r'},
    {"default", no_argument,		0, 'D'},
    {"ascii", no_argument,		0, '7'},
    {"catman", no_argument, 		0, 'c'},

#ifdef HAS_TROFF

    {"troff", no_argument, 		0, 't'},
    {"troff-device", optional_argument,	0, 'T'},
#  ifdef TROFF_IS_GROFF
    {"ditroff", no_argument, 		0, 'Z'},
#  endif
    {0, 0, 0, 0}
};

static const char args[] = "7DlM:P:S:adfhkVum:p:tT:we:L:Zcr:";

#  ifdef TROFF_IS_GROFF
static int ditroff;
#  endif /* TROFF_IS_GROFF */

#else /* !HAS_TROFF */

    {0, 0, 0, 0}
};

static const char args[] = "7DlM:P:S:adfhkVum:p:we:L:cr:";

#endif /* HAS_TROFF */

/* issue a usage message, then exit with the given status */
static void usage (int status)
{
#ifdef HAS_TROFF
#  ifdef TROFF_IS_GROFF
	char formatter[]="groff";
#  else
	char formatter[]="troff";
#  endif /* TROFF_IS_GROFF */
#endif /* HAS_TROFF */
	
#ifdef HAS_TROFF
	printf(CATGETS(man_USAGE_TROFF, 
	       "usage: %s [-c|-f|-k|-w|-tZT device] [-adlhu7V] [-Mpath] [-Ppager] [-Slist]\n"
	       "           [-msystem] [-pstring] [-Llocale] [-eextension] [section] page ...\n"),
               program_name);
#else
	printf(CATGETS(man_USAGE_NOTROFF, 
	       "usage: %s [-c|-f|-k|-w] [-adlhu7V] [-Mpath] [-Ppager] [-Slist] [-msystem]\n"
               "           [-pstring] [-Llocale] [-eextension] [section] page ...\n"),
               program_name);
#endif

	puts(CATGETS(man_OPT1, 
	       "-a, --all                   find all matching manual pages.\n"
               "-d, --debug                 emit debugging messages.\n"
               "-e, --extension             limit search to extension type `extension'.\n"
               "-f, --whatis                equivalent to whatis.\n"
               "-k, --apropos               equivalent to apropos.\n"
               "-w, --where, --location     print physical location of man page(s).\n"
               "-l, --local-file            interpret `page' argument(s) as local filename(s).\n"
               "-u, --update                force a cache consistency check.\n"
               "-r, --prompt string         provide the `less' pager with a prompt\n"
               "-c, --catman                used by catman to reformat out of date cat pages.\n"
               "-7, --ascii                 display ASCII translation of certain latin1 chars."));
#ifdef HAS_TROFF
	printf(CATGETS(man_OPT2,
	       "-t, --troff                 use %s to format pages.\n"
	       "-T, --troff-device device   use %s with selected device.\n"),
	       formatter, formatter);
#  ifdef TROFF_IS_GROFF
	puts(CATGETS(man_OPTDIT,
	       "-Z, --ditroff               use groff and force it to produce ditroff."));
#  endif /* TROFF_IS_GROFF */
#endif /* HAS_TROFF */

	puts(CATGETS(man_OPT3,
	       "-D, --default               reset all options to their default values.\n"
               "-M, --manpath path          set search path for manual pages to `path'.\n"
               "-P, --pager pager           use program `pager' to display output.\n"
               "-S, --sections list         use colon separated section list.\n"
	       "-m, --systems system        search for man pages from other unix system(s).\n"
	       "-L, --locale locale         define the locale for this particular man search.\n"
	       "-p, --preprocessor string   string indicates which preprocessors to run."));
	puts(  "                             e - [n]eqn   p - pic    t - tbl\n"
               "                             g - grap     r - refer  v - vgrind");
        puts(CATGETS(man_OPT4,
               "-V, --version               show version.\n"
               "-h, --help                  show this usage message."));

               exit (status);
}

static void gripe_no_name (char *section)
{
	if (section)
		printf (CATGETS(man_NO_NAME_S, 
			"What manual page do you want from section %s?\n"), section);
	else
		puts (CATGETS(man_NO_NAME, "What manual page do you want?"));

	exit (FAIL);
}

static __inline__ void gripe_no_man(char *name, char *sec)
{
	if (troff)
		printf (CATGETS(man_NO_SRC_MAN, "No source manual entry for %s"), 
			name);
	else

/* On AIX and IRIX, fall back to the vendor supplied browser */
#if defined _AIX || defined __sgi

		putenv("MANPATH=");  /* reset the MANPATH variable */
		execv("/usr/bin/man", global_argv);
#else
		printf (CATGETS(man_NO_MAN, "No manual entry for %s"), name);
#endif
			 
	if (sec)
		printf (CATGETS(man_SEC, " in section %s\n"), sec);
	else
		putchar ('\n');
}

/* fire up the appropriate external program */
static void do_extern (char *argv[])
{
	/* privs are already dropped */
	argv[0] = basename(external);
	execv(external, argv);
	exit(FATAL);
}

/* lookup $MANOPT and if available, put in *argv[] format for getopt() */
static __inline__ char **manopt_to_env(int *argc)
{
	char *manopt, *opt_start, **argv;
	
	opt_start = manopt = getenv("MANOPT");
	if (manopt == NULL || *manopt == '\0')
		return NULL;

	manopt = xstrdup(manopt);

	/* allocate space for the program name */
	*argc = 0;
	argv = (char **) xmalloc ((*argc + 3) * sizeof (char *));
	argv[(*argc)++] = program_name;
	
	/* for each [ \t]+ delimited string, allocate an array space and fill
	   it in. An escaped space is treated specially */	
	while (*manopt) {
		switch (*manopt) {
			case ' ':
			case '\t':
				if (manopt != opt_start) {
					*manopt = '\0';
					argv = (char **) 
					       xrealloc (argv, (*argc + 3) * 
						         sizeof (char *));
					argv[(*argc)++] = opt_start;
				}
				while (isspace(*(manopt + 1)))
					*++manopt = '\0';
				opt_start = manopt + 1;
				break;
			case '\\':
				if (*(manopt + 1) == ' ')
					manopt++;
				break;
			default:
				break;
		}
		manopt++;
	}

	if (*opt_start)
		argv[(*argc)++] = opt_start;
	argv[*argc] = NULL;			
		
	return argv;
}

/* return char array with 'less' special chars escaped */
static __inline__ char *escape_special(char *string)
{
	static char *escaped_string; 
	char *ptr;

	/* 2*strlen will always be long enough to hold the escaped string */
	ptr = escaped_string = (char *) xrealloc ( escaped_string, 
						  2 * strlen(string) + 1);
	
	while (*string) {
		if (*string == '?' || 
		    *string == ':' || 
		    *string == '.' ||
		    *string == '%' || 
		    *string == '\\')
				    	*ptr++ = '\\';
		    	
		*ptr++ = *string++;
	}

	*ptr = *string;
	return escaped_string;
}
			
#ifdef MAN_DB_UPDATES
/* test for new files. If any found return 1, else 0 */
static int need_to_rerun(void)
{						
	int rerun = 0;
	char **mp;
	
	for (mp = manpathlist; *mp; mp++) {
		char *catpath;

		if ( (catpath = global_catpath(*mp)) ) {
			database = mkdbname(catpath);
			free(catpath);
			global_manpath = 1;
		} else {
			database = mkdbname(*mp);
			global_manpath = 0;
			drop_effective_privs();
		}

		if (update)
			reset_db_time();

		/* if no change or couldn't update (EOF), forget it */
		if (update_db(*mp) > 0)
			rerun = 1;

		if (!global_manpath)
			regain_effective_privs();

	}

	/* only ever need to do this once */
	update_required = 0;
	
	return rerun;
}
#endif /* MAN_DB_UPDATES */

int main (int argc, char *argv[])
{
	int argc_env, status = 0, exit_status = OK;
	char **argv_env, *tmp;
	char *internal_locale;
	char *nextarg;
	extern int optind;


#ifdef __profile__
	char wd[PATH_MAX];
	char *cwd = wd;
#endif /* __profile__ */

                
	/* initialise the locale */
#ifdef HAVE_SETLOCALE
	internal_locale = NLS_INIT;
#endif /* HAVE_SETLOCALE */

/* export argv, it might be needed when invoking the vendor supplied browser */
#if defined _AIX || defined __sgi
	global_argv = argv;
#endif

	program_name = xstrdup(basename((argv[0])));

/* if the user happens to be root and man is installed setuid, forget all
 * of the priv dropping etc. Just make sure that any cat files produced
 * are owned by MAN_OWNER by doing explicit chown() calls */

#ifdef SECURE_MAN_UID
	/* record who we actually are */
	ruid = getuid ();
	euid = geteuid ();

	if (ruid == 0)
		setuid(ruid);

	/* so that manpath code respects user privs */
	drop_effective_privs();
#endif

#ifdef __profile__
	/* This will enable us to do some profiling and know
	where gmon.out will end up. Must chdir(cwd) before we return */
#  ifdef HAVE_GETCWD
	if (!getcwd(cwd, PATH_MAX - 1))
#  else /* not HAVE_GETCWD */
	if (!getwd(cwd))
#  endif
		cwd[0] = '\0';
#endif /* __profile__ */

	/* First of all, find out if $MANOPT is set. If so, put it in 
	   *argv[] format for getopt to play with. */
	if ( (argv_env = manopt_to_env (&argc_env)) ) {
		man_getopt (argc_env, argv_env);
		optind = 0;
	}

	/* give the actual program args to getopt */
	man_getopt (argc, argv);

	/* if the user wants whatis or apropos, give it to them... */
	if (external)
		do_extern (argv);

#ifdef HAVE_SETLOCALE
	/* close this locale and reinitialise incase a new locale was 
	   issued as an argument or in $MANOPT */
	if (locale) {
		internal_locale = setlocale(LC_MESSAGES, locale);
#  ifdef NLS
		NLS_CLOSE;
		catfd = catopen("man_db", MCLoadBySet);
#  endif /* NLS */
	}
#endif /* HAVE_SETLOCALE */
	
	if (pager == NULL)
		if ((pager = getenv ("PAGER")) == NULL)
			pager = PAGER;
	if (*pager == '\0')
		pager = CAT;

	if (prompt_string == NULL)
#ifdef LESS_PROMPT
		prompt_string = LESS_PROMPT;
#else
		prompt_string = CATGETS(man_LESS_PROMPT, 
					" Manual page $MAN_PN "
					"?ltline %lt?L/%L.:byte %bB?s/%s..?e (END):"
					"?pB %pB\\\\%..");
#endif

	if (debug)
		fprintf (stderr, "\nusing %s as pager\n", pager);

	if (optind == argc)
		gripe_no_name (NULL);

	putenv("LESSCHARSET=latin1");

	/* man issued with `-l' option */
	if (local_man_file) {
		for (;  optind < argc;  ++optind) {
			if (strcmp (argv[optind], "-") == 0)
				display (NULL, "", NULL, "(stdin)");
			else {
				/* See if we need to decompress the file(s)
				   first */
#ifdef COMP_SRC
				struct compression *comp;

				if ( (comp = comp_info(argv[optind])) )
					(void) decompress(argv[optind], comp);
#endif /* COMP_SRC */
				if (!display (NULL, argv[optind], NULL, 
					     basename(argv[optind]))) {
					error (0, errno, argv[optind]);
					exit_status = NOT_FOUND;
				}

#ifdef COMP_SRC
				remove_ztemp();
#endif /* COMP_SRC */
			}
		}
		exit (exit_status);
	}

	if (manp == NULL)
		manp = add_nls_manpath(manpath (alt_system_name), 
				       internal_locale);
	else
		free(manpath(NULL));

	create_pathlist(xstrdup(manp), manpathlist);

	if (debug)
		fprintf(stderr, "*manpath search path* = %s\n", manp);

	/* finished manpath processing, regain privs */
	regain_effective_privs();

	section_list = get_section_list ();

	if (optind == argc - 1) {
		tmp = is_section ( argv[optind] );

		if (tmp)
			gripe_no_name (tmp);
	}

#ifdef MAN_DB_UPDATES
	/* If `-a' or consistency check forced `-u', do it now. */
	if (update || findall)
		(void) need_to_rerun();
#endif /* MAN_DB_UPDATES */

	while (optind < argc) {
		nextarg = argv[optind++];

		/*
     		 * See if this argument is a valid section name.  If not,
      		 * is_section returns NULL.
      		 */

		tmp = is_section (nextarg);

		if (tmp) {
			section = tmp;

			if (debug)
				fprintf (stderr, "\nsection: %s\n", section);

			continue;
		}

		/* this is where we actually start looking for the man page */

		status = man (nextarg);

		/* clean out the memory cache for each man page */
		free_hashtab();

		if (!status && !catman) {
			gripe_no_man(nextarg, section);
			exit_status = NOT_FOUND;
		} else {
			if (debug)
				fprintf(stderr,
					"\nFound %d man pages\n", status);
			if (catman) {
				printf("%s", nextarg);
				if (section)
					printf("(%s)", section);
				if (optind != argc)
					fputs(", ", stdout);
				else
					fputs(".\n", stdout);
			} 
					
		}

		chkr_garbage_detector();
	}

#ifdef __profile__
	/* For profiling */
	if (cwd[0])
		chdir(cwd);
#endif /* __profile__ */

	exit (exit_status);
}

/* parse the arguments contained in *argv[] and set appropriate vars */
static void man_getopt (int argc, char *argv[])
{
	int c, option_index; /* not used, but required by getopt_long() */
	static int apropos, whatis; /* retain values between calls */

	while ((c = getopt_long (argc, argv, args,
				 long_options, &option_index)) != EOF) {

		switch (c) {

			case 'l':
				local_man_file = 1;
				break;
			case 'e':
				extension = optarg;
				break;
			case 'M':
				manp = optarg;
				break;
		    	case 'P':
				pager = optarg;
				break;
		    	case 'S':
				colon_sep_section_list = optarg;
				break;
			case 'V':
				ver();
				break;
		    	case 'a':
				findall = 1; 
				break;
			case 'u':
				update = 1;
				break;
			case 'c':
				catman = 1;
				break;
		    	case 'd':
#ifndef debug
				debug = 1;
#endif
				break;
		    	case 'f':
				external = WHATIS;
				apropos = 1;
				break;
		    	case 'k':
				external = APROPOS;
				whatis = 1;
				break;
		    	case 'm':
				alt_system_name = optarg;
				break;
			case 'L':
				locale = optarg;
				break;
		    	case 'p':
				preprocessors = optarg;
				break;
			case '7':
				ascii = 1;
				break;
#ifdef HAS_TROFF
		    	case 't':
				troff = 1;
				break;

			case 'T':
				/* @@@ traditional nroff knows -T,
                                   troff does not (gets ignored).
                                   All incarnations of groff know it.
                                   Why does -T imply -t? */
				/* as this is an optional argument */
				if (optarg)
					roff_device = optarg;
				troff = 1;
				break;
			case 'Z':
#ifdef TROFF_IS_GROFF
				ditroff = 1;
				troff = 1;
#endif/* TROFF_IS_GROFF */
				break;
#endif /* HAS_TROFF */
		    	case 'w':
				print_where = 1;
				break;
			case 'r':
				prompt_string = optarg;
				break;
			case 'D':
		    		/* discard all preset options */
		    		local_man_file = findall = update = catman = debug
		    			 = troff
		    			 = print_where = ascii = 0;
#ifdef TROFF_IS_GROFF
				ditroff = 0;
#endif
		    		roff_device = extension = pager = locale
		    			     = colon_sep_section_list
		    			     = alt_system_name = external
		    			     = preprocessors = manp = NULL;
		    		break;
		    	case 'h':
		    		usage(OK);
		    		break;
		    	default:
				usage(FAIL);
				break;
		}
	}

	/* check for incompatible options */
	if (troff + whatis + apropos + catman + print_where > 1) {
		error (0, 0,
		       strappend(NULL,
		       		 troff ? "-[tTZ] " : "",
		       		 whatis ? "-f " : "",
		       		 apropos ? "-k " : "",
		       		 catman ? "-c " : "",
		       		 print_where ? "-w " : "", 
		       		 CATGETS(man_INCOMP_OPT,
		       		          ": incompatible options"), NULL));
		usage(FAIL);
	}
}

/*
 * Check to see if the argument is a valid section number.  If the
 * first character of name is a numeral, or the name matches one of
 * the sections listed in section_list, we'll assume that it's a section.
 * The list of sections in config.h simply allows us to specify oddly
 * named directories like .../man3f.  Yuk.
 */
static __inline__ char *is_section (char *name)
{

	if ( !(isdigit (name[0]) && !isdigit (name[1])) ) {
		char **vs;
	
		for (vs = section_list; *vs; vs++)
			if ((strcmp (*vs, name) == 0)) 
				return name;
		return NULL;
	}
	return name;
}

/* Snarf pre-processors from file, return (static) string or NULL on failure */
static char *get_preprocessors_from_file (char *file)
{
	char *directive = NULL;
#ifdef PP_COOKIE
	FILE *fp;
	static char line[128];

	if (*file == '\0')
		return NULL;

	if ((fp = fopen (file, "r"))) {
		if (fgets (line, sizeof (line), fp)) {
			if (!memcmp (line, PP_COOKIE, 4)) {
				int len;
				directive = line+4;
				/* strip trailing newline */
				len = strlen (directive);
				if (len && (directive[len-1] = '\n'))
					directive[len-1] = 0;
			}
		}
		fclose (fp);
	}

	/* if we couldn't read the first line from file, or we didn't
           find PP_COOKIE, then directive == NULL */
#endif
	return directive;
}


/* Determine pre-processors, set save_cat and return
   (static) string */
static char *get_preprocessors (char *file)
{
	char *pp_string;
	char *pp_source;

	/* try in order: command line, file, environment, default */
	if ((pp_string = preprocessors)) {
		pp_source = "command line";
		save_cat = 0;
	} else if ((pp_string = get_preprocessors_from_file (file))) {
		pp_source = "file";
		save_cat = 1;
	} else if ((pp_string = getenv ("MANROFFSEQ"))) {
		pp_source = "environment";
		save_cat = 0;
	} else {
		pp_string = DEFAULT_MANROFFSEQ;
		pp_source = "default";
		save_cat = 1;
	}

	if (debug)
		fprintf (stderr, "pre-processors `%s' from %s\n",
			 pp_string, pp_source);
	return pp_string;
}


/* Return command (malloced string) to format file to stdout */
static __inline__ char *make_roff_command (char *dir, char *file)
{
	char *pp_string = get_preprocessors (file);
	char *fmt_prog;
	char *command;

#ifdef ALT_EXT_FORMAT
	/* Check both external formatter locations */
	if (global_manpath && dir) {
		char *catpath = global_catpath (dir);

		/* If we have an alternate catpath */
		if (strcmp(catpath, dir) != 0) {
			fmt_prog = strappend(catpath, "/",
				    	     troff ? TFMT_PROG : NFMT_PROG, 
				    	     NULL);
			if (access (fmt_prog, X_OK)) {
				free (fmt_prog);
				fmt_prog = xstrdup(troff ? TFMT_PROG : NFMT_PROG);
				if (access (fmt_prog, X_OK)) {
					free (fmt_prog);
					fmt_prog = NULL;
				}
			}
		/* If we don't */
		} else {
			free (catpath);
#endif /* ALT_EXT_FORMAT */

			fmt_prog = xstrdup(troff ? TFMT_PROG : NFMT_PROG);
			if (access (fmt_prog, X_OK)) {
				free (fmt_prog);
				fmt_prog = NULL;
			}
			
#ifdef ALT_EXT_FORMAT
		}
	} else
		fmt_prog = NULL;
#endif /* ALT_EXT_FORMAT */
	
	if (debug && fmt_prog)
		fprintf(stderr, "External formatter %s\n", fmt_prog);
				
	if (!fmt_prog) {
		/* we don't have an external formatter script */
		char *dev;	/* either " -T<mumble>" or "" */
		int using_tbl = 0;

		if (roff_device)
			dev = strappend (NULL, " -T", roff_device, NULL);
		else
			dev = "";

		if (*file)
			command = strappend (NULL, SOELIM " '", 
					     file, "'", NULL); 
		else
			command = xstrdup (SOELIM);

		do {
			char *filter;
			int wants_dev = 0; /* filter wants a dev argument */

			/* set filter according to *pp_string, on
                           errors set filter to "" */
			switch (*pp_string) {
			case 'e':
				filter = troff ? EQN : NEQN;
				wants_dev = 1;
				break;
			case 'g':
				filter = GRAP;
				break;
			case 'p':
				filter = PIC;
				break;
			case 't':
				filter = TBL;
				using_tbl = 1;
				break;
			case 'v':
				filter = VGRIND;
				break;
			case 'r':
				filter = REFER;
				break;
			case 0:
				/* done with preprocessors, now add roff */
#ifdef TROFF_IS_GROFF
				if (troff && ditroff) filter = TROFF " -Z";
				else
#endif
                                if (troff) {
#ifdef HAS_TROFF
                                  filter = TROFF;
#else
                                  assert (0);
#endif
                                } else {
#ifdef NROFF_MISSING
                                  assert (0);
#else
                                  filter = NROFF;
#endif
                                }

				wants_dev = 1;
				break;
			default:
				filter = "";
			}

			if (*filter) {
				command = strappend (command,
						     " | ",
						     filter,
						     wants_dev ? dev : "",
						     NULL);
			} else {
				assert (*pp_string); /* didn't fail on roff */
				error (0, 0,
				       CATGETS(man_BAD_PP,
					       "ignoring unknown preprocessor `%c'"),
				       *pp_string);
			      }
		} while (*pp_string++);

#ifndef GNU_NROFF
		/* tbl needs col */
		if (using_tbl && !troff && *COL)
			command = strappend (command, " | ", COL, NULL);
#endif /* GNU_NROFF */

		if (roff_device) free (dev);
	} else {
		/* use external formatter script, it takes arguments
		   input file, preprocessor string, and (optional)
		   output device */
		command = strappend (fmt_prog,
				     " '", file, "' \"", pp_string, "\" ",
				     roff_device ? roff_device : "", NULL);
	}

	return command;
}

/* Return command (malloced string) to display file, NULL means stdin */
static char *make_display_command (char *file, char *title)
{
	char *command;

	command = strappend (NULL, "{ export MAN_PN LESS; MAN_PN=\'",
			     escape_special(title),
			     "\'; LESS=\"" LESS_OPTS, prompt_string,
			     "\"; ", NULL);
	if (file) {
	  if (ascii) {
	    command = strappend (command, CAT " '", file,
	      			   "' |" TR TR_SET1 TR_SET2 " |",
	      			   pager, "; }", NULL);
	  } else {
	    command = strappend (command, pager, " '", file, 
				   "'; }", NULL);
	  }
	} else {
	  if (ascii) {
	    command = strappend (command, TR TR_SET1 TR_SET2 " |",
	      			   pager, "; }", NULL);
	  } else {
	    command = strappend (command, pager, "; }", NULL);
	  }
	}

	return command;
}


#ifdef SECURE_MAN_UID
/* Required if man is setuid and user has ruid==0 */
static __inline__ int do_chown(const char *file)
{
	int status;
	
	if ((status = chown (file, euid, (uid_t) -1)) )
		error (0, errno, CATGETS (man_CHOWN, "can't chown %s"), file);
	
	return status;
}
#endif /* SECURE_MAN_UID */


/* return a (malloced) temporary name in cat_file's directory */
static char *tmp_cat_filename (char *cat_file)
{
	char *name = xmalloc (strlen (cat_file) + 12);
	char *base;
	strcpy (name, cat_file);
	base = basename(name);
	sprintf (base, "%d", getpid());
	return name;
}


/* If delete unlink tmp_cat, else commit tmp_cat to cat_file.
   Return non-zero on error.
 */
static int commit_tmp_cat (char *cat_file, char *tmp_cat, int delete)
{
	int status;

	if (!delete) {
		if (debug) {
			fprintf (stderr, "fixing temporary cat's mode\n");
			status = 0;
		} else {
			status = chmod (tmp_cat, CATMODE);
			if (status)
				error(0, errno,
				      CATGETS(man_CHMOD, "can't chmod %s"),
				      tmp_cat);
		}
	} else
		status = 0;

#if defined(SECURE_MAN_UID) && defined(MAN_CATS)
	/* @@@ if root has a private man hierarchy (strange idea), the
           generated cats don't belong to him */
	if (!delete && !status && (ruid == 0)) {
		if (debug) {
			fprintf (stderr, "chowning temporary cat\n");
			status = 0;
		} else {
			status = do_chown(tmp_cat);
			if (status)
				error(0, errno,
				      CATGETS(man_CHOWN, "can't chown %s"),
				      tmp_cat);
		}
	}
#endif /* SECURE_MAN_UID && MAN_CATS */

	if (!delete && !status) {
		if (debug) {
			fprintf (stderr, "renaming temporary cat to %s\n",
				 cat_file);
			status = 0;
		} else {
			if ((status = rename (tmp_cat, cat_file)))
				error (0, errno,
				       CATGETS (man_RENAME,
						"can't rename %s to %s"),
				       tmp_cat, cat_file);
		}
	}

	if (delete || status) {
		if (debug)
			fprintf (stderr, "unlinking temporary cat\n");
		else
			if (unlink (tmp_cat))
				error (0, errno,
				       CATGETS (man_UNLINK,
						"can't unlink %s"),
				       tmp_cat);
	}

	return status;
}

#ifdef MAN_CATS

/* Return stream to write formatted manual page to for saving as cat file */
static __inline__ FILE *open_cat_stream (char *cat_file)
{
	FILE *save;
#  ifdef COMP_CAT
	pid_t child;
	int pipe_fd[2];
#  endif

	tmp_cat_file = tmp_cat_filename (cat_file);
	if (!debug) {
		push_cleanup ((void (*)())unlink, tmp_cat_file);
	}

#  ifdef COMP_CAT
	/* write to a pipe that compresses into tmp_cat_file */

	/* create pipe */
	if (pipe (pipe_fd)) {
		error (0, errno, CATGETS (man_PIPE, "can't create pipe"));
		return NULL;
	}

	/* fork the compressor */
	fflush (NULL);
	child = fork ();
	if (child < 0) {
		error (0, errno, CATGETS (man_FORK, "can't fork"));
		return NULL;
	} else if (child == 0) {
		/* compress reading end of pipe to tmp_cat_file */
		char *const envp[] = { NULL };
		int cat_fd;

		pop_all_cleanups ();

		if (debug) {
			fprintf (stderr, "compressing to temporary cat %s\n",
				 tmp_cat_file);
			tmp_cat_file = "/dev/null";
		}

		/* connect standard input to reading end of pipe */
		if (dup2 (pipe_fd[0], 0) == -1)
			error (0, errno, CATGETS (man_DUP2, "can't dup2"));
		close (pipe_fd[0]);
		close (pipe_fd[1]);

		/* set up standard output to write to tmp_cat_file */
		cat_fd = open (tmp_cat_file, O_WRONLY|O_CREAT|O_TRUNC, CATMODE);
		if (cat_fd == -1) {
			error (FATAL, errno,
			       CATGETS (man_CREATE_CAT, "can't create %s"),
			       tmp_cat_file);
		}
		if (dup2 (cat_fd, 1) == -1)
			error (0, errno, CATGETS (man_DUP2, "can't dup2"));
		close (cat_fd);

		nice (10);

		/* compress standard input to standard output */
		/* @@@ disgusting: have to split COMPRESSOR into argv */
		{
			char *cmd;
			char *path;
			char *argv[8];
			int n;
			cmd = xstrdup (COMPRESSOR);
			path = strtok (cmd, " \t\n");
			argv[0] = basename (path);
			for (n = 1;  argv[n-1];  ++n) {
				argv[n] = strtok (NULL, " \t\n");
			}
			execve (path, argv, envp);
			error (FATAL, errno,
			       CATGETS (man_EXEC, "can't exec %s"),
			       path);
		}
	}

	/* connect save to the writing end of pipe */
	close (pipe_fd[0]);
	save = fdopen (pipe_fd[1], "w");

	cat_comp_pid = child;
#  else
	/* write directly to tmp_cat_file */
	if (debug) {
		fprintf (stderr, "saving to temporary cat %s\n", tmp_cat_file);
		tmp_cat_file = "/dev/null";
	} else {
		push_cleanup ((void (*)())unlink, tmp_cat_file);
	}
	save = fopen (tmp_cat_file, "w");
#  endif

	return save;
}

/* Close the cat page stream, return non-zero on error.
   If delete don't update the cat file.
 */
static __inline__ int close_cat_stream (FILE *cat_stream, char *cat_file, int delete)
{
	int status;

	status = fclose (cat_stream);
#  ifdef COMP_CAT
	{			/* get compressor's exit status */
		int comp_status;

		while (waitpid (cat_comp_pid, &comp_status, 0) == -1) {
			if (errno != EINTR) {
				error (0, errno,
				       CATGETS (man_WAITPID, "waiting for pid %u"),
				       cat_comp_pid);
				comp_status = -1;
				break;
			}
		}

		if (debug)
			fprintf (stderr, "compressor exited with status %d\n",
				 comp_status);
		status |= comp_status;
	}
#  endif
	status |= commit_tmp_cat (cat_file, tmp_cat_file, delete || status);
	if (!debug) pop_cleanup ();
	free (tmp_cat_file);
	return status;
}

/*
 * format a manual page with roff_cmd, display it with disp_cmd, and
 * save it to cat_file
 */
static int format_display_and_save (char *roff_cmd, char *disp_cmd, char *cat_file)
{
	FILE *in  = checked_popen (roff_cmd, "r");
	FILE *out = checked_popen (disp_cmd, "w");
	FILE *sav = open_cat_stream (cat_file);
	int instat = 1, outstat;
	RETSIGTYPE (*old_handler)() = signal (SIGPIPE, SIG_IGN);

	if (in && out) {
		/* copy in to both out and sav */
		char buf[PIPE_BUF];
		int inned;	/* #bytes in buf */
		int outing = 1;
		int saving = sav != NULL;

		while ((inned = fread (buf, 1, PIPE_BUF, in))) {
			int outed = 0; /* #bytes already written to out */
			int saved = 0; /* dto. to sav  */

			/* write buf to out and sav, cope with short writes */
			do {
				int n;

				if (outing && (outed < inned)) {
					n = fwrite (buf+outed, 1, inned-outed, 
						    out);
					outed += n;
					if (!n && saving)
						fprintf (stderr,
							 CATGETS(man_STILL_SAVING, 
							"Still saving the page, please wait...\n"));
					outing = n; /* stop outing on error */
				}

				if (saving && (saved < inned)) {
					n = fwrite (buf+saved, 1, inned-saved, 
						    sav);
					saved += n;
					saving = n; /* stop saving on error */
			        }

			} while (   (outing && (outed < inned))
				 || (saving && (saved < inned)) );
		}
	}

	if (in)  instat  = pclose (in);
	if (sav) close_cat_stream (sav, cat_file, instat);
	if (out) outstat = pclose (out);
	signal (SIGPIPE, old_handler);
	return instat;
}
#endif /* MAN_CATS */

/*
 * optionally chdir to dir, if necessary update cat_file from man_file
 * and display it.  if man_file is NULL cat_file is a stray cat.  If
 * !save_cat or cat_file is NULL we must not save the formatted cat.
 */
static int display (char *dir, char *man_file, char *cat_file, char *title)
{
	int found;
	static int pause;
	char *roff_cmd;		/* command to format man_file to stdout */

	/* if dir is set chdir to it */
	if (dir) {
		if (debug)
			fprintf (stderr, "chdir %s\n", dir);

		if (chdir (dir)) {
			error (0, errno,
			       CATGETS (man_CD, "can't chdir to %s"),
			       dir);
			return 0;
		}
	}

	/* define roff_cmd */
	{
		char *source_file;
#ifdef COMP_SRC
		if ( !man_file || !(source_file = get_ztemp()) )
			source_file = man_file;
#else
		source_file = man_file;
#endif /* COMP_SRC */

		if (source_file)
			roff_cmd = make_roff_command (dir, source_file);
		else
			roff_cmd = NULL;
	}

	if (troff) {
		found = !access (man_file, R_OK);
		if (found) {
			if (pause && do_prompt(title))
				return 0;
			checked_system (roff_cmd);
		}
	} else {

		int format;
		char *cmd;
		int status;
		char *catpath;

		/* cat_file is the alternate cat_file.. */
		/* If !man_file, we have a straycat
		   If !cat_file, we can't produce a cat_file, but we
		   may still have one tucked away under /usr... 
		   Check there first ala the FSSTND, and display if newer
		   than man_file, if older, ignore it altogether */

		if (!local_man_file && global_manpath) {
			catpath = global_catpath(dir);

			assert (catpath);
			assert (dir);

			if (man_file && strcmp(catpath, dir) != 0) {
				/* we may have a FSSTND cat != cat_file */
				char *std_cat_file;
	
				std_cat_file = convert_name(man_file, NULL);
				status = is_newer (man_file, std_cat_file);
	
				if (status != -2 && !(status & 1) == 1) {
					cat_file = std_cat_file;
					save_cat = format = 0;
				} else
					format = 1;
				/* @@@ memory leak of std_cat_file */
			} else
				format = 1;

			free (catpath);
		} else
			format = 1;
		
		if (!man_file) {
			assert (cat_file);
			format = 0;
		} else if (!cat_file) {
			assert (man_file);
			save_cat = 0;
			format = 1;
		} else if (format) {
			int status;
			char *cat_dir;
			char *tmp;

			status = is_newer (man_file, cat_file);
			format = (status == -2) || ((status & 1) == 1);

			/* don't save if we haven't a cat directory */
			cat_dir = xstrdup (cat_file);
			if ((tmp = strrchr (cat_dir, '/'))) *tmp = 0;
			save_cat = is_directory (cat_dir) == 1;
			if (debug && !save_cat)
				fprintf(stderr, "cat dir %s does not exist\n",
					cat_dir);
			free (cat_dir);
		}

		found = !access (format ? man_file : cat_file, R_OK);

		if (debug)
			fprintf(stderr, "format: %d, save_cat: %d, found: %d\n",
				format, save_cat, found);

		if (found) {
			if (print_where) {
				if (man_file)
					printf ("%s ", man_file);
				if (cat_file && !format)
					printf ("%s ", cat_file);
				putchar ('\n');
			} else if (catman) {
				if (format) {
					if (!save_cat) {
						error (0, 0,
						       CATGETS (man_NO_CAT, 
						       		"\ncannot write to %s in catman mode"),
						       cat_file);
					} else {
						char *tmpcat = tmp_cat_filename (cat_file);
						cmd = strappend (NULL, roff_cmd,
#ifdef COMP_CAT	
								 " | " COMPRESSOR " >",
#else
								 " >",
#endif /* COMP_CAT */
								 tmpcat, NULL);
						/* save the cat as real user
						   (1) required for user man hierarchy
						   (2) else depending on ruid's privs
						       is ok, effectively disables
						       catman for non-root.
						 */
						push_cleanup ((void (*)())unlink, tmpcat);
						status = do_system_drop_privs (cmd);
						if (status) gripe_system (cmd, status);
						free (cmd);

						commit_tmp_cat (cat_file, tmpcat, status);
						pop_cleanup ();
						free (tmpcat);
					}
				}

			} else if (format) {
				/* no cat or out of date */
				char *disp_cmd;

				if (pause && do_prompt(title)) {
					if (roff_cmd)
						free (roff_cmd);
					if (local_man_file)
						return 1;
					else
						return 0;
				}

				fprintf (stderr,
					CATGETS(man_REFORMAT, 
					"Reformatting %s, please wait...\n"),
					title);
				disp_cmd = make_display_command (NULL, title);
	
#ifdef MAN_CATS
				if (save_cat) {
					/* save cat */
					format_display_and_save (roff_cmd,
								 disp_cmd,
								 cat_file);
				} else 
#endif /* MAN_CATS */
					{
					/* don't save cat */
					cmd = strappend (NULL, roff_cmd,
							 " | ", disp_cmd, NULL);
					status = do_system_drop_privs (cmd);
					/* some shells report broken pipe, ignore it */
					if (status && 
					    (status != 256*(SIGPIPE + 0x80)) )
						gripe_system (disp_cmd, status);
					free (cmd);
				}

				free (disp_cmd);

			} else {
				/* display preformatted cat */
#if defined(COMP_SRC)
				struct compression *comp;

				if (pause && do_prompt(title)) {
					if (roff_cmd)
						free (roff_cmd);
					return 0;
				}

				comp = comp_info (cat_file);
				if (comp) {
					char *disp_cmd;

					disp_cmd = make_display_command (NULL, title);
					cmd = strappend (NULL, comp->prog,
							 " '", cat_file,
							 "' | ", disp_cmd, NULL);
					free(disp_cmd);
				} else 
					cmd = make_display_command (cat_file, 
								    title);
#elif defined(COMP_CAT)
				char *disp_cmd;

				if (pause && do_prompt(title)) {
					if (roff_cmd)
						free (roff_cmd);
					return 0;
				}

				disp_cmd = make_display_command (NULL, title);
				cmd = strappend (NULL, DECOMPRESSOR " '",
						 cat_file,
						 "' | ", disp_cmd, NULL);
				free (disp_cmd);
#else /* !(COMP_SRC || COMP_CAT) */
				if (pause && do_prompt(title)) {
					if (roff_cmd)
						free (roff_cmd);
					return 0;
				}

				cmd = make_display_command (cat_file, title);
#endif /* COMP_SRC */
				status = do_system_drop_privs (cmd);
				/* some shells report broken pipe, ignore it */
				if (status && (status != (SIGPIPE + 0x80) << 8))
					gripe_system (cmd, status);
				free (cmd);
			}
		}
	}

	if (roff_cmd)
		free (roff_cmd);
		
	if (!pause)
		pause = found;

	return found;
}


static char *find_cat_file (char *path, char *man_file, char *sec)
{
	char *cat_file, *cat_path;

	/* could do this with `global' */

	cat_path = global_catpath(man_file);
	cat_file = convert_name(man_file, cat_path);

	if (cat_path)
		free(cat_path);

	return cat_file;
}


/*
 * See if the preformatted man page or the source exists in the given
 * section.
 */
static int try_section (char *path, char *sec, char *name)
{
	int found = 0;
	char **names, **np;
	char *title;

	if (debug) 
		fprintf (stderr, "trying section %s with globbing\n", sec);

	title = strappend (NULL, name, "(", sec, ")", NULL);

#ifndef NROFF_MISSING /* #ifdef NROFF */
	/*
  	 * Look for man page source files.
  	 */

	names = look_for_file(path, sec, name, 0);
	if (!names)
		/*
    		 * No files match.  
    		 * See if there's a preformatted page around that
    		 * we can display.
    		 */
#endif /* NROFF_MISSING */
	{
		if (catman) {
			free(title);
			return ++found;
		}
		
		if (!troff) {
			names = look_for_file (path, sec, name, 1);

			if (names)
				for (np = names; *np; np++)
					found += display (path, NULL, *np, 
							  title);
		}
	}
#ifndef NROFF_MISSING
	else {
		for (np = names; *np ; np++){
			char *man_file;
			char *cat_file;

			if ( (man_file = ult_src (*np, path, NULL, SO_LINK|SOFT_LINK|HARD_LINK)) == NULL) {
				free (title);
				return 0;
			}

			if (debug)
				fprintf (stderr, "found ultimate source file %s\n", man_file);

			cat_file = find_cat_file (path, man_file, sec);
			found += display (path, man_file, cat_file, title);
			free (cat_file);
#ifdef COMP_SRC
			/* if ult_src() produced a ztemp file, we need to 
			   remove it (and unexist it) before proceeding */
			remove_ztemp();
#endif /* COMP_SRC */
			/* free(man_file); can't free this, it's static !! */
		}
	}
#endif /* NROFF_MISSING */

	free (title);
	return found;
}

#ifdef MAN_DB_UPDATES
/* wrapper to dbdelete which deals with opening/closing the db */
static void dbdelete_wrapper(char *page, struct mandata *info)
{
	if ( !catman && (dbf = MYDBM_RWOPEN(database)) ) {
		if ( dbdelete(page, info) == 1)
			if (debug)
				fprintf(stderr, "%s(%s) not in db!\n",
					page, info->ext);
		MYDBM_CLOSE(dbf);
	}
}
#endif /* MAN_DB_UPDATES */

/* This started out life as try_section, but a lot of that routine is 
   redundant wrt the db cache. */
static int try_db_section (char *orig_name, char *path, struct mandata *in)
{
	int found = 0;
	char *file, *name;
	char *title;
#ifdef MAN_DB_UPDATES
	struct stat buf;
#endif

	if (debug) {
		fprintf (stderr, "trying a db located file.\n");
		dbprintf(in);
	}
		
	/* if the pointer holds some data, this is a reference to the 
	   real page, use that instead. */
	if (*in->pointer != '-')
		name = in->pointer;
	else
		name = orig_name;
		
	/* make sure the file we want is the same as the one in the 
	   filesystem */

#ifdef MAN_DB_UPDATES
	/* The next piece of code examines the db found manual page
	   and checks for consistency */
	file = make_filename(path, name, in, "man");
	if (lstat(file, &buf) == 0 && buf.st_mtime != in->_st_mtime) {
		/* update of this file required */
		if (debug)
			fprintf(stderr, "%s needs to be recached: %ld %ld.\n", 
				file, (long)in->_st_mtime, (long)buf.st_mtime);
		if ( (dbf = MYDBM_RWOPEN(database)) ) {
			dbdelete(orig_name, in);
			test_manfile(file, path);
			in = dblookup_exact(orig_name, in->ext);
			MYDBM_CLOSE(dbf);
			if (!in)
				return 0;
			if (*in->pointer != '-')
				name = in->pointer;
			else
				name = orig_name;
		} 
		  else
			error (0, errno,
			       CATGETS(man_UPDATE_DB, "can't update index cache %s"),
			       database);
	}
#endif /* MAN_DB_UPDATES */

#ifndef NROFF_MISSING /* #ifdef NROFF */
	/*
  	 * Look for man page source files.
  	 */

	title = strappend (NULL, name, "(", in->ext, ")", NULL);

	if (in->id < STRAY_CAT) {	/* There should be a src page */
		file = make_filename(path, name, in, "man");
		if (debug)
			fprintf(stderr, "Checking physical location: %s\n", file);

		if (access (file, R_OK) == 0) {
			char *man_file;
			char *cat_file;

			if ( (man_file = ult_src (file, path, NULL, SO_LINK|SOFT_LINK|HARD_LINK)) == NULL) {
				free (title);
				return found; /* zero */
			}

			if (debug)
				fprintf (stderr, "found ultimate source file %s\n", man_file);

			cat_file = find_cat_file (path, man_file, in->ext);
			found += display (path, man_file, cat_file, title);
			free (cat_file);
#ifdef COMP_SRC
			/* if ult_src() produced a ztemp file, we need to 
			   remove it (and unexist it) before proceeding */
			remove_ztemp();
#endif /* COMP_SRC */

		} /* else {drop through to the bottom and return 0 anyway} */
	} else 

#endif /* NROFF_MISSING */
	
	if (in->id <= WHATIS_CAT) {

		/* The db says we have a stray cat or whatis ref */
		   
		if (catman) {
			free (title);
			return ++found;
		}

		/* show this page but force an update later to make sure
		   we haven't just added the new page */
		found_a_stray = 1;
		
		if (!troff) {
			file = make_filename(path, name, in, "cat");
			if (debug)
				fprintf(stderr, "Checking physical location: %s\n", file);

			if ( access(file, R_OK) != 0 ) {
				char *catpath;
				catpath = global_catpath(path);

				if ( catpath && strcmp(catpath, path) != 0 ) {
				
					file = make_filename(catpath, name, 
							     in, "cat");
					free(catpath);
					if (debug)
						fprintf(stderr, "Checking physical location: %s\n", file);

					if ( access(file, R_OK) != 0 ) {
						/* don't delete here, 
						   return==0 will do that */
						free (title);
						return found; /* zero */
					}
				} else {
					if (catpath)
						free(catpath);
					free (title);
					return found; /* zero */
				}
			}

			found += display (path, NULL, file, title);
		}
	}
	free (title);
	return found;
}

/* test for existence, if fail: call dbdelete_wrapper, else return amount */
static int exist_check(char *name, char *manpath, struct mandata *loc)
{
	int exists;

	exists = try_db_section(name, manpath, loc);
			
#ifdef MAN_DB_UPDATES
	if (exists == 0)
		dbdelete_wrapper(name, loc);
#endif /* MAN_DB_UPDATES */

	return exists;
}
	
/* db wrapper for try_db_section(). If db not accessable, return -1, 
   otherwise return amount of pages found/displayed */
static int try_db(char *manpath, char *sec, char *name)
{
	int found = 0;
	struct nlist *in_cache;
	struct mandata *loc, *data, *store[ENTRIES], *exact_ext = NULL;
	struct mandata **exact_sec = store;

	/* find out where our db for this manpath should be */
	
	if (global_manpath) {
		char *catpath;

		catpath = global_catpath(manpath);
		assert(catpath);
		database = mkdbname(catpath);
		free(catpath);
	} else
		database = mkdbname(manpath);

	in_cache = lookup(manpath); /* have we looked here already? */
	
	if ( !in_cache ) {
		if ( (dbf = MYDBM_RDOPEN(database)) && !dbver_rd(dbf)) {
			if (debug)
				fprintf(stderr,
					"Succeeded in opening %s O_RDONLY\n",
					database);

			/* if section is set, only return those that match,
			   otherwise NULL retrieves all available */
			data = dblookup_all(name, section);
			(void) install_db_ptr(manpath, data); 
			MYDBM_CLOSE(dbf);
#ifdef MAN_DB_CREATES
		} else if (!global_manpath) {
			/* create one */
			free_hashtab();
			if (debug)
				fprintf(stderr, 
					"Failed to open %s O_RDONLY\n",
					database);
			/* create_db should really return EOF on failure. */
			if (create_db (manpath) == 0) {
				data = infoalloc();
				data->next = data->addr = NULL;
				(void) install_db_ptr(manpath, data);
				return -1;
			}
			return -2;
#endif /* MAN_DB_CREATES */
		} else {
			if (debug)
				fprintf(stderr, 
					"Failed to open %s O_RDONLY\n",
					database);
			data = infoalloc();
			data->next = (struct mandata *) NULL;
			data->addr = NULL;
			(void) install_db_ptr(manpath, data);
			return -1; /* indicate failure to open db */
		}
	} else
		data = in_cache->defn;

	/* if we already know that there is nothing here, get on with it */
	if (!data)
		return found; /* 0 */

	/* We already tried (and failed) to open this db before */
	if (!data->addr)
		return -1;
		
	/* cycle through the mandata structures (there's usually only 
	   1 or 2) and see what we have w.r.t. the current section */
	for (loc = data; loc; loc = loc->next) {
		if (!extension || strcmp(extension, loc->ext) == 0
			       || strcmp(extension, 
					 loc->ext + strlen(sec)) == 0) {
			if (strcmp(loc->ext, sec) == 0)
				exact_ext = loc;
			else if (strncmp(loc->ext, sec, strlen(sec) ) == 0)
				*(exact_sec++) = loc;
		}
	}
	*exact_sec = NULL;

	/* ALL free()ing of structures must be done by free_hashtab() only */

	/* first see if we have the right extension */
	if (exact_ext)
		found = exist_check(name, manpath, exact_ext);

	/* if (not or -a) and (we have a correct section), show that */
	if (findall || !found) {
		for (exact_sec = store; *exact_sec; exact_sec++) {
			found += exist_check(name, manpath, *exact_sec);
			if (found && !findall)
				break;
		}
	}

	return found;
}

/* try to locate the page under the specified manpath, in the derised section,
   with the supplied name. glob if necessary. Initially try to find it via
   a db cache access, if that fails, search the filesystem. */
static int locate_page(char *manpath, char *sec, char *name)
{
	int found, db_ok;

	/* sort out whether we want to treat this hierarchy as 
	   global or user. Differences:

	   global: if setuid, use privs; don't create db.
	   user  : if setuid, drop privs; allow db creation. */
	   
	if (is_global_mandir(manpath)) {
		global_manpath = 1;
	} else {
		global_manpath = 0;
		drop_effective_privs();
	}
		
	if (debug)
		fprintf (stderr, "searching in %s, section %s\n", 
			 manpath, sec);

	db_ok = try_db(manpath, sec, name);

#ifdef MAN_DB_CREATES
	if (db_ok == -2) /* we created a db in the last call */
		db_ok = try_db(manpath, sec, name);
#endif /* MAN_DB_CREATES */

	if (db_ok == -1)  /* we failed to find/open a db */
		found = try_section (manpath, sec, name);
	else
		found = db_ok;
	
	if (!global_manpath)
		regain_effective_privs();
		
	return found;
}

/*
 * Search for manual pages.
 *
 * If preformatted manual pages are supported, look for the formatted
 * file first, then the man page source file.  If they both exist and
 * the man page source file is newer, or only the source file exists,
 * try to reformat it and write the results in the cat directory.  If
 * it is not possible to write the cat file, simply format and display
 * the man file.
 *
 * If preformatted pages are not supported, or the troff option is
 * being used, only look for the man page source file.
 *
 */
static int man (char *name)
{
	int found = 0;

	fflush (stdout);

	if (section) {
		char **mp;
		
		for (mp = manpathlist; *mp; mp++) {
			found += locate_page(*mp, section, name);
			if (found && !findall)   /* i.e. only do this section... */
				return found;
		}
	} else {
		char **sp;
		
		for (sp = section_list; *sp; sp++) {
			char **mp;
			
			for (mp = manpathlist; *mp; mp++) {
				found += locate_page(*mp, *sp, name);
				if (found && !findall)   /* i.e. only do this section... */
					return found;
			}
		}
	}
	
#ifdef MAN_DB_UPDATES
	/* check to see if any of the databases need updating */
	if ((!found || found_a_stray) && update_required) {
		/* must free_hashtab() here in case testmandirs() 
		   wants to use it */
		free_hashtab();
	
		if ( need_to_rerun() )
			return man(name);
	}
#endif /* MAN_DB_UPDATES */

	return found;
}


static __inline__ char **get_section_list (void)
{
	int i = 0;
	char **sections = NULL;
	char *sec;

	if (colon_sep_section_list == NULL) {
		colon_sep_section_list = getenv ("MANSECT");
		if (colon_sep_section_list == NULL || 
		    *colon_sep_section_list == '\0')
			return std_sections;
	}

	sec = xstrdup(colon_sep_section_list);

	for (sec = strtok(colon_sep_section_list, ":"); sec; 
	     sec = strtok(NULL, ":")) {
		sections = (char **) xrealloc (sections,
 					       (i + 2) * sizeof (char *));
 		sections[i++] = sec;
 	}
 	sections[i] = NULL;
	return sections;
}

/* allow user to skip a page or quit after viewing desired page 
   return 1 to skip
   return 0 to view
 */
static __inline__ int do_prompt (char *name)
{
	if (isatty (STDOUT_FILENO) && isatty (STDIN_FILENO)) {
		fprintf (stderr, CATGETS(man_NEXT, 
		         "--Man-- next: %s "
		         "[ view (return) | skip (Ctrl-D) | quit (Ctrl-C) ]\n"), 
		         name);
		fflush (stderr);

		do {
			switch( getchar() ) {
				case '\n':
					return 0;
				case EOF:
					return 1;
				default:
					break;
			}
		} while (1);
	}
	return 0;
}
