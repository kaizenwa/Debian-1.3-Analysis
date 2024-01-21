/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
**      Profiling module
**
**	Main Author : petdr
*/

#include        "imp.h"

#include	<unistd.h>
#include	<errno.h>
#include	<string.h>

#include        "std.h"
#include	"prof_mem.h"

#if defined(PROFILE_TIME)

#include	<signal.h>

#ifdef HAVE_SYS_PARAM
#include	<sys/param.h>
#endif

#ifdef HAVE_SYS_TIME
#include	<sys/time.h>
#endif

/* 
** if `HZ' is not defined, we may be able to use `sysconf(_SC_CLK_TCK)' instead
*/
#if !defined(HZ) && defined(HAVE_SYSCONF) && defined(_SC_CLK_TCK)
#define HZ ((int)sysconf(_SC_CLK_TCK))
#endif

#if !defined(HZ) || !defined(SIGPROF) || !defined(HAVE_SETITIMER)
#error "Time profiling not supported on this system"
#endif

#endif	/* PROFILE_TIME */

/*******************
  Need to make these command line options
*******************/
#define CALL_TABLE_SIZE 4096
#define TIME_TABLE_SIZE 4096
#define CLOCK_TICKS     5

#define USEC            1000000

/*
** profiling node information 
*/

typedef struct s_prof_call_node
{
        Code			*Callee;
        Code			*Caller;
        unsigned long		count;
        struct s_prof_call_node	*left;
        struct s_prof_call_node	*right;
} prof_call_node;

typedef struct s_prof_time_node
{
        Code			*Addr;
        unsigned long		count;
        struct s_prof_time_node	*left;
        struct s_prof_time_node	*right;
} prof_time_node;

/* 
** Macro definitions 
*/

#define hash_addr_pair(Callee, Caller)                                      \
        (int) ((( (unsigned long)(Callee) ^ (unsigned long)(Caller) ) >> 2) \
                % CALL_TABLE_SIZE )

#define hash_prof_addr(Addr)                                                \
	(int) ( (( (unsigned long)(Addr) ) >> 2) % TIME_TABLE_SIZE )


/*
** Global Variables
*/

Code *		volatile	prof_current_proc;
static volatile	int		in_profiling_code = FALSE;

/* 
** Private global variables
*/

#ifdef PROFILE_CALLS
static	FILE	 	*declfptr = NULL;
static	prof_call_node	*addr_pair_table[CALL_TABLE_SIZE] = {NULL};
#endif

#ifdef PROFILE_TIME
static	prof_time_node	*addr_table[TIME_TABLE_SIZE] = {NULL};
#endif

/* ======================================================================== */

#ifndef HAVE_STRERROR

/*
** Apparently SunOS 4.1.3 doesn't have strerror()
**	(!%^&!^% non-ANSI systems, grumble...)
**
** This code should perhaps go somewhere other than in prof.c.
*/

extern int sys_nerr;
extern char *sys_errlist[];

char *
strerror(int errnum)
{
	if (errnum >= 0 && errnum < sys_nerr && sys_errlist[errnum] != NULL) {
		return sys_errlist[errnum];
	} else {
		static char buf[30];
		sprintf(buf, "Error %d", errnum);
		return buf;
	}
}

#endif

/* ======================================================================== */

/* utility routines for opening and closing files */

#if defined(PROFILE_TIME) || defined(PROFILE_CALLS)

static FILE *
checked_fopen(const char *filename, const char *message, const char *mode)
{
	FILE *file;

	errno = 0;
	file = fopen(filename, mode);
	if (!file) {
		fprintf(stderr, "Mercury runtime: couldn't %s file `%s': %s\n",
				message, filename, strerror(errno));
		exit(1);
	}
	return file;
}

static void
checked_fclose(FILE *file, const char *filename)
{
	errno = 0;
	if (fclose(file) != 0) {
		fprintf(stderr,
			"Mercury runtime: error closing file `%s': %s\n",
			filename, strerror(errno));
		exit(1);
	}
}

#endif /* defined(PROFILE_TIME) || defined(PROFILE_CALLS) */

#ifdef	PROFILE_TIME

static void
checked_setitimer(int which, struct itimerval *value)
{
	errno = 0;
	if (setitimer(which, value, NULL) != 0) {
		perror("Mercury runtime: cannot set timer for profiling");
		exit(1);
	}
}

static void
checked_signal(int sig, void (*handler)(int))
{
	/*
	   We really need sigaction and SA_RESTART, otherwise profiling signals
	   might interrupt I/O, causing a profiled program to get I/O errors.
	   But if we haven't got it, I guess we just have to punt...
	*/
#ifndef SA_RESTART
#define SA_RESTART 0
#endif

#ifdef HAVE_SIGACTION
	struct sigaction	act;
	act.sa_flags = SA_RESTART;
	if (sigemptyset(&act.sa_mask) != 0) {
		perror("Mercury runtime: cannot set clear signal mask");
		exit(1);
	}
	act.sa_handler = handler;
#endif /* HAVE_SIGACTION */
	errno = 0;

#ifdef HAVE_SIGACTION
	if (sigaction(sig, &act, NULL) != 0)
#else
	if (signal(sig, handler) == SIG_ERR)
#endif /* HAVE_SIGACTION */
	{
		perror("Mercury runtime: cannot install signal handler");
		exit(1);
	}
}

#endif /* PROFILE_TIME */

/* ======================================================================== */

#ifdef PROFILE_TIME

static void prof_time_profile(int);

/*
**	prof_init_time_profile:
**		Writes the value of HZ (no. of ticks per second.) at the start
**		of the file 'Prof.Counts'.
**		Then sets up the profiling timer and starts it up. 
**		At the moment it is after every X ticks of the clock.
**		SYSTEM SPECIFIC CODE
*/

void
prof_init_time_profile()
{
	FILE 	*fptr;
	struct itimerval itime;

	/* output the value of HZ */
	fptr = checked_fopen("Prof.Counts", "create", "w");
	fprintf(fptr, "%d %d\n", HZ, CLOCK_TICKS);
	checked_fclose(fptr, "Prof.Counts");

	itime.it_value.tv_sec = 0;
	itime.it_value.tv_usec = (long) (USEC / HZ) * CLOCK_TICKS; 
	itime.it_interval.tv_sec = 0;
	itime.it_interval.tv_usec = (long) (USEC / HZ) * CLOCK_TICKS;

	checked_signal(SIGPROF, prof_time_profile);
	checked_setitimer(ITIMER_PROF, &itime);
}

#endif /* PROFILE_TIME */

/* ======================================================================== */

#ifdef PROFILE_CALLS

/*
**	prof_call_profile:
**		Saves the callee, caller pair into a hash table. If the
**		address pair already exists then it increments a count.
*/

void
prof_call_profile(Code *Callee, Code *Caller)
{
	prof_call_node	*node, **node_addr, *new_node;
	int		 hash_value;

	in_profiling_code = TRUE;

	hash_value = hash_addr_pair(Callee, Caller);

	node_addr = &addr_pair_table[hash_value];
	while ((node = *node_addr) != NULL) {
		if (node->Callee < Callee)
			node_addr = &node->left;
		else if (node->Callee > Callee)
			node_addr = &node->right;
		else if (node->Caller < Caller)
			node_addr = &node->left;
		else if (node->Caller > Caller)
			node_addr = &node->right;
		else {
			node->count++;
			in_profiling_code = FALSE;
			return;
		}
	}

	new_node = prof_make(prof_call_node);
	new_node->Callee = Callee;
	new_node->Caller = Caller;
	new_node->count  = 1;
	new_node->left   = NULL;
	new_node->right  = NULL;
	*node_addr = new_node;

	in_profiling_code = FALSE;
	return;
}

#endif /* PROFILE_CALLS */

/* ======================================================================== */

#ifdef PROFILE_TIME

/*
**	prof_time_profile:
**		Signal handler to be called when ever a SIGPROF is received.
**		Saves the current code address into a hash table. If the
**		address already exists, it increments its count.
*/

static void
prof_time_profile(int signum)
{
	prof_time_node	*node, **node_addr, *new_node;
	int		hash_value;

	/* Ignore any signals we get in this function or in prof_call_profile */
	if (in_profiling_code)
		return;

	in_profiling_code = TRUE;

	hash_value = hash_prof_addr(prof_current_proc);

	node_addr = &addr_table[hash_value];
	while ((node = *node_addr) != NULL) {
		if (node->Addr < prof_current_proc)
			node_addr = &node->left;
		else if (node->Addr > prof_current_proc)
			node_addr = &node->right;
		else {
			node->count++;
			in_profiling_code = FALSE;
			return;
		}
	}

	new_node = prof_make(prof_time_node);
	new_node->Addr  = prof_current_proc;
	new_node->count = 1;
	new_node->left  = NULL;
	new_node->right = NULL;
	*node_addr = new_node;

	in_profiling_code = FALSE;
	return;
}

/* ======================================================================== */

/*
**	prof_turn_off_time_profiling:
**		Turns off the time profiling.
*/

void
prof_turn_off_time_profiling()
{
	struct itimerval itime;

	itime.it_value.tv_sec = 0;
	itime.it_value.tv_usec = 0;
	itime.it_interval.tv_sec = 0;
	itime.it_interval.tv_usec = 0;

	checked_setitimer(ITIMER_PROF, &itime);
}
	
#endif /* PROFILE_TIME */

/* ======================================================================== */

#ifdef PROFILE_CALLS

/*
**	prof_output_addr_pair_table :
**		Writes the hash table to a file called "Prof.CallPair".
**		Caller then callee followed by count.
*/

static	void	print_addr_pair_node(FILE *fptr, prof_call_node *node);

void
prof_output_addr_pair_table(void)
{
	FILE		*fptr;
	int		i;

	fptr = checked_fopen("Prof.CallPair", "create", "w");
	for (i = 0; i < CALL_TABLE_SIZE ; i++)
		print_addr_pair_node(fptr, addr_pair_table[i]);

	checked_fclose(fptr, "Prof.CallPair");
}

static void
print_addr_pair_node(FILE *fptr, prof_call_node *node)
{
	if (node != (prof_call_node *) NULL) {
		fprintf(fptr, "%ld %ld %lu\n",
			(long) node->Caller,
			(long) node->Callee,
			node->count);
		print_addr_pair_node(fptr, node->left);
		print_addr_pair_node(fptr, node->right);
	}
}

#endif /* PROFILE_CALLS */

/* ======================================================================== */

#ifdef PROFILE_CALLS

/*
**	prof_output_addr_decls:
**		Ouputs the main predicate labels as well as their machine
**		addresses to a file called "Prof.Decl".
**		This is called from insert_entry() in label.c.
*/

void
prof_output_addr_decls(const char *name, const Code *address)
{
	if (!declfptr) {
		declfptr = checked_fopen("Prof.Decl", "create", "w");
	}
	fprintf(declfptr, "%ld\t%s\n", (long) address, name);
}

#endif /* PROFILE_CALLS */

/* ======================================================================== */

#ifdef PROFILE_TIME

/*
**	prof_output_addr_table:
**		Outputs the addresses saved whenever SIGPROF is received to
**		the file "Prof.Counts"
*/

static	void	print_time_node(FILE *fptr, prof_time_node *node);

void
prof_output_addr_table()
{
	FILE *fptr;
	int  i;

	fptr = checked_fopen("Prof.Counts", "append to", "a");
	for (i = 0; i < TIME_TABLE_SIZE ; i++)
		print_time_node(fptr, addr_table[i]);

	checked_fclose(fptr, "Prof.Counts");
}

static void
print_time_node(FILE *fptr, prof_time_node *node)
{
	if (node != (prof_time_node *) NULL) {
		fprintf(fptr, "%ld %lu\n",
			(long) node->Addr,
			node->count);
		print_time_node(fptr, node->left);
		print_time_node(fptr, node->right);
	}
}

#endif /* PROFILE_TIME */
