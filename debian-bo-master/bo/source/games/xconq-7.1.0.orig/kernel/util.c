/* Random utilities not classifiable elsewhere.
   Copyright (C) 1987, 1988, 1989, 1991, 1992, 1993, 1994, 1995, 1996
   Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

#include "config.h"
#include "misc.h"
extern void update_debugging PARAMS ((void));
extern void toggle_debugging PARAMS ((int *flagp));
extern void prealloc_debug PARAMS ((void));
#include "lisp.h"
#include "dir.h"

/* Accommodate certain obsolete (pre-ANSI) Unix systems. (?) */

#ifdef TIME_RETURNS_LONG
#ifndef time_t
#define time_t long
#endif
#endif

/* Direction-to-delta conversions. */

int dirx[NUMDIRS] = DIRX;

int diry[NUMDIRS] = DIRY;

/* If true, warnings will not be issued. */

int warnings_suppressed;

/* If true, allocation attempts will garner complaints, but not actually
   fail.  (This is primarily for debugging addon code, such as new AIs
   and user interfaces.) */

int xmalloc_warnings;

/* If true, all allocation will fail, suppresses further attempts. */

int memory_exhausted;

static char *padbuf;

/* Random number handling is important to game but terrible/nonexistent
   in some systems.  Do it ourselves and hope it's better.  Unfortunately,
   it's hard to prevent clueless hackers from calling rand or random
   and thereby confusing everything, so there (should be) is a check in
   the test dir that searches for calls to these. */

/* The random state *must* be at least 32 bits. */

long initrandstate = -1;

long randstate = -1;

/* Seed can come from elsewhere, for repeatability.  Otherwise, it comes
   from the current time, scaled down to where 32-bit arithmetic won't
   overflow. */

void
init_xrandom(seed)
int seed;
{
    time_t tm;
    	
    if (seed >= 0) {
	/* If the random state is already set, changes are somewhat
	   suspicious. */
	if (randstate >= 0) {
	    Dprintf("Randstate being changed from %d to %d\n",
		    randstate, seed);
	}
	randstate = seed;
    } else {
    	time(&tm);
    	randstate = (long) tm;
    }
    /* Whatever its source, put the randstate into known range (0 - 99999). */
    randstate = ABS(randstate);
    randstate %= 100000L;
    /* This is kept around for the sake of error reporting. */
    initrandstate = randstate;
}

/* Numbers lifted from Numerical Recipes, p. 198. */
/* Arithmetic must be 32-bit. */

int
xrandom(m)
int m;
{
    randstate = (8121 * randstate + 28411) % 134456L;
    return ((m * randstate) / 134456L);
}

/* Percentage probability, with bounds checking. */

int
probability(prob)
int prob;
{
    if (prob <= 0)
      return FALSE;
    if (prob >= 100)
      return TRUE;
    return (xrandom(100) < prob);
}

int
roll_dice(n)
int n;
{
    int numdice, dice, i, rslt;

    if (n >> 14 == 0 || n >> 14 == 3)
      return n;
    numdice = (n >> 11) & 0x07;
    dice = (n >> 7) & 0x0f;
    rslt = n & 0x7f;
    for (i = 0; i < numdice; ++i) {
	rslt += xrandom(dice);
    }
    return rslt;
}

int
multiply_dice(n, mult)
int n, mult;
{
    int numdice, dice, addend;

    if (n >> 14 == 0 || n >> 14 == 3)
      return (n * mult) / 100;
    numdice = (n >> 11) & 0x07;
    dice = (n >> 7) & 0x0f;
    dice = (dice * mult) / 100;
    addend = ((n & 0x7f) * mult) / 100;
    return (1 << 14) | (numdice << 11) | (dice << 7) | (addend & 0x7f);
}

/* For a number in the range 0 - 10000, divide it by 100 and use the
   remainder as the probability of adding 1. */

int
prob_fraction(n)
int n;
{
  return (n / 100 + (probability(n % 100) ? 1 : 0));
}

#ifdef DEBUGGING

/* This tracks our total space allocation. */

#define NUMMALLOCRECS 200

int overflow;

int numoverflow;

int totmalloc;

int nextmalloc;

int copymalloc;

int grandtotmalloc;

struct a_malloc {
    int size;
    int count;
} *mallocs = NULL;

static void record_malloc PARAMS ((int amt));
static int malloc_record_compare PARAMS ((const void *m1, const void *m2));

/* Given an amount of memory allocated, record it with the others. */

static void
record_malloc(amt)
int amt;
{
    int i;

    /* Might need to allocate the record of allocations. */
    if (mallocs == NULL) {
	/* Don't use xmalloc here!! */
	mallocs = (struct a_malloc *)
	  malloc(NUMMALLOCRECS * sizeof(struct a_malloc));
	overflow = 0;
	numoverflow = 0;
    }
    /* Search for already-recorded allocations of same-size blocks. */
    for (i = 0; i < nextmalloc; ++i) {
	if (amt == mallocs[i].size) {
	    ++(mallocs[i].count);
	    return;
	}
    }
    if (nextmalloc < NUMMALLOCRECS) {
	/* Record this allocation as a new size of allocation. */
	mallocs[nextmalloc].size = amt;
	mallocs[nextmalloc].count = 1;
	++nextmalloc;
    } else {
    	/* Add to the overflow bucket, which is used for allocations whose
	   sizes could not be recorded individually.  Yes, we could allocate
	   more buckets, but this seems like too much trouble for an
	   uncommon case. */
    	overflow += amt;
    	++numoverflow;
    }
}

static int
malloc_record_compare(m1, m2)
CONST void *m1, *m2;
{
    return (((struct a_malloc *) m1)->size * ((struct a_malloc *) m1)->count
            - ((struct a_malloc *) m2)->size * ((struct a_malloc *) m2)->count);
}

/* Display memory consumption and reset records.  This does not account
   for freeing, but Xconq usually tries to hang onto and reuse anything
   it allocates, very rarely calls free(). */

void
report_malloc()
{
    extern int lispmalloc, numsymbols;
    int i;

    /* If this is called too soon, just leave silently. */
    if (mallocs == NULL)
      return;
    if (nextmalloc == 0) {
	Dprintf("No mallocs since last report.\n");
	return;
    }
    Dprintf("Mallocs since last report:\n");
    Dprintf("    Amount =  Bytes x  Times\n");
    /* Sort the entries. */
    qsort(mallocs, nextmalloc, sizeof(struct a_malloc), malloc_record_compare);
    /* Write out all the records, formatting nicely. */
    for (i = 0; i < nextmalloc; ++i) {
	Dprintf("%10d = %6d x %6d\n",
		mallocs[i].size * mallocs[i].count,
		mallocs[i].size,  mallocs[i].count);
    }
    if (overflow > 0)
      Dprintf("%10d = ?????? x %6d\n", overflow, numoverflow);
    Dprintf("Total malloced = %d bytes.\n", totmalloc);
    Dprintf("String copies = %d bytes.\n", copymalloc);
    Dprintf("Lisp malloced = %d bytes.\n", lispmalloc);
    Dprintf("Symbols interned = %d.\n", numsymbols);
    Dprintf("Grand total allocation = %d bytes.\n", grandtotmalloc);
    /* Reset all the counters for next time. */
    nextmalloc = 0;
    overflow = numoverflow = 0;
    totmalloc = copymalloc = lispmalloc = 0;
}

#endif /* DEBUGGING */

/* This is our improved and adapted version of malloc, that guarantees
   zeroing of the block, checks for memory exhaustion, and collects
   usage statistics. */

char *
xmalloc(amt)
int amt;
{
    char *value;

    if (xmalloc_warnings) {
	run_warning("Should not be calling xmalloc (requested %d bytes)", amt);
	/* Keep going though. */
    }
    if (memory_exhausted)
      return NULL;
    /* Do the actual allocation. */
    value = (char *) malloc(amt);
    if (value == NULL) {
#ifdef DEBUGGING
	if (Debug) {
	    Dprintf("Unable to allocate %d bytes.\n", amt);
	    /* Write out the allocation report if possible. */
	    report_malloc();
	}
#endif /* DEBUGGING */
	/* This is pretty serious, no way to recover. */
	memory_exhausted = TRUE;
	run_error("Memory exhausted!!");
	/* In case run_error doesn't exit. */
  	exit(1);
    }
    /* Save callers from having to clear things themselves. */
    memset(value, 0, amt);
#ifdef DEBUGGING
    /* This can't be controlled by `Debug', because much allocation
       happens before any command line or dialog options are interpreted. */
    totmalloc += amt;
    grandtotmalloc += amt;
    record_malloc(amt);
#endif /* DEBUGGING */
    return value;
}

/* Like sprintf, but appends. */

#ifdef __STDC__
void
tprintf(char *buf, char *str, ...)
{
    va_list ap;
    char line[300];

    va_start(ap, str);
    vsprintf(line, str, ap);
    strcat(buf, line);
    va_end(ap);
}

void
tnprintf(char *buf, int n, char *str, ...)
{
    va_list ap;
    int n1 = n - strlen(buf);
    char line[300];

    if (n1 > 0) {
	va_start(ap, str);
	vsprintf(line, str, ap);
	strncat(buf, line, n1);
	va_end(ap);
    }
}

void
vtprintf(char *buf, char *str, va_list ap)
{
    char line[300];

    vsprintf(line, str, ap);
    strcat(buf, line);
}
#else
void
tprintf(buf, str, a1, a2, a3, a4, a5, a6, a7, a8, a9)
char *buf, *str;
long a1, a2, a3, a4, a5, a6, a7, a8, a9;
{
    char line[300];

    sprintf(line, str, a1, a2, a3, a4, a5, a6, a7, a8, a9);
    strcat(buf, line);
}

void
tnprintf(buf, n, str, a1, a2, a3, a4, a5, a6, a7, a8, a9)
char *buf, *str;
int n;
long a1, a2, a3, a4, a5, a6, a7, a8, a9;
{
    int n1 = n - strlen(buf);
    char line[300];

    if (n1 > 0) {
	sprintf(line, str, a1, a2, a3, a4, a5, a6, a7, a8, a9);
	strncat(buf, line, n1);
    }
}
#endif

/* Copy a string to newly-allocated space.  The new space is never freed. */

char *
copy_string(str)
char *str;
{
    int len = strlen(str);
    char *rslt;

    rslt = xmalloc(len + 1);
    strcpy(rslt, str);
#ifdef DEBUGGING
    copymalloc += len + 1;
#endif
    return rslt;
}

/* Insert the given number of blanks between each char of the string. */

char *
pad_blanks(str, n)
char *str;
int n;
{
    char *pb;
    int i;
  
    if (padbuf == NULL)
      padbuf = xmalloc(BUFSIZE);
    pb = padbuf;
    while (*str && pb < padbuf + BUFSIZE - 2) {
	*pb++ = *str++;
	if (*str) {
	    for (i = 0; i < n; i++)
	      *pb++ = ' ';
	}
    }
    *pb = '\0';
    return padbuf;
}

/* Get a *numeric* index into a string (more useful than ptr, in Xconq).
   Return -1 on failed search. */

int
iindex(ch, str)
int ch;
char *str;
{
    int i;

    if (str == NULL)
      return (-1);
    for (i = 0; str[i] != '\0'; ++i)
      if (ch == str[i])
        return i;
    return (-1);
}

/* Return a time difference as an long integer number of seconds. */

long
idifftime(t1, t0)
time_t t1, t0;
{
#ifdef MAC /* (should be anywhere that actually has a difftime function) */
    return ((long) difftime(t1, t0));
#else
    return ((long) t1 - (long) t0);
#endif
}

/* This little routine goes at the end of all switch statements on internal
   data values. */

void
case_panic(str, var)
char *str;
int var;
{
    run_error("Panic! Unknown %s %d", str, var);
}

/* Integer square root - good enough, no float trickery or libs needed. */

/* Improved version from Bruce Fast, via Scott Herod. */

int
isqrt(i)
int i;
{
    int j, k;

    if (i > 3) {
        for (j = i, k = -1; j >>= 2; k <<= 1);
        k = (~k + i / ~k) / 2;
	k = (k + i / k) / 2;
	k = (1 + k + i / k) / 2;
	return (k + i / k) / 2;
    } else if (i > 0) {
	return 1;
    } else {
	return 0;
    }
}

#ifdef DEBUGGING

/* Debugging flags.
   These are set up so that they can be macros or variables, as desired. */

#ifndef Debug
int Debug = FALSE;
#endif
#ifndef DebugM
int DebugM = FALSE;
#endif
#ifndef DebugG
int DebugG = FALSE;
#endif

/* These are where normal debugging output goes. */

FILE *dfp = NULL;
FILE *dgfp = NULL;
FILE *dmfp = NULL;

/* Map all the debugging outputs to stdout. */
/* You might think stderr would be a better choice, but this way
   the debugging output is sync'ed properly (read: consistently!)
   with "normal" stdout output. */

void
init_debug_to_stdout()
{
   	dfp  = stdout;
    dgfp = stdout;
    dmfp = stdout;
}

/* Junk associated with debug output. */

/* This is all fairly elaborate because we need to be able to collect
   detailed logs of AI activity over different periods of time, and just
   dumping to stdout doesn't work in a window system. */

FILE *ffp = NULL;

int firstdebug = TRUE;

void
update_debugging()
{
	if (Debug || DebugG || DebugM) {
		/* Always close the file if open, forces to desirable state. */
		if (ffp != NULL) {
			fclose(ffp);
			ffp = NULL;
		}
		/* Reopen the file. */
		if (ffp == NULL) {
			ffp = fopen("Xconq.DebugOut", "a");
		}
		if (ffp != NULL) {
			if (Debug)
			  dfp = ffp;
			if (DebugG)
			  dgfp = ffp;
			if (DebugM)
			  dmfp = ffp;
		}
	}
}

/* Debug output goes to a file. */

void
toggle_debugging(flagp)
int *flagp;
{
	/* Always close the file if open, forces to desirable state. */
	if (ffp != NULL) {
		fclose(ffp);
		ffp = NULL;
	}
	/* Flip the state of the debugging flag, if supplied. */ 
	if (flagp != NULL) {
		*flagp = ! *flagp;
	}
	/* (Re-)open the debugging transcript file. */
	if (ffp == NULL) {
		ffp = fopen("Xconq.DebugOut", (firstdebug ? "w" : "a"));
		firstdebug = FALSE;
	}
	if (ffp != NULL && flagp != NULL) {
		/* Indicate which flags are now on. */
		fprintf(ffp, "\n\n*********** %s %s %s **********\n\n",
				(Debug ? "Debug" : ""), (DebugM ? "DebugM" : ""),
				(DebugG ? "DebugG" : ""));
	}
	/* Set specific debug file pointers to be the same as the
	   pointer to the file. */
	if (ffp != NULL) {
		if (Debug)
		  dfp = ffp;
		if (DebugG)
		  dgfp = ffp;
		if (DebugM)
		  dmfp = ffp;
	}
	/* If all debugging flags have been turned off, close the file too. */
	if (!Debug && !DebugG && !DebugM) {
		if (ffp != NULL) {
			fclose(ffp);
			ffp = NULL;
		}
	}
	prealloc_debug();
}

#ifdef __STDC__

void
debug_printf(char *str, ...)
{
    va_list ap;

    va_start(ap, str);
    vfprintf(dfp, str, ap);
    va_end(ap);
}

void
debugm_printf(char *str, ...)
{
    va_list ap;

    va_start(ap, str);
    vfprintf(dmfp, str, ap);
    va_end(ap);
}

void
debugg_printf(char *str, ...)
{
    va_list ap;

    va_start(ap, str);
    vfprintf(dgfp, str, ap);
    va_end(ap);
}
#else
/* Kind of cheesy, should only use if real ANSI not available. */

void
debug_printf(str, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
char *str;
long a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12;
{
    fprintf(dfp, str, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12);
}

void
debugm_printf(str, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
char *str;
long a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12;
{
    fprintf(dmfp, str, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12);
}

void
debugg_printf(str, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
char *str;
long a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12;
{
    fprintf(dgfp, str, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12);
}
#endif /* __STDC__ */

#endif /* DEBUGGING */

void
#ifdef __STDC__
init_error(char *str, ...)
#else
init_error(str, a1, a2, a3, a4, a5, a6, a7, a8, a9)
char *str;
long a1, a2, a3, a4, a5, a6, a7, a8, a9;
#endif
{
    char buf[BUFSIZE];

#ifdef __STDC__
    {
	va_list ap;

	va_start(ap, str);
	vsprintf(buf, str, ap);
	va_end(ap);
    }
#else
    sprintf(buf, str, a1, a2, a3, a4, a5, a6, a7, a8, a9);
#endif
    low_init_error(buf);
}

void
#ifdef __STDC__
init_warning(char *str, ...)
#else
init_warning(str, a1, a2, a3, a4, a5, a6, a7, a8, a9)
char *str;
long a1, a2, a3, a4, a5, a6, a7, a8, a9;
#endif
{
    char buf[BUFSIZE];

	if (warnings_suppressed)
	  return;
#ifdef __STDC__
    {
	va_list ap;

	va_start(ap, str);
	vsprintf(buf, str, ap);
	va_end(ap);
    }
#else
    sprintf(buf, str, a1, a2, a3, a4, a5, a6, a7, a8, a9);
#endif
    low_init_warning(buf);
}

/* A run error is fatal. */

void
#ifdef __STDC__
run_error(char *str, ...)
#else
run_error(str, a1, a2, a3, a4, a5, a6, a7, a8, a9)
char *str;
long a1, a2, a3, a4, a5, a6, a7, a8, a9;
#endif
{
    char buf[BUFSIZE];

#ifdef __STDC__
    {
	va_list ap;

	va_start(ap, str);
	vsprintf(buf, str, ap);
	va_end(ap);
    }
#else
    sprintf(buf, str, a1, a2, a3, a4, a5, a6, a7, a8, a9);
#endif
    low_run_error(buf);
}

/* Runtime warnings are for when it's important to bug the players,
   usually a problem with Xconq or a game design. */

void
#ifdef __STDC__
run_warning(char *str, ...)
#else
run_warning(str, a1, a2, a3, a4, a5, a6, a7, a8, a9)
char *str;
long a1, a2, a3, a4, a5, a6, a7, a8, a9;
#endif
{
    char buf[BUFSIZE];

	if (warnings_suppressed)
	  return;
#ifdef __STDC__
    {
	va_list ap;

	va_start(ap, str);
	vsprintf(buf, str, ap);
	va_end(ap);
    }
#else
    sprintf(buf, str, a1, a2, a3, a4, a5, a6, a7, a8, a9);
#endif
    low_run_warning(buf);
}
