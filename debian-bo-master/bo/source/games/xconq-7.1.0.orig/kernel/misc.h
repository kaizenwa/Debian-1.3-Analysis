/* Random definitions used everywhere in Xconq.
   Copyright (C) 1987, 1988, 1989, 1991, 1992, 1993, 1994, 1995, 1996
   Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

#ifndef TRUE
#define TRUE (1)
#endif
#ifndef FALSE
#define FALSE (0)
#endif

/* This is how we do optional prototypes and const decls. */

#ifdef MSDOS
#define PARAMS(ARGS) ARGS
#define CONST const
#endif

#ifndef PARAMS
#ifdef ANSI_PROTOTYPES
#define PARAMS(ARGS) ARGS
#else
#define PARAMS(ARGS) ()
#endif /* ANSI_PROTOTYPES */
#endif /* PARAMS */

#ifndef CONST
#ifdef __STDC__
#define CONST const
#else
#define CONST
#endif /* __STDC__ */
#endif /* CONST */

#ifndef ABS
#define ABS(x) (((x) < 0) ? (0 - (x)) : (x))
#endif

#ifndef min
#define min(x,y) (((x) < (y)) ? (x) : (y))
#endif

#ifndef max
#define max(x,y) (((x) > (y)) ? (x) : (y))
#endif

#define between(lo,n,hi) ((lo) <= (n) && (n) <= (hi))

#define flip_coin() (xrandom(257) % 2)

#define avg(a,b) (((a) + (b)) / 2)

#ifndef isspace
#define isspace(c) ((c) == ' ' || (c) == '\n' || (c) == '\t' || (c) == '\r')
#endif

#define lowercase(c) (isupper(c) ? tolower(c) : (c))

#define uppercase(c) (islower(c) ? toupper(c) : (c))

/* This tests a string to see if it has anything in it. */

#define empty_string(s) ((s) == NULL || s[0] == '\0')

extern char spbuf[];
extern char tmpbuf[];

#ifdef DEBUGGING

/* Debugging definitions. */

#define Dprintf if (Debug && dfp) debug_printf
#define DMprintf if (DebugM && dmfp) debugm_printf
#define DGprintf if (DebugG && dgfp) debugg_printf

#define Dprintlisp(X) if (Debug && dfp) fprintlisp(dfp, (X))
#define DMprintlisp(X) if (DebugM && dmfp) fprintlisp(dmfp, (X))
#define DGprintlisp(X) if (DebugG && dgfp) fprintlisp(dgfp, (X))

/* If the debug flags are not macros, then declare them as globals. */

#ifndef Debug
extern int Debug;
#endif
#ifndef DebugM
extern int DebugM;
#endif
#ifndef DebugG
extern int DebugG;
#endif

extern FILE *dfp;
extern FILE *dmfp;
extern FILE *dgfp;

#else /* DEBUGGING */

/* Make defns and calls vanish if possible. */

#define Dprintf if (0) debug_printf
#define DMprintf if (0) debugm_printf
#define DGprintf if (0) debugg_printf

#define Dprintlisp(X)
#define DMprintlisp(X)
#define DGprintlisp(X)

#define Debug (0)
#define DebugM (0)
#define DebugG (0)

#define dfp stdout
#define dmfp stdout
#define dgfp stdout

#endif /* DEBUGGING */

typedef struct a_library_path {
    char *path;
    struct a_library_path *next;
} LibraryPath;

#define for_all_library_paths(p)  \
  for (p = xconq_libs; p != NULL; p = p->next)

extern LibraryPath *xconq_libs;
extern LibraryPath *last_user_xconq_lib;

extern char *getenv();

extern void init_xrandom PARAMS ((int seed));
extern int xrandom PARAMS ((int m));
extern int probability PARAMS ((int prob));
extern int roll_dice PARAMS ((int n));
extern int multiply_dice PARAMS ((int dice, int mult));
extern int prob_fraction PARAMS ((int n));

extern char *xmalloc PARAMS ((int amt));
extern void report_malloc PARAMS ((void));
extern void tprintf PARAMS ((char *buf, char *str, ...));
extern void tnprintf PARAMS ((char *buf, int n, char *str, ...));
extern char *copy_string PARAMS ((char *str));
extern char *pad_blanks PARAMS ((char *str, int n));
extern int iindex PARAMS ((int ch, char *str));
extern long idifftime PARAMS ((time_t t1, time_t t0));
extern void case_panic PARAMS ((char *str, int var));
extern int isqrt PARAMS ((int i));
extern void init_debug_to_stdout PARAMS ((void));
extern void debug_printf PARAMS ((char *str, ...));
extern void debugm_printf PARAMS ((char *str, ...));
extern void debugg_printf PARAMS ((char *str, ...));

extern void vtprintf PARAMS ((char *buf, char *str, va_list ap));
