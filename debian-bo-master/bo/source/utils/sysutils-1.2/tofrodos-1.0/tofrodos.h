/*
	tofrodos.h	Converts text files between DOS and Unix formats.
	Copyright (c) 1996 by Christopher S L Heng. All rights reserved.

	$Id: tofrodos.h 1.2 1996/06/11 21:53:07 chris Exp $
*/

#if !defined(TOFRODOS_H_INCLUDED)
#define	TOFRODOS_H_INCLUDED

#if defined(__cplusplus)
extern "C" {
#endif

/* macros */
#define	UNIXTODOS	0	/* convert from Unix to DOS format */
#define	DOSTOUNIX	1	/* convert from DOS to Unix format */

#define	EXIT_ERROR	1	/* exit code on error */
#define	FROMDOSNAME	"fromdos"
#define	FROMDOSNAME2	"dos2unix"
#define	TODOSNAME	"todos"
#define	TODOSNAME2	"unix2dos"

/* conditional macros - depends on system and/or compiler */
#if defined(MSDOS)	/* MSDOS system */
#define	DEFDIRECTION	UNIXTODOS
#else			/* all systems other than DOS */
#define	DEFDIRECTION	DOSTOUNIX
#endif


/* global variables */
extern int abortonerr ; /* 1 = abort list of files if error in any */
extern int alwaysconvert ; /* convert all \r\n to \r\r\n when direction */
		/* is UNIXTODOS, and delete all \r when direction is */
		/* DOSTOUNIX */
extern int direction ; /* UNIXTODOS or DOSTOUNIX */
extern int forcewrite ; /* convert even if file is not writeable */
extern int overwrite ; /* 1 = overwrite (default), 0 = make backup */
extern char * progname ; /* name of binary */
extern int verbose ; /* 1 = be noisy, 0 = shut up */

/* function prototypes */
extern int init ( char * firstarg );
extern int parseargs ( int argc, char ** argv );
#if defined(__WATCOMC__)	/* sighandler() never returns */
#pragma aux sighandler aborts
#endif
extern void sighandler ( int sig );

#if defined(__cplusplus)
}
#endif

#endif
