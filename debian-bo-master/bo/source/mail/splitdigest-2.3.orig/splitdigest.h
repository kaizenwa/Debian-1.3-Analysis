/*
 * splitdigest.h
 * Copyright (c) 1994 by Christopher Heng. All rights reserved.
 * You many not remove or alter any of the copyright notices and/or the
 * conditions for use and distribution.
 * See the file COPYING for additional conditions for use and distribution.
 * COPYING contains the GNU General Public License version 2.
 *
 * Header file for splitdigest.c
 *
 * $Id: splitdigest.h,v 2.5 1995/07/08 10:04:45 chris Released $
 */

/* warning: you must include <stdio.h> before this file (it uses the	*/
/* FILE pointer)							*/

/* macros for the the configuration file and tables */
#if !defined(MAXCFTABLE)
#define MAXCFTABLE	128	/* should be more than enough */
#endif
#if !defined(DEFCONFIG)
#define DEFCONFIG	"/usr/local/lib/splitdigest.config"
#endif

/* macros for the compressor to use (obsolete - do not use) */
#if !defined(DEFCOMPRESS)
#define DEFCOMPRESS	"/usr/bin/gzip"
#endif
#if !defined(COMPRESSARG)
#define	COMPRESSARG	"-9"
#endif
#if !defined(DEFTOCOMPRESS)
#define DEFTOCOMPRESS	0
#endif

/* macros for undigestion or mere splitting of files (obsolete) */
#if !defined(DEFUNDIGESTOPT)
#define DEFUNDIGESTOPT 1
#endif

/* you should not need to modify the items below this */

/* macros for files */
#define TEMPFILELEN	16	/* length of buffer for temp file */
#define TEMPLATE	"./sdXXXXXX"
#define READMODE	"r"
#define WRITEMODE	"w"
#define MAXFILELEN	255	/* yeah, I know, should use the posix */
				/* functions and whatnot, but this IS */
				/* a quick and dirty program, you know. */

/* macro for line buffer */
#define MAXBUFFERLEN	1024	/* we can't cope with longer than this */
				/* (primitive!) */

/* usage message */
#define USAGESTR	"Usage: splitdigest [options] [file...]\n"\
			"Options include\n"\
			"\t-C <file>\tSpecify configuration file to use.\n"\
			"\t-c\t\tCompress output file (obsolete).\n"\
			"\t-h\t\tDisplay usage.\n"\
			"\t-l\t\tPreserve Content-Length header.\n"\
			"\t-o <outdir>\tSpecify output directory.\n"\
			"\t-s\t\tSeparate digests only. Don't undigest (obsolete).\n"\
			"\t-t\t\tDo not compress output file (default - obsolete).\n"\
			"\t-u\t\tUndigest while extracting digests (default -obsolete).\n"\
			"\t-v\t\tDisplay version number.\n"
#define SHUSAGESTR	"Usage: splitdigest [options] [file...]\n"\
			"Type \"splitdigest -h\" for more information.\n"

/* mail stuff */
#define	FROMFIELD	"From "

/* exit codes */
#define	EXIT_SIGINT	2

/* global variables */
extern char * pgmname ;
extern char * configfile ;
extern int compression ;
extern int undigest ;
extern char * infilename ;
extern int kill_contentlen ;	/* 1 = remove Content-Length header */

/* function declarations */
void realcleanup ( void );
void uncatfile ( FILE * file );
