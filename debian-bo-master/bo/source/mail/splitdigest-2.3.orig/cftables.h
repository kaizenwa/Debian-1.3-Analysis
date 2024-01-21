/*
 * cftables.h	- header for the matching routines.
 * Copyright (c) 1994 by Christopher Heng. All rights reserved.
 * You may not remove or alter the copyright notice and/or the conditions for
 * use and distribution. Additional terms and conditions
 * for use and distribution are included in the file COPYING, which contains
 * the GNU General Public License version 2.
 *
 * $Id: cftables.h,v 2.3 1995/07/08 10:04:45 chris Released $
 */

/* maximum num of conversion specifiers for calling sscanf() */
/* WARNING: This is just for documentation only. The code in isheader() */
/* assumes the value is 6. If you change this, you must change isheader() */
#define MAXSCANFARGS	6

/* macro for the scanf arg */
#if !defined(MAXSCANFBUF)
#define MAXSCANFBUF	512
#endif

/* macro for the header and message separator characters */
/* redefine this in the Makefile if you have to, not here */
#if !defined(SEPCHAR)
#define SEPCHAR		'-'
#endif

/* structure for the strings to compare */
typedef struct cftable_struc {
	char *	headerstring ;
	char *	endstring ;
	char *	fnpattern ;
	int	numspec ;
	int	numhdrsep ;
	int	nummsgsep ;
} cftable_t ;

/* union for the args to pass sscanf() sprintf() */
typedef union arg_union {
	int iptr ;
	char sptr[MAXSCANFBUF] ; 
} arg_t ;

/* function prototypes */
int inittables ( void );
int isheader ( char * s, char * fname, char * endstring, cftable_t ** cf );
int isend ( char * s, char * endstring );
int ishdrsep ( char * s, cftable_t * cf );
int ismsgsep ( char * s, cftable_t * cf );

