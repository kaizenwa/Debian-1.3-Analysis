/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */


#ifndef __MISC_H
#define __MISC_H


/*
 * $Id: misc.h,v 2.1 1992/10/01 00:41:34 panos Exp $
 */

/*
 * Align to a power of 2
 */
#define align2( num, al )					(((num)+(al)-1) & ~((al)-1))

#ifdef __ARGS
#undef __ARGS
#endif

#ifdef PROTOTYPES
#   define __ARGS( s )               s
#else
#   define __ARGS( s )               ()
#endif


char *make_string __ARGS( ( unsigned count, ... ) ) ;
char *make_pathname __ARGS( ( unsigned count, ... ) ) ;

char **argv_copy_and_clear __ARGS( ( char **argv, int start, int count ) ) ;
char *dirname __ARGS( ( char *pathname ) ) ;
char *basename __ARGS( ( char *pathname ) ) ;

#endif 	/* __MISC_H */

