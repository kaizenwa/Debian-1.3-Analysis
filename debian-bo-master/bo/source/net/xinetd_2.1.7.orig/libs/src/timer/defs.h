/*
 * (c) Copyright 1993 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */


/*
 * $Id: defs.h,v 3.1 1992/11/23 16:22:01 panos Exp $
 */

#define PRIVATE					static

#ifndef NULL
#define NULL						0
#endif

#ifndef FALSE
#define FALSE						0
#define TRUE						1
#endif

#define TIMER_NULL				((timer_s *)0)
#define SIGVEC_NULL				((struct sigvec *)0)
#define SIGACTION_NULL			((struct sigaction *)0)

#define HANDLE_ERROR( flags, retval, errp, errval, msg )							\
										if ( flags & TIMER_RETURN_ERROR )            \
										{                                            \
											*errp = errval ;                          \
											return( retval ) ;                        \
										}                                            \
										else                                         \
										{                                            \
											char *s = msg ;                           \
																									\
											(void) write( 2, s, strlen( s ) ) ;       \
											abort() ;                                 \
											_exit( 1 ) ;                              \
											/* NOTREACHED */                          \
										}

