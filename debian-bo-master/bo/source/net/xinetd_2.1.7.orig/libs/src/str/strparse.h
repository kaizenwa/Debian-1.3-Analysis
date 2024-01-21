/*
 * (c) Copyright 1992, 1993 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */


/*
 * $Id: strparse.h,v 3.1 1993/06/13 02:48:35 panos Exp $
 */

struct str_handle
{
   char *string ;
   char *separator ;
   char *pos ;
   int flags ;
   int *errnop ;
   int no_more ;
} ;

int str_errno ;

#ifndef NULL
#define NULL         0
#endif

#ifndef FALSE
#define FALSE        0
#define TRUE         1
#endif

#define PRIVATE		static

#define TERMINATE( msg )   {                                         \
                              char *s = msg ;                        \
                                                                     \
                              (void) write( 2, s, strlen( s ) ) ;    \
                              (void) abort() ;                       \
                              _exit( 1 ) ;                           \
                              /* NOTREACHED */                       \
                           }


#define HANDLE_ERROR( flags, retval, errp, errval, msg )    \
            if ( flags & STR_RETURN_ERROR )                 \
            {                                               \
               *errp = errval ;                             \
               return( retval ) ;                           \
            }                                               \
            else                                            \
               TERMINATE( msg )


