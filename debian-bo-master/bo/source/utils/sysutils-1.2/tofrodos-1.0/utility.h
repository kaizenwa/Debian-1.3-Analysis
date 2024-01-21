/*
	utility.h	Utility functions.
	Copyright (c) 1996 by Christopher S L Heng. All rights reserved.

	$Id: utility.h 1.1 1996/05/17 21:47:29 chris Exp $
*/

#if !defined(UTILITY_H_INCLUDED)
#define	UTILITY_H_INCLUDED

#if defined(__cplusplus)
extern "C" {
#endif

/* function declarations */
#if defined(__WATCOMC__)	/* errnomem() never returns */
#pragma aux errnomem aborts
#endif
extern void errnomem ( int exitcode );
extern void * xmalloc ( size_t len );
extern char * xstrdup( const char * s );

#if defined(__cplusplus)
}
#endif


#endif
