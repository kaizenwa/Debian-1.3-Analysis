/*
 *	xtrojka (c) 1994,1995,1996 Maarten Los
 *
 *	#include "COPYRIGHT"	
 *
 *	created:	26.xi.1995
 *	modified:
 *
 *	This module is used for displaying debug information
 */

#ifndef _debug_h_
#define _debug_h_


#ifdef DEBUG_INFO

#include <stdio.h>
#include "xtrojka.h"

extern flag is_debug_info;

#define DEBUG(s1,s2)	if(is_debug_info) { \
			fprintf(stderr,"DEBUG: %s: %s\n", s1, s2); \
			fflush(stderr); }

#else

#define DEBUG(s1,s2)	;

#endif /* DEBUG_INFO */

#endif /* _debug_h_ */
