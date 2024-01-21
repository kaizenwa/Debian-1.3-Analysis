/*
 * (c) Copyright 1992, 1993 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

#ifndef SS_KMP_H
#define SS_KMP_H

/*
 * $Id: ss_kmp.h,v 3.1 1993/06/13 02:44:52 panos Exp $
 */

typedef int next_int ;			/* must be signed */

struct kmp_header
{
	next_int *next ;
} ;

#endif	/* SS_KMP_H */

