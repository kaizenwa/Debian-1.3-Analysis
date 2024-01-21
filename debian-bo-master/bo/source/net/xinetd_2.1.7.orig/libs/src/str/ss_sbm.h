/*
 * (c) Copyright 1992, 1993 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */


#ifndef SS_SBM_H
#define SS_SBM_H

/*
 * $Id: ss_sbm.h,v 3.1 1993/06/13 02:46:00 panos Exp $
 */

typedef int last_int ;			/* must be signed */

struct sbm_header
{
	last_int *last_occurrence ;
} ;

#endif	/* SS_SBM_H */

