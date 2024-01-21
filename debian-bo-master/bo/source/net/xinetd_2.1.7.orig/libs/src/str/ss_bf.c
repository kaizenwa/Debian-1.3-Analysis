/*
 * (c) Copyright 1992, 1993 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

static char RCSid[] = "$Id" ;

#include "ss_impl.h"

PRIVATE int bf_setup() ;
PRIVATE char *bf_match() ;
PRIVATE void bf_done() ;

struct ss_ops __strs_bfops = { bf_setup, bf_match, bf_done } ;


PRIVATE int bf_setup( hp )
	header_s *hp ;
{
#ifdef lint
	hp = hp ;
#endif
	return( SS_OK ) ;
}


PRIVATE char *bf_match( hp, str, len )
	header_s		*hp ;
	char			*str ;
	int			len ;
{
	register int	pfc		= SS_PATTERN( hp )[ 0 ] ;	/* pattern first char */
	register char	*pos		= str ;
	register char	*endpos	= &str[ len - SS_PATLEN( hp ) + 1 ] ;
	char				*endpat	= &SS_PATTERN( hp )[ SS_PATLEN( hp ) ] ;

	while ( pos < endpos )
	{
		register int strc = SS_MAP( hp, *pos ) ;
		register char *pp ;									/* pattern pointer */
		register char *sp ;									/* string pointer */

		pos++ ;
		if ( strc != pfc )
			continue ;

		for ( pp = &SS_PATTERN( hp )[ 1 ], sp = pos ;; sp++, pp++ )
		{
			if ( pp == endpat )			/* we are guaranteed a non-empty pattern */
				return( pos-1 ) ;
			if ( *pp != SS_MAP( hp, *sp ) )
				break ;
		}
	}
	return( CHAR_NULL ) ;
}


PRIVATE void bf_done( hp )
	header_s *hp ;
{
#ifdef lint
	hp = hp ;
#endif
}

