/*
 * Copyright (c) 1990,1991 Regents of The University of Michigan.
 * All Rights Reserved.
 *
 * Permission to use, copy, modify, and distribute this software and
 * its documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appears in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation, and that the name of The University
 * of Michigan not be used in advertising or publicity pertaining to
 * distribution of the software without specific, written prior
 * permission. This software is supplied as is without expressed or
 * implied warranties of any kind.
 *
 *	Research Systems Unix Group
 *	The University of Michigan
 *	c/o Mike Clark
 *	535 W. William Street
 *	Ann Arbor, Michigan
 *	+1-313-763-0525
 *	netatalk@itd.umich.edu
 */

#include  <sys/types.h>
#include  <sys/socket.h>
#include  <netatalk/at.h>
#include  <stdio.h>
#include  <strings.h>

Usage( av0 )
    char	*av0;
{
    char	*p;

    if (( p = rindex( av0, '/' )) == 0 ) {
	p = av0;
    } else {
	p++;
    }

    fprintf( stderr, "Usage: %s obj:type@zone\n", p );
    exit( 1 );
}

main( ac, av )
    int		ac;
    char	**av;
{
    char		*Obj = 0, *Type = 0, *Zone = 0;

    if ( ac != 2 ) {
	Usage( av[ 0 ] );
    }

    /*
     * Get the name. If Type or Obj aren't specified, error.
     */
    if ( nbp_name( av[ 1 ], &Obj, &Type, &Zone ) || !Obj || !Type ) {
	Usage( av[ 0 ] );
    }

    if ( nbp_unrgstr( Obj, Type, Zone ) < 0 ) {
	fprintf( stderr, "Can't unregister %s:%s@%s\n", Obj, Type,
		Zone ? Zone : "*" );
	exit( 1 );
    }
}
