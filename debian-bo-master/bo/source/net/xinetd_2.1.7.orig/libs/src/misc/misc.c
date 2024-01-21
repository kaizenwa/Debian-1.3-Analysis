/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

static char RCSid[] = "$Id: misc.c,v 1.6 1995/09/10 18:32:56 chuck Exp $" ;
static char misc_version[] = VERSION ;

#include <varargs.h>
#include <sys/param.h>
#if defined(linux) || defined(BSD)
#include <stdlib.h>
#endif

/*
 * MISCELLANEOUS FUNCTIONS
 */

#include "misc.h"

#ifndef NULL
#define NULL		0
#endif

char *strncpy() ;
char *strrchr() ;


/*
 * Create a new argv array,
 * copy the original to the new one,
 * and clear the old one
 */
char **argv_copy_and_clear( org_argv, start, count )
	char **org_argv ;							/* original argv */
	int start ;									/* from where to start copy/clear */
	int count ;									/* how many entries to copy/clear */
{
	char **new_argv ;
	char *p ;
	int i ;
	int j ;
#if !defined(linux) && !defined(BSD)
	char *malloc() ;
#endif

	new_argv = (char **) malloc( count * sizeof( char * ) ) ;
	if ( new_argv == NULL )
		return( NULL ) ;

	for ( i = 0 ; i < count ; i++ )
	{
		new_argv[ i ] = make_string( 1, org_argv[ start+i ] ) ;
		if ( new_argv[ i ] == NULL )
		{
			for ( j = i-1 ; j >= 0 ; j-- )
				free( new_argv[ j ] ) ;
			free( (char *) new_argv ) ;
			return( NULL ) ;
		}
		for ( p = org_argv[ start+i ] ; *p ; p++ )
			*p = ' ' ;
	}
	return( new_argv ) ;
}


/*
 * We always return a pointer in pathname
 */
char *basename( pathname )
	char *pathname ;
{
	char *s = strrchr( pathname, '/' ) ;

	if ( s == NULL )
		return( pathname ) ;
	else
		return( s+1 ) ;
}


/*
 * We always return a malloced string
 *
 * There are 2 special cases:
 *
 *		1) pathname == "/"
 *				In this case we return "/"
 *		2) pathname does not contain a '/'
 *				In this case we return "."
 */
char *dirname( pathname )
	char *pathname ;
{
	int len ;
	char *s = strrchr( pathname, '/' ) ;
	char *p ;
#if !defined(linux) && !defined(BSD)
	char *malloc() ;
#endif

	if ( s == NULL )
		return( make_string( 1, "." ) ) ;
	else
	{
		len = s - pathname ;
		if ( len == 0 )
			return( make_string( 1, "/" ) ) ;
	}

	p = malloc( len+1 ) ;
	if ( p == NULL )
		return( NULL ) ;
	else
	{
		strncpy( p, pathname, len )[ len ] = '\0' ;
		return( p ) ;
	}
}


char *make_string( count, va_alist )
	register unsigned count ;
	va_dcl
{
	va_list ap ;
	register unsigned i ;
	register unsigned len = 0 ;
	register char *s, *p ;
	char *new_string ;

	if ( count == 0 )
		return( NULL ) ;

	va_start( ap ) ;
	for ( i = 0 ; i < count ; i++ )
	{
		s = va_arg( ap, char * ) ;
		if ( s == NULL )
			continue ;
		len += strlen( s ) ;
	}
	va_end( ap ) ;

	new_string = malloc( len + 1 ) ;
	if ( new_string == NULL )
		return( NULL ) ;

	p = new_string ;
	va_start( ap ) ;
	for ( i = 0 ; i < count ; i++ )
	{
		s = va_arg( ap, char * ) ;
		if ( s == NULL )
			continue ;
		while ( *p++ = *s++ ) ;
		p-- ;
	}
	va_end( ap ) ;
	return( new_string ) ;
}


char *make_pathname( count, va_alist )
	register unsigned count ;
	va_dcl
{
	va_list ap ;
	register unsigned i ;
	register unsigned len = 0 ;
	register char *s, *p ;
	char *pathname ;

	if ( count == 0 )
		return( NULL ) ;

	va_start( ap ) ;
	for ( i = 0 ; i < count ; i++ )
	{
		s = va_arg( ap, char * ) ;
		len += strlen( s ) ;
	}
	va_end( ap ) ;

	pathname = malloc( len + count ) ;
	if ( pathname == NULL )
		return( NULL ) ;
	
	p = pathname ;
	va_start( ap ) ;
	for ( i = 0 ; i < count ; i++ )
	{
		s = va_arg( ap, char * ) ;
		while ( *p++ = *s++ ) ;
		*(p-1) = '/' ;			/* change '\0' to '/' */
	}
	*(p-1) = '\0' ;
	return( pathname ) ;
}

