/*
 * (c) Copyright 1992, 1993 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

static char RCSid[] = "$Id: strprint.c,v 3.1 1993/06/13 02:50:11 panos Exp $" ;

#ifndef NO_SIO
#include "sio.h"
#endif

#include "str.h"

#define INT_NULL						((int *)0)

/*
 * The strx_* functions will never overwrite the buffer
 * The str_* functions may overwrite the buffer
 */

/*
 * Group 1: the strx_* functions
 */

/*
 * This is the general purpose conversion function. It is invoked
 * by all the other str[x]_* functions
 */
void strx_printv( ccp, buf, len, format, ap )
	int *ccp ;
	char *buf ;
	int len ;
	char *format ;
	va_list ap ;
{
#ifndef NO_SIO
	__sio_od_t od ;
	int cc ;

   /*
    * First initialize the descriptor
	 * Notice that if no length is given, we initialize buf_end to the
	 * highest possible address.
    */
   od.buf = buf ;                						/* NOT NEEDED        */
   od.buf_end = len ? &buf[ len ] : (char *) ~0 ;	/* NEEDED				*/
   od.buffer_size = 0 ;          						/* NOT NEEDED        */
   od.start = buf ;              						/* NOT NEEDED        */
   od.nextb = buf ;              						/* NEEDED            */
   od.buftype = 0 ;              						/* NOT NEEDED        */

   /*
    * Do the conversion
    */
   cc = __sio_converter( &od, -1, format, ap ) ;
	if ( len == 0 || od.nextb < od.buf_end )
		*(od.nextb) = '\0' ;
   if ( ccp )
      *ccp = cc ;
#endif	/* ! NO_SIO */
}


void strx_print( ccp, buf, len, format, va_alist )
	int *ccp ;
	char *buf ;
	int len ;
	char *format ;
	va_dcl
{
	va_list ap ;

	va_start( ap ) ;
	strx_printv( ccp, buf, len, format, ap ) ;
	va_end( ap ) ;
}


char *strx_sprint( buf, len, format, va_alist )
	char *buf ;
	int len ;
	char *format ;
	va_dcl
{
	va_list ap ;

	va_start( ap ) ;
	strx_printv( INT_NULL, buf, len, format, ap ) ;
	va_end( ap ) ;
	return( buf ) ;
}


char *strx_sprintv( buf, len, format, ap )
	char *buf ;
	int len ;
	char *format ;
	va_list ap ;
{
	strx_printv( INT_NULL, buf, len, format, ap ) ;
	return( buf ) ;
}


int strx_nprint( buf, len, format, va_alist )
	char *buf ;
	int len ;
	char *format ;
	va_dcl
{
	int cc ;
	va_list ap ;

	va_start( ap ) ;
	strx_printv( &cc, buf, len, format, ap ) ;
	va_end( ap ) ;
	return( cc ) ;
}


int strx_nprintv( buf, len, format, ap )
	char *buf ;
	int len ;
	char *format ;
	va_list ap ;
{
	int cc ;

	strx_printv( &cc, buf, len, format, ap ) ;
	return( cc ) ;
}



/*
 * Group 2: the str_* functions
 */

void str_print( ccp, buf, format, va_alist )
	int *ccp ;
	char *buf ;
	char *format ;
	va_dcl
{
	va_list ap ;

	va_start( ap ) ;
	strx_printv( ccp, buf, 0, format, ap ) ;
	va_end( ap ) ;
}


void str_printv( ccp, buf, format, ap )
	int *ccp ;
	char *buf ;
	char *format ;
	va_list ap ;
{
	strx_printv( ccp, buf, 0, format, ap ) ;
}


char *str_sprint( buf, format, va_alist )
	char *buf ;
	char *format ;
	va_dcl
{
	va_list ap ;

	va_start( ap ) ;
	strx_printv( INT_NULL, buf, 0, format, ap ) ;
	va_end( ap ) ;
	return( buf ) ;
}


char *str_sprintv( buf, format, ap )
	char *buf ;
	char *format ;
	va_list ap ;
{
	strx_printv( INT_NULL, buf, 0, format, ap ) ;
	return( buf ) ;
}


int str_nprint( buf, format, va_alist )
	char *buf ;
	char *format ;
	va_dcl
{
	int cc ;
	va_list ap ;

	va_start( ap ) ;
	strx_printv( &cc, buf, 0, format, ap ) ;
	va_end( ap ) ;
	return( cc ) ;
}



int str_nprintv( buf, format, ap )
	char *buf ;
	char *format ;
	va_list ap ;
{
	int cc ;

	strx_printv( &cc, buf, 0, format, ap ) ;
	return( cc ) ;
}


