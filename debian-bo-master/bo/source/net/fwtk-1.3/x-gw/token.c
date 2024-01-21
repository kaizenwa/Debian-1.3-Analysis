/*-
 * Copyright (c) 1993, Trusted Information Systems, Incorporated
 * All rights reserved.
 *
 * Redistribution and use are governed by the terms detailed in the
 * license document ("LICENSE") included with the toolkit.
 */

/*
 *	Author: Wei Xu, Trusted Information Systems, Inc.
 */
static	char	RcsId[] = "Header: token.c,v 1.2 94/07/14 19:03:38 wei Locked ";

#include        "ulib.h"

extern  char    *strsave();
extern  char   **addstrlist();
extern  void     clearstrlist();

 /*********************************************************************
  * return the point of the beginning of the next token.
  *********************************************************************/
char * next_token( s,seperator )
char * s;
char * seperator; /* can be ' ', ':', ... */
{
    while( *s == '\t' || *s == *seperator ) { s++; }
    return( s );
}

 /*********************************************************************
  * return the point of the last string +1 of the token.
  *********************************************************************/
char * end_token( s,seperator )
char * s;
char * seperator; /* can be ' ', ':', ... */
{
     while( *s != '\t' && *s != *seperator && *s != '\0' ) { s++; }
     return( s );
}

/**********************************************************************
 * extract the nth token from input string 
 * return: a) the function returns the arry of the token (to be freed).
 *         b) return n: the total element of the token arry.
 **********************************************************************/
char **tokens( string,seperator,n )
char * string;    /* the input string */
char * seperator; /* can be ' ', ':', ... */
int  * n;         /* the total element */
{
    int   cnt=0;
    char *bgn, *end, **list=(char**)0;
    char  buf[BUFSIZ];

    if( !string || !*string ) return list;

    bgn=end=string;
    while( *bgn && *end ) {
          bgn = next_token( end,seperator );
          end = end_token ( bgn,seperator );
    
	  cnt = end-bgn;
	  list = addstrlist( list, bgn, &cnt );
    }
    if( n ) *n = cnt;
 
    return list;
}
