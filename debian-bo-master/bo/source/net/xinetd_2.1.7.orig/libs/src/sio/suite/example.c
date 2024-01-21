/*
 * (c) Copyright 1992, 1993 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

static char RCSid[] = "$Id: example.c,v 8.1 1993/03/13 01:21:48 panos Exp $" ;

#include "sio.h"

main( argc, argv )
   int argc ;
   char *argv[] ;
{
   char *file = (argc > 1) ? argv[ 1 ] : "tee.file" ;
   int fd = creat( file, 0644 ) ;
   long length ;
   char *s ;

   while ( s = Sfetch( 0, &length ) )
   {
      Swrite( 1, s, length ) ;
      Swrite( fd, s, length ) ;
   }
   exit( 0 ) ;
}

