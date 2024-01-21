/* tailf.c -- tail a log file and then follow it
 * Created: Tue Jan  9 15:49:21 1996 by r.faith@ieee.org
 * Revised: Thu Jan 11 16:47:20 1996 by r.faith@ieee.org
 * Copyright 1996 Rickard E. Faith (r.faith@ieee.org)
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any
 * later version.
 * 
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 675 Mass Ave, Cambridge, MA 02139, USA.
 * 
 * $Id: tailf.c,v 1.1 1996/01/11 21:51:07 faith Exp $
 *
 * less -F and tail -f cause a disk access every five seconds.  This
 * program avoids this problem by waiting for the file size to change.
 * Hence, the file is not accessed, and the access time does not need to be
 * flushed back to disk.  This is sort of a "stealth" tail.
 */

#include <stdio.h>
#include <unistd.h>
#include <malloc.h>
#include <sys/stat.h>

size_t filesize( const char *filename )
{
   struct stat sb;

   if (!stat( filename, &sb )) return sb.st_size;
   return 0;
}

void tailf( const char *filename, int lines )
{
   char **buffer;
   int  head = 0;
   int  tail = 0;
   FILE *str;
   int  i;

   if (!(str = fopen( filename, "r" ))) {
      fprintf( stderr, "Cannot open \"%s\" for read\n", filename );
      perror( "" );
      exit( 1 );
   }

   buffer = malloc( lines * sizeof( char * ) );
   for (i = 0; i < lines; i++) buffer[i] = malloc( BUFSIZ + 1 );

   while (fgets( buffer[tail], BUFSIZ, str )) {
      if (++tail >= lines) {
	 tail = 0;
	 head = 1;
      }
   }

   if (head) {
      for (i = tail; i < lines; i++) fputs( buffer[i], stdout );
      for (i = 0; i < tail; i++)         fputs( buffer[i], stdout );
   } else {
      for (i = head; i < tail; i++) fputs( buffer[i], stdout );
   }
   fflush( stdout );

   for (i = 0; i < lines; i++) free( buffer[i] );
   free( buffer );
}

int main( int argc, char **argv )
{
   char        buffer[BUFSIZ];
   size_t      osize, nsize;
   FILE        *str;
   const char  *filename;
   int         count;
   
   if (argc != 2) {
      fprintf( stderr, "Usage: tailf logfile\n" );
      exit( 1 );
   }

   filename = argv[1];
   
   tailf( filename, 10 );
   
   for (osize = filesize( filename );;) {
      nsize = filesize( filename );
      if (nsize != osize) {
	 if (!(str = fopen( filename, "r" ))) {
	    fprintf( stderr, "Cannot open \"%s\" for read\n", filename );
	    perror( argv[0] );
	    exit( 1 );
	 }
	 fseek( str, osize, SEEK_SET );
	 while ((count = fread( buffer, 1, sizeof( buffer ), str )) > 0)
	    fwrite( buffer, 1, count, stdout );
	 fflush( stdout );
	 fclose( str );
	 osize = nsize;
      }
      usleep( 250000 );		/* 250mS */
   }
   return 0;
}
