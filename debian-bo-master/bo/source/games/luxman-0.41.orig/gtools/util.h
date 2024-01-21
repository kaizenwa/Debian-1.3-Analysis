/*
   util.h

   This file is part of libgtools.
   
   Copyright (C) 1994,1995 Frank McIngvale (frankm@nuance.com)
   
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#ifndef _util_h_
#define _util_h_

#include <stdio.h>

struct zFile {
  FILE *fp;
  int piped;
};

/*
   zfopen
   zfclose

   Light wrappers around fopen and fclose to support .gz files.
   If file is compressed, data is piped in from gzip -dc.
   If file not compressed, file opened as usual.
*/   
zFile *zfopen( char *filename, char *mode );
int zfclose( zFile *zfp );

/*
   char *str_next_piece( char **start, char *delim )

   Non-static replacement for strtok().

   `delim' is a string of all possible delimiter characters.
   `*start' is where to start scanning.

   Returns:
		On success - Pointer to next piece
		On failure (no more tokens) - Returns NULL

   `*start' is updated so that the next call will return the
   next piece, etc.

   Note: This routine steps on the string it is scanning (replacing
         delimiters will NULLs).
*/   
char *str_next_piece( char **start, char *delim );

/*
   char *resolve_tilde( char *dest, char *path )

   Copies `path' to `dest', substituting an actual directory
   name for a ~ form.

   These forms are handled:

   ~/...
   ~user/...

   The ~ must be the first char in `path'.
   
   `dest' must already be allocated.
*/
char *resolve_tilde( char *dest, char *path );

/*
   int find_file( char **dest, char *basename, char *paths, char *exts )

   Tries to find a file in one of several directories,
   using one of several extensions.

   `paths' is a colon-delimited list of directories to
   search. `exts' is a colon-delimited list of file extensions
   to try; these should be given WITHOUT leading .'s.
   `basename' is the filename that will be qualified with `paths'
   and `exts'.

   On success (file found), `dest' is allocated and the string
   is copied to it and 1 is returned.

   On failure, 0 returned.

   `paths' and/or `exts' may be NULL.
*/

int find_file( char **dest, char *basename, char *paths, char *exts );

#endif



