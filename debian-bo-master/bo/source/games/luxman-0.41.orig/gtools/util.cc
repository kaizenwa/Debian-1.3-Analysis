/*
   util.cc

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

#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <pwd.h>
#include <sys/types.h>
#include <gtools/util.h>
#include <limits.h>

void do_fatal_error( char *file, int line, char *fmt, ... );
#define fatal( fmt, arg... ) do_fatal_error( __FILE__, __LINE__, fmt, ## arg )

zFile *zfopen( char *filename, char *mode )
{
  zFile *zfp;
  int len;
  char *p, *cmdbuf;
  
  zfp = new zFile;
  if ( !zfp )
	return NULL;

  len = strlen( filename );
  
  /* If not opening for reading, there is nothing else to do */
  if ( mode[0] != 'r' )
	{
	  zfp->fp = fopen( filename, mode );
	  
	  if ( !zfp->fp )
		{
		  delete zfp;
		  return NULL;
		}
	  
	  zfp->piped = 0;
	  return zfp;
	}

  /* See if file compressed */
  p = strrchr( filename, '.' );

  if ( p && !strcmp( p, ".gz" ) )
	{
	  /* command line: gzip -dc filename */
	  cmdbuf = (char*)alloca( 10 + len );
	  if ( !cmdbuf )
		{
		  delete zfp;
		  return NULL;
		}
	  
	  sprintf( cmdbuf, "gzip -dc %s", filename );

	  zfp->fp = popen( cmdbuf, mode );
	  if ( !zfp->fp )
		{
		  delete zfp;
		  return NULL;
		}
		  
	  zfp->piped = 1;
	}
  else	/* not compressed */
	{
	  zfp->fp = fopen( filename, mode );
	  if ( !zfp->fp )
		{
		  delete zfp;
		  return NULL;
		}
	  
	  zfp->piped = 0;
	}

  return zfp;
}

int zfclose( zFile *zfp )
{
  int r;
  
  if ( zfp->piped )
	{
	  r = pclose( zfp->fp );
	  delete zfp;
	}
  else
	{
	  r = fclose( zfp->fp );
	  delete zfp;
	}

  return r;
}
	  
static char *seps = ":; \t";

/* Non-static replacement for strtok */
char *str_next_piece( char **start, char *delim )
{
  char *s, *e;

  if ( !start || !*start )
	return NULL;

  /* Find start of next piece */
  s = *start;

  /* Skip delimiters */
  while( *s && strchr( delim, *s ) )
    ++s;

  if ( !*s )		
	{
	  /* No non-delimiters found */
	  *start = s;
	  return NULL;
	}
  
  e = s;

  /* Find end of piece */
  while( *e && !strchr( delim, *e ) )
    ++e;

  if ( !*e )
    {
	  /* End of piece == End of string */
      *start = NULL;
      return s;
    }	
  else
    {
	  /* End of piece != End of string */
      *start = e+1;
      *e = 0;
      return s;
    }
}

static int
try_exts( char **dest, char *pathname, char *exts )
{
  char *buf;
  char *ext;
  int plen;
  char *i_exts;

  /* Try without any extension */
  if ( access( pathname, 0 ) == 0 )
    {
      *dest = strdup( pathname );
	  if ( !*dest )
		fatal( "Out of memory" );
	  
      return 1;
    }

  if ( !exts )
    return 0;

  /* Try with extensions */

  /* Make copy of exts since str_next_piece trashes the string */
  i_exts = (char*)alloca( strlen( exts ) + 1 );
  if ( !i_exts )
	fatal( "Out of memory" );
  
  strcpy( i_exts, exts );

  ext = str_next_piece( &i_exts, seps );

  plen = strlen( pathname );

  while( ext )
    {
      /*            path + . + ext + NULL */
      buf = (char*)alloca( plen + 1 + strlen( ext ) + 1 );

      if ( !buf )
		fatal( "Out of memory" );

      strcpy( buf, pathname );
      buf[plen] = '.';
      strcpy( buf+plen+1, ext );

#ifdef DEBUG	 	  	  
      printf("\tTrying:%s:\n", buf );
#endif	  
	  
      if ( access( buf, 0 ) == 0 )
		{
		  *dest = strdup( buf );
		  if ( !*dest )
			fatal( "Out of memory" );
		  
		  return 1;
		}

      ext = str_next_piece( &i_exts, seps );
    }	

  return 0;
}

/* Resolves path to absolute name if path is of form:

   ~user/...
   or
   ~/...
*/
char *resolve_tilde( char *dest, char *path )
{
  char *homedir;
  int lhome;
  char *start;
  struct passwd *pwd;
  
  if ( path[0] != '~' )
	{
	  strcpy( dest, path );
	  return dest;
	}

  /* Of form ~/ ? */
  if ( path[1] == '/' )
	{
	  start = path + 1;
	  pwd = getpwuid( getuid() );
	  if ( !pwd )
		return NULL;
	}
  else	/* Of form ~user/ */
	{
	  start = strchr( path, '/' );
	  if ( !start )
		return NULL;
	  
	  *start = 0;
	  
	  pwd = getpwnam( path + 1 );
	  if ( !pwd )
   		return NULL;
	  
	  *start = '/';
	}

  homedir = strdup( pwd->pw_dir );
  if ( !homedir )
	return NULL;
  
  lhome = strlen( homedir );
  if ( homedir[lhome-1] == '/' )
	homedir[--lhome] = 0;

  strcpy( dest, homedir );
  strcat( dest, start );

  free( homedir );
  
  return dest;
}

int find_file( char **dest, char *basename, char *paths, char *exts )
{
  char *path, rpath[NAME_MAX+PATH_MAX+1];
  char bname[NAME_MAX+PATH_MAX+1];
  int plen, len;
  char *newpath;
  char *i_paths;

  if ( !strlen( basename ) )
	return 0;

  /* Try basename */
  if ( resolve_tilde( bname, basename ) == NULL )
	return 0;
  
  if ( access( bname, 0 ) == 0 )
    {
      *dest = strdup( bname );
	  if ( !*dest )
		fatal( "Out of memory" );
	  
      return 1;
    }

  if ( !paths )
    return 0;

  /* Make copy of paths since str_next_piece trashes string */
  i_paths = (char*)alloca( strlen( paths ) + 1 );
  if ( !i_paths )
	fatal( "Out of memory" );
  
  strcpy( i_paths, paths );

  path = str_next_piece( &i_paths, seps );

  len = strlen( bname );

  while( path )
    {
	  if ( resolve_tilde( rpath, path ) == NULL )
		return 0;
	  
      plen = strlen( rpath );

	  if ( rpath[plen-1] == '/' )
		rpath[--plen] = 0;

	  newpath = (char*)alloca( plen + 1 + len + 1 );
	  if ( !newpath )
			fatal( "Out of memory" );

	  strcpy( newpath, rpath );
	  newpath[plen] = '/';
	  strcpy( newpath + plen + 1, bname );
			 
#ifdef DEBUG	  
      printf("Trying:%s:\n", newpath );
#endif	  

      if ( try_exts( dest, newpath, exts ) == 1 )
		return 1;

      path = str_next_piece( &i_paths, seps );
    }

  return 0;
}

	 
  
  

	  
