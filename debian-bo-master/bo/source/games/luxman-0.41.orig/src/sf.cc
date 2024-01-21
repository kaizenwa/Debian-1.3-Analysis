/*
   sf.cc

   This file is part of LuxMan.
   
   Copyright (C) 1995 Frank McIngvale (frankm@nuance.com)
   
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

#include <gtools/util.h>
#include <string.h>
#include <malloc.h>
#include "sf.h"

/* Field seperator(s) */
static char *FS = "|";

/* Returns 1 if buf is a comment or blank line */
static int is_comment_line( char *buf )
{
  /* Skip leading whitespace */
  while( *buf && strchr( " \t\r\n", *buf ) )
	++buf;

  if ( !*buf || *buf == '#' )
	return 1;
  else
	return 0;
}

static int parse_line( VLList *names, VLList *files, VLList *dirs, char *line )
{
  char *p, *s;
  
  if ( (p = str_next_piece( &line, FS )) == NULL )
	return -2;

  s = strdup( p );
  if ( !s )
	return -1;

  names->add_tail( s );
  
  if ( (p = str_next_piece( &line, FS )) == NULL )
	return -1;

  s = strdup( p );
  if ( !s )
	return -1;

  files->add_tail( s );

  if ( (p = str_next_piece( &line, FS )) == NULL )
	return -1;

  s = strdup( p );
  if ( !s )
	return -1;

  dirs->add_tail( s );
  
  return 0;
}

Scenarios::Scenarios( FILE *fp )
{
  char buf[200];
  int r;
  
  names = new VLList();
  files = new VLList();
  dirs = new VLList();
  
  if ( !names || !files || !dirs )
	{
	  error = -1;
	  return;
	}

  while( fgets( buf, 200, fp ) != NULL )
	{
	  if ( is_comment_line( buf ) )
		continue;

	  if ( (r=parse_line( names, files, dirs, buf )) != 0 )
		{
		  error = r;
		  return;
		}
	}

  error = 0;
}

Scenarios::~Scenarios()
{
  char *s;

  while( (s=(char*)names->pop_head()) )
	free( s );

  while( (s=(char*)files->pop_head()) )
	free( s );

  while( (s=(char*)dirs->pop_head()) )
	free( s );
  
  delete names;
  delete files;
  delete dirs;
}
  
