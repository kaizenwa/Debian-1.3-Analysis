/*
   lf.cc

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

/* `Level' file routines */

/*
   A `Level' is a bracketed section in a file:
   
{
	maze =
	tile = 
	fruit =
	depth =
    regen_wait = 
	fruit_val =
	fruit_quota =
    fruit_off =
	fruit_on = 
	bg =
	dot =
	bigdot =
	ghost_blue =
	ghost_flash =
    lux_body =
	glasses =
}
*/

#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <gtools/screen.h>
#include "lf.h"

#define LTRIM( s ) while( *s && strchr( " \t\b", *s ) ) ++s

static int is_comment_line( char *buf );
static int get_param_name( char *dest, char *buf );
static int get_int_param( char *buf );
static char* get_str_param( char *buf );
static int get_clr_param( char *buf );

static int is_comment_line( char *buf )
{
  while( *buf && strchr( " \t\b\r\n", *buf ) )
	++buf;

  if( !*buf || *buf == '#' )
	return 1;
  else
	return 0;
}

/* Takes line of form (in buf):

   param = value

   .. and copies `param' (trimmed) to dest.

   Returns 0 on success, -1 on failure.
*/   
static int get_param_name( char *dest, char *buf )
{
  char *s, *e;
  int n;
  
  s = buf;

  while( *s && *s != '=' && !isalpha(*s) )
	++s;

  if ( !*s || *s == '=' )
	return -1;

  e = s;

  while( *e && (isalnum(*e) || *e == '_') )
	++e;

  if ( !*e )
	return -1;

  n = (unsigned)e - (unsigned)s;

  strncpy( dest, s, n );
  dest[n] = 0;
  return 0;
}

/* Takes a line of the form (in buf):

   param = value

   .. and returns `value' as an integer.
*/   
static int get_int_param( char *buf )
{
  char *p;

  p = strchr( buf, '=' );

  if ( !p )
	return 0;

  return atoi( p+1 );
}

/* Takes a line of the form (in buf):

   param = value

   .., allocates a string, copies `value' into it, and
   returns the newly allocated string.
   
   Returns NULL on failure.
*/
static char* get_str_param( char *buf )
{
  char *p, *e, *s;
  int n;
  
  p = strchr( buf, '=' );

  if ( !p )
	return NULL;

  ++p;
  
  while( *p && strchr( " \t\n\r\b", *p ) )
	++p;

  if ( !*p )
	return NULL;

  e = p;

  while( *e && !strchr( " \t\n\r\b", *e ) )
	++e;

  n = (unsigned)e - (unsigned)p;

  s = (char*)malloc( n+1 );
  strncpy( s, p, n );
  s[n] = 0;

  return s;
  
  return 0;
}

struct clr_assoc {
  char *name;
  int val;
};

static struct clr_assoc clr_tab[] = {
    { "BLACK",	BLACK },
    { "BLUE",    BLUE },
    { "GREEN",    GREEN },
    { "CYAN",    CYAN },
    { "RED",    RED },
    { "MAGENTA",    MAGENTA },
    { "BROWN",    BROWN },
    { "LIGHTGRAY",    LIGHTGRAY },
    { "DARKGRAY",        DARKGRAY},    
    { "LIGHTBLUE",    LIGHTBLUE},
    { "LIGHTGREEN",    LIGHTGREEN},
    { "LIGHTCYAN",    LIGHTCYAN},
    {"LIGHTRED",    LIGHTRED},
    {"LIGHTMAGENTA",    LIGHTMAGENTA},
    {"YELLOW",    YELLOW},
    {"WHITE",    WHITE},
	{"TRANS", 0xff } };

/* Takes a line of the form (in buf):

   param = value

   If `value' is a color name (see above), the
   color is returned as an integer.

   -1 is returned on error.
*/
static int get_clr_param( char *buf )
{
  int n, i;
  char *s;

  if ( (s = get_str_param( buf )) == NULL )
	return -1;

  n = sizeof( clr_tab ) / sizeof( clr_tab[0] );

  for( i=0; i<n; ++i )
	{
	  if ( !strcasecmp( s, clr_tab[i].name ) )
		{
		  free( s );
		  return clr_tab[i].val;	/* Don't depend on order */
		}
	}

  free( s );
  return -1;
}

/*
   Returns:
   0	OK (line processed)
   1	} found
   -1	Error
*/
static int process_line( Level *lev, char *buf )
{
  char *p;
  char param[200];
  
  p = buf;
  LTRIM( p );

  if ( *p == '}' )
	return 1;

  if ( get_param_name( param, buf ) != 0 )
	return -1;

  if ( !strcasecmp( param, "maze" ) )
	{
	  if ( (lev->mazename = get_str_param( buf )) == NULL )
		return -1;
	}
  else if ( !strcasecmp( param, "tile" ) )
	{
	  if ( (lev->tile = get_str_param( buf )) == NULL )
		return -1;
	}
  else if ( !strcasecmp( param, "fruit" ) )
	{
	  if ( (lev->fruitname = get_str_param( buf )) == NULL )
		return -1;
	}
  else if ( !strcasecmp( param, "depth" ) )
	lev->depth = get_int_param( buf );
  else if ( !strcasecmp( param, "regen_wait" ) )
	lev->regen_wait = get_int_param( buf );
  else if ( !strcasecmp( param, "fruit_val" ) )
	lev->fruitval = get_int_param( buf );
  else if ( !strcasecmp( param, "fruit_off" ) )
	lev->fruit_off = get_int_param( buf );
  else if ( !strcasecmp( param, "fruit_on" ) )
	lev->fruit_on = get_int_param( buf );
  else if ( !strcasecmp( param, "fruit_quota" ) )
	lev->max_fruit = get_int_param( buf );
  else if ( !strcasecmp( param, "bg" ) )
	lev->bg = get_clr_param( buf );
  else if ( !strcasecmp( param, "dot" ) )
	lev->dot_clr = get_clr_param( buf );
  else if ( !strcasecmp( param, "bigdot" ) )
	lev->bigdot_clr = get_clr_param( buf );
  else if ( !strcasecmp( param, "ghost_blue" ) )
	lev->en_frames = get_int_param( buf );
  else if ( !strcasecmp( param, "ghost_flash" ) )
	lev->fl_frames = get_int_param( buf );
  else if ( !strcasecmp( param, "lux_body" ) )
	lev->lux_body = get_clr_param( buf );
  else if ( !strcasecmp( param, "glasses" ) )
	lev->lux_glasses = get_clr_param( buf );
  else
	return 0;	/* Ignore unknown line types */

  return 0;
}

int lf_read_level( Level *lev, FILE *fp )
{
  char buf[200];
  char *p;
  int r;
  
  /* Skip lines until { found */
  do {
	if ( fgets( buf, 200, fp ) == NULL )
	  return -2;		/* No more levels */

	p = buf;
	LTRIM( p );
  } while( *p != '{' );

  r = 0;
  
  /* Process lines until error or until } found */
  do {
	if ( fgets( buf, 200, fp ) == NULL )
	  return -1;

	if ( is_comment_line( buf ) )
	  continue;
	
	r = process_line( lev, buf );
	
	if ( r < 0 )
	  return -1;
	
  } while( r == 0 );

  return 0;
}

Level::Level()
{
  /* Items required in level file */
  mazename = fruitname = tile = NULL;
  depth = -1;

  /* Optional items */
  regen_wait = 0;
  bg = BLACK;
  dot_clr = YELLOW;
  bigdot_clr = YELLOW;
  fruitval = 100;
  max_fruit = 100;
  fruit_on = 200;
  fruit_off = 200;
  en_frames = 100;
  fl_frames = 60;
  lux_body = YELLOW;
  lux_glasses = BLACK;  
}

Level::~Level()
{
  if ( mazename )
	free( mazename );
  if ( tile )
	free( tile );
  if ( fruitname )
	free( fruitname );
}
