/*
   fmalloc.cc

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

#include <stdlib.h>
#include <gtools/fmalloc.h>

#define FM_MAGIC1	(0xfeab1e01)
#define FM_MAGIC2	(0xee158a45)

/*
   Mallocs:
   MAGIC	(int)
   size		(int)
   data				<- Returns this 
   MAGIC	(int)
*/
#ifdef DEBUG
void *Malloc( int bytes )
{
  void *p;
  int *i;
  
  p = malloc( bytes + 3*sizeof( int ) );

  i = (int*)(p);

  *i = FM_MAGIC1;
  *(i+1) = bytes;
  p += 2*sizeof(int);

  i = (int*)(p + bytes);
  *i = FM_MAGIC2;
  
  return p;
}

int Free( void *p )
{
  int i;
	
  if ( (i=Mcheck( p )) == 0 )
	{
	  free( p - 2*sizeof(int) );
	  return 0;
	}
  else
	return i;
}

int Mcheck( void *p )
{
  int *i;
  int len;
  
  i = (int*)( p - 2*sizeof(int) );

  if ( *i != FM_MAGIC1 )
	return -1;

  len = *(i+1);

  i = (int*)(p + len);
  
  if ( *i != FM_MAGIC2 )
	 return -2;

  return 0;
}

#else

void *Malloc( int bytes )
{
  return malloc( bytes );
}

int Mcheck( void *p )
{
  return 0;
}

int Free( void *p )
{
  free( p );
  return 0;
}
#endif

