/*
   isleep.cc

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

#include <rawkey/rawkey.h>
#include <unistd.h>

/* Sleep for `sec' seconds while allowing VT switch */
void isleep( int sec )
{
  long us;
  long slept;
  
  us = sec * 1000000;
  
  slept = 0;

  while( slept < us )
	{
	  scan_keyboard();
	  usleep( 50000 );
	  slept += 50000;
	}
}
	  
