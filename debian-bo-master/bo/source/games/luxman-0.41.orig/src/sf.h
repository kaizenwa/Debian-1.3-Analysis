/*
   sf.h

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

/*
   A scenario file:

   # Comment
   |Scenario name|level_filename|base_dirs|
   |Scenario name|level_filename|base_dirs|

   etc.

   Where:
		Scenario name:	Any descriptive string
		level_filename:	Name of level file 
		base_dirs:		Base directory. Under this LuxMan expects
						to find:

						(base)/maps		(Image bitmaps)
						(base)/mazes	(Maze bitmaps)
						(base)/11k		(Sounds)
						(base)/fonts	(Fonts)

						Multiple paths may be given, e.g.:
						`.:/usr/games/lib/luxman'
*/

#ifndef _sf_h_
#define _sf_h_

#include <stdio.h>
#include <lib/vllist.h>
#include "lf.h"

struct Scenarios {

  VLList *names;	  	/* Scenario names (char *'s) */
  VLList *files;	  	/* Level files (char *'s) */
  VLList *dirs;			/* base directories (char *'s) */
  
  /* Init from file */
  Scenarios( FILE *fp );
  ~Scenarios();

  int error;			/* Error from reading file:
 						   0	No error
						   -1 	Out of memory
						   -2	Error in file */
};

/*
   Returns:
		0	File read OK
	   -1	Error in file
	   -2	Too many scenarios
*/	   
int read_scenario_file( Scenarios *sns, FILE *fp );
	
#endif

