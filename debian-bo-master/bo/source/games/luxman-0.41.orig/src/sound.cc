/*
   sound.cc

   This file is part of LuxMan.
   
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

#include <gtools/util.h>
#include <stdlib.h>
#include "sound.h"
#include "globals.h"

extern "C" {

  struct SoundLoadType {
	char *name;
	int id;
	int divide;
	float prefade;
	float postfade;
  };
}

SoundLoadType slt[] = {
  { "siren.snd", SND_SIREN, 2,      0.0, 0.0 },
  { "power.snd", SND_ENERGIZED, 2,  0.0, 0.0 },
  { "eyes.snd", SND_EYES, 2,        0.0, 0.0 },
  { "begin.snd", SND_BEGIN, 1,      0.0, 0.0 },
  { "credit.snd", SND_CREDIT, 1,    0.0, 0.0 },
  { "credit.snd", SND_DOT, 2,       0.0, 0.0 },
  { "eat.snd", SND_EATGHOST, 1,     0.0, 0.0 },
  { "freeman.snd", SND_FREELIFE, 1, 0.0, 0.0 },
  { "fruit.snd", SND_FRUIT, 2,      0.0, 0.0 },
  { "pacdeath.snd", SND_LUXDIE, 1,  0.0, 0.0 } };

int load_sounds( void )
{
  int i, n;
  char *name;
  char *path;

  if ( gb_use_8k_sound == ARGV_TRUE )
	path = "./8k:/usr/games/lib/luxman/8k";
  else
	path = "./11k:/usr/games/lib/luxman/11k";
  
  n = sizeof( slt ) / sizeof( slt[0] );

  for( i=0; i<n; ++i )
	{
	  if ( find_file( &name, slt[i].name, path, NULL ) == 1 )
		{
		  snd_load_sample( slt[i].id, name, slt[i].divide,
				  slt[i].prefade, slt[i].postfade );
		  free( name );
		}
	}

  return 0;
}
