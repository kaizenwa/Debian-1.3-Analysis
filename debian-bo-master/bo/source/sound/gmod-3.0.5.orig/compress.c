// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#include <string.h>
#include <stdlib.h>

#include "defines.h"
#include "structs.h"
#include "globals.h"

short
compress_voice (short last_voice, short position, int notes_per_track,
		int skip, short first_voice = 0)
{
  int voice;

  for (voice = (int)first_voice; voice < last_voice; voice++)
    {
      if (voice == position)
	{
	  voice += (skip - 1);
	}
      else if ((pattern_table[voice] != NULL) &&
	       !bcmp (pattern_table[voice],
		      pattern_table[position], sizeof (struct note_info) *
		      notes_per_track))
	{
	  free (pattern_table[position]);
	  pattern_table[position] = NULL;
	  return voice;
	}
    }

  return position;
}
