// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#include "defines.h"
#include "structs.h"
#include "globals.h"

void
init_voices ()
{
  int i;

  for (i = 0; i < MAX_TRACK; i++)
    {
      voices[i].sample = 255;
      voices[i].note = 0;

      voices[i].slide_pitch = 0;
      voices[i].slide_goal = 0;
      voices[i].slide_rate = 0;
      voices[i].slide_dir = 0;
      voices[i].slide_period_goal = 0;
      voices[i].pitchbender = 0;
      voices[i].finetune = 0;
      voices[i].last_rate = 0;
      voices[i].last_note = 0;
      voices[i].slide_period = 0;
      voices[i].glissando = 0;

      voices[i].volslide = 0;
      voices[i].finevol = MY_FALSE;

      voices[i].pattern = 0;
      voices[i].position = 0;
      voices[i].loop_times = 0;
      voices[i].cut_count = 0;

      voices[i].arpeg_num = 0;

      voices[i].retrigger = 0;
      voices[i].retrig_vol = 0;

      voices[i].vibra_rate = 0;
      voices[i].vibra_old_rate = 0;
      voices[i].vibra_position = 0;
      voices[i].vibra_depth = 0;
      voices[i].vibra_wave = 0;

      voices[i].last_info = 0;
      voices[i].fineslide_adj = 0;
    }
}
