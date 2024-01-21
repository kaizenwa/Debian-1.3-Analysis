// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#ifdef USE_LOCAL
#include "soundcard.h"
#else
#include <sys/soundcard.h>
#endif

#include <sys/ultrasound.h>

#include "defines.h"
#include "structs.h"
#include "globals.h"
#include "protos.h"

#include "Sample.h"

void
set_speed (int parm, unsigned char clock_speed)
{
  if (!parm)
    parm = 1;

  if (parm < 32)
    ticks_per_division = parm;
  else
    tick_duration = ((double) clock_speed * 100.0 / (24.0 * (double) parm));
}

void
set_volslide (int channel, int amount, char type)
{
  int n;

  if (type == MODULE_S3M)
    {
      if ((n = (amount & 0x0f)))
	voices[channel].volslide = -(VOL_SLIDE_RATE * n);
      else
	voices[channel].volslide = VOL_SLIDE_RATE * ((amount >> 4) & 0x0f);
    }
  else
    {
      if ((n = ((amount & 0xf0) >> 4)))
	voices[channel].volslide = VOL_SLIDE_RATE * n;
      else
	voices[channel].volslide = -(VOL_SLIDE_RATE * (amount & 0xf));
    }

  voices[channel].finevol = MY_FALSE;
}

/* Slide up/down should never have RATE or NOTE set to 0 */
void
set_slideto (int channel, int rate, int note, unsigned char type,
	     unsigned char slideType)
{
  extern Sample *samples[];
  int size, curr_note;
  unsigned char set_dir = MY_TRUE;

  voices[channel].slide_pitch = type;

  if (rate != 0)
    {
      if (type == SLIDE_PORT)
	voices[channel].last_rate = rate;
    }
  else
    rate = voices[channel].last_rate;

  if (note == 0)		/* only PORT should have 0 note */
    {
      if (voices[channel].last_note == 0)
	{			/* last port completed */
	  voices[channel].slide_pitch = 0;
	  return;
	}

      voices[channel].slide_rate =
	samples[voices[channel].sample]->slide_rate(rate,
						   voices[channel].slide_dir);
      note = voices[channel].last_note;
      set_dir = MY_FALSE;
    }
  else if (type == SLIDE_PORT)
    voices[channel].last_note = note;

  curr_note = voices[channel].note * 100 + voices[channel].pitchbender;

  size = (note * 100) - curr_note;

  if (!size)
    {
      voices[channel].slide_pitch = 0;

      if (type == SLIDE_PORT)
	{
	  voices[channel].last_note = 0;
	  voices[channel].slide_dir = SLIDE_NEG;
	}

      return;
    }

  if (set_dir == MY_TRUE)
    {
      if (size < 0)
	{
	  if (type == SLIDE_PORT)
	    voices[channel].slide_dir = SLIDE_NEG;
	  rate = -rate;
	}
      else
	{
	  if (type == SLIDE_PORT)
	    voices[channel].slide_dir = SLIDE_POS;
	}

      voices[channel].slide_rate =
	samples[voices[channel].sample]->slide_rate(rate, SLIDE_POS);
    }

  voices[channel].slide_goal = voices[channel].pitchbender + size;

  if (slideType == SLIDE_NOTE_LIN)
    voices[channel].slide_period_goal = note * 100;
  else
    voices[channel].slide_period_goal = period_table[note - NOTE_BASE] * 256;
}

void
set_arpeg (int channel, int amount)
{
  voices[channel].arpeg_num = 3;
  voices[channel].arpeg_note[0] = 0;
  voices[channel].arpeg_curr = 0;
  voices[channel].arpeg_note[1] = (amount >> 4) * 100;
  voices[channel].arpeg_note[2] = (amount & 0x0f) * 100;
}

void
set_vibrato (int channel, int amount)
{
  int depth;

  voices[channel].vibra_rate = (amount >> 4) & 0x0f;

  if (voices[channel].vibra_rate == 0)
    voices[channel].vibra_rate = voices[channel].vibra_old_rate;
  else
    voices[channel].vibra_old_rate = voices[channel].vibra_rate;

  depth = (amount & 0x0f);

  if (depth != 0)
    voices[channel].vibra_depth = depth;
}
