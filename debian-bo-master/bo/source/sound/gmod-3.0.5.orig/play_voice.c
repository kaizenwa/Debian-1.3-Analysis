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
#include "Sequencer.h"

void
lets_play_voice (int channel, struct voice_info *v,
		 struct song_info *song_char, int tick_no)
{
  extern Sequencer *seq;

  int note, bend, vibra;

  if (v->slide_pitch == SLIDE_OFF)
    {
      sync_time ();
      GUS_VOICEOFF (gus_dev, channel);
      v->slide_pitch = 0;
    }

  if ((v->slide_pitch == SLIDE_ONCE) && (tick_no != 0))
    ;
  else
    {
      if ((v->slide_pitch && v->slide_rate) &&
	  ((tick_no > 0) || (v->slide_pitch == SLIDE_ONCE)))
	{
	  if (song_char->slide_type == SLIDE_PERIOD_LIN)
	    {
	      v->slide_period -= v->slide_rate;
	      period_to_note(v->slide_period / 256, &note, &bend);
	    }
	  else if (song_char->slide_type == SLIDE_FREQ_LIN)
	    {
	      v->slide_period = (int)((111978.0 * 256.0) / (111978.0 / (v->slide_period / 256) + (v->slide_rate / 256.0)));
	      period_to_note(v->slide_period / 256, &note, &bend);
	    }
	  else /* note linear (specific to XM) */
	    {
	      v->slide_period += (100 * v->slide_rate) / (64 * 256);
	      note = v->slide_period / 100;
	      bend = v->slide_period % 100;
	    }

	  if ((v->slide_pitch == SLIDE_PORT) && v->glissando)
	    {
	      if (bend > 0)
		note += 1;
	      bend = 0;
	    }
	  v->pitchbender = (note * 100 + bend) - (v->note * 100);

	  if (v->slide_rate < 0)
	    {
	      if (v->pitchbender <= v->slide_goal)
		{
		  v->pitchbender = v->slide_goal;
		  v->slide_period = v->slide_period_goal;
		  if (v->slide_pitch == SLIDE_PORT)
		    {
		      v->last_note = 0;
		      v->slide_pitch = 0;	/* Stop */
		    }
		  else if (song_char->type == MODULE_S3M)
		    /* slide down hit lower limit */
		    v->slide_pitch = SLIDE_OFF;
		  else
		    v->slide_pitch = 0;	/* Stop */
		  v->slide_rate = 0;
		  v->fineslide_adj = 0;
		}
	    }
	  else
	    {
	      if (v->pitchbender >= v->slide_goal)
		{
		  v->pitchbender = v->slide_goal;
		  v->slide_period = v->slide_period_goal;
		  if (v->slide_pitch == SLIDE_PORT)
		    {
		      v->last_note = 0;
		      v->slide_pitch = 0;	/* Stop */
		    }
		  else if (song_char->type == MODULE_S3M)
		    /* slide up hit upper limit */
		    v->slide_pitch = SLIDE_OFF;
		  else
		    v->slide_pitch = 0;	/* Stop */
		  v->slide_rate = 0;
		  v->fineslide_adj = 0;
		}
	    }

	  /* correct for rounding by the driver */
	  vibra = v->pitchbender + v->finetune;
	  if (vibra >= 0)
	    vibra++;
	  else
	    vibra--;

	  sync_time ();
	  SEQ_PITCHBEND (gus_dev, channel, vibra);
	}
    }

  if ((v->finevol == MY_TRUE) && (tick_no != 0))
    ;
  else if ((v->volslide) && ((tick_no > 0) || (v->finevol == MY_TRUE) || (song_char->vol_on_zero == MY_TRUE)))
    {
      vibra = seq->volume(channel) + v->volslide;

      if (vibra > 255)
	{
	  vibra = 255;
	  v->volslide = 0;
	}
      else if (vibra < 0)
	{
	  vibra = 0;
	  v->volslide = 0;
	}

      sync_time();
      seq->volume(channel, vibra);
    }

  if (v->cut_count)
    if (v->cut_count == tick_no)
      {
	sync_time ();
	seq->volume(channel, 0);
	v->cut_count = 0;    // added with pattern-delay fix

	// stop volume/pan envelopes here too?
      }

  if (v->delay_count)
    if (v->delay_count == tick_no)
      {
	bend = v->finetune;
	if (bend >= 0)
	  bend++;
	else
	  bend--;
	
	sync_time ();
	seq->noteDelay(channel, 0);
	seq->note(channel, v->note, seq->volume(channel));
	SEQ_PITCHBEND (gus_dev, channel, bend);
	v->delay_count = 0;    // added with pattern-delay fix
      }

  if ((v->retrigger) && (tick_no > 0))
    if ((tick_no % v->retrigger) == 0)
      {
	vibra = seq->volume(channel);

	if ((v->retrig_vol != 0) && (v->retrig_vol != 8))
	  {
	    switch (v->retrig_vol)
	      {
	      case 6:
		vibra = (vibra * 2) / 3;
		break;
	      case 7:
		vibra /= 2;
		break;
	      case 14:
		vibra = (vibra * 3) / 2;
		break;
	      case 15:
		vibra *= 2;
		break;
	      default:
		if (v->retrig_vol > 8)
		  vibra += ((1 << (v->retrig_vol - 9)) * VOL_SLIDE_RATE);
		else
		  vibra -= ((1 << (v->retrig_vol - 1)) * VOL_SLIDE_RATE);
		break;
	      }
	    if (vibra > 255)
	      vibra = 255;
	    else if (vibra < 0)
	      vibra = 0;
	  }

	sync_time();
	seq->note(channel, v->note, vibra);
      }

  if (v->arpeg_num)
    {
      v->arpeg_curr = tick_no % v->arpeg_num;

      /* correct for rounding by the driver */
      vibra = v->pitchbender + v->arpeg_note[v->arpeg_curr] + v->finetune;
      if (vibra >= 0)
	vibra++;
      else
	vibra--;

      sync_time ();
      SEQ_PITCHBEND (gus_dev, channel, vibra);
    }

  if ((v->vibra_rate) && (tick_no > 0))
    {
      vibra = vibra_table[v->vibra_wave & 0x03][v->vibra_position];
      vibra = (vibra * v->vibra_depth) / 128;
      
      if (song_char->slide_type == SLIDE_NOTE_LIN)
	{
	  note = v->slide_period / 100;
	  bend = v->slide_period % 100;
	}
      else
	period_to_note ((v->slide_period / 256) + vibra, &note, &bend);

      vibra = (note * 100 + bend) - (v->note * 100) + v->finetune;

      /* correct for rounding by the driver */
      if (vibra >= 0)
	vibra++;
      else
	vibra--;

      sync_time ();
      SEQ_PITCHBEND (gus_dev, channel, vibra);
      v->vibra_position += v->vibra_rate;
      v->vibra_position %= NUM_VIBRA;
    }

  seq->doTick(channel, tick_no);
  seq->doUpdates(channel);
}
