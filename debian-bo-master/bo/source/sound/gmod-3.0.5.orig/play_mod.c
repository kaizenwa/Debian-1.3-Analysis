// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#include <sys/ioctl.h>
#include <sys/time.h>
#include <sys/types.h>
#include <string.h>
#include <unistd.h>

#ifdef USE_LOCAL
#include "soundcard.h"
#else
#include <sys/soundcard.h>
#endif

#include <sys/ultrasound.h>
#include <stdio.h>
#include <stdlib.h>

#include "defines.h"
#include "structs.h"
#include "globals.h"
#include "protos.h"

#ifdef USE_X
#include "TopShell.h"
#endif

#include "Sequencer.h"

int position;
int jump_to_pos;
struct options_info p_options;
struct song_info *p_song_char;
char played[MAX_POSITION];
struct effect_info effects;
int pat_start;
double saved_td;
int saved_tpd;
int saved_position;

void
play_module (int start_position, struct song_info *song_char_x,
	     struct options_info options_x, int start_delay)
{
  int i;
  extern Sequencer *seq;

  p_options = options_x;
  p_song_char = song_char_x;
  memset(&effects, 0, sizeof(effects));
  init_voices();
  seq->numVoices(p_song_char->nr_channels, p_song_char->vol_type);
  seq->panFactor(p_options.pan_factor);

  if (start_position == 0)
    for (i = 0; i < p_song_char->songlength; i++)
      played[i] = MY_FALSE;

  SEQ_START_TIMER ();

  for (i = 0; i < p_song_char->nr_channels; i++)
    {
      SEQ_BENDER_RANGE (gus_dev, i, 8191);
      SEQ_EXPRESSION (gus_dev, i, 255);
      seq->mainVolume(p_options.main_volume * p_song_char->global_vol / 255);
      seq->pan(i, p_song_char->panning[i]);
      SEQ_PITCHBEND (gus_dev, i, 0);
    }

  next_time = 0.0;

  if (start_position == 0)
    {
      TICKS_PER_DIVISION (p_song_char->play_speed);
      TEMPO (p_song_char->tempo, p_song_char->clock_speed);
    }
  else
    {
      ticks_per_division = saved_tpd;
      tick_duration = saved_td;
    }

  this_time = 0.0;
  
  if (start_position == 0)
    next_time += start_delay; // was tick_duration

  sync_time ();

  position = start_position;
  jump_to_pos = 0;
  pat_start = 0;
  play_next_position();
}

int
do_repeat()
{
  if ((position >= p_song_char->songlength) && (p_options.repeat))
    {
      play_module(0, p_song_char, p_options, 0);
      return (0);
    }

  return (position >= p_song_char->songlength);
}

int
play_next_position()
{
  int extra_ticks;
  int i;
  int voice;
  int tick, pattern, channel, pos, go_to;

  saved_position = 0;

  if (position >= p_song_char->songlength)
    return (do_repeat());

  if ((played[position] > MY_FALSE) && !(jump_to_pos & MOVE_LOOP))
    {
#ifdef USE_X
      sync_time ();
      SEQ_ECHO_BACK (ECHO_LOOP);
#else
      if (p_options.loop_breaker == 1)
	{
	  position = p_song_char->songlength;
	  return (do_repeat());
	}
      else
#endif
	for (i = 0; i < p_song_char->songlength; i++)
	  played[i] = MY_FALSE;
    }

  played[position] = MY_TRUE; // queued
  pos = tune[position];

  if (pos == ORDER_STOP)
    {
      position = p_song_char->songlength;
      return (do_repeat());
    }
  else if ((pos == ORDER_SKIP) || (pos >= p_song_char->nr_patterns))
    {
      position++;
      return (do_repeat());
    }

  sync_time ();
  SEQ_ECHO_BACK ((((unsigned int) position << 16) & 0x00ff0000) |
		 (((unsigned int) pos << 8) & 0x0000ff00) |
		 ECHO_MESSAGE);

  if (pattern_tempo[pos])
    set_speed (pattern_tempo[pos], p_song_char->clock_speed);

  jump_to_pos = 0;
  for (pattern = pat_start; pattern < pattern_len[pos] && jump_to_pos == 0; pattern++)
    {
#ifdef USE_X
      sync_time ();
      SEQ_ECHO_BACK(((unsigned int)pattern << 8) | ECHO_PATTERN);
#endif
      for (channel = 0; channel < p_song_char->nr_channels; channel++)
	{
	  voice = voice_table[pos][channel];
	  if ((go_to = play_note (channel, position, pattern,
				  &((pattern_table[voice])[pattern]),
				  p_song_char, &effects, &p_options)) != 0)
	    jump_to_pos |= go_to;
	}

      /* next_time += tick_duration; */
      for (extra_ticks = 0; extra_ticks <= effects.delay_notes; extra_ticks++)
	{
	  for (tick = 0; tick < ticks_per_division; tick++)
	    {
	      for (channel = 0; channel < p_song_char->nr_channels; channel++)
		{
		  lets_play_voice (channel, &voices[channel], p_song_char,
				   tick);
		}
	      next_time += tick_duration;
	    }
	}

      effects.delay_notes = 0;

#ifndef USE_X
      if (stop_flag)
	jump_to_pos = MOVE_EXIT;
#endif
    }			/* pattern */

  pat_start = 0;

  if (jump_to_pos & MOVE_LOOP)
    {
      pat_start = effects.pattern;
      effects.pattern = 0;
      position -= 1;
    }
  if (jump_to_pos & MOVE_JUMP)
    {
      pat_start = effects.pattern;
      position = effects.position - 1;
    }
  if (jump_to_pos & MOVE_BREAK)
    {
      pat_start = effects.pattern;
    }
  if (jump_to_pos & MOVE_EXIT)
    {
      saved_position = position;
      position = p_song_char->songlength - 1;
    }
  /*
    }

    if (p_options.repeat)
    start_position = 0;
    }
    while (p_options.repeat
#ifndef USE_X
    && !stop_flag
#endif
    );
    */
  position++;

  //return (position >= p_song_char->songlength);
  return (do_repeat());
}

int
end_module(unsigned char lstop_flag)
{
#ifdef USE_X
  extern TopShell *topShell;
  //  extern TrackShell *trackShell;
#endif
  int i;

  if (lstop_flag != STOP_FORWBACK)
    free_patterns ();
  else
    {
      saved_tpd = ticks_per_division;
      saved_td = tick_duration;

      // prevent a loop from being detected

      for (i = 0; i < p_song_char->songlength; i++)
	if (played[i] == MY_TRUE)
	  played[i] = 0;
      
      // prevent loop detection by "rewind"
      played[actual_pos] = 0;
    }
  
#ifdef USE_X
      topShell->setPosition(-1);
#endif

  return (saved_position);
}  
