// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#ifdef USE_LOCAL
#include "soundcard.h"
#else
#include <sys/soundcard.h>
#endif

#include <sys/ultrasound.h>

#include "commands.h"
#include "defines.h"
#include "structs.h"
#include "globals.h"
#include "protos.h"

#include "Six69.h"

int
load_669_module(FILE * mod_fd, struct song_info *song_char,
		struct options_info options, unsigned char *buffer)
{
  extern Sample *samples[];
  extern Sequencer *seq;

  int i;
  int sample_ptr;
  int position;
  int voice;
  unsigned char *tune_ptr, *len_ptr, *tempo_ptr;	/* array 0-127 */
  unsigned char header[0x1f1];	/* changed from 1084 */
  unsigned char *sample_infox;
  int nr_samples;		/* 16 or 32 samples */
  int slen, npat;
  int format_version = 0;

  memcpy (header, buffer, HDR_SIZE);

  if (fread(header + HDR_SIZE, 1, sizeof(header) - HDR_SIZE, mod_fd) !=
      sizeof(header) - HDR_SIZE)
    {
      /* short file (header) */
      return 0;
    }

  if (INTEL_SHORT(header) != 0x6669)
    format_version = 1;

  song_char->comment = (char *)realloc(song_char->comment, 109);
  bzero(song_char->comment, 109);
  strncpy(song_char->comment, header + 2, 108);
  npat = header[0x6f];
  song_char->nr_patterns = npat;
  tune_ptr = &header[0x71];

  for (slen = 0; slen < 128 && tune_ptr[slen] != 0xff; slen++);

  for (i = 0; i < slen; i++)
    tune[i] = tune_ptr[i];

  len_ptr = &header[0x171];

  for (i = 0; i < 0x80; i++)
    pattern_len[i] = len_ptr[i] + 1;

  tempo_ptr = &header[0xf1];

  for (i = 0; i < slen; i++)
    pattern_tempo[i] = tempo_ptr[i];

  nr_samples = header[0x6e];

  if ((sample_infox = (unsigned char *)malloc(nr_samples * 0x19)) == NULL)
    {
      /* Could not allocate memory for sample information */
      return 0;
    }

  if (fread(sample_infox, 1, nr_samples * 0x19, mod_fd) != (nr_samples * 0x19))
    {
      /* Could not read sample information */
      free(sample_infox);
      return 0;
    }

  song_char->nr_samples = nr_samples;
  song_char->nr_tracks = npat * 8;
  song_char->songlength = slen;
  song_char->nr_channels = 8;
  song_char->lowest_note = 36;
  song_char->highest_note = 99;
  song_char->play_speed = 4;
  song_char->tempo = 75;
  song_char->vol_type = VOL_LINEAR;
  song_char->vol_on_zero = MY_FALSE;
  song_char->slide_type = SLIDE_FREQ_LIN;
  song_char->clock_speed = 60;
  strcpy (song_char->desc, "669 / 8 channels");

  for (position = 0; position < npat; position++)
    {
      unsigned char patterns[0x600];
      int pat, channel, x;

      /*
	 int pp = 0x1f1 + (nr_samples * 0x19) + (position * 0x600);
	 */

      for (voice = 0; voice < song_char->nr_channels; voice++)
	if ((pattern_table[position * song_char->nr_channels + voice] =
	     (struct note_info *) malloc (sizeof (struct note_info) * 64)) ==
	    NULL)
	  {
	    /* Can't allocate memory for a pattern */
	    free(sample_infox);
	    return 0;
	  }

      if ((x = fread(patterns, 1, 0x600, mod_fd)) != 0x600)
	{
	  /* Short file */
	  free(sample_infox);
	  return 0;
	}

      for (pat = 0; pat < 64; pat++)
	{

	  for (channel = 0; channel < 8; channel++)
	    {
	      unsigned char *p;

	      unsigned vol, period, sample, effect, params;
	      unsigned effect1 = 0, effect2 = 0, parm1 = 0, parm2 = 0;

	      p = &patterns[pat * 24 + channel * 3];

	      if (p[0] >= 0xfe)
		{
		  sample = 0;
		  period = 0;
		}
	      else
		{
		  period = (p[0] >> 2) + 36;	/* AJR:  changed from 48 */
		  sample = (((p[0] << 4) & 0x30) | ((p[1] >> 4) & 0x0f)) + 1;
		}

	      effect = (p[2] >> 4);
	      params = p[2] & 0x0f;
	      vol = (p[1] & 0x0f) * 17;

	      if (period)
		{
		  effect1 = CMD_VOLUME;
		  parm1 = vol;
		}

	      if (p[0] == 0xfe)
		{
		  effect1 = CMD_VOLUME;
		  parm1 = vol;
		}

	      if (p[2] == 0xff)
		{
		  effect2 = CMD_NOP;
		}
	      else
		switch (effect)
		  {
		  case 0:	/* a - Portamento up */
		    parm2 = params * SLIDE_RATE_669;
		    if (parm2 == 0)
		      effect2 = 0;
		    else
		      effect2 = CMD_SLIDEUP;
		    break;

		  case 1:	/* b - Portamento down */
		    parm2 = params * SLIDE_RATE_669;
		    if (parm2 == 0)
		      effect2 = 0;
		    else
		      effect2 = CMD_SLIDEDOWN;
		    break;

		  case 2:	/* c - Port to note */
		    parm2 = params * SLIDE_RATE_669;
		    if (parm2 == 0)
		      effect2 = 0;
		    else
		      effect2 = CMD_SLIDETO;
		    break;

		  case 3:	/* d - Frequency adjust */
		    parm2 = params * SLIDE_RATE_669;
		    if (parm2 == 0)
		      effect2 = 0;
		    else
		      effect2 = CMD_FINEPORTUP;
		    break;

		  case 4:	/* e - Frequency vibrato */
		    parm2 = 0x70 || params;
		    if (parm2 == 0x70)
		      effect2 = 0;
		    else
		      effect2 = CMD_VIBRATO;
		    break;

		  case 5:	/* f - Set tempo */
		    if ((params == 0) && (format_version == 1))
		      parm2 = 1;
		    else
		      parm2 = params;
		    if (parm2 == 0)
		      effect2 = 0;
		    else
		      effect2 = CMD_SET_TICKS;
		    break;

		  default:
		    effect2 = 0;
		    parm2 = 0;
		  }

	      if (period)
		if (period < song_char->lowest_note)
		  period = song_char->lowest_note;
		else if (period > song_char->highest_note)
		  period = song_char->highest_note;

	      voice = position * song_char->nr_channels + channel;
	      voice_table[position][channel] = voice;

	      (pattern_table[voice])[pat].note = period;
	      (pattern_table[voice])[pat].sample = sample;
	      (pattern_table[voice])[pat].command[0] = effect1;
	      (pattern_table[voice])[pat].parm1[0] = parm1;
	      (pattern_table[voice])[pat].parm2[0] = 0;
	      (pattern_table[voice])[pat].command[1] = effect2;
	      (pattern_table[voice])[pat].parm1[1] = parm2;
	      (pattern_table[voice])[pat].parm2[1] = 0;
	    }

	}

      if (options.compress)
	for (channel = 0; channel < song_char->nr_channels; channel++)
	  voice_table[position][channel] =
	    compress_voice(voice_table[position][channel],
			   voice_table[position][channel],
			   64, 1);
    }

  sample_ptr = 0x1f1 + (nr_samples * 0x19) + (npat * 0x600);	/* Location where the
								 * first sample is
								 * stored */
  // total_mem = 0;

  for (i = 0; i < nr_samples; i++)
    {
      samples[i] = new Six69_sample;
      samples[i]->load(*seq, mod_fd, i, 1, &sample_infox[i * 0x19]);
    }

  /* set panning */

  for (i = 0; i < song_char->nr_channels; i++)
    if ((i % 2) == 0)
      song_char->panning[i] = 0;
    else
      song_char->panning[i] = 255;

  free(sample_infox);
  return 1;
}
