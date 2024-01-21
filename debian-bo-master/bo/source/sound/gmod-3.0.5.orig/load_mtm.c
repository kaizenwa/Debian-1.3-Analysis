// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#include <sys/types.h>

#ifdef USE_LOCAL
#include "soundcard.h"
#else
#include <sys/soundcard.h>
#endif

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <sys/fcntl.h>

#include "commands.h"
#include "defines.h"
#include "structs.h"
#include "globals.h"
#include "protos.h"

#include "mtm.h"

#define HEADER_SIZE	66
#define SAMPLE_SIZE	37
#define TRACK_SIZE	192
#define PATTERN_SIZE	64
#define NOTES_PER_TRACK	64
#define NR_CHANNELS	32


typedef struct
{
  char magic[3];		/* must be "MTM" */
  u_char version;		/* upper 4 bits - major, lower minor */
  char name[20];		/* null-term song name string */
  u_short nr_saved_tracks;	/* number of tracks saved */
  u_char last_saved_pattern;	/* last pattern number saved */
  u_char last_played_pattern;	/* last pattern to play (songlength-1) */
  u_short comment_len;		/* length of extra comment field */
  u_char nr_samples;		/* number of samples saved (NOS) */
  u_char attribute;		/* currently defined as 0 */
  u_char bpt;			/* beats per track */
  u_char nr_played_tracks;	/* number of tracks toplay back */
  u_char pan_positions[32];	/* pan positions for each voice (0-15) */
}

mtm_header;

typedef struct
{
  u_char pitch;
  u_char instrument;
  u_char effect;
  u_char argument;
} mtm_note;

typedef struct
{
  mtm_note notes[NOTES_PER_TRACK];
} mtm_track;

typedef struct
{
  u_short tracks[NR_CHANNELS];
} mtm_pattern;

void
dump_mtm_header (mtm_header * h, struct song_info *song_char)
{
  strncpy (song_char->name, h->name, 20);
  song_char->name[20] = '\0';
  sprintf (song_char->desc, "MultiTracker v%d.%d / %u channels", h->version >> 4,
	   h->version & 0x0f, h->nr_played_tracks);
}

#define PITCH(p)	((*(u_char *)(p))>>2)
#define INSTRUMENT(p)	(((*(u_char *)(p)<<4) | (*((u_char *)(p)+1)>>4)) & 077)
#define EFFECT(p)	(*((u_char *)(p)+1) & 0xf)
#define ARGUMENT(p)	(*((u_char *)(p)+2))

static mtm_track
get_track (char *buf)
{
  mtm_track t;
  int i;

  for (i = 0; i < NOTES_PER_TRACK; i++, buf += 3)
    {
      t.notes[i].pitch = PITCH (buf);
      t.notes[i].instrument = INSTRUMENT (buf);
      t.notes[i].effect = EFFECT (buf);
      t.notes[i].argument = ARGUMENT (buf);
    }

  return (t);
}

static mtm_pattern
get_pattern (char *buf)
{
  mtm_pattern p;
  int i;

  for (i = 0; i < NR_CHANNELS; i++, buf += 2)
    p.tracks[i] = INTEL_SHORT (buf);

  return (p);
}


int
load_mtm_module(FILE * mod_fd, struct song_info *song_char,
		unsigned char *buffer)
{
  extern Sample *samples[];
  extern Sequencer *seq;

  mtm_header header;
  int total_sample_size, i;
  char *raw_data;
  unsigned char *sample_data;
  mtm_track *tracks;
  mtm_pattern *patterns;
  u_char order[128];
  int tracki, notei, pati;
  int voice;
  int params, effect, sample, note;

  song_char->lowest_note = 36;
  song_char->highest_note = 98;
  song_char->vol_type = VOL_LINEAR;
  song_char->vol_on_zero = MY_FALSE;
  song_char->slide_type = SLIDE_PERIOD_LIN;
  song_char->clock_speed = 60;

  /* read in header */
  raw_data = (char *) malloc (HEADER_SIZE);
  memcpy (raw_data, buffer, HDR_SIZE);
  fread (raw_data + HDR_SIZE, 1, HEADER_SIZE - HDR_SIZE, mod_fd);

  memcpy (&header.magic, raw_data, 3);
  header.version = BYTE (raw_data + 3);
  memcpy (header.name, raw_data + 4, 20);
  header.nr_saved_tracks = INTEL_SHORT (raw_data + 24);
  header.last_saved_pattern = BYTE (raw_data + 26);
  header.last_played_pattern = BYTE (raw_data + 27);
  header.comment_len = INTEL_SHORT (raw_data + 28);
  header.nr_samples = BYTE (raw_data + 30);
  header.attribute = BYTE (raw_data + 31);
  header.bpt = BYTE (raw_data + 32);
  header.nr_played_tracks = BYTE (raw_data + 33);
  memcpy (header.pan_positions, raw_data + 34, 32);

  song_char->nr_samples = header.nr_samples;
  free (raw_data);
  dump_mtm_header (&header, song_char);

  /* ======== get sample data =============================== */
  total_sample_size = SAMPLE_SIZE * header.nr_samples;
  sample_data = (unsigned char *)malloc(total_sample_size);
  fread (sample_data, 1, total_sample_size, mod_fd);

  fread (order, 1, 128, mod_fd);

  /* =========== read tracks ============== */
  raw_data = (char *) malloc (TRACK_SIZE);
  tracks = (mtm_track *) malloc (sizeof (mtm_track));

  pattern_table[0] = (struct note_info *)
    calloc (1, sizeof (struct note_info) * NOTES_PER_TRACK);

  for (i = 0; i < header.nr_saved_tracks; i++)
    {
      fread (raw_data, 1, TRACK_SIZE, mod_fd);

      tracks[0] = get_track (raw_data);

      pattern_table[i + 1] = (struct note_info *)
	malloc (sizeof (struct note_info) * NOTES_PER_TRACK);

      for (notei = 0; notei < NOTES_PER_TRACK; notei++)
	{
	  params = tracks[0].notes[notei].argument;
	  effect = tracks[0].notes[notei].effect;
	  sample = tracks[0].notes[notei].instrument;
	  note = tracks[0].notes[notei].pitch;

	  if (effect == CMD_EXTENDED)
	    {
	      effect = ((CMD_EXTENDED << 4) & 0xf0) +
		((params >> 4) & 0x0f);
	      params &= 0x0f;
	    }

	  if (note)		/* note->period */
	    {
	      note = note + 3 * 12;	/* shift up 3 octaves */
	    }

	  switch (effect)
	    {
	    case CMD_SPEED:
	      effect = CVT_MOD_SPEED (params);
	      break;
	    case CMD_VOLUME:
	      if (params > 0)
		params = (params * 4) - 1;
	      break;
	    case CMD_BREAK:
	      params = ((params >> 4) & 0x0f) * 10 + (params & 0x0f);
	      break;
	    case CMD_SET_PAN:
	      params *= 17;
	      break;
	    }

	  (pattern_table[i + 1])[notei].note = note;
	  (pattern_table[i + 1])[notei].sample = sample;
	  (pattern_table[i + 1])[notei].command[0] = effect;
	  (pattern_table[i + 1])[notei].parm1[0] = params;
	  (pattern_table[i + 1])[notei].parm2[0] = 0;
	  (pattern_table[i + 1])[notei].command[1] = 0;
	  (pattern_table[i + 1])[notei].parm1[1] = 0;
	  (pattern_table[i + 1])[notei].parm2[1] = 0;
	}
    }
  free (raw_data);

  /* =========== read patterns ============== */
  raw_data = (char *) malloc (PATTERN_SIZE * (header.last_saved_pattern + 1));
  patterns = (mtm_pattern *) malloc (sizeof (mtm_pattern) *
				     (header.last_saved_pattern + 1));

  fread (raw_data, 1, PATTERN_SIZE * (header.last_saved_pattern + 1), mod_fd);
  for (i = 0; i <= header.last_saved_pattern; i++)
    patterns[i] = get_pattern (raw_data + i * PATTERN_SIZE);

  free (raw_data);

  for (i = 0; i <= header.last_saved_pattern; i++)
    for (voice = 0; voice < NR_CHANNELS; voice++)
      voice_table[i][voice] = patterns[i].tracks[voice];


  song_char->comment = (char *) realloc (song_char->comment, header.comment_len + 1);
  bzero (song_char->comment, header.comment_len + 1);
  fread (song_char->comment, 1, header.comment_len, mod_fd);

  for (i = 0; i < header.nr_samples; i++)
    {
      samples[i] = new MTM_sample;
      samples[i]->load(*seq, mod_fd, i, 1, sample_data + i * SAMPLE_SIZE);
    }

  song_char->nr_tracks = header.nr_saved_tracks + 1;
  song_char->songlength = header.last_played_pattern + 1;
  song_char->nr_channels = header.nr_played_tracks;
  song_char->play_speed = 6;
  song_char->tempo = 125;
  song_char->nr_patterns = header.last_saved_pattern + 1;

  /* copy pattern orders into the tune area */
  for (i = 0; i <= header.last_played_pattern; i++)
    {
      tune[i] = order[i];
    }

  /* determine maximum channel actually used */

  song_char->nr_channels = 0;

  for (tracki = 0; tracki < header.nr_played_tracks; tracki++)
    for (pati = 0; pati < (header.last_saved_pattern + 1); pati++)
      if ((tracki > song_char->nr_channels) && (patterns[pati].tracks[tracki]))
	song_char->nr_channels = tracki;

  song_char->nr_channels++;

  /* set proper panning */

  for (i = 0; i < song_char->nr_channels; i++)
    song_char->panning[i] = (header.pan_positions[i] & 0x0f) * 17;

  /* all done */
  free(sample_data);
  free (tracks);
  free (patterns);

  return (1);
}
