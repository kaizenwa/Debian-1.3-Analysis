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

#include "ult.h"

#define HEADER_SIZE	48
#define SAMPLE_SIZE	64
#define TRACK_SIZE	192
#define PATTERN_SIZE	64
#define NOTES_PER_TRACK	64
#define NR_CHANNELS	32

#define REPEAT_NOTE	0xfc	/* note signifying a RLE pattern */

typedef struct
{
  char title[33];
  int version;
  int text_len;
  char *text;
  int nr_samples;
  int nr_channels;
  int nr_patterns;
  int order[256];
  int pan_positions[32];
  int sample_offset;
  int pattern_offset;
  /*  int eoh_offset; */
}

ult_header;

typedef struct
{
  int a;
}

ult_pattern;

void convert_ult_effect (u_long *, u_long *);

void
dump_ult_header (ult_header * h, struct song_info *song_char)
{
  sprintf (song_char->desc, "UltraTracker v%d / %u channels", h->version,
	   h->nr_channels);
  strcpy (song_char->name, h->title);

  if (h->version >= 2 && h->text_len)
    {
      song_char->comment = (char *) realloc (song_char->comment, h->text_len + 1);
      bzero (song_char->comment, h->text_len + 1);
      strncpy (song_char->comment, h->text, h->text_len);
    }
}

ult_header
get_ult_header (FILE * mod_fd, unsigned char *buffer, u_char **sample_data)
{
  int i;
  u_char *raw_data;
  u_char tmpchar;
  int total_sample_size;
  ult_header header;
  u_long sample_size = SAMPLE_SIZE;

  /* read in first part of header */
  raw_data = (unsigned char *) malloc (HEADER_SIZE);
  memcpy (raw_data, buffer, HDR_SIZE);	/* HEADER_SIZE == HDR_SIZE ! */
  /* fread (raw_data, 1, HEADER_SIZE, mod_fd); */

  memcpy (header.title, raw_data + 15, 32);	/* copy song title */
  header.title[32] = 0;
  raw_data[15] = 0;		/* terminate "V00x" in magic number.
				 * WARNING: may overwrite title, so copy
				 * title first */

  /* get format version:
   *  2 has header+47 defined as a text_len byte,
   *  3 has pan-position table after NOP byte
   */
  header.version = atoi ((char *)(raw_data + 12));

  if (header.version >= 2)
    header.text_len = raw_data[47] * 32;
  else
    header.text_len = 0;
  free (raw_data);

  if (header.text_len)
    {
      header.text = (char *)malloc (header.text_len + 1);
      fread (header.text, 1, header.text_len, mod_fd);
      header.text[header.text_len] = 0;
    }
  else
    header.text = NULL;

  /* get samples */
  fread (&tmpchar, 1, 1, mod_fd);
  header.nr_samples = tmpchar;

  header.sample_offset = HEADER_SIZE + header.text_len + 1;

  /* ======== get sample data =============================== */

  /* version 4 files have C2 frequency */
  if (header.version >= 4)
    sample_size += 2;

  total_sample_size = sample_size * header.nr_samples;
  *sample_data = (u_char *)malloc(total_sample_size);
  fread (*sample_data, 1, total_sample_size, mod_fd);

  raw_data = (unsigned char *)malloc (258);
  fread (raw_data, 1, 258, mod_fd);

  for (i = 0; i < 256; i++)
    header.order[i] = (unsigned) raw_data[i];

  /* these are stored as "last channel" and "last pattern",
   * so we add one to make it ordinal
   */
  header.nr_channels = raw_data[i++] + 1;
  header.nr_patterns = raw_data[i] + 1;

  header.pattern_offset = header.sample_offset +
    header.nr_samples * sample_size + 258;

  if (header.version >= 3)
    {
      fread (raw_data, 1, header.nr_channels, mod_fd);
      for (i = 0; i < header.nr_channels; i++)
	header.pan_positions[i] = raw_data[i];

      header.pattern_offset += header.nr_channels;
    }
  else
    {
      for (i = 0; i < header.nr_channels; i++)
	header.pan_positions[i] = 7;
    }

  free (raw_data);

  return (header);
}


int
load_ult_module (FILE * mod_fd, struct song_info *song_char,
		 struct options_info options, unsigned char *buffer)
{
  extern Sample *samples[];
  extern Sequencer *seq;

  ult_header header;
  int i;
  u_char *sample_data;
  int chani, notei, pati, absi;
  int voice;
  int sample_size;
#ifdef DEBUG
  int tn = 0;
#endif

  song_char->lowest_note = 36;
  song_char->highest_note = 95;
  song_char->vol_on_zero = MY_FALSE;
  song_char->slide_type = SLIDE_PERIOD_LIN;
  song_char->clock_speed = 60;

  header = get_ult_header(mod_fd, buffer, &sample_data);

  song_char->nr_samples = header.nr_samples;
  song_char->nr_patterns = header.nr_patterns;

  /* ========== copy pattern order into tune[] ============== */
#ifdef SHOW_ORDER
  for (i = 0; i < 256 && header.order[i] != 255; i++)
    printf ("%03d%c", header.order[i], (i + 1) % 20 ? ' ' : '\n');
  putchar ('\n');
#endif

  /* copy pattern orders into the tune area and find song length
   * NB: not in the docs, but apparently empty patterns are set
   * to 255; I'm assuming the first 255 ends the song.
   */
  song_char->songlength = 0;
  for (i = 0; i < 256 && header.order[i] != 255; i++)
    {
      tune[i] = header.order[i];
      song_char->songlength++;
    }

  song_char->nr_tracks = header.nr_patterns * header.nr_channels;
  song_char->nr_channels = header.nr_channels;
  song_char->play_speed = 6;
  song_char->tempo = 125;

  /* set panning */

  for (i = 0; i < header.nr_channels; i++)
    song_char->panning[i] = (header.pan_positions[i] & 0x0f) * 17;

  dump_ult_header (&header, song_char);

  /* get patterns from file */
  for (chani = 0; chani < header.nr_channels; chani++)
    {
      /* allocate memory for track */

      for (pati = 0; pati < header.nr_patterns; pati++)
	pattern_table[header.nr_patterns * chani + pati] = (struct note_info *)
	  malloc (sizeof (struct note_info) * NOTES_PER_TRACK);

      for (absi = 0; absi < (NOTES_PER_TRACK * header.nr_patterns);)
	{
	  unsigned char *p;
	  u_char note_bytes[7];
	  int repeat;

	  u_long param[2], note, effect[2], sample, param2;

	  fread (note_bytes, 1, 5, mod_fd);
	  if (note_bytes[0] == REPEAT_NOTE)
	    {
	      fread (note_bytes + 5, 1, 2, mod_fd);
	      repeat = note_bytes[1];

	      if (repeat == 0)
		{
		  repeat = 1;
		}
	      p = note_bytes + 2;
	    }
	  else
	    {
	      repeat = 1;
	      p = note_bytes;
	    }

	  note = p[0];
	  sample = p[1];
	  effect[0] = p[2] >> 4;/* are these switched? */
	  effect[1] = p[2] & 0xf;
	  param[0] = INTEL_SHORT (p + 3) >> 8;
	  param[1] = INTEL_SHORT (p + 3) & 0xff;

	  convert_ult_effect (&effect[0], &param[0]);
	  convert_ult_effect (&effect[1], &param[1]);

	  /* special case for setoffset fine */

	  if ((effect[0] == CMD_SETOFFSET_1024) && (effect[1] == CMD_SETOFFSET_1024))
	    {
	      effect[0] = CMD_SETOFFSET_FINE;
	      effect[1] = 0;
	      param2 = param[1];
	      param[1] = 0;
	    }
	  else
	    param2 = 0;


	  if (note)		/* note->period */
	    {
	      note = note + 3 * 12 - 1;	/* shift up 3 octaves */
	    }

	  for (i = 0; i < repeat && absi < (NOTES_PER_TRACK * header.nr_patterns); i++)
	    {
	      pati = absi / NOTES_PER_TRACK;
	      notei = absi % NOTES_PER_TRACK;

	      voice = header.nr_patterns * chani + pati;
	      voice_table[pati][chani] = voice;

	      (pattern_table[voice])[notei].note = note;
	      (pattern_table[voice])[notei].sample = sample;
	      (pattern_table[voice])[notei].command[0] = effect[0];
	      (pattern_table[voice])[notei].parm1[0] = param[0];
	      (pattern_table[voice])[notei].parm2[0] = param2;
	      (pattern_table[voice])[notei].command[1] = effect[1];
	      (pattern_table[voice])[notei].parm1[1] = param[1];
	      (pattern_table[voice])[notei].parm2[1] = 0;
	      absi++;
#ifdef DEBUG
	      tn++;
#endif
	    }
#ifdef DEBUG
	  if (i != repeat)
	    fprintf (stderr, "bail: i:%d repeat:%d\n", i, repeat);
#endif
	}

      if (options.compress)
	for (pati = 0; pati < header.nr_patterns; pati++)
	  voice_table[pati][chani] =
	    compress_voice (voice_table[pati][chani],
			    voice_table[pati][chani],
			    NOTES_PER_TRACK, 1);
    }

  sample_size = SAMPLE_SIZE;

  if (header.version >= 4)
    sample_size += 2;

  for (i = 0; i < header.nr_samples; i++)
    {
      samples[i] = new ULT_sample;
      samples[i]->load(*seq, mod_fd, i, 1, sample_data + i * sample_size,
		       &header.version);
    }

  /* all done */
  free (sample_data);

  if (header.text_len)
    free (header.text);		/* song text of size header.text_len */

  if (header.version == 1)
    song_char->vol_type = VOL_LOG;
  else
    song_char->vol_type = VOL_LINEAR;

  return (1);
}


void
convert_ult_effect (u_long * effect, u_long * param)
{
  switch (*effect)
    {
    case 0:			/* arpeggio */
    case 1:			/* portamento up */
    case 2:			/* portamento down */
    case 3:			/* tone portamento */
    case 4:			/* vibrato */
    case 7:			/* tremolo */
    case 0xa:			/* volslide */
    case 0xc:			/* volume */
      break;
    case 9:			/* sample offset */
      *effect = CMD_SETOFFSET_1024;
      break;
    case 0xd:			/* pattern break */
      *param = ((*param >> 4) & 0x0f) * 10 + (*param & 0x0f);
      break;
    case 0xe:
      *effect = (0xe0) + ((*param >> 4) & 0x0f);
      *param &= 0x0f;
      break;
    case 0xf:			/* set speed */
      *effect = CVT_MOD_SPEED (*param);
      break;
    case 5:			/* special sample commands */
      *effect = 0;
      *param = 0;
      break;
    case 0xb:			/* set balance */
      *effect = CMD_SET_PAN;
      *param = (*param & 0x0f) * 17;
      break;
    default:
      *effect = 0;
      *param = 0;
    }
}
