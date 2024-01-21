// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>

#ifdef USE_LOCAL
#include "soundcard.h"
#else
#include <sys/soundcard.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#include "commands.h"
#include "defines.h"
#include "structs.h"
#include "globals.h"
#include "protos.h"

#include "Sequencer.h"
#include "s3m.h"

struct list_node
{
  struct list_node *prev, *next;
  unsigned int number;
  unsigned int location;
  char type;
};

static unsigned char s3m_effect_tab[] =
{CMD_SET_TICKS, CMD_JUMP, CMD_BREAK, CMD_VOLSLIDE,	/* A B C D */
 CMD_SLIDEDOWN, CMD_SLIDEUP, CMD_SLIDETO, CMD_VIBRATO,	/* E F G H */
 CMD_TREMOR, CMD_ARPEG2, CMD_VIBRAANDVOL, CMD_PORTANDVOL,	/* I J K L */
 CMD_INFO, CMD_INFO, CMD_SETOFFSET, CMD_INFO,	/* M N O P */
 CMD_RETRIGVOL, CMD_TREMOLO, CMD_EXTENDED, CMD_SET_BPM,	/* Q R S T */
 CMD_INFO, CMD_GLOBAL_VOL, CMD_INFO, CMD_INFO,	/* U V W X */
 CMD_INFO, CMD_INFO};		/* Y Z */


void
insert_in_list (struct list_node *list, char type, int number,
		unsigned int location)
{
  struct list_node *current = list->next;
  struct list_node *node;

  node = (struct list_node *) malloc (sizeof (struct list_node));
  node->type = type;
  node->number = number;
  node->location = location;

  while ((current != list) && (current->location < node->location))
    current = current->next;

  node->next = current;
  node->prev = current->prev;
  (current->prev)->next = node;
  current->prev = node;
}

void
catchup (FILE * mod_fd, int wanted, int *file_pos)
{
  char input[1024];
  int bytes_to_read;

  while (wanted > *file_pos)
    {
      bytes_to_read = wanted - *file_pos;
      if (bytes_to_read > 1024)
	bytes_to_read = 1024;
      fread (input, bytes_to_read, 1, mod_fd);
      (*file_pos) += bytes_to_read;
    }
}

void
cvt_s3m_effect (unsigned char *effect, unsigned char *parm)
{
  if ((*effect > 0) && (*effect <= 26))
    *effect = s3m_effect_tab[*effect - 1];
  else
    *effect = CMD_INFO;

  if ((*effect == CMD_SET_BPM) && (*parm <= 0x20))
    *effect = CMD_INFO;
}

void
load_s3m_pattern (FILE * mod_fd, int pat_no, struct song_info *song_char,
		  unsigned char channel_map[], int *file_pos)
{
  int i;
  unsigned char has_note, has_volume, has_command;
  int read_bytes, voice;
  unsigned char channel, note, instrument, volume, command, info;
  unsigned char buffer[100];
  int index;
  int patternEnd;

  if (mod_fd)
    {
      int bytesRead;

      bytesRead = fread(buffer, 1, 2, mod_fd);
      (*file_pos) += bytesRead;
      
      if (bytesRead != 2)
	patternEnd = 0;
      else
	patternEnd = *file_pos + INTEL_SHORT(buffer);
    }

  for (i = 0; i < song_char->nr_channels; i++)
    {
      voice_table[pat_no][i] = (pat_no * song_char->nr_channels) + i;
      pattern_table[voice_table[pat_no][i]] =
	(struct note_info *) calloc (1, sizeof (struct note_info) * 64);
    }

  if (mod_fd)
    {
      for (i = 0; i < 64; i++)
	{
	  while (*file_pos < patternEnd)
	    {
	      has_note = 0;
	      has_volume = 0;
	      has_command = 0;
	      read_bytes = 0;
	      index = 0;

	      note = instrument = volume = command = info = 0;

	      fread (buffer, 1, 1, mod_fd);
	      (*file_pos) += 1;

	      if (buffer[0] == 0)
		break;

	      channel = channel_map[buffer[0] & 31];

	      if (buffer[0] & 32)
		{
		  has_note = 1;
		  read_bytes += 2;
		}

	      if (buffer[0] & 64)
		{
		  has_volume = 1;
		  read_bytes += 1;
		}

	      if (buffer[0] & 128)
		{
		  has_command = 1;
		  read_bytes += 2;
		}

	      fread (buffer, read_bytes, 1, mod_fd);
	      (*file_pos) += read_bytes;

	      if (has_note)
		{
		  if (buffer[0] == 255)	/* empty note */
		    note = /* NOTE_LAST; */ 0;
		  else if (buffer[0] == 254)	/* stop sample */
		    note = NOTE_STOP;
		  else
		    {
		      note = ((buffer[0] >> 4) & 0x0f) * 12 + (buffer[0] & 0x0f) + 12;
		      if (note < song_char->lowest_note)
			note = song_char->lowest_note;
		      else if (note > song_char->highest_note)
			note = song_char->highest_note;
		    }
		  instrument = buffer[1];
		  index += 2;
		}

	      if (has_volume)
		{
		  volume = buffer[index];
		  if (volume > 0)
		    volume = volume * 4 - 1;
		  index += 1;
		}

	      if (has_command)
		{
		  command = buffer[index];
		  info = buffer[index + 1];
		  cvt_s3m_effect (&command, &info);
		}

	      if (channel != 255)
		{
		  voice = voice_table[pat_no][channel];

		  (pattern_table[voice])[i].note = note;
		  (pattern_table[voice])[i].sample = instrument;
		  if (has_volume)
		    {
		      (pattern_table[voice])[i].command[0] = CMD_VOLUME;
		      (pattern_table[voice])[i].parm1[0] = volume;
		    }
		  else
		    {
		      (pattern_table[voice])[i].command[0] = 0;
		      (pattern_table[voice])[i].parm1[0] = 0;
		    }

		  (pattern_table[voice])[i].parm2[0] = 0;
		  (pattern_table[voice])[i].command[1] = command;
		  (pattern_table[voice])[i].parm1[1] = info;
		  (pattern_table[voice])[i].parm2[1] = 0;
		}
	    }
	}
    }
}

unsigned int
load_s3m_ins (FILE * mod_fd, unsigned char *sample_inf, int *file_pos)
{
  unsigned int ret_val;

  fread (sample_inf, 0x50, 1, mod_fd);
  (*file_pos) += 0x50;

  ret_val = INTEL_SHORT (sample_inf + 0x0e) * 16;

  if (strncmp ((char *)(sample_inf + 0x4c), "SCRS", 4) != 0)
    ret_val = 0;

  return ret_val;
}

int
load_s3m_module (FILE ** mod_fd, struct song_info *song_char,
		 struct options_info options, unsigned char *buffer,
		 char *command)
{
  extern Sample *samples[];
  extern Sequencer *seq;

  int i;

  unsigned char header[0x60];

  int nr_samples;
  int slen, npat, loadpat = 0;
  char mname[29];
  unsigned short flags;
  unsigned char *raw_data, *sample_inf;
  unsigned int current_samp;
  struct list_node load_list, *current_list, *tmp_list;
  struct list_node sample_list;
  unsigned short loop_flags = 0;
  int file_position;
  unsigned char channel_map[32];
  int max_loaded = 0;
  unsigned short version;
  int reloadSamples = 0;
  int cutFactor = 1;

  load_list.next = load_list.prev = &load_list;
  sample_list.next = sample_list.prev = &sample_list;

  memcpy (header, buffer, HDR_SIZE);

  if (fread (header + HDR_SIZE, 1, sizeof (header) - HDR_SIZE, *mod_fd) !=
      sizeof (header) - HDR_SIZE)
    {
      /* Short header */
      return 0;
    }

  strncpy (mname, (char *)header, 28);
  mname[28] = '\0';
  remove_noprint (mname);

  strcpy (song_char->name, mname);

  nr_samples = INTEL_SHORT (&header[0x22]);
  flags = INTEL_SHORT (&header[0x26]);
  song_char->nr_samples = nr_samples;
  song_char->nr_channels = 0;

  for (i = 0; i < 32; i++)
    channel_map[i] = 255;

  for (i = 0x40; i < (0x40 + 32); i++)
    if (header[i] <= 15)
      {
	channel_map[i - 0x40] = song_char->nr_channels;
	if (header[i] <= 7)
	  song_char->panning[song_char->nr_channels] = 0;
	else
	  song_char->panning[song_char->nr_channels] = 255;
	song_char->nr_channels++;
      }

  if (flags & 16)
    {				/* amiga limits */
      song_char->lowest_note = 48;
      song_char->highest_note = 83;
    }
  else
    {
      song_char->lowest_note = 12;
      song_char->highest_note = 107;
    }

  version = INTEL_SHORT (&header[0x28]);
  if (((version >> 12) & 0x0f) == 0x01)
    sprintf (song_char->desc, "ScreamTracker v%x.%02x / %u channels",
	     (version >> 8) & 0x0f, version & 0xff, song_char->nr_channels);
  else
    sprintf (song_char->desc, "S3M-Tracker v%x.%02x / %u channels",
	     (version >> 8) & 0x0f, version & 0xff, song_char->nr_channels);

  if ((version == 0x1300) || (flags & 64))
    song_char->vol_on_zero = MY_TRUE;
  else
    song_char->vol_on_zero = MY_FALSE;

  song_char->global_vol = header[0x30];
  if (song_char->global_vol > 0)
    song_char->global_vol = (song_char->global_vol * 4) - 1;
  else
    song_char->global_vol = 255;
  song_char->play_speed = header[0x31];
  if ((song_char->tempo = header[0x32]) <= 0x20)
    song_char->tempo = 125;
  song_char->vol_type = VOL_LINEAR;
  song_char->slide_type = SLIDE_PERIOD_LIN;
  song_char->clock_speed = 60;

  slen = INTEL_SHORT (&header[0x20]);
  npat = INTEL_SHORT (&header[0x24]);

  song_char->nr_tracks = npat * song_char->nr_channels;
  song_char->songlength = slen;

  raw_data = (unsigned char *) malloc (slen);
  fread (raw_data, slen, 1, *mod_fd);

  for (i = 0; i < slen; i++)
    {
      tune[i] = raw_data[i];

      if (tune[i] == 255)
	tune[i] = ORDER_STOP;
      else if (tune[i] == 254)
	tune[i] = ORDER_SKIP;
      else if (tune[i] > loadpat)
	loadpat = tune[i];
    }

  loadpat++;
  song_char->nr_patterns = loadpat;

  free (raw_data);

  if (INTEL_SHORT (&header[0x2a]) == 2)
    loop_flags = WAVE_UNSIGNED;

  sample_inf = (unsigned char *) malloc (nr_samples * 0x50);

  raw_data = (unsigned char *) malloc (nr_samples * 2);
  fread (raw_data, nr_samples, 2, *mod_fd);

  for (i = 0; i < nr_samples; i++)
    if ((current_samp = INTEL_SHORT (&raw_data[i * 2]) * 16) > 0)
      insert_in_list (&load_list, 'I', i, current_samp);

  free (raw_data);

  raw_data = (unsigned char *) malloc (npat * 2);
  fread (raw_data, npat, 2, *mod_fd);

  for (i = 0; i < loadpat; i++)
    if ((current_samp = INTEL_SHORT (&raw_data[i * 2]) * 16) > 0)
      insert_in_list (&load_list, 'P', i, current_samp);
    else
      insert_in_list(&load_list, 'E', i, 0xffff * 16);
	
  free (raw_data);

  file_position = 0x60 + slen + (nr_samples * 2) + (npat * 2);

  if (header[0x35] == 252)
    {
      raw_data = (unsigned char *) malloc (32);
      fread (raw_data, 32, 1, *mod_fd);
      file_position += 32;

      for (i = 0; i < 32; i++)
	if ((channel_map[i] != 255) && (raw_data[i] & 32))
	  song_char->panning[channel_map[i]] = (raw_data[i] & 0x0f) * 17;
    }

  for (i = 0; i < nr_samples; i++)
    samples[i] = new S3M_sample;

  do
    {
      current_list = load_list.next;

      while (current_list != &load_list)
	{
	  if (current_list->location < file_position)
	    current_list = current_list->next;
	  else
	    {
	      if ((current_list->type == 'P') || (current_list->type == 'E'))
		{
		  if (current_list->type == 'P')
		    {
		      catchup (*mod_fd, current_list->location, &file_position);
		      load_s3m_pattern (*mod_fd, current_list->number,
					song_char, channel_map, &file_position);
		    }
		  else
		    load_s3m_pattern(0, current_list->number, song_char,
				     channel_map, &file_position);

		  if (options.compress)
		    {
		      if (current_list->number > max_loaded)
			max_loaded = current_list->number;
		      for (i = 0; i < song_char->nr_channels; i++)
			voice_table[current_list->number][i] =
			  compress_voice ((max_loaded + 1) * song_char->nr_channels,
					  voice_table[current_list->number][i],
					  64, song_char->nr_channels - i);
		    }
		}
	      else if (current_list->type == 'I')
		{
		  catchup (*mod_fd, current_list->location, &file_position);
		  current_samp =
		    load_s3m_ins (*mod_fd, sample_inf +
				  (current_list->number * 0x50), &file_position);
		  if (current_samp != 0)
		    insert_in_list (&load_list, 'S', current_list->number,
				    current_samp);
		}
	      else
		{
		  insert_in_list(&sample_list, 'S', current_list->number,
				 current_list->location);
		  catchup (*mod_fd, current_list->location, &file_position);
		  i = samples[current_list->number]->
		    load(*seq, *mod_fd, current_list->number, cutFactor,
			 &loop_flags, sample_inf +
			 (current_list->number * 0x50));
		  
		  if (i < 0)
		    {
		      file_position -= i;
		      reloadSamples = 1;
		    }
		  else
		    file_position += i;
		}

	      (current_list->prev)->next = current_list->next;
	      (current_list->next)->prev = current_list->prev;

	      tmp_list = current_list->next;
	      free (current_list);
	      current_list = tmp_list;
	    }
	}

      if ((reloadSamples) && (cutFactor < MAX_CUT))
	{
	  seq->reset_samples();
	  reloadSamples = 0;
	  cutFactor++;
	  current_list = sample_list.next;

	  while (current_list != &sample_list)
	    {
	      insert_in_list(&load_list, 'S', current_list->number,
			     current_list->location);
	      tmp_list = current_list->next;
	      free(current_list);
	      current_list = tmp_list;
	    }

	  sample_list.prev = sample_list.next = &sample_list;
	}

      if (load_list.next != &load_list)
	{
	  if (command == NULL)	/* not compressed */
	    fseek (*mod_fd, 0, SEEK_SET);
	  else
	    {
	      pclose (*mod_fd);
	      *mod_fd = popen (command, "rb");
	    }
	  file_position = 0;
	}
    }
  while (load_list.next != &load_list);

  current_list = sample_list.next;
  
  while (current_list != &sample_list)
    {
      tmp_list = current_list->next;
      free(current_list);
      current_list = tmp_list;
    }

  free (sample_inf);

  return 1;
}
