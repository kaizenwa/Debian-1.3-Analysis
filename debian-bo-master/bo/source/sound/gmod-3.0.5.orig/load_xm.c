// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#ifdef USE_LOCAL
#include "soundcard.h"
#else
#include <sys/soundcard.h>
#endif

#include "defines.h"
#include "structs.h"
#include "globals.h"
#include "protos.h"
#include "commands.h"

#include "Sequencer.h"
#include "xm.h"

void catchup (FILE *, int, int *);

static unsigned char xm_effect_tab[] =
{CMD_ARPEG, CMD_SLIDEUP, CMD_SLIDEDOWN, CMD_SLIDETO, /* 0 1 2 3 */
 CMD_VIBRATO, CMD_PORTANDVOL, CMD_VIBRAANDVOL, CMD_TREMOLO, /* 4 5 6 7 */
 CMD_SET_PAN, CMD_SETOFFSET, CMD_VOLSLIDE, CMD_JUMP, /* 8 9 A B */
 CMD_VOLUME, CMD_BREAK, CMD_EXTENDED, CMD_SPEED, /* C D E F */
 CMD_GLOBAL_VOL, CMD_GLOBALVOL_SLIDE, CMD_INFO, CMD_INFO, /* G H I J */
 CMD_KEYOFF, CMD_SETENV_POS, CMD_INFO, CMD_INFO, /* K L M N */
 CMD_INFO, CMD_PANSLIDE, CMD_INFO, CMD_RETRIGVOL, /* O P Q R */
 CMD_INFO, CMD_TREMOR, CMD_INFO, CMD_INFO, /* S T U V */
 CMD_INFO, CMD_XFINEPORTUP, CMD_INFO, CMD_INFO}; /* W X Y Z */

void
load_xm_pattern (FILE * mod_fd, int pat_no, struct song_info *song_char,
		 int *file_pos, unsigned char compress, u_short num_bytes)
{
  int i, j, voice;
  int has_note, has_instr, has_volume, has_command, has_info, index;
  u_char note, instrument, volume, command, info, vol_command;
  u_char buffer[100];

  for (i = 0; i < song_char->nr_channels; i++)
    {
      voice_table[pat_no][i] = (pat_no * song_char->nr_channels) + i;
      pattern_table[voice_table[pat_no][i]] =
	(struct note_info *) calloc (1, sizeof (struct note_info) * pattern_len[pat_no]);
    }

  if (num_bytes > 0)
    {
      for (j = 0; j < pattern_len[pat_no]; j++)
	{
	  for (i = 0; i < song_char->nr_channels; i++)
	    {
	      has_note = 0;
	      has_instr = 0;
	      has_volume = 0;
	      has_command = 0;
	      has_info = 0;
	      info = 0;
	      index = 0;

	      voice = voice_table[pat_no][i];
	      note = instrument = volume = command = info = 0;
	      
	      fread (buffer, 1, 1, mod_fd);
	      *file_pos += 1;
	      
	      if ((buffer[0] & 0x80) > 0)
		{
		  if (buffer[0] & 0x01)
		    has_note = 1;
		  
		  if (buffer[0] & 0x02)
		    has_instr = 1;
		  
		  if (buffer[0] & 0x04)
		    has_volume = 1;
		  
		  if (buffer[0] & 0x08)
		    has_command = 1;
		  
		  if (buffer[0] & 0x10)
		    has_info = 1;
		  
		  fread (buffer, 1, has_note + has_instr + has_volume +
			 has_command + has_info, mod_fd);
		  
		  *file_pos += (has_note + has_instr + has_volume +
				has_command + has_info);
		}
	      else
		{
		  fread (buffer + 1, 1, 4, mod_fd);
		  *file_pos += 4;
		  has_note = has_instr = has_volume = has_command = has_info = 1;
		}
	      
	      if (has_note)
		{
		  note = buffer[index++] + 11;
		  if (note >= 108)
		    note = NOTE_STOP;
		}

	      if (has_instr)
		instrument = buffer[index++];
	      
	      if (has_volume)
		{
		  volume = buffer[index++];
		  
		  switch (volume & 0xf0)
		    {
		    case 0x10: /* set volume */
		    case 0x20:
		    case 0x30:
		    case 0x40:
		    case 0x50:
		      if (volume <= 0x50)
			{
			  volume -= 0x10;
			  
			  if (volume > 0)
			    volume = volume * 4 - 1;
			  vol_command = CMD_VOLUME;
			}
		      else
			has_volume = 0;
		      break;
		    case 0x60: /* volslide down */
		      volume &= 0x0f;
		      vol_command = CMD_VOLSLIDE;
		      break;
		    case 0x70: /* volslide up */
		      volume = (volume << 4) & 0xf0;
		      vol_command = CMD_VOLSLIDE;
		      break;
		    case 0x80: /* fine volside down */
		      volume &= 0x0f;
		      vol_command = CMD_FINEVOLDOWN;
		      break;
		    case 0x90: /* fine volside up */
		      volume &= 0x0f;
		      vol_command = CMD_FINEVOLUP;
		      break;
		    case 0xa0: /* vibrato speed */
		      volume &= 0x0f;
		      vol_command = CMD_VIBRASPEED;
		      break;
		    case 0xb0: /* vibrato */
		      volume &= 0x0f;
		      vol_command = CMD_VIBRATO;
		      break;
		    case 0xc0: /* set panning */
		      volume = (volume & 0x0f) * 17;
		      vol_command = CMD_SET_PAN;
		      break;
		    case 0xd0: // panning slide left
		      volume &= 0x0f;
		      vol_command = CMD_PANSLIDE;
		      break;
		    case 0xe0: // panning slide right
		      volume = (volume << 4) & 0xf0;
		      vol_command = CMD_PANSLIDE;
		      break;
		    case 0xf0: /* tone portamento */
		      volume &= 0x0f;
		      vol_command = CMD_SLIDETO;
		      break;
		    default: /* unsupported */
		      has_volume = 0;
		    }
		}

	      if (has_command)
		{
		  command = buffer[index++];
		  
		  if (command < 36)
		    command = xm_effect_tab[command];
		  else
		    command = CMD_INFO;
		}

	      if (has_info)
		info = buffer[index++];

	      switch (command)
		{
		  //case CMD_SET_PAN:
		  //info = (info >> 4) & 0x0f;
		  //break;
		case CMD_SPEED:
		  command = CVT_MOD_SPEED (info);
		  break;
		case CMD_VOLUME:
		  if (info > 0)
		    info = (info * 4) - 1;
		  break;
		case CMD_BREAK:
		  info = ((info >> 4) & 0x0f) * 10 + (info & 0x0f);
		  break;
		case CMD_EXTENDED:
		  command = ((CMD_EXTENDED << 4) & 0xf0) + ((info >> 4) & 0x0f);
		  info &= 0x0f;
		  break;
		case CMD_KEYOFF: 
		  note = NOTE_STOP;
		  if (has_info)
		    command = CMD_INFO;
		  else
		    command = 0;
		  break;
		case CMD_XFINEPORTUP:
		  if ((info & 0xf0) == 0x20)
		    command = CMD_XFINEPORTDOWN;
		  info &= 0x0f;
		}

	      (pattern_table[voice])[j].note = note;
	      (pattern_table[voice])[j].sample = instrument;
	      
	      if (has_volume)
		{
		  (pattern_table[voice])[j].command[0] = vol_command;
		  (pattern_table[voice])[j].parm1[0] = volume;
		}

	      (pattern_table[voice])[j].command[1] = command;
	      (pattern_table[voice])[j].parm1[1] = info;
	    }
	}
    }
  
  if (compress)
    {
      for (i = 0; i < song_char->nr_channels; i++)
	{
	  for (j = 0; (j < pat_no) && (voice_table[pat_no][i] == (pat_no * song_char->nr_channels + i)); j++)
	    if (pattern_len[j] == pattern_len[pat_no])
	      voice_table[pat_no][i] =
		compress_voice ((j + 1) * song_char->nr_channels,
				voice_table[pat_no][i], pattern_len[pat_no], 1,
				j * song_char->nr_channels);
	  
	  if (voice_table[pat_no][i] == (pat_no * song_char->nr_channels + i))
	    voice_table[pat_no][i] = compress_voice(voice_table[pat_no][i],
						    voice_table[pat_no][i],
						    pattern_len[pat_no], 1,
						    (pat_no * song_char->nr_channels));
	}
    }
}

void
load_xm_samples(FILE ** mod_fd, int nr_samples, int *file_pos, char *command)
{
  extern Sample *samples[];
  extern Sequencer *seq;
  int cutFactor = 1;
  int i;
  int oldPos = *file_pos;

  for (i = 0; i < nr_samples; i++)
    samples[i] = new XM_sample;

  do {
    i = 0;

    while (i < nr_samples)
      {
	if ((samples[i]->load(*seq, *mod_fd, i, cutFactor, period_table,
			      file_pos) < 0) && (cutFactor < MAX_CUT))
	  {
	    cutFactor++;
	    i = nr_samples + 1;   // cause repeat of outer loop
	    
	    seq->reset_samples();

	    if (!command)
	      {
		fseek(*mod_fd, oldPos, SEEK_SET);
		*file_pos = oldPos;
	      }
	    else
	      {
		pclose(*mod_fd);
		*mod_fd = popen(command, "rb");
		*file_pos = 0;
		catchup(*mod_fd, oldPos, file_pos);
	      }
	  }
	else
	  i++;
      }
  } while (i != nr_samples);
}

int
load_xm_module (FILE ** mod_fd, struct song_info *song_char,
		struct options_info options, unsigned char *buffer,
		char *command)
{
  unsigned char header[336];
  char mname[21];
  int i;
  unsigned int maxpat;
  long header_size;
  int file_pos = 336, old_pos;

  memcpy (header, buffer, HDR_SIZE);

  if (fread (header + HDR_SIZE, 1, sizeof (header) - HDR_SIZE, *mod_fd) !=
      sizeof (header) - HDR_SIZE)
    return 0;

  strncpy (mname, (char *)(header + 17), 20);
  mname[20] = '\0';
  remove_noprint (mname);

  strcpy (song_char->name, mname);
  song_char->nr_channels = INTEL_SHORT(header + 68);
  song_char->lowest_note = 11;
  song_char->highest_note = 107;
  song_char->nr_samples = INTEL_SHORT(header + 72);
  song_char->play_speed = INTEL_SHORT(header + 76);
  song_char->tempo = INTEL_SHORT(header + 78);
  song_char->vol_type = VOL_LINEAR;
  song_char->vol_on_zero = MY_FALSE;
  song_char->clock_speed = 60;
  song_char->type = MODULE_XM;
  sprintf(song_char->desc, "XM / %u channels", song_char->nr_channels);

  if (INTEL_SHORT(header + 74) & 0x01)
    song_char->slide_type = SLIDE_NOTE_LIN;
  else
    song_char->slide_type = SLIDE_PERIOD_LIN;

  for (i = 0; i < song_char->nr_channels; i++)
    song_char->panning[i] = 128;

  maxpat = song_char->nr_patterns = INTEL_SHORT(header + 70);
  for (i = 0; i < 256; i++)
    {
      tune[i] = BYTE(header + 80 + i);

      if (tune[i] >= maxpat)
	{
	  maxpat = tune[i] + 1;
	  load_xm_pattern(*mod_fd, tune[i], song_char, &file_pos,
			  options.compress, 0);
	}
    }

  song_char->nr_tracks = song_char->nr_patterns * song_char->nr_channels;
  song_char->songlength = INTEL_SHORT(header + 64);

  header_size = INTEL_LONG(header + 60);

  catchup (*mod_fd, file_pos + (header_size - 276), &file_pos);

  for (i = 0; i < song_char->nr_patterns; i++)
    {
      fread (header, 1, 9, *mod_fd);
      file_pos += 9;
      header_size = INTEL_LONG(header);
      catchup (*mod_fd, file_pos + (header_size - 9), &file_pos);
      pattern_len[i] = INTEL_SHORT(header + 5);
      old_pos = file_pos;
      load_xm_pattern (*mod_fd, i, song_char, &file_pos, options.compress,
		       INTEL_SHORT(header + 7));
      catchup (*mod_fd, old_pos + INTEL_SHORT(header + 7), &file_pos);
    }

  song_char->nr_patterns = maxpat;

  load_xm_samples(mod_fd, song_char->nr_samples, &file_pos, command);

  return 1;
}
