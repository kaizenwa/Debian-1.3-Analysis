// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#include <sys/ioctl.h>
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

#ifdef USE_X
#include "TopShell.h"
#include "SampleShell.h"
#include "CommentShell.h"
#else
#include "CursesScr.h"
#endif

#include "commands.h"
#include "defines.h"
#include "structs.h"
#include "globals.h"
#include "protos.h"

#include "Sequencer.h"
#include "mod.h"

/* prototype */

char *fix_name(char *the_string);
void catchup(FILE *, int, int *);

int
load_mod_module (FILE ** mod_fd, struct song_info *song_char,
		 struct options_info options, unsigned char *buffer,
		 char *command)
{
  extern Sample *samples[];
  extern Sequencer *seq;

  int i;
  int gus_onboard_mem;
  int sample_ptr, pattern_loc;
  int position;
  int voice;
  unsigned char *tune_ptr;	/* array 0-127 */
  unsigned char header[1084];
  int nr_samples;		/* 16 or 32 samples */
  int slen, npat;
  int pat_size, pat_tmp_size, offset;
  char mname[21];
  int bend;
  unsigned char is_flt8 = MY_FALSE;
  int cutFactor = 1;

  memcpy(header, buffer, HDR_SIZE);

  if (fread(header + HDR_SIZE, 1, sizeof(header) - HDR_SIZE, *mod_fd) !=
      sizeof(header) - HDR_SIZE)
    {
      /* Short header */
      return 0;
    }

  strncpy(mname, (char *)header, 20);
  mname[20] = '\0';
  remove_noprint(mname);
  strcpy(song_char->name, mname);

  if (!memcmp(&header[1080], "M.K.", 4) ||
      !memcmp(&header[1080], "M!K!", 4) ||
      !memcmp(&header[1080], "M&K!", 4))
    {
      nr_samples = 31;
      song_char->nr_channels = 4;
    }
  else if (!memcmp(&header[1080], "FLT", 3))
    {
      nr_samples = 31;
      song_char->nr_channels = 4;
      if (header[1083] == '8')
	is_flt8 = MY_TRUE;
    }
  else if (!memcmp(&header[1081], "CHN", 3))
    {
      nr_samples = 31;
      song_char->nr_channels = header[1080] - 48;
    }
  else if (!memcmp(&header[1080], "OCTA", 4))
    {
      nr_samples = 31;
      song_char->nr_channels = 8;
    }
  else if (!memcmp(&header[1082], "CH", 2))
    {
      nr_samples = 31;
      song_char->nr_channels = strtol(&header[1080], NULL, 10);
    }
  else
    {
      nr_samples = 15;
      song_char->nr_channels = 4;
    }

  if (options.extend_oct == OCTAVE_EXTEND)
    {
      song_char->lowest_note = NOTE_BASE;
      song_char->highest_note = NOTE_BASE + NUM_PERIODS - 1;
    }
  else
    {
      song_char->lowest_note = 48;
      song_char->highest_note = 83;
    }

  sprintf (song_char->desc, "MOD / %u channels", song_char->nr_channels);
  song_char->nr_samples = nr_samples;
  song_char->play_speed = 6;
  song_char->tempo = 125;
  song_char->vol_type = VOL_LINEAR;
  song_char->vol_on_zero = MY_FALSE;
  song_char->slide_type = SLIDE_PERIOD_LIN;

  if (options.use_50hz)
    song_char->clock_speed = 50;
  else
    song_char->clock_speed = 60;

  for (i = 0; i < song_char->nr_channels; i++)
    song_char->panning[i] = panning (i);

  pat_size = 64 * song_char->nr_channels * 4;

  if (nr_samples == 31)
    {
      sample_ptr = pattern_loc = 1084;
      slen = header[950];
      tune_ptr = &header[952];
    }
  else
    {
      sample_ptr = pattern_loc = 600;
      slen = header[470];
      tune_ptr = &header[472];
    }

  npat = 0;

  for (i = 0; i < 128; i++)
    {
      tune[i] = tune_ptr[i];

      if (tune_ptr[i] > npat)
	npat = tune_ptr[i];
    }

  npat++;

  if (is_flt8 == MY_TRUE)
    npat++;

  song_char->nr_tracks = npat * song_char->nr_channels;
  song_char->songlength = slen;
  song_char->nr_patterns = npat;

  gus_onboard_mem = seq->memory();

  for (position = 0; position < npat; position++)
    {
      unsigned char patterns[64][song_char->nr_channels][4];
      int pat, channel;

      int pp = pattern_loc + (position * pat_size);

      if (pp < sizeof(header))
	{
	  pat_tmp_size = sizeof (header) - pp;
	  if (pat_tmp_size > pat_size)
	    {
	      memcpy(patterns, &header[pp], pat_size);
	      pat_tmp_size = 0;
	    }
	  else
	    {
	      memcpy(patterns, &header[pp], pat_tmp_size);
	      offset = pat_tmp_size;
	      pat_tmp_size = pat_size - pat_tmp_size;
	    }
	}
      else
	{
	  pat_tmp_size = pat_size;
	  offset = 0;
	}

      if (pat_tmp_size > 0)
	if (fread ((char *) patterns + offset, 1, pat_tmp_size, *mod_fd) != pat_tmp_size)
	  {
	    /* Short file */
	    return 0;
	  }

      for (voice = 0; voice < song_char->nr_channels; voice++)
	if ((pattern_table[position * song_char->nr_channels + voice] = (struct note_info *) malloc (sizeof (struct note_info) * 64)) == NULL)
	  {
	    /* Can't allocate memory for a pattern */
	    return 0;
	  }

      for (pat = 0; pat < 64; pat++)
	{
	  for (channel = 0; channel < song_char->nr_channels; channel++)
	    {
	      unsigned short tmp;
	      unsigned char *p;

	      unsigned period, sample, effect, params, note;

	      p = &patterns[pat][channel][0];

	      tmp = (p[0] << 8) | p[1];
	      sample = (tmp >> 8) & 0x10;
	      period =
		MIN (tmp & 0xFFF, 1023);
	      tmp = (p[2] << 8) | p[3];
	      sample |= tmp >> 12;
	      effect = (tmp >> 8) & 0xF;
	      params = tmp & 0xFF;

	      if (effect == CMD_EXTENDED)
		{
		  effect = ((CMD_EXTENDED << 4) & 0xf0) +
		    ((params >> 4) & 0x0f);
		  params &= 0x0f;
		}

	      note = 0;

	      if (period)
		{
		  /* Convert period to a Midi note number */

		  period_to_note (period, (int *)&note, &bend);

		  if (((note < song_char->lowest_note) || 
		       (note > song_char->highest_note)) &&
		      (options.extend_oct == OCTAVE_AUTO))
		    {
		      song_char->lowest_note = NOTE_BASE;
		      song_char->highest_note = NOTE_BASE + NUM_PERIODS - 1;
		    }
		  
		  if (note < song_char->lowest_note)
		    note = song_char->lowest_note;
		  else if (note > song_char->highest_note)
		    note = song_char->highest_note;
		}

	      switch (effect)
		{
		case CMD_SPEED:
		  if (options.tolerant && (params == 0))
		    effect = 0;
		  else if (options.bpm_tempos)
		    effect = CVT_MOD_SPEED (params);
		  else
		    effect = CMD_SET_TICKS;
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

	      voice = position * song_char->nr_channels + channel;
	      voice_table[position][channel] = voice;

	      (pattern_table[voice])[pat].note = note;
	      (pattern_table[voice])[pat].sample = sample;
	      (pattern_table[voice])[pat].command[0] = effect;
	      (pattern_table[voice])[pat].parm1[0] = params;
	      (pattern_table[voice])[pat].parm2[0] = 0;
	      (pattern_table[voice])[pat].command[1] = 0;
	      (pattern_table[voice])[pat].parm1[1] = 0;
	      (pattern_table[voice])[pat].parm2[1] = 0;
	    }
	}

      if (options.compress)
	for (channel = 0; channel < song_char->nr_channels; channel++)
	  voice_table[position][channel] =
	    compress_voice (voice_table[position][channel],
			    voice_table[position][channel],
			    64, 1);
    }

  if (is_flt8 == MY_TRUE)
    {
      for (position = 0; position < npat; position += 2)
	for (i = 0; i < song_char->nr_channels; i++)
	  voice_table[position][i + 4] = voice_table[position + 1][i];

      song_char->nr_channels = 8;
    }

  sample_ptr += (npat * pat_size);	/* Location where the first sample is stored */
  //printf("Calculated %d, real %d\n", sample_ptr, ftell(*mod_fd));

  for (i = 0; i < nr_samples; i++)
    samples[i] = new MOD_sample;

  do {
    i = 0;

    while (i < nr_samples)
      {
	if ((samples[i]->load(*seq, *mod_fd, i, cutFactor,
			      &header[20 + (i * 30)], &options.ntsc) ==
	     -ENOSPC) && (cutFactor < MAX_CUT))
	  {
	    cutFactor++;
	    i = nr_samples + 1;
	    seq->reset_samples();
	    
	    if (!command)
	      fseek(*mod_fd, sample_ptr, SEEK_SET);
	    else
	      {
		pclose(*mod_fd);
		*mod_fd = popen(command, "rb");
		slen = 0;    // reusing slen here!
		catchup(*mod_fd, sample_ptr, &slen);
	      }
	  }
	else
	  i++;
      }
  } while (i != nr_samples);

  if (gus_onboard_mem == seq->memory())
      return 0;

  return 1;
}

int
load_module(char *name, struct song_info *song_char,
	    struct options_info options)
{
  extern Sample *samples[];
#ifndef USE_X
  extern CursesScr *cursScreen;
#endif
  int i;

  unsigned char header[HDR_SIZE];
  char *command = NULL;

  int ret_val;
  int gus_onboard_mem;

  FILE *mod_fd;

  char compressed = MY_FALSE;

#ifdef USE_X
  extern TopShell *topShell;
  extern SampleShell *sampleShell;
  extern CommentShell *commentShell;
  char *slashpos, tmp_char = '\0';
#endif

  /* added by Peter Federighi */
  char *fixedname = NULL;

  extern Sequencer *seq;

  song_char->type = MODULE_NOT_S3M;
  song_char->global_vol = 255;
  song_char->name[0] = '\0';
  song_char->desc[0] = '\0';
  song_char->comment[0] = '\0';

  seq->sync();
  seq->reset_samples();

  gus_onboard_mem = seq->memory();

  for (i = 0; i < MAX_POSITION; i++)
    pattern_len[i] = 64;

  for (i = 0; i < MAX_POSITION; i++)
    pattern_tempo[i] = 0;

  for (i = 0; i < MAX_PATTERN * MAX_TRACK; i++)
    pattern_table[i] = NULL;

#ifndef USE_X
  cursScreen->setFile(name);
#else
  if ((slashpos = strrchr(name, '/')) == NULL)
    slashpos = name;
  else
    slashpos++;

  if (strlen(slashpos) > 33)
    {
      tmp_char = slashpos[33];
      slashpos[33] = '\0';
    }

  topShell->moduleFile(slashpos);

  if (tmp_char != '\0')
    slashpos[33] = tmp_char;
#endif

  if (!(mod_fd = fopen(name, "rb")))
    return 0;

  if (fread(header, 1, sizeof (header), mod_fd) != sizeof (header))
    {
      fclose(mod_fd);
      return 0;
    }

  if ((header[0] == 31) && ((header[1] == 139) || (header[1] == 157)))
    compressed = GCOMPRESSED;
  else if ((header[2] == '-') && (header[3] == 'l') &&
	   ((header[4] == 'h') || (header[4] == 'z')) &&
	   (header[6] == '-'))
    compressed = LHACOMPRESSED;
  else if ((header[0] == 'P') && (header[1] == 'K') && (header[2] == 3) &&
	   (header[3] == 4))
    compressed = ZIPCOMPRESSED;

  if (compressed)
    {
      /* modified by Peter Federighi */
      fixedname = fix_name (name);

      switch (compressed)
	{
	case GCOMPRESSED:
	  command = (char *) malloc (strlen (fixedname) +
				     strlen (GDECOMP_PGM) + 1);
	  sprintf (command, "%s%s", GDECOMP_PGM, fixedname);
	  break;
	case LHACOMPRESSED:
	  command = (char *) malloc (strlen (fixedname) +
				     strlen (LHADECOMP_PGM) + 1);
	  sprintf (command, "%s%s", LHADECOMP_PGM, fixedname);
	  break;
	case ZIPCOMPRESSED:
	  command = (char *) malloc (strlen (fixedname) +
				     strlen (ZIPDECOMP_PGM) + 1);
	  sprintf (command, "%s%s", ZIPDECOMP_PGM, fixedname);
	  break;
	default:
	  break;
	}

      fclose(mod_fd);
      mod_fd = popen (command, "rb");

      if (!mod_fd)
	{
	  free(command);
	  free(fixedname);
	  return 0;
	}

      if (fread(header, 1, sizeof(header), mod_fd) != sizeof(header))
	{
	  free(command);
	  free(fixedname);
	  return 0;
	}
    }

  if ((*(unsigned short *) &header[0] == 0x6669) ||
      !memcmp(header, "JN", 2))
    ret_val = load_669_module(mod_fd, song_char, options, header);

  else if (!memcmp(header, "MTM", 3))
    ret_val = load_mtm_module(mod_fd, song_char, header);

  else if (!memcmp(header, "MAS_UTrack_V", 12))
    ret_val = load_ult_module(mod_fd, song_char, options, header);
  else if (!memcmp(&header[0x2c], "SCRM", 4))
    {
      song_char->type = MODULE_S3M;
      ret_val = load_s3m_module(&mod_fd, song_char, options, header, command);
    }
  else if (!memcmp(header, "Extended Module: ", 17))
    ret_val = load_xm_module(&mod_fd, song_char, options, header, command);
  else
    ret_val = load_mod_module(&mod_fd, song_char, options, header, command);

  if (compressed == MY_FALSE)
    fclose (mod_fd);
  else
    {
      free (command);
      free (fixedname);		/* added by Peter Federighi */
      pclose (mod_fd);
    }

  if (!ret_val)
    free_patterns ();
  else
    {
#ifdef USE_X
      topShell->moduleTitle(song_char->name);
      topShell->setMaxPosition(song_char->songlength);
      topShell->setChannels(song_char->nr_channels);
      commentShell->setComment(song_char->comment);
      sampleShell->setSamples(samples, song_char->nr_samples);
#else
      cursScreen->setInfo(song_char->name, song_char->desc,
			  song_char->comment, song_char->nr_channels,
			  song_char->nr_patterns, song_char->songlength);
      cursScreen->setSamples(samples,
			     song_char->nr_samples,
			     options.show_empty_samples);
      cursScreen->setMem(seq->memory(), gus_onboard_mem);
#endif
    }

  return (ret_val);
}

/*
 * Added by Peter Federighi (allanon@u.washington.edu) to fix names so they can
 * be parsed by 'sh' properly for the 'popen' calls.
 */
char *
fix_name (char *the_string)
{
  short counter, numinvalid = 0, pos = 0, length = strlen (the_string);
  char *new_name;
  char *valid_char = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    "abcdefghijklmnopqrstuvwxyz"
      "1234567890-_=+.,/\0";

  if (length == 0)
    return NULL;

  /* First, find out how many invalid characters there are */
  for (counter = 0; counter < length; counter++)
    {
      if (strchr (valid_char, the_string[counter]) == NULL)
	numinvalid += 1;
    }

  /* malloc memory for new string, leave it to the user to free() it*/
  new_name = (char *)malloc (length + numinvalid + 1);

  /* create new string, valid for command line parsing */
  for (counter = 0; counter < length; counter++)
    {
      if (strchr (valid_char, the_string[counter]) == NULL)
	{
	  new_name[pos] = '\\';
	  new_name[pos + 1] = the_string[counter];
	  pos += 2;
	}
      else
	{
	  new_name[pos] = the_string[counter];
	  pos++;
	}
    }
  /* either do this or make the for loop go to 'counter <= length' */
  new_name[length + numinvalid] = '\0';
  return new_name;
}
