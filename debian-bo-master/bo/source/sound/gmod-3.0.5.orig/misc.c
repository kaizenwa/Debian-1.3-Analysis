// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#include <sys/ioctl.h>
#include <stdlib.h>		/* for "free" */
#include <ctype.h>		/* for "isprint */
#include <unistd.h>		/* for "pause" */

#ifdef USE_LOCAL
#include "soundcard.h"
#else
#include <sys/soundcard.h>
#endif

#include <sys/ultrasound.h>

#include "defines.h"
#include "structs.h"
#include "globals.h"

#include "Sample.h"

int
panning (int ch)
{
  static int panning_tab[] =
    {0, 255, 255, 0};

  return panning_tab[ch % 4];
}

double last_sync;
double last_dump;

void
sync_time ()
{
  if (next_time > this_time)
    {
      SEQ_WAIT_TIME ((long) next_time);
      this_time = next_time;
    }
}

void
free_patterns ()
{
  int i;
  extern Sample *samples[];

  for (i = 0; i < MAX_PATTERN * MAX_TRACK; i++)
    if (pattern_table[i] != NULL)
    {
      free (pattern_table[i]);
      pattern_table[i] = NULL;
    }

  for (i = 0; i < MAX_SAMPLES; i++)
    if (samples[i])
      {
	delete samples[i];
	samples[i] = 0;
      }
}

void
stop_all_channels (unsigned char song_end)
{
  int j;

  GUS_NUMVOICES (gus_dev, 32);

  for (j = 0; j < 32; j++)
    {
      GUS_VOICEOFF (gus_dev, j);
      GUS_RAMPOFF (gus_dev, j);
    }

  if (song_end == MY_TRUE)
    SEQ_ECHO_BACK (ECHO_END);

  SEQ_DUMPBUF ();
}

void
remove_noprint (char *string)
{
  char *str2 = string;

  /* remove non-printable characters */

  while (*str2 != '\0')
    {
      if (!isprint (*str2))
	*str2 = '?';
      str2++;
    }

#if 0
  /* remove trailing blanks */

  str2--;

  while ((str2 >= string) && (*str2 == ' '))
    str2--;

  *(str2 + 1) = '\0';
#endif
}
