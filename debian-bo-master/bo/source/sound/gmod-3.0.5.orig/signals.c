// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#ifndef USE_X

#include <sys/ioctl.h>
#include <signal.h>
#include <stdio.h>
#include <sys/time.h>

#ifdef USE_LOCAL
#include "soundcard.h"
#else
#include <sys/soundcard.h>
#endif

#include <sys/types.h>
#include <unistd.h>

#include "CursesScr.h"

#include "defines.h"
#include "structs.h"
#include "globals.h"
#include "protos.h"

#include "Sequencer.h"

void
timer_handler (int sig)
{
  extern CursesScr *cursScreen;

  int keypress = EOF;
  unsigned int volume;
  int tmp_volume;
  static int reset_arg = MY_FALSE;
  /*
     char message[80];
     */
  static unsigned int keyed_arg = 0;

  extern Sequencer *seq;

  if (sig != -1)
    signal (sig, timer_handler);

  switch (sig)
    {
    case -1:
      keypress = cursScreen->getChar();
      break;
    case SIGTERM:
      keypress = 'q';
      break;
    case SIGUSR1:
      keypress = 'p';
      break;
    case SIGUSR2:
      keypress = 'n';
      break;
    }

  while (keypress != EOF)
    {
      switch (keypress)
	{
#ifdef USE_NCURSES
	case KEY_LEFT:
#endif
	case 'b':
	  if (keyed_arg == 0)
	    position_change--;
	  else
	    position_change -= keyed_arg;
	  reset_arg = MY_TRUE;
	  stop_flag = STOP_FORWBACK;
	  seq->stop_playback ();
	  break;
#ifdef USE_NCURSES
	case KEY_RIGHT:
#endif
	case 'f':
	  if (keyed_arg == 0)
	    position_change++;
	  else
	    position_change += keyed_arg;
	  reset_arg = MY_TRUE;
	  stop_flag = STOP_FORWBACK;
	  seq->stop_playback ();
	  break;
#ifdef USE_NCURSES
	case KEY_NPAGE:
#endif
	case 'n':
	  stop_flag = STOP_NEXT;
	  seq->stop_playback ();
	  break;
#ifdef USE_NCURSES
	case KEY_PPAGE:
#endif
	case 'p':
	  stop_flag = STOP_PREV;
	  seq->stop_playback ();
	  break;
	case 'q':
	  stop_flag = STOP_EXIT;
	  seq->stop_playback ();
	  break;
	  //#ifdef USE_NCURSES
	  //	case KEY_UP:
	  //#endif
	case '+':
	  if (mixerfd != -1)
	    {
	      ioctl (mixerfd, MIXER_READ (SOUND_MIXER_SYNTH), &volume);
	      volume &= 0xff;
	      if (keyed_arg == 0)
		tmp_volume = (int) volume + 1;
	      else
		tmp_volume = (int) volume + keyed_arg;
	      if (tmp_volume > MIXER_MAX_VOL)
		tmp_volume = MIXER_MAX_VOL;
	      volume = tmp_volume * 256 + tmp_volume;
	      ioctl (mixerfd, MIXER_WRITE (SOUND_MIXER_SYNTH), &volume);
	    }
	  reset_arg = MY_TRUE;
	  break;
	  //#ifdef USE_NCURSES
	  //	case KEY_DOWN:
	  //#endif
	case '-':
	  if (mixerfd != -1)
	    {
	      ioctl (mixerfd, MIXER_READ (SOUND_MIXER_SYNTH), &volume);
	      volume &= 0xff;
	      if (keyed_arg == 0)
		tmp_volume = (int) volume - 1;
	      else
		tmp_volume = (int) volume - keyed_arg;
	      if (tmp_volume < MIXER_MIN_VOL)
		tmp_volume = MIXER_MIN_VOL;
	      volume = tmp_volume * 256 + tmp_volume;
	      ioctl (mixerfd, MIXER_WRITE (SOUND_MIXER_SYNTH), &volume);
	    }
	  reset_arg = MY_TRUE;
	  break;
#ifdef USE_NCURSES
	case KEY_UP:
	  cursScreen->scrollSamples(-1);
	  break;
	case KEY_DOWN:
	  cursScreen->scrollSamples(1);
	  break;
#endif
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
	  if (reset_arg == MY_TRUE)
	    {
	      reset_arg = MY_FALSE;
	      keyed_arg = 0;
	    }
	  keyed_arg = (keyed_arg * 10) + (keypress - '0');
	  break;
	}

      keypress = cursScreen->getChar();
    }
}

void
set_signals (void)
{
  signal (SIGTERM, timer_handler);
  signal (SIGUSR1, timer_handler);
  signal (SIGUSR2, timer_handler);
}

#endif /* USE_X */
