// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#include <sys/types.h>
#include <stdio.h>
#include <unistd.h>

#ifdef USE_LOCAL
#include "soundcard.h"
#else
#include <sys/soundcard.h>
#endif

#include "defines.h"
#include "structs.h"
#include "globals.h"

#ifdef USE_X
#include <stdlib.h>

#include "TrackShell.h"
#include "OptShell.h"
#include "TopShell.h"
#else
#include "CursesScr.h"
#endif

#include "Sequencer.h"

unsigned int
proc_input (void)
{
  unsigned int seq_input;
  extern Sequencer *seq;
  extern char played[MAX_POSITION];
#ifdef USE_X
  extern TrackShell *trackShell;
  extern OptShell *optShell;
  extern TopShell *topShell;
#else
  extern CursesScr *cursScreen;
#endif

  if (seq->read ((char *)(&seq_input)) == 4)
    {
      if ((seq_input & 0xff) == SEQ_ECHO)
	{
	  seq_input >>= 8;
	  switch (seq_input & 0xff)
	    {
	    case ECHO_MESSAGE:
	      actual_pos = (seq_input >> 16) & 0xff;
	      played[actual_pos] = 2;   // played
#ifdef USE_X
	      topShell->setPosition(actual_pos);
	      trackShell->updateTracker(tune[actual_pos], 
					pattern_len[tune[actual_pos]],
					voice_table, pattern_table);
#else
	      cursScreen->setPos(actual_pos, (seq_input >> 8) & 0xff);
#endif
	      break;
#ifdef USE_X
	    case ECHO_SPEED0:
	      if (optShell->speed0Checked() == FALSE)
		topShell->doNext(1);
	      break;
	    case ECHO_LOOP:
	      if (optShell->loopBreakChecked() == TRUE)
		topShell->doNext(1);
	      break;
	    case ECHO_PATTERN:
	      trackShell->setCurrent(seq_input >> 8);
	      break;
#endif
	    }
	}
      else
	seq_input = ECHO_NONE;
    }
  else
    seq_input = ECHO_NONE;

  return (seq_input);
}
