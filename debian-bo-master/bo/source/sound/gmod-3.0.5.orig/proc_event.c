// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#ifndef USE_X

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

#include "defines.h"
#include "structs.h"
#include "globals.h"
#include "protos.h"

#include "Sequencer.h"

void
NoXProcessEvent ()
{
  fd_set read_fds, write_fds;
  static int song_finished = 0;
  static int write_enabled = 1;
  extern Sequencer *seq;

  FD_ZERO (&read_fds);
  FD_ZERO (&write_fds);
  FD_SET (0, &read_fds);	/* standard input */
  FD_SET (seq->seq_fd(), &read_fds);

  if (write_enabled)
    FD_SET (seq->seq_fd(), &write_fds);

  select (seq->seq_fd() + 1, &read_fds, &write_fds, NULL, NULL);
  
  if (FD_ISSET (seq->seq_fd(), &write_fds))
    {
      if (stop_flag)
	song_finished = 1;

      do
	{
	  while ((seq->buffer_size() < 1) && (!song_finished))
	    if ((song_finished = play_next_position()))
	      {
		sync_time ();
		stop_all_channels (MY_TRUE);
	      }
	}
      while ((seq->buffer_size() > 0) && (seq->write() != -1));

      if ((seq->buffer_size() == 0) && (song_finished))
	write_enabled = 0;
   }
  
  if (FD_ISSET (seq->seq_fd(), &read_fds))
    {
      seq_input = proc_input ();
      
      if (seq_input == ECHO_END)
	{
	  song_finished = 0;
	  write_enabled = 1;
	}
    }

  if (FD_ISSET (0, &read_fds))
    timer_handler (-1);
}
#endif
