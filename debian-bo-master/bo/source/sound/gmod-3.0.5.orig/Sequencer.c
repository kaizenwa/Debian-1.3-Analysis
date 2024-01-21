// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#include <sys/types.h>
#include <errno.h>
#include <unistd.h>

#ifdef USE_LOCAL
#include "soundcard.h"
#else
#include <sys/soundcard.h>
#endif

#include "defines.h"
#include "structs.h"
#include "protos.h"
#include "globals.h"

#include "Sequencer.h"
#include <deque.h>
#include "out_buffer.h"

#ifdef USE_X
#include "TopShell.h"
#endif

Sequencer::Sequencer() : voices(0)
{
  buffer_list = new deque<struct out_buffer *>;
  song_finished = 0;
}

int
Sequencer::buffer_size() const
{
  return buffer_list->size();
}

int
Sequencer::open()
{
  int i, n;
  struct synth_info info;
  struct midi_info midi;

#ifndef OSS_GETVERSION
#define OSS_GETVERSION _IOR('M', 118, int)
#endif

  // Versions of the driver prior to 0x0307f0 may have a problem
  // when used with the GUS PnP, so attempt to work around the
  // problem

  if ((ioctl(seqfd, OSS_GETVERSION, &n) == -1) || (n < 0x0307f0))
    {					
      if ((seqfd = ::open("/dev/sequencer", O_WRONLY | O_NONBLOCK, 0)) == -1)
	return -1;

      if (ioctl(seqfd, SNDCTL_SEQ_NRMIDIS, &n) != -1)
	{
	  int midi_dev = -1;

	  for (i = 0; (i < n) && (midi_dev == -1); i++)
	    {
	      midi.device = i;

	      if ((ioctl(seqfd, SNDCTL_MIDI_INFO, &midi) != -1) &&
		  (midi.dev_type == SNDCARD_GUS))
		midi_dev = i;
	    }

	  ::close(seqfd);

	  if (midi_dev != -1)
	    {
	      char midi_name[12];
	      
	      sprintf(midi_name, "/dev/midi%02d", midi_dev);
	      ::open(midi_name, O_WRONLY | O_NONBLOCK, 0);
	    }
	}
    }

  if ((seqfd = ::open("/dev/sequencer", O_RDWR | O_NONBLOCK, 0)) == -1)
    return -1;

  if (ioctl(seqfd, SNDCTL_SEQ_NRSYNTHS, &n) == -1)
    return -2;

  gus_dev = -1;

  for (i = 0; (i < n) && (gus_dev == -1); i++)
    {
      info.device = i;

      if (ioctl(seqfd, SNDCTL_SYNTH_INFO, &info) == -1)
	return -3;

      if (info.synth_type == SYNTH_TYPE_SAMPLE
	  && info.synth_subtype == SAMPLE_TYPE_GUS)
	gus_dev = i;
    }

  if (gus_dev == -1)
    return -4;

#ifdef USE_X
  writeNotifier = new QSocketNotifier(seqfd, QSocketNotifier::Write);
  writeNotifier->setEnabled(FALSE);
  QObject::connect(writeNotifier, SIGNAL(activated(int)), this,
		   SLOT(writeReady()));
  readNotifier = new QSocketNotifier(seqfd, QSocketNotifier::Read);
  readNotifier->setEnabled(FALSE);
  QObject::connect(readNotifier, SIGNAL(activated(int)), this,
		   SLOT(readReady()));
#endif
  
  return (seqfd);
}

int
Sequencer::write()
{
  struct out_buffer *buffer_item;
  int result;

  buffer_item = buffer_list->front();
      
  if ((result = ::write(seqfd, buffer_item->buffer + buffer_item->offset, buffer_item->n_bytes)) == -1)
    {
      if (errno != EAGAIN)
	{
	  perror("write /dev/sequencer");
	  exit(ERR_SEQUENCER);
	}
    }
  else if (result != buffer_item->n_bytes)
    {
      buffer_item->n_bytes -= result;
      buffer_item->offset += result;
      fflush(stdout);
    }
  else
    {
      delete buffer_item;
      buffer_list->pop_front();
    }

  return (result);
}

void
Sequencer::dump()
{
  out_buffer *buffer_entry;

  if (_seqbufptr > 0)
    {
      buffer_entry = new out_buffer;
      memcpy(buffer_entry->buffer, _seqbuf, _seqbufptr);
      buffer_entry->n_bytes = _seqbufptr;
      buffer_entry->offset = 0;
      buffer_list->push_back(buffer_entry);
      _seqbufptr = 0;
    }
}

void
seqbuf_dump()
{
  extern Sequencer *seq;

  seq->dump();
}

void
Sequencer::force()
{
  int first_time = 1;

  while (buffer_list->size() != 0)
    {
      if (!first_time)
	sleep(1);
      else
	first_time = 0;

      write();
    }
}

void
Sequencer::flush_input() const
{
  char input[4];

  while (read(input) == 4);
}

void
Sequencer::stop_playback()
{
  struct out_buffer *buffer_item;

#ifdef USE_X
  writeEnabled(FALSE);
#endif

  ioctl(seqfd, SNDCTL_SEQ_RESET, 0);

  while (buffer_list->size() != 0)
    {
      buffer_item = buffer_list->front();
      delete buffer_item;
      buffer_list->pop_front();
    }
	      
  _seqbufptr = 0;
#ifdef USE_X
  stop_all_channels(MY_FALSE);
  SEQ_ECHO_BACK(ECHO_STOP);
#else
  stop_all_channels (MY_TRUE);
#endif
  force();
  ioctl(seqfd, SNDCTL_SEQ_SYNC, 0);
}

#ifdef USE_X
void
Sequencer::readReady()
{
  unsigned int seq_input;
  extern TopShell *topShell;

  while ((seq_input = proc_input()) != ECHO_NONE)
    {
      if (seq_input == ECHO_END)
	{
	  song_finished = 0;
	  topShell->doNext(1);
	}
      else if (seq_input == ECHO_STOP)
	song_finished = 0;
    }
}

void
Sequencer::writeReady()
{
  do
    {
      while ((buffer_size() < 1) && (!song_finished))
	if ((song_finished = play_next_position()))
	  {
	    sync_time ();
	    stop_all_channels (MY_TRUE);
	  }
    }
  while ((buffer_size() > 0) && (write() != -1));
  
  if ((buffer_size() == 0) && (song_finished))
    {
      writeEnabled(FALSE);
      song_finished = 0;
    }
}

void
Sequencer::readEnabled(bool status)
{
  readNotifier->setEnabled(status);
}

void
Sequencer::writeEnabled(bool status)
{
  writeNotifier->setEnabled(status);
}
#endif

void
Sequencer::numVoices(int num, int volType)
{
  if (voices)
    delete [] voices;

  numVoices_ = num;
  voices = new Voice[num];

  for (int i = 0; i < num; i++)
    {
      voices[i].channel(i);
      voices[i].volType(volType);
    }
}

void
Sequencer::mainVolume(int vol)
{
  for (int i = 0; i < numVoices_; i++)
    voices[i].mainVolume(vol);
}

void
Sequencer::globalVolSlide(int amt)
{
  for (int i = 0; i < numVoices_; i++)
    voices[i].globalVolSlide(amt);
}

void
Sequencer::panFactor(int factor)
{
  for (int i = 0; i < numVoices_; i++)
    voices[i].panFactor(factor);
}

#ifdef USE_X
#include "Sequencer.moc"
#endif
