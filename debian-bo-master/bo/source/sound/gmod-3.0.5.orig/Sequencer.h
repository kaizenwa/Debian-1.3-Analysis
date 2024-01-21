// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#ifndef _Sequencer_h
#define _Sequencer_h

#include <sys/ioctl.h>
#include <sys/soundcard.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>

#ifdef USE_X
#include <qapp.h>
#include <qsocknot.h>
#endif

#include "Voice.h"

struct out_buffer;
class Sample;
template<class T> class deque;

class Sequencer
#ifdef USE_X
  : public QObject
#endif
{
#ifdef USE_X
  Q_OBJECT
#endif

private:
  Voice *voices;
  int gus_dev;
  int seqfd;

public:
  Sequencer();
  ~Sequencer() { if (voices) delete [] voices; };

  int write();
  void dump();
  void force();
  void flush_input() const;
  void stop_playback();
  int buffer_size() const;
  int open();
  void sync() const;
  void reset_samples() const;
  void close() const { ::close(seqfd); };
  int memory() const;
  int patch_load(FILE *, int, const Sample &, int &, int) const;
  int read(char *) const;

  // The following functions provide access to the sequencer voices.  They
  // will be removed once the sequencer does the command processing.
  void numVoices(int, int);
  void volume(int chan, int vol) { voices[chan].volume(vol); };
  int volume(int chan) const { return voices[chan].volume(); };
  void pan(int chan, int panVal) { voices[chan].pan(panVal); };
  void panSlide(int chan, int amt) { voices[chan].panSlide(amt); };
  void keyOff(int chan) { voices[chan].keyOff(); };
  void mainVolume(int vol);
  void globalVolSlide(int amt);
  void note(int c, int n, int v) { voices[c].note(n, v); };
  void sample(int chan, const Sample *s) { voices[chan].sample(s); };
  void doUpdates(int chan) { voices[chan].doUpdates(gus_dev); };
  void noteDelay(int chan, int d) { voices[chan].noteDelay(d); };
  void tremolo(int chan, int amt) { voices[chan].tremolo(amt); };
  void tremoloWave(int chan, int wave) { voices[chan].tremoloWave(wave); };
  void tremor(int chan, int amt) { voices[chan].tremor(amt); };
  void doTick(int chan, int tick) { voices[chan].doTick(tick); };
  void resetEffects(int chan) { voices[chan].resetEffects(); };
  void setEnvelopePos(int c, int p) { voices[c].setEnvelopePos(p); };

  void panFactor(int factor);

  // The following functions will be removed after everything is in the class
  int seq_fd() const { return seqfd; };
  int gus_device() const { return gus_dev; };

#ifdef USE_X
  void readEnabled(bool);
  void writeEnabled(bool);
#endif

private:
  int song_finished;
  int numVoices_;

  // buffer_list is a pointer so deque.h doesn't need to be included
  deque<struct out_buffer *> *buffer_list;

#ifdef USE_X
  QSocketNotifier *writeNotifier;
  QSocketNotifier *readNotifier;

 private slots:
 void writeReady();
  void readReady();

#endif
};

inline int Sequencer::read(char *inbuf) const
{
  return (::read (seqfd, inbuf, 4));
}

inline void Sequencer::sync() const
{
  ioctl (seqfd, SNDCTL_SEQ_SYNC, 0);
}

inline void Sequencer::reset_samples() const
{
  ioctl (seqfd, SNDCTL_SEQ_RESETSAMPLES, &gus_dev);
}

inline int Sequencer::memory() const
{
  int memory;

  memory = gus_dev;
  ioctl (seqfd, SNDCTL_SYNTH_MEMAVL, &memory);

  return (memory);
}

#endif
