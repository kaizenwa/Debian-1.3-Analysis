// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#include <string.h>
#include <sys/types.h>

#ifdef USE_LOCAL
#include "soundcard.h"
#else
#include <sys/soundcard.h>
#endif

#include "Six69.h"
#include "Sequencer.h"

void remove_noprint(char *);

int
Six69_sample::load(Sequencer &seq, FILE * mod_fd, int sample_no, int cutFactor,
		   void *header, void *)
{
  int bytes_read;
  unsigned char *buf = (unsigned char *)header;

  _mode = WAVE_UNSIGNED;
  _length = INTEL_LONG(buf + 13);
  loop_start = INTEL_LONG(buf + 17);
  loop_end = INTEL_LONG(buf + 21);

  if (loop_end > _length)
    loop_end = 0;

#if 0
  else if (loop_end == _length)
    loop_end--;

  if (loop_end < loop_start)
    {
      loop_start = 0;
      loop_end = 0;
    }
#endif

  strncpy(_name, buf, 13);
  _name[13] = '\0';
  remove_noprint(_name);

#if 0
  if (loop_end == 0)
    loop_end = 1;
  if (loop_end >= _length)
    loop_end = 1;
#endif
  
  if (loop_end > 2)
    _mode |= WAVE_LOOPING;
  
  _base_freq = NTSC_RATE;
  _base_note = C2FREQ;

  if (_length > 0)
    _ok = !seq.patch_load(mod_fd, sample_no, *this, bytes_read, cutFactor);
  else
    _ok = 0;

  return bytes_read;
}
