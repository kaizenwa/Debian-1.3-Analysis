// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#include "ult.h"
#include "Sequencer.h"

void remove_noprint(char *);

int
ULT_sample::load(Sequencer &seq, FILE * mod_fd, int sample_no, int cutFactor,
		 void *header, void *version)
{
#define SAMP_16BIT      4
#define SAMP_LOOP       8
#define SAMP_REVERSE    16
  int bytes_read = 0;
  unsigned char *buf = (unsigned char *)header;
  int loop = 0;

  _mode = 0;

  memcpy(_name, buf, 32);
  _name[32] = 0;
  remove_noprint(_name);

  loop_start = INTEL_LONG (buf + 44);
  loop_end = INTEL_LONG (buf + 48);
  loop = BYTE(buf + 61);
  _volume = BYTE(buf + 60);

  if (*((int *)version) >= 4)
    {
      _base_freq = (unsigned short) (INTEL_SHORT (buf + 62));
      //s.finetune = (signed short)(INTEL_SHORT(buf + 64));
    }
  else
    {
      _base_freq = NTSC_RATE;
      //s.finetune = (signed short)(INTEL_SHORT(buf + 62));
    }

  /* calculate length in "samples" and turn it into bytes */
  _length = INTEL_LONG(buf + 56) - INTEL_LONG(buf + 52);

  if (loop & SAMP_16BIT)
    _length *= 2;

  if (loop & SAMP_16BIT)
    _mode |= WAVE_16_BITS;
  if (loop & SAMP_LOOP)
    _mode |= WAVE_LOOPING;
  if (loop & SAMP_REVERSE)
    _mode |= WAVE_BIDIR_LOOP;

  _base_note = C2FREQ;

 if (_length > 0)
   _ok = !seq.patch_load(mod_fd, sample_no, *this, bytes_read, cutFactor);
 else
   _ok = 0;

  return bytes_read;
}
