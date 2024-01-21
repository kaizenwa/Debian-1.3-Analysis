// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#include <string.h>

#include "mtm.h"
#include "Sequencer.h"

void remove_noprint(char *);

int
MTM_sample::load (Sequencer &seq, FILE * mod_fd, int sample_no, int cutFactor,
		  void *header, void *)
{
#define MTM_ATTRIBUTE_16BIT 1
  int bytes_read;
  unsigned char *buf = (unsigned char *)header;

  _mode = WAVE_UNSIGNED;
  _ok = 0;
  memcpy(_name, buf, 22);
  _name[22] = '\0';
  remove_noprint(_name);

  _length = INTEL_LONG(buf + 22);
  loop_start = INTEL_LONG(buf + 26);
  loop_end = INTEL_LONG(buf + 30);
  _finetune = *(buf + 34) & 0x0f;

  if (_finetune <= 7)
    _finetune *= 12.5;
  else
    _finetune = (short)(12.5 * (_finetune - 16));

  _volume = *(buf + 35);

  if (_volume > 0)
    _volume = _volume * 4 - 1;

  if (*(buf + 36) & MTM_ATTRIBUTE_16BIT)
    _mode |= WAVE_16_BITS;

  /* found a mod with lots of samples with loop_ends of 2.  played fine
       * without them.  should I check that here?
       */
  if (loop_end > 2)
    _mode |= WAVE_LOOPING;
#if 0
  else
    loop_end = loop_start = 0;
#endif

  _base_note = C2FREQ;	/* was 261630 - Middle C */
  _base_freq = NTSC_RATE;

  if (_length > 0)
    _ok = !seq.patch_load(mod_fd, sample_no, *this, bytes_read, cutFactor);

  return bytes_read;
}
