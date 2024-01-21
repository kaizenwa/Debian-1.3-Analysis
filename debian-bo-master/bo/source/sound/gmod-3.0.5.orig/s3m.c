// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#include <errno.h>
#include <string.h>

#include "s3m.h"
#include "Sequencer.h"

void remove_noprint(char *);

int
S3M_sample::load(Sequencer &seq, FILE * mod_fd, int sample_no, int cutFactor,
		 void *lf, void *bf)
{
  unsigned char *buffer = (unsigned char *)bf;
  int bytes_read;
  int rc;

  _ok = 0;
  _mode = *((unsigned int *)lf);
  _length = INTEL_LONG(&buffer[0x10]);

  if (_length == 0)
      return 0;

  loop_start = INTEL_LONG(&buffer[0x14]);
  loop_end = INTEL_LONG(&buffer[0x18]);
  _volume = buffer[0x1c];

  if (buffer[0x1f] & 0x01)
    _mode |= WAVE_LOOPING;

  if (buffer[0x1f] & 0x04)
    _mode |= WAVE_16_BITS;

  _base_freq = INTEL_LONG(&buffer[0x20]);

  strncpy(_name, (char *)&buffer[0x30], 28);
  _name[28] = '\0';
  remove_noprint(_name);
  _base_note = C2FREQ;	/* Middle C */

  if (_volume > 0)
    _volume = (_volume * 4) - 1;
  else
    _volume = 0;

  rc = seq.patch_load(mod_fd, sample_no, *this, bytes_read, cutFactor);

  if (!rc)
    _ok = 1;
  else if (rc == -ENOSPC)
    bytes_read = -bytes_read;

  return bytes_read;
}
