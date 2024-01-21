// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#include <errno.h>
#include <string.h>

#include "mod.h"
#include "Sequencer.h"

void remove_noprint(char *);

int
MOD_sample::load(Sequencer &seq, FILE * mod_fd, int sample_no, int cutFactor,
		 void *header, void *pf)
{
  int bytes_read = 0;
  int rc = -EINVAL;
  unsigned char ntscflag = *((unsigned char *)pf);
  unsigned char *buf = (unsigned char *)header;

  _length = REV_SHORT(buf + 22) * 2;
  loop_start = REV_SHORT(buf + 26) * 2;
  loop_end = loop_start + (REV_SHORT(buf + 28) * 2);

#if 0
  if (loop_start > _length)
    loop_start = 0;

  if (loop_end <= loop_start)
    loop_end = loop_start + 2;
#endif
  _ok = 0;

  if (loop_end > 2)
    _mode = WAVE_LOOPING;
  else
    _mode = 0;

  strncpy(_name, buf, 22);
  _name[22] = '\0';
  remove_noprint(_name);
  
  _base_note = C2FREQ;	/* Middle C */
  _base_freq = ((ntscflag == 1) ? NTSC_RATE : PAL_RATE);
  _volume = BYTE(buf + 25);

  if (_volume > 0)
    _volume = (_volume * 4) - 1;
  else
    _volume = 0;
  
  _finetune = BYTE(buf + 24) & 0x0f;
  
  if (_finetune <= 7)
    _finetune *= 12.5;
  else
    _finetune = (short)(12.5 * (_finetune - 16));
  
  if (_length > 0)
    rc = seq.patch_load(mod_fd, sample_no, *this, bytes_read, cutFactor);

  if (!rc)
    _ok = 1;
    
  return rc;
}
