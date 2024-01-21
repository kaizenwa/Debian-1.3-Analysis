// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#ifdef USE_LOCAL
#include "soundcard.h"
#else
#include <sys/soundcard.h>
#endif

#include "Sample.h"

void
Sample::get_patch_info(struct patch_info &patch) const
{
  patch.mode = _mode;
  patch.len = _length;
  patch.loop_start = loop_start;
  patch.loop_end = loop_end;
  patch.base_note = _base_note;  /* set real base note here */
  patch.base_freq = _base_freq;
}
