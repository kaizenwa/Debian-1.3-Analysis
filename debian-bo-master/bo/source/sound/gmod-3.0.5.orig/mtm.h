// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#ifndef mtm_h
#define mtm_h

#include "Sample.h"

class MTM_sample : public Sample
{
public:
  int load(Sequencer &, FILE *, int, int, void *, void *);
};

#endif
