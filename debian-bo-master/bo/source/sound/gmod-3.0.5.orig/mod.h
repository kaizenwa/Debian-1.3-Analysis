// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#ifndef mod_h
#define mod_h

#include "Sample.h"

class MOD_sample : public Sample
{
public:
  int load(Sequencer &, FILE *, int, int, void *, void *);
};

#endif
