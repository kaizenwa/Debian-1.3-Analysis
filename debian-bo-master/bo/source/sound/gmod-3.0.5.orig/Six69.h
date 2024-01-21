// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#ifndef Six69_h
#define Six69_h

#include "Sample.h"

class Six69_sample : public Sample
{
public:
  int load(Sequencer &, FILE *, int, int, void *, void *);
};

#endif
