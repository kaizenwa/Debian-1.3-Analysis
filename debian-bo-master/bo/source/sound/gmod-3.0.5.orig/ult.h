// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#ifndef ult_h
#define ult_h

#include "Sample.h"

class ULT_sample : public Sample
{
public:
  int load(Sequencer &, FILE *, int, int, void *, void *);
};

#endif
