// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#ifndef s3m_h
#define s3m_h

#include "Sample.h"

class S3M_sample : public Sample
{
public:
  int load(Sequencer &, FILE *, int, int, void *, void *);
  int slide_rate(int, unsigned char) const;
};

inline int
S3M_sample::slide_rate(int rate, unsigned char slide_dir) const
{
   return ((int)(double(Sample::slide_rate(rate, slide_dir)) *
		 (_base_freq / 8363.0)));
}

#endif
