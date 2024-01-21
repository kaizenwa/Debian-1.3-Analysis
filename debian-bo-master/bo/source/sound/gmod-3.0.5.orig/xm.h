// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#ifndef xm_h
#define xm_h

#include "Sample.h"
#include "Envelope.h"

class XM_sample : public Sample
{
private:
  Envelope volumeEnvelope;
  Envelope panEnvelope;
  unsigned char panning_;

public:
  int load(Sequencer &, FILE *, int, int, void *, void *);
  int hasVolumeEnvelope() const { return volumeEnvelope.enabled(); };
  int hasPanEnvelope() const { return panEnvelope.enabled(); };
  int volumeEnvelopeY(int &a, unsigned short &b, char c) const
  { return volumeEnvelope.getY(a, b, c); };
  int panEnvelopeY(int &a, unsigned short &b, char c) const
  { return panEnvelope.getY(a, b, c); };
  void decode(char *) const;
  int pan() const { return panning_; };
};

#endif
