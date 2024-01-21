// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#ifndef __Envelope_h
#define __Envelope_h

struct EnvelopeSegment
{
  int startX, startY;
  int endX, endY;

  double m, b;
};

class Envelope
{
public:
  Envelope();
  ~Envelope();
  void load(int, int, int, int, int, unsigned short, unsigned char *);
  int getY(int &, unsigned short &, char) const;
  int enabled() const;

private:
  struct EnvelopeSegment *envelope;
  int numSegments;
  int sustainX;
  int loopOn;
  int sustainOn;
  int loopStartX;
  int loopEndX;
  unsigned short fadeRate;
};

inline
Envelope::Envelope() : envelope(0), numSegments(0), loopOn(0), sustainOn(0),
  fadeRate(0)
{
}

inline
Envelope::~Envelope()
{
  if (envelope != 0)
    delete [] envelope;
}

inline int
Envelope::enabled() const
{
  return ((numSegments != 0) || (fadeRate));
}

#endif
