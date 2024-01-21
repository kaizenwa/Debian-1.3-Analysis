// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#include <sys/types.h>

#include "defines.h"
#include "Envelope.h"

void
Envelope::load(int numPoints, int sustainPoint, int loopStartPoint,
	       int loopEndPoint, int typeFlags, unsigned short fadeout,
	       unsigned char *data)
{
  int i, prevX, prevY;
  unsigned char *dataPtr = data;

  fadeRate = fadeout;

  if (!(typeFlags & 0x01))
    {
      numSegments = 0;
      return;
    }

  sustainOn = typeFlags & 0x02;
  loopOn = typeFlags & 0x04;
  numSegments = numPoints - 1;

  if (envelope != 0)
    delete [] envelope;

  envelope = new EnvelopeSegment[numSegments];
  
  if (sustainOn)
    sustainX = INTEL_SHORT(dataPtr + (sustainPoint * 4));

  if (loopOn)
    {
      loopStartX = INTEL_SHORT(dataPtr + (loopStartPoint * 4));
      loopEndX = INTEL_SHORT(dataPtr + (loopEndPoint * 4));
    }

  prevX = INTEL_SHORT(dataPtr);
  dataPtr += 2;
  prevY = INTEL_SHORT(dataPtr);
  dataPtr += 2;

  for (i = 0; i < numSegments; i++)
    {
      envelope[i].startX = prevX;
      envelope[i].startY = prevY;
      prevX = envelope[i].endX = INTEL_SHORT(dataPtr);
      dataPtr += 2;
      prevY = envelope[i].endY = INTEL_SHORT(dataPtr);
      dataPtr += 2;

      envelope[i].m = double(envelope[i].endY - envelope[i].startY) /
	double(envelope[i].endX - envelope[i].startX);
      envelope[i].b = double(envelope[i].startY) -
	double(envelope[i].m * envelope[i].startX);
    }
}

int
Envelope::getY(int &x, unsigned short &fade, char inSustain) const
{
  int i;
  int y = -1;

  if (x >= 0)
    {
      if (numSegments == 0)
	{
	  y = 64;
	  x = 0;
	}
      else
	{
	  if ((sustainOn) && (inSustain == MY_TRUE) && (x > sustainX))
		x = sustainX;

	  if ((loopOn) && (x > loopEndX))
	    x = loopStartX;

	  for (i = 0; i < numSegments; i++)
	    {
	      if ((envelope[i].startX <= x) && (envelope[i].endX >= x))
		y = envelope[i].m * x + envelope[i].b;
	    }
	}

      x++;

      if ((inSustain == MY_FALSE) && (y >= 0))
	{
	  if (fade == 0)
	    y = -1;
	  else
	    {
	      y = (y * fade) / 65535;
	  
	      if (fadeRate <= fade)
		fade -= fadeRate;
	      else
		fade = 0;
	    }
	}
   }

  return (y);
}
