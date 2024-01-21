/* MPEG/WAVE Sound library

   (C) 1997 by Jung woo-jae */

// Bitwindow.cc

// It's for MPEG layer 3

#include "mpegsound.h"

int Mpegbitwindow::getbits(int bits)
{
  char store[5];
  int *current;
  int bi;

  current=(int *)store;
  *((int *)(store+1))=0;

  if(bits)
  {
    bi=(bitindex&7);
    store[0]=buffer[(bitindex>>3)&(WINDOWSIZE-1)]<<bi;
    bi=8-bi;
    bitindex+=bi;

    while(bits)
    {
      if(!bi)
      {
	store[0]=buffer[(bitindex>>3)&(WINDOWSIZE-1)];
	bitindex+=8;
	bi=8;
      }

      if(bits>=bi)
      {
	(*current)<<=bi;
	bits-=bi;
	bi=0;
      }
      else
      {
	(*current)<<=bits;
	bi-=bits;
	bits=0;
      }
    }
    bitindex-=bi;
  }

  return *((int *)(store+1));
}
