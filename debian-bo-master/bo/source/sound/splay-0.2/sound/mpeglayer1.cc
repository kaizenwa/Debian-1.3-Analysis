/* MPEG/WAVE Sound library

   (C) 1997 by Jung woo-jae */

// Mpeglayer1.cc
// It's for MPEG Layer 1

#include "mpegsound.h"

// Mpeg layer 1
void Mpegtoraw::extractlayer1(void)
{
  REAL fraction[MAXCHANNEL][MAXSUBBAND];
  REAL scalefactor[MAXCHANNEL][MAXSUBBAND];

  int bitalloc[MAXCHANNEL][MAXSUBBAND],
      sample[MAXCHANNEL][MAXSUBBAND];

  register int i,j;
  int s=stereobound,l;


// Bitalloc
  for(i=0;i<s;i++)
  {
    bitalloc[LS][i]=getbits(4);
    bitalloc[RS][i]=getbits(4);
  }
  for(;i<MAXSUBBAND;i++)
    bitalloc[LS][i]=
    bitalloc[RS][i]=getbits(4);

// Scale index
  if(inputstereo)
    for(i=0;i<MAXSUBBAND;i++)
    {
      if(bitalloc[LS][i])scalefactor[LS][i]=scalefactorstable[getbits(6)];
      if(bitalloc[RS][i])scalefactor[RS][i]=scalefactorstable[getbits(6)];
    }
  else
    for(i=0;i<MAXSUBBAND;i++)
      if(bitalloc[LS][i])scalefactor[LS][i]=scalefactorstable[getbits(6)];

  for(l=0;l<SCALEBLOCK;l++)
  {
    // Sample
    for(i=0;i<s;i++)
    {
      if((j=bitalloc[LS][i]))sample[LS][i]=getbits(j+1);
      if((j=bitalloc[RS][i]))sample[RS][i]=getbits(j+1);
    }
    for(;i<MAXSUBBAND;i++)
      if((j=bitalloc[LS][i]))sample[LS][i]=sample[RS][i]=getbits(j+1);


    // Fraction
    if(outputstereo)
      for(i=0;i<MAXSUBBAND;i++)
      {
	if((j=bitalloc[LS][i]))
	  fraction[LS][i]=(REAL(sample[LS][i])*factortable[j]+offsettable[j])
			  *scalefactor[LS][i];
	else fraction[LS][i]=0.0;
	if((j=bitalloc[RS][i]))
	  fraction[RS][i]=(REAL(sample[RS][i])*factortable[j]+offsettable[j])
			  *scalefactor[RS][i];
	else fraction[RS][i]=0.0;
      }
    else
      for(i=0;i<MAXSUBBAND;i++)
	if((j=bitalloc[LS][i]))
	  fraction[LS][i]=(REAL(sample[LS][i])*factortable[j]+offsettable[j])
			  *scalefactor[LS][i];
	else fraction[LS][i]=0.0;

    subbandsynthesis(fraction[LS],fraction[RS]);
  }
}
