/***
 * CopyPolicy: GNU Public License 2 applies
 * Copyright (C) by Heiko Eissfeldt
 *
 *
 * ---------------------------------------------------------------------
 *  definitions for sun pcm output
 * ---------------------------------------------------------------------
 */

#include "byteorder.h"

typedef struct SUNHDR {
  DWORD magic;			/* dns. a la .snd */
  DWORD data_location;		/* offset to data */
  DWORD size;			/* # of data bytes */
  DWORD format;			/* format code */
  DWORD sample_rate;		/* in Hertz */
  DWORD channelcount;		/* 1 for mono, 2 for stereo */
  char info[8];			/* comments */
} SUNHDR;

/* Prototypes */
int InitSun ( int audio, long channels, unsigned long rate, long nBitsPerSample,
	     unsigned long expected_bytes);
int ExitSun ( int audio, unsigned long nBytesDone );
unsigned long GetSunHdrSize( void );


