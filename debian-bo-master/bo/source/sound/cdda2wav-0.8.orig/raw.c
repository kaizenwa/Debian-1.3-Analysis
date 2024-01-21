#include <unistd.h>
#include "uti.h"

int InitRaw ( int audio, long channels, unsigned long rate, long nBitsPerSample,
	     unsigned long expected_bytes)
{
  return 0;
}

int ExitRaw ( int audio, unsigned long nBytesDone )
{
  return 0;
}

unsigned long GetRawHdrSize( void )
{
  return 0UL;
}
