#include <stdio.h>
#include <unistd.h>
#include "byteorder.h"
#include "uti.h"
#include "sun.h"

static SUNHDR sunHdr;


int InitSun ( int audio, long channels, unsigned long rate, long nBitsPerSample,
	     unsigned long expected_bytes)
{
  unsigned long format = nBitsPerSample > 8 ? 0x03 : 0x02;

  sunHdr.magic         = cpu_to_le32(0x646e732eUL);
  sunHdr.data_location = cpu_to_be32(0x20);
  sunHdr.size          = cpu_to_be32(expected_bytes);
  sunHdr.format        = cpu_to_be32(format);
  sunHdr.sample_rate   = cpu_to_be32(rate);
  sunHdr.channelcount  = cpu_to_be32(channels);

  return write (audio, &sunHdr, sizeof (sunHdr));
}

int ExitSun ( int audio, unsigned long nBytesDone )
{
  sunHdr.size = cpu_to_be32(nBytesDone);

  /* goto beginning */
  if (lseek(audio, 0L, SEEK_SET) == -1) {
    return 0;
  }
  return write (audio, &sunHdr, sizeof (sunHdr));
}

unsigned long GetSunHdrSize( void )
{
  return sizeof( sunHdr );
}
