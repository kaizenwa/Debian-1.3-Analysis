#include <stdio.h>
#include <unistd.h>
#include "uti.h"
#include "wav.h"

static WAVEHDR waveHdr;

int InitWav ( int audio, long channels, unsigned long rate, long nBitsPerSample,
	     unsigned long expected_bytes)
{
  unsigned long nBlockAlign = channels * ((nBitsPerSample + 7) / 8);
  unsigned long nAvgBytesPerSec = nBlockAlign * rate;
  unsigned long temp = expected_bytes + sizeof(WAVEHDR) - sizeof(CHUNKHDR);

  waveHdr.chkRiff.ckid    = cpu_to_le32(FOURCC_RIFF);
  waveHdr.fccWave         = cpu_to_le32(FOURCC_WAVE);
  waveHdr.chkFmt.ckid     = cpu_to_le32(FOURCC_FMT);
  waveHdr.chkFmt.dwSize   = cpu_to_le32(sizeof (PCMWAVEFORMAT));
  waveHdr.wFormatTag      = cpu_to_le16(WAVE_FORMAT_PCM);
  waveHdr.nChannels       = cpu_to_le16(channels);
  waveHdr.nSamplesPerSec  = cpu_to_le32(rate);
  waveHdr.nBlockAlign     = cpu_to_le16(nBlockAlign);
  waveHdr.nAvgBytesPerSec = cpu_to_le32(nAvgBytesPerSec);
  waveHdr.wBitsPerSample  = cpu_to_le16(nBitsPerSample);
  waveHdr.chkData.ckid    = cpu_to_le32(FOURCC_DATA);
  waveHdr.chkRiff.dwSize  = cpu_to_le32(temp);
  waveHdr.chkData.dwSize  = cpu_to_le32(expected_bytes);

  return write (audio, &waveHdr, sizeof (waveHdr));
}

int ExitWav ( int audio, unsigned long nBytesDone )
{
  unsigned long temp = nBytesDone + sizeof(WAVEHDR) - sizeof(CHUNKHDR);

  waveHdr.chkRiff.dwSize = cpu_to_le32(temp);
  waveHdr.chkData.dwSize = cpu_to_le32(nBytesDone);

  /* goto beginning */
  if (lseek(audio, 0L, SEEK_SET) == -1) {
    return 0;
  }
  return write (audio, &waveHdr, sizeof (waveHdr));
}

unsigned long GetWavHdrSize( void )
{
  return sizeof( waveHdr );
}
