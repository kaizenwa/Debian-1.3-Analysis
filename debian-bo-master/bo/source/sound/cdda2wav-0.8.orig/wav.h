/***
 * CopyPolicy: GNU Public License 2 applies
 * Copyright (C) by Heiko Eissfeldt
 *
 *
 * ---------------------------------------------------------------------
 *  definitions for RIFF-output (from windows MMSYSTEM)
 * ---------------------------------------------------------------------
 */

#include "byteorder.h"

typedef DWORD FOURCC;	/* a four character code */

typedef struct CHUNKHDR {
  FOURCC ckid;		/* chunk ID */
  DWORD dwSize; 	/* chunk size */
} CHUNKHDR;

/* flags for 'wFormatTag' field of WAVEFORMAT */
#define WAVE_FORMAT_PCM 1

/* specific waveform format structure for PCM data */
typedef struct pcmwaveformat_tag {
  WORD wFormatTag;	/* format type */
  WORD nChannels;	/* number of channels (i.e. mono, stereo, etc.) */
  DWORD nSamplesPerSec; /* sample rate */
  DWORD nAvgBytesPerSec;/* for buffer size estimate */
  WORD nBlockAlign;	/* block size of data */
  WORD wBitsPerSample;
} PCMWAVEFORMAT;
typedef PCMWAVEFORMAT *PPCMWAVEFORMAT;


/* MMIO macros */
#define mmioFOURCC(ch0, ch1, ch2, ch3) \
  ((DWORD)(BYTE)(ch0) | ((DWORD)(BYTE)(ch1) << 8) | \
  ((DWORD)(BYTE)(ch2) << 16) | ((DWORD)(BYTE)(ch3) << 24))

#define FOURCC_RIFF	mmioFOURCC ('R', 'I', 'F', 'F')
#define FOURCC_LIST	mmioFOURCC ('L', 'I', 'S', 'T')
#define FOURCC_WAVE	mmioFOURCC ('W', 'A', 'V', 'E')
#define FOURCC_FMT	mmioFOURCC ('f', 'm', 't', ' ')
#define FOURCC_DATA	mmioFOURCC ('d', 'a', 't', 'a')


/* simplified Header for standard WAV files */
typedef struct WAVEHDR {
  CHUNKHDR chkRiff;
  FOURCC fccWave;
  CHUNKHDR chkFmt;
  WORD wFormatTag;	/* format type */
  WORD nChannels;	/* number of channels (i.e. mono, stereo, etc.) */
  DWORD nSamplesPerSec; /* sample rate */
  DWORD nAvgBytesPerSec;/* for buffer estimation */
  WORD nBlockAlign;	/* block size of data */
  WORD wBitsPerSample;
  CHUNKHDR chkData;
} WAVEHDR;

#define IS_STD_WAV_HEADER(waveHdr) ( \
  waveHdr.chkRiff.ckid == FOURCC_RIFF && \
  waveHdr.fccWave == FOURCC_WAVE && \
  waveHdr.chkFmt.ckid == FOURCC_FMT && \
  waveHdr.chkData.ckid == FOURCC_DATA && \
  waveHdr.wFormatTag == WAVE_FORMAT_PCM)

/* Prototypes */
int InitWav ( int audio, long channels, unsigned long rate, long nBitsPerSample,
	     unsigned long expected_bytes);
int ExitWav ( int audio, unsigned long nBytesDone );
unsigned long GetWavHdrSize( void );


