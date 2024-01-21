/***
 * CopyPolicy: GNU Public License 2 applies
 * Copyright (C) by Heiko Eissfeldt
 *
 * prototypes from cdda2wav.c
 */
#define max(a,b) ((a) > (b) ? (a) : (b))
#define min(a,b) ((a) < (b) ? (a) : (b))


/* verbose levels */
#define SHOW_TOC	1
#define SHOW_SUMMARY	2
#define SHOW_INDICES	4
#define SHOW_MCN	8
#define SHOW_ISRC	16
#define SHOW_STARTPOSITIONS	32

#define get_previous_read_buffer() (bufferCdda + (1-swap) * (OFF + nsectors*CD_FRAMESIZE_RAW))
#define get_next_read_buffer() (bufferCdda + swap * (OFF + nsectors*CD_FRAMESIZE_RAW))

extern long cd_fd;
extern char dev_name [200];
extern char aux_name [200];
extern int audio;
extern int need_big_endian;
extern int soundcard_fd;
extern int echo;
extern int swap;
extern int sh_bits;
extern int OutSampleSize;
extern long Remainder;
extern int verbose;
extern int quiet;
extern int no_file;
extern int SkippedSamples;


void FatalError (const char *szMessage, ...);
void AnalyzeQchannel ( unsigned frame );
long SamplesNeeded( long amount, long undersampling);

