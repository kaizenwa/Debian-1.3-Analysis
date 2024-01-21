/***
 * CopyPolicy: GNU Public License 2 applies
 * Copyright (C) by Heiko Eissfeldt
 *
 * header file interface.h for cdda2wav */

#ifndef CD_FRAMESIZE_RAW
#define CD_FRAMESIZE_RAW 2352
#endif
#define CD_FRAMESAMPLES (CD_FRAMESIZE_RAW / 4)

#define MAXTRK	100	/* maximum of audio tracks */

typedef struct TOC {	/* structure of table of contents (cdrom) */
  unsigned char reserved1;
  unsigned char bFlags;
  unsigned char bTrack;
  unsigned char reserved2;
  u_int32_t dwStartSector;
  unsigned char ISRC[15];
} TOC;

TOC g_toc [MAXTRK]; /* 100 */

#define IS_AUDIO(i) (!(g_toc[i].bFlags & 0x04))
unsigned char MCN[15];
unsigned char Extra_buffer[2048];
int have_CD_extra;
unsigned int session_start;

unsigned interface;
unsigned OFF;
unsigned char *bufferCdda;
int lowendian;

extern int trackindex_disp;
extern int sem_id;
extern unsigned overlap;
extern unsigned nsectors;
#define NSECTORS 75

/* interface types */
#define GENERIC_SCSI	0
#define COOKED_IOCTL	1

/* constants for sub-q-channel info */
#define GET_ALL			0
#define GET_POSITIONDATA	1
#define GET_CATALOGNUMBER	2
#define GET_TRACK_ISRC		3

typedef struct subq_chnl {
    unsigned char reserved;
    unsigned char audio_status;
    unsigned short subq_length;
    unsigned char format;
    unsigned char control_adr;
    unsigned char track;
    unsigned char index;
    unsigned char data[40];	/* this has subq_all, subq_position,
				   subq_catalog or subq_track_isrc format */
} subq_chnl;

typedef struct subq_all {
    unsigned char abs_min;
    unsigned char abs_sec;
    unsigned char abs_frame;
    unsigned char abs_reserved;
    unsigned char trel_min;
    unsigned char trel_sec;
    unsigned char trel_frame;
    unsigned char trel_reserved;
    unsigned char mc_valid;     /* MSB */
    unsigned char media_catalog_number[15];
    unsigned char tc_valid;	/* MSB */
    unsigned char track_ISRC[15];
} subq_all;

typedef struct subq_position {
    unsigned char abs_reserved;
    unsigned char abs_min;
    unsigned char abs_sec;
    unsigned char abs_frame;
    unsigned char trel_reserved;
    unsigned char trel_min;
    unsigned char trel_sec;
    unsigned char trel_frame;
} subq_position;

typedef struct subq_catalog {
    unsigned char mc_valid;	/* MSB */
    unsigned char media_catalog_number[15];
} subq_catalog;

typedef struct subq_track_isrc {
    unsigned char tc_valid;	/* MSB */
    unsigned char track_isrc[15];
} subq_track_isrc;

/* cdrom access function pointer */
void     (*EnableCdda) (int Switch);
unsigned (*ReadToc) ( TOC *ptoc );
void     (*ReadCdRom) (unsigned char *p, long lSector, unsigned long SectorBurstVal );
subq_chnl *(*ReadSubQ) ( unsigned char format, unsigned char track );
void Read_Subinfo(unsigned int pos, unsigned int length);
void dump_extra_info(void);

/*int  OpenCdRom ( char *name );*/
void SetupInterface( unsigned char *int_name );
void free_semshm(void);
