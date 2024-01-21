/* xmp.h - Extended module player definitions
 * Copyright (C) 1996 C.Matsuoka and H.Carraro Jr
 *
 * $Id: xmp.h,v 1.6 1997/03/18 16:02:44 claudio Exp $
 *
 * $Log: xmp.h,v $
 * Revision 1.6  1997/03/18 16:02:44  claudio
 * Changed fields in xmp_channel.
 *
 * Revision 1.5  1997/03/13 13:20:42  claudio
 * Lots of changes for xmp 0.99.
 *
 * Revision 1.4  1997/01/19 01:40:11  claudio
 * Added GLOBAL_VSLIDE flag. Added field 'arp' in struct xmp_channel.
 *
 * Revision 1.3  1997/01/01 23:59:44  claudio
 * Minor changes for 0.09d.
 *
 * Revision 1.2  1996/12/10 19:17:50  claudio
 * Added "key" in the channel struct.
 *
 * Revision 1.1  1996/12/07 20:40:12  claudio
 * Initial revision
 *
 */

#ifndef __XMP_H
#define __XMP_H

#if(defined GUS_DEVICE)||(defined AWE_DEVICE)
#define USE_OSS
#endif

#ifdef USE_OSS
#include <sys/soundcard.h>
#else
#define WAVE_LOOPING 0x01
#define WAVE_BIDIR_LOOP 0x02
#define WAVE_16_BITS 0x04
#endif

#include <stdio.h>
#include <signal.h>

typedef signed char int8;
typedef signed short int int16;
typedef signed int int32;
typedef unsigned char uint8;
typedef unsigned short int uint16;
typedef unsigned int uint32;

/* 9.10:   My compiler is leaving holes in structures, which is wasting
 *	   space and preventing "binary" I/O to external data files.  Can I
 *	   turn off the padding, or otherwise control the alignment of
 *	   structs?
 *
 * A:	   Your compiler may provide an extension to give you this control
 *	   (perhaps a #pragma), but there is no standard method.  See also
 *	   question 17.2.
 */

#ifdef __GNUC__
#define PACKED __attribute__((packed))
#else
#define PACKED
#endif

#define L_CHANNELS 64		/* Logical channels */ 

enum period_modes {
    XMP_DEFAULT_PERIOD_MODE,
    XMP_LINEAR_PERIOD_MODE,
    XMP_AMIGA_PERIOD_MODE
};

enum echo_msg { 
    ECHO_NONE,ECHO_END, ECHO_CHN, ECHO_VOL,
    ECHO_PAN, ECHO_INS, ECHO_ORD, ECHO_ROW
};

enum fx_codes {
    FX_0, FX_1, FX_2, FX_3, FX_4, FX_5, FX_6, FX_7,
    FX_8, FX_9, FX_A, FX_B, FX_C, FX_D, FX_E, FX_F,
    FX_G, FX_H, FX_I, FX_J, FX_K, FX_L, FX_M, FX_N,
    FX_O, FX_P, FX_Q, FX_R, FX_S, FX_T, FX_U, FX_V,
    FX_W, FX_X, FX_Y, FX_Z
};

#include "devices.h"

/* Macros for period conversion */
#define NOTE_B0		11
#define NOTE_Bb0	NOTE_B0+1 
#define MAX_NOTE	NOTE_B0*8
#define MAX_PERIOD	0x1c56
#define MIN_PERIOD_A	0x0008
#define MAX_PERIOD_A	0x1abf
#define MIN_PERIOD_L	0x0000
#define MAX_PERIOD_L	0x1e00

/* Sample flags */
#define XMP_SMP_DIFF	0x01
#define XMP_SMP_UNS	0x02
#define XMP_SMP_8BDIFF	0x04

/* Signals */
#define SIGNAL_ABORT	SIGABRT
#define SIGNAL_RESTART	SIGUSR1

/* Amiga limits */
#define AMIGA_LIMIT_UPPER 108
#define AMIGA_LIMIT_LOWER 907

/* Global flags */
#define PATTERN_BREAK	0x0001 
#define PATTERN_LOOP	0x0002 
#define MODULE_ABORT	0x0004 
#define MODULE_RESTART	0x0008 
#define GLOBAL_VSLIDE	0x0010
#define ROW_MAX		0x0100

#define MSN(x)		(((x)&0xf0)>>4)
#define LSN(x)		((x)&0x0f)
#define SET_FLAG(a,b)	((a)|=(b))
#define RESET_FLAG(a,b)	((a)&=~(b))
#define TEST_FLAG(a,b)	((a)&(b))

#define EVENT(p,c,r)	xxt[xxp[p]->info[c].index]->event[r]

/* Endianism fixup */
#define FIX_ENDIANISM_16(x)	(x=((((x)&0xff00)>>8)|(((x)&0xff)<<8)))
#define FIX_ENDIANISM_32(x)	(x=(((x)&0xff000000)>>24)|(((x)&0xff0000)>>8)|\
				  (((x)&0xff00)<<8)|(((x)&0xff)<<24))
#ifdef XMP_LITTLE_ENDIAN
#define L_ENDIAN16(x)	(x=x)
#define L_ENDIAN32(x)	(x=x)
#define B_ENDIAN16(x)	FIX_ENDIANISM_16(x)
#define B_ENDIAN32(x)	FIX_ENDIANISM_32(x)
#else
#define L_ENDIAN16(x)	FIX_ENDIANISM_16(x)
#define L_ENDIAN32(x)	FIX_ENDIANISM_32(x)
#define B_ENDIAN16(x)	(x=x)
#define B_ENDIAN32(x)	(x=x)
#endif

/* Constants */
#define PAL_RATE	250.0		/* 1/(50Hz * 80us) */
#define NTSC_RATE	208.0		/* 1/(60Hz * 80us) */
#define C4_FREQ		130812		/* 440Hz/(2^(21/12))*1000 */
#define C4_PAL_RATE	8287		/* 7093789.2/period(C4)*2 */
#define C4_NTSC_RATE	8363		/* 7159090.5/period(C4)*2 */

/* [Amiga] PAL color carrier frequency (PCCF) = 4.43361825 MHz */
/* [Amiga] CPU clock = 1.6*PCCF = 7.0937892 MHz */

/* The following macros are used to set the flags for each channel */
#define VOL_SLIDE	0x0001
#define PAN_SLIDE	0x0002
#define TONEPORTA	0x0004
#define PITCHBEND	0x0008
#define VIBRATO		0x0010
#define TREMOLO		0x0020
#define FINE_VOLS	0x0040
#define FINE_BEND	0x0080
/* These don't need to be "persistent" between frames */
#define NEW_NOTE	0x0100
#define NEW_VOL		0x0200
#define NEW_PAN		0x0400
#define NEW_INS		0x0800
#define FINETUNE	0x1000

#define MODULE_NAME_MAXSIZE 40

/* Prefixes: 
 * a_ for arpeggio
 * v_ for volume slide
 * p_ for pan
 * f_ for frequency (period) slide
 * y_ for vibrato (v is already being used by volume)
 * s_ for slide to note (tone portamento)
 * t_ for tremolo
 */

struct xmp_channel {
    int pch;			/* Physical channel number */
    int mute;			/* Channel is muted */
    int flags;			/* Channel flags (see above) */
    uint8 note;			/* Note number */
    uint8 key;
    int arp;
    int period;			/* Amiga or linear period */
    int pitchbend;		/* Pitch bender value */
    int finetune;		/* Guess what */
    int ins;			/* Instrument number */
    int oldins;			/* Previous instrument number */
    int pan;			/* Current pan */
    int delay;			/* Note delay in frames */
    int retrig;			/* Retrig delay in frames */
    int rval;
    int rtype;			/* Retrig type -- funny name... */
    int rcount;			/* Retrig counter */
    int volume;			/* Current volume */
    int v_val;			/* Volume slide value */
    int v_fval;			/* Fine volume slide value */
    int p_val;			/* Current pan value */
    uint16 v_idx;		/* Volume envelope index */
    int y_type;			/* Vibrato waveform */
    int y_depth;		/* Vibrato depth */
    int y_sweep;		/* Vibrato sweep */
    int y_rate;			/* Vibrato rate */
    int y_idx;			/* Vibrato index */
    int t_type;			/* Tremolo waveform */
    int t_depth;		/* Tremolo depth */
    int t_rate;			/* Tremolo rate */
    int t_idx;			/* Tremolo index */
    int f_val;			/* Frequency slide value */
    int f_fval;			/* Fine frequency slide value */
    uint16 p_idx;		/* Pan envelope index */
    int release;		/* Note released */
    int fadeout;		/* Current fadeout (release) value */
    int gliss;			/* Glissando active */
    int s_end;			/* Target period for tone portamento */
    int s_sgn;			/* Tone portamento up/down switch */
    int s_val;			/* Delta for tone portamento */
    int a_val[3];		/* Arpeggio relative notes */
    int a_idx;			/* Arpeggio index */
    int insvib_idx;		/* Instrument vibrato index */
    int insvib_swp;		/* Instrument vibrato sweep */
    int offset;
};

struct retrig_t {
    int s;
    int m;
    int d;
};

struct options {
    int verbose;
    int period_mode;
    int reverse;		/* Reverse stereo (-1, 0, 1) */
    int loop;			/* Allow pattern jumps */
    int start;
    int panel;			/* Enables the X11 panel */
    int mix;			/* Percentage of L/R channel separation */
    int flags;                  /* Set Amiga & Linear Table Mode */
    int modrange;		/* Force Amiga period limits */
    int ntsc;			/* Use NTSC timing instead of PAL */
    int ignoreff;		/* Ignore 0xff patterns in S3M */
    int freq;			/* Software mixing rate (Hz) */
    int bits;			/* Output resolution */
    int dump;
    int smp8bit;
};

struct fmt_info {
    char *id;
    int (*loader)();
    struct fmt_info *next;
};

void die(char *,...);
int report(char *,...);
int note_to_period(int,int,int);
int period_to_note(int);
int period_to_bend(int,int,int,int,int);
void c2spd_to_note(int,char *,char *);
void diff2abs(int,int,char *);
void sig2uns(int,int,char *);
char *str_adj(char *);
int xm_load(FILE *,struct dev_info *);
int mod_load(FILE *,struct dev_info *);
int s3m_load(FILE *,struct dev_info *);
int stm_load(FILE *,struct dev_info *);
int ssn_load(FILE *,struct dev_info *);
int far_load(FILE *,struct dev_info *);
int mtm_load(FILE *,struct dev_info *);
int ptm_load(FILE *,struct dev_info *);
int okt_load(FILE *,struct dev_info *);
int it_load(FILE *,struct dev_info *);
int play_module(struct dev_info *);
int get_envelope(uint16 *,uint8,uint16);
void check_alignment(void);

#endif /* __XMP_H */

