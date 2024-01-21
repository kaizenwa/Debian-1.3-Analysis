/* xxm.h - Extended extended module format definitions
 * Copyright (C) 199[67] C.Matsuoka and H.Carraro Jr
 *
 * $Id: xxm.h,v 1.1 1997/03/13 13:27:34 claudio Exp $
 *
 * $Log: xxm.h,v $
 * Revision 1.1  1997/03/13 13:27:34  claudio
 * Initial revision
 *
 */

#ifndef __XXM_H
#define __XXM_H

#define XXM_FLG_LINEAR	0x01
#define XXM_FLG_MODRNG	0x02
#define XXM_FLG_LOOPS3M	0x04
#define XXM_FLG_LOOPXXM	0x08

struct xxm_header {
    uint8 magic[4];			/* XXM\0 */
    uint8 name[22];			/* Module name */
    uint8 zero;				/* Always zero */
    uint8 ver;				/* Version */
    uint8 rsvd1[2];			/* Reserved */
    uint8 flg;				/* Flags */
    uint8 ptc;				/* Number of patches */
    uint16 trk;				/* Number of tracks */
    uint8 pat;				/* Reserved */
    uint8 chn;				/* Tracks per pattern */
    uint8 ins;				/* Number of instruments */
    uint8 smp;				/* Number of samples */
    uint8 tpo;				/* Initial tempo */
    uint8 bpm;				/* Initial BPM */
    uint8 len;				/* Module length in patterns */
    uint8 rst;				/* Restart position */
    uint8 gvl;				/* Global volume */
    uint8 chd;				/* Chord mode */
    uint32 date;			/* Date in Unix format */
} PACKED;

struct xxm_channel {
    uint8 pan;
    uint8 vol;
    uint8 rsvd;
    uint8 flg;
} PACKED;

struct xxm_trackinfo {
    uint16 index;			/* Track index */
    int8 xpose;				/* Track transpose */
    uint8 flags;			/* Flags */
} PACKED;

struct xxm_pattern {
    uint8 magic[2];			/* 'XP' */
    uint8 rows;				/* Number of rows */
    struct xxm_trackinfo info[1];
} PACKED;

#define XXM_EVENT_PACK 0x80
#define XXM_EVENT_MASK 0x7f
#define XXM_EVENT_NOT 0x01
#define XXM_EVENT_INS 0x02
#define XXM_EVENT_VOL 0x04
#define XXM_EVENT_FXT 0x08
#define XXM_EVENT_FXP 0x10

struct xxm_event {
    uint8 not;				/* Note number (0==no note) */
    uint8 ins;				/* Patch number */
    uint8 vol;				/* Volume (0 to 64) */
    uint8 fxt;				/* Effect type */
    uint8 fxp;				/* Effect parameter */
    uint8 f2t;				/* Secondary effect type */
    uint8 f2p;				/* Secondary effect parameter */
} PACKED;

struct xxm_track {
    uint8 magic[2];			/* 'XT' */
    uint8 rows;				/* Number of rows */
    struct xxm_event event[1];
} PACKED;

#define XXM_ENV_ON	0x01
#define XXM_ENV_SUS	0x02
#define XXM_ENV_LOOP	0x04
#define XXM_ENV_RLS	0x08

struct xxm_envinfo {
    uint8 flg;				/* Flags */
    uint8 npt;				/* Number of envelope points */
    uint8 scl;				/* Envelope scaling */
    uint8 sus;				/* Sustain point */
    uint8 lps;				/* Loop start point */
    uint8 lpe;				/* Loop end point */
} PACKED;

struct xxm_instrument_header {
    uint8 magic[2];			/* 'XI' */
    uint8 name[24];			/* Instrument name */
    uint8 zero;				/* Always zero */
    uint8 nsm;				/* Number of samples */
    uint16 rls;
    struct xxm_envinfo aei;		/* Amplitude envelope info */
    struct xxm_envinfo pei;		/* Pan envelope info */
    struct xxm_envinfo fei;		/* Frequency envelope info */
} PACKED;

struct xxm_instrument_map {
    uint8 ins[96];			/* Instrument number for each key */
} PACKED;

struct xxm_instrument {
    uint8 vol;				/* Volume */
    uint8 pan;				/* Pan */
    int8 xpo;				/* Transpose */
    int8 fin;				/* Finetune */
    uint8 vos;				/* Volume scaling */
    uint8 fsc;				/* Frequency scaling */
    uint8 vsc;				/* Velocity scaling */
    uint8 nna;				/* New note action (heresy!) */
    uint16 flg;				/* Flags */
    uint16 rsvd1;
    uint8 vwf;				/* Vibrato waveform */
    uint8 vde;				/* Vibrato depth */
    uint8 vra;				/* Vibrato rate */
    uint8 vsw;				/* Vibrato sweep */
    uint8 dly;				/* Delay rate */
    uint8 dre;				/* Delay repetitions */
    uint8 dam;				/* Delay amplitude */
    uint8 psp;				/* Portamento speed */
    uint8 rvv;				/* Random volume variation */
    uint8 rpv;				/* Random pan variation */
    uint8 sid;				/* Sample number */
    uint8 rsvd2[9];			/* Reserved */
} PACKED;

#define XXM_SMP_16BIT	0x01
#define XXM_SMP_FDLOOP	0x02
#define XXM_SMP_BDLOOP	0x04
#define XXM_SMP_RLS	0x02

struct xxm_sample {
    uint8 magic[2];			/* 'XS' */
    uint32 len;				/* Sample length */
    uint32 lps;				/* Loop start */
    uint32 lpe;				/* Loop end */
    uint16 flg;				/* Flags */
} PACKED;

#endif /* __XM_H */

