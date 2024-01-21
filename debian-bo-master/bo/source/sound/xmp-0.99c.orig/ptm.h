/* ptm.h - Poly Tracker module format definitions
 * Copyright (C) 1997 C.Matsuoka and H.Carraro Jr
 *
 * $Id: ptm.h,v 1.1 1997/02/08 20:15:54 claudio Exp $
 *
 * $Log: ptm.h,v $
 * Revision 1.1  1997/02/08 20:15:54  claudio
 * Initial revision
 *
 */

#ifndef __PTM_H
#define __PTM_H

#define PTM_CH_MASK 0x1f
#define PTM_NI_FOLLOW 0x20
#define PTM_VOL_FOLLOWS 0x80
#define PTM_FX_FOLLOWS 0x40

struct ptm_file_header {
    uint8 name[28];		/* Song name */
    uint8 doseof;		/* 0x1a */
    uint8 vermin;		/* File type */
    uint8 vermaj;		/* File type */
    uint8 rsvd1;		/* Reserved */
    uint16 ordnum;		/* Number of orders (must be even) */
    uint16 insnum;		/* Number of instruments */
    uint16 patnum;		/* Number of patterns */
    uint16 chnnum;		/* Number of channels */
    uint16 flags;		/* Flags (set to 0) */
    uint16 rsvd2;		/* Reserved */
    uint8 magic[4];		/* 'PTMF' */
    uint8 rsvd3[16];		/* Reserved */
    uint8 chset[32];		/* Channel settings */
    uint8 order[256];		/* Orders */
    uint16 patseg[128];
} PACKED;

struct ptm_instrument_header {
    uint8 type;			/* Sample type */
    uint8 dosname[12];		/* DOS file name */
    uint8 vol;			/* Volume */
    uint16 c4spd;		/* C4 speed */
    uint16 smpseg;		/* Sample segment (not used) */
    uint32 smpofs;		/* Sample offset */
    uint32 length;		/* Length */
    uint32 loopbeg;		/* Loop begin */
    uint32 loopend;		/* Loop end */
    uint32 gusbeg;		/* GUS begin address */
    uint32 guslps;		/* GUS loop start address */
    uint32 guslpe;		/* GUS loop end address */
    uint8 gusflg;		/* GUS loop flags */
    uint8 rsvd1;		/* Reserved */
    uint8 name[28];		/* Instrument name */
    uint8 magic[4];		/* 'PTMS' */
} PACKED;

#endif /* __PTM_H */

