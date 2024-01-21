/* okt.h - Oktalyzer module format definitions
 * Copyright (C) 1997 C.Matsuoka and H.Carraro Jr
 *
 * $Id: okt.h,v 1.1 1997/03/06 13:51:54 claudio Exp $
 *
 * $Log: okt.h,v $
 * Revision 1.1  1997/03/06 13:51:54  claudio
 * Initial revision
 *
 */

#ifndef __OKT_H
#define __OKT_H

/* I know this is the wrong way to load this kind of module, but I was
 * too lazy to do it in the right way. Any volunteer to rewrite it?
 */

struct okt_instrument_header {
    uint8 name[20];			/* Intrument name */
    uint32 length;			/* Sample length */
    uint16 loopstart;			/* Loop start */
    uint16 looplen;			/* Loop length */
    uint8 rsvd1;			/* Padding */
    uint8 volume;			/* Volume */
    uint16 rsvd2;			/* Padding */
} PACKED;

struct okt_file_header {
    uint8 magic[8];			/* 'OKTASONG' */
    uint8 cmod[4];			/* 'CMOD' */
    uint32 cmod_len;			/* Chunk length (8) */
    uint16 ch_flags[4];			/* 0=normal, 1=tied */
    uint8 samp[4];			/* 'SAMP' */
    uint32 samp_len;			/* Chunk length (480) */
    struct okt_instrument_header ins[36];
    uint8 spee[4];			/* 'SPEE' */ 
    uint32 spee_len;			/* Chunk length (2) */
    uint16 tempo;			/* Initial tempo */
    uint8 slen[4];			/* 'SLEN' */
    uint32 slen_len;			/* Chunk length (2) */
    uint16 patterns;			/* Number of patterns */
    uint8 plen[4];			/* 'PLEN' */
    uint32 plen_len;			/* Chunk length (2) */
    uint16 modlen;			/* Module length */
    uint8 patt[4];			/* 'PATT' */
    uint32 patt_len;			/* Chunk length (128) */
    uint8 orders[128];			/* Orders */
} PACKED;

struct okt_pattern_header {
    uint8 pbod[4];			/* 'PBOD' */
    uint32 pbod_len;			/* Chunk length */
    uint16 rows;			/* Number of pattern lines */
} PACKED;

struct okt_event {
    uint8 note;				/* Note (1-36) */
    uint8 ins;				/* Intrument number */
    uint8 fxt;				/* Effect type */
    uint8 fxp;				/* Effect parameter */
} PACKED;

struct okt_sample_header {
    uint8 sbod[4];			/* 'SBOD' */
    uint32 sbod_len;			/* Chunk length */
} PACKED;

#endif /* __OKT_H */
