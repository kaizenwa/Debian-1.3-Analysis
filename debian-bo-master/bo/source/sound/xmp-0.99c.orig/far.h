/* far.h - Farandole Composer module definitions
 * Copyright (C) 1997 C.Matsuoka and H.Carraro Jr
 *
 * $Id$
 *
 * $Log$
 */

#ifndef __FAR_H
#define __FAR_H

struct far_file_header {
    uint8 magic[4];			/* File magic: 'FAR\xfe' */
    uint8 name[40];			/* Song name */
    uint8 crlf[3];			/* 0x0d 0x0a 0x1A */
    uint16 headersize;			/* Remaining header size in bytes */
    uint8 version;			/* Version MSN=major, LSN=minor */
    uint8 ch_on[16];			/* Channel on/off switches */
    uint8 rsvd1[9];			/* Current editing values */
    uint8 tempo;			/* Default tempo */
    uint8 pan[16];			/* Channel pan definitions */
    uint8 rsvd2[4];			/* Grid, mode (for editor) */
    uint16 textlen;			/* Length of embedded text */
} PACKED;

struct far_file_header2 {
    uint8 order[256];			/* Orders */
    uint8 patterns;			/* Number of stored patterns (?) */
    uint8 songlen;			/* Song length in patterns */
    uint8 restart;			/* Restart pos */
    uint16 patsize[256];		/* Size of each pattern in bytes */
} PACKED;

struct far_instrument_header {
    uint8 name[32];			/* Instrument name */
    uint32 length;			/* Length of sample (up to 64Kb) */
    uint8 finetune;			/* Finetune (unsuported) */
    uint8 volume;			/* Volume (unsuported?) */
    uint32 loopstart;			/* Loop start */
    uint32 loopend;			/* Loop end */
    uint8 sampletype;			/* 1=16 bit sample */
    uint8 loopmode; 
} PACKED;

struct far_event {
    uint8 note;
    uint8 instrument;
    uint8 volume;			/* In reverse nibble order? */
    uint8 effect;
} PACKED;

#endif /* __FAR_H */
