/* mtm.h - Multitracker module format definitions
 * Copyright (C) 199[67] C.Matsuoka and H.Carraro Jr
 *
 * $Id: mtm.h,v 1.1 1997/02/02 23:00:42 claudio Exp $
 *
 * $Log: mtm.h,v $
 * Revision 1.1  1997/02/02 23:00:42  claudio
 * Initial revision
 *
 */

#ifndef __MTM_H
#define __MTM_H

struct mtm_file_header {
    uint8 magic[3];			/* "MTM" */
    uint8 version;			/* MSN=major, LSN=minor */
    uint8 name[20];			/* ASCIIZ Module name */
    uint16 tracks;			/* Number of tracks saved */
    uint8 patterns;			/* Number of patterns saved */
    uint8 modlen;			/* Module length */
    uint16 extralen;			/* Length of the comment field */
    uint8 samples;			/* Number of samples */
    uint8 attr;				/* Always zero */
    uint8 rows;				/* Number rows per track */
    uint8 channels;			/* Number of tracks per pattern */
    uint8 pan[32];			/* Pan positions for each channel */
} PACKED;

struct mtm_instrument_header {
    uint8 name[22];			/* Instrument name */
    uint32 length;			/* Instrument length in bytes */
    uint32 loopstart;			/* Sample loop start */
    uint32 loopend;			/* Sample loop end */
    uint8 finetune;			/* Finetune */
    uint8 volume;			/* Playback volume */
    uint8 attr;				/* &0x01: 16bit sample */
} PACKED;

#endif /* __MTM_H */
