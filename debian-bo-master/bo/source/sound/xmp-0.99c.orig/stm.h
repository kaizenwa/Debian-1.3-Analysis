/* stm.h - [obsolete] Scream Tracker module format definitions
 * Copyright (C) 1997 C.Matsuoka and H.Carraro Jr
 *
 * $Id: stm.h,v 1.1 1997/01/26 22:40:54 claudio Exp $
 *
 * $Log: stm.h,v $
 * Revision 1.1  1997/01/26 22:40:54  claudio
 * Initial revision
 *
 */

#ifndef __STM_H
#define __STM_H

#define STM_TYPE_SONG	0x01
#define STM_TYPE_MODULE	0x02

struct stm_instrument_header {
    uint8 name[12];			/* ASCIIZ instrument name */
    uint8 id;				/* Id=0 */
    uint8 idisk;			/* Instrument disk */
    uint16 rsvd1;			/* Reserved */
    uint16 length;			/* Sample length */
    uint16 loopbeg;			/* Loop begin */
    uint16 loopend;			/* Loop end */
    uint8 volume;			/* Playback volume */
    uint8 rsvd2;			/* Reserved */
    uint16 c2spd;			/* C4 speed */
    uint32 rsvd3;			/* Reserved */
    uint16 paralen;			/* Length in paragraphs */
} PACKED;

struct stm_file_header {
    uint8 name[20];			/* ASCIIZ song name */
    uint8 magic[8];			/* '!Scream!' */
    uint8 rsvd1;			/* '\x1a' */
    uint8 type;				/* 1=song, 2=module */
    uint8 vermaj;			/* Major version number */    
    uint8 vermin;			/* Minor version number */
    uint8 tempo;			/* Playback tempo */
    uint8 patterns;			/* Number of patterns */
    uint8 gvol;				/* Global volume */
    uint8 rsvd2[13];			/* Reserved */
    struct stm_instrument_header ins[31];
} PACKED;

#endif /* __STM_H */

