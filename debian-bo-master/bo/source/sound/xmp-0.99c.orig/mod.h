/* mod.h - Protracker MOD file definitions
 * Copyright (C) 1997 C.Matsuoka and H.Carraro Jr
 *
 * $Id: mod.h,v 1.1 1997/03/05 01:50:54 claudio Exp $
 *
 * $Log: mod.h,v $
 * Revision 1.1  1997/03/05 01:50:54  claudio
 * Initial revision
 *
 */

#ifndef __MOD_H
#define __MOD_H

struct mod_instrument_header {
    uint8 name[22];			/* Instrument name */
    uint16 length;			/* Sample length in 16-bit words */
    int8 finetune;			/* Finetune (signed nibble) */
    int8 volume;			/* Linear playback volume */
    uint16 loopstart;			/* Loop start in 16-bit words */
    uint16 looplength;			/* Loop length in 16-bit words */
} PACKED;

struct mod_file_header {
    uint8 name[20];
    struct mod_instrument_header ins[31];
    uint8 songlen;
    uint8 endjump;
    uint8 orders[128];
    uint8 magic[4];
} PACKED;

struct mod15_file_header {
    uint8 name[20];
    struct mod_instrument_header ins[15];
    uint8 songlen;
    uint8 endjump;
    uint8 orders[128];
} PACKED;

union mod_header {
    struct mod_file_header m31;
    struct mod15_file_header m15;
} PACKED;

#endif /* __MOD_H */
