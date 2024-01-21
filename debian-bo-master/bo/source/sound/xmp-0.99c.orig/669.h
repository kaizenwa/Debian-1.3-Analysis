/* 669.h - 669 module format definitions
 * Copyright (C) 1997 C.Matsuoka and H.Carraro Jr
 *
 * $Id: 669.h,v 1.1 1997/02/06 16:47:06 claudio Exp $
 *
 * $Log: 669.h,v $
 * Revision 1.1  1997/02/06 16:47:06  claudio
 * Initial revision
 *
 */

#ifndef __669_H
#define __669_H

struct ssn_file_header {
    uint8 marker[2];			/* 'if'=standard, 'JN'=extended */
    uint8 message[108];			/* Song message */
    uint8 nos;				/* Number of samples (0-64) */
    uint8 nop;				/* Number of patterns (0-128) */
    uint8 loop;				/* Loop order number */
    uint8 order[128];			/* Order list */
    uint8 tempo[128];			/* Tempo list for patterns */
    uint8 pbrk[128];			/* Break list for patterns */
} PACKED;

struct ssn_instrument_header {
    uint8 name[13];			/* ASCIIZ instrument name */
    uint32 length;			/* Instrument length */
    uint32 loopstart;			/* Instrument loop start */
    uint32 loopend;			/* Instrument loop end */
} PACKED;

#endif /* __669_H */

