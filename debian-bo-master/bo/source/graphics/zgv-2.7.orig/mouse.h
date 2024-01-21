/* Zgv v2.7 - GIF, JPEG and PBM/PGM/PPM viewer, for VGA PCs running Linux.
 * Copyright (C) 1993-1995 Russell Marks. See README for license details.
 *
 * mouse.h - header for mouse.c.
 *
 *
 * mouse.[ch] are taken from 'selection', so this copyright applies:
 *
 *    Copyright (c) 1994, 1995 Andrew J. Haylett.  All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * licence or royalty fees, to use, copy, modify, and distribute this
 * software and its documentation for any purpose, provided that the above
 * copyright notice and the following two paragraphs appear (1) in all 
 * source copies of this software and (2) in accompanying documentation
 * wherever the programatic interface of this software, or any derivative
 * of it, is described.
 *
 * IN NO EVENT SHALL ANDREW J. HAYLETT BE LIABLE TO ANY PARTY FOR DIRECT,
 * INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT OF
 * THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF HE HAS BEEN 
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * ANDREW J. HAYLETT SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT
 * NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS ON AN "AS IS" 
 * BASIS, AND ANDREW J. HAYLETT HAS NO OBLIGATION TO PROVIDE MAINTENANCE,
 * SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

/* Interface file for mouse driver */
/* Andrew Haylett */
/* [# Edit 6, Date 30-Jul-94, Module mouse.h #] */
/* Modified by Edwin Fong 31-Dec-94 */

#ifndef MOUSE_H
#define MOUSE_H

#define MS_BUTLEFT	0x04
#define MS_BUTMIDDLE	0x02
#define MS_BUTRIGHT	0x01

#define MACCEL              2
#define MBAUD               1200
#define MDELTA              25
#define MDEV                "/dev/mouse"
#define MTOGGLE             0
#define MSAMPLE             100
#define MTYPE               NO_MOUSE

typedef enum {
    P_MS = 0,
    P_SUN = 1,
    P_MSC = 2,
    P_MM = 3,
    P_LOGI = 4,
    P_BM = 5,
    P_PS2 = 6,
    NO_MOUSE = 7
} mouse_type;

#define NR_TYPES 7	/* keep in step with mouse_type! */

struct ms_event {
    enum { MS_NONE = 0, MS_BUTUP = 1, MS_BUTDOWN = 2, MS_MOVE = 3, MS_DRAG = 4}
	ev_code;
    unsigned char ev_butstate;
    int ev_x, ev_y;
    int ev_dx, ev_dy;
};

int ms_init(const int accel, const int baud, const int delta,
	    const char *dev, const unsigned int toggle, const int sample,
	    const mouse_type type);

int get_ms_event(struct ms_event *ev);

#endif /* MOUSE_H */
