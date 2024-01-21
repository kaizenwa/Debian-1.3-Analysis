/*

    3DKIT   version   1.2
    High speed 3D graphics and rendering library for Linux.

    Copyright (C) 1996  Paul Sheer   psheer@hertz.mech.wits.ac.za

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
    MA 02111-1307, USA

*/



/*  File: wtriangle.c  

    Comments or suggestions welcome.

    See triangle.c for commentary.
    This is a modification of triangle.c for doing surface wraps.
    Passed to glwtriangle are not only the shading at each corner, but
    the position in a bitmap that that corner corresponds to.
    A contorted triangle of that part of the bitmap is thus drawn.
    At each pixel the shading (interpolated z value) is added to the
    local bitmap color and written to the screen. This allows, for
    example, a 16 color bitmap to have 16 shades of each color over
    the 256 color palette - the shade for each color that is displayed
    corresponds to the shadowing of the surface. Of course 256 color
    bitmaps can be drawn with no shadowing by passing 0 as the current
    shading at each corner, hence sacrificing realism for impression.

    The TD_tridata structure contains the bitmap. If the triangle faces
    away bitmap2 is drawn, in the same way as triangle.c doesn't draw
    the triangle (see HERE below). Bitmaps must be ONLY 256 pixels in
    width and 512 high, scanning from left to right.

*/

#include <vga.h>
#include <vgagl.h>
#include "triangle.h"

#define SHLB 8

#define S_MASK 0x01ff00

/* Scales the colour by powers of two: */
#define SHC 0

#define ASSIGNVP8(x, y, vp) vp = VBUF + (y) * BYTEWIDTH + (x);
#define ASSIGNVPOFFSET8(x, y, vp) vp = (y) * BYTEWIDTH + (x);
#define setpixel (*(currentcontext.ff.driver_setpixel_func))


/*extern GraphicsContext currentcontext; */

int px1, px2, py;
long c, c_x, xd, xd_x, yd, yd_x;
long dx0, dy0;
unsigned char *dat;

/*Functions to draw a horizontal varying color line going left or right */

/*draw using setpixel: */
static void colhline_neg_setpixel (void)
{
    int count, y = py + dy0, x1 = px1 + dx0, x2 = px2 + dx0;
    if (__clip) {
	if (y < __clipy1 || y > __clipy2)
	    return;
	if (x1 > __clipx2 + 1) {
	    xd -= (x1 - __clipx2 - 1) * xd_x;
	    yd -= (x1 - __clipx2 - 1) * yd_x;
	    c -= (x1 - __clipx2 - 1) * c_x;
	    x1 = __clipx2 + 1;
	}
	if (x2 < __clipx1) {
	    x2 = __clipx1;
	}
    }
    count = x1 - x2;
    if (count > 0) {
	do {
	    setpixel (--x1, y, dat[(xd >> SHLB) + (yd & S_MASK)] + (c >> (SHLB + SHC)));
	    c -= c_x;
	    yd -= yd_x;
	    xd -= xd_x;
	} while (--count);
    }
}

static void colhline_pos_setpixel (void)
{
    int count, y = py + dy0, x1 = px1 + dx0, x2 = px2 + dx0;
    if (__clip) {
	if (y < __clipy1 || y > __clipy2)
	    return;
	if (x1 < __clipx1) {
	    c += (__clipx1 - x1) * c_x;
	    xd += (__clipx1 - x1) * xd_x;
	    yd += (__clipx1 - x1) * yd_x;
	    x1 = __clipx1;
	}
	if (x2 > __clipx2 + 1) {
	    x2 = __clipx2 + 1;
	}
    }
    count = x2 - x1;
    if (count > 0) {
	do {
	    setpixel (x1++, y, dat[(xd >> SHLB) + (yd & S_MASK)] + (c >> (SHLB + SHC)));
	    c += c_x;
	    yd += yd_x;
	    xd += xd_x;
	} while (--count);
    }
}

/*draw to 64k vga buffer setting vga page appropriately: */
static void colhline_neg_paged (void)
{
    int count, y = py + dy0, x1 = px1 + dx0, x2 = px2 + dx0;
    unsigned short offst;
    int pg;
    long vp;
    if (__clip) {
	if (y < __clipy1 || y > __clipy2)
	    return;

	if (x1 > __clipx2 + 1) {
	    xd -= (x1 - __clipx2 - 1) * xd_x;
	    yd -= (x1 - __clipx2 - 1) * yd_x;
	    c -= (x1 - __clipx2 - 1) * c_x;
	    x1 = __clipx2 + 1;
	}
	if (x2 < __clipx1) {
	    x2 = __clipx1;
	}
    }
    count = x1 - x2;
    ASSIGNVPOFFSET8 (x1, y, vp);
    pg = vp >> 16;
    vga_setpage (pg);
    offst = vp;
    if (count > 0) {
	do {
	    if (!offst--)
		vga_setpage (--pg);
	    *(VBUF + offst) = dat[(xd >> SHLB) + (yd & S_MASK)] + (c >> (SHLB + SHC));
	    c -= c_x;
	    yd -= yd_x;
	    xd -= xd_x;
	} while (--count);
    }
}

static void colhline_pos_paged (void)
{
    int count, y = py + dy0, x1 = px1 + dx0, x2 = px2 + dx0;
    unsigned short offst;
    int pg;
    long vp;
    if (__clip) {
	if (y < __clipy1 || y > __clipy2)
	    return;

	if (x1 < __clipx1) {
	    c += (__clipx1 - x1) * c_x;
	    xd += (__clipx1 - x1) * xd_x;
	    yd += (__clipx1 - x1) * yd_x;
	    x1 = __clipx1;
	}
	if (x2 > __clipx2 + 1) {
	    x2 = __clipx2 + 1;
	}
    }
    count = x2 - x1;
    ASSIGNVPOFFSET8 (x1, y, vp);
    pg = vp >> 16;
    vga_setpage (pg);
    offst = vp;
    if (count > 0) {
	do {
	    *(VBUF + offst) = dat[(xd >> SHLB) + (yd & S_MASK)] + (c >> (SHLB + SHC));
	    if (!(++offst))
		vga_setpage (++pg);
	    c += c_x;
	    yd += yd_x;
	    xd += xd_x;
	} while (--count);
    }
}

/*draw to a linear address space (320x200 or virtual screen): */
static void colhline_neg_direct (void)
{
    int count, y = py + dy0, x1 = px1 + dx0, x2 = px2 + dx0;
    unsigned char *vp;
    if (__clip) {
	if (y < __clipy1 || y > __clipy2)
	    return;
	if (x1 > __clipx2 + 1) {
	    xd -= (x1 - __clipx2 - 1) * xd_x;
	    yd -= (x1 - __clipx2 - 1) * yd_x;
	    c -= (x1 - __clipx2 - 1) * c_x;
	    x1 = __clipx2 + 1;
	}
	if (x2 < __clipx1)
	    x2 = __clipx1;
    }
    count = x1 - x2;
    ASSIGNVP8 (x1, y, vp);
    if (count > 0 /*&& yd > 0*/) {
	do {
	    *(--vp) = dat[(xd >> SHLB) + (yd & S_MASK)] + (c >> (SHLB + SHC));
	    c -= c_x;
	    yd -= yd_x;
	    xd -= xd_x;
	} while (--count);
    }
}


static void colhline_pos_direct (void)
{
    int count, y = py + dy0, x1 = px1 + dx0, x2 = px2 + dx0;
    unsigned char *vp;
    if (__clip) {
	if (y < __clipy1 || y > __clipy2)
	    return;
	if (x1 < __clipx1) {
	    xd += (__clipx1 - x1) * xd_x;
	    yd += (__clipx1 - x1) * yd_x;
	    c += (__clipx1 - x1) * c_x;
	    x1 = __clipx1;
	}
	if (x2 > __clipx2 + 1)
	    x2 = __clipx2 + 1;
    }
    count = x2 - x1;
    ASSIGNVP8 (x1, y, vp);
    if (count > 0 /*&& yd > 0*/) {
	do {
	    *(vp++) = dat[(xd >> SHLB) + (yd & S_MASK)] + (c >> (SHLB + SHC));
	    c += c_x;
	    yd += yd_x;
	    xd += xd_x;
	} while (--count);
    }
}

/*The following have not yet been implemented */

/* Draws to planar 256 (these could be complicated) */
/*static void colhline_neg_planar (void);
static void colhline_pos_planar (void);*/

/* Draws using accelerated */
/*static void colhline_neg_accel (void);
static void colhline_pos_accel (void);*/


static inline void Xchg (int *a, int *b)
{
    int t = *a;
    *a = *b;
    *b = t;
}


void gl_wtriangle (int x0, int y0, int xd0, int yd0, int z0,
		   int x1, int y1, int xd1, int yd1, int z1,
		   int x2, int y2, int xd2, int yd2, int z2,
	TD_tridata * tri)  /*This does not alter tri structure*/

{
    void (*colhline_pos) (void);
    void (*colhline_neg) (void);

    int dir;
    int tz0, tz1;

    long nz;
    long c_y, xd_y, yd_y;
    long g0, g1h = 0, g1l = 0;
    long c0, X0, Y0;

    int bf = tri->bf;

/*
    int xd0 = tri->xd0;
    int xd1 = tri->xd1;
    int xd2 = tri->xd2;

    int yd0 = tri->yd0;
    int yd1 = tri->yd1;
    int yd2 = tri->yd2;

    int x0 = tri->x0;
    int x1 = tri->x1;
    int x2 = tri->x2;

    int y0 = tri->y0;
    int y1 = tri->y1;
    int y2 = tri->y2;

    int z0 = tri->z0;
    int z1 = tri->z1;
    int z2 = tri->z2;
*/

    dir = 1;


/*Max triangle size in the order of (2^31) >> SHLB)^(.5) : */

    if ((nz = (x0 - x1) * (y0 - y2) - (y0 - y1) * (x0 - x2)) == 0)
	return;			/*the points are collinear. */

    c_x = -(((y0 - y1) * (z0 - z2) - (z0 - z1) * (y0 - y2)) << SHLB) / nz;
    c_y = -(((z0 - z1) * (x0 - x2) - (x0 - x1) * (z0 - z2)) << SHLB) / nz;

    xd_x = -(((y0 - y1) * (xd0 - xd2) - (xd0 - xd1) * (y0 - y2)) << SHLB) / nz;
    xd_y = -(((xd0 - xd1) * (x0 - x2) - (x0 - x1) * (xd0 - xd2)) << SHLB) / nz;

    yd_x = -(((y0 - y1) * (yd0 - yd2) - (yd0 - yd1) * (y0 - y2)) << SHLB) / nz;
    yd_y = -(((yd0 - yd1) * (x0 - x2) - (x0 - x1) * (yd0 - yd2)) << SHLB) / nz;

    if ((abs (c_x) > (6 << SHLB)) || (abs (c_y) > (6 << SHLB))) {
	/*so that high colour gradients don't screw up at the edges. */
	/*4 is the maximum gradient per pixel. */

/* here we reduce the variation in color so that roundoff doesn't go
   off the scale */
/*   c_x = (5 * c_x) >> 3;
   c_y = (5 * c_y) >> 3;
   tz0 = (z0*6 + z1 + z2) << (SHLB - 3);
   tz1 = (z0 + z1*6 + z2) << (SHLB - 3);
   z2 = (z0 + z1 + z2*6) << (SHLB - 3);
   z0 = tz0;
   z1 = tz1;
 */

	c_x >>= 2;
	c_y >>= 2;
	tz0 = ((2 * z0 + z1 + z2) << SHLB) / 4;
	tz1 = ((z0 + 2 * z1 + z2) << SHLB) / 4;
	z2 = ((z0 + z1 + 2 * z2) << SHLB) / 4;
	z0 = tz0;
	z1 = tz1;

/*
   c_x = c_x >> 1;
   c_y = c_y >> 1;
   tz0 = (((z0 << 2) + z1 + z2) << SHLB) / 6;
   tz1 = ((z0 + (z1 << 2) + z2) << SHLB) / 6;
   z2 =  ((z0 + z1 + (z2 << 2)) << SHLB) / 6;
   z0 = tz0;
   z1 = tz1;
 */

/*
   c_x = 0;
   c_y = 0;
   z0 = ((z0 + z1 + z2) << SHLB) / 3;
   z1 = z0;
   z2 = z0;
 */

    } else {
	z0 <<= SHLB;
	z1 <<= SHLB;
	z2 <<= SHLB;
    }

/************** BOOLEAN LOGIC HERE ************/
/* The following allows a triangle to have a different picture on either side */
/* To print triangles that don't appear when viewed from behind use bf = 0|1  */
/* To print triangles that appear with a different picture when viewed from   */
/*   behind use bf = 2|3                                                      */

    dat = tri->bitmap1;
    if (nz > 0) {		/* nz is the cross product of the vectors of the two sides
				   it indicates whether the points were ordered clockwise
				   or anti-clockwise (you can find out which way by testing) */
	if (!bf)
	    return;
	if (bf == 2)
	    dat = tri->bitmap1;
	dir++;
    } else {
	if (bf == 1)
	    return;
	if (bf == 3)
	    dat = tri->bitmap2;
    }

    if (y1 < y0) {
	Xchg (&y0, &y1);
	Xchg (&x0, &x1);
	Xchg (&z0, &z1);
	Xchg (&xd0, &xd1);
	Xchg (&yd0, &yd1);
	dir++;
    }
    if (y2 < y1) {
	Xchg (&y2, &y1);
	Xchg (&x2, &x1);
	Xchg (&z2, &z1);
	Xchg (&xd2, &xd1);
	Xchg (&yd2, &yd1);

	dir++;
    }
    if (y1 < y0) {
	Xchg (&y0, &y1);
	Xchg (&x0, &x1);
	Xchg (&z0, &z1);
	Xchg (&xd0, &xd1);
	Xchg (&yd0, &yd1);

	dir++;
    }
    c0 = z0;
    X0 = xd0 << SHLB;
    Y0 = yd0 << SHLB;

    if (y2 == y0)
	return;
    g0 = ((long) (x2 - x0) << SHLB) / (y2 - y0);
    if (y1 != y0)
	g1h = ((long) (x1 - x0) << SHLB) / (y1 - y0);
    if (y2 != y1)
	g1l = ((long) (x2 - x1) << SHLB) / (y2 - y1);

    dir = dir & 1;


/* Very large triangles (larger than the screen) sometimes become a problem,
   if so: */
    if (__clip) {
	if (((abs (x0 - x1) + abs (x1 - x2) + abs (x0 - x2)) >
	     ((__clipx2 - __clipx1) * 2)) || ((y2 - y0) > (__clipy2 - __clipy1)))
	    return;
	if (y2 < __clipy1 || y0 > __clipy2 ||
	    (x0 < __clipx1 && x1 < __clipx1 && x2 < __clipx1) ||
	    (x0 > __clipx2 && x1 > __clipx2 && x2 > __clipx2))
	    return;
    }

    dx0 = x0;
    dy0 = y0;

    if (BYTESPERPIXEL != 1) {
/*I don't know what non-256 color modes will do, but here goes anyway... */
	colhline_pos = colhline_pos_setpixel;
	colhline_neg = colhline_neg_setpixel;
    } else if (MODETYPE == CONTEXT_VIRTUAL) {
	colhline_pos = colhline_pos_direct;
	colhline_neg = colhline_neg_direct;
    } else if (MODETYPE == CONTEXT_PAGED) {
	colhline_pos = colhline_pos_paged;
	colhline_neg = colhline_neg_paged;
    } else if (MODETYPE == CONTEXT_LINEAR) {
	colhline_pos = colhline_pos_direct;
	colhline_neg = colhline_neg_direct;
    } else {
/*Check for accelerated functions might come here */
	colhline_pos = colhline_pos_setpixel;
	colhline_neg = colhline_neg_setpixel;
    }


    if (dir == 1) {
	if (y1 != y0)
	    for (py = 0; py < y1 - y0; py++) {
		px1 = ((g0 * py) + (abs (g0) >> 1)) >> SHLB;
		px2 = ((g1h * py) - (abs (g1h) >> 1)) >> SHLB;
		xd = X0 + xd_x * px1 + xd_y * py;
		yd = Y0 + yd_x * px1 + yd_y * py;
		c = c0 + c_x * px1 + c_y * py;
		colhline_neg ();
	    }
	py = y1 - y0;

	px1 = ((g0 * py) + (abs (g0) >> 1)) >> SHLB;
	px2 = x1 - x0;
	xd = X0 + xd_x * px1 + xd_y * py;
	yd = Y0 + yd_x * px1 + yd_y * py;
	c = c0 + c_x * px1 + c_y * py;
	colhline_neg ();
	if (y1 != y2)
	    for (py = y1 - y0 + 1; py <= y2 - y0; py++) {
		px1 = ((g0 * py) + (abs (g0) >> 1)) >> SHLB;
		px2 = (((g1l * (py - y1 + y0)) - (abs (g1l) >> 1)) >> SHLB) + x1 - x0;
		xd = X0 + xd_x * px1 + xd_y * py;
		yd = Y0 + yd_x * px1 + yd_y * py;
		c = c0 + c_x * px1 + c_y * py;
		colhline_neg ();

	    }
    } else {
	if (y0 != y1)
	    for (py = 0; py < y1 - y0; py++) {
		px1 = ((g0 * py) - (abs (g0) >> 1)) >> SHLB;
		px2 = ((g1h * py) + (abs (g1h) >> 1)) >> SHLB;
		xd = X0 + xd_x * px1 + xd_y * py;
		yd = Y0 + yd_x * px1 + yd_y * py;
		c = c0 + c_x * px1 + c_y * py;
		colhline_pos ();
	    }
	py = y1 - y0;

	px1 = ((g0 * py) - (abs (g0) >> 1)) >> SHLB;
	px2 = x1 - x0;
	xd = X0 + xd_x * px1 + xd_y * py;
	yd = Y0 + yd_x * px1 + yd_y * py;
	c = c0 + c_x * px1 + c_y * py;

	colhline_pos ();
	if (y1 != y2)
	    for (py = y1 - y0 + 1; py <= y2 - y0; py++) {
		px1 = ((g0 * py) - (abs (g0) >> 1)) >> SHLB;
		px2 = (((g1l * (py - y1 + y0)) + (abs (g1l) >> 1)) >> SHLB) + x1 - x0;
		xd = X0 + xd_x * px1 + xd_y * py;
		yd = Y0 + yd_x * px1 + yd_y * py;
		c = c0 + c_x * px1 + c_y * py;

		colhline_pos ();
	    }
    }

}
