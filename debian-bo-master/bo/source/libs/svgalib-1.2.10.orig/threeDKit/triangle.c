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


/* File:  triangle.c
   
   Comments or suggestions welcome.

   The following routine draws an interpolated triangle using the
   direct writes to the virtual or paged screen. It takes ten parameters.
   The first nine are three sets of three coordinates, vis. the x and y 
   position and the colour (z = colour). The final value tells whether the
   triangle should be drawn (or not drawn) if the points are clockwise (or
   anti-clockwise). This lends itself to painters' algorithms for rendering,
   where the inside of a solid can never be seen and hence need not be drawn.
   This can be used to not draw triangles that appear from the back and are
   hence drawn anticlockwise: (See "HERE" below for the boolean logic.)

   The routine is as optimised as I could think of. I don't think a four
   way interpolation of a rectangle would be faster than two triangles.
   Drawing is optimised by considering that horizontal
   lines of the triangle can be drawn incrementally in one direction.
   A check is done to see if the current context is 256 color and virtual
   or paged --- if so, direct (linear or paged) access is performed,
   otherwise setpixel is used. This means that G320x200x256, G640x480x256,
   G800x600x256, G1024x768x256, and 256 color virtual screens have about a
   50% smaller execution time. Also, clipping is handled far more
   efficiently (even using setpixel), so that anything drawn out of the 
   clip window takes almost no execution time. The demo provided seems
   twice as fast in close zoom with these changes. (Note that the
   demo spends a large proportion of its execution time copying and 
   clearing the screen, so there isn't always a huge speed increase.)

   The triangles are almost perfect except for when the topmost or bottom
   most side is almost horizontal and a small line protrudes from the corner.
   If I have the urge I may fix this.

   Note: I don't know what the effect is of compiling with 16 bit integers,
   as linux uses 32 bit integers and the code was tested only on
   linux with gcc.

 */

#include <vga.h>
#include <vgagl.h>

#define SHLB 8

/* Scales the colour by powers of two: */
#define SHC 0

#define ASSIGNVP8(x, y, vp) vp = VBUF + (y) * BYTEWIDTH + (x);
#define ASSIGNVPOFFSET8(x, y, vp) vp = (y) * BYTEWIDTH + (x);
#define setpixel (*(currentcontext.ff.driver_setpixel_func))


/*extern GraphicsContext currentcontext; */

int px1, px2, py;
long c, cx;
long dx0, dy0;

/*Functions to draw a horizontal varying color line going left or right */

/*draw using setpixel: */
static void colhline_neg_setpixel (void)
{
    int count, y = py + dy0, x1 = px1 + dx0, x2 = px2 + dx0;
    if (__clip) {
	if (y < __clipy1 || y > __clipy2)
	    return;
	if (x1 > __clipx2 + 1) {
	    c -= (x1 - __clipx2 - 1) * cx;
	    x1 = __clipx2 + 1;
	}
	if (x2 < __clipx1) {
	    x2 = __clipx1;
	}
    }
    count = x1 - x2;
    if (count > 0) {
	do {
	    setpixel (--x1, y, c >> (SHLB + SHC));
	    c -= cx;
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
	    c += (__clipx1 - x1) * cx;
	    x1 = __clipx1;
	}
	if (x2 > __clipx2 + 1) {
	    x2 = __clipx2 + 1;
	}
    }
    count = x2 - x1;
    if (count > 0) {
	do {
	    setpixel (x1++, y, c >> (SHLB + SHC));
	    c += cx;
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
	    c -= (x1 - __clipx2 - 1) * cx;
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
	    *(VBUF + offst) = c >> (SHLB + SHC);
	    c -= cx;
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
	    c += (__clipx1 - x1) * cx;
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
	    *(VBUF + offst) = c >> (SHLB + SHC);
	    if (!(++offst))
		vga_setpage (++pg);
	    c += cx;
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
	    c -= (x1 - __clipx2 - 1) * cx;
	    x1 = __clipx2 + 1;
	}
	if (x2 < __clipx1) {
	    x2 = __clipx1;
	}
    }
    count = x1 - x2;
    ASSIGNVP8 (x1, y, vp);
    if (count > 0) {
	do {
	    *(--vp) = c >> (SHLB + SHC);
	    c -= cx;
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
	    c += (__clipx1 - x1) * cx;
	    x1 = __clipx1;
	}
	if (x2 > __clipx2 + 1) {
	    x2 = __clipx2 + 1;
	}
    }
    count = x2 - x1;
    ASSIGNVP8 (x1, y, vp);
    if (count > 0) {
	do {
	    *(vp++) = c >> (SHLB + SHC);
	    c += cx;
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


void gl_triangle (int x0, int y0, int z0, int x1, int y1, int z1, int x2, int y2, int z2, int bf)
{

    void (*colhline_pos) (void);
    void (*colhline_neg) (void);

    int t;
    int dir;
    int tz0, tz1;

    long nx, ny, nz;
    long cy;
    long g0, g1h = 0, g1l = 0;
    long c0;

    dir = 1;

/*Max triangle size in the order of (2^31) >> SHLB)^(.5) : */

    if ((nz = (x0 - x1) * (y0 - y2) - (y0 - y1) * (x0 - x2)) == 0)
	return;
    nx = ((y0 - y1) * (z0 - z2) - (z0 - z1) * (y0 - y2)) << SHLB;
    ny = ((z0 - z1) * (x0 - x2) - (x0 - x1) * (z0 - z2)) << SHLB;

    cx = -nx / nz;
    cy = -ny / nz;

    if ((abs (cx) > (6 << SHLB)) || (abs (cy) > (6 << SHLB))) {
	/*so that high colour gradients don't screw up at the edges. */
	/*4 is the maximum gradient per pixel. */

/* here we reduce the variation in color so that roundoff doesn't go
   off the scale */
/*      cx = (5 * cx) >> 3;
   cy = (5 * cy) >> 3;
   tz0 = (z0*6 + z1 + z2) << (SHLB - 3);
   tz1 = (z0 + z1*6 + z2) << (SHLB - 3);
   z2 = (z0 + z1 + z2*6) << (SHLB - 3);
   z0 = tz0;
   z1 = tz1;
 */

	cx >>= 2;
	cy >>= 2;
	tz0 = ((2 * z0 + z1 + z2) << SHLB) / 4;
	tz1 = ((z0 + 2 * z1 + z2) << SHLB) / 4;
	z2 = ((z0 + z1 + 2 * z2) << SHLB) / 4;
	z0 = tz0;
	z1 = tz1;

/*
   cx = cx >> 1;
   cy = cy >> 1;
   tz0 = (((z0 << 2) + z1 + z2) << SHLB) / 6;
   tz1 = ((z0 + (z1 << 2) + z2) << SHLB) / 6;
   z2 =  ((z0 + z1 + (z2 << 2)) << SHLB) / 6;
   z0 = tz0;
   z1 = tz1;
 */

/*
   cx = 0;
   cy = 0;
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

    if (nz > 0) {		/* nz is the cross product of the vectors of the two sides
				   it indicates whether the points were ordered clockwise
				   or anti-clockwise (you can find out which way by testing) */
	if (!bf)
	    return;
	dir++;
    } else {
	if (bf == 1)
	    return;
    }

    if (y1 < y0) {
	t = y1;
	y1 = y0;
	y0 = t;

	t = x1;
	x1 = x0;
	x0 = t;

	t = z1;
	z1 = z0;
	z0 = t;

	dir++;
    }
    if (y2 < y1) {
	t = y1;
	y1 = y2;
	y2 = t;

	t = x1;
	x1 = x2;
	x2 = t;

	t = z1;
	z1 = z2;
	z2 = t;

	dir++;
    }
    if (y1 < y0) {
	t = y1;
	y1 = y0;
	y0 = t;

	t = x1;
	x1 = x0;
	x0 = t;

	t = z1;
	z1 = z0;
	z0 = t;

	dir++;
    }
    c0 = z0;			/* << SHLB; */

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
		c = c0 + cx * px1 + cy * py;
		colhline_neg ();
	    }
	py = y1 - y0;

	px1 = ((g0 * py) + (abs (g0) >> 1)) >> SHLB;
	px2 = x1 - x0;
	c = c0 + cx * px1 + cy * py;
	colhline_neg ();
	if (y1 != y2)
	    for (py = y1 - y0 + 1; py <= y2 - y0; py++) {
		px1 = ((g0 * py) + (abs (g0) >> 1)) >> SHLB;
		px2 = (((g1l * (py - y1 + y0)) - (abs (g1l) >> 1)) >> SHLB) + x1 - x0;
		c = c0 + cx * px1 + cy * py;

		colhline_neg ();

	    }
    } else {
	if (y0 != y1)
	    for (py = 0; py < y1 - y0; py++) {
		px1 = ((g0 * py) - (abs (g0) >> 1)) >> SHLB;
		px2 = ((g1h * py) + (abs (g1h) >> 1)) >> SHLB;
		c = c0 + cx * px1 + cy * py;
		colhline_pos ();
	    }
	py = y1 - y0;

	px1 = ((g0 * py) - (abs (g0) >> 1)) >> SHLB;
	px2 = x1 - x0;
	c = c0 + cx * px1 + cy * py;

	colhline_pos ();
	if (y1 != y2)
	    for (py = y1 - y0 + 1; py <= y2 - y0; py++) {
		px1 = ((g0 * py) - (abs (g0) >> 1)) >> SHLB;
		px2 = (((g1l * (py - y1 + y0)) + (abs (g1l) >> 1)) >> SHLB) + x1 - x0;
		c = c0 + cx * px1 + cy * py;

		colhline_pos ();
	    }
    }

}
