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



/* File striangle.c 

   Comments or suggestions welcome.

   The following routine draws a solid flat triangle using gl_hline
   It takes eight parameters: the vertices, the color,
   and a value to tell whether the
   triangle should be drawn (or not drawn) if the points are clockwise (or
   anti-clockwise). See triangle.c for more info.

*/

#include <vga.h>
#include <vgagl.h>

#define SHLB 8

/* Scales the colour by powers of two: */
#define SHC 0


/*extern GraphicsContext currentcontext; */


void gl_striangle (int x0, int y0, int x1, int y1, int x2, int y2, int color, int bf)
{
    int t;
    int dir;

    long nz;
    long g0, g1h = 0, g1l = 0;
    int px1, px2, py;
    long c;

    dir = 1;
    c = color;

/*Max triangle size in the order of (2^31) >> SHLB)^(.5) : */

    if ((nz = (x0 - x1) * (y0 - y2) - (y0 - y1) * (x0 - x2)) == 0)
	return;

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

	dir++;
    }
    if (y2 < y1) {
	t = y1;
	y1 = y2;
	y2 = t;

	t = x1;
	x1 = x2;
	x2 = t;

	dir++;
    }
    if (y1 < y0) {
	t = y1;
	y1 = y0;
	y0 = t;

	t = x1;
	x1 = x0;
	x0 = t;

	dir++;
    }
    if (y2 == y0)
	return;
    g0 = ((long) (x2 - x0) << SHLB) / (y2 - y0);
    if (y1 != y0)
	g1h = ((long) (x1 - x0) << SHLB) / (y1 - y0);
    if (y2 != y1)
	g1l = ((long) (x2 - x1) << SHLB) / (y2 - y1);

    dir = dir & 1;


    if (__clip) {
/* Very large triangles (larger than the screen) sometimes become a problem,
   if so: */
	if (((abs (x0 - x1) + abs (x1 - x2) + abs (x0 - x2)) >
	     ((__clipx2 - __clipx1) * 2)) || ((y2 - y0) > (__clipy2 - __clipy1)))
	    return;

	if (y2 < __clipy1 || y0 > __clipy2 ||
	    (x0 < __clipx1 && x1 < __clipx1 && x2 < __clipx1) ||
	    (x0 > __clipx2 && x1 > __clipx2 && x2 > __clipx2))
	    return;
    }
    if (dir == 1) {
	if (y1 != y0)
	    for (py = 0; py < y1 - y0; py++) {
		px1 = ((g0 * py) + (abs (g0) >> 1)) >> SHLB;
		px2 = ((g1h * py) - (abs (g1h) >> 1)) >> SHLB;
		gl_hline (x0 + px2, y0 + py, x0 + px1 - 1, c);
	    }
	py = y1 - y0;
	px1 = ((g0 * py) + (abs (g0) >> 1)) >> SHLB;
	gl_hline (x1, y0 + py, x0 + px1 - 1, c);
	if (y1 != y2)
	    for (py = y1 - y0 + 1; py <= y2 - y0; py++) {
		px1 = ((g0 * py) + (abs (g0) >> 1)) >> SHLB;
		px2 = (((g1l * (py - y1 + y0)) - (abs (g1l) >> 1)) >> SHLB) + x1 - x0;
		gl_hline (x0 + px2, y0 + py, x0 + px1 - 1, c);
	    }
    } else {
	if (y0 != y1)
	    for (py = 0; py < y1 - y0; py++) {
		px1 = ((g0 * py) - (abs (g0) >> 1)) >> SHLB;
		px2 = ((g1h * py) + (abs (g1h) >> 1)) >> SHLB;
		gl_hline (x0 + px1, y0 + py, x0 + px2 - 1, c);
	    }
	py = y1 - y0;
	px1 = ((g0 * py) - (abs (g0) >> 1)) >> SHLB;
	gl_hline (x0 + px1, y0 + py, x1 - 1, c);
	if (y1 != y2)
	    for (py = y1 - y0 + 1; py <= y2 - y0; py++) {
		px1 = ((g0 * py) - (abs (g0) >> 1)) >> SHLB;
		px2 = (((g1l * (py - y1 + y0)) + (abs (g1l) >> 1)) >> SHLB) + x1;
		gl_hline (x0 + px1, y0 + py, px2 - 1, c);
	    }
    }

}
