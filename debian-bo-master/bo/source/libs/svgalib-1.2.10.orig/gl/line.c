/* Framebuffer Graphics Libary for Linux, Copyright 1993 Harm Hanemaayer */
/* line.c       Line drawing */


#include <stdlib.h>
#include <vga.h>
#include "inlstring.h"		/* include inline string operations */

#include "vgagl.h"
#include "def.h"
#include "driver.h"


#ifdef __alpha__

static inline int muldiv64(int m1, int m2, int d)
{
    return (long) m1 *(long) m2 / (long) d;
}

#else

/* We use the 32-bit to 64-bit multiply and 64-bit to 32-bit divide of the */
/* 386 (which gcc doesn't know well enough) to efficiently perform integer */
/* scaling without having to worry about overflows. */

static inline int muldiv64(int m1, int m2, int d)
{
/* int32 * int32 -> int64 / int32 -> int32 */
    int result;
    __asm__(
	       "imull %%edx\n\t"
	       "idivl %3\n\t"
  :	       "=a"(result)	/* out */
  :	       "a"(m1), "d"(m2), "g"(d)		/* in */
  :	       "ax", "dx"	/* mod */
	);
    return result;
}

#endif				/* !__alpha__ */

#ifdef __alpha__

static inline int gl_regioncode(int x, int y)
{
    int result = 0;
    if (x < __clipx1)
	result |= 1;
    else if (x > __clipx2)
	result |= 2;
    if (y < __clipy1)
	result |= 4;
    else if (y > __clipy2)
	result |= 8;
    return result;
}

#else

#define INC_IF_NEG(y, result)				\
{							\
	__asm__("btl $31,%1\n\t"			\
		"adcl $0,%0"				\
		: "=r" ((int) result)			\
		: "rm" ((int) (y)), "0" ((int) result)	\
		);					\
}

static inline int gl_regioncode(int x, int y)
{
    int dx1, dx2, dy1, dy2;
    int result;
    result = 0;
    dy2 = __clipy2 - y;
    INC_IF_NEG(dy2, result);
    result <<= 1;
    dy1 = y - __clipy1;
    INC_IF_NEG(dy1, result);
    result <<= 1;
    dx2 = __clipx2 - x;
    INC_IF_NEG(dx2, result);
    result <<= 1;
    dx1 = x - __clipx1;
    INC_IF_NEG(dx1, result);
    return result;
}

#endif				/* __alpha__ */

/* Partly based on vgalib by Tommy Frandsen */
/* This would be a lot faster if setpixel was inlined */

void gl_line(int x1, int y1, int x2, int y2, int c)
{
    int dx, dy, ax, ay, sx, sy, x, y;

    if (__clip)
	/* Cohen & Sutherland algorithm */
	for (;;) {
	    int r1 = gl_regioncode(x1, y1);
	    int r2 = gl_regioncode(x2, y2);
	    if (!(r1 | r2))
		break;		/* completely inside */
	    if (r1 & r2)
		return;		/* completely outside */
	    if (r1 == 0) {
		swap(x1, x2);	/* make sure first */
		swap(y1, y2);	/* point is outside */
		r1 = r2;
	    }
	    if (r1 & 1) {	/* left */
		y1 += muldiv64(__clipx1 - x1, y2 - y1, x2 - x1);
		x1 = __clipx1;
	    } else if (r1 & 2) {	/* right */
		y1 += muldiv64(__clipx2 - x1, y2 - y1, x2 - x1);
		x1 = __clipx2;
	    } else if (r1 & 4) {	/* top */
		x1 += muldiv64(__clipy1 - y1, x2 - x1, y2 - y1);
		y1 = __clipy1;
	    } else if (r1 & 8) {	/* bottom */
		x1 += muldiv64(__clipy2 - y1, x2 - x1, y2 - y1);
		y1 = __clipy2;
	    }
	}
    dx = x2 - x1;
    dy = y2 - y1;
    ax = abs(dx) << 1;
    ay = abs(dy) << 1;
    sx = (dx >= 0) ? 1 : -1;
    sy = (dy >= 0) ? 1 : -1;
    x = x1;
    y = y1;

    if (ax > ay) {
	int d = ay - (ax >> 1);
	while (x != x2) {
	    setpixel(x, y, c);

	    if (d > 0 || (d == 0 && sx == 1)) {
		y += sy;
		d -= ax;
	    }
	    x += sx;
	    d += ay;
	}
    } else {
	int sy = (dy >= 0) ? 1 : -1;
	int d = ax - (ay >> 1);
	while (y != y2) {
	    setpixel(x, y, c);

	    if (d > 0 || (d == 0 && sy == 1)) {
		x += sx;
		d -= ay;
	    }
	    y += sy;
	    d += ax;
	}
    }
    setpixel(x, y, c);
}


static void gl_setcirclepixels(int x, int y, int sx, int sy, int c)
{
    if (__clip) {
	int z = max(x, y);
	if (sx - z < __clipx1 || sx + z > __clipx2
	    || sy - z < __clipy1 || sy + z > __clipy2) {
	    /* use setpixel clipping */
	    gl_setpixel(sx + x, sy + y, c);
	    gl_setpixel(sx - x, sy + y, c);
	    gl_setpixel(sx + x, sy - y, c);
	    gl_setpixel(sx - x, sy - y, c);
	    gl_setpixel(sx + y, sy + x, c);
	    gl_setpixel(sx - y, sy + x, c);
	    gl_setpixel(sx + y, sy - x, c);
	    gl_setpixel(sx - y, sy - x, c);
	    return;
	}
    }
    setpixel(sx + x, sy + y, c);
    setpixel(sx - x, sy + y, c);
    setpixel(sx + x, sy - y, c);
    setpixel(sx - x, sy - y, c);
    setpixel(sx + y, sy + x, c);
    setpixel(sx - y, sy + x, c);
    setpixel(sx + y, sy - x, c);
    setpixel(sx - y, sy - x, c);
}

void gl_circle(int sx, int sy, int r, int c)
{
    int x, y, d;
    if (r < 1) {
	gl_setpixel(sx, sy, c);
	return;
    }
    if (__clip)
	if (sx + r < __clipx1 || sx - r > __clipx2
	    || sy + r < __clipy1 || sy - r > __clipy2)
	    return;
    x = 0;
    y = r;
    d = 1 - r;
    gl_setcirclepixels(x, y, sx, sy, c);
    while (x < y) {
	if (d < 0)
	    d += x * 2 + 3;
	else {
	    d += x * 2 - y * 2 + 5;
	    y--;
	}
	x++;
	gl_setcirclepixels(x, y, sx, sy, c);
    }
}
