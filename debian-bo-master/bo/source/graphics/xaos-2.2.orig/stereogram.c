#ifdef _plan9_
#include <u.h>
#include <stdio.h>
#include <libc.h>
#else
#include <stdlib.h>
#include <math.h>
#endif
#include "config.h"
#include "zoom.h"
#define PIXELWIDTH (d->pixelwidth)	/*all distances in cm */
#define PIXELHEIGHT (d->pixelheight)
#define USER_DIST  (60.0)
/*#define INDEX_DIST (0.3) */
#define INDEX_DIST (0.3)
#define EYE_DIST   (8.5)
#define START1 (60.0)
/*#define FNC(x) pow(x,0.5) */
#define FNC(x) x
void do_stereogram(zoom_context * d)
{
    int table[256];
    int i, y, lc;
    register unsigned char *cs, *c, *c1, *ce, *src, *src1;
    static int minc = 0;
    double start, maxdist, dist;
    dist = (d->s.mc - d->s.nc) / 2;
    maxdist = INDEX_DIST * FNC(d->num_colors) + START1;
    do {
	start = dist * maxdist - INDEX_DIST * FNC( /*(d->num_colors+minc)/2 */ minc);
	maxdist *= 5;
    } while (start + INDEX_DIST * (FNC(minc)) < 25.0);
    for (i = 0; i < d->num_colors; i++) {
	double dist;
	if (i != 0)
	    dist = i;
	else
	    dist = d->num_colors;
	dist = INDEX_DIST * (FNC(dist)) + start;
#ifdef SHIFT
	table[d->colors[i]] = 1024 * EYE_DIST * dist / (dist + USER_DIST) / PIXELWIDTH;
#else
	table[d->colors[i]] = EYE_DIST * dist / (dist + USER_DIST) / PIXELWIDTH;
#endif
    }
    minc = 255;
    for (i = 0; i < d->height; i++) {
	c1 = d->vbuff + i * d->scanline;
	c = cs = d->svbuff + i * d->scanline;
	ce = cs + d->width;
	src = src1 = c;
	lc = 1024;
	while (c < ce) {
	    y = *c1;
	    if (y == lc)
		src++;
	    else {
		lc = y;
		if (y < minc && y != 0)
		    minc = y;
		y = table[y];
#ifdef SHIFT
		src = c - (y >> 10);
#else
		src = c - y;
#endif
	    }
	    if (src < src1) {
		*c = /*cs-src */ d->colors[240 + (rand() & 15)];
	    } else {
		/*if((y&1023)>(rand()%1024)) src--; */
		if (src <= cs)
		    *c = d->colors[240 + (rand() & 15) /*cs-src */ ];
		else
		    *c = *src;
		src1 = src;
	    }
	    c++;
	    c1++;
	}
    }
}
