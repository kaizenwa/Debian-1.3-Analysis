
/* 
 *     XaoS, a fast portable realtime fractal zoomer 
 *                  Copyright (C) 1996,1997 by
 *
 *      Jan Hubicka          (hubicka@paru.cas.cz)
 *      Thomas Marsh         (tmarsh@austin.ibm.com)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
#ifdef _plan9_
#include <u.h>
#include <libc.h>
#else
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "config.h"
#include <assert.h>
#endif
#include "config.h"
#include "zoom.h"
#include "palette.h"


#define SEGMENTSIZE 8
#define DEFNSEGMENTS (255/8)
#define MAXNSEGMENTS (255/4)
#define NSEGMENTS ((255/segmentsize))
static int segmentsize;


static unsigned char colors[256][3];
static CONST unsigned char colors1[DEFNSEGMENTS][3] =
{
    {8, 14, 32},
    {120, 119, 238},
    {24, 7, 25},
    {197, 66, 28},
    {29, 18, 11},
    {135, 46, 71},
    {24, 27, 13},
    {241, 230, 128},
    {17, 31, 24},
    {240, 162, 139},
    {11, 4, 30},
    {106, 87, 189},
    {29, 21, 14},
    {12, 140, 118},
    {10, 6, 29},
    {50, 144, 77},
    {22, 0, 24},
    {148, 188, 243},
    {4, 32, 7},
    {231, 146, 14},
    {10, 13, 20},
    {184, 147, 68},
    {13, 28, 3},
    {169, 248, 152},
    {4, 0, 34},
    {62, 83, 48},
    {7, 21, 22},
    {152, 97, 184},
    {8, 3, 12},
    {247, 92, 235},
    {31, 32, 16}
};
static zoom_context *context;
static int (*set_color) (int, int, int, int);
static int needupdate;
static int allocate(int r, int g, int b, int init, int index)
{
    int n;
    if ((n = set_color((int) r, (int) g, (int) b, init)) == -1)
	return 0;
    n = (unsigned char) n;
    if (context->colors[index] != n)
	needupdate = 1;
    context->colors[index] = n;
    context->cmap[0][n] = (unsigned char) r;
    context->cmap[1][n] = (unsigned char) g;
    context->cmap[2][n] = (unsigned char) b;
    return (1);
}

static int mksmooth(int nsegments)
{
    int i, y;
    float r, g, b, rs, gs, bs;

    for (i = 0; i < nsegments; i++) {
	r = colors[i][0];
	g = colors[i][1];
	b = colors[i][2];
	rs = (colors[(i + 1) % nsegments][0] - r) / segmentsize;
	gs = (colors[(i + 1) % nsegments][1] - g) / segmentsize;
	bs = (colors[(i + 1) % nsegments][2] - b) / segmentsize;
	context->num_colors = i * segmentsize;
	for (y = 0; y < segmentsize; y++) {
	    if (!allocate((int) r, (int) g, (int) b, i == 0 && y == 0, i * segmentsize + y)) {
		if (!i)
		    context->num_colors = 2;
		return 0;
	    }
	    r += rs;
	    g += gs;
	    b += bs;
	}
    }
    context->num_colors = i * segmentsize;
    return 1;
}

static void randomize_segments(int whitemode, int nsegments)
{
    int i = 0;

    if (whitemode) {
	colors[0][0] = 255,
	    colors[0][1] = 255,
	    colors[0][2] = 255;
	for (i = 0; i < nsegments; i += 2) {
	    if (i != 0) {
		colors[i][0] = rand() % 256,
		    colors[i][1] = rand() % 256,
		    colors[i][2] = rand() % 256;
	    }
	    if (i + 1 < nsegments)
		colors[i + 1][0] = rand() % 35,
		    colors[i + 1][1] = rand() % 35,
		    colors[i + 1][2] = rand() % 35;
	}
    } else {
	for (i = 0; i < nsegments; i += 2) {
	    colors[i][0] = rand() % 35,
		colors[i][1] = rand() % 35,
		colors[i][2] = rand() % 35;
	    if (i + 1 < nsegments)
		colors[i + 1][0] = rand() % 256,
		    colors[i + 1][1] = rand() % 256,
		    colors[i + 1][2] = rand() % 256;
	}
    }
    colors[i - 1][0] = colors[0][0];
    colors[i - 1][1] = colors[0][1];
    colors[i - 1][2] = colors[0][2];
}

int mkpalette(zoom_context * c, int (*sethandler) (int, int, int, int), int randomsize)
{
    int i, col = 0, ncolors = c->num_colors;
    int whitemode = rand() % 2;
    context = c;
    needupdate = 0;
    if (randomsize || !context->num_colors) {
	segmentsize = pow(2.0, (double) (rand() % 4 + 2));
    } else {
	if (context->num_colors > 8) {
	    i = 0;
	    do {
		segmentsize = rand() % context->num_colors / 2;
		i++;
		if (segmentsize < 8)
		    segmentsize = 8;
	    } while ((context->num_colors) % segmentsize && i < 1000000);
	    col = context->num_colors;
	}
    }
    checkcontext(c);
    assert(sethandler != NULL);
    set_color = sethandler;
    context = c;
    if (randomsize)
	i = rand() % NSEGMENTS;
    else
	i = NSEGMENTS;
    if (i < 0)
	i = 1;
    randomize_segments(whitemode, i);
    mksmooth(i);
    if (!randomsize) {
	if (context->num_colors > col)
	    context->num_colors = col;
    }
    return (context->num_colors != ncolors || needupdate);
}

/*FIXME: hack!!!! */
int mkstereogrampalette(zoom_context * c, int (*sethandler) (int, int, int, int))
{
    int i, ncolors = c->num_colors;
    context = c;
    needupdate = 0;
    for (i = 1; i < 240; i++)
	c->colors[i] = i;
    c->colors[0] = 240;
    allocate(0, 0, 0, 1, 240);
    for (i = 1; i < 16; i++)
	allocate(i * 4, i * 4, i * 16, 0, 240 + i);
    c->num_colors = 239;
    return (context->num_colors != ncolors || needupdate);
}

int mkdefaultpalette(zoom_context * c, int (*sethandler) (int, int, int, int), int randomsize)
{
    int i, ncolors = c->num_colors;
    context = c;
    needupdate = 0;
    checkcontext(c);
    segmentsize = 8;
    assert(sethandler != NULL);

    set_color = sethandler;
    context = c;
    memcpy(colors, colors1, sizeof(colors));
    if (randomsize)
	i = 128 / 8;
    else
	i = NSEGMENTS;
    if (i < 0)
	i = 1;
    mksmooth(i);
    return (context->num_colors != ncolors || needupdate);
}
