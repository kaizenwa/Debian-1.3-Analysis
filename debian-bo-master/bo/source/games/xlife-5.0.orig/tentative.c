/*
 * XLife Copyright 1989 Jon Bennett jb7m+@andrew.cmu.edu, jcrb@cs.cmu.edu
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of the copyright holders not be used in
 * advertising or publicity pertaining to distribution of the software without
 * specific, written prior permission.  The copyright holders make no
 * representations about the suitability of this software for any purpose.  It
 * is provided "as is" without express or implied warranty.
 *
 * THE COPYRIGHT HOLDERS DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 */

#include "defs.h"
#include "tile.h"

void moveload(void)
/* Move loaded pattern to active cell */
{
    loadx = XPOS(event.xmotion.x,xpos);
    loady = YPOS(event.xmotion.y,ypos);
    XClearWindow(disp,lifew);
    redrawscreen();
}

void turnload(void)
/* Turn loaded pattern 90 degrees about its origin */
{
    int t; 

    t =- tyx; tyx = txx; txx = t;
    t =- tyy; tyy = txy; txy = t;
    XClearWindow(disp,lifew);
    redrawscreen();
} 

void flipload(void)
/* Flip pattern about its x axis */
{
    int t; 

    tyx =- tyx;
    tyy =- tyy;
    XClearWindow(disp,lifew);
    redrawscreen();
} 

void boxpattern(const int color)
/* Draw a bounding box in a given color around a tentative pattern */
{
    coord_t xmin, ymin, xmax, ymax;

    bounding_box(&tentative, &xmin, &ymin, &xmax, &ymax);

    trdrawbox(xmin, ymin, xmax, ymax, color);
}

void make_tentative(coord_t x1, coord_t y1, coord_t x2, coord_t y2)
/* make all active cells in a bounding box tentative */
{
    coord_t xmin, ymin, xmax, ymax;
    tile *ptr;
    int dx, dy;

    xmin    = (x1 > x2) ? x2 : x1;
    ymin    = (y1 > y2) ? y2 : y1;
    xmax    = (x1 > x2) ? x1 : x2;
    ymax    = (y1 > y2) ? y1 : y2;

    for (ptr = active.tiles; ptr != NULL; ptr = ptr->next)
    {
	int	state;

	if (ptr->dead)
	    continue;
	else if (ptr->x + BOXSIZE < xmin
		 || ptr->y + BOXSIZE < ymin 
		 || ptr->x > xmax
		 || ptr->y > ymax)
	    continue;
 
	for (dx = 0; dx < BOXSIZE; dx++)
	    for (dy = 0; dy < BOXSIZE; dy++)
		if (ptr->x + dx < xmin || ptr->y + dy < ymin 
			 || ptr->x + dx > xmax || ptr->y + dy > ymax)
		    continue;
		else if (state = getcell(&ptr->cells, dx, dy))
		{
		    setcell(&ptr->cells, dx, dy, 0);
		    chgcell(&tentative, ptr->x + dx, ptr->y + dy, state);
		}
    }
}

/* tentative.c ends here */
