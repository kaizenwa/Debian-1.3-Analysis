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

#include <stdio.h>

#include "defs.h"
#include "data.h"
#include "tile.h"

#if STATEBITS > 1
unsigned int maxstates;
#endif

cellcount_t chgpoints[MAXSTATES], chgrects[MAXSTATES];
XPoint points[MAXSTATES][MAXCHANGE];
XRectangle rects[MAXSTATES][MAXCHANGE];

int getcell(cellbox *ptr, const int xdx, const int ydx)
/* get state of cell xdx, ydx within box *ptr */
{
#if STATEBITS > 1
    if (maxstates > 2)
	return(ptr->nstate.cell[ydx][xdx]);
    else
#endif /* STATEBITS > 1 */
	if (ydx > 3)
	    return((ptr->twostate.live2 & 1 << ((ydx - 4)*8 + xdx)) !=0);
	else
	    return((ptr->twostate.live1 & 1 << (ydx*8 + xdx)) !=0);
}

void setcell(cellbox *ptr, const int xdx, const int ydx, const int val)
/* set state of cell xdx, ydx within box *ptr */
{
#if STATEBITS > 1
    if (maxstates > 2)
	ptr->nstate.cell[ydx][xdx] = val;
    else
#endif /* STATEBITS > 1 */
	if (val)
	{
	    if (ydx > 3)
		ptr->twostate.live2 |= 1 << ( (ydx - 4)*8  + xdx);
	    else
		ptr->twostate.live1 |= 1 << ( ydx*8  + xdx);
	}
	else
	{
	    if (ydx > 3)
		ptr->twostate.live2 &= 0xffffffff^(1 << ( (ydx - 4)*8  + xdx));
	    else
		ptr->twostate.live1 &= 0xffffffff^(1 << ( ydx*8  + xdx));
	}
}

void forget(cellbox *ptr)
/* remove a box's info about last generation's state */
{
#if STATEBITS > 1
    register int dy, dx;

    if (maxstates > 2)
    {
	for (dy = 0; dy < BOXSIZE; dy++)
	    for (dx = 0; dx < BOXSIZE; dx++)
		ptr->nstate.ocell[dy][dx] = 0;
    }
    else
#endif /* STATEBITS > 1 */
    {
	ptr->twostate.olive1 ^= ptr->twostate.olive1;
	ptr->twostate.olive2 ^= ptr->twostate.olive2;
    }
}

static void displayline(line,x,y,oline)
/* post changes in a half-cell to the X structure arrays */
u32bits line,x,y,oline;
{
    int yy;
    u32bits diff;
    diff=line ^ oline;
    if (scale == 1)
    {
	if (diff & 0x1)
	{
	    if (line & 0x1)
	    {
		points[1][chgpoints[1]].x = x - xpos;
		points[1][chgpoints[1]].y = y - ypos;
		chgpoints[1]++;
	    }
	    else
	    {
		points[0][chgpoints[0]].x = x - xpos;
		points[0][chgpoints[0]].y = y - ypos;
		chgpoints[0]++;
	    }
	}
	++x;
	if (diff & 0x2)
	{
	    if (line & 0x2)
	    {
		points[1][chgpoints[1]].x = x - xpos;
		points[1][chgpoints[1]].y = y - ypos;
		chgpoints[1]++;
	    }
	    else
	    {
		points[0][chgpoints[0]].x = x - xpos;
		points[0][chgpoints[0]].y = y - ypos;
		chgpoints[0]++;
	    }
	}
	++x;
	if (diff & 0x4)
	{
	    if (line & 0x4)
	    {
		points[1][chgpoints[1]].x = x - xpos;
		points[1][chgpoints[1]].y = y - ypos;
		chgpoints[1]++;
	    }
	    else
	    {
		points[0][chgpoints[0]].x = x - xpos;
		points[0][chgpoints[0]].y = y - ypos;
		chgpoints[0]++;
	    }
	}
	++x;
	if (diff & 0x8){
	    if (line & 0x8){
		points[1][chgpoints[1]].x = x - xpos;
		points[1][chgpoints[1]].y = y - ypos;
		chgpoints[1]++;
	    }
	    else
	    {
		points[0][chgpoints[0]].x = x - xpos;
		points[0][chgpoints[0]].y = y - ypos;
		chgpoints[0]++;
	    }
	}
	++x;
	if (diff & 0x10)
	{
	    if (line & 0x10)
	    {
		points[1][chgpoints[1]].x = x - xpos;
		points[1][chgpoints[1]].y = y - ypos;
		chgpoints[1]++;
	    }
	    else
	    {
		points[0][chgpoints[0]].x = x - xpos;
		points[0][chgpoints[0]].y = y - ypos;
		chgpoints[0]++;
	    }
	}
	++x;
	if (diff & 0x20)
	{
	    if (line & 0x20)
	    {
		points[1][chgpoints[1]].x = x - xpos;
		points[1][chgpoints[1]].y = y - ypos;
		chgpoints[1]++;
	    }
	    else
	    {
		points[0][chgpoints[0]].x = x - xpos;
		points[0][chgpoints[0]].y = y - ypos;
		chgpoints[0]++;
	    }
	}
	++x;
	if (diff & 0x40)
	{
	    if (line & 0x40)
	    {
		points[1][chgpoints[1]].x = x - xpos;
		points[1][chgpoints[1]].y = y - ypos;
		chgpoints[1]++;
	    }
	    else
	    {
		points[0][chgpoints[0]].x = x - xpos;
		points[0][chgpoints[0]].y = y - ypos;
		chgpoints[0]++;
	    }
	}
	++x;	
	if (diff & 0x80)
	{
	    if (line & 0x80)
	    {
		points[1][chgpoints[1]].x = x - xpos;
		points[1][chgpoints[1]].y = y - ypos;
		chgpoints[1]++;
	    }
	    else
	    {
		points[0][chgpoints[0]].x = x - xpos;
		points[0][chgpoints[0]].y = y - ypos;
		chgpoints[0]++;
	    }
	}
    }
    else
    {
	yy =((y - ypos) << (scale -1));
	if (diff & 0x1)
	{
	    if (line & 0x1)
	    {
		rects[1][chgrects[1]].x = ((x - xpos) << (scale -1));
		rects[1][chgrects[1]].y = yy;
		chgrects[1]++;
	    }
	    else
	    {
		rects[0][chgrects[0]].x = ((x - xpos) << (scale -1));
		rects[0][chgrects[0]].y = yy;
		chgrects[0]++;
	    }
	}
	++x;
	if (diff & 0x2)
	{
	    if (line & 0x2)
	    {
		rects[1][chgrects[1]].x = ((x - xpos) << (scale -1));
		rects[1][chgrects[1]].y = yy;
		chgrects[1]++;
	    }
	    else
	    {
		rects[0][chgrects[0]].x = ((x - xpos) << (scale -1));
		rects[0][chgrects[0]].y = yy;
		chgrects[0]++;
	    }
	}
	++x;
	if (diff & 0x4)
	{
	    if (line & 0x4)
	    {
		rects[1][chgrects[1]].x = ((x - xpos) << (scale -1));
		rects[1][chgrects[1]].y = yy;
		chgrects[1]++;
	    }
	    else
	    {
		rects[0][chgrects[0]].x = ((x - xpos) << (scale -1));
		rects[0][chgrects[0]].y = yy;
		chgrects[0]++;
	    }
	}
	++x;
	if (diff & 0x8)
	{
	    if (line & 0x8)
	    {
		rects[1][chgrects[1]].x = ((x - xpos) << (scale -1));
		rects[1][chgrects[1]].y = yy;
		chgrects[1]++;
	    }
	    else
	    {
		rects[0][chgrects[0]].x = ((x - xpos) << (scale -1));
		rects[0][chgrects[0]].y = yy;
		chgrects[0]++;
	    }
	}
	++x;
	if (diff & 0x10)
	{
	    if (line & 0x10)
	    {
		rects[1][chgrects[1]].x = ((x - xpos) << (scale -1));
		rects[1][chgrects[1]].y = yy;
		chgrects[1]++;
	    }
	    else
	    {
		rects[0][chgrects[0]].x = ((x - xpos) << (scale -1));
		rects[0][chgrects[0]].y = yy;
		chgrects[0]++;
	    }
	}
	++x;
	if (diff & 0x20)
	{
	    if (line & 0x20)
	    {
		rects[1][chgrects[1]].x = ((x - xpos) << (scale -1));
		rects[1][chgrects[1]].y = yy;
		chgrects[1]++;
	    }
	    else
	    {
		rects[0][chgrects[0]].x = ((x - xpos) << (scale -1));
		rects[0][chgrects[0]].y = yy;
		chgrects[0]++;
	    }
	}
	++x;
	if (diff & 0x40)
	{
	    if (line & 0x40)
	    {
		rects[1][chgrects[1]].x = ((x - xpos) << (scale -1));
		rects[1][chgrects[1]].y = yy;
		chgrects[1]++;
	    }
	    else
	    {
		rects[0][chgrects[0]].x = ((x - xpos) << (scale -1));
		rects[0][chgrects[0]].y = yy;
		chgrects[0]++;
	    }
	}
	++x;
	if (diff & 0x80)
	{
	    if (line & 0x80)
	    {
		rects[1][chgrects[1]].x = ((x - xpos) << (scale -1));
		rects[1][chgrects[1]].y = yy;
		chgrects[1]++;
	    }
	    else
	    {
		rects[0][chgrects[0]].x = ((x  - xpos) << (scale -1));
		rects[0][chgrects[0]].y = yy;
		chgrects[0]++;
	    }
	}
    }
	
}

void displaybox(u32bits x, u32bits y, cellbox *ptr)
/* post changes in a tile's state to the change arrays */
{
#if STATEBITS > 1
    register int i, j;

    if (maxstates > 2)
    {
	for (i = 0; i < BOXSIZE; i++)
	    for (j = 0; j < BOXSIZE; j++)
	    {
		int ns = ptr->nstate.cell[i][j];

		if (ns != ptr->nstate.ocell[i][j])
		    if (scale == 1)
		    {
			points[ns][chgpoints[ns]].x = x+j - xpos;
			points[ns][chgpoints[ns]].y = y+i - ypos;
			chgpoints[ns]++;
		    }
		    else
		    {
			rects[ns][chgrects[ns]].x = ((x+j-xpos) << (scale-1));
			rects[ns][chgrects[ns]].y = ((y+i-ypos) << (scale-1));
			chgrects[ns]++;
		    }
	    }
    }
    else
#endif /* STATEBITS > 1 */
    {
	register u32bits live1 = ptr->twostate.live1;
	register u32bits long live2 = ptr->twostate.live2;
	register u32bits long olive1 = ptr->twostate.olive1;
	register u32bits long olive2 = ptr->twostate.olive2;

	displayline(live1,x,y,olive1);
	displayline(live1>>8,x,++y,olive1>>8);
	displayline(live1>>16,x,++y,olive1>>16);
	displayline(live1>>24,x,++y,olive1>>24);
	displayline(live2,x,++y,olive2);
	displayline(live2>>8,x,++y,olive2>>8);
	displayline(live2>>16,x,++y,olive2>>16);
	displayline(live2>>24,x,++y,olive2>>24);
    }
}


#define	TX(x, y)	(tx(x,y,txx,txy)+loadx-xpos)
#define TY(x, y)	(ty(x,y,tyx,tyy)+loady-ypos)

static void trdisplayline(line,x,y,oline)
/* post changes in a half-cell to the X structure arrays */
/* use global geometric transformations */
u32bits line,oline;
long x,y;
{
    int sc,yy;
    u32bits diff;
    sc = (1 << (scale -1)) - (scale > 2);
    diff = line ^ oline;
    if (scale == 1)
    {
	if (diff & 0x1)
	{
	    if (line & 0x1)
	    {
		points[1][chgpoints[1]].x = TX(x, y);
		points[1][chgpoints[1]].y = TY(x, y);
		chgpoints[1]++;
	    }
	    else
	    {
		points[0][chgpoints[0]].x = TX(x, y);
		points[0][chgpoints[0]].y = TY(x, y);
		chgpoints[0]++;
	    }
	}
	++x;
	if (diff & 0x2)
	{
	    if (line & 0x2)
	    {
		points[1][chgpoints[1]].x = TX(x, y);
		points[1][chgpoints[1]].y = TY(x, y);
		chgpoints[1]++;
	    }
	    else
	    {
		points[0][chgpoints[0]].x = TX(x, y);
		points[0][chgpoints[0]].y = TY(x, y);
		chgpoints[0]++;
	    }
	}
	++x;
	if (diff & 0x4)
	{
	    if (line & 0x4)
	    {
		points[1][chgpoints[1]].x = TX(x, y);
		points[1][chgpoints[1]].y = TY(x, y);
		chgpoints[1]++;
	    }
	    else
	    {
		points[0][chgpoints[0]].x = TX(x, y);
		points[0][chgpoints[0]].y = TY(x, y);
		chgpoints[0]++;
	    }
	}
	++x;
	if (diff & 0x8)
	{
	    if (line & 0x8)
	    {
		points[1][chgpoints[1]].x = TX(x, y);
		points[1][chgpoints[1]].y = TY(x, y);
		chgpoints[1]++;
	    }
	    else
	    {
		points[0][chgpoints[0]].x = TX(x, y);
		points[0][chgpoints[0]].y = TY(x, y);
		chgpoints[0]++;
	    }
	}
	++x;
	if (diff & 0x10)
	{
	    if (line & 0x10)
	    {
		points[1][chgpoints[1]].x = TX(x, y);
		points[1][chgpoints[1]].y = TY(x, y);
		chgpoints[1]++;
	    }
	    else
	    {
		points[0][chgpoints[0]].x = TX(x, y);
		points[0][chgpoints[0]].y = TY(x, y);
		chgpoints[0]++;
	    }
	}
	++x;
	if (diff & 0x20)
	{
	    if (line & 0x20)
	    {
		points[1][chgpoints[1]].x = TX(x, y);
		points[1][chgpoints[1]].y = TY(x, y);
		chgpoints[1]++;
	    }
	    else
	    {
		points[0][chgpoints[0]].x = TX(x, y);
		points[0][chgpoints[0]].y = TY(x, y);
		chgpoints[0]++;
	    }
	}
	++x;
	if (diff & 0x40)
	{
	    if (line & 0x40)
	    {
		points[1][chgpoints[1]].x = TX(x, y);
		points[1][chgpoints[1]].y = TY(x, y);
		chgpoints[1]++;
	    }
	    else
	    {
		points[0][chgpoints[0]].x = TX(x, y);
		points[0][chgpoints[0]].y = TY(x, y);
		chgpoints[0]++;
	    }
	}
	++x;	
	if (diff & 0x80)
	{
	    if (line & 0x80)
	    {
		points[1][chgpoints[1]].x = TX(x, y);
		points[1][chgpoints[1]].y = TY(x, y);
		chgpoints[1]++;
	    }
	    else
	    {
		points[0][chgpoints[0]].x = TX(x, y);
		points[0][chgpoints[0]].y = TY(x, y);
		chgpoints[0]++;
	    }
	}
    }
    else
    {
	if (diff & 0x1)
	{
	    if (line & 0x1)
	    {
		rects[1][chgrects[1]].x = (TX(x, y) << (scale -1));
		rects[1][chgrects[1]].y = (TY(x, y) << (scale -1));
		chgrects[1]++;
	    }
	    else
	    {
		rects[0][chgrects[0]].x = (TX(x, y) << (scale -1));
		rects[0][chgrects[0]].y = (TY(x, y) << (scale -1));
		chgrects[0]++;
	    }
	}
	++x;
	if (diff & 0x2)
	{
	    if (line & 0x2)
	    {
		rects[1][chgrects[1]].x = (TX(x, y) << (scale -1));
		rects[1][chgrects[1]].y = (TY(x, y) << (scale -1));
		chgrects[1]++;
	    }
	    else
	    {
		rects[0][chgrects[0]].x = (TX(x, y) << (scale -1));
		rects[0][chgrects[0]].y = (TY(x, y) << (scale -1));
		chgrects[0]++;
	    }
	}
	++x;
	if (diff & 0x4)
	{
	    if (line & 0x4)
	    {
		rects[1][chgrects[1]].x = (TX(x, y) << (scale -1));
		rects[1][chgrects[1]].y = (TY(x, y) << (scale -1));
		chgrects[1]++;
	    }
	    else
	    {
		rects[0][chgrects[0]].x = (TX(x, y) << (scale -1));
		rects[0][chgrects[0]].y = (TY(x, y) << (scale -1));
		chgrects[0]++;
	    }
	}
	++x;
	if (diff & 0x8)
	{
	    if (line & 0x8)
	    {
		rects[1][chgrects[1]].x = (TX(x, y) << (scale -1));
		rects[1][chgrects[1]].y = (TY(x, y) << (scale -1));
		chgrects[1]++;
	    }
	    else
	    {
		rects[0][chgrects[0]].x = (TX(x, y) << (scale -1));
		rects[0][chgrects[0]].y = (TY(x, y) << (scale -1));
		chgrects[0]++;
	    }
	}
	++x;
	if (diff & 0x10)
	{
	    if (line & 0x10)
	    {
		rects[1][chgrects[1]].x = (TX(x, y) << (scale -1));
		rects[1][chgrects[1]].y = (TY(x, y) << (scale -1));
		chgrects[1]++;
	    }
	    else
	    {
		rects[0][chgrects[0]].x = (TX(x, y) << (scale -1));
		rects[0][chgrects[0]].y = (TY(x, y) << (scale -1));
		chgrects[0]++;
	    }
	}
	++x;
	if (diff & 0x20)
	{
	    if (line & 0x20)
	    {
		rects[1][chgrects[1]].x = (TX(x, y) << (scale -1));
		rects[1][chgrects[1]].y = (TY(x, y) << (scale -1));
		chgrects[1]++;
	    }
	    else
	    {
		rects[0][chgrects[0]].x = (TX(x, y) << (scale -1));
		rects[0][chgrects[0]].y = (TY(x, y) << (scale -1));
		rects[0][chgrects[0]].width = sc;
		rects[0][chgrects[0]].height = sc;
		chgrects[0]++;
	    }
	}
	++x;
	if (diff & 0x40)
	{
	    if (line & 0x40)
	    {
		rects[1][chgrects[1]].x = (TX(x, y) << (scale -1));
		rects[1][chgrects[1]].y = (TY(x, y) << (scale -1));
		chgrects[1]++;
	    }
	    else
	    {
		rects[0][chgrects[0]].x = (TX(x, y) << (scale -1));
		rects[0][chgrects[0]].y = (TY(x, y) << (scale -1));
		chgrects[0]++;
	    }
	}
	++x;
	if (diff & 0x80)
	{
	    if (line & 0x80)
	    {
		rects[1][chgrects[1]].x = (TX(x, y) << (scale -1));
		rects[1][chgrects[1]].y = (TY(x, y) << (scale -1));
		chgrects[1]++;
	    }
	    else
	    {
		rects[0][chgrects[0]].x = (TX(x, y) << (scale -1));
		rects[0][chgrects[0]].y = (TY(x, y) << (scale -1));
		chgrects[0]++;
	    }
	}
    }
	
}

void trdisplaybox(u32bits x, u32bits y, cellbox *ptr)
{
#if STATEBITS > 1
    register int i, j;

    if (maxstates > 2)
    {
	for (i = 0; i < BOXSIZE; i++)
	    for (j = 0; j < BOXSIZE; j++)
	    {
		int ns = ptr->nstate.cell[i][j];

		if (ns != ptr->nstate.ocell[i][j])
		    if (scale == 1)
		    {
			points[ns][chgpoints[ns]].x = TX(x+j,y+i);
			points[ns][chgpoints[ns]].y = TY(x+j,y+i);
			chgpoints[ns]++;
		    }
		    else
		    {
			rects[ns][chgrects[ns]].x = (TX(x+j,y+i) << (scale-1));
			rects[ns][chgrects[ns]].y = (TY(x+j,y+i) << (scale-1));
			chgrects[ns]++;
		    }
	    }
    }
    else
#endif /* STATEBITS > 1 */
    {
	register u32bits live1 = ptr->twostate.live1;
	register u32bits live2 = ptr->twostate.live2;
	register u32bits olive1 = ptr->twostate.olive1;
	register u32bits olive2 = ptr->twostate.olive2;

	trdisplayline(live1,x,y,olive1);
	trdisplayline(live1>>8,x,++y,olive1>>8);
	trdisplayline(live1>>16,x,++y,olive1>>16);
	trdisplayline(live1>>24,x,++y,olive1>>24);
	trdisplayline(live2,x,++y,olive2);
	trdisplayline(live2>>8,x,++y,olive2>>8);
	trdisplayline(live2>>16,x,++y,olive2>>16);
	trdisplayline(live2>>24,x,++y,olive2>>24);
    }    
}

void trdrawbox(coord_t xmin,coord_t ymin,coord_t xmax,coord_t ymax, int color)
/* box the area defined by two corners (using global transform) */
{
    int	x1, y1, x2, y2;

    xmin -= xorigin;
    xmax -= xorigin;
    ymin -= yorigin;
    ymax -= yorigin;
    x1 = tx(xmin, ymin,txx,txy) + loadx;
    y1 = ty(xmin, ymin,tyx,tyy) + loady;
    x2 = tx(xmax, ymax,txx,txy) + loadx;
    y2 = ty(xmax, ymax,tyx,tyy) + loady;

    drawbox(x1, y1, x2, y2, color);
}

/* cell.c ends here */
