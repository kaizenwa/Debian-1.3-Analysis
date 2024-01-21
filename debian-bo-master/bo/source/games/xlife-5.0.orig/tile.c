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

/*
 * This module encapsulates cell-box handling. Everything else in the program
 * except possibly the evolution function should go through these functions
 * to change the board state. Entry points are:
 *
 * void initcells(context)        -- initialize active pattern
 *
 * int lookcell(context, x, y)    -- get state of cell
 *
 * void chgcell(context, x, y, c) -- set cell to state c
 *
 * void changesize(n)             -- change the cellbox allocation size
 *
 * void center()                  -- center viewport on barycenter of pattern
 *
 * void clear()                   -- clear the universe, freeing all storage
 *
 * void redisplay()               -- update visible display of active board
 *
 * void redrawscreen()            -- redraw, assuming nothing about old state
 *
 * void saveall()                 -- save entire board state to file
 *
 * This code knows that cells are packed in 8x8 tiles on a hash list, but
 * it doesn't know anything about the internals of cell representation within
 * cell boxes.  The function changesize() changes the size of cellboxes.
 *
 * This code relies on the existence of a triad of functions:
 *
 * getcell(ptr, dx, dy)      -- get state of cell at x, y in box *ptr
 * setcell(ptr, dx, dy, val) -- set cell at x, y in box *ptr to state val
 * forget(ptr)               -- cause a box to forget last generation's state
 *
 * to access cell state. It also relies on the existence of these functions:
 *
 * displaybox(x, y, ptr)     -- post box state to Xrect arrays
 * fetchtile(context, x, y)  -- get tile containing cell (x, y), NULL if none
 * maketile(context, x, y)   -- get tile holding cell (x, y), creating if none
 */

#include <stddef.h>	/* we need offsetof() */
#include "defs.h"
#include "data.h"
#include "tile.h"

#define HASH(x,y) 	(((x>>3) & 0x7f)<< 7) + ((y>>3) & 0x7f)
#define MARK		'*'
#define SPACE		'.'
#define	USL_MAX		4294967295U	/* max decimal value of "unsigned" */

#ifndef NULL
#define NULL 0
#endif

/* public variables */
pattern active, tentative; 
tile *freep;
static int tilesize;

void setscale(const int sc)
/* preset scale for X rectangles describing active board */
{
    if (scale > 1)
    {
	int	i, j, size;

	size = (1 << (sc - 1)) - (sc > 2);
	for (i = 0; i < MAXSTATES; i++)
	    for (j = 0; j < MAXCHANGE; j++)
	    {
		rects[i][j].height = size;
		rects[i][j].width = size;
	    }
    }
}

void initcells(pattern *context)
/* initialize the cell hash list */
{
    memset(context, '\0', sizeof(pattern));
}

void chgcell(pattern *context, coord_t x, coord_t y, cell_t c)
/* turn a cell to state ON */
{
    unsigned long ydx,xdx;
    tile *ptr;

    ptr = maketile(context, x,y);
    ydx = y - ptr->y;
    xdx = x - ptr->x;
    if (c)
	ptr->dead = 0;

    setcell(&ptr->cells, xdx, ydx, c);
}

void changesize(int newsize)
/* change the tile allocation size */
{
    if (tilesize != newsize)
    {
	tile	*ptr, *nptr;

	clear_pattern(&active);
	clear_pattern(&tentative);

	for (ptr = freep; ptr; ptr = nptr)
	{
	    nptr = ptr->next;
	    free(ptr);
	}
	freep = (tile *)NULL;

	tilesize = offsetof(tile, cells) + newsize;
    }
}

void killtile(pattern *context, tile *ptr)
/* nuke a tile, reclaiming all associated storage and fixing neighbor links */
{
    unsigned long hv=HASH(ptr->x,ptr->y);

    if (ptr != context->tiles){
	ptr->fore->next=ptr->next;
    }
    else{
	context->tiles = ptr->next;
    }
    if (ptr == context->hashlist[hv]){
	context->hashlist[hv] = ptr->hnext;
    }
    else{
	ptr->hfore->hnext=ptr->hnext;
    }
    if (ptr->next) ptr->next->fore=ptr->fore;
    if (ptr->hnext) ptr->hnext->hfore=ptr->hfore;
    if (ptr->rt) ptr->rt->lf=NULL;
    if (ptr->lf) ptr->lf->rt=NULL;
    if (ptr->up) ptr->up->dn=NULL;
    if (ptr->dn) ptr->dn->up=NULL;
    ptr->next=freep;
    freep=ptr;
    context->tilecount--;
}

tile *createtile(context, x,y, hv)
/* create a tile to hold cells near life-world coordinates x, y */
pattern *context;	/* universe it belongs in */
coord_t x,y;		/* associated life-world coordinates */
unsigned long hv;	/* tile-retrieval hash value */
{
    tile *ptr;

#ifdef PROF
    create_called++;
#endif PROF

    if (freep == NULL){
#ifdef PROF
	create_null++;
#endif PROF
	if ((ptr = (tile *)malloc(tilesize)) ==  (tile *)NULL){
	    perror("create: malloc error: ");
	    exit(-1);
	}
    }
    else{
	ptr=freep;
	freep=freep->next;
    }
    memset(ptr, '\0', tilesize);

    ptr->next=context->tiles;
    context->tiles=ptr;
    
    if (ptr->next != NULL){
	ptr->next->fore=ptr;
    }	
    ptr->hnext=context->hashlist[hv];
    context->hashlist[hv]= ptr;
    
    if (ptr->hnext != NULL){
	ptr->hnext->hfore=ptr;
    }
    ptr->x = x;
    ptr->y = y;
    context->tilecount++;
    return(ptr);
}


static tile *findtile(context, x, y, hv)
pattern *context;
coord_t x,y;
unsigned long hv;    
{
    tile *ptr = context->hashlist[hv];

#ifdef PROF
    clink_called++;
#endif PROF
    if (freep){
	if (ptr==NULL){
	    ptr=freep;
	    context->hashlist[hv]= ptr;
	    freep=freep->next;
	    memset(ptr, '\0', tilesize);
	    ptr->x = x;
	    ptr->y = y;
	    ptr->next=context->tiles;
	    context->tiles=ptr;    

	    if (ptr->next){
		ptr->next->fore=ptr;
	    }	
	    context->tilecount++;
	    return(ptr);
	}
	else{
	    do{
#ifdef PROF
		clink_search++;
#endif PROF
		if ((ptr->x == x) && (ptr->y == y)){
		    return(ptr);
		}
		ptr=ptr->hnext;
	    }while (ptr!=NULL);
	    
	    return(NULL);
	}
    }
    else
    {
	if (ptr==NULL)
	    return(NULL);
	do{
#ifdef PROF
	    clink_search++;
#endif PROF
	    if ((ptr->x == x) && (ptr->y == y)){
		return(ptr);
	    }
	    ptr=ptr->hnext;
	}while (ptr!=NULL);
	
	return(NULL);
    }
}

int lookcell(pattern *context, coord_t x, coord_t y)
/* get the state of a cell from its world coordinates */
{
    tile *ptr;
    coord_t hx, hy;
    unsigned long hv;
    
    hx = x & 0xfffffff8;
    hy = y & 0xfffffff8;
    hv = HASH(hx,hy);
    if (ptr = findtile(context, hx, hy, hv))
	return(getcell(&ptr->cells, x - hx, y - hy));
    else
	return(0);
}

tile *fetchtile(pattern *context, coord_t x, coord_t y)
{
    return(findtile(context, x, y, HASH(x, y)));
}

tile *maketile(pattern *context, coord_t x, coord_t y)
{
    tile *ptr;
    unsigned long hv;    
    
    x &= 0xfffffff8;
    y &= 0xfffffff8;
    hv = HASH(x,y);
    if (ptr = findtile(context, x, y, hv))
	return(ptr);
    else
	return(createtile(context, x, y, hv));
}

void center(void)
/* center the display on the `center of mass' of the live boxes */
{
    double x,y;
    int ctr=0;
    tile *ptr;
    x=0.0;
    y=0.0;
    XClearWindow(disp,lifew);
    for (ptr = active.tiles; ptr != NULL; ptr = ptr->next){
	if (!ptr->dead){
	    x+= ptr->x;
	    y+= ptr->y;
	    ctr++;
	}
    } 
    if (ctr>0) {
       x/=ctr;
       y/=ctr;
    }
    else {
       x=xorigin;
       y=yorigin;
    }
    xpos = x - ((unsigned)width >> scale);
    ypos = y - ((unsigned)height >> scale);
    redrawscreen();
}

void displaystats(void)
{
    if (dispboxes)
    {
	sprintf(inpbuf,
		"Generations: %6lu, Cells: %7lu, Boxes: %6lu, ",
		active.generations,active.cellcount,active.tilecount);
    }
    else
    {	
	sprintf(inpbuf,
		"Generations: %6lu, Cells: %6lu",
		active.generations,active.cellcount);
    }
    DoExpose(INPUTWIN);
}

void clear_pattern(pattern *pp)
/* free given pattern */
{
    tile *ptr, *nptr;

    initcells(pp);
    ptr = pp->tiles;
    while (ptr)
    {
	nptr = ptr->next;
	free(ptr);
	ptr = nptr;
    }
    pp->tiles = NULL;

    pp->generations = 0;
    pp->tilecount = 0;
    pp->cellcount = 0;
}

void clear(void)
/* clear the board or tentative pattern, freeing all storage */
{
    if (tentative.tiles)
    {
	clear_pattern(&tentative);
	free_loadscript();    		/* restart load script */

	XClearWindow(disp,lifew);
	XClearWindow(disp,inputw);
	redisplay();
    }
    else
    {
	clear_pattern(&active);
	state = STOP;

	XClearWindow(disp,lifew);
	XClearWindow(disp,inputw);
	displaystats();
    }
}

#define ONSCREEN(x, y)	(((x > xpos-BOXSIZE) && (x < (xpos+BOXSIZE+((unsigned)width >> (scale-1))))) && ((y > ypos-BOXSIZE) && (y < (ypos+BOXSIZE+((unsigned)height >> (scale -1))))))

#define POINTS	4000	/* accumulate this many point changes before writing */
#define RECTS	64	/* accumulate this many box changes before writing */

void redisplay(void)
/* re-display the visible part of the board */
{
    tile *ptr;
    coord_t x,y;
    int ctr = 0;
    long tentx,tenty;
    int state;

    displaystats();    
    if (state==HIDE)
	return;

#if STATEBITS
    chgpoints[0] = chgpoints[1] = chgrects[0] = chgrects[1] = 0;
#else    
    for (state = 0; state < maxstates; state++)
	chgpoints[state] = chgrects[state] = 0;
#endif
    
    for (ptr = active.tiles; ptr != NULL; ptr = ptr->next)
    {
	x=ptr->x;
	y=ptr->y;
	if (ONSCREEN(x, y))
	{
	    displaybox(x, y, &ptr->cells);
	    ctr++;
	    if (scale == 1)
	    {
#if STATEBITS == 1
		if ((chgpoints[1] >= POINTS) || (chgpoints[0] >= POINTS))
		{
		    XDrawPoints(disp,lifew,cellgc[0],points[0],chgpoints[0],CoordModeOrigin);
		    XDrawPoints(disp,lifew,cellgc[1],points[1],chgpoints[1],CoordModeOrigin);
		    chgpoints[1]=chgpoints[0]=0;
		}
#else
		for (state = 0; state < maxstates; state++)
		    if (chgpoints[state])
		    {
			XDrawPoints(disp,lifew,cellgc[state],
				    points[state],chgpoints[state],
				    CoordModeOrigin);
			chgpoints[state] = 0;
		    }
#endif
		ctr=0;
	    }
	    else
	    {
		if (ctr == RECTS)
		{
#if STATEBITS == 1
		    XFillRectangles(disp,lifew,cellgc[0],rects[0],chgrects[0]);
		    XFillRectangles(disp,lifew,cellgc[1],rects[1],chgrects[1]);
		    chgrects[1]=chgrects[0]=0;
#else
		    for (state = 0; state < maxstates; state++)
		    {
			XFillRectangles(disp,lifew,cellgc[state],
					rects[state],chgrects[state]);
			chgrects[state] = 0;
		    }
#endif
		    ctr=0;
		}
	    }
	}
    }
    /* draw tentative points with appropriate transformation */
    for (ptr = tentative.tiles; ptr != NULL; ptr = ptr->next)
    {
	tentx=ptr->x - STARTX;
	tenty=ptr->y - STARTY;
	if (ONSCREEN(tx(tentx,tenty,txx,txy)+loadx,
		     ty(tentx,tenty,tyx,tyy)+loady)) {
	    trdisplaybox(tentx, tenty, &ptr->cells);
	    ctr++;
	    if (scale == 1)
	    {
#if STATEBITS == 1
		if ((chgpoints[1] >= POINTS) || (chgpoints[0] >= POINTS))
		{
		    XDrawPoints(disp,lifew,cellgc[0],points[0],chgpoints[0],CoordModeOrigin);
		    XDrawPoints(disp,lifew,cellgc[1],points[1],chgpoints[1],CoordModeOrigin);
		    chgpoints[1]=chgpoints[0]=0;
		}
#else
		for (state = 0; state < maxstates; state++)
		    if (chgpoints[state])
		    {
			XDrawPoints(disp,lifew,cellgc[state],
				    points[state],chgpoints[state],
				    CoordModeOrigin);
			chgpoints[state] = 0;
		    }
#endif
		ctr=0;
	    }
	    else
	    {
		if (ctr == RECTS)
		{
#if STATEBITS == 1
		    XFillRectangles(disp,lifew,cellgc[0],rects[0],chgrects[0]);
		    XFillRectangles(disp,lifew,cellgc[1],rects[1],chgrects[1]);
		    chgrects[1]=chgrects[0]=0;
#else
		    for (state = 0; state < maxstates; state++)
		    {
			XFillRectangles(disp,lifew,cellgc[state],
					rects[state],chgrects[state]);
			chgrects[state] = 0;
		    }
#endif
		    ctr=0;
		}
	    }
	}
    }
    if (ctr)
    {
	if (scale == 1)
	{
#if STATEBITS == 1
	    XDrawPoints(disp,lifew,cellgc[0],points[0],chgpoints[0],CoordModeOrigin);
	    XDrawPoints(disp,lifew,cellgc[1],points[1],chgpoints[1],CoordModeOrigin);
	    chgpoints[1]=chgpoints[0]=0;
#else
	    for (state = 0; state < maxstates; state++)
	    {
		XDrawPoints(disp,lifew,cellgc[state],
			    points[state],chgpoints[state],
			    CoordModeOrigin);
		chgpoints[state] = 0;
	    }
#endif
	}
	else
	{
#if STATEBITS == 1
	    XFillRectangles(disp,lifew,cellgc[0],rects[0],chgrects[0]);
	    XFillRectangles(disp,lifew,cellgc[1],rects[1],chgrects[1]);
	    chgrects[1]=chgrects[0]=0;
#else
	    for (state = 0; state < maxstates; state++)
	    {
		XFillRectangles(disp,lifew,cellgc[state],
				rects[state],chgrects[state]);
		chgrects[state] = 0;
	    }
#endif
	}
	ctr=0;
    }
    /* draw pivot of tentative points and box them */
    if (tentative.tiles)
    {
	drawpivot((loadx-xpos) << (scale - 1),
		  (loady-ypos) << (scale - 1));

	boxpattern(1);	/* white outline box */
    }

    XFlush(disp);
}

void redrawscreen(void)
/* force redraw of entire board */
{
    tile *ptr;

    for (ptr = active.tiles; ptr != NULL; ptr = ptr->next)
	forget(&ptr->cells);
    for (ptr = tentative.tiles; ptr != NULL; ptr = ptr->next)
	forget(&ptr->cells);
    redisplay();
#if STATEBITS > 1
    DoExpose(INPUTWIN);		/* refresh the color picker */
#endif				/* STATEBITS > 1 */
}

cellcount_t bounding_box(
/* get the bounding box of a pattern */
pattern *context,
coord_t *xmin, coord_t *ymin, coord_t *xmax, coord_t *ymax)
{
    int dx, dy;
    cellcount_t cellcount = 0;
    tile *ptr;

    *xmin = USL_MAX; *ymin = USL_MAX; *xmax = 0; *ymax = 0; cellcount = 0;
    for (ptr = context->tiles; ptr != NULL; ptr = ptr->next)
	if (!ptr->dead)
	    for (dx = 0; dx < BOXSIZE; dx++)
		for (dy = 0; dy < BOXSIZE; dy++)
		    if (getcell(&ptr->cells, dx, dy))
		    {
			cellcount++;
			if (ptr->x+dx < *xmin)
			    *xmin = ptr->x+dx;
			if (ptr->y+dy < *ymin)
			    *ymin = ptr->y+dy;
			if (ptr->x+dx > *xmax)
			    *xmax = ptr->x+dx;
			if (ptr->y+dy > *ymax)
			    *ymax = ptr->y+dy;
		    }

    return(cellcount);
}


void saveall(FILE *ofp, char mode)
/* save the entire board contents (or the tentative pattern if there is one) */
{
    tile *ptr;
    int dx, dy, val,x=0;
    coord_t xmin, ymin, xmax, ymax;
    cellcount_t cellcount;
    pattern	*pp = tentative.tiles ? &tentative : &active;

    if (fname[0] != 0)
	fprintf(ofp,"#N %s\n",fname);
	
#if STATEBITS > 1
    if (maxstates > 2 && active_rules[0])
	(void) fprintf(ofp, "#U %s\n", active_rules);
#endif				/* STATEBITS > 1 */

    stamp("#0", ofp);

    while (x < numcomments)
    {
	fprintf(ofp,"#C %s \n",comments[x]);
	x++;
    }
	
    if (mode == 'A')		/* save in absolute mode */
    {
	fputs("#A\n", ofp);
	for (ptr = pp->tiles; ptr != NULL; ptr = ptr->next)
	    if (!ptr->dead)
		for (dx = 0; dx < BOXSIZE; dx++)
		    for (dy = 0; dy < BOXSIZE; dy++)
			if (val = getcell(&ptr->cells, dx, dy))
#if STATEBITS > 1
			    if (maxstates > 2)
				(void) fprintf(ofp, "%d %d %d\n",
					       ptr->x+dx, ptr->y+dy, val);
			    else 
#endif
				(void) fprintf(ofp,
					       "%d %d\n", ptr->x+dx,ptr->y+dy);
	return;
    }

    /*
     * Otherwise, determine the bounding box of the live cells.
     */
    cellcount = bounding_box(pp, &xmin, &ymin, &xmax, &ymax);

    /* caller may want save to shortest format */
    if (mode == '\0')
	if (((ymax - ymin + 1) * (xmax - xmin + 1)) > cellcount * 8)
	    mode = 'R';
	else
	    mode = 'P';

    /* now that we have the bounding box, we can gen the other formats */
    if (mode == 'R')
    {
	for (ptr = pp->tiles; ptr != NULL; ptr = ptr->next)
	    if (!ptr->dead)
		for (dx = 0; dx < BOXSIZE; dx++)
		    for (dy = 0; dy < BOXSIZE; dy++)
			if (val = getcell(&ptr->cells, dx, dy))
#if STATEBITS > 1
			    if (maxstates > 2)
				(void) fprintf(ofp, "%d %d %d\n",
					       ptr->x+dx-xmin, ptr->y+dy-ymin,
					       val);
			    else 
#endif
				(void) fprintf(ofp, "%d %d\n",
					       ptr->x+dx-xmin, ptr->y+dy-ymin);
    }
    else
    {
	coord_t x, y;

	(void) fprintf(ofp, "#P\n");
	for (y = ymin; y <= ymax; y++)
	{
	    for (x = xmin; x <= xmax; x++)
	    {
		tile *ptr;
		int val;

		if (val = lookcell(pp, x, y))
		{
#if STATEBITS > 1
		    if (maxstates > 2)
			(void) fputc(itos(val), ofp);
		    else 
#endif
			(void) fputc(MARK, ofp);
		}
		else
		    (void) fputc(SPACE, ofp);
	    }
	    (void) fputc('\n', ofp);
	}

	(void) fputs("## The following sets edit modes for GNU EMACS\n", ofp);
	(void) fputs("## Local ", ofp); (void) fputs("Variables:\n", ofp);
	(void) fputs("## mode:picture\n", ofp);
	(void) fputs("## truncate-lines:t\n", ofp);
	(void) fputs("## End:\n", ofp);
    }
}


/* tile.c ends here */
