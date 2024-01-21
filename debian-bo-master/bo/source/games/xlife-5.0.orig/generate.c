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
 *
 * CMU SUCKS
 */

/*
 * This module contains the evolution and rule-setting code.  It has three
 * entry points:
 *
 *	evolve() --- perform evolution on the board
 *	newrules() --- prompt for & accept new 2-state rules
 *	readrules() --- read N-state rules from given file
 */

#include "defs.h"
#include "tile.h"
#include "tab.h"

static bool edebug;
static float payoffs[2][2] = {1, 0, 0, 0};
static int count, ringdepth = 0, diagonals = 0;

#define VALENCE_DRIVEN	0	/* based on neighborhood count (live, born) */
#define TABLE_DRIVEN	1	/* based on transition table (trans) */
#define PAYOFF_DRIVEN	2	/* based on payoff matrix (payoffs) */
static bool ev_mode = VALENCE_DRIVEN;

/* strategy indices */
#define COOPERATE	0
#define DEFECT		1

/* cell state values */
#define COOPERATOR	1
#define DEFECTOR	2

static void evolve2(pattern *context)
{
    register u_long t1,t2,t3,*tmpptr,y, live_total = 0, changes = 0;
    tile *cptr,*tptr,*cptrup,*cptrdn,*cptrlf,*cptrrt;

    /*
     * Make adjacency links
     */
    cptr = context->tiles;
    while (cptr)
    {
	if (!(cptr->cells.twostate.live1 || cptr->cells.twostate.live2)){
	    cptr = cptr->next;
	    continue;
	}

	cptrup=cptr->up;
	cptrdn=cptr->dn;
	cptrlf=cptr->lf;
	cptrrt=cptr->rt;
	
	t1=cptr->cells.twostate.live1&0xff;
	if (t1)
	{
	    if (cptrup==NULL)
	    {
		cptrup=maketile(context, cptr->x,cptr->y - BOXSIZE);
		cptrup->dn=cptr;
	    }
	    t2=tab1[t1];
	    cptrup->cells.twostate.on[7]+=t2;
	    cptr->cells.twostate.on[1]+=t2;
	    cptr->cells.twostate.on[0]+=tab2[t1];
	}
	
	t1=(cptr->cells.twostate.live2 & 0xff000000)>>24;	    
	if (t1)
	{
	    if (cptrdn==NULL)
	    {
		cptrdn=maketile(context, cptr->x,cptr->y + BOXSIZE);
		cptrdn->up=cptr;
	    }
	    t2=tab1[t1];
	    cptrdn->cells.twostate.on[0]+=t2;
	    cptr->cells.twostate.on[6]+=t2;
	    cptr->cells.twostate.on[7]+=tab2[t1];
	}

	t1=cptr->cells.twostate.live1;
	t2=cptr->cells.twostate.live2;

	if (t1 & 0x1010101)
	{
	    if (cptrlf==NULL)
	    {
		cptrlf=maketile(context, cptr->x - BOXSIZE,cptr->y);
		cptrlf->rt=cptr;
	    }
	    if (t1 & 0x1)
	    {
		cptrlf->cells.twostate.on[0]+=0x10000000;
		cptrlf->cells.twostate.on[1]+=0x10000000;
		if (cptrlf->up==NULL)
		{
		    cptrlf->up=maketile(context, cptr->x - BOXSIZE,cptr->y - BOXSIZE);
		}
		cptrlf->up->cells.twostate.on[7]+= 0x10000000;
		cptrlf->up->dn=cptrlf;
	    }
	    if (t1 & 0x100)
	    {
		cptrlf->cells.twostate.on[0]+=0x10000000;
		cptrlf->cells.twostate.on[1]+=0x10000000;
		cptrlf->cells.twostate.on[2]+=0x10000000;
	    }
	    if (t1 & 0x10000)
	    {
		cptrlf->cells.twostate.on[1]+=0x10000000;
		cptrlf->cells.twostate.on[2]+=0x10000000;
		cptrlf->cells.twostate.on[3]+=0x10000000;
	    }
	    if (t1 & 0x1000000)
	    {
		cptrlf->cells.twostate.on[2]+=0x10000000;
		cptrlf->cells.twostate.on[3]+=0x10000000;
		cptrlf->cells.twostate.on[4]+=0x10000000;
	    }
	}
	
	if (t2 & 0x1010101)
	{
	    if (cptrlf==NULL)
	    {
		cptrlf=maketile(context, cptr->x - BOXSIZE,cptr->y);
		cptrlf->rt=cptr;
	    }
	    if (t2 & 0x1)
	    {
		cptrlf->cells.twostate.on[3]+=0x10000000;
		cptrlf->cells.twostate.on[4]+=0x10000000;
		cptrlf->cells.twostate.on[5]+=0x10000000;
	    }
	    if (t2 & 0x100)
	    {
		cptrlf->cells.twostate.on[4]+=0x10000000;
		cptrlf->cells.twostate.on[5]+=0x10000000;
		cptrlf->cells.twostate.on[6]+=0x10000000;
	    }
	    if (t2 & 0x10000)
	    {
		cptrlf->cells.twostate.on[5]+=0x10000000;
		cptrlf->cells.twostate.on[6]+=0x10000000;
		cptrlf->cells.twostate.on[7]+=0x10000000;
	    }	    
	    if (t2 & 0x1000000)
	    {
		cptrlf->cells.twostate.on[6]+=0x10000000;
		cptrlf->cells.twostate.on[7]+=0x10000000;
		if (cptrlf->dn==NULL)
		{
		    cptrlf->dn=maketile(context, cptr->x - BOXSIZE,cptr->y + BOXSIZE);
		}
		cptrlf->dn->cells.twostate.on[0]+= 0x10000000;
		cptrlf->dn->up=cptrlf;
	    }
	}
	
	if (t1 & 0x80808080)
	{
	    if (cptrrt == NULL)
	    {
		cptrrt=maketile(context, cptr->x + BOXSIZE,cptr->y);
		cptrrt->lf=cptr;
	    }
	    if (t1 & 0x80)
	    {
		cptrrt->cells.twostate.on[0]+=0x1;
		cptrrt->cells.twostate.on[1]+=0x1;
		if (cptrrt->up==NULL)
		{
		    cptrrt->up=maketile(context, cptr->x + BOXSIZE,cptr->y - BOXSIZE);
		}
		cptrrt->up->cells.twostate.on[7]+= 0x1;
		cptrrt->up->dn=cptrrt;
	    }
	    if (t1 & 0x8000)
	    {
		cptrrt->cells.twostate.on[0]+=0x1;
		cptrrt->cells.twostate.on[1]+=0x1;
		cptrrt->cells.twostate.on[2]+=0x1;
	    }
	    if (t1 & 0x800000)
	    {
		cptrrt->cells.twostate.on[1]+=0x1;
		cptrrt->cells.twostate.on[2]+=0x1;
		cptrrt->cells.twostate.on[3]+=0x1;
	    }
	    if (t1 & 0x80000000)
	    {
		cptrrt->cells.twostate.on[2]+=0x1;
		cptrrt->cells.twostate.on[3]+=0x1;
		cptrrt->cells.twostate.on[4]+=0x1;
	    }
	}
	
	if (t2 & 0x80808080)
	{
	    if (cptrrt == NULL)
	    {
		cptrrt=maketile(context, cptr->x + BOXSIZE,cptr->y);
		cptrrt->lf=cptr;
	    }
	    if (t2 & 0x80)
	    {
		cptrrt->cells.twostate.on[3]+=0x1;
		cptrrt->cells.twostate.on[4]+=0x1;
		cptrrt->cells.twostate.on[5]+=0x1;
	    }
	    if (t2 & 0x8000)
	    {
		cptrrt->cells.twostate.on[4]+=0x1;
		cptrrt->cells.twostate.on[5]+=0x1;
		cptrrt->cells.twostate.on[6]+=0x1;
	    }
	    if (t2 & 0x800000)
	    {
		cptrrt->cells.twostate.on[5]+=0x1;
		cptrrt->cells.twostate.on[6]+=0x1;
		cptrrt->cells.twostate.on[7]+=0x1;
	    }
	    if (t2 & 0x80000000)
	    {
		cptrrt->cells.twostate.on[6]+=0x1;
		cptrrt->cells.twostate.on[7]+=0x1;
		if (cptrrt->dn==NULL)
		{
		    cptrrt->dn=maketile(context, cptr->x + BOXSIZE,cptr->y + BOXSIZE);
		}
		cptrrt->dn->cells.twostate.on[0]+= 0x1;
		cptrrt->dn->up=cptrrt;
	    }
	}
	
	t1=(cptr->cells.twostate.live1 & 0xff00) >> 8;
	t2=(cptr->cells.twostate.live1 & 0xff0000) >> 16;
	
	if (t1)
	{
	    t3 = tab1[t1];
	    cptr->cells.twostate.on[1]+=tab2[t1];
	    cptr->cells.twostate.on[0]+=t3;
	    cptr->cells.twostate.on[2]+=t3;
	}
	
	t1=(cptr->cells.twostate.live1 & 0xff000000) >> 24;
	
	if (t2)
	{
	    t3 = tab1[t2];
	    cptr->cells.twostate.on[2]+=tab2[t2];
	    cptr->cells.twostate.on[1]+=t3;
	    cptr->cells.twostate.on[3]+=t3;
	}
	
	t2=(cptr->cells.twostate.live2 & 0xff);
	
	if (t1)
	{
	    t3 = tab1[t1];
	    cptr->cells.twostate.on[3]+=tab2[t1];
	    cptr->cells.twostate.on[2]+=t3;
	    cptr->cells.twostate.on[4]+=t3;
	}
	
	t1=(cptr->cells.twostate.live2 & 0xff00) >> 8;
	
	if (t2)
	{
	    t3 = tab1[t2];
	    cptr->cells.twostate.on[4]+=tab2[t2];
	    cptr->cells.twostate.on[3]+=t3;
	    cptr->cells.twostate.on[5]+=t3;
	}
	
	t2=(cptr->cells.twostate.live2 & 0xff0000) >> 16;	    
	
	if (t1)
	{
	    t3 = tab1[t1];
	    cptr->cells.twostate.on[5]+=tab2[t1];
	    cptr->cells.twostate.on[4]+=t3;
	    cptr->cells.twostate.on[6]+=t3;
	}
	
	if (t2)
	{
	    t3 = tab1[t2];
	    cptr->cells.twostate.on[6]+=tab2[t2];
	    cptr->cells.twostate.on[5]+=t3;
	    cptr->cells.twostate.on[7]+=t3;
	}
	
	cptr->up=cptrup;
	cptr->dn=cptrdn;
	cptr->lf=cptrlf;
	cptr->rt=cptrrt;
	cptr=cptr->next;
    }

    /*
     * Perform actual evolution.
     */
    for (cptr = context->tiles; cptr; cptr = cptr->next)
    {
	t1=cptr->cells.twostate.live1;
	cptr->cells.twostate.olive1=t1;
	tmpptr=cptr->cells.twostate.on;
	t2=0;
	t3= *tmpptr;
	if (t3 &0xffff)
	{
	    t2=lookup[((t3 & 0xffff)<<4) + (t1&0xf)];
	}
	if (t3 &0xffff0000)
	{
	    t2|=lookup[((t3 & 0xffff0000)>>12) + ((t1>>4)&0xf)] << 4;
	}
	*tmpptr=0;
	tmpptr++;
	t3= *tmpptr;
	if (t3 &0xffff)
	{
	    t2|=lookup[((t3 & 0xffff)<<4) + ((t1>>8)&0xf)] << 8;
	}
	if (t3 &0xffff0000)
	{
	    t2|=lookup[((t3 & 0xffff0000)>>12) + ((t1>>12)&0xf)] << 12;
	}
	*tmpptr=0;
	tmpptr++;
	t3= *tmpptr;
	if (t3 &0xffff)
	{
	    t2|=lookup[((t3 & 0xffff)<<4) + ((t1>>16)&0xf)] << 16;
	}
	if (t3 &0xffff0000)
	{
	    t2|=lookup[((t3 & 0xffff0000)>>12) + ((t1>>20)&0xf)] << 20;
	}
	*tmpptr=0;
	tmpptr++;
	t3= *tmpptr;
	if (t3 &0xffff)
	{
	    t2|=lookup[((t3 & 0xffff)<<4) + ((t1>>24)&0xf)] << 24;
	}
	if (t3 &0xffff0000)
	{
	    t2|=lookup[((t3 & 0xffff0000)>>12) + ((t1>>28)&0xf)] << 28;
	}
	*tmpptr=0;
	tmpptr++;
	t3= *tmpptr;
	cptr->cells.twostate.live1=t2;
	t1=cptr->cells.twostate.olive2=cptr->cells.twostate.live2;	
	t2=0;

	if (t3 &0xffff)
	{
	    t2=lookup[((t3 & 0xffff)<<4) + (t1&0xf)];
	}
	if (t3 &0xffff0000)
	{
	    t2|=lookup[((t3 & 0xffff0000)>>12) + ((t1>>4)&0xf)] << 4;
	}
	*tmpptr=0;
	tmpptr++;
	t3= *tmpptr;
	if (t3 &0xffff)
	{
	    t2|=lookup[((t3 & 0xffff)<<4) + ((t1>>8)&0xf)] << 8;
	}
	if (t3 &0xffff0000)
	{
	    t2|=lookup[((t3 & 0xffff0000)>>12) + ((t1>>12)&0xf)] << 12;
	}
	*tmpptr=0;
	tmpptr++;
	t3= *tmpptr;
	if (t3 &0xffff)
	{
	    t2|=lookup[((t3 & 0xffff)<<4) + ((t1>>16)&0xf)] << 16;
	}
	if (t3 &0xffff0000)
	{
	    t2|=lookup[((t3 & 0xffff0000)>>12) + ((t1>>20)&0xf)] << 20;
	}
	*tmpptr=0;
	tmpptr++;
	t3= *tmpptr;
	if (t3 &0xffff)
	{
	    t2|=lookup[((t3 & 0xffff)<<4) + ((t1>>24)&0xf)] << 24;
	}
	if (t3 &0xffff0000)
	{
	    t2|=lookup[((t3 & 0xffff0000)>>12) + ((t1>>28)&0xf)] << 28;
	}
	*tmpptr=0;
	cptr->cells.twostate.live2=t2;
	
	/*
	 * Box-aging and statistics gathering.
	 */
	t1=cptr->cells.twostate.live1;
	for (y=0;y<32;y++)
	    if (t1 & (1<<y))
		context->cellcount++;
	t1=cptr->cells.twostate.live2;
	for (y=0;y<32;y++)
	    if (t1 & (1<<y))
		context->cellcount++;

 	if (cptr->cells.twostate.live1 || cptr->cells.twostate.live2)
	{
	    cptr->dead = 0;
	    live_total++;
	}
	else
	    cptr->dead++;

 	if (cptr->cells.twostate.live1 != cptr->cells.twostate.olive1
		|| cptr->cells.twostate.live2 != cptr->cells.twostate.olive2)
	    changes++;
    }

    if (live_total == 0 || changes == 0)
	state = STOP;
}

void newrules(void)
/* prompt for and accept 2-state rules */
{
    int i,k;
    char *ptr;
    
    ptr = inpbuf;
    strcpy(ptr,"Rules: ");
    ptr = ptr + 7;

    if (ev_mode == PAYOFF_DRIVEN)
    {
	(void) strcpy(ptr, active_rules);
    }
    else if (ev_mode == TABLE_DRIVEN)
    {
	(void) strcpy(ptr, active_rules);
    }
    else /* (ev_mode == VALENCE_DRIVEN) */
    {
	k=live;
	for (i=0; i<9; i++)
	{
	    if (k&1)
	    {
		*ptr=i+48;
		ptr++;
	    }
	    k>>=1;
	}
	*ptr='/';
	ptr++;
	k=born;
	for (i=0; i<9; i++)
	{
	    if (k&1)
	    {
		*ptr=i+48;
		ptr++;
	    }
	    k>>=1;
	}
    }

    ptr = inpbuf + strlen(inpbuf);
    strcpy(ptr,"   New Rules:  ");
    minbuflen=strlen(inpbuf);
    XClearWindow(disp,inputw);
    XDrawString(disp, inputw, ntextgc,ICOORDS(0,0),inpbuf, strlen(inpbuf));
    
    getxstring();
    (void) strncpy(active_rules, inpbuf, strlen(active_rules));
    showrules();
    inpbuf[0]=0;
    
    ptr=inpbuf+minbuflen;

    if (strchr(ptr, '$'))
    {
	ev_mode = PAYOFF_DRIVEN;
	diagonals = TRUE;
	ringdepth = 2;

	(void) sscanf(ptr, "%f$%f",
		      &payoffs[DEFECT][COOPERATE],
		      &payoffs[COOPERATE][DEFECT]);

	alloc_states(5);	/* must alloc 5 states to get pseudocolors */

	changesize(sizeof(struct nstate_t));
	(void) sprintf(active_rules, "%.2f$%.2f",
		       payoffs[DEFECT][COOPERATE], payoffs[COOPERATE][DEFECT]);
	DoExpose(INPUTWIN);
    }
    else /* if (strchr(ptr, '/')) */
    {
	ev_mode = VALENCE_DRIVEN;

	k=0;
	while ((*ptr>47)&&(*ptr<58))
	{   k|=(1<<(*ptr-48));
	    ptr++;
	}
	live=k;
	k=0;
	if (*ptr=='/')
	{   ptr++;
	    while ((*ptr>47)&&(*ptr<58))
	    {   k|=(1<<(*ptr-48));
		ptr++;
	    }
	    born=k;
	}
	gentab();
    }
    XClearWindow(disp,inputw);
}


/* now the n-state case... */

#if STATEBITS > 1
#include <ctype.h>

#define SUCCEED	0
#define FAIL	-1

#define NHSIZE		4	/* neighborhood size */

/*
 * These macros unroll a bunch of loops inside the tile-evolution function.
 * They aren't parametrized for BOXSIZE, but have the double benefit of
 * eliminating loop overhead and allowing the optimizer to fold constant
 * array references. On a machine with character-addressable memory the
 * results ought to run like a banshee.
 */
/* first, macros to test whether a tile edge is active */
#define H_EDGE(h, r) \
		(h[r][0] | h[r][1] | h[r][2] | h[r][3] \
		| h[r][4] | h[r][5] | h[r][6] | h[r][7])
#define V_EDGE(h, r) \
		(h[0][r] | h[1][r] | h[2][r] | h[3][r] \
		| h[4][r] | h[5][r] | h[6][r] | h[7][r])

/*
 * Offsets of current tile in hold area.  The idea here is that we need to copy
 * enough of the surrounding tiles into the hold area to do all resolution. 
 * For table-driven functions one edge row suffices; for payoff-driven
 * functions, two edge rows/columns are needed (because we need to compute the
 * payoffs of the inner edge cells).
 */
#define HX	2
#define HY	2

/* macros to grab a row or column out of a tile and put it in the hold area */
#define GET_COL(tcell, tr, fcell, fr) \
{ \
	tcell[HY+0][tr] = fcell[0][fr]; \
	tcell[HY+1][tr] = fcell[1][fr]; \
	tcell[HY+2][tr] = fcell[2][fr]; \
	tcell[HY+3][tr] = fcell[3][fr]; \
	tcell[HY+4][tr] = fcell[4][fr]; \
	tcell[HY+5][tr] = fcell[5][fr]; \
	tcell[HY+6][tr] = fcell[6][fr]; \
	tcell[HY+7][tr] = fcell[7][fr]; \
}
#define GET_ROW(tcell, tc, fcell, fr) \
{ \
	tcell[tc][HX+0] = fcell[fr][0]; \
	tcell[tc][HX+1] = fcell[fr][1]; \
	tcell[tc][HX+2] = fcell[fr][2]; \
	tcell[tc][HX+3] = fcell[fr][3]; \
	tcell[tc][HX+4] = fcell[fr][4]; \
	tcell[tc][HX+5] = fcell[fr][5]; \
	tcell[tc][HX+6] = fcell[fr][6]; \
	tcell[tc][HX+7] = fcell[fr][7]; \
}

/* these work in the opposite direction, from hold area to tile */
#define SET_COL(tcell, tr, fcell, fr) \
{ \
	tcell[0][tr] = fcell[HY+0][fr]; \
	tcell[1][tr] = fcell[HY+1][fr]; \
	tcell[2][tr] = fcell[HY+2][fr]; \
	tcell[3][tr] = fcell[HY+3][fr]; \
	tcell[4][tr] = fcell[HY+4][fr]; \
	tcell[5][tr] = fcell[HY+5][fr]; \
	tcell[6][tr] = fcell[HY+6][fr]; \
	tcell[7][tr] = fcell[HY+7][fr]; \
}
#define SET_ROW(tcell, tc, fcell, fr) \
{ \
	tcell[tc][0] = fcell[fr][HX+0]; \
	tcell[tc][1] = fcell[fr][HX+1]; \
	tcell[tc][2] = fcell[fr][HX+2]; \
	tcell[tc][3] = fcell[fr][HX+3]; \
	tcell[tc][4] = fcell[fr][HX+4]; \
	tcell[tc][5] = fcell[fr][HX+5]; \
	tcell[tc][6] = fcell[fr][HX+6]; \
	tcell[tc][7] = fcell[fr][HX+7]; \
}

/* fill the hold area from a tile */
#define GET_INTERIOR(to, from) \
{ \
      GET_COL(to, HX+0, from, 0); \
      GET_COL(to, HX+1, from, 1); \
      GET_COL(to, HX+2, from, 2); \
      GET_COL(to, HX+3, from, 3); \
      GET_COL(to, HX+4, from, 4); \
      GET_COL(to, HX+5, from, 5); \
      GET_COL(to, HX+6, from, 6); \
      GET_COL(to, HX+7, from, 7); \
}

static float compute_payoff(int me, int you)
/* compute payoff function for Prisoner's Dilemma simulations */
{
    if (you == 0)
	return(0);
    else
	return(payoffs[me - 1][you - 1]);	/* ugh -- value-dependent */
}

static cell_t	hold[HY + BOXSIZE + HY][HX + BOXSIZE + HX];

static void get_corner(int dy, int dx, tile *cp, int sy, int sx)
/* fetch a tile corner into the hold area */
{
    hold[dy  ][dx  ] = cp->cells.nstate.ocell[sy  ][sx  ];
    hold[dy+1][dx  ] = cp->cells.nstate.ocell[sy+1][sx  ];
    hold[dy  ][dx+1] = cp->cells.nstate.ocell[sy  ][sx+1];
    hold[dy+1][dx+1] = cp->cells.nstate.ocell[sy+1][sx+1];
}

static void evolven(pattern *context)
/* slower table-driven evolve for n-state spaces */
{
    register tile	*cptr;
    register int	i, j;
    int			nzcount, live_total = 0, changes = 0;
    float		payoff[HY + BOXSIZE + HY][HX + BOXSIZE + HX];

    if (edebug >= 2)
	(void) fprintf(stderr,
		       "\nEvolving generation %d\n", context->generations);

    /* first, save old state and extend active region as necessary */
    for (cptr = context->tiles; cptr; cptr = cptr->next)
    {
	(void) memcpy(&cptr->cells.nstate.ocell,
		      &cptr->cells.nstate.cell,
		      sizeof(cell_t[BOXSIZE][BOXSIZE]));

	if (V_EDGE(cptr->cells.nstate.ocell, 1))
	    maketile(context, cptr->x - BOXSIZE, cptr->y);

	if (V_EDGE(cptr->cells.nstate.ocell, BOXSIZE))
	    maketile(context, cptr->x + BOXSIZE, cptr->y);

	if (H_EDGE(cptr->cells.nstate.ocell, 1))
	    maketile(context, cptr->x, cptr->y - BOXSIZE);

	if (H_EDGE(cptr->cells.nstate.ocell, BOXSIZE))
	    maketile(context, cptr->x, cptr->y + BOXSIZE);

#define ADJACENT(x,y)	(hold[y-1][x-1] || hold[y-1][x] || hold[y-1][x+1] \
			 || hold[y][x-1] || hold[y][x] || hold[y][x+1] \
			 || hold[y+1][x-1] || hold[y+1][x] || hold[y+1][x+1])

	/* we may need to grab the diagonally-adjacent tiles too */
	if (diagonals)
	{
	    if (ADJACENT(HY,           HX))
		maketile(context, cptr->x - BOXSIZE, cptr->y - BOXSIZE);
	    if (ADJACENT(HY+BOXSIZE-1, HX))
		maketile(context, cptr->x - BOXSIZE, cptr->y + BOXSIZE);
	    if (ADJACENT(HY,           HX+BOXSIZE-1))
		maketile(context, cptr->x + BOXSIZE, cptr->y - BOXSIZE);
	    if (ADJACENT(HY+BOXSIZE-1, HX+BOXSIZE-1))
		maketile(context, cptr->x + BOXSIZE, cptr->y + BOXSIZE);
	}
    }

    /* evolution in action */
    for (cptr = context->tiles; cptr; cptr = cptr->next)
    {
	register tile	*ncptr;

	if (edebug >= 2)
	{
	    int	i, j;

	    count = 0;
	    for (i = 0; i < HY + BOXSIZE + HY; i++)
		for (j = 0; j < HX + BOXSIZE + HX; j++)
		    if (hold[i][j])
			count++;

	    (void) fprintf(stderr, "Tile %p at %d, %d:",
			   cptr, cptr->x - xpos, cptr->y - ypos);

	    if (count == 0)
		(void) fputs(" empty", stderr);
	    (void) fputc('\n', stderr);
	}

	if (cptr->dead)
	    continue;

	memset(hold, '\0', sizeof(hold));

	GET_INTERIOR(hold, cptr->cells.nstate.ocell);

	memset(payoff, '\0', sizeof(payoff));

	if (edebug >= 3)
	    drawbox(cptr->x, cptr->y, 
		cptr->x + BOXSIZE - 1, cptr->y + BOXSIZE - 1,
		1);

	/* fill in left edge cells if needed */
	if (ncptr = fetchtile(context, cptr->x-BOXSIZE, cptr->y))
	{
	    GET_COL(hold, 1, ncptr->cells.nstate.ocell, BOXSIZE-1);
	    if (ringdepth > 1)
		GET_COL(hold, 0, ncptr->cells.nstate.ocell, BOXSIZE-2);
	}

	/* fill in right edge cells if needed */
	if (ncptr = fetchtile(context, cptr->x+BOXSIZE, cptr->y))
	{
	    GET_COL(hold, BOXSIZE+2, ncptr->cells.nstate.ocell, 0);
	    if (ringdepth > 1)
		GET_COL(hold, BOXSIZE+3, ncptr->cells.nstate.ocell, 1);
	}

	/* fill in top edge cells if needed */
	if (ncptr = fetchtile(context, cptr->x, cptr->y-BOXSIZE))
	{
	    GET_ROW(hold, 1, ncptr->cells.nstate.ocell, BOXSIZE-1);
	    if (ringdepth > 1)
		GET_ROW(hold, 0, ncptr->cells.nstate.ocell, BOXSIZE-2);
	}

	/* fill in bottom edge cells if needed */
	if (ncptr = fetchtile(context, cptr->x, cptr->y+BOXSIZE))
	{
	    GET_ROW(hold, BOXSIZE+2, ncptr->cells.nstate.ocell, 0);
	    if (ringdepth > 1)
		GET_ROW(hold, BOXSIZE+3, ncptr->cells.nstate.ocell, 1);
	}

	if (diagonals)
	{
 	    if (ncptr = fetchtile(context, cptr->x-BOXSIZE, cptr->y-BOXSIZE))
		get_corner(0,0,                 ncptr, BOXSIZE-2,BOXSIZE-2);

 	    if (ncptr = fetchtile(context, cptr->x+BOXSIZE, cptr->y-BOXSIZE))
		get_corner(0,BOXSIZE+2,         ncptr, BOXSIZE-2,0);

 	    if (ncptr = fetchtile(context, cptr->x+BOXSIZE, cptr->y+BOXSIZE))
		get_corner(BOXSIZE+2,BOXSIZE+2, ncptr, 0,0);

 	    if (ncptr = fetchtile(context, cptr->x-BOXSIZE, cptr->y+BOXSIZE))
		get_corner(BOXSIZE+2,0,         ncptr, 0,BOXSIZE-2);
	}

	if (ev_mode == PAYOFF_DRIVEN)
	{
	    /* flatten away the transition pseudo-states */
	    for (i = 0; i <= BOXSIZE + 3; i++)
		for (j = 0; j <= BOXSIZE + 3; j++)
		    if (hold[i][j] > 2)
			hold[i][j] -= 2;

	    for (i = 1; i <= BOXSIZE + 2; i++)
		for (j = 1; j <= BOXSIZE + 2; j++)
		{
		    int ostate = hold[i][j];

		    payoff[i][j] = compute_payoff(ostate, hold[i-1][j-1]);
		    payoff[i][j] += compute_payoff(ostate, hold[i-1][j]);
		    payoff[i][j] += compute_payoff(ostate, hold[i-1][j+1]);
		    payoff[i][j] += compute_payoff(ostate, hold[i][j-1]);
		    payoff[i][j] += compute_payoff(ostate, hold[i][j+1]);
		    payoff[i][j] += compute_payoff(ostate, hold[i+1][j-1]);
		    payoff[i][j] += compute_payoff(ostate, hold[i+1][j]);
		    payoff[i][j] += compute_payoff(ostate, hold[i+1][j+1]);
		}
	}

	/* apply the transition function */
	nzcount = 0;
	for (i = HY; i < BOXSIZE + HY; i++)
	    for (j = HX; j < BOXSIZE + HX; j++)
	    {
		register int newval;

		if (ev_mode == PAYOFF_DRIVEN)
		{
		    static struct {int yi; int xi;} neighbors[] =
		    {
			{-1, -1},
			{-1,  0},
			{-1,  1},
			{ 0, -1},
			{ 0,  0},
			{ 0,  1},
			{ 1, -1},
			{ 1,  0},
			{ 1,  1},
		    };
		    float highest, maxpayoff[MAXSTATES];
		    int	k;

		    /* compute the maximum payoff of each strategy */
		    memset(maxpayoff, '\0', sizeof(maxpayoff));
		    for (k=0; k < sizeof(neighbors)/sizeof(neighbors[0]); k++)
		    {
			int np = payoff[i+neighbors[k].yi][j+neighbors[k].xi];

			newval = hold[i+neighbors[k].yi][j+neighbors[k].xi];
			if (np > maxpayoff[newval])
			    maxpayoff[newval] = np;
		    }

		    /*
		     * Pick the strategy with the highest payoff.
		     * Break ties towards lowest-numbered strategy.
		     */
		    if (newval = hold[i][j])
		    {
			highest = 0;
			for (k = maxstates-1; k >= 0; k--)
			    if (maxpayoff[k] >= highest)
			    {
				newval = k;
				highest = maxpayoff[k];
			    }
		    }

		    /* this creates the transition pseudo-states */
		    if (newval != hold[i][j])
			newval += 2;
		}
		else	/* use the table-driven transition function */
		{
		    newval = newstate(hold[i][j],
				      hold[i-1][j], hold[i][j+1],
				      hold[i+1][j], hold[i][j-1]);

		    /*
		     * If the transition function is undefined here,
		     * give user a chance to tell us what to do.
		     */
		    if (newval == BADSTATE)
		    {
			FILE	*fp;

			drawbox(cptr->x + j - 2, cptr->y + i - 2, 
				cptr->x + j, cptr->y + i,
				1);
			state = STOP;
			newval = patch_transition(hold[i][j],
						  hold[i-1][j], hold[i][j+1],
						  hold[i+1][j], hold[i][j-1]);
			drawcell(RXPOS(cptr->x + j - 1, xpos),
				 RYPOS(cptr->y + i - 1, ypos),
				 newval);
			erasebox(cptr->x + j - 2, cptr->y + i - 2, 
				 cptr->x + j, cptr->y + i);
			if (active_rules[0] && (fp = fopen(PATCH_LOG, "a")))
			{
			    (void) fprintf(fp, "%c%c%c%c%c%c",
					   itos(hold[i][j]),
					   itos(hold[i-1][j]), itos(hold[i][j+1]),
					   itos(hold[i+1][j]), itos(hold[i][j-1]),
					   itos(newval));
			    stamp("\t#", fp);
			    (void) fclose(fp);
			}
		    }
		}

		if (newval != cptr->cells.nstate.cell[i-HY][j-HX])
		    changes++;

		/* OK, assign the new cell state */
		cptr->cells.nstate.cell[i-HY][j-HX] = newval;

		if (newval)
		{
		    if (dispboxes)
			context->cellcount++;
		    nzcount++;
		}
	    }

	if (nzcount == 0)
	    cptr->dead++;
	else
	{
	    cptr->dead = 0;
	    live_total++;
	}

	/* we may want to dump the tile state for debugging purposes */
	if (edebug >= 2 && count)
	{
	    for (i = 0; i < HY + BOXSIZE + HY; i++)
	    {
		cell_t	c;

		/* display old tile */
		for (j = 0; j < BOXSIZE; j++)
		{
		    if (i < HY || i > HY + BOXSIZE - 1)
			(void) fputc(' ', stderr);
		    else
		    {
			c = cptr->cells.nstate.ocell[i-HY][j];
			(void) fputc(c ? itos(c) : '.', stderr);
		    }
		}

		/* display the contents of the hold area */
		(void) fputs("   ", stderr);
		for (j = 0; j < HY + BOXSIZE + HY; j++)
		{
		    c = hold[i][j];
		    (void) fputc(c ? itos(c) : '.', stderr);
		    if (j == HX - 1 || j == BOXSIZE + HX - 1)
			(void) putc('|', stderr);
		}

#ifdef DUMP_PAYOFF
		if (ev_mode == PAYOFF_DRIVEN)
	        {
		    (void) fputs("   ", stderr);
		    for (j = 1; j < HY + BOXSIZE + HY - 1; j++)
			(void) fprintf(stderr, " %3.1f", payoff[i][j]);
		}
#endif /* DUMP_PAYOFF */
		
		/* display the new tile state */
		(void) fputs("   ", stderr);
		for (j = 0; j < BOXSIZE; j++)
		{
		    if (i < HY || i > HY + BOXSIZE - 1)
			(void) fputc(' ', stderr);
		    else
		    {
			c = cptr->cells.nstate.cell[i-HY][j];
			(void) fputc(c ? itos(c) : '.', stderr);
		    }
		}

		(void) fputc('\n', stderr);

		if (i == HY - 1 || i == BOXSIZE + HY - 1)
		{
		    for (j = 0; j < BOXSIZE; j++)
			putc(' ', stderr);

		    (void) fputs("   ", stderr);
		    for (j = 0; j < HX + BOXSIZE + HX; j++)
		    {
			putc('-', stderr);
			if (j == HX - 1 || j == BOXSIZE + HX - 1)
			    (void) putc('+', stderr);
		    }

		    (void) fputs("   ", stderr);
		    for (j = 0; j < BOXSIZE; j++)
			putc(' ', stderr);

		    (void) fputc('\n', stderr);
		}
	    }
	}
    }

    if (changes == 0 || live_total == 0)
	state = STOP;
}

/*
 * Parsing machinery for the transition-rule compiler begins here.
 *
 * !!!*** Nifty hack alert **** Nifty hack alert **** Nifty hack alert ****!!!
 *
 * An implementation problem with the state-quintuple format is that we need
 * to apply the `code-generator' function to all glob patterns implied by an
 * RE of the form
 *
 *  <ss><ss><ss><ss><ss>
 *
 * where <ss> can be a digit or a digit set bracketed by [, ]; but there
 * could be up to MAXSTATES to the 5th power of these, which is too many to
 * generate into a scratch array or file and then crunch through.
 *
 * Read on, and be enlightened...
 *						Eric S. Raymond
 *						esr@snark.thyrsus.com
 */
static cell_t	sofar[NHSIZE + 2];

#define E_NOCLOSE	"Missing ] character"
#define E_BADCHAR	"Invalid character"
#define E_WRONGNUM	"Wrong number of states"

static char *parse_recurse(buf, ecount)
char			*buf;
int			ecount;
{
    while (buf[0] == ' ' || buf[0] == '\t')
	buf++;

    if (ecount > NHSIZE + 2)
	return(E_WRONGNUM);
    else if (is_state(buf[0]))
    {
	/* 
	 * Sequential cons of next state onto the tip of the branch
	 * passed on.
	 */
	sofar[ecount] = stoi(*buf);
	return(parse_recurse(buf + 1, ecount + 1));
    }
    else if (buf[0] == '[')
    {
	char	*ep = strchr(buf, ']');

	if (ep == (char *)NULL)
	    return(E_NOCLOSE);

	/*
	 * Wild-carding time. We iterate through the set; for each member,
	 * we start a recurse from just past the *end* of the set.
	 */
	while (is_state(*++buf))
	{
	    char	*err;

	    sofar[ecount] = stoi(*buf);
	    if ((err = parse_recurse(ep + 1, ecount + 1)) == E_NOCLOSE
			|| err == E_BADCHAR || err == E_WRONGNUM)
		return(err);
	}
	return((char *)NULL);
    }
    else if (buf[0] == ']')
	return((char *)NULL);
    else if (buf[0] != '\n' && buf[0] != '\0')
	return(E_BADCHAR);
    else if (ecount < NHSIZE + 2)
	return(E_WRONGNUM);
    else
    {
	make_transition(sofar[0],sofar[1],sofar[2],sofar[3],sofar[4],sofar[5]);
	return((char *)NULL);
    }
}

/*
 * Here is the actual transition function implementation. Note that this
 * will eat up obscene amounts of memory for large STATEBITS values;
 * 32K for 3, 1M for 4, 32M for 5 and so on. However, it has the merit of
 * being simple and very fast.
 */

static cell_t trans[MAXSTATES][MAXSTATES][MAXSTATES][MAXSTATES][MAXSTATES];

int make_transition(s, a, b, c, d, t)
/* add a transition to the search tree */
int	s, a, b, c, d, t;
{
    int	oldval = trans[s][a][b][c][d];

    /* enforce rotational symmetry */
    trans[s][a][b][c][d] =
	trans[s][b][c][d][a] =
	    trans[s][c][d][a][b] =
		trans[s][d][a][b][c] = t;

    if (edebug)
    {
	(void) fprintf(stderr,
		       "+ %d%d%d%d%d & %d%d%d%d%d & %d%d%d%d%d & %d%d%d%d%d\n",
		       s, a, b, c, d,
		       s, b, c, d, a,
		       s, c, d, a, b,
		       s, b, c, d, a);
    }

    return(oldval);
}

int newstate(s, a, b, c, d)
/* get new state given the old one and the neighbors */
int s, a, b, c, d;
{
    return(trans[s][a][b][c][d]);
}


char *parse_rule(char *buf)
/* parse a transition rule */
{
    char	*err;

    if ((err = parse_recurse(buf, 0)) == E_NOCLOSE
		|| err == E_WRONGNUM || err == E_BADCHAR)
	return(err);
    else
	return((char *)NULL);
}

char *readrules(const char *file)
/* read a transition-rule set out of a file */
{
    static char			buf[80], buf2[80];
    FILE			*fp;
    int				s, n, c, r, line = 0;

    if ((fp = fopen(file, "r")) == (FILE *)NULL)
	return(SYSERR);

    ev_mode = TABLE_DRIVEN;
    diagonals = FALSE;
    ringdepth = 1;

    (void) memset(trans, BADSTATE, sizeof(trans));

    while (fgets(buf, sizeof(buf), fp) != (char *)NULL)
    {
	char	*cp, *errmsg;

	if (edebug)
	    (void) fputs(buf, stderr);

	line++;
	if ((cp = strchr(buf, '#')) != (char *)NULL)
	{
	    *cp = '\n';
	    *++cp = '\0';
	}

	if (buf[0] == '\n' || buf[0] == '\0')
	    continue;
	else if (strncmp(buf, "debug ", 6) == 0)
	    edebug = atoi(buf + 6);
	else if (strncmp(buf, "states ", 7) == 0)
	{
	    if (atoi(buf + 7) > MAXSTATES)
		fatal("I can't handle that many states\n");
#if STATEBITS > 1
	    changesize(sizeof(struct nstate_t));
	    alloc_states(atoi(buf + 7));
#endif /* STATEBITS > 1 */
	}
	else if (strncmp(buf, "passive ", 8) == 0)
	{
	    int i, j, k, l, m, maxpassive = atoi(buf+8);

	    for (i = 0; i <= maxpassive; i++)
		for (j = 0; j <= maxpassive; j++)
		    for (k = 0; k <= maxpassive; k++)
			for (l = 0; l <= maxpassive; l++)
			    for (m = 0; m <= maxpassive; m++)
				make_transition(i, j, k, l, m, i);
	}
	else if (sscanf(buf, "%d(%d*%d)%d", &s, &n, &c, &r) == 4)
	{
	    int	i,j,k,l;

	    if (c == 0)
	    {
		for (i = 0; i < maxstates; i++)
		    if (i != n)
			for (j = 0; j < maxstates; j++)
			    if (j != n)
				for (k = 0; k < maxstates; k++)
				    if (k != n)
					for (l = 0; l < maxstates; l++)
					    if (l != n)
						make_transition(s,i,j,k,l,r);
	    }
	    else if (c == 1)
	    {
		for (i = 0; i < maxstates; i++)
		    if (i != n)
			for (j = 0; j < maxstates; j++)
			    if (j != n)
				for (k = 0; k < maxstates; k++)
				    if (k != n)
					make_transition(s,n,i,j,k,r);
	    }
	    else if (c == 2)
	    {
		for (i = 0; i < maxstates; i++)
		    if (i != n)
			for (j = 0; j < maxstates; j++)
			    if (j != n)
			    {
				make_transition(s, n, n, i, j, r);
				make_transition(s, n, i, n, j, r);
			    }
	    }
	    else /* (c == 3) */
	    {
		for (i = 0; i < maxstates; i++)
		    if (i != n)
			make_transition(s, n, n, n, i, r);
	    }
	}
	else if ((errmsg = parse_recurse(buf, 0)) != (char *)NULL)
	{
	    (void) sprintf(buf2, "%s, line %d", errmsg, line);
	    (void) fclose(fp);
	    return(buf2);
	}
    }

    (void) fclose(fp);
    return((char *)NULL);
}
#endif /* STATEBITS > 1 */

/* now, pull it all together */

void generate(pattern *context)
{
    register tile	*cptr, *nextptr;

#ifdef PROF    
    link_called = link_search = 0;
    create_called = create_null = 0;
#endif PROF

    context->cellcount=0;

#if STATEBITS > 1
    if (ev_mode != VALENCE_DRIVEN)
	evolven(context);
    else
#endif
	evolve2(context);

    /* clean up tiles that have gone blank */
    for (cptr = context->tiles; cptr; cptr = nextptr)
    {
	nextptr = cptr->next;
	if (cptr->dead > DEADTIME)
	    killtile(context, cptr);
    }

    context->generations++;

#ifdef PROF
    printf("num=%d ",context->tilecount);
    if (link_called)
    {
	printf("l_c=%d ",link_called);
	if (link_search)
	    printf(" l_ave=%f ",link_search/(float)link_called);
    }
    if (create_called)
    {
	printf("c_c=%d ",create_called);
	if (create_null)
	{
	    printf(" c_ave=%f ",create_null/(float)create_called);
	}
    }

    printf("\n");
#endif PROF
}
