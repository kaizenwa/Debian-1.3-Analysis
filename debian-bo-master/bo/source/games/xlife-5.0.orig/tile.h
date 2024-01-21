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

#define BOXSIZE         8

/* someday, on a 64-bit machine, this might be unsigned int rather than long */
typedef unsigned long	u32bits;

typedef union 
{
    struct twostate_t
    {
	u32bits live1,live2,olive1,olive2;
	u32bits on[BOXSIZE];
    }
    twostate;	/* 48 bytes (on a 32-bit machine) */
#if STATEBITS > 1
    struct nstate_t
    {
	cell_t	cell[BOXSIZE][BOXSIZE];
	cell_t	ocell[BOXSIZE][BOXSIZE];
    }
    nstate;	/* 128 bytes (assuming char-sized cells) */
#endif /* STATEBITS */
}
cellbox;

typedef struct tile_t
{
    coord_t x,y;
    int dead;
    struct tile_t *up, *dn, *lf, *rt, *fore, *next,*hfore,*hnext;

    /* MUST be last in the structure or our dynamic allocation will fail */
    cellbox cells;
}
tile;

/*
 * A `pattern' is a linked list of tiles with its own context globals.
 * The universe may contain multiple patterns; they are overlayed on
 * the screen, but can be manipulated independently and evolve independently.
 */
typedef struct
{
    tile		*tiles;			/* head of the cell list */
    coord_t		tilecount;		/* count of boxes */
    coord_t		cellcount;		/* count of cells */
    unsigned long	generations;		/* context generation number */

#define HASHSIZE	32768
    tile		*hashlist[HASHSIZE];	/* hash list */
}
pattern;

extern pattern active, tentative;	/* the two pattern layers */
extern tile *freep;			/* free-tile list */

#define MAXCHANGE	8192
extern cellcount_t chgpoints[MAXSTATES], chgrects[MAXSTATES];
extern XPoint points[MAXSTATES][MAXCHANGE];
extern XRectangle rects[MAXSTATES][MAXCHANGE];

/* cell.c */
extern int getcell(cellbox *ptr, const int xdx, const int ydx);
extern void setcell(cellbox *ptr, const int xdx, const int ydx, const int val);
extern void forget(cellbox *ptr);
extern void displaybox(u32bits x, u32bits y, cellbox *ptr);
extern void trdisplaybox(u32bits x, u32bits y, cellbox *ptr);
extern void trdrawbox(coord_t xmin, coord_t ymin,
		      coord_t xmax, coord_t ymax, int color);

/* generate.c */
GLOBAL void generate(pattern *context);
GLOBAL void newrules(void);
GLOBAL char *readrules(const char *file);
GLOBAL char *parse_rule(char *buf);

/* tile.c */
extern void setscale(const int sc);
extern void changesize(const int newsize);
extern void initcells(pattern *context);
extern void chgcell(pattern *context, coord_t x, coord_t y, cell_t c);
extern void killtile(pattern *context, tile *ptr);
extern int lookcell(pattern *context, coord_t x, coord_t y);
extern tile *fetchtile(pattern *context, coord_t x, coord_t y);
extern tile *maketile(pattern *context, coord_t x, coord_t y);
extern void center(void);
extern void displaystats(void);
extern void clear_pattern(pattern *);
extern void clear(void);
extern void redisplay(void);
extern void redrawscreen(void);
extern void stamp(char *leader, FILE *ofp);
extern cellcount_t bounding_box(pattern *context,
		coord_t *xmin, coord_t *ymin, coord_t *xmax, coord_t *ymax);
extern void saveall(FILE *ofp, char mode);

