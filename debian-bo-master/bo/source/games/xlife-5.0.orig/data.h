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

/* file.c */
GLOBAL void fileinit(void);
GLOBAL void savefile(void);
GLOBAL void do_loadreq(LoadReq *loadqueue);
GLOBAL int add_loadreq();
GLOBAL void free_loadscript(void);
GLOBAL void saveloadscript(void);
GLOBAL void confirmload(void);
GLOBAL void undoload(void);
GLOBAL void genload(void);
GLOBAL void loadfile(void);
GLOBAL void use_rules(char *cp);
GLOBAL void loadrules(void);

/* file_misc.c */
GLOBAL void name_file(void);
GLOBAL void comment(void);
GLOBAL void view_comments(void);

/* gentab.c */
GLOBAL void gentab(void);

/* help.c */
GLOBAL void help(void);
GLOBAL void redraw_help(void);

/* key.c */
GLOBAL void getxstring(void);
GLOBAL void set_transition(void);
GLOBAL void test_transition(void);
GLOBAL int patch_transition(cell_t s, 
			    cell_t n1, cell_t n2, cell_t n3, cell_t n4);

/* main.c */
GLOBAL void Motion(void), DoResize(void), DoExpose(const int win);
GLOBAL int alloc_states(int newmaxstates);

/* tentative.c */
GLOBAL void moveload(void);
GLOBAL void flipload(void);
GLOBAL void turnload(void);
GLOBAL void boxpattern(const int color);
GLOBAL void make_tentative(coord_t x1, coord_t y1, coord_t x2, coord_t y2);

/* tile.c declarations are in tile.h */

/* utils.c */
GLOBAL void announce(const char *s);
GLOBAL void fatal(const char *s);
GLOBAL int ClassifyWin();
GLOBAL char *itoa(int n);
GLOBAL void drawcell(coord_t x, coord_t y, cell_t c);
GLOBAL void drawpivot(coord_t x, coord_t y);
GLOBAL void drawbox(coord_t x1, coord_t y1, coord_t x2, coord_t y2, int color);
GLOBAL void erasebox(coord_t x1, coord_t y1, coord_t x2, coord_t y2);
GLOBAL void showcoord(const bool force);
GLOBAL void showrules(void);
GLOBAL void benchmark(void);
GLOBAL void randomize(void);
GLOBAL void settimeout(unsigned long ms);
GLOBAL void color_button(int location, int color, int large);
GLOBAL void setcolor(int val, unsigned long x, unsigned long y);

/* UNIX interface */
extern int sys_nerr, errno;
extern char *sys_errlist[];
#define SYSERR sys_errlist[(errno > sys_nerr? 0 : errno)]

/* X I/O state information */
GLOBAL Display *disp;
GLOBAL Window rootw, mainw, lifew, helpw, inputw, coordw, rulew;
GLOBAL int screen;
GLOBAL unsigned long fcolor, bcolor;
GLOBAL XEvent event;
GLOBAL XFontStruct *nfont, *bfont;
GLOBAL GC ntextgc, btextgc,inputgc,cellgc[MAXSTATES],xorgc;
GLOBAL KeySym ks;

#define INPBUFLEN 255
GLOBAL char inpbuf[INPBUFLEN];
GLOBAL int minbuflen;
#define DIRBUFLEN 100
GLOBAL int numcomments;
#define MAXCOMMENTS 50
GLOBAL char comments[MAXCOMMENTS][256];
GLOBAL char loadirbuf[DIRBUFLEN];
GLOBAL char keybuf[16];
GLOBAL char lookup[0xfffff];
GLOBAL char fname[256];

#if STATEBITS > 1
extern unsigned int maxstates;
#endif /* STATEBITS > 1 */

GLOBAL coord_t xpos,ypos;	/* coords of upper left corner of viewport */
GLOBAL coord_t xorigin,yorigin;	/* virtual origin */
GLOBAL coord_t lastx,lasty;	/* last coord pair displayed on status line */
GLOBAL coord_t loadx, loady;	/* location to load new points */
GLOBAL int txx,txy,tyx,tyy;	/* transform for tentative points */ 

GLOBAL bool dispboxes;
GLOBAL unsigned long scale;
GLOBAL unsigned long born;
GLOBAL unsigned long live;

GLOBAL int width;
GLOBAL int height;
GLOBAL int inputlength;
GLOBAL int state;
GLOBAL bool dispcoord;
GLOBAL struct timeval timeout;

#if STATEBITS == 1
#define paintcolor 1
#else
GLOBAL int paintcolor;
GLOBAL char active_rules[100];
#endif

#ifdef PROF
GLOBAL int link_called;
GLOBAL int link_search;
GLOBAL int create_called;
GLOBAL int create_null;
#endif /* PROF */

