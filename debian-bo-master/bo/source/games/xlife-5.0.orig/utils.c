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
#include <pwd.h>
#include <string.h>

void announce(const char *s)
/* display a string in the input window */
{
    (void) strcpy(inpbuf, s);
    DoExpose(INPUTWIN);
}

void fatal(const char *s)
{
    fprintf(stderr, s);
    exit(-22);
}

int ClassifyWin(win)
Window win;
{
    if (win == lifew)
	return LIFEWIN;
    if (win == inputw)
	return INPUTWIN;
    if (win == mainw)
	return MAINWIN;
    if (win == rulew)
	return RULEWIN;
    if (win == coordw)
	return COORDWIN;
    if (win == helpw)
	return HELPWIN;
}

char *itoa(int n)
{
    static char buf[16];
    char sign;
    char *ptr;

    if (n >= 0)
	sign = ' ';
    else {
	n = -n;
	sign = '-';
    }
    buf[15] = 0;
    ptr = buf + 14;
    do {
	*--ptr = n % 10 + '0';
	if (ptr <= buf)
	    return(buf);
	n /= 10;
    } while (n > 0);
    *--ptr = sign;
    return(ptr);
}

void drawcell(coord_t x, coord_t y, cell_t c)
{
    if (scale == 1)
	XDrawPoint(disp,lifew,cellgc[c],x,y);
    else
    {
	int sc;
	sc = (1 << scale -1) - (scale > 2);
	XFillRectangle(disp,lifew,cellgc[c],MOUSE2CELL(x),MOUSE2CELL(y),sc,sc);
    }
}

void drawpivot(coord_t x, coord_t y)
{
    int sc;

    if (scale > 2) 
    {
	sc = (1 << scale -1) - 1;
	XDrawLine(disp,lifew,xorgc,x,y,x+sc-1,y+sc-1);
	XDrawLine(disp,lifew,xorgc,x,y+sc-1,x+sc-1,y);
	XDrawPoint(disp,lifew,xorgc,x+sc/2,y+sc/2);
    }
    else 
	XDrawPoint(disp,lifew,cellgc[1],x,y);
}

void drawbox(coord_t x1, coord_t y1, coord_t x2, coord_t y2, int color)
/* box the area defined by two corners (life-world coordinates) */
{
    if (scale > 2)
    {
	int width, height, xbase, ybase, cellsize;
	width    = (x1 > x2) ? (x1 - x2) : (x2 - x1);
	height   = (y1 > y2) ? (y1 - y2) : (y2 - y1);
	xbase    = (x1 > x2) ? x2 : x1;
	ybase    = (y1 > y2) ? y2 : y1;
	cellsize = (1 << (scale - 1));

	XDrawRectangle(disp, lifew, cellgc[color],
		       RXPOS(xbase, xpos) - 1, RYPOS(ybase, ypos) - 1,
		       (width + 1)*cellsize, (height + 1)*cellsize);
	/*
	 * Actually, draw 2 boxes.  The second is two pixels wider.  This
	 * compensates for the fact that a single-pixel vertical row is
	 * near-invisible on many displays.
	 */
	XDrawRectangle(disp, lifew, cellgc[color],
		       RXPOS(xbase, xpos) - 2, RYPOS(ybase, ypos) - 1,
		       (width + 1)*cellsize + 2, (height + 1)*cellsize);

	/* At small scales, draw a third for better visibility */
	if (scale > 3)
	    XDrawRectangle(disp, lifew, cellgc[color],
			   RXPOS(xbase, xpos) - 3, RYPOS(ybase, ypos) - 2,
			   (width + 1)*cellsize + 3, (height + 1)*cellsize);

	XFlush(disp);
    }
}

void erasebox(coord_t x1, coord_t y1, coord_t x2, coord_t y2)
/* actually, overwrite the box outline in black */
{
    if (scale > 2)
    {
	drawbox(x1, y1, x2, y2, 0);

	/*
	 * The process of erasing the old box may have messed up some
	 * cell edges in the box interior or around its outside edge. 
	 * Fix this by forcing a redisplay.
	 */
	redisplay();
    }
}

void showcoord(const bool force)
/* update the coordinate & state display */
{
    /* Coordinate display added by Paul Callahan (callahan@cs.jhu.edu) */
    if (dispcoord)
    {
	coord_t x,y;
	char pstring[50];

	x = XPOS(event.xmotion.x, xpos); 
	y = YPOS(event.xmotion.y, ypos); 

	if (force || lastx != x || lasty != y)
	{
	    lastx=x;
	    lasty=y; 
	    sprintf(pstring, "(%d,%d)=%d",
		    x - xorigin, y - yorigin,
		    lookcell(&active, x,y));
	    XClearWindow(disp,coordw);
	    XDrawString(disp,coordw,ntextgc,CWCOORDS(0,0),
			pstring,strlen(pstring));
	}
    }
}

void showrules(void)
{
    char	buf[80], *cp;

    (void) strcpy(buf, active_rules);
    buf[0] = toupper(buf[0]);
    if ((cp = strrchr(buf, '.')) != (char *)NULL)
	*cp = '\0';
    XClearWindow(disp,rulew);
    XDrawString(disp,rulew,ntextgc,CWRULES(0,0),
		buf,strlen(buf));
}

void randomize(void)
{
#define DENSITY	0.1
    cellcount_t num = ((unsigned)(width * height) >> (scale - 1)) * DENSITY;
    
    while (num--)
    {
	chgcell(&active,
		random()%((unsigned)width>>(scale-1))+xpos,
		random()%((unsigned)height>>(scale-1))+ypos,
		1);
    }
    redisplay();
}

void settimeout(unsigned long ms)
{
    timeout.tv_sec=ms/1000;
    timeout.tv_usec=(ms%1000)*1000;
}

void benchmark(void)
{
    char outbuf[100];
    u_long num,count;
    double tm;
    struct timeval start,end;
    struct timezone tmz;

    state=STOP;

    strcpy(inpbuf,"Number of generations:");
    minbuflen=22;
 
    XClearWindow(disp,inputw);
    XDrawString(disp, inputw, ntextgc,ICOORDS(0,0),inpbuf, strlen(inpbuf));

    getxstring();

    strcpy(outbuf,inpbuf+22);
    inpbuf[0]=0;
    sscanf(outbuf,"%d",&count);
    
    gettimeofday(&start,&tmz);
    for (num=0;num<count;num++)
	generate(&active);

    gettimeofday(&end,&tmz);
    tm=(((end.tv_sec * 1000000) + end.tv_usec) - ((start.tv_sec * 1000000) + start.tv_usec))/1000000.0;
    sprintf(inpbuf,"%s:%f","Time",tm);

    XClearWindow(disp,lifew);
    redrawscreen();
    XClearWindow(disp,inputw);
    XDrawString(disp, inputw, ntextgc,ICOORDS(0,0),inpbuf, strlen(inpbuf));
    getxstring();
    inpbuf[0]=0;
}


#if STATEBITS > 1
void color_button(int location, int color, int large)
{
    XWindowAttributes inattrs;

    XGetWindowAttributes(disp, inputw, &inattrs);
    XFillRectangle(disp,inputw,cellgc[color],
		   inattrs.width - BORDERWIDTH
			   - (maxstates - location) * (FONTWIDTH + 3)
			   - large,
		   BORDERWIDTH + 3 - large,
		   FONTWIDTH + large * 2, FONTHEIGHT - 1 + large * 2);
}

void stamp(char *leader, FILE *ofp)
/* generate \n-terminated time and user info string */
{
    struct passwd *pw;
    char machine[80];
    unsigned long timeval;
    extern char *ctime();

    timeval = time(0);
    gethostname(machine, sizeof(machine));
    if ((pw = getpwuid(getuid())) != NULL)
	fprintf(ofp, "%s %s \"%s\"@%s %s",
		leader, pw->pw_name, pw->pw_gecos, machine,ctime(&timeval));
}

void setcolor(int val, unsigned long x, unsigned long y)
{
    if (val == -1)
    {
	int	j, left;
	XWindowAttributes inattrs;

	XGetWindowAttributes(disp, inputw, &inattrs);
	left = inattrs.width - BORDERWIDTH - maxstates * (FONTWIDTH + 3);

	if (x < left)
	    return;

	val = (x - left) / (FONTWIDTH + 3);
    }

    color_button(paintcolor, 1, 2); 
    color_button(paintcolor, paintcolor, 0);
    color_button(val, 0, 2);
    color_button(val, val, 0);
    paintcolor = val;
}
#endif /* STATEBITS > 1 */

