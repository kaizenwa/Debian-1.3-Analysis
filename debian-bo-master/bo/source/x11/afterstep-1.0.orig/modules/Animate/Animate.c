/*
 * Animate! Animation module for AfterStep
 * 
 * Copyright (c) 1996 Alfredo Kengi Kojima (kojima@inf.ufrgs.br)
 * 
 * Copyright (c) 1996 Kaj Groner <kajg@mindspring.com>
 *   Added .steprc parsing and twisty iconify.
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
/*
 * changes needed in afterstep
 * broadcast(M_DEICONIFY) must include the icon position>
 */

#include <stdlib.h>
#include <stdio.h>
#include <X11/Xlib.h>
#include <signal.h>
#include <math.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include "../../configure.h"
#include "../../afterstep/module.h"
#include "../../lib/aftersteplib.h"
#include "Animate.h"

#define AS_PI 3.14159265358979323846

Display *dpy;
int scr;
Window root;
GC DrawGC;
char *ProgName;
int Channel[2];
void Loop();
void ParseOptions(char *);

struct ASAnimate Animate = { NULL, ANIM_ITERATIONS, ANIM_DELAY,
                             ANIM_TWIST, ANIM_WIDTH, AnimateResizeTwist };

/*
 * This makes a twisty iconify/deiconify animation for a window, similar to
 * MacOS.  Parameters specify the position and the size of the initial
 * window and the final window
 */
void AnimateResizeTwist(int x, int y, int w, int h, int fx, int fy, int fw, int fh)
{
    float cx, cy, cw, ch;
    float xstep, ystep, wstep, hstep;
    XPoint points[5];
    float angle, angle_finite, a, d;
    
    x += w/2;
    y += h/2;
    fx += fw/2;
    fy += fh/2;

    xstep = (float)(fx-x)/Animate.iterations;
    ystep = (float)(fy-y)/Animate.iterations;
    wstep = (float)(fw-w)/Animate.iterations;
    hstep = (float)(fh-h)/Animate.iterations;

    cx = (float)x;
    cy = (float)y;
    cw = (float)w;
    ch = (float)h;
    a = atan(ch/cw);
    d = sqrt((cw/2)*(cw/2)+(ch/2)*(ch/2));

    angle_finite = 2*AS_PI*Animate.twist;
    for (angle=0;; angle+=(float)(2*AS_PI*Animate.twist/Animate.iterations)) {
        if (angle > angle_finite)
	    angle = angle_finite;
        points[0].x = cx+cos(angle-a)*d;
        points[0].y = cy+sin(angle-a)*d;
        points[1].x = cx+cos(angle+a)*d;
        points[1].y = cy+sin(angle+a)*d;
        points[2].x = cx+cos(angle-a+AS_PI)*d;
        points[2].y = cy+sin(angle-a+AS_PI)*d;
        points[3].x = cx+cos(angle+a+AS_PI)*d;
        points[3].y = cy+sin(angle+a+AS_PI)*d;
        points[4].x = cx+cos(angle-a)*d;
        points[4].y = cy+sin(angle-a)*d;
	XGrabServer(dpy);
	XDrawLines(dpy, root, DrawGC, points, 5, CoordModeOrigin);
	XFlush(dpy);
	sleep_a_little(Animate.delay*1000);
	XDrawLines(dpy, root, DrawGC, points, 5, CoordModeOrigin);
	XUngrabServer(dpy);
	cx+=xstep;
	cy+=ystep;
	cw+=wstep;
	ch+=hstep;
        a = atan(ch/cw);
        d = sqrt((cw/2)*(cw/2)+(ch/2)*(ch/2));
        if (angle >= angle_finite)
	    break;
    }		
    XFlush(dpy);    
}

/*
 * This makes a zooming iconify/deiconify animation for a window, like most
 * any other icon animation out there.  Parameters specify the position and
 * the size of the initial window and the final window
 */
void AnimateResizeZoom(int x, int y, int w, int h, int fx, int fy, int fw, int fh)
{
    float cx, cy, cw, ch;
    float xstep, ystep, wstep, hstep;
    int i;
    
    xstep = (float)(fx-x)/Animate.iterations;
    ystep = (float)(fy-y)/Animate.iterations;
    wstep = (float)(fw-w)/Animate.iterations;
    hstep = (float)(fh-h)/Animate.iterations;

    cx = (float)x;
    cy = (float)y;
    cw = (float)w;
    ch = (float)h;
    for (i=0; i<Animate.iterations; i++) {
	XGrabServer(dpy);
	XDrawRectangle(dpy, root, DrawGC, (int)cx, (int)cy, (int)cw, (int)ch);
	XFlush(dpy);
	sleep_a_little(Animate.delay);
	XDrawRectangle(dpy, root, DrawGC, (int)cx, (int)cy, (int)cw, (int)ch);
	XUngrabServer(dpy);
	cx+=xstep;
	cy+=ystep;
	cw+=wstep;
	ch+=hstep;
    }		
    XFlush(dpy);    
}

#if 0
/*
 * This makes a animation that looks like that light effect
 * when you turn off an old TV.
 * Used for window destruction
 */
void AnimateClose(int x, int y, int w, int h)
{
    int i, step;
    
    if (h>4) {
	step = h*4/Animate.iterations;
	if (step==0) {
	    step = 2;
	}
	for (i=h; i>=2; i-=step) {
	    XDrawRectangle(dpy, root, DrawGC, x, y, w, i);
	    XFlush(dpy);
	    sleep_a_little(ANIM_DELAY2*600);
	    XDrawRectangle(dpy, root, DrawGC, x, y, w, i);
	    y+=step/2;
	}
    }
    if (w<2) return;
    step = w*4/Animate.iterations;
    if (step==0) {
	step = 2;
    }
    for (i=w; i>=0; i-=step) {
	XDrawRectangle(dpy, root, DrawGC, x, y, i, 2);
	XFlush(dpy);
	sleep_a_little(ANIM_DELAY2*1000);
	XDrawRectangle(dpy, root, DrawGC, x, y, i, 2);
	x+=step/2;
    }
    sleep_a_little(100000);    
    XFlush(dpy);
}
#endif

void DeadPipe(int foo)
{
    exit(0);
}

int main(int argc, char **argv)
{
    char *temp;
    XGCValues gcv;
    unsigned long color;
    char mask_mesg[20];
    
    ProgName = rindex(argv[0], '/')?rindex(argv[0], '/')+1:argv[0];
    if (argc != 6) {
	fprintf(stderr,"%s should only be executed by afterstep!\n",ProgName);
	exit(1);
    }
    
    /* Dead pipe == AS died */
    signal (SIGPIPE, DeadPipe);
    
    Channel[0] = atoi(argv[1]);
    Channel[1] = atoi(argv[2]);
    
    dpy = XOpenDisplay("");
    if (dpy==NULL) {
	fprintf(stderr,"%s: could not open display\n",ProgName);
	exit(1);
    }
    root = DefaultRootWindow(dpy);
    scr = DefaultScreen(dpy);

    sprintf(mask_mesg,"SET_MASK %lu\n",(unsigned long)(M_CONFIGURE_WINDOW|
            M_ICONIFY|
            M_DEICONIFY|
	    M_LOCKONSEND));

    SendInfo(Channel, mask_mesg, 0);

    ParseOptions(argv[3]);
    color = WhitePixel(dpy,scr);
    if (Animate.color) {
	XColor xcol;
	if (XParseColor(dpy,DefaultColormap(dpy,scr),Animate.color, &xcol)) {
	    if (XAllocColor(dpy, DefaultColormap(dpy,scr), &xcol)) {
		color = xcol.pixel;
	    } else {
		fprintf(stderr,"%s: could not allocate color '%s'\n",
			ProgName,Animate.color);
	    }	    
	} else {
	    fprintf(stderr,"%s: could not parse color '%s'\n",
		    ProgName,Animate.color);
	}
    }
    gcv.line_width = Animate.width;
    gcv.function = GXequiv;
    gcv.foreground = color;
    gcv.background = color;
    gcv.subwindow_mode = IncludeInferiors;
    DrawGC=XCreateGC(dpy, root, GCFunction|GCForeground|GCLineWidth|GCBackground
			|GCSubwindowMode, &gcv);
    SendText(Channel,"Nop",0);
    Loop();
}

void GetWindowSize(Window win, int *x, int *y, int *w, int *h)
{
    Window root_r;
    unsigned int bw_r, d_r;
    
    XGetGeometry(dpy, win, &root_r, x, y, (unsigned int*) w, (unsigned int*) h,
		 &bw_r, &d_r);
}

/*
 * Wait for some event like iconify, deiconify and stuff.
 * 
 */
void Loop()
{
    unsigned long header[3], *body;
    int c, x0, y0, w0, h0, xf, yf, wf, hf;
    int iconify;
    Window win;
    
    while (1) {
	c=ReadASPacket(Channel[1], header, &body);
	if (c>0) {
	    switch (header[1]) {
	     case M_DEICONIFY:
		win = body[1];
		x0 = body[3];
		y0 = body[4];

		GetWindowSize(win, &xf, &yf, &wf, &hf);		
		Animate.resize(x0, y0, 64, 64, xf, yf, wf, hf);
		break;

	     case M_ICONIFY:
		xf = body[3];
		yf = body[4];
		if (xf<=-10000) break;
		wf = 64;
		hf = 64;
		free(body);

                SendInfo(Channel, "UNLOCK 1\n", 0);
		c=ReadASPacket(Channel[1], header, &body);
		if (c<=0) break;
		if (header[1]!=M_CONFIGURE_WINDOW) break;
		x0 = body[3];
		y0 = body[4];
		w0 = body[5];
		h0 = body[6];
		Animate.resize(x0, y0, w0, h0, xf, yf, wf, hf);
		break;
	    }
	    free(body);	    
	}
        SendInfo(Channel, "UNLOCK 1\n", 0);
    }
}

/*****************************************************************************
 * 
 * This routine is responsible for reading and parsing the config file
 * Ripped from the Pager.
 *
 ****************************************************************************/
void ParseOptions(char *filename)
{
    FILE *fd = (FILE *)0;
    char *line, *t;
    int Clength;

    line = safemalloc(256);

    fd = fopen(filename,"r");
    if(fd == (FILE *)0)
    {
        fprintf(stderr,"%s: can't open config file %s",ProgName,filename);
        exit(1);
    }

    Clength = strlen(ProgName);

    while (fgets(line, 256-1, fd) != (char *)0)
    {
	line[strlen(line)-1] = 0;

	if (*line == '*' && mystrncasecmp(line+1, ProgName, Clength)==0)
	{
	    t = line + Clength + 1;
            if (mystrncasecmp(t, "Color", 5)==0)
	    {
	        for (t+=5; isspace(*t)&&*t != '\n'&&*t != 0; t++);

	        if (Animate.color)
	            free(Animate.color);
	        Animate.color = strdup(t);
	    }

            else if (mystrncasecmp(t, "Delay", 5)==0)
	    {
	        for (t+=5; isspace(*t)&&*t != '\n'&&*t != 0; t++);

	        Animate.delay = atoi(t);
            }

            else if (mystrncasecmp(t, "Width", 5)==0)
	    {
	        for (t+=5; isspace(*t)&&*t != '\n'&&*t != 0; t++);

	        Animate.width = atoi(t);
            }

            else if (mystrncasecmp(t, "Twist", 5)==0)
	    {
	        for (t+=5; isspace(*t)&&*t != '\n'&&*t != 0; t++);

	        Animate.twist = atof(t);
            }

            else if (mystrncasecmp(t, "Iterations", 5)==0)
	    {
	        for (t+=10; isspace(*t)&&*t != '\n'&&*t != 0; t++);

	        Animate.iterations = atoi(t);
            }

            else if (mystrncasecmp(t, "Resize", 6)==0)
	    {
	        for (t+=6; isspace(*t)&&*t != '\n'&&*t != 0; t++);

		if (mystrncasecmp(t, "Twist", 5)==0)
		    Animate.resize = AnimateResizeTwist;
		else if (mystrncasecmp(t, "Zoom", 4)==0)
		    Animate.resize = AnimateResizeZoom;
		else
		    fprintf(stderr, "%s: Bad Resize method '%s'\n", ProgName, t);
            }

	}
    }

    free(line);
    fclose(fd);
}

