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
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>

#include <stdio.h>

#include "defs.h"
#include "tile.h"

#define	char	unsigned char
#include "icon.h"
#include "cursor_data.h"
#include "cursor_mask.h"
#undef char

#include "patchlevel.h"

extern char *getenv();

#ifndef GRAB_FRACTION
#define GRAB_FRACTION	0.8	/* fraction of screen to seize */
#endif /* GRAB_FRACTION */

static coord_t savex, savey;	/* last point seen during boxing */

static char *fixedcolors[] =
{
    "Black",
    "White",
    "Brown",
    "Blue",
    "Green",
    "Red",
    "Yellow",
    "Purple"
};

static XColor cellcolor[MAXSTATES];
static XGCValues xgcv;

int alloc_states(int newmaxstates)
{
    int	i;

    if (sizeof(fixedcolors)/sizeof(char *) < newmaxstates)
	fatal("Not enough colors for requested number of states\n");

    for (i = maxstates; i < newmaxstates; i++) 
    {
 	 XColor	exact;

	 if (!XAllocNamedColor(disp, DefaultColormap(disp,screen),
			       fixedcolors[i], &cellcolor[i], &exact))
	     fatal("Color allocation failed!\n");
	 xgcv.foreground = exact.pixel;
	 if ((cellgc[i] = XCreateGC(disp, mainw,
				    GCForeground | GCBackground,
				    &xgcv)) == 0)
	     fatal("Color creation failed!\n");
    }

    maxstates = newmaxstates;
}

void DoExpose(const int win)
{
    switch(win)
    {
    case LIFEWIN:
	while (XCheckMaskEvent(disp,ExposureMask,&event));
	XClearWindow(disp,lifew);
	redrawscreen();
    case INPUTWIN:
	XClearWindow(disp, inputw);
	XDrawString(disp, inputw, ntextgc, ICOORDS(0,0), inpbuf, 
                    strlen(inpbuf));
#if STATEBITS > 1
	if (maxstates > 2)
	{
	    int	j;

	    for (j = 0; j < maxstates; j++)
		color_button(j, j, 0);
	    setcolor(paintcolor, 0, 0);
	}
#endif
	break;
    case RULEWIN:
	showrules();
	break;
    case HELPWIN:
	redraw_help();
	break;
    default:
	break;

    }
}

void Motion(void)
/* handle X motion events */
{
    if (ClassifyWin(event.xmotion.window) == LIFEWIN)
    {
	if (event.xmotion.state & Button1MotionMask)
	{
	    drawcell(event.xmotion.x, event.xmotion.y, paintcolor);
       	    chgcell(&active,
		    XPOS(event.xmotion.x,xpos),
		    YPOS(event.xmotion.y,ypos), paintcolor);
	}
	else if (event.xmotion.state & Button3MotionMask)
	{
	    /* erase the old box, draw the new one */
	    erasebox(loadx, loady, savex, savey);
	    savex = XPOS(event.xmotion.x,xpos);
	    savey = YPOS(event.xmotion.y,ypos);
       	    drawbox(loadx, loady, savex, savey, 1);
	}
	else
	{
	    if (event.xmotion.state & Button3MotionMask)
	    {
		drawcell(event.xmotion.x, event.xmotion.y, 0);
		chgcell(&active,
			XPOS(event.xmotion.x,xpos),
			YPOS(event.xmotion.y,ypos),
			0); 
	    }
	}
    }	   
    XFlush(disp);
}

void DoResize(void)
{
    int owidth=width,oheight=height;
    width = event.xconfigure.width;
    height = event.xconfigure.height;
    xpos+= (owidth - width)/2;
    ypos+= (oheight - height)/2;	
    inputlength = width/FONTWIDTH;
    XResizeWindow(disp,inputw, 
                  width-RULEW-dispcoord*(COORDW+BORDERWIDTH)-BORDERWIDTH*2,
                  INPUTH);
    XResizeWindow(disp,lifew,width-BORDERWIDTH*2,height-INPUTH-BORDERWIDTH*3);
    XMoveWindow(disp,inputw,0,height-INPUTH-BORDERWIDTH*2);
    XMoveWindow(disp,rulew,width-RULEW-COORDW-BORDERWIDTH*2,
                            height-INPUTH-BORDERWIDTH*2);
    XMoveWindow(disp,coordw,width-COORDW-BORDERWIDTH*2,
                            height-INPUTH-BORDERWIDTH*2);
    redrawscreen();
    
}

int DoKeySymIn(keysym)
KeySym keysym;
{
    switch (keysym)
    {
    case XK_4:
    case XK_KP_4:
    case XK_Left:
	XClearWindow(disp,lifew);
	xpos-= (unsigned)width/2 >> scale;
	redrawscreen();
	break;
    case XK_6:
    case XK_KP_6:
    case XK_Right:
	XClearWindow(disp,lifew);
	xpos+= (unsigned)width/2 >> scale;
	redrawscreen();
	break;
    case XK_8:
    case XK_KP_8:
    case XK_Up:
	XClearWindow(disp,lifew);
	ypos-= (unsigned)height/2 >> scale;
	redrawscreen();
	break;
    case XK_2:
    case XK_KP_2:
    case XK_Down:
	XClearWindow(disp,lifew);
	ypos+= (unsigned)height/2 >> scale;
	redrawscreen();
	break;
    case XK_7:
    case XK_KP_7:
	XClearWindow(disp,lifew);
	xpos-= (unsigned)width/2 >> scale;
	ypos-= (unsigned)height/2 >> scale;
	redrawscreen();
	break;
    case XK_9:
    case XK_KP_9:
	XClearWindow(disp,lifew);
	xpos+= (unsigned)width/2 >> scale;
	ypos-= (unsigned)height/2 >> scale;
	redrawscreen();
	break;
    case XK_3:
    case XK_KP_3:
	XClearWindow(disp,lifew);
	xpos+= (unsigned)width/2 >> scale;
	ypos+= (unsigned)height/2 >> scale;
	redrawscreen();
	break;
    case XK_1:
    case XK_KP_1:
	XClearWindow(disp,lifew);
	xpos-= (unsigned)width/2 >> scale;
	ypos+= (unsigned)height/2 >> scale;
	redrawscreen();
	break;
    case XK_5:
    case XK_KP_5:
	center();
	break;
    case XK_Help:
	help();
	break;
    default:
	return 0;
    }

    /* could process it */
    return 1;
}

void DoKeyIn(kbuf)
char kbuf[16];
{
    char pstring[50];

    switch(kbuf[0])
    {
    case 'r':
	redrawscreen();
	break;

    case 'R':
#if STATEBITS > 1
	changesize(sizeof(struct twostate_t));
	alloc_states(2);
#endif /* STATEBITS > 1 */
	newrules();
	break;

#if STATEBITS > 1
    case 'F':
	loadrules();
	break;
#endif /* STATEBITS > 1 */

    case '=':
    case '+':
	if (scale < 7)
	{
	    XClearWindow(disp,lifew);
	    setscale(++scale);
	    xpos += XPOS(event.xmotion.x, 0);
	    ypos += YPOS(event.xmotion.y, 0);
	    redrawscreen();
	}
	break;

    case '.':
	XClearWindow(disp,lifew);
	xpos += (unsigned)(event.xbutton.x - width/2) >> (scale -1);
	ypos += (unsigned)(event.xbutton.y - height/2) >> (scale -1);
	redrawscreen();
	break;

    case '-':
	if (scale > 1)
	{
	    XClearWindow(disp,lifew);
	    xpos -= XPOS(event.xmotion.x, 0);
	    ypos -= YPOS(event.xmotion.y, 0);
	    setscale(--scale);
	    redrawscreen();
	}
	break;

    case 'g':
	confirmload();
	state=(state==RUN)?STOP:RUN;
	XClearWindow(disp,lifew);
	redrawscreen();
	break;

    case 'c':
	dispboxes ^= 1;
	break;

    case 'o':
	confirmload();
	generate(&active);
	XClearWindow(disp,lifew);
	redrawscreen();
	break;

    case 'p':
	if (dispcoord)
	{
	    dispcoord = FALSE;
	}
	else
	{
	    lastx=lastx-1;	/* fake move to force coordinates to print */
	    dispcoord = TRUE;
	}
	/* force resize of input window */
	XResizeWindow(disp,inputw,
		      width-RULEW-dispcoord*(COORDW+BORDERWIDTH)-BORDERWIDTH*2,
		      INPUTH);
	break;

    case 'O':
	announce("Origin set to active cell");
	xorigin=XPOS(event.xmotion.x,xpos);
	yorigin=YPOS(event.xmotion.y,ypos);
	break;

    case 'C':
	clear();
	break;

    case 'S':
	savefile();
	break;

    case 'W':
	saveloadscript();
	free_loadscript();
	break;

    case 'D':
	free_loadscript();
	if (tentative.tiles)
	{
	    undoload();
	    XClearWindow(disp,lifew);
	    redrawscreen();
	    announce("Load script (and latest load) discarded");
	}
	else
	    announce("Load script discarded"); 
	break;

    case 'l':
	loadfile();
	break;

    case 'h':
	confirmload();
	if (state==HIDE)
	{
	    state=STOP;
	    XClearWindow(disp,lifew);
	    redrawscreen();
	}
	else
	{
	    state=HIDE;
	}
	break;

    case '?':
	help();
	break;

    case 'f':
	settimeout(DELAY_FAST);
	break;

    case 'm':
	settimeout(DELAY_MED);
	break;

    case 's':
	settimeout(DELAY_SLOW);
	break;

    case '!':
	randomize();
	break;

    case 'N':
	name_file();
	break;

    case 'A':
	comment();
	break;

    case 'V':
	view_comments();
	break;

    case 'B':
	benchmark();
	break;

    case 'Q':
	exit(0);
	break;

    case 'U':
	/* Get rid of loaded pattern */
	undoload();
	XClearWindow(disp,lifew);
	redrawscreen();
	break;

    case 'I':
	/* Force confirm of loaded pattern */
	confirmload();
	XClearWindow(disp,lifew);
	redrawscreen();
	break;

    case 'G':
	/* perform some generations on loaded pattern */ 
	genload();
	XClearWindow(disp,lifew);
	redrawscreen();
	break;

#if STATEBITS > 1
    case 'a':
	set_transition();
	break;

    case 't':
	test_transition();
	break;
#endif

    default:
	break;
    }
    kbuf[0]='\0';		/* get rid of old keystroke so shift doesn't bring it back */
}

void Button(void)
/* handle a button-press event */
{
#if STATEBITS > 1
    if (ClassifyWin(event.xbutton.window) == INPUTWIN)
	setcolor(-1, event.xbutton.x, event.xbutton.y);
#endif				/* STATEBITS > 1 */

    if (ClassifyWin(event.xbutton.window) == LIFEWIN)
    {
	switch(event.xbutton.button)
	{
	case 1:
            if (tentative.tiles)
		moveload();
	    else
	    {
		drawcell(event.xbutton.x, event.xbutton.y, paintcolor);
		chgcell(&active,
			XPOS(event.xbutton.x,xpos),
			YPOS(event.xbutton.y,ypos),
			paintcolor);
		showcoord(TRUE);
		if (paintcolor == 1)
		    active.cellcount++;
#if STATEBITS == 1
		else if (paintcolor == 0)
		    active.cellcount--;
#endif
            }
	    break;
	case 2:
            if (tentative.tiles)
		flipload();
            else
	    {
		savex = loadx = XPOS(event.xmotion.x, xpos);
		savey = loady = YPOS(event.xmotion.y, ypos);		
            }
	    break;
	case 3:
            if (tentative.tiles)
		turnload();
	    else
	    {
		drawcell(event.xbutton.x, event.xbutton.y, 0);
		chgcell(&active,
			XPOS(event.xbutton.x,xpos),
			YPOS(event.xbutton.y,ypos),
			0); 
		showcoord(TRUE);
		active.cellcount--;
            }
	}
    }
    XFlush(disp);
}

static void Release()
{
    if (ClassifyWin(event.xbutton.window) == LIFEWIN
				&& event.xbutton.button == 3)
	make_tentative(loadx, loady, savex, savey);
}

main(argc, argv)
int argc;
char **argv;
{
    int i,tmp;
    Cursor cursor;
    Pixmap icon,cursor_data,cursor_mask;
    XSizeHints hints;
    XWMHints wm_hints;
    XClassHint class_hints;
    XSetWindowAttributes winat;
    XColor white,black,dummy;
    XComposeStatus stat;
    char *geomstring = NULL;
    char *initpat = NULL;
    struct timeval inputdelay, timeouttemp;
    LoadReq *loadqueue = NULL;
    XTextProperty windowName, iconName;
    unsigned long cwhite, cblack;
    char * window_name = "Xlife: a cellular-automaton laboratory";
    char * icon_name = "Xlife";
    
    if (!(disp = XOpenDisplay(getenv("DISPLAY"))))
	fatal("Can't open Display\n");

    if (XDefaultDepth(disp, screen) < STATEBITS)
	fatal("Not enough colors for compiled STATEBITS value\n");

    for (i = 1; i < argc; i++)
    {
	if (strcmp(argv[i], "-geometry") == 0)
	    geomstring = argv[++i];
	else if (*argv[i] == '=')
	    geomstring = argv[i];
	else if (*argv[i] != '-')
	{
	    initpat = argv[i];
	    break;
	}
    }

    screen = DefaultScreen(disp);
    rootw = RootWindow(disp, screen);
    cwhite = WhitePixel(disp, screen);
    cblack = BlackPixel(disp, screen);
    fcolor = cwhite;
    bcolor = cblack;

    hints.x = 0;
    hints.y = 0;    
    width = DisplayWidth(disp,screen);
    height = DisplayHeight(disp,screen);

    hints.width = width * GRAB_FRACTION;
    hints.height = height * GRAB_FRACTION;
    
    hints.flags = PPosition | PSize;

    if (geomstring != NULL)
    {
	int result;

	result = XParseGeometry(geomstring,&hints.x,&hints.y,
				&hints.width,&hints.height);
	if (result & XNegative)
	    hints.x += (DisplayWidth(disp,screen) - hints.width) * GRAB_FRACTION;

	if (result & YNegative)
	    hints.y += (DisplayHeight(disp,screen) - hints.height) * GRAB_FRACTION;
	if (result & XValue || result & YValue)
	{
	    hints.flags |= USPosition;
	    hints.flags &= ~PPosition;
	}
	if (result & WidthValue || result & HeightValue)
	{
	    hints.flags |= USSize;
	    hints.flags &= ~PSize;
	}
    }
    
    mainw = XCreateSimpleWindow(disp, rootw,
		0, 0, hints.width, hints.height, 0, fcolor, bcolor);

    if (!mainw)
	fatal("Can't open main window");

    icon = XCreateBitmapFromData(disp, mainw, icon_bits, icon_width, icon_height);

    if (XStringListToTextProperty ( &window_name, 1, &windowName ) == 0 )
    {
        (void) fprintf ( stderr, "%s: structure allocation for windowName failed.\n", argv[0] );
        exit (-1);
    }

    if (XStringListToTextProperty(&icon_name, 1, &iconName) == 0)
    {
        (void) fprintf ( stderr, "%s: structure allocation for iconName failed.\n", argv[0] );
        exit (-1);
    }

    wm_hints.initial_state = NormalState;
    wm_hints.input = True;
    wm_hints.icon_pixmap = icon;
    wm_hints.flags = IconPixmapHint | StateHint | InputHint;

    class_hints.res_name =  argv[0];
    class_hints.res_class =  "Basicwin";

    XSetWMProperties(disp, mainw, &windowName, &iconName, argv, argc, &hints, &wm_hints, &class_hints );

    changesize(sizeof(struct twostate_t));
    alloc_states(2);
    black = cellcolor[0];
    white = cellcolor[1];

    /* text display is forced to black on white */
    xgcv.background = cwhite;
    xgcv.foreground = cblack;
    ntextgc = XCreateGC(disp, mainw, GCForeground | GCBackground, &xgcv);
    btextgc = XCreateGC(disp, mainw, GCForeground | GCBackground, &xgcv);
    xgcv.background = bcolor;
    xgcv.foreground = fcolor;

    /* create XOR GC for pivot display */
    tmp=xgcv.function;
    xgcv.function = GXinvert;
    xorgc = XCreateGC(disp, mainw, GCForeground |
                                   GCBackground | GCFunction, &xgcv);
    xgcv.function=tmp;
    
    if (!((nfont = XLoadQueryFont(disp, NORMALFONT)) && (bfont = XLoadQueryFont(disp, BOLDFONT))))
	fatal("Can't load font\n");
    XSetFont(disp, ntextgc, nfont->fid);
    XSetFont(disp, btextgc, bfont->fid);
    xgcv.function = GXcopy;
    xgcv.plane_mask = 1;

    cursor_data = XCreateBitmapFromData(disp, mainw, cursor_data_bits, cursor_data_width, cursor_data_height);
    cursor_mask = XCreateBitmapFromData(disp, mainw, cursor_mask_bits, cursor_mask_width, cursor_mask_height);
    cursor = XCreatePixmapCursor(disp, cursor_data, cursor_mask, &white, &black, cursor_data_x_hot, cursor_data_y_hot);
    XDefineCursor(disp, mainw, cursor);
    
    inputgc = XCreateGC(disp, mainw, GCFunction | GCPlaneMask, &xgcv);

    width = hints.width;
    height = hints.height;

    lifew = XCreateSimpleWindow(disp, mainw,
		0, 0,width-BORDERWIDTH*2, (height - INPUTH - BORDERWIDTH*3), 
                BORDERWIDTH,
		fcolor, bcolor);
    helpw = XCreateSimpleWindow(disp, mainw,
		0, 0,width-BORDERWIDTH*2, (height - BORDERWIDTH*2), 
                BORDERWIDTH,
		cblack, cwhite);
    rulew = XCreateSimpleWindow(disp, mainw,
		width-COORDW-RULEW-BORDERWIDTH*2,
		height-INPUTH-BORDERWIDTH*2,
                RULEW, INPUTH, BORDERWIDTH,
		cblack, cwhite);
    coordw = XCreateSimpleWindow(disp, mainw,
		width-COORDW-BORDERWIDTH*2,
		height-INPUTH-BORDERWIDTH*2,
                COORDW, INPUTH, BORDERWIDTH,
		cblack, cwhite);
    inputw = XCreateSimpleWindow(disp, mainw,
		0, (height - INPUTH - BORDERWIDTH*2), 
                width - (COORDW + RULEW + BORDERWIDTH)-BORDERWIDTH*2, 
                INPUTH, BORDERWIDTH,
		cblack, cwhite);

    winat.win_gravity = SouthGravity;
    XChangeWindowAttributes(disp,inputw,CWWinGravity,&winat);

    XSelectInput(disp, mainw, ExposureMask | StructureNotifyMask);
    XSelectInput(disp, inputw, KeyPressMask | ButtonPressMask | ExposureMask);
    XSelectInput(disp, lifew, KeyPressMask | ButtonPressMask | Button1MotionMask | PointerMotionMask | Button3MotionMask | ButtonReleaseMask | ExposureMask);
    XSelectInput(disp, helpw, KeyPressMask | ExposureMask);
    XSelectInput(disp, rulew, ExposureMask);
    XSelectInput(disp, coordw, ExposureMask);

    fileinit();
    initcells(&active);
    numcomments = 0;
    settimeout(0);
    xpos = xorigin = lastx = STARTX;
    ypos = yorigin = lasty = STARTY;
    dispcoord = TRUE;
    dispboxes = FALSE;
    setscale(scale = 4);
    born = 8;
    live = 12;
    inputlength = width/FONTWIDTH;
    state = STOP;
    fname[0]='\0';
    (void) strcpy(active_rules, "life");
#if STATEBITS > 1
    paintcolor = 1;
#endif
    gentab();
    srandom(time((time_t *)NULL));

    /*
     * Only accept one pattern since it is highly unlikely that overlaying
     * n patterns is what you want to do
     */
    if (initpat != NULL)
    {
        loadx = xpos; 
        loady = ypos;
        txx = 1; 
        txy = 0; 
        tyx = 0; 
        tyy = 1; 
        add_loadreq(&loadqueue,0,initpat,0,
                    STARTX, STARTY, 1,0,0,1);
        do_loadreq(loadqueue);
    }

    XMapWindow(disp, inputw);
    XMapWindow(disp, helpw);
    XMapWindow(disp, lifew);
    XMapWindow(disp, mainw);
    XMapWindow(disp, rulew);
    XMapWindow(disp, coordw);
    XLowerWindow(disp, helpw);

    showcoord(TRUE);
    showrules();
    XFlush(disp);
    
    for (;;)
    {
	while (XCheckMaskEvent(disp, KeyPressMask | ButtonPressMask | Button1MotionMask | PointerMotionMask | Button3MotionMask | ButtonReleaseMask | ExposureMask | StructureNotifyMask,&event))
	{
	    switch(event.type)
	    {
	      case KeyPress:
		XLookupString(&event.xkey, keybuf, 16, &ks, &stat);
		if (!DoKeySymIn(ks))
		    DoKeyIn(keybuf);
		break;
	      case MotionNotify:
                /* don't allow drawing until load is confirmed */
		if (!tentative.tiles)
		    Motion();
		break;
	      case ButtonPress:
		Button();
		break;
	      case ButtonRelease:
		Release();
		break;
	      case ConfigureNotify:
		DoResize();
		break;
	      case Expose:
		DoExpose(ClassifyWin(event.xexpose.window));
	      default:
		break;
	    }

        }

	showcoord(FALSE);
	showrules();

	if ((state == RUN) || (state == HIDE)) {
	    generate(&active);
	    redisplay();
	}
	else
	{
	    inputdelay.tv_sec = 0;
	    inputdelay.tv_usec = 100000;
	    (void) select(32,0,0,0,&inputdelay);
	}

	if (state== RUN)
	{
	    timeouttemp.tv_sec = timeout.tv_sec;
	    timeouttemp.tv_usec = timeout.tv_usec;
	    (void) select(32,0,0,0,&timeouttemp);
	}
    }
}

/* main.c ends here */
