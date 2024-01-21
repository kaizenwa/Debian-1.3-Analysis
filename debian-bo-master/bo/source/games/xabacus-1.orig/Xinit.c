#include <stdio.h>
#include <X11/cursorfont.h>

#include "icon.bm"

#include "main.h"

Pixel getcolor();
Pixel bgcolor, bdcolor;	/* background & border*/

/*--------------------------------------------------------------------
 * makeWindows, connects to X-server, creates the application windows, 
 *			allocates GC's & sets WM hints, creates cursors & maps windows
 *
 *			passed: argc, argv straight from the commandline
 *			returns: nothing
 *			dep: win struct, a bunch of globals; see maindefs.h
 */
makeWindows(argc, argv, display_name)
int argc; 
char **argv;
char *display_name;
{
unsigned int border_width=1;
char *window_name=APPNAME;
char *icon_name=APPNAME;
/*char *display_name=NULL;*/
Pixmap icon_pixmap;
int i;

#ifdef DEBUG
	fprintf(stderr,"DEBUG: makeWIndows()\n");
#endif

	/* connect to X server */
    if ( (display=XOpenDisplay(display_name)) == NULL ) {
        fprintf(stderr,"%s: cannot connect to X server %s\n", argv[0], 
XDisplayName(display_name));
        exit( -1 );
    }

	/* get screen size from display structure macro */
    screen = DefaultScreen(display);

	bgcolor=getcolor(colors[BACKGROUND]);
	bdcolor=getcolor(BDWINCOLOR);

	/* create opaque window;inherit attributes from root*/
   	base= XCreateSimpleWindow(display, RootWindow(display, screen),
posx, posy, width, height, border_width, bdcolor, bgcolor);

	if(demo){
		demobase=XCreateSimpleWindow(display, RootWindow(display, screen),
posx, posy, demowidth, demoheight, border_width, bdcolor, bgcolor);

	load_font(&font_info);
	}

	/* create separate GC for drawing */
	for(i=0; i<MAXCOLORS; i++) makeGCs(&gc[i], colors[i]);

	/* do special settings*/
    XSetLineAttributes(display, gc[FRAME], framewidth, LineSolid, CapButt, 
JoinMiter);	/* extra thick lines for the frame*/

	if(demo) XSetFont(display, gc[FRAME], font_info->fid);
	XSetFillStyle(display,gc[BEADS],FillSolid);
	XSetFunction(display,gc[BEADS],GXxor);	/* so we don't disturb BG while
												animating beads*/

    /* Create bitmap for icon */
    icon_pixmap= XCreateBitmapFromData(display, base, icon_bits, icon_width,
icon_height);

    if(demo){
    	/* Set resize hints */
		size_hints.flags= PPosition | PMinSize | PMaxSize;
    	size_hints.width= demowidth;	 		
		size_hints.height= demoheight;
    	size_hints.min_width=size_hints.max_width=demowidth;	
		size_hints.min_height=size_hints.max_height=demoheight;
    	size_hints.x= posx+10;			 		size_hints.y= posy+10;

    	/* set Properties for window manager (always before mapping) */
    	XSetStandardProperties(display, demobase, DEMOWINAME, DEMOWINAME, 
			icon_pixmap, argv, argc, &size_hints);

    	/* Select event types wanted */
    	XSelectInput(display, demobase, ExposureMask | KeyPressMask | 
			ButtonPressMask | ButtonReleaseMask);
	}

   	/* Set resize hints re-using size_hints struct for the base-window*/
	size_hints.flags= PPosition | PSize | PMinSize|PMaxSize;
   	size_hints.width=size_hints.min_width=width;	 		
	size_hints.height= height;
	/* the abacus can be made as wide as the screen or MAXCOLS (whichever is
	 * smaller)*/
	size_hints.max_width=DisplayWidth(display, screen);
	/* height is fixed at run-time */
	size_hints.min_height=size_hints.max_height=height;
   	size_hints.x= posx;			 		size_hints.y= posy;

    /* set Properties for window manager (always before mapping) */
    XSetStandardProperties(display, base, window_name, icon_name, 
icon_pixmap, argv, argc, &size_hints);

    /* Select event types wanted */
    XSelectInput(display, base, ExposureMask | KeyPressMask | ButtonPressMask | 
StructureNotifyMask | PointerMotionHintMask | ButtonMotionMask | 
ButtonReleaseMask);

	/* create & map unique cursor to this window*/
	Hand=XCreateFontCursor(display, XC_hand2);
	XDefineCursor(display, base, Hand);

	if (demo) XDefineCursor(display, demobase, Hand);

    XMapWindow(display, base);
    if (demo) XMapWindow(display, demobase);

}/*make Windows*/
/*-----------------------------------------------------------------
 * makeGCs, creates a default GC then sets the color, line &
 *      font attributes; font must have been loaded before this is called
 *
 *      passed: pointer to GC to be set,
 *              colorname to set("red","green"...)
 *      returns: nothing
 *          dep: XGC values, win, display, getcolor()
 */
makeGCs(gc, colorname)
GC *gc;
char *colorname;
{
unsigned long valuemask = 0; /* use defaults*/
XGCValues values;

    *gc = XCreateGC(display, base, valuemask, &values); /* ...create gc*/

    /* ...set color & other GC attributes*/
    XSetForeground(display, *gc, getcolor(colorname));
    XSetBackground(display, *gc, getcolor(colors[BACKGROUND]));
    XSetLineAttributes(display, *gc, 0, LineSolid, CapButt, JoinMiter);

}/* makeGCs*/

/*-------------------------------------------------------------------
 * closeDisplay, de-allocates GC's, fonts & pixmaps, breaks connection with
 *          X server & de-allocates all data, exits application
 *
 *          passed: nothing
 *          returns:doesn't return
 *          dep: globals in maindefs.h
 */
/* ARGSUSED*/
closeDisplay()
{
int i;
    for(i=0; i<MAXCOLORS; i++) XFreeGC(display, gc[i]);

    if (demo) XUnloadFont(display, font_info->fid);

	XFreeCursor(display, Hand);

    XCloseDisplay(display);

	exit(0);
}

/*------------------------------------------------------------------------
 * getcolor, allocates requested color in the default colormap & returns
 *          a RGB pixel-value for use in allocating a GC.
 *          Automatically detects a monochrome display, & assumes
 *          black-on-white thus returning black instead of requested colors
 *          & white when requested.
 *
 *          passed: colorname to allocate
 *          returns: RGB pixel-value encoided in an unsigned long (I'm
 *                  breaking Rule#1 of Xlib programming by accessing
 *                  the individual  structure elements rather than
 *                  using correct types (Pixel is defined for Xt only))
 *              dep: nothing
 */
unsigned long getcolor(colorname)
char *colorname;
{
int depth;              /* of the screen*/
Colormap cmap;          /* for allocating colors*/
XColor generic;         /* hold pixel-values while making different GC's*/
int i;

    depth=DisplayPlanes(display, screen);   /* is it mono or color*/
    cmap=DefaultColormap(display, screen);

    /* mono screen; ignore colorname; default: black on white */
    if(depth==1){
        if(!strcmp(colorname,"white"))      /* user wants white*/
            return(WhitePixel(display, screen));
        else
            return(BlackPixel(display, screen));
    }
    else{

        /* parse the color & get pixel values to set color... */
        if(!XParseColor(display, cmap, colorname, &generic)){
            fprintf(stdout,"%s: Bad colorname:%s \n", APPNAME, colorname);
            return(BlackPixel(display, screen));    /* send back default*/
        }

        /* allocate color in colormap if space is available*/
        if(!XAllocColor(display, cmap, &generic)){
            fprintf(stdout,"Could not allocate %s, colormap full or \
write-immune\n",colorname);
             return(BlackPixel(display, screen));
        }

        /* color is good*/
        return(generic.pixel);
    }
}/* get color*/

load_font(font_info)
XFontStruct **font_info;
{
    /*char *fontname = "-*-times-*-r-*-*-*-180-*";*/
	char *altfontname = "8x13";

    /* Access font */
    if ((*font_info=XLoadQueryFont(display,demofont)) == NULL) {
		(void) fprintf( stderr, "Cannot open %s font\n",demofont);
		(void) fprintf( stderr, "Attempting %s font as alternate\n",
						altfontname);

		if((*font_info=XLoadQueryFont(display,altfontname)) == NULL) {
        	(void) fprintf( stderr, "Cannot open %s alternate font\n",
						altfontname);
			(void) fprintf( stderr, "use the -demofont option to specify a \
font to use\n");
        	exit( -1 );
		}
    }
}

/*-----------------------------------------------------------------
 * report_button, reports which of the 3 mouse button was pressed
 *          This implementation is *technically* incorrect,
 *  I looked-up the type of data the button-event was (unsigned int);
 *  I don't know how else to do it.
 *  To maintain portability integrity the user must never directly
 *  access the data-types! (I just broke this rule)
 *      -LF
 *
 *          passed: the xevent item with the value of button pressed
 *          returns: 1- if left-button pressed
 *                   2- if middle button
 *                   3- if right button
 */
int report_button(butn)
unsigned int butn;
{

    switch(butn){
        case Button1:
            return(1);

        case Button2:
            return(2);

        case Button3:
            return(3);
    }/* switch*/

}/* report button*/

