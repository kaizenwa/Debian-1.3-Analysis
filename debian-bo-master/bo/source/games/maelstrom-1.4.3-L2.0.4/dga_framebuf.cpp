
// The X11 DGA graphics module:
//

#if defined(linux) && defined(USE_DGA)

#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>

#include "dga_framebuf.h"
#include "cursor_sprite.h"

#ifndef True
#define	True	1
#endif
#ifndef False
#define False	0
#endif


DGA_FrameBuf:: DGA_FrameBuf(unsigned short width, unsigned short height,
				int bpp) : FrameBuf(width, height, bpp)
{
	XVisualInfo     vinfo_return;
	int EventBase, ErrorBase;
	XF86VidModeModeInfo **modes;
	XSetWindowAttributes  xswa;
	int i, ram, nmodes = 0;

	/* Open our DISPLAY */
	if ( (display=XOpenDisplay(NULL)) == NULL ) {
		error("X: Can't open display...\n");
		exit(3);
	}

	/* Make sure all is destroyed if killed off */
	XSetCloseDownMode(display, DestroyAll);

	/* We can only handle 8 bit color displays (for now) */
	if (!XMatchVisualInfo(display, DefaultScreen(display), 
					8, PseudoColor, &vinfo_return) ) {
		error("X: Can't do DGA on a truecolor display (yet)\n");
		exit(3);
	}

	/* Allocate our framebuffer memory */
	shared_len = WIDTH*HEIGHT;
	shared_mem = new unsigned char[shared_len];
	backbuf = new unsigned char[shared_len];

	/* Set our X display resolution to width x height */
	zoom = 0;
	if (XF86VidModeQueryExtension(display, &EventBase, &ErrorBase)) {
		if (XF86VidModeGetAllModeLines(display, DefaultScreen(display),
							&nmodes, &modes)) {
			for ( i=0; i<nmodes; ++i ) {
#ifdef DEBUG
				mesg("Mode %d: %dx%d\n", i+1,
					modes[i]->hdisplay, modes[i]->vdisplay);
#endif
				if ( (modes[i]->hdisplay == width) &&
					(modes[i]->vdisplay == height) ) {
					zoom = i;
					break;
				}
			}
			if ( (zoom == 0) && (i == nmodes) ) {
				error(
		"X: Warning: no %dx%d mode defined in your XF86Config file.\n",
								width, height);
			}

			/* if zoom == 0, we are at width-height mode already */
			if ( zoom > 0 ) {
				for ( i=0; i<zoom; ++i ) {
					if (!XF86VidModeSwitchMode(display,
						DefaultScreen(display), 1)) {
						error(
				"X: Warning: couldn't switch video modes\n");
						zoom = i;
						break;
					}
				}
				XFlush(display);
#ifdef DEBUG
				mesg("Switched video modes...\n");
#endif
			}
		} else {
			error("X: Warning: Couldn't get video modes\n");
		}
	} else {
		error("X: Warning: Couldn't set display resolution\n");
	}

	/* See if DGA is supported by the X server */
	if (!XF86DGAQueryExtension(display, &EventBase, &ErrorBase)) {
		error("X: DGA not supported by this X server!\n");
		exit(3);
	}

	/* Create our main window */
	/* override redirect tells the window manger to just ignore us :-) */
	xswa.override_redirect = True;
	xswa.event_mask = ExposureMask | KeyPressMask | KeyReleaseMask |
				PointerMotionMask | ButtonPressMask |
							ButtonReleaseMask;

	/* we want to get everything including colormap */
	TopWin = XCreateWindow(display, DefaultRootWindow(display), 0, 0,
				WidthOfScreen(ScreenOfDisplay(display, 0)),
			        HeightOfScreen(ScreenOfDisplay(display, 0)), 0,
				CopyFromParent, InputOutput, CopyFromParent,
				CWEventMask|CWOverrideRedirect, &xswa);


	XMapWindow(display, TopWin);
	XRaiseWindow(display, TopWin);

	/* We want all the key presses */
	XGrabKeyboard(display, TopWin, True, GrabModeAsync, 
						GrabModeAsync, CurrentTime);

	/* and all the mouse moves */
	XGrabPointer(display, TopWin, True, PointerMotionMask |
		ButtonPressMask | ButtonReleaseMask, GrabModeAsync,
				GrabModeAsync, None,  None, CurrentTime);

	/*
	 * Lets go live -- this is a priviledged operation...
	 */
	XF86DGASetViewPort(display, DefaultScreen(display), 0, 0);
	{ // A ROOT canal. :)
		(void) seteuid(0);
		XF86DGAGetVideo(display, DefaultScreen(display),
					&vbase, &vwidth, &vpage, &ram);
		(void) seteuid(getuid());
	}
	vlines = (vpage/vwidth);
	XF86DGADirectVideo(display, DefaultScreen(display),
		XF86DGADirectGraphics|XF86DGADirectMouse|XF86DGADirectKeyb);
	XF86DGASetVidPage(display, DefaultScreen(display), 0);

	/* Allocate area refresh queue */
	Reefer = new Stack<Area>(ChunkSize);

	/* Set mouse handler */
	mouseSprite.width = *((unsigned short *)cursor_sprite);
	mouseSprite.height = *((unsigned short *)&cursor_sprite[2]);
	mouseSprite.pixels = (unsigned char *)&cursor_sprite[4];
	mouseSprite.mask = (unsigned char *)&cursor_sprite[
				(mouseSprite.width*mouseSprite.height)+4];
	behind_mouse = new unsigned char
				[mouseSprite.width*mouseSprite.height];
#ifdef CENTER_MOUSE
	mouseX = WIDTH/2;
	mouseY = HEIGHT/2;
#else
	mouseX = 0;
	mouseY = 0;
#endif
	mouse_accel = 1.0;
}

DGA_FrameBuf:: ~DGA_FrameBuf()
{
	int i;

	/* Back to normal X11 access */
	XF86DGADirectVideo(display, DefaultScreen(display), 0);
	XUngrabPointer(display, CurrentTime);
	XUngrabKeyboard(display, CurrentTime);
	XUnmapWindow(display, TopWin);
	if ( zoom > 0 ) {
		for ( i=0; i<zoom; ++i ) {
			if (!XF86VidModeSwitchMode(display,
						DefaultScreen(display), -1)) {
				error(
				"X: Warning: couldn't switch video modes\n");
				break;
			}
		}
		XFlush(display);
	}
	delete[] shared_mem;
	delete[] backbuf;
	XCloseDisplay(display);
}

/* Just try to allocate a full colormap */
int
DGA_FrameBuf:: Alloc_Cmap(Color Cmap[NUM_COLORS])
{
	return(Alloc_Private_Cmap(Cmap));
}

/* Allocate a private colormap :) */
int
DGA_FrameBuf:: Alloc_Private_Cmap(Color Cmap[NUM_COLORS])
{
	static int alloct=0;
	int i;
        XColor xcols[NUM_COLORS], white;

	/* The world is black and white... */
	white.red = white.green = white.blue = 0;
	for (i = 0; i < NUM_COLORS; i++) {
		if ( !Cmap[i].red && !Cmap[i].green && !Cmap[i].blue )
			Black = i;
		if ( (white.red < Cmap[i].red) &&
				(white.green < Cmap[i].green) &&
						(white.blue < Cmap[i].blue) ) {
			white.red = Cmap[i].red;
			white.green = Cmap[i].green;
			white.blue = Cmap[i].blue;
			White = i;
		}
	}

	/* Oops, nope, it's color! :-) */
	/* Create a custom visual colormap, if needed */
	if ( ! alloct ) {
		colormap = XCreateColormap(display, TopWin,
				DefaultVisual(display, DefaultScreen(display)),
							AllocAll);
		alloct = 1;
	}

	/* Allocate custom colors... */
	for(i=0;i<NUM_COLORS;i++) {
		Pixel_colors[i] = xcols[i].pixel = i;
		Color_Map[i].red = xcols[i].red   = Cmap[i].red<<8;
		Color_Map[i].green = xcols[i].green = Cmap[i].green<<8;
		Color_Map[i].blue = xcols[i].blue  = Cmap[i].blue<<8;
		xcols[i].flags = (DoRed|DoGreen|DoBlue);
	}
	memset((void *)backbuf, Black, shared_len);
	memset((void *)shared_mem, Black, shared_len);
	XStoreColors(display, colormap, xcols, NUM_COLORS);
	XInstallColormap(display, colormap);
	XSetWindowColormap(display, TopWin, colormap);
	Refresh();
	return(NUM_COLORS);
}

void
DGA_FrameBuf:: Hide_Cursor(void)
{
	if ( hidden_cursor ==  0 ) {
		showmouse = 0;
		EraseMouse();
		hidden_cursor = 1;
	}
}

void
DGA_FrameBuf:: Show_Cursor(void)
{
	if ( hidden_cursor == 1 ) {
		DrawMouse();
		showmouse = 1;
		hidden_cursor = 0;
	}
}

/* This function was adapted from 'xscreensaver', by Jamie Zawinski 

	-- Thanks! :)
*/
void
DGA_FrameBuf:: Fade(int steps)
{
	static int      state = XFADE_IN;
	static XColor   orig_colors[NUM_COLORS];
	XColor          curr_colors[NUM_COLORS];
	static long     orig_truecolors[NUM_COLORS];
	int             i, j;
	const int       true_slowfactor=4;
	static Colormap *fade_cmap, new_cmap;

	/* Find out the state of the fade */
	state = ( (state == XFADE_IN) ? XFADE_OUT : XFADE_IN );

	/* Do we really fade? */
	switch (DoFade) {
		case FADE_FAKE:
			/* Do a pixellated fade */
			Pixel_Fade(state);
			
		case FADE_NONE:
			/* We're done */
			return;

		case FADE_REAL:
			/* Continue... */
			break;
	}

	/* Set the requested pixels */
	for ( i=0; i<NUM_COLORS; ++i )
		orig_colors[i].pixel = i;

	/* Make a copy of the default colormap and use that in fade */
	if ( state == XFADE_OUT ) {
		XQueryColors(display, colormap, orig_colors, NUM_COLORS);
		fade_cmap = &colormap;
	}

	memcpy(curr_colors, orig_colors, NUM_COLORS*sizeof(XColor));
	if ( state == XFADE_OUT ) {
		for ( i=steps-1; i >= 0; i-- ) {
			for (j = 0; j < NUM_COLORS; j++) {
				curr_colors[j].red   = 
					orig_colors[j].red   * i / steps;
				curr_colors[j].green = 
					orig_colors[j].green * i / steps;
				curr_colors[j].blue  = 
					orig_colors[j].blue  * i / steps;
			}
			XStoreColors (display, *fade_cmap, 
						curr_colors, NUM_COLORS);
			select_usleep(5);
			XSync (display, False);
		}
	} else {
		Refresh();
		for ( i=0; i < steps; i++ ) {
			for (j = 0; j < NUM_COLORS; j++) {
				curr_colors[j].red   = 
					orig_colors[j].red   * i / steps;
				curr_colors[j].green = 
					orig_colors[j].green * i / steps;
				curr_colors[j].blue  = 
					orig_colors[j].blue  * i / steps;
			}
			XStoreColors (display, *fade_cmap, 
						curr_colors, NUM_COLORS);
			select_usleep(5);
			XSync (display, False);
		}
	}
	
	if ( state == XFADE_IN ) {
		/* Restore the original colormap */
		XStoreColors (display, colormap, orig_colors, NUM_COLORS);
		XSync(display, False);
	}
}

/* Adapted thankfully from "DeathRoids Source!" by Augusto Roman! */
void
DGA_FrameBuf:: DGA_Blit(int x0, int y0, int width, int height)
{
	char *vidaddr;
	char *memaddr;
	int   curr_bank, next_bank;
	int   i;

	/* Set the graphics offset: */
	memaddr = (char *)shared_mem+((y0*WIDTH)+x0);
	vidaddr = vbase+(((y0%vlines)*vwidth)+x0);
	curr_bank = (y0/vlines);
	next_bank = vlines-(y0%vlines);

	XF86DGASetVidPage(display, DefaultScreen(display), curr_bank);
	for ( i=height; i; --i ) {
		if ( next_bank-- == 0 ) {
			XF86DGASetVidPage(display, DefaultScreen(display),
								++curr_bank);
			vidaddr = vbase+x0;
			next_bank = (vpage/vwidth)-1;
		}
		memcpy(vidaddr, memaddr, width);
		memaddr += WIDTH;
		vidaddr += vwidth;
	}
}
void
DGA_FrameBuf:: DGA_BlitBuf(int x0, int y0, int width, int height, char *memaddr)
{
	char *vidaddr;
	int   curr_bank, next_bank;
	int   i;

	/* Set the graphics offset: */
	vidaddr = vbase+(((y0%vlines)*vwidth)+x0);
	curr_bank = (y0/vlines);
	next_bank = vlines-(y0%vlines);

	XF86DGASetVidPage(display, DefaultScreen(display), curr_bank);
	for ( i=height; i; --i ) {
		if ( next_bank-- == 0 ) {
			XF86DGASetVidPage(display, DefaultScreen(display),
								++curr_bank);
			vidaddr = vbase+x0;
			next_bank = (vpage/vwidth)-1;
		}
		memcpy(vidaddr, memaddr, width);
		memaddr += width;
		vidaddr += vwidth;
	}
}

/* BTW, you _can_ pass negative x0 and y0 values to this function */
void
DGA_FrameBuf:: RefreshArea(int x0, int y0, int width, int height)
{
	Area area;

	/* Do bounds checking */
	if ( y0 < 0 ) {
		if ( (height += y0) <= 0 )
			return;
		y0 = 0;
	}
	if ( (y0 + height) >= HEIGHT ) {
		if ( y0 > HEIGHT )
			return;
		height = (HEIGHT-y0);
	}
	if ( x0 < 0 ) {
		if ( (width += x0) <= 0 )
			return;
		x0 = 0;
	}
	if ( (x0 + width) >= WIDTH ) {
		if ( x0 > WIDTH )
			return;
		width = (WIDTH-x0);
	}

	area.x = x0;
	area.y = y0;
	area.width = width;
	area.height = height;
	Reefer->Add(&area);
}

void
DGA_FrameBuf:: Refresh(void)
{ 
	/* Don't refresh during fake pixel fade */
	if ( faded ) return;

	DGA_Blit(0, 0, WIDTH, HEIGHT);
}

void
DGA_FrameBuf:: FlushEvents(void)
{
	int    numevents;
	XEvent event;

	for ( numevents=XPending(display); numevents > 0; --numevents )
		GetEvent(&event);
}
	
int
DGA_FrameBuf:: NumEvents(void)
{
	return(XPending(display));
}

void
DGA_FrameBuf:: GetEvent(XEvent *event)
{
	Flush(0);
	XNextEvent(display, event);

	/* Handle some events internally */
	switch (event->type) {
		case MotionNotify:
			MoveMouse(event->xmotion.x_root, event->xmotion.y_root,
									event);
			break;
		case ButtonPress:
			event->xbutton.x = event->xbutton.x_root = (int)mouseX;
			event->xbutton.y = event->xbutton.y_root = (int)mouseY;
			break;
		case ButtonRelease:
			event->xbutton.x = event->xbutton.x_root = (int)mouseX;
			event->xbutton.y = event->xbutton.y_root = (int)mouseY;
			break;
		default:
			break;
	}
}

int
DGA_FrameBuf:: KeyToAscii(XEvent *event, char *buf, int buflen, KeySym *key)
{
	return(XLookupString(&event->xkey, buf, buflen, key, NULL));
}

/* Sorting routine for refresh area stack */
int sort_areas(DGA_FrameBuf::Area *item1, DGA_FrameBuf::Area *item2) {
	return(item1->y - item2->y);
}

void
DGA_FrameBuf:: Flush(int sync)
{
	int nreefs;
	Area *area;

	Reefer->Sort(sort_areas);
	for ( nreefs = Reefer->Size(); nreefs; --nreefs ) {
		area = Reefer->Pop();	// This should never be NULL
		DGA_Blit(area->x, area->y, area->width, area->height);
	}
	if ( showmouse )
		DrawMouse();
	Unused(sync);
}

#endif /* Linux and use DGA */
