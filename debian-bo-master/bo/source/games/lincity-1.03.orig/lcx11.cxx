// lcx11.cxx    X11 stuff - part of lin-city
// Copyright (c) I J Peters 1995,1996.  Please read the file 'COPYRIGHT'.

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#ifdef IRIX
#include <strings.h>
#endif
// I'm told the following is needed.
#ifdef SUNOS
#include <string.h>
#endif

// this is for OS/2 - RVI
#ifdef __EMX__
#include <float.h>
#endif

//#ifndef SUNOS
//#ifndef HP_UX
//#ifdef __LINUX__
//#include <getopt.h>
//#endif
//#endif
//#endif
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <X11/keysymdef.h>
#include "lin-city.h"
#include "lctypes.h"
#include "lcx11.h"

void setcustompalette(void)
{
	char s[100];
	int n,r,g,b,i,flag[256];
	XColor pal[256];
	FILE *inf;
	for (i=0;i<256;i++)
		flag[i]=0;
        if ((inf=fopen(colour_pal_file,"r"))==0)
                HandleError("Can't find the colour pallet file",FATAL);

        while (feof(inf)==0)
        {
                fgets(s,99,inf);
                if (sscanf(s,"%d %d %d %d",&n,&r,&g,&b)==4)
                {
                        pal[n].red=r;
                        pal[n].green=g;
                        pal[n].blue=b;
                        pal[n].flags= DoRed | DoGreen | DoBlue;
			pal[n].pixel=colour_table[n]; // ???
			flag[n]=1;
                }
        }
        fclose (inf);
        for (i=0;i<256;i++)
        {
                if (flag[i]==0)
                {
                        printf("Colour %d not loaded\n",i);
                        HandleError("Can't continue",FATAL);
                }
                pal[i].red=(unsigned char)((pal[i].red
                *(1-gamma_correct_red))+(64*sin((float)pal[i].red
                *M_PI/128))*gamma_correct_red);

                pal[i].green=(unsigned char)((pal[i].green
                *(1-gamma_correct_green))+(64*sin((float)pal[i].green
                *M_PI/128))*gamma_correct_green);

                pal[i].blue=(unsigned char)((pal[i].blue
                *(1-gamma_correct_blue))+(64*sin((float)pal[i].blue
                *M_PI/128))*gamma_correct_blue);
        }

	do_setcustompalette(pal);
}

void open_setcustompalette(XColor *inpal)
{
	do_setcustompalette(inpal);
}

void do_setcustompalette(XColor *inpal)
{
//        char s[100];
        int i,n,me=0,flag[256],vid;
	long unsigned int plane_masks[3];
	XColor pal[256];
printf("Got to do_setcustompalette()\n");
	display.cmap=XDefaultColormap(display.dpy,display.screen);
	if (XAllocColorCells(display.dpy,display.cmap,0
		,plane_masks,0,colour_table,256)==0)
	{
		me=(*DefaultVisual(display.dpy,display.screen)).map_entries;
		vid=(*DefaultVisual(display.dpy,display.screen)).visualid;
        	display.cmap=XCreateColormap(display.dpy,display.win  
                	,DefaultVisual(display.dpy,display.screen)
		/*	,PseudoColor */
			,AllocNone);
//	XSetWindowColormap(display.dpy,display.win,display.cmap); // 1
printf("map entries=%d\n",me);
	        if (me==256) 
		{
			if (XAllocColorCells(display.dpy,display.cmap,0
                		,plane_masks,0,colour_table,256)!=0)
				printf("Allocated 256 cells\n");
			else
				printf("Couldn't allocate 256 cells\n");
		}
		else
			for (i=0;i<256;i++)
				colour_table[i]=i;
	}
        if (!display.cmap)
                HandleError("No default colour map",FATAL);
//printf("Got past create colourmap\n");

        for (i=0;i<256;i++)
                flag[i]=0;

	for (n=0;n<256;n++)
	{
                pal[n].red=inpal[n].red << 10;
                pal[n].green=inpal[n].green << 10;
                pal[n].blue=inpal[n].blue << 10;
                pal[n].flags= DoRed | DoGreen | DoBlue;
		if (me==256)
			pal[n].pixel=colour_table[n];
		else
		{
			if (XAllocColor(display.dpy
				,display.cmap,&(pal[n]))==0)
				HandleError("alloc colour failed"
					,FATAL);
			colour_table[n]=pal[n].pixel;
			XSetForeground(display.dpy
				,display.pixcolour_gc[n]
				,colour_table[n]);
		}
                flag[n]=1;
        }
//        for (i=0;i<256;i++)
//               if (flag[i]==0)
//                {
//                        printf("Colour %d not loaded\n",i);
//                        do_error("Can't continue");
//                }
//	if (me==256)
//	        XStoreColors(display.dpy,display.cmap,pal,256);
//        XSetWindowColormap(display.dpy,display.win,display.cmap);
	if (me==256)
	{
		XStoreColors(display.dpy,display.cmap,pal,256);
		XFlush(display.dpy);
	}
//	else
		XSetWindowColormap(display.dpy,display.win,display.cmap);
}

void initfont()
{
	int i;
	FILE *finf;
	if ((finf=fopen(fontfile,"r"))==0)
		HandleError("Can't open the font file",FATAL);
	for (i=0;i<256*8;i++)
		myfont[i]=fgetc(finf);
	fclose(finf);
}

void Fgl_setfontcolors(int bg,int fg)
{
	text_fg=fg;
	text_bg=bg;
}


void parse_xargs(int argc, char **argv, char **geometry)
{
	int         option;
//	extern int  optind;
	extern char *optarg;

//	int bits,i;
#ifdef ALLOW_PIX_DOUBLING
printf("Options include Pix doubling\n");
	while ((option = getopt(argc,argv,"vbndg:wR:G:B:")) != EOF)
#else
	while ((option = getopt(argc,argv,"vbng:wR:G:B:")) != EOF)
#endif
	{
        	switch (option)
        	{
            		case 'v':
                		verbose=TRUE;
                		break;
            		case 'g':
                		*geometry=optarg;
                		break;
#ifdef ALLOW_PIX_DOUBLING
			case 'd' :
				pix_double=1;
// and let it run through; we are not allowed a border with pix doubling
#endif
			case 'b' :
				borderx=0;
				bordery=0;
				break;
			case 'n' :
				no_init_help=TRUE;
				break;
                        case 'w':
                                gamma_correct_red=GAMMA_CORRECT_RED;
                                gamma_correct_green=GAMMA_CORRECT_GREEN;
                                gamma_correct_blue=GAMMA_CORRECT_BLUE;
                                break;
                        case 'R':
                                sscanf(optarg,"%f",&gamma_correct_red);
                                break;
                        case 'G':
                                sscanf(optarg,"%f",&gamma_correct_green);
                                break;
                        case 'B':
                                sscanf(optarg,"%f",&gamma_correct_blue);
                                break;


		}
	}
	if (verbose)
		printf("Version %d\n",VERSION);
	if (!(display.dpy = XOpenDisplay(display.dname)))
	{
printf(" Can't open the dispay!\n");
		HandleError("Cannot open display.\n",FATAL);
		exit(-1);
	}
/* Record the screen number and root window. */
	display.screen = DefaultScreen(display.dpy);
	display.root = RootWindow(display.dpy, display.screen);

        winW = WINWIDTH+borderx*2+pix_double*WINWIDTH;
        winH = WINHEIGHT+bordery*2+pix_double*WINHEIGHT;
        winX = (DisplayWidth(display.dpy, display.screen)
                - winW) / 2;
        winY = (DisplayHeight(display.dpy, display.screen)
                - winH) / 2;
        if (*geometry != NULL) 
            XParseGeometry(*geometry, &winX, &winY, &winW, &winH);
}


void Create_Window(char *geometry)
{
	short q;
	Visual *vid;
	XSetWindowAttributes xswa;
	XSizeHints          sizehint;
	XWMHints            wmhints;
	int depth;
//	long unsigned int plane_masks[3];
	unsigned char       wname[256];     /* Window Name */
	unsigned long vmask=CWEventMask | CWBackPixel | CWBackingStore;
//	XVisualInfo vinfo;

	depth=DefaultDepth(display.dpy, display.screen);
        xswa.event_mask = 0;
        xswa.background_pixel = display.bg;
	xswa.backing_store=WhenMapped;
printf("DefaultVisual id=%d bp-rgb=%d map-entries=%d\n"
 ,(int)(*DefaultVisual(display.dpy,display.screen)).visualid
 ,(*DefaultVisual(display.dpy,display.screen)).bits_per_rgb
 ,(*DefaultVisual(display.dpy,display.screen)).map_entries);
//	if (XMatchVisualInfo(display.dpy
//		,display.screen,depth,
//		TrueColor
//		/* DirectColor */
//		/* PseudoColor */
//		,&vinfo))
//	{
//		vid=vinfo.visual;
//		display.cmap=xswa.colormap=XCreateColormap(display.dpy
//			,display.root,vid, AllocNone ); // 1
//		printf("Got a TrueColor visual.\n");
//               if (XAllocColorCells(display.dpy,display.cmap,0
//                      ,plane_masks,0,colour_table,256)!=0)
//		        printf("Allocated 256 cells\n");
//		else
//		{
//			printf("... but couldn't allocate colours.\n");
//			printf("Using default visual\n");
//			vid=DefaultVisual(display.dpy, display.screen);
//		}
//		if (vid!=DefaultVisual(display.dpy, display.screen))
//			vmask|=CWColormap;
//	}
//	else
//	{
		vid=DefaultVisual(display.dpy, display.screen);
		display.cmap
			=XDefaultColormap(display.dpy,display.screen);
		printf("Using default visual\n");
//	}
        display.win = XCreateWindow(display.dpy, display.root, 
            winX, winY, 
            winW, winH, 0, 
//            DefaultDepth(display.dpy, display.screen), 
		depth,
            InputOutput,  /* vid , */
          DefaultVisual(display.dpy, display.screen), 
	/*	PseudoColor,  */
	vmask, &xswa);

        sizehint.x = winX-100;
        sizehint.y = winY;
        sizehint.width = winW;
        sizehint.height = winH;
        sizehint.min_width = winW;
        sizehint.min_height = winH;
	sizehint.max_width = winW;
	sizehint.max_height = winH;
        if (geometry != NULL)
printf("Geometry is null\n"),
            sizehint.flags = USPosition | USSize | PMinSize | PMaxSize;
        else
printf("Geometry is set\n"),
            sizehint.flags = PPosition | PSize | PMinSize | PMaxSize;
         XSetNormalHints(display.dpy, display.win, &sizehint);
            
        display.protocol_atom = XInternAtom(display.dpy, "WM_PROTOCOLS",
False);
        display.kill_atom = XInternAtom(display.dpy, "WM_DELETE_WINDOW",
False);

        /* Title */
        sprintf( (char *) wname, 
"xlin-city, Version %d.%02d, (Copyright) IJ Peters - copying policy GNU GPL",
            VERSION/100,VERSION%100);
        XChangeProperty(display.dpy, display.win,
            XA_WM_NAME, XA_STRING, 8, PropModeReplace, wname, 
            strlen((char *)wname));

        /* Window Manager Hints (This is supposed to make input work.) */
        wmhints.flags = InputHint;
        wmhints.input = True;
        XSetWMHints(display.dpy, display.win, &wmhints);
       XMapWindow(display.dpy, display.win);
        XSelectInput(display.dpy, display.win,
                 KeyPressMask | ButtonPressMask | ButtonReleaseMask 
		| ExposureMask | StructureNotifyMask);
	for (q=0;q<256;q++)
	{
		display.pixcolour_gc[q]=XCreateGC(display.dpy
			,display.win,0,NULL);
		XSetForeground(display.dpy,display.pixcolour_gc[q],q);
		XSetBackground(display.dpy,display.pixcolour_gc[q]
			,display.bg);
		XSetGraphicsExposures(display.dpy,display.pixcolour_gc[q]
			,False);
	}
}


void HandleError(char *description, int degree)
{
    fprintf(stderr
	,"An error has occurred.  The description is below...\n");
    fprintf(stderr, "%s\n", description);

    if (degree == FATAL)
    {
        fprintf(stderr, "Program aborting...\n");
        exit(-1);
    }
}

void Fgl_setpixel(int x,int y,int col)
{
        int i;
	if (clipping_flag)
		if (x<xclip_x1 || x>xclip_x2 || y<xclip_y1 || y>xclip_y2)
			return; 
	col&=0xff;
//printf("In setpixel\n");
#ifdef ALLOW_PIX_DOUBLING
	if (pix_double)
	{
		i=y*(640+BORDERX)+x;
		if ((int)*(pixmap+i)!=col)
		{
			*(pixmap+i)=(unsigned char)col;
			XFillRectangle(display.dpy,display.win
				,display.pixcolour_gc[col],x*2,y*2,2,2);
		}
	}
	else
	{
#endif
        i=(y+bordery)*(640+BORDERX)+x+borderx;
        if ((int)*(pixmap+i)!=col)
        {
                *(pixmap+i)=(unsigned char)col;
                XDrawPoint(display.dpy,display.win
                        ,display.pixcolour_gc[col],x+borderx,y+bordery);
        }
#ifdef ALLOW_PIX_DOUBLING
	}
#endif
}

int Fgl_getpixel(int x,int y)
{
        return(*(pixmap+(y+bordery)*(640+BORDERX)+x+borderx));
}

void Fgl_hline(int x1,int y1,int x2,int col)
{
        int x,i;
//	if (clipping_flag)
//	{
//		if (x1<xclip_x1)
//			x1=xclip_x1;
//		else if (x1>xclip_x2)
//			x1=xclip_x2;
//		if (y1<xclip_y1)
//			return;
//		else if (y1>xclip_y2)
//			return;
//               if (x2<xclip_x1)
//                        x2=xclip_x1;
//                else if (x2>xclip_x2)
//                        x2=xclip_x2;
//	}

//printf("In hline %d %d %d %d\n",x1,y1,x2,col);
	col&=0xff;
        i=(y1+bordery)*(640+BORDERX);
        for (x=x1+borderx;x<=x2+borderx;x++)
                *(pixmap+i+x)=col;
#ifdef ALLOW_PIX_DOUBLING
	if (pix_double)
		XFillRectangle(display.dpy,display.win
			,display.pixcolour_gc[col],x1*2,y1*2
			,(x2-x1)*2+1,2);
	else
#endif
        XDrawLine(display.dpy,display.win
                ,display.pixcolour_gc[col],x1+borderx
			,y1+bordery,x2+borderx,y1+bordery);
}

void Fgl_line(int x1,int y1,int dummy,int y2,int col)
// vertical lines only.
{
        int y;
//printf("In (v)line %d %d %d %d\n",x1,y1,y2,col);
//        if (clipping_flag)
//        {
//                if (x1<xclip_x1)
//                        x1=xclip_x1;
//                else if (x1>xclip_x2)
//                        x1=xclip_x2;
//                if (y1<xclip_y1)
//                        y1=xclip_y1;
//                else if (y1>xclip_y2)
//                        y1=xclip_y2;
//                if (y2<xclip_y1)
//                        y2=xclip_y1;
//                else if (y2>xclip_y2)
//                        y2=xclip_y2;
//	}

	col&=0xff;
        for (y=y1+bordery;y<=y2+bordery;y++)
                *(pixmap+x1+borderx+(y*(640+BORDERX)))=col;
//printf("In line - got set up pixmap\n");
#ifdef ALLOW_PIX_DOUBLING
	if (pix_double)
		XFillRectangle(display.dpy,display.win
			,display.pixcolour_gc[col],x1*2,y1*2
			,2,(y2-y1)*2+1);
	else
#endif
        XDrawLine(display.dpy,display.win
                ,display.pixcolour_gc[col],x1+borderx
			,y1+bordery,x1+borderx,y2+bordery);
}

void Fgl_write(int x,int y, char *s)
{
	int i;
//printf("writing <%s>\n",s);
	for (i=0;i<(int)(strlen(s));i++)
		my_x_putchar(x+i*8,y,s[i]);
}

void open_write(int x,int y, char *s)
{
        int i;
//printf("writing <%s>\n",s);
        for (i=0;i<(int)(strlen(s));i++)
                open_x_putchar(x+i*8,y,s[i]);
}

void my_x_putchar(int xx,int yy,int c)
{
	int x,y,b;
//printf(" %c",c);
	for (y=0;y<8;y++)
	{
		b=myfont[c*8+y];
		for (x=0;x<8;x++)
		{
			if ((b & 0x80)==0)
				Fgl_setpixel(xx+x,yy+y,text_bg);
			else
				Fgl_setpixel(xx+x,yy+y,text_fg);
			b=b<<1;
		}
	}
}

void open_x_putchar(int xx,int yy,int c)
{                                          
        int x,y,b;                         
//printf(" %c",c);
        for (y=0;y<open_font_height;y++)                  
        {                                  
                b=open_font[c*open_font_height+y];           
                for (x=0;x<8;x++)          
                {                          
                        if ((b & 0x80)==0) 
                                Fgl_setpixel(xx+x,yy+y,text_bg);
                        else               
                                Fgl_setpixel(xx+x,yy+y,text_fg);
                        b=b<<1;            
                }                          
        }                                  
}

void Fgl_fillbox(int x1,int y1,int w,int h,int col)
{
	int x,y;
	if (clipping_flag)
	{
		if (x1<xclip_x1)
			x1=xclip_x1;
		if (x1+w>xclip_x2)
			w=xclip_x2-x1;
		if (y1<xclip_y1)
			y1=xclip_y1;
		if (y1+h>xclip_y2)
			h=xclip_y2-y1;
	}
	col&=0xff;
//printf("In fillbox %d %d %d %d %d\n",x1,y1,w,h,col);
//	if ((y1+h)>480)
//	{
//		printf("Bad y1+h in fillbox val=%d\n",y1+h);
//		h=y1-480;
//	}
	for (y=y1+bordery;y<y1+h+bordery;y++)
	{
		for (x=x1+borderx;x<x1+w+borderx;x++)
			*(pixmap+y*(640+BORDERX)+x)=col;
//		XDrawLine(display.dpy,display.win,display.pixcolour_gc[col]
//		,x1+borderx,y,x1+w-1+borderx,y);
	}
#ifdef ALLOW_PIX_DOUBLING
	if (pix_double)
		XFillRectangle(display.dpy,display.win
			,display.pixcolour_gc[col],x1*2,y1*2,w*2,h*2);
	else
#endif
	XFillRectangle(display.dpy,display.win
		,display.pixcolour_gc[col],x1+borderx,y1+bordery,w,h);

//printf("Done\n");
}

void Fgl_putbox(int x1,int y1,int w,int h,void *buf)
{
	unsigned char *b;
	b=(unsigned char *)buf;
	int x,y;
//printf("In Fgl_putbox(%d %d %d %d %p\n",x1,y1,w,h,buf);
	for (y=y1;y<y1+h;y++)
		for (x=x1;x<x1+w;x++)
		{
//printf(" %d ",*b);
			Fgl_setpixel(x,y,*(b++));
		}
}

void Fgl_getbox(int x1,int y1,int w,int h,void *buf)
{
        unsigned char *b;
        b=(unsigned char *)buf;
        int x,y;
        for (y=y1;y<y1+h;y++)
                for (x=x1;x<x1+w;x++)
                        *(b++)=(unsigned char)Fgl_getpixel(x,y);
}

void HandleEvent(XEvent *event)
{
//printf("XEvent - type=%d\n",event->type);
	switch (event->type)
	{
		case (KeyPress):
		{
			XKeyEvent *key_event = (XKeyEvent *) event;
                	char buf[128];
                	KeySym ks;
                	XComposeStatus status;
			XLookupString(key_event,buf,128,&ks,&status);
//			if (buf[0]=='q' || buf[0]=='Q')
//				stopflag=TRUE;
//			else if (buf[0]=='r' || buf[0]=='R')
//				refresh_screen(0,0,640,480);
			x_key_value=buf[0];
//printf("Key=%d\n",x_key_value);
			if (x_key_value==XK_Return)
				x_key_value=0xd;
			else if (x_key_value==XK_BackSpace
				|| x_key_value==XK_Delete || x_key_value==8)
				x_key_value=127;
		} break;

		case (ButtonPress):
		{
			XButtonEvent *ev = (XButtonEvent *) event;
			mouse_button=ev->button;
			if ((ev->state & ShiftMask)!=0)
				cs_mouse_shifted=1;
			else
				cs_mouse_shifted=0;
//printf("Button %d pressed\n",mouse_button);
//printf("Button state = %d\n",cs_mouse_shifted);
//			do_buttons(0,0);
			cs_mouse_handler(mouse_button,0,0);
		} break;

		case (ButtonRelease):
		{
//printf("Button released\n");
			mouse_button=0;
			cs_mouse_handler(mouse_button,0,0);
		} break;

		case (Expose):
		{
			if (suppress_next_expose)
			{
// printf("Expose event supressed\n");
				suppress_next_expose=0;
				break;
			}
			XExposeEvent *ev = (XExposeEvent *) event;
printf("Expose event\n");
// printf("Expose event %d %d %d %d\n",ev->x,ev->y,ev->x+ev->width
// ,ev->y+ev->height);
			refresh_screen(ev->x,ev->y
				,ev->x+ev->width,ev->y+ev->height);
		} break;

//		default: printf("Unknown event %d\n",event->type);
	}
}

void refresh_screen(int x1,int y1,int x2,int y2)  // bounds of refresh area
{
	int x,y;
//printf("In refresh %d %d %d %d\n",x1,y1,x2,y2);
	for (y=y1;y<y2;y++)
		for (x=x1;x<x2;x++)
                XDrawPoint(display.dpy,display.win
                        ,display.pixcolour_gc[*(pixmap
				+x+y*(640+BORDERX))&0xff],x,y);
//			Fgl_setpixel(x,y,*(pixmap+x+y*(640+BORDERX)));
//printf("Got past draws\n");
}

void Fgl_enableclipping(void)
{
	clipping_flag=1;
}

void Fgl_setclippingwindow(int x1,int y1,int x2,int y2)
{
	xclip_x1=x1;
	xclip_y1=y1;
	xclip_x2=x2;
	xclip_y2=y2;
}

void Fgl_disableclipping(void)
{
	clipping_flag=0;
}

void do_call_event(int wait)
{
        int dummy_int,x,y;
        Window dummy_win;
//printf("In call event\n");
	XEvent xev;
//	x_key_value=0;
        if (XPending(display.dpy) )  // || wait!=0)
        {
                XNextEvent(display.dpy, &xev);
                HandleEvent(&xev);
        }
	else if (wait)
		lc_usleep(1000);

        XQueryPointer(display.dpy,display.win,&dummy_win,&dummy_win
                ,&dummy_int,&dummy_int,&x,&y,(unsigned int *) &dummy_int);
#ifdef ALLOW_PIX_DOUBLING
	if (pix_double)
	{
		x/=2;
		y/=2;
	}
	else
	{
#endif
	x-=borderx;
	y-=bordery;
#ifdef ALLOW_PIX_DOUBLING
	}
#endif
	if (x!=cs_mouse_x || y!=cs_mouse_y)
		cs_mouse_handler(mouse_button,x-cs_mouse_x,y-cs_mouse_y);
}

void call_event(void)
{
        do_call_event(0);
}

void call_wait_event(void)
{
	do_call_event(1);
}

#ifdef USE_PIXMAPS

void init_pixmaps()
{
        init_icon_pixmap(CST_GREEN);
// powerlines
	init_icon_pixmap(CST_POWERL_H_L);
	init_icon_pixmap(CST_POWERL_V_L);
	init_icon_pixmap(CST_POWERL_LD_L);
	init_icon_pixmap(CST_POWERL_RD_L);
	init_icon_pixmap(CST_POWERL_LU_L);
	init_icon_pixmap(CST_POWERL_RU_L);
	init_icon_pixmap(CST_POWERL_LDU_L);
	init_icon_pixmap(CST_POWERL_LDR_L);
	init_icon_pixmap(CST_POWERL_LUR_L);
	init_icon_pixmap(CST_POWERL_UDR_L);
	init_icon_pixmap(CST_POWERL_LUDR_L);
	prog_box("",6);
	init_icon_pixmap(CST_POWERL_H_D);
	init_icon_pixmap(CST_POWERL_V_D);
	init_icon_pixmap(CST_POWERL_LD_D);
	init_icon_pixmap(CST_POWERL_RD_D);
	init_icon_pixmap(CST_POWERL_LU_D);
	init_icon_pixmap(CST_POWERL_RU_D);
	init_icon_pixmap(CST_POWERL_LDU_D);
	init_icon_pixmap(CST_POWERL_LDR_D);
	init_icon_pixmap(CST_POWERL_LUR_D);
	init_icon_pixmap(CST_POWERL_UDR_D);
	init_icon_pixmap(CST_POWERL_LUDR_D);
// HT windmills
	init_icon_pixmap(CST_WINDMILL_1_G);
	init_icon_pixmap(CST_WINDMILL_2_G);
	init_icon_pixmap(CST_WINDMILL_3_G);
	prog_box("",12);
	init_icon_pixmap(CST_WINDMILL_1_RG);
	init_icon_pixmap(CST_WINDMILL_2_RG);
	init_icon_pixmap(CST_WINDMILL_3_RG);
	init_icon_pixmap(CST_WINDMILL_1_R);
	init_icon_pixmap(CST_WINDMILL_2_R);
	init_icon_pixmap(CST_WINDMILL_3_R);
//LT windmills
        init_icon_pixmap(CST_WINDMILL_1_W);
        init_icon_pixmap(CST_WINDMILL_2_W);
        init_icon_pixmap(CST_WINDMILL_3_W);
// communes
        init_icon_pixmap(CST_COMMUNE_1);
        init_icon_pixmap(CST_COMMUNE_2);
        init_icon_pixmap(CST_COMMUNE_3);
        init_icon_pixmap(CST_COMMUNE_4);
        init_icon_pixmap(CST_COMMUNE_5);
	prog_box("",18);
        init_icon_pixmap(CST_COMMUNE_6);
        init_icon_pixmap(CST_COMMUNE_7);
        init_icon_pixmap(CST_COMMUNE_8);
        init_icon_pixmap(CST_COMMUNE_9);
        init_icon_pixmap(CST_COMMUNE_10);
        init_icon_pixmap(CST_COMMUNE_11);
        init_icon_pixmap(CST_COMMUNE_12);
        init_icon_pixmap(CST_COMMUNE_13);
        init_icon_pixmap(CST_COMMUNE_14);
// farms   (3 7 11 and 15 are the only ones needed)
        init_icon_pixmap(CST_FARM_O3);
        init_icon_pixmap(CST_FARM_O7);
        init_icon_pixmap(CST_FARM_O11);
        init_icon_pixmap(CST_FARM_O15);
// Lt. Industry
	prog_box("",24);
        init_icon_pixmap(CST_INDUSTRY_L_C);
        init_icon_pixmap(CST_INDUSTRY_L_Q1);
        init_icon_pixmap(CST_INDUSTRY_L_Q2);
        init_icon_pixmap(CST_INDUSTRY_L_Q3);
        init_icon_pixmap(CST_INDUSTRY_L_Q4);
        init_icon_pixmap(CST_INDUSTRY_L_L1);
        init_icon_pixmap(CST_INDUSTRY_L_L2);
        init_icon_pixmap(CST_INDUSTRY_L_L3);
        init_icon_pixmap(CST_INDUSTRY_L_L4);
        init_icon_pixmap(CST_INDUSTRY_L_M1);
        init_icon_pixmap(CST_INDUSTRY_L_M2);
        init_icon_pixmap(CST_INDUSTRY_L_M3);
        init_icon_pixmap(CST_INDUSTRY_L_M4);
        init_icon_pixmap(CST_INDUSTRY_L_H1);
        init_icon_pixmap(CST_INDUSTRY_L_H2);
	prog_box("",30);
        init_icon_pixmap(CST_INDUSTRY_L_H3);
        init_icon_pixmap(CST_INDUSTRY_L_H4);
// Hv. Industry
        init_icon_pixmap(CST_INDUSTRY_H_C);
        init_icon_pixmap(CST_INDUSTRY_H_L1);
        init_icon_pixmap(CST_INDUSTRY_H_L2);
        init_icon_pixmap(CST_INDUSTRY_H_L3);
        init_icon_pixmap(CST_INDUSTRY_H_L4);
        init_icon_pixmap(CST_INDUSTRY_H_L5);
        init_icon_pixmap(CST_INDUSTRY_H_L6);
        init_icon_pixmap(CST_INDUSTRY_H_L7);
        init_icon_pixmap(CST_INDUSTRY_H_L8);
        init_icon_pixmap(CST_INDUSTRY_H_M1);
        init_icon_pixmap(CST_INDUSTRY_H_M2);
        init_icon_pixmap(CST_INDUSTRY_H_M3);
        init_icon_pixmap(CST_INDUSTRY_H_M4);
	prog_box("",36);
        init_icon_pixmap(CST_INDUSTRY_H_M5);
        init_icon_pixmap(CST_INDUSTRY_H_M6);
        init_icon_pixmap(CST_INDUSTRY_H_M7);
        init_icon_pixmap(CST_INDUSTRY_H_M8);
        init_icon_pixmap(CST_INDUSTRY_H_H1);
        init_icon_pixmap(CST_INDUSTRY_H_H2);
        init_icon_pixmap(CST_INDUSTRY_H_H3);
        init_icon_pixmap(CST_INDUSTRY_H_H4);
        init_icon_pixmap(CST_INDUSTRY_H_H5);
        init_icon_pixmap(CST_INDUSTRY_H_H6);
        init_icon_pixmap(CST_INDUSTRY_H_H7);
        init_icon_pixmap(CST_INDUSTRY_H_H8);
// water
	prog_box("",42);
        init_icon_pixmap(CST_WATER);
        init_icon_pixmap(CST_WATER_D);
        init_icon_pixmap(CST_WATER_R);
        init_icon_pixmap(CST_WATER_U);
        init_icon_pixmap(CST_WATER_L);
        init_icon_pixmap(CST_WATER_LR);
        init_icon_pixmap(CST_WATER_UD);
        init_icon_pixmap(CST_WATER_LD);
        init_icon_pixmap(CST_WATER_RD);
        init_icon_pixmap(CST_WATER_LU);
        init_icon_pixmap(CST_WATER_UR);
        init_icon_pixmap(CST_WATER_LUD);
        init_icon_pixmap(CST_WATER_LRD);
        init_icon_pixmap(CST_WATER_LUR);
        init_icon_pixmap(CST_WATER_URD);
        init_icon_pixmap(CST_WATER_LURD);
// tracks
        init_icon_pixmap(CST_TRACK_LR);
	prog_box("",48);
        init_icon_pixmap(CST_TRACK_LU);
        init_icon_pixmap(CST_TRACK_LD);
        init_icon_pixmap(CST_TRACK_UD);
        init_icon_pixmap(CST_TRACK_UR);
        init_icon_pixmap(CST_TRACK_DR);
        init_icon_pixmap(CST_TRACK_LUR);
        init_icon_pixmap(CST_TRACK_LDR);
        init_icon_pixmap(CST_TRACK_LUD);
        init_icon_pixmap(CST_TRACK_UDR);
        init_icon_pixmap(CST_TRACK_LUDR);
// roads
        init_icon_pixmap(CST_ROAD_LR);
        init_icon_pixmap(CST_ROAD_LU);
        init_icon_pixmap(CST_ROAD_LD);
        init_icon_pixmap(CST_ROAD_UD);
        init_icon_pixmap(CST_ROAD_UR);
        init_icon_pixmap(CST_ROAD_DR);
        init_icon_pixmap(CST_ROAD_LUR);
        init_icon_pixmap(CST_ROAD_LDR);
	prog_box("",54);
        init_icon_pixmap(CST_ROAD_LUD);
        init_icon_pixmap(CST_ROAD_UDR);
        init_icon_pixmap(CST_ROAD_LUDR);
// rail
        init_icon_pixmap(CST_RAIL_LR);
        init_icon_pixmap(CST_RAIL_LU);
        init_icon_pixmap(CST_RAIL_LD);
        init_icon_pixmap(CST_RAIL_UD);
        init_icon_pixmap(CST_RAIL_UR);
        init_icon_pixmap(CST_RAIL_DR);
        init_icon_pixmap(CST_RAIL_LUR);
        init_icon_pixmap(CST_RAIL_LDR);
        init_icon_pixmap(CST_RAIL_LUD);
        init_icon_pixmap(CST_RAIL_UDR);
        init_icon_pixmap(CST_RAIL_LUDR);
// potteries
	prog_box("",60);
        init_icon_pixmap(CST_POTTERY_0);
        init_icon_pixmap(CST_POTTERY_1);
        init_icon_pixmap(CST_POTTERY_2);
        init_icon_pixmap(CST_POTTERY_3);
        init_icon_pixmap(CST_POTTERY_4);
        init_icon_pixmap(CST_POTTERY_5);
        init_icon_pixmap(CST_POTTERY_6);
        init_icon_pixmap(CST_POTTERY_7);
        init_icon_pixmap(CST_POTTERY_8);
        init_icon_pixmap(CST_POTTERY_9);
        init_icon_pixmap(CST_POTTERY_10);
// mills
        init_icon_pixmap(CST_MILL_0);
        init_icon_pixmap(CST_MILL_1);
        init_icon_pixmap(CST_MILL_2);
        init_icon_pixmap(CST_MILL_3);
        init_icon_pixmap(CST_MILL_4);
	prog_box("",66);
        init_icon_pixmap(CST_MILL_5);
        init_icon_pixmap(CST_MILL_6);
// blacksmiths
        init_icon_pixmap(CST_BLACKSMITH_0);
	init_icon_pixmap(CST_BLACKSMITH_1);
	init_icon_pixmap(CST_BLACKSMITH_2);
	init_icon_pixmap(CST_BLACKSMITH_3);
	init_icon_pixmap(CST_BLACKSMITH_4);
	init_icon_pixmap(CST_BLACKSMITH_5);
	init_icon_pixmap(CST_BLACKSMITH_6);
// residences
        init_icon_pixmap(CST_RESIDENCE_LL);
        init_icon_pixmap(CST_RESIDENCE_ML);
        init_icon_pixmap(CST_RESIDENCE_HL);
        init_icon_pixmap(CST_RESIDENCE_LH);
        init_icon_pixmap(CST_RESIDENCE_MH);
        init_icon_pixmap(CST_RESIDENCE_HH);
// coal power
        init_icon_pixmap(CST_POWERS_COAL_EMPTY);
        init_icon_pixmap(CST_POWERS_COAL_LOW);
	prog_box("",72);
        init_icon_pixmap(CST_POWERS_COAL_MED);
        init_icon_pixmap(CST_POWERS_COAL_FULL);
// substations
        init_icon_pixmap(CST_SUBSTATION_R);
        init_icon_pixmap(CST_SUBSTATION_G);
        init_icon_pixmap(CST_SUBSTATION_RG);
// markets
        init_icon_pixmap(CST_MARKET_EMPTY);
        init_icon_pixmap(CST_MARKET_LOW);
        init_icon_pixmap(CST_MARKET_MED);
        init_icon_pixmap(CST_MARKET_FULL);
// coal mines
        init_icon_pixmap(CST_COALMINE_EMPTY);
        init_icon_pixmap(CST_COALMINE_LOW);
        init_icon_pixmap(CST_COALMINE_MED);
        init_icon_pixmap(CST_COALMINE_FULL);
// ore mines
	prog_box("",78);
        init_icon_pixmap(CST_OREMINE_1);
        init_icon_pixmap(CST_OREMINE_2);
        init_icon_pixmap(CST_OREMINE_3);
        init_icon_pixmap(CST_OREMINE_4);
        init_icon_pixmap(CST_OREMINE_5);
        init_icon_pixmap(CST_OREMINE_6);
        init_icon_pixmap(CST_OREMINE_7);
        init_icon_pixmap(CST_OREMINE_8);
// tips
        init_icon_pixmap(CST_TIP_0);
        init_icon_pixmap(CST_TIP_1);
        init_icon_pixmap(CST_TIP_2);
        init_icon_pixmap(CST_TIP_3);
        init_icon_pixmap(CST_TIP_4);
        init_icon_pixmap(CST_TIP_5);
        init_icon_pixmap(CST_TIP_6);
        init_icon_pixmap(CST_TIP_7);
        init_icon_pixmap(CST_TIP_8);
// rockets
	prog_box("",84);
        init_icon_pixmap(CST_ROCKET_1);
        init_icon_pixmap(CST_ROCKET_2);
        init_icon_pixmap(CST_ROCKET_3);
        init_icon_pixmap(CST_ROCKET_4);
        init_icon_pixmap(CST_ROCKET_5);
        init_icon_pixmap(CST_ROCKET_6);
        init_icon_pixmap(CST_ROCKET_7);
        init_icon_pixmap(CST_ROCKET_FLOWN);
// fire stations
        init_icon_pixmap(CST_FIRESTATION_1);
        init_icon_pixmap(CST_FIRESTATION_2);
        init_icon_pixmap(CST_FIRESTATION_3);
        init_icon_pixmap(CST_FIRESTATION_4);
        init_icon_pixmap(CST_FIRESTATION_5);
        init_icon_pixmap(CST_FIRESTATION_6);
        init_icon_pixmap(CST_FIRESTATION_7);
        init_icon_pixmap(CST_FIRESTATION_8);
        init_icon_pixmap(CST_FIRESTATION_9);
        init_icon_pixmap(CST_FIRESTATION_10);
// cricket
	prog_box("",90);
        init_icon_pixmap(CST_CRICKET_1);
        init_icon_pixmap(CST_CRICKET_2);
        init_icon_pixmap(CST_CRICKET_3);
        init_icon_pixmap(CST_CRICKET_4);
        init_icon_pixmap(CST_CRICKET_5);
        init_icon_pixmap(CST_CRICKET_6);
        init_icon_pixmap(CST_CRICKET_7);
// fire
        init_icon_pixmap(CST_FIRE_1);
        init_icon_pixmap(CST_FIRE_2);
        init_icon_pixmap(CST_FIRE_3);
        init_icon_pixmap(CST_FIRE_4);
        init_icon_pixmap(CST_FIRE_5);
        init_icon_pixmap(CST_FIRE_DONE1);
        init_icon_pixmap(CST_FIRE_DONE2);
        init_icon_pixmap(CST_FIRE_DONE3);
        init_icon_pixmap(CST_FIRE_DONE4);

// others
        init_icon_pixmap(CST_PARKLAND_PLANE);
        init_icon_pixmap(CST_RECYCLE);
        init_icon_pixmap(CST_HEALTH);
        init_icon_pixmap(CST_BURNT);
        init_icon_pixmap(CST_MONUMENT_5);
        init_icon_pixmap(CST_SCHOOL);
        init_icon_pixmap(CST_SHANTY);
	prog_box("",96);
        init_icon_pixmap(CST_POWERS_SOLAR);
        init_icon_pixmap(CST_UNIVERSITY);
        init_icon_pixmap(CST_EX_PORT);

//        init_icon_pixmap();
//        init_icon_pixmap();
//        init_icon_pixmap();

}

void init_icon_pixmap(int type)
{
	unsigned char *g;
	int x,y;
#ifdef ALLOW_PIX_DOUBLING
	if (pix_double)
		icon_pixmap[type]=XCreatePixmap(display.dpy, display.win
			,main_types[type].size*16*2
			,main_types[type].size*16*2
			,DefaultDepth(display.dpy, display.screen));
	else
#endif
	icon_pixmap[type]=XCreatePixmap(display.dpy, display.win
		,main_types[type].size*16,main_types[type].size*16
		,DefaultDepth(display.dpy, display.screen));

//printf("icon_pixmap[%d]=%x",type,icon_pixmap[type]);
	g=(unsigned char *)main_types[type].graphic;
#ifdef ALLOW_PIX_DOUBLING
	if (pix_double)
	{
		for (y=0;y<main_types[type].size*16;y++)
		for (x=0;x<main_types[type].size*16;x++)
			XFillRectangle(display.dpy,icon_pixmap[type]
				,display.pixcolour_gc[*(g++)]
				,x*2,y*2,2,2);
	}
	else
	{
#endif
	for (y=0;y<main_types[type].size*16;y++)
		for (x=0;x<main_types[type].size*16;x++)
			XDrawPoint(display.dpy,icon_pixmap[type]
		,display.pixcolour_gc[*(g++)],x,y);
#ifdef ALLOW_PIX_DOUBLING
	}
#endif
//printf(" - done.\n");
}

//void update_pixmap(int x1,int y1,int sizex,int sizey,int dx,int dy
//	,int real_size,char *g)
//{
//	int x,y,*i,*j;
//#ifdef CATCH_U_P_SIZE_FAULT
//	if ((sizex % sizeof(int)) !=0)
//		printf("Bad size for update_pixmap - x=%d \n"
//			,sizex);
//#endif
//printf("x1=%d y1=%d sizex=%d sizey=%d dx=%d dy=%d\n",x1,y1,sizex
//,sizey,dx,dy);
//	for (y=0;y<sizey;y++)
//	{
//		i=(int *)(pixmap+((dy+y+bordery)*(640+BORDERX)+dx+borderx));
//		j=(int *)(g+((y1+y)*real_size*16+x1));
//printf("%p %p -> %p %p size=%d\n",pixmap,g,i,j,real_size);
//		for (x=0;x<(int)(sizex/sizeof(int));x++)
//			*(i++)=*(j++);
//	}
//}

void update_pixmap(int x1,int y1,int sizex,int sizey,int dx,int dy
        ,int real_size,char *g)
{
	char *i,*j;
        int x,y;
//printf("x1=%d y1=%d sizex=%d sizey=%d dx=%d dy=%d\n",x1,y1,sizex
//,sizey,dx,dy);
        for (y=0;y<sizey;y++)
        {
                i=(pixmap+((dy+y+bordery)*(640+BORDERX)+dx+borderx));
                j=(g+((y1+y)*real_size*16+x1));
//printf("%p %p -> %p %p size=%d\n",pixmap,g,i,j,real_size);
                for (x=0;x<sizex;x++)
                        *(i++)=*(j++);
        }
}

#endif

