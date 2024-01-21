/* This is part of tmview, a dvi previewer. (c) 1995 Thomas Moor         */
/*                                                                       */
/* This program may be used without any warranty. It may be modified and */
/* distributed without any restrictions.                                 */

             
#include <stdio.h>
#include <X11/Xos.h>                      /* X header files              */ 
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xproto.h>
#include <X11/cursorfont.h>

#include <math.h>

#include "defslx.h"

/* prefered Image format */
/* you may set up tmview for the depth of your display. 8bit and 16bit is  */
/* tested. If you use tmview with a depth not the same as defined here, it */
/* will become very slow. */

/* 16 bit per pixel */
/*#define IUNIT unsigned short 
#define IBYTES_PER_PIXEL 2
#define IBITS_PER_PIXEL 16
#define IGET_PIXEL(ptr) (*(ptr))
#define ISET_PIXEL(ptr,c) (*(ptr) = (c)) */
/* 8 bit per pixel */
#define IUNIT unsigned char 
#define IBYTES_PER_PIXEL 1
#define IBITS_PER_PIXEL 8
#define IGET_PIXEL(ptr) (*(ptr))
#define ISET_PIXEL(ptr,c) (*(ptr) = (c))


/* #define DEBUGX  /* watch tmview contacting the XServer */

/* from the file globals.c we need ... */
extern int truevgaxdim,truevgaydim; 
extern int vgaxdim, vgaydim, directscreen;
extern int vgastatuslen, vgastatushight, vgamaxstatuslines, vgastatuslines;

#include "writelx.h"

                            
/* from the file subs.c w need ... */
extern void pfprot(char*, ...);
extern void pfverb(char*, ...);
extern void allocmem(void*,long int);
extern void freemem(void*);


typedef unsigned long Pixel;


/* globals belonging to the X stuff */
static Display *dpy;
static int screen;
static Window dviwin;
static Colormap colormap;
static GC imagecopygc, wincopygc, winfillgc;
static GC wincleartextgc,  wintextgc;
static Cursor standardcursor;
static char* tfontname;
static char* tbackname;
static char* tforename;
static char* dwhitename;
static char* dblackname;
static char* geometry;

/* globals belongimg to ma paintings */
static Pixel palette[NUMBER_OF_COLORS];
static XImage *offimage=NULL;
static char* offscreen=NULL;
static int ibytes_per_line, ibytes_per_pixel;
static int ofscx1, ofscy1, ofscx2, ofscy2;
static int fontw, fonth, fonts, fonta;
static Pixel white, black, dred, dgreen, dblue, lred, lgreen, lblue, lgrey;

/* pointer to bitmap functions */
void (*vgaupdate)(int, int, int, int)=NULL;
void (*vgasetclip)(int, int, int, int)=NULL;
void (*vgadrawrect)(int x, int y, int w, int h, int c)=NULL; 
void (*vgadrawrector)(int x, int y, int w, int h, int c)=NULL; 
void (*vgadrawrectbg)(int x, int y, int w, int h, int c)=NULL; 
void (*vgacopybitmapgs)(int x, int y, int w, int h, void* src)=NULL;
void (*vgacopybitmapbw)(int x, int y, int w, int h, void* src)=NULL;


/* Function prototypes textdarwing */
void preparefont(void); 
void closefont(void);


/* forward for exposeevent handling */
void myexpose(XEvent *event);  
void myupdatestatus(int,int,int,int);

/* forward for bitmap function select */
void setfix(void);
void setvar(void);


/* #include "eventlister.c" */ 



/* this became display dependent. the pxlmem should be accesable by the dvi-
   encoding part and by the routines that come with the display. */

void free_off(void) {
  /* freemem(&offscreen); is done by DexstroyImage ? */
  if(offimage!=NULL)
    XDestroyImage(offimage);
  offimage=NULL;
}

void alloc_off(void) {
  long size;
  int i,j;

  /*truevgaxdim is divisable by 4 !!! */

  free_off();
  offimage= XCreateImage(dpy, DefaultVisual(dpy,screen),
				 DefaultDepth(dpy,screen),
				 ZPixmap, 0, NULL,
				 truevgaxdim,truevgaydim,
				 32, 0);
  size = MAX((long)offimage->bytes_per_line*truevgaydim,1);
  allocmem(&offscreen,size);
  offimage->data=offscreen;
  ibytes_per_line=offimage->bytes_per_line;
  ibytes_per_pixel=offimage->depth >> 3;

  if(ibytes_per_pixel != IBYTES_PER_PIXEL) {
    pfprot("\nwarning: tmVIEW prefers %d-bit depth. Recompile for better performance.\n", IBYTES_PER_PIXEL*8);
    setvar();
  } else {
    pfprot("\nwritelx: using own pix-map functions");
    setfix();
  }

  for(i=0;i<truevgaydim;i++)
    for(j=0;j<truevgaxdim;j++)
      XPutPixel(offimage,j,i,white);

#ifdef DEBUGX 
  pfprot("\n\nImage: bitmap_pad %d ",offimage->bitmap_pad);
  pfprot("bitmap_unit %d ",offimage->bitmap_unit);

  if(offimage->byte_order==LSBFirst) {
    pfprot("bytes LSBfirst ");
  } else
    pfprot("bytes MSBFirst ");
  if(offimage->bitmap_bit_order==LSBFirst) {
    pfprot("bits LSBfirst ");
  } else
    pfprot("bits MSBFirst ");
  pfprot("bytes_per_line %d truevgaxdim %d ",ibytes_per_line,truevgaxdim);
  printf("image at 0x%x, data at 0x%x\n",offimage,offscreen);
#endif
}



void vgaclose(void) {
  free_off();
  /* tidy up ... is to be done */
  XCloseDisplay (dpy);
}


void vgaerror(char* mssg)
{
  pfprot("\nfatal error: display: %s\n",mssg);   
  exit(1);
}



#define UDSIZE 10000L     


void myupdate(int x1, int y1, int x2, int y2) {
  int i,w,h;
  if(offscreen==NULL) return;
  if(x1<0) x1=0; if(y1<0) y1=0;
  if(x2>vgaxdim-1) x2=vgaxdim-1;
  if(y2>vgaydim-1) y2=vgaydim-1;
  if(x2<x1 || y2<y1) return;  
  /* pfprot("\n(myupdate x %d y %d w %d h %d)",x1,y1,x2-x1+1,y2-y1+1);*/
  /* XPutImage(dpy,dviwin,imagecopygc,offimage,x1,y1,x1,y1,x2-x1+1,y2-y1+1);*/
  w=x2-x1+1;
  h=1+UDSIZE/(w*ibytes_per_pixel);
  for(i=y1;h>0;i+=h) {
    h=MIN(h,y2-i+1);
    XPutImage(dpy,dviwin,imagecopygc,offimage,x1,i,x1,i,w,h);
  }
  XFlush(dpy); 
  /* pfprot(".. done ))"); */
}


void vgascreen(int son) {
  directscreen=son; 
}



void mysetclip(int x1, int y1, int x2, int y2) {
   if(x1<0) x1=0; if(y1<0) y1=0;
   if(x2>vgaxdim-1) x2=vgaxdim-1;
   if(y2>vgaydim-1) y2=vgaydim-1;
   if(x2<x1 || y2<y1) {
      x1=0;x2=0;x2=0;y1=0; /* dirty point? should not happen anyway */
   } 
   ofscx1=x1; 
   ofscy1=y1; 
   ofscx2=x2; 
   ofscy2=y2; 
}

void mysetclipoff(void) {
   ofscx1=0; 
   ofscy1=0; 
   ofscx2=vgaxdim-1; 
   ofscy2=vgaydim-1; 
}


void setcolormapcol(int i,unsigned int r,unsigned int g,unsigned int b){
  XColor color;
  color.red=r;   
  color.green=g;
  color.blue=b;
  color.flags=DoRed | DoGreen | DoBlue;
  color.pixel=i;
  XAllocColor(dpy,colormap,&color);
  XQueryColor(dpy,colormap,&color);
#ifdef DEBUGX
  pfprot("set %d color: r %d g %d b %d pixel 0x%lx \n", 
      i,color.red,color.green, color.blue,color.pixel); 
#endif
  palette[i]=color.pixel;
}


void setcolormap(void) {
  int i;
  float gl;
  XColor whitecolor, blackcolor, color; 

  colormap=DefaultColormap(dpy,screen);
  /* printf("colormap is 0x%x \n",(int) colormap); */

  setcolormapcol(COLORS_PER_GREY,PALDRED);
  setcolormapcol(COLORS_PER_GREY+1,PALDGREEN);
  setcolormapcol(COLORS_PER_GREY+2,PALDBLUE);
  setcolormapcol(COLORS_PER_GREY+3,PALLGREY);
  setcolormapcol(COLORS_PER_GREY+4,PALLRED);
  setcolormapcol(COLORS_PER_GREY+5,PALLGREEN);
  setcolormapcol(COLORS_PER_GREY+6,PALLBLUE);
  setcolormapcol(COLORS_PER_GREY+7,PALLGREY);
  
  if(dblackname!=NULL) 
    XAllocNamedColor(dpy,colormap,dblackname,&blackcolor,&color);
  else {     
    blackcolor.red   =0;
    blackcolor.green =0;
    blackcolor.blue  =0;
  } 
  if(dwhitename!=NULL)
    XAllocNamedColor(dpy,colormap,dwhitename,&whitecolor,&color);
   else {     
    whitecolor.red   =65535;
    whitecolor.green =65535;
    whitecolor.blue  =65535;
  }

  for(i=0; i<COLORS_PER_GREY; i++) {
    gl= (i==0) ? 0.0: exp(GAMMA*log(i/(float) (COLORS_PER_GREY-1)));
    color.red  =MAX(0,MIN(65535,(1-gl)*blackcolor.red  + gl*whitecolor.red));
    color.green=MAX(0,MIN(65535,(1-gl)*blackcolor.green+ gl*whitecolor.green));
    color.blue =MAX(0,MIN(65535,(1-gl)*blackcolor.blue + gl*whitecolor.blue));
    setcolormapcol(COLORS_PER_GREY-1-i,color.red,color.green,color.blue);
  }

  white =palette[0];
  black =palette[COLORS_PER_GREY-1];           
  dred  =palette[COLORS_PER_GREY];           
  dgreen=palette[COLORS_PER_GREY+1];           
  dblue =palette[COLORS_PER_GREY+2];           
  lred  =palette[COLORS_PER_GREY+4];           
  lgreen=palette[COLORS_PER_GREY+5];           
  lblue =palette[COLORS_PER_GREY+6];           
  lgrey =palette[COLORS_PER_GREY+7];           
}



set_sizehints (XSizeHints *hintp, int w, int h, int x, int y,char* geom)
{
    int geom_result;
    
    /* set the size hints, algorithm from xlib xbiff */

    hintp->width = hintp->min_width = VGAXDIMMIN;
    hintp->height = hintp->min_height = VGAYDIMMIN;
    hintp->flags = PMinSize;
    hintp->x = hintp->y = 0;
    geom_result = NoValue;
    if (geom != NULL) {
        geom_result = XParseGeometry (geom, &hintp->x, &hintp->y,
				      (unsigned int *)&hintp->width,
				      (unsigned int *)&hintp->height);
	if ((geom_result & WidthValue) && (geom_result & HeightValue)) {
	    hintp->width = MAX(hintp->width, hintp->min_width);
	    hintp->height = MAX(hintp->height, hintp->min_height);
	    hintp->flags |= USSize;
	}
	if ((geom_result & XValue) && (geom_result & YValue)) {
	    hintp->flags += USPosition;
	}
    }
    if (!(hintp->flags & USSize)) {
	hintp->width = w;
	hintp->height= h;
	hintp->flags |= PSize;
    }
    if (geom_result & XNegative) {
	hintp->x = DisplayWidth (dpy, DefaultScreen (dpy)) + hintp->x -
		    hintp->width;
    }
    if (geom_result & YNegative) {
	hintp->y = DisplayHeight (dpy, DefaultScreen (dpy)) + hintp->y -
		    hintp->height;
    }
}




void vgaopen(void) {
  char* displayname=NULL;
  int* depthlist;
  int n;
  XPixmapFormatValues* pixforlist;
  XVisualInfo* vislist;
  XVisualInfo  vistemp;
  int npixforlist;
  int i;

  offscreen=NULL;  /* to be shure */

  
  /* better ask option !! */    
  dpy=XOpenDisplay(displayname);
  if (!dpy) {
    pfprot("vgaopen:  unable to open display '%s'\n",
         XDisplayName (displayname));
    vgaerror("display");
  }
  screen=DefaultScreen(dpy);

  /* for debugging only: costs performance ! */
  /* XSynchronize(dpy,True); */


  /* show resources */
#ifdef DEBUGX
  pfprot("(rmfstr\n %s\n)\n\n",XResourceManagerString(dpy));
#endif

  tfontname=XGetDefault(dpy,"DviLX","sfont");
  tforename=XGetDefault(dpy,"DviLX","sforeground");
  tbackname=XGetDefault(dpy,"DviLX","sbackground");
  dblackname=XGetDefault(dpy,"DviLX","foreground");
  dwhitename=XGetDefault(dpy,"DviLX","background");
  geometry=XGetDefault(dpy,"DviLX","geometry");

#ifdef DEBUGX

  pfprot("(getdef tfont  %s)\n",tfontname);
  pfprot("(getdef tfore  %s)\n",tforename);
  pfprot("(getdef tback  %s)\n",tbackname);
  pfprot("(getdef dwhite %s)\n",dwhitename);
  pfprot("(getdef dblack %s)\n",dblackname);
  pfprot("(getdef geom   %s)\n",geometry);

  /* Get some info */
  depthlist=XListDepths(dpy,screen,&n);
  printf("\nList of depths:");
  if(depthlist != NULL)
    for(i=0;i<n;i++) printf(" %d",depthlist[i]);
  printf("\n\n");
  XFree(depthlist);
  printf("Default depth: %d \n",DefaultDepth(dpy,screen));

  vistemp.screen=screen;
  vislist=XGetVisualInfo(dpy,VisualScreenMask,&vistemp,&n);
  pfprot("\nVisuals on default Screen: ");
  for(i=0;i<n;i++) pfprot(" %d",vislist[i].depth);
  pfprot("\n\n");
  XFree(vislist);

  pixforlist=XListPixmapFormats(dpy,&npixforlist);
  printf("\nList of pixfors:");
  if(pixforlist != NULL)
    for(i=0;i<npixforlist;i++) 
      printf(" (d %d bpp %d scpad %d)",
          pixforlist[i].depth,pixforlist[i].bits_per_pixel,
          pixforlist[i].scanline_pad);
  printf("\n\n");
  XFree(pixforlist);

  if(XImageByteOrder(dpy)==LSBFirst) {
    pfprot("ImageBytes LSBfirst ");
  } else
    pfprot("ImageBytes MSBFirst ");

  if(XBitmapBitOrder(dpy)==LSBFirst) {
    pfprot("Bitmap LSBfirst ");
  } else
    pfprot("Bitmap MSBFirst ");

  pfprot("BitmapPad %d",XBitmapPad(dpy));
  pfprot("BitmapUnit %d\n",XBitmapUnit(dpy));

#endif

  /* set up a colormap */
  setcolormap();

  setvar(); /* for saftey ??? seems to be nonsens !!!*/
}


void vgagraph(void) {

  XSetWindowAttributes attr;
  XWindowAttributes gattr;
  char* geom=NULL; 
  XSizeHints hints;
  XGCValues values;
  unsigned long valuemask, attrmask;

    
  vgaxdim = MAX(VGAXDIMMIN,vgaxdim);  
  vgaydim = MAX(VGAYDIMMIN,vgaydim);
  set_sizehints (&hints, vgaxdim, vgaydim, 10,10,geometry);
 

  /* setup window attributes */
  attr.event_mask = KeyPressMask | ButtonPressMask |
		    ButtonReleaseMask | PointerMotionMask |
                    EnterWindowMask | LeaveWindowMask | 
		    ButtonMotionMask | ExposureMask | StructureNotifyMask;

   

  attr.background_pixel = palette[0];
  attr.backing_store=NotUseful;
  attrmask = CWBackPixel | CWEventMask | CWBackingStore;
  dviwin = XCreateWindow (dpy, RootWindow(dpy, screen), hints.x, hints.y,
			   hints.width, hints.height,5, 
                           DefaultDepth(dpy,screen), InputOutput, 
                           DefaultVisual(dpy,screen),
			   attrmask, &attr);

  XSetStandardProperties(dpy, dviwin, "tmVIEW", NULL, (Pixmap) 0,
				NULL, 0, &hints);

  XMapWindow (dpy, dviwin);
  XFlush(dpy);

  standardcursor=XCreateFontCursor(dpy,XC_crosshair);
  XDefineCursor(dpy,dviwin,standardcursor);


  XGetWindowAttributes(dpy,dviwin,&gattr);

  vgaxdim=gattr.width;
  vgaydim=gattr.height;
  truevgaxdim=(vgaxdim | 3) +1;
  truevgaydim=vgaydim;

  values.function=GXcopy;
  valuemask=GCFunction;
  imagecopygc=XCreateGC(dpy, dviwin, valuemask, &values);
  wincopygc=XCreateGC(dpy, dviwin, valuemask, &values);
  values.fill_style=FillSolid;
  valuemask=GCFillStyle;
  winfillgc=XCreateGC(dpy, dviwin, valuemask, &values);

  alloc_off();
  mysetclip(0,0,vgaxdim-1,vgaydim-1);

  preparefont();
  directscreen=1;
}



/***************************************************************************/
/*   now the pixel copy stuff:                                             */
/*   destination is allways the offscreen buffer                           */
/*   when directscreen is on, draw on the screen too                       */
/*                                                                         */
/*   first there is a set of functions using XGetPixel/XPutPixel           */
/*   (this is slow, because those functions are not ment to be             */
/*   used on huge pixmaps)                                                 */
/*   second there is a set of functions assuming 8/16-bit-per-pixel        */
/*   (this is not that slow as the above)                                  */
/*                                                                         */
/***************************************************************************/


void mycopybitsbw(int tx, int ty, int sw, int sh, void* src) {
/* this is with self made clipping. no good  ? */
/* but it will do for all kind of BMUNIT       */
 
  int j,i,supi,supj,mini,minj,sx, skip;
  BMUNIT *srow, *sbmu;
  BMUNIT premask, firstpremask;
  register BMUNIT data, mask;
  short sbmu_wide;


  /*fprintf(prot,"(vgacopybitmap1 : tx %d ty %d sw %d sh %d, ...\n",
       tx,ty,sw, sh); */                             
  mini=minj=0;
  if(ty<ofscy1) {mini= ofscy1-ty; ty=ofscy1;} 
  if(tx<ofscx1) {minj= ofscx1-tx; tx=ofscx1;} 
  supi= MIN(sh,ofscy2+1-ty+mini);
  supj= MIN(sw,ofscx2+1-tx+minj);
  if(supi <= mini || supj <= minj) {
     /*fprintf(prot,"clipped all away. no drawings)\n"); */
     return;
  }

  sbmu_wide= ROUNDUP(sw,BITS_PER_BMUNIT);
  skip= minj & (BITS_PER_BMUNIT-1);
  sx= minj >> BITS_LOG2;
  premask = 1 << (BITS_PER_BMUNIT-1);
  firstpremask = premask >> skip;

  supj=supj+tx-minj;
  supi=supi+ty-mini;
  
  srow=((BMUNIT*)src)+mini*sbmu_wide+sx;
  for (i = ty;i<supi;i++) {
    mask = firstpremask;
    sbmu=srow;
    data=*sbmu;
    for (j = tx;j<supj;j++) {       
      if(data & mask) XPutPixel(offimage,j,i,black);
      mask >>= 1;
      if(!mask) {
        mask=premask;
        data= *(++sbmu);
      }
    }
    srow+=sbmu_wide;
    sbmu=srow;
  }
  if(directscreen) {
    XPutImage(dpy,dviwin,imagecopygc,offimage,tx,ty,tx,ty,supj-tx,supi-ty);
  /* XFlush(dpy); */ 
  }
}




void mycopybitsgs(int tx, int ty, int sw, int sh, void* src) { 
/* this is with self made clipping. no good  ? */
/* but it will do for all kind of BMUNIT       */
/* compatible to gl_putbox                     */

  int j,i,supi,supj,mini,minj,sx, skip;
  BMUNIT *srow, *sbmu;
  BMUNIT premask, firstpremask;
  register BMUNIT data, mask;
  short sbmu_wide;
  int rs, firstrs;

  /* fprintf(prot,"(vgacopybitmap8 : tx %d ty %d sw %d sh %d, ...\n",
       tx,ty,sw, sh);                             
  fprintf(prot," clipping : %d %d %d %d, ...\n",
       ofscx1,ofscy1,ofscx2,ofscy2);*/
  mini=minj=0;
  if(ty<ofscy1) {mini= ofscy1-ty; ty=ofscy1;} 
  if(tx<ofscx1) {minj= ofscx1-tx; tx=ofscx1;} 
  supi= MIN(sh,ofscy2+1-ty+mini);
  supj= MIN(sw,ofscx2+1-tx+minj);
  if(supi <= mini || supj <= minj) {
     /* fprintf(prot,"clipped all away. no drawings)\n"); */
     return;
  }

  
  sbmu_wide= ROUNDUP(sw << GREYSCALE_LOG2,BITS_PER_BMUNIT);
  skip= minj & ((BITS_PER_BMUNIT>>GREYSCALE_LOG2)-1);
  sx= minj >> (BITS_LOG2-GREYSCALE_LOG2);
  premask = ((1<<GREYSCALE)-1) << (BITS_PER_BMUNIT-GREYSCALE);
  firstpremask = premask >> (skip<< GREYSCALE_LOG2);
  firstrs = BITS_PER_BMUNIT-GREYSCALE - skip*GREYSCALE;

  supj=supj+tx-minj;
  supi=supi+ty-mini;

  srow=((BMUNIT*)src)+mini*sbmu_wide+sx;
  for (i = ty;i<supi;i++) {
    mask = firstpremask;
    rs = firstrs;
    sbmu=srow;
    data=*sbmu;
    for (j = tx;j<supj;j++) {
      if(XGetPixel(offimage,j,i)==white) 
         XPutPixel(offimage,j,i,palette[((data & mask) >> rs)]);
      mask >>= GREYSCALE;
      rs-=GREYSCALE;
      if(!mask) {
        mask=premask;
        rs=BITS_PER_BMUNIT-GREYSCALE;
        data= *(++sbmu);
      }
    }
    srow+=sbmu_wide;
    sbmu=srow;
  }
  if(directscreen) {
    XPutImage(dpy,dviwin,imagecopygc,offimage,tx,ty,tx,ty,supj-tx,supi-ty); 
  /* XFlush(dpy); */ 
  }
}


void myfillbox(int tx, int ty, int w, int h, int c) {
 
  int i,j,supj,supi;
  Pixel cpix;

  /* fprintf(prot,"(vgafillbox : tx %d ty %d sw %d sh %d, ...\n",
       tx,ty,sw, sh); */                             
  if(ty<ofscy1) {h-=ofscy1-ty;  ty=ofscy1;} 
  if(tx<ofscx1) {w-=ofscx1-tx; tx=ofscx1;} 
  if(ty+h>ofscy2+1) h=ofscy2+1-ty;
  if(tx+w>ofscx2+1) w=ofscx2+1-tx;
  if(w <= 0 || h<= 0) {
     /*fprintf(prot,"clipped all away. no drawings)\n"); */
     return;
  }
  supi=ty+h;
  supj=tx+w;

  /* fprintf(prot,"clipped or box: tx %d ty %d w %d h %d   \n",
     tx,ty,w,h); */ 

  cpix=palette[c];

  for (i=ty;i<supi;i++) 
    for (j=tx;j<supj;j++) 
       XPutPixel(offimage,j,i,cpix);
  if(directscreen) {
    XSetForeground(dpy,winfillgc,cpix);
    XFillRectangle(dpy,dviwin,winfillgc,tx,ty,w,h); 
  }
}

void myfillboxor(int tx, int ty, int w, int h, int c) {
 
  int i,j,supj,supi;
  Pixel cpix;

  /* fprintf(prot,"(vgafillboxor : tx %d ty %d sw %d sh %d, ...\n",
       tx,ty,sw, sh); */                             
  if(ty<ofscy1) {h-=ofscy1-ty;  ty=ofscy1;} 
  if(tx<ofscx1) {w-=ofscx1-tx; tx=ofscx1;} 
  if(ty+h>ofscy2+1) h=ofscy2+1-ty;
  if(tx+w>ofscx2+1) w=ofscx2+1-tx;
  if(w <= 0 || h<= 0) {
     /*fprintf(prot,"clipped all away. no drawings)\n"); */
     return;
  }
 /* fprintf(prot,"clipped or box: tx %d ty %d w %d h %d   \n",
     tx,ty,w,h); */ 


  supi=ty+h;
  supj=tx+w;

  cpix=palette[c];
 
  for (i=ty;i<supi;i++) 
    for (j=tx;j<supj;j++) 
      if(XGetPixel(offimage,j,i)==white) 
         XPutPixel(offimage,j,i,cpix);
  if(directscreen) {
    XPutImage(dpy,dviwin,imagecopygc,offimage,tx,ty,tx,ty,w,h); 
  /* XFlush(dpy); */ 
  }
}

void myfillboxbg(int tx, int ty, int w, int h, int c) {
 
  int i,j,supj,supi;
  Pixel cpix;
  register Pixel xdc;

  /* fprintf(prot,"(vgafillboxor : tx %d ty %d w %d h %d, ...\n",
       tx,ty,w,h); */                              
  if(ty<ofscy1) {h-=ofscy1-ty;  ty=ofscy1;} 
  if(tx<ofscx1) {w-=ofscx1-tx; tx=ofscx1;} 
  if(ty+h>ofscy2+1) h=ofscy2+1-ty;
  if(tx+w>ofscx2+1) w=ofscx2+1-tx;
  if(w <= 0 || h<= 0) {
     /*fprintf(prot,"clipped all away. no drawings)\n"); */
     return;
  }

  supi=ty+h;
  supj=tx+w;

  cpix=palette[c];

  /* fprintf(prot,"clipped bgbox : tx %d ty %d w %d h %d   \n",
     tx,ty,w,h); */

  for (i=ty;i<supi;i++) 
    for (j=tx;j<supj;j++) {
      xdc=XGetPixel(offimage,j,i); 
      if(xdc==lred || xdc==lgreen || xdc==lblue || xdc==white) {
        if(xdc==white) XPutPixel(offimage,j,i,cpix);  
        else if(xdc!=cpix) XPutPixel(offimage,j,i,lgrey);
      }
    }
  if(directscreen) {
    XPutImage(dpy,dviwin,imagecopygc,offimage,tx,ty,tx,ty,w,h); 
  /* XFlush(dpy); */ 
  }
}


void mycopybitsbwfix(int tx, int ty, int sw, int sh, void* src) {
/* this is with self made clipping. no good  ? */
/* but it will do for all kind of BMUNIT       */
 
  int j,i,supi,supj,mini,minj,sx, skip, w,h;
  BMUNIT *srow, *sbmu;
  BMUNIT premask, firstpremask;
  register BMUNIT data, mask;
  short sbmu_wide;
  IUNIT *ydest, *xdest;

  /*fprintf(prot,"(vgacopybitmap1 : tx %d ty %d sw %d sh %d, ...\n",
       tx,ty,sw, sh); */                             
  mini=minj=0;
  if(ty<ofscy1) {mini= ofscy1-ty; ty=ofscy1;} 
  if(tx<ofscx1) {minj= ofscx1-tx; tx=ofscx1;} 
  supi= MIN(sh,ofscy2+1-ty+mini);
  supj= MIN(sw,ofscx2+1-tx+minj);
  if(supi <= mini || supj <= minj) {
     /*fprintf(prot,"clipped all away. no drawings)\n"); */
     return;
  }
  w=supj-minj;
  h=supi-mini;

  /* fprintf(prot,"clipped: tx %d ty %d mi %d si %d mj %d sj %d \n",
     tx,ty,mini,supi,minj,supj); */

  sbmu_wide= ROUNDUP(sw,BITS_PER_BMUNIT);
  skip= minj & (BITS_PER_BMUNIT-1);
  sx= minj >> BITS_LOG2;
  premask = 1 << (BITS_PER_BMUNIT-1);
  firstpremask = premask >> skip;

  ydest=((IUNIT*)(offscreen+ty*ibytes_per_line))+tx;  
  srow=((BMUNIT*)src)+mini*sbmu_wide+sx;
  for (i = mini;i<supi;i++) {
    xdest=ydest;
    mask = firstpremask;
    sbmu=srow;
    data=*sbmu;
    for (j = minj;j<supj;j++) {       
      if(data & mask) ISET_PIXEL(xdest,black);
      mask >>= 1;
      xdest++;
      if(!mask) {
        mask=premask;
        data= *(++sbmu);
      }
    }
    srow+=sbmu_wide;
    sbmu=srow;
    ((char*)ydest)+=ibytes_per_line;
  }
  if(directscreen) {
    XPutImage(dpy,dviwin,imagecopygc,offimage,tx,ty,tx,ty,w,h);
  /* XFlush(dpy); */ 
  }
}




void mycopybitsgsfix(int tx, int ty, int sw, int sh, void* src) { 
/* this is with self made clipping. no good  ? */
/* but it will do for all kind of BMUNIT       */
/* compatible to gl_putbox                     */

  int j,i,supi,supj,mini,minj,sx, skip;
  BMUNIT *srow, *sbmu;
  BMUNIT premask, firstpremask;
  register BMUNIT data, mask;
  short sbmu_wide;
  int rs, firstrs, w,h;
  IUNIT *ydest, *xdest;

  /* fprintf(prot,"(vgacopybitmap8 : tx %d ty %d sw %d sh %d, ...\n",
       tx,ty,sw, sh);                             
  fprintf(prot," clipping : %d %d %d %d, ...\n",
       ofscx1,ofscy1,ofscx2,ofscy2);*/
  mini=minj=0;
  if(ty<ofscy1) {mini= ofscy1-ty; ty=ofscy1;} 
  if(tx<ofscx1) {minj= ofscx1-tx; tx=ofscx1;} 
  supi= MIN(sh,ofscy2+1-ty+mini);
  supj= MIN(sw,ofscx2+1-tx+minj);
  if(supi <= mini || supj <= minj) {
     /* fprintf(prot,"clipped all away. no drawings)\n"); */
     return;
  }
  w=supj-minj;
  h=supi-mini;

  /* fprintf(prot,"clipped: tx %d ty %d mi %d si %d mj %d sj %d \n",
     tx,ty,mini,supi,minj,supj); */ 

  
  sbmu_wide= ROUNDUP(sw << GREYSCALE_LOG2,BITS_PER_BMUNIT);
  skip= minj & ((BITS_PER_BMUNIT>>GREYSCALE_LOG2)-1);
  sx= minj >> (BITS_LOG2-GREYSCALE_LOG2);
  premask = ((1<<GREYSCALE)-1) << (BITS_PER_BMUNIT-GREYSCALE);
  firstpremask = premask >> (skip<< GREYSCALE_LOG2);
  firstrs = BITS_PER_BMUNIT-GREYSCALE - skip*GREYSCALE;

  ydest=((IUNIT*)(offscreen+ty*ibytes_per_line))+tx;
  srow=((BMUNIT*)src)+mini*sbmu_wide+sx;
  for (i = mini;i<supi;i++) {
    xdest=ydest;
    mask = firstpremask;
    rs = firstrs;
    sbmu=srow;
    data=*sbmu;
    for (j = minj;j<supj;j++) {
      if(IGET_PIXEL(xdest)==white) 
         ISET_PIXEL(xdest,palette[((data & mask) >> rs)]);
      mask >>= GREYSCALE;
      rs-=GREYSCALE;
      xdest++;
      if(!mask) {
        mask=premask;
        rs=BITS_PER_BMUNIT-GREYSCALE;
        data= *(++sbmu);
      }
    }
    srow+=sbmu_wide;
    sbmu=srow;
    ((char*)ydest)+=ibytes_per_line;
  }
  if(directscreen) {
    XPutImage(dpy,dviwin,imagecopygc,offimage,tx,ty,tx,ty,w,h); 
  /* XFlush(dpy); */ 
  }
}


void myfillboxfix(int tx, int ty, int w, int h, int c) {
 
  IUNIT *ydest, *xdest;
  int i,j;
  Pixel cpix;

  /* fprintf(prot,"(vgafillbox : tx %d ty %d sw %d sh %d, ...\n",
       tx,ty,sw, sh); */                             
  if(ty<ofscy1) {h-=ofscy1-ty;  ty=ofscy1;} 
  if(tx<ofscx1) {w-=ofscx1-tx; tx=ofscx1;} 
  if(ty+h>ofscy2+1) h=ofscy2+1-ty;
  if(tx+w>ofscx2+1) w=ofscx2+1-tx;
  if(w <= 0 || h<= 0) {
     /*fprintf(prot,"clipped all away. no drawings)\n"); */
     return;
  }

  /* fprintf(prot,"clipped or box: tx %d ty %d w %d h %d   \n",
     tx,ty,w,h); */ 

  cpix=palette[c];
  ydest=((IUNIT*)(offscreen+ty*ibytes_per_line))+tx;
  for (i=h;i>0;i--) {
    xdest=ydest; 
    for (j=w;j>0;j--) {
       ISET_PIXEL(xdest,cpix);
       xdest++;
    }
    ((char*)ydest)+=ibytes_per_line;
  }
  if(directscreen) {
    XSetForeground(dpy,winfillgc,cpix);
    XFillRectangle(dpy,dviwin,winfillgc,tx,ty,w,h); 
  }
}

void myfillboxorfix(int tx, int ty, int w, int h, int c) {
 
  IUNIT *ydest, *xdest;
  int i,j;
  Pixel cpix;

  /* fprintf(prot,"(vgafillboxor : tx %d ty %d sw %d sh %d, ...\n",
       tx,ty,sw, sh); */                             
  if(ty<ofscy1) {h-=ofscy1-ty;  ty=ofscy1;} 
  if(tx<ofscx1) {w-=ofscx1-tx; tx=ofscx1;} 
  if(ty+h>ofscy2+1) h=ofscy2+1-ty;
  if(tx+w>ofscx2+1) w=ofscx2+1-tx;
  if(w <= 0 || h<= 0) {
     /*fprintf(prot,"clipped all away. no drawings)\n"); */
     return;
  }

 /* fprintf(prot,"clipped or box: tx %d ty %d w %d h %d   \n",
     tx,ty,w,h); */ 

  cpix=palette[c];
  ydest=((IUNIT*)(offscreen+ty*ibytes_per_line))+tx;
  for (i=h;i>0;i--) {
    xdest=ydest; 
    for (j=w;j>0;j--) {
       if(IGET_PIXEL(xdest)==white) ISET_PIXEL(xdest,cpix);
       xdest++;
    }
    ((char*)ydest)+=ibytes_per_line;
  }
  if(directscreen) {
    XPutImage(dpy,dviwin,imagecopygc,offimage,tx,ty,tx,ty,w,h); 
  /* XFlush(dpy); */ 
  }
}

void myfillboxbgfix(int tx, int ty, int w, int h, int c) {
 
  IUNIT *ydest;
  int i,j;
  Pixel cpix;
  register IUNIT *xdest;
  register Pixel xdc;

  /* fprintf(prot,"(vgafillboxor : tx %d ty %d w %d h %d, ...\n",
       tx,ty,w,h); */                              
  if(ty<ofscy1) {h-=ofscy1-ty;  ty=ofscy1;} 
  if(tx<ofscx1) {w-=ofscx1-tx; tx=ofscx1;} 
  if(ty+h>ofscy2+1) h=ofscy2+1-ty;
  if(tx+w>ofscx2+1) w=ofscx2+1-tx;
  if(w <= 0 || h<= 0) {
     /*fprintf(prot,"clipped all away. no drawings)\n"); */
     return;
  }

  /* fprintf(prot,"clipped bgbox : tx %d ty %d w %d h %d   \n",
     tx,ty,w,h); */

  cpix=palette[c];
  ydest=((IUNIT*)(offscreen+ty*ibytes_per_line))+tx;
  for (i=h;i>0;i--) {
    xdest=ydest;
    for (j=w;j>0;j--) {      
      xdc=IGET_PIXEL(xdest); 
      if(xdc==lred || xdc==lgreen || xdc==lblue || xdc==white) {
        if(xdc==white) ISET_PIXEL(xdest,cpix);  
        else if(xdc!=cpix) ISET_PIXEL(xdest,lgrey);
      }
      xdest++;
    }
    ((char*)ydest)+=ibytes_per_line;
  }
  if(directscreen) {
    XPutImage(dpy,dviwin,imagecopygc,offimage,tx,ty,tx,ty,w,h); 
  /* XFlush(dpy); */ 
  }
}




void vgaxscroll(short dx) {
  int i,dbx,len;  
  if(abs(dx) >= vgaxdim) return;
  XFlush(dpy);
  len=(vgaxdim-abs(dx))*ibytes_per_pixel;
  dbx=dx*ibytes_per_pixel;
  if(dx<0) {
    for(i=0;i<vgaydim;i++)
      memmove(offscreen+(long)i*ibytes_per_line,
              offscreen+(long)i*ibytes_per_line-dbx,len);
     if(directscreen) XCopyArea(dpy, dviwin, dviwin,wincopygc, -dx, 0,
		vgaxdim+dx, vgaydim, 0, 0); 
  }  else {
    for(i=0;i<vgaydim;i++)
      memmove(offscreen+(long)i*ibytes_per_line+dbx,
              offscreen+(long)i*ibytes_per_line,len);
    if(directscreen) XCopyArea(dpy, dviwin, dviwin, wincopygc, 0, 0,
		vgaxdim-dx, vgaydim, dx, 0); 
  }
  XFlush(dpy);
  if(directscreen) myexpose(NULL); 
}
   
void vgayscroll(short dy) {
  int i;
  if(abs(dy) >= vgaydim) return;
  XFlush(dpy);
  if(dy<0) {
    for(i=0;i<vgaydim+dy;i++)
      memmove(offscreen+(long)i*ibytes_per_line,
              offscreen+(long)(i-dy)*ibytes_per_line,ibytes_per_line);
    if(directscreen) XCopyArea(dpy, dviwin, dviwin,wincopygc, 0, -dy,
		vgaxdim, vgaydim+dy, 0, 0); 

  }  else {
    for(i=vgaydim-1;i>=dy;i--)
      memmove(offscreen+(long)i*ibytes_per_line,
              offscreen+(long)(i-dy)*ibytes_per_line,ibytes_per_line);
    if(directscreen) XCopyArea(dpy, dviwin, dviwin, wincopygc, 0, 0,
		vgaxdim, vgaydim-dy, 0, dy); 
  }
  XFlush(dpy);  
  if(directscreen) myexpose(NULL); 
}   

void setvar(void) {
  vgaupdate=myupdate;
  vgasetclip=mysetclip;
  vgadrawrect=myfillbox; 
  vgadrawrector=myfillboxor; 
  vgadrawrectbg=myfillboxbg; 
  vgacopybitmapgs=mycopybitsgs;
  vgacopybitmapbw=mycopybitsbw;
}

void setfix(void) {
  vgaupdate=myupdate;
  vgasetclip=mysetclip;
  vgadrawrect=myfillboxfix; 
  vgadrawrector=myfillboxorfix; 
  vgadrawrectbg=myfillboxbgfix; 
  vgacopybitmapgs=mycopybitsgsfix;
  vgacopybitmapbw=mycopybitsbwfix;
}




/***************************************************************************/
/* events ******************************************************************/
/***************************************************************************/


void myexpose(XEvent *event) { /* call with an event or NULL */
  XGraphicsExposeEvent *gexev;
  XExposeEvent *exev;   
  XEvent aevent;

  XSync(dpy,False);
  if(event==NULL) {
    event=&aevent;
    if(!XCheckTypedEvent(dpy,Expose | GraphicsExpose,event))
    return;
  }  
  do{
    if(event->type==Expose) {
      exev = (XExposeEvent *) event;
      myupdate(exev->x,exev->y,
              exev->x+exev->width-1,exev->y+exev->height-1);
      myupdatestatus(exev->x,exev->y,
              exev->x+exev->width-1,exev->y+exev->height-1);
      /* pfprot("Expose: x %d y %d w %d h %d ",exev->x,exev->y,
              exev->x+exev->width-1,exev->y+exev->height-1); */
    }
    if(event->type==GraphicsExpose) {
      gexev = (XGraphicsExposeEvent *) event;
      myupdate(gexev->x,gexev->y,
        gexev->x+gexev->width-1,gexev->y+gexev->height-1);
      myupdatestatus(gexev->x,gexev->y,
        gexev->x+gexev->width-1,gexev->y+gexev->height-1);
      /* pfprot("GraphicsExpose: x %d y %d w %d h %d ",gexev->x,gexev->y,
              gexev->x+gexev->width-1,gexev->y+gexev->height-1); */
    }
  } while(XCheckTypedEvent(dpy,Expose | GraphicsExpose,event));
  /* while(XCheckTypedEvent(dpy,Expose | GraphicsExpose,&event)); */
  /* pfprot("done"); */
}





int  mouseleft=0;
int  mouseright=0;
int  mousex=0; 
int  mousey=0;

void vgagetmouse(int *x, int *y, int *left, int* right) {
  *left= mouseleft;
  *right= mouseright;
  *x=mousex;
  *y=mousey;
 /* pfprot("(getmouse x %d y %d wx %d wy %d)",mousex,mousey,clientx,clienty);*/
}  


void vgasetmouse(int x, int y) {
 mousex=x;
 mousey=y;
 /*pfprot("(setmouse x %d y %d)",x,y); */
}  



uchar vgagetchar(int block) {
  uchar res=KEYNOP;  
  int i;
  XEvent event;
  XEvent dummyevent;
  XKeyEvent *keyev;
  XConfigureEvent *conev;
  XMotionEvent *moev;
  XCrossingEvent *crossev;
  XButtonEvent *butev;
  
  KeySym keysym;
  char *keynam;
  int slen;
  char keystr[256+1];
  int newxdim, newydim, nev; 

  nev=XEventsQueued(dpy,QueuedAfterFlush);
  if(!block && nev==0) return(res);
  XNextEvent(dpy,&event); 
  /* MyEvents(&event);  use eventlister for protocoll */
  switch (event.type) {
  case KeyPress:
    keyev = (XKeyEvent *) &event;
    while(XCheckTypedEvent(dpy,KeyPress,&dummyevent));  
    slen=XLookupString (keyev, keystr, 256, &keysym, NULL);
    if (keysym == NoSymbol) 
       keynam = "NoSymbol";
    else if (!(keynam = XKeysymToString (keysym)))
       keynam = "(no name)";
    if(keysym>=' ' && keysym <127)       { res=keysym;   break;}
    if(strcmp(STR_RET,keynam)==0)        { res=KEYRET;   break;}
    if(strcmp(STR_TAB,keynam)==0)        { res=KEYTAB;   break;} 
    if(strcmp(STR_ESC,keynam)==0)        { res=KEYESC;   break;} 
    if(strcmp(STR_BS,keynam)==0)         { res=127;      break;} 
    if(slen==1) 
    if(keystr[0]>=' ' && keystr[0] <127) { res=keystr[0];break;}
    if(strcmp(STR_RIGHT,keynam)==0)      { res=KEYRIGHT; break;}
    if(strcmp(STR_LEFT,keynam)==0)       { res=KEYLEFT;  break;}
    if(strcmp(STR_UP,keynam)==0)         { res=KEYUP;    break;}
    if(strcmp(STR_DOWN,keynam)==0)       { res=KEYDOWN;  break;}
    if(strcmp(STR_NEXT,keynam)==0)       { res=KEYNEXT;  break;}
    if(strcmp(STR_PREV,keynam)==0)       { res=KEYPREV;  break;}
    if(strcmp(STR_HOME,keynam)==0)       { res=KEYHOME;  break;}
    if(strcmp(STR_END,keynam)==0)        { res=KEYEND;   break;}
    break;
  case Expose:
  case GraphicsExpose: 
    myexpose(&event);
    break;
  case DestroyNotify:
    res='q';
    break;
  case ConfigureNotify:
    if(offscreen==NULL) break; /* not started up jet */
    /* pfprot("(oldsize %d %d)",vgaxdim,truevgaydim); */
    conev=(XConfigureEvent *) &event;
    newxdim = MAX(VGAXDIMMIN,conev->width);  
    newydim = MAX(VGAYDIMMIN,conev->height);
    if(newxdim==vgaxdim && newydim==truevgaydim) break;
    truevgaxdim=(newxdim | 3) + 1;
    truevgaydim=newydim;
    vgaxdim=newxdim;
    vgaydim=newydim;
    pfprot("(resize %d %d)\n",vgaxdim,vgaydim); 
    free_off();
    alloc_off(); 
    vgamaxstatuslines=(truevgaydim-3)/vgastatushight;
    vgastatuslines=0;
    vgastatuslen=(vgaxdim-2)/fontw;
    mysetclipoff();
    res=KEYRESIZE;
    break;
  case ButtonPress:
    butev = (XButtonEvent *) &event;
    if(butev->button ==1) mouseleft=1;
    if(butev->button ==3) mouseright=1;
    break;      
  case ButtonRelease:
    butev = (XButtonEvent *) &event;
    if(butev->button ==1) mouseleft=0;
    if(butev->button ==3) mouseright=0;
    break;      
  case MotionNotify:  
    do{
      moev = (XMotionEvent *) &event;
      mousex=moev->x;
      mousey=moev->y;
    } while(XCheckTypedEvent(dpy,MotionNotify,&event));  
  case LeaveNotify:
  case EnterNotify:
    crossev= (XCrossingEvent *) &event;
    if(crossev->mode != NotifyNormal) break;
    mousex=crossev->x;
    mousey=crossev->y;
  default:
    break;
  }
  return(res);
}





/***************************************************************************/
/* now the font stuff ...                                                  */


Pixel tforepixel;
Pixel tbackpixel;
Font  tfontfont;
XFontStruct* tfontstruct;
char* offstatus=NULL;


void preparefont(void) {
  XColor colordef, colorex;
  XGCValues values;
  unsigned long valuemask;
  char** fontlist;
  int i,nfontlist;
  
  if(tforename!=NULL) {
    XAllocNamedColor(dpy,colormap,tforename,&colordef,&colorex);
    tforepixel=colordef.pixel;
  } else 
    tforepixel= BlackPixel(dpy,screen);     
  if(tbackname!=NULL) {
    XAllocNamedColor(dpy,colormap,tbackname,&colordef,&colorex);
    tbackpixel=colordef.pixel;
  } else 
    tbackpixel= WhitePixel(dpy,screen); 

  
  if(tfontname==NULL) tfontname="*-fixed-*";
  fontlist=XListFonts(dpy,tfontname,20,&nfontlist);

#ifdef DEBUGX  
  pfprot("(fontnames:\n");
  for(i=0;i<nfontlist;i++)
   pfprot("%s\n",fontlist[i]);
  pfprot("\n");
  if(nfontlist==0) vgaerror("no such font");
#endif  

  tfontstruct=XLoadQueryFont(dpy,fontlist[0]);
  if(tfontstruct==NULL) vgaerror("Cant load font");
  
  XFreeFontNames(fontlist);

#ifdef DEBUGX
  pfprot("(fontinfo:\n");
  pfprot("min: lb %d rb %d w %d as %d des %d\n",
   tfontstruct->min_bounds.lbearing, tfontstruct->min_bounds.rbearing,
   tfontstruct->min_bounds.width, tfontstruct->min_bounds.ascent,
   tfontstruct->min_bounds.descent);
  pfprot("max: lb %d rb %d w %d as %d des %d)\n\n",
   tfontstruct->max_bounds.lbearing, tfontstruct->max_bounds.rbearing,
   tfontstruct->max_bounds.width, tfontstruct->max_bounds.ascent,
   tfontstruct->max_bounds.descent);
#endif

  if(tfontstruct->min_bounds.width !=tfontstruct->max_bounds.width)
     pfprot("\nprepare font: warning: font will be taken as fixed !!\n");
  
  fonth=tfontstruct->descent+tfontstruct->ascent;
  fontw=tfontstruct->max_bounds.width;
  fonts=2;
  fonta=tfontstruct->ascent+1;

  tfontfont=tfontstruct->fid;
 
  values.foreground=tforepixel;
  values.background=tbackpixel;
  values.font=tfontfont;
  valuemask= GCForeground | GCBackground | GCFont ;
  wintextgc=XCreateGC(dpy, dviwin, valuemask, &values);
  values.foreground=tbackpixel;
  valuemask= GCForeground; 
  wincleartextgc=XCreateGC(dpy, dviwin, valuemask, &values);

  vgastatushight=fonth+fonts;
  vgamaxstatuslines=(truevgaydim-3)/vgastatushight;
  vgastatuslen=(vgaxdim-2)/fontw;

}    

void closefont(void) {
  XFreeFont(dpy,tfontstruct);
  freemem(&offstatus);
}




void myupdatestatus(int x1, int y1, int x2, int y2) {
 int i,xc,yc,hc,wc;

 if(offstatus==NULL) return;
 if(x1<0) x1=0; 
 if(y1<0) y1=0;
 if(y1<vgaydim && vgastatuslines!=vgamaxstatuslines) y1=vgaydim;
 if(x2>vgaxdim-1) x2=vgaxdim-1;
 if(y2>truevgaydim-1) y2=truevgaydim-1;
 if(x2<x1 || y2<y1) return;  

 XFillRectangle(dpy,dviwin,wincleartextgc,
         x1,y1,x2-x1+1,y2-y1+1); 
 if(vgastatuslines!=vgamaxstatuslines) { 
    XDrawLine(dpy,dviwin,wintextgc,0,vgaydim,vgaxdim-1,vgaydim);
    XDrawLine(dpy,dviwin,wintextgc,0,vgaydim+2,vgaxdim-1,vgaydim+2);
 } 

 xc=MAX((x1-1)/fontw,0);
 yc=MAX((y1-vgaydim-3)/vgastatushight,0);
 wc=MIN((x2-1)/fontw - xc+1,vgastatuslen-xc);
 hc=MIN((y2-vgaydim-3)/vgastatushight-yc+1,vgastatuslines-yc);
 if(wc<=0 || hc <=0) return;

 for(i=0;i<hc;i++) 
  XDrawImageString(dpy,dviwin,wintextgc,
     1+xc*fontw, vgaydim+3+(yc+i)*vgastatushight+fonta,
     offstatus+(yc+i)*(vgastatuslen+1)+xc, wc); 
}


int minpo=0; /* dirty hack to speed up a bit in "proceedthis" */

int vgasetstatuslines(int n) {
  int i;
  vgastatuslines=MAX(0,MIN(vgamaxstatuslines,n));
  vgaydim= truevgaydim - vgastatuslines* vgastatushight - 3;
  mysetclip(0,0,vgaxdim-1,vgaydim-1);
  freemem(&offstatus);
  allocmem(&offstatus,vgastatuslines*(vgastatuslen+1));
  memset(offstatus,' ',vgastatuslines*(vgastatuslen+1));
  for(i=0;i<vgastatuslines;i++) offstatus[i*(vgastatuslen+1)]=0;
  myupdatestatus(0,0,vgaxdim-1,truevgaydim-1);
  minpo=vgastatuslen-1;
  return(vgastatuslines);
}


void vgaupdatestatus(void) {
 int i;
 if(offstatus==NULL) return;
 for(i=0;i<vgastatuslines;i++) 
  XDrawImageString(dpy,dviwin,wintextgc,
     1+minpo*fontw, vgaydim+3+i*vgastatushight+fonta,
     offstatus+i*(vgastatuslen+1)+minpo, vgastatuslen-minpo); 
  minpo=vgastatuslen-1;
}


void vgadrawstatus(char chr, int line, int pos) { 
  if(line>=vgastatuslines || line <0)  return;
  if(pos>=vgastatuslen || pos <0) return;
  if(pos<minpo) minpo=pos;
  offstatus[pos+line*(vgastatuslen+1)]=chr;
}











