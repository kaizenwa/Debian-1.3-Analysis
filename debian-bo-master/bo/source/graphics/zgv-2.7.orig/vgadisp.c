/* Zgv v2.7 - GIF, JPEG and PBM/PGM/PPM viewer, for VGA PCs running Linux.
 * Copyright (C) 1993-1995 Russell Marks. See README for license details.
 *
 * vgadisp.c - vga specific display routines.
 */


#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <malloc.h>
#include <stdlib.h>
#include <signal.h>
#include <sys/file.h>
#include <vga.h>
#include <vgagl.h>
#include "zgv.h"
#include "magic.h"
#include "gifeng.h"
#include "readjpeg.h"
#include "readpnm.h"
#include "readbmp.h"
#include "readpng.h"
#include "readtga.h"
#include "readnbkey.h"
#include "helppage.h"
#include "3deffects.h"
#include "rc_config.h"
#include "rcfile.h"
#include "vgadisp.h"
#include "mouse.h"		       /* for mouse stuff */


/* zgv.c initialises these */
int curvgamode;
int zoom,virtual;
int vkludge;
int pixelsize;		/* in bytes - 1 for 8-bit, 3 for 24-bit */
static int isjpeg;
int pic_incr;		/* after view, 0=stay, -1=prev, 1=next */

char viewerhelp[][80]={
  "? (question mark) \t this help page",
  "5 to 8 \t\t mode change",
#ifndef HANDS_ON_VC_CODE
  "F1 - F12  SF3 - SF10 \t SVGA mode change (try these first)",
#endif
  "0 \t\t\t toggle virtual mode",
  "v \t\t\t toggle smoothing in virtual/zoom modes",
  "comma / dot \t contrast down/up (8-bit modes)",
  "left / right chevron \t brightness down/up (8-bit modes)",
  "asterisk \t\t reset contrast and brightness",
  "z \t\t\t toggle zoomed mode",
  "r \t\t\t rotate clockwise 90 degrees",
  "m \t\t\t mirror",
  "f \t\t\t flip",
  "c \t\t\t toggle centering picture on screen",
  "Esc or x \t\t exit the viewer",
  " ",
  "hjkl or qaop keys scroll around the picture",
  "HJKL or QAOP (also the cursor keys) scroll in bigger steps",
  ""	/* end */
  };



struct {
  int rgb;
  byte col;
  } vkcache[65536];

#define MOVSTP 10    /* for q,a,o,p panning */
#define BIGSTP 100   /* for Q,A,O,P panning (i.e. with shift) */

#define FIX_TO_EIGHT_BIT	1
#define FIX_TO_HIGHCOLOUR	2

byte *theimage;   /* quite slow if passed as parameter */
int width,height,numcols; /* same here */
byte *image_palette;

double contrast=1.0;  /* note that double contrast is in fact 2 :-) */
int brightness=0;
int scrnwide,scrnhigh,scrnpixelsize,scrncols;
int scaling=1,interp=0,inextpix=1;

int palr[256],palg[256],palb[256];
byte palr64[256],palg64[256],palb64[256];
int palrgb[768];

int loading_file_type;
int repeat_sig,first_repeat;
int saved_px,saved_py;		/* for persistance in cfg.repeat_timer mode */

extern int zgv_ttyfd;
int sgres;

void repeat_sighandler();


#define GET15BITCOLOUR(r,g,b) (((r>>3)<<10)|((g>>3)<<5)|(b>>3))
#define GET16BITCOLOUR(r,g,b) (((r>>3)<<11)|((g>>2)<<5)|(b>>3))



/* to read a picture only, set show_dont_tell=0.
 * If you're only reading the picture, not showing it, then:
 * 1. picture is returned in global vars. 'theimage' and 'image_palette'
 * 2. you have to set pixelsize=whatever and restore it afterwards.
 * 3. you have to free() both theimage and image_palette when finished.
 */
int readpicture(giffn,howfarfunc,show_dont_tell)
char *giffn;
hffunc howfarfunc;
int show_dont_tell;
{
int result=0;
PICINFO ginfo;
byte *palette;


first_repeat=1;

do
  {
  ginfo.numcols=256;	/* only changes for GIF files */
  theimage=NULL;
  if(!first_repeat) howfarfunc=NULL;
  
  switch(loading_file_type=magic_ident(giffn))
    {
    case _IS_GIF:
      /* part of the New Strategy (tm); always use 8-bit for GIFs.
       * fix the mode to a similar 8-bit one if possible.
       */
      if(show_dont_tell) fix_to_similar_mode(FIX_TO_EIGHT_BIT);
      pixelsize=1;
      result=readgif(giffn,&theimage,&palette,howfarfunc,cfg.errignore);
      getgifinfo(&ginfo);
      height=ginfo.height; width=ginfo.width;
      if(cfg.errignore && result==_PICERR_CORRUPT) result=_PIC_OK;
      break;
  
    case _IS_JPEG:
      /* don't *always* use 24-bit for JPEGs... check for 15/16/24-bit
       * video modes before doing so, as well as the config.
       */
      if(show_dont_tell)
        {
        pixelsize=has_highcolour_mode()?3:1;
        if(pixelsize==3 && cfg.jpeg24bit==0)
          pixelsize=1;
        }
  
      theimage=NULL;
      result=read_JPEG_file(giffn,howfarfunc,&palette);
      
      /* if error, then palette has already been nuked */
      if(theimage==NULL)
        result=_PICERR_NOMEM;
      else
        if(result!=_PIC_OK) { free(theimage); theimage=NULL; }
        
      if(result!=_PIC_OK) result=_PICERR_SHOWN_ALREADY;
      break;
      
#ifdef PNG_SUPPORT
    case _IS_PNG:
      if(show_dont_tell)
        {
        pixelsize=has_highcolour_mode()?3:1;
        if(pixelsize==3 && cfg.jpeg24bit==0)
          pixelsize=1;
        }
  
      theimage=NULL;
      result=read_png_file(giffn,howfarfunc,&palette);
      
      /* if error, then palette has already been nuked */
      if(theimage==NULL)
        result=_PICERR_NOMEM;
      else
        if(cfg.errignore)
          result=_PIC_OK;
        else
          if(result!=_PIC_OK) { free(theimage); theimage=NULL; }
        
      break;
#endif /* PNG_SUPPORT */
      
    case _IS_PNM: case _IS_BMP: case _IS_TGA:
      if(show_dont_tell) pixelsize=has_highcolour_mode()?3:1;
      switch(loading_file_type)
        {
        case _IS_PNM:
          result=read_pnm_file(giffn,howfarfunc,&theimage,&palette,&pixelsize,
                  &ginfo); break;
        case _IS_BMP:
          result=read_bmp_file(giffn,howfarfunc,&theimage,&palette,&pixelsize,
                  &ginfo); break;
        case _IS_TGA:
          result=read_tga_file(giffn,howfarfunc,&theimage,&palette,&pixelsize,
                  &ginfo); break;
        }
      
      width=ginfo.width;
      height=ginfo.height;
      if(result!=_PIC_OK)
        {
        if(theimage!=NULL) { free(theimage); theimage=NULL; }
        if(palette!=NULL)  { free(palette);  palette=NULL; }
        }
      else
        if(loading_file_type==_IS_TGA && tga_need_flip)
          fx_flip();	/* right way up! */
      break;

    
    /* if they voted for "none of the above"... */
    default:
      return(_PICERR_BADMAGIC);
    }
  
  
  if(show_dont_tell)
    {
    if(pixelsize==3)
      fix_to_similar_mode(FIX_TO_HIGHCOLOUR);
    else
      fix_to_similar_mode(FIX_TO_EIGHT_BIT);
  
    if(result==_PIC_OK)
      {
      numcols=ginfo.numcols;
      sgres=showgif(palette);
      free(theimage);
      free(palette);
      }
    }
  else
    image_palette=palette;
  }
while(show_dont_tell && cfg.repeat_timer && result==_PIC_OK && sgres==0);

return(result);
}


/* modes aren't counted if they're locked out */
int has_highcolour_mode()
{
int try;

if(cfg.hicolmodes) return(1);	/* forces positive response */

try=modematch(-1,-1,15);
if(try!=-1)
  if(cfg.mode_allowed[try])
    return(1);
    
try=modematch(-1,-1,16);
if(try!=-1)
  if(cfg.mode_allowed[try])
    return(1);
    
try=modematch(-1,-1,24);
if(try!=-1)
  if(cfg.mode_allowed[try])
    return(1);

return(0);
}


/* since we've already tested if it's possible, this can't fail */
fix_to_similar_mode(modetype)
int modetype;
{
int f,newmode,numpixels,pixdiff,newnp,newdiff,mode_ok;
vga_modeinfo *vminfo;


/* here's the plan;
 * - we try to keep the number of pixels as close as possible:
 *   as this will probably be quite noticeable, *this takes priority*.
 * - we then try to match any 8-bit modes for EIGHT_BIT, or match
 *   24/16/15-bit modes (in that order) for HIGHCOLOUR.
 * - of course, the mode has to be either 8 or 15/16/24-bit as reqd.
 */

newmode=-1; pixdiff=(1<<30);
numpixels=get_mode_width(curvgamode)*get_mode_height(curvgamode);

/* account for effect of 'virtual' */
if((curvgamode==7)||(curvgamode==8)) numpixels*=2;

/* we check 0 - 255 at the most */
for(f=0;f<256;f++)
  {
  if((!vga_hasmode(f))||(cfg.mode_allowed[f]==0)) continue;
  vminfo=vga_getmodeinfo(f);
  if(vminfo==NULL) continue;
  
  mode_ok=0;
  if(modetype==FIX_TO_EIGHT_BIT)
    {
    if(vminfo->colors==256) mode_ok=1;
    }
  else
    {
    if(vminfo->colors>256) mode_ok=1;
    }
  
  if(mode_ok==0) continue;
  
  newnp=(vminfo->width)*(vminfo->height);
  if((f==7)||(f==8)) newnp*=2;   /* account for effect of 'virtual' */
  newdiff=numpixels-newnp;
  if(newdiff<0) newdiff=(-newdiff);
  
  if(newdiff<=pixdiff)
    {
    newmode=f;
    pixdiff=newdiff;
    }
  }

/* we rely on the 15, 16, 24-bit mode number ordering to sort that
 * out for us (hence the '<=' above).
 */
curvgamode=newmode;
if((curvgamode==7)||(curvgamode==8)) virtual=1; else virtual=0;
}


aborted_file_cleanup()
{
switch(loading_file_type)
  {
  case _IS_GIF:
    aborted_file_gif_cleanup();
    break;
  case _IS_JPEG:
    aborted_file_jpeg_cleanup();
    break;
  case _IS_PNM:
    aborted_file_pnm_cleanup();
    break;
  case _IS_BMP:
    aborted_file_bmp_cleanup();
    break;
  case _IS_TGA:
    aborted_file_tga_cleanup();
    break;
#ifdef PNG_SUPPORT
  case _IS_PNG:
    aborted_file_png_cleanup();
    break;
#endif /* PNG_SUPPORT */
  }
}


is_this_file_jpeg()
{
return(isjpeg);
}


makerealpal()
{
int f;
int red=299, green=587, blue=114;	/* NTSC uses these */
int r,rlo,g,glo,b,blo;
int gval;

for(f=0;f<256;f++)
  {
  r=palr[f]>>2; rlo=palr[f]&3;
  g=palg[f]>>2; glo=palg[f]&3;
  b=palb[f]>>2; blo=palb[f]&3;

  if(cfg.gamma)
    {
    gval=(rlo*red+glo*green+blo*blue)/4;
    /* so gval is sub-value, 0<=gval<1000. (really <750... :( )
     * possible emulated sub-values are...
     * 0, 114 (b), 299 (r), 413 (r+b), 587 (g), 701 (g+b), 886 (r+g)
     */
  
    if(gval>=886)
      r++,g++;
    else if(gval>=701)
      g++,b++;
    else if(gval>=587)
      g++;
    else if(gval>=413)
      r++,b++;
    else if(gval>=299)
      r++;
    else if(gval>=114)
      b++;

    if(r>63) r=63; if(g>63) g=63; if(b>63) b=63;
    }

  palrgb[f*3  ]=palr64[f]=r;
  palrgb[f*3+1]=palg64[f]=g;
  palrgb[f*3+2]=palb64[f]=b;
  }
}


filterpal(palette)
byte *palette;
{
int f;

for(f=0;f<256;f++)   /* don't *really* need to know number of colours */
  {
  palr[f]=dimmer(contrastup(palette[    f]));
  palg[f]=dimmer(contrastup(palette[256+f]));
  palb[f]=dimmer(contrastup(palette[512+f]));
  }

makerealpal();
}


int dimmer(a)
int a;
{
a+=brightness;
if(a<0) a=0;
if(a>255) a=255;
return(a);
}


int contrastup(cp)
int cp;
{
float g;

g=(float)(cp);
g=128+(g-128)*contrast;
if(g<0)g=0;
if(g>255)g=255;
return((int)g);
}


int get_mode_width(vm)
int vm;
{
vga_modeinfo *vmi;

vmi=vga_getmodeinfo(vm);
return(vmi->width);
}

int get_mode_height(vm)
int vm;
{
vga_modeinfo *vmi;

vmi=vga_getmodeinfo(vm);
return(vmi->height);
}

int get_mode_pixelbytes(vm)
int vm;
{
vga_modeinfo *vmi;

vmi=vga_getmodeinfo(vm);
return(vmi->bytesperpixel);
}

int get_mode_numcols(vm)
int vm;
{
vga_modeinfo *vmi;

vmi=vga_getmodeinfo(vm);
return(vmi->colors);
}


graphicson()
{
int x;

vga_lockvc();
if((vga_hasmode(curvgamode))&&(cfg.mode_allowed[curvgamode]))
  vga_setmode(curvgamode);
else
  {
  /* we haven't? Aaargh!!! Ok, use 640x480 or 360x480 instead. */
  if((vga_hasmode(G640x480x256))&&(cfg.mode_allowed[G640x480x256]))
    {
    vga_setmode(curvgamode=G640x480x256);
    virtual=0;
    }
  else
    {
    if((vga_hasmode(G360x480x256))&&(cfg.mode_allowed[G360x480x256]))
      {
      vga_setmode(curvgamode=G360x480x256);
      virtual=1;
      }
    else
      {
      /* *must* have 320x200 (see rcfile.c for more info) */
      vga_setmode(curvgamode=G320x200x256);
      virtual=0;
      }
    }
  }

if(pixelsize!=1) gl_setcontextvga(curvgamode);

scrnwide=get_mode_width(curvgamode);
scrnhigh=get_mode_height(curvgamode);
scrnpixelsize=get_mode_pixelbytes(curvgamode);
scrncols=get_mode_numcols(curvgamode);

if(virtual)
  scrnwide*=2;
vga_setpalvec(0,256,palrgb);
if(vkludge)
  {
  for(x=0;x<65536;x++) vkcache[x].rgb=-1;
  /* these are fairly likely to crop up, so index them automatically */
  for(x=0;x<numcols;x++)
    closest(palr64[x],palg64[x],palb64[x]);
  }
vga_unlockvc();
}


graphicsoff()
{
/* we let zgv.c deal with this now, so this is just a placeholder */
}


/* returns 1 if ESC or 'x' was pressed, 0 if because of timeout when
 * using cfg.repeat_timer option.
 */
int showgif(palette)   /* global theimage,height,width implied */
byte *palette;
{
int opx,opy,px,py,quitshow,key,redraw;
int npx=0,npy=0;    /* equivalent to px,py in scaling>1 mode */
int f;
extern int x,y,mx,my, has_mouse, mfd;
int left_button=0, right_button=0, butstatep=0;
struct ms_event ev;

wait_for_foreground();

/* if image has less than 256 colours, we fill in the 64 greys so that
 * using the vkludge on, say, a mono file will look much better.
 */
if(numcols<256)
  {
  for(f=numcols;f<numcols+64;f++)
    palette[f]=palette[256+f]=palette[512+f]=((f-numcols)<<2);
  }

pic_incr=repeat_sig=0;
if(cfg.repeat_timer && !tagview_mode)
  {
  if(cfg.repeat_timer==-1)
    repeat_sig=2;	/* make it look (nearly) instant */
  else
    {
    signal(SIGALRM,repeat_sighandler);
    alarm(cfg.repeat_timer);
    }
  }

if(tagview_mode)
  {
  signal(SIGALRM,repeat_sighandler);
  alarm(cfg.tag_timer);
  }

quitshow=0;
filterpal(palette);
if(!cfg.repeat_timer || (cfg.repeat_timer && first_repeat))
  {
  px=py=0;
  graphicson();
  first_repeat=0;
  if(cfg.revert)
    {
    scaling=1;
    interp=0;
    }
  }
else
  if(cfg.repeat_timer)
    {
    px=saved_px;
    py=saved_py;
    }

redrawgif(px,py);

if(has_mouse && mfd != -1)
  {
  int c;
  while(read(mfd,&c,1)==1);	       /* clear the read buffer */
  }

while(!quitshow && repeat_sig!=1)
  {
  usleep(10000);
  key=readnbkey(zgv_ttyfd);
  opx=px; opy=py; redraw=0;

  if(has_mouse)
    {
    x = px, y = py;
    mx = width*scaling-1; my = height*scaling-1;
    /* using a loop like this is a nasty kludge, but it helps a bit */
    for(f=0;get_ms_event(&ev)>0 && f<16;f++)
      {
      px=x; py=y;
      if(ev.ev_butstate != butstatep)
        {
        butstatep = ev.ev_butstate;
        if(left_button && !(ev.ev_butstate & 4))
          key = RK_ENTER;
        if(right_button && !(ev.ev_butstate & 1))
          key = 127;
        left_button = ev.ev_butstate & 4;
        right_button = ev.ev_butstate & 1;
        }
      }
    }
      
  /* svga modes - note that F11=shift-F1 and F12=shift-F2 */
  if(((key>=RK_F1)&&(key<=RK_F12))||((key>=RK_SHIFT_F3)&&(key<=RK_SHIFT_F10)))
    /* the RK_F?'s are defined such that this sort of thing is ok to do: */
    /* (here comes the 'if' statement from hell...) */
    if((vga_hasmode(key-RK_F1+10))&&(cfg.mode_allowed[key-RK_F1+10])&&
       (((pixelsize==1)&&(get_mode_numcols(key-RK_F1+10)==256))||
        ((pixelsize==3)&&(get_mode_numcols(key-RK_F1+10)>256))))
      {
      curvgamode=key-RK_F1+10;   /* so F1 = mode 10 */
      virtual=0;
      graphicson();
      redraw=1;
      }

  if(key=='0' || (key>='5' && key<='8'))
    {
    if(key=='0')
      {
      virtual=(virtual==1)?0:1;
      graphicson();
      redraw=1;
      }
    else
      {
      if((vga_hasmode(key-48))&&(cfg.mode_allowed[key-48])&&
         (((pixelsize==1)&&(get_mode_numcols(key-48)==256))||
          ((pixelsize==3)&&(get_mode_numcols(key-48)>256))))
        {
        curvgamode=key-48;
        if((curvgamode==7)||(curvgamode==8)) virtual=1; else virtual=0;
        if(virtual) scaling=1;
        graphicson();
        redraw=1;
        }
      }
    }
  else
    switch(key)
      {
      case 's': case 'd':
        if(virtual) break;
        zoom=0;
        if(scaling<512)
          {
          int oldscale=scaling;
          
          scaling+=(key=='d')?scaling:1;
          if(scaling>512) scaling=512;
          samecentre(&px,&py,scaling,px,py,oldscale);
          redraw=1; /* no cls reqd. - must be bigger */
          }
        break;
      case 'S': case 'D':
        if(virtual) break;
        zoom=0;
        if(scaling>1)
          {
          int oldscale=scaling;
          
          scaling-=(key=='D')?scaling/2:1;
          if(scaling<1) scaling=1;
          samecentre(&px,&py,scaling,px,py,oldscale);
          redraw=1;
          if(width*scaling<scrnwide || height*scaling<scrnhigh) graphicson();
          }
        break;
      case 'n':
        if(zoom || scaling>1)
          {
          samecentre(&px,&py,1,px,py,scaling);
          scaling=1; zoom=0;
          redraw=1; graphicson(); 
          }
        scaling=1; redraw=1;
        if(scaling>1)  { scaling--; redraw=1; graphicson(); }
        if(scaling==1) { px=npx; py=npy; }
        break;
      case RK_HOME: case 'A'-0x40:
        px=py=0; break;
      case RK_END:  case 'E'-0x40:
        px=py=1<<30; break;
      case RK_PAGE_UP: case 'U'-0x40:
        py-=scrnhigh*9/10; break;
      case RK_PAGE_DOWN: case 'V'-0x40:
        py+=scrnhigh*9/10; break;
      case '-':
        px-=scrnwide*9/10; break;
      case '=':
        px+=scrnwide*9/10; break;
      case 'v': vkludge=((vkludge==1)?0:1); redraw=1; graphicson(); break;
      case ',': contrast-=0.05;
        filterpal(palette); vga_setpalvec(0,256,palrgb); break;
      case '.': contrast+=0.05;
        filterpal(palette); vga_setpalvec(0,256,palrgb); break;
      case '<': brightness-=10;
        filterpal(palette); vga_setpalvec(0,256,palrgb); break;
      case '>': brightness+=10;
        filterpal(palette); vga_setpalvec(0,256,palrgb); break;
      case '*': contrast=1.0; brightness=0;
        filterpal(palette); vga_setpalvec(0,256,palrgb); break;
      case 'q': case 'k': py-=MOVSTP; break;
      case 'a': case 'j': py+=MOVSTP; break;
      case 'o': case 'h': px-=MOVSTP; break;
      case 'p': case 'l': px+=MOVSTP; break;
      case 'Q': case 'K': case RK_CURSOR_UP:    py-=BIGSTP; break;
      case 'A': case 'J': case RK_CURSOR_DOWN:  py+=BIGSTP; break;
      case 'O': case 'H': case RK_CURSOR_LEFT:  px-=BIGSTP; break;
      case 'P': case 'L': case RK_CURSOR_RIGHT: px+=BIGSTP; break;
      case 'm': fx_mirror(); px=py=0; redraw=1; break;
      case 'f': fx_flip();   px=py=0; redraw=1; break;
      case 'r': fx_rot();    px=py=0; redraw=1; break;
      case 'R': fx_rot(); fx_flip(); fx_mirror(); px=py=0; redraw=1; break;
      case 'z':
        zoom=(!zoom); scaling=redraw=1; px=py=0; graphicson(); break;
      case 'i':
        interp=(!interp); redraw=1; break;
      case '1':
        inextpix=(inextpix==1)?2:1;
        redraw=1;
        break;
      case 'c':
        cfg.centreflag=(!cfg.centreflag);
        redraw=1; px=py=0; graphicson(); break;
      case '?':
        showhelp(zgv_ttyfd,"- KEYS FOR FILE DISPLAY SCREEN -",viewerhelp);
        /* falls through to 'refresh screen' */
      case 12: case 18:     /* 12,18 = Ctrl-L, Ctrl-R */
        graphicson(); redraw=1; break;
      case 'N'-0x40:
        pic_incr=1; quitshow=1; break;
      case 'P'-0x40:
        pic_incr=-1; quitshow=1; break;
      case RK_ENTER:
	pic_incr=1; quitshow=2; break;
      case ' ':
	pic_incr=1; quitshow=3; break;
       case 127: case 8:
	pic_incr=-1; quitshow=2; break;
      case RK_ESC: case 'x':
        quitshow=1;
      }
      
  if(!zoom)
    {
    int swidth=width*scaling,sheight=height*scaling;
    
    if(sheight<=scrnhigh)
      py=0;
    else
      if(sheight-py<scrnhigh) py=sheight-scrnhigh;
    if(swidth<=scrnwide)
      px=0;
    else
      if(swidth-px<scrnwide) px=swidth-scrnwide;
    if(px<0) px=0;
    if(py<0) py=0;
    if(scaling>1)
      {
      npx=px/scaling;
      npy=py/scaling;
      }
    }
  else
    px=py=npx=npy=0;
    
  if((redraw)||(opx!=px)||(opy!=py)) redrawgif(px,py,npx,npy);
  if(repeat_sig==2) repeat_sig--;
  }
graphicsoff();

if(cfg.selecting)
  show_dimensions(px,py,scaling);

if(cfg.repeat_timer || tagview_mode)
  {
  alarm(0);
  saved_px=px;
  saved_py=py;
  if(quitshow==1) tagview_mode=0;
  }
  
return(quitshow);
}


void repeat_sighandler()
{
repeat_sig=1;
}


/* this routine is getting ridiculous */
redrawgif(px,py,npx,npy)
int px,py,npx,npy;
{
int x,y,xdim;
int x_add,y_add;
byte *realline,*ptr;

if(zoom)
  drawzoomedgif(height,width,theimage);
else
  {
  int swidth=width*scaling,sheight=height*scaling;
  
  /* draw non-zoomed pic */
  x_add=y_add=0;	/* these control the centering */
  if(cfg.centreflag)
    {
    if(swidth<scrnwide)
      x_add=(scrnwide-swidth)>>1;
    
    if(sheight<scrnhigh)
      y_add=(scrnhigh-sheight)>>1;
      
    if(virtual) x_add>>=1;
    }
  
  if(swidth-px<scrnwide) xdim=swidth-px; else xdim=scrnwide;
  if((py>=sheight)||(px>=swidth)) return(0);
  /* hopefully the following is fairly quick as fewer ifs... ? */
  if((virtual)&&(pixelsize==1))
    {
    if((realline=calloc(1,scrnwide))==NULL) return(0);
    for(y=0;(y<sheight-py)&&(y<scrnhigh);y++)
      {
      for(x=0;x<xdim;x++)
        *(realline+(x>>1))=getvpix(px,py,x,y);
      vga_drawscansegment(realline,x_add,y+y_add,xdim>>1);
      }
    free(realline);
    }
  else if(scaling>1)
    {
    /* Better grab your Joo Janta 200 Super-Chromatic Peril Sensitive
     * Sunglasses (both pairs) for this next bit...
     */
    int cdown=-1,i,pxx,pyy,pyym,ifres2;
    int a1,a2,a3,a4,in_rg,in_dn,in_dr;
    int wp=width*pixelsize,sci=scaling*inextpix;
    int scaleincr=0,subpix_xpos,subpix_ypos,sxmulsiz,symulsiz,simulsiz=0;
    int sisize=0,sis2=0;
    unsigned char *ptr1,*ptr2,*ptr3,*ptr4;
    unsigned char *src,*dst,*nextline;
    
    /* this doesn't allow for 'virtual' yet! */
    if((realline=calloc(pixelsize,scrnwide))==NULL) return(0);
    if(pixelsize==3)
    if((nextline=calloc(pixelsize,scrnwide))==NULL) return(0);
    
    if(interp)
      {
      sisize=0;
      while(sisize<256) sisize+=scaling;
      scaleincr=sisize/scaling;
      simulsiz=scaleincr*sisize;
      sis2=sisize*sisize;
      }
    
    for(y=0,pyy=py;(y<sheight-py)&&(y<scrnhigh);y++,pyy++)
      {
      /* this is horribly slow... :-( */
      if(cdown<=0 || interp)
        {
        src=theimage+pixelsize*(pyy/scaling)*width;
        dst=realline;
        if(pixelsize==1)
          for(x=0;(x<swidth-px)&&(x<scrnwide);x++)
            *dst++=*(src+(px+x)/scaling);
        else if(interp==0)
          /* normal */
          for(x=0;(x<swidth-px)&&(x<scrnwide);x++)
            {
            ptr=src+((px+x)/scaling)*pixelsize;
            *dst++=*ptr++; *dst++=*ptr++; *dst++=*ptr;
            }
        else
          {
          /* interpolated */
          
          /* This has been hacked into unreadability in an attempt to get it
           * as fast as possible.
           * It's still really slow. :-(
           */

          in_rg=inextpix*3;
          in_dn=inextpix*wp;
          in_dr=in_dn+in_rg;
          pyym=pyy%scaling;
          ifres2=(pyy>=sheight-sci);
          subpix_ypos=(pyy%scaling)*scaleincr;
          subpix_xpos=(px%scaling)*scaleincr;  /* yes px not pxx */
          
          ptr1=ptr3=src+(px/scaling)*3;
          ptr2=ptr4=ptr1+in_rg;
          if(pyy<sheight-sci)
            {
            ptr3=ptr1+in_dn;
            ptr4=ptr1+in_dr;
            }
          
          symulsiz=sisize*subpix_ypos;
          sxmulsiz=sisize*subpix_xpos;
          
          for(x=0,pxx=px;x<swidth-px && x<scrnwide;x++,pxx++)
            {
            a3=symulsiz-(a4=subpix_xpos*subpix_ypos);
            a2=sxmulsiz-a4;
            a1=sis2-sxmulsiz-symulsiz+a4;

            for(i=0;i<3;i++)
              *dst++=(ptr1[i]*a1+ptr2[i]*a2+
                      ptr3[i]*a3+ptr4[i]*a4)/sis2;
              
            subpix_xpos+=scaleincr;
            sxmulsiz+=simulsiz;
            if(subpix_xpos>=sisize)
              {
              subpix_xpos=sxmulsiz=0;
              ptr1+=3; ptr2+=3; ptr3+=3; ptr4+=3;
              }

            if(pxx>=swidth-sci)
              {
              ptr2-=3;
              ptr4-=3;
              }
            }
          }
              
        cdown=(cdown==-1)?(scaling-(py%scaling)):scaling;
        }
      
      cdown--;
      
      if(scrnpixelsize!=3)
        eventuallydrawscansegment(realline,x_add,y+y_add,scrnwide*pixelsize);
      else
        gl_putbox(x_add,y+y_add,scrnwide,1,realline);
      }
    free(realline);
    }
  else
    {
    switch(scrnpixelsize)
      {
      case 3:
        gl_putboxpart(x_add,y_add,(scrnwide<width)?scrnwide:width,
	        (scrnhigh<height)?scrnhigh:height,width,height,theimage,px,py);
        break;
      
      default:
        /* for 320x200 (I think), 320x240, 320x400, 360x480 we need the
         * length to be a multiple of 8. :(  We allocate an image-width
         * line (blanked), and display via that.
         */
        if(curvgamode==G320x200x256 || curvgamode==G320x240x256 ||
           curvgamode==G320x400x256 || curvgamode==G360x480x256)
          {
          unsigned char *hack;
          
          if((hack=malloc(scrnwide))==NULL) return(0);
          for(y=0;(y<height-py)&&(y<scrnhigh);y++)
            {
            /* assume pixelsize==1, because it must be */
            memcpy(hack,theimage+((py+y)*width+px),xdim);
            vga_drawscansegment(hack,x_add,y+y_add,xdim);
            }
          free(hack);	/* bit like zgv, really :-) */
          }
        else
          for(y=0;(y<height-py)&&(y<scrnhigh);y++)
            eventuallydrawscansegment(theimage+pixelsize*((py+y)*width+px),
            	x_add,y+y_add,xdim*pixelsize);
        break;
      }
    }
  }
return(0);
}


eventuallydrawscansegment(ptr,x,y,len)
byte *ptr;
int x,y,len;
{
unsigned short buf[2048];

if(scrnpixelsize!=2)
  vga_drawscansegment(ptr,x,y,len);
else
  {
  int f;
  unsigned short *bufref;

  if(scrncols==32768)
    for(f=0,bufref=buf;f<len;f+=3,bufref++)
      *bufref=GET15BITCOLOUR(ptr[f+2],ptr[f+1],ptr[f]);
  else
    for(f=0,bufref=buf;f<len;f+=3,bufref++)
      *bufref=GET16BITCOLOUR(ptr[f+2],ptr[f+1],ptr[f]);

  vga_drawscansegment((byte *)buf,x,y,(len/3)<<1);
  }
}


int getvpix(px,py,x,y)
int px,py,x,y;
{
int p1,p2,r,g,b;

if((vkludge)&&(pixelsize==1))
  {
  p1=*(theimage+(py+y)*width+px+x);
  if(px+x+1>=width) return(p1);
  p2=*(theimage+(py+y)*width+px+x+1);
  r=(palr64[p1]+palr64[p2])>>1;
  g=(palg64[p1]+palg64[p2])>>1;
  b=(palb64[p1]+palb64[p2])>>1;
  return(closest(r,g,b));
  }
else
  return(*(theimage+pixelsize*((py+y)*width+px+x)));
}


/* this routine is nasty writ big, but about as quick as I can manage */
drawzoomedgif()
{
register int a,b,x,yp,yw;
long y,sw,sh,lastyp;
int xoff,yoff;
int x_add,y_add;	/* for centering */
int c=0,cr=0,cg=0,cb=0,pixwide,pixhigh;
int bigimage;
byte *rline;
int tmp1,tmp2,tr,tg,tb,tn;

if((rline=malloc(scrnwide*pixelsize))==NULL) return(0);
for(x=0;x<scrnwide*pixelsize;x++)
  rline[x]=0;

/* try landscapey */
sw=scrnwide; sh=(int)((scrnwide*((long)height))/((long)width));
if(sh>scrnhigh)
  /* no, oh well portraity then */
  { sh=scrnhigh; sw=(int)((scrnhigh*((long)width))/((long)height)); }

/* so now our zoomed image will be sw x sh */

/* fix centering if needed */
x_add=y_add=0;
if(cfg.centreflag)
  {
  if(sw<scrnwide)
    x_add=(scrnwide-sw)>>1;
  
  if(sh<scrnhigh)
    y_add=(scrnhigh-sh)>>1;
  
  if(virtual) x_add>>=1;
  }

bigimage=(width>sw)?1:0;   /* 1 if image has been reduced, 0 if made bigger */
if(bigimage)
  /* it's been reduced - easy, just make 'em fit in less space */
  {
  if(virtual) sw>>=1;
  lastyp=-1;
  graphicson();
  pixhigh=(int)(((float)height)/((float)sh)+0.5);
  pixwide=(int)(((float)width)/((float)sw)+0.5);
  pixhigh++;
  pixwide++;
  for(y=0;y<height;y++)
    {
    yp=(y*sh)/height;
    if(yp!=lastyp)
      {
      yw=y*width;
      if(vkludge) /* we try to resample a bit. get that pentium RSN :) */
        {
        for(x=0;x<width;x++,yw++)
          {
          tr=tg=tb=tn=0;
          for(b=0;(b<pixhigh)&&(y+b<height);b++)
            for(a=0;(a<pixwide)&&(x+a<width);a++)
              {
              if(scrncols<=256)
                {
                tmp2=*(theimage+yw+a+b*width);
                tr+=palr64[tmp2];
                tg+=palg64[tmp2];
                tb+=palb64[tmp2];
                }
              else
                {
                tb+=*(theimage+3*(yw+a+b*width));
                tg+=*(theimage+3*(yw+a+b*width)+1);
                tr+=*(theimage+3*(yw+a+b*width)+2);
                }  
              tn++;
              }
          tr/=tn; tg/=tn; tb/=tn;
          if(scrncols<=256)
            rline[(x*sw)/width]=closest(tr,tg,tb);
          else
            {
            tmp1=(x*sw)/width*pixelsize;
            rline[tmp1]=tb;
            rline[tmp1+1]=tg;
            rline[tmp1+2]=tr;
            }
          }
        }
      else
        for(x=0;x<width;x++,yw++)
          {
          if(scrncols<=256)
            rline[(x*sw)/width]=*(theimage+yw);
          else
            memcpy(rline+(x*sw)/width*3,theimage+yw*3,3);
          }
      eventuallydrawscansegment(rline,x_add,yp+y_add,scrnwide*pixelsize);
      lastyp=yp;
      }
    }
  free(rline);
  }
else
  /* well, we need to fill in the gaps because it's been made bigger. */
  {
    /* a pixel from the original is now pixwide x pixhigh */
  pixwide=(int) ( (((float)sw)/((float)width )) +1.0 );
  pixhigh=(int) ( (((float)sh)/((float)height)) +1.0 );

  free(rline);  /* get rid of that... */
    /* and get a screen's worth... hahahahahahahahaha er, sorry. */
  if((rline=malloc(pixelsize*scrnwide*(scrnhigh+pixhigh)))==NULL) return(0);
    /* notice that we allow for a screwy 'pixhigh' value to avoid
       any nasty old segfaults! oh, now we wipe it: */
  for(x=0;x<scrnwide*scrnhigh*pixelsize;x++) rline[x]=0;
  
  if(virtual)
    sw>>=1;   /* leave pixwide though as it *might* miss with >> */
  for(y=0;y<height;y++)
    {
    yoff=(y*sh)/height;
    yw=y*width*pixelsize;
    for(x=0;x<width;x++,yw+=pixelsize)
      {
      if(scrncols<=256)
        c=*(theimage+yw);
      else
        {
        cb=*(theimage+yw); cg=*(theimage+yw+1); cr=*(theimage+yw+2);
        }
      xoff=(x*sw)/width;
      for(b=0;b<pixhigh && yoff+b<scrnhigh;b++)
        for(a=0;a<pixwide && xoff+a<scrnwide;a++)
          if(scrncols<=256)
            *(rline+(yoff+b)*scrnwide+xoff+a)=c;
          else
            {
            tmp1=3*((yoff+b)*scrnwide+xoff+a);
            *(rline+tmp1)=cb;
            *(rline+tmp1+1)=cg;
            *(rline+tmp1+2)=cr;
            }
      }
    eventuallydrawscansegment(rline+pixelsize*y*scrnwide,
    	x_add,y+y_add,scrnwide*pixelsize); /* really pessimistic */
    }
  for(;y<scrnhigh;y++)          /* and do the rest */
    eventuallydrawscansegment(rline+pixelsize*y*scrnwide,
    				x_add,y+y_add,scrnwide*pixelsize);
  free(rline);
  }
return(0);
}


fx_mirror()
{
unsigned char *src,*dst;
byte *tmp;
int x,y;

tmp=malloc(width*pixelsize);
if(tmp==NULL) return(0);

if(pixelsize==1)
  {
  for(y=0;y<height;y++)
    {
    dst=tmp;
    src=theimage+y*width+width-1;
    for(x=0;x<width;x++)
      *dst++=*src--;
    memcpy(src+1,tmp,width);
    }
  }
else
  {
  /* we know pixelsize must be 3 then, so take advantage of that */
  
  for(y=0;y<height;y++)
    {
    dst=tmp;
    src=theimage+3*(y*width+width-1);
    for(x=0;x<width;x++)
      {
      *dst++=*src++;
      *dst++=*src++;
      *dst++=*src;
      src-=5;
      }
    memcpy(src+3,tmp,width*3);
    }
  }

free(tmp);
return(0);
}


fx_flip()
{
unsigned char *tmp,*ptr1,*ptr2;
int y,y2,wp=width*pixelsize;

tmp=malloc(wp);
if(tmp==NULL) return(0);

ptr1=theimage;
ptr2=theimage+(height-1)*wp;

for(y=0,y2=height-1;y<height/2;y++,y2--,ptr1+=wp,ptr2-=wp)
  {
  memcpy(tmp,ptr1,wp);
  memcpy(ptr1,ptr2,wp);
  memcpy(ptr2,tmp,wp);
  }

free(tmp);
return(0);
}


fx_rot()
{
unsigned char *tmp,*tmp2=NULL,*ptr1,*ptr2=NULL;
int x,y,y2,wp=width*pixelsize,hp=height*pixelsize;
int blockup=4;

tmp=malloc(wp*height);
if(pixelsize==3) tmp2=malloc(wp*blockup);
if(tmp==NULL || (pixelsize==3 && tmp2==NULL)) return(0);

if(pixelsize==1)
  for(y=0,y2=hp-1;y<height;y++,y2--)
    {
    ptr1=tmp+y2; ptr2=theimage+wp*y;
    for(x=0;x<width;x++,ptr1+=hp)
      *ptr1=*ptr2++;
    }
else
  {
  int hpm3=hp-3;
  
  for(y=0,y2=height-1;y<height;y++,y2--)
    {
    ptr1=tmp+y2*3;
    if(y%blockup==0)
      {
      memcpy(tmp2,theimage+wp*y,wp*((y2<blockup)?y2:blockup));
      ptr2=tmp2;
      }
    for(x=0;x<width;x++,ptr1+=hpm3)
      {
      *ptr1++=*ptr2++;
      *ptr1++=*ptr2++;
      *ptr1++=*ptr2++;
      }
    }
  }
  
x=height;y=width;
width=x;height=y;

if(pixelsize==3) free(tmp2);
free(theimage);
theimage=tmp;
graphicson(); /* clear screen 'cos image diff. size */
return(0);
}


/* rgb values must be 0..63 */
/* because of course, only 6 bits of the rgb values actually count on VGA */
int closest(r,g,b)
int r,g,b;
{
int idx,rgb;
byte *pr,*pg,*pb;
register byte distnum;
register int xr,xg,xb,dist,distquan,f,checknumcols;

rgb=((b<<12)|(g<<6)|r);
idx=rgb&0xffff;
if(vkcache[idx].rgb==rgb)
  return(vkcache[idx].col);
distnum=0;
distquan=20000; /* standard arbitrary bignum */
/* if numcols=256 we do 0-255, otherwise 0-numcols+63 */
checknumcols=((numcols==256)?256:numcols+64);
for(pr=palr64,pg=palg64,pb=palb64,f=0;f<checknumcols;f++,pr++,pg++,pb++)
  {
  xr=(r-*pr);
  xg=(g-*pg);
  xb=(b-*pb);
  if((dist=xr*xr+xg*xg+xb*xb)<distquan)
    {
    distnum=f;
    distquan=dist;
    if(dist==0) break;  /* premature exit if it can't get any better */
    }
  }
vkcache[idx].rgb=rgb;
return((int)(vkcache[idx].col=distnum));
}


samecentre(ppx,ppy,newscale,oldpx,oldpy,oldscale)
int *ppx,*ppy,newscale,oldpx,oldpy,oldscale;
{
int xa,ya,sw,sh;

/* even if the centre flag is off, we still need to do this */
xa=ya=0;
sw=oldscale*width;
sh=oldscale*height;
if(sw<scrnwide) xa=(scrnwide-sw)>>1;
if(sh<scrnhigh) ya=(scrnhigh-sh)>>1;  
if(virtual) xa>>=1;

/* finds centre of old screen, and makes it centre of new one */
*ppx=(oldpx-xa+(scrnwide>>1))*newscale/oldscale;
*ppy=(oldpy-ya+(scrnhigh>>1))*newscale/oldscale;

xa=ya=0;
sw=newscale*width;
sh=newscale*height;
if(sw<scrnwide) xa=(scrnwide-sw)>>1;
if(sh<scrnhigh) ya=(scrnhigh-sh)>>1;  
if(virtual) xa>>=1;

*ppx-=(scrnwide>>1)+xa;
*ppy-=(scrnhigh>>1)+ya;
}


/* this gets run on picture exit when cfg.selecting is true.
 * the output is designed to be put straight into a pnmcut command-line.
 */
show_dimensions(px,py,scaling)
int px,py,scaling;
{
int x,y,w,h;

x=px/scaling; y=py/scaling;
if(width*scaling<scrnwide)  w=width;  else w=scrnwide/scaling;
if(height*scaling<scrnhigh) h=height; else h=scrnhigh/scaling;
printf("%4d %4d %4d %4d\n",x,y,w,h);
}
