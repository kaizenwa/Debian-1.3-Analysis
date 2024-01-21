/* This is part of tnview, a dvi previewer. (c) 1995 Thomas Moor         */
/*                                                                       */
/* This program may be used without any warranty. It may be modified and */
/* distributed without any restrictions.                                 */

#include <stdio.h>
#include <math.h>
#include <vga.h>
#include <vgagl.h>
#include <vgamouse.h>
#include <sys/time.h>

#include "defsx.h" 
#include "writevga.h"                       

/* from the file globals.c we need ... */
extern int truevgaxdim,truevgaydim; 
extern int vgaxdim, vgaydim;
extern int vgastatuslen, vgastatushight, vgamaxstatuslines, vgastatuslines;

/* from the file subs.c w need ... */
extern void pfprot(char*, ...);
extern void pfverb(char*, ...);
extern void allocmem(void*,long int);
extern void freemem(void*);


/* globals used by my painting stuff */  
static GraphicsContext offscreen, physicalscreen;
static uchar* offscreenmem;
static int ofscx1, ofscy1, ofscx2, ofscy2;
static int thegraphmode, oldgraphmode;
static int fontw, fonth, fonts;

/* Function prototypes for ugly font-scaling */
void preparefont(void); 
void closefont(void);



int (*flip)(void)=vga_flip; 

/* #define DEBUGVGA */

/* debugging makros to report pixel warnings */
/* #define DEBUGPIXEL */

#ifdef DEBUGPIXEL
#define CPT(apt,mess) \
     if(apt<VBUF || apt>=VBUF+vgaxdim*vgaydim ) \
     {pfprot("(ctp %s)\n",mess); exit(1);}

#define CXY(mess) \
      if(x2<=x1 || y2<=y1) \
        {pfprot("(cxy %s)\n",mess);}

#define CXW(mess) \
      if(w<0 || h<0) \
        {pfprot("(cxw %s)\n",mess);}\

#else /* no debugging */
#define CPT(apt,mess) 
#define CXW(mess) 
#define CXY(mess)
#endif





void vgaclose(void) {
    vga_setmode(oldgraphmode);
    gl_freecontext(&offscreen);      /* does not hurt if not allocated ? */
#ifndef MECK
    gl_freecontext(&physicalscreen); /* does not hurt if not allocated ? */
#endif
    closefont();
}

void vgaerror(char* mssg)
{
   pfprot("\nfatal error: display: %s\n",mssg);   
   vgaclose();
   exit(1);
}

void vgaopen(void) {  
  vga_init();
}

#ifdef MECK
#define setpalcol vga_setpalette
#else
#define setpalcol gl_setpalettecolor
#endif

void setpalette(void) {
  int i,gl;

  for(i=0;i<COLORS_PER_GREY;i++) {
    gl=(i==0) ? 0: 63 * exp(GAMMA*log(i/(float) (COLORS_PER_GREY-1))) ;
    setpalcol(COLORS_PER_GREY-1-i,gl,gl,gl);
  }
#ifdef MECK
    setpalcol(8,PALDRED);   
    setpalcol(9,PALDGREEN);   
    setpalcol(10,PALDBLUE);   
    setpalcol(11,0,0,0);   /* not used */
    setpalcol(12,PALLRED);   
    setpalcol(13,PALLGREEN);  
    setpalcol(14,PALLBLUE);   
    setpalcol(15,PALLGREY);  
#else
    setpalcol(SETPALTEXTCOL);           
    setpalcol(SETPALTEXTBACKCOL);      
    setpalcol(SETPALBORDERCOL);   
    setpalcol(SETPALFRAMECOL);    
    setpalcol(SETPALRECTCOL);     
    setpalcol(SETPALTFMCOL);      
    setpalcol(SETPALHREFCOL);     
    setpalcol(SETPALMIXEDCOL);     
    setpalcol(SETPALMARKSCOL);     
    setpalcol(SETPALMARKHCOL);     
    setpalcol(SETPALMARKDCOL);     
    setpalcol(SETPALFOUNDCOL);     
#endif
}

void mysetclip(int x1, int y1, int x2, int y2) {
   if(x1<0) x1=0; if(y1<0) y1=0;
   if(x2>vgaxdim-1) x2=vgaxdim-1;
   if(y2>vgaydim-1) y2=vgaydim-1;
   if(x2<x1 || y2<y1) {
      CXY("clip") 
      x1=0;x2=0;x2=0;y1=0; /* dirty point? should not happen anyway */
   } 
   ofscx1=x1; 
   ofscy1=y1; 
   ofscx2=x2; 
   ofscy2=y2; 
   gl_enableclipping();
   gl_setclippingwindow(x1,y1,x2,y2);
}

void mysetclipof(void) {
   ofscx1=0; 
   ofscy1=0; 
   ofscx2=truevgaxdim-1; 
   ofscy2=truevgaydim-1; 
   gl_setclippingwindow(ofscx1,ofscy1,ofscx2,ofscy2);
   gl_enableclipping();
}


void myupdate(int x1, int y1, int x2, int y2) {
   if(x1<0) x1=0; if(y1<0) y1=0;
   if(x2>truevgaxdim-1) x2=truevgaxdim-1;
   if(y2>truevgaydim-1) y2=truevgaydim-1;
   if(x2<x1 || y2<y1) return; 

#ifndef MECK
   gl_setcontext(&physicalscreen);
   gl_copyboxfromcontext(&offscreen,x1,y1,x2-x1+1,y2-y1+1,x1,y1);
   gl_setcontext(&offscreen);
   gl_enableclipping();
#else
   x1=x1 & 0xfff8; x2=x2 | 0x0007;  /* make divisible by 8 */
   /* pfprot("(myupdate: vga_copytoplanar16 x1 %d y1 %d x2 %d y2 %d)",
            x1,y1,x2,y2); */
   vga_copytoplanar16(VBUF+y1*BYTEWIDTH+x1,              BYTEWIDTH, 
                      (y1*truevgaxdim+x1)>>3,            truevgaxdim>>3,
                      x2-x1+1, y2-y1+1);
#endif
}



void vgagraph(void) {
  int n;
  uchar g;
  oldgraphmode=vga_getcurrentmode();
#ifndef MECK
  n=0;
  if(vgaxdim==320 && vgaydim==200) n=5;
  if(vgaxdim==640 && vgaydim==480) n=10;
  if(vgaxdim==800 && vgaydim==600) n=11;
  if(vgaxdim==1024 && vgaydim==768) n=12;
  if (n==0) { 
    n=vga_getdefaultmode();
    if(n!=5 && n!=10 && n!=10 && n!=11 && n!=12) { 
      pfprot("\n\
warning: display: using default option -d640x480
         you may select one out of -d320x200 -d640x480 -d800x600 -d1024x768,
         or set the environment variable GSVGAMODE, depending on your svgalib
         setup resp. hardware\n"); 
      n=10; 
    }
  }
  if (!vga_hasmode(n)) 
    vgaerror("no such vga-mode on this machine");
  thegraphmode=n;
  if(n==5)  {truevgaxdim=320; truevgaydim=200; vgaxdim=320; vgaydim=200;}
  if(n==10 || n==4) 
            {truevgaxdim=640; truevgaydim=480; vgaxdim=640; vgaydim=480;}
  if(n==11) {truevgaxdim=800; truevgaydim=600; vgaxdim=800; vgaydim=600;}
  if(n==12) {truevgaxdim=1024; truevgaydim=768; vgaxdim=1024; vgaydim=768;}
#else    /* mode 4 only */
  pfprot("\n\
warning: display: 640x480x16 only, due to compiletime-option.
          recompile, to use a wide range of 256-color-modes,
          depending on your svgalib-setup resp. hardware\n"); 
  if (!vga_hasmode(4)) 
    vgaerror("no such vga-mode on this machine");
  thegraphmode=4;
  allocmem(&offscreenmem,640L*480L);
  truevgaxdim=640; truevgaydim=480; vgaxdim=640; vgaydim=480;
#endif

#ifdef HASMOUSE
  vga_setmousesupport(1);
#endif
  /* init physical screen */
  vga_setmode(thegraphmode);
#ifndef MECK
  gl_setcontextvga(thegraphmode); 
  gl_getcontext(&physicalscreen);
  gl_disableclipping();
  setpalette();
#else
  setpalette();
#endif
  /* init offscreen, always 256-colors */
#ifndef MECK
  if(gl_setcontextvgavirtual(thegraphmode) ==-1) 
      vgaerror("libgl: can't allocate virtual context");
  gl_getcontext(&offscreen);
#else
  /* 640x480 is only supported */
  gl_setcontext(&offscreen);
  gl_setcontextvirtual(640,480,1,8,offscreenmem);
#endif
  mysetclip(0,0,vgaxdim-1,vgaydim-1);
  preparefont();
  vgastatushight=fonth+fonts;
  vgamaxstatuslines=truevgaydim/vgastatushight;
  vgastatuslen=(truevgaxdim-1)/fontw;
#ifdef HASMOUSE
  mouse_setxrange(-vgaxdim,2*vgaxdim);
  mouse_setyrange(-vgaydim,2*vgaydim);
  mouse_setwrap(MOUSE_NOWRAP);
#endif 
}

int vgasetstatuslines(int n) {
  vgastatuslines=MAX(0,MIN(vgamaxstatuslines,n));
  vgaydim= truevgaydim- vgastatuslines* vgastatushight;
  vgaxdim= truevgaxdim;

  mysetclipof();   /* not clipped !! */
  if(vgastatuslines!=vgamaxstatuslines)
  gl_fillbox(0,vgaydim,truevgaxdim,vgastatuslines*vgastatushight,TEXTBACKCOL);
  else
  gl_fillbox(0,0,truevgaxdim,truevgaydim,TEXTBACKCOL);
  myupdate(0,0,vgaxdim-1,truevgaydim-1);
  mysetclip(0,0,vgaxdim-1,vgaydim-1);
  return(vgastatuslines);
}

void vgaupdatestatus(void) {
  mysetclipof();
  myupdate(0, vgaydim,truevgaxdim-1,truevgaydim-1); 
  mysetclip(0,0,vgaxdim-1,vgaydim-1);
}


void vgadrawstatus(char chr, int line, int pos) {
  char ocs[2]={0,0};
  if(line>=vgastatuslines || line <0) return;
  if(pos >=vgastatuslen || pos <0) return;
  mysetclipof();       /* not clipped */
  ocs[0]=chr;
  gl_writen(pos*fontw+1, vgaydim +(line+1)* vgastatushight-fonth,1,ocs);
  mysetclip(0,0,vgaxdim-1,vgaydim-1);
  /*pfprot("(vgadrawstatus [%s] l %d p %d l %d)",ocs,line,pos);*/
}


void vgaxscroll(short dx) {  
  if(abs(dx) >= vgaxdim) return;
  if(dx<0) 
    gl_copybox(-dx,0,vgaxdim+dx,vgaydim,0,0);
  else
    gl_copybox(0,0,vgaxdim-dx,vgaydim, dx,0);
}
   
void vgayscroll(short dy) {
  if(abs(dy) >= vgaydim) return;   
  if(dy<0) 
    gl_copybox(0,-dy,vgaxdim,vgaydim+dy,0,0);
  else
    gl_copybox(0,0,vgaxdim, vgaydim-dy, 0,dy);
}   


uchar vgagetchar(int block) {  /* blocking not implemented yet */
  uchar input,next1,next2,next3;
  uchar com=KEYNOP;

  input = vga_getkey();
  if(input >= ' ' && input < 128) com= input;
  else switch(input) {
    case 9: com=KEYTAB; break;
    case 10: com=KEYRET; break;
    case 27:                           /* escape */
      next1=0; next2=0; next3=0;
      next1=vga_getkey();
      switch(next1) {
      case 91:  
        next2=vga_getkey();
        switch(next2) {
        case '6': com= KEYNEXT;   break; 
        case '5': com= KEYPREV;   break;
        case 'A': com= KEYUP;     break;
        case 'B': com= KEYDOWN;   break;
        case 'D': com= KEYLEFT;   break;
        case 'C': com= KEYRIGHT;  break;
        case 'G': com= KEYCENTER; break;
        case '1': com= KEYHOME;   break;
        case '4': com= KEYEND;    break;
        } 
      break;
      case 0: case 27: com= KEYESC; break; /* allow wild escape abuse */
      default:
      }
    break;
  }  
  /*if(input !=0 && input != 255)
    fprintf(prot,"(vgacommeand: %d %d %d %d)",input,next1,next2,next3);*/
  while(input !=0 && input != 255) input=vga_getkey();
  return(com);
}

int omx, omy;

void vgasetmouse(int x, int y) {
  omx=x;
  omy=y;
#ifdef HASMOUSE
  mouse_setposition(x,y);
#endif
}  


void vgagetmouse(int *x, int *y, int *left, int* right) {
  int button,nmx,nmy;
#ifdef HASMOUSE
  mouse_update();
  nmx = mouse_getx();
  nmy = mouse_gety();
  if(abs(nmx-omx) > MOUSESPEED) *x=omx+MOUSESPEED*(nmx-omx);
  else *x=nmx;
  if(abs(nmy-omy) > MOUSESPEED) *y=omy+MOUSESPEED*(nmy-omy);
  else *y=nmy;
  vgasetmouse(*x,*y);
  button = mouse_getbutton();
  *left= (button & MOUSE_LEFTBUTTON);
  *right=(button & MOUSE_RIGHTBUTTON);
#endif
#ifndef HASMOUSE
  *x=omx;
  *y=omy;
  *left=0;
  *right=0;
#endif
}

void vgascreen(int son) {  
}



/**************************************************************************/
/*
   now the pixel copy stuff. 
   destination is allways the offscreen context  
*/


void mycopybitsbw(int tx, int ty, int sw, int sh, void* src) {
/* this is with self made clipping. no good  ? */
/* but it will do for all kind of BMUNIT       */
 
  int j,i,supi,supj,mini,minj,sx, skip;
  BMUNIT *srow, *sbmu;
  BMUNIT premask, firstpremask;
  register BMUNIT data, mask;
  short sbmu_wide;
  char *ydest, *xdest;

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
  ydest=VBUF+ty*BYTEWIDTH;
  /* fprintf(prot,"clipped: tx %d ty %d mi %d si %d mj %d sj %d \n",
     tx,ty,mini,supi,minj,supj); */
  
  sbmu_wide= ROUNDUP(sw,BITS_PER_BMUNIT);
  skip= minj & (BITS_PER_BMUNIT-1);
  sx= minj >> BITS_LOG2;
  premask = 1 << (BITS_PER_BMUNIT-1);
  firstpremask = premask >> skip;
  
  srow=((BMUNIT*)src)+mini*sbmu_wide+sx;
  for (i = mini;i<supi;i++) {
    xdest=ydest+tx;
    mask = firstpremask;
    sbmu=srow;
    data=*sbmu;
    for (j = minj;j<supj;j++) {       
      CPT(xdest,"copybitbw") 
      if(!mask) {
        mask=premask;
        data= *(++sbmu);
      }
      if(data & mask) *xdest=BLACKCOL;
      mask >>= 1;
      xdest++;
    }
    srow+=sbmu_wide;
    sbmu=srow;
    ydest+=BYTEWIDTH;
  }
}


#if 0
void mycopybitsbw(int tx, int ty, int sw, int sh, void* src) {
/* same as above, but all with gllib */
/* will do for all kind of BMUNIT    */ 
  int j,i,xdest;
  BMUNIT *srow, *row;
  BMUNIT premask;
  register BMUNIT data, mask;
  short sbmu_wide;

  /*fprintf(prot,"(vgacopybitmap1 : tx %d ty %d sw %d sh %d, ...\n",
       tx,ty,sw, sh); */
                            
  if(ty+sh<=0) return;
  if(tx+sw<=0) return;
  if(ty >= vgaydim) return;
  if(tx >= vgaxdim) return;

  /* fprintf(prot,"not clipped away yet \n"); */
  
  sbmu_wide= ROUNDUP(sw,BITS_PER_BMUNIT);
  premask = 1 << (BITS_PER_BMUNIT-1);

  srow=((BMUNIT*)src);
  for (i = 0;i<sh;i++) {
    xdest=tx;
    row=srow;
    for (j = 0;j<sbmu_wide;j++) { 
      mask=premask;
      data= *(row++);
      while(mask) {      
        if(data & mask) gl_setpixel(xdest,ty,BLACKCOL);
        xdest++;
        mask >>= 1;
      }
    }
    srow+=sbmu_wide;
    ty++;
  }
}
#endif


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
  char *ydest, *xdest;

#ifdef DEBUGVGA
  pfprot("(mycopybitsgs : ptr 0x%x tx %d ty %d sw %d sh %d, ...",
       src,tx,ty,sw, sh);                             
  pfprot(" clipping : %d %d %d %d, ...\n",
       ofscx1,ofscy1,ofscx2,ofscy2);
#endif
  mini=minj=0;
  if(ty<ofscy1) {mini= ofscy1-ty; ty=ofscy1;} 
  if(tx<ofscx1) {minj= ofscx1-tx; tx=ofscx1;} 
  supi= MIN(sh,ofscy2+1-ty+mini);
  supj= MIN(sw,ofscx2+1-tx+minj);
  if(supi <= mini || supj <= minj) {
#ifdef DEBUGVGA
     pfprot("clipped all away. no drawings)"); 
#endif
     return;
  }
  ydest=VBUF+ty*BYTEWIDTH;
  /* fprintf(prot,"clipped: tx %d ty %d mi %d si %d mj %d sj %d \n",
     tx,ty,mini,supi,minj,supj); */ 
  
  sbmu_wide= ROUNDUP(sw << GREYSCALE_LOG2,BITS_PER_BMUNIT);
  skip= minj & ((BITS_PER_BMUNIT>>GREYSCALE_LOG2)-1);
  sx= minj >> (BITS_LOG2-GREYSCALE_LOG2);
  premask = ((1<<GREYSCALE)-1) << (BITS_PER_BMUNIT-GREYSCALE);
  firstpremask = premask >> (skip<< GREYSCALE_LOG2);
  firstrs = BITS_PER_BMUNIT-GREYSCALE - skip*GREYSCALE;

  srow=((BMUNIT*)src)+mini*sbmu_wide+sx;
  for (i = mini;i<supi;i++) {
    xdest=ydest+tx;
    mask = firstpremask;
    rs = firstrs;
    sbmu=srow;
    data=*sbmu;
    for (j = minj;j<supj;j++) {
      CPT(xdest,"copybitgs") 
      if(!mask) {
        mask=premask;
        rs=BITS_PER_BMUNIT-GREYSCALE;
        data= *(++sbmu); 
      }
      /* pfprot("- ptr 0x%x data %d -",xdest,((data &mask)>>rs));  */
      (*xdest) |= ((data & mask) >> rs); 
      mask >>= GREYSCALE;
      rs-=GREYSCALE;
      xdest++;
    }
    srow+=sbmu_wide;
    sbmu=srow;
    ydest+=BYTEWIDTH;
  }
#ifdef DEBUGVGA
    pfprot("done)"); 
#endif
}


void myfillboxor(int tx, int ty, int w, int h, int c) {
 
  char *ydest, *xdest;
  int i;

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
  CXW("rector");
  ydest=VBUF+ty*BYTEWIDTH;
  /* fprintf(prot,"clipped or box: tx %d ty %d w %d h %d   \n",
     tx,ty,w,h); */ 

  for (;h>0;h--) {
    xdest=ydest+tx; 
    for (i=w;i>0;i--) {
       CPT(xdest,"rector")                
       (*(xdest++))|=c;
    }
    ydest+=BYTEWIDTH;
  }
}

void myfillboxbg(int tx, int ty, int w, int h, int c) {
 
  char *ydest;
  int i;
  register char *xdest;
  register char xdc;
  /* fprintf(prot,"(vgafillboxor : tx %d ty %d w %d h %d, ...\n",
       tx,ty,w,h); */                              
  if(ty<ofscy1) {h-=ofscy1-ty;  ty=ofscy1;} 
  if(tx<ofscx1) {w-=ofscx1-tx; tx=ofscx1;} 
  if(ty+h>ofscy2+1) h=ofscy2+1-ty;
  if(tx+w>ofscx2+1) w=ofscx2+1-tx;
  if(tx+w>vgaxdim) w=vgaxdim-tx;
  if(w <= 0 || h<= 0) {
     /*fprintf(prot,"clipped all away. no drawings)\n"); */
     return;
  }
  CXW("rectbg");
  ydest=VBUF+ty*BYTEWIDTH;
  
  /* fprintf(prot,"clipped bgbox : tx %d ty %d w %d h %d   \n",
     tx,ty,w,h); */

  for (;h>0;h--) {
    xdest=ydest+tx;
    for (i=w;i>0;i--) {      
      CPT(xdest,"rectbg")
      xdc=*xdest; 
      if(xdc) {      
      /*if(xdc>c) *xdest=c;                       /* transparent modus */     
        if(xdc>BACKCOLS && xdc!= c) *xdest=MIXEDCOL;   /* mixing modus */  
      } else *xdest=c;
      xdest++;
    }
    ydest+=BYTEWIDTH;
  }
}


void myfillbox(int tx, int ty, int w, int h, int c) {
 
  char *ydest;
  int i;
  register char *xdest;
  register char xdc;
  /* fprintf(prot,"(vgafillbox : tx %d ty %d w %d h %d, ...\n",
       tx,ty,w,h); */                              
  if(ty<ofscy1) {h-=ofscy1-ty;  ty=ofscy1;} 
  if(tx<ofscx1) {w-=ofscx1-tx; tx=ofscx1;} 
  if(ty+h>ofscy2+1) h=ofscy2+1-ty;
  if(tx+w>ofscx2+1) w=ofscx2+1-tx;
  if(tx+w>vgaxdim) w=vgaxdim-tx;
  if(w <= 0 || h<= 0) {
     /*fprintf(prot,"clipped all away. no drawings)\n"); */
     return;
  }
  CXW("rectbg");
  ydest=VBUF+ty*BYTEWIDTH;
  
  /* fprintf(prot,"clipped bgbox : tx %d ty %d w %d h %d   \n",
     tx,ty,w,h); */

  for (;h>0;h--) {
    xdest=ydest+tx;
    for (i=w;i>0;i--) {      
      CPT(xdest,"rectbg")
      (*(xdest++))=c; 
    }     
    ydest+=BYTEWIDTH;
  }
}

  


void (*vgaupdate)(int, int, int, int) = myupdate;
void (*vgasetclip)(int, int, int, int) = mysetclip;
void (*vgadrawrect)(int x, int y, int w, int h, int c) = myfillbox; 
void (*vgadrawrector)(int x, int y, int w, int h, int c) = myfillboxor; 
void (*vgadrawrectbg)(int x, int y, int w, int h, int c) = myfillboxbg; 
void (*vgacopybitmapgs)(int x, int y, int w, int h, void* src) = mycopybitsgs; 
void (*vgacopybitmapbw)(int x, int y, int w, int h, void* src) = mycopybitsbw;


/***************************************************************************/
/* now the font stuff ...                                                  */

#include "9x14thin.h"
/*#include "gekl9x16.h" */ 
/*#include "font9x16.h" */

/* now the pronounced ugly fontscaling code: */

char* expandedfont=NULL;

void preparefont(void) {
  long i;
  char* buptr;
  fonth=FONTH;
  fontw=FONTW;
  fonts=FONTS;
  allocmem(&expandedfont,256L*fontw*fonth);
  gl_expandfont(fontw,fonth,TEXTCOL,tmviewfont,expandedfont);
  for(i=0;i<256L*fontw*fonth;i++)
    if(expandedfont[i]==0) expandedfont[i]=TEXTBACKCOL;
  /* use this to scale fonts up a bit. to use with high-res-displays ... 
    if(truevgaxdim/fontw > 100 ) {
    allocmem(&buptr,128L*fontw*fonth*4);
    gl_scalebox(fontw,fonth*128,expandedfont,2*fontw,2*128*fonth,buptr);
    freemem(&expandedfont);
    expandedfont=buptr;
    fonth*=2;
    fontw*=2;
    fonts*=2;   
  } */
  gl_setfontcolors(TEXTBACKCOL,TEXTCOL);
  gl_setwritemode(WRITEMODE_OVERWRITE);
  gl_setfont(fontw,fonth,expandedfont); 
}    

void closefont(void) {
  freemem(&expandedfont);
}


