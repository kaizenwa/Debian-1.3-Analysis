/* This is part of tnview, a dvi previewer. (c) 1995 Thomas Moor         */
/*                                                                       */
/* This program may be used without any warranty. It may be modified and */
/* distributed without any restrictions.                                 */

#define INCL_WIN
#define INCL_GPI

#include <os2.h>                        /* PM header file               */ 
#include <stdio.h>
#include <math.h>

#include "defsos2.h"
#include "writepm.h"                             


/* from the file globals.c we need ... */
extern int truevgaxdim,truevgaydim; 
extern int vgaxdim, vgaydim , directscreen;
extern int vgastatuslen, vgastatushight, vgamaxstatuslines, vgastatuslines;

/* from the file subs.c w need ... */
extern void pfprot(char*, ...);
extern void pfverb(char*, ...);
extern void allocmem(void*,long int);
extern void freemem(void*);



typedef struct BINF {
  ULONG cbFix;
  ULONG cx,cy;
  USHORT cPlanes,cBitCount;
  ULONG ulCompression,cbImage,cxRes,cyRes,cclrUsed;
  RGB2 rgb[NUMBER_OF_COLORS+1]; /* safty ? */
} BINF;

/* globals belonging to the os2 and pm stuff */
HMQ  hmq = NULLHANDLE;                /* (only) Message queue handle     */
HWND hwndFrame = NULLHANDLE;          /* (only) Frame window handle      */
HWND hwndClient = NULLHANDLE;         /* (only) Client window handle      */
HAB  hab;                             /* PM anchor block handle       */ 



/* globals used by my painting stuff */
static char* offscreen;
static char* offend;
static int ofscx1, ofscy1, ofscx2, ofscy2;
static int fontw, fonth, fonts;
static RECTL scrollrect;


static BINF offbinf;
static HBITMAP offbmp=NULLHANDLE;
static HPS offps=NULLHANDLE;
static HDC offdc=NULLHANDLE;
static HPOINTER hptrPointer=NULLHANDLE;


/* Function prototypes */
MRESULT EXPENTRY MyWindowProc( HWND hwnd, ULONG msg, MPARAM mp1, MPARAM mp2 );

/* Function prototypes for ugly font-scaling */
void preparefont(void); 
void closefont(void);
char* expandedfont=NULL;


/* debugging makros to report pixel warnings */
/*#define DEBUGPIXEL*/

#ifdef DEBUGPIXEL
#define CPT(apt,mess) \
     if(apt<offscreen || apt>=offscreen+truevgaxdim*truevgaydim ) \
     {fprintf(prot,"(ctp %s)\n",mess); fflush(prot); exit(1);}

#define CXY(mess) \
      if(x2<=x1 || y2<=y1) \
        {fprintf(prot,"(cxy %s)\n",mess); fflush(prot);}

#define CXW(mess) \
      if(w<0 || h<0) \
        {fprintf(prot,"(cxw %s)\n",mess); fflush(prot);}\

#else /* no debugging */
#define CPT(apt,mess) 
#define CXW(mess) 
#define CXY(mess)
#endif



/* this became display dependent. the pxlmem should be accesable by the dvi-
   encoding part and by the routines that come with the display. */

void free_off(void) {
  freemem(&offscreen);
}

void alloc_off(void) {
  long size;

  free_off();
  size = MAX((long) truevgaxdim*truevgaydim,1);
  allocmem(&offscreen,size);
  memset(offscreen,0,size);
  offend=offscreen+size-truevgaxdim;
  offbinf.cx=truevgaxdim;
  offbinf.cy=truevgaydim;
  offbinf.cbImage=truevgaxdim*truevgaydim;
}


uchar cc;
void offtophys(HPS hps, long t1x,long t1y,long t2x,long t2y) {
  POINTL abtl[4],pt1,pt2,pt3,pt4,pt5,pt6;
  long s1x,s1y,s2x,s2y;
  
  if(offscreen==NULL) return;

  s1x=t1x;
  s1y=t1y;
  s2x=s1x+t2x-t1x+1;
  s2y=s1y+t2y-t1y+1;
  
  /* pfprot("\n(offtophys: t1x %d t1y %d t2x %d t2y %d ",
         t1x,t1y,t2x,t2y);
  pfprot("\nsource: s1x %d s1y %d s2x %d s2y %d )",s1x,s1y,s2x,s2y); */


   
  abtl[0].x=t1x;
  abtl[0].y=t1y;
  abtl[1].x=t2x;
  abtl[1].y=t2y;
  abtl[2].x=s1x;
  abtl[2].y=s1y;
  abtl[3].x=s2x;
  abtl[3].y=s2y;

  /*if(cc) GpiSetColor(hps,CLR_RED);
  else GpiSetColor(hps,CLR_GREEN);
  if(cc==0) GpiSetBackColor(hps,CLR_RED);
  else GpiSetBackColor(hps,CLR_GREEN);
  cc=1-cc;  
  GpiMove(hps,abtl);
  GpiBox(hps,DRO_OUTLINEFILL,abtl+1,0,0);
  GpiSetColor(hps,CLR_DEFAULT);
  GpiMove(hps,abtl);
  GpiBox(hps,DRO_OUTLINE,abtl+1,0,0); */ 
    
  if((t2x-t1x >=0) && (t2y-t1y >=0)){
    GpiSetColor(hps,CLR_DEFAULT);
    GpiDrawBits(hps,offscreen,
           (PBITMAPINFO2)&offbinf,4L,abtl,ROP_SRCCOPY,BBO_IGNORE); 
  }

  /*GpiWCBitBlt(hps,vgabmp,4L,abtl,ROP_SRCCOPY,BBO_IGNORE); */

  /*fprintf(prot,"\n(last error: %x)",WinGetLastError(hab));*/
}      



void vgaclose(void) {
  free_off();
  WinDestroyWindow(hwndFrame);         /* Tidy up...                   */
  WinDestroyMsgQueue(hmq);             /* Tidy up...                   */
  WinTerminate(hab);                   /* Terminate the application    */
}


void vgaerror(char* mssg)
{
   PERRINFO  pErrInfoBlk;
   PSZ       pszOffSet;
   void      stdprint(void);

   pfprot("\nfatal error: display: %s\n",mssg);   
   if(hwndClient != NULLHANDLE) 
     WinPostMsg(hwndClient, WM_QUIT, (MPARAM)NULL, (MPARAM)NULL);
}



void setoffbincol(int i, int r, int g, int b){
  offbinf.rgb[i].bRed=r;   
  offbinf.rgb[i].bGreen=g;
  offbinf.rgb[i].bBlue=b;
  offbinf.rgb[i].fcOptions=0;
}


void myupdate(int x1, int y1, int x2, int y2) {
   RECTL   rc;
   HPS    hps;                       /* Presentation Space handle    */
 
   
   if(x1<0) x1=0; if(y1<0) y1=0;
   if(x2>truevgaxdim-1) x2=truevgaxdim-1;
   if(y2>truevgaydim-1) y2=truevgaydim-1;
   if(x2<x1 || y2<y1) return;  
   CXY("update") 
   rc.xLeft=x1; rc.yBottom=truevgaydim-1-y2; 
   rc.xRight=x2+1; rc.yTop=truevgaydim-1-y1+1;
  
   /* do drawing along messages or ..*/
   WinInvalidateRect(hwndClient,&rc,FALSE);  
   /* immediatly ... */
   if(hwndClient == 0L) return;
   hps = WinBeginPaint(hwndClient, NULLHANDLE, &rc ); 
   offtophys(hps,rc.xLeft,rc.yBottom,rc.xRight,rc.yTop); 
   WinEndPaint( hps );                   

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
}

void mysetclipoff() {
   ofscx1=0; 
   ofscy1=0; 
   ofscx2=truevgaxdim-1; 
   ofscy2=truevgaydim-1; 
}




void vgaopen(void) {
  return;
}


void vgagraph(void) {
  int n;
  uchar g;
  int i,gl;
  char title[MAXPATHSTR+20];
  ULONG flCreate;                       /* Window creation control flags*/
  POINTL pt1,pt2;
  RECTL rc;
  HPS hps;
  SWP swp;

  offscreen=NULL;
  vgaxdim = MAX(VGAXDIMMIN,vgaxdim);  
  vgaydim = MAX(VGAYDIMMIN,vgaydim);
 

  if((hab = WinInitialize(0)) == 0L)   /* Initialize PM     */
     vgaerror("cant open pm");   /* Terminate the application    */

  if((hmq = WinCreateMsgQueue( hab, 0 )) == 0L)/* Create a msg queue */
     vgaerror("cant open mspgq");   /* Terminate the application    */

  if((hptrPointer = WinLoadPointer(HWND_DESKTOP,0L,ID_POINTER))== 0L)
     vgaerror("cant load pointer");   /* Terminate the application    */

  /* if((hptrPointer = WinQuerySysPointer(HWND_DESKTOP,SPTR_TEXT,FALSE))== 0L)
     vgaerror("cant load sys pointer");   /* Terminate the application    */


  if (!WinRegisterClass(                /* Register window class        */
     hab,                               /* Anchor block handle          */
     (PSZ)"MyWindow",                   /* Window class name            */
     (PFNWP)MyWindowProc,               /* Address of window procedure  */
     CS_SIZEREDRAW | CS_MOVENOTIFY,     /* Class style                  */
     0                                  /* No extra window words        */
     ))
       vgaerror("cant open window"); /* Terminate the application    */

  flCreate = FCF_STANDARD &            /* Set frame control flags to   */
             ~FCF_SHELLPOSITION;        /* standard except for shell    */
                                        /* positioning.                 */

  if ((hwndFrame = WinCreateStdWindow(
               HWND_DESKTOP,            /* Desktop window is parent     */
               0,                       /* STD. window styles           */
               &flCreate,               /* Frame control flag           */
               "MyWindow",              /* Client window class name     */
               "",                      /* No window text               */
               0,                       /* No special class style       */
               (HMODULE)0L,             /* Resource is in .EXE file     */
               ID_WINDOW,               /* Frame window identifier      */
               &hwndClient              /* Client window handle         */
               )) == 0L)
       vgaerror("cant open window"); /* Terminate the application    */
  
  sprintf(title,"tmVIEW");
  WinSetWindowText(hwndFrame, title);
    
    
  if (!WinSetWindowPos( hwndFrame,      /* Shows and activates frame    */
                   HWND_TOP,            /* window at default position   */
                   20L, 20L, vgaxdim+5, vgaydim+10,     /* and size. guess  */
                   SWP_MOVE | SWP_SIZE |  SWP_ACTIVATE | SWP_SHOW
                 ))
    vgaerror("cant display window"); /* Terminate the application    */
  if(WinQueryWindowPos(hwndClient, (PSWP)&swp)){ /* get client size */
    vgaxdim=swp.cx;
    vgaydim=swp.cy;
  }

  hps = WinBeginPaint( hwndClient, NULLHANDLE, &rc );
  pt1.x=0;
  pt2.x=vgaxdim-1;
  pt1.y=0;
  pt2.y=vgaydim-1;
  GpiSetColor(hps,CLR_DARKRED);
  GpiMove(hps,&pt1);
  GpiBox(hps,DRO_OUTLINEFILL,&pt2,0,0);
  WinEndPaint( hps );  

  offbinf.cbFix=36L;
  offbinf.cPlanes=1;
  offbinf.cBitCount=(USHORT) 8;
  offbinf.ulCompression=BCA_UNCOMP;

  offbinf.cxRes=0;
  offbinf.cyRes=0;  
  offbinf.cclrUsed=NUMBER_OF_COLORS;

  setoffbincol(SETPALTEXTCOL);
  setoffbincol(SETPALTEXTCOL);           
  setoffbincol(SETPALTEXTBACKCOL);      
  setoffbincol(SETPALBORDERCOL);   
  setoffbincol(SETPALFRAMECOL);    
  setoffbincol(SETPALRECTCOL);     
  setoffbincol(SETPALTFMCOL);      
  setoffbincol(SETPALHREFCOL);     
  setoffbincol(SETPALMIXEDCOL);     
  setoffbincol(SETPALMARKSCOL);     
  setoffbincol(SETPALMARKHCOL);     
  setoffbincol(SETPALMARKDCOL);     
  setoffbincol(SETPALFOUNDCOL);     

  for(i=0;i<COLORS_PER_GREY;i++) {
    gl= (i==0) ? 0: 255 * exp(GAMMA*log(i/(float) (COLORS_PER_GREY-1))) ;
    setoffbincol(COLORS_PER_GREY-1-i,gl,gl,gl);
  }

  truevgaxdim=(vgaxdim | 3) +1;
  truevgaydim=vgaydim;
  alloc_off();
  mysetclip(0,0,vgaxdim-1,vgaydim-1);
  preparefont();
  vgastatushight=fonth+fonts;
  vgamaxstatuslines=truevgaydim/vgastatushight;
  vgastatuslen=vgaxdim/fontw;
  scrollrect.xLeft=0; 
  scrollrect.xRight=vgaxdim-1;
  scrollrect.yBottom=0;
  scrollrect.yTop=truevgaydim-1; 
  directscreen=1;
}


void vgascreen(int son) {
  directscreen=son; 
}



/***************************************************************************/
/*   now the pixel copy stuff:                                             */
/*   destination is allways the offscreen buffer                           */
/*   when directscreen is on, draw on the screen too                       */
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
  ydest=offend-ty*offbinf.cx;
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
      if(data & mask) *xdest=BLACKCOL;
      mask >>= 1;
      xdest++;
      if(!mask) {
        mask=premask;
        data= *(++sbmu);
      }
    }
    srow+=sbmu_wide;
    sbmu=srow;
    ydest-=offbinf.cx;
  }
  if(directscreen) 
    myupdate(tx,ty,tx+sw-1,ty+sh-1); 
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
  char *ydest, *xdest;

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
  ydest=offend-ty*offbinf.cx;
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
      (*xdest) |= ((data & mask) >> rs);
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
    ydest-=offbinf.cx;
  }
  if(directscreen) 
    myupdate(tx,ty,tx+sw-1,ty+sh-1); 
}


void mycopybits88(int tx, int ty, int sw, int sh, char* src) { 
/* this is with self made clipping. no good  ? */
/* copy bytes to bytes                         */
/* compatible to gl_putbox                     */

  int j,i,supi,supj,mini,minj,skip;
  char *srow, *sbmu;
  char *ydest, *xdest;


  mini=minj=0;
  if(ty<ofscy1) {mini= ofscy1-ty; ty=ofscy1;} 
  if(tx<ofscx1) {minj= ofscx1-tx; tx=ofscx1;} 
  supi= MIN(sh,ofscy2+1-ty+mini);
  supj= MIN(sw,ofscx2+1-tx+minj);
  if(supi <= mini || supj <= minj) {
     /* fprintf(prot,"clipped all away. no drawings)\n"); */
     return;
  }
  ydest=offend-ty*offbinf.cx+tx;
  /* fprintf(prot,"(copy 88 clipped: tx %d ty %d mi %d si %d mj %d sj %d \n",
     tx,ty,mini,supi,minj,supj); */ 
  
  srow= src+mini*sw+minj;
  for (i = mini;i<supi;i++) {
    xdest=ydest;
    sbmu=srow;
    for (j = minj;j<supj;j++) {
      *xdest = *sbmu;
      xdest++;
      sbmu++;
    }
    srow+=sw;
    ydest-=offbinf.cx;
  }
}


void myfillbox(int tx, int ty, int w, int h, int c) {
 
  char *ydest, *xdest;
  int i,j;

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
  CXW("rect");
  ydest=offend-ty*offbinf.cx;
  /* fprintf(prot,"clipped or box: tx %d ty %d w %d h %d   \n",
     tx,ty,w,h); */ 

  for (i=h;i>0;i--) {
    xdest=ydest+tx; 
    for (j=w;j>0;j--) {
       CPT(xdest,"rect")                
       (*(xdest++))=c;
    }
    ydest-=offbinf.cx;
  }
  if(directscreen) 
    myupdate(tx,ty,tx+w-1,ty+h-1); 
}

void myfillboxor(int tx, int ty, int w, int h, int c) {
 
  char *ydest, *xdest;
  int i,j;

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
  ydest=offend-ty*offbinf.cx;
  /* fprintf(prot,"clipped or box: tx %d ty %d w %d h %d   \n",
     tx,ty,w,h); */ 

  for (i=h;i>0;i--) {
    xdest=ydest+tx; 
    for (j=w;j>0;j--) {
       CPT(xdest,"rector")                
       (*(xdest++))|=c;
    }
    ydest-=offbinf.cx;
  }
  if(directscreen) 
    myupdate(tx,ty,tx+w-1,ty+h-1); 

}

void myfillboxbg(int tx, int ty, int w, int h, int c) {
 
  char *ydest;
  int i,j;
  register char *xdest;
  register char xdc;
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
  CXW("rectbg");
  ydest=offend-ty*offbinf.cx;
  
  /* fprintf(prot,"clipped bgbox : tx %d ty %d w %d h %d   \n",
     tx,ty,w,h); */

  for (i=h;i>0;i--) {
    xdest=ydest+tx;
    for (j=w;j>0;j--) {      
      CPT(xdest,"rectbg")
      xdc=*xdest; 
      if(xdc) {      
      /*if(xdc>c) *xdest=c;                       /* transparent modus */     
        if(xdc>BACKCOLS && xdc!= c) *xdest=MIXEDCOL;   /* mixing modus */  
      } else *xdest=c;
      xdest++;
    }
    ydest-=offbinf.cx;
  }
  if(directscreen) 
    myupdate(tx,ty,tx+w-1,ty+h-1); 
}



int vgasetstatuslines(int n) {
  vgastatuslines=MAX(0,MIN(vgamaxstatuslines,n));
  vgaydim= truevgaydim- vgastatuslines* vgastatushight;
  vgaxdim= truevgaxdim;
  scrollrect.xLeft=0; 
  scrollrect.xRight=vgaxdim-1;
  scrollrect.yBottom=truevgaydim-vgaydim;
  scrollrect.yTop=truevgaydim-1; 
  mysetclipoff();
  if(vgastatuslines!=vgamaxstatuslines) {
    myfillbox(0,vgaydim,truevgaxdim,vgastatuslines*vgastatushight,TEXTBACKCOL);
    myupdate(0,vgaydim,vgaxdim-1,truevgaydim-1);
  } else {
    myfillbox(0,0,truevgaxdim,truevgaydim,TEXTBACKCOL);
    myupdate(0,0,vgaxdim-1,truevgaydim-1);
  }
  mysetclip(0,0,vgaxdim-1,vgaydim-1);
  return(vgastatuslines);
}


void vgaupdatestatus(void) {
  mysetclipoff();
  myupdate(0, vgaydim,truevgaxdim-1,truevgaydim-1); 
  mysetclip(0,0,vgaxdim-1,vgaydim-1); 
}


void vgadrawstatus(char chr, int line, int pos) {
  int i; 
  if(line>=vgastatuslines || line <0) return;
  if(pos>=vgastatuslen || pos <0) return;
  mysetclipoff();
  mycopybits88(pos*fontw, vgaydim+(line+1)* vgastatushight-fonth, 
        fontw, fonth, expandedfont + (unsigned char) chr *fonth*fontw);
  mysetclip(0,0,vgaxdim-1,vgaydim-1); 
  /* pfprot("(vgadrawstatus [%c] l %d p %d )",chr,line,pos); */ 
}



void vgaphysscroll(short dx,short dy) {
  WinScrollWindow(hwndClient,dx,-dy,
        NULL,NULL,NULLHANDLE,NULL,SW_INVALIDATERGN); 
  /*WinScrollWindow(hwndClient,dx,-dy,NULL,NULL,NULLHANDLE,NULL,0); 
  WinInvalidateRect(hwndClient,NULL,FALSE);*/
}   


void vgaxscroll(short dx) {
  int i;  
  if(abs(dx) >= vgaxdim) return;
  if(dx<0) {
    for(i=0;i<vgaydim;i++)
      memmove(offend-(long)i*offbinf.cx,offend-(long)i*offbinf.cx-dx,vgaxdim+dx);
  }  else {
    for(i=0;i<vgaydim;i++)
      memmove(offend-(long)i*offbinf.cx+dx,offend-(long)i*offbinf.cx,vgaxdim-dx);
  }
  if(directscreen)
     WinScrollWindow(hwndClient,dx,0,
        NULL,&scrollrect,NULLHANDLE,NULL,SW_INVALIDATERGN); 
}
   
void vgayscroll(short dy) {
  int i;
  if(abs(dy) >= vgaydim) return;
  if(dy<0) {
    for(i=0;i<vgaydim+dy;i++)
      memmove(offend-(long)i*offbinf.cx,offend-(long)(i-dy)*offbinf.cx,vgaxdim);
  }  else {
    for(i=vgaydim-1;i>=dy;i--)
      memmove(offend-(long)i*offbinf.cx,offend-(long)(i-dy)*offbinf.cx,vgaxdim);
  }  
  if(directscreen)
     WinScrollWindow(hwndClient,0,-dy,
        NULL,&scrollrect,NULLHANDLE,NULL,SW_INVALIDATERGN); 
}   




void (*vgaupdate)(int, int, int, int) = myupdate;
void (*vgasetclip)(int, int, int, int) = mysetclip;
void (*vgadrawrect)(int x, int y, int w, int h, int c) = myfillbox; 
void (*vgadrawrector)(int x, int y, int w, int h, int c) = myfillboxor; 
void (*vgadrawrectbg)(int x, int y, int w, int h, int c) = myfillboxbg; 
void (*vgacopybitmapgs)(int x, int y, int w, int h, void* src) = mycopybitsgs;
void (*vgacopybitmapbw)(int x, int y, int w, int h, void* src) = mycopybitsbw;



static resizehack=0;

uchar vgagetchar(int block) {
  uchar com;  
  QMSG qmsg;                            /* Message from message queue   */
  if(resizehack) {  /* hopfully nobody sees this ever */
   resizehack=0;
   return(KEYRESIZE);
  } 
  if(!block && !WinPeekMsg( hab, &qmsg, 0L, 0, 0,PM_NOREMOVE)) return(KEYNOP); 
  do{
    WinGetMsg( hab, &qmsg, 0L, 0, 0 ); 
    com=(uchar) WinDispatchMsg(hab, &qmsg);
  } while(com != 'q' && com != KEYRESIZE && WinPeekMsg( hab, &qmsg, 0L, 0, 0,PM_NOREMOVE));
  return(com);
}


int  clientx,clienty,clientact;
int  mousex,mousey;
int  mouseleft,mouseright;

void whereami(void) {
  SWP swp1;
  SWP swp2;
  WinQueryWindowPos(hwndFrame,(PSWP) &swp1);
  WinQueryWindowPos(hwndClient,(PSWP) &swp2);
  clientx=swp1.x+swp2.x;
  clienty=swp1.y+swp2.y;
}



void vgagetmouse(int *x, int *y, int *left, int* right) {
  POINTL ptl;
  whereami();
   if(WinQueryPointerPos(HWND_DESKTOP,&ptl) && clientact){
    mousex=ptl.x-clientx;
    mousey=truevgaydim-(ptl.y-clienty);
  }
  *x = mousex;
  *y = mousey; 
  *left= mouseleft;
  *right= mouseright;
 /* pfprot("(getmouse x %d y %d wx %d wy %d)",mousex,mousey,clientx,clienty);*/
}  


void vgasetmouse(int x, int y) {
  if(!clientact) return;
  whereami();
  WinSetPointerPos(HWND_DESKTOP,
    (LONG) (x+clientx),(LONG) (truevgaydim-y + clienty));
  /* pfprot("(setmouse x %d y %d wx %d wy %d)",x,y,clientx,clienty); */
}  




MRESULT EXPENTRY MyWindowProc( HWND hwnd, ULONG msg, MPARAM mp1, MPARAM mp2 ) {
  long res=FALSE;
  ushort input;
  int newxdim, newydim; 
  switch( msg )
  {
    case WM_CREATE:
      break;     

    case WM_ERASEBACKGROUND:
      /*
       * Return TRUE to request PM to paint the window background
       * in SYSCLR_WINDOW.
       */
      res=FALSE;
      break;
    case WM_PAINT:
      /*
       * Window contents are drawn here in WM_PAINT processing.
       */
      {
      HPS    hps;                       /* Presentation Space handle    */
      RECTL   rc;
      char pagestring[MAXPATHSTR+13];
                                        /* Create a presentation space  */
      hps = WinBeginPaint( hwnd, NULLHANDLE, &rc );
      offtophys(hps,rc.xLeft,rc.yBottom,rc.xRight,rc.yTop); 
      /*sprintf(pagestring,"tmVIEW");
      WinSetWindowText(hwndFrame, pagestring);*/  
      WinEndPaint( hps );                   
      break;
      }
    case WM_COMMAND:                  /* WM_COMMAND command value     */
      input = SHORT1FROMMP(mp1);      /* Extract the command value    */
      switch (input) {
      case ID_RIGHT:
        res=KEYRIGHT; break;
      case ID_LEFT:
        res=KEYLEFT; break;
      case ID_DOWN:
        res=KEYDOWN; break;
      case ID_UP:
        res=KEYUP; break;
      case ID_PAGEDOWN:
        res=KEYNEXT; break;
      case ID_PAGEUP:
        res=KEYPREV; break;
      case ID_BEGIN:
        res=KEYHOME; break;
      case ID_END:
        res=KEYEND; break;
      case ID_DELETE:
        res=127; break;
      case ID_ESCAPE:
        res=KEYESC; break;
      case ID_CTRLC:
        res='q'; break;
      default:
        /* res=KEYNOP;*/
      }
      break;
    
    case WM_CHAR:
      if (!(SHORT1FROMMP(mp1) & KC_KEYUP)) 
      if (SHORT1FROMMP(mp1) & KC_CHAR){
        input=SHORT1FROMMP(mp2);
        /* pfprot("(%d)",input); */
        res=KEYNOP;
        if(input >= ' ' && input < 128) res= input;
        if(input==13) res=KEYRET;
        if(input==9) res=KEYTAB; 
	}
      break;

    case WM_ACTIVATE: 
      clientact=SHORT1FROMMP(mp1);
      res=KEYMOUSE;
      break;

    case WM_MOUSEMOVE:
      WinSetPointer(HWND_DESKTOP,hptrPointer);  
      res=KEYNOP; /* dont call default */
      break;

    case WM_BUTTON1DOWN:
    case WM_BUTTON1MOTIONSTART:
      mouseleft=1;      
      res=KEYMOUSE; 
      break;
    case WM_BUTTON1UP:
    case WM_BUTTON1MOTIONEND:
      mouseleft=0;      
      res=KEYMOUSE; 
      break;
    case WM_BUTTON2DOWN:
    case WM_BUTTON2MOTIONSTART:
      mouseright=1;     
      res=KEYMOUSE; 
      break;
    case WM_BUTTON2UP:
    case WM_BUTTON2MOTIONEND:
      mouseright=0;     
      res=KEYMOUSE; 
      break;


    case WM_SIZE:
         /* Window is being resized so get new client window size and   */
         /* recompute the end points for the lines.                     */
      if(offscreen==NULL) break; /* not started up jet */
      newxdim = MAX(VGAXDIMMIN,SHORT1FROMMP( mp2 ));   /* Get new client window size    */
      newydim = MAX(VGAYDIMMIN,SHORT2FROMMP( mp2 ));
      /* pfprot("(WM_SIZE %d %d)",newxdim,newydim); */
      truevgaxdim=(newxdim | 3) + 1;
      truevgaydim=newydim;
      vgaxdim=newxdim;
      vgaydim=newydim;
      scrollrect.xLeft=0; 
      scrollrect.xRight=vgaxdim-1;
      scrollrect.yBottom=0;
      scrollrect.yTop=truevgaydim-1; 
      free_off();
      alloc_off(); 
      vgamaxstatuslines=truevgaydim/vgastatushight;
      vgastatuslines=0;
      vgastatuslen=vgaxdim/fontw;
      mysetclipoff(); /* set the clipp values !! dont draw over offscreen */
      resizehack=1;
      break;  
    case WM_CLOSE:
      /*
       * This is not the place to put my termination routines
       */
      WinPostMsg( hwnd, WM_QUIT, (MPARAM)0,(MPARAM)0 ); 
      res='q';                                     /* Cause termination*/
      break;
    default:
      /*
       * Everything else comes here.  This call MUST exist
       * in your window procedure.
       */

      return(WinDefWindowProc( hwnd, msg, mp1, mp2 ));
  }
  if(res==KEYMOUSE)  return(WinDefWindowProc( hwnd, msg, mp1, mp2 ));
  return (MRESULT) res;
}



/***************************************************************************/
/* now the font stuff ...                                                  */

/* #include "geklaut9x16.h" */
/* #include "font9x16.h" */
#include "9x14thin.h" 

/* now the pronounced ugly fontscaling code: */
/* should be done totaly different for pm anyway */


/* stolen from libgl ...  */
void expandfont(void) {
  int i, x, y, b;
  uchar *f1;
  uchar *f2;
  allocmem(&expandedfont,256L*FONTW*FONTH);
  f1=tmviewfont;
  f2=expandedfont;
  for (i = 0; i < 256; i++) {
    for (y = 0; y < FONTH; y++) {
      for (x = 0; x < FONTW; x++) {
        if (x % 8 == 0)
	  b = *f1++;
	if (b & (128 >> (x % 8)))  /* pixel */
     	  *f2 = TEXTCOL;
	else 
     	  *f2 = TEXTBACKCOL;
 	f2++; 
      }
    }
  }
}


void preparefont(void) {
  long i;
  char* buptr;
  fonth=FONTH;
  fontw=FONTW;
  fonts=FONTS;
  
  expandfont();
  /* use this to scale fonts up a bit. to use with high-res-displays ... 
    if(truevgaxdim/fontw > 100 ) {
    allocmem(buptr,128L*fontw*fonth*4);
    gl_scalebox(fontw,fonth*128,expandedfont,2*fontw,2*128*fonth,buptr); 
    freemem(&expandedfont);
    expandedfont=buptr;
    fonth*=2;
    fontw*=2;
    fonts*=2;   
  } */ 
}    

void closefont(void) {
  freemem(&expandedfont);
}











