/* This is part of tmview, a dvi previewer. (c) 1995 Thomas Moor         */
/*                                                                       */
/* This program may be used without any warranty. It may be modified and */
/* distributed without any restrictions.                                 */


#include <stdio.h>
#include <math.h>
#include "defs.h"
#include "globals.h"
#include "subs.h"
#include "writedis.h"
#include "readpk.h"
#include "halfhyp.h"
#include "drawit.h"


/* #define DEBUGDRAW */ 


/* eieiei and again forward decl. */
void drawpage(int, int, int, int);

/* ugly switch */
static int tmpnomarks=0;

  

void drawlistclear(void) {
  int i=0;
  drawlistelement *cptr, *nptr; 
  cptr=thedrawlist;
  while(cptr != NULL){
    nptr=cptr->next;
    i++;
    freemem(&cptr);
    cptr=nptr;
  }
  thedrawlist=NULL;
#ifdef DEBUGDRAW
  if(i!=0)
    pfprot("(drawlistclear: freed %d entries, thats %.1fKbyte)",
          i,i*sizeof(drawlistelement) / 1024.0);
  else
   pfprot("(drawlistclear 0)"); 
#endif
}

drawlistdata* drawlistadd(selfdrawfunction* sdf) {
  int i;
  drawlistelement* nptr; 
  allocmem(&nptr,sizeof(drawlistelement));
  nptr->next=NULL;
  nptr->drawmyself=sdf;
  if(thedrawlist==NULL) {
    thedrawlist=nptr;
    nptr->prev=nptr;
  } else {
    thedrawlist->prev->next=nptr;
    nptr->prev=thedrawlist->prev;
    thedrawlist->prev=nptr;
  }
  return(&(nptr->data));
  /* i=0;
  nptr=thedrawlist;
  while(nptr != NULL){
    nptr=nptr->next;
    i++;
  }
  pfprot(" %d elements now)\n",i); */ 
}




#define HIDEHIS 50  /* buffer will be reset if drawing in mode 1 */
int hidehis[HIDEHIS][4];
int nexthide=0;

void drawaddupdatelist(int x1, int y1,int x2, int y2) {
  if(directscreen==1) return;
  if(nexthide>=HIDEHIS) { /* this cant happen ? */
    pfprot("(hidehis overflow. grep for HIDEHIS. sorry)");
    for(nexthide=0;nexthide<HIDEHIS-1; nexthide++) {
      hidehis[nexthide][0]= hidehis[nexthide+1][0];
      hidehis[nexthide][1]= hidehis[nexthide+1][1];
      hidehis[nexthide][2]= hidehis[nexthide+1][2];
      hidehis[nexthide][3]= hidehis[nexthide+1][3];
    }    
  }
  hidehis[nexthide][0]=x1;
  hidehis[nexthide][1]=y1;
  hidehis[nexthide][2]=x2;
  hidehis[nexthide][3]=y2;
  nexthide++;
}

void updatelist(void) {
   while(nexthide>=1) { 
     --nexthide;
     (*vgaupdate)(hidehis[nexthide][0],
                  hidehis[nexthide][1],
                  hidehis[nexthide][2],
                  hidehis[nexthide][3]);
   } 
#if ALLSCREEN || CURSORSNOTONSCREEN
   vgascreen(1);   /* turn screen drawing on again */
#else
   vgascreen(0);
#endif
}

void updateall(void) {
#if NOTHINGONSCREEN
   (*vgaupdate)(0,0,vgaxdim-1,vgaydim-1);
   nexthide=0;  
#else
   updatelist();      /* will set vgascreen */
#endif
}

void drawupdatepage(void) {  /* this does a total reset */
#if NOTHINGONSCREEN || SCROLLONSCREEN
   vgascreen(0);
   drawpage(0,0,vgaxdim-1,vgaydim-1);
   (*vgaupdate)(0,0,vgaxdim-1,vgaydim-1);
#else
   vgascreen(1);
   drawpage(0,0,vgaxdim-1,vgaydim-1);
#endif
   nexthide=0;  
}


 
void drawhidemark(int x, int y) {
   int ms=MARKSIZE;
   tmpnomarks=1;
#if CURSORSNOTONSCREEN  /* switch drirect screen off */
   vgascreen(0);
#endif 
   drawpage(x-MARKSIZE,y-MARKSIZE,x+MARKSIZE,y+MARKSIZE);
} 

void drawshowmark(int x, int y, int col){
   int ms=MARKSIZE; 
   /* (*vgasetclip)(0,0,vgaxdim-1,vgaydim-1); */
   (*vgadrawrect)(x-ms,y,2*ms+1,1,BLACKCOL);
   (*vgadrawrect)(x,y-ms,1,2*ms+1,BLACKCOL);
   (*vgadrawrectbg)(x-ms,y-ms,2*ms+1,2*ms+1,col);
   drawaddupdatelist(x-ms,y-ms,x+ms,y+ms);
}

void showmarks(void){ 
  if(markon) 
    drawshowmark(marksxpxl,marksypxl,MARKSCOL); 
  if(hypon) 
    drawshowmark(marksxpxl,marksypxl,MARKHCOL); 
}

void showmarkd(void){    
  if(markon) 
  drawshowmark(MMTOPXL(markdxmm-visfmk->hoffmm)-dvixpos,
               MMTOPXL(markdymm-visfmk->voffmm)-dviypos,MARKDCOL);
}

void showmarkt(void){ 
  if(texton) {   
  (*vgadrawrectbg)(textx1pxl-dvixpos,texty1pxl-dviypos,
                   textx2pxl-textx1pxl+1, texty2pxl-texty1pxl+1,FOUNDCOL);
  (*vgadrawrectbg)(textx3pxl-dvixpos,texty3pxl-dviypos,
                   textx4pxl-textx3pxl+1, texty4pxl-texty3pxl+1,FOUNDCOL);
  drawaddupdatelist(textx1pxl-dvixpos,texty1pxl-dviypos,
                    textx2pxl-dvixpos,texty2pxl-dviypos);
  drawaddupdatelist(textx3pxl-dvixpos,texty3pxl-dviypos,
                    textx4pxl-dvixpos,texty4pxl-dviypos);
  }
} 

void drawshowallmarks(void) {
  showmarkd();
  showmarkt();
  showmarks();
}


void drawshowfixedmarksonly(void){ 
  if(markon) 
    drawshowmark(marksxpxl,marksypxl,MARKSCOL); 
  if(hypon) 
    drawshowmark(marksxpxl,marksypxl,MARKHCOL); 
}


void drawhidefixedmarksonly(void){ 
  if(markon==0 && hypon==0) return;  
  drawhidemark(marksxpxl,marksypxl);
  showmarkd(); /* for the case of intersection */
  showmarkt();
}

void drawhideallmarks(void){  
#if CURSORSNOTONSCREEN  /* switch drirect screen off */
  vgascreen(0);
#endif 
  if(markon!=0) {
    drawhidemark(MMTOPXL(markdxmm-visfmk->hoffmm)-dvixpos,
                 MMTOPXL(markdymm-visfmk->voffmm)-dviypos);
    drawhidemark(marksxpxl,marksypxl);
  }
  if(hypon!=0) {
    drawhidemark(marksxpxl,marksypxl);
  }
  if(texton!=0) {
    tmpnomarks=1; 
    drawpage(textx1pxl-dvixpos,texty1pxl-dviypos,
             textx2pxl-dvixpos,texty2pxl-dviypos);
    tmpnomarks=1;
    drawpage(textx3pxl-dvixpos,texty3pxl-dviypos,
             textx4pxl-dvixpos,texty4pxl-dviypos);
  }
} 



void drawhiderect(void) {
   int x, y, h, w;
   if(recton==0) return;
#if CURSORSNOTONSCREEN  /* switch drirect screen off */
   vgascreen(0);
#endif 
   x=-dvixpos+MMTOPXL(rectxmm-visfmk->hoffmm);
   y=-dviypos+MMTOPXL(rectymm-visfmk->voffmm);
   w=MMTOPXL(rectxmm-visfmk->hoffmm+rectwmm)-MMTOPXL(rectxmm-visfmk->hoffmm)+1;
   h=MMTOPXL(rectymm-visfmk->voffmm+recthmm)-MMTOPXL(rectymm-visfmk->voffmm)+1;
   recton=0;
   /*drawpage(x,y,x+w-1,y+h-1); */
   drawpage(x,y,x+w-1,y);
   drawpage(x,y+h-1,x+w-1,y+h-1);
   drawpage(x,y,x,y+h-1);
   drawpage(x+w-1,y,x+w-1,y+h-1);
   recton=1;
} 

void drawshowrect(void){
   int x, y, h, w;
   if(recton==0) return; 
   /*fprintf(prot,"(drawshowrect)");*/
   x=-dvixpos+MMTOPXL(rectxmm-visfmk->hoffmm);
   y=-dviypos+MMTOPXL(rectymm-visfmk->voffmm);
   w=MMTOPXL(rectxmm-visfmk->hoffmm+rectwmm)-MMTOPXL(rectxmm-visfmk->hoffmm)+1;
   h=MMTOPXL(rectymm-visfmk->voffmm+recthmm)-MMTOPXL(rectymm-visfmk->voffmm)+1;
   (*vgadrawrect)(x,y,w,1,RECTCOL);
   (*vgadrawrect)(x,y+h-1,w,1,RECTCOL);
   (*vgadrawrect)(x,y,1,h,RECTCOL);
   (*vgadrawrect)(x+w-1,y,1,h,RECTCOL);
   drawaddupdatelist(x,y,x+w-1,y);
   drawaddupdatelist(x,y+h-1,x+w-1,y+h-1);
   drawaddupdatelist(x,y,x,y+h-1);
   drawaddupdatelist(x+w-1,y,x+w-1,y+h-1);
}



void drawcharbmp(drawlistdata* arg) { 
  chdesc *thechar;

  /*fprintf(prot,"<High, its me. CharBmp. lets see>\n");*/
 
  thechar=(*arg).chr.chdp;  
  if(thechar->bmp.bits == NULL) pkloadchar(thechar);
  else touchlrumem(&(thechar->bmp.bits));
 (*vgacopybitmapbw)( (*arg).chr.h -dvixpos -thechar->hof,
                     (*arg).chr.v -dviypos -thechar->vof, 
                     thechar->bmp.w,
                     thechar->bmp.h,
                     thechar->bmp.bits);
}

void drawcharbmp2bw(drawlistdata* arg) {  
  chdesc *thechar;

  /*fprintf(prot,"<High, its me. CharBmp2BW. lets see>\n");*/

  thechar=(*arg).chr.chdp;  
  if((thechar->bmp2.bits == NULL) || (thechar->shrink != fshrink) 
      || (thechar->bmp2.type != BLACKNWHITE)) {
    if(thechar->bmp.bits == NULL) pkloadchar(thechar);
    pkshrinkcharazbw(thechar);
  } else touchlrumem(&(thechar->bmp2.bits));
  (*vgacopybitmapbw)((*arg).chr.h -dvixpos -thechar->hof2,
                     (*arg).chr.v -dviypos -thechar->vof2, 
                     thechar->bmp2.w,
                     thechar->bmp2.h,
                     thechar->bmp2.bits);
}

void drawcharbmp2gs(drawlistdata* arg) {  
  chdesc *thechar;

#ifdef DEBUGDRAW	
  pfprot("(CharBmp2GS ");
#endif
 
  thechar=(*arg).chr.chdp;  
  if((thechar->bmp2.bits == NULL) || (thechar->shrink != fshrink) 
      || (thechar->bmp2.type != GREYSCALE)) {
    if(thechar->bmp.bits == NULL) pkloadchar(thechar);
    pkshrinkcharazprecgs(thechar);
  } else touchlrumem(&(thechar->bmp2.bits));
#ifdef DEBUGDRAW	
  pfprot(" call vgacopybitmapgs ");
#endif
  (*vgacopybitmapgs)((*arg).chr.h -dvixpos -thechar->hof2,
                     (*arg).chr.v -dviypos -thechar->vof2, 
                     thechar->bmp2.w,
                     thechar->bmp2.h,
                     thechar->bmp2.bits);
#ifdef DEBUGDRAW	
  pfprot(" done)");
#endif
}



void drawruleor(drawlistdata* arg) {
  (*vgadrawrector)( (*arg).rect.x-dvixpos, 
                    (*arg).rect.y-dviypos, 
                    (*arg).rect.w, 
                    (*arg).rect.h, 
                    (*arg).rect.c);
} 

void drawrulebg(drawlistdata* arg) {
  (*vgadrawrectbg)( (*arg).rect.x-dvixpos, 
                    (*arg).rect.y-dviypos, 
                    (*arg).rect.w, 
                    (*arg).rect.h, 
                    (*arg).rect.c);
} 

void drawpage(int x1, int y1, int x2, int y2) {
  drawlistelement *eptr;
  int i;
  HTeX_Anchor *HTeXAp;

#ifdef DEBUGDRAW	
  pfprot("(drawpage: dest: %d %d %d %d dvipos %d %d ...",
              x1,y1,x2,y2,dvixpos,dviypos);
#endif
  if(visfmk==NULL) return;
  if(x1<0) x1=0; if(y1<0) y1=0;
  if(x2>vgaxdim-1) x2=vgaxdim-1;
  if(y2>vgaydim-1) y2=vgaydim-1;
  if(x2<x1 || y2<y1) return;  
  (*vgasetclip)(x1,y1,x2,y2);
  (*vgadrawrect)(x1,y1,x2-x1+1,y2-y1+1,WHITECOL);
  /*Farbtest  ... */
  /*for(i=0;i<64;i++)
    (*vgadrawrect)(i*10,0,10,10,i);*/  
  eptr=thedrawlist;
#ifdef DEBUGDRAW	
  pfprot(" ... drawlist ... ");
#endif
  while(eptr != NULL) {
    eptr->drawmyself(&(eptr->data));
    eptr=eptr->next;
  }
#ifdef DEBUGDRAW	
  pfprot(" ... other stuff ... ");
#endif
  /* boarder = paper farme */
  (*vgadrawrect)(-dvixpos-MMTOPXL(visfmk->hoffmm)-1,
                 -dviypos-MMTOPXL(visfmk->voffmm)-1,
                  MMTOPXL(visfmk->papxmm)+3,1,BORDERCOL);
  (*vgadrawrect)(-dvixpos-MMTOPXL(visfmk->hoffmm)-1,
                 -dviypos-MMTOPXL(visfmk->voffmm)-1,
                  1,MMTOPXL(visfmk->papymm)+3,BORDERCOL);
  (*vgadrawrect)(-dvixpos-MMTOPXL(visfmk->hoffmm)-1,
                 -dviypos-MMTOPXL(visfmk->voffmm)+MMTOPXL(visfmk->papymm)+1,
                  MMTOPXL(visfmk->papxmm)+3,1,BORDERCOL);
  (*vgadrawrect)(-dvixpos-MMTOPXL(visfmk->hoffmm)+MMTOPXL(visfmk->papxmm)+1,
                 -dviypos-MMTOPXL(visfmk->voffmm)-1,
                  1,MMTOPXL(visfmk->papymm)+3,BORDERCOL);
  if(frameon){
    /* draw  frame */
    (*vgadrawrect)(
    -dvixpos-MMTOPXL(visfmk->hoffmm)+MMTOPXL(visfmk->lrandmm)-1,
    -dviypos-MMTOPXL(visfmk->voffmm)+MMTOPXL(visfmk->orandmm)-1,
    MMTOPXL(visfmk->papxmm-visfmk->rrandmm)-
    MMTOPXL(visfmk->lrandmm)+3,1,FRAMECOL);
    (*vgadrawrect)(
    -dvixpos-MMTOPXL(visfmk->hoffmm)+MMTOPXL(visfmk->lrandmm)-1,
    -dviypos-MMTOPXL(visfmk->voffmm)+MMTOPXL(visfmk->papymm-visfmk->urandmm)+1,
    MMTOPXL(visfmk->papxmm-visfmk->rrandmm)-
    MMTOPXL(visfmk->lrandmm)+3,1,FRAMECOL);
    (*vgadrawrect)(
    -dvixpos-MMTOPXL(visfmk->hoffmm)+MMTOPXL(visfmk->lrandmm)-1,
    -dviypos-MMTOPXL(visfmk->voffmm)+MMTOPXL(visfmk->orandmm)-1,
    1,MMTOPXL(visfmk->papymm-visfmk->urandmm)-
    MMTOPXL(visfmk->orandmm)+3,FRAMECOL);
    (*vgadrawrect)(
    -dvixpos-MMTOPXL(visfmk->hoffmm)+
    MMTOPXL(visfmk->papxmm-visfmk->rrandmm)+1,
    -dviypos-MMTOPXL(visfmk->voffmm)+MMTOPXL(visfmk->orandmm)-1,
    1,MMTOPXL(visfmk->papymm-visfmk->urandmm)-
    MMTOPXL(visfmk->orandmm)+3,FRAMECOL);
  }
  drawshowrect();
  /* fprintf(prot,"(read tmp %d)",tmpnomarks); */
  if(tmpnomarks==0) 
    drawshowallmarks();
  else
    tmpnomarks=0;
  if(hypon) {
    HTeXAp = cpage->anchorlist;
    for (i=0; i < cpage->nanchors; i++, HTeXAp++) {
      if ((HTeXAp->type&HTeX_A_HREF) == 0) continue; /* Only box hrefs */
        vgadrawrectbg(HTeXAp->x1pxl-dvixpos,HTeXAp->y1pxl-dviypos, 
      HTeXAp->x2pxl-HTeXAp->x1pxl+1,HTeXAp->y2pxl-HTeXAp->y1pxl+1,HREFCOL);
    }
  }
  drawaddupdatelist(x1,y1,x2,y2);
  (*vgasetclip)(0,0,vgaxdim-1,vgaydim-1);
#ifdef DEBUGDRAW	
  pfprot(" ... drawpage done)");
#endif
}


void drawsr(const char* str, int line, int pos, int len) {
  int i,slen;
  slen=strlen(str);
  pos=MAX(0,MIN(vgastatuslen,pos));
  len=MAX(0,MIN(len,vgastatuslen-pos));
  i=pos;
  for(;i < pos+len-slen; i++) vgadrawstatus(' ',line,i);
  for(;i < pos+len; i++) vgadrawstatus(str[slen-(len+pos-i)],line,i);
}

void drawsl(const char* str, int line, int pos, int len) {
  int i,slen,dlen;
  slen=strlen(str);
  pos=MAX(0,MIN(vgastatuslen,pos));
  len=MAX(0,MIN(len,vgastatuslen-pos));
  dlen=MIN(slen,len);
  for(i=0;i<dlen;i++)
    vgadrawstatus(str[i],line,pos+i);
  for(;i < len; i++) vgadrawstatus(' ',line,pos+i);
}

void drawsn(const char* str, int line,int pos) {
  drawsl(str,line,pos,strlen(str));
}


static char counterstr[10*10];
static int pagestrlen=0;
static int markstrlen=0;
static int lastarglen=0;

void setcounterstr(void) {
   int i,j;
   char bustr[20];
   if(cpage==NULL) {counterstr[0]=0; return;}
   for(i=9;i>0 && cpage->count[i] ==0; i--); 
   i=MAX(0,i);
   counterstr[0]=0;
   for(j=0;j<=i;j++) {
     sprintf(bustr,"%d;",MIN(9999,MAX(-9999,cpage->count[j])));
     strcat(counterstr,bustr);
   }
   counterstr[strlen(counterstr)-1]=0;
 }


void drawstatusline(void); /* forward */
int doupdate=1;


void drawstatusmarks(void) {
  char bustr1[180], bustr2[180];
  char* refstr;
  int bui,ol;

  if(cpage==NULL || visfmk==NULL) return;
  if(statusforce) {
    drawstatusline();
    return;
  }
  switch(statustype) {
  case 0:
    refstr=NULL;
    if(hypon) 
      htex_withinanchor(cpage,dvixpos+marksxpxl, dviypos+marksypxl, &refstr);
    if(refstr==NULL) {
      sprintf(bustr1,"(%%.%df,%%.%df) [%s]",unitcomma,unitcomma,unitname);
      sprintf(bustr2, bustr1, 
            mmtounit*(PXLTOMM(dvixpos+marksxpxl)+visfmk->hoffmm-markdxmm),
            mmtounit*(PXLTOMM(dviypos+marksypxl)+visfmk->voffmm-markdymm));
      ol=markstrlen;
      markstrlen=strlen(bustr2);
      drawsr(bustr2,0,vgastatuslen-MAX(ol,markstrlen),vgastatuslen);
    } else {
      strncpy(bustr1,refstr,100);
      bustr1[100]=0;
      sprintf(bustr2,"(%s)",bustr1);
      ol=markstrlen;
      markstrlen=strlen(bustr2);
      drawsr(bustr2,0,vgastatuslen-MAX(ol,markstrlen),vgastatuslen);
    }  
    break;
  case 1:
    sprintf(bustr1,"(%%6.%df,%%6.%df)  (%%6.%df,%%6.%df) [%s]",
               unitcomma,unitcomma,unitcomma,unitcomma,unitname);
    sprintf(bustr2, bustr1, 
             mmtounit*markdxmm, 
             mmtounit*markdymm,
             mmtounit*(PXLTOMM(dvixpos+marksxpxl)+visfmk->hoffmm-markdxmm), 
             mmtounit*(PXLTOMM(dviypos+marksypxl)+visfmk->voffmm-markdymm));
    markstrlen=strlen(bustr2);
    drawsr(bustr2,0,vgastatuslen-37,37);
    break;
  default: return;
  }
  if(doupdate) vgaupdatestatus();
}
    

void drawstatuspage(void) {
  char bustr[MAXPATHSTR+10*10];
  int ol;

  if(cpage==NULL || visfmk==NULL) return;
  if(statusforce) {
    drawstatusline();
    return;
  }
  switch(statustype) {
  case 0:
    setcounterstr();
    sprintf(bustr,"%s[%s]",dviname,counterstr);
    ol=pagestrlen;
    pagestrlen=strlen(bustr);
    drawsl(bustr,0,0,MAX(pagestrlen,ol));
    break;
  default: return;
  }
  if(doupdate) vgaupdatestatus();
}


void drawstatusarg(void) {
  char bustr[MAXARGSTR+MAXPATHSTR+10*11];

  if(cpage==NULL || visfmk==NULL) return;
  if(statusforce) {
    drawstatusline();
    return;
  }
  switch(statustype) {
  case 0:
    if(numberargstrlen>0) {
      numberargstr[numberargstrlen]=0;
      sprintf(bustr," <%s> ",numberargstr);
      drawsn(bustr,0,pagestrlen);
      lastarglen=1;
    } else {
      if(lastarglen>0) {
        drawsl(" ",0,pagestrlen,vgastatuslen);
        drawstatusmarks();
      }
      lastarglen=0;
    }
    break;
  default: return;
  }
  if(doupdate) vgaupdatestatus();
}

void drawstatuszoom(void) {
  char bustr[MAXARGSTR+MAXPATHSTR+10];
  if(cpage==NULL || visfmk==NULL) return;
  if(statusforce) {
    drawstatusline();
    return;
  }
  switch(statustype) {
  case 1:
    sprintf(bustr,"%.3f   ",1/fshrink);
    drawsn(bustr,0,5); 
    break;
  default: return;
  }
  if(doupdate) vgaupdatestatus();
}

void drawstatusline(void) {
  int i;
  /* fprintf(prot,"(drawstatusline ..."); fflush(prot); */
  if(vgastatuslines!=1) {
     i=vgastatuslines;
     vgasetstatuslines(1);
     drawpage(0,vgaydim-vgastatushight*(i+1),vgaxdim-1,vgaydim-1);
     (*vgaupdate)(0,vgaydim-vgastatushight*(i+1),vgaxdim-1,vgaydim-1);
  }
  if(cpage==NULL || visfmk==NULL) return;
  statusforce=0;
  doupdate=0;
  markstrlen=0; 
  pagestrlen=0;
  lastarglen=0;
  switch(statustype) {
  case 0:  
    drawsl(" ",0,0,vgastatuslen);
    break;
  case 1:  
    if(vgastatuslen>=70) {
      drawsl("ZOOM ",0,0,vgastatuslen);
      drawsn("PAGEMARK/SCREENMARK ",0,vgastatuslen-20-37);
    } else {
      drawsl("PM/SM ",0,0,vgastatuslen);
    }
    break;
  }
  /* fprintf(prot,"details ..."); fflush(prot); */
  drawstatuspage();            /* order is essential !!! */ 
  drawstatusmarks();
  drawstatusarg();
  drawstatuszoom();
  vgaupdatestatus();
  doupdate=1;
  /* fprintf(prot,")"); fflush(prot);*/
}
  

  
  
  






