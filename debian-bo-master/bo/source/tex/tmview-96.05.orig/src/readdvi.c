/* This is part of tmview, a dvi previewer. (c) 1995 Thomas Moor         */
/*                                                                       */
/* This program may be used without any warranty. It may be modified and */
/* distributed without any restrictions.                                 */


#include <stdio.h>
#include <math.h>
#include <string.h>
#include "regex.h"
#include "defs.h"
#include "globals.h"
#include "subs.h"
#include "readtfm.h"
#include "readpk.h"
#include "halfhyp.h"
#include "drawit.h"
#include "readdvi.h"


/* #define DEBUGDVI */
/* #define DEBUGFONTDATABASE */



/********************************************************************/
/* fontdatabase managing ********************************************/
/********************************************************************/

#define FONTDATASTEP 32
#define MAXFONTDATA 10000

static int nfontdatabase;
static int maxfontdatabase;

void initfontdatabase(void) {
  nfontdatabase=0;
  maxfontdatabase=0;
  fontdatabase=NULL;
#ifdef DEBUGFONTDATABASE
  pfprot("(initfontdatabase)");
#endif
}

int allocfontdesc(char* ffpath, char* ffname, long dvimag, long scf, long dsz) {
  fontdesc* font;
  int i;
#ifdef DEBUGFONTDATABASE
  pfprot("(allocfontdesc: ");
#endif
  for(i=0;i<nfontdatabase && fontdatabase[i].usecount!=-1; i++);
  if(i<nfontdatabase) {
    font=fontdatabase+i;
#ifdef DEBUGFONTDATABASE
    pfprot(" found unused ");
#endif
  } else {
    if(maxfontdatabase<=nfontdatabase) {
      if(nfontdatabase> MAXFONTDATA) {
        pfprot("\nfatal error: fontdatabase exploded .. :-(\n");
        exit(1);
      }
#ifdef DEBUGFONTDATABASE
      pfprot(" allocmem ");
#endif
      maxfontdatabase+=FONTDATASTEP;
      reallocmem(&fontdatabase,maxfontdatabase * sizeof(fontdesc));
    }
    font=fontdatabase+nfontdatabase;
    i=nfontdatabase;
    nfontdatabase++;
  }
  font->chv = NULL;
  font->fonam = NULL;
  font->fopath = NULL;
  font->fodvimag = dvimag;
  font->foscf = scf;
  font->fodsz = dsz;
  font->pkfile = NULL;
  font->encoding = NULL; 
  font->mch = -1;
  font->usecount=0;
  stralloccpy(&(font->fonam),ffname);
  stralloccpy(&(font->fopath),ffpath);
#ifdef DEBUGFONTDATABASE
  pfprot("n=%d)",nfontdatabase);
#endif
  return(i);
}

int searchfontdatabase(char* ffpath, char* ffname,
                       long dvimag, long scf, long dsz) {
  fontdesc* font;
  int i;
#ifdef DEBUGFONTDATABASE
  pfprot("(searchfontdatabase %s %s %6.1f ",ffpath,ffname,fmag);
#endif
  for(i=0, font=fontdatabase;i<nfontdatabase;i++,font++) {
    if(font->usecount<0) continue;
    if(font->fodvimag != dvimag) continue;
    if(font->foscf != scf) continue;
    if(font->fodsz != dsz) continue;
    if(strcmp(font->fonam,ffname)) continue;
    if(strcmp(font->fopath,ffpath)) continue;
#ifdef DEBUGFONTDATABASE
    pfprot(" found)");
#endif
    return(i);
  }
#ifdef DEBUGFONTDATABASE
  pfprot(" not found)");
#endif
  return(-1);
}


void cleanfontdatabase(void) {
  fontdesc* font;
  int c;
  int i,j;
#ifdef DEBUGFONTDATABASE
  pfprot("(cleanfontdatabase ");
  j=0;
#endif
  for(i=0, font=fontdatabase; i<nfontdatabase;i++,font++) {
    if(font->usecount==0) {
#ifdef DEBUGFONTDATABASE
      j++;
      pfprot("-- font %d mch %d --",i,font->mch);
#endif
      for(c=0;c<=font->mch;c++)
        if (font->chv[c].fty & PKTYPE) {
          freelrumem(&(font->chv[c].bmp.bits));  
          freelrumem(&(font->chv[c].bmp2.bits));  
        }
      freemem(&(font->chv));
      freemem(&(font->pkfile));
      freemem(&(font->fonam));
      freemem(&(font->fopath));
      font->usecount=-1;
      font->mch=-1;
    }
  }
#ifdef DEBUGFONTDATABASE
  pfprot(" killed %d fontdescs)",j);
#endif
}

void killfontdatabase(void) {
  int i;
#ifdef DEBUGFONTDATABASE
  pfprot("(killfontdatabase)");
#endif
  for(i=0;i<nfontdatabase;i++)
    if(fontdatabase[i].usecount>=0) 
      fontdatabase[i].usecount=0;
  cleanfontdatabase();
  freemem(&fontdatabase);
  maxfontdatabase=0;
  nfontdatabase=0;
}


/*************************************************************************/
/* read from a dvifile ***************************************************/
/*************************************************************************/

FILE *dvifile=NULL;

void dvibad(char* s) {
  if(dvierrorflag==0) {
    pfprot("\nbad dvi: %s\n", s);  
    dvierrorflag=1;
  }
}

void dvibytes(char *s, uchar  l) {
  if(l==0 || dvierrorflag) return;
  s[0]=0;  
  s[l] = '\0';  
  if(fread(s,1,(long) l,dvifile)==0) dvibad("ends prematurely !");
}

uchar dvibyte(void) { 
  char rbuf[1]={138};
  if(dvierrorflag) return(138);  
  if(fread(rbuf, 1, 1, dvifile)==0) dvibad("ends prematurely !");
  return(rbuf[0]);
}

void dviskip(long l)  {  
  if(dvierrorflag) return;  
  if(fseek(dvifile,l,1)!=0) dvibad("ends prematurely !");
} 

long dvinum(uchar b)  {  
  uchar i;  
  long n;
  uchar rbuf[4]={0,0,0,0};
  if(dvierrorflag) return(0L);  
  if(fread(rbuf, b, 1, dvifile)==0) dvibad("ends prematurely !");
  n = 0;
  for (i = 1; i <= b; i++)  
    n = (n << 8) | (uchar) rbuf[i-1];  
  return n;
}

long dviint(uchar b)  {  
  uchar i;  
  long n;
  uchar rbuf[4]={0,0,0,0};
  if(dvierrorflag) return(0L);  
  if(fread(rbuf, b, 1, dvifile)==0) dvibad("ends prematurely !"); 
  n = rbuf[0];
  if (n > 127)  n -= 256;
  for (i = 2; i <= b; i++)  
    n = (n << 8) | rbuf[i-1];  
  return n;
}

void dviposit(long l) {
  if(dvierrorflag) return;  
  if(fseek(dvifile, l, 0) !=0) dvibad("ends prematurely !") ;
}

long dvitell(void) {
  if(dvierrorflag) return(0L);  
  return(ftell(dvifile));
}

int dviopen(void) { 
  dvifile= fopen(visfmk->dvifilename, "rb");
  return(dvifile != NULL);
}

void dviclose(void) {
  if(dvifile != NULL) 
    fclose(dvifile);
  dvifile=NULL;
}

void dvipositpost(void) {

  long l;  
  uchar j, em;  

#ifdef DEBUGDVI
  pfprot("(postposit ... ");
#endif
  if(dvierrorflag) return;  
  dviposit(0L);
  if (dvibyte() != 247) { 
    dvibad("no valid preamble");
    return;
  }
  em = dvibyte();
  fseek(dvifile,0L,2);
  do {
    fseek(dvifile,-2L,1);
    j=dvibyte();
  } while (j == 223);
  if (j != em) {
    dvibad("no valid postamble");
    return;
  }
  fseek(dvifile, -5L, 1);
  l = dvinum(4);  
  fseek(dvifile, l, 0);  
  if (dvibyte() != 248) dvibad("no valid postamble");
#ifdef DEBUGDVI
  pfprot(")");
#endif
}


/************************************************************************/
/* process a dvifile ****************************************************/
/************************************************************************/



/* local globals for dvi-file-processing */

static long hs[SMAX], vs[SMAX], ws[SMAX], xs[SMAX], ys[SMAX], zs[SMAX];
static char comment[257], dvicomment[257];

static double conv, vconv, sconv, svconv;
static long  dvimag ,a, b, q, w, x, y, z, h, v;
static long  mag, spl,  mxh, mxv, lastpage;
static int k;

static ushort s, mst;

void fontdef(long fontnr) { 
  uchar  p, q;
  int f,fdb,a;
  long dsz,scf,chksum;
  float fontmag;  
  fontdesc *font;
  char ffpath[MAXPATHSTR], fftuto[MAXPATHSTR];
  char fontname[MAXPATHSTR];
  char messtr[MAXPATHSTR+20];

  if (fontnr < 0)  {
    dvibad("negative fontnumber");
    return;
  } 
  chksum = dvinum(4);  
  scf = dvinum(4);  
  dsz = dvinum(4);  
  p = dvibyte(); 
  q = dvibyte();  
  dvibytes(fontname, q);
  if (p == 0)  strcpy(ffpath, fontprefix);  
  else dvibytes(ffpath, p); 
  fontmag = xres * (dvimag / 1000.0 * ((double)scf / dsz));
  pfprot("preparing font: %8s at %6.1f ", fontname, fontmag);
  pfverb("(scf %d dsz %d) ",scf,dsz);
  /* messaging: this is defined in tmview.c !! */
  sprintf(messtr,"%10s at %6.1f",fontname,fontmag);
  if(proceedthis(messtr)) {dvibad("user interrupt"); return;}

  if((fdb=searchfontdatabase(ffpath,fontname,dvimag,scf,dsz))!=-1) {
    pfprot(" --- found in fontdatabase");
    font=fontdatabase+fdb;
  } else {
    fdb=allocfontdesc(ffpath,fontname,dvimag,scf,dsz);
    font=fontdatabase+fdb;  
    font->pkfile = NULL;
    allocmem(&(font->chv),(MAXFONTS+1) * sizeof(chdesc)); 
    memset(font->chv,0,(MAXFONTS+1) * sizeof(chdesc));  
                                               /* unsused bitmaps=NULL */
                                               /* unused  fty  0 */
                                               /* unused dimensions 0*/
    if(pkfind(ffpath,fontname,fontmag,fftuto))
      pkdef(fftuto,fdb,chksum);
    if(tfmfind(tfmprefix,fontname,fftuto))
      tfmdef(fftuto,fdb,chksum);
    reallocmem(&(font->chv),sizeof(chdesc)*(font->mch + 2));
                                            /* +1 is enuough ?? */
  }
  f = fontnr & 0xff;  a = 0;
  if ((fontnr >= MAXFONTS) || (fontvect[f].fontdataptr != -1)) do {
    if (fontvect[a].fontdataptr == -1)  f = a;
    else if (fontvect[a].dfn == fontnr)  {
      dvibad("double fontnumber");
      return;
    }
    a++;
  } while (a<MAXFONTS);
  if (fontvect[f].fontdataptr != -1) {
    dvibad("too many fonts");
    return;
  }
  font->usecount++;
  fontvect[f].fontdataptr=fdb;
  fontvect[f].dfn = fontnr;
  pfprot("\n");
}


uchar getintnr(long fontnr) {   
  int a, f, atleast;
  f = fontnr & 0xff;  
  if (fontvect[f].fontdataptr != -1) 
    if (fontvect[f].dfn == fontnr)
      return(f);
  a = 0;
  do {
    if (fontvect[a].fontdataptr == -1) a++;
      else if (fontvect[a].dfn != fontnr) atleast=a++;
        else return(a);
  } while (a < MAXFONTS);
  pfprot("\n %d ->",fontnr);
  dvibad("is not defined as a fontnumber");
  return(atleast);
}



static void pagebeg(void) {  
  uchar j;
  for (j = 0; j <= 9; j++)  /* count[j] = forget it */ dvinum(4);
  dvinum(4);
  s = 0; 
  w = 0;  
  x = 0;  
  y = 0;  
  z = 0;    
  h = 0;
  v = 0;
  htex_beginpage(cpage);
}

static void pageend(void) {
  if (s != 0)  pfprot("warning: stack not empty at eop");
  htex_endpage(); 
}


static void preamb(void) {  
  uchar l;
  long num,den;
#ifdef DEBUGDVI
  pfprot("(preamb ... ");
#endif
  l = dvibyte();  
  num = dvinum(4);  
  den = dvinum(4);
  den = MAX(1,den); 
  dvimag = dvinum(4);  
  if (newmag != 0) dvimag = newmag;
  conv = num / 254000.0 * ((double)xres / den) * (dvimag / 1000.0);
  vconv = num / 254000.0 * ((double)yres / den) * (dvimag / 1000.0);
  l = dvibyte();  dvibytes(dvicomment, l);  pfprot("%s\n", dvicomment);
#ifdef DEBUGDVI
  pfprot("num %d den %d mag %d .. preamb done)",num,den,dvimag);
#endif
}

static int testdvi(void) {
  uchar l;
  dviposit(14L); 
  l=dvibyte();
  dvibytes(comment, l);
#ifdef DEBUGDVI
  pfprot("(test dvi <%s> <%s>)",dvicomment,comment);
#endif
  if(strcmp(comment,dvicomment)==0) return(1);
  else return(0);
}
  

static void postamb(void) {
  long num,den;
#ifdef DEBUGDVI
  pfprot("(postamb ... ");
#endif
  lastpage=dvinum(4);  num = dvinum(4);  den = dvinum(4);
  dvimag = dvinum(4);  if (newmag != 0) dvimag = newmag;
  den=MAX(1,den);
  conv = num / 254000.0 * ((double)xres / den) * (dvimag / 1000.0);
  vconv = num / 254000.0 * ((double)yres / den) * (dvimag / 1000.0);
  mxv = dvinum(4);  mxh = dvinum(4);  mst = dvinum(2);  pageanz = dvinum(2);
#ifdef DEBUGDVI
  pfprot(")");
#endif
}


static initpagelist(void) {
  short i;
  uchar o;
#ifdef DEBUGDVI
  pfprot("(initpagelist: pagenaz %d ",pageanz);
#endif
  allocmem(&pagelist,MAX(1,pageanz)*sizeof(pagelistelement));
  memset(pagelist,0,MAX(1,pageanz)*sizeof(pagelistelement));
  cpage=pagelist;
  if(pageanz==0) {
    dvibad("zero pages ?");
    return;
  }
  i=pageanz;
  pagelist[--i].addr=lastpage;
  while(i>=0) {
    dviposit(pagelist[i].addr);
    o= dvibyte();
    if(o!=139) {
      dvibad("corruptet pagelist");
      return;
    }
    for (o = 0; o <= 9; o++)  pagelist[i].count[o] = dvinum(4);
    pagelist[i].num=i;
    if(i==0) break;                           /*SCHLEIGENENDE HIER !!! */
    /*pfprot("%3d %3d %3d %3d %3d %3d %3d %3d %3d %3d  \n",
      pagelist[i].count[0],pagelist[i].count[1],pagelist[i].count[2],
      pagelist[i].count[3],pagelist[i].count[4],pagelist[i].count[5],
      pagelist[i].count[6],pagelist[i].count[7],pagelist[i].count[8],
      pagelist[i].count[9]);*/
    pagelist[--i].addr=dvinum(4);
  }
#ifdef DEBUGDVI
  pfprot(")");
#endif 
}    
  
  
short gotoprpage(short pn) {
  uchar o;
  pn=MIN(pageanz-1,pn);
  pn=MAX(0,pn);
  dviposit(pagelist[pn].addr);
  o= dvibyte();
  if(o!=139) 
    dvibad("corruptet pagelist");
  if(dvierrorflag)
    return(0);
  return(pn);
}

short gotocccpage(double* ccc, short sp) {
  short i;
  /*fprintf(prot, "(gotocccpage pos %d ccc %f %f %f %f %f %f %f %f %f %f )",
    sp,ccc[0], ccc[1], ccc[2], ccc[3], ccc[4], ccc[5], ccc[6],
    ccc[7], ccc[8], ccc[9]); */
  if(dvierrorflag) return(0);
  sp=MIN(pageanz-1,MAX(0,sp)); /* for safty only ?? */
  i=sp+1;
  if(i==pageanz) i=0;
  do {
   if((ccc[0] == PGMAGIC || ccc[0] == pagelist[i].count[0]) &&
      (ccc[1] == PGMAGIC || ccc[1] == pagelist[i].count[1]) &&
      (ccc[2] == PGMAGIC || ccc[2] == pagelist[i].count[2]) &&
      (ccc[3] == PGMAGIC || ccc[3] == pagelist[i].count[3]) &&
      (ccc[4] == PGMAGIC || ccc[4] == pagelist[i].count[4]) &&
      (ccc[5] == PGMAGIC || ccc[5] == pagelist[i].count[5]) &&
      (ccc[6] == PGMAGIC || ccc[6] == pagelist[i].count[6]) &&
      (ccc[7] == PGMAGIC || ccc[7] == pagelist[i].count[7]) &&
      (ccc[8] == PGMAGIC || ccc[8] == pagelist[i].count[8]) &&
      (ccc[9] == PGMAGIC || ccc[9] == pagelist[i].count[9]) )
      break;
   i++;
   if(i==pageanz) i=0;
 } while(i!=sp);
 /*fprintf(prot,"--- found (?) %d )",i);*/ 
 return(i);
}
    


void initdvi(void) { 
  long f;
  uchar o;
  int i;
#ifdef DEBUGDVI
  pfprot("(initdvi...");
#endif
  dviname=visfmk->dvifilename;
  for(i=0;visfmk->dvifilename[i]!=0;i++)
    if(visfmk->dvifilename[i]==DIRSEPCHAR)
      dviname=visfmk->dvifilename+i+1; 
  
  pfprot("\nfile: %s\n",visfmk->dvifilename);
  dvierrorflag=0;

  allocmem(&fontvect,MAXFONTS*sizeof(fontdescvect));
  memset(fontvect,-1,MAXFONTS*sizeof(fontdescvect)); /* usecount -1 */
 
  if(!dviopen()) {
    dvibad("cant open dvifile"); 
  }
  dviposit(0L);
  if (dvibyte() != 247) {
    dvibad("no valid preamble");
  }
  preamb(); 
  dvipositpost();
  postamb();
  o=0;
  while(o != 249 && dvierrorflag==0) {
    o = dvibyte(); 
    switch (o) {
      case 243:  case 244:  case 245:  case 246:
        f = dvinum(o - 242);  fontdef(f);  break;
      case 249: break;
      default: dvibad("there not only fontdefs in postamble ...");
    }
  }
  initpagelist();
  htex_init();
  dviclose();
#ifdef DEBUGDVI
  pfprot("...initdvi)");
#endif 
}


void killdvi(void) {
  int i;
  int c;
#ifdef DEBUGDVI
  pfprot("(killdvi ...");
#endif 
  /* dviclose(); */
  drawlistclear();
  if(fontvect!=NULL){
    for(i=0;i<MAXFONTS;i++) {
      if (fontvect[i].fontdataptr != -1)
        (fontdatabase[fontvect[i].fontdataptr].usecount)--;
    }
    freemem(&fontvect);
  }
  freemem(&greytab);
  htex_kill();
  freemem(&pagelist);
  pageanz=0;
  cpage=NULL;
#ifdef DEBUGDVI
  pfprot(")");
#endif 
}


static void listchar(int c,int fontintnr) {
  
  fontdesc *font;  
  chdesc *thechar;
  short hr,vr,ar,br;
  drawlistdata* data;

  font = fontdatabase+fontvect[fontintnr].fontdataptr;
  if(font->mch < c) {
    pfprot(
    "(putchar: warning: skipping undefined charakter %d in %s)",c,font->fonam);
    q = 0;
    return;
  }
  /*pfprot("(listchar %d ... )",c);*/ 
  /*if(font->encoding)
     fprintf(prot,"%s",font->encoding[c]); */ 
  thechar=font->chv+c;
  if (thechar->fty & TFMTYPE)       
    htex_recordbits(h,v-thechar->tfmtfh,
               thechar->tfmtfw,thechar->tfmtfd+thechar->tfmtfh);  
  else 
    htex_recordbits(h,v,1,1);  
  if(thechar->fty & PKTYPE) {  /* try pk first*/
    /*fprintf(prot," its a pk: c %d fin %d h %d v %d ...)",c,fontintnr,h,v);*/ 
    q = thechar->tfw;
    if(fshrink == 1.0) {
      data=drawlistadd(*drawcharbmp);
      (*data).chr.h= LROUND(conv * h);
      (*data).chr.v= LROUND(vconv * v);
      (*data).chr.chdp=thechar; 
      return;
    }
    if(colors == BLACKNWHITE || fshrink < 1.0) { 
      data=drawlistadd(*drawcharbmp2bw);
      (*data).chr.h= LROUND(sconv * h);
      (*data).chr.v= LROUND(svconv * v);
      (*data).chr.chdp=thechar; 
      return;
    } 
    /*if shrinked and greyed { ... */
    /*pfprot("do shr&gs");*/
    data=drawlistadd(*drawcharbmp2gs);
    (*data).chr.h= LROUND(sconv * h);
    (*data).chr.v= LROUND(svconv * v);
    (*data).chr.chdp=thechar; 
    return;
  } /* end pk */
  if (thechar->fty & TFMTYPE) {                           /* try tfm */
    hr = LROUND(sconv * h);
    vr = LROUND(svconv * (v - thechar->tfmtfh));
    br = LROUND(sconv * (thechar->tfmtfw));
    ar = LROUND(svconv * (thechar->tfmtfd + thechar->tfmtfh));
    data=drawlistadd(*drawrulebg);
    (*data).rect.c=TFMCOL;
    (*data).rect.x=hr; (*data).rect.y=vr;
    (*data).rect.w=br; (*data).rect.h=1;  
    data=drawlistadd(*drawrulebg);
    (*data).rect.c=TFMCOL;
    (*data).rect.x=hr; (*data).rect.y=vr+ar-1;
    (*data).rect.w=br; (*data).rect.h=1;  
    data=drawlistadd(*drawrulebg);
    (*data).rect.c=TFMCOL;
    (*data).rect.x=hr; (*data).rect.y=vr;
    (*data).rect.w=1;  (*data).rect.h=ar;  
    data=drawlistadd(*drawrulebg);
    (*data).rect.c=TFMCOL;
    (*data).rect.x=hr+br-1; (*data).rect.y=vr;
    (*data).rect.w=1;       (*data).rect.h=ar;   
    /* drawlistadd(*drawrulebg,hr, vr, br, ar, TFMCOL, NULL); */ 
    q = thechar->tfmtfw;
    return;  
  } /* end tfm */  
  pfprot(
   "(putchar: warning: skipping undefined charakter %d in %s)",c,font->fonam);
  q = 0;
}


#define MAXSPECIAL 500  /*lazy, but don't expect long specials */
char specstr[MAXSPECIAL];

static void listspecial(void) {  
  long i;
  
  if(spl>=MAXSPECIAL) {
    pfprot("(special too long: ");
    for (i = spl; i > 0; i--)  pfprot("%c",0x7f & dvibyte());
    pfprot(")");
    return;
  }
  dvibytes(specstr,spl);
  if(checkndoHyperTeX(specstr,h,v)) return;
  /* insert other specials here */
  pfprot("(special <%s> ignored)",specstr);  
};


static void listrule(void) {  
  double le,bo,hi,wi;
  ushort gs;
  drawlistdata* data;
  if ((b <= 0) || (a<=0))  return;
  htex_recordbits(h,v,b,a); 
 
  le = LROUND(sconv * h);
  bo = LROUND(svconv * v);
  hi = svconv * a;  
  wi = sconv * b;
 
  /*fprintf(prot,
       "(listrule: le %5.1f bo %5.1f wi %5.1f hi %5.1f) ",le,bo,wi,hi);*/ 
  if(fshrink == 1.0) {        /* do it like they told me ... not shrunken*/  
    wi=LCEIL(wi); hi=LCEIL(hi);
    data=drawlistadd(*drawruleor);
    (*data).rect.x=le;
    (*data).rect.y=bo-hi+1;
    (*data).rect.w=wi;
    (*data).rect.h=hi;
    (*data).rect.c=BLACKCOL;
    return;
  }   
  if(colors==BLACKNWHITE) {                     /* shrunken, blacknwhite*/
    wi=MAX(1,LROUND(wi));                    /*enshure drawing something*/
    hi=MAX(1,LROUND(hi)); 
    data=drawlistadd(*drawruleor);
    (*data).rect.x=le;
    (*data).rect.y=bo-hi+1;
    (*data).rect.w=wi;
    (*data).rect.h=hi;
    (*data).rect.c=BLACKCOL;
    return;
  }                                                    /* is greyscaled*/
  data=drawlistadd(*drawruleor);
  (*data).rect.x=le;
  (*data).rect.y=bo-LFLOOR(hi)+1;
  (*data).rect.w=LFLOOR(wi);
  (*data).rect.h=LFLOOR(hi);
  (*data).rect.c=BLACKCOL;
  gs=(uchar)(LROUND((COLORS_PER_GREY-1)*(wi-LFLOOR(wi)))); 
  if(wi < 1) gs=MAX(1,gs);
  data=drawlistadd(*drawruleor);
  (*data).rect.x=le+LFLOOR(wi);
  (*data).rect.y=bo-LROUND(hi)+1;
  (*data).rect.w=1;
  (*data).rect.h=LROUND(hi);
  (*data).rect.c=gs; 
  gs=(uchar)(LROUND((COLORS_PER_GREY-1)*(hi-LFLOOR(hi))));  
  if(hi < 1) gs=MAX(1,gs);
  data=drawlistadd(*drawruleor);
  (*data).rect.x=le;
  (*data).rect.y=bo-LFLOOR(hi);
  (*data).rect.w=LROUND(wi);
  (*data).rect.h=1;
  (*data).rect.c=gs; 
  /*pfprot("did id\n");*/
}


int  readpage(short pn, float fshr, uchar col,int force){
  uchar o,c;
  int fint,i,ishr;
  ushort d1,d2;
  long f;

#ifdef DEBUGDVI
  pfprot("(readpage ...");
#endif 

  if(!dviopen()) {
    dvibad("dvifile disappeard"); 
  }
  if(!testdvi()) {
    pfprot("\ndvifile has changed? reload\n");
    killdvi();
    initdvi();
    dviopen();
  }
 
  fshr=MAX(MINSHRINK,MIN(MAXSHRINK,fshr));
  if(ishrinkonly)
    fshr=MAX(1,LROUND(fshr));
  else
    fshr=(ROUND(100*fshr)/100.0); 
  if(col != colors || fshr != fshrink || force) {
    if(col == GREYSCALE) {
      freemem(&greytab);
      ishr=ROUND(fshr);  
      allocmem(&greytab,(ishr+1)*(ishr+1)+1); /* fuck +1 */
      for(i=0;i<=ishr*ishr;i++) 
        greytab[i]=ROUNDUP(i*(COLORS_PER_GREY-1),ishr*ishr);
      for(;i<=(ishr+1)*(ishr+1);i++) 
        greytab[i]=COLORS_PER_GREY-1;
    } else col=BLACKNWHITE;
  }
  pn=gotoprpage(pn);
  if(!force)                             /* protect for cpage==NULL */
  if(col == colors && fshr == fshrink && 
    pn==cpage->num){
    dviclose();
#ifdef DEBUGDVI
    pfprot("... not realy readpage)");
#endif 
    return(0);
  }
  cpage=pagelist+pn;
  fshrink=fshr;
  colors=col;
  sconv=conv/fshr;
  svconv=vconv/fshr;
  drawlistclear();
  if(dvierrorflag) return(1);
  pfprot("[%d",cpage->num);
  pagebeg();
  q=0;     
  o = dvibyte();
  while (o != 140 && dvierrorflag ==0) {
    /*pfprot("(%d)",o);*/
    if (o <= 127) {
       listchar(o,fint);  h += q;
    } else if (o >= 171 && o <= 234) {
	 fint=getintnr(o-171);
    } else
    switch (o) {
    case 128:  case 129:  case 130:  case 131:
      c = dvinum(o - 127);  listchar(c,fint);  h += q;  break;
    case 132:  a = dviint(4);  b = dviint(4);
      listrule();  h += b;  break;
    case 133:  case 134:  case 135:  case 136:
      c = dvinum(o - 132);  listchar(c,fint);  break;
    case 137:  a = dviint(4);  b = dviint(4);
      listrule();  break;
    case 138: break;
    case 139: dvibad("unexpectet bop"); break;
    case 140: break;
    case 141:  if (s < SMAX) {	s++;  hs[s - 1] = h;  vs[s - 1] = v;
	ws[s - 1] = w;	xs[s - 1] = x;	ys[s - 1] = y; zs[s - 1] = z;
      } else  dvibad("stack overflow");  break;
    case 142:  if (s > 0) {  h = hs[s - 1];  v = vs[s - 1];
	w = ws[s - 1];	x = xs[s - 1];	y = ys[s - 1];	z = zs[s - 1];	s--;
      } else  dvibad("stack underflow");  break;
    case 143:  case 144:  case 145:  case 146:
      h += dviint(o - 142);  break;
    case 147:  h += w;  break;
    case 148:  case 149:  case 150:  case 151:
      w = dviint(o - 147);  h += w;  break;
    case 152:  h += x;  break;
    case 153:  case 154:  case 155:  case 156:
      x = dviint(o - 152);  h += x;  break;
    case 157:  case 158:  case 159:  case 160:
      v += dviint(o - 156);  break;
    case 161:  v += y;  break;
    case 162:  case 163:  case 164:  case 165:
      y = dviint(o - 161);  v += y;  break;
    case 166:  v += z;  break;
    case 167:  case 168:  case 169:  case 170:
      z = dviint(o - 166);  v += z;  break;
    case 235:  case 236:  case 237:  case 238:
      f = dvinum(o - 234);  fint=getintnr(f);  break;
    case 239:  case 240:  case 241:  case 242:
      spl = dvinum(o - 238);  listspecial();  break;
    case 243:  case 244:  case 245:  case 246:
      f = dvinum(o-242);
      /* fprintf(prot,"(fontdef:%d)",f); */
      dviskip(12L); 
      d1 = dvibyte();  d2 = dvibyte();  dviskip((long)(d1 + d2)); break;
    case 250:  case 251:  case 252:
    case 253:  case 254:  case 255:  
      pfprot("skipping unknown opc > 249 ");break;
    default:
    } /* end switch o */
    o = dvibyte();
  } 
  htex_scalebox(sconv,svconv);
  pageend();
  pfprot("] ");
  dviclose();
  if(dvierrorflag)   
    drawlistclear();
#ifdef DEBUGDVI
  pfprot("... done readpage)");
#endif 
  return(1);
}    
    


/****************************************************************************/
/* searching textstrings ****************************************************/
/****************************************************************************/


/*  searchmachine ?? is it good? is it bad? */
/*  lots of global data is used ?!? */



static int fstart,fend, found;
static int boundx1pxl, boundx2pxl, boundy1pxl, boundy2pxl;
static long dpos;
static int dpage;

static char errorstr[MAXARGSTR+1];

#define MAXSBUF MAXARGSTR

typedef struct positioninfo {
  long pos; 
  int page;
  int x1pxl;
  int y1pxl;
  int x2pxl;
  int y2pxl;
} positioninfo;

#define INFLEN sizeof(positioninfo)

static positioninfo sbufinfo1[MAXSBUF+1];
static positioninfo sbufinfo2[MAXSBUF+1];
static char sbufstr1[MAXSBUF+1];
static char sbufstr2[MAXSBUF+1];
static positioninfo *sbufinfoc, *sbufinfoo;
static char *sbufstrc, *sbufstro;
static int slenc, sleno;

static struct re_pattern_buffer repatbuf;
static struct re_registers reregs;
static char schnell[256];


positioninfo* cxinfo(int idx) {
  if(idx <sleno) return(sbufinfoo+idx);
  return(sbufinfoc+(idx-sleno));
}

/*
char* cxstr(int idx) {
  if(idx <sleno) return(sbufstro+idx);
  return(sbufstrc+(idx-sleno));
}
*/

void searchsbuf(void) {
  int res;
  res=re_search_2(&repatbuf,sbufstro,sleno,sbufstrc,slenc,
            0,sleno,&reregs,slenc+sleno);
#ifdef DEBUGDVI
  pfprot("\n bufo (%d) bufc (%d)\n%s%s\n",sleno,slenc,sbufstro,sbufstrc);
  pfprot(" -- res %d numregs %d-- ",res,reregs.num_regs);
  if(reregs.num_regs > 0)
    pfprot("start[0] %d end[0] %d --",reregs.start[0],reregs.end[0]); 
#endif
  if(res>-1 && reregs.num_regs > 0) {  /* asking num_regs for safty only */
     fstart=MIN(MAX(0,reregs.start[0]),sleno+slenc-1); /* for safty only */
     fend=MIN(MAX(0,reregs.end[0]),sleno+slenc-1);     /* for safty only */
     found=1;
  }
}


void settextmark(void){
  int i;
#ifdef DEVUGDVI
  pfprot("(settextmark ...");
#endif
  textx1pxl=cxinfo(fstart)->x1pxl;
  texty1pxl=cxinfo(fstart)->y1pxl;
  textx2pxl=cxinfo(fstart)->x2pxl;
  texty2pxl=cxinfo(fstart)->y2pxl;  
  for(i=fstart+1; i<fend; i++) {
    if(cxinfo(i)->y1pxl>texty2pxl && cxinfo(i)->x2pxl<textx1pxl) break;
    if(cxinfo(i)->page != cxinfo(i-1)->page) break;
    if(textx1pxl>cxinfo(i)->x1pxl) textx1pxl=cxinfo(i)->x1pxl;
    if(texty1pxl>cxinfo(i)->y1pxl) texty1pxl=cxinfo(i)->y1pxl;
    if(textx2pxl<cxinfo(i)->x2pxl) textx2pxl=cxinfo(i)->x2pxl;
    if(texty2pxl<cxinfo(i)->y2pxl) texty2pxl=cxinfo(i)->y2pxl;
  }
  textx3pxl=textx1pxl;
  texty3pxl=texty1pxl;
  textx4pxl=textx1pxl;
  texty4pxl=texty1pxl;
  if(i<fend) {if(cxinfo(i)->page == cxinfo(i-1)->page) {
    textx3pxl=cxinfo(i)->x1pxl;
    texty3pxl=cxinfo(i)->y1pxl;
    textx4pxl=cxinfo(i)->x2pxl;
    texty4pxl=cxinfo(i)->y2pxl;  
    for(i++; i<fend; i++) {
      if(cxinfo(i)->y1pxl>texty4pxl && cxinfo(i)->x2pxl<textx3pxl) break;
      if(cxinfo(i)->page != cxinfo(i-1)->page) break;
      if(textx3pxl>cxinfo(i)->x1pxl) textx3pxl=cxinfo(i)->x1pxl;
      if(texty3pxl>cxinfo(i)->y1pxl) texty3pxl=cxinfo(i)->y1pxl;
      if(textx4pxl<cxinfo(i)->x2pxl) textx4pxl=cxinfo(i)->x2pxl;
      if(texty4pxl<cxinfo(i)->y2pxl) texty4pxl=cxinfo(i)->y2pxl;
    }
  }}
#ifdef DEVUGDVI
  pfprot(")");
#endif   
}  


void searchsetq(int c,int fontintnr) {
  
  fontdesc *font;  
  chdesc *thechar;

  font = fontdatabase+fontvect[fontintnr].fontdataptr;
  if(font->mch < c) {
    q = 0;
  } else {
    thechar=font->chv+c;
    if (thechar->fty == TFMTYPE)  /* is only tfm !!! */    
       q = thechar->tfmtfw;
    else                      /* else it must be a pk */ 
       q = thechar->tfw;
  }
}


void searchchar(int c,int fontintnr) {
  
  fontdesc *font;
  chdesc *achar;  
  int alen,i,x1pxl,x2pxl,y1pxl,y2pxl;
  char *astr;

  font = fontdatabase+ fontvect[fontintnr].fontdataptr;
  q=0;
  if(font->mch < c)  return;
  achar=font->chv+c;
  if(achar->fty == TFMTYPE)    /* is only tfm !!! */    
    q = achar->tfmtfw;
  else                         /* else it must be a pk */ 
    q = achar->tfw;
  if(!font->encoding) return;
  if (achar->fty & TFMTYPE) {       
      x1pxl=LROUND(sconv  * h);
      y1pxl=LROUND(svconv * (v-achar->tfmtfh));
      x2pxl=LROUND(sconv  * (h+achar->tfmtfw));
      y2pxl=LROUND(svconv * (v+achar->tfmtfd));  
  } else { 
      x1pxl=LROUND(sconv  * h);
      y1pxl=LROUND(svconv * v);
      x2pxl=LROUND(sconv  * h);
      y2pxl=LROUND(svconv * v);  
  }  
  if(x1pxl<boundx1pxl) return;
  if(x2pxl>boundx2pxl) return;
  if(y1pxl<boundy1pxl) return;
  if(y2pxl>boundy2pxl) return;
  astr=font->encoding[c];
  alen=strlen(astr);
  if(alen==0) return;
  if(alen+slenc > MAXSBUF) {                        /* dothesearch */
    searchsbuf();
    if(found) return;
    if(sbufstrc==sbufstr1) {   /* sawp buffer 1 and 2 */  
      sbufstrc=sbufstr2;
      sbufstro=sbufstr1;
      sbufinfoc=sbufinfo2;
      sbufinfoo=sbufinfo1;
    } else {
      sbufstrc=sbufstr1;
      sbufstro=sbufstr2;
      sbufinfoc=sbufinfo1;
      sbufinfoo=sbufinfo2;
    } 
    sleno=slenc;
    sbufstrc[0]=0; 
    slenc=0;
  }                    
  strcat(sbufstrc,astr);   /* add to buffer */
  sbufinfoc[slenc].pos=dpos;
  sbufinfoc[slenc].page=dpage;    
  sbufinfoc[slenc].x1pxl=x1pxl;
  sbufinfoc[slenc].y1pxl=y1pxl;
  sbufinfoc[slenc].x2pxl=x2pxl;
  sbufinfoc[slenc].y2pxl=y2pxl;  
  for(i=1;i<alen;i++)
    memmove(sbufinfoc+i+slenc,sbufinfoc+slenc,INFLEN);  
  slenc+=alen;
} 


void searchreaddvi(int *spage, long *spos) {
  long startpos;
  uchar o,c;
  int fint;
  ushort d1,d2;
  char messtr[20];
  long f;

  dpage=*spage-1;
  startpos=*spos;
#ifdef DEBUGDVI
  pfprot("(searchraeddvi spage %d spos %d ...",*spage,*spos);
#endif
  do { /* run through pages */
    dpage++;
    if(dpage != gotoprpage(dpage)) break;
    sprintf(messtr,"      [%d]",MAX(-9999,MIN(pagelist[dpage].count[0],9999)));
    if(proceedthis(messtr)) break; 
    dpos=0;
    dviskip(44L); /* counts and 4*/
    s = 0; w = 0; x = 0; y = 0; z = 0;  h = 0; v = 0; q=0;
    o = dvibyte(); 
    while (!dvierrorflag && o != 140 && found!=1) {
      /* pfprot("(%d)",o); */ 
       switch (o) {
      case 128:  case 129:  case 130:  case 131:
        c = dvinum(o - 127);   
        if(dpos>=startpos) searchchar(c,fint);
        searchsetq(c,fint);   
        h += q;  break;
      case 132:  a = dviint(4);  b = dviint(4); h += b;  break;
      case 133:  case 134:  case 135:  case 136:
        c = dvinum(o - 132);  
        if(dpos>=startpos) searchchar(c,fint);  break;
      case 137:  a = dviint(4);  b = dviint(4); break;
      case 138: break;
      case 139: dvibad("unexpectet bop"); break;
      case 140: break;
      case 141:  if (s < SMAX) {	s++;  hs[s - 1] = h;  vs[s - 1] = v;
	  ws[s - 1] = w;	xs[s - 1] = x;	ys[s - 1] = y; zs[s - 1] = z;
        } else  dvibad("stack overflow");  break;
      case 142:  if (s > 0) {  h = hs[s - 1];  v = vs[s - 1];
	w = ws[s - 1];	x = xs[s - 1];	y = ys[s - 1];	z = zs[s - 1];	s--;
        } else  dvibad("stack underflow");  break;
      case 143:  case 144:  case 145:  case 146:
        h += dviint(o - 142);  break;
      case 147:  h += w;  break;
      case 148:  case 149:  case 150:  case 151:
        w = dviint(o - 147);  h += w;  break;
      case 152:  h += x;  break;
      case 153:  case 154:  case 155:  case 156:
        x = dviint(o - 152);  h += x;  break;
      case 157:  case 158:  case 159:  case 160:
        v += dviint(o - 156);  break;
      case 161:  v += y;  break;
      case 162:  case 163:  case 164:  case 165:
        y = dviint(o - 161);  v += y;  break;
      case 166:  v += z;  break;
      case 167:  case 168:  case 169:  case 170:
        z = dviint(o - 166);  v += z;  break;
      case 235:  case 236:  case 237:  case 238:
        f = dvinum(o - 234);  fint=getintnr(f);  break;
      case 239:  case 240:  case 241:  case 242:
        spl = dvinum(o - 238); dviskip((long)spl); 
        break;
      case 243:  case 244:  case 245:  case 246:
        f = dvinum(o-242);
        dviskip(12L); 
        d1 = dvibyte();  d2 = dvibyte();  dviskip((long)(d1 + d2)); break;
      case 250:  case 251:  case 252:
      case 253:  case 254:  case 255:  
        break;
      default:
        if (o <= 127) { 
          if(dpos>=startpos) searchchar(o,fint);
          searchsetq(o,fint);  
          h += q;
        } else
          if (o >= 171 && o <= 234) {
	    fint=getintnr(o-171);
          } break;
      } /* end switch o */
      dpos++;
      o = dvibyte(); 
    } /* end a page */
    startpos=0;
  } while(found ==0); /*end search*/
  if(found==0)
    searchsbuf(); /* last chance to find anything ... */
  if(found) {
    *spos=dpos;
    *spage=dpage;
  }
#ifdef DEBUGDVI
  pfprot(" ... searchreaddvi done)");
#endif
}


const char* searchdvi(char* sstr, int* spage, long* spos){
  int ssl;
  int myspage;
  long myspos;
  const char* restr;

#ifdef DEBUGDVI
  pfprot("(searchdvi sstr (%d) %s ... ",ssl,sstr);
#endif
  if(!dviopen()) return(0);
  if(!testdvi()) {
    pfprot("\ndvifile has changed? reload\n");
    killdvi();
    initdvi();
    dviopen();
  }
  boundx1pxl=MMTOPXL(visfmk->lrandmm)-MMTOPXL(visfmk->hoffmm);
  boundy1pxl=MMTOPXL(visfmk->orandmm)-MMTOPXL(visfmk->voffmm);
  boundx2pxl=MMTOPXL(visfmk->papxmm-visfmk->rrandmm)-MMTOPXL(visfmk->hoffmm);
  boundy2pxl=MMTOPXL(visfmk->papymm-visfmk->urandmm)-MMTOPXL(visfmk->voffmm);
  sbufstrc=sbufstr1;
  sbufstro=sbufstr2;
  sbufinfoc=sbufinfo1;
  sbufinfoo=sbufinfo2;
  sbufstrc[0]=0; 
  sbufstro[0]=0; 
  slenc=0;
  sleno=0;
  ssl=strlen(sstr);
  if(ssl==0) return(0);
  repatbuf.buffer=NULL;
  repatbuf.allocated=0;
  repatbuf.translate=NULL;
  repatbuf.fastmap=schnell;
  re_syntax_options= RE_SYNTAX_POSIX_EXTENDED;	
  if((restr=re_compile_pattern(sstr,ssl,&repatbuf))!=NULL) {
#ifdef DEBUGDVI
    pfprot("\nregex-error %s)\n",restr);
#endif
    strcpy(errorstr,"REGEX PANIC: ");
    strncat(errorstr,restr,MAXARGSTR-13);
    errorstr[MAXARGSTR]=0;
    repatbuf.fastmap=NULL;
    regfree(&repatbuf); 
    return(errorstr);
  }
  found=0;
  myspage=*spage;
  myspos=*spos;
  searchreaddvi(&myspage,&myspos);
  if(found==1) {
    *spage=cxinfo(fstart)->page;
    *spos=cxinfo(fstart)->pos;
    settextmark();
  }
  repatbuf.fastmap=NULL;
  regfree(&repatbuf); 
  dviclose();
#ifdef DEBUGDVI
  pfprot(" ..done seachdvi)");
#endif
  if(found)
    return(NULL);
  strcpy(errorstr,"SEARCH FAILED");
  return(errorstr);
}  
    
/*****************************************************************************/
/* searching anchors (hypertex) **********************************************/
/*****************************************************************************/


void anchorcharq(int c,int fontintnr) {
  
  fontdesc *font;  
  chdesc *thechar;

  font = fontdatabase+fontvect[fontintnr].fontdataptr;
  if(font->mch < c) {q=0; return;}
  thechar=font->chv+c;
  if (thechar->fty == TFMTYPE)  /* is only tfm !!! */    
       q = thechar->tfmtfw;
  else                      /* else it must be a pk */ 
       q = thechar->tfw;
  if (thechar->fty & TFMTYPE)       
    htex_recordbits(h,v-thechar->tfmtfh,
               thechar->tfmtfw,thechar->tfmtfd+thechar->tfmtfh);  
  else 
    htex_recordbits(h,v,1,1);  
}


readanchors(int page) {
  uchar o,c;
  int fint,i,j,an;
  ushort d1,d2;
  long f,startpos;

  if(!dviopen()) return;
  if(!testdvi()) {
    pfprot("\ndvifile has changed? reload\n");
    killdvi();
    initdvi();
    dviopen();
  }
  if(page != gotoprpage(page)){ 
    dviclose();
    return;
  }
  dviskip(44L); /* counts and 4*/
  htex_beginpage(pagelist+page);
  s = 0; w = 0; x = 0; y = 0; z = 0;  h = 0; v = 0; q=0;
  o = dvibyte(); 
  while (!dvierrorflag && o != 140) {
    /* fprintf(prot,"(%d)",o);*/ 
    switch (o) {
    case 128:  case 129:  case 130:  case 131:
      c = dvinum(o - 127);
      anchorcharq(c,fint);   
      h += q;  break;
    case 132:  a = dviint(4);  b = dviint(4);  
      if ((b > 0) && (a>0)) 
        htex_recordbits(h,v,b,a); 
      h+=b;
      break;
    case 133:  case 134:  case 135:  case 136:
      c = dvinum(o - 132);
      anchorcharq(c,fint);  
      break;
    case 137:  a = dviint(4);  b = dviint(4); 
      if ((b > 0) && (a>0)) 
        htex_recordbits(h,v,b,a); 
      break;
    case 138: break;
    case 139: dvibad("unexpectet bop"); break;
    case 140: break;
    case 141:  if (s < SMAX) {	s++;  hs[s - 1] = h;  vs[s - 1] = v;
	ws[s - 1] = w;	xs[s - 1] = x;	ys[s - 1] = y; zs[s - 1] = z;
      } else  dvibad("stack overflow");  break;
    case 142:  if (s > 0) {  h = hs[s - 1];  v = vs[s - 1];
      w = ws[s - 1];	x = xs[s - 1];	y = ys[s - 1];	z = zs[s - 1];	s--;
      } else  dvibad("stack underflow");  break;
    case 143:  case 144:  case 145:  case 146:
      h += dviint(o - 142);  break;
    case 147:  h += w;  break;
    case 148:  case 149:  case 150:  case 151:
      w = dviint(o - 147);  h += w;  break;
    case 152:  h += x;  break;
    case 153:  case 154:  case 155:  case 156:
      x = dviint(o - 152);  h += x;  break;
    case 157:  case 158:  case 159:  case 160:
      v += dviint(o - 156);  break;
    case 161:  v += y;  break;
    case 162:  case 163:  case 164:  case 165:
      y = dviint(o - 161);  v += y;  break;
    case 166:  v += z;  break;
    case 167:  case 168:  case 169:  case 170:
      z = dviint(o - 166);  v += z;  break;
    case 235:  case 236:  case 237:  case 238:
      f = dvinum(o - 234);  fint=getintnr(f);  break;
    case 239:  case 240:  case 241:  case 242:
      spl = dvinum(o - 238);
      if(spl<MAXSPECIAL) {
       dvibytes(specstr,spl);
       checkndoHyperTeX(specstr,h,v);
      } else
        dviskip((long)spl); 
      break;
    case 243:  case 244:  case 245:  case 246:
      f = dvinum(o-242);
      dviskip(12L); 
      d1 = dvibyte();  d2 = dvibyte();  dviskip((long)(d1 + d2)); break;
    case 250:  case 251:  case 252:
    case 253:  case 254:  case 255:  
      break;
    default:
      if (o <= 127) { 
        anchorcharq(o,fint);  
        h += q;
      } else
        if (o >= 171 && o <= 234) {
	  fint=getintnr(o-171);
      } 
      break;
      } /* end switch o */
      o = dvibyte();
    } /* end a page */
  htex_endpage(); 
  dviclose();
}  


int  searchhref(char* href, int* spage, int* xpxl, int* ypxl){
  int an;
  char messtr[MAXPATHSTR+20];

    
  if(htex_is_url(href)) return(0); 
  an=htex_findonpage(href,pagelist+ *spage);
  if(an==-1) {
    if(pageanz>1) 
      htex_findondonepage(href,spage,&an);
    if(an== -1) {
      for(*spage=0;*spage<pageanz;(*spage)++) {
        if(pagelist[*spage].anchorsdone!=DONE) {
          sprintf(messtr,"      [%d]",
            MAX(-9999,MIN(pagelist[*spage].count[0],9999)));
          if(proceedthis(messtr)) break; 
          readanchors(*spage);
          an=htex_findonpage(href,pagelist+ *spage);
	}
        if(an!=-1) break;
      }
    }
  }
  if(an==-1) return(0);
  *xpxl=LROUND(sconv*pagelist[*spage].anchorlist[an].llx);
  *ypxl=LROUND(svconv*pagelist[*spage].anchorlist[an].lly);
  return(1);
}




