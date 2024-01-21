
/*
    xskat - a card game for 1 to 3 players.
    Copyright (C) 1996  Gunter Gerhardt

    This program is free software; you can redistribute it freely.
    Use it at your own risk; there is NO WARRANTY.
*/

#define XIO_C

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <ctype.h>
#include <X11/Xlib.h>
#include <X11/X.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include "skat.h"
#include "bitmaps.h"
#include "xio.h"
#include "text.h"

int left(s)
int s;
{
  return (s+1)%3;
}

change_gc(sn,fg,gcp)
int sn;
unsigned long fg;
GC *gcp;
{
  XGCValues gcv;

  gcv.foreground=fg;
  XChangeGC(dpy[sn],gcp[sn],GCForeground,&gcv);
}

change_gcbg(sn,bg,gcp)
int sn;
unsigned long bg;
GC *gcp;
{
  XGCValues gcv;

  gcv.background=bg;
  XChangeGC(dpy[sn],gcp[sn],GCBackground,&gcv);
}

change_gcxor(sn,fg)
int sn;
unsigned long fg;
{
  change_gc(sn,fg^bgpix[sn],gcxor);
}

int istrue(s)
char *s;
{
  char h[5];
  int i;

  for (i=0;i<4 && *s;i++,s++) {
    h[i]=tolower(*s);
  }
  h[i]=0;
  return !strcmp(h,"true");
}

v_gtextc(sn,c,x,y,w,t)
int sn,c,x,y,w;
char *t;
{
  int l;

  l=strlen(t);
  x+=(w-charw[sn]*l)/2;
  y+=(20-charh[sn])/2+dfont[sn]->ascent;
  if (c) {
    change_gcbg(sn,btpix[sn],gc);
  }
  XDrawImageString(dpy[sn],win[sn],gc[sn],x,y,t,l);
  XDrawImageString(dpy[sn],bck[sn],gc[sn],x,y,t,l);
  if (c) {
    change_gcbg(sn,bgpix[sn],gc);
  }
}

v_gtext(sn,x,y,w,t)
int sn,x,y,w;
char *t;
{
  v_gtextc(sn,0,x,y,w,t);
}

b_text(s,str)
int s;
char *str;
{
  int sn;

  for (sn=0;sn<numsp;sn++) {
    if (sn!=s) {
      v_gtextc(sn,1,s==left(sn)?desk.cbox1x:desk.cbox2x,desk.cboxy,64,str);
    }
  }
}

do_msaho(sn,str)
int sn;
char *str;
{
  v_gtextc(sn,1,desk.pboxx,desk.pboxy,64,str);
  v_gtextc(sn,1,desk.pboxx+64,desk.pboxy,64,textarr[TX_PASSE]);
}

draw_skat()
{
  putcard(spieler,cards[30],desk.skatx,desk.skaty);
  putcard(spieler,cards[31],desk.skatx+64,desk.skaty);
}

home_skat()
{
  homecard(spieler,desk.skatx,desk.skaty);
  homecard(spieler,desk.skatx+64,desk.skaty);
}

nimm_stich()
{
  int i;

  for (i=0;i<3;i++) {
    homecard(ausspl,desk.stichx+i*64,desk.stichy);
  }
}

drop_card(i,s)
int i,s;
{
  int sn,sna[3],x1[3],y1[3],x2[3],y2[3];

  for (sn=0;sn<numsp;sn++) {
    sna[sn]=sn;
    if (s==left(sn)) {
      x1[sn]=desk.com1x;
      y1[sn]=desk.com1y[sn];
    }
    else {
      x1[sn]=desk.com2x;
      y1[sn]=desk.com2y[sn];
    }
    if (sn==s || ouveang && s==spieler) {
      x1[sn]=desk.playx+(i%10)*desk.cardx;
      if (sn==s) y1[sn]=desk.playy;
      putdesk(sn,x1[sn],y1[sn]);
    }
    else {
      if (stich==10) putdesk(sn,x1[sn],y1[sn]);
    }
    x2[sn]=desk.stichx+vmh*64;
    y2[sn]=desk.stichy;
  }
  movecard(numsp,sna,x1,y1,x2,y2);
  for (sn=0;sn<numsp;sn++) {
    putcard(sn,stcd[vmh]=cards[i],desk.stichx+vmh*64,desk.stichy);
  }
  gespcd[cards[i]]=2;
  if ((cards[i]&7)!=BUBE) gespfb[cards[i]>>3]++;
  cards[i]=-1;
  stdwait();
}

Pixmap create_colcards(sn)
int sn;
{
  Pixmap pm,pmc;
  int i,j,pl,m0,m1,m2,m3,of,ix;

  pm=XCreatePixmap(dpy[sn],win[sn],colcards_width,colcards_height/4,
		   desk.plan[sn]);
  if (pm==None) return pm;
  XFillRectangle(dpy[sn],pm,gcbck[sn],
		 0,0,colcards_width,colcards_height/4);
  change_gcxor(sn,wpix[sn]);
  XCopyPlane(dpy[sn],bwcards[sn],pm,gcxor[sn],0,0,60,94,0,0,1);
  XCopyPlane(dpy[sn],bwcards[sn],pm,gcxor[sn],0,0,60,94,64,0,1);
  XCopyPlane(dpy[sn],bwcards[sn],pm,gcxor[sn],0,0,60,94,128,0,1);
  XCopyPlane(dpy[sn],bwcards[sn],pm,gcxor[sn],0,0,60,94,192,0,1);
  of=colcards_height/4*colcards_width/8;
  for (pl=0;pl<16;pl++) {
    m0=pl&1?0:0xff;
    m1=pl&2?0:0xff;
    m2=pl&4?0:0xff;
    m3=pl&8?0:0xff;
    for (i=0;i<colcards_height/4;i++) {
      for (j=0;j<colcards_width/8;j++) {
	ix=i*colcards_width/8+j;
	colcards_pixs[ix]=
	  (colcards_bits[ix]^m0)&
	  (colcards_bits[ix+of]^m1)&
	  (colcards_bits[ix+of+of]^m2)&
	  (colcards_bits[ix+of+of+of]^m3);
      }
    }
    pmc=XCreateBitmapFromData(dpy[sn],win[sn],colcards_pixs,
			      colcards_width,colcards_height/4);
    if (pmc==None) return pmc;
    change_gcxor(sn,color[pl].pixel^wpix[sn]^bgpix[sn]);
    XCopyPlane(dpy[sn],pmc,pm,gcxor[sn],
	       0,0,colcards_width,colcards_height/4,0,0,1);
    XFreePixmap(dpy[sn],pmc);
  }
  change_gcxor(sn,fgpix[sn]);
  return pm;
}

alloc_colors(sn)
int sn;
{
  int i;

  for (i=0;i<16;i++) {
    if (!XAllocColor(dpy[sn],cmap[sn],&color[i])) {
      if (i<=1) {
	desk.col[sn]=2;
      }
      color[i].pixel=i?bpix[sn]:wpix[sn];
    }
  }
  rpix[sn]=color[1].pixel;
}

unsigned long get_col(sn,ucol,prog,col,def,xcol)
int sn;
char *ucol,*prog,*col;
unsigned long def;
XColor *xcol;
{
  char *spec;

  if (ucol && XParseColor(dpy[sn],cmap[sn],ucol,xcol)) {
    if (XAllocColor(dpy[sn],cmap[sn],xcol)) return xcol->pixel;
  }
  spec=XGetDefault(dpy[sn],prog,col);
  if (spec && XParseColor(dpy[sn],cmap[sn],spec,xcol)) {
    if (XAllocColor(dpy[sn],cmap[sn],xcol)) return xcol->pixel;
  }
  xcol->pixel=def;
  XQueryColor(dpy[sn],cmap[sn],xcol);
  return def;
}

calc_desk()
{
  int sn;

  desk.x=0;
  desk.y=0;
  desk.w=640;
  desk.h=400;
  desk.com1x=desk.x+64;
  desk.com2x=desk.x+desk.w-128;
  for (sn=0;sn<numsp;sn++) {
    desk.com1y[sn]=desk.com2y[sn]=desk.y+3;
    if (ouveang && sn!=spieler) {
      if (spieler==left(sn)) {
	desk.com2y[sn]+=105;
      }
      else {
	desk.com1y[sn]+=105;
      }
    }
  }
  desk.cardx=(desk.w-64)/9;
  desk.playx=desk.x+(desk.w-64-9*desk.cardx)/2;
  desk.playy=desk.y+desk.h-96;
  desk.skatx=desk.x+(desk.w-2*64)/2;
  desk.skaty=desk.y+145;
  desk.stichx=desk.x+(desk.w-3*64)/2;
  desk.stichy=desk.skaty;
  desk.cbox1x=desk.com1x;
  desk.cbox2x=desk.com2x;
  desk.cboxy=desk.com1y[0]+105;
  desk.pboxx=desk.skatx;
  desk.pboxy=desk.skaty+107;
}

extractnam(sn,str)
int sn;
char *str;
{
  char *eos;
  int z,s;

  spnames[sn][0][0]=0;
  spnames[sn][1][0]=0;
  if (!str) str="";
  if (!(eos=strchr(str,'@')) && !(eos=strchr(str,':'))) eos=str+strlen(str);
  for (z=0;z<2 && str!=eos;z++) {
    while (*str==' ' || *str=='-') str++;
    for (s=0;s<9 && str!=eos && *str!=' ' && *str!='-';s++,str++) {
      spnames[sn][z][s]=*str;
    }
    spnames[sn][z][s]=0;
  }
}

usage()
{
  fprintf(stderr,"\
xskat [-display|-d display] [-geometry|-g geometry] [-font|-fn font]\n\
  [-iconic|-i] [-title|-T string] [-name prog] [-fg color] [-bg color]\n\
  [-bt color] [-help|-h] [-color] [-mono] [-up] [-down] [-alt] [-seq]\n\
  [-list|-l filename] [-alist] [-nlist] [-log filename] [-dolog] [-nolog]\n\
  [-game filename] [-lang language] [-start player#] [player@display...]\n\
After starting the game a mouse click will bring up a menu.\n\
");
}

invopt(opt)
char *opt;
{
  fprintf(stderr,"Invalid option %s\n",opt);
  usage();
  exit(1);
}

nomem()
{
  fprintf(stderr,"Out of memory\n");
  exit(1);
}

finish(sn,ex)
int sn,ex;
{
  int s;

  quit=1;
  for (s=0;s<numsp;s++) {
    di_term(s,sn);
  }
  for (s=numsp;s<3;s++) {
    lost[s]=1;
  }
  while (!lost[0] || !lost[1] || !lost[2]) {
    hndl_events();
  }
  if (ex) exit(1);
}

ioerr(d)
Display *d;
{
  int sn;

  for (sn=0;sn<numsp && dpy[sn]!=d;sn++);
  lost[sn]=1;
  finish(sn,1);
}

int getdeffn(prog_name,pfn,res,suf)
char *prog_name,**pfn,*res,*suf;
{
  char *home,*fn;
  int r;

  fn=XGetDefault(dpy[0],prog_name,res);
  r=1;
  if (!fn) {
    r=0;
    home=getenv("HOME");
    if (home && (fn=malloc(strlen(home)+3+strlen(suf)))) {
      strcpy(fn,home);
      strcat(fn,"/.");
      strcat(fn,suf);
    }
    else {
      fn=suf;
    }
  }
  *pfn=fn;
  return r;
}

xinit(argc,argv)
int argc;
char *argv[];
{
  char *prog_name;
  char *disp_name[3];
  char *font_name;
  char *title;
  char *fg_col;
  char *bg_col;
  char *bt_col;
  char *res;
  int bwcol;
  int downup;
  int altseq;
  int sn,x,y,w,h,f,logdef;
  XGCValues gcv;
  Pixmap icon;
  XSizeHints hints;
  XClassHint classhint;
  XWMHints wmhints;
  int scr,gcvf;
  XColor fgcol;
  unsigned long borw;

  signal(SIGPIPE,SIG_IGN);
  numsp=1;
  disp_name[0]=font_name=title=fg_col=bg_col=bt_col=0;
  bwcol=downup=altseq=lang=geber=logging=alist[0]=-1;
  prog_name=strrchr(argv[0],'/');
  if (prog_name) prog_name++;
  else prog_name=argv[0];
  wmhints.flags=0;
  hints.flags=0;
  while (argc>1) {
    if (!strcmp(argv[1],"-help") || !strcmp(argv[1],"-h")) {
      usage();
      exit(0);
    }
    else if (!strcmp(argv[1],"-color")) {
      bwcol=1;
    }
    else if (!strcmp(argv[1],"-mono")) {
      bwcol=0;
    }
    else if (!strcmp(argv[1],"-iconic") || !strcmp(argv[1],"-i")) {
      wmhints.flags|=StateHint;
      wmhints.initial_state=IconicState;
    }
    else if (!strcmp(argv[1],"-down")) {
      downup=1;
    }
    else if (!strcmp(argv[1],"-up")) {
      downup=0;
    }
    else if (!strcmp(argv[1],"-alt")) {
      altseq=1;
    }
    else if (!strcmp(argv[1],"-seq")) {
      altseq=0;
    }
    else if (!strcmp(argv[1],"-dolog")) {
      logging=1;
    }
    else if (!strcmp(argv[1],"-nolog")) {
      logging=0;
    }
    else if (!strcmp(argv[1],"-alist")) {
      alist[0]=1;
    }
    else if (!strcmp(argv[1],"-nlist")) {
      alist[0]=0;
    }
    else if ((argv[1][0]!='-') && numsp<3) {
      disp_name[numsp++]=argv[1];
    }
    else if (argc>2) {
      if (!strcmp(argv[1],"-display") || !strcmp(argv[1],"-d")) {
	disp_name[0]=argv[2];
      }
      else if (!strcmp(argv[1],"-geometry") || !strcmp(argv[1],"-g")) {
	f=XParseGeometry(argv[2],&x,&y,&w,&h);
      }
      else if (!strcmp(argv[1],"-name") || !strcmp(argv[1],"-n")) {
	prog_name=argv[2];
      }
      else if (!strcmp(argv[1],"-title") || !strcmp(argv[1],"-T")) {
	title=argv[2];
      }
      else if (!strcmp(argv[1],"-fg")) {
	fg_col=argv[2];
      }
      else if (!strcmp(argv[1],"-bg")) {
	bg_col=argv[2];
      }
      else if (!strcmp(argv[1],"-bt")) {
	bt_col=argv[2];
      }
      else if (!strcmp(argv[1],"-font") || !strcmp(argv[1],"-fn")) {
	font_name=argv[2];
      }
      else if (!strcmp(argv[1],"-list") || !strcmp(argv[1],"-l")) {
	list_file=argv[2];
      }
      else if (!strcmp(argv[1],"-log")) {
	prot_file=argv[2];
	logging=1;
      }
      else if (!strcmp(argv[1],"-game")) {
	game_file=argv[2];
      }
      else if (!strcmp(argv[1],"-lang")) {
	lang=langidx(argv[2],0);
      }
      else if (!strcmp(argv[1],"-start")) {
	geber=atoi(argv[2]);
	if (geber<1 || geber>3) geber=0;
	else geber=(geber+1)%3;
      }
      else {
	invopt(argv[1]);
      }
      argc--;argv++;
    }
    else {
      invopt(argv[1]);
    }
    argc--;argv++;
  }
  calc_desk();
#ifdef LOGDIR
  {
    FILE *f;
    char logf[1000];
    sprintf(logf,"%s/%lu",LOGDIR,(unsigned long)getuid());
    f=fopen(logf,"w");
    if (f) fclose(f);
  }
#endif
  for (sn=0;sn<numsp;sn++) {
    if (sn) {
      font_name=title=fg_col=bg_col=bt_col=0;
      bwcol=downup=altseq=alist[sn]=-1;
      extractnam(sn,disp_name[sn]);
      if (strchr(disp_name[sn],'@')) {
	disp_name[sn]=strchr(disp_name[sn],'@')+1;
      }
    }
    else {
      extractnam(sn,getenv("LOGNAME"));
    }
    if (!(dpy[sn]=XOpenDisplay(disp_name[sn]))) {
      fprintf(stderr,"Can't open display %s\n",XDisplayName(disp_name[sn]));
      exit(1);
    }
    XSetIOErrorHandler(ioerr);
    scr=DefaultScreen(dpy[sn]);
    cmap[sn]=DefaultColormap(dpy[sn],scr);
    desk.plan[sn]=DefaultDepth(dpy[sn],scr);
    bpix[sn]=BlackPixel(dpy[sn],scr);
    wpix[sn]=WhitePixel(dpy[sn],scr);
    bgpix[sn]=get_col(sn,bg_col,prog_name,"background",wpix[sn],&fgcol);
    fgpix[sn]=get_col(sn,fg_col,prog_name,"foreground",bpix[sn],&fgcol);
    borw=(long)fgcol.red+fgcol.green+fgcol.blue<0x1E000L?wpix[sn]:bpix[sn];
    btpix[sn]=get_col(sn,bt_col,prog_name,"button",borw,&fgcol);
    if (!title &&
	!(title=XGetDefault(dpy[sn],prog_name,"title"))) title=prog_name;
    if (!font_name &&
	!(font_name=XGetDefault(dpy[sn],prog_name,"font"))) {
      font_name="9x15";
    }
    if (!(dfont[sn]=XLoadQueryFont(dpy[sn],font_name))) {
      fprintf(stderr,"Font %s not found\n",font_name);
      exit(1);
    }
    charw[sn]=dfont[sn]->max_bounds.width;
    charh[sn]=dfont[sn]->max_bounds.ascent+dfont[sn]->max_bounds.descent+1;
    if (!sn && !list_file) {
      getdeffn(prog_name,&list_file,"list","xskat.lst");
    }
    if (list_file && !*list_file) list_file=0;
    if (!sn && !prot_file) {
      logdef=getdeffn(prog_name,&prot_file,"log","xskat.log");
    }
    if (prot_file && !*prot_file) prot_file=0;
    if (!sn && logging<0) {
      res=XGetDefault(dpy[sn],prog_name,"dolog");
      logging=res && istrue(res) || !res && logdef;
    }
    if (!sn && !game_file) {
      game_file=XGetDefault(dpy[sn],prog_name,"game");
    }
    if (game_file && !*game_file) game_file=0;
    if (bwcol<0) {
      res=XGetDefault(dpy[sn],prog_name,"color");
      if (res) bwcol=istrue(res);
    }
    desk.col[sn]=bwcol?1<<desk.plan[sn]:2;
    if (downup<0) {
      res=XGetDefault(dpy[sn],prog_name,"down");
      if (res) downup=istrue(res);
    }
    sort1[sn]=!downup;
    if (altseq<0) {
      res=XGetDefault(dpy[sn],prog_name,"alt");
      if (res) altseq=istrue(res);
    }
    alternate[sn]=!!altseq;
    if (alist[sn]<0) {
      res=XGetDefault(dpy[sn],prog_name,"alist");
      alist[sn]=res && istrue(res);
    }
    if (res=XGetDefault(dpy[sn],prog_name,"alias")) {
      extractnam(sn,res);
    }
    if (!sn && lang<0) {
      lang=langidx(XGetDefault(dpy[sn],prog_name,"language"),1);
    }
    if (!sn && geber<0) {
      if (res=XGetDefault(dpy[sn],prog_name,"start")) {
	geber=atoi(res);
      }
      if (geber<1 || geber>3) geber=0;
      else geber=(geber+1)%3;
    }
    if ((f&(XValue|YValue))==(XValue|YValue)) {
      hints.x=f&XNegative?XDisplayWidth(dpy[sn],scr)-x-desk.w:x;
      hints.y=f&YNegative?XDisplayHeight(dpy[sn],scr)-y-desk.h:y;
      hints.flags|=USPosition;
    }
    hints.flags|=PMinSize|PMaxSize;
    hints.min_width=hints.max_width=desk.w;
    hints.min_height=hints.max_height=desk.h;
    icon=XCreateBitmapFromData(dpy[sn],DefaultRootWindow(dpy[sn]),icon_bits,
			       icon_width,icon_height);
    if (icon==None) nomem();
    win[sn]=XCreateSimpleWindow(dpy[sn],DefaultRootWindow(dpy[sn]),
				desk.x,desk.y,desk.w,desk.h,
				0,fgpix[sn],bgpix[sn]);
    classhint.res_name=prog_name;
    classhint.res_class=prog_name;
    XSetClassHint(dpy[sn],win[sn],&classhint);
    XSetStandardProperties(dpy[sn],win[sn],title,title,icon,0,0,&hints);
    wmhints.flags|=IconPixmapHint;
    wmhints.icon_pixmap=icon;
    XSetWMHints(dpy[sn],win[sn],&wmhints);
    cursor[sn][0]=XCreateFontCursor(dpy[sn],XC_hand2);
    cursor[sn][1]=XCreateFontCursor(dpy[sn],XC_watch);
    XDefineCursor(dpy[sn],win[sn],cursor[sn][1]);
    gcvf=GCGraphicsExposures|GCForeground|GCBackground;
    gcv.graphics_exposures=False;
    gcv.font=dfont[sn]->fid;
    gcv.foreground=fgpix[sn];
    gcv.background=bgpix[sn];
    gc[sn]=XCreateGC(dpy[sn],win[sn],gcvf|GCFont,&gcv);
    gcv.foreground=bgpix[sn];
    gcv.background=fgpix[sn];
    gcbck[sn]=XCreateGC(dpy[sn],win[sn],gcvf,&gcv);
    gcv.foreground=fgpix[sn]^bgpix[sn];
    gcv.background=0;
    gcv.function=GXxor;
    gcxor[sn]=XCreateGC(dpy[sn],win[sn],gcvf|GCFunction,&gcv);
    bwcards[sn]=XCreateBitmapFromData(dpy[sn],win[sn],bwcards_bits,
				      bwcards_width,bwcards_height);
    if (bwcards[sn]==None) nomem();
    if (desk.col[sn]>2) {
      alloc_colors(sn);
      colcards[sn]=create_colcards(sn);
      if (colcards[sn]==None) nomem();
    }
    bck[sn]=XCreatePixmap(dpy[sn],win[sn],desk.w,desk.h,desk.plan[sn]);
    if (bck[sn]==None) nomem();
    XFillRectangle(dpy[sn],win[sn],gcbck[sn],0,0,desk.w,desk.h);
    XFillRectangle(dpy[sn],bck[sn],gcbck[sn],0,0,desk.w,desk.h);
    XSelectInput(dpy[sn],win[sn],ButtonPressMask|ExposureMask|KeyPressMask);
    XMapWindow(dpy[sn],win[sn]);
  }
  init_text();
  init_dials();
  for (sn=0;sn<numsp;sn++) {
    if (!spnames[sn][0][0]) {
      sprintf(spnames[sn][0],textarr[TX_SPIELER],sn+1);
    }
  }
  if (numsp==2) {
    strcpy(spnames[2][0],textarr[TX_COMPUTER]);
  }
  else if (numsp==1) {
    strcpy(spnames[1][0],textarr[TX_COMPUTER]);
    strcpy(spnames[2][0],spnames[1][0]);
    strcpy(spnames[1][1],textarr[TX_LINKS]);
    strcpy(spnames[2][1],textarr[TX_RECHTS]);
  }
}

waitt(t,f)
int t,f;
{
  struct timeval timeout;
  int sn;

  for (sn=0;sn<numsp;sn++) {
    if (!lost[sn]) {
      if (!f) {
	XFlush(dpy[sn]);
      }
      else {
	XSync(dpy[sn],0);
      }
    }
  }
  timeout.tv_sec=t/1000;
  timeout.tv_usec=(t%1000)*1000L;
  select(0,0,0,0,&timeout);
  if (f>1) refresh();
}

stdwait()
{
  waitt(700,2);
}

backgr(sn,x,y,w,h)
int sn,x,y,w,h;
{
  XFillRectangle(dpy[sn],bck[sn],gcbck[sn],x,y,w,h);
  XFillRectangle(dpy[sn],win[sn],gcbck[sn],x,y,w,h);
}

putdesk(sn,x,y)
int sn,x,y;
{
  backgr(sn,x,y,64,94);
}

drawcard(sn,i,x,y)
int sn,i,x,y;
{
  int p,x1,y1,x2,y2,x3,y3,f,w,dxy,dx,dy;

  x+=2;
  f=i>>3;
  w=i&7;
  if (desk.col[sn]>2 && ((KOENIG<=w && w<=BUBE) || i<0)) {
    x1=i>=0?(w-KOENIG+1)*64:0;
    XCopyArea(dpy[sn],colcards[sn],bck[sn],gc[sn],x1,0,60,94,x,y);
  }
  else {
    x1=i>=0?KOENIG<=w && w<=BUBE?(w-KOENIG+3)*64:128:64;
    XFillRectangle(dpy[sn],bck[sn],gcbck[sn],x,y,60,94);
    change_gcxor(sn,wpix[sn]);
    XCopyPlane(dpy[sn],bwcards[sn],bck[sn],gcxor[sn],0,0,60,94,x,y,1);
    change_gcxor(sn,bpix[sn]^wpix[sn]^bgpix[sn]);
    XCopyPlane(dpy[sn],bwcards[sn],bck[sn],gcxor[sn],x1,0,60,94,x,y,1);
  }
  if (i>=0) {
    change_gcxor(sn,(desk.col[sn]>2 && f<2?rpix[sn]:bpix[sn])^
		 wpix[sn]^bgpix[sn]);
    p=cnts[w];
    if (w==BUBE && f==1) dxy=dx=-1;
    else dxy=dx=0;
    do {
      x1=f*16;
      y1=96;
      if (bigs[p+1]>39) x1+=64;
      x2=x1+15;
      y2=y1+15;
      dxy=-dxy;
      x3=bigs[p++]-2+dxy+dx;
      y3=bigs[p++]+dxy;
      dx=0;
      if (desk.col[sn]<3 || f>1) {
	XCopyPlane(dpy[sn],bwcards[sn],bck[sn],gcxor[sn],
		   x1,y1,x2-x1+1,y2-y1+1,x+x3,y+y3,1);
      }
      else {
	XCopyPlane(dpy[sn],bwcards[sn],bck[sn],gcxor[sn],
		   x1+128,y1,x2-x1+1,y2-y1+1,x+x3,y+y3,1);
      }
    } while (p!=cnts[w+1]);
    for (p=0;p<8;p+=2) {
      x1=f*8;
      y1=112;
      if (smls[p+1]>11) x1+=32;
      x2=x1+7;
      y2=y1+7;
      if (f>0 && f<3 && p<3) dy=-1;
      else dy=0;
      x3=smls[p]-2;
      y3=smls[p+1]+dy;
      if (desk.col[sn]<3 || f>1) {
	XCopyPlane(dpy[sn],bwcards[sn],bck[sn],gcxor[sn],
		   x1,y1,x2-x1+1,y2-y1+1,x+x3,y+y3,1);
      }
      else {
	XCopyPlane(dpy[sn],bwcards[sn],bck[sn],gcxor[sn],
		   x1+128,y1,x2-x1+1,y2-y1+1,x+x3,y+y3,1);
      }
      x1=256+(w==AS?lang*20:w==ZEHN?p>3?23:0:w<=BUBE?(w-KOENIG+1)*5+lang*20:
	      (p>3?31:8)+(w-NEUN)*5);
      y1=95+(w!=ZEHN && w<=BUBE?p>3?7:0:14);
      x2=x1+(w==ZEHN?7:4);
      y2=y1+6;
      x3=smlc[p]-(w==ZEHN?p&2?2:1:0);
      y3=smlc[p+1];
      XCopyPlane(dpy[sn],bwcards[sn],bck[sn],gcxor[sn],
		 x1,y1,x2-x1+1,y2-y1+1,x+x3,y+y3,1);
    }
  }
  change_gcxor(sn,fgpix[sn]);
  XCopyArea(dpy[sn],bck[sn],win[sn],gc[sn],x,y,60,94,x,y);
}

putcard(sn,i,x,y)
int sn,i,x,y;
{
  if (i<0) putdesk(sn,x,y);
  else drawcard(sn,i,x,y);
}

putback(sn,x,y)
int sn,x,y;
{
  drawcard(sn,-1,x,y);
}

putamark(sn,s)
int sn,s;
{
  int a=44,b=46,xp1,xp2,yp,xyarr[4];

  if (sn==s || ouveang) return;
  change_gc(sn,bpix[sn],gc);
  xp1=desk.com1x+32;
  xp2=desk.com2x+32;
  yp=desk.com1y[0]+2;
  xyarr[0]=(s==left(sn)?xp1-a/2:xp2-a/2);
  xyarr[1]=yp;
  xyarr[2]=xyarr[0]+a-1;
  xyarr[3]=xyarr[1];
  XDrawLine(dpy[sn],win[sn],gc[sn],xyarr[0],xyarr[1],xyarr[2],xyarr[3]);
  XDrawLine(dpy[sn],bck[sn],gc[sn],xyarr[0],xyarr[1],xyarr[2],xyarr[3]);
  xyarr[0]=(s==left(sn)?xp1-b/2:xp2-b/2);
  xyarr[1]=yp+1;
  xyarr[2]=xyarr[0]+b-1;
  xyarr[3]=xyarr[1];
  XDrawLine(dpy[sn],win[sn],gc[sn],xyarr[0],xyarr[1],xyarr[2],xyarr[3]);
  XDrawLine(dpy[sn],bck[sn],gc[sn],xyarr[0],xyarr[1],xyarr[2],xyarr[3]);
  xyarr[0]=(s==left(sn)?xp1-b/2:xp2-b/2);
  xyarr[1]=yp+88;
  xyarr[2]=xyarr[0]+b-1;
  xyarr[3]=xyarr[1];
  XDrawLine(dpy[sn],win[sn],gc[sn],xyarr[0],xyarr[1],xyarr[2],xyarr[3]);
  XDrawLine(dpy[sn],bck[sn],gc[sn],xyarr[0],xyarr[1],xyarr[2],xyarr[3]);
  xyarr[0]=(s==left(sn)?xp1-a/2:xp2-a/2);
  xyarr[1]=yp+89;
  xyarr[2]=xyarr[0]+a-1;
  xyarr[3]=xyarr[1];
  XDrawLine(dpy[sn],win[sn],gc[sn],xyarr[0],xyarr[1],xyarr[2],xyarr[3]);
  XDrawLine(dpy[sn],bck[sn],gc[sn],xyarr[0],xyarr[1],xyarr[2],xyarr[3]);
  change_gc(sn,fgpix[sn],gc);
}

putmark(s)
int s;
{
  int sn;

  for (sn=0;sn<numsp;sn++) {
    putamark(sn,s);
  }
}

remmark()
{
  int sn;

  for (sn=0;sn<numsp;sn++) {
    putback(sn,desk.com1x,desk.com1y[0]);
    putback(sn,desk.com2x,desk.com1y[0]);
    putamark(sn,spieler);
  }
}

movecard(nn,sn,x1,y1,x2,y2)
int nn,sn[],x1[],y1[],x2[],y2[];
{
  int dx[3],dy[3],i,j,n=8;

  for (i=0;i<nn;i++) {
    dx[i]=x2[i]-x1[i];
    dy[i]=y2[i]-y1[i];
  }
  for (i=0;i<n;i++) {
    for (j=0;j<nn;j++) {
      XDrawRectangle(dpy[sn[j]],win[sn[j]],gcxor[sn[j]],
		     x1[j]+i*dx[j]/n,y1[j]+i*dy[j]/n,64,96);
    }
    waitt(9,0);
    for (j=0;j<nn;j++) {
      XDrawRectangle(dpy[sn[j]],win[sn[j]],gcxor[sn[j]],
		     x1[j]+i*dx[j]/n,y1[j]+i*dy[j]/n,64,96);
    }
  }
  refresh();
}

homecard(s,x,y)
int s,x,y;
{
  int sn,sna[3],x1[3],y1[3],x2[3],y2[3];

  for (sn=0;sn<numsp;sn++) {
    sna[sn]=sn;
    x2[sn]=desk.x+desk.w;
    if (s!=sn) {
      y2[sn]=desk.com1y[0];
      if (s==left(sn)) x2[sn]=desk.x-64;
    }
    else y2[sn]=desk.playy;
    putdesk(sn,x,y);
    x1[sn]=x;
    y1[sn]=y;
  }
  movecard(numsp,sna,x1,y1,x2,y2);
}

givecard(s,n)
int s,n;
{
  int sn,sna[3],x1[3],y1[3],x2[3],y2[3];

  for (sn=0;sn<numsp;sn++) {
    sna[sn]=sn;
    if (s<0) {
      x1[sn]=desk.skatx;
      y1[sn]=desk.skaty;
    }
    else if (s!=sn) {
      if (s==left(sn)) x1[sn]=desk.com1x;
      else x1[sn]=desk.com2x;
      y1[sn]=desk.com1y[0];
    }
    else {
      if (!n) x1[sn]=desk.playx;
      else if (n==1) x1[sn]=desk.playx+3*desk.cardx;
      else x1[sn]=desk.playx+7*desk.cardx;
      y1[sn]=desk.playy;
    }
    x2[sn]=desk.x+desk.w;
    if (geber!=sn) {
      y2[sn]=desk.com1y[0];
      if (geber==left(sn)) x2[sn]=desk.x-64;
    }
    else y2[sn]=desk.playy;
  }
  movecard(numsp,sna,x2,y2,x1,y1);
  for (sn=0;sn<numsp;sn++) {
    putback(sn,x1[sn],y1[sn]);
    if (s==hoerer) putamark(sn,s);
    if (s==sn) {
      putback(sn,x1[sn]+desk.cardx,y1[sn]);
      putback(sn,x1[sn]+2*desk.cardx,y1[sn]);
      if (n==1) putback(sn,x1[sn]+3*desk.cardx,y1[sn]);
    }
    else if (s<0) {
      putback(sn,x1[sn]+64,y1[sn]);
    }
  }
  waitt(300,2);
}

initscr(sn)
int sn;
{
  int i,x,y,w;

  di_info(sn,-1);
  if (predef) {
    x=(desk.w-strlen(textarr[TX_VORDEFINIERTES_SPIEL])*charw[sn])/2;
    y=desk.y+2*charh[sn];
    w=strlen(textarr[TX_VORDEFINIERTES_SPIEL])*charw[sn];
    v_gtext(sn,x,y,w,textarr[TX_VORDEFINIERTES_SPIEL]);
  }
  sort(sn);
  for (i=0;i<10;i++) {
    putcard(sn,cards[sn*10+i],desk.playx+i*desk.cardx,desk.playy);
  }
  if (ouveang) {
    if (sn==spieler) {
      for (sn=0;sn<numsp;sn++) {
	if (sn!=spieler) initscr(sn);
      }
    }
    else {
      y=spieler==left(sn)?desk.com1y[sn]:desk.com2y[sn];
      for (i=0;i<10;i++) {
	putcard(sn,cards[spieler*10+i],desk.playx+i*desk.cardx,y);
      }
      x=spieler==left(sn)?desk.com2x:desk.com1x;
      y=spieler==left(sn)?desk.com2y[sn]:desk.com1y[sn];
      putback(sn,x,y);
    }
  }
}

clr_desk()
{
  int sn;

  for (sn=0;sn<numsp;sn++) {
    backgr(sn,desk.x,desk.y,desk.w,desk.h);
    di_info(sn,3);
  }
  if (ouveang) {
    for (sn=0;sn<numsp;sn++) {
      if (sn!=spieler) di_info(sn,-2);
    }
    ouveang=0;
    calc_desk();
    for (sn=0;sn<numsp;sn++) {
      if (sn!=spieler) di_info(sn,3);
    }
    ouveang=1;
  }
}

draw_box(sn,x,y,w)
int sn,x,y,w;
{
  int xy[4];

  xy[0]=x+2;xy[1]=y-1;
  xy[2]=x+w-3;xy[3]=y+18;
  XDrawRectangle(dpy[sn],win[sn],gc[sn],xy[0],xy[1],xy[2]-xy[0],xy[3]-xy[1]);
  XDrawRectangle(dpy[sn],bck[sn],gc[sn],xy[0],xy[1],xy[2]-xy[0],xy[3]-xy[1]);
  xy[0]++;xy[1]++;
  xy[2]--;xy[3]--;
  XDrawRectangle(dpy[sn],win[sn],gc[sn],xy[0],xy[1],xy[2]-xy[0],xy[3]-xy[1]);
  XDrawRectangle(dpy[sn],bck[sn],gc[sn],xy[0],xy[1],xy[2]-xy[0],xy[3]-xy[1]);
  xy[0]++;xy[1]++;
  change_gc(sn,btpix[sn],gc);
  XFillRectangle(dpy[sn],win[sn],gc[sn],xy[0],xy[1],xy[2]-xy[0],xy[3]-xy[1]);
  XFillRectangle(dpy[sn],bck[sn],gc[sn],xy[0],xy[1],xy[2]-xy[0],xy[3]-xy[1]);
  change_gc(sn,fgpix[sn],gc);
}

put_box(s)
int s;
{
  int sn;

  for (sn=0;sn<numsp;sn++) {
    if (s!=sn) {
      if (s==left(sn)) draw_box(sn,desk.cbox1x,desk.cboxy,64);
      else draw_box(sn,desk.cbox2x,desk.cboxy,64);
    }
    else {
      draw_box(sn,desk.pboxx,desk.pboxy,64);
      draw_box(sn,desk.pboxx+64,desk.pboxy,64);
    }
  }
}

rem_box(s)
int s;
{
  int sn;

  for (sn=0;sn<numsp;sn++) {
    if (s!=sn) {
      if (s==left(sn)) backgr(sn,desk.cbox1x,desk.cboxy-5,66,28);
      else backgr(sn,desk.cbox2x,desk.cboxy-5,64,28);
    }
    else {
      backgr(sn,desk.pboxx,desk.pboxy-5,64,28);
      backgr(sn,desk.pboxx+64,desk.pboxy-5,64,28);
    }
  }
}

inv_box(s,c)
int s,c;
{
  int sn,x,y;

  for (sn=0;sn<numsp;sn++) {
    change_gcxor(sn,btpix[sn]^fgpix[sn]^bgpix[sn]);
    x=(s!=sn?s==left(sn)?desk.cbox1x:desk.cbox2x:desk.pboxx+(c?64:0))+4;
    y=(s!=sn?desk.cboxy:desk.pboxy)+1;
    XFillRectangle(dpy[sn],win[sn],gcxor[sn],x,y,56,16);
    XFillRectangle(dpy[sn],bck[sn],gcxor[sn],x,y,56,16);
    change_gcxor(sn,fgpix[sn]);
  }
}

put_fbox()
{
  draw_box(spieler,desk.pboxx+24,desk.pboxy,80);
  v_gtextc(spieler,1,desk.pboxx+24,desk.pboxy,80,textarr[TX_DRUECKEN]);
}

rem_fbox()
{
  backgr(spieler,desk.pboxx+24,desk.pboxy-5,80,28);
}

inv_fbox()
{
  change_gcxor(spieler,btpix[spieler]^fgpix[spieler]^bgpix[spieler]);
  XFillRectangle(dpy[spieler],win[spieler],gcxor[spieler],
		 desk.pboxx+28,desk.pboxy+1,72,16);
  XFillRectangle(dpy[spieler],bck[spieler],gcxor[spieler],
		 desk.pboxx+28,desk.pboxy+1,72,16);
  change_gcxor(spieler,fgpix[spieler]);
}

int card_at(sn,x,y)
int sn,x,y;
{
  int c,i;

  if (y>=desk.playy) {
    x-=desk.playx+2;
    if (x<0 || x%desk.cardx>59 || x>9*desk.cardx+59) return 0;
    c=x/desk.cardx;
    if (c>9) c=9;
    if (cards[10*sn+c]>=0) return c+1;
    for (i=0;i<2;i++) {
      if (c--==0) return 0;
      if (c*desk.cardx<=x && x<c*desk.cardx+60 && cards[10*sn+c]>=0) {
	return c+1;
      }
    }
  }
  return 0;
}

int hndl_reizen(sn,x,y)
int sn,x,y;
{
  int b;

  if (x<=desk.pboxx+56) b=0;
  else b=64;
  if (x>=desk.pboxx+7+b && x<=desk.pboxx+56+b &&
      y>=desk.pboxy+1 && y<=desk.pboxy+16) {
    di_delres(sn);
    if (b) maxrw[sn]=0;
    else maxrw[sn]=215;
    do_entsch();
    return 1;
  }
  return 0;
}

int hndl_druecken(sn,x,y)
int sn,x,y;
{
  int c,sna[1],x1[1],y1[1],x2[1],y2[1];

  c=card_at(sn,x,y);
  if (c) {
    c--;
    swap(&cards[10*sn+c],&cards[drkcd+30]);
    sna[0]=sn;
    x1[0]=desk.playx+c*desk.cardx;
    y1[0]=desk.playy;
    x2[0]=desk.skatx+drkcd*64;
    y2[0]=desk.skaty;
    movecard(1,sna,x1,y1,x2,y2);
    putcard(sn,cards[drkcd+30],x2[0],y2[0]);
    initscr(sn);
    drkcd=1-drkcd;
    return 1;
  }
  if (x>=desk.pboxx+31 && x<=desk.pboxx+96 &&
      y>=desk.pboxy+1 && y<=desk.pboxy+16) inv_fbox();
  else return 0;
  stdwait();
  inv_fbox();
  rem_fbox();
  home_skat();
  if (lower(cards[31],cards[30],0)) {
    swap(&cards[31],&cards[30]);
  }
  prot2.skat[1][0]=cards[30];
  prot2.skat[1][1]=cards[31];
  info_stich(0,cards[30],1);
  info_stich(1,cards[31],1);
  for (c=0;c<2;c++) {
    stsum+=cardw[cards[c+30]&7];
    gespcd[cards[c+30]]=1;
    cards[c+30]=-1;
  }
  gedr=2;
  do_ansagen();
  return 1;
}

int hndl_spielen(sn,x,y)
int sn,x,y;
{
  int i,c;

  c=card_at(sn,x,y);
  if (c) {
    c--;
    calc_poss(sn);
    for (i=0;i<possc;i++) {
      if (10*sn+c==possi[i]) {
        drop_card(10*sn+c,sn);
        do_next();
        break;
      }
    }
    return 1;
  }
  return 0;
}

hndl_button(sn,x,y)
int sn,x,y;
{
  int ok=0;

  if (phase==REIZEN) {
    if (saho && sn==sager || !saho && sn==hoerer) ok=hndl_reizen(sn,x,y);
  }
  else if (phase==DRUECKEN) {
    if (sn==spieler) ok=hndl_druecken(sn,x,y);
  }
  else if (phase==SPIELEN) {
    if (sn==(ausspl+vmh)%3) ok=hndl_spielen(sn,x,y);
  }
  if (!ok) di_options(sn);
}

setcurs(f)
int f;
{
  int x,y,w,sn,newsn=-1;
  static int actsn=-1,wsn=-1,wtime;

  for (sn=0;sn<numsp;sn++) {
    if (phase==REIZEN) {
      if (saho && sn==sager || !saho && sn==hoerer) newsn=sn;
    }
    else if (phase==HANDSPIEL || phase==DRUECKEN ||
	     phase==ANSAGEN || phase==RESULT) {
      if (sn==spieler) newsn=sn;
      if (numsp==1) newsn=0;
    }
    else if (phase==SPIELEN) {
      if (sn==(ausspl+vmh)%3) newsn=sn;
    }
  }
  if (f>=0) newsn=f;
  if (newsn!=actsn) {
    wtime=0;
    if (actsn>=0) XDefineCursor(dpy[actsn],win[actsn],cursor[actsn][1]);
    actsn=newsn;
    if (actsn>=0) {
      for (sn=0;sn<numsp;sn++) {
	di_info(sn,actsn);
      }
      XDefineCursor(dpy[actsn],win[actsn],cursor[actsn][0]);
    }
  }
  if (numsp==1) return;
  if (actsn==-1) wtime=0;
  if (!wtime || wtime>=15*1000) {
    if (wsn!=-1 && (actsn!=wsn ||
		    wtime==16*1000 ||
		    phase!=SPIELEN)) {
      x=(desk.w-30*charw[wsn])/2;
      y=desk.playy-2*charh[wsn];
      w=30*charw[wsn];
      v_gtext(wsn,x,y,w,"                              ");
      wsn=-1;
      if (wtime==16*1000) wtime-=1500;
      if (phase!=SPIELEN) wtime=0;
    }
    if (wtime==15*1000 && phase==SPIELEN) {
      wsn=actsn;
      x=(desk.w-strlen(textarr[TX_DU_BIST_DRAN])*charw[wsn])/2;
      y=desk.playy-2*charh[wsn];
      w=strlen(textarr[TX_DU_BIST_DRAN])*charw[wsn];
      v_gtext(wsn,x,y,w,textarr[TX_DU_BIST_DRAN]);
    }
  }
  wtime+=50;
}
