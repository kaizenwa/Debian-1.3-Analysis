
/*
    xskat - a card game for 1 to 3 players.
    Copyright (C) 1996  Gunter Gerhardt

    This program is free software; you can redistribute it freely.
    Use it at your own risk; there is NO WARRANTY.
*/

#define XDIAL_C

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <X11/Xlib.h>
#include <X11/X.h>
#include "skat.h"
#include "xio.h"
#include "text.h"
#include "xdial.h"

#define INIT_DI(d)\
{\
  memcpy(d[1],d[0],sizeof(d[0]));\
  memcpy(d[2],d[0],sizeof(d[0]));\
  init_di(d[0]);\
  init_di(d[1]);\
  init_di(d[2]);\
}

info_reiz()
{
  int sn;
  static char txt[3][40];

  for (sn=0;sn<numsp;sn++) {
    dioptions[sn][10].str=txt[sn];
    sprintf(txt[sn],"%s %d",textarr[TX_GEREIZT_BIS_L],reizw[reizp]);
  }
}

info_spiel()
{
  int sn;
  static char txt[3][40];

  for (sn=0;sn<numsp;sn++) {
    dioptions[sn][11].str=txt[sn];
    sprintf(txt[sn],"%s %s",textarr[TX_GESPIELT_WIRD],
	    textarr[TX_NULL+trumpf+1]);
  }
}

info_stich(p,c,f)
int p,c,f;
{
  int sn;
  static char txt[3][3][20];

  for (sn=0;sn<numsp;sn++) {
    if (!f || sn==spieler) {
      dioptions[sn][14+p].str=txt[sn][p];
      txt[sn][p][0]=0;
      strcat(txt[sn][p],textarr[TX_KARO+(c>>3)]);
      strcat(txt[sn][p]," ");
      strcat(txt[sn][p],textarr[TX_AS+(c&7)]);
      dioptions[sn][12].spec=f?OB_HIDDEN:OB_NONE;
      dioptions[sn][13].spec=f?OB_NONE:OB_HIDDEN;
    }
  }
}

clear_info()
{
  int sn;

  for (sn=0;sn<numsp;sn++) {
    dioptions[sn][10].str=textarr[TX_GEREIZT_BIS_L];
    dioptions[sn][11].str=textarr[TX_GESPIELT_WIRD];
    dioptions[sn][12].spec=OB_NONE;
    dioptions[sn][13].spec=OB_HIDDEN;
    dioptions[sn][14].str=OB_NONE;
    dioptions[sn][15].str=OB_NONE;
    dioptions[sn][16].str=OB_NONE;
  }
}

set_names(ob,idx)
OBJECT *ob;
int idx;
{
  int z,s;

  for (z=0;z<2;z++) {
    for (s=0;s<3;s++) {
      ob[idx+z*3+s].str=spnames[s][z];
    }
  }
}

int ismemb(w,ob)
Window w;
OBJECT *ob;
{
  int i;

  for (i=0;i<ob[0].spec;i++) {
    if (w==ob[i].win) return i?i:-1;
  }
  return 0;
}

init_di(ob)
OBJECT *ob;
{
  int i;

  for (i=1;i<ob[0].spec;i++) {
    if (ob[i].str!=OB_NONE) {
      ob[i].str=textarr[(int)ob[i].str-OB_NONE-1];
    }
  }
}

init_dials()
{
  INIT_DI(diende);
  INIT_DI(diterm);
  INIT_DI(dismlres);
  INIT_DI(diproto);
  INIT_DI(diliste);
  INIT_DI(dioptions);
  INIT_DI(dicopyr);
  init_di(dihand);
  init_di(diloesch);
  init_di(dispiel);
  init_di(difehler);
  init_di(diueberr);
  init_di(diansage);
  init_di(diresult);
}

hndl_btevent(sn,event)
int sn;
XEvent *event;
{
  int bt,i,ag,s1,s2,al;
  OBJECT *ob;

  ob=actdial[sn];
  for (bt=1;bt<ob[0].spec;bt++) {
    if (event->xbutton.window==ob[bt].win &&
	ob[bt].spec&(OB_BUTTON|OB_EXIT)) {
      if ((i=bt+ob[bt].next)!=bt) {
	while (i!=bt) {
	  if (ob[i].spec&OB_SELECTED) {
	    ob[i].spec&=~OB_SELECTED;
	    draw_di(sn,i);
	  }
	  i+=ob[i].next;
	}
	if (!(ob[bt].spec&OB_SELECTED)) {
	  ob[bt].spec|=OB_SELECTED;
	  draw_di(sn,bt);
	}
      }
      else {
	ob[bt].spec^=OB_SELECTED;
	draw_di(sn,bt);
      }
      if (ob[bt].spec&OB_EXIT) {
	waitt(100,1);
	if (ob==dihand) {
	  remove_di(sn);
	  if (bt==2) handsp=1;
	  do_handok();
	}
	else if (ob==diende[sn]) {
	  remove_di(sn);
	  if (bt==2) {
	    lost[sn]=1;
	    XUnmapWindow(dpy[sn],win[sn]);
	    XFlush(dpy[sn]);
	    finish(sn,0);
	  }
	  else if (phase==RESULT) phase=GEBEN;
	}
	else if (ob==diterm[sn]) {
	  remove_di(sn);
	  lost[sn]=1;
	  XUnmapWindow(dpy[sn],win[sn]);
	  XFlush(dpy[sn]);
	}
	else if (ob==diloesch) {
	  remove_di(sn);
	  if (bt==2) {
	    splfirst[0]=splfirst[1]=splfirst[2]=splstp=0;
	    splsum[0]=splsum[1]=splsum[2]=0;
	    sum[0]=sum[1]=sum[2]=0;
	    asplsum[0]=asplsum[1]=asplsum[2]=0;
	    asum[0]=asum[1]=asum[2]=0;
	    if (list_file) unlink(list_file);
	  }
	  di_liste(sn,1);
	}
	else if (ob==dispiel) {
	  remove_di(sn);
	  ag=0;
	  if (dispiel[6].spec&OB_SELECTED) trumpf=-1;
	  else if (dispiel[2].spec&OB_SELECTED) trumpf=0;
	  else if (dispiel[3].spec&OB_SELECTED) trumpf=1;
	  else if (dispiel[4].spec&OB_SELECTED) trumpf=2;
	  else if (dispiel[5].spec&OB_SELECTED) trumpf=3;
	  else if (dispiel[7].spec&OB_SELECTED) trumpf=4;
	  else {
	    create_di(sn,dispiel);
	    ag=1;
	  }
	  if (!ag && !handsp && trumpf!=-1 &&
	      (dispiel[8].spec&OB_SELECTED
	       || dispiel[9].spec&OB_SELECTED
	       || dispiel[10].spec&OB_SELECTED)) {
	    create_di(sn,difehler);
	    ag=1;
	  }
	  if (!ag && trumpf==-1 &&
	      reizw[reizp]>nullw[handsp+(dispiel[10].spec&OB_SELECTED?2:0)]) {
	    create_di(sn,diueberr);
	    ag=1;
	  }
	  if (!ag) {
	    if (dispiel[10].spec&OB_SELECTED) ouveang=schwang=schnang=1;
	    else if (dispiel[9].spec&OB_SELECTED) schwang=schnang=1;
	    else if (dispiel[8].spec&OB_SELECTED) schnang=1;
	    if (trumpf==-1) schwang=schnang=0;
	    di_ansage();
	  }
	}
	else if (ob==difehler) {
	  remove_di(sn);
	  di_spiel();
	}
	else if (ob==diueberr) {
	  remove_di(sn);
	  di_spiel();
	}
	else if (ob==diansage) {
	  remove_di(sn);
	  do_angesagt();
	}
	else if (ob==diresult) {
	  remove_di(sn);
	  if (handsp && sn==spieler) {
	    putdesk(sn,desk.playx+8*desk.cardx,desk.playy);
	    putdesk(sn,desk.playx+9*desk.cardx,desk.playy);
	  }
	  if (bt==15) di_ende(sn);
	  else if (bt==16) di_proto(sn,1,0);
	  else phase=GEBEN;
	}
	else if (ob==diproto[sn]) {
	  remove_di(sn);
	  if (bt==48) {
	    protsort[sn]^=1;
	    di_proto(sn,0,0);
	  }
	  else if (bt==46) {
	    if (sn || !protsort[sn]) di_liste(sn,1);
	    else {
	      di_proto(0,1,1);
	      if (phase==RESULT) phase=GEBEN;
	    }
	  }
	  else if (phase==RESULT) phase=GEBEN;
	}
	else if (ob==diliste[sn]) {
	  remove_di(sn);
	  if (bt==21) {
	    if (splfirst[sn]>=12) {
	      splfirst[sn]-=12;
	      di_liste(sn,0);
	    }
	    else {
	      alist[sn]^=1;
	      di_liste(sn,0);
	    }
	  }
	  else if (bt==23) {
	    if (!sn && splfirst[sn]+12>=splstp) {
	      di_loesch(sn);
	    }
	    else {
	      if (splfirst[sn]+12<splstp) {
		splfirst[sn]+=12;
		di_liste(sn,0);
	      }
	      else {
		di_liste(sn,1);
	      }
	    }
	  }
	  else if (phase==RESULT) phase=GEBEN;
	}
	else if (ob==dioptions[sn]) {
	  remove_di(sn);
	  s1=dioptions[sn][4].spec&OB_SELECTED?1:0;
	  al=dioptions[sn][6].spec&OB_SELECTED?1:0;
	  s2=dioptions[sn][8].spec&OB_SELECTED?1:0;
	  if (s1!=sort1[sn] || s2!=sort2[sn] || al!=alternate[sn]) {
	    sort1[sn]=s1;
	    sort2[sn]=s2;
	    alternate[sn]=al;
	    initscr(sn);
	  }
	  if (bt==19) di_ende(sn);
	  else if (bt==17) di_liste(sn,1);
	  else if (bt==18) di_proto(sn,1,0);
	  else if (bt==2) di_copyr(sn);
	}
	else if (ob==dicopyr[sn]) {
	  remove_di(sn);
	  di_options(sn);
	}
      }
    }
  }
}

hndl_events()
{
  int sn,b,x,y,i;
  XEvent event;
  char buf[2];

  if (!lost[0] && !lost[1] && !lost[2]) setcurs(-1);
  waitt(50,1);
  for (sn=0;sn<numsp;sn++) {
    b=x=y=0;
    while (!lost[sn] && XCheckMaskEvent(dpy[sn],~0,&event)) {
      switch (event.type) {
      case KeyPress:
	if (!actdial[sn] && XLookupString(&event,buf,1,0,0)) {
	  b=sn+1;
	  x=y=-1;
	}
	break;
      case ButtonPress:
	if (actdial[sn]) {
	  hndl_btevent(sn,&event);
	}
	else {
	  b=sn+1;
	  x=event.xbutton.x;
	  y=event.xbutton.y;
	}
	break;
      case Expose:
	if (event.xexpose.window==win[sn]) {
	  XCopyArea(dpy[sn],bck[sn],win[sn],gc[sn],
		    event.xexpose.x,event.xexpose.y,
		    event.xexpose.width,event.xexpose.height,
		    event.xexpose.x,event.xexpose.y);
	}
	else if (actdial[sn] &&
		 (i=ismemb(event.xexpose.window,actdial[sn]))) {
	  if (!event.xexpose.count) {
	    draw_di(sn,i<0?0:i);
	  }
	}
	else if (resdial[sn] &&
		 (i=ismemb(event.xexpose.window,resdial[sn]))) {
	  if (!event.xexpose.count) {
	    draw_dial(sn,i<0?0:i,resdial[sn]);
	  }
	}
	break;
      }
    }
    if (b) hndl_button(b-1,x,y);
  }
}

create_dial(sn,x,y,dy,ob)
int sn;
int x,y,dy;
OBJECT *ob;
{
  Window rt,wi;
  int i,bd;

  rt=XCreateSimpleWindow(dpy[sn],win[sn],x-1,y-dy-1,
			 ob[0].w*charw[sn],dy+ob[0].h*charh[sn],
			 1,fgpix[sn],bgpix[sn]);
  ob[0].y=dy;
  ob[0].win=rt;
  XSelectInput(dpy[sn],rt,ExposureMask);
  for (i=1;i<ob[0].spec;i++) {
    if (ob[i].spec&OB_HIDDEN) {
      ob[i].win=None;
      continue;
    }
    ob[i].spec&=~OB_SELECTED;
    bd=ob[i].spec&OB_BUTTON?1:ob[i].spec&OB_EXIT?2:0;
    wi=XCreateSimpleWindow(dpy[sn],rt,
			   ob[i].x*charw[sn]-bd,dy+ob[i].y*charh[sn]-bd,
			   ob[i].w*charw[sn]-!!bd,ob[i].h*charh[sn]-!!bd,
			   bd,fgpix[sn],bd?btpix[sn]:bgpix[sn]);
    ob[i].win=wi;
    XSelectInput(dpy[sn],wi,ExposureMask|
		 (ob[i].spec&(OB_BUTTON|OB_EXIT)?ButtonPressMask:0));
    XMapWindow(dpy[sn],wi);
  }
  XMapWindow(dpy[sn],rt);
}

create_di(sn,ob)
int sn;
OBJECT *ob;
{
  int x,y;

  x=(desk.w-ob[0].w*charw[sn])/2;
  y=(desk.h-ob[0].h*charh[sn])/2;
  create_dial(sn,x,y,0,ob);
  actdial[sn]=ob;
}

remove_dial(sn,ob)
int sn;
OBJECT *ob;
{
  XDestroyWindow(dpy[sn],ob[0].win);
}

remove_di(sn)
int sn;
{
  remove_dial(sn,actdial[sn]);
  actdial[sn]=0;
}

draw_dial(sn,i,ob)
int sn,i;
OBJECT *ob;
{
  int x,y,w,h,l;

  if (!ob) return;
  if (!i) {
    w=ob[0].w*charw[sn];
    h=ob[0].h*charh[sn]+ob[0].y;
    XClearWindow(dpy[sn],ob[0].win);
    change_gc(sn,btpix[sn],gc);
    XDrawRectangle(dpy[sn],ob[0].win,gc[sn],0,0,w-1,h-1);
    XDrawRectangle(dpy[sn],ob[0].win,gc[sn],1,1,w-3,h-3);
    change_gc(sn,fgpix[sn],gc);
    XDrawRectangle(dpy[sn],ob[0].win,gc[sn],2,2,w-5,h-5);
  }
  else {
    if (ob[i].spec&OB_HIDDEN) return;
    XClearWindow(dpy[sn],ob[i].win);
    if (ob[i].str) {
      l=strlen(ob[i].str);
      x=y=0;
      if (ob[i].spec&(OB_BUTTON|OB_EXIT|OB_CENTERED)) {
	x=(ob[i].w-l)*charw[sn]/2;
	y=(ob[i].h-1)*charh[sn]/2;
      }
      XDrawString(dpy[sn],ob[i].win,gc[sn],x,y+dfont[sn]->ascent,
		  ob[i].str,l);
      if (ob[i].spec&OB_BOLD) {
	XDrawString(dpy[sn],ob[i].win,gc[sn],x+1,y+dfont[sn]->ascent,
		    ob[i].str,l);
      }
      if (ob[i].spec&OB_UNDERLINED && l) {
	XDrawLine(dpy[sn],ob[i].win,gc[sn],x,ob[i].h*charh[sn]-1,
		  x+l*charw[sn],ob[i].h*charh[sn]-1);
      }
    }
    if (ob[i].spec&OB_SELECTED) {
      if (ob[i].spec&(OB_BUTTON|OB_EXIT)) {
	change_gcxor(sn,btpix[sn]^fgpix[sn]^bgpix[sn]);
      }
      XFillRectangle(dpy[sn],ob[i].win,gcxor[sn],0,0,
		     ob[i].w*charw[sn],ob[i].h*charh[sn]);
      if (ob[i].spec&(OB_BUTTON|OB_EXIT)) {
	change_gcxor(sn,fgpix[sn]);
      }
    }
  }
}

draw_di(sn,idx)
int sn,idx;
{
  draw_dial(sn,idx,actdial[sn]);
}

refresh()
{
  int sn,i;
  XEvent event;

  for (sn=0;sn<numsp;sn++) {
    if (lost[sn]) continue;
    if (resdial[sn]) {
      for (i=0;i<resdial[sn][0].spec;i++) {
	if (resdial[sn][i].win) {
	  while (XCheckTypedWindowEvent(dpy[sn],resdial[sn][i].win,
					Expose,&event)) {
	    if (!event.xexpose.count) {
	      draw_dial(sn,i,resdial[sn]);
	    }
	  }
	}
      }
    }
    if (actdial[sn]) {
      for (i=0;i<actdial[sn][0].spec;i++) {
	if (actdial[sn][i].win) {
	  while (XCheckTypedWindowEvent(dpy[sn],actdial[sn][i].win,
					Expose,&event)) {
	    if (!event.xexpose.count) {
	      draw_dial(sn,i,actdial[sn]);
	    }
	  }
	}
      }
    }
    while (XCheckTypedWindowEvent(dpy[sn],win[sn],Expose,&event)) {
      XCopyArea(dpy[sn],bck[sn],win[sn],gc[sn],event.xexpose.x,event.xexpose.y,
		event.xexpose.width,event.xexpose.height,
		event.xexpose.x,event.xexpose.y);
    }
    XFlush(dpy[sn]);
  }
}

di_info(sn,th)
int sn,th;
{
  int s,x,y,w;
  char txt[20];
  char *clr="                    ";

  if (numsp==1) return;
  for (s=0;s<3;s++) {
    if (s!=sn) {
      strcpy(txt,spnames[s][0]);
      if (spnames[s][1][0]) {
	strcat(txt," ");
	strcat(txt,spnames[s][1]);
      }
      w=20*charw[sn];
      x=(s==left(sn)?desk.com1x:desk.com2x)+30-10*charw[sn];
      y=(s==left(sn)?desk.com1y[sn]:desk.com2y[sn])+130;
      if (th<-1) {
	v_gtext(sn,x,y,w,clr);
	continue;
      }
      v_gtext(sn,x,y,w,txt);
      y+=charh[sn];
      if (phase==SPIELEN && stich==1 && s==spieler) {
	  v_gtext(sn,x,y,w,clr);
	  strcpy(txt,textarr[TX_SPIELT]);
	  strcat(txt,textarr[TX_NULL+trumpf+1]);
	  v_gtext(sn,x,y,w,txt);
	  y+=charh[sn];
	  if (ouveang) {
	    if (handsp && trumpf==-1) {
	      v_gtext(sn,x,y,w,textarr[TX_OUVE_HAND]);
	    }
	    else {
	      v_gtext(sn,x,y,w,textarr[TX_OUVE]);
	    }
	  }
	  else if (schnang) {
	    v_gtext(sn,x,y,w,textarr[TX_SCHN_ANGE]);
	  }
	  else if (schwang) {
	    v_gtext(sn,x,y,w,textarr[TX_SCHW_ANGE]);
	  }
	  else if (handsp) {
	    v_gtext(sn,x,y,w,textarr[TX_HAND]);
	  }
      }
      else {
	if (th>=0) {
	  v_gtext(sn,x,y,w,clr);
	  if (s==th) {
	    v_gtext(sn,x,y,w,textarr[TX_UEBERLEGT]);
	  }
	  v_gtext(sn,x,y+charh[sn],w,clr);
	}
      }
    }
  }
}

di_hand()
{
  create_di(spieler,dihand);
}

di_term(sn,s)
int sn,s;
{
  static char txt[20];

  if (lost[sn]) return;
  if (actdial[sn]) remove_di(sn);
  diterm[sn][2].str=txt;
  strcpy(txt,spnames[s][0]);
  if (spnames[s][1][0]) {
    strcat(txt," ");
    strcat(txt,spnames[s][1]);
  }
  create_di(sn,diterm[sn]);
}

di_ende(sn)
int sn;
{
  create_di(sn,diende[sn]);
}

di_loesch(sn)
int sn;
{
  create_di(sn,diloesch);
}

di_ansage()
{
  static char txt[30];

  if (numsp==1 && iscomp(spieler)) {
    diansage[2].str=txt;
    strcpy(txt,textarr[TX_NULL+trumpf+1]);
    if (handsp) {
      strcat(txt," ");
      strcat(txt,textarr[TX_HAND]);
    }
    create_di(0,diansage);
  }
  else do_angesagt();
}

di_spiel()
{
  static char txt[33];

  sprintf(txt,textarr[TX_GEREIZT_BIS_N],reizw[reizp]);
  dispiel[12].str=txt;
  create_di(spieler,dispiel);
}

list_fun(sn)
int sn;
{
  int i,j,s,e,curr[3],acurr[3],*cp;
  static char txt[3][13][40];
  char form[20];

  curr[0]=splsum[0];
  curr[1]=splsum[1];
  curr[2]=splsum[2];
  acurr[0]=asplsum[0];
  acurr[1]=asplsum[1];
  acurr[2]=asplsum[2];
  cp=alist[sn]?acurr:curr;
  for (j=0;j<splfirst[sn];j++) {
    modsum(curr,acurr,j,(int *)0,(int *)0);
  }
  diliste[sn][8].str=txt[sn][0];
  sprintf(txt[sn][0],"%8d%10d%10d",cp[0],cp[1],cp[2]);
  for (i=splfirst[sn],j=1;j<13 && i<splstp;i++,j++) {
    diliste[sn][8+j].str=txt[sn][j];
    modsum(curr,acurr,i,&s,&e);
    if (alist[sn] && e>0) {
      sprintf(form,"%%%dd%%%dd%%+%dd",s<1?18:8,s==1?20:10,s>1?17:7);
      sprintf(txt[sn][j],form,s<1?cp[1]:cp[0],s>1?cp[1]:cp[2],e);
    }
    else {
      sprintf(form,"%%%dd%%+%dd",(s+1)*10-2,37-(s+1)*10);
      sprintf(txt[sn][j],form,cp[s],e);
    }
  }
  while (j<13) {
    diliste[sn][8+j++].str=OB_NONE;
  }
}

di_liste(sn,ini)
int sn,ini;
{
  set_names(diliste[sn],2);
  if (ini) splfirst[sn]=((splstp>0?splstp-1:0)/12)*12;
  diliste[sn][21].str=splfirst[sn]>=12?"<-":alist[sn]?"-->":"<--";
  diliste[sn][23].str=splfirst[sn]+12>=splstp?textarr[TX_LOESCHEN]:"->";
  diliste[sn][23].spec=splfirst[sn]+12<splstp ||
    (splstp && !sn && ini)?OB_EXIT:OB_HIDDEN;
  list_fun(sn);
  create_di(sn,diliste[sn]);
}

pformat(f,spec,txt)
FILE *f;
int spec;
char *txt;
{
  int i,l;

  i=12;
  if (spec&OB_CENTERED) {
    l=(12-strlen(txt))/2;
    i-=l;
    while (l-->0) fputc(' ',f);
  }
  while (*txt) {
    if (spec&OB_BOLD) fprintf(f,"%c\b%c",*txt,*txt);
    else if (spec&OB_UNDERLINED) fprintf(f,"_\b%c",*txt);
    else fputc(*txt,f);
    txt++;
    i--;
  }
  while (i-->0) fputc(' ',f);
}

prot_fun(sn,f)
int sn;
FILE *f;
{
  int tr,e,i,j,s,stiche[10][3];
  static char txt[3][10][3][20];

  tr=trumpf;
  trumpf=prot1.trumpf;
  for (s=0;s<3;s++) {
    for (i=0;i<10;i++) stiche[i][s]=prot1.stiche[i][s];
    if (protsort[sn]) {
      for (i=0;i<9;i++) {
        for (j=i+1;j<10;j++) {
          if (lower(stiche[i][s],stiche[j][s],trumpf==-1)) {
            swap(&stiche[i][s],&stiche[j][s]);
	  }
        }
      }
    }
  }
  trumpf=tr;
  for (i=0;i<10;i++) {
    for (s=0;s<3;s++) {
      if (protsort[sn]) {
        e=prot1.trumpf!=-1 &&
	  (stiche[i][s]>>3==prot1.trumpf || (stiche[i][s]&7)==BUBE)
	    ?e=OB_BOLD:OB_NONE;
      }
      else {
	if (i && prot1.stichgem<=i) {
	  e=OB_HIDDEN;
	}
	else {
	  e=prot1.anspiel[i]==s?OB_UNDERLINED:OB_NONE;
	  if (prot1.gemacht[i]==s) e|=OB_BOLD;
	}
      }
      diproto[sn][8+3*i+s].spec=e;
      diproto[sn][8+3*i+s].str=txt[sn][i][s];
      if (prot1.stichgem || protsort[sn]) {
	strcpy(txt[sn][i][s],textarr[TX_KARO+(stiche[i][s]>>3)]);
	strcat(txt[sn][i][s],textarr[TX_A+(stiche[i][s]&7)]);
      }
      else {
	strcpy(txt[sn][i][s],textarr[TX_PASSE]);
      }
    }
    if (f && diproto[sn][8+3*i].spec!=OB_HIDDEN) {
      fprintf(f,"  ");
      for (s=0;s<3;s++) {
	pformat(f,diproto[sn][8+3*i+s].spec,txt[sn][i][s]);
      }
      fprintf(f,"\n");
    }
  }
}

im_skat(s,i)
char *s;
int i;
{
  strcpy(s,textarr[TX_KARO+(prot1.skat[i][0]>>3)]);
  strcat(s,textarr[TX_A+(prot1.skat[i][0]&7)]);
  strcat(s,",");
  strcat(s,textarr[TX_KARO+(prot1.skat[i][1]>>3)]);
  strcat(s,textarr[TX_A+(prot1.skat[i][1]&7)]);
}

di_proto(sn,ini,log)
int sn,ini,log;
{
  static char txt[3][40],imski[3][40],imskw[3][40],bis[3][10],aug[3][20];
  char *hd="----------------------------------------\n";
  char *tl="========================================\n";
  char hdbuf[100];
  int s,p;
  FILE *f;

  if (log) {
    f=fopen(prot_file,"a");
    if (!f) {
      fprintf(stderr,"Can't write file %s\n",prot_file);
      return;
    }
    for (s=0;s<3;s++) {
      pformat(f,OB_CENTERED,spnames[s][0]);
    }
    fputc('\n',f);
    for (s=0;s<3;s++) {
      pformat(f,OB_CENTERED,spnames[s][1]);
    }
    fprintf(f,"\n%s",hd);
  }
  else f=0;
  if (ini) protsort[sn]=0;
  set_names(diproto[sn],2);
  diproto[sn][43].str=txt[sn];
  strcpy(txt[sn],spnames[prot1.spieler][0]);
  if (spnames[prot1.spieler][1][0]) {
    strcat(txt[sn]," ");
    strcat(txt[sn],spnames[prot1.spieler][1]);
  }
  strcat(txt[sn],textarr[TX_SPIELTE]);
  strcat(txt[sn],textarr[TX_NULL+prot1.trumpf+1]);
  diproto[sn][39].str=imski[sn];
  im_skat(imski[sn],1);
  diproto[sn][42].str=bis[sn];
  sprintf(bis[sn],"%d",prot1.gereizt);
  if (prot1.trumpf==-1) {
    diproto[sn][44].str=prot1.gewonn?textarr[TX_GEWONNEN]:textarr[TX_VERLOREN];
    diproto[sn][45].str=OB_NONE;
  }
  else {
    diproto[sn][44].str=prot1.gewonn?textarr[TX_GEWO_MIT]:textarr[TX_VERL_MIT];
    diproto[sn][45].str=aug[sn];
    sprintf(aug[sn],textarr[TX_AUGEN],prot1.augen);
  }
  p=protsort[sn] && prot1.trumpf==-1 && prot1.ehsso==4?0:prot1.ehsso;
  if (p) {
    diproto[sn][40].str=textarr[TX_HAND_GESP+p-1];
  }
  else {
    diproto[sn][40].str=imskw[sn];
  }
  sprintf(imskw[sn],"%s ",textarr[TX_IM_SKAT_WAR]);
  im_skat(imskw[sn]+strlen(imskw[sn]),0);
  diproto[sn][48].str=protsort[sn]?"<-":"->";
  prot_fun(sn,f);
  if (!prot1.stichgem) {
    diproto[sn][41].spec=OB_HIDDEN;
    diproto[sn][40].str=OB_NONE;
    diproto[sn][42].str=OB_NONE;
    diproto[sn][43].str=OB_NONE;
    diproto[sn][44].str=OB_NONE;
    diproto[sn][45].str=OB_NONE;
  }
  else {
    diproto[sn][41].spec=OB_NONE;
  }
  if (log) {
    strcpy(hdbuf,hd);
    if (prot1.predef!=1) {
      sprintf(hdbuf+4," random_seed %d %d",prot1.savseed,prot1.gamenr);
      hdbuf[strlen(hdbuf)]=' ';
    }
    fputs(hdbuf,f);
    protsort[sn]=1;
    prot_fun(sn,f);
    fputs(hd,f);
    fprintf(f,"%s %s\n",textarr[TX_IM_SKAT_IST],imski[sn]);
    if (prot1.stichgem) {
      fprintf(f,"%s\n",imskw[sn]);
      fprintf(f,"%s %s\n",textarr[TX_GEREIZT_BIS],bis[sn]);
      fprintf(f,"%s\n",txt[sn]);
      if (diproto[sn][40].str!=imskw[sn]) {
	fprintf(f,"%s\n",diproto[sn][40].str);
      }
      fprintf(f,"%s",diproto[sn][44].str);
      if (diproto[sn][45].str) fprintf(f," %s",diproto[sn][45].str);
      fputc('\n',f);
    }
    fputs(tl,f);
    fclose(f);
  }
  else {
    diproto[sn][46].str=
      !sn && protsort[sn]?textarr[TX_SPEICHERN]:textarr[TX_SPIELLISTE];
    create_di(sn,diproto[sn]);
  }
}

di_result()
{
  int sn;
  static char sa[30],sw[40],su[3][3][10];

  if (handsp && !iscomp(spieler)) {
    putcard(spieler,spcards[10],desk.playx+8*desk.cardx,desk.playy);
    putcard(spieler,spcards[11],desk.playx+9*desk.cardx,desk.playy);
  }
  diresult[2].str=
    mes1?textarr[TX_UEBERREIZT]:mes2?textarr[TX_SCHNEIDERFREI]:
      mes3?textarr[TX_NICHT_SCHWARZ]:OB_NONE;
  diresult[3].str=
    spgew?textarr[TX_DER_A_GEWINNT]:textarr[TX_DER_A_VERLIERT];
  diresult[4].str=sa;
  if (trumpf==-1) {
    strcpy(sa,textarr[TX_DAS_NULLSPIEL]);
  }
  else {
    if (schwz || !nullv) {
      strcpy(sa,textarr[TX_DAS_SPIEL_SCHWARZ]);
    }
    else {
      sprintf(sa,textarr[TX_MIT_AUGEN],stsum,120-stsum);
    }
  }
  diresult[5].str=sw;
  sprintf(sw,"%s %d.",textarr[TX_DER_SPIELWERT_IST],spgew?spwert:-spwert);
  for (sn=0;sn<numsp;sn++) {
    sprintf(su[sn][0],"%d",alist[sn]?asum[0]:sum[0]);
    sprintf(su[sn][1],"%d",alist[sn]?asum[1]:sum[1]);
    sprintf(su[sn][2],"%d",alist[sn]?asum[2]:sum[2]);
    diresult[12].str=su[sn][0];
    diresult[13].str=su[sn][1];
    diresult[14].str=su[sn][2];
    if (numsp>1) {
      set_names(dismlres[sn],5);
      dismlres[sn][1].str=diresult[2].str;
      dismlres[sn][2].str=diresult[3].str;
      dismlres[sn][3].str=diresult[4].str;
      dismlres[sn][4].str=diresult[5].str;
      dismlres[sn][11].str=diresult[12].str;
      dismlres[sn][12].str=diresult[13].str;
      dismlres[sn][13].str=diresult[14].str;
      resdial[sn]=dismlres[sn];
      create_dial(sn,(desk.w-resdial[sn][0].w*charw[sn])/2,5,3,resdial[sn]);
      dismlres[sn][11].spec|=spieler==0?OB_SELECTED:OB_NONE;
      dismlres[sn][12].spec|=spieler==1?OB_SELECTED:OB_NONE;
      dismlres[sn][13].spec|=spieler==2?OB_SELECTED:OB_NONE;
      if (alist[sn] && spgew) {
	dismlres[sn][11].spec^=OB_SELECTED;
	dismlres[sn][12].spec^=OB_SELECTED;
	dismlres[sn][13].spec^=OB_SELECTED;
      }
      phase=GEBEN;
    }
    else {
      set_names(diresult,6);
      create_di(sn,diresult);
      diresult[12].spec|=spieler==0?OB_SELECTED:OB_NONE;
      diresult[13].spec|=spieler==1?OB_SELECTED:OB_NONE;
      diresult[14].spec|=spieler==2?OB_SELECTED:OB_NONE;
      if (alist[sn] && spgew) {
	diresult[12].spec^=OB_SELECTED;
	diresult[13].spec^=OB_SELECTED;
	diresult[14].spec^=OB_SELECTED;
      }
    }
  }
}

di_delres(sn)
int sn;
{
  if (resdial[sn]) {
    remove_dial(sn,resdial[sn]);
    resdial[sn]=0;
  }
}

di_options(sn)
int sn;
{
  dioptions[sn][17].spec=splstp?OB_EXIT:OB_HIDDEN;
  dioptions[sn][18].spec=splres?OB_EXIT:OB_HIDDEN;
  create_di(sn,dioptions[sn]);
  dioptions[sn][5-sort1[sn]].spec|=OB_SELECTED;
  dioptions[sn][7-alternate[sn]].spec|=OB_SELECTED;
  dioptions[sn][9-sort2[sn]].spec|=OB_SELECTED;
}

di_copyr(sn)
int sn;
{
  create_di(sn,dicopyr[sn]);
}
