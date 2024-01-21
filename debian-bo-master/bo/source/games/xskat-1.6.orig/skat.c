
/*
    xskat - a card game for 1 to 3 players.
    Copyright (C) 1996  Gunter Gerhardt

    This program is free software; you can redistribute it freely.
    Use it at your own risk; there is NO WARRANTY.
*/

#define SKAT_C

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include "skat.h"
#include "text.h"

int iscomp(s)
int s;
{
  return s>=numsp;
}

swap(i,j)
int *i,*j;
{
  int h;

  h=*i;
  *i=*j;
  *j=h;
}

setrnd(s,v)
long *s,v;
{
  *s=v<<1?v:-1;
}

int rndval(s,m)
long *s;
int m;
{
  register long h=*s;
  int i;

  for (i=0;i<7;i++) h=(h<<16)|((((h<<1)^(h<<4))>>16)&0xffff);
  *s=h;
  return h&m;
}

int rnd(m)
int m;
{
  return rndval(&seed[1],m);
}

synerr(f,s)
FILE *f;
char *s;
{
  int c,l,n;

  fprintf(stderr,"Error in file %s",game_file);
  if (feof(f)) fputs(": unexpected EOF\n",stderr);
  else {
    fprintf(stderr," reading: '%s'\n",s);
    fputs("before:\n",stderr);
    l=3;
    n=200;
    while (l && n) {
      if ((c=fgetc(f))==EOF) l=0;
      else {
	if (c=='\n') l--;
	n--;
	fputc(c,stderr);
      }
    }
  }
  fclose(f);
  game_file=0;
}

int get_game()
{
  static int opened;
  static FILE *f;
  char s[100],*p;
  int n,cpos[4],cmax[4],state,rep;
  long num;

  if (predef==1) predef=0;
  if (!game_file) return 0;
  if (!opened) {
    if (!strcmp(game_file,"-")) {
      game_file="stdin";
      f=stdin;
    }
    else {
      f=fopen(game_file,"r");
    }
    if (!f) {
      fprintf(stderr,"Can't open file %s\n",game_file);
      exit(1);
    }
    opened=1;
  }
  state=rep=0;
  while (state!=4) {
    if (fscanf(f," %99s",s)!=1) {
      if (!state) break;
      synerr(f,"");
      return 0;
    }
    else if (*s=='#') {
      do {
	fgets(s,99,f);
      } while (s[strlen(s)-1]!='\n' && s[0]);
    }
    else if (!strcmp(s,"random_seed")) {
      if (!state) state=2;
      else {
	synerr(f,s);
	return 0;
      }
    }
    else if (!strcmp(s,"repeat")) {
      if (!state) {
	if (rep) break;
	rewind(f);
	rep=1;
      }
      else {
	synerr(f,s);
	return 0;
      }
    }
    else {
      num=strtol(s,&p,10);
      if (p!=s+strlen(s)) {
	synerr(f,s);
	return 0;
      }
      switch (state) {
      case 0:
	n=0;
	cpos[0]=30;cmax[0]=2;
	cpos[1]=sager*10;cmax[1]=10;
	cpos[2]=geber*10;cmax[2]=10;
	cpos[3]=hoerer*10;cmax[3]=10;
	state=1;
      case 1:
	if (num<0 || num>3 || !cmax[num]) {
	  synerr(f,s);
	  return 0;
	}
	cards[cpos[num]]=n;
	cpos[num]++;
	cmax[num]--;
	n++;
	if (n==32) {
	  predef=1;
	  return 1;
	}
	break;
      case 2:
	predef=2;
	setrnd(&seed[0],savseed=num);
	state=3;
	break;
      case 3:
	gamenr=num;
	while (num-->0) {
	  for (n=0;n<32;n++) rndval(&seed[0],0);
	}
	state=4;
	break;
      }
    }
  }
  fclose(f);
  game_file=0;
  return 0;
}

mischen()
{
  int i,j;

  if (!get_game()) {
    for (i=0;i<32;i++) cards[i]=i;
    for (i=0;i<32;i++) swap(&cards[i],&cards[rndval(&seed[0],31)]);
    for (i=0;i<10;i++) swap(&cards[geber*10+i],&cards[i]);
    for (i=0;i<10;i++) swap(&cards[hoerer*10+i],&cards[geber==1?i:10+i]);
    gamenr++;
  }
  setrnd(&seed[1],seed[0]);
  for (i=0;i<32;i++) gespcd[i]=0;
  for (i=0;i<4;i++) gespfb[i]=0;
  for (i=0;i<3;i++) {
    for (j=0;j<5;j++) hatnfb[i][j]=0;
  }
}

int lower(c1,c2,n)
int c1,c2,n;
{
  int f1,f2,w1,w2;

  if (c1<0) return 1;
  if (c2<0) return 0;
  f1=c1>>3;
  f2=c2>>3;
  w1=c1&7;
  w2=c2&7;
  if (n) {
    if (sortw[f1]<sortw[f2]) return 1;
    if (sortw[f1]>sortw[f2]) return 0;
    if (w1==ZEHN) return w2<=BUBE;
    if (w2==ZEHN) return w1>BUBE;
    return w1>w2;
  }
  if (w2==BUBE) {
    if (w1!=BUBE) return 1;
    return f1<f2;
  }
  else {
    if (w1==BUBE) return 0;
    if (f2==trumpf && f1!=trumpf) return 1;
    if (f1==trumpf && f2!=trumpf) return 0;
    if (sortw[f1]<sortw[f2]) return 1;
    if (sortw[f1]>sortw[f2]) return 0;
    return w1>w2;
  }
}

sort(sn)
int sn;
{
  int i,j,f=sn*10;
  int hatfb[4],fbsum,firstf;

  sortw[0]=0;
  sortw[1]=1;
  sortw[2]=2;
  sortw[3]=3;
  if (alternate[sn]) {
    hatfb[0]=hatfb[1]=hatfb[2]=hatfb[3]=0;
    for (i=f;i<f+10;i++) {
      if (cards[i]>=0 && ((cards[i]&7)!=BUBE || sort2[sn])) {
        hatfb[cards[i]>>3]=1;
      }
    }
    if (!sort2[sn] && trumpf>=0 && trumpf<4 && hatfb[trumpf]) {
      hatfb[trumpf]=0;
      firstf=trumpf;
    }
    else firstf=-1;
    fbsum=hatfb[0]+hatfb[1]+hatfb[2]+hatfb[3];
    if ((hatfb[0] || hatfb[1]) && (hatfb[2] || hatfb[3])) {
      switch (fbsum) {
      case 4:
        sortw[1]=2;
        sortw[2]=1;
        break;
      case 3:
        if (hatfb[0] && hatfb[1]) {
          sortw[0]=0;
          sortw[1]=2;
          sortw[2]=sortw[3]=1;
        }
        else {
          sortw[2]=0;
          sortw[3]=2;
          sortw[0]=sortw[1]=1;
        }
        break;
      case 2:
        if (firstf>1) {
          sortw[0]=sortw[1]=1;
          sortw[2]=sortw[3]=0;
        }
        break;
      }
    }
  }
  for (i=f;i<f+9;i++) {
    for (j=i+1;j<f+10;j++) {
      if (lower(cards[i],cards[j],sort2[sn])^sort1[sn])
        swap(&cards[i],&cards[j]);
    }
  }
  sortw[0]=0;
  sortw[1]=1;
  sortw[2]=2;
  sortw[3]=3;
}

testnull(s)
int s;
{
  int i,f,c;
  int a[4],l[4],n[4],m[4],h[4];

  for (i=0;i<4;i++) a[i]=l[i]=n[i]=m[i]=h[i]=0;
  f=1;
  for (i=0;i<10;i++) {
    c=cards[10*s+i];
    a[c>>3]++;
    if ((c&7)>BUBE) l[c>>3]++;
    else if ((c&7)<BUBE && (c&7)!=ZEHN) h[c>>3]=1;
    else m[c>>3]=1;
    if ((c&7)==NEUN) n[c>>3]=1;
  }
  for (i=0;i<4;i++) {
    if ((a[i] && l[i]!=a[i] && l[i]<2) ||
	(l[i]==1 && n[i]) ||
	(l[i]!=3 && !m[i] && h[i])) f=0;
  }
  if (f) maxrw[s]=35;
}

calc_rw(s)
int s;
{
  int i,c,f,tr,bb,as,ze,dk;
  int b[4],t[4];

  maxrw[s]=0;
  b[0]=b[1]=b[2]=b[3]=0;
  t[0]=t[1]=t[2]=t[3]=0;
  bb=as=ze=dk=0;
  for (i=0;i<10;i++) {
    c=cards[10*s+i];
    if ((c&7)==BUBE) {
      b[c>>3]=1;
      bb++;
    }
    else t[c>>3]++;
  }
  tr=0;
  for (i=1;i<4;i++) {
    if (t[i]>=t[tr]) tr=i;
  }
  for (i=0;i<10;i++) {
    c=cards[10*s+i];
    if ((c&7)!=BUBE && c>>3!=tr) {
      switch (c&7) {
      case AS:as++;break;
      case ZEHN:ze++;break;
      default:dk+=cardw[c&7];
      }
    }
  }
  if ((bb+t[tr]==4 &&
       (
        (as==2 && ze>=2 && dk+10*ze>=24) ||
        (as>=3 && as+ze>=4)
	)) ||
      (bb+t[tr]==5 &&
       (
        (ze>=2 && dk+10*ze>=27 && (b[3] || b[2])) ||
        (as>=1 && ze>=1 && dk+10*ze>=14 && (b[3] || b[2])) ||
        (as>=2 && dk+10*ze>=6) ||
        (as>=3)
	)) ||
      (bb+t[tr]==6 &&
       (
        (dk+10*ze+11*as>=11)
	)) ||
      bb+t[tr]>=7
      ) {
    f=2;
    if (b[3]) {
      while (f<5 && b[4-f]) f++;
    }
    maxrw[s]=f*rwert[tr];
  }
  if (!maxrw[s]) testnull(s);
  if (!maxrw[s] && (b[3] || b[2] || bb==2) &&
      ((b[3] && b[2] && as>=2) ||
       (bb+t[tr]==4 && as>=1 && dk+10*ze+11*as>=27) ||
       (bb+t[tr]==5 && dk+10*ze+11*as>=18) ||
       (bb+t[tr]==5 && bb==2 && 10*ze+11*as>=20) ||
       (bb+t[tr]==6))) maxrw[s]=18;
}

do_geben()
{
  int sn;

  sort2[0]=sort2[1]=sort2[2]=0;
  trumpf=-1;
  schnang=schwang=ouveang=0;
  calc_desk();
  geber=(geber+1)%3;
  hoerer=(geber+1)%3;
  sager=(geber+2)%3;
  mischen();
  setcurs(-1);
  givecard(hoerer,0);
  givecard(sager,0);
  givecard(geber,0);
  givecard(-1,0);
  givecard(hoerer,1);
  givecard(sager,1);
  givecard(geber,1);
  givecard(hoerer,2);
  givecard(sager,2);
  givecard(geber,2);
  for (sn=0;sn<numsp;sn++) initscr(sn);
  putmark(hoerer);
  lastmsaho[0]=0;
  lastmsaho[1]=0;
  lastmsaho[2]=0;
  saho=1;
  reizp=0;
  put_box(sager);
  put_box(hoerer);
  calc_rw(1);
  calc_rw(2);
  clear_info();
  phase=REIZEN;
}

do_sagen(s,w)
int s,w;
{
  char str[4];

  sprintf(str,"%d",w);
  b_text(s,str);
  inv_box(s,0);
  stdwait();
  inv_box(s,0);
}

do_passen(s)
int s;
{
  b_text(s,textarr[TX_PASSE]);
  inv_box(s,64);
  stdwait();
  inv_box(s,64);
  rem_box(s);
}

do_akzept(s)
int s;
{
  char txt[33];

  b_text(s,textarr[TX_JA]);
  inv_box(s,0);
  stdwait();
  inv_box(s,0);
  strcpy(txt,"                                ");
  txt[strlen(textarr[TX_JA])]=0;
  b_text(s,txt);
}

do_msagen(sn,w)
int sn,w;
{
  char str[4];

  if (lastmsaho[sn]==w) return;
  lastmsaho[sn]=w;
  sprintf(str,"%d",w);
  do_msaho(sn,str);
}

do_mhoeren(sn)
int sn;
{
  if (lastmsaho[sn]==1) return;
  lastmsaho[sn]=1;
  do_msaho(sn,textarr[TX_JA]);
}

do_entsch()
{
  int rw;

  rw=reizw[reizp];
  if (saho) {
    if (maxrw[sager]>=rw) {
      do_sagen(sager,rw);
      saho=0;
      if (sager==hoerer) {
        spieler=sager;
	do_handspiel();
      }
    }
    else {
      do_passen(sager);
      if (sager==geber || sager==hoerer) {
        if (sager==hoerer) {
          reizp--;
	  do_handspiel();
        }
        else {
          if (reizp) {
            spieler=hoerer;
            reizp--;
	    do_handspiel();
          }
          else {
            rem_box(sager);
            sager=hoerer;
          }
        }
      }
      else {
        rem_box(sager);
        sager=geber;
        put_box(sager);
      }
    }
  }
  else {
    if (maxrw[hoerer]>=rw) {
      do_akzept(hoerer);
      reizp++;
      saho=1;
    }
    else {
      do_passen(hoerer);
      if (sager==geber) {
        spieler=sager;
	do_handspiel();
      }
      else {
        rem_box(hoerer);
        rem_box(sager);
        hoerer=sager;
        sager=geber;
        reizp++;
        saho=1;
        put_box(hoerer);
        put_box(sager);
      }
    }
  }
}

do_reizen()
{
  while (phase==REIZEN &&
	 ((iscomp(sager) && saho) || (iscomp(hoerer) && !saho))) {
    do_entsch();
  }
  if (phase==REIZEN) {
    if (saho) do_msagen(sager,reizw[reizp]);
    else do_mhoeren(hoerer);
  }
}

drueck(f,n)
int f,n;
{
  int i,j;

  do {
    for (i=1;i<8;i++) {
      if (inhand[f][i]) {
        inhand[f][i]=0;
        if (!gedr && cards[31]==(f<<3)+i) {
          swap(&cards[30],&cards[31]);
          break;
        }
        for (j=0;j<10;j++) {
          if (cards[spieler*10+j]==(f<<3)+i) {
            swap(&cards[30+gedr],&cards[10*spieler+j]);
            break;
          }
        }
        break;
      }
    }
    gedr++;
  } while (--n && gedr<2);
}

truempfe()
{
  int i,c;

  for (c=0;c<2;c++) {
    if ((cards[30+c]&7)==BUBE || cards[30+c]>>3==trumpf) {
      for (i=0;i<10;i++) {
        if ((cards[10*spieler+i]&7)!=BUBE
          && cards[10*spieler+i]>>3!=trumpf) {
          swap(&cards[30+c],&cards[10*spieler+i]);
          break;
        }
      }
    }
  }
}

testgrand(bb,b)
int bb,b[4];
{
  int i,j,fl,ih,g3,g4;

  fl=g3=g4=0;
  for (i=0;i<4;i++) {
    ih=0;
    for (j=0;j<8;j++) {
      if (j!=BUBE && inhand[i][j]) ih++;
    }
    for (j=0;j<8;j++) {
      if (j!=BUBE) {
        if (inhand[i][j]) fl++;
        else if (7-ih>j) break;
      }
    }
    if ((ih>4) ||
	(ih>3 && (inhand[i][AS] || inhand[i][ZEHN]))) g4=1;
    if (ih>4 && (inhand[i][AS] || inhand[i][ZEHN])) g3=1;
    if (ih>3 && inhand[i][AS] && inhand[i][ZEHN]) g3=1;
  }
  if ((fl+bb>6) ||
      (bb==4 && g4) ||
      (bb==3 && (b[3] || spieler==ausspl) && g3)) trumpf=4;
}

calc_drueck()
{
  int i,c,f,bb,n;
  int b[4],t[4],p[4];

  if (maxrw[spieler]==35) {
    trumpf=-1;
    handsp=1;
    gedr=2;
    return;
  }
  b[0]=b[1]=b[2]=b[3]=0;
  t[0]=t[1]=t[2]=t[3]=0;
  p[0]=p[1]=p[2]=p[3]=0;
  bb=0;
  for (i=0;i<4;i++) {
    for (c=0;c<8;c++) inhand[i][c]=0;
  }
  for (i=0;i<12;i++) {
    c=spcards[i];
    if ((c&7)==BUBE) {
      b[c>>3]=1;
      bb++;
    }
    else {
      p[c>>3]+=cardw[c&7];
      t[c>>3]++;
      inhand[c>>3][c&7]=1;
    }
  }
  f=2;
  while (f<5 && b[4-f]==b[3]) f++;
  trumpf=0;
  while (f*rwert[trumpf]<reizw[reizp]) trumpf++;
  for (i=trumpf+1;i<4;i++) {
    if (t[i]>t[trumpf] || (t[i]==t[trumpf] && p[i]<=p[trumpf])) trumpf=i;
  }
  truempfe();
  for (n=1;n<8 && gedr<2;n++) {
    for (i=0;i<4 && gedr<2;i++) {
      if (t[i]==n && trumpf!=i) {
	if (inhand[i][AS]) {
	  if (!inhand[i][ZEHN] && n!=1) drueck(i,n==2?1:2);
	}
	else drueck(i,n==1?1:2);
      }
    }
  }
  if (bb>2 || (b[3] && bb==2 && spieler==ausspl)) testgrand(bb,b);
  gespcd[cards[30]]=1;
  gespcd[cards[31]]=1;
}

do_handspiel()
{
  int i;

  if (reizp<0) {
    clr_desk();
    phase=GEBEN;
    stich=1;
    fill_st();
    prot2.stichgem=0;
    prot2.trumpf=4;
    prot2.predef=predef;
    prot2.savseed=savseed;
    prot2.gamenr=gamenr-1;
    prot2.anspiel[0]=(geber+1)%3;
    prot2.gemacht[0]=-1;
    prot2.skat[1][0]=cards[30];
    prot2.skat[1][1]=cards[31];
    prot1=prot2;
    if (logging) di_proto(0,1,1);
    splres=1;
    return;
  }
  info_reiz();
  drkcd=0;
  handsp=0;
  stsum=0;
  vmh=0;
  gedr=0;
  ausspl=(geber+1)%3;
  for (i=0;i<10;i++) spcards[i]=cards[spieler*10+i];
  if (lower(cards[31],cards[30],0)) {
    swap(&cards[31],&cards[30]);
  }
  prot2.skat[0][0]=spcards[10]=cards[30];
  prot2.skat[0][1]=spcards[11]=cards[31];
  rem_box(sager);
  rem_box(hoerer);
  if (!iscomp(spieler)) {
    phase=HANDSPIEL;
    di_hand();
  }
  else do_handok();
}

do_handok()
{
  if (iscomp(spieler) || handsp) {
    home_skat();
    if (iscomp(spieler)) calc_drueck();
    stsum=cardw[cards[30]&7]+cardw[cards[31]&7];
    if (lower(cards[31],cards[30],0)) {
      swap(&cards[31],&cards[30]);
    }
    prot2.skat[1][0]=cards[30];
    prot2.skat[1][1]=cards[31];
  }
  if (!iscomp(spieler) && !handsp) {
    draw_skat();
    put_fbox();
    phase=DRUECKEN;
  }
  else do_ansagen();
}

do_ansagen()
{
  phase=ANSAGEN;
  if (!iscomp(spieler)) {
    di_spiel();
  }
  else {
    remmark();
    di_ansage();
  }
}

do_angesagt()
{
  int sn;

  if (!iscomp(spieler)) {
    remmark();
  }
  phase=SPIELEN;
  stich=1;
  schwz=1;
  nullv=0;
  info_spiel();
  sort2[0]=sort2[1]=sort2[2]=trumpf==-1;
  if (ouveang && numsp>1) {
    for (sn=0;sn<numsp;sn++) di_info(sn,-2);
    calc_desk();
  }
  for (sn=0;sn<numsp;sn++) initscr(sn);
}

int higher(c1,c2)
int c1,c2;
{
  int f1,w1,f2,w2;

  if (c2==-1) return 1;
  f1=c1>>3;
  w1=c1&7;
  f2=c2>>3;
  w2=c2&7;
  if (trumpf==-1) {
    if (f1==f2) {
      if (w1==ZEHN) return w2>BUBE;
      if (w2==ZEHN) return w1<=BUBE;
      return w1<w2;
    }
    return 1;
  }
  if (w1==BUBE) {
    if (w2==BUBE) return f1>f2;
    else return 1;
  }
  if (w2==BUBE) return 0;
  if (f1==f2) return w1<w2;
  if (f2==trumpf) return 0;
  return 1;
}

calc_result()
{
  int i,c,f;
  int b[4],s[8];

  mes1=mes2=mes3=0;
  if (trumpf==-1) {
    f=0;
    if (handsp) f++;
    if (ouveang) f+=2;
    spwert=nullw[f];
    if (nullv) {
      spgew=0;
      if (!handsp) spwert<<=1;
    }
    else spgew=1;
    return;
  }
  b[0]=b[1]=b[2]=b[3]=0;
  s[0]=s[1]=s[2]=s[3]=s[4]=s[5]=s[6]=s[7]=0;
  for (i=0;i<12;i++) {
    c=spcards[i];
    if ((c&7)==BUBE) b[c>>3]=1;
    else if (c>>3==trumpf) s[c&7]=1;
  }
  s[BUBE]=s[BUBE+1];
  s[BUBE+1]=s[BUBE+2];
  s[BUBE+2]=s[BUBE+3];
  f=1;
  while (f<4 && b[3-f]==b[3]) f++;
  if (f==4 && trumpf!=4) {
    while (f<11 && s[f-4]==b[3]) f++;
  }
  f++;
  if (handsp) f++;
  if (stsum>=90 || schnang || stsum<=30) f++;
  if (schnang) f++;
  if (schwz || schwang || !nullv) f++;
  if (schwang) f++;
  if (ouveang) f++;
  if (trumpf==4 && ouveang) spwert=(f-1)*36;
  else spwert=f*rwert[trumpf];
  if (stsum>60 && spwert>=reizw[reizp]
    && (stsum>=90 || !schnang)
    && (schwz || !schwang)) spgew=1;
  else {
    if (spwert<reizw[reizp]) mes1=1;
    else if (schnang && stsum<90) mes2=1;
    else if (schwang && !schwz) mes3=1;
    spgew=0;
    if (!handsp) spwert<<=1;
    else if (spwert<reizw[reizp]) {
      spwert=((reizw[reizp]-1)/rwert[trumpf]+1)*rwert[trumpf];
    }
  }
}

get_next()
{
  int s;

  prot2.anspiel[stich-1]=ausspl;
  prot2.stiche[stich-1][ausspl]=stcd[0];
  prot2.stiche[stich-1][(ausspl+1)%3]=stcd[1];
  prot2.stiche[stich-1][(ausspl+2)%3]=stcd[2];
  if (higher(stcd[0],stcd[1])) {
    if (higher(stcd[0],stcd[2])) s=0;
    else s=2;
  }
  else {
    if (higher(stcd[1],stcd[2])) s=1;
    else s=2;
  }
  ausspl=(ausspl+s)%3;
  prot2.gemacht[stich-1]=ausspl;
  if (spieler==ausspl) {
    stsum+=cardw[stcd[0]&7]+cardw[stcd[1]&7]+cardw[stcd[2]&7];
    nullv=1;
  }
  else schwz=0;
  info_stich(0,stcd[0],0);
  info_stich(1,stcd[1],0);
  info_stich(2,stcd[2],0);
}

save_list()
{
  FILE *f;
  int i;

  if (!list_file) return;
  f=fopen(list_file,"w");
  if (!f) {
    fprintf(stderr,"Can't write file %s\n",list_file);
    return;
  }
  fprintf(f,"%d %d %d\n",splsum[0],splsum[1],splsum[2]);
  fprintf(f,"a %d %d %d\n",asplsum[0],asplsum[1],asplsum[2]);
  for (i=0;i<splstp;i++) {
    fprintf(f,"%d\n",splist[i]);
  }
  fclose(f);
}

modsum(s1,s2,p,as,ae)
int *s1,*s2,p,*as,*ae;
{
  int s,e;

  s=splist[p]/10000;
  e=splist[p]%10000>>1;
  if (!(splist[p]&1)) e=-e;
  s1[s]+=e;
  if (e<0) {
    s2[s]-=e;
  }
  else {
    s2[(s+1)%3]+=e;
    s2[(s+2)%3]+=e;
  }
  if (as) *as=s;
  if (ae) *ae=e;
}

read_list()
{
  FILE *f;
  int c;

  if (!list_file) return;
  f=fopen(list_file,"r");
  if (!f) return;
  splstp=0;
  if (fscanf(f,"%d %d %d\n",&splsum[0],&splsum[1],&splsum[2])!=3) {
    splsum[0]=splsum[1]=splsum[2]=0;
    fclose(f);
    return;
  }
  if (fscanf(f,"a %d %d %d\n",&asplsum[0],&asplsum[1],&asplsum[2])!=3) {
    asplsum[0]=asplsum[1]=asplsum[2]=0;
  }
  sum[0]=splsum[0];
  sum[1]=splsum[1];
  sum[2]=splsum[2];
  asum[0]=asplsum[0];
  asum[1]=asplsum[1];
  asum[2]=asplsum[2];
  do {
    if (fscanf(f,"%d\n",&splist[splstp])) {
      modsum(sum,asum,splstp,(int *)0,(int *)0);
      splstp++;
    }
    else {
      while ((c=getc(f))!=EOF && c!='\n');
    }
  } while (!feof(f) && splstp<360);
  fclose(f);
}

fill_st()
{
  int i,j,s;

  for (s=0;s<3;s++) {
    i=stich-1;
    for (j=0;j<10;j++) {
      if (cards[10*s+j]>=0) prot2.stiche[i++][s]=cards[10*s+j];
    }
  }
}

do_next()
{
  int i;

  if (vmh==2) {
    get_next();
    stdwait();
    nimm_stich();
    vmh=0;
    stich++;
    if (stich==11 || (trumpf==-1 && nullv)) {
      if (stich<11) fill_st();
      calc_result();
      prot2.stichgem=stich-1;
      prot2.spieler=spieler;
      prot2.trumpf=trumpf;
      prot2.gereizt=reizw[reizp];
      prot2.gewonn=spgew;
      prot2.augen=stsum;
      prot2.ehsso=
        ouveang?trumpf==-1 && handsp?5:4:schwang?3:schnang?2:handsp?1:0;
      prot2.predef=predef;
      prot2.savseed=savseed;
      prot2.gamenr=gamenr-1;
      prot1=prot2;
      if (splstp>=360) {
      	for (i=0;i<12;i++) {
	  modsum(splsum,asplsum,i,(int *)0,(int *)0);
      	}
      	for (i=12;i<splstp;i++) splist[i-12]=splist[i];
      	splstp-=12;
      }
      splist[splstp++]=spieler*10000+spwert*2+spgew;
      modsum(sum,asum,splstp-1,(int *)0,(int *)0);
      save_list();
      if (logging) di_proto(0,1,1);
      splres=1;
      clr_desk();
      phase=RESULT;
      di_result();
    }
  }
  else vmh++;
}

calc_poss(s)
int s;
{
  int i,j,f1,w1,f2,w2;

  possc=0;
  for (i=0;i<10;i++) {
    if (cards[s*10+i]>=0) possi[possc++]=s*10+i;
  }
  if (vmh) {
    f1=stcd[0]>>3;
    w1=stcd[0]&7;
    if (trumpf!=-1 && w1==BUBE) f1=trumpf;
    i=j=0;
    do {
      f2=cards[possi[i]]>>3;
      w2=cards[possi[i]]&7;
      if (trumpf!=-1 && w2==BUBE) f2=trumpf;
      if (f1==f2) possi[j++]=possi[i];
    } while (++i<possc);
    if (j) possc=j;
    else hatnfb[s][f1]=1;
  }
}

calc_high(f)
int f;
{
  int i,j;

  high[0]=high[1]=high[2]=high[3]=high[4]=-1;
  for (i=0;i<4;i++) {
    for (j=0;j<8;j++) {
      if (j==BUBE) j++;
      if (gespcd[i<<3|j]<f) {
        high[i]=i<<3|j;
        break;
      }
    }
  }
  for (i=3;i>=0;i--) {
    if (gespcd[i<<3|BUBE]<f) {
      high[trumpf]=i<<3|BUBE;
      break;
    }
  }
}

int uebernehmen(p,h)
int p,h;
{
  int i,j,ci,cj,wi,wj;

  j=0;
  for (i=0;i<possc;i++) {
    ci=cards[possi[i]];
    if (!higher(stcd[p],ci)) {
      if (j) {
        cj=cards[possi[j-1]];
        wi=cardw[ci&7];
        wj=cardw[cj&7];
        if ((ausspl+vmh)%3==spieler) {
          if (wj==11) continue;
          if (wi==11 && cj>>3!=trumpf) {
            j=i+1;
            continue;
          }
        }
        if (wi==10) wi=12-h*2;
        if (wj==10) wj=12-h*2;
        if (wi==2 && wj==2) {
          wi=cj>>3;
          wj=ci>>3;
        }
        else {
          if (wi==2) wi=5-h*6;
          if (wj==2) wj=5-h*6;
        }
        if ((wi<wj)^h) j=i+1;
      }
      else j=i+1;
    }
  }
  if (j) {
    cj=cards[possi[j-1]];
    wj=cardw[cj&7];
    if (!h && wj==10 && !gespcd[(cj&0x18)|AS]) j=0;
    else playcd=j-1;
  }
  return j!=0;
}

schmieren()
{
  int i,j,wi,wj,ci,cj;

  j=0;
  for (i=1;i<possc;i++) {
    wi=cardw[(ci=cards[possi[i]])&7];
    wj=cardw[(cj=cards[possi[j]])&7];
    if (wi==2) wi=-1;
    else if (ci>>3==trumpf && cj>>3!=trumpf) wi=1;
    else if (wi==11) wi=9;
    if (wj==2) wj=-1;
    else if (cj>>3==trumpf && ci>>3!=trumpf) wj=1;
    else if (wj==11) wj=9;
    if (wi>wj) j=i;
  }
  playcd=j;
}

int einstechen()
{
  int ci;

  if (!cardw[stcd[0]&7] || !uebernehmen(0,0)) return 0;
  ci=cards[possi[playcd]];
  if ((ci&7)<=ZEHN || (ci&7)==BUBE) return 0;
  if (ci>>3==trumpf) return 1;
  return 0;
}

int niedrighoch(f)
int f;
{
  int i,ok,gespsav[32];

  for (i=0;i<32;i++) gespsav[i]=gespcd[i];
  ok=0;
  do {
    calc_high(1);
    if (ok) ok=2;
    for (i=0;i<possc;i++) {
      if (cards[possi[i]]==high[f]) {
        ok=1;
        playcd=i;
        gespcd[cards[possi[i]]]=2;
      }
    }
  } while (ok==1);
  for (i=0;i<32;i++) gespcd[i]=gespsav[i];
  return ok;
}

int trumpfausspielen()
{
  int i,j,g1,g2,tr,trdr,wi,wj;

  g1=(spieler+1)%3;
  g2=(spieler+2)%3;
  if (!hatnfb[g1][trumpf] ||
    !hatnfb[g2][trumpf]) {
    if (niedrighoch(trumpf)) return 1;
  }
  calc_high(1);
  tr=wj=0;
  j=-1;
  for (i=0;i<possc;i++) {
    if (cards[possi[i]]>>3==trumpf || (cards[possi[i]]&7)==BUBE) {
      tr++;
      wi=cardw[cards[possi[i]]&7];
      if (wi==2) wi=-1;
      if (j<0 || wi<wj) {
        j=i;
        wj=wi;
      }
    }
  }
  if (trumpf<4) trdr=7-gespfb[trumpf];
  else trdr=0;
  for (i=0;i<4;i++) if (!gespcd[i<<3|BUBE]) trdr++;
  if ((tr>2 && (trumpf!=4 || trdr-tr)) || (tr>1 && trdr-tr && trdr-tr<=2)) {
    playcd=j;
    return 1;
  }
  for (i=0;i<possc;i++) {
    for (j=0;j<4;j++) {
      if (j!=trumpf && cards[possi[i]]==high[j]) {
	if ((cards[possi[i]]&7)==AS) playcd=i;
        else niedrighoch(j);
        return 1;
      }
    }
  }
  return 0;
}

int hochausspielen()
{
  int i,j;

  calc_high(2);
  for (i=0;i<possc;i++) {
    for (j=0;j<4;j++) {
      if (j!=trumpf &&
        cards[possi[i]]==high[j] &&
        !hatnfb[spieler][j]) {
        playcd=i;
        return 1;
      }
    }
  }
  return 0;
}

schenken()
{
  int i,j,ci,cj,wi,wj;

  j=0;
  for (i=1;i<possc;i++) {
    ci=cards[possi[i]];
    cj=cards[possi[j]];
    wi=cardw[ci&7];
    wj=cardw[cj&7];
    if (wi==2) wi=5;
    if (wj==2) wj=5;
    if (wi==5 && wj==5) {
      wi=ci>>3;
      wj=cj>>3;
    }
    if ((ci&7)!=BUBE && ci>>3==trumpf) wi+=5;
    if ((cj&7)!=BUBE && cj>>3==trumpf) wj+=5;
    if (wi<wj) j=i;
  }
  playcd=j;
}

int zehnblank(ci)
int ci;
{
  int i,f,n,z,a,cj;

  f=ci>>3;
  n=z=a=0;
  for (i=0;i<possc;i++) {
    cj=cards[possi[i]];
    if (cj>>3==f) {
      n++;
      if ((cj&7)==ZEHN) z=1;
      else if ((cj&7)==AS) a=1;
    }
  }
  return z && !a && n==2 && !hatnfb[spieler][f];
}

abwerfen(f)
int f;
{
  int i,j,k,ci,cj,wi,wj;
  int n[4];

  j=0;
  for (i=1;i<possc;i++) {
    ci=cards[possi[i]];
    cj=cards[possi[j]];
    wi=cardw[ci&7];
    wj=cardw[cj&7];
    if (wi==2) wi=5;
    if (wj==2) wj=5;
    if (wi==5 && wj==5) {
      wi=ci>>3;
      wj=cj>>3;
    }
    else {
      if (wi==5 || ci>>3==trumpf) wi+=5;
      if (wj==5 || cj>>3==trumpf) wj+=5;
      if (!wi && zehnblank(ci)) wi+=5;
      if (!wj && zehnblank(cj)) wj+=5;
      if (f) {
	if (trumpf==4) {
	  if ((ci&7)!=BUBE && hatnfb[spieler][ci>>3]) wi-=30;
	  if ((cj&7)!=BUBE && hatnfb[spieler][cj>>3]) wj-=30;
	}
	else {
	  if (hatnfb[spieler][ci>>3] || wi>4) wi+=8;
	  if (hatnfb[spieler][cj>>3] || wj>4) wj+=8;
	}
        if (wi==wj && stich<=3 && ci>>3!=cj>>3) {
          n[0]=n[1]=n[2]=n[3]=0;
          for (k=0;k<possc;k++) {
            if ((cards[possi[k]]&7)!=BUBE) {
              n[cards[possi[k]]>>3]++;
            }
          }
          if (n[ci>>3]<n[cj>>3]) wi--;
          if (n[cj>>3]<n[ci>>3]) wj--;
          if (spieler==(ausspl+1)%3) swap(&wi,&wj);
        }
      }
      else {
        if ((ci&7)==BUBE || (!hatnfb[spieler][ci>>3] && wi>=4)) wi+=8;
        if ((cj&7)==BUBE || (!hatnfb[spieler][cj>>3] && wj>=4)) wj+=8;
      }
    }
    if (wi<wj) j=i;
  }
  playcd=j;
}

int buttern()
{
  int fb,mi,se;

  se=(ausspl+1)%3;
  mi=spieler==ausspl?(ausspl+2)%3:ausspl;
  fb=stcd[0]>>3;
  if ((stcd[0]&7)==BUBE) fb=trumpf;
  if (!hatnfb[se][fb]) return 0;
  calc_high(2);
  if (spieler==ausspl) {
    if (higher(stcd[0],high[fb])) {
      if (fb==trumpf) return 0;
      return hatnfb[mi][fb]==1 && hatnfb[mi][trumpf]!=1;
    }
    if (hatnfb[mi][fb]==1 && hatnfb[mi][trumpf]==1) return 0;
    if (fb==trumpf) return rnd(3)>1;
    return rnd(3)>0;
  }
  if (higher(stcd[0],high[trumpf]) && higher(stcd[0],high[fb])) return 1;
  if (fb!=trumpf && higher(stcd[0],high[fb]) && !hatnfb[spieler][fb]) {
    return rnd(3)>0;
  }
  return 0;
}

int hatas()
{
  int f,i;

  f=stcd[0]>>3;
  if ((stcd[0]&7)==BUBE ||
      cardw[stcd[0]&7]>4 || hatnfb[spieler][f] || f==trumpf) return 0;
  f=f<<3|AS;
  for (i=0;i<possc;i++) {
    if (cards[possi[i]]==f) {
      playcd=i;
      return 1;
    }
  }
  return 0;
}

int schnippeln(f)
int f;
{
  int fb,i,j,k,as,hi;

  fb=stcd[0]>>3;
  if ((stcd[0]&7)==BUBE ||
      (stcd[f]&7)==BUBE ||
      fb==trumpf || stcd[f]>>3==trumpf ||
      (f && fb!=stcd[1]>>3) ||
      gespcd[fb<<3|ZEHN]==2 ||
      gespfb[fb]>3) {
    return 0;
  }
  as=0;
  for (i=0;i<possc;i++) {
    if (cards[possi[i]]==(fb<<3|AS)) as=i+1;
    if (cards[possi[i]]==(fb<<3|ZEHN)) return 0;
  }
  if (!as || rnd(1)) return 0;
  possi[as-1]=possi[--possc];
  j=k=0;
  for (i=1;i<possc;i++) {
    if (cards[possi[i]]<cards[possi[j]]) j=i;
    if (cards[possi[i]]>cards[possi[k]]) k=i;
  }
  hi=f?higher(stcd[0],stcd[1])^spieler==ausspl:cards[possi[j]]<stcd[0];
  playcd=hi?j:k;
  return 1;
}

m_bvsp()
{
  if (!trumpfausspielen()) schenken();
}

m_bmsp()
{
  if (!uebernehmen(0,0)) schenken();
}

m_bhsp()
{
  if (!uebernehmen(!higher(stcd[0],stcd[1]),1)) schenken();
}

m_bvns()
{
  if (!hochausspielen()) abwerfen(1);
}

m_bmns()
{
  if (spieler==ausspl) {
    if (schnippeln(0) || uebernehmen(0,1)) return;
  }
  else {
    if (einstechen() || hatas()) return;
  }
  if (buttern()) schmieren();
  else abwerfen(0);
}

m_bhns()
{
  if (schnippeln(1)) return;
  if (higher(stcd[0],stcd[1])^spieler!=ausspl) {
    if (!uebernehmen(spieler!=ausspl,1)) abwerfen(0);
  }
  else {
    schmieren();
  }
}

m_bsp()
{
  playcd=0;
  if (!vmh) m_bvsp();
  else if (vmh==1) m_bmsp();
  else m_bhsp();
}

m_bns()
{
  playcd=0;
  if (!vmh) m_bvns();
  else if (vmh==1) m_bmns();
  else m_bhns();
}

int kleiner(i,j)
int i,j;
{
  int w1,w2;

  w1=cards[possi[i]]&7;
  w2=cards[possi[j]]&7;
  if (w1==ZEHN) return w2<=BUBE;
  if (w2==ZEHN) return w1>BUBE;
  return w1>w2;
}

int hat(i)
int i;
{
  return !hatnfb[spieler][cards[possi[i]]>>3];
}

int minmax(f)
int f;
{
  int i,j,hi,hj;

  j=0;
  for (i=1;i<possc;i++) {
    hi=hat(i);
    hj=hat(j);
    if ((hi && !hj) || (kleiner(i,j)^f && (hi || !hj))) j=i;
  }
  return j;
}

int drunter(f)
int f;
{
  int i,j;

  j=0;
  for (i=1;i<possc;i++) {
    if (higher(cards[possi[j]],cards[possi[i]])) j=i;
  }
  for (i=0;i<possc;i++) {
    if (higher(stcd[f],cards[possi[i]]) &&
      higher(cards[possi[i]],cards[possi[j]])) j=i;
  }
  return j;
}

m_nsp()
{
  if (!vmh) playcd=minmax(0);
  else if (hatnfb[spieler][stcd[0]>>3]) playcd=minmax(1);
  else if (vmh==2) playcd=drunter(!higher(stcd[0],stcd[1]));
  else playcd=drunter(0);
}

m_nns(s)
int s;
{
  if (!vmh) playcd=minmax(0);
  else {
    if (hatnfb[s][stcd[0]>>3] ||
	(vmh==2 && (higher(stcd[0],stcd[1])^spieler==ausspl))) {
      playcd=minmax(1);
    }
    else playcd=minmax(0);
  }
}

make_best(s)
int s;
{
  if (possc==1) playcd=0;
  else if (trumpf>=0) {
    if (s==spieler) m_bsp();
    else m_bns();
  }
  else {
    if (s==spieler) m_nsp();
    else m_nns(s);
  }
  drop_card(possi[playcd],s);
}

adjfb(s,v)
int s,v;
{
  int i,c;
  int fb[5];

  fb[0]=fb[1]=fb[2]=fb[3]=fb[4]=0;
  for (i=0;i<10;i++) {
    if ((c=cards[10*s+i])>=0) {
      if (trumpf!=-1 && (c&7)==BUBE) fb[trumpf]=1;
      else fb[c>>3]=1;
    }
  }
  for (i=0;i<5;i++) {
    if (!fb[i]) {
      hatnfb[s][i]=v;
    }
  }
}

do_spielen()
{
  int s;

  while (phase==SPIELEN && iscomp(s=(ausspl+vmh)%3)) {
    if (s==spieler) {
      adjfb((spieler+1)%3,2);
      adjfb((spieler+2)%3,2);
    }
    if (ouveang) {
      adjfb(spieler,1);
    }
    calc_poss(s);
    make_best(s);
    do_next();
  }
}

computer()
{
  if (quit) return;
  if (phase==GEBEN) do_geben();
  if (phase==REIZEN) do_reizen();
  if (phase==SPIELEN) do_spielen();
}

play()
{
  phase=GEBEN;
  do {
    computer();
    computer();
    hndl_events();
  } while (!quit);
}

main(argc,argv)
int argc;
char *argv[];
{
  setrnd(&seed[0],savseed=time((time_t *)0));
  xinit(argc,argv);
  read_list();
  play();
  return 0;
}
