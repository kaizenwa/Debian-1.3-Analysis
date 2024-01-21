
/* Written by Peter Ekberg, peda@lysator.liu.se */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <stdio.h>
#include <math.h>

#include "thrust_types.h"
#include "graphics.h"
#include "things.h"
#include "fast_gr.h"
#include "gr_drv.h"
#include "thrust.h"

void
writeblock(word bx, word by, byte block)
{
  word tempx, tempy;

  *(bana+bx+by*lenx)=block;
  tempx=bx;
  tempy=by;
  if(pblockx+BBILDX>lenx && tempx<BBILDX)
    tempx+=lenx;
  if(pblocky+BBILDY>leny && tempy<BBILDY)
    tempy+=leny;
  if(insideblock(tempx, tempy, pblockx, pblocky, 0, 0))
    putblock(bblockx-pblockx+tempx, tempy%BBILDY,
	     blocks+(block<<6));
}

#define NR_TP (6)
#define SZ_TP (3)
#define DIFF_TP (8)

void
drawteleline(int round, int x1, int y1, int x2, int y2, int j, int k)
{
  int l;
  static unsigned char telemem[2*5*4*NR_TP*SZ_TP];
  static unsigned char *tm;

  switch(round) {
  case 0:
    tm=&telemem[0];
    break;
  case 1:
    for(l=-2; l<=2; l++) {
      putpixel(x1+(j+2)*DIFF_TP+k, y1+l, 65);
      putpixel(x1-(j+2)*DIFF_TP-k, y1+l, 65);
      putpixel(x1+l, y1+(j+2)*DIFF_TP+k, 65);
      putpixel(x1+l, y1-(j+2)*DIFF_TP-k, 65);
      if(loaded) {
	putpixel(x2+(j+2)*DIFF_TP+k, y2+l, 65);
	putpixel(x2-(j+2)*DIFF_TP-k, y2+l, 65);
	putpixel(x2+l, y2+(j+2)*DIFF_TP+k, 65);
	putpixel(x2+l, y2-(j+2)*DIFF_TP-k, 65);
      }
    }
    break;
  case 2:
    for(l=-2; l<=2; l++) {
      *(tm++) = getpixel(x1+(j+2)*DIFF_TP+k, y1+l);
      *(tm++) = getpixel(x1-(j+2)*DIFF_TP-k, y1+l);
      *(tm++) = getpixel(x1+l, y1+(j+2)*DIFF_TP+k);
      *(tm++) = getpixel(x1+l, y1-(j+2)*DIFF_TP-k);
      if(loaded) {
	*(tm++) = getpixel(x2+(j+2)*DIFF_TP+k, y2+l);
	*(tm++) = getpixel(x2-(j+2)*DIFF_TP-k, y2+l);
	*(tm++) = getpixel(x2+l, y2+(j+2)*DIFF_TP+k);
	*(tm++) = getpixel(x2+l, y2-(j+2)*DIFF_TP-k);
      }
    }
    break;
  case 3:
    for(l=-2; l<=2; l++) {
      putpixel(x1+(j+2)*DIFF_TP+k, y1+l, *(tm++));
      putpixel(x1-(j+2)*DIFF_TP-k, y1+l, *(tm++));
      putpixel(x1+l, y1+(j+2)*DIFF_TP+k, *(tm++));
      putpixel(x1+l, y1-(j+2)*DIFF_TP-k, *(tm++));
      if(loaded) {
	putpixel(x2+(j+2)*DIFF_TP+k, y2+l, *(tm++));
	putpixel(x2-(j+2)*DIFF_TP-k, y2+l, *(tm++));
	putpixel(x2+l, y2+(j+2)*DIFF_TP+k, *(tm++));
	putpixel(x2+l, y2-(j+2)*DIFF_TP-k, *(tm++));
      }
    }
    break;
  }
}

void
drawteleport(void)
{
  int i, j, k;
  int x1, y1, x2, y2;

  x1=x2=154+7+shipdx;
  y1=y2= 82+7+shipdy;
  if(loaded) {
    x2=161-(int)((252-loadpoint)*cos(alpha)/7.875);
    y2= 89+(int)((252-loadpoint)*sin(alpha)/7.875);
  }

  syncscreen();
  for(i=0; i<NR_TP+SZ_TP-1; i++) {
    for(k=min(SZ_TP-1, i), j=max(i-(SZ_TP-1), 0); j<=min(i, NR_TP-1); k--, j++)
      drawteleline(1, x1, y1, x2, y2, j, k);
    usleep(50000L);
    displayscreen();
  }

  usleep(250000UL);
  syncscreen();
  putscr(bildx, bildy);

  drawteleline(0, 0, 0, 0, 0, 0, 0);
  for(i=0; i<NR_TP+SZ_TP-1; i++)
    for(k=min(SZ_TP-1, i), j=max(i-(SZ_TP-1), 0); j<=min(i, NR_TP-1); k--, j++)
      drawteleline(2, x1, y1, x2, y2, j, k);
  drawteleline(0, 0, 0, 0, 0, 0, 0);
  for(i=0; i<NR_TP+SZ_TP-1; i++)
    for(k=min(SZ_TP-1, i), j=max(i-(SZ_TP-1), 0); j<=min(i, NR_TP-1); k--, j++)
      drawteleline(1, x1, y1, x2, y2, j, k);
  displayscreen();
  
  usleep(250000UL);
  
  syncscreen();
  drawteleline(0, 0, 0, 0, 0, 0, 0);
  for(i=0; i<NR_TP+SZ_TP-1; i++) {
    for(k=min(SZ_TP-1, i), j=max(i-(SZ_TP-1), 0); j<=min(i, NR_TP-1); k--, j++)
      drawteleline(3, x1, y1, x2, y2, j, k);
    usleep(50000L);
    displayscreen();
  }
}

void
swap(int *pa, int *pb)
{
  int	t;
  t=*pa; *pa=*pb; *pb=t;
}

void
drawlinev(int x1, int y1, int x2, int y2, byte color, byte *storage)
{
  int d, dx, dy;
  int Ai, Bi, xi;
  byte *ptr=bild;

  if(y1>y2) {
    swap(&x1, &x2);
    swap(&y1, &y2);
  }
  xi=(x2>x1)?1:-1;
  dx=abs(x2-x1);
  dy=(y2-y1);
  Ai=(dx-dy)<<1;
  Bi=(dx<<1);
  d=Bi-dy;

  ptr+=y1*(PBILDX<<1);
  *(storage++)=*(ptr+x1);
  *(ptr+x1)=color;
  for(y1++, ptr+=PBILDX<<1; y1<=y2; y1++, ptr+=PBILDX<<1) {
    if(y1==PBILDY)
      ptr=bild;
    if(d<0)
      d+=Bi;
    else {
      x1+=xi;
      d+=Ai;
    }
    *(storage++)=*(ptr+x1);
    *(ptr+x1)=color;
  }
}

void
undrawlinev(int x1, int y1, int x2, int y2, byte *storage)
{
  int d, dx, dy;
  int Ai, Bi, xi;
  byte *ptr=bild;

  if(y1>y2) {
    swap(&x1, &x2);
    swap(&y1, &y2);
  }
  xi=(x2>x1)?1:-1;
  dx=abs(x2-x1);
  dy=(y2-y1);
  Ai=(dx-dy)<<1;
  Bi=(dx<<1);
  d=Bi-dy;

  ptr+=y1*(PBILDX<<1);
  *(ptr+x1)=*(storage++);
  for(y1++, ptr+=PBILDX<<1; y1<=y2; y1++, ptr+=PBILDX<<1) {
    if(y1==PBILDY)
      ptr=bild;
    if(d<0)
      d+=Bi;
    else {
      x1+=xi;
      d+=Ai;
    }
    *(ptr+x1)=*(storage++);
  }
}

void
drawlineh(int x1, int y1, int x2, int y2, byte color, byte *storage)
{
  int d, dx, dy;
  int Ai, Bi, yi, i;
  byte *ptr=bild;

  if(x1>x2) {
    swap(&x1, &x2);
    swap(&y1, &y2);
  }
  if(y2>y1) {
    yi=PBILDX<<1;
    i=1;
  }
  else {
    yi=-PBILDX<<1;
    i=-1;
  }
  dx=x2-x1;
  dy=abs(y2-y1);
  Ai=(dy-dx)<<1;
  Bi=(dy<<1);
  d=Bi-dx;
  if(y1>=PBILDY)
    y1-=PBILDY;
  y2=y1;
  y1=y1*PBILDX<<1;

  ptr+=y1;
  *(storage++)=*(ptr+x1);
  *(ptr+x1)=color;
  for(x1++; x1<=x2; x1++) {
    if(d<0)
      d+=Bi;
    else {
      ptr+=yi;
      y2+=i;
      if(y2==-1)
	ptr+=PBILDY*PBILDX<<1;
      if(y2==PBILDY)
	ptr=bild;
      d+=Ai;
    }
    *(storage++)=*(ptr+x1);
    *(ptr+x1)=color;
  }
}

void
undrawlineh(int x1, int y1, int x2, int y2, byte *storage)
{
  int d, dx, dy;
  int Ai, Bi, yi, i;
  byte *ptr=bild;

  if(x1>x2) {
    swap(&x1, &x2);
    swap(&y1, &y2);
  }
  if(y2>y1) {
    yi=PBILDX<<1;
    i=1;
  }
  else {
    yi=-PBILDX<<1;
    i=-1;
  }
  dx=x2-x1;
  dy=abs(y2-y1);
  Ai=(dy-dx)<<1;
  Bi=(dy<<1);
  d=Bi-dx;
  if(y1>=PBILDY)
    y1-=PBILDY;
  y2=y1;
  y1=y1*PBILDX<<1;

  ptr+=y1;
  *(ptr+x1)=*(storage++);
  for(x1++; x1<=x2; x1++) {
    if(d<0)
      d+=Bi;
    else {
      ptr+=yi;
      y2+=i;
      if(y2==-1)
	ptr+=PBILDY*PBILDX<<1;
      if(y2==PBILDY)
	ptr=bild;
      d+=Ai;
    }
    *(ptr+x1)=*(storage++);
  }
}

void
drawline(int x1, int y1, int x2, int y2, byte color, byte *storage)
{
  if(y1>y2+64)
    y2+=PBILDY;
  if(y2>y1+64)
    y1+=PBILDY;
  if(abs(x1-x2)<abs(y1-y2))
    drawlinev(x1, y1, x2, y2, color, storage);
  else
    drawlineh(x1, y1, x2, y2, color, storage);
}

void
undrawline(int x1, int y1, int x2, int y2, byte *storage)
{
  if(y1>y2+64)
    y2+=PBILDY;
  if(y2>y1+64)
    y1+=PBILDY;
  if(abs(x1-x2)<abs(y1-y2))
    undrawlinev(x1, y1, x2, y2, storage);
  else
    undrawlineh(x1, y1, x2, y2, storage);
}

void
drawbullets(void)
{
  int l;
  bullet *bulletptr;
  word tempx, tempy;
  byte target;

  for(l=0, bulletptr=bullets; l<maxbullets; l++, bulletptr++)
    if((*bulletptr).life) {
      tempx=(*bulletptr).x>>3;
      tempy=(*bulletptr).y>>3;
      if(pixx+PUSEX>lenx3 && tempx<PUSEX)
	tempx+=lenx3;
      if(pixy+PUSEY>leny3 && tempy<PUSEY)
	tempy+=leny3;
      if(insidepixel(tempx, tempy, pixx, pixy, 4, 4))
	drawsquare(bildx+tempx-pixx, tempy%PBILDY,
		   bulletmap+((*bulletptr).dir<<4),
		   bulletstorage+(l<<4), 4, 4);
      else {
	target = *(bana+(tempx>>3)%lenx+((tempy>>3)%leny)*lenx);
	if(target!=32) {
	  /* Add code to take care of offscreen hits */
	  if((*bulletptr).owner)
	    switch(target) {
	    case '`':
	    case 'a':
	    case 'b':
	    case 'c':
	    case 'd':
	    case 'e':
	    case 'f':
	    case 'g':
	    case 'h':
	    case 'i':
	    case 'j':
	    case 'k':
	    case 'l':
	    case 'L':
	    case 'M':
	    case 'N':
	    case 'O':
	    case 'P':
	    case 'Q':
	    case 'R':
	    case 'S':
	    case 'T':
	    case 'U':
	    case 'V':
	    case 'W':
	    case 'X':
	    case 'Y':
	    case 'Z':
	    case ']':
	    case '\\':
	    case '^':
	    case '_':
	      hit((tempx+3)%lenx3, (tempy+3)%leny3, 4, (*bulletptr).owner);
	    }
	  (*bulletptr).life=0;
	}
      }
    }
}

void
undrawbullets(void)
{
  int l;
  bullet *bulletptr;
  word tempx, tempy;
  word crash;

  for(l=maxbullets-1, bulletptr=bullets+maxbullets-1; l>=0; l--, bulletptr--)
    if((*bulletptr).life) {
      tempx=(*bulletptr).x>>3;
      tempy=(*bulletptr).y>>3;
      if(pixx+PUSEX>lenx3 && tempx<PUSEX)
	tempx+=lenx3;
      if(pixy+PUSEY>leny3 && tempy<PUSEY)
	tempy+=leny3;
      if(insidepixel(tempx, tempy, pixx, pixy, 4, 4)) {
	crash=testcrash(bulletmap+((*bulletptr).dir<<4),
			bulletstorage+(l<<4), 16, 0);
	if(crash) {
	  if(crash>=4)
	    hit((tempx+3)%lenx3, (tempy+3)%leny3, crash, (*bulletptr).owner);
	  (*bulletptr).life=0;
	}
	undrawsquare(bildx+tempx-pixx, tempy%PBILDY,
		     bulletstorage+(l<<4), 4, 4);
      }
    }
}

void
drawfragments(void)
{
  int l;
  fragment *fragmentptr;
  word tempx, tempy;
  static byte fragmentmap[4]={ 12, 12, 12, 12 };

  for(l=0, fragmentptr=fragments; l<maxfragments; l++, fragmentptr++)
    if((*fragmentptr).life) {
      tempx=(*fragmentptr).x>>3;
      tempy=(*fragmentptr).y>>3;
      if(pixx+PUSEX>lenx3 && tempx<PUSEX)
	tempx+=lenx3;
      if(pixy+PUSEY>leny3 && tempy<PUSEY)
	tempy+=leny3;
      if(insidepixel(tempx, tempy, pixx, pixy, 2, 2))
	drawsquare(bildx+tempx-pixx, tempy%PBILDY,
		   fragmentmap, fragmentstorage+(l<<2), 2, 2);
      else if(*(bana+(tempx>>3)%lenx+((tempy>>3)%leny)*lenx)!=32)
	(*fragmentptr).life=0;
    }
}

void
undrawfragments(void)
{
  int l;
  fragment *fragmentptr;
  word tempx, tempy;
  word crash;
  static byte fragmentmap[4]={ 12, 12, 12, 12 };

  for(l=maxfragments-1, fragmentptr=fragments+maxfragments-1;
      l>=0;
      l--, fragmentptr--)
    if((*fragmentptr).life) {
      tempx=(*fragmentptr).x>>3;
      tempy=(*fragmentptr).y>>3;
      if(pixx+PUSEX>lenx3 && tempx<PUSEX)
	tempx+=lenx3;
      if(pixy+PUSEY>leny3 && tempy<PUSEY)
	tempy+=leny3;
      if(insidepixel(tempx, tempy, pixx, pixy, 2, 2)) {
	crash=testcrash(fragmentmap, fragmentstorage+(l<<2), 4, 0);
	if(crash) {
/*	  if(crash>=4)
	    hit((tempx+3)%lenx3, (tempy+3)%leny3, crash); */
	  (*fragmentptr).life=0;
	}
	undrawsquare(bildx+tempx-pixx, tempy%PBILDY,
		     fragmentstorage+(l<<2), 2, 2);
      }
    }
}

void
drawspacestationblip(void)
{
  word tempx, tempy;

  tempx=ssx;
  tempy=ssy;
  if(pblockx+BBILDX>lenx && tempx<BBILDX)
    tempx+=lenx;
  if(pblocky+BBILDY>leny && tempy<BBILDY)
    tempy+=leny;
  if(insideblock(tempx, tempy, pblockx, pblocky, 0, 0))
    putblock(bblockx-pblockx+tempx, tempy%BBILDY,
	     blocks+((ssblip?32:222-(scount&0xc))<<6));
}

void
drawload(int flag)
{
  word tempx, tempy;

  tempx=loadbx;
  tempy=loadby;
  if(pblockx+BBILDX>lenx && tempx<BBILDX)
    tempx+=lenx;
  if(pblocky+BBILDY>leny && tempy<BBILDY)
    tempy+=leny;
  putblock(bblockx-pblockx+tempx, tempy%BBILDY,
	   blocks+((flag?109:32)<<6));
}

word
drawshuttle(void)
{
  word crash=0, tmp;
#ifdef DEBUG2
  int debug2i;
#endif
  int x1, x2=0, y1, y2=0, lx, ly;
  static byte wiremap[64] = {
    13, 13, 13, 13, 13, 13, 13, 13,
    13, 13, 13, 13, 13, 13, 13, 13,
    13, 13, 13, 13, 13, 13, 13, 13,
    13, 13, 13, 13, 13, 13, 13, 13,
    13, 13, 13, 13, 13, 13, 13, 13,
    13, 13, 13, 13, 13, 13, 13, 13,
    13, 13, 13, 13, 13, 13, 13, 13,
    13, 13, 13, 13, 13, 13, 13, 13 };

  if(loaded || loadcontact) {
    x1=bildx+161+shipdx;
    y1=(bildy+89+shipdy);
    if(loaded) {
      x2=bildx+161-(int)((252-loadpoint)*cos(alpha)/7.875);
      y2=bildy+ 89+(int)((252-loadpoint)*sin(alpha)/7.875);
    }
    else {
      x2=(loadbx<<3)+3;
      if(abs(x2-x1)>PBILDX/2)
	x2+=PBILDX;
      y2=(loadby<<3)+3;
    }
    lx=abs(x1-x2)%PBILDX;
    ly=abs(y1-y2)%PBILDY;
    if(lx>64)
      lx=abs(lx-PBILDX);
    if(ly>64)
      ly=abs(ly-PBILDY);
    drawline(x1, y1%PBILDY, x2, y2%PBILDY, 11, wirestorage);
    tmp=testcrash(wiremap, wirestorage, max(lx, ly)+1, shield);
#ifdef DEBUG2
    if(tmp) {
      printf("Crash: Wire destroyed. By %d. Wirelength %d.\n",
	     tmp, max(lx, ly)+1);
      printf("Wirestorage:");
      for(debug2i=0; debug2i<max(lx, ly)+1; debug2i++)
	printf(" %02x", *(wirestorage+debug2i));
      printf("\n");
      printf("Killer line: x1=%d, y1=%d, x2=%d, y2=%d\n",
	     x1, y1%PBILDY, x2, y2%PBILDY);
      undrawline(x1+2 , y1%PBILDY, x2+2 , y2%PBILDY, wirestorage);
      usleep(10000000UL);
    }
#endif
    crash=max(crash, tmp);
  }
  /* Draw the shuttle */
  drawship(bildx+154+shipdx, (bildy+82+shipdy)%PBILDY,
	   (shield?shieldship:ship)+(dir<<8), shipstorage);
  tmp=testcrash(ship+(dir<<8), shipstorage, 256, shield);
#ifdef DEBUG2
  if(tmp)
    printf("Crash: Ship destroyed. By %d.\n", tmp);
#endif
  crash=max(tmp, crash);
  if(loaded || loadcontact) {
    if(loaded)
      drawsquare(x2-3, (y2-3)%PBILDY, loadmap, loadstorage, 8, 8);
    else if(loadcontact) {
      x1=loadbx<<3;
      y1=loadby<<3;
      if(pixx+PBILDX>lenx3 && x1<PBILDX)
	x1+=lenx3;
      if(pixy+PBILDY>leny3 && y1<PBILDY)
	y1+=leny3;
      drawsquare(bildx-pixx+x1, y1%PBILDY, loadmap, loadstorage, 8, 8);
    }
    tmp=testcrash(loadmap, loadstorage, 64, shield);
#ifdef DEBUG2
    if(tmp)
      printf("Crash: Load destroyed. By %d.\n", tmp);
#endif
    crash=max(crash, tmp);
  }
  return(crash);
}

void
undrawshuttle(void)
{
  int x1, x2=0, y1, y2=0;

  if(loaded || loadcontact)
    if(loaded) {
      x2=bildx+161-(int)((252-loadpoint)*cos(alpha)/7.875);
      y2=bildy+ 89+(int)((252-loadpoint)*sin(alpha)/7.875);
      undrawsquare(x2-3, (y2-3)%PBILDY, loadstorage, 8, 8);
    }
    else if(loadcontact) {
      x1=loadbx<<3;
      y1=loadby<<3;
      if(pixx+PBILDX>lenx3 && x1<PBILDX)
	x1+=lenx3;
      if(pixy+PBILDY>leny3 && y1<PBILDY)
	y1+=leny3;
      undrawsquare(bildx-pixx+x1, y1%PBILDY, loadstorage, 8, 8);
    }
  undrawship(bildx+154+shipdx, (bildy+82+shipdy)%PBILDY,
	     shipstorage);
  if(loaded || loadcontact) {
    x1=bildx+161+shipdx;
    y1=(bildy+89+shipdy);
    if(loadcontact) {
      x2=(loadbx<<3)+3;
      if(abs(x2-x1)>PBILDX/2)
	x2+=PBILDX;
      y2=(loadby<<3)+3;
    }
    undrawline(x1, y1%PBILDY, x2, y2%PBILDY, wirestorage);
  }
}

void
drawfuellines(void)
{
  drawsquare(bildx+shipdx+151, (bildy+shipdy+98)%PBILDY,
	     fuelmap, fuelstorage, 4, 32);
  drawsquare(bildx+shipdx+168, (bildy+shipdy+98)%PBILDY,
	     fuelmap+128, fuelstorage+128, 4, 32);
}

void
undrawfuellines(void)
{
  undrawsquare(bildx+shipdx+151, (bildy+shipdy+98)%PBILDY,
	       fuelstorage, 4, 32);
  undrawsquare(bildx+shipdx+168, (bildy+shipdy+98)%PBILDY,
	       fuelstorage+128, 4, 32);
}
