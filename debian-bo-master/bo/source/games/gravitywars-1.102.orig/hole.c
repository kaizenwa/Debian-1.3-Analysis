/* GravityWars 1.1,  (C) Sami Niemi -95 */

#include "memory.h"

/*-----------------------------------------------------------------setbullet */
/* I think this one made holes just in the screen. Not in the virtual
/* BlockMap.. I.e. They dissappear when flown over.. */

void OLDmakehole(int x, int y, int type) {

  static long  adr;
  static short page,xx,yy,n;
  static uchar *adr2;
  static uchar *holemask;
  static uchar *gfx;
  static uchar col;

  x-=7;
  y-=7;

  xx=x&31;
  yy=y&31;
  gfx=&block[0][(yy<<5)+xx];


  holemask=hole[type];

  adr=(y<<9)+(y<<7)+x;
  page=adr >> 16;
  vga_setpage(page);

  for(yy=0; yy<=15; yy++) {
    for(xx=0; xx<=15; xx++) {

      if ( (adr >> 16) != page) {
	page++;
	vga_setpage(page);
      }

      if (*(holemask)) { 
	col=*(gfx);
	*(vga_ptr+(adr&65535))=col;
      }
      
      gfx++;  
      adr++;
      holemask++;
    }
    adr+=624;
    gfx+=16;
  }
}

/*-----------------------------------------------------------------setbullet */
void makehole(int x, int y, int type) {

  static short xx,yy,s,d;
  static uchar *holemask;
  static uchar *gfx;

  x-=15;
  y-=15;

  gfx=&backgnd[((y&31)<<6)+(x&31)];

  holemask=hole[type];

  getbox(x,y,tmpmix);
  
  s=d=0;
  for(yy=0; yy<=31; yy++) {
    for(xx=0; xx<=31; xx++) {
      tmpmix[d]=holemask[d] ? gfx[s] : tmpmix[d];
      s++;
      d++;
    }
    s+=32;
  }

  changeblocks(x,y,tmpmix);

}



