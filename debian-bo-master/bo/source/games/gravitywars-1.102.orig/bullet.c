/* GravityWars 1.1,  (C) Sami Niemi -95 */

#include "memory.h"
 
/*-----------------------------------------------------------------setbullet */
void setbullet(int x, int y,int num) {

  static long adr;
  static short page,xx,yy,n;
  static char *adr2;
  static char *bullet;

  bullet=bulletback[num];

  adr=(y<<9)+(y<<7)+x;
  page=adr >> 16;
  vga_setpage(page);


  n=0;
  for(yy=0; yy<=2; yy++) {
    for(xx=0; xx<=2; xx++) {

      if ( (adr >> 16) != page) {
	page++;
	vga_setpage(page);
      }
      adr2=vga_ptr+(adr&65535);
      bullet[n]=*(adr2);
      *(adr2)=bulletgfx[n];
      n++;
      adr++;
    }
    adr+=637;
  }
}

/*----------------------------------------------------------------killbullet */
void killbullet(int x, int y,int num) {

  static long adr;
  static short page,xx,yy,n;
  static uchar *adr2;

  static char *bullet;

  bullet=bulletback[num];
 
  adr=(y<<9)+(y<<7)+x;
  page=adr >> 16;
  vga_setpage(page);


  n=0;
  for(yy=0; yy<=2; yy++) {
    for(xx=0; xx<=2; xx++) {

      if ( (adr >> 16) != page) {
	page++;
	vga_setpage(page);
      }
      *(vga_ptr+(adr&65535))=bullet[n];
      n++;
      adr++;
    }
    adr+=637;
  }
}

 

