/* GravityWars 1.1,  (C) Sami Niemi -95 */

#include "memory.h"
 
/*---------------------------------------------------------- drawSplitScreen */
void drawSplitScreen() {
  
  static long adr;
  static int mask,xx,yy;
  static char *dp;

  vga_setpage(0);
  mask=adr=0;

  for (yy=0; yy<=15; yy++) {
    for (xx=0; xx<=639; xx++) {
      if (adr>65535) {
	adr=0;
	mask++;
	vga_setpage(mask);
      }
      *(vga_ptr+adr++)=0;
    }
  }
  
}


/*--------------------------------------------------------------- drawScreen */
int drawScreen() {

  static short x,y,xx,yy,blk;
  static char *address;
  static char *gfx;
  static long ptr;

/*---- ScreenShot-----
  static long adr;
  static int mask;
---------------------*/
 
  
  for(yy=y=0; y<=44; y++,yy+=32) {
    for(xx=x=0; x<=19; x++,xx+=32) {
      putbox(xx,yy, block[level[x+y*20]]);
    }
  }

  drawSplitScreen();

  wipe.active=255; /* Not Active */

/* This routine writes the screen to stdout to produce
/* a screen shot.. */
/* SCREEN SHOT
  vga_setpage(0);
  mask=adr=0;

  putchar('*');
  for (yy=0; yy<=1439; yy++) {
    for (xx=0; xx<=639; xx++) {
      if (adr>65535) {
        adr=0;
        mask++;
        vga_setpage(mask);
      }
      putchar(*(vga_ptr+adr++));
    }
  }
*/

}




