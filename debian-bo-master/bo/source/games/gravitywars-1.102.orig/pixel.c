/* GravityWars 1.1,  (C) Sami Niemi -95 */

#include "memory.h"
 
/*-----------------------------------------------------------------setpixel */
void setpixel(int x, int y, uchar c) {

  static long adr;
 
  adr=(y<<9)+(y<<7)+x;
  vga_setpage(adr >> 16);
  *(vga_ptr+(adr&65535))=c;
}


/*-----------------------------------------------------------------setpixel */
uchar getpixel(int x, int y) {

  static long adr;

  adr=(y<<9)+(y<<7)+x;
  vga_setpage(adr >> 16);
  return *(vga_ptr+(adr&65535));
}
	
