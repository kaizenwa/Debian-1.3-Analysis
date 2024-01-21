/* GravityWars 1.1,  (C) Sami Niemi -95 */

#include "memory.h"


/*----------------------------------------------------------------watermask */
/* Raterize the ship when in the water, and only to a certain height.. */

void watermask(short x, short y, uchar type, char *back_adr) {

  static short xx,yy,n;
  static uchar *mix_adr;
  static uchar *ship_adr;
  static uchar col;
  static short m,start;

  y-=9;

  if (type==E_WATER)
    start=0;
  else {
    start=(y & 65504)+32-y; 
    if (start>25) start=0;
  }

  m=(x^y);

  mix_adr=shipmix;
  ship_adr=ship[1+(g_flag & 1)][sa[0] >> 5];

  n=0;
  for(yy=0; yy<=31; yy++) { 
    for(xx=0; xx<=31; xx++,n++) {
      col=ship_adr[n];
      if (col!=0) {
	if ( (((xx^yy)^m)&1) && (yy>start)) {
	  col=WATERCOLOR;
	}
      }
      else { 
	col=back_adr[n];
      }
      mix_adr[n]=col;
    }  
  }
}  


