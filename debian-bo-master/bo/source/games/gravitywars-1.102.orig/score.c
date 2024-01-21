/* GravityWars 1.1,  (C) Sami Niemi -95 */

#include "memory.h"

/* Level code letters */
void putletter(short adr, short num) {   /* adr=x+(y<<8) */

  static short xx,yy;
  static char *to;
  static char *from;
  static uchar colr;

  to=&score[adr];
  from=&fonts[(num<<3)+num+num];

  for(yy=0; yy<=6; yy++) {
    for(xx=0; xx<=8; xx++) {
      colr=*(from++);
      if (colr!=0) colr+=48;
      *(to++)=colr;
    }
    to+=247;
    from+=251; /*253*/
  }
}

/* Score Digits */
void putdigit(short adr, short num) {   /* adr=x+(y<<8) */

  static short xx,yy;
  static char *to;
  static char *from;

  to=&score[adr];
  from=&digits[(num<<3)];

  for(yy=0; yy<=5; yy++) {
    for(xx=0; xx<=7; xx++) {
      *(to++)=*(from++);   
    }
    to+=248;
    from+=80;
  }
}

#define SCR_AB 55040
#define SCR_AP 65280
/*-----------------------------------------------------------------putscore */
void putscore(int nr, short y) {

#ifdef foo

  static long adr;
  static short xx,yy;
  static uchar len,mask,col; 
  static uchar *dp;
  static uchar *back;
  static uchar *adr3;
  
  adr=(y<<9)+(y<<7)+8;
  mask=adr >> 16;
  adr=adr & 65535;
  dp=score;
  back=scoreback;
 
  vga_setpage(mask);
 
  if (adr < SCR_AB) {

    /* NORMAL */
    for (yy=0; yy<=15; yy++) {
      for (xx=0; xx<=255; xx++) {
	adr3=vga_ptr+adr++;
	*(back++)=*(adr3);
/*        if ((col=*(dp++))!=0) */
	  *(adr3)=*(dp++);
      }
      adr+=384;
    }

  }
  else {
    /* BORDER */
    for (yy=0; yy<=15; yy++) {


      if (adr>SCR_AP) { /* Approaching */
	

	if (adr<=65535) { /* SplitLine */

          len=(65536-adr);

          for(xx=len; xx>=1; xx--) {
	    adr3=vga_ptr+adr++;
	    *(back++)=*(adr3);
/*	    if ((col=*(dp++))!=0)*/
	      *(adr3)=*(dp++); /*col*/

          }

          mask++;
	  vga_setpage(mask);
          adr=0;

          for(xx=(256-len); xx>=1; xx--) {
	    adr3=vga_ptr+adr++;
	    *(back++)=*(adr3);
/*	    if ((col=*(dp++))!=0)*/
	      *(adr3)=*(dp++); /*col*/
	  }
        }
        else {
          mask++;
	  vga_setpage(mask);
          adr=adr & 65535;
          for (xx=0; xx<=255; xx++) {
	    adr3=vga_ptr+adr++;
            *(back++)=*(adr3);
/*	    if ((col=*(dp++))!=0)*/
	      *(adr3)=*(dp++);/*col*/

          }
        }
      }
      else {
        for (xx=0; xx<=255; xx++) {
	  adr3=vga_ptr+adr++;
	  *(back++)=*(adr3);
/*	  if ((col=*(dp++))!=0) */
	    *(adr3)=*(dp++); /*col*/
        }
      }
      adr+=384;
    }
  }
#endif
} 

void putscoreOnly(int nr, short y) {  /* Don't save the background */
#ifndef foo
  static long adr;
  static short xx,yy;
  static uchar len,mask,col; 
  static uchar *dp;
  static uchar *adr3;
  
  adr=184; /*(y<<9)+(y<<7)+8;*/
  mask=adr >> 16;
  adr=adr & 65535;
  dp=score;
 
  vga_setpage(mask);
 
  if (adr < SCR_AB) {

    /* NORMAL */
    for (yy=0; yy<=15; yy++) {
      for (xx=0; xx<=255; xx++) {
	adr3=vga_ptr+adr++;
	*(adr3)=*(dp++);
      }
      adr+=384;
    }

  }
  else {
    /* BORDER */
    for (yy=0; yy<=15; yy++) {


      if (adr>SCR_AP) { /* Approaching */
	

	if (adr<=65535) { /* SplitLine */

          len=(65536-adr);

          for(xx=len; xx>=1; xx--) {
	    adr3=vga_ptr+adr++;
	    *(adr3)=*(dp++); /*col*/
	    
          }

          mask++;
	  vga_setpage(mask);
          adr=0;

          for(xx=(256-len); xx>=1; xx--) {
	    adr3=vga_ptr+adr++;
	    *(adr3)=*(dp++); /*col*/
	  }
        }
        else {
          mask++;
	  vga_setpage(mask);
          adr=adr & 65535;
          for (xx=0; xx<=255; xx++) {
	    adr3=vga_ptr+adr++;
	    *(adr3)=*(dp++);/*col*/

          }
        }
      }
      else {
        for (xx=0; xx<=255; xx++) {
	  adr3=vga_ptr+adr++;
	  *(adr3)=*(dp++); /*col*/
        }
      }
      adr+=384;
    }
  }
#endif
} 
void updatescore() {           /* Terrible Routine... (optimal speed though) */

  static short fuel;
  static short time;
  static short score;

  fuel=Dec2BCD[ShipFuel>>4];
  time=Dec2BCD[ShipTime>>7];
  score=Dec2BCD[ShipScore];

  putdigit(1322,fuel>>4);
  putdigit(1330,fuel&15);

  putdigit(1371,ShipLife);
  
  putdigit(1415,time>>4);
  putdigit(1423,time&15);
  
  putdigit(1482,score>>16);
  putdigit(1490,(score>>12)&15);
  putdigit(1498,(score>>8)&15);
  putdigit(1506,(score>>4)&15);
  putdigit(1514,score&15);

  putscoreOnly(0,0);

}




/*----------------------------------------------------------------killscore */
void killscore(int nr, short y, short newy) {
#ifdef foo
  static long adr;
  static short xx,yy,y1,y2;
  static uchar len,mask; 
  static uchar *dp;
  static uchar *back;
  
  adr=(y<<9)+(y<<7)+8;
  mask=adr >> 16;
  adr=adr & 65535;
  back=scoreback;

/*
  if (y<newy) {
    y1=y;
    y2=newy;
  }
  else {
    if (y>newy) {
      y1=newy+16;
      y2=y+16;
    }
    else
      goto skipkillscore;
  }
*/

  vga_setpage(mask);
 
  if (adr < SCR_AB) {

    /* NORMAL */
    for (yy=0; yy<=15; yy++) {
      for (xx=0; xx<=255; xx++) {
	*(vga_ptr+adr++)=*(back++);
      }
      adr+=384;
    }

  }
  else {
    /* BORDER */
    for (yy=0; yy<=15; yy++) {


      if (adr>SCR_AP) { /* Approaching */
	

	if (adr<=65535) { /* SplitLine */

          len=(65536-adr);

          for(xx=len; xx>=1; xx--) {
	    *(vga_ptr+adr++)=*(back++);
           }

          mask++;
	  vga_setpage(mask);
          adr=0;

          for(xx=(256-len); xx>=1; xx--) {
	    *(vga_ptr+adr++)=*(back++);
	  }
        }
        else {
          mask++;
	  vga_setpage(mask);
          adr=adr & 65535;
          for (xx=0; xx<=255; xx++) {
	    *(vga_ptr+adr++)=*(back++);
	  }
        }
      }
      else {
        for (xx=0; xx<=255; xx++) {
	  *(vga_ptr+adr++)=*(back++);
	}
      }
      adr+=384;
    }
  }
  skipkillscore:
#endif
} 

