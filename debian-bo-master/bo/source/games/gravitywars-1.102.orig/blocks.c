/* GravityWars 1.1,  (C) Sami Niemi -95 */

#include "memory.h"
 
/*-------------------------------------------------------------------putbox */
/* Put a 32*32 linear box into an unlinear 65536 block address space        */
/*--------------------------------------------------------------------------*/
void putbox(int x, int y, uchar *dp) {  

  static long adr;
  static short xx,yy;
  static uchar len,mask; 
  static uchar *address;

  adr=(y<<9)+(y<<7)+x;
  mask=adr >> 16;
  adr=adr & 65535;
 
  vga_setpage(mask);
 
  if (adr < 45024) {

    /* NORMAL */
    address=vga_ptr+adr;
    for (yy=0; yy<=31; yy++) {
      for (xx=0; xx<=31; xx++) {
        *(address++)=*(dp++);
      }
      address+=608;
    }

  }
  else {

   /* BORDER */
    for (yy=0; yy<=31; yy++) {

      if (adr>65504) { /* Approaching */
        if (adr<=65535) { /* SplitLine */

          len=(65536-adr);
	  
          for(xx=len; xx>=1; xx--) {
            *(vga_ptr+adr++)=*(dp++);
          }

          mask++;
          vga_setpage(mask);
          adr=0;

          for(xx=(32-len); xx>=1; xx--) {
            *(vga_ptr+adr++)=*(dp++);
          }
        }
        else {
          mask++;
          vga_setpage(mask);
          adr=adr & 65535;
          for (xx=0; xx<=31; xx++) {
            *(vga_ptr+adr++)=*(dp++);
          }
        }
      }
      else {
        for (xx=0; xx<=31; xx++) {
          *(vga_ptr+adr++)=*(dp++);
        }
      }
      adr+=608;
    }
  }
  vga_setpage(0);
}



/*--------------------------------------------------------------------------*/
/* Get a 32*32 linear box from the blockmap/blockgfx address space          */
/*--------------------------------------------------------------------------*/
void getbox(int orx, int ory, uchar *out) {

  static int rx,ry,cx,cy,ix,iy,ox,oy,n;

  static char *from;
  static char *to;
  static int  num;
  static int  mod;

  rx=orx;
  ry=ory;

  ox=rx&31;
  oy=ry&31;

  /* Left Up */
  n=(ry>>5);
  num=(rx>>5)+(n<<4)+(n<<2);

  ix=ox;
  iy=oy;
  from=block[level[num]]+ix+(iy<<5);
  to=&out[0];
  for(cy=iy; cy<=31; cy++) {
    for(cx=ix; cx<=31; cx++) {
      *(to++)=*(from++);
    }
    to+=ix;
    from+=ix;
  }
  
  /* Right Up */
  if (ox>0) {
    rx=orx+31;
    ix=31-rx&31;
    iy=oy;
    from=block[level[num+1]]+(iy<<5);
    to=&out[ix];
    for(cy=iy; cy<=31; cy++) {
      for(cx=ix; cx<=31; cx++) {
	*(to++)=*(from++);
      }
      to+=ix;
      from+=ix;
    }
    
    
    
    
    /* Right Down */
    if (oy>0) {
      rx=orx+31;
      ry=ory+31;
      iy=ry&31;
      ix=31-rx&31;
      from=block[level[num+21]];
      to=&out[ix+((31-iy)<<5)];
      for(cy=0; cy<=iy; cy++) {
	for(cx=ix; cx<=31; cx++)
	  *(to++)=*(from++);
	to+=ix;
	from+=ix;
      }
    }
  }
  
  
  /* Left Down */
  if (oy>0) {
    ry=ory+31;
    rx=ox;
    ix=rx&31;
    iy=ry&31;
    from=block[level[num+20]]+ix;
    to=&out[((31-iy)<<5)];
    for(cy=0; cy<=iy; cy++) {
      for(cx=0; cx<=(31-ix); cx++)
	*(to++)=*(from++);
      to+=ix;
      from+=ix;
    }
  } 
}
/*--------------------------------------------------------------------------*/
/* Put a 32*32 linear box to the blockmap/blockgfx address space            */
/*--------------------------------------------------------------------------*/
void changeblocks(int orx, int ory, uchar *out) {

  static int rx,ry,cx,cy,ix,iy,ox,oy,n;

  static char *from;
  static char *to;
  static int  num;
  static int  mod;

  rx=orx;
  ry=ory;

  ox=rx&31;
  oy=ry&31;

  /* Left Up */
  n=(ry>>5);
  num=(rx>>5)+(n<<4)+(n<<2);

  if (objects[num]==L_RED_DOOR)  {
    ix=ox;
    iy=oy;
    from=block[level[num]]+ix+(iy<<5);
    to=&out[0];
    for(cy=iy; cy<=31; cy++) {
      for(cx=ix; cx<=31; cx++) {
	*(from++)=*(to++);
      }
      to+=ix;
      from+=ix;
    }
  }

  /* Right Up */
  if (ox>0) {
    if (objects[num+1]==L_RED_DOOR) {
      rx=orx+31;
      ix=31-rx&31;
      iy=oy;
      from=block[level[num+1]]+(iy<<5);
      to=&out[ix];
      for(cy=iy; cy<=31; cy++) {
	for(cx=ix; cx<=31; cx++) {
	  *(from++)=*(to++);
	}
	to+=ix;
	from+=ix;
      }
    }
    
    
    
    
    /* Right Down */
    if ( (oy>0) && (objects[num+21]==L_RED_DOOR) ) {
      rx=orx+31;
      ry=ory+31;
      iy=ry&31;
      ix=31-rx&31;
      from=block[level[num+21]];
      to=&out[ix+((31-iy)<<5)];
      for(cy=0; cy<=iy; cy++) {
	for(cx=ix; cx<=31; cx++)
	  *(from++)=*(to++);
	to+=ix;
	from+=ix;
      }
    }
  }
  
  
  /* Left Down */
  if ( (oy>0) && (objects[num+20]==L_RED_DOOR) ) {
    ry=ory+31;
    rx=ox;
    ix=rx&31;
    iy=ry&31;
    from=block[level[num+20]]+ix;
    to=&out[((31-iy)<<5)];
    for(cy=0; cy<=iy; cy++) {
      for(cx=0; cx<=(31-ix); cx++)
	*(from++)=*(to++);
      to+=ix;
      from+=ix;
    }
  } 
}

#ifdef GETBOX
/*--------------------------------------------------------------------------*/
/* Get a 32*32 linear box from an unlinear 65536 block address space        */
/*--------------------------------------------------------------------------*/
void getbox2(int x, int y, uchar *dp) {

  static long  adr;
  static short xx,yy;
  static uchar len,mask;

  adr=(y<<9)+(y<<7)+x;
  mask=adr >> 16;
  adr=adr & 65535;
 
  vga_setpage(mask);
 
  if (adr < 45024) {

    /* NORMAL */
    for (yy=0; yy<=31; yy++) {
      for (xx=0; xx<=31; xx++) {
        *(dp++)=*(vga_ptr+adr++);
      }
      adr+=608;
    }

  }
  else {

    /* BORDER */
    for (yy=0; yy<=31; yy++) {


      if (adr>65504) { /* Approaching */
        if (adr<=65535) { /* SplitLine */

          len=(65536-adr);

          for(xx=len; xx>=1; xx--) {
            *(dp++)=*(vga_ptr+adr++);
          }

          mask++;
	  vga_setpage(mask);
          adr=0;

          for(xx=(32-len); xx>=1; xx--) {
            *(dp++)=*(vga_ptr+adr++);
	  }
        }
        else {
          mask++;
	  vga_setpage(mask);
          adr=adr & 65535;
          for (xx=0; xx<=31; xx++) {
            *(dp++)=*(vga_ptr+adr++);
          }
        }
      }
      else {
        for (xx=0; xx<=31; xx++) {
          *(dp++)=*(vga_ptr+adr++);
        }
      }
      adr+=608;
    }
  }
  vga_setpage(0);
} 
#endif

/* Put a combination of 3 blocks to x,y coordinates (a stamp) */
void putstamp(short x, short y, short a, short b, short c) {

  static uchar box[1024];
  static short n;
  static char *adr;  

  adr=block[a];
  getbox(x,y,box);
  for(n=0; n<=1023; n++)
    box[n]=adr[n] ? adr[n] : box[n];
  putbox(x,y,box);

  x+=32;
  adr=block[b];
  getbox(x,y,box);
  for(n=0; n<=1023; n++)
    box[n]=adr[n] ? adr[n] : box[n];
  putbox(x,y,box);

  x+=32;
  adr=block[c];
  getbox(x,y,box);
  for(n=0; n<=1023; n++)
    box[n]=adr[n] ? adr[n] : box[n];
  putbox(x,y,box);
}


void killstamp(short x, short y) {

  static uchar box[1024];

  getbox(x,y,box);
  putbox(x,y,box);

  x+=32;
  getbox(x,y,box);
  putbox(x,y,box);

  x+=32;
  getbox(x,y,box);
  putbox(x,y,box);
}


/* Write a Digit into one of the stamps w/ the digit info */
void stampnum(short num) {

  static short bcd,a,b;
  static short xx,yy;
  static char *to;
  static char *from;
 

  bcd=Dec2BCD[num];
  a=bcd>>4;
  b=bcd&15;
  

  to=&block[69][24];
  from=&numbers[a<<3];
  for(yy=0; yy<=10; yy++) {
    for(xx=0; xx<=7; xx++) {
      *(to++)=*(from++);
    }
    to+=24;
    from+=72;
  }

  to=&block[70][1];
  from=&numbers[b<<3];
  for(yy=0; yy<=10; yy++) {
    for(xx=0; xx<=7; xx++) {
      *(to++)=*(from++);
    }
    to+=24;
    from+=72;
  }


}


