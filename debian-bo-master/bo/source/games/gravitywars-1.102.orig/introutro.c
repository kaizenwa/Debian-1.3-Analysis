/* GravityWars 1.1,  (C) Sami Niemi -95 */

#include <vgakeyboard.h>
#include <math.h>
#include <vga.h>
#include <asm/io.h>
#include "memory.h"
 
void intro() {

  long  pal_m;
  long  pal_n;
  uchar pal[768],p1[768],p2[768];
  uchar map[320000]; 
  uchar dst[307200];
  uchar filename[128];
  uchar reg;

  signed long n,x,y;
  char col;
  unsigned char num;
  long adr;
 
  unsigned char *s;
  long d;
  char wrdy[4];

  float nn;
  long nnn,sn1,sn2;


  long m;
 
  FILE *fileptr2;

  for(nn=m=0; nn<=6.28318; nn+=0.0245,m++)
    bigsin[m]=512+512*sin(nn);

  strcpy(filename,gamename);
  strcpy(&filename[gamenamelen],"data/gravmix.gw");
  if ( ( fileptr2=fopen(filename,"r") )!=NULL) {
    fread(map,320000,1,fileptr2);
  
    fclose(fileptr2);
  }
  else {
    printf("ERROR!!!!!!!!! CAN'T LOAD THE TITLE GFX!\n");
    exit(1);
  }

  /* LBM -> RAW Convert */

  s=map;

  do {
    wrdy[0]=wrdy[1];
    wrdy[1]=wrdy[2];
    wrdy[2]=wrdy[3];
    wrdy[3]=*(s++);
  } while((wrdy[0]!='C') ||
          (wrdy[1]!='M') ||
          (wrdy[2]!='A') ||
          (wrdy[3]!='P'));
  s+=4;
  for(n=0; n<=767; n++) {
    pal[n]=s[n]>>2;
  }

  memcpy(p1,pal,768);
  for (n=0; n<=767; n++) {
    p2[n]=63-pal[n];
  }

  do {
    wrdy[0]=wrdy[1];
    wrdy[1]=wrdy[2];
    wrdy[2]=wrdy[3];
    wrdy[3]=*(s++);
  } while((wrdy[0]!='B') ||
          (wrdy[1]!='O') ||
          (wrdy[2]!='D') ||
          (wrdy[3]!='Y'));

  s+=4;

  d=0;
  adr=0;
  do {
    
    num=*(s++);
    if (num&128) {
      col=*(s++);
      for(n=1; n<=(257-num); n++) {

	vga_setpage(adr >> 16);
	*(vga_ptr+(d&65535))=col;
	d++;

        adr++;
      }
    }
    else {
      num++;
      if (num==256)
        num=1;
      for(n=1; n<=num; n++) {
      

        vga_setpage(adr >> 16);
        *(vga_ptr+(d&65535))=*(s++);
        d++;

        adr++;
      }
    }
  } while(adr<307200);

  /*-----------*/


  gl_setdisplaystart(0,0);


  /* Split Screen (sl=SplitScreenLine)   */
  outb(0x18,0x3d4);       /* LineCompare */
  outb(0,0x3d5);        /* sl&255      */
  outb(7,0x3d4);          /* Overflow    */
  reg=inb(0x3d5);         
  outb(reg&239,0x3d5);   /* outb(reg|((sl&256)>>4),0x3d5) */
  outb(9,0x3d4);          /* Maximum Scanline */
  reg=inb(0x3d5);          
  outb(reg&191,0x3d5);    /* outb(reg|((sl&512)>>3),0x3d5) */
  



  while(keyboard_update());

  for(pal_n=0; pal_n<=1024; pal_n+=16) {
    for (pal_m=0; pal_m<=767; pal_m++)
      p1[pal_m]=((((long)pal[pal_m])*pal_n)>>10);
    vga_waitretrace();
    gl_setpalette(p1);
  }
  gl_setpalette(pal);

  pal[756]=34;
  pal[757]=17;
  pal[758]=19;

  pal[759]=25;
  pal[760]=16;
  pal[761]=41;

  pal[762]=22;
  pal[763]=23;
  pal[764]=22;

  while(keyboard_update()==0) {



    for(nnn=64; nnn<=192; nnn++) {
      sn1=bigsin[nnn];
      sn2=bigsin[(nnn+128) & 255];

      for(m=0; m<=767; m++)
        p1[m]=(pal[m]*sn1)>>10;
      for(m=756; m<=765; m++)
        p1[m]=(pal[m]*sn2)>>10;
      if(keyboard_update()) goto exitfade;
      vga_waitretrace();
      gl_setpalette(p1);
    }

    for(m=0; m<=100; m++) {
      if(keyboard_update()) goto exitfade;
      vga_waitretrace();
    }

    for(nnn=192; nnn<=320; nnn++) {
      sn1=bigsin[nnn&255];
      sn2=bigsin[(nnn+128)&255];

      for(m=0; m<=767; m++)
        p1[m]=(pal[m]*sn1)>>10;
      for(m=756; m<=765; m++)
        p1[m]=(pal[m]*sn2)>>10;
      if(keyboard_update()) goto exitfade;
      vga_waitretrace();
      gl_setpalette(p1);
    }

    for(m=0; m<=100; m++) {
      if(keyboard_update()) 
	goto exitfade;
      vga_waitretrace();
    }
  }
 exitfade:

  if (*(scans+SCANCODE_ESCAPE))
    escape=TRUE;
    

  for(pal_n=1024; pal_n>=0; pal_n-=16) {
    for (pal_m=0; pal_m<=767; pal_m++)
      pal[pal_m]=(((long)p1[pal_m])*pal_n)>>10;
    vga_waitretrace();
    gl_setpalette(pal);
  }
  gl_setpalette(p0);

  /* Split Screen (sl=SplitScreenLine)   */
  outb(0x18,0x3d4);       /* LineCompare */
  outb(208,0x3d5);        /* sl&255      */
  outb(7,0x3d4);          /* Overflow    */
  reg=inb(0x3d5);         
  outb(reg|0x10,0x3d5);   /* outb(reg|((sl&256)>>4),0x3d5) */
  outb(9,0x3d4);          /* Maximum Scanline */
  reg=inb(0x3d5);          
  outb(reg&191,0x3d5);    /* outb(reg|((sl&512)>>3),0x3d5) */
  

  
}


void outro() {
  long  pal_m;
  long pal_n;

  putstamp(240,win_y+192,139,144,146);
  putstamp(336,win_y+192,147,153,50);
  killstamp(400,win_y+192);
 
  putstamp(176,win_y+256,108,109,117);
  putstamp(272,win_y+256,119,120,122);
  putstamp(368,win_y+256,127,130,136);

  sleep(3);
  waitanykey();

  if (levelnum&1) {
    for(pal_n=1024; pal_n>=0; pal_n-=16) {
      for (pal_m=0; pal_m<=767; pal_m++)
	p1[pal_m]=(((long)palB[pal_m])*pal_n)>>10;
      vga_waitretrace();
      gl_setpalette(p1);
    }
    gl_setpalette(p0);
  }
  else {
    for(pal_n=1024; pal_n>=0; pal_n-=16) {
      for (pal_m=0; pal_m<=767; pal_m++)
        p1[pal_m]=(((long)pal[pal_m])*pal_n)>>10;
      vga_waitretrace();
      gl_setpalette(p1);
    }
    gl_setpalette(p0);
  }
  mouse_close();
  keyboard_close();

  vga_setmode(TEXT);
  printf("-----------------------------------------------------------------------------\n"
	 "                     C o n g r a t u l a t i o n s ! ! ! \n"
	 "             Thanks for playing the game, I hope you enjoyed it!\n"
	 "             Now... Feel free to design some new levels with the\n"
	 "             tools (or develop better ones..) that come with the\n"
	 "             package, since these ones just seem to be too easy!\n"
	 "             (or maybe you just cheated...)\n"
	 "-----------------------------------------------------------------------------\n");
  exit(0);
}
  

