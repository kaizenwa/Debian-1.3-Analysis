/* GravityWars 1.1,  (C) Sami Niemi -95 */

#include <vga.h>
#include <vgagl.h>
#include <math.h>
#include <asm/io.h>
#include "memory.h"

/*--------------------------------------------------------------- initScreen */
int initScreen() {
  vga_modeinfo *minfo;
  short n,ptr;
  uchar reg;

  vga_init();
  vga_setmode(G640x480x256);
  gl_setcontextvga(G640x480x256);
  gl_setcontextvirtual(640,1440,BYTESPERPIXEL,BITSPERPIXEL,VBUF);
  vga_claimvideomemory(640*480*3);

  vga_ptr=vga_getgraphmem();
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

/*--------------------------------------------------------------- initParams */
void initParams() {

  short a,b,c,d,e;
  long n;

  for(n=0; n<=N_BULLETS; n++) {
    bullet[n].active=FALSE;
  }

  /* Count a "DECIMAL to BCD" table 

     Hmmm.. I think I've got some explaining to do over here.. :) 
     The game was just too slow on my machine, so I had to
     optimize  the things as much as I could.. I had plenty of memory
     so I thought that I'd be OK to waste some of it...
  */

  n=0;
  for(a=0; a<=9; a++)
    for(b=0; b<=9; b++)
      for(c=0; c<=9; c++)
	for(d=0; d<=9; d++)
	  for(e=0; e<=9; e++, n++) 
	    Dec2BCD[n]=(a<<16)|(b<<12)|(c<<8)|(d<<4)|e;

  
  /* Should really be set from #defines in an include file.. */

  ship_flag=0;
  g_flag=0;
  ScoreChange=0;
  thrust_len=0;
  watercount=0;
  num_of_collisions=0;
  ShipScore=0;
  ShipLife=3;
  delay_len=0;
  escape=FALSE;
  loadtime[0]=0;
  loadtime[1]=0;
}


/*---------------------------------------------------------------------------*/
/* Just a routine to read some *.dat info */

void readquotes(char *dest) {

  static int ch,pos;

  pos=0;
  while( (ch=fgetc(fileptr))!='"');
  while( (ch=fgetc(fileptr))!='"')
    dest[pos++]=ch;
}


/*---------------------------------------------------------------------------*/
/* Load & Parse the level */
void loaddata() {

  char command[256];
  char buffer[128];
  char fname[128];
  char tmp_nextlevel[128];

  static short err;

  static short x,y,adr,count,n,ptr;
  static char ch;

  strcpy(nextlevel,gamename);
  strcpy(&nextlevel[gamenamelen],"levels/level00");
  nextlevel[gamenamelen+12]=(levelnum/10)+'0';
  nextlevel[gamenamelen+13]=(levelnum%10)+'0';
  
/*  printf("(Loading: %s)\n",nextlevel); */

  strcpy(tmp_nextlevel,nextlevel);
  strcpy(fname,tmp_nextlevel);
  strcpy(&fname[strlen(fname)],".dat");

  err=0;
  n_anim=0;
  
  if ( ( fileptr=fopen(fname,"r") )!=NULL) {
    
    do {
      err=fscanf(fileptr,"%s",command);

      if (err!=EOF) {
        if (!strcmp(command,"anim")) {
	  if (n_anim>N_ANIM) {
	    printf("\nOoops... The level designer has screwed up..\n"
		   "You're not allowed to have more than %d animations..\n"
		   ,N_ANIM);
	    doPanic();
	  }
          fscanf(fileptr,"%s",buffer);
          anim[n_anim].x=atoi(buffer);
          fscanf(fileptr,"%s",buffer);
          anim[n_anim].y=atoi(buffer);
          fscanf(fileptr,"%s",buffer);
          anim[n_anim].start=atoi(buffer);
          fscanf(fileptr,"%s",buffer);
          anim[n_anim].stop=atoi(buffer);
          fscanf(fileptr,"%s",buffer);
          anim[n_anim].frame=atoi(buffer);
          fscanf(fileptr,"%s",buffer);
          anim[n_anim].speed=atoi(buffer);
	  n_anim++;
        }
        else

/*
          if (!strcmp(command,"level")) 
            readquotes(LEVEL);
	  else
            if (!strcmp(command,"author")) 
	      readquotes(AUTHOR);
	    else
              if (!strcmp(command,"comment")) 
                readquotes(COMMENT);
              else
                if (!strcmp(command,"next")) 
                  fscanf(fileptr,"%s\n",nextlevel);
                else
*/  
	  if (!strcmp(command,"time")) {
	    fscanf(fileptr,"%s",buffer);
	    BaseTime=atoi(buffer)<<7;
	    ShipTime=BaseTime;
	  }
	  else
	    if (!strcmp(command,"fuel")) {
	      fscanf(fileptr,"%s\n",buffer);
	      BaseFuel=atoi(buffer)<<4;
	      ShipFuel=BaseFuel;
	    }
      }
    } while(err!=EOF);
    
/*
    printf("---------------------------------------"
	   "--------------------------------------\n"
	   "Level...... %s\n"
	   "Author..... %s\n"
	   "Comments... %s\n"
	   "---------------------------------------"
	   "--------------------------------------\n"
	   ,LEVEL,AUTHOR,COMMENT);
*/

    fclose(fileptr);
  }
  else {
/*
    printf("Error: Can't open %s!\n",fname);
    doPanic();
*/
    outro();
  }


/* Load Level Data */

  strcpy(fname,tmp_nextlevel);
  strcpy(&fname[strlen(fname)],".gfx");
 
  if ( ( fileptr=fopen(fname,"r") )!=NULL) {

    for(y=ptr=0; y<=44; y++,ptr+=20) {
      fread(&level[ptr],20,1,fileptr);
    }
    fclose(fileptr);
  }
  else {
    printf("Error: Can't open %s!\n",fname);
    doPanic();
  }

  strcpy(fname,tmp_nextlevel);
  strcpy(&fname[strlen(fname)],".obj");
 
  if ( ( fileptr=fopen(fname,"r") )!=NULL) {

    for(y=ptr=0; y<=44; y++,ptr+=20) {
      fscanf(fileptr,"%s",&objects[ptr]);
    }

    fclose(fileptr);
  }
  else {
    printf("Error: Can't open %s!\n",fname);
    doPanic();
  }

/* Go through the level and copy the L_RED_DOOR blocks to indipendent blocks */

  count=0;
  for(y=0; y<=44; y++) {
    for(x=0; x<=19; x++) {
      adr=x+(y<<4)+(y<<2);
      ch=objects[adr];

      if (ch==L_RED_DOOR) {
	if (count>(N_DESTROYEABLE+1)) {
	  printf(
		 "Sorry to interrupt you, but it looks like the level   \n"
		 "designer screwed up... There can only be %d so called \n"
		 "Red Doors (+surrounding blocks) in a level.\n\n",
		 N_DESTROYEABLE);
	  doPanic();
	}
	memcpy(block[216+count],block[level[adr]],1024);
	level[adr]=216+count;	
	count++;
      }
    }
  }


/* Go through the level and make c_except[] structures */
  count=0;
  n_cexcept=0;
  for(y=0; y<=44; y++) {
    for(x=0; x<=19; x++) {

      if ((ch=objects[x+(y<<4)+(y<<2)])=='f') {
	if (n_cexcept>N_CEXCEPT) {
	  printf("\nOoops... The level designer has screwed up..\n"
		 "You're not allowed to have more than %d fuelstations..\n"
		 ,N_CEXCEPT-1);
	  doPanic();
	}
 	c_except[n_cexcept].x=x;
	c_except[n_cexcept].y=y-1;
	c_except[n_cexcept].type=E_FUEL;
	n_cexcept++;    
      }
      else 
	if (ch=='s') {
	  if (count>0) {
	    printf("\nOoops... The level designer has screwed up..\n"
		   "You can't have more than one startfield..\n"
		   ,N_CEXCEPT-1);
	    doPanic();
	  }
	  c_except[n_cexcept].x=x;
	  c_except[n_cexcept].y=y-1;
	  c_except[n_cexcept].type=E_START;
	  n_cexcept++;
	  Lsx[0]=(x<<5) << STEP;
	  Lsy[0]=((y<<5)-28) << STEP;
	}
      else
	if (ch=='x') {
	  stop_x=x;
	  stop_y=y;
	  level[x+(y<<4)+(y<<2)]=38;
	  objects[x+(y<<4)+(y<<2)]=E_NULL;
	}
    }
  }

  n_cexcept--;

  sx[0]=Lsx[0];
  sy[0]=Lsy[0];
  sa[0]=0;
  sVx[0]=sVy[0]=sVg[0]=1;  /* Shouldn't ever be Zero */
  sA[0]=0;
  sf[0]=0;

  /* Count the nymber of keys */
  NumKeys=0;
  for(n=0; n<=899; n++) {
    if ((objects[n]==E_KEY) || (objects[n]==E_WKEY)) NumKeys++;
  }
 
  for(n=0; n<=N_ACTION; n++) {
    action[n].state=FALSE;
  }

  /* Default Air Values */
  gravity=10; /*15*/
  lift_thrust=10;
  medium=1010; 
  friction=1000; /*24;*/
  MaxNorm=3072;
  MaxFall=4096;

}

/*---------------------------------------------------------------------------*/
/* Load the graphics */
/* Format:

 char Palette1[768];
 char Palette2[768];
 char blocks[];        // Consists of 32*32 blocks starting at (1,1)
                       // 9 by 24, I think.. (A single pixel between
                       // The blocks.
*/ 


int loadGfx() {


  short obj,err,n,x,y,yy,ptr,row;

  char hs[10];

  char filename[128];

/* Load Gfx */
  strcpy(filename,gamename);
  strcpy(&filename[gamenamelen],"data/blocks.gw");
  if ( ( fileptr=fopen(filename,"r") )!=NULL) {
    
    fread(pal,768,1,fileptr);      /* Palette */

    /* Copy and invert the palette for FADE routine */
   

    for(n=0; n<=767; n++) {
      realpal[n]=pal[n];
      p2B[n]=63-pal[n];
      p0[n]=0;
      p3[n]=63;
    } 
    

    fread(palB,768,1,fileptr);      /* Palette II */
    for(n=0; n<=767; n++) {
      realpal2[n]=palB[n];
      p2[n]=63-palB[n];
    }  
    
    fread(buffer,321,1,fileptr);    /* get rid of the grid */
    
    obj=0;
    for(row=0; row<=23; row++) { 
      err=fread(buffer,10560,1,fileptr); /* Gfx (320*33)*/
      /* printf("Err: %d\n",err);*/

      /*if  (err==1) {*/
	ptr=0;
	for(n=0; n<=8; n++) {
	  for(x=0; x<=31; x++) 
	    for(y=0,yy=0; y<=31; y++,yy+=320) 
	      block[obj][x+(y << 5)]=buffer[ptr+x+yy]; 
	  obj++; 
	  ptr+=33;
/*	  printf("Object #%d\n",obj);*/
	}
      /*}*/
    }  


    fclose(fileptr);  
  }
  else {
    printf("ERROR!!!!!!!!! CAN'T LOAD THE GFX!\n");
    exit(1);
  }

/* Get Ship (also for the unused one)*/

  memcpy(ship[0][0],block[208],1024);
  memcpy(ship[1][0],block[208],1024);

  memcpy(ship[2][0],block[207],1024);
  memcpy(ship[3][0],block[207],1024);


  /* Copy The Background block to a 3*3 area (Slow code) */
  n=0;
  for(y=0; y<=63; y++) {
    for(x=0; x<=63; x++) {
      backgnd[n]=block[38][(x&31)+((y&31)<<5)];
      n++;
    }
  }


/* Load Score */

  strcpy(filename,gamename);
  strcpy(&filename[gamenamelen],"data/score.gw");
  if ( ( fileptr=fopen(filename,"r") )!=NULL) {
    fread(score,4096,1,fileptr);    /* Picture */  
    fclose(fileptr);
  }
  else {
    printf("ERROR!!!!!!!!! CAN'T LOAD THE SCORE GFX!\n");
    exit(1);
  } 

  strcpy(filename,gamename);
  strcpy(&filename[gamenamelen],"data/digits.gw");
  if ( ( fileptr=fopen(filename,"r") )!=NULL) {
    fread(digits,528,1,fileptr);    /* Picture */
    fclose(fileptr);
  }
  else {
    printf("ERROR!!!!!!!!! CAN'T LOAD THE DIGITS GFX!\n");
    exit(1);
  }

  strcpy(filename,gamename);
  strcpy(&filename[gamenamelen],"data/numbers.gw");
  if ( ( fileptr=fopen(filename,"r") )!=NULL) {
    fread(numbers,880,1,fileptr);    
    fclose(fileptr);
  }
  else {
    printf("ERROR!!!!!!!!! CAN'T LOAD THE NUMBERS GFX!\n");
    exit(1);
  }

  strcpy(filename,gamename);
  strcpy(&filename[gamenamelen],"data/high.gw");
  if ( ( fileptr=fopen(filename,"r") )!=NULL) {
    fread(highscore,4096,1,fileptr);    /* Picture */
    fclose(fileptr);
  }
  else {
    printf("ERROR!!!!!!!!! CAN'T LOAD THE HIGHSCORE GFX!\n");
    exit(1);
  }
 
  strcpy(filename,gamename);
  strcpy(&filename[gamenamelen],"data/level.gw");
  if ( ( fileptr=fopen(filename,"r") )!=NULL) {
    fread(levelcode,4096,1,fileptr);    /* Picture */
    fclose(fileptr);
  }
  else {
    printf("ERROR!!!!!!!!! CAN'T LOAD THE LEVELCODE GFX!\n");
    exit(1);
  }

  strcpy(filename,gamename);
  strcpy(&filename[gamenamelen],"data/fonts.gw");
  if ( ( fileptr=fopen(filename,"r") )!=NULL) {
    fread(fonts,1820,1,fileptr);    /* Picture */
    fclose(fileptr);
  }
  else {
    printf("ERROR!!!!!!!!! CAN'T LOAD THE FONTS GFX!\n");
    exit(1);
  }

  strcpy(filename,gamename);
  strcpy(&filename[gamenamelen],"data/hscore.gw");
  if ( ( fileptr=fopen(filename,"r") )!=NULL) {
    fscanf(fileptr,"%s",hs);
    fclose(fileptr);
  }
  else {
    printf("ERROR!!!!!!!!! CAN'T LOAD THE HIGHSCORE!\n");
    exit(1);
  }

  HighScore=atoi(hs);

}

/*------------------------------------------------------------------- rotGfx */
/* Rotates the ship and saves the frames                                     */
/*---------------------------------------------------------------------------*/

int rotGfx() {
  
  short n,x,y;
  long	sx,sy,dx,dy,rx,ry,ptr,adr;
  float	ang;
  
  angX[0]=0;
  angY[0]=-ACCEL;
 
  
  for(n=1; n<=31; n++) {
    ptr=0;
    ang=.196349540*n;
    dx=1024*cos(ang);
    dy=1024*sin(ang);

    angX[n]=ACCEL*cos(ang+1.57079);
    angY[n]=-ACCEL*sin(ang+1.57079);

    rx=16*1024+16*(dy-dx);
    ry=16*1024+16*(-dx-dy);
    
    for(y=0; y<=31; y++) {
      rx-=dy;
      ry+=dx;
      for(x=0; x<=31; x++) {
	rx+=dx;
	ry+=dy;
	
	if ( (rx<0) || (rx>32768) || (ry<0) || (ry>32768) ) {		      
	  ship[0][n][ptr]=0;
	  ship[1][n][ptr]=0;
	  ship[2][n][ptr]=0;	
	  ship[3][n][ptr]=0;
	  ptr++;
	}
	else {
	  adr=(rx>>10) + ((ry>>10) << 5);
	  ship[0][n][ptr]=ship[0][0][adr];
	  ship[1][n][ptr]=ship[1][0][adr];
 
	  ship[2][n][ptr]=ship[2][0][adr];
          ship[3][n][ptr]=ship[3][0][adr];
	  ptr++;
	}
      }
      rx-=(dx << 5);
      ry-=(dy << 5);
    }
  }
}










