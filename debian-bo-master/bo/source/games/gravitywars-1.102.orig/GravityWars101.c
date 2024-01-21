/* -------------------------------------------------------------------------
   Gravity Wars v1.1, (C) Sami Niemi 1995, 1996

   NOTES: [960824] [RELEASE NOTES 961101]

   The game started as a bunch of fun routines that made the ship fly.
   Later on I added collition control, and after Pär got involved with
   the graphics I just couldn't stop... So, the structure of the game is
   just terrible, with lots of unecessary variables , flags that
   are used as variables etc.. I should've written the program in c++, and
   for X! Svgalib just contains too many bugs.. 
   
   I'd be fun to see this game ported to different operating systems, though
   the structure of the game might make it hard, especially since I used 
   the SVGALIB.

   You're free to modify the code and the graphics as long as you write
   that the original code was written by Sami Niemi, and the original
   graphics were drawn by Pär Johannesson (If you're using any parts of
   the original graphics)
   
   I'd love to hear from any changes that you've made to the game!

   / Sami

   e96sn@efd.lth.se
   sniemi@kuai.se
   http://www.kuai.se/~sniemi
   http://www.efd.lth.se/~e96sn


   REMEMBER:

   getbox(x,y,*ptr) Gets the original graphics based on the block[]&level[]
   getbox2(x,y,*ptr Gets the graphics directly from the screen



   BUGS (i.e. I don't have the time to fix'em):

   - consoleswitching takes away the splitscreen (real easy to fix)

   ------------------------------------------------------------------------- */
#include <unistd.h>
#include "includes.h"
#include "memory.h"
 
/*--------------------------------------------------------------------- main */
int main(int n_args, char *arg[]) {
  

  int n,ch,x,y;
  
  long tmp_long;
  
  long  pal_m;
  long pal_n;
  
  short baselevel;
  char filename[128];
  
  char firstlevel[20]="levels/level01";
  char buf[128];
  
  levelnum=1;
  
  if (readlink(arg[0],buf,128)==-1) 
    strcpy(buf,arg[0]);
  
  n=strlen(buf);
  while(n>=0)                       
    if (buf[n--]=='/') break;
  if (n<0) {
    strcpy(gamename,"./");
    gamenamelen=2;
  }  
  else {
    strcpy(gamename,buf);
    gamename[n+2]=0;
    gamenamelen=strlen(gamename);
  }
  

  if (n_args>1) {
    n=0;
    do {
      n++;
    } while ( (strncmp(arg[1],codes[n],6)) && (n<99));
    if (n!=99) {
      levelnum=n+1;
      if (n_args>2)
	delay_len=atoi(arg[2]);
    } else {
      if ((arg[1][0]<'0') || (arg[1][0]>'9')) {
	printf("\nUsage: %s [code-to-a-given-level] [beam-adjustment]\n"
	       "(Sure you've got the right code?)\n\n",arg[0]);
	exit(0);
      }
      else 
	delay_len=atoi(arg[1]);
    } 
  }    
  
  baselevel=levelnum;

  strcpy(firstlevel,nextlevel);


  printf("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
"\n"
"-----------------------------------------------------------------------------\n"
"                         G R A V I T Y    W A R S\n\n"
"                                   v 1.1\n"
"                              (November 1995)\n"
"\n"
"            A Gravity Force clone for the Linux operating system\n"

"\n\n"
"                           Coding by Sami Niemi\n\n"

"                       Graphics by Pär Johannesson\n"

"-----------------------------------------------------------------------------\n"
);
  initParams();
  loadGfx();
  rotGfx();

  initScreen();

  keyboard_init();
  mouse_init("/dev/mouse", MOUSE_MICROSOFT, MOUSE_DEFAULTSAMPLERATE);
  scans=keyboard_getstate();

  ending=TRUE;

  while(ending) {

    intro();
    if (escape) 
      goto escaped;
    loaddata();
    drawScreen();

/*
    old_x=0;
    old_y=0;
    getbox(old_x, old_y, shipback[0]);
    putbox(old_x, old_y, shipback[0]);
    moveShip(0,0);
*/
    old_x=sx[0] >> STEP;
    old_y=sy[0] >> STEP;
    moveShip(0,0);

    updatescore();
    win_y=(sy[0] >> STEP)-232;
    if (win_y<8)
      win_y=8;
    else
      if (win_y>968)
	win_y=968;
    putscore(0,win_y); 


    /*CenterShip*/
    y=(sy[0] >> STEP)-240;
    if (y<16)
      y=16;
    else
      if (y>960) 
	y=960;
    gl_setdisplaystart(0,y);

    stampnum(levelnum);
    putstamp(272,y+184,68,69,70);

    if (levelnum&1) {
      for(pal_n=0; pal_n<=1024; pal_n+=16) {
	for (pal_m=0; pal_m<=767; pal_m++)
	  p1[pal_m]=(((long)pal[pal_m])*pal_n)>>10;
	vga_waitretrace();
	gl_setpalette(p1);
      }
      gl_setpalette(pal);
    }
    else {
      for(pal_n=0; pal_n<=1024; pal_n+=16) {
        for (pal_m=0; pal_m<=767; pal_m++)
          p1[pal_m]=(((long)palB[pal_m])*pal_n)>>10;
        vga_waitretrace();
        gl_setpalette(p1);
      }
      gl_setpalette(palB);
    }

    waitanykey();

    killstamp(272,y+184);
 
    anim_frame=0;
    ShipScore=0;
  
    control(0); 

    /*------ The End --------*/
    if (ending) {

      for(n=0; n<=4095; n++) {
	tmpscore[n]=score[n];
	score[n]=highscore[n];
      }

      tmp_long=Dec2BCD[ShipScore];

      putdigit(1375,tmp_long>>16);
      putdigit(1383,(tmp_long>>12)&15);
      putdigit(1391,(tmp_long>>8)&15);
      putdigit(1399,(tmp_long>>4)&15);
      putdigit(1407,tmp_long&15);

      if (ShipScore>HighScore) {
	HighScore=ShipScore;

	strcpy(filename,gamename);
	strcpy(&filename[gamenamelen],"data/hscore.gw");
	if ( ( fileptr=fopen(filename,"w") )!=NULL) {
	  fprintf(fileptr,"%d\n",ShipScore);
	  fclose(fileptr);
	}
	else {
	  printf("Can't write the HighScore!!!!");
	  doPanic();
	}
      }
 
      tmp_long=Dec2BCD[HighScore];
 
      putdigit(1474,tmp_long>>16);
      putdigit(1482,(tmp_long>>12)&15);
      putdigit(1490,(tmp_long>>8)&15);
      putdigit(1498,(tmp_long>>4)&15);
      putdigit(1506,tmp_long&15);

      putscoreOnly(0,win_y);

      putstamp(272,win_y+192,54,60,61);
      waitanykey();
      killstamp(272,win_y+192);
      if (*(scans+SCANCODE_ESCAPE))
	goto escaped;

      for(n=0; n<=4095; n++) {
	score[n]=tmpscore[n];
      }

      if (levelnum&1) {
	for(pal_n=1024; pal_n>=0; pal_n-=16) {
	  for (pal_m=0; pal_m<=767; pal_m++)
	    p1[pal_m]=(((long)pal[pal_m])*pal_n)>>10;
	  vga_waitretrace();
	  gl_setpalette(p1);
	}
	gl_setpalette(p0);
      }
      else {
        for(pal_n=1024; pal_n>=0; pal_n-=16) {
          for (pal_m=0; pal_m<=767; pal_m++)
            p1[pal_m]=(((long)palB[pal_m])*pal_n)>>10;
          vga_waitretrace();
          gl_setpalette(p1);
        }
        gl_setpalette(p0);
      }

      strcpy(nextlevel,firstlevel);
      levelnum=baselevel;
      ShipLife=3;
    }

  }
 escaped:    

  mouse_close();
  keyboard_close();

  vga_setmode(TEXT);
  printf("\n");
  exit(0);

}































