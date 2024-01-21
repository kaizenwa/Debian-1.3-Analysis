
/* Written by Peter Ekberg, peda@lysator.liu.se */

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#if defined(HAVE_GETOPT_H) && defined(HAVE_GETOPT_LONG_ONLY)
# include <getopt.h>
#elif !defined(HAVE_GETOPT_LONG_ONLY)
# include "getopt.h"
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include <math.h>

#include "thrust_types.h"
#include "thrust.h"
#include "highscore.h"
#include "graphics.h"
#include "fast_gr.h"
#include "gr_drv.h"
#include "font5x5.h"
#include "things.h"
#include "conf.h"
#include "init.h"
#include "level.h"
#include "keyboard.h"
#include "options.h"

#ifdef HAVE_SOUND
#include "soundIt.h"

#define CHAN_1 0
#define CHAN_2 1
#define CHAN_3 2
#define CHAN_4 3
#define SND_BOOM   0
#define SND_BOOM2  1
#define SND_HARP   2
#define SND_THRUST 3
#define SND_ZERO   4
#endif

byte *bulletmap;
byte *blocks;
byte *ship;
byte *shieldship;
byte *bana;
byte *fuelmap;
byte *loadmap;
byte *shipstorage;
byte *bulletstorage;
byte *fragmentstorage;
byte *fuelstorage;
byte *loadstorage;
byte *wirestorage;

word lenx; /* x-size of level */
word leny; /* y-size of level */
word lenx3, leny3;
             /* Status of game. */
double alpha, deltaalpha;
word loaded, loadcontact, loadpointshift;
int loadpoint;
int countdown;
word crash, shoot, repetetive;
word refueling;
int speedx, speedy;
long absspeed, oldabs;
int kdir, dir;
int shipdx, shipdy;
int x, y;              /* Top left corner, 8 units per pixel. */
int pixx, pixy;        /* Top left corner in pixels.   */
int pblockx, pblocky;  /* Top left corner in blocks (8x8 pixels). */
int vx, vy;				/* Speed of the ship. */
int bildx, bildy;      /* Top left corner of backing store (in pixels). */
int bblockx, bblocky;  /* Top left corner of backing store (in blocks). */
int loadbx, loadby;    /* Position of the load (in blocks). */
int gravity;
int score;
byte shield;
byte colorr, colorg, colorb;
int nodemo=0;
int Thrust_Is_On=0;
int play_sound=1;
double gamma_correction=1.0;

#define checkfork(b, a) \
  case b-1: \
    if(easyrider==a || easyrider==a+1) \
      easyrider=a+1; \
    else if(easyrider!=9) \
      easyrider=-1; \
    break

int
insideblock(int blkx, int blky, int pblkx, int pblky,
	    int sx, int sy)
{
  return((blkx>=pblkx-sx) && (blkx<pblkx+BBILDX) &&
	 (blky>=pblky-sy) && (blky<pblky+BBILDY));
}

int
insidepixel(int x, int y, int pixx, int pixy, int sx, int sy)
{
  return((x>pixx-sx) && (x<pixx+PUSEX) &&
	 (y>pixy-sy) && (y<pixy+PUSEY));
}

void
updateborder(int pblkx, int pblky, int bblkx, int bblky,
	     int vx, int vy)
{
  word k;

  if(vy<=0) /* update bottom border */
    for(k=0; k<BBILDX; k++)
      putblock(bblkx+k, (bblky+BBILDY-1)%BBILDY, blocks+
	       (*(bana+(pblkx+k)%lenx+((pblky+BBILDY-1)%leny)*lenx)<<6));
  else      /* update top border */
    for(k=0; k<BBILDX; k++)
      putblock(bblkx+k, bblky, blocks+
	       (*(bana+(pblkx+k)%lenx+(pblky%leny)*lenx)<<6));
  if(vx>0)  /* update right border */
    for(k=0; k<BBILDY; k++)
      putblock(bblkx+BBILDX-1, (bblky+k)%BBILDY, blocks+
	       (*(bana+(pblkx+BBILDX-1)%lenx+((pblky+k)%leny)*lenx)<<6));
  else      /* update left border */
    for(k=0; k<BBILDY; k++)
      putblock(bblkx, (bblky+k)%BBILDY, blocks+
	       (*(bana+pblkx%lenx+((pblky+k)%leny)*lenx)<<6));
}

void
pause_message(void)
{
  char *str[] = {
    "GAME PAUSED.",
    "PRESS 'C' TO CONTINUE..." };

  chflag=1;
  gcenter(80, str[0]);
  gcenter(91, str[1]);
  chflag=0;
  displayscreen();
}

void
escape_message(void)
{
  char *str[] = {
    "ARE YOU SURE YOU WANT TO QUIT (Y/N)?" };

  chflag=1;
  gcenter(85, str[0]);
  chflag=0;
  displayscreen();
}

byte
whatkeys(void)
{
  byte keys=0;

/* Use this to create a demo.
 * Don't forget to uncomment the fputc statement below.
 */
  /*
  static FILE *f=NULL;
  if(f == NULL)
    f=fopen("foobarfoo", "w");
  */

  keys = getkeys();

  /*
  fputc((keys&escape_bit)?quit_bit:keys, f);
  */
  return(keys);
}

byte
nextmove(int reset)
{
  static byte *p;
  byte retbits=0;

  if(reset) {
    keywaiting();
    p=&bin_demomoves[0];
  }
  else if(keywaiting()) {
    retbits=quit_bit;
    flushkeyboard();
  }
  else {
    retbits=*(p++);
    retbits&=~thrust_bit;
    retbits|=*(p-!!(random()%20)) & thrust_bit;
  }

  return(retbits);
}

int
game(int demo)
{
  byte actionbits=0;
  double ax, ay, aradial, acircum;
  word lives, endlevel;
  word dying;
  word alive;
  word fuel;
  int l;
  word wrapers;
  int level;
  int round;
  static char **levels[LEVELS]
    = { level1, level2, level3, level4, level5, level6 };
  static char textstr[40];
  int localscore;
  options end;
  int ch;
  int lastlevel;
  int easyrider=0;
  int restartx=0, restarty=0;
  int loadedrestart=0;
  restartpoint *restartxy;
  int gravitymsg;
  int teleport;

  if(demo)
    nextmove(1);

  lives=3; /* 3 */
  localscore=0;
  score=0;
  round=0; /* 0 */
  level=0; /* 0 */
  lastlevel=-1;
  shield=0;
  fuel=1000; /* 1000 */
  gravitymsg=0;
  teleport=0;

  while(level<LEVELS && lives>0 && fuel) {
#ifdef DEBUG2
    printf("Newlevel: %d\n", level);
#endif
    endlevel=0;
    wrapers=0;
    dying=0;
    alive=1;
    
    srandom(time(NULL));
    if(level!=lastlevel || !spacestation) {
      if(level==0 && lastlevel>0)
	gravitymsg=1;
      if(!readbana(levels[level])) {
	printf("Illegal definition of level %d.\n", level+1);
	return(1);
      }
      restartx=(lenx+restartpoints[0].x-(154>>3))%lenx;
      restarty=restartpoints[0].y-(82>>3)-4*(round&1);
      loadedrestart=0;
      initgame(round, 1, restartx, restarty);
    }
    else {
      loaded=loadedrestart;
      initgame(round, 0, restartx, restarty);
    }

    initscreen(round);
    putscr(pixx%PBILDX, pixy%PBILDY);
    lastlevel=level;

    printgs(250, 184, "FUEL");
    sprintf(textstr, "LIVES: %d", lives);
    printgs(20, 192, textstr);
    sprintf(textstr, "SCORE: %d", score);
    printgs(20, 184, textstr);
    sprintf(textstr, "LEVEL %d",  level+1);
    gcenter(70, textstr);
    if(gravitymsg)
      gcenter(60, (round&1) ? "REVERSED GRAVITY": "NORMAL GRAVITY");
    gravitymsg=0;
    displayscreen();
    syncscreen();
    fade_in();
    usleep(1000000UL);
    syncscreen();
    putscr(pixx%PBILDX, pixy%PBILDY);
    while(!endlevel) {
      actionbits=demo ? nextmove(0) : whatkeys();

      if(actionbits&quit_bit) {
#ifdef DEBUG2
	printf("Endlevel: User pressed quit-key\n");
#endif
	endlevel=1;
      }

      if(actionbits&pause_bit) {
#ifdef HAVE_SOUND
	if(play_sound) {
	  Thrust_Is_On=0;
	  Snd_effect(SND_ZERO, CHAN_1);
	}
#endif
	pause_message();
	keyclose();
	end=NOTHING;
	while(getkey());
	while(end==NOTHING) {
	  ch=getkey();
	  switch(tolower(ch)) {
	  case 'p':
	    if(easyrider!=9)
	      easyrider=0;
	    break;
	  case 't':
	    checkfork('m', 0);
	    checkfork('b', 1);
	    checkfork('z', 2);
	    checkfork('h', 3);
	    checkfork('s', 4);
	    checkfork('p', 5);
	    checkfork('v', 6);
	    checkfork('o', 7);
	    checkfork('e', 8);
	    break;
	  case 'c':
	  case 'q':
	  case 27:
	    end=PLAY;
	    break;
	  }
	  usleep(10000UL);
	}
	if(easyrider!=9)
	  easyrider=0;
	keyinit();
      }
      if(actionbits&escape_bit) {
#ifdef HAVE_SOUND
	if(play_sound) {
	  Thrust_Is_On=0;
	  Snd_effect(SND_ZERO, CHAN_1);
	}
#endif
	escape_message();
	keyclose();
	end=NOTHING;
	while(end==NOTHING) {
	  ch=getkey();
	  switch(tolower(ch)) {
	  case 'y':
	    end=END;
#ifdef DEBUG2
	    printf("Endlevel: User answered yes efter ESC.\n");
#endif
	    endlevel=1;
	    level=LEVELS;
	    break;
	  case 'n':
	    end=PLAY;
	    break;
	  }
	  usleep(10000UL);
	}
	keyinit();
      }
      if(alive && (actionbits&right_bit)) {
	decr(kdir, 0, 96);
	dir=kdir/3;
      }
      if(alive && (actionbits&left_bit)) {
	incr(kdir, 96, 0);
	dir=kdir/3;
      }
      if(alive && (actionbits&fire_bit)) {
	if(!shoot) {
	  shoot=1;
	  newbullet(x+((160+shipdx)<<3)+64*cos(dir * M_PI/16),
		    y+(( 88+shipdy)<<3)-64*sin(dir * M_PI/16),
		    speedx/256.0+32*cos(dir * M_PI/16),
		    speedy/256.0+32*sin(dir * M_PI/16),
		    kdir/6, 1);
	}
	else if(repetetive || easyrider)
	  shoot=0;
      }
      else
	shoot=0;
      refueling=0;
      if(alive && (actionbits&pickup_bit)) {
	if(fuel>0) {
	  if(shield++==3) {
#if !(defined(DEBUG) || defined(DEBUG2))
	    if(!easyrider)
	      fuel--;
#endif
	    shield=1;
	  }
	}
	else
	  shield=0;
	l=closestfuel((pixx+shipdx+160)%lenx3,
		      (pixy+shipdy+88)%leny3);
	if(l>=0)
	  if(resonablefuel((pixx+shipdx+160)%lenx3,
			   (pixy+shipdy+88)%leny3, l)) {
#ifndef DEBUG
	    if(!easyrider)
	      fuel+=6;
#endif
	    refueling=1;
	    things[l].alive--;
	    if(things[l].alive==1)
	      things[l].score=300;
	  }
	if(!loaded)
	  if(inloadcontact((pixx+shipdx+160)%lenx3,
			   (pixy+shipdy+88)%leny3)) {
	    loadcontact=1;
	    *(bana+lenx*loadby+loadbx)=32;
	    drawload(0);
	  }
      }
      else {
	shield=0;
	if(alive && loadcontact) {
	  *(bana+lenx*loadby+loadbx)=109;
	  drawload(1);
	  loadcontact=0;
	}
      }
      if(alive && (actionbits&thrust_bit)) {
	if(fuel>0) {
#ifdef HAVE_SOUND
	  if(play_sound)
	    if(Thrust_Is_On==0) {
	      Thrust_Is_On=1;
	      Snd_effect(SND_THRUST, CHAN_1);
	    }
#endif
#if !(defined(DEBUG) || defined(DEBUG2))
	  if(!easyrider)
	    fuel--;
#endif
	  oldabs=speedx*(long)speedx+speedy*(long)speedy;

	  if(loaded) { /* Ship and blob */
	    aradial  = cos(dir * M_PI/16 - alpha);
	    acircum  = sin(dir * M_PI/16 - alpha);
	    ax =
	      SPEED/2
	      * (aradial*cos(alpha) - acircum*sin(alpha)/(1 + REL_MASS))
	      / (1 + REL_MASS);
	    ay =
	      SPEED/2
	      * (aradial*sin(alpha) - acircum*cos(alpha)/(1 + REL_MASS))
	      / (1 + REL_MASS);
	    deltaalpha += SPEED/2 * acircum * M_PI/262144;
	  }
	  else {       /* Ship, no blob */
	    ax=SPEED*cos(dir * M_PI/16) / 2;
	    ay=SPEED*sin(dir * M_PI/16) / 2;
	  }

	  speedx+=ax;
	  speedy+=ay;
	  absspeed=speedx*(long)speedx+speedy*(long)speedy;
	  if(absspeed>1000000000L && absspeed>oldabs) {
	    speedx-=ax;
	    speedy-=ay;
	  }
	}
	else {
#ifdef HAVE_SOUND
	  if(play_sound)
	    if(Thrust_Is_On == 1) {
	      Snd_effect(SND_ZERO, CHAN_1);
	      Thrust_Is_On = 0;
	    }
#endif
	}
      }
      else {
#ifdef HAVE_SOUND
	if(play_sound)
	  if(Thrust_Is_On == 1) {
	    Snd_effect(SND_ZERO, CHAN_1);
	    Thrust_Is_On = 0;
	  }
#endif
      }
      if(loaded) {
	if(loadpointshift) {
	  speedx+=shipdx*12;
	  speedy+=shipdy*12;
	}
	alpha+=deltaalpha;
	if(alpha>2*M_PI)
	  alpha-=2*M_PI;
	if(alpha<0)
	  alpha+=2*M_PI;
	loadpointshift=0;
	if(++loadpoint>126)
	  loadpoint=126;
	else
	  loadpointshift=1;
	shipdx= cos(alpha)*loadpoint/5.90625;
	shipdy=-sin(alpha)*loadpoint/5.90625;
	if(loadpointshift) {
	  speedx-=shipdx*12;
	  speedy-=shipdy*12;
	}
	deltaalpha-=deltaalpha/1024;
      }
      else
	shipdx=shipdy=0;
      /* Gravity and Aerodynamics */
      if(speedx>0)
	speedx=speedx-(speedx>>9)-1;
      else if(speedx<0)
	speedx=speedx-(speedx>>9)+1;
      if(alive) {
	if(gravity>=0)
	  speedy-=(SPEED*gravity+1)>>8;
	else
	  speedy-=(SPEED*gravity>>8)+1;
	if(speedy>0)
	  speedy--;
	else if(speedy<0)
	  speedy++;
	/* Move the Ship */
	speedx=min(speedx, 16384);
	speedx=max(speedx, -16384);
	speedy=min(speedy, 16384);
	speedy=max(speedy, -16384);
	if(speedx>=0)
	  vx=(speedx+1)>>8;
	else
	  vx=(speedx>>8)+1;
	if(speedy>=0)
	  vy=(speedy+1)>>8;
	else
	  vy=(speedy>>8)+1;
	x=(x+vx+(lenx<<6))%(lenx<<6);
	y=(y-vy+(leny<<6))%(leny<<6);
      }

      /* Bunkerfire */
      if(!ssblip)
	bunkerfirebullets();
      movebullets();
      movefragments();
      drawfuel(fuel);

      /* Move the Spacestationblip */
      scount=(scount+1)&15;
      if(!scount && spacestation && ssblip)
	ssblip--;

      if(!spacestation) {
	countdown--;
	if(countdown<0) {
#ifndef DEBUG
	  if(alive && !easyrider) {
	    dying=1;
#ifdef DEBUG2
	    printf("Dying: Spacestation countdown.\n");
#endif
	  }
#endif
	}
	else {
	  chflag=1;
	  if(countdown&16)
	    chcolor=0;
	  else
	    chcolor=TEXTCOLOR;
	  sprintf(textstr, "%d  ", (countdown+99)/100);
	  printgs(155, 180, textstr);
	  chcolor=TEXTCOLOR;
	  chflag=0;
	}
      }

      /* Precalculate some values */
      pixx=x>>3;
      pixy=y>>3;
      bildx=(pixx+PBILDX-4)%PBILDX+4;
      bildy=pixy%PBILDY;
      pblockx=pixx>>3;
      pblocky=pixy>>3;
      bblockx=bildx>>3;
      bblocky=bildy>>3;
      if(wrapers)
	if(pblocky>BBILDY && pblocky<2*BBILDY) {
	  y-=PBILDY<<3;
	  pixy-=PBILDY;
	  pblocky-=BBILDY;
	  unwrapbullets();
	  unwrapfragments();
	  wrapers--;
	}
      if(pblocky>leny-3) {
	/*	if(loaded) {*/
	  endlevel=1;
#ifdef DEBUG2
	  printf("Endlevel: Finished level.\n");
#endif
	  teleport=1;
	  /*	}*/
	y+=(PBILDY-leny3)<<3;
	pixy+=PBILDY-leny3;
	pblocky+=BBILDY-leny;
	wrapbullets();
	wrapfragments();
	if(!++wrapers)
	  wrapers--;
      }

      /* Check if at a restart barrier. If so, update the restart point. */
      restartxy=atbarrier((pblockx+((154+shipdx)>>3))%lenx,
			  pblocky+((82+shipdy)>>3));
      if(restartxy) {
	restartx=(lenx+restartxy->x-(154>>3))%lenx;
	restarty=restartxy->y-(82>>3);
	loadedrestart=loaded;
      }

      /* Scroll the screen */
      setmargin(253, 1);
      updateborder(pblockx, pblocky, bblockx, bblocky, vx, vy);
      
      drawspacestationblip();
      setmargin(255, 1);
      drawbullets();
      if(alive)
	crash=drawshuttle();
      drawfragments();
      if(alive && refueling)
	drawfuellines();
      /* Check if end of life. */
#ifndef DEBUG
      if(!easyrider)
	if(alive && crash) {
	  lives--;
	  dying=1;
#ifdef DEBUG2
	  printf("Dying: Crashing.\n");
#endif
	}
#endif
      /* Wait for the screen retrace and then dump the graphics to it. */
      /* Screendump */

/*	if(keyboard_keypressed(63)) *//* F5 */
/*	  savegraphics(bildx, bildy);*/
      syncscreen();
      putscr(bildx, bildy);

      /* Remove moveable objects from screen in reverse order. */
      if(alive && refueling)
	undrawfuellines();
      setmargin(120, 1);
      undrawfragments();
      if(alive)
	undrawshuttle();
      undrawbullets();
	
      /* Remove objects */
      if(!easyrider)
	localscore+=killdyingthings();
      else
	killdyingthings();
      if(dying) {
	alive=0;
	dying=0;
#ifdef HAVE_SOUND
	if(play_sound) {
	  Snd_effect(SND_ZERO, CHAN_1);
	  Thrust_Is_On=0;
	  Snd_effect(SND_BOOM2, CHAN_2);
	}
#endif
	explodeship();
      }
      if(!alive && !livefragments()) {
#ifdef HAVE_SOUND
	if(play_sound) {
	  Snd_effect(SND_ZERO, CHAN_1);
	  Thrust_Is_On=0;
	}
#endif
	endlevel=1;
#ifdef DEBUG2
	printf("Endlevel: Shit crashed.\n");
#endif
      }
      animatesliders();
      if(localscore>score) {
	chflag=1;
	if(localscore/10000 > score/10000) {
	  lives++;
	  sprintf(textstr, "LIVES: %d", lives);
	  printgs(20, 192, textstr);
	}
	score=localscore;
	sprintf(textstr, "SCORE: %d         ", score);
	printgs(20, 184, textstr);
	chflag=0;	
      }
    }
    if(teleport) {
#ifdef HAVE_SOUND
      if(play_sound)
	Snd_effect(SND_ZERO, CHAN_1);
#endif
      bin_colors[65*3+0]=colorr;
      bin_colors[65*3+1]=colorg;
      bin_colors[65*3+2]=colorb;
      fadepalette(0, 255, bin_colors, 64, 1);
      drawteleport();
    }

    if(!(actionbits&(quit_bit|escape_bit)))
      usleep(1000000UL);
    fade_out();

    if(!demo && !(actionbits&(quit_bit|escape_bit))) {
      if(teleport || !spacestation) {
	chflag=1;
	printgs(250, 184, "FUEL");
	drawfuel(fuel);
	sprintf(textstr, "LIVES: %d", lives);
	printgs(20, 192, textstr);
	sprintf(textstr, "SCORE: %d         ", score);
	printgs(20, 184, textstr);

	if(!spacestation) {
	  sprintf(textstr, "PLANET DESTROYED");
	  gcenter(61, textstr);
	}

	if(teleport && loaded)
	  sprintf(textstr, "MISSION %d COMPLETE", level+1);
	else if(spacestation)
	  sprintf(textstr, "MISSION INCOMPLETE");
	else
	  sprintf(textstr, "MISSION  %d  FAILED", level+1);
	gcenter(73-6*(teleport && loaded && spacestation), textstr);

	if((teleport && loaded) || !spacestation) {
	  if(teleport && loaded)
	    sprintf(textstr, "BONUS %d", 4000+400*level-2000*spacestation);
	  else
	    sprintf(textstr, "NO BONUS");
	  gcenter(85-6*(!!spacestation), textstr);
	}

	displayscreen();
	fade_in();
	usleep(2000000UL);
	if(!easyrider && teleport && loaded)
	  localscore+=4000+400*level-2000*spacestation;
	if((teleport && loaded) || !spacestation) {
	  if(++level==LEVELS) {
	    level=0;
	    round=(round+1)%4;
	  }
	}
	if(localscore/10000 > score/10000) {
	  lives++;
	  sprintf(textstr, "LIVES: %d", lives);
	  printgs(20, 192, textstr);
	}
	score=localscore;
	sprintf(textstr, "SCORE: %d         ", score);
	printgs(20, 184, textstr);
	chflag=0;
	displayscreen();
	usleep(2000000UL);
	fade_out();
      }
    }
    teleport=0;

    if(demo)
      level=LEVELS;
  }

  if(!demo) {
    chflag=1;
    printgs(250, 184, "FUEL");
    drawfuel(fuel);
    sprintf(textstr, "LIVES: %d", lives);
    printgs(20, 192, textstr);
    sprintf(textstr, "SCORE: %d         ", score);
    printgs(20, 184, textstr);

    gcenter(73, "GAME OVER");

    displayscreen();
    fade_in();
    usleep(2000000UL);
    fade_out();
  }

  return(0);
}

void
pressanykey(void)
{
  keyclose();

  do 
    usleep(10000UL);
  while(!getkey());

  keyinit();
}

int
instructions(void)
{
  int i;
  static char *keys[] = {
    "ESC", "P", "C" };
  static char *func[] = {
    "TURN LEFT", "TURN RIGHT", "THRUST", "FIRE",
    "PICK UP & SHIELD", "QUIT GAME (Q=ESC)", "PAUSE", "CONTINUE" };

  gcenter(50, "THE FOLLOWING KEYS ARE USED:");
  for(i=0; i<8; i++) {
    chcolor = HIGHLIGHT;
    if(i<5)
      printgs(140-gstrlen(keystring(scancode[i])),
	      63+i*8+2*(i>4),
	      keystring(scancode[i]));
    else
      printgs(140-gstrlen(keys[i-5]), 63+i*8+2*(i>4), keys[i-5]);
    chcolor = TEXTCOLOR;
    printgs(145, 63+i*8+2*(i>4), func[i]);
  }
  gcenter(150, "PRESS ANY KEY FOR THE MAIN MENU.");

  fade_in();
  pressanykey();
  fade_out();

  return(0);
}

int
about(void)
{
  int i, j;
  char tmp[100];
  static char *str[] = {
    "THRUST VERSION " VERSION,
    "",
    "WRITTEN BY",
    "",
    "PETER EKBERG",
    "PEDA@LYSATOR.LIU.SE",
    "",
    "THANKS TO THE AUTHORS",
    "OF THE ORIGINAL",
    "FOR THE C64.",
    NULL
  };

  for(i=0; str[i]; i++) {
    for(j=0; j<strlen(str[i]); j++)
      tmp[j] = toupper(str[i][j]);
    tmp[j]='\0';
    if(i==5)
      chcolor = HIGHLIGHT;
    else
      chcolor = TEXTCOLOR;
    gcenter(40+9*i, tmp);
  }
  gcenter(145, "PRESS ANY KEY FOR MAIN MENU.");

  fade_in();
  pressanykey();
  fade_out();

  return(0);
}

char *
enterhighscorename(void)
{
  static char name[40];
  char str[40];
  
  strcpy(name, standardname());
  sprintf(str, "YOU MANAGED %d POINTS!", score);
  gcenter(64, str);
  gcenter(75, "YOU MADE IT INTO THE HIGHSCORE LIST!");
  gcenter(86, "ENTER YOUR NAME:");
  printgs(130, 97, name);
  fade_in();

  keyclose();

  if(readgs(130, 97, name, 39, 80, 0)==-1)
    strcpy(name, standardname());

  keyinit();

  fade_out();

  return(name);
}

int
showhighscores(void)
{
  char str[100];
  byte tmp=chcolor;
  int i;
  int scorew, namew;
  int len;

  gcenter(50, "THE CURRENT HIGHSCORES ARE");

  scorew=namew=0;
  for(i=0; i<HIGHSCORES; i++) {
    sprintf(str, "%d", highscorelist[i].score);
    len=gstrlen(str);
    if(len>scorew)
      scorew=len;
    len=gstrlen(highscorelist[i].name);
    if(len>namew)
      namew=len;
  }

  for(i=0; i<HIGHSCORES; i++) {
    sprintf(str, "%d", highscorelist[i].score);
    chcolor = SCORETEXT;
    printgs(155+(scorew-namew)/2-gstrlen(str), 70+11*i, str);
    chcolor = SCORENAME;
    printgs(165+(scorew-namew)/2, 70+11*i, highscorelist[i].name);
  }

  chcolor=tmp;
  gcenter(145, "PRESS ANY KEY FOR MAIN MENU.");

  fade_in();
  pressanykey();
  fade_out();

  return(0);
}

void
newhighscore(void)
{
  char *name;

  name = enterhighscorename();
  inserthighscore(name, score);
  writehighscores();
  showhighscores();
}

options
menu(void)
{
  int i;
  options end=NOTHING;
  int ch;
  static char *menuchoises[NOTHING]= { "I", "C", "H", "P", "A", "Q" };
  static char *menuoptions[NOTHING]= {
    "INSTRUCTIONS", "CONFIGURATION", "HIGHSCORES", "PLAY GAME", "ABOUT", "QUIT"};
  int count=0;

  clearscr();
  for(i=0; i<title_rows; i++)
    puthline(title_pixels+i*title_cols,
	     (PUSEX-title_cols)>>1,
	     ((PUSEY+24-title_rows)>>1)+i,
	     title_cols);
  for(i=0; i<NOTHING; i++) {
    chcolor = TEXTCOLOR;
    printgs(20, 134+i*11, menuoptions[i]);
    chcolor = HIGHLIGHT;
    printgs(20, 134+i*11, menuchoises[i]);
  }
  chcolor = TEXTCOLOR;

  fade_in();
  keyclose();

  while(end==NOTHING) {
    ch=getkey();
    switch(tolower(ch)) {
    case 0:
      break;
    case 'i':
      end=INST;
      break;
    case 'p':
      end=PLAY;
      break;
    case 'h':
      end=HI;
      break;
    case 'a':
      end=ABOUT;
      break;
    case 'c':
      end=CONF;
      break;
    case 'd':
      end=DEMO;
      break;
    case 'q':
    case 27:
      end=END;
      break;
    default:
      count=0;
      break;
    }
    usleep(10000UL);

    if(++count==800 && end==NOTHING) {
      if(!nodemo)
	end=DEMO;
      count=0;
    }
  }

  keyinit();
  fade_out();

  return(end);
}

int
main(int argc, char **argv)
{
  int end=0;
  int optc;

  do {
    static struct option longopts[] = {
      OPTS,
      SVGA_OPTS,
      X_OPTS,
      { 0, 0, 0, 0 }
    };

    optc=getopt_long_only(argc, argv,
			  OPTC SVGA_OPTC X_OPTC, longopts, (int *)0);
    switch(optc) {
    case EOF:
    case 's':      /* --svgamode */
    case 'm':      /* --noshm */
    case 'X':      /* -display */
    case 'g':      /* -geometry */
      break;
    case 'e':      /* --nosoundeffects */
      play_sound=0;
      break;
    case 'd':      /* --nodemo */
      nodemo=1;
      break;
    case 'c':      /* --gamma */
      {
	char *end;
	double tmp;

	if(*optarg) {
	  tmp = strtod(optarg, &end);
	  if(*end || tmp==HUGE_VAL || tmp<=0.0)
	    tmp = 0.0;
	}
	else
	  tmp = 0.0;

	if(tmp == 0.0)
	  printf("Illegal gamma correction value: \"%s\"\n", optarg);
	else
	  gamma_correction = tmp;
      }
      break;
    case 'h':      /* --help */
      printf("Thrust: version " VERSION " -- the Game\n");
      printf("usage: thrust [OPTION]...\n\n"
	     "  -v, --version\n"
	     "  -h, --help\n"
	     "  -d, --nodemo           Do not run the demo.\n"
	     "  -e, --nosoundeffects   Do not play sound effects.\n"
	     "  -c, --gamma=Value      Gamma correction of colors.\n"
	     "  -s, --svgamode=MODE    The format of MODE is G<width>x<height>x<colors>\n"
	     "  -m, --noshm            Do not use shared memory.\n"
	     "  -display display-name  See the X man page for details.\n"
	     "  -geometry geom-spec    See the X man page for details.\n");
      exit(1);
    case 'v':      /* --version */
      printf("Thrust: version " VERSION "\n");
      exit(0);
    default:
      fprintf(stderr, "Thrust: bad usage (see 'thrust -h')\n");
      exit(1);
    }
  } while(optc != EOF);

  graphics_preinit();
  inithardware(argc, argv);

  if(!initmem()) {
    restorehardware();
    return(1);
  }
  inithighscorelist();
  initkeys();

  sleep(1);

  while(!end) {
    switch(menu()) {
    case INST:
      instructions();
      break;
    case PLAY:
      if(!(end=game(0)))
	if(ahighscore(score))
	  newhighscore();
      break;
    case HI:
      showhighscores();
      break;
    case ABOUT:
      about();
      break;
    case CONF:
      conf();
      break;
    case DEMO:
      game(1);
      break;
    case END:
      end=1;
      break;
    default:
      break;
    }
  }

  restoremem();
  restorehardware();
  
  return(0);
}
