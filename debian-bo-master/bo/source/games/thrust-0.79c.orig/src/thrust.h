
/* Written by Peter Ekberg, peda@lysator.liu.se */

#ifndef THRUST_H
#define THRUST_H

/* DEBUG  will make you undestructible.
   DEBUG2 gives you some trace output. */

/*#define DEBUG*/
/*#define DEBUG2*/

/* The speed of the game. Gravity and thrust is affected.
   Don't try too large values. 256 is what it used to
   be in the good old days. */

#define SPEED 180

/* The relative mass of the blob. I.e relative to the ship.
   Higher values makes it harder to lift.
   Try negative values at your own risk. */

#define REL_MASS 1.1


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

/* How many levels and what they are named. */

#define LEVELS (6)
extern char *level1[];
extern char *level2[];
extern char *level3[];
extern char *level4[];
extern char *level5[];
extern char *level6[];


#ifdef HAVE_SOUND
extern unsigned char sound_boom[];
extern unsigned char sound_boom2[];
extern unsigned char sound_harp[];
extern unsigned char sound_thrust[];
extern unsigned char sound_zero[];
extern unsigned int  sound_boom_len;
extern unsigned int  sound_boom2_len;
extern unsigned int  sound_harp_len;
extern unsigned int  sound_thrust_len;
extern unsigned int  sound_zero_len;
#endif

#include "thrust_types.h"

typedef enum { INST, PLAY, HI, ABOUT, END, CONF, NOTHING, DEMO } options;

extern unsigned int title_nr_colors;
extern unsigned int title_cols, title_rows;
extern unsigned char title_colors[];
extern unsigned char title_pixels[];
extern unsigned char bin_blocks[];
extern unsigned char bin_ship[];
extern unsigned char bin_shield[];
extern unsigned char bin_colors[];
extern unsigned char bin_bullets[];
extern unsigned char bin_demomoves[];

extern byte *bulletmap;
extern byte *blocks;
extern byte *ship;
extern byte *shieldship;
extern byte *bana;
extern byte *fuelmap;
extern byte *loadmap;
extern byte *shipstorage;
extern byte *bulletstorage;
extern byte *fragmentstorage;
extern byte *fuelstorage;
extern byte *loadstorage;
extern byte *wirestorage;

extern word lenx; /* Banans max i x-len, används senare till den aktuella */
extern word leny; /* Banans max i y-len, storleken på banan.Sätts i readbana */
extern word lenx3,leny3;

extern double alpha,deltaalpha;
extern word loaded,loadcontact,loadpointshift;
extern int loadpoint;
extern int countdown;
extern word crash,shoot,repetetive;
extern word refueling;
extern int speedx,speedy;
extern long absspeed,oldabs;
extern int kdir,dir;
extern int shipdx,shipdy;
extern int x,y;
extern int pixx,pixy;
extern int pblockx,pblocky;
extern int vx,vy;
extern int bildx,bildy;
extern int bblockx,bblocky;
extern int loadbx,loadby;
extern int gravity;
extern int score;
extern byte shield;
extern byte colorr, colorg, colorb;
extern int play_sound;
extern double gamma_correction;

#define GAMMA(x)  ((int)(255*pow((x)/255.0, 1/gamma_correction)))
#define TEXTCOLOR (32)
#define HIGHLIGHT (33)
#define FUELCOLOR (34)
#define SCORETEXT (35)
#define SCORENAME (36)

#ifdef __STDC__
int insideblock(int blockx, int blocky,
		int pblockx, int pblocky, int sx, int sy);
int insidepixel(int x, int y, int pixx, int pixy, int sx, int sy);
void updateborder(int pblockx, int pblocky,
		  int bblockx, int bblocky, int vx, int vy);
void fade_in(void);
void fade_out(void);
void pause_message(void);
void escape_message(void);
byte whatkeys(void);
byte nextmove(int reset);
int game(int demo);
void pressanykey(void);
int instructions(void);
int about(void);
char *enterhighscorename(void);
int showhighscores(void);
void newhighscore(void);
options menu(void);
#endif

#endif /* THRUST_H */
