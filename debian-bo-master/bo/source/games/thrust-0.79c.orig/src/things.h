
/* Written by Peter Ekberg, peda@lysator.liu.se */

#ifndef THINGS_H
#define THINGS_H

#include "thrust_types.h"

struct bulletdef {
  int life,x,y,vx,vy,dir,owner;
};
typedef struct bulletdef bullet;

struct fragmentdef {
  int life,x,y,vx,vy;
};
typedef struct fragmentdef fragment;

struct thingdef	{
  int alive,px,py,x,y,type,score;
  void *data;
};
typedef struct thingdef thing;
extern word nrthings;

struct sliderdef {
  int type,x1,y1,x2,y2,dir,active,stage,count,match;
  struct sliderdef *next;
};
typedef struct sliderdef slider;
extern word nrsliders;

struct buttondatadef {
  int major;
  int tag;
  slider *sliders;
};
typedef struct buttondatadef buttondata;

struct restartpointdef {
  word x, y;
};
typedef struct restartpointdef restartpoint;
extern word nrrestartpoints;

struct barrierdef {
  word x, y;
  restartpoint *restart;
};
typedef struct barrierdef barrier;
extern word nrbarriers;


#define maxbullets (64)
extern bullet bullets[maxbullets];
#define maxfragments (512)
extern fragment fragments[maxfragments];
#define maxthings (32)
extern thing things[maxthings];
#define maxsliders (32)
extern slider sliders[maxsliders];
#define maxbarriers (512)
extern barrier barriers[maxbarriers];
#define maxrestartpoints (16)
extern restartpoint restartpoints[maxrestartpoints];

extern word spacestation;
extern word ssx,ssy,scount;	/* Spacestation-variables */
extern word ssblip;

#ifdef __STDC__
void newslider(int x, int y, int type);
int majorbutton(int button);
void newthing(int x, int y, int px, int py, int type, void *data);
void animatesliders(void);
void startupsliders(int button);
restartpoint *atbarrier(word bx, word by);
void deletething(thing *tp, int dx, int dy, int sx, int sy);
void newbullet(word x, word y, int vx, int vy, word dir, int owner);
void wrapbullets(void);
void unwrapbullets(void);
void movebullets(void);
word crashtype(word type);
int inloadcontact(int x, int y);
int resonablefuel(int x, int y, int l);
int closestfuel(int x, int y);
int closestbutton(int x, int y);
void hit(word x, word y, word crash, word owner);
void bunkerfirebullet(thing *b);
void bunkerfirebullets(void);
int killdyingthings(void);
void newfragment(word x, word y);
void explodething(thing *thingptr);
void explodeship(void);
void wrapfragments(void);
void unwrapfragments(void);
void movefragments(void);
word livefragments(void);
#endif

#endif /* THINGS_H */
