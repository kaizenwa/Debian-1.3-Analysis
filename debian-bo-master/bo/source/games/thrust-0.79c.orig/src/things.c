
/* Written by Peter Ekberg, peda@lysator.liu.se */

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef HAVE_VALUES_H
# include <values.h>
#else
# include <limits.h>
# define MAXINT  INT_MAX
# define MAXLONG LONG_MAX
#endif

#include <stdlib.h>
#include <math.h>

#include "thrust_types.h"
#include "things.h"
#include "fast_gr.h"
#include "graphics.h"
#include "thrust.h"

#ifdef HAVE_SOUND
# include "soundIt.h"

# define CHAN_1 0
# define CHAN_2 1
# define CHAN_3 2
# define CHAN_4 3

# define SND_BOOM   0
# define SND_BOOM2  1
# define SND_HARP   2
# define SND_THRUST 3
# define SND_ZERO   4
#endif

word nrthings=0;
word nrsliders=0;
word nrbarriers=0;
word nrrestartpoints=0;

bullet bullets[maxbullets];
fragment fragments[maxfragments];
thing things[maxthings];
slider sliders[maxsliders];
barrier barriers[maxbarriers];
restartpoint restartpoints[maxrestartpoints];

word spacestation;
word ssx,ssy,scount;	/* Spacestation-variables */
word ssblip;

void
newslider(x, y, type)
     int x, y, type;
{
  sliders[nrsliders].x1=x;
  sliders[nrsliders].y1=y;
  sliders[nrsliders].type=type;
  sliders[nrsliders].dir=type%3;
  sliders[nrsliders].match=0;
  sliders[nrsliders].active=0;
  sliders[nrsliders++].next=NULL;
}

int
majorbutton(int button)
{
  if(((buttondata *)things[button].data)->major==-1)
    return(button);
  else
    return(((buttondata *)things[button].data)->major);
}

void
newthing(int x, int y, int px, int py, int type, void *data)
{
  int life;

  switch(type) {
  case 1:       /* Fuel */
    life=140;
    break;
  default:
    life=2;
  }
  things[nrthings].alive=life;
  things[nrthings].x=x;
  things[nrthings].y=y;
  things[nrthings].px=px;
  things[nrthings].py=py;
  things[nrthings].type=type;
  things[nrthings].data=data;
  things[nrthings++].score=0;
}

void
animatesliders(void)
{
  slider *s=sliders;
  int i;

  for(i=0; i<nrsliders; i++, s++)
    if(s->active) {
      switch(s->stage) {
      case 0:
	if(!(s->count&7)) {
	  switch(s->type) {
	  case 2:
	    if(s->count==0) {
	      writeblock(s->x1-(s->count>>3),   s->y1, 'x');
	      writeblock(s->x1-(s->count>>3)-1, s->y1, 136);
	      writeblock(s->x1-(s->count>>3)-2, s->y1, 'q');
	    }
	    else if(s->count==8) {
	      writeblock(s->x1-(s->count>>3),   s->y1, 'w');
	      writeblock(s->x1-(s->count>>3)-1, s->y1, 'r');
	      writeblock(s->x1-(s->count>>3)-2, s->y1, 'q');
	    }
	    else {
	      writeblock(s->x1-(s->count>>3),   s->y1, ' ');
	      writeblock(s->x1-(s->count>>3)-1, s->y1, 'r');
	      writeblock(s->x1-(s->count>>3)-2, s->y1, 'q');
	    }
	    break;
	  case 5:
	    if(s->count==0) {
	      writeblock(s->x1-(s->count>>3)  , s->y1, 138);
	      writeblock(s->x1-(s->count>>3)-1, s->y1, 139);
	    }
	    else if(s->count==8) {
	      writeblock(s->x1-(s->count>>3)  , s->y1, 141);
	      writeblock(s->x1-(s->count>>3)-1, s->y1, 134);
	    }
	    else {
	      writeblock(s->x1-(s->count>>3)  , s->y1, 135);
	      writeblock(s->x1-(s->count>>3)-1, s->y1, 134);
	    }
	    break;
	  case 7:
	    writeblock(s->x1+(s->count>>3), s->y1, 'y');
	    break;
	  case 8:
	    writeblock(s->x1-(s->count>>3), s->y1, '|');
	    break;
	  case 10:
	    writeblock(s->x1, s->y1+(s->count>>3), 129);
	    break;
	  case 11:
	    writeblock(s->x1, s->y1-(s->count>>3), 128);
	    break;
	  }
	}
	else if(!(s->count&3)) {
	  switch(s->type) {
	  case 2:
	    if(s->count==4) {
	      writeblock(s->x1-(s->count>>3)-1, s->y1, 137);
	      writeblock(s->x1-(s->count>>3)-2, s->y1, 'n');
	    }
	    else {
	      writeblock(s->x1-(s->count>>3)-1, s->y1, 'o');
	      writeblock(s->x1-(s->count>>3)-2, s->y1, 'n');
	    }
	    break;
	  case 5:
	    if(s->count==4) {
	      writeblock(s->x1-(s->count>>3),   s->y1, 133);
	      writeblock(s->x1-(s->count>>3)-1, s->y1, 140);
	      writeblock(s->x1-(s->count>>3)-2, s->y1, 'u');
	    }
	    else if(s->count==12) {
	      writeblock(s->x1-(s->count>>3),   s->y1, 132);
	      writeblock(s->x1-(s->count>>3)-1, s->y1, 'v');
	      writeblock(s->x1-(s->count>>3)-2, s->y1, 'u');
	    }
	    else {
	      writeblock(s->x1-(s->count>>3),   s->y1, ' ');
	      writeblock(s->x1-(s->count>>3)-1, s->y1, 'v');
	      writeblock(s->x1-(s->count>>3)-2, s->y1, 'u');
	    }
	    break;
	  case 7:
	    writeblock(s->x1+(s->count>>3), s->y1, ' ');
	    break;
	  case 8:
	    writeblock(s->x1-(s->count>>3), s->y1, ' ');
	    break;
	  case 10:
	    writeblock(s->x1, s->y1+(s->count>>3), ' ');
	    break;
	  case 11:
	    writeblock(s->x1, s->y1-(s->count>>3), ' ');
	    break;
	  }
	}
	s->count++;
	if(s->count >= ((abs(s->x1-s->x2)+abs(s->y1-s->y2)+1) << 3)-1) {
	  s->count=0;
	  s->stage=1;
	}
	break;
      case 1:
	s->count++;
	if(s->count>500) {
	  s->count=0;
	  s->stage=2;
	}
	break;
      case 2:
	if(!(s->count&7)) {
	  switch(s->type) {
	  case 2:
	    if(s->count>>3==s->x1-s->x2) {
	      writeblock(s->x2+(s->count>>3)-1, s->y1, 136);
	      writeblock(s->x2+(s->count>>3)-2, s->y1, 'q');
	    }
	    else {
	      writeblock(s->x2+(s->count>>3)-1, s->y1, 'r');
	      writeblock(s->x2+(s->count>>3)-2, s->y1, 'q');
	    }
	    break;
	  case 5:
	    if(s->count>>3==s->x1-s->x2) {
	      writeblock(s->x2+(s->count>>3)  , s->y1, 138);
	      writeblock(s->x2+(s->count>>3)-1, s->y1, 139);
	      writeblock(s->x2+(s->count>>3)-2, s->y1, 'p');
	    }
	    else if(s->count>>3==s->x1-s->x2-1) {
	      writeblock(s->x2+(s->count>>3)  , s->y1, 141);
	      writeblock(s->x2+(s->count>>3)-1, s->y1, 134);
	      writeblock(s->x2+(s->count>>3)-2, s->y1, 'p');
	    }
	    else {
	      writeblock(s->x2+(s->count>>3)  , s->y1, 135);
	      writeblock(s->x2+(s->count>>3)-1, s->y1, 134);
	      writeblock(s->x2+(s->count>>3)-2, s->y1, 'p');
	    }
	    break;
	  case 7:
	    writeblock(s->x2-(s->count>>3), s->y1, 'y');
	    break;
	  case 8:
	    writeblock(s->x2+(s->count>>3), s->y1, '|');
	    break;
	  case 10:
	    writeblock(s->x1, s->y2-(s->count>>3), 129);
	    break;
	  case 11:
	    writeblock(s->x1, s->y2+(s->count>>3), 128);
	    break;
	  }
	}
	else if(!(s->count&3)) {
	  switch(s->type) {
	  case 2:
	    if(s->count>>3==s->x1-s->x2) {
	      writeblock(s->x2+(s->count>>3)  , s->y1, 'p');
	      writeblock(s->x2+(s->count>>3)-1, s->y1, 'p');
	      writeblock(s->x2+(s->count>>3)-2, s->y1, 'p');
	    }
	    else if(s->count>>3==s->x1-s->x2-1) {
	      writeblock(s->x2+(s->count>>3)  , s->y1, 137);
	      writeblock(s->x2+(s->count>>3)-1, s->y1, 'n');
	      writeblock(s->x2+(s->count>>3)-2, s->y1, 'p');
	    }
	    else {
	      writeblock(s->x2+(s->count>>3)  , s->y1, 'o');
	      writeblock(s->x2+(s->count>>3)-1, s->y1, 'n');
	      writeblock(s->x2+(s->count>>3)-2, s->y1, 'p');
	    }
	    break;
	  case 5:
	    if(s->count>>3==s->x1-s->x2) {
	      writeblock(s->x2+(s->count>>3)  , s->y1, 'p');
	      writeblock(s->x2+(s->count>>3)-1, s->y1, 'p');
	    }
	    else if(s->count>>3==s->x1-s->x2-1) {
	      writeblock(s->x2+(s->count>>3)  , s->y1, 140);
	      writeblock(s->x2+(s->count>>3)-1, s->y1, 'u');
	    }
	    else {
	      writeblock(s->x2+(s->count>>3)  , s->y1, 'v');
	      writeblock(s->x2+(s->count>>3)-1, s->y1, 'u');
	    }
	    break;
	  case 7:
	    writeblock(s->x2-(s->count>>3), s->y1, 'p');
	    break;
	  case 8:
	    writeblock(s->x2+(s->count>>3), s->y1, 'p');
	    break;
	  case 10:
	    writeblock(s->x1, s->y2-(s->count>>3), 'p');
	    break;
	  case 11:
	    writeblock(s->x1, s->y2+(s->count>>3), 'p');
	    break;
	  }
	}
	s->count++;
	if(s->count >= ((abs(s->x1-s->x2)+abs(s->y1-s->y2)+1) << 3)-1)
	  s->active=0;
	break;
      }
    }
}

void
startupsliders(int button)
{
  slider *s;

  s=((buttondata *)things[button].data)->sliders;

  while(s) {
    if(s->active) {
      switch(s->stage) {
      case 1:
	s->count=0;
	break;
      case 2:
	s->count=(((abs(s->x1-s->x2)+abs(s->y1-s->y2)+1) << 3)-1) - s->count-7;
	if(s->count<0)
	  s->count=0;
	s->stage=0;
	break;
      default:
	break;
      }
    }
    else {
#ifdef HAVE_SOUND
      if(play_sound)
	Snd_effect(SND_HARP, CHAN_3);
#endif
      s->active=1;
      s->stage=0;
      s->count=0;
    }
    s=s->next;
  }
}

restartpoint *
atbarrier(word bx, word by)
{
  int i;

  for(i=0; i<nrbarriers; i++)
    if((barriers[i].x==bx) && (barriers[i].y==by))
      return(barriers[i].restart);
  return(NULL);
}

void
deletething(thing *tp, int dx, int dy, int sx, int sy)
{
  int tx, ty, i, j;

  tx=(*tp).px;
  ty=(*tp).py;
  (*tp).alive=0;

  switch((*tp).type)
    {
    case 1:		/* Fuel */
      for(i=0; i<2; i++)
	for(j=0; j<2; j++)
	  writeblock(tx+i, ty+j, 32);
      break;
    case 2:		/* Spacestation */
      spacestation=0;
      countdown=1000;
      for(i=0; i<3; i++)
	for(j=0; j<3; j++)
	  writeblock(tx+i, ty+j, 32);
      break;
    case 3:             /* Bunkers */
      writeblock(tx,   ty,   32);
      writeblock(tx+1, ty,   32);
      writeblock(tx+2, ty,   32);
      writeblock(tx+2, ty+1, 32);
      break;
    case 4:
      writeblock(tx,   ty+1, 32);
      writeblock(tx,   ty,   32);
      writeblock(tx+1, ty,   32);
      writeblock(tx+2, ty,   32);
      break;
    case 5:
      writeblock(tx,   ty+1, 32);
      writeblock(tx+1, ty+1, 32);
      writeblock(tx+2, ty+1, 32);
      writeblock(tx+2, ty,   32);
      break;
    case 6:
      writeblock(tx,   ty,   32);
      writeblock(tx,   ty+1, 32);
      writeblock(tx+1, ty+1, 32);
      writeblock(tx+2, ty+1, 32);
      break;
    }
}

void
newbullet(word x, word y, int vx, int vy, word dir, int owner)
{
  static word nr=0;

#ifdef HAVE_SOUND
  if(play_sound)
    Snd_effect(SND_BOOM, CHAN_2);
#endif
  bullets[nr].life=60;
  bullets[nr].x=x;
  bullets[nr].y=y;
  bullets[nr].vx=vx;
  bullets[nr].vy=vy;
  bullets[nr].dir=dir;
  bullets[nr++].owner=owner;

  if(nr==maxbullets)
    nr=0;
}

void
wrapbullets(void)
{
  bullet *bulletptr;
  int l;

  for(l=0, bulletptr=bullets; l<maxbullets; l++, bulletptr++)
    if((*bulletptr).life)
      (*bulletptr).y+=(PBILDY-leny3)<<3;
}

void
unwrapbullets(void)
{
  bullet *bulletptr;
  int l;

  for(l=0, bulletptr=bullets; l<maxbullets; l++, bulletptr++)
    if((*bulletptr).life)
      (*bulletptr).y-=PBILDY<<3;
}

void
movebullets(void)
{
  int l;
  bullet *bulletptr;

  for(l=0, bulletptr=bullets; l<maxbullets; l++, bulletptr++)
    if((*bulletptr).life) {
      (*bulletptr).life--;
      (*bulletptr).x=((*bulletptr).x+(lenx<<6)+(*bulletptr).vx) % (lenx<<6);
      (*bulletptr).y=((*bulletptr).y+(leny<<6)-(*bulletptr).vy) % (leny<<6);
    }
}

word
crashtype(word type)
{
  switch(type) {
  case 1:      /* Fuel */
  case 2:      /* Spacestation */
  case 3:      /* 3-6 Bunkers */
  case 4:
  case 5:
  case 6:
  case 7:      /* 7-8 Buttons */
  case 8:
    return(4);
  }
  return(0);
}

int
inloadcontact(int x, int y)
{
  int dx, dy;
  int dist;
  int res=0;
  double angle;
  double sp, spr;

  dx=x-(loadbx<<3)-2;
  dy=(loadby<<3)-y+2;
  dist=dx*dx+dy*dy;
  if(dist<1024) {
    res=1-loadcontact;
  }
  else if(dist>=1024 && loadcontact) {
    loadcontact=0;
    loaded=1;
    alpha = atan2(dy, dx);
    angle = atan2(speedy, speedx) - alpha;
    sp=hypot(speedx, speedy);
    deltaalpha=sp*sin(angle)/2 * M_PI/262144;
    deltaalpha=min(deltaalpha,  M_PI/16);
    deltaalpha=max(deltaalpha, -M_PI/16);
    spr=sp*cos(angle);
    speedx = spr*cos(alpha)/(1 + REL_MASS);
    speedy = spr*sin(alpha)/(1 + REL_MASS);
  }
  return(res);
}

int
resonablefuel(int x, int y, int l)
{
  thing *tp;
  int dx, dy;

  tp=&things[l];
  dx=x-(int)(*tp).x;
  dy=(int)(*tp).y-y;
  if(dx>(int)(lenx3>>1))
    dx=lenx3-dx;
  if(-dx>(int)(lenx3>>1))
    dx=-lenx3+dx;
  return(dx>-10 && dx<9 && dy>5 && dy<60);
}

int
closestfuel(int x, int y)
{
  word i, which=0;
  thing *thingptr;
  int dx, dy;
  int minimum=MAXINT, d;
  int hit=0;

  for(i=0, thingptr=things; i<nrthings; i++, thingptr++)
    if((*thingptr).type==1 && (*thingptr).alive>1) {
      dx=abs((int)x-(int)(*thingptr).x);
      dy=abs((int)y-(int)(*thingptr).y);
      if(dx>(lenx3>>1))
	dx-=lenx3;
      dy*=3;
      d=dx*dx+dy*dy;
      if(d<minimum) {
	minimum=d;
	which=i;
	hit=1;
      }
    }
  return(hit?which:-1);
}

int
closestbutton(int x, int y)
{
  word i, which=0;
  thing *thingptr;
  int dx, dy;
  int minimum=MAXINT, d;
  int hit=0;

  for(i=0, thingptr=things; i<nrthings; i++, thingptr++)
    if((*thingptr).type>=7 && (*thingptr).type<=8) {
      dx=abs((int)x-(int)(*thingptr).x);
      dy=abs((int)y-(int)(*thingptr).y);
      if(dx>(lenx>>1))
	dx-=lenx;
      d=dx*dx+dy*dy;
      if(d<minimum) {
	minimum=d;
	which=i;
	hit=1;
      }
    }
  return(hit?which:-1);
}

void
hit(word x, word y, word crash, word owner)
{
  word i, which=0;
  thing *thingptr;
  long dx, dy;
  long minimum=MAXLONG, d;
  int hit=0;
  int kill=1;

  for(i=0, thingptr=things; i<nrthings; i++, thingptr++)
    if((*thingptr).alive>1 && crashtype((*thingptr).type)==crash) {
      dx=(int)x-(int)(*thingptr).x;
      dy=(int)y-(int)(*thingptr).y;
      d=(long)dx*dx+(long)dy*dy;
      if(d<minimum) {
	minimum=d;
	which=i;
	hit=1;
      }
    }
  if(hit) {
#ifdef HAVE_SOUND
    if(play_sound)
      Snd_effect(SND_BOOM2, CHAN_4);
#endif
    explodething(&things[which]);
    switch(things[which].type) {
    case 2:
      ssblip+=10;
      if(ssblip>100)
	things[which].alive=1;  /* Dying */
      break;
    case 7:                     /* Cannot kill buttons */
    case 8:
      startupsliders(majorbutton(which));
      break;
    default:
      things[which].alive=1;	/* Dying */
      break;
    }
    if(kill) {
    }
    if(owner)
      switch(things[which].type) {
      case 1:
	things[which].score=150;
	break;
      case 3:
      case 4:
      case 5:
      case 6:
	things[which].score=750;
	break;
      }
  }
}

void
bunkerfirebullet(thing *b)
{
  int dir;

  dir=random()%16;
  switch((*b).type) {
  case 3:
    dir=(dir-3)&31;
    newbullet(((*b).x<<3)+14, ((*b).y<<3)-68,
	      32*cos(dir * M_PI/16), 32*sin(dir * M_PI/16),
	      dir>>1, 0);
    break;
  case 4:
    dir=(dir+3)&31;
    newbullet(((*b).x<<3)-26, ((*b).y<<3)-68,
	      32*cos(dir * M_PI/16), 32*sin(dir * M_PI/16),
	      dir>>1, 0);
    break;
  case 5:
    dir=(dir-14)&31;
    newbullet(((*b).x<<3)+14, ((*b).y<<3)+44,
	      32*cos(dir * M_PI/16), 32*sin(dir * M_PI/16),
	      dir>>1, 0);
    break;
  case 6:
    dir=(dir+12)&31;
    newbullet(((*b).x<<3)-26, ((*b).y<<3)+44,
	      32*cos(dir * M_PI/16), 32*sin(dir * M_PI/16),
	      dir>>1, 0);
    break;
  }
}

void
bunkerfirebullets(void)
{
  int l;
  thing *thingptr;

  for(l=0, thingptr=things; l<nrthings; l++, thingptr++)
    if((*thingptr).alive)
      if((*thingptr).type>2 && (*thingptr).type<7)
	if(!(random()%256))
	  bunkerfirebullet(thingptr);
}

int
killdyingthings(void)
{
  int l;
  thing *thingptr;
  int res;

  res=0;
  for(l=0, thingptr=things; l<nrthings; l++, thingptr++)
    if((*thingptr).alive==1) {
      deletething(thingptr,pblockx,pblocky,bblockx,bblocky);
      res+=(*thingptr).score;
    }

  return(res);
}

void
newfragment(word x, word y)
{
  static word nr=0;
  int dir;
  int speed;

  dir=random()%32;
  speed=random()%48+16;
  fragments[nr].life=90-speed;
  fragments[nr].x=x;
  fragments[nr].y=y;
  fragments[nr].vx=speed*cos(dir * M_PI/16)/8;
  fragments[nr++].vy=speed*sin(dir * M_PI/16)/8;

  if(nr==maxfragments)
    nr=0;
}

void
explodething(thing *thingptr)
{
  int i;

  for(i=0; i<30; i++) {
    newfragment((thingptr->x<<3)-30+random()%61,
		(thingptr->y<<3)-30+random()%61);
  }
}

void
explodeship(void)
{
  int i;

  for(i=0; i<50; i++) {
    newfragment((x+((154+shipdx)<<3)+random()%61)%(lenx3<<3),
		(y+(( 82+shipdy)<<3)+random()%61)%(leny3<<3));
    if(loaded)
      newfragment((x+((161-(int)((252-loadpoint)*cos(alpha)/7.875))<<3)
		   +random()%61)%(lenx3<<3),
		  (y+(( 89+(int)((252-loadpoint)*sin(alpha)/7.875))<<3)
		   +random()%61)%(leny3<<3));
  }
}

void
wrapfragments(void)
{
  fragment *fragmentptr;
  int l;

  for(l=0, fragmentptr=&fragments[0]; l<maxfragments; l++, fragmentptr++)
    if((*fragmentptr).life>0)
      (*fragmentptr).y+=(PBILDY-leny3)<<3;
}

void
unwrapfragments(void)
{
  fragment *fragmentptr;
  int l;

  for(l=0, fragmentptr=&fragments[0]; l<maxfragments; l++, fragmentptr++)
    if((*fragmentptr).life>0)
      (*fragmentptr).y-=PBILDY<<3;
}

void
movefragments(void)
{
  int l;
  fragment *fragmentptr;

  for(l=0, fragmentptr=&fragments[0]; l<maxfragments; l++, fragmentptr++)
    if((*fragmentptr).life>0) {
      (*fragmentptr).life--;
      (*fragmentptr).x=((*fragmentptr).x+(lenx<<6)+(*fragmentptr).vx)
	% (lenx<<6);
      (*fragmentptr).y=((*fragmentptr).y+(leny<<6)-(*fragmentptr).vy)
	% (leny<<6);
    }
}

word
livefragments(void)
{
  int l;
  fragment *fragmentptr;

  for(l=0, fragmentptr=&fragments[0]; l<maxfragments; l++, fragmentptr++)
    if(fragmentptr->life>0)
      return(1);
  return(0);
}
