/*	Xasteroids
	Copyright 1990 by Phil Goetz
	goetz@cs.buffalo.EDU
	Version 5, 9 Feb 93

	Changes from version 4.3:

		High score script.
		Enemy moves towards you at higher levels.

	Changes from version 3:

		Better collision detection:  Actually checks for intersection
			of line segments if non-round objects are very close.
		Explosions!  (Thanks to Peter Phillips.)
		Fine rotation repeating detected using KeyRelease.
		Thrust indicator behind ship.  (Thanks to Peter Phillips.)
		Doesn't place ship in center of screen for new levels.
		Seeds random-number generator with time.
			(Thanks to Craig Smith.)
		Shields!  (Thanks to Peter Phillips.  I rewrote the bounce code
			& added the energy bar & other refinements.)
		Solved won't-take-input bug. (Thanks to Doug Merritt.)
		Detects failure of XOpenDisplay. (Thanks to Pat Ryan.)

	Contributors:	Peter Phillips <pphillip@cs.ubc.ca>
			Pat Ryan <pat@jaameri.gsfc.nasa.gov>
			Craig Smith <csmith@cscs.UUCP>
			Doug Merritt <doug@netcom.com>
			Makefile by James Berg <berg@plains>
			Hi score script by Chris Moore <moore@src.bae.co.uk>
			Man page by David Partain <dpl@ida.liu.se>
*/
#include <stdio.h>		/* For NULL	*/
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>	/* For erasing cursor - not important	*/
#include <math.h>

/* Indexes for 1st dimension of obj	*/
/* The order they are in is important	*/
#define	AST	0
#define	ENEMY	96
#define ENEMYBUL 97
#define	FBUL	98
#define	LASTBUL	102
#define	SHIP	103
#define LASTOBJ 103	/* Must be ship!  See makeasts().	*/

/* Shapes	*/
/* Order is important!  See collide().	*/
#define	ASTSHAPE1	0
#define ASTSHAPE2	1
#define ASTSHAPE3	2
#define ENBULSH		3
#define	BULSHAPE	4
#define	SHIPSHAPE	5
#define SHIPTHRSHAPE	6
#define	ENEMYSHAPE	7
#define LASTSHAPE	7

/* Masses	*/
#define M_BIG	8.0
#define M_MED	4.0
#define M_SMALL	1.0
#define M_SHIP	1.5
#define M_ENEMY	1.0
#define M_BULLET 0.1

/* Keys		*/
#define FIRE		'p'
#define PAUSE		27	/* escape	*/
#define SHIELD		'`'
#define THRUST		'o'

#define BMAX		300	/* Max particles in a "boom" + 1	*/
#define letheight	20	/* height of font	*/
#define	pi		3.1415926535897932384
#define SHIPSIZE	28

typedef struct _Boom *Boom;
struct _Boom {Boom next; int dur, part; double bcoord[BMAX][2], bvec[BMAX][2];};
typedef struct {int shape, alive, time;
		double mass, x, y, xvel, yvel, rot, rotvel;} Objtype;
typedef struct {double angle; int length;} PolarPair;
typedef struct {double x, y, mag;} Vector;

/* Global variables:	*/
Objtype	obj[SHIP+1];
/*	In shapes pairs, 1st # is radians, 2nd is length in pixels.
	Degrees: 0 ->, pi/2 down, pi <-, 3*pi/2 up
	IF YOU CHANGE THE SHAPES, you MUST change numpairs & shapesize
*/
PolarPair shapes[LASTSHAPE+1][11] =
	{	{{0,0}, {3*pi/2,40}, {0,20}, {pi/4,28}, {pi/2,40}, /* just crossed 0-deg line */
		 {3*pi/4,28},{pi,40},{5*pi/4,28},{3*pi/2,40},{7*pi/4,28},{0,20}},
/*	hexagon if you prefer
		{{0,0}, {3*pi/2, 20}, {pi/6, 20}, {pi/2, 20},
                 {5*pi/6, 20}, {7*pi/6, 20}, {3*pi/2, 20}, {11*pi/6, 20}},
*/
		{{0,0}, {3*pi/2,20}, {0,10}, {pi/4,14}, {pi/2,20},
		 {3*pi/4,14},{pi,20},{5*pi/4,14},{3*pi/2,20},{7*pi/4,14},{0,10}},
		{{0,0}, {3*pi/2,10}, {0,5}, {pi/4,7}, {pi/2,10},
		 {3*pi/4,7},{pi,10},{5*pi/4,7},{3*pi/2,10},{7*pi/4,7},{0,5}},
		{{0,0}, {7*pi/4, 4}, {pi/4, 4}, {3*pi/4, 4}, {5*pi/4, 4}},
		{{0,0}, {0,10}},
		{{0,0}, {5*pi/4,28}, {0,20}, {pi/4,28},{3*pi/4,28},{pi,20},{7*pi/4,28}},	/* Ship */
		{{0,0}, {5*pi/4,28}, {0,20}, {pi/4,28},{3*pi/4,28},{pi,20},
		 {7*pi/4,28}, {3*pi/4, 7}, {9*pi/8, 13}, {15*pi/8, 13}},	/* Thrusting ship */
		{{0,0}, {pi,20},{7*pi/4,28},{pi/4,28},{pi,20}}
	};
Boom	blist = NULL;
double	drawscale = 1, speedscale = 1;
int	width, height,
	energy,		/* # of turns shield is good for	*/
	highscore = 0, level,
	nextbul = FBUL,			/* Obj# of next bullet fired	*/
	numasts, oldscore = 99,
	rndint = 73, ships, score,
	numpairs[LASTSHAPE+1]	= {11, 11, 11, 5, 2, 7, 10, 5},
	shapesize[LASTSHAPE+1]	= {44, 21, 10, 2, 1, SHIPSIZE+1, 35, 20},
	shield_on;

initasts()
{	int i;
	extern Objtype obj[SHIP+1];

	for (i = 0; i < LASTOBJ+1; i++)
	{	obj[i].rot = 0;
		obj[i].rotvel = 0;
	}
	for (i = 0; i < ENEMY; i++)
	{	obj[i].shape = ASTSHAPE1;
	}
	obj[SHIP].shape = SHIPSHAPE;
	obj[SHIP].mass = M_SHIP;
	obj[ENEMY].shape = ENEMYSHAPE;
	obj[ENEMY].mass = M_ENEMY;
	obj[ENEMYBUL].shape = ENBULSH;
	obj[ENEMYBUL].mass = M_BULLET;
	for (i = FBUL; i < LASTBUL+1; i++)
	{	obj[i].shape = BULSHAPE;
		obj[i].mass = M_BULLET;
}	}

makeasts()
{	int i;
	extern Objtype obj[SHIP+1];
	extern int level, numasts, rndint;
	extern double speedscale;
	unsigned char a;

	for (i = 0; i < SHIP; i++)
		obj[i].alive = 0; /* Erase objs from last level except ship */
	for (i = ENEMYBUL; i < LASTBUL+1; i++)
		obj[i].time = 0;		/* No bullets in the air */
	for (i = 0; i < ((level > 8) ? 8 : level+4); i++)	/* Asteroids:	*/
	{	a = rand(rndint); a>>=1;	/* a = rand# from 0 to 127 */
		if (a > 63)
			obj[i].x = (double) a;
			else obj[i].x = (double) (width - a);
		a = rand(rndint); a>>=1;	/* Now the same for y	*/
		if (a >  63)
			obj[i].y = (double) a;
			else obj[i].y = (double) height - a;
		a = rand(rndint); a = 4 - a>>5;
		obj[i].rot = (double) a;
		a = rand(rndint);
		obj[i].rotvel = ((double) a)/2048;
		a = rand(rndint);
		obj[i].xvel = cos((double) a);
		obj[i].yvel = sin((double) a);
		obj[i].shape = ASTSHAPE1;
		obj[i].mass = M_BIG;
		obj[i].alive = 1;
	}
	numasts = i;
}

makeenemy()	/* Start an enemy ship	*/
{	extern Objtype obj[SHIP+1];
	extern int height, level, rndint;
	unsigned char c;

	obj[ENEMY].alive = 1;
	obj[ENEMY].x = 0;
	obj[ENEMY].y = (double) height/4;
	c = rand(rndint); obj[ENEMY].y += (double) c; /* May put enemy outside window	*/
	obj[ENEMY].xvel = .4 + ((double) level)/3.;
	obj[ENEMY].yvel = 0;
	obj[ENEMY].rot = 0;
	obj[ENEMY].rotvel = 0;
}

int nextast()	/* Find next unused asteroid object	*/
{	extern Objtype obj[SHIP+1];
	int i;
	for (i = 0; obj[i].alive; i++);	/* guaranteed to find one	*/
	return i;
}

int collide(i, j)	/* Returns non-zero if i collided with j	*/
			/* Ship must be j!  (See below)			*/
	int i, j;
{	extern Objtype obj[SHIP+1];
	extern int shapesize[LASTSHAPE+1];
	extern double drawscale;
	double	mi, mj,				/* Slopes of lines	*/
		ix1, ix2, iy1, iy2, jx1, jx2, jy1, jy2,	/* Endpoints	*/
		roti, rotj,
		xcross,	ycross,		/* coord of intersection	*/
		z;
	int	diff, xd, yd,
		a, b,
		shapei, shapej;
	xd = obj[i].x - obj[j].x;
	yd = obj[i].y - obj[j].y;
	diff = sqrt((double)(xd*xd + yd*yd));
	shapei = obj[i].shape; shapej = obj[j].shape;
	/* Note this will miss if drawscale is < 0	*/
	if (diff < (shapesize[shapei] + shapesize[shapej])*drawscale)
	{   /* If both are round objects, approximation is good */
	    if (shapei < SHIPSHAPE && shapej < SHIPSHAPE) return 1;
	    if (j == SHIP && shield_on) return 1;	/* Ship always j! */
	    roti = obj[i].rot; rotj = obj[j].rot;
	    ix1 = (double) obj[i].x; iy1 = (double) obj[i].y;
	    for (a = 1; a < numpairs[shapei]; a++)
	    {	ix2 = ix1 + (double) shapes[shapei][a].length * drawscale *
			cos(shapes[shapei][a].angle + roti);
		iy2 = iy1 + (double) shapes[shapei][a].length * drawscale *
			sin(shapes[shapei][a].angle + roti);
		if (ix1 == ix2)
		{	printf("\nif1 = if2"); return 1;} /* Easy way out */
		mi = (iy2-iy1)/(ix2-ix1);
		z = mi*ix1;
		jx1 = (double) obj[j].x; jy1 = (double) obj[j].y;
		for (b = 1; b < numpairs[shapej]; b++)
		{	jx2 = jx1 + (double) shapes[shapej][b].length *
				drawscale * cos(shapes[shapej][b].angle + rotj);
			jy2 = jy1 + (double) shapes[shapej][b].length *
				drawscale * sin(shapes[shapej][b].angle + rotj);
			if (jx1 == jx2)
			{	ycross = ix1 + (jx1-ix1)*mi;
				if ((jx1-ix1) * (ix2-jx1) >= 0 &&
					(ycross-jy1)*(jy2-ycross) >= 0)
					return 1;
				else jx2 += .0001;	/* Avoid divide by 0 */
			}
			mj = (jy2-jy1)/(jx2-jx1);
			if (mj == mi) goto Loopend;	/* Parallel lines */
			xcross = (iy1 - jy1 + mj*jx1 - z) / (mj - mi);
			if ((xcross-ix1) * (ix2-xcross) > 0
				&& (xcross-jx1) * (jx2-xcross) > 0) return 1;
Loopend:		jx1 = jx2; jy1 = jy2;
		}
		ix1 = ix2; iy1 = iy2;
	}   }
	return 0;
}

blastpair(i, j)		/* Generate random velocity vector v.	*/
	int i, j ;	/* Add v to i, -v to j.			*/
{	extern int rndint;
	extern Objtype obj[SHIP+1];
	unsigned char c;	/* for rand	*/
	double vx, vy;
	c = rand(rndint);
/*	c = 4 - c>>5;	if you need angles from -3 to 4		*/
	c>>2;		/* possibly save some time on sin/cos	*/
	vx = cos((double) c); vy = sin((double) c);
	obj[i].xvel = obj[i].xvel + vx;
	obj[i].yvel = obj[i].yvel + vy;
	obj[j].xvel = obj[j].xvel - vx;
	obj[j].yvel = obj[j].yvel - vy;
	obj[i].rotvel = obj[i].rotvel + .05;
	obj[j].rotvel = obj[j].rotvel - .05;
}

/* dot product of 2 vectors	*/
#define dot(i,j)	(i.x*j.x + i.y*j.y)
/* rotational inertia (constant eliminated) of obj. i	*/
#define rotinert(i)	(double) (obj[i].mass*shapesize[obj[i].shape]*shapesize[obj[i].shape])

/* cause two objects to collide elastically	*/
bounce(i, j)
int	i,j;
{
double	rotrat, temp;
extern	Objtype obj[SHIP+1];
Vector	vi, vj,		/* velocity of objs i, j		*/
	ij,		/* vector from center of i to center of j */
	vi_ij, vj_ij,	/* velocity of obj along vector ij	*/
	vipij, vjpij,	/* velocity perpendicular to ij		*/
	vi_ijf, vj_ijf;	/* post-collision velocity along ij	*/

vi.x = obj[i].xvel; vi.y = obj[i].yvel;
vj.x = obj[j].xvel; vj.y = obj[j].yvel;
ij.x = obj[j].x - obj[i].x; ij.y = obj[j].y - obj[i].y;
ij.mag = sqrt(ij.x*ij.x + ij.y*ij.y);
/*
Calculate velocities projected onto ij;
	vi_ij = vi*cos(a) = (vi dot ij) / |ij|		*/
vi_ij.mag = dot(vi, ij) / ij.mag;
vi_ij.x = (ij.x * vi_ij.mag) / ij.mag;
vi_ij.y = (ij.y * vi_ij.mag) / ij.mag;
vj_ij.mag = dot(vj, ij) / ij.mag;
vj_ij.x = (ij.x * vj_ij.mag) / ij.mag;
vj_ij.y = (ij.y * vj_ij.mag) / ij.mag;
if (vi_ij.mag - vj_ij.mag < 0)	/* Objs moving away from each other -
	Since objs are round (at least when bouncing), this means
	they are moving away from each other already.	*/
	return;
/*
Calculate component of velocities perpendicular to ij:
	|vipij| = |vi|*sin(a) = |vi x ij| / |ij|
Same as
	|vipij| = |vi|*cos(pi/2 - a) = (vi dot (perp. to ij)) / |ij|	*/
temp = vi.y*ij.x - vi.x*ij.y;	/* - (cross product when 3rd coord is 0)*/
temp /= (ij.mag*ij.mag);
vipij.x = -ij.y*temp; vipij.y = ij.x*temp;
temp = (vj.x*ij.y - vj.y*ij.x) / (ij.mag*ij.mag);
vjpij.x = -ij.y*temp; vjpij.y = ij.x*temp;
/*
Calculate the linear elastic collision along ij:
	mass(i)*vi_ij + mass(j)*vj_ij = mass(i)*vi_ijf + mass(j)*vj_ijf
	vi_ij + vi_ijf = vj_ij + vj_ijf	(derived by dividing equation
	for conservation of kinetic energy by eq. for cons. of momentum) */
temp = obj[i].mass/obj[j].mass;
vj_ijf.x = (temp * (2*vi_ij.x - vj_ij.x) + vj_ij.x) / (1 + temp);
vj_ijf.y = (temp * (2*vi_ij.y - vj_ij.y) + vj_ij.y) / (1 + temp);
vi_ijf.x = vj_ijf.x + vj_ij.x - vi_ij.x;
vi_ijf.y = vj_ijf.y + vj_ij.y - vi_ij.y;
/*
Now, given vi_ijf and vj_ijf, add them to the perpendicular
	components to get final velocity vectors		*/
obj[i].xvel = vi_ijf.x + vipij.x;
obj[i].yvel = vi_ijf.y + vipij.y;
obj[j].xvel = vj_ijf.x + vjpij.x;
obj[j].yvel = vj_ijf.y + vjpij.y;
/*
Now calculate rotational velocity exchange	*/
rotrat = rotinert(i)/rotinert(j);
temp = rotrat * (2*obj[i].rotvel - obj[j].rotvel) / (1+rotrat);
obj[i].rotvel = temp + obj[j].rotvel - obj[i].rotvel;
obj[j].rotvel = temp;
}

botline(disp, window, gc)	/* Print status line text	*/
	Display *disp;
	Drawable window;
	GC gc;
{	extern int highscore, ships, score;
	char text[70];
	sprintf(text, "Ships:%2d   Score:%6d   Shield:        High:%6d",
		ships, score, highscore);
	XDrawImageString (disp, window, gc, 0, height+letheight,
			text, strlen(text));
}

printss(disp, window, gc)	/* Print ships and score	*/
	Display *disp;
	Drawable window;
	GC gc;
{	extern int height, highscore, oldscore, ships, score;
	extern Objtype obj[SHIP+1];	/* to kill ship	*/
	char sstring[30];

	if (score != oldscore)
	{	if (score/10000 > oldscore/10000)
		{	ships++; botline(disp, window, gc);}
		if (score/10000 < oldscore/10000)
		{	ships--; botline(disp, window, gc);
			if (!ships) obj[SHIP].alive = 0;
		}
		if (score > highscore)	/* Separate if to avoid flashing */
		{	highscore = score;
			sprintf(sstring, "%6d", highscore);
			XDrawImageString (disp, window, gc, 460,
				height+letheight, sstring, strlen(sstring));
		}
		sprintf(sstring, "%6d", score);
		XDrawImageString (disp, window, gc, 170, height+letheight,
				sstring, strlen(sstring));
		oldscore = score;
	}

	/* Draw shield energy bar	*/
	XFillRectangle(disp, window, gc, 340, height+8, energy>>1, 10);
	XClearArea(disp, window, 340+(energy>>1), height+8, 8, 10, False);
}

upscore(killer, up)	/* Only award score for things the player shot */
	int killer, up;
{	extern int score;
	if (killer != ENEMYBUL && killer != SHIP)
		score = score + up;
}

/* boom, movebooms, drawbooms all by Peter Phillips */
boom(ob, particles, duration)
int ob;
int particles;
int duration;
{ extern int rndint;
  int i;
  unsigned int r1, r2;
  Boom b;
  double x, y;
  double angle, length;

  b = (Boom) malloc(sizeof(struct _Boom));
  b->dur = duration;
  b->part = particles;
  x = obj[ob].x;
  y = obj[ob].y;
  for (i = 0; i < particles; i++) {
    r1 = (rand(rndint) >> 2) % 100;
    r2 = (rand(rndint) >> 2) % 7;

    b->bcoord[i][0] = x;
    b->bcoord[i][1] = y;
    angle = r1 * pi / 50.0;
    length = 3 + r2;
    b->bvec[i][0] = cos(angle) * length + obj[ob].xvel;
    b->bvec[i][1] = sin(angle) * length + obj[ob].yvel;
  }
  b->next = blist;
  blist = b;
}

/* move the various booms that are active */
movebooms()
{
  int i;
  Boom b, prevb;

  prevb = NULL;
  b = blist;
  while (b != NULL) {
    b->dur--;
    if (b->dur < 0) { /* delete this boom */
      Boom temp;

      temp = b;
      if (prevb == NULL) {
        blist = b->next;
      } else {
        prevb->next = b->next;
      }
      b = b->next;
      free(temp);
    } else {  /* move boom, advance list */
      for (i = 0; i < b->part; i++) {
        b->bcoord[i][0] += b->bvec[i][0];
        b->bcoord[i][1] += b->bvec[i][1];
      }
      prevb = b;
      b = b->next;
    }
  }
}

/* Draw the various booms */
drawbooms(disp, window, gc)
     Display *disp;
     Drawable window;
     GC gc;
{
  int i;
  Boom b;
  XPoint figure[BMAX];

  b = blist;
  while (b != NULL) {
    for (i = 0; i < b->part; i++) {
      figure[i].x = (int) b->bcoord[i][0];
      figure[i].y = (int) b->bcoord[i][1];
    }
    XDrawPoints(disp, window, gc, figure, b->part, CoordModeOrigin);
    b = b->next;
  }
}

deletebooms()	/* delete all booms */
{	Boom b;

	b = blist;
	while (b != NULL)
	{	b->dur = 0;
		b = b->next;
}	}

killast(killer, i)
	int killer, i;		/* i = Asteroid # to kill	*/
{	extern Objtype obj[SHIP+1];
	extern int numasts;
	int k, na, oldna;

	if (obj[i].shape == ASTSHAPE1)
	{	na = nextast();		/* Could put 6 lines in a sub */
		obj[na].x = obj[i].x;
		obj[na].y = obj[i].y;
		obj[na].xvel = obj[i].xvel;
		obj[na].yvel = obj[i].yvel;
		obj[na].alive++;
		obj[na].shape = ASTSHAPE2;
		obj[na].mass = M_MED;
		obj[i].shape = ASTSHAPE2;
		obj[i].mass = M_MED;
		blastpair(i, na);
		boom(i, 30, 12);
		numasts++;
		if (numasts == 2)	/* Big asteroid was last remaining */
			upscore(killer, 25+level*2000);
		else	upscore(killer, 25);
	}
	else if (obj[i].shape == ASTSHAPE2)
	{
		for (k = 0; k < 3; k++)
		{	oldna = na;
			na = nextast();
			obj[na].x = obj[i].x;
			obj[na].y = obj[i].y;
			obj[na].xvel = obj[i].xvel;
			obj[na].yvel = obj[i].yvel;
			obj[na].alive++;
			obj[na].shape = ASTSHAPE3;
			obj[na].mass = M_SMALL;
			if (k == 1) blastpair(oldna,na);
		}
		obj[i].shape = ASTSHAPE3;
		obj[i].mass = M_SMALL;
		blastpair(na, i);
		boom(i, 20, 10);
		if (numasts == 1) upscore(killer, 500*level);
		numasts = numasts + 3;
		upscore(killer, 50);
	}
	else if (obj[i].shape == ASTSHAPE3)
	{	boom(i, 10, 8);
		obj[i].alive = 0; numasts--; upscore(killer, 100);}
	else	/* enemy {ship or bullet}	*/
	{	boom(i, 9, 7);
		obj[i].alive = 0; upscore(killer, 500);}
}
moveobjs(crash)
	int *crash;
{	extern Objtype obj[SHIP+1];
	extern int ships;
	extern double speedscale;
	int i, j;	/* Indexes	*/
	double *temp;

	movebooms();
	for (i = 0; i < LASTOBJ+1; i++)
		if (obj[i].alive)
		{	temp = &obj[i].x;
			*temp = *temp + obj[i].xvel*speedscale;
			while (*temp < 0) *temp = *temp + (double) width;
			while (*temp > width) *temp = *temp - (double) width;
			temp = &obj[i].y;
			*temp = *temp + obj[i].yvel*speedscale;
			while (*temp < 0) *temp = *temp + height;
			while (*temp > height) *temp = *temp - height;
			obj[i].rot = obj[i].rot + obj[i].rotvel;
		}
	for (i = 0; i < FBUL; i++)
	    if (obj[i].alive)
	    {
		if (obj[SHIP].alive && collide(i, SHIP))
		{	if (shield_on) bounce(SHIP, i);
			else
			{	*crash = 2;
				ships--; obj[SHIP].alive = 0;
				killast(SHIP, i);
				continue;
		}	}
		for (j = ENEMYBUL; j < LASTBUL+1; j++)
			if (obj[j].alive && collide(i, j) &&
			   (j != ENEMYBUL || (i != ENEMYBUL && i != ENEMY)))
			{	obj[j].alive = 0;	/* Kill the bullet */
				/* Don't have 2 bullets kill same asteroid */
				if (obj[i].alive) killast(j,i);
			}
	    }
}

fire()
{	extern Objtype obj[SHIP+1];
	extern int width, nextbul;
	extern double drawscale, speedscale;
	double *shiprot, cosrot, sinrot;

	obj[nextbul].alive++;
	shiprot = &obj[SHIP].rot;
	cosrot = cos(*shiprot); sinrot = sin(*shiprot);
	obj[nextbul].x = obj[SHIP].x + 20 * cosrot * drawscale;
	obj[nextbul].y = obj[SHIP].y + 20 * sinrot * drawscale;
	obj[nextbul].xvel = obj[SHIP].xvel + 10 * cosrot;
	obj[nextbul].yvel = obj[SHIP].yvel + 10 * sinrot;
	obj[nextbul].rot = *shiprot;
	obj[nextbul].time = width/(speedscale*11);	/* loops before bullet expires	*/
	nextbul++; if (nextbul == LASTBUL+1) nextbul = FBUL;
}

hyper()
{	extern Objtype obj[SHIP+1];
	extern int width, height, rndint;
	unsigned char c;
	unsigned int i;

	c = rand(rndint); i = c; i<<=2;	/* 0 - 1024	*/
	while (i > width) i -= width;
	obj[SHIP].x = (double) i;
	c = rand(rndint); i = c; i<<=2;	/* 0 - 1024	*/
	while (i > height) i -= height;
	obj[SHIP].y = (double) i;
}

vdraw(disp, window, gc, shape, x, y, rot)
	Display *disp;
	Drawable window;
	GC gc;
	int shape;
	double x, y, rot;

{	int line;
	extern PolarPair shapes[LASTSHAPE+1][11];
	extern int numpairs[LASTSHAPE+1];
	extern double drawscale;
	XPoint figure[20];
	figure[0].x = (int) x; figure[0].y = (int) y;
	for (line=1; line < numpairs[shape]; line++)	/* 2 pairs = 1 line */
	{	figure[line].x  = (int) shapes[shape][line].length *
			cos(shapes[shape][line].angle + rot) * drawscale;
		figure[line].y  = (int) shapes[shape][line].length *
			sin(shapes[shape][line].angle + rot) * drawscale;
	}
	XDrawLines (disp, window, gc, figure, numpairs[shape], CoordModePrevious);
}

main(argc, argv)
	int argc;
	char **argv;
{	Colormap cmap;
	Cursor cursor;
	Display *disp;
	Font font;
	GC gc, pmgc;
	KeySym key;
	Pixmap pixmap;
	Window window;
	XColor black, exact;
	XEvent event;
	XSizeHints hint;
	XWMHints wmhints;
	extern int width, height;
	int screen, depth;
	char text[30];
	unsigned long fg, bg;

	extern double drawscale, speedscale;
	extern int level, numasts, rndint, ships, score, oldscore;
	extern Objtype obj[SHIP+1];
	unsigned char c;	/* for rand	*/
	double *temp, dx, dy, dist;
	int crashed, flashon, len, pause = 0, delay = 64,
		enemycount, counter, counterstart = 1,
		i,	/* index for drawing objs, counting bullets */
		r;	/* radius of shield circle	*/

	disp = XOpenDisplay(0);
	if (disp == (Display *) NULL)
	{	fprintf(stderr, "Could not open display\n");
		exit(1);
	}
	screen = DefaultScreen(disp);
	bg = BlackPixel(disp, screen);
	fg = WhitePixel(disp, screen);
	hint.x = 150; hint.y = 200; hint.width = 550; hint.height = 550;
	hint.flags = PPosition | PSize;
	width = hint.width; height = hint.height-letheight-1;
	depth = DefaultDepth (disp, screen);
	window = XCreateSimpleWindow (disp, DefaultRootWindow(disp),
		hint.x, hint.y, hint.width, hint.height, 5, fg, bg);
	pixmap = XCreatePixmap (disp, window, width, height, depth);
	XSetStandardProperties (disp, window, "asteroids", "asteroids", None,
		argv, argc, &hint);
	gc = XCreateGC (disp, window, 0, 0);
	XSetGraphicsExposures(disp, gc, 0);	/* IMPORTANT!  If you do not
		specifically ask not to get Expose events, every XCopyArea
		will generate one, & the event queue will fill up.	*/
	font = XLoadFont(disp, "10x20\0");	/* If you don't have this
		font, try replacing it with 9x15\0	*/
	XSetFont(disp, gc, font);
	pmgc = XCreateGC (disp, window, 0, 0);
	XSetBackground (disp, gc, bg);
	XSetForeground (disp, gc, fg);
	XSetForeground (disp, pmgc, bg);  /* fg of pixmap is bg of window */
	wmhints.flags = InputHint;
	wmhints.input = True;
	XSetWMHints(disp, window, &wmhints);	/* Thanks to Doug Merritt */
	XSelectInput (disp, window,
		KeyPressMask | KeyReleaseMask | StructureNotifyMask);
	XMapRaised (disp, window);

	/* Erase cursor. Just delete next 5 lines if any error.	*/
	cmap = XDefaultColormap(disp, screen);
	XAllocNamedColor(disp, cmap, "Black", &exact, &black);
	cursor = XCreateFontCursor(disp, XC_dot);
	XRecolorCursor(disp, cursor, &black, &black);
	XDefineCursor(disp, window, cursor);

	XFillRectangle (disp, pixmap, pmgc, 0, 0, width, height);
/*	Can delete next line if it causes trouble	*/
	srand((unsigned) time(0));	/* By Craig Smith	*/
	initasts();
Newgame:
	deletebooms();
	ships = 3;
	score = 0; oldscore = -1;
	for (level = 0; ;)
	{   if (level < 15) level++;
	    makeasts ();
Newship:    botline(disp, window, gc);
	    if (!obj[SHIP].alive)
	    {	obj[SHIP].x = width/2;
		obj[SHIP].y = height/2;
		obj[SHIP].xvel = 0;
		obj[SHIP].yvel = 0;
		obj[SHIP].rot = 3*pi/2;
		obj[SHIP].rotvel = 0;
		energy = 80;
		shield_on = 0;
	    }
	    obj[SHIP].alive = (ships > 0);
	    crashed = 0; flashon = 0; enemycount = 20;
	    counter = 0;
	    while (numasts)
	    {	for (i = FBUL; i < LASTBUL+1; i++)  /* Bullet timer */
		    if (obj[i].alive)
		    {	obj[i].time--;
			if (!obj[i].time) obj[i].alive = 0; /* Not --! */
		    }
		while (XEventsQueued(disp, QueuedAfterReading))
		{   XNextEvent(disp, &event);
		    switch (event.type)
		    {	case MappingNotify:
			    XRefreshKeyboardMapping (&event);
			    break;
			case ConfigureNotify:
			    width = event.xconfigure.width;
			    height = event.xconfigure.height-letheight-1;
			    XFreePixmap (disp, pixmap);
			    pixmap = XCreatePixmap (disp, window, width, height, depth);
			    XFillRectangle (disp, pixmap, pmgc, 0, 0, width, height);
			    botline(disp, window, gc);
			    break;
			case KeyPress:
			    len = XLookupString (&event, text, 10, &key, 0);
			    if (len == 1 && !shield_on) switch (text[0])
			    {	case 'e':
				    obj[SHIP].rotvel = obj[SHIP].rotvel - .1; break;
				case 'r':
				    obj[SHIP].rotvel = obj[SHIP].rotvel + .1; break;
				case 'w':
				    obj[SHIP].rot -= pi/4; break;
				case 't':
				    obj[SHIP].rot += pi/4; break;
				case 'd':
				    obj[SHIP].rotvel = obj[SHIP].rotvel - .02; break;
				case 'f':
				    obj[SHIP].rotvel = obj[SHIP].rotvel + .02; break;
				case THRUST:
				    obj[SHIP].xvel += cos(obj[SHIP].rot);
				    obj[SHIP].yvel += sin(obj[SHIP].rot);
				    obj[SHIP].shape = SHIPTHRSHAPE;
				    break;
				case FIRE:
				    if (obj[SHIP].alive) fire(); break;
				case ' ':
				    if (obj[SHIP].alive)
				    {	hyper(); flashon = 1;
/*				    NOT XSetForeground (disp, gc, bg);
	If you set the fg black, & print the highscore, it will effectively erase it.	*/
				        XSetForeground (disp, pmgc, fg);
					XFillRectangle (disp, pixmap, pmgc, 0, 0, width, height);
				    }
				    break;
				case SHIELD:
				    if (energy)
				    {	shield_on = 1;
					obj[SHIP].shape = SHIPSHAPE;}
					break;
				case '.':	/* decrease delay	*/
				    if (delay > 1) delay >>=1; break;
				case ',':	/* increase delay	*/
				    delay <<=1; break;
				case 'm':	/* decrease drawscale - may go negative */
				    drawscale -= .1; break;
				case 'n':	/* increase drawscale	*/
				    drawscale += .1; break;
				case '2':	/* increase speedscale	*/
				    speedscale += .1; break;
				case '1':	/* decrease speedscale	*/
				    speedscale -= .1; break;
				case 'b':	/* increase moves/update */
				    counterstart++; break;
				case 'v':	/* decrease moves/update */
				    if (counterstart > 1) counterstart--;
				    break;
				case PAUSE:	/* pause	*/
				    pause = 1 - pause; break;
				case '+':	/* cheat	*/
				    ships++; botline(disp, window, gc); break;
				case 'Q':	/* quit		*/
				    goto End;
				case 's':	/* start new ship */
				    if (!obj[SHIP].alive)
					if (ships < 1) goto Newgame;
					else goto Newship;
				    break;
			    }
			    break;
			case KeyRelease:
			    len = XLookupString(&event, text, 10, &key, 0);
			    if (len == 1) switch (text[0])
			    {	case 'e':
				    obj[SHIP].rotvel = 0; break;
				case 'r':
				    obj[SHIP].rotvel = 0; break;
				case THRUST:
				    obj[SHIP].shape = SHIPSHAPE;
				    break;
				case SHIELD:
				    shield_on = 0; break;
			    }
/*			    break;		*/
		}   }
		if (!pause)
		{   moveobjs(&crashed);
		    if (ships) score--;	/* timer effect	*/
		    if (!counter)
		    {	counter = counterstart;	/* Restart counter */
			if (crashed == 2)
			{   crashed--; flashon++;
			    boom(SHIP, BMAX-1, 70);
			    XSetForeground (disp, pmgc, fg);
			    XFillRectangle (disp, pixmap, pmgc, 0, 0, width, height);
			    botline(disp, window, gc);
			}
			/* Write copyright notice	*/
			if (!ships && blist == NULL)
			{   sprintf(text, "Xasteroids");
			    XDrawImageString (disp, pixmap, gc,
				width/2-50, height/2-2*letheight,
				text, strlen(text));
			    sprintf(text, "Copyright 1990 by Phil Goetz");
			    XDrawImageString (disp, pixmap, gc,
				width/2-140, height/2,
				text, strlen(text));
			    sprintf(text, "goetz@cs.buffalo.edu");
			    XDrawImageString (disp, pixmap, gc,
				width/2-100, height/2+2*letheight,
				text, strlen(text));
			}
			/*	Draw objects	*/
			for (i = 0; i <= LASTOBJ; i++)
			    if (obj[i].alive)
				vdraw(disp, pixmap, gc, obj[i].shape,
					obj[i].x, obj[i].y, obj[i].rot);
			if (shield_on && obj[SHIP].alive)
			{   r = abs((int) (drawscale*SHIPSIZE));
			    XDrawArc(disp, pixmap, gc,
				((int) obj[SHIP].x) - r,
				((int) obj[SHIP].y) - r,
				2*r, 2*r, 0, 360*64);
			    energy--;
			    if (!energy) shield_on = 0;
			}
			drawbooms(disp, pixmap, gc);
			/* update display:	*/
			XCopyArea(disp, pixmap, window, gc, 0, 0, width, height, 0, 0);
			printss(disp, window, gc);
			/* erase objects	*/
			XFillRectangle (disp, pixmap, pmgc, 0, 0, width, height);
			if (flashon)
			{   flashon--;
			    XSetForeground (disp, pmgc, bg);
			    XFillRectangle (disp, pixmap, pmgc, 0, 0, width, height);
			}
			XSync(disp, 0);
		    }
		    counter--;
		    i = (rand(rndint)>>8) & 255;
		    if (!obj[ENEMY].alive)
			{   if (i < level)
			    {	c = rand(rndint);
				if (c < level * 10) makeenemy();
			}   }
		    else
		    {	i += (obj[SHIP].y > obj[ENEMY].y)
				? level>>1 : -(level>>1);
			obj[ENEMY].yvel += (i>128+6*obj[ENEMY].yvel) ? .5 : -.5;
		    }
		    enemycount--; if (!enemycount)
		    {	enemycount = 100;
			if (obj[ENEMY].alive)
			{   obj[ENEMYBUL].alive++;
			    obj[ENEMYBUL].x = obj[ENEMY].x;
			    obj[ENEMYBUL].y = obj[ENEMY].y;
			    dx = obj[SHIP].x - obj[ENEMY].x;
			    dy = obj[SHIP].y - obj[ENEMY].y;
			    dist = sqrt(dx*dx + dy*dy);
			    obj[ENEMYBUL].xvel = 3*dx/dist;
			    obj[ENEMYBUL].yvel = 3*dy/dist;
			}
			else	obj[ENEMYBUL].alive = 0;
		    }
		    for (i = 0; i < delay; i++);
		}
	    }
	}
End:	printf("\nYour high score was %d\n", highscore);
	XFreeGC (disp, gc);
	XFreeGC (disp, pmgc);
	XFreePixmap (disp, pixmap);
	XDestroyWindow (disp, window);
	XCloseDisplay (disp);
	exit(0);
}
