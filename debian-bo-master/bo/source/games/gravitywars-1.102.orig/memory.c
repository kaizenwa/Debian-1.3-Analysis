/* GravityWars 1.1,  (C) Sami Niemi -95 */

#include "includes.h"

#define SIZE 1.5  /* Sinus Table Size */

FILE *fileptr;

char gamename[128];
int gamenamelen;

char buffer[320*33]; /* The Gfx is save on a 320*x screen, with lines
			between the objects.. -> only 9 objects per line */


/* The Buffer for the Ships and The Backgrounds (2 ships supported!) */

uchar ship[4][32][32*32];  /* 32 frames of ships 32*32 */
/*    ship[0][..][..]=Ship#1
          [1]        =Ship#2
          [2]        =Thrust#1
          [3]        =Thrust#2
*/

uchar shipremo[2][32*32]; /* Not only ships */
uchar shipback[2][32*32]; /* Not only ships */
uchar shipbackM[32*32];
uchar shipmix[32*32];

uchar block[216+N_DESTROYEABLE][32*32]; /* Allocate Mem for the Gfx blocks */
uchar level[20*45];                     /* Level Gfx Map */
uchar objects[20*45+80];                /* Level Object Map */ 
                                        /* +80 to avoid too long lines */
uchar backgnd[32*32*4];

uchar actionback[N_ACTION][32*32];
uchar actionmix[32*32];

uchar tmpmix[32*32];

uchar bulletback[N_BULLETS+1][9];

uchar bulletgfx[9]= { 
                     12,13,12,
		     13,14,13,
		     12,13,12
		   };

uchar score[256*16];
uchar scoreback[256*16];
uchar digits[88*6];
uchar numbers[880]; /* old was 800 ?? */
uchar highscore[4096];
uchar levelcode[4096];
uchar tmpscore[4096];
uchar fonts[1820];

/*uchar LEVEL[128],AUTHOR[128],COMMENT[128],*/
uchar nextlevel[128];


short n_cexcept,n_anim; /* Number of these.. */
 
long SIN[32]={  399*SIZE,  783*SIZE, 1137*SIZE, 1448*SIZE,
		  1702*SIZE, 1892*SIZE, 2008*SIZE, 2048*SIZE,  
	       2008*SIZE, 1892*SIZE, 1702*SIZE, 1448*SIZE,
		  1137*SIZE,  783*SIZE,  399*SIZE, 0*SIZE,           
	       -399*SIZE, -783*SIZE,-1137*SIZE,-1448*SIZE,
		  -1702*SIZE,-1892*SIZE,-2008*SIZE,-2048*SIZE,        
	      -2008*SIZE,-1892*SIZE,-1702*SIZE,-1448*SIZE,
		  -1137*SIZE, -783*SIZE, -399*SIZE, 0*SIZE};

long bigsin[256];	       

uchar hole[2][1024]={
                    {
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,1,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,1,0,0,0,1,1,1,0,0,1,1,1,0,1,1,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,1,1,1,0,1,1,1,0,1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,1,1,0,1,1,1,0,1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,1,0,0,0,1,1,1,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 
		    },
		    {
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,1,1,0,0,0,1,1,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,1,1,0,1,1,1,0,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,1,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,0,1,1,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,1,1,0,0,1,1,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 			
		    }
		  };

struct excepttype {
  uchar type;
  short x,y;
} c_except[N_CEXCEPT];

struct animtype {
  short x,y,start,stop,frame,speed;
} anim[N_ANIM+1];

struct actiontype {
  short x,y,start,stop,frame,state,speed,delay;
} action[N_ACTION+1];

uchar pal[768],palB[768],realpal[768],realpal2[768];

struct bullettype {
  long x,y,ang,xs,ys,dis,type;
  unsigned char active;
} bullet[N_BULLETS+1];

struct wipetype {    /* Stores a block 2 B destryed in vertical blank. */ 
  int x,y,active;
} wipe;

/* sx/sy=coord, sa=angle
   sV=Velocity(x,y, gravity),
   sA=Acceleration
   sf=Flags 

*/

/* Original Ship Coordinates for the level */
long Lsx[2],Lsy[2];

/* s=  Ship X/Y
   os= Old Ship X/Y
   a=  Angle
   V=  Velocity
   A=  Acceleration
   f=  flags (explosion state [0-5], etc.)
*/
long sx[2],sy[2],osx[2],osy[2],sa[2],sVx[2],sVy[2],sVg[2],sA[2],sf[2];
long gravity,lift_thrust,medium,friction;


/* sf=[0-5] Explotion State */


uchar *vga_ptr; 
uchar *scans;

short angX[32],angY[32];
short loadtime[2]={0,0};
short win_y;
short anim_frame;
short col_frame;

uchar ship_flag=0; /* 0=STILL 
		      1=WATER
		      2=??
		      */
uchar g_flag=0;

uchar ScoreChange=0;

short thrust_len=0;
short watercount=0;
short num_of_collisions=0;
short MaxNorm,MaxFall;            /* Max speeds */
uchar fueling; /* A flag for fueling on/off */

long  ShipScore=0;
long  HighScore=23456;
short ShipFuel; 
short ShipTime; 
short ShipLife=3;
short NumKeys;
short stop_x,stop_y;
short levelnum;

short old_x,old_y;

short BaseFuel,BaseTime,BaseLife;

long  delay_len=0;

long  Dec2BCD[100000];

uchar ending,escape=FALSE;

uchar p0[768],p1[768],p2[768],p3[768];
uchar p2B[768];

/* The easy way... */
char codes[100][6]={
 "jkfobt",
 "evfyvb",
 "tirtxq",
 "cqaryo",
 "vhhhcv",
 "alficj",
 "dgeigb",
 "lzjcvh",
 "uxxuqy",
 "knhsvl",
 "nxxvfb",
 "gijkrq",
 "merxjm",
 "gdlgae",
 "gkrnem",
 "zumypr",
 "zxaiht",
 "yvzqvi",
 "ebmsjo",
 "wpandf",
 "cezocq",
 "fdnhlu",
 "cmscen",
 "kkqyca",
 "oyppms",
 "uqytec",
 "jlfyvt",
 "sxfmbl",
 "zmvqmy",
 "qbwhqk",
 "ckaafg",
 "dquiop",
 "bioivq",
 "twepoq",
 "pgrloj",
 "yqvasa",
 "hvqbgg",
 "sjpgsm",
 "ynicez",
 "vtfohv",
 "yfnvfg",
 "wodorj",
 "xjtmsn",
 "asakxh",
 "jsargj",
 "mgoccw",
 "iakloe",
 "xnpszh",
 "fzchmz",
 "ovrrmz",
 "abgrdk",
 "nnkzya",
 "dxotpp",
 "cwreed",
 "duywln",
 "wmqcdt",
 "osgyrg",
 "yxgosv",
 "eusvby",
 "aesybg",
 "nzsdbx",
 "yppgpi",
 "ppfvex",
 "skumhv",
 "khbfhc",
 "lvbfae",
 "czvtfm",
 "buchpi",
 "gisawb",
 "xhizop",
 "dbmhgn",
 "lkmjdt",
 "xeoznf",
 "jupdwm",
 "euvmvj",
 "eykqfs",
 "dtcref",
 "lblbdz",
 "gmvyqt",
 "kupfhk",
 "qnjadq",
 "ujjwcn",
 "dnpoqu",
 "pxgmxw",
 "ghtxoa",
 "ieprgs",
 "hadtwg",
 "ibvzpm",
 "thlavi",
 "ydrrbf",
 "tjjicp",
 "blpgen",
 "mnoimf",
 "wgohik",
 "rgniyo",
 "nrzyab",
 "pdpgjt",
 "twijgx",
 "qcfhln",
 "tevimt"
 };

