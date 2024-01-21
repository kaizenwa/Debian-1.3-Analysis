/* GravityWars 1.1,  (C) Sami Niemi -95 */

#include "config.h"
#include <stdio.h>

extern char gamename[128];
extern gamenamelen;

extern FILE *fileptr;
extern char buffer[320*33];
extern uchar ship[4][32][32*32];
extern uchar shipremo[2][32*32]; 
extern uchar shipback[2][32*32]; 
extern uchar shipbackM[32*32];
extern uchar shipmix[32*32];
extern uchar block[216+N_DESTROYEABLE][32*32];
extern uchar level[20*45];
extern uchar objects[20*45+80];
extern uchar backgnd[32*32*4];
extern uchar actionback[N_ACTION][32*32];
extern uchar actionmix[32*32];
extern uchar tmpmix[32*32];
extern uchar bulletback[N_BULLETS+1][9];
extern uchar score[256*16];
extern uchar scoreback[256*16];
extern uchar digits[88*6];
extern uchar numbers[880]; /* old was 800 ?? */
extern uchar highscore[4096];
extern uchar levelcode[4096];
extern uchar tmpscore[4096];
extern uchar fonts[1820];
extern uchar nextlevel[128];
extern short n_cexcept,n_anim;  
extern long bigsin[256];	       

extern struct excepttype {
  uchar type;
  short x,y;
} c_except[N_CEXCEPT];

extern struct animtype {
  short x,y,start,stop,frame,speed;
} anim[N_ANIM+1];

extern struct actiontype {
  short x,y,start,stop,frame,state,speed,delay;
} action[N_ACTION+1];

extern uchar pal[768],palB[768],realpal[768],realpal2[768];

extern struct bullettype {
  long x,y,ang,xs,ys,dis,type;
  unsigned char active;
} bullet[N_BULLETS+1];

extern struct wipetype {    
  int x,y,active;
} wipe;

extern long Lsx[2],Lsy[2];
extern long sx[2],sy[2],osx[2],osy[2],sa[2],sVx[2],sVy[2],sVg[2],sA[2],sf[2];
extern long gravity,lift_thrust,medium,friction;
extern uchar *vga_ptr; 
extern uchar *scans;
extern short angX[32],angY[32];
extern short loadtime[2];
extern short win_y;
extern short anim_frame;
extern short col_frame;
extern uchar ship_flag;
extern uchar g_flag;
extern uchar ScoreChange;
extern short thrust_len;
extern short watercount;
extern short num_of_collisions;
extern short MaxNorm,MaxFall;
extern uchar fueling;

extern long  ShipScore;
extern long  HighScore;
extern short ShipFuel; 
extern short ShipTime; 
extern short ShipLife;
extern short NumKeys;
extern short stop_x,stop_y;
extern short levelnum;

extern short old_x,old_y;

extern short BaseFuel,BaseTime,BaseLife;

extern long  delay_len;

/* If you are running out of memory change the BSD routine
/* I just didn't like performing 4 divisions by 10, or
/* figuring out a better way... */
extern long  Dec2BCD[100000];

extern uchar ending,escape;

extern uchar p0[768],p1[768],p2[768],p3[768];
extern uchar p2B[768];

extern uchar hole[2][1024];
extern long SIN[32];
extern uchar bulletgfx[9];

extern char codes[100][6];





