/* GravityWars 1.1,  (C) Sami Niemi -95 */

#include <vgakeyboard.h>
#include "memory.h"

 
/*------------------------------------------------------------------ control */
/* The control & moveship routines contain really bad code because
/* they have been changed too many times since the start of the project...
/* I wish I had written them from scratch after the n:th change... 
/* As you see some of the routines take the ship # as an argument =>
/* the game was going to be a multiuser game, but lazy as i am..            */

void control(short nr) {

  static int ch;
  static short n,m,bulletmax,angle;
  static long x,y,tmp;
  static uchar *bul_adr;
  static uchar bul_col;
  static int objadr;

  keyboard_update();

  ending=FALSE;

  while ((*(scans+SCANCODE_ESCAPE)==0) && (!ending) && (!escape)) {

    mouse_update();
    if (mouse_getbutton()!=0) doPanic();

    keyboard_update();    

    if (                                      /* Ctrl */
	(
	 *(scans+SCANCODE_LEFTCONTROL) ||
	 *(scans+SCANCODE_RIGHTCONTROL)
	 )
	&& (loadtime[nr]==0)
	&& (sf[nr]==0)             /*!!!!!!!!!! SHOULD BE HANDELED AS FLAGS!!*/
	&& ( (ship_flag & 2)!=2)
	) {    
     
      for(n=0; n<=N_BULLETS; n++) {   /* Shoots if there are free bullets */

	if (!bullet[n].active) {
	  bullet[n].active=TRUE;
	  bullet[n].ang=angle=sa[nr] >> 5;
	  bullet[n].dis=16;
	  bullet[n].xs=((angX[angle] << 6)+(angX[angle]<<5)+sVx[nr]);
	  bullet[n].ys=((angY[angle] << 6)+(angY[angle]<<5)+sVy[nr]+sVg[nr]);
	  bullet[n].x=sx[nr]+16384+(angX[angle] << 8)+(angX[angle] << 7);
          bullet[n].y=sy[nr]+16384+(angY[angle] << 8)+(angY[angle] << 7);
  
	  
	  setbullet(bullet[n].x >> STEP,bullet[n].y >> STEP,n);
	  
	  

	  loadtime[nr]=16;
	  break;

	}
      }
    }

    if ((ship_flag & 1)==0) {
      if (*(scans+SCANCODE_CURSORBLOCKLEFT)) {  /* Left */
	sa[nr]+=12;  /*22222222222*/
	if (sa[nr]>1023) sa[nr]=0;
      }
      
      if (*(scans+SCANCODE_CURSORBLOCKRIGHT)) { /* Right */
	sa[nr]-=12;
	if (sa[nr]<0) sa[nr]=1023;
      }
    }

    if ( (*(scans+SCANCODE_CURSORBLOCKUP)) && (ShipFuel>0) ) {    /* Up */

      ShipFuel--;
      if ((ShipFuel&15)==0) 
	updatescore();
      
      if (ShipFuel==0) {
	putstamp(272,win_y+184,67,105,154);
	sleep(2);
	killstamp(272,win_y+184);
      }
  
      if (ShipFuel<0) 
	ShipFuel=0;


      sVx[nr]+=((angX[sa[nr] >> 5]*friction)>>10);
      tmp=(angY[sa[nr] >> 5]*friction)>>10;
      sVy[nr]+=tmp;
      sVg[nr]+=tmp;
      if (sVg[nr]<0) sVg[nr]=0;


      g_flag=g_flag | 1;

      thrust_len++;
    } 
    else {
      thrust_len=0;
      g_flag=g_flag & 254;
    }

    sVx[nr]=(sVx[nr]*medium) >> 10;      /* Tröghetsmoment */
    sVy[nr]=(sVy[nr]*medium) >> 10;


    if ((ship_flag & 1)==0)
      sVg[nr]+=gravity; /* Gravity */


    if (sVx[nr]>MaxNorm) sVx[nr]=MaxNorm;
    if (sVy[nr]>MaxNorm) sVy[nr]=MaxNorm;
    if (sVx[nr]<-MaxNorm) sVx[nr]=-MaxNorm;
/*
    if (sVy[nr]<-MaxFall) sVy[nr]=-MaxFall; 
*/
    if (sVg[nr]>MaxNorm) sVg[nr]=MaxNorm; 
    if ( (sVy[nr]+sVg[nr])<-MaxFall) sVy[nr]=-MaxFall+sVg[nr];
 

    if ((loadtime[0]--) < 0) loadtime[0]=0;
    if ((loadtime[1]--) < 0) loadtime[1]=0; 
 
    ShipTime--;
    if ((ShipTime&127)==0) 
      updatescore();
    if (ShipTime==0) 
      explode();
    

    old_x=sx[nr] >> STEP;
    old_y=sy[nr] >> STEP;

    if ((ship_flag & 1)==0) {
      sx[nr]+=sVx[nr];
      sy[nr]+=sVy[nr]+sVg[nr];
    }

    moveShip(0,g_flag);

    centerShip(0);



    for(n=0; n<=N_BULLETS; n++) {


      if (bullet[n].active) {

	x=bullet[n].x>>STEP;
	y=bullet[n].y>>STEP;

	tmp=y>>5;
	objadr=(x>>5)+(tmp<<4)+(tmp<<2);

	/* remove bullet */
	killbullet(x,y,n);

	if (bullet[n].dis>43) 
	  bullet[n].active=FALSE;
	
	bul_adr=bulletback[n];
	for(tmp=0; tmp<=8; tmp++) {
	  bul_col=bul_adr[tmp];
	  if (
	      (bul_col==DOOR1COLOR) ||
	      (bul_col==DOOR2COLOR) ||
	      (bul_col==DOOR3COLOR) ||
	      (bul_col==WATERCOLOR)
	      ){

	    switch(objects[objadr]) {
	    	      
	    case L_RED_DOOR:
	      makehole(x,y,0);
	      for(m=0; m<=N_ACTION; m++) {
		if (!action[m].state) {
		  action[m].state=TRUE;
		  action[m].x=x;
		  action[m].y=y;
		  action[m].start=48;
		  action[m].stop=51;
		  action[m].frame=48;
		  action[m].speed=4;
		  action[m].delay=4;
		  goto nomoreloop;
		}
	      }
	      break;

	    case E_TOP_WATER:
              for(m=0; m<=N_ACTION; m++) {
                if (!action[m].state) {
                  action[m].state=TRUE;
                  action[m].x=x;
                  action[m].y=y;
                  action[m].start=112;
                  action[m].stop=117;
                  action[m].frame=112;
                  action[m].speed=4;
                  action[m].delay=4;
                  goto nomoreloop;
                }
              }
              break;
 

	    default:
	      for(m=0; m<=N_ACTION; m++) {
		if (!action[m].state) {
		  action[m].state=TRUE;
		  action[m].x=x;
		  action[m].y=y;
		  action[m].start=48;
		  action[m].stop=51;
		  action[m].frame=48;
		  action[m].speed=4;
		  action[m].delay=4;
		  goto nomoreloop;
		}
	      }
	      break;
	    };

	  nomoreloop:

	    bullet[n].active=FALSE;
	    break;
	  }
	  else 
	  if (
	      (bul_col>127) &&
	      (bul_col<175)
	      ) {
	    
            for(m=0; m<=N_ACTION; m++) {
              if (!action[m].state) {
                action[m].state=TRUE;
                action[m].x=x;
                action[m].y=y;
                action[m].start=49;
                action[m].stop=51;
                action[m].frame=49;
                action[m].speed=6;
                action[m].delay=6;
		goto nomoreloop2;
              }
            }
          nomoreloop2:
 	    
	    bullet[n].active=FALSE;
	    break;
	  }
	}
	
	if (bullet[n].active) {
	  bullet[n].x+=bullet[n].xs;
	  bullet[n].y+=bullet[n].ys;
	  bullet[n].dis++;

	  setbullet(bullet[n].x>>STEP,bullet[n].y>>STEP,n);
	}
      }
    }
  }
}
