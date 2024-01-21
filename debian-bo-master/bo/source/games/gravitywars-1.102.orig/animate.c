/* GravityWars 1.1,  (C) Sami Niemi -95 */

#include "memory.h"
 
/*------------------------------------------------------------------ animate */
void animate() {

  static short obj,n,m,fr,adj,x,y,num_anim;
  static uchar col,c1,c2,c3;

  static uchar *blockadr;
  static uchar *actionadr;

/* Fade the Red Door */

  c1=51+(SIN[(col_frame>>1)&31]>>8);

  pal[DOOR1COLOR*3]=c1;
  pal[DOOR1COLOR*3+1]=15;
  pal[DOOR1COLOR*3+2]=15;
  palB[DOOR1COLOR*3]=c1;
  palB[DOOR1COLOR*3+1]=15;
  palB[DOOR1COLOR*3+2]=15;
  p2[DOOR1COLOR*3]=48-c1;
  p2[DOOR1COLOR*3+1]=48;
  p2[DOOR1COLOR*3+2]=48;
  p2B[DOOR1COLOR*3]=48-c1;
  p2B[DOOR1COLOR*3+1]=48;
  p2B[DOOR1COLOR*3+2]=48;

  gl_setpalettecolor(DOOR1COLOR,c1,0,0);
  col=42+((2048+SIN[(16+col_frame)&31])>>8);
  c1=col>>1;
  c2=63-col;
  c3=col>>2;

  pal[GREENCOLOR*3]=c1;
  pal[GREENCOLOR*3+1]=c2;
  pal[GREENCOLOR*3+2]=c3;
  palB[GREENCOLOR*3]=c1;
  palB[GREENCOLOR*3+1]=c2;
  palB[GREENCOLOR*3+2]=c3;
  p2[GREENCOLOR*3]=63-c1;
  p2[GREENCOLOR*3+1]=63-c2;
  p2[GREENCOLOR*3+2]=63-c3;
  p2B[GREENCOLOR*3]=63-c1;
  p2B[GREENCOLOR*3+1]=63-c2;
  p2B[GREENCOLOR*3+2]=63-c3;
 
  gl_setpalettecolor(GREENCOLOR,c1,c2,c3);
  col_frame++;


/* Statical Animations */

  num_anim=n_anim-1;
  if (n_anim>0) {
    obj=anim_frame & ANIMSPEED; 
    for(n=obj; n<=num_anim; n+=ANIMSTEP) {
      fr=anim[n].frame;
      if ( ( (anim[n].start ^ anim[n].frame) & 7)==0) 
	putbox(anim[n].x << 5, anim[n].y << 5, block[fr>>3]);
      fr+=anim[n].speed;
      if (fr>anim[n].stop) fr=anim[n].start; 
      anim[n].frame=fr; 
    }
    anim_frame++;
  }
/* Action Animations */

  for(n=0; n<=N_ACTION; n++) {
    if (action[n].state) {
      
      blockadr=block[action[n].frame];
      x=action[n].x-16;
      y=action[n].y-16;
      if(action[n].frame==action[n].start) {

	getbox(x,y,actionback[n]);
	actionadr=actionback[n];

	for(m=0; m<=1023; m++)  /* Plot the new ship before scrolling */
	  actionmix[m]=blockadr[m] ? blockadr[m] : actionadr[m];
	putbox(x,y, actionmix);
 	action[n].frame++;
      }
      else
	if (action[n].frame==action[n].stop) {  
	  
/*	  beep(20000);*/
      
          if (action[n].delay>0)
            action[n].delay--;
	  else {
	    putbox(action[n].x-16,action[n].y-16,actionback[n]);
	    action[n].state=FALSE;
	  } 
	}
	else {
	  if (action[n].delay>0)
	    action[n].delay--;
	  else {

	    actionadr=actionback[n];
 	    for(m=0; m<=1023; m++)  /* Plot the new ship before scrolling */
	      actionmix[m]=blockadr[m] ? blockadr[m] : actionadr[m];
	    putbox(x,y, actionmix);
	    action[n].frame++;
	    action[n].delay=action[n].speed;
	  }
	}
    }
  }
}

void AnimateAll() {

#ifdef ANIM
  vga_waitretrace();
  animate();
#endif

#ifdef SCORE
  putscoreOnly(0,win_y);
#endif 
}

