/* GravityWars 1.1,  (C) Sami Niemi -95 */

#include "memory.h"
 
/*--------------------------------------------------------------- centerShip */
/* Centers the scrolling picture in Y direction */

void centerShip(short nr) {

  static short y;

  y=(sy[nr] >> STEP)-240;

  if (y<16)
    y=16;
  else
    if (y>960)
      y=960;
    
  gl_setdisplaystart(0,y);

#ifdef ANIM

  animate();

#endif
  

#ifdef SCORE
  if ((y>16) || (y<960) || (ScoreChange)){
                /* Don't draw the scoretable in the upper part of the screen */
    y+=8;       /* Why? - Because it's not moving....*/

    /*fixscore();*/
    killscore(nr,win_y,y);
    win_y=y;
    putscore(nr,win_y);
  }
#endif
}


/*------------------------------------------------------------------ scrollY */
/* Scroll the screen from y1 -> y2 */

void scrollY(short nr, short y1, short y2) {

  static int y,n;
  
  if ((y1<950) || (y2<950)) {     /* y2 -> changed ?????? */

    if (y1<16)
      y1=16;
    else
      if (y1>=968)
	y1=968;  /* 968 */
    
    if (y2<24)
      y2=24;
    else
      if (y2>968)
	y2=968;

    if (y1<y2) {
      for(y=y1+8+4; y<=y2; y+=3) {
	
	vga_waitretrace();


#ifdef ANIM
	animate();
#endif

	for(n=0; n<=delay_len; n++);

#ifdef SCORE
	killscore(nr,win_y,y);
	win_y=y;
	putscore(nr,win_y);      

	gl_setdisplaystart(0,y-8);

#endif

	
/*      
#ifdef SCORE
	putscoreOnly(nr,win_y);
#endif
*/
	
      }
      vga_waitretrace();
      
      delay_len=delay_len<<1;
      for(n=0; n<=delay_len; n++);
      delay_len=delay_len>>1;
 
#ifdef SCORE
      killscore(nr,win_y,y2);
      win_y=y2;
      putscore(nr,win_y);
#endif
      gl_setdisplaystart(0,y2-8);

    }
    else {
      for(y=y1+8-4; y>=y2; y-=3) {
	
        vga_waitretrace();
	

#ifdef ANIM
	animate();
#endif

	for(n=0; n<=delay_len; n++);

#ifdef SCORE
        killscore(nr,win_y,y);
        win_y=y;
        putscore(nr,win_y);
#endif
	gl_setdisplaystart(0,y-8);
 

/*	
#ifdef SCORE
       	putscoreOnly(nr,win_y);
#endif 
*/          
      }
      vga_waitretrace();
      gl_setdisplaystart(0,y2-8);

      delay_len=delay_len<<1;
      for(n=0; n<=delay_len; n++);
      delay_len=delay_len>>1;
 

#ifdef SCORE
      killscore(nr,win_y,y2);
      win_y=y2;
      putscore(nr,win_y);
#endif

    }
  }
}
