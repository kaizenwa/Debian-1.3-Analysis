/***********************************************************
*  Mirror Magic II -- McDuffins Revenge                    *
*----------------------------------------------------------*
*  ©1994 Artsoft Development                               *
*        Holger Schemel                                    *
*        33659 Bielefeld-Senne                             *
*        Telefon: (0521) 493245                            *
*        eMail: aeglos@valinor.ms.sub.org                  *
*               aeglos@uni-paderborn.de                    *
*               q99492@pbhrzx.uni-paderborn.de             *
*----------------------------------------------------------*
*  tools.c                                                 *
*                                                          *
*  Letzte Aenderung: 29.09.1994                            *
***********************************************************/

#include "tools.h"
#include "game.h"
#include "events.h"
#include "sound.h"

void BackToFront()
{
  int x,y;

  if (!redraw_mask)
    return;
  else if (redraw_mask & REDRAW_ALL || 
	   (redraw_mask & REDRAW_FIELD && redraw_mask & REDRAW_DOOR))
    XCopyArea(display,pix[DB_BACK],window,gc,0,0,WIN_XSIZE,WIN_YSIZE,0,0);
  else if (redraw_mask & REDRAW_FIELD)
    XCopyArea(display,pix[DB_BACK],window,gc,6,6,SXSIZE+4,SYSIZE+4,6,6);
  else if (redraw_mask & REDRAW_DOOR)
    XCopyArea(display,pix[DB_BACK],window,gc,DX,DY,DXSIZE,DYSIZE,DX,DY);

  if (redraw_mask & REDRAW_TILES && 
      !(redraw_mask & REDRAW_FIELD) && !(redraw_mask & REDRAW_ALL))
  {
    if (redraw_tiles>REDRAWTILES_TH)
      XCopyArea(display,pix[DB_BACK],window,gc,SX,SY,SXSIZE,SYSIZE,SX,SY);
    else
      for(x=0;x<FIELDX;x++)
	for(y=0;y<FIELDY;y++)
	  if (redraw[x][y])
	    XCopyArea(display,pix[DB_BACK],window,gc,
		      SX+x*32,SY+y*32,32,32,SX+x*32,SY+y*32);
  }

  XFlush(display);
  for(x=0;x<FIELDX;x++)
    for(y=0;y<FIELDY;y++)
      redraw[x][y]=0;
  redraw_tiles=0;
  redraw_mask=0;
}

void ClearWindow()
{
  XFillRectangle(display,drawto,gc,SX-2,SY-2,SXSIZE+4,SYSIZE+4);
  redraw_mask|=REDRAW_FIELD;
}

void DrawText(int x, int y, char *text, int font, int col)
{
  DrawTextExt(drawto, gc, x, y, text, font, col);
  if (x<DX)
    redraw_mask|=REDRAW_FIELD;
  else
    redraw_mask|=REDRAW_DOOR;
}

void DrawTextExt(Drawable d, GC gc, int x, int y, char *text, int font,int col)
{
  if (font!=FS_SMALL && font!=FS_BIG)
    font=FS_SMALL;
  if (col<FC_RED || col>FC_YELLOW)
    col=FC_RED;
  for(;*text;text++,x+=(14+font*18))
  {
    int k=*text; 
    int l=-1;

    if (k>64 && k<91) l=k-65;
    if (k>96 && k<123) l=k-97;
    if (k>47 && k<58) l=k-18;
    if (k==63) l=26;
    if (k==33) l=27;
    if (k==58) l=28;
    if (k==46) l=29;

    if (l<0)
      XFillRectangle(display,d,gc,
		     x,y,14+(font==FS_BIG)*18,12+(font==FS_BIG)*20);
    else if (font==FS_BIG)
      XCopyArea(display,pix[FONTBIG],d,gc,
		(l%20)*32,(2*col+l/20)*32, 32,32, x,y);
    else
      XCopyArea(display,pix[FONTSMALL],d,gc,
		l*14,col*14, 14,12, x,y);
  }
}

void DrawGraphic(int x, int y, int g)
{
  DrawGraphicExt(drawto, gc, x, y, g);
  redraw_tiles++;
  redraw[x][y]=TRUE;
  redraw_mask|=REDRAW_TILES;
}

void DrawGraphicExt(Drawable d, GC gc, int x, int y, int g)
{
  if (g>=0)
    XCopyArea(display,pix[BACK],d,gc,
	      SX+(g%16)*32,SY+(g/16)*32, 32,32, SX+x*32,SY+y*32);
  else
    XFillRectangle(display,d,gc, SX+x*32,SY+y*32, 32,32);
}

int el2gfx(int e)
{
  int g;

  if (!e) return(-1);
  else if (e<30) g=e-1;
  else if (e==30) g=37;
  else if (e==31) g=32;
  else if (e==96) g=36;
  else if (e==97) g=47;
  else if (e>97 && e<114) g=e-50;
  else if (e==114) g=38;
  else if (e>114 && e<119) g=e-47;
  else if (e>118 && e<143) g=e-39;
  else if (e>142 && e<147) g=e-103;
  else if (e==147) g=44+RND(3);
  else if (e>147 && e<151) g=e-44;
  else if (e>150 && e<155) g=e-43;
  else if (e==155) g=107;
  else if (e>31 && e<96) 
    return(-1);

  return(g);
}

void DrawElement(int xe, int ye, int e)
{
  int g;

  if (!e) return;
  else if (e>31 && e<96) 
  { 
    DrawWalls(xe,ye,e); 
    return; 
  }
  else
    g=el2gfx(e);

  XCopyArea(display,pix[BACK],drawto,gc,
	    SX+(g%16)*32,SY+(g/16)*32, 32,32, SX+xe*32,SY+ye*32);

  redraw_tiles++;
  redraw[xe][ye]=TRUE;
  redraw_mask|=REDRAW_TILES;
}

void DrawWalls(int xw, int yw, int w)
{
  int h,g=33;
  int gx=SX+(g%16)*32, gy=SY+(g/16)*32;

  if (w&0x40) 
    gy+=16;
  if (w&0x10) 
    gx+=16;
  for(h=0;h<4;h++)
  {
    if (w&(1<<h))
      XCopyArea(display,pix[BACK],drawto,gc,
		gx,gy, 16,16, SX+xw*32+16*(h%2),SY+yw*32+16*(h/2));
    else if (DO)
    {
      int xw1=xw*32+16*(h%2),yw1=yw*32+16*(h/2);
      XFillRectangle(display,drawto,gc, SX+xw1,SY+yw1, 16,16);
    }
  }

  if (!redraw[xw][yw])
  {
    redraw_tiles++;
    redraw[xw][yw]=TRUE;
    redraw_mask|=REDRAW_TILES;
  }
}

void DrawWalls2(int xw, int yw, int w, int p, int b)
{
  int h,gx,gy;
  int g1=33, g2=(p+1)/2, g3=(w>79)*16, g4=(p+1)%2;
  
  if (p==0) 
  {
    DrawWalls(xw,yw,w);
    return;
  }

  for(h=0;h<4;h++)
  {
    gx=0;

    if (b&(1<<h) && w&(1<<h)) 
    {
      gx=SX+((g1+g2)%16)*32+g3;
      gy=SY+((g1+g2)/16)*32+g4;
    }
    else if (w&(1<<h)) 
    {
      gx=SX+(g1%16)*32+g3;
      gy=SY+(g1/16)*32+16;
    }

    if (gx) 
      XCopyArea(display,pix[BACK],drawto,gc,
		gx,gy, 16,16, SX+xw*32+16*(h%2),SY+yw*32+16*(h/2));
  }

  if (!redraw[xw][yw])
  {
    redraw_tiles++;
    redraw[xw][yw]=TRUE;
    redraw_mask|=REDRAW_TILES;
  }
}

void DrawMicroElement(int xe, int ye, int e)
{
  int g;

  if (!e) return;
  else if (e>31 && e<96) 
  { 
    DrawMicroWalls(xe,ye,e); 
    return; 
  }
  else
    g=el2gfx(e);

  XCopyArea(display,pix[BACK],drawto,gc,
	    326+(g%14)*8,266+(g/14)*8, 8,8, SX+12*32+xe*8,SY+6*32+ye*8);
}

void DrawMicroWalls(int xw, int yw, int w)
{
  int h,g=33;
  int gx=326+(g%14)*8, gy=266+(g/14)*8;

  if (w&0x40) 
    gy+=4;
  if (w&0x10) 
    gx+=4;
  for(h=0;h<4;h++)
  {
    if (w&(1<<h))
      XCopyArea(display,pix[BACK],drawto,gc,
		gx,gy, 4,4, SX+12*32+xw*8+4*(h%2),SY+6*32+yw*8+4*(h/2));
    else if (DO)
    {
      int xw1=xw*8+4*(h%2),yw1=yw*8+4*(h/2);
      XFillRectangle(display,drawto,gc, SX+12*32+xw1,SY+6*32+yw1, 4,4);
    }
  }
}

void DrawMicroLevel(int xpos, int ypos)
{
  int x,y;

  XFillRectangle(display,drawto,gc, xpos,ypos, 4*32,3*32);

  for(x=0;x<16;x++) for(y=0;y<12;y++)
    DrawMicroElement(x,y,Ur[x][y]);

  XCopyArea(display,pix[DB_BACK],window,gc,
	    SX+12*32,SY+6*32, 4*32,3*32, SX+12*32,SY+6*32);
}

int AYS_in_range(int x, int y)
{
  if (y>DY+249 && y<DY+278)
  {
    if (x>DX+1 && x<DX+48)
      return(1);
    else if (x>DX+51 && x<DX+98) 
      return(2);
  }
  return(0);
}

BOOL AreYouSure(char *text, int behind)
{
  int mx,my,tx,ty,tl,tc,rt=-1;
  int dr=DR;
  static char txt[100];
  BOOL choosen=-1;

  if (ABS(behind)==3)
    XCopyArea(display,pix[DB_BACK],pix[DB_DOOR],gc, DX,DY, 100,280, 300,0);

  XFillRectangle(display,pix[DB_DOOR],gc,100,0,100,280);

  for(ty=0;ty<13;ty++)
  {
    if (!(*text))
      break;
    for(tl=0,tx=0;tx<7;tl++,tx++)
    {
      tc=*(text+tx);
      if (!tc || tc==32)
	break;
    }
    if (!tl)
    { 
      text++; 
      ty--; 
      continue; 
    }
    sprintf(txt,text); 
    txt[tl]=0;
    DrawTextExt(pix[DB_DOOR],gc,152-(tl*14)/2,SY+ty*16,txt,FS_SMALL,FC_YELLOW);
    text+=(tl+(tc==32));
  }
  if (!behind)
    return(FALSE);

  XCopyArea(display,pix[DOOR],pix[DB_DOOR],gc, 202,2, 96,28, 102,250);

  if (dr)
    CloseDoor(ABS(behind));
  OpenDoor(1);

  if (game_status!=MAINMENU)
    InitAnimation();

  while(rt<0)
  {
    ColorCycling();
    DoAnimation();
    Delay(10000);

    if (XPending(display))
    {
      XEvent event;

      XNextEvent(display, &event);
      switch(event.type)
      {
	case Expose:
	  HandleExposeEvent((XExposeEvent *) &event);
	  break;
	case ButtonPress:
	  mx = ((XButtonEvent *) &event)->x;
	  my = ((XButtonEvent *) &event)->y;
	  if (!(choosen=AYS_in_range(mx,my)))
	    choosen=-1;
	  break;
	case ButtonRelease:
	  button_status = MB_NOT_PRESSED;
	  mx = ((XButtonEvent *) &event)->x;
	  my = ((XButtonEvent *) &event)->y;
	  if (choosen==AYS_in_range(mx,my))
	    rt=(choosen==1);
	  break;
	case KeyPress:
	case KeyRelease:
	  switch(XLookupKeysym((XKeyEvent *)&event,
			       ((XKeyEvent *)&event)->state))
	  {
	    case XK_Return:
	      rt=1;
	      break;
	    case XK_Escape:
	      rt=0;
	      break;
	    }
	  break;
	default:
	  break;
      }
    }
  }

  if (game_status!=MAINMENU)
    StopAnimation();

  XCopyArea(display,pix[DB_BACK],pix[DB_DOOR],gc, DX,DY, 100,280, 100,0);
  CloseDoor(1);
  if (dr && behind>0)
    OpenDoor(behind);

  return(rt);
}

void OpenDoor(int nr)
{
  MoveDoor(nr,1);
  ClearEventQueue();
}

void CloseDoor(int nr)
{
  MoveDoor(nr,0);
  ClearEventQueue();
}

void MoveDoor(int nr, int dr)   /* dr==0: Tür zu, dr==1: Tür auf. */
{
  int i;

  if (dr)
  {
    XCopyArea(display,pix[DOOR],pix[DOOR],gc, 104,136, 8,8, 146,136);
    XCopyArea(display,pix[DOOR],window,gc, 104,136, 8,8, DX+46,DY+136);
    XFlush(display);
    for(i=0;i<30;i++)
    {
      ColorCycling();
      if (game_status==MAINMENU)
	DoAnimation();
      Delay(10000);
    }
  }
  else
    XCopyArea(display,pix[DOOR],pix[DOOR],gc, 88,136, 8,8, 146,136);

  PlaySoundStereo(SND_OEFFNEN,PSND_MAX_RIGHT);

  for(i=100*dr;i>=0 && i<=100;i=i+4-dr*8)
  {
    XCopyArea(display,pix[DB_DOOR],pix[DB_BACK],gc,
	      nr*100,i/2, 100,280-i/2, DX,DY);

    XFillRectangle(display,pix[DB_BACK],gc,DX,DY+280-i/2,100,i/2);

    XSetClipOrigin(display,clip_gc[DOOR],DX-100+i,DY);
    XCopyArea(display,pix[DOOR],pix[DB_BACK],clip_gc[DOOR],
	      100-i,0, i,30, DX,DY);
    XCopyArea(display,pix[DOOR],pix[DB_BACK],clip_gc[DOOR],
	      100-i,250, i,30, DX,DY+250);
    XSetClipOrigin(display,clip_gc[DOOR],DX-i,DY);
    XCopyArea(display,pix[DOOR],pix[DB_BACK],clip_gc[DOOR],
	      100,0, i,30, DX+100-i,DY);
    XCopyArea(display,pix[DOOR],pix[DB_BACK],clip_gc[DOOR],
	      100,250, i,30, DX+100-i,DY+250);
    if (i>14)
    {
      XSetClipOrigin(display,clip_gc[DOOR],DX-i,DY);
      XCopyArea(display,pix[DOOR],pix[DB_BACK],clip_gc[DOOR],
		114,30,i-14,220,DX+114-i,DY+30);
      XSetClipOrigin(display,clip_gc[DOOR],DX-100+i,DY);
      XCopyArea(display,pix[DOOR],pix[DB_BACK],clip_gc[DOOR],
		100-i,30,i-14,220,DX,DY+30);
    }

    redraw_mask|=REDRAW_DOOR;
    BackToFront();
    Delay(10000);

    ColorCycling();
    if (game_status==MAINMENU)
      DoAnimation();
  }

  DR=dr;
}

long mainCounter(int mode)
{
  static struct timeval base_time = { 0, 0 };
  struct timeval current_time;
  long counter_ms;

  gettimeofday(&current_time,NULL);
  if (mode==0 || current_time.tv_sec<base_time.tv_sec)
    base_time = current_time;

  counter_ms = (current_time.tv_sec - base_time.tv_sec)*1000
             + (current_time.tv_usec - base_time.tv_usec)/1000;

  if (mode==1)
    return(counter_ms/10);	/* return 1/100 secs since last init */
  else
    return(counter_ms);		/* return 1/1000 secs since last init */
}

void InitCounter() /* set counter back to zero */
{
  mainCounter(0);
}

long Counter()	/* returns 1/100 secs since last call of InitCounter() */
{
  return(mainCounter(1));
}

long Counter2()	/* returns 1/1000 secs since last call of InitCounter() */
{
  return(mainCounter(2));
}

void WaitCounter(long value) 	/* wait for counter to reach value */
{
  long wait;

  while((wait=value-Counter())>0)
    microsleep(wait*10000);
}

void WaitCounter2(long value) 	/* wait for counter to reach value */
{
  long wait;

  while((wait=value-Counter2())>0)
    microsleep(wait*1000);
}

void Delay(long value)
{
  microsleep(value);
}

BOOL DelayReached(long *counter_var, int delay)
{
  long actual_counter = Counter();

  if (actual_counter>*counter_var+delay || actual_counter<*counter_var)
  {
    *counter_var = actual_counter;
    return(TRUE);
  }
  else
    return(FALSE);
}

unsigned long ReadPixel(Drawable d, int x, int y)
{
  XImage *pixelimage;
  unsigned long pixelvalue;

  if (!(pixelimage=XGetImage(display, d, x,y, 1,1, AllPlanes, ZPixmap)))
    return(0);

  pixelvalue = XGetPixel(pixelimage,0,0);
  XDestroyImage(pixelimage);
  return(pixelvalue);
}

void SetRGB(unsigned long pixel, 
	    unsigned short red, unsigned short green, unsigned short blue)
{
  XColor color;

  if (color_status==STATIC_COLORS)
    return;

  color.pixel = pixel;
  color.red = red;
  color.green = green;
  color.blue = blue;
  color.flags = DoRed | DoGreen | DoBlue;
  XStoreColor(display, cmap, &color);
  XFlush(display);
}
