#ifndef _Ver
#define _Ver "Test version"
#endif

/*
Ver. 2.02
*/

/*
                                    xtet42
  			    tetris for two players
			          
			          Written By:
			      Hugo Eide Gunnarsen
                     in the period : 31.09.91 - 02.10.91

Please report bugs to : hugogu@lise.unit.no

Please send mail to me if you have installed this game on your workstation.
Also send mail if you have any suggestion for score accounting, and high-score.

Enjoy the game.

*/

/*
1.61 - 2.00
Better support for b/w displays.
 
*/

#ifdef XTET42RANDOM
#define random rand  
#define srandom srand
#endif


#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>
#include <X11/keysym.h>

#ifndef _Finger
#define _Finger "finger"
#endif

#ifndef _Lock 
#define _Lock "/local/games/lib/xtet42/.xtet42.lock"
#endif

#ifndef _Unlock
#define _Unlock "/local/games/lib/xtet42/.xtet42.unlock"
#endif

#ifndef _Log
#define _Log "/local/games/lib/xtet42/.xtet42.log"
#endif

#ifndef _Hiscore
#define _Hiscore "/local/games/lib/xtet42/.xtet42.hiscore"
#endif

#ifndef _Hione
#define _Hione "/local/games/lib/xtet42/.xtet42.hiscore.single"
#endif

static int bricks[7][4][4][4]=
{{
{{1,0,0,0},{1,0,0,0},{1,0,0,0},{1,0,0,0}},
{{1,1,1,1},{0,0,0,0},{0,0,0,0},{0,0,0,0}},
{{1,0,0,0},{1,0,0,0},{1,0,0,0},{1,0,0,0}},
{{1,1,1,1},{0,0,0,0},{0,0,0,0},{0,0,0,0}}},
{{    
{1,0,0,0},{1,1,0,0},{0,1,0,0},{0,0,0,0}},
{{0,1,1,0},{1,1,0,0},{0,0,0,0},{0,0,0,0}},
{{1,0,0,0},{1,1,0,0},{0,1,0,0},{0,0,0,0}},
{{0,1,1,0},{1,1,0,0},{0,0,0,0},{0,0,0,0}}},
{{    
{1,1,0,0},{0,1,1,0},{0,0,0,0},{0,0,0,0}},
{{0,1,0,0},{1,1,0,0},{1,0,0,0},{0,0,0,0}},
{{1,1,0,0},{0,1,1,0},{0,0,0,0},{0,0,0,0}},
{{0,1,0,0},{1,1,0,0},{1,0,0,0},{0,0,0,0}}},
{{    
{1,1,0,0},{1,1,0,0},{0,0,0,0},{0,0,0,0}},
{{1,1,0,0},{1,1,0,0},{0,0,0,0},{0,0,0,0}},
{{1,1,0,0},{1,1,0,0},{0,0,0,0},{0,0,0,0}},
{{1,1,0,0},{1,1,0,0},{0,0,0,0},{0,0,0,0}}},
{{    
{1,0,0,0},{1,1,0,0},{1,0,0,0},{0,0,0,0}},
{{1,1,1,0},{0,1,0,0},{0,0,0,0},{0,0,0,0}},
{{0,1,0,0},{1,1,0,0},{0,1,0,0},{0,0,0,0}},
{{0,1,0,0},{1,1,1,0},{0,0,0,0},{0,0,0,0}}},
{{    
{1,0,0,0},{1,1,1,0},{0,0,0,0},{0,0,0,0}},
{{1,1,0,0},{1,0,0,0},{1,0,0,0},{0,0,0,0}},
{{1,1,1,0},{0,0,1,0},{0,0,0,0},{0,0,0,0}},
{{0,1,0,0},{0,1,0,0},{1,1,0,0},{0,0,0,0}}},
{{    
{1,0,0,0},{1,0,0,0},{1,1,0,0},{0,0,0,0}},
{{1,1,1,0},{1,0,0,0},{0,0,0,0},{0,0,0,0}},
{{1,1,0,0},{0,1,0,0},{0,1,0,0},{0,0,0,0}},
{{0,0,1,0},{1,1,1,0},{0,0,0,0},{0,0,0,0}}}};
 
static int botrig[7][4][2]=
{
{{0,3},{3,0},{0,3},{3,0}},
{{1,2},{2,1},{1,2},{2,1}},
{{2,1},{1,2},{2,1},{1,2}},
{{1,1},{1,1},{1,1},{1,1}},
{{1,2},{2,1},{1,2},{2,1}},
{{2,1},{1,2},{2,1},{1,2}},
{{1,2},{2,1},{1,2},{2,1}}
};

static int bonus[5]={0,25,63,138,345};
char *Hiscore;

int oldstakk[2][10][41],stakk[2][10][41];
int debug=0,winner,looser,pau=0,two;

typedef struct
{
  Display *display;
  int scn;
  int depth;
  Window win;
  GC gc;
  XEvent report;
  XFontStruct *font_info;
  char name[9];
  int height;
  int width;
  int color[8][3];
  int white,black;
  int type,r,x,y,ox,oy,or,offset;
  int count,score;
  int direction;
} User;

User scn[2];
int players = -1,endgame=0,speed = 40,rowsdown;
int score[2][15];
char name[2][15][10];

int GetColor(nr,r,g,b)
     int nr;
     int r;
     int g;
     int b;
{
  Colormap cmap;
  XColor exact_def;

  cmap=DefaultColormap(scn[nr].display,scn[nr].scn);
  exact_def.red=r;
  exact_def.green=g;
  exact_def.blue=b;
  if (!XAllocColor(scn[nr].display, cmap, &exact_def)) 
  {
    fprintf(stderr, "can't allocate color\n");
    exit(0);
  }
  return(exact_def.pixel);
}

#ifdef XTET42USLEEP
void usleep(i)
     int i;
{
  struct timeval tm;

  tm.tv_sec  = i/1000000;
  tm.tv_usec = i%1000000;
  select(0,0,0,0,&tm);
}
#endif
void 
load_font(display,font_info)
     Display *display;
     XFontStruct **font_info;
{
  char *fontname="-adobe-courier-*-r-*-*-12-*-*-*-*-*-*-*";

  if ((*font_info = XLoadQueryFont(display,fontname)) == NULL)
  {
    fprintf(stderr,"Cannot open %s font\n",fontname);
    exit( -1 );
  }
}

void getGC(display,win,gc,font_info,scn)
     Display *display;
     Window win;
     GC *gc;
     XFontStruct *font_info;
     int scn;
{
  unsigned long valuemask = 0;
  XGCValues values;
  
  *gc = XCreateGC(display, win, valuemask, &values);
  XSetFont(display, *gc, font_info->fid);
  XSetForeground(display, *gc, BlackPixel(display,scn));
}

void NewWindow(display_name)
     char *display_name;
{
  Display *display;
  unsigned int width, height;
  int x,y,n;
  unsigned int border_width = 4;
  XSizeHints size_hints;
  char *ut;
  char *window_name;
  XTextProperty windowName;
  unsigned long white,black;

  ut=(char *)malloc(81);
  if (display_name)
  {
    strcpy(ut,display_name);
    strcat(ut,":0.0");
  }
  else ut=NULL;

  if ((display=XOpenDisplay(ut))==NULL)
  {
    fprintf(stderr,"Can't open %s's display\n",display_name);
    exit(0);
  }
  else
  {
    x=y=0;
    players++;
    scn[players].display=display;
    strcpy((window_name=(char *)malloc(8+sizeof(_Ver))),"xtet42-");
    strcat(window_name,_Ver);
    load_font(scn[players].display,&scn[players].font_info);
    scn[players].scn = DefaultScreen(scn[players].display);
    scn[players].depth=DefaultDepth(scn[players].display,scn[players].scn);
    width = 280;
    if (two) height = 670;
    else height=340;
    scn[players].width=280;
    scn[players].height=670;
    white=WhitePixel(scn[players].display,scn[players].scn);
    black=BlackPixel(scn[players].display,scn[players].scn);
    if (scn[players].depth>2) {
      scn[players].color[7][0]=GetColor(players,64000,64000,64000);
      scn[players].color[7][1]=GetColor(players,48000,48000,48000);
      scn[players].color[7][2]=GetColor(players,32000,32000,32000);
    }
    else {
      scn[players].color[7][0]=black;
      scn[players].color[7][1]=white;
      scn[players].color[7][2]=black;
    }
    scn[players].win=XCreateSimpleWindow(scn[players].display,RootWindow(scn[players].display,scn[players].scn),x,y,width, height, border_width,WhitePixel(scn[players].display,scn[players].scn),scn[players].color[7][1]);/*WhitePixel(scn[players].display,scn[players].scn)*/
    size_hints.flags = PPosition | PMinSize | PMaxSize | PSize | PResizeInc;
    size_hints.min_width = 280;
    size_hints.min_height = height;
    size_hints.max_width = 280;
    size_hints.max_height = height;
    size_hints.width_inc=1;
    size_hints.height_inc=1;
    if (XStringListToTextProperty(&window_name,1,&windowName) ==(Status) 0) 
    {
      fprintf(stderr,"structure allocation for windowName failed.\n");
      exit(0);
    }
    XSetWMProperties(scn[players].display,scn[players].win,&windowName,NULL,NULL,0,&size_hints,NULL,NULL);
    XSelectInput(scn[players].display,scn[players].win, ExposureMask | KeyPressMask | ButtonPressMask | StructureNotifyMask | Button1MotionMask);
    getGC(scn[players].display,scn[players].win,&scn[players].gc,scn[players].font_info,scn[players].scn);
    XMapWindow(scn[players].display,scn[players].win);
    if (scn[players].depth>4) {
      scn[players].color[0][0]=GetColor(players,64000,0,0);
      scn[players].color[0][1]=GetColor(players,48000,0,0);
      scn[players].color[0][2]=GetColor(players,32000,0,0);
      scn[players].color[1][0]=GetColor(players,0,64000,0);
      scn[players].color[1][1]=GetColor(players,0,48000,0);
      scn[players].color[1][2]=GetColor(players,0,32000,0);
      scn[players].color[2][0]=GetColor(players,0,0,64000);
      scn[players].color[2][1]=GetColor(players,0,0,48000);
      scn[players].color[2][2]=GetColor(players,0,0,32000);
      scn[players].color[3][0]=GetColor(players,64000,64000,0);
      scn[players].color[3][1]=GetColor(players,48000,48000,0);
      scn[players].color[3][2]=GetColor(players,32000,32000,0);
      scn[players].color[4][0]=GetColor(players,0,64000,64000);
      scn[players].color[4][1]=GetColor(players,0,48000,48000);
      scn[players].color[4][2]=GetColor(players,0,32000,32000);
      scn[players].color[5][0]=GetColor(players,64000,0,64000);
      scn[players].color[5][1]=GetColor(players,48000,0,48000);
      scn[players].color[5][2]=GetColor(players,32000,0,32000);
      scn[players].color[6][0]=GetColor(players,48000,0,64000);
      scn[players].color[6][1]=GetColor(players,32000,0,48000);
      scn[players].color[6][2]=GetColor(players,8000,0,32000);
    }
    else {
      for(n=0;n<7;n++) {
	scn[players].color[n][0]=black;
	scn[players].color[n][1]=white;
	scn[players].color[n][2]=black;
      }
    }
    scn[players].type=random() % 7;
    scn[players].r=0;
    scn[players].x=4;
    scn[players].y=0;
    scn[players].ox=4;
    scn[players].oy=0;
    scn[players].score=0;
    scn[players].count=0;
    scn[players].offset=19;
  }
  free(ut);
}

void SetColor(nr,color)
     int nr;
     int color;
{
  XGCValues values;

  values.foreground=color;
  XChangeGC(scn[nr].display,scn[nr].gc,GCForeground,&values);
}

void DrawButton(updown,bar,nr,x,y,w,h,text)
     int updown;
     int bar;
     int nr;
     int x;
     int y;
     int w;
     int h;
     char *text;
{
  int u,l,len,xlen;

  u=0,l=2;
  if (updown) u=2,l=0;
  if (bar)
  {
    SetColor(nr,scn[nr].color[7][1]);
    XFillRectangle(scn[nr].display,scn[nr].win,scn[nr].gc,x+2,y+2,w-4,h-4);
  }
  SetColor(nr,scn[nr].color[7][u]);
  XDrawLine(scn[nr].display,scn[nr].win,scn[nr].gc,x,y,x,y+h-1);
  XDrawLine(scn[nr].display,scn[nr].win,scn[nr].gc,x,y,x+w-1,y);
  XDrawLine(scn[nr].display,scn[nr].win,scn[nr].gc,x+1,y,x+1,y+h-1);
  XDrawLine(scn[nr].display,scn[nr].win,scn[nr].gc,x,y+1,x+w-1,y+1);
  SetColor(nr,scn[nr].color[7][l]);
  XDrawLine(scn[nr].display,scn[nr].win,scn[nr].gc,x+w-2,y+2,x+w-2,y+h-2);
  XDrawLine(scn[nr].display,scn[nr].win,scn[nr].gc,x+w-1,y+1,x+w-1,y+h-1);
  XDrawLine(scn[nr].display,scn[nr].win,scn[nr].gc,x,y+h-1,x+w-1,y+h-1);
  XDrawLine(scn[nr].display,scn[nr].win,scn[nr].gc,x+1,y+h-2,x+w-2,y+h-2);
  if (text)
  {
    len=strlen(text);
    xlen=XTextWidth(scn[nr].font_info,text,len);
    XDrawString(scn[nr].display,scn[nr].win,scn[nr].gc,x+((w-xlen)>>1),y+14,text,len);
  }
}

void DrawSqr(nr,type,x,y)
     int nr;
     int type;
     int x;
     int y;
{
  int andre;
  
  x+=10;
  y+=10;
  if (pau) type=7;
  if (type==7)
  {
    SetColor(nr,scn[nr].color[7][1]);
    XFillRectangle(scn[nr].display,scn[nr].win,scn[nr].gc,x,y,16,16);    
  }
  else
  {
    SetColor(nr,scn[nr].color[type][1]);
    XFillRectangle(scn[nr].display,scn[nr].win,scn[nr].gc,x+2,y+2,12,12);
    SetColor(nr,scn[nr].color[type][0]);
    XDrawLine(scn[nr].display,scn[nr].win,scn[nr].gc,x,y,x,y+15);
    XDrawLine(scn[nr].display,scn[nr].win,scn[nr].gc,x,y,x+15,y);
    XDrawLine(scn[nr].display,scn[nr].win,scn[nr].gc,x+1,y,x+1,y+15);
    XDrawLine(scn[nr].display,scn[nr].win,scn[nr].gc,x,y+1,x+15,y+1);
    SetColor(nr,scn[nr].color[type][2]);
    XDrawLine(scn[nr].display,scn[nr].win,scn[nr].gc,x+14,y+2,x+14,y+14);
    XDrawLine(scn[nr].display,scn[nr].win,scn[nr].gc,x+15,y+1,x+15,y+15);
    XDrawLine(scn[nr].display,scn[nr].win,scn[nr].gc,x,y+15,x+15,y+15);
    XDrawLine(scn[nr].display,scn[nr].win,scn[nr].gc,x+1,y+14,x+14,y+14);
  }
  if (players)
  {
    andre=0;
    if (nr==0) andre=1;
    if (type==7)
    {
      SetColor(andre,scn[andre].color[7][1]);
      XFillRectangle(scn[andre].display,scn[andre].win,scn[andre].gc,x,654-y,16,16);
    }
    else
    {  
      SetColor(andre,scn[andre].color[type][1]);
      XFillRectangle(scn[andre].display,scn[andre].win,scn[andre].gc,x+2,654-y+2,12,12);
      SetColor(andre,scn[andre].color[type][0]);
      XDrawLine(scn[andre].display,scn[andre].win,scn[andre].gc,x,654-y,x,654-y+15);
      XDrawLine(scn[andre].display,scn[andre].win,scn[andre].gc,x,654-y,x+15,654-y);
      XDrawLine(scn[andre].display,scn[andre].win,scn[andre].gc,x+1,654-y,x+1,654-y+15);
      XDrawLine(scn[andre].display,scn[andre].win,scn[andre].gc,x,654-y+1,x+15,654-y+1);
      SetColor(andre,scn[andre].color[type][2]);
      XDrawLine(scn[andre].display,scn[andre].win,scn[andre].gc,x+14,654-y+2,x+14,654-y+14);
      XDrawLine(scn[andre].display,scn[andre].win,scn[andre].gc,x+15,654-y+1,x+15,654-y+15);
      XDrawLine(scn[andre].display,scn[andre].win,scn[andre].gc,x,654-y+15,x+15,654-y+15);
      XDrawLine(scn[andre].display,scn[andre].win,scn[andre].gc,x+1,654-y+14,x+14,654-y+14);
    }
  }
}


void DrawType(nr)
     int nr;
{
  int dx,dy;
  unsigned char typ,or;
  unsigned short x,y,r,t,i,j;
  
  dx=(x=scn[nr].x)-scn[nr].ox;
  dy=(y=scn[nr].y)-scn[nr].oy;
  r=scn[nr].r;
  or=scn[nr].or;
  t=scn[nr].type;
  x<<=4;
  y<<=4;
  if (dx!=0) 
  {
    if (dx==-1)
    {
      for(j=0;j<=botrig[t][r][0];j++)
      {
	if (bricks[t][r][0][j]==1) DrawSqr(nr,t,x,y+(j<<4));
	if (bricks[t][r][botrig[t][r][1]][j]==1) DrawSqr(nr,7,x+((botrig[t][r][1]+1)<<4),y+(j<<4));
     }
      for(j=0;j<=botrig[t][r][0];j++) for(i=0;i<botrig[t][r][1];i++)
      {
	if (bricks[t][r][i+1][j]!=bricks[t][r][i][j])
	{
	  if (bricks[t][r][i+1][j]) DrawSqr(nr,t,x+((i+1)<<4),y+(j<<4));
	  else DrawSqr(nr,7,x+((i+1)<<4),y+(j<<4));
	}
      }
    }
    else 
    {
      for(j=0;j<=botrig[t][r][0];j++)
      {
	if (bricks[t][r][0][j]==1) DrawSqr(nr,7,x-16,y+(j<<4));
	if (bricks[t][r][botrig[t][r][1]][j]==1) DrawSqr(nr,t,x+(botrig[t][r][1]<<4),y+(j<<4));
      }
      for(j=0;j<=botrig[t][r][0];j++) for(i=0;i<botrig[t][r][1];i++)
      {
	if (bricks[t][r][i][j]!=bricks[t][r][i+1][j])
	{
	  if (bricks[t][r][i][j]) DrawSqr(nr,t,x+(i<<4),y+(j<<4));
	  else DrawSqr(nr,7,x+(i<<4),y+(j<<4));
	}
      }  
    }
  }
  else if (dy==1)
  {
    for(i=0;i<=botrig[t][r][1];i++) 
    {
      if (bricks[t][r][i][botrig[t][r][0]]==1) 	DrawSqr(nr,t,x+(i<<4),y+((botrig[t][r][0])<<4));
      if (bricks[t][r][i][0]==1) DrawSqr(nr,7,x+(i<<4),y-16);
    }
    for(j=0;j<botrig[t][r][0];j++) for(i=0;i<=botrig[t][r][1];i++)
    {  
      typ=bricks[t][r][i][j];
      if (typ!=bricks[t][r][i][j+1]) 
      {
	if (typ==0) DrawSqr(nr,7,x+(i<<4),y+(j<<4));
	else DrawSqr(nr,t,x+(i<<4),y+(j<<4));
      }
    }
  }
  else if (or!=r)
  {
    for(j=0;j<4;j++) for(i=0;i<4;i++)
    {  
      typ=bricks[t][r][i][j];
      if (typ!=bricks[t][or][i][j]) 
      {
	if (typ==0) DrawSqr(nr,7,x+(i<<4),y+(j<<4));
	else DrawSqr(nr,t,x+(i<<4),y+(j<<4));
      }
    }
  }
  else
  {
    for(i=0;i<4;i++) for(j=0;j<4;j++) 
      if (bricks[t][r][j][i]==1) DrawSqr(nr,7,(scn[nr].ox<<4)+(j<<4),(scn[nr].oy<<4)+(i<<4));
    for(i=0;i<4;i++) for(j=0;j<4;j++) 
      if (bricks[t][r][j][i]==1) DrawSqr(nr,t,x+(j<<4),y+(i<<4));
  }
  scn[nr].ox=scn[nr].x;
  scn[nr].or=scn[nr].r;
  scn[nr].oy=scn[nr].y;
  XFlush(scn[0].display);
  if (players) XFlush(scn[1].display);
}

void DrawBar(nr)
     int nr;
{
  int add;
  if (players)
  {
    add=16+(scn[nr].offset<<4);
  
    SetColor(nr,scn[nr].color[7][0]);
    XDrawLine(scn[nr].display,scn[nr].win,scn[nr].gc,170,10,170,660);
    XDrawLine(scn[nr].display,scn[nr].win,scn[nr].gc,171,9,171,661);
    SetColor(nr,scn[nr].color[7][2]);
    XDrawLine(scn[nr].display,scn[nr].win,scn[nr].gc,8,9,8,660);
    XDrawLine(scn[nr].display,scn[nr].win,scn[nr].gc,9,9,9,659);
    SetColor(nr,scn[nr].color[7][1]); 
    XFillRectangle(scn[nr].display,scn[nr].win,scn[nr].gc,8,12+add,164,6);
    SetColor(nr,scn[nr].color[7][0]);
    XDrawLine(scn[nr].display,scn[nr].win,scn[nr].gc,9,10+add,171,10+add);
    XDrawLine(scn[nr].display,scn[nr].win,scn[nr].gc,8,11+add,171,11+add);
    SetColor(nr,scn[nr].color[7][2]);
    XDrawLine(scn[nr].display,scn[nr].win,scn[nr].gc,9,19+add,170,19+add);
    XDrawLine(scn[nr].display,scn[nr].win,scn[nr].gc,9,18+add,171,18+add);
  }
}

void DrawScore(nr)
     int nr;
{
  int andre;
  char score[10];
  
  andre=0;
  if (nr==0) andre=1;
  SetColor(nr,scn[nr].color[7][1]);
  XFillRectangle(scn[nr].display,scn[nr].win,scn[nr].gc,195,58,60,15);    
  if (players) SetColor(andre,scn[andre].color[7][1]);
  if (players) XFillRectangle(scn[andre].display,scn[andre].win,scn[andre].gc,195,388,60,15);    
  SetColor(nr,scn[nr].color[7][0]);
  sprintf(score,"%i",scn[nr].score);
  XDrawString(scn[nr].display,scn[nr].win,scn[nr].gc,195,68,score,strlen(score));  
  if(players)
  {
    SetColor(andre,scn[andre].color[7][0]);
    sprintf(score,"%i",scn[nr].score);
    XDrawString(scn[andre].display,scn[andre].win,scn[andre].gc,195,398,score,strlen(score)); 
  }
}

void ReDraw(nr)
     int nr;
{
  int sx,sy;

  for(sy=scn[nr].offset;sy>=0;sy--)
  {
    for(sx=0;sx<10;sx++)
    {
      DrawSqr(nr,stakk[nr][sx][sy],(sx<<4),(sy<<4));
    }
  }
  DrawBar(nr);
}

void Refresh(nr,off)
     int nr;
     int off;
{
  int sx,sy;

  for(sy=0;sy<=scn[nr].offset;sy++)
    for(sx=0;sx<10;sx++)
      if ((stakk[nr][sx][sy]!=oldstakk[nr][sx][sy])||(sy>=scn[nr].offset-off)) DrawSqr(nr,stakk[nr][sx][sy],(sx<<4),(sy<<4));

  DrawBar(nr);
}

void DrawList(nr)
     int nr;
{
  int n;
  char buf[81];
  
  if (players)
  {
    SetColor(nr,scn[nr].color[7][0]);
    XDrawLine(scn[nr].display,scn[nr].win,scn[nr].gc,170,10,170,660);
    XDrawLine(scn[nr].display,scn[nr].win,scn[nr].gc,171,9,171,661);
    SetColor(nr,scn[nr].color[7][2]);
    XDrawLine(scn[nr].display,scn[nr].win,scn[nr].gc,8,9,8,660);
    XDrawLine(scn[nr].display,scn[nr].win,scn[nr].gc,9,9,9,659);
    SetColor(nr,scn[nr].color[7][1]);
    XFillRectangle(scn[nr].display,scn[nr].win,scn[nr].gc,10,330,160,10);
  }
  DrawButton(0,1,nr,10,10,160,320,NULL);
  if (players) DrawButton(0,1,nr,10,340,160,320,NULL);
  if (!players)
  SetColor(nr,scn[nr].color[5][1]);
  if (players) XDrawString(scn[nr].display,scn[nr].win,scn[nr].gc,37,30,"Hiscore Winners",15);
  else XDrawString(scn[nr].display,scn[nr].win,scn[nr].gc,37,30,"Hiscore  Single",15);
  SetColor(nr,scn[nr].color[5][2]);
  for(n=0;n<15;n++)
  {
    sprintf(buf,"%-8s %6i",name[0][n],score[0][n]);
    XDrawString(scn[nr].display,scn[nr].win,scn[nr].gc,37,50+(19*n),buf,strlen(buf));
  }
  if (players)
  {
    SetColor(nr,scn[nr].color[2][1]);
    XDrawString(scn[nr].display,scn[nr].win,scn[nr].gc,37,360,"Hiscore Loosers",15);
    SetColor(nr,scn[nr].color[2][2]);
    for(n=0;n<15;n++)
    {
      sprintf(buf,"%-8s %6i",name[1][n],score[1][n]);
      XDrawString(scn[nr].display,scn[nr].win,scn[nr].gc,37,380+(19*n),buf,strlen(buf));
    }
  }
  scn[nr].offset=19;
  DrawBar(nr);
  XFlush(scn[nr].display);
}


void GameOver(nr)
     int nr;
{
  FILE *Inn;
  int mid,c,m,n;
  int now[2],high[2],who[2];
  char buf[81];
  time_t timenow;
  char timec[26];
  
  if (!players) nr=1;
  endgame=1;
  who[1]=nr;
  who[0]=0;
  if (who[1]==0) who[0]=1;
  now[0]=now[1]=17;
  high[0]=high[1]=16;

  XFlush(scn[0].display);
  if (players) XFlush(scn[1].display);
  winner=who[0];
  looser=who[1];
  if (!debug&&players)
  {
    DrawButton(1,1,who[1],190,300,70,20,"LOOSER");
    DrawButton(1,1,who[0],190,300,70,20,"WINNER");
  }
  else if (players)
  {
    DrawButton(1,1,who[1],190,300,70,20,"DEBUG");
    DrawButton(1,1,who[0],190,300,70,20,"DEBUG");
  }
  
  for(c=0;(c<10) && rename(_Unlock,_Lock)!=0;usleep(200000),c++);
  if (c==10) 
  {
    fprintf(stderr,"Sorry can't lock hiscore-file\n");
    exit(1);
  }
  Inn=fopen(Hiscore,"r");
  for(m=0;m<=players;m++)
  {
    for(n=0;n<15;n++)
    {
      fscanf(Inn,"%s %i\n",buf,&mid);
      score[m][n]=mid;
      strcpy(name[m][n],buf);
      if ((scn[who[m]].score>score[m][n])&&(now[m]==17)) now[m]=n;
      if (strcmp(scn[who[m]].name,name[m][n])==0) high[m]=n;
    }
  }
  fflush(Inn);
  fclose(Inn);

  if (!debug)
  {
    for(m=0;m<=players;m++)
    {
      if ((now[m]<15)&&(high[m]>15))
      {
	for(n=14;n>now[m];n--)
        {
	  strcpy(name[m][n],name[m][n-1]);
	  score[m][n]=score[m][n-1];
	}
	strcpy(name[m][now[m]],scn[who[m]].name);
	score[m][now[m]]=scn[who[m]].score;
      }
      else if (now[m]<high[m])
      {
	for(n=high[m];n>now[m];n--)
        {
	  strcpy(name[m][n],name[m][n-1]);
	  score[m][n]=score[m][n-1];
	}
	strcpy(name[m][now[m]],scn[who[m]].name);
	score[m][now[m]]=scn[who[m]].score;    
      }
      else if (now[m]==high[m]) score[m][now[m]]=scn[who[m]].score;  
    }
  
    Inn=fopen(Hiscore,"w");  
    for(n=0;n<15;n++) fprintf(Inn,"%s %i\n",name[0][n],score[0][n]);
    if (players) for(n=0;n<15;n++) fprintf(Inn,"%s %i\n",name[1][n],score[1][n]);
    fflush(Inn);
    fclose(Inn);

#ifdef Log
    if (players)
    {
      timenow=time(0);
      strcpy(timec,ctime(&timenow));
      timec[strlen(timec)-1]='\0';
      Inn=fopen(_Log,"a"); 
      fprintf(Inn,"%s - %i v %s - %i %s",scn[who[0]].name,scn[who[0]].score,scn[who[1]].name,scn[who[1]].score,timec);
      fflush(Inn);
      fclose(Inn);
    }
#endif
  }
  rename(_Lock,_Unlock);
  DrawList(0);
  if (players) DrawList(1);
  DrawButton(0,1,0,190,150,70,20,"New");
  if (players) DrawButton(0,1,1,190,150,70,20,"New");
}

int OneDown(nr)
     int nr;
{
  int sx,sy,mx,my,OK,andre,ant,n,speedbonus,rem;

  scn[nr].count=0;
  OK=1;
  for(sy=botrig[scn[nr].type][scn[nr].r][0];(sy>=0)&&(OK);sy--)
  {
    for(sx=0;(sx<=botrig[scn[nr].type][scn[nr].r][1])&&(OK);sx++)
    {
      if ((bricks[scn[nr].type][scn[nr].r][sx][sy]==1)&&(stakk[nr][scn[nr].x+sx][scn[nr].y+sy+1]<7)) OK=0;
    }
  }
  if (OK)
  {
    scn[nr].y++;
    DrawType(nr);
    return(1);
  }
  else
  {
    rem=0;
    for(sy=0;sy<=botrig[scn[nr].type][scn[nr].r][0];sy++) 
    {
      for(sx=0;sx<=botrig[scn[nr].type][scn[nr].r][1];sx++)
      {
	if (bricks[scn[nr].type][scn[nr].r][sx][sy]>0) stakk[nr][scn[nr].x+sx][scn[nr].y+sy]=scn[nr].type;
      }
    }
    andre=0;
    if (nr==0) andre=1;
    ant=0;
    for(n=sy=botrig[scn[nr].type][scn[nr].r][0];n>=0;n--)
    {
      OK=1;
      for(sx=0;sx<10;sx++) if (stakk[nr][sx][scn[nr].y+sy]==7) OK=0;
      if (OK) 
      {
	if (!rem)
	{
	  rem=1;
	  for(my=0;my<=scn[0].offset;my++)for(mx=0;mx<10;mx++)
	    oldstakk[0][mx][my]=stakk[0][mx][my];
	  if (players) for(my=0;my<=scn[1].offset;my++)for(mx=0;mx<10;mx++)
	    oldstakk[1][mx][my]=stakk[1][mx][my];
	}
	ant++;
	if (speed>30) 
	{
	  if ((rowsdown % 1)==0) speed-=1;
	}
	else if (speed>20) 
	{
	  if ((rowsdown % 2)==0) speed-=1;
	}
	else if (speed>10) 
	{
	  if ((rowsdown % 4)==0) speed-=1;
	}
	else if (speed>5) 
	{
	  if ((rowsdown % 8)==0) speed-=1;
	}
	else if(speed<=5)
	{
	  if ((rowsdown % 16)==0) speed-=1;
	}
	if (speed<1) speed=1;
	
	rowsdown++;
	if (players) 
        {
	  scn[nr].offset++;
	  scn[andre].offset--;
	}

	for(my=scn[nr].y+sy;my>0;my--)
	  for(mx=0;mx<10;mx++)
	    stakk[nr][mx][my]=stakk[nr][mx][my-1];
	for(mx=0;mx<10;mx++)
	  stakk[nr][mx][0]=7;
      }
      else sy--;
    }
    speedbonus=(int)((float)((41-speed+6)/3));
    scn[nr].score+=(5+bonus[ant])*speedbonus;
    DrawScore(nr);
    if ((ant>0)&&(players)) 
    {
      rem=ant;
      for(mx=0;mx<10;mx++) if (stakk[andre][mx][ant-1]<7)
      {
	GameOver(andre);
	return(0);
      }
      
      for(my=0;my<scn[andre].offset+2;my++)
	for(mx=0;mx<10;mx++)
	  stakk[andre][mx][my]=stakk[andre][mx][my+ant];

      for(my=scn[nr].offset+1;my>ant;my--)
	for(mx=0;mx<10;mx++) 
	  stakk[nr][mx][my]=stakk[nr][mx][my-ant];

      for(my=0;my<=ant;my++)
	for(mx=0;mx<10;mx++) 
	  stakk[nr][mx][my]=7;
      while ((ant>0)&&(scn[andre].y>0)) scn[andre].y--,ant--;
      OK=1;
      for(sy=botrig[scn[andre].type][scn[andre].r][0];(sy>=0)&&(OK);sy--)
	for(sx=0;(sx<=botrig[scn[andre].type][scn[andre].r][1])&&(OK);sx++)
	  if ((bricks[scn[andre].type][scn[andre].r][sx][sy]==1)&&(stakk[andre][scn[andre].x+sx][scn[andre].y+sy]<7)) OK=0;
      if (!OK) 
      {
	GameOver(andre);
	return(0);
      }
      else 
      {
	DrawType(andre);
	Refresh(nr,rem);
	if (players) Refresh(andre,-rem);
      }
    }
    else Refresh(nr,rem);
    scn[nr].ox=scn[nr].x=4;
    scn[nr].oy=scn[nr].y=0;
    scn[nr].type=random() % 7;
    scn[nr].or=scn[nr].r=0;
    OK=1;
    for(sy=botrig[scn[nr].type][scn[nr].r][0];(sy>=0)&&(OK);sy--)
      for(sx=0;(sx<=botrig[scn[nr].type][scn[nr].r][1])&&(OK);sx++)
	if ((bricks[scn[nr].type][scn[nr].r][sx][sy]==1)&&(stakk[nr][scn[nr].x+sx][scn[nr].y+sy]<7)) OK=0;
    if (!OK) 
    {
      GameOver(nr);
      return(0);
    }
    else DrawType(nr);
    return(0);
  }
}

void DrawFrame(nr)
     int nr;
{
  int andre;
  
  andre=0;
  if (nr==0) andre=1;
  if (players) 
  {
    DrawButton(0,0,nr,0,0,280,670,NULL);
    DrawButton(1,0,nr,8,8,164,654,NULL);
    DrawButton(1,0,nr,190,350,70,60,NULL);
  }
  else
  {
    DrawButton(0,0,nr,0,0,280,340,NULL);
    DrawButton(1,0,nr,8,8,164,324,NULL);
  }
  DrawButton(1,0,nr,190,20,70,60,NULL);
  
  XDrawString(scn[nr].display,scn[nr].win,scn[nr].gc,195,38,scn[nr].name,strlen(scn[nr].name)); 
  if (players) XDrawString(scn[nr].display,scn[nr].win,scn[nr].gc,195,368,scn[andre].name,strlen(scn[andre].name)); 
  XDrawString(scn[nr].display,scn[nr].win,scn[nr].gc,195,53,"Score:",6);
  if (players) XDrawString(scn[nr].display,scn[nr].win,scn[nr].gc,195,383,"Score:",6);
  SetColor(nr,scn[nr].color[7][1]);
  DrawScore(nr);
  DrawButton(0,0,nr,190,100,70,20,"Quit");
  if (endgame) DrawButton(0,1,nr,190,150,70,20,"New");
  else
  {
    if (pau) DrawButton(0,1,nr,190,150,70,20,"Continue");
    else DrawButton(0,1,nr,190,150,70,20,"Pause");
  }
  if (endgame)
  {
    if (!debug)
    {
      if (nr==looser) DrawButton(1,1,nr,190,300,70,20,"LOOSER");
      else DrawButton(1,1,nr,190,300,70,20,"WINNER");
    }
    else DrawButton(1,1,nr,190,300,70,20,"DEBUG");
  }
}
  
void RemovePlayers()
{
  int nr;
  
  for(nr=0;nr<players;nr++)
  {
    XUnloadFont(scn[nr].display,scn[nr].font_info->fid);
    XFreeGC(scn[nr].display,scn[nr].gc);
    XCloseDisplay(scn[nr].display);
  }
}

void Usage(prog)
     char *prog;
{
  fprintf(stderr,"%s [ -m1 <machine1> ] [ -p1 <player1> ] [ -m2 <machine2> [ -p2 <player2> ] ]\n",prog);
  exit(0);
}

void main(argc,argv)
     int argc;
     char **argv;
{
  char *m1,*m2,*p1,*p2;
  int c1=1,c2=1;
  int count,nr,rotate;
  char buf[10];
  int bufsize=10;
  KeySym keysym;
  XComposeStatus compose;
  int OK,sy,sx,subadd;
  Window root,child;
  unsigned int mask;
  int px,py,rx,ry,n,wx,wy,drop;

  srandom(time(0));
  m1=m2=p1=p2=NULL;
  for(n=1;n<argc;n++) {
    if (strcmp(argv[n],"-m1")==0) m1=argv[++n];
    if (strcmp(argv[n],"-m2")==0) m2=argv[++n];
    if (strcmp(argv[n],"-p1")==0) {
      p1=argv[++n];
      if (strlen(p1)>8) {
	fprintf(stderr,"Do not use names longer then 8 characters\n");
	exit(0);
      }
    }
    if (strcmp(argv[n],"-p2")==0) {
      p2=argv[++n];
      if (strlen(p2)>8) {
	fprintf(stderr,"Do not use names longer then 8 characters\n");
	exit(0);
      }
    }
    if (strcmp(argv[n],"-c1")==0) c1=-1;
    if (strcmp(argv[n],"-c2")==0) c2=-1;
  }

  if (!m2) two=0;
  else two=1;
  
  if (p2&&!m2) Usage(argv[0]);

  if (!p1) strcpy(scn[0].name,cuserid(NULL));
  else strcpy(scn[0].name,p1);
  NewWindow(m1);
  scn[0].direction=c1;
  if (two) {
    if (!p2) strcpy(scn[1].name,cuserid(NULL));
    else strcpy(scn[1].name,p2);
    NewWindow(m2);
    scn[1].direction=c2;
  }

  if (players) Hiscore=(char *)malloc(strlen(_Hiscore)+1);
  else Hiscore=(char *)malloc(strlen(_Hione)+1);
  if (players) strcpy(Hiscore,_Hiscore);
  else strcpy(Hiscore,_Hione);
  if (strcmp(scn[0].name,scn[1].name)==0) debug=1;
  if (scn[0].display==scn[1].display) debug=1;
  if (debug) fprintf(stderr,"Starting debug modus...\n");
  for(sy=0;sy<20;sy++) for(sx=0;sx<10;sx++) stakk[0][sx][sy]=7;
  for(sx=0;sx<10;sx++) stakk[0][sx][20]=1;
  DrawFrame(0);
  DrawBar(0);
  if (players)
  {
    for(sy=0;sy<20;sy++) for(sx=0;sx<10;sx++) stakk[1][sx][sy]=7;
    for(sx=0;sx<10;sx++) stakk[1][sx][20]=1;
    DrawFrame(1);
    DrawBar(1);
  }
  
  while (1)  
  {
    usleep(50000);

    for(nr=0;nr<=players;nr++) 
    {
      XDrawPoint(scn[nr].display,scn[nr].win,scn[nr].gc,640,0);
      if (!endgame&&!pau)
      {
	scn[nr].count++;
	if (scn[nr].count>speed) 
	{
	  scn[nr].count=0;
	  OneDown(nr);
	}
      }
      else
      {
	XDrawLine(scn[nr].display,scn[nr].win,scn[nr].gc,999,999,999,999);
      }
      if (XCheckWindowEvent(scn[nr].display,scn[nr].win,ExposureMask|KeyPressMask|ButtonPressMask|StructureNotifyMask,&scn[nr].report))
      {
	switch  (scn[nr].report.type) 
	{
	  case Expose:
	  if (scn[nr].report.xexpose.count!=0)
	    break;
	  DrawFrame(nr);
	  if (!endgame) 
	  {
	    ReDraw(0);
	    if (players) ReDraw(1);
	  }
	  DrawScore(0);
	  if (players) DrawScore(1);
	  if (endgame) DrawList(nr);
	  else 
	  {
	    if (players) DrawType(1);
	    DrawType(0);
	  }
	  break;
	case KeyPress:
	  count=XLookupString((XKeyEvent *)&scn[nr].report,(char *)buf,(int)bufsize,(KeySym *)&keysym,(XComposeStatus *)&compose);
	  if (!endgame&&!pau)
	  {
	    if ((keysym==32)||(keysym==XK_x)) 
	    {
	      if (OneDown(nr)==1) 
	      {
		drop=1;
		while ((scn[nr].y!=0)&&(!endgame)) OneDown(nr),drop++;
	      }
	      else drop=0;
	      if (drop>0)
	      {
		if (!endgame) scn[nr].score+=drop*(speed-41+6)/3;
		DrawScore(nr);
	      }
	      break;
	    }
	    rotate=0;
	    if ((keysym==65362)||(keysym==XK_k)||(keysym==XK_w)||(keysym==XK_c)) rotate=scn[nr].direction;
	    if (keysym==XK_v) rotate= -scn[nr].direction;
	    if (rotate!=0)
	    {
	      OK=1;
	      scn[nr].r+=rotate;
	      if (scn[nr].r==4) scn[nr].r=0;
	      else if (scn[nr].r==-1) scn[nr].r=3;
	      for(sy=botrig[scn[nr].type][scn[nr].r][0];(sy>=0)&&(OK);sy--)
		for(sx=0;(sx<=botrig[scn[nr].type][scn[nr].r][1])&&(OK);sx++)
		  if ((bricks[scn[nr].type][scn[nr].r][sx][sy]==1)&&(stakk[nr][scn[nr].x+sx][scn[nr].y+sy]<7)) OK=0;
	      
	      if (scn[nr].x+botrig[scn[nr].type][scn[nr].r][1]>9) OK=0;
	      scn[nr].r-=rotate;
	      if (scn[nr].r==4) scn[nr].r=0;
	      else if (scn[nr].r==-1) scn[nr].r=3;
	      if (OK==1)
	      {
		scn[nr].r+=rotate;
		if (scn[nr].r==4) scn[nr].r=0;
		else if (scn[nr].r==-1) scn[nr].r=3;
		if (!endgame) DrawType(nr);
	      }
	    }
	    subadd=0;
	    if ((keysym==65361)||(keysym==XK_h)||(keysym==XK_a)||(keysym==XK_b)) if (scn[nr].x>0) subadd= -1;
	    if ((keysym==65363)||(keysym==XK_l)||(keysym==XK_d)||(keysym==XK_m)) if (scn[nr].x<9-botrig[scn[nr].type][scn[nr].r][1]) subadd=1;
	    if (subadd!=0)
	    {
	      OK=1;
	      for(sy=botrig[scn[nr].type][scn[nr].r][0];(sy>=0)&&(OK);sy--)
		for(sx=0;(sx<=botrig[scn[nr].type][scn[nr].r][1])&&(OK);sx++)
		  if ((bricks[scn[nr].type][scn[nr].r][sx][sy]==1)&&(stakk[nr][scn[nr].x+sx+subadd][scn[nr].y+sy]<7)) OK=0;
	      if (OK) scn[nr].x+=subadd;
	      if (!endgame) DrawType(nr);
	    }
	    if ((keysym==65364)||(keysym==XK_j)||(keysym==XK_s)||(keysym==XK_n)) OneDown(nr);
	  }
	  break;
	case ButtonPress:
	  XQueryPointer(scn[nr].display,scn[nr].win,&root,&child,&px,&py,&wx,&wy,&mask);
	  XTranslateCoordinates (scn[nr].display,scn[nr].win,root,0,0,&rx,&ry,&child);
	  if (px-rx>200)
	  {
	    if (px-rx<265)
	    {
              if ((py-ry>150)&&(py-ry<170)) 
              {
                DrawButton(1,0,nr,190,150,70,20,NULL);
                XFlush(scn[nr].display);
                usleep(250000);
                DrawButton(0,0,nr,190,150,70,20,NULL);        
                XFlush(scn[nr].display);              
		if (endgame)
		{
		  endgame=0;
		  for(n=0;n<=players;n++)
		  {
		    SetColor(n,scn[n].color[7][1]);
		    XFillRectangle(scn[n].display,scn[n].win,scn[n].gc,190,300,70,20);
		    scn[n].oy=scn[n].y=0;
		    scn[n].score=0;
		    scn[n].ox=scn[n].x=4;
		    scn[n].type=random() % 7; 
		    scn[n].offset=19;
		    scn[n].count=speed=40;
		    for(sy=0;sy<20;sy++) for(sx=0;sx<10;sx++) 
		      stakk[n][sx][sy]=7;
		    for(sx=0;sx<10;sx++) stakk[n][sx][20]=1;
		    DrawFrame(n);
		    ReDraw(n);
		    DrawType(n);
		  }
		}
		else
		{
		  if (pau) pau=0;
		  else pau=1;
		  ReDraw(0);
		  if (players) ReDraw(1);
		  if (pau) 
		  {
		    DrawButton(0,1,0,190,150,70,20,"Continue");
		    if (players) DrawButton(0,1,1,190,150,70,20,"Continue");
		  }
		  else 
		  {
		    DrawButton(0,1,0,190,150,70,20,"Pause");
		    if (players) DrawButton(0,1,1,190,150,70,20,"Pause");
		    if (players) DrawType(1);
		    DrawType(0);
		  }
		}
              }
	      if ((py-ry>100)&&(py-ry<120)) 
	      {
		DrawButton(1,0,nr,190,100,70,20,NULL);
		XFlush(scn[nr].display);
		usleep(500000);
		DrawButton(0,0,nr,190,100,70,20,NULL);	      
		XFlush(scn[nr].display);
		RemovePlayers();
		exit(0);
	      }
	    }
	  }
	  break;
	default:
	  break;
	}
      }
    }
  }
}






