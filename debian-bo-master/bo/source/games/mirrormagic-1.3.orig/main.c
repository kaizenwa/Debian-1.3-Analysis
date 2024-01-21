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
*  main.c                                                  *
*                                                          *
*  Letzte Aenderung: 29.09.1994                            *
***********************************************************/

#include "main.h"
#include "init.h"
#include "events.h"
#include "sound.h"

Display        *display;
int		screen;
Window  	window;
GC      	gc, line_gc[2];
GC		clip_gc[NUM_PICTURES];
Pixmap		pix[NUM_PIXMAPS];
Pixmap		clipmask[NUM_PICTURES];
XImage	       *imagemask;
XpmAttributes 	xpm_att[NUM_PICTURES];
Drawable        drawto;
Colormap	cmap;

int		sound_pipe[2];
int		sound_device;
char	       *sound_device_name = SOUND_DEVICE;
int     	width, height;
unsigned long	pen_fg, pen_bg, pen_ray, pen_magicolor[2];

int		game_status = MAINMENU;
int		button_status = MB_NOT_PRESSED;
int		color_status;
int	    	sound_status = SOUND_STATUS, sound_on = TRUE;
int		sound_loops_allowed = FALSE, sound_loops_on = FALSE;
int		sound_music_on = FALSE;
int		toons_on = TRUE;

BOOL		redraw[FIELDX][FIELDY];
int		redraw_mask;
int		redraw_tiles;

int		Ray[FIELDX][FIELDY], Ur[FIELDX][FIELDY];
int		Hit[FIELDX][FIELDY], Box[FIELDX][FIELDY];
int		level_nr, leveltime;

struct PlayerInfo player;

struct DamageList    	DamList[300],G[2],First;
struct HiScore      	highscore[MAX_PLAYERS];
struct PacMan         	Pac[192],Cyc[192];
struct XY		Step[16] =
{
   1,0,  2,-1, 1,-1, 1,-2, 0,-1, -1,-2,-1,-1,-2,-1,
   -1,0, -2,1, -1,1, -1,2, 0,1,  1,2,  1,1,  2,1
};

short Sign[16] =
{
   0xA,0xF,0xB,0xF,0x3,0xF,0x7,0xF,
   0x5,0xF,0xD,0xF,0xC,0xF,0xE,0xF
};

short Els[] =
{
   119,1,17,22,25,29,30,31,0,114,98,97,139,117,135,149,
   145,143,144,96,147,148,150,151
};

short RayList[300][2];
short RP,DP,GP,DR=0,SC,OV,AW;
short DBM,ABM=0,TWO=0,MS2=0,PLY=0,IRQ=0,SPK=0;
short SR,PM,EL,EN,LW,BL,SL,LP,WN,PT,DO,OB,BK,S1,RT,RY=1;
short SRx,SRy;
short LX,LY,XS,YS,EX,EY, OK,BT,MT,OL,CR=0,MB=0,CB=0,CC[4];
short Pal,r,d,x,y,k,l,ll;
short Sprite[3],EC,Ec,OC,Oc,PC,Pc,CT,Ct,i,j;

char 		*progname;

int main(int argc, char *argv[])
{
  progname = argv[0];
  OpenAll(argc,argv);

  EventLoop();

  CloseAll();
  exit(0);
}




