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
*  main.h                                                  *
*                                                          *
*  Letzte Aenderung: 29.09.1994                            *
***********************************************************/

#ifndef MAIN_H
#define MAIN_H

#define XK_MISCELLANY
#define XK_LATIN1

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Intrinsic.h>
#include <X11/keysymdef.h>

#include XPM_INCLUDE_FILE

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

typedef int BOOL;

#define TRUE		1
#define FALSE		0

#define MIN(a,b) 	(((a)<(b))?(a):(b))
#define MAX(a,b) 	(((a)>(b))?(a):(b))
#define ABS(a)		(((a)<0)?(-(a)):(a))

#define WIN_XPOS	0
#define WIN_YPOS	0
#define WIN_XSIZE	640
#define WIN_YSIZE	400
#define FIELDX		16
#define FIELDY		12

#define BACK		0
#define DOOR		1
#define	FONTBIG		2
#define FONTSMALL	3
#define MAGICOLOR	4
#define DB_BACK		5
#define DB_DOOR		6

/* bis auf weiteres... */
/* Pixmaps with Xpm or X11 Bitmap files */
#define PIX_BACK        BACK
#define PIX_DOOR        DOOR
#define PIX_BIGFONT     FONTBIG
#define PIX_SMALLFONT   FONTSMALL
/* Pixmaps without them */
#define PIX_DB_BACK     DB_BACK
#define PIX_DB_DOOR     DB_DOOR

#define NUM_PICTURES	5
#define NUM_PIXMAPS	7

#define MAX_PLAYERS	10
#define MAX_NAMELEN	(10+1)

struct PictureFile
{
  char *picture_filename;
  char *picturemask_filename;
};

struct DamageList
{
  int Nr,Wk;
  int XP,YP;
  int Mr;
};

struct HiScore
{
  char Name[8];
  int Score;
};

struct PacMan
{
  int XP,YP;
  int Dr;
};

struct XY
{
  int x,y;
};

struct PlayerInfo
{
  char login_name[MAX_NAMELEN];
  char alias_name[MAX_NAMELEN];
  BOOL last_used;
  int handicap;
  unsigned int setup;
};

extern Display	       *display;
extern int		screen;
extern Window  		window;
extern GC      		gc, line_gc[2];
extern GC		clip_gc[];
extern XImage 	       *image[];
extern Pixmap		clipmask[];
extern Pixmap		pix[];
extern XImage	       *imagemask;
extern XpmAttributes 	xpm_att[];
extern Drawable    	drawto;
extern Colormap		cmap;

extern int		sound_pipe[2];
extern int		sound_device;
extern char	       *sound_device_name;
extern int     		width, height;
extern unsigned long	pen_fg, pen_bg, pen_ray, pen_magicolor[2];

extern int		game_status;
extern int		button_status;
extern int		color_status;
extern int		sound_status, sound_on;
extern int		sound_loops_allowed, sound_loops_on;
extern int		sound_music_on;
extern int		toons_on;

extern BOOL		redraw[16][12];
extern int		redraw_mask;
extern int		redraw_tiles;

extern int		Ray[FIELDX][FIELDY], Ur[FIELDX][FIELDY];
extern int		Hit[FIELDX][FIELDY], Box[FIELDX][FIELDY];
extern int		level_nr, leveltime;

extern struct PlayerInfo player;

extern struct DamageList	DamList[300],G[2],First;
extern struct HiScore		highscore[MAX_PLAYERS];
extern struct PacMan		Pac[192],Cyc[192];
extern struct XY		Step[16];

extern short		Sign[16];
extern short		Els[];

extern short 		RayList[300][2];
extern short 		RP,DP,GP,DR,SC,OV,AW;
extern short 		DBM,ABM,TWO,MS2,PLY,IRQ,SPK;
extern short 		SR,PM,EL,EN,LW,BL,SL,LP,WN,PT,DO,OB,BK,S1,RT,RY;
extern short		SRx,SRy;
extern short 		LX,LY,XS,YS,EX,EY, OK,BT,MT,OL,CR,MB,CB,CC[4];
extern short 		Pal,r,d,x,y,k,l,ll;
extern short 		Sprite[3],EC,Ec,OC,Oc,PC,Pc,CT,Ct,i,j;

extern char	       *progname;


/* often used screen positions */
#define SX		8
#define SY		8
#define REAL_SX		(SX-2)
#define REAL_SY		(SY-2)
#define DX		534
#define DY		60
#define TILEX		32
#define TILEY		32
#define SXSIZE		FIELDX*TILEX
#define SYSIZE		FIELDY*TILEY
#define FULL_SXSIZE	(2+SXSIZE+2)
#define FULL_SYSIZE	(2+SYSIZE+2)
#define DXSIZE		100
#define DYSIZE		280

/* font type and colors */
#define FS_SMALL	0
#define FS_BIG		1
#define FC_RED		0
#define FC_BLUE		1
#define FC_GREEN	2
#define FC_YELLOW	3

/* values for game_status */
#define MAINMENU	0
#define PLAYING		1
#define LEVELED		2
#define HELPSCREEN	3
#define CHOOSENAME	4
#define TYPENAME	5
#define HALLOFFAME	6
#define SETUP		7
#define EXITGAME	8

/* return values for GameActions */
#define ACT_GO_ON	0
#define ACT_GAME_OVER	1
#define ACT_NEW_GAME	2

/* values for color_status */
#define STATIC_COLORS	0
#define	DYNAMIC_COLORS	1

#ifndef GFX_PATH
#define GFX_PATH	"./graphics"
#endif

#ifndef LEVEL_PATH
#define LEVEL_PATH	"."
#endif
#ifndef SCORE_PATH
#define SCORE_PATH	"."
#endif
#ifndef NAMES_PATH
#define NAMES_PATH	"."
#endif

#define LEVEL_NAME	"RAY.level"
#define SCORE_NAME	"RAY.score"
#define NAMES_NAME	"RAY.names"
#define LEVEL_FILE	LEVEL_PATH "/" LEVEL_NAME
#define SCORE_FILE	SCORE_PATH "/" SCORE_NAME
#define NAMES_FILE	NAMES_PATH "/" NAMES_NAME

#define LEVEL_PERMS	(S_IRUSR|S_IWUSR | S_IRGRP|S_IWGRP | S_IROTH|S_IWOTH)
#define SCORE_PERMS	(S_IRUSR|S_IWUSR | S_IRGRP|S_IWGRP | S_IROTH|S_IWOTH)
#define NAMES_PERMS	(S_IRUSR|S_IWUSR | S_IRGRP|S_IWGRP | S_IROTH|S_IWOTH)

#define NAMES_COOKIE12	"MIRRORMAGIC_NAMES_FILE_VERSION_1.2"
#define NAMES_COOKIE	"MIRRORMAGIC_NAMES_FILE_VERSION_1.3"
#define NAMES_COOKIE_LEN	(strlen(NAMES_COOKIE)+1)

/* Leerer Login- und Alias-Name */
#define EMPTY_LOGIN	"NO_LOGIN"
#define EMPTY_ALIAS	"NO_NAME"

/* values for button_status */
#define MB_NOT_PRESSED	FALSE
#define MB_RELEASED	FALSE
#define MB_PRESSED	TRUE
#define MB_MENU_CHOICE	FALSE
#define MB_MENU_MARK	TRUE
#define MB_LEFT		1
#define MB_MIDDLE	2
#define MB_RIGHT	3

/* values for redraw_mask */
#define REDRAW_ALL	(1L<<0)
#define REDRAW_FIELD	(1L<<1)
#define REDRAW_TILES	(1L<<2)
#define REDRAW_DOOR	(1L<<3)
#define REDRAWTILES_TH	FIELDX*FIELDY/2

/* I have forgotten what these values stand for... */
#define glx 		38
#define gly 		24
#define gbx 		30
#define gby 		64
#define gsx 		24
#define gsy 		102
#define gex 		8
#define gey 		158
#define gox 		60
#define goy 		158
#define geey 		(gey+98)
#define goey 		(goy+100)
#define GLX 		(DX+glx)
#define GLY 		(DY+gly)
#define GBX 		(DX+gbx)
#define GBY 		(DY+gby)
#define GSX 		(DX+gsx)
#define GSY 		(DY+gsy)
#define GEX 		(DX+gex)
#define GEY 		(DY+gey)
#define GOX 		(DX+gox)
#define GOY 		(DY+goy)
#define GEEY 		(DY+geey)
#define GOEY 		(DY+goey)

/* sound control */

#define ST(x)		(((x)-8)*16)

/*
#define ST(x) 		(-(x)-1)
*/

#define SOUNDS 		19

#endif
