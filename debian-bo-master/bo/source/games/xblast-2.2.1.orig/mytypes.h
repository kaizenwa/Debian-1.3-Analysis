/*
 * Program XBLAST V2.2.1 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * January 26th, 1997
 * started August 1993
 *
 * File: mytypes.h
 * types declarations
 *
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public Licences as by published
 * by the Free Software Foundation; either version 2; or (at your option)
 * any later version
 *
 * This program is distributed in the hope that it will entertaining,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of 
 * MERCHANTABILTY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
 * Publis License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.
 * 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifndef _MYTYPES_H
#define _MYTYPES_H

#include <sys/types.h>
#include <sys/uio.h>

/*
 *  PFV Pointer to void/int functions
 */

typedef void (*PFV)();
typedef int (*PFI)();

/* 
 * enumeration types 
 */

/*
 * team mode type
 */
typedef enum {
  TM_Single = 0, TM_Team, TM_Double
} XBTeamMode;

/*
 * player health
 */
typedef enum {
  Healthy = 0, IllBomb, IllSlow, IllRun, IllMini, IllEmpty, IllInvisible, 
  IllMalfunction, IllReverse, IllTeleport,
  MAX_ILL
} BMHealth;

/*
 *  some enum types for levels
 */

typedef enum {
  ShadowNone=0, ShadowBlock, ShadowExtra, ShadowFull,
  MAX_SHADOW
} BMShadowMode;

typedef enum {
  DEnone = 0, DEsingle, DEall, DEspecial, DEget
} BMExtraDistribution;

typedef enum {
  BTFree = 0, BTShadow, BTBlock, BTBlockRise, BTExtra, BTExtraOpen, BTBomb, 
  BTRange, BTSick, BTSpecial, BTVoid, BTEvil
} BMMapTile;

typedef enum {
  GoStop = 0, GoUp, GoLeft, GoDown, GoRight, GoDefault, 
  MAX_DIR
} BMDirection;

typedef enum {
  PM_Same = 0, PM_Polar, PM_Right, PM_Inner, PM_LeftRight, PM_Below, 
  PM_Horizontal, PM_Vertical, PM_Circle,
  MAX_PM
} BMPosMod;

typedef enum {
  FUSEshort = 0, FUSEnormal, FUSElong
} BMFuseTime;

typedef enum {
  BMTevil = -3, BMTspecial, BMTdefault, 
  BMTnormal, BMTnapalm, BMTblastnow, BMTclose, BMTfirecracker, BMTfirecracker2, 
  BMTconstruction, BMTthreebombs, BMTgrenade, BMTtrianglebombs, BMTdestruction, 
  BMTfungus, BMTrenovation, BMTpyro, BMTpyro2, BMTrandom,
  NUM_BMT
} BMBombType;

/*
 *  position vector
 */
typedef struct {
  short y,x;
} BMPosition;

/*
 * Pointer to bitmap data
 */
typedef struct {
  int width, height;
  unsigned char *data;
} BitmapStruct;


/* player to display relation */
typedef struct {
  int num;
  int p1, p2;
} DispPlayer;


/*
 * Database entries
 */
typedef struct {
  char *name;
  char *value;
} BMEntry;

/*
 * config parameters 
 */
typedef struct {
  int num_player;
  int num_disp;
  int default_disp;
  XBTeamMode team_mode;
  int fork;
  int com_mode;
  char *display[MAX_DISPLAY];
  int pl_at_disp[MAX_PLAYER];
  DispPlayer disp_player[MAX_PLAYER];
} XBConfig;

/*
 *  Setup parameters
 */

typedef struct {
  int max_victories;
  int max_lives;
  int start_level;
  int random_mode;
  int random_spos;
  int sound_flag;
  int frame_time;
  int color_mode;
  int print_stat;
  char *use_level;
} XBSettings;

/*
 * Extra Probabilities
 */

typedef struct 
{
  int bomb;
  int range;
  int ill;
  int invinc;
  int evil;
} BMExtraProb;

/*
 * Block tile definition
 */

typedef struct {
  int id;
  char *fg;
  char *bg;
  char *add;
} BMBlockTile;

/* 
 * shrink element structure 
 */

/* generic shrink data */
typedef struct {
  int time;
  BMMapTile block;
  short x,y;
} ShrinkGeneric;

/* gd shrink data */
typedef struct {
  int x,y;
  int offset;
  int level;
} shri_data;

typedef int shri_xoff_data;

typedef struct {
  int offset;
  int level;
  int block;
} shri_style2;

typedef struct {
  int num;
  shri_style2 *styl;
} shri_style;

/* Scramble Structure */
typedef struct {
  int time;
  int num_blocks;
  BMPosition *blocks;
} ScrambleStruct; 

/*
 *  Leveldata Struct
 */

typedef struct {
  char *name;
  char *author;
  char *resource;
  char *DSCtips;
  long game_mode;
  void *data;
  char *file;
} BMLevel;

typedef struct {
  PFV shrink_func;
  ScrambleStruct scrdraw;
  ScrambleStruct scrdel;
} BMShrinkData;

typedef struct {
  PFV init_func;
  PFV game_func;
  PFV extra_func;
  PFV key_func;
} BMFuncData;

typedef struct {
  short          bombs;
  short          range;
  BMPosition     position[MAX_PLAYER];
  BMPosMod       pos_mod;
  short          pm_radius;
  BMHealth       init_health;
  unsigned short init_flags;
} BMPlayerData;

typedef struct {
  BMShadowMode        shadow;
  BMExtraDistribution distrib_extras;
  BMExtraProb         prob;
  BMMapTile           maze[MAZE_W][MAZE_H];
} BMMapData;

typedef struct {
  PFV         bomb_click;
  PFV         wall_click;
  PFV         player_click;
  BMDirection bomb_dir;
  BMFuseTime  fuse_time;
  BMBombType  defaultBMT;
  BMBombType  buttonBMT;
  BMBombType  evilBMT;
} BMBombData;

typedef struct {
  BMBlockTile block[MAX_BLOCK];
} BMGraphicsData;

typedef struct {
  BMLevel        level;
  BMShrinkData   shrink;
  BMFuncData     func;
  BMPlayerData   player;
  BMBombData     bomb;
  BMGraphicsData graphics;
  BMMapData      map;
} BMLevelData;

typedef struct _level_list {
  BMLevel *data;
  struct _level_list *next;
} BMLevelList;

/*
 * colors etc
 */
typedef struct {
  char *title1, *title2;
  char *lighttext1, *lighttext2;
  char *darktext1, *darktext2;
  char *statusled, *statusfg, *statusbg;
  char *expl1, *expl2, *expl3;
  char *bomb;
} DisplayColor;

typedef struct {
  char *helmet;
  char *face;
  char *body;
  char *hands_feet;
  char *arms_legs;
  char *backpack;
} PlayerColor;

typedef struct {
  int helmet;
  int face;
  int body;
  int hands_feet;
  int arms_legs;
  int backpack;
} PlayerColorCell;

typedef struct {
  PFV func;
  char *msg;
} FuncInfo;

/*
 * BMPlayer 
 */

typedef struct _bmplayer {
  int y, x;
  int id, team;
  int sprite, anime;
  int disp;
  BMDirection d_soll,d_ist,d_look;
  int invincible;
  int dying;
  int stunned;
  BMHealth health;
  BMHealth illness;
  int illtime;
  int junkie;
  int lives;
  int score;
  int range;
  int bombs;
  unsigned int extra_flags;
  int special_bombs;
  int remote_control;
  int kick;
  int air_button;
  int victories;
  int teleport;
  int cloaking;
  int num_extras;
  int abort;
} BMPlayer;

typedef struct {
  char *name;
  char *tag;
  char *pause;
  char *winlevel;
  char *wingame;
  char *loselife;
  char *loselevel;
  char *gloat;
  char *welcome;
  char *abort;
  char *abortcancel;
} PlayerStrings;

/* player action at last turn */
typedef struct {
  int player;
  int dir;
  int bomb;
  int special;
  int pause;
  int abort;
} PlayerAction;

/* key action : link keycode(keysyms) to variables and values */
typedef struct {
  int *addr;
  int value;
} KeyPressAction;

typedef struct {
  char *keysym;
  int *addr;
  int value;
} KeyPressDefine;

/*
 * Sprites
 */

#define SPM_NO_DISPLAY    0
#define SPM_AT_DISPLAY(d) (1<<(d))
#define SPM_ALL_DISPLAYS  ((1<<(MAX_DISPLAY)) - 1)
#define SPM_MASK_ONLY     (1<<(MAX_DISPLAY))


typedef struct {
  int type;
  PFV draw;
  int ysort;
  int x,y;
  int mode;
  int anime;
  int player;
} PlayerSprite;

typedef struct {
  int type;
  PFV draw;
  int ysort;
  int x,y;
  int mode;
  int anime;
} BombSprite;

typedef struct {
  int type;
  PFV draw;
  int ysort;
  int x,y;
  int mode;
  int anime;
} AnySprite;

typedef union {
  int             type ;
  AnySprite       any;
  PlayerSprite    player;
  BombSprite      bomb;
} Sprite;
    


typedef struct explosion {
  BMPlayer *player;
  int range;
  int x,y;
  int dx,dy;       /* small deviations from x and y */
  BMDirection dir; /* Direction */
  int malfunction; /* flag for malfunction illness */
  int count;
  int blink;
  BMBombType type;
  int type_extr;
  struct explosion *next;
} Explosion;


typedef struct {
  double x,y;
} BMPoint;

typedef struct {
  int x, y;
  int w, h;
} BMRectangle;

typedef struct {
  int fg, bg, add;
} XBColorTriple;

/* ipc connection struct */
typedef struct {
  int in, out;
  int nkey_in;
  int nkey_out;
  int skey_in;
  int skey_out;
  struct iovec key_in[MAX_PLAYER]; 
  struct iovec key_out[MAX_PLAYER];
  int pid;
} XBConnection;

#ifdef DEBUG
/* debug settings */
typedef struct {
  int no_title;
} XBDebug;
#endif

#endif
/*
 * end of file mytype.h
 */
