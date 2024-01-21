/*
    XBlockOut a 3D Tetris

    Copyright (C) 1992,1993,1994  Thierry EXCOFFIER

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 1, or (at your option)
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

    Contact: Thierry.EXCOFFIER@ligia.univ-lyon1.fr
*/

#include "ansi.h"

/* If the widthest text change (no more the much long) then
   modify initmenu.c in consequence */

#define HARD_DRAW	"To hard drawing"
#define SMOOTH_DRAW	"To smooth drawing"
#define WIRE_DRAW	"To wire frame"
#define TRANSP_DRAW	"To transparent faces"
#define USER_LAND	"User land"
#define SMALL_LAND	"Small land"
#define BIG_LAND	"Big land"
#define FLAT_PIECE	"Flat pieces"
#define SIMPLE_PIECE	"Simple pieces"
#define COMPLEX_PIECE	"Complex pieces"
#define TRAINING_PLAY	"To normal play"
#define NORMAL_PLAY	"To training play"
#define SHOW_ZOO	"Show zoo"
#define HIDE_ZOO	"Hide zoo"
#define SHOW_SCORE	"Show score"
#define HIDE_SCORE	"Hide score"
#define STOPPED_GAME	"STOPPED"
#define RUNNING_GAME	"RUNNING"
#define SUSPENDED_GAME	"SUSPENDED"
#define SHOW_GAME	"SHOW"
#define DEMO_GAME	"DEMO"
#define SPEED_GAME	"SPEEDTEST"



extern void fctnextpiece(R2(struct moving_button *b,int but)) ;
extern void fctland(R2(struct moving_button *b,int but)) ;
extern void fcttype(R2(struct moving_button *b,int but)) ;
extern void fctwidth(R2(struct moving_button *b,int but)) ;
extern void fctheight(R2(struct moving_button *b,int but)) ;
extern void fctdepth(R2(struct moving_button *b,int but)) ;
extern void fctlevel(R2(struct moving_button *b,int but)) ;
extern void fcttraining(R2(struct moving_button *b,int but)) ;
extern void fctvolume(R2(struct moving_button *b,int but)) ;
extern void fctzoo(R2(struct moving_button *b,int but)) ;
extern void fctscore(R2(struct moving_button *b,int but)) ;
extern void fctsmooth(R2(struct moving_button *b,int but)) ;
extern void fctdraw(R2(struct moving_button *b,int but)) ;
extern void fctkey(R2(struct moving_button *b,int but)) ;
extern void fctquit(R2(struct moving_button *b,int but)) ;
extern void fctcopyright(R2(struct moving_button *b,int but)) ;

extern void  textscore(R1(struct moving_button *b)) ;
extern void  textthescore(R1(struct moving_button *b)) ;
extern void  texthiscore(R1(struct moving_button *b)) ;
extern void  textcube(R1(struct moving_button *b)) ;
extern void  textbloc(R1(struct moving_button *b)) ;
extern void  textdlevel(R1(struct moving_button *b)) ;
extern void  textlevel(R1(struct moving_button *b)) ;
extern void  textnextpiece(R1(struct moving_button *b)) ;
extern void  textland(R1(struct moving_button *b)) ;
extern void  texttype(R1(struct moving_button *b)) ;
extern void  textwidth(R1(struct moving_button *b)) ;
extern void  textheight(R1(struct moving_button *b)) ;
extern void  textdepth(R1(struct moving_button *b)) ;
extern void  textstartlevel(R1(struct moving_button *b)) ;
extern void  texttraining(R1(struct moving_button *b)) ;
extern void  textvolume(R1(struct moving_button *b)) ;
extern void  textzoo(R1(struct moving_button *b)) ;
extern void  textframe(R1(struct moving_button *b)) ;
extern void  textsmooth(R1(struct moving_button *b)) ;
extern void  textdraw(R1(struct moving_button *b)) ;
extern void  textkey(R1(struct moving_button *b)) ;
extern void  textstate(R1(struct moving_button *b)) ;


extern void displaystairs(R1(struct menu *m)) ;
extern void displaynextpiece(R1(struct bl *bl)) ;
extern void displayzoo(R3(struct bl *bl,int doclear,int dodraw)) ;
extern void displaystat(R2(struct bl *bl,int i)) ;

