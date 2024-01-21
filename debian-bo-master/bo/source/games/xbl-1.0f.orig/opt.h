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

struct opt
	{
	int verbose ;		/* Verbose mode 0=off */
	int newmap ;		/* Use a new colormap */
	int use_bw ;		/* Work in black and white */
	int buffering ;		/* Buffering mode */
	int statis ;		/* Display statistique at end game */
	int drawmode ; 		/* wire , solid , transparent */
	int wx,wy,wz ; 		/* World size */
	int smooth ;		/* Smooth drawing (hard if false) */
	int level ;		/* Start level of game */
	int mode ;		/* PLAY , TRAINING */
	int land ; 		/* USER,SMALL,BIG */
	int volume ;		/* Sound volume 0...10 */
	int speedtest ;		/* If set, test drawing speed */
	int clearline ;		/* 0 Useclear rectangle 1:use clear line */
	int linewidth ;		/* 0 fast, else : thickness */
	char thefont[256] ;	/* Font for texts */
	char thefont2[256] ;	/* Font for big texts */
	int button_height ;	/* Height of relief button */
	int keyboard ;		/* Key board type */
	char userkey[256] ;	/* User key map */
	char displayname[256] ;	/* Name of the display */
	char geometry[256] ;	/* Geometry of the game window */
	char menugeometry[256] ;/* Geometry of the menu window */
	char scoregeometry[256];/* Geometry of the score window */
	char zoogeometry[256];  /* Geometry of the zoo window */
        int state ;             /* SHOW,STOP,SUSPEND,RUN,DEMO */
	int backcolor ;		/* background color for menu */
	int presskey ;		/* Supress the message presskey */
	int visual ;		/* If true, search a best visual */
	int time_to_demo ;	/* Number of seconds between demo */
	int Time_to_demo ;	/* current Number of seconds before demo */
	} ;
