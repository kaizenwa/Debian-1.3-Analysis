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
#define OPTIMUM_DISPLAY_TIME 0.040
#define BETWEEN 20 /* Number of images for a transformation of moving bloc */
#define MAXFRAMESEC 100

struct realtime
	{
	time_t starttime ;	/* Start time of real timer */
	time_t lastcurrenttime ; /* Last time of real timer */
	int imagenumber ;		/* Current image number */
	int lastimagenumber ;		/* Image number of last second change */
	float displaytime ;		/* Approx time to display a frame */
	float lrtime ;			/* Real time of last display frame */
	float lastfall ;		/* Real Time of the last fall */
	float falltime ; 		/* Time to shut one stage */
	int nocalcdisplaytime ;
	int framesec[MAXFRAMESEC] ;
	int microsleepdelay ;	/* Minimum micro sleep time */
	} ;

extern void inittime(R2(struct realtime *r,int waitone)) ;
extern void nextframe(R2(struct bl *bl,struct realtime *r)) ;
extern void microsleep(R1(int i)) ;

