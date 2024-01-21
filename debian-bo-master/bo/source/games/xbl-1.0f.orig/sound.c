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
#include <X11/Xlib.h>

static XKeyboardState xkbs ;	/* To save the bell status */

void setbell( d , p , dur )
Display *d ;
int p,dur ;
{
XKeyboardControl xkbc ;

xkbc.bell_pitch = p ;
xkbc.bell_duration = dur ;
XChangeKeyboardControl( d , KBBellPitch|KBBellDuration , &xkbc ) ;
}

void savebell(d) Display *d ; { XGetKeyboardControl( d , &xkbs ) ; }

void restorebell(d) Display *d ; 
{
setbell( d, (int)xkbs.bell_pitch ,  (int)xkbs.bell_duration ) ;
}


void sound( d , p , dur ,vol )
Display *d ;
int p,dur ;
int vol ;
{
setbell( d , p , dur ) ;
XBell(d,vol) ;
}

/* vol = 0...10 */
void playsound(d,type,vol)
Display *d ;
int type ;
int vol ;
{
int i,j ;

if ( vol==0 ) return ;

vol = (vol-5)*20 ;

switch(type)
	{
	case 1 :
		sound(d,1000,10,vol) ;
		break ;
	case 2 :
		for(j=100;j<20000;j=j*1.1)
			sound(d,j,5,vol) ;
		break ;
	case 3 :
		for(j=20000;j>1000;j*=.9)
			sound(d,j,5,vol) ;
		for(j=1000;j<20000;j*=1.1)
			sound(d,j,5,vol) ;
		break ;
	case 4 :
		if ( vol==-100 ) return ;
		for(j=10000;j>1000;j-=1000)
			{
			for(i=1000;i<j;i*=1.5)
				sound(d,i,5,vol) ;
			for(i=j;i>1000;i*=.7)
				sound(d,i,5,vol) ;
			}
		break ;
	}
restorebell(d) ;
}



