/*
   sound.h

   This file is part of LuxMan.

   Copyright (C) 1994,1995 Bruce Tenison (rbtenis@ibm.net)
   Copyright (C) 1994,1995 Frank McIngvale (frankm@nuance.com)
   
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#ifndef _sound_h_
#define _sound_h_

#include "sndclient.h"

/* Looping sounds */
#define SND_SIREN 		0
#define SND_ENERGIZED	1
#define SND_EYES		2

/* One-shot sounds */
#define SND_BEGIN		3
#define SND_CREDIT		4
#define SND_DOT			5
#define SND_EATGHOST	6
#define SND_FREELIFE	7
#define SND_FRUIT		8
#define SND_LUXDIE		9

#define SND_LAST		9

/* Channels */
#define CHAN1 		0
#define CHAN2 		1	

#define CHAN_LAST	1

int load_sounds( void );

#endif	
