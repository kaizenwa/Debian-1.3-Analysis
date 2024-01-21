/*
   vts.h

   This file is part of LuxMan.
   
   Copyright (C) 1995 Frank McIngvale (frankm@nuance.com)
   
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

#ifndef _vts_h_
#define _vts_h_

extern int vt_switched_out;		/* 1 if we are switched out, 0 if not */

/* Use this pair of routines to watch a section of code for a VT
   switch. Probably doesn't work if rawkey enabled. */
void vt_watch_switch();		/* Start watching */
void vt_unwatch_switch();	/* Stop watching */

#endif
