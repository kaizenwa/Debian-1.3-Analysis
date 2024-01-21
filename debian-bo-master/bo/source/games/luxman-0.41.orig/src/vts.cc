/*
   vts.cc

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

#include <sys/ioctl.h>
#include <linux/vt.h>
#include <string.h>
#include <signal.h>
#include <unistd.h>

static int saved_alarm = 0;
int vt_switched_out = 0;

static void vt_away( int s )
{
  ioctl( 0, VT_RELDISP, VT_ACKACQ );
  vt_switched_out = 1;
  saved_alarm = alarm( 0 );
  signal( SIGUSR1, vt_away );
}

static void vt_back( int s )
{
  signal( SIGUSR2, vt_back );
  vt_switched_out = 0;
  alarm( saved_alarm );
}

struct vt_mode vtm_old, vtm;

void vt_watch_switch()
{
  ioctl( 0, VT_GETMODE, &vtm_old );
  memcpy( &vtm, &vtm_old, sizeof( struct vt_mode ) );
  vtm.mode = VT_PROCESS;
  signal( SIGUSR1, vt_away );
  signal( SIGUSR2, vt_back );
  vtm.relsig = SIGUSR1;
  vtm.acqsig = SIGUSR2;
  ioctl( 0, VT_SETMODE, &vtm );
}

void vt_unwatch_switch()
{
  ioctl( 0, VT_SETMODE, &vtm_old );
}
