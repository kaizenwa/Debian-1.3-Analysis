/*
   error.cc

   This file is part of LuxMan.
   
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
#include <stdarg.h>
#include <signal.h>
#include <vga.h>
#include <rawkey/rawkey.h>
#include <stdlib.h>
#include <stdio.h>
#include "error.h"
#include "globals.h"
#include <gtools/gr.h>
#include "sndclient.h"

void do_fatal_error( char *file, int line, char *fmt, ... )
{
  va_list ap;

  if ( gb_pid_sndserver >= 0 )
	{
	  snd_close_server();
	  kill( gb_pid_sndserver, SIGTERM );
	}
  
  rawmode_exit();
  gr_close();

  va_start( ap, fmt );

  printf("*** Fatal error in %s, line %d ***\n", file, line );
  
  vprintf( fmt, ap );

  printf("\n");
  
  exit(1);
}


