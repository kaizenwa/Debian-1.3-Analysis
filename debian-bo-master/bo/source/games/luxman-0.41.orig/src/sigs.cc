/*
   sigs.cc

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

#include <signal.h>
#include <stdio.h>
#include <stdarg.h>
#include <unistd.h>
#include <stdlib.h>
#include <rawkey/rawkey.h>
#include <gtools/gr.h>
#include "globals.h"
#include "sndclient.h"

/*
   Signals which cause program termination.
   (SIGUSR1 & 2 also cause termination, but
   they are being caught by rawkey).
*/   
static int term_sigs[] = {
   SIGHUP,  SIGINT, SIGBUS, SIGKILL, SIGPIPE,
   SIGALRM, SIGTERM, SIGSTKFLT };

/* Signals which cause core dumps */
/* Note! These are NOT caught if DEBUG is defined. */
static int core_sigs[] = {
   SIGQUIT, SIGILL, SIGTRAP, SIGABRT, SIGIOT,
   SIGFPE,  SIGSEGV };
  
static char *email = "frankm@nuance.com";

static void do_sig( char *fmt, ... )
{
  va_list list;

  snd_close_server();
  kill( gb_pid_sndserver, SIGTERM );

  va_start( list, fmt );
  
  rawmode_exit();
  gr_close();

  printf("*****************************************************************************\n");
  vprintf( fmt, list );
  printf("*****************************************************************************\n");

  exit(1);
}

static void sig_handler_term( int s )
{
  do_sig( "Oops! Got signal #%d (term type).\n"
		 "Please report this to %s. Thanks.\n", s, email );
}

static void sig_handler_core( int s )
{
  do_sig( "Oops! Got signal #%d (core type).\n"
		 "Please report this to %s. Thanks.\n", s, email );
}

void set_sig_handlers()
{
  int i, n;

  n = sizeof( term_sigs ) / sizeof( term_sigs[0] );

  for( i=0; i<n; ++i )
	signal( term_sigs[i], sig_handler_term );
											  
#ifndef DEBUG
  n = sizeof( core_sigs ) / sizeof( core_sigs[0] );

  for( i=0; i<n; ++i )
	signal( core_sigs[i], sig_handler_core );
#endif  
}

