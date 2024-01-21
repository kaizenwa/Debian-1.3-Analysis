/*
   sndclient.c

   This file is part of LuxMan.
   
   Copyright (C) 1994,1995 Frank McIngvale (frankm@nuance.com)

   Thanks to Michael Weller (eowmob@exp-math.uni-essen.de) for
   improvements.
   
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

#include <fcntl.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
#include "sndclient.h"
#include "globals.h"

static int fd_cmd = -1;
static int fd_ret = -1;

int snd_open_server( int n_ids, int n_chans, int cmd_pipe, int ret_pipe )
{
  int cmd;
  int ret;

  fd_cmd = cmd_pipe;
  fd_ret = ret_pipe;
  
  if ( gb_dont_use_sound == ARGV_TRUE )
	return 0;
  
  cmd = SCMD_INIT_SERVER;
  write( fd_cmd, &cmd, sizeof( int ) );

  write( fd_cmd, &n_ids, sizeof( int ) );
  write( fd_cmd, &n_chans, sizeof( int ) );

  read( fd_ret, &ret, sizeof( int ) );

  return ret;
}

int snd_open_sound( int rate )
{
  int cmd = SCMD_OPEN_SOUND;
  int ret;

  if ( gb_dont_use_sound == ARGV_TRUE )
	return 0;
  
  write( fd_cmd, &cmd, sizeof( int ) );
  write( fd_cmd, &rate, sizeof( int ) );

  read( fd_ret, &ret, sizeof( int ) );
  return ret;
}

void snd_close_server()
{
  int cmd = SCMD_CLOSE_SERVER;
  
  write( fd_cmd, &cmd, sizeof( int ) );
}

void snd_close_sound()
{
  int cmd = SCMD_CLOSE_SOUND;
  
  write( fd_cmd, &cmd, sizeof( int ) );
}

int snd_load_sample( int id, char *name, int divide, float prefade,
		    float postfade )
{
  int cmd = SCMD_LOAD_SAMPLE;
  int len, ret;

  len = strlen( name );
  
  write( fd_cmd, &cmd, sizeof( int ) );
  write( fd_cmd, &id, sizeof( int ) );
  write( fd_cmd, &len, sizeof( int ) );
  write( fd_cmd, name, len );
  write( fd_cmd, &divide, sizeof( int ) );
  write( fd_cmd, &prefade, sizeof( float ) );
  write( fd_cmd, &postfade, sizeof( float ) );

  read( fd_ret, &ret, sizeof( int ) );

  return ret;
}

int snd_unload_sample( int id )
{
  int cmd = SCMD_UNLOAD_SAMPLE;
  int ret;

  write( fd_cmd, &cmd, sizeof( int ) );
  write( fd_cmd, &id, sizeof( int ) );

  read( fd_ret, &ret, sizeof( int ) );

  return ret;
}

int snd_play_sample( int id, int chan, int override )
{
  int cmd = SCMD_PLAY_SAMPLE;
  int ret;

  write( fd_cmd, &cmd, sizeof( int ) );
  write( fd_cmd, &id, sizeof( int ) );
  write( fd_cmd, &chan, sizeof( int ) );
  write( fd_cmd, &override, sizeof( int ) );

  read( fd_ret, &ret, sizeof( int ) );

  return ret;
}

int snd_start_loop( int id, int chan )
{
  int cmd = SCMD_START_LOOP;
  int ret;

  write( fd_cmd, &cmd, sizeof( int ) );
  write( fd_cmd, &id, sizeof( int ) );
  write( fd_cmd, &chan, sizeof( int ) );

  read( fd_ret, &ret, sizeof( int ) );

  return ret;
}

void snd_sleep( void )
{
  int cmd = SCMD_SLEEP;
  
  write( fd_cmd, &cmd, sizeof( int ) );
}

void snd_wakeup( void )
{
  int cmd = SCMD_WAKEUP;
  int ret;
  
  write( fd_cmd, &cmd, sizeof( int ) );

  /* May block */
  read( fd_ret, &ret, sizeof( int ) );
}

int snd_query_complete( int chan )
{
  int cmd = SCMD_QUERY_COMPLETE;
  int ret;

  write( fd_cmd, &cmd, sizeof( int ) );
  write( fd_cmd, &chan, sizeof( int ) );

  read( fd_ret, &ret, sizeof( int ) );

  return ret;
}

int snd_sync( void )
{
  int cmd = SCMD_SYNC;
  int ret;

  write( fd_cmd, &cmd, sizeof( int ) );

  read( fd_ret, &ret, sizeof( int ) );

  return ret;
}

int snd_clear_chan( int chan )
{
  int cmd = SCMD_CLEAR_ONE;
  int ret;

  write( fd_cmd, &cmd, sizeof( int ) );
  write( fd_cmd, &chan, sizeof( int ) );

  read( fd_ret, &ret, sizeof( int ) );

  return ret;
}

int snd_clear_all( int do_block )
{
  int cmd = SCMD_CLEAR_ALL;
  int ret;

  write( fd_cmd, &cmd, sizeof( int ) );
  write( fd_cmd, &do_block, sizeof( int ) );

  read( fd_ret, &ret, sizeof( int ) );

  return ret;
}

  
