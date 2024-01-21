/*
   sndclient.h

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

/*
 *=================================================================
 *  See `sndserver.h' for documentation on what each call does.
 *=================================================================
 */

#ifndef _sndclient_h_
#define _sndclient_h_

#ifdef __cplusplus
extern "C" {
#endif
  
#include "sndserver.h"

/* Writes commands to server via fd_cmd. Reads results from fd_ret. */
int snd_open_server( int n_ids, int n_chans, int fd_cmd, int fd_ret );
void snd_close_server( void );

int snd_open_sound( int rate );
void snd_close_sound( void );

int snd_load_sample( int id, char *name, int divide, float prefade,
		    float postfade );
int snd_unload_sample( int id );

int snd_play_sample( int id, int chan, int override );
int snd_start_loop( int id, int chan );

void snd_sleep( void );
void snd_wakeup( void );

int snd_query_complete( int chan );
int snd_sync( void );

int snd_clear_chan( int chan );
int snd_clear_all( int do_block );

#ifdef __cplusplus
}
#endif

#endif


