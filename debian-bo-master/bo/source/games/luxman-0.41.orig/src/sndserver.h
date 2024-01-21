/*
   sndserver.h

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

#ifndef _sndserver_h_
#define _sndserver_h_

/*
 *
 * The soundserver is started with the command line:
 *
 *     snd-server "fd_cmd fd_ret"
 *
 * Where `fd_cmd' and `fd_ret' are file descriptors of
 * open pipes passed from the calling process.
 *
 * These are the server commands (written to `fd_cmd'):
 * (Results are read from `fd_ret')
 *
 * Commands:
 *
 * 	Init server
 * 	Close server
 *
 * 	Open sound
 * 	Close sound
 *
 * 	Load sample
 * 	Unload sample
 *
 * 	Play sample on chan
 * 	Start looping on chan
 *
 * 	Sleep
 * 	Wakeup
 *
 * 	Query channel complete
 * 	Sync
 *
 * 	Clear one channel
 * 	Clear all channels
 *
 * Commands are written as:
 * 	command			(int)
 * 				One of the SCMD_.. constants
 * 	data			(variable)
 * 				Command-dependent data, as given
 * 				in the next section.
 * -------------------------------------------------------------------------
 *
 * Init server:
 * 	num_ids		(int)
 * 			Number of id's to reserve for samples
 * 	num_chans	(int)
 * 			Number of channels to allocate.
 * 	rate		(int)
 * 			Playback rate
 *
 * 	Returns:	(int)
 * 			0, on success
 * 			-1, out of memory
 * 			-2, no access to sound device
 * 			-3, can't open sound device
 * 			-4, ioctl error talking to sound device
 *
 * Close server:
 * 	(no params)
 *
 * 	Returns:	Nothing
 *
 * Load sample:
 * 	id		(int)
 * 			Desired ID - [0..(num_ids-1)]
 * 	namelen		(int)
 * 			Length of following name
 * 	name		(namelen chars -- NOT null terminated)
 * 			Name of sample file to load
 * 	divide		(int)
 * 			Value to divide samples by
 *      prefade         (float)
 *                      Use `prefade' % of sample to fade-in.
 *                      Should be in the range 0..100. 
 *                      (i.e. 0 mean no fadein, 100 means fadein
 *                      for entire sample)
 *      postfade        (float)
 *                      Use `postfade' % of sample to fade-out.
 *                      Should be in the range 0..100
 *
 * 	Returns:	(int)
 * 			0, on success
 * 			-1, bad id
 * 			-2, file not found
 * 			-3, out of memory
 *                      -4, pre or postfade arg out of range
 *
 *  Note:  We get around having to make a `clipping' pass during
 *         mixing by being clever and dividing down the samples
 *         when we load them. If you do this wrong (ie, put
 *         3 samples on a channel, but only divide each by 2)
 *         the output will sound nasty.
 *
 * Note 2: The fading is done such that if the pre- and postfade
 *         overlap, the overlapped section will be faded twice.
 *        
 * Unload sample:
 * 	id		(int)
 *
 * 	Returns:	(int)
 * 			 0, on success
 * 			-1, on failure (bad id)
 *
 * 	Note:		Caller must ensure that sample is not
 * 			currently being played or weirdness
 * 			will occur.
 *
 * Play sample on channel:
 * 	id		(int)
 * 			Sample ID
 * 	chan		(int)
 * 			Channel to use
 * 	override	(int)
 * 			If channel busy:
 * 			1 - Clear and play new sample
 * 			0 - Do nothing
 *
 * 	Returns:	(int)
 * 			0, on success (sample started)
 * 			-1, id out of range
 * 			-2, bad channel number
 *
 * Start looping sample on channel:
 * 	id		(int)
 * 			Sample ID
 * 	chan		(int)
 * 			Channel to use
 *
 * 	Returns:	(int)
 * 			0, on success (looping started)
 * 	                <0, on failure (bad id or chan)
 *
 * Sleep:
 * 	(no params)
 *
 * 	Returns:	Nothing
 * 	Note:		Pauses sound playback until `Wakeup'
 *
 * Wakeup:
 * 	(no params)
 *
 * 	Returns:	(int)
 * 			0 when process has awakened
 *
 * Query channel complete:
 * 	chan		(int)
 * 			Channel number to query
 *
 * 	Returns:	(int)
 * 	 		 1 - Channel complete or empty
 *  			 0 - Channel not complete (or looping)
 * 			-1 - Bad channel number
 *
 * Sync:
 * 	(no params)
 *
 * 	Returns:	(int)
 * 			0, when all sounds have stopped
 * 			-1, sound not initalized
 *
 * 	Note:		The idea here is to use `query channel complete'
 * 			to wait for all data to be sent to the
 * 			sound driver, then do a `sync' to wait for
 * 			the last of the data to play.
 * 			You will get weird results if you do not
 * 			use `sync' this way.
 *
 * Clear one channel:
 * 	chan		(int)
 * 			Channel to clear
 * 	
 * 	Returns:	 0, on success (channel cleared)
 * 			-1, on failure (bad chan #)
 *
 * Clear all channels:
 * 	block		(int)
 * 			If 1, process waits for sound to actually
 * 			stop.
 * 			If 0, returns immediately.
 *
 * 	Returns:	(int)
 * 			 0, when complete (`complete' as defined by `block')
 * 			-1, sound not initialized
 */

#define SCMD_INIT_SERVER	1
#define SCMD_CLOSE_SERVER	2

#define SCMD_OPEN_SOUND		3
#define SCMD_CLOSE_SOUND	4

#define SCMD_LOAD_SAMPLE	5
#define SCMD_UNLOAD_SAMPLE	6

#define SCMD_PLAY_SAMPLE	7
#define SCMD_START_LOOP		8

#define SCMD_QUERY_COMPLETE	9
#define SCMD_SYNC			10

#define SCMD_SLEEP			11
#define SCMD_WAKEUP			12

#define SCMD_CLEAR_ONE		13
#define SCMD_CLEAR_ALL	    14

#endif
