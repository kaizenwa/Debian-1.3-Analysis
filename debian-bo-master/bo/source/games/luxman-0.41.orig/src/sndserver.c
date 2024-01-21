/*
   sndserver.c

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

#include <sys/time.h>
#include <stdarg.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <signal.h>
#include <string.h>
#include <stdlib.h>
#include <sys/ioctl.h>
#include <linux/soundcard.h>
#include "sndserver.h"

/*#define DEBUG*/

typedef unsigned char uchar;

/* Stores one sample */
typedef struct {
  uchar *buf;
  int len;
} Sample;

typedef struct {
  uchar *buf;		/* NULL if no sample on channel */
  int len;
  
  uchar *cur;		/* Current pointer into `buf' */
  int num_left;		/* Num bytes from `cur' to end of `buf' */
  
  int repeat;		/* 1 if this is a looping channel, 0 if one-shot */
} Channel;

Sample *sample=NULL;			/* Sample array */
int num_samples=0;				/* Max, not number in use */
Channel *channel=NULL;			/* Channel array */
int num_chans=0;				/* Max, not number in use */

uchar *mix_buf=NULL;			/* Buffer to mix into */
int mix_len=0;

int initted = 0;
int paused = 0;

/* Name of sound device */
char *snddev = "/dev/dsp";

/* FD to read commands from (pipe) */
int fd_command = -1;

/* FD to write results to (pipe) */
int fd_return = -1;

/* Where to write samples */
int fd_sound = -1;

int dprintf( char *fmt, ... )
{
#ifdef DEBUG
  va_list ap;

  va_start( ap, fmt );
  return vprintf( fmt, ap );
#else
  return 0;
#endif
}

int mix_first( int n )
{
  int i;
  
  if ( channel[n].num_left < mix_len )
	{
	  if ( channel[n].repeat )
		{
		  i = mix_len - channel[n].num_left;
		  memcpy( mix_buf, channel[n].cur, channel[n].num_left );
		  memcpy( mix_buf + channel[n].num_left, channel[n].buf, i );
		  channel[n].cur = channel[n].buf + i;
		  channel[n].num_left = channel[n].len - i;
		}
	  else
		{
		  i = mix_len - channel[n].num_left;
		  memcpy( mix_buf, channel[n].cur, channel[n].num_left );
		  memset( mix_buf + channel[n].num_left, 0, i );
		  channel[n].num_left = 0;
		  channel[n].buf = NULL;
		}
	}
  else
	{
	  memcpy( mix_buf, channel[n].cur, mix_len );
	  channel[n].cur += mix_len;
	  channel[n].num_left -= mix_len;
	}

  return 0;
}

/* Mix channel N into mix_buf */
int mix_n( int n )
{
  uchar *p;
  int i;
  
  p = mix_buf;
  i = mix_len;

  while( i-- )
	{
	  if ( !channel[n].num_left )
		{
		  if ( channel[n].repeat )
			{
			  channel[n].cur = channel[n].buf;
			  channel[n].num_left = channel[n].len;
			}
		  else
			return 0;
		}

	  *(p++) += *(channel[n].cur++);
	  --(channel[n].num_left);
	}

  return 0;
}

/* Mix and output next block of sound -- may block */
int output_next_blk()
{
  int i;
  int f;
  
  if ( !initted || (fd_sound < 0) )
	return -1;
  
  /* Mix first channel */
  f = 0;

  i = 0;
  while( i < num_chans )
	{
	  if ( channel[i].buf &&
		  (channel[i].num_left || channel[i].repeat) )
		{
		  mix_first( i );
		  f = 1;
		  ++i;
		  break;
		}
	  ++i;
	}
  
  /* Mix rest */
  for( ; i<num_chans; ++i )
	{
	  if ( channel[i].buf && (channel[i].num_left || channel[i].repeat) )
		mix_n( i );
	}

  if ( !f )
	memset( mix_buf, 0, mix_len );
  
  /* Write mix to sound device */
  write( fd_sound, mix_buf, mix_len );

  return 0;
}

void close_server();

/* This does memory initialization so that samples can be loaded */
int init_server( int n_ids, int n_chans )
{
  int i;

  if ( initted )
	close_server();
  
  /* Alloc Sample array */
  num_samples = n_ids;

  sample = (Sample*)malloc( n_ids * sizeof(Sample) );

  if ( !sample )
	return -1;

  for( i=0; i<n_ids; ++i )
	{
	  sample[i].buf = NULL;
	  sample[i].len = 0;
	}

  num_chans = n_chans;

  channel = (Channel*)malloc( n_chans * sizeof( Channel ) );

  if ( !channel )
	return -1;

  for( i=0; i<n_chans; ++i )
	{
	  channel[i].buf = channel[i].cur = NULL;
	  channel[i].len = channel[i].num_left = 0;
	  channel[i].repeat = 0;
	}

  initted = 1;

  dprintf( "Sound server initted\n");
  dprintf( "ids: %d; chans: %d\n", num_samples, num_chans );
  
  return 0;
}

/* This actually opens the sound device */
int init_sound( int rate )
{
  int frag = 0x00020007;

  if ( fd_sound >= 0 )
	{
	  close( fd_sound );
	  fd_sound = -1;
	}
	  
  if ( access( snddev, W_OK ) != 0 )
	return -1;

  fd_sound = open( snddev, O_WRONLY );

  if ( fd_sound < 0 )
	return -2;

  /* Set fragment size */
#ifndef OLD_KERNEL  
  if ( ioctl( fd_sound, SNDCTL_DSP_SETFRAGMENT, &frag ) == -1 )
	{
	  close( fd_sound );
	  fd_sound = -1;
	  return -3;
	}
#endif
  
  /* Set playback speed */
  if ( ioctl( fd_sound, SNDCTL_DSP_SPEED, &rate ) == -1 )
	{
	  close( fd_sound );
	  fd_sound = -1;
	  return -3;
	}

  /* Get fragment size */
#ifndef OLD_KERNEL  
  if ( ioctl( fd_sound, SNDCTL_DSP_GETBLKSIZE, &mix_len ) == -1 )
	{
	  close( fd_sound );
	  fd_sound = -1;
	  return -3;
	}
#else
  mix_len = 128;
#endif
  
  /* Setup mixer buffer */
  mix_buf = (uchar*)malloc( mix_len * sizeof( uchar ) );

  if ( !mix_buf )
	return -1;

  dprintf( "mixlen: %d\n", mix_len );
  
  return 0;
}

/* Will also close sound device if still open */
void close_server()
{
  int i;

  if ( !initted )
	return;
  
  initted = 0;
  
  for( i=0; i<num_samples; ++i )
	{
	  if ( sample[i].buf )
		free( sample[i].buf );
	}
  
  free( sample );
  free( channel );
  free( mix_buf );

  if ( fd_sound >= 0 )
	{
	  close( fd_sound );
	  fd_sound = -1;
	}
  
  dprintf( "Sound server closed.\n" );
}

/* This closes the sound device */
void close_sound()
{
  if ( fd_sound >= 0 )
	{
	  close( fd_sound );
	  fd_sound = -1;
	}
}

int load_sample( int id, char *filename, int divide, float prefade,
		float postfade )
{
  FILE *fp;
  uchar *p;
  int i, n;
  float f;

  if ( !initted || id >= num_samples )
	return -1;

  dprintf("Sndserv: Loading `%s' as id %d; div: %d.\n", filename,
		  id, divide );
  
  fp = fopen( filename, "r" );
  if ( !fp )
	return -2;
  
  fseek( fp, 0, SEEK_END );

  sample[id].len = ftell( fp );

  dprintf( "Length: %d\n", sample[id].len );
  
  fseek( fp, 0, SEEK_SET );

  if ( sample[id].buf )
	free( sample[id].buf );

  sample[id].buf = (uchar*)malloc( sample[id].len * sizeof( uchar ) );
  if ( !sample[id].buf )
	return -3;

  fread( sample[id].buf, 1, sample[id].len, fp );

  fclose( fp );

  /* Do fadein */
  if ( prefade < 0 || prefade > 100 )
    return -4;

  /* Calc fade length */
  n = sample[id].len * (prefade / 100.0);
  p = sample[id].buf;

  for( i=0; i<n; ++i )
    {
      f = (float)( *p - (uchar)128 ) * ((float)i)/((float)n);
      f = f + 128.0;
      *p = (uchar)f;
      ++p;
    }

  /* Do fadeout */
  if ( postfade < 0 || postfade > 100 )
    return -4;

  /* Calc fade length */
  n = sample[id].len * (postfade / 100.0);
  p = sample[id].buf + sample[id].len - 1;

  for( i=0; i<n; ++i )
    {
      f = (float)( *p - (uchar)128 ) * ((float)i)/((float)n);
      f = f + 128.0;
      *p = (uchar)f;
      --p;
    }

  /* Do predivide */
  p = sample[id].buf;
  i = sample[id].len;

  while( i-- )
	{
	  *p /= (uchar)(divide);
	  ++p;
	}
  
  return 0;
}
  
int unload_sample( int id )
{
  if ( !initted || id >= num_samples )
	return -1;

  dprintf("snd: Unloading %d\n", id );
  
  if ( sample[id].buf )
	{
	  free( sample[id].buf );
	  sample[id].buf = NULL;
	  sample[id].len = 0;
	}

  return 0;
}

int assign_to_channel( int id, int chan, int override )
{
  if ( !initted || id >= num_samples )
	return -1;

  if ( chan >= num_chans )
	return -2;

  dprintf("snd: Assigning %d to chan %d; over = %d\n", id, chan, override );
  
  /* Watch override */
  if ( override == 0 && (channel[chan].buf &&
						 (channel[chan].num_left > 0 ||
						  channel[chan].repeat) ) )
	return 0;

  /* Else, assign */
  channel[chan].buf = channel[chan].cur = sample[id].buf;
  channel[chan].len = channel[chan].num_left = sample[id].len;
  channel[chan].repeat = 0;
  
  return 0;
}

int assign_loop_to_channel( int id, int chan )
{
  int r;

  dprintf( "snd: assigning loop of %d on chan %d\n", id, chan );
  
  r = assign_to_channel( id, chan, 1 );

  if ( r != 0 )
	return r;

  channel[chan].repeat = 1;
  return 0;
}

int query_complete( int chan )
{
  if ( !initted || chan >= num_chans || fd_sound < 0 )
	return 1;	/* So that we can run with sound disabled */

  dprintf( "snd: querying %d\n", chan );
  
  if ( channel[chan].repeat )
	return 0;

  if ( !channel[chan].buf || channel[chan].num_left == 0 )
	return 1;
  else
	return 0;
}

int sound_sync( void )
{
  if ( !initted || fd_sound < 0 )
	return 0;		/* So that we can run without sound */

  dprintf( "snd: syncing\n" );
  
  ioctl( fd_sound, SNDCTL_DSP_SYNC );

  return 0;
}

int clear_channel( int chan )
{
  if ( !initted || chan >= num_chans )
	return -1;

  dprintf( "snd: clearing %d\n", chan );
  
  channel[chan].buf = channel[chan].cur = NULL;
  channel[chan].len = channel[chan].num_left = 0;
  channel[chan].repeat = 0;
  
  return 0;
}

int clear_all_channels( int do_block )
{
  int i;

  if ( !initted )
	return -1;

  dprintf( "snd: clearing all; block=%d\n", do_block );
  
  for( i=0; i<num_chans; ++i )
	clear_channel( i );

  if ( do_block && fd_sound >= 0 )
	ioctl( fd_sound, SNDCTL_DSP_SYNC );

  return 0;
}

void process_cmd()
{
  int cmd;
  int ret;
  int n_ids, n_chans, rate;
  int id, len, divide;
  char *name;
  int c, chan, override, block;
  float prefade, postfade;

  /* Read command */
  read( fd_command, &cmd, sizeof( int ) );

  switch( cmd )
	{
	case SCMD_INIT_SERVER:
	  read( fd_command, &n_ids, sizeof( int ) );
	  read( fd_command, &n_chans, sizeof( int ) );

	  ret = init_server( n_ids, n_chans );
	  break;

	case SCMD_CLOSE_SERVER:
	  close_server();
	  return;

	case SCMD_OPEN_SOUND:
	  read( fd_command, &rate, sizeof( int ) );

	  ret = init_sound( rate );
	  break;

	case SCMD_CLOSE_SOUND:
	  close_sound();
	  return;
	  
	case SCMD_LOAD_SAMPLE:
	  read( fd_command, &id, sizeof( int ) );
	  read( fd_command, &len, sizeof( int ) );

	  name = (char*)alloca( (len+1) );
	  if ( !name )
		{
		  /* Skip rest of command */
		  while( len-- )
			read( fd_command, &c, 1 );
		  read( fd_command, &divide, sizeof( int ) );
		  
		  ret = -3;
		  break;
		}

	  read( fd_command, name, len );
	  name[len] = 0;
	  read( fd_command, &divide, sizeof( int ) );
	  read( fd_command, &prefade, sizeof( float ) );
	  read( fd_command, &postfade, sizeof( float ) );

	  ret = load_sample( id, name, divide, prefade, postfade );
	  break;

	case SCMD_UNLOAD_SAMPLE:
	  read( fd_command, &id, sizeof( int ) );

	  ret = unload_sample( id );
	  break;

	case SCMD_PLAY_SAMPLE:
	  read( fd_command, &id, sizeof( int ) );
	  read( fd_command, &chan, sizeof( int ) );
	  read( fd_command, &override, sizeof( int ) );

	  ret = assign_to_channel( id, chan, override );
	  break;

	case SCMD_START_LOOP:
	  read( fd_command, &id, sizeof( int ) );
	  read( fd_command, &chan, sizeof( int ) );

	  ret = assign_loop_to_channel( id, chan );
	  break;

	case SCMD_SLEEP:
	  paused = 1;
	  return;

	case SCMD_WAKEUP:
	  paused = 0;
	  ret = 0;
	  break;
	  
	case SCMD_QUERY_COMPLETE:
	  read( fd_command, &chan, sizeof( int ) );

	  ret = query_complete( chan );
	  break;

	case SCMD_SYNC:
	  ret = sound_sync();
	  break;

	case SCMD_CLEAR_ONE:
	  read( fd_command, &chan, sizeof( int ) );

	  ret = clear_channel( chan );
	  break;

	case SCMD_CLEAR_ALL:
	  read( fd_command, &block, sizeof( int ) );
	  
	  ret = clear_all_channels( block );
	  break;
	}

  write( fd_return, &ret, sizeof( int ) );
}

void sigkill( int s )
{
  printf("Sound server killed\n");
  exit(0);
}

int got_pipe;

void sigpipe( int s )
{
  got_pipe = 1;
}

void usage()
{
  fputs("Usage: luxman-snd \"fd1 fd2\"\n"
		"       However you shouldn't start this program by hand anyway.\n",
			stderr);
  exit(1);
}

int main( int argc, char *argv[] )
{
  fd_set readfs;
  struct timeval tv;

  signal( SIGTERM, sigkill );
  signal( SIGPIPE, sigpipe );
  got_pipe = 0;

  /* get pipe fd's from command line */
  if ( argc < 2 )
	usage();
  
  sscanf( argv[1], "%d %d", &fd_command, &fd_return );
  
  /* Note: we rely on the static init of fd_* ! */
  
  if ( (fd_command < 0) || (fd_return < 0) )
	usage();
 
  printf("Sound server started [pid:%d]\n", (int)getpid() );
  
  FD_ZERO( &readfs );
  
  while( !got_pipe )
	{
	  /* See if any commands have come in */
	  FD_SET( fd_command, &readfs );
	  
	  if ( (fd_sound < 0) || paused )
		{
		  /* Sleep during select so that don't eat the cpu */
		  tv.tv_sec = 10;
		  tv.tv_usec = 0;
		}
	  else
		{
		  tv.tv_sec = 0;
		  tv.tv_usec = 0;
		}
	  
	  select( fd_command + 1, &readfs, NULL, NULL, &tv );

	  if ( FD_ISSET( fd_command, &readfs ) )
		process_cmd();		/* Yes, process command */

	  /* Output next chunk (may block) */
	  if ( initted && !paused )
		output_next_blk();
	}

  close_server();

  printf( "Sound server process exiting.\n");
  
  return 0;
}
