/*
 * Programm XBLAST V2.2 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * December 9th 1996
 * started August 1993
 *
 * File: sound.c 
 * sound client for XBlast
 *
 * Author: Norbert Nicolay, e-mail: nicolay@ikp.uni-koeln.de
 *         July 30th 1996
 *
 * $Id: sound.c,v 1.10 1996/11/19 21:50:55 norbert Exp norbert $
 *
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public Licences as published
 * by the Free Software Foundation; either version 2; or (at your option)
 * any later version
 *
 * This program is distributed in the hope that it will entertaining,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of 
 * MERCHANTABILTY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
 * Publis License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.
 * 675 Mass Ave, Cambridge, MA 02139, USA.
 * 
 * $Log: sound.c,v $
 * Revision 1.10  1996/11/19 21:50:55  norbert
 * Simple protocol to control proper sound server start established.
 *
 * Revision 1.9  1996/09/09 21:39:07  norbert
 * Code cleaned up. Enumeration implemented.
 *
 * Revision 1.8  1996/09/06 22:18:38  norbert
 * New function unload_sound to free sample memory. The argument of
 * the function stop_sound now is evaluated.
 *
 * Revision 1.7  1996/08/13 21:06:48  norbert
 * Now server is execed as a seperate process. Thus it is possible to
 * write servers for other machines.
 *
 * Revision 1.6  1996/08/13 20:46:25  norbert
 * New sliding bomb sound introduced. Shrinking wall sounds still not
 * implemented.
 * -
 *
 * Revision 1.5  1996/08/12 21:34:21  norbert
 * Some RCS cosmetics.
 *
 * Revision 1.4  1996/08/12 21:29:43  norbert
 * This version now supports mono samples which can be positioned in the
 * stereo panorama with an additional argument to the client's play_sound()
 * function. Stereo channel flipping now fixed with dsp_syncs where possible.
 *
 * Revision 1.3  1996/08/02 13:36:33  norbert
 * Incorrect termination of sound server on ioctl errors fixed. Now some
 * ioctl errors are simply ignored, on other ones the server now terminates
 * correctly without disturbing the parent.
 *
 * Revision 1.2  1996/08/01 19:14:22  norbert
 * Statistics implemented, repeating of songs ready. TODO: shrinking still
 * incomplete.
 *
 * Revision 1.1  1996/08/01 08:32:21  norbert
 * Initial revision
 *
 *
 */

#if defined(XBLAST_SOUND)

#define _SOUND_C_

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/fcntl.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <linux/soundcard.h>
#include <signal.h>
#include "sound.h"

#if !defined(TRUE)
#define TRUE 1
#endif

#if !defined(FALSE)
#define FALSE 0
#endif

static int sound_initialized = FALSE;

static int cmdpipe[2];  /* pipe i/o descriptors */
static int ackpipe[2];  
static int server_pid = -1;

static char *mono_args[] = {
  XBLAST_SOUND_SERVER,
  "-mono",
  NULL,
};

static char *stereo_args[] = {
  XBLAST_SOUND_SERVER,
  NULL,
};

#if defined(__STDC__)
int
init_sound(char *mode)
#else
int
init_sound(mode)
  char *mode;
#endif
{
  char **server_args;

  if (NULL == mode) {
    return 1;
  } else if (0 == strcmp(mode,"stereo")) {
    server_args = stereo_args;
  } else if (0 == strcmp(mode,"mono")) {
    server_args = mono_args;
  } else {
    return 1;
  }

  if (pipe(cmdpipe) < 0)
    {
      fprintf(stderr, "could not create cmd pipe for sound communication\n");
      return(-1);
    }
  if (pipe(ackpipe) < 0)
    {
      fprintf(stderr, "could not create ack pipe for sound communication\n");
      close(cmdpipe[0]);
      close(cmdpipe[1]);
      return(-1);
    }
  if ((server_pid = fork()) == 0)
    {
      /* child (sound server) */
      long farg = O_NONBLOCK;

      /* set read pipes to nonblocking mode */
      (void) fcntl(cmdpipe[0], F_SETFL, farg);
      close(cmdpipe[1]);
      close(0);
      (void) dup(cmdpipe[0]);
      close(ackpipe[0]);
      close(1);
      (void) dup(ackpipe[1]);

      if (execvp(server_args[0], server_args) < 0)
	{
	  int ack_val = SND_ACK_ERROR;
	  fprintf(stderr, "Could not exec sound server\n"); 
	  fflush(stderr);
	  write(1, &ack_val, sizeof(ack_val)); 
	}
      exit(0);
    }
  else if (server_pid > 0)
    {
      int ack_val;
      /* parent (client) */
      long farg = O_NONBLOCK;
      
      /* set write pipe to nonblocking mode */
      (void) fcntl(cmdpipe[1], F_SETFL, farg);
      close(cmdpipe[0]);
      close(ackpipe[1]);
      
      read(ackpipe[0], &ack_val, sizeof(ack_val));
      if (ack_val == SND_ACK_OK)
	{
	  sound_initialized = TRUE;
	  return(0);
	}
      else
	{
	  sound_initialized = FALSE;
	  return(1);
	}
    }
  else
    {
      fprintf(stderr, "could not fork sound server\n");
      close(cmdpipe[0]);
      close(cmdpipe[1]);
      sound_initialized = FALSE;
      return(-1);
    }
}

#if defined(__STDC__)
int
stop_sound(int id)
#else
int
stop_sound(id)
int id;
#endif
{
  if (sound_initialized == TRUE)
    {
      int cmd[2];
      cmd[0] = SND_STOP_SOUND;
      cmd[1] = id;
      write(cmdpipe[1], cmd, sizeof(cmd));
      read(ackpipe[0], cmd, 1);
    }
  return(0);
}

#if defined(__STDC__)
int
play_sound(int id, int position)
#else
int
play_sound(id, position)
int id;
int position;
#endif
{
  if (sound_initialized == TRUE)
    {
      /* Note: position argument is ignored for stereo sounds */
      int cmd[2];
      cmd[0] = SND_PLAY_SOUND;
      cmd[1] = id | (position << 16);
      write(cmdpipe[1], cmd, sizeof(cmd));
    }
  return(0);
}

#if defined(__STDC__)
int
load_sound(int id)
#else
int
load_sound(id)
int id;
#endif
{
  if (sound_initialized == TRUE)
    {
      int cmd[2];
      cmd[0] = SND_LOAD_SOUND;
      cmd[1] = id;
      write(cmdpipe[1], cmd, sizeof(cmd));
      read(ackpipe[0], cmd, 1);
    }
  return(0);
}

#if defined(__STDC__)
int
unload_sound(int id)
#else
int
unload_sound(id)
int id;
#endif
{
  if (sound_initialized == TRUE)
    {
      int cmd[2];
      cmd[0] = SND_UNLOAD_SOUND;
      cmd[1] = id;
      write(cmdpipe[1], cmd, sizeof(cmd));
      read(ackpipe[0], cmd, 1);
    }
  return(0);
}

#if defined(__STDC__)
void
flush_audio(void)
#else
void
flush_audio()
#endif
{
}

#if defined(__STDC__)
int
stop_sound_server(void)
#else
int
stop_sound_server()
#endif
{
  int wait_status;

  if (server_pid != -1)
    {
      kill(server_pid, SIGINT);
      wait(&wait_status);
    }
  return(0);
}

#endif
