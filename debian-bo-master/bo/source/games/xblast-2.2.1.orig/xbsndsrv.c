/*
 * Programm XBLAST V2.1.11 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * November 15th 1996
 * started August 1993
 *
 * File: xbsndsrv.c 
 * sound server and sound processing
 *
 * Author: Norbert Nicolay, e-mail: nicolay@ikp.uni-koeln.de
 *         July 30th 1996
 *
 * $Id: xbsndsrv.c,v 1.13 1996/11/21 21:23:20 norbert Exp norbert $
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
 * $Log: xbsndsrv.c,v $
 * Revision 1.13  1996/11/21 21:23:20  norbert
 * Server error messages nicer.
 *
 * Revision 1.12  1996/11/21 21:16:01  norbert
 * Revision info.
 *
 * Revision 1.11  1996/11/21 21:08:52  norbert
 * Mono mode implemented. Fixed a minor problem with the sample rate. The rate
 * is now set after the setting of the number of sound channels.
 *
 * Revision 1.10  1996/11/19 21:50:03  norbert
 * Simple protocol to control proper sound server start established.
 *
 * Revision 1.9  1996/09/09 21:39:07  norbert
 * New method of stereo mixing introduced.
 *
 * Revision 1.8  1996/09/06 22:19:26  norbert
 * New function unload_sound to free sample memory. The argument of
 * the function stop_sound now is evaluated. Reading the command pipe is now
 * optimized for the play_sound command (now processed first).
 *
 * Revision 1.7  1996/09/06 21:41:28  norbert
 * Several new sounds added. Shrinking sounds now working.
 *
 * Revision 1.6  1996/08/13 21:08:07  norbert
 * Now the sound server is execed as a seperate process.
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
#include <string.h>
#include <unistd.h>
#include <sys/fcntl.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <linux/soundcard.h>
#include <signal.h>
#include "sound.h"

#if !defined(TRUE)
#define TRUE 1
#endif

#if !defined(FALSE)
#define FALSE 0
#endif

static const char rcs_id[] = "$Id: xbsndsrv.c,v 1.13 1996/11/21 21:23:20 norbert Exp norbert $";

typedef unsigned char u8;
typedef short s16;

typedef struct _xbsound {
  int              id;
  int              repeat;
  int              mono;
  int              position;
  u8              *samples;
  int              length;
  struct _xbsound *next;
} XBSOUND;

/*
 * sound sample table
 */
static struct _sound_name {
  int          sound_id;     /* the sound's id to refer to it */
  const char  *name;         /* raw samples data file name */
  u8          *samples;      /* pointer to samples memory */
  int          length;       /* length in samples of the sound */
  int          repeat;       /* repeat flag to play sound endlessly */
  int          mono;         /* mono flag indicating mono sounds */
} sound_name[] = {
  {SND_BAD,      "xb_bad.raw", NULL, 0, FALSE, FALSE},  /* got a skull */
  {SND_DROP,     "xb_drop.raw", NULL, 0, FALSE, TRUE},  /* dropped a bomb */
  {SND_NEWBOMB,  "xbnbmb.raw", NULL, 0, FALSE, TRUE},   /* got an extra bomb */
  {SND_NEWKICK,  "xbnkick.raw", NULL, 0, FALSE, TRUE},  /* got kick extra */
  {SND_NEWPUMP,  "xbnpmp.raw", NULL, 0, FALSE, TRUE},   /* got pump extra */
  {SND_NEWRC,    "xbnrc.raw", NULL, 0, FALSE, TRUE},    /* got rem. control */
  {SND_MOREFIRE, "xbfire.raw", NULL, 0, FALSE, TRUE},   /* got more range */
  {SND_DEAD,     "xb_dead.raw", NULL, 0, FALSE, FALSE}, /* player died */
  {SND_EXPL,     "xb_expl.raw", NULL, 0, FALSE, TRUE},  /* normal explosion */
  {SND_KICK,     "xb_kick.raw", NULL, 0, FALSE, TRUE},  /* kick a bomb */
  {SND_PUMP,     "xb_pump.raw", NULL, 0, FALSE, TRUE},  /* pump a bomb */
  {SND_OUCH,     "xb_ouch.raw", NULL, 0, FALSE, FALSE}, /* player lost life */
  {SND_INTRO,    "xb_intro.raw", NULL, 0, FALSE, FALSE},/* intro fanfare */
  {SND_APPL,     "xb_appl.raw", NULL, 0, FALSE, FALSE}, /* applause */
  {SND_BUTT,     "xb_butt.raw", NULL, 0, FALSE, TRUE},  /* triggered button */
  {SND_SHOOT,    "xb_shoot.raw", NULL, 0, FALSE, FALSE},/* using rem. ctrl. */
  {SND_INVIS,    "xb_nvis.raw", NULL, 0, FALSE, FALSE}, /* player invisible */
  {SND_INVINC,   "xb_nvnc.raw", NULL, 0, FALSE, FALSE}, /* player invincible */
  {SND_NEWTELE,  "xbntel.raw", NULL, 0, FALSE, TRUE},   /* player got telep. */
  {SND_TELE,     "xbtele.raw", NULL, 0, FALSE, TRUE},   /* player uses tele. */
  {SND_INJ,      "xbinj.raw", NULL, 0, FALSE, FALSE},   /* player got junkie */
  {SND_MINIBOMB, "xbmbmb.raw", NULL, 0, FALSE, TRUE},   /* small bomb expl. */
  {SND_WON,      "xb_won.raw", NULL, 0, FALSE, FALSE},  /* player won */
  {SND_HAUNT,    "xb_haunt.raw", NULL, 0, FALSE, FALSE},/* haunting bomb */
  {SND_SPIRAL,   "xb_spir.raw", NULL, 0, FALSE, TRUE},  /* spiral shrinking */
  {SND_SPBOMB,   "xb_spbmb.raw", NULL, 0, FALSE, TRUE}, /* got special bomb */
  {SND_SLIDE,    "xbslide.raw", NULL, 0, FALSE, TRUE},  /* bomb slide sound */
  {SND_FINALE,   "xbfin.raw", NULL, 0, FALSE, FALSE},   /* final fanfare */
  {SND_WARN,     "xb_warn.raw", NULL, 0, FALSE, FALSE}, /* shrink warn sound */
  {SND_STUN,     "xb_stun.raw", NULL, 0, FALSE, FALSE}, /* player stun sound */
  {SND_WHIRL,    "xb_whrl.raw", NULL, 0, TRUE, FALSE},  /* intro whirl */
  {SND_COMPOUND, "xb_cmpnd.raw", NULL, 0, FALSE, FALSE},/* compound shrink */
  {SND_SNG1,     "xbsng1.raw", NULL, 0, TRUE, FALSE},   /* Backgr. song #1 */
  {SND_SNG2,     "xbsng2.raw", NULL, 0, TRUE, FALSE},   /* Backgr. song #2 */
  {SND_SNG3,     "xbsng3.raw", NULL, 0, TRUE, FALSE},   /* Backgr. song #3 */
  {SND_SNG4,     "xbsng4.raw", NULL, 0, TRUE, FALSE},   /* Backgr. song #4 */
  {SND_SNG5,     "xbsng5.raw", NULL, 0, TRUE, FALSE},   /* Backgr. song #5 */
  {SND_SNG6,     "xbsng6.raw", NULL, 0, TRUE, FALSE},   /* Backgr. song #6 */
  {SND_LAST,     NULL, NULL, 0}              
};

#define SUBSIZE          2048
#define FRAGSIZE         0x0002000b

#define SOUND_DEVICE "/dev/dsp"
#define SAMPLE_RATE     22050
#define SAMPLE_CHANNELS     1
#define SAMPLE_SIZE         8

static s16 sumbuff[SUBSIZE];
static u8  playbuff[SUBSIZE];

static int mono_mode = FALSE;

static int fragsize =        FRAGSIZE;
static int sample_rate =     SAMPLE_RATE;
static int sample_channels = SAMPLE_CHANNELS;
static int sample_size =     SAMPLE_SIZE;

/*
 * outcomment the following line to suppress server statistics
 */
#define SERVER_STATISTICS

#if defined(SERVER_STATISTICS)
static double total_samples  = 0.0;
static int    total_played   = 0;
static int    total_loaded   = 0;
static int    total_unloaded = 0;
#endif

/*
 * Abort signal handler
 */
static void
server_abort()
{
#if defined(SERVER_STATISTICS)
  fprintf(stderr, "XBlast sound server statistics:\n");
  fprintf(stderr, "\tloaded %d sounds,\n", total_loaded);
  fprintf(stderr, "\tfreed %d sounds,\n", total_unloaded);
  fprintf(stderr, "\tplayed %d sounds,\n", total_played);
  fprintf(stderr, "\tprocessed %10.3f Mega samples on sound device.\n", 
	 total_samples / 1000000.0);
#endif
  fprintf(stderr, "XBlast sound server terminated.\n");
  fflush(stderr);
  exit(0);
}

/*
 * initialize sound device
 */
#if defined(__STDC__)
static void
init_dsp(int dsp)
#else
static void
init_dsp(dsp)
int dsp;
#endif
{
  if (ioctl(dsp, SNDCTL_DSP_SETFRAGMENT, &fragsize) < 0)
    {
      fprintf(stderr, "XBlast sound server: could not set fragment size %8x on sound device\n", fragsize);
    }
  if (ioctl(dsp, SNDCTL_DSP_STEREO, &sample_channels) < 0)
    {
      fprintf(stderr, "XBlast sound server: could not set %d sample channels on sound device\n", sample_channels);
    }
  if (ioctl(dsp, SNDCTL_DSP_SPEED, &sample_rate) < 0)
    {
      fprintf(stderr, "XBlast sound server: could not set sample rate %d on sound device\n", sample_rate);
    }
  if (ioctl(dsp, SNDCTL_DSP_SETFMT, &sample_size) < 0)
    {
      fprintf(stderr, "XBlast sound server: could not set sample size %d on sound device\n", sample_size);
    }
  if (ioctl(dsp, SNDCTL_DSP_GETBLKSIZE, &fragsize) < 0)
    {
      fprintf(stderr, "XBlast sound server: could not get block size of sound device\n");
    }
}


/*
 * resync sound device 
 */
#if defined(__STDC__)
static void 
resync(int dsp)
#else
static void
resync(dsp)
int dsp;
#endif
{
  /* clear sample sum buffer */
  register int i;
  register u8 *s = playbuff;
  
  for (i = 0; i < SUBSIZE; i++)
    {
      *s++ = 128;
    }
  /* resync sound device to correct any channel flipping */
  write(dsp, playbuff, SUBSIZE);
  write(dsp, playbuff, SUBSIZE);
  write(dsp, playbuff, SUBSIZE);
  (void) ioctl(dsp, SNDCTL_DSP_SYNC, NULL);
}

/*
 * load sound samples into server memory
 */
#if defined(__STDC__)
static int
server_load_sound(int number)
#else
static int
server_load_sound(number)
int number;
#endif
{
  char fname[1000];
  int f;

  strcpy(fname, SOUND_DIR);
  strcat(fname, "/");
  strcat(fname, sound_name[number].name);

  if ((f = open(fname, O_RDONLY)) < 0)
    {
      fprintf(stderr, "could not open sound data file %s\n", fname);
      return(-1);
    }
  else
    {
      int sound_size;
      u8 *sb;
      struct stat snd_stat;

      (void) fstat(f, &snd_stat);
      sound_size = snd_stat.st_size / sizeof(u8);
      if (sound_name[number].samples != NULL)
	{
	  free(sound_name[number].samples);
	  sound_name[number].samples = NULL;
	  sound_name[number].length  = 0;
	}
	  
      if ((sb = (u8 *) malloc(sound_size * sizeof(u8))) == NULL)
	{
	  close(f);
	  return(-1);
	}
      else
	{
	  read(f, sb, sound_size * sizeof(u8));
	  sound_name[number].samples = sb;
	  sound_name[number].length = sound_size;
	  close(f);
#if defined(SERVER_STATISTICS)
	  total_loaded++;
#endif
	  /*
	   * convert stereo samples to mono if running in mono mode 
	   */
	  if (mono_mode == TRUE && sound_name[number].mono == FALSE)
	  {
	    int i;
	    u8 *m, *s;
	    s16 sum;

	    m = s = sound_name[number].samples;

	    sound_name[number].length >>= 1;
	    for (i = 0; i < sound_name[number].length; i++)
	      {
		sum = *s + *(s + 1);
		*m++ = sum >> 1;
		s += 2;
	      }
	  }
	  return(0);
	}
    }
}

/*
 * free sample memory of a given sound
 */
#if defined(__STDC__)
static void
server_unload_sound(int id)
#else
static void
server_unload_sound(id)
int id;
#endif
{
  if (sound_name[id].samples != NULL)
    {
      free(sound_name[id].samples);
      sound_name[id].samples = NULL;
      sound_name[id].length  = 0;
#if defined(SERVER_STATISTICS)
      total_unloaded++;
#endif
    }
}

/*
 * main function
 */
int
main(argc, argv)
int argc;
char **argv;
{
  int dsp;
  XBSOUND *first = NULL;
  int do_sync = TRUE;
  int did_sync = FALSE;
  int ack_val;

  /*
   * open and prepare sound device
   */
  if ((dsp = open(SOUND_DEVICE, O_WRONLY)) < 0)
    {
      fprintf(stderr, "XBlast sound server: could not open sound device %s\n",
	      SOUND_DEVICE);
      ack_val = SND_ACK_ERROR;
      write(1, &ack_val, sizeof(ack_val));
      exit(-1);
    }
  ack_val = SND_ACK_OK;
  write(1, &ack_val, sizeof(ack_val));

  while (--argc > 0)
    {
      ++argv;
      if (!strcmp("-mono", *argv))
	{
	  mono_mode = TRUE;
	}
      else
	{
	  fprintf(stderr, "XBlast sound server: unknown option %s ignored\n",
		  *argv);
	}
    }

  if (mono_mode == TRUE)
    {
      sample_channels = 0;
    }

  init_dsp(dsp);
  /*
   * install server abort signal handler
   */
  signal(SIGINT, server_abort);

  fprintf(stderr, "XBlast sound server $Revision: 1.13 $ running in %s mode.\n",
	  (mono_mode == TRUE) ? "mono" : "stereo");

  /*
   * loop forever (or SIGINT)
   */
  while (1)
    {
      /* clear sample sum buffer */
      {
	register int i;
	register s16 *s = sumbuff;

	for (i = 0; i < SUBSIZE; i++)
	  {
	    *s++ = 128;
	  }
      }

      if (first == NULL)
	{
	  /* no sound to play, may sync */
	  do_sync = TRUE;
	}
      else
	{
	  do_sync = FALSE;
	  did_sync = FALSE;
	}

      /* sum samples in sumup buffer */
      if (mono_mode == TRUE)
	{
	  /*
	   * process sounds in mono mode
	   */
	  XBSOUND *xs;
	  
	  for (xs = first; xs != NULL; xs = xs->next)
	    {
	      int i;
	      register s16 *s = sumbuff;
	      
	      for (i = 0; i < SUBSIZE && xs->length > 0; i++, xs->length--)
		{
		  *s++ += ((s16) *xs->samples++) - 128;
		}

#if defined(SERVER_STATISTICS)
	      total_samples += (double) i;
#endif
	      /* repeat a sound if this is required */
	      if (xs->length <= 0 && xs->repeat == TRUE)
		{
		  int id = xs->id;
		  xs->length  = sound_name[id].length;
		  xs->samples = sound_name[id].samples;
		}
	    }
	  
	  /* correct clipping */
	  {
	    register int i;
	    register s16 *s = sumbuff;
	    for (i = 0; i < SUBSIZE; i++)
	      {
		if (*s > 255)
		  {
		    *s = 255;
		  }
		else if (*s < 0)
		  {
		    *s = 0;
		  }
		s++;
	      }
	  }
      
	  /* copy sum buffer to playback buffer and play it */
	  {
	    register u8 *d = playbuff;
	    register s16 *s = sumbuff;
	    register int i;
	    
	    for (i = 0; i < SUBSIZE; i++)
	      {
		*d++ = (u8) *s++;
	      }
	    
	    /* play buffer */
	    if (first != NULL)
	      {
		write(dsp, playbuff, SUBSIZE);
	      }
	    else
	      {
		(void) ioctl(dsp, SNDCTL_DSP_SYNC, NULL);
	      }
	  }
	}
      else
	{
	  /*
	   * process sounds in stereo  mode
	   */
	  XBSOUND *xs;
	  
	  for (xs = first; xs != NULL; xs = xs->next)
	    {
	      int i;
	      register s16 *s = sumbuff;
	      
	      for (i = 0; i < SUBSIZE && xs->length > 0; i++, xs->length--)
		{
		  if (xs->mono == TRUE)
		    {
		      /* calc. position of mono sounds and add to sumup buffer */
		      int pos    = xs->position;
		      s16 sample = ((s16) *xs->samples++) - 128;
		      s16 sr, sl;
		      
#if 0
		      if (pos < SOUND_MIDDLE_POSITION)
			{
			  /* sound on left side */
			  sl = sample;
			  sr = sample / (SOUND_MIDDLE_POSITION - pos);
			}
		      else if (pos > SOUND_MIDDLE_POSITION)
			{
			  /* sound on right side */
			  sr = sample;
			  sl = sample / (pos - SOUND_MIDDLE_POSITION);
			}
		      else
			{
			  /* sound in the middle */
			  sl = sr = sample;
			}
#else 
		      sr = (sample*(pos+1)) >> 4;
		      sl = (sample*(pos-MAX_SOUND_POSITION+1)) >> 4;
#endif
		      
		      *s++ += sl;
		      *s++ += sr;
		      i++;
		    }
		  else
		    {
		      *s++ += ((s16) *xs->samples++) - 128;
		    }
		}
	      
#if defined(SERVER_STATISTICS)
	      total_samples += (double) i;
#endif
	      /* repeat a sound if this is required */
	      if (xs->length <= 0 && xs->repeat == TRUE)
		{
		  int id = xs->id;
		  xs->length  = sound_name[id].length;
		  xs->samples = sound_name[id].samples;
		}
	    }
	  
	  /* correct clipping */
	  {
	    register int i;
	    register s16 *s = sumbuff;
	    for (i = 0; i < SUBSIZE; i++)
	      {
		if (*s > 255)
		  {
		    *s = 255;
		  }
		else if (*s < 0)
		  {
		    *s = 0;
		  }
		s++;
	      }
	  }
      
	  /* copy sum buffer to playback buffer and play it */
	  {
	    register u8 *d = playbuff;
	    register s16 *s = sumbuff;
	    register int i;
	    
	    for (i = 0; i < SUBSIZE; i++)
	      {
		*d++ = (u8) *s++;
	      }
	    
	    /* play buffer */
	    if (first != NULL)
	      {
		write(dsp, playbuff, SUBSIZE);
	      }
	    else
	      {
		(void) ioctl(dsp, SNDCTL_DSP_SYNC, NULL);
	      }
	  }
	}
      /* resync sound device to correct any channel flipping */
#if 0
      if (do_sync == TRUE && did_sync == FALSE)
	{
	  (void) ioctl(dsp, SNDCTL_DSP_SYNC, NULL);
	  did_sync = TRUE;
	}
#endif
      /* remove all empty sounds from list */
      {
	XBSOUND *s, *n;
	
	while (first != NULL && first->length <= 0)
	  {
	    XBSOUND *t = first->next;
	    free(first);
	    first = t;
	  }
	s = first;
	while (s != NULL)
	  {
	    if ((n = s->next) != NULL && 
		n->length <= 0)
	      {
		s->next = n->next;
		free(n);
	      }
	    s = s->next;
	  }
      }
      
      /* check for new commands in input pipe */
      {
	int command_buff[1024];
	int *cmd = command_buff;
	int n;
	
	if ((n = read(0, command_buff, 1024)) > 0)
	  {
	    /* there are commands in the pipe */
	    while (n > 0)
	      {
		if (*cmd == SND_PLAY_SOUND)
		  {
		    XBSOUND *new = (XBSOUND *) malloc(sizeof(XBSOUND));
		    int id = *++cmd;
		    
		    if (new != NULL)
		      {
			/* append new sound to play to play list */
#if defined(SERVER_STATISTICS)
			total_played++;
#endif
			new->id       = id & 0xffff;
			new->position = (id >> 16) & 0xffff;
			id           &= 0xffff;
			new->repeat   = sound_name[id].repeat;
			new->mono     = sound_name[id].mono;
			new->samples  = sound_name[id].samples;
			new->length   = sound_name[id].length;
			new->next     = NULL;

			if (first == NULL)
			  {
			    first = new;
			  }
			else
			  {
			    XBSOUND *s;
			    for (s = first; s->next != NULL; s = s->next)
			      {
				;
			      }
			    s->next = new;
			  }
		      }
		  }
		else if (*cmd == SND_STOP_SOUND)
		  {
		    int stop_id = *++cmd;

		    if (stop_id == 0 || stop_id == STOP_ALL_SOUNDS)
		      {
			XBSOUND *s;
			for (s = first; s != NULL; s = s->next)
			  {
			    s->length = 0;
			    s->repeat = 0;
			  }
			resync(dsp);
		      }
		    else
		      {
			XBSOUND *s;
			for (s = first; s != NULL; s = s->next)
			  {
			    if (s->id == stop_id)
			      {
				s->length = 0;
				s->repeat = 0;
			      }
			  }
		      }
		    write(1, cmd, 1);
		  }
		else if (*cmd == SND_LOAD_SOUND)
		  {
		    server_load_sound(*++cmd);
		    write(1, cmd, 1);
		  }
		else if (*cmd == SND_UNLOAD_SOUND)
		  {
		    server_unload_sound(*++cmd);
		    write(1, cmd, 1);
		  }
		n -= 2 * sizeof(int);
		cmd++;
	      }
	  }
      }
    }
}

#endif
