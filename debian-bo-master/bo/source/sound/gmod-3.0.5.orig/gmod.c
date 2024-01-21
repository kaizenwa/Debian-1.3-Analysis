// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

/*
 *	gmod.c	- Module player for GUS and Linux.
 *		(C) Hannu Savolainen, 1993
 *
 *	NOTE!	This program doesn't try to be a complete module player.
 *		It's just a too I used while developing the driver. In
 *		addition it can be used as an example on programming
 *		the VoxWare Sound Driver with GUS.
 */

/*
 * Many modifications have been done by Andrew J. Robinson.
 * Refer to the ChangeLog for details.
 */


#include <sys/ioctl.h>
#include <sys/time.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>

#ifdef USE_LOCAL
#include "soundcard.h"
#else
#include <sys/soundcard.h>
#endif

#include <sys/ultrasound.h>
#include <stdio.h>
#include <string.h>

#include <time.h>		/* for randomize */
#include <stdlib.h>		/* for randomize */

#include "defines.h"
#include "structs.h"
#include "tables.h"
#include "protos.h"

#ifdef USE_X
#include <limits.h>
#include <qmsgbox.h>

#include "TopShell.h"
#include "TrackShell.h"
#include "OptShell.h"
#include "QueueShell.h"
#include "SampleShell.h"
#include "CommentShell.h"
#else
#include "CursesScr.h"
#endif

#include "Sequencer.h"
#include "Sample.h"

SEQ_DEFINEBUF (2048);

int pattern_len[MAX_POSITION];
int pattern_tempo[MAX_POSITION];
struct note_info *pattern_table[MAX_PATTERN * MAX_TRACK];

struct voice_info voices[MAX_TRACK];

int tune[MAX_POSITION];
short voice_table[MAX_POSITION][MAX_TRACK];
double tick_duration;

int mixerfd;
Sample *samples[MAX_SAMPLES];
int tmp, gus_dev;
double this_time, next_time;
int ticks_per_division;

#ifndef USE_X
unsigned char stop_flag = 0;
#endif
unsigned char background = 0;
int actual_pos = 0;
int position_change = 0;

unsigned int seq_input = ECHO_NONE;

struct x_struct x_info;
int current_mod;

#ifdef USE_X
TopShell *topShell;
TrackShell *trackShell;
OptShell *optShell;
QueueShell *queueShell;
SampleShell *sampleShell;
CommentShell *commentShell;
char * empty_list[] = { "" };
#else
CursesScr *cursScreen;
#endif

void read_rc (FILE *, char *, struct options_info *);
unsigned int proc_input (void);
void set_signals (void);

#ifdef USE_X
  struct options_info options =
    {255, 0, 1, 1, 0, 0, 0, 255, 0, 0, 100, 0};
#else
  struct options_info options =
    {255, 0, 0, 1, 1, 0, 0, 0, 255, 0, 0, 100, 0};
#endif
struct options_info saved_opt;
FILE *rc_fp = NULL;
struct song_info song_char;
int start_pos = 0;

Sequencer *seq;

int
main (int argc, char *argv[])
{
  static char ident[] = IDENT;
  int i, seq_open_ret, name_start, num_files, rand_swap;
#ifndef USE_X
  int exit_code = ERR_NOERROR;
  char mixer_name[13];
  int mix_devmask;
#endif
  char *tmp_argv, *rc_filename;
  
  // do all the opens first in case the process is SUID or SGID

  Sequencer lseq;
  seq = &lseq;
  seq_open_ret = lseq.open();

#ifndef USE_X
  if (options.mixer == 255)
    sprintf (mixer_name, "%s", "/dev/mixer");
  else
    sprintf (mixer_name, "%s%u", "/dev/mixer", options.mixer);

  mixerfd = open (mixer_name, O_RDWR, 0);
#endif

  // drop SUID and SGID priveledge if we have it
  setuid(getuid());
  setgid(getgid());

  song_char.comment = (char *)calloc (1, 1);

  for (i = 0; i < MAX_PATTERN * MAX_TRACK; i++)
    pattern_table[i] = NULL;
  
  for (i = 0; i < MAX_SAMPLES; i++)
    samples[i] = 0;

#ifdef USE_X
  QApplication a(argc, argv);
  commentShell = new CommentShell;
  optShell = new OptShell;
  queueShell = new QueueShell;
  sampleShell = new SampleShell;
  trackShell = new TrackShell;
  topShell = new TopShell(commentShell, optShell, queueShell, sampleShell, trackShell);
  a.setMainWidget(topShell);
  topShell->show();
#endif

  name_start = parse_args (argc, argv, &options);

#ifdef USE_X
  saved_opt.extend_oct = options.extend_oct;
#endif

#ifndef USE_X
  printf (HEADING);
  printf ("Original source (C) Hannu Savolainen, 1993\n");
  printf ("MTM/ULT loaders by Robert Sanders\n");
  printf ("Continuing development by Andrew J. Robinson\n\n");

  if (name_start == argc)
    {
      printf ("\nUsage: %s [options] modfile . . .\n", argv[0]);
      printf ("Use %s -h for help.\n\n", argv[0]);
      exit (ERR_BADARGS);
    }
#endif

  switch (seq_open_ret)
    {
    case -1:
#ifdef USE_X
      QMessageBox::message("Xgmod Error", "Cannot open /dev/sequencer");
#else
      perror ("/dev/sequencer");
#endif
      exit (ERR_SEQUENCER);
    case -2:
#ifdef USE_X
      QMessageBox::message("Xgmod Error", "Cannot determine number of synths");
#else
      perror ("/dev/sequencer");
#endif
      exit (ERR_SEQUENCER);
    case -3:
#ifdef USE_X
      QMessageBox::message("Xgmod Error", "Cannot determine synth information");
#else
      perror ("/dev/sequencer");
#endif
      exit (ERR_SEQUENCER);
    case -4:
#ifdef USE_X
      QMessageBox::message("Xgmod Error", "Gravis Ultrasound not detected");
#else
      fprintf (stderr, "Gravis Ultrasound not detected\n");
#endif
      exit (ERR_NOGUS);
    default:
      gus_dev = lseq.gus_device();
    }

#ifndef USE_X
  if (options.mixer == 255)
    sprintf (mixer_name, "%s", "/dev/mixer");
  else
    sprintf (mixer_name, "%s%u", "/dev/mixer", options.mixer);

  if (mixerfd == -1)
    printf ("Mixer (%s) not available.\n", mixer_name);
  else
    {
      ioctl (mixerfd, SOUND_MIXER_READ_DEVMASK, &mix_devmask);
      if (!(mix_devmask & SOUND_MASK_SYNTH))
	{
	  printf ("This mixer (%s) does not support volume control.\n", mixer_name);
	  close (mixerfd);
	  mixerfd = -1;
	}
    }
#endif

  if ((tmp_argv = getenv ("HOME")) != NULL)
    {
      rc_filename = (char *) malloc (strlen (tmp_argv) + 9);
      strcpy (rc_filename, tmp_argv);
      strcat (rc_filename, USER_RC_NAME);
      rc_fp = fopen (rc_filename, "r");
      free (rc_filename);
    }

  if (rc_fp == NULL)
    rc_fp = fopen (RC_NAME, "r");

#ifndef USE_X
  cursScreen = new CursesScr(background);
#endif /* USE_X */

  num_files = argc - name_start;
  srand (time (NULL));

  if (options.randomize)
    {
      for (i = name_start; i < argc; i++)
	{
	  rand_swap = name_start + (rand () % num_files);
	  tmp_argv = argv[i];
	  argv[i] = argv[rand_swap];
	  argv[rand_swap] = tmp_argv;
	}
    }

  x_info.nrFileStrings = argc - name_start;

#ifndef USE_X
#define String char *
#endif

  if (x_info.nrFileStrings > 0)
    {
      x_info.fileStrings = (char * *)malloc (sizeof (char *) * x_info.nrFileStrings);

      for (i = name_start; i < argc; i++)
	{
	  x_info.fileStrings[i - name_start] = strdup (argv[i]);
#ifdef USE_X
	  queueShell->addFile(x_info.fileStrings[i - name_start]);
#endif
	}
    }
  else
    x_info.fileStrings = NULL;
  
  saved_opt = options;

#ifdef USE_X
  seq->readEnabled(TRUE);
#else
  set_signals ();
#endif

  stop_all_channels (MY_FALSE);

#ifdef USE_X
  current_mod = -1;

  a.exec();
#else
  current_mod = 0;

  while ((stop_flag != STOP_EXIT) && (current_mod < x_info.nrFileStrings))
    {
      if (start_playback (stop_flag))
	{
	  seq_input = ECHO_NONE;

	  while (seq_input != ECHO_END)
	    {
	      NoXProcessEvent ();
	    }
	  
	  ioctl(seq->seq_fd(), SNDCTL_SEQ_RESET, 0);
	  start_pos = end_module (stop_flag);

	  switch (stop_flag)
	    {
	    case STOP_FORWBACK:
	      start_pos = actual_pos + position_change;
	      
	      if (start_pos < 0)
		start_pos = 0;
	      
	      break;
	    case STOP_EXIT:
	      break;
	    case STOP_PREV:
	      if (current_mod > 0)
		current_mod--;
	      start_pos = 0;
	      break;
	    default:
	    case STOP_NEXT:
	      current_mod++;
	      start_pos = 0;
	      break;
	    }
	}
      else
	{
	  current_mod++;
	  start_pos = 0;
	}
    }

  //SEQ_DUMPBUF ();

  lseq.close();
  fclose (rc_fp);
  
#ifndef USE_X
  delete cursScreen;
#endif

  exit (exit_code);
#endif /* not USE_X */
}

int
start_playback(unsigned char lstop_flag)
{
  int load_rc = 0;
  int start_delay; 
  extern char played[MAX_POSITION];
#ifdef USE_X
  extern TopShell *topShell;
#endif

#ifdef USE_X
  if (optShell->highlightChecked() == TRUE)
    queueShell->currentClicked();
#endif
  if (lstop_flag != STOP_FORWBACK)
    {
      struct timeval tv_start, tv_end;
      start_pos = 0;

      gettimeofday(&tv_start, NULL);

      options = saved_opt;

#ifdef USE_X
      if (optShell->fiftyhzChecked() == TRUE)
	options.use_50hz = 1;
      else
	options.use_50hz = 0;
      
      if (optShell->ntscChecked() == TRUE)
	options.ntsc = 1;
      else
	options.ntsc = 0;
      
      if (optShell->bpmChecked() == TRUE)
	options.bpm_tempos = 0;
      else
	options.bpm_tempos = 1;

      options.extend_oct = optShell->octaveSelected();
#endif

      if (rc_fp != NULL)
	{
	  read_rc (rc_fp, x_info.fileStrings[current_mod], &options);
	}
      
      load_rc = load_module (x_info.fileStrings[current_mod], &song_char, options);
      
      gettimeofday(&tv_end, NULL);
      tv_end.tv_sec -= tv_start.tv_sec;
      
      if (tv_start.tv_usec > tv_end.tv_usec)
	{
	  tv_end.tv_sec -= 1;
	  tv_end.tv_usec = tv_end.tv_usec + 1000000 - tv_start.tv_usec;
	}
      else
	tv_end.tv_usec -= tv_start.tv_usec;

      if (tv_end.tv_sec >= 1)
	start_delay = 0;
      else
	start_delay = (1000000 - tv_end.tv_usec) / 10000;
    }
  else
    {
      load_rc = 1;
      start_delay = 0;
      // prevent "rewind" from detecting a loop
      played[start_pos] = 0;
    }

#ifndef USE_X
  stop_flag = 0;
#endif

  if (load_rc)
    {
      actual_pos = 0;
      position_change = 0;
      
      seq->sync();
      
      if (song_char.nr_channels < 14)
	{
	  GUS_NUMVOICES (gus_dev, 14);
	}
      else
	{
	  GUS_NUMVOICES (gus_dev, song_char.nr_channels);
	}
      
      /* set the proper volume method */
      SEQ_VOLUME_MODE (gus_dev, VOL_METHOD_LINEAR);
      
      //SEQ_DUMPBUF ();
      seq->force();
      seq->sync();
      
#ifdef USE_X
      //flush_xbuffer ();
      seq->writeEnabled(TRUE);
#endif
      play_module (start_pos, &song_char, options, start_delay);
    }
#ifdef USE_X
  else
    topShell->moduleTitle("Load Failed");
#endif

  return (load_rc);
}
