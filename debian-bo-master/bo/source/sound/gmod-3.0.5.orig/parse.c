// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#include <unistd.h>
#include <getopt.h>
#include <stdio.h>
#include <stdlib.h>

#include "defines.h"
#include "structs.h"
#include "globals.h"

int
parse_args (int argc, char *argv[], struct options_info *options)
{
  extern char *optarg;
  extern int optind;
  int option, num_err = 0;

#ifdef USE_X
  while ((option = getopt (argc, argv, "chP:rRv:")) != -1)
#else
#ifdef USE_NCURSES
    while ((option = getopt (argc, argv, "bchlm:nP:qrRsv:xz5")) != -1)
#else
    while ((option = getopt (argc, argv, "bcehlm:nP:qrRsv:xz5")) != -1)
#endif
#endif
      switch (option)
	{
	case 'h':
	  printf (HEADING);
	  printf ("Original source (C) Hannu Savolainen, 1993\n");
	  printf ("MTM/ULT loaders by Robert Sanders\n");
          printf ("Continuing development by Andrew J. Robinson\n\n");
	  printf ("Usage: %s [options] [modfile . . .]\n\n", argv[0]);
	  printf ("Options:\n");
#ifndef USE_X
	  printf ("     -b     DISABLE BPM tempos\n");
#endif
	  printf ("     -c     DISABLE compression of modules in memory\n");
#ifndef USE_X
#ifndef USE_NCURSES
	  printf ("     -e     Show empty samples (for messages)\n");
#endif
#endif
	  printf ("     -h     Help\n");
#ifndef USE_X
	  printf ("     -l     Break infinite loops\n");
	  printf ("     -m x   Use mixer number x (0 - 15)\n");
	  printf ("     -n     Use NTSC sample timing\n");
#endif
	  printf ("     -P     Set panning factor to x percent (-100 to 100)\n");
#ifndef USE_X
	  printf ("     -q     Quiet mode\n");
#endif
	  printf ("     -r     Infinitely repeat module\n");
	  printf ("     -R     Randomize module play order\n");
#ifndef USE_X
	  printf ("     -s     Ignore speed 0 commands\n");
#endif
	  printf ("     -v x   Set volume to x (0 - 255)\n");
#ifndef USE_X
	  printf ("     -x     Extend octaves\n");
	  printf ("     -z     Background mode\n");
	  printf ("     -5     Use 50 Hz clock frequency\n\n");
	  printf ("One or more MultiTracker, UltraTracker, MOD (4/6/8 channel), S3M,\n");
	  printf ("669, or XM files should be listed on the command line.\n");
#endif
	  exit (ERR_NOERROR);
#ifndef USE_X
	case 'b':
	  options->bpm_tempos = 0;
	  break;
#endif
	case 'c':
	  options->compress = 0;
	  break;
#ifndef USE_X
#ifndef USE_NCURSES
	case 'e':
	  options->show_empty_samples = 1;
	  break;
#endif
#endif
#ifndef USE_X
	case 'l':
	  options->loop_breaker = 1;
	  /* override repeat */
	  options->repeat = 0;
	  break;
	case 'm':
	  option = atoi (optarg);
	  if ((option > 15) || (option < 0))
	    {
	      printf ("%s: mixer number must be between 0 and 15.\n", argv[0]);
	      num_err = 1;
	    }
	  else
	    options->mixer = option;
	  break;
	case 'n':
	  options->ntsc = 1;
	  break;
#endif
	case 'P':
	  option = atoi (optarg);
	  if ((option > 100) || (option < -100))
	    {
	      printf ("%s: panning factor must be between -100 and 100.\n", argv[0]);
	      num_err = 1;
	    }
	  else
	    options->pan_factor = option;
	  break;
#ifndef USE_X
	case 'q':
	  fclose (stdout);
	  fclose (stderr);
	  break;
#endif
	case 'r':
	  options->repeat = 1;
	  /* override loop breaker */
#ifndef USE_X
	  options->loop_breaker = 0;
#endif
	  break;
	case 'R':
	  options->randomize = 1;
	  break;
#ifndef USE_X
	case 's':
	  options->tolerant = 1;
	  break;
#endif
	case 'v':
	  option = atoi (optarg);
	  if ((option > 255) || (option < 0))
	    {
	      printf ("%s: volume must be between 0 and 255.\n", argv[0]);
	      num_err = 1;
	    }
	  else
	    options->main_volume = option;
	  break;
#ifndef USE_X
	case 'x':
	  options->extend_oct = OCTAVE_EXTEND;
	  break;
	case 'z':
	  background = 1;
	  break;
	case '5':
	  options->use_50hz = 1;
	  break;
#endif
	case '?':
	  num_err = 1;
	  break;
	case ':':
	  num_err = 1;
	  break;
	}

  if (num_err)
    {
      printf ("\nUsage: %s [options] modfile . . .\n", argv[0]);
      printf ("Use %s -h for help.\n\n", argv[0]);
      exit (ERR_BADARGS);
    }

  return optind;
}
