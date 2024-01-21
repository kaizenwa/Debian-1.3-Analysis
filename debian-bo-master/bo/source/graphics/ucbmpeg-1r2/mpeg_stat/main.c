/* MPEGSTAT - analyzing tool for MPEG-I video streams
 * Most recently by Steve Smoot (see man page for history)
 *
 *  Copyright (c) 1995 The Regents of the University of California.
 * All rights reserved.
 *
 * Tcl interface by Keving Gong
 *
 * Copyright (c) 1995 The Regents of the University of California.
 * 
 * Technical University of Berlin, Germany, Dept. of Computer Science
 * Tom Pfeifer - Multimedia systems project - pfeifer@fokus.gmd.de
 *
 * Jens Brettin, Harald Masche, Alexander Schulze, Dirk Schubert
 *
 * This program uses parts of the source code of the Berkeley MPEG player
 *
 * ---------------------------
 *
 * Copyright (c) 1993 Technical University of Berlin, Germany
 *
 * for the parts of the Berkeley player used:
 *
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * ---------------------------
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose, without fee, and without written agreement is
 * hereby granted, provided that the above copyright notices and the following
 * two paragraphs appear in all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA 
 * or the Technical University of Berlin BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA or the Technical University of Berlin HAS BEEN ADVISED OF THE 
 * POSSIBILITY OF SUCH DAMAGE.
 * 
 * THE UNIVERSITY OF CALIFORNIA and the Technical University of Berlin 
 * SPECIFICALLY DISCLAIM ANY WARRANTIES, INCLUDING, BUT NOT LIMITED TO, 
 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS ON AN "AS IS" BASIS, AND THE 
 * UNIVERSITY OF CALIFORNIA and the Technical University of Berlin HAVE NO 
 * OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, 
 * OR MODIFICATIONS.
 */

/* MAIN.C CHANGED FOR MPEG ANALYZER, 1993 */

#include "video.h"
#include "proto.h"
#include <sys/types.h>
#include <signal.h>
#include <string.h>
#include <stdlib.h>
#include <netinet/in.h>

#include "util.h"
#include "opts.h"

/* VERSION */
#define VERSION "2.2b"

/* Define buffer length. */

#define BUF_LENGTH 80000

/* External declaration of main decoding call. */

extern VidStream *mpegVidRsrc();
extern VidStream *NewVidStream();

/* Declaration of global variable to hold dither info. */

int ditherType;

/* Global file pointer to incoming data. */
FILE *input;

/* global options state */
int opts = LOUD;
int start_opt = -1;
int end_opt = -1;
int rate_frames = 0;
int COLLECTING;
FILE *block_fp;
FILE *qscale_fp;
FILE *size_fp;
FILE *offs_fp;
FILE *rate_fp;
FILE *syslogOutput;
FILE *hist_fp;
FILE *userdat_fp;

/* End of File flag. */
int EOF_flag = 0;

/* The video stream */
static VidStream *theStream;

/*
 *--------------------------------------------------------------
 *
 * int_handler --
 *
 *	Handles Cntl-C interupts..
 *      (Two copies, to handle different signal types)
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */
#ifndef SIG_ONE_PARAM
void int_quit(){exit(1);}
void
int_handler()
{
  fprintf(stderr,"\nBreak!\n");
  signal(SIGINT, int_quit);
  /* If we havent done much, just exit */
  if ((theStream==NULL) || (theStream->past == NULL)) exit(1);
  fprintf(stderr,"Warning, some stats may not make sense with incomplete data!\n");
  PrintAllStats();
  if (curVidStream != NULL)
    DestroyVidStream(curVidStream);
  exit(1);
}
#else
void int_quit(signum)int signum;{exit(1);}
void int_handler(signum)
int signum;
{
  fprintf(stderr,"\nBreak!\n");
  signal(SIGINT, int_quit);
  /* If we havent done much, just exit */
  if ((theStream==NULL) || (theStream->past == NULL)) exit(1);
  fprintf(stderr,"Warning, some stats may not make sense with incomplete data!\n");
  PrintAllStats();
  if (curVidStream != NULL)
    DestroyVidStream(curVidStream);
  exit(1);
}
#endif

/*
 *--------------------------------------------------------------
 *
 * main --
 *
 *	Parses command line, starts decoding and displaying.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

void
main(argc, argv)
     int argc;
     char **argv;
{

  char *name;
  char tmpstr[256];
  int mark=1;
  int i,index;

  input = stdin;
  name = "<stdin>";
  ditherType = ORDERED2_DITHER;

  printf("\n%s -- MPEG Analyzer for MPEG I video streams (version %s)\n\n", 
	 argv[0],VERSION);
  if (argc == 1) {
	name = "<stdin>";
	input = stdin;
      }
  else if (argc==2) {
    if (strcmp(argv[1],"-") == 0) {
      input = stdin;
      name = "<stdin>";
    } else {
      input = fopen(argv[1], "r");
      if (input == NULL) {
	if (strcmp(argv[1],"-?") == 0 ||  strcmp(argv[1],"-help") == 0) {
	    Usage();
	  } else {
	    fprintf(stderr, "Could not open MPEG file %s\n", argv[1]);
	    Usage();
	  }
      }
      name = argv[1];
    }
  } else {
    index = 1;
    while (index<argc) {
      BOOLEAN match;

      match=FALSE;

      if ( strncmp(argv[index], "-block_info",6) == 0 ) {
	match = TRUE;
	if (strcmp(argv[index+1],"-") == 0) {
	  printf("Writing block information to <stdout>\n");
	  block_fp=stdout;
	} else if ((block_fp=fopen(argv[index+1],"w"))==NULL) {
	  fprintf(stderr,"Could not open block info file:  %s\n",argv[index+1]);
	  Usage();
	} else {	  
	  printf("Writing block information to %s\n",argv[index+1]);
	}
	opts ^= BLOCK_INFO;
	index+=2; continue;
      }
      if ( strncmp(argv[index], "-qscale",5) == 0 ) {
	match = TRUE;
	if (strcmp(argv[index+1],"-") == 0) {
	qscale_fp=stdout;
	printf("Writing qscale information to <stdout>\n");
	} else if ((qscale_fp=fopen(argv[index+1],"w"))==NULL) {
	  fprintf(stderr,"Could not open qscale output file:  %s\n",argv[index+1]);
	  Usage();
	}  else {	  
	  printf("Writing qscale information to %s\n",argv[index+1]);
	}
	opts ^= QSCALE_INFO;
	index+=2; continue;
      }
      if ( strncmp(argv[index], "-userdat", 8) == 0 ) {
	match = TRUE;
	if (strcmp(argv[index+1],"-") == 0) {
	userdat_fp = stdout;
	printf("Writing user data information to <stdout>\n");
	} else if ((userdat_fp = fopen(argv[index+1],"w"))==NULL) {
	  fprintf(stderr,"Could not open user data output file:  %s\n",argv[index+1]);
	  Usage();
	}  else {	  
	  printf("Writing user data information to %s\n",argv[index+1]);
	}
	opts ^= USERDAT_INFO;
	index+=2; continue;
      }
      if ( strncmp(argv[index], "-offsets",7) == 0 ) {
	match = TRUE;
	if (strcmp(argv[index+1],"-") == 0) {
	offs_fp=stdout;
	printf("Writing offset information to <stdout>\n");
	} else if ((offs_fp=fopen(argv[index+1],"w"))==NULL) {
	  fprintf(stderr,"Could not open offset output file:  %s\n",argv[index+1]);
	  Usage();
	}  else {	  
	  printf("Writing offset information to %s\n",argv[index+1]);
	}
	opts ^= OFFS_INFO;
	index+=2; continue;
      }
      if ( strncmp(argv[index], "-size",5) == 0 ) {
	match = TRUE;
	if (strcmp(argv[index+1],"-") == 0) {
	  size_fp=stdout;
	  printf("Writing size information to <stdout>\n");
	} else if ((size_fp=fopen(argv[index+1],"w"))==NULL) {
	  fprintf(stderr,"Could not open size output file:  %s\n",argv[index+1]);
	  Usage();
	}  else {	  
	  printf("Writing size information to %s\n",argv[index+1]);
	}
	opts ^= SIZE_INFO;
	index+=2; continue;
      }
      if (strncmp(argv[index],"-ratelength",6) == 0) {
	match = TRUE;
	opts^=RATE_LENGTH_SET; 
	rate_frames = atoi(argv[index+1]);
	if (rate_frames == 0) Usage();
	index+=2; continue;
      }
      if ( strncmp(argv[index], "-rate",5) == 0 ) {
	match = TRUE;
	if (strcmp(argv[index+1],"-") == 0) {
	  rate_fp=stdout;
	  printf("Writing rate information to <stdout>\n");
	} else if ((rate_fp=fopen(argv[index+1],"w"))==NULL) {
	  fprintf(stderr,"Could not open rate output file:  %s\n",argv[index+1]);
	  Usage();
	}  else {	  
	  printf("Writing rate information to %s\n",argv[index+1]);
	}
	opts ^= RATE_INFO;
	index+=2; continue;
      }
      if ( strncmp(argv[index], "-hist",5) == 0 ) {
	match = TRUE;
	if (strcmp(argv[index+1],"-") == 0) {
	  hist_fp=stdout;
	  printf("Writing histogram information to <stdout>\n");
	} else if ((hist_fp=fopen(argv[index+1],"w"))==NULL) {
	  fprintf(stderr,"Could not open histogram output file:  %s\n",argv[index+1]);
	  Usage();
	}  else {	  
	  printf("Writing histogram information to %s\n",argv[index+1]);
	}
	opts ^= HIST_INFO;
	index+=2; continue;
      }
      if ( strncmp(argv[index], "-syslog",7) == 0 ) {
	match = TRUE;
	if (strcmp(argv[index+1],"-") == 0) {
	  syslogOutput=stdout;
	  printf("Writing system layer information to <stdout>\n");
	} else if ((syslogOutput=fopen(argv[index+1],"w"))==NULL) {
	  fprintf(stderr,"Could not open system layer output file:  %s\n",argv[index+1]);
	  Usage();
	}  else {	  
	  printf("Writing system layer information to %s\n",argv[index+1]);
	}
	opts ^= SYSLAYER_LOG;
	index+=2; continue;
      }
      if ( strcmp(argv[index], "-all") == 0 ) {
	match = TRUE;
	if (strcmp(argv[index+1],"-") == 0) {
	  printf("Writing all information to <stdout>\n");
	  printf("Writing qscale information to <stdout>\n");
	  printf("Writing offset information to <stdout>\n");
	  printf("Writing size information to <stdout>\n");
	  printf("Writing rate information to <stdout>\n");
	  printf("Writing histogram information to <stdout>\n");
	  printf("Writing user data information to <stdout>\n");
	  offs_fp = stdout;
	  size_fp = stdout;
	  block_fp = stdout;
	  qscale_fp = stdout;
	  rate_fp = stdout;
	  hist_fp = stdout;
	  userdat_fp = stdout;
	  index++;
	} else {
	  index++;
	  sprintf(tmpstr,"%s.blk",argv[index]);
	  if ((block_fp=fopen(tmpstr,"w")) == NULL) {
	    fprintf(stderr,"Could not open block info file:  %s\n",tmpstr);
	    Usage();
	  } else {	  
	    printf("Writing block information to %s\n",tmpstr);
	  }
	  sprintf(tmpstr,"%s.qs",argv[index]);
	  if ((qscale_fp=fopen(tmpstr,"w")) == NULL) {
	    fprintf(stderr,"Could not open qscale output file:  %s\n",tmpstr);
	    Usage();
	  }  else {	  
	    printf("Writing qscale information to %s\n",tmpstr);
	  }
	  sprintf(tmpstr,"%s.off",argv[index]);
	  if ((offs_fp=fopen(tmpstr,"w"))==NULL) {
	    fprintf(stderr,"Could not open offset output file:  %s\n",tmpstr);
	    Usage();
	  }  else {	  
	    printf("Writing offset information to %s\n",tmpstr);
	  }
	  sprintf(tmpstr,"%s.sz",argv[index]);
	  if ((size_fp=fopen(tmpstr,"w"))==NULL) {
	    fprintf(stderr,"Could not open size output file:  %s\n",tmpstr);
	    Usage();
	  }  else {	  
	    printf("Writing size information to %s\n",tmpstr);
	  }
	  sprintf(tmpstr,"%s.hist",argv[index]);
	  if ((hist_fp=fopen(tmpstr,"w"))==NULL) {
	    fprintf(stderr,"Could not open histogram output file:  %s\n",tmpstr);
	    Usage();
	  }  else {	  
	    printf("Writing histogram information to %s\n",tmpstr);
	  }
	  sprintf(tmpstr,"%s.ud",argv[index]);
	  if ((userdat_fp=fopen(tmpstr,"w"))==NULL) {
	    fprintf(stderr,"Could not open user data output file:  %s\n",tmpstr);
	    Usage();
	  }  else {	  
	    printf("Writing user data information to %s\n",tmpstr);
	  }
	  sprintf(tmpstr,"%s.rt",argv[index]);
	  if ((rate_fp=fopen(tmpstr,"w"))==NULL) {
	    fprintf(stderr,"Could not open rate output file:  %s\n",tmpstr);
	    Usage();
	  }  else {	  
	    printf("Writing rate information to %s\n",tmpstr);
	  }
	}
	index++;
	opts ^= SIZE_INFO;
	opts ^= QSCALE_INFO;
	opts ^= BLOCK_INFO;
	opts ^= OFFS_INFO;
	opts ^= RATE_INFO;
	opts ^= HIST_INFO;
	opts ^= USERDAT_INFO;
	continue;
      }
      
      if (strcmp(argv[index],"-quiet") == 0) {
	match = TRUE;
	opts ^= LOUD;
	index++; continue;
      }
      if (strcmp(argv[index],"-dct") == 0) {
	match = TRUE;
	opts ^= DCT_INFO;
	index++; continue;
      }
      if (strcmp(argv[index],"-aswan") == 0) {
	/* Dont ask */
	match = TRUE;
	opts ^= BITS_INFO;
	index++; continue;
      }
      if (strncmp(argv[index],"-verif",6) == 0) {
	match = TRUE;
	opts ^= VERIFY;
	index++; continue;
      }
      if (strcmp(argv[index],"-time") == 0) {
	match = TRUE;
	opts ^= TIME_MEASURE;
	index++; continue;
      }
      if (strcmp(argv[index],"-start") == 0) {
	match = TRUE;
	opts^=START_F; 
	start_opt = atoi(argv[index+1]);
	if (start_opt == 0) Usage();
	index+=2; continue;
      }
      if (strcmp(argv[index],"-end") == 0) {
	match = TRUE;
	opts ^= END_F; 
	end_opt = atoi(argv[index+1]);
	if (end_opt == 0) Usage();
	index+=2; continue;
      }
      if (index == argc-1) {
	match = TRUE;
	if (strcmp(argv[index],"-") == 0) {
	  input = stdin;
	  name = "<stdin>";
	  index++;
	} else {
	  input = fopen(argv[index], "r");
	  name = argv[index];
	  if (input == NULL) {
	    fprintf(stderr, "Could not open MPEG file:  %s\n", argv[index]);
	    Usage();
	  }
	  index++;
	}} else {
	  input = stdin;
	  name = "<stdin>";
	}
      if (!match) {
	fprintf(stderr,"Invalid argument %s\n",argv[index]);
	Usage();
      }
    }}

  /* Check options for sanity */
  if ((opts&END_F) && (end_opt<start_opt)) Usage();
  if (start_opt<=1) COLLECTING=COLLECT_ON;
  else COLLECTING=COLLECT_OFF;

  if ((RATE_LENGTH_SET&opts) && !(opts&RATE_INFO)){
    fprintf(stderr,"You set ratelength, but no rate file.\n");
    fprintf(stderr,"Tossing rate information, hope thats ok.\n");
    opts^=RATE_INFO;
    rate_fp=fopen("/dev/null","w");
  }

  if ((opts&DCT_INFO) & !(opts&BLOCK_INFO)) {
    fprintf(stderr, "DCT information is collected into Block file\n");
    fprintf(stderr, "So -dct requires -block_info file (or -all file)\n");
    exit(1);
  }

  /* Ok, done parsing, lets go! */
  
  printf("Reading %s\n\n", name);

  if ((opts&START_F) || (opts&END_F)) {
    sprintf(tmpstr,"output from %s (%d to ",
	    name,(start_opt>0)?start_opt:1);
    if (end_opt>0) {sprintf(tmpstr,"%s%d) */\n",strdup(tmpstr),end_opt);}
    else sprintf(tmpstr,"%send */\n",tmpstr);
  } else {sprintf(tmpstr,"output from %s */\n",name);}

  if (opts&SIZE_INFO)   fprintf(size_fp,"/* Size file %s",tmpstr);
  if (opts&QSCALE_INFO) fprintf(qscale_fp,"/* Qscale file %s",tmpstr);
  if (opts&BLOCK_INFO)  fprintf(block_fp,"/* Block file %s",tmpstr);
  if (opts&OFFS_INFO)   fprintf(offs_fp,"/* Offset file %s",tmpstr);
  if (opts&USERDAT_INFO) fprintf(userdat_fp,"/* User data file %s",tmpstr);
  if (opts&RATE_INFO)   fprintf(rate_fp,"/* Rate file %s",tmpstr); /*HI*/
    
  if ((opts&START_F) || (opts&END_F)) {
    printf("Collecting statistics from frame %d to ",(start_opt>0)?start_opt:1);
    if (end_opt>0) {printf("%d.\n",end_opt);} else {printf("end.\n");}
  }
  
  if (opts&SIZE_INFO) fprintf(size_fp,"Num\tType\tSize\n");

  signal(SIGINT, int_handler);

  init_tables();
  
  EOF_flag = 0;
  curBits = 0;
  bitOffset = 0;
  bufLength = 0;
  bitBuffer = NULL;
  totNumFrames = 0;

  theStream = NewVidStream(BUF_LENGTH);
  if (theStream==NULL) {
    fprintf(stderr,"Could not create Video Stream Object!\n");
    exit(1);
  }

  realTimeStart = ReadSysClock();
  mpegVidRsrc(0, theStream);
}
 

/*
 *--------------------------------------------------------------
 *
 * Usage --
 *
 *	Gives message describing options
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */
void
Usage()
{
    fprintf(stderr, "\nUsage:  mpeg_stat [options] [mpeg_file]\n");
    fprintf(stderr, "Options:\n");
    fprintf(stderr, "  -quiet: \t\t Turn off output of frame types/matricies as encountered\n");
    fprintf(stderr, "  -verify: \t\t Do more work to help assure the validity of the stream\n\t\t\t (slows processing somewhat)\n");
    fprintf(stderr, "  -start N: \t\t Begin collection at frame N (first frame is 1)\n");
    fprintf(stderr, "  -end N: \t\t End collection at frame N (end >= start)\n");
    fprintf(stderr, "  -histogram file:\t Put detailed histograms into file\n");
    fprintf(stderr, "  -qscale file: \t Put qscale information into file\n");
    fprintf(stderr, "  -size file: \t\t Write individual frame type and size into file\n");
    fprintf(stderr, "  -offsets file: \t Write high level header offsets into file\n");
    fprintf(stderr, "  -block_info file:\t Put macroblock usage into file\n");
    fprintf(stderr, "  -dct \t\t\t Puts decoded DCT info into block file\n");
    fprintf(stderr, "  -rate file: \t\t Put instantaneous rate information in file\n");
    fprintf(stderr, "  -ratelength N: \t Measure bitrate per N frames, not one second's worth\n");
    fprintf(stderr, "  -syslog file: \t Store parsing of systerm layer into file\n");
    fprintf(stderr, "  -userdata file: \t Store user data information into file\n");
    fprintf(stderr, "  -time: \t\t Measure time to decode frames\n");
    fprintf(stderr, "  -all file: \t\t Put all information into files with basename file\n");
    fprintf(stderr, "\nA single dash (-) may be used to denote standard in/out in place of a filename.\n");
    exit(1);
}
