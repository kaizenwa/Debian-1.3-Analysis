/*
 * Copyright (c) 1995 The Regents of the University of California.
 * All rights reserved.
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose, without fee, and without written agreement is
 * hereby granted, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */
#include "video.h"
#include "proto.h"
#include <sys/types.h>
#include <signal.h>
#include <math.h>
#include "util.h"
#include "dither.h"
#include "tcl.h"
#include "tk.h"
#include "show.h"


/* Define buffer length. */

#define BUF_LENGTH 80000

/* Function return type declarations */
void usage();

/* External declaration of main decoding call. */

extern VidStream *mpegVidRsrc();
extern VidStream *NewVidStream();

/* Declaration of global variable to hold dither info. */

int ditherType;

/* Global file pointer to incoming data. */
FILE *input = NULL;

/* End of File flag. */
static int EOF_flag = 0;

/* Loop flag. */
int loopFlag = 0;

/* Shared memory flag. */
int shmemFlag = 0;

/* Quiet flag. */
int quietFlag = 0;

/* Minimum time to display a frame. */
double displayTime = 500.0;

/* Display image on screen? */
int noDisplayFlag = 0;

/* Setjmp/Longjmp env. */
jmp_buf env;

int bytesRead;
int oldBytesRead = 0;
double	stoppedTime;
int numSkipped = 0;

extern int movieOpen;

extern double chrom_mult;
extern double chrom_add;
extern double lum_mult;
extern double lum_add;
extern double atof();

VidStream *theStream = NULL;

void	ContinuePlay();


/*
 *--------------------------------------------------------------
 *
 * int_handler --
 *
 *	Handles Cntl-C interupts..
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
int_handler()
{
  if (!quietFlag) {
    fprintf(stderr, "Interrupted!\n");
  }
  if (curVidStream != NULL)
    DestroyVidStream(&curVidStream);
  exit(1);
}


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
SetUpPlayFrame(browserDisplay, browserGC, browserWin)
    Display *browserDisplay;
    GC browserGC;
    Window browserWin;
{
  char *name;

  name = "";
  LUM_RANGE = 8;
  CR_RANGE = CB_RANGE = 4;
  noDisplayFlag = 0;
  chrom_mult = 1.0;
  chrom_add = 0.0;
  lum_mult = 1.0;
  lum_add = 0.0;
#ifdef SH_MEM
  shmemFlag = 1;
#endif

  lum_values = (int *) malloc(LUM_RANGE*sizeof(int));
  cr_values = (int *) malloc(CR_RANGE*sizeof(int));
  cb_values = (int *) malloc(CB_RANGE*sizeof(int));

  signal(SIGINT, int_handler);

  init_tables();

  switch (ditherType) {

  case HYBRID_DITHER:

    InitColor();
    InitHybridDither();
    FindGC(browserGC);
    FindWindow(browserWin);
    display = browserDisplay;
    InitDisplay(name);
    break;
    
    case HYBRID2_DITHER:
    InitColor();
    InitHybridErrorDither();
    FindGC(browserGC);
    FindWindow(browserWin);
    display = browserDisplay;
    InitDisplay(name);
    break;
    
  case FS4_DITHER:
    InitColor();
    InitFS4Dither();
    FindGC(browserGC);
    FindWindow(browserWin);
    display = browserDisplay;
    InitDisplay(name);
    break;
    
  case FS2_DITHER:
    InitColor();
    InitFS2Dither();
    FindGC(browserGC);
    FindWindow(browserWin);
    display = browserDisplay;
    InitDisplay(name);
    break;
    
  case FS2FAST_DITHER:
    InitColor();
    InitFS2FastDither();
    FindGC(browserGC);
    FindWindow(browserWin);
    display = browserDisplay;
    InitDisplay(name);
    break;
    
  case Twox2_DITHER:
    InitColor();
    Init2x2Dither();
    FindGC(browserGC);
    FindWindow(browserWin);
    display = browserDisplay;
    InitDisplay(name);
    PostInit2x2Dither();
    break;

  case RGBL_DITHER:
    InitRGBLColor();
    InitRGBLDisplay(name);
    InitRGBLDither();
    break;

  case GRAY_DITHER:
    InitGrayDisplay(name);
    break;

  case FULL_COLOR_DITHER:
    InitColorDither();
    InitColorDisplay(name);
    break;

  case NO_DITHER:
    shmemFlag = 0;
    break;

  case ORDERED_DITHER:
    InitColor();
    InitOrderedDither();
    FindGC(browserGC);
    FindWindow(browserWin);
    display = browserDisplay;
    InitDisplay(name);
    break;

  case MONO_DITHER:
  case MONO_THRESHOLD:
  case MONO_FS4_DITHER:
  case HALFTONE_DITHER:
    InitMonoDisplay(name);
    break;

  case ORDERED2_DITHER:
    InitColor();
    FindGC(browserGC);
    FindWindow(browserWin);
    display = browserDisplay;
    InitDisplay(name);
    InitOrdered2Dither();
    break;

  case BIG_ORDERED_DITHER:
    InitColor();
    FindGC(browserGC);
    FindWindow(browserWin);
    display = browserDisplay;
    InitDisplay(name);
    InitBigOrderedDither();
    break;

  case MBORDERED_DITHER:
    InitColor();
    FindGC(browserGC);
    FindWindow(browserWin);
    display = browserDisplay;
    InitDisplay(name);
    InitMBOrderedDither();
    break;

  default:
    fprintf(stdout, "Error:  incorrect dither type: %d\n",
	    ditherType);
    ditherType = ORDERED2_DITHER;
    break;
  }

#ifdef SH_MEM
    if (shmemFlag && (display != NULL)) {
      if (!XShmQueryExtension(display)) {
	shmemFlag = 0;
	if (!quietFlag) {
	  fprintf(stderr, "Shared memory not supported\n");
	  fprintf(stderr, "Reverting to normal Xlib.\n");
	}
      }
    }
#endif

    
}


XImage *
playframe(movie, x, y, width, height, justFirst)
    char *movie;
    int x;
    int y;
    int *width;
    int *height;
    int justFirst;
{
    extern XImage *GetImage _ANSI_ARGS_((void));
    extern int numSkipped;
  int i;
    char someText[256];

    SetFirstFrame();
    SetDisplayPosition(x, y);
    ShowStatusOutline();

  input = fopen(movie, "r");
  if (input == NULL) {
    sprintf(someText, "Could not open file %s", movie);
    HandleMPEGError(someText);
  }

    movieOpen = TRUE;
    bytesRead = 0;
    numSkipped = 0;

    EOF_flag = 0;
    curBits = 0;
    bitOffset = 0;
    bufLength = 0;
    bitBuffer = NULL;
    totNumFrames = 0;
#ifdef ANALYSIS 
    init_stats();
#endif
    init_clock();
    stoppedTime = 0.0;

    if ( theStream != NULL )
	DestroyVidStream(&theStream);

    if ( theStream == NULL )
	theStream = NewVidStream(BUF_LENGTH);

  mpegVidRsrc(0, theStream);

    *width = curVidStream->h_size;
    *height = curVidStream->v_size;

    ContinuePlay(justFirst);

    return GetImage();
}


void	ContinuePlay(justFirst)
    int justFirst;
{
    extern int globalCommand;
    extern int	justShowOne;
    int lastNumFrames;

    if ( displayTime == 0.0 )
	lastNumFrames = totNumFrames;

    if ( (totNumFrames == 1) && justFirst )
	return;

  while ( mpegVidRsrc(0, theStream) != NULL )
  {

	if ( (totNumFrames == 1) && justFirst )
	    return;

	if ( justShowOne && (totNumFrames != lastNumFrames) )
	    return;

	/* handle any TK events */
	if ( ! justFirst )
	{
	    while ( Tk_DoOneEvent(TK_X_EVENTS | TK_TIMER_EVENTS |
				  TK_DONT_WAIT | TK_IDLE_EVENTS) != 0 )
	    {
		extern int globalCommand;

		if ( tk_NumMainWindows == 0 )
		    exit(0);

		switch(globalCommand)
		{
		    case REWIND_COMMAND:
			fclose(input);
			movieOpen = FALSE;
			return;
		    case STOP_COMMAND:
			return;
		    case PLAY_COMMAND:
			return;
		      case NEXT_COMMAND:
			return;
		    default:
			break;
		}
	    }
	}
  }

    oldBytesRead = bytesRead;
    fclose(input);
    input = NULL;
    movieOpen = FALSE;
    if ( loopFlag )
	globalCommand = REWIND_COMMAND;
}



/*
 *--------------------------------------------------------------
 *
 * usage --
 *
 *	Print mpeg_show usage
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	exits with a return value -1
 *
 *--------------------------------------------------------------
 */

void
usage(s)
char *s;	/* program name */
{
    fprintf(stderr, "Usage:\n");
    fprintf(stderr, "mpeg_blocks mpegfile [dither]\n");
    fprintf(stderr, "\tdither\t[ordered|ordered2|mbordered|fs4|fs2|fs2fast|hybrid|\n");
    fprintf(stderr, "                    hybrid2]\n");
    exit (-1);
}



/*
 *--------------------------------------------------------------
 *
 * DoDitherImage --
 *
 *	Called when image needs to be dithered. Selects correct
 *      dither routine based on info in ditherType.
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
DoDitherImage(l, Cr, Cb, disp, h, w) 
unsigned char *l, *Cr, *Cb, *disp;
int h,w;
{
    static int counter = 0;
    char    fileName[256];
    FILE *filePtr;
    register int index;

  switch(ditherType) {
  case HYBRID_DITHER:
    HybridDitherImage(l, Cr, Cb, disp, h, w);
    break;

  case HYBRID2_DITHER:
    HybridErrorDitherImage(l, Cr, Cb, disp, h, w);
    break;

  case FS2FAST_DITHER:
    FS2FastDitherImage(l, Cr, Cb, disp, h, w);
    break;

  case FS2_DITHER:
    FS2DitherImage(l, Cr, Cb, disp, h, w);
    break;

  case FS4_DITHER:
    FS4DitherImage(l, Cr, Cb, disp, h, w);
    break;

  case Twox2_DITHER:
    Twox2DitherImage(l, Cr, Cb, disp, h, w);
    break;

  case BIG_ORDERED_DITHER:
    BigOrderedDitherImage(l, Cr, Cb, disp, h, w);
    break;

  case FULL_COLOR_DITHER:
    ColorDitherImage(l, Cr, Cb, disp, h, w);
    break;

  case GRAY_DITHER:
    GrayDitherImage(l, Cr, Cb, disp, h, w);
    break;

  case NO_DITHER:
    break;

  case ORDERED_DITHER:
    OrderedDitherImage(l, Cr, Cb, disp, h, w);
    break;

  case MONO_DITHER:
    MonoDitherImage(l, Cr, Cb, disp, h, w);
    break;

  case MONO_THRESHOLD:
    MonoThresholdImage(l, Cr, Cb, disp, h, w);
    break;

  case MONO_FS4_DITHER:
    MonoFS4DitherImage(l, Cr, Cb, disp, h, w);
    break;

  case HALFTONE_DITHER:
    HalftoneDitherImage(l, Cr, Cb, disp, h, w);
    break;

  case ORDERED2_DITHER:
    Ordered2DitherImage(l, Cr, Cb, disp, h, w);
    break;

  case MBORDERED_DITHER:
    MBOrderedDitherImage(l, Cr, Cb, disp, h, w);
    break;

  case RGBL_DITHER:
    RGBLDitherImage(l, Cr, Cb, disp, h, w);
    break;
  }
}

