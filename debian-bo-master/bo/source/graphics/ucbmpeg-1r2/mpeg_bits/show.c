#define PROMPT  "grabsh: "

#include "tcl.h"
#include "tk.h"
#include "show.h"
#include "dither.h"
#include "video.h"


#include <sys/time.h>
#include <string.h>
#include <sys/stat.h>
#include <assert.h>

#define TRUE	1
#define FALSE	0


/**********************************************************************/

/*
 * Command used to initialize wish:
 */

/* was wish.tcl */
char initCmd[] = "source $tk_library/tk.tcl";

Tk_Window mywin;
Tk_Window w;			/* NULL means window has been deleted. */
Tk_TimerToken timeToken = 0;
int idleHandler = 0;
Tcl_Interp *interp;
int x, y;
int inPlayLoop = FALSE;
char title[512];
jmp_buf env2;
char ditherName[512];
int movieOpen = FALSE;
int numBytes = 0;
double	stopStartTime;
int justShowOne = FALSE;
float percentPlay = 1.0;
char	program[1024];
int firstTimePlayed = 1;


extern FILE *input;
extern VidStream *theStream;
extern int justRewound;

void PlayLoop();
void ContinuePlayLoop();
extern double ReadSysClock();


static    Window  winID;
static    Display *winDisplay;
static    GC	    gc;
int	globalCommand = NO_COMMAND;
float realFPS;


#ifdef BLEAH
Tcl_CmdBuf buffer;
#endif

int tty;
extern int Tk_SquareCmd _ANSI_ARGS_((ClientData clientData,
	Tcl_Interp *interp, int argc, char **argv));



/*
 * Information for testing out command-line options:
 */

int synchronize = 0;
char *fileName = NULL;
char *name = NULL;
char *displayName = NULL;
char *geometry = NULL;

Tk_ArgvInfo argTable[] = {
    {"-file", TK_ARGV_STRING, (char *) NULL, (char *) &fileName,
	"File from which to read commands"},
    {"-geometry", TK_ARGV_STRING, (char *) NULL, (char *) &geometry,
	"Initial geometry for window"},
    {"-display", TK_ARGV_STRING, (char *) NULL, (char *) &displayName,
	"Display to use"},
    {"-name", TK_ARGV_STRING, (char *) NULL, (char *) &name,
	"Name to use for application"},
    {"-sync", TK_ARGV_CONSTANT, (char *) 1, (char *) &synchronize,
	"Use synchronous mode for display server"},
    {(char *) NULL, TK_ARGV_END, (char *) NULL, (char *) NULL,
	(char *) NULL}
};

#ifdef BLEAH
void StdinProc(clientData, mask)
    ClientData clientData;		/* Not used. */
    int mask;
{
    char line[200];
    static int gotPartial = 0;
    char *cmd;
    int result;

    if (mask & TK_READABLE) {
	if (fgets(line, 200, stdin) == NULL) {
	    if (!gotPartial) {
		if (tty) {
		    Tcl_Eval(interp, "destroy .", 0, (char **) NULL);
		    exit(0);
		} else {
		    Tk_DeleteFileHandler(0);
		}
		return;
	    } else {
		line[0] = 0;
	    }
	}
	cmd = Tcl_AssembleCmd(buffer, line);
	if (cmd == NULL) {
	    gotPartial = 1;
	    return;
	}
	gotPartial = 0;
	result = Tcl_RecordAndEval(interp, cmd, 0);
	if (*interp->result != 0) {
	    if ((result != TCL_OK) || (tty)) {
		printf("%s\n", interp->result);
	    }
	}
	if (tty) {
	    printf(Tcl_GetVar (interp, "prompt", TCL_GLOBAL_ONLY));
	    fflush(stdout);
	}
    }
}
#endif


static void StructureProc(clientData, eventPtr)
    ClientData clientData;	/* Information about window. */
    XEvent *eventPtr;		/* Information about event. */
{
    if (eventPtr->type == DestroyNotify) {
	w = NULL;
    }
}


int OrigSizeC(nulldata, interp, argc, argv)
ClientData nulldata;
Tcl_Interp *interp;
int argc;
char **argv;
{
    char command[256];
    extern int origWidth, origHeight;
    extern int windowWidth, windowHeight;
    extern int oldBytesRead;

    windowWidth = origWidth+2*IMAGE_LEFT;
    if ( windowWidth < 205 )
	windowWidth = 205;
    windowHeight = origHeight+IMAGE_TOP+STATUS_HEIGHT+
		    2*STATUS_BORDER+30;

    sprintf(command, "wm geometry . %dx%d", windowWidth, windowHeight);
    Tcl_Eval(interp, command);

    ResizeWindow();
    Tcl_Eval(interp, "update idletasks");
    InitXImage();
    UpdateStatusBar(oldBytesRead);
    ShowFrameInfo();

    if ( ! inPlayLoop )
	RefreshCurrentImage();

    return TCL_OK;
}


int DoubleSizeC(nulldata, interp, argc, argv)
ClientData nulldata;
Tcl_Interp *interp;
int argc;
char **argv;
{
    char command[256];
    extern int origWidth, origHeight;
    extern int windowWidth, windowHeight;
    extern int oldBytesRead;
    extern int ditherType;

#ifdef BLEAH
    ditherType = Twox2_DITHER;
    SetUpPlayFrame();
#endif

    windowWidth = 2*origWidth+2*IMAGE_LEFT;
    if ( windowWidth < 205 )
	windowWidth = 205;
    windowHeight = 2*origHeight+IMAGE_TOP+STATUS_HEIGHT+
		    2*STATUS_BORDER+30;

    sprintf(command, "wm geometry . %dx%d", windowWidth, windowHeight);
    Tcl_Eval(interp, command);

    ResizeWindow();
    Tcl_Eval(interp, "update idletasks");
    InitXImage();
    UpdateStatusBar(oldBytesRead);
    ShowFrameInfo();

    if ( ! inPlayLoop )
	RefreshCurrentImage();

    return TCL_OK;
}


int ToggleLoopC(nulldata, interp, argc, argv)
ClientData nulldata;
Tcl_Interp *interp;
int argc;
char **argv;
{
    extern int loopFlag;

    if ( argv[1][0] == '1' )
	loopFlag = TRUE;
    else
	loopFlag = FALSE;

    return TCL_OK;
}



int SetTitleC (nulldata, interp, argc, argv)
ClientData nulldata;
Tcl_Interp *interp;
int argc;
char **argv;
{

  char command[256];
  int  height, width;
  extern int firstTimePlayed;
  extern int first;
  int i;

  firstTimePlayed = 1;
  first = 1;
  
  assert( argc == 2 );
  strcpy( title, argv[1]);
  if ( title[0] == '/' )
    sprintf(command, "wm title . \"%s (%s)\"", strrchr(title, '/')+1, ditherName);
  else
    sprintf(command, "wm title . \"%s (%s)\"", title, ditherName);
  Tcl_Eval(interp, command);
  Tcl_Eval(interp, "update");
  Tcl_Eval(interp, "ClearState");
  
  displayTime = 0.0;
  resetPlayWindow();
  
  if (i = setjmp(env2)) {
    if ( i == 2 ) {
      Tcl_Eval( interp, "AnnounceError \"Unexpected read error\"" );
    } else if (i == 3) {
      Tcl_Eval( interp, 
	       "AnnounceError \"Improper or missing sequence end code.\"");
    }
    if (theStream != NULL) {
      DestroyVidStream( &theStream );
      theStream = (VidStream *)NULL;
    }
    Tcl_Eval( interp, "set videoLoaded 0" );
    return TCL_OK;
  }
  
  playframe(title, IMAGE_LEFT, IMAGE_TOP, &width, &height, TRUE);
    
  sprintf(command, "changeWorkDir %s", title);
  Tcl_Eval(interp, command);

  sprintf(command, "set mpegWidth %d", theStream->h_size);
  Tcl_Eval(interp, command);
  sprintf(command, "set mpegHeight %d", theStream->v_size);
  Tcl_Eval(interp, command);
  {
    int num_h_mb, num_v_mb;
    
    num_h_mb = theStream->h_size / 16;
    if ((theStream->h_size % 16) != 0)
      num_h_mb++;
    
    num_v_mb = theStream->v_size / 16;
    if ((theStream->v_size % 16) != 0) 
      num_v_mb++;
    
    sprintf(command, ".infowin.mvFrame.mvCanvas configure -width %d -height %d",
	    num_h_mb * 24 + 2, 
	    num_v_mb * 24 + 2);
    Tcl_Eval(interp, command);
  }
  return TCL_OK;
}  

int RewindC(nulldata, interp, argc, argv)
     ClientData nulldata;
     Tcl_Interp *interp;
     int argc;
     char **argv;
{
  int height, width;
  
  /* If there's previous frames to restore, do it and play one frame */
  if (restoreVidStream( theStream ))
    {
      /* Clear the gauges */
      Tcl_Eval( interp, "ClearGauges\n");
      
      /* Play one frame */
      Tcl_Eval( interp, "NextFrame\n" );
    }

  justRewound = TRUE;
  
  return TCL_OK;
}


int StopC(nulldata, interp, argc, argv)
ClientData nulldata;
Tcl_Interp *interp;
int argc;
char **argv;
{
  if ( inPlayLoop ) {
    globalCommand = STOP_COMMAND;
  }
  /* otherwise, ignore */
  return TCL_OK;
}


int PlayPauseC(nulldata, interp, argc, argv)
ClientData nulldata;
Tcl_Interp *interp;
int argc;
char **argv;
{
  int	    width, height;
  extern double stoppedTime;
  int stillPlaying = TRUE;
  
  inPlayLoop = TRUE;
  stoppedTime += (ReadSysClock()-stopStartTime);
  
  globalCommand = PLAY_COMMAND;
  ContinuePlay(FALSE);
  
  globalCommand = STOP_COMMAND;

  inPlayLoop = FALSE;
  stopStartTime = ReadSysClock();
  
  justRewound = FALSE;

  return TCL_OK;
}


#ifndef NO_CHANGE
int NextFrameC(nulldata, interp, argc, argv)
     ClientData nulldata;
     Tcl_Interp *interp;
     int argc;
     char **argv;
{
  extern double stoppedTime;
  extern int totNumFrames;
  int width, height, lastNumFrames;
  char command[256];

  if ( inPlayLoop ) {
    globalCommand = NEXT_COMMAND;
  } else {
    if ( movieOpen ) {
      stoppedTime += (ReadSysClock() - stopStartTime);
      lastNumFrames = totNumFrames;
      while ( mpegVidRsrc(0, theStream) ) {
	if (lastNumFrames != totNumFrames) 
	    break;
      }
      stopStartTime = ReadSysClock();
    }
  }

  sprintf(command, "update idletasks");
  Tcl_Eval(interp, command);

  justRewound = FALSE;

  return(TCL_OK);
}
#endif

int SetFPSC(nulldata, interp, argc, argv)
ClientData nulldata;
Tcl_Interp *interp;
int argc;
char **argv;
{
    extern double displayTime;
    int	    newFPS;

    newFPS = atoi(argv[1]);
    if ( newFPS == 0 )
    {
	displayTime = 0.0;
	justShowOne = TRUE;
    }
    else
    {
	displayTime = 1.0/(float)newFPS;
	justShowOne = FALSE;
    }

    realFPS = (float)newFPS;

    return TCL_OK;
}


int SetPercentC(nulldata, interp, argc, argv)
ClientData nulldata;
Tcl_Interp *interp;
int argc;
char **argv;
{
    int	    newPercent;

    newPercent = atoi(argv[1]);
    percentPlay = (float)newPercent*0.01;

    return TCL_OK;
}



int NewShow(nulldata, interp, argc, argv)
ClientData nulldata;
Tcl_Interp *interp;
int argc;
char **argv;
{
    char    command[512];

    sprintf(command, "%s %s %s &", program, title, argv[1]);
    system(command);

    return TCL_OK;
}


static void BrowserRedrawHandler(clientData, eventPtr)
    ClientData clientData;	/* Information about window. */
    XEvent *eventPtr;		/* Information about event. */
{
    extern int oldBytesRead;
    extern int windowReady;
    extern int windowHeight, windowWidth;
    extern int handlingError;
    XExposeEvent *exposeEvent;
    int	height, width;

    if ( handlingError )
	return;

    exposeEvent = &(eventPtr->xexpose);

    /* should really keep track of redraw events and do at once */

    if ( exposeEvent->count != 0 )
	return;

    if ( ! windowReady )
	return;

    height = Tk_Height(w);
    width = Tk_Width(w);

    if ( (height != windowHeight) || (width != windowWidth) )
    {
	windowHeight = height;
	windowWidth = width;

	ResizeWindow();
	Tcl_Eval(interp, "update idletasks");
	
	InitXImage();
    }
    else
    {
	/* reprint Frame: and status bar */
	ShowFPS();
	ShowStatusOutline();
    }

    UpdateStatusBar(oldBytesRead);
    ShowFrameInfo();

    if ( ! inPlayLoop )
	RefreshCurrentImage();
}


/*
 * Procedure to map initial window.  This is invoked as a do-when-idle
 * handler.  Wait for all other when-idle handlers to be processed
 * before mapping the window, so that the window's correct geometry
 * has been determined.
 */

static void DelayedMap(clientData)
    ClientData clientData;	/* Not used. */
{

    while (Tk_DoOneEvent(1) != 0) {
	/* Empty loop body. */
    }
    if (w == NULL) {
	return;
    }
    Tk_MapWindow(w);
}


static void 
db_usage() {
  fprintf (stderr, "\n\nUsage:\n");
  fprintf (stderr, "mpeg_bits ?-dither <dither name>? ?<filename>?\n");
  fprintf (stderr, "<dither name> is any of the following:\n");
  fprintf (stderr, "\thybrid, hybrid2, fs4, fs2, fs2fast, ordered, ordered2, mbordered, mono, threshold.\n");
  fprintf (stderr, "<filename> is the name of an MPEG file.\n");
  fprintf (stderr, "No filename allows the user to select a file through\n");
  fprintf (stderr, "the file dialog, and no dither name defaults to ordered2.\n");
  exit(0);
}

/************************************************************************/

int main(argc, argv)
    int argc;
     char **argv;
{
  char *args, *p, *msg;
  char buf[20];
  char command[256];
  char editsFileName[256];
  int result;
  Tk_3DBorder border;
  struct stat stbuf;
  extern int ditherType;
  int width, height;
  int i, ditherGiven, titleGiven, editsFileGiven;
  
  strcpy(program, argv[0]);
  
  printf("\nmpeg_bits (version 1.0b)\n\n");

  /*  tclInit(); */
  
  interp = Tcl_CreateInterp();
  
#ifdef TCL_MEM_DEBUG
  Tcl_InitMemory(interp);
#endif
  
  ditherGiven = 0;
  titleGiven = 0;
  editsFileGiven = 0;
  for ( i = 1; i < argc; i++) {
    if (!strcmp( argv[i], "-edits")) {
      if (++i < argc) {
	editsFileGiven = 1;
	strcpy( editsFileName, argv[i]);
      } else {
	fprintf( stderr, "Must include edits file name when using the -edits flag.");
	db_usage();
      } 
    } else if (!strcmp( argv[i], "-dither")) {
      if ( ++i < argc ) {
	ditherGiven = 1;
	strcpy( ditherName, argv[i]);
	if (strcmp(ditherName, "hybrid") == 0) {
	  ditherType = HYBRID_DITHER;
	} else if (strcmp(ditherName, "hybrid2") == 0) {
	  ditherType = HYBRID2_DITHER;
	} else if (strcmp(ditherName, "fs4") == 0) {
	  ditherType = FS4_DITHER;
	} else if (strcmp(ditherName, "fs2") == 0) {
	  ditherType = FS2_DITHER;
	} else if (strcmp(ditherName, "fs2fast") == 0) {
	  ditherType = FS2FAST_DITHER;
#ifdef NO_CHANGE
	} else if (strcmp(ditherName, "gray") == 0) {
	  ditherType = GRAY_DITHER;
	} else if (strcmp(ditherName, "2x2") == 0) {
	  ditherType = Twox2_DITHER;
	} else if (strcmp(ditherName, "color") == 0) {
	  ditherType = FULL_COLOR_DITHER;
	} else if (strcmp(ditherName, "none") == 0) {
	  ditherType = NO_DITHER;
#endif
	} else if (strcmp(ditherName, "ordered") == 0) {
	  ditherType = ORDERED_DITHER;
	} else if (strcmp(ditherName, "ordered2") == 0) {
	  ditherType = ORDERED2_DITHER;
	} else if (strcmp(ditherName, "mbordered") == 0) {
	  ditherType = MBORDERED_DITHER;
#ifdef NO_CHANGE
	} else if (strcmp(ditherName, "mono") == 0) {
	  ditherType = MONO_DITHER;
	} else if (strcmp(ditherName, "threshold") == 0) {
	  ditherType = MONO_THRESHOLD;
#endif
	} else {
	  fprintf( stderr, "Illegal dither option.");
	  db_usage();
	}
      } else {
	fprintf( stderr, "Must include dither selection when using -dither flag.");
	db_usage();
      }
    } else if ( i = argc-1 ) {
      titleGiven = 1;
      strcpy(title, argv[i]);
    } else {
      fprintf( stderr, "Bad argument: %s.", argv[i]);
      db_usage();
    }
  }
  
  if (!titleGiven && editsFileGiven) {
    printf("Edits file %s being ignored because no stream title was supplied\n", editsFileName);
    editsFileGiven = 0;
  }
  

  if (!ditherGiven) {
    ditherType = ORDERED2_DITHER;
    sprintf(ditherName, "ordered2");
  }
  
  if (titleGiven) {
    if ( stat( title, &stbuf ) == -1 ) {
      printf( "Can't find file \"%s\": starting program with no video loaded\n", 
	     title);
      sprintf( title, "*no video loaded*");;
      titleGiven = 0;
    }
  } else {
    sprintf( title, "*no video loaded*");;
  }
  
  argv[1] = "-f";
  {
    char *show_tcl = (char *) malloc((unsigned int) 100);
    sprintf(show_tcl, "%s/show.tcl", BITS_DIR);
    argv[2] = show_tcl;
  }
  argc = 3;
  
  if (Tk_ParseArgv(interp, (Tk_Window) NULL, &argc, argv, argTable, 0)
      != TCL_OK) {
    fprintf(stderr, "%s\n", interp->result);
    exit(1);
  }
  if (name == NULL) {
    if (fileName != NULL) {
      p = fileName;
    } else {
      p = argv[0];
    }
    name = strrchr(p, '/');
    if (name != NULL) {
      name++;
    } else {
      name = p;
    }
  }
  
  if ( (w = Tk_CreateMainWindow(interp, displayName, name, "")) == NULL )
    {
      fprintf(stderr, "%s\n", interp->result);
      exit(1);
    }
  
  Tk_SetClass(w, "Tk");
  
  Tk_CreateEventHandler(w, StructureNotifyMask, StructureProc,
			(ClientData) NULL);
  Tk_DoWhenIdle(DelayedMap, (ClientData) NULL);
  
  Tk_CreateEventHandler(w, /* mask */ ExposureMask,
			BrowserRedrawHandler, (ClientData) NULL);
  
  tty = isatty(0);
  Tcl_SetVar(interp, "tcl_interactive", "0", TCL_GLOBAL_ONLY);
  
  if (Tcl_AppInit(interp) != TCL_OK) {
    fprintf(stderr, "Tcl_AppInit failed: %s\n", interp->result);
  }
  
  args = Tcl_Merge(argc-1, argv+1);
  Tcl_SetVar(interp, "argv", args, TCL_GLOBAL_ONLY);
  ckfree(args);
  sprintf(buf, "%d", argc-1);
  Tcl_SetVar(interp, "argc", buf, TCL_GLOBAL_ONLY);
  Tcl_SetVar(interp, "prompt", PROMPT, TCL_GLOBAL_ONLY);
  
  if (synchronize) {
    XSynchronize(Tk_Display(w), True);
  }
  Tk_GeometryRequest(w, 200, 200);
#if (TK_MAJOR_VERSION>=4)
  border = NULL; /* Someone running 4.0 can fix this... */
#else
  border = Tk_Get3DBorder(interp, w, None, "#4eee94");
#endif
  if (border == NULL) {
    Tcl_SetResult(interp, (char *) NULL, TCL_STATIC);
    Tk_SetWindowBackground(w, WhitePixelOfScreen(Tk_Screen(w)));
  } else {
    Tk_SetBackgroundFromBorder(w, border);
  }
  XSetForeground(Tk_Display(w), DefaultGCOfScreen(Tk_Screen(w)),
		 BlackPixelOfScreen(Tk_Screen(w)));
  
  
  Tcl_CreateCommand (interp, "OrigSize", OrigSizeC, (ClientData) 0,
		     (void (*) ()) NULL);
  
  Tcl_CreateCommand (interp, "DoubleSize", DoubleSizeC, (ClientData) 0,
		     (void (*) ()) NULL);
  
  Tcl_CreateCommand (interp, "ToggleLoop", ToggleLoopC, (ClientData) 0,
		     (void (*) ()) NULL);
  
  Tcl_CreateCommand (interp, "SetTitle", SetTitleC, (ClientData) 0,
		     (void (*) ()) NULL);
  
  Tcl_CreateCommand (interp, "Rewind", RewindC, (ClientData) 0,
		     (void (*) ()) NULL);
  
  Tcl_CreateCommand (interp, "Stop", StopC, (ClientData) 0,
		     (void (*) ()) NULL);
  
  Tcl_CreateCommand (interp, "PlayPause", PlayPauseC, (ClientData) 0,
		     (void (*) ()) NULL);
  
  Tcl_CreateCommand (interp, "SetFPS", SetFPSC, (ClientData) 0,
		     (void (*) ()) NULL);
  
  Tcl_CreateCommand (interp, "SetPercent", SetPercentC, (ClientData) 0,
		     (void (*) ()) NULL);
  
  Tcl_CreateCommand (interp, "newshow", NewShow, (ClientData) 0,
		     (void (*) ()) NULL);
  
#ifndef NO_CHANGE
  Tcl_CreateCommand (interp, "NextFrame", NextFrameC, (ClientData) 0,
		     (void (*) ()) NULL);
#endif
  
  if (geometry != NULL) {
    Tcl_SetVar(interp, "geometry", geometry, TCL_GLOBAL_ONLY);
  }
  
#ifdef BLEAH
  result = Tcl_Eval(interp, "source $tk_library/menu.tcl");
  if (result != TCL_OK) {
    goto error;
  }
  
  result = Tcl_Eval(interp, initCmd);
  if (result != TCL_OK) {
    goto error;
  }
#endif
  
  if (fileName != NULL) {
    result = Tcl_VarEval(interp, "source ", fileName, (char *) NULL);
    if (result != TCL_OK) {
      goto error;
    }
    tty = 0;
  } else {
    tty = isatty(0);
#ifdef BLEAH
    Tk_CreateFileHandler(0, TK_READABLE, StdinProc, (ClientData) 0);
    if (tty) {
      printf(Tcl_GetVar (interp, "prompt", TCL_GLOBAL_ONLY));
    }
#endif
  }
  
  fflush(stdout);
  
#ifdef BLEAH
  buffer = Tcl_CreateCmdBuf();
#endif
  
#ifdef LINUX
  (void) Tcl_Eval(interp, "source /var/X11/lib/X11/tk/tk.tcl");
  (void) Tcl_Eval(interp, "source /var/X11/lib/X11/tk/button.tcl");
  (void) Tcl_Eval(interp, "source /var/X11/lib/X11/tk/tkerror.tcl");
#else
  (void) Tcl_Eval(interp, "source /usr/cluster/lib/tk/tk.tcl");
  (void) Tcl_Eval(interp, "source /usr/cluster/lib/tk/button.tcl");
  (void) Tcl_Eval(interp, "source /usr/cluster/lib/tk/tkerror.tcl");
#endif
  
  sprintf(command, "set libDir %s\n", BITS_DIR);
  Tcl_Eval(interp, command);
  Tcl_Eval(interp, "Init_Dirs\n");
  Tcl_Eval(interp, "Init_Win\n");
  (void) Tcl_Eval(interp, "update");
  
  winID = Tk_WindowId(w);
  winDisplay = Tk_Display(w);
  gc = XCreateGC(winDisplay, winID, 0, 0);
  SetUpPlayFrame(winDisplay, gc, winID);
  
  if ( title[0] == '/' )
    sprintf(command, "wm title . \"%s (%s)\"", 
	    strrchr(title, '/')+1, 
	    ditherName);
  else
    sprintf(command, "wm title . \"%s (%s)\"", 
	    title, 
	    ditherName);
  Tcl_Eval(interp, command);
  Tcl_Eval(interp, "update");
  
  globalCommand = STOP_COMMAND;
  

  if (i = setjmp(env2)) {
    if ( i == 2 ) {
      Tcl_Eval( interp, "AnnounceError \"Unexpected read error\"" );
    } else if (i == 3) {
      Tcl_Eval( interp, 
	       "AnnounceError \"Improper or missing sequence end code.\"");
    }
    if (theStream != NULL) {
      DestroyVidStream( &theStream );
      theStream = (VidStream *)NULL;
    }
    titleGiven = 0;
  }
  


  if (titleGiven) {
    playframe(title, IMAGE_LEFT, IMAGE_TOP, &width, &height, TRUE);
    sprintf( command, "changeWorkDir %s", title );
    Tcl_Eval(interp, command);
    
    sprintf(command, "set mpegWidth %d", theStream->h_size);
    Tcl_Eval(interp, command);
    sprintf(command, "set mpegHeight %d", theStream->v_size);
    Tcl_Eval(interp, command);
    {
      int num_h_mb, num_v_mb;
      
      num_h_mb = theStream->h_size / 16;
      if ((theStream->h_size % 16) != 0)
	num_h_mb++;
      
      num_v_mb = theStream->v_size / 16;
      if ((theStream->v_size % 16) != 0) 
	num_v_mb++;
      sprintf(command, 
	      ".infowin.mvFrame.mvCanvas configure -width %d -height %d",
	      num_h_mb * 24 + 2, 
	      num_v_mb * 24 + 2);
      Tcl_Eval(interp, command);
      if (editsFileGiven) {
	sprintf( command, "loadEditList [string trim %s]\n", editsFileName);
	Tcl_Eval(interp, command);
      }
    }
  } else {
    Tcl_Eval( interp, "noVideoDisable" );
    Tcl_Eval( interp, "set videoLoaded 0" );
  }
  
  Tk_MainLoop();
  
  Tcl_DeleteInterp(interp);
  
#ifdef BLEAH
  Tcl_DeleteCmdBuf(buffer);
#endif
  
  exit(0);
  
 error:
  msg = Tcl_GetVar(interp, "errorInfo", TCL_GLOBAL_ONLY);
  if (msg == NULL) {
    msg = interp->result;
  }
  fprintf(stderr, "%s\n", msg);
  
#ifdef BLEAH
  Tcl_Eval(interp, "destroy .", 0, (char **) NULL);
#endif
  
  exit(1);
  return 0;
}


void PlayLoop()
{
  int	stillPlaying = TRUE;
  int	height, width;
  
  inPlayLoop = TRUE;
  
  while ( stillPlaying )
    {
      playframe(title, IMAGE_LEFT, IMAGE_TOP, &width, &height, FALSE);
      switch(globalCommand)
	{
	case NO_COMMAND:
	  stillPlaying = FALSE;
	  break;
	case REWIND_COMMAND:    /* continue playing */
	  break;
	case STOP_COMMAND:
	  fclose(input);
	  playframe(title, IMAGE_LEFT, IMAGE_TOP, &width, &height, TRUE);
	  stillPlaying = FALSE;
	  break;
	case PLAY_COMMAND:
	  stillPlaying = FALSE;
	  break;
#ifndef NO_CHANGE
	case NEXT_COMMAND:
	  stillPlaying = FALSE;
	  break;
#endif
	}
      globalCommand = NO_COMMAND;
    }
  
  inPlayLoop = FALSE;
  
  stopStartTime = ReadSysClock();
}


void ContinuePlayLoop()
{
  extern double stoppedTime;
  int	stillPlaying = TRUE;
  int	    height, width;
  
  inPlayLoop = TRUE;
  stoppedTime += (ReadSysClock()-stopStartTime);
  
  while ( stillPlaying )
    {
      ContinuePlay(FALSE);
      switch(globalCommand)
	{
	case NO_COMMAND:
	  stillPlaying = FALSE;
	  break;
	case REWIND_COMMAND:    /* continue playing */
	  stillPlaying = FALSE;
	  PlayLoop();
	  break;
	case STOP_COMMAND:
	  fclose(input);
	  playframe(title, IMAGE_LEFT, IMAGE_TOP, &width, &height, TRUE);
	  stillPlaying = FALSE;
	  break;
	case PLAY_COMMAND:
	  stillPlaying = FALSE;
	  break;
#ifndef NO_CHANGE
	case NEXT_COMMAND:
	  stillPlaying = FALSE;
	  break;
#endif
	}
      globalCommand = NO_COMMAND;
    }
  
  inPlayLoop = FALSE;
  
  stopStartTime = ReadSysClock();
}


/*
 *----------------------------------------------------------------------
 *
 * Tcl_AppInit --
 *
 *	This procedure performs application-specific initialization.
 *	Most applications, especially those that incorporate additional
 *	packages, will have their own version of this procedure.
 *
 * Results:
 *	Returns a standard Tcl completion code, and leaves an error
 *	message in interp->result if an error occurs.
 *
 * Side effects:
 *	Depends on the startup script.
 *
 *----------------------------------------------------------------------
 */

int
Tcl_AppInit(interp)
    Tcl_Interp *interp;
{
    Tk_Window main;

    main = Tk_MainWindow(interp);
    if (main == NULL) {
	return TCL_ERROR;
    }

    /*
     * Call the init procedures for included packages.  Each call should
     * look like this:
     *
     * if (Mod_Init(interp) == TCL_ERROR) {
     *     return TCL_ERROR;
     * }
     *
     * where "Mod" is the name of the module.
     */

    if (Tcl_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    if (Tk_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }

    return TCL_OK;
}				/* Tcl_AppInit */

















