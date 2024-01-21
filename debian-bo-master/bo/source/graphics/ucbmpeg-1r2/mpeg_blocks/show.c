#define PROMPT  "grabsh: "

#include "tcl.h"
#include "tk.h"
#include "show.h"
#include "dither.h"
#include "video.h"


#include <sys/time.h>
#include <string.h>
#include <sys/stat.h>

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
int movieOpen = FALSE;
int numBytes = 0;
double	stopStartTime;
int justShowOne = FALSE;
float percentPlay = 1.0;
char	program[1024];

extern FILE *input;
extern VidStream *theStream;

void PlayLoop();
void ContinuePlayLoop();
extern double ReadSysClock();


static    Window  winID;
static    Display *winDisplay;
static    GC	    gc;
int	globalCommand = NO_COMMAND;
float realFPS;


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


int RewindC(nulldata, interp, argc, argv)
ClientData nulldata;
Tcl_Interp *interp;
int argc;
char **argv;
{
    int	height, width;

    if ( inPlayLoop )
    {
	globalCommand = REWIND_COMMAND;
    }
    else
    {
	if ( movieOpen )
	    fclose(input);

	/* need to play one frame */
	playframe(title, IMAGE_LEFT, IMAGE_TOP, &width, &height, TRUE);

	stopStartTime = ReadSysClock();
    }

    return TCL_OK;
}


int StopC(nulldata, interp, argc, argv)
ClientData nulldata;
Tcl_Interp *interp;
int argc;
char **argv;
{
    if ( inPlayLoop )
    {
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

    if ( inPlayLoop )
    {
	globalCommand = PLAY_COMMAND;
    }
    else
    {
	if ( movieOpen )	    /* restart playing */
	    ContinuePlayLoop();
	else			    /* start over */
	    playframe(title, IMAGE_LEFT, IMAGE_TOP, &width, &height, FALSE);
    }

    return TCL_OK;
}

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

  sprintf(command, "update ideltasks");
  Tcl_Eval(interp, command);

  return(TCL_OK);
}


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


/************************************************************************/

int main(argc, argv)
    int argc;
    char **argv;
{
    char *args, *p, *msg;
    char buf[20];
    char command[256];
    int result;
    Tk_3DBorder border;
    struct stat stbuf;
    extern int ditherType;
    char    *ditherName;
    int width, height;
    char show_tcl[256];

    /* Version */
    printf("\nmpeg_blocks (version 1.0)\n\n");

    strcpy(program, argv[0]);
    interp = Tcl_CreateInterp();

#ifdef TCL_MEM_DEBUG
    Tcl_InitMemory(interp);
#endif

    if ( argc == 3 ) {
      ditherName = argv[2];
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
      } else if (strcmp(ditherName, "ordered") == 0) {
	ditherType = ORDERED_DITHER;
      } else if (strcmp(ditherName, "ordered2") == 0) {
	ditherType = ORDERED2_DITHER;
      } else if (strcmp(ditherName, "mbordered") == 0) {
	ditherType = MBORDERED_DITHER;
      } else {
	perror("Illegal dither option.");
	usage(argv[0]);
      }
    } else if ( argc == 2 ) {
      ditherType = ORDERED2_DITHER;
      ditherName = "ordered2";
    } else {
      usage(NULL);	
      exit(1);
    }

    strcpy(title, argv[1]);

    argv[1] = "-f";
    printf("%s/show.tcl", BLOCKS_LIB);
    sprintf(show_tcl, "%s/show.tcl", BLOCKS_LIB);
    argv[2] = show_tcl;
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
    Tcl_SetVar (interp, "prompt", PROMPT, TCL_GLOBAL_ONLY);

    {
      char bitdir[256];
      sprintf(bitdir, "%s/bitmaps", BLOCKS_LIB);
      Tcl_SetVar(interp, "bitmapDir", bitdir, TCL_GLOBAL_ONLY);
    }
    if (synchronize) {
	XSynchronize(Tk_Display(w), True);
    }
    Tk_GeometryRequest(w, 200, 200);
    border = Tk_Get3DBorder(interp, w, None, "#4eee94");
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

    Tcl_CreateCommand (interp, "NextFrame", NextFrameC, (ClientData) 0,
	(void (*) ()) NULL);

    if (geometry != NULL) {
	Tcl_SetVar(interp, "geometry", geometry, TCL_GLOBAL_ONLY);
    }

    if (fileName != NULL) {
	result = Tcl_VarEval(interp, "source ", fileName, (char *) NULL);
	if (result != TCL_OK) {
	    goto error;
	}
	tty = 0;
    } else {
	tty = isatty(0);
    }

    fflush(stdout);

    sprintf(command, "source %s/tk.tcl", BLOCKS_LIB);
    Tcl_Eval(interp, command);
    sprintf(command, "source %s/button.tcl", BLOCKS_LIB);
    Tcl_Eval(interp, command);
    sprintf(command, "source %s/tkerror.tcl", BLOCKS_LIB);  
    Tcl_Eval(interp, command);  

    Tcl_Eval(interp, "update");

    winID = Tk_WindowId(w);
    winDisplay = Tk_Display(w);
    gc = XCreateGC(winDisplay, winID, 0, 0);
    SetUpPlayFrame(winDisplay, gc, winID);

    if ( title[0] == '/' )
	sprintf(command, "wm title . \"%s (%s)\"", strrchr(title, '/')+1, ditherName);
    else
	sprintf(command, "wm title . \"%s (%s)\"", title, ditherName);
    Tcl_Eval(interp, command);
    Tcl_Eval(interp, "update");

/* stat the file */
    stat(title, &stbuf);
    numBytes = stbuf.st_size; 

    globalCommand = STOP_COMMAND;
    playframe(title, IMAGE_LEFT, IMAGE_TOP, &width, &height, TRUE);

    sprintf(command, "set Mpeg(width) %d", theStream->h_size);
    Tcl_Eval(interp, command);
    sprintf(command, "set Mpeg(height) %d", theStream->v_size);
    Tcl_Eval(interp, command);
    {
      int i;

      sprintf(command, ".infowin.mvFrame.mvCanvas configure -width %d -height %d",
	      theStream->mb_width * 24 + 2, theStream->mb_height * 24 + 2);
      Tcl_Eval(interp, command);

      for (i = 0; i < theStream->mb_height * theStream->mb_width; i++) {
	sprintf(command, "set FMV(%d) \"\"", i);
	Tcl_Eval(interp, command);
	sprintf(command, "set BMV(%d) \"\"", i);
	Tcl_Eval(interp, command);
      }

    }


    Tk_MainLoop();

    Tcl_DeleteInterp(interp);

    exit(0);

error:
    msg = Tcl_GetVar(interp, "errorInfo", TCL_GLOBAL_ONLY);
    if (msg == NULL) {
	msg = interp->result;
    }
    fprintf(stderr, "%s\n", msg);

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
	      case NEXT_COMMAND:
		stillPlaying = FALSE;
		break;
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
	      case NEXT_COMMAND:
		stillPlaying = FALSE;
		break;
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
