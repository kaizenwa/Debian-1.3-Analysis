/*
 * MIDI player plug-in for Linux/AWE32
 * Xaw version for drvmidi-0.3.1
 *	ver.0.3 beta4; Jan. 29, 1997
 *
 * Copyright (C) 1996,1997 Takashi Iwai
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <stdio.h>
#include <ctype.h>
#include <stdarg.h>
#include <signal.h>
#include <unistd.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/fcntl.h>
#include <time.h>
#ifndef NO_CONPANEL
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Command.h>
#endif
#include "npapi.h"

/* #define DEBUG_ON */


/*================================================================
 * drvmidi program
 *================================================================*/

#define DRVMIDI		"drvmidi"

#define LOAD_CMD	"LOAD"
#define QUIT_CMD	"ZAPP"
#define STOP_CMD	"QUIT"
#define PAUSE_CMD	"STOP"
#define RESTART_CMD	"RSTA"
#define REPEAT_CMD	"AUTO"
#define FWRD_CMD	"fwrd"
#define BACK_CMD	"back"
#define DRUM_CMD	"DRUM"

#ifdef NO_CONPANEL
#define MIME_DESC	"MIDI Player (no control)"
#else
#define MIME_DESC	"MIDI Player plug-in"
#endif


/*================================================================
 * global variable
 *================================================================*/

typedef struct _MidiDrvCtl {
	int pid;	/* process id */
	int pipe;	/* pipe to communicate */
	struct _PluginInstance *instlist;
} MidiDrvCtl;
	
static MidiDrvCtl drvctl;


/*================================================================
 * instance state information about the plugin.
 *================================================================*/

typedef struct _PluginInstance
{
	NPWindow *fWindow;
	uint16 fMode;

	/* MIDI file name */
	char *curFile;
	char *songName;

	/* window attributes */
	int x, y, width, height;

#ifndef NO_CONPANEL	
	/* widget sets */
	Display *display;
	Widget toplevel, start, stop, pause;
	Pixmap startPix, stopPix, pausePix;
	Pixmap startStPix, stopStPix, pauseStPix;
#endif

	/* flags */
	int loop, autostart;
	int playing;
	int drumflag;

	/* instance list */
	NPP instance;
	struct _PluginInstance *next;
} PluginInstance;


/*----------------------------------------------------------------
 * function prototypes
 *----------------------------------------------------------------*/

static void pipe_printf(const char *fmt, ...);
static void pipe_puts(const char *str);
static int compare(const char *s1, const char *s2);
#ifndef NO_CONPANEL
static void PauseCB(Widget wid, XtPointer closure, XtPointer call_data);
static void StartCB(Widget wid, XtPointer closure, XtPointer call_data);
static void StopCB(Widget wid, XtPointer closure, XtPointer call_data);
#endif
static void ExclusiveStart(PluginInstance *This);
static void StopThis(PluginInstance *This);

#ifdef DEBUG_ON
static void logging(const char *fmt, ...);
#define add_log(X)  logging X
#else
#define add_log(X)	/**/
#endif


/*----------------------------------------------------------------
 * instance linked list
 *----------------------------------------------------------------*/

static void InitList(void)
{
	drvctl.instlist = NULL;
}

static void AppendList(PluginInstance *This)
{
	PluginInstance *cur;
	for (cur = drvctl.instlist; cur; cur = cur->next) {
		if (cur == This)
			return;
	}
	This->next = drvctl.instlist;
	drvctl.instlist = This;
}

static void FreeList(PluginInstance *This)
{
	PluginInstance *cur, *prev;
	prev = NULL;
	for (cur = drvctl.instlist; cur; prev = cur, cur = cur->next) {
		if (cur == This) {
			if (cur == drvctl.instlist)
				drvctl.instlist = cur->next;
			else
				prev->next = cur->next;
			break;
		}
	}
}


/*================================================================
 * implementations of plugin API functions
 *================================================================*/

char*
NPP_GetMIMEDescription(void)
{
	add_log(("get mime desc"));
	return "audio/midi:mid:" MIME_DESC ";"
		"audio/x-midi:midi:" MIME_DESC ";"
		"music/crescendo:midi:" MIME_DESC;
}

NPError
NPP_GetValue(void *future, NPPVariable variable, void *value)
{
	NPError err = NPERR_NO_ERROR;

	add_log(("midiplg: get value %d", variable));
	switch (variable) {
	case NPPVpluginNameString:
		*((char **)value) = MIME_DESC;
		break;
	case NPPVpluginDescriptionString:
		*((char **)value) =
		"This is a plugin to play embedded MIDI samples";
		break;
	default:
		err = NPERR_GENERIC_ERROR;
	}
	return err;
}


#define MAX_OPEN_RETRY		5

NPError
NPP_Initialize(void)
{
	int drv_pipe[2];
	int res;
	int i;

	add_log(("initialize"));
	for (i = 0; i < MAX_OPEN_RETRY; i++) {
		int fd;
		if ((fd = open("/dev/sequencer", O_WRONLY)) != -1) {
			close(fd);
			break;
		}
		sleep(1);
	}

	res = pipe(drv_pipe);
	if (res)
		return NPERR_FILE_NOT_FOUND;

	if ((drvctl.pid = fork()) == 0) {
		/* child: redirect data to stdin */
		close(drv_pipe[1]); 
		dup2(drv_pipe[0], fileno(stdin));
		close(drv_pipe[0]);
		execlp(DRVMIDI, DRVMIDI, "-ip", NULL);
		/* Won't come back from here */
		fprintf(stderr, "WARNING: come back from drvmidi\n");
		exit(1);
	}

	add_log(("fork = %d", drvctl.pid));
    
	close(drv_pipe[0]);
	drvctl.pipe = drv_pipe[1];

	InitList();

	return NPERR_NO_ERROR;
}


jref
NPP_GetJavaClass()
{
	/* doesn't use Java */
	add_log(("midiplg: get java class"));
	return NULL;
}

void
NPP_Shutdown(void)
{
	add_log(("shutdown %d", drvctl.pid));
	if (drvctl.pid) {
		pipe_puts(QUIT_CMD);
		usleep(200000);
		close(drvctl.pipe);
		/*kill(drvctl.pid, SIGTERM);*/
		drvctl.pid = 0;
		drvctl.pipe = 0;
	}
	add_log(("shutdown done", drvctl.pid));
}


/* new instance */
NPError 
NPP_New(NPMIMEType pluginType,
	NPP instance,
	uint16 mode,
	int16 argc,
	char* argn[],
	char* argv[],
	NPSavedData* saved)
{
	PluginInstance* This;
	int i;
	
	add_log(("new"));
	if (instance == NULL)
		return NPERR_INVALID_INSTANCE_ERROR;
		
	instance->pdata = NPN_MemAlloc(sizeof(PluginInstance));
	
	This = (PluginInstance*) instance->pdata;
	
	if (This == NULL)
		return NPERR_OUT_OF_MEMORY_ERROR;

	AppendList(This);

	This->fWindow = NULL;
	This->fMode = mode;
	This->playing = FALSE;
	This->curFile = NULL;
	This->songName = NULL;

	This->loop = TRUE;
	This->autostart = TRUE;
	This->drumflag = (1 << 9);
	for (i = 0; i < argc; i++) {
		add_log(("--parm %s = %s", argn[i], argv[i]));
		if (compare(argn[i], "loop") == 0) {
			if (compare(argv[i], "no") == 0 ||
			    compare(argv[i], "false") == 0)
				This->loop = FALSE;
		} else if (compare(argn[i], "autostart") == 0) {
			if (compare(argv[i], "no") == 0 ||
			    compare(argv[i], "false") == 0)
				This->autostart = FALSE;
		} else if (compare(argn[i], "drumflag") == 0) {
			This->drumflag = (int)strtol(argv[i], NULL, 16);
		} else if (compare(argn[i], "song") == 0) {
			This->songName = (char*)NPN_MemAlloc(strlen(argv[i]) + 1);
			if (This->songName)
				strcpy(This->songName, argv[i]);
		}
	}

#ifndef NO_CONPANEL
	This->startPix = This->stopPix = This->pausePix = 0;
	This->startStPix = This->stopStPix = This->pauseStPix = 0;
	This->start = This->pause = This->stop = NULL;
#endif

	if (This->songName) {
		NPN_GetURL(instance, This->songName, NULL);
	}
	return NPERR_NO_ERROR;
}


NPError 
NPP_Destroy(NPP instance, NPSavedData** save)
{
	PluginInstance* This;

	add_log(("destroy"));
	if (instance == NULL)
		return NPERR_INVALID_INSTANCE_ERROR;

	This = (PluginInstance*) instance->pdata;

	if (This != NULL) {
		add_log(("destroy instance [%x]", This));
#ifndef NO_CONPANEL
		/* remove pixmaps */
		XFreePixmap(This->display, This->startPix);
		XFreePixmap(This->display, This->stopPix);
		XFreePixmap(This->display, This->pausePix);
		XFreePixmap(This->display, This->startStPix);
		XFreePixmap(This->display, This->stopStPix);
		XFreePixmap(This->display, This->pauseStPix);
#endif
		FreeList(This);
		if (This->playing)
			pipe_puts(STOP_CMD);
		if (This->curFile) {
			NPN_MemFree(This->curFile);
		}
		if (This->songName) {
			NPN_MemFree(This->songName);
		}
		NPN_MemFree(instance->pdata);
		instance->pdata = NULL;
	}
	return NPERR_NO_ERROR;
}


#ifndef NO_CONPANEL
static void PauseCB(Widget wid, XtPointer closure, XtPointer call_data)
{
	PluginInstance *This = (PluginInstance*)closure;
	if (This == NULL) return;
	if (This->playing) {
		pipe_puts(PAUSE_CMD);
	}
}


static void StartCB(Widget wid, XtPointer closure, XtPointer call_data)
{
	PluginInstance *This = (PluginInstance*)closure;
	if (This == NULL) return;
	if (!This->playing && This->curFile) {
		ExclusiveStart(This);
	}
}


static void StopCB(Widget wid, XtPointer closure, XtPointer call_data)
{
	PluginInstance *This = (PluginInstance*)closure;
	if (This == NULL) return;
	if (This->playing)
		StopThis(This);
}

static void DestroyCB(Widget wid, XtPointer closure, XtPointer call_data)
{
	add_log(("destroy callback: [%x]", closure));
}


#include "play.xbm"
#include "play_s.xbm"
#include "stop.xbm"
#include "stop_s.xbm"
#include "pause.xbm"
#include "pause_s.xbm"

#endif  /* !NO_CONPANEL */

NPError 
NPP_SetWindow(NPP instance, NPWindow* window)
{
	PluginInstance* This;
	NPSetWindowCallbackStruct *wsinfo;
#ifndef NO_CONPANEL
	Widget test;
#endif

	add_log(("set window"));
	if (instance == NULL)
		return NPERR_INVALID_INSTANCE_ERROR;

	This = (PluginInstance*) instance->pdata;
	This->fWindow = window;

	if (window == NULL || window->window == NULL) {
		add_log(("--> clear win"));
		return NPERR_NO_ERROR;
	}

	This->x = window->x; /* position in netscape page */
	This->y = window->y;
	This->width = window->width; /* maximum width */
	This->height = window->height; /* height */
	add_log(("--> instance [%x]", This));
	add_log(("--> new (%d,%d) %d x %d", This->x, This->y, This->width, This->height));
	if (This->width <= 0 || This->height <= 0 ||
	    window->ws_info == NULL)
		return NPERR_NO_ERROR;
			
	wsinfo = window->ws_info;

#ifndef NO_CONPANEL
	This->display = wsinfo->display;
	This->toplevel = XtWindowToWidget(This->display, (Window)window->window);
	test = XtNameToWidget(This->toplevel, "start");
	if (test == NULL) {
		add_log(("--> add widget : toplevel = %d", This->toplevel));
		This->startPix = XCreateBitmapFromData
			(This->display, (Window)window->window,
			 play_bits, play_width, play_height);
		This->stopPix = XCreateBitmapFromData
			(This->display, (Window)window->window,
			 stop_bits, stop_width, stop_height);
		This->pausePix = XCreateBitmapFromData
			(This->display, (Window)window->window,
			 pause_bits, pause_width, pause_height);
		This->startStPix = XCreateBitmapFromData
			(This->display, (Window)window->window,
			 play_s_bits, play_s_width, play_s_height);
		This->stopStPix = XCreateBitmapFromData
			(This->display, (Window)window->window,
			 stop_s_bits, stop_s_width, stop_s_height);
		This->pauseStPix = XCreateBitmapFromData
			(This->display, (Window)window->window,
			 pause_s_bits, pause_s_width, pause_s_height);

		This->start = XtVaCreateManagedWidget
			("start", commandWidgetClass, This->toplevel,
			 XtNx, 0, XtNy, 0,
			 XtNlabel, "",
			 XtNleftBitmap, This->startPix,
			 NULL, NULL);
		add_log(("start button is %x", This->start));
		This->stop = XtVaCreateManagedWidget
			("stop", commandWidgetClass, This->toplevel,
			 XtNx, play_width + 16, XtNy, 0,
			 XtNlabel, "",
			 XtNleftBitmap, This->stopStPix,
			 NULL, NULL);
		This->pause = XtVaCreateManagedWidget
			("pause", commandWidgetClass, This->toplevel,
			 XtNx, (play_width + stop_width + 32), XtNy, 0,
			 XtNlabel, "",
			 XtNleftBitmap, This->pauseStPix,
			 NULL, NULL);
		XtAddCallback(This->start, XtNcallback, StartCB, This);
		XtAddCallback(This->stop, XtNcallback, StopCB, This);
		XtAddCallback(This->pause, XtNcallback, PauseCB, This);
		XtAddCallback(This->start, XtNdestroyCallback, DestroyCB, This);
		XtSetSensitive(This->start, TRUE);
		XtSetSensitive(This->stop, FALSE);
		XtSetSensitive(This->pause, FALSE);
	}
#endif  /* !NO_CONPANEL */

	return NPERR_NO_ERROR;
}

NPError 
NPP_NewStream(NPP instance,
	      NPMIMEType type,
	      NPStream *stream, 
	      NPBool seekable,
	      uint16 *stype)
{
	add_log(("new stream"));
	*stype = NP_ASFILEONLY;
	return NPERR_NO_ERROR;
}


/* PLUGIN DEVELOPERS:
 */

int32 STREAMBUFSIZE = 0X0FFFFFFF; /* If we are reading from a file in NPAsFile
				   * mode so we can take any size stream in our
				   * write call (since we ignore it) */

int32 
NPP_WriteReady(NPP instance, NPStream *stream)
{
	add_log(("write ready"));
	/* we use stream as file */
	return STREAMBUFSIZE;
}


int32 
NPP_Write(NPP instance, NPStream *stream, int32 offset, int32 len, void *buffer)
{
	add_log(("write"));
	return len;		/* The number of bytes accepted */
}


NPError 
NPP_DestroyStream(NPP instance, NPStream *stream, NPError reason)
{
	add_log(("destroy stream"));
	return NPERR_NO_ERROR;
}


void 
NPP_StreamAsFile(NPP instance, NPStream *stream, const char* fname)
{
	PluginInstance* This;

	add_log(("stream as file"));
	if (instance == NULL)
		return;

	This = (PluginInstance*) instance->pdata;
	if (This->curFile)
		NPN_MemFree(This->curFile);
	This->curFile = (char*)NPN_MemAlloc(strlen(fname) + 1);
	if (This->curFile == NULL)
		return;

	strcpy(This->curFile, fname);
	if (This->autostart)
		ExclusiveStart(This);
}

static void StopThis(PluginInstance *This)
{
	pipe_puts(STOP_CMD);
	This->playing = FALSE;
#ifndef NO_CONPANEL
	XtSetSensitive(This->start, TRUE);
	XtVaSetValues(This->start,
		      XtNleftBitmap, This->startPix,
		      NULL, NULL);
	XtSetSensitive(This->stop, FALSE);
	XtVaSetValues(This->stop,
		      XtNleftBitmap, This->stopStPix,
		      NULL, NULL);
	XtSetSensitive(This->pause, FALSE);
	XtVaSetValues(This->pause,
		      XtNleftBitmap, This->pauseStPix,
		      NULL, NULL);
#endif
	add_log(("- stop %s", This->curFile));
}


static void ExclusiveStart(PluginInstance *This)
{
	PluginInstance *cur;
	add_log(("- exclusive start file %s", This->curFile));
	for (cur = drvctl.instlist; cur; cur = cur->next) {
		if (cur->playing)
			StopThis(cur);
	}
	pipe_puts(STOP_CMD);
	pipe_printf("%s %d", REPEAT_CMD, This->loop);
	pipe_printf("%s %d", DRUM_CMD, This->drumflag);
	pipe_puts(LOAD_CMD);
	pipe_puts(This->curFile);
	This->playing = TRUE;
#ifndef NO_CONPANEL
	XtSetSensitive(This->start, FALSE);
	XtVaSetValues(This->start,
		      XtNleftBitmap, This->startStPix,
		      NULL, NULL);
	XtSetSensitive(This->stop, TRUE);
	XtVaSetValues(This->stop,
		      XtNleftBitmap, This->stopPix,
		      NULL, NULL);
	XtSetSensitive(This->pause, TRUE);
	XtVaSetValues(This->pause,
		      XtNleftBitmap, This->pausePix,
		      NULL, NULL);
#endif
}

void 
NPP_Print(NPP instance, NPPrint* printInfo)
{
	add_log(("print"));
	if(printInfo == NULL)
		return;

	if (instance != NULL) {
		if (printInfo->mode == NP_FULL) {
			/* Do the default*/
			printInfo->print.fullPrint.pluginPrinted = FALSE;
		}
		else {	/* If not fullscreen, we must be embedded */
		}
	}
}

/*================================================================
 * pipe communication
 *================================================================*/

static void pipe_printf(const char *fmt, ...)
{
	char buf[256];
	va_list ap;
	va_start(ap, fmt);
	vsprintf(buf, fmt, ap);
	pipe_puts(buf);
	va_end(ap);
}

static void pipe_puts(const char *str)
{
	int len;
	char lf = '\n';
	if (drvctl.pid == 0) return;
	len = strlen(str);
	write(drvctl.pipe, str, len);
	write(drvctl.pipe, &lf, 1);
}

static int compare(const char *s1, const char *s2)
{
	for (; *s1 && *s2; s1++, s2++) {
		if (tolower(*s1) != tolower(*s2))
			return 1;
	}
	if (*s1 == 0 && *s2 == 0)
		return 0;
	return 1;
}

/*----------------------------------------------------------------
 * logging
 *----------------------------------------------------------------*/

#ifdef DEBUG_ON

static FILE *logfp;	/* log file */
#define LOGFILE "/tmp/midiplg.log"

static void log_init(void)
{
	if (logfp == NULL) {
		logfp = fopen(LOGFILE, "a");
		logging("----------------");
	}
}

static void logging(const char *fmt, ...)
{
	va_list ap;
	time_t curt;
	va_start(ap, fmt);
	log_init();
	curt = time(&curt);
	fprintf(logfp, "%s ", ctime(&curt));
	vfprintf(logfp, fmt, ap);
	fprintf(logfp, "\n");
	fflush(logfp);
	va_end(ap);
}

#endif
