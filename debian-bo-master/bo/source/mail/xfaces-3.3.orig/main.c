/*                             -*- Mode: C++-C -*- 
 * 
 *		 Copyright 1994 Christopher B. Liebman
 *
 *     Permission to use, copy, modify, distribute, and sell this software
 *     and its documentation for any purpose is hereby granted without fee,
 *     provided that the above copyright notice appear in all copies and that
 *     both that copyright notice and this permission notice appear in
 *     supporting documentation, and that the name Christopher B. Liebman not
 *     be used in advertising or publicity pertaining to distribution of this
 *     software without specific, written prior permission.
 *
 *    THIS SOFTWARE IS PROVIDED `AS-IS'.  CHRISTOPHER B. LIEBMAN, DISCLAIMS
 *    ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 *    LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 *    PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL CHRISTOPHER
 *    B. LIEBMAN, BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING SPECIAL,
 *    INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA, OR
 *    PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 *    WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF
 *    OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author          : Chris Liebman
 * Created On      : Tue Jan 11 14:11:30 1994
 * Last Modified By: Chris Liebman
 * Last Modified On: Mon Mar  7 16:56:37 1994
 * Update Count    : 130
 * Status          : Released
 * 
 * HISTORY
 * 13-Feb-1994		Chris Liebman	
 *    Last Modified: Sun Feb 13 00:40:08 1994 #111 (Chris Liebman)
 *    Added new command support and support to annotate mail items.
 *
 * 8-Feb-1994		Chris Liebman	
 *    Last Modified: Sat Feb  5 22:03:01 1994 #82 (Chris Liebman)
 *    Added new command line options.  Added new Tiled widget.
 *
 * 2-Feb-1994		Chris Liebman	
 *    Last Modified: Tue Feb  1 14:41:17 1994 #72 (Chris Liebman)
 *    Added annotation support.
 *
 * 31-Jan-1994		Chris Liebman	
 *    Last Modified: Sun Jan 30 02:05:43 1994 #67 (Chris Liebman)
 *    New search code.
 *    Command line options.
 *    New resources.
 *    List Commad support.
 *
 * 24-Jan-1994		Chris Liebman	
 *    Last Modified: Sun Jan 23 18:46:43 1994 #18 (Chris Liebman)
 *    Added new bindings.
 *
 * 20-Jan-1994		Chris Liebman	
 *    Last Modified: Tue Jan 18 17:14:31 1994 #9 (Chris Liebman)
 *    Added new closeness resource and some cleanups.
 *
 * 14-Jan-1994		Chris Liebman	
 *    Last Modified: Fri Jan 14 09:59:56 1994 #4 (Chris Liebman)
 *    Added ignoreMessageBindings resource.
 * 
 * PURPOSE
 * 	This is xfaces initialization.
*/

#ifndef lint
static char *RCSid = "$Id: main.c,v 1.29 1994/03/08 02:19:48 liebman Exp $";
#endif

#include "faces.h"
#include <pwd.h>
#ifdef __STDC__
#include <stdlib.h>
#endif

/*
 * The resources.
*/

FacesResourcesRec TheFacesResources;

/*
 * This is ugly but it will remove some error messages when
 * some options are not compiled in.
*/

#ifdef SHAPE
#ifdef XPM
#define	IMAGE_TYPES	"xpm-shape:xpm:xbm-shape:xbm"
#else /* XPM*/
#define	IMAGE_TYPES	"xbm-shape:xbm"
#endif /* XPM */
#else /* SHAPE */
#ifdef XPM
#define	IMAGE_TYPES	"xpm:xbm"
#else /* XPM*/
#define	IMAGE_TYPES	"xbm"
#endif /* XPM*/
#endif /* SHAPE */

#ifdef XFACE
#define	IMAGE_SEARCH	"beforeImage\nresource\nfacedb\nx-face\nafterImage"
#else /* XFACE */
#define	IMAGE_SEARCH	"beforeImage\nresource\nfacedb\nafterImage"
#endif /* XFACE */
#define	SOUND_SEARCH	"beforeSound\nresource\nfacedb\nafterSound"
#define	COMMAND_SEARCH	"beforeCommand\nresource\nafterCommand"

/*
 * Here is a description of our resources for the toolkit.
*/

static Boolean defFalse = False;
static Boolean defTrue = True;
#define XtNvolume "volume"		/* Int: volume for bell */
#define XtCVolume "Volume"

#define	offset(field)	XtOffset(FacesResources, field)
static XtResource FacesResourcesList[] =
{
    {
	"spoolFile", "SpoolFile", XtRString, sizeof(String),
	offset(spool_file), XtRString, NULL
    },
    {
	"spoolDir", "SpoolDir", XtRString, sizeof(String),
	offset(spool_dir), XtRString, "/usr/spool/mail"
    },
    {
	"popHost", "PopHost", XtRString, sizeof(String),
	offset(pop_host), XtRString, NULL
    },
    {
	"popAuthFile", "PopAuthFile", XtRString, sizeof(String),
	offset(pop_auth_file), XtRString, NULL
    },
    {
	"popPort", "PopPort", XtRInt, sizeof(int),
	offset(pop_port), XtRString, "110"
    },
    {
	"listCommand", "Command", XtRString, sizeof(String),
	offset(list_command), XtRString, NULL
    },
    {
	"imagePath", "Path", XtRString, sizeof(String),
	offset(image_path), XtRString, "/usr/images"
    },
    {
	"soundPath", "Path", XtRString, sizeof(String),
	offset(sound_path), XtRString, "/usr/sounds"
    },
    {
	"facedbPath", "Path", XtRString, sizeof(String),
	offset(facedb_path), XtRString, "/usr/local/faces"
    },
    {
	"machine", "Path", XtRString, sizeof(String),
	offset(machine), XtRString, "machine.tab"
    },
    {
	"people", "Path", XtRString, sizeof(String),
	offset(people), XtRString, "people.tab"
    },
    {
	"update", "Update", XtRInt, sizeof(int),
	offset(update), XtRString, "60"
    },
    {
        XtNvolume, XtCVolume, XtRInt, sizeof(int),
	offset(volume), XtRString, "33"
    },
    {
	"fromField",  "FromField", XtRString, sizeof(String),
	offset(from_field), XtRString, "From "
    },
    {
	"noMailImage", "Image", XtRString, sizeof(String),
	offset(no_mail_image), XtRString, "nomail"
    },
    {
	"noMailSound", "Sound", XtRString, sizeof(String),
	offset(no_mail_sound), XtRString, NULL
    },
    {
	"keepOrder", "KeepOrder", XtRBoolean, sizeof(Boolean),
	offset(keep_order), XtRBoolean, (caddr_t) &defFalse
    },
    {
	"compressImages", "CompressImages", XtRBoolean, sizeof(Boolean),
	offset(compress_images), XtRBoolean, (caddr_t) &defTrue
    },
    {
	"useSound", "UseSound", XtRBoolean, sizeof(Boolean),
	offset(use_sound), XtRBoolean, (caddr_t) &defTrue
    },
    {
	"useShape", "UseShape", XtRBoolean, sizeof(Boolean),
	offset(use_shape), XtRBoolean, (caddr_t) &defTrue
    },
    {
	"useCommands", "UseCommands", XtRBoolean, sizeof(Boolean),
	offset(use_commands), XtRBoolean, (caddr_t) &defFalse
    },
    {
	"useContentLength", "UseContentLength", XtRBoolean, sizeof(Boolean),
	offset(use_content_length), XtRBoolean, (caddr_t) &defFalse
    },
    {
	"shapeBorders", "ShapeBorders", XtRBoolean, sizeof(Boolean),
	offset(shape_borders), XtRBoolean, (caddr_t) &defTrue
    },
    {
	"shapeInternal", "ShapeInternal", XtRBoolean, sizeof(Boolean),
	offset(shape_internal), XtRBoolean, (caddr_t) &defTrue
    },
    {
	"closeness", "Closeness", XtRInt, sizeof(int),
	offset(closeness), XtRString, (caddr_t) "40000"
    },
    {
	"imageTypes", "ImageTypes", XtRString, sizeof(String),
	offset(image_types_str), XtRString, IMAGE_TYPES
    },
    {
	"imageSearch", "Search", XtRString, sizeof(String),
	offset(image_search_str), XtRString, IMAGE_SEARCH
    },
    {
	"soundSearch", "Search", XtRString, sizeof(String),
	offset(sound_search_str), XtRString, SOUND_SEARCH
    },
    {
	"commandSearch", "Search", XtRString, sizeof(String),
	offset(command_search_str), XtRString, COMMAND_SEARCH
    },
    {
	"ignoreMessageBindings", "IgnoreBindings", XtRString, sizeof(String),
	offset(ignore_message_bindings_str), XtRString, NULL
    },
    {
	"beforeImageBindings", "ImageBindings", XtRString, sizeof(String),
	offset(before_image_bindings_str), XtRString, NULL
    },
    {
	"afterImageBindings", "ImageBindings", XtRString, sizeof(String),
	offset(after_image_bindings_str), XtRString, NULL
    },
    {
	"beforeSoundBindings", "SoundBindings", XtRString, sizeof(String),
	offset(before_sound_bindings_str), XtRString, NULL
    },
    {
	"afterSoundBindings", "SoundBindings", XtRString, sizeof(String),
	offset(after_sound_bindings_str), XtRString, NULL
    },
    {
	"beforeCommandBindings", "CommandBindings", XtRString, sizeof(String),
	offset(before_command_bindings_str), XtRString, NULL
    },
    {
	"afterCommandBindings", "CommandBindings", XtRString, sizeof(String),
	offset(after_command_bindings_str), XtRString, NULL
    },
    {
	"annotationCount", "Count", XtRInt, sizeof(int),
        offset(annotation_count), XtRString, "1"
    },
    {
	"unknownAnnotationCount", "Count", XtRInt, sizeof(int),
        offset(unknown_annotation_count), XtRString, "1"
    },
    {
	"annotationAbove", "AnnotationAbove", XtRBoolean, sizeof(Boolean),
	offset(annotation_above), XtRString, "False"
    },
    {
	"background", "Background", XtRPixel, sizeof(Pixel),
	offset(background), XtRString, XtDefaultBackground
    },
    {
	"shapeExtra", "Shape", XtRBoolean, sizeof(Boolean),
	offset(shape_extra), XtRString, "True"
    },
    {
	"pathByChdir", "PathByChdir", XtRBoolean, sizeof(Boolean),
	offset(path_by_chdir), XtRString, "True"
    },
    {
	"lookupHostname", "LookupHostname", XtRBoolean, sizeof(Boolean),
	offset(lookup_hostname), XtRString, "False"
    },
};

#undef offset

static XrmOptionDescRec options[] =
{
    {
	"-c", "frame.setWidth", XrmoptionSepArg, NULL
    },
    {
	"-e", "listCommand", XrmoptionSepArg, NULL
    },
    {
	"-f", "facedbPath", XrmoptionSepArg, NULL
    },
    {
	"-h", "frame.tileHeight",  XrmoptionSepArg, NULL
    },
    {
	"-p", "update", XrmoptionSepArg, NULL
    },
    {
	"-s", "spoolFile", XrmoptionSepArg, NULL
    },
    {
	"-w", "frame.tileWidth",  XrmoptionSepArg, NULL
    },
    {
	"-C", "compressImages", XrmoptionNoArg, "False"
    },
    {
	"-K", "keepOrder", XrmoptionNoArg, "True"
    },
    {
	"-S", "shapeExtra", XrmoptionNoArg, "False"
    },
    {
	"-pop", "popHost", XrmoptionSepArg, NULL
    }
};


static Atom wm_delete_window;

static void
quit (w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    if (event->type == ClientMessage &&
        event->xclient.data.l[0] != wm_delete_window)
    {
        XBell (XtDisplay(w), 0);
        return;
    }
    

    /*
     * First suspend layout.  Then cleanup.
    */
    
    XtVaSetValues(TheFrame, XtNlayout, False, NULL);
    
    MailBoxClear();
    MailBoxClean();
    
    XCloseDisplay (XtDisplay(w));
    exit (0);
}

static XtActionsRec xfaces_actions[] =
{
    { "quit",	quit },
};

/*
 * Here are the main widgets.
*/

Widget TheTopLevel;		       /* The top level shell widget. */
Widget TheFrame;		       /* A frame to hold the faces. */

FaceImage	*NoMailImage = NULL;
FaceSound	*NoMailSound = NULL;

void
CheckMailNow()
{
    /*
     * First suspend layout.
    */

    XtVaSetValues(TheFrame, XtNlayout, False, NULL);
    
    /*
     *   Wipe the current face display list.
    */
    
    MailBoxClear();
    
    /*
     * Process items.
    */

#ifdef POP
    if (TheFacesResources.pop_host)
    {
	PopCheck();
    }
    else
#endif
    if (TheFacesResources.list_command)
    {
	CmdCheck();
    }
    else
    {
	MailCheck();
    }
    
    /*
     *    Build the FaceDisplay list from the item list.
    */
    
    MailBoxClean();
    
    /*
     * Now resume layout.
    */
    
    XtVaSetValues(TheFrame, XtNlayout, True, NULL);
    
    /*
     *    Shape the window.
    */
    
#ifdef SHAPE
    FaceShapeCreate();
#endif
    
    
}

static void StartTimer();

/*
 * Check for changes to the mail spool file and kick the
 * timer again.
*/

/* ARGSUSED */
static void
CheckMail( data, id )
caddr_t data;
XtIntervalId *id;
{
    
    CheckMailNow();
    
    /*
     * Restart the timer.
    */
    
    StartTimer(TheFacesResources.update);
    
    return;
}

static void
StartTimer(secs)
int secs;
{
    (void) XtAddTimeOut((unsigned long) secs * 1000,
			CheckMail,
                        (caddr_t) NULL );
}

/*
 * Here is the main routine.
*/

int
main(argc, argv)
int argc;
char *argv[];
{
    char *name;
    char *home;
    struct passwd *pw;
    FacesResources	fr = &TheFacesResources;
    
    /*
     * Create the top level widget.
    */
    
    TheTopLevel = XtInitialize(NULL, XFACES_CLASS,
			       options, XtNumber(options),
			       &argc, argv);
    
    /*
     * Load any application resources.
    */
    
    XtGetApplicationResources( TheTopLevel,
                              fr,
                              FacesResourcesList,
                              XtNumber(FacesResourcesList),
			      NULL,
                              0 );
    
    /*
     * This is a hack so that f.delete will do something useful in this
     * single-window application.
    */
    
    XtAppAddActions (XtWidgetToApplicationContext(TheTopLevel),
		     xfaces_actions, XtNumber(xfaces_actions));
    
    XtOverrideTranslations(TheTopLevel,
		    XtParseTranslationTable ("<Message>WM_PROTOCOLS: quit()"));
    
    /*
     * If the spool file is null then create one from the
     * user name.
    */
    
    if (fr->spool_file == NULL)
    {
	if ((name = getlogin()) == NULL)
	{
	    pw = getpwuid(getuid());
	    if (pw != NULL || pw->pw_name != NULL)
	    {
		name = pw->pw_name;
	    }
	    else
	    {
		name = "nobody";
	    }
	    endpwent();
	}
	
	fr->spool_file = XtMalloc(strlen(fr->spool_dir)+ strlen(name) + 2);
	(void) sprintf(fr->spool_file, "%s/%s", fr->spool_dir, name);
    }
    
    if (fr->pop_auth_file == NULL)
    {
	home = getenv("HOME");
	if (home == NULL)
	{
	    home = "/tmp";
	}
	
	fr->pop_auth_file = XtMalloc(strlen(home) + 30);
	sprintf(fr->pop_auth_file, "%s/.popauth", home);
    }

    /*
     *   Initialize the image types.
    */
    
    FaceImageXbmInit();
#ifdef XPM
    FaceImageXpmInit();
#endif
#ifdef IKON
    FaceImageIkonInit();
#endif
    
    /*
     * Initialize the search types.
    */
    
    FaceSearchBindingInit();
    FaceSearchFacedbInit();
    FaceSearchResourceInit();
    FaceSearchUserHostInit();
#ifdef XFACE
    FaceSearchXFaceInit();
#endif
    
    /*
     * Parse up the image/sound paths.
    */
    
    fr->image_path_str  = XtNewString(fr->image_path);
    fr->sound_path_str  = XtNewString(fr->sound_path);
    fr->facedb_path_str = XtNewString(fr->facedb_path);
    
    fr->image_paths  = PathParse(fr->image_path_str);
    fr->sound_paths  = PathParse(fr->sound_path_str);
    fr->facedb_paths = PathParse(fr->facedb_path_str);
    
    /*
     *   Parse the image types.
    */
    
    fr->image_types_str = XtNewString(fr->image_types_str);
    fr->image_types = FaceImageTypeListParse(fr->image_types_str);
    
    /*
     *    Create binding lists.
    */
    
    fr->ignore_message_bindings =FaceBindingParse(fr->ignore_message_bindings_str, 0, 0);
    fr->before_image_bindings = FaceBindingParse(fr->before_image_bindings_str, 1, 1);
    fr->after_image_bindings = FaceBindingParse(fr->after_image_bindings_str, 1, 1);
    fr->before_sound_bindings = FaceBindingParse(fr->before_sound_bindings_str, 1,1 );
    fr->after_sound_bindings =FaceBindingParse(fr->after_sound_bindings_str, 1, 1);
    /* fr->command_bindings = FaceBindingParse(fr->command_bindings_str, 1, 0); */
    
    /*
     * Parse the search instructions.
    */
    
    fr->image_search_str = XtNewString(fr->image_search_str);
    fr->image_search = FaceSearchParse(fr->image_search_str, FormatImage);
    fr->sound_search_str = XtNewString(fr->sound_search_str);
    fr->sound_search = FaceSearchParse(fr->sound_search_str, FormatAudio);
    fr->command_search_str = XtNewString(fr->command_search_str);
    fr->command_search = FaceSearchParse(fr->command_search_str, FormatCommand);
    
    /*
     * Now we create a box to keep our faces in.
    */
    
    TheFrame = XtVaCreateManagedWidget("frame", tiledWidgetClass,
				       TheTopLevel, NULL);
    
    /*
     * Initialize the face list.
    */
    
#ifdef SOUND
    FaceSoundInit();
#endif /* SOUND */
    FaceActionsInit();
    FaceDisplayInit();
    FaceAnnotateInit();
    MailItemInit();
    
    XtRealizeWidget(TheTopLevel);
    
    wm_delete_window = XInternAtom (XtDisplay(TheTopLevel),
				    "WM_DELETE_WINDOW",
                                    False);
    
    (void) XSetWMProtocols (XtDisplay(TheTopLevel), XtWindow(TheTopLevel),
                            &wm_delete_window, 1);
    
    /*
     * Set the initial shape.
    */

#ifdef SHAPE
    FaceShapeCreate();
#endif
    
    /*
     * Kick off the timer. (we start the timer with one second so that the
     * the shape stuff will get flushed first.
    */
    
    StartTimer(1);
    
    /*
     * Ok, we are all set up!
    */
    
    XtMainLoop();
    
    return 0;
}

void
regerror(s)
String	s;
{
    fprintf(stderr, "regerror: %s\n", s);
    abort();
}
