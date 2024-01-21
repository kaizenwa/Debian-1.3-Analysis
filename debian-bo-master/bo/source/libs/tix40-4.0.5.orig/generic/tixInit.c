/*
 * tixInit.c --
 *
 *	Initialze the internals of Tix.
 *
 * Copyright (c) 1996, Expert Interface Technologies
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#include "tkPort.h"
#include "tkInt.h"
#include <tixInt.h>
#include "tixPatchLevel.h"

#ifdef  _Windows
#ifndef _WINDOWS
#define _WINDOWS
#endif
#endif

#ifdef _WINDOWS
#include <tkWinInt.h>
#endif

extern TIX_DECLARE_CMD(Tix_CallMethodCmd);
extern TIX_DECLARE_CMD(Tix_ChainMethodCmd);
extern TIX_DECLARE_CMD(Tix_ClassCmd);
extern TIX_DECLARE_CMD(Tix_DoWhenIdleCmd);
extern TIX_DECLARE_CMD(Tix_DoWhenMappedCmd);
extern TIX_DECLARE_CMD(Tix_FileCmd);
extern TIX_DECLARE_CMD(Tix_FlushXCmd);
extern TIX_DECLARE_CMD(Tix_FormCmd);
extern TIX_DECLARE_CMD(Tix_GridCmd);
extern TIX_DECLARE_CMD(Tix_GeometryRequestCmd);
extern TIX_DECLARE_CMD(Tix_Get3DBorderCmd);
extern TIX_DECLARE_CMD(Tix_GetBooleanCmd);
extern TIX_DECLARE_CMD(Tix_GetIntCmd);
extern TIX_DECLARE_CMD(Tix_GetMethodCmd);
extern TIX_DECLARE_CMD(Tix_HListCmd);
extern TIX_DECLARE_CMD(Tix_HandleOptionsCmd);
extern TIX_DECLARE_CMD(Tix_InputOnlyCmd);
extern TIX_DECLARE_CMD(Tix_ItemStyleCmd);
extern TIX_DECLARE_CMD(Tix_ManageGeometryCmd);
extern TIX_DECLARE_CMD(Tix_MapWindowCmd);
extern TIX_DECLARE_CMD(Tix_MoveResizeWindowCmd);
extern TIX_DECLARE_CMD(Tix_NoteBookFrameCmd);
extern TIX_DECLARE_CMD(Tix_RaiseWindowCmd);
extern TIX_DECLARE_CMD(Tix_ShellInputCmd);
extern TIX_DECLARE_CMD(Tix_StringSubCmd);
extern TIX_DECLARE_CMD(Tix_TListCmd);
extern TIX_DECLARE_CMD(Tix_TmpLineCmd);
extern TIX_DECLARE_CMD(Tix_UnmapWindowCmd);
extern TIX_DECLARE_CMD(Tix_MwmCmd);

static Tix_TclCmd commands[] = {
    /*
     * Commands that are part of the intrinsics:
     */
    {"tixCallMethod",           Tix_CallMethodCmd},
    {"tixChainMethod",          Tix_ChainMethodCmd},
    {"tixClass",                Tix_ClassCmd},
    {"tixDisplayStyle",         Tix_ItemStyleCmd},
    {"tixDoWhenIdle",           Tix_DoWhenIdleCmd},
    {"tixDoWhenMapped",         Tix_DoWhenMappedCmd},
    {"tixFile",                 Tix_FileCmd},
    {"tixFlushX",           	Tix_FlushXCmd},
    {"tixForm",                 Tix_FormCmd},
    {"tixHList",                Tix_HListCmd},
    {"tixItemStyle",            Tix_ItemStyleCmd},	/* Old name */
    {"tixGeometryRequest",      Tix_GeometryRequestCmd},
    {"tixGet3DBorder",		Tix_Get3DBorderCmd},
    {"tixGetBoolean",		Tix_GetBooleanCmd},
    {"tixGetInt",		Tix_GetIntCmd},
    {"tixGetMethod",            Tix_GetMethodCmd},
    {"tixHandleOptions",        Tix_HandleOptionsCmd},
#ifndef _WINDOWS
    {"tixInputOnly",		Tix_InputOnlyCmd},
#endif
    {"tixManageGeometry",       Tix_ManageGeometryCmd},
    {"tixMapWindow",            Tix_MapWindowCmd},
    {"tixMoveResizeWindow",     Tix_MoveResizeWindowCmd},
#ifndef _WINDOWS
    {"tixMwm",     		Tix_MwmCmd},
#endif
    {"tixNoteBookFrame",        Tix_NoteBookFrameCmd},
    {"tixRaiseWindow",          Tix_RaiseWindowCmd},
    {"tixShellInput",           Tix_ShellInputCmd},
    {"tixStringSub",		Tix_StringSubCmd},
    {"tixTmpLine",              Tix_TmpLineCmd},
    {"tixUnmapWindow",          Tix_UnmapWindowCmd},
    {"tixWidgetClass",          Tix_ClassCmd},
    {"tixWidgetDoWhenIdle",     Tix_DoWhenIdleCmd},

#ifndef TIX_VERSION_4_0_x
    {"tixTList",     		Tix_TListCmd},
    {"tixGrid",     		Tix_GridCmd},
#endif

    {(char *) NULL,		(int (*)()) NULL}
};

typedef struct {
    int		isBeta;
    char      * binding;
    int		isDebug;
    char      * fontSet;
    char      * tixlibrary;
    char      * scheme;
    char      * schemePriority;
} OptionStruct;

static OptionStruct tixOption;

#define DEF_TIX_TOOLKIT_OPTION_BETA		"1"
#define DEF_TIX_TOOLKIT_OPTION_BINDING		"Motif"
#define DEF_TIX_TOOLKIT_OPTION_DEBUG		"1"
#define DEF_TIX_TOOLKIT_OPTION_FONTSET		"14Point"
#define DEF_TIX_TOOLKIT_OPTION_LIBRARY		""
#define DEF_TIX_TOOLKIT_OPTION_SCHEME		"TixGray"
#define DEF_TIX_TOOLKIT_OPTION_SCHEME_PRIORITY	"79"

static Tk_ConfigSpec configSpecs[] = {
    {TK_CONFIG_BOOLEAN, "-beta", "tixBeta", "TixBeta",
       DEF_TIX_TOOLKIT_OPTION_BETA, Tk_Offset(OptionStruct, isBeta), 0},
    {TK_CONFIG_STRING, "-binding", "binding", "TixBinding",
       DEF_TIX_TOOLKIT_OPTION_BINDING, Tk_Offset(OptionStruct, binding),
       0},
    {TK_CONFIG_BOOLEAN, "-debug", "tixDebug", "TixDebug",
       DEF_TIX_TOOLKIT_OPTION_DEBUG, Tk_Offset(OptionStruct, isDebug), 0},
    {TK_CONFIG_STRING, "-fontset", "tixFontSet", "TixFontSet",
       DEF_TIX_TOOLKIT_OPTION_FONTSET, Tk_Offset(OptionStruct, fontSet),
       0},
    {TK_CONFIG_STRING, "-scheme", "tixScheme", "TixScheme",
       DEF_TIX_TOOLKIT_OPTION_SCHEME, Tk_Offset(OptionStruct, scheme),
       0},
    {TK_CONFIG_STRING, "-scheme", "tixSchemePriority", "TixSchemePriority",
       DEF_TIX_TOOLKIT_OPTION_SCHEME_PRIORITY,
       Tk_Offset(OptionStruct, schemePriority),
       0},
    {TK_CONFIG_STRING, "-tixlibrary", "tixLibrary", "TixLibrary",
       DEF_TIX_TOOLKIT_OPTION_LIBRARY, Tk_Offset(OptionStruct, tixlibrary),
       0},
    {TK_CONFIG_END, (char *) NULL, (char *) NULL, (char *) NULL,
       (char *) NULL, 0, 0}
};

#ifndef TIX_LIBRARY
#ifndef _WINDOWS
#define TIX_LIBRARY "/usr/local/lib/tix"
#else
#define TIX_LIBRARY "../library"
#endif
#endif

/*----------------------------------------------------------------------
 *
 * 			Some global variables
 *
 *----------------------------------------------------------------------
 */
Tk_Uid tixNormalUid   = (Tk_Uid)NULL;
Tk_Uid tixDisabledUid = (Tk_Uid)NULL;

/*----------------------------------------------------------------------
 *
 * 			The Display Item types
 *
 *----------------------------------------------------------------------
 */

extern Tix_DItemInfo tix_ImageTextItemType;
extern Tix_DItemInfo tix_TextItemType;
extern Tix_DItemInfo tix_WindowItemType;
extern Tix_DItemInfo tix_ImageItemType;

static int		ParseToolkitOptions _ANSI_ARGS_((Tcl_Interp * interp));
static int 		mwmProtocolHandler _ANSI_ARGS_((ClientData clientData,
			    XEvent *eventPtr));
static int		Tix_Init_Internal _ANSI_ARGS_((Tcl_Interp * interp,
			     int doSource));
int 			Tix_EtInit _ANSI_ARGS_((Tcl_Interp * interp));

/*----------------------------------------------------------------------
 * ParseToolkitOptions() --
 *
 *	Before the Tix initialized, we need to determine the toolkit
 *	options which are set by the options database.
 *----------------------------------------------------------------------
 */
static int
ParseToolkitOptions(interp)
    Tcl_Interp * interp;
{
    char buff[10];
    int flag;

    tixOption.isBeta = 0;
    tixOption.binding = NULL;
    tixOption.isDebug = 0;
    tixOption.fontSet = NULL;
    tixOption.tixlibrary = NULL;
    tixOption.scheme = NULL;
    tixOption.schemePriority = NULL;

    /*
	  * The toolkit options may be set in the resources of the main window
     */
    if (Tk_ConfigureWidget(interp, Tk_MainWindow(interp), configSpecs,
	    0, 0, (char *) &tixOption, 0) != TCL_OK) {
	return TCL_ERROR;
    }

    /*
     * Now lets set the Tix toolkit variables so that the Toolkit can
     * initialize according to user options.
     */
    flag = TCL_GLOBAL_ONLY;
    sprintf(buff, "%d", tixOption.isBeta);
    Tcl_SetVar2(interp, "tix_priv", "-beta", buff, flag);
    sprintf(buff, "%d", tixOption.isDebug);
    Tcl_SetVar2(interp, "tix_priv", "-debug", buff, flag);

    if (strlen(tixOption.tixlibrary) == 0) {
	/* Set up the TCL variable "tix_library" according to the environment
	 * variable.
	 */
	 tixOption.tixlibrary= getenv("TIX_LIBRARY");
	 if (tixOption.tixlibrary == NULL) {
	     tixOption.tixlibrary = TIX_LIBRARY;
	 }
     }

    Tcl_SetVar2(interp, "tix_priv", "-binding",
	tixOption.binding,    		flag);
    Tcl_SetVar2(interp, "tix_priv", "-fontset", 
	tixOption.fontSet,    		flag);
    Tcl_SetVar2(interp, "tix_priv", "-scheme",  
	tixOption.scheme,     		flag);
    Tcl_SetVar2(interp, "tix_priv", "-schemepriority",
	tixOption.schemePriority,     flag);
    Tcl_SetVar2(interp, "tix_priv", "-libdir",  
	tixOption.tixlibrary, 		flag);

    return TCL_OK;
}

static int mwmProtocolHandler(clientData, eventPtr)
    ClientData clientData;
    XEvent *eventPtr;
{
    TkWindow *winPtr;
    Window handlerWindow;

    if (eventPtr->type != ClientMessage) {
	return 0;
    }

	 handlerWindow = eventPtr->xany.window;
    winPtr = (TkWindow *) Tk_IdToWindow(eventPtr->xany.display, handlerWindow);
    if (winPtr != NULL) {
	if (eventPtr->xclient.message_type ==
	    Tk_InternAtom((Tk_Window) winPtr,"_MOTIF_WM_MESSAGES")) {
	    TkWmProtocolEventProc(winPtr, eventPtr);
	    return 1;
	}
    }
    return 0;
}

/*----------------------------------------------------------------------
 * Tix_Init_Internal() --
 *
 *	Initialize the Tix library. The doSource argument specifies
 *	we should source the file Init.tcl from the Tix script library
 *	path. A doSource is not necessary if Tix was included in an ET
 *	applicattion.
 *----------------------------------------------------------------------
 */
static int
Tix_Init_Internal(interp, doSource)
	 Tcl_Interp * interp;
	 int doSource;
{
    Tk_Window topLevel;
    char * appName;

    extern Tk_ImageType tixPixmapImageType;
    extern Tk_ImageType tixCompoundImageType;

    topLevel = Tk_MainWindow(interp);

    /* Initialize some global variables */
    tixNormalUid   = Tk_GetUid("normal");
    tixDisabledUid = Tk_GetUid("disabled");
    
    /* This is for tixMwm command */
    Tk_CreateGenericHandler(mwmProtocolHandler, NULL);

    /* Set the "tix_version" variable */
    Tcl_SetVar(interp, "tix_version",    TIX_VERSION,    TCL_GLOBAL_ONLY);
    Tcl_SetVar(interp, "tix_patchLevel", TIX_PATCHLEVEL, TCL_GLOBAL_ONLY);

    /* Initialize the Tix commands */
    Tix_CreateCommands(interp, commands, (ClientData) topLevel,
	(void (*)()) NULL);

    /* Initialize the image readers */
#ifndef _WINDOWS
    Tk_CreateImageType(&tixPixmapImageType);
#endif
    Tk_CreateImageType(&tixCompoundImageType);

#ifdef _WINDOWS
    Tcl_GlobalEval(interp, "set tixPriv(isWindows) 1");
#endif

    /* Initialize the display item types */
    Tix_AddDItemType(&tix_ImageTextItemType);
    Tix_AddDItemType(&tix_TextItemType);
    Tix_AddDItemType(&tix_WindowItemType);
    Tix_AddDItemType(&tix_ImageItemType);

    /* Parse options database for fontSets, schemes, etc */
    if (ParseToolkitOptions(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }

    if ((appName = Tcl_GetVar(interp, "argv0", TCL_GLOBAL_ONLY))== NULL) {
	appName = "tixwish";
    }

    if (doSource) {
	char stuff[100];

	/* Load the Tix library */
	if (Tix_LoadTclLibrary(interp, "TIX_LIBRARY", "tix_library", 
	    "Init.tcl",	TIX_LIBRARY, appName) != TCL_OK) {
	    return TCL_ERROR;
	}

	/* Let's check whether the TIX_LIBRARY variable is set to 
	 * a pre-4.0.2 version of Tix. (All 4.0.2+ version will correctly
	 * identify their own versions and will print out warning messages
	 * if the version of the binary does not match with the script library
	 */
	strcpy(stuff, "tixScriptVersion");
	if (Tcl_GlobalEval(interp, stuff) == TCL_ERROR) {
	    fprintf(stderr, 
		"Warning: Tix script library version (pre 4.0.2)\n");
	    fprintf(stderr, "  does not match binary version (%s).\n",
		TIX_PATCHLEVEL);
	    fprintf(stderr, "  Please check your TIX_LIBRARY environment ");
	    fprintf(stderr, "variable and your Tix installation.\n");
	    Tcl_ResetResult(interp);
	}
	return TCL_OK;
    } else {
	Tcl_SetVar(interp, "tix_library", "nowhere", TCL_GLOBAL_ONLY);
	return TCL_OK;
    }
}

/* Tix_Init --
 *
 * 	This is the function to call in your Tcl_AppInit() function
 */
int
Tix_Init(interp)
    Tcl_Interp * interp;
{
    return Tix_Init_Internal(interp, 1);
}

/* Tix_EtInit --
 *
 * 	This takes special care when you initialize the Tix
 *	library from an ET application.
 */
int Tix_EtInit(interp)
	 Tcl_Interp * interp;
{
    return Tix_Init_Internal(interp, 0);
}


