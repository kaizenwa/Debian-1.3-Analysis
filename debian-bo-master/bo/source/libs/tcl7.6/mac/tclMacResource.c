/*
 * tclMacResource.c --
 *
 *	This file contains several commands that manipulate or use
 *	Macintosh resources.  Included are extensions to the "source"
 *	command, the mac specific "beep" and "resource" commands, and
 *	administration for open resource file references.
 *
 * Copyright (c) 1996 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * SCCS: @(#) tclMacResource.c 1.6 96/10/08 14:33:54
 */

#include <FSpCompat.h>
#include <Resources.h>
#include <Sound.h>
#include <Strings.h>

#include "tcl.h"
#include "tclInt.h"
#include "tclMacInt.h"

/*
 * Hash table to track open resource files.
 */
static Tcl_HashTable nameTable;		/* Id to process number mapping. */
static Tcl_HashTable resourceTable;	/* Process number to id mapping. */
static int newId = 0;			/* Id source. */
static int initialized = 0;		/* 0 means static structures haven't 
					 * been initialized yet. */
/*
 * Procedures defined for just this file.
 */
static void		ResourceInit _ANSI_ARGS_((void));

/*
 *----------------------------------------------------------------------
 *
 * Tcl_ResourceCmd --
 *
 *	This procedure is invoked to process the "resource" Tcl command.
 *	See the user documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *----------------------------------------------------------------------
 */

int
Tcl_ResourceCmd(
    ClientData dummy,			/* Not used. */
    Tcl_Interp *interp,			/* Current interpreter. */
    int argc,				/* Number of arguments. */
    char **argv)			/* Argument strings. */
{
    int c, result;
    size_t length;
    long fileRef;
    FSSpec fileSpec;
    Tcl_DString buffer;
    char *nativeName;
    Tcl_HashEntry *resourceHashPtr;
    Tcl_HashEntry *nameHashPtr;
    Handle resource;
    OSErr err;

    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" option ?arg ...?\"", (char *) NULL);
	return TCL_ERROR;
    }
    c = argv[1][0];
    length = strlen(argv[1]);
    result = TCL_OK;

    if (!initialized) {
	ResourceInit();
    }

    if ((c == 'c') && (strncmp(argv[1], "close", length) == 0)) {
	nameHashPtr = Tcl_FindHashEntry(&nameTable, argv[2]);
	if (nameHashPtr == NULL) {
	    Tcl_AppendResult(interp, "invalid resource file reference \"",
		    argv[2], "\"", (char *) NULL);
	    return TCL_ERROR;
	}
	fileRef = (long) Tcl_GetHashValue(nameHashPtr);
	if (fileRef == 0) {
	    Tcl_AppendResult(interp, "can't close system resource",
		    (char *) NULL);
	    return TCL_ERROR;
	}
	Tcl_DeleteHashEntry(nameHashPtr);
	resourceHashPtr = Tcl_FindHashEntry(&resourceTable, (char *) fileRef);
	if (resourceHashPtr == NULL) {
	    panic("how did this happen");
	}
	ckfree(Tcl_GetHashValue(resourceHashPtr));
	Tcl_DeleteHashEntry(resourceHashPtr);

	CloseResFile((short) fileRef);
	return TCL_OK;
    } else if ((c == 'g') && (strncmp(argv[1], "getSTR", length) == 0)) {
	int rsrcId;
	unsigned char size;
	char *resourceName = NULL, *stringPtr, *resFileRef = NULL;
	
	if (!((argc == 3) || (argc == 4))) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " ", argv[1], " resourceId ?resourceRef?\"",
		    (char *) NULL);
	    return TCL_ERROR;
	}
        if (Tcl_GetInt(interp, argv[2], &rsrcId) != TCL_OK) {
	    Tcl_ResetResult(interp);
	    resourceName = argv[2];
        }

	if (argc == 4) {
	    resFileRef = argv[3];
	}
	
	resource = TclMacFindResource(interp, "STR ", resourceName,
		rsrcId, resFileRef);
			    
	if (resource != NULL) {
	    size = (*resource)[0];
	    stringPtr = (char *) ckalloc(size + 1);
	    strncpy(stringPtr, (*resource) + 1, size);
	    stringPtr[size] = '\0';
	    Tcl_SetResult(interp, stringPtr, TCL_DYNAMIC);
	    ReleaseResource(resource);
	    return TCL_OK;
	} else {
	    Tcl_AppendResult(interp, "could not load 'STR ' resource: \"",
			argv[2], "\"", (char *) NULL);
	    return TCL_ERROR;
	}
    } else if ((c == 'g') && (strncmp(argv[1], "getSTR#", length) == 0)) {
	int rsrcId, index, total, i;
	char *resourceName = NULL, *stringPtr, *resFileRef = NULL;
	char * ptr;
	
	if (!((argc == 4) || (argc == 5))) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " ", argv[1], " resourceId index ?resourceRef?\"",
		    (char *) NULL);
	}
        if (Tcl_GetInt(interp, argv[2], &rsrcId) != TCL_OK) {
	    Tcl_ResetResult(interp);
	    resourceName = argv[2];
        }
        if ((Tcl_GetInt(interp, argv[3], &index) != TCL_OK) || (index <= 0)) {
	    Tcl_AppendResult(interp, "invalid STR# index \"",
			argv[3], "\"", (char *) NULL);
	    return TCL_ERROR;
        }
	if (argc == 5) {
	    resFileRef = argv[4];
	}

	resource = TclMacFindResource(interp, "STR#", resourceName, rsrcId,
		resFileRef);

	if (resource != NULL) {
	    total = * (short *) resource;
	    if (index > total) {
		Tcl_ResetResult(interp);
		return TCL_OK;
	    }
	    HLock(resource);
	    ptr = *resource + 2;
	    for (i = 1; i != index; i++) {
		ptr += *ptr + 1;
	    }
	    stringPtr = (char *) ckalloc(*ptr + 1);
	    strncpy(stringPtr, ptr + 1, *ptr);
	    stringPtr[*ptr] = '\0';
	    Tcl_SetResult(interp, stringPtr, TCL_DYNAMIC);
	    HUnlock(resource);
	    ReleaseResource(resource);
	    return TCL_OK;
	} else {
	    Tcl_AppendResult(interp, "could not load 'STR#' resource: \"",
			argv[2], "\"", (char *) NULL);
	    return TCL_ERROR;
	}
    } else if ((c == 'g') && (strncmp(argv[1], "getTEXT", length) == 0)) {
	int rsrcId;
	char *resourceName = NULL, *stringPtr, *resFileRef = NULL;
	
	if (!((argc == 3) || (argc == 4))) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " ", argv[1], " resourceId ?resourceRef?\"",
		    (char *) NULL);
	}
        if (Tcl_GetInt(interp, argv[2], &rsrcId) != TCL_OK) {
	    Tcl_ResetResult(interp);
	    resourceName = argv[2];
        }

	if (argc == 4) {
	    resFileRef = argv[3];
	}
	
	resource = TclMacFindResource(interp, "TEXT", resourceName, rsrcId,
		resFileRef);
			    
	if (resource != NULL) {
	    stringPtr = TclMacConvertTextResource(resource);
	    Tcl_SetResult(interp, stringPtr, TCL_DYNAMIC);
	    ReleaseResource(resource);
	    return TCL_OK;
	} else {
	    Tcl_AppendResult(interp, "could not load 'TEXT' resource: \"",
			argv[2], "\"", (char *) NULL);
	    return TCL_ERROR;
	}
    } else if ((c == 'l') && (strncmp(argv[1], "list", length) == 0)) {
	int count, i, limitSearch = false;
	short id, saveRef;
	Str255 theName;
	ResType rezType;
			
	if (!((argc == 3) || (argc == 4))) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " ", argv[1], " resourceType ?resourceRef?\"",
		    (char *) NULL);
	}
	if (strlen(argv[2]) != 4) {
	    Tcl_AppendResult(interp, "not a valid resourceType: \"", 
		argv[2], "\"", (char *) NULL);
	}
	rezType =  *((long *) argv[2]);

	if (argc == 4) {
	    nameHashPtr = Tcl_FindHashEntry(&nameTable, argv[3]);
	    if (nameHashPtr == NULL) {
		Tcl_AppendResult(interp, "invalid resource file reference \"",
			argv[3], "\"", (char *) NULL);
		return TCL_ERROR;
	    }
	    fileRef = (long) Tcl_GetHashValue(nameHashPtr);
	    saveRef = CurResFile();
	    UseResFile((short) fileRef);
	    limitSearch = true;
	}

	Tcl_ResetResult(interp);
	if (limitSearch) {
	    count = Count1Resources(rezType);
	} else {
	    count = CountResources(rezType);
	}
	SetResLoad(false);
	for (i = 1; i <= count; i++) {
	    if (limitSearch) {
		resource = Get1IndResource(rezType, i);
	    } else {
		resource = GetIndResource(rezType, i);
	    }
	    if (resource != NULL) {
		GetResInfo(resource, &id, &rezType, theName);
		if (theName[0] != 0) {
		    theName[theName[0]+1] = '\0';
		    Tcl_AppendElement(interp, (char *) theName + 1);
		} else {
		    sprintf((char *) theName, "%d", id);
		    Tcl_AppendElement(interp, (char *) theName);
		}
		ReleaseResource(resource);
	    }
	}
	SetResLoad(true);
	
	if (limitSearch) {
	    UseResFile(saveRef);
	}
	
	return TCL_OK;
    } else if ((c == 'o') && (strncmp(argv[1], "open", length) == 0)) {
	int new;
	char *resourceId;
	
	if (argc != 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"resource open fileName\"", NULL);
	    return TCL_ERROR;
	}
	nativeName = Tcl_TranslateFileName(interp, argv[2], &buffer);
	if (nativeName == NULL) {
	    return TCL_ERROR;
	}
	err = FSpLocationFromPath(strlen(nativeName), nativeName, &fileSpec) ;
	Tcl_DStringFree(&buffer);
	if ( err != noErr ) {
	    Tcl_AppendResult(interp, "path doesn't lead to a file", NULL);
	    return TCL_ERROR;
	}
	
	fileRef = (long) FSpOpenResFileCompat(&fileSpec, fsRdPerm);
	if (fileRef == -1) {
	    return TCL_ERROR;
	}
	
	resourceHashPtr = Tcl_CreateHashEntry(&resourceTable,
		(char *) fileRef, &new);
	if (!new) {
	    resourceId = (char *) Tcl_GetHashValue(resourceHashPtr);
	    Tcl_AppendResult(interp, resourceId, NULL);
	    return TCL_OK;
	}
  	
  	resourceId = (char *) ckalloc(15);
	sprintf(resourceId, "resource%d", newId);
	Tcl_SetHashValue(resourceHashPtr, resourceId);
	newId++;

	nameHashPtr = Tcl_CreateHashEntry(&nameTable, resourceId, &new);
	if (!new) {
	    panic("resource id has repeated itself");
	}
	Tcl_SetHashValue(nameHashPtr, fileRef);
	
	Tcl_AppendResult(interp, resourceId, NULL);
	return TCL_OK;
    } else if ((c == 't') && (strncmp(argv[1], "types", length) == 0)) {
	int count, i, limitSearch = false;
	short saveRef;
	Str255 theName;
	ResType rezType;
			
	if (!((argc == 2) || (argc == 3))) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		    " ", argv[1], " ?resourceRef?\"", (char *) NULL);
	}

	if (argc == 3) {
	    nameHashPtr = Tcl_FindHashEntry(&nameTable, argv[2]);
	    if (nameHashPtr == NULL) {
		Tcl_AppendResult(interp, "invalid resource file reference \"",
			argv[2], "\"", (char *) NULL);
		return TCL_ERROR;
	    }
	    fileRef = (long) Tcl_GetHashValue(nameHashPtr);
	    saveRef = CurResFile();
	    UseResFile((short) fileRef);
	    limitSearch = true;
	}

	Tcl_ResetResult(interp);
	if (limitSearch) {
	    count = Count1Types();
	} else {
	    count = CountTypes();
	}
	for (i = 1; i <= count; i++) {
	    if (limitSearch) {
		Get1IndType(&rezType, i);
	    } else {
		GetIndType(&rezType, i);
	    }
	    sprintf((char *) theName, "%-4.4s", &rezType);
	    Tcl_AppendElement(interp, (char *) theName);
	}
	
	if (limitSearch) {
	    UseResFile(saveRef);
	}
	
	return TCL_OK;
    } else {
	Tcl_AppendResult(interp, "unknown option \"", argv[1],
		"\": should be close, getSTR, getSTR#, getTEXT, ",
		"list, open or types", (char *) NULL);
	return TCL_ERROR;
    }

    return TCL_ERROR;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_MacSourceCmd --
 *
 *	This procedure is invoked to process the "source" Tcl command.
 *	See the user documentation for details on what it does.  In addition,
 *	it supports sourceing from the resource fork of type 'TEXT'.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *----------------------------------------------------------------------
 */

int
Tcl_MacSourceCmd(
    ClientData dummy,			/* Not used. */
    Tcl_Interp *interp,			/* Current interpreter. */
    int argc,				/* Number of arguments. */
    char **argv)			/* Argument strings. */
{
    char *errNum = "wrong # args: ";
    char *errBad = "bad argument: ";
    char *errStr;
    char *fileName = NULL, *rsrcName = NULL;
    int rsrcID = -1;

    if (argc < 2 || argc > 4)  {
    	errStr = errNum;
    	goto sourceFmtErr;
    }
    
    if (argc == 2)  {
	return Tcl_EvalFile(interp, argv[1]);
    }
    
    /*
     * The following code supports a few older forms of this command
     * for backward compatability.
     */
    if (!strcmp(argv[1], "-rsrc") || !strcmp(argv[1], "-rsrcname")) {
	rsrcName = argv[2];
    } else if (!strcmp(argv[1], "-rsrcid")) {
	if (Tcl_GetInt(interp, argv[2], &rsrcID) != TCL_OK) {
	    return TCL_ERROR;
	}
    } else {
    	errStr = errBad;
    	goto sourceFmtErr;
    }
    
    if (argc == 4) {
	fileName = argv[3];
    }
	
    return TclMacEvalResource(interp, rsrcName, rsrcID, fileName);
	
    sourceFmtErr:
    Tcl_AppendResult(interp, errStr, "should be \"", argv[0],
	    " fileName\" or \"", argv[0], " -rsrc name ?fileName?\" or \"",
	    argv[0], " -rsrcid id ?fileName?\"", (char *) NULL);
    return TCL_ERROR;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_MacBeepCmd --
 *
 *	This procedure makes the beep sound.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	Makes a beep.
 *
 *----------------------------------------------------------------------
 */

int
Tcl_MacBeepCmd(
    ClientData dummy,			/* Not used. */
    Tcl_Interp *interp,			/* Current interpreter. */
    int argc,				/* Number of arguments. */
    char **argv)			/* Argument strings. */
{
    Handle sound;
    Str255 sndName;
    int volume = -1;
    char * sndArg = NULL;
    long curVolume;

    if (argc == 1) {
	SysBeep(1);
	return TCL_OK;
    } else if (argc == 2) {
	if (!strcmp(argv[1], "-list")) {
	    int count, i;
	    short id;
	    Str255 theName;
	    ResType rezType;
			
	    Tcl_ResetResult(interp);
	    count = CountResources('snd ');
	    for (i = 1; i <= count; i++) {
		sound = GetIndResource('snd ', i);
		if (sound != NULL) {
		    GetResInfo(sound, &id, &rezType, theName);
		    if (theName[0] == 0) {
			continue;
		    }
		    theName[theName[0]+1] = '\0';
		    Tcl_AppendElement(interp, (char *) theName + 1);
		}
	    }
	    return TCL_OK;
	} else {
	    sndArg = argv[1];
	}
    } else if (argc == 3) {
	if (!strcmp(argv[1], "-volume")) {
	    volume = atoi(argv[2]);
	} else {
	    goto beepUsage;
	}
    } else if (argc == 4) {
	if (!strcmp(argv[1], "-volume")) {
	    volume = atoi(argv[2]);
	    sndArg = argv[3];
	} else {
	    goto beepUsage;
	}
    } else {
	goto beepUsage;
    }
	
    /*
     * Set Volume
     */
    if (volume >= 0) {
	GetSysBeepVolume(&curVolume);
	SetSysBeepVolume((short) volume);
    }
	
    /*
     * Play the sound
     */
    if (sndArg == NULL) {
	SysBeep(1);
    } else {
	strcpy((char *) sndName + 1, sndArg);
	sndName[0] = strlen(sndArg);
	sound = GetNamedResource('snd ', sndName);
	if (sound != NULL) {
#if (THINK_C == 7)
	    SndPlay(NULL, sound, false);
#else
	    SndPlay(NULL, (SndListHandle) sound, false);
#endif
	    return TCL_OK;
	} else {
	    if (volume >= 0) {
		SetSysBeepVolume(curVolume);
	    }
	    Tcl_ResetResult(interp);
	    Tcl_AppendResult(interp, " \"", sndArg, 
		    "\" is not a valid sound.  (Try ", argv[0],
		    " -list)", NULL);
	    return TCL_ERROR;
	}
    }

    /*
     * Reset Volume
     */
    if (volume >= 0) {
	SetSysBeepVolume(curVolume);
    }
    return TCL_OK;

    beepUsage:
    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " [-volume num] [-list | sndName]?\"", (char *) NULL);
    return TCL_ERROR;
}

/*
 *-----------------------------------------------------------------------------
 *
 * TclMacEvalResource --
 *
 *	Used to extend the source command.  Sources Tcl code from a Text
 *	resource.  Currently only sources the resouce by name file ID may be
 *	supported at a later date.
 *
 * Side Effects:
 *	Depends on the Tcl code in the resource.
 *
 * Results:
 *      Returns a Tcl result.
 *
 *-----------------------------------------------------------------------------
 */

int
TclMacEvalResource(
    Tcl_Interp *interp,		/* Interpreter in which to process file. */
    char *resourceName,		/* Name of TEXT resource to source,
				   NULL if number should be used. */
    int resourceNumber,		/* Resource id of source. */
    char *fileName)		/* Name of file to process.
				   NULL if application resource. */
{
    Handle sourceText;
    Str255 rezName;
    char msg[200];
    int result;
    short saveRef, fileRef = -1;
    char idStr[64];
    FSSpec fileSpec;
    Tcl_DString buffer;
    char *nativeName;

    saveRef = CurResFile();
	
    if (fileName != NULL) {
	OSErr err;
	
	nativeName = Tcl_TranslateFileName(interp, fileName, &buffer);
	if (nativeName == NULL) {
	    return TCL_ERROR;
	}
	err = FSpLocationFromPath(strlen(nativeName), nativeName, &fileSpec);
	Tcl_DStringFree(&buffer);
	if (err != noErr) {
	    Tcl_AppendResult(interp, "Error finding the file: \"", 
		fileName, "\".", NULL);
	    return TCL_ERROR;
	}
		
	fileRef = FSpOpenResFileCompat(&fileSpec, fsRdPerm);
	if (fileRef == -1) {
	    Tcl_AppendResult(interp, "Error reading the file: \"", 
		fileName, "\".", NULL);
	    return TCL_ERROR;
	}
		
	UseResFile(fileRef);
    } else {
	/*
	 * The default behavior will search through all open resource files.
	 * This may not be the behavior you desire.  If you want the behavior
	 * of this call to *only* search the application resource fork, you
	 * must call UseResFile at this point to set it to the application
	 * file.  This means you must have already obtained the application's 
	 * fileRef when the application started up.
	 */
    }
	
    /*
     * Load the resource by name or ID
     */
    if (resourceName != NULL) {
	strcpy((char *) rezName + 1, resourceName);
	rezName[0] = strlen(resourceName);
	sourceText = GetNamedResource('TEXT', rezName);
    } else {
	sourceText = GetResource('TEXT', (short) resourceNumber);
    }
	
    if (sourceText == NULL) {
	result = TCL_ERROR;
    } else {
	char *sourceStr = NULL;
	
	sourceStr = TclMacConvertTextResource(sourceText);
	ReleaseResource(sourceText);
		
	/*
	 * We now evaluate the Tcl source
	 */
	result = Tcl_Eval(interp, sourceStr);
	ckfree(sourceStr);
	if (result == TCL_RETURN) {
	    result = TCL_OK;
	} else if (result == TCL_ERROR) {
	    sprintf(msg, "\n    (rsrc \"%.150s\" line %d)", resourceName,
		    interp->errorLine);
	    Tcl_AddErrorInfo(interp, msg);
	}
				
	goto rezEvalCleanUp;
    }
	
    rezEvalError:
    sprintf(idStr, "ID=%d", resourceNumber);
    Tcl_AppendResult(interp, "The resource \"",
	    (resourceName != NULL ? resourceName : idStr),
	    "\" could not be loaded from ",
	    (fileName != NULL ? fileName : "application"),
	    ".", NULL);

    rezEvalCleanUp:
    if (fileRef != -1) {
	CloseResFile(fileRef);
    }

    UseResFile(saveRef);
	
    return result;
}

/*
 *-----------------------------------------------------------------------------
 *
 * TclMacConvertTextResource --
 *
 *	Converts a TEXT resource into a Tcl suitable string.
 *
 * Side Effects:
 *	Mallocs the returned memory, converts '\r' to '\n', and appends a NULL.
 *
 * Results:
 *      A new malloced string.
 *
 *-----------------------------------------------------------------------------
 */

char *
TclMacConvertTextResource(
    Handle resource)		/* Handle to TEXT resource. */
{
    int i, size;
    char *resultStr;

    size = SizeResource(resource);
    
    resultStr = ckalloc(size + 1);
    
    for (i=0; i<size; i++) {
	if ((*resource)[i] == '\r') {
	    resultStr[i] = '\n';
	} else {
	    resultStr[i] = (*resource)[i];
	}
    }
    
    resultStr[size] = '\0';

    return resultStr;
}

/*
 *-----------------------------------------------------------------------------
 *
 * TclMacFindResource --
 *
 *	Higher level interface for loading resources.
 *
 * Side Effects:
 *	Attempts to load a resource.
 *
 * Results:
 *      A handle on success.
 *
 *-----------------------------------------------------------------------------
 */

Handle
TclMacFindResource(
    Tcl_Interp *interp,		/* Interpreter in which to process file. */
    char *resourceType,		/* Type of resource to load. */
    char *resourceName,		/* Name of resource to source,
				   NULL if number should be used. */
    int resourceNumber,		/* Resource id of source. */
    char *resFileRef)		/* Registered resource file reference,
				 * NULL if searching all open resource files. */
{
    Tcl_HashEntry *nameHashPtr;
    long fileRef;
    ResType rezType;
    int limitSearch = false;
    short saveRef;
    Handle resource;

    if (strlen(resourceType) != 4) {
	Tcl_AppendResult(interp, "not a valid resource type: \"", 
		resourceType, "\"", (char *) NULL);
	return NULL;
    }
    rezType =  *((long *) resourceType);

    if (resFileRef != NULL) {
	nameHashPtr = Tcl_FindHashEntry(&nameTable, resFileRef);
	if (nameHashPtr == NULL) {
	    Tcl_AppendResult(interp, "invalid resource file reference \"",
			resFileRef, "\"", (char *) NULL);
	    return NULL;
	}
	fileRef = (long) Tcl_GetHashValue(nameHashPtr);
	saveRef = CurResFile();
	UseResFile((short) fileRef);
	limitSearch = true;
    }

    if (resourceName == NULL) {
	if (limitSearch) {
	    resource = Get1Resource(rezType, resourceNumber);
	} else {
	    resource = GetResource(rezType, resourceNumber);
	}
    } else {
	c2pstr(resourceName);
	if (limitSearch) {
	    resource = Get1NamedResource(rezType, (StringPtr) resourceName);
	} else {
	    resource = GetNamedResource(rezType, (StringPtr) resourceName);
	}
	p2cstr((StringPtr) resourceName);
    }

    if (limitSearch) {
	UseResFile(saveRef);
    }

    return resource;
}

/*
 *----------------------------------------------------------------------
 *
 * ResourceInit --
 *
 *	Initialize the structures used for resource management.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Read the code.
 *
 *----------------------------------------------------------------------
 */

static void
ResourceInit()
{
    Tcl_HashEntry *resourceHashPtr;
    Tcl_HashEntry *nameHashPtr;
    long fileRef;
    char * resourceId;
    int new;

    initialized = 1;
    Tcl_InitHashTable(&nameTable, TCL_STRING_KEYS);
    Tcl_InitHashTable(&resourceTable, TCL_ONE_WORD_KEYS);

    /*
     * Place the application resource file into our cache.
     */
    fileRef = CurResFile();
    resourceHashPtr = Tcl_CreateHashEntry(&resourceTable, (char *) fileRef,
	    &new);
    resourceId = (char *) ckalloc(strlen("application") + 1);
    sprintf(resourceId, "application");
    Tcl_SetHashValue(resourceHashPtr, resourceId);

    nameHashPtr = Tcl_CreateHashEntry(&nameTable, resourceId, &new);
    Tcl_SetHashValue(nameHashPtr, fileRef);

    /*
     * Place the system resource file into our cache.
     */
    fileRef = 0;
    resourceHashPtr = Tcl_CreateHashEntry(&resourceTable, (char *) fileRef,
	    &new);
    resourceId = (char *) ckalloc(strlen("system") + 1);
    sprintf(resourceId, "system");
    Tcl_SetHashValue(resourceHashPtr, resourceId);

    nameHashPtr = Tcl_CreateHashEntry(&nameTable, resourceId, &new);
    Tcl_SetHashValue(nameHashPtr, fileRef);
}
