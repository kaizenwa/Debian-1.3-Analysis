/*
 * tclXdup.c
 *
 * Extended Tcl dup command.
 *-----------------------------------------------------------------------------
 * Copyright 1991-1995 Karl Lehenbauer and Mark Diekhans.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies.  Karl Lehenbauer and
 * Mark Diekhans make no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without express or
 * implied warranty.
 *-----------------------------------------------------------------------------
 * $Id: tclXdup.c,v 5.0 1995/07/25 05:42:24 markd Rel $
 *-----------------------------------------------------------------------------
 */

#include "tclExtdInt.h"

/*
 * Prototypes of internal functions.
 */
static int
ConvertFileHandle _ANSI_ARGS_((Tcl_Interp *interp,
                               char       *handle));

static FILE *
DoNormalDup _ANSI_ARGS_((Tcl_Interp *interp,
                         OpenFile   *srcFilePtr));

static FILE *
DoSpecifiedDup _ANSI_ARGS_((Tcl_Interp *interp,
                            OpenFile   *srcFilePtr,
                            char       *newFileId));

static int
DupFileHandle _ANSI_ARGS_((Tcl_Interp *interp,
                           char       *srcFileId,
                           char       *targetFileId));

static int
BindFileNum _ANSI_ARGS_((Tcl_Interp *interp,
                         char       *fileNumStr));

/*
 *-----------------------------------------------------------------------------
 *
 * ConvertFileHandle --
 *
 * Convert a file handle to its file number. The file handle maybe one 
 * of "stdin", "stdout" or "stderr" or "fileNNN", were NNN is the file
 * number.  If the handle is invalid, -1 is returned and a error message
 * will be returned in interp->result.  This is used when the file may
 * not be currently open.
 *
 *-----------------------------------------------------------------------------
 */
static int
ConvertFileHandle (interp, handle)
    Tcl_Interp *interp;
    char       *handle;
{
    int fileId = -1;

    if (handle [0] == 's') {
        if (STREQU (handle, "stdin"))
            fileId = 0;
        else if (STREQU (handle, "stdout"))
            fileId = 1;
        else if (STREQU (handle, "stderr"))
            fileId = 2;
    } else {
       if (STRNEQU (handle, "file", 4))
           Tcl_StrToInt (&handle [4], 10, &fileId);
    }
    if (fileId < 0)
        Tcl_AppendResult (interp, "invalid file handle: ", handle,
                          (char *) NULL);
    return fileId;
}

/*
 *-----------------------------------------------------------------------------
 *
 * DoNormalDup --
 *   Process a normal dup command (i.e. the new file is not specified).
 *
 * Parameters:
 *   o interp (I) - If an error occures, the error message is in result.
 *   o srcFilePtr (I) - Tcl file control block for the file to dup.
 * Returns:
 *   A pointer to the file structure for the new file, or NULL if an
 * error occured.
 *-----------------------------------------------------------------------------
 */
static FILE *
DoNormalDup (interp, srcFilePtr)
    Tcl_Interp *interp;
    OpenFile   *srcFilePtr;
{
    int    newFileId, newFile2Id;
    FILE  *filePtr;

    newFileId = dup (fileno (srcFilePtr->f));
    if (newFileId < 0)
        goto unixError;

    if (srcFilePtr->f2 == NULL) {
        filePtr = Tcl_SetupFileEntry (interp, newFileId,
                                      srcFilePtr->permissions);
    } else {
        newFile2Id = dup (fileno (srcFilePtr->f2));
        if (newFile2Id < 0)
            goto unixError;
        filePtr = Tcl_SetupFileEntry2 (interp, newFileId, newFile2Id, NULL);
    }

    return filePtr;

unixError:
    interp->result = Tcl_PosixError (interp);
    if (newFileId >= 0)
        close (newFileId);
    return NULL;
}

/*
 *-----------------------------------------------------------------------------
 *
 * DoSpecifiedDup --
 *   Process a dup command where the file is dupped to a specified fileid.
 * The new file may or be open or closed, but its better if is open 
 * if stdin, stdout or stderr are being used, otherwise the a different
 * stdio file descriptior maybe bound to these descriptors.
 *
 * Parameters:
 *   o interp (I) - If an error occures, the error message is in result.
 *   o srcFilePtr (I) - Tcl file control block for the file to dup.
 *   o targetFileId (I) - The id (handle) name for the new file.
 * Returns:
 *   A pointer to the open structure for the new file, or NULL if an
 * error occured.
 *-----------------------------------------------------------------------------
 */
static FILE *
DoSpecifiedDup (interp, srcFilePtr, targetFileId)
    Tcl_Interp *interp;
    OpenFile   *srcFilePtr;
    char       *targetFileId;
{
    int       newFileNum = -1, newFile2Num = -1;
    FILE     *newFilePtr;

    /*
     * If the target file is currently open, close it.  Use the close command,
     * as this will handle cleaning up pipelines, etc. Also get the file
     * number for the file.  
     */
    if (Tcl_GetOpenFile (interp, targetFileId, 
                         FALSE, FALSE,  /* No checking */
                         &newFilePtr) != TCL_OK) {
        /*
         * Not open, must convert file number.
         */
        Tcl_ResetResult (interp);

        newFileNum = ConvertFileHandle (interp, targetFileId);
        if (newFileNum < 0)
            return NULL;
    } else {
        char *argv [2];

        newFileNum = fileno (newFilePtr);
        fflush (newFilePtr);

        argv [0] = "dup";
        argv [1] = targetFileId;
        if (Tcl_CloseCmd (NULL, interp, 2, argv) != TCL_OK)
            return NULL;
    }

    /*
     * Duplicate the src file to the specified file id.  This functionallity
     * could be obtained with dup2 on most systems.  If there are two FILE
     * structs associated with the src file, they must both be dupped.
     */
    close (newFileNum);
    if (fcntl (fileno (srcFilePtr->f), F_DUPFD, newFileNum) < 0)
        goto unixError;

    if (srcFilePtr->f2 != NULL) {
        newFile2Num = dup (fileno (srcFilePtr->f2));
        if (newFile2Num < 0)
            goto unixError;
    }

    /*
     * Set up the Tcl file descriptor.
     */
    if (newFile2Num >= 0) {
        newFilePtr = Tcl_SetupFileEntry2 (interp,
                                          newFileNum,
                                          newFile2Num,
                                          NULL);
    } else {
        newFilePtr = Tcl_SetupFileEntry (interp,
                                         newFileNum,
                                         srcFilePtr->permissions);
    }
    if (newFilePtr == NULL)
        goto unixError;

    return newFilePtr;

  unixError:
    interp->result = Tcl_PosixError (interp);
    if (newFileNum >= 0)
        Tcl_CloseForError (interp, newFileNum);
    if (newFile2Num >= 0)
        close (newFile2Num);
    return NULL;

}

/*
 *-----------------------------------------------------------------------------
 *
 * DupFileHandle --
 *   Duplicate a Tcl file handle.
 *
 * Parameters:
 *   o interp (I) - If an error occures, the error message is in result.
 *   o srcFileId (I) - The id  of the file to dup.
 *   o targetFileId (I) - The id for the new file.  NULL if any id maybe
 *     used.
 * Returns:
 *   TCL_OK or TCL_ERROR.
 *-----------------------------------------------------------------------------
 */
static int
DupFileHandle (interp, srcFileId, targetFileId)
    Tcl_Interp *interp;
    char       *srcFileId;
    char       *targetFileId;
{
    OpenFile *srcFilePtr;
    FILE     *newFilePtr;
    off_t     seekOffset = -1;

    srcFilePtr = Tcl_GetOpenFileStruct (interp, srcFileId);
    if (srcFilePtr == NULL)
	return TCL_ERROR;

    if (srcFilePtr->numPids > 0) {
        Tcl_AppendResult (interp, "can not \"dup\" a Tcl pipeline created ",
                          "with \"open\" the command, use \"pipe\", \"fork\" ",
                          "and \"execl\" instead.", (char *) NULL);
        return TCL_ERROR;
    }

    /*
     * If writable, flush out the buffer.  If readable, remember were we are
     * so the we can set it up for the next stdio read to come from the same
     * place.  The location is only recorded if the file is a reqular file,
     * since you cann't seek on other types of files.
     */
    if (srcFilePtr->permissions  & TCL_FILE_WRITABLE) {
        if (srcFilePtr->f2 != NULL) {
            if (fflush (srcFilePtr->f2) != 0)
                goto unixError;
        } else {
            if (fflush (srcFilePtr->f) != 0)
                goto unixError;
        }
    }
    if (srcFilePtr->permissions & TCL_FILE_READABLE) {
        struct stat statBuf;
        
        if (fstat (fileno (srcFilePtr->f), &statBuf) < 0)
            goto unixError;
        if ((statBuf.st_mode & S_IFMT) == S_IFREG) {
            seekOffset = ftell (srcFilePtr->f);
            if (seekOffset < 0)
                goto unixError;
        }
    }

    /*
     * Process the dup depending on if dup-ing to a new file or a target
     * file handle.
     */
    if (targetFileId != NULL)
        newFilePtr = DoSpecifiedDup (interp, srcFilePtr, targetFileId);
    else
        newFilePtr = DoNormalDup (interp, srcFilePtr);

    if (newFilePtr == NULL)
        return TCL_ERROR;

    if (seekOffset >= 0) {
        if (fseek (newFilePtr, seekOffset, SEEK_SET) != 0)
            goto unixError;
    }
    Tcl_ResetResult (interp);
    sprintf (interp->result, "file%d", fileno (newFilePtr));
    return TCL_OK;

unixError:
    Tcl_ResetResult (interp);
    interp->result = Tcl_PosixError (interp);
    return TCL_ERROR;
}

/*
 *-----------------------------------------------------------------------------
 *
 * BindFileNumber --
 *   Bind a file number of an open file to a Tcl file handle.
 *
 * Parameters:
 *   o interp (I) - If an error occures, the error message is in result.
 *   o fileNumStr (I) - The string number of the open file.
 * Returns:
 *   TCL_OK or TCL_ERROR.
 *-----------------------------------------------------------------------------
 */
static int
BindFileNumber (interp, fileNumStr)
    Tcl_Interp *interp;
    char       *fileNumStr;
{
    unsigned  fileNum;
    int       fileStat, perms;
    char     *mode;
    FILE     *filePtr;

    if (!Tcl_StrToUnsigned (fileNumStr, 0, &fileNum)) {
        Tcl_AppendResult (interp, "invalid integer file number \"", fileNumStr,
                          "\", expected unsigned integer or Tcl file id",
                          (char *) NULL);
        return TCL_ERROR;
    }

    if ((fileNum < tclNumFiles) && (tclOpenFiles [fileNum] != NULL)) {
        Tcl_AppendResult (interp, "file number \"", fileNumStr,
                          "\" is already bound to a Tcl file  id",
                          (char *) NULL);
        return TCL_ERROR;
    }

    /*
     * Make sure file is open and determine the access mode.
     */
    fileStat = fcntl (fileNum, F_GETFL, 0);
    if (fileStat == -1)
        goto unixError;

    switch (fileStat & O_ACCMODE) {
      case O_RDONLY:
        mode = "r";
        perms = TCL_FILE_READABLE;
        break;
      case O_WRONLY:
        mode = "w";
        perms = TCL_FILE_WRITABLE;
        break;
      case O_RDWR:
        mode = "r+";
        perms = TCL_FILE_READABLE | TCL_FILE_WRITABLE;
        break;
    }

    filePtr = fdopen (fileNum, mode);
    if (filePtr == NULL)
        goto unixError;

    Tcl_EnterFile (interp, filePtr, perms);
    return TCL_OK;

unixError:
    Tcl_ResetResult (interp);
    interp->result = Tcl_PosixError (interp);
    return TCL_ERROR;
}

/*
 *-----------------------------------------------------------------------------
 *
 * Tcl_DupCmd --
 *     Implements the dup TCL command:
 *         dup fileId ?targetFileId?
 *
 * Results:
 *      Returns TCL_OK and interp->result containing a filehandle
 *      if the requested file or pipe was successfully duplicated.
 *
 *      Return TCL_ERROR and interp->result containing an
 *      explanation of what went wrong if an error occured.
 *-----------------------------------------------------------------------------
 */
int
Tcl_DupCmd (clientData, interp, argc, argv)
    ClientData  clientData;
    Tcl_Interp *interp;
    int         argc;
    char      **argv;
{
    if ((argc < 2) || (argc > 3)) {
        Tcl_AppendResult (interp, tclXWrongArgs, argv[0], 
                          " fileId ?targetFileId?", (char *) NULL);
        return TCL_ERROR;
    }

    /*
     * If a number is supplied, bind it to a file handle rather than doing
     * a dup.
     */
    if (ISDIGIT (argv [1][0])) {
        if (argc != 2) {
            Tcl_AppendResult (interp, "the second argument, targetFileId, is ",
                              "not allow when binding a file number to a Tcl ",
                              "file id", (char *) NULL);
            return TCL_ERROR;
        }
        return BindFileNumber (interp, argv [1]);
    } else {
        return DupFileHandle (interp, argv [1], argv [2]);
    }
}
