/*
 * tclXfstat.c
 *
 * Extended Tcl fstat command.
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
 * $Id: tclXfstat.c,v 5.0 1995/07/25 05:59:10 markd Rel $
 *-----------------------------------------------------------------------------
 */
#include "tclExtdInt.h"

#ifndef NO_SYS_SOCKET_H

#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#endif

/*
 * Prototypes of internal functions.
 */
static int
GetHostInfo _ANSI_ARGS_((Tcl_Interp *interp,
                         FILE       *filePtr,
                         int         which));

static char *
GetFileType _ANSI_ARGS_((struct stat  *statBufPtr));

static void
ReturnStatList _ANSI_ARGS_((Tcl_Interp   *interp,
                            FILE         *filePtr,
                            struct stat  *statBufPtr));

static int
ReturnStatArray _ANSI_ARGS_((Tcl_Interp   *interp,
                             FILE         *filePtr,
                             struct stat  *statBufPtr,
                             char         *arrayName));

static int
ReturnStatItem _ANSI_ARGS_((Tcl_Interp   *interp,
                            FILE         *filePtr,
                            struct stat  *statBufPtr,
                            char         *itemName));

/*
 * Defines for GetHostInfo
 */
#define GETHOSTINFO_LOCAL 0
#define GETHOSTINFO_REMOTE 1

#ifndef NO_SYS_SOCKET_H

/*
 *-----------------------------------------------------------------------------
 *
 * GetHostInfo --
 *     Return a host address, name (if it can be obtained) and port number.
 *     
 * Parameters:
 *   o interp (O) - List is returned in the result.
 *   o filePtr (I) - Pointer to file.  Should be a socket connection.
 *   o which (I) -  GETHOSTINFO_LOCAL or GETHOSTINFO_REMOTE indicating if
 *     local or remote information should be returned.
 * Returns:
 *   TCL_OK or TCL_ERROR.
 *-----------------------------------------------------------------------------
 */
static int
GetHostInfo (interp, filePtr, which)
    Tcl_Interp *interp;
    FILE       *filePtr;
    int         which;
{
    int                 socketFD, nameLen;
    struct sockaddr_in  sockaddr;
    struct hostent     *hostEntry;
    char               *hostName;
    char                portText [32];

    socketFD = fileno (filePtr);
    nameLen = sizeof (sockaddr);

    if (which == GETHOSTINFO_REMOTE) {
        if (getpeername (socketFD, (struct sockaddr *) &sockaddr,
                         &nameLen) < 0)
            goto unixError;
    } else {
        if (getsockname (socketFD, (struct sockaddr *) &sockaddr,
                         &nameLen) < 0)
            goto unixError;
    }

    hostEntry = gethostbyaddr ((char *) &(sockaddr.sin_addr),
                               sizeof (sockaddr.sin_addr),
                               AF_INET);
    if (hostEntry != NULL)
        hostName = hostEntry->h_name;
    else
        hostName = "";

    Tcl_AppendElement (interp, inet_ntoa (sockaddr.sin_addr));

    Tcl_AppendElement (interp, hostName);

    sprintf (portText, "%u", ntohs (sockaddr.sin_port));
    Tcl_AppendElement (interp, portText);
       
    return TCL_OK;

  unixError:
    Tcl_ResetResult (interp);
    interp->result = Tcl_PosixError (interp);
    return TCL_ERROR;
}
#else

/*
 *-----------------------------------------------------------------------------
 *
 * GetHostInfo --
 *     Version of this functions that always returns an error on systems that
 * don't have sockets.
 *-----------------------------------------------------------------------------
 */
static int
GetHostInfo (interp, filePtr)
    Tcl_Interp *interp;
    FILE       *filePtr;
{
    interp->result = "sockets are not available on this system";
    return TCL_ERROR;
}
#endif /* NO_SYS_SOCKET_H */

/*
 *-----------------------------------------------------------------------------
 *
 * GetFileType --
 *
 *   Looks at stat mode and returns a text string indicating what type of
 * file it is.
 *
 * Parameters:
 *   o statBufPtr (I) - Pointer to a buffer initialized by stat or fstat.
 * Returns:
 *   A pointer static text string representing the type of the file.
 *-----------------------------------------------------------------------------
 */
static char *
GetFileType (statBufPtr)
    struct stat  *statBufPtr;
{
    char *typeStr;

    /*
     * Get a string representing the type of the file.
     */
    if (S_ISREG (statBufPtr->st_mode)) {
        typeStr = "file";
    } else if (S_ISDIR (statBufPtr->st_mode)) {
        typeStr = "directory";
    } else if (S_ISCHR (statBufPtr->st_mode)) {
        typeStr = "characterSpecial";
    } else if (S_ISBLK (statBufPtr->st_mode)) {
        typeStr = "blockSpecial";
    } else if (S_ISFIFO (statBufPtr->st_mode)) {
        typeStr = "fifo";
    } else if (S_ISLNK (statBufPtr->st_mode)) {
        typeStr = "link";
    } else if (S_ISSOCK (statBufPtr->st_mode)) {
        typeStr = "socket";
    } else {
        typeStr = "unknown";
    }

    return typeStr;
}

/*
 *-----------------------------------------------------------------------------
 *
 * ReturnStatList --
 *
 *   Return file stat infomation as a keyed list.
 *
 * Parameters:
 *   o interp (I) - The list is returned in result.
 *   o filePtr (I) - Pointer to the Tcl open file structure.
 *   o statBufPtr (I) - Pointer to a buffer initialized by stat or fstat.
 *-----------------------------------------------------------------------------
 */
static void
ReturnStatList (interp, filePtr, statBufPtr)
    Tcl_Interp   *interp;
    FILE         *filePtr;
    struct stat  *statBufPtr;
{
    char statList [200];

    sprintf (statList, 
             "{atime %ld} {ctime %ld} {dev %ld} {gid %ld} {ino %ld} {mode %ld} ",
              (long) statBufPtr->st_atime, (long) statBufPtr->st_ctime,
              (long) statBufPtr->st_dev,   (long) statBufPtr->st_gid,
              (long) statBufPtr->st_ino,   (long) statBufPtr->st_mode);
    Tcl_AppendResult (interp, statList, (char *) NULL);

    sprintf (statList, 
             "{mtime %ld} {nlink %ld} {size %ld} {uid %ld} {tty %d} {type %s}",
             (long) statBufPtr->st_mtime,  (long) statBufPtr->st_nlink,
             (long) statBufPtr->st_size,   (long) statBufPtr->st_uid,
             (int) isatty (fileno (filePtr)), GetFileType (statBufPtr));
    Tcl_AppendResult (interp, statList, (char *) NULL);

}

/*
 *-----------------------------------------------------------------------------
 *
 * ReturnStatArray --
 *
 *   Return file stat infomation in an array.
 *
 * Parameters:
 *   o interp (I) - Current interpreter, error return in result.
 *   o filePtr (I) - Pointer to the Tcl open file structure.
 *   o statBufPtr (I) - Pointer to a buffer initialized by stat or fstat.
 *   o arrayName (I) - The name of the array to return the info in.
 * Returns:
 *   TCL_OK or TCL_ERROR.
 *-----------------------------------------------------------------------------
 */
static int
ReturnStatArray (interp, filePtr, statBufPtr, arrayName)
    Tcl_Interp   *interp;
    FILE         *filePtr;
    struct stat  *statBufPtr;
    char         *arrayName;
{
    char numBuf [30];

    sprintf (numBuf, "%ld", (long) statBufPtr->st_dev);
    if  (Tcl_SetVar2 (interp, arrayName, "dev", numBuf, 
                      TCL_LEAVE_ERR_MSG) == NULL)
        return TCL_ERROR;

    sprintf (numBuf, "%ld", (long) statBufPtr->st_ino);
    if  (Tcl_SetVar2 (interp, arrayName, "ino", numBuf,
                         TCL_LEAVE_ERR_MSG) == NULL)
        return TCL_ERROR;

    sprintf (numBuf, "%ld", (long) statBufPtr->st_mode);
    if  (Tcl_SetVar2 (interp, arrayName, "mode", numBuf, 
                      TCL_LEAVE_ERR_MSG) == NULL)
        return TCL_ERROR;

    sprintf (numBuf, "%ld", (long) statBufPtr->st_nlink);
    if  (Tcl_SetVar2 (interp, arrayName, "nlink", numBuf,
                      TCL_LEAVE_ERR_MSG) == NULL)
        return TCL_ERROR;

    sprintf (numBuf, "%ld", (long) statBufPtr->st_uid);
    if  (Tcl_SetVar2 (interp, arrayName, "uid", numBuf,
                      TCL_LEAVE_ERR_MSG) == NULL)
        return TCL_ERROR;

    sprintf (numBuf, "%ld", (long) statBufPtr->st_gid);
    if  (Tcl_SetVar2 (interp, arrayName, "gid", numBuf,
                      TCL_LEAVE_ERR_MSG) == NULL)
        return TCL_ERROR;

    sprintf (numBuf, "%ld", (long) statBufPtr->st_size);
    if  (Tcl_SetVar2 (interp, arrayName, "size", numBuf,
                      TCL_LEAVE_ERR_MSG) == NULL)
        return TCL_ERROR;

    sprintf (numBuf, "%ld", (long) statBufPtr->st_atime);
    if  (Tcl_SetVar2 (interp, arrayName, "atime", numBuf,
                      TCL_LEAVE_ERR_MSG) == NULL)
        return TCL_ERROR;

    sprintf (numBuf, "%ld", (long) statBufPtr->st_mtime);
    if  (Tcl_SetVar2 (interp, arrayName, "mtime", numBuf,
                      TCL_LEAVE_ERR_MSG) == NULL)
        return TCL_ERROR;

    sprintf (numBuf, "%ld", (long) statBufPtr->st_ctime);
    if  (Tcl_SetVar2 (interp, arrayName, "ctime", numBuf,
                      TCL_LEAVE_ERR_MSG) == NULL)
        return TCL_ERROR;

    if (Tcl_SetVar2 (interp, arrayName, "tty", 
                     isatty (fileno (filePtr)) ? "1" : "0",
                     TCL_LEAVE_ERR_MSG) == NULL)
        return TCL_ERROR;

    if (Tcl_SetVar2 (interp, arrayName, "type", GetFileType (statBufPtr),
                     TCL_LEAVE_ERR_MSG) == NULL)
        return TCL_ERROR;

    return TCL_OK;

}

/*
 *-----------------------------------------------------------------------------
 *
 * ReturnStatItem --
 *
 *   Return a single file status item.
 *
 * Parameters:
 *   o interp (I) - Item or error returned in result.
 *   o filePtr (I) - Pointer to the Tcl open file structure.
 *   o statBufPtr (I) - Pointer to a buffer initialized by stat or fstat.
 *   o itemName (I) - The name of the desired item.
 * Returns:
 *   TCL_OK or TCL_ERROR.
 *-----------------------------------------------------------------------------
 */
static int
ReturnStatItem (interp, filePtr, statBufPtr, itemName)
    Tcl_Interp   *interp;
    FILE         *filePtr;
    struct stat  *statBufPtr;
    char         *itemName;
{
    if (STREQU (itemName, "dev"))
        sprintf (interp->result, "%ld", (long) statBufPtr->st_dev);
    else if (STREQU (itemName, "ino"))
        sprintf (interp->result, "%ld", (long) statBufPtr->st_ino);
    else if (STREQU (itemName, "mode"))
        sprintf (interp->result, "%ld", (long) statBufPtr->st_mode);
    else if (STREQU (itemName, "nlink"))
        sprintf (interp->result, "%ld", (long) statBufPtr->st_nlink);
    else if (STREQU (itemName, "uid"))
        sprintf (interp->result, "%ld", (long) statBufPtr->st_uid);
    else if (STREQU (itemName, "gid"))
        sprintf (interp->result, "%ld", (long) statBufPtr->st_gid);
    else if (STREQU (itemName, "size"))
        sprintf (interp->result, "%ld", (long) statBufPtr->st_size);
    else if (STREQU (itemName, "atime"))
        sprintf (interp->result, "%ld", (long) statBufPtr->st_atime);
    else if (STREQU (itemName, "mtime"))
        sprintf (interp->result, "%ld", (long) statBufPtr->st_mtime);
    else if (STREQU (itemName, "ctime"))
        sprintf (interp->result, "%ld", (long) statBufPtr->st_ctime);
    else if (STREQU (itemName, "type"))
        interp->result = GetFileType (statBufPtr);
    else if (STREQU (itemName, "tty"))
        interp->result = isatty (fileno (filePtr)) ? "1" : "0";
    else if (STREQU (itemName, "remotehost")) {
        if (GetHostInfo (interp, filePtr, GETHOSTINFO_REMOTE) != TCL_OK)
            return TCL_ERROR;
    } else if (STREQU (itemName, "localhost")) {
        if (GetHostInfo (interp, filePtr, GETHOSTINFO_LOCAL) != TCL_OK)
            return TCL_ERROR;
    } else {
        Tcl_AppendResult (interp, "Got \"", itemName, "\", expected one of ",
                          "\"atime\", \"ctime\", \"dev\", \"gid\", \"ino\", ",
                          "\"mode\", \"mtime\", \"nlink\", \"size\", ",
                          "\"tty\", \"type\", \"uid\", \"remotehost\", or ",
                          "\"localhost\"", (char *) NULL);
        return TCL_ERROR;
    }

    return TCL_OK;

}

/*
 *-----------------------------------------------------------------------------
 *
 * Tcl_FstatCmd --
 *     Implements the fstat TCL command:
 *         fstat fileId ?item?|?stat arrayvar?
 *
 *-----------------------------------------------------------------------------
 */
int
Tcl_FstatCmd (clientData, interp, argc, argv)
    ClientData  clientData;
    Tcl_Interp *interp;
    int         argc;
    char      **argv;
{
    FILE         *filePtr;
    struct stat  statBuf;

    if ((argc < 2) || (argc > 4)) {
        Tcl_AppendResult (interp, tclXWrongArgs, argv [0], 
                          " fileId ?item?|?stat arrayVar?", (char *) NULL);
        return TCL_ERROR;
    }

    if (Tcl_GetOpenFile (interp, argv[1],
                         FALSE, FALSE,  /* No checking */
                         &filePtr) != TCL_OK)
        return TCL_ERROR;
    
    if (fstat (fileno (filePtr), &statBuf)) {
        interp->result = Tcl_PosixError (interp);
        return TCL_ERROR;
    }

    /*
     * Return data in the requested format.
     */
    if (argc == 4) {
        if (!STREQU (argv [2], "stat")) {
            Tcl_AppendResult (interp, "expected item name of \"stat\" when ",
                              "using array name", (char *) NULL);
            return TCL_ERROR;
        }
        return ReturnStatArray (interp, filePtr, &statBuf, argv [3]);
    }
    if (argc == 3)
        return ReturnStatItem (interp, filePtr, &statBuf, argv [2]);

    ReturnStatList (interp, filePtr, &statBuf);
    return TCL_OK;

}
