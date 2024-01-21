/* 
 * tclMacChan.c
 *
 *	Channel drivers for Macintosh channels for the
 *	console fds.
 *
 * Copyright (c) 1996 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * SCCS: @(#) tclMacChan.c 1.35 96/05/30 14:20:57
 */

#include "tclInt.h"
#include "tclPort.h"
#include <Aliases.h>
#include <Errors.h>
#include <Files.h>
#include <Gestalt.h>
#include <Processes.h>
#include <Strings.h>

/*
 * Static routines for this file:
 */

static int		StdIOBlockMode _ANSI_ARGS_((ClientData instanceData,
			    int mode));
static int		StdIOClose _ANSI_ARGS_((ClientData instanceData,
			    Tcl_Interp *interp));
static Tcl_File		StdGetFile _ANSI_ARGS_((ClientData instanceData,
		            int direction));
static int		StdIOInput _ANSI_ARGS_((ClientData instanceData,
			    char *buf, int toRead, int *errorCode));
static int		StdIOOutput _ANSI_ARGS_((ClientData instanceData,
			    char *buf, int toWrite, int *errorCode));
static int		StdIOSeek _ANSI_ARGS_((ClientData instanceData,
			    long offset, int mode, int *errorCode));
static int		StdReady _ANSI_ARGS_((ClientData instanceData,
		            int mask));
static void		StdWatch _ANSI_ARGS_((ClientData instanceData,
		            int mask));

/*
 * This structure describes the channel type structure for file based IO:
 */

static Tcl_ChannelType consoleChannelType = {
    "file",			/* Type name. */
    StdIOBlockMode,		/* Set blocking/nonblocking mode.*/
    StdIOClose,			/* Close proc. */
    StdIOInput,			/* Input proc. */
    StdIOOutput,		/* Output proc. */
    StdIOSeek,			/* Seek proc. */
    NULL,			/* Set option proc. */
    NULL,			/* Get option proc. */
    StdWatch,			/* Initialize notifier. */
    StdReady,			/* Are there events? */
    StdGetFile			/* Get Tcl_Files out of channel. */
};

/*
 * Hack to allow Mac Tk to override the TclGetStdChannels function.
 */
 
typedef void (*TclGetStdChannelsProc) _ANSI_ARGS_((Tcl_Channel *stdinPtr,
	Tcl_Channel *stdoutPtr, Tcl_Channel *stderrPtr));
	
TclGetStdChannelsProc getStdChannelsProc = NULL;

/*
 * Static variables to hold channels for stdin, stdout and stderr.
 */

static Tcl_Channel stdinChannel = NULL;
static Tcl_Channel stdoutChannel = NULL;
static Tcl_Channel stderrChannel = NULL;

/*
 *----------------------------------------------------------------------
 *
 * StdIOBlockMode --
 *
 *	Set blocking or non-blocking mode on channel.
 *
 * Results:
 *	0 if successful, errno when failed.
 *
 * Side effects:
 *	Sets the device into blocking or non-blocking mode.
 *
 *----------------------------------------------------------------------
 */

static int
StdIOBlockMode(instanceData, mode)
    ClientData instanceData;		/* Unused. */
    int mode;				/* The mode to set. */
{
    /*
     * Do not allow putting stdin, stdout or stderr into nonblocking mode.
     */
    
    if (mode == TCL_MODE_NONBLOCKING) {
	return EFAULT;
    }
    
    return 0;
}

/*
 *----------------------------------------------------------------------
 *
 * StdIOClose --
 *
 *	Closes the IO channel.
 *
 * Results:
 *	0 if successful, the value of errno if failed.
 *
 * Side effects:
 *	Closes the physical channel
 *
 *----------------------------------------------------------------------
 */

static int
StdIOClose(instanceData, interp)
    ClientData instanceData;	/* Unused. */
    Tcl_Interp *interp;		/* Unused. */
{
    int fd, errorCode = 0;

    /*
     * Invalidate the stdio cache if necessary.  Note that we assume that
     * the stdio file and channel pointers will become invalid at the same
     * time.
     */

    fd = (int) instanceData;
    if (fd == 0) {
	fd = 0;
	stdinChannel = NULL;
    } else if (fd == 1) {
	stdoutChannel = NULL;
    } else if (fd == 2) {
	stderrChannel = NULL;
    } else {
	panic("recieved invalid std file");
    }

    if (close(fd) < 0) {
	errorCode = errno;
    }

    return errorCode;
}

/*
 *----------------------------------------------------------------------
 *
 * StdGetFile --
 *
 *	Called from Tcl_GetChannelFile to retrieve Tcl_Files from inside
 *	a file based channel.
 *
 * Results:
 *	The appropriate Tcl_File or NULL if not present. 
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static Tcl_File
StdGetFile(instanceData, direction)
    ClientData instanceData;		/* The file state. */
    int direction;			/* Which Tcl_File to retrieve? */
{
    if ((direction == TCL_READABLE) || (direction == TCL_WRITABLE)) {
	return (Tcl_File) instanceData;
    }
    return (Tcl_File) NULL;
}

/*
 *----------------------------------------------------------------------
 *
 * StdIOInput --
 *
 *	Reads input from the IO channel into the buffer given. Returns
 *	count of how many bytes were actually read, and an error indication.
 *
 * Results:
 *	A count of how many bytes were read is returned and an error
 *	indication is returned in an output argument.
 *
 * Side effects:
 *	Reads input from the actual channel.
 *
 *----------------------------------------------------------------------
 */

int
StdIOInput(instanceData, buf, bufSize, errorCode)
    ClientData instanceData;		/* Unused. */
    char *buf;				/* Where to store data read. */
    int bufSize;			/* How much space is available
                                         * in the buffer? */
    int *errorCode;			/* Where to store error code. */
{
    int fd;
    int bytesRead;			/* How many bytes were read? */

    *errorCode = 0;
    errno = 0;
    fd = (int) instanceData;
    bytesRead = read(fd, buf, (size_t) bufSize);
    if (bytesRead > -1) {
        return bytesRead;
    }
    *errorCode = errno;
    return -1;
}

/*
 *----------------------------------------------------------------------
 *
 * StdIOOutput--
 *
 *	Writes the given output on the IO channel. Returns count of how
 *	many characters were actually written, and an error indication.
 *
 * Results:
 *	A count of how many characters were written is returned and an
 *	error indication is returned in an output argument.
 *
 * Side effects:
 *	Writes output on the actual channel.
 *
 *----------------------------------------------------------------------
 */

static int
StdIOOutput(instanceData, buf, toWrite, errorCode)
    ClientData instanceData;		/* Unused. */
    char *buf;				/* The data buffer. */
    int toWrite;			/* How many bytes to write? */
    int *errorCode;			/* Where to store error code. */
{
    int written;
    int fd;

    *errorCode = 0;
    errno = 0;
    fd = (int) instanceData;
    written = write(fd, buf, (size_t) toWrite);
    if (written > -1) {
        return written;
    }
    *errorCode = errno;
    return -1;
}

/*
 *----------------------------------------------------------------------
 *
 * StdReady --
 *
 *	Called by the notifier to check whether events of interest are
 *	present on the channel.  On the Macintosh all files are always
 *	considered to be readable and writeable.
 *
 * Results:
 *	Returns OR-ed combination of TCL_READABLE, TCL_WRITABLE and
 *	TCL_EXCEPTION to indicate which events of interest are present.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static int
StdReady(instanceData, mask)
    ClientData instanceData;		/* The file state. */
    int mask;				/* Events of interest; an OR-ed
                                         * combination of TCL_READABLE,
                                         * TCL_WRITABLE and TCL_EXCEPTION. */
{
    return (TCL_READABLE | TCL_WRITABLE);
}

/*
 *----------------------------------------------------------------------
 *
 * StdIOSeek --
 *
 *	Seeks on an IO channel. Returns the new position.
 *
 * Results:
 *	-1 if failed, the new position if successful. If failed, it
 *	also sets *errorCodePtr to the error code.
 *
 * Side effects:
 *	Moves the location at which the channel will be accessed in
 *	future operations.
 *
 *----------------------------------------------------------------------
 */

static int
StdIOSeek(instanceData, offset, mode, errorCodePtr)
    ClientData instanceData;			/* Unused. */
    long offset;				/* Offset to seek to. */
    int mode;					/* Relative to where
                                                 * should we seek? */
    int *errorCodePtr;				/* To store error code. */
{
    int newLoc;
    int fd;

    *errorCodePtr = 0;
    fd = (int) instanceData;
    newLoc = lseek(fd, offset, mode);
    if (newLoc > -1) {
        return newLoc;
    }
    *errorCodePtr = errno;
    return -1;
}

/*
 *----------------------------------------------------------------------
 *
 * StdWatch --
 *
 *	Initialize the notifier to watch Tcl_Files from this channel.
 *	This doesn't do anything on the Macintosh.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static void
StdWatch(instanceData, mask)
    ClientData instanceData;		/* The file state. */
    int mask;				/* Events of interest; an OR-ed
                                         * combination of TCL_READABLE,
                                         * TCL_WRITABLE and TCL_EXCEPTION. */
{
    Tcl_Time timeout = { 0, 0 };

    /*
     * Currently, files are always ready under the Macintosh,
     * so we just set a 0 timeout.  Since there s no notification
     * scheme - we just set the timeout time to zero.
     */

    Tcl_SetMaxBlockTime(&timeout);
}

/*
 *----------------------------------------------------------------------
 *
 * TclGetAndDetachPids --
 *
 *	Stores a list of the command PIDs for a command channel in
 *	interp->result and detaches the PIDs.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Modifies interp->result.
 *
 *----------------------------------------------------------------------
 */

void
TclGetAndDetachPids(interp, chan)
    Tcl_Interp *interp;
    Tcl_Channel chan;
{
    return;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_PidCmd --
 *
 *	This procedure is invoked to process the "pid" Tcl command.
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
Tcl_PidCmd(dummy, interp, argc, argv)
    ClientData dummy;			/* Not used. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    ProcessSerialNumber psn;
    Tcl_Channel chan;

    if (argc > 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
		argv[0], " ?channelId?\"", (char *) NULL);
	return TCL_ERROR;
    }
    
    if (argc == 2) {
        chan = Tcl_GetChannel(interp, argv[1], NULL);
	if (chan == (Tcl_Channel) NULL) {
	    return TCL_ERROR;
	}
	
	/*
	 * We can't create pipelines on the Mac so
	 * this will always return an empty list.
	 */
	return TCL_OK;
    }
    
    GetCurrentProcess(&psn);
    sprintf(interp->result, "0x%08x%08x", psn.highLongOfPSN, psn.lowLongOfPSN);
    
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * TclGetDefaultStdChannel --
 *
 *	Constructs a channel for the specified standard OS handle.
 *
 * Results:
 *	Returns the specified default standard channel, or NULL.
 *
 * Side effects:
 *	May cause the creation of a standard channel and the underlying
 *	file.
 *
 *----------------------------------------------------------------------
 */

Tcl_Channel
TclGetDefaultStdChannel(type)
    int type;			/* One of TCL_STDIN, TCL_STDOUT, TCL_STDERR. */
{
    Tcl_Channel channel = NULL;
    int fd = 0;			/* Initializations needed to prevent */
    int mode = 0;		/* compiler warning (used before set). */
    char *bufMode = NULL;
    char channelName[20];
    int channelPermissions;

    /*
     * If the channels were not created yet, create them now and
     * store them in the static variables.
     */

    switch (type) {
	case TCL_STDIN:
	    fd = 0;
	    channelPermissions = TCL_READABLE;
	    bufMode = "line";
	    break;
	case TCL_STDOUT:
	    fd = 1;
	    channelPermissions = TCL_WRITABLE;
	    bufMode = "line";
	    break;
	case TCL_STDERR:
	    fd = 2;
	    channelPermissions = TCL_WRITABLE;
	    bufMode = "none";
	    break;
	default:
	    panic("TclGetDefaultStdChannel: Unexpected channel type");
	    break;
    }

    sprintf(channelName, "console%d", (int) fd);
    channel = Tcl_CreateChannel(&consoleChannelType, channelName,
	    (ClientData) fd, channelPermissions);
    /*
     * Set up the normal channel options for stdio handles.
     */

    Tcl_SetChannelOption(NULL, channel, "-translation", "cr");
    Tcl_SetChannelOption(NULL, channel, "-buffering", bufMode);
    
    return channel;
}
