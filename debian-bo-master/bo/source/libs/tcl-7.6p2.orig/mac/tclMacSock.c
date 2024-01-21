/* 
 * tclMacSock.c
 *
 *	Channel drivers for Macintosh sockets.
 *
 * Copyright (c) 1996-1997 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * SCCS: @(#) tclMacSock.c 1.36 97/01/21 14:14:21
 */

#include "tclInt.h"
#include "tclPort.h"
#include "tclMacInt.h"
#include <AddressXlation.h>
#include <Aliases.h>
#include <Devices.h>
#include <Errors.h>
#include <Events.h>
#include <Files.h>
#include <Gestalt.h>
#include <MacTCP.h>
#include <Processes.h>
#include <Strings.h>

/*
 * If debugging is on we may drop into the debugger to handle certain cases
 * that are not supposed to happen.  Otherwise, we change ignore the error
 * and most code should handle such errors ok.
 */
#ifndef TCL_DEBUG
    #define Debugger()
#endif

/*
 * This is the size of the channel name for File based channels
 */

#define CHANNEL_NAME_SIZE	64
static char channelName[CHANNEL_NAME_SIZE+1];

/*
 * The preferred buffer size for Macintosh channels.
 */

#define CHANNEL_BUF_SIZE	8192

/*
 * Port information structure.  Used to match service names
 * to a Tcp/Ip port number.
 */
typedef struct {
    char *name;			/* Name of service. */
    int port;			/* Port number. */
} PortInfo;

/*
 * This structure describes per-instance state of a tcp based channel.
 */

typedef struct TcpState {
    TCPiopb pb;			   /* Parameter block used by this stream. 
				      This must be in the first position. */
    ProcessSerialNumber	psn;	   /* PSN used to wake up process. */
    StreamPtr tcpStream;	   /* Macintosh tcp stream pointer. */
    int port;			   /* The port we are connected to. */
    int flags;			   /* Bit field comprised of the flags
				    * described below.  */
    int checkMask;		   /* OR'ed combination of TCL_READABLE and
				    * TCL_WRITABLE as set by an asynchronous
				    * event handler. */
    int watchMask;		   /* OR'ed combination of TCL_READABLE and
				    * TCL_WRITABLE as set by Tcl_WatchFile. */
    Tcl_File sock;		   /* The file handle for the socket. */
    Tcl_TcpAcceptProc *acceptProc; /* Proc to call on accept. */
    ClientData acceptProcData;	   /* The data for the accept proc. */
    rdsEntry rdsarray[5+1];	   /* Array used when cleaning out recieve 
				    * buffers on a closing socket. */
    struct TcpState *nextPtr;	   /* The next socket on the global socket
				    * list. */
} TcpState;

/*
 * This structure is used by domain name resolver callback.
 */

typedef struct DNRState {
    struct hostInfo hostInfo;	/* Data structure used by DNR functions. */
    int done;			/* Flag to determine when we are done. */
    ProcessSerialNumber psn;	/* Process to wake up when we are done. */
} DNRState;

/*
 * The following macros may be used to set the flags field of
 * a TcpState structure.
 */

#define TCP_ASYNC_SOCKET	(1<<0)  /* The socket is in async mode. */
#define TCP_ASYNC_CONNECT	(1<<1)  /* The socket is trying to connect. */
#define TCP_CONNECTED		(1<<2)  /* The socket is connected. */
#define TCP_WATCH 		(1<<3)  /* TclMacWatchSocket has been called 
					 * since thelast time we entered
					 * Tcl_WaitForEvent. */
#define TCP_LISTENING 		(1<<4)  /* This socket is listening for
					 * a connection. */
#define TCP_LISTEN_CONNECT 	(1<<5)  /* Someone has connect to the
					 * listening port. */
#define TCP_REMOTE_CLOSED 	(1<<6)  /* The remote side has closed
					 * the connection. */
#define TCP_RELEASE	 	(1<<7)  /* The socket may now be released. */

/*
 * Static routines for this file:
 */

static pascal void	CleanUpExitProc _ANSI_ARGS_((void));
static void		CloseCompletionRoutine _ANSI_ARGS_((TCPiopb *pb));
static TcpState *	CreateSocket _ANSI_ARGS_((Tcl_Interp *interp,
			    int port, char *host, char *myAddr,  int myPort,
			    int server, int async));
static pascal void	DNRCompletionRoutine _ANSI_ARGS_((
			    struct hostInfo *hostinfoPtr,
			    DNRState *dnrStatePtr));
static void		FreeSocketInfo _ANSI_ARGS_((TcpState *statePtr));
static long		GetBufferSize _ANSI_ARGS_((void));
static OSErr		GetHostFromString _ANSI_ARGS_((char *name,
			    ip_addr *address));
static OSErr		GetLocalAddress _ANSI_ARGS_((unsigned long *addr));
static void		IOCompletionRoutine _ANSI_ARGS_((TCPiopb *pb));
static void		InitMacTCPParamBlock _ANSI_ARGS_((TCPiopb *pBlock,
			    int csCode));
static int		InitSockets _ANSI_ARGS_((void));
static TcpState *	NewSocketInfo _ANSI_ARGS_((Tcl_File file));
static OSErr		ResolveAddress _ANSI_ARGS_((ip_addr tcpAddress,
			    Tcl_DString *dsPtr));
static void		SocketFreeProc _ANSI_ARGS_((ClientData clientData));
static void		TcpAccept _ANSI_ARGS_((ClientData data, int mask));
static int		TcpBlockMode _ANSI_ARGS_((ClientData instanceData, int mode));
static int		TcpClose _ANSI_ARGS_((ClientData instanceData,
			    Tcl_Interp *interp));
static Tcl_File		TcpGetFile _ANSI_ARGS_((ClientData instanceData,
		            int direction));
static int		TcpGetOptionProc _ANSI_ARGS_((ClientData instanceData,
                            char *optionName, Tcl_DString *dsPtr));
static int		TcpInput _ANSI_ARGS_((ClientData instanceData,
			    char *buf, int toRead, int *errorCode));
static int		TcpOutput _ANSI_ARGS_((ClientData instanceData,
			    char *buf, int toWrite, int *errorCode));
static int		TcpReady _ANSI_ARGS_((ClientData instanceData,
		            int mask));
static void		TcpWatch _ANSI_ARGS_((ClientData instanceData,
		            int mask));

/*
 * This structure describes the channel type structure for TCP socket
 * based IO:
 */

static Tcl_ChannelType tcpChannelType = {
    "tcp",			/* Type name. */
    TcpBlockMode,		/* Set blocking or
                                 * non-blocking mode.*/
    TcpClose,			/* Close proc. */
    TcpInput,			/* Input proc. */
    TcpOutput,			/* Output proc. */
    NULL,			/* Seek proc. */
    NULL,			/* Set option proc. */
    TcpGetOptionProc,		/* Get option proc. */
    TcpWatch,			/* Initialize notifier. */
    TcpReady,			/* Are there events? */
    TcpGetFile			/* Get Tcl_Files out of channel. */
};

/*
 * Universal Procedure Pointers (UPP) for various callback
 * routines used by MacTcp code.
 */
ResultUPP resultUPP = NULL;
TCPIOCompletionUPP completeUPP = NULL;
TCPIOCompletionUPP closeUPP = NULL;

/*
 * Built-in commands, and the procedures associated with them:
 */

static PortInfo portServices[] = {
    {"echo",		7},
    {"discard",		9},
    {"systat",		11},
    {"daytime",		13},
    {"netstat",		15},
    {"chargen",		19},
    {"ftp-data",	20},
    {"ftp",		21},
    {"telnet",		23},
    {"telneto",		24},
    {"smtp",		25},
    {"time",		37},
    {"whois",		43},
    {"domain",		53},
    {"gopher",		70},
    {"finger",		79},
    {"hostnames",	101},
    {"sunrpc",		111},
    {"nntp",		119},
    {"exec",		512},
    {"login",		513},
    {"shell",		514},
    {"printer",		515},
    {"courier",		530},
    {"uucp",		540},
    {NULL,		0},
};

/*
 * Every open socket has an entry on the following list.
 */

static TcpState *socketList = NULL;

/*
 * Globals for holding information about OS support for sockets.
 */
static int socketsTestInited = false;
static int hasSockets = false;
static int socketsInitalized = false;
static short driverRefNum = 0;
static int socketNumber = 0;
static int socketBufferSize = CHANNEL_BUF_SIZE;
static ProcessSerialNumber applicationPSN;

/*
 *----------------------------------------------------------------------
 *
 * InitMacTCPParamBlock--
 *
 *	Initialize a MacTCP parameter block.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Initializes the parameter block.
 *
 *----------------------------------------------------------------------
 */

static void
InitMacTCPParamBlock(
    TCPiopb *pBlock,		/* Tcp parmeter block. */
    int csCode)			/* Tcp operation code. */
{
    memset(pBlock, 0, sizeof(TCPiopb));
    pBlock->ioResult = 1;
    pBlock->ioCRefNum = driverRefNum;
    pBlock->csCode = (short) csCode;
}

/*
 *----------------------------------------------------------------------
 *
 * TcpBlockMode --
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
TcpBlockMode(
    ClientData instanceData, 		/* Channel state. */
    int mode)				/* The mode to set. */
{
    TcpState *statePtr = (TcpState *) instanceData;
    
    if (mode == TCL_MODE_BLOCKING) {
	statePtr->flags |= TCP_ASYNC_SOCKET;
    } else {
	statePtr->flags &= ~TCP_ASYNC_SOCKET;
    }
    return 0;
}

/*
 *----------------------------------------------------------------------
 *
 * TcpClose --
 *
 *	Close the socket.
 *
 * Results:
 *	0 if successful, the value of errno if failed.
 *
 * Side effects:
 *	Closes the socket.
 *
 *----------------------------------------------------------------------
 */

static int
TcpClose(
    ClientData instanceData,		/* The socket to close. */
    Tcl_Interp *interp)			/* Interp for error messages. */
{
    TcpState *statePtr = (TcpState *) instanceData;
    int errorCode = 0, done = false;
    StreamPtr tcpStream;
    OSErr err;

    tcpStream = statePtr->tcpStream;
    statePtr->flags &= ~TCP_CONNECTED;

    InitMacTCPParamBlock(&statePtr->pb, TCPClose);
    statePtr->pb.tcpStream = tcpStream;
    statePtr->pb.ioCompletion = closeUPP; 
    statePtr->pb.csParam.close.userDataPtr = (Ptr) statePtr;
    err = PBControlAsync((ParmBlkPtr) &statePtr->pb);
    if (err != noErr) {
	statePtr->flags |= TCP_RELEASE;
        return errorCode;
    }

    /*
     * Delete a file handler that may be active for this socket.
     * Channel handlers are already deleted in the generic IO close
     * code which called this function.
     */
    
    Tcl_DeleteFileHandler(statePtr->sock);

    /*
     * Free the file handle.  As a side effect, this will call the
     * SocketFreeProc to release the SocketInfo associated with this file.
     */

    Tcl_FreeFile(statePtr->sock);
    
    return errorCode;
}

/*
 *----------------------------------------------------------------------
 *
 * CloseCompletionRoutine --
 *
 *	Handles the close protocol for a Tcp socket.  This will do
 *	a series of calls to release all data currently buffered for
 *	the socket.  This is important to do to as it allows the remote
 *	connection to recieve and issue it's own close on the socket.
 *	Note that this function is running at interupt time and can't
 *	allocate memory or do much else except set state.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The buffers for the socket are flushed.
 *
 *----------------------------------------------------------------------
 */

static void
CloseCompletionRoutine(
    TCPiopb *pbPtr)		/* Tcp parameter block. */
{
    TcpState *statePtr;
    OSErr err;
    
    statePtr = (TcpState *) pbPtr;
    
    /*
     * If there is an error we assume the remote side has already
     * close.  We are done closing as soon as we decide that the
     * remote connection has closed.
     */
    if (pbPtr->ioResult != noErr) {
	statePtr->flags |= TCP_RELEASE;
	return;
    }
    
    if (statePtr->flags & TCP_REMOTE_CLOSED) {
	statePtr->flags |= TCP_RELEASE;
	return;
    }
    
    /*
     * If we just did a recieve we need to return the buffers.
     * Otherwise, attempt to recieve more data until we recieve an
     * error.
     */
    if (statePtr->pb.csCode == TCPNoCopyRcv) {
	InitMacTCPParamBlock(&statePtr->pb, TCPRcvBfrReturn);
	statePtr->pb.ioCompletion = closeUPP; 
	statePtr->pb.csParam.receive.rdsPtr = (Ptr) statePtr->rdsarray;
	err = PBControlAsync((ParmBlkPtr) &statePtr->pb);
    } else {
	InitMacTCPParamBlock(&statePtr->pb, TCPNoCopyRcv);
	statePtr->pb.ioCompletion = closeUPP; 
	statePtr->pb.csParam.receive.commandTimeoutValue = 1;
	statePtr->pb.csParam.receive.rdsPtr = (Ptr) statePtr->rdsarray;
	statePtr->pb.csParam.receive.rdsLength = 5;
	err = PBControlAsync((ParmBlkPtr) &statePtr->pb);
    }

    if (err != noErr) {
	statePtr->flags |= TCP_RELEASE;
	return;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * TcpInput --
 *
 *	Reads input from the IO channel into the buffer given. Returns
 *	count of how many bytes were actually read, and an error 
 *	indication.
 *
 * Results:
 *	A count of how many bytes were read is returned.  A value of -1
 *	implies an error occured.  A value of zero means we have reached
 *	the end of data (EOF).
 *
 * Side effects:
 *	Reads input from the actual channel.
 *
 *----------------------------------------------------------------------
 */

int
TcpInput(
    ClientData instanceData,		/* Channel state. */
    char *buf, 				/* Where to store data read. */
    int bufSize, 			/* How much space is available
                                         * in the buffer? */
    int *errorCode)			/* Where to store error code. */
{
    TcpState *statePtr = (TcpState *) instanceData;
    StreamPtr tcpStream;
    OSErr err;
    int timeOut, mask;

    *errorCode = 0;
    errno = 0;
    tcpStream = statePtr->tcpStream;

    if (bufSize == 0) {
        return 0;
    }

    /*
     * If an asynchronous connect is in progress, attempt to wait for it
     * to complete before reading.
     */
    
    if (statePtr->flags & TCP_ASYNC_CONNECT) {
        if (statePtr->flags & TCP_ASYNC_SOCKET) {
            timeOut = 0;
        } else {
            timeOut = -1;
        }
        mask = TclWaitForFile(statePtr->sock, TCL_WRITABLE, timeOut);
        if (mask & TCL_WRITABLE) {
            statePtr->flags &= (~(TCP_ASYNC_CONNECT));
        } else if (timeOut == 0) {
            *errorCode = errno = EWOULDBLOCK;
            return -1;
        }
    }
            
    statePtr->pb.ioCRefNum = driverRefNum;
    statePtr->pb.tcpStream = tcpStream;
    statePtr->pb.csCode = TCPStatus;
    err = PBControlSync((ParmBlkPtr) &(statePtr->pb));
    if (err != noErr) {
	Debugger();
	statePtr->flags |= TCP_REMOTE_CLOSED;
	return 0;	/* EOF */
    }
    if (statePtr->pb.csParam.status.amtUnreadData < bufSize) {
	bufSize = statePtr->pb.csParam.status.amtUnreadData;
    }
    
    /* EWOULDBLOCK ??? */
    if (bufSize == 0) {
	SInt8 connectionState = statePtr->pb.csParam.status.connectionState;
	if (connectionState == 14) {
	    statePtr->flags |= TCP_REMOTE_CLOSED;
	    return 0;
	}
	if (connectionState != 8) {
	    Debugger();
	}
	*errorCode = EWOULDBLOCK;
	statePtr->checkMask &= ~TCL_READABLE;
	return -1;
    }
    
    InitMacTCPParamBlock(&statePtr->pb, TCPRcv);
    statePtr->pb.tcpStream = tcpStream;
    statePtr->pb.csParam.receive.rcvBuff = buf;
    statePtr->pb.csParam.receive.rcvBuffLen = bufSize;
    err = PBControlSync((ParmBlkPtr) &(statePtr->pb));
    switch (err) {
	case noErr:
	    return statePtr->pb.csParam.receive.rcvBuffLen;
	case connectionClosing:
	    *errorCode = errno = ESHUTDOWN;
	    statePtr->flags |= TCP_REMOTE_CLOSED;
	    return 0;
	case connectionDoesntExist:
	case connectionTerminated:
	    *errorCode = errno = ENOTCONN;
	    statePtr->flags |= TCP_REMOTE_CLOSED;
	    return 0;
	case invalidStreamPtr:
	default:
	    return -1;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * TcpGetFile --
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
TcpGetFile(instanceData, direction)
    ClientData instanceData;		/* The file state. */
    int direction;			/* Which Tcl_File to retrieve? */
{
    TcpState *statePtr = (TcpState *) instanceData;

    if ((direction == TCL_READABLE) || (direction == TCL_WRITABLE)) {
	return statePtr->sock;
    }
    return (Tcl_File) NULL;
}

/*
 *----------------------------------------------------------------------
 *
 * TcpOutput--
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
TcpOutput(
    ClientData instanceData, 		/* Channel state. */
    char *buf, 				/* The data buffer. */
    int toWrite, 			/* How many bytes to write? */
    int *errorCode)			/* Where to store error code. */
{
    TcpState *statePtr = (TcpState *) instanceData;
    StreamPtr tcpStream;
    OSErr err;
    int amount;
    wdsEntry dataSegment[2];
    int timeOut, mask;

    *errorCode = 0;
    tcpStream = statePtr->tcpStream;

    /*
     * If an asynchronous connect is in progress, attempt to wait for it
     * to complete before reading.
     */
    
    if (statePtr->flags & TCP_ASYNC_CONNECT) {
        if (statePtr->flags & TCP_ASYNC_SOCKET) {
            timeOut = 0;
        } else {
            timeOut = -1;
        }
        mask = TclWaitForFile(statePtr->sock, TCL_WRITABLE, timeOut);
        if (mask & TCL_WRITABLE) {
            statePtr->flags &= (~(TCP_ASYNC_CONNECT));
        } else if (timeOut == 0) {
            *errorCode = EWOULDBLOCK;
	    Tcl_SetErrno(EWOULDBLOCK);
            return -1;
        }
    }
        
    statePtr->pb.ioCRefNum = driverRefNum;
    statePtr->pb.tcpStream = tcpStream;
    statePtr->pb.csCode = TCPStatus;
    err = PBControlSync((ParmBlkPtr) &(statePtr->pb));
    if (err != noErr) {
	return -1;
    }
    amount = statePtr->pb.csParam.status.sendWindow - 
		statePtr->pb.csParam.status.amtUnackedData;
    if (amount <= 0) {
	statePtr->checkMask &= ~TCL_WRITABLE;
	*errorCode = EWOULDBLOCK;
	return -1;
    } else if (toWrite < amount) {
	amount = toWrite;
    }
    
    dataSegment[0].length = amount;
    dataSegment[0].ptr = buf;
    dataSegment[1].length = 0;
    InitMacTCPParamBlock(&statePtr->pb, TCPSend);
    statePtr->pb.tcpStream = tcpStream;
    statePtr->pb.csParam.send.wdsPtr = (Ptr) dataSegment;
    statePtr->pb.csParam.send.pushFlag = 1;
    err = PBControlSync((ParmBlkPtr) &(statePtr->pb));
    switch (err) {
	case noErr:
	    return amount;
	case connectionClosing:
	    *errorCode = errno = ESHUTDOWN;
	    statePtr->flags |= TCP_REMOTE_CLOSED;
	    return -1;
	case connectionDoesntExist:
	case connectionTerminated:
	    *errorCode = errno = ENOTCONN;
	    statePtr->flags |= TCP_REMOTE_CLOSED;
	    return -1;
	case invalidStreamPtr:
	default:
	    return -1;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * TcpGetOptionProc --
 *
 *	Computes an option value for a TCP socket based channel, or a
 *	list of all options and their values.
 *
 *	Note: This code is based on code contributed by John Haxby.
 *
 * Results:
 *	A standard Tcl result. The value of the specified option or a
 *	list of all options and	their values is returned in the
 *	supplied DString.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static int
TcpGetOptionProc(
    ClientData instanceData, 		/* Socket state. */
    char *optionName, 			/* Name of the option to
                                         * retrieve the value for, or
                                         * NULL to get all options and
                                         * their values. */
    Tcl_DString *dsPtr)			/* Where to store the computed
                                         * value; initialized by caller. */
{
    TcpState *statePtr = (TcpState *) instanceData;
    int doPeerName = false, doSockName = false, doAll = false;
    ip_addr tcpAddress;
    char buffer[128];
    OSErr err;
    Tcl_DString dString;
    int timeOut, mask;

    /*
     * If an asynchronous connect is in progress, attempt to wait for it
     * to complete before reading.
     */
    
    if (statePtr->flags & TCP_ASYNC_CONNECT) {
        if (statePtr->flags & TCP_ASYNC_SOCKET) {
            timeOut = 0;
        } else {
            timeOut = -1;
        }
        mask = TclWaitForFile(statePtr->sock, TCL_WRITABLE, timeOut);
        if (mask & TCL_WRITABLE) {
            statePtr->flags &= (~(TCP_ASYNC_CONNECT));
        } else if (timeOut == 0) {
	    Tcl_SetErrno(EWOULDBLOCK);
            return -1;
        }
    }
            
    /*
     * Determine which options we need to do.  Do all of them
     * if optionName is NULL.
     */
    if (optionName == (char *) NULL || optionName[0] == '\0') {
        doAll = true;
    } else {
	if (!strcmp(optionName, "-peername")) {
	    doPeerName = true;
	} else if (!strcmp(optionName, "-sockname")) {
	    doSockName = true;
	} else {
	    Tcl_SetErrno(EINVAL);
	    return TCL_ERROR;
	}
    }

    /*
     * Get status on the stream.
     */
    statePtr->pb.ioCRefNum = driverRefNum;
    statePtr->pb.tcpStream = statePtr->tcpStream;
    statePtr->pb.csCode = TCPStatus;
    err = PBControlSync((ParmBlkPtr) &(statePtr->pb));
    if (err != noErr) {
	Debugger();
	return TCL_ERROR;
    }

    Tcl_DStringInit(&dString);
    /*
     * Get the sockname for the socket.
     */
    if (doAll || doSockName) {
	if (doAll) {
	    Tcl_DStringAppendElement(dsPtr, "-sockname");
	    Tcl_DStringStartSublist(dsPtr);
	}
	tcpAddress = statePtr->pb.csParam.status.localHost;
	sprintf(buffer, "%d.%d.%d.%d", tcpAddress>>24,
		tcpAddress>>16 & 0xff, tcpAddress>>8 & 0xff,
		tcpAddress & 0xff);
	Tcl_DStringAppendElement(dsPtr, buffer);
	if (ResolveAddress(tcpAddress, &dString) == noErr) {
	    Tcl_DStringAppendElement(dsPtr, dString.string);
	} else {
	    Tcl_DStringAppendElement(dsPtr, "<unknown>");
	}
	sprintf(buffer, "%d", statePtr->pb.csParam.status.localPort);
	Tcl_DStringAppendElement(dsPtr, buffer);
	if (doAll) {
	    Tcl_DStringEndSublist(dsPtr);
	}
    }

    /*
     * Get the peername for the socket.
     */
    if ((doAll || doPeerName) && (statePtr->flags & TCP_CONNECTED)) {
	if (doAll) {
	    Tcl_DStringAppendElement(dsPtr, "-peername");
	    Tcl_DStringStartSublist(dsPtr);
	}
	tcpAddress = statePtr->pb.csParam.status.remoteHost;
	sprintf(buffer, "%d.%d.%d.%d", tcpAddress>>24,
		tcpAddress>>16 & 0xff, tcpAddress>>8 & 0xff,
		tcpAddress & 0xff);
	Tcl_DStringAppendElement(dsPtr, buffer);
	Tcl_DStringSetLength(&dString, 0);
	if (ResolveAddress(tcpAddress, &dString) == noErr) {
	    Tcl_DStringAppendElement(dsPtr, dString.string);
	} else {
	    Tcl_DStringAppendElement(dsPtr, "<unknown>");
	}
	sprintf(buffer, "%d", statePtr->pb.csParam.status.remotePort);
	Tcl_DStringAppendElement(dsPtr, buffer);
	if (doAll) {
	    Tcl_DStringEndSublist(dsPtr);
	}
    }

    Tcl_DStringFree(&dString);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * TcpReady --
 *
 *	Called by the notifier to check whether events of interest are
 *	present on the channel.
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
TcpReady(instanceData, mask)
    ClientData instanceData;		/* The file state. */
    int mask;				/* Events of interest; an OR-ed
                                         * combination of TCL_READABLE,
                                         * TCL_WRITABLE and TCL_EXCEPTION. */
{
    TcpState *statePtr = (TcpState *) instanceData;

    return (statePtr->checkMask & mask);
}

/*
 *----------------------------------------------------------------------
 *
 * TcpWatch --
 *
 *	Initialize the notifier to watch Tcl_Files from this channel.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Sets the watchMask for the channel.
 *
 *----------------------------------------------------------------------
 */

static void
TcpWatch(instanceData, mask)
    ClientData instanceData;		/* The file state. */
    int mask;				/* Events of interest; an OR-ed
                                         * combination of TCL_READABLE,
                                         * TCL_WRITABLE and TCL_EXCEPTION. */
{
    TcpState *statePtr = (TcpState *) instanceData;

    statePtr->watchMask = mask;
}

/*
 *----------------------------------------------------------------------
 *
 * SocketFreeProc --
 *
 *	This callback is invoked by Tcl_FreeFile in order to delete
 *	the notifier data associated with a file handle.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Removes the SocketInfo from the global socket list.
 *
 *----------------------------------------------------------------------
 */

static void
SocketFreeProc(
    ClientData clientData)	/* Channel state. */
{
    TcpState *statePtr = (TcpState *) clientData;
    TCPiopb statusPB;
    OSErr err;

    /*
     * Get the status of this connection.  We need to do a
     * few tests to see if it's OK to release the stream now.
     */
    if (!(statePtr->flags & TCP_RELEASE)) {
	return;
    }
    statusPB.ioCRefNum = driverRefNum;
    statusPB.tcpStream = statePtr->tcpStream;
    statusPB.csCode = TCPStatus;
    err = PBControlSync((ParmBlkPtr) &statusPB);
    if ((statusPB.csParam.status.connectionState == 0) ||
	(statusPB.csParam.status.connectionState == 2)) {
	/*
	 * If the conection state is 0 then this was a client
	 * connection and it's closed.  If it is 2 then this a
	 * server client and we may release it.  If it isn't
	 * one of those values then we return and we'll try to
	 * clean up later.
	 */
    } else {
	return;
    }
    
    /*
     * The Close request is made async.  We know it's
     * OK to release the socket when the TCP_RELEASE flag
     * gets set.
     */
    InitMacTCPParamBlock(&statePtr->pb, TCPRelease);
    statePtr->pb.tcpStream = statePtr->tcpStream;
    err = PBControlSync((ParmBlkPtr) &statePtr->pb);
    if (err != noErr) {
        Debugger(); /* Ignoreing leaves stranded stream.  Is there an alternative?  */
    }

    /*
     * Free the buffer space used by the socket and the 
     * actual socket state data structure.
     */
    ckfree((char *) statePtr->pb.csParam.create.rcvBuff);
    FreeSocketInfo(statePtr);
}

/*
 *----------------------------------------------------------------------
 *
 * NewSocketInfo --
 *
 *	This function allocates and initializes a new SocketInfo
 *	structure.
 *
 * Results:
 *	Returns a newly allocated SocketInfo.
 *
 * Side effects:
 *	Adds the socket to the global socket list, allocates memory.
 *
 *----------------------------------------------------------------------
 */

static TcpState *
NewSocketInfo(
    Tcl_File file)	/* Channel file. */
{
    TcpState *statePtr;

    statePtr = (TcpState *) ckalloc((unsigned) sizeof(TcpState));
    statePtr->tcpStream = (StreamPtr) Tcl_GetFileInfo(file, NULL);
    statePtr->psn = applicationPSN;
    statePtr->sock = file;
    statePtr->flags = 0;
    statePtr->checkMask = 0;
    statePtr->watchMask = 0;
    statePtr->acceptProc = (Tcl_TcpAcceptProc *) NULL;
    statePtr->acceptProcData = (ClientData) NULL;
    statePtr->nextPtr = socketList;
    socketList = statePtr;
    Tcl_SetNotifierData(file, SocketFreeProc, (ClientData) statePtr);
    return statePtr;
}

/*
 *----------------------------------------------------------------------
 *
 * FreeSocketInfo --
 *
 *	This function deallocates a SocketInfo structure that is no
 *	longer needed.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Removes the socket from the global socket list, frees memory.
 *
 *----------------------------------------------------------------------
 */

static void
FreeSocketInfo(
    TcpState *statePtr)		/* The state pointer to free. */
{
    if (statePtr == socketList) {
	socketList = statePtr->nextPtr;
    } else {
	TcpState *p;
	for (p = socketList; p != NULL; p = p->nextPtr) {
	    if (p->nextPtr == statePtr) {
		p->nextPtr = statePtr->nextPtr;
		break;
	    }
	}
    }
    ckfree((char *) statePtr);
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_MakeTcpClientChannel --
 *
 *	Creates a Tcl_Channel from an existing client TCP socket.
 *
 * Results:
 *	The Tcl_Channel wrapped around the preexisting TCP socket.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

Tcl_Channel
Tcl_MakeTcpClientChannel(
    ClientData sock)	/* The socket to wrap up into a channel. */
{
    TcpState *statePtr;
    Tcl_File sockFile;
    char channelName[20];
    Tcl_Channel chan;

    if (!socketsInitalized) {
	if (InitSockets() == 0) {
	    return NULL;
	}
    }
	
    sockFile = Tcl_GetFile(sock, TCL_MAC_SOCKET);
    statePtr = NewSocketInfo(sockFile);
    /* TODO: do we need to set the port??? */
    
    sprintf(channelName, "sock%d", socketNumber);
    
    chan = Tcl_CreateChannel(&tcpChannelType, channelName,
            (ClientData) statePtr, (TCL_READABLE | TCL_WRITABLE));
    if (chan != (Tcl_Channel) NULL) {
	Tcl_SetChannelBufferSize(chan, socketBufferSize);
	Tcl_SetChannelOption(NULL, chan, "-translation", "auto crlf");
    }
    return chan;
}

/*
 *----------------------------------------------------------------------
 *
 * CreateSocket --
 *
 *	This function opens a new socket and initializes the
 *	SocketInfo structure.
 *
 * Results:
 *	Returns a new SocketInfo, or NULL with an error in interp.
 *
 * Side effects:
 *	Adds a new socket to the socketList.
 *
 *----------------------------------------------------------------------
 */

static TcpState *
CreateSocket(
    Tcl_Interp *interp,		/* For error reporting; can be NULL. */
    int port,			/* Port number to open. */
    char *host,			/* Name of host on which to open port. */
    char *myaddr,		/* Optional client-side address */
    int myport,			/* Optional client-side port */
    int server,			/* 1 if socket should be a server socket,
				 * else 0 for a client socket. */
    int async)			/* 1 create async, 0 do sync. */
{
    ip_addr macAddr;
    OSErr err;
    TCPiopb pb;
    StreamPtr tcpStream;
    Tcl_File handle;
    TcpState *statePtr;
    char * buffer;
    
    /*
     * Figure out the ip address from the host string.
     */
    if (host == NULL) {
	err = GetLocalAddress(&macAddr);
    } else {
	err = GetHostFromString(host, &macAddr);
    }
    if (err != noErr) {
	Tcl_SetErrno(0);
	if (interp != (Tcl_Interp *) NULL) {
	    Tcl_AppendResult(interp, "couldn't open socket: ",
                        Tcl_PosixError(interp), (char *) NULL);
	}
	return (TcpState *) NULL;
    }
    
    /*
     * Create a MacTCP stream and create the state used for socket
     * transactions from here on out.
     */

    buffer = ckalloc(socketBufferSize);
    InitMacTCPParamBlock(&pb, TCPCreate);
    pb.csParam.create.rcvBuff = buffer;
    pb.csParam.create.rcvBuffLen = socketBufferSize;
    err = PBControlSync((ParmBlkPtr) &pb);
    if (err != noErr) {
        Tcl_SetErrno(0); /* TODO: set to ENOSR - maybe?*/
        if (interp != (Tcl_Interp *) NULL) {
	    Tcl_AppendResult(interp, "couldn't open socket: ",
		Tcl_PosixError(interp), (char *) NULL);
        }
	return (TcpState *) NULL;
    }

    tcpStream = pb.tcpStream;
    handle = Tcl_GetFile((ClientData) tcpStream, TCL_MAC_SOCKET);
    statePtr = NewSocketInfo(handle);
    statePtr->port = port;
    
    if (server) {
        /* 
         * Set up server connection.
         */

	InitMacTCPParamBlock(&statePtr->pb, TCPPassiveOpen);
	statePtr->pb.tcpStream = tcpStream;
	statePtr->pb.csParam.open.localPort = statePtr->port;
	statePtr->pb.ioCompletion = completeUPP; 
	statePtr->pb.csParam.open.userDataPtr = (Ptr) statePtr;
	statePtr->flags |= TCP_LISTENING;
	err = PBControlAsync((ParmBlkPtr) &(statePtr->pb));
	Tcl_SetErrno(EINPROGRESS);
    } else {
	/*
	 * Attempt to connect. The connect may fail at present with an
	 * EINPROGRESS but at a later time it will complete. The caller
	 * will set up a file handler on the socket if she is interested in
	 * being informed when the connect completes.
	 */

	InitMacTCPParamBlock(&statePtr->pb, TCPActiveOpen);
	statePtr->pb.tcpStream = tcpStream;
	statePtr->pb.csParam.open.remoteHost = macAddr;
	statePtr->pb.csParam.open.remotePort = port;
	statePtr->pb.csParam.open.localHost = 0;
	statePtr->pb.csParam.open.localPort = myport;
	statePtr->pb.csParam.open.userDataPtr = (Ptr) statePtr;
	statePtr->pb.ioCompletion = completeUPP;
	if (async) {
	    statePtr->flags |= TCP_ASYNC_CONNECT;
	    err = PBControlAsync((ParmBlkPtr) &(statePtr->pb));
	    Tcl_SetErrno(EINPROGRESS);
	} else {
	    err = PBControlSync((ParmBlkPtr) &(statePtr->pb));
	}
    }
    
    switch (err) {
	case noErr:
	    if (!async) {
		statePtr->flags |= TCP_CONNECTED;
	    }
	    return statePtr;
	case duplicateSocket:
	    Tcl_SetErrno(EADDRINUSE);
	    break;
	case openFailed:
	    Tcl_SetErrno(ECONNREFUSED);
	    break;
	case invalidStreamPtr:
	case connectionExists:
	default:
	    /*
	     * These cases should never occur.  However, we will fail gracefully
	     * and hope Tcl can resume.  The alternative is to panic which is 
	     * probably a bit drastic.
	     */ 
	    Debugger();
	    Tcl_SetErrno(err);
    }

    /*
     * We had error during the connection.  Release the stream
     * and file handle.  Also report to the interp.
     */
    pb.ioCRefNum = driverRefNum;
    pb.csCode = TCPRelease;
    pb.tcpStream = tcpStream;
    pb.ioCompletion = NULL; 
    err = PBControlSync((ParmBlkPtr) &pb);

    if (interp != (Tcl_Interp *) NULL) {
	Tcl_AppendResult(interp, "couldn't open socket: ",
	    Tcl_PosixError(interp), (char *) NULL);
    }

    Tcl_FreeFile(handle);
    ckfree(buffer);
    FreeSocketInfo(statePtr);
    return (TcpState *) NULL;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_OpenTcpClient --
 *
 *	Opens a TCP client socket and creates a channel around it.
 *
 * Results:
 *	The channel or NULL if failed. On failure, the routine also
 *	sets the output argument errorCodePtr to the error code.
 *
 * Side effects:
 *	Opens a client socket and creates a new channel.
 *
 *----------------------------------------------------------------------
 */

Tcl_Channel
Tcl_OpenTcpClient(
    Tcl_Interp *interp, 		/* For error reporting; can be NULL. */
    int port, 				/* Port number to open. */
    char *host, 			/* Host on which to open port. */
    char *myaddr, 			/* Client-side address */
    int myport, 			/* Client-side port */
    int async)				/* If nonzero, attempt to do an
                                         * asynchronous connect. Otherwise
                                         * we do a blocking connect. 
                                         * - currently ignored */
{
    Tcl_Channel chan;
    TcpState *statePtr;
    char channelName[20];

    if (TclHasSockets(interp) != TCL_OK) {
	return NULL;
    }

    if (!socketsInitalized) {
	if (InitSockets() == 0) {
	    return NULL;
	}
    }
	
    /*
     * Create a new client socket and wrap it in a channel.
     */

    statePtr = CreateSocket(interp, port, host, myaddr, myport, 0, async);
    if (statePtr == NULL) {
	return NULL;
    }
    
    sprintf(channelName, "sock%d", socketNumber++);
    chan = Tcl_CreateChannel(&tcpChannelType, channelName,
            (ClientData) statePtr, (TCL_READABLE | TCL_WRITABLE));
    Tcl_SetChannelOption(NULL, chan, "-translation", "auto crlf");
    Tcl_SetChannelBufferSize(chan, socketBufferSize);

    return chan;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_OpenTcpServer --
 *
 *	Opens a TCP server socket and creates a channel around it.
 *
 * Results:
 *	The channel or NULL if failed.
 *
 * Side effects:
 *	Opens a server socket and creates a new channel.
 *
 *----------------------------------------------------------------------
 */

Tcl_Channel
Tcl_OpenTcpServer(
    Tcl_Interp *interp,			/* For error reporting - may be
                                         * NULL. */
    int port,				/* Port number to open. */
    char *host,				/* Name of local host. */
    Tcl_TcpAcceptProc *acceptProc,	/* Callback for accepting connections
                                         * from new clients. */
    ClientData acceptProcData)		/* Data for the callback. */
{
    Tcl_Channel chan;
    TcpState *statePtr;
    char channelName[20];

    if (TclHasSockets(interp) != TCL_OK) {
	return NULL;
    }

    if (!socketsInitalized) {
	if (InitSockets() == 0) {
	    return NULL;
	}
    }
	
    /*
     * Create a new client socket and wrap it in a channel.
     */

    statePtr = CreateSocket(interp, port, host, NULL, 0, 1, 1);
    if (statePtr == NULL) {
	return NULL;
    }

    statePtr->acceptProc = acceptProc;
    statePtr->acceptProcData = acceptProcData;

    /*
     * Set up the callback mechanism for accepting connections
     * from new clients. The caller will use Tcl_TcpRegisterCallback
     * to register a callback to call when a new connection is
     * accepted.
     */

    Tcl_CreateFileHandler(statePtr->sock, TCL_READABLE, TcpAccept,
            (ClientData) statePtr);

    sprintf(channelName, "sock%d", socketNumber++);

    chan = Tcl_CreateChannel(&tcpChannelType, channelName,
            (ClientData) statePtr, 0);
    Tcl_SetChannelOption(NULL, chan, "-translation", "auto crlf");
    Tcl_SetChannelBufferSize(chan, socketBufferSize);

    return chan;
}

/*
 *----------------------------------------------------------------------
 *
 * TclMacWatchSocket --
 *
 *	This function imlements the socket specific portion of the
 *	Tcl_WatchFile function in the notifier.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The watched socket will be placed into non-blocking mode, and
 *	an entry on the asynch handler list will be created if necessary. 
 *
 *----------------------------------------------------------------------
 */

void
TclMacWatchSocket(
    Tcl_File file, 		/* Socket to watch. */
    int mask)			/* OR'ed combination of TCL_READABLE,
				 * TCL_WRITABLE, and TCL_EXCEPTION:
				 * indicates conditions to wait for
				 * in select. */
{
    TcpState *statePtr = (TcpState *) Tcl_GetNotifierData(file, NULL);

    /*
     * Create socket info on demand if necessary.  We should only enter this
     * code if the socket was created outside of Tcl.  Since this may be
     * the first time that the socket code has been called, we need to invoke
     * TclHasSockets to ensure that everything is initialized properly.
     *
     * Note: This may not work as certain state may be incorrect.
     */

    if (statePtr == NULL) {
	if (TclHasSockets(NULL) != TCL_OK) {
	    return;
	}
	if (!socketsInitalized) {
	    InitSockets();
	}
	
	statePtr = NewSocketInfo(file);
    }

    statePtr->watchMask = mask;
}

/*
 *----------------------------------------------------------------------
 *
 * TclMacNotifySocket --
 *
 *	Look through the currently opened socket channels.  For each
 *	channel we get the Tcp streams current status.  Based on the
 *	status we determine if the channel should be made readable or
 *	writeable.  The channel is also made read/write-able if there
 *	is an error while getting the status.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	May set a channel to be readable or writeable.
 *
 *----------------------------------------------------------------------
 */

int
TclMacNotifySocket()
{
    TcpState *statePtr;
    TcpState dummyState;
    TCPiopb statusPB;
    int numFound = 0;
    int foundSomething;
    int amount;
    int didStatus;
    OSErr err;

    if (socketList == NULL) {
	return 0;
    }

    /*
     * Establish or remove any notifiers.
     */

    for (statePtr = socketList; statePtr != NULL;
				statePtr = statePtr->nextPtr) {
	/*
	 * Check to see if this socket is dead and needs to be cleaned
	 * up.  We use a dummy statePtr whose only valid field is the
	 * nextPtr to allow the loop to continue even if the element
	 * is deleted.
	 */
	if (statePtr->flags & TCP_RELEASE) {
	    dummyState.nextPtr = statePtr->nextPtr;
	    SocketFreeProc(statePtr);
	    statePtr = &dummyState;
	    continue;
	}
	foundSomething = false;
	didStatus = false;
	if (statePtr->watchMask & TCL_READABLE) {
	    if (statePtr->checkMask & TCL_READABLE) {
		foundSomething = true;
	    } else if (statePtr->flags & TCP_CONNECTED) {
		statusPB.ioCRefNum = driverRefNum;
		statusPB.tcpStream = statePtr->tcpStream;
		statusPB.csCode = TCPStatus;
		err = PBControlSync((ParmBlkPtr) &statusPB);
		didStatus = true;
		/*
		 * If there is an error or there is more data available
		 * we make the channel readable.
		 */
		if ((err != noErr) ||
			(statusPB.csParam.status.amtUnreadData > 0)) {
		    statePtr->checkMask |= TCL_READABLE;
		    foundSomething = true;
		}
	    }
	}
	if (statePtr->watchMask & TCL_WRITABLE) {
	    if (statePtr->checkMask & TCL_WRITABLE) {
		foundSomething = true;
	    } else if (statePtr->flags & TCP_CONNECTED) {
		if (!didStatus) {
		    statusPB.ioCRefNum = driverRefNum;
		    statusPB.tcpStream = statePtr->tcpStream;
		    statusPB.csCode = TCPStatus;
		    err = PBControlSync((ParmBlkPtr) &statusPB);
		}
		/*
		 * If there is an error or there if there is room to
		 * send more data we make the channel writeable.
		 */
		amount = statusPB.csParam.status.sendWindow - 
		    statusPB.csParam.status.amtUnackedData;
		if ((err != noErr) || (amount > 0)) {
		    statePtr->checkMask |= TCL_WRITABLE;
		    foundSomething = true;
		}
	    }
	}
	if (foundSomething) {
	    numFound++;
	}
    }

    return numFound;
}

/*
 *----------------------------------------------------------------------
 *
 * TclMacSocketReady --
 *
 *	This function is invoked by Tcl_FileReady to check whether
 *	the specified conditions are present on a socket.
 *
 * Results:
 *	The return value is 0 if none of the conditions specified by
 *	mask were true for socket the last time the system checked.
 *	If any of the conditions were true, then the return value is a
 *	mask of those that were true.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

int
TclMacSocketReady(
    Tcl_File file, 	/* File handle for a stream. */
    int mask)		/* OR'ed combination of TCL_READABLE,
			 * TCL_WRITABLE, and TCL_EXCEPTION:
			 * indicates conditions caller cares about. */
{
    TcpState *statePtr = (TcpState *) Tcl_GetNotifierData(file, NULL);

    return (statePtr->checkMask & mask);
}

/*
 *----------------------------------------------------------------------
 *
 * TcpAccept --
 *	Accept a TCP socket connection.  This is called by the event 
 *	loop, and it in turns calls any registered callbacks for this
 *	channel.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Evals the Tcl script associated with the server socket.
 *
 *----------------------------------------------------------------------
 */

static void
TcpAccept(
    ClientData data, 	/* Callback token. */
    int mask)		/* Not used. */
{
    TcpState *statePtr;
    TcpState *newStatePtr;
    Tcl_File handle;
    StreamPtr tcpStream;
    Tcl_Channel chan;
    char remoteHostname[255];
    OSErr err;
    
    statePtr = (TcpState *) data;

    Tcl_DeleteFileHandler(statePtr->sock);
    statePtr->flags &= ~TCP_LISTEN_CONNECT;
    statePtr->checkMask &= ~TCL_READABLE;

    /*
     * Transfer sever stream to new connection.
     */
    tcpStream = statePtr->tcpStream;
    handle = Tcl_GetFile((ClientData) tcpStream, TCL_MAC_SOCKET);
    newStatePtr = NewSocketInfo(handle);
    newStatePtr->tcpStream = tcpStream;
    sprintf(channelName, "sock%d", socketNumber++);
    chan = Tcl_CreateChannel(&tcpChannelType, channelName,
            (ClientData) newStatePtr, (TCL_READABLE | TCL_WRITABLE));
    newStatePtr->flags |= TCP_CONNECTED;
    Tcl_SetChannelOption(NULL, chan, "-translation", "auto crlf");
    Tcl_SetChannelBufferSize(chan, socketBufferSize);

    /*
     * Reopen passive connect.  Make new tcpStream the server.
     */
    InitMacTCPParamBlock(&statePtr->pb, TCPCreate);
    statePtr->pb.csParam.create.rcvBuff = ckalloc(socketBufferSize);
    statePtr->pb.csParam.create.rcvBuffLen = socketBufferSize;
    err = PBControlSync((ParmBlkPtr) &statePtr->pb);
    if (err != noErr) {
	/* 
	 * Hmmm...  We can't reopen the server.  We'll go ahead
	 * an continue - but we are kind of broken now...
	 */
    }

    tcpStream = statePtr->tcpStream = statePtr->pb.tcpStream;
    statePtr->sock = Tcl_GetFile((ClientData) tcpStream, TCL_MAC_SOCKET);
    Tcl_SetNotifierData(statePtr->sock, SocketFreeProc, (ClientData) statePtr);
    
    InitMacTCPParamBlock(&statePtr->pb, TCPPassiveOpen);
    statePtr->pb.tcpStream = tcpStream;
    statePtr->pb.csParam.open.localHost = 0;
    statePtr->pb.csParam.open.localPort = statePtr->port;
    statePtr->pb.ioCompletion = completeUPP; 
    statePtr->pb.csParam.open.userDataPtr = (Ptr) statePtr;
    statePtr->flags |= TCP_LISTENING;
    err = PBControlAsync((ParmBlkPtr) &(statePtr->pb));
    /*
     * TODO: deal with case where we can't recreate server socket...
     */

    /*
     * Remove old file handler & create new one.
     */
    Tcl_CreateFileHandler(statePtr->sock, TCL_READABLE, TcpAccept,
            (ClientData) statePtr);
    
    /*
     * Finally we run the accept procedure.  We must do this last to make
     * sure we are in a nice clean state.  This Tcl code can do anything
     * including closing the server or client sockets we've just delt with.
     */
    if (statePtr->acceptProc != NULL) {
	ip_addr ourAddress = statePtr->pb.csParam.open.remoteHost;
	
	sprintf(remoteHostname, "%d.%d.%d.%d", ourAddress>>24,
		ourAddress>>16 & 0xff, ourAddress>>8 & 0xff,
		ourAddress & 0xff);
		
	(statePtr->acceptProc)(statePtr->acceptProcData, chan, 
	    remoteHostname, statePtr->pb.csParam.open.remotePort);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_GetHostName --
 *
 *	Returns the name of the local host.  The result is cached to
 *	be speedy after the first call.
 *
 * Results:
 *	Returns a string containing the host name, or NULL on error.
 *	The returned string must be freed by the caller.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

char *
Tcl_GetHostName()
{
    static int  hostnameInited = 0;
    static char hostname[255];
    ip_addr ourAddress;
    Tcl_DString dString;
    OSErr err;
    
    if (hostnameInited) {
        return hostname;
    }
    
    if (TclHasSockets(NULL) != TCL_OK) {
	hostname[0] = '\0';
        hostnameInited = 1;
	return hostname;
    }

    if (!socketsInitalized) {
	if (InitSockets() == 0) {
	    return NULL;
	}
    }
	
    err = GetLocalAddress(&ourAddress);

    if (err == noErr) {
        /*
         * Search for the doman name and return it if found.  Otherwise, 
         * just print the IP number to a string and return that.
         */
        Tcl_DStringInit(&dString);
        err = ResolveAddress(ourAddress, &dString);
	if (err == noErr) {
	    strcpy(hostname, dString.string);
	} else {
	    sprintf(hostname, "%d.%d.%d.%d", ourAddress>>24, ourAddress>>16 & 0xff,
		ourAddress>>8 & 0xff, ourAddress & 0xff);
	}
	Tcl_DStringFree(&dString);
	
        hostnameInited = 1;
        return hostname;
    }
    
    return (char *) NULL;
}

/*
 *----------------------------------------------------------------------
 *
 * ResolveAddress --
 *
 *	This function is used to resolve an ip address to it's full 
 *	domain name address.
 *
 * Results:
 *	An os err value.
 *
 * Side effects:
 *	Treats client data as int we set to true.
 *
 *----------------------------------------------------------------------
 */

static OSErr 
ResolveAddress(
    ip_addr tcpAddress, 	/* Address to resolve. */
    Tcl_DString *dsPtr)		/* Returned address in string. */
{
    int i;
    EventRecord dummy;
    DNRState dnrState;
    OSErr err;

    /*
     * Call AddrToName to resolve our ip address to our domain name.
     * The call is async, so we must wait for a callback to tell us
     * when to continue.
     */
     for (i = 0; i < NUM_ALT_ADDRS; i++) {
	dnrState.hostInfo.addr[i] = 0;
     }
    dnrState.done = 0;
    GetCurrentProcess(&(dnrState.psn));
    err = AddrToName(tcpAddress, &dnrState.hostInfo, resultUPP, (Ptr) &dnrState);
    if (err == cacheFault) {
	while (!dnrState.done) {
	    WaitNextEvent(0, &dummy, 1, NULL);
	}
    }
    
    /*
     * If there is no error in finding the domain name we set the
     * result into the dynamic string.  We also work around a bug in
     * MacTcp where an extranious '.' may be found at the end of the name.
     */
    if (dnrState.hostInfo.rtnCode == noErr) {
	i = strlen(dnrState.hostInfo.cname) - 1;
	if (dnrState.hostInfo.cname[i] == '.') {
	    dnrState.hostInfo.cname[i] = '\0';
	}
	Tcl_DStringAppend(dsPtr, dnrState.hostInfo.cname, -1);
    }
    
    return dnrState.hostInfo.rtnCode;
}

/*
 *----------------------------------------------------------------------
 *
 * DNRCompletionRoutine --
 *
 *	This function is called when the Domain Name Server is done
 *	seviceing our request.  It just sets a flag that we can poll
 *	in functions like Tcl_GetHostName to let them know to continue.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Treats client data as int we set to true.
 *
 *----------------------------------------------------------------------
 */

static pascal void 
DNRCompletionRoutine(
    struct hostInfo *hostinfoPtr, 	/* Host infor struct. */
    DNRState *dnrStatePtr)		/* Completetion state. */
{
    dnrStatePtr->done = true;
    WakeUpProcess(&(dnrStatePtr->psn));
}

/*
 *----------------------------------------------------------------------
 *
 * TclHasSockets --
 *
 *	This function determines whether sockets are available on the
 *	current system and returns an error in interp if they are not.
 *	Note that interp may be NULL.  This call uses the Macintosh
 *	gestalt function to determine the existance of Mac Tcp.
 *
 * Results:
 *	Returns TCL_OK if the system supports sockets, or TCL_ERROR with
 *	an error in interp.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

#define gestaltMacTCPVersion 'mtcp'
int
TclHasSockets(
    Tcl_Interp *interp)		/* Interp for error messages. */
{
    long response;
    
static int socketsTestInited = false;
static int hasSockets = false;
    if (!socketsTestInited) {
	if (Gestalt(gestaltMacTCPVersion, &response) == noErr) {
	    hasSockets = true;
	} else {
	    hasSockets = false;
	}
	socketsTestInited = true;
    }
    if (hasSockets) {
	return TCL_OK;
    }
    if (interp != NULL) {
	Tcl_AppendResult(interp, "sockets are not available on this system",
		NULL);
    }
    return TCL_ERROR;
}

/*
 *----------------------------------------------------------------------
 *
 * InitSockets --
 *
 *	Load the MacTCP driver and open the name resolver.  We also
 *	create several UPP's used by our code.  Lastly, we install
 *	a patch to ExitToShell to clean up socket connections if
 *	we are about to exit.
 *
 * Results:
 *	1 if successful, 0 on failure.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static int
InitSockets()
{
    ParamBlockRec pb; 
    OSErr err;
	
    if (socketsInitalized) {
	return 1;
    }

    /*
     * Load MacTcp driver and name server resolver.
     */
	
		
    pb.ioParam.ioCompletion = 0L; 
    pb.ioParam.ioNamePtr = "\p.IPP"; 
    pb.ioParam.ioPermssn = fsCurPerm; 
    err = PBOpenSync(&pb); 
    if (err != noErr) {
	return 0;
    }
    driverRefNum = pb.ioParam.ioRefNum; 
	
    socketBufferSize = GetBufferSize();
    err = OpenResolver(NULL);
    if (err != noErr) {
	return 0;
    }

    GetCurrentProcess(&applicationPSN);
    /*
     * Create UPP's for various callback routines.
     */
    resultUPP = NewResultProc(DNRCompletionRoutine);
    completeUPP = NewTCPIOCompletionProc(IOCompletionRoutine);
    closeUPP = NewTCPIOCompletionProc(CloseCompletionRoutine);

    /*
     * Install an ExitToShell patch.  We use this patch instead
     * of the Tcl exit mechanism because we need to ensure that
     * these routines are cleaned up even if we crash or are forced
     * to quit.  There are some circumstances when the Tcl exit
     * handlers may not fire.
     */
    TclMacInstallExitToShellPatch(CleanUpExitProc);
    
    socketsInitalized = true;
    return 1;
}

/*
 *----------------------------------------------------------------------
 *
 * CleanUpExitProc --
 *
 *	This procedure is invoked as an exit handler when ExitToShell
 *	is called.  It aborts any lingering socket connections.  This 
 *	must be called or the Mac OS will more than likely crash.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static pascal void
CleanUpExitProc()
{
    TcpState *statePtr;

    while (socketList != NULL) {
	statePtr = socketList;
	socketList = statePtr->nextPtr;

	/*
	 * Close and Release the connection.
	 */
	statePtr->pb.ioCRefNum = driverRefNum;
	statePtr->pb.csCode = TCPClose;
	statePtr->pb.tcpStream = statePtr->tcpStream;
	statePtr->pb.csParam.close.ulpTimeoutValue = 60 /* seconds */;
	statePtr->pb.csParam.close.ulpTimeoutAction = 1 /* 1:abort 0:report */;
	statePtr->pb.csParam.close.validityFlags = timeoutValue | timeoutAction;
	statePtr->pb.ioCompletion = NULL; 
	PBControlSync((ParmBlkPtr) &(statePtr->pb));

	statePtr->pb.ioCRefNum = driverRefNum;
	statePtr->pb.csCode = TCPRelease;
	statePtr->pb.tcpStream = statePtr->tcpStream;
	statePtr->pb.ioCompletion = NULL; 
	PBControlSync((ParmBlkPtr) &(statePtr->pb));
    }
}

/*
 *----------------------------------------------------------------------
 *
 * GetHostFromString --
 *
 *	Looks up the passed in domain name in the domain resolver.  It
 *	can accept strings of two types: 1) the ip number in string
 *	format, or 2) the domain name.
 *
 * Results:
 *	We return a ip address or 0 if there was an error or the 
 *	domain does not exist.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static OSErr
GetHostFromString(
    char *name, 		/* Host in string form. */
    ip_addr *address)		/* Returned IP address. */
{
    OSErr err;
    int i;
    EventRecord dummy;
    DNRState dnrState;
	
    if (TclHasSockets(NULL) != TCL_OK) {
	return 0;
    }

    if (!socketsInitalized) {
	if (InitSockets() == 0) {
	    return -1;
	}
    }

    /*
     * Call StrToAddr to get the ip number for the passed in domain
     * name.  The call is async, so we must wait for a callback to 
     * tell us when to continue.
     */
    for (i = 0; i < NUM_ALT_ADDRS; i++) {
	dnrState.hostInfo.addr[i] = 0;
    }
    dnrState.done = 0;
    GetCurrentProcess(&(dnrState.psn));
    err = StrToAddr(name, &dnrState.hostInfo, resultUPP, (Ptr) &dnrState);
    if (err == cacheFault) {
	while (!dnrState.done) {
	    WaitNextEvent(0, &dummy, 1, NULL);
	}
    }
    
    /*
     * For some reason MacTcp may return a cachFault a second time via
     * the hostinfo block.  This seems to be a bug in MacTcp.  In this case 
     * we run StrToAddr again - which seems to then work just fine.
     */
    if (dnrState.hostInfo.rtnCode == cacheFault) {
	dnrState.done = 0;
	err = StrToAddr(name, &dnrState.hostInfo, resultUPP, (Ptr) &dnrState);
	if (err == cacheFault) {
	    while (!dnrState.done) {
		WaitNextEvent(0, &dummy, 1, NULL);
	    }
	}
    }

    if (dnrState.hostInfo.rtnCode == noErr) {
	*address = dnrState.hostInfo.addr[0];
    }
    
    return dnrState.hostInfo.rtnCode;
}

/*
 *----------------------------------------------------------------------
 *
 * IOCompletionRoutine --
 *
 *	This function is called when a client or server socket gets a
 *	connection from a remote host.  It will then simply set state
 *	to tell the notifier that this socket is now ready for action.
 *	Note that this function is running at interupt time and can't
 *	allocate memory or do much else except set state.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Sets some state in the socket state.  May also wake the process
 *	if we are not currently running.
 *
 *----------------------------------------------------------------------
 */

static void
IOCompletionRoutine(
    TCPiopb *pbPtr)		/* Tcp parameter block. */
{
    TcpState *statePtr;
    
    statePtr = (TcpState *) pbPtr->csParam.open.userDataPtr;
    
    /*
     * Always wake the process in case it's in WaitNextEvent.
     * If an error has a occured - just return.  We will deal
     * with the problem later.
     */
    WakeUpProcess(&statePtr->psn);
    if (pbPtr->ioResult != noErr) {
	return;
    }
    
    if (statePtr->flags & TCP_ASYNC_CONNECT) {
	statePtr->flags &= ~TCP_ASYNC_CONNECT;
	statePtr->flags |= TCP_CONNECTED;
	statePtr->checkMask |= TCL_READABLE & TCL_WRITABLE;
    } else if (statePtr->flags & TCP_LISTENING) {
	statePtr->flags &= ~TCP_LISTENING;
	statePtr->flags |= TCP_LISTEN_CONNECT;
	statePtr->checkMask |= TCL_READABLE;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * GetLocalAddress --
 *
 *	Get the IP address for this machine.  The result is cached so
 *	the result is returned quickly after the first call.
 *
 * Results:
 *	Macintosh error code.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static OSErr 
GetLocalAddress(
    unsigned long *addr)	/* Returns host IP address. */
{
    struct GetAddrParamBlock pBlock;
    OSErr err = noErr;
    static unsigned long localAddress = 0;

    if (localAddress == 0) {
	memset(&pBlock, 0, sizeof(pBlock));
	pBlock.ioResult = 1;
	pBlock.csCode = ipctlGetAddr;
	pBlock.ioCRefNum = driverRefNum;
	err = PBControlSync((ParmBlkPtr) &pBlock);

	if (err != noErr) {
	    return err;
	}
	localAddress = pBlock.ourAddress;
    }
    
    *addr = localAddress;
    return noErr;
}

/*
 *----------------------------------------------------------------------
 *
 * GetBufferSize --
 *
 *	Get the appropiate buffer size for our machine & network.  This
 *	value will be used by the rest of Tcl & the MacTcp driver for
 *	the size of its buffers.  If out method for determining the
 *	optimal buffer size fails for any reason - we return a 
 *	reasonable default.
 *
 * Results:
 *	Size of optimal buffer in bytes.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static long 
GetBufferSize()
{
    UDPiopb iopb;
    OSErr err = noErr;
    long bufferSize;
	
    memset(&iopb, 0, sizeof(iopb));
    err = GetLocalAddress(&iopb.csParam.mtu.remoteHost);
    if (err != noErr) {
	return CHANNEL_BUF_SIZE;
    }
    iopb.ioCRefNum = driverRefNum;
    iopb.csCode = UDPMaxMTUSize;
    err = PBControlSync((ParmBlkPtr)&iopb);
    if (err != noErr) {
	return CHANNEL_BUF_SIZE;
    }
    bufferSize = (iopb.csParam.mtu.mtuSize * 4) + 1024;
    if (bufferSize < CHANNEL_BUF_SIZE) {
	bufferSize = CHANNEL_BUF_SIZE;
    }
    return bufferSize;
}

/*
 *----------------------------------------------------------------------
 *
 * TclSockGetPort --
 *
 *	Maps from a string, which could be a service name, to a port.
 *	Used by socket creation code to get port numbers and resolve
 *	registered service names to port numbers.
 *
 * Results:
 *	A standard Tcl result.  On success, the port number is
 *	returned in portPtr. On failure, an error message is left in
 *	interp->result.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

int
TclSockGetPort(
    Tcl_Interp *interp, 	/* Interp for error messages. */
    char *string, 		/* Integer or service name */
    char *proto, 		/* "tcp" or "udp", typically - 
    				 * ignored on Mac - assumed to be tcp */
    int *portPtr)		/* Return port number */
{
    PortInfo *portInfoPtr = NULL;
    
    if (Tcl_GetInt(interp, string, portPtr) == TCL_OK) {
	if (*portPtr > 0xFFFF) {
	    Tcl_AppendResult(interp, "couldn't open socket: port number too high",
                (char *) NULL);
	    return TCL_ERROR;
	}
	if (*portPtr < 0) {
	    Tcl_AppendResult(interp, "couldn't open socket: negative port number",
                (char *) NULL);
	    return TCL_ERROR;
	}
	return TCL_OK;
    }
    for (portInfoPtr = portServices; portInfoPtr->name != NULL; portInfoPtr++) {
	if (!strcmp(portInfoPtr->name, string)) {
	    break;
	}
    }
    if (portInfoPtr != NULL && portInfoPtr->name != NULL) {
	*portPtr = portInfoPtr->port;
	Tcl_ResetResult(interp);
	return TCL_OK;
    }
    
    return TCL_ERROR;
}
