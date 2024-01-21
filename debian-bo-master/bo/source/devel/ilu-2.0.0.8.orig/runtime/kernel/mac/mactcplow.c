/*----------------------------------------------------------
#
#	NewsWatcher	- Macintosh NNTP Client Application
#
#	Written by Steven Falkenburg
#	©1990 Apple Computer, Inc.
#
#-----------------------------------------------------------
#
#	tcplow.c
#
#	This module all the low-level calls to MacTCP.
#
#-----------------------------------------------------------*/

#pragma load "MacHeaders"

#include <stdlib.h>
#include <string.h>

#include "MacTCPCommonTypes.h"
#include "TCPPB.h"
#include "AddressXlation.h"
#include "GetMyIPAddr.h"
 
#include "mactcpapi.h"
#include "mactcplow.h"
#include "macutil.h"


pascal	void DNRResultProc (struct hostInfo *hInfoPtr, char *userDataPtr);


static short gRefNum;

/* really should give some time back to the system, but we'll punt for now... */
Boolean GiveTime()
{
	EventRecord ev;
	int i;
		
	if (WaitNextEvent(everyEvent,&ev,0,nil)) {
		printf("GiveTime(): got event: %d\n",ev.what);
	}
#ifdef BUSYWAIT
	for (i=0; i < 10000; i++)
		;
	SystemTask();
#endif
	return true;
}

static void InitBlock (TCPiopb *pBlock)
{
	memset(pBlock, 0, sizeof(TCPiopb));
	pBlock->ioResult = 1;
	pBlock->ioCRefNum = gRefNum;
}


static OSErr NewBlock (TCPiopb **pBlock)
{
	*pBlock = (TCPiopb *)MyNewPtr(sizeof(TCPiopb));
	if (MyMemErr() != noErr)
		return MyMemErr();
	InitBlock(*pBlock);
	return noErr;
}


/* Opens the MacTCP driver.
   This routine must be called prior to any of the below functions. */

OSErr OpenTCPDriver (void)
{
	OSErr	err;
	
	err = OpenDriver("\p.IPP",&gRefNum);
	return(err);
}


/* Creates a new TCP stream in preparation for initiating a connection.
   A buffer must be provided for storing incoming data waiting to be processed */

OSErr LowTCPCreateStream (StreamPtr *streamPtr, Ptr connectionBuffer,
	unsigned long connBufferLen, TCPNotifyProc notifPtr)
{
	TCPiopb pBlock;
		
	InitBlock(&pBlock);
	pBlock.csCode = TCPCreate;
	pBlock.csParam.create.rcvBuff = connectionBuffer;
	pBlock.csParam.create.rcvBuffLen = connBufferLen;
	pBlock.csParam.create.notifyProc = notifPtr;
	PBControl((ParmBlkPtr)&pBlock,true);
	while (pBlock.ioResult > 0) 
		GiveTime();
	*streamPtr = pBlock.tcpStream;
	return gCancel ? -1 : pBlock.ioResult;
}


/* If TCPWaitForConnection is called asynchronously, this command retrieves the 
   result of the call.  It should be called when the above command completes. */

OSErr LowFinishTCPWaitForConn (TCPiopb *pBlock, ip_addr *remoteHost,
	tcp_port *remotePort, ip_addr *localHost, tcp_port *localPort)
{	
	OSErr err;
	
	*remoteHost = pBlock->csParam.open.remoteHost;
	*remotePort = pBlock->csParam.open.remotePort;
	*localHost = pBlock->csParam.open.localHost;
	*localPort = pBlock->csParam.open.localPort;
	err = pBlock->ioResult;
	MyDisposPtr((Ptr)pBlock);
	return err;
}


/* Waits for a connection to be opened on a specified port from a specified address.
   It completes when a connection is made, or a timeout value is reached.  This call
   may be made asynchronously. */

OSErr LowTCPWaitForConnection (StreamPtr streamPtr, byte timeout,
	ip_addr *remoteHost, tcp_port *remotePort, ip_addr *localHost,
	tcp_port *localPort, Boolean async, TCPiopb **returnBlock)
{
	OSErr err;
	TCPiopb *pBlock;
	
	if ((err = NewBlock(&pBlock)) != noErr)
		return err;
	
	pBlock->csCode = TCPPassiveOpen;
	pBlock->tcpStream = streamPtr;
	pBlock->csParam.open.ulpTimeoutValue = timeout;
	pBlock->csParam.open.ulpTimeoutAction = 1;
	pBlock->csParam.open.validityFlags = 0xC0;
	pBlock->csParam.open.commandTimeoutValue = timeout;
	pBlock->csParam.open.remoteHost = *remoteHost;
	pBlock->csParam.open.remotePort = *remotePort;
	pBlock->csParam.open.localPort = *localPort;
	PBControl((ParmBlkPtr)pBlock,true);
	if (!async) {
		while (pBlock->ioResult > 0 && GiveTime())
			;
		err = gCancel ? -1 : pBlock->ioResult;
		if (gCancel) LowTCPAbort(streamPtr);
		LowFinishTCPWaitForConn(pBlock,remoteHost,remotePort,localHost,localPort);
		return err;
	}
	
	*returnBlock = pBlock;
	return noErr;
}


/* Attempts to initiate a connection with a host specified by host and port. */

OSErr LowTCPOpenConnection (StreamPtr streamPtr, byte timeout, 
	ip_addr remoteHost, tcp_port remotePort, ip_addr *localHost,
	tcp_port *localPort)
{
	OSErr err;
	TCPiopb pBlock;
	
	InitBlock(&pBlock);
	pBlock.csCode = TCPActiveOpen;
	pBlock.tcpStream = streamPtr;
	pBlock.csParam.open.ulpTimeoutValue = timeout;
	pBlock.csParam.open.ulpTimeoutAction = 1;
	pBlock.csParam.open.validityFlags = 0xC0;
	pBlock.csParam.open.commandTimeoutValue = timeout;
	pBlock.csParam.open.remoteHost = remoteHost;
	pBlock.csParam.open.remotePort = remotePort;
	pBlock.csParam.open.localPort = *localPort;
	PBControl((ParmBlkPtr)&pBlock,true);
	while (pBlock.ioResult > 0 && GiveTime())
		;
	*localHost = pBlock.csParam.open.localHost;
	*localPort = pBlock.csParam.open.localPort;
	err = gCancel ? -1 : pBlock.ioResult;
	if (gCancel) LowTCPAbort(streamPtr);
	return err;
}


/* This routine should be called when a TCPSendData call completes.  It returns the
   error code generated upon completion of the CallTCPSend. */

OSErr LowFinishTCPSend (TCPiopb *pBlock)
{
	OSErr err;
	
	err = pBlock->ioResult;
	MyDisposPtr((Ptr)pBlock);
	return err;
}


/* Sends data through an open connection stream.  Note that the connection must be
   open before any data is sent. This call may be made asynchronously. */

OSErr LowTCPSendData (StreamPtr streamPtr, byte timeout, Boolean push,
	Boolean urgent, Ptr wdsPtr, Boolean async, TCPiopb **returnBlock)
{	
	OSErr err;
	TCPiopb *pBlock;
	
	if ((err = NewBlock(&pBlock)) != noErr)
		return err;
	
	pBlock->csCode = TCPSend;
	pBlock->tcpStream = streamPtr;
	pBlock->csParam.send.ulpTimeoutValue = timeout;
	pBlock->csParam.send.ulpTimeoutAction = 1;
	pBlock->csParam.send.validityFlags = 0xC0;
	pBlock->csParam.send.pushFlag = push;
	pBlock->csParam.send.urgentFlag = urgent;
	pBlock->csParam.send.wdsPtr = wdsPtr;
	PBControl((ParmBlkPtr)pBlock,true);
	if (!async) {
		while (pBlock->ioResult > 0 && GiveTime())
			;
		err = gCancel ? -1 : pBlock->ioResult;
		if (gCancel) LowTCPAbort(streamPtr);
		LowFinishTCPSend(pBlock);
		return err;
	}
	
	*returnBlock = pBlock;
	return noErr;
}


OSErr LowFinishTCPNoCopyRcv (TCPiopb *pBlock, Boolean *urgent, Boolean *mark)
{
	OSErr err;
	
	*urgent = pBlock->csParam.receive.urgentFlag;
	*mark = pBlock->csParam.receive.markFlag;
	
	err = pBlock->ioResult;
	MyDisposPtr((Ptr)pBlock);
	return err;
}


OSErr LowTCPNoCopyRcv (StreamPtr streamPtr, byte timeout, Boolean *urgent,
	Boolean *mark, Ptr rdsPtr, short numEntry, Boolean async,
	TCPiopb **returnBlock)
{
	OSErr	err = noErr;
	TCPiopb *pBlock;
	
	if ((err = NewBlock(&pBlock)) != noErr)
		return err;
	
	pBlock->csCode = TCPNoCopyRcv;
	pBlock->tcpStream = streamPtr;
	pBlock->csParam.receive.commandTimeoutValue = timeout;
	pBlock->csParam.receive.rdsPtr = rdsPtr;
	pBlock->csParam.receive.rdsLength = numEntry;
	PBControl((ParmBlkPtr)pBlock,true);
	if (!async) {
		while (pBlock->ioResult > 0 && GiveTime())
			;
		err = gCancel ? -1 : pBlock->ioResult;
		if (gCancel) LowTCPAbort(streamPtr);
		LowFinishTCPNoCopyRcv(pBlock,urgent,mark);
		return err;
	}
	
	*returnBlock = pBlock;
	return noErr;
}


OSErr LowTCPBfrReturn (StreamPtr streamPtr, Ptr rdsPtr)
{
	TCPiopb pBlock;

	InitBlock(&pBlock);	
	pBlock.csCode = TCPRcvBfrReturn;
	pBlock.tcpStream = streamPtr;
	pBlock.csParam.receive.rdsPtr = rdsPtr;
	PBControl((ParmBlkPtr)&pBlock,true);
	while (pBlock.ioResult > 0) GiveTime();
	return gCancel ? -1 : pBlock.ioResult;
}


/* If the above is called asynchronously, this routine returns the data that was
   received from the remote host. */
   
OSErr LowFinishTCPRecv (TCPiopb *pBlock, Boolean *urgent, Boolean *mark,
	unsigned short *rcvLen)
{
	OSErr err;
	
	*rcvLen = pBlock->csParam.receive.rcvBuffLen;
	*urgent = pBlock->csParam.receive.urgentFlag;
	*mark = pBlock->csParam.receive.markFlag;
	err = pBlock->ioResult;
	MyDisposPtr((Ptr)pBlock);
	return err;
}


/* Attempts to pull data out of the incoming stream for a connection. If data is
   not present, the routine waits a specified amout of time before returning with
   a timeout error.  This call may be made asynchronously. */
   
OSErr LowTCPRecvData (StreamPtr streamPtr, byte timeout, Boolean *urgent,
	Boolean *mark, Ptr rcvBuff, unsigned short *rcvLen, Boolean async,
	TCPiopb **returnBlock)
{
	OSErr err;
	TCPiopb *pBlock;
	
	if ((err = NewBlock(&pBlock)) != noErr)
		return err;
	
	pBlock->csCode = TCPRcv;
	pBlock->tcpStream = streamPtr;
	pBlock->csParam.receive.commandTimeoutValue = timeout;
	pBlock->csParam.receive.rcvBuff = rcvBuff;
	pBlock->csParam.receive.rcvBuffLen = *rcvLen;
	PBControl((ParmBlkPtr)pBlock,true);
	if (!async) {
		while (pBlock->ioResult > 0 && GiveTime())
			;
		err = gCancel ? -1 : pBlock->ioResult;
		if (gCancel) LowTCPAbort(streamPtr);
		LowFinishTCPRecv(pBlock,urgent,mark,rcvLen);
		return err;
	}
	
	*returnBlock = pBlock;
	return noErr;
}
	

/* Gracefully closes a connection with a remote host.  This is not always possible,
   and the programmer might have to resort to CallTCPAbort, described next. */

OSErr LowTCPClose (StreamPtr streamPtr, byte timeout)
{
	OSErr err;
	TCPiopb pBlock;

	InitBlock(&pBlock);	
	pBlock.csCode = TCPClose;
	pBlock.tcpStream = streamPtr;
	pBlock.csParam.close.ulpTimeoutValue = timeout;
	pBlock.csParam.close.validityFlags = 0xC0;
	pBlock.csParam.close.ulpTimeoutAction = 1;
	PBControl((ParmBlkPtr)&pBlock,true);
	while (pBlock.ioResult > 0 && GiveTime())
		;
	err = gCancel ? -1 : pBlock.ioResult;
	if (gCancel) LowTCPAbort(streamPtr);
	return err;
}


/* Should be called if a CallTCPClose fails to close a connection properly.
   This call should not normally be used to terminate connections. */
   
OSErr LowTCPAbort (StreamPtr streamPtr)
{
	TCPiopb pBlock;
	
	InitBlock(&pBlock);
	pBlock.csCode = TCPAbort;
	pBlock.tcpStream = streamPtr;
	PBControl((ParmBlkPtr)&pBlock,true);
	while (pBlock.ioResult > 0) GiveTime();
	return gCancel ? -1 : pBlock.ioResult;
}

OSErr LowTCPStatus (StreamPtr streamPtr, TCPStatusPB *theStatus)
{
	TCPiopb pBlock;
	
	InitBlock(&pBlock);
	pBlock.csCode = TCPStatus;
	pBlock.tcpStream = streamPtr;
	PBControl((ParmBlkPtr)&pBlock,true);
	while (pBlock.ioResult > 0) GiveTime();
	*theStatus = pBlock.csParam.status;
	return gCancel ? -1 : pBlock.ioResult;
}


/* Deallocates internal buffers used to hold connection data. This should be
   called after a connection has been closed. */

OSErr LowTCPRelease (StreamPtr streamPtr, Ptr *recvPtr, unsigned long *recvLen)
{
	TCPiopb pBlock;
	
	InitBlock(&pBlock);
	pBlock.csCode = TCPRelease;
	pBlock.tcpStream = streamPtr;
	PBControl((ParmBlkPtr)&pBlock,true);
	while (pBlock.ioResult > 0) GiveTime();
	*recvPtr = pBlock.csParam.create.rcvBuff;
	*recvLen = pBlock.csParam.create.rcvBuffLen;
	return gCancel ? -1 : pBlock.ioResult;
}

OSErr LowTCPGlobalInfo (Ptr *tcpParam, Ptr *tcpStat)
{
	TCPiopb pBlock;
	
	InitBlock(&pBlock);
	pBlock.csCode = TCPGlobalInfo;
	PBControl((ParmBlkPtr)&pBlock,true);
	while (pBlock.ioResult > 0) GiveTime();
	*tcpParam = (Ptr) pBlock.csParam.globalInfo.tcpParamPtr;
	*tcpStat = (Ptr) pBlock.csParam.globalInfo.tcpStatsPtr;
	return gCancel ? -1 : pBlock.ioResult;
}


/*	DNRResultProc is the MacTCP domain name resolver completion routine. */

static pascal void DNRResultProc (struct hostInfo *hInfoPtr, char *userDataPtr)
{
#pragma unused (hInfoPtr)

	*(Boolean*)userDataPtr = true;
}


/*	LowIPNameToAddr invokes the domain name system to translate a domain name
	into an IP address. */
	
OSErr LowIPNameToAddr (char *name, unsigned long *addr)
{
	struct hostInfo hInfo;
	OSErr err;
	Boolean done=false;
	
	if ((err = OpenResolver(nil)) != noErr) return err;
	err = StrToAddr(name, &hInfo, DNRResultProc, (char*)&done);
	if (err == cacheFault) {
		while (!done) GiveTime();
		err = hInfo.rtnCode;
	}
	CloseResolver();
	*addr = hInfo.addr[0];
	return gCancel ? -1 : err;
}


/*	LowIPAddrToName invokes the domain name system to translate an IP address
	into a domain name. */
	
OSErr LowIPAddrToName (unsigned long addr, char *name)
{
	struct hostInfo hInfo;
	OSErr err;
	Boolean done=false;
	
	if ((err = OpenResolver(nil)) != noErr) return err;
	err = AddrToName(addr, &hInfo, DNRResultProc, (char*)&done);
	if (err == cacheFault) {
		while (!done) GiveTime();
		err = hInfo.rtnCode;
	}
	CloseResolver();
	hInfo.cname[254] = 0;
	strcpy(name,hInfo.cname);
	return gCancel ? -1 : err;
}


/* LowGetMyIPAddr returns the IP address of this Mac. */

OSErr LowGetMyIPAddr (unsigned long *addr)
{
	struct	IPParamBlock	IPBlock;
	
	memset(&IPBlock, 0, sizeof(IPBlock));
	IPBlock.ioResult = 1;
	IPBlock.csCode = ipctlGetAddr;
	IPBlock.ioCRefNum = gRefNum;
	PBControl((ParmBlkPtr)&IPBlock,true);
	while (IPBlock.ioResult > 0) GiveTime();
	*addr = IPBlock.ourAddress;
	return gCancel ? -1 : IPBlock.ioResult;
}


/*	LowGetMyIPAddrStr returns the IP address of this Mac as a dotted decimal
	string. */
	
OSErr LowGetMyIPAddrStr (char *addrStr)
{
	unsigned long addr;
	OSErr err;
	static char theAddrStr[16];
	static Boolean gotIt=false;
	
	if (!gotIt) {
		if ((err = LowGetMyIPAddr(&addr)) != noErr) return err;
		if ((err = OpenResolver(nil)) != noErr) return err;
		err = AddrToStr(addr,theAddrStr);
		CloseResolver();
		if (err != noErr) return err;
		gotIt = true;
	}
	strcpy(addrStr,theAddrStr);
	return noErr;
}
