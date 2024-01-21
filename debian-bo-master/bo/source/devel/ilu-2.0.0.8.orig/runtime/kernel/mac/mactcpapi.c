/*----------------------------------------------------------
#
#	NewsWatcher	- Macintosh NNTP Client Application
#
#	Written by Steven Falkenburg
#	©1990 Apple Computer, Inc.
#
#-----------------------------------------------------------
#
#	tcp.c
#
#	This module handle all the high-level calls to MacTCP.
#
#-----------------------------------------------------------*/

#pragma segment ilu
#pragma load "MacHeaders"

#include <stdlib.h>
#include <string.h>

#include "MacTCPCommonTypes.h"
#include "TCPPB.h"

#include "mactcpapi.h"
#include "mactcplow.h"
#include "macutil.h"


#ifndef MAX_WDS
#define MAX_WDS		6
#endif


/* InitNetwork opens the network driver. */

OSErr InitNetwork (void)
{
	return OpenTCPDriver();
}


/* CreateStream() creates an unconnected network stream to be
   used later by OpenConnection.  The length of the receive
   buffer must be specified in the call. */
   
OSErr CreateStream (unsigned long *stream, unsigned long recvLen)
{
	Ptr recvPtr;
	OSErr err;
	
	recvPtr = MyNewPtr(recvLen);
	if ((err = MyMemErr())==noErr) {
		err = LowTCPCreateStream(stream,recvPtr,recvLen,(TCPNotifyProc)nil);
	} else {
		printf("CreateStream: memory allocation error...\n");
	}	
	return err;
}
	

/* ReleaseStream() frees the allocated buffer space for a given connection
   stream.  This call should be made after CloseConnection. */
   
OSErr ReleaseStream (unsigned long stream)
{
	OSErr err;
	Ptr recvPtr;
	unsigned long recvLen;
	
	if ((err = LowTCPRelease(stream,&recvPtr,&recvLen)) == noErr)
		MyDisposPtr(recvPtr);
	return err;
}


/* OpenConnection() initiates a connection to a remote machine,
   given that machine's network number and connection port.  A timeout
   value for the call must be given, and the stream identifier is returned. */

OSErr OpenConnection (unsigned long stream, long remoteHost, short remotePort,
	Byte timeout)
{
	ip_addr localHost;
	tcp_port localPort = 0;
	
	return LowTCPOpenConnection(stream,timeout,remoteHost,remotePort,&localHost,
								&localPort);
}


/* WaitForConnection() listens for a connection on a particular port from a
	particular host.  It returns when a connection has been established. */

OSErr WaitForConnection (unsigned long stream, Byte timeout, short localPort,
						long *remoteHost, short *remotePort)
{
	ip_addr localHost;
	
	return LowTCPWaitForConnection(stream,timeout,(ip_addr *)remoteHost,
				(tcp_port *)remotePort,&localHost,(tcp_port *)&localPort,false,nil);
}


/*	AsyncWaitForConnection() listens for a connection on a particular port from a
	particular host.  It is executed asynchronously and returns immediately */

OSErr AsyncWaitForConnection (unsigned long stream, Byte timeout,
	short localPort, long remoteHost, short remotePort, TCPiopb **returnBlock)
{
	ip_addr localHost;
	
	return LowTCPWaitForConnection(stream,timeout,(ip_addr *)&remoteHost,
		(tcp_port *)&remotePort,&localHost,(tcp_port *)&localPort,true,returnBlock);
}


/*	AsyncGetConnectionData() should be called when a call to AsyncWaitForConnection
	completes (when returnBlock->ioResult <= 0).  This call retrieves the information
	about this new connection and disposes the parameter block. */
	
OSErr AsyncGetConnectionData (TCPiopb *returnBlock, long *remoteHost,
	short *remotePort)
{
	ip_addr localHost;
	tcp_port localPort;
	
	return LowFinishTCPWaitForConn(returnBlock,(ip_addr *)remoteHost,
						(tcp_port *)remotePort,&localHost,&localPort);
}


/* 	IsAUX is a helper function used by CloseConnection to figure out whether 
	we are running under A/UX. */
	
static Boolean IsAUX (void)
{
	return ((*(short*)0xB22) & (1<<9)) != 0;
}


/* CloseConnection() terminates a connection to a remote host, given the
   stream identifier of the connection. It waits for the remote host to also
   close its end of the connection. */
   
OSErr CloseConnection (unsigned long stream)
{
	byte state;
	unsigned short length;
	CStr255 data;
	OSErr err;

	err = LowTCPClose(stream,10);
	if (err != noErr && err != connectionDoesntExist &&
		err != connectionClosing && err != connectionTerminated) goto exit;
	if (IsAUX()) return noErr;
	while (true) {
		if ((err = GetConnectionState(stream, &state)) != noErr) goto exit;
		if (state == 0) return noErr;
		if (!GiveTime()) {
			err = -1;
			goto exit;
		}
		length = 255;
		RecvData(stream,data,&length,true);
	}
	return noErr;

exit:

	AbortConnection(stream);
	return err;
}


/* AbortConnection() aborts a connection to a remote host, given the
   stream identifier of the connection */
   
OSErr AbortConnection (unsigned long stream)
{
	return LowTCPAbort(stream);
}


/* SendData() sends data along a connection stream to a remote host. */

OSErr SendData (unsigned long stream, Ptr data, unsigned short length)
{	
	struct wdsEntry myWDS[2];	/* global write data structure */

	myWDS[0].length = length;
	myWDS[0].ptr = data;
	myWDS[1].length = 0;
	myWDS[1].ptr = nil;
	return LowTCPSendData(stream,120,false,false,(Ptr) myWDS,false,nil);
}

/* SendMultiData() is similar to SendData, but takes an array of strings to send
   to the remote host. 

	A/UX versions previous to and including 3.0b15 have a fixed limit of
	6 WDS segments per TCPSend.
	So we enforce that limit since folks using SendMultiData may want
	to send more than 6 chucks of data.
*/

OSErr SendMultiData (unsigned long stream, CStr255 data[], short numData)
{
	struct wdsEntry theWDS[MAX_WDS+1];
	short i = 0, j, numToSend;
	OSErr err = noErr;

	while (numData > 0 && err == noErr) {
		numToSend = (numData > MAX_WDS) ? MAX_WDS : numData;
		for (j=0; j < numToSend; i++,j++) {
			theWDS[j].ptr = (Ptr)data[i];
			theWDS[j].length = strlen((Ptr)data[i]);
		}
		theWDS[numToSend].ptr = nil;
		theWDS[numToSend].length = 0;
		err = LowTCPSendData(stream,120,false,false,(Ptr)theWDS,false,nil);
		numData -= numToSend;
	}

	return err;
}


/* RecvData() waits for data to be received on a connection stream.  When data
   arrives, it is copied into the data buffer and the call terminates. */

OSErr RecvData (unsigned long stream, Ptr data, unsigned short *length,
	Boolean retry)
{
	Boolean	urgent,mark;
	OSErr	err;
	unsigned short recvLength;

	do {
		recvLength = *length;
		err = LowTCPRecvData(stream,40,&urgent,&mark,data,&recvLength,false,nil);
	}
	while (retry && err==commandTimeout);
	*length = recvLength;
#ifdef OLDEN	/* this was for pure ascii reads */
	if (err == noErr) *(data+*length) = 0;
#endif
	return err;
}


/*	GetConnectionState gets the connection state of a stream. */

OSErr GetConnectionState (unsigned long stream, byte *state)
{
	TCPStatusPB theStatus;
	OSErr err;
	
	err = LowTCPStatus(stream,&theStatus);
	if (err == connectionDoesntExist) {
		*state = 0;
		return noErr;
	}
	*state = theStatus.connectionState;
	return err;
}


/*	IPNameToAddr invokes the domain name system to translate a domain name
	into an IP address. */
	
OSErr IPNameToAddr (char *name, unsigned long *addr)
{
	OSErr err;
	short i;
	static struct {
		CStr255 name;
		unsigned long addr;
	} cache[10];
	static short numCache=0;
	
	for (i=0; i<numCache; i++) {
		if (strcmp(name, cache[i].name) == 0) {
			*addr = cache[i].addr;
			return noErr;
		}
	}
	if ((err = LowIPNameToAddr(name, addr)) != noErr) return err;
	if (numCache < 10) {
		strcpy(cache[numCache].name, name);
		cache[numCache].addr = *addr;
		numCache++;
	}
	return noErr;
}


/*	IPAddrToName invokes the domain name system to translate an IP address
	into a domain name. */
	
OSErr IPAddrToName (unsigned long addr, char *name)
{
	return LowIPAddrToName(addr, name);
}


/*	GetMyIPAddr returns the IP address of this Mac. */

OSErr GetMyIPAddr (unsigned long *addr)
{
	return LowGetMyIPAddr(addr);
}


/*	GetMyIPAddrStr returns the IP address of this Mac as a dotted decimal
	string. */
	
OSErr GetMyIPAddrStr (char *addrStr)
{
	return LowGetMyIPAddrStr(addrStr);
}


/*	GetMyIPName returns the domain name of this Mac. */

OSErr GetMyIPName (char *name)
{
	unsigned long addr;
	short len;
	static OSErr err;
	static Boolean gotIt=false;
	static CStr255 theName;
	
	if (!gotIt) {
		if ((err = LowGetMyIPAddr(&addr)) != noErr) return err;
		err = LowIPAddrToName(addr,theName);
		gotIt = true;
		len = strlen(theName);
		if (theName[len-1] == '.') theName[len-1] = 0;
	}
	if (err != noErr) return err;
	strcpy(name,theName);
	return noErr;
}
	
