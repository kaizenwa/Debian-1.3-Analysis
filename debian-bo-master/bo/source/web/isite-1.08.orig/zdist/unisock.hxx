/************************************************************************
Copyright Notice

Copyright (c) MCNC, Clearinghouse for Networked Information Discovery and
Retrieval, 1994. 

Permission to use, copy, modify, distribute, and sell this software and
its documentation, in whole or in part, for any purpose is hereby granted
without fee, provided that

1. The above copyright notice and this permission notice appear in all
copies of the software and related documentation. Notices of copyright
and/or attribution which appear at the beginning of any file included in
this distribution must remain intact. 

2. Users of this software agree to make their best efforts (a) to return
to MCNC any improvements or extensions that they make, so that these may
be included in future releases; and (b) to inform MCNC/CNIDR of noteworthy
uses of this software. 

3. The names of MCNC and Clearinghouse for Networked Information Discovery
and Retrieval may not be used in any advertising or publicity relating to
the software without the specific, prior written permission of MCNC/CNIDR. 

THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND,
EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY WARRANTY
OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. 

IN NO EVENT SHALL MCNC/CNIDR BE LIABLE FOR ANY SPECIAL, INCIDENTAL,
INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND, OR ANY DAMAGES WHATSOEVER
RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER OR NOT ADVISED OF THE
POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF LIABILITY, ARISING OUT OF OR
IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. 
************************************************************************/

/*@@@
File:		unisock.hxx
Version:	1.00
Description:	Multiplatform TCP/IP socket code.
Author:		Kevin Gamiel, kevin.gamiel@cnidr.org
Note:		Currently supports BSD and Winsock
@@@*/

#ifndef _UNISOCK_
#define _UNISOCK_

/*
Porting Information

You should define the appropriate value depending on your platform.
You're choices are:

#define UNISOCK_WINSOCK
#define UNISOCK_BSD
#define UNISOCK_MACTCP

I'll take a stab at figuring out what platform you are compiling on first
and if I fail, the compiler should bail out.
*/
// MS VC++
#if (defined _WINDOWS) | (defined _Windows)
#define UNISOCK_WINSOCK
#endif

#if (defined UNIX) | (defined unix)
#define UNISOCK_BSD
#define BSD_COMP
#endif

#if (defined THINKC)
#define UNISOCK_MACTCP
#endif

#ifndef __OBJECTCENTER__
#if (!defined UNISOCK_WINSOCK) & (!defined UNISOCK_BSD) & (!defined UNISOCK_MACTCP)
#error YOU MUST DEFINE A UNISOCK PLATFORM - See unisock.hxx
#endif
#endif

#ifdef UNISOCK_BSD
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
extern "C" {
#include <netdb.h>
}
#include <sys/ioctl.h>
#include <unistd.h>
#include <sys/uio.h>
#include <sys/time.h>
#define TSOCKET INT
#define SOCKADDR struct sockaddr
#define SOCKET_ERROR (-1)
#define INVALID_SOCKET (TSOCKET)(~0)
#endif

#ifdef UNISOCK_WINSOCK
#include "winsock.h"
#define TSOCKET SOCKET
#endif
#include "string.hxx"
#include "gdt.h"

// These values MUST NOT collide with errno values
#define UNI_BASE 2500
#define UNI_OK 0
#define UNI_UNCONNECTED UNI_BASE + 1
#define UNI_CONNECTED UNI_BASE + 2
#define UNI_PEERABORT UNI_BASE + 3

class UNISOCK {
protected:
	TSOCKET c_socket;
	INT 	c_error,
		c_state,
		c_family,
		c_comm_type,
		c_protocol,
		c_reverse_name_lookup;
	STRING	c_hostname,
		c_ipaddress;
	GDT_BOOLEAN	c_inetd,
			c_inetd_not_set_yet;
public:
	UNISOCK(INT family, INT comm_type, INT protocol);
	~UNISOCK();

	UNISOCK & operator=(const UNISOCK & Other);

	void SetHostname(const CHR *Hostname) { c_hostname = Hostname; }
	void GetHostname(STRING *Hostname) { *Hostname = c_hostname; }
	void SetIPAddress(CHR *Address) { c_ipaddress = Address; }
	void GetIPAddress(STRING *Address) { *Address = c_ipaddress; }

	// For TCP, connects to host.  For UDP, stores address for
	// subsequent sending of data.	
	void Connect(SOCKADDR *name);

	void Listen(UINT Port, INT backlog);
	INT Accept(UNISOCK *NewSocket);
	INT IsConnected() { return c_state == UNI_CONNECTED; }
	void Close();

	INT Send(PCHR buf, INT len);
	INT Recv(PCHR buf, INT len);

	// Determine if and how much data is ready.
	INT DataReady(INT4 SecondsToWait = 0);
	INT4 DataReadyCount();
	
	// Simple error handling	
	INT LastError() { return c_error; }

	void ErrorMessage(PCHR msg, INT maxlen);
	void ErrorMessage(INT err, PCHR msg, INT maxlen);
	
	TSOCKET Socket() const { return c_socket; }
	void SetSocket(TSOCKET NewSocket, INT State=UNI_CONNECTED);
//	void SetSocket(INT NewSocket, INT State=UNI_CONNECTED);

	void BlockingMode(INT f);
	void BlockingModeON() { BlockingMode(0); }
	void BlockingModeOFF() { BlockingMode(1); }

	GDT_BOOLEAN StartedByInetd();
};

GDT_BOOLEAN StartedByInetd();

// Well defined port numbers

// UDP Ports
#define PORT_ECHO 7
#define PORT_DISCARD 9
#define PORT_USERS 11
#define PORT_DAYTIME 13
#define PORT_NETSTAT 15
#define PORT_QUOTE 17
#define PORT_CHARGEN 19
#define PORT_TIME 37
#define PORT_NAMESERVER 42
#define PORT_NICNAME 43
#define PORT_DOMAIN 53
#define PORT_BOOTPS 67
#define PORT_BOOTPC 68
#define PORT_TFTP 69
#define PORT_SUNRPC 111
#define PORT_NTP 123
#define PORT_SNMP 161
#define PORT_SNMP_TRAP 162
#define PORT_BIFF 512
#define PORT_WHO 513
#define PORT_SYSLOG 514
#define PORT_TIMED 525

#endif

