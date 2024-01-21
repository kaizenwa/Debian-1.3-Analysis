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
File:		unisock.cxx
Version:	1.00
Description:	Multiplatform TCP/IP Socket code.
Author:		Kevin Gamiel, Kevin.Gamiel@cnidr.org
@@@*/

#include <iostream.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#ifdef UNIX
//#include <bstring.h>
#endif

#include "unisock.hxx"

#ifdef UNISOCK_WINSOCK
#define errno WSAGetLastError()
#define close(a) closesocket(a)
#endif


/*
These 2 definitions check to see if the socket is in an unconnected state
and if so, sets an error code and returns.  As with all the unisock methods,
the caller should always call UNISOCK::LastError() after each unisock call to 
determine the error state.
*/
#define RET_IF_SOCK_CLOSED if(c_state == UNI_UNCONNECTED) {\
	c_error = c_state;\
	return;\
	} else c_error = UNI_OK;

#define RET_VAL_IF_SOCK_CLOSED if(c_state == UNI_UNCONNECTED) {\
	c_error = c_state;\
	return 0;\
	} else c_error = UNI_OK;

/*
Desc:	Constructor for socket
Pre:	family = One of the supported address families (usually PF_INET)
		BSD - See /usr/include/sys/socket.h for AF_ values
		Winsock - Only supports PF_INET 

	comm_type - SOCK_STREAM | SOCK_DGRAM
		BSD - Supports SOCK_RAW

	protocol - 0 for no protocol (typical) or other value if you
		know what you are doing
Post:	Check UNISOCK::LastError() for error state.
	If error state == UNI_OK then a socket has been allocated
*/
UNISOCK::UNISOCK(INT family, INT comm_type, INT protocol = 0)
{
#ifdef IPDEBUG1
	printf("UNISOCK::UNISOCK():fam=%i, comm=%i, prot=%i\n",
		family, comm_type, protocol);
#endif

	c_family = family;
	c_comm_type = comm_type;
	c_protocol = protocol;
	c_error = UNI_OK;
	c_state = UNI_UNCONNECTED;
	c_reverse_name_lookup = 1;
	c_hostname = "";
	c_ipaddress = "";
	c_inetd_not_set_yet = GDT_TRUE;
	c_inetd = GDT_FALSE;

#ifdef UNISOCK_WINSOCK
	WORD VersionReqd;
	WSADATA WSAData;

	VersionReqd = 0x0101;
	WSAStartup(VersionReqd, &WSAData);
#endif
}

/*
Desc:	Destructor for socket
*/
UNISOCK::~UNISOCK()
{
#ifdef IPDEBUG1
	cerr << "UNISOCK::~UNISOCK()\n";
#endif

	if(c_state == UNI_CONNECTED)
		UNISOCK::Close();

#ifdef UNISOCK_WINSOCK
	WSACleanup();
#endif
}

/*
Desc:	Associates local address with remote address
Pre:	name = valid socket address for address family and protocol in use
Post:	Check UNISOCK::LastError() for error state.
	If error state == UNI_OK then connection was successful
*/
void UNISOCK::Connect(SOCKADDR *name)
{
#ifdef IPDEBUG1
	cerr << "UNISOCK::Connect()\n";
#endif

	c_error = UNI_OK;

	if((c_socket = socket(c_family, c_comm_type, c_protocol)) == 
		INVALID_SOCKET) {
		c_error = errno;
		return;
	}

	// Connect to remote host
	if(connect(c_socket, name, sizeof(*name)) == SOCKET_ERROR) {
		// Set error flag
		c_error = errno;
		return;
	}

#ifdef IPDEBUG1
	cerr << "UNISOCK::Connect():Connected\n";
#endif

	c_state = UNI_CONNECTED;
}

/*
Desc:	Send data to remote site
Pre:	Socket is connected
Post:	Returns SOCKET_ERROR on failure.  Call UNISOCK::LastError() for code.
	Otherwise, returns the actual number of bytes sent (may be less 
		than len)
*/
INT UNISOCK::Send(PCHR buf, INT len)
{
#ifdef IPDEBUG1
	cerr << "UNISOCK::Send()\n";
#endif

	RET_VAL_IF_SOCK_CLOSED

	INT err;
	err = send(c_socket, buf, len, 0);
	if(err == SOCKET_ERROR)
		c_error = errno;
	
	return err;
}

/*
Desc:	Reads up to len bytes from socket subsystem into buf
Pre:	Socket is connected
Post:	Returns 0 if remote side closed gracefully
	Returns SOCKET_ERROR on failure.  Call UNISOCK::LastError() for code.
	Otherwise returns the number of bytes copied into buf
*/
INT UNISOCK::Recv(PCHR buf, INT len)
{
#ifdef IPDEBUG1
	cerr << "UNISOCK::Recv():Want " << len << " bytes put into ";
	printf("%X", (UINT)buf);
	cerr << " from socket " << c_socket << "\n";
#endif

	RET_VAL_IF_SOCK_CLOSED

	INT err;
	err = recv(c_socket, buf, len, 0);

#ifdef IPDEBUG1
	cerr << "UNISOCK::Recv():Received " << err << " bytes\n";
#endif

	if(err == SOCKET_ERROR)
		c_error = errno;

	return err;
}

void UNISOCK::Close()
{
#ifdef IPDEBUG1
	cerr << "UNISOCK::Close()\n";	
#endif
	RET_IF_SOCK_CLOSED

	if(close(c_socket) == -1)
		c_error = errno;

	c_socket = (TSOCKET)-1;

	c_state = UNI_UNCONNECTED;
}

/*
Desc:	Polls socket for readability
Pre:	Socket is connected
	SecondsToWait - number of seconds to block (see Note)
Post:	Returns 1 if data available for guaranteed reading.
	Returns 0 if no data available.
	Returns SOCKET_ERROR if error.  Call UNISOCK::GetLastError() for 
		error code.
Note:	SecondsToWait of 0 will poll the socket and return instantly.
	SecondsToWait of -1 will block indefinitely
	Any other value of SecondsToWait will block only for that period of time
*/
INT UNISOCK::DataReady(INT4 SecondsToWait)
{
#ifdef IPDEBUG1
	cerr << "UNISOCK::DataReady()\n";	
#endif

	if(c_state == UNI_UNCONNECTED) {
		c_error = c_state;
		return SOCKET_ERROR;
	} else c_error = UNI_OK;

	// Initialize the set of sockets to check for readability
	fd_set ReadFDS;
	FD_ZERO(&ReadFDS);
	FD_SET(c_socket, &ReadFDS);
	// Set select time 
	struct timeval *TimeVal;
	if(SecondsToWait < 0)
		// Block indefinitely
		TimeVal = (struct timeval *)NULL;
	else {
		TimeVal = new timeval;
		TimeVal->tv_sec = SecondsToWait;
		TimeVal->tv_usec = 0;
	}
	
	// Check the socket for readability.
	INT err;
#ifdef _HP_
	err = select(c_socket + 1, (int *)&ReadFDS, (int *)NULL,
		(int *)NULL, TimeVal);
#else
	err = select(c_socket + 1, (fd_set *)&ReadFDS, (fd_set *)NULL,
		(fd_set *)NULL, TimeVal);
#endif
#ifdef IPDEBUG1
	cerr << "done with " << err << endl;
#endif
	if(TimeVal)
		delete TimeVal;

	switch(err) {
             	case SOCKET_ERROR:
                  	// Peer aborted connection
			c_error = errno;
			break;
		case 0:
              		// Timed out.  No data ready for reading
                   	 return 0;
		default:
			// _Some_ socket has data to be read 
                     if(FD_ISSET(c_socket, &ReadFDS) == 0)
                            // It wasn't our socket
                            return 0;

                     // Our socket has some data to read!
      			return 1;
	}

	return SOCKET_ERROR;
}

/*
Desc:	Determines the number of bytes that can be read immediately with 
		UNISOCK::Recv()
Pre:	Socket is connected
Post:	Returns >= 0 on success, indicating the number of bytes ready for
		reading with UNISOCK::Recv()
	Returns SOCKET_ERROR on error.  Call UNISOCK::GetLastError() for 
		error code
*/
INT4 UNISOCK::DataReadyCount()
{
#ifdef IPDEBUG1
	cerr << "UNISOCK::DataReadyCount()\n";
#endif

	RET_VAL_IF_SOCK_CLOSED

	INT Err;
#ifdef UNISOCK_WINSOCK
	u_long BytesAvail;
#else
	UINT BytesAvail;
#endif

#ifdef UNISOCK_WINSOCK
	Err = ioctlsocket(c_socket, FIONREAD, &BytesAvail);
#else
	Err = ioctl(c_socket, FIONREAD, (PCHR)((UINT *)&BytesAvail));
#endif

	if(Err == SOCKET_ERROR) {
		c_error = errno;
		return Err;
	}

#ifdef IPDEBUG1
	cerr << "UNISOCK::DataReadyCount():" << BytesAvail << " bytes ready\n";
#endif

	return BytesAvail;
}

void UNISOCK::ErrorMessage(PCHR msg, INT maxlen)
{
#ifdef IPDEBUG1
	cerr << "UNISOCK::ErrorMessage(PCHR,INT)\n";
#endif
	ErrorMessage(c_error, msg, maxlen);
}

void UNISOCK::ErrorMessage(INT err, PCHR msg, INT maxlen)
{
#ifdef IPDEBUG1
	cerr << "UNISOCK::ErrorMessage(INT,PCHR,INT)\n";
#endif

	// err is either one we define in unisock.hxx or errno
	switch(err) {
		case UNI_UNCONNECTED:
			strncpy(msg, "Socket is closed", maxlen);
			break;
		default: {
			PCHR p = strerror(err);
			if(!p)
				strncpy(msg, "Unknown Error", maxlen);
			else 
				strncpy(msg, p, maxlen);
		}
	}	

	// Append the error value to the error message
	CHR errstr[16];
	sprintf(errstr, "[%i]", err);
	INT n = strlen(errstr);
	strncat(msg, errstr, n);
}

void UNISOCK::Listen(UINT port, INT backlog)
{
	INT val = 1;
	struct sockaddr_in name_in;

	if((c_socket = socket(c_family, c_comm_type, c_protocol)) == 
		INVALID_SOCKET) {
		c_error = errno;
		return;
	}

	// Bind to the port, then listen

	memset(&name_in, 0, sizeof(name_in));
 
	name_in.sin_family = AF_INET;
	name_in.sin_addr.s_addr = htonl(INADDR_ANY);
	name_in.sin_port = htons(port);
	
	setsockopt(c_socket, SOL_SOCKET, SO_REUSEADDR, (PCHR)&val, sizeof(INT));

	if(bind(c_socket, (SOCKADDR *)&name_in, sizeof(name_in)) == 
		SOCKET_ERROR ) {
		c_error = errno;
		return;
	}

	if (listen(c_socket, backlog) == SOCKET_ERROR) {
		c_error = errno;
		return;
	}
}

//
// 1 on success, -1 on failure
//
INT UNISOCK::Accept(UNISOCK *NewSocket)
{
	struct sockaddr_in name_in;
	INT name_in_size;
	TSOCKET s;

	name_in_size = sizeof(name_in);

	c_error = UNI_OK;
	do {
		if((s = accept(c_socket, (SOCKADDR *)&name_in, 
			&name_in_size)) == INVALID_SOCKET) {
			if(errno != EINTR) {
				c_error = errno;
				return -1;
			}
                }
        } while (errno == EINTR);

	NewSocket->SetIPAddress(inet_ntoa(name_in.sin_addr));
	NewSocket->SetSocket(s);

	if(c_reverse_name_lookup) {
		// Determine the hostname.
		struct hostent *peer;
		peer = gethostbyaddr((char *)&name_in.sin_addr, 
			sizeof(name_in.sin_addr), AF_INET);
		if(peer)
			NewSocket->SetHostname((CHR *)peer->h_name);
		else
			NewSocket->SetHostname(inet_ntoa(name_in.sin_addr));
	}

	return 1;
}

void UNISOCK::SetSocket(TSOCKET socket, INT State)
{
	c_socket = socket;
	c_state = State;
}

void UNISOCK::BlockingMode(INT f)
{
#ifdef UNISOCK_WINSOCK
  u_long flag;
#else
  UINT flag;
#endif
  INT err;

  c_error = UNI_OK;

  if(f == 0)
    flag = 0;
  else
    flag = 1;

//	if((err = ioctl(c_socket, FIONBIO, &flag)) == SOCKET_ERROR)
#ifdef UNISOCK_WINSOCK
  err = ioctlsocket(c_socket, FIONBIO, &flag);
#else
  err = ioctl(c_socket, FIONBIO, &flag);
#endif
  
  if(err == SOCKET_ERROR)
    c_error = errno;

}

UNISOCK & UNISOCK::operator=(const UNISOCK & Other)
{
	c_family = Other.c_family;
	c_comm_type = Other.c_comm_type;
	c_protocol = Other.c_protocol;
	c_error = Other.c_error;
	c_state = Other.c_state;
	c_reverse_name_lookup = Other.c_reverse_name_lookup;
	c_hostname = Other.c_hostname;
	c_ipaddress = Other.c_ipaddress;
	c_socket = Other.c_socket;

	return *this;
}

GDT_BOOLEAN UNISOCK::StartedByInetd()
{
	if(c_inetd_not_set_yet)
		c_inetd = StartedByInetd();
	else c_inetd_not_set_yet = GDT_TRUE;
	return c_inetd;
}

GDT_BOOLEAN StartedByInetd()
{
	struct sockaddr_in Name;
	INT NameLen;
	NameLen = sizeof(Name);
	if(getpeername(fileno(stdout), (SOCKADDR *)&Name, &NameLen) == -1) {
		return GDT_FALSE;
	} else {
		return GDT_TRUE;
	}
}

