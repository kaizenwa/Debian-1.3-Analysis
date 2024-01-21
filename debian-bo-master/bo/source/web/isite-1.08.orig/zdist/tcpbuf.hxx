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
File:		tcpbuf.hxx
Version:	1.00
Description:	Class tcpbuf
Author:		Kevin Gamiel, Kevin.Gamiel@cnidr.org
@@@*/

#ifndef _tcpbuf_
#define _tcpbuf_

#include <iostream.h>

#include "unisock.hxx"
#include "gdt.h"
#include "string.hxx"
#include "ipbuf.hxx"
#include "tcpsock.hxx"

#define IPS_CLOSED 0
#define IPS_OPEN 1

class tcpbuf : public ipbuf {
	friend class tcpstream;
	TCPSOCK c_socket;
public:
	tcpbuf();		
	~tcpbuf();		

	tcpbuf *Open(const PCHR host, UINT port);
	tcpbuf *Close();
	void Listen(UINT port, INT backlog) { c_socket.Listen(port, backlog); }
	INT Accept(UNISOCK *NewSocket) { return c_socket.Accept(NewSocket); }
	void BlockingModeON() { c_socket.BlockingModeON(); }
	void BlockingModeOFF() { c_socket.BlockingModeOFF(); }

	INT IsConnected() { return c_socket.IsConnected(); }

	void SetSocket(TSOCKET NewSocket, INT State=UNI_CONNECTED) 
		{ c_socket.SetSocket(NewSocket, State); }
	void SetSocket(TCPSOCK & NewSocket) { c_socket = NewSocket; }

	TSOCKET Socket() { return c_socket.Socket(); }
	INT DataReady(INT4 SecondsToWait = 0) { return 
		c_socket.DataReady(SecondsToWait); }
	UINT4 DataReadyCount() { return c_socket.DataReadyCount(); }
	INT LastError() { return c_socket.LastError(); }
	void ErrorMessage(INT err, PCHR msg, INT maxlen) 
		{ c_socket.ErrorMessage(err, msg, maxlen); }
	void ErrorMessage(PCHR msg, INT maxlen) 
		{ c_socket.ErrorMessage(msg, maxlen); }

	INT overflow(INT c = EOF);
	INT sputc(INT c);
	INT underflow();
	INT sync();
protected:
};

#endif

