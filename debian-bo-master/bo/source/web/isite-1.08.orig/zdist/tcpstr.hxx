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
File:		tcpstream.h
Version:        1.00
Description:    TCP/IP C++ stream class
Author:         Kevin Gamiel, Kevin.Gamiel@cnidr.org
@@@*/

/****************************************************************************
These headers closely resemble those included with the GNU libg++ distribution
in the file libg++-2.6.2/libio/fstream.h.
****************************************************************************/

#ifndef _tcpstream_
#define _tcpstream_

#include <iostream.h>
#include "tcpbuf.hxx"

class tcpstreambase : public iostream {
	tcpbuf c_tcpbuf;
	void __ib_init();
public:
	tcpstreambase();	
	
	void Open(const PCHR host, UINT port);
	void Close();

	INT IsConnected() const { return rdbuf()->IsConnected(); }
	
	void Listen(UINT port, INT backlog) { rdbuf()->Listen(port, backlog); }
	INT Accept(UNISOCK *NewSocket) { return rdbuf()->Accept(NewSocket); }

	TSOCKET Socket() { return rdbuf()->Socket(); }
	void BlockingModeON() { rdbuf()->BlockingModeON(); }
	void BlockingModeOFF() { rdbuf()->BlockingModeOFF(); }
protected:
	void setbuf(PCHR ptr, INT len) { rdbuf()->setbuf(ptr, len); }
	tcpbuf* rdbuf() const { return (tcpbuf*)&c_tcpbuf; }

};

// Input IP stream
class itcpstream : public tcpstreambase {
public:
	itcpstream() : tcpstreambase() { }

	void Open(const PCHR host, UINT port)
		{ tcpstreambase::Open(host, port); }
};

// Output IP stream
class otcpstream : public tcpstreambase {
public:
	otcpstream() : tcpstreambase() { }

	void Open(const PCHR host, UINT port)
		{ tcpstreambase::Open(host, port); }
};

class tcpstream : public tcpstreambase {
public:
	tcpstream() : tcpstreambase() { }
/*
	void SetSocket(UNISOCK & NewSocket, INT State=UNI_CONNECTED) 
		{ rdbuf()->SetSocket(NewSocket, State); }
*/
	void SetSocket(TCPSOCK & NewSocket) { rdbuf()->SetSocket(NewSocket); }
	void SetSocket(TSOCKET NewSocket) { rdbuf()->SetSocket(NewSocket); }

	void open(const PCHR host, UINT port)
		{ tcpstreambase::Open(host, port); }
	void close() { rdbuf()->Close(); }

	INT is_open() const { return rdbuf()->IsConnected(); }

	INT DataReady(INT4 SecondsToWait = 0) const { return 
		rdbuf()->DataReady(SecondsToWait); }
	UINT4 DataReadyCount() const { return rdbuf()->DataReadyCount(); }

	INT LastError() { return rdbuf()->LastError();}
	void ErrorMessage(INT err, PCHR msg, INT maxlen)
		{ rdbuf()->ErrorMessage(err, msg, maxlen);}
	void ErrorMessage(PCHR msg, INT maxlen) 
		{ rdbuf()->ErrorMessage(msg, maxlen);}
};

#endif

