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
File:		tcpbuf.cxx
Version:	1.00
Description:	TCP/IP Buffer class for C++ streams.
Author:		Kevin Gamiel, Kevin.Gamiel@cnidr.org
@@@*/

#include <iostream.h>
#include <stdio.h>
#include "tcpbuf.hxx"

tcpbuf::tcpbuf()
{
#ifdef IPDEBUG1
	cout << "tcpbuf::tcpbuf()\n";	
	DumpPointers();
#endif
}

tcpbuf::~tcpbuf()
{
#ifdef IPDEBUG1
	cout << "tcpbuf::~tcpbuf()\n";	
#endif
	if(c_socket.IsConnected())
		c_socket.Close();
}

tcpbuf *tcpbuf::Open(const PCHR host, UINT port)
{
	c_socket.Open(host, port);
	return this;
}

tcpbuf *tcpbuf::Close()
{
	c_socket.Close();
	return this;
}

/*
Write out all characters pending in the "put" area
*/
int tcpbuf::overflow(int c)
{
	INT4 count = out_waiting();

#ifdef IPDEBUG1
	cout << "tcpbuf::overflow():Trying to flush " << count << " bytes...\n";
#endif
	// flush all pending characters out
	c_socket.Send(pbase(), count);
	if(c != EOF) {
		char b[1];
		b[0] = c;
		c_socket.Send(b,1);
	}

	if(c_socket.LastError())
		return EOF;
		
	setp(pbase(), epptr());

#ifdef IPDEBUG1
	cout << "tcpbuf::overflow():Done\n";
#endif

	return 0;
}

/*
According to the description of underflow in MS VC++ iostream Class Library 
Reference, p. 125, our options for underflow include:

	- if data already in get area, return first char
	- if no data in get area, fill it and return first char
	- if no data left to read, return EOF

That implies that I should block waiting for at least one character to
be read from the socket before returning.  I would rather not do that, but
I will for now until I figure out a way around it.
*/
int tcpbuf::underflow()
{
	UINT4 count;
	INT bytes_read = 0;

#ifdef IPDEBUG1
	cout << "tcpbuf::underflow()\n";	
#endif

	sync();

	/* 
	If the get area contains any data, return the first character
	in the get area.
	*/
	if(in_avail())
		return *gptr();

	INT err;
/*
	for(;;) {
		err = c_socket.DataReady();
#ifdef IPDEBUG1
		cout << "tcpbuf::underflow():DataReady returned ";
		cout << err << "\n";
#endif
		if(err == SOCKET_ERROR)
			return EOF;
		if(err > 0)
			break;
	} 
*/
err = c_socket.DataReady();
if(err == SOCKET_ERROR)
	return EOF;
if(err == 0)
	return 0;


	/*
	We think we now have some data waiting on the socket.
	Do we really?  If so, how much?
	*/
	if((count = c_socket.DataReadyCount()) <= 0) {
		// Remote socket closed
		c_socket.Close();
		return EOF;
	}

	/*
	Limit count to less than or equal to the size of our reserve area.
	*/
	count = count < blen() ? count : blen();

#ifdef IPDEBUG1
	cout << "tcpbuf::underflow():There are " << count;
	cout << " bytes ready for reading...\n";
	DumpPointers();
#endif

	/*
	Read the waiting data into the get area.
	*/
	if((bytes_read = c_socket.Recv(eback(), count)) == -1) {
		// Set error bit
#ifdef IPDEBUG1
		cout << "tcpbuf::underflow():recv returned -1\n";
#endif
		return EOF;
	}

	/*
	Reset the get area to reflect the data we just read
	*/
	setg(eback(), eback(), egptr() + bytes_read);

#ifdef IPDEBUG1
	DumpPointers();
#endif
	
	return *gptr();
}

int tcpbuf::sputc(int c)
{
#ifdef IPDEBUG1
	cout << "tcpbuf::sputc()\n";	
#endif
	cout << c;

	return 1;
}

int tcpbuf::sync()
{
	INT4 count = out_waiting();

#ifdef IPDEBUG1
	cout << "tcpbuf::sync()\n";	
	cout << count << " waiting for output\n";
#endif

	if (count)
		if(overflow(EOF) == EOF)
			return EOF;

	setg(eback(), eback(), eback());
	setp(pbase(), epptr());

	return 0;
}
