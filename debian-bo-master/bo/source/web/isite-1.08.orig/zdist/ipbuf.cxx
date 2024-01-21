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
File:		ipbuf.cxx
Version:	1.00
Description:	Class ipbuf
Author:		Kevin Gamiel, Kevin.Gamiel@cnidr.org
@@@*/

#include "ipbuf.hxx"

int ip_close(int socket);

char *prolog = "test";

/*
This method is provided for convenience in initializing those things common
to our various forms of constructors.
*/
inline
void ipbuf::init()
{
#ifdef IPDEBUG1
	cout << "ipbuf::init()\n";	
#endif
	c_error = 0;
	
	int size = (int)sizeof(c_buffer);

	// Set the reserve area
	streambuf::setbuf(c_buffer, size);

	/*
	Set the put and get pointers.  They share the reserve area.
	*/
	setp(c_buffer, c_buffer + size);
	setg(c_buffer, c_buffer, c_buffer);
}

ipbuf::ipbuf()
{
#ifdef IPDEBUG1
	cout << "ipbuf::ipbuf()\n";	
#endif
	init();
}

ipbuf::~ipbuf()
{
#ifdef IPDEBUG1
	cout << "ipbuf::~ipbuf()\n";	
#endif
}

void ipbuf::DumpPointers()
{
	printf("\nTCP BUFFER MAP (all values in hex)\n");
	printf("------------------------------------\n");
	printf("RESERVE:\t%X <---- %X bytes ----> %X\n", (UINT)base(), 
		(UINT)blen(), (UINT)ebuf());
	printf("PUT:\t\t%X <---- %X ----> %X\t\t", (UINT)pbase(), (UINT)pptr(),
		 (UINT)epptr());
	if(epptr() != pbase())
		printf("Space Avail\n");
	else
		printf("No Space Avail\n");

	printf("GET:\t\t%X <---- %X ----> %X\t\t", (UINT)eback(), (UINT)gptr(),
		 (UINT)egptr());
	if(egptr() == eback()) {
		printf("Empty\n\n");
		return;
	}
	if(egptr() != eback())
		printf("Data Ready\n\n");
}

