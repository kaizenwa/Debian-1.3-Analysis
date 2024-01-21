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
File:		tcpsock.cxx
Version:	1.00
Description:	TCP/IP Socket class
Author:		Kevin Gamiel, Kevin.Gamiel@cnidr.org
@@@*/

#include <iostream.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "unisock.hxx"
#include "tcpsock.hxx"

// Local prototypes
INT socket_close(INT socket);

TCPSOCK::TCPSOCK():UNISOCK(PF_INET, SOCK_STREAM, 0)
{
	c_backlog = 5;
}

TCPSOCK::~TCPSOCK()
{
}

void TCPSOCK::Open(const PCHR host, UINT port)
{
	struct sockaddr_in name_in;
	struct hostent *hostentry;
	UINT4 addr;
	INT4 taddr;
#ifdef IPDEBUG1
	cout << "TCPSOCK::Open()\n";	
#endif

	// Resolve hostname
	addr = inet_addr(host);
	taddr = (INT)addr;
	if(taddr != -1) {
		// host value has proper IP address syntax
		hostentry = gethostbyaddr((char *)&addr, (INT)sizeof(addr), 
			AF_INET);
		if(hostentry == NULL) {
			// Set error flag
			perror(host);
			return;
		}
	} else {
		// host does not have proper IP address syntax.
		// We assume its a host name
		if((hostentry = gethostbyname(host)) == NULL) {
			// Set error flag
			perror(host);
			return;
		}
             	memcpy(&addr, hostentry->h_addr_list[0], (INT4)sizeof(addr));
	}
	// We now have a valid addr value

	CHR **p = hostentry->h_addr_list;
	struct in_addr in;
	memcpy(&in.s_addr, *p, (INT4)sizeof (in.s_addr));
	c_ipaddress = inet_ntoa(in);
	c_hostname = hostentry->h_name;

	/*
	This code lists aliases
         for (p = hostentry->h_addr_list; *p != 0; p++) {
             struct in_addr in;
             char **q;
             (void) memcpy(&in.s_addr, *p, (INT4)sizeof (in.s_addr));
             (void) printf("%s\t%s", inet_ntoa(in), hostentry->h_name);
	printf("\naliases:\n");
             for (q = hostentry->h_aliases; *q != 0; q++)
                 (void) printf(" %s", *q);
             (void) putchar('\n');
         }
	*/

	// Initialize the internet address structure	
	memset((char*)&name_in, 0, (INT4)sizeof(name_in));
	name_in.sin_family = AF_INET;
	name_in.sin_port = htons(port);
	name_in.sin_addr.s_addr = addr;
	
	// Connect to remote host
	Connect((SOCKADDR *)&name_in);

}

TCPSOCK & TCPSOCK::operator=(const TCPSOCK & Other)
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
	c_backlog = Other.c_backlog;

	return *this;
}
