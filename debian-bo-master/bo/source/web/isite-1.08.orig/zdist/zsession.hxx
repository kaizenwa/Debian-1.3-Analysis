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
File:		zsession.hxx
Version:	1.00
Description:	Z39.50 Session.  Base class for ZCLIENT and ZSERVER.
Author:		Kevin Gamiel, kevin.gamiel@cnidr.org
@@@*/

#ifndef _ZSESSION_H_
#define _ZSESSION_H_

#include "config.hxx"
#include "tcpsock.hxx"
#include "zpdu.hxx"
#include "registry.hxx"

#define ZPREFMSGSIZE 32768
#define ZMAXRECORDSIZE 8388608
#define ZPROTVER "yyy"
#define ZOPTIONS "yynnnnnnnnnnnnn"
#define CNIDR_IMP_ID "34"

class ZSESSION {
protected:
	TCPSOCK 	*c_tcp;	// network connection
	INT 		c_debuglevel;
	enum 		{ open, closed } c_state;
	INT4 		c_prefmsgsize,
			c_maxrecordsize,
			c_timeout;
	UINT		c_port;
	STRING		c_protocol_version,
			c_options,
			c_refid,
			c_group,
			c_inifile,
			c_dbname;
	PUCHR 		ReadPDU(INT4 *len, unsigned short *type);
	INT4 		SendPDU(PZPDU pdu);
	REGISTRY	*c_defaults;
	void LoadDefaultsFromFile(const STRING & Filename,const STRING & Group);
	void LoadDefaultsFromCommandLine(int ac, char **argv, STRING & File,
        	STRING & Group, STRING *LastGroup, STRING *LastFile);

	void SetProtocolVersion(const STRING & Value) 
		{ c_protocol_version = Value; }

	void SetPreferredMessageSize(const INT4 Value) 
		{ c_prefmsgsize = Value; }
	void SetExceptionalRecordSize(const INT4 Value) 
		{ c_maxrecordsize = Value; }

	//
	// Options
	//
	void ClearOptions();
	void SetOption(const INT Option, const GDT_BOOLEAN Value);
	void SetOptions(const STRING & Options);

	enum {	SEARCH,
		PRESENT,
		DELSET,
		RESOURCEREPORT,
		TRIGGERRESOURCECTRL,
		RESOURCECTRL,
		ACCESSCTRL,
		SCAN,
		SORT,
		RESERVED,
		EXTENDEDSERVICES,
		LEVEL1SEG,
		LEVEL2SEG,
		CONCURRENTOPS,
		NAMEDRESULTSETS };
	//
	// Misc
	//
	void PrintConnectionDetails();

public:
	ZSESSION();
	~ZSESSION();

	//
	// Shared by clients and servers
	//
	INT Close();

	void SetDebugLevel(INT Level) { c_debuglevel = Level; }

	GDT_BOOLEAN GetOption(const INT Option);
	void GetProtocolVersion(INT *Version);
};

#endif
