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
File:		zclibase.hxx
Version:	1.00
Description:	Z39.50 client API
Author:		Kevin Gamiel, kevin.gamiel@cnidr.org
@@@*/

#ifndef _ZCLIENT_BASE_HXX_
#define _ZCLIENT_BASE_HXX_

#include "zsession.hxx"

class ZCLIENT_BASE : public ZSESSION {
private:
	void StoreDefaults(const STRING & Group);
	STRING	c_query,
		c_host,
		c_element_set_name;
	INT 	c_error;
	INT4	c_hitcount;
public:
	ZCLIENT_BASE();
	virtual ~ZCLIENT_BASE();

	//
	// All methods return GDT_TRUE on success, GDT_FALSE on failure.
	// Use GetLastError() to determine reason.
	//
	GDT_BOOLEAN Initialize( const STRING &Host, const UINT Port);
	GDT_BOOLEAN Initialize( const STRING &Host, const UINT Port,
				const STRING &GroupId,
				const STRING &UserId,
				const STRING &Password);
	GDT_BOOLEAN Initialize(	const STRING &Host, 
				const UINT Port,
				const STRING &RefId,
				const STRING &ProtVer,
				const STRING &Options,
				const INT4 PrefMsgSize,
				const INT4 ExceptionalRecSize,
				const STRING &GroupId,
				const STRING &UserId,
				const STRING &Password,
				const STRING &ImpId,
				const STRING &ImpName,
				const STRING &ImpVersion);

	virtual GDT_BOOLEAN Search(const STRING &Database,
				const STRING &Term,
				INT4 *HitCount);

	GDT_BOOLEAN Present(	const INT4 Start,
				const INT4 Count,
				const STRING &PreferredRecordSyntax,
				const STRING &ElementSetName,
				ZRECORDLIST *Records);

	GDT_BOOLEAN Add_Records(const STRING &Database,
				ZRECORDLIST *Records);

	GDT_BOOLEAN Delete_Records(const STRING &Database,
				ZRECORDLIST *Records);
/*
	INT Scan(const PCHR DatabaseName, const PCHR Term);
*/
	INT GetLastError() { return c_error; }
};

#endif
