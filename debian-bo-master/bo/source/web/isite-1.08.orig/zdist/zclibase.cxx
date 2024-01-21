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
to MCNC any improvements or Extensions that they make, so that these may
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
File:		zclibase.cxx
Version:	1.00
Description:	Z39.50 client API
Author:		Kevin Gamiel, kevin.gamiel@cnidr.org
@@@*/

#include <iostream.h>
#include <stdlib.h>
#include "zclibase.hxx"

ZCLIENT_BASE::ZCLIENT_BASE() : ZSESSION()
{
	c_hitcount = 0;
	c_error = 0;
}


ZCLIENT_BASE::~ZCLIENT_BASE()
{
}


GDT_BOOLEAN ZCLIENT_BASE::Initialize(const STRING &Host, const UINT Port)
{
	return Initialize(Host, Port, "",ZPROTVER,c_options,1048576,1048576,
		"","","", "34","zclient_base",ZDIST_VERSION);
}

GDT_BOOLEAN ZCLIENT_BASE::Initialize(const STRING &Host, const UINT Port,
	const STRING & GroupId, const STRING & UserId, const STRING & Password)
{
	return Initialize(Host, Port, "",ZPROTVER, c_options,1048576,1048576,
		GroupId, UserId, Password, "34","zclient_base",ZDIST_VERSION);
}

GDT_BOOLEAN ZCLIENT_BASE::Initialize(const STRING &Host, 
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
			const STRING &ImpVersion)
{
	CHR msg[512], *tmpbuf;
	UCHR *buf;
	ZINITREQUEST *Request;
	STRING Value;
	INT4 NumValue;

	c_error = 0;

	if(c_tcp != NULL)
		delete c_tcp;

	c_tcp = new TCPSOCK();

	tmpbuf = Host.NewCString();
	c_tcp->Open(tmpbuf, Port);
	delete [] tmpbuf;
        if((c_error = c_tcp->LastError())) {
                c_tcp->ErrorMessage(c_error, msg, (sizeof(msg) - 1));
                cerr << "Initialize:" << msg << endl;
		return GDT_FALSE;
        }
	if(c_debuglevel >= 5) {
		cerr << "Connection open to " << Host << "[" << Port << "]";
		cerr << endl;
	}
	
	Request = new ZINITREQUEST(RefId,ProtVer,Options,PrefMsgSize,
		ExceptionalRecSize, GroupId, UserId, Password,
		ImpId, ImpName, ImpVersion);

	if(!SendPDU(Request)) {
		delete Request;
		return GDT_FALSE;
	}

	delete Request;

	//
	// Read the response PDU
	//
	INT4 len;
	unsigned short type;
	if((buf = ReadPDU(&len, &type)) == NULL)  {
		c_tcp->Close();
		return GDT_FALSE;
	}

	//
	// Is it an Init Response?
	//
	if(type != INITRESPONSE_TAG) {
		c_tcp->Close();
		delete [] buf;
		cerr << "Expected init response, got " << type << "\n";
		c_error = 9999;
		return GDT_FALSE;
	}

	//
	// Did they accept our Init Request?
	//
	ZINITRESPONSE *Response;
	Response = new ZINITRESPONSE(buf, len);
	delete [] buf;	
	GDT_BOOLEAN return_val = GDT_FALSE;
	if(Response->HasResult()) {
		INT4 num;
		Response->GetResult(&num);
		if(num) {
			c_state = open;
			return_val = GDT_TRUE;
		} else {
			c_error = 9998;
			c_tcp->Close();
			return_val = GDT_FALSE;
		}
	} else {
		c_error = 9997;
		c_tcp->Close();
		return_val = GDT_FALSE;
	}

	//
	// What options do they support?
	//
	if(Response->HasOptions()) {
		Response->GetOptions(&Value);
		SetOptions(Value);
	} else SetOptions(ZOPTIONS);

	//
	// What protocol version will we be using?
	//
	if(Response->HasProtocolVersion()) {
		Response->GetProtocolVersion(&Value);
		SetProtocolVersion(Value);
	} else SetProtocolVersion(ZPROTVER);

	//
	// Message sizes
	//
	if(Response->HasExceptionalRecordSize()) {
		Response->GetExceptionalRecordSize(&NumValue);
		SetExceptionalRecordSize(NumValue);
	} else SetExceptionalRecordSize(ZMAXRECORDSIZE);

	if(Response->HasPreferredMessageSize()) {
		Response->GetPreferredMessageSize(&NumValue);
		SetPreferredMessageSize(NumValue);
	} else SetPreferredMessageSize(ZPREFMSGSIZE);

	if(c_debuglevel >= 5)
		PrintConnectionDetails();

	delete Response;

	return return_val;
}

GDT_BOOLEAN ZCLIENT_BASE::Search(const STRING &Database, const STRING &Term,
	INT4 *HitCount)
{
	ZSEARCHREQUEST *s;
	INT4 len;
	INT4 SearchStatus;
	PUCHR buf;

	*HitCount = 0;

	s = new ZSEARCHREQUEST("",0,1,0,1,"Default",Database,"","",
		"", Term, BIB1_ATTRSET_OID);

	if(!SendPDU(s)) {
		delete s;
		return GDT_FALSE;
	}
	delete s;
	
	unsigned short type;
	if((buf = ReadPDU(&len, &type)) == NULL)  {
		c_tcp->Close();
		return GDT_FALSE;
	}
	
	if(type != SEARCHRESPONSE_TAG) {
		c_tcp->Close();
		delete [] buf;
		cerr << "Expected search response, got " << type << "\n";
		return GDT_FALSE;
	}

	ZSEARCHRESPONSE *r;
	r = new ZSEARCHRESPONSE(buf, len);
	delete [] buf;
	r->GetResultCount(HitCount);
	r->GetSearchStatus(&SearchStatus);

	delete r;
	return SearchStatus ? GDT_TRUE : GDT_FALSE;	
}

/*
INT ZCLIENT::Scan(const PCHR DatabaseName, const PCHR Term)
{
	ZSCANREQUEST *s;
	INT4 len;
	PUCHR buf;

	s = new ZSCANREQUEST("",DatabaseName, BIB1_ATTRSET_OID, Term,0,5,2);	

	if(!SendPDU(s))
		return 0;

	unsigned short type;
	if((buf = ReadPDU(&len, &type)) == NULL)  {
		c_tcp->Close();
		return 0;
	}
	if(type != SCANRESPONSE_TAG) {
		c_tcp->Close();
		delete [] buf;
		cerr << "Expected scan response, got " << type << "\n";
		return 0;
	}

	ZSCANRESPONSE *r;
	r = new ZSCANRESPONSE(buf, len);
	delete [] buf;
	delete s;
	return 1;	
}
*/

GDT_BOOLEAN ZCLIENT_BASE::Present(const INT4 Start, const INT4 Count,
			const STRING &PreferredRecordSyntax, 
			const STRING &ElementSetName,
			ZRECORDLIST *Records)
{
	ZPRESENTREQUEST *s;
	INT4 len;
	PUCHR buf;

	s = new ZPRESENTREQUEST("", "Default", Start, Count,
		ElementSetName, PreferredRecordSyntax);

	if(!SendPDU(s)) {
		delete s;
		return GDT_FALSE;
	}

	delete s;
	unsigned short type;
	if((buf = ReadPDU(&len, &type)) == NULL)  {
		c_tcp->Close();
		return GDT_FALSE;
	}
	if(type != PRESENTRESPONSE_TAG) {
		c_tcp->Close();
		delete [] buf;
		cerr << "Expected present response, got " << type << "\n";
		return GDT_FALSE;
	}

	//
	// Build a ZRECORDLIST from the BER
	//
	ZPRESENTRESPONSE *r;
	r = new ZPRESENTRESPONSE(buf, len);
	delete [] buf;

	// How many records returned?
	INT4 i;
	r->GetNumberOfRecordsReturned(&i);

	if(i == 0)
		// Everything is OK, but no records returned
		return GDT_TRUE;

	// Do we have response records, nonsurrogatediag or multiple-nonsur?
	BERBROWSER *b;
	b = new BERBROWSER(r);
	if(b->HasTag(RESPONSERECORDS_TAG)) {
		// We have real records
		BERBROWSER *sub;
		sub = new BERBROWSER(*b);
		INT4 j, count;
		b->GetSubdirectory(RESPONSERECORDS_TAG, b);
		count = b->GetSubdirectoryCount();
		STRING DBName, OID, Data;
		for(j=0;j < count;j++) {
			ZNAMEPLUSRECORD *npr;
			b->GetSubdirectoryX(j, sub);
			sub->GetChar(0, &DBName);
			//
			// What kind of record?
			//
			sub->GetSubdirectory(1, sub);
			if(sub->HasTag(RETRIEVALRECORD_TAG)) {
				ASN1TAGU Tag(ASN1_EXTERNAL);
				sub->GetSubdirectory(1, sub);
				sub->GetSubdirectory(Tag, sub);
				Tag.Number = ASN1_OBJECTIDENTIFIER;
				sub->GetOID(Tag, &OID);

				// USMARC or SUTRS?
				if(OID == USMARC_OID) {
					sub->GetChar(1, &Data);
					ZUSMARCRECORD *s;
					s = new ZUSMARCRECORD(Data);
					npr = new ZNAMEPLUSRECORD(DBName, s);
					Records->AddRecord(npr);
				} if(OID == SUTRS_OID) {
					//
					// SUTRS isn't encoded very
					// consistently across servers,
					// unfortunately.  I have to test
					// for every case here.
					//
					if(sub->HasTag(0)) {
						ASN1TAGU IntTag(27);
						sub->GetSubdirectory(0, sub);
						sub->GetChar(IntTag, &Data);
					} else if(sub->HasTag(1)) {
						sub->GetChar(1, &Data);
					}
					ZSUTRSRECORD *s;
					s = new ZSUTRSRECORD(Data);
					npr = new ZNAMEPLUSRECORD(DBName, s);
					Records->AddRecord(npr);
				}
			} else if(sub->HasTag(SURROGATEDIAGNOSTIC_TAG)) {
			} // else fragments go here
			else {
				cerr << "Protocol error in record type" << endl;
			}	
		}
		
	} else if(b->HasTag(NONSURROGATEDIAGNOSTIC_TAG)) {
		cerr << "Diagnostic returned" << endl;
		// Diagnostic
	} else if(b->HasTag(MULTIPLENONSURDIAGNOSTICS_TAG)) {
		// Multiple
		cerr << "Multiple Diagnostics returned" << endl;
	}
	
	delete b;
	return GDT_TRUE;	
}

GDT_BOOLEAN ZCLIENT_BASE::Add_Records(const STRING &Database,
			ZRECORDLIST *Records)
{
	INT4 len;
	PUCHR buf;
	ZESUPDATEREQUEST *s;
	unsigned short type;

	s = new ZESUPDATEREQUEST("", ES_UPDATE_RECORD_INSERT_ACTION,
		Database, Records);

	if(!SendPDU(s)) {
		delete s;
		return GDT_FALSE;
	}

	delete s;

	if((buf = ReadPDU(&len, &type)) == NULL)  {
		c_tcp->Close();
		return GDT_FALSE;
	}

	if(type != EXTENDEDSERVICESRESPONSE_TAG) {
		c_tcp->Close();
		delete [] buf;
		cerr << "Expected extended service response, got " << type << "\n";
		return GDT_FALSE;
	}

	ZESUPDATERESPONSE *Response;

	Response = new ZESUPDATERESPONSE(buf, len);
	delete [] buf;

	INT4 OperationStatus;
	INT4 TaskStatus;
	INT4 UpdateStatus;

	if (	!Response->GetOperationStatus(&OperationStatus) ||
		OperationStatus != ES_DONE_OPERATIONSTATUS ||
		!Response->GetTaskStatus(&TaskStatus) ||
		TaskStatus != ES_TP_COMPLETE_TASKSTATUS ||
		!Response->GetUpdateStatus(&UpdateStatus) ||
		UpdateStatus != ES_UPDATE_SUCCESS_UPDATESTATUS) {
		delete Response;
		return GDT_FALSE;
	}

	delete Response;

	return GDT_TRUE;	
}

GDT_BOOLEAN ZCLIENT_BASE::Delete_Records(const STRING &Database,
			ZRECORDLIST *Records)
{
	INT4 len;
	PUCHR buf;
	ZESUPDATEREQUEST *s;
	unsigned short type;

	s = new ZESUPDATEREQUEST("", ES_UPDATE_RECORD_DELETE_ACTION,
		Database, Records);

	if(!SendPDU(s)) {
		delete s;
		return GDT_FALSE;
	}

	delete s;

	if((buf = ReadPDU(&len, &type)) == NULL)  {
		c_tcp->Close();
		return GDT_FALSE;
	}

	if(type != EXTENDEDSERVICESRESPONSE_TAG) {
		c_tcp->Close();
		delete [] buf;
		cerr << "Expected extended service response, got " << type << "\n";
		return GDT_FALSE;
	}

	ZESUPDATERESPONSE *Response;

	Response = new ZESUPDATERESPONSE(buf, len);
	delete [] buf;

	INT4 OperationStatus;
	INT4 TaskStatus;
	INT4 UpdateStatus;

	if (	!Response->GetOperationStatus(&OperationStatus) ||
		OperationStatus != ES_DONE_OPERATIONSTATUS ||
		!Response->GetTaskStatus(&TaskStatus) ||
		TaskStatus != ES_TP_COMPLETE_TASKSTATUS ||
		!Response->GetUpdateStatus(&UpdateStatus) ||
		UpdateStatus != ES_UPDATE_SUCCESS_UPDATESTATUS) {
		delete Response;
		return GDT_FALSE;
	}

	delete Response;

	return GDT_TRUE;	
}

