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
File:		zrecords.cxx
Version:	1.00
Description:	Z39.50 records.
Author:		Kevin Gamiel, kevin.gamiel@cnidr.org
@@@*/

#include <string.h>
#include "zrecords.hxx"

ZRECORDBASE::ZRECORDBASE()
{
}

void ZRECORDBASE::AddBER(BERBROWSER &ber)
{
}

ZRECORDBASE::~ZRECORDBASE()
{
}

ZRECORDSCHOICE::ZRECORDSCHOICE()
{
}

void ZRECORDSCHOICE::AddBER(BERBROWSER &ber)
{
}

ZRECORDSCHOICE::~ZRECORDSCHOICE()
{
}

ZNAMEPLUSRECORD::ZNAMEPLUSRECORD(const STRING & DatabaseName,
	ZRECORDBASE *Record) : ZRECORDSCHOICE()
{
	c_db_name = DatabaseName;
	c_record = Record;
}

void ZNAMEPLUSRECORD::Print(ostream & os)
{
	os << "Database Name: " << c_db_name << endl;
	c_record->Print(os);
}

void ZNAMEPLUSRECORD::AddBER(BERBROWSER &ber)
{
	ber.AddChar(0, c_db_name);
	ber.AddSubdirectory(RECORD_TAG, &ber);
	c_record->AddBER(ber);
}

ZNAMEPLUSRECORD::~ZNAMEPLUSRECORD()
{
	if(c_record)
		delete c_record;
}

ZESUPDATERECORD::ZESUPDATERECORD(const STRING & RecordId,
	ZRECORDBASE *Record) : ZRECORDSCHOICE()
{
	c_record_id = RecordId;
	c_record = Record;
}

void ZESUPDATERECORD::Print(ostream & os)
{
	os << "Record Id: " << c_record_id << endl;
	c_record->Print(os);
}

void ZESUPDATERECORD::AddBER(BERBROWSER &ber)
{
	BERBROWSER sub(ber);

	ber.AddSubdirectory(ES_UPDATE_RECORDID_TAG, &sub);
	sub.AddChar(ES_UPDATE_STRING_RECORDID_TAG, c_record_id);

	ber.AddSubdirectory(ES_UPDATE_RECORD_TAG, &sub);
	c_record->AddBER(sub);
}

ZESUPDATERECORD::~ZESUPDATERECORD()
{
	if(c_record)
		delete c_record;
}

void ZRECORDLIST::Init(INT Tag)
{
	// Allocate the record array to hold a reasonable number of
	// records.  We can grow it later if we need to.
	c_recordlist = new ZRECORDSCHOICE*[16];
	c_maxrecords = 16;
	c_recordcount = 0;	
	c_recordtype = new ASN1TAG(Tag);
}

ZRECORDLIST::ZRECORDLIST()
{
	Init(RESPONSERECORDS_TAG);
}

ZRECORDLIST::ZRECORDLIST(INT Tag)
{
	Init(Tag);
}

ZRESPONSERECORDS::ZRESPONSERECORDS() : ZRECORDLIST(RESPONSERECORDS_TAG)
{
}

void ZRECORDLIST::Print(ostream & os)
{
	ZRECORDSCHOICE *Record;
	INT numrecs;
	numrecs = GetRecordCount();

	for(INT i=0;i < numrecs;i++) {
		Record = GetRecord(i);
		Record->Print(os);
	}
}

void ZRECORDLIST::Clear()
{
	INT4 i;
	for(i=0;i<c_recordcount;i++)
		delete c_recordlist[i];

	delete [] c_recordlist;
	delete c_recordtype;
	Init(RESPONSERECORDS_TAG);
}

ZRECORDLIST::~ZRECORDLIST()
{
	INT4 i;
	for(i=0;i<c_recordcount;i++)
		delete c_recordlist[i];

	delete [] c_recordlist;
	delete c_recordtype;
}

//
// Given a PRESENTRESPONSE PDU, for example, adds every BER node from
// the RECORDS node on down.
//
void ZRECORDLIST::AddBER(BERBROWSER &ber)
{
	ASN1TAG tt(NONSURROGATEDIAGNOSTIC_TAG);

	ber.AddSubdirectory(*c_recordtype, &ber);

	if(*c_recordtype == tt) {
		// Single record in response
		c_recordlist[0]->AddBER(ber);
	} else {
		BERBROWSER sub = ber;
		ASN1TAGU seqtag(ASN1_SEQUENCE);

		for(INT i=0;i < c_recordcount;i++) {
			// Add a sequence tag
			ber.AddSubdirectory(seqtag, &sub);
			c_recordlist[i]->AddBER(sub);
		}
	}
}

void ZRECORDLIST::AddRecord(ZRECORDSCHOICE *Record)
{
	if(c_recordcount == c_maxrecords) {
		// Grow the records array
		INT newmax;
		ZRECORDSCHOICE **templist;
		newmax = c_maxrecords + 64;
		templist = new ZRECORDSCHOICE*[newmax];
		memcpy(templist, c_recordlist, 
			(unsigned int)(c_maxrecords * sizeof(ZRECORDSCHOICE *)));
		c_maxrecords = newmax;
		delete [] c_recordlist;
		c_recordlist = templist;
	}
	c_recordlist[c_recordcount] = Record;	
	c_recordcount++;
}

ZRECORDSCHOICE *ZRECORDLIST::GetRecord(const INT RecNum)
{
	if(RecNum < c_recordcount)
		return c_recordlist[RecNum];
	return NULL;
}


ZEXTBASE::ZEXTBASE(const STRING & oid, const GDT_BOOLEAN IsRetrievalRecord):
	ZRECORDBASE()
{
	/*
	This is strange and I believe the result of an error in the Z39.50
	ASN.1.  In the NamePlusRecord SEQUENCE there is a tag record=1.
	Under that tag is a choice among several different record types.
	The first is retrievalRecord=1 which is of type EXTERNAL.  I believe
	the retrievalRecord tag is unecessary and is causing my C++ record
	class structure problems.  I need a standalone EXTERNAL record in other
	places in the standard so I need to derive it directly from 
	ZRECORDBASE, which imposes no tags of its own.  However, I also
	need an EXTERNAL hanging from a retrievalRecord (with tag of 1)
	for the case I've just described.  I may simply be missing something
	b/c I'm tired, but the only way I can fix this now is to add a flag
	to the ZEXTBASE class that says whether the particular instance of
	the EXTERNAL is really a retrievalRecord or not.  That way, the
	descendants can reset that flag appropriately and the AddBER()
	method for ZEXTBASE can either add the retrievalRecord tag or not.
	I realize this is an abuse of C++, but its not the first and surely
	won't be the last;-)
	*/
	c_is_retrieval_record = IsRetrievalRecord;
	c_oid = oid;
}

void ZEXTBASE::GetOID(STRING * oid)
{
	*oid = c_oid;
}

void ZEXTBASE::AddBER(BERBROWSER &ber)
{
	if(c_is_retrieval_record)
		ber.AddSubdirectory(1, &ber);	// See note in constructor

	// EXTERNAL tag
	ASN1TAGU tag(ASN1_EXTERNAL);
	ber.AddSubdirectory(tag, &ber);
	tag.Number = ASN1_OBJECTIDENTIFIER;
	ber.AddOID(tag, c_oid);
}

ZEXTBASE::~ZEXTBASE()
{
}

ZEXTSINGLE::ZEXTSINGLE(const STRING & oid, const STRING & data,
	const GDT_BOOLEAN IsRetrievalRecord): ZEXTBASE(oid, IsRetrievalRecord)
{
	c_data = data;
}

void ZEXTSINGLE::AddBER(BERBROWSER &ber)
{
	ZEXTBASE::AddBER(ber);	// EXTERNALs are encoded the same at the base

	// Add single stuff
	ber.AddSubdirectory(0, &ber);

//	ASN1TAGU tag(0);
//	tag.Number = ASN1_GENERALSTRING;
	ASN1TAGU tag(ASN1_GENERALSTRING);
	ber.AddChar(tag, c_data);
}

ZSUTRSRECORD::ZSUTRSRECORD(const STRING & Data) : 
	ZEXTSINGLE(SUTRS_OID, Data, GDT_TRUE)
{
}

void ZSUTRSRECORD::Print(ostream & os)
{
	os << "ZSUTRSRECORD::Print:OID=" << c_oid << endl;
	os << "ZSUTRSRECORD::Print:Data=" << c_data;
}


ZEXTOCTETALIGNED::ZEXTOCTETALIGNED(const STRING & oid, const STRING & data,
	const GDT_BOOLEAN IsRetrievalRecord): ZEXTBASE(oid, IsRetrievalRecord)
{
	c_data = data;
}

void ZEXTOCTETALIGNED::AddBER(BERBROWSER &ber)
{
	ber.AddSubdirectory(1, &ber);

	ASN1TAGU tag(ASN1_EXTERNAL);

	ber.AddSubdirectory(tag, &ber);
	tag.Number = ASN1_OBJECTIDENTIFIER;
	ber.AddOID(tag, c_oid);

	// Add octet-aligned stuff
	ber.AddChar(1, c_data);
}

ZUSMARCRECORD::ZUSMARCRECORD(const STRING & Data) : 
	ZEXTOCTETALIGNED(USMARC_OID, Data, GDT_TRUE)
{
}

ZESTASKPACKAGE::ZESTASKPACKAGE(const STRING & package_type,
	const STRING & package_name,
	const STRING & user_id,
	const INT4 & retention_time,
	const STRING & description,
	const INT4 & task_status)
	:ZEXTBASE(EXTENDEDSERVICES_OID, GDT_FALSE)
{
	c_package_type = package_type;
	c_package_name = package_name;
	c_user_id = user_id;
	c_retention_time = retention_time;
	c_description = description;
	c_task_status = task_status;
}

void ZESTASKPACKAGE::Print(ostream & os)
{
	os << "Package Type   : " << c_package_type << endl;
	os << "Package Name   : " << c_package_name << endl;
	os << "User Id        : " << c_user_id << endl;
	os << "Retention Time : " << c_retention_time << endl;
	os << "Description    : " << c_description << endl;
	os << "Task Status    : " << c_task_status << endl;
}

void ZESTASKPACKAGE::AddBER(BERBROWSER &ber)
{
	ZEXTBASE::AddBER(ber);	// EXTERNALs are encoded the same at the base

	ber.AddOID(ES_TP_PACKAGETYPE_TAG, c_package_type);
	ber.AddChar(ES_TP_PACKAGENAME_TAG, c_package_name);
	ber.AddChar(ES_TP_USERID_TAG, c_user_id);
	ber.AddInt(ES_TP_RETENTIONTIME_TAG, c_retention_time);
	ber.AddChar(ES_TP_DESCRIPTION_TAG, c_description);
	ber.AddInt(ES_TP_TASKSTATUS_TAG, c_task_status);
	ber.AddSubdirectory(ES_TP_TASKSPECIFIC_TAG, &ber);

	ASN1TAGU exttag(ASN1_EXTERNAL);

	ber.AddSubdirectory(exttag, &ber);

	ASN1TAGU oidtag(ASN1_OBJECTIDENTIFIER);

	ber.AddOID(oidtag, c_package_type);

	ber.AddSubdirectory(ES_TS_TASKPACKAGE_TAG, &ber);
}

ZDEFAULTDIAGFORMAT::ZDEFAULTDIAGFORMAT(const INT4 Condition,
                const STRING & AddInfo, 
                const STRING & DiagSetId, 
                const INT Version): ZRECORDSCHOICE()
{
	c_oid = DiagSetId;
	c_condition = Condition;
	c_addinfo = AddInfo;
	c_version = Version;
}

void ZDEFAULTDIAGFORMAT::AddBER(BERBROWSER & ber)
{
	// Add the OID
	ASN1TAGU tag(ASN1_OBJECTIDENTIFIER);
	ber.AddOID(tag, c_oid);

	// Add the condition
	tag.Number = ASN1_INTEGER;
	ber.AddInt(tag, c_condition);
	
	// Add the AddInfo (I thought this was optional?)
	if(c_version < 3)
		tag.Number = ASN1_VISIBLESTRING;
	else
		tag.Number = INTERNATIONALSTRING_TAG;
	ber.AddChar(tag, c_addinfo);
}

//
// Convenience method.  Builds a record list consisting of a single
//      nonsurrogate diagnostic record of default diag format.
//
ZRECORDLIST *NonsurrogateDiagnosticRecord(const INT4 Condition,
	const STRING & AddInfo, const STRING & DiagSetId,
	const INT Version)
{
	ZRECORDLIST *records;
	records = new ZRECORDLIST(NONSURROGATEDIAGNOSTIC_TAG);
	ZDEFAULTDIAGFORMAT *rec;
	rec = new ZDEFAULTDIAGFORMAT(Condition, AddInfo, DiagSetId, Version);
	records->AddRecord(rec);
 
	return records;
}

