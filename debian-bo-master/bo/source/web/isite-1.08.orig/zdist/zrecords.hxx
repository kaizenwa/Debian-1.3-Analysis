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
File:		zrecords.hxx
Version:	1.00
Description:	Z39.50 record classes
Author:		Kevin Gamiel, kevin.gamiel@cnidr.org
@@@*/

#ifndef _ZRECORDS_HXX_
#define _ZRECORDS_HXX_

#include "ber.hxx"
#include "ztags.hxx"

//
// Base class for low-level Z39.50 records:
//
class ZRECORDBASE {
public:
	ZRECORDBASE();
	virtual ~ZRECORDBASE();
	virtual void AddBER(BERBROWSER &ber);	

	virtual void Print(ostream & os) { cout << "*****" << endl;}
	// Virtual functions for derived classes
	virtual void GetRecordData(STRING *Data) {}
	virtual void GetOID(STRING *oid) {}
};

//
// Base class for EXTERNAL records
//
class ZEXTBASE : public ZRECORDBASE {
protected:
	GDT_BOOLEAN	c_is_retrieval_record;
	STRING 		c_oid;
public:
       ZEXTBASE() {}
	ZEXTBASE(const STRING & oid, const GDT_BOOLEAN IsRetrievalRecord=
		GDT_FALSE);	// See note in constructor code
	void GetOID(STRING *oid);
	virtual void AddBER(BERBROWSER &ber);	
	virtual ~ZEXTBASE();
	virtual void Print(ostream & os) { cout << "EXTBASE" << endl;}
};

//
// External Record, single-ASN1-type
//
class ZEXTSINGLE : public ZEXTBASE {
protected:
	STRING c_data;
public:
	ZEXTSINGLE(const STRING & oid, const STRING & data,
		const GDT_BOOLEAN IsRetrievalRecord=GDT_FALSE);
	virtual void Print(ostream & os) { cout << "ZEXTSINGLE:" << c_data; }
	virtual void AddBER(BERBROWSER &ber);	
};

//
// External Record, octet-aligned
//
class ZEXTOCTETALIGNED : public ZEXTBASE {
protected:
	STRING c_data;
public:
	ZEXTOCTETALIGNED(const STRING & oid, const STRING & data,
		const GDT_BOOLEAN IsRetrievalRecord=GDT_FALSE);
	virtual void Print(ostream & os) { cout << "ZEXTOCTETALIGNED:" << c_data; }
	virtual void AddBER(BERBROWSER &ber);	
};

//
// External Record, arbitrary choice
//
class ZEXTARBITRARY : public ZEXTBASE {
public:
	ZEXTARBITRARY();
};

//
// SUTRS - Simple Unstructured Text Record Syntax
//
//	Encoded as an EXTERNAL, single-ASN1-encoding
//
class ZSUTRSRECORD : public ZEXTSINGLE {
public:
	ZSUTRSRECORD(const STRING & Data);
	ZSUTRSRECORD(const BERBROWSER &ber);
	virtual void GetRecordData(STRING *Data) { *Data = c_data; }
	virtual void Print(ostream & os);
};

//
// USMARC
//
//	Encoded as an EXTERNAL, octet-aligned
//
class ZUSMARCRECORD : public ZEXTOCTETALIGNED {
public:
	ZUSMARCRECORD(const STRING & Data);
	virtual void GetRecordData(STRING *Data) { *Data = c_data; }
};

//
// Extended services task package
//
class ZESTASKPACKAGE : public ZEXTBASE {
protected:
	STRING	c_package_type;
	STRING	c_package_name;
	STRING	c_user_id;
	INT4	c_retention_time;
	STRING	c_description;
	INT4	c_task_status;
public:
	ZESTASKPACKAGE(const STRING & package_type,
		const STRING & package_name,
		const STRING & user_id,
		const INT4 & retention_time,
		const STRING & description,
		const INT4 & task_status);
	virtual void Print(ostream & os);
	virtual void AddBER(BERBROWSER &ber);	
};

// 
// Virtual base class for Records CHOICE
//
class ZRECORDSCHOICE {
public:
	ZRECORDSCHOICE();
	virtual ~ZRECORDSCHOICE();
	
	virtual void AddBER(BERBROWSER &ber);	
	virtual void Print(ostream & os) {cout << "***" << endl;}
	virtual void GetRecordData(STRING *Data) { }
	virtual void GetOID(STRING *oid) {}
};

//
// NAMEPLUSRECORD 
//
class ZNAMEPLUSRECORD : public ZRECORDSCHOICE {
private:
	STRING	c_db_name;
	ZRECORDBASE *c_record;
public:
	ZNAMEPLUSRECORD(const STRING & DatabaseName, 
		ZRECORDBASE *Record);
	~ZNAMEPLUSRECORD();

	virtual void AddBER(BERBROWSER &ber);	
	virtual void Print(ostream & os);
	virtual void GetRecordData(STRING *Data) { c_record->GetRecordData(Data); }
	virtual void GetOID(STRING *oid) { c_record->GetOID(oid); }
};

//
// Extended service update record 
//
class ZESUPDATERECORD : public ZRECORDSCHOICE {
private:
	STRING	c_record_id;
	ZRECORDBASE *c_record;
public:
	ZESUPDATERECORD(const STRING & RecordId, 
		ZRECORDBASE *Record);
	~ZESUPDATERECORD();

	virtual void AddBER(BERBROWSER &ber);	
	virtual void Print(ostream & os);
	virtual void GetRecordData(STRING *Data) { c_record->GetRecordData(Data); }
	virtual void GetOID(STRING *oid) { c_record->GetOID(oid); }
};

//
// Default diagnostic record.  
//
// The AddInfo is either a Visible String or an International String 
// depending on the Z39.50 version.  Instead
// of making separate classes, I've added a flag in the constructor
// that defaults to version 2.  Simply specify version 3 or above
// to get an International String.
//
class ZDEFAULTDIAGFORMAT : public ZRECORDSCHOICE {
	STRING	c_oid,
		c_addinfo;
	INT4	c_condition,
		c_version;
public:
	ZDEFAULTDIAGFORMAT(const INT4 Condition, 
		const STRING & AddInfo, 
		const STRING & DiagSetId,
		const INT Version=2);
	void AddBER(BERBROWSER &ber);
};

class ZRECORDLIST {
	ZRECORDSCHOICE **c_recordlist;
	INT 	c_maxrecords,
		c_recordcount;
	ASN1TAG	*c_recordtype;
	void 	Init(INT Tag);
public:
	ZRECORDLIST();
	ZRECORDLIST(INT Tag);
	virtual ~ZRECORDLIST();

	virtual void AddRecord(ZRECORDSCHOICE *Record);
	virtual ZRECORDSCHOICE *GetRecord(const INT RecNum);
	virtual const INT GetRecordCount() { return c_recordcount; }
	virtual void AddBER(BERBROWSER &ber);
	virtual void Print(ostream & os);
	void Clear();	
};

class ZRESPONSERECORDS : public ZRECORDLIST {
public:
	ZRESPONSERECORDS();
};

//
// Convenience function.  Builds a record list consisting of a single
//      nonsurrogate diagnostic record of default diag format.
//
ZRECORDLIST *NonsurrogateDiagnosticRecord(const INT4 Condition,
        const STRING & AddInfo, const STRING & DiagSetId,
        const INT Version=2);


/*
class ZSURROGATEDIAG : public ZRECORD {
};

class ZFRAGMENT : public ZRECORD {
};

class ZSTARTINGFRAGMENT : public ZFRAGMENT {
};

class ZINTERMEDIATEFRAGMENT : public ZFRAGMENT {
};

class ZFINALFRAGMENT : public ZFRAGMENT {
};
*/


/*
class ZNONSURROGATEDIAGNOSTIC : public ZRECORD {

};

class ZMULTIPLENONSURDIAGNOSTICS : public ZRECORDLIST {
};
*/

//
// GRS1
//
//    Encoded as an EXTERNAL, single-ASN1-encoding
//
/*
	class Z_GRS1_RECORD : public ZEXTSINGLE {
	public:
	  Z_GRS1_RECORD();
	  Z_GRS1_RECORD(const BERBROWSER &ber);
	  
	  virtual void GetRecordData(STRING *Data) { *Data = c_data; }
	  virtual void Print(ostream & os);
	};
*/

#endif
