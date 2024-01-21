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
File:		zpdu.hxx
Version:	1.00
Description:	Z39.50 PDU classes
Author:		Kevin Gamiel, kevin.gamiel@cnidr.org
@@@*/

#ifndef _ZPDU_HXX_
#define _ZPDU_HXX_

#include "ber.hxx"
#include "ztags.hxx"

//***************************************************************************
//
//	class ZPDU - Base class for Z39.50 PDUs
//	
//***************************************************************************
class ZPDU : public BERTREE {
protected:
	PBERBROWSER 	c_root,		// always point to the root of tree
			c_browser;	// general purpose browser
	void Init();
public:
	ZPDU(PUCHR Buf, INT4 Length);
	ZPDU(ASN1TAG Tag);
	~ZPDU();

	DD_STRING(ReferenceId,REFERENCEID_TAG)
};
typedef ZPDU far *PZPDU;

#include "zrecords.hxx"

//***************************************************************************
//
//	class ZINITBASE - Base class for Z39.50 Initialize PDU
//	
//***************************************************************************
class ZINITBASE : public ZPDU {
public:
	ZINITBASE(ASN1TAG Tag);
	ZINITBASE(PUCHR Buf, INT4 Length);
	DD_BITSTRING(ProtocolVersion,PROTOCOLVERSION_TAG)
	DD_BITSTRING(Options,OPTIONS_TAG)
	DD_INT(PreferredMessageSize, PREFERREDMESSAGESIZE_TAG)
	DD_INT(ExceptionalRecordSize, EXCEPTIONALRECORDSIZE_TAG)
	DD_STRING(ImplementationId, IMPLEMENTATIONID_TAG)
	DD_STRING(ImplementationName, IMPLEMENTATIONNAME_TAG)
	DD_STRING(ImplementationVersion, IMPLEMENTATIONVERSION_TAG)
};

//***************************************************************************
//
//	class ZINITQUEST - Z39.50 Initialize Request PDU
//
//***************************************************************************
class ZINITREQUEST : public ZINITBASE {
private:
public:
	ZINITREQUEST(PUCHR Buf, INT4 Length);
	ZINITREQUEST(	const STRING RefId, 
			const STRING ProtVer,
			const STRING Options,
			const INT4 PrefMsgSize,
			const INT4 ExceptionalRecSize,
			const STRING GroupId,
			const STRING UserId,
			const STRING Password,
			const STRING ImpId,
			const STRING ImpName,
			const STRING ImpVersion);
	
	DD_SUBDIR(Authentication, AUTHENTICATION_TAG)
	GDT_BOOLEAN GetGroupId(STRING *GroupId);
	GDT_BOOLEAN GetUserId(STRING *UserId);
	GDT_BOOLEAN GetPassword(STRING *Password);
};

//***************************************************************************
//
//	class ZINITSPONSE - Z39.50 Initialize Response PDU
//
//***************************************************************************
class ZINITRESPONSE : public ZINITBASE {
public:
	ZINITRESPONSE(PUCHR Buf, INT4 Length);
	ZINITRESPONSE(	const STRING& RefId, 
			const STRING& ProtVer,
			const STRING& Options,
			const INT4 PrefMsgSize,
			const INT4 ExceptionalRecSize,
			const INT4 Result,
			const STRING& ImpId,
			const STRING& ImpName,
			const STRING& ImpVersion);

	//DD_BOOLEAN(Result, ASN1TAGU(RESULT_TAG))
	DD_BOOLEAN(Result, RESULT_TAG)
};


//***************************************************************************
//
//	class ZSEARCHBASE - Base class for Z39.50 Search PDUs
//	
//***************************************************************************
class ZSEARCHBASE : public ZPDU {
public:
	ZSEARCHBASE(ASN1TAG Tag) : ZPDU(Tag) {}
	ZSEARCHBASE(PUCHR Buf, INT4 Length) : ZPDU(Buf,Length) {}
};

//***************************************************************************
//
//	class ZSEARCHREQUEST - Z39.50 Search Request PDU
//	
//***************************************************************************
class ZSEARCHREQUEST : public ZSEARCHBASE {
public:
	ZSEARCHREQUEST(PUCHR Buf, INT4 Length) : ZSEARCHBASE(Buf,Length) {}
	ZSEARCHREQUEST(	const STRING& RefId,
			const INT4 SmallSetUpperBound,
			const INT4 LargeSetLowerBound,
			const INT4 MedSetPresentNum,
			const INT4 ReplaceIndicator,
			const STRING& ResultSetName,
			const STRING& DatabaseNames,
			const PCHR SmallElementSetNames,
			const PCHR MediumElementSetNames,
			const STRING& PrefRecSyntax,
			const STRING& Query,
			const STRING& AttrSetId,
			const INT QueryType=101);
			
	DD_INT(SmallSetUpperBound, SMALLSETUPPERBOUND_TAG)
	DD_INT(LargeSetLowerBound, LARGESETLOWERBOUND_TAG)
	DD_INT(MediumSetPresentNumber, MEDIUMSETPRESENTNUMBER_TAG)
	DD_BOOLEAN(ReplaceIndicator, REPLACEINDICATOR_TAG)
	DD_STRING(ResultSetName, RESULTSETNAME_TAG)
	DD_SUBDIR(DatabaseNames, DATABASENAMES_TAG)
	void GetDatabaseName(STRING *DBName);// Fetch only 1 database name
	DD_SUBDIR(SmallSetElementSetNames, SMALLSETELEMENTSETNAMES_TAG)
	DD_SUBDIR(MediumSetElementSetNames, MEDIUMSETELEMENTSETNAMES_TAG)
	DD_OID(PreferredRecordSyntax, PREFERREDRECORDSYNTAX_TAG)
	DD_SUBDIR(Query, QUERY_TAG)
};

//***************************************************************************
//
//	class ZSEARCHRESPONSE - Z39.50 Search Response PDU
//	
//***************************************************************************
class ZSEARCHRESPONSE : public ZSEARCHBASE {
public:
	ZSEARCHRESPONSE(PUCHR Buf, INT4 Length) : ZSEARCHBASE(Buf,Length) {}
	ZSEARCHRESPONSE(const STRING& RefId,
			const INT4 ResultCount,
			const INT4 NumberOfRecordsReturned,
			const INT4 NextResultSetPosition,
			const INT4 SearchStatus,
			const INT4 ResultSetStatus,
			const INT4 PresentStatus,
			ZRECORDLIST *Records);
	DD_INT(ResultCount, RESULTCOUNT_TAG)
	DD_INT(NumberOfRecordsReturned, NUMBEROFRECORDSRETURNED_TAG)
	DD_INT(NextResultSetPosition, NEXTRESULTSETPOSITION_TAG)
	DD_BOOLEAN(SearchStatus, SEARCHSTATUS_TAG)
	DD_INT(ResultSetStatus, RESULTSETSTATUS_TAG)
	DD_INT(PresentStatus, PRESENTSTATUS_TAG)
};

//***************************************************************************
//
//	class ZPRESENTBASE - Base class for Z39.50 Present PDUs
//	
//***************************************************************************
class ZPRESENTBASE : public ZPDU {
public:
	ZPRESENTBASE(ASN1TAG Tag) : ZPDU(Tag) {}
	ZPRESENTBASE(PUCHR Buf, INT4 Length) : ZPDU(Buf,Length) {}
};

//***************************************************************************
//
//	class ZPRESENTREQUEST - Z39.50 Present Request PDU
//	
//***************************************************************************
class ZPRESENTREQUEST : public ZPRESENTBASE {
public:
	ZPRESENTREQUEST(PUCHR Buf, INT4 Length) : ZPRESENTBASE(Buf,Length) {}
	ZPRESENTREQUEST(const STRING& RefId,
			const STRING& ResultSetId,
			const INT4 ResultSetStartPoint,
			const INT4 NumberOfRecordsRequested,
			const STRING& ElementSetName,
			const STRING& PreferredRecordSyntax);
			
	DD_STRING(ResultSetId, RESULTSETID_TAG)
	DD_INT(ResultSetStartPoint, RESULTSETSTARTPOINT_TAG)
	DD_INT(NumberOfRecordsRequested, NUMBEROFRECORDSREQUESTED_TAG)
	DD_OID(PreferredRecordSyntax, PREFERREDRECORDSYNTAX_TAG)
	void GetElementSetName(STRING *ElementSetName);
	DD_SUBDIR(AdditionalRanges, 212)
	DD_SUBDIR(CompSpec, 209)
};

//***************************************************************************
//
//	class ZPRESENTRESPONSE - Z39.50 Present Response PDU
//	
//***************************************************************************
class ZPRESENTRESPONSE : public ZPRESENTBASE {
public:
	ZPRESENTRESPONSE(PUCHR Buf, INT4 Length) : ZPRESENTBASE(Buf,Length) {}
	ZPRESENTRESPONSE(const STRING& RefId,
			const INT4 NumberOfRecordsReturned,
			const INT4 NextResultSetPosition,
			const INT4 PresentStatus,
			ZRECORDLIST *Records);
			
	DD_INT(NextResultSetPosition, NEXTRESULTSETPOSITION_TAG)
	DD_INT(NumberOfRecordsReturned, NUMBEROFRECORDSRETURNED_TAG)
	DD_INT(PresentStatus, PRESENTSTATUS_TAG)
};

//***************************************************************************
//
//	class ZSCANBASE - Base class for Z39.50 Scan PDUs
//	
//***************************************************************************
class ZSCANBASE : public ZPDU {
public:
	ZSCANBASE(ASN1TAG Tag) : ZPDU(Tag) {}
	ZSCANBASE(PUCHR Buf, INT4 Length) : ZPDU(Buf,Length) {}
	
	DD_INT(StepSize, STEPSIZE_TAG)
};

//***************************************************************************
//
//	class ZSCANREQUEST - Z39.50 Scan Request PDU
//	
//***************************************************************************
class ZSCANREQUEST : public ZSCANBASE {
public:
	ZSCANREQUEST(PUCHR Buf, INT4 Length) : ZSCANBASE(Buf,Length) {}
	ZSCANREQUEST(	const PCHR RefId,
			const PCHR DatabaseNames,
			const PCHR AttributeSet,
			const PCHR TermList,
			INT4 StepSize,
			INT4 NumTermsRequested,
			INT4 PrefPosInResponse);

	DD_SUBDIR(DatabaseNames, DATABASENAMES_TAG)
	DD_OID(AttributeSet, ATTRIBUTESETID_TAG)
	DD_SUBDIR(TermListAndStartPoint, ATTRIBUTESPLUSTERM_TAG)
	DD_INT(NumberOfTermsRequested, NUMBEROFTERMSREQUESTED_TAG)
	DD_INT(PreferredPositionInResponse, PREFERREDPOSITIONINRESPONSE_TAG)
};

//***************************************************************************
//
//	class ZSCANRESPONSE - Z39.50 Scan Response PDU
//	
//***************************************************************************
class ZSCANRESPONSE : public ZSCANBASE {
public:
	ZSCANRESPONSE(PUCHR Buf, INT4 Length) : ZSCANBASE(Buf,Length) {}
	//ZSCANRESPONSE();
	
	DD_INT(NumberOfEntriesReturned, NUMBEROFENTRIESRETURNED_TAG)
};

//***************************************************************************
//
//	class ZESBASE - Base class for Z39.50 Extended Services PDUs
//	
//***************************************************************************
class ZESBASE : public ZPDU {
public:
	ZESBASE(ASN1TAG Tag) : ZPDU(Tag) {}
	ZESBASE(PUCHR Buf, INT4 Length) : ZPDU(Buf,Length) {}
};

//***************************************************************************
//
//	class ZESREQUEST - Z39.50 Extended Service Request PDU
//	
//***************************************************************************
class ZESREQUEST : public ZESBASE {
public:
	ZESREQUEST(PUCHR Buf, INT4 Length) : ZESBASE(Buf,Length) {}
	ZESREQUEST(	const STRING& RefId,
			const INT4 Function,
			const STRING& PackageType,
			const STRING& PackageName,
			const STRING& UserId,
			const INT4 RetentionTime,
			const STRING& Description,
			const INT4 WaitAction);
			
	DD_INT(Function, ES_FUNCTION_TAG)
	DD_OID(PackageType, ES_PACKAGETYPE_TAG)
	DD_STRING(PackageName, ES_PACKAGENAME_TAG)
	DD_STRING(UserId, ES_USERID_TAG)
	DD_INT(RetentionTime, ES_RETENTIONTIME_TAG)
	DD_SUBDIR(Permissions, ES_PERMISSIONS_TAG)
	DD_STRING(Description, ES_DESCRIPTION_TAG)
	DD_INT(WaitAction, ES_WAITACTION_TAG)
};

//***************************************************************************
//
//	class ZESRESPONSE - Z39.50 Extended Service Response PDU
//	
//***************************************************************************
class ZESRESPONSE : public ZESBASE {
public:
	ZESRESPONSE(PUCHR Buf, INT4 Length) : ZESBASE(Buf,Length) {}
	ZESRESPONSE(	const STRING& RefId,
			const INT4 OperationStatus,
			ZDEFAULTDIAGFORMAT *DiagRecord,
			ZESTASKPACKAGE *TaskPackage);

	DD_INT(OperationStatus, ES_OPERATIONSTATUS_TAG)
	DD_SUBDIR(Diagnostics, ES_DIAGNOSTICS_TAG)
	DD_SUBDIR(TaskPackage, ES_TASKPACKAGE_TAG)
	GDT_BOOLEAN GetTaskStatus(INT4 *TaskStatus);
};

//***************************************************************************
//
//	class ZESUPDATEREQUEST - Z39.50 ES Update Request PDU
//	
//***************************************************************************
class ZESUPDATEREQUEST : public ZESREQUEST {
public:
	ZESUPDATEREQUEST(PUCHR Buf, INT4 Length) : ZESREQUEST(Buf,Length) {}
	ZESUPDATEREQUEST(const STRING& RefId,
			const INT4 Action,
			const STRING& DatabaseName,
			ZRECORDLIST *Records);
	GDT_BOOLEAN GetAction(INT4 *Action);
	GDT_BOOLEAN GetDatabaseName(STRING *DatabaseName);
};

//***************************************************************************
//
//	class ZESUPDATERESPONSE - Z39.50 ES Update Response PDU
//	
//***************************************************************************
class ZESUPDATERESPONSE : public ZESRESPONSE {
public:
	ZESUPDATERESPONSE(PUCHR Buf, INT4 Length) : ZESRESPONSE(Buf,Length) {}
	ZESUPDATERESPONSE(const STRING& RefId,
			ZESTASKPACKAGE *TaskPackage,
			const INT4 Action,
			const STRING& DatabaseName,
			const INT4 UpdateStatus,
			ZDEFAULTDIAGFORMAT *Record);
	GDT_BOOLEAN GetUpdateStatus(INT4 *UpdateStatus);
};

//***************************************************************************
//
//	class ZCLOSE - Z39.50 Close PDU
//	
//***************************************************************************
class ZCLOSE : public ZPDU {
public:
	ZCLOSE(PUCHR Buf, INT4 Length) : ZPDU(Buf, Length) {}
	ZCLOSE(const PCHR RefId, INT4 Reason);

	DD_INT(CloseReason, CLOSEREASON_TAG)
};

#endif
