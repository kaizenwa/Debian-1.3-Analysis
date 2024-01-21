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
File:		zpdu.cxx
Version:	1.00
Description:	Z39.50 PDU classes
Author:		Kevin Gamiel, kevin.gamiel@cnidr.org
@@@*/

#include <string.h>
#include "zpdu.hxx"
#include "strlist.hxx"

#ifndef IFSTR
#define IFSTR(a) if((a) && (*a))
#endif
#ifndef IFOSTR
#define IFOSTR(a) if(a.GetLength() > 0)
#endif

GDT_BOOLEAN AddTerm(BERBROWSER & b, const STRING& Term);
GDT_BOOLEAN IsBoolean(const STRING& Term);
void GetTerm(const STRING &Term, STRING *Term1, STRING *Term2);
void GetOperator(const STRING &Term, STRING *TheOperator);
void ParseList(const STRING &Term, STRLIST *AttrList, STRING *TermOnly);

//***************************************************************************
//
//	Init - Initializer common to all ZPDU class constructors.
//
//***************************************************************************
void ZPDU::Init()
{
	c_root = new BERBROWSER(this);
	c_browser = new BERBROWSER(this);
}

//***************************************************************************
//
//	ZPDU - Constructor for (decoding a) ZPDU object.
//
//		Buf - Buffer of data satisfying call to IsCompleteBER()
//		Length - length of Buf
//
//***************************************************************************
ZPDU::ZPDU(PUCHR Buf,INT4 Length) : BERTREE(Buf,Length)
{
	Init();
}

//***************************************************************************
//
//	ZPDU - Constructor for ZPDU object.
//
//		Tag - valid ASN.1 Z39.50 PDU tag value
//
//***************************************************************************
ZPDU::ZPDU(ASN1TAG Tag) : BERTREE(Tag)
{
	Init();
}

//***************************************************************************
//
//	~ZPDU - Destructor for ZPDU
//
//***************************************************************************
ZPDU::~ZPDU()
{
	if(c_root)
		delete c_root;
	if(c_browser)
		delete c_browser;
}

//***************************************************************************
//
//	ZINITBASE - Constructor for object shared by both Init Requests and
//		Init Responses.
//
//		Tag - either 20 or 21, Init Request or Init Response
//
//***************************************************************************
ZINITBASE::ZINITBASE(ASN1TAG Tag) : ZPDU(Tag)
{
}

//***************************************************************************
//
//	ZINITBASE - Constructor for (decoding) object shared by both Init 
//		Requests and Init Responses.
//
//		Buf - Buffer of data satisfying call to IsCompleteBER()
//		Length - length of Buf
//
//***************************************************************************
ZINITBASE::ZINITBASE(PUCHR Buf, INT4 Length) : ZPDU(Buf,Length)
{
}

//***************************************************************************
//
//	ZINITREQUEST - Constructor for (decoding) Init Request PDU
//
//		Buf - Buffer of data satisfying call to IsCompleteBER()
//		Length - length of Buf
//
//***************************************************************************
ZINITREQUEST::ZINITREQUEST(PUCHR Buf, INT4 Length) : ZINITBASE(Buf, Length)
{
}

//***************************************************************************
//
//	ZINITREQUEST - Constructor for Init Request PDU
//
//		RefId - Reference Identifier (NULL if none)
//		Options - Z39.50 options supported
//		PrefMsgSize - PreferredMessageSize
//		ExceptionalRecSize - Maximum record size supported
//		GroupId - Authentication group id (NULL if none)
//		UserId - Authentication user id (NULL if none)
//		Password - Authentication password (NULL if none)
//		ImpId - Implementation Identifier
//		ImpName - Implementation Name
//		ImpVersion - Implementation Version
//
//	NOTES:
//		Authentication
//			If GroupId, UserId and Password, idPass encoded.  
//			If UserId and Password, idPass encoded.
//			If Password only, open encoded.  
//				If Password only, should be of the form 
//				"username/password".  This encoding is not 
//				preferred.
//			If None, anonymous encoded
//
//		You can call this method passing PCHR instead of STRING
//			arguments.  In that case, the compiler will call 
//			STRING(PCHR x) for each PCHR arg and will call 
//			~STRING()at the end of the method.
//
//***************************************************************************
ZINITREQUEST::ZINITREQUEST(	const STRING RefId,
				const STRING ProtVer,
				const STRING Options,
				const INT4 PrefMsgSize,
				const INT4 ExceptionalRecSize,
				const STRING GroupId,
				const STRING UserId,
				const STRING Password,
				const STRING ImpId,
				const STRING ImpName,
				const STRING ImpVersion)
				:ZINITBASE(INITREQUEST_TAG)
{
	BERBROWSER *Browser;
	
	Browser = new BERBROWSER(this);

	AddReferenceId(RefId);
	AddProtocolVersion(ProtVer);
	AddOptions(Options);
	AddPreferredMessageSize(PrefMsgSize);
	AddExceptionalRecordSize(ExceptionalRecSize);

	//
	// Do we need to encode the Authentication?
	//
	IFOSTR(GroupId) {
		// Encode using idPass
		AddAuthentication();
		Browser->GetSubdirectory(AUTHENTICATION_TAG, Browser);
		//  Patch from Dort Rottner for IDPass tag
                Browser->AddSubdirectory(ASN1TAGU(ASN1_SEQUENCE));
                Browser->GetSubdirectory(ASN1TAGU(ASN1_SEQUENCE), Browser);
		Browser->AddChar(GROUPID_TAG, GroupId);
		Browser->AddChar(USERID_TAG, UserId);
		Browser->AddChar(PASSWORD_TAG, Password);
	} else IFOSTR(UserId) {
		// Encode using idPass
		AddAuthentication();
		Browser->GetSubdirectory(AUTHENTICATION_TAG, Browser);
		Browser->AddChar(USERID_TAG, UserId);
		Browser->AddChar(PASSWORD_TAG, Password);
	} else IFOSTR(Password) {
		// Encode using open
		AddAuthentication();
		Browser->GetSubdirectory(AUTHENTICATION_TAG, Browser);
		Browser->AddChar(ASN1TAGU(ASN1_VISIBLESTRING), Password);
	} //else {
		// Encode using anonymous
		//AddAuthentication();
		//Browser->GetSubdirectory(AUTHENTICATION_TAG, Browser);
		//Browser->AddNULL(ASN1TAGU(ASN1_NULL));
	//}

	AddImplementationId(ImpId);
	AddImplementationName(ImpName);
	AddImplementationVersion(ImpVersion);

	delete Browser;
}

GDT_BOOLEAN ZINITREQUEST::GetGroupId(STRING *GroupId)
{
	if(!c_root->HasTag(AUTHENTICATION_TAG)) 
		return GDT_FALSE;
	c_root->GetSubdirectory(AUTHENTICATION_TAG, c_browser);
	if(!c_browser->HasTag(GROUPID_TAG))
		return GDT_FALSE;
	c_browser->GetChar(GROUPID_TAG, GroupId);
	return GDT_TRUE;
}

GDT_BOOLEAN ZINITREQUEST::GetUserId(STRING *UserId)
{
	if(!c_root->HasTag(AUTHENTICATION_TAG)) 
		return GDT_FALSE;
	c_root->GetSubdirectory(AUTHENTICATION_TAG, c_browser);
	if(!c_browser->HasTag(USERID_TAG))
		return GDT_FALSE;
	c_browser->GetChar(USERID_TAG, UserId);
	return GDT_TRUE;
}

GDT_BOOLEAN ZINITREQUEST::GetPassword(STRING *Password)
{
	if(!c_root->HasTag(AUTHENTICATION_TAG)) 
		return GDT_FALSE;
	c_root->GetSubdirectory(AUTHENTICATION_TAG, c_browser);
	if(!c_browser->HasTag(PASSWORD_TAG))
		return GDT_FALSE;
	c_browser->GetChar(PASSWORD_TAG, Password);
	return GDT_TRUE;
}

//***************************************************************************
//
//	ZINITRESPONSE - Constructor for (decoding) Init Response PDU
//
//		Buf - Buffer of data satisfying call to IsCompleteBER()
//		Length - length of Buf
//
//***************************************************************************
ZINITRESPONSE::ZINITRESPONSE(PUCHR Buf, INT4 Length):ZINITBASE(Buf, Length)
{
}

//***************************************************************************
//
//	ZINITRESPONSE - Constructor for Init Response PDU
//
//		RefId - Reference Identifier
//		Options - Z39.50 options supported
//		PrefMsgSize - PreferredMessageSize
//		ExceptionalRecSize - Maximum record size supported
//		Result - Result of initialize request
//			0 - failure
//			1 - success	
//		ImpId - Implementation Identifier
//		ImpName - Implementation Name
//		ImpVersion - Implementation Version
//
//		You can call this method passing PCHR instead of STRING
//			arguments.  In that case, the compiler will call 
//			STRING(PCHR x) for each PCHR arg and will call 
//			~STRING()at the end of the method.
//
//***************************************************************************
ZINITRESPONSE::ZINITRESPONSE(	const STRING& RefId,
				const STRING& ProtVer,
				const STRING& Options,
				const INT4 PrefMsgSize,
				const INT4 ExceptionalRecSize,
				const INT4 Result,
				const STRING& ImpId,
				const STRING& ImpName,
				const STRING& ImpVersion)
				:ZINITBASE(INITRESPONSE_TAG)
{
	AddReferenceId(RefId);
	AddProtocolVersion(ProtVer);
	AddOptions(Options);
	AddPreferredMessageSize(PrefMsgSize);
	AddExceptionalRecordSize(ExceptionalRecSize);
	AddResult(Result);
	AddImplementationId(ImpId);
	AddImplementationName(ImpName);
	AddImplementationVersion(ImpVersion);
}

//***************************************************************************
//
//	ZSEARCHREQUEST - Constructor for Search Request PDU
//
//		RefId - Reference Identifier
//		SmallSetUpperBound
//		LargeSetLowerBound
//		MedSetPresentNum
//			These values define when and how many records should
//			be piggy-backed on the search response.  If you
//			do NOT want records piggy-backed, rather you want
//			to explicitly request them from the result set via
//			Present, set to 0,1,0 respectively.  See the 
//			Z39.50 standard for more details.
//		ResultSetName - String identifier for the result set on
//			the server side (in case they support multiple
//			result sets.
//		DatabaseNames - A comma-delimited list of database names
//			in which to query.
//		SmallElementSetName - Record syntax for piggy-backed records.
//		MediumElementSetName - Record syntax for piggy-backed records.
//		PrefRecSyntax - Record syntax for piggy-backed records.
//		Query - A query string of the form:
//			"dog"
//			"AND(dog,cat)"
//			"OR(dog.cat)"
//			"dog[1=21,2=3]"
//			"AND(dog[1=21],OR(cat[1=4],mouse[1,4]))"
//***************************************************************************
ZSEARCHREQUEST::ZSEARCHREQUEST( const STRING& RefId,
				const INT4 SmallSetUpperBound,
				const INT4 LargeSetLowerBound,
				const INT4 MedSetPresentNum,
				const INT4 ReplaceIndicator,
				const STRING& ResultSetName,
				const STRING& DatabaseNames,
				const PCHR SmallElementSetName,
				const PCHR MediumElementSetName,
				const STRING& PrefRecSyntax,
				const STRING& Query,
				const STRING& AttrSetId,
				const INT QueryType) :
				ZSEARCHBASE(SEARCHREQUEST_TAG)
{

	AddReferenceId(RefId);
	AddSmallSetUpperBound(SmallSetUpperBound);
	AddLargeSetLowerBound(LargeSetLowerBound);
	AddMediumSetPresentNumber(MedSetPresentNum);
	AddReplaceIndicator(ReplaceIndicator);
	AddResultSetName(ResultSetName);
	
	// Add database names.  We only support one until we get a good
	// STRLIST method for spliting on commas
	AddDatabaseNames();
	STRING s;
	s = DatabaseNames;
	STRINGINDEX i = s.Search(',');
	if(i)
		s.EraseAfter(i-1);
	if(c_root->GetSubdirectory(DATABASENAMES_TAG, c_browser)) {
		STRING *os;
		os = new STRING(s);
		c_browser->AddChar(DATABASENAME_TAG, *os);
		delete os;
	}

	IFSTR(SmallElementSetName) {
		AddSmallSetElementSetNames();
		if(c_root->GetSubdirectory(SMALLSETELEMENTSETNAMES_TAG, 
			c_browser))
			c_browser->AddChar(GENERICELEMENTSETNAME_TAG,
				SmallElementSetName);
	}
	IFSTR(MediumElementSetName) {
		AddMediumSetElementSetNames();
		if(c_root->GetSubdirectory(MEDIUMSETELEMENTSETNAMES_TAG, 
			c_browser))
			c_browser->AddChar(GENERICELEMENTSETNAME_TAG,
				MediumElementSetName);
	}
	AddPreferredRecordSyntax(PrefRecSyntax);
	
	// Query
	AddQuery();
	c_root->GetSubdirectory(QUERY_TAG, c_browser);
/*
to test type 0 query, uncomment these lines and comment out those below

	c_browser->AddSubdirectory(TYPE0_TAG, c_browser);
	ASN1TAGU tag(ASN1_GENERALSTRING);
	c_browser->AddChar(tag, Query);
*/
	if(QueryType == 101) {
		c_browser->AddSubdirectory(TYPE1_TAG, c_browser);
		c_browser->AddOID(ASN1TAGU(ATTRIBUTESETID_TAG), AttrSetId);
		AddTerm(*c_browser, Query);
	}
/*
	c_browser->AddSubdirectory(TYPE1_TAG, c_browser);
	c_browser->AddOID(ASN1TAGU(ATTRIBUTESETID_TAG), BIB1_ATTRSET_OID);
	c_browser->AddSubdirectory(OP_TAG, c_browser);
	c_browser->AddSubdirectory(ATTRIBUTESPLUSTERM_TAG, c_browser);
	BERBROWSER sub(this);
	c_browser->AddSubdirectory(ATTRIBUTELIST_TAG, &sub);
	sub.AddSubdirectory(ASN1TAGU(ASN1_SEQUENCE), &sub);
	sub.AddInt(ATTRIBUTETYPE_TAG, 1);
	sub.AddInt(NUMERIC_TAG, 4);
	c_browser->AddChar(TERMGENERAL_TAG, Query);
*/
}

void ZSEARCHREQUEST::GetDatabaseName(STRING *DBName)
{
	*DBName = "";
	if(!c_root->HasTag(DATABASENAMES_TAG)) 
		return;
	c_root->GetSubdirectory(DATABASENAMES_TAG, c_browser);
	c_browser->GetChar(DATABASENAME_TAG, DBName);
}

ZSCANREQUEST::ZSCANREQUEST(	const PCHR RefId,
				const PCHR DatabaseNames,
				const PCHR AttributeSet,
				const PCHR TermList,
				INT4 StepSize,
				INT4 NumTermsRequested,
				INT4 PrefPosInResponse):
				ZSCANBASE(SCANREQUEST_TAG)
{
	AddReferenceId(RefId);
	// Add database names.  We only support one until we get a good
	// STRLIST method for spliting on commas
	AddDatabaseNames();
	STRING s;
	s = DatabaseNames;
	STRINGINDEX i = s.Search(',');
	if(i)
		s.EraseAfter(i-1);
	if(c_root->GetSubdirectory(DATABASENAMES_TAG, c_browser))
		c_browser->AddChar(DATABASENAME_TAG, s);

	AddTermListAndStartPoint();
	c_root->GetSubdirectory(ATTRIBUTESPLUSTERM_TAG, c_browser);
	BERBROWSER sub(this);
	c_browser->AddSubdirectory(ATTRIBUTELIST_TAG, &sub);
	sub.AddSubdirectory(ASN1_SEQUENCE, &sub);
	sub.AddInt(ATTRIBUTETYPE_TAG, 1);
	sub.AddInt(NUMERIC_TAG, 21);
	c_browser->AddChar(TERMGENERAL_TAG, TermList);
	
	//
	AddStepSize(StepSize);
	AddNumberOfTermsRequested(NumTermsRequested);
	AddPreferredPositionInResponse(PrefPosInResponse);
}

ZCLOSE::ZCLOSE(const PCHR RefId, INT4 Reason) : ZPDU(CLOSE_TAG)
{
	AddReferenceId(RefId);
	AddCloseReason(Reason);
}


//***************************************************************************
//
//	ZSEARCHRESPONSE - Constructor for Search Response PDU
//
//		RefId - Reference Identifier
//		ResultCount - Number of hits
//
//***************************************************************************
ZSEARCHRESPONSE::ZSEARCHRESPONSE(const STRING& RefId,
				const INT4 ResultCount,
				const INT4 NumberOfRecordsReturned,
				const INT4 NextResultSetPosition,
				const INT4 SearchStatus,
				const INT4 ResultSetStatus,
				const INT4 PresentStatus,
				ZRECORDLIST *Records)
				:ZSEARCHBASE(SEARCHRESPONSE_TAG)
{
	AddReferenceId(RefId);
	AddResultCount(ResultCount);
	AddNumberOfRecordsReturned(NumberOfRecordsReturned);
	AddNextResultSetPosition(NextResultSetPosition);
	AddSearchStatus(SearchStatus);
	if(SearchStatus == GDT_FALSE)
		AddResultSetStatus(ResultSetStatus);
	else
		AddPresentStatus(PresentStatus);
	if(Records)
		Records->AddBER(*c_browser);
}

//***************************************************************************
//
//	ZPRESENTREQUEST - Constructor for Present Request PDU
//
//		RefId - Reference Identifier
//
//***************************************************************************
ZPRESENTREQUEST::ZPRESENTREQUEST(const STRING& RefId,
				const STRING& ResultSetId,
				const INT4 ResultSetStartPoint,
				const INT4 NumberOfRecordsRequested,
				const STRING& ElementSetName,
				const STRING& PreferredRecordSyntax)
				:ZPRESENTBASE(PRESENTREQUEST_TAG)
{
	AddReferenceId(RefId);
	AddResultSetId(ResultSetId);
	AddResultSetStartPoint(ResultSetStartPoint);
	AddNumberOfRecordsRequested(NumberOfRecordsRequested);

	// Add Generic Element Set Name
	BERBROWSER sub(this);
	c_root->AddSubdirectory(SIMPLE_TAG, &sub);
	sub.AddChar(GENERICELEMENTSETNAME_TAG, ElementSetName);

	AddPreferredRecordSyntax(PreferredRecordSyntax);
}

void ZPRESENTREQUEST::GetElementSetName(STRING *ElementSetName)
{
	BERBROWSER sub(this);
	if(c_root->HasTag(SIMPLE_TAG)) {
		c_root->GetSubdirectory(SIMPLE_TAG, &sub);
		sub.GetChar(GENERICELEMENTSETNAME_TAG, ElementSetName);
	}
}

//***************************************************************************
//
//	ZPRESENTRESPONSE - Constructor for Present Response PDU
//
//		RefId - Reference Identifier
//
//***************************************************************************
ZPRESENTRESPONSE::ZPRESENTRESPONSE(const STRING& RefId,
				const INT4 NumberOfRecordsReturned,
				const INT4 NextResultSetPosition,
				const INT4 PresentStatus,
				ZRECORDLIST *Records)
				:ZPRESENTBASE(PRESENTRESPONSE_TAG)
{
	AddReferenceId(RefId);
	AddNumberOfRecordsReturned(NumberOfRecordsReturned);
	AddNextResultSetPosition(NextResultSetPosition);
	AddPresentStatus(PresentStatus);
	Records->AddBER(*c_browser);
}

//***************************************************************************
//
//	ZESREQUEST - Constructor for Extended Service Request PDU
//
//		RefId - Reference Identifier
//		Function - Create, delete, or modify
//		PackageType - OID of the extended service to use
//		PackageName - Task package name
//		UserId - User id
//		RetentionTime - Length of time to retain this task package
//		Description - Description of this task package
//		WaitAction - Whether the target should (or may) include
//			the task package in the response.  Choices are:
//			"wait", "wait if possible", "do not wait", or
//			"do not send task package back in the response"
//***************************************************************************
ZESREQUEST::ZESREQUEST(	const STRING& RefId,
			const INT4 Function,
			const STRING& PackageType,
			const STRING& PackageName,
			const STRING& UserId,
			const INT4 RetentionTime,
			const STRING& Description,
			const INT4 WaitAction)
			:ZESBASE(EXTENDEDSERVICESREQUEST_TAG)
{

	AddReferenceId(RefId);
	AddFunction(Function);
	AddPackageType(PackageType);
	AddPackageName(PackageName);
	AddUserId(UserId);
	AddRetentionTime(RetentionTime);
	AddPermissions();
	AddDescription(Description);
	AddWaitAction(WaitAction);
}

//***************************************************************************
//
//	ZESRESPONSE - Constructor for Extended Service Response PDU
//
//		RefId - Reference Identifier
//		OperationStatus - Operation Status
//		DiagRecord - Diagnostic record
//		TaskPackage - Task Package record
//***************************************************************************
ZESRESPONSE::ZESRESPONSE(	const STRING& RefId,
				const INT4 OperationStatus,
				ZDEFAULTDIAGFORMAT *DiagRecord,
				ZESTASKPACKAGE *TaskPackage)
				:ZESBASE(EXTENDEDSERVICESRESPONSE_TAG)
{

	AddReferenceId(RefId);
	AddOperationStatus(OperationStatus);

	if ( DiagRecord ) {
		c_root->AddSubdirectory(ES_DIAGNOSTICS_TAG, c_browser);

		ASN1TAGU seqtag(ASN1_SEQUENCE);
		BERBROWSER sub(*c_browser);

		c_browser->AddSubdirectory(seqtag, &sub);
		DiagRecord->AddBER(sub);
	}

	if ( TaskPackage ) {
		c_root->AddSubdirectory(ES_TASKPACKAGE_TAG, c_browser);
		TaskPackage->AddBER(*c_browser);
	}
}

GDT_BOOLEAN ZESRESPONSE::GetTaskStatus(INT4 *TaskStatus)
{
	if( !c_root->GetSubdirectory(ES_TASKPACKAGE_TAG, c_browser) )
		return GDT_FALSE;

	ASN1TAGU exttag(ASN1_EXTERNAL);

	if( !c_browser->GetSubdirectory(exttag, c_browser) )
		return GDT_FALSE;

	ASN1TAGU oidtag(ASN1_OBJECTIDENTIFIER);
	STRING OID;

	if( !c_browser->GetOID(oidtag, &OID) || OID != EXTENDEDSERVICES_OID)
		return GDT_FALSE;

	if( !c_browser->GetInt(ES_TP_TASKSTATUS_TAG, TaskStatus) )
		return GDT_FALSE;

	return GDT_TRUE;
}

//***************************************************************************
//
//	ZESUPDATEREQUEST - Constructor for ES Update Request PDU
//
//		RefId - Reference Identifier
//		Action - Action to perform
//		DatabaseName - Database to update
//		Records - Records to update in the database
//***************************************************************************
ZESUPDATEREQUEST::ZESUPDATEREQUEST( const STRING& RefId,
				const INT4 Action,
				const STRING& DatabaseName,
				ZRECORDLIST *Records) :
				ZESREQUEST(RefId,
					ES_CREATE_FUNCTION,
					ES_UPDATE_OID,
					"",
					"",
					0,
					"",
					ES_WAIT_WAITACTION)
{

	c_root->AddSubdirectory(ES_TASKSPECIFIC_TAG, c_browser);

	ASN1TAGU exttag(ASN1_EXTERNAL);

	c_browser->AddSubdirectory(exttag, c_browser);

	ASN1TAGU oidtag(ASN1_OBJECTIDENTIFIER);

	c_browser->AddOID(oidtag, ES_UPDATE_OID);

	c_browser->AddSubdirectory(ES_TS_ESREQUEST_TAG, c_browser);

	BERBROWSER sub(*c_browser);

	c_browser->AddSubdirectory(ES_TS_TOKEEP_TAG, &sub);

	sub.AddInt(ES_UPDATE_ACTION_TAG, Action);
	sub.AddChar(ES_UPDATE_DATABASENAME_TAG, DatabaseName);

	// Hopefully the record list was created using ES_TS_NOTTOKEEP_TAG
	// as the record type.

	Records->AddBER(*c_browser);
}

GDT_BOOLEAN ZESUPDATEREQUEST::GetAction(INT4 *Action)
{

	if( !c_root->GetSubdirectory(ES_TASKSPECIFIC_TAG, c_browser) )
		return GDT_FALSE;

	ASN1TAGU exttag(ASN1_EXTERNAL);

	if( !c_browser->GetSubdirectory(exttag, c_browser) )
		return GDT_FALSE;

	ASN1TAGU oidtag(ASN1_OBJECTIDENTIFIER);
	STRING OID;

	if( !c_browser->GetOID(oidtag, &OID) || OID != ES_UPDATE_OID )
		return GDT_FALSE;

	if( !c_browser->GetSubdirectory(ES_TS_ESREQUEST_TAG, c_browser) )
		return GDT_FALSE;

	if( !c_browser->GetSubdirectory(ES_TS_TOKEEP_TAG, c_browser) )
		return GDT_FALSE;

	if( !c_browser->GetInt(ES_UPDATE_ACTION_TAG, Action) )
		return GDT_FALSE;

	return GDT_TRUE;
}

GDT_BOOLEAN ZESUPDATEREQUEST::GetDatabaseName(STRING *DatabaseName)
{

	if( !c_root->GetSubdirectory(ES_TASKSPECIFIC_TAG, c_browser) )
		return GDT_FALSE;

	ASN1TAGU exttag(ASN1_EXTERNAL);

	if( !c_browser->GetSubdirectory(exttag, c_browser) )
		return GDT_FALSE;

	ASN1TAGU oidtag(ASN1_OBJECTIDENTIFIER);
	STRING OID;

	if( !c_browser->GetOID(oidtag, &OID) || OID != ES_UPDATE_OID )
		return GDT_FALSE;

	if( !c_browser->GetSubdirectory(ES_TS_ESREQUEST_TAG, c_browser) )
		return GDT_FALSE;

	if( !c_browser->GetSubdirectory(ES_TS_TOKEEP_TAG, c_browser) )
		return GDT_FALSE;

	if( !c_browser->GetChar(ES_UPDATE_DATABASENAME_TAG, DatabaseName) )
		return GDT_FALSE;

	return GDT_TRUE;
}

//***************************************************************************
//
//	ZESUPDATERESPONSE - Constructor for ES Update Response PDU
//
//		RefId - Reference Identifier
//		TaskPackage - Task Package record
//		Action - Action performed
//		DatabaseName - Database updated
//		UpdateStatus - Update Status
//***************************************************************************
ZESUPDATERESPONSE::ZESUPDATERESPONSE(	const STRING& RefId,
				ZESTASKPACKAGE *TaskPackage,
				const INT4 Action,
				const STRING& DatabaseName,
				const INT4 UpdateStatus,
				ZDEFAULTDIAGFORMAT *DiagRecord)
				:ZESRESPONSE(RefId,
					ES_DONE_OPERATIONSTATUS,
					NULL,
					TaskPackage)
{

	if ( !TaskPackage )
		return;

	c_root->GetSubdirectory(ES_TASKPACKAGE_TAG, c_browser);

	ASN1TAGU exttag(ASN1_EXTERNAL);

	c_browser->GetSubdirectory(exttag, c_browser);

	c_browser->GetSubdirectory(ES_TP_TASKSPECIFIC_TAG, c_browser);

	c_browser->GetSubdirectory(exttag, c_browser);

	c_browser->GetSubdirectory(ES_TS_TASKPACKAGE_TAG, c_browser);

	BERBROWSER sub(*c_browser);

	c_browser->AddSubdirectory(ES_TS_TOKEEP_TAG, &sub);

	sub.AddInt(ES_UPDATE_ACTION_TAG, Action);
	sub.AddChar(ES_UPDATE_DATABASENAME_TAG, DatabaseName);

	c_browser->AddSubdirectory(ES_TS_TARGETPART_TAG, &sub);

	sub.AddInt(ES_UPDATE_UPDATESTATUS_TAG, UpdateStatus);

	if ( DiagRecord ) {
		sub.AddSubdirectory(ES_UPDATE_GLOBALDIAGNOSTICS_TAG, c_browser);

		ASN1TAGU seqtag(ASN1_SEQUENCE);

		c_browser->AddSubdirectory(seqtag, &sub);
		DiagRecord->AddBER(sub);
	}
}

GDT_BOOLEAN ZESUPDATERESPONSE::GetUpdateStatus(INT4 *UpdateStatus)
{
	if( !c_root->GetSubdirectory(ES_TASKPACKAGE_TAG, c_browser) )
		return GDT_FALSE;

	ASN1TAGU exttag(ASN1_EXTERNAL);

	if( !c_browser->GetSubdirectory(exttag, c_browser) )
		return GDT_FALSE;

	ASN1TAGU oidtag(ASN1_OBJECTIDENTIFIER);
	STRING OID;

	if( !c_browser->GetOID(oidtag, &OID) || OID != EXTENDEDSERVICES_OID )
		return GDT_FALSE;

	STRING PackageType;

	if( !c_browser->GetOID(ES_TP_PACKAGETYPE_TAG, &PackageType) ||
		PackageType != ES_UPDATE_OID )
		return GDT_FALSE;

	if( !c_browser->GetSubdirectory(ES_TP_TASKSPECIFIC_TAG, c_browser) )
		return GDT_FALSE;

	if ( !c_browser->GetSubdirectory(exttag, c_browser) )
		return GDT_FALSE;

	if( !c_browser->GetOID(oidtag, &OID) || OID != PackageType )
		return GDT_FALSE;

	if( !c_browser->GetSubdirectory(ES_TS_TASKPACKAGE_TAG, c_browser) )
		return GDT_FALSE;

	if( !c_browser->GetSubdirectory(ES_TS_TARGETPART_TAG, c_browser) )
		return GDT_FALSE;

	if( !c_browser->GetInt(ES_UPDATE_UPDATESTATUS_TAG, UpdateStatus) )
		return GDT_FALSE;

	return GDT_TRUE;
}

GDT_BOOLEAN AddTerm(BERBROWSER & b, const STRING& Term)
{
	if(IsBoolean(Term)) {
		STRING Term1, Term2, Operator;
		BERBROWSER sub(b);
		b.AddSubdirectory(RPNRPNOP_TAG, &sub);
		GetTerm(Term, &Term1, &Term2);
		AddTerm(sub, Term1);
		AddTerm(sub, Term2);
		sub.AddSubdirectory(OPERATOR_TAG, &sub);
		GetOperator(Term, &Operator);
		Operator.UpperCase();
		if(Operator == "AND")
			sub.AddNULL(AND_TAG);
		else if(Operator == "OR")
			sub.AddNULL(OR_TAG);
		else if(Operator == "ANDNOT")
			sub.AddNULL(ANDNOT_TAG);
	} else {
		BERBROWSER sub(b);
		b.AddSubdirectory(OP_TAG, &sub);
		sub.AddSubdirectory(ATTRIBUTESPLUSTERM_TAG, &sub);
		BERBROWSER sub2(sub);
		BERBROWSER sub3(sub);
		sub.AddSubdirectory(ATTRIBUTELIST_TAG, &sub2);
		INT 	i,
			attrcount;
		STRLIST AttrList;
		STRING TermOnly;
		ParseList(Term, &AttrList, &TermOnly);
//AttrList.AddEntry("1");
//AttrList.AddEntry("4");
		attrcount = AttrList.GetTotalEntries();
		INT4 Type, Value;
		STRING Temp;
		for(i=1;i <= attrcount;i+=2) {
			AttrList.GetEntry(i, &Temp);
			Type = Temp.GetInt();
			AttrList.GetEntry(i+1, &Temp);
			Value = Temp.GetInt();
			sub2.AddSubdirectory(ASN1TAGU(ASN1_SEQUENCE), &sub3);
			sub3.AddInt(ATTRIBUTETYPE_TAG, Type);
			sub3.AddInt(NUMERIC_TAG, Value);
		}
		sub.AddChar(TERMGENERAL_TAG, TermOnly);
	}
	return GDT_TRUE;
}

GDT_BOOLEAN IsBoolean(const STRING& Term)
{
	STRING Temp;
	Temp = Term;
	Temp.UpperCase();
	if((Temp.Search("AND(")) ||
		(Temp.Search("OR(")) ||
		(Temp.Search("ANDNOT(")))
		return GDT_TRUE;
	return GDT_FALSE;
}

//
// Pass a boolean term of the form:
//
//	and(dog[1=2],cat[1=2])
//
//	GetTerm(1, "and(dog[1=21],cat)", &Term1, &Term2)
//
// On return, Term1="dog[1=21]", Term2="cat"
// 
void GetTerm(const STRING &Term, STRING *Term1, STRING *Term2)
{
	STRING Temp;
	STRINGINDEX p;
	Temp = Term;
	p = Temp.Search("(");
	Temp.EraseBefore(p+1);
	p = Temp.SearchReverse(")");
	Temp.EraseAfter(p-1);

	CHR *buf, *term1, *term2;
	buf = Temp.NewCString();
	term1 = Temp.NewCString();
	term2 = Temp.NewCString();
	INT 	len=Temp.GetLength(),
		term1pos=0,
		i;
	enum { NONE, INPARENS } State;
	State = NONE;
	GDT_BOOLEAN Done = GDT_FALSE;
	INT parencount=0;
	for(i=0;i < len;i++) {
		switch(State) {
			case NONE:
				switch(buf[i]) {
					case '[':
					case '(':
						State = INPARENS;
						parencount++;
						term1[term1pos] = buf[i];
						term1pos++;
						break;
					case ',':
						term1[term1pos] = '\0';
						strcpy(term2, buf+i+1);
						Done = GDT_TRUE;
						break;
					default:
						term1[term1pos] = buf[i];
						term1pos++;
						break;
				}
				break;
			case INPARENS:
				switch(buf[i]) {
				case '(':
				case '[':
				  parencount++;
				  break;
				case ')':
				case ']':
				  parencount--;
				  if(parencount==0)
				    State = NONE;
				  break;
				}

				term1[term1pos] = buf[i];
				term1pos++;
				break;
		}
		if(Done)
			break;
	}
	*Term1 = term1;
	*Term2 = term2;
}

void GetOperator(const STRING &Term, STRING *TheOperator)
{
	STRING Temp;
	Temp = Term;
	STRINGINDEX p = Temp.Search("(");
	Temp.EraseAfter(p-1);
	*TheOperator = Temp;
}

void ParseList(const STRING &Term, STRLIST *AttrList, STRING *TermOnly)
{
	STRINGINDEX p;
	STRING Temp = Term;

	AttrList->Clear();

	if((p = Temp.Search("[")) == 0) {
		// No attribute list specified
		*TermOnly = Term;
		return;
	}

	CHR *termonly = Term.NewCString();
	termonly[p-1] = '\0';
	*TermOnly = termonly;
	delete [] termonly;

	Temp.EraseBefore(p+1);
	p = Temp.SearchReverse("]");	
	Temp.EraseAfter(p-1);
	INT len;
	CHR *buf = Temp.NewCString();
	len = Temp.GetLength();
	CHR Type[32], Value[32];
	INT TypePos=0, ValuePos=0, i;
	enum { INTYPE, INVALUE } State;
	State = INTYPE;

	for(i=0;i < len;i++) {
		switch(State) {
			case INTYPE:
				if((buf[i] == '=') || (buf[i] == ',')) {
					State = INVALUE;
					Type[TypePos] = '\0';
					AttrList->AddEntry(Type);
					TypePos = 0;
				} else Type[TypePos++] = buf[i];				
				break;
			case INVALUE:
				if(buf[i] == ',') {
					State = INTYPE;
					Value[ValuePos] = '\0';
					AttrList->AddEntry(Value);
					ValuePos = 0;
				} else Value[ValuePos++] = buf[i];				
				break;
		}
	}
	Value[ValuePos] = '\0';
	AttrList->AddEntry(Value);
}
