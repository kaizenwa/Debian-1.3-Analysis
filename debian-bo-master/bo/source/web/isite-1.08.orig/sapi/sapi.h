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
File:           sapi.h
Version:        1.00
Description:    Search API
Author:         Kevin Gamiel, kgamiel@cnidr.org
@@@*/

#ifndef _sapi_
#define _sapi_

#include "gdt.h"
#include "list.h"
#include "ini.h"
#include "strbox.h"

#include "rset.hxx"
#include "idb.hxx"

#ifdef SAPI_CONQUEST
#include "tsrbasic.h"
#include "hl_api.h"
#endif /* SAPI_CONQUEST */

#ifdef SAPI_CARL
#include "carl.hxx"
#endif

/* Valid database types */
#define SAPI_DBTYPE_ISEARCH 0	/* Isearch */
#define SAPI_DBTYPE_SCRIPT 2	/* Script */
#define SAPI_DBTYPE_CONQUEST 4	/* Conquest Software, Inc. */
#define SAPI_DBTYPE_PGIF 7	/* Postgres interface */
#define SAPI_DBTYPE_CARL 8 	// CARL

typedef struct tagSAPI_RESULTSET *PSAPI_RESULTSET;
typedef struct tagSAPI_DBASE *PSAPI_DBASE;
typedef struct tagSAPI *PSAPI;
typedef struct tagSAPI_FIELD *PSAPI_FIELD;
typedef struct tagSAPI_QUERY *PSAPI_QUERY;

enum QUERYATOMTYPE { t_Operand, t_Operator};

struct tagQUERYATOM {
	enum QUERYATOMTYPE which;
	PCHR Data;
};

typedef struct tagQUERYATOM QUERYATOM;
typedef struct tagQUERYATOM *PQUERYATOM;

typedef struct tagSAPI_FIELD {
	PCHR Name;
	INT Type;
} SAPI_FIELD;

typedef struct tagSAPI_RESULTSET {
	PCHR Name;
	INT4 HitCount;
	INT Diagnostic;
	PSAPI_DBASE Database;
	PRSET prset;
} SAPI_RESULTSET;

typedef struct tagSAPI_DBASE {
	PCHR Name;
	INT Type;
	PCHR Location;		/* 
				if Type is CGI, this is cgi script, 
				if Type is File, this is command-line script
				otherwise, this is path of database 
				*/
	PCHR Results;		/* If Type is File, results go here */
	LIST *Fields;
#ifdef SAPI_CONQUEST
	HLTHREAD QueryThread;
#endif
	PIDB pdb;
#ifdef SAPI_CARL
	CARL_DB_DRIVER *CarlDriver;
#endif
} SAPI_DBASE;

typedef struct tagSAPI_QUERY {
	char *Term;
	STACK *Stack;
	SQUERY *squery;	
} SAPI_QUERY;

typedef struct tagSAPI {
	PCHR ConfigFile;
	LIST *Database;
} SAPI;


#define NULL_SAPI ((PSAPI)NULL)
#define NULL_DBASE ((PSAPI_DBASE)NULL)
#define NULL_QUERY ((PSAPI_QUERY)NULL)
#define NULL_RESULTSET ((PSAPI_RESULTSET)NULL)

PSAPI sapi_CreateInitFile(PCHR File, PCHR Group);
void sapi_Destroy(PSAPI s);
INT sapi_GetDatabaseCount(PSAPI s);
PSAPI_DBASE sapi_GetDatabaseX(PSAPI s, INT i);
PSAPI_DBASE sapi_GetDatabase(PSAPI s, PCHR Database);
PSAPI_RESULTSET sapi_Search(PSAPI s, PSAPI_DBASE Database, PSAPI_QUERY Query);
void sapi_AddDatabase(PSAPI s, PSAPI_DBASE d);

PSAPI_DBASE dbase_Create();
PSAPI_DBASE dbase_CreateInit(PCHR Name, INT Type, PCHR Location, PCHR Results);
PSAPI_DBASE dbase_CreateInitFile(PCHR File, PCHR Group);
void dbase_Destroy(PSAPI_DBASE d);
PCHR dbase_GetName(PSAPI_DBASE d);
INT dbase_GetFieldCount(PSAPI_DBASE d);
PSAPI_FIELD dbase_GetField(PSAPI_DBASE d, INT i);
void dbase_GetFieldNames(PSAPI_DBASE d, STRLIST *FieldNames);
INT dbase_GetType(PSAPI_DBASE d);

PCHR field_GetName(PSAPI_FIELD f);
INT field_GetType(PSAPI_FIELD f);

PSAPI_RESULTSET ResultSet_Create();
void ResultSet_SetHitCount(PSAPI_RESULTSET r, INT4 h);
void ResultSet_Destroy(PSAPI_RESULTSET r);
INT4 ResultSet_GetHitCount(PSAPI_RESULTSET r);
void ResultSet_SetDatabase(PSAPI_RESULTSET r, PSAPI_DBASE d);
PCHR ResultSet_GetRecord(PSAPI_RESULTSET RS, INT4 RecordNum, INT4 MaxLen,
        INT4 *ActualLen, PCHR ElementSet, PCHR RecordSyntax,
	STRING *ActualRecordSyntax);

PSAPI_QUERY query_Create();
PSAPI_QUERY query_CreateSimple(PCHR s);
PSAPI_QUERY query_CreateStack(STACK *S);
PSAPI_QUERY query_CreateIsearch(SQUERY & Query);
void query_Destroy(PSAPI_QUERY q);

void sapi_Error(PCHR Function, INT Code, PCHR Msg);

#endif

