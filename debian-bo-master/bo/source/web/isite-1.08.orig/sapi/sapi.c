/***********************************************************************
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
File:           sapi.c
Version:        1.00
Description:    Search API
Author:         Kevin Gamiel, kgamiel@cnidr.org
@@@*/

/*
Define which search engine to compile into zserver via the Makefile
*/
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>

#if defined(_MSDOS) || defined(_WIN32)
#include <process.h>
#else
#include <unistd.h>
#endif

#include "config.h"
#include "gdt.h"
#include "idb.hxx"
#include "string.hxx"
#include "common.hxx"
#include "squery.hxx"
#include "rset.hxx"

#include "script.h"

#ifdef SAPI_CARL
#include "carl.hxx"
#endif

#ifdef SAPI_PGIF
#include "pgif.h"
#endif

#ifdef SAPI_CONQUEST
#include <string.h>
#include <ctype.h>
#include "tsrbasic.h"
#include "hl_api.h"
#define EXTFLG
#include "conquest.h"
#endif /* SAPI_CONQUEST */

#ifndef NEWSTRUCT
#define NEWSTRUCT(a) (a*)calloc(1,sizeof(a))
#endif

#ifndef NEWSTRING
#define NEWSTRING(a) (PCHR)calloc(1,a)
#endif

#ifndef FREE
#define FREE(a) free(a)
#endif

//#include "libcnidr.h"
#include "sapi.h"

/* Error messages */
#define SAPI_NOMEM 0
#define SAPI_BADFILE 1
#define SAPI_BADDBLIST 2
 
CHR Temp[MAXPROFILESTR];

PSAPI sapi_Create()
{
  PSAPI s;

  if((s=NEWSTRUCT(SAPI))==NULL) {
    fprintf(stderr, "sapi_Create:Out of memory\n");
    return NULL;
  }

  s->ConfigFile = NULL;
  s->Database = list_Create();

  return s;
}

void sapi_Destroy(PSAPI s)
{
  if(s==NULL)
    return;
  
  if(s->ConfigFile!=NULL)
    FREE(s->ConfigFile);
  if(s->Database) {
    /* KAG punting on freeing list right now ;-)*/
  }
  
  FREE(s);
}

PSAPI sapi_CreateInitFile(PCHR File, PCHR Group)
{
  PSAPI s;
  CHR TempDB[MAXPROFILESTR];
  PSAPI_DBASE DB;
  INT i, err;
  struct stat Stat;

  /* Create a new SAPI structure */
  if((s=sapi_Create())==NULL)
    return NULL;

  /* Copy the configuration file name */
  if((s->ConfigFile=NEWSTRING(strlen(File)+1))==NULL) {
    sapi_Error("sapi_CreateInit", SAPI_NOMEM, "");
    sapi_Destroy(s);
    return NULL;
  }
  strcpy(s->ConfigFile, File);

  /* Does the file exist?  */
  err = stat(File, &Stat);
  if(err != 0) {
    sprintf(Temp, "sapi_CreateInitFile [%s]", File);
    perror(Temp);
    return NULL_SAPI;
  }
	
  /* Open the file */
  if ( CacheProfileFile(File) == 0 ) {
    sapi_Error("sapi_CreateInit", SAPI_BADFILE, File);
    sapi_Destroy(s);
    return NULL;
  }
	
  /* Load the SAPI structures with contents of file */

  /* Read the DBList entry to get list of all database names */
  GetPrivateProfileString(Group, "DBList", "", Temp,sizeof(Temp),File);
  if(Temp[0]=='\0') {
    sapi_Error("sapi_CreateInit", SAPI_BADDBLIST, File);
    sapi_Destroy(s);
    return NULL;
  }
	
  /* Step through each database name in the list */
  for(i=1;i<=strbox_itemCount(Temp);i++) {
    strbox_item(Temp, i, TempDB);	
    if(Temp[0]=='\0') {
      fprintf(stderr,
	      "sapi_CreateInitFile:Bad item[%i] in DBList\n",
	      i);
      continue;
    }
    DB = dbase_CreateInitFile(File, TempDB);
    if(DB!=NULL)
      sapi_AddDatabase(s, DB);
  }

  /* Close file */
  if (UncacheProfileFile() == 0)
    AbortProfileChanges();

  return s;
}

void sapi_AddDatabase(PSAPI s, PSAPI_DBASE d)
{
  if(s==NULL)
    return;

  if(s->Database==NULL)
    return;

  if(d==NULL)
    return;

  list_InsertAfter(s->Database, list_Last(s->Database),d, LIST_NEAR);
}

INT sapi_GetDatabaseCount(PSAPI s) 
{
  if(s==NULL)
    return -1;

  if(s->Database==NULL)
    return -1;

  return list_Length(s->Database);	
}

/*
This only supports a Query of type Stack.  See SAPI_QUERY structure and
methods for details.
*/
PSAPI_RESULTSET sapi_Search(PSAPI s, PSAPI_DBASE Database, PSAPI_QUERY Query)
{
  PSAPI_RESULTSET r;

  if((s == NULL_SAPI) || (Database == NULL_DBASE) || 
     (Query == NULL_QUERY) ||
     ((Query->Stack==NULL) && (Query->Term == (PCHR)NULL) &&
      (Query->squery == NULL))) {
    fprintf(stderr, "sapi_Search:Bad input to call\n");
    return NULL;
  }

  if((r=ResultSet_Create())==NULL)
    return NULL;

  ResultSet_SetDatabase(r,Database);

#ifdef SAPI_DEBUG
  fprintf(stderr, "sapi_Search()\n");
  fprintf(stderr, "\tDatabase Name:%s\n", Database->Name);
  fprintf(stderr, "\tLocation:%s\n", Database->Location);
  fprintf(stderr, "\tType:%i\n", Database->Type);
  if(Database->Results)
    fprintf(stderr, "\tResults:%s\n", Database->Results);
#endif
  switch(Database->Type) {
  case SAPI_DBTYPE_ISEARCH: {
    /* Has database object been created? */
    if(Database->pdb==NULL) {
      fprintf(stderr,
	      "sapi_Search:Invalid database\n");
      return NULL;
    }
    
    if(Query->squery == NULL) {
      cerr << "sapi_Search():";
      cerr << "We only support the new Isearch";
      cerr << " query structures" << endl;
      return NULL;
    }
    
    /* Execute the search */
    r->prset=Database->pdb->Search(*Query->squery);
    
    /* How many hits? */
    r->HitCount = r->prset->GetTotalEntries();		

#ifdef SAPI_DEBUG
    fprintf(stderr, "sapi_Search:Isearch Search Engine\n");
    fprintf(stderr, "\tHitCount:%d\n", r->HitCount);
#endif  // SAPI_DEBUG
    break;
  }
#ifdef SAPI_CGI
  case SAPI_DBTYPE_CGI:
    fprintf(stderr, "sapi_Search:No CGI yet\n");
    r->HitCount = 0;
    break;
#endif
  case SAPI_DBTYPE_SCRIPT: {
    /* 
       SCRIPT search engine is a pretty stupid one.
       You need to feed it the terms as a single string
       on the command line, so piece that string
       together now.
       */	
    PCHR Term;
    PQUERYATOM Atom;
    
    /* Is term a stack or simple term? */
    if(Query->Term != (PCHR)NULL) {
      Term = (CHR *)malloc(strlen(Query->Term)+1);
      strcpy(Term, Query->Term);
    } else {
      Atom = (PQUERYATOM)stack_Top(Query->Stack);
      
      if(Atom == NULL) {
	fprintf(stderr, 
		"sapi_Search:Null query\n");
	return NULL;
      }
      
      Term=NEWSTRING(strlen(Atom->Data)+1);
      strcpy(Term, Atom->Data);

    }
    
    /* Now call the SCRIPT search engine */
    r->HitCount = script_Search(Database->Location,
				Database->Results, Term, &r->Diagnostic);
    
    FREE(Term);
    
    break;
  }	
#ifdef SAPI_PGIF
  case SAPI_DBTYPE_PGIF: {
    SQUERY query;
    /*PRSET prset;*/
    RESULT result;
    INT hits;
    STRING s;
    PCHR Term;
 
    
    /* Get single term */
    PQUERYATOM Atom = (PQUERYATOM)stack_Top(Query->Stack);
    if(Atom == NULL) {
      fprintf(stderr, "sapi_Search:Null query\n");
      return NULL;
    }
    Term = NEWSTRING(strlen(Atom->Data)+1);
    strcpy(Term, Atom->Data);
    fprintf(stderr,"Search term: %s\n",Term);
    
    r->HitCount = pgif_search(Term);
    break;
  }
#endif
#ifdef SAPI_CONQUEST
  case SAPI_DBTYPE_CONQUEST: {
    ResultSet_Destroy(r);
    r = conquest_Search(s, Database, Query->Stack);
    break;
  }
#endif
#ifdef SAPI_CARL
  case SAPI_DBTYPE_CARL: {
    if(Query->Term == NULL) {
      cerr << "sapi_Search: Single terms only!";
      cerr << endl;
    }
    
    if(!Database->CarlDriver->OpenDatabase(
					   Database->Location))
      return NULL;
    
    r->HitCount = Database->CarlDriver->Search(Query->Term, 
					       Database->Name);
    if(r->HitCount < 0) {
      ResultSet_Destroy(r);
      return NULL;
    }

    break;
  }
#endif
  default:
    fprintf(stderr, "sapi_Search:Bad DB type[%i]\n",
	    Database->Type);
    return NULL;
  }

  return r;
}

/*
	1 <= x <= sapi_GetDatabaseCount()
*/
PSAPI_DBASE sapi_GetDatabaseX(PSAPI s, INT x)
{
  POSITION *Pos;
  PSAPI_DBASE DBase;
  INT i, count;
  CHR msg[128];

  if(s == NULL_SAPI) {
    fprintf(stderr, "sapi_GetDatabaseX: NULL sapi value (??)\n");
    return NULL_DBASE;
  }

  if(s->Database == (LIST *)NULL) {
    fprintf(stderr, 
	    "sapi_GetDatabaseX:0 databases to request from\n");
    return NULL_DBASE;
  }

  count = list_Length(s->Database);

  if((x < 1) || (x > count)) {
    sprintf(msg, "sapi_GetDatabaseX:Value out of range [%i]\n", x);
    fprintf(stderr, msg);
    return NULL_DBASE;
  }

  Pos = list_First(s->Database);

  for(i=1;i < x;i++)
    Pos = list_Next(s->Database, Pos);

  DBase = (PSAPI_DBASE)list_Retrieve(s->Database, Pos);
  if((DBase == NULL_DBASE) || (DBase->Name == (PCHR)NULL)) {
    fprintf(stderr, 
	    "sapi_GetDatabaseX: Database unavailable\n");
    return NULL_DBASE;
  }

  return sapi_GetDatabase(s, DBase->Name);
}

/*
Initializes the database and returns a pointer to the SAPI_DBASE object or NULL.
*/
PSAPI_DBASE sapi_GetDatabase(PSAPI s, PCHR DatabaseName)
{
  POSITION *Pos;
  PSAPI_DBASE DBase;
  INT i, Found=0;
	
  if(s==NULL) {
    fprintf(stderr, "sapi_GetDatabase: NULL sapi value (??)\n");
    return NULL;
  }

  if((DatabaseName==NULL)||(DatabaseName[0]=='\0')) {
    fprintf(stderr, "sapi_GetDatabase: NULL Database requested\n");
    return NULL;
  }

  if(s->Database == NULL) {
    fprintf(stderr, 
	    "sapi_GetDatabase:0 databases from which to request\n");
    return NULL;
  }

  Pos = list_First(s->Database);
  int dbcount = list_Length(s->Database);
  /* Step through all database names in list */
  for(i=1;i<=dbcount;i++) {
    DBase = (PSAPI_DBASE)list_Retrieve(s->Database, Pos);
    if(DBase == NULL) {
      continue;
    }
    if(!strcasecmp(DBase->Name, DatabaseName)) {
      Found = 1;
      break;
    }
    Pos = list_Next(s->Database, Pos);
  }
  if((DBase==NULL) || (Found == 0)){
    fprintf(stderr, "sapi_GetDatabase:Database unavailable:[%s]\n", 
	    DatabaseName);
    return NULL;
  }
  /* Initialize/Validate database */
  switch(DBase->Type) {
  case SAPI_DBTYPE_ISEARCH: {
    STRING DBPathName, DBFileName;
    
#ifdef SAPI_DEBUG
    fprintf(stderr, "sapi_GetDatabase:Location=%s\n",
	    DBase->Location);
    fprintf(stderr, "sapi_GetDatabase:Name=%s\n",
	    DBase->Name);
#endif
    /* Open database */
    DBPathName=DBase->Location;
    DBFileName=DBase->Name;
    if((DBase->pdb=new IDB(DBPathName,DBFileName))==NULL) {
      fprintf(stderr, 
	      "sapi_GetDatabase:ISEARCH failed\n");
      return NULL;
    }
    
    /* "Validate" by checking number of records */
    if(DBase->pdb->GetTotalRecords()<=0) {
      fprintf(stderr, 
	      "sapi_GetDatabase:ISEARCH database failed\n");
      fprintf(stderr, 
	      "sapi_GetDatabase:0 records available\n");
      delete DBase->pdb;	
      return NULL;
    }
#ifdef SAPI_DEBUG
    fprintf(stderr, 
	    "records available = %d\n", 
	    DBase->pdb->GetTotalRecords());
#endif
    
    break;
  }
  case SAPI_DBTYPE_SCRIPT: {
    INT err;
    /* Does the file exist?  */
    struct stat Stat;
    err = stat(DBase->Location, &Stat);
    if(err != 0) {
      sprintf(Temp, "sapi_GetDatabase [%s]", 
	      DBase->Location);
      perror(Temp);
      return NULL_DBASE;
    }
    break;
  }
#ifdef SAPI_CONQUEST
  case SAPI_DBTYPE_CONQUEST:
    break;
#endif
#ifdef SAPI_PGIF
  case SAPI_DBTYPE_PGIF:
    pgif_init(DBase->Location);
    break;
#endif
#ifdef SAPI_CARL
  case SAPI_DBTYPE_CARL: {
    if(DBase->Location[0] == '\0') {
      cerr << "sapi_GetDatabase: CARL databases ";
      cerr << "must include a Location directive ";
      cerr << "specifying the hostname of the ";
      cerr << "remote database engine." << endl;
      exit(1);
    }
    DBase->CarlDriver = new CARL_DB_DRIVER();
    break;
  }
#endif
  default:
    sprintf(Temp, 
	    "sapi_GetDatabase [%s] Database type not supported\n", 
	    DatabaseName);
    fprintf(stderr, Temp);
    fprintf(stderr, 
	    "sapi_GetDatabase: Type %i probably not compiled in\n",
	    DBase->Type);	
    return NULL;
  }

  return DBase;
}

PSAPI_DBASE dbase_Create()
{
  PSAPI_DBASE d;

  if((d=NEWSTRUCT(SAPI_DBASE))==NULL) {
    fprintf(stderr, "dbase_Create:Out of memory\n");
    return NULL;
  }

  d->Name = NULL;
  d->Type = SAPI_DBTYPE_ISEARCH;
  d->Location = NULL;
  d->Results = NULL;
	
  return d;
}

/*
Results is optional and only needed if Type is SAPI_DBTYPES_SCRIPT
or SAPI_DBTYPES_CONQUEST
*/
PSAPI_DBASE dbase_CreateInit(PCHR Name, INT Type, PCHR Location, PCHR Results)
{
  PSAPI_DBASE d;

  /* Sanity check */
  if((Name==NULL)||(Name[0]=='\0')) {
    fprintf(stderr, "dbase_CreateInit:Null Name\n");
    return NULL_DBASE;
  }
  if((Location==NULL)||(Location[0]=='\0')) {
    fprintf(stderr, "dbase_CreateInit:Null Location\n");
    return NULL_DBASE;
  }
  if((Type!=SAPI_DBTYPE_ISEARCH)&&
     (Type!=SAPI_DBTYPE_SCRIPT)&&
     (Type!=SAPI_DBTYPE_CONQUEST)&&
     (Type!=SAPI_DBTYPE_CARL)&&
     (Type!=SAPI_DBTYPE_PGIF)) {
    fprintf(stderr, "dbase_CreateInit:Bad database type\n");
    return NULL_DBASE;
  }
  if((Type==SAPI_DBTYPE_SCRIPT)||(Type==SAPI_DBTYPE_CONQUEST)) {
    if((Results==NULL)||(Results[0]=='\0')) {
      fprintf(stderr, "dbase_CreateInit:Null Results\n");
      return NULL_DBASE;
    }
  }

  if((d=dbase_Create())==NULL)
    return NULL_DBASE;

  if((d->Name=NEWSTRING(strlen(Name)+1))==NULL) {
    fprintf(stderr, "dbase_CreateInit:Out of memory\n");
    dbase_Destroy(d);
    return NULL_DBASE;
  }
  strcpy(d->Name, Name);

  if((d->Location=NEWSTRING(strlen(Location)+1))==NULL) {
    fprintf(stderr, "dbase_CreateInit:Out of memory\n");
    dbase_Destroy(d);
    return NULL_DBASE;
  }
  strcpy(d->Location, Location);

  d->Type = Type;

  if((d->Type == SAPI_DBTYPE_SCRIPT)||(d->Type==SAPI_DBTYPE_CONQUEST)) {
    if((d->Results=NEWSTRING(strlen(Results)+1))==NULL) {
      fprintf(stderr, "dbase_CreateInit:Out of memory\n");
      dbase_Destroy(d);
      return NULL_DBASE;
    }
    strcpy(d->Results, Results);
  }

  return d;
}

void dbase_Destroy(PSAPI_DBASE d)
{
  if(d==NULL_DBASE)
    return;

  if(d->Name!=NULL)
    FREE(d->Name);

  if(d->Location!=NULL)
    FREE(d->Location);
	
  if(d->Results!=NULL)
    FREE(d->Results);
#ifdef SAPI_CONQUEST
  if(d->Type == SAPI_DBTYPE_CONQUEST) {
    cqhl_stop_query(d->QueryThread);
    cqhl_close_query(d->QueryThread);
    cqhl_close_query_thread(d->QueryThread);
    cqhl_close_conquest();
  }
#endif
#ifdef SAPI_PGIF
  pgif_destroy();
#endif
#ifdef SAPI_CARL
  delete d->CarlDriver;
#endif
  FREE(d);
}


/* 
Group is the database name (and the actual group name, of course)
*/
PSAPI_DBASE dbase_CreateInitFile(PCHR File, PCHR Group)
{
  PSAPI_DBASE d;
  CHR Location[MAXPROFILESTR],TempType[MAXPROFILESTR],
  Results[MAXPROFILESTR];
  INT Type;

  Results[0]='\0';

  /* Sanity check */
  if((File==NULL)||(File[0]=='\0'))
    return NULL;

  if((Group==NULL) || (Group[0]=='\0'))
    return NULL;
	
  /* Read the Name, Location and Type from the Group */
  GetPrivateProfileString(Group, "Location", "", Location,
			  sizeof(Location),File);
  if(Location[0]=='\0') {
    fprintf(stderr,"dbase_CreateInitFile:No Location for [%s]\n",
	    Group);
    return NULL;
  }
  GetPrivateProfileString(Group, "Type", "", TempType,sizeof(TempType),File);
  if(TempType[0]=='\0') {
    fprintf(stderr,"dbase_CreateInitFile:No Type for [%s]\n",
	    Group);
    return NULL;
  }

  if(!(strcasecmp(TempType, "ISEARCH")))
    Type=SAPI_DBTYPE_ISEARCH;
  if(!(strcasecmp(TempType, "SCRIPT")))
    Type=SAPI_DBTYPE_SCRIPT;
  if(!(strcasecmp(TempType, "CONQUEST")))
    Type=SAPI_DBTYPE_CONQUEST;
  if(!(strcasecmp(TempType, "PGIF")))
    Type=SAPI_DBTYPE_PGIF;
  if(!(strcasecmp(TempType, "CARL")))
    Type=SAPI_DBTYPE_CARL;

  /* Defaults to ISEARCH */
  if((Type == SAPI_DBTYPE_SCRIPT)||(Type==SAPI_DBTYPE_CONQUEST)) {
    GetPrivateProfileString(Group, "Results", "", Results,
			    sizeof(Results), File);
    if(Results[0]=='\0') {
      fprintf(stderr,
	      "dbase_CreateInitFile:No Results for [%s]\n",
	      Group);
      return NULL;
    }
  }

  /* Build the database entry */
  if((d=dbase_CreateInit(Group,Type,Location,Results))==NULL)
    return NULL;

  return d;
}

//
// Only valid for Isearch dbs
//
void dbase_GetFieldNames(PSAPI_DBASE db, STRLIST *FieldNames)
{
  FieldNames->Clear();
  if(db->Type != SAPI_DBTYPE_ISEARCH)
    return;

  DFDT dfdt;
  DFD dfd;
  db->pdb->GetDfdt(&dfdt);
  INT count, i;
  STRING FieldName;
  count = dfdt.GetTotalEntries();
  for(i=1;i <= count;i++) {
    dfdt.GetEntry(i, &dfd);
    dfd.GetFieldName(&FieldName);
    FieldNames->AddEntry(FieldName);
  }
}

INT dbase_GetType(PSAPI_DBASE db)
{
  return db->Type;
}

PSAPI_RESULTSET ResultSet_Create()
{
  PSAPI_RESULTSET r;

  if((r=NEWSTRUCT(SAPI_RESULTSET))==NULL) {
    fprintf(stderr, "ResultSet_Create:Out of memory\n");
    return NULL;
  }

  r->Name = NULL;
  r->HitCount = 0;
  r->Diagnostic = 0;

  return r;
}

void ResultSet_Destroy(PSAPI_RESULTSET r)
{
  CHR TempFile[512];

  if(r==NULL)
    return;

  if(r->Name!=NULL)
    FREE(r->Name);

  if(r->Database!=NULL) {
    if(r->Database->Type==SAPI_DBTYPE_SCRIPT) {
      sprintf(TempFile, "%s.%ld", r->Database->Results, 
	      getpid());
#ifdef SAPI_DEBUG
      fprintf(stderr, "ResultSet_Destroy:Unlinking %s\n", 
	      TempFile);
#endif
      unlink(TempFile);
    }
  }
}

void ResultSet_SetDatabase(PSAPI_RESULTSET r, PSAPI_DBASE d)
{
  if(r==NULL)
    return;

  r->Database = d;
}

PCHR dbase_GetName(PSAPI_DBASE d)
{
  if((d != (PSAPI_DBASE)NULL) && (d->Name != (PCHR)NULL))
    return d->Name;
  return NULL;
}

void ResultSet_SetHitCount(PSAPI_RESULTSET r, INT4 h)
{
  if(r==NULL)
    return;
  
  r->HitCount = h;
}

INT4 ResultSet_GetHitCount(PSAPI_RESULTSET r)
{
  if(r==NULL)
    return -1;

  return r->HitCount;
}

#define USMARC_OID "1.2.840.10003.5.10"
#define SUTRS_OID "1.2.840.10003.5.101"

/*
Callers, don't forget to free the buffer!
*/
PCHR ResultSet_GetRecord(PSAPI_RESULTSET RS, INT4 RecordNum, INT4 MaxLen, 
	INT4 *ActualLen, PCHR ElementSet, PCHR RecordSyntax,
	STRING *ActualRecordSyntax)
{
  PCHR Record;

  *ActualRecordSyntax = SUTRS_OID;

  if(RS==NULL)
    return NULL;

  Record = NULL;

  switch(RS->Database->Type) {
  case SAPI_DBTYPE_ISEARCH: {
    RESULT RsRecord;
    STRING *Buffer;
    STRING ElementSetName;
    
    ElementSetName = ElementSet;
    RS->prset->GetEntry(RecordNum, &RsRecord);
    
    Buffer = new STRING;
    
    RS->Database->pdb->Present(RsRecord, ElementSetName, 
			       RecordSyntax, Buffer);
    
    *ActualLen = Buffer->GetLength();
    Record = Buffer->NewCString();
    delete Buffer;
    break;
  }
#ifdef SAPI_CGI
  case SAPI_DBTYPE_CGI:
    fprintf(stderr, "ResultSet_GetRecord:No CGI yet\n");
    break;
#endif
  case SAPI_DBTYPE_SCRIPT:
    return script_GetRecord(RS->Database->Results, 
			    RecordNum, MaxLen, ActualLen, ElementSet);
    break;
#ifdef SAPI_CONQUEST
  case SAPI_DBTYPE_CONQUEST: {
    return conquest_GetRecord(RS, RecordNum, MaxLen, 
			      ActualLen, ElementSet, RecordSyntax);
    break;
  }
#endif
#ifdef SAPI_PGIF
  case SAPI_DBTYPE_PGIF: {
    return pgif_GetRecord(RecordNum,ElementSet,
			  RecordSyntax,ActualLen);
    break;
  }
#endif
#ifdef SAPI_CARL
  case SAPI_DBTYPE_CARL: {
    STRING Rec;
    if(RS->Database->CarlDriver->GetRecord(
					   RecordNum, &Rec)) {
      Record = Rec.NewCString();
      *ActualRecordSyntax = USMARC_OID;
    } else
      return NULL;
    break;
  }
#endif
    
  default:
    fprintf(stderr, "ResultSet_GetRecord:bad DB type\n");
    break;
    
  }
  return Record;
}

void sapi_Error(PCHR Function, INT Code, PCHR Msg)
{
  CHR errmsg[80];

  switch(Code) {
  case 0:
    strcpy(errmsg, "Out of memory");
    break;
  case 1:
    strcpy(errmsg, "Bad sapi.ini file");
    break;
  case 2:
    strcpy(errmsg, "No databases in DBList");
    break;
  default:
    strcpy(errmsg, "Unknown error");
    break;
  }

  if((Msg==NULL)||(Msg[0]=='\0'))
    fprintf(stderr, "sapi:%s():%s\n", Function, errmsg);
  else
    fprintf(stderr, "sapi:%s():%s[%s]\n", Function,
	    errmsg,Msg);
}

PSAPI_QUERY query_Create()
{
  PSAPI_QUERY q;
  
  if((q=NEWSTRUCT(SAPI_QUERY))==NULL) {
    fprintf(stderr, "query_Create:Out of memory\n");
    return NULL;
  }

  q->Stack = (STACK *)NULL;
  q->Term = (PCHR)NULL;
  q->squery = NULL;	
  
  return q;
}

PSAPI_QUERY query_CreateSimple(PCHR s)
{
  PSAPI_QUERY q;

  if((s == (PCHR)NULL) || (s[0] == '\0')) {
    fprintf(stderr, "query_CreateSimple: NULL term\n");
    return NULL_QUERY;
  }
	
  if((q = query_Create()) == NULL_QUERY)
    return NULL_QUERY;

  if((q->Term = NEWSTRING(strlen(s)+1)) == (PCHR)NULL) {
    query_Destroy(q);
    return NULL_QUERY;
  }

  strcpy(q->Term, s);

  return q;
}

PSAPI_QUERY query_CreateIsearch(SQUERY & Query)
{
  PSAPI_QUERY q;

  if((q=query_Create())==NULL)
    return NULL;

  q->squery = new SQUERY;
  *q->squery = Query;
	
  return q;
}

PSAPI_QUERY query_CreateStack(STACK *S)
{
  PSAPI_QUERY q;

  if((q=query_Create())==NULL)
    return NULL;

  q->Stack = S;

  return q;
}

void query_Destroy(PSAPI_QUERY q)
{
  if(!q)
    return;

  if(q->Term)
    FREE(q->Term);

  if(q->squery)
    delete q->squery;	

  /* Free the query stack (when you have time ;-)
     if(q->Stack!=NULL)
     */
}
