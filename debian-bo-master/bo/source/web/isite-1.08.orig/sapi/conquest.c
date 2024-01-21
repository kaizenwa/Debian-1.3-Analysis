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
File:           conquest.c
Version:        1.00
Description:    SAPI driver for Conquest Software, Inc. database system
Author:         Kevin Gamiel, kgamiel@cnidr.org
@@@*/

#include "gdt.h"
#include "libz3950.h"
#include <string.h>
#include <ctype.h>
#define UNIX_VERSION
#ifndef UNIX_VERSION
#include <io.h>
#endif
#include "tsrbasic.h"
#include "hl_api.h"
#define EXTFLG

#include "list.h"
#include "sapi.h"

void error_reporter_func( void *user_ptr );
void display_error( void );

#include "conquest.h"

PSAPI_RESULTSET conquest_Search(PSAPI s, PSAPI_DBASE Database, STACK *Query)
{
	PSAPI_RESULTSET r;
	PCHR Term;
	PQUERYATOM Atom = (PQUERYATOM)stack_Top(Query);
	INT i;
	enum query_state_enum query_state;
	long unsigned int DocCount;
	INT2 fieldCount,j;
	CHR progress_string[MAX_STRING];	
	CHR temp[200];

	if((s==NULL)||(Database==NULL)||(Query==NULL)) {
		fprintf(stderr, "sapi_Search:Bad input to call\n");
		return NULL;
	}

	if((r=ResultSet_Create())==NULL)
		return NULL;

	ResultSet_SetDatabase(r,Database);

#ifdef SAPI_DEBUG
	fprintf(stderr, "conquest_Search()\n");
	fprintf(stderr, "\tDatabase Name:%s\n", Database->Name);
	fprintf(stderr, "\tLocation:%s\n", Database->Location);
	fprintf(stderr, "\tType:%i\n", Database->Type);
	fprintf(stderr, "\tResults:%s\n", Database->Results);
#endif
	if(Database->Type!=SAPI_DBTYPE_CONQUEST) {
		fprintf(stderr, "Incorrect DB type\n");
		return NULL;
	}
		
	if(Atom == NULL) {
		fprintf(stderr, "sapi_Search:Null query\n");
		return NULL;
	}
	Term=NEWSTRING(STRLEN(Atom->Data)+1);
	STRCPY(Term, Atom->Data);
	cqhl_open_conquest(Database->Location, 
		error_reporter_func);
	cqhl_open_query_thread(&Database->QueryThread, NULL);	
	if(Database->QueryThread == NULL) {
		fprintf(stderr, "Unable to open thread\n");
		return NULL;
	}
	cqhl_set_library(Database->QueryThread, Database->Name);
	cqhl_open_query(Database->QueryThread, OPEN_FOR_READ);
			
	/* Query */ 
	for(i=0;i<strlen(Term);i++)
		toupper(Term[i]);			
#ifdef SAPI_DEBUG
	fprintf(stderr, "\tQuery Term=%s\n", Term);
#endif
	cqhl_set_query_string(Database->QueryThread, 
		Term, DOC_BODY);
	cqhl_start_query(Database->QueryThread);
	query_state = START;
	while (query_state != COMPLETE) {
		cqhl_execute_query(Database->QueryThread, 
			&query_state, progress_string);
	}

	/* Get results */
	cqhl_get_doc_count(Database->QueryThread, &DocCount);
	r->HitCount = (INT4)DocCount;

/* This code gets fields available for the specified library 
	cqhl_get_field_count(Database->QueryThread,Database->Name, &fieldCount);
	for(j=1;j<=fieldCount;j++) {
		char fname[100];
		INT2 fsize,fflags;
		UCHR ftype,ffield_id;

		cqhl_get_field_info(Database->QueryThread, Database->Name, 
			j, fname, &fsize,&fflags, &ftype, &ffield_id);
		printf("%s\n", fname);
	}
*/
	return r;
}

/*
Callers, don't forget to free the buffer!
*/
PCHR conquest_GetRecord(PSAPI_RESULTSET RS, INT4 RecordNum, INT4 MaxLen, 
	PINT4 ActualLen, PCHR ElementSet, PCHR RecordSyntax)
{
	PCHR Record;
	FILE *fp;
	CHR file[1024];
	CHR RankInfo[40];
	CHR DocTitle[200];
	UINT4 t=0,total=0;
	long unsigned int libid, docid, numhits;
	UCHR coarserank, finerank,maxhitrank, avghitrank, usercomprank;

	if(RS==NULL)
		return NULL;

	Record = NULL;

	if(RS->Database->Type!=SAPI_DBTYPE_CONQUEST) {
		fprintf(stderr, "Incorrect DB type\n");
		return NULL;
	}

	Record = NEWSTRING(MaxLen);	

	/* Get some ranking information */
	cqhl_get_doc_stats(RS->Database->QueryThread, (INT4)RecordNum,
		&libid, &docid, &numhits, &coarserank, &finerank,&maxhitrank,
		&avghitrank, &usercomprank);
	sprintf(RankInfo, "%i. (%2i) ", RecordNum, coarserank);

	/* Get the TITLE of the document */
	cqhl_get_doc_field_value( RS->Database->QueryThread, (INT4)RecordNum, 
		"TITLE", DocTitle);

	if(!strcasecmp(ElementSet,"B")) {
		CHR URLQuery[200];
#ifdef SAPI_DEBUG
	fprintf(stderr, "rankinfo=%s\n", RankInfo);
#endif
		strcpy(URLQuery, DocTitle);
		if(!strcasecmp(RecordSyntax,HTML_OID)) 	{
			strbox_EscapeSpaces(URLQuery);
			sprintf(Record, "<b>%s</b> <i><a href=\"http://vinca.cnidr.org/htbin/conquestgw?resume_lib+%s+F\">%s</a></i><p>", RankInfo,
				URLQuery,DocTitle);
		} else
			sprintf(Record, "%s - %s\n", RankInfo, DocTitle);
		*ActualLen=strlen(Record);
	} else {
		/* Crack the data file and grab the whole thing */
		cqhl_get_doc_field_value(
			RS->Database->QueryThread,
			(INT4)RecordNum, "FILE_NAME",
			Record);
		sprintf(file,"%s/%s",RS->Database->Results,
			Record);
		fp=fopen(file,"r");
		if(!fp) {
			perror("sapi_GetRecord");
			return NULL;
		}
		Record[0]='\0';
		if(!strcasecmp(RecordSyntax,HTML_OID)) {
			sprintf(Record,"<header><title>%s</title></header><body><pre>\n",DocTitle);
		}
		total = strlen(Record);
#ifdef SAPI_DEBUG
		fprintf(stderr, "Record=%s\n", Record);	
		fprintf(stderr, "len=%i\n", total);	
#endif
		while((t=fread(Record+total,1,MaxLen,fp))>0) {
			total+=t;
		}
		Record[total]='\0';
#ifdef SAPI_DEBUG
		fprintf(stderr, "Record=%s\n", Record);	
		fprintf(stderr, "len=%i\n", total);	
#endif
		if(!strcasecmp(RecordSyntax,HTML_OID)) 	
			strcat(Record,"</pre></body>\n");
		fclose(fp);
				
		*ActualLen=strlen(Record);
	}
#ifdef SAPI_DEBUG
	fprintf(stderr, "rankinfo=%s\n", RankInfo);
#endif

	return Record;
}

/***********************/
/* error_reporter_func */
/***********************/
void error_reporter_func( void *user_ptr )
{
  struct error_str error_desc_str;
 
  user_ptr = user_ptr;   /* So lint does not complain. */
 
  cqhl_get_error_description( &error_desc_str );
 
  if (strnequ(error_desc_str.source, "USER", 4)) return;
 
  display_error();
 
  if ( (strnequ(error_desc_str.severity, "NULL",    4)) ||
       (strnequ(error_desc_str.severity, "WARNING", 7)) ||
       (strnequ(error_desc_str.severity, "PARSE", 5)) )
    return;
  else
    exit(5);
}
 
 
/*****************/
/* display_error */
/*****************/
void
display_error( void )
{
  struct error_str error_desc_str;
  char out_line[ 100 ];
 
  cqhl_get_error_description( &error_desc_str );
 
  sprintf( out_line, "\nError: %d, Severity: %s, Source: %s",
                     error_desc_str.number,
                     error_desc_str.severity,
                     error_desc_str.source );
  puts( out_line );
  puts( error_desc_str.msg1 );
  puts( error_desc_str.msg2 );
  puts( error_desc_str.msg3 );
  puts( error_desc_str.msg4 );
 
  /* If stdout is being re-directed to a file, then send message to stderr. */
  if ( !isatty( fileno(stdout) ) )
  {
    fputs( out_line, stderr );               fputc( '\n', stderr );
    fputs( error_desc_str.msg1, stderr );    fputc( '\n', stderr );
    fputs( error_desc_str.msg2, stderr );    fputc( '\n', stderr );
    fputs( error_desc_str.msg3, stderr );    fputc( '\n', stderr );
    fputs( error_desc_str.msg4, stderr );    fputc( '\n', stderr );
  }
}
 
