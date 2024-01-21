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
File:           script.c
Version:        1.00
Description:    Simple SCRIPT-type database driver
Author:         Kevin Gamiel, kgamiel@cnidr.org
@@@*/

#include <stdio.h>
#include <string.h>
#include <ctype.h>

#if defined(_MSDOS) || defined(_WIN32)
#include <process.h>
#else
#include <unistd.h>
#endif

#include "config.h"
#include "gdt.h"
#include "ini.h"
#include "strbox.h"
#include "script.h"

#define MAXSEPARATOR 80

#ifndef NEWSTRING
#define NEWSTRING(a) (PCHR)calloc(1,a)
#endif

PCHR script_GetRecord(PCHR ResultsFile, INT4 n, INT4 MaxLen, INT4 *l, PCHR es)
{
	FILE *fp;
	CHR buf[81], Separator[MAXSEPARATOR], TempFile[512];
	INT4 i;
	INT Status = 0;
	PCHR Record;
	INT4 len;

	sprintf(TempFile, "%s.%ld", ResultsFile, getpid());
	fp=fopen(TempFile,"r");
	if(fp==NULL) {
		fprintf(stderr, "script_GetRecord:Can't open %s\n",
			TempFile);
		return NULL;
	}

	fseek(fp, 0, 2);
	len = ftell(fp);
	rewind(fp);
	
	if((Record=NEWSTRING(len+1))==NULL) {
		fprintf(stderr, "script_GetRecord:Out of memory\n");
		return NULL;
	}	

	/* Get the separator string */
	GetPrivateProfileString("Default", "Separator", "", Separator, 
		sizeof(Separator), TempFile);
	if(Separator[0]=='\0') {
		fprintf(stderr,
		"script_GetRecord:Bad Separator[%s]\n", TempFile);
		return NULL;
	}
	/* Scan down past the [Data] group */
	while(fgets(buf, 80, fp)!=NULL) {
		if(strncmp(buf, "[Data]",6)!=0)
			continue;
		break;
	}
	/* Scan to the beginning of the record requested */
	for(i=1;i<n;i++) {
		while(fgets(buf, 80, fp)!=NULL) {
			if(!strncmp(buf,Separator,
				strlen(Separator))!=0) {
				Status = 1;
				break;
			}
		}
	}
	if(n==1)
		Status = 1;
	if(Status == 0) {
		fprintf(stderr, "script_GetRec:No record[%s,%d]\n",
			TempFile,n);
		return NULL;
	}

	/* Read till the next separator or EOF */
	while(fgets(buf, 80, fp)!=NULL) {
		if(!strncmp(buf,Separator,strlen(Separator))!=0)
			break;
if(!strcmp(es, "B")) {
	int done = 0;
	for(i=0;i < strlen(buf);i++) {
		if(isalpha(buf[i])) {
			strbox_chop(buf);
			strcat(Record, buf);
			done = 1;
			break;
		}
	}
	if (done == 1)
		break;
} else strcat(Record, buf);
		if((strlen(Record)+80) > MaxLen)
			/* truncate the record */
			break;
	}
	
	/* Tell caller how big record is */
	*l = strlen(Record);

	fclose(fp);

	return Record;
}

INT4 script_Search(PCHR Script, PCHR ResultsFile, PCHR Query, PINT Diag)
{
	CHR cmd[1024];
	PCHR TempFile;

	if((Script == (PCHR)NULL) || (Script[0] == '\0')) {
		fprintf(stderr, "script_Search: NULL Script value\n");
		return -1;
	}

	if((ResultsFile == (PCHR)NULL) || (ResultsFile[0] == '\0')) {
		fprintf(stderr, "script_Search: NULL ResultsFile value\n");
		return -1;
	}
	if((Query == (PCHR)NULL) || (Query[0] == '\0')) {
		fprintf(stderr, "script_Search: NULL Query value\n");
		return -1;
	}

	/* 
	Call the command line script
	*/
 	TempFile = NEWSTRING(strlen(ResultsFile)+20);
	sprintf(TempFile, "%s.%ld", ResultsFile,getpid());
	sprintf(cmd, "%s %s %s", Script, TempFile, Query);

#ifdef SAPI_DEBUG
	fprintf(stderr, "\tTempFile:%s\n", TempFile);
	fprintf(stderr, "\tQuery:%s\n", Query);
	fprintf(stderr, "\tCmd:%s\n", cmd);
#endif
	system(cmd);
	
	/* Now use SCRIPT API to read results */
	*Diag = GetPrivateProfileInt("Default", "Diagnostic", 1,  TempFile);

	return GetPrivateProfileInt("Default", "HitCount", 0,  TempFile);
}
