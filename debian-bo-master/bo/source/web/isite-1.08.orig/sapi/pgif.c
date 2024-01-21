/* PGIF - Postgres Interface for CNIDR's ISITE package.  For more information
   see "README" in this directory. */

/* Copyright 1995 Clearinghouse for Networked Information Discovery and   
   Retrieval, all rights reserved.  Copyright assigned to CNIDR by Scott
   Technologies, Inc., the developer of PGIF.  Scott Technologies
   provides services for integration of databases and free text search
   capabilities, as well as development of network and end-user interfaces.

   Scott Technologies, Inc.
   +1 919 732 7930
   1406 US 70 E 
   Hillsborough NC, 27278

*/


#include <stdio.h>
#include "libpq.h"


/* This is the name of a table that will hold the result sets */
char sessionRSETname[80];  

void pgif_init(char *DBName)
{

fprintf(stderr,"In pgif_init().\n");
fprintf(stderr,"Going to open %s for postgres.\n",DBName);
sprintf(sessionRSETname,"%s%d",DBName,getpid());
fprintf(stderr,"sessionRSETname = %s \n",sessionRSETname);
PQsetdb (DBName);

}


/* Be Warned: ZDist 1.02 will not cause this to be called, so you'll
have to clean out the old result sets periodically. */

void pgif_destroy()
{
char cmd[80];

fprintf(stderr,"In pgif_destroy().\n");

sprintf(cmd,"destroy %s",sessionRSETname);
PQexec(cmd);

}



/* ParseAttributes compliments of KAG. */
/*
PRE:	S = term[attribute type, attribute value, ...]
POST:	The integer pointers passed in will be set according to the
	attribute/value pairs.
EXAMPLE:S = "mousetrap[1,2145,2,3]"
	*Use = 2145, *Relation = 3
*/	
void ParseAttributes(char *S, int *Use, int *Relation, int *Position, 
	int *Structure, int *Truncation, int *Completeness)
{
	typedef enum tagSTATE {sNone, sUse, sRelation, sPosition, sStructure,
		sTruncation, sCompleteness} STATE;
	int Len;
	char *p,*q;
	int AttrValue=1; /* 1 == Attribute, 0 == Value */
	STATE State=sNone;

	/* If you get all 0's back, you know something went wrong */
	*Use = 0;
	*Relation = 0;
	*Position = 0;
	*Structure = 0;
	*Truncation = 0;
	*Completeness = 0;

	/* S must have data and must include an opening and closing bracket */
	if(!S || S[0]=='\0' || !(q=strchr(S, '[')) || !strchr(S, ']'))
		return;
			
	Len = strlen(S);
	if(!(p=strtok(q+1, ",")))
		return;

	do {
		int Temp;
		Temp = atoi(p);
		switch(State) {
			case sNone:
				switch(Temp) {
					case 1:
						State = sUse;
						break;
					case 2: 
						State = sRelation;
						break;
					case 3:
						State = sPosition;
						break;
					case 4:
						State = sStructure;
						break;
					case 5:
						State = sTruncation;
						break;
					case 6:
						State = sCompleteness;
						break;
					default:
						State = sUse;
				}						
				break;
			case sUse:
				*Use = Temp;
				State = sNone;
				break;
			case sRelation:
				*Relation = Temp;
				State = sNone;
				break;
			case sPosition:
				*Position = Temp;
				State = sNone;
				break;
			case sStructure:
				*Structure = Temp;
				State = sNone;
				break;
			case sTruncation:
				*Truncation = Temp;
				State = sNone;
				break;
			case sCompleteness:
				*Completeness = Temp;
				State = sNone;
				break;
		}
		/* Toggle */
		if(AttrValue==0)
			AttrValue = 1;
		else AttrValue = 0;
	} while(p=strtok(NULL, ","));
}

char *QueryDatatoPostgresQuery(char *term, int use, int trunc, int strct, int rel, char *query)
{

char newterm[80], *where;

if ((where = strstr(term,"[")) != NULL) {
	strncpy(newterm,term, where-term);
	newterm[where-term]='\0';
	}
else {
	strcpy(newterm,term);
	}
fprintf(stderr,"newterm = %s\n", newterm);


if ((use==4) || (use==2145) || (use==0) ) {
	sprintf(query,"retrieve into %s (a.all) from a in headlines where a.title ~ \"%s*\" ", sessionRSETname, newterm);
	}

fprintf(stderr,"postquel query is: %s\n", query);
return query;
}

int
DoPostgresSearch(char *term,int use,int rel,int pos,int strct,int trunc,int compl)
{

int i,j,k,g,n,m,t,numhits;
PortalBuffer *p;
char pnames[PORTALS_INITIAL_SIZE][portal_name_length];
char querystring[8192]; /* max postgres query str size */
char pnumber[256];
 

PQexec ("begin");
sprintf(querystring,"destroy %s", sessionRSETname); /* delete the old Rset */
PQexec(querystring);
PQexec("end");

PQexec("begin");
QueryDatatoPostgresQuery(term,use,trunc,strct,rel,querystring);
PQexec(querystring);
PQexec("end");

PQexec("begin");
sprintf(querystring,"retrieve portal eportal (hitcount = count{%s.title})", sessionRSETname);
PQexec(querystring);

PQexec ("fetch all in eportal");
p=PQparray ("eportal");
g=PQngroups (p);
 
t=0;
for (k=0; k<g; k++) {
   n=PQntuplesGroup (p,k);
   m=PQnfieldsGroup (p,k);
 
   for (i=0; i< m; i++)
      printf("%-15s",PQfnameGroup (p,k,i));
   printf("\n");
 
   for (i=0; i< n; i++) {
      for (j=0; j< m; j++) {
         strcpy(pnumber,PQgetvalue (p,t+i, j));
         }
      }
   t+=n;
   }
PQexec("close eportal");
PQexec("end");  /* end the transaction, but not the session */
sscanf(pnumber,"%d",&numhits);
return numhits;
}

int pgif_search(char *Term)
{
int Use, Rel, Pos, Strct, Trunc, Compl;
int numhits;

fprintf(stderr,"In pgif_search(). \n");
fprintf(stderr,"going to search for %s\n",Term);

ParseAttributes(Term, &Use, &Rel, &Pos, &Strct, &Trunc, &Compl);

fprintf(stderr,"Term  %s\n",Term);
fprintf(stderr,"Use   %d\n",Use);
fprintf(stderr,"Rel   %d\n",Rel);
fprintf(stderr,"Pos   %d\n",Pos);
fprintf(stderr,"Strct %d\n",Strct);
fprintf(stderr,"Trunc %d\n",Trunc);
fprintf(stderr,"Compl %d\n",Compl);

numhits = DoPostgresSearch(Term, Use, Rel, Pos, Strct, Trunc, Compl);
return numhits;

}


/* Flaming assumption of doom: the first record is record # 1. */
/* You were warned. */
/* More warnings:  This routine is maximally cheezy.  On each record present,
   we (on average) will traverse half the result set, pulling it slowly
   and without pain killers from Postgres.  This is absolutely heinous.
   If you do this with a "real" DBMS, use SQL cursors so you only pass the
   parts of the result set that you're interested in.

   We ignore Element Set (basically, we only implement "F") and we ignore 
   RecordSyntax (we return SUTRS).  These are simple, but largely data-
   dependent, hacks.
*/

char * pgif_GetRecord(int RecordNum, char *ElementSet,
                    char *RecordSyntax, int *ActualLen)
{

char *retbuff;
int i,j,k,g,n,m,t;
PortalBuffer *p;
char pnames[PORTALS_INITIAL_SIZE][portal_name_length];
char querystring[8192]; /* max postgres query str size */
char pnumber[256];

fprintf(stderr,"In pgif_GetRecord().\n");
/*
strcpy(retbuff,"This is a string, eh?\n");
*ActualLen = strlen(retbuff);
*/

retbuff=(char *)malloc(1024);
PQexec("begin");
sprintf(querystring,"retrieve portal eportal (%s.all)", sessionRSETname);
PQexec(querystring);

PQexec ("fetch all in eportal");
p=PQparray ("eportal");
g=PQngroups (p);
 
t=0;
for (k=0; k<g; k++) {
   n=PQntuplesGroup (p,k);
   m=PQnfieldsGroup (p,k);

   if (RecordNum > n) {
	fprintf(stderr,"pgif_getRecord: Record Num = %d, num of tuples returned = %d. Error.\n");
	return NULL;
	}
 
/*   for (i=0; i< m; i++)
      printf("%-15s",PQfnameGroup (p,k,i));
   printf("\n"); 
*/

RecordNum--; /* switch to zero-based address from one-based. */

   for (i=0; i< RecordNum; i++) {  /* burn off excess records */
      for (j=0; j< m; j++) {
         strcpy(pnumber,PQgetvalue (p,t+i, j));
         }
      }
   /* Now wwe're ready to read RecordNum: */
   if (i!=RecordNum) {
	fprintf(stderr,"grave error after burning off slack. %d\n",i);
	return NULL;
	}
   strcpy(retbuff,"");
   for (j=0; j< m; j++) {
      strcat(retbuff,PQgetvalue (p,t+i, j));
      strcat(retbuff, "\n");
      }
   strcat(retbuff,"\n\n");

   t+=n;
   }
PQexec("close eportal");
PQexec("end");  /* end the transaction, but not the session */

*ActualLen = strlen(retbuff);
return retbuff;

}



