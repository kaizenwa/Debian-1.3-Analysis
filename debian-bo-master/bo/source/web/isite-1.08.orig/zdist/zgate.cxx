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

#include <iostream.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>
#include <stdlib.h>
#include <signal.h>
#include <unistd.h>
#include <locale.h>

#include "config.hxx"
#include "ztags.hxx"
#include <string.h>
#include "string.hxx"
#include "strlist.hxx"
#include "gdt.h"
#include "cgi.h"

#define DEFAULT_TIMEOUT 180

void USR1Handler(int t);
void ZgateAlarmHandler(int t);
void ZgateStopHandler(int t);
void DisplaySearchForm(CHR *Filename, INT SessionId);
void MyExit(INT Status);

volatile GDT_BOOLEAN WaitingForSignal;
char *TempFile;

CHR *Action;
CHR *Host, *Tmp;
UINT Port;
CHR *DBName, *DBInfo;
STRLIST List;
INT PID;
enum { FROM_COMMANDLINE, FROM_CGI } Source;
STRING TempString;
struct stat statbuf;

int main(int argc, char **argv)
{
	CGI *cgi;
	int i;
	int max_open_files;
	CHR mypath[1024];

	if (!setlocale(LC_CTYPE,"")) {
	  cout << "Warning: Failed to set the locale!" << endl;
	}

	getcwd(mypath, sizeof(mypath));
	ios::sync_with_stdio();

	if ((max_open_files = (int)sysconf(_SC_OPEN_MAX)) < 0) {
		cout << "I can't determine the maximum number of open files!";
		cout << endl;
		MyExit(1);
	}

	for (i = 3; i < max_open_files; i++)
		close(i);

        // Install an interrupt handler for an alarm signal
        signal(SIGALRM, ZgateAlarmHandler);
        alarm(DEFAULT_TIMEOUT);

        signal(SIGINT, ZgateStopHandler);
	signal(SIGUSR1, USR1Handler);

	cout << "Content-type: text/html" << endl << endl;
//	cout << "<A HREF=\"http://www.cnidr.org/\">CNIDR</A> http to Z39.50 gateway, Version " << ZDIST_VERSION << endl;

	if(argc > 1) {
		// Read from command-line instead of CGI
		TempString = argv[1];
		Action = TempString.NewCString();
		Source = FROM_COMMANDLINE;
	} else {
		Source = FROM_CGI;
		cgi = cgi_Create();
		cgi_GetInput(cgi);
		//cgi_DisplayContents(cgi);

		if((Action = cgi_GetValueByName(cgi, "ACTION")) == NULL) {
			cout << "Your form didn't include an ACTION!" << endl;
			MyExit(1);
		}
	}

	if(!strcasecmp(Action, "INIT")) {
		if((Tmp = cgi_GetValueByName(cgi, "FORM_HOST_PORT")) == NULL) {
			cout << "Your form didn't include a FORM_HOST_PORT!";
			cout << endl;
			MyExit(1);
		}
		STRING TempString;
		TempString = Tmp;
		List.Split(",", TempString);
		List.GetEntry(1, &TempString);
		DBInfo = TempString.NewCString();
		// If this file doesn't exist, die now
		if(stat(DBInfo, &statbuf)== -1) {
			cout << "Your gateway doesn't include a search form ";
			cout << "named <B>" << DBInfo << "</B>.  Please ";
			cout << "read the Isite documentation for more ";
			cout << "<A HREF=\"http://vinca.cnidr.org/software/Isite";
			cout << "/Isite.html\">information</A>." << endl;
			MyExit(1);
		}

		List.GetEntry(2, &TempString);
		Host = TempString.NewCString();
		List.GetEntry(3, &TempString);
		Port = TempString.GetInt();

		//	
		// Do we have a groupid/userid/password?
		//	
		CHR *GroupId=NULL, *UserId=NULL, *Password=NULL;
		TempString = "";
		GroupId = cgi_GetValueByName(cgi, "GROUPID");
		UserId = cgi_GetValueByName(cgi, "USERID");
		Password = cgi_GetValueByName(cgi, "PASSWORD");

		if(GroupId) {
			TempString.Cat(" -gid ");
			TempString.Cat(GroupId);
		}
		if(UserId) {
			TempString.Cat(" -uid ");
			TempString.Cat(UserId);
		}
		if(Password) {
			TempString.Cat(" -pass ");
			TempString.Cat(Password);
		}
	
		char cmd[1024];
		TempFile = tempnam("/tmp", "zgate");
		if(TempString.GetLength() > 0) {
			CHR *GroupUserPass;
			GroupUserPass = TempString.NewCString();
			sprintf(cmd, "%s/zcon %s %ld %s %i %s &",mypath,
				TempFile, getpid(),Host, Port, GroupUserPass);
		} else
			sprintf(cmd, "%s/zcon %s %ld %s %i &",mypath,TempFile,
				getpid(),Host, Port);
		INT err;
		WaitingForSignal = GDT_TRUE;
		err = system(cmd);
		if(err == -1) {
			perror("zcon");
			MyExit(1);
		}
//		while(WaitingForSignal);
                for (;;)
		  pause();
//		MyExit(0);
	}
	
	CHR *DBName, *Term1, *Term2, *Use1, *Use2, *RecSyntax, *ESName,
		*BooleanOp, *RSName, *MaxRecords;
	if(!strcasecmp(Action, "SEARCH")) {
		if((Tmp = cgi_GetValueByName(cgi, "SESSION_ID")) == NULL) {
			cout << "Your form didn't include a SESSION_ID!";
			cout << endl;
			MyExit(1);
		}
		if(*Tmp == '\0') {
			cout << "Your form didn't include a SESSION_ID!";
			cout << endl;
			MyExit(1);
		}
		pid_t PID = atoi(Tmp);
		TempFile = new CHR[1024];
		sprintf(TempFile, "/tmp/zcon.%ld", PID);
		if((DBName = cgi_GetValueByName(cgi, "DBNAME")) == NULL) {
			DBName = new CHR[11];
			strcpy(DBName, "XXDEFAULT");
		}
		if((Term1 = cgi_GetValueByName(cgi, "TERM_1")) == NULL) {
			cout << "Your form didn't include a TERM_1!";
			cout << endl;
			MyExit(1);
		}
		// Optional
		Term2 = cgi_GetValueByName(cgi, "TERM_2");
		Use2 = cgi_GetValueByName(cgi, "USE_2");

		if((ESName = cgi_GetValueByName(cgi, "ESNAME")) == NULL) {
			ESName = new CHR[2];
			strcpy(ESName, "B");
		}
		if((RecSyntax = cgi_GetValueByName(cgi, "RECSYNTAX")) == NULL) {
			RecSyntax = new CHR[strlen(USMARC_OID)+1];
			strcpy(RecSyntax, USMARC_OID);
		}
		if((Use1 = cgi_GetValueByName(cgi, "USE_1")) == NULL) {
			Use1 = (CHR *)malloc(5);
			strcpy(Use1, "1035");
		}
		if((Use2 = cgi_GetValueByName(cgi, "USE_2")) == NULL) {
			Use2 = (CHR *)malloc(5);
			strcpy(Use2, "1035");
		}
		if((BooleanOp = cgi_GetValueByName(cgi, "BOOLEAN_OP"))==NULL) {
			BooleanOp = (CHR *)malloc(5);
			strcpy(BooleanOp, "AND");
		}
		CHR *TheTerm;
		if((Term2 != NULL) && (Term2[0] != '\0')) {
			// Boolean query
			TheTerm = (CHR *)malloc(strlen(Term1)+strlen(Term2)+64);
			sprintf(TheTerm, "%s(%s[1,%s],%s[1,%s])", BooleanOp,
				Term1, Use1, Term2, Use2);
		} else {
			TheTerm = (CHR *)malloc(strlen(Term1)+64);
			sprintf(TheTerm, "%s[1,%s]", Term1, Use1);
		}
		if((MaxRecords = cgi_GetValueByName(cgi, "MAXRECORDS"))==NULL) {
			MaxRecords = (CHR *)malloc(3);
			strcpy(MaxRecords, "10");
		}
		FILE *fp;

                // Make sure that another zgate is not still running by
                // looking for other zcon.<PID> files in /tmp
                //
                if ((fp = fopen(TempFile, "r")) != NULL) {
                   fclose(fp);
                   cout << "<PRE>The HTTP server is still terminating your last" << endl;
                   cout << "stop request.  This may take up to 10 seconds. Please " << endl;
                   cout << "attempt the search again or <A HREF=\"/gateway.html\">reconnect</A>.<PRE>" << endl;
                   cout << "Please be patient, the search may take up to 12 seconds.";
                   MyExit(0);
                } else {
                   fp = fopen(TempFile, "w");
                   fprintf(fp, "%ld\nSEARCH\n%s\n%s\n%s\n%s\n%s\n",getpid(),
                        DBName, TheTerm, ESName,RecSyntax, MaxRecords);
                   fclose(fp);
                   WaitingForSignal = GDT_TRUE;
                   if(kill(PID, SIGUSR1) == -1) {
		     cout << "Your session has expired.  Please ";
		     cout << "<A HREF=\"/gateway.html\">reinitialize.</A>";
		     cout << endl;
		     unlink(TempFile);
		     MyExit(0);
                   }
//		while(WaitingForSignal);
		   for (;;)
		     pause();
//		MyExit(0);
                }
	      }
	if(!strcasecmp(Action, "PRESENT")) {
		pid_t PID;
		INT4 Start, Count;
		TempFile = new CHR[1024];

		if(Source == FROM_CGI) {
			if((Tmp=cgi_GetValueByName(cgi,"SESSION_ID")) == NULL) {
				cout << "Form didn't include a SESSION_ID!";
				cout << endl;
				unlink(TempFile);
				MyExit(1);
			}
			PID = atoi(Tmp);
		
			if((ESName=cgi_GetValueByName(cgi, "ESNAME")) == NULL) {
				ESName = new CHR[11];
				strcpy(ESName, "B");
			}
			if((RecSyntax=cgi_GetValueByName(cgi,
				"RECSYNTAX"))==NULL) {
				RecSyntax = new CHR[strlen(USMARC_OID)+1];
				strcpy(RecSyntax, USMARC_OID);
			}
			if((RSName=cgi_GetValueByName(cgi, "RSNAME")) == NULL) {
				RSName = new CHR[11];
				strcpy(RSName, "DEFAULT");
			}
			if((Tmp=cgi_GetValueByName(cgi, "START")) == NULL) {
				cout << "Form didn't include a START value!";
				cout << endl;
				unlink(TempFile);
				MyExit(1);
			}
			Start = atoi(Tmp);
			if((Tmp = cgi_GetValueByName(cgi, "COUNT")) == NULL) {
				cout << "Form didn't include a COUNT value!";
				cout << endl;
				unlink(TempFile);
				MyExit(1);
			}
			Count = atoi(Tmp);
		} else {
			// Started from CommandLine:
			//
			// zgate action pid rsname start count esname recsyntax
			//
			PID = atoi(argv[2]);
			TempString = argv[3];
			RSName = TempString.NewCString();
			Start = atoi(argv[4]);
			Count = atoi(argv[5]);
			TempString = argv[6];
			ESName = TempString.NewCString();
			TempString = argv[7];
			RecSyntax = TempString.NewCString();
		}
		sprintf(TempFile, "/tmp/zcon.%ld", PID);
		FILE *fp;
		fp = fopen(TempFile, "w");
		fprintf(fp, "%ld\nPRESENT\n%s\n%i\n%i\n%s\n%s\n",getpid(), 
			RSName, Start, Count, ESName, RecSyntax);
		fclose(fp);
		WaitingForSignal = GDT_TRUE;
		if(kill(PID, SIGUSR1) == -1) {
			cout << "Your session has expired.  Please reinitialize." << endl;
			unlink(TempFile);
			MyExit(0);	
		}
//		while(WaitingForSignal);
		for (;;)
		  pause();
//		MyExit(0);
	}
}

//
// This function is called by a SIGALRM.  Zgate should exit normally,
// but if not, then this routine will exit.  The TempFile used to
// communicate with zcon is also deleted.
//
void ZgateAlarmHandler(int t)
{
  fclose(stdout);
  fclose(stderr);
  fclose(stdin);

  unlink(TempFile);
  
  MyExit(0);
}

//
// This function is called by a SIGINT.  This routine will execute
// if the user hits the STOP button in Netscape.  The TempFile used
// to communicate with zcon is also deleted.
//
void ZgateStopHandler(int t)
{
  fclose(stdout);
  fclose(stderr);
  fclose(stdin);

  unlink(TempFile);
  
  MyExit(0);
}

void USR1Handler(int t)
{
	// Read results from TempFile
	FILE *fp;

	if((fp = fopen(TempFile, "r")) == NULL) {
		perror(TempFile);
		unlink(TempFile);
		MyExit(1);
	}

	char buf[1024];
	if(!strcasecmp(Action, "INIT")) {
		fgets(buf, sizeof(buf), fp);
		if(!strcasecmp(buf, "SUCCESS\n")) {
			// Display search form
			fgets(buf, sizeof(buf), fp);
			PID = atoi(buf);
			DisplaySearchForm(DBInfo, PID);
		} else {
			cout << "<h1>Failed to Initialize to " << Host << endl;
		}
	} else {
		while(fgets(buf, sizeof(buf), fp)) {
			cout << buf;
		}
	}
	fclose(fp);
	unlink(TempFile);	
	MyExit(1);
}

/*
Reads the HTML form Filename and adds a CGI variable named SESSION_ID
with the SessionId value to the form.  It assumes that the SESSION_ID
should be placed within the first <FORM></FORM> pair it finds.  It
then prints the form to standard output.
*/
void DisplaySearchForm(CHR *Filename, INT SessionId)
{
	FILE *fp;
	if((fp = fopen(Filename, "r")) == NULL) {
		cout << "Your gateway doesn't include a search form ";
		cout << "named <B>" << Filename << "</B>.  Please ";
		cout << "read the Isite documentation for more ";
		cout << "<A HREF=\"http://vinca.cnidr.org/software/Isite";
		cout << "/Isite.html\">information</A>." << endl;
		return;
	}

	fseek(fp, 0, 2);
	INT len = ftell(fp);
	rewind(fp);
	CHR *origbuf, *buf;
	origbuf = (CHR *)malloc(len+1);
	buf = (CHR *)malloc(len+1);
	fread(buf, 1, len, fp);
	fclose(fp);	
	buf[len] = '\0';	
	strcpy(origbuf, buf);
	INT i;
	for(i=0;i<len;i++)
		buf[i] = toupper(buf[i]);
	CHR *p = strstr(buf, "</FORM>");
	if(!p) {
		cout << "Your form ";
		cout << "named <B>" << Filename << "</B> doesn't include ";
		cout << "a proper FORM tag.  Please ";
		cout << "read the Isite documentation for more ";
		cout << "<A HREF=\"http://vinca.cnidr.org/software/Isite";
		cout << "/Isite.html\">information</A>." << endl;
		MyExit(1);
	}

	// Go ahead and print the form up to </FORM>
        origbuf[len] = origbuf[p-buf];          // hang onto the critical char
        origbuf[p-buf] = (char) 0;              // end off the string
        cout << origbuf;
        origbuf[p-buf] = origbuf[len];          // restore...
        origbuf[len] = (char) 0;

	//cout.form("%.*s", p - buf, origbuf);

	// Print the SESSION_ID variable
	cout << "<INPUT NAME=\"SESSION_ID\" VALUE=\"" << SessionId;
	cout << "\" TYPE=\"HIDDEN\">" << endl;

	// Print the rest of the form
	cout << (origbuf+(p-buf));

	cout.flush();
}

void MyExit(INT Status)
{
	fclose(stdout);
	fclose(stdin);
	fclose(stderr);

	exit(Status);
}
