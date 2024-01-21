#include <iostream.h>
#include <stdio.h>
#include <sys/types.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <values.h>
#include <locale.h>

#if defined(_MSDOS) || defined(_WIN32)
#include <process.h>
#else
#include <unistd.h>
#endif

#include "zclibase.hxx"
#include "marc.hxx"

#define DEFAULT_TIMEOUT 300

void AlarmHandler(int t);
void USR1Handler(int t);

char gCommandFile[1024];

ZCLIENT_BASE *zclient;
pid_t CallersPID;

//
// Caller passes a temp filename as the first argument and its pid as the
// second arg.  When this app finishes (trying) to initialize to the 
// Z39.50 server, it creates the temporary file, writes a filename it will
// use for subsequent IPC on line 1 and its pid on line 2.  The caller
// can use that pid and filename combo to later send commands to this
// process (SIGUSR1).
//
// Host should be argv[3], port argv[4]
//
// args after that are optional:
//
//	-gid	GroupId
//	-uid	UserId
//	-pass	Password
//
int main(int argc, char **argv)
{


  if (!setlocale(LC_CTYPE,"")) {
    cout << "Warning: Failed to set the locale!" << endl;
  }

	ios::sync_with_stdio();

	fclose(stdin);
	fclose(stdout);
	fclose(stderr);

	// Install an interrupt handler for an alarm signal
	signal(SIGALRM, AlarmHandler);
	alarm(DEFAULT_TIMEOUT);

	signal(SIGUSR1, USR1Handler);

	CallersPID = atoi(argv[2]);

	FILE *fp;
	if((fp = fopen(argv[1], "w")) == NULL) {
		// Big trouble.  Nothing we can do.  Caller should time out
		// and/or check for existence of this file.
		kill(CallersPID, SIGUSR1);
		exit(1);
	}

	strcpy(gCommandFile, argv[1]);
	
	zclient = new ZCLIENT_BASE();
	
	// Initialize with the server
	//
	// Was a groupid, userid and password included?
	//
	STRING GroupId, UserId, Password;
	if(argc > 5) {
		// Yep
		STRING arg;
		INT i;
		for(i=5;i<argc;i+=2) {
			arg = argv[i];
			if(arg.CaseEquals("-gid"))
				GroupId = argv[i+1];	
			if(arg.CaseEquals("-uid"))
				UserId = argv[i+1];	
			if(arg.CaseEquals("-pass"))
				Password = argv[i+1];	
		}
		if(!zclient->Initialize(argv[3], atoi(argv[4]),
			GroupId, UserId, Password)) {
			fprintf(fp, "FAILURE [%d]\n%ld\n", 
				zclient->GetLastError(), getpid());
			fclose(fp);
			kill(CallersPID, SIGUSR1);
			exit(1);
		}

	} else {
		//
		// Nope, do it the easy way
		//
		if(!zclient->Initialize(argv[3], atoi(argv[4]))) {
			fprintf(fp, "FAILURE [%d]\n%ld\n", 
				zclient->GetLastError(), getpid());
			fclose(fp);
			kill(CallersPID, SIGUSR1);
			exit(1);
		}
	}
	fprintf(fp, "SUCCESS\n%ld\n", getpid());
	fclose(fp);
	kill(CallersPID, SIGUSR1);
	sprintf(gCommandFile, "/tmp/zcon.%ld", getpid());

	// Accept all other commands via the signal handler
	for(;;)
		pause();
}

void AlarmHandler(int t)
{
	fclose(stdout);
	fclose(stderr);
	fclose(stdin);
	
	unlink(gCommandFile);

	zclient->Close();
	delete zclient;
	exit(0);
}

#define CMD_SEARCH 0
#define CMD_PRESENT 1

void USR1Handler(int t)
{
	char cmd[1024];

	// reset the alarm
	alarm(DEFAULT_TIMEOUT);

	FILE *fp;
	if((fp = fopen(gCommandFile, "r")) == NULL) {
		perror(gCommandFile);
		exit(1);
	}
	int	cmd_count=0,
		command=-1,
		start=1,
		count=1,
		MaxRecords=10;
	char	db[64],
		resultset[64],
		term[128],
		esname[128],
		recsyntax[128];
	strcpy(esname, "B");
	strcpy(recsyntax, USMARC_OID);
	while(fgets(cmd, 256, fp)) {
		int len=strlen(cmd);
		if(len > 1)	
			cmd[len-1] = '\0';
		else break;
		cout << "Read command: " << cmd << endl;
		switch(cmd_count) {
			case 0:
				sscanf(cmd, "%ld", &CallersPID);
				break;
			case 1:
				if(!strcasecmp(cmd, "search"))
					command=CMD_SEARCH;
				else if(!strcasecmp(cmd, "present"))
					command=CMD_PRESENT;
				else {
					cout << "Invalid command" << endl;
					kill(CallersPID, SIGUSR1);
					exit(1);
				}
				break;
			case 2:
				if(command==CMD_SEARCH)
					strcpy(db, cmd);
				if(command==CMD_PRESENT)
					strcpy(resultset, cmd);
				break;
			case 3:
				if(command==CMD_SEARCH)
					strcpy(term, cmd);
				if(command==CMD_PRESENT)
					start = atoi(cmd);
				break;
			case 4:
				if(command==CMD_PRESENT)
					count = atoi(cmd);
				if(command==CMD_SEARCH)
					strcpy(esname, cmd);
			case 5:
				if(command==CMD_SEARCH)
					strcpy(recsyntax, cmd);
				if(command==CMD_PRESENT)
					strcpy(esname, cmd);
			case 6:
				if(command==CMD_SEARCH)
					MaxRecords = atoi(cmd);
				if(command==CMD_PRESENT)
					strcpy(recsyntax, cmd);
		}	
		cmd_count++;
	}
	fclose(fp);

	unlink(gCommandFile);
	fp = fopen(gCommandFile, "w");
	
	static INT4 HitCount;
	switch(command) {
		case CMD_SEARCH: {
			fprintf(fp, "<TITLE>%s[%s]</TITLE>\n",
				db, term);
			if(!zclient->Search(db, term, &HitCount)) {
				cout << "Search failed." << endl;
				exit(1);
			}
			fprintf(fp, "<H1>Query Results</H1>\n");
        		if(HitCount == 0) {
				fprintf(fp, "<I>No records matched your query</I><BR>");
                		break;
			}
 
        		INT4 FetchCount;
        		if(HitCount <= MaxRecords)
                        		FetchCount = HitCount;
                		else
                        		FetchCount = MaxRecords;
 
        		//fprintf(fp, "Fetching %ld records\n", FetchCount);
 
        		ZRECORDLIST RecordList;
        		if(!zclient->Present(1, FetchCount, recsyntax, esname, &RecordList)) {
                		fprintf(fp, "Failed to present\n");
                		exit(1);
        		}
 
        		INT4 Count, i;
        		Count = RecordList.GetRecordCount();
        		fprintf(fp, "<I>Records 1 through %d of %d returned.</I><HR>", Count, HitCount);
        		ZRECORDSCHOICE *Record;
        		STRING Data, OID;
        		for(i=0;i < Count;i++) {
                		Record = RecordList.GetRecord(i);
                		if(!Record)
                        		continue;
                		Record->GetOID(&OID);
                		Record->GetRecordData(&Data);
                		if(OID == USMARC_OID) {
                        		MARC *m;
                        		m = new MARC(Data);
					fprintf(fp, "<PRE>"); 
                        		m->Print(fp);
					fprintf(fp, "</PRE>"); 
                        		delete m;
                		} else {
					CHR *temp;
					temp = Data.NewCString();
                        		fprintf(fp, "%s\n", temp);
					delete [] temp;
				}
				fprintf(fp, "<BR><A HREF=\"/cgi-bin/zgate?present+%ld+Default+%d+1+F+%s\">More on this record</A>", getpid(), start+i, recsyntax);

				fprintf(fp, "<HR>");
        		}
			if(HitCount > FetchCount) {
				fprintf(fp, "<form method=\"POST\" action=\"/cgi-bin/zgate\">");
				fprintf(fp, "<input name=\"action\" value=\"PRESENT\" type=\"HIDDEN\">");
				fprintf(fp, "<input name=\"start\" value=\"%d\" type=\"HIDDEN\">", start+FetchCount);
				fprintf(fp, "<input name=\"ESNAME\" value=\"B\" type=\"HIDDEN\">");
				fprintf(fp, "<input name=\"RECSYNTAX\" value=\"%s\" type=\"HIDDEN\">", recsyntax);
				fprintf(fp, "<input name=\"count\" value=\"%d\" type=\"HIDDEN\">", Count <= (HitCount-(start+Count-1))?Count:(HitCount-(start+Count-1)));
			fprintf(fp, "<input name=\"SESSION_ID\" value=\"%ld\" type=\"HIDDEN\">", getpid());
				fprintf(fp, "<input type=\"SUBMIT\" value=\"Show More Records\">\n");
				fprintf(fp, "</form>\n");
			}
 
}
			break;
		case CMD_PRESENT: {
        		ZRECORDLIST RecordList;
			fprintf(fp, "<TITLE>%s[%s] (%d-%d)</TITLE>\n",
				db, term, start, count);
        		if(!zclient->Present(start, count, recsyntax, esname, 
				&RecordList)) {
                		fprintf(fp, "Failed to present\n");
                		exit(1);
        		}
 
        		INT4 Count, i;
        		Count = RecordList.GetRecordCount();
if(strcasecmp(esname, "F"))
        		fprintf(fp, "<I>Records %d through %d of %d returned.</I><HR>", start, start+Count-1, HitCount);
        		ZRECORDSCHOICE *Record;
        		STRING Data, OID;
        		for(i=0;i < Count;i++) {
                		Record = RecordList.GetRecord(i);
                		if(!Record)
                        		continue;
                		Record->GetOID(&OID);
                		Record->GetRecordData(&Data);
                		if(OID == USMARC_OID) {
                        		MARC *m;
                        		m = new MARC(Data);
					fprintf(fp, "<PRE>"); 
                        		m->Print(fp);
					fprintf(fp, "</PRE>"); 
                        		delete m;
                		} else {
					CHR *temp;
					temp = Data.NewCString();
                        		fprintf(fp, "%s\n", temp);
					delete [] temp;
				}
				if(strcasecmp(esname, "F"))
					fprintf(fp, "<BR><A HREF=\"/cgi-bin/zgate?present+%ld+Default+%d+1+F+%s\">More on this record</A>", getpid(), start+i, recsyntax);
				fprintf(fp, "<HR>");
        		}
if(strcasecmp(esname, "F")) {
			if(HitCount > (start+Count-1)) {
				fprintf(fp, "<form method=\"POST\" action=\"/cgi-bin/zgate\">");
				fprintf(fp, "<input name=\"action\" value=\"PRESENT\" type=\"HIDDEN\">");
				fprintf(fp, "<input name=\"start\" value=\"%d\" type=\"HIDDEN\">", start+Count);
				fprintf(fp, "<input name=\"RECSYNTAX\" value=\"%s\" type=\"HIDDEN\">", recsyntax);
				fprintf(fp, "<input name=\"ESNAME\" value=\"B\" type=\"HIDDEN\">");
				fprintf(fp, "<input name=\"count\" value=\"%d\" type=\"HIDDEN\">", count <= (HitCount-(start+count-1))?count:(HitCount-(start+count-1)));
			fprintf(fp, "<input name=\"SESSION_ID\" value=\"%ld\" type=\"HIDDEN\">", getpid());
				fprintf(fp, "<input type=\"SUBMIT\" value=\"Show More Records\">\n");
				fprintf(fp, "</form>\n");
			}
}
			break;
		}
			
	}
	fprintf(fp, "Search and retrieval software courtesy of <A HREF=\"http://www.cnidr.org/\">CNIDR</A>");

	fclose(fp);
	kill(CallersPID, SIGUSR1);
	
	// Reinstall the signal handler
	signal(SIGUSR1, USR1Handler);
	
	alarm(DEFAULT_TIMEOUT);
}
