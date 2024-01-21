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
File:		zserver.cxx
Version:	1.00
Description:	Z39.50 server
Author:		Kevin Gamiel, kevin.gamiel@cnidr.org
@@@*/

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <locale.h>

#if defined(_MSDOS) || defined(_WIN32)
#include <direct.h>
#include <limits.h>
#include <io.h>
#include <process.h>
#include <sys/locking.h>

#define MAXPATHLEN _MAX_PATH
#define pid_t int
#else
#include <sys/file.h>
#include <sys/wait.h>
#include <unistd.h>
#endif

#include "zserver.hxx"
#include "sapi.h"
#include "rset.hxx"
#include "opstack.hxx"
#include "operator.hxx"
#include "sterm.hxx"
#include "idb.hxx"
#include "config.hxx"
#include "zsquery.hxx"
#include "common.hxx"
#include "zrecords.hxx"

int main(int argc, char **argv)
{
	ZSERVER *server;

	if (!setlocale(LC_CTYPE,"")) {
	  cout << "Warning: Failed to set the locale!" << endl;
	}

	ios::sync_with_stdio();

	server = new ZSERVER(argc, argv);
	server->StartServer();
	delete server;

	return 0;
}

ZSERVER::ZSERVER(int argc, char **argv):ZSESSION()
{
	if(StartedByInetd()) {
		//
		// Route all console output to a file.
		//
		c_output_file.open(ZSERVER_INETD_OUTPUT,ios::app);

#ifndef __SUNPRO_CC
		cerr = cout = c_output_file;
		// for now, disallow this because of incompatibilities between
		// the relevant iostream classes.....
#endif
		c_debuglevel = 0;
	}
	c_group = "Default";
	c_inifile = "zserver.ini";
	c_result_set = NULL;
	c_sapi = NULL;
	c_default_db = "";
	
	cerr << endl << "CNIDR zserver, Version " << ZDIST_VERSION;
	cerr << ", Copyright (c) 1995,96 MCNC/CNIDR\n" << endl;

	// Set defaults from zserver.ini in CWD (if available) 
	LoadDefaultsFromFile(c_inifile, c_group);
	
	// Set defaults from command line
	STRING g, file;
	g = c_group;
	file = c_inifile;
	LoadDefaultsFromCommandLine(argc, argv, c_inifile, c_group, 
		&g, &file);

	c_group = g;
	c_inifile = file;
	ExpandFileSpec(&c_inifile);

	c_inipath = c_inifile;
	RemoveFileName(&c_inipath);

	//
	// Now that we've loaded all possible combination of user variables,
	// lets copy those variables from the generic REGISTRY structure
	// into private class variables for ease of use.  If any values
	// were not explicity set by the user, hard-coded defaults will
	// be inserted.
	//
	StoreDefaults(c_group);

	// Read list of databases to serve from zserver configuration file
	STRLIST Position, DBList;
	Position.AddEntry("Default");
	Position.AddEntry("DBList");
	c_defaults->GetData(Position, &DBList);
	c_database_count = DBList.GetTotalEntries();
	if(c_database_count <= 0) {
		cerr << "No databases configured for zserver [" << c_inifile;
		cerr << "]." << endl;
		//cerr << "Running anyway in test mode." << endl;
		exit(1);
	}

	// Load SAPI configuration file
	c_defaults->ProfileGetString(c_group, "SAPI", "sapi.ini", &c_sapifile);

        if(c_debuglevel >= 2) {
                cerr << "zserver.ini Path = " << c_inifile << endl;
                cerr << "SAPI File = " << c_sapifile << endl;
        }
	
	//
	// If the filename has a leading slash, assume its a full path,
	// otherwise, append it to the inipath
	//
        if(c_sapifile.GetChr(1) != '/')
		c_sapifile.Insert(1, c_inipath);
	CHR *p = c_sapifile.NewCString();
	
	c_sapi = sapi_CreateInitFile(p, "Default");
	if(!c_sapi) {
		cerr << "Failed to load CNIDR Search API [" << p << "]" << endl;
		exit(1);
	}
	c_sapipath = c_sapifile;
	RemoveFileName(&c_sapipath);
	if(c_debuglevel >=2)
		cerr << "Looking for map files in " << c_sapipath << endl;
	
	if(p)
		delete [] p;
	if(sapi_GetDatabaseCount(c_sapi) <= 0) {
		cerr << "There are no databases available via SAPI!  ";
		cerr << "Edit " << c_sapi << endl;
		exit(1);
	}

	// Load the default field mapping table, bib1;
	c_mapping_table = new REGISTRY("map");

	// Attempt mount the databases
	c_dblist = new PSAPI_DBASE[c_database_count];
	INT actual_db_count=0;
	STRING DBName;
	for(INT i=1;i <= c_database_count;i++) {
		DBList.GetEntry(i, &DBName);
		cerr << "Attempting to mount database " << DBName << "...";
		p = DBName.NewCString();
		c_dblist[actual_db_count] = sapi_GetDatabase(c_sapi, p);
		delete [] p;
		if(c_dblist[actual_db_count]) {
			STRING Filename;
			cerr << "success." << endl;
			if(i == 1)
				c_default_db = DBName;
			actual_db_count++;

			// Load the field mapping for this database
			// Nassib is working on making this process
			// easier, but for now I'm calling a separate
			// function that does it by hand.
			
			// Store the default mapping table, bib1.map, for
			// this database.
			Filename = c_sapipath;
                        if(c_sapipath.GetLength() > 0)
                                if(c_sapipath.GetChr(c_sapipath.GetLength()) 
                                      != '/')
					Filename.Cat("/");
			Filename.Cat("bib1.map");
			AddToMappingTable(Filename, DBName);

			// Now step through any mapping files the user
			// has specified, overlaying the default bib1
			// mapping.

			// Get the (optional) list of mapping files
			// for this database from the sapi file.
			REGISTRY *sapireg;
			STRING MapFile;
			STRLIST ResultList;
			STRINGINDEX count, j;
	
			sapireg = new REGISTRY("sapi");
			sapireg->ProfileLoadFromFile(c_sapifile, ResultList);
			Position.Clear();
			Position.AddEntry(DBName);
			Position.AddEntry("FieldMaps");
			sapireg->GetData(Position, &ResultList);
			count = ResultList.GetTotalEntries();	
			for(j=1;j <= count;j++) {
				ResultList.GetEntry(j, &MapFile);
				if(MapFile.GetChr(1) != '/') {
					Filename = c_sapipath;
                                        if(c_sapipath.GetLength() > 0)
						if(c_sapipath.GetChr(
						  c_sapipath.GetLength())!='/')
							Filename.Cat("/");
					Filename.Cat(MapFile);
				} else Filename = MapFile;
				AddToMappingTable(Filename, DBName);
			}

		} else
			cerr << "failed." << endl;
	}
	c_database_count = actual_db_count;
}

ZSERVER::~ZSERVER()
{
	if(c_sapi)
		sapi_Destroy(c_sapi);
	if(c_result_set)
		ResultSet_Destroy(c_result_set);
}

void ZSERVER::StartServer()
{
	STRING StrVal;

	//
	// Were we started by inetd or from the command line?
	//
	if(StartedByInetd()) {
		INT err;
		CHR msg[128];
		/* Can't do any debugging output until I fix the
		hexdir stuff to use streams instead of stderr.

		c_defaults->ProfileGetString(c_group, "DebugLevel", "1", 
			&StrVal);
		c_debuglevel = StrVal.GetInt();
		*/
		c_debuglevel = 0;
		c_tcp = new TCPSOCK();
		if((err = c_tcp->LastError())) {
			c_tcp->ErrorMessage(err, msg, 
				(sizeof(msg) - 1));
                       	cerr << msg << endl;
                       	return;
		}
               	c_tcp->SetSocket((TSOCKET)fileno(stdout));
		StartSession();
	} else {
		c_defaults->ProfileGetString(c_group, "DebugLevel", "1", 
			&StrVal);
		c_debuglevel = StrVal.GetInt();
		cerr << "DebugLevel=" << c_debuglevel << "." << endl;

#if !defined(_MSDOS) && !defined(_WIN32)
		if(c_server_type.CaseEquals("STANDALONE"))
			StartForkingServer(c_port);
		else
#endif
			StartServer(c_port);
	}
}

#if !defined(_MSDOS) && !defined(_WIN32)
INT ZSERVER::StartForkingServer(UINT Port)
{
	if(!Listen(Port))
		return 0;
	
	if(c_debuglevel >= 1) {
		cerr << "Forking Server Started, Port " << Port;
		cerr << "..." << endl;
	}

        INT child_proc, zpid;
        INT ChildCount=0;
        PINT Children;
        INT i,Status,j;
        INT id;
        INT MaxChildren;
 
        MaxChildren = 50;
        Children = (PINT)calloc(sizeof(INT),MaxChildren);
 
        for(;;) {
		if(!AcceptClient())
			return 0;
		if(c_debuglevel >= 1)
			cerr << "Received connection..." << endl;

                // Any children finished yet?
                for(i=0;i<ChildCount;i++) {
                        id = waitpid(Children[i], &Status,
                                WNOHANG|WUNTRACED);
                        switch(id) {
                                case 0:
                                        break;
                                case -1:
                                        perror("zserver_RunForkingServer");
                                        break;
                                default:
                                        ChildCount--;
                                        Children[i]=-5;
 
                        }
                }

                // Squeeze children list
                j = 0;
                for(i=0;i<MaxChildren;i++)
                        if(Children[i] != -5)
                                Children[j++]=Children[i];

		// Are there too many children already? 
                if(ChildCount == MaxChildren) {
                        // Send init response set to denied
                        cerr << "Session limit exceeded" << endl;
                        // SEND INIT RESPONSE HERE
                } else {
                	if((child_proc=fork())==0) {
                        	// In child 
                        	zpid = getpid();
				if(c_debuglevel >= 1)
					cerr << "Child pid = " << zpid << endl;
				c_server_tcp->Close();
				StartSession();
                        	exit(0);
                	} else {
                        	// In Parent
                        	if(child_proc == -1) {
                                	// Error creating child 
					perror("child_proc");
                        	} else {
                                	//Keep track of number of children for
                                	//load balancing purposes
                                	Children[ChildCount++]=child_proc;
                        	}
                                // Close the child socket
				delete c_tcp;
                	} // else
                } // else
        } // for

	return 1;
}
#endif

INT ZSERVER::Listen(UINT Port)
{
	INT err;
	CHR msg[128];

	c_server_tcp = new TCPSOCK();
        if((err = c_server_tcp->LastError())) {
                c_server_tcp->ErrorMessage(err, msg, (sizeof(msg) - 1));
                cerr << msg << "\n";
		return 0;
        }

	c_server_tcp->Listen(Port, 5);
        if((err = c_server_tcp->LastError())) {
                c_server_tcp->ErrorMessage(err, msg, (sizeof(msg) - 1));
                cerr << "Listen failed: " << msg << endl;
		return 0;
        }
	if(c_debuglevel >= 2)
		cerr << "Listening on port " << Port << "..." << endl;
	if(c_debuglevel >= 7) {
		cerr << "Listening to socket # " << c_server_tcp->Socket();
		cerr << "..." << endl;
	}

	c_server_tcp->BlockingModeON();
	return 1;
}

INT ZSERVER::StartServer(UINT Port)
{
	if(Listen(Port)) {
		if(c_debuglevel >= 1) {
			cerr << "Single Connection Server Started, Port ";
			cerr << Port << "..." << endl;
		}
		if(!AcceptClient())
			return 0;

		// We don't need the main socket anymore, so kill it.
		delete c_server_tcp;

		if(c_debuglevel >= 1)
			cerr << "Received connection..." << endl;
		StartSession();
		return 1;
	} else
		return 0;
}

//
// 1 if connection, 0 otherwise.
//
// Blocks until connection arrives
//
INT ZSERVER::AcceptClient()
{
	INT err;
	CHR msg[128];
	TCPSOCK *new_socket;
	new_socket = new TCPSOCK;
	
	if(c_debuglevel >= 5)
		cerr << "Waiting to Accept client..." << endl;

	if(c_server_tcp->Accept(new_socket) > 0) {
		if(c_debuglevel >= 1)
			cerr << "Connection has arrived..." << endl;
	
		new_socket->GetHostname(&c_client_hostname);
		new_socket->GetIPAddress(&c_client_ipaddress);

		// Allocate the tcp stream to be used for this client 
		// connection.
		c_tcp = new TCPSOCK();
		if((err = c_tcp->LastError())) {
			c_tcp->ErrorMessage(err, msg, 
				(sizeof(msg) - 1));
                       	cerr << msg << endl;
                       	return 0;
		}
               	*c_tcp = *new_socket;

		return 1;
	}
	if((err = c_tcp->LastError())) {
		c_tcp->ErrorMessage(err, msg, (sizeof(msg) - 1));
		cerr << msg << endl;
		return 0;
	}
	return 0;
}

void ZSERVER::StartSession()
{
	unsigned short type;
	INT4 len, BytesSent;
	CHR TempBuf[64];
	UCHR *buf;
	CHR *TempString;
	INT Finished = 0;
	time_t Start, Done;

	time(&Start);
	do {
		if((buf = ReadPDU(&len, &type)) == NULL)  {
			// Client closed on us without a proper CLOSE
			time(&Done);
			sprintf(TempBuf, "%ld", Done - Start);
			Log("DISCONNECT", TempBuf, 1, 0);
			c_tcp->Close();
			return;
		}
		switch(type) {
			case INITREQUEST_TAG: 
			{
				ZINITREQUEST *rq;	// Init Request PDU
				INT4 Result = 1;	// Assume success

				// Cast buffer to PDU
				rq = new ZINITREQUEST(buf, len);

				// Negotiate based on request parameters
		
				//	
				// Grab the Reference ID
				//
				c_refid = "";
				rq->GetReferenceId(&c_refid);

				//	
				// We support versions 2 and 3 (default=2)
				//
				rq->GetProtocolVersion(&c_protocol_version);
				c_protocol_version.UpperCase();
				if((!c_protocol_version.Equals("YY")) && 
					(!c_protocol_version.Equals("YYY")))
					c_protocol_version = "YY";

				//
				// We support Init, Search and Present.
				// If you need their options, here they
				// are.  Ours were set in our constructor.
				//
				STRING Value;
				rq->GetOptions(&Value);

				//
				// Negotiate preferred message size
				//
				rq->GetPreferredMessageSize(&c_prefmsgsize);
				if(c_prefmsgsize > ZPREFMSGSIZE)
					c_prefmsgsize = ZPREFMSGSIZE;
				if(c_prefmsgsize < 1024)
					c_prefmsgsize = 1024;
			
				//	
				// Negotiate maximum message size
				//
				rq->GetExceptionalRecordSize(&c_maxrecordsize);
				if(c_maxrecordsize > ZMAXRECORDSIZE)
					c_maxrecordsize = ZMAXRECORDSIZE;
				if(c_maxrecordsize < 1024)
					c_maxrecordsize = 1024;

				//
				// GroupId/UserId/Password?
				//
				rq->GetGroupId(&c_groupid);
				rq->GetUserId(&c_userid);
				rq->GetPassword(&c_password);

				//
				// If you want to do authentication,
				// do it here.  We'll add support later.
				//

				delete rq;

				//
				// Build the response
				//
				ZINITRESPONSE *rs;
				rs = new ZINITRESPONSE(c_refid,
					c_protocol_version,
					c_options,
					c_prefmsgsize,
					c_maxrecordsize,
					Result,
					CNIDR_IMP_ID, 
					ZSERVER_NAME,
					ZDIST_VERSION);

				if((BytesSent = SendPDU(rs)) == 0)
					Finished = 1;
				delete rs;

				Log("INIT", "-", 1, BytesSent);
				break;
			}
			case SEARCHREQUEST_TAG: 
			{
				ZSEARCHREQUEST *rq;	// Search Request PDU

				rq = new ZSEARCHREQUEST(buf, len);

				// Perform the search
				ZSEARCHRESPONSE *rs;	// Search Response PDU
				rs = Search(*rq);
				delete rq;

				if((BytesSent = SendPDU(rs)) == 0)
					Finished = 1;
		
				// Get result of search for logging purposes	
				INT4 Result;
				rs->GetResultCount(&Result);
				TempString = c_dbname.NewCString();	

				Log("SEARCH", TempString, Result, BytesSent);

				delete [] TempString;
				delete rs;

				break;
			}
			case PRESENTREQUEST_TAG: 
			{
				ZPRESENTREQUEST *rq;	// Present Request PDU

				rq = new ZPRESENTREQUEST(buf, len);

				ZPRESENTRESPONSE *rs;	// Present Response PDU
				rs = Present(*rq);
                                if(!rs) {
                                        cerr << "Fatal Error. No PRESENTRESPONSE generated" << endl;
                                        exit(1);
                                }

				delete rq;

				if((BytesSent = SendPDU(rs)) == 0)
					Finished = 1;
				
				INT4 Status;
				rs->GetPresentStatus(&Status);
				TempString = c_dbname.NewCString();	
				
				Log("PRESENT", TempString, Status, BytesSent);
	
				delete [] TempString;
				delete rs;

				break;
			}
			case EXTENDEDSERVICESREQUEST_TAG:
			{
				ZESREQUEST *rq;	// Extended Service Request PDU

				rq = new ZESREQUEST(buf, len);

				INT4 Function;
				STRING PackageType;
				STRING PackageName;
				STRING UserId;
				INT4 RetentionTime;
				STRING Description;
				INT4 WaitAction;

				rq->GetFunction(&Function);

				rq->GetPackageType(&PackageType);
				rq->GetPackageName(&PackageName);
				rq->GetUserId(&UserId);
				rq->GetRetentionTime(&RetentionTime);
				rq->GetDescription(&Description);
				rq->GetWaitAction(&WaitAction);

				delete rq;

				ZESTASKPACKAGE *TaskPackage;

				RetentionTime = 0;

				TaskPackage = NULL;
				if ( WaitAction !=
					ES_DONT_RETURN_PACKAGE_WAITACTION )
					TaskPackage = new ZESTASKPACKAGE(
						PackageType,
						PackageName,
						UserId,
						RetentionTime,
						Description,
						ES_TP_COMPLETE_TASKSTATUS);

				ZESRESPONSE *rs;// Extended Service Response PDU

				TempString = "UNSUPPORTED";

				ZDEFAULTDIAGFORMAT *diagrec;	

				diagrec = new ZDEFAULTDIAGFORMAT(221, PackageType, BIB1_DIAG_OID);
				rs = new ZESRESPONSE("", ES_FAILURE_OPERATIONSTATUS, diagrec, NULL);
				delete diagrec;

				if ( TaskPackage )
					delete TaskPackage;

				if((BytesSent = SendPDU(rs)) == 0)
					Finished = 1;

				INT4 OperationStatus;

				rs->GetOperationStatus(&OperationStatus);

				Log("EXTENDSERVICE", TempString, OperationStatus, BytesSent);

				delete rs;

				break;
			}
			case CLOSE_TAG: 
			{
				// Cast buffer to PDU
				ZCLOSE *pdu;
				pdu = new ZCLOSE(buf, len);

				// Why did they close?
				INT4 Reason;
				pdu->GetCloseReason(&Reason);
				if(c_debuglevel >= 1) {
					cerr << "Client has closed.  Reason: ";
					switch(Reason) {
					case 0:
						cerr << "Finished";
						break;
					case 1:
						cerr << "Shutdown";
						break;
					case 2:
						cerr << "System Problem";
						break;
					case 3:
						cerr << "Cost Limit";
						break;
					case 4:
						cerr << "Resources";
						break;
					case 5:
						cerr << "Security Violation";
						break;
					case 6:
						cerr << "ProtocolError";
						break;
					case 7:
						cerr << "Lack of Activity";
						break;
					case 8:
						cerr << "Peer Abort";
						break;
					case 9:
						cerr << "Unspecified";
						break;
					default:
						cerr << "Unknown reason";
						break;
					}
					cerr << endl;
				}
		
				// We're done!
				Finished = 1;
				time(&Done);
				sprintf(TempBuf, "%ld", Done - Start);
				Log("CLOSE", TempBuf, 1, 0);
				break;
			}
			default:
				cerr << "Unsupported PDU of " << type << endl;
		}
		delete [] buf;
	} while(!Finished);
}

ZSEARCHRESPONSE *ZSERVER::Search(ZSEARCHREQUEST & Request)
{
	ZSEARCHRESPONSE *rs;
	INT hitcount, i;
	CHR *p;
	PSAPI_DBASE sapi_db = NULL;
	PSAPI_QUERY sapi_query = NULL;
	BERBROWSER *b;
	ZRECORDLIST *Records;
	ZDEFAULTDIAGFORMAT *diagrec;	
	
	// We only support a single database name
	Request.GetDatabaseName(&c_dbname);
	if(c_dbname.CaseEquals("xxdefault"))
		c_dbname = c_default_db;
	
	// Is database name valid?
	for(i=0;i < c_database_count;i++) {
		p = dbase_GetName(c_dblist[i]);
		if(p) {
			if(c_dbname.CaseEquals(p)) {
				sapi_db = c_dblist[i];
				break;
			}
		}
	}
	if(i == c_database_count) {
		// illegal database name requested
		if(c_debuglevel >= 2) {
			cerr << "Illegal database name requested: " << c_dbname;
			cerr << endl;
		}
		// Construct a non-surrogate diagnostic record and return
		// with response
		Records = new ZRECORDLIST(NONSURROGATEDIAGNOSTIC_TAG);
		diagrec = new ZDEFAULTDIAGFORMAT(109, c_dbname, BIB1_DIAG_OID);
		Records->AddRecord(diagrec);
		rs = new ZSEARCHRESPONSE(c_refid, 0, 0, 0, GDT_FALSE, 3, 5,
			Records);
		delete diagrec;
		delete Records;
		return rs;
	}

	if(c_debuglevel >= 2)
		cerr << "Database name " << c_dbname << " requested..." << endl;

	// If this is an Isearch database, perform custom query :-)
	if(dbase_GetType(sapi_db) == SAPI_DBTYPE_ISEARCH) {
		ZSQUERY *query;
		STRLIST FieldsAvailable;

		dbase_GetFieldNames(sapi_db, &FieldsAvailable);
		INT count, i;
		count = FieldsAvailable.GetTotalEntries();
		STRING temp;
		if(count == 0)
			cerr << "No fields available!!" << endl;
		for(i=1;i <= count;i++) {
			FieldsAvailable.GetEntry(i, &temp);
		}
		b = new BERBROWSER(&Request);
		query = new ZSQUERY();
		query->SetDebugLevel(c_debuglevel);
		query->ConvertQuery(b, c_mapping_table, FieldsAvailable,
			c_dbname);
		delete b;
		if(query->Error()) {
			INT code;
			STRING addinfo;
			query->GetErrorInfo(&code, &addinfo);
			// build diag rec here
			Records = new ZRECORDLIST(NONSURROGATEDIAGNOSTIC_TAG);
			diagrec = new ZDEFAULTDIAGFORMAT(code, addinfo,
				BIB1_DIAG_OID);
			Records->AddRecord(diagrec);
			rs = new ZSEARCHRESPONSE(c_refid, 0, 0, 0, GDT_FALSE, 
				3, 5, Records);
			delete diagrec;
			delete Records;
			return rs;
		}
		sapi_query = query_CreateIsearch(*query);
	} else {
		// Convert from Z39.50 structure to KWAQS
		CHR *query_str;
		b = new BERBROWSER(&Request);
		KWAQS_STRING *q;
		q = new KWAQS_STRING;
		q->SetDebugLevel(c_debuglevel);
		q->Convert(b, Request);
		if(q->Error()) {
			INT code;
			STRING addinfo;
			q->GetErrorInfo(&code, &addinfo);
			// build diag rec here
			Records = new ZRECORDLIST(NONSURROGATEDIAGNOSTIC_TAG);
			diagrec = new ZDEFAULTDIAGFORMAT(code, addinfo,
				BIB1_DIAG_OID);
			Records->AddRecord(diagrec);
			rs = new ZSEARCHRESPONSE(c_refid, 0, 0, 0, GDT_FALSE, 
				3, 5, Records);
			delete diagrec;
			delete Records;
			return rs;
		}
		query_str = q->NewCString();
		sapi_query = query_CreateSimple(query_str);
		delete [] query_str;
	}

	c_result_set = sapi_Search(c_sapi, sapi_db, sapi_query);
	query_Destroy(sapi_query);

	hitcount = ResultSet_GetHitCount(c_result_set);
	c_hitcount = hitcount;

	if(c_debuglevel >= 5)
		cerr << "There were " << hitcount << " hits." << endl;

	INT4 IntValue;
	Request.GetSmallSetUpperBound(&IntValue);
 
	if((hitcount > 0) && (IntValue > 0)) {
	  ZRECORDLIST *RecordList;
	  RecordList = new ZRECORDLIST(NONSURROGATEDIAGNOSTIC_TAG);
	  //ZDEFAULTDIAGFORMAT *diagrec;
	  diagrec = new ZDEFAULTDIAGFORMAT(1005, "", BIB1_DIAG_OID);
	  RecordList->AddRecord(diagrec);
	  rs = new ZSEARCHRESPONSE(c_refid, hitcount, 0, 1, 
				   GDT_TRUE, 1, 0, RecordList);
	  delete diagrec;
	  delete Records;
	} else
	  rs = new ZSEARCHRESPONSE(c_refid, hitcount, 0, 1, GDT_TRUE, 1,0,
				   NULL);
	
	return rs;
}

//
// After we've loaded all possible combinations of user variables from files
// and the command line, copy them from the generic REGISTRY structure
// into private class variables for ease of use.  If an attribute doesn't
// have a value, a hard-coded default is added.
//
void ZSERVER::StoreDefaults(const STRING & Group)
{
	//STRLIST Pos, ValList;
	STRING StrVal;

	c_defaults->ProfileGetString(Group, "Port", "210", &StrVal);
	c_port = StrVal.GetInt();

	c_defaults->ProfileGetString(Group, "DebugLevel", "1", &StrVal);
	c_debuglevel = StrVal.GetInt();

	c_defaults->ProfileGetString(Group, "ServerType", "STANDALONE", 
		&c_server_type);

	c_defaults->ProfileGetString(Group, "MaxSessions", "10", &StrVal);
	c_max_sessions = StrVal.GetInt();

	c_defaults->ProfileGetString(Group, "TimeOut", "3600", &StrVal);
	c_timeout = StrVal.GetInt();

	c_defaults->ProfileGetString(Group, "Trace", "OFF", &c_trace);
	
	c_defaults->ProfileGetString(Group, "TraceLog","/tmp/zserver_trace.log",
		&c_tracelog);

	c_defaults->ProfileGetString(Group, "AccessLog",
		"/tmp/zserver_access.log", &c_accesslog);

	c_defaults->ProfileGetString(Group, "ReverseNameLookup", "ON", 
		&c_reverse_name_lookup);

	c_defaults->ProfileGetString(Group, "SAPI", "sapi.ini", &c_sapifile);

	c_defaults->ProfileGetString(Group, "DiagSetId", "1.2.840.10003.3.1", 
		&c_diagsetid);

	c_defaults->ProfileGetString(Group, "iImpID", "34", &c_imp_id);
	c_defaults->ProfileGetString(Group, "iImpName", "CNIDR zserver", 
		&c_imp_name);
	c_defaults->ProfileGetString(Group, "iImpVersion", ZDIST_VERSION,
		&c_imp_version);

	c_defaults->ProfileGetString(Group, "iPreferredMsgSize", "32768", 
		&StrVal);
	c_preferred_msg_size = StrVal.GetInt();

	c_defaults->ProfileGetString(Group, "iMaxRecSize", "8388608", 
		&StrVal);
	c_max_rec_size = StrVal.GetInt();

}

ZPRESENTRESPONSE *ZSERVER::Present(ZPRESENTREQUEST & Request)
{
	ZRESPONSERECORDS *records;
	records = new ZRESPONSERECORDS();

	//
	// Version 3 conformance states we must recognize
	// AdditionalRanges.
	//
	if(Request.HasAdditionalRanges())
		return PresentResponseWithDiagnostic(BIB1_DIAG_OID, 243, "");
		
	//
	// What range of records being requested?
	INT4 start_point, num_requested;
	Request.GetResultSetStartPoint(&start_point);
	Request.GetNumberOfRecordsRequested(&num_requested);
	if((start_point + (num_requested - 1)) > c_hitcount)
	  num_requested = c_hitcount - start_point + 1;
 
	if(c_debuglevel >= 5) {
		cerr << "Client has asked for records " << start_point;
		cerr << " through " << (start_point + num_requested-1) << endl;
	}

// Patch for zserver returning wrong Present result -- John Wehle (john@feith.com)
	if (start_point < 1 || (start_point + num_requested - 1) >
	    ResultSet_GetHitCount(c_result_set))
	  return PresentResponseWithDiagnostic(BIB1_DIAG_OID, 13, "");
 
	INT i;
	CHR *record;
	INT4 rec_length;
	CHR *es_cname, *c_rec_syntax;
	STRING es_name, rec_syntax;

	//
	// Element Set Name?
	//
	// We must recognize COMPLEX type
	//
	if(Request.HasCompSpec())
		return PresentResponseWithDiagnostic(BIB1_DIAG_OID, 244, "");

	//
	// Default to "B"
	//
	Request.GetElementSetName(&es_name);
	if(es_name.Equals(""))
		es_name = "B";
	if(c_debuglevel >= 5) {
	  cerr << "Client has asked for element set name of ";
	  cerr << es_name << endl;
	}

	// Preferred Record Syntax?
	Request.GetPreferredRecordSyntax(&rec_syntax);
	if(c_debuglevel >= 5) {
		cerr << "Client has asked for record syntax of ";
		cerr << rec_syntax << endl;
	}

	// Default to SUTRS if client didn't specify
	if(rec_syntax.Equals(""))
		rec_syntax = SUTRS_OID;
	c_rec_syntax = rec_syntax.NewCString();

	STRING temprecord;
	STRING actual_rec_syntax = rec_syntax;
	for(i=start_point;i < (start_point+num_requested);i++) {
		es_cname = es_name.NewCString();
		ZNAMEPLUSRECORD *npr;
		// make sure we haven't exceeded the maxrecsize for complete
		// pdu
		record = (CHR *)ResultSet_GetRecord(c_result_set, i, 
			c_max_rec_size, &rec_length, (CHR *)es_cname, 
			(CHR *)c_rec_syntax, &actual_rec_syntax);
		if((!record)||(!*record)) {
			// Hmm.  If they asked for something other than
			// "B" or "F", lets try a default for them
			if((!es_name.CaseEquals("B"))&&
				(!es_name.CaseEquals("F"))) {
				delete [] es_cname;
				es_cname = new CHR[2];
				strcpy(es_cname, "F");
				record = (CHR *)ResultSet_GetRecord(
					c_result_set, i, c_max_rec_size, 
					&rec_length, (CHR *)es_cname, 
					(CHR *)c_rec_syntax,&actual_rec_syntax);
				if(!record) {
					// I give up
					record = new CHR[80];
					sprintf(record, 
					"Failed to retrieve record #%i", i);
				} else {
					// Should I warn them???
				}
			} else {
				record = new CHR[80];
				sprintf(record, "Failed to retrieve record #%i"
					, i);
			}
		}
	
		if(actual_rec_syntax.Equals(USMARC_OID)) {
			ZUSMARCRECORD *usm;
			temprecord = record;
			usm = new ZUSMARCRECORD(temprecord);
			npr = new ZNAMEPLUSRECORD(c_dbname, usm);
		} else {
			// Default to SUTRS (??)
			ZSUTRSRECORD *s;
			s = new ZSUTRSRECORD(record);
			npr = new ZNAMEPLUSRECORD(c_dbname, s);
		}
		records->AddRecord(npr);
		delete [] record;
		delete [] es_cname;
	}
	delete [] c_rec_syntax;
	return new ZPRESENTRESPONSE(c_refid, records->GetRecordCount(),
		(start_point + records->GetRecordCount()), 0, records);
}

KWAQS_STRING::KWAQS_STRING() : STRING()
{
	c_debuglevel = 0;
	c_errorcode = 0;
	c_addinfo = "";
}

GDT_BOOLEAN KWAQS_STRING::Convert(BERBROWSER *ber, ZSEARCHREQUEST & Request)
{
        if(c_debuglevel >= 5)
        	ber->HexDir();
 
        if(!ber->HasTag(QUERY_TAG)) {
                cerr << "PROTOCOL ERROR: QUERY_TAG not present" << endl;
                c_errorcode = 108;
                c_addinfo = "No query present in search request PDU!";
                return GDT_FALSE;
        }
 
        BERBROWSER sub = *ber;
        ber->GetSubdirectory(QUERY_TAG, &sub);
 
        // What type of query?
        INT query_type = -1;
        if(sub.HasTag(TYPE0_TAG))
                query_type = 0;
        if(sub.HasTag(TYPE1_TAG))
                query_type = 1;
        if(sub.HasTag(TYPE2_TAG))
                query_type = 2;
        if(sub.HasTag(TYPE100_TAG))
                query_type = 100;
        if(sub.HasTag(TYPE101_TAG))
                query_type = 101;
        if(sub.HasTag(TYPE102_TAG))
                query_type = 102;
 
        if(c_debuglevel >= 5)
                cerr << "Query type is " << query_type << endl;
 
        if(query_type == 0) {
                STRING query_string;
 
                sub.GetSubdirectory(TYPE0_TAG, &sub);
                ASN1TAGU tag(ASN1_GENERALSTRING);
                sub.GetChar(tag, &query_string);
 
                if(c_debuglevel >= 5) {
                        cerr << "Type 0 query string is \"";
                        cerr << query_string << "\"" << endl;
                }
		UCHR *t;
		t = (UCHR *)query_string.NewCString();
		Set(t, strlen((CHR *)t));
		delete [] t;
                return GDT_FALSE;
        } else if ((query_type == 1) || (query_type == 101)) {
                sub.GetSubdirectory(TYPE1_TAG, &sub);
 
                // Get the query-global attribute set id
                STRING attr_set;
                sub.GetOID(ATTRIBUTESETID_TAG, &attr_set);
                if(c_debuglevel >= 5)
                        cerr << "Attribute Set ID = " << attr_set << endl;
		if(!sub.HasTag(OP_TAG)) {
			// We only support a single operand query right now
                	c_errorcode = 3;
                	c_addinfo = "This database only supports a single term";
                	return GDT_FALSE;
		}
                
		sub.GetSubdirectory(OP_TAG, &sub);
		if(!sub.HasTag(ATTRIBUTESPLUSTERM_TAG)) {
			// We only support attrplusterm 
                	c_errorcode = 3;
                	c_addinfo = "DB only supports attributes plus term";
                	return GDT_FALSE;
		}
		sub.GetSubdirectory(ATTRIBUTESPLUSTERM_TAG, &sub);

		if(!sub.HasTag(TERMGENERAL_TAG)) {
			// We only support attrplusterm 
                	c_errorcode = 229;
                	c_addinfo = "Only general term type allowed";
                	return GDT_FALSE;
		}
		STRING query_string;
                sub.GetChar(TERMGENERAL_TAG, &query_string);

		// We now have the term.  We need to add the attribute list.
	
		// Is there an attribute list present?  We won't require one.
		if(!sub.HasTag(ATTRIBUTELIST_TAG)) {
			// Nope.  We're done.
			UCHR *t;
			t = (UCHR *)query_string.NewCString();
			Set(t, strlen((CHR *)t));
			delete [] t;
			return GDT_TRUE;
		}
		// Add the opening '['
		query_string.Cat("[");

		sub.GetSubdirectory(ATTRIBUTELIST_TAG, &sub);
        	BERBROWSER seq = sub;
		INT4 attr_count, attr_type, attr_value, i;
		CHR buf[32];
		attr_count = sub.GetSubdirectoryCount();

		for(i = 0;i < attr_count;i++) {
			sub.GetSubdirectoryX(i, &seq);	
			// We ignore per/term attribute set ids
			// Get the attribute type
			seq.GetInt(ATTRIBUTETYPE_TAG, &attr_type);
			// We only support numeric for attribute value
			if(seq.HasTag(NUMERIC_TAG)) {
				seq.GetInt(NUMERIC_TAG, &attr_value);
				// append these to the query term
				sprintf(buf, "%ld,%ld", 
					(long)attr_type, (long)attr_value);
				query_string.Cat(buf);
				if(i+1 < attr_count)
					query_string.Cat(",");	
			}
		}	
		query_string.Cat("]");
		UCHR *t;
		t = (UCHR *)query_string.NewCString();
		Set(t, strlen((CHR *)t));
		delete [] t;
	
		return GDT_TRUE;
        } else {
                // We don't support the requested query type
                c_errorcode = 107;
                return GDT_FALSE;
        }
	return GDT_TRUE;
}

GDT_BOOLEAN KWAQS_STRING::Error()
{
	if(c_errorcode == 0)
		return GDT_FALSE;
	return GDT_TRUE;
}

void KWAQS_STRING::GetErrorInfo(INT *ErrorCode, STRING *AddInfo)
{
	if(c_errorcode == 0)
		return;

	*AddInfo = c_addinfo;
	*ErrorCode = c_errorcode;
}

//
// Log file entry format (all on single line):
//
// host pid groupid/userid [datetime gmt_offset] method extra_data 
// protocol/version status bytes_sent
//
void ZSERVER::Log(CHR *Method, CHR *Extra, INT Status, INT4 BytesSent)
{
	INT fd;
	FILE *fp;
	CHR *logfile, *buf;
	STRING temp;
	pid_t pid;
	CHR TempString[128];

	pid = getpid();
	logfile = c_accesslog.NewCString();
	if((fp = fopen(logfile, "a")) == NULL) {
		delete [] logfile;
		perror(logfile);
		return;
	}
	fd = fileno(fp);

	// If another process has the file locked already, we'll block
	// here until the file is unlocked.
#if defined(_MSDOS) || defined(_WIN32)
	lseek(fd, SEEK_SET, 0);
	if (locking(fd, LK_LOCK, LONG_MAX) == -1) {
	  delete [] logfile;
	  perror(logfile);
	  fclose(fp);
	  return;
	}
	lseek(fd, SEEK_END, 0);
#else
	struct flock lock;
	lock.l_type = F_WRLCK;
	lock.l_start = 0;
	lock.l_whence = SEEK_CUR;
	lock.l_len = 0;

	if(fcntl(fd, F_SETLKW, &lock) == -1) {
		delete [] logfile;
		perror(logfile);
		fclose(fp);
		return;
	}
#endif

	// Hostname
	temp = c_client_hostname;

	// pid
	sprintf(TempString, " %ld ", pid);
	temp.Cat(TempString);

	// Z39.50 GroupId/UserId
	if(c_groupid.Equals(""))
		temp.Cat("-/");
	else {
		temp.Cat(c_groupid);
		temp.Cat("/");
	}
	if(c_userid.Equals(""))
		temp.Cat("-");
	else
		temp.Cat(c_userid);
	temp.Cat(" [");

	// Date and time
	struct tm *Time;
	time_t time_clock;

	time(&time_clock);
	Time = localtime(&time_clock);
	strftime(TempString, sizeof(TempString), "%d/%b/%Y:%H:%M:%S] ", Time);
	temp.Cat(TempString);

	// Method
	temp.Cat(Method);
	temp.Cat(" ");

	// Extra stuff
	temp.Cat(Extra);

	// Protocol/Version
	// Very peculiar to Z39.50 ProtocolVersion bitstring
	temp.Cat(" Z39.50/");
	sprintf(TempString, "%d ", c_protocol_version.GetLength());
	temp.Cat(TempString);

	// Status and BytesSent.  
	// Status doesn't really apply to us.  I'll make one up for now.
	// We'll say 1 means good, 0 bad:-)
	sprintf(TempString, "%d %d ", Status, BytesSent);
	temp.Cat(TempString);
	
	temp.Cat("\n");

	buf = temp.NewCString();
	write(fd, buf, strlen(buf));
	delete [] buf;

#if defined(_MSDOS) || defined(_WIN32)
	lseek(fd, SEEK_SET, 0);
	if (locking(fd, LK_UNLCK, LONG_MAX) == -1) {
	  delete [] logfile;
	  perror(logfile);
	  fclose(fp);
	  return;
	}
#else
	lock.l_type = F_UNLCK;
	if(fcntl(fd, F_SETLK, &lock) == -1) {
		delete [] logfile;
		perror(logfile);
		fclose(fp);
		return;
	}
#endif

	fclose(fp);
	delete [] logfile;
}

void ZSERVER::AddToMappingTable(const STRING &FileToAdd, 
	const STRING &DatabaseName)
{
	STRLIST FromPos, Position, ResultList, ToPos, TempList;

	// Build entry into mapping table for the database
	Position.Clear();
	Position.AddEntry(DatabaseName);

	REGISTRY *FromFile;
	FromPos.Clear();
	FromFile = new REGISTRY("blah");
	FromFile->ProfileLoadFromFile(FileToAdd, FromPos);
	FromPos.AddEntry("Default");
	FromFile->GetData(FromPos, &ResultList);

	// Now have a list of all mapping directives ("bib1/4")
	// Step through each one, build a position list, get the value
	// of that directive and SetData on the internal mapping table
	// for the current database.
	STRINGINDEX count, i;
	STRING Entry;
	count = ResultList.GetTotalEntries();
	if(c_debuglevel >= 8) {
		cerr << "Attempting to add " << count << " fields from ";
		cerr << FileToAdd;
		cerr << " to internal mapping tables..." << endl;
	}

	ToPos.AddEntry(DatabaseName);
	for(i=1;i <= count;i++) {
		ResultList.GetEntry(i, &Entry);
		FromPos.SetEntry(2, Entry);
		FromFile->GetData(FromPos, &TempList);
		ToPos.SetEntry(2, Entry);
		c_mapping_table->SetData(ToPos, TempList);
	}	
}

ZPRESENTRESPONSE *ZSERVER::PresentResponseWithDiagnostic(
	const STRING & DiagSetOID, const INT4 ErrorCode, 
	const STRING & AddInfo)
{
	ZPRESENTRESPONSE *rs;

	ZRECORDLIST *RecordList;
	RecordList = new ZRECORDLIST(NONSURROGATEDIAGNOSTIC_TAG);
	ZDEFAULTDIAGFORMAT *diagrec;
	diagrec = new ZDEFAULTDIAGFORMAT(ErrorCode, AddInfo, DiagSetOID);
	RecordList->AddRecord(diagrec);
	rs = new ZPRESENTRESPONSE(c_refid, 1, 1, 5, RecordList);
	delete diagrec;
	delete RecordList;
	return rs;
}
