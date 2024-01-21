#include <stdlib.h>
#include <string.h>
#include <locale.h>

#include "config.hxx"
#include "zclibase.hxx"
#include "marc.hxx"

#if defined(_WIN32)
#define CLEAR_SCREEN() system("cls")
#elif defined(_MSDOS)
#define CLEAR_SCREEN() cout << "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
#else
#define CLEAR_SCREEN() system("clear")
#endif

void DisplayHits(INT4 TotalHits, ZRECORDLIST & HitList, INT4 Start, INT4 Count);

class IZCLIENT : public ZCLIENT_BASE {
	ZRECORDLIST	c_hitlist;
	INT4		c_display_start,
			c_display_count,
			c_hitcount;
	void DisplayName();	
	CHR c;
	STRING		c_host,
			c_database,
			c_term,
			c_esname,
			c_recsyntax;
	UINT		c_port;
	enum		{ CLOSED, INIT } c_state;
public:
	IZCLIENT();
	~IZCLIENT();

	GDT_BOOLEAN DisplayMainMenu();	
	void ReleaseNotes();
	GDT_BOOLEAN Connect();
	void Configure();
	virtual GDT_BOOLEAN Search();
	GDT_BOOLEAN Add_Record();
	GDT_BOOLEAN Delete_Record();
	INT GetState() { return c_state; }
	void ConnectionDetails();
};

void IZCLIENT::ConnectionDetails()
{
  CLEAR_SCREEN();
  DisplayName();

  cout << "Connection Details" << endl << endl;
  PrintConnectionDetails();
  cout << "Press Enter to Continue...";
  cin.get(c);
}

void IZCLIENT::ReleaseNotes()
{
	CLEAR_SCREEN();
	DisplayName();

	cout << "Release Notes" << endl << endl;

	cout << "This simple client is for programmer reference purposes only!";
	cout << endl;
	cout << "The intention is to provide guidelines for client developers.";
	cout << endl << endl;
	cout << "I'll gladly accept comments and suggestions." << endl;
	cout << endl;
	cout << "Kevin.Gamiel@cnidr.org" << endl << endl;
	cout << endl;
	cout << endl;
	cout << endl;
	cout << endl;
	cout << endl;
	cout << endl;
	cout << endl;
	cout << endl;
	cout << endl;
	cout << endl;
	cout << endl;
	cout << "Press Enter to Continue...";
	cin.get(c);
}

void IZCLIENT::Configure()
{
	CLEAR_SCREEN();
	DisplayName();
	cout << "Configure" << endl << endl;
	cout << "Enter Default Record Syntax [" << c_recsyntax << "]: ";
	CHR buf[64];
	cin.getline(buf, sizeof(buf));
	if(*buf == '\0') {
		if(c_recsyntax == "")
			c_recsyntax = USMARC_OID;
	} else c_recsyntax = buf;
	
}

GDT_BOOLEAN IZCLIENT::Search()
{
	CLEAR_SCREEN();
	DisplayName();
	cout << "Search" << endl << endl;
	cout << "Enter Database [" << c_database << "]: ";
	CHR buf[64];
	cin.getline(buf, sizeof(buf));
	if(*buf == '\0') {
		if(c_database == "")
			c_database = "xxdefault";
	} else c_database = buf;
	cout << "Enter Term [" << c_term << "]: ";
	cin.getline(buf, sizeof(buf));
	if(*buf == '\0') {
		if(c_term == "") 
			return GDT_FALSE;
	} else c_term = buf;
	if(!ZCLIENT_BASE::Search(c_database, c_term, &c_hitcount)) {
		c_hitcount = -1;
		return GDT_FALSE;
	}
	INT4 FetchCount;
	if(c_hitcount <= 5)
		FetchCount = c_hitcount;
	else
		FetchCount = 5;

	c_hitlist.Clear();
	if(!ZCLIENT_BASE::Present(1, FetchCount, c_recsyntax,"B", &c_hitlist)) {
		cout << "Failed to present" << endl;
		return GDT_FALSE;
	}

	if(c_hitlist.GetRecordCount() <= 0) {
		cout << "No records returned!" << endl;
		return GDT_FALSE;
	}

	DisplayHits(c_hitcount, c_hitlist, 1, 5);

	int num;
	ZRECORDLIST RecordList;
	INT4 Count;
	ZRECORDSCHOICE *Record;
	STRING Data, OID;
	CHR cmd[1024];
	CHR *TempFile = tmpnam(NULL);
	sprintf(cmd, "more %s", TempFile);
	FILE *fp;
	CHR c;
	for(;;) {
		RecordList.Clear();
		cout << endl << "Enter # of record to view (Return to exit): ";

		cin.getline(buf, sizeof(buf));
		if(!buf[0])
			break;
		num = atoi(buf);
		Count = c_hitlist.GetRecordCount();	
		if((num <= 0) || (num > Count))
			break;
		if(!ZCLIENT_BASE::Present(num, 1, c_recsyntax,"F", &RecordList)){
			cout << "Failed to present" << endl;
			exit(1);
		}
		Count = RecordList.GetRecordCount();	
		if(Count < 1) {
			cout << "No records returned!" << endl;
			exit(1);
		}
		Record = RecordList.GetRecord(0);
		if(!Record) {
			cout << "Record not available!" << endl;
			continue;
		}
		Record->GetOID(&OID);
		Record->GetRecordData(&Data);
		if((fp = fopen(TempFile, "w")) == NULL) {
			perror(TempFile);
			exit(1);
		}
		fprintf(fp, "CNIDR izclient, Version %s, Copyright (c) 1995,1996 MCNC/CNIDR\n", ZDIST_VERSION);
		fprintf(fp, "______________________________________________________________________________\n");
		fprintf(fp, "Record %d of %d\n\n", num, c_hitcount);
		if(OID == USMARC_OID) {
			MARC *m;
			m = new MARC(Data);
			m->Print(fp);
			delete m;
		} else {
			CHR *buf;
			buf = Data.NewCString();
			fwrite(buf, 1, Data.GetLength(), fp);
		}
		fclose(fp);
		CLEAR_SCREEN();
		
#if defined(_MSDOS) && !defined(_WIN32)
		cout << endl;
		cout << "Sorry...I can't view records in this enviroment!";
#else
		system(cmd);
#endif
		cout << endl << "Press return continue...";
		cin.get(c);
		CLEAR_SCREEN();
		DisplayHits(c_hitcount, c_hitlist, 1, 10);
	}
	return GDT_TRUE;
}

GDT_BOOLEAN IZCLIENT::Add_Record()
{
	CLEAR_SCREEN();
	DisplayName();
	cout << "Add Record" << endl << endl;
	cout << "Enter Database [" << c_database << "]: ";

	CHR buf[132];

	cin.getline(buf, sizeof(buf));
	if(*buf == '\0') {
		if(c_database == "")
			c_database = "xxdefault";
	} else c_database = buf;

	ZESUPDATERECORD *ur;
	ZEXTSINGLE *s;
	ZRECORDLIST RecordList(ES_TS_NOTTOKEEP_TAG);

	cout << "Enter data to index : ";

	cin.getline(buf, sizeof(buf));
	if(*buf == '\0')
		return GDT_FALSE;

	strcat(buf, "\n");

	s = new ZEXTSINGLE(SUTRS_OID, buf, GDT_FALSE);

	cout << "Enter the record id to associate with the data : ";

	cin.getline(buf, sizeof(buf));
        if(*buf == '\0')
                return GDT_FALSE;

	ur = new ZESUPDATERECORD(buf, s);
	RecordList.AddRecord(ur);

	if( !ZCLIENT_BASE::Add_Records(c_database, &RecordList) )
		return GDT_FALSE;

	return GDT_TRUE;
}

GDT_BOOLEAN IZCLIENT::Delete_Record()
{
	CLEAR_SCREEN();
	DisplayName();
	cout << "Delete Record" << endl << endl;
	cout << "Enter Database [" << c_database << "]: ";

	CHR buf[132];

	cin.getline(buf, sizeof(buf));
	if(*buf == '\0') {
		if(c_database == "")
			c_database = "xxdefault";
	} else c_database = buf;

	ZESUPDATERECORD *ur;
	ZEXTSINGLE *s;
	ZRECORDLIST RecordList(ES_TS_NOTTOKEEP_TAG);

	cout << "Enter the record id to delete : ";

	cin.getline(buf, sizeof(buf));
        if(*buf == '\0')
                return GDT_FALSE;

	s = new ZEXTSINGLE(SUTRS_OID, "", GDT_FALSE);
	ur = new ZESUPDATERECORD(buf, s);
	RecordList.AddRecord(ur);

	if( !ZCLIENT_BASE::Delete_Records(c_database, &RecordList) )
		return GDT_FALSE;

	return GDT_TRUE;
}

GDT_BOOLEAN IZCLIENT::Connect()
{
	CLEAR_SCREEN();
	DisplayName();

	cout << "Connect to Service" << endl << endl;
	cout << "Enter Host Name [localhost]: ";
	CHR buf[64];
	cin.getline(buf, sizeof(buf));
	if(*buf == '\0')
		strcpy(buf, "localhost");
	c_host = buf;
	cout << "Enter Port [210]: ";
	cin.getline(buf, sizeof(buf));
	if(*buf == '\0')
		strcpy(buf, "210");
	c_port = atoi(buf);

	if(!Initialize(c_host, c_port)) {
		cout << "Failed to initialize to " << c_host << "[";
		cout << c_port << "]" << endl;
		return GDT_FALSE;
	}
	c_state = INIT;
	return GDT_TRUE;
}

void IZCLIENT::DisplayName()
{
	cout << "CNIDR izclient " << ZDIST_VERSION;
	cout << ", Copyright (c) 1995,96 CNIDR";
	if(c_state != CLOSED) {
		cout << " - " << c_host;
		if(c_hitcount > -1)
			cout << " - " << c_hitcount << " hits";
	} else
		cout << " - No Connection";
	cout << endl;
	cout << "_____________________________________________________________________________";
	cout << endl << endl;
}

GDT_BOOLEAN IZCLIENT::DisplayMainMenu()
{
	CLEAR_SCREEN();
	DisplayName();
	
	cout << "\t" << "R\t\t- Release Notes" << endl << endl; 
	cout << "\t" << "C\t\t- Connect to Service" << endl << endl;
	cout << "\t" << "S\t\t- Search" << endl << endl;
	cout << "\t" << "A\t\t- Add Record" << endl << endl;
	cout << "\t" << "E\t\t- Erase Record" << endl << endl;
	cout << "\t" << "O\t\t- Options" << endl << endl;
	if(GetState() != CLOSED) {
		cout << "\t" << "D\t\t- Print Connection Details";
		cout << endl << endl;
	} else {
		cout << endl;
		cout << endl;
	}
	cout << "\t" << "Q\t\t- Quit" << endl << endl;
	cout << endl;
	cout << endl;
	cout << endl;
	cout << endl;
	
	cout << "Enter Selection: ";

	CHR buf[64];

	cin.getline(buf, sizeof(buf));

	switch(toupper(buf[0])) {
		case 'D':
			if(GetState() == CLOSED)
				break;
			ConnectionDetails();
			break;
		case 'R':
			ReleaseNotes();
			break;
		case 'O':
			Configure();
			break;
		case 'C':
			if(!Connect())
				return GDT_FALSE;
			break;
		case 'S':
			if(GetState() == CLOSED) {
				if(!Connect())
					return GDT_FALSE;
			}
			if(!Search())
				return GDT_FALSE;
			break;
		case 'A':
			if(GetState() == CLOSED) {
				if(!Connect())
					return GDT_FALSE;
			}
			if(!Add_Record())
				return GDT_FALSE;
			break;
		case 'E':
			if(GetState() == CLOSED) {
				if(!Connect())
					return GDT_FALSE;
			}
			if(!Delete_Record())
				return GDT_FALSE;
			break;
		case 'Q':
			return GDT_FALSE;
		default:
			return GDT_FALSE;
	}
	return GDT_TRUE;
}

IZCLIENT::IZCLIENT()
{
	c_display_start = 1;
	c_display_count = 8;
	c_state = CLOSED;	
	c_database = "xxdefault";
	c_term = "";
	c_hitcount = -1;
	c_recsyntax = USMARC_OID;
	c_esname = "B";
}

IZCLIENT::~IZCLIENT()
{
}

int main(int argc, char **argv)
{
  IZCLIENT client;

  if (!setlocale(LC_CTYPE,"")) {
    cout << "Warning: Failed to set the locale!" << endl;
  }

  for(;;) {
    if(!client.DisplayMainMenu())
      break;
  }
  
  return 0;
}

void DisplayHits(INT4 TotalHits, ZRECORDLIST & HitList, INT4 Start, INT4 Count)
{
	ZRECORDSCHOICE *Record;
	STRING Data, OID;
	INT4 i;
	if((Start+Count-1) > HitList.GetRecordCount())
		Count = HitList.GetRecordCount() - Start + 1;

	CLEAR_SCREEN();
	cout << "CNIDR izclient, Version " << ZDIST_VERSION;
	cout << ", Copyright (c) 1995,1996 MCNC/CNIDR" << endl;
	cout << "_____________________________________________________________________________";
	cout << endl;
	cout << TotalHits << " total records matched your query" << endl;
	cout << endl;
	for(i=Start-1;i < Count;i++) {
		Record = HitList.GetRecord(i);
		if(!Record)
			continue;
		Record->GetOID(&OID);
		Record->GetRecordData(&Data);
		cout << i+1 << ") ";
		if(OID == USMARC_OID) {
			MARC *m;
			m = new MARC(Data);
			m->SetDisplayFormat(MARC_FORMAT_TITLE);
			m->SetDisplayWidth(70);
			m->Print();
			delete m;
		} else
			cout << Data << endl;
	}
}
