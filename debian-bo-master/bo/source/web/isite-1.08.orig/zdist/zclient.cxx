#include <stdlib.h>
#include <locale.h>

#include "config.hxx"
#include "zclibase.hxx"
#include "marc.hxx"
  
#if defined(_WIN32)
#define CLEAR_SCREEN() system("cls")
#elif defined(_MSDOS)
#define CLEAR_SCREEN() cout << "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
#else
#  define CLEAR_SCREEN() system("clear")
#endif

#define DEBUGLEVEL 0
#define MAXHITS 100
void DisplayHits(INT4 TotalHits, ZRECORDLIST & HitList, INT4 Start, INT4 Count);

int main(int argc, char **argv)
{
	ZCLIENT_BASE client;

	if (!setlocale(LC_CTYPE,"")) {
	  cout << "Warning: Failed to set the locale!" << endl;
	}

	cout << "CNIDR zclient, Version " << ZDIST_VERSION;
	cout << ", Copyright (c) 1995,1996 MCNC/CNIDR" << endl << endl;

	if(argc < 5) {
		cout << "Usage: " << argv[0] << " host port database query";
		cout << " [record_syntax]" << endl << endl;
		exit(1);
	}
	
	client.SetDebugLevel(DEBUGLEVEL);

	if(!client.Initialize(argv[1], atoi(argv[2]))) {
		cout << "Failed to initialize" << endl;
		exit(1);
	}

	INT4 HitCount;
	if(!client.Search(argv[3], argv[4], &HitCount)) {
		cout << "Failed to search" << endl;
		exit(1);
	}

	if(HitCount == 0)
		exit(0);
	
	INT4 FetchCount;
	STRING recsyntax;
	if(argc > 5)
		recsyntax = argv[5];
	else {
		recsyntax = SUTRS_OID;
		if(HitCount <= MAXHITS)
			FetchCount = HitCount;
		else
			FetchCount = MAXHITS;
	}

	ZRECORDLIST HitList;
	if(!client.Present(1, FetchCount, recsyntax, "B", &HitList)) {
		cout << "Failed to present" << endl;
		exit(1);
	}

	if(HitList.GetRecordCount() <= 0) {
		cout << "No records returned!" << endl;
		exit(1);
	}

	DisplayHits(HitCount, HitList, 1, MAXHITS);

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
	CHR buf[80];
	for(;;) {
		RecordList.Clear();
		cout << endl << "Enter # of record to view (return to quit): ";
		cin.getline(buf, sizeof(buf));
		if(!buf[0])
			break;
		num = atoi(buf);
		Count = HitList.GetRecordCount();	
		if((num <= 0) || (num > Count))
			break;
		if(!client.Present(num, 1, SUTRS_OID, "F", &RecordList)){
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
		fprintf(fp, "CNIDR zclient, Version %s, Copyright (c) 1995,1996 MCNC/CNIDR\n\n", ZDIST_VERSION);
		fprintf(fp, "Record %d of %d\n\n", num, HitCount);
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
		DisplayHits(HitCount, HitList, 1, 10);
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
	cout << "CNIDR zclient, Version " << ZDIST_VERSION;
	cout << ", Copyright (c) 1995,1996 MCNC/CNIDR" << endl;
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
