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
File:		zsession.cxx
Version:	1.00
Description:	ZSESSION, base class for ZCLIENT and ZSERVER
Author:		Kevin Gamiel, kevin.gamiel@cnidr.org
@@@*/

#include <string.h>
#include <stdlib.h>

#if defined(_MSDOS) || defined(_WIN32)
#  include <process.h>
#endif

#include "zsession.hxx"
#include "strlist.hxx"
#include "common.hxx"

extern int berutil_ByteOrder;

ZSESSION::ZSESSION()
{
	c_tcp = NULL;
	c_debuglevel = 1;
	c_state = closed;
	c_prefmsgsize = ZPREFMSGSIZE;
	c_maxrecordsize = ZMAXRECORDSIZE;
	c_protocol_version = ZPROTVER;

	c_refid = "";
	berutil_ByteOrder = IsBigEndian() ? 1:0;
	c_timeout = 3600;		// 1 hour inactivity timeout

	// Create a new REGISTRY object in memory that will hold all the
	// user-configurable data.
	c_defaults = new REGISTRY("zsession");

	ClearOptions();
	SetOptions(ZOPTIONS);
}

ZSESSION::~ZSESSION()
{
	Close();
}

INT ZSESSION::Close()
{
	if(c_state != closed) {
		ZCLOSE *pdu;

		pdu = new ZCLOSE("",0);
		SendPDU(pdu);
		c_tcp->Close();
		delete c_tcp;
		c_tcp = NULL;
		c_state = closed;
	}

	return 1;
}

//***************************************************************************
//
//      ReadPDU - Read a full Z39.50 PDU, in the form of a BER-encoded
//		character buffer, from the remote host.
//
//	PRE:	Network connection has been opened via Initialize().
//
//	POST:	Pointer to buffer of length len on success, NULL on failure.
//		On success, type contains the TAG number of the PDU.
//
//***************************************************************************
PUCHR ZSESSION::ReadPDU(INT4 *len, unsigned short *type)
{
	PUCHR b, t;
	CHR msg[512];
	INT err;
	UINT4 	max_buf_size,
		chunk_size = 1024,
		total = 0;
	INT4	AmountRead,
		ready_count,
		left = 0;

	*type = 0;
	*len = 0;

	max_buf_size = chunk_size;
	b = new UCHR[max_buf_size];

        for(;;) {
		if(c_debuglevel >= 5)
			cerr << "Waiting for data to arrive..." << endl;
                if((err = c_tcp->DataReady(c_timeout)) == 1) {
			ready_count = c_tcp->DataReadyCount();
			if(c_debuglevel >= 5) {
				cerr << ready_count;
				cerr << " bytes ready for reading..." << endl;
			}
			if(ready_count <= 0) {
				// peer closed connection
				if(c_debuglevel >= 1)
					cerr << "Peer closed connection" <<endl;
				c_tcp->Close();
				c_state = closed;
				delete [] b;
				return NULL;
			}
			if((total + ready_count) > max_buf_size) {
				while((total + ready_count) > max_buf_size)
					max_buf_size += chunk_size;
				if(c_debuglevel >= 5) {
					cerr << "growing buffer to ";
					cerr << max_buf_size << " bytes...";
					cerr << endl;
				}
				// grow the buffer
				t = new UCHR[max_buf_size];
				if(!t) {
					delete [] b;
					cerr << "out of memory" << endl;
					return NULL;
				}
				memcpy(t, b, total);
				delete [] b;
				b = t;
			}
				
                        AmountRead = c_tcp->Recv((CHR *)b+total, 
				(INT4)ready_count);
                        if((err = c_tcp->LastError())) {
                                if(err == UNI_UNCONNECTED)
                                        break;
                                c_tcp->ErrorMessage(err,msg,(sizeof(msg) - 1));
                                cerr << "ReadPDU:" << msg << endl;
				delete [] b;
				return NULL;
                        }
                        if(AmountRead > 0) {
                                total += AmountRead;
				if(c_debuglevel >= 5) {
					cerr << AmountRead;
					cerr << " bytes have been read...";						cerr << total << " total..." << endl;
				}
                        } else break;
                        if(IsCompleteBER(b, (INT4)total, &left))
                                break;
			if(c_debuglevel >= 5) {
				cerr << left << " bytes left in PDU...";
				cerr << endl;
			}

                } else {
			if(err == 0) {
				// We timed out waiting for a PDU
				if(c_timeout != 0) {
					cerr << "Read PDU Timeout has expired!" << endl;
					return NULL;
				}
				// Its OK, we're only polling
			} else {
				cerr << "Error on DataReady()" << endl;
			}
		}
        }
	*len = total;
	if(c_debuglevel >= 2)
		cerr << "Read complete PDU, " << total << " bytes." << endl;

	// Trim the buffer to the correct size
	t = new UCHR[total];
	if(!t) {
		delete [] b;
		cerr << "out of memory" << endl;
		return NULL;
	}
	memcpy(t, b, total);
	delete [] b;
	b = t;
	
	// Determine the type of PDU
	PDATA_DIR d;
	d = dalloc(15);
	if(!bld_dir(b, d)) {
		delete [] b;
		return 0;
	}
	if(c_debuglevel >= 8) {
	  cerr << "pid=" << getpid() << endl;
	  hex_dirf(d, 0, stderr);
	}
	*type = d->fldid;
	dfree(d);

#ifdef KAGDEBUG
	FILE *fp = fopen("/tmp/kagber.out", "w");
	if(fp) {
		fwrite(b, 1, total, fp);
		fclose(fp);
	}	
#endif
	// Return the BER-encoded buffer	
	return b;
}

//***************************************************************************
//
//      SendPDU - Send a Z39.50 PDU to the remote host
//
//	PRE:	pdu - A valid Z39.50 PDU.
//		Network connection has been opened via Initialize()
//
//	POST:	Number of bytes sent on success, 0 on failure.  
//		On failure, closes the socket.
//
//***************************************************************************
INT4 ZSESSION::SendPDU(PZPDU pdu)
{
	CHR msg[1024];
	INT err;
	INT4 BytesSent;

	if(c_debuglevel >= 8)
		pdu->HexDir();

	INT4 len;
        PUCHR t = pdu->GetRecord(&len);
	if(t == NULL) {
		cerr << "SendPDU:Error encoding PDU." << endl;
		return 0;
	}

	//
	// Send the PDU	
	//
	INT4 	Left = len,
		TotalSent = 0;
	for(;;) {	
		BytesSent = c_tcp->Send((CHR *)t+TotalSent, Left);
        	if((err = c_tcp->LastError())) {
                	c_tcp->ErrorMessage(err, msg, (sizeof(msg) - 1));
                	cerr << "SendPDU:" << msg << endl;
			c_tcp->Close();
			return 0;
        	}
		if(BytesSent != len) {
			TotalSent += BytesSent;
			Left = len - TotalSent;
		} else {
			TotalSent = BytesSent;
			break;
		}
	}
        free(t);

	if(c_debuglevel >= 2)
		cerr << "SendPDU: Sent PDU of " << TotalSent << " bytes.  berutil reported length of " << len << endl;

	return TotalSent;
}

void ZSESSION::LoadDefaultsFromFile(const STRING & Filename,
	const STRING & Group)
{
	STRLIST Position;

	c_defaults->ProfileLoadFromFile(Filename, Position);
}

//
// Processes command-line arguments in order and overrides internal settings
// for each valid option.  Note that a command-line option might refer to 
// a zserver.ini file (or more) and they are each completely processed in
// turn, overriding options as it goes.
//
void ZSESSION::LoadDefaultsFromCommandLine(int ac, char **argv, STRING & File,
	STRING & Group, STRING *LastGroup, STRING *LastFile)
{
	// If you encounter a -i, simply call LoadDefaultsFromFile with 
	// filename.
	// For each individual command-line option, change the value in 
	// c_defaults.
	int x;
	CHR *av;
	CHR *V;
	CHR *p;
	CHR fn[256];
	CHR group[256];
	CHR option[256];
	CHR setting[256];
	int argc = ac - 1;
	STRLIST Position, Setting;

	if (argc < 1)
		return;
	*LastGroup = Group;
	*LastFile = File;

	File.GetCString(fn, sizeof(fn));
	Group.GetCString(group, sizeof(group));

	for (x = 1; x <= argc; x++) {
		av = argv[x];
		if ( (av[0] != '-') || ( (av[1] != 'i') && (av[1] != 'o') ) ) {
			cerr << "Syntax error: " << av << endl;
			return;
		}

		V = new CHR[strlen(av+2)+1];
		strcpy(V, av+2);
		switch (av[1]) {
			case 'i':
				if (V[0] == '\0') {
					File.GetCString(fn, sizeof(fn));
					Group.GetCString(group, sizeof(group));
				} else {
					p = strchr(V, ',');
					if (p == NULL) {
						Group.GetCString(group,
							sizeof(group));
					} else {
						*p = '\0';
						*group = '*';
						strcpy(group+1, p+1);
					}
					strcpy(fn, V);
					if (fn[0] == '\0')
						File.GetCString(fn,sizeof(fn));
				}
				*LastGroup = group;
				*LastFile = fn;
				LoadDefaultsFromFile(fn, group);		
				break;
			case 'o':
				p = strchr(V, '=');
				if (p == NULL) {
					setting[0] = '\0';
				} else {
					*p = '\0';
					strcpy(setting, p+1);
				}
				strcpy(option, V);
				Position.Clear();
				Position.AddEntry(group);
				Position.AddEntry(option);
				Setting.Clear();
				Setting.AddEntry(setting);
				c_defaults->SetData(Position, Setting);
				break;
		} /* switch */
		delete [] V;
	}
}

void ZSESSION::SetOption(const INT Option, const GDT_BOOLEAN Value)
{
	CHR f;
	if(Value)
		f = 'y';
	else f = 'n';
	c_options.SetChr(Option, f);
}

GDT_BOOLEAN ZSESSION::GetOption(const INT Option)
{
	CHR f;
	f = c_options.GetChr(Option + 1);
	if((f == 'y') || (f == 'Y'))
		return GDT_TRUE;
	return GDT_FALSE;
}

void ZSESSION::ClearOptions()
{
	c_options = "nnnnnnnnnnnnnnnn";
}

#define YESORNO(a) if(a == 'Y') cout << "YES"; else cout << "NO"; cout << endl;

void ZSESSION::PrintConnectionDetails()
{
	CHR c;
	INT i, len=c_options.GetLength();

	c_options.UpperCase();
	cout << "Options:\t";
	for(i=0;i<len;i++) {
		c = c_options.GetChr(i+1);
		switch(i) {
			case 0:
				cout << "Search\t\t\t:";
				YESORNO(c);
				break;
			case 1:
				cout << "\t\tPresent\t\t\t:";
				YESORNO(c);
				break;
			case 2:
				cout << "\t\tDelete Result Set\t:";
				YESORNO(c);
				break;
			case 3:
				cout << "\t\tResource Report\t\t:";
				YESORNO(c);
				break;
			case 4:
				cout << "\t\tTrigger Resource Ctrl\t:";
				YESORNO(c);
				break;
			case 5:
				cout << "\t\tResource Ctrl\t\t:";
				YESORNO(c);
				break;
			case 6:
				cout << "\t\tAccess Ctrl\t\t:";
				YESORNO(c);
				break;
			case 7:
				cout << "\t\tScan\t\t\t:";
				YESORNO(c);
				break;
			case 8:
				cout << "\t\tSort\t\t\t:";
				YESORNO(c);
				break;
			case 9:
				cout << "\t\tReserved\t\t:";
				YESORNO(c);
				break;
			case 10:
				cout << "\t\tExtended Services\t:";
				YESORNO(c);
				break;
			case 11:
				cout << "\t\tLevel-1 Segmentation\t:";
				YESORNO(c);
				break;
			case 12:
				cout << "\t\tLevel-2 Segmentation\t:";
				YESORNO(c);
				break;
			case 13:
				cout << "\t\tConcurrent Operations\t:";
				YESORNO(c);
				break;
			case 14:
				cout << "\t\tNamed Result Sets\t:";
				YESORNO(c);
				break;
		}
	}
	INT v;
	GetProtocolVersion(&v);
	cout << "Version:\t" << v << endl;
	cout << "PrefMsgSize:\t" << c_prefmsgsize << endl;
	cout << "MaxMsgSize:\t" << c_maxrecordsize << endl;
}

void ZSESSION::GetProtocolVersion(INT *Version)
{
	CHR c;
	INT i, len=c_protocol_version.GetLength(), v=0;

	c_protocol_version.UpperCase();
	for(i=0;i<len;i++) {
		c = c_protocol_version.GetChr(i+1);
		if(c == 'Y')
			v++;
	}
	*Version = v;
}

void ZSESSION::SetOptions(const STRING & Options)
{
	ClearOptions();

	c_options = Options;

	CHR *t = Options.NewCString();
	STRINGINDEX l = Options.GetLength();

	STRINGINDEX i;
	for(i=0;i<l;i++)
		c_options.SetChr(i+1, t[i]);
}

