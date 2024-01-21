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
File:		zserver.hxx
Version:	1.00
Description:	Z39.50 server
Author:		Kevin Gamiel, kevin.gamiel@cnidr.org
@@@*/

#ifndef _ZSERVER_H_
#define _ZSERVER_H_

#include <fstream.h>

#include "sapi.h"
#include "zsession.hxx"
#include "registry.hxx"

#define ZSERVER_NAME "CNIDR zserver"

class ZSERVER : public ZSESSION {
private:
	ofstream	c_output_file;
	INT		c_max_sessions,
			c_preferred_msg_size,
			c_max_rec_size,
			c_database_count,
	                c_hitcount;
	STRING		c_server_type,
			c_trace,
			c_tracelog,
			c_accesslog,
			c_sapifile,
			c_sapipath,
			c_diagsetid,
			c_imp_id,
			c_imp_name,
			c_imp_version,
			c_reverse_name_lookup,
			c_client_hostname,
			c_client_ipaddress,
			c_groupid,
			c_userid,
			c_password,
			c_default_db,
			c_inipath;
	ZINITREQUEST	*c_init_req;
	ZINITRESPONSE	*c_init_res;
	INT		c_forking;
	TCPSOCK		*c_server_tcp;
	PSAPI		c_sapi;
	PSAPI_DBASE	*c_dblist;
	PSAPI_RESULTSET	c_result_set;
	REGISTRY	*c_mapping_table;
	INT		c_argc;
	CHR		**c_argv;

	INT AcceptClient();
	INT Listen(UINT Port);
	void StartSession();
	ZSEARCHRESPONSE *Search(ZSEARCHREQUEST & Request);
	ZPRESENTRESPONSE *Present(ZPRESENTREQUEST & Request);
	void SetDefaults();
	void StoreDefaults(const STRING & Group);
#if !defined(_MSDOS) && !defined(_WIN32)
	INT StartForkingServer(UINT Port);
#endif
	INT StartServer(UINT Port);
	void Log(CHR *Method, CHR *Extra, INT Status, INT4 BytesSent);
	void ZSERVER::AddToMappingTable(const STRING &FileToAdd,
	       	const STRING &DatabaseName);
	void Init();
	ZPRESENTRESPONSE *PresentResponseWithDiagnostic(
        	const STRING & DiagSetOID, const INT4 ErrorCode, 
        	const STRING & AddInfo);
public:
	ZSERVER(int argc, char **argv);
	~ZSERVER();

	void DatabaseName(STRING *Value) { *Value = c_dbname; }
	void StartServer();
};

class KWAQS_STRING : public STRING {
private:
	INT	c_errorcode,
		c_debuglevel;
	STRING	c_addinfo;
public:
	KWAQS_STRING();
	GDT_BOOLEAN Error();
	void GetErrorInfo(INT *ErrorCode, STRING *AddInfo);
	GDT_BOOLEAN Convert(BERBROWSER *b, ZSEARCHREQUEST & Request);
	void SetDebugLevel(INT level) { c_debuglevel = level; }
};

#endif
