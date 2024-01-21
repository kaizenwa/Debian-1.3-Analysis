/************************************************************************
Copyright Notice

Copyright (c) MCNC, Clearinghouse for Networked Information Discovery and
Retrieval, 1994, 1995. 

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
File:		ber.hxx
Version:	1.00
Description:	Wrapper classes for OCLC berutils, BER record code
Author:		Kevin Gamiel, kevin.gamiel@cnidr.org
Note:		Portions of this code and code design were provided by Mike 
		Gursky of Ovid, Inc.  Thanks, Mike!
@@@*/
#ifndef _BER_H_
#define _BER_H_

#include <sys/types.h>
#if !defined(_MSDOS) && !defined(_WIN32)
#include <unistd.h>
#endif
#include "gdt.h"
#include "berutil.h"
#include "string.hxx"


//***************************************************************************
//
//	class ASN1TAG - ASN1 Tag
//
//	MEMBERS:
//		Number		tag number
//		Class		tag class
//		ASN1TAG()	constructor
//		==		equivalence operator
//
//***************************************************************************
//@ManMemo: My ASN class
class ASN1TAG {
public:
	enum { 	DEFAULT_TAG = -1,
		DEFAULT_CLASS = ASN1_CONTEXT };
	//@ManMemo: BER Tag Value
	UINT	Number;		// tag number
	//@ManMemo: BER Class
	CHR	Class;		// tag class
	//@ManMemo: The constructor
	ASN1TAG(UINT theNum = DEFAULT_TAG, CHR theClass = DEFAULT_CLASS) {
		Number = theNum;
		Class = theClass;
	}

	operator ==(const ASN1TAG& tag) const
	{
		return((Number == tag.Number) && (Class == tag.Class))?1:0;
	}
};

//***************************************************************************
//
//	class ASN1?TAG		- ASN1 Tag of specific class
//				- where ? is U,A,C,P for Universal, Application,
//					Context and Private, respectively.
//
//	MEMBERS:
//		ASN1?Tag()	constructor
//
//***************************************************************************
class ASN1TAGU : public ASN1TAG {
public:
	ASN1TAGU(UINT num) : ASN1TAG(num, ASN1_UNIVERSAL) {}
};

class ASN1TAGA : public ASN1TAG {
public:
	ASN1TAGA(UINT num) : ASN1TAG(num, ASN1_APPLICATION) {}
};

class ASN1TAGC : public ASN1TAG {
public:
	ASN1TAGC(UINT num) : ASN1TAG(num, ASN1_CONTEXT) {}
};

class ASN1TAGP : public ASN1TAG {
public:
	ASN1TAGP(UINT num) : ASN1TAG(num, ASN1_PRIVATE) {}
};


//***************************************************************************
//
//	class BERTREE - Base class for a single BER record. 
//
//	NOTES:
//		This class is related to the blk_dir data structure from
//		the OCLC berutils.  In the berutil model, the blk_dir
//		acts as a memory manager for a tree of dir_nodes.
//		BERTREE contains the root of a BER tree, which points
//		to the managing blk_dir.  Through the friend class BERBROWSER,
//		nodes are added, deleted and browsed.
//
//***************************************************************************
class BERTREE {
friend class BERBROWSER;
protected:
	void Init();
	PDATA_DIR 	c_dir;	// root of directory structure
	enum { NUMDIRS = 15 };	// default number of dir_nodes to allocate
	INT 		c_exit_on_error,
			c_ptrlist_count,
			c_ptrlist_maxcount;
	PUCHR		*c_ptrlist;
	PUCHR		c_buf;
public:
	BERTREE(const STRING & Filename);// Read ber from a file
	BERTREE(PUCHR buf, INT4 len);
	BERTREE(ASN1TAG tag);
	~BERTREE();
	void ExitOnError(INT YesOrNo) { c_exit_on_error = YesOrNo; }
	void Error(PCHR Msg);

	void AddPointer(PUCHR p);
	void DeletePointers();

	PUCHR GetRecord(INT4 *len) { return bld_rec(c_dir, len); }
	void GetRecordLength(INT4 *Length) { *Length = rec_len(c_dir); }

	void HexDir(FILE *fp) { hex_dirf(c_dir, 0, fp); fflush(fp);}
	void HexDir() { HexDir(stderr); }

	friend ostream & operator<<(ostream& os, BERTREE& b);
};
typedef BERTREE far *PBERTREE;

//***************************************************************************
//
//	class BERBROWSER
//
//***************************************************************************
typedef BERBROWSER far *PBERBROWSER;

class BERBROWSER {
//friend class BERTREE;
private:
	PBERTREE c_block;
	PDATA_DIR c_dirptr;
	
	PDATA_DIR FindTag(ASN1TAG Tag);
	void HTMLDump(DATA_DIR *d);
	void PrintHTMLNode(DATA_DIR *d);
public:
	// Constructors/Destructor
	BERBROWSER(PBERTREE Tree);
	BERBROWSER(const BERBROWSER & b);
	~BERBROWSER() {}

	void HexDir() { c_block->HexDir(); }
	void HTMLDump();

	// Directory operations
	INT GetParent(PBERBROWSER Parent);
	INT GetSubdirectory(ASN1TAG Tag, PBERBROWSER Subdir);
	INT GetSubdirectoryX(INT4 DirNum, PBERBROWSER Subdir);
	INT4 GetSubdirectoryCount() { return c_dirptr->count; }
	INT AddSubdirectory(ASN1TAG Tag, PBERBROWSER Newdir);
	INT AddSubdirectory(ASN1TAG Tag);
	//INT AddSubdirectory(PBERBROWSER Dir);
	INT HasTag(ASN1TAG Tag) { return FindTag(Tag)?1:0; }
	void GetTag(ASN1TAG *Tag) { *Tag = c_dirptr->fldid; }

	// Leaf operations
	INT AddNULL(ASN1TAG Tag);
	INT HasNULL(ASN1TAG Tag);

	INT AddChar(ASN1TAG Tag, const STRING& Data);
	INT GetChar(ASN1TAG Tag, PSTRING Data);

	INT AddBitString(ASN1TAG Tag, const STRING& Data);
	INT GetBitString(ASN1TAG Tag, STRING *Data);

	INT GetInt(ASN1TAG Tag, INT4 *Data);
	INT AddInt(ASN1TAG Tag, INT4 Data);

	INT AddOID(ASN1TAG Tag, const STRING& Data);
	INT GetOID(ASN1TAG Tag, PSTRING Data);
};

//
// Set of macros for making common method declarations easier
//
#define DD_HASTAG(Name,Tag)\
	inline INT Has##Name() const\
		{ return c_root->HasTag(Tag);};

#define DD_GET(Name,Tag,Type,Suffix)\
	inline INT Get##Name(Type *argValue) const\
		{ return c_root->Get##Suffix(Tag,argValue);};

#define DD_ADD(Name,Tag,Type,Suffix)\
	inline INT Add##Name(const Type & argValue) const\
		{ return c_root->Add##Suffix(Tag,argValue);};

#define DD_ADDONSTACK(Name,Tag,Type,Suffix)\
	inline INT Add##Name(Type argValue) const\
		{ return c_root->Add##Suffix(Tag,argValue);};

#define DD_ALL(Name,Tag,Type,Suffix)\
	DD_HASTAG(Name,Tag)\
	DD_GET(Name,Tag,Type,Suffix)\
	DD_ADD(Name,Tag,Type,Suffix)

#define DD_ALLONSTACK(Name,Tag,Type,Suffix)\
	DD_HASTAG(Name,Tag)\
	DD_GET(Name,Tag,Type,Suffix)\
	DD_ADDONSTACK(Name,Tag,Type,Suffix)

#define DD_BITSTRING(Name, Tag) DD_ALL(Name,(Tag),STRING,BitString)
#define DD_CHAR(Name, Tag) DD_ALL(Name,(Tag),STRING,Char)
#define DD_STRING(Name, Tag) DD_ALL(Name,(Tag),STRING,Char)
#define DD_INT(Name, Tag) DD_ALLONSTACK(Name,(Tag),INT4,Int)
#define DD_OID(Name, Tag) DD_ALLONSTACK(Name,(Tag),STRING,OID)
#define DD_BOOLEAN(Name, Tag) DD_ALLONSTACK(Name,(Tag),INT4,Int)
#define DD_SUBDIR(Name, Tag)\
	DD_HASTAG(Name,Tag)\
	inline INT Add##Name() const\
		{ return c_root->AddSubdirectory(Tag);};\
	inline INT Get##Name(PBERBROWSER & Dir) const\
		{ return c_root->GetSubdirectory(Tag,Dir);};
/*
	inline INT Add##Name(PBERBROWSER & Dir) const\
		{ return c_root->AddSubdirectory(Tag,Dir);};\
*/

#endif // _BER_H_

