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
File:		ber.cxx
Version:	1.00
Description:	C++ wrappers around OCLC berutils
Author:		Kevin Gamiel, kevin.gamiel@cnidr.org
@@@*/

#include <stdlib.h>
#include <iostream.h>
#include <string.h>
#include <ctype.h>
#include "ber.hxx"

#define MAX_PTR_COUNT 1024

void BERTREE::Init()
{
	c_exit_on_error = 0;
	c_ptrlist_count = 0;
	c_ptrlist_maxcount = MAX_PTR_COUNT;
	c_ptrlist = new PUCHR[c_ptrlist_maxcount];
	c_buf = (PUCHR)NULL;
}

BERTREE::BERTREE(const STRING & Filename)
{
	Init();
	c_dir = dalloc(15);

        size_t len;
        FILE *fp;
        CHR *fn = Filename.NewCString();
        if((fp = fopen(fn, "r")) == NULL) {
                perror(fn);
                exit(1);
        }
        delete [] fn;
        fseek(fp, 0, 2);
        len = ftell(fp);
        rewind(fp);

	c_buf = new UCHR[len];
	if(!c_buf) {
		Error("BERTREE::BERTREE(PUCHR buf)");
		return;
	}
        fread(c_buf, 1, len, fp);
        fclose(fp);
	if(!bld_dir(c_buf, c_dir))
		Error("BERTREE::BERTREE(PUCHR buf)");
}

BERTREE::BERTREE(PUCHR buf, INT4 len)
{
	Init();
	c_dir = dalloc(15);
	c_buf = new UCHR[len];
	if(!c_buf) {
		Error("BERTREE::BERTREE(PUCHR buf)");
		return;
	}
	memcpy(c_buf, buf, (int)len);
	if(!bld_dir(c_buf, c_dir))
		Error("BERTREE::BERTREE(PUCHR buf)");
}

BERTREE::BERTREE(ASN1TAG tag)
{ 
	Init();
	c_dir = dmake(tag.Number, tag.Class, NUMDIRS);
	if(!c_dir) {
		Error("BERTREE");
		return;
	}

}

/*
The berutils do not make copies of the character data.
Since I want to encapsulate all the data for ease of maintenance, I added
simple memory management to the BERTREE.  When you new a character buffer
from within a method for data to be added to the BERTREE (dir_node), pass
that new pointer to this method.  When the BERTREE is destroyed, it will
"garbage collect" this memory.
*/

void BERTREE::AddPointer(PUCHR p)
{
        if(c_ptrlist_count == c_ptrlist_maxcount) {
                // Grow the list
                c_ptrlist_maxcount += MAX_PTR_COUNT;
                PUCHR *newlist = new PUCHR[c_ptrlist_maxcount];
                if(!newlist) {
                        Error("AddPointer");
                        return;
                }
                memcpy(newlist, c_ptrlist, c_ptrlist_count);
                delete [] c_ptrlist;
                c_ptrlist = newlist;
        }
        c_ptrlist[c_ptrlist_count] = p;
        c_ptrlist_count++;
}
void BERTREE::DeletePointers()
{
	for(INT i=0;i<c_ptrlist_count;i++)
		delete [] c_ptrlist[i];
	delete [] c_ptrlist;
}

BERTREE::~BERTREE()
{
	DeletePointers();
	if(c_buf)
		delete [] c_buf;
	dfree(c_dir);
}

// Copy constructor
BERBROWSER::BERBROWSER(const BERBROWSER & b) 
{
	c_block = b.c_block;
	c_dirptr = b.c_dirptr;
}

BERBROWSER::BERBROWSER(PBERTREE Tree) 
{ 
	c_block = Tree; 
	c_dirptr = c_block->c_dir; 
}

void BERTREE::Error(PCHR Msg)
{
	cerr << "PROTOCOL ERROR: " << Msg << "\n";
	if(c_exit_on_error)
		exit(1);
}

void BERBROWSER::HTMLDump()
{
	cout << "<FORM METHOD=\"POST\" ACTION=\"/cgi-bin/zdl\">" << endl;
	HTMLDump(c_dirptr);
	cout << "<INPUT TYPE=\"SUBMIT\"></FORM>" << endl;
}

void BERBROWSER::HTMLDump(DATA_DIR *d)
{
	DATA_DIR *p;
	PrintHTMLNode(d);
	if(d->form == 1) {
		cout << endl << "<UL>" << endl;
		for(p=d->ptr.child;p;p = p->next) {
			// constructed
			cout << "<LI>";
			HTMLDump(p);
		} 
		cout << endl << "</UL>" << endl;
	} 
}

void BERBROWSER::PrintHTMLNode(DATA_DIR *d)
{
	static INT4 bc=0;

bc++;
	cout << "Tag=" << d->fldid;
	cout << "<A HREF=\"/misc/class.html\">Class=";
	switch(d->Class) {
		case 0:
			cout << "U</A>";
			break;
		case 1:
			cout << "A</A>";
			break;
		case 2:
			cout << "C</A>";
			break;
		case 3:
			cout << "P</A>";
			break;
		default:
			cout << "UNKNOWN</A>";
			break;
	}
	if(d->form == 0) {
		INT4 i, c=d->count;
		if(c > 20) {
			cout << ", Value=<TEXTAREA NAME=\"BLAH" << bc << "\"";
			cout << " ROWS=3 COLS=20>";
		} else
			cout << ", Value=<INPUT NAME=\"BLAH" << bc << "\" VALUE=\"";

		for(i=0;i<c;i++)
			if(isprint(d->ptr.data[i]))
				cout << d->ptr.data[i];
			else cout << '?';
			//cout << (isprint(d->ptr.data[i])?d->ptr.data[i]:'.');

		if(c > 20)
			cout << "</TEXTAREA>" << endl;
		else	cout << "\">";
	}
	cout << endl;
}

INT BERBROWSER::AddNULL(ASN1TAG Tag)
{
	return (daddchar(c_dirptr, Tag.Number, Tag.Class, NULL, 0)) ? 1:0;
}

INT BERBROWSER::AddInt(ASN1TAG Tag, INT4 Data)
{
	return daddnum(c_dirptr, Tag.Number, Tag.Class, (PUCHR)&Data, 
		sizeof(INT4)) ? 1:0;
}

INT BERBROWSER::GetInt(ASN1TAG Tag, INT4 *Data)
{
	PDATA_DIR d;

	if(d = FindTag(Tag)) {
		*Data = dgetnum(d);
		return 1;
	} else
		return 0;
}

INT BERBROWSER::AddBitString(ASN1TAG Tag, const STRING& Data)
{
	if(Data.GetLength() == 0)
		return 0;

	UCHR *b = (UCHR *)Data.NewCString();
	if(!daddbits(c_dirptr, Tag.Number, Tag.Class, (CHR *)b)) {
		c_block->Error("AddBitString");
		return 0;
	}
	c_block->AddPointer((PUCHR)b);

	return 1;
}

INT BERBROWSER::GetBitString(ASN1TAG Tag, STRING *Data)
{
	PDATA_DIR d;

	if(d = FindTag(Tag)) {
		CHR *b = dgetbits(d);
		if(!b) {
			c_block->Error("GetBitString");
			return 0;
		}
		STRING *TempString;
		TempString = new STRING((UCHR *)b, (STRINGINDEX)strlen(b));
		*Data = *TempString;
		delete TempString;
		return 1;
	} else
		return 0;
}

INT BERBROWSER::AddChar(ASN1TAG Tag, const STRING& Data)
{
	// This is a crucial part of our conversion to C++.
	// The berutils don't normally copy the character data
	// into their structures, only the pointer.  We are
	// responsible for allocating and freeing the data ourselves.
	// Therefore, I'll allocate the memory myself and then pass the
	// pointer to the new memory to my memory manager in the BERTREE
	// for this PDU.  When this BERTREE is destroyed, it will
	// "garbage collect" this memory for us.
	PUCHR t;
	INT4 len;

	len = Data.GetLength();
	if(len == 0) {
		return 0;
	}

	t = (UCHR *)Data.NewCString();
	if(!t) {
		c_block->Error("AddChar");
		return 0;
	}
	c_block->AddPointer(t);

	if(!daddchar(c_dirptr, Tag.Number, Tag.Class, t, len)) {
		c_block->Error("AddChar");
		return 0;
	}

	return 1;
}

//
// This version allocates a new buffer and copies the data.  User must free
// with delete [] ptr;
//
INT BERBROWSER::GetChar(ASN1TAG Tag, PSTRING Data)
{
	PDATA_DIR d;

	if(d = FindTag(Tag)) {
		PSTRING TempString;
		TempString = new STRING(d->ptr.data,(STRINGINDEX)d->count);
		*Data = *TempString;
		delete TempString;
		return 1;
	} else
		return 0;
}

INT BERBROWSER::GetParent(PBERBROWSER Parent)
{
	return 0;
}

INT BERBROWSER::GetSubdirectoryX(INT4 DirNum, PBERBROWSER Subdir)
{
	PDATA_DIR d;
	INT4 i;

	if((DirNum >= c_dirptr->count) || (c_dirptr->form != 1))
		return 0;

	d = c_dirptr->ptr.child;
	for(i=0;i < DirNum;i++)
		d = d->next;

	Subdir->c_block = c_block;
	Subdir->c_dirptr = d;
	return 1;
}

INT BERBROWSER::GetSubdirectory(ASN1TAG Tag, PBERBROWSER Subdir)
{
	PDATA_DIR d;

	if(d = FindTag(Tag)) {
		Subdir->c_block = c_block;
		Subdir->c_dirptr = d;
		return 1;
	} else {
		CHR msg[80];
		sprintf(msg, "GetSubdirectory: Unable to find tag %i", 
			Tag.Number);
		c_block->Error(msg);
		return 0;
	}
}

INT BERBROWSER::AddSubdirectory(ASN1TAG Tag, PBERBROWSER Newdir)
{
	PDATA_DIR d;

	if(d = daddtag(c_dirptr, Tag.Number, Tag.Class)) {
		Newdir->c_block = c_block;
		Newdir->c_dirptr = d;
		return 1;
	} else {
		c_block->Error("AddSubdirectory");
		return 0;
	}
}

INT BERBROWSER::AddSubdirectory(ASN1TAG Tag)
{
	PDATA_DIR d;

	if(d = daddtag(c_dirptr, Tag.Number, Tag.Class)) {
		return 1;
	} else {
		c_block->Error("AddSubdirectory");
		return 0;
	}
}

/*
Returns NULL or pointer to DATA_DIR node containing Tag
*/
PDATA_DIR BERBROWSER::FindTag(ASN1TAG Tag)
{

	if((c_dirptr == NULL) || (c_dirptr->ptr.child == NULL))
		return NULL;

	DATA_DIR *d=c_dirptr->ptr.child;
	INT4 i, count = c_dirptr->count;

	for(i=0;(i < count) && (d != NULL);i++) {
		if(d->fldid == Tag.Number)
			return d;
		d = d->next;
	}
	
	return NULL;
}

ostream& operator<<(ostream& os, BERTREE& b) {
	INT4 len;
	PUCHR t = b.GetRecord(&len);
	if(!t) {
		b.Error("operator<<");
		return os;
	}
        os.write(t, (int)len);
        free(t);

        return os;
}

INT BERBROWSER::AddOID(ASN1TAG Tag, const STRING& Data)
{
	if(Data.GetLength() == 0)
		return 0;

	CHR *b = (CHR *)Data.NewCString();
	if(!daddoid(c_dirptr, Tag.Number, Tag.Class, b)) {
//		c_block->Error("AddOID");
//		return 0;
	  CHR msg[256];
	  sprintf(msg, "AddOID: %s", b);
	  delete [] b;
	  c_block->Error(msg);
	  return 0;
	}
	c_block->AddPointer((PUCHR)b);

	return 1;
}

INT BERBROWSER::GetOID(ASN1TAG Tag, PSTRING Data)
{
	PDATA_DIR d;

	if(d = FindTag(Tag)) {
		PCHR b = dgetoid(d);
		if(!b) {
			c_block->Error("GetOID");
			return 0;
		}
		PSTRING TempString;
		TempString = new STRING((UCHR *)b, (STRINGINDEX)strlen(b));
		*Data = *TempString;
		delete TempString;
		return 1;
	} else
		return 0;
}

