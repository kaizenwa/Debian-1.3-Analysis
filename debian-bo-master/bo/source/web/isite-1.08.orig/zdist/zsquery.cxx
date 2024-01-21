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
File:		zsquery.cxx
Version:	1.00
Description:	Derived from Isearch's SQUERY with Z39.50-specific 
		functionality added.
Author:		Kevin Gamiel, kevin.gamiel@cnidr.org
@@@*/

#include "zsquery.hxx"

ZSQUERY::ZSQUERY()
{
	//c_mapping = new REGISTRY("mapping");
	c_errorcode = 0;
	c_addinfo = "";
	c_debuglevel = 0;
}

ZSQUERY::~ZSQUERY()
{
	//delete c_mapping;
}

//
// ber should point to the ENTIRE SEARCH REQUEST PDU, not just the Query
// subtree.
//
void ZSQUERY::ConvertQuery(BERBROWSER *ber, REGISTRY *Mapping,
	const STRLIST & AvailableFields, const STRING & DefaultDbName)
{
	BERBROWSER sub = *ber;

	c_available_fields = AvailableFields;

	// We are using a pointer only until Nassib implements the operator=
	// for REGISTRY class.
	c_mapping = Mapping;

	if(!ber->HasTag(QUERY_TAG)) {
		cerr << "PROTOCOL ERROR: QUERY_TAG not present" << endl;
		c_errorcode = 108;
		c_addinfo = "No query present in search request PDU!";
		return;
	}

	// Get the (single) database name
        if(ber->HasTag(DATABASENAMES_TAG)) {
		ber->GetSubdirectory(DATABASENAMES_TAG, &sub);
		sub.GetChar(DATABASENAME_TAG, &c_dbname);
		if(c_dbname.CaseEquals("xxdefault"))
			c_dbname = DefaultDbName;
	} else c_dbname = DefaultDbName;

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
		// We consider type 0 queries to be Isearch command line
		// syntax.  We expect the ANY to be a General String.
		STRING query_string;

		sub.GetSubdirectory(TYPE0_TAG, &sub);
		ASN1TAGU tag(ASN1_GENERALSTRING);
		sub.GetChar(tag, &query_string);

		if(c_debuglevel >= 5) {
			cerr << "Type 0 query string is \"";
			cerr << query_string << "\"" << endl;
		}

		// I'm not sure which Isearch method to call yet!
		SetRpnTerm(query_string);	
		return;
	} else if ((query_type == 1) || (query_type == 101)) {
	  if(query_type == 1)
	    sub.GetSubdirectory(TYPE1_TAG, &sub);
	  else
	    sub.GetSubdirectory(TYPE101_TAG, &sub);
 
		// Get the query-global attribute set id
		sub.GetOID(ATTRIBUTESETID_TAG, &c_attr_set);
		if(c_debuglevel >= 5)
			cerr << "Attribute Set ID = " << c_attr_set << endl;

		OPSTACK *stack;
		stack = new OPSTACK();
		// new line
		sub.GetSubdirectoryX(1, &sub);
		if(!BuildStack(&sub, stack)) {
			// error info has already been set.  Just return
			delete stack;
			return;
		}
		SetOpstack(*stack);
		delete stack;
	} else if (query_type == 102) {
		c_addinfo = "102";
		c_errorcode = 107;
	} else {
		// We don't supported the requested query type
		c_errorcode = 107;
		return;
	}
}

//
// You should always call this after creation of the object to see
// if the query conversion was successful.  If it returns true, call
// GetErrorInfo().
GDT_BOOLEAN ZSQUERY::Error()
{
	if(c_errorcode == 0)
		return GDT_FALSE;
	return GDT_TRUE;
}

// The error code returned will
// be a value from the Bib-1 Diagnostic set described
// in the Z39.50 standard.  The string value will either be empty or will
// be additional info for the diagnostic record.
//
void ZSQUERY::GetErrorInfo(INT *ErrorCode, STRING *AddInfo)
{
	if(c_errorcode == 0)
		return;

	*AddInfo = c_addinfo;
	*ErrorCode = c_errorcode;
}

//
// Converts from a Z39.50 type 1 or 101 query to an OPSTACK
//
GDT_BOOLEAN ZSQUERY::BuildStack(BERBROWSER *ber, OPSTACK *stack)
{
	BERBROWSER sub = *ber;
	STERM *term;

	ASN1TAG Tag;
	ber->GetTag(&Tag);

	// Is this a boolean query or a single term?
	if(Tag.Number == OP_TAG) {
	//if(ber->HasTag(OP_TAG)) {
		// Single term

		term = new STERM();

		// Is this operand:
		//
		//	1) attributes plus term 
		//	2) result set id
		//	3) result set plus attributes?
		//
		if(sub.HasTag(ATTRIBUTESPLUSTERM_TAG)) {
			if(c_debuglevel >= 5) {
				cerr << "Decoding AttributesPlusTerm." << endl;
			}

			STRING query_term;
			term = new STERM();
			sub.GetSubdirectory(ATTRIBUTESPLUSTERM_TAG, &sub);

			// What kind of term do we have
			if(!sub.HasTag(TERMGENERAL_TAG)) {
				c_errorcode = 229;
				c_addinfo = "Only GENERAL supported";
				return GDT_FALSE;
			}
			sub.GetChar(TERMGENERAL_TAG, &query_term);
			term->SetTerm(query_term);
			if(c_debuglevel >= 5) {
				cerr << "Term = \"" << query_term << "\"";
				cerr << endl;
			}
			
			// Perform mapping here
			if(sub.HasTag(ATTRIBUTELIST_TAG)) {
				if(c_debuglevel >= 5)
					cerr << "Decoding attr list" << endl;
				// We have an attribute list
				ATTRLIST *attrlist;
				attrlist = new ATTRLIST();
				INT4 count, i, Type, Value;
				BERBROWSER seq = sub;
				STRING MappedFieldName;
				sub.GetSubdirectory(ATTRIBUTELIST_TAG,
					&sub);
				count = sub.GetSubdirectoryCount();
				if(c_debuglevel >= 5)
					cerr << count << " attrs avail" << endl;
				STRING attrsetid;
				for(i=0;i < count;i++) {
					sub.GetSubdirectoryX(i, &seq);

					//
					// Is there a local attrsetid specified?
					//
					if(seq.HasTag(1))
						seq.GetOID(1, &attrsetid);
					else attrsetid = c_attr_set;

					//
					// Get attribute type
					//
					seq.GetInt(ATTRIBUTETYPE_TAG, &Type);	
					if(Type == 1) {
						if(c_debuglevel >= 5)
							cerr << "USE=";

						//
						// Is this a COMPLEX value?
						//
						if(seq.HasTag(224)) {
							c_errorcode=246;
							return GDT_FALSE;
						}

						//
						// Any value other than numeric,
						// return diagnostic.
						//
						if(!seq.HasTag(NUMERIC_TAG)) {
							c_errorcode=108;
							return GDT_FALSE;
						}

						seq.GetInt(
							NUMERIC_TAG, &Value);
						if(c_debuglevel >= 5)
							cerr << Value << endl;

						MapUseAttribute(attrsetid,
							c_dbname,
							Value,
							&MappedFieldName);
						//if(MappedFieldName.Equals(""))
							//return GDT_FALSE;
						//
						// We now have an Isearch
						// field name, but is that
						// field *actually* in the
						// database we're searching?
						//
if(!MappedFieldName.Equals("")) 
						if(!c_available_fields.SearchCase(MappedFieldName)) {
							CHR StrValue[64];
							c_errorcode=114;
				c_addinfo = "Unsupported Use Attribute: ";
							sprintf(StrValue, "%d",
								Value);
							c_addinfo.Cat(StrValue);
	
							return GDT_FALSE;
						}
						attrlist->AttrSetFieldName(
							MappedFieldName);
					} //else if(Type == x)...
				}
				if (count>0)
				  term->SetAttributes(*attrlist);
				
			}
			
			// Push the AttributesPlusTerm onto query stack
			*stack << *term;
			return GDT_TRUE;	
		} else if(sub.HasTag(RESULTSETID_TAG)) {
			// not supported
			c_errorcode = 100;
			c_addinfo = "Operand type of ResultSetID not supported";
			return GDT_FALSE;
		} else if(sub.HasTag(RESULTSETPLUSATTRIBUTES_TAG)) {
			// not supported
			c_errorcode = 245;
			c_addinfo = "ResultSetPlusAttributes not supported";
			return GDT_FALSE;
		} else {
			// protocol error
			c_errorcode = 100;
			c_addinfo = "Protocol error: no query Operand!";
			return GDT_FALSE;
		}
		
	} else if(Tag.Number == RPNRPNOP_TAG) {
		// boolean query
		OPERATOR *op;
		op = new OPERATOR();
		if(c_debuglevel >= 5) {
			cerr << "Decoding Boolean Query." << endl;
		}
		BERBROWSER rpn1 = sub;
		BERBROWSER rpn2 = sub;
		BERBROWSER Operator = sub;

		sub.GetSubdirectoryX(0, &rpn1);
		sub.GetSubdirectoryX(1, &rpn2);
		sub.GetSubdirectoryX(2, &Operator);

		// Push the operator onto query stack
		// Push the second operand onto query stack
		// Push the first operand onto query stack
		if(Operator.HasTag(AND_TAG)) {
			if(c_debuglevel >= 5)
				cerr << "Operator=AND" << endl;
			op->SetOperatorType(OperatorAnd);
		} else if(Operator.HasTag(OR_TAG)) {
			if(c_debuglevel >= 5)
				cerr << "Operator=OR" << endl;
			op->SetOperatorType(OperatorOr);
		} else if(Operator.HasTag(ANDNOT_TAG)) {
			if(c_debuglevel >= 5)
				cerr << "Operator=ANDNOT" << endl;
			op->SetOperatorType(OperatorAndNot);
		} else if(Operator.HasTag(PROX_TAG)) {
			if(c_debuglevel >= 5)
				cerr << "Operator=PROX" << endl;
			// We don't support this yet.
			c_errorcode = 129;
			c_addinfo = "Proximity not supported";
			return GDT_FALSE;
		} else {
			if(c_debuglevel >= 1) 
				cerr << "Unsupported operator in Boolean query" << endl;
			c_errorcode = 110;
			c_addinfo = "Unknown OPERATOR";
			return GDT_FALSE;
		}
	
		BuildStack(&rpn1, stack);
		BuildStack(&rpn2, stack);
		*stack << *op;
		delete op;
	} else {
		// bad query
		if(c_debuglevel >= 1) 
			cerr << "Protocol error in RPN structure" << endl;
		c_errorcode = 108;
		c_addinfo = "Protocol error in RPN query structure:1";
		return GDT_FALSE;
	}

	return GDT_TRUE;
}
void ZSQUERY::MapUseAttribute(const STRING & AttrSetID, const STRING & DbName,
	const INT4 Value, STRING *MappedValue)
{
	*MappedValue = "";

	//
	// Get the name of the attribute set entry in the field mapping table.  
	// For ease of use, we use the string "bib1" and "stas" in our field 
	// mapping tables instead of the oids.  Therefore, if the requested OID
	// is bib1 or stas, use the string instead of the oid.
	//
	STRING AttrSetPrefix;
	if(AttrSetID.CaseEquals(BIB1_ATTRSET_OID))
		AttrSetPrefix = "BIB1";
	else if(AttrSetID.CaseEquals(STAS_ATTRSET_OID))
		AttrSetPrefix = "STAS";
	else AttrSetPrefix = AttrSetID;

	//
	// Construct the directive from the AttributeSetID, a "/" and
	// the Use attribute value.
	//
	STRING Key;
	CHR StringValue[64];
	Key = AttrSetPrefix;
	Key.Cat("/");
	sprintf(StringValue, "%d", Value);
	Key.Cat(StringValue);

	//
	// We do a lookup into the mapping table for the database name/
	// key combination.
	//
	STRING TheMappedValue;
        STRLIST Position, Result;
        Position.Clear();
	Position.AddEntry(DbName);
	Position.AddEntry(Key);
        c_mapping->GetData(Position, &Result);
	if(c_debuglevel >= 5)
		cerr << "Mapping: AttrSetId=" << AttrSetID << endl;

	// Any mapping available?
	if(Result.GetTotalEntries() == 0) {
		if(c_debuglevel >= 5) {
			cerr << "Mapping: No mapping for '";
			cerr << Key << "'" << endl;
		}
		return;
	}

	Result.GetEntry(1, &TheMappedValue);
	
	if(c_debuglevel >= 5) {
		cerr << "Mapping: Converting field from '" << Key << "' to ";
		cerr << TheMappedValue << endl; 
	}

	*MappedValue = TheMappedValue;
}
