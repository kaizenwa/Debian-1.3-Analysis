static const char rcsid[] = "$Id: be_gen_isl.cc,v 2.8 1996/04/30 06:33:07 janssen Exp $";

/*
 *======================================================================
 *
 * Copyright (c) 1991, 1992, 1993, 1994 Xerox Corporation.  All Rights Reserved.
 *
 * Unlimited use, reproduction, and distribution of this software is
 * permitted.  Any copy of this software must include both the above
 * copyright notice of Xerox Corporation and this paragraph.  Any
 * distribution of this software must comply with all applicable United
 * States export control laws.  This software is made available AS IS,
 * and XEROX CORPORATION DISCLAIMS ALL WARRANTIES, EXPRESS OR IMPLIED,
 * INCLUDING WITHOUT LIMITATION THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE, AND NOTWITHSTANDING ANY OTHER
 * PROVISION CONTAINED HEREIN, ANY LIABILITY FOR DAMAGES RESULTING FROM
 * THE SOFTWARE OR ITS USE IS EXPRESSLY DISCLAIMED, WHETHER ARISING IN
 * CONTRACT, TORT (INCLUDING NEGLIGENCE) OR STRICT LIABILITY, EVEN IF
 * XEROX CORPORATION IS ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
 *
 *======================================================================
 */

#include	<stream.h>
#include	<idl.hh>
#include	<idl_extern.hh>
#include	<be.hh>

static const char anonymousTypeFormat[]		= "AnonType-%d-";

static const char attributeGenericPrefix[]	= "ilu--prefix-idlAttribute-";
static const char attributeGetPrefix[]		= "-get-";
static const char attributeSetPrefix[]		= "-set-";
static const char qualifiedNameSeparator[]	= "-";
static const char repIDSeparator[]		= "/";

static void
bePrefixMessage()
{
	cerr << idl_global->prog_name() << ": \"" <<
		idl_global->main_filename()->get_string() << "\": ";
}

static void
beWarning(char *message)
{
	bePrefixMessage();
	cerr << "warning: " << message << "\n";
}

static void
beError(char *message)
{
	beGlobals.nErrors += 1;
	bePrefixMessage();
	cerr << "error: " << message << "\n";
}

static void
beInternalError(char *message)
{
	bePrefixMessage();
	cerr << "BE internal error: " << message << "\n";
	exit(1);
}

//////////////////////////////////////////////////////////////////////////////

class BE_Imports {
public:
  BE_Imports();

  void add_import(AST_Decl *d);
  void gen_isl(ostream &o);

private:
  struct Node
  {
    Node *next;
    char *interfaceName;
    char *fileName;
  };
  Node *list;
};

class nullbuf : public streambuf
{
public:
	nullbuf();
	int overflow(int ch = EOF);
};

//////////////////////////////////////////////////////////////////////////////

// Send "pregen_types" message to declaration "d".
static void
pregenTypesForDecl(AST_Decl *d, ostream &o)
{
	switch (d->node_type())
	{
	case AST_Decl::NT_module:
		be_module::narrow_from_decl(d)->pregen_types(o);
		break;

	case AST_Decl::NT_interface:
		be_interface::narrow_from_decl(d)->pregen_types(o);
		break;

	case AST_Decl::NT_interface_fwd:
		be_interface_fwd::narrow_from_decl(d)->pregen_types(o);
		break;

	case AST_Decl::NT_const:
		be_constant::narrow_from_decl(d)->pregen_types(o);
		break;

	case AST_Decl::NT_except:
		be_exception::narrow_from_decl(d)->pregen_types(o);
		break;

	case AST_Decl::NT_attr:
		be_attribute::narrow_from_decl(d)->pregen_types(o);
		break;

	case AST_Decl::NT_op:
		be_operation::narrow_from_decl(d)->pregen_types(o);
		break;

	case AST_Decl::NT_argument:
		be_argument::narrow_from_decl(d)->pregen_types(o);
		break;

	case AST_Decl::NT_union:
		be_union::narrow_from_decl(d)->pregen_types(o);
		break;

	case AST_Decl::NT_union_branch:
		be_union_branch::narrow_from_decl(d)->pregen_types(o);
		break;

	case AST_Decl::NT_struct:
		be_structure::narrow_from_decl(d)->pregen_types(o);
		break;

	case AST_Decl::NT_field:
		be_field::narrow_from_decl(d)->pregen_types(o);
		break;

	case AST_Decl::NT_enum:
		be_enum::narrow_from_decl(d)->pregen_types(o);
		break;

	case AST_Decl::NT_enum_val:
		be_enum_val::narrow_from_decl(d)->pregen_types(o);
		break;

	case AST_Decl::NT_string:
		be_string::narrow_from_decl(d)->pregen_types(o);
		break;

	case AST_Decl::NT_array:
		be_array::narrow_from_decl(d)->pregen_types(o);
		break;

	case AST_Decl::NT_sequence:
		be_sequence::narrow_from_decl(d)->pregen_types(o);
		break;

	case AST_Decl::NT_typedef:
		be_typedef::narrow_from_decl(d)->pregen_types(o);
		break;

	case AST_Decl::NT_pre_defined:
		be_predefined_type::narrow_from_decl(d)->pregen_types(o);
		break;

	default:
		cerr << "node type #" << d->node_type() << '\n';
		beInternalError("pregen: unknown node type");
		break;
	}
}

// Send "gen_isl" message to declaration "d".
static void
genIslForDecl(AST_Decl *d, ostream &o)
{
	switch (d->node_type())
	{
	case AST_Decl::NT_module:
		be_module::narrow_from_decl(d)->gen_isl(o);
		break;

	case AST_Decl::NT_interface:
		be_interface::narrow_from_decl(d)->gen_isl(o);
		break;

	case AST_Decl::NT_interface_fwd:
		be_interface_fwd::narrow_from_decl(d)->gen_isl(o);
		break;

	case AST_Decl::NT_const:
		be_constant::narrow_from_decl(d)->gen_isl(o);
		break;

	case AST_Decl::NT_except:
		be_exception::narrow_from_decl(d)->gen_isl(o);
		break;

	case AST_Decl::NT_attr:
		be_attribute::narrow_from_decl(d)->gen_isl(o);
		break;

	case AST_Decl::NT_op:
		be_operation::narrow_from_decl(d)->gen_isl(o);
		break;

	case AST_Decl::NT_argument:
		be_argument::narrow_from_decl(d)->gen_isl(o);
		break;

	case AST_Decl::NT_union:
		be_union::narrow_from_decl(d)->gen_isl(o);
		break;

	case AST_Decl::NT_union_branch:
		be_union_branch::narrow_from_decl(d)->gen_isl(o);
		break;

	case AST_Decl::NT_struct:
		be_structure::narrow_from_decl(d)->gen_isl(o);
		break;

	case AST_Decl::NT_field:
		be_field::narrow_from_decl(d)->gen_isl(o);
		break;

	case AST_Decl::NT_enum:
		be_enum::narrow_from_decl(d)->gen_isl(o);
		break;

	case AST_Decl::NT_enum_val:
		be_enum_val::narrow_from_decl(d)->gen_isl(o);
		break;

	case AST_Decl::NT_string:
		be_string::narrow_from_decl(d)->gen_isl(o);
		break;

	case AST_Decl::NT_array:
		be_array::narrow_from_decl(d)->gen_isl(o);
		break;

	case AST_Decl::NT_sequence:
		be_sequence::narrow_from_decl(d)->gen_isl(o);
		break;

	case AST_Decl::NT_typedef:
		be_typedef::narrow_from_decl(d)->gen_isl(o);
		break;

	case AST_Decl::NT_pre_defined:
		be_predefined_type::narrow_from_decl(d)->gen_isl(o);
		break;

	default:
		cerr << "node type #" << d->node_type() << '\n';
		beInternalError("gen: unknown node type");
		break;
	}
}

// Send "gen_isl_typedef" message to AST_Type "t".
static idl_bool
genIslTypedefForType(AST_Decl *t, ostream &o, AST_Decl *td)
{
	switch (t->node_type())
	{
	case AST_Decl::NT_interface:
		return be_interface::narrow_from_decl(t)->
			gen_isl_typedef(o, td);

	case AST_Decl::NT_interface_fwd:
		return be_interface_fwd::narrow_from_decl(t)->
			gen_isl_typedef(o, td);

	case AST_Decl::NT_union:
		return be_union::narrow_from_decl(t)->gen_isl_typedef(o, td);

	case AST_Decl::NT_struct:
		return be_structure::narrow_from_decl(t)->
			gen_isl_typedef(o, td);

	case AST_Decl::NT_enum:
		return be_enum::narrow_from_decl(t)->gen_isl_typedef(o, td);

	case AST_Decl::NT_string:
		return be_string::narrow_from_decl(t)->gen_isl_typedef(o, td);

	case AST_Decl::NT_array:
		return be_array::narrow_from_decl(t)->gen_isl_typedef(o, td);

	case AST_Decl::NT_sequence:
		return be_sequence::narrow_from_decl(t)->gen_isl_typedef(o, td);

	case AST_Decl::NT_typedef:
		return be_typedef::narrow_from_decl(t)->gen_isl_typedef(o, td);

	case AST_Decl::NT_pre_defined:
		return be_predefined_type::narrow_from_decl(t)->
			gen_isl_typedef(o, td);

	default:
		cerr << "node type #" << t->node_type() << '\n';
		beInternalError("genTypedef: unknown node type");
		return I_FALSE;
	}
}

// Send "gen_type_name" message to AST_Type "t".
static void
genTypeNameForType(AST_Decl *t, ostream &o)
{
	switch (t->node_type())
	{
	case AST_Decl::NT_interface:
		be_interface::narrow_from_decl(t)->gen_type_name(o);
		break;

	case AST_Decl::NT_interface_fwd:
		be_interface_fwd::narrow_from_decl(t)->gen_type_name(o);
		break;

	case AST_Decl::NT_union:
		be_union::narrow_from_decl(t)->gen_type_name(o);
		break;

	case AST_Decl::NT_struct:
		be_structure::narrow_from_decl(t)->gen_type_name(o);
		break;

	case AST_Decl::NT_enum:
		be_enum::narrow_from_decl(t)->gen_type_name(o);
		break;

	case AST_Decl::NT_string:
		be_string::narrow_from_decl(t)->gen_type_name(o);
		break;

	case AST_Decl::NT_array:
		be_array::narrow_from_decl(t)->gen_type_name(o);
		break;

	case AST_Decl::NT_sequence:
		be_sequence::narrow_from_decl(t)->gen_type_name(o);
		break;

	case AST_Decl::NT_typedef:
		be_typedef::narrow_from_decl(t)->gen_type_name(o);
		break;

	case AST_Decl::NT_pre_defined:
		be_predefined_type::narrow_from_decl(t)->gen_type_name(o);
		break;

	default:
		cerr << "node type #" << t->node_type() << '\n';
		beInternalError("genTypeName: unknown node type");
		break;
	}
}

// Send "be_dump" message to declaration "d".
static void
dumpDecl(ostream &o, int level, AST_Decl *d)
{
	switch (d->node_type())
	{
	case AST_Decl::NT_module:
		be_module::narrow_from_decl(d)->be_dump(o, level);
		break;

	case AST_Decl::NT_interface:
		be_interface::narrow_from_decl(d)->be_dump(o, level);
		break;

	case AST_Decl::NT_interface_fwd:
		be_interface_fwd::narrow_from_decl(d)->be_dump(o, level);
		break;

	case AST_Decl::NT_const:
		be_constant::narrow_from_decl(d)->be_dump(o, level);
		break;

	case AST_Decl::NT_except:
		be_exception::narrow_from_decl(d)->be_dump(o, level);
		break;

	case AST_Decl::NT_attr:
		be_attribute::narrow_from_decl(d)->be_dump(o, level);
		break;

	case AST_Decl::NT_op:
		be_operation::narrow_from_decl(d)->be_dump(o, level);
		break;

	case AST_Decl::NT_argument:
		be_argument::narrow_from_decl(d)->be_dump(o, level);
		break;

	case AST_Decl::NT_union:
		be_union::narrow_from_decl(d)->be_dump(o, level);
		break;

	case AST_Decl::NT_union_branch:
		be_union_branch::narrow_from_decl(d)->be_dump(o, level);
		break;

	case AST_Decl::NT_struct:
		be_structure::narrow_from_decl(d)->be_dump(o, level);
		break;

	case AST_Decl::NT_field:
		be_field::narrow_from_decl(d)->be_dump(o, level);
		break;

	case AST_Decl::NT_enum:
		be_enum::narrow_from_decl(d)->be_dump(o, level);
		break;

	case AST_Decl::NT_enum_val:
		be_enum_val::narrow_from_decl(d)->be_dump(o, level);
		break;

	case AST_Decl::NT_string:
		be_string::narrow_from_decl(d)->be_dump(o, level);
		break;

	case AST_Decl::NT_array:
		be_array::narrow_from_decl(d)->be_dump(o, level);
		break;

	case AST_Decl::NT_sequence:
		be_sequence::narrow_from_decl(d)->be_dump(o, level);
		break;

	case AST_Decl::NT_typedef:
		be_typedef::narrow_from_decl(d)->be_dump(o, level);
		break;

	case AST_Decl::NT_pre_defined:
		be_predefined_type::narrow_from_decl(d)->be_dump(o, level);
		break;

	default:
		cerr << "node type #" << d->node_type() << '\n';
		beInternalError("dump: unknown node type");
		break;
	}
}

//////////////////////////////////////////////////////////////////////////////

static void
indent(ostream &o, int level)
{
	for (int i = 0; i < level; i++)
		o << "    ";
}

static void
dumpHeader(ostream &o, int level, AST_Decl *d, char *heading)
{
	indent(o, level);
	o << heading << " " << d->local_name()->get_string() << "\n";
}

static void
dumpScope(ostream &o, int level, UTL_Scope *s)
{
	indent(o, level);
	o << "-- locals --\n";
	UTL_ScopeActiveIterator localIterator(s, UTL_Scope::IK_localtypes);

	while (!localIterator.is_done())
	{
		dumpDecl(o, level, localIterator.item());
		localIterator.next();
	}

	indent(o, level);
	o << "-- decls --\n";
	UTL_ScopeActiveIterator iterator(s, UTL_Scope::IK_decls);

	while (!iterator.is_done())
	{
		dumpDecl(o, level, iterator.item());
		iterator.next();
	}
}

//////////////////////////////////////////////////////////////////////////////

static void
pregenTypesForScope(UTL_Scope *s, ostream &o)
{
	UTL_ScopeActiveIterator iterator(s, UTL_Scope::IK_decls);

	while (!iterator.is_done())
	{
		pregenTypesForDecl(iterator.item(), o);
		iterator.next();
	}
}


typedef unsigned long	NodeTypeFilter;

#define Singleton(nt)	((unsigned long) 1 << (int) (nt))

static const NodeTypeFilter	NTF_enum_val	=
	Singleton(AST_Decl::NT_enum_val);
static const NodeTypeFilter	NTF_attr_and_op	=
	Singleton(AST_Decl::NT_attr) | Singleton(AST_Decl::NT_op);
static const NodeTypeFilter	NTF_normal	=
	~ (NTF_enum_val | NTF_attr_and_op);

static void
genIslForScope(UTL_Scope *s, ostream &o,
	char *prefix = "", char *separator = "", char *suffix = "",
	NodeTypeFilter filter = NTF_normal,
	UTL_Scope::ScopeIterationKind iterationKind = UTL_Scope::IK_decls)
{
	UTL_ScopeActiveIterator iterator(s, iterationKind);
	int count = 0;

	while (!iterator.is_done())
	{
		AST_Decl *d = iterator.item();

		if ((filter & Singleton(d->node_type())) != 0 &&
		    (!beGlobals.opts.useImports || d->in_main_file()))
		{
			o << (count == 0 ? prefix : separator);
			genIslForDecl(d, o);
			count += 1;
		}
		iterator.next();
	}
	if (count > 0)
		o << suffix;
}

static void
genIslForLocalTypesInScope(UTL_Scope *s, ostream &o)
{
	genIslForScope(s, o, "", "", "", NTF_normal, UTL_Scope::IK_localtypes);
}

//////////////////////////////////////////////////////////////////////////////

enum IslBaseType
{
	Isl_null,
	Isl_INTEGER,
	Isl_SHORT_INTEGER,
	Isl_CARDINAL,
	Isl_SHORT_CARDINAL,
	Isl_BYTE,
	Isl_BOOLEAN,
	Isl_REAL,
	Isl_SHORT_REAL,
	Isl_SHORT_CHARACTER,
	Isl_ilu_CString
};

static char ilu_CString_spelling[]	= "ilu.CString";

static char *
islBaseTypeName(IslBaseType ibt)
{
	switch (ibt)
	{
	case Isl_INTEGER:		return "INTEGER";
	case Isl_SHORT_INTEGER:		return "SHORT INTEGER";
	case Isl_CARDINAL:		return "CARDINAL";
	case Isl_SHORT_CARDINAL:	return "SHORT CARDINAL";
	case Isl_BYTE:			return "BYTE";
	case Isl_BOOLEAN:		return "BOOLEAN";
	case Isl_REAL:			return "REAL";
	case Isl_SHORT_REAL:		return "SHORT REAL";
	case Isl_SHORT_CHARACTER:	return "SHORT CHARACTER";
	case Isl_ilu_CString:		return ilu_CString_spelling;
	default:			return "?";
	}
}

static IslBaseType
expressionTypeToIslType(AST_Expression::ExprType et)
{
	switch (et)
	{
	case AST_Expression::EV_short:		return Isl_SHORT_INTEGER;
	case AST_Expression::EV_ushort:		return Isl_SHORT_CARDINAL;
	case AST_Expression::EV_long:		return Isl_INTEGER;
	case AST_Expression::EV_ulong:		return Isl_CARDINAL;
	case AST_Expression::EV_float:		return Isl_SHORT_REAL;
	case AST_Expression::EV_double:		return Isl_REAL;
	case AST_Expression::EV_char:		return Isl_SHORT_CHARACTER;
	case AST_Expression::EV_octet:		return Isl_BYTE;
	case AST_Expression::EV_bool:		return Isl_BOOLEAN;
	case AST_Expression::EV_string:		return Isl_ilu_CString;
	}
	cerr << "et is " << et << '\n';
	beInternalError("expressionTypeToIslType: unknown type");
	return Isl_null;
}

static unsigned long
numberOfValuesInType(AST_Expression::ExprType et)
{
	switch (et)
	{
	case AST_Expression::EV_short:
	case AST_Expression::EV_ushort:
		return 65536;

	case AST_Expression::EV_long:
	case AST_Expression::EV_ulong:
		// The following is off by one, but for our purposes,
		// we just need a very large number.
		return ~0;

	case AST_Expression::EV_char:
		return 256;

	case AST_Expression::EV_octet:
		return 256;

	case AST_Expression::EV_bool:
		return 2;
	}
	cerr << "et is " << et << '\n';
	beInternalError("numberOfValuesInType: unknown type");
	return 0;
}

static IslBaseType
predefinedTypeToIslType(AST_PredefinedType::PredefinedType pt)
{
	switch (pt)
	{
	case AST_PredefinedType::PT_long:	return Isl_INTEGER;
	case AST_PredefinedType::PT_ulong:	return Isl_CARDINAL;
	case AST_PredefinedType::PT_short:	return Isl_SHORT_INTEGER;
	case AST_PredefinedType::PT_ushort:	return Isl_SHORT_CARDINAL;
	case AST_PredefinedType::PT_float:	return Isl_SHORT_REAL;
	case AST_PredefinedType::PT_double:	return Isl_REAL;
	case AST_PredefinedType::PT_char:	return Isl_SHORT_CHARACTER;
	case AST_PredefinedType::PT_boolean:	return Isl_BOOLEAN;
	case AST_PredefinedType::PT_octet:	return Isl_BYTE;

	case AST_PredefinedType::PT_any:
		if (!beGlobals.errors.typeAny)
		{
			beGlobals.errors.typeAny = I_TRUE;
			beError("type \"any\" is not allowed");
		}
		break;

	case AST_PredefinedType::PT_pseudo:
		if (!beGlobals.errors.typeObject)
		{
			beGlobals.errors.typeObject = I_TRUE;
			beError("type \"Object\" is not allowed");
		}
		break;

	default:
		cerr << "pt is " << pt << '\n';
		beInternalError("predefinedTypeToIslType: unknown pt");
	}
	return Isl_null;
}

//////////////////////////////////////////////////////////////////////////////

static char *
newString(char *source)
{
	return strcpy(new char[strlen(source) + 1], source);
}

static char *
newAnonymousTypeName()
{
	return newString(form(anonymousTypeFormat,
				++beGlobals.anonymousTypeIndex));
}

//////////////////////

static void
changeUnderscoreToDash(char *src)
{
	for (;;)
	{
		switch (*src)
		{
		case 0:			return;
		case '_':		*src = '-';	break;
		}
		src += 1;
	}
}

// The following only works with ASCII.
#define Normalize(ch)	((ch) & ~ ('a' ^ 'A'))

static int
keywordCompare(char *s1, char *s2)
{
	for (;;)
	{
		int	diff;

		if ((diff = Normalize(*s1) - Normalize(*s2)) != 0)
			return diff;
		if (*s1 == 0)
			return 0;
		s1 += 1;
		s2 += 1;
	}
}

// The following table must be sorted so that binary search will work.
static char *	islKeywords[] =
{
	"array",
	"asynchronous",
	"authentication",
	"boolean",
	"brand",
	"byte",
	"cardinal",
	"character",
	"class",
	"collectible",
	"constant",
	"default",
	"documentation",
	"end",
	"enumeration",
	"exception",
	"false",
	"from",
	"functional",
	"imports",
	"in",
	"inout",
	"integer",
	"interface",
	"limit",
	"long",
	"methods",
	"object",
	"of",
	"optional",
	"others",
	"out",
	"raises",
	"real",
	"record",
	"sequence",
	"short",
	"sibling",
	"singleton",
	"sink",
	"source",
	"superclass",
	"superclasses",
	"supertypes",
	"true",
	"type",
	"union",
};

static idl_bool
isIslKeyword(char *src)
{
	int	lo	= 0;
	int	hi	= sizeof islKeywords / sizeof islKeywords[0] - 1;

	while (lo <= hi)
	{
		int	mid	= (lo + hi) >> 1;
		int	result;

		if ((result = keywordCompare(src, islKeywords[mid])) < 0)
			hi = mid - 1;
		else if (result > 0)
			lo = mid + 1;
		else
			return I_TRUE;
	}
	return I_FALSE;
}

static const int	islKeywordQuote	= '"';

// The src string must be from the heap.
static char *
quoteKeywords(char *src)
{
	if (isIslKeyword(src))
	{
		int	length	= strlen(src);
		char *	quoted	= new char[length + 3];

		quoted[0] = islKeywordQuote;
		memcpy(quoted + 1, src, length);
		quoted[length + 1] = islKeywordQuote;
		quoted[length + 2] = 0;
		delete src;
		return quoted;
	}
	return src;
}

// The src string must be from the heap.
static char *
translatedName(char *src, idl_bool quoteKeys = I_TRUE)
{
	changeUnderscoreToDash(src);
	return quoteKeys ? quoteKeywords(src) : src;
}

//////////////////////

static void
printSimpleDeclName(ostream &o, AST_Decl *d, idl_bool quoteKeys = I_TRUE)
{
	char *name = translatedName(newString(d->local_name()->get_string()),
					quoteKeys);

	o << name;
	delete name;
}

static idl_bool
isInCurrentIslInterface(AST_Decl *d)
{
	if (d->in_main_file())
	{
		if (beGlobals.opts.islInterfacePerTopIdlModule)
		{
			UTL_IdListActiveIterator iterator(d->name());
			Identifier *moduleName =
					beGlobals.topModule->local_name();
			Identifier *name;

			iterator.next();
			name = iterator.item();
			return (idl_bool) moduleName->compare(name);
		}
		return I_TRUE;
	}
	return I_FALSE;
}

static char *
stringCopy(char *dst, const char *src)
{
	while (*src)
		*dst++ = *src++;
	return dst;
}

static const char idlExtension[] = ".idl";

static char *
filePrefixOfDecl(AST_Decl *d)
{
	const int idlExtensionLength = strlen(idlExtension);
	char buffer[1024];

	strcpy(buffer, d->file_name()->get_string());
	char *slash = strrchr(buffer, '/');
	char *base = slash ? slash + 1 : buffer;
	const int nameLength = strlen(buffer);

	if (nameLength > idlExtensionLength)
	{
		char *s = buffer + nameLength - idlExtensionLength;

		if (strcmp(s, idlExtension) == 0)
			*s = 0;
	}
	return translatedName(newString(base));
}

static char *
qualifiedDeclName(AST_Decl *d)
{
	UTL_IdListActiveIterator iterator(d->name());
	char buffer[1024];
	char *dst = buffer;

	if (iterator.item()->get_string()[0] == 0)
		iterator.next();
	else
		beInternalError("qualifiedDeclName: first item not null");

	if (beGlobals.opts.useImports && !isInCurrentIslInterface(d))
	{
		if (beGlobals.imports)
			beGlobals.imports->add_import(d);
		if (beGlobals.opts.islInterfacePerTopIdlModule)
		{
			dst = stringCopy(dst, iterator.item()->get_string());
			iterator.next();
		}
		else
		{
			char *importedName = filePrefixOfDecl(d);

			dst = stringCopy(dst, importedName);
			delete importedName;
		}
		*dst++ = '.';
	}
	else if (beGlobals.opts.islInterfacePerTopIdlModule)
		iterator.next();

	for (int count = 0; !iterator.is_done(); count += 1, iterator.next())
	{
		if (count > 0)
			dst = stringCopy(dst, qualifiedNameSeparator);
		dst = stringCopy(dst, iterator.item()->get_string());
	}
	*dst = 0;
	return translatedName(newString(buffer));
}

static void
printQualifiedDeclName(ostream &o, AST_Decl *d)
{
	char *name = qualifiedDeclName(d);

	o << name;
	delete name;
}

static char *
typedefName(AST_Decl *td)
{
	return td ? qualifiedDeclName(td) : newAnonymousTypeName();
}

//////////////////////

static int
typeIsVoid(AST_Type *t)
{
	return t->node_type() == AST_Decl::NT_pre_defined &&
		AST_PredefinedType::narrow_from_decl(t)->pt() ==
						AST_PredefinedType::PT_void;
}

//////////////////////

static const int escapeChar = '#';

static int
escapedCharCode(int ch)
{
	switch (ch)
	{
	case escapeChar:	return escapeChar;
	case '"':		return '"';
	case '\n':		return 'n';
	case '\r':		return 'r';
	}
	return 0;
}

static void
printIslStringCharacter(ostream &o, int ch)
{
	const int escaped = escapedCharCode(ch);

	if (escaped != 0)
		o << form("%c%c", escapeChar, escaped);
	else if (' ' <= ch && ch <= '~')
		o << form("%c", ch);
	else
		o << form("%c%02x", escapeChar, ch & 0xff);
}

static void
printIslString(ostream &o, char *str)
{
	o << '"';
	for (int i = 0; str[i] != 0; i++)
		printIslStringCharacter(o, str[i]);
	o << '"';
}

static void
printConstantValue(ostream &o, AST_Expression *e)
{
	AST_Expression::AST_ExprValue *v = e->eval(AST_Expression::EK_const);

	switch (v->et)
	{
	case AST_Expression::EV_short:
		o << v->u.sval;
		break;

	case AST_Expression::EV_ushort:
		o << v->u.usval;
		break;

	case AST_Expression::EV_long:
		o << v->u.lval;
		break;

	case AST_Expression::EV_ulong:
		o << v->u.ulval;
		break;

	case AST_Expression::EV_float:
		o << form("%.10g", v->u.fval);
		break;

	case AST_Expression::EV_double:
		o << form("%.16g", v->u.dval);
		break;

	case AST_Expression::EV_char:
		o << (unsigned) (unsigned char) v->u.cval;
		break;

	case AST_Expression::EV_octet:
		o << (unsigned) v->u.oval;
		break;

	case AST_Expression::EV_bool:
		o << (v->u.bval ? "TRUE" : "FALSE");
		break;

	case AST_Expression::EV_string:
		printIslString(o, v->u.strval->get_string());
		break;

	default:
		cerr << "v->et is " << v->et << '\n';
		beInternalError("printConstantValue: unknown expression type");
		break;
	}
}

static unsigned long
positiveConstantValue(AST_Expression *e)
{
	AST_Expression::AST_ExprValue *v = e->eval(AST_Expression::EK_const);

	if (v->et != AST_Expression::EV_ulong)
		beInternalError("positiveConstantValue expected ulong");
	return v->u.ulval;
}

//////////////////////

static void
printTypeHeader(ostream &o, char *name)
{
	o << "TYPE ";
	o << name;
	o << " = ";
}

static void
printTypeHeaderForDecl(ostream &o, AST_Decl *d)
{
	o << "TYPE ";
	printQualifiedDeclName(o, d);
	o << " = ";
}

//////////////////////

static int
nDeclsInScope(UTL_Scope *s)
{
	UTL_ScopeActiveIterator iterator(s, UTL_Scope::IK_decls);
	int count = 0;

	while (!iterator.is_done())
	{
		count += 1;
		iterator.next();
	}
	return count;
}

static void
topLevelScopeIterator(ostream &o, UTL_Scope *s,
	void (*proc)(ostream &o, AST_Module *module))
{
	UTL_ScopeActiveIterator iterator(s, UTL_Scope::IK_decls);
	int count = 0;

	for (; !iterator.is_done(); iterator.next())
	{
		AST_Decl * const d = iterator.item();

		// #included things don't count when using the
		// importation model.
		if (beGlobals.opts.useImports && !d->in_main_file())
			continue;

		switch (d->node_type())
		{
		case AST_Decl::NT_module:
			if (proc)
			{
				if (count > 0)
					o << "\n\n";
				(*proc)(o, be_module::narrow_from_decl(d));
				count += 1;
			}
			break;

		case AST_Decl::NT_pre_defined:
			break;

		default:
			if (beGlobals.opts.islInterfacePerTopIdlModule)
			{
				beWarning("top level declaration is not a \
module -- reverting to !topmodules");
				beGlobals.opts.islInterfacePerTopIdlModule =
					I_FALSE;
			}
			break;
		}
	}
}

static void
printFilePrefix(ostream &o, AST_Decl *d)
{
	char *prefix = filePrefixOfDecl(d);

	o << prefix;
	delete prefix;
}

static void
genIslForTopLevelModule(ostream &o, AST_Module *module)
{
	beGlobals.topModule = module;
	o << "INTERFACE ";
	if (beGlobals.opts.islInterfacePerTopIdlModule)
		printSimpleDeclName(o, module);
	else
		printFilePrefix(o, module);
	if (beGlobals.opts.useImports)
	{
		nullbuf nullStream;
		ostream nullOStream((streambuf *) &nullStream);
		const int anonTypeIndexSave = beGlobals.anonymousTypeIndex;

		beGlobals.imports = new BE_Imports;
		genIslForScope(module, nullOStream);
		beGlobals.imports->gen_isl(o);
		delete beGlobals.imports;
		beGlobals.imports = 0;
		beGlobals.anonymousTypeIndex = anonTypeIndexSave;
	}
	o << ";\n\n";
	genIslForScope(module, o);
}

//////////////////////////////////////////////////////////////////////////////

BE_Imports::BE_Imports()
{
	list = 0;
}

void
BE_Imports::add_import(AST_Decl *d)
{
	char *name;
	idl_bool allocatedName;

	if (beGlobals.opts.islInterfacePerTopIdlModule)
	{
		UTL_IdListActiveIterator iterator(d->name());

		iterator.next();
		name = iterator.item()->get_string();
		allocatedName = I_FALSE;
	}
	else
	{
		name = filePrefixOfDecl(d);
		allocatedName = I_TRUE;
	}

	for (Node *n = list; n; n = n->next)
	{
		if (strcmp(name, n->interfaceName) == 0)
		{
			if (allocatedName)
				delete name;
			return;
		}
	}

	Node *x = new Node;
	x->next = list;
	x->interfaceName = name;
	x->fileName = d->in_main_file() ? 0 : d->file_name()->get_string();
	list = x;
}

void
BE_Imports::gen_isl(ostream &o)
{
	int count = 0;

	for (Node *n = list; n; n = n->next)
	{
		o << (count == 0 ? "\n\tIMPORTS\t" : ",\n\t\t");
		o << n->interfaceName;
		if (n->fileName)
			o << " FROM \"" << n->fileName << "\"";
		count += 1;
	}
	if (count > 0)
		o << " END";
}


nullbuf::nullbuf() : streambuf()
{
}

int
nullbuf::overflow(int)
{
	return 0;
}

//////////////////////////////////////////////////////////////////////////////


void
be_predefined_type::pregen_types(ostream &)
{
	// empty
}

void
be_predefined_type::gen_isl(ostream &)
{
	// empty
}

void
be_predefined_type::gen_type_name(ostream &o)
{
	o << islBaseTypeName(predefinedTypeToIslType(pt()));
}

idl_bool
be_predefined_type::gen_isl_typedef(ostream &, AST_Decl *)
{
	return I_FALSE;
}

void
be_predefined_type::be_dump(ostream &o, int level)
{
	dumpHeader(o, level, this, "predefined_type");
}


void
be_module::pregen_types(ostream &o)
{
	pregenTypesForScope(this, o);
}

void
be_module::gen_isl(ostream &o)
{
	genIslForScope(this, o);
}

void
be_module::be_dump(ostream &o, int level)
{
	dumpHeader(o, level, this, "module");
	dumpScope(o, level + 1, this);
}


void
be_root::gen_isl(ostream &o)
{
	if (beGlobals.opts.islInterfacePerTopIdlModule)
	{
		// Run with null procedure to check that only
		// modules are at the top level of declaration.
		topLevelScopeIterator(o, this, NULL);
	}

	if (beGlobals.opts.islInterfacePerTopIdlModule)
		topLevelScopeIterator(o, this, genIslForTopLevelModule);
	else
		genIslForTopLevelModule(o, this);

	if (beGlobals.nErrors != 0)
	{
		exit(1);
	}
}

void
be_root::be_dump(ostream &o)
{
	dumpHeader(o, 0, this, "root");
	dumpScope(o, 1, this);
}


static char	methodSeparator[] = ",\n\t\t";

void
be_interface::pregen_types(ostream &)
{
	// empty
}

static void dumpStringList (ostream &o, UTL_StrList *p)
{
  UTL_StrlistActiveIterator *i;
  if (p != NULL)
    {
      i = new UTL_StrlistActiveIterator(p);
      while (!i->is_done())
	{
	  o << "  <" << i->item()->get_string() << ">\n";
	  i->next();
	}
      delete(i);
    }
}

void
be_interface::gen_isl(ostream &o)
{
	genIslForScope(this, o);
	pregenTypesForScope(this, o);

	printTypeHeaderForDecl(o, this);
	o << "OBJECT";
	o << " OPTIONAL";

	if (n_inherits() == 1)
	{
		o << "\n\tSUPERCLASS ";
		printQualifiedDeclName(o, inherits()[0]);
	}
	else if (n_inherits() > 1)
	{
		o << "\n\tSUPERCLASSES ";
		for (int index = 0; index < n_inherits(); index++)
		{
			if (index)
				o << ", ";
			printQualifiedDeclName(o, inherits()[index]);
		}
		o << " END";
	}
	this->gen_type_id(o);

	genIslForScope(this, o, "\n\tMETHODS\n\t\t", methodSeparator,
		"\n\tEND", NTF_attr_and_op);
	o << ";\n";
}

void
be_interface::gen_type_name(ostream &o)
{
	printQualifiedDeclName(o, this);
}

static char *
qualifiedRepIDName(AST_Decl *d)
{
	UTL_IdListActiveIterator iterator(d->name());
	char buffer[1024];
	char *dst = buffer;

	if (iterator.item()->get_string()[0] == 0)
		iterator.next();
	else
		beInternalError("qualifiedRepIDName: first item not null");

	if (beGlobals.opts.useImports && !isInCurrentIslInterface(d))
	{
		if (beGlobals.imports)
			beGlobals.imports->add_import(d);
		if (beGlobals.opts.islInterfacePerTopIdlModule)
		{
			dst = stringCopy(dst, iterator.item()->get_string());
			iterator.next();
		}
		else
		{
			char *importedName = filePrefixOfDecl(d);

			dst = stringCopy(dst, importedName);
			delete importedName;
		}
		*dst++ = '/';
	}
	else if (beGlobals.opts.islInterfacePerTopIdlModule)
		iterator.next();

	for (int count = 0; !iterator.is_done(); count += 1, iterator.next())
	{
		if (count > 0)
			dst = stringCopy(dst, repIDSeparator);
		dst = stringCopy(dst, iterator.item()->get_string());
	}
	*dst = 0;
	return translatedName(newString(buffer));
}

char *
be_interface::repository_id ()
{
  if (this->_repository_id == NULL)
    {
      UTL_StrList *l = this->pragmas();
      if (l == NULL)
	{
	  char buf[1024];
	  sprintf (buf, "IDL:%s:1.0", qualifiedRepIDName(this));
	  this->_repository_id = newString(buf);
	}
      else
	{
	  /* look at pragmas to see if there's anything we need
	    from there (id itself, or prefix, or version) */
	  char *prefix = NULL;
	  char *id = NULL;
	  char *version = NULL;
	  char *s;
	  char *this_type = this->local_name()->get_string();
	  UTL_StrlistActiveIterator *i = new UTL_StrlistActiveIterator(l);
	  while (!i->is_done())
	    {
	      s = i->item()->get_string();
	      if (strncmp(s, "#pragma prefix ", strlen("#pragma prefix ")) == 0)
		{
		  static char buf[1024];
		  if (sscanf(s + strlen("#pragma prefix "), "\"%[^\n\"]\"", buf) == 1)
		    prefix = buf;
		}
	      else if (strncmp(s, "#pragma ID ", strlen("#pragma ID ")) == 0)
		{
		  static char buf[1024];
		  static char type[1024];
		  if ((sscanf(s + strlen("#pragma ID "), "%s \"%[^\n\"]\"", type, buf) == 2)
		      && (strcmp(type, this_type) == 0))
		    id = buf;
		}
	      else if (strncmp(s, "#pragma version ", strlen("#pragma version ")) == 0)
		{
		  static char buf[16];
		  if (sscanf(s + strlen("#pragma version "), "%s", buf) == 1)
		    version = buf;
		}
	      i->next();
	    }
	  if (id != NULL)
	    this->_repository_id = newString(id);
	  else
	    {
	      char buf[1024];
	      sprintf (buf, "IDL:%s%s%s:%s",
		       (prefix == NULL) ? "" : prefix,
		       (prefix == NULL) ? "" : "/",
		       qualifiedRepIDName(this),
		       (version == NULL) ? "1.0" : version);
	      this->_repository_id = newString(buf);
	    }
	}
    }
  return this->_repository_id;
}

void
be_interface::gen_type_id (ostream &o)
{
  o << " TYPEID \"" << this->repository_id() << "\"";
}

idl_bool
be_interface::gen_isl_typedef(ostream &, AST_Decl *)
{
	return I_FALSE;
}

void
be_interface::be_dump(ostream &o, int level)
{
	dumpHeader(o, level, this, "interface");
	dumpScope(o, level + 1, this);
}


void
be_interface_fwd::pregen_types(ostream &)
{
	// empty
}

void
be_interface_fwd::gen_isl(ostream &)
{
	// empty
}

void
be_interface_fwd::gen_type_name(ostream &)
{
	beInternalError("gen_type_name of interface_fwd");
}

idl_bool
be_interface_fwd::gen_isl_typedef(ostream &, AST_Decl *)
{
	return I_FALSE;
}

void
be_interface_fwd::be_dump(ostream &o, int level)
{
	dumpHeader(o, level, this, "interface_fwd");
}


void
be_exception::pregen_types(ostream &)
{
	// empty
}

void
be_exception::gen_isl(ostream &o)
{
	const int	nFields		= nDeclsInScope(this);

	if (nFields > 0)
	{
		genIslForLocalTypesInScope(this, o);
		pregenTypesForScope(this, o);
		printTypeHeaderForDecl(o, this);
		o << "RECORD";
		genIslForScope(this, o, "\n\t", ",\n\t");
		o << "\n\tEND;\n";
	}

	o << "EXCEPTION ";
	printQualifiedDeclName(o, this);
	if (nFields > 0)
	{
		o << " : ";
		printQualifiedDeclName(o, this);
	}
	o << ";\n";
}

void
be_exception::be_dump(ostream &o, int level)
{
	dumpHeader(o, level, this, "exception");
	dumpScope(o, level + 1, this);
}


void
be_structure::pregen_types(ostream &)
{
	// empty
}

void
be_structure::gen_isl(ostream &o)
{
	genIslForLocalTypesInScope(this, o);
	pregenTypesForScope(this, o);
	printTypeHeaderForDecl(o, this);
	o << "RECORD";
	genIslForScope(this, o, "\n\t", ",\n\t");
	o << "\n\tEND;\n";
}

void
be_structure::gen_type_name(ostream &o)
{
	printQualifiedDeclName(o, this);
}

idl_bool
be_structure::gen_isl_typedef(ostream &, AST_Decl *)
{
	return I_FALSE;
}

void
be_structure::be_dump(ostream &o, int level)
{
	dumpHeader(o, level, this, "structure");
	dumpScope(o, level + 1, this);
}


void
be_enum::pregen_types(ostream &)
{
	// empty
}

void
be_enum::gen_isl(ostream &o)
{
	printTypeHeaderForDecl(o, this);
	o << "ENUMERATION";
	genIslForScope(this, o, " ", ", ", "", NTF_enum_val);
	o << " END;\n";
}

void
be_enum::gen_type_name(ostream &o)
{
	printQualifiedDeclName(o, this);
}

idl_bool
be_enum::gen_isl_typedef(ostream &, AST_Decl *)
{
	return I_FALSE;
}

void
be_enum::be_dump(ostream &o, int level)
{
	dumpHeader(o, level, this, "enum");
	dumpScope(o, level + 1, this);
}


static void
printRaisesList(ostream &o, UTL_ExceptList *list)
{
	UTL_ExceptlistActiveIterator iterator(list);
	int index;

	for (index = 0; !iterator.is_done(); iterator.next(), index++)
	{
		o << (index == 0 ? "\n\t\t\tRAISES " : ", ");
		printQualifiedDeclName(o, iterator.item());
	}
	if (index)
		o << " END";
}

void
be_operation::pregen_types(ostream &o)
{
	pregenTypesForDecl(return_type(), o);
	pregenTypesForScope(this, o);
}

void
be_operation::gen_isl(ostream &o)
{
	switch (flags())
	{
	case OP_oneway:		o << "ASYNCHRONOUS ";	break;
	}

	printSimpleDeclName(o, this);
	o << " (";
	genIslForScope(this, o, "", ", ");
	o << ")";

	if (!typeIsVoid(return_type()))
	{
		o << " : ";
		genTypeNameForType(return_type(), o);
	}

	printRaisesList(o, exceptions());
}

void
be_operation::be_dump(ostream &o, int level)
{
	dumpHeader(o, level, this, "operation");
	dumpScope(o, level + 1, this);
}


void
be_field::pregen_types(ostream &o)
{
	pregenTypesForDecl(field_type(), o);
}

void
be_field::gen_isl(ostream &o)
{
	printSimpleDeclName(o, this);
	o << " : ";
	genTypeNameForType(field_type(), o);
}

void
be_field::be_dump(ostream &o, int level)
{
	dumpHeader(o, level, this, "field");
}


void
be_argument::pregen_types(ostream &o)
{
	pregenTypesForDecl(field_type(), o);
}

void
be_argument::gen_isl(ostream &o)
{
	switch (direction())
	{
	case dir_IN:			break;
	case dir_OUT:	o << "OUT ";	break;
	case dir_INOUT:	o << "INOUT ";	break;
	}
	printSimpleDeclName(o, this);
	o << " : ";
	genTypeNameForType(field_type(), o);
}

void
be_argument::be_dump(ostream &o, int level)
{
	dumpHeader(o, level, this, "argument");
}


void
be_attribute::pregen_types(ostream &o)
{
	pregenTypesForDecl(field_type(), o);
}

void
be_attribute::gen_isl(ostream &o)
{
	o << attributeGenericPrefix << attributeGetPrefix;
	printSimpleDeclName(o, this, I_FALSE);
	o << " () : ";
	genTypeNameForType(field_type(), o);
	if (!readonly())
	{
		o << methodSeparator;
		o << attributeGenericPrefix << attributeSetPrefix;
		printSimpleDeclName(o, this, I_FALSE);
		o << " (value : ";
		genTypeNameForType(field_type(), o);
		o << ")";
	}
}

void
be_attribute::be_dump(ostream &o, int level)
{
	dumpHeader(o, level, this, "attribute");
}


void
be_union::pregen_types(ostream &)
{
	// empty
}

void
be_union::gen_isl(ostream &o)
{
	genIslForLocalTypesInScope(this, o);
	pregenTypesForScope(this, o);

	printTypeHeaderForDecl(o, this);
	genTypeNameForType(disc_type(), o);
	o << " UNION";
	genIslForScope(this, o, "\n\t", ",\n\t");
	o << "\n\tEND";
	if (needs_others_clause())
		o << " OTHERS";
	o << ";\n";
}

void
be_union::gen_type_name(ostream &o)
{
	printQualifiedDeclName(o, this);
}

idl_bool
be_union::gen_isl_typedef(ostream &, AST_Decl *)
{
	return I_FALSE;
}

void
be_union::be_dump(ostream &o, int level)
{
	dumpHeader(o, level, this, "union");
	dumpScope(o, level + 1, this);
}

idl_bool
be_union::needs_others_clause()
{
	idl_bool	has_default_arm;
	unsigned long	num_cases;
	unsigned long	num_values;

	get_case_info(&has_default_arm, &num_cases);
	if (has_default_arm)
		return I_FALSE;
	if (udisc_type() == AST_Expression::EV_any)
	{
		/* CONVENTION: indicates enum discr */
		AST_Enum *e = AST_Enum::narrow_from_decl(disc_type());

		num_values = e->nmembers();
	}
	else
		num_values = numberOfValuesInType(udisc_type());
	return num_cases < num_values;
}

void
be_union::get_case_info(idl_bool *has_default_arm, unsigned long *num_cases)
{
	UTL_ScopeActiveIterator iterator(this, UTL_Scope::IK_decls);

	*has_default_arm = I_FALSE;
	*num_cases = 0;
	while (!iterator.is_done())
	{
		AST_Decl * const d = iterator.item();

		if (d->node_type() == AST_Decl::NT_union_branch)
		{
			be_union_branch *const branch =
				be_union_branch::narrow_from_decl(d);
			AST_UnionLabelSet *const labs = branch->labels();
			const long nLabels = labs->cardinality();

			for (int i = 0; i < nLabels; i++)
			{
				AST_UnionLabel *const ul = labs->get_element(i);

				switch (ul->label_kind())
				{
				case AST_UnionLabel::UL_default:
					*has_default_arm = I_TRUE;
					break;

				case AST_UnionLabel::UL_label:
					*num_cases += 1;
					break;
				}
			}
		}
		iterator.next();
	}
}


void
be_union_branch::pregen_types(ostream &o)
{
	pregenTypesForDecl(field_type(), o);
}

void
be_union_branch::gen_isl(ostream &o)
{
	printSimpleDeclName(o, this);
	o << " : ";
	genTypeNameForType(field_type(), o);
	o << " = ";
	if (labels()->contains_default_label())
		o << "DEFAULT";
	else
	{
		const long nLabels = labels()->cardinality();

		for (int i = 0; i < nLabels; i++)
		{
			printConstantValue(o,
				labels()->get_element(i)->label_val());
			if (i + 1 < nLabels)
				o << ", ";
		}
		o << " END";
	}
}

void
be_union_branch::be_dump(ostream &o, int level)
{
	dumpHeader(o, level, this, "union_branch");
}


void
be_constant::pregen_types(ostream &)
{
	// empty
}

void
be_constant::gen_isl(ostream &o)
{
	o << "CONSTANT ";
	printQualifiedDeclName(o, this);
	o << " : ";
	o << islBaseTypeName(expressionTypeToIslType(et()));
	o << " = ";
	printConstantValue(o, constant_value());
	o << ";\n";
}

void
be_constant::be_dump(ostream &o, int level)
{
	dumpHeader(o, level, this, "constant");
}


void
be_enum_val::pregen_types(ostream &)
{
	// empty
}

void
be_enum_val::gen_isl(ostream &o)
{
	printSimpleDeclName(o, this);
}

void
be_enum_val::be_dump(ostream &o, int level)
{
	dumpHeader(o, level, this, "enum_val");
}


void
be_array::pregen_types(ostream &o)
{
	gen_isl(o);
}

void
be_array::gen_isl(ostream &o)
{
	(void) gen_isl_typedef(o, NULL);
}

void
be_array::gen_type_name(ostream &o)
{
	if (pd_generated_type_name == NULL && beGlobals.imports == NULL)
		beInternalError("array type name");
	else
		o << pd_generated_type_name;
}

idl_bool
be_array::gen_isl_typedef(ostream &o, AST_Decl *td)
{
	if (pd_generated_type_name == NULL)
	{
		pregenTypesForDecl(base_type(), o);
		if (beGlobals.imports == NULL)
		{
			pd_generated_type_name = typedefName(td);
			printTypeHeader(o, pd_generated_type_name);
		}
		o << "ARRAY OF ";
		for (int index = 0; index < n_dims(); index++)
		{
			if (index)
				o << ", ";
			o << positiveConstantValue(dims()[index]);
		}
		o << " ";
		genTypeNameForType(base_type(), o);
		o << ";\n";
		return I_TRUE;
	}
	return I_FALSE;
}

void
be_array::be_dump(ostream &o, int level)
{
	dumpHeader(o, level, this, "array");
}


void
be_sequence::pregen_types(ostream &o)
{
	gen_isl(o);
}

void
be_sequence::gen_isl(ostream &o)
{
	(void) gen_isl_typedef(o, NULL);
}

void
be_sequence::gen_type_name(ostream &o)
{
	if (pd_generated_type_name == NULL && beGlobals.imports == NULL)
		beInternalError("sequence type name");
	else
		o << pd_generated_type_name;
}

idl_bool
be_sequence::gen_isl_typedef(ostream &o, AST_Decl *td)
{
	if (pd_generated_type_name == NULL)
	{
		unsigned long maxSize = positiveConstantValue(max_size());

		pregenTypesForDecl(base_type(), o);
		if (beGlobals.imports == NULL)
		{
			pd_generated_type_name = typedefName(td);
			printTypeHeader(o, pd_generated_type_name);
		}
		o << "SEQUENCE OF ";
		genTypeNameForType(base_type(), o);
		if (maxSize > 0)
			o << " LIMIT " << maxSize;
		o << ";\n";
		return I_TRUE;
	}
	return I_FALSE;
}

void
be_sequence::be_dump(ostream &o, int level)
{
	dumpHeader(o, level, this, "sequence");
}


void
be_string::pregen_types(ostream &o)
{
	gen_isl(o);
}

void
be_string::gen_isl(ostream &o)
{
	(void) gen_isl_typedef(o, NULL);
}

void
be_string::gen_type_name(ostream &o)
{
	if (pd_generated_type_name == NULL && beGlobals.imports == NULL)
		beInternalError("string type name");
	else
		o << pd_generated_type_name;
}

idl_bool
be_string::gen_isl_typedef(ostream &o, AST_Decl *td)
{
	if (beGlobals.imports != NULL)
		return I_FALSE;
	if (pd_generated_type_name == NULL)
	{
		unsigned long maxSize = positiveConstantValue(max_size());

		if (maxSize > 0)
		{
			pd_generated_type_name = typedefName(td);
			printTypeHeader(o, pd_generated_type_name);
			o << "SEQUENCE OF SHORT CHARACTER LIMIT "
				<< maxSize << ";\n";
			return I_TRUE;
		}
		pd_generated_type_name = ilu_CString_spelling;
	}
	return I_FALSE;
}

void
be_string::be_dump(ostream &o, int level)
{
	dumpHeader(o, level, this, "string");
}


void
be_typedef::pregen_types(ostream &)
{
	// empty
}

void
be_typedef::gen_isl(ostream &o)
{
	if (!genIslTypedefForType(base_type(), o, this))
	{
		printTypeHeaderForDecl(o, this);
		genTypeNameForType(base_type(), o);
		o << ";\n";
	}
}

void
be_typedef::gen_type_name(ostream &o)
{
	printQualifiedDeclName(o, this);
}

idl_bool
be_typedef::gen_isl_typedef(ostream &, AST_Decl *)
{
	return I_FALSE;
}

void
be_typedef::be_dump(ostream &o, int level)
{
	dumpHeader(o, level, this, "typedef");
}
