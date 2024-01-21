/*
Copyright (c) 1991, 1992, 1993, 1994 Xerox Corporation.  All Rights Reserved.

Unlimited use, reproduction, and distribution of this software is
permitted.  Any copy of this software must include both the above
copyright notice of Xerox Corporation and this paragraph.  Any
distribution of this software must comply with all applicable United
States export control laws.  This software is made available AS IS,
and XEROX CORPORATION DISCLAIMS ALL WARRANTIES, EXPRESS OR IMPLIED,
INCLUDING WITHOUT LIMITATION THE IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS FOR A PARTICULAR PURPOSE, AND NOTWITHSTANDING ANY OTHER
PROVISION CONTAINED HEREIN, ANY LIABILITY FOR DAMAGES RESULTING FROM
THE SOFTWARE OR ITS USE IS EXPRESSLY DISCLAIMED, WHETHER ARISING IN
CONTRACT, TORT (INCLUDING NEGLIGENCE) OR STRICT LIABILITY, EVEN IF
XEROX CORPORATION IS ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.

$Id: genstub.c,v 1.48 1996/07/03 19:23:11 janssen Exp $
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "iluptype.h"
#include "io.h"
#include "manifest.h"
#include "name.h"
#include "util.h"
#include "genstub.h"
#include "pythonversion.h"

static char *		ifcName;

/************************************************************************/

static void
stImported(Imported i)
{
	printImportIfc(i->name, FALSE);
}

/************************************************************************/

static void
printString(const char *s)
{
	int	i;

	putchar('\'');
	for (i = 0; s[i]; i++)
	{
		int	ch	= (unsigned char) s[i];

		if (ch < ' ' || '~' < ch)
			printf("\\%03o", ch);
		else
		{
			if (ch == '\'' || ch == '\\')
				putchar('\\');
			putchar(ch);
		}
	}
	putchar('\'');
}

static char *
pythonString (char *s)
{
  int l = strlen(s);
  char *out;
  register char *p, *q;

  out = (char *) malloc(l+1);
  for (p = s, q = out;  *p != 0;  p++)
    {
      if (*p == '-')
	*q++ = '_';
      else
	*q++ = *p;
    }
  *q = 0;
  return (out);  
}

static void
stConstantValue(ConstantValue v, Type t)
{
	switch (v->type)
	{
	case boolean_Type:
		printf("%d", v->val.b != 0);
		break;

	case integer_Type:
		if (v->val.i.value == 0x80000000 && v->val.i.sign < 0)
			printf("(-%lu - 1)", v->val.i.value - 1);
		else
			printf("%s%lu", v->val.i.sign < 0 ? "-" : "",
				v->val.i.value);
		break;

	case real_Type:
		printf("%s%s.%se%ld", v->val.r.sign < 0 ? "-" : "",
			v->val.r.value,
			v->val.r.fraction ? v->val.r.fraction : "0",
			v->val.r.exponent);
		break;

	case shortcharacter_Type:
		if (t == NULL)
		  printString(v->val.s);
		else
		  {
		    char *p = pythonString(v->val.s);
		    Type t2 = type_description(t)->structuredDes.uniond.discriminator_type;
		    if (t2->importInterfaceName != NULL)
		      printf ("%s.", getInterfaceName(ur_type(t2)->interface));
		    printf ("%s.%s", getTypeName(ur_type(t2)), p);
		    free(p);
		  }
		break;

	default:
		fatal("internal error: unexpected constant value (%d)",
			v->type);
	}
}

static void
stConstant(Constant c, int *pCount)
{
	char *	prefix	= "";
	char *	suffix	= "";
	char	buffer[128];

	if (c->import != 0)
		return;
	if (++*pCount == 1)
		newline();
	switch (type_basic_type(c->type))
	{
	case byte_Type:
	case boolean_Type:
	case character_Type:
	case shortcharacter_Type:
	case shortinteger_Type:
	case integer_Type:
	case shortcardinal_Type:
	case enumeration_Type:
		switch (c->value->type)
		{
		case real_Type:
			prefix = "int(";
			suffix = ")";
			break;

		case shortcharacter_Type:
			prefix = "ord(";
			suffix = ")";
			break;

		default:
			/* null */
			break;
		}
		break;

	case longinteger_Type:
	case cardinal_Type:
	case longcardinal_Type:
		switch (c->value->type)
		{
		case boolean_Type:
		case integer_Type:
			suffix = "L";
			break;

		case real_Type:
			prefix = "long(";
			suffix = ")";
			break;

		case shortcharacter_Type:
			prefix = "long(ord(";
			suffix = "))";
			break;

		default:
			/* null */
			break;
		}
		break;

	case real_Type:
	case shortreal_Type:
		switch (c->value->type)
		{
		case boolean_Type:
		case integer_Type:
			suffix = ".0";
			break;

		case shortcharacter_Type:
			prefix = "float(ord(";
			suffix = "))";
			break;

		default:
			/* null */
			break;
		}
		break;

	case longreal_Type:
		prefix = buffer;
		sprintf(prefix, "%s.%s(", nameModuleIlu,
			simpleTypeName(c->type));
		suffix = ")";
		switch (c->value->type)
		{
		case shortcharacter_Type:
			strcat(prefix, "ord(");
			suffix = "))";
			break;

		default:
			/* null */
			break;
		}
		break;

	default:
		/* null */
		break;
	}
	printf("%s = %s", getConstantName(c), prefix);
	stConstantValue(c->value, NULL);
	printf("%s\n", suffix);
}

/************************************************************************/

static void
stDefineIoFunc(Type t, const char *prefix)
{
	newline();
	printf("def ");
	printTypeIoFuncName(t, prefix);
	printf("(%s", nameVarCall);
	if (prefix != prefixFuncInput)
		printf(", %s", nameVarValue);
	printf("):\n");
}

/************************************/

static void
stDefineEnumFieldLiteral(EnumField e, const char *typeName)
{
	char	fieldName[1024];

	getEnumFieldName(e, fieldName);
	printf("    %s = %d;\n", fieldName, e->id);
}

static void
stDefineEnumLiterals(Type t)
{
	TypeDescription	d	= type_description(t);
	list		fields	= d->structuredDes.enumeration;

	newline();
	list_enumerate(fields, (EnumProc) stDefineEnumFieldLiteral,
		getTypeName(t));
}

static void
stEnumFieldCase(EnumField e, int *count)
{
	char	fieldName[1024];

	if (++(*count) > 1)
		printf(",");
	printf("\n  ");
	getEnumFieldName(e, fieldName);
	printf("        %s : '%s'", fieldName, fieldName);
}

static void
stDefineEnumImageDict(Type t)
{
	TypeDescription	d	= type_description(t);
	list		fields	= d->structuredDes.enumeration;
	int count = 0;

	printf("    __image__ = {");
	list_enumerate(fields, (EnumProc) stEnumFieldCase, &count);
	printf("}\n");
}

static void
stEnumerationType(Type t)
{
  newline();
  printf ("class %s:", getTypeName(t));
  stDefineEnumLiterals(t);
  stDefineEnumImageDict(t);
}

/************************************/

static void
stRecordFieldInput(Argument f)
{
	printf("    %s.%s = ", nameVarValue, getArgumentName(f));
	ioTypeInput(f->type);
	newline();
}

static void
printNones (int count)
{
  int i;

  for (i = 0;  i < count;  i++)
    printf ("%sNone", (i > 0) ? ", " : "");
}

static void
stDefineRecordTypeInput(Type t)
{
	TypeDescription	d	= t->description;
	list		fields	= d->structuredDes.record;

	stDefineIoFunc(t, prefixFuncInput);
	printf("    %s.%sRecord(%s)\n", nameModuleIlu,
		prefixFuncInput, nameVarCall);
	printf("    %s = %s (", nameVarValue, getTypeName(t));
	printNones(list_size(fields));
	printf(")\n");
	list_enumerate(fields, (EnumProc) stRecordFieldInput, NULL);
	printf("    %s.%sRecord(%s)\n", nameModuleIlu,
		prefixFuncEnd, nameVarCall);
	printf("    return %s\n", nameVarValue);
}

static void
stRecordFieldOutSize(Argument f, const char *prefix)
{
	char	argName[1024];

	if (prefix == prefixFuncSizeOf)
		printf("\\\n      + ");
	else
		printf("\n    ");
	sprintf(argName, "%s['%s']", nameVarValue, getArgumentName(f));
	ioTypeOutSize(f->type, argName, prefix);
}

static void
stDefineRecordTypeOutSize(Type t, const char *prefix)
{
	TypeDescription	d	= t->description;
	list		fields	= d->structuredDes.record;

	stDefineIoFunc(t, prefix);
	if (prefix == prefixFuncSizeOf)
		printf("    %s = ", nameVarSize);
	else
		printf("    ");
	printf("%s.%sRecord(%s)", nameModuleIlu, prefix, nameVarCall);
	list_enumerate(fields, (EnumProc) stRecordFieldOutSize,
		(void *) prefix);
	printf("\n    %s.%sRecord(%s)\n", nameModuleIlu,
		prefixFuncEnd, nameVarCall);
	if (prefix == prefixFuncSizeOf)
		printf("    return %s\n", nameVarSize);
}

static void
listFieldParam (Argument f, char *prefix)
{
  printf ("%s%s", prefix, getArgumentName(f));
}

static void
listPickleFieldParam (Argument f, char *prefix)
{
  printf ("self.%s, ", getArgumentName(f));
}

static void
initRecordField (Argument f, void *junk)
{
  listFieldParam (f, "        self.");
  listFieldParam (f, " = _");
  printf (";\n");
}

static void
stDefineRecordTypeClass(Type t)
{
  TypeDescription	d	= t->description;
  list			fields	= d->structuredDes.record;

  newline();
  printf ("class %s (iluRt.IluRecord):\n", getTypeName(t));
  printf ("    __ilu_type_name__ = '%s.%s'\n",
	  interface_name(t->interface), type_name(t));
  printf ("    def __init__(self");
  list_enumerate(fields, (EnumProc) listFieldParam, ", _");
  printf ("):\n");
  list_enumerate(fields, (EnumProc) initRecordField, 0);

  printf ("\n");
  printf ("    def __getinitargs__(self):\n");
  printf ("      return (");
  list_enumerate(fields, (EnumProc) listPickleFieldParam, 0);
  printf (")");
  printf ("\n");
}

static void
stRecordType(Type t)
{
  stDefineRecordTypeClass(t);
  stDefineRecordTypeInput(t);
  stDefineRecordTypeOutSize(t, prefixFuncOutput);
  stDefineRecordTypeOutSize(t, prefixFuncSizeOf);
}

/************************************/

typedef struct
{
	Argument	defaultArm;
	Type		unionType;
	const char *	prefix;
	int		count;
} DutRock;

static void
stUnionCaseHeader(Argument f, DutRock *dr)
{
	int	nValues	= list_size(f->values);

	printf("    %sif %s", dr->count == 1 ? "" : "el", nameVarDiscrim);
	if (nValues == 1)
	{
		printf(" == ");
		stConstantValue(list_ref(f->values, 0), dr->unionType);
	}
	else
	{
		int	i;

		printf(" in (");
		for (i = 0; i < nValues; i++)
		{
			if (i > 0)
				printf(", ");
			stConstantValue(list_ref(f->values, i), dr->unionType);
		}
		printf(")");
	}
	printf(":\n");
}

static void
stUnionCaseBody(Argument f, const char *prefix)
{
	printf("\t");
	if (prefix == prefixFuncInput)
	{
		printf("%s = (%s, ", nameVarValue, nameVarDiscrim);
		ioTypeInput(f->type);
		printf(")\n");
	}
	else
	{
		char	argName[64];

		if (prefix == prefixFuncSizeOf)
			printf("%s = %s + ", nameVarSize, nameVarSize);
		sprintf(argName, "%s[1]", nameVarValue);
		ioTypeOutSize(f->type, argName, prefix);
		newline();
	}
}

static void
stUnionFieldInputOutSize(Argument f, DutRock *r)
{
	if (f != r->defaultArm)
	{
	  ++r->count;
	  stUnionCaseHeader(f, r);
	  stUnionCaseBody(f, r->prefix);
	}
}

static int
unionDiscSize(Type t)
{
  switch (type_ur_kind(t))
    {
    case cardinal_Type:
    case integer_Type:
    case enumeration_Type:
      return (4);
    case shortcardinal_Type:
    case shortinteger_Type:
    case character_Type:
      return (2);
    case boolean_Type:
    case byte_Type:
    case shortcharacter_Type:
      return (1);
    default:
      return (4);
    }
}

static void
stDefineUnionTypeInputOutSize(Type t, const char *prefix)
{
	TypeDescription	d	= t->description;
	list		fields	= d->structuredDes.uniond.types;
	DutRock		rock;

	rock.defaultArm = d->structuredDes.uniond.default_arm;
	rock.prefix = prefix;
	rock.count = 0;
	rock.unionType = t;

	stDefineIoFunc(t, prefix);
	printf("    %s = ", nameVarDiscrim);
	if (prefix != prefixFuncInput)
	{
		printf("%s[0]\n", nameVarValue);
		printf("    ");
		if (prefix == prefixFuncSizeOf)
			printf("%s = ", nameVarSize);
	}
	printf("%s.%sUnion(%s", nameModuleIlu, prefix, nameVarCall);
	if (prefix != prefixFuncInput)
		printf(", %s", nameVarDiscrim);
	printf(", %d)\n", unionDiscSize(d->structuredDes.uniond.discriminator_type));

	list_enumerate(fields, (EnumProc) stUnionFieldInputOutSize, &rock);
	if (rock.count == 0)
		printf("    if 1: # no case values\n");
	else
		printf("    else:\n");
	if (rock.defaultArm != 0)
		stUnionCaseBody(rock.defaultArm, prefix);
	else if (!d->structuredDes.uniond.others_allowed)
		printf("\traise TypeError\n");
	else if (prefix == prefixFuncInput)
		printf("\t%s = (%s, None)\n", nameVarValue, nameVarDiscrim);
	else
		printf("\tpass\n");

	printf("    %s.%sUnion(%s)\n", nameModuleIlu, prefixFuncEnd,
		nameVarCall);
	if (prefix == prefixFuncInput)
		printf("    return %s\n", nameVarValue);
	else if (prefix == prefixFuncSizeOf)
		printf("    return %s\n", nameVarSize);
}

static void
stUnionType(Type t)
{
	stDefineUnionTypeInputOutSize(t, prefixFuncInput);
	stDefineUnionTypeInputOutSize(t, prefixFuncOutput);
	stDefineUnionTypeInputOutSize(t, prefixFuncSizeOf);
}

/************************************/

static void
printLoopIndent(const int level)
{
	int	i;

	for (i = 0; i < level; i++)
		printf("    ");
}

static long
totalArrayLength(list dimList)
{
	int	nDims	= list_size(dimList);
	long	result	= 1;
	int	i;

	for (i = 0; i < nDims; i++)
		result *= (long) list_ref(dimList, i);
	return result;
}

static void
stDefineArrayTypeInput(Type t)
{
	TypeDescription	d	= t->description;
	Type		eType	= d->structuredDes.array.type;
	list		dimList	= d->structuredDes.array.dimensions;
	int		nDims	= list_size(dimList);
	char *		eName	= arraySpecialElemTypeName(eType);
	int		nVDims	= eName == 0 ? nDims : nDims - 1;
	int		i;

	stDefineIoFunc(t, prefixFuncInput);
	printf("    %s.%sArray(%s)\n", nameModuleIlu, prefixFuncInput,
		nameVarCall);

	for (i = 0; i < nVDims; i++)
	{
		long	length	= (long) list_ref(dimList, i);

		printLoopIndent(i);
		printf("    %s%d = []\n", nameVarValue, i);
		printLoopIndent(i);
		printf("    for %s%d in range(0,%ld):\n", nameVarIndex, i,
			length);
	}
	printLoopIndent(nVDims);
	printf("    %s%d.append(", nameVarValue, nVDims - 1);
	if (eName)
		ioArraySpecialElemInput(eName,
			(long) list_ref(dimList, nVDims));
	else
		ioTypeInput(eType);
	printf(")\n");

	for (i = nVDims - 1; i > 0; i--)
	{
		printLoopIndent(i);
		printf("    %s%d.append(%s%d)\n", nameVarValue, i - 1,
			nameVarValue, i);
	}

	printf("    %s.%sArray(%s)\n", nameModuleIlu,
		prefixFuncEnd, nameVarCall);
	printf("    return %s0\n", nameVarValue);
}

static void
stDefineArrayTypeOutSize(Type t, const char *prefix)
{
	TypeDescription	d	= t->description;
	Type		eType	= d->structuredDes.array.type;
	list		dimList	= d->structuredDes.array.dimensions;
	int		nDims	= list_size(dimList);
	char *		eName	= arraySpecialElemTypeName(eType);
	int		nVDims	= eName == 0 ? nDims : nDims - 1;
	long		totLen	= totalArrayLength(dimList);
	char		argName[1024];
	int		i;

	stDefineIoFunc(t, prefix);
	printf("    ");
	if (prefix == prefixFuncSizeOf)
		printf("%s = ", nameVarSize);
	printf("%s.%sArray(%s, %ld)\n", nameModuleIlu, prefix,
		nameVarCall, totLen);

	for (i = 0; i < nVDims; i++)
	{
		long	length	= (long) list_ref(dimList, i);

		printLoopIndent(i);
		printf("    for %s%d in range(0,%ld):\n", nameVarIndex, i,
			length);
	}
	printLoopIndent(nVDims);
	printf("    ");
	if (prefix == prefixFuncSizeOf)
		printf("%s = %s + ", nameVarSize, nameVarSize);
	strcpy(argName, nameVarValue);
	for (i = 0; i < nVDims; i++)
	{
		char	subscript[64];

		sprintf(subscript, "[%s%d]", nameVarIndex, i);
		strcat(argName, subscript);
	}
	if (eName)
		ioArraySpecialElemOutSize(eName, argName, prefix,
			(long) list_ref(dimList, nVDims));
	else
		ioTypeOutSize(eType, argName, prefix);
	newline();

	printf("    %s.%sArray(%s)\n", nameModuleIlu,
		prefixFuncEnd, nameVarCall);
	if (prefix == prefixFuncSizeOf)
		printf("    return %s\n", nameVarSize);
}

static void
stArrayType(Type t)
{
	TypeDescription	d	= baseTypeDescription(t);
	Type		eType	= d->structuredDes.array.type;
	list		dimList	= d->structuredDes.array.dimensions;

	if (arraySpecialElemTypeName(eType) != 0 && list_size(dimList) == 1)
		return;
	stDefineArrayTypeInput(t);
	stDefineArrayTypeOutSize(t, prefixFuncOutput);
	stDefineArrayTypeOutSize(t, prefixFuncSizeOf);
}

/************************************/

static void
stDefineSequenceTypeInput(Type t)
{
	TypeDescription	d	= t->description;
	Type		eType	= d->structuredDes.sequence.type;
	cardinal	limit	= d->structuredDes.sequence.limit;

	stDefineIoFunc(t, prefixFuncInput);
	printf("    %s = %s.%sSequence(%s, %lu)\n", nameVarLength,
		nameModuleIlu, prefixFuncInput, nameVarCall, limit);
	printf("    %s = []\n", nameVarValue);
	printf("    for %s in range(0, %s):\n", nameVarIndex, nameVarLength);

	printf("        %s.append(", nameVarValue);
	ioTypeInput(eType);
	printf(")\n");

	printf("    %s.%sSequence(%s)\n", nameModuleIlu,
		prefixFuncEnd, nameVarCall);
	printf("    return %s\n", nameVarValue);
}

static void
stDefineSequenceTypeOutSize(Type t, const char *prefix)
{
	TypeDescription	d	= t->description;
	Type		eType	= d->structuredDes.sequence.type;
	cardinal	limit	= d->structuredDes.sequence.limit;
	char		argName[64];

	stDefineIoFunc(t, prefix);
	printf("    ");
	if (prefix == prefixFuncSizeOf)
		printf("%s = ", nameVarSize);
	printf("%s.%sSequence(%s, len(%s), %lu)\n", nameModuleIlu, prefix,
		nameVarCall, nameVarValue, limit);
	printf("    for %s in range(0, len(%s)):\n", nameVarIndex,
		nameVarValue);

	printf("        ");
	if (prefix == prefixFuncSizeOf)
		printf("%s = %s + ", nameVarSize, nameVarSize);
	sprintf(argName, "%s[%s]", nameVarValue, nameVarIndex);
	ioTypeOutSize(eType, argName, prefix);
	newline();

	printf("    %s.%sSequence(%s)\n", nameModuleIlu,
		prefixFuncEnd, nameVarCall);
	if (prefix == prefixFuncSizeOf)
		printf("    return %s\n", nameVarSize);
}

static void
stSequenceType(Type t)
{
	TypeDescription	d	= baseTypeDescription(t);
	Type		eType	= d->structuredDes.sequence.type;

	if (sequenceSpecialElemTypeName(eType) != 0)
		return;
	stDefineSequenceTypeInput(t);
	stDefineSequenceTypeOutSize(t, prefixFuncOutput);
	stDefineSequenceTypeOutSize(t, prefixFuncSizeOf);
}

/************************************/

static void
stAliasType(Type t)
{
	switch (type_description(t)->type)
	{
	case enumeration_Type:
		stEnumerationType(t);
		break;

	case object_Type:
		newline();
		printf("%s = ", getTypeName(t));
		printClassVarName(t->supertype, NULL);
		newline();
		break;

	default:
		/* null */
		break;
	}
}

/************************************/

static void	stClass(Type t);		/* forward declaration */

static void
stType(Type t)
{
	if (t->description)
	{
		switch (t->description->type)
		{
		case enumeration_Type:	stEnumerationType(t);	break;
		case record_Type:	stRecordType(t);	break;
		case union_Type:	stUnionType(t);		break;
		case array_Type:	stArrayType(t);		break;
		case sequence_Type:	stSequenceType(t);	break;
		case object_Type:	stClass(t);		break;
		default:		/* null */		break;
		}
	}
	else
		stAliasType(t);
}

/************************************************************************/

static void
stException(Exception e, int *pCount)
{
	if (e->import != 0)
		return;
	if (++*pCount == 1)
		newline();
	if (e->corba_rep_id == NULL)
	  printf("%s = 'ilu:%s.%s'\n", getExceptionName(e), ifcName,
		 name_base_name(e->name));
	else
	  printf("%s = '%s'\n", getExceptionName(e), e->corba_rep_id);
}

/************************************************************************/

static cardinal *	methodIdTable;
static int		nMethodIds;

static int
getMethodIndex(cardinal methodId)
{
	int	i;

	for (i = 0; i < nMethodIds; i++)
		if (methodIdTable[i] == methodId)
			return i + 1;
	fatal("getMethodIndex failed");
}

static void
stExceptionListItem(Exception e, int *pCount)
{
	printf("%s%s", ++*pCount > 1 ? ", " : "", getExceptionName(e));
}

static void
stMethodRecord(Procedure m, int *pMethodIndex)
{
	int	nExceptions	= 0;

	methodIdTable[(*pMethodIndex)++ - 1] = m->id;
	printf("\t    ('%s', %d, %s.%s, %s.%s, (",
	       name_base_name(m->name),
	       m->id,
	       nameModuleIlu, booleanImage(m->functional),
	       nameModuleIlu, booleanImage(m->asynch));
	list_enumerate(m->exceptions, (EnumProc) stExceptionListItem,
		&nExceptions);
	if (nExceptions == 1)
		printf(",");
	printf(")),\\\n");
}

static void
stSuperclassClass(Type t, int *pCount)
{
	if (++*pCount > 1)
		printf(", ");
	printClassVarName(t, nameVarClass);
}

static void
stClassRecord(Type t)
{
	Class	c		= t->description->structuredDes.object;
	int	methodIndex	= 1;
	int	nSuperclasses	= 0;

	printf("    %s = %s.FormClassRecord(\\\n", nameVarClass, nameModuleIlu);
	printf("\t'%s.%s',\\\n", ifcName, type_name(t));
	printf("\t'%s',\\\n", c->brand ? c->brand : "");
	printf("\t'%s',\\\n", t->uid);
	if (c->singleton)
		printf("\t'%s',\\\n", c->singleton);
	else
		printf("\tNone,\\\n");
	printf("\t%s.%s,\\\n", nameModuleIlu, booleanImage(c->collectible));
	printf("\t%s.%s,\\\n", nameModuleIlu, booleanImage(c->optional));
	if (c->authentication)
		printf("\t'%s',\\\n", c->authentication);
	else
		printf("\tNone,\\\n");
	printf("\t(\\\n");
	list_enumerate(c->methods, (EnumProc) stMethodRecord, &methodIndex);
	printf("\t),\\\n\t(");
	list_enumerate(c->superclasses, (EnumProc) stSuperclassClass,
		&nSuperclasses);
	if (nSuperclasses == 1)
		printf(",");
	printf(")\\\n\t)\n");
}

static void
stArgInput(Argument a, int *pCount)
{
	if (a->direction == Out || a->direction == InOut)
	{
		if (++*pCount > 1)
			printf(",\\\n\t      ");
		ioTypeInput(a->type);
	}
}

static void
stArgOutSize(Argument a, const char *prefix)
{
	if (a->direction == In || a->direction == InOut)
	{
		if (prefix == prefixFuncOutput)
			printf("\t    ");
		else
			printf("\\\n\t      + ");
		ioTypeOutSize(a->type, getArgumentName(a), prefix);
		if (prefix == prefixFuncOutput)
			newline();
	}
}

static void
stMethodSendRequest(Procedure m)
{
	Type	tClass	= m->object;
	Class	c	= tClass->description->structuredDes.object;

	printf("\t    %s.BeginRequest(%s,\\\n", nameModuleIlu, nameVarCall);
	printf("\t      ");
	if (c->singleton == 0)
		ioObjDiscrimOutSize(tClass, prefixFuncSizeOf);
	else
		printf("0");
	list_enumerate(m->arguments, (EnumProc) stArgOutSize,
		(void *) prefixFuncSizeOf);
	printf(")\n");

	if (c->singleton == 0)
	{
		printf("\t    ");
		ioObjDiscrimOutSize(tClass, prefixFuncOutput);
		newline();
	}
	list_enumerate(m->arguments, (EnumProc) stArgOutSize,
		(void *) prefixFuncOutput);

	printf("\t    %s.FinishRequest(%s)\n", nameModuleIlu, nameVarCall);
}

static void
stMethodGetReply(Procedure m)
{
	int	nResults	= methodResultCount(m);

	printf("\t    %s = %s.GetReply(%s)\n", nameVarExceptCode,
		nameModuleIlu, nameVarCall);
	printf("\t    if %s != 0:\n", nameVarExceptCode);
	printf("\t\t%s(%s, %s.%s, %s)\n", nameFuncCatchExcept, nameVarCall,
		getTypeName(m->object), nameVarClass, nameVarExceptCode);
	if (nResults > 0)
	{
		int	count	= 0;

		printf("\t    %s = ", nameVarResult);
		if (m->returnType)
		{
			++count;
			ioTypeInput(m->returnType);
		}
		list_enumerate(m->arguments, (EnumProc) stArgInput, &count);
		newline();
	}
}

static void
stMethod(Procedure m)
{
	Type	tClass	= m->object;
	Class	c	= tClass->description->structuredDes.object;

	newline();
	printf("    def %s(%s", getProcedureName(m), nameVarSelf);
	printArgList(m->arguments, 1);
	printf("):\n");
	if (methodResultCount(m) > 0)
	  printf("\t%s = None\n", nameVarResult);

#if (ILUPYTHON_MAJOR_VERSION >= 1 && ILUPYTHON_MINOR_VERSION >= 2)

	/* put out docstrings in Python */
	if (m->doc_string != NULL)
	  {
	    char *p;

	    printf ("        \"\"\"");
	    for (p = m->doc_string;  *p != '\0';  p++)
	      {
		putchar (*p);
		if (*p == '\n')
		  printf ("        ");
	      }
	    if (*--p != '\n')
	      printf ("\n        ");
	    printf ("\"\"\"\n");
	  }

#endif /* (ILUPYTHON_MAJOR_VERSION >= 1 && ILUPYTHON_MINOR_VERSION >= 2) */

	printf("\t%s = %s.BeginCall(%s, %s.%s, %d)\n", nameVarCall, nameModuleIlu,
		nameVarSelf, getTypeName(m->object), nameVarClass,
		getMethodIndex(m->id));
	printf("\ttry:\n");
	stMethodSendRequest(m);
	if (m->asynch == 0)
	  {
	    stMethodGetReply(m);
	    printf("\t    %s.ReplyRead(%s)\n", nameModuleIlu, nameVarCall);
	  }
	printf("\tfinally:\n\t    %s.FinishCall(%s)\n", nameModuleIlu, nameVarCall);
	if (methodResultCount(m) > 0)
	  printf("\treturn %s\n", nameVarResult);
}

static void
stSuperclass(Type t, int *pCount)
{
	if (++*pCount > 1)
		printf(", ");
	printClassVarName(t, NULL);
}

static void
stClassClass(Type t)
{
	Class	c	= t->description->structuredDes.object;
	int	nSupers	= 0;

	nMethodIds = list_size(c->methods);
	if (nMethodIds > 0)
	  methodIdTable =
	    (cardinal *) iluparser_Malloc(nMethodIds * sizeof(cardinal));
	else
	  methodIdTable = NULL;

	newline();
	printf("class %s(", getTypeName(t));
	list_enumerate(c->superclasses, (EnumProc) stSuperclass, &nSupers);
	if (nSupers == 0)
		printf("%s.%s", nameModuleIlu, nameClassStub);
	printf("):\n");

#if (ILUPYTHON_MAJOR_VERSION >= 1 && ILUPYTHON_MINOR_VERSION >= 2)

	/* put out docstrings in Python */
	if (c->doc_string != NULL)
	  {
	    char *p;

	    printf ("    \"\"\"");
	    for (p = c->doc_string;  *p != '\0';  p++)
	      {
		putchar (*p);
		if (*p == '\n')
		  printf ("    ");
	      }
	    if (*--p != '\n')
	      printf ("\n    ");
	    printf ("\"\"\"\n");
	  }

#endif /* (ILUPYTHON_MAJOR_VERSION >= 1 && ILUPYTHON_MINOR_VERSION >= 2) */

	stClassRecord(t);
	list_enumerate(c->methods, (EnumProc) stMethod, NULL);

	if (methodIdTable != NULL)
	  iluparser_Free(methodIdTable);
}

static void
stRegisterClass(Type t)
{
	newline();
	printf("%s.RegisterClass(", nameModuleIlu);
	printClassVarName(t, NULL);
	printf(")\n");
}

static void
stClass(Type t)
{
	stClassClass(t);
	stRegisterClass(t);
}

/************************************************************************/

static void
stCatchException(Exception e, int *pCount)
{
	Type	et	= e->import ? e->import->type : e->type;

	printf("    %sif %s == %s:\n", ++*pCount > 1 ? "el" : "",
	       nameVarExceptName, getExceptionName(e));
	printf("\t%s = %s", nameVarExceptName, getExceptionName(e));
	newline();
	if (et)
	  {
	    printf("\t%s = ", nameVarExceptValue);
	    ioTypeInput(et);
	    newline();
	  }
}

static void
stFuncCatchExceptions(list exceptions)
{
	int	nExceptions	= 0;

	newline();
	printf("def %s(%s, %s, %s):\n", nameFuncCatchExcept,
		nameVarCall, nameVarClass, nameVarExceptCode);
	printf("    %s = None\n", nameVarExceptValue);
	printf("    %s = %s.ExceptionName(%s, %s, %s)\n", nameVarExceptName,
		nameModuleIlu, nameVarCall, nameVarClass, nameVarExceptCode);
	list_enumerate(exceptions, (EnumProc) stCatchException, &nExceptions);
	printf("    raise %s, %s\n", nameVarExceptName, nameVarExceptValue);
}

/************************************************************************/

void
generateStub(Interface ifc)
{
	int	nConstants	= 0;
	int	nExceptions	= 0;

	currentIfc = ifc;
	ifcName = interface_name(ifc);
	printBanner("Stubs", ifc);
	printImportTable();
	list_enumerate(ifc->imports,    (EnumProc) stImported,  NULL);
	list_enumerate(ifc->constants,  (EnumProc) stConstant,  &nConstants);
	list_enumerate(ifc->exceptions, (EnumProc) stException, &nExceptions);
	list_enumerate(ifc->types,      (EnumProc) stType,      NULL);
	stFuncCatchExceptions(ifc->exceptions);
}
