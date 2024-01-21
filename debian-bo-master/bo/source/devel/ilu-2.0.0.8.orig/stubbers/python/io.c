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

$Id: io.c,v 1.4 1994/09/01 17:57:08 severson Exp $
*/

#include <stdio.h>
#include "iluptype.h"
#include "manifest.h"
#include "util.h"
#include "io.h"

static void
ioSimpleTypeInput(const char *simpleName)
{
	printf("%s.%s%s(%s)", nameModuleIlu, prefixFuncInput, simpleName,
		nameVarCall);
}

static void
ioSimpleTypeOutSize(const char *simpleName, const char *argName,
	const char *prefix)
{
	printf("%s.%s%s(%s, %s)", nameModuleIlu, prefix, simpleName,
		nameVarCall, argName);
}

/************************************/

static void
ioCompoundTypeInput(Type t)
{
	printTypeIoFuncName(t, prefixFuncInput);
	printf("(%s)", nameVarCall);
}

static void
ioCompoundTypeOutSize(Type t, const char *argName, const char *prefix)
{
	printTypeIoFuncName(t, prefix);
	printf("(%s, %s)", nameVarCall, argName);
}

/************************************/

void
ioArraySpecialElemInput(const char *eName, const long length)
{
	printf("%s.%s%s(%s, %ld)", nameModuleIlu, prefixFuncInput, eName,
		nameVarCall, length);
}

void
ioArraySpecialElemOutSize(const char *eName, const char *argName,
	const char *prefix, const long length)
{
	printf("%s.%s%s(%s, %s, %ld)", nameModuleIlu, prefix, eName,
		nameVarCall, argName, length);
}

static void
ioArrayTypeInput(Type t)
{
	TypeDescription	d	= baseTypeDescription(t);
	list		dims	= d->structuredDes.array.dimensions;
	int		nDims	= list_size(dims);

	if (nDims == 1)
	{
		Type	eType	= d->structuredDes.array.type;
		char *	eName	= arraySpecialElemTypeName(eType);

		if (eName != 0)
		{
			ioArraySpecialElemInput(eName,
				(long) list_ref(dims, 0));
			return;
		}
	}
	ioCompoundTypeInput(t);
}

static void
ioArrayTypeOutSize(Type t, const char *argName, const char *prefix)
{
	TypeDescription	d	= baseTypeDescription(t);
	list		dims	= d->structuredDes.array.dimensions;
	int		nDims	= list_size(dims);

	if (nDims == 1)
	{
		Type	eType	= d->structuredDes.array.type;
		char *	eName	= arraySpecialElemTypeName(eType);

		if (eName != 0)
		{
			ioArraySpecialElemOutSize(eName, argName, prefix,
				(long) list_ref(dims, 0));
			return;
		}
	}
	ioCompoundTypeOutSize(t, argName, prefix);
}

/************************************/

static void
ioSequenceTypeInput(Type t)
{
	TypeDescription	d	= baseTypeDescription(t);
	Type		eType	= d->structuredDes.sequence.type;
	char *		eName	= sequenceSpecialElemTypeName(eType);

	if (eName != 0)
	{
		long	limit	= d->structuredDes.sequence.limit;

		printf("%s.%s%s(%s, %ld)", nameModuleIlu,
			prefixFuncInput, eName,
			nameVarCall, limit);
		return;
	}
	ioCompoundTypeInput(t);
}

static void
ioSequenceTypeOutSize(Type t, const char *argName, const char *prefix)
{
	TypeDescription	d	= baseTypeDescription(t);
	Type		eType	= d->structuredDes.sequence.type;
	char *		eName	= sequenceSpecialElemTypeName(eType);

	if (eName != 0)
	{
		long	limit	= d->structuredDes.sequence.limit;

		printf("%s.%s%s(%s, %s, %ld)", nameModuleIlu,
			prefix, eName, nameVarCall, argName, limit);
		return;
	}
	ioCompoundTypeOutSize(t, argName, prefix);
}

/************************************/

void
ioObjDiscrimInput(Type t)
{
	printf("%s.%sObjectID(%s, %s.%s, ", nameModuleIlu, prefixFuncInput,
		nameVarCall, nameModuleIlu, booleanImage(1));
	printClassVarName(t, NULL);
	printf(")");
}

void
ioObjDiscrimOutSize(Type t, const char *prefix)
{
	printf("%s.%sObjectID(%s, %s, %s.%s, ", nameModuleIlu, prefix,
		nameVarCall, nameVarSelf, nameModuleIlu, booleanImage(1));
	printClassVarName(t, NULL);
	printf(")");
}

static void
ioObjectTypeInput(Type t)
{
	printf("%s.%sObjectID(%s, %s.%s, ",
		nameModuleIlu, prefixFuncInput, nameVarCall,
		nameModuleIlu, booleanImage(0));
	printClassVarName(t, NULL);
	printf(")");
}

static void
ioObjectTypeOutSize(Type t, const char *argName, const char *prefix)
{
	printf("%s.%sObjectID(%s, %s, %s.%s, ",
		nameModuleIlu, prefix,
		nameVarCall, argName, nameModuleIlu, booleanImage(0));
	printClassVarName(t, NULL);
	printf(")");
}

/************************************/

static void
ioOptionalTypeInput(Type t)
{
	TypeDescription	d	= baseTypeDescription(t);

	printf("%s.%sOptional(%s) and ", nameModuleIlu, prefixFuncInput,
		nameVarCall);
	ioTypeInput(d->structuredDes.optional);
}

static void
ioOptionalTypeOutSize(Type t, const char *argName, const char *prefix)
{
	TypeDescription	d	= baseTypeDescription(t);

	printf("%s.%sOptional(%s, %s != None)", nameModuleIlu, prefix,
		nameVarCall, argName);
	printf(" %s ((%s != None) and ",
		prefix == prefixFuncSizeOf ? "+" : "or",
		argName);
	ioTypeOutSize(d->structuredDes.optional, argName, prefix);
	printf(")");
}

/************************************/

void
ioTypeInput(Type t)
{
	char *	simpleName	= simpleTypeName(t);

	if (simpleName)
	{
		ioSimpleTypeInput(simpleName);
		return;
	}
	switch (baseTypeDescription(t)->type)
	{
	case record_Type:
	case union_Type:
		ioCompoundTypeInput(t);
		break;

	case array_Type:
		ioArrayTypeInput(t);
		break;

	case sequence_Type:
		ioSequenceTypeInput(t);
		break;

	case object_Type:
		ioObjectTypeInput(t);
		break;

	case optional_Type:
		ioOptionalTypeInput(t);
		break;

	default:
		fatal("ioTI: unexpected primitive type (%d)",
			baseTypeDescription(t)->type);
	}
}

void
ioTypeOutSize(Type t, const char *argName, const char *prefix)
{
	char *	simpleName	= simpleTypeName(t);

	if (simpleName)
	{
		ioSimpleTypeOutSize(simpleName, argName, prefix);
		return;
	}
	switch (baseTypeDescription(t)->type)
	{
	case record_Type:
	case union_Type:
		ioCompoundTypeOutSize(t, argName, prefix);
		break;

	case array_Type:
		ioArrayTypeOutSize(t, argName, prefix);
		break;

	case sequence_Type:
		ioSequenceTypeOutSize(t, argName, prefix);
		break;

	case object_Type:
		ioObjectTypeOutSize(t, argName, prefix);
		break;

	case optional_Type:
		ioOptionalTypeOutSize(t, argName, prefix);
		break;

	default:
		fatal("ioTOS: unexpected primitive type (%d)",
			baseTypeDescription(t)->type);
	}
}
