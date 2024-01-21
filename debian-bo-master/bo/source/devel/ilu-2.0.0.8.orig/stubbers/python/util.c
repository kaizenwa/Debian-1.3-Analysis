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

$Id: util.c,v 1.7 1996/04/30 03:24:13 janssen Exp $
*/

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include "iluptype.h"
#include "manifest.h"
#include "name.h"
#include "util.h"

char *		programName;
Interface	currentIfc;
boolean		generatingSkeleton;

void
sysFatal(const char *msg)
{
	fprintf(stderr, "Fatal error: ");
	perror(msg);
	exit(1);
}

void
fatal(const char *fmt, ...)
{
	va_list	args;

	va_start(args, fmt);
	fprintf(stderr, "Fatal error: ");
	vfprintf(stderr, fmt, args);
	va_end(args);
	putc('\n', stderr);
	exit(1);
}

/************************************************************************/

boolean
isPrefixOf(const char *prefix, const char *base)
{
	int	i;

	for (i = 0; prefix[i]; i++)
		if (prefix[i] != base[i])
			return FALSE;
	return TRUE;
}

/************************************************************************/

char *
booleanImage(int value)
{
	return value ? "TRUE" : "FALSE";
}

TypeDescription
baseTypeDescription(Type t)
{
	Type	base	= t;

	for (;;)
	{
		if (base->description)
			return base->description;
		base = base->supertype;
	}
}

char *
simpleTypeName(Type t)
{
	TypeDescription	d	= baseTypeDescription(t);

	switch (d->type)
	{
	case boolean_Type:		return "Boolean";
	case byte_Type:			return "Byte";
	case cardinal_Type:		return "Cardinal";
	case character_Type:		return "Character";
	case enumeration_Type:		return "Enum";
	case integer_Type:		return "Integer";
	case longcardinal_Type:		return "LongCardinal";
	case longinteger_Type:		return "LongInteger";
	case longreal_Type:		return "LongReal";
	case real_Type:			return "Real";
	case shortcardinal_Type:	return "ShortCardinal";
	case shortcharacter_Type:	return "Byte";
	case shortinteger_Type:		return "ShortInteger";
	case shortreal_Type:		return "ShortReal";
	default:			return 0;
	}
}

char *
arraySpecialElemTypeName(Type t)
{
	switch (baseTypeDescription(t)->type)
	{
	case byte_Type:			return "Opaque";
	case shortcharacter_Type:	return "StringVec";
	case character_Type:		return "WStringVec";
	default:			return 0;
	}
}

char *
sequenceSpecialElemTypeName(Type t)
{
	switch (baseTypeDescription(t)->type)
	{
	case byte_Type:			return "Bytes";
	case shortcharacter_Type:	return "String";
	case character_Type:		return "WString";
	default:			return 0;
	}
}

static void
countResultArg(Argument a, int *pCount)
{
	if (a->direction == Out || a->direction == InOut)
		*pCount += 1;
}

int
methodResultCount(Procedure m)
{
	int	count	= m->returnType != 0;

	list_enumerate(m->arguments, (EnumProc) countResultArg, &count);
	return count;
}

/************************************************************************/

static char	spacing[] = "                                                ";
char *		sol = spacing + sizeof spacing - 1;

void
indent(int levelDelta)
{
	sol -= levelDelta * 2;
	if (sol < spacing || spacing + sizeof spacing <= sol)
		fatal("internal error: indentation");
}

void
newline(void)
{
	putchar('\n');
}

/************************************************************************/

void
printBanner(const char *part, Interface ifc)
{
	static char *prefixesForPython[2] = { "#", "#" };

	printf("# %s for \"%s\"\n#\n", part, interface_name(ifc));
	iluparser_GenerateBoilerplate(stdout, ifc, programName,
		prefixesForPython);
	printf("\n");
}

void
printImportIfc(const char *ifcName, boolean skelToo)
{
	if (strcmp(ifcName, "ilu") == 0)
		printf("import %s\n", nameModuleIlu);
	else
	{
		char	buffer[1024];
		char *	name	= getImportName(ifcName, buffer);

		printf("import %s; _imported_modules['%s'] = %s; del %s;",
		       name, name, name, name);
		if (skelToo)
		{
		  newline();
		  printf("import %s__skel; _imported_modules['%s__skel'] = %s__skel; del %s__skel;",
			 name, name, name, name);
		}
		newline();
	}
}

void
printImportTable (void)
{
  printf ("_imported_modules = {};\n");
}

/************************************************************************/

static void
printArg(Argument a, int *pCount)
{
	if (a->direction == In || a->direction == InOut)
		printf("%s%s", ++*pCount > 1 ? ", " : "", getArgumentName(a));
}

void
printArgList(list argList, int nPrevArgs)
{
	int	count	= nPrevArgs;

	list_enumerate(argList, (EnumProc) printArg, &count);
}

static void
printIfcPrefix(Interface ifc)
{
	if (generatingSkeleton || ifc != currentIfc)
		printf("%s.", getInterfaceName(ifc));
}

void
printClassVarName(Type t, const char *varName)
{
	Type	urt	= ur_type(t);

	printIfcPrefix(urt->interface);
	printf("%s", getTypeName(urt));
	if (varName)
		printf(".%s", varName);
}

void
printExceptionName(Exception e)
{
	printIfcPrefix(e->interface);
	printf("%s", getExceptionName(e));
}

void
printTypeIoFuncName(Type t, const char *prefix)
{
	Type	urt	= ur_type(t);

	printIfcPrefix(urt->interface);
	printf(fmtFuncIo, prefix, getTypeName(urt));
}
