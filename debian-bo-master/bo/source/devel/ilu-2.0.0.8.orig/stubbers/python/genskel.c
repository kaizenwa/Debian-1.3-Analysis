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

$Id: genskel.c,v 1.22 1996/06/19 01:32:14 janssen Exp $
*/

#include <stdio.h>
#include "iluptype.h"
#include "io.h"
#include "manifest.h"
#include "name.h"
#include "util.h"
#include "genskel.h"

static char *	ifcName;
static char *	sysIfcName = "sys";
static char *	tracebackIfcName = "traceback";
static char *	surrogateModuleName = "_theSurrogateModule";

/************************************************************************/

static void
skImported(Imported i)
{
  printImportIfc(i->name, TRUE);
}

static void
skImportSurrogate (char *name)
{
  printImportIfc (name, FALSE);
  printf("%s = %s;\ndel %s;\n\n", surrogateModuleName, name, name);	 
}

/************************************************************************/

static void
skArgInput(Argument a)
{
	if (a->direction == In || a->direction == InOut)
	{
		printf("    %s = ", getArgumentName(a));
		ioTypeInput(a->type);
		newline();
	}
}

static void
skMethodReceiveRequest(Procedure m)
{
	Type	tClass	= m->object;
	Class	c	= tClass->description->structuredDes.object;

	printf("    %s = ", nameVarSelf);
	if (c->singleton != 0)
		printf("%s.GetSingleton(%s)\n", nameModuleIlu, nameVarCall);
	else
	{
		ioObjDiscrimInput(tClass);
		newline();
	}
	list_enumerate(m->arguments, (EnumProc) skArgInput, NULL);
}

static void
skMethodException(Exception e, int *pCount)
{
	Type	et	= e->import ? e->import->type : e->type;

	printf("    except ");
	printExceptionName(e);
	if (et)
		printf(", %s", nameVarExceptValue);
	printf(":\n");

	++*pCount;
	printf("\t%s.BeginException(%s, %d, %s.BeginSizingException(%s, %d) + ",
	       nameModuleIlu, nameVarCall, *pCount, nameModuleIlu, nameVarCall, *pCount);
	if (et)
	{
		printf("\\\n\t  ");
		ioTypeOutSize(et, nameVarExceptValue, prefixFuncSizeOf);
	}
	else
		printf(" 0");
	printf(")\n");

	if (et)
	{
		printf("\t");
		ioTypeOutSize(et, nameVarExceptValue, prefixFuncOutput);
		newline();
	}

	printf("\t%s.FinishException(%s)\n", nameModuleIlu, nameVarCall);
	printf("\treturn\n");
}

static void
skMethodDefaultExceptionHandler(Procedure m)
{
	printf("    except:\n");
	printf("\t%s.CaughtUnexpectedException(%s)\n", nameModuleIlu,
		nameVarCall);
}

typedef struct
{
	int		nResults;
	const char *	prefix;
	int		count;
} RosRock;

static void
skResultOutSize(Type t, RosRock *r)
{
	char		buffer[64];
	const char *	argName;

	if (r->nResults == 1)
		argName = nameVarResult;
	else
	{
		sprintf(buffer, "%s[%d]", nameVarResult, r->count);
		argName = buffer;
	}
	r->count += 1;
	if (r->prefix == prefixFuncOutput)
		printf("    ");
	else if (r->count > 1)
		printf("\\\n      + ");
	ioTypeOutSize(t, argName, r->prefix);
	if (r->prefix == prefixFuncOutput)
		newline();
}

static void
skArgOutSize(Argument a, RosRock *r)
{
	if (a->direction == Out || a->direction == InOut)
		skResultOutSize(a->type, r);
}

static void
skResultsOutSize(Procedure m, const int nResults, const char *prefix)
{
	if (nResults > 0)
	{
		RosRock	rock;

		rock.nResults = nResults;
		rock.prefix = prefix;
		rock.count = 0;

		if (m->returnType)
			skResultOutSize(m->returnType, &rock);
		list_enumerate(m->arguments, (EnumProc) skArgOutSize, &rock);
	}
	else if (prefix == prefixFuncSizeOf)
		printf("0");
}

static void
skMethodSendReply(Procedure m, const int nResults)
{
	printf("    %s.BeginReply(%s, %s.%s, %s.BeginSizingReply(%s, %s.%s) + ",
		nameModuleIlu, nameVarCall, nameModuleIlu,
		booleanImage(list_size(m->exceptions) != 0),
		nameModuleIlu, nameVarCall, nameModuleIlu,
		booleanImage(list_size(m->exceptions) != 0));
	skResultsOutSize(m, nResults, prefixFuncSizeOf);
	printf(")\n");
	skResultsOutSize(m, nResults, prefixFuncOutput);
	printf("    %s.FinishReply(%s)\n", nameModuleIlu, nameVarCall);
}

static void
skMethodFinishRequest(Procedure m)
{
	char *	name		= getProcedureName(m);
	int	nResults	= methodResultCount(m);

	printf("    %s.RequestRead(%s)\n", nameModuleIlu, nameVarCall);
	if (m->asynch != 0)
	  {
	    printf("    ");
	  }
	else
	  {
	    printf("    try:\n");
	    printf("\t");
	    if (nResults > 0)
	      printf("%s = ", nameVarResult);
	  }
	printf("%s.%s(", nameVarSelf, name);
	printArgList(m->arguments, 0);
	printf(")\n");

	if (m->asynch == 0)
	  {
	    int	nExcepts	= 0;
	    
	    list_enumerate(m->exceptions, (EnumProc) skMethodException,
			   &nExcepts);
	    skMethodDefaultExceptionHandler(m);
	    skMethodSendReply(m, nResults);
	  }
	else
	  {
	    printf("    %s.NoReply(%s)\n", nameModuleIlu, nameVarCall);
	  }

}

static void
skMethod(Procedure m, const char *className)
{
	char *	name	= getProcedureName(m);

	newline();
	printf("def ");
	printf(fmtFuncSkel, className, name);
	printf("(%s):\n", nameVarCall);
	skMethodReceiveRequest(m);
	skMethodFinishRequest(m);
}

static void
skClassMethodSkeletons(Type t)
{
	Class	c	= t->description->structuredDes.object;
	char *	name	= getTypeName(t);

	list_enumerate(c->methods, (EnumProc) skMethod, name);
}

/************************************/

static void
skPrintMethodSkeletonName(Procedure m, const char *className)
{
	char *	name	= getProcedureName(m);

	printf("\\\n  ");
	printf(fmtFuncSkel, className, name);
	printf(",");
}

static void
skRegisterClassMethodSkeletons(Type t)
{
	Class	c	= t->description->structuredDes.object;
	char *	name	= getTypeName(t);

	newline();
	printf("%s.RegisterSkeletons(", nameModuleIlu);
	printClassVarName(t, nameVarClass);
	printf(", (");
	list_enumerate(c->methods, (EnumProc) skPrintMethodSkeletonName, name);
	printf("))\n");
}

/************************************/

static void
skClassRecord(Type t)
{
	printf("    %s = ", nameVarClass);
	printClassVarName(t, nameVarClass);
	newline();
}

static void
skDummyMethod(Procedure m)
{
	newline();
	printf("    def %s(%s", getProcedureName(m), nameVarSelf);
	printArgList(m->arguments, 1);
	printf("):\n");
	printf("\traise %s.%s, '%s'\n", nameModuleIlu, nameExceptUnimpl,
		getIslProcedureName(m));
}

static void
skClassClass(Type t)
{
	Class	c	= t->description->structuredDes.object;

	newline();
	printf("class %s(%s.%s):\n", getTypeName(t),
		nameModuleIlu, nameClassSkel);
	skClassRecord(t);
	list_enumerate(c->methods, (EnumProc) skDummyMethod, NULL);
}

/************************************/

static void
skClass(Type t)
{
	skClassMethodSkeletons(t);
	skRegisterClassMethodSkeletons(t);
	skClassClass(t);
}

/************************************/

static void
skAliasObjectType(Type t)
{
	Type	undert	= under_type(t);

	newline();
	printf("%s = ", getTypeName(t));
	if (undert->interface != currentIfc)
	{
	  printf ("_imported_modules['%s__skel'].",
		  getSimpleInterfaceName(undert->interface));
	}
	printf("%s", getTypeName(undert));
	newline();
}

static void
skAliasType(Type t)
{
	switch (type_description(t)->type)
	{
	case object_Type:		skAliasObjectType(t);	break;
	default:			/* null */		break;
	}
}

/************************************/

static void
skType(Type t)
{
	if (t->description)
	{
		switch (t->description->type)
		{
		case object_Type:	skClass(t);		break;
		default:		/* null */		break;
		}
	}
	else
		skAliasType(t);
}

/************************************************************************/

void
generateSkel(Interface ifc)
{
	currentIfc = ifc;
	ifcName	= interface_name(ifc);
	generatingSkeleton = TRUE;
	printBanner("Skeletons", ifc);
	printf("import %s;\n", sysIfcName);
	printImportTable();
	printImportIfc(ifcName, FALSE);
	list_enumerate(ifc->imports, (EnumProc) skImported, NULL);
	list_enumerate(ifc->types,   (EnumProc) skType, NULL);
	generatingSkeleton = FALSE;
}
