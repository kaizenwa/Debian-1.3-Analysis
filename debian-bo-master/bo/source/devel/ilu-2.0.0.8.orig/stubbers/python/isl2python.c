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

$Id: isl2python.c,v 1.11 1996/04/30 03:24:13 janssen Exp $
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "iluptype.h"
#include "manifest.h"
#include "name.h"
#include "util.h"
#include "genstub.h"
#include "genskel.h"
#include "prttree.h"

#ifdef _IS_POSIX
#include <unistd.h>
#include <errno.h>
#endif /* _IS_POSIX */

static boolean
matchImportedWithString(void *p1, void *p2)
{
	return strcmp(((Imported) p1)->name, (char *) p2) == 0;
}

static void
addInterfaceToImports(list imports, Interface ifc)
{
	char *	ifcName	= ifc->name->base_name;

	if (list_find(imports, matchImportedWithString, ifcName) == 0)
	{
		Imported	i = (Imported) iluparser_Malloc(sizeof *i);

		i->name = ifcName;
		i->filename = 0;
		list_insert(imports, i);
	}
}

static boolean
matchPointer(void *p1, void *p2)
{
	return p1 == p2;
}

typedef struct
{
	list	imports;
	list	sortedTypes;
} PtRock;

static void
processType(Type t, PtRock *r)
{
	static list	pendingTypes;

	if (t->importInterfaceName != 0)
	{
		Type	urt	= ur_type(t);

		if (urt->interface)
			addInterfaceToImports(r->imports, urt->interface);
		return;
	}

	if (list_find(r->sortedTypes, matchPointer, t) != 0)
		return;
	if (pendingTypes == 0)
		pendingTypes = new_list();
	if (list_find(pendingTypes, matchPointer, t) != 0)
		fatal("internal error: circularity in types");
	list_insert(pendingTypes, t);

	if (t->description)
	{
		switch (t->description->type)
		{
		case object_Type:
		    {
			Class	c	= t->description->structuredDes.object;

			list_enumerate(c->superclasses, (EnumProc) processType,
				r);
		    }
		    break;

		default:
		    /* null */
		    break;
		}
	}
	else
	{
		/* alias */
		processType(t->supertype, r);
	}

	list_remove(pendingTypes, t);
	list_insert(r->sortedTypes, t);
}

static void
normalizeInterface(Interface ifc)
{
	PtRock	rock;

	rock.imports = ifc->imports;
	rock.sortedTypes = new_list();
	list_enumerate(ifc->types, (EnumProc) processType, &rock);
	list_clear(ifc->types, FALSE);
	ifc->types = rock.sortedTypes;
}

/************************************************************************/

static int		flagPrintTree;
static int		flagGenStub;
static int		flagGenSkel;
static int		flagRmFirst;

static void
possibleUnlink (const char *filename)
{
#ifdef _IS_POSIX
  if (flagRmFirst)
    {
      if (unlink(filename) < 0)
	{
	  int e = errno;
	  if (e != ENOENT)
	    fprintf (stderr, "Warning:  couldn't delete existing file \"%s\", %s.\n",
		     filename, strerror(e));
	}
    }
#endif /* _IS_POSIX */
}

static void
reopenStdout(const char *what, const char *fmtModuleName, Interface ifc)
{
	char	moduleName[512];
	char	fileName[512];

	sprintf(moduleName, fmtModuleName, getSimpleInterfaceName(ifc));
	sprintf(fileName, fmtFileName, moduleName);
	fprintf(stderr, "%s for interface \"%s\" to %s ...\n", what,
		interface_name(ifc), fileName);
	possibleUnlink(fileName);
	if (freopen(fileName, "w", stdout) == NULL)
		sysFatal(fileName);
}

static void
generate(Interface ifc)
{
	normalizeInterface(ifc);

	if (flagPrintTree)
		printTree(ifc);

	if (flagGenStub)
	{
		if (flagGenStub == 'f')
			reopenStdout("client stubs", "%s", ifc);
		generateStub(ifc);
	}

	if (flagGenSkel)
	{
		if (flagGenSkel == 'f')
			reopenStdout("server stubs", fmtSkelModuleName, ifc);
		generateSkel(ifc);
	}
}

/************************************/

typedef enum
{
	OptTree, OptStub, OptSkel, OptRm,
	OptNull
} Option;

typedef struct
{
	char *	name;
	Option	opt;
} OptTuple;

static OptTuple		optionTable[] =
{
    {	"tree",		OptTree	},
    {	"stub",		OptStub	},
    {	"skel",		OptSkel	},
    {   "removefirst",	OptRm   },
};

static Option
getOption(char *name)
{
	int	i;

	for (i = 0; i < sizeof optionTable / sizeof optionTable[0]; i++)
	{
		if (strcmp(name, optionTable[i].name) == 0)
			return optionTable[i].opt;
	}
	return OptNull;
}

/************************************/

static void
usage(void)
{
	fprintf(stderr, "usage: %s [-removefirst] [-tree] files\n", programName);
	exit(0);
}

int
main(int argc, char **argv)
{
	int	optind	= 1;

	programName = iluparser_GetProgramName(argv[0]);

	while (optind < argc && argv[optind][0] == '-')
	{
		char *	opt	= argv[optind++];

		switch (getOption(opt + 1))
		{
		case OptTree:		flagPrintTree = 1;	break;
		case OptStub:		flagGenStub = 's';	break;
		case OptSkel:		flagGenSkel = 's';	break;
		case OptRm:		flagRmFirst = 1;	break;
		default:		usage();
		}
	}
	if (flagGenStub == 0 && flagGenSkel == 0)
		flagGenStub = flagGenSkel = 'f';

	while (optind < argc)
	{
		char *	fileName= argv[optind++];
		list	s;

		if ((s = ParseFile(fileName)) == 0)
			fatal("Couldn't find or parse %s.\n", fileName);
		list_enumerate(s, (EnumProc) generate, NULL);
	}

	return 0;
}
