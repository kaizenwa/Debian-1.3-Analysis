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

$Id: name.c,v 1.6 1996/04/30 03:24:13 janssen Exp $
*/

#include <stdio.h>
#include <string.h>
#include "iluptype.h"
#include "manifest.h"
#include "name.h"
#include "util.h"

static void
translateNameString(char *dst, const char *src)
{
	while (*src)
	{
		*dst++ = *src == '-' ? '_' : *src;
		src++;
	}
	*dst = 0;
}

static char	language[] = "python";

static char *
setItemName(Name n, char *importInterface)
{
	char	iname[1024];
	char	buffer[1024];
	char *	dst = buffer;

	if (importInterface)
	{
		translateNameString(iname, importInterface);
		sprintf (dst, "_imported_modules['%s'].", iname);
		dst += strlen(dst);
	}
	translateNameString(dst, n->base_name);
	name_set_lang_name(n, language, buffer);
	return name_lang_name(n, language);
}

static char *
getItemName(Name n, char *importInterface)
{
	char *	result	= name_lang_name(n, language);

	if (result == 0)
		result = setItemName(n, importInterface);
	return result;
}

/************************************************************************/

char *
getArgumentName(Argument a)
{
	return getItemName(a->name, NULL);
}

char *
getConstantName(Constant c)
{
	return getItemName(c->name, c->importInterfaceName);
}

char *
getEnumFieldName(EnumField e, char *buffer)
{
	translateNameString(buffer, e->name);
	return buffer;
}

char *
getExceptionName(Exception e)
{
	return getItemName(e->name, e->importInterfaceName);
}

char *
getImportName(const char *name, char *buffer)
{
	translateNameString(buffer, name);
	return buffer;
}

char *
getInterfaceName(Interface i)
{
  char buffer[1024];
  char newname[1024];
  char *result	= name_lang_name(i->name, "imported_interface");

  if (result == 0)
    {
      translateNameString(buffer, name_base_name(i->name));
      sprintf (newname, "_imported_modules['%s']", buffer);
      name_set_lang_name(i->name, "imported_interface", newname);
      result = name_lang_name(i->name, "imported_interface");
    }
  return result;
}

char *
getSimpleInterfaceName (Interface i)
{
  return getItemName(i->name, NULL);
}

char *
getProcedureName(Procedure p)
{
	char *	result	= name_lang_name(p->name, language);

	if (result == 0)
	{
		char	buffer[1024];
		char *	src	= p->name->base_name;

		if (isPrefixOf(prefixIdlAttribute, src))
			src += strlen(prefixIdlAttribute);

		translateNameString(buffer, src);
		name_set_lang_name(p->name, language, buffer);
		result = name_lang_name(p->name, language);
	}
	return result;
}

char *
getTypeName(Type t)
{
	char *	result;

	result = name_lang_name(t->name, language);
	if (result == 0)
		result = setItemName(t->name, NULL);
	if (t->importInterfaceName)
		fatal("getTypeName: type is external (%s)", result);
	return result;
}

char *
getIslProcedureName(Procedure p)
{
	char *	result	= procedure_name(p);

	if (isPrefixOf(prefixIdlAttribute, result))
		result += strlen(prefixIdlAttribute);
	return result;
}
