/**
 *
 * $Id: RepType.c,v 1.6 1996/12/30 07:36:30 u27113 Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/
static char rcsid[] = "$Id: RepType.c,v 1.6 1996/12/30 07:36:30 u27113 Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/RepType.h>

#include <stdio.h>
#ifdef HAVE_STRING_H
#define ANSI_STRING
#include <string.h>
#else
#include <strings.h>
#endif
#include <ctype.h>

#include <XmI/DebugUtil.h>

static XmRepTypeList rep_types = NULL;
static int number_of_types = 0;
static int max_number_of_types = 0;

/* The linux libc boys seem to have removed this from string.h.  I wonder
 * why -- it seems a pretty stupid thing to do.. libc-5.3.12 */
extern int strcasecmp();

static String *
__XmRepTypeCopyList(String *names, Boolean cap)
{
    String *newNames;
    int i,j;
    int num_names=0;
    String *tmp = names;

    while (tmp != NULL)
    {
	num_names++;
	tmp++;
    }
    
    newNames = (String*)XtCalloc(num_names, sizeof(String));
    
    for (i=0; i<num_names; i++)
    {
	if (cap)
	{
	    newNames[i] = (String)XtMalloc(strlen(names[i]) + 3);
	    strcpy(newNames[i], "Xm");
	    for (j=0; j<strlen(names[i]); j++)
		newNames[i][j+2] = toupper(names[i][j]);
	    newNames[strlen(names[i] + 2)] = '\0';
	}
	else
	    newNames[i] = XtNewString(names[i]);
    }

    return newNames;
}

static int
__XmRepTypeStringToValue(XmRepTypeEntry entry, 
			 String value_name)
{
    int  length = strlen(value_name);
    char *pStr;
    int  i;

    XdbDebug(__FILE__, NULL, "__XmRepTypeStringToValue(%s,%s)\n",
		entry->rep_type_name, value_name);

    pStr = value_name;
    if ( (length > 2) &&
         (tolower(pStr[0]) == 'x') && (tolower(pStr[1]) == 'm') )
        pStr += 2;

    for ( i = entry->num_values - 1; i >= 0; --i )
        if ( strcasecmp(pStr, entry->value_names[i]) == 0 )
            break;

    if ( i >= 0 ) {
        if ( entry->values )
            i = entry->values[i];
        return i;
    } else
        return -1;
}

static String
__XmRepTypeValueToString(XmRepTypeEntry entry, 
			 unsigned char value)
{
    int i = value;

    Boolean error;

    if (entry->values == NULL) 
	error = value >= entry->num_values;
    else
    {
	error = True;
	for (i=0; i<entry->num_values; i++)
	    if (entry->values[i] == value)
	    {
		error = False;
		value = i;
		break;
	    }
    }

    if (error) 
	return NULL;
    else
	return entry->value_names[i];
}

static Boolean
__XmCvtStringToRep(Display *display,
		   XrmValue *args,
		   Cardinal *num_args,
		   XrmValue *from,
		   XrmValue *to,
		   XtPointer *converter_data)
{
    XmRepTypeEntry entry;
    int result;
    static unsigned char value;

    XdbDebug(__FILE__, NULL, "__XmCvtStringToRep()\n");

    entry = XmRepTypeGetRecord((XmRepTypeId)*(int*)args[0].addr);

    if (from->addr == NULL) {
	XtDisplayStringConversionWarning(display, from->addr,
					 entry->rep_type_name);
	return False;
    }

    result = __XmRepTypeStringToValue(entry, (String) from->addr);

    if (result < 0)
    {
	XtDisplayStringConversionWarning(display, from->addr,
					 entry->rep_type_name);

	return False;
    }

    value = (unsigned char)result;

    if (to->addr)
	*(unsigned char *)to->addr = value;
    else
    {
	to->addr = (XtPointer)&value;
	to->size = sizeof(unsigned char);
    }
    
    return True;
}


static Boolean
__XmCvtRepToString(Display *display,
		   XrmValue *args,
		   Cardinal *num_args,
		   XrmValue *from,
		   XrmValue *to,
		   XtPointer *converter_data)
{
    XmRepTypeEntry entry;
    static String value;

    entry = XmRepTypeGetRecord((XmRepTypeId)*(int*)args[0].addr);

    if (from->addr == NULL) {
	String params[1];
	Cardinal numParams = 1;

	params[0] = (String) from->addr;
	XtAppWarningMsg(XtDisplayToApplicationContext(display),
			"conversionError", entry->rep_type_name,
			"XtToolkitError",
			"Cannot convert value NULL of type %s to type string",
			params, &numParams);

	return False;
    }

    value = __XmRepTypeValueToString(entry, *(unsigned char*)from->addr);

    if (value == NULL)
    {
	String params[2];
	Cardinal numParams = 2;
	char ASCIIValue[10];

	value = entry->value_names[0];
	sprintf (ASCIIValue, "%i", (int)from->addr);
	params[0] = (String)ASCIIValue;
	params[1] = (String)from->addr;
	XtAppWarningMsg(XtDisplayToApplicationContext(display),
			"conversionError", entry->rep_type_name,
			"XtToolkitError",
			"Cannot convert value %s of type %s to type string",
			params, &numParams);

	return False;
    }

    if (to->addr)
	*(String *)to->addr = value;
    else

	to->addr = (XtPointer)&value;

    to->size = sizeof(String);
    
    return True;
}

void 
XmRepTypeAddReverse(XmRepTypeId rep_type_id)
{
    XmRepTypeEntry entry = XmRepTypeGetRecord(rep_type_id);
    XtConvertArgRec Args[1];

    if (!entry->reverse_installed)
    {
	entry->reverse_installed = True;

	Args[0].address_mode = XtImmediate;
	Args[0].address_id = (XtPointer)(int)entry->rep_type_id;
	Args[0].size = sizeof(XmRepTypeId);

	XtSetTypeConverter(entry->rep_type_name, XmRString,
			   (XtTypeConverter) __XmCvtRepToString,
			   Args, 1, XtCacheNone, NULL);
    }

    XtFree((char*)entry);
}

XmRepTypeId
XmRepTypeGetId(String rep_type)
{
    int i;

    for (i=0; i<number_of_types; i++)
	if (!strcmp(rep_type, rep_types[i].rep_type_name))
	    return rep_types[i].rep_type_id;

    return XmREP_TYPE_INVALID;
}

String *
XmRepTypeGetNameList(XmRepTypeId rep_type_id,
		     Boolean use_uppercase_format)
{
    String *name_list = NULL;
    XmRepTypeEntry entry = XmRepTypeGetRecord(rep_type_id);

    if (entry)
	name_list = __XmRepTypeCopyList(entry->value_names, use_uppercase_format);

    XtFree((char*)entry);

    return name_list;
}

XmRepTypeEntry 
XmRepTypeGetRecord(XmRepTypeId rep_type_id)
{
    int i;

    XmRepTypeEntry entry = (XmRepTypeEntry)XtMalloc(sizeof(XmRepTypeEntryRec));

    for (i=0; i<number_of_types; i++)
	if (rep_type_id == rep_types[i].rep_type_id)
	{
	    *entry = rep_types[i];
	    return entry;
	}

    XtFree((char*)entry);
    return NULL;
}

XmRepTypeList
XmRepTypeGetRegistered()
{
    XmRepTypeList list = (XmRepTypeList)XtCalloc(number_of_types + 1, sizeof(XmRepTypeEntryRec));
    int i;
    
    if (number_of_types == 0)
	XmRegisterConverters();

    for (i=0; i<number_of_types; i++)
	list[i] = rep_types[i];

    list[number_of_types].rep_type_name = NULL;

    return list;
}

void
XmRepTypeInstallTearOffModelConverter()
{
    static char *tear_off_models[] = {
	"tear_off_enabled",
	"tear_off_disabled"
    };

    XmRepTypeRegister(XmRTearOffModel,
		      tear_off_models,
		      NULL, 2);
}

XmRepTypeId
XmRepTypeRegister(String rep_type,
		  String *value_names,
		  unsigned char *values,
		  unsigned char num_values)
{
#if 0
    XmRepTypeEntry entry = XmRepTypeGetRecord(XmRepTypeGetId(rep_type));
    XtConvertArgRec Args[1];
    int i;

    if (entry)
    {
	XmRepTypeId id;
	id = entry->rep_type_id;
	XtFree((char*)entry);
	return id;
    }
#else
    XmRepTypeId id = XmRepTypeGetId(rep_type);
    XtConvertArgRec Args[1];
    int i;

    if (id != XmREP_TYPE_INVALID)
	return id;
#endif

    if (number_of_types + 1 > max_number_of_types)
    {
	max_number_of_types = (max_number_of_types + 1) *2;

	rep_types = (XmRepTypeList)XtRealloc((char*)rep_types, sizeof(XmRepTypeListRec) * max_number_of_types);
    }

    rep_types[number_of_types].rep_type_name = XtNewString(rep_type);
    rep_types[number_of_types].value_names = (char**)XtMalloc(sizeof(char*) * (num_values+1));
    for (i=0; i<num_values; i++)
	rep_types[number_of_types].value_names[i] = XtNewString(value_names[i]);
    rep_types[number_of_types].value_names[i] = NULL;
    if (values)
    {
	rep_types[number_of_types].values = (unsigned char*)XtMalloc(sizeof(unsigned char) * num_values);
	for (i=0; i<num_values; i++)
	    rep_types[number_of_types].values[i] = values[i];
    }
    else
	rep_types[number_of_types].values = NULL;

    rep_types[number_of_types].num_values = num_values;
    rep_types[number_of_types].reverse_installed = False;
    rep_types[number_of_types].rep_type_id = number_of_types;

    Args[0].address_mode = XtImmediate;
    Args[0].address_id = (XtPointer)number_of_types;
    Args[0].size = sizeof(XmRepTypeId);

    XtSetTypeConverter(XmRString, rep_type,
                       __XmCvtStringToRep,
                       Args, 1, XtCacheNone, NULL);

    /* now install the forward converter */

    return number_of_types++;
}

Boolean 
XmRepTypeValidValue(XmRepTypeId rep_type_id,
		    unsigned char test_value,
		    Widget enable_default_warning)
{
    XmRepTypeEntry entry = XmRepTypeGetRecord(rep_type_id);

    if (entry)
    {
	int i = 0;

	if (entry->values)
	{
	    while (i < entry->num_values)
	    {
		if (test_value == entry->values[i])
		{
		    XtFree((char*)entry);
		    return True;
		}
		i++;
	    }
	}
	else
	{
	    String *tmp = entry->value_names;

	    while (*tmp)
	    {
		i++;
		tmp++;
	    }

	    if (test_value <= i++)
	    {
		XtFree((char*)entry);
		return True;
	    }
	}

	XtFree((char*)entry);
	_XmWarning(enable_default_warning, "XmRepTypeValidValue: invalid type.\n");
	return False;
    }

    _XmWarning(enable_default_warning, "XmRepTypeValidValue: missing type.\n");
    return False;
}

