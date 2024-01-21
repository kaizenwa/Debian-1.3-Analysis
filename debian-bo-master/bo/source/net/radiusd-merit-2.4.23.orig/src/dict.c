/*
 *
 *	RADIUS
 *	Remote Authentication Dial In User Service
 *
 *
 *	Livingston Enterprises, Inc.
 *	6920 Koll Center Parkway
 *	Pleasanton, CA   94566
 *
 *	Copyright 1992 Livingston Enterprises, Inc.
 *
 *	Permission to use, copy, modify, and distribute this software for any
 *	purpose and without fee is hereby granted, provided that this
 *	copyright and permission notice appear on all copies and supporting
 *	documentation, the name of Livingston Enterprises, Inc. not be used
 *	in advertising or publicity pertaining to distribution of the
 *	program without specific prior permission, and notice be given
 *	in supporting documentation that copying and distribution is by
 *	permission of Livingston Enterprises, Inc.
 *
 *	Livingston Enterprises, Inc. makes no representations about
 *	the suitability of this software for any purpose.  It is
 *	provided "as is" without express or implied warranty.
 *
 * Public entry points in this file:
 *
 * dict_attrfind
 * dict_attrget
 * dict_init
 * dict_valfind
 * dict_valget
 *
 */

static char     sccsid[] =
		"@(#)dict.c 1.2 Copyright 1992 Livingston Enterprises Inc";

static char     rcsid[] = "$Id: dict.c,v 1.9 1996/05/17 14:19:03 web Exp $";

#include	<stdio.h>
#include	<stdlib.h>
#include	<time.h>
#include	<sys/types.h>
#include	<ctype.h>
#include	<netinet/in.h>
#include	<syslog.h>

#include	"radius.h"

extern int      debug_flag;
extern char    *radius_dir;

static DICT_ATTR *dictionary_attributes;
static DICT_VALUE *dictionary_values;

/*************************************************************************
 *
 *	Function: dict_init
 *
 *	Purpose: Initialize the dictionary.  Read all ATTRIBUTES into
 *		 the dictionary_attributes list.  Read all VALUES into
 *		 the dictionary_values list.
 *
 *************************************************************************/

int
dict_init ()

{
	FILE           *dictfd;
	char            dummystr[AUTH_ID_LEN];
	char            namestr[AUTH_ID_LEN];
	char            valstr[AUTH_ID_LEN];
	char            attrstr[AUTH_ID_LEN];
	char            typestr[AUTH_ID_LEN];
	int             line_no;
	DICT_ATTR      *attr;
	DICT_VALUE     *dval;
	char            buffer[256];
	int             value;
	int             type;
	static char    *func = "dict_init";

	dprintf(2, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

	sprintf (buffer, "%s/%s", radius_dir, RADIUS_DICTIONARY);
	if ((dictfd = fopen (buffer, "r")) == (FILE *) NULL)
	{
		fprintf (stderr, "%s: Couldn't open dictionary: %s\n",
			 func, buffer);
		return (-1);
	}

	line_no = 0;
	while (fgets (buffer, sizeof (buffer), dictfd) != (char *) NULL)
	{
		line_no++;

		/* Skip empty space */
		if (*buffer == '#' || *buffer == '\0' || *buffer == '\n')
		{
			continue;
		}

		if (strncmp (buffer, "ATTRIBUTE", 9) == 0)
		{

			/* Read the ATTRIBUTE line */
			if (sscanf (buffer, "%s%s%s%s", dummystr, namestr,
				    valstr, typestr) != 4)
			{
				fprintf (stderr,
			    "%s: Invalid attribute on line %d of dictionary\n",
					 func, line_no);
				return (-1);
			}

			/*
			 * Validate all entries
			 */
			if (strlen (namestr) > NAME_LENGTH)
			{
				fprintf (stderr,
			   "%s: Invalid name length on line %d of dictionary\n",
					 func, line_no);
				return (-1);
			}

			if (!isdigit (*valstr))
			{
				fprintf (stderr,
				 "%s: Invalid value on line %d of dictionary\n",
					 func, line_no);
				return (-1);
			}
			value = atoi (valstr);

			if (strcmp (typestr, "string") == 0)
			{
				type = PW_TYPE_STRING;
			}
			else if (strcmp (typestr, "integer") == 0)
			{
				type = PW_TYPE_INTEGER;
			}
			else if (strcmp (typestr, "ipaddr") == 0)
			{
				type = PW_TYPE_IPADDR;
			}
			else if (strcmp (typestr, "date") == 0)
			{
				type = PW_TYPE_DATE;
			}
			else if (strcmp (typestr, "octets") == 0)
			{
				type = PW_TYPE_OCTETS;
			}
			else if (strcmp (typestr, "vendor") == 0)
			{
				type = PW_TYPE_VENDOR;
			}
			else
			{
				fprintf (stderr,
				  "%s: Invalid type on line %d of dictionary\n",
					 func, line_no);
				return (-1);
			}

			/* Create a new attribute for the list */
			if ((attr =
				(DICT_ATTR *) malloc (sizeof (DICT_ATTR)))
							== (DICT_ATTR *) NULL)
			{
				fprintf (stderr, "%s: FATAL out of memory\n",
					func);
				abort ();
			}
			strcpy (attr->name, namestr);
			attr->value = value;
			attr->type = type;

			/* Insert it into the list */
			attr->next = dictionary_attributes;
			dictionary_attributes = attr;
		}
		else if (strncmp (buffer, "VALUE", 5) == 0)
		{
			/* Read the VALUE line */
			if (sscanf (buffer, "%s%s%s%s", dummystr, attrstr,
				    namestr, valstr) != 4)
			{
				fprintf (stderr,
			   "%s: Invalid value entry on line %d of dictionary\n",
					 func, line_no);
				return (-1);
			}

			/*
			 * Validate all entries
			 */
			if (strlen (attrstr) > NAME_LENGTH)
			{
				fprintf (stderr,
		      "%s: Invalid attribute length on line %d of dictionary\n",
					 func, line_no);
				return (-1);
			}

			if (strlen (namestr) > NAME_LENGTH)
			{
				fprintf (stderr,
			   "%s: Invalid name length on line %d of dictionary\n",
					 func, line_no);
				return (-1);
			}

			if (!isdigit (*valstr))
			{
				fprintf (stderr,
				 "%s: Invalid value on line %d of dictionary\n",
					 func, line_no);
				return (-1);
			}
			value = atoi (valstr);

			/* Create a new VALUE entry for the list */
			if ((dval =
				(DICT_VALUE *) malloc (sizeof (DICT_VALUE)))
							== (DICT_VALUE *) NULL)
			{
				fprintf (stderr, "%s: FATAL out of memory\n",
					func);
				abort ();
			}
			strcpy (dval->attrname, attrstr);
			strcpy (dval->name, namestr);
			dval->value = value;

			/* Insert it into the list */
			dval->next = dictionary_values;
			dictionary_values = dval;
		}
	}
	fclose (dictfd);
	return (0);
} /* end of dict_init () */

/*************************************************************************
 *
 *	Function: dict_attrget
 *
 *	Purpose: Return the full attribute structure based on the
 *		 attribute id number.
 *
 *************************************************************************/

DICT_ATTR *
dict_attrget (attribute)

int             attribute;

{
	DICT_ATTR      *attr;

	attr = dictionary_attributes;
	while (attr != (DICT_ATTR *) NULL)
	{
		if (attr->value == attribute)
		{
			return (attr);
		}
		attr = attr->next;
	}
	return ((DICT_ATTR *) NULL);
} /* end of dict_attrget () */

/*************************************************************************
 *
 *	Function: dict_attrfind
 *
 *	Purpose: Return the full attribute structure based on the
 *		 attribute name.
 *
 *************************************************************************/

DICT_ATTR *
dict_attrfind (attrname)

char           *attrname;
{
	DICT_ATTR      *attr;

	attr = dictionary_attributes;
	while (attr != (DICT_ATTR *) NULL)
	{
		if (strcasecmp (attr->name, attrname) == 0)
		{
			return (attr);
		}
		attr = attr->next;
	}
	return ((DICT_ATTR *) NULL);
} /* end of dict_attrfind () */

/*************************************************************************
 *
 *	Function: dict_valfind
 *
 *	Purpose: Return the full value structure based on the
 *		 value name.
 *
 *************************************************************************/

DICT_VALUE *
dict_valfind (valname)

char           *valname;

{
	DICT_VALUE     *val;

	val = dictionary_values;
	while (val != (DICT_VALUE *) NULL)
	{
		if (strcasecmp (val->name, valname) == 0)
		{
			return (val);
		}
		val = val->next;
	}
	return ((DICT_VALUE *) NULL);
} /* end of dict_valfind () */

/*************************************************************************
 *
 *	Function: dict_valget
 *
 *	Purpose: Return the full value structure based on the
 *		 actual value and the associated attribute name.
 *
 *************************************************************************/

DICT_VALUE *
dict_valget (value, attrname)

UINT4           value;
char           *attrname;

{
	DICT_VALUE     *val;

	val = dictionary_values;
	while (val != (DICT_VALUE *) NULL)
	{
		if (strcmp (val->attrname, attrname) == 0 &&
				val->value == value)
		{
			return (val);
		}
		val = val->next;
	}
	return ((DICT_VALUE *) NULL);
} /* end of dict_valget () */
