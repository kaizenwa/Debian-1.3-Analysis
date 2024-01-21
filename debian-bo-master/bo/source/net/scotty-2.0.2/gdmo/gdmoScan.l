/*
 * gdmoScan.l
 *
 * Implementation of a lexical scanner for GDMO files
 * (GDMO = Guidelines for the Definition of Managed Objects)
 * CCITT Rec. X.722 respectively ISO/IEC 10165-4
 *
 * Copyright (c) 1994, 1995
 *
 * M. Kernchen (email: M.Kernchen@tu-bs.de)
 * TU Braunschweig, Germany
 * Institute for Operating Systems and Computer Networks
 *
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that this copyright
 * notice appears in all copies.  The University of Braunschweig
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 */

%{	/* DEFINITIONS */

#include "gdmo.h"
#include "gdmoParser.h"

extern char	*gdmo_file;	/* source file name */
int		lineno;		/* current line number in source file */

/*
 * Global variable to keep track of the actual position in the file.
 */

long	gdmo_file_pos;

/* it's risky and only possible for flex, not for lex.
 * to have the same effect with lex one would have to add
 * gdmo_file_pos += yyleng;
 * in front of every action!!
 * but what should i do?
 */
#define YY_USER_ACTION { gdmo_file_pos += yyleng; }

/*
 * Variable to remember the starting delimiter of a delimited string.
 */

static char	delimiter;

static text_in_file	file_offset;	/* structure for delimited-string */

/*
 * Some lex scanners need the definition of a yywrap() function. Add
 * one if we are not using a flex scanner.
 */

%}

/* %x use exclusive start conditions (not %s) */

%x delimited_str object_id typeref valueref context_type
/* XXX maybe field_name is no longer needed!? */

DIGIT		[0-9]
UPCASE		[A-Z]
DOWNCASE	[a-z]
ALPHA_NUM	[a-zA-Z0-9]
WHITE_SPACE	[ \t]*

/* ASN.1 definitions: */
TYPE_NAME	{UPCASE}(-?{ALPHA_NUM}+)*
IDENTIFIER	{DOWNCASE}(-?{ALPHA_NUM}+)*
VALUE_NAME	{DOWNCASE}(-?{ALPHA_NUM}+)*
MODUL_NAME	{UPCASE}(-?{ALPHA_NUM}+)*
NUMBER		0|([1-9]{DIGIT}*)
		/* XXX correct?? */

/* X.722 definitions: */
LABEL_STRING	{DOWNCASE}(-?[a-zA-Z0-9\/]+)*

TYPE_REFERENCE	{MODUL_NAME}"."{TYPE_NAME}
VALUE_REFERENCE {MODUL_NAME}"."{VALUE_NAME}

/* FIELD_NAME	{VALUE_NAME} almost the same as LABEL_STRING */

UNMATCHED_CONSTRUCT	{UPCASE}(-?{UPCASE}+)*

%% /* RULES */
  /* !!comments in the RULE-section must not start in the 1st column,
   * except when they are part of an action!!
   *
   * !!the action of a rule has to start on the same line as the rule!!
   */

  /* \n
   *		{ lineno++; }
   * should be matched by the second rule!
   */

	/* first of all eat up WHITE SPACE! */
<INITIAL,object_id,typeref,valueref,context_type>{WHITE_SPACE}	{
		    /* eat up white space */
		}
<INITIAL,object_id,typeref,valueref,context_type>{WHITE_SPACE}\n	{
		    /* eat up white space followed by \n */
		    lineno++; /* next line number! */
		}

	/* eat up everything that seems to be a COMMENT */
<INITIAL,object_id,typeref,valueref,context_type>"--"(-?[^\-\n]+)*"--"	{
		    /* eat up comment ending with "--" */
		}
<INITIAL,object_id,typeref,valueref,context_type>"--"(-?[^\-\n]+)*-?\n	{
		    /* eat up comment ending with end of line */
		    lineno++; /* next line number! */
		}

	/* return some simple characters */
";"		{ return ';'; }
":"		{ return ':'; }
","		{ return ','; }

	/* here comes a DELIMITED STRING */
[\!\"\#\$\%\^\&\*\'\`\~\?\@\\]	{
		    /* where is the delimited string: */
		    file_offset.path = gdmo_file;
		    file_offset.pos = gdmo_file_pos+1; /* without delimiter */

		    delimiter = yytext[0];/* remember the starting delimiter */
		    BEGIN(delimited_str);
		}
<delimited_str>\n	{ lineno++; /* don't forget to count the lines */ }
<delimited_str>[^\!\"\#\$\%\^\&\*\'\`\~\?\@\\\n]*	{
		    /* eat up the text-string except delimiter or '\n' */
		}
<delimited_str>[\!\"\#\$\%\^\&\*\'\`\~\?\@\\]	{
		    /* maybe there is a better solution!? */
		    if (delimiter == yytext[0]) { /* the closing delimiter? */
			char c;
			if ((c = input()) != delimiter) {
			    unput(c); /* read one char to far */

			    /* found the end of delimited-string */
			    /* eval length of the delimited string */
			    file_offset.len = gdmo_file_pos - file_offset.pos;
/* without delimiter			    ++file_offset.len; */
/* XXX check it out again + 1 why? */
			    yylval.text_in_file_ptr = &file_offset;
			    BEGIN(INITIAL);
			    return T_DELIMITED_STRING;
			}
			/* double delimiter we're still in a
			 * delimited-string, eat up
			 * but don't forget to actualise gdmo_file_pos
			 */
			++gdmo_file_pos;
		    }
		    /* not the ending delimiter of this
		       delimited-string, eat up */
		}

  /* haven't found this in the recommendation yet:
   * (but seen in another GDMO-Compiler)
   * "LIBCOMMENT"			return(T_LIBCOMMENT);
   * "LIBAUTHOR"			return(T_LIBAUTHOR);

   * "COMMON-ERROR"			return(T_COMMON_ERR);
   * "NON-CONFIRMED"			return(T_NONCONFIRMED);
   *  non-confirmed scheinbar eine erweiterung von notification (OIM.gdmo)
   */

"LIBCOMMENT"	{ return T_LIBCOMMENT; }

	/* and now the TOKEN */
"AS"		{ return T_AS; }

"{"		{ BEGIN(object_id); return '{'; }
<object_id>{VALUE_REFERENCE}	{
		    yylval.string_val = yytext;
		    return T_OBJ_ID_PART;
		}
<object_id>{IDENTIFIER}	{ /* same as VALUE_REFERENCE */
		    yylval.string_val = yytext;
		    return T_OBJ_ID_PART;
		}
<object_id>{NUMBER}		{ /* XXX maybe return an int!? */
		    yylval.string_val = yytext;
		    return T_OBJ_ID_PART;
		}
<object_id>{IDENTIFIER}"("{NUMBER}")"	{
		    yylval.string_val = yytext;
		    return T_OBJ_ID_PART;
		}
  /* <object_id>"("	{ return '('; }
   * <object_id>")"	{ return ')'; }
   */
<object_id>"}"	{ BEGIN(INITIAL); return '}'; }

"PARAMETERS"	{ return T_PARAMETERS; }
"REGISTERED"	{ return T_REGISTERED; }
"SYNTAX"	{ BEGIN(typeref); return T_SYNTAX; }
<typeref>{TYPE_REFERENCE}	{
		    /* XXX maybe i have to further evaluate this */
		    yylval.string_val = yytext;
		    BEGIN(INITIAL);
		    return T_TYPE_REFERENCE;
		}
"WITH"		{ return T_WITH; }

"MANAGED"	{ return T_MANAGED; }
"OBJECT"	{ return T_OBJECT; }
"CLASS"		{ return T_CLASS; }
"DERIVED"	{ return T_DERIVED; }
"FROM"		{ return T_FROM; }
"CHARACTERIZED"	{ return T_CHARACTERIZED; }
"BY"		{ return T_BY; }
"CONDITIONAL"	{ return T_CONDITIONAL; }
"PACKAGES"	{ return T_PACKAGES; }
"PRESENT"	{ return T_PRESENT; }
"IF"		{ return T_IF; }

"PACKAGE"	{ return T_PACKAGE; }
"BEHAVIOUR"	{ return T_BEHAVIOUR; }
"ATTRIBUTES"	{ return T_ATTRIBUTES; }
"GROUPS"	{ return T_GROUPS; }
"ACTIONS"	{ return T_ACTIONS; }
"NOTIFICATIONS"	{ return T_NOTIFICATIONS; }
"REPLACE-WITH-DEFAULT"	{ return T_REPLACE_WITH_DEFAULT; }
"DEFAULT"	{ return T_DEFAULT; }
"INITIAL"	{ return T_INITIAL; }
"VALUE"		{ BEGIN(valueref); return T_VALUE; }
<valueref>"DERIVATION"	{
		    /* it isn't really a value-reference,
		     * but a value-reference must not
		     * start with DERIVATION !!
		     */
		    BEGIN(INITIAL);
		    return T_DERIVATION;
		}
<valueref>{VALUE_REFERENCE}	{
		    /* XXX maybe i have to further evaluate this */
		    yylval.string_val = yytext;
		    BEGIN(INITIAL);
		    return T_VALUE_REFERENCE;
		}
"PERMITTED"	{ return T_PERMITTED; }
"REQUIRED"	{ return T_REQUIRED; }
"VALUES"	{ BEGIN(typeref); return T_VALUES; }
"RULE"		{ return T_RULE; }
"GET"		{ return T_GET; }
"REPLACE"	{ return T_REPLACE; }
"GET-REPLACE"	{ return T_GET_REPLACE; }
"ADD"		{ return T_ADD; }
"REMOVE"	{ return T_REMOVE; }
"ADD-REMOVE"	{ return T_ADD_REMOVE; }

"PARAMETER"	{ return T_PARAMETER; }
"CONTEXT"	{ BEGIN(context_type); return T_CONTEXT; }
<context_type>"ACTION-INFO"	{
		    BEGIN(INITIAL);
		    return T_ACTION_INFO;
		}
<context_type>"ACTION-REPLY"	{
		    BEGIN(INITIAL);
		    return T_ACTION_REPLY;
		}
<context_type>"EVENT-INFO"	{
		    BEGIN(INITIAL);
		    return T_EVENT_INFO;
		}
<context_type>"EVENT-REPLY"	{
		    BEGIN(INITIAL);
		    return T_EVENT_REPLY;
		}
<context_type>"SPECIFIC-ERROR"	{
		    BEGIN(INITIAL);
		    return T_SPECIFIC_ERROR;
		}
<context_type>{TYPE_REFERENCE}"."{IDENTIFIER}	{ /* XXX not sure */
		    /* XXX maybe i have to further evaluate this */
		    yylval.string_val = yytext;
		    BEGIN(INITIAL);
		    return T_TYPE_REFERENCE_ID;
		}

"NAME"		{ return T_NAME; }
"BINDING"	{ return T_BINDING; }
"SUBORDINATE"	{ return T_SUBORDINATE; }
"AND"		{ return T_AND; }
"SUBCLASSES"	{ return T_SUBCLASSES; }
"NAMED"		{ return T_NAMED; }
"SUPERIOR"	{ return T_SUPERIOR; }
"CREATE"	{ return T_CREATE; }
"DELETE"	{ return T_DELETE; }
"WITH-REFERENCE-OBJECT" { return T_WITH_REFERENCE_OBJECT; }
"WITH-AUTOMATIC-INSTANCE-NAMING" { return T_WITH_AUTOMATIC_INSTANCE_NAMING; }
"ONLY-IF-NO-CONTAINED-OBJECTS" { return T_ONLY_IF_NO_CONTAINED_OBJECTS; }
"DELETES-CONTAINED-OBJECTS" { return T_DELETES_CONTAINED_OBJECTS; }

"ATTRIBUTE"	{ return T_ATTRIBUTE; }
"MATCHES"	{ return T_MATCHES; }
"FOR"		{ return T_FOR; }
"EQUALITY"	{ return T_EQUALITY; }
"ORDERING"	{ return T_ORDERING; }
"SUBSTRINGS"	{ return T_SUBSTRINGS; }
"SET-COMPARISON" { return T_SET_COMPARISON; }
"SET-INTERSECTION" { return T_SET_INTERSECTION; }

"GROUP"		{ return T_GROUP; }
"ELEMENTS"	{ return T_ELEMENTS; }
"FIXED"		{ return T_FIXED; }
"DESCRIPTION"	{ return T_DESCRIPTION; }

"DEFINED"	{ return T_DEFINED; }

"ACTION"	{ return T_ACTION; }
"MODE"		{ return T_MODE; }
"CONFIRMED"	{ return T_CONFIRMED; }
"INFORMATION"	{ return T_INFORMATION; }
"REPLY"		{ return T_REPLY; }

"NOTIFICATION"	{ return T_NOTIFICATION; }
"IDS"		{ /* BEGIN(field_name); */ return T_IDS; }

"NON-CONFIRMED"	{ return T_NON_CONFIRMED; }

{LABEL_STRING}	{
		    yylval.string_val = yytext;
		    return T_TEMPLATE_LABEL;
		}

  /* because FIELD_NAME is almost the same as LABEL_STRING
   * <field_name>{FIELD_NAME}	{
   * 		       * XXX maybe i have to further evaluate this *
   * 		       yylval.string_val = yytext;
   * 		       BEGIN(INITIAL);
   * 		       return T_FIELD_NAME;
   * 		   }
   */

{UNMATCHED_CONSTRUCT} { /* match every unmatched CONSTRUCT-NAME */
		    return yytext[0]; /* XXX noch schoener loesen! */
		}

.		{ /* match every unmatched character */
/*		    fprintf(stderr, "%s:%d: token `%s' not matched\n",
 *			    gdmo_file, lineno, yytext);
 */
		    return yytext[0]; /* to force error-message */
		}

<object_id,typeref,valueref,context_type>. {
		    /* match every unmatched character in exclusive start
		     * condition, probably an error, so put the character
		     * back and switch to INITIAL
		     */
/*		    unput(yytext[0]);
 *		    BEGIN(INITIAL);
 */
		    BEGIN(INITIAL);
		    return yytext[0]; /* to force error-message */
		}

<<EOF>>		{
		    /* reached the end of source file, reset the global
		     * variables for the file position and
		     * reset start condition, then exit
		     */
/*		    gdmo_file_pos = -1;
 *		    lineno = 1;
 */
		    BEGIN(INITIAL);	/* XXX wirklich n"otig ? */
		    yyterminate();
		}

%%

/*
 * Define yywrap so that we always have one.
 */

#ifndef yywrap
int
yywrap()
{
	return 1;
}
#endif
