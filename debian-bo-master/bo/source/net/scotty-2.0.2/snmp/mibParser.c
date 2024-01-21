/*
 * mibParser.c
 *
 * Functions to parse MIB files for creating a MIB tree. The structure
 * of MIB information must meet the definitions specified in RFC 1155.
 * This code is partly derived from the MIB parser of the CMU SNMP
 * implementation, but it now looks quite different as we have made 
 * many changes and rewritten large parts of it. Please read the 
 * Copyright notice of the CMU source.
 *
 * Copyright (c) 1994, 1995
 *
 * Sven Schmidt, J. Schoenwaelder
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

#include "snmp.h"
#include "mib.h"

#include <sys/stat.h>

#define SYMBOL_MAXLEN 64	/* Maximum # characters in a symbol. */

/*
 * Token used by the MIB parser.
 */

#define ERROR		-2

#define DEFINITIONS	51
#define EQUALS		52
#define BEGIN		53
#define IMPORTS		54
#define EXPORTS		55
#define END		57
#define COMMENT		58
#define LABEL		59
#define	CONTINUE	60

#define SYNTAX		70

#define LEFTBRACKET	80
#define RIGHTBRACKET	81
#define	LEFTPAREN	82
#define RIGHTPAREN	83
#define COMMA		84
#define SEMICOLON	85

#define ACCESS		90
#define READONLY	91
#define READWRITE	92
#define READCREATE	93
#define	WRITEONLY	94
#define NOACCESS	95
#define FORNOTIFY	96

#define STATUS		100
#define MANDATORY	101
#define OPTIONAL	102
#define OBSOLETE	103
#define CURRENT		104
#define DEPRECATED	105
#define RECOMMENDED	106

#define OBJGROUP	110
#define OBJECTS		111
#define OBJECTIDENTITY	112

#define MODULECOMP	120
#define REFERENCE	121

#define MODULEIDENTITY	130
#define LASTUPDATED	153
#define ORGANIZATION	154
#define CONTACTINFO	155

#define NOTIFYTYPE	156

#define TRAPTYPE	157

#define	CAPABILITIES	158
#define	FROM		158

#define TEXTUALCONV	160
#define DISPLAYHINT	161

#define OBJTYPE		186
#define PUNCT		127
#define NUMBER		129
#define DESCRIPTION	135
#define QUOTESTRING	136
#define INDEX		137
#define DEFVAL		138
#define IMPLIED		139
#define SIZE		140
#define AUGMENTS	146
#define UNITS		149
#define NUM_ENTRIES	151

#define	HASHTAB_SIZE	17			/* size of hash table */
#define	HASH(x)		(x % HASHTAB_SIZE)	/* Hash function      */

#define NODEHASHSIZE	127			/* hashsize for nodes */

#define MAXTC		128

#define IN_MIB		1
#define OUT_OF_MIB	2

/*
 * Structure definition to hold the keyword table:
 */

typedef struct Keyword {
   char			*name;		/* keyword name			    */
   int			key;		/* value			    */
   int			hash;		/* hash of name			    */
   struct Keyword	*nextPtr;	/* pointer to next in hash table    */
} Keyword;

static Keyword keywords[] =
{
   { "DEFINITIONS",		DEFINITIONS,		0 },
   { "BEGIN",			BEGIN,			0 },
   { "END",			END,			0 },
   { "IMPORTS",			IMPORTS,		0 },
   { "FROM",			FROM,			0 },
   { "EXPORTS",			EXPORTS,		0 },
   { "::=",			EQUALS,			0 },

   { "{",			LEFTBRACKET,		0 },
   { "}",			RIGHTBRACKET,		0 },
   { "(",			LEFTPAREN,		0 },
   { ")",			RIGHTPAREN,		0 },
   { ",",			COMMA,			0 },
   { ";",			SEMICOLON,		0 },

   { "SYNTAX",			SYNTAX,			0 },

   { "INTEGER",			ASN1_INTEGER,		0 },
   { "OCTETSTRING",		ASN1_OCTET_STRING,	0 },
   { "OCTET",			CONTINUE,		0 },
   { "OBJECTIDENTIFIER",	ASN1_OBJECT_IDENTIFIER,	0 },
   { "OBJECT",			CONTINUE,		0 },
   { "BIT",			CONTINUE,		0 },
   { "NULL",			ASN1_NULL,		0 },
   { "SEQUENCE",		ASN1_SEQUENCE,		0 },
   { "OF",			ASN1_SEQUENCE_OF,	0 },
   { "Integer32",		Integer32,		0 },
   { "IpAddress",		ASN1_IpAddress,		0 },
   { "NetworkAddress",		NetworkAddress,		0 },
   { "Counter",			Counter,		0 },
   { "Counter32",		ASN1_Counter32,		0 },
   { "Gauge",			ASN1_Gauge32,		0 },
   { "Gauge32",			Gauge,			0 },
   { "TimeTicks",		ASN1_TimeTicks,		0 },
   { "Opaque",			ASN1_Opaque,		0 },
   { "NsapAddress",		ASN1_NsapAddress,	0 },
   { "Counter64",		ASN1_Counter64,		0 },
   { "UInteger32",		ASN1_UInteger32,	0 },
   { "Unsigned32",		ASN1_Gauge32,		0 },

   { "ACCESS",			ACCESS,			0 },
   { "read-only",		READONLY,		0 },
   { "read-write",		READWRITE,		0 },
   { "read-create",		READCREATE,		0 },
   { "write-only",		WRITEONLY,		0 },
   { "not-accessible",		NOACCESS,		0 },
   { "accessible-for-notify",   FORNOTIFY,		0 },

   { "STATUS",			STATUS,			0 },
   { "mandatory",		MANDATORY,		0 },
   { "optional",		OPTIONAL,		0 },
   { "obsolete",		OBSOLETE,		0 },
   { "current",			CURRENT,		0 },
   { "deprecated",		DEPRECATED,		0 },
   { "recommended",		RECOMMENDED,		0 },

   { "DESCRIPTION",		DESCRIPTION,		0 },

   { "OBJECT-GROUP",		OBJGROUP,		0 },
   { "OBJECTS",			OBJECTS,		0 },
   { "OBJECT-TYPE",		OBJTYPE,		0 },

   { "OBJECT-IDENTITY",		OBJECTIDENTITY,		0 },

   { "MODULE-COMPLIANCE",	MODULECOMP,		0 },
   { "REFERENCE",		REFERENCE,		0 },

   { "MODULE-IDENTITY",		MODULEIDENTITY,		0 },
   { "LAST-UPDATED",		LASTUPDATED,		0 },
   { "ORGANIZATION",		ORGANIZATION,		0 },
   { "CONTACT-INFO",		CONTACTINFO,		0 },

   { "NOTIFICATION-TYPE",	NOTIFYTYPE,		0 },

   { "TRAP-TYPE",		TRAPTYPE,		0 },

   { "AGENT-CAPABILITIES",	CAPABILITIES,		0 },

   { "TEXTUAL-CONVENTION",	TEXTUALCONV,		0 },
   { "DISPLAY-HINT",		DISPLAYHINT,		  },

   { "AUGMENTS",		AUGMENTS,		0 },
   { "UNITS",			UNITS,			0 },
   { "NUM-ENTRIES",		NUM_ENTRIES,		0 },
   { "INDEX",			INDEX,			0 },
   { "IMPLIED",			IMPLIED,		0 },
   { "DEFVAL",			DEFVAL,			0 },
   { "SIZE",			SIZE,			0 },
   { "MAX-ACCESS",		ACCESS,			0 },
   { NULL }
};

struct subid {			/* Element of an object identifier          */
   char		*parentName;	/* with an integer subidentifier or         */
   char		*label;		/* a textual string label, or both.         */
   int		subid;
   struct subid	*next;
};

/*
 * Some other useful module global variables:
 */

static FILE *fp;		/* The current FILE pointer. */
static int lastchar = ' ';	/* The last read character. */
static int line = 0;		/* The current line number. */

static Keyword  *hashtab[HASHTAB_SIZE];

/*
 * A faster strcmp(). Note, some compiler optimize strcmp() et.al.
 * themself, so this is not always the best solution.
 */

#define fstrcmp(a,b)  ((a)[0] != (b)[0] || (a)[1] != (b)[1] || \
			strcmp((a), (b)))


/*
 * Forward declarations for procedures defined later in this file:
 */

static void
AddNewNode		_ANSI_ARGS_((MIB_Node **nodeList, char *label, 
				     char *parentName, u_int subid));
static MIB_IntEnum*
ScanIntEnums		_ANSI_ARGS_((char *str));

static int
ReadIntEnums		_ANSI_ARGS_((char **strPtr));

static MIB_TextConv*
CreateTC		_ANSI_ARGS_((char *name, int syntax, char *displayHint,
				     char *enums));
static MIB_Node*
ParseFile		_ANSI_ARGS_((FILE *fp));

static int
ParseHeader		_ANSI_ARGS_((FILE *fp, char *keyword));

static int
ParseASN1Type		_ANSI_ARGS_((FILE *fp, char *keyword));

static MIB_Node*
ParseModuleCompliance	_ANSI_ARGS_((FILE *fp, char *name, 
				     MIB_Node **nodeList));
static MIB_Node*
ParseModuleIdentity	_ANSI_ARGS_((FILE *fp, char *name, 
				     MIB_Node **nodeList));
static MIB_Node*
ParseNotificationType	_ANSI_ARGS_((FILE *fp, char *name,
				     MIB_Node **nodeList));
static int
ParseCapabilitiesType	_ANSI_ARGS_((FILE *fp));

static int
ParseTrapType		_ANSI_ARGS_((FILE *fp, char *name));

static MIB_Node*
ParseObjectGroup	_ANSI_ARGS_((FILE *fp, char *name,
				     MIB_Node **nodeList));
static MIB_Node*
ParseObjectIdentity	_ANSI_ARGS_((FILE *fp, char *name,
				     MIB_Node **nodeList));
static MIB_Node*
ParseObjectID		_ANSI_ARGS_((FILE *fp, char *name,
				     MIB_Node **nodeList));
static MIB_Node*
ParseObjectType		_ANSI_ARGS_((FILE *fp, char *name,
				     MIB_Node **nodeList));

static void
HashKeywords		_ANSI_ARGS_((void));

static int
ReadKeyword		_ANSI_ARGS_((FILE *fp, char *keyword));

static struct subid *
ReadSubID		_ANSI_ARGS_((FILE *fp));


/*
 * Create a new node and link it into the list.
 */

static void
AddNewNode (nodeList, label, parentName, subid)
     MIB_Node **nodeList;
     char *label;
     char *parentName;
     u_int subid;
{
    MIB_Node *newPtr = MIB_MallocNode (label);
    newPtr->parentName = ckstrdup (parentName);
    newPtr->syntax  = ASN1_OBJECT_IDENTIFIER;
    newPtr->subid   = subid;
    newPtr->nextPtr = *nodeList;
    *nodeList = newPtr;
}

/*
 * MIB_Parse() opens a MIB file specified by file and returns a tree
 * of objects in that MIB. The function returns a pointer to the
 * "root" of the MIB, or NULL if an error occurred.
 */

MIB_Node*
MIB_Parse (file, frozen, root)
     char *file;
     char *frozen;
     MIB_Node *root;
{
    MIB_Node *nodePtr = NULL;
    struct stat stbuf;
    time_t mib_mtime = 0, frozen_mtime = 0;

    if ((fp = fopen (file, "r")) == NULL) {
	return NULL;
    }

    mib_FileName = ckstrdup (file);
    if (stat (file, &stbuf) == 0) {
	mib_mtime = stbuf.st_mtime;
    }

    if (stat (frozen, &stbuf) == 0) {
	frozen_mtime = stbuf.st_mtime;
    }

    if (mib_mtime == 0 || frozen_mtime == 0 || frozen_mtime < mib_mtime) {
	/* save pointer to still known tt's: */
	mib_TCSaveMark = mib_TCList;
	nodePtr = ParseFile (fp);
	fclose (fp);
	if (nodePtr != NULL || mib_TCList != mib_TCSaveMark) {
	    fp = fopen (frozen, "w");
	    if (fp != NULL) {
		MIB_WriteFrozenFile (fp, nodePtr);
		fclose (fp);
	    }
	}
    } else {
	fclose (fp);
	nodePtr = NULL;
	fp = fopen (frozen, "r");
	if (fp) {
	    nodePtr = MIB_ReadFrozenFile (fp);
	    fclose (fp);
	}
    }

    MIB_AddToTree (&root, nodePtr);
    
    return root;
}


/*
 * ScanIntEnums() converts a string containing pairs of labels and integer
 * values into a chained list of MIB_IntEnum's.
 * NOTE: this puts \0 into the passed string.
 */

static MIB_IntEnum *
ScanIntEnums (str)
     char *str;
{
    int done = 0;
    MIB_IntEnum *enumList = NULL, **enumPtr = &enumList;

    if (! str) return NULL;

    if (strncmp (str, "D ", 2) != 0) return NULL;

    str += 2;

    while (*str) {

	char *val, *desc = str;

	while (*str && isspace (*str))
	  str++;
	while (*str && ! isspace (*str))
	  str++;
	if (! *str) break;
	*str++ = 0;
	val = str;
	while (*str && ! isspace (*str))
	  str++;
	if (! *str)
	  done = 1;
	else
	  *str++ = 0;

	*enumPtr = (MIB_IntEnum *) ckalloc (sizeof (MIB_IntEnum));
	(*enumPtr)->value = val;
	(*enumPtr)->name = desc;
 	(*enumPtr)->nextPtr = 0;
	enumPtr = &(*enumPtr)->nextPtr;

	if (done) break;
    }

    return enumList;
}

static MIB_TextConv *
CreateTC (name, syntax, displayHint, enums)
     char *name;
     int syntax;
     char *displayHint;
     char *enums;
{
    MIB_TextConv *tcPtr = MIB_LookupTC (name);

    if (tcPtr) {
	return tcPtr;
    }

    tcPtr = (MIB_TextConv *) ckalloc (sizeof (MIB_TextConv));
    memset ((char *) tcPtr, '\0', sizeof (MIB_TextConv));
    
    if (name) {
	tcPtr->name = ckstrdup (name);
    }
    tcPtr->syntax = syntax;
    if (displayHint) {
	tcPtr->displayHint = ckstrdup (displayHint);
    }
    if (enums) {
	tcPtr->enumList = ScanIntEnums (ckstrdup (enums));
    }
    
    return MIB_AddTC (tcPtr);
}


/*
 * ReadIntEnums() scans a enum description, like 
 * ``INTEGER { foo(1), bar(2) }'', with the next token to be read 
 * the one after the ``{''; upon return the closing ``}'' is read.
 * the scanned enums are return in the passed dstring, in the form:
 * ``D foo 3 bar 4''
 */

static int
ReadIntEnums (strPtr)
     char **strPtr;
{
    Tcl_DString result;
    int syntax;
    int fail = 0;		/* parser thinks anything as alright */

    Tcl_DStringInit (&result);
    
    /* mark we append Descriptions: */
    Tcl_DStringAppend (&result, "D", 1);
    
    /* got LEFTBRACKET:  ``{'' */
    do {
	char str [SYMBOL_MAXLEN];
	char num [SYMBOL_MAXLEN];
	char keyword [SYMBOL_MAXLEN];

	syntax = ReadKeyword (fp, str);
#if 0
/** XXX: dont check - all we need is a string */
	if (syntax != LABEL) { fail = 1; break; }
#endif
	/* got the string:  ``{ foo'' */
	syntax = ReadKeyword (fp, keyword);
	if (syntax != LEFTPAREN) { fail = 1; break; }
	/* ``{ foo ('' */
	syntax = ReadKeyword (fp, num);
	if (syntax != NUMBER) { fail = 1; break; }
	/* append to the collecting string: */
	Tcl_DStringAppend (&result, " ", 1);
	Tcl_DStringAppend (&result, str, -1);
	Tcl_DStringAppend (&result, " ", 1);
	Tcl_DStringAppend (&result, num, -1);
	/* ``{ foo (99'' */
        syntax = ReadKeyword (fp, keyword);
	if (syntax != RIGHTPAREN) { fail = 1; break; }
	/* ``{ foo (99)'' */
	/* now there must follow either a ``,'' or a ``}'' */
        syntax = ReadKeyword (fp, keyword);

    } while (syntax == COMMA);
    
    /* the list is scanned, now there must be a ``}'' */
    
    if (fail || syntax != RIGHTBRACKET) {
	fprintf (stderr,
		 "%s: %d: Warning: cannot scan enums - ignored\n",
		 mib_FileName, line);
    }

    *strPtr = ckstrdup (Tcl_DStringValue(&result));
    Tcl_DStringFree (&result);

    return syntax;
}


/*
 * ParseFile() reads a MIB file specified by *fp and return a
 * linked list of objects in that MIB.
 */

static MIB_Node*
ParseFile (fp)
     FILE *fp;
{
    char name[SYMBOL_MAXLEN];
    char keyword[SYMBOL_MAXLEN];

    int	state  = OUT_OF_MIB;
    int	syntax = 0;

    MIB_Node *nodePtr = NULL;
    MIB_Node *nodeList = NULL;
    MIB_Node *lastOTPtr = NULL;

    /* 
     * this is really a klude: try to fetch type-definitions
     * like: ``Boolean ::= INTEGER { true(1), false(2) }''
     * and: ``KBytes ::= INTEGER''.
     */

    char tt_name[SYMBOL_MAXLEN];
    MIB_TextConv *tcPtr = 0;
    /*
     * init hashtable and parse mib 
     */

    HashKeywords();
    line = 0;

    while ((syntax = ReadKeyword (fp, keyword)) != EOF) {
	
	if (state == OUT_OF_MIB) {

	    /*
	     * we are at the beginning of a mib
	     */
	
	    switch (syntax) {
	      case DEFINITIONS:
		state = IN_MIB;
		if ((syntax = ParseHeader (fp, name)) == EOF) {
		    fprintf (stderr,
			     "%s:%d: bad format in MIB header\n",
			     mib_FileName, line);
		    return NULL;
		}

                /* maybe a type: */
                strncpy (tt_name, name, SYMBOL_MAXLEN);

		break;
	      case END:
		fprintf (stderr, "%s: end before start of MIB.\n", 
			 mib_FileName);
		return NULL;
	      case ERROR:
		fprintf (stderr, "%s:%d: error in MIB\n", mib_FileName, line);
		return NULL;
	      case LABEL:
		strncpy (name, keyword, SYMBOL_MAXLEN);
		break;
	      case LEFTBRACKET:
		{ int cnt = 1;
		  for (;;) {
		      char buf [SYMBOL_MAXLEN];
		      int syntax;
		      if ((syntax = ReadKeyword (fp, buf)) == LEFTBRACKET)
			cnt ++;
		      else if (syntax == RIGHTBRACKET)
			cnt--;
		      if (! cnt)
			break;
		  }
	        }
		break;
	      default:
		fprintf (stderr, "%s:%d: %s is a reserved word\n", 
			 mib_FileName, line, keyword);
		return NULL;
	    }

	} else {

	    /*
	     * we are in a mib
	     */
	
	    switch (syntax) {
	      case DEFINITIONS:
		fprintf (stderr, "%s: Fatal: nested MIBS\n", mib_FileName);
		return NULL;
		break;
	      case END:
		state = OUT_OF_MIB;
		break;
	      case EQUALS:
		syntax = ParseASN1Type (fp, name);
		if (syntax == END) 
                  state = OUT_OF_MIB;
	
		if (syntax == ASN1_INTEGER
		    || syntax == ASN1_OCTET_STRING 
		    || syntax == ASN1_OBJECT_IDENTIFIER) 
		  {
		    /* sure a type; if one with the same name already
		     * exists (eg. DisplayString in rfc1443.tc
		     * (wanted) and then in rfc1213.mib (unwanted))
		     * ignore and use the existing one (but this may
		     * hurt -- you have been warned) */
		      
		      tcPtr = CreateTC (tt_name, syntax, 0, 0);
		  } else if (syntax == ASN1_SEQUENCE 
			     && lastOTPtr && lastOTPtr->syntax == LABEL) {
		      lastOTPtr->syntax = syntax;
		  }
		break;
	      case ERROR:
		fprintf (stderr, "%s:%d: error in MIB\n", mib_FileName, line);
		return NULL;
	      case LABEL:
		strncpy (name, keyword, SYMBOL_MAXLEN);
		/* maybe a type: */
		strncpy (tt_name, keyword, SYMBOL_MAXLEN);
		break;
	      case MODULECOMP:
		if ((nodePtr = ParseModuleCompliance (fp, name, &nodeList)) == NULL) {
		    fprintf (stderr,
			     "%s:%d: bad format in MODULE-COMPLIANCE\n",
			     mib_FileName, line);
		    return NULL;
		}
		nodePtr->nextPtr = nodeList;
		nodeList = nodePtr;
		break;
	      case MODULEIDENTITY:
		if ((nodePtr = ParseModuleIdentity (fp, name, &nodeList)) == NULL) {
		    fprintf (stderr,
			     "%s:%d: bad format in MODULE-IDENTIY\n",
			     mib_FileName, line);
		    return NULL;
		}
		nodePtr->nextPtr = nodeList;
		nodeList = nodePtr;
		break;
	      case NOTIFYTYPE:
		if ((nodePtr = ParseNotificationType (fp, name, &nodeList)) == NULL) {
		    fprintf (stderr,
			     "%s:%d: bad format in NOTIFICATION-TYPE\n",
			     mib_FileName, line);
		    return NULL;
		}
		nodePtr->nextPtr = nodeList;
		nodeList = nodePtr;
		break;
	      case CAPABILITIES:
		if (ParseCapabilitiesType (fp) == 0) {
		    fprintf (stderr, 
			     "%s:%d: bad format in AGENT-CAPABILITIES\n",
			     mib_FileName, line);
                    return NULL;
		}
		break;
	      case TRAPTYPE:
		if (ParseTrapType (fp, name) < 0) {
		    fprintf (stderr,
			     "%s:%d: bad format in TRAP-TYPE\n",
			     mib_FileName, line);
		    return NULL;
		}
		break;
	      case OBJGROUP:
		if ((nodePtr = ParseObjectGroup (fp, name, &nodeList)) == NULL) {
		    fprintf (stderr,
			     "%s:%d: bad format in OBJECT-GROUP\n",
			     mib_FileName, line);
		    return NULL;
		}
		nodePtr->nextPtr = nodeList;
		nodeList = nodePtr;
		break;
	      case OBJECTIDENTITY:
		if ((nodePtr = ParseObjectIdentity (fp, name, &nodeList)) == NULL) {
		    fprintf (stderr,
			     "%s:%d: bad format in OBJECT-IDENTITY\n",
			     mib_FileName, line);
		    return NULL;
		}
		nodePtr->nextPtr = nodeList;
		nodeList = nodePtr;
		break;
	      case ASN1_OBJECT_IDENTIFIER:
		if ((nodePtr = ParseObjectID (fp, name, &nodeList)) == NULL) {
		    fprintf (stderr,
			     "%s:%d: bad format in OBJECT-IDENTIFIER\n",
			     mib_FileName, line);
		    return NULL;
		}
		nodePtr->nextPtr = nodeList;
		nodeList = nodePtr;
		break;
	      case OBJTYPE:
               if ((nodePtr = ParseObjectType (fp, name, &nodeList)) == NULL) {
		   fprintf(stderr,
			   "%s:%d: bad format in OBJECT-TYPE\n",
			   mib_FileName, line);
                  return NULL;
               }
		nodePtr->nextPtr = nodeList;
		nodeList = nodePtr;
		/* save for SEQUENCE hack: */
		lastOTPtr = nodePtr;
		break;
		
		/* 
		 * expect a leftparen; eg. for ``MacAddress ::= OCTET
		 * STRING (SIZE (6))''; action is to skip over. 
		 */
	      case LEFTPAREN:
		{ int cnt = 1;
		  for (;;) {
		      char buf [SYMBOL_MAXLEN];
		      int syntax;
		      if ((syntax = ReadKeyword (fp, buf)) == LEFTPAREN)
			cnt ++;
		      else if (syntax == RIGHTPAREN)
			cnt--;
		      if (! cnt)
			break;
		  }
	        }
		break;

	      case LEFTBRACKET:
		{ 
		    char *enums;
		    if (ReadIntEnums (&enums) != RIGHTBRACKET) {
			fprintf (stderr, "%s:%d: bad mib format\n",
				 mib_FileName, line);
			ckfree (enums);
		    } else if (tcPtr) {
			tcPtr->enumList = ScanIntEnums (enums);
		    }
	        }
		break;

	      default:
		fprintf (stderr, "%s:%d: bad mib format\n", 
			 mib_FileName, line);
		return NULL;
		break;
	    }
	}
    }
    
    /*
     * check if we finished correctly
     */

    if (state == OUT_OF_MIB) {
	return nodeList;
    }

    fprintf (stderr, "%s: Fatal: incomplete MIB\n", mib_FileName);
    return NULL;
}


/*
 * ParseHeader() parses a MIB header and places the LABEL that
 * follows in the string pointed to by keyword. Returns EOF if no end
 * of MIB header found, ERROR if no LABEL found and LABEL if MIB
 * header is passed and a label placed into keyword.
 */

static int
ParseHeader (fp, keyword)
     FILE *fp;
     char *keyword;
{
    int syntax;
   
    if ((syntax = ReadKeyword (fp, keyword)) != EQUALS) {
	return ERROR;
    }

    if ((syntax = ReadKeyword (fp, keyword)) != BEGIN) {
	return ERROR;
    }

    syntax = ReadKeyword (fp, keyword);

    /*
     * if it's EXPORTS clause, read the next keyword after SEMICOLON
     */

    if (syntax == EXPORTS) {
	while ((syntax = ReadKeyword (fp, keyword)) != SEMICOLON)
	  if (syntax == EOF) return EOF;
	syntax = ReadKeyword (fp, keyword);
    }

    /*
     * if it's IMPORTS clause, read the next keyword after SEMICOLON
     */

    if (syntax == IMPORTS) {
	while ((syntax = ReadKeyword (fp, keyword)) != SEMICOLON) {
	    if (syntax == EOF) return EOF;
	}
	syntax = ReadKeyword (fp, keyword);
    }

    /*
     * return syntax (should be label or defined label (e.g. DisplayString))
     */

    return syntax;
}


/*
 * Parses an ASN1 syntax and places the LABEL that follows in the
 * string pointed to by keyword. Returns 0 on error, LABEL on success.  
 */

static int
ParseASN1Type (fp, keyword)
     FILE *fp;
     char *keyword;
{
    int level = 0, syntax = 0, merk, osyntax;
    char name [SYMBOL_MAXLEN];
    char otype [SYMBOL_MAXLEN];
    char convention [SYMBOL_MAXLEN];
    char *displayHint = NULL;
    char *enums = NULL;
    
    /* save passed name: */
    strcpy (name, keyword);
    
    syntax = ReadKeyword (fp, keyword);
    switch (syntax) {
      case ASN1_INTEGER:
	break;
      case ASN1_OCTET_STRING:
	break;
      case ASN1_OBJECT_IDENTIFIER:
	break;
      case ASN1_SEQUENCE:
	while ((syntax = ReadKeyword (fp, keyword)) != RIGHTBRACKET)
	  if (syntax == EOF) return 0;
	syntax = ASN1_SEQUENCE;
	break;
      case TEXTUALCONV:

	 /* default: no convention/enums seen: */
	 convention [0] = 0;

         while ((syntax = ReadKeyword (fp, keyword)) != SYNTAX
		&& syntax != DISPLAYHINT)
	   if (syntax == EOF)
	     return 0;

         /*
	  * read the keyword following SYNTAX or DISPLAYHINT
	  */

	 merk = ReadKeyword (fp, keyword);
	/* ugh. and yet another ugly hack to this ugly parser... */
	if (syntax == SYNTAX && merk == LABEL)
	  {
	      MIB_TextConv *newTcPtr, *tcPtr = MIB_LookupTC (keyword);
	      if (tcPtr) {
		  newTcPtr = CreateTC (name, tcPtr->syntax, 0, 0);
		  newTcPtr->displayHint = tcPtr->displayHint;
		  newTcPtr->enumList = tcPtr->enumList;
		  return tcPtr->syntax;
	      }
	      /* alas, we are (hopefully) done. */
	      return 0;
	  }
	
	 if (syntax == DISPLAYHINT)
	   {
	       /* save convention */
	       strcpy (convention, keyword);

	       /* skip to SYNTAX: */
	       while ((syntax = ReadKeyword (fp, keyword)) != SYNTAX)
		 if (syntax == EOF)
		   return 0;
	       
	       if ((merk = ReadKeyword (fp, keyword)) == LABEL)
		 return 0;
	   }
	
	/* XXX */
	/* save object type: */
	strcpy (otype, keyword);
 	osyntax = merk;
	
	/*
	 * if next keyword is a bracket, we have to continue
	 */
	
	if ((syntax = ReadKeyword (fp, keyword)) == LEFTPAREN) {
	    level = 1;
	    while (level != 0) {
		if ((syntax = ReadKeyword (fp, keyword)) == EOF)
		  return 0;
		if (syntax == LEFTPAREN)
		  ++level;
		if (syntax == RIGHTPAREN)
		  --level;
	    }
	    syntax = ReadKeyword (fp, keyword);
	}
	
	if (syntax == LEFTBRACKET) {
	    syntax = ReadIntEnums (&enums);
	}
	
	 /* found MIB_TextConv: */
	
	if (convention && *convention) {
	    displayHint = convention;
	}
	if (enums && *enums == '\0') {
	    ckfree (enums);
	    enums = NULL;
	}

	CreateTC (name, osyntax, displayHint, enums);

	if (enums) {
	    ckfree (enums);
	    enums = NULL;
	}
	
	break;
      default:
	{ MIB_TextConv *tcPtr = MIB_LookupTC (keyword);
	  if (tcPtr) {
	      MIB_TextConv *newTcPtr;
	      newTcPtr = CreateTC (name, tcPtr->syntax, 0, 0);
	      newTcPtr->displayHint = tcPtr->displayHint;
	      newTcPtr->enumList = tcPtr->enumList;
	      return tcPtr->syntax;
	  } else {
	      fprintf (stderr, "%s:%d: Warning: unknown syntax \"%s\"\n",
		       mib_FileName, line, keyword);
	      return 0;
	  }
        }
    }
    
    return syntax;
}


/*
 * ParseModuleCompliance() parses a MODULE-COMPLIANCE macro.
 * Returns NULL if an error was detected.
 */

static MIB_Node*
ParseModuleCompliance (fp, name, nodeList)
     FILE *fp;
     char *name;
     MIB_Node **nodeList;
{
    char keyword[SYMBOL_MAXLEN];
    int syntax;
    MIB_Node *nodePtr;
    struct subid *subidList, *freePtr;
    
    nodePtr = MIB_MallocNode (name);
    
    /*
     * read keywords until syntax EQUALS is found
     */
    
    while ((syntax = ReadKeyword (fp, keyword)) != EQUALS) {
	switch (syntax) {
	  case DESCRIPTION:
	    nodePtr->fileOffset = ftell (fp);
	    if ((syntax = ReadKeyword (fp, keyword)) != QUOTESTRING) {
		fprintf (stderr, "%d --> %s\n", syntax, keyword);
		return NULL;
	    }
	    break;
	  case EOF:
	    return NULL;
	  default:
	    break;
	}
    }
    
    /*
     * parse list of the form { parent [parent list [...]] nr }
     */
    
    if ((subidList = ReadSubID (fp)) == NULL) {
	return NULL;
    }
    
    /*
     * create new nodes, if neccessary and free subidList
     */
    
    while (subidList != NULL && subidList->subid != -1) {
	
	/*
	 * check if it's node for this OBJECT-IDENTIFIER
	 */
	
	if (subidList->label == NULL) {
	    nodePtr->parentName = ckstrdup (subidList->parentName);
	    nodePtr->subid = subidList->subid;
	    
	} else {
	    
	    /*
	     * a new node must be created and linked into the list
	     */

	    AddNewNode (nodeList, subidList->label, 
			subidList->parentName, subidList->subid);
	}
	
	/*
	 * free this entry
	 */
       
	freePtr  = subidList;
	subidList = subidList->next;
	ckfree ((char *) freePtr);
   }
    
    return nodePtr;
}


/*
 * ParseModuleIdentity() parses a MODULE-IDENTITY macro.
 */

static MIB_Node*
ParseModuleIdentity (fp, name, nodeList)
     FILE *fp;
     char *name;
     MIB_Node **nodeList;
{
    char keyword[SYMBOL_MAXLEN];
    int	syntax;
    MIB_Node *nodePtr;
    struct subid *subidList, *freePtr;
    
    nodePtr = MIB_MallocNode (name);

    /*
     * read keywords until syntax EQUALS is found
     */

    while ((syntax = ReadKeyword (fp, keyword)) != EQUALS) {
	switch (syntax) {
	  case DESCRIPTION:
            nodePtr->fileOffset = ftell (fp);
            if ((syntax = ReadKeyword (fp, keyword)) != QUOTESTRING) {
		fprintf (stderr, "%d --> %s\n", syntax, keyword);
		return NULL;
            }
            break;
	  case EOF:
            return NULL;
	  default:
            break;
	}
    }

    /*
     * parse list of the form { parent [parent list [...]] nr }
     */

    if ((subidList = ReadSubID (fp)) == NULL) {
	return NULL;
    }
    
    /*
     * create new nodes, if neccessary and free subidList
     */

    while (subidList != NULL && subidList->subid != -1) {

	/*
         * check if it's node for this OBJECT-IDENTIFIER
	 */

	if (subidList->label == NULL) {
	    nodePtr->parentName = ckstrdup (subidList->parentName);
	    nodePtr->subid = subidList->subid;

	} else {

	    /*
	     * a new node must be created and linked into the list
	     */

	    AddNewNode (nodeList, subidList->label, 
			subidList->parentName, subidList->subid);
	}
	
	/*
	 * free this entry
	 */
	
	freePtr = subidList;
	subidList = subidList->next;
	ckfree ((char *) freePtr);
    }

    return nodePtr;
}


/*
 * ParseNotificationType() parses a NOTIFICATION-TYPE macro.
 */

static MIB_Node*
ParseNotificationType (fp, name, nodeList)
     FILE *fp;
     char *name;
     MIB_Node **nodeList;
{
    char keyword[SYMBOL_MAXLEN];
    int	syntax;
    MIB_Node *nodePtr;
    struct subid *subidList, *freePtr;

    nodePtr = MIB_MallocNode (name);

    /*
     * read keywords until syntax EQUALS is found
     */

    while ((syntax = ReadKeyword (fp, keyword)) != EQUALS) {
	switch (syntax) {
	  case DESCRIPTION:
            nodePtr->fileOffset = ftell (fp);
            if ((syntax = ReadKeyword (fp, keyword)) != QUOTESTRING) {
		fprintf (stderr, "%d --> %s\n", syntax, keyword);
		return NULL;
            }
            break;
	  case EOF:
            return NULL;
	  default:
            break;
	}
    }
    
    /*
     * parse list of the form { parent [parent list [...]] nr }
     */
    
    if ((subidList = ReadSubID (fp)) == NULL) {
	return NULL;
    }

    /*
     * create new nodes, if neccessary and free subidList
     */

    while (subidList != NULL && subidList->subid != -1) {

	/*
         * check if it's node for this OBJECT-IDENTIFIER
	 */

	if (subidList->label == NULL) {
	    nodePtr->parentName = ckstrdup (subidList->parentName);
	    nodePtr->subid = subidList->subid;

	} else {

	    /*
	     * a new node must be created and linked into the list
	     */

	    AddNewNode (nodeList, subidList->label, 
			subidList->parentName, subidList->subid);
	}

	/*
         * free this entry
	 */
	
	freePtr = subidList;
	subidList = subidList->next;
	ckfree ((char *) freePtr);
    }
    
    return nodePtr;
}


/*
 * ParseCapabilitiesType() parses or better ignores agent 
 * capabilities macros.
 */

static int
ParseCapabilitiesType (fp)
     FILE *fp;
{
    char keyword[SYMBOL_MAXLEN];
    int	syntax;

    while ((syntax = ReadKeyword (fp, keyword)) != EQUALS) {
	switch (syntax) {
	  case EOF:
            return 0;
	  default:
            break;
	}
    }

    while ((syntax = ReadKeyword (fp, keyword)) != RIGHTBRACKET) {
	switch (syntax) {
          case EOF:
            return 0;
          default:
            break;
        }
    }

    return 1;
}


/*
 * ParseTrapType() parses a TRAP-TYPE macro.
 */

static int
ParseTrapType (fp, name)
     FILE *fp;
     char *name;
{
    char keyword[SYMBOL_MAXLEN];
    int  syntax;

    /*
     * read keywords until syntax EQUALS is found
     */

    while ((syntax = ReadKeyword (fp, keyword)) != EQUALS) {
	if (syntax == EOF) return -1;
    }

    /*
     * parse a number defining the trap number
     */

    syntax = ReadKeyword (fp, keyword);
    if (syntax != NUMBER) 
      return -1;

    /*
     * TRAP-TYPE macro is complete
     */
    
    return (atoi (keyword));
}


/*
 * ParseObjectGroup() parses an OBJECT-GROUP macro.
 */

static MIB_Node*
ParseObjectGroup (fp, name, nodeList)
     FILE *fp;
     char *name;
     MIB_Node **nodeList;
{
    char keyword[SYMBOL_MAXLEN];
    int	syntax;
    MIB_Node *nodePtr;
    struct subid *subidList, *freePtr;

    /*
     * next keyword must be OBJECTS
     */
    
    if ((syntax = ReadKeyword (fp, keyword)) != OBJECTS)
      return NULL;
    
    nodePtr = MIB_MallocNode (name);
    
    /*
     * read keywords until EQUALS are found
     */
    
    while ((syntax = ReadKeyword (fp, keyword)) != EQUALS) {
	switch (syntax) {
	  case STATUS:
	    syntax = ReadKeyword (fp, keyword);
	    if (syntax != CURRENT && syntax != OBSOLETE) {
		fprintf (stderr, "%d --> %s\n", syntax, keyword);
		return NULL;
	    }
	    break;
	  case DESCRIPTION:
	    nodePtr->fileOffset = ftell (fp);
	    if ((syntax = ReadKeyword (fp, keyword)) != QUOTESTRING) {
		fprintf (stderr, "%d --> %s\n", syntax, keyword);
		return NULL;
	    }
	    break;
	  case EOF:
	    return NULL;
	  default:
	    break;
	}
    }
    
    /*
     * parse list of the form { parent [parent list [...]] nr }
     */

    if ((subidList = ReadSubID (fp)) == NULL) {
	return NULL;
    }
    
    /*
     * create new nodes, if neccessary and free subidList
     */

    while (subidList != NULL && subidList->subid != -1) {

	/*
         * check if it's node for this OBJECT-IDENTIFIER
	 */

	if (subidList->label == NULL) {
	    nodePtr->parentName = ckstrdup (subidList->parentName);
	    nodePtr->subid = subidList->subid;

	} else {

	    /*
	     * a new node must be created and linked into the list
	     */

	    AddNewNode (nodeList, subidList->label, 
			subidList->parentName, subidList->subid);
	}
	
	/*
	 * free this entry
	 */

	freePtr = subidList;
	subidList = subidList->next;
	ckfree ((char *) freePtr);
    }
    
    return nodePtr;
}




/*
 * ParseObjectIdentity() parses an OBJECT-IDENTITY macro.
 */

static MIB_Node*
ParseObjectIdentity (fp, name, nodeList)
     FILE *fp;
     char *name;
     MIB_Node **nodeList;
{
    char keyword[SYMBOL_MAXLEN];
    int	syntax;
    MIB_Node *nodePtr;
    struct subid *subidList, *freePtr;
    
    nodePtr = MIB_MallocNode (name);

    /*
     * read keywords until EQUALS are found
     */

    while ((syntax = ReadKeyword (fp, keyword)) != EQUALS) {
	switch (syntax) {
	  case STATUS:
            syntax = ReadKeyword (fp, keyword);
            if (syntax != CURRENT && syntax != OBSOLETE) {
		fprintf (stderr, "%d --> %s\n", syntax, keyword);
		return NULL;
            }
            break;
          case DESCRIPTION:
            nodePtr->fileOffset = ftell (fp);
            if ((syntax = ReadKeyword (fp, keyword)) != QUOTESTRING) {
		fprintf (stderr, "%d --> %s\n", syntax, keyword);
		return NULL;
            }
            break;
	  case EOF:
            return NULL;
	  default:
            break;
	}
    }
    
    /*
     * parse list of the form { parent [parent list [...]] nr }
     */

    if ((subidList = ReadSubID (fp)) == NULL) {
	return NULL;
    }

    /*
     * create new nodes, if neccessary and free subidList
     */

    while (subidList != NULL && subidList->subid != -1) {

	/*
         * check if it's node for this OBJECT-IDENTIFIER
	 */

	if (subidList->label == NULL) {
	    nodePtr->parentName = ckstrdup (subidList->parentName);
	    nodePtr->subid = subidList->subid;
	    
	} else {
	
	    /*
	     * a new node must be created and linked into the list
	     */

	    AddNewNode (nodeList, subidList->label, 
			subidList->parentName, subidList->subid);
	}

	/*
	 * free this entry
	 */

	freePtr = subidList;
	subidList = subidList->next;
	ckfree ((char *) freePtr);
    }

    return nodePtr;
}




/*
 * ParseObjectID() parses an OBJECT-IDENTIFIER entry.
 */

static MIB_Node*
ParseObjectID (fp, name, nodeList)
     FILE *fp;
     char *name;
     MIB_Node **nodeList;
{
    char keyword[SYMBOL_MAXLEN];
    int	syntax;
    MIB_Node *nodePtr;
    struct subid *subidList, *freePtr;

    /*
     * next keyword must be EQUALS
     */

    if ((syntax = ReadKeyword (fp, keyword)) != EQUALS)
      return NULL;
    
    nodePtr = MIB_MallocNode (name);
    nodePtr->syntax = ASN1_OBJECT_IDENTIFIER;
    
    /*
     * parse list of the form { parent [parent list [...]] nr }
     */

    if ((subidList = ReadSubID (fp)) == NULL) {
	return NULL;
    }
    
    /*
     * create new nodes, if neccessary
     */

    while (subidList != NULL && subidList->subid != -1) {

	/*
         * check if it's node for this OBJECT-IDENTIFIER
	 */

	if (subidList->label == NULL) {
	    nodePtr->parentName = ckstrdup (subidList->parentName);
	    nodePtr->subid  = subidList->subid;
	    
	} else {
	
	    /*
	     * a new node must be created and linked into the list
	     */

	    AddNewNode (nodeList, subidList->label, 
			subidList->parentName, subidList->subid);
	}
	
	/*
	 * free this entry
	 */

	freePtr = subidList;
	subidList = subidList->next;
	ckfree ((char *) freePtr);
    }
    
    return (nodePtr);
}



/*
 * ParseObjectType() parses an OBJECT-TYPE macro.
 */

static MIB_Node*
ParseObjectType (fp, name, nodeList)
     FILE *fp;
     char *name;
     MIB_Node **nodeList;
{
    char keyword[SYMBOL_MAXLEN];
    int	syntax;
    MIB_Node *nodePtr;
    struct subid *subidList, *freePtr;
    Tcl_DString dst;

    /*
     * next keyword must be SYNTAX
     */

    if ((syntax = ReadKeyword (fp, keyword)) != SYNTAX)
      return NULL;
    
    nodePtr = MIB_MallocNode (name);
    nodePtr->fileOffset = -1;

    /*
     * next keyword defines OBECT-TYPE syntax
     */

    if ((syntax = ReadKeyword (fp, keyword)) == ACCESS)
      return NULL;
    
    nodePtr->syntax = syntax;
    
    /*
     * no read keywords until ACCESS is found
     */

    /*
     * if the syntax found is LABEL, we should lookup the typedef:
     */
    
    if (syntax == LABEL) {

        nodePtr->tc = MIB_LookupTC (keyword);
	if (nodePtr->tc) {
	    nodePtr->syntax = nodePtr->tc->syntax;
	}
#if 0
	else {
	    fprintf (stderr, "%s:%d: Warning: unknown syntax \"%s\"\n",
		     mib_FileName, line, keyword);
	}
#endif

	/* 
	 * old eat-it-up code: skip anything to the ACCESS keyword: 
	 */
	
	while ((syntax = ReadKeyword (fp, keyword)) != ACCESS)
	  if (syntax == EOF)
	    return NULL;

    } else if (syntax == ASN1_INTEGER) {
    
	/*
	 * if the syntax found is INTEGER, there may follow the enums for a
	 * textual description of the integer value; extract - don't skip.
	 */

	char *enums = NULL;
	
	/* 
	 * We may get something like ``{ foo ( 99), ... }'' or
	 * ``(0..99)'' or nothing. The second case is ignored.
	 */ 

	syntax = ReadKeyword (fp, keyword);
	if (syntax == LEFTBRACKET) {
	    syntax = ReadIntEnums (&enums);
	    
	    /*
	     * old eat-it-up code: skip anything to the ACCESS keyword:
	     */
	    
	    while ((syntax = ReadKeyword (fp, keyword)) != ACCESS) {
		if (syntax == EOF) return NULL;
	    }

	} else if (syntax == LEFTPAREN) {

	    /* got LEFTPAREN: ``('' */
	    /* XXX: fetch here ranges... -- we simply skip */
	    int level = 1;
	    
	    while ((syntax = ReadKeyword (fp, keyword)) != RIGHTPAREN
		   && level > 0) 
	      {
		  if (syntax == EOF)
		    return NULL;
		  else if (syntax == RIGHTPAREN)
		    level--;
	      }
	}
	
	/* 
	 * We simply skip up to the ACCESS key, like the old version 
	 * did: wrong, but effective... 
	 */
	
	while (syntax != ACCESS) {
	    syntax = ReadKeyword (fp, keyword);
	    if (syntax == EOF) {
		return NULL;
	    }
	}

	if (enums && *enums) {

	    /* we fake a name, to allow saving to the frozen-index: */

	    char *name = ckalloc (strlen (nodePtr->label) + 10);
	    sprintf (name, "_%s", nodePtr->label);
	    nodePtr->tc = CreateTC (name, ASN1_INTEGER, 0, enums);
	    ckfree (name);
	    ckfree (enums);
	    enums = NULL;
	}

    } else if (syntax == ASN1_SEQUENCE) {
	if ((syntax = ReadKeyword (fp, keyword)) == ASN1_SEQUENCE_OF) {
	    nodePtr->syntax = syntax;
	}
	while (syntax != ACCESS) {
            syntax = ReadKeyword (fp, keyword);
            if (syntax == EOF) {
                return NULL;
            }
        }
	/* we are fine. now continue below with the ACCESS mode: */ 
    } else{
	
	/* 
	* old eat-it-up code: skip anything to the ACCESS keyword: 
	*/
	
	while ((syntax = ReadKeyword (fp, keyword)) != ACCESS)
	  if (syntax == EOF)
	    return NULL;
    }
    
    /*
     * next keyword defines ACCESS mode for object
     */

    syntax = ReadKeyword (fp, keyword);
    if (syntax < READONLY || syntax > NOACCESS) {
	fprintf (stderr, "%s:%d: scan error near `%s'\n", 
		 mib_FileName, line, keyword);
	return NULL;
    }

    switch (syntax) {
      case READONLY:   nodePtr->access = M_READONLY; break;
      case READCREATE: nodePtr->access = M_READCREATE; break;
      case READWRITE:  nodePtr->access = M_READWRITE; break;
      case WRITEONLY:  nodePtr->access = M_WRITEONLY; break;
      case FORNOTIFY:  nodePtr->access = M_FORNOTIFY; break;
      default:         nodePtr->access = M_NOACCESS;
    }
    
    /*
     * next keyword must be STATUS
     */

    if ((syntax = ReadKeyword (fp, keyword)) != STATUS)
      return NULL;
    
    /*
     * next keyword defines status of object
     */

    syntax = ReadKeyword (fp, keyword);
    if (syntax < MANDATORY || syntax > DEPRECATED) {
	fprintf (stderr, "%s:%d: scan error near `%s'\n", 
		 mib_FileName, line, keyword);
	return NULL;
    }

    /*
     * now determine optional parts of OBJECT-TYPE macro
     */

    while ((syntax = ReadKeyword (fp, keyword)) != EQUALS) {
	switch (syntax) {
	  case DESCRIPTION:
            nodePtr->fileOffset = ftell (fp);
            if ((syntax = ReadKeyword (fp, keyword)) != QUOTESTRING) {
		return NULL;
            }
            break;
	  case INDEX:
	  case AUGMENTS:
	    Tcl_DStringInit (&dst);
	    if ((syntax = ReadKeyword (fp, keyword)) != LEFTBRACKET) {
	        return NULL;
	    }
	    while ((syntax = ReadKeyword (fp, keyword)) != RIGHTBRACKET) {
		switch (syntax) {
		  case COMMA:
		  case IMPLIED:
		    continue;
		  case LABEL:
		    Tcl_DStringAppendElement (&dst, keyword);
		    break;
		  default:
		    Tcl_DStringFree (&dst);
		    return NULL;
		}
	    }
	    nodePtr->index = ckstrdup (Tcl_DStringValue (&dst));
	    Tcl_DStringFree (&dst);
	    break;
	  case DEFVAL:
	    if ((syntax = ReadKeyword (fp, keyword)) != LEFTBRACKET) {
                return NULL;
            }
            while ((syntax = ReadKeyword (fp, keyword)) != RIGHTBRACKET) {
		if (syntax == EOF) {
		    return NULL;
		}
		nodePtr->index = keyword;
	    }
	    break;
	  case EOF:
            return NULL;
	  default:
            break;
	}
    }
    
    /*
     * parse list of the form { parent [parent list [...]] nr }
     */
    
    if ((subidList = ReadSubID (fp)) == NULL) {
	return NULL;
    }

    /*
     * create new nodes, if neccessary
     */

    while (subidList != NULL && subidList->subid != -1) {
    
	/*
	 * check if it's node for this OBJECT-IDENTIFIER
	 */

	if (subidList->label == NULL) {
	    nodePtr->parentName = ckstrdup (subidList->parentName);
	    nodePtr->subid = subidList->subid;

	} else {

	    /*
	     * a new node must be created and linked into the list
	     */

	    AddNewNode (nodeList, subidList->label, 
			subidList->parentName, subidList->subid);
	}
	
	/*
         * free this entry
	 */

	freePtr  = subidList;
	subidList = subidList->next;
	ckfree ((char *) freePtr);
    }
    
    return nodePtr;
}


/*
 * HashKeywords() builds up a hash table, including the defined keywords
 * as records. This enables the other functions to directly reference
 * records by doing arithmetic transformations on the keywords name.
 */

static void
HashKeywords()
{
    char *cp = NULL;
    int	hash_val = 0;
    int	hash_index = 0;
    Keyword *tp = NULL;

    memset ((char *) hashtab, '\0', sizeof (hashtab));
    
    for (tp = keywords; tp->name; tp++) {
	hash_val = 0;
	for (cp = tp->name; *cp; cp++) 
	  hash_val += *cp;
	hash_index = HASH (hash_val);
	tp->hash   = hash_val;

	if (hashtab[hash_index] != NULL) {
	    tp->nextPtr = hashtab[hash_index];
	}
	hashtab[hash_index] = tp;
    }
}

/*
 * ReadKeyword() parses a keyword from the MIB file and places it in
 * the string pointed to by keyword. Returns the syntax of keyword or
 * EOF if any error.
 */

static int
ReadKeyword (fp, keyword)
     FILE *fp;
     char *keyword;

{
    char *cp = keyword;
    int	ch = lastchar;
    int	hash_val = 0;

    Keyword *tp;
    
    *keyword = '\0';

    /*
     * skip spaces
     */

    while (isspace (ch) && ch != EOF) {
	if (ch == '\n') line++;
	ch = getc (fp);
    }

    if (ch == EOF) return EOF;

    /*
     * skip textual descriptions enclosed in " characters
     */

    if (ch == '"') {
	int len = 0;
	*keyword = '\0';
	while ((ch = getc (fp)) != EOF) {
	    if (ch == '\n') {
		line++;
	    } else if (ch == '"') {
		lastchar = ' ';
		return QUOTESTRING;
	    } else if (len < SYMBOL_MAXLEN - 2) {
		keyword [len++] = ch, keyword [len] = 0;
	    }
	}
	return EOF;
    }

    /*
     * skip comments
     */

    if (ch == '-') {
	hash_val += ch;
	*cp++ = ch;
	
	if ((ch = getc (fp)) == '-') {
	    *keyword = '\0';
	    while ((ch = getc (fp)) != EOF) {
		if (ch == '\n') {
		    line++;
		    break;
		}
	    }
	    if (ch == EOF) return EOF;
	    
	    lastchar = ' ';
	    return ReadKeyword (fp, keyword);
	}
    }
   
    /*
     * Read characters until end of keyword is found
     */

    do {
	if (ch == '\n') line++;
	
	if (isspace (ch) || ch == '(' || ch == ')' || ch =='{' ||
	    ch == '}' || ch == ',' || ch == ';') {

	    /*
             * check for keyword of length 1
	     */

	    if (!isspace (ch) && *keyword == '\0') {
		hash_val += ch;
		*cp++ = ch;
		lastchar = ' ';
	    } else if (ch == '\n') {
		lastchar = ' ';
	    } else {
		lastchar = ch;
	    }
	       
	    *cp = '\0';

	    /*
	     * is this a defined keyword ?
	     */

	    for (tp = hashtab[HASH(hash_val)]; tp != NULL; tp = tp->nextPtr) {
		if ((tp->hash == hash_val) && !fstrcmp (tp->name, keyword))
		  break;
	    }
	    
	    if (tp != NULL) {

		/*
		 * if keyword is not complete, continue; otherwise return
		 */
		
		if (tp->key == CONTINUE)
		  continue;
		return tp->key;
	    }
	    
	    /*
	     * is it a LABEL ?
	     */
	    
	    for (cp = keyword; *cp; cp++) {
		if (! isdigit (*cp)) return LABEL;
	    }
	    
	    /*
	     * keywords consists of digits only
	     */
	    
	    return NUMBER;

	} else {
	
	    /*
	     * build keyword and hashvalue
	     */

	    hash_val += ch;
	    *cp++ = ch;
	}
    } while ((ch = getc (fp)) != EOF);
    
    return EOF;
}




/*
 * ReadSubID() parses a list of the form { iso org(3) dod(6) 1 }
 * and creates a parent-child entry for each node. Returns NULL
 * if we found an error.
 */

static struct subid*
ReadSubID (fp)
     FILE *fp;
{
   char	name[SYMBOL_MAXLEN]; 
   char	keyword[SYMBOL_MAXLEN]; 
   int is_child	= 0;
   int syntax = 0;
   struct subid	*np = NULL;
   struct subid *subidList = NULL;

   /*
    * EQUALS are passed, so first keyword must be LEFTBRACKET
    */

   if ((syntax = ReadKeyword (fp, keyword)) != LEFTBRACKET) return NULL;

   /*
    * now read keywords until RIGHTBRACKET is passed
    */

   while ((syntax = ReadKeyword (fp, keyword)) != RIGHTBRACKET) {
       switch (syntax) {
	 case EOF:
	   return NULL;
         case LABEL:

	 do_label:

	   /* allocate memory for new element */
	   
	   np = (struct subid *) ckalloc (sizeof(struct subid));
	   memset ((char *) np, '\0', sizeof (struct subid));
	   np->subid  = -1;
	   
	   /* if this label followed another one, it's the child */
	   
	   if (is_child) {
	       np->parentName = ckstrdup (name);
               np->label  = ckstrdup (keyword);
	   } else {
	       np->parentName = ckstrdup (keyword);
               is_child = 1;		/* next labels are children   */
	   }
	   
	   np->next = subidList;
	   subidList = np;
	   strcpy (name, keyword);
	   break;
         case LEFTPAREN:
	   if ((syntax = ReadKeyword (fp, keyword)) != NUMBER) return NULL;
	   np->subid = atoi (keyword);
	   if ((syntax = ReadKeyword (fp, keyword)) != RIGHTPAREN)
	     return NULL;
	   break;            
         case NUMBER:
	   if (! np)
	     {
		 /* something like:   { 0 1 } */
		 char *label = MIB_Name (keyword, 1);
		 if (! label)
		   return NULL;
		 strcpy (keyword, label);
		 goto do_label;
	     }

            if (np->subid != -1) {
		np = (struct subid *) ckalloc (sizeof (struct subid));
		memset ((char *) np, '\0', sizeof (struct subid));
		np->parentName = ckstrdup (name);
		np->subid = -1;
		np->next = subidList;
		subidList = np;
	    }
	   np->subid = atoi (keyword);
	   break;
         default:
	   return NULL;
       }
   }
   return subidList;
}
