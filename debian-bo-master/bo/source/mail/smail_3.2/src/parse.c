/*
#ident	"@(#)smail/src:RELEASE-3_2:parse.c,v 1.5 1996/02/28 06:47:11 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 by Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 *
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * parse.c:
 *	Parse configuration files in a standard way.
 *
 *	The directory, router and transport files all share a common
 *	format which are parsed using routines in this file.  Although
 *	the format is different, the rules for lexical tokens in the
 *	method and config files are the same, so routines for parsing
 *	these files is given as well.
 *
 *	external functions:  read_entry, parse_entry, parse_config.
 */
#ifdef STANDALONE
# define xmalloc malloc
# define xrealloc realloc
# define xfree free
#endif	/* STANDALONE */

#include <sys/types.h>
#include <stdio.h>
#include <ctype.h>
#include "defs.h"
#include "smail.h"
#include "parse.h"
#include "dys.h"
#include "exitcodes.h"
#ifndef DEPEND
# include "debug.h"
# include "extern.h"
#endif

/* variables exported from this file */
char *on = "1";				/* boolean on attribute value */
char *off = "0";			/* boolean off attribute value */

/* functions local to this file */
static char *c_quote();
static char *eof_error();
static char *finish_parse();
static char *skip_space();


/*
 * parse_entry - parse an entry from director, router or transport file
 *
 * given an entry from the given file, parse that entry, returning
 * returning the name of the entry and name/value pairs for all of the
 * attributes in the entry.  Names which fit the form of a boolean
 * attribute will return a pointer to one of the external variables
 * "on" or "off".
 *
 * Returns the name of the entry on a successful read.  Returns NULL
 * on end of file or error.  For an error, a message is returned as an
 * error string.  Calling xfree on the name is sufficient to free all
 * of the string storage.  To free the list entries, step through the
 * lists and free each in turn.
 */
char *
parse_entry(entry, generic, driver, error)
    register char *entry;		/* entry string to be parsed */
    struct attribute **generic;		/* all generic attributes */
    struct attribute **driver;		/* all driver attributes */
    char **error;			/* returned error message */
{
    struct str str;			/* string for processing entry */
    int this_attribute;			/* offset of attribute name */

    *error = NULL;			/* no error yet */
    /*
     * grab the entry name as a collection of characters followed
     * by optional white space followed by a `:'
     */
    STR_INIT(&str);
    while (*entry && !isspace(*entry) && *entry != ':' && *entry != '#') {
	STR_NEXT(&str, *entry++);
    }
    entry = skip_space(entry);

    if (*entry != ':') {
	*error = "field name does not end in `:'";
	return NULL;
    }
    STR_NEXT(&str, '\0');		/* done with the name */
    entry++;

    if (str.i == 0) {
	*error = "null field name";
	return NULL;
    }

    /*
     * loop grabbing attributes and values until the end of
     * the entry
     */
    while (*entry) {
	int i;				/* temp */

	/* attribute name begins at next non-white space character */
	entry = skip_space(entry);

	if (*entry == ';') {
	    /* `;' separates generic and driver attributes */
	    entry = skip_space(entry + 1);
	    if (*entry) {
		STR_NEXT(&str, ';');
	    }
	}

	/* be lenient about a `;' or `,' with no following attributes */
	if (*entry == '\0') {
	    break;
	}

	/* attribute name is of the form [+-]?[A-Za-z0-9_-]+ */
	this_attribute = str.i;
	if (*entry == '+' || *entry == '-') {
	    STR_NEXT(&str, *entry++);
	}
	i = str.i;
	while (*entry && (isalnum(*entry) || *entry == '_' || *entry == '-')) {
	    STR_NEXT(&str, *entry++);
	}
	if (i == str.i) {
	    *error = "null attribute name";
	    return NULL;
	}
	STR_NEXT(&str, '\0');		/* terminate the attribute name */
	entry = skip_space(entry);

	if (*entry == '\0' || *entry == ',' || *entry == ';') {
	    /* end of boolean attribute */
	    if (*entry == ',') {
		/* skip over commas */
		entry++;
	    }
	    continue;
	}
	if (*entry != '=') {
	    /* not boolean form and not "name = value" form */
	    *error = xmalloc(strlen(str.p + this_attribute) +
			     sizeof("no value for attribute "));
	    (void) sprintf(*error, "no value for attribute %s",
			   str.p + this_attribute);
	    return NULL;
	}

	/* note that this is a value */
	STR_NEXT(&str, '=');

	entry = skip_space(entry + 1);

	if (*entry == '"') {
	    entry++;
	    /*
	     * if a quote, skip to the closing quote, following standard
	     * C convention with \-escapes.  Note that read_entry will
	     * have already done some processing for \ chars at the end of
	     * input lines.
	     */
	    while (*entry && *entry != '"') {
		if (*entry == '\\') {
		    int c;

		    entry = c_quote(entry + 1, &c);
		    STR_NEXT(&str, c);
		} else {
		    STR_NEXT(&str, *entry++);
		}
	    }
	    if (*entry == '\0') {
		/*
		 * make sure that the string doesn't suddenly come
		 * to an end at a funny spot
		 */
		*error = eof_error(str.p + this_attribute);
		return NULL;
	    }
	    entry++;			/* skip the " */
	} else {
	    /*
	     * not in double quotes, only a limited set of characters
	     * are allowed in an unquoted string, though \ quotes any
	     * character.
	     */
	    while (*entry && (*entry == '\\' ||
			      index("!@$%^&*-_+~/?|<>:[]{}.`'", *entry) ||
			      isalnum(*entry)))
	    {
		if (*entry == '\\') {
		    entry++;
		    if (*entry == '\0') {
			/* must have something after \ */
			*error = eof_error(str.p + this_attribute);
			return NULL;
		    }
		}
		STR_NEXT(&str, *entry++);
	    }
	}
	STR_NEXT(&str, '\0');		/* close off the value */
	entry = skip_space(entry);

	/*
	 * make sure the entry ends in something reasonable
	 */
	if (*entry == ',') {
	    entry++;			/* commas are okay, and are ignored */
	} else if (*entry != '\0' && *entry != ';') {
	    /* end of string or ; separator are okay, anything else is not */
	    *error = xmalloc(strlen(str.p + this_attribute) +
			     sizeof("illegal attribute separator after "));
	    (void) sprintf(*error, "illegal attribute separator after %s",
			   str.p + this_attribute);
	    return NULL;
	}
    }
    STR_NEXT(&str, '\0');		/* two nul bytes signal the end */
    STR_DONE(&str);			/* finish off the string */

    /*
     * turn all this into the finished tokens
     */
    *error = finish_parse(str.p + strlen(str.p) + 1, generic, driver);

    if (*error) {
	return NULL;			/* error found in finish_parse */
    }

    return str.p;			/* entry name was first */
}

/*
 * c_quote - translate a \escape as C would within a string or char literal
 *
 * return next character to fetch from string and the actual char in val.
 */
static char *
c_quote(p, val)
    register char *p;			/* start point in string */
    register int *val;			/* put result char here */
{
    register int c;			/* current character */
    int cnt;

    switch (c = *p++) {
    case '\0':
	*val = '\0';
	return p - 1;
    case 'b':
	*val = '\b';
	return p;
    case 'e':
	*val = '\033';
	return p;
    case 'f':
	*val = '\f';
	return p;
    case 'g':
	*val = '\007';
	return p;
    case 'n':
	*val = '\n';
	return p;
    case 'r':
	*val = '\r';
	return p;
    case 't':
	*val = '\t';
	return p;
    case 'v':
	*val = '\v';
	return p;
    case 'x':
    case 'X':
	/*
	 * x or X followed by up to three hex digits form a char
	 */
	cnt = 3;
	*val = 0;
	while (cnt && (isdigit(c = *p++) ||
		       (c >= 'A' && c <= 'F') ||
		       (c >= 'a' && c <= 'f')))
	{
	    *val = (*val * 16) +
		(isdigit(c)? c - '0': isupper(c)? c - 'A': c - 'a');
	}
	return p - 1;
    case '0': case '1': case '2': case '3':
    case '4': case '5': case '6': case '7':
	/* handle the normal, octal, case of chars specified numerically */
	cnt = 2;			/* two more digits, three total */
	*val = c - '0';
	while (cnt && (c = *p++) >= '0' && c <= '7') {
	    *val = (*val * 8) + c - '0';
	}
	return p - 1;
    default:
	*val = c;
	return p;
    }
}

/*
 * eof_error - form an unexpected eof error on the given attribute
 */
static char *
eof_error(name)
    char *name;				/* name of attribute */
{
    static char *message = "unexpected end of string for attribute %s";
    char *error = xmalloc(strlen(name) + sizeof(message));

    (void) sprintf(error, message, name);
    return error;
}

/*
 * finish_parse - turn nul-separated token strings into an attribute list
 *
 * return an error message or NULL, return generic and driver attributes
 * in the appropriate passed list pointers.
 */
static char *
finish_parse(tokens, generic, driver)
    register char *tokens;		/* strings of nul-terminated tokens */
    struct attribute **generic;		/* generic attributes go here */
    struct attribute **driver;		/* driver attributes go here */
{
    struct attribute **attr = generic;	/* begin adding generic attributes */
    *generic = NULL;
    *driver = NULL;

    /*
     * loop, snapping up tokens until no more remain
     */
    while (*tokens) {
	struct attribute *new;

	if (*tokens == ';') {
	    /* after `;' parse driver attributes */
	    attr = driver;
	    tokens++;			/* otherwise ignore `;' */
	}

	/*
	 * get a new token and link it into the output list
	 */
	new = (struct attribute *)xmalloc(sizeof(*new));
	new->succ = *attr;
	(*attr) = new;

	/* fill in the name */
	new->name = tokens;
	/* step to the next token */
	tokens = tokens + strlen(tokens) + 1;

	/* check for boolean attribute form */
	if (new->name[0] == '-' || new->name[0] == '+') {
	    /* boolean value */
	    if (*tokens == '=') {
		/* can't have both [+-] and a value */
		return "mixed [+-]attribute and value assignment";
	    }

	    /*
	     * -name turns off attribute, +name turns it on
	     */
	    if (new->name[0] == '-') {
		new->value = off;
	    } else {
		new->value = on;
	    }
	    new->name++;		/* don't need [+-] anymore */
	} else {
	    if (*tokens == '=') {
		/* value token for attribute */
		new->value = tokens + 1; /* don't include `=' in the value */
		/* advance to the next token */
		tokens = tokens + strlen(tokens) + 1;
	    } else {
		/* just name is equivalent to +name */
		new->value = on;
	    }
	}
    }

    return NULL;
}


/*
 * parse_config - parse config file name/value pairs
 *
 * given a string, such as returned by read_entry, turn it into
 * a single attribute entry.  On error, return NULL, with an
 * error message in *error.
 */
struct attribute *
parse_config(entry, error)
    register char *entry;		/* config from read_entry */
    char **error;			/* return error message */
{
    struct str str;			/* area for building result */
    int attr_type = ' ';		/* `+' `-' or SPACE */
    int value_offset;			/* offset in str.p of value */
    struct attribute *attr = (struct attribute *)xmalloc(sizeof(*attr));

    attr->succ = NULL;
    STR_INIT(&str);

    /* can be preceded by whitespace */
    entry = skip_space(entry);

    if (*entry == '+') {
	entry++;			/* skip over a leading + */
	attr_type = '+';		/* positive boolean */
    } else if (*entry == '-') {
	entry++;			/* skip over a leading - */
	attr_type = '-';		/* negative boolean */
    }

    /*
     * get the attribute name
     */
    while (*entry && (isalnum(*entry) || *entry == '_' || *entry == '-')) {
	STR_NEXT(&str, *entry++);
    }
    STR_NEXT(&str, '\0');		/* termiante attribute name */

    entry = skip_space(entry);

    if (*entry == '\0') {
	/* boolean attribute */
	STR_DONE(&str);
	attr->name = str.p;
	if (attr_type == '-') {
	    attr->value = off;
	} else {
	    attr->value = on;
	}

	return attr;
    } else if (*entry != '=') {
	*error = "expected `=' after attribute name";
	return NULL;
    }

    if (attr_type != ' ') {
	*error = "unexpected pattern:  `= value' follows boolean attribute";
	return NULL;
    }

    /* form is name = value, find the value */

    entry = skip_space(entry + 1);

    value_offset = str.i;

    if (*entry == '"') {
	entry++;
	/* if a quote, skip to the closing quote (\ quotes next char) */
	while (*entry && *entry != '"') {
	    if (*entry == '\\') {
		int c;

		entry = c_quote(entry + 1, &c);
		STR_NEXT(&str, c);
	    } else {
		STR_NEXT(&str, *entry++);
	    }
	}
	if (*entry == '\0') {
	    /*
	     * make sure that the string doesn't suddenly come
	     * to an end at a funny spot
	     */
	    *error = "unexpected end of attribute";
	    return NULL;
	}
	entry++;			/* skip the " */
    } else {
	/*
	 * not in double quotes, only a limited set of characters
	 * are allowed in an unquoted string, though \ quotes any
	 * character.
	 */
	while (*entry && (*entry == '\\' ||
			  index("!@$%^&*-_+~/?|<>:[]{}.`'", *entry) ||
			  isalnum(*entry)))
	{
	    if (*entry == '\\') {
		entry++;
		if (*entry == '\0') {
		    /* must have something after \ */
		    *error = "unexpected end of attribute";
		    return NULL;
		}
	    }
	    STR_NEXT(&str, *entry++);
	}
    }
    STR_NEXT(&str, '\0');		/* close off the value */
    entry = skip_space(entry);

    /*
     * make sure this is really the end of the entry
     */
    if (*entry != '\0') {
	*error = "expected end of entry";
	return NULL;
    }

    STR_DONE(&str);

    attr->name = str.p;
    attr->value = str.p + value_offset;

    return attr;
}


/*
 * parse_table - parse an entry in a table file
 *
 * table files have entries of the form:
 *
 *	string1		string2
 *
 * returned attribute has string1 as name and string2 as value.
 */
struct attribute *
parse_table(entry, error)
    register char *entry;		/* config from read_entry */
    char **error;			/* return error message */
{
    struct attribute *attr = (struct attribute *)xmalloc(sizeof(*attr));
    struct str str;
    int offset_transport;		/* offset to transport in str.p */

    attr->succ = NULL;
    STR_INIT(&str);

    entry = skip_space(entry);
    while (*entry && !isspace(*entry) && *entry != '#') {
	STR_NEXT(&str, *entry++);
    }
    STR_NEXT(&str, '\0');		/* terminate name of host */

    entry = skip_space(entry);
    if (*entry == '\0') {
	*error = "unexpected end of entry";
	STR_FREE(&str);
	return NULL;
    }

    offset_transport = str.i;
    while (*entry && !isspace(*entry) && *entry != '#') {
	STR_NEXT(&str, *entry++);
    }
    STR_NEXT(&str, '\0');		/* terminate name of transport */

    entry = skip_space(entry);
    if (*entry) {
	*error = "expected end of entry";
	STR_FREE(&str);
	return NULL;
    }

    STR_DONE(&str);
    attr->name = str.p;
    attr->value = str.p + offset_transport;

    return attr;
}

/*
 * skip_space - skip over comments and white space
 *
 * a comment is a `#' up to the end of a line
 */
static char *
skip_space(p)
    register char *p;			/* current place in string */
{
    for (;;) {
	if (*p == '#') {
	    /* skip over comment */
	    p++;
	    while (*p && *p != '\n') {
		p++;
	    }
	} else if (!isspace(*p)) {
	    /* found something that isn't white space, return it */
	    return p;
	} else {
	    p++;			/* advance past the white-space char */
	}
    }
}


/*
 * read_entry - read an entry from a file into memory
 *
 * a director, router or transport file entry is terminated by a line
 * which does not begin with whitespace.
 *
 * return NULL on end of file or error.  The region return may be
 * reused for subsequent return values and should be copied if it
 * is to be preserved.
 */
char *
read_entry(f)
    register FILE *f;			/* input file */
{
    register int c;			/* current character */
    static struct str str;		/* build the entry here */
    static int inited = FALSE;		/* TRUE if str has been STR_INIT'd */

    /*
     * scan for the beginning of an entry, which begins at the first
     * non-white space, non-comment character
     */
    while ((c = getc(f)) != EOF && (isspace(c) || c == '#')) {
	if (c == '#') {
	    while ((c = getc(f)) != EOF && c != '\n') ;
	    if (c == EOF) {
		break;
	    }
	}
    }

    /*
     * no entry was found
     */
    if (c == EOF) {
	return NULL;
    }

    /*
     * copy characters up to the end of the entry.
     * Note, that initialized once and reused.
     */
    if (!inited) {
	inited = TRUE;
	STR_INIT(&str);
    } else {
	str.i = 0;			/* back to the beginning */
    }
    STR_NEXT(&str, c);			/* copy in the first character */

    /*
     * copy characters until a newline followed by non-white-space
     */
    while ((c = getc(f)) != EOF) {
	if (c == '\n') {
	    STR_NEXT(&str, c);
	    c = getc(f);
	    /*
	     * end of file or line beginning with non-white space
	     * marks the end.
	     */
	    if (c == '\n' || c == '#') {
		/* blank lines and comments don't count */
		(void) ungetc(c, f);
		continue;
	    }
	    if (c == EOF || (c != ' ' && c != '\t')) {
		break;
	    }
	}
	if (c == '\\') {
	    /* \newline is swallowed along with following white-space */
	    if ((c = getc(f)) == EOF) {
		break;
	    }
	    if (c == '\n') {
		while ((c = getc(f)) == ' ' || c == '\t') ;
	    } else {
		STR_NEXT(&str, '\\');
	    }
	}
	STR_NEXT(&str, c);
    }

    /*
     * that's the end of that entry
     */
    if (c != EOF) {
	(void) ungetc(c, f);		/* first character for next time */
    }
    STR_NEXT(&str, '\0');		/* end of the entry */
    return str.p;
}

#ifdef STANDALONE
/*
 * read from standard input and write out the compiled
 * entry information on the standard output
 */
void
main(argc, argv)
    int argc;				/* count of arguments */
    char *argv[];			/* vector of arguments */
{
    char *entry;			/* entry read from stdin */
    enum { config, table, other } file_type; /* type of file to look at */

    if (argc >= 2 && EQ(argv[1], "-c")) {
	file_type = config;
    } else if (argc >= 2 && EQ(argv[1], "-t")) {
	file_type = table;
    } else {
	file_type = other;
    }

    /*
     * read entries until EOF
     */
    while (entry = read_entry(stdin)) {
	if (file_type == config) {
	    char *error;
	    struct attribute *attr = parse_config(entry, &error);

	    if (attr == NULL) {
		(void) fprintf(stderr, "error in <stdin>: %s\n", error);
		exit(EX_DATAERR);
	    }
	    (void) printf("%s = %s\n", attr->name, attr->value);
	} else if (file_type == table) {
	    char *error;
	    struct attribute *attr = parse_table(entry, &error);

	    if (attr == NULL) {
		(void) fprintf(stderr, "error in <stdin>: %s\n", error);
		exit(EX_DATAERR);
	    }
	    (void) printf("%s = %s\n", attr->name, attr->value);
	} else {
	    struct attribute *generic;	/* generic attribute list */
	    struct attribute *driver;	/* driver attribute list */
	    char *error;		/* error message */
	    char *name = parse_entry(entry, &generic, &driver, &error);

	    if (name == NULL) {
		(void) fprintf(stderr, "error in <stdin>: %s\n", error);
		exit(EX_DATAERR);
	    }
	    (void) printf("Entry Name:  %s:\n    Generic Attributes:\n", name);
	    while (generic) {
		(void) printf("\t%s = %s\n", generic->name, generic->value);
		generic = generic->succ;
	    }
	    (void) printf("    Driver Attributes:\n");
	    while (driver) {
		(void) printf("\t%s = %s\n", driver->name, driver->value);
		driver = driver->succ;
	    }
	}
    }

    exit(EX_OK);
}
#endif	/* STANDALONE */
