/*
#ident	"@(#)smail/src:RELEASE-3_2:string.c,v 1.21 1996/02/28 06:47:20 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * strings.c:
 *	miscellaneous string operations
 *
 *	external functions:  strcmpic, strncmpic, strip, strcolon,
 *			     is_string_in_list, strerror, strsysexit,
 *			     str_printf, xprintf, dprintf, c_atol,
 *			     base62, read_line, str_cat, str_ncat,
 *			     vfprintf, ivaltol, ltoival, copy, rcopy
 */
#include <sys/types.h>
#include <stdio.h>
#include <ctype.h>
#include "defs.h"
#ifndef STANDALONE
# include <varargs.h>
# include "smail.h"
# include "dys.h"
# ifndef DEPEND
#  include "extern.h"
#  include "exitcodes.h"
# endif
#else /* STANDALONE */
extern char lowcase[];			/* lower case conversion table */
extern char upcase[];			/* upper case conversion table */
#endif /* STANDALONE */

/* functions local to this file */
static char *bltoa();
static void str_printf_va();


/*
 * str2lower - convert a string to lower case....
 */
char *
str2lower(s)
    register char *s;
{
    register char *sp;

    for (sp = s; *sp; sp++)
	*sp = lowercase(*sp);

    return s;
}

/*
 * strcmpic - case independent strcmp function
 */
int
strcmpic(s1, s2)
    register char *s1, *s2;		/* strings to be compared */
{
    register int c1, c2;		/* temp */

    /* Make sure that strings exist */
    if ((s1 == NULL) || (s2 == NULL))
	return ((s1 == s2) ? 0 : ((s2 == NULL) ? 1 : -1));

    while (*s1 && *s2) {
	if ((c1 = lowercase(*s1++)) != (c2 = lowercase(*s2++))) {
	    return c1-c2;
	}
    }

    /*
     * one or both chars must be `\0'.  If only one is `\0', then
     * the other string is longer.
     */
    return (int)((*s1)-(*s2));
}

/*
 * strcmpic - case independent strcmp function
 */
int
strncmpic(s1, s2, n)
    register char *s1, *s2;		/* strings to compare */
    int n;				/* compare up to this many chars */
{
    register int c1, c2;		/* temp */
    register int cnt = n;		/* count of chars so far compared */

    while (*s1 && *s2 && cnt > 0) {
	if ((c1 = lowercase(*s1++)) != (c2 = lowercase(*s2++))) {
	    return c1-c2;
	}
	cnt--;				/* count this character */
    }

    /*
     * If we ran out of chars, then the string segments are equal, otherwise
     * one or both strings must have ended. In this case the subtraction
     * will show which one is shorter, if any.
     */
    return cnt ? (int)((*s1)-(*s2)) : 0;
}


/*
 * strip - strip quotes and escaped characters
 */
int
strip(addr)
    register char *addr;		/* strip this address */
{
    int was_stripped = FALSE;		/* TRUE if any stripping was done */
    register char *p = addr;		/* write pointer to addr */
    register int c;			/* read char in addr */

    while ((c = *addr++)) {
	if (c == '\\') {		/* skip to char after \ */
	    *p++ = *addr++;
	    was_stripped = TRUE;
	} else if (c == '"') {		/* don't copy quote char */
	    was_stripped = TRUE;
	} else {
	    *p++ = c;
	}
    }
    *p++ = '\0';			/* end of string */

    return was_stripped;
}

/*
 * strcolon - step through string parts separated by colons
 *
 * when called with a string, return a copy of the first part of
 * the string up to, but excluding the first `:'.  When called with
 * NULL return a copy of the next part of the previously passed string,
 * with each part separated by a colon `:'.
 *
 * return NULL if no more parts are left.
 *
 * strcolon is typically used in a loop on ':' separated names such as:
 *
 *	for (p = strcolon(names); p; p = strcolon((char *)NULL)) {
 *	    ... do something with the name p ...
 *	}
 *
 * the malloc region returned is reused, so if you wish to keep a string
 * around, you will need to copy it.
 */
char *
strcolon(s)
    register char *s;			/* string or NULL */
{
    static char *next = NULL;		/* pointer to next ':' */
    static char *region = NULL;		/* region used to store result */
    static int alloc = 0;		/* alloc size of region */

    if (!s) {
	s = next;
	if (s == NULL) {
	    return NULL;
	}
    }
    next = index(s, ':');
    if (next) {
	register int len = next - s;

	if (len >= alloc) {
	    if (region == NULL) {
		region = xmalloc(alloc = len + 1);
	    } else {
		region = xrealloc(region, alloc = len + 1);
	    }
	}
	(void) memcpy(region, s, (size_t) (next - s));
	region[next - s] = '\0';
	next++;
	return region;
    }
    return s;
}

/*
 * is_string_in_list - return true if string is in colon-separated list
 *
 * given a string and a colon separated list of strings, return TRUE
 * if the given string is in the list, else FALSE.  Case is not
 * significant in comparisons.
 */
int
is_string_in_list(string, list)
    register char *string;		/* string to look for */
    char *list;				/* list of strings */
{
    register char *s;

    for (s = strcolon(list); s; s = strcolon((char *)NULL)) {
	if (EQIC(string, s)) {
	    return TRUE;
	}
    }

    return FALSE;
}


#ifndef HAVE_STRERROR

/*
 * strerror -  return a string representing the error stored in errno.
 */
char *
strerror(errnum)
    int errnum;
{
    static char misc_err[50];		/* used when sprintf must be used */
#if 0
    extern const char *const sys_errlist[];	/* 4.4BSD */
#else
    extern char *sys_errlist[];		/* list of error strings */
#endif
    extern int sys_nerr;		/* number of entries in sys_errlist */

    if (errnum > sys_nerr || errnum < 0) {
	/* there is no entry for it in sys_errlist, build one */
	(void) sprintf(misc_err, "Unknown errno (%d)", errnum);
	return misc_err;
    } else {
	/* there is an entry in sys_errlist, use it */
	return sys_errlist[errnum];
    }
}

#endif /* HAVE_STRERROR */

/*
 * strsysexit - return a string corresponding to an exit code.
 */
char *
strsysexit(exitcode)
    int exitcode;
{
    static char buf[50];		/* buffer for generating message */

    switch (exitcode) {
    case EX_USAGE:
	return "EX_USAGE";
    case EX_DATAERR:
	return "EX_DATAERR";
    case EX_NOINPUT:
	return "EX_NOINPUT";
    case EX_NOUSER:
	return "EX_NOUSER";
    case EX_NOHOST:
	return "EX_NOHOST";
    case EX_UNAVAILABLE:
	return "EX_UNAVAILABLE";
    case EX_SOFTWARE:
	return "EX_SOFTWARE";
    case EX_OSERR:
	return "EX_OSERR";
    case EX_OSFILE:
	return "EX_OSFILE";
    case EX_CANTCREAT:
	return "EX_CANTCREAT";
    case EX_IOERR:
	return "EX_IOERR";
    case EX_TEMPFAIL:
	return "EX_TEMPFAIL";
    case EX_PROTOCOL:
	return "EX_PROTOCOL";
    case EX_NOPERM:
	return "EX_NOPERM";
    default:
	(void) sprintf(buf, "EX_%d", exitcode);
	return buf;
    }
}


/*
 * str_printf - highly simplified printf to a dynamic string
 * str_printf_va - ditto, but taking a va_list parameter
 *
 * note that we only support %s, %d, %o, %x and %c.  Also support
 * a %z which inserts a null byte (support %N for backward compatibility).
 */
/*VARARGS*/
void
str_printf(sp, fmt, va_alist)
    struct str *sp;			/* append to this string */
    char *fmt;				/* printf-style format string  */
    va_dcl
{
    va_list ap;				/* placeholder for varargs */

    va_start(ap);
    str_printf_va(sp, fmt, ap);
    va_end(ap);
}

static void
str_printf_va(sp, fmt, ap)
    register struct str *sp;		/* append to this string */
    register char *fmt;			/* printf-style format string */
    va_list ap;				/* placeholder for varargs */
{
    register int c;			/* current char in fmt */
    register int islong;                /* to handle %ld */
    long n;                             /* temp */
    char *s;				/* temp */

    /*
     * loop on the format string copying into the string sp
     */
    while ((c = *fmt++)) {
	if (c != '%') {
	    STR_NEXT(sp, c);
	} else {
	    if ((islong = (*fmt == 'l')))
		fmt++;
	    switch (c = *fmt++) {
	    case '\0':
		STR_NEXT(sp, '%');
		--fmt;
		break;
	    case 's':
		if ((s = va_arg(ap, char *))) {
		    STR_CAT(sp, s);
		} else {
		    STR_CAT(sp, "(null)");
		}
		break;
	    case 'c':
		STR_NEXT(sp, va_arg(ap, int));
		break;
	    case 'o':
		n = islong ? va_arg(ap,long) : (long)va_arg(ap,unsigned);
		STR_CAT(sp, bltoa(8, n));
		break;
	    case 'x':
		n = islong ? va_arg(ap,long) : (long)va_arg(ap,unsigned);
		STR_CAT(sp, bltoa(16, n));
		break;
	    case 'u':
		n = islong ? va_arg(ap,long) : (long)va_arg(ap,unsigned);
		STR_CAT(sp, bltoa(10, n));
		break;
	    case 'd':
		n = islong ? va_arg(ap,long) : (long)va_arg(ap,int);
		if (n < 0) {
		    STR_NEXT(sp, '-');
		    n = -n;
		}
		STR_CAT(sp, bltoa(10, n));
		break;
	    case 'N':			/* how to insert a nul byte */
	    case 'z':
		STR_NEXT(sp, '\0');
		break;				/* FIXME: ???? */
	    case '%':
		STR_NEXT(sp, '%');
		break;
	    default:
		break;
	    }
	}
    }

    /*
     * add a trailing null, but make sure the next character will
     * overwrite it.
     */

    STR_NEXT(sp, '\0');
    sp->i--;
}

/*
 * bltoa - convert long integer to string representation in given base
 *
 * standard bug about pointing to static data.
 */
static char *
bltoa(base, n)
    register unsigned base;		/* base for conversion */
    register long     n;                /* number to convert */
{
    static char buf[BITS_PER_INT + 1];	/* plenty big */
    register char *p = buf + sizeof(buf); /* start at end and move backward */
    register int i;

    *--p = '\0';			/* terminate string */
    if (n == 0) {
	/* special case, 0 is just "0" */
	*--p = '0';
	return p;
    }
    /* get more significant digits until no more are required */
    while (n > 0) {
	/* allow for bases up to 16 */
	i = (int) (n % base);
	n /= base;
	*--p = "0123456789abcdef"[i];
    }
    return p;
}

/*
 * xprintf - str_print to a region, returning pointer to region
 */
/*VARARGS1*/
char *
xprintf(fmt, va_alist)
    char *fmt;				/* str_printf-style format string */
    va_dcl
{
    struct str str;
    va_list ap;

    STR_INIT(&str);
    va_start(ap);
    str_printf_va(&str, fmt, ap);
    va_end(ap);
    STR_NEXT(&str, '\0');
    STR_DONE(&str);
    return str.p;
}

/*
 * dprintf - debugging printf to a file
 *
 * This allows the DEBUGx() macros to explicitly follow the convention that
 * printing a NULL string pointer displays "(null)".  The System V
 * printf() does not follow this convention, and thus cannot be used.
 */
/*VARARGS2*/
int
dprintf(file, fmt, va_alist)
    FILE *file;
    char *fmt;
    va_dcl
{
    static struct str str;
    static int inited = FALSE;
    va_list ap;

    if (! inited) {
	inited = TRUE;
	STR_INIT(&str);
    } else {
	str.i = 0;
    }
    va_start(ap);
    str_printf_va(&str, fmt, ap);
    va_end(ap);
    STR_NEXT(&str, '\0');

    return fputs(str.p, file);
}

/*
 * c_atol - convert an ascii string to a long, C-style.
 *
 * Handle the forms  [+-]?0[0-7]* [+-]?0x[0-9a-f]* and [+-]?[1-9][0-9]*.
 * an optional suffix of one of the letters:  k, K, or M is allowed, as
 * a multplier by 1024 or 1048576.  If the string given is malformed,
 * error will be set to an error message.
 */
long
c_atol(input, error)
    register char *input;		/* input string */
    char **error;			/* return error message here */
{
    char *input_string = input;
    long val = 0;
    int sign = 1;
    int digval;
    int base = 10;

    /* handle a leading sign character */
    if (*input == '-') {
	sign = -1;
	input++;
    } else if (*input == '+') {
	input++;
    }

    /* what is the base */
    if (*input == '0') {
	input++;
	base = 8;
	if (*input == 'x' || *input == 'X') {
	    base = 16;
	    input++;
	}
    }

    /* decode the number */
    while (*input) {
	if (isdigit(*input)) {
	    digval = *input++ - '0';
	} else {
	    if (islower(*input)) {
		digval = *input++ - 'a' + 10;
	    } else if (isupper(*input)) {
		digval = *input++ - 'A' + 10;
	    } else {
		break;
	    }
	}
	if (digval >= base) {
	    --input;
	    break;
	}
	val = val*base + digval;
    }

    /* set the correct sign */
    val *= sign;

    /* is there an optional multiplier? */
    if (*input == 'k' || *input == 'K') {
	input++;
	val *= 1024;
    }
    if (input && *input == 'M') {
	input++;
	val *= 1024 * 1024;
    }

    /* there should be nothing left of the input string at this point */
    if (error && *input) {
	*error = xprintf("invalid number: %s", input_string);
    }
    return val;
}

/*
 * base62 - convert a long integer into ASCII base 62
 *
 * uses upper and lowercase letters, plus numbers.
 * always returns a string of exactly 6 characters, plus a null byte.
 *
 * returns a static area which is reused for each call.
 */
char *
base62(val)
    unsigned long val;
{
    static char base62_chars[] =
	"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
    static char buf[7];
    register char *p = buf + sizeof(buf) - 1;

    *p = '\0';
    while (p > buf) {
	*--p = base62_chars[val % 62];
	val /= 62;
    }

    return buf;
}

#ifdef notused
/*
 * read_line - read a line from an input stream and return a pointer to it
 *
 * Any trailing newline is stripped from the returned string.
 *
 * returns a static area which may be reused after subsequent calls.
 */
char *
read_line(f)
    register FILE *f;
{
    static int inited = FALSE;
    static struct str line;
    register int c;

    if (! inited) {
	inited = TRUE;
	STR_INIT(&line);
    } else {
	line.i = 0;
    }
    while ((c = getc(f)) != '\n' && c != EOF) {
	STR_NEXT(&line, c);
    }
    STR_NEXT(&line, '\0');
    return line.p;
}
#endif	/* notused */

/*
 * str_cat - concatenate a C string onto the end of a dynamic string
 * str_ncat - concatenate a C string with a given length onto the
 *	      end of a dynamic string
 *
 * Concatenate a string onto the end of a dynamic string region,
 * growing the region as necessary.
 */

void
str_cat(sp, s)
    register struct str *sp;
    char *s;
{
    str_ncat(sp, s, (unsigned)strlen(s) + 1);
    sp->i--;
}

void
str_ncat(sp, s, n)
    register struct str *sp;
    char *s;
    unsigned n;
{
    if (sp->i + n > sp->a) {
	/*
	 * need to expand the region:
	 * bump up to a sufficiently large region which is a multiple
	 * of STR_BUMP (except for a pointer).  Allow at least 10 free
	 * chars in the region, for future expansion.
	 */
	sp->a = ((sp->i + n + STR_BUMP + 10) & ~(STR_BUMP-1)) - sizeof(long);
	sp->p = xrealloc(sp->p, sp->a);
    }
    /* copy string to the end of the region, copy the nul byte but
     * don't count it */
    (void) memcpy(sp->p + sp->i, s, n);
    sp->i += n;
}

#ifndef HAVE_VFPRINTF
/*
 * vfprintf - a hacked version of vfprintf() for sites that don't have it
 *
 * XXX - will _doprnt() work here?
 */
int
vfprintf(file, fmt, ap)
    FILE *file;
    char *fmt;
    va_list ap;
{
    int a,b,c,d,e,f,g,h,i,j,k,l,m,n,o;

    a = va_arg(ap, int);
    b = va_arg(ap, int);
    c = va_arg(ap, int);
    d = va_arg(ap, int);
    e = va_arg(ap, int);
    f = va_arg(ap, int);
    g = va_arg(ap, int);
    h = va_arg(ap, int);
    i = va_arg(ap, int);
    j = va_arg(ap, int);
    k = va_arg(ap, int);
    l = va_arg(ap, int);
    m = va_arg(ap, int);
    n = va_arg(ap, int);
    o = va_arg(ap, int);

    return fprintf(file, fmt, a,b,c,d,e,f,g,h,i,j,k,l,m,n,o);
}
#endif	/* HAVE_VFPRINTF */

/*
 * ivaltol - convert time interval string into a long integer
 *
 * Take a string defining an interval and convert it into seconds.  An
 * interval is the sum of seconds desribed by terms formed from a
 * decimal integer and a suffix.  The suffix gives a multiplier for
 * the term and can be one of 'y' (years), 'w' (weeks), 'd' (days),
 * 'h' (hours), 'm' (minutes) or 's' (seconds).  If the last term does
 * not have a suffix, 's' is assumed.  For example, an interval string
 * of "1h10m3s" would represent one hour, ten minutes and 30 seconds
 * and would be converted to the equivalent number of seconds.
 *
 * A negative integer is returned for illegal intervals.
 *
 * Note:  interval values are only useful if they fit within an
 *	  unsigned integer, so callers of this routine should do range
 *	  checking to make sure the value returned is not too large.
 */
long
ivaltol(s)
    register char *s;			/* string containing queue interval */
{
    long ret = 0;			/* return value */
    long cur = 0;			/* value of current part */

    while (*s) {
	switch (*s++) {
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
	    cur = cur * 10 + s[-1] - '0';
	    break;

	case 'y':			/* years = 365.24 days */
	    ret += (cur * 60*60*24*365) + (cur * 60*60*24*24)/100;
	    cur = 0;
	    break;

	case 'w':			/* weeks */
	    ret += cur * 60*60*24*7;
	    cur = 0;
	    break;

	case 'd':			/* days */
	    ret += cur * 60*60*24;
	    cur = 0;
	    break;

	case 'h':			/* hours */
	    ret += cur * 60*60;
	    cur = 0;
	    break;

	case 'm':
	    ret += cur * 60;		/* minutes */
	    cur = 0;
	    break;

	case 's':
	    ret += cur;			/* seconds */
	    cur = 0;
	    break;

	default:
	    return -1L;
	}
    }

    return ret + cur;			/* all done */
}

/*
 * ltoival - convert long integer into a time interval string
 *
 * Break down an interval into weeks/days/hours/minutes/seconds,
 * expressing it as it might have been input to ivaltol.
 */
char *
ltoival(n)
    long n;
{
    static char ret[40];
    static char units[] = "smhd";
    int vals[sizeof(units) - 1];
    int i;

    if (n < 0) {
	return NULL;
    }
    if (n == 0) {
	return "0";
    }

    vals[0] = (n % 60);
    n /= 60;
    vals[1] = (n % 60);
    n /= 60;
    vals[2] = (n % 24);
    n /= 24;
    vals[3] = (n % 7);
    n /= 7;

    if (n) {
	sprintf(ret, "%ldw", n);
    } else {
	ret[0] = '\0';
    }
    for (i = sizeof(units) - 2; i >= 0; --i) {
	if (vals[i]) {
	    sprintf(ret + strlen(ret), "%d%c", vals[i], units[i]);
	}
    }
    return ret;
}

/*
 * copy - copy a NUL-terminated string
 * rcopy - copy a string given start and end+1
 */

char *
copy(s)
    char *s;
{
    return rcopy(s, s + strlen(s));
}

char *
rcopy(s, e)
    char *s, *e;
{
    char *p;
    int n;

    n = (e - s);
    p = xmalloc(n + 1);
    (void) memcpy(p, s, (size_t) n);
    p[n] = '\0';
    return p;
}
