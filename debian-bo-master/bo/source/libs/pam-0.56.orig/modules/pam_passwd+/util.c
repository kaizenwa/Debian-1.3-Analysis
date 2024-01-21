/*
 * various utilities, like stream readers, error handlers, etc.
 */

#include "passwd.h"
#include <sys/utsname.h>

/*
 * convert an integer to a string representing that number
 */

char *tonum (int n)
{
    char buf[BUFSIZ];		/* temporary buffer */

    (void) sprintf (buf, "%d", n);
    return (strdup(buf));
}

/*
 * for the string formatting
 */

char *sfmt (char *ptr, int sgn, int n1, int n2, int num,
	    char *string, char *length)
{
    int slen;		/* pctstring length */
/*
 * if the string is empty, skip it
 */
    if (string == (char *) 0)
	return(ptr);
/*
 * just a giant switch statement
 */
    switch (num) {
    case F_NUMBER:			/* length */
/*
 * just copy the value
 */
	for (n1 = 0; length[n1]; n1++)
	    *ptr++ = length[n1];
	break;

    case F_LOWER:			/* lower case */
/*
 * string is shorter than beginning mark
 */
        if (sgn) {
	    slen = strlen(string);
	    if (slen <= n1)
		return(ptr);

	    if (slen <= n2)
		n2 = slen - 1;
/*
 * just copy the indicated parts of the string, reversed
 */
	    for( ; n2 >= n1; n2--)
		*ptr++ = lowcase(string[n2]);
	} else {
/*
 * string is shorter than beginning mark
 */
	    slen = strlen(string);
	    if (slen <= n1)
		return(ptr);
/*
 * just copy the indicated parts of the string
 */
	    for( ; n1 <= n2 && string[n1]; n1++)
		*ptr++ = lowcase(string[n1]);
	}
	break;

	case F_FIRST:			/* first upper case */
/*
 * string is shorter than beginning mark
 */
        if (sgn) {
	    slen = strlen(string);
	    if (slen <= n1)
		return(ptr);

	    if (slen <= n2)
		n2 = slen - 1;
/*
 * just copy the indicated parts of the string, reversed
 */
	    for( ; n2 >= n1; n2--) {
		if (n2 == 0)
		    *ptr++ = upcase(string[n2]);
		else
		    *ptr++ = string[n2];
	    }
	} else {
/*
 * string is shorter than beginning mark
 */
	    slen = strlen(string);
	    if (slen <= n1)
		return(ptr);
/*
 * just copy the indicated parts of the string
 */
	    for( ; n1 <= n2 && string[n1]; n1++) {
		if (n1 == 0)
		    *ptr++ = upcase(string[n1]);
		else
		    *ptr++ = string[n1];
	    }
	}
	break;

    case F_UPPER:			/* upper case */
/*
 * string is shorter than beginning mark
 */
        if (sgn) {
	    slen = strlen(string);
	    if (slen <= n1)
		return(ptr);

	    if (slen <= n2)
		n2 = slen - 1;
/*
 * just copy the indicated parts of the string, reversed
 */
	    for( ; n2 >= n1; n2--)
		*ptr++ = upcase(string[n2]);
	} else {
/*
 * string is shorter than beginning mark
 */
	    slen = strlen(string);
	    if (slen <= n1)
		return(ptr);
/*
 * just copy the indicated parts of the string
 */
	    for( ; n1 <= n2 && string[n1]; n1++)
		*ptr++ = upcase(string[n1]);
	}
	break;

    default:			/* as is */
        if (sgn) {
/*
 * string is shorter than beginning mark
 */
	  slen = strlen(string);
	  if (slen <= n1)
	      return(ptr);

	  if (slen <= n2)
	      n2 = slen - 1;
/*
 * just copy the indicated parts of the string, reversed
 */
	  while(n2 >= n1)
	      *ptr++ = string[n2--];
	} else {
/*
 * string is shorter than beginning mark
 */
	  slen = strlen(string);
	  if (slen <= n1)
	      return(ptr);
/*
 * just copy the indicated parts of the string
 */
	  while (n1 <= n2 && string[n1])
	      *ptr++ = string[n1++];
	}
	break;
    }
/*
 * return the new string position
 */
    return(ptr);
}

/*
 * for the number formatting
 */

char *nfmt (char *ptr, int sgn, char *string, char *length)
  {
/*
 * If the entry is negative the use the complement value stored in the length.
 */
    if (sgn)
	string = length;

    if (string != (char *) 0)
      {
	while (*string)
	    *ptr++ = *string++;
      }
/*
 * return the new string position
 */
    return(ptr);
  }

/*
 * read a C string
 * returns position to be read next (just after closing quote)
 */

char *getcstring (char *from, char *to, int quo)
{
    int n;		/* number from \nnn escape */
    int i;		/* counter in a for loop */
/*
 * go until string ends or you hit a NUL
 */
    while (*from && (int) (unsigned int) *((char *) from) != quo) {
/*
 * not an escape -- take anything
 */
	if (*from++ != '\\') {
	    *to++ = from[-1];
	    continue;
	}
/*
 * escapes
 * -------
 * \ddd -- octal bit pattern
 */
	if (isdigit(*from)) {
	    n = 0;
	    for (i = 0; i < 3 && isdigit(*from); i++)
	        n = n * 10 + *from++ - '0';
	    *to++ = (char) (n&0xff);
	    continue;
	}
/*
 * \xDD -- hexadecimal bit pattern
 */
	if (*from == 'x' || *from == 'X') {
	    n = 0;
	    ++from;
	    while (isxdigit (*from)) {
	        i = (int) *from++;
		if (i >= 'a')
		    i ^= ('A' ^ 'a');
		i -= '0';
		if (i >= 10)
		    i -= 7;
		n <<= 4;
		n  += i;
	    }
	    *to++ = (char) (n & 0xff);
	    continue;
	}
/*
 * \c --- standard C escape
 */
	switch (*++from) {
	case 'n':	 *to++ = '\n';		break;
	case 't':	 *to++ = '\t';		break;
	case 'b':	 *to++ = '\b';		break;
	case 'r':	 *to++ = '\r';		break;
	case 'f':	 *to++ = '\f';		break;
	case '\\':	 *to++ = '\\';		break;
	default:	 *to++ = from[-1];	break;
	}
    }
    *to = '\0';
    return (from);
}

/*
 * gets the host name, if possible
 */

int findhost (char *name, int siznam)
{
    struct utsname uts;
    if (uname (&uts) < 0)
        return -1;

    strncpy (name, uts.nodename, siznam);
    return 0;
}
	
/*
 * gets the domain name, if possible
 */

int finddomain (char *name, int siznam)
  {
    register char *p;	/* used to get domain part of host name */
    struct utsname uts;
    int  value;

    value = uname (&uts);
    if (value < 0)
        return value;

    if (uts.domainname[0] != '\0' && strcmp (uts.domainname, "(none)")) {
        strncpy (name, uts.domainname, siznam);
	return 0;
    }
/*
 * everything after first "." is it
 */
    p = strchr (uts.nodename, '.');
    if (p == (char *) 0)
        return (-1);

    if (*++p == '\0')
        return(-1);
/*
 * copy it in
 */
    (void) strncpy(name, p, siznam);
    return(0);
}
