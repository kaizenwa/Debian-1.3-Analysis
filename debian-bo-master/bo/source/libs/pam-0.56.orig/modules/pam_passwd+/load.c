/*
 * initialization routines
 * this initializes various things used to verify the
 * validity of the password
 */

#include "passwd.h"

static void set_pwsig (struct _options *opt);

/*
 * this table contains all the information one could ever want to know
 * about the internal variables
 */

struct intvar  iv[NUMIPVAR] = {
    { 'A', TY_STR,  0, 1 }, /* user variable A [%s] */
    { 'B', TY_STR,  1, 1 }, /* user variable B [%s] */
    { 'C', TY_STR,  2, 1 }, /* user variable C [%s] */
    { 'D', TY_STR,  3, 1 }, /* user variable D [%s] */
    { 'E', TY_STR,  4, 1 }, /* user variable E [%s] */
    { 'F', TY_STR,  5, 1 }, /* user variable F [%s] */
    { 'G', TY_STR,  6, 1 }, /* user variable G [%s] */
    { 'H', TY_STR,  7, 1 }, /* user variable H [%s] */
    { 'I', TY_STR,  8, 1 }, /* user variable I [%s] */
    { 'J', TY_STR,  9, 1 }, /* user variable J [%s] */
    { 'K', TY_STR, 10, 1 }, /* user variable K [%s] */
    { 'L', TY_STR, 11, 1 }, /* user variable L [%s] */
    { 'M', TY_STR, 12, 1 }, /* user variable M [%s] */
    { 'N', TY_STR, 13, 1 }, /* user variable N [%s] */
    { 'O', TY_STR, 14, 1 }, /* user variable O [%s] */
    { 'P', TY_STR, 15, 1 }, /* user variable P [%s] */
    { 'Q', TY_STR, 16, 1 }, /* user variable Q [%s] */
    { 'R', TY_STR, 17, 1 }, /* user variable R [%s] */
    { 'S', TY_STR, 18, 1 }, /* user variable S [%s] */
    { 'T', TY_STR, 19, 1 }, /* user variable T [%s] */
    { 'U', TY_STR, 20, 1 }, /* user variable U [%s] */
    { 'V', TY_STR, 21, 1 }, /* user variable V [%s] */
    { 'W', TY_STR, 22, 1 }, /* user variable W [%s] */
    { 'X', TY_STR, 23, 1 }, /* user variable X [%s] */
    { 'Y', TY_STR, 24, 1 }, /* user variable Y [%s] */
    { 'Z', TY_STR, 25, 1 }, /* user variable Z [%s] */
    { 'a', TY_NUM, 26, 0 }, /* alphanumerics */
    { 'b', TY_NUM, 27, 0 }, /* alphabetics */
    { 'c', TY_NUM, 28, 0 }, /* capitals */
    { 'd', TY_STR, 29, 0 }, /* domain name */
    { 'f', TY_STR, 30, 1 }, /* First Name [%s] */
    { 'g', TY_STR, 31, 0 }, /* group name */
    { 'h', TY_STR, 32, 0 }, /* host name */
    { 'i', TY_STR, 33, 1 }, /* Initials [%s] */
    { 'l', TY_NUM, 34, 0 }, /* small letters */
    { 'm', TY_STR, 35, 1 }, /* Middle Name [%s] */
    { 'n', TY_STR, 36, 1 }, /* Full Name [%s] */
    { 'o', TY_STR, 37, 1 }, /* Office [%s] */
    { 'p', TY_STR, 38, 0 }, /* new password */
    { 'q', TY_STR, 39, 0 }, /* old password */
    { 's', TY_STR, 40, 1 }, /* Last Name [%s] */
    { 't', TY_STR, 41, 1 }, /* Extension [%s] */
    { 'u', TY_STR, 42, 0 }, /* user name */
    { 'v', TY_NUM, 43, 0 }, /* mixed case */
    { 'w', TY_NUM, 44, 0 }, /* digits */
    { 'x', TY_STR, 45, 1 }, /* Home phone number [%s] */
    { '\0',TY_NUM, -1, 0 }, /* END-OF-LIST */
};

/*
 * Ensure that we don't strand memory in the heap
 */

void cleanup_load (struct _options *opt)
{
    struct intvar *ipx = iv;

    while (ipx->name != '\0') {
        IVCLEAN  (opt, ipx);
        IVSTRING (opt, ipx) = (char *) 0;
        IVLENGTH (opt, ipx) = (char *) 0;
	++ipx;
    }
}

/*
 * Reset the interpolated password and old password values.
 */

static void set_pwsig (struct _options *opt)
  {
    char buf[BUFSIZ];	/* buffer for new password */

    if (opt->pwsig > 0) {
        (void) strncpy (buf, opt->password, opt->pwsig);
	buf[opt->pwsig] = '\0';
	IVSASSIGN (opt, 'p', buf);
    } else
        IVSASSIGN (opt, 'p', opt->password);

    if (opt->pwsig > 0) {
        (void) strncpy (buf, opt->password_old, opt->pwsig);
	buf[opt->pwsig] = '\0';
	IVSASSIGN (opt, 'q', buf);
    } else
        IVSASSIGN (opt, 'q', opt->password_old);
  }

/*
 * load the number of significant chars in the password
 */

void loadsig (struct _options *opt, char *number)
{
    int sgn;		/* 1 if number is negative */
/*
 * skip leading blanks
 */
    while(isspace(*number))
        number++;
/*
 * grab the sign if any
 */
    sgn = 0;
    if (*number == '+' || *number == '-')
        sgn = (*number++ == '-');
/*
 * if there's no number, ignore the line
 */
    if (!isdigit(*number))
        return;
/*
 * read the number as the number of significant characters
 * and set the sign properly
 */
    opt->pwsig = 0;
    while(isdigit(*number))
        opt->pwsig = opt->pwsig * 10 + *number++ - '0';

    if (sgn)
        opt->pwsig = -(opt->pwsig);
    else
        if (opt->pwsig >= BUFSIZ)
	    opt->pwsig = BUFSIZ-1;

    set_pwsig (opt);
}

/*
 * this initializes the variables associated with the password
 */

void initpw (struct _options *opt)
{
    char buf[BUFSIZ];		/* buffer for string constants */
    const char *p;			/* pointer in a for loop */
    struct group *g;		/* points to user's group info */
    int len;			/* length of password */
    int iupper;			/* number of uppercase characters */
    int ilower;			/* number of lowercase characters */
    int ialpha;			/* number of alphabetic characters */
    int inum;			/* number of numeric characters */
    int ialnum;			/* number of alphanumeric characters */
/*
 * assign the password, name, host, domain, etc.
 */
    set_pwsig (opt);

    IVSASSIGN (opt,'u', opt->pwd->pw_name);
 
    g = getgrgid (opt->pwd->pw_gid);
    if (g == (struct group *) 0)
        p = (char *) 0;
    else
        p = g->gr_name;

    if (p == (char *) 0)
        p = "";

    IVSASSIGN (opt, 'g', p);

    p = "";
    if (findhost(buf, BUFSIZ) >= 0)
        p = (const char *) buf;
    IVSASSIGN (opt, 'h', p);

    p = "";
    if (finddomain(buf, BUFSIZ) >= 0)
        p = (char *) buf;
    IVSASSIGN (opt, 'd', p);
/*
 * do some quick counts of characters
 */
    len = iupper = ilower = inum = 0;
    p   = IVSTRING(opt, findiv('p'));
    while (*p) {
	if (isupper (*p))
	    ++iupper;
	else
	    if (islower (*p))
	        ++ilower;
	    else
	        if (isdigit (*p))
		    ++inum;
	++p;
        ++len;
    }

    ialpha = ilower + iupper;
    ialnum = ialpha + inum;

    IVNASSIGN (opt, 'a', ialnum, len - ialnum); 
    IVNASSIGN (opt, 'b', ialpha, len - ialpha); 
    IVNASSIGN (opt, 'c', iupper, len - iupper); 
    IVNASSIGN (opt, 'l', ilower, len - ilower);
    IVNASSIGN (opt, 'v', !((!!iupper) && (!!ilower)),
	                  ((!!iupper) && (!!ilower)));
    IVNASSIGN (opt, 'w', inum, len - inum); 
}

/*
 * load the escapes defined by the GECOS field
 */

int loadgecos (struct _options *opt, char *format)
{
    struct buffers {
        char data [GECOSSIZE];	/* Buffer area */
    } addrs[9];

    char *format_ptr, *fb;	/* used to collect the format string */
    char fbuf[BUFSIZ];		/* format string stuffed here */
    int indx;			/* counter in a for loop */
    int num_str;		/* number of strings read in */
    char pbuf[2*BUFSIZ];	/* buffer for gecos field (no &) */
    int found_first    = 0;	/* 1 if first name read */
    int found_middle   = 0;	/* 1 if middle name read */
    int found_last     = 0;	/* 1 if last name read */
    int found_full     = 0;	/* 1 if full name read */
    int found_initials = 0;	/* 1 if initials read */
    struct intvar *ip;		/* points to internal variable */
#define isname(chr) (isalpha(chr) || (chr) == '-')

    memset (addrs, 0, sizeof (addrs));
/*
 * expand gecos field, replacing & by login name
 */
    fb = pbuf;
    for (format_ptr = opt->pwd->pw_gecos; *format_ptr; ++format_ptr)
        if (*format_ptr == '&') {
	    if (opt->pwd->pw_name != (char *) 0) {
	        char *ob = opt->pwd->pw_name;
		while (*ob)
		    *fb++ = *ob++;
	    }
	} else
	    *fb++ = *format_ptr;
    *fb = '\0';
/*
 * Find the first quote in the format string
 */
    format_ptr = strchr (format, '"');
    if (format_ptr == (char *) 0)
        return(-1);
/*
 * skip initial quote, stuff string
 */
    fb = fbuf;

    while (*++format_ptr != '"') {
        if (*format_ptr == '\0') {
	    logfunc (opt, LG_SYNTAX,
		     "missing \" in GECOS specification on line %d",
		     opt->linect);
	    return(-1);
	}
	
	*fb++ = *format_ptr;
    }
    *fb = '\0';

    /* skip closing quote */
    format_ptr++;
/*
 * read in the strings
 */
    num_str = sscanf(pbuf, fbuf,
		     addrs[0].data, addrs[1].data, addrs[2].data,
		     addrs[3].data, addrs[4].data, addrs[5].data,
		     addrs[6].data, addrs[7].data, addrs[8].data);
/*
 * quickly check the format of the symbols
 */
    fb   = format_ptr;
    indx = 0;
    while (*fb && *fb != '\n') {
        if (isspace (*fb)) {
	    ++fb;
	    continue;
	}

        ip = findiv(*fb);
	if (ip == (struct intvar *) 0) {
	    logfunc (opt, LG_SYNTAX,
		     "bad GECOS specification at line %d (\"%s\")",
		     opt->linect, fb);
	    return (-1);
	}
/*
 * make the assignment or report an error
 */
	if (!ip->userset) {
	    logfunc (opt, LG_SYNTAX,
		     "bad GECOS specification at line %d (\"%s\")",
		     opt->linect, fb);
	    return (-1);
	}

        if (indx == num_str) {
	    logfunc (opt, LG_SYNTAX,
		  "wrong number of escapes in GECOS specification on line %d",
		     opt->linect);
	    return 1;
	}

	IVSPASSIGN(opt, ip, addrs[indx].data);

	switch (ip->name) {
	case 'f':	found_first++;    break;	/* first name */
	case 'm':	found_middle++;   break;	/* middle name */
	case 's':	found_last++;     break;	/* last name */
	case 'i':	found_initials++; break;	/* initials */
	case 'n':	found_full++;     break;	/* full name */
	}

	++indx;
	++fb;
    }
/*
 * Fetch the first portion of the name string
 */
    ip = findiv('n');
    if (found_full) {
        format_ptr = IVSTRING(opt,ip);
	fb = addrs[0].data;
	while (isname (*format_ptr))
	    *fb++ = *format_ptr++;
	*fb = '\0';

	while (*format_ptr && !isname(*format_ptr))
	    format_ptr++;
/*
 * Collect the second component of the name
 */
	fb = addrs[1].data;
	while (isname (*format_ptr))
	    *fb++ = *format_ptr++;
	*fb = '\0';

	while (*format_ptr && !isname(*format_ptr))
	    format_ptr++;
/*
 * Collect the third component of the name
 */
	fb = addrs[2].data;
	while (*format_ptr)
	    *fb++ = *format_ptr++;
	*fb = '\0';
/*
 * Process the format of "last, first middle"
 */
        if (strchr(IVSTRING(opt,ip), ',') != (char *) 0) {
	    found_full = 0;
	    if (addrs[2].data[0]) {
	        if (!found_last)
		    IVSASSIGN (opt, 's', addrs[0].data);
		if (!found_first)
		    IVSASSIGN (opt, 'f', addrs[1].data);
		if (!found_middle)
		    IVSASSIGN (opt, 'm', addrs[2].data);
	    } else {
	        if (addrs[1].data[0]) {
		    if (!found_last)
		        IVSASSIGN (opt, 's', addrs[0].data);
		    if (!found_first)
		        IVSASSIGN (opt, 'f', addrs[1].data);
		} else {
		    if (!found_last)
		        IVSASSIGN (opt, 's', addrs[0].data);
		}
	    }
/*
 * Process the format of "first middle last"
 */
	} else {
	    if (addrs[2].data[0]) {
	        if (!found_first)
		    IVSASSIGN (opt, 'f', addrs[0].data);
		if (!found_middle)
		    IVSASSIGN (opt, 'm', addrs[1].data);
		if (!found_last)
		    IVSASSIGN (opt, 's', addrs[2].data);
	    } else {
	        if (addrs[1].data[0]) {
		    if (!found_first)
		        IVSASSIGN (opt, 'f', addrs[0].data);
		    if (!found_last)
		        IVSASSIGN (opt, 's', addrs[1].data);
		} else {
		    if (!found_last)
		        IVSASSIGN (opt, 's', addrs[0].data);
		}
	    }
	}
    }
/*
 * If there is no full name at this point then attempt to construct it
 * from the pieces. (The fullname is reset for the format "last, first".)
 */
    if (!found_full) {
        fbuf[0] = '\0';
	ip = findiv('f');
	if (IVSTRING (opt, ip) != (char *) 0)
	    (void) strcat(fbuf, IVSTRING (opt, ip));

	ip = findiv('m');
	if (IVSTRING (opt, ip) != (char *) 0) {
	    if (fbuf[0] != '\0')
	        (void) strcat(fbuf, " ");
	    (void) strcat(fbuf, IVSTRING (opt, ip));
	}

	ip = findiv('s');
	if (IVSTRING (opt, ip) != (char *) 0) {
	    if (fbuf[0] != '\0')
	        (void) strcat(fbuf, " ");
	    (void) strcat(fbuf, IVSTRING (opt, ip));
	}
/*
 * Assign the fullname field
 */
	if (strlen((char *) fbuf) > 0)
	    IVSASSIGN (opt, 'n', fbuf);
    }
/*
 * initials not yet known -- fill them in
 */
    if (!found_initials) {
        format_ptr = fbuf;
	ip = findiv('f');
	if (IVSTRING(opt,ip) != (char *) 0 && *IVSTRING(opt,ip) != '\0')
	    *format_ptr++ = tolower (*IVSTRING(opt,ip));

	ip = findiv('m');
	if (IVSTRING(opt,ip) != (char *) 0 && *IVSTRING(opt,ip) != '\0')
	    *format_ptr++ = tolower (*IVSTRING(opt,ip));

	ip = findiv('s');
	if (IVSTRING(opt,ip) != (char *) 0 && *IVSTRING(opt,ip) != '\0')
	    *format_ptr++ = tolower (*IVSTRING(opt,ip));

	*format_ptr = '\0';
/*
 * The result is the initial strings
 */
	if (strlen((char *) fbuf) > 0)
	    IVSASSIGN (opt, 'i', fbuf);
    }
/*
 * if debugging print out some goodies
 */
    for (indx = 0; iv[indx].name != '\0'; indx++) {
        if (iv[indx].userset)
	    logfunc (opt, LG_DEBUG, "variable %c: \"%s\",\"%s\"\n",
		     iv[indx].name,
		     IVSTRING (opt, &iv[indx]),
		     IVLENGTH (opt, &iv[indx]));
    }
    return(0);
}

/*
 * set a variable
 */

void setvar (struct _options *opt, char *buf)
{
    char keychar;		/* what to set */
    int  closechar;		/* how to end value string */
    struct intvar *ipx;		/* Pointer to the variable to be set */
    char valbuf[BUFSIZ];	/* value */
/*
 * eat leading spaces; if nothing, done!
 */
    while (isspace(*buf))
        buf++;

    if (!*buf)
        return;
/*
 * Fetch the variable name and skip to the string value.
 */
    keychar = *buf++;
    while(isspace(*buf))
      buf++;
/*
 * now act on it
 */
    closechar = *buf++;
    switch (closechar) {
    case '"':			/* string */
    case '\'':			/* string */
	break;

    case '[':			/* file name */
        closechar = ']';
	break;

    case '{':			/* program name */
        closechar = '}';
	break;

    default:			/* something else (bad) */
        logfunc (opt, LG_SYNTAX,
		 "bad SETVAR specification at line %d (\"%s\")",
		 opt->linect, &buf[-1]);
	return;
    }
/*
 * copy the value into the buffer
 * if not terminated properly, error
 */
    buf = getcstring (buf, valbuf, closechar);

    if (*buf != closechar) {
        logfunc (opt, LG_SYNTAX,
		 "missing %c in SETVAR specification on line %d",
		 closechar, opt->linect);
	return;
    }
/*
 * if it's a program or a file, grab the first line
 */
    if (closechar == ']' && 
	firstline (opt, valbuf, valbuf, BUFSIZ, fopen, fclose) == 0)
        return;
    else
        if (closechar == '}' && 
	    firstline(opt, valbuf, valbuf, BUFSIZ, popen, pclose) == 0)
	    return;
/*
 * now set the value for the output. Only strings may be set by the
 * user so there is no need to do anything special for numbers.
 */
    ipx = findiv(keychar);
    if ((ipx == (struct intvar *) 0) || (ipx->userset == 0)) {
        logfunc (opt, LG_SYNTAX,
		 "bad SETVAR specification at line %d (\"%s\")",
		 opt->linect, &buf[-1]);
    }
    else
        IVSPASSIGN (opt, ipx, valbuf);
}

/*
 * load the logging level
 */

void loadlevel (struct _options *opt, char *line)
{
    beginlog(opt, line);
}

/*
 * returns a pointer to an internal variable
 */

struct intvar *findiv (int c)
{
    register struct intvar *ip;	/* pointer to internal variable */
  
    for(ip = &iv[0]; ip->name != '\0'; ip++)
        if (((ip->name ^ c) & 0xFF) == 0)
	    return(ip);
    return((struct intvar *) 0);
}
