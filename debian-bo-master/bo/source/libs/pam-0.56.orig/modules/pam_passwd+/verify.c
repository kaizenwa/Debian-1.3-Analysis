/*
 * this is the driver for the password tester
 */

#include "passwd.h"

/*
 * this expands all escapes
 */

void escape_password (struct _options *opt,
		      char *src_ptr,
		      char *dst_ptr)
  {
    int n1, n2;		/* begin, end position numbers */
    int sgn;		/* sign of escape */
    int num;		/* format field */
    struct intvar *ip;	/* points to variable storage */
/*
 * do the required interpolations
 */
    while (*src_ptr)
      {
/*
 * If this is an escape code then process the escape code.
 */
	if (*src_ptr == '\\')
	  {
	    if (src_ptr[1] == '%'  || src_ptr[1] == '#' ||
		src_ptr[1] == '\\' || src_ptr[1] == '\t')
	      {
		++src_ptr;
		*dst_ptr++ = *src_ptr++;
		continue;
	      }
	  }
/*
 * an unescaped tab ends the test
 * and begins the error message
 */
	if (*src_ptr == '\t')
	  {
	    do
	      ++src_ptr;
	    while(isspace(*src_ptr));
	    
	    if (*src_ptr != '\0')
	      opt->sucmsg = src_ptr;
	    break; /* This is always the end of the rule. */
	  }
/*
 * Handle all of the non-escape characters
 */
	if (*src_ptr != '%')
	  {
	    *dst_ptr++ = *src_ptr++;
	    continue;
	  }
/*
 * get the sign if any
 */
	++src_ptr;
	if (*src_ptr == '-')
	  {
	    ++src_ptr;
	    sgn = 1;
	  }
	else
	  {
	    sgn = 0;
	    if (*src_ptr == '+')
	      ++src_ptr;
	  }
/*
 * Fetch the first number sequence
 */
	if (isdigit ((int) (*src_ptr)))
	  {
	    n1 = 0;
	    n2 = (int) strtol (src_ptr, &src_ptr, 10);
	    if (*src_ptr == '.')
	      {
		n1 = n2;
		n2 = (int) strtol (++src_ptr, &src_ptr, 10);
	      }
	  }
	else
	  {
	    n1 = 0;
	    n2 = BUFSIZ;
	  }
/*
 * Look for the number sign code
 */
	switch((int) *src_ptr)
	  {
	case '^':		/* all upper case */
	    num = F_UPPER;
	    src_ptr++;
	    break;

	case '|':		/* first letter cap */
	    num = F_FIRST;
	    src_ptr++;
	    break;

	case '*':		/* all lower case */
	    num = F_LOWER;
	    src_ptr++;
	    break;

	case '#':		/* associated number */
	    num = F_NUMBER;
	    src_ptr++;
	    break;

	default:		/* as is */
	    num = F_ASIS;
	    break;
	  }
/*
 * Fetch the field designator
 */
	ip = findiv ((int) (*src_ptr++));
	if (ip == (struct intvar *) 0)
	  {
	    continue;
	  }

	switch (ip->nstype)
	  {
	case TY_STR:		/* string variable */
	    dst_ptr = sfmt (dst_ptr, sgn, n1, n2, num,
			    IVSTRING(opt,ip), IVLENGTH(opt,ip));
	    break;
	      
	case TY_NUM:		/* numeric variable */
	    dst_ptr = nfmt (dst_ptr, sgn,
			    IVSTRING(opt,ip), IVLENGTH(opt,ip));
	    break;
	      
	default:
	    break;
	  }
      }
/*
 * close the buffer
 */
    *dst_ptr = '\0';
  }

/*
 * verify the password is okay
 */

int verify_password (struct _options *opt)
{
    char buf[BUFSIZ];		/* input buffer */
    char passbuf[2*BUFSIZ];	/* buffer ater escape translation */
    FILE *fp;			/* test file pointer */
    int  answer;
    int  indx;
/*
 * Define the logging levels
 */
    opt->log_default = LG_DEFAULT;
    if (opt->ctrl & PAM_ST_DEBUG)
        opt->log_default |= LG_DEBUG;
/*
 * set up the password and hostname
 */
    initpw   (opt);
    beginlog (opt, "");
    logfunc  (opt, LG_DEBUG,
	      "pam_passwd+_passwd: \"%s\",\"%s\"",
	      IVSTRING (opt, findiv('p')),
	      IVLENGTH (opt, findiv('p')));
/*
 * open the file
 */
    opt->linect = 0;
    fp          = fopen(opt->pwtest, "r");

    if (fp == NULL) {
        cleanup_load(opt);
	do_converse (opt, 1, "Unable to access password validation file");
	return (0);
    }
/*
 * now grab lines, replacing % and # as required
 */
    answer = 1;
    while (mgets (opt, buf, BUFSIZ, fp) >= 0) {
/*
 * Skip comments
 */
	++opt->linect;
	if (buf[0] == '#' || buf[0] == '\0')
	    continue;
/*
 * look for various special things
 */
	if (strncmp((char *) buf, "GECOS:", 6) == 0 ||
	    strncmp((char *) buf, "SETGECOS:", 9) == 0) {
	    if (loadgecos(opt, &buf[6]) < 0) {
	        cleanup_load(opt);
		(void) fclose(fp);
		return (0);
	    }
	    continue;
	}

	if (strncmp((char *) buf, "SIGCHARS:", 9) == 0) {
	    loadsig(opt, &buf[9]);
	    continue;
	}

	if (strncmp((char *) buf, "SETVAR:", 7) == 0) {
	    escape_password(opt, &buf[7], passbuf);
	    setvar(opt, passbuf);
	    continue;
	}

	if (strncmp((char *) buf, "LOGLEVEL:", 9) == 0) {
	    loadlevel(opt, &buf[9]);
	    continue;
	}

	if (strncmp((char *) buf, "FORCEGECOS:", 11) == 0) {
	    continue;
	}
/*
 * now for the escapes
 * go down the tests and interpolate all the escapes
 */
	opt->sucmsg = "The password is not satisfactory.";
	escape_password (opt, buf, passbuf);

	if (opt->ctrl & PAM_ST_DEBUG) {
	    _pam_log_error ("rule: %s", buf);
	    _pam_log_error ("to parser: %s", passbuf);
	}
/*
 * It failed. Return the error message text.
 */
	if (! passtest(opt, passbuf)) {
	    do_converse (opt, 1, (const char *) opt->sucmsg);
	    answer = 0;
	    break;
	}
    }
/*
 * Erase the pending allocated strings
 */
    cleanup_load(opt);
/*
 * Ensure that all logging files are closed
 */
    for (indx = 0; indx < MAXLOGTO; ++indx) {
        if (opt->logto[indx].flags != 0)
	    endlogging (opt, indx);
    }
/*
 * End of the processing.
 */
    (void) fclose(fp);
    return (answer);
  }
