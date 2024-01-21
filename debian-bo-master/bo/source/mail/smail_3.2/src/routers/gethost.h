/* @(#) gethost.h,v 1.4 1992/07/11 11:50:26 tron Exp */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * gethost.h:
 *	interface file for gethostbyname and gethostbyaddr drivers.
 *
 * local_domain_only attribute added by Dan Danz (dan@phoenix.az.stratus.com)
 */

/* flag attributes, only for the gethostbyaddr driver */
#define GETHOST_FAIL_IFERR  0x00010000	/* fail mal-formed domain literals */
#define GETHOST_CHECK_LOCAL 0x00020000	/* check for a match of local host */

/* flag attributes, only for the gethostbyname driver */
#define GETHOST_ONLY_LOCAL  0x00010000	/* must not have a domain */

/* private information stored per router file entry, for gethostbyname only */
struct gethostbyname_private {
    char *domain;			/* optional domain names */
    char *required;			/* required domain names */
};
