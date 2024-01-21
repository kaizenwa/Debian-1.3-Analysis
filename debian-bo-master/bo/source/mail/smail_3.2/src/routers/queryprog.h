/* @(#) queryprog.h,v 1.3 1992/07/11 11:50:38 tron Exp */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * queryprog.h:
 *	interface file for router driver in queryprog.c
 */

/* private data structure for uuname driver */
struct queryprogram_private {
    char *cmd;				/* command to display names */
    char *domain;			/* domains to strip from names */
    char *required;			/* required domain names */
    int hash_table_len;			/* hash slots in hash_table */
    struct hash_table *cache;		/* temp - cache of known names */
};

#define QP_READ_PATH	  0x0001000	/* read path from prog output */
#define QP_READ_TRANSPORT 0x0002000	/* read transport from prog output */
