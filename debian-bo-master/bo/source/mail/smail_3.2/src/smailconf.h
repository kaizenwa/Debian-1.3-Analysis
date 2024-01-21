/*
#ident	"@(#)smail/src:RELEASE-3_2:smailconf.h,v 1.7 1996/02/26 18:22:20 woods Exp
 */

/*
 *    Copyright (C) 1987, 1988 by Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 *
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */
/*
 * smailconf.h:
 *	interface file for routines in smailconf.c.
 */

/* types used in smailconf.c */
/*
 * attribute tables are arrays of these structures.
 */
struct attr_table {
    char *name;				/* name of attribute */
    /* type of attribute */
    enum {
	t_string,			/* a string attribute */
	t_boolean,			/* boolean, on or off, attribute */
	t_char,				/* single character attribute */
	t_int,				/* integer attribute */
	t_mode,				/* file (permissions) mode - octal int */
	t_long,				/* long integer attribute */
	t_interval,			/* time interval (hour/min/sec) */
	t_double,			/* double precision attribute */
	t_proc,				/* attribute handled by procedure */
	t_infoproc			/* procedure which displays info */
    } type;
    char *value;			/* value to put in config file */
    union u_attr {
	char *v_string;			/* string variable */
	int  v_boolean;			/* boolean variable */
	int  v_char;			/* char variable, accessed as int */
	int  v_int;			/* integer variable */
	long v_long;			/* long variable */
	double v_double;		/* double variable */
	char *(*v_proc)();		/* procedure to handle attribute */
	char *(*v_infoproc)();		/* procedure for info attribute */
    } *uptr;				/* point to configuration variable */
    long offset;			/* offset into data structure */
};

/* convenience typedef for use in initializing conf_form tables */
typedef union u_attr tup;
