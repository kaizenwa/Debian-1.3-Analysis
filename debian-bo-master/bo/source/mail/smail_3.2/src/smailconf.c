/*
#ident	"@(#)smail/src:RELEASE-3_2:smailconf.c,v 1.28 1996/05/29 14:50:24 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * smailconf.c:
 *	loading and processing for the configuration files
 *
 *	external functions: read_config_file, read_standard_file,
 *			    fill_attributes, find_attribute,
 *			    add_config_stat, is_newconf, make_lib_fn
 */
#ifdef	STANDALONE
# ifdef	scs
#  define void int
# endif	/* scs */
#endif	/* STANDALONE */
#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "defs.h"
#include "smail.h"
#include "smailconf.h"
#include "parse.h"
#include "main.h"
#include "exitcodes.h"
#include "dys.h"
#ifndef DEPEND
# include "extern.h"
# include "debug.h"
#endif

/* These 2 values are used in handling int values as suffix multipliers */
#define K_MULTIPLIER	1024
#define M_MULTIPLIER	(1024*1024)

/* functions local to this file */
static void print_config_help();
static void print_config_all();

/* types local to this file */
/*
 * config_stat_list is used by add_config_stat() and is_newconf() to
 * determine if any configuration files have changed.  Optional config
 * files which do not exist have a 0 stored in the mtime field.
 */
struct config_stat {
    struct config_stat *succ;
    char    *fn;			/* name of config file */
    time_t  mtime;			/* time of last mod */
    dev_t   dev;			/* device file resides on */
    ino_t   ino;			/* inode number for file */
} *config_stat_list = NULL;

/* define the attributes which may be in the config file */
static struct attr_table conf_attributes[] = {
    { "auth_domains", t_string, NULL, (tup *)&auth_domains, 0 },
    { "auth_domain", t_string, NULL, (tup *)&auth_domains, 0 },
    { "auto_mkdir", t_boolean, NULL, (tup *)&auto_mkdir, 0 },
    { "auto_mkdir_mode", t_mode, NULL, (tup *)&auto_mkdir_mode, 0 },
    { "config_file", t_string, NULL, (tup *)&config_file, 0 },
    { "console", t_string, NULL, (tup *)&cons_fn, 0 },
    { "copying_file", t_string, NULL, (tup *)&copying_file, 0 },
    { "date_field", t_string, NULL, (tup *)&date_field, 0 },
    { "delivery_grades", t_string, NULL, (tup *)&delivery_grades, 0 },
    { "delivery_mode", t_string, NULL, (tup *)&delivery_mode_string, 0 },
    { "director_file", t_string, NULL, (tup *)&director_file, 0 },
    { "domains", t_string, NULL, (tup *)&visible_domains, 0 },
    { "domain", t_string, NULL, (tup *)&visible_domains, 0 },
    { "visible_domains", t_string, NULL, (tup *)&visible_domains, 0 },
    { "visible_domain", t_string, NULL, (tup *)&visible_domains, 0 },
    { "error_copy_postmaster", t_boolean, NULL,
      (tup *)&error_copy_postmaster, 0 },
    { "flock_mailbox", t_boolean, NULL, (tup *)&flock_mailbox, 0 },
    { "fnlock_interval", t_int, NULL, (tup *)&fnlock_interval, 0 },
    { "fnlock_mode", t_mode, NULL, (tup *)&fnlock_mode, 0 },
    { "fnlock_retries", t_int, NULL, (tup *)&fnlock_retries, 0 },
    { "from_field", t_string, NULL, (tup *)&from_field, 0 },
    { "grades", t_string, NULL, (tup *)&grades, 0 },
    { "hit_table_len", t_int, NULL, (tup *)&hit_table_len, 0 },
    { "host_lock_timeout", t_interval, NULL, (tup *)&host_lock_timeout, 0 },
    { "hostnames", t_string, NULL, (tup *)&hostnames, 0 },
    { "hostname", t_string, NULL, (tup *)&hostnames, 0 },
    { "listen_name", t_string, NULL, (tup *)&listen_name, 0 },
    { "lock_by_name", t_boolean, NULL, (tup *)&lock_by_name, 0 },
    { "lock_mode", t_mode, NULL, (tup *)&lock_mode, 0 },
    { "log_mode", t_mode, NULL, (tup *)&log_mode, 0 },
    { "logfile", t_string, NULL, (tup *)&log_fn, 0 },
    { "max_hop_count", t_int, NULL, (tup *)&max_hop_count, 0 },
    { "max_load_ave", t_double, NULL, (tup *)&max_load_ave, 0 },
    { "max_message_size", t_long, NULL, (tup *)&max_message_size, 0 },
    { "message_buf_size", t_int, NULL, (tup *)&message_bufsiz, 0 },
    { "message_id_field", t_string, NULL, (tup *)&message_id_field, 0 },
    { "message_log_mode", t_mode, NULL, (tup *)&message_log_mode, 0 },
    { "method_dir", t_string, NULL, (tup *)&method_dir, 0 },
    { "more_hostnames", t_string, NULL, (tup *)&more_hostnames, 0 },
    { "gateway_names", t_string, NULL, (tup *)&more_hostnames, 0 },
    { "nobody", t_string, NULL, (tup *)&nobody, 0 },
    { "open_interval", t_int, NULL, (tup *)&open_interval, 0 },
    { "open_retries", t_int, NULL, (tup *)&open_retries, 0 },
    { "paniclog", t_string, NULL, (tup *)&panic_fn, 0 },
    { "postmaster_address", t_string, NULL, (tup *)&postmaster_address, 0 },
    { "postmaster", t_string, NULL, (tup *)&postmaster_address, 0 },
    { "primary_name", t_string, NULL, (tup *)&primary_name, 0 },
    { "qualify_file", t_string, NULL, (tup *)&qualify_file, 0 },
    { "queue_only", t_boolean, NULL, (tup *)&queue_only, 0 },
    { "received_field", t_string, NULL, (tup *)&received_field, 0 },
    { "resolve_timeout", t_interval, NULL, (tup *)&resolve_timeout, 0 },
    { "require_configs", t_boolean, NULL, (tup *)&require_configs, 0 },
    { "retry_duration", t_interval, NULL, (tup *)&retry_duration, 0 },
    { "retry_file", t_string, NULL, (tup *)&retry_file, 0 },
    { "retry_interval", t_interval, NULL, (tup *)&retry_interval, 0 },
    { "return_path_field", t_string, NULL, (tup *)&return_path_field, 0 },
    { "router_file", t_string, NULL, (tup *)&router_file, 0 },
    { "runq_grades", t_string, NULL, (tup *)&runq_grades, 0 },
    { "second_config_file", t_string, NULL, (tup *)&second_config_file, 0 },
    { "sender_env_variable", t_string, NULL, (tup *)&sender_env_variable, 0 },
    { "smail", t_string, NULL, (tup *)&smail, 0 },
    { "smail_lib_dir", t_string, NULL, (tup *)&smail_lib_dir, 0 },
    { "smail_util_dir", t_string, NULL, (tup *)&smail_util_dir, 0 },
    { "smart_path", t_string, NULL, (tup *)&smart_path, 0 },
    { "smart_transport", t_string, NULL, (tup *)&smart_transport, 0 },
    { "smart_user", t_string, NULL, (tup *)&smart_user, 0 },
    { "smtp_accept_max", t_int, NULL, (tup *)&smtp_accept_max, 0 },
    { "smtp_accept_queue", t_int, NULL, (tup *)&smtp_accept_queue, 0 },
    { "smtp_banner", t_string, NULL, (tup *)&smtp_banner, 0 },
    { "smtp_debug", t_boolean, NULL, (tup *)&smtp_debug, 0 },
    { "smtp_info", t_boolean, NULL, (tup *)&smtp_info, 0 },
    { "smtp_receive_command_timeout", t_interval, NULL,
      (tup *)&smtp_receive_command_timeout, 0 },
    { "smtp_receive_message_timeout", t_interval, NULL,
      (tup *)&smtp_receive_message_timeout, 0 },
    { "spool_dirs", t_string, NULL, (tup *)&spool_dirs, 0 },
    { "spool_dir", t_string, NULL, (tup *)&spool_dir, 0 },
    { "spool_grade", t_char, NULL, (tup *)&spool_grade, 0 },
    { "spool_mode", t_mode, NULL, (tup *)&spool_mode, 0 },
    { "switch_percent_and_bang", t_boolean, NULL,
      (tup *)&switch_percent_and_bang, 0 },
    { "rfc1413_query_timeout", t_interval, NULL, (tup *)&rfc1413_query_timeout, 0 },
    { "transport_file", t_string, NULL, (tup *)&transport_file, 0 },
    { "trusted", t_string, NULL, (tup *)&trusted, 0 },
    { "trusted_users", t_string, NULL, (tup *)&trusted, 0 },
    { "trusted_user", t_string, NULL, (tup *)&trusted, 0 },
    { "trusted_groups", t_string, NULL, (tup *)&trusted_groups, 0 },
    { "trusted_group", t_string, NULL, (tup *)&trusted_groups, 0 },
    { "uucp_name", t_string, NULL, (tup *)&uucp_name, 0 },
    { "version", t_infoproc, NULL, (tup *)version, 0 },
    { "visible_name", t_string, NULL, (tup *)&visible_name, 0 },
};
struct attr_table *end_conf_attributes = ENDTABLE(conf_attributes);

/* Flag values used by format_attribute */
#define FA_USE_PREFIX	1
#define FA_USE_VALUE	2
#define FA_IS_COMMENT	4



/*
 * read_config_file - read a config file and fill in conf_attributes
 *
 * return an error message or NULL (if no error).
 */
char *
read_config_file(fn)
    char *fn;				/* name of a config file */
{
    char *entry;			/* entry found by read_entry */
    FILE *f;				/* input file */
    struct stat statbuf;

    if (fn == NULL || EQ(fn, "-")) {
	/* a name of `-' turns off use of the external file */
	return NULL;
    }

    f = fopen(fn, "r");
    if (f == NULL) {
	if (require_configs) {
	    return xprintf("%s: %s", fn, strerror(errno));
	}

	add_config_stat(fn, (struct stat *)NULL);
	return NULL;
    }

    (void)fstat(fileno(f), &statbuf);
    add_config_stat(fn, &statbuf);

    /*
     * read all of the entries in the config file
     */
    while (entry = read_entry(f)) {
	char *error;
	struct attr_table *conf_attr;
	struct attribute *attr = parse_config(entry, &error);

	if (attr == NULL) {
	    return xprintf("%s: parse error: %s", fn, error);
	}
	conf_attr = find_attribute(attr->name, conf_attributes,
				   end_conf_attributes);
	if (conf_attr == NULL) {
	    return xprintf("%s: unknown attribute: %s",
			   fn, attr->name);
	}
	conf_attr->value = attr->value;

	if (conf_attr->type == t_boolean) {
	    /* make sure boolean types are given as booleans */
	    if (attr->value != on && attr->value != off) {
		return xprintf("%s: boolean attribute %s has non-boolean form",
			       fn, attr->name);
	    }
	} else {
	    /* make sure non-boolean types aren't given as booleans */
	    if ((attr->value == on &&
		 conf_attr->type != t_proc) ||
		(attr->value == off &&
		 conf_attr->type != t_string &&
		 conf_attr->type != t_int &&
		 conf_attr->type != t_mode &&
		 conf_attr->type != t_long &&
		 conf_attr->type != t_interval &&
		 conf_attr->type != t_double &&
		 conf_attr->type != t_proc))
	    {
		return xprintf("%s: non-boolean attribute %s has boolean form",
			       fn, attr->name);
	    }
	}

	switch (conf_attr->type) {
	case t_string:
	    if (attr->value == off) {
		conf_attr->uptr->v_string = NULL;
	    } else {
		conf_attr->uptr->v_string = attr->value;
	    }
	    break;

	case t_boolean:
	    conf_attr->uptr->v_boolean =
		(int)c_atol(attr->value, &error);
	    break;

	case t_char:
	    if (strlen(attr->value) != 1) {
		return xprintf("%s: bad character constant for attribute %s",
			       fn, attr->name);
	    }
	    conf_attr->uptr->v_char = attr->value[0];
	    break;

	case t_int:
	case t_mode:
	    error = NULL;
	    conf_attr->uptr->v_int = (int)c_atol(attr->value, &error);

	    if (error) {
		return xprintf("%s: attribute %s: %s",
			       fn, attr->name, error);
	    }
	    break;

	case t_long:
	    error = NULL;
	    conf_attr->uptr->v_long = c_atol(attr->value, &error);

	    if (error) {
		return xprintf("%s: attribute %s: %s",
			       fn, attr->name, error);
	    }
	    break;

	case t_interval:
	    {
		long val;

		if ((val = ivaltol(attr->value)) < 0) {
		    return xprintf(
			       "%s: attribute %s: malformed interval value %s",
				   fn, attr->name, attr->value);
		}
		conf_attr->uptr->v_long = val;
	    }
	    break;

	case t_double:
	    {
		double val;
		char c;			/* catch sscanf spill over */

		/* should be exactly one item set by sscanf */
		if (sscanf(attr->value, "%lf%c", &val, &c) != 1) {
		    return xprintf(
			      "%s: attribute %s: malformed floating number %s",
				   fn, attr->name, attr->value);
		}
		conf_attr->uptr->v_double = val;
	    }
	    break;

	case t_proc:
	    if (error = (*((char *(*)())conf_attr->uptr))(attr))
	    {
		return xprintf("%s: %s", fn, error);
	    }
	    break;

	case t_infoproc:
	    return xprintf("%s: %s: read-only attribute", fn, attr->name);

	default:
	    return xprintf("%s: %s: unknown attribute type", fn, attr->name);
	}
    }

    return NULL;
}



char *
quote_string_value(str)
     char * str;
{
    static struct str val;
    static int initialised = FALSE;
    char s;
    char buf[4];

    if (!initialised) {
	STR_INIT(&val);
	initialised++;
    }
    
    val.i = 0;			/* zero string length */

    STR_NEXT(&val, '\"');
    while (s = *str++) {
	switch (s) {
	  case '\t':
	    STR_NEXT(&val, '\\');
	    STR_NEXT(&val, 't');
	    break;
	  case '\r':
	    STR_NEXT(&val, '\\');
	    STR_NEXT(&val, 'r');
	    break;
	  case '\n':
	    STR_NEXT(&val, '\\');
	    STR_NEXT(&val, 'n');
	    break;
	  case '\\':
	    STR_NEXT(&val, '\\');
	    STR_NEXT(&val, '\\');
	    break;
	  case '\"':
	    STR_NEXT(&val, '\\');
	    STR_NEXT(&val, '\"');
	    break;
	  case ' ':
	    STR_NEXT(&val, s);
	    break;
	  default:
	    if (iscntrl(s)) {
		sprintf(buf, "\\%03o", (int) s);
		STR_CAT(&val, buf);
	    } else {
		STR_NEXT(&val, s);
	    }
	}
    }
    STR_CAT(&val, "\"");	/* STR_CAT used here since it terminates the string */
    return(val.p);
}



/*
 * format_attribute - given a attribute, return a formatted value
 *
 * Parameters:
 *	conf_attr	- pointer to attribute entry
 *	name		- name of entry (for errors)
 *	prefix		- (returned) prefix, ie +/- for boolean
 *			  or # for infoproc types
 *	value		- (returned) value, maybe NULL
 *	ptr		- ptr to config struct
 *
 * Data may either be directly pointed to by the attribute struct, as in
 * global config variables, or may be an offset into a struct in which case
 * ptr must be set.
 *
 * Return value is bit mask which items are valid
 */

int
format_attribute(conf_attr, name, quoted, prefix, value, ptr, flags)
     struct attr_table * conf_attr;	/* The attriute info */
     char * name;			/* The conf variable name */
     int  quoted;			/* whether returned value should be quoted */
     char * * prefix;			/* Any prefix to be added to name */
     char * * value;			/* The value of the variable */
     char * ptr;			/* Pointer to anonymous struct */
     long flags;			/* flags entry from config */
{
    static struct str val;
    static int initialised = FALSE;
    tup * vptr;				/* pointer to attr value */
    int ret;				/* return mask */
    long lval;				/* int/long conversion var */

    /* NB This routine depends on the minimum length of val (which should
     * be STR_BUMP) being reasonable - ie greater than 20, and not being
     * shrunk.  If someone tampers behind the scenes expect the whole lot
     * to fall down :-(
     */
    if (!initialised) {
	STR_INIT(&val);
	initialised++;
    }

    /* Initialise default values */
    *prefix = NULL;
    *value = NULL;
    val.i = 0;				/* zero string length */
    ret = 0;

    if (conf_attr == NULL) {
	if (errfile)
	    (void) fprintf(errfile, "%s: unknown attribute: %s\n", program, name);
	return ret;
    }

    if (ptr == NULL) {
	vptr = conf_attr->uptr;
    } else {
	vptr = (tup *) ((char *) ptr + conf_attr->offset);
    }

    switch (conf_attr->type) {
      case t_string:
	{
	    if (vptr->v_string) {
		*value = quoted ? quote_string_value(vptr->v_string) : vptr->v_string;
		ret = FA_USE_VALUE;
	    } else {
		*prefix = "-";
		ret = FA_USE_PREFIX;
	    }
	}
	break;

      case t_boolean:
	{
	    int v2;
	    /* Booleans are handled a bit differently in the drivers */
	    if (conf_attr->offset != 0) {
		/* If offset is non-zero, then use it as a mask on flags */
		v2 = flags & conf_attr->offset;
	    } else {
		v2 = vptr->v_boolean;
	    }

	    if (v2) {
		*value = "true";
		*prefix = "+";
	    } else {
		*value = "false";
		*prefix = "-";
	    }
	    ret = FA_USE_PREFIX;
	}
	break;

      case t_char:
	STR_NEXT(&val, vptr->v_char);
	STR_NEXT(&val, 0);
	*value = val.p;
	ret = FA_USE_VALUE;
	break;

      case t_int:
      case t_long:
	if (conf_attr->type == t_int) {
	    lval = vptr->v_int;
	} else {
	    lval = vptr->v_long;
	}

	if (lval == 0) {
	    *prefix = "-";
	    ret = FA_USE_PREFIX;
	} else {
	    if ((lval >= M_MULTIPLIER) && !(lval % M_MULTIPLIER)) {
		(void) sprintf(val.p, "%ldM", (lval / M_MULTIPLIER));
	    } else if ((lval >= K_MULTIPLIER) && !(lval % K_MULTIPLIER)) {
		(void) sprintf(val.p, "%ldK", (lval / K_MULTIPLIER));
	    } else {
		(void) sprintf(val.p, "%ld", vptr->v_int);
	    }
	    *value = val.p;
	    ret = FA_USE_VALUE;
	}
	break;

      case t_mode:
	if (vptr->v_int == 0) {
	    *prefix = "-";
	    ret = FA_USE_PREFIX;
	} else {
	    (void) sprintf(val.p, "%#o", vptr->v_int);
	    *value = val.p;
	    ret = FA_USE_VALUE;
	}
	break;

      case t_interval:
	if (vptr->v_long == 0) {
	    *prefix = "-";
	    ret = FA_USE_PREFIX;
	} else {
	    if ((*value = ltoival(vptr->v_long)) == NULL) {
		sprintf(val.p, "%ld", vptr->v_long);
		*value = val.p;
	    }
	    ret = FA_USE_VALUE;
	}
	break;

      case t_double:
	(void) sprintf(val.p, "%g", vptr->v_double);
	*value = val.p;
	ret = FA_USE_VALUE;
	break;

      case t_proc:
	if (errfile) {
	    (void) fprintf(errfile, "%s: %s: cannot print proc attributes\n",
			   program, name);
	}
	break;

      case t_infoproc:
	*value = quote_string_value((*((char *(*)())vptr))(name));
	*prefix = "#";
	ret = FA_USE_VALUE|FA_USE_PREFIX|FA_IS_COMMENT;
	break;

      default:
	if (errfile) {
	    (void) fprintf(errfile, "%s: %s: unknown type\n");
	}
	break;
    }
    return(ret);

}



/*
 * print_config_variable - write the value for a config variable on stdout
 */
void
print_config_variable(name)
    char *name;
{
    struct attr_table *conf_attr;
    char *prefix;
    char *value;
    int ret;

    if (EQ(name, "help")) {
	print_config_help();
	return;
    }
    if (EQ(name, "all") || EQ(name, "CONFIG")) {
	print_config_all();
	return;
    }
    if (EQ(name, "DIRECTORS")) {
	dump_director_config(stdout);
	return;
    }
    if (EQ(name, "ROUTERS")) {
	dump_router_config(stdout);
	return;
    }
    if (EQ(name, "TRANSPORTS")) {
	dump_transport_config(stdout);
	return;
    }
    if (EQ(name, "QUALIFY")) {
	dump_qualify_config(stdout);
	return;
    }
    if (EQ(name, "RETRY")) {
	dump_retry_config(stdout);
	return;
    }
    if (EQ(name, "ALL")) {
	fputs("#\n# -- config file\n#\n", stdout);
	print_config_all();
	fputs("#\n# -- end of config\n#\n", stdout);
	dump_director_config(stdout);
	dump_router_config(stdout);
	dump_transport_config(stdout);
	dump_qualify_config(stdout);
	dump_retry_config(stdout);
	return;
    }

    conf_attr = find_attribute(name, conf_attributes, end_conf_attributes);
    ret = format_attribute(conf_attr, name, debug, &prefix, &value, (char *) NULL, 0L);

    if (ret) {
	if (debug) {
	    printf("%s%s%s%s\n",
		   ((ret & FA_USE_PREFIX) ? prefix : ""),
		   name,
		   ((ret & FA_USE_VALUE) ? "=" : ""),
		   ((ret & FA_USE_VALUE) ? value : ""));
	} else {
	    (void) printf("%s\n", value ? value : "(null)");
	}
    }
}




/*
 * print_config_help - output a listing of all config attributes, with type
 */
static void
print_config_help()
{
    register struct attr_table *t;
    register tup *last_uptr = NULL;

    for (t = conf_attributes; t < end_conf_attributes; t++) {
	register char *typename;

	if (t->uptr == last_uptr) {
	    continue;
	}
	last_uptr = t->uptr;
	switch(t->type) {
	case t_string:  typename = "string";  break;
	case t_boolean: typename = "boolean"; break;
	case t_char:    typename = "char";    break;
	case t_int:     typename = "int";     break;
	case t_mode:    typename = "mode";    break;
	case t_long:    typename = "long";    break;
	case t_interval:typename = "interval";break;
	case t_double:  typename = "double";  break;
	case t_proc:    typename = "special"; break;
	case t_infoproc:typename = "info";    break;
	default:        typename = "unkown type";  break;
	}
	(void) printf("%s=%s\n", t->name, typename);
    }
}

/*
 * print_config_all - print all config file variables
 */
static void
print_config_all()
{
    register struct attr_table *t;
    register tup *last_uptr = NULL;
    int save_debug = debug;

    debug = 1;
    for (t = conf_attributes; t < end_conf_attributes; t++) {
	if (t->uptr == last_uptr) {
	    continue;
	}
	last_uptr = t->uptr;
	switch(t->type) {
	case t_string:
	case t_boolean:
	case t_char:
	case t_int:
	case t_mode:
	case t_long:
	case t_interval:
	case t_double:
	case t_infoproc:
	    print_config_variable(t->name);
	    break;
	default:
	    (void) printf("%s: unknown type\n", t->name);
	    break;
	}
    }
    debug = save_debug;
}


/*
 * read_standard_file - read (for example) a transport, director or router file
 *
 * given a template default structure, some information about that structure,
 * and an attr_table describing attributes, read a file of standard
 * attribute descriptions and return a list describing all of the entries
 * in that file.
 *
 * inputs:
 *	f	      - open input file.  File format must conform to what is
 *			expected by read_entry and parse_entry in parse.c.
 *	template      - pointer to the template structure.
 *	struct_size   - size of template structure.
 *	name_offset   - offset to the name element in the template structure.
 *	flags_offset  - offset to flags element of structure.
 *	succ_offset   - offset to succ element of structure.
 *	attr_table    - table defining generic attributes.
 *	end_attr_table- end of the attribute table
 *
 *	driv_function - function to handle driver attributes for each entry.
 *			Also, this function can check the validity of the
 *			generic attributes in the passed structure.  The
 *			function should return NULL, or an error message.
 *			It is called as:
 *
 *			    char *
 *			    f(s, d)
 *				char *s;   --- structure being defined
 *				struct attribute *d;  --- driver attributes
 *
 *			If driv_function is NULL, no driver attributes are
 *			allowed and no function is called.
 *	head	      - pointer to a variable in which to store the head
 *			of the generated structure list.
 *
 * output:
 *	If an error occured, an error message will be returned.
 *	Otherwise NULL will be returned and a pointer to the new
 *	structure list will be stored at the location pointed to by
 *	`head'.
 */
char *
read_standard_file(f, template, struct_size,
		   name_offset, flags_offset, succ_offset,
		   attr_table, end_attr_table,
		   driv_function, head)
    FILE *f;				/* input file */
    register char *template;		/* template structure */
    int struct_size;			/* size of template structure */
    int name_offset;			/* name field offset in structure */
    int flags_offset;			/* where to store boolean flags */
    int succ_offset;			/* where to store succ pointer */
    struct attr_table *attr_table;	/* start of attribute table */
    struct attr_table *end_attr_table;	/* e.g., ENDTABLE(attr_table) */
    char *(*driv_function)();		/* function to handle driver attrs */
    char **head;			/* output structure list */
{
    char **link_ptr;			/* pointer to last succ element */
    char *cur;				/* current structure to define */
    char *s;				/* text for current entry */

    DEBUG(DBG_CONF_MID, "read_standard_file called\n");
    /* start out with no structure list entries */
    link_ptr = head;
    *link_ptr = NULL;

    /*
     * scan through reading entries.  For each entry allocate a
     * structure and fill it in.  Also, link the new structure to the
     * previous structure.
     */
    while (s = read_entry(f)) {
	char *error;
	struct attribute *generic_attrs; /* generic attributes for entry */
	struct attribute *driver_attrs;	/* driver attributes for entry */
	char *name;			/* name of entry */

	/* create the new structure and forward link to it */
	*link_ptr = cur = xmalloc(struct_size);
	link_ptr = (char **)(cur + succ_offset);

	/* fill in the entry structure, starting with the template */
	(void) memcpy(cur, template, (size_t) struct_size);

	/* get all of the attributes */
	name = parse_entry(s, &generic_attrs, &driver_attrs, &error);
	if (name == NULL) {
	    return error;
	}

	/* fill in the name */
	*(char **)(cur + name_offset) = name;

	/*
	 * fill in the generic attributes, calling attr_function for
	 * attributes not in attr_table
	 */
	error = fill_attributes(cur, generic_attrs,
				(long *)(cur + flags_offset),
				attr_table, end_attr_table);
	if (error) {
	    return error;
	}
	if (driv_function) {
	    error = (*driv_function)(cur, driver_attrs);
	    if (error) {
		return error;
	    }
	} else if (driver_attrs) {
	    return xprintf("no driver attributes allowed in file");
	}
    }

    /* everything went okay */
    return NULL;
}


/* 
 * dump_standard_config - print out the config used
 *
 * The reverse of read_standard_file - this prints out something
 * like the original config file!
 */
void
dump_standard_config(f, config, name, flags, attr_table, end_attr_table)
    FILE *f;				/* output file */
    register char *config;		/* filled in config */
    char * name;			/* name of driver */
    long flags;				/* boolean flags */
    struct attr_table *attr_table;	/* start of attribute table */
    struct attr_table *end_attr_table;	/* e.g., ENDTABLE(attr_table) */
{
    int ret;				/* return mask from format_attribue */
    struct attr_table *attr;		/* attribute entry */
    char * prefix;			/* returned prefix */
    char * value;			/* returned value */

    for (attr = attr_table; (attr < end_attr_table); attr++) {
	if (attr->type != t_proc) {
	    ret = format_attribute(attr, name, 1, &prefix, &value, config, flags);
	    if (ret) {
		fprintf(f, "%s%s%s%s%s%s,\n",
			((ret & FA_IS_COMMENT) ? "" : "\t"),
			((ret & FA_USE_PREFIX) ? prefix : ""),
			((ret & FA_IS_COMMENT) ? "\t" : ""),
			attr->name,
			((ret & FA_USE_VALUE) ? "=" : ""),
			((ret & FA_USE_VALUE) ? value : ""));
	    }
	}
    }
}



/*
 * fill_attributes - build a structure and fill in attributes
 *
 * Given a list of attributes extracted from one of the configuration
 * files and an array of known attributes, fill in a stucture to which
 * those attributes pertain.
 *
 * inputs:
 *	sp	      - pointer to stucture that is to be filled in
 *	attrs	      - list of given attributes
 *	flags	      - pointer to long where boolean flags should be OR'd in
 *	attr_table    - table describing known attributes
 *	end_attr_table- end of known attribies (i.e., ENDTABLE(attr_table))
 *
 * output:
 *	returns either an error message, or NULL if everything went okay.
 */
char *
fill_attributes(sp, attrs, flags, attr_table, end_attr_table)
    char *sp;
    struct attribute *attrs;
    long *flags;
    struct attr_table *attr_table;
    struct attr_table *end_attr_table;
{
    char *error;

    /* step through all of the attributes from the file entry */
    for (; attrs; attrs = attrs->succ) {
	register struct attr_table *t;	/* table entry */
	register char *value = attrs->value;

	/* find the attribute in the list of known attributes */
	t = find_attribute(attrs->name, attr_table, end_attr_table);
	if (t == NULL) {
	    /* didn't find the attribute */
	    return xprintf("%s: unknown attribute", attrs->name);
	}

	/* what do with this attribute? */
	if (t->type == t_boolean) {
	    /* make sure boolean types are given as booleans */
	    if (attrs->value != on && attrs->value != off) {
		return xprintf("boolean attribute %s has non-boolean form",
			       attrs->name);
	    }
	} else {
	    if ((attrs->value == on &&
		 t->type != t_proc) ||
		(attrs->value == off &&
		 t->type != t_string &&
		 t->type != t_int &&
		 t->type != t_mode &&
		 t->type != t_long &&
		 t->type != t_interval &&
		 t->type != t_double &&
		 t->type != t_proc))
	    {
		return xprintf("non-boolean attribute %s has boolean form",
			       attrs->name);
	    }
	}

	/* fill in the attribute */
	switch (t->type) {
	case t_string:
	    if (value == off) {
		*(char **)(sp + t->offset) = NULL;
	    } else {
		*(char **)(sp + t->offset) = value;
	    }
	    break;

	case t_boolean:
	    if (value == on) {
		*flags |= t->offset;
	    } else {
		*flags &= ~t->offset;
	    }
	    break;

	case t_char:
	    if (strlen(attrs->value) != 1) {
		return xprintf("bad character constant for attribute %s",
			       attrs->name);
	    }
	    *(sp + t->offset) = value[0];
	    break;

	case t_int:
	case t_mode:
	    error = NULL;
	    *(int *)(sp + t->offset) = (int)c_atol(value, &error);

	    if (error) {
		return xprintf("attribute %s: %s", attrs->name, error);
	    }
	    break;

	case t_long:
	    error = NULL;
	    *(long *)(sp + t->offset) = c_atol(value, &error);

	    if (error) {
		return xprintf("attribute %s: %s", attrs->name, error);
	    }
	    break;

	case t_interval:
	    {
		long val;

		if ((val = ivaltol(value)) < 0) {
		    return xprintf("attribute %s: malformed interval value %s",
				   attrs->name, value);
		}
		*(long *)(sp + t->offset) = val;
	    }
	    break;	/* FIXME: ???? was fallthru */

	case t_double:
	    {
		double val;
		char c;			/* catch sscanf spill over */

		/* should be exactly one item set by sscanf */
		if (sscanf(value, "%lf%c", &val, &c) != 1) {
		    return xprintf(
				  "attribute %s: malformed floating number %s",
				   attrs->name, value);
		}
		*(double *)(sp + t->offset) = val;
	    }
	    break;

	case t_proc:
	    if ((error = (*((char *(*)())t->uptr))(sp, attrs)))
	    {
		return error;
	    }
	}
    }

    return NULL;
}

/*
 * find_attribute - find an attribute within an attribute table
 */
struct attr_table *
find_attribute(name, table, end)
    register char *name;		/* name to search for */
    register struct attr_table *table;	/* table to search in */
    register struct attr_table *end;	/* end of table */
{
    while (table < end) {
	if (EQ(name, table->name)) {
	    return table;
	}
	table++;
    }
    return NULL;
}


/*
 * add_config_stat - add a new config file to config_stat_list
 *
 * Pass a filename plus a pointer to a stat structure or NULL.
 */
void
add_config_stat(fn, statp)
    char *fn;				/* name of file */
    struct stat *statp;			/* stat of the file, or NULL */
{
    struct config_stat *new = (struct config_stat *)xmalloc(sizeof(*new));

    DEBUG2(DBG_CONF_HI, "add_config_stat(%s, %s) called\n", fn, statp ? "FOUND" : "MISSING");
    new->fn = COPY_STRING(fn);
    if (statp) {
	new->mtime = statp->st_mtime;
	new->dev = statp->st_dev;
	new->ino = statp->st_ino;
    } else {
	new->mtime = (time_t)0;
    }
    new->succ = config_stat_list;
    config_stat_list = new;
}

/*
 * is_newconf - check if we have a new set of configuration files
 *
 * returns TRUE if a configuration fie has changed, FALSE otherwise
 */
int
is_newconf()
{
    struct stat statbuf;
    struct config_stat *csp;

    for (csp = config_stat_list; csp; csp = csp->succ) {
	if (stat(csp->fn, &statbuf) < 0) {
	    if (csp->mtime != (time_t)0) {
		/* file no longer exists */
		return TRUE;
	    }
	} else {
	    if (csp->mtime == (time_t)0) {
		/* file now exists */
		return TRUE;
	    }
	    if (csp->mtime != statbuf.st_mtime ||
		csp->dev != statbuf.st_dev ||
		csp->ino != statbuf.st_ino)
	    {
		/* existing file has changed */
		return TRUE;
	    }
	}
    }

    /* nothing has changed */
    return FALSE;
}

/*
 * make_lib_fn - build a filename relative to the smail lib directory
 *
 * If the filename begins with '/' or is "-" return it unmodified.
 * Return NULL if smail_lib_dir is undefined, or if the given
 *  filename is NULL.
 */
char *
make_lib_fn(fn)
    char *fn;				/* filename to expand */
{
    if (fn == NULL) {
	return NULL;
    }
    if (fn[0] == '/' || (fn[0] == '-' && fn[1] == '\0')) {
	return fn;
    }
    if (smail_lib_dir == NULL) {
	return NULL;
    }
    return xprintf("%s/%s", smail_lib_dir, fn);
}


#ifdef	STANDALONE

struct transport *transports;
struct director *directors;
struct router *routers;

/*
 * find_direct_driver - given a driver's name, return the driver structure
 *
 * return NULL if driver does not exist.
 */
struct direct_driver *
find_direct_driver(name)
    register char *name;		/* search key */
{
#ifdef	notyet
    register struct direct_driver *ddp;	/* pointer to table of drivers */

    for (ddp = direct_drivers; ddp->name; ddp++) {
	if (EQ(ddp->name, name)) {
	    return ddp;			/* found the driver */
	}
    }

    return NULL;			/* driver not found */
#else	/* notyet */
    static struct direct_driver dd = {
	NULL, 1, NULL, NULL, NULL, NULL,
    };

    return &dd;
#endif	/* notyet */
}

/*
 * find_transport_driver - given a driver's name, return the driver structure
 *
 * return NULL if driver does not exist.
 */
struct transport_driver *
find_transport_driver(name)
    register char *name;		/* search key */
{
#ifdef	notyet
    register struct transport_driver *tdp; /* pointer to table of drivers */

    for (tdp = transport_drivers; tdp->name; tdp++) {
	if (EQ(tdp->name, name)) {
	    return tdp;			/* found the driver */
	}
    }

    return NULL;			/* driver not found */
#else	/* notyet */
    static struct transport_driver td = {
	NULL, 1, NULL, NULL, NULL, NULL,
    };
    return &td;
#endif	/* notyet */
}

/*
 * find_route_driver - given a driver's name, return the driver structure
 *
 * return NULL if driver does not exist.
 */
struct route_driver *
find_route_driver(name)
    register char *name;		/* search key */
{
#ifdef	notyet
    register struct route_driver *rdp;	/* pointer to table of drivers */

    for (rdp = route_drivers; rdp->name; rdp++) {
	if (EQ(rdp->name, name)) {
	    return rdp;			/* found the driver */
	}
    }

    return NULL;			/* driver not found */
#else	/* notyet */
    static struct route_driver rd = {
	NULL, 1, NULL, NULL, NULL, NULL,
    };
    return &rd;
#endif	/* notyet */
}

void
main(argc, argv)
    int argc;				/* count of arguments */
    char *argv[];			/* vector of arguments */
{
    char *error;

    if (argc != 2) {
	(void) fprintf(stderr, "Usage: %s config-file", argv[0]);
	/*NOTREACHED*/
    }

    config_file = argv[1];
    if ((error = read_config_file(config_file, CONFIG_PRIMARY)) ||
	(second_config_file &&
	 error = read_config_file(second_config_file, CONFIG_SECONDARY))
	(error = read_transport_file()) ||
	(error = read_router_file()) ||
	(error = read_director_file()) ||
	(error = read_qualify_file()) ||
	(error = read_retry_file()))
    {
	(void) fprintf(stderr, "%s: %s\n", argv[0], error);
    }
}
#endif	/* STANDALONE */
