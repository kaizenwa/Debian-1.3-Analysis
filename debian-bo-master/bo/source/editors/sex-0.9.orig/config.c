/*
 * File:	config.c
 * Purpose:	Central location for most compile time SeX configurabilities.
 * Author:	Lars Wirzenius
 * Version:	"@(#)SeX:$Id: config.c,v 1.6 1996/12/06 13:15:29 liw Exp $"
 * Description:	This file implements reading and saving configuration files
 *		and reading and setting configurable values.
 */
 

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <publib.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>

#include "error.h"
#include "config.h"


#define CONFIG_FILENAME	"~/.sexrc"



/*
 * All configuration variables.
 */
 
static int save_backup = 0;
static char *fmt_program = "fmt";
static char *font = "fixed";
static long max_killring = 64*1024;
static long indent_width = 8;


/*
 * Interface to Publib's cfg routines.
 */

static struct cfg_variable vars[] = {
	{ "save_backup", CFG_BOOLEAN, &save_backup },
	{ "fmt_program", CFG_STRING, &fmt_program },
	{ "font", CFG_STRING, &font },
	{ "max_killring", CFG_LONG_EXPR, &max_killring },
	{ "indent_width", CFG_LONG_EXPR, &indent_width },
	{ NULL }
};



/*
 * Convert enum config to pointer to variable.  Index is 0..CONFIG_MAX.
 */
static void *enum_to_ptr[CONFIG_MAX];

static void init_enum_to_ptr(void) {
	static int done = 0;

	if (done)
		return;
	done = 1;

	enum_to_ptr[CONFIG_SAVE_BACKUP] = &save_backup;
	enum_to_ptr[CONFIG_FMT_PROGRAM] = &fmt_program;
	enum_to_ptr[CONFIG_FONT] = &font;
	enum_to_ptr[CONFIG_MAX_KILLRING] = &max_killring;
	enum_to_ptr[CONFIG_INDENT_WIDTH] = &indent_width;

#ifndef NDEBUG
	{
		int i;
		for (i = 0; i < CONFIG_MAX; ++i)
			assert(enum_to_ptr[i] != NULL);
	}
#endif
}



/*
 * Prototypes.
 */

static char *config_filename(void);



/*
 * Function:	config_read
 * Purpose:	Read the configuration file.
 * Arguments:	-
 * Return:	-1 for error, 0 for OK.
 */
int config_read(void) {
	char *fullname;
	struct stat st;

	fullname = config_filename();
	if (fullname == NULL)
		return -1;
	
	if (stat(fullname, &st) == -1 && errno == ENOENT)
		return 0;

	if (cfg_read_file(vars, fullname) == -1) {
		error(NULL, "Error: %s: %s", fullname, cfg_error());
		return -1;
	}
	return 0;
}



/*
 * Function:	config_save
 * Purpose:	Save the configuration file.
 * Arguments:	-
 * Return:	-1 for error, 0 for OK.
 */
int config_save(void) {
	char *fullname;

	fullname = config_filename();
	if (fullname == NULL)
		return -1;
	if (cfg_write_file(vars, fullname) == -1) {
		error(NULL, "Error: %s: %s", fullname, cfg_error());
		return -1;
	}
	return 0;
}



/*
 * Function:	config_get_boolean
 * Purpose:	Get value of boolean configuration variable.
 * Arguments:	var	variable identifier
 * Return:	zero or nonzero.
 */
int config_get_boolean(enum config var) {
	assert(var >= 0);
	assert(var < CONFIG_MAX);
	init_enum_to_ptr();
	return * (int *) enum_to_ptr[var];
}



/*
 * Function:	config_set_boolean
 * Purpose:	Set value of boolean configuration variable.
 * Arguments:	var	variable identifier
 *		newval	new value
 * Return:	-
 */
void config_set_boolean(enum config var, int newval) {
	assert(var >= 0);
	assert(var < CONFIG_MAX);
	init_enum_to_ptr();
	*(int *) enum_to_ptr[var] = newval;
}



/*
 * Function:	config_get_long
 * Purpose:	Get value of long configuration variable.
 * Arguments:	var	variable identifier
 * Return:	Integer.
 */
long config_get_long(enum config var) {
	assert(var >= 0);
	assert(var < CONFIG_MAX);
	init_enum_to_ptr();
	return * (long *) enum_to_ptr[var];
}



/*
 * Function:	config_set_long
 * Purpose:	Set value of long configuration variable.
 * Arguments:	var	variable identifier
 *		newval	new value
 * Return:	-
 */
void config_set_long(enum config var, long newval) {
	assert(var >= 0);
	assert(var < CONFIG_MAX);
	init_enum_to_ptr();
	*(long *) enum_to_ptr[var] = newval;
}



/*
 * Function:	config_get_string
 * Purpose:	Get value of string configuration variable.
 * Arguments:	var	variable identifier
 * Return:	zero or nonzero.
 */
char *config_get_string(enum config var) {
	assert(var >= 0);
	assert(var < CONFIG_MAX);
	init_enum_to_ptr();
	return * (char **) enum_to_ptr[var];
}



/*
 * Function:	config_set_string
 * Purpose:	Set value of string configuration variable.
 * Arguments:	var	variable identifier
 *		newval	new value
 * Return:	-
 */
int config_set_string(enum config var, char *newval) {
	assert(var >= 0);
	assert(var < CONFIG_MAX);
	assert(newval != NULL);
	init_enum_to_ptr();
	newval = strdup(newval);
	if (newval == NULL) {
		error(NULL, "Error: out of memory modifying configuration");
		return -1;
	}
	*(char **) enum_to_ptr[var] = newval;
	return 0;
}



/***********************************************************************
 * Local functions.
 */
 
 
/*
 * Purpose:	Make the configuration file name.
 */
static char *config_filename(void) {
	static char *fullname = NULL;
	
	if (fullname == NULL) {
		fullname = malloc(FILENAME_MAX);
		if (fullname == NULL) {
			error(NULL, "Error: out of memory");
			return NULL;
		}
		if (fnqualify(fullname, CONFIG_FILENAME, 
		              FILENAME_MAX) >= FILENAME_MAX) {
			error(NULL, "Error: configuration file name too long");
			return NULL;
		}
	}
	
	return fullname;
}
