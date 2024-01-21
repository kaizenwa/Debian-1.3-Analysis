/*
 * File:	config.h
 * Purpose:	Central location for most compile time SeX configurabilities.
 * Author:	Lars Wirzenius
 * Version:	"@(#)SeX:$Id: config.h,v 1.7 1996/12/06 13:15:29 liw Exp $"
 * Description:	This file defines various variables and macros that configure
 *		SeX in various ways.
 *
 *		Note that all variables are static.  This increases the
 *		memory consumption a little, but very little.  The important
 *		thing is to avoid macros.
 */
 
#ifndef config_h
#define config_h



/*
 * Macro:	HELP_FILE
 * Purpose:	Give the name of the help file.
 */
#ifndef HELP_FILE
#define HELP_FILE	"/usr/lib/SeX/sex.help.txt"
#endif



/*
 * Stuff for the configuration file (~/.sexrc).
 */
 
enum config {
	CONFIG_SAVE_BACKUP,
	CONFIG_FMT_PROGRAM,
	CONFIG_FONT,
	CONFIG_MAX_KILLRING,
	CONFIG_INDENT_WIDTH,
	CONFIG_MAX
};
 
int config_read(void);
int config_save(void);
int config_get_boolean(enum config);
void config_set_boolean(enum config, int);
long config_get_long(enum config);
void config_set_long(enum config, long);
char *config_get_string(enum config);
int config_set_string(enum config, char *);

#endif
