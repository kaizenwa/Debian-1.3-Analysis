/*
	System: Structured text retrieval tool sgrep.
	Module: config.h
	Author: Pekka Kilpeläinen & Jani Jaakkola
	Description: header file for user configurable options
	Version history: Original version February 1995 by JJ & PK
	Copyright: University of Helsinki, Dept. of Computer Science
		   Distributed under GNU General Public Lisence
		   See file COPYING for details
*/

/*
 * This is sgrep configuration file for user configurable options
 */

/* CONFIGURATION OPTIONS */

 
/* 
 * Environment variable for specifying options
 */
#define ENV_OPTIONS "SGREPOPT"

/* 
 * Default macro files
 */
#define USER_SGREPRC	".sgreprc" 
#define SYSTEM_SGREPRC	LIBDIR"/sgreprc"

/* 
 * Define this if you wan't sgrep to be able to exec external preprocessors 
 */
#define USE_EXEC

/*
 * This is the shell, with which the external processes are executed
 * This is passed directly to exec function
 */  
#define EXEC_SHELL "/bin/sh","sh","-c"

/*
 * Default preprocessor. NULL means use inbuilt. "-" means none.
 * NOTE: There isn't inbuilt preprocessor yet in version 1.0
 */
#define PRE_PROCESSOR "m4"

/*
 * If you want stream mode by default define this
 */
/* #define STREAM_MODE */

/*
 * Do you want -V progress reporting option ?
 * NOTE: it's actually useful mainly for debugging, and 
 *       might slow things down
 */
#define PROGRESS_REPORTS

/*
 * Prefix for the temp files, if they must be created
 * Currently (for v1.0) temp files are created only when reading from stdin
 */
#define TEMP_FILE_PREFIX "/tmp/sgrep-tmp"

/* 
 * If this is TRUE then failure to open a file is considered fatal
 */
#define OPEN_FAILURE FALSE

/* 
 * The default output styles
 */
#define LONG_OUTPUT ( \
	"------------- #%n %f: %l (%s,%e : %i,%j)\\n%r\\n")
#define SHORT_OUTPUT ( "%r" )


/* LIMITS */


/*
 * Maximum number of expresssions, you can specify 
 * with the -e or -f options 
 */
#define MAX_EXPRESSIONS	50

/* Size of command buffer (maximum size of all command files combined) */
#define COMBUF_SIZE  16384


/* DEBUGGING OPTIONS */


/* 
 * This turns on assertions for all modules. It's really useful, if you are
 * going to do some hacking. It slows things down though.
 */
#define ASSERT

/*
 * This prevents using macros for get_region, prev_region and add_region
 * functions. Point is to use assertions, which are defined only
 * in the actual function body
 */
#define NO_MACROS

/*
 * If you wan't to see how operator tree is shrinked, define this
 */
/* #define DEBUG_OPTTREE */
