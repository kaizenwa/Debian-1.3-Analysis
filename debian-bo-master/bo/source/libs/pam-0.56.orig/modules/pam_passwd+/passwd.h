/*
 * passwd -- a super dooper password changer
 * it checks for specifically bad passwords.
 *
 * this is the header file
 */

#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <grp.h>
#include <pwd.h>
#include <sys/types.h>
#include "sys.h"
#include <syslog.h>
#include <string.h>
#include <memory.h>
#include <malloc.h>
#include <unistd.h>
#include <time.h>
#include <regex.h>

#ifndef LINUX    /* AGM added this as of 0.2 */
#include <security/pam_appl.h>
#endif           /* ditto */
#include <security/pam_modules.h>

extern char *strdup (const char *s);

#ifdef GETUSERSHELL
extern char *GETUSERSHELL();	/* return legal user shells */
#endif

/*
 * the following should be reset to those parameters
 * that fit best with your system
 */

#define DEFPWFILE	"/usr/lib/passwd+/passwd.data" /* password file */
#define PWTESTFILE	"/usr/lib/passwd+/passwd.test" /* password test file */
#define DEFAULTTEST				/* default test if needed */
#define LG_DEFAULT	LG_NONE			/* default logging */
#define LG_OUTDEF	"> /var/log/passwd.log"	/* default logging location */
#define LG_INIT		" "			/* initial logging */

/*
 * the following may be changed but odds are you won't want to
 */

#define ROOTID		0		/* the UID of the superuser */
#define PWSIGCHARS	80		/* significant characters */
#define MAXLOGTO	10		/* max number of open logs */
#define GECOSSIZE	100		/* max size of gecos fields */

/*
 * the following should NEVER be changed (unless you're suicidal!)
 */

/**** WARNING -- IF YOU CHANGE THIS REMEMBER TO DELETE THE ****/
/**** 		 APPROPRIATE ESCAPES IN VERIFY.C           ****/
#define NUVAR		10	/* number of user definable escapes */

/***** WARNING -- IF YOU CHANGE THIS THEN MAKE SURE THAT YOU HAVE ****/
/*****            THE VARIABLES IN 'load.c'.                      ****/
#define NUMIPVAR	47	/* Total number of variables */

/*
 *========== L O G G I N G  M A C R O S ==========
 *
 * We allow the user to log a number of ways
 * The macros below indicate his choices
 */

#define LG_FILE		0x01		/* to a file */
#define LG_PIPE		0x02		/* to a pipe */
#define LG_STDERR	0x04		/* to syslog */
#define	LG_SYSLOG	0x08		/* syslog; may not be valid */

/*
 * he can log several sorts of things:
 */

#define	LG_NONE		0x00		/* log nothing */
#define	LG_SYNTAX	0x01		/* log syntax errors */
#define LG_USE		0x02		/* log use of program */
#define LG_RESULT	0x04		/* log result of run */
#define LG_ITEM		0x08		/* log why test failed */
#define LG_DEBUG	0x10		/* log debugging information */
#define LG_SYSTEM	0x20		/* log system problems */
#define LG_ALL		0x3f		/* log everything */

/*
 * a convenience to make excluding components easy
 */

#define LG_NEG		'!'		/* negation character */

/*
 * some generally useful macros
 */
					/* ends log keyword */
#define log_end(x)	(ispunct((x))||isspace((x))||((x)=='\0'))
#define log_set(x,y)	((x)|=((y)&LG_ALL))	/* set one bit for logging */
#define log_reset(x,y)	((x)&=((~(y))&LG_ALL))	/* clear one bit for logging */
#define	log_test(x,y)	(((x)&(y))==(y))	/* 1 if to log y */

/*
 *========== E S C A P E   S E Q U E N C E S   F O R   T E S T S ==========
 *
 * these tell how to access escape sequences
 */

#define	F_ASIS		0	/* insert the string as is */
#define F_NUMBER	1	/* insert the length of the string */
#define F_UPPER		2	/* insert the string in upper case */
#define F_LOWER		3	/* insert the string in lower case */
#define F_FIRST		4	/* insert the string first capitalized */

/*
 * a number or a string?
 */

#define	TY_STR		0	/* string */
#define	TY_NUM		1	/* number */

/*
 * map upper case to lower case
 */

#define lowcase(x)	(isupper((x)) ? tolower((x)) : (x))
#define upcase(x)	(islower((x)) ? toupper((x)) : (x))

/*
 * structure to hold "%" and "#" escapes (ie, internal variables)
 */

struct intvar {
	char name;		/* internal variable name to the user */
	int  nstype;		/* 1 if a number, 0 if a string */
        int  item_num;		/* Item number for value strings */
	int  userset;		/* 1 if this can be reset */
};

struct intvar_value {
	char *string;	/* string value */
	char *length;	/* length (as a string) */
};

#define IVSTRING(opt,ipx)  (((opt->intvar_values)[(ipx)->item_num]).string)
#define IVLENGTH(opt,ipx)  (((opt->intvar_values)[(ipx)->item_num]).length)

#define IVCLEAN(opt,ipx)				\
  do {							\
    if (IVSTRING(opt, ipx) != (char *) 0)		\
        free (IVSTRING(opt, ipx));			\
    if (IVLENGTH(opt, ipx) != (char *) 0)		\
        free (IVLENGTH(opt, ipx));			\
  } while (0)

#define	IVSPASSIGN(opt, ipx, y)				\
  do {							\
    IVCLEAN(opt, ipx);					\
    IVSTRING(opt, ipx) = strdup((const char *)(y));	\
    IVLENGTH(opt, ipx) = tonum(strlen((const char *)y));\
  } while (0)

#define	IVSASSIGN(opt, x, y)				\
  do {							\
    register struct intvar *ipx = findiv((int)(x));	\
    IVSPASSIGN (opt, ipx, y);				\
  } while (0)

#define IVNASSIGN(opt, x, y, z)				\
  do {							\
    register struct intvar *ipx = findiv((int)(x));	\
    IVCLEAN(opt, ipx);					\
    IVSTRING(opt, ipx) = tonum((y));			\
    IVLENGTH(opt, ipx) = tonum((z));			\
  } while (0)

/*
 *========== N U L L   P O I N T E R S ==========
 *
 * NULL pointers -- these are used to shut lint up
 */

#if 0
#define CH_NULL	((char *) NULL)		/* NULL character pointer */
#define FI_NULL ((FILE *) NULL)		/* NULL file pointer */
#define GR_NULL ((struct group *) NULL)	/* NULL group structure pointer */
#define IV_NULL ((struct intvar *) NULL)/* NULL escape structure pointer */
#define PW_NULL ((struct passwd *) NULL)/* NULL password structure pointer */
#define TI_NULL ((time_t *) NULL)	/* NULL time pointer */
#endif

/*
 * each log gets an entry like this
 */
struct loginfo
{
    unsigned short log;		/* what is being logged (level) */
    char *loc;		/* where it is going */
    FILE *locp;			/* file pointer if needed */
    unsigned short flags;	/* generic flag describing loc */
};

/*
 *========== D E C L A R A T I O N S ==========
 *
 * globals
 */

struct _options
{
    /* Variables for the main pam_passwd function */
    pam_handle_t *pamh;
    int           flags;
    int		  ctrl;
    struct passwd *pwd;
    const char	  *user;
    char	  *pwtest;
    char	  *password;
    char	  *password_old;

    /* Globals from the passwd+ program */
    int		  pwsig;	/* number of significant chars in password */
    unsigned int  logging;	/* logging level */
    int		  log_default;  /* default logging level */

    /* From the load.c module */
    struct intvar_value  intvar_values[NUMIPVAR];

    /* From the log.c module */
    struct loginfo logto[MAXLOGTO];	/* logging files */

    /* From the pattern.c module */
    char	   *pattern;		/* points to typed-in pattern */
    regex_t	   *regpattern;		/* Pointer to the compiled pattern */

    /* From the verify.c module */
    int		   linect;		/* count of lines */
    const char	   *sucmsg;		/* message on failure */

    /* For the parser, test.y */
    char	   *lptr;		/* Used to walk the strings */
    int		   ateol;		/* end of the string */
    int		   retval;		/* successful return value */
};

extern struct intvar iv[NUMIPVAR];	/* internal variables */

/* generic options */

#define PAM_ST_DEBUG         01
#define PAM_ST_NO_WARN       02
#define PAM_ST_USE_PASS1     04
#define PAM_ST_TRY_PASS1    010

/* simulation options */

#define PAM_ST_EXPIRED      020
#define PAM_ST_FAIL_1       040
#define PAM_ST_FAIL_2      0100
#define PAM_ST_PRELIM      0200
#define PAM_ST_REQUIRE_PWD 0400

/*
 * external declarations for procedures
 */

#include "extdecs.h"

/*
 * lint stuff
 */

#define FPRINTF	(void) fprintf	/* return value of fprintf NEVER used */
#define PRINTF	(void) printf	/* return value of printf NEVER used */
#define SPRINTF	(void) sprintf	/* return value of sprintf NEVER used */
