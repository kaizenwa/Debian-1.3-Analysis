/* Functions for processing option strings are OptionFunc's, and they
 * have the following arguments:
 *	wd	-- the entire "xxx<SEP>yyy" string;
 *	s	-- points to "yyy" in wd;
 *	global	-- is !0 if this is a global invocation.
 */
typedef int (*OptionFunc) __P((char *wd, char *s, int global));

/* Flags for use in the options description */
#define 	IS_ROOT		001 /* name is just leading part of xxx=yyy;
				     * that is, name might be "xx" and entire
				     * option name is xx0, xx1, ...
				     */
#define		GLOBAL		002 /* option is permitted as global opt */
#define		LOCAL		004 /* option is permitted as local opt */
#define		LIST		007 /* Run globbraces on argument, then
				     * expanded list as well as string.
				     */

struct option {
    char *name;			/* name of option */
    int flags;
    OptionFunc process;		/* Function to process this option */
};
typedef struct option Option;

/* Option-processing functions */

int option_clear_settings __P(( char *word, char *s, int isglobal ));
int option_global_reset_settings __P(( void ));
int option_local_clear_settings __P(( void ));

static int option_groups	__P(( char *word, char *s, int isglobal ));
static int option_arg		__P(( char *word, char *p, int isglobal ));
static int option_cd		__P(( char *word, char *s, int isglobal ));
static int option_die		__P(( char *word, char *s, int isglobal ));
static int option_env		__P(( char *word, char *s, int isglobal ));
static int option_fd		__P(( char *word, char *s, int isglobal ));
static int option_gethostbyname	__P(( char *word, char *s, int isglobal ));
static int option_gid		__P(( char *word, char *s, int isglobal ));
static int option_group_slash	__P(( char *word, char *s, int isglobal ));
static int option_groups	__P(( char *word, char *s, int isglobal ));
static int option_info		__P(( char *word, char *s, int isglobal ));
static int option_logfile	__P(( char *word, char *s, int isglobal ));
static int option_loguid	__P(( char *word, char *s, int isglobal ));
static int option_mail		__P(( char *word, char *s, int isglobal ));
static int option_mailany	__P(( char *word, char *s, int isglobal ));
static int option_nargs		__P(( char *word, char *s, int isglobal ));
static int option_nice		__P(( char *word, char *s, int isglobal ));
static int option_owner		__P(( char *word, char *s, int isglobal ));
static int option_password	__P(( char *word, char *s, int isglobal ));
static int option_patterns	__P(( char *word, char *s, int isglobal ));
static int option_print		__P(( char *word, char *s, int isglobal ));
static int option_relative_path	__P(( char *word, char *s, int isglobal ));
static int option_renewtime	__P(( char *word, char *s, int isglobal ));
static int option_rlog_host	__P(( char *word, char *s, int isglobal ));
static int option_setenv	__P(( char *word, char *s, int isglobal ));
static int option_syslog	__P(( char *word, char *s, int isglobal ));
static int option_timeout	__P(( char *word, char *s, int isglobal ));
static int option_timestampbyhost __P(( char *word, char *s, int isglobal ));
static int option_timestampuid	__P(( char *word, char *s, int isglobal ));
static int option_u_g		__P(( char *word, char *s, int isglobal ));
static int option_uid		__P(( char *word, char *s, int isglobal ));
static int option_umask		__P(( char *word, char *s, int isglobal ));
