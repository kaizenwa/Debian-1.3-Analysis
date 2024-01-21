/* The code should compile with either ANSI C or K&R compilers. */

/*
 *      Copyright (c) 1995, 1996 by William Deich.
 *      Written by William Deich.  Not derived from licensed software.

 *    You may distribute under the terms of either the GNU General Public
 *    License or the Artistic License, as specified in the README file.
 */

/* Use __P() to have prototypes in STD C code, and not use
 * prototypes in K&R C: declare functions as:
 *	func_type  funcname __P((arglist));
 */

/* ==================================================================== */

#include "localsys.h"
#include "hsearch.h"
#include "version.h"

/* ==================================================================== */

#ifndef HAVE_ALNUM
#define alnum(c) (isalpha(c) || isdigit(c))
#endif

#define SEP	" \t\v\n"		/* How to split fields on input lines */
#define QM	"\"'"			/* Quotemarks in fields */
#define CM	"#"			/* Comments in input file */
#define SAFE_IFS "IFS= \t\n"
#define OPTION_SEP '='			/* option separator */
#define CONDITION_SEP '~'		/* condition separator */

#define CLEAR_SETTINGS NULL

#ifndef SAFE_PATH
#define SAFE_PATH "PATH=/bin:/usr/bin:/usr/ucb"
#endif

/* The name under this program assumes it is installed.  If argv[0] isn't
 * [/.../]ONETRUENAME, we assume we're running via symlink.
 */
#ifndef ONETRUENAME
#define ONETRUENAME "super"
#endif

/* Kind of help we give */
#define HELP_BASIC	0	/* Basic help shows what you can execute */
#define HELP_FULL	1	/* Full help on each command */
#define HELP_FACTS	2	/* Just-the-facts-ma'm mode */

#ifndef SUPERFILE
#define SUPERFILE "/usr/local/lib/super.tab"
#endif

#ifndef PERUSER_SUPERFILE
#define PERUSER_SUPERFILE ".supertab"
#endif

#ifndef MAXFD
int getdtablesize __P(( void ));
#define MAXFD (getdtablesize()-1)
#endif

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 256
#endif

/* MAXSETENV is maximum number of variables that can be setenv'd on a
 * super control line.  This isn't the maximum that can be passed; it's
 * only the number of environment variables definitions that can be
 * made on one control line.
 */
#define MAXSETENV 40

/* If defined, then user patterns are allowed to match the uid as well as
 * the actual username.  We DISABLE this by default because users are
 * almost always identified by username.
 */
/* #define MATCH_DECIMAL_UID */

/* If defined, then group patterns are allowed to match the
 * gid as well as the group name, if any.  We ENABLE this by default
 * because it's not unusual for users to be put into unnamed groups
 * in the password file.
 */
#define MATCH_DECIMAL_GID

/* maximum number of tries at entering the password */
#define MAXTRY 3


#ifdef HAVE_INNETGR
extern int innetgr();
#endif

#ifndef HAVE_MEMSET
void *memset __P(( void *s, int c, size_t n ));
#endif

/* special code to indicate we haven't specified a uid yet */
#define UID_NOTSET ((uid_t) ~0)

/* special code to indicate we haven't specified a gid yet */
#define GID_NOTSET ((gid_t) ~0)

/* special code to indicate we haven't specified supplementary groups */
#define GROUPS_NOTSET ((GETGROUPS_T) ~0)


/* Number of elements in an array */
#define NELEM(x) (sizeof(x)/(sizeof(*x)))

/* n rounded up to a multiple of m -- both should be positive */
#define ROUNDUP(n, m) ( ((n) % (m) == 0) ? (n) : ((n)/(m) + 1)*(m) )

/* STRBEG evaluates to !0 if s1 begins with substring s2 */
#define STRBEG(s1, s2) (strncmp((s1), (s2), strlen(s2)) == 0)
 
/* STRMATCH3 expects 'end' to be a pointer into s2, and evaluates to !0
 * if characters s2..(end-1) fully match s1 (that is, the next character in
 * s1 is null.
 */
#define STRMATCH3(s1, s2, end) \
	       (strncmp(s1, s2, end-s2) == 0 && s1[end-s2]=='\0')
		    

#define UNTIL(e) while(!(e))

/* ========================================================================= */

/*
 * Super's main external variables
 */
    extern char *prog;			/* this program */
    extern int debug;			/* debug level; 0=off */
    extern int check_syntax;		/* just check syntax of superfile */
    extern char *superfile;		/* The actual superfile to be opened. */

/* The following external variables control the Error() routine.
 * They are modified at various points by super(), to give detailed control
 * over the error messages that are printed.
 */
    extern int error_stderr;		/* stderr() bool */
    extern int error_syslog;		/* syslog() bool */
    extern char *error_rlog_host;	/* where rsyslog() msgs are sent */
    extern char *error_command;		/* our program name */
    extern char *error_user;		/* our caller's username */

    extern int error_line;		/* input line number of error */
    extern int error_nl;		/* number of lines this msg refers to */
    extern char *error_srcfile;		/* filename of input with error */

/*
 * For use with strqtokS -- see that routine for definitions of strqS_xxx
 */
    extern unsigned char *strqS_qm;	/* For fast access by strqtokS */
    extern unsigned char my_qm[256];
    extern unsigned char *strqS_cc;	/* For fast access by strqtokS */
    extern unsigned char my_cc[256];
 

/*
 * Extern variable and routines used to compile/match user/group/host patterns.
 */
extern char *(*pat_compile) __P(( char *));
extern int (*pat_compare) __P(( char *));
extern int need_re_anchor;
/* ========================================================================= */

/*
 * Basic structures from which we construct other, bigger entities.
 */

struct simpleList {
    char *pat;
    struct simpleList *next;
};
typedef struct simpleList SimpleList;

struct simple2List {
    char *pat;
    struct simpleList *other;
    struct simple2List *next;
};
typedef struct simple2List Simple2List;

struct timeEnt {
    short begin;	/* Start time, in minutes */
    short end;		/* End time, in minutes */
    char day;		/* day of week to match (0..6); 7 means any day */
    char invert;	/* match is to be inverted */
};
typedef struct timeEnt TimeEnt;

struct timeList {
    TimeEnt te;
    struct timeList *next;
};
typedef struct timeList TimeList;

/* Linked open files */
struct fileList {
    char *givenname;	/* filename, as given in the parent file */
    char *fullname;	/* filename, fully expanded */
    FILE *fp;
    int line;		/* current line */
    int nl;		/* number of lines in this block */
    struct fileList *prev;
};
typedef struct fileList FileList;

struct countedString {
    char *s;		/* malloc'd string */
    int n;		/* number of characters in s */
    unsigned char used;	/* !0 means this string is in use */
};
typedef struct countedString CountedString;

struct strArray {
    CountedString *str;	/* Pts to malloc'd array of CountedString's */
    int n;		/* number strings allocated */
};
typedef struct strArray StrArray;

struct ourTime {
    time_t start;	/* when program started */
    short min;		/* local time of day, in minutes */
    char day;		/* local day, with 0=sunday */
};	
typedef struct ourTime OurTime;

struct conditions {
    int user;		/* !0 -> Last match to a user pattern */
    int time;		/* !0 -> Matched a time pattern */
    int allinverted;	/* !0 -> all time patterns scanned were inverted */
};
typedef struct conditions Conditions;

/* The progList struct is for listing all Cmd::File pairs on a control line. */
struct progList {
    char *Cmd;
    char *File;
};
typedef struct progList ProgList;

/* ========================================================================= */

/* Password information */
struct passInfo {
    int required;	/* -1 means not in use */
    int timeout;	/* Time before password must be re-entered */
    int renewtime;	/* update the timestamp file with each use of cmd? */
    int perhost;	/* create timestamp files separately for each host? */
    char user[1024];	/* value of timestampuid=xxx, if entered */
};
typedef struct passInfo PassInfo;

/* Information for logging use */
struct logInfo {
    FILE *fp;			/* logfile pointer */
    char filename[1024];	/* logfile name */
    char user[1024];		/* value of loguid=xxx, if entered */
    uid_t uid;			/* UID under which we open logfile */
    pid_t pid;			/* PID of the logger; -1 means none running. */
    unsigned char newfile;	/* Bool: !0 if logfile given but not yet used */
    unsigned char newuid;	/* Bool: !0 if loguid given, but not yet used */
};
typedef struct logInfo LogInfo;

/* progMatch is for keeping track of matches in a ProgList */
struct progMatch {
    ProgList *cmd_file;
    int match;	/* index in proglist of matched command; -1 if no match */
    int evermatched;	/* 0 if no cmd in file matched pat */
    char *commandfound;	/* If match >= 0, commandfound points to actual
			 * command matched.  This can differ from
			 * proglist.Cmd[match], because that Cmd can be
			 * a pattern.
			 */
    int n;
    int nalloc;
};
typedef struct progMatch ProgMatch;

/* ========================================================================= */

/*
 * Global information from the :global lines
 */
struct globalInfo {
    char owner[32];	/* Owner of FullPath must be this person; overridden
			 * by local owner=xxx option, if present.
			 */
    char *chdir_path;	/* Change to this dir before exec'ing; null if none */
    int relative_path;	/* Allow filenames to be relative.  This is
			 * in general a stupid idea.  Don't do it!
			 */
    int group_slash;	/* Allow group names to have slashes.  If you
			 * allow this, you make it harder to catch certain
			 * command-line typos.  Don't do it!
			 */
    int nice_incr;	/* value of the nice=nnn increment value */

    int mask;		/* umask setting */

    PassInfo passinfo;	/* password information */

    Simple2List userbefore;	/* list of u/g/h pats before per-cmd pats */
    Simple2List userafter;	/* list of u/g/h pats after per-cmd pats */
    SimpleList b_a_text;	/* list of original text for above */
    int user_clear;		/* clear userbefore list if new val seen */

    TimeList timebefore;	/* A list of the actual time ranges used */
    TimeList timeafter;		/* A list of the actual time ranges used */
    int time_clear;		/* clear timebefore list if new val seen */

    int use_after;		/* !set to !0 when we see <> */

    LogInfo log;		/* Information for logging to file */

    char mailcmd[500];		/* Information for logging via mail */
    int mail_success;		/* bool: mail on success? (-1 = unknown) */
    int gethostbyname;		/* bool: use gethostbyname()? */

    GETGROUPS_T groups[NGROUPS_MAX];	/* groups from [add]groups=xxx,... */
    int ngroups;		/* number of supplementary groups */
    int groups_added;		/* bool: groups were addgroups=, not groups= */
};
typedef struct globalInfo GlobalInfo;

/*
 * Information describing the caller
 */
struct userInfo {
    struct passwd caller;		/* who's invoking program */
    char hostname[MAXHOSTNAMELEN];	/* whence came the user */
    char lc_hostname[MAXHOSTNAMELEN];	/* hostname in lower case */
    int orig_mask;			/* umask setting at startup */

    uid_t new_uid;			/* new uid, from uid=xxx or u+g=xxx */
    gid_t new_gid;			/* new gid, from gid=xxx or u+g=xxx */

    OurTime ourtime;			/* when we started, etc */

    char encr[20];			/* encrypted password */
    char salt[4];			/* salt from password */
};
typedef struct userInfo UserInfo;

/*
 * Per-entry (in the super.tab file) information.  This gets filled in
 * at various points, as the program learns more.
 */
struct localInfo {
    ProgMatch progs;	/* Records prog::file sets, and is updated w/ matches */

    char *info;		/* value of info=xxx option */
    char *die;		/* Gets msg from die=msg ; null if none */
    char *print;	/* Gets msg from print=msg ; null if none */
    char *chdir_path;	/* Change to this dir before exec'ing; null if none */

    char user[32];	/* value of uid=xxx option */
    char group[32];	/* value of gid=xxx option */
    char u_g[32];	/* value of u+g=xxx option */
    char owner[32];	/* value of owner=xxx option */
    uid_t file_uid;	/* uid of the matched FullPath */
    gid_t file_gid;	/* gid of the matched FullPath */
    GETGROUPS_T groups[NGROUPS_MAX];	/* groups from [add]groups=xxx,... */
    int ngroups;	/* number of supplementary groups */
    int groups_added;	/* bool: groups were addgroups=, not groups= */
    char **env;		/* null-terminated list of vars from env=var[,...] */
    char *setenv[MAXSETENV+1];	/* values of setenv=var[,...] option */
    char *fdlist;	/* value of fd=nnn[,...] option */
    int mask;		/* value of umask=xxx option */
    int nice_incr;	/* value of the nice=nnn increment value */
    Simple2List userpats;	/* list of PermittedUser patterns */
    SimpleList origtext;	/* list of PermittedUser patterns */
    TimeList time;	/* A list of the actual time ranges used */
    int usr_args[2];	/* number of user-entered args allowed */
    StrArray argpats;	/* argNNN=xxx arguments */

    LogInfo log;	/* Information for logging to file */

    char mailcmd[500];	/* Information for logging via mail */
    int mail_success;	/* bool: mail on successful tries? (-1 = unknown) */

    int *fd;		/* descriptors from fdlist string */
    PassInfo passinfo;	/* password requirements on this command */
};
typedef struct localInfo LocalInfo;

/* ========================================================================= */

extern FileList *currfile;		/* list of currently-open files */

extern GlobalInfo globalinfo;		/* :global info */

extern UserInfo userinfo;		/* User's info */

extern LocalInfo localinfo;		/* per-cmd info */

extern Conditions matches;		/* To keep track of what matched */

/* ========================================================================= */
int	add_variable __P(( char *varname, char *vardefn ));
void	anchor __P(( char *in, char *out ));
char*	approve __P((char *usrcmd, int help, int version, int verbose));
int	blkfree __P((char **));
int	build_cmd_file __P((char *, int , char *, char **));
char**	buttonup __P((char *));
int	check_owner __P(( void ));
int	check_pass __P(( char *cmd ));
int	checkenv __P(( char *name,  char *value,  char *pat ));
char*	clean_buf __P(( char *buf, char *outbuf ));
void	close_writer __P(( void ));
int	colon_define __P(( char *command ));
int	colon_global __P(( char *command ));
int	colon_include __P(( char *command ));
int	conditions_and_options __P((char *cond_or_opt_wd));
char*	dayname __P(( int daynum ));
int	daynum __P(( int unixtime ));
void	debug_print __P((char *path, char **arglist, int n_builtin));
void	debug_hello __P((void));
int	do_options __P((int argc, char **argv, int *help, int *vers, int *verbose));
char*	do_variables __P(( char *str));
char*	ends __P(( char *s1, char *s2 ));
#ifdef HAVE_STDARG_H
int	Error __P(( int show_perror, int die, char * fmt, ... ));
#else
int	Error();
#endif
FileList* file_open __P(( FileList *list, char *name ));
FileList* file_close __P(( FileList *list ));
char*	fillbuffer __P(( FILE *fp, int *indentok, int *nl ));
int	findgid __P(( char *grouplabel ));
int	findgroup __P(( char *grouplabel ));
int	fixup_fullpath __P((int , char *, char *, char *, int ));
void	free_SimpleList __P(( SimpleList *));
void	free_Simple2List __P(( Simple2List *));
void	free_TimeList __P(( TimeList *));
int	get_canonical_hostname __P((char *buf, int len));
int	get_encrypted_pw __P(( void ));
int	get_owner __P((char *path, uid_t *uid, gid_t *gid));
int	get_password __P(( char *cmd, char *user,  char *salt,  char *encr ));
char*	Getenv __P(( char *s ));
int	Getgroups __P((int, GETGROUPS_T *));
int	getlogdir __P((char *user, char *buf));
struct passwd *
	getpwentry __P(( char *username));
int	global_arg __P((char *word));
int	globbraces __P(( char *s, int wrap_in_braces, char ***globlist ));
int	handle_option __P((char *word, char *s, int isglobal));
int	ingroup __P(( char * user, gid_t gid, char * gp_pat ));
void	init_nice_incr __P((int is_global));
void	init_strqtokS __P(( void ));
void	init_umask __P((int is_global));
void	init_globalinfo __P( (void) );
void	init_localinfo __P( (void) );
int	init_userinfo __P( (void) );
int	InsertCondition __P(( char *, char *, int ));
int	InsertTimeList __P(( char *, char **, TimeList *, char *, int ));
int	InsertUserList __P((char *, char **, Simple2List *, SimpleList *, int));
void	logmsg __P(( char * cmd, char ** args ));
int	makedir __P(( char *directories ));
char*	makedirname __P(( char *prefix, char *hostname, char *path ));
int	match_pattern __P((int match, int glob, char *str, char *pattern));
void	match_ugh_user __P((Simple2List *sl, int isglobal));
void	matchtime __P(( OurTime *our, TimeList *tl ));
char**	newargs __P((char *path_plus, char **argv, int *n_builtin));
FILE*	open_writer __P(( char *user, char *filename, pid_t *pid_p ));
void	opensuperlog __P(( void ));
int	option_clear_settings __P((char *word, char *s, int isglobal));
int	option_global_reset_settings __P((void));
int	option_local_clear_settings __P((void));
int	parseline __P((int givehelp, int checksyntax, char *buf, char *usrcmd));
void	printhelp __P(( int verbose ));
void	printhelp_hello __P(( int verbose, char *usrcmd ));
int	process_colon_cmds __P(( char *command ));
int	process_logfile_opt __P(( void ));
int	rcl_nice_incr __P((void));
int	rcl_umask __P((void));
void	re_anchor __P(( char *in, char *out));
char*	s_re_comp __P(( char *));
int	s_re_exec __P(( char *));
void	readtime_init __P(( void ));
int	readtime __P(( char *str, short *t1, short *t2, char *d ));
int	set_chdir __P((void));
int	set_nice_incr __P((void));
int	set_u_g __P(( void ));
void	set_umask __P((void));
int	Setgroups __P((int, GETGROUPS_T *));
int	shell_compare __P(( char *));
char*	shell_compile __P(( char *));
void	store_nice_incr __P((int nice_incr, int is_global));
void	store_umask __P((int mask, int is_global));
char*	str_val __P(( char *left, int sep, char *str ));
int	StrEltCpy __P(( StrArray *a, int ielt, char *str));
char*	StrEltGetPtr __P(( StrArray *a, int ielt ));
void	StrEltsUnused __P(( StrArray *a ));
void	StrInit __P(( StrArray *a ));
int	stringcopy __P(( char *to, char *from, int n));
int	StrLastInUse __P(( StrArray *a ));
int	StrNalloc __P(( StrArray *a, int nelt));
int	StrNelts __P(( StrArray *a ));
void	strtolower __P(( char *string ));
char*	strqtokS __P(( char *, char *, char *, char *, unsigned int ));
int	user_supertab __P(( char *command ));
