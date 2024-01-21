#ifndef _SUCK_KILLFILE_H
#define _SUCK_KILLFILE_H 1

#ifdef HAVE_UNISTD_H
#include <sys/types.h> /* for pid_t */
#endif

#ifdef USE_REGEX
#include <regex.h>	/* for regex_t */
#endif

#ifdef USE_REGEX
typedef struct {
	regex_t *ptrs;
	int nr;
} my_regex, *pmy_regex;
#endif

/* this is a structure so can add other kill options later */
typedef struct {
	int hilines;	/* nr of lines max article length */
	int lowlines;	/* nr of lines min article length */
	int maxgrps;	/* max nr of grps (to prevent spams) */
	char quote;	/* character to use as quote (for case compare) */
	char pathhost_sep;
	char subject_sep;
	char from_sep;
	char nntp_sep;
#ifdef USE_REGEX
	my_regex path;
	my_regex from;
	my_regex subj;
	my_regex nntphost;
#else
	char *path;	/* which hosts in path to kill */
	char *from;	/* look for name in from to kill */
	char *subj;	/* look for words in subj to kill */
	char *nntphost;	/* look for words in nntp_posting_host */
#endif
} OneKill, *POneKill;

typedef struct {
	OneKill match;
	int delkeep;
	char *group;	/* dynamically allocated */
} Group, *PGroup;

typedef struct {
	int Stdin;
	int Stdout;
	pid_t Pid;
} Child;
 
typedef struct killstruct {
	int logyn;
	int totgrps;
	PGroup grps;	/* dynamicly allocated array */
	int ( *killfunc)(struct killstruct *, char *); /*function to call */
	Child child;		/* these two are last since can't initialize */
	OneKill master;
} KillStruct, *PKillStruct;

/* function prototypes for killfile.c */
int get_one_article_kill(PMaster, int, PKillStruct);
PKillStruct parse_killfile(int);
void free_killfile(PKillStruct);
int chk_msg_kill(PKillStruct, char *);

/* function prototypes for killprg.c */
int killprg_forkit(PKillStruct, char *);
int chk_msg_kill_fork(PKillStruct, char *);
void killprg_closeit(PKillStruct);

#endif /* _SUCK_KILLFILE_H */
