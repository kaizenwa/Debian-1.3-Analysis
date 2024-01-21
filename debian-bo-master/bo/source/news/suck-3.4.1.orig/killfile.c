#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <string.h>
#include <ctype.h>

#include "suck_config.h"

#ifdef USE_REGEX
#include <regex.h>
#endif

#include "both.h"
#include "suck.h"
#include "suckutils.h"
#include "killfile.h"
#include "phrases.h"
#ifdef TIMER
#include "timer.h"
#endif

#define DEFAULT_PATHHOST_SEP ','		/* character used to separate hosts in the pathhost line in killfile */
#define DEFAULT_SUBJECT_SEP ','		/* character used to separate words in the subject line in killfile */
#define DEFAULT_FROM_SEP ','		/* character used to separate words in the subject line in killfile */
#define DEFAULT_NNTP_SEP ','		/* character used to separate names in the nntphost line in killfile */

#define GROUP_KEEP "keep"
#define GROUP_DELETE "delete"		/* word in param file parsed for on group line to signify keep or delete */
#define COMMA ','			/* group separator in newsgroup line in article */

/* local function prototypes */
char *get_a_chunk_mem(PMaster);
int check_line(char *, const char *, const char *, int, int, char);
void free_node(OneKill);
int parse_a_file(const char *, const char *, POneKill);
int pass_two(PKillStruct);
int check_a_group(POneKill, char *, int *);
const char *strnstr(const char *, const char *);

#ifdef USE_REGEX
void regex_scan(pmy_regex, char *, char, char);
int regex_check(char *, const char *, pmy_regex);
#else
char *malloc_line(int which, const char *linein);
#endif
/*-------------------------------------------------------------------------*/
/* the enum must match the phrases in suckengl.c  killf_reasons[] */
enum { REASON_NONE, REASON_TOOMANYLINES, REASON_NOTENUFLINES, REASON_PATHHOST,
REASON_SUBJECT, REASON_FROM, REASON_NNTPHOST, REASON_NRGRPS, REASON_NOKEEP, REASON_TIE};
enum { CHECK_EXACT, CHECK_CHECK };

const struct {
	int len;
	const char *name;
	int headerlen;
	const char *header;
} Params[] = {
	{8, "HILINES=", 7, "Lines: " },
	{9, "LOWLINES=", 7, "Lines: "},
	{5, "PATH=", 6, "Path: " },
	{8, "SUBJECT=", 9, "Subject: " },
	{5, "FROM=", 6, "From: " },
	{7, "NRGRPS=", 12, "Newsgroups: "},
	{8, "NNTPHOST=", 19,"NNTP-Posting-Host: "},
	{6, "GROUP=", 0, ""},
	{6, "QUOTE=", 0, ""},
	{8, "PROGRAM=", 0, ""},
	{9, "PATH_SEP=", 0, ""},
	{12,"SUBJECT_SEP=", 0, ""},
	{9, "FROM_SEP=", 0, ""},
	{13,"NNTPHOST_SEP=", 0, ""}
};


enum { PARAM_HILINE, PARAM_LOWLINE, PARAM_PATH, PARAM_SUBJ, PARAM_FROM, PARAM_NRGRPS, PARAM_HOST, PARAM_GROUP, PARAM_QUOTE,
 PARAM_PROGRAM, PARAM_PATHHOST_SEP, PARAM_SUBJECT_SEP, PARAM_FROM_SEP, PARAM_NNTP_SEP};
#define NR_PARAMS ((int) (sizeof(Params)/sizeof(Params[0])))

enum { DELKEEP_KEEP, DELKEEP_DELETE };
/*--------------------------------------------------------------------------*/
PKillStruct parse_killfile(int logfile_yn) {
	FILE *fptr;
	char buf[MAXLINLEN+1];
	int doprg = FALSE, retval = TRUE;

	static KillStruct Masterkill = { TRUE, 0, NULL, chk_msg_kill };	/* initialize everything first */

	/* kill file is going to get three passes, 1st one to count how many group files to process */
	/* so can allocate memory for all the group stuff. */
	/* also process PROGRAM line if it exists.  If we have one we don't do anything else */
	/* 2nd pass will be to actually process the group files */
	/* 3rd pass will be to process the master delete stuff */

	Masterkill.logyn = logfile_yn;
	
	/* FIRST PASS THRU MASTER KILLFILE - look for group delete/keeps and count em  and check for PROGRAM call*/
	if((fptr = fopen(full_path(FP_GET, FP_DATADIR, N_KILLFILE), "r")) == NULL) {
	/*	MyPerror(full_path(FP_GET, FP_DATADIR, N_KILLFILE));	*/
	/* this is not really an error, so don't report it as such */
		retval = FALSE;
	}
	else {
#ifdef DEBUG2
		do_debug("Pass 1 kill file: %s\n", full_path(FP_GET, FP_DATADIR, N_KILLFILE));
#endif
		while(fgets(buf, MAXLINLEN, fptr) != NULL && doprg == FALSE) {
#ifdef DEBUG2
			do_debug("Read kill file line: %s\n", buf);
#endif
			if(strncmp(buf, Params[PARAM_GROUP].name, (size_t) Params[PARAM_GROUP].len) == 0) {
				Masterkill.totgrps++;	/* how many group files must we process */
			}						
			if(strncmp(buf, Params[PARAM_PROGRAM].name, (size_t) Params[PARAM_PROGRAM].len) == 0) {
				doprg = killprg_forkit(&Masterkill, &buf[Params[PARAM_PROGRAM].len]);
			}
		}
		(void) fclose(fptr);

		if(doprg == TRUE) {
			Masterkill.totgrps = 0;
		}
		else {
			/* SECOND PASS - call routine */
			if(Masterkill.totgrps > 0) {
				if(pass_two(&Masterkill) == RETVAL_ERROR) {
					retval = FALSE;
				}
			}
			/* THIRD PASS - process master delete stuff */
			if(retval != FALSE && parse_a_file(N_KILLFILE, "Master", &(Masterkill.master)) != RETVAL_OK) {
				retval = FALSE;
			}
		}
	}

	if(retval == FALSE) {
		free_killfile(&Masterkill);	/* just in case any memory got allocated */
	}
	return (retval == FALSE) ? NULL :  &Masterkill;
}
/*-------------------------------------------------------------------------------------------*/
int pass_two(PKillStruct killp ) {

	int retval = RETVAL_OK;
	FILE *fptr;
	char buf[MAXLINLEN];
	int grpon = 0;
	size_t i;
	char *grpname, *grpfile, *delkeep;

	grpname = grpfile = delkeep = NULL;

	/* SECOND PASS - now that we know how many, we can allocate the space for em */
	/* and then have parse_a_file read em in. */
	if((killp->grps = calloc((size_t) killp->totgrps, sizeof(Group))) == NULL) {
		retval = RETVAL_ERROR;
		error_log(ERRLOG_REPORT, killf_phrases[1], NULL);
	}
	else if((fptr = fopen(full_path(FP_GET, FP_DATADIR, N_KILLFILE), "r")) == NULL) {
		MyPerror(full_path(FP_GET, FP_DATADIR, N_KILLFILE));
		retval = RETVAL_ERROR;
	}
	else {
#ifdef DEBUG2
		do_debug("Pass 2 kill file: %s\n", full_path(FP_GET, FP_DATADIR, N_KILLFILE));
#endif
		while(retval == RETVAL_OK && fgets(buf, MAXLINLEN, fptr) != NULL) {
#ifdef DEBUG2
			do_debug("Read kill file line: %s\n", buf);
#endif
			if(strncmp(buf, Params[PARAM_GROUP].name, (size_t) Params[PARAM_GROUP].len) == 0 ) {
				
				/* now parse the line for the 3 required elements */
				/* keep/delete group_name filename */
				delkeep = &buf[Params[PARAM_GROUP].len];
				if(strncmp(delkeep, GROUP_KEEP, (size_t) strlen(GROUP_KEEP)) == 0) {
					killp->grps[grpon].delkeep = DELKEEP_KEEP;
				}
				else if(strncmp(delkeep, GROUP_DELETE, (size_t) strlen(GROUP_DELETE)) == 0) {
					killp->grps[grpon].delkeep = DELKEEP_DELETE;
				}
				else {
					retval = RETVAL_ERROR;
				}
				if(retval == RETVAL_OK) {
					grpname = strchr(delkeep, ' ');	/* find the space */
					if(grpname == NULL) {
						retval = RETVAL_ERROR;
					}
					else {
						++grpname;	/* move past space */
						grpfile = strchr(grpname, ' ');
						if(grpfile == NULL) {
							retval = RETVAL_ERROR;
						}
						else {
							*grpfile = '\0';	/* truncate the group name for easier copying later */
							++grpfile;
						}
						/* nuke newline */
						i = strlen(grpfile) - 1;
						if(grpfile[i] == '\n') {
							grpfile[i] = '\0';
						}
					}

				}
				if(retval == RETVAL_ERROR) {
					error_log(ERRLOG_REPORT, killf_phrases[2], buf);
				}
				else {	/* have all three params, put them in place and parse the file */
					/* +1 for newline */
					if((killp->grps[grpon].group = malloc(strlen(grpname)+1)) == NULL) {
						error_log(ERRLOG_REPORT, killf_phrases[0], NULL);
						retval = RETVAL_ERROR;
					}
					else {
						strcpy(killp->grps[grpon].group, grpname);
						retval = parse_a_file(grpfile, grpname, &(killp->grps[grpon].match));
					}
				}
				grpon++;	/* finished with this group */
			}
		}
		(void) fclose(fptr);
	}

	return retval;
}
/*--------------------------------------------------------------------------*/
void free_killfile(PKillStruct master) {

	int i;

	if(master != NULL) {
		/* first kill off killprg if its there */
		if(master->killfunc==chk_msg_kill_fork) {
			killprg_closeit(master);
		}
		free_node(master->master);
		if(master->totgrps > 0) {
			for(i=0;i<master->totgrps;i++) {
				free_node(master->grps[i].match);
				free(master->grps[i].group);
			}
			free(master->grps);
		}
	}
}
/*--------------------------------------------------------------------*/
void free_node(OneKill node) {
#ifndef USE_REGEX
	if(node.path != NULL) {
		free(node.path);
	}
	if(node.from != NULL) {
		free(node.from);
	}
	if(node.subj != NULL) {
		free(node.subj);
	}
	if(node.nntphost != NULL) {
		free(node.nntphost);
	}
#else
	int i;

	if(node.path.ptrs != NULL) {
		for(i = 0; i < node.path.nr; i++) {
			regfree(&(node.path.ptrs[i]));
		}
		free(node.path.ptrs);
	}
	if(node.from.ptrs != NULL) {
		for(i = 0; i < node.from.nr; i++) {
			regfree(&(node.from.ptrs[i]));
		}
		free(node.from.ptrs);
	}
	if(node.subj.ptrs != NULL) {
		for(i = 0; i < node.subj.nr; i++) {
			regfree(&(node.subj.ptrs[i]));
		}
		free(node.subj.ptrs);
	}
	if(node.nntphost.ptrs != NULL) {
		for(i = 0; i < node.nntphost.nr; i++) {
			regfree((&node.nntphost.ptrs[i]));
		}
		free(node.nntphost.ptrs);
	}
#endif
}
/*--------------------------------------------------------------------------*/
int get_one_article_kill(PMaster master, int logcount, PKillStruct killp) {

	char buf[MAXLINLEN+1], *inbuf;
	const char *fname;
	int retval;
	FILE *fptr;

	retval = RETVAL_OK;

	/* build command to get article header*/
	sprintf(buf, "head %s\r\n", (master->curr)->msgnr);

	switch(send_command(master, buf, NULL, 221)) {
	  case RETVAL_ERROR:
		retval = RETVAL_ERROR;
		break;
	  case RETVAL_OK:
		if((inbuf = get_a_chunk_mem(master)) == NULL) {
			retval = RETVAL_ERROR;
		}
		/* the killfunc pointer points to either chk_msg_kill() or chk_msg_kill_fork() */
		  else if((*killp->killfunc)(killp, inbuf) == FALSE) {
			if(master->MultiFile == TRUE) {
				/* open file */
				/* file name will be ####-#### ex 001-166 (nron,total) */
				sprintf(buf,"%0*d-%d", logcount, master->itemon, master->nritems);
				fname = full_path(FP_GET, FP_MSGDIR, buf);
				if((fptr = fopen(fname, "w")) == NULL) {
					MyPerror(fname);
					retval = RETVAL_ERROR;
				}
				else {
					fputs(inbuf, fptr);
					fputs("\n", fptr);	/* needed */
					sprintf(buf, "body %s\r\n", (master->curr)->msgnr);
					switch(send_command(master, buf, NULL, 222)) {
					  case RETVAL_OK:
						retval = get_a_chunk(master->sockfd, fptr);
						break;
					  case RETVAL_ERROR:
						retval = RETVAL_ERROR;
						break;
					  case RETVAL_UNEXPECTEDANS:
						break;
					}
					(void) fclose(fptr);
					if(retval != RETVAL_OK) {
						unlink(fname);
					}
				}
			}
			else {
				fputs(inbuf, stdout);
				fputs("\n", stdout);
				sprintf(buf, "body %s\r\n", (master->curr)->msgnr);
				switch(send_command(master, buf, NULL, 222)) {
				  case RETVAL_OK:
					retval = get_a_chunk(master->sockfd, stdout);
					/* this is needed as a separator */
					/* in stdout version */
					fputs(".\n", stdout);
					break;
				  case RETVAL_ERROR:
					retval = RETVAL_ERROR;
					break;
				  case RETVAL_UNEXPECTEDANS:
					/* nothing to do  */
					break;
				}			
			
			}
			if(retval == RETVAL_OK) {
				master->nrgot++;
			}
		}
		break;
	  case RETVAL_UNEXPECTEDANS:
		break;
	}
	return retval;
}
/*---------------------------------------------------------------*/
/* this routine is same as get_a_chunk() except it goes to memory */
char *get_a_chunk_mem(PMaster master) {

	static char *buf = NULL;
	static int bufsize = 8192;

	int retval, done, partial, len, currbuf;
	char *inbuf, *newbuf;

	retval = RETVAL_OK;
	done = FALSE;
	partial = FALSE;
	currbuf = 0;
	if(buf == NULL) {
#ifdef DEBUG2
		do_debug("allocing memory, size = %d\n", bufsize);
#endif
		if((buf=malloc((size_t) bufsize)) == NULL) {
			error_log(ERRLOG_REPORT, killf_phrases[0], NULL);
		}
	}
	while(buf != NULL && done == FALSE) {
		len=sgetline(master->sockfd, &inbuf);
#ifdef TIMER
		(void) TimerFunc(TIMER_ADDBYTES, len, NULL);
#endif
		if(len < 0) {
			free(buf);
			buf = NULL;
			done = TRUE;
		}
		else if(partial == FALSE && inbuf[0] == '.') {
			if(len == 2 && inbuf[1] == '\n') {
				done = TRUE;
			}
			else if(master->MultiFile == TRUE) {
				/* handle double dots IAW RFC977 2.4.1*/
				/* don't do if we aren't doing multifile, since */
				/* stdout needs the .. to distinguish dots and EOM */
				inbuf++;	/* move past first dot */
				len--;
			}
		}
		if(done == FALSE) {
			if((len+currbuf) > bufsize) {
				/* buffer not big enough realloc */
				bufsize = len+currbuf;
				if((newbuf = realloc(buf, (size_t) bufsize)) == NULL) {
					free(buf);
					buf = NULL;
					error_log(ERRLOG_REPORT, killf_phrases[0], NULL);
#ifdef DEBUG2
					do_debug("Re-alloc failed\n");
#endif
				}
				else {
					buf = newbuf;
#ifdef DEBUG2
					do_debug("Re-alloc succeeded, new size = %d\n", bufsize);
#endif
				}
			}	
			if(buf != NULL) {
				strcpy(buf+currbuf, inbuf);
				currbuf += len;
				partial= (len==MAXLINLEN&&inbuf[len-1]!='\n') ? TRUE : FALSE;
#ifdef DEBUG2
				do_debug("changing partial to %s\n", (partial == TRUE) ? "TRUE" : "FALSE" );
#endif
			}
		}
	}
	return buf;
}
/*-------------------------------------------------------------------------*/
/* chk_msg_kill - return TRUE if kill article, FALSE if keep                   */
/* if kill article, add it to killlog 					   */
int chk_msg_kill(PKillStruct killp, char *headerbuf) {

	int why, goodwhy, killyn, i, del, keep, match;
	const char *group = "Master";
	FILE *fptr;

	goodwhy = why = REASON_NONE;

	/* first check against master delete */
	if((killyn = check_a_group(&(killp->master), headerbuf, &why)) == FALSE) {
		/* okay now have to parse group line */
		/* then check to see if I have group keep/deletes for each group */
		/* default actions */
 		keep = FALSE;
		del = FALSE;
		for(i=0;i<killp->totgrps;i++) {
			if(check_line(headerbuf, "Newsgroups: ", killp->grps[i].group, ',', CHECK_EXACT, ' ') == TRUE) {
				/* bingo this article matches one of our group check it */
				match = check_a_group(&(killp->grps[i].match), headerbuf, &why);
				if(killp->grps[i].delkeep == DELKEEP_KEEP) {
					/* matched keep group */
					if(match == TRUE) {
						keep = TRUE;
					}
					else {
						del = TRUE;
						group = killp->grps[i].group;
						goodwhy = REASON_NOKEEP;
					}
				}
				else {
					if(match == TRUE) {
						del = TRUE;
						goodwhy = why;
						group = killp->grps[i].group;
					}
					else {
						keep = TRUE;
					}
				}
			}
		}
		/* now determine if we kill or keep this sucker */
		if(keep == FALSE && del == FALSE) {
			/* no group matches, keep it */
			killyn = FALSE;
		}
		else if(keep != del) {
			/* only matched one group, figure out which */
			killyn = ( del == TRUE) ? TRUE : FALSE;
			why = goodwhy;
		}
		else {
			/* matched both, use TIEBREAKER */
			why = REASON_TIE;
#ifdef KILL_TIEBREAKER_KEEP
			killyn = FALSE;
#else
			killyn = TRUE;
#endif
		}
	}
	if(killyn == TRUE && killp->logyn == TRUE) {
		/* log it */
		if((fptr = fopen(full_path(FP_GET, FP_TMPDIR, N_KILLLOG), "a")) == NULL) {
			MyPerror(killf_phrases[3]);
		}
		else {
			print_phrases(fptr, killf_phrases[4], group, killf_reasons[why], headerbuf, NULL);
			fclose(fptr);
		}
	}
	return killyn;			
}
/*-----------------------------------------------------------------------*/
int check_a_group(POneKill killp, char *headerbuf, int *why) {

	int i, match = FALSE;
	char *startline, *tptr;
	
	/* check hilines first */
	if(killp->hilines > 0) { 
		if((startline = strstr(headerbuf, Params[PARAM_HILINE].header)) != NULL)  {
			i = 0;	/* just in case */
			sscanf(startline+Params[PARAM_HILINE].headerlen, "%d", &i);
#ifdef DEBUG2
			do_debug("Article has %d lines, hilines = %d\n", i, killp->hilines);
#endif
			if(killp->hilines < i) {
				/* JACKPOT */
				match = TRUE;
				*why = REASON_TOOMANYLINES;
			}
		}
		
	}
	/* now check low lines */
	if(match == FALSE && killp->lowlines > 0) { 
		if((startline = strstr(headerbuf, Params[PARAM_LOWLINE].header)) != NULL)  {
			i = 0;	/* just in case */
			sscanf(startline+Params[PARAM_LOWLINE].headerlen, "%d", &i);
#ifdef DEBUG2
			do_debug("Article has %d lines, lowlines = %d\n", i, killp->lowlines);
#endif
			if(i < killp->lowlines) {
				/* JACKPOT */
				match = TRUE;
				*why = REASON_NOTENUFLINES;
			}
		}
		
	}
	/* now check nrgrps */
	if(match == FALSE && killp->maxgrps > 0) {
		if((startline = strstr(headerbuf, Params[PARAM_NRGRPS].header)) != NULL) {
			/* count the nr of commas in the group line */
			i = 1;	/* have at least one group */
			tptr = startline;
			while(i <= killp->maxgrps && *tptr != '\n' && *tptr != '\0' ) {
				if(*tptr++ == COMMA) {
					i++;
				}
			}
			if(i > killp->maxgrps) {
				match = TRUE;
				*why = REASON_NRGRPS;
			}		
		}
	}
	/* check host */
#ifdef USE_REGEX
	if(match == FALSE && killp->path.ptrs != NULL) {
		if(regex_check(headerbuf, Params[PARAM_PATH].header, &(killp->path)) == TRUE) {
#else
	if(match == FALSE && killp->path != NULL) {
		if(check_line(headerbuf, Params[PARAM_PATH].header, killp->path, killp->pathhost_sep, CHECK_CHECK, killp->quote) == TRUE) {
#endif
			/* jackpot */
			match = TRUE;
			*why = REASON_PATHHOST;
		}
	}
	/* check from */
#ifdef USE_REGEX
	if(match == FALSE && killp->from.ptrs != NULL) {
		if(regex_check(headerbuf, Params[PARAM_FROM].header, &(killp->from)) == TRUE) {
#else
	if(match == FALSE && killp->from != NULL) {
		if(check_line(headerbuf, Params[PARAM_FROM].header, killp->from, killp->from_sep, CHECK_CHECK, killp->quote) == TRUE) {
#endif
			/* jackpot */
			match = TRUE;
			*why = REASON_FROM;
		}
	}
	/* check subject */
#ifdef USE_REGEX
	if(match == FALSE && killp->subj.ptrs != NULL) {
		if(regex_check(headerbuf, Params[PARAM_SUBJ].header, &(killp->subj)) == TRUE) {
#else
	if(match == FALSE && killp->subj != NULL) {
		if(check_line(headerbuf, Params[PARAM_SUBJ].header, killp->subj, killp->subject_sep, CHECK_CHECK, killp->quote) == TRUE) {
#endif
			/* jackpot */
			match = TRUE;
			*why = REASON_SUBJECT;
		}
	}
	/* check NNTP_Posting_Host */
#ifdef USE_REGEX
	if(match == FALSE && killp->nntphost.ptrs != NULL) {
		if(regex_check(headerbuf, Params[PARAM_HOST].header, &(killp->nntphost)) == TRUE) {
#else
	if(match == FALSE && killp->nntphost != NULL) {
		if(check_line(headerbuf, Params[PARAM_HOST].header, killp->nntphost, killp->nntp_sep, CHECK_CHECK, killp->quote) == TRUE) {
#endif
			/* jackpot */
			match = TRUE;
			*why = REASON_NNTPHOST;
		}
	}
	return match;
}
#ifndef USE_REGEX
/*-----------------------------------------------------------------------*/
char *malloc_line(int which, const char *linein) {

	/* malloc and copy a header line WITHOUT the header field */
	char *ptr;
	size_t x;

				       /* len = how much not to malloc */
	if((ptr = malloc((strlen(linein)+1) - Params[which].len)) == NULL) {
		error_log(ERRLOG_REPORT, killf_phrases[0], NULL);
	} 
	else {
		strcpy(ptr, &linein[Params[which].len]);
		/* strip the newline, if present */
		x = strlen(ptr);
		if(ptr[x-1] == '\n') {
			ptr[x-1] = '\0';
		}
	}
	return ptr;
}
#endif /* USE_REGEX */
/*--------------------------------------------------------------------------*/
int check_line(char *header, const char *whichline, const char *linematch, int separator, int check_quote, char quote_char) {
	/* search for each item in linematch on whichline in the header */
	/* if check_quote = CHECK_CHECK check if first char is char_quote */
	/* if it is, then do strcmp, else do a strncmp */
	/* if check_quote = CHECK_EXACT don't check first char just strcmp */

	char *startline, *endline, *nextcomma;
	const char *currcomma;
	int casecmp, match = FALSE;
	
	if((startline = strstr(header, whichline)) != NULL) {
		currcomma = linematch;	/* start us off */
		endline = strchr(startline, '\n');	
		/* end this line, so we only search the right header line */
		if(endline != NULL) {
			*endline = '\0';
		}
		do { 
			nextcomma = strchr(currcomma, separator);
			if(nextcomma != NULL) {
				*nextcomma = '\0';	/* null terminate current entry in linematch */
			}
#ifdef DEBUG2
			do_debug("Checking %s for \"%s\"\n", whichline, currcomma);
#endif
			/* now set the case comparison flag for this string */
			casecmp = (check_quote == CHECK_EXACT) ? TRUE : FALSE;
			if(check_quote == CHECK_CHECK) { 
				if(*currcomma == quote_char) {
					casecmp = TRUE;
					currcomma++;
				}
			}
			if(casecmp == TRUE) {
				if(strstr(startline, currcomma) != NULL) {
					/* jackpot */
					match = TRUE;
				}
			}
			else if(strnstr(startline, currcomma) != NULL) {
				match = TRUE;
			}
			if(nextcomma != NULL) {
				*nextcomma = separator;		/* must restore */
				currcomma = nextcomma+1;	/* check next +1 to move past comma */
			}
		}
		while(nextcomma != NULL && match == FALSE);
		if(endline != NULL) {	/* restore previously nuked nl */
			*endline ='\n';
		}
	}
	return match;
}
/*-------------------------------------------------------------------------------*/
int parse_a_file(const char *fname, const char *group, POneKill mykill) {

	FILE *fptr;
	char buf[MAXLINLEN+1];
	int i;
	int retval = RETVAL_OK;

	/* first initialized the killstruct */
	mykill->hilines = mykill->lowlines = mykill->maxgrps = 0;
	mykill->pathhost_sep = DEFAULT_PATHHOST_SEP;
	mykill->subject_sep = DEFAULT_SUBJECT_SEP;
	mykill->from_sep = DEFAULT_FROM_SEP;
	mykill->nntp_sep = DEFAULT_NNTP_SEP;

#ifndef USE_REGEX
	mykill->path = mykill->from = mykill->subj = NULL;
#else
	mykill->path.ptrs = mykill->from.ptrs = mykill->subj.ptrs = mykill->nntphost.ptrs = NULL;
#endif

	mykill->quote = KILLFILE_QUOTE;

	/* now read in the killfile and parse it */
	if((fptr = fopen(full_path(FP_GET, FP_DATADIR, fname), "r")) == NULL) {
		MyPerror(full_path(FP_GET, FP_DATADIR, fname));
		retval = RETVAL_ERROR;
	}
	else {
#ifdef DEBUG2
		do_debug("Opening kill file: %s\n", full_path(FP_GET, FP_DATADIR, fname));
#endif

		while(fgets(buf, MAXLINLEN, fptr) != NULL) {
#ifdef DEBUG2
			do_debug("Read kill file line: %s\n", buf);
#endif
			buf[MAXLINLEN] = '\0';	/* just in case */

			for(i = 0 ; i < NR_PARAMS; i++) {
				if(strncmp(buf, Params[i].name, (size_t) Params[i].len) == 0) {
					switch(i) {
					  case PARAM_HILINE:
						(void) sscanf(&buf[Params[PARAM_HILINE].len], "%d", &(mykill->hilines));
#ifdef DEBUG2
						do_debug("Killfile hilines = %d\n", mykill->hilines);
#endif
						break;
					  case PARAM_LOWLINE:
						(void) sscanf(&buf[Params[PARAM_LOWLINE].len], "%d", &(mykill->lowlines));
#ifdef DEBUG2
						do_debug("Killfile lowlines = %d\n", mykill->lowlines);
#endif
						break;
					  case PARAM_NRGRPS:
						(void) sscanf(&buf[Params[PARAM_NRGRPS].len], "%d", &(mykill->maxgrps));
#ifdef DEBUG2
						do_debug("Killfile maxgrps = %d\n", mykill->maxgrps);
#endif
						break;						
					  case PARAM_PATH:
#ifdef USE_REGEX
						regex_scan(&(mykill->path), buf, mykill->pathhost_sep, mykill->quote);
#else
						if((mykill->path = malloc_line(PARAM_PATH, buf)) == NULL) {
							error_log(ERRLOG_REPORT, killf_phrases[6], group, NULL);
						}
#ifdef DEBUG2 
						else {
							do_debug("Killfile path = %s\n", mykill->path);
						}
#endif /* DEBUG2 */
#endif /* USE_REGEX */
						break;
					  case PARAM_FROM:
#ifdef USE_REGEX
						regex_scan(&(mykill->from), buf, mykill->from_sep, mykill->quote);
#else

						if((mykill->from = malloc_line(PARAM_FROM, buf)) == NULL) {
							error_log(ERRLOG_REPORT, killf_phrases[7], group, NULL);
						}
#ifdef DEBUG2 
						else {
							do_debug("Killfile from = %s\n", mykill->from);
						}
#endif /* DEBUG2 */
#endif /* USE_REGEX */
						break;
					  case PARAM_SUBJ:
#ifdef USE_REGEX
						regex_scan(&(mykill->subj), buf, mykill->subject_sep, mykill->quote);
#else
						if((mykill->subj = malloc_line(PARAM_SUBJ, buf)) == NULL) {
							error_log(ERRLOG_REPORT, killf_phrases[8], group, NULL);
						}
#ifdef DEBUG2 
						else {
							do_debug("Killfile Subject = %s\n", mykill->subj);
						}
#endif /* DEBUG2 */
#endif /* USE_REGEX */
						break;
					  case PARAM_HOST:
#ifdef USE_REGEX
						regex_scan(&(mykill->nntphost), buf, mykill->nntp_sep, mykill->quote);
#else
						if((mykill->nntphost = malloc_line(PARAM_HOST, buf)) == NULL) {
							error_log(ERRLOG_REPORT, killf_phrases[9], group, NULL);
						}
#ifdef DEBUG2 
						else {
							do_debug("Killfile NNTP Host = %s\n", mykill->nntphost);
						}
#endif /* DEBUG2 */
#endif /* USE_REGEX */
						break;

					  case PARAM_QUOTE:
						if(buf[Params[PARAM_QUOTE].len] == '\0' || buf[Params[PARAM_QUOTE].len] == '\n') {
							error_log(ERRLOG_REPORT, "%s %s %s\n", killf_phrases[10], NULL);
						}
						else {
							mykill->quote = buf[Params[PARAM_QUOTE].len];
#ifdef DEBUG2
							do_debug("Killfile Quote = '%c'\n", mykill->quote);
#endif
						}
						break;
					  case PARAM_PATHHOST_SEP:
						if(buf[Params[PARAM_PATHHOST_SEP].len] == '\0' || buf[Params[PARAM_PATHHOST_SEP].len] =='\n') {
							error_log(ERRLOG_REPORT, killf_phrases[11], NULL);
						}
						else {
							mykill->pathhost_sep = buf[Params[PARAM_PATHHOST_SEP].len];
#ifdef DEBUG2
							do_debug("Killfile pathhost_sep = '%c'\n", mykill->pathhost_sep);
#endif
						}
						break;
						break;
					  case PARAM_SUBJECT_SEP:
						if(buf[Params[PARAM_SUBJECT_SEP].len] == '\0' || buf[Params[PARAM_SUBJECT_SEP].len] =='\n') {
							error_log(ERRLOG_REPORT, killf_phrases[12],NULL);
						}
						else {
							mykill->subject_sep = buf[Params[PARAM_SUBJECT_SEP].len];
#ifdef DEBUG2
							do_debug("Killfile subject_sep = '%c'\n", mykill->subject_sep);
#endif
						}
						break;
					  case PARAM_FROM_SEP:
						if(buf[Params[PARAM_FROM_SEP].len] == '\0' || buf[Params[PARAM_FROM_SEP].len] =='\n') {
							error_log(ERRLOG_REPORT, killf_phrases[13], NULL);
						}
						else {
							mykill->from_sep = buf[Params[PARAM_FROM_SEP].len];
#ifdef DEBUG2
							do_debug("Killfile from_sep = '%c'\n", mykill->from_sep);
#endif
						}
						break;
					  case PARAM_NNTP_SEP:
						if(buf[Params[PARAM_NNTP_SEP].len] == '\0' || buf[Params[PARAM_NNTP_SEP].len] =='\n') {
							error_log(ERRLOG_REPORT, killf_phrases[14],NULL);
						}
						else {
							mykill->nntp_sep = buf[Params[PARAM_NNTP_SEP].len];
#ifdef DEBUG2
							do_debug("Killfile nntp_sep = '%c'\n", mykill->nntp_sep);
#endif
						}
						break;
					  default:	/* ignore group  and program lines, those processed elsewhere */
						break;
					  
					}
				}
			}
		}
		(void) fclose(fptr);
	}
	return retval;
}
/*---------------------------------------------------------------------------------------------*/
const char *strnstr(const char *linein, const char *matchstr) {

	/* see if matchstr exists in inline, case insensitive */
	/* return start of string in inline, else return NULL */

	const char *tptr, *startchar, *retstr = NULL;
	
	if(linein != NULL && matchstr != NULL) {
		while(*linein != '\0' && retstr == NULL) {
			/* first see if I can find the first char */
			while(*linein != '\0' && tolower(*linein) != tolower(*matchstr)) {
				linein++;
			}
			if(*linein != '\0') {
				/* bingo */
				startchar = linein;
				/* now try to match rest of string */
				tptr = matchstr;
				while(tolower(*startchar) == tolower(*tptr) && *tptr != '\0') {
					startchar++;
					tptr++;
				}
				if(*tptr == '\0') {
					/* bingo, we have a match */
					retstr =  linein;
				}
				else {
					/* no match */
					linein++;
				}
			}		
		}
	}
	return retstr;
}
#ifdef USE_REGEX
/*----------------------------------------------------------------------------------*/
/* scan a line and build an array of compiled regex pointers to call regexex() with */
/* ---------------------------------------------------------------------------------*/
void regex_scan(pmy_regex which, char *linein, char separator, char quotechar) {

	regex_t *retval = NULL;
	char *tptr, *startline, errmsg[256];
	int err, i, flags, nr = 0;

	if(linein != NULL) {
#ifdef DEBUG2
		do_debug("String to parse =%s", linein);
#endif
		/* first find the = in the line to skip the parameter part */
		tptr = linein;
		while(*tptr != '\0' && *tptr != '=') {
			tptr++;
		}
		startline = tptr+1;
		/* now count each of em, so that can alloc my array of regex_t */
		if(*tptr != '\0') {
			nr = 1;
			while(*tptr != '\0') {
				if(*tptr++ == separator) {
					nr++;
				}
			}
			/* now alloc memory */
			if((retval = calloc(sizeof(regex_t), nr)) == NULL) {
				error_log(ERRLOG_REPORT, killf_phrases[5], NULL);
			}
			else {
				/* now for each string, do the regcomp */
				for(i=0;i<nr;i++) {
					tptr = startline;
					while(*tptr != '\0' && *tptr != '\n' && *tptr != separator) {
						tptr++;
					}
					/* at end of separator */
					*tptr++ = '\0';	/* so can call regcomp() correctly */
							/* the ++ so next one set up correctly */
					if(*startline == quotechar) {
						startline++;
						flags = REG_NOSUB;
					}
					else {
						flags = REG_NOSUB | REG_ICASE;
					}
#ifdef DEBUG2
					do_debug("regcomping=%s, flags=%d\n", startline, flags);
#endif
					if((err = regcomp(&(retval[i]), startline, flags)) != 0) {
						/* whoops */
						regerror(err, &(retval[i]), errmsg, sizeof(errmsg));
						error_log(ERRLOG_REPORT, "%s\n", errmsg);
					}
					startline=tptr;		/* so at start of next one */
				}
			}
		}			
	}
	which->ptrs = retval;
	which->nr = nr;
}
/*------------------------------------------------------------------------------------*/
int regex_check(char *headerbuf, const char *whichline, pmy_regex expr) {
	int i, match = FALSE;
	char *startline, *endline;

	if((startline = strstr(headerbuf, whichline)) != NULL) {
		endline = strchr(startline, '\n');	
		/* end this line, so we only search the right header line */
		if(endline != NULL) {
			*endline = '\0';
		}
		for(i = 0; i < expr->nr && match == FALSE; i++) {
			if(regexec(&(expr->ptrs[i]), startline, 0, NULL, 0) == 0) {
				match = TRUE;
			}
		}
		/* put the nl back on that we deleted */
		if(endline != NULL) {
			*endline = '\n';
		}	
	}
	return match;
}
#endif

