/*  VER 075  TAB P   $Id: pull.c,v 1.4 1996/11/22 12:31:52 src Exp $
 *
 *  pull news	   
 *
 *  copyright 1996 Egil Kvaleberg, egilk@sn.no
 *  the GNU General Public License applies
 */

#include "common.h"
#include "proto.h"
#include "news.h"

/* BUG: must delete intmpname in case of error/interrupt etc. */
/* BUG: have NNTP timeout function with unlimited line length */

/* 
 *  globals for pull_group
 */
char intmpname[PATH_MAX];

static FILE *pull_spoolfp = 0;
static int sequence = 0;

extern long bytes_in_spool; /* external: number of bytes read */

/*
 *  open spool file
 */
static int open_spool(void)
{
    if (!pull_spoolfp) {
	/* temporary name */
	build_filename(intmpname,spooldir,IN_COMING,_IN_TMP,spoolname,NULL);

	/* open temporary spool file proper */
	if ((pull_spoolfp = fopen(intmpname,"w")) == NULL) {
	    log_msg(L_ERRno,"cannot open incoming spool '%s'", intmpname);
	    return 0;
	}
	log_msg(L_DEBUG3,"opened incoming spool: %s",intmpname);
	bytes_in_spool = 0;
    }
    return 1;
}

/*
 *  flush spool file
 */
static int flush_spool(void)
{
    char inspoolname[PATH_MAX];
    FILE *f;
    char nambuf[50];

    /* finished of spooled batch */
    if (pull_spoolfp) {
	progtitle("pull: flush spool");
	f = pull_spoolfp;
	pull_spoolfp = 0;
	/* BUG: errors? */
	if (fclose(f) == EOF) {
	    log_msg(L_ERRno,"error writing '%s'", intmpname);
	    unlock_exit(6);
	}

	/* spool file is now ready for newsrun */
	/* rename it so that it becomes visible */
	sprintf(nambuf,_INSPOOL,time((time_t *)0),getpid(),++sequence);
	build_filename(inspoolname,spooldir,IN_COMING,nambuf,NULL);
	if (rename(intmpname,inspoolname) == EOF) {
	    log_msg(L_ERRno,"cannot rename spool %s to %s",	
				intmpname, inspoolname);
	    unlink(intmpname); 
	    unlock_exit(6);
	}
    }
    return 1;
}

/*
 *  pull newsgroup
 *  return false if no point in continuing 
 */
static int pull_group(char *group, long *wherep)
{
    int more = 1;
    char *p;
    FILE *f;
    long first,last;
    long where;

    if (noaction_flag) return 1;

    where = *wherep;
    switch (select_group(group,&first,&last)) {
    case 1:	  /* OK */
	break;	
    case 0:	  /* no such group - continue */
	++unavailable_groups; 
	return 1;
    default:
	return 0; /* no point in continuing */
    }
    log_msg(L_DEBUG3,"group selected %s, %ld-%ld",group,first,last);
    /* BUG: check for consistency?! */

    if (zap_flag) {
	/* silently update to latest article */
	log_msg(L_DEBUG,"zap from %d to %ld in %s",where,last+1,group);
	where = last+1;
    } else {
	/* fetch articles proper */
	if (where < 0 || where < first) {
	    log_msg(L_DEBUG,"bumping from %ld to %ld in %s",where,first,group);
	    where = first;
	}
	while (where <= last) {
	    if (!pull_spoolfp && !open_spool()) return 0;
	    if (!fetch_article(where,pull_spoolfp)) {
		 more = 0;
		 break;
	    }
	    /* BUG: NOTE that 400 is used for aborting a transfer... */
	    ++where;

	    if (bytes_in_spool > minspool) {
		/* flush spool if too large */
		/* BUG: L_DEBUG3 */
		log_msg(L_DEBUG,"flushing spool...");
		flush_spool();
		bytes_in_spool = 0;
	    }
	}
    }
    /* OK, update article index */
    *wherep = where;

    return more;
}

static int pull_active;
static int pull_anything;
static FILE *pull_in;
static FILE *pull_tmp;
static char activename[PATH_MAX];
static char active_tmp[PATH_MAX];
static char active_old[PATH_MAX];

/*
 *  clean up news fetching
 *  NOTE: can be called by a signal
 */
void pull_cleanup(void)
{
    FILE *f;

    if (!pull_active) return;
    pull_active = 0;

    /* finish of spooled batch */
    flush_spool();

    progtitle("pull: cleanup");
    if (f = pull_in) {
	pull_in = 0;
	fclose(f);
    }
    if (f = pull_tmp) {
	pull_tmp = 0;
	fclose(pull_tmp);
    }

    /* clean up and update host active */
    if (pull_anything) {
	pull_anything = 0;

	if (active_old[0]) {
	    log_msg(L_DEBUG3,"renaming %s to %s",activename,active_old);
	    unlink(active_old);
	    if (rename(activename,active_old) == EOF) {
		log_msg(L_ERRno,"cannot rename host active %s to %s",
							activename,active_old);
		unlock_exit(6);
	    }
	}
	log_msg(L_DEBUG3,"renaming %s to %s",active_tmp,activename);
	if (rename(active_tmp,activename) == EOF) {
	    log_msg(L_ERRno,"cannot rename host active %s to %s", 
						    active_tmp,activename);
	    unlock_exit(6);
	}
    } else {
	log_msg(L_DEBUG,"no news is good news!");
	unlink(active_tmp);
    }
}

/*
 *  pull news 
 */
void pull(char *spoolname)
{
    char *p,*t;
    char buf[BUFSIZ];
    int skip = 0;
    long where,where0;
    char group[BUFSIZ];

    progtitle("pull: opening host active");

    pull_spoolfp = 0;
    pull_anything = 0;
    pull_active = 0;

    /* BUG: add procid or something... */
    /* BUG: make directory... */
    build_filename(activename,spooldir,IN_LATEST,spoolname,NULL);
    build_filename(active_tmp,spooldir,IN_LATEST,spoolname,_TMP,NULL);
    build_filename(active_old,spooldir,IN_LATEST,spoolname,_OLD,NULL);

    if (!(pull_in = fopen(activename,"r"))) {
	log_msg(L_ERRno,"cannot find a host active '%s'",activename);
	active_old[0] = '\0';
	/* but let us continue */
    }
    if (!(pull_tmp = fopen(active_tmp,"w"))) {
	log_msg(L_ERRno,"cannot create temporary host active '%s'",active_tmp);
	unlock_exit(6);
    }

    pull_active = 1;
    buf[BUFSIZ-1] = '\0';
    if (pull_in) {
	while (fgets(buf,BUFSIZ-1,pull_in)) {
	    progtitle("pull: reading host active");
	    p = buf;
	    while (isspace(*p)) ++p;
	    /* BUG: implement include-file mechanism... */
	    /* BUG: or have one rc per server, and let another file
		    decide which groups we'll really use! use the
		    active file as a basis for this file */
	    /* BUG: remove group duplicates... */
	    if (!*p || *p == '#') {
		/* keep comments */
		fputs(buf,pull_tmp);
	    } else if (*p == ':') {
		/* tag */
		++p;
		while (isspace(*p)) ++p;
		t = p;
		if (p=strchr(t,'\n')) *p = '\0';
		if (p=strchr(t,'\t')) *p = '\0';
		if (p=strchr(t,' ')) *p = '\0';
		if (end_tag && strcmp(t,end_tag)==0) {
		    log_msg(L_DEBUG,"end at tag '%s'",t);
		    skip = 1;
		}
		fprintf(pull_tmp,":%s\n",t);
	    } else {
		where = 0;
		if (sscanf(p,"%[^ \n\t!:] %ld",group,&where) < 1) {
		    log_msg(L_ERRno,"bad syntax in host active '%s'",buf);
		    fprintf(pull_tmp,"#ERR# %s",buf);
		} else {
		    if (!skip) {
			if (is_active(group)) {
			    where0 = where;
			    log_msg(L_DEBUG3,"pull group %s",group);
			    if (!pull_group(group,&where)) {
				fetch_aborted = 1;
				skip = 1;
			    }
			    if (where0 != where) {
				++pull_anything;
				fflush(pull_tmp);
			    }
			} else {
			    log_msg(L_DEBUG3,"group %s not active",group);
			}
		    }
		    /* updated count */
		    fprintf(pull_tmp,"%s %ld\n",group,where);
		}
	    }
	}
	fclose(pull_in);
	pull_in = 0;
    }

    /*
     * fetch the remaining previously unseen groups,
     * adding them to the host active file 
     */
    progtitle("pull: unseen groups");
    if (!skip) { 
	while (unseen_active(group)) {
	    where = 0;
	    log_msg(L_DEBUG3,"pull unseen group %s",group);
	    if (!pull_group(group,&where)) break;
	    fprintf(pull_tmp,"%s %ld\n",group,where);
	    ++unseen_groups;
	    if (where) {
		++pull_anything;
		fflush(pull_tmp);
	    }
	}
    }

    fclose(pull_tmp);
    pull_tmp = 0;

    /* finish of spooled batch */
    pull_cleanup();

    pull_active = 0;
}
