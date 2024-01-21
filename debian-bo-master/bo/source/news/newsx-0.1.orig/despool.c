/*  VER 015  TAB P   $Id: despool.c,v 0.19 1996/11/22 12:31:52 src Exp $
 *
 *  do the posting 
 *
 *  copyright 1996 Egil Kvaleberg, egilk@sn.no
 *  the GNU General Public License applies
 */

#include "common.h"
#include "proto.h"
#include "news.h"
#include <sys/stat.h>

/* 
 *  globals for despool_line
 */
char togoname[PATH_MAX];
char tempname[PATH_MAX];
char oldname[PATH_MAX];
FILE *togofp = NULL;
FILE *tempfp = NULL;
char buf[BUFSIZ];
int articles = 0;

/*
 *  despool, find next article_name
 *  return static pointer, NULL if no more
 *
 *  also handles cleanup afterwards
 */
char *despool_line(void)
{
    char *p;
    struct stat st;

    if (!togofp) {
	progtitle("lock spool");

	/* file not open yet: prepare names */
	if (spoolname[0] != '/') {
	    build_filename(togoname,spooldir,OUT_GOING,spoolname,NULL);
	} else {
	    /* assume full path is specified */
	    build_filename(togoname,spoolname,NULL);
	}

	if (inn_flag) {
	    /* INN style */

	    /* lock file first */
	    build_filename(tempname,I_LOCK,spoolname,NULL);
	    lock("",tempname);

	    /* togoname is already the filename */
	} else {
	    /* C news style */

	    /* lock file first */
	    lock(togoname,C_LOCK);

	    /* then prepare name of file */
	    build_filename(togoname,togoname,C_TOGO,NULL);
	}

	progtitle("open spool");

	build_filename(tempname,togoname,_TMP,NULL);
	build_filename(oldname,togoname,_OLD,NULL);

	/* open spool file proper */
	if ((togofp = fopen(togoname,"r")) == NULL) {
	    log_msg(L_ERRno,"cannot open spool '%s'", togoname);
	    unlock_exit(5);
	}
	/* verify that it is a regular file */
	if (fstat(fileno(togofp),&st) < 0) {
	    log_msg(L_ERRno,"cannot fstat '%s'", togoname);
	    unlock_exit(5);
	}
	if (!S_ISREG(st.st_mode)) {
	    /* BUG: both of these must be set: S_IRUSR S_IWUSR */

	    /* BUG: will usually mean we use INN flag under Cnews */
	    log_msg(L_ERR,"'%s' is not a regular file", togoname);
	    if (inn_flag && S_ISDIR(st.st_mode)) {
		/* see if Cnews spool */
		build_filename(togoname,togoname,C_TOGO,NULL);
		if (stat(togoname,&st) >= 0 && S_ISREG(st.st_mode)) {
		    log_msg(L_ERR,"you specified '-i' for a Cnews spool");
		}
	    }
	    fclose(togofp);
	    unlock_exit(5);
	}

	log_msg(L_DEBUG3,"opened spool file: %s",togoname);
    }

    /* read the next line */
    progtitle("read togo");

    while (fgets(buf,sizeof(buf),togofp)) {
	++articles;
	/* assume that filename is first, regardless */
	/* extract filename proper */
	for (p=buf; *p; ++p) {
	    if (*p == ' ') {
		*p = '\0';
		break;
	    }
	    if (*p == '\n') {
		*p = '\0';
		break;
	    }
	}
	/* if not empty, then it's kosher */
	if (buf[0]) return buf;
    } 

    /* spool file empty, clean up */
    if (fclose(togofp) == EOF) {
	log_msg(L_ERRno,"error reading '%s'", togoname);
	unlock_exit(5);
    }
    
    if (!noaction_flag && articles > 0) {
	/* something happened */
	progtitle("closing spool");

	if (tempfp && fclose(tempfp) == EOF) {
	    log_msg(L_ERRno,"error closing '%s'", tempname);
	    unlock_exit(5);
	}

	/* creating spool backup, overwriting the previous */
	if (rename(togoname,oldname) == EOF) {
	    log_msg(L_ERRno,"cannot make spool backup '%s'", oldname);
	    unlock_exit(5);
	}

	/* make new spool queue */
	if (tempfp) {
	    if (rename(tempname,togoname) == EOF) {
		log_msg(L_ERRno,"cannot rename spool %s to %s", tempname, togoname);
		unlock_exit(5);
	    }
	} else {
	    /* empty queue */
	    if ((togofp = fopen(togoname, "w")) == NULL
	      || fclose(togofp) == EOF) {
		log_msg(L_ERRno,"cannot create spool '%s'", togoname);
		unlock_exit(5);
	    }
	}

	if (!keep_old_flag) {
	    /* and remove the old one */
	    if (unlink(oldname) == EOF) {
		log_msg(L_ERRno,"cannot unlink batch file '%s'", oldname);
		unlock_exit(5);
	    }
	}
    }
    return (char *)0;
}

/*
 *  initial article_name is given
 */
void despool(char *article_name)
{
    do {
	/* post the article in question */
	if (!submit_article(article_name)) {
	    /* no success: write to temp file */

	    if (!noaction_flag) {
		if (!tempfp) {
		    /* open "temp" file */
		    if ((tempfp = fopen(tempname, "a")) == NULL) {
			log_msg(L_ERRno,"cannot open temporary '%s'",tempname);
			unlock_exit(5);
		    }
		}
    
		/* NOTE: looses rest of line -
		 * OK since probably only we'll ever see it again */
		strcat(buf,"\n");
		if (fputs(buf, tempfp) == EOF) {
		    log_msg(L_ERRno,"cannot write temporary '%s'", tempname);
		    unlock_exit(5);
		}
	    }
	}
    } while ((article_name = despool_line()));
}
