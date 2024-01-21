/*  VER 121   TAB P   $Id: getarticle.c,v 1.13 1996/11/25 05:50:35 src Exp $
 *
 *  fetch articles via an NNTP server
 *
 *  copyright 1996 Egil Kvaleberg, egilk@sn.no
 *  the GNU General Public License applies
 */

#include "common.h"
#include "proto.h"
#include "nntp.h"

long bytes_in_spool; /* external: number of bytes read */

/* 
 *  BUG: this is a real memory hog
 */
char *temp_store = 0;
long temp_len = 0;
#define TEMP_STEP 100000

static char cur_group[80]; /* for error reporting */
static int no_stat = 0; /* for hosts that lack a STAT */

/*
 *  read an article proper
 *  and feed it to the spool
 *  return false on errors that mean we should not continue
 */
static int read_article(FILE *fp)
{
    char line[NNTP_STRLEN+1];
    char buf[40];
    int newline = 1;
    long len;
    long bytecount = 0L; /* BUG: */

    line[NNTP_STRLEN] = '\0'; /* better safe than sorry */

    /* fetch the article, header and body */
    for (;;) {
	if (!get_server_msg(line, NNTP_STRLEN)) {
	    /* timeout: simply give up */
	    return 0;
	}
	len = strlen(line);
	gross_bytecount += len;

	/* end of file */
	if (newline && line[0]=='.' && (line[1]=='\r' || line[1]=='\n')) break;

	/* find and strip newlines */
	newline = 0;
	while (len > 0 && (line[len-1]=='\r' || line[len-1]=='\n')) {
	    newline=1;
	    --len;
	}
	if (bytecount+len+newline > temp_len) {
	     /* there is not enough room */
	     if (!(temp_store = realloc(temp_store,temp_len+TEMP_STEP))) {
		 log_msg(L_ERR,"Out of memory");
		 return 0;
	     }
	     temp_len += TEMP_STEP;
	}
	if (len > 0) memcpy(temp_store+bytecount,line,len);
	bytecount += len;
	if (newline) temp_store[bytecount++] = '\n';
    }
    if (bytecount > 0) {
	progtitle2(cur_group, " writing..");
	fprintf(fp,"#! rnews %ld\n",bytecount);
	fwrite(temp_store,1,bytecount,fp);
	bytes_in_spool += bytecount;
	net_bytecount += bytecount;
	++fetched_articles;
    }
    if (ferror(fp)) {
	/* panic */
	log_msg(L_ERRno,"error writing to incoming spool, group is %s",
								cur_group);
	return 0;
    }

    return 1;
}

/*
 *  fetch current article proper
 *  return false if no point in continuing
 */
static int current_article(long where, FILE *f)
{
    char request[NNTP_STRLEN+1];
    char status[NNTP_STRLEN+1];
    int ok;

    if (no_stat) {
	sprintf(request, " %ld", where);
	progtitle2(cur_group, request);

	sprintf(request, "ARTICLE %ld\r\n", where);
    } else {
	sprintf(request, "ARTICLE\r\n");
    }

    if (!put_server_msg(request)) {
	return 0;
    }

    /* get status */
    if (!get_server_nntp(status, sizeof(status))) {
	/* timeout */
	return 0;
    }
    switch (atoi(status)) {
    case OK_ARTICLE:			/* article OK */
	ok = read_article(f);
	if (ok && debug_flag == 1) {
	    /* show that something is happening */
	    fputc('.',stderr);
	    fflush(stderr);
	}
	return ok;

    case ERR_NOARTIG:			/* no such article in group */
    case ERR_NOART:			/* no such article */
	progtitle2(cur_group, ", no article");
	/* article has disappeared, ignore it */
	log_msg(L_DEBUG,"article in %s disappeared: %s",cur_group,status);
	return 1;

    case OK_HEAD:			/* not complete... */
    case OK_BODY:			/* not complete... */
    case OK_NOTEXT:			/* not complete... */
    case ERR_NCING:			/* not in group */
    case ERR_NOCRNT:			/* nothing selected */
	/* should not happen... */
    default:				/* 0therwise, protocol error */
	progtitle2(cur_group, ", error");
	log_msg(L_ERR,"NNTP article read error: got '%s'", status);
	/* stop here */
	return 0;
    }
}

/*
 *  fetch an article		
 *  return false if no point in continuing
 */
int fetch_article(long where,FILE *f)
{
    char request[NNTP_STRLEN+1];
    char status[NNTP_STRLEN+1];
    char msgid[NNTP_STRLEN+1];
    long a;
    char *endptr;

    if (!no_stat) {
	/* enquire article status and message ID */
	sprintf(request, " %ld", where);
	progtitle2(cur_group, request);

	sprintf(request, "STAT %ld\r\n", where);
    
	if (!put_server_msg(request)) {
	    return 0;
	}
    
	/* get status */
	if (!get_server_nntp(status, sizeof(status))) {
	    /* timeout */
	    return 0;
	}
    } else {
	sprintf(status,"%d",99);
    }

    switch (strtoul(status,&endptr,10)) {
    case OK_NOTEXT:			/* follows STAT... */
	/* 223 3800 <jeqk9rzgqa4.fsf@storm.stud.ntnu.no> status */
	if (sscanf(endptr,"%ld %[^ \n\t]",&a,msgid) != 2) {
	    log_msg(L_ERR,"bad STAT reponse: %s", status);
	    return 0;
	}
	if (a != where) {
	    log_msg(L_ERR,"STAT %ld out of phase: %s", where, status);
	    return 0;
	}
	/* check if already in news history database */
	if (!nohist_opt && history_lookup(msgid)) {
	    ++history_articles;
	    return 1;
	}
	/* check if read already */
	if (!new_msgid(msgid)) {
	    ++already_articles;
	    return 1;
	}
	/* BUG: have max file size?? */

	log_msg(L_DEBUG3,"fetching article %ld",where);
	return current_article(where,f);

    case ERR_COMMAND:			/* STAT is not implemented */
	progtitle2(cur_group, ", no STAT");
	log_msg(L_INFO,"server lacks STAT command: %s",status);
	no_stat = 1;
    case 99:
	log_msg(L_DEBUG3,"unconditionally fetching article %ld",where);
	return current_article(where,f);

    case ERR_NOARTIG:			/* no such article in group */
    case ERR_NOART:			/* no such article */
	progtitle2(cur_group, ", no article");
	/* article no longer there, ignore it */
	log_msg(L_DEBUG,"article %ld in %s not on server",where,cur_group);
	return 1;

    case OK_ARTICLE:			/* follows ARTICLE... */
    case OK_HEAD:			/* follows HEAD... */
    case OK_BODY:			/* follows BODY... */
    case ERR_NCING:			/* not in group */
    case ERR_NOCRNT:			/* nothing selected */
	/* should not happen... */
    default:				/* otherwise, protocol error */
	progtitle2(cur_group, ", STAT error");
	log_msg(L_ERR,"NNTP article read error: got '%s'", status);
	/* stop here */
	return 0;
    }
}

/*
 *  select a group		
 *  return 1 if OK, 0 if no group, -1 if no point in continuing
 */
int select_group(char *group,long *firstp,long *lastp)
{
    int ok = 0;
    int nstatus;
    long msgs;
    char request[NNTP_STRLEN+1];
    char status[NNTP_STRLEN+1];

    strncpy(cur_group,group,sizeof(cur_group)-1); /* for error reporting */
    progtitle(cur_group);

    sprintf(request, "GROUP %s\r\n", group);

    if (!put_server_msg(request)) {
	return -1;
    }

    /* get status */
    if (!get_server_nntp(status, sizeof(status))) {
	/* timeout */
	return -1;
    }

    switch (atoi(status)) {
    case OK_GROUP:			/* Group selected */
	/* BUG: check article name??? */
	if (sscanf(status,"%d %ld %ld %ld",&nstatus,&msgs,firstp,lastp) != 4) {
	    log_msg(L_ERR,"group select bad format: '%s'", status);
	    return -1;
	}
	++fetched_groups;
	return 1;

    case ERR_NOGROUP:			/* server haven't seen it before */
	log_msg(L_ERR,"no such group: '%s'", group);
	return 0;

    default:				/* otherwise, protocol error */
	log_msg(L_ERR,"NNTP group select protocol error: got '%s'", status);
	return -1;
    }
}
