/*  VER 104   TAB P   $Id: putarticle.c,v 0.22 1996/11/26 12:49:26 src Exp $
 *
 *  POST articles via an NNTP server
 *
 *  copyright 1996 Egil Kvaleberg, egilk@sn.no
 *  the GNU General Public License applies
 */

#include "common.h"
#include "proto.h"
#include "nntp.h"

/*
 *  local stuff
 */
#define MAILFOLDER_TAG "From "
#define MAILFOLDER_TAG_LEN 5

/* for log */
static char msgid[NNTP_STRLEN];
static char msgsender[NNTP_STRLEN];

static int article_lines = 0;
 
/*
 *  check if header tag
 */
static int is_tag(char *line, char *tag)
{
    char c,d;

    while ((c = *tag++)) {
	d = *line++;
	if (toupper(c) != toupper(d)) return 0;
    }
    return 1;
}
 
/*
 *  get current local time
 *  return static pointer
 *  "Wed Apr 10 11:00:00 1996"
 */
static char *current_time(void)
{
    static char buf[100];
    char *p;
    time_t t;

    time(&t);

    strcpy(buf,asctime(localtime(&t)));
    /* remove trailing newline */
    if ((p=strchr(buf,'\n'))) *p = '\0';

    return buf;
}

/*
 *  check out header...
 *  update skip state
 */
static void check_header(char *line, int *skip)
{
    char *p;

    switch (toupper(line[0])) {
    case 'F':
	if (is_tag(line,p="From: ")) {
	    /* use From: if no Sender: */
	    if (!msgsender[0])
		strncpy(msgsender,line+strlen(p),sizeof(msgsender)-1);
	}
	break;
    case 'M':
	if (is_tag(line,p="Message-ID: ")) {
	    /* record local Message-ID */
	    strncpy(msgid,line+strlen(p),sizeof(msgid)-1);
	    if (nomsgid_flag) {
		/* but don't transmit it */
		*skip = 1;
		return;
	    }
	}
	break;
    case 'N':
	if (is_tag(line,"NNTP-Posting-Host:")) {
	    /*
	     * when POSTing, this header may cause
	     * messages to be rejected
	     * fix by: Riku Saikkonen <rjs@isil.lloke.dna.fi>		    
	     *		       and Simon J. Mudd <sjmudd@bitmailer.net>
	     */
	    *skip = 1;
	    return;
	}
	break;
    case 'P':
	if (is_tag(line,"Path:") && !keep_path_flag) {
	    /*
	     * skip local Path when POSTing:
	     * the main reason is that the local client
	     * may not have a registered host name
	     */
	    *skip = 1;
	    return;
	}
	break;
    case 'S':
	if (is_tag(line,p="Sender: ")) {
	    /* record Sender: */
	    strncpy(msgsender,line+strlen(p),sizeof(msgsender)-1);
	}
	break;
    case 'X':
	if (is_tag(line,"Xref:")) {
	    /* skip local Xref */
	    *skip = 1;
	    return;
	}
	break;
    case ' ':
    case '\t':
	/* continued header line, maintain skip state */
	return;
    }
    /* keep header line */
    *skip = 0;
}

/*
 *  transfer an article...
 *  set static variable article_lines
 */
static int write_article(FILE *fp)
{
    char line[NNTP_STRLEN+2+1];
    int len;
    int hdr = 1;
    int skiphdr = 0;
    FILE *folder_file = 0;
    int multi_1st = 0;
    int multi_last = 1;

    msgid[0] = '\0';
    msgsender[0] = '\0';
    article_lines = 0;

    /* transfer the article, header and body */
    progtitle("post: transfer article");
    for (;;) {
	line[NNTP_STRLEN] = '\0'; 
	if (!fgets(line, NNTP_STRLEN, fp)) break;

	/* must remove trailing linefeed */
	if ((len=strlen(line)) == 0) break;

	if (line[len-1] == '\n') {
	    /* a complete line, or line termination */
	    multi_1st = multi_last; /* full line? */
	    multi_last = 1;
	    line[--len] = '\0';
	} else {
	    /*
	     * no newline detected - handle very long lines too
	     * problem pinpointed by Riku Saikkonen <rjs@isil.lloke.dna.fi>
	     */
	    multi_1st = multi_last; /* 1st part? */
	    multi_last = 0;
	}
	if (multi_1st) {
	    if (len == 0) {
		/* empty line: end of header */
		hdr = skiphdr = 0;
	    } else if (len == 1 && line[0] == '.') {
		/*
		 * posting contains EOF, so convert to something
		 * harmless according to RFC-977, section 3.10.1
		 * fix by Riku Saikkonen <rjs@isil.lloke.dna.fi>
		 */
		strcpy(line,"..");
	    } 
	    if (hdr) {
		/* update skip state */
		check_header(line, &skiphdr);
	    }
	    ++article_lines;
	}
	if (!skiphdr) {
	    if (multi_last) {
		line[len++] = '\r';
		line[len++] = '\n';
		line[len] = '\0';
	    }
	    if (!put_server_msg(line)) unlock_exit(9);
    
	    /* save to folder also */
	    if (folder) {
		if (!folder_file) {
		    if (!(folder_file = fopen(folder,"a"))) {
			log_msg(L_ERRno,"can't open folder: %s",folder);
			folder = 0;
		    } else {
			/* folder item header, as for mail folders */
			fprintf(folder_file, "%s%s %s\n",
				MAILFOLDER_TAG, spoolname, current_time());
		    }
		}
		if (folder_file) {
		    /* save to folder too */
		    if (multi_1st 
		     && strncmp(line, MAILFOLDER_TAG, MAILFOLDER_TAG_LEN)==0) {
			/* mail folder hack */
			fputc('>', folder_file);
		    }
		    if (multi_last) {
			/* back to Unix convention */
			line[len-1] = '\0';
			line[len-2] = '\n';
		    }
		    fputs(line, folder_file);
		}
	    }
	    net_bytecount += len;
	}
    }
    if (!multi_last && !skiphdr) {
	/* no trailing newline, so we need to add one */
	if (!put_server_msg("\r\n")) unlock_exit(9);
	if (folder_file) fputc('\n', folder_file);
    }

    if (folder_file) {
	/* BUG: what if posting failed... */
	/* BUG: or if noaction_flag.. */
	/* trailing blank line */
	fputc('\n',folder_file);
	fflush(folder_file);
	if (ferror(folder_file)) {
	    log_msg(L_ERRno,"error writing folder: %s",folder);
	}
	fclose(folder_file);
    }

    if (ferror(fp)) {
	/* panic */
	log_msg(L_ERRno,"error transferring article");
	unlock_exit(1);
    }

    /* send termination */
    progtitle("post: send termination");
    if (!put_server_msg(".\r\n")) unlock_exit(9);

    /* article successfully read */
    log_msg(L_DEBUG,"(%d lines)", article_lines);

    return 1;
}

/*
 *  put_article using POST
 *  return string if all right
 */
static char *put_article(FILE *fp)
{
    char status[NNTP_STRLEN+1];
    char *p;
    char *endptr;
    char *ok = 0;

    /* read status line from server */
    if (!noaction_flag) {
	if (!get_server_nntp(status, sizeof(status))) unlock_exit(9);
    } else {
	sprintf(status,"%d",CONT_POST);
    }
    switch (strtoul(status,&endptr,10)) {
    case CONT_POST:		    /* Posting allowed, continue */
	if (!write_article(fp)) return 0;
	break;

    case ERR_NOPOST:		    /* posting not allowed */
    case ERR_POSTFAIL: 
	log_msg(L_ERR,"posting prohibited: got '%s'", status);
	unlock_exit(4);

    /* otherwise must be a protocol error */
    default:
	log_msg(L_ERR,"NNTP post protocol error: got '%s'", status);
	unlock_exit(4);
    }

    /* get status of posting */
    progtitle("post: get status");
    if (!noaction_flag) {
	if (!get_server_nntp(status, sizeof(status))) unlock_exit(9);
    } else {
	sprintf(status,"%d",OK_POSTED);
    }
    switch (strtoul(status,&endptr,10)) {
    case OK_POSTED:			/* Article posted OK */
	++posted_articles;
	if (!noaction_flag)
	    ok = "OK";
	else
	    ok = "TEST";
	break;

    case ERR_NOPOST:			/* Posting not allowed */
	log_msg(L_ERR,"posting not allowed: got '%s'", status);
	unlock_exit(4);

    case ERR_GOTIT:			/* Already got it */
	/* not in RFC-977 */
	goto gotit;

    case ERR_POSTFAIL:			/* Posting failed */
	/* see if secondary error message */
	switch (strtoul(endptr,&endptr,10)) {
	case ERR_GOTIT:
	  gotit:
	    /* already gotit: we'll just continue */
	    log_msg(L_DEBUG,"duplicate");
	    ++duplicate_articles;
	    ok = "Duplicate";
	    break;

	default:
	    /* posting failed, some other reason */
	    log_msg(L_ERR,"posting failed: got '%s'", status);
	    unlock_exit(4);
	}
	break;

    default:				/* 0therwise, protocol error */
	log_msg(L_ERR,"NNTP posting protocol error: got '%s'", status);
	unlock_exit(4);
    }

    return ok;
}

/*
 *  submit an article with specified id to the server
 */
static void post_article(void)
{
    progtitle("post: issuing POST");
    put_server("POST\r\n");
}

/*
 *  as post_article, but use IHAVE mechanism 
 */
static void ihave_article(char *msgid)
{
    char request[NNTP_STRLEN+1];

    progtitle("post: issuing IHAVE");
    sprintf(request, "IHAVE %s\r\n", msgid);

    put_server(request);

    /* BUG: must find article ID first
     * BUG: reponses are:
	335	Go ahead
	400	Goodbye - you do not have transfer permission
	435	Not wanted - do not send it
	436	Try again later
	437	Rejected - do not try again
	235	Transferred OK
    */

}

/*
 *  submit articles to the currently open NNTP server socket.
 *  return string if article should be removed from outgoing batch
 */
char *submit_article(char *articlename)
{
    FILE *fp;
    char *ok;
    char fullname[PATH_MAX];

    if (articlename[0] != '/') {
	build_filename(fullname,spooldir,"/",articlename,NULL);
    } else {
	/* assume full path is specified */
	/* this is the case for INN spools */
	build_filename(fullname,articlename,NULL);
    }
    progtitle("post: reading article");
    log_msg(L_DEBUG,"reading %s", fullname);

    if ((fp = fopen(fullname, "r")) == NULL) {
	/* article is missing - throw it away */
	log_msg(L_ERRno,"cannot find article '%s'",fullname);
	++missing_articles;
	ok = "Missing";
    } else {

	/* request post */
	/* BUG: check if post_allowed? */
	post_article();

	/* and do it */
	ok = put_article(fp);
    }
    fclose(fp);

    if (ok && logfile) {
	progtitle("post: logging");
	/* BUG: inefficient */
	if (!(fp = fopen(logfile,"a"))) {
	    log_msg(L_ERRno,"can't open logfile: %s",logfile);
	} else {
	    fprintf(fp,"%s %s %s %s %s %s, %d lines\n",
			current_time() + 4, /* skip "Day " */
			spoolname,
			msgid[0] ? msgid : "<?>",
			articlename,
			msgsender[0] ? msgsender : "?",
			ok, 
			article_lines);
	    if (fclose(fp) == EOF) {
		log_msg(L_ERRno,"can't write to logfile: %s",logfile);
	    }
	}
    }

    return ok;
}

