/*  VER 151   TAB P   $Id: doit.c,v 1.19 1996/11/22 12:31:52 src Exp $
 *
 *  an NNTP news exchange client
 *
 *  copyright 1996 Egil Kvaleberg, egilk@sn.no
 *  the GNU General Public License applies
 */

#include "common.h"
#include "proto.h"
#include "nntp.h" 

#include <string.h>
#include <signal.h>

/*
 *  local functions
 */
static void statistics(time_t starttime);
static void set_signals(void);

/*
 *  do it
 */
int doit()
{
    int ret;
    time_t starttime;
    char *article_name = 0;

    progtitle("begin");
    set_signals();

    log_open();

    log_msg(L_DEBUG,"server: %s, spool: %s", hostname, spoolname);

    /* load the active file before we connect to the server */
    if (!nopull_opt) {
	load_active();
    }

    /* see if there are any articles batched up */
    if (!nopost_opt && !(article_name = despool_line())) {
	log_msg(L_DEBUG,"outgoing spool is empty");
	if (nopull_opt) {
	    /* don't bother */
	    return 0;
	}
    }

    /* set up the connection to the server */
    if (!noaction_flag) switch (ret = open_server(hostname,hostport)) {
    case -1 :
	return 3;
    case OK_CANPOST:
	++post_allowed;
	log_msg(L_DEBUG,"OK, can post");
	break;
    case OK_NOPOST:
	log_msg(L_DEBUG,"OK, but cannot post");
	break;
    default:
	log_msg(L_ERR,"can't talk to '%s': got response code %d", hostname, ret);
	return 4;
    }

    /* if authinfo details supplied, then use 'em */
    if (ai_username)
	do_authinfo(ai_username,ai_password);

    /* switch INN to nnrpd instead of innd if needed */
    if (mode_reader_flag)
	do_mode_reader();

    time(&starttime);

    /* now put the actual articles */
    if (article_name && !zap_flag)
	despool(article_name);

    /* and pull any new articles */
    if (!nopull_opt) {
	log_msg(L_DEBUG,"fetching news");
	pull(spoolname);
    }

    /* time for goodbye */
    if (!noaction_flag)
	close_server();

    statistics(starttime);

    /* reached so far: everything is all right */
    return 0;
}

/*
 *  neither lies nor damned lies
 */
static void statistics(time_t starttime)
{
    time_t endtime;

    progtitle("statistics");
    time(&endtime); 

    if (posted_articles) {
	log_msg(L_INFO,"posted %ld article%s",
			   posted_articles, posted_articles==1 ? "":"s");
    }
    if (duplicate_articles || missing_articles) {
	log_msg(L_INFO,"posting errors: %d duplicate%s, %d missing article%s",
			   duplicate_articles, duplicate_articles==1 ? "":"s",
			   missing_articles, missing_articles==1 ? "":"s");
    }
    if (fetched_articles) {
	log_msg(L_INFO,"fetched %d article%s in %ld group%s",
			   fetched_articles, fetched_articles==1 ? "":"s",
			   fetched_groups, fetched_groups==1 ? "":"s");
    }	  
    if (unavailable_groups) {
	log_msg(L_INFO,"%d group%s were not available from this server",
			   unavailable_groups, unavailable_groups==1 ? "":"s");
    }
    if (unseen_groups) {
	log_msg(L_INFO,"fetched %d group%s never seen before",
			   unseen_groups, unseen_groups==1 ? "":"s");
    }
    if (already_articles || history_articles) {
	log_msg(L_INFO,"%ld article%s already in spool, %ld fetched already",
			   history_articles, history_articles==1 ? "":"s",
			   already_articles);  
    }
    if (fetch_aborted) {
	log_msg(L_INFO,"article fetch aborted");
    }

    /* transfer speed statistics */
    if (gross_bytecount > 0) {
	log_msg(L_INFO,"connected for %lds; %ld characters transferred",
					endtime-starttime, gross_bytecount);
	if (net_bytecount > 0) {
	    log_msg(L_INFO,"average article transfer speed %ld cps",
		net_bytecount / (starttime==endtime ? 1 : endtime-starttime));
	}
    }
}

/*
 *  signal handler to report signal in log and possibly
 *  submit remaining batch to news and dump uncollected message ids.
 */
static RETSIGTYPE sig_interrupt(int signo)
{
    /* pretty primitive, but it works of sort */
    extern int alarm_active;

    if (alarm_active) {
	progtitle("interrupt -> alarm");
	log_msg(L_ERR,"received signal while alarm active %d", signo);
	/* alarm enabled, so treat it as such */
	sig_alrm(signo);
	return;
    }
    progtitle("interrupt");

    log_msg(L_ERR,"received signal: %s", strsignal(signo));

    /* clean up fetching */
    pull_cleanup();

    /* remove any locks */
    unlock_exit(1);
}

/*
 *  signal handler to ignore signal
 */
static RETSIGTYPE sig_ignore(int signo)
{
    signal(SIGUSR1, sig_ignore);
    log_msg(L_DEBUG3,"received signal: %s", strsignal(signo));
}

/*
 *  set up signal handler to catch appropriate signals.
 */
static void set_signals(void)
{
    /* these signals all causes us to terminate gracefully: */
    signal(SIGHUP,  sig_interrupt);
    signal(SIGINT,  sig_interrupt);
    signal(SIGQUIT, sig_interrupt);
    signal(SIGTERM, sig_interrupt);

    /* this signal is just to check that we are alive: */
    signal(SIGUSR1, sig_ignore);
}

