/*
 * snmptrapd.c - receive and log snmp traps
 *
 */
/***********************************************************
	Copyright 1989 by Carnegie Mellon University

                      All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of CMU not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

CMU DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
CMU BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.
******************************************************************/
#include <sys/types.h>
#include <netinet/in.h>
#include <stdio.h>
#include <sys/time.h>
#include <errno.h>
#include <syslog.h>
#ifdef linux
#include <stdlib.h>
#include <string.h>
#include <arpa/inet.h>
#endif

#include "mib.h"
#include "snmp.h"
#include "snmp_impl.h"
#include "asn1.h"
#include "snmp_api.h"
#include "snmp_client.h"

#ifndef BSD4_3

#ifndef NFDBITS
typedef long	fd_mask;
#define NFDBITS	(sizeof(fd_mask) * NBBY)	/* bits per mask */

#define	FD_SET(n, p)	((p)->fds_bits[(n)/NFDBITS] |= (1 << ((n) % NFDBITS)))
#define	FD_CLR(n, p)	((p)->fds_bits[(n)/NFDBITS] &= ~(1 << ((n) % NFDBITS)))
#define	FD_ISSET(n, p)	((p)->fds_bits[(n)/NFDBITS] & (1 << ((n) % NFDBITS)))
#define FD_ZERO(p)	bzero((char *)(p), sizeof(*(p)))
#endif

#endif

extern int  errno;
int	snmp_dump_packet = 0;
int Print = 0;

static void init_syslog();

char *
trap_description(trap)
    int trap;
{
    switch(trap){
	case SNMP_TRAP_COLDSTART:
	    return "Cold Start";
	case SNMP_TRAP_WARMSTART:
	    return "Warm Start";
	case SNMP_TRAP_LINKDOWN:
	    return "Link Down";
	case SNMP_TRAP_LINKUP:
	    return "Link Up";
	case SNMP_TRAP_AUTHFAIL:
	    return "Authentication Failure";
	case SNMP_TRAP_EGPNEIGHBORLOSS:
	    return "EGP Neighbor Loss";
	case SNMP_TRAP_ENTERPRISESPECIFIC:
	    return "Enterprise Specific";
	default:
	    return "Unknown Type";
    }
}

char *
uptime_string(timeticks, buf)
    register u_long timeticks;
    char *buf;
{
    int	seconds, minutes, hours, days;

    timeticks /= 100;
    days = timeticks / (60 * 60 * 24);
    timeticks %= (60 * 60 * 24);

    hours = timeticks / (60 * 60);
    timeticks %= (60 * 60);

    minutes = timeticks / 60;
    seconds = timeticks % 60;

    if (days == 0){
	sprintf(buf, "%d:%02d:%02d", hours, minutes, seconds);
    } else if (days == 1) {
	sprintf(buf, "%d day, %d:%02d:%02d", days, hours, minutes, seconds);
    } else {
	sprintf(buf, "%d days, %d:%02d:%02d", days, hours, minutes, seconds);
    }
    return buf;
}

int snmp_input(op, session, reqid, pdu, magic)
    int op;
    struct snmp_session *session;
    int reqid;
    struct snmp_pdu *pdu;
    void *magic;
{
    struct variable_list *vars;
    char buf[64];

    if (op == RECEIVED_MESSAGE && pdu->command == TRP_REQ_MSG){
	if (Print){
	    printf("%s: %s Trap (%d) Uptime: %s\n", inet_ntoa(pdu->agent_addr.sin_addr),
		trap_description(pdu->trap_type), pdu->specific_type, uptime_string(pdu->time, buf));
	    for(vars = pdu->variables; vars; vars = vars->next_variable)
		print_variable(vars->name, vars->name_length, vars);
	} else {
	    syslog(LOG_WARNING, "%s: %s Trap (%d) Uptime: %s\n", inet_ntoa(pdu->agent_addr.sin_addr),
		trap_description(pdu->trap_type), pdu->specific_type, uptime_string(pdu->time, buf));
	}
    } else if (op == TIMED_OUT){
	printf("Timeout: This shouldn't happen!\n");
    }

    return 0;
}


int
main(argc, argv)
    int	    argc;
    char    *argv[];
{
    struct snmp_session session, *ss;
    int	arg;
    int count, numfds, block;
    fd_set fdset;
    struct timeval timeout, *tvp;


    init_syslog();
    init_mib();
    /*
     * usage: snmptrapd [-p]
     */
    for(arg = 1; arg < argc; arg++){
	if (argv[arg][0] == '-'){
	    switch(argv[arg][1]){
		case 'd':
		    snmp_dump_packet++;
		    break;
		case 'p':
		    Print++;
		    break;
#if 1
		  case 's':
		    /* ignore */
		    break;

		  case 'v':
		    arg++;
		    /* ignore */
		    break;
#endif

		default:
		    printf("invalid option: -%c\n", argv[arg][1]);
		    printf("Usage: snmptrapd [-p] [-s] [-v 1]\n");
		    break;
	    }
	    continue;
	}
    }

    bzero((char *)&session, sizeof(struct snmp_session));
    session.peername = NULL;
    session.community = NULL;
    session.community_len = 0;
    session.retries = SNMP_DEFAULT_RETRIES;
    session.timeout = SNMP_DEFAULT_TIMEOUT;
    session.authenticator = NULL;
    session.callback = snmp_input;
    session.callback_magic = NULL;
    session.local_port = SNMP_TRAP_PORT;
    ss = snmp_open(&session);
    if (ss == NULL){
	printf("Couldn't open snmp\n");
	exit(-1);
    }

    while(1){
	numfds = 0;
	FD_ZERO(&fdset);
	block = 1;
	tvp = &timeout;
	timerclear(tvp);
	snmp_select_info(&numfds, &fdset, tvp, &block);
	if (block == 1)
	    tvp = NULL;	/* block without timeout */
	count = select(numfds, &fdset, 0, 0, tvp);
	if (count > 0){
		snmp_read(&fdset);
	} else switch(count){
	    case 0:
		snmp_timeout();
		break;
	    case -1:
		if (errno == EINTR){
		    continue;
		} else {
		    perror("select");
		}
		return -1;
	    default:
		printf("select returned %d\n", count);
		return -1;
	}
    }

    return 0;
}

static void
init_syslog(){
/*
 * These definitions handle 4.2 systems without additional syslog facilities.
 */
#ifndef LOG_CONS
#define LOG_CONS	0	/* Don't bother if not defined... */
#endif
#ifndef LOG_LOCAL0
#define LOG_LOCAL0	0
#endif
    /*
     * All messages will be logged to the local0 facility and will be sent to
     * the console if syslog doesn't work.
     */
    openlog("snmptrapd", LOG_CONS, LOG_LOCAL0);
    syslog(LOG_INFO, "Starting snmptrapd");
}
