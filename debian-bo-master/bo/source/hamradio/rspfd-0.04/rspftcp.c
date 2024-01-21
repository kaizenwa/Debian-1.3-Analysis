/*
 * Takes udp requests and processes them
 */
#include <arpa/inet.h> 
#include <linux/ax25.h> 
#include <linux/if.h>
#include <netinet/in.h> 
#include <sys/socket.h> 
#include <sys/time.h>
#include <sys/types.h> 
#include <ctype.h>
#include <netdb.h>
#include <stdio.h>
#include <syslog.h>
#include <string.h>
#include <unistd.h>

#include "rspfax25.h"
#include "rspftcp.h"
#include "queue.h"
#include "rspfd.h"
#include "rspfif.h"

extern int errno;
extern char *sys_errlist[];
extern struct queue *routerq;
extern struct queue *adj_queue;
extern struct queue *linkq;
extern struct queue *ifqueue;
extern struct queue *rt_queue;
extern struct queue *nodgrpq;
extern const char *StatusName[];
extern const char *version;
extern struct rspf_mib rspf_stats;
extern int debug_mode;


static int send_adjs(FILE * out);
static int send_links(FILE * out);
static int send_routers(FILE * out);
static int send_interfaces(FILE * out);
static int send_help(FILE * out);
static int send_quit(FILE * out);
static int send_stats(FILE * out);
static int send_routes(FILE * out);
static int send_nodegroups(FILE * out);

char* in_ntoa(u_long addr);

int init_tcp(int port)
{
	struct sockaddr_in asin;
	struct protoent *ppe;
	int s;
	
	bzero((char*)&asin, sizeof(asin));
	asin.sin_family = AF_INET;
	asin.sin_addr.s_addr = htonl(INADDR_ANY);
	asin.sin_port = htons(port);
	
	if ( (ppe = getprotobyname("tcp")) == 0)
	{
		syslog(LOG_DAEMON | LOG_ERR, "init_tcp(): getprotobyname failed (%m)");
		return -1;
	}
	
	if ( (s= socket(PF_INET, SOCK_STREAM, ppe->p_proto)) < 0)
	{
		syslog(LOG_DAEMON | LOG_ERR, "init_tcp(): socket failed (%m)");
		(void)close(s);
		return -1;
	}
	
	if (bind(s, (struct sockaddr *)&asin, sizeof(asin)) < 0)
	{
		syslog(LOG_DAEMON | LOG_ERR, "init_tcp(): bind failed (%m)");
		return -1;
	}
	listen(s, 3);	
	
	return s;

}

struct tcp_commands {
	const char *	name;
	int		(*function)(FILE * out);
};

static const struct tcp_commands commands[] = {
	{ "adj",	send_adjs },
	{ "group",	send_nodegroups },
	{ "?",		send_help },
	{ "help",	send_help },
	{ "iface",	send_interfaces },
	{ "link",	send_links },
	{ "quit",	send_quit },
	{ "router",	send_routers },
	{ "route",	send_routes },

	{ "stat",	send_stats },
	{ 0,		0	}
};


int do_tcp(FILE * out, char *inbuf)
{
	const struct tcp_commands * c = commands;
	int			status = 1;
	char *			s = inbuf;

	/* Get rid of line-termination characters. */
	while ( *s != '\0' && *s != '\r' && *s != '\n' )
		s++;
	*s = '\0';

	if (debug_mode)
		printf("do_tcp(): Processing command: '%s'.\n", inbuf);
	
	while ( c->name != 0 ) {
		if ( strncasecmp(inbuf, c->name, strlen(c->name)) == 0 ) {
			status = (*(c->function))(out);
			break;
		}
		c++;
	}

	if ( c->name == 0 )
		fprintf(out, "Type \"help\" for instructions.\r\n");

	if ( status > 0 )
		fprintf(out, "> ");

	fflush(out);
	return status;
}
	
static int send_adjs(FILE * out)
{
	struct rspf_adj *ptr;
	char tbuf[30];
	time_t elapsed;
	qmark adj_qm;
	
	ptr = (struct rspf_adj*)qmove_first(adj_queue, &adj_qm);

	fprintf(out, "Callsign IP Address       Port seq #  %% hrd Lastheard\r\n");
      	while(ptr != NULL) 
      	{
		if (ptr->rrhtime != 0) 
      		{
			elapsed = time(NULL) -  ptr->rrhtime;
			strftime(tbuf, 30, "%a %H:%M:%S", localtime(&(ptr->rrhtime)));
		} else
			strcpy(tbuf, "Not Router");
		fprintf(out,"%8s %15s %5s %#6x %3d %14s %10s\r\n", ax25_ntoa(&ptr->dladdr), in_ntoa(ptr->addr), ptr->port, ptr->tx_pkts, ptr->rx_ratio, tbuf, StatusName[ptr->status]);
		
		ptr = (struct rspf_adj*)qmove_next(adj_queue,&adj_qm);
	}
	return 1;
} /* print_adjs */	

static int send_links(FILE * out)
{
	struct link *ptr;
	qmark link_qm;
	
	ptr = (struct link*)qmove_first(linkq, &link_qm);
	fprintf(out, "Link States\r\nSource Address   Destinatn Address\tCost Horizon Seq #\r\n");
	while (ptr != NULL)
	{
		fprintf(out, "%15s->", in_ntoa(ptr->saddr));
		fprintf(out, "%15s/%2d\t%4d (%4d)  %5u\r\n",
			in_ntoa(ptr->daddr), ptr->sigbits, ptr->cost, ptr->horizon, ptr->seq_no);
		ptr = (struct link*)qmove_next(linkq, &link_qm);
	}
	return 1;
} /* send_links */

static int send_routers(FILE * out)
{
	struct router *rtr;
	qmark rt_qm;
	char tbuf[30];
	
	fprintf(out, "Router List\r\nIP Address Sequence sub-seq  Bull Time\r\n");
	rtr = (struct router*)qmove_first(routerq, &rt_qm);
	while(rtr != NULL)
	{
		strftime(tbuf, 30, "%a %H:%M:%S", localtime(&(rtr->bulltime)));
		fprintf(out, "%15s %4u %3u %s\r\n", in_ntoa(rtr->addr), rtr->seq_no, rtr->sub_seq_no, tbuf);
		rtr = (struct router*)qmove_next(routerq, &rt_qm);
	}
	return 1;
} /*send routers */

static int send_help(FILE * out)
{
	fprintf(out, "Valid commands are;\r\n");
	fprintf(out,
	 "  adjs, groups, help, ifaces, links, routes, routers, stats.\r\n");
	return 1;
}

static int send_quit(FILE * out)
{
	fprintf(out, "Goodbye!\r\n");
	return -1;
}

static int send_interfaces(FILE * out)
{
	struct rspf_if *ifptr;
	qmark if_qm;
	int adjcnt;
	struct rspf_adj *aptr;
	qmark adj_qm;
	
	
	fprintf(out, "Interfaces\r\n   Name    Cost Nodes\r\n");
	ifptr = (struct rspf_if*)qmove_first(ifqueue, &if_qm);
	while(ifptr != NULL)
	{
		adjcnt = 0;
		aptr = (struct rspf_adj*)qmove_first(adj_queue, &adj_qm);
		while(aptr != NULL)
		{
			if (strcmp(aptr->port, ifptr->name) == 0)
				adjcnt++;
			aptr = (struct rspf_adj*)qmove_next(adj_queue, &adj_qm);
		}
		fprintf(out, "%10s %4d %4d\r\n", ifptr->name, ifptr->cost, adjcnt);
		ifptr = (struct rspf_if*)qmove_next(ifqueue, &if_qm);
	}
	return 1;
}

static int send_stats(FILE * out)
{
	fprintf(out, "System Statistics\r\nIncoming              \t Outgoing\r\n");
	fprintf(out, "Messages       %6lu \t %6lu\r\n", rspf_stats.rspfInMsgs, rspf_stats.rspfOutMsgs);
	fprintf(out, "Rtr-Rtr Hellos %6lu \t %6lu\r\n", rspf_stats.rspfInRrhs, rspf_stats.rspfOutRrhs);
	fprintf(out, "Rtr Envelopes  %6lu \t %6lu\r\n", rspf_stats.rspfInRouteEnvs, rspf_stats.rspfOutRouteEnvs);
	fprintf(out, "Header Errors  %6lu\r\n", rspf_stats.rspfInHdrErrors);
	fprintf(out, "Unknown Types  %6lu\r\n", rspf_stats.rspfInUnknownTypes);
	fprintf(out, "Not RSPF iface %6lu\r\n", rspf_stats.rspfInNotIfaces);
	fprintf(out, "RRH Timer     %6lu\tSus Pings     %6lu\r\n", rspf_stats.rspfRrhTimer,  rspf_stats.rspfSusPings);
	fprintf(out, "Sus Timeout   %6lu\tPing Timer    %6lu\tBullitn Timer %6lu\r\n", rspf_stats.rspfSusTimeout, rspf_stats.rspfPingTimer, rspf_stats.rspfBullTimer);
	fprintf(out, "Bull Timeout  %6lu\r\n", rspf_stats.rspfBullTimeout);
	fprintf(out, "Current Adjs  %6lu\tCurrent Iface %6lu\r\n", rspf_stats.rspfCurrAdjacencies, rspf_stats.rspfCurrIfaces);
	fprintf(out, "Envelope No   %6u\tSequence No   %5u\tSub-Seq No    %6u\r\n", rspf_stats.EnvelopeNumber, rspf_stats.SequenceNumber, rspf_stats.SubSequenceNumber);
	fprintf(out, "Link Horizon  %6u\tGroup Horizon %6u\tLocal Horizon %6u\r\n", rspf_stats.rspfLinkHorizon, rspf_stats.rspfGroupHorizon, rspf_stats.rspfLocalHorizon);
	fprintf(out, "Port Horizon  %6u\r\n", rspf_stats.rspfPortableHorizon);
	return 1;
}

static int send_routes(FILE * out)
{
	struct rspf_route *rt;
	qmark rtqm;
	
	fprintf(out, "Routes\r\n");
	fprintf(out, "Destination    Sigbits  Cost  Iface\r\n");
	rt = (struct rspf_route*)qmove_first(rt_queue, &rtqm);
	while(rt != NULL)
	{
		fprintf(out, "%15s %7u  %4u  %5s\r\n", in_ntoa(rt->addr), rt->sigbits, rt->cost, rt->port);
		rt = (struct rspf_route*)qmove_next(rt_queue, &rtqm);
	}
	return 1;
}

static int send_nodegroups(FILE * out)
{
	struct nodegroup *ng;
	qmark ngqm;
	
	fprintf(out, "Node Groups\r\n");
	fprintf(out, "Group               Cost  Iface\r\n");
	ng = (struct nodegroup*)qmove_first(nodgrpq, &ngqm);
	while(ng != NULL)
	{
		fprintf(out, "%15s/%2u  %4u  %s\r\n", in_ntoa(ng->addr), ng->sigbits, ng->cost, ng->iface);
		ng = (struct nodegroup*)qmove_next(nodgrpq, &ngqm);
	}
	return 1;
}
