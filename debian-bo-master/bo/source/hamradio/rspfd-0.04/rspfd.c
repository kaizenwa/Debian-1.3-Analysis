/* 
 * rspfd: Radio Shortest Path Daemon. A router for packet radio networks.
 * Copyright (C) 1995 Craig Small VK2XLZ
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/*
 * rspf_ping() and in_cksum() taken from Mike Muuss' ping.c (Public Domain)
 * rspf_check() taken from Alan Cox et.al linux kernel tcp.c's tcp_check()
 *					 (GNU-GPL)
 */
#include <linux/ax25.h>
#include <linux/if_ether.h>
#include <linux/if.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <sys/param.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <net/if_arp.h>
#include <netinet/ip_icmp.h>
#include <netinet/ip.h>
#include <fcntl.h>
#include <netdb.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>
#include <unistd.h>
#include <arpa/inet.h>

#include "rspfd.h"
#include "queue.h"
#include "deltaq.h"
#include "rspfif.h"
#include "rspfax25.h"
#include "rspftcp.h"
#include "rspf_out.h"
#include "rspfroute.h"

extern int errno;
extern char *sys_errlist[];
const char *version = 
	"RSPFd: v0.04 Craig Small (vk2xlz@gonzo.vk2xlz.ampr.org)\n";
const char *StatusName[] = { "OK", "Tentative", "Suspect", "Bad" };	

struct queue *routerq;
struct queue *adj_queue;
struct queue *pingq;
struct queue *fragq;		/* key = ipaddr, envid, frag# */
struct queue *linkq;		/* no key */
struct queue *outfragq;		/* no key */
struct queue *rt_queue;		/* no key */
struct queue *nodgrpq;		/* no key */

struct rspf_mib rspf_stats;
int debug_mode;
/* 
 * Function prototypes 	
 */
void sig_child(void);
void read_config_file();
void rspf_rx(int sig);
void recv_packet(unsigned char *buf, int bufsize, char *port);
void recv_rspf(char *data, ax25_address callsign, char *port, int length, u_long saddr, u_long daddr);
void add_fragment(u_char *data, int datalen, u_long saddr);
void recv_routeenv(u_long addr, u_short env_no);
qmark get_next_frag(u_long addr, u_short env_no, u_char frag);
void check_fragment( struct rspf_frag *(*fragment), 
	u_char *(*bptr), int length, int *fragmented, 
	qmark (*frag_qm));
void recv_ip(char* buf, ax25_address callsign, char * port, int len);
void recv_arp(u_char *data,ax25_address saddr,char *port,int datalen);
void recv_icmp(char * data, ax25_address callsign, char *port, int length, u_long saddr, u_long daddr);
void recv_rspfroute(u_short env_no);
void save_frag(u_long addr, int frag, struct rspfnode_hdr *nodehdr, struct rspflink_hdr *linkjhdr,
		int nodes, int links, int adjs, int incomplete, int env_no, int new_info, int old_info);
void recv_rspf_frag(u_long saddr, u_long daddr, char *data, int datalen, char *iface);

u_short get_rx_pkts(ax25_address callsign,  char *port);
time_t lastheard(ax25_address dladdr, char *port);
void add_nodegroup(u_long addr, u_int sigbits, u_int cost, char *iface);


/*
 * Sending routines
 */

/* Functions called by the timer events */
void rrh_timer(int id);
void reasm_timer(int id);
void check_adjacencies(int id);
void route_bull_timer(int id);
void check_routers(int id);



/* 
 * Adjacency table manipulation
 */	 
struct rspf_adj *get_adjacency(u_long addr, char *port);
void add_adjacency(u_long addr, ax25_address dladdr, char *port, u_int tx_pkts, AdjStatus status);
void del_adjacency(u_long addr, char *port);
void sus_adjacency(u_long addr, ax25_address dladdr, char *port, u_int tx_pkts);


int send_ping(u_long daddr, char *iface);
void rspf_pinger(int id);
void do_spf(void);


/* 
 * Link state table manipulation 
 */
void del_link(u_long saddr, u_long daddr, u_char sigbits);
int add_link(u_long saddr, u_long daddr, u_char sigbits, u_char horizon, u_char cost, u_short seq_no);
void del_link_source(u_long saddr, u_short env_no);
void clean_dead_links();


/*
 * in_ntoa()
 *
 * Converts, properly, IP address from internal format to dotted decimal
 *
 * Returns:
 *	A statically allocated pointer to the dotted decimal address
 *
 * Arguments:
 *	unsigned long: Net-byte ordered IP address
 *
 * Caveats:
 *	The result will be overwritten when this is next used
 */
char * in_ntoa(u_long addr)
{
	char *retval;
	struct in_addr in;
	
	in.s_addr = addr;
	retval = inet_ntoa(in);
	return retval;
}

/*
 * router_key
 *
 * Creates composite key for the queues that joinsIP address and the
 * port name together.  Put in a function to give consistency
 *
 * Returns:
 *	char*		The composite key
 *
 * Arguments:
 *	u_long		ip address of the station
 *	char*		port the station is on
 *
 * Caveats:
 *	Value returned is static variable and is overwritten at next use
 */
char * router_key(u_long addr, char* port)
{
	static char retval[15 + IFNAMSIZ];
	
	strncpy(retval, in_ntoa(addr), 15);
	strncpy(retval+ strlen(retval), port, IFNAMSIZ);
	return retval;
}

#ifdef 0
void sig_child(void)
{
	int pid;
	union wait status;
	
	while ( (pid = wait3(&status, WHNOHANG, (struct rusage*)NULL)) > 0)
		; /*nothing */
}
#endif

int	serv_skt = -1;

void sig_pipe(int sig)
{
	if ( serv_skt >= 0 ) {
		shutdown(serv_skt, 2);
		close(serv_skt);
		serv_skt = -1;
	}
}

int main(int argc, char **argv)
{
	fd_set read_fds;	
	fd_set exception_fds;	
	static unsigned char buf[512];
	int size;
	struct sockaddr sa;
	int asize=sizeof(sa);
	int i;
	int rx_skt;
	int lst_skt;
	struct sockaddr_in cli_addr;
	int clilen;
	int oldmask;
	int childpid, fd;
	int argcnt;
	char msg[300];
	FILE *	serv_stream = 0;
	

	/*
	 * Parse the command arguments
	 */
	argcnt = 1; 
	for(argcnt = 1; argcnt < argc; argcnt++ )
	{
		if (argv[argcnt][0] != '-')
		{
			fprintf(stderr, "%s: Unknown flag %s.\n", argv[0], argv[argcnt]);
			continue;
		}
		switch(argv[argcnt][1])
		{
		case 'd':
			fprintf(stderr, "%s: Debug mode set.\n", argv[0]);
			debug_mode = 1;
			break;
	
		case 'h':
			printf("%s\n",version);
			printf("RSPF daemon comes with ABSOLUTELY NO WARRANTY.\n");
			printf("This is free software, and you are welcome to\n");
			printf("redistribute it under certain conditions.\n");
			printf("----------------------------------------------\n");
			printf(" -d	Debug mode\n");
			printf(" -h	This help screen\n");
			return 0;
			break;
				
		default:
			fprintf(stderr, "%s: Unknown flag (%s).\n", argv[0], argv[argcnt]);
			continue;
		}
	}		
	/* Open a file for syslogd - in debug mode it echos to stdout too*/
	if (debug_mode)
		openlog(argv[0], LOG_PERROR, LOG_DAEMON);	
	else
		openlog(argv[0], 0, LOG_DAEMON);
		
	syslog(LOG_DAEMON | LOG_NOTICE, "RSPF started");	
	
	/* 
	 * Things to do to make a daemon process wrs:unp p82-
	 */
	 
	/* Ignore termal signals */
	signal(SIGTTOU, SIG_IGN);
	signal(SIGTTIN, SIG_IGN);
/*	signal(SIGTSTP, SIG_IGN);*/

/*	if ( (childpid = fork()) < 0)
	{
		syslog(LOG_DAEMON | LOG_ERR, "Cannot fork child");
		exit(1);
	} else if (childpid > 0)
		exit(0);*/	/* parent exiting */
		
	/* This is the child process now */
	if (setpgrp() == -1)
		syslog(LOG_DAEMON | LOG_ERR, "Cannot change process group");
	
	if ( (fd = open("/dev/tty", O_RDWR)) >= 0)
	{
		ioctl(fd, TIOCNOTTY, (char*)NULL);
		close(fd);
	}
	
	/* Close all files */
	for (fd = 3; fd < NOFILE; fd++)
		close(fd);
	errno = 0;
	
	/* Change to root directiory */
	chdir("/");
	
	/* Reset file access creation mask */
	umask(0);
		
	/* ignore anything to do with the children */
	signal(SIGCLD, SIG_IGN);

	/*
	 * Set-up the queues
	 */
	routerq = create_queue();
	adj_queue = create_queue();
	iface_setup();
	pingq= create_queue();
	fragq = create_queue();
	linkq = create_queue();
	outfragq = create_queue();
	rt_queue = create_queue();
	nodgrpq = create_queue();
	/*
	 * Setup some global variables
	 */
	rspf_stats.SequenceNumber = 0;
	rspf_stats.SubSequenceNumber = 0;

	if (debug_mode)	
		printf(version);

	
	/* Setup up UDP port */
	lst_skt = init_tcp(9006);
	serv_skt = -1;
	
		
	if ((rx_skt = socket(AF_INET, SOCK_PACKET, htons(ETH_P_AX25))) == -1)
	{
		perror("socket");
		return(1);
	}
	/*
	 * If we want to re-read the config file, we HUP the process
	 * this function also sets the right signal handler.
	 */
	read_config_file(); 

	
	/* Kick the RSPF RRH timer into action and send RRH's */
	rrh_timer(0);
	
	/* Kick the adjacency checker program */
	check_adjacencies(0);
	
	/* Kick the routing bulletin timer into action */
	route_bull_timer(0);

	/* Kick the router checker into action */
	check_routers(0);
	
	/* Get signal mask */
	oldmask = sigblock(0);
	
	for (;;) 
	{
		int	max_fd = rx_skt;

		if ( lst_skt > max_fd )
			max_fd = lst_skt;
		if ( serv_skt > max_fd )
			max_fd = serv_skt;

		/* Set up select mask, must be done for every pass */
		FD_ZERO(&read_fds);
		FD_ZERO(&exception_fds);
		FD_SET(rx_skt, &read_fds);
		FD_SET(rx_skt, &exception_fds);
		FD_SET(lst_skt, &read_fds);
		FD_SET(lst_skt, &exception_fds);
		if (serv_skt > 0) {
			FD_SET(serv_skt, &read_fds);
			FD_SET(serv_skt, &exception_fds);
		}
		
		i = select(max_fd + 1, &read_fds, (fd_set *) 0, &exception_fds, NULL);			
		if (i < 0)
			continue;
		/* Test to see if we came out of select due to rx frames */
		if (FD_ISSET(rx_skt, &read_fds)
		 || FD_ISSET(rx_skt, &exception_fds))
		{
			if ((size = recvfrom(rx_skt, buf, sizeof(buf), 0, &sa, &asize)) == -1)
			{
				perror("recv");
				exit(1);
			}
			oldmask = sigblock( sigmask(SIGALRM));
			recv_packet(buf, size, sa.sa_data);
			sigsetmask(oldmask);
			fflush(stdout);
		}
		if (FD_ISSET(lst_skt, &read_fds)
		 || FD_ISSET(lst_skt, &exception_fds))
		{
			if (serv_skt > 0)
			{	
				int tmp_skt;
				
				tmp_skt = accept(lst_skt, NULL, NULL);
				strcpy(msg, "Server in use - cannot connect.\n");
				
				write(tmp_skt, msg, strlen(msg)+1);
				close(tmp_skt);
				continue;
			}
			if ((serv_skt = accept(lst_skt, (struct sockaddr*)&cli_addr, &clilen)) > 0)
			{
				time_t now;
				signal(SIGPIPE, sig_pipe);
				serv_stream = fdopen(serv_skt, "w");
				/* Make socket non-blocking */
				fcntl(serv_skt, F_SETOWN, FNDELAY);
				
				gethostname(msg, 200);
				time(&now);
				fprintf(serv_stream,
				 "%s RSPF daemon server (%s) ", msg, version);

				strftime(msg, 100,
				 "%a %b %d %H:%M:%S %Z %Y ready.\r\n\n> ",
				 localtime(&now));
				fprintf(serv_stream, "%s", msg);
				fflush(serv_stream);
			}
		}
		
		if ((serv_skt > 0)
		 && (FD_ISSET(serv_skt, &read_fds)
		  || FD_ISSET(serv_skt, &exception_fds)))
		{
			char inbuf[200];
			int status = read(serv_skt, inbuf, sizeof(inbuf) - 1);
			/* Null-terminate the input bufffer */
			
			if ( status > 0 ) {
				inbuf[status] = '\0';
				status = do_tcp(serv_stream, inbuf);
			}

			if ( status <= 0 ) {
				shutdown(serv_skt, 2);
				fclose(serv_stream);
				serv_skt = -1;
			}
		}
			
				
	}
	closelog();
}

	

void recv_rspf(char * data, ax25_address callsign, char *port, int length, u_long saddr, u_long daddr)
{
	struct rspfrrh *rrh;
	struct rspfroute_hdr *rtehdr;
	time_t now;
	struct rspf_adj *adj;
	int wrong_version;
	
	
	
	now =  time(NULL);
	/* Make sure this packet came on a configured interface */
	if (! is_rspfif(port)) {
		rspf_stats.rspfInNotIfaces++;
		syslog(LOG_DAEMON | LOG_WARNING, "RSPF message from %s (%s) on non-rspf port %s",
					in_ntoa(saddr), ax25_ntoa(&callsign), port);
		return;
	}
	/* Use the PID to decode the RSPF packet */
	switch (data[1]){
	    case TYPE_RRH:
	    if (debug_mode)

	    	rspf_stats.rspfInRrhs++;
		rrh= (struct rspfrrh*)data;
		if (debug_mode)
			printf("recv_rspf(): Rx RRH from router %s (%s) on port %s. Sequence number %#3x.\n", in_ntoa(saddr), ax25_ntoa(&callsign), port, ntohs(rrh->tx_pkts) );
		/* Check version - first digit only (ie 2x) */
		wrong_version = 0;
		if ((int)(rrh->version / 10) != (int)(RSPF_VERSION / 10))
			wrong_version = 1;			
		/* 
		 * Check the RRH's checksum, with pseudo header and host byte order
		 */
		 
		if (rspf_check((char*)rrh, length, saddr ,daddr) && !wrong_version) {
			if (debug_mode)
			{
				printf("Bad checksum. Received %#3x ", rrh->checksum);
				rrh->checksum = 0;
				printf("Required %#3x.\n", rspf_check((char*)rrh, length, saddr, daddr));
			}
			rspf_stats.rspfInHdrErrors++;
			return;
		}			

		/* 
		 * Now we update adjacency table
		 */
		adj = get_adjacency(saddr, port);
		if (adj == NULL)
		{
			if (!wrong_version)
			{
				/* We've got a new adjacency, mark it tentative and fire up pinger */
				sus_adjacency(saddr, callsign, port, ntohs(rrh->tx_pkts) );			
			} else {
				/* 
				 * As were not sure what this is, don't use RRH counter
				 */
				sus_adjacency(saddr, callsign, port, 0);
			}
			/*
			 * New routers get our full routing table
			 */
			send_full_bulletin(port);
			
		} else {
			/* We've got the adjacency, so it's OK status */
			if (!wrong_version)
				add_adjacency(saddr, callsign, port, ntohs(rrh->tx_pkts), adj->status);
		}			
		break;
	
	    case TYPE_ROUTING:
	    	rspf_stats.rspfInRouteEnvs++;
	    	
	    	if (length < RSPFROUTE_LEN) {
	    		syslog(LOG_DAEMON | LOG_ERR, "recv_rspf(): Routing packet to small (%d bytes)\n", length);
	    		rspf_stats.rspfInHdrErrors++;
	    		return;
	    	}
		rtehdr = (struct rspfroute_hdr*)data;

		if ((int)(rtehdr->version / 10) == (int)(RSPF_VERSION / 10))
		{
			recv_rspf_frag(saddr, daddr, data, length, port);
		}
	 	/* 
		 * Now we update adjacency table, we cannot really say it is a router 
		 * until we get sequence numbers from a RRH
		 */
		adj = get_adjacency(saddr, port);
		if (adj == NULL)
		{
			/* We've got a new router, mark it tentative and fire up pinger */
			sus_adjacency(saddr, callsign, port, 0 );			
			/* Send new router our full routing table */
			send_full_bulletin(port);
		}
		break;
				          
	    default:		        
	        {
	        struct in_addr inaddr;
	        inaddr.s_addr = saddr;
	        rspf_stats.rspfInUnknownTypes++;
	        syslog(LOG_DAEMON | LOG_WARNING, "RSPFd Unknown RSPF packet type %d from router %s.\n", data[1], inet_ntoa(inaddr));
	        return;
		break;	        
		}
	}
			

}

/*
 * Returns the number of packets seen from a node (modulus 65536)
 */
u_short get_rx_pkts(ax25_address dladdr, char *port)
{
	FILE *fp;
	char buf[256];
	char dcall[16];
	char dport[IFNAMSIZ];
	u_long dframes;
	char callsign[9];
	
	strcpy(callsign, ax25_ntoa(&dladdr));
	
	if ( (fp = fopen(PROC_PATH, "r")) == NULL) {
		syslog(LOG_DAEMON | LOG_ERR, "Cannot open %s (%m).\n", PROC_PATH);
		return 0;
	}
	/* Read and throw first line (headers) */
	fgets(buf, 256, fp);
	
	while ( (fgets(buf, 256, fp)) != NULL) {
		if (sscanf(buf, "%s %s %lu", dcall, dport, &dframes) == 3) {
		    if ( (strncmp(dcall, callsign, 16) == 0) && (strncmp(dport, port, IFNAMSIZ) == 0) ) {
			fclose(fp);
			return (u_short)(dframes & 0xffff);
		    }
		}
	}
	fclose(fp);
	return 0;
}

void recv_ip(char* buf, ax25_address callsign, char * port, int len)
{
	struct iphdr* ip;
	int rslen;
	u_long addr;

	/* Check that we have a big enough packet for IP */
	if (len < sizeof(struct iphdr)) {
		syslog(LOG_DAEMON | LOG_WARNING, "recv_ip(): Packet too small (%d < %d). DL address %s\n", len, sizeof(struct iphdr), ax25_ntoa(&callsign));
		return;
	}
	ip = (struct iphdr*)buf;
	
	/* Check that this packet is for us */
	addr = get_iface_addr(port);

	if ( addr != ip->daddr) {
		addr = get_bcast_addr(port);
		if( addr == INADDR_NONE)
		{
			syslog(LOG_DAEMON | LOG_WARNING, "recv_ip(): Cannot get broadcast address for interface %s.", port);
			return;
		}
		if ( addr != ip->daddr) {
			return;
		}
	}
	

	/* Check we have at least the length for a RRH (smallest packet) */
	rslen = ntohs(ip->tot_len) - (ip->ihl << 2);

		
	/* Determine what to do, based upon the protocol byte */
	switch (ip->protocol) {
		case IPPROTO_RSPF:
			rspf_stats.rspfInMsgs++;
			recv_rspf(buf + (ip->ihl << 2), callsign, port, rslen, ip->saddr, ip->daddr);
			break;
				
		case IPPROTO_ICMP:
			recv_icmp(buf + (ip->ihl << 2), callsign, port, rslen, ip->saddr, ip->daddr);
			break;			
	}		
	
		
} /* recv_ip() */

		

unsigned short rspf_check(unsigned char *rh, int len,
	 unsigned long saddr, unsigned long daddr)
{     
	unsigned long sum;
   
	if (saddr == 0) 
		return 0; /*saddr = ip_my_addr();*/

/*
 * stupid, gcc complains when I use just one __asm__ block,
 * something about too many reloads, but this is just two
 * instructions longer than what I want
 */
	__asm__("
	    addl %%ecx, %%ebx
	    adcl %%edx, %%ebx
	    adcl $0, %%ebx
	    "
	: "=b"(sum)
	: "0"(daddr), "c"(saddr), "d"((ntohs(len) << 16) + IPPROTO_RSPF*256)
	: "bx", "cx", "dx" );
	__asm__("
	    movl %%ecx, %%edx
	    cld
	    cmpl $32, %%ecx
	    jb 2f
	    shrl $5, %%ecx
	    clc
1:	    lodsl
	    adcl %%eax, %%ebx
	    lodsl
	    adcl %%eax, %%ebx
	    lodsl
	    adcl %%eax, %%ebx
	    lodsl
	    adcl %%eax, %%ebx
	    lodsl
	    adcl %%eax, %%ebx
	    lodsl
	    adcl %%eax, %%ebx
	    lodsl
	    adcl %%eax, %%ebx
	    lodsl
	    adcl %%eax, %%ebx
	    loop 1b
	    adcl $0, %%ebx
	    movl %%edx, %%ecx
2:	    andl $28, %%ecx
	    je 4f
	    shrl $2, %%ecx
	    clc
3:	    lodsl
	    adcl %%eax, %%ebx
	    loop 3b
	    adcl $0, %%ebx
4:	    movl $0, %%eax
	    testw $2, %%dx
	    je 5f
	    lodsw
	    addl %%eax, %%ebx
	    adcl $0, %%ebx
	    movw $0, %%ax
5:	    test $1, %%edx
	    je 6f
	    lodsb
	    addl %%eax, %%ebx
	    adcl $0, %%ebx
6:	    movl %%ebx, %%eax
	    shrl $16, %%eax
	    addw %%ax, %%bx
	    adcw $0, %%bx
	    "
	: "=b"(sum)
	: "0"(sum), "c"(len), "S"(rh)
	: "ax", "bx", "cx", "dx", "si" );

  	/* We only want the bottom 16 bits, but we never cleared the top 16. */
  
  	return((~sum) & 0xffff);
}


void add_adjacency(u_long addr, ax25_address dladdr, char *port, u_int tx_pkts, AdjStatus status)
{
	time_t	now;
	struct rspf_adj *adj;
	char key[KEY_SIZE];
	qmark adj_qm;

	now = time(NULL);
	strcpy(key, in_ntoa(addr));

	adj = (struct rspf_adj*)qfind_first(adj_queue, key, &adj_qm);
	if ( adj == NULL) {
		/* New Adjacency */
		adj = (struct rspf_adj*)malloc(sizeof(struct rspf_adj));
		if (adj == NULL) {
			syslog(LOG_DAEMON | LOG_ERR,"add_adjacency(): Memory squeze.");
			return;
		}
		/* Copy the key in and add it to queue*/
		adj->dladdr = dladdr;
		strcpy(adj->port, port);
		if (tx_pkts != 0)
			adj->rrhtime = now;
		else
			adj->rrhtime = 0;
		add_qnode(adj_queue, (void*)adj, key);
		adj->rx_ratio = 0;
		rspf_stats.rspfCurrAdjacencies++;
	} else {
		if (tx_pkts != 0) {
			/* Update pkt stats */
			int drx, dtx;
			drx = get_rx_pkts(dladdr, port) - adj->rx_pkts;
			dtx = tx_pkts - adj->tx_pkts;
			/* Stop division by 0 error */
			if (dtx == 0)
				dtx = 1;		
			adj->rx_ratio = (u_int)( ((u_long)drx * 100) / (u_long)dtx);

			adj->rrhtime = now;
		} else {
			adj->rx_ratio = 0;	
		}
	}
	adj->addr = addr;
	adj->rx_pkts = get_rx_pkts(dladdr, port);
	adj->tx_pkts = tx_pkts;
	adj->status = status;
	adj->cost = get_iface_cost(port);
	adj->horizon = get_horizon(addr, port);
	/*
	 * Re-calc the routes if we have a new good adjacency
	 */
	if (status == Ok)
	{
		do_spf();
	}
}

/*
 * del_adjacency()
 *
 * Deletes the specified adjacency from the adjacency list 
 *
 * Returns:
 *	Nothing
 *
 * Arguments:
 *	u_long		ip address of adjacency
 *	char*		port it occurs on
 */
void del_adjacency(u_long addr, char *port)
{
	char key[KEY_SIZE];
	qmark adj_qm;
	
	strcpy(key, in_ntoa(addr));	
	if ( qfind_first(adj_queue, key, &adj_qm) == NULL) {
		syslog(LOG_DAEMON | LOG_ERR, "Cannot find %s on %s in adjacency queue.\n", in_ntoa(addr), port);
		return;
	}		
	rspf_stats.rspfCurrAdjacencies--;
	del_qnode(adj_queue, adj_qm, 1);
}

	
struct rspf_adj *get_adjacency(u_long addr, char *port)
{
	char key[KEY_SIZE];
	struct rspf_adj *adj;
	qmark adj_qm;

	strcpy(key, in_ntoa(addr));
	adj = (struct rspf_adj*)qfind_first(adj_queue, key, &adj_qm);
	return adj;				
}
				
/*
 * rrh_timer()
 *
 * This is called by the delta queue when it is time to send the RRH's on
 * the ports.  It also re-schedules a timer event for the next RRH
 */
void rrh_timer(int id)
{
	static int timer_id = 0;
	char buf[256];	/* Max 20 interfaces */
	char *bufptr = buf;
	char ifname[IFNAMSIZ];
	int ifcount;
	
	/*
	 * Sanity check on timer ID, so we know we've been called properly
	 */
	if (timer_id != id)
	{
		syslog(LOG_DAEMON | LOG_ERR, "rrh_timer(): Incorrect timer ID. Got %#3x, expected %#3x\n", id, timer_id);
		return;
	}		
	
	ifcount = rspf_ifaces(buf, 256);

	while (ifcount-- > 0) {
		strcpy(ifname, bufptr);
		bufptr += strlen(ifname) + 1;
		send_rrh(ifname);
	}
	/* Now set up the timer for next RRH */
	timer_id = dq_add(rspf_stats.rspfRrhTimer, rrh_timer);
} /* rrh_timer()*/

/*
 * route_bull_timer()
 *
 * Called by timer event when it is time to send a new routing bulletin
 *
 * Returns:
 *	Nothing
 *
 * Arguments:
 *	int	timer id
 */
void route_bull_timer(int id)
{
	static int timer_id = 0;
	 
	/*
	 * Sanity check on timer ID, so we know we've been called properly
	 */
	if (timer_id != id)
	{
		syslog(LOG_DAEMON | LOG_ERR, "route_bull_timer(): Incorrect timer ID. Got %#3x, expected %#3x\n", id, timer_id);
		return;
	}		

	if (rspf_stats.SequenceNumber  != -1)
	{
		send_full_bulletin(NULL);	
		rspf_stats.SequenceNumber++;
		
		/* Now set up the timer for next routing bulletin */
		timer_id = dq_add(rspf_stats.rspfBullTimeout, route_bull_timer);
	} else {
		/* We've run out of Sequence numbers, wait until bull times
		* out and reset seq#
		 */
		rspf_stats.SequenceNumber++;
		timer_id = dq_add(rspf_stats.rspfBullTimeout, route_bull_timer);
	}
}


void recv_packet(unsigned char *buf, int bufsize, char *port)
{
	ax25_address daddr;
	struct full_sockaddr_ax25 saddr;
	int pid;
	unsigned char *axdata;
	int datalen;
	int count;
	unsigned char *startbuf = buf;
	
	/* skip over start flag */
	buf++;

	/* Now we have destination address */
	bcopy(buf, daddr.ax25_call, 7);
	buf += 7;
	/* put in the digipeaters */
	count = 0;
	while ( (count < AX25_MAX_DIGIS) && ((buf[6] & 0x01)!= 1) ) {
		bcopy(buf,(saddr.fsa_digipeater[count++].ax25_call), 7);
		buf += 7;
	}
	saddr.fsa_ax25.sax25_family = AF_AX25;
	saddr.fsa_ax25.sax25_ndigis = count;
	
	/* Now put in source address */
	bcopy(buf, (saddr.fsa_ax25.sax25_call.ax25_call), 7);
	buf += 7;
	
	/* Check the control bit, proceed only if UI or I frame */
	if ( ( (*buf & 0xef) != 0x03) && ((*buf & 0x01) != 0) ) {
		return;

	}
	buf++;
	/* Now at PID, extract it */
	pid = *buf++;

	/* pointing at data */		
	axdata = buf;
	datalen = bufsize - (int)(buf - startbuf);
	
	switch(pid)
	{
	    case PID_IP:
	    	recv_ip(axdata, saddr.fsa_ax25.sax25_call, port, datalen);
	    	break;
	    case PID_ARP:
	    	recv_arp(axdata, saddr.fsa_ax25.sax25_call, port, datalen);
	    	break;
	}
	 
}

/*
 * in_cksum()
 *
 * Checksum for ICMP messages
 * Taken from Mike Muuss' PING program
 *
 * Returns:
 *	u_short		The checksum
 *
 * Arguments:
 *	u_short*	The start of the output buffer
 *	int		length of buffer
 */
u_short in_cksum(u_short *addr, int len)
{
	register int nleft = len;
	register u_short *w = addr;
	register int sum = 0;
	u_short answer = 0;

	/*
	 *  Our algorithm is simple, using a 32 bit accumulator (sum),
	 *  we add sequential 16 bit words to it, and at the end, fold
	 *  back all the carry bits from the top 16 bits into the lower
	 *  16 bits.
	 */
	while( nleft > 1 )  {
		sum += *w++;
		nleft -= 2;
	}

	/* mop up an odd byte, if necessary */
	if( nleft == 1 ) {
		*(u_char *)(&answer) = *(u_char *)w ;
		sum += answer;
	}

	/*
	 * add back carry outs from top 16 bits to low 16 bits
	 */
	sum = (sum >> 16) + (sum & 0xffff);	/* add hi 16 to low 16 */
	sum += (sum >> 16);			/* add carry */
	answer = ~sum;				/* truncate to 16 bits */
	return (answer);
}

/*
 * recv_icmp()
 *
 * Takes and decodes an icmp packet and then decides what to do with it
 *
 * Returns:
 *	Nothing
 *
 * Arguments:
 *	char*		Data in icmp packet
 *	ax25_address	Callsign of sender
 *	char*		Port that it is received
 *	int		Length of data
 *	u_long		Sender IP address
 *	u_long		Destination IP address
 */
void recv_icmp(char * data, ax25_address callsign, char *port, int length, u_long saddr, u_long daddr)	
{
	struct icmphdr *icp = (struct icmphdr*)data;
	struct pings *pptr;
	struct rspf_adj *adj;
	char key[KEY_SIZE];
	qmark ping_qm;
	
	/* Check the length of the header so it is bigger than min size */
	if (length < sizeof(struct icmphdr)) 
	{
		syslog(LOG_DAEMON | LOG_WARNING, "recv_icmp(): Packet too small (%d bytes) from router %s", length, in_ntoa(saddr));
		return;
	}

	/* Check the checksum, if we have one */	
	if (icp->checksum != 0) 
	{
		if (in_cksum((u_short*)data, length) != 0) 
		{
			return;
		}
	}
	
	/* Check that this is for us */
	if (icp->un.echo.id != (getpid() & 0xffff) ) 
	{
		return;
	}
	
	switch(icp->type) 
	{
		case ICMP_ECHOREPLY:
			strcpy(key, in_ntoa(saddr));
			pptr = (struct pings*)qfind_first(pingq, key, &ping_qm);
			if ( pptr == NULL) 
				return;
			/* We have while loop as they're may be more than one
			 * item for a router, but we must delete old nodes that
			 * appear before ours
			 */				
			while (pptr != NULL) 
			{
				if (pptr->echo_id == icp->un.echo.sequence) 
				{
					strcpy(key, in_ntoa(pptr->addr));
					adj = get_adjacency(pptr->addr, pptr->port);
					if (adj != NULL) 
					{
						/* Update adjacency state if we can */
						if ( (adj->status == Tentative) || (adj->status == Suspect) )
						{
							adj->status = Ok;		
							do_spf();
							/* Tell everyone the good news */
							send_news(daddr, saddr, 32, get_iface_cost(port), port);
						} else {
							syslog(LOG_DAEMON | LOG_ERR, "icmp_recv(): Got ping from router %s on %s when non sus", 
									ax25_ntoa(&pptr->dladdr), pptr->port); 
						}
					} else
						syslog(LOG_DAEMON | LOG_ERR, "icmp_recv(): Got ping from %s on %s but not and adjacency", 
									ax25_ntoa(&pptr->dladdr), pptr->port);
					/* Revoke the time-out item in delta queue */
					dq_del(pptr->id);
					/* Remove ping from queue */

					del_qnode(pingq, ping_qm, 1);
					pptr = NULL;
					return;
				}
				pptr = (struct pings*)qfind_next(pingq, key, &ping_qm);
			}
							
			break;
	}
} /* recv_icmp() */

/*
 * sus_adjacency()
 *
 * Makes an adjacency suspicous and sets up the ping routines
 */
void sus_adjacency(u_long addr, ax25_address dladdr, char *port, u_int tx_pkts)
{
	struct pings *pptr;
	char key[KEY_SIZE];
	struct rspf_adj *adj;
	qmark ping_qm;

	/* Update adjancecy's status. If we know about it, it's sus, if
	 * it is new, its Tentative
	 */
	if ( (adj = get_adjacency(addr, port)) != NULL) {
		adj->status = Suspect;
	} else {
		add_adjacency( addr, dladdr, port, tx_pkts, Tentative);	
	}
	/* Now fix up delta queue */
	strcpy(key, in_ntoa(addr));			
	pptr = (struct pings*)qfind_first(pingq, key, &ping_qm);	
	if ( pptr == NULL) {
		pptr = (struct pings*)malloc(sizeof(struct pings));
		if (pptr == NULL) {
			syslog(LOG_DAEMON | LOG_ERR, "sus_adjacency(): Memory sequeze");
			return;
		}
		pptr->addr = addr;
		strcpy(pptr->port, port);

		pptr->dladdr = dladdr;

		add_qnode(pingq, (void*)pptr, key);
	} else {
		dq_del(pptr->id);
	}
	pptr->count = rspf_stats.rspfSusPings;
	/* 
	 * We now have to start up the pinger events.  Now fiurst version I
	 * setup something in the delta queue then manually called the pinger
	 * to get the first one going.  This is a 'Bad Thing' as we then get a
	 * phantom entry in the delta queue.  So I now add the pinger to the 
	 * delta queue but make the time-out 1 second, pinger will then use the
	 * normal delay for the following pings.
	 */
	pptr->id = dq_add(1, rspf_pinger ); 
	if (debug_mode)
		printf("sus_adjacency(): Attempting to ping %s on port %s .\n", in_ntoa(addr), port);
}
	
/*
 * rspf_pinger()
 *
 * Sends a ping to a sus node, called by the delta queue generally			
 *
 * Returns:
 *	Nothing
 *
 * Arguments:
 *	int	Identification of the delta queue
 */
void rspf_pinger(int id)
{
	struct pings *pptr;
	struct rspf_adj *adj;
	u_long saddr;
	qmark ping_qm;
	

	if ( (pptr = qmove_first(pingq, &ping_qm)) == NULL) {
		syslog(LOG_DAEMON | LOG_ERR, "rspf_pinger(): Ping queue is empty, Delta queue id = %#3x", id);	
		return;
	}
	
	while (pptr != NULL) {
		
		if (pptr->id == id)
			break;
		pptr = (struct pings*)qmove_next(pingq, &ping_qm);
	}
	
	if (pptr == NULL) {
		syslog(LOG_DAEMON | LOG_ERR, "rspf_pinger(): Could not find id %#3x in ping queue", id);
		return;
	}
	/* If we're here we've got something in the ping list */
	
	if ( (adj = get_adjacency(pptr->addr, pptr->port)) == NULL) {
		syslog(LOG_DAEMON | LOG_ERR, "rspf_pinger(): Address %s in ping list not in adjacencies.\n", ax25_ntoa(&pptr->dladdr));
		/* Now remove the ping entry */
		del_qnode(pingq, ping_qm, 1);
		pptr = NULL;
		return;
	}
	/* We only worry about tentative or suspect adjacencies */		
	if ( (adj->status != Tentative) && (adj->status != Suspect) ) {
		/* Remove ping entry */
		del_qnode(pingq, ping_qm, 1);
		pptr = NULL;
		return;
	}
	/* Have we run out of pings? */	
	if (--(pptr->count) <= 0) {

		/* send bad news about addr, port - wizzer*/
		if (adj->status == Suspect)
		{
			
			/* Remove link state entry */
			saddr = get_iface_addr(pptr->port);
			if( saddr == INADDR_NONE)
			{
				syslog(LOG_DAEMON | LOG_WARNING, "rspf_pinger(): Cannot get address for interface %s", pptr->port);
			} else {
				send_news(saddr, pptr->addr, 32, 0xff, pptr->port);
			}
		}
		del_adjacency(pptr->addr, pptr->port);
		/* Remove ping entry */
		del_qnode(pingq, ping_qm, 1);
		pptr = NULL;
		return;
	}
	/* Setup next delta queue event */
	pptr->id = dq_add(rspf_stats.rspfPingTimer, rspf_pinger );

	/* Send another ping */
	if ( (pptr->echo_id = send_ping(pptr->addr, pptr->port)) == 0) {
		syslog(LOG_DAEMON | LOG_ERR, "rspf_pinger() send_ping failed, dropping %s (%m)", in_ntoa(pptr->addr));
		dq_del(pptr->id);
		del_adjacency(pptr->addr, pptr->port);
		/* Remove ping entry */
		del_qnode(pingq,ping_qm, 1);
		pptr = NULL;
	}
} /* rspf_pinger() */

/*
 * get_lastheard()
 *
 * Gets the last time the system heard the specified node on the specified port
 *
 * Returns:
 *	time_t		Time we last heard the node
 *
 * Arguements:
 *	ax25_address	Callsign of the required node
 *	char*		Name of port we heard the node
 */
time_t get_lastheard(ax25_address dladdr, char *port)
{
	FILE *fp;
	char buf[256];
	char dcall[16];
	char dport[IFNAMSIZ];
	time_t dtime;
	char callsign[9];
	
	strcpy(callsign, ax25_ntoa(&dladdr));
	
	if ( (fp = fopen(PROC_PATH, "r")) == NULL) {
		syslog(LOG_DAEMON | LOG_ERR, "Cannot open %s (%m).\n", PROC_PATH);
		return 0;
	}
	/* Read and throw first line (headers) */
	fgets(buf, 256, fp);
	
	while ( (fgets(buf, 256, fp)) != NULL) {
		if (sscanf(buf, "%s %s %*u %ld", dcall, dport, &dtime) == 3) {
		    if ( (strncmp(dcall, callsign, 16) == 0) && (strncmp(dport, port, IFNAMSIZ) == 0) ) {
			fclose(fp);
			return dtime;
		    }
		}
	}
	fclose(fp);
	return 0;
}

/*
 * check_adjacencies()
 *
 * This is called by the timer event to check on our adjacencies and 
 * make sus any that we have not heard from for a long while
 *
 * Returns:
 *	Nothing
 *
 * Arguments:
 *	int	The timer event id
 */
void check_adjacencies(int id)
{ 
	static int timer_id = 0;
	struct rspf_adj *adj;
	time_t lasthrd;
	time_t now;
	qmark adj_qm;
	u_int mintime;	

	/*
	 * Sanity check for the timer id
	 */
	if (timer_id != id)
	{
		syslog(LOG_DAEMON | LOG_ERR, "check_adjacencies(): Incorrect timer ID. Got %#3x, expected %#3x.\n", id, timer_id);
		return;
	}
			
	/* The most we will wait before checking again is a set parameter */
	mintime = rspf_stats.rspfSusTimeout;

	adj = (struct rspf_adj*)qmove_first(adj_queue, &adj_qm);		
	while (adj != NULL) {

		/* 
		 * Get last time we heard this station 0 means never heard
		 * or some error.  As we are always greater than SUS_TIMEOUT
		 * seconds past Unix epoch time (0:00 1/1/70 UTC) then a 0
		 * will make the adjacency suspect always
		 */
		lasthrd =  get_lastheard(adj->dladdr, adj->port);
		now = time(NULL);
		if (adj->status == Ok) {
			if ( (now - lasthrd) > rspf_stats.rspfSusTimeout ) {
				/* We have a node that is suspect, mark it as such */
				sus_adjacency(adj->addr, adj->dladdr, adj->port, adj->tx_pkts);
			}
		} else {
			/* Determine when we have to next look here 
			 * we come back when the next node would be suspect
			 * if we don't get anything
			 */
			if (mintime > (rspf_stats.rspfSusTimeout - (u_int)(now - lasthrd)) )
				mintime = rspf_stats.rspfSusTimeout - (u_int)(now - lasthrd);

		}
		/* Move onto next one */
		adj = (struct rspf_adj*)qmove_next(adj_queue, &adj_qm);
	}

	/* Setup timer for next check 
	 * mintime is the number of seconds before any node can be
	 * suspect, so we don't bother to come back until that happens
	 */
	if (mintime < 1)
		mintime = 1;
	timer_id = dq_add(mintime, check_adjacencies);

} /* check_adjacencies */

/*
 * fragment_reasm()
 *
 * Called by timer when fragment reassembly timeout has happened, so kick
 * reassembly function in
 *
 * Returns:
 *	Nothing
 *
 * Arguments:
 *	int		timer queue id
 */
void reasm_timer(int id)
{
	struct rspf_frag *fragment;
	qmark frag_qm;
	int oldmask;
	
	oldmask = sigblock(sigmask(SIGALRM));
	fragment = (struct rspf_frag*)qmove_first(fragq, &frag_qm);
	while (fragment != NULL) {
		if (fragment->timer_id == id) {
			/* Delete the fragment */
			del_qnode(fragq, frag_qm, 1);
			fragment = NULL;
			do_spf();
			sigsetmask(oldmask);
			return;
		}
		fragment = (struct rspf_frag*)qmove_next(fragq, &frag_qm);
	}
	sigsetmask(oldmask);
	syslog(LOG_DAEMON | LOG_ERR, "fragment_reasm(): Timer id %#3x called us but no fragments waiting on that id.");
} /* fragment_reasm() */
	
		
void recv_arp(u_char *data,ax25_address saddr,char *port,int datalen) 
{
	struct ax25arp_hdr {
		u_short	ar_hrd;
		u_short ar_pro;
		u_char	ar_hln;
		u_char	ar_pln;
		u_short ar_op;
		ax25_address ar_sha;
		u_char	ar_sip[4];
		ax25_address ar_tha;
		u_char	ar_tip[4];
	};
		
	
	struct ax25arp_hdr *arp;
	u_long *tip, *sip;
	struct rspf_adj *adj;

	
/*	if (datalen < sizeof(struct ax25arp_hdr))
		return;*/
			
	arp = (struct ax25arp_hdr*)data;
	
	if ( (ntohs(arp->ar_hrd) != ARPHRD_AX25) || (ntohs(arp->ar_pro) != PID_IP) ) 
	{
		return; 	
	}
	tip = (u_long*)&(arp->ar_tip);
	sip = (u_long*)&(arp->ar_sip);
	switch(ntohs(arp->ar_op)) 
	{
		case ARPOP_REQUEST:
			/* Check we have the replier in adjacencies */
			adj = get_adjacency(*sip, port);
			if (adj == NULL)
			{
				/* We've got a new router, mark it tentative and fire up pinger */
				sus_adjacency(*sip, arp->ar_sha, port, 0 );			
			}
			break;
		case ARPOP_REPLY:
			/* Check we have the replier in adjacencies */
			adj = get_adjacency(*sip, port);
			if (adj == NULL)
			{
				/* We've got a new router, mark it tentative and fire up pinger */
				sus_adjacency(*sip, arp->ar_sha, port, 0 );			
			}
			break;
		case ARPOP_RREQUEST:
			break;
		case ARPOP_RREPLY:
			break;
	}			
 }
 


void read_config_file()
{
	char buf[128];
	char param[20], value[108];
	FILE *fp;
	
	if ( (fp = fopen(CONFIG_FILE, "r")) == NULL) {
		syslog(LOG_DAEMON | LOG_ERR, "Cannot find configuration file: %s (%m)\n",CONFIG_FILE); 
		return;
	}
	/* Reset any variables here */
	clear_all_ifaces();
	
	while ( fgets(buf, 128, fp) != NULL) {
		if ( buf[0] == '#')
			continue;
		if (sscanf(buf, "%[^=]%*c%[^\n]", param, value) == 2) {
			/* A big wierdo switch */
			if (strcmp(param, "rrhtimer") == 0) {
				rspf_stats.rspfRrhTimer = atoi(value);
			} else if (strcmp(param, "suspings") == 0) {
				rspf_stats.rspfSusPings = atoi(value);
			} else if (strcmp(param, "pingtimer") == 0) {
				rspf_stats.rspfPingTimer = atoi(value);
			} else if (strcmp(param, "pingtimeout") == 0) {
				;
			} else if (strcmp(param, "reasmtimeout") == 0) {
				rspf_stats.rspfReasmTimeout = atoi(value);
			} else if (strcmp(param, "rspfiface") == 0) {
				char iface[IFNAMSIZ];
				int quality;
				if (sscanf(value, "%s %d", iface, &quality) == 2) {
					add_iface(iface, quality);
				}
			} else if (strcmp(param, "sustimeout") == 0) {
				rspf_stats.rspfSusTimeout = atoi(value);
			} else if (strcmp(param, "linkhorizon") == 0) {
				rspf_stats.rspfLinkHorizon = atoi(value);
			} else if (strcmp(param, "grouphorizon") == 0) {
				rspf_stats.rspfGroupHorizon = atoi(value);
			} else if (strcmp(param, "localhorizon") == 0) {
				rspf_stats.rspfLocalHorizon = atoi(value);
			} else if (strcmp(param, "portablehorizon") == 0) {
				rspf_stats.rspfPortableHorizon = atoi(value);
			} else if (strcmp(param, "bulltimer") == 0) {
				rspf_stats.rspfBullTimer = atoi(value);
			} else if (strcmp(param, "bulltimeout") == 0) {
				rspf_stats.rspfBullTimeout = atoi(value);
			} else if (strcmp(param, "nodegroup") == 0 ) {
				char ipaddr[40];
				int sigbits;
				char iface[IFNAMSIZ];
				int cost;
				if (sscanf(value, "%s %d %s %d",ipaddr, &sigbits, iface, &cost) == 4)
				{
					add_nodegroup(inet_addr(ipaddr), sigbits, cost, iface);
				}
				else if (sscanf(value, "%s %d %s", ipaddr, &sigbits, iface) == 3)
				{
					if ((cost = get_iface_cost(iface)) == 254)
						cost = 1;
					add_nodegroup(inet_addr(ipaddr), sigbits, cost, iface);
				} else
					fprintf(stderr, "rspfd: Config format is: nodegroup=<ipaddr> <sigbits> <port> [<cost>].\n");
			}
		}
	}
	fclose(fp);
	signal(SIGHUP, read_config_file);
}


/* 
 * add_link()
 *
 * adds a link to the link queue, replaces it if there is already one
 *
 * Returns:
 *	int	Flag if new route has more hoziron
 *
 * Arguments:
 *	u_long		Start address of link
 *	u_long		Destination address of link
 *	u_char		Significant bits of destination address
 *	u_char		Horizon of link	
 *	u_char		Cost of link
 *	u_short		Sequence number of the bull that put us here
 */
int add_link(u_long saddr, u_long daddr, u_char sigbits, u_char horizon, u_char cost, u_short seq_no)
{
	struct  link *lnk;
	int closer = 0;
	qmark link_qm;
	
	lnk = (struct link*)qmove_first(linkq, &link_qm);
	
	while (lnk != NULL) {
		if ( (lnk->saddr == saddr) && (lnk->daddr == daddr) && 
		     (lnk->sigbits == sigbits) ) {
			/* We got a match, see if newer longer route */
			if (lnk->horizon < horizon)
				closer = 1;
			/* Copy across information */
			lnk->horizon = horizon;
			lnk->cost = cost;
			lnk->seq_no = seq_no;
			return closer;
		}
		/* No match, so get next one */
		lnk = (struct link*)qmove_next(linkq, &link_qm);
	} /* while */
	/* If we get here, there was no match, so add a new queue node */
	lnk = (struct link*)malloc(sizeof(struct link));
	if (lnk == NULL) {
		syslog(LOG_DAEMON | LOG_ERR, "add_link(): Memory squeze. (%m)");
		return 1;
	}
	/* Copy data across */
	lnk->seq_no = seq_no;
	lnk->saddr = saddr;
	lnk->daddr = daddr;
	lnk->sigbits = sigbits;
	lnk->horizon = horizon;
	lnk->cost = cost;
	add_qnode(linkq, lnk, NULL);
	return 1;
} /* add_link */				
		

/*
 * del_link_source()
 *
 * Removes all links that have the given source address, that don't
 * have specified sequence number
 *
 * Returns:
 *	Nothing
 *
 * Arguments:
 *	u_long		ip address of required source address
 *	u_short		Seqence number which we don't delete
 */		
void del_link_source(u_long saddr, u_short seq_no)
{
	struct link *lnk;
	qmark link_qm;
	
	lnk = (struct link*)qmove_first(linkq, &link_qm);
	
	while(lnk != NULL) {
		if ( (lnk->saddr == saddr) && (lnk->seq_no != seq_no) )
		{
			del_qnode(linkq, link_qm, 1);
			lnk = (struct link*)qmove_first(linkq, &link_qm);
		} else {
			lnk = (struct link*)qmove_next(linkq, &link_qm);
		}
	}
} /* del_link_source */

/*
 * del_link()
 *
 * Deletes a link from the queue
 *
 * Returns:
 *	Nothing
 *
 * Arguments:
 *	saddr	Source address of link
 *	daddr	Destination address of link
 *	sigbits	Siginicint bits of dest addr
 */
void del_link(u_long saddr, u_long daddr, u_char sigbits)
{
	struct link *lnk;
	qmark link_qm;
	
	lnk = (struct link*)qmove_first(linkq, &link_qm);
	
	while(lnk != NULL) {
		if ( (lnk->saddr == saddr) && (lnk->daddr == daddr) && (lnk->sigbits == sigbits) )
		{
			del_qnode(linkq, link_qm, 1);
		}
		lnk = (struct link*)qmove_next(linkq, &link_qm);
	}	
} /* del_link */

/*
 * clean_dead_links()
 *
 * Removes any links that have a cost of 255
 *
 * Arguments:
 *	Nothing
 *
 * Returns:
 *	Nothing
 */
void clean_dead_links()
{
	struct link *lnk;
	qmark lnk_qm;

	if (debug_mode)
		printf("clean_dead_links(): entering function\n");
		
	lnk = (struct link*)qmove_first(linkq, &lnk_qm);
	while(lnk != NULL)
	{
		if (lnk->cost == 255)
		{
			del_qnode(linkq, lnk_qm, 1);
			lnk = (struct link*)qmove_first(linkq, &lnk_qm);
		} else {
			lnk = (struct link*)qmove_next(linkq, &lnk_qm);
		}
	}
}
		

	
void recv_rspf_frag(u_long saddr, u_long daddr, char *data, int datalen, char *iface)
{
	struct rspf_frag *frg;
	char key[KEY_SIZE];
	char rkey[KEY_SIZE];
	qmark frag_qm;
	struct rspfroute_hdr *rth;
	struct rspfnode_hdr *nodehdr;
	struct rspflink_hdr *linkhdr;
	struct rspfadj_hdr *adjhdr;
	struct router* rtr;
	qmark rtr_qm;
	int nodes, links, adjs;
	int incomplete = 0;

	char *bptr;
	char *endptr = data + datalen;
	time_t now;
	int old_info, new_info;
	char newbuf[2048], oldbuf[2048];
	int newcnt, oldcnt, newnodes, oldnodes;
	int size;
	char ifbuf[128];
	char *ifname;
	int ifcount;
	
	rth = (struct rspfroute_hdr*)data;
	
	/* Check checksum */
	if (rspf_check(data, datalen, saddr ,daddr)) {
		rth->checksum = 0;
		rspf_stats.rspfInHdrErrors++;
		return;
	}			
	now = time(NULL);
	newcnt = oldcnt = newnodes = oldnodes = links = adjs = 0;

	/* Create key */
	strcpy(key, in_ntoa(saddr));
	
	if (debug_mode)
		printf("recv_rspf_frag(): Got Envelope %u (frag %u/%u) from %s.\n", htons(rth->env_no), rth->frag, rth->frag_tot, in_ntoa(saddr));
	/* Attempt to find fragment */
	frg = (struct rspf_frag*)qfind_first(fragq, key, &frag_qm);
	/* Remove node but keep entry for a while */
	del_qnode(fragq, frag_qm, 0);
	
	if (frg != NULL) {
		/* Turn off the timer */
		if(frg->timer_id != 0)
	 		dq_del(frg->timer_id);
		/* If this is an old fragment, ditch it */
		if (ntohs(rth->env_no) < frg->env_no) {
			free(frg);
			frg = NULL;
			return;
		} else {
			/* If this envelope is newer, finish off last one and
			 * process this one */
			if (ntohs(rth->env_no) > frg->env_no) {
			/* wizzer - do link stuff */
			free(frg);
			frg = NULL;
			
			nodes = rth->nodes;
			links = 0;
			adjs = 0;
			} else { /* This is the one */
				nodehdr = &(frg->nodehdr);
				linkhdr = &(frg->linkhdr);
				nodes = frg->nodes;
				links = frg->links;
				adjs = frg->adjs;
				incomplete = frg->incomplete;
				old_info = frg->old_info;
				new_info = frg->new_info;
			}
		}
	} else {	/* no new fragment */
		nodes = rth->nodes;
		links = 0;
		adjs = 0;
	}

	/* Attempt to find start of data header */
	if ( (rth->frag == 1) || ( (frg != NULL) && (rth->frag == frg->frag + 1) ) ) {
		bptr = data + RSPFROUTE_LEN;
	} else {
		/* We've lost a fragment somewhere */
		nodes--;
		links = 0;
		adjs = 0;
		incomplete = 1;						
		/* No sync means we cannot use this fragment */
		if (rth->sync == 0) {
			rspf_stats.rspfReasmFails++;
			if (rth->frag < rth->frag_tot)
				save_frag(saddr, rth->frag, nodehdr, linkhdr, nodes, links, adjs, incomplete, ntohs(rth->env_no), new_info, old_info);
			if (frg != NULL)
			{
				free(frg);
				frg = NULL;
			}
			return;
		}
		bptr = &(rth->sync) + rth->sync;
	}					
	while(nodes > 0) {
		nodes--;
		
		if (links == 0) {
			old_info = 0;
			new_info = 1;		
			/* Get node header */
			if (bptr + RSPFNODE_LEN > endptr) {
				if (rth->frag < rth->frag_tot)
					save_frag(saddr, rth->frag, nodehdr, linkhdr, nodes, links, adjs, incomplete, ntohs(rth->env_no), new_info, old_info);
				if (frg != NULL)
				{
					free(frg);
					frg = NULL;
				}
				return;
			}
			nodehdr = (struct rspfnode_hdr*)bptr;
			bptr += RSPFNODE_LEN;
			links = nodehdr->links;
			adjs = 0;
			/* 
			 * See if this routing update is our own, which we ignore
			 * in 0.02 we did this with hostname, now we have to look
			 * at each interface.
			 */
			if (get_iface_addr(iface) != *((u_long*)&nodehdr->addr))
			{
			
				ifcount = rspf_ifaces(ifbuf, 128);
				ifname = ifbuf;
				while(ifcount-- > 0)
				{
					if (get_iface_addr(ifname) == *((u_long*)&nodehdr->addr))				
					{
						old_info = 0;
						new_info = 0;
						if (debug_mode)
							printf("recv_rspf_frag(): Got our routing bull for iface %s on iface %s (ignoring).\n", ifname, iface);
						break;
					}
					ifname += strlen(ifname) + 1;
				} /* while */
				/* If it is not new info, then it must be one of
				 * the routing updates for another of our frequencies
				 */
				if (new_info)
				{
					/*
					 * We look for the router, first see if the sequence number
					 * bigger than the one we hold 
					 */
					strcpy(rkey, in_ntoa(*(u_long*)(nodehdr->addr)));
					rtr = (struct router*)qfind_first(routerq, rkey, &rtr_qm);
					if (rtr != NULL) {
						/* 
						 * We've heard them before, check that sequence
						 * number is less than we hold for that router 
						 */
						if ( ntohs(nodehdr->seq_no) < rtr->seq_no)  {
							/* Received seqno (nodehdr) < last heard seqno (rtr) 
							 * Let's check how long ago (in secs) it was when we got this
							 */
							if ( (now - rtr->bulltime) < rspf_stats.rspfBullTimeout) 
							{
								/* The seq# is still current, so ignore it */
								new_info = 0;
								old_info = 1;
							} /* Too long ago since we last heard them, ignore seq # */
						} /* seq# => one we hold */	
	
						/*
						 * Check if we have already got this information, 
						 * if so ignore it. We don't worry about modulus 
						 * for sub sequence numbers
						 */
						if ( (ntohs(nodehdr->seq_no) == rtr->seq_no) && (nodehdr->sub_seq_no < rtr->sub_seq_no) ) {
							new_info = 0;
							old_info = 1;
						} 
						/* If what we are given is exactly what we already hold,
						 * Then unset the new info bit so we ignore it but don't send it.
						 */
						if ( (ntohs(nodehdr->seq_no) == rtr->seq_no) && (nodehdr->sub_seq_no == rtr->sub_seq_no) )
						{
							new_info = 0;
							old_info = 0;
						}
						/* Zero means poll*/
						if ( nodehdr->seq_no == 0)
						{
							new_info = 0;
							old_info = 1;
						}
					} else {	
						/* rtr is NULL, make new entry */	
						rtr = (struct router*)malloc(sizeof(struct router));
						if (rtr == NULL) {
							syslog(LOG_DAEMON | LOG_ERR, "recv_rspf_frag(): Memory squeze (%m)");
							if (frg != NULL)
							{
								free(frg);
								frg = NULL;
							}
							return;
						}
						add_qnode(routerq, (void*)rtr, rkey);
						rtr->addr = *(u_long*)(nodehdr->addr);
						
					}
					if (debug_mode)
					{
						printf("\tRouting bull %u:%u from %s. (We have %u:%u) ", ntohs(nodehdr->seq_no), nodehdr->sub_seq_no, in_ntoa(*(u_long*)nodehdr->addr), rtr->seq_no, rtr->sub_seq_no );
						if (new_info)
							printf("<new>\n");
						else if (old_info)
							printf("<old>\n");
						else 
							printf("<same>\n");
					}
					/* Update router table */
				} /* new_info */
			
			}
			else /* This is one of our routing updates! */
			{
				if (debug_mode)
					printf("\tRouting bull %u:%u from us! (We have %u:%u)\n ", ntohs(nodehdr->seq_no), nodehdr->sub_seq_no, rspf_stats.SequenceNumber, rspf_stats.SubSequenceNumber );
				new_info = 0;
				rtr = NULL;
				/*
				 * Crash recovery, if the received counter is 
				 * greater than our own, then we must of crashed
				 * if our counter is smallish
				 */
				if (ntohs(nodehdr->seq_no) > rspf_stats.SequenceNumber 
					&& ntohs(nodehdr->seq_no) != 0xffff && rspf_stats.SequenceNumber != 0)
				{
					syslog(LOG_DAEMON | LOG_WARNING, "recv_rspf_frag(): Updating sequence number from %u to %u \n", rspf_stats.SequenceNumber, ntohs(nodehdr->seq_no) + 1);
					rspf_stats.SequenceNumber = ntohs(nodehdr->seq_no) + 1;
					rspf_stats.SubSequenceNumber = 0;
				}
			}	
			
		}

					
		while(links > 0) {
			links--;
			if (adjs == 0) {
				/* Get next link header */
				if (bptr + RSPFLINK_LEN > endptr) {
					if (rth->frag < rth->frag_tot)
						save_frag(saddr, rth->frag, nodehdr, linkhdr, nodes, links, adjs, incomplete, ntohs(rth->env_no), new_info, old_info);
					if (frg != NULL)
					{
						free(frg);						
						frg = NULL;
					}
					return;
				}
				linkhdr = (struct rspflink_hdr*)bptr;
				bptr += RSPFLINK_LEN;
				adjs = linkhdr->adjacencies;
			}	
			while(adjs > 0) {
				adjs--;
				/* Get next adj header */
				if (bptr + RSPFADJ_LEN > endptr) {
					if (rth->frag < rth->frag_tot)
						save_frag(saddr, rth->frag, nodehdr, linkhdr, nodes, links, adjs, incomplete, ntohs(rth->env_no), new_info, old_info);
					if (frg != NULL)
					{
						free(frg);						
						frg = NULL;
					}
					return;			
				}
				adjhdr = (struct rspfadj_hdr*)bptr;
				bptr += RSPFADJ_LEN;
				if (new_info)
				{
					if (adjhdr->sig_bits > 32)
						syslog(LOG_DAEMON | LOG_WARNING, "recv_rspf_frag(): Incorrect number of bits from router %s.\n", in_ntoa(saddr));
					else
						add_link(*(u_long*)(nodehdr->addr), *(u_long*)(adjhdr->addr), (adjhdr->sig_bits & RSPFADJ_SIG_BITS), linkhdr->horizon-1, linkhdr->cost, ntohs(nodehdr->seq_no));				
				}
				if (adjhdr->sig_bits & RSPFADJ_LASTFLAG)
					break;
			} /* adjs */
			if (adjhdr != NULL && adjhdr->sig_bits & RSPFADJ_LASTFLAG)
				break;			
		} /* links */
		/* If we get to here and we have got all the right number of 
		 * adjs and this is a full routing bulletin
		 * then delete any other ones 
		 */
		if (new_info && links == 0 && adjs == 0 && nodehdr->sub_seq_no == 0)
			del_link_source(*(u_long*)(nodehdr->addr), ntohs(nodehdr->seq_no));

		/*
		 * We only update sequence numbers and route bull timestamp
		 * if we got the whole routing bulletin and its newer
		 */
		if (new_info && links == 0 && adjs == 0 & rtr != NULL)
		{
			rtr->seq_no = ntohs(nodehdr->seq_no);
			rtr->sub_seq_no = nodehdr->sub_seq_no;
			rtr->bulltime = now;			
		}
		/*
		 * We now have to see if this is new information for this node
		 * and if so forward it onto all other interfaces
		 */
		if (new_info)
		{
			if ((size = get_rtr_bull(*(u_long*)nodehdr->addr, newbuf + newcnt, sizeof(newbuf) - newcnt)) > 0)
			{
				newcnt += size;
				newnodes++;
			} else {
				syslog(LOG_ERR | LOG_DAEMON, "recv_rspf_frag() get_rtr_bull failed (%m)");
				return;
			}
		}	
		/* 
		 * We also save information about a router if we get old stuff
		 * so we can update the poor unfortunate who has been left behind
		 */
		if (old_info)
		{
			if ((size = get_rtr_bull(*(u_long*)nodehdr->addr, oldbuf + oldcnt, sizeof(oldbuf) - oldcnt)) > 0)
			{
				oldcnt += size;
				oldnodes++;
			} else {
				syslog(LOG_ERR | LOG_DAEMON, "recv_rspf_frag() get_rtr_bull failed (%m)");
				return;
			}
		}	
	} /* nodes */
	/* 
	 * After processing the incoming packet, we need to see if
	 * we launch SPF algorithm. We do this if the onflow count
	 * is set (ie there was new information)
	 */
	if (newcnt > 0)
		do_spf();
		
	/*
	 * If we have new information, send this along to all interfaces
	 * so all nodes are updated.
	 */
	if (newcnt > 0)
	{
		ifname = ifbuf;
		ifcount = rspf_ifaces(ifbuf, sizeof(ifbuf));
		while(ifcount-- > 0)
		{
			send_rspf_env(newbuf, newcnt, newnodes, ifname);
			ifname += strlen(ifname) + 1;
		}
	}
	/* Remove all the dead links */
	clean_dead_links();
	/* 
	 * If we received (and ignored) old information, send our newer 
	 * information onto the interface we got this old stuff, so the
	 * backward router gets hopefully updated.
	 */
	if (oldcnt > 0)
	{
		send_rspf_env(oldbuf, oldcnt, oldnodes, iface);	
	}
	if (frg != NULL)
	{
		free(frg);
		frg = NULL;
	}
} /* recv_rspf_frag */	
		

void save_frag(u_long addr, int frag, struct rspfnode_hdr *nodehdr, struct rspflink_hdr *linkhdr,
		int nodes, int links, int adjs, int incomplete, int env_no, int new_info, int old_info)
{
	struct rspf_frag *frg;
	char key[KEY_SIZE];

	if (debug_mode)
		printf("save_frag(): addr = %s, env_no = %d, frag = %d\n", in_ntoa(addr), env_no, frag);
	frg = (struct rspf_frag*)malloc(sizeof(struct rspf_frag));
	if (frg == NULL) {
		syslog(LOG_DAEMON | LOG_ERR, "save_frag(): Memory squeze (%m)");
		return;
	}
	/* Copy the data across */
	frg->addr = addr;			
	frg->env_no = env_no;
	frg->frag = frag;
	frg->nodehdr = *nodehdr;
	frg->linkhdr = *linkhdr;
	frg->nodes = nodes;
	frg->links = links;
	frg->adjs = adjs;
	frg->new_info = new_info;
	frg->old_info = old_info;
	frg->incomplete = incomplete;
	
	frg->timer_id = dq_add(rspf_stats.rspfReasmTimeout, reasm_timer);
	
	strcpy(key, in_ntoa(addr));
	add_qnode(fragq, (void*)frg, key);
	
	/* Update stats, but only do it once for each envelope */
	if (frag == 1)
		rspf_stats.rspfReasmReqds++;
		
} /* save_frag */
	
/* 
 * do_spf()
 *
 * Impementation of Djikstra's algorithm for finiding the shortest path
 *
 * Returns:
 *	Nothing
 *
 * Arguments:
 *	Nothing
 */
void do_spf()
{
	struct queue *trial;
	struct queue *path;
	struct path *tptr, *pptr;
	struct link *lptr;
	struct rspf_adj *adjptr;
	struct nodegroup *ngptr;
	qmark link_qm, path_qm, trial_qm, adj_qm, ng_qm;
	qmark best_trial;
	u_char best_cost;
	u_long parent;		/* Last one put into the path table */
	u_long adjacency;	/* Adjacent router from parent */
	u_char pcost;		/* cost to parent */

	if (debug_mode)
		printf("do_spf(): entering function\n");

	trial = create_queue();
	path = create_queue();
	
	parent = adjacency = INADDR_NONE;
	pcost = 0;
	
	do {	
		/*
		 * Step 1
		 * Scan link state table, looking for all entries that have source
		 * address as parent and no better router already
		 */
		if (parent == INADDR_NONE)
		{
			/* Special case, we are the router 
			 * First we add all of our adjacencies
			 */
			adjptr = (struct rspf_adj*)qmove_first(adj_queue, &adj_qm);
			while(adjptr != NULL)
			{
				if (adjptr->status == Ok)
				{
					tptr = (struct path*)malloc(sizeof(struct path));
					if (tptr == NULL) 
					{
						syslog(LOG_DAEMON | LOG_ERR, "do_spf(tptr) Memory Squeze (%m)");
						return;
					}
					tptr->daddr = adjptr->addr;
					tptr->adjaddr = adjptr->addr;
					tptr->sigbits = 32;
					tptr->paddr = adjptr->addr;
					tptr->cost = get_iface_cost(adjptr->port);
					add_qnode(trial, (void*)tptr, NULL);
				}
				adjptr = (struct rspf_adj*)qmove_next(adj_queue, &adj_qm);
			} /* adjptr != NULL */
			/*
			 * Now we add the node groups to the trial table
			 */
			ngptr = (struct nodegroup*)qmove_first(nodgrpq, &ng_qm);
			while(ngptr != NULL)
			{
				tptr = (struct path*)malloc(sizeof(struct path));
				if (tptr == NULL) 
				{
					syslog(LOG_DAEMON | LOG_ERR, "do_spf(tptr) Memory Squeze (%m)");
					return;
				}
				tptr->daddr = ngptr->addr;
				tptr->adjaddr = ngptr->addr;
				tptr->sigbits = ngptr->sigbits;
				tptr->paddr = ngptr->addr;
				tptr->cost = ngptr->cost;
				add_qnode(trial, (void*)tptr, NULL);
				
				ngptr = (struct nodegroup*)qmove_next(nodgrpq, &ng_qm);
			}
		} else {	/* not first time */
			lptr = (struct link*)qmove_first(linkq, &link_qm);	 
			while(lptr != NULL) 
			{
				if (lptr->saddr == parent) 
				{
					/* Make sure that this is best router to dest */
					tptr = (struct path*)qmove_first(trial, &trial_qm);
					while(tptr != NULL) 
					{
						if (tptr->daddr == lptr->daddr && tptr->sigbits == lptr->sigbits) 
						{
							if (tptr->cost > lptr->cost + pcost) 
							{
								del_qnode(trial, trial_qm, 1);
								tptr = NULL;
								break;
							} else if (tptr->cost < lptr->cost + pcost) 
								{
									/* The one we have is better */
									break;
								} else  /* tie-breaker is ip address */
									if (tptr->adjaddr < adjacency) 
									{
										/* This one is 'better' */
										break;
									} else 
									{
										del_qnode(trial, trial_qm, 1);
										free(tptr);
										tptr = NULL;
										break;
									}
						} /* match of daddr */
						tptr = (struct path*)qmove_next(trial, &trial_qm);	
					} /* while tptr */
					/* 
					 * We now check to see if there is a path already in
					 * the paths table for this destination
					 */
					pptr = (struct path*)qmove_first(path, &path_qm);
					while(pptr != NULL)
					{
						if (pptr->daddr == lptr->daddr && pptr->sigbits == lptr->sigbits)
						{
							/* We've found a path in the paths table*/
							break;
						}
						pptr = (struct path*)qmove_next(path, &path_qm);
					}
					/* 
					 * If there is no better path in either the trial or paths table..
					 */
					if (tptr == NULL && pptr == NULL)  
					{ 
						tptr = (struct path*)malloc(sizeof(struct path));
						if (tptr == NULL) 
						{
							syslog(LOG_DAEMON | LOG_ERR, "do_spf(tptr) Memory Squeze (%m)");
							return;
						}
						tptr->daddr = lptr->daddr;
						tptr->adjaddr = adjacency;
						tptr->sigbits = lptr->sigbits;
						tptr->paddr = parent;
						tptr->cost = lptr->cost + pcost;
						add_qnode(trial, (void*)tptr, NULL);
					} /* tptr = NULL */
						
			} /* saddr = parent */
				lptr = (struct link*)qmove_next(linkq, &link_qm);
			} /* while lptr */
		} /* not intial one */
		/* 
		 * Step 2
		 * Scan trial table and look for the entry with the smallest
		 * overall cost
		 */
		best_trial = NULL;
		best_cost = 0xff;
		tptr = (struct path*)qmove_first(trial, &trial_qm);
		while(tptr != NULL) 
		{
			if (best_cost > tptr->cost) 
			{
				best_trial = trial_qm;
				best_cost = tptr->cost;
			}
			tptr = (struct path*)qmove_next(trial, &trial_qm);
		}
		if (best_trial == NULL) 
		{
			/* we've finished */
			break;
		}
		tptr = q_entry(trial, best_trial);
		/* Move from trial table to paths table 
		 * Do not free trial node as it is moved */
		del_qnode(trial, best_trial, 0);
		add_qnode(path, (void*)tptr, NULL);
	
		/* Make new parent */
		parent = tptr->daddr;
		pcost = tptr->cost;		
		adjacency = tptr->adjaddr;		
	} while(tptr != NULL);

	/*
	 * OK, we have to add all the routes into the kernels routing table
	 */
	nuke_routes();
	pptr = (struct path*)qmove_first(path, &path_qm);
	while(pptr != NULL)
	{
		if (pptr->daddr != pptr->adjaddr)
			add_route(pptr->daddr, pptr->sigbits, pptr->adjaddr, pptr->cost);
		else 
			add_route(pptr->daddr, pptr->sigbits, 0, pptr->cost);
		del_qnode(path, path_qm, 1);			
		pptr = (struct path*)qmove_first(path, &path_qm);
	}
 	/* 
 	 * Now we have to clean up all the queues so we don't get memory leaks
 	 */
 	nuke_queue(trial, 1);
 	nuke_queue(path, 1);
 	
 	if (debug_mode)
 		printf("do_spf(): exiting function.\n");
 		
} /* do_spf()*/

/*
 * get_rtr_bull()
 *
 * Creates the routing bulletin for specified router
 *
 * Returns:
 *	Modifed Arguments 
 *
 * Arguments:
 *	u_long		IP address of router
 *	u_char*		Buffer to put bulletin in (modified)
 *	int*		Max size of buffer, modifed to return size used
 */
int get_rtr_bull(u_long addr, u_char *buf, int len)
{
	u_char *endbuf = buf + len;
	u_char *bptr = buf;
	struct link *lnk;
	qmark lnk_qm;
	struct rspfnode_hdr *nodehdr;
	struct rspflink_hdr *linkhdr;
	struct rspfadj_hdr *adjhdr;
	struct router *rtr;
	qmark rtr_qm;
	int links, adjs, cost, horizon, prevcost, prevhorizon;
	char ifbuf[128];
	char *ifname;
	int ifcount;

	if (debug_mode)
		printf("get_rtr_bull*(): addr = %s,  buflen = %u.\n", in_ntoa(addr), len);

	/*
	 * Check to see if the specified address is for one of the interface
	 * of our router.  We don't have to check the return value as our 
	 * caller will do so.
	 */
	ifname = ifbuf;
	ifcount = rspf_ifaces(ifbuf, 128);
	while(ifcount-- > 0)
	{
		if (get_iface_addr(ifname) == addr)
		{
			return get_my_bull(addr, buf, len);
		}
		ifname += strlen(ifname) + 1;
	}
	if (endbuf - bptr < RSPFNODE_LEN)
	{
		syslog(LOG_DAEMON | LOG_WARNING, "get_rtr_bull(): Buffer not big enough (%d bytes)", (int)(endbuf-bptr));
		return -1;
	}	
	nodehdr = (struct rspfnode_hdr*) bptr;
	linkhdr = NULL;
	adjhdr = NULL;
	bptr += RSPFNODE_LEN;

	rtr = (struct router*)qmove_first(routerq, &rtr_qm);
	while(rtr != NULL)
	{
		if (rtr->addr == addr)
			break;
		rtr = (struct router*)qmove_next(routerq, &rtr_qm);
	}
	if (rtr == NULL)
	{
		syslog(LOG_DAEMON | LOG_ERR, "get_rtr_bull(): Cannot find router %s", in_ntoa(rtr->addr));
		return -1;
	}
	nodehdr->seq_no = htons(rtr->seq_no);
	nodehdr->sub_seq_no = rtr->sub_seq_no;

	prevcost = cost = prevhorizon = horizon = links = adjs = 0;		
	bcopy((char*)&addr, nodehdr->addr, 4);
		
	lnk= (struct link*)qmove_first(linkq, &lnk_qm);
	while(lnk != NULL)
	{
		if (lnk->saddr == addr)
		{
			if (horizon == 0 && cost == 0)
			{
				/* New link, perhaps */
				if (lnk->cost > prevcost || (lnk->cost == prevcost && lnk->horizon > prevhorizon))
				{
					/* This is a new link */
					if (linkhdr != NULL)
						linkhdr->adjacencies = adjs;
					adjs = 0;
					links++;
					if (endbuf - bptr < RSPFLINK_LEN)
					{
						syslog(LOG_DAEMON | LOG_WARNING, "get_rtr_bull(): Ran out of buffer at link.");
						return -1;
					}

					linkhdr	= (struct rspflink_hdr*)bptr;
					bptr += RSPFLINK_LEN;
					horizon = linkhdr->horizon = lnk->horizon;
					linkhdr->erp = 0;
					cost = linkhdr->cost = lnk->cost;
				}	
			}					
			if (lnk->horizon == horizon && lnk->cost == cost)
			{
				adjhdr = (struct rspfadj_hdr*)bptr;
				bptr += RSPFADJ_LEN;
				if (endbuf - bptr < 0)
				{
					syslog(LOG_DAEMON | LOG_WARNING, "get_rtr_bull(): Ran out of buffer at adj.");
					return -1;
				}
				adjs++;
				adjhdr->sig_bits = lnk->sigbits;
				bcopy((char*)&lnk->daddr, adjhdr->addr, 4);
			}
		} /* saddr = addr */
		lnk = (struct link*)qmove_next(linkq, &lnk_qm);
		if (lnk == NULL && cost == 0 && horizon == 0)
			break;
		
		if (lnk == NULL)
		{	
			/* Roll back pointers and set memory of horizon and cost */
			if(linkhdr != NULL)
				linkhdr->adjacencies = adjs;
			adjs = 0;	
			prevcost = cost;
			prevhorizon = horizon;
			cost = horizon = 0;
			lnk = (struct link*)qmove_first(linkq, &lnk_qm);
		}
	} /*while */
	nodehdr->links = links;
	/* Set the number of adjacencies in last link,  it may not be set yet */
	if (adjs != 0 && linkhdr != NULL)
		linkhdr->adjacencies = adjs;
		
	/* Have to set the last flag of the last adjacency */
	if (adjhdr != NULL)
		adjhdr->sig_bits |= RSPFADJ_LASTFLAG;
	return (int)(bptr - buf); 
} /* get_rtr_bull */

int get_my_bull(u_long addr, u_char *buf, int len)
{
	u_char *endbuf = buf + len;
	u_char *bptr = buf;
	
	struct rspfnode_hdr *nodehdr;
	struct rspflink_hdr *linkhdr;
	struct rspfadj_hdr *adjhdr;
	
	struct rspf_adj *adj;
	struct nodegroup *ng;
	qmark adj_qm, ng_qm;
	
	int links, adjs, cost, horizon, prevcost, prevhorizon;
	
	if (debug_mode)
		printf("get_my_bull(): Have %d byte buffer, ", len);
	
	nodehdr = (struct rspfnode_hdr*) bptr;
	linkhdr = NULL;
	adjhdr = NULL;
	bptr += RSPFNODE_LEN;
	
	bcopy((char*)&addr, nodehdr->addr, 4);
	nodehdr->seq_no = htons(rspf_stats.SequenceNumber);
	nodehdr->sub_seq_no = rspf_stats.SubSequenceNumber;	
	
	links = adjs = 0;
	prevcost = prevhorizon = 0;
	cost = horizon = 255;
	
	while(1)
	{
		/* Find the smallest cost or cost and horizon */
		adj = (struct rspf_adj*)qmove_first(adj_queue, &adj_qm);
		while(adj != NULL)
		{
			if (adj->cost < cost || (adj->cost == cost && adj->horizon < horizon))
			{
				/* Make sure that the cost and horizon are above what we 
				 * have already processed
				 */
				if (adj->cost > prevcost || (adj->cost == prevcost && adj->horizon > horizon) )
				{
					cost = adj->cost;
					horizon = adj->horizon;
				}
			}
			adj = (struct rspf_adj*)qmove_next(adj_queue, &adj_qm);
		}

		/* Next we do the same for the node groups */
		ng = (struct nodegroup*)qmove_first(nodgrpq, &ng_qm);
		while(ng != NULL)
		{
			if (ng->cost < cost || (ng->cost == cost && rspf_stats.rspfGroupHorizon < horizon))
			{
				/* Make sure that the cost and horizon are above what we 
				 * have already processed
				 */
				if (ng->cost > prevcost || (ng->cost == prevcost && rspf_stats.rspfGroupHorizon > horizon) )
				{
					cost = ng->cost;
					horizon = rspf_stats.rspfGroupHorizon;
				}
			}
			ng = (struct nodegroup*)qmove_next(nodgrpq, &ng_qm);
		}	
		/* 
		 * Stop here if we cannot find any more adjacencies		
		 */
		if ((cost == prevcost && horizon == prevhorizon) || cost == 255)
		{
			nodehdr->links = links;
			break;	
		}
		
		if (endbuf - bptr < RSPFLINK_LEN)
		{
			syslog(LOG_DAEMON | LOG_WARNING, "get_my_bull(): Ran out of buffer at link.");
			return -1;
		}
		linkhdr = (struct rspflink_hdr*)bptr;
		bptr += RSPFLINK_LEN;
		linkhdr->horizon = horizon;
		linkhdr->erp = 0;
		linkhdr->cost = cost;
		linkhdr->adjacencies = adjs = 0;
		links++;
		/* Find all the adjacencies with this horizon and cost */					
		
		adj = (struct rspf_adj*)qmove_first(adj_queue, &adj_qm);		
		while(adj != NULL)
		{
			if (adj->cost == cost && adj->horizon == horizon)
			{
				if (endbuf - bptr < RSPFADJ_LEN)
				{
					syslog(LOG_DAEMON | LOG_WARNING, "get_my_bull(): Ran out of buffer at adj.");
					return -1;
				}
				adjhdr = (struct rspfadj_hdr*)bptr;
				bptr += RSPFADJ_LEN;
				adjs++;
				adjhdr->sig_bits = 32;
				bcopy((char*)&adj->addr, adjhdr->addr, 4);
			}				
			adj = (struct rspf_adj*)qmove_next(adj_queue, &adj_qm);
		} /* while adj */
		
		/* If we have the right horizon for node groups, we add them */
		if (horizon == rspf_stats.rspfGroupHorizon)
		{
			ng = (struct nodegroup*)qmove_first(nodgrpq, &ng_qm);		
			while(ng != NULL)
			{
				if (ng->cost == cost) /* Node groups have same horizon */
				{
					if (endbuf - bptr < RSPFADJ_LEN)
					{
						syslog(LOG_DAEMON | LOG_WARNING, "get_my_bull(): Ran out of buffer at adj.");
						return -1;
					}
					adjhdr = (struct rspfadj_hdr*)bptr;
					bptr += RSPFADJ_LEN;
					adjs++;
					adjhdr->sig_bits = ng->sigbits;
					bcopy( (char*)&ng->addr, adjhdr->addr, 4);
				}				
				ng = (struct nodegroup*)qmove_next(nodgrpq, &ng_qm);
			} /* while ng */
		} /* horizon = group */
		linkhdr->adjacencies = adjs;
		prevcost = cost;
		prevhorizon = horizon;
		cost = 255;
	} /* while 1 */
	if (debug_mode)
		printf(" used %d bytes.\n", (int)(bptr - buf));
	nodehdr->links = links;
	return (int)(bptr - buf);
} /* get_my_bull */


u_char get_horizon(u_long addr, char *iface)
{
	struct router *rtr;
	qmark rtr_qm, ng_qm;
	struct nodegroup *nptr;
	
	
	/* 
	 * See if node is a router, if so use link horizon
	 */
	rtr = (struct router*)qmove_first(routerq, &rtr_qm);
	while(rtr != NULL)
	{
		if (rtr->addr == addr)
			return rspf_stats.rspfLinkHorizon;
		rtr = (struct router*)qmove_next(routerq, &rtr_qm);
	}
	/*
	 * Next is Locals
	 */
	nptr = (struct nodegroup*)qmove_first(nodgrpq, &ng_qm);
	while(nptr != NULL)
	{
		/* 
		 * Must take into account sig bits
		 */
		if ( (nptr->addr << (32 - nptr->sigbits)) == (addr << (32 - nptr->sigbits)) )
			return rspf_stats.rspfLocalHorizon;
		
		nptr = (struct nodegroup*)qmove_next(nodgrpq, &ng_qm);
	}		
	/*
	 * Anything else is a Portable 
	 */
	return rspf_stats.rspfPortableHorizon;
} /* get_horizon */

/*
 * check_routers()
 *
 * Check any routers that we have not heard any information from for
 * a long time and remove them.  This is so we don't have routers in 
 * our table forever
 *
 * Returns:
 *	Nothing
 *
 * Arguments:
 *	int	Timer id
 */
void check_routers(int id)
{ 
	static int timer_id = 0;
	struct router *rtr;
	time_t now;
	qmark rtr_qm;
	u_int mintime;	
	int ls_changed;
	
	if (debug_mode)
		printf("check_routers(): id = %#3x.\n", id);
		
	/*
	 * Sanity check for the timer id
	 */
	if (timer_id != id)
	{
		syslog(LOG_DAEMON | LOG_ERR, "check_routers(): Incorrect timer ID. Got %#3x, expected %#3x.\n", id, timer_id);
		return;
	}
			
	/* The most we will wait before checking again is a set parameter */
	mintime = rspf_stats.rspfBullTimeout;
	ls_changed = 0;

	rtr = (struct router*)qmove_first(routerq, &rtr_qm);		
	while (rtr != NULL) {

		if ( (now - rtr->bulltime) > rspf_stats.rspfBullTimeout ) 
		{
			/* Remove all links for this router */
			del_link_source(rtr->addr, rtr->seq_no + 1);
			del_qnode(routerq, rtr_qm, 1);
			ls_changed = 1;
			rtr = (struct router*)qmove_first(routerq, &rtr_qm);
		} else 
		{
			/* Determine when we have to next look here 
			 * in other words when the next router will be 
			 * lost
			 */
			if (mintime > (rspf_stats.rspfBullTimeout - (u_int)(now - rtr->bulltime)) )
				mintime = rspf_stats.rspfSusTimeout - (u_int)(now - rtr->bulltime);
			/* Move onto next one */
			rtr = (struct router*)qmove_next(routerq, &rtr_qm);
		}

	}

	/* 
	 * We have to do  SPF if we have removed any links
	 */
	if (ls_changed)
		do_spf();
		
	/* Setup timer for next check 
	 * mintime is the number of seconds before any node can be
	 * suspect, so we don't bother to come back until that happens
	 */
	if (mintime < 1)
		mintime = 1;
	timer_id = dq_add(mintime, check_routers);

} /* check_routers */

void add_nodegroup(u_long addr, u_int sigbits, u_int cost, char *iface)
{
	struct nodegroup *nptr;
	qmark ng_qm;

	if (debug_mode)
		printf("add_nodegroup(): addr = %s/%u  cost = %u.\n", in_ntoa(addr), sigbits, cost);
		
	nptr = (struct nodegroup*)qmove_first(nodgrpq, &ng_qm);
	while(nptr != NULL)
	{
		if (nptr->addr == addr && nptr->sigbits == sigbits)
			break;
		
		nptr = (struct nodegroup*)qmove_next(nodgrpq, &ng_qm);
	}
	if (nptr == NULL)
	{
		if ( (nptr = (struct nodegroup*)malloc(sizeof(struct nodegroup))) == NULL)
		{
			syslog(LOG_DAEMON | LOG_ERR, "add_nodegroup(): memory squeze, dropping nodegroup (%m)");
			return;
		}
		nptr->addr = addr;
		nptr->sigbits = sigbits;
		add_qnode(nodgrpq, (void*)nptr, NULL);
	}
	nptr->cost = cost;
	strcpy(nptr->iface, iface);
	add_route(addr, sigbits, 0, cost);
} /* add_nodegroup */

	
