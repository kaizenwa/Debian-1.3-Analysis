/*
 * $Id: trafshow.c,v 1.30 1995/11/09 15:30:00 begemot Exp $
 * $Log: trafshow.c,v $
 * Revision 1.30  1995/11/09  15:30:00  begemot
 * Strict option checking.
 *
 * Revision 1.29  1995/02/26  19:55:45  begemot
 * ICMP reqest-reply packets now displayed as single line.
 *
 * Revision 1.28  1995/02/26  18:59:25  begemot
 * Added RCS Id & Log entries into the all source files.
 *
*/

/*
 * Copyright (C) 1994-1996 D.Gorodchanin. See COPYING for more info.
 */

#include "trafshow.h"

static unsigned long total  = 0;
static unsigned long ill    = 0;
static unsigned long frag   = 0;
static unsigned long tcp    = 0;
static unsigned long udp    = 0;
static unsigned long icmp   = 0;
static unsigned long unkn   = 0;

int update_interval = SCREEN_UPDATE;
int remove_interval = CHANNEL_REMOVE;
int forget_interval = CHANNEL_FORGET;
unsigned char iface[MAX_IF_NAME+1] = "";
int force_mono = 0;
int dont_resolve = 0;

time_t start;
time_t now;

static void quit (int const signal)
{
	screen_close();
	exit(0);
}

static void error (char const * const where)
{
	int err = errno;
	
	screen_close();
	if ((errno = err) != 0) {
		perror(where);
	} else  {
		fprintf(stderr, where);
	}
	exit(1);
}

static int open_packet_socket()
{
	int fd;

	fd = socket (AF_INET, SOCK_PACKET, htons(ETH_P_ALL));
	if (fd < 0) {
		error("open");
	}
	return fd;
}

static int read_packet(const int fd, unsigned char * const buffer, 
		       unsigned char * * const packet,
		       unsigned char * const ifname)
{
	int len;
	int fromlen; 
	struct sockaddr fromaddr;

	do {
		fromlen = sizeof(fromaddr);
		len = recvfrom(fd, buffer, MAX_PACKET_LEN , 0,
			       &fromaddr, &fromlen);
	} while (len < 0 && errno == EINTR);
	
	if (len < 0)  {
		error("recvfrom");
	}
	
	if (*iface && strcmp(iface, fromaddr.sa_data))  {
		return 0;
	}
	
	if ( fromaddr.sa_family == 1) {
		*packet = buffer + ETH_HLEN;
		len -= ETH_HLEN;
	} else  {
		*packet = buffer;
	}
	
	strcpy(ifname, fromaddr.sa_data);

	return len;
}

static void update_packets(unsigned char const * const packet, int const len,
			   unsigned char const * const ifname)
{
	
	struct iphdr const * const iph = (struct iphdr const * const) packet;
	
	total += len;

	if (len < sizeof(iph) || (iph->ihl * 4 > len) || 
	    ip_fast_csum((unsigned char const * const)iph, iph->ihl))  {
		ill += len;
		return;
	}
	
	if (iph->frag_off & 0xff1f /* htons (frag offset) */ )  {
		frag += len;
		return;
	}
	
	switch (iph->protocol)  {
		
	 case IPPROTO_TCP :
		{
			unsigned short *p = 
			(unsigned short *)(((long *)iph) + iph->ihl);
			
			tcp += len;
			update_channels(iph->saddr, iph->daddr,
					p[0], p[1], iph->protocol,
					len, ifname);
		}
		break;
		
	 case IPPROTO_UDP :
		{
			unsigned short *p = 
			(unsigned short *)(((long *)iph) + iph->ihl);
			
			udp += len;
			update_channels(iph->saddr, iph->daddr,
					p[0], p[1], iph->protocol,
					len, ifname);
		}
		break;
		
	 case IPPROTO_ICMP:
		{
			static int icmp_complement[19] =  { 
				8,	/*  0: echo    - echorq        */
				0xffff, /*  1: unknown - unknown       */
				0xffff, /*  2: unknown - unknown       */
				0xffff, /*  3: destun  - unknown       */
				0xffff, /*  4: sqnch   - unknown       */
				0xffff, /*  5: redir   - unknown       */
				0xffff, /*  6: unknown - unknown       */
				0xffff, /*  7: unknown - unknown       */
				0,	/*  8: echorq  - echo          */
				0xffff, /*  9: unknown - unknown       */
				0xffff, /* 10: unknown - unknown       */
				0xffff, /* 11: timexd  - unknown       */
				0xffff, /* 12: parmpb  - unknown       */
				0x14,   /* 13: timerq  - time          */
				0x13,   /* 14: time    - timerq        */
				0x16,   /* 15: inforq  - info          */
				0x15,   /* 16: info    - inforq        */
				0x18,   /* 17: addrrq  - addr          */
				0x17,   /* 18: addr    - addrrq        */
			};
			int type = 
			*((unsigned char *)(((long *)iph) + iph->ihl));
				
			icmp += len;
			update_channels(iph->saddr, iph->daddr,
					type, type > 18 ? 0xffff : icmp_complement[type],
					iph->protocol,
					len, ifname);
		}
		break;
		
	 default:
		unkn += len;
		update_channels(iph->saddr, iph->daddr,
				0, 0, iph->protocol, 
				len, ifname);
		break;
	}
	
}

static void usage(const char * name)
{
	printf( "\nUsage: %s <options>\n\n"
		"Options:\n"
		"\t-m\t\tforce monochrome mode\n"
		"\t-n\t\tdon't resolve hostnames\n"
		"\t-i <iface>\tshow traffic only for specified interface\n"
		"\t-f <seconds>\tbefore remove inactive connection from internal tables\n"
		"\t-r <seconds>\tbefore remove inactive connection from screen\n"
		"\t-u <secends>\tscreen update interval\n\n"
		"Version 1.0, Copyright (C) D.Gorodchanin 1994-1996.\n"
		, name);
}

void main(int const argc, char * const argv[])
{
	unsigned char buffer[MAX_PACKET_LEN];
	unsigned char *packet;
	unsigned char ifname[MAX_IF_NAME + 1];
	int  pfd;
	int  size;
	fd_set set;
	struct timeval timeout;
	int nfd;
	int c;
	time_t next = 0;

	while ((c = getopt(argc, argv, "?hf:i:mnr:u:")) > 0)  {
		switch (c)  {
		 case 'f':
			forget_interval = atoi(optarg);
			break;
		 case 'i':
			strncpy(iface, optarg, MAX_IF_NAME);
			iface[MAX_IF_NAME] = 0;
			break;
		 case 'm':
			force_mono = 1;
			break;
		 case 'n':
			dont_resolve = 1;
			break;
		 case 'r':
			remove_interval = atoi(optarg);
			break;
		 case 'u':
			update_interval = atoi(optarg);
			break;
		 case 'h':
		 case '?':
		        usage(argv[0]);
			exit(1);
		}
	}
	
	if (optind < argc)  {
		fprintf(stderr, "Invalid argument `%s', try `%s -h' for help \n", argv[optind], argv[0]);
		exit(1);
	}
	
	time(&start);
	
	screen_open();
	
	pfd = open_packet_socket();
	init_channels_table();
	
	signal(SIGINT, quit);
	signal(SIGTERM, quit);
	
	for (;;)  {
		
		time(&now);
		
		FD_ZERO(&set);
		FD_SET(pfd, &set);
		FD_SET(fileno(stdin), &set);
		
		timeout.tv_sec = next > now ? next - now : 0;
		timeout.tv_usec = 0;
		
		if (timeout.tv_sec > update_interval)  {
			/* Someone changed system clock */ 
			timeout.tv_sec = update_interval;
		}
		
		do  {
			nfd = select(pfd + 1, &set, NULL, NULL, &timeout);
		} while (nfd < 0 && errno == EINTR);
		if (nfd < 0)  {
			error("select");
		}
		
		if (FD_ISSET(pfd, &set))  {
			if ((size = read_packet(pfd, buffer, &packet, ifname)))  {
				update_packets(packet, size, ifname);
			}
		}
		
		if (FD_ISSET(fileno(stdin), &set))  {
			read(fileno(stdin), buffer, sizeof(buffer));
			screen_close();
			screen_open();
		}
		
		if (timeout.tv_sec == 0 && timeout.tv_usec == 0)  {
			screen_update(total, ill, frag, tcp, udp, icmp, unkn);
			next = now + update_interval;
		}
	}
}
