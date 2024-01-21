/*
 * $Id: packet.h,v 1.25 1995/02/26 19:21:02 begemot Exp $
 * $Log: packet.h,v $
 * Revision 1.25  1995/02/26  19:21:02  begemot
 * Nothing important
 *
 * Revision 1.24  1995/02/26  18:59:25  begemot
 * Added RCS Id & Log entries into the all source files.
 *
 */

/*
 * Copyright (C) 1994-1996 D.Gorodchanin. See COPYING for more info.
 */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <limits.h>
#include <strings.h>
#include <signal.h>
#include <netdb.h>
#include <unistd.h>
#include <curses.h>
#include <pwd.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/types.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <linux/if_ether.h>
#include <linux/if.h>
#include <linux/ip.h>

/* Start of user defines section */

#define SCREEN_UPDATE   3	       /* Screen update interval (sec) */
#define CHANNEL_REMOVE  10	       /* Inactive channel remove from screen (sec) */
#define CHANNEL_FORGET  180            /* Inactive channel drop time (sec) */
#define CHANNELS_COUNT  100	       /* Max. channels in cash  */
#define HOSTS_COUNT     50	       /* Max. hostnames in cash */
#define MIN_LINES	3	       /* Min. lines on terminal */
#define MIN_RAWS	80	       /* Min. columns on terminal */
#define MAX_PACKET_LEN	8192	       /* Max of MTU for all devices */

/* End of user defines section */

#define _PATH_TCP_INFO "/proc/net/tcp"
#define _PATH_UDP_INFO "/proc/net/udp"

#define MAX_IF_NAME	4
#define MAX_SERV_NAME	6
#define MAX_USER_NAME   8
#define MAX_PROT_NAME	4
#define MAX_DATA_SIZE   10			 
#define MAX_HOST_NAME   ((MIN_RAWS - 2 * \
			  (MAX_DATA_SIZE + 1 + MAX_SERV_NAME + 1) \
			  - MAX_PROT_NAME - 1 - 1) / 2 )  

struct channel_entry  {
	unsigned long saddr;
	unsigned long daddr;
	unsigned long sport;
	unsigned long dport;
	unsigned long in;
	unsigned long out;
	unsigned long scr_seq;
	time_t   tm;
	unsigned short next;
	unsigned char proto;
	unsigned char line;
	unsigned char sserv[MAX_SERV_NAME+1];
	unsigned char dserv[MAX_SERV_NAME+1];
	unsigned char suser[MAX_USER_NAME+1];
	unsigned char duser[MAX_USER_NAME+1];
	unsigned char ifname[MAX_IF_NAME+1];
	unsigned char get_user_try;
};

extern int update_interval;
extern int remove_interval;
extern int forget_interval;
extern int dont_resolve;
extern int force_mono;
extern unsigned char iface[MAX_IF_NAME+1];
extern time_t start;
extern time_t now;

/* table.c */
void update_channels (unsigned long const saddr,
		      unsigned long const daddr,
		      unsigned short const sport,
		      unsigned short const dport,
		      unsigned char const proto,
		      int const size,
		      unsigned char const * const ifname);

int get_channels_list(struct channel_entry * * const list,
		      int const size);

void init_channels_table( void );

/* host.c */
unsigned short ip_fast_csum(unsigned char const * const buff, int const wlen);
char const * get_host_name(unsigned long const addr);

/* screen.c */
void screen_open( void );
void screen_update(unsigned long const total, unsigned long const ill, 
		   unsigned long const frag, unsigned long const tcp,
		   unsigned long const udp, unsigned long const icmp,
		   unsigned long const unkn ); 
void screen_close( void );
