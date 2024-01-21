/*
    IPX routing daemon

    Copyright (C) 1996, Volker Lendecke <lendecke@namu01.gwdg.de>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#include <signal.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include "ipxd.h"
#include "ipxripd.h"
#include "ipxsapd.h"

#define DEVICE_LENGTH (31)
#define MAX_DEVICE    MAX_IFACE
struct ticks_entry {
	char device_name[DEVICE_LENGTH+1];
	int  ticks;
};

static struct ipx_interface ifaces[MAX_IFACE];
int time_since_last_bcast=0;
static struct rip_packet rip_in_buf;
static struct sap_packet sap_in_buf;

static struct ticks_entry ticks_table[MAX_DEVICE];
int ticks_entries=0;
static char default_ticks_file[] = TICKS_FILE;
static char *ticks_file = default_ticks_file;

int passive = 0;

volatile int check_request=0;
static volatile int dump_request=0;
static volatile int timer_request=0;
static volatile int terminate=0;
static volatile int scan_ticks_request=0;

int new_log_entry=1;
int debug_option=0;
FILE *log_file=stderr;

static IPXNet
ifc_net(struct ipx_interface *ifc)
{
	return ntohl(ifc->r_output.dest_addr.sipx_network);
}

static void
int_handler()
{
	signal(SIGINT,int_handler);
	dump_request=1;
}

static void
timer_handler()
{
	signal(SIGALRM,timer_handler);
	timer_request=1;
	check_request=1;
}

static void
terminate_handler()
{
	signal(SIGTERM,terminate_handler);
	terminate=1;
}

static void
hup_handler()
{
	signal(SIGHUP,hup_handler);
	scan_ticks_request=1;
	check_request=1;
}

struct ipx_interface *
first_interface(void)
{
	int i = first_ifc_index();

	if (i < 0)
	{
		return NULL;
	}

	return &(ifaces[i]);
}

struct ipx_interface *
next_interface(struct ipx_interface *ifc)
{
	int i = next_ifc_index(ifc_get_index(ifc));

	if (i < 0)
	{
		return NULL;
	}

	return &(ifaces[i]);
}

int
ifc_get_index(struct ipx_interface *ifc)
{
	if (ifc == NULL)
	{
		return -1;
	}
	return ifc - ifaces;
}

int
first_ifc_index(void)
{
	int i;

	for (i = 0; i < MAX_IFACE; i += 1)
	{
		if (ifaces[i].valid != 0)
		{
			return i;
		}
	}
	return -1;
}

int
next_ifc_index(int i)
{
	if ((i < 0) || (i >= MAX_IFACE-1))
	{
		return -1;
	}

	i = i+1;

	while (i < MAX_IFACE)
	{
		if (ifaces[i].valid != 0)
		{
			return i;
		}
		i = i+1;
	}
	return -1;
}

static int
first_free_ifc_index(void)
{
	int i;

	for (i = 0; i < MAX_IFACE; i += 1)
	{
		if (ifaces[i].valid == 0)
		{
			return i;
		}
	}
	return -1;
}

static void
read_ticks_table(void)
{
	char buf[512];
	char device[512];
	int ticks;
	FILE *f = fopen(ticks_file, "r");

	ticks_entries = 0;
	if (f == NULL)
	{
		return;
	}

	while (fgets(buf, sizeof(buf), f) != NULL)
	{
		if (   (strlen(buf) == 0)
		    || (buf[0] == '#'))
		{
			continue;
		}
		if (sscanf(buf, "%s %d", device, &ticks) != 2)
		{
			continue;
		}
		if (strlen(device) > DEVICE_LENGTH)
		{
			fclose(f);
			return;
		}
		strcpy(ticks_table[ticks_entries].device_name, device);
		ticks_table[ticks_entries].ticks = ticks;
		ticks_entries += 1;

		if (ticks_entries >= MAX_DEVICE)
		{
			fclose(f);
			return;
		}
	}
	fclose(f);
	return;
}

int
find_ticks(char *device)
{
	int i;
	for (i = 0; i < ticks_entries; i += 1)
	{
		if (strcmp(device, ticks_table[i].device_name) == 0)
		{
			return ticks_table[i].ticks;
		}
	}
	return DEFAULT_TICKS;
}

static void
dump_ticks(void)
{
	int i;

	LOG_ENTRY;
	LOG_START;
	fprintf(log_file, "Ticks table:\n");
	for (i = 0; i < ticks_entries; i += 1)
	{
		fprintf(log_file, "Device: %s, Ticks: %d\n",
			ticks_table[i].device_name,
			ticks_table[i].ticks);
	}
	LOG_END;
}

static int
ipx_init_interface(struct ipx_interface *ifc,
		   IPXNet network, IPXNode node,
		   char *device, int type, void *data)
{
	int result;

	ipx_assign_node(ifc->ifnode, node);

	ifc->ticks = find_ticks(device);

	if ((result = ipx_rip_init_ifc(ifc, network, device, type, data)) != 0)
	{
		return result;
	}
	if ((result = ipx_sap_init_ifc(ifc, network, device, type, data)) != 0)
	{
		return result;
	}
	ifc->valid = 1;
	return 0;
}

static int
ipx_create_ifc(IPXNet network, IPXNode node,
	       char *device, int type, void *data)
{
	struct ipx_interface *ifc = &(ifaces[first_free_ifc_index()]);
	int result;

	if (ifc == NULL)
	{
		LOG_START;
		fprintf(log_file,"too many interfaces (max. %i)\n",
			(int)MAX_IFACE);
		LOG_END;
		return -1;
	}

	if (network == 0)
	{
		LOG_START;
		fprintf(log_file,"interface bound to net 0\n");
		LOG_END;
		return -1;
	}

	if (   (passive != 0)
	    && ((type & IPX_KRT_INTERNAL) != 0))
	{
		LOG_START;
		fprintf(log_file, "passive mode only whithout internal net\n");
		LOG_END;
		return -1;
	}

	if ((result = ipx_init_interface(ifc, network, node, device, type,
					 data)) != 0)
	{
		return result;
	}
	
	return 0;
}

static void
init(void)
{
	struct itimerval itval;
	int i;
	
	LOG_ENTRY;
	LOG_START;
	fprintf(log_file,"Init start\n");
	LOG_END;

	read_ticks_table();
	
	for (i = 0; i < MAX_IFACE; i++)
	{
		ifaces[i].valid = 0;
	}

	if (ipx_kern_scan_ifaces(ipx_create_ifc, NULL) != 0)
	{
		exit(1);
	}

	itval.it_interval.tv_sec = TIMER_RATE;
	itval.it_value.tv_sec = TIMER_RATE;
	itval.it_interval.tv_usec = 0;
	itval.it_value.tv_usec = 0;
	if (setitimer(ITIMER_REAL, &itval, (struct itimerval *)NULL) < 0)
	{
		LOG_START;
		fprintf(log_file, "FATAL ERROR: can't set itimer: %s\n",
			strerror(errno));
		LOG_END;
		exit(1);
	}
	
	signal(SIGINT,int_handler);
	signal(SIGALRM,timer_handler);
	signal(SIGTERM,terminate_handler);
	signal(SIGHUP,hup_handler);

	ipx_rip_initial_broadcasts();
	ipx_sap_initial_broadcasts();
	
	LOG_ENTRY;
	LOG_START;
	fprintf(log_file,"Init end\n");
	LOG_END;
}

static int
ipx_visit_ifc(IPXNet network, IPXNode node,
	      char *device, int type, void *data)
{
	struct ipx_interface *ifc;
	int result;

	if (network == 0)
	{
		return 0;
	}

	for (ifc = first_interface();
	     ifc != NULL;
	     ifc = next_interface(ifc))
	{
		if (ifc_net(ifc) == network)
		{
			ifc->visited = 1;
			return 0;
		}
	}

	/* We have a new interface */

	ifc = &(ifaces[first_free_ifc_index()]);

	if (ifc == NULL)
	{
		LOG_START;
		fprintf(log_file,"too many interfaces (max. %i)\n",
			(int)MAX_IFACE);
		LOG_END;
		return -1;
	}

	if ((result = ipx_init_interface(ifc, network, node, device, type,
					 data)) != 0)
	{
		return result;
	}

	ipx_rip_initial_broadcast(ifc);
	ipx_sap_initial_broadcast(ifc);

	ifc->visited = 1;
	
	return 0;
}


static void
ipx_check_interfaces(void)
{
	struct ipx_interface *ifc;

	for (ifc = first_interface();
	     ifc != NULL;
	     ifc = next_interface(ifc))
	{
		ifc->visited = 0;
	}

	ipx_kern_scan_ifaces(ipx_visit_ifc, NULL);

	for (ifc = first_interface();
	     ifc != NULL;
	     ifc = next_interface(ifc))
	{
		if (ifc->visited == 0)
		{
			/* Interface is down now */
			ipx_sap_down_ifc(ifc);
			ipx_rip_down_ifc(ifc);
			ifc->valid = 0;
		}
	}
}


static void
run(void)
{
	struct sockaddr_ipx sipx;
	int addr_len=sizeof(sipx);
	int size;
	fd_set rset;
	int result;
	int max_sk;
	struct ipx_interface *ifc = NULL;
	int got_rip, got_sap;

	while(1)
	{
		FD_ZERO(&rset);
		max_sk = 0;
		got_rip = got_sap = 0;

		for (ifc = first_interface();
		     ifc != NULL;
		     ifc = next_interface(ifc))
		{
			if (ifc->r_output.sk > max_sk)
			{
				max_sk = ifc->r_output.sk;
			}
			FD_SET(ifc->r_output.sk, &rset);

			if (ifc->s_output.sk > max_sk)
			{
				max_sk = ifc->s_output.sk;
			}
			FD_SET(ifc->s_output.sk, &rset);
		}		

		LOG_ENTRY;

		result = select(max_sk+1, &rset, NULL, NULL, NULL);

		if (   (result < 0)
		    && (errno != EINTR))
		{
			LOG_START;
			fprintf(log_file, "select error: %s\n",
				strerror(errno));
			LOG_END;
			continue;
		}

		size = -1;

		if (result == 0)
		{
			LOG_START;
			fprintf(log_file, "select timeout expired\n");
			LOG_END;
			continue;
		}

		if (result > 0)
		{
			for (ifc = first_interface();
			     ifc != NULL;
			     ifc = next_interface(ifc))
			{
				if (FD_ISSET(ifc->r_output.sk, &rset))
				{
					size=recvfrom(ifc->r_output.sk,
						      &rip_in_buf,
						      sizeof(rip_in_buf), 0,
						      (struct sockaddr*)&sipx,
						      &addr_len);
					if (size > 0)
					{
						got_rip = 1;
					}
					/* only one packet at a time */
					break;
				}
				if (FD_ISSET(ifc->s_output.sk, &rset))
				{
					size=recvfrom(ifc->s_output.sk,
						      &sap_in_buf,
						      sizeof(sap_in_buf), 0,
						      (struct sockaddr*)&sipx,
						      &addr_len);
					if (size > 0)
					{
						got_sap = 1;
					}
					/* only one packet at a time */
					break;
				}
			}
		}

		if (size < 0)
		{
			/* One reason can be an interface that died. */
			check_request = 1;
		}
		if (got_rip != 0)
		{
			DL_START;
			fprintf(log_file,"RIP from: ");
			ipx_fprint_saddr(log_file,&sipx);
			fprintf(log_file,"\n");
			ipx_rip_fdump(log_file, &rip_in_buf, size);
			DL_END;
			handle_rip(&rip_in_buf, size, &sipx,ifc);
		}
		if (got_sap != 0)
		{
			DL_START;
			fprintf(log_file,"SAP from: ");
			ipx_fprint_saddr(log_file,&sipx);
			fprintf(log_file,"\n");
			ipx_sap_fdump(log_file, &sap_in_buf, size);
			DL_END;
			handle_sap(&sap_in_buf, size, &sipx,ifc);
		}	
		if (dump_request)
		{
			fdump_routes(log_file);
			fdump_servers(log_file);
			dump_ticks();
			dump_request=0;
		}
		if (timer_request)
		{
			static int rip_broadcast = 0;
			int do_broadcast = 0;

			time_since_last_bcast += TIMER_RATE;
			if (time_since_last_bcast >= BROADCAST_TIME/2)
			{
				do_broadcast = 1;
				rip_broadcast = (rip_broadcast==0) ? 1 : 0;
				time_since_last_bcast -= BROADCAST_TIME/2;
			}

			LOG_ENTRY;
			ipx_rip_do_aging(TIMER_RATE,
					    (do_broadcast  != 0)
					 && (rip_broadcast != 0));
			LOG_ENTRY;
			ipx_sap_do_aging(TIMER_RATE,
					    (do_broadcast!=0)
					 && (rip_broadcast == 0));

			timer_request=0;
		}
		if (check_request)
		{
			ipx_check_interfaces();
			check_request = 0;
		}
		if (scan_ticks_request)
		{
			read_ticks_table();
			scan_ticks_request = 0;
		}
		if (terminate)
		{
			LOG_START;
			if (terminate==1)
				fprintf(log_file,
					"exiting on signal 15 (SIGTERM)\n");
			else
				fprintf(log_file,
					"exiting on signal 1 (SIGHUP)\n");
			LOG_END;
			return;
		}
	}
}

static void
help()
{
	fprintf(stderr,
		"IPX routing daemon v0.91, (c) Ales Dryak, 1995\n"
		"Usage: ipxd [options]\n");
}

static int
parse_cmdline(int argc,char** argv)
{
	char* log_name = DEFAULT_LOGNAME;
	int opt;

	while ((opt = getopt(argc, argv, "dt:l:p")) != EOF)
	{
		switch (opt)
		{
		case 'd':
			debug_option=1;
			break;
		case 't':
			ticks_file = optarg;
			break;
		case 'l':
			log_name = optarg;
			break;
		case 'p':
			passive = 1;
			break;
		default:
			fprintf(stderr,
				"Unknown command line option %s\n",*argv);
			help();
			exit(1);
		}
	}
	if (optind != argc)
	{
		fprintf(stderr,"Too many parameters");
		help();
		exit(1);
	}
	if (debug_option)
	{
		log_file=stdout;
		return 0;
	}
	if ((log_file=fopen(log_name,"a"))==NULL)
	{
		perror(log_name);
		exit(1);
	}
	return 1;
}

static void
daemonize()
{
	int fd,c;
	
	if ((c = fork()) > 0) exit(0);
  	if (c < 0)
  	{
		fprintf(stderr, "ipxripd: can't fork: %s\n",strerror(errno));
		exit(1);
	}

	close(0);
	close(1);
	close(2);
	if ((fd = open("/dev/tty", O_RDWR)) >= 0) 
	{
		ioctl(fd, TIOCNOTTY, NULL);
		close(fd);
	}
}

int
main(int argc,char** argv)
{
	if (parse_cmdline(argc,argv))
	{
		daemonize();
	}
	init();
	run();
	ipx_rip_done();
	ipx_sap_done();
	return 0;
}

