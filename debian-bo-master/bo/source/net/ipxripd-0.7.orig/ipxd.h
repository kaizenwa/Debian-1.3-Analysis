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

#ifndef _IPXD_H_
#define _IPXD_H_

#include <stdio.h>
#include <sys/time.h>
#include "ipxutil.h"
#include "ipxkern.h"
#include "ipxsap.h"
#include "ipxrip.h"

#define TICKS_FILE ("/etc/ipx_ticks")
#define DEFAULT_TICKS (1)

#define TIMER_RATE	(30)
#define EXPIRE_TIME	(180)
#define BROADCAST_TIME	(60)

#define MAX_IFACE	(32)

struct ipx_interface
{
	IPXNode ifnode;
	int ticks;		/* How many ticks does it cost to send
				   over this interface? */
	int valid;
	int visited;
	
	struct sap_output s_output;
	struct rip_output r_output;
};

extern int new_log_entry;
extern int debug_option;
extern FILE *log_file;
extern int passive;

extern int check_request;

struct ipx_interface *first_interface(void);
struct ipx_interface *next_interface(struct ipx_interface *ifc);
int ifc_get_index(struct ipx_interface *ifc);
int first_ifc_index(void);
int next_ifc_index(int i);
int find_ticks(char *device);

static inline void
print_time()
{
	time_t t;
	time(&t);
	fprintf(log_file,"\n%s",ctime(&t));
}

#define LOG_ENTRY	{new_log_entry=1;}
#define LOG_START	{if (new_log_entry) {print_time(); new_log_entry=0;}
#define LOG_END		fflush(log_file);}
#define DL_ENTRY	{if (debug_option) LOG_ENTRY}
#define DL_START	{if (debug_option) LOG_START
#define DL_END		LOG_END }
#define DEFAULT_LOGNAME "/dev/null"

#endif /* _IPXD_H_ */
