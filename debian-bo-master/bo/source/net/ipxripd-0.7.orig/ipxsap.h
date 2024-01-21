/*

    IPX support library

    Copyright (C) 1994, 1995  Ales Dryak <e-mail: A.Dryak@sh.cvut.cz>
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
#ifndef __IPXSAP_H__

#define __IPXSAP_H__

#include <stdio.h>
#include "ipxutil.h"

#define IPX_SAP_PORT		(0x452U)
#define IPX_SAP_PTYPE		(4U)
#define IPX_SAP_OP_REQUEST	(1U)
#define IPX_SAP_GENERAL_RQ	(0xFFFFU)
#define IPX_SAP_OP_RESPONSE	(2U)
#define IPX_SAP_OP_GNS_REQUEST	(3U)
#define IPX_SAP_OP_GNS_RESPONSE	(4U)
#define IPX_SAP_MAX_ENTRIES	(7U)
#define IPX_SAP_SERVER_DOWN	(16U)
#define IPX_SAP_SERVER_NAME_LEN	(48U)
#define IPX_SAP_REQUEST_LEN	(4U)

typedef unsigned short int ser_type_t;
typedef char ser_name_t[IPX_SAP_SERVER_NAME_LEN];

struct sap_entry
{
	ser_type_t ser_type __attribute__ ((packed));
	ser_name_t ser_name __attribute__ ((packed));
	IPXNet network      __attribute__ ((packed));
	IPXNode node        __attribute__ ((packed));
	IPXPort port        __attribute__ ((packed));
	hop_t hops          __attribute__ ((packed));
};

struct sap_packet
{
	unsigned short int operation      __attribute__ ((packed));
	struct sap_entry   sap_entries[IPX_SAP_MAX_ENTRIES]
		                          __attribute__ ((packed));
};

int ipx_sap_size(int n,unsigned short int operation);
void ipx_sap_dump(struct sap_packet *pkt,int len);
void ipx_sap_fdump(FILE *file,struct sap_packet *pkt,int len);
void ipx_sap_assign_ser_name(ser_name_t dest,ser_name_t src); 
void ipx_sap_fprint_name(FILE *file,ser_name_t sname);
int ipx_sap_name_equal(ser_name_t n1,ser_name_t n2);
int ipx_sap_type_equal(ser_type_t t1,ser_type_t t2);

struct sap_output
{
	struct sockaddr_ipx dest_addr;
	int sk; /* the socket to send/receive on */
	int send_error;
	int entries;
	struct sap_packet buffer;
};

int ipx_sap_output_init(struct sap_output *out,IPXNet iface);
void ipx_sap_output_flush(struct sap_output *out);
void ipx_sap_output_request(struct sap_output *out,ser_type_t ser_type);
void ipx_sap_output_gns_request(struct sap_output *out,ser_type_t ser_type);
void ipx_sap_output_response(struct sap_output *out,ser_type_t type,
			     ser_name_t name,struct sockaddr_ipx *addr,
			     hop_t hops,int down_allow);
void ipx_sap_output_gns_response(struct sap_output *out,ser_type_t type,
				 ser_name_t name,struct sockaddr_ipx *addr,
				 hop_t hops);
void ipx_sap_output_set_destination(struct sap_output *out,IPXNode node,
				    IPXPort port);

extern int (*ipx_sap_output_func)(int sock, void *buffer,int size,
				   struct sockaddr_ipx *daddr);

#endif
