/*
 *  ipxlib.h
 *
 *  Copyright (C) 1995 by Volker Lendecke
 *
 */

#ifndef _IPXLIB_H
#define _IPXLIB_H


#include <linux/types.h>
#include <linux/ncp.h>
#include <linux/ncp_fs.h>
#include <linux/ipx.h>
#include <stdio.h>

typedef unsigned long  IPXNet;
typedef unsigned short IPXPort;
typedef unsigned char  IPXNode[IPX_NODE_LEN];

#define IPX_USER_PTYPE (0x00)
#define IPX_RIP_PTYPE (0x01)
#define IPX_SAP_PTYPE (0x04)
#define IPX_AUTO_PORT (0x0000)
#define IPX_SAP_PORT  (0x0452)
#define IPX_RIP_PORT  (0x0453)

#define IPX_SAP_GENERAL_QUERY (0x0001)
#define IPX_SAP_GENERAL_RESPONSE (0x0002)
#define IPX_SAP_NEAREST_QUERY (0x0003)
#define IPX_SAP_NEAREST_RESPONSE (0x0004)

#define IPX_SAP_FILE_SERVER (0x0004)

struct sap_query {
	unsigned short query_type; /* net order */
	unsigned short server_type; /* net order */
};

struct sap_server_ident {
	unsigned short server_type          __attribute__ ((packed));
	char           server_name[48]      __attribute__ ((packed));
	IPXNet         server_network       __attribute__ ((packed));
	IPXNode        server_node          __attribute__ ((packed));
	IPXPort        server_port          __attribute__ ((packed));
	unsigned short intermediate_network __attribute__ ((packed));
};
       
#define IPX_RIP_REQUEST (0x1)
#define IPX_RIP_RESPONSE (0x2)

struct ipx_rip_packet {
	__u16 operation         __attribute__ ((packed));
	struct ipx_rt_def {
		__u32 network   __attribute__ ((packed));
		__u16 hops      __attribute__ ((packed));
		__u16 ticks     __attribute__ ((packed));
	} rt[1]                 __attribute__ ((packed));
};

#define IPX_BROADCAST_NODE ("\xff\xff\xff\xff\xff\xff")
#define IPX_THIS_NODE      ("\0\0\0\0\0\0")
#define IPX_THIS_NET (0)

#ifndef IPX_NODE_LEN
#define IPX_NODE_LEN (6)
#endif

void
ipx_print_node(IPXNode node);
void
ipx_print_network(IPXNet net);
void
ipx_print_port(IPXPort port);
void
ipx_print_saddr(struct sockaddr_ipx* sipx);
void
ipx_fprint_node(FILE *file, IPXNode node);
void
ipx_fprint_network(FILE *file, IPXNet net);
void
ipx_fprint_port(FILE *file, IPXPort port);
void
ipx_fprint_saddr(FILE *file, struct sockaddr_ipx* sipx);
int
ipx_sscanf_node(char *buf, unsigned char node[IPX_NODE_LEN]);
void
ipx_assign_node(IPXNode dest, IPXNode src);
int
ipx_node_equal(IPXNode n1,IPXNode n2);

#endif /* _IPXLIB_H */
