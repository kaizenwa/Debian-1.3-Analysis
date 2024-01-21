/* ipxparse.c */ 

/* Copyright 1996 Volker Lendecke, Goettingen, Germany
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <linux/if_ether.h>
#include <strings.h>
#include <sys/ioctl.h>
#include <linux/if.h>
#include <signal.h>
#include <linux/ip.h>
#include <linux/tcp.h>
#include <netinet/protocols.h>
#include <stdio.h>
#include <time.h>
#include <signal.h>
#include <ctype.h>
#include "ipxutil.h"

#define DUMPALLSAPS	    /* #define if you want to dump all SAP's */

struct ipx_address
{
	unsigned long net;
	unsigned char  node[IPX_NODE_LEN]; 
	unsigned short sock;
};

struct ipx_packet
{
	unsigned short	ipx_checksum;
#define IPX_NO_CHECKSUM	0xFFFF
	unsigned short  ipx_pktsize;
	unsigned char   ipx_tctrl;
	unsigned char   ipx_type;
#define IPX_TYPE_UNKNOWN	0x00
#define IPX_TYPE_RIP		0x01	/* may also be 0 */
#define IPX_TYPE_SAP		0x04	/* may also be 0 */
#define IPX_TYPE_SPX		0x05	/* Not yet implemented */
#define IPX_TYPE_NCP		0x11	/* $lots for docs on this (SPIT) */
#define IPX_TYPE_PPROP		0x14	/* complicated flood fill brdcast [Not supported] */
	struct ipx_address ipx_dest __attribute__ ((packed));
	struct ipx_address ipx_source __attribute__ ((packed));
};

#define NCP_ALLOC_SLOT_REQUEST   (0x1111)
#define NCP_REQUEST              (0x2222)
#define NCP_DEALLOC_SLOT_REQUEST (0x5555)

struct ncp_request_header {
	__u16   type      __attribute__ ((packed));
	__u8    sequence  __attribute__ ((packed));
	__u8    conn_low  __attribute__ ((packed));
	__u8    task      __attribute__ ((packed));
	__u8    conn_high __attribute__ ((packed));
	__u8    function  __attribute__ ((packed));
	__u8    data[0]   __attribute__ ((packed));
};

#define NCP_REPLY                (0x3333)
#define NCP_POSITIVE_ACK         (0x9999)

struct ncp_reply_header {
	__u16   type             __attribute__ ((packed));
	__u8    sequence         __attribute__ ((packed));
	__u8    conn_low         __attribute__ ((packed));
	__u8    task             __attribute__ ((packed));
	__u8    conn_high        __attribute__ ((packed));
	__u8    completion_code  __attribute__ ((packed));
	__u8    connection_state __attribute__ ((packed));
	__u8    data[0]          __attribute__ ((packed));
};

void handle_ipx (unsigned char *buf, int length, char *frame, int no);
void handle_ncp (struct sockaddr_ipx *source,
		 struct sockaddr_ipx *target,
		 unsigned char *buf, int length, int no);

#define	SAP_MAX_SERVER_NAME_LENGTH	48	/* in network packets */
#define	SAP_MAX_SAPS_PER_PACKET		7
#define	SAP_SHUTDOWN	16 /* Magic "hops" value to stop SAP advertising */

/* SAP Query structure (returned in sap_packet as an array)
 * NBO == Network Byte Order)
 */
typedef struct saps {
	__u16	serverType		__attribute__ ((packed)); /* NBO */
	__u8   serverName[SAP_MAX_SERVER_NAME_LENGTH] __attribute__ ((packed));
	struct ipx_address serverAddress __attribute__ ((packed));
	__u16	serverHops		__attribute__ ((packed)); /* NBO */
} SAPS;

/* General Service/Nearest Server Response SAP packet */
union sap_packet {
	unsigned short	sapOperation;
	struct sap_query {
		__u16	sapOperation	__attribute__ ((packed));
		__u16	serverType	__attribute__ ((packed));
	} query;
	struct sap_response {
		__u16	sapOperation	__attribute__ ((packed));
		/* each SAP can has a max of SAP_MAX_SAPS_PER_PACKET packets */
		SAPS	sap[SAP_MAX_SAPS_PER_PACKET] __attribute__ ((packed));
	} response;
};

/* print out one SAP record */
static void
print_sap(FILE *file, SAPS *sapp)
{
	fprintf(file, "  Name:%s, serverType 0x%x, ",
		sapp->serverName,
		ntohs(sapp->serverType));
	ipx_fprint_network(file, ntohl(sapp->serverAddress.net));
	fprintf(file, ":");
	ipx_fprint_node(file, sapp->serverAddress.node);
	fprintf(file, ":");
	ipx_fprint_port(file, ntohs(sapp->serverAddress.sock));
	fprintf(file, " (Hops %d)\n", ntohs(sapp->serverHops));
}

void
handle_ipx (unsigned char *buf, int length, char *frame, int no)
{
	struct ipx_packet *h = (struct ipx_packet *)buf;
	struct sockaddr_ipx s_addr;
	struct sockaddr_ipx d_addr;
	union  sap_packet *sappacket;
	int	hbo_dsock;	/* Host Byte Order of Destination SOCKet */
	int	hbo_sapop;	/* Host Byte Order of SAP OPeration */

	memset(&s_addr, 0, sizeof(s_addr));
	memset(&d_addr, 0, sizeof(d_addr));

	memcpy(s_addr.sipx_node, h->ipx_source.node, sizeof(s_addr.sipx_node));
	s_addr.sipx_port = h->ipx_source.sock;
	s_addr.sipx_network  = h->ipx_source.net;

	memcpy(d_addr.sipx_node, h->ipx_dest.node, sizeof(d_addr.sipx_node));
	d_addr.sipx_port = h->ipx_dest.sock;
	d_addr.sipx_network  = h->ipx_dest.net;

	printf("%6.6d %s from ", no, frame);

	ipx_print_saddr(&s_addr);
	printf(" to ");
	ipx_print_saddr(&d_addr);
	printf("\n");

	if (   (ntohs(s_addr.sipx_port) == 0x451)
	    || (ntohs(d_addr.sipx_port) == 0x451))
	{
		handle_ncp(&s_addr, &d_addr, buf + sizeof(struct ipx_packet),
			   length - sizeof(struct ipx_packet), no);
	}
	else	/* next 3 handle IPX by type vs by socket (one or other) */
		/* Note: most things use either ipx_type OR socket, not both */
	if (h->ipx_type == 0x01)
		printf(" type 0x01 (RIP packet (router))\n");
	else
	if (h->ipx_type == 0x05)
		printf(" type 0x05 (SPX sequenced packet)\n");
	else
	if (h->ipx_type == 0x14)
		printf(" type 0x14 (propogated Client-NetBios)\n");
	else
	{
		hbo_dsock = ntohs(d_addr.sipx_port);
		if (hbo_dsock == 0x452)	/* SAP */
		{
			sappacket = (union sap_packet *)
				(buf + sizeof(struct ipx_packet));
			hbo_sapop = ntohs(sappacket->sapOperation);
			if ((hbo_sapop == 0x01) || (hbo_sapop == 0x03))
			{
				printf(" type 0x%x, SAP op:0x%x %s Query, "
				       "serverType 0x%x wanted\n",
				       h->ipx_type, hbo_sapop,
				       (hbo_sapop == 0x01)?"General Service" :
				       (hbo_sapop == 0x03)?"Nearest Server" :
				       "Error",
				       ntohs(sappacket->query.serverType));
			}
			else
			{
				int hops;

				hops = ntohs(sappacket->
					     response.sap[0].serverHops);
				printf(" type 0x%x, SAP op:0x%x %s %s\n",
				       h->ipx_type, hbo_sapop,
				       (hbo_sapop == 0x02)
				       ? "General Service Response" :
				       (hbo_sapop == 0x04)
				       ? "Nearest Server Response" :
				       "Unknown", 
				       (hops >= SAP_SHUTDOWN)
				       ? "[Shutdown]" : "");

				/* Service ending */
				if (hops >=	SAP_SHUTDOWN)
				{
					print_sap(stdout,
						  sappacket->response.sap);
				}
#ifdef DUMPALLSAPS
				/* If you want to dump all SAP's */
				else
				{	int	num_saps;
					SAPS	*sapp;

					num_saps = (length
						    - sizeof(struct ipx_packet)
						    - 2) / sizeof(SAPS);

					sapp = sappacket->response.sap;
					for(; num_saps > 0; sapp++, num_saps--)
						print_sap(stdout, sapp);
				}
#endif /* DUMPALLSAPS */
			}

		}
		else	/* Other IPX types */
		    printf(" type 0x%x, Socket 0x%x (%s)\n", h->ipx_type,
			   hbo_dsock,
		       (hbo_dsock == 0x451) ? "NCP" :
/*		       (hbo_dsock == 0x452) ? "SAP" :*/
		       (hbo_dsock == 0x453) ? "RIP" :
		       (hbo_dsock == 0x455) ? "Client-NetBios" :
		       (hbo_dsock == 0x456) ? "Diags" :
		       (hbo_dsock == 0x002) ? "Xecho" :
		       (hbo_dsock == 0x8063) ? "NVT2" : "Other");
	}
	
}

void handle_ncp (struct sockaddr_ipx *source,
		 struct sockaddr_ipx *target,
		 unsigned char *buf, int length, int no)
{
	struct ncp_request_header *rq = (struct ncp_request_header *)buf;
	struct ncp_reply_header *rs = (struct ncp_reply_header *)buf;
	unsigned char *data = NULL;
	int data_length = 0;
	int i;

	if (ntohs(rq->type) == NCP_REQUEST)
	{
		/* Request */
		printf("NCP request: conn: %-5d, seq: %-3d, task: %-3d, ",
		       rq->conn_low + 256 * rq->conn_high,
		       rq->sequence, rq->task);

		data = buf + sizeof(struct ncp_request_header);
		data_length = length - sizeof(struct ncp_request_header);

		switch(rq->function)
		{
		case 20:
			printf("fn: %-3d\n", rq->function);
			printf("Get File Server Date and Time\n");
			break;
		case 21:
			printf("fn: %-3d, subfn: %-3d\n",
			       rq->function, data[2]);
			switch(data[2])
			{
			case 0:
				printf("Send Broadcast Message\n");
				break;
			case 1:
				printf("Get Broadcast Message\n");
				break;
			}
			data += 3;
			data_length -= 3;
			break;
		case 22:
			printf("fn: %-3d, subfn: %-3d\n",
			       rq->function, data[2]);
			switch(data[2])
			{
			case 00:
				printf("Set Directory Handle\n");
				break;
			case 01:
				printf("Get Directory Path\n");
				break;
			case 18:
				printf("Allocate Permanent Dir Handle\n");
				break;
			case 20:
				printf("Deallocate Directory Handle\n");
				break;
			case 21:
				printf("Get Volume Info with handle\n");
				break;
			}
			data += 3;
			data_length -= 3;
			break;
		case 23:
			printf("fn: %-3d, subfn: %-3d\n", rq->function,
			       data[2]);
			switch(data[2])
			{
			case 17:
				printf("Get Fileserver Information\n");
				break;
			case 23:
				printf("Get Crypt Key\n");
				break;
			case 24:
				printf("Encrypted Login\n");
				break;
			case 28:
				printf("Get Connection Information\n");
				break;
			case 53:
				printf("Get Bindery Object ID\n");
				break;
			case 55:
				printf("Scan Bindery Object\n");
				break;
			case 61:
				printf("Read Property Value\n");
				break;
			case 62:
				printf("Write Property Value\n");
				break;
			case 70:
				printf("Get Bindery Access Level\n");
				break;
			}
			
			data += 3;
			data_length -= 3;
			break;
		case 24:
			printf("fn: %-3d\n", rq->function);
			printf("End of Job\n");
			break;
		case 34:
			printf("fn: %-3d, subfn: %-3d\n", rq->function,
			       data[2]);
			data += 3;
			data_length -= 3;
			break;
		case 62:
			printf("fn: %-3d\n", rq->function);
			printf("File Search Initialize\n");
			break;
		case 63:
			printf("fn: %-3d\n", rq->function);
			printf("File Search Continue\n");
			break;
		case 64:
			printf("fn: %-3d\n", rq->function);
			printf("Search for a file\n");
			break;
		case 66:
			printf("fn: %-3d\n", rq->function);
			printf("Close File\n");
			break;
		case 72:
			printf("fn: %-3d\n", rq->function);
			printf("Read from File\n");
			break;
		case 73:
			printf("fn: %-3d\n", rq->function);
			printf("Write to File\n");
			break;
		case 75:
			printf("fn: %-3d\n", rq->function);
			printf("Set File Time Date Stamp\n");
			break;
		case 87:
			printf("fn: %-3d, subfn: %-3d\n",
			       rq->function, data[0]);
			switch(data[0])
			{
			case 1:
			{
				unsigned char *p = &(data[0]);
				printf("Open Create File or Subdirectory\n");
				printf("Name Space: %d\n", p[1]);
				printf("Open Create Mode: %x\n", p[2]);
				printf("Search Attributes: %x\n",
				       *(__u16 *)&(p[3]));
				printf("Return Information Mask: %x\n",
				       (unsigned int)(*(__u32 *)&(p[5])));
				printf("Desired Access Rights: %x\n",
				       *(__u16 *)&(p[9]));
				break;
			}
			case 2:
				printf("Initialize Search\n");
				break;
			case 3:
				printf("Search for File or Subdirectory\n");
				break;
			case 6:
				printf("Obtain File Or Subdirectory "
				       "Information\n");
				break;
			case 8:
				printf("Delete a File Or Subdirectory\n");
				break;
			}
			data += 1;
			data_length -= 1;
			break;
		default:
			printf("fn: %-3d\n", rq->function);
		}
	}

	if (ntohs(rs->type) == NCP_REPLY)
	{
		printf("NCP respons: conn: %-5d, seq: %-3d, task: %-3d, ",
		       rs->conn_low + 256 * rs->conn_high,
		       rs->sequence, rs->task);
		printf("compl: %-3d, conn_st: %-3d\n",
		       rs->completion_code, rs->connection_state);

		data = buf + sizeof(struct ncp_reply_header);
		data_length = length - sizeof(struct ncp_reply_header);
	}

	if (data == NULL)
	{
		data = buf;
		data_length = length;
	}

	i = 0;
	while (i < data_length)
	{
		int j;
		for (j = i; j < i+16; j++)
		{
			if (j >= data_length)
			{
				printf("  ");
			}
			else
			{
				printf("%-2.2X", data[j]);
			}
		}
		printf(" ");
		for (j = i; j < i+16; j++)
		{
			if (j >= data_length)
			{
				break;
			}
			if (isprint(data[j]))
			{
				printf("%c", data[j]);
			}
			else
			{
				printf(".");
			}
		}
		printf("\n");
		i += 16;
	}
	printf("\n");
}


void
main (int argc, char *argv[])
{
	unsigned char buf[16384];
	unsigned char packet[8192];
	unsigned char *b;
	int len;
	int i = 1;

	while (fgets(buf, sizeof(buf), stdin) != NULL)
	{
		if (strlen(buf) == sizeof(buf)-1)
		{
			fprintf(stderr, "line too long\n");
			exit(1);
		}

		b = strchr(buf, ' ');
		if (b == NULL)
		{
			fprintf(stderr, "illegal line format\n");
			exit(1);
		}

		*b = '\0';
		b += 1;
		len = 0;

		while ((b[0] != '\0') && (b[1] != '\0'))
		{
			unsigned int value;
			if (sscanf(b, "%2x", &value) != 1)
			{
				fprintf(stderr, "illegal packet\n");
				exit(1);
			}
			packet[len] = value;
			b += 2;
			len += 1;
		}
		handle_ipx(packet, len, buf, i);
		i += 1;
	}
	
	exit (0);
}
