/*
 *  ncplib.c
 *
 *  Copyright (C) 1995, 1996 by Volker Lendecke
 *
 */

#include "ncplib.h"
#include "ncplib_err.h"

typedef __u8  byte;
typedef __u16 word;
typedef __u32 dword;

#include <sys/ioctl.h>
/* #include <sys/wait.h> */  /* generates a warning here */
extern pid_t wait(int *);
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <unistd.h>
#include <signal.h>
#include <fcntl.h>
#include <ctype.h>
#include <linux/route.h>
#include <sys/param.h>
#include <stdlib.h>
#include <mntent.h>
#include <pwd.h>
#include <sys/stat.h>
#include <stdarg.h>

static long
ncp_negotiate_buffersize(struct ncp_conn *conn,
			 int size, int *target);
static long
ncp_login_object(struct ncp_conn *conn,
		 const unsigned char *username,
		 int login_type,
		 const unsigned char *password);

static long
ncp_do_close(struct ncp_conn *conn);

void
str_upper(char *name)
{
	while (*name)  {
		*name = toupper(*name);
		name = name + 1;
	}
}

#if 0

static int debug_level = 5;
static FILE *logfile = stderr;

static void
dprintf(int level, char *p, ...)
{
	va_list ap;

	if (level > debug_level)
	{
		return;
	}

	va_start(ap, p);
	vfprintf(logfile, p, ap);
	va_end(ap);
	fprintf(logfile, "\n");
	fflush(logfile);
}

#endif

/* I know it's terrible to include a .c file here, but I want to keep
   the file nwcrypt.c intact and separate for copyright reasons */
#include "nwcrypt.c"

void
ipx_fprint_node(FILE* file,IPXNode node)
{
	fprintf(file,"%02X%02X%02X%02X%02X%02X",
		(unsigned char)node[0],
		(unsigned char)node[1],
		(unsigned char)node[2],
		(unsigned char)node[3],
		(unsigned char)node[4],
		(unsigned char)node[5]
		);
}

void
ipx_fprint_network(FILE* file,IPXNet net)
{
	fprintf(file,"%08lX",ntohl(net));
}

void
ipx_fprint_port(FILE* file,IPXPort port)
{
	fprintf(file,"%04X",ntohs(port));
}

void
ipx_fprint_saddr(FILE* file,struct sockaddr_ipx* sipx)
{
	ipx_fprint_network(file,sipx->sipx_network);
	fprintf(file,":");
	ipx_fprint_node(file,sipx->sipx_node);
	fprintf(file,":");
	ipx_fprint_port(file,sipx->sipx_port);
}

void
ipx_print_node(IPXNode node)
{
	ipx_fprint_node(stdout,node);
}

void
ipx_print_network(IPXNet net)
{
	ipx_fprint_network(stdout,net);
}

void
ipx_print_port(IPXPort port)
{
	ipx_fprint_port(stdout,port);
}

void
ipx_print_saddr(struct sockaddr_ipx* sipx)
{
	ipx_fprint_saddr(stdout,sipx);
}

int
ipx_sscanf_node(char *buf, unsigned char node[6])
{
	int i;
	int n[6];

	if ((i = sscanf(buf, "%2x%2x%2x%2x%2x%2x",
			&(n[0]), &(n[1]), &(n[2]),
			&(n[3]), &(n[4]), &(n[5]))) != 6)
	{
		return i;
	}

	for (i=0; i<6; i++)
	{
		node[i] = n[i];
	}
	return 6;
}

void
ipx_assign_node(IPXNode dest, IPXNode src)
{
	memcpy(dest, src, IPX_NODE_LEN);
}

int
ipx_node_equal(IPXNode n1,IPXNode n2)
{
	return memcmp(n1,n2, IPX_NODE_LEN)==0;
}

static int
ipx_recvfrom(int sock, void *buf, int len, unsigned int flags,
	     struct sockaddr_ipx *sender, int *addrlen, int timeout,
	     long *err)
{
	fd_set rd, wr, ex;
	struct timeval tv;
	int result;

	FD_ZERO(&rd); FD_ZERO(&wr); FD_ZERO(&ex);
	FD_SET(sock, &rd);

	tv.tv_sec  = timeout;
	tv.tv_usec = 0;
		
	if ((result = select(sock+1, &rd, &wr, &ex, &tv)) == -1)
	{
		*err = errno;
		return -1;
	}

	if (FD_ISSET(sock, &rd))
	{
		result = recvfrom(sock, buf, len, flags,
				  (struct sockaddr *)sender, addrlen);
	}
	else
	{
		result = -1;
		errno = ETIMEDOUT;
	}
	if (result < 0)
	{
		*err = errno;
	}
	return result;
}

static int
ipx_recv(int sock, void *buf, int len, unsigned int flags, int timeout,
	 long *err)
{
	struct sockaddr_ipx sender;
	int addrlen = sizeof(sender);

	return ipx_recvfrom(sock, buf, len, flags, &sender, &addrlen,
			    timeout, err);
}

static long
ipx_sap_find_nearest(int server_type, struct sockaddr_ipx *result,
		     char server_name[NCP_BINDERY_NAME_LEN])
{
	struct sockaddr_ipx addr;
	char data[1024];
	int sock;
	int opt;
	int packets;
	int len;

	struct sap_server_ident *ident;

	if ((sock = socket(AF_IPX,SOCK_DGRAM,PF_IPX)) < 0)
	{
		return errno;
	}
	
	opt=1;
	/* Permit broadcast output */
	if(setsockopt(sock,SOL_SOCKET,SO_BROADCAST, &opt,sizeof(opt))==-1)
	{
		goto finished;
	}
	
	memzero(addr);
	addr.sipx_family  = AF_IPX;
	addr.sipx_network = htonl(0x0);
	addr.sipx_port    = htons(0x0);
	addr.sipx_type    = IPX_SAP_PTYPE;
	
	if(bind(sock,(struct sockaddr*)&addr,sizeof(addr))==-1)
	{
		if (errno == EADDRNOTAVAIL)
		{
			errno = NCPL_ET_NO_INTERFACE;
		}
		goto finished;
	}
	
	*(unsigned short *)data       = htons(IPX_SAP_NEAREST_QUERY);
	*(unsigned short *)&(data[2]) = htons(server_type);
	
	memzero(addr);
	addr.sipx_family  = AF_IPX;
	addr.sipx_port    = htons(IPX_SAP_PORT);
	addr.sipx_type    = IPX_SAP_PTYPE;
	addr.sipx_network = htonl(0x0);
	ipx_assign_node(addr.sipx_node, IPX_BROADCAST_NODE);
	
	if (sendto(sock, data, 4, 0,
		   (struct sockaddr *)&addr, sizeof(addr)) < 0)
	{
		goto finished;
	}

	packets = 5;
	do
	{
		long err;
		len = ipx_recv(sock, data, 1024, 0, 1, &err);
		if (len < 66)
		{
			packets = packets - 1;
			continue;
		}
	}
	while (   (ntohs(*((__u16 *)data)) != IPX_SAP_NEAREST_RESPONSE)
	       && (packets > 0));

	if (packets == 0)
	{
		close(sock);
		return NCPL_ET_NO_SERVER;
	}

	ident = (struct sap_server_ident *)(data+2);

	result->sipx_family  = AF_IPX;
	result->sipx_network = ident->server_network;
	result->sipx_port    = ident->server_port;
	ipx_assign_node(result->sipx_node, ident->server_node);

	memcpy(server_name, ident->server_name, sizeof(ident->server_name));
	
	errno = 0;
	
 finished:
	close(sock);
	return errno;
}

static int
ipx_make_reachable(IPXNet network)
{
	struct rtentry rt_def;
	/* Router */
	struct sockaddr_ipx *sr = (struct sockaddr_ipx *)&rt_def.rt_gateway;
	/* Target */
	struct sockaddr_ipx *st = (struct sockaddr_ipx *)&rt_def.rt_dst;

	struct ipx_rip_packet rip;
	struct sockaddr_ipx addr;
	int addrlen;
	int sock;
	int opt;
	int res=-1;
	int i;
	int packets;

	if (geteuid() != 0)
	{
		errno = EPERM;
		return -1;
	}

	memzero(rip);

	sock = socket(AF_IPX, SOCK_DGRAM, PF_IPX);

	if (sock == -1)
	{
		return -1;
	}

	opt=1;
	/* Permit broadcast output */
	if (setsockopt(sock, SOL_SOCKET, SO_BROADCAST, &opt, sizeof(opt)) != 0)
	{
		goto finished;
	}
	
	memzero(addr);
	addr.sipx_family  = AF_IPX;
	addr.sipx_network = htonl(0x0);
	addr.sipx_port    = htons(0x0);
	addr.sipx_type    = IPX_RIP_PTYPE;
	
	if (bind(sock, (struct sockaddr*)&addr, sizeof(addr)) != 0)
	{
		goto finished;
	}
	
	addr.sipx_family  = AF_IPX;
	addr.sipx_port    = htons(IPX_RIP_PORT);
	addr.sipx_type    = IPX_RIP_PTYPE;
	addr.sipx_network = htonl(0x0);
	ipx_assign_node(addr.sipx_node, IPX_BROADCAST_NODE);

	rip.operation = htons(IPX_RIP_REQUEST);
	rip.rt[0].network = htonl(network);

	if (sendto(sock, &rip, sizeof(rip), 0,
		   (struct sockaddr *)&addr, sizeof(addr)) < 0)
	{
		goto finished;
	}

	packets = 3;
	do
	{
		long err;
		int len;

		if (packets == 0)
		{
			goto finished;
		}

		addrlen = sizeof(struct sockaddr_ipx);

		len = ipx_recvfrom(sock, &rip, sizeof(rip),0, sr, &addrlen, 1,
				   &err);

		if (len < sizeof(rip))
		{
			packets = packets - 1;
			continue;
		}
	}
	while (ntohs(rip.operation) != IPX_RIP_RESPONSE);

	if (rip.rt[0].network != htonl(network))
	{
		goto finished;
	}

	rt_def.rt_flags  = RTF_GATEWAY;
	st->sipx_network = htonl(network);
	st->sipx_family  = AF_IPX;
	sr->sipx_family  = AF_IPX;

	i = 0;
	do
	{
		res = ioctl(sock, SIOCADDRT, &rt_def);
		i++;
	}
	while ((i < 5) && (res < 0) && (errno == EAGAIN));
	
 finished:
	close(sock);

	if (res != 0)
	{
		errno = ENETUNREACH;
	}
	return res;
}
	
static int
install_wdog(struct ncp_conn *conn)
{
	int parent_pid = getpid();
	int pid;
	int sock = conn->wdog_sock;

	char buf[1024];
	struct sockaddr_ipx sender;
	int sizeofaddr = sizeof(struct sockaddr_ipx);
	int pktsize;


	if ((pid = fork()) < 0)
	{
		return -1;
	}

	if (pid != 0)
	{
		/* Parent, should go on as usual */
		conn->wdog_pid = pid;
		return 0;
	}

	while (1)
	{
		long err;
		/* every 120 seconds we look if our parent is
		   still alive */
		pktsize = ipx_recvfrom(sock, buf, sizeof(buf), 0,
				       &sender, &sizeofaddr, 120, &err);

		if (getppid() != parent_pid)
		{
			/* our parent has died, so nothing to do
			   anymore */
			exit(0);
		}

		if (   (pktsize != 2)
		    || (buf[1] != '?'))
		{
			continue;
		}

		buf[1] = 'Y';
		pktsize = sendto(sock, buf, 2, 0,
				 (struct sockaddr *)&sender,
				 sizeof(sender));
	}
}

#define ncp_printf printf

static void
assert_conn_locked(struct ncp_conn *conn);

static void
assert_conn_not_locked(struct ncp_conn *conn)
{
	if (conn->lock != 0)
	{
		ncp_printf("ncpfs: conn already locked!\n");
	}
}

static void
ncp_lock_conn(struct ncp_conn *conn)
{
	assert_conn_not_locked(conn);
	conn->lock = 1;
}

static void
ncp_unlock_conn(struct ncp_conn *conn)
{
	assert_conn_locked(conn);
	conn->lock = 0;
}

static long
do_ncp_call(struct ncp_conn *conn, int request_size)
{
	struct ncp_request_header request =
		*((struct ncp_request_header *)(&(conn->packet)));

	int result;
	int retries = 3;
	int len;
	long err;
	struct ncp_reply_header *r =
		(struct ncp_reply_header *)&(conn->packet);

	while (retries > 0)
	{
		retries -= 1;

		result = sendto(conn->ncp_sock, conn->packet,
				request_size,
				0, (struct sockaddr *)&(conn->i.addr),
				sizeof(conn->i.addr));

		if (result < 0)
		{
			return errno;
		}

	re_select:
		len = ipx_recv(conn->ncp_sock,
			       conn->packet, NCP_PACKET_SIZE, 0, 1, &err);

		if (   (len == sizeof(*r))
		    && (r->type == NCP_POSITIVE_ACK))
		{
			goto re_select;
		}
		if (   (len          >= sizeof(*r))
		    && (r->type      == NCP_REPLY)
		    && (   (request.type    == NCP_ALLOC_SLOT_REQUEST)
			|| (   (r->sequence  == request.sequence)
			    && (r->conn_low  == request.conn_low)
			    && (r->conn_high == request.conn_high))))
		{
			conn->reply_size = len;
			break;
		}
		if (len < 0)
		{
			return err;
		}
	}
	return 0;
}

static int
ncp_mount_request(struct ncp_conn *conn, int function)
{
	struct ncp_request_header *h
		= (struct ncp_request_header *)(conn->packet);
	struct ncp_reply_header *reply 
		= (struct ncp_reply_header *)(conn->packet);
	struct ncp_ioctl_request request;

	int result;

	assert_conn_locked(conn);

	if (conn->has_subfunction != 0)
	{
		*(__u16 *)&(h->data[0]) = conn->current_size
			- sizeof(struct ncp_request_header)- 2;
	}

	request.function   = function;
	request.size       = conn->current_size;
	request.data       = conn->packet;

	if ((result = ioctl(conn->mount_fid,NCP_IOC_NCPREQUEST, &request)) < 0)
	{
		return result;
	}

	conn->completion  = reply->completion_code;
	conn->conn_status = reply->connection_state;
	conn->ncp_reply_size = result - sizeof(struct ncp_reply_header);

	if ((reply->completion_code != 0) && (conn->verbose != 0))
	{
		ncp_printf("ncp_request_error: %d\n", reply->completion_code);
	}
	return reply->completion_code == 0 ? 0 : NCPL_ET_REQUEST_ERROR;
}

static long
ncp_temp_request(struct ncp_conn *conn, int function)
{
	struct ncp_request_header *h
		= (struct ncp_request_header *)&(conn->packet);
	struct ncp_reply_header *r
		= (struct ncp_reply_header *)&(conn->packet);

	long err;

	assert_conn_locked(conn);

	if (conn->has_subfunction != 0)
	{
		*(__u16 *)&(h->data[0]) = conn->current_size
			- sizeof(struct ncp_request_header) - 2;
	}

	h->type = NCP_REQUEST;

	conn->sequence += 1;
	h->sequence = conn->sequence;
	h->conn_low  = (conn->i.connection) & 0xff; 
	h->conn_high = ((conn->i.connection) & 0xff00) >> 8;
	h->task      = 1;
	h->function  = function;

	if ((err = do_ncp_call(conn, conn->current_size)) != 0)
	{
		return err;
	}
	
	conn->completion  = r->completion_code;
	conn->conn_status = r->connection_state;
	conn->ncp_reply_size =
		conn->reply_size - sizeof(struct ncp_reply_header);

	if ((r->completion_code != 0) && (conn->verbose != 0))
	{
		ncp_printf("ncp_completion_code: %d\n", r->completion_code);
	}
	return r->completion_code == 0 ? 0 : NCPL_ET_REQUEST_ERROR;
}

static long
ncp_connect_addr(struct ncp_conn *conn, const struct sockaddr_ipx *target,
		 int wdog_needed)
{
	struct ncp_request_header *h =
		(struct ncp_request_header *)&(conn->packet);

	struct sockaddr_ipx addr;
	int addrlen;

	int ncp_sock, wdog_sock;
	long err;

	conn->is_connected = NOT_CONNECTED;
	conn->verbose = 0;
	
	if ((ncp_sock = socket(AF_IPX, SOCK_DGRAM, PF_IPX)) == -1)
	{
		return errno;
	}

	if ((wdog_sock = socket(AF_IPX, SOCK_DGRAM, PF_IPX)) == -1)
	{
		return errno;
	}

	addr.sipx_family  = AF_IPX;
	addr.sipx_port    = htons(0x0);
	addr.sipx_type    = NCP_PTYPE;
	addr.sipx_network = IPX_THIS_NET;
	ipx_assign_node(addr.sipx_node, IPX_THIS_NODE);

	addrlen = sizeof(addr);

	if (   (bind(ncp_sock, (struct sockaddr *)&addr, sizeof(addr)) == -1)
	    || (getsockname(ncp_sock, (struct sockaddr *)&addr, &addrlen)==-1))
	{
		int saved_errno = errno;
		close(ncp_sock); close(wdog_sock);
		return saved_errno;
	}

	addr.sipx_port = htons(ntohs(addr.sipx_port) + 1);

	if (bind(wdog_sock, (struct sockaddr *)&addr, sizeof(addr)) == -1)
	{
		int saved_errno = errno;
		close(ncp_sock); close(wdog_sock);
		return saved_errno;
	}

	conn->ncp_sock = ncp_sock;
	conn->wdog_sock = wdog_sock;

	h->type = NCP_ALLOC_SLOT_REQUEST;

	conn->sequence = 0;
	conn->i.addr   = *target;
	h->sequence    = conn->sequence;
	h->conn_low    = 0xff;
	h->conn_high   = 0xff;
	h->task        = 1;
	h->function    = 0;

	if ((err = do_ncp_call(conn, sizeof(*h))) != 0)
	{
		if (   (err != ENETUNREACH)
		    || (ipx_make_reachable(htonl(target->sipx_network)) != 0)
		    || ((err = do_ncp_call(conn, sizeof(*h))) != 0))
		{
			close(ncp_sock); close(wdog_sock);
			return err;
		}
	}

	if (wdog_needed != 0)
	{
		install_wdog(conn);
	}
	else
	{
		conn->wdog_pid = 0;
	}

	conn->sequence     = 0;
	conn->i.connection = h->conn_low + (h->conn_high * 256);
	conn->is_connected = CONN_TEMPORARY;

	if (   (ncp_negotiate_buffersize(conn, 1024,
					 &(conn->i.buffer_size)) != 0)
	    || (conn->i.buffer_size < 512)
	    || (conn->i.buffer_size > 1024))
	{
		ncp_do_close(conn);
		return -1;
	}

	return 0;
}

static long
ncp_connect_any(struct ncp_conn *conn, int wdog_needed)
{
	struct sockaddr_ipx addr;
	char name[NCP_BINDERY_NAME_LEN];
	long result;

	if ((result = ipx_sap_find_nearest(IPX_SAP_FILE_SERVER,
					   &addr, name)) != 0)
	{
		return result;
	}

	if ((result = ncp_connect_addr(conn, &addr, wdog_needed)) != 0)
	{
		return result;
	}
	strcpy(conn->server, name);
	return 0;
}

struct sockaddr_ipx *
ncp_find_fileserver(const char *server_name, long *err)
{
	char server[NCP_BINDERY_NAME_LEN];
	char nearest[NCP_BINDERY_NAME_LEN];
	struct nw_property prop;
	struct prop_net_address *n_addr = (struct prop_net_address *)&prop;
	struct ncp_conn conn;

	static struct sockaddr_ipx result;

	initialize_NCPL_error_table();
	if (strlen(server_name) >= sizeof(server))
	{
		*err = NCPL_ET_NAMETOOLONG;
		return NULL;
	}

	strcpy(server, server_name);
	str_upper(server);

	if ((*err = ipx_sap_find_nearest(IPX_SAP_FILE_SERVER,
					 &result, nearest)) != 0)
	{
		return NULL;
	}

	/* We have to ask the nearest server for our wanted server */

	memzero(conn);
	if ((*err = ncp_connect_addr(&conn, &result, 0)) != 0)
	{
		return NULL;
	}

	/* The following optimization should have been done before
           ncp_connect_addr. This would be convenient if there was a
           simple way to find out whether there is a route to the
           server. Parsing /proc/net/ipx_route is not too nice, so we
           just connect to the server and immediately disconnect
           again. This way we also find out if the server still has
           free connection slots. */

	if (strcmp(server, nearest) == 0)
	{
		/* Our wanted server answered the SAP GNS request, so
                   use it */
		ncp_do_close(&conn);
		errno = 0;
		return &result;
	}

	if (ncp_read_property_value(&conn, NCP_BINDERY_FSERVER, server, 1,
				    "NET_ADDRESS", &prop) != 0)
	{
		ncp_do_close(&conn);
		*err = NCPL_ET_HOST_UNKNOWN;
		return NULL;
	}
	
	if ((*err = ncp_do_close(&conn)) != 0)
	{
		return NULL;
	}

	result.sipx_family  = AF_IPX;
	result.sipx_network = n_addr->network;
	result.sipx_port    = n_addr->port;
	ipx_assign_node(result.sipx_node, n_addr->node);

	/* To make the final server reachable, we connect again. See
           above. (When can we rely on all users running ipxd??? :-)) */
	memzero(conn);
	if (   ((*err = ncp_connect_addr(&conn, &result, 0)) != 0)
	    || ((*err = ncp_do_close(&conn)) != 0))
	{
		return NULL;
	}
	
	return &result;
}

static long
ncp_open_temporary(struct ncp_conn *conn,
		   const struct ncp_conn_spec *spec)
{
	struct sockaddr_ipx *addr;
	long err;

	if (spec == NULL)
	{
		return ncp_connect_any(conn, 1);
	}

	if ((addr = ncp_find_fileserver(spec->server, &err)) == NULL)
	{
		return err;
	}

	if ((err = ncp_connect_addr(conn, addr, 1)) != 0)
	{
		return err;
	}

	strcpy(conn->server, spec->server);

	if (strlen(spec->user) != 0)
	{
		if (ncp_login_object(conn, spec->user, spec->login_type,
				     spec->password) != 0)
		{
			ncp_do_close(conn);
			return EACCES;
		}
		strcpy(conn->user, spec->user);
	}

	return 0;
}

char *
ncp_find_permanent(const struct ncp_conn_spec *spec)
{
	FILE *mtab;
	struct ncp_conn_ent *conn_ent;
	char *result = NULL;
	int mount_fid;
	struct ncp_fs_info i;

	initialize_NCPL_error_table();	

	if ((mtab = fopen(MOUNTED, "r")) == NULL)
	{
		return NULL;
	}

	while ((conn_ent = ncp_get_conn_ent(mtab)) != NULL)
	{
		if (spec != NULL)
		{
			if (   (conn_ent->uid != spec->uid)
			    || (   (strlen(spec->server) != 0)
				&& (strcasecmp(conn_ent->server,
					       spec->server) != 0))
			    || (   (strlen(spec->user) != 0)
				&& (strcasecmp(conn_ent->user,
					       spec->user) != 0)))
			{
				continue;
			}
		}

		mount_fid = open(conn_ent->mount_point, O_RDONLY, 0);
		if (mount_fid < 0)
		{
			continue;
		}

		i.version = NCP_GET_FS_INFO_VERSION;

		if (ioctl(mount_fid, NCP_IOC_GET_FS_INFO, &i) < 0)
		{
			close(mount_fid);
			continue;
		}

		close(mount_fid);
		result = conn_ent->mount_point;
		break;
	}

	fclose(mtab);
	errno = (result == NULL) ? ENOENT : 0;
	return result;
}
	
static int
ncp_open_permanent(struct ncp_conn *conn,
		   const struct ncp_conn_spec *spec)
{
	char *mount_point;

	if (conn->is_connected != NOT_CONNECTED)
	{
		errno = EBUSY;
		return -1;
	}

	if ((mount_point = ncp_find_permanent(spec)) == NULL)
	{
		return -1;
	}

	if (strlen(mount_point) >= sizeof(conn->mount_point))
	{
		errno = ENAMETOOLONG;
		return -1;
	}

	/* The rest has already been done in ncp_find_permanent, so we
	 * do not check errors anymore */
	conn->mount_fid = open(mount_point, O_RDONLY, 0);
	conn->i.version = NCP_GET_FS_INFO_VERSION;
	ioctl(conn->mount_fid, NCP_IOC_GET_FS_INFO, &(conn->i));
	if (spec != NULL)
	{
		strncpy(conn->server, spec->server, sizeof(conn->server));
		strncpy(conn->user, spec->user, sizeof(conn->user));
	}
	else
	{
 		memset(conn->server, '\0', sizeof(conn->server));
 		memset(conn->user, '\0', sizeof(conn->user));
	}		
	strcpy(conn->mount_point, mount_point);
	conn->is_connected = CONN_PERMANENT;
	return 0;
}

struct ncp_conn *
ncp_open(const struct ncp_conn_spec *spec, long *err)
{
	struct ncp_conn *result;

	initialize_NCPL_error_table();	

	result = malloc(sizeof(struct ncp_conn));

	if (result == NULL)
	{
		*err = ENOMEM;
		return NULL;
	}

	memzero(*result);

	if (ncp_open_permanent(result, spec) == 0)
	{
		return result;
	}

	if ((*err = ncp_open_temporary(result, spec)) != 0)
	{
		free(result);
		return NULL;
	}
	return result;
}


struct ncp_conn *
ncp_open_mount(const char *mount_point, long *err)
{
	struct ncp_conn *result;

	initialize_NCPL_error_table();	

	if (strlen(mount_point) >= sizeof(result->mount_point))
	{
		*err = ENAMETOOLONG;
		return NULL;
	}

	result = malloc(sizeof(struct ncp_conn));

	if (result == NULL)
	{
		*err = ENOMEM;
		return NULL;
	}

	memzero(*result);

	result->is_connected = NOT_CONNECTED;

	result->mount_fid = open(mount_point, O_RDONLY, 0);
	if (result->mount_fid < 0)
	{
		free(result);
		*err = ENODEV;
		return NULL;
	}
	strcpy(result->mount_point, mount_point);
	result->is_connected = CONN_PERMANENT;
	return result;
}

static long
ncp_user_disconnect(struct ncp_conn *conn)
{
	struct ncp_request_header *h
		= (struct ncp_request_header *)(conn->packet);
	long result;

	h->type = NCP_DEALLOC_SLOT_REQUEST;
	
	conn->sequence += 1;
	h->sequence  = conn->sequence;
	h->conn_low  = (conn->i.connection) & 0xff;
	h->conn_high = ((conn->i.connection) & 0xff00) >> 8;
	h->task      = 1;
	h->function  = 0;

	if ((result = do_ncp_call(conn, sizeof(*h))) != 0)
	{
		return result;
	}

	close(conn->ncp_sock);
	close(conn->wdog_sock);

	if (conn->wdog_pid != 0)
	{
		kill(conn->wdog_pid, SIGTERM);
		wait(NULL);
	}

	return 0;
}

static long
ncp_do_close(struct ncp_conn *conn)
{
	long result = -1;

	switch (conn->is_connected)
	{
	case CONN_PERMANENT:
		result = close(conn->mount_fid);
		break;
	
	case CONN_TEMPORARY:
		result = ncp_user_disconnect(conn);
		break;

	default:
		break;
	}
	
	conn->is_connected = NOT_CONNECTED;

	return result;
}

long
ncp_close(struct ncp_conn *conn)
{
	long result;
	if ((result = ncp_do_close(conn)) != 0)
	{
		return result;
	}
	free(conn);
	return 0;
}

struct ncp_conn_ent *
ncp_get_conn_ent(FILE *filep)
{
	static struct ncp_conn_ent entry;
	char server[2*NCP_BINDERY_NAME_LEN];
	char *user;
	struct mntent *mnt_ent;
	int fid;

	memzero(server);
	memzero(entry);

	while ((mnt_ent = getmntent(filep)) != NULL)
	{
		if (strcmp(mnt_ent->mnt_type, "ncpfs") != 0)
		{
			continue;
		}

		if (strlen(mnt_ent->mnt_fsname) >= sizeof(server))
		{
			continue;
		}

		strcpy(server, mnt_ent->mnt_fsname);
		user = strchr(server, '/');
		if (user != NULL)
		{
			*user = '\0';
			user += 1;
			if (strlen(user) >= sizeof(entry.user))
			{
				continue;
			}
			strcpy(entry.user, user);
		}

		if (   (strlen(server) >= sizeof(entry.server))
		    || (strlen(mnt_ent->mnt_dir) >= sizeof(entry.mount_point)))
		{
			continue;
		}
		strcpy(entry.server, server);
		strcpy(entry.mount_point, mnt_ent->mnt_dir);

		fid = open(entry.mount_point, O_RDONLY, 0);

		if (fid == -1)
		{
			continue;
		}
        
		if (ioctl(fid, NCP_IOC_GETMOUNTUID, &entry.uid) != 0) {
			close(fid);
			continue;
		}

		close(fid);
		return &entry;
	}

	return NULL;
}

static struct ncp_conn_spec *
ncp_get_nwc_ent(FILE *nwc)
{
	static struct ncp_conn_spec spec;
	char line[512];
	int line_len;
	char *user;
	char *password;

	memzero(spec);
	spec.uid = getuid();

	while (fgets(line, sizeof(line), nwc) != NULL)
	{
		if (   (line[0] == '\n')
		    || (line[0] == '#'))
		{
			continue;
		}

		line_len = strlen(line);
		if (line[line_len-1] == '\n')
		{
			line[line_len-1] = '\0';
		}

		user = strchr(line, '/');
		password = strchr(user != NULL ? user : line, ' ');

		if (password != NULL)
		{
			*password = '\0';
			password += 1;
		}

		if (user != NULL)
		{
			*user = '\0';
			user += 1;
			if (strlen(user) >= sizeof(spec.user))
			{
				continue;
			}
			strcpy(spec.user, user);
		}

		if (strlen(line) >= sizeof(spec.server))
		{
			continue;
		}
		strcpy(spec.server, line);

		if (password != NULL)
		{
			while (*password == ' ')
			{
				password += 1;
			}

			if (strlen(password) >= sizeof(spec.password))
			{
				continue;
			}
			strcpy(spec.password, password);
		}
		return &spec;
	}
	return NULL;
}

FILE *
ncp_fopen_nwc(const char *user, const char *mode)
{
	char path[MAXPATHLEN];
	char *home = NULL;
	struct stat st;

	if (mode == NULL)
	{
		mode = "r";
	}

	if (user == NULL)
	{
		home = getenv("HOME");
	}
	else
	{
		struct passwd *pwd;

		if ((pwd = getpwnam(user)) != NULL)
		{
			home = pwd->pw_dir;
		}
	}

	if (   (home == NULL)
	    || (strlen(home) + sizeof(NWCLIENT) + 2  > sizeof(path)))
	{
		errno = ENAMETOOLONG;
		return NULL;
	}

	strcpy(path, home);
	strcat(path, "/");
	strcat(path, NWCLIENT);

	if (stat(path, &st) != 0)
	{
		return NULL;
	}

	if ((st.st_mode & (S_IRWXO | S_IRWXG)) != 0)
	{
		errno = EINVAL;
		return NULL;
	}
	
	return fopen(path, mode);
}


struct ncp_conn_spec *
ncp_find_conn_spec(const char *server, const char *user, const char *password,
		   int login_necessary, uid_t uid, long *err)
{
	static struct ncp_conn_spec spec;

	struct ncp_conn conn;

	FILE *nwc;
	struct ncp_conn_spec *nwc_ent;

	initialize_NCPL_error_table();	

	*err = 0;
	memzero(spec);
	spec.uid = getuid();

	if (server != NULL)
	{
		if (strlen(server) >= sizeof(spec.server))
		{
			*err = NCPL_ET_NAMETOOLONG;
			return NULL;
		}
		strcpy(spec.server, server);
	}
	else
	{
		nwc = ncp_fopen_nwc(NULL, NULL);

		if (nwc == NULL)
		{
			*err = NCPL_ET_NO_SPEC;
			return NULL;
		}

		nwc_ent = ncp_get_nwc_ent(nwc);
		fclose(nwc);

		if (nwc_ent == NULL)
		{
			*err = NCPL_ET_NO_SPEC;
			return NULL;
		}
		strcpy(spec.server, nwc_ent->server);
		strcpy(spec.user, nwc_ent->user);
	}

	if (user != NULL)
	{
		if (strlen(user) >= sizeof(spec.user))
		{
			*err = NCPL_ET_NAMETOOLONG;
			return NULL;
		}
		strcpy(spec.user, user);
	}

	str_upper(spec.server);
	str_upper(spec.user);
	spec.login_type = NCP_BINDERY_USER;

	if (ncp_open_permanent(&conn, &spec) == 0)
	{
		ncp_do_close(&conn);
		return &spec;
	}

	if (password != NULL)
	{
		if (strlen(password) >= sizeof(spec.password))
		{
			*err = NCPL_ET_NAMETOOLONG;
			return NULL;
		}
		strcpy(spec.password, password);
	}
	else
	{
		if ((nwc = ncp_fopen_nwc(NULL, NULL)) != NULL)
		{
			while ((nwc_ent = ncp_get_nwc_ent(nwc)) != NULL)
			{
				if (   (strcasecmp(spec.server,
						   nwc_ent->server) != 0)
				    || (   (*spec.user != '\0')
					&& (strcasecmp(spec.user,
						       nwc_ent->user) != 0)))
				{
					continue;
				}
				strcpy(spec.user, nwc_ent->user);
				strcpy(spec.password, nwc_ent->password);
				break;
			}
			fclose(nwc);
		}
	}

	if (login_necessary == 0)
	{
		memset(spec.user, 0, sizeof(spec.user));
		memset(spec.password, 0, sizeof(spec.password));
	}

	if (strlen(spec.user) == 0)
	{
		return &spec;
	}

	if ((strlen(spec.password) == 0) && (password == NULL))
	{
		char *password;
		if (!(isatty(0) && isatty(1)))
		{
			return NULL;
		}
		printf("Logging into %s as %s\n",
		       spec.server, spec.user);

		password = getpass("Password: ");
		if (strlen(password) > sizeof(spec.password))
		{
			return NULL;
		}
		strcpy(spec.password, password);
	}
	else
	{
		if (strcmp(spec.password, NWC_NOPASSWORD) == 0)
		{
			*spec.password = '\0';
		}
	}

	str_upper(spec.server);
	str_upper(spec.user);
	str_upper(spec.password);
	return &spec;
}

struct ncp_conn *
ncp_initialize_as(int *argc, char **argv,
		  int login_necessary, int login_type, long *err)
{
	char *server   = NULL;
	char *user     = NULL;
	char *password = NULL;
	struct ncp_conn_spec *spec;
	int i = 1;

	int get_argument(int arg_no, char **target)
	{
		int count = 1;

		if (target != NULL)
		{
			if (arg_no+1 >= *argc)
			{
				/* No argument to switch */
				errno = EINVAL;
				return -1;
			}
			*target = argv[arg_no+1];
			count = 2;
		}

		/* Delete the consumed switch from the argument list
		   and decrement the argument count */
		while (count+arg_no < *argc)
		{
			argv[arg_no] = argv[arg_no+count];
			arg_no += 1;
		}
		*argc -= count;
		return 0;
	}

	initialize_NCPL_error_table();	

	*err = EINVAL;

	while (i < *argc)
	{
		if (   (argv[i][0] != '-')
		    || (strlen(argv[i]) != 2))
		{
			i += 1;
			continue;
		}

		switch (argv[i][1])
		{
		case 'S':
			if (get_argument(i, &server) != 0)
			{
				return NULL;
			}
			continue;
		case 'U':
			if (get_argument(i, &user) != 0)
			{
				return NULL;
			}
			continue;
		case 'P':
			if (get_argument(i, &password) != 0)
			{
				return NULL;
			}
			continue;
		case 'n':
			if (get_argument(i, 0) != 0)
			{
				return NULL;
			}
			password = NWC_NOPASSWORD;
			continue;
		}
		i += 1;
	}

	spec = ncp_find_conn_spec(server, user, password, login_necessary,
				  getuid(), err);

	if (spec == NULL)
	{
		if (login_necessary != 0)
		{
			return NULL;
		}
		else
		{
			return ncp_open(NULL, err);
		}
	}
	
	spec->login_type = login_type;

	if (login_necessary == 0)
	{
		spec->user[0] = '\0';
	}

	return ncp_open(spec, err);
}
		
struct ncp_conn *
ncp_initialize(int *argc, char **argv,
	       int login_necessary, long *err)
{
	return ncp_initialize_as(argc, argv, login_necessary,
				 NCP_BINDERY_USER, err);
}

static long
ncp_request(struct ncp_conn *conn, int function)
{
	switch (conn->is_connected) {
	case CONN_PERMANENT:
		return ncp_mount_request(conn, function);
	case CONN_TEMPORARY:
		return ncp_temp_request(conn, function);
	default:
	}
	return ENOTCONN;
}

/****************************************************************************/
/*                                                                          */
/* Helper functions                                                         */
/*                                                                          */
/****************************************************************************/

static inline int
min(int a, int b)
{
	return (a<b) ? a : b;
}

struct nw_time_buffer {
	__u8 year        __attribute__ ((packed));
	__u8 month       __attribute__ ((packed));
	__u8 day         __attribute__ ((packed));
	__u8 hour        __attribute__ ((packed));
	__u8 minute      __attribute__ ((packed));
	__u8 second      __attribute__ ((packed));
	__u8 wday        __attribute__ ((packed));
};

static time_t
nw_to_ctime(struct nw_time_buffer *source)
{
	struct tm u_time;

	memzero(u_time);
	u_time.tm_sec   = source->second;
	u_time.tm_min   = source->minute;
	u_time.tm_hour  = source->hour;
	u_time.tm_mday  = source->day;
	u_time.tm_mon   = source->month - 1;
	u_time.tm_year  = source->year;

	if (u_time.tm_year < 80)
	{
		u_time.tm_year += 100;
	}

	return mktime(&u_time);
}

static void
assert_conn_locked(struct ncp_conn *conn)
{
	if (conn->lock == 0) {
		ncp_printf("ncpfs: conn not locked!\n");
	}
}

static void
ncp_add_byte(struct ncp_conn *conn, byte x)
{
	assert_conn_locked(conn);
	*(byte *)(&(conn->packet[conn->current_size])) = x;
	conn->current_size += 1;
	return;
}

static void
ncp_add_word(struct ncp_conn *conn, word x)
{
	assert_conn_locked(conn);
	*(word *)(&(conn->packet[conn->current_size])) = x;
	conn->current_size += 2;
	return;
}

static void
ncp_add_dword(struct ncp_conn *conn, dword x)
{
	assert_conn_locked(conn);
	*(dword *)(&(conn->packet[conn->current_size])) = x;
	conn->current_size += 4;
	return;
}

static void
ncp_add_mem(struct ncp_conn *conn, const void *source, int size)
{
	assert_conn_locked(conn);
	memcpy(&(conn->packet[conn->current_size]), source, size);
	conn->current_size += size;
	return;
}

static void
ncp_add_pstring(struct ncp_conn *conn, const char *s)
{
	int len = strlen(s);
	assert_conn_locked(conn);
	if (len > 255) {
		ncp_printf("ncpfs: string too long: %s\n", s);
		len = 255;
	}
	ncp_add_byte(conn, len);
	ncp_add_mem(conn, s, len);
	return;
}

static void
ncp_init_request(struct ncp_conn *conn)
{
	ncp_lock_conn(conn);

	conn->current_size = sizeof(struct ncp_request_header);
	conn->has_subfunction = 0;
}

static void
ncp_init_request_s(struct ncp_conn *conn, int subfunction)
{
	ncp_init_request(conn);
	ncp_add_word(conn, 0); /* preliminary size */

	ncp_add_byte(conn, subfunction);

	conn->has_subfunction = 1;
}

static char *
ncp_reply_data(struct ncp_conn *conn, int offset)
{
	return &(conn->packet[sizeof(struct ncp_reply_header) + offset]);
}

static byte
ncp_reply_byte(struct ncp_conn *conn, int offset)
{
	return *(byte *)(ncp_reply_data(conn, offset));
}

static word
ncp_reply_word(struct ncp_conn *conn, int offset)
{
	return *(word *)(ncp_reply_data(conn, offset));
}

static dword
ncp_reply_dword(struct ncp_conn *conn, int offset)
{
	return *(dword *)(ncp_reply_data(conn, offset));
}

/* Here the ncp calls begin
 */

static long
ncp_negotiate_buffersize(struct ncp_conn *conn,
			 int size, int *target)
{
	long result;

	ncp_init_request(conn);
	ncp_add_word(conn, htons(size));
	
	if ((result = ncp_request(conn, 33)) != 0) {
		ncp_unlock_conn(conn);
		return result;
	}

	*target =min(ntohs(ncp_reply_word(conn, 0)), size);

	ncp_unlock_conn(conn);
	return 0;
}


long
ncp_get_file_server_description_strings(struct ncp_conn *conn,
					char target[512])
{
	long result;

	ncp_init_request_s(conn, 201);

	if ((result = ncp_request(conn, 23)) != 0)
	{
		ncp_unlock_conn(conn);
		return result;
	}

	memcpy(target, ncp_reply_data(conn, 0), 512);
	ncp_unlock_conn(conn);
	return 0;
}

long
ncp_get_file_server_time(struct ncp_conn *conn, time_t *target)
{
	long result;

	ncp_init_request(conn);

	if ((result = ncp_request(conn, 20)) != 0)
	{
		ncp_unlock_conn(conn);
		return result;
	}

	*target= nw_to_ctime((struct nw_time_buffer *)ncp_reply_data(conn, 0));
	ncp_unlock_conn(conn);
	return 0;
}

long
ncp_get_connlist(struct ncp_conn *conn,
		 __u16 object_type, const char *object_name,
		 int *returned_no, __u8 conn_numbers[256])
{
	long result;

	ncp_init_request_s(conn, 21);
	ncp_add_word(conn, htons(object_type));
	ncp_add_pstring(conn, object_name);

	if ((result = ncp_request(conn, 23)) != 0)
	{
		ncp_unlock_conn(conn);
		return result;
	}

	*returned_no = ncp_reply_byte(conn, 0);
	memcpy(conn_numbers, ncp_reply_data(conn, 1), (*returned_no));
	ncp_unlock_conn(conn);
	return 0;
}

long
ncp_send_broadcast(struct ncp_conn *conn,
		   __u8 no_conn, const __u8 *connections,
		   const char *message)
{
	long result;

	if (strlen(message) > 58)
	{
		return NCPL_ET_MSG_TOO_LONG;
	}

	ncp_init_request_s(conn, 0);
	ncp_add_byte(conn, no_conn);
	ncp_add_mem(conn, (char *)(connections), no_conn);
	ncp_add_pstring(conn, message);

	result = ncp_request(conn, 21);
	ncp_unlock_conn(conn);
	return result;
}

/*
 * result is a 8-byte buffer
 */
long
ncp_get_encryption_key(struct ncp_conn *conn,
		       char *target)
{
	long result;

	ncp_init_request_s(conn, 23);

	if ((result = ncp_request(conn, 23)) != 0) {
		ncp_unlock_conn(conn);
		return result;
	}

	if (conn->ncp_reply_size < 8) {
		ncp_printf("ncp_reply_size %d < 8\n",
		       conn->ncp_reply_size);
		ncp_unlock_conn(conn);
		return result;
	}

	memcpy(target, ncp_reply_data(conn, 0), 8);
	ncp_unlock_conn(conn);
	return 0;
}

long
ncp_get_bindery_object_id(struct ncp_conn *conn,
			  __u16 object_type,
			  const char *object_name,
			  struct ncp_bindery_object *target)
{
	long result;
	ncp_init_request_s(conn, 53);
	ncp_add_word(conn, htons(object_type));
	ncp_add_pstring(conn, object_name);

	if ((result = ncp_request(conn, 23)) != 0) {
		ncp_unlock_conn(conn);
		return result;
	}

	if (conn->ncp_reply_size < 54) {
		ncp_printf("ncp_reply_size %d < 54\n",
		       conn->ncp_reply_size);
		ncp_unlock_conn(conn);
		return result;
	}

	target->object_id   = ntohl(ncp_reply_dword(conn, 0));
	target->object_type = ntohs(ncp_reply_word (conn, 4));
	memcpy(target->object_name, ncp_reply_data(conn, 6), 48);
	ncp_unlock_conn(conn);
	return 0;
}

long
ncp_get_bindery_object_name(struct ncp_conn *conn,
			    __u32 object_id,
			    struct ncp_bindery_object *target)
{
	long result;
	ncp_init_request_s(conn, 54);
	ncp_add_dword(conn, htonl(object_id));

	if ((result = ncp_request(conn, 23)) != 0) {
		ncp_unlock_conn(conn);
		return result;
	}

	target->object_id   = ntohl(ncp_reply_dword(conn, 0));
	target->object_type = ntohs(ncp_reply_word (conn, 4));
	memcpy(target->object_name, ncp_reply_data(conn, 6), 48);
	ncp_unlock_conn(conn);
	return 0;
}

long
ncp_scan_bindery_object(struct ncp_conn *conn,
			__u32 last_id, __u16 object_type, char *search_string,
			struct ncp_bindery_object *target)
{
	long result;
	ncp_init_request_s(conn, 55);
	ncp_add_dword(conn, htonl(last_id));
	ncp_add_word(conn, htons(object_type));
	ncp_add_pstring(conn, search_string);

	if ((result = ncp_request(conn, 23)) != 0) {
		ncp_unlock_conn(conn);
		return result;
	}

	target->object_id = ntohl(ncp_reply_dword(conn, 0));
	target->object_type = ntohs(ncp_reply_word(conn, 4));
	memcpy(target->object_name, ncp_reply_data(conn, 6),
	       NCP_BINDERY_NAME_LEN);
	target->object_flags = ncp_reply_byte(conn, 54);
	target->object_security = ncp_reply_byte(conn, 55);
	target->object_has_prop = ncp_reply_byte(conn, 56);

	ncp_unlock_conn(conn);
	return 0;
}
	
long
ncp_create_bindery_object(struct ncp_conn *conn,
			  struct ncp_bindery_object *source)
{
	long result;
	ncp_init_request_s(conn, 50);
	ncp_add_byte(conn, source->object_flags);
	ncp_add_byte(conn, source->object_security);
	ncp_add_word(conn, htons(source->object_type));
	ncp_add_pstring(conn, source->object_name);

	result = ncp_request(conn, 23);
	ncp_unlock_conn(conn);
	return result;
}


long
ncp_delete_bindery_object(struct ncp_conn *conn,
			  __u16 object_type,
			  const char *object_name)
{
	long result;
	ncp_init_request_s(conn, 51);
	ncp_add_word(conn, htons(object_type));
	ncp_add_pstring(conn, object_name);

	result = ncp_request(conn, 23);
	ncp_unlock_conn(conn);
	return result;
}

long
ncp_read_property_value(struct ncp_conn *conn,
			int object_type, const char *object_name,
			int segment, const char *prop_name,
			struct nw_property *target)
{
	long result;
	ncp_init_request_s(conn, 61);
	ncp_add_word(conn, htons(object_type));
	ncp_add_pstring(conn, object_name);
	ncp_add_byte(conn, segment);
	ncp_add_pstring(conn, prop_name);

	if ((result = ncp_request(conn, 23)) != 0) {
		ncp_unlock_conn(conn);
		return result;
	}

	memcpy(&(target->value), ncp_reply_data(conn, 0), 128);
	target->more_flag = ncp_reply_byte(conn, 128);
	target->property_flag = ncp_reply_byte(conn, 129);
	ncp_unlock_conn(conn);
	return 0;
}


long
ncp_scan_property(struct ncp_conn *conn,
		  __u16 object_type, const char *object_name,
		  __u32 last_id, char *search_string,
	  	  struct ncp_property_info *property_info)
{
	long result;
	ncp_init_request_s(conn, 60);
	ncp_add_word(conn, htons(object_type));
	ncp_add_pstring(conn, object_name);
	ncp_add_dword(conn, htonl(last_id));
	ncp_add_pstring(conn, search_string);

	if ((result = ncp_request(conn, 23)) != 0) {
		ncp_unlock_conn(conn);
		return result;
	}

	memcpy(property_info->property_name,ncp_reply_data(conn, 0), 16);
	property_info->property_flags = ncp_reply_byte(conn,16);
	property_info->property_security = ncp_reply_byte(conn,17);
	property_info->search_instance = ntohl(ncp_reply_dword(conn,18));
	property_info->value_available_flag = ncp_reply_byte(conn,22);
	property_info->more_properties_flag = ncp_reply_byte(conn,23);
	ncp_unlock_conn(conn);
	return 0;
}

long
ncp_add_object_to_set(struct ncp_conn *conn,
		      __u16 object_type, const char *object_name,
		      const char *property_name,
		      __u16 member_type,
		      const char *member_name)
{
	long result;
	ncp_init_request_s(conn, 65);
	ncp_add_word(conn, htons(object_type));
	ncp_add_pstring(conn, object_name);
	ncp_add_pstring(conn, property_name);
	ncp_add_word(conn, htons(member_type));
	ncp_add_pstring(conn, member_name);

	result = ncp_request(conn, 23);
	ncp_unlock_conn(conn);
	return result;
}

long
ncp_change_property_security(struct ncp_conn *conn,
			     __u16 object_type, const char *object_name,
		  	     const char *property_name,
			     __u8 property_security)
{
	long result;
	ncp_init_request_s(conn, 59);
	ncp_add_word(conn, htons(object_type));
	ncp_add_pstring(conn, object_name);
	ncp_add_byte(conn, property_security);
	ncp_add_pstring(conn, property_name);

	result = ncp_request(conn, 23);
	ncp_unlock_conn(conn);
	return result;
}

long
ncp_create_property(struct ncp_conn *conn,
		     __u16 object_type, const char *object_name,
	  	     const char *property_name,
		     __u8 property_flags, __u8 property_security)
{
	long result;
	ncp_init_request_s(conn, 57);
	ncp_add_word(conn, htons(object_type));
	ncp_add_pstring(conn, object_name);
	ncp_add_byte(conn, property_flags);
	ncp_add_byte(conn, property_security);
	ncp_add_pstring(conn, property_name);

	result = ncp_request(conn, 23);
	ncp_unlock_conn(conn);
	return result;
}

long
ncp_delete_object_from_set(struct ncp_conn *conn,
			   __u16 object_type, const char *object_name,
			   const char *property_name,
			   __u16 member_type,
			   const char *member_name)
{
	long result;
	ncp_init_request_s(conn, 66);
	ncp_add_word(conn, htons(object_type));
	ncp_add_pstring(conn, object_name);
	ncp_add_pstring(conn, property_name);
	ncp_add_word(conn, htons(member_type));
	ncp_add_pstring(conn, member_name);

	result = ncp_request(conn, 23);
	ncp_unlock_conn(conn);
	return result;
}

long 
ncp_delete_property(struct ncp_conn *conn,
		    __u16 object_type, const char *object_name,
		    const char *property_name)
{
	long result;
	ncp_init_request_s(conn, 58);
	ncp_add_word(conn, htons(object_type));
	ncp_add_pstring(conn, object_name);
	ncp_add_pstring(conn, property_name);

	result = ncp_request(conn, 23);
	ncp_unlock_conn(conn);
	return result;
}

long
ncp_write_property_value(struct ncp_conn *conn,
		         __u16 object_type, const char *object_name,
		         const char *property_name,
			 __u8 segment,
			 struct nw_property *property_value)
{
	long result;
	ncp_init_request_s(conn, 62);
	ncp_add_word(conn, htons(object_type));
	ncp_add_pstring(conn, object_name);
	ncp_add_byte(conn,segment);
	ncp_add_byte(conn, property_value->more_flag);
	ncp_add_pstring(conn, property_name);
	ncp_add_mem(conn, property_value->value, 128);

	result = ncp_request(conn, 23);
	ncp_unlock_conn(conn);
	return result;
}

long
ncp_login_encrypted(struct ncp_conn *conn,
		    const struct ncp_bindery_object *object,
		    const unsigned char *key,
		    const unsigned char *passwd)
{
	dword tmpID = htonl(object->object_id);
	unsigned char buf[128];
	unsigned char encrypted[8];
	long result;

	shuffle((byte *)&tmpID, passwd, strlen(passwd), buf);
	nw_encrypt(key, buf, encrypted);

	ncp_init_request_s(conn, 24);
	ncp_add_mem(conn, encrypted, 8);
	ncp_add_word(conn, htons(object->object_type));
	ncp_add_pstring(conn, object->object_name);

	result = ncp_request(conn, 23);
	ncp_unlock_conn(conn);
	return result;
}

long
ncp_login_user(struct ncp_conn *conn,
	       const unsigned char *username,
	       const unsigned char *password)
{
	return ncp_login_object(conn, username, NCP_BINDERY_USER, password);
}
		
static long
ncp_login_object(struct ncp_conn *conn,
		 const unsigned char *username,
		 int login_type,
		 const unsigned char *password)
{
	long result;
	unsigned char ncp_key[8];
	struct ncp_bindery_object user;

	if ((result = ncp_get_encryption_key(conn, ncp_key)) != 0) {
		return result;
	}

	if ((result = ncp_get_bindery_object_id(conn, login_type,
						username, &user)) != 0) {
		return result;
	}

	if ((result = ncp_login_encrypted(conn, &user,
					  ncp_key, password)) != 0)
	{
		struct nw_property p;
		struct ncp_prop_login_control *l
			= (struct ncp_prop_login_control *)&p;

		if (conn->completion != NCP_GRACE_PERIOD)
		{
			return result;
		}

		fprintf(stderr, "Your password has expired\n");

		if ((result = ncp_read_property_value(conn, NCP_BINDERY_USER,
						      username, 1,
						      "LOGIN_CONTROL",
						      &p)) == 0)
		{
			fprintf(stderr, "You have %d login attempts left\n",
				l->GraceLogins);
		}
	}
	return 0;
}

long
ncp_get_volume_info_with_number(struct ncp_conn *conn, int n,
				struct ncp_volume_info *target)
{
	long result;
	int len;

	ncp_init_request_s(conn, 44);
	ncp_add_byte(conn, n);

	if ((result = ncp_request(conn, 22)) != 0) {
		ncp_unlock_conn(conn);
		return result;
	}

	target->total_blocks = ncp_reply_dword(conn, 0);
	target->free_blocks  = ncp_reply_dword(conn, 4);
	target->purgeable_blocks = ncp_reply_dword(conn, 8);
	target->not_yet_purgeable_blocks = ncp_reply_dword(conn, 12);
	target->total_dir_entries = ncp_reply_dword(conn, 16);
	target->available_dir_entries = ncp_reply_dword(conn, 20);
	target->sectors_per_block = ncp_reply_byte(conn, 28);

	memzero(target->volume_name);

	len = ncp_reply_byte(conn, 29);
	if (len > NCP_VOLNAME_LEN) {
		ncp_printf("ncpfs: volume name too long: %d\n", len);
		ncp_unlock_conn(conn);
		return -EIO;
	}

	memcpy(&(target->volume_name), ncp_reply_data(conn, 30), len);
	ncp_unlock_conn(conn);
	return 0;
}

long
ncp_get_volume_number(struct ncp_conn *conn, const char *name, int *target)
{
	long result;

	ncp_init_request_s(conn, 5);
	ncp_add_pstring(conn, name);

	if ((result = ncp_request(conn, 22)) != 0) {
		ncp_unlock_conn(conn);
		return result;
	}

	*target = ncp_reply_byte(conn, 0);
	ncp_unlock_conn(conn);
	return 0;
}


long
ncp_file_search_init(struct ncp_conn *conn,
		     int dir_handle, const char *path,
		     struct ncp_filesearch_info *target)
{
	long result;

	ncp_init_request(conn);
	ncp_add_byte(conn, dir_handle);
	ncp_add_pstring(conn, path);

	if ((result = ncp_request(conn, 62)) != 0) {
		ncp_unlock_conn(conn);
		return result;
	}

	target->volume_number = ncp_reply_byte(conn, 0);
	target->directory_id  = ntohs(ncp_reply_word(conn, 1));
	target->sequence_no   = ntohs(ncp_reply_word(conn, 3));
	target->access_rights = ncp_reply_byte(conn, 5);
	ncp_unlock_conn(conn);
	return 0;
}
	

long
ncp_file_search_continue(struct ncp_conn *conn,
			 struct ncp_filesearch_info *fsinfo,
			 int attributes, const char *name,
			 struct ncp_file_info *target)
{
	long result;

	ncp_init_request(conn);

	ncp_add_byte(conn, fsinfo->volume_number);
	ncp_add_word(conn, htons(fsinfo->directory_id));
	ncp_add_word(conn, htons(fsinfo->sequence_no));

	ncp_add_byte(conn, attributes);
	ncp_add_pstring(conn, name);

	if ((result = ncp_request(conn, 63)) != 0) {
		ncp_unlock_conn(conn);
		return result;
	}

	fsinfo->sequence_no = ntohs(ncp_reply_word(conn, 0));

	memzero(target->file_name);
	memcpy(&(target->file_name), ncp_reply_data(conn, 4),
	       NCP_MAX_FILENAME);

	target->file_attributes = ncp_reply_byte(conn, 18);
	target->file_mode       = ncp_reply_byte(conn, 19);
	target->file_length     = ntohl(ncp_reply_dword(conn, 20));
	target->creation_date   = ntohs(ncp_reply_word(conn, 24));
	target->access_date     = ntohs(ncp_reply_word(conn, 26));
	target->update_date     = ntohs(ncp_reply_word(conn, 28));
	target->update_time     = ntohs(ncp_reply_word(conn, 30));

	ncp_unlock_conn(conn);
	return 0;
}

long
ncp_get_finfo(struct ncp_conn *conn,
	      int dir_handle, const char *path, const char *name,
	      struct ncp_file_info *target)
{
	long result;

	struct ncp_filesearch_info fsinfo;

	if ((result = ncp_file_search_init(conn, dir_handle, path,
					   &fsinfo)) != 0) {
		return result;
	}

	if ((result = ncp_file_search_continue(conn, &fsinfo, 0, name,
					       target)) == 0) {
		return result;
	}

	if ((result = ncp_file_search_init(conn, dir_handle, path,
					   &fsinfo)) != 0) {
		return result;
	}

	return ncp_file_search_continue(conn, &fsinfo, aDIR, name, target);
}

long
ncp_open_file(struct ncp_conn *conn,
	      int dir_handle, const char *path,
	      int attr, int access,
	      struct ncp_file_info *target)
{
	long result;

	ncp_init_request(conn);
	ncp_add_byte(conn, dir_handle);
	ncp_add_byte(conn, attr);
	ncp_add_byte(conn, access);
	ncp_add_pstring(conn, path);

	if ((result = ncp_request(conn, 76)) != 0) {
		ncp_unlock_conn(conn);
		return result;
	}

	memcpy(&(target->file_id), ncp_reply_data(conn, 0),
	       NCP_FILE_ID_LEN);
	
	memzero(target->file_name);
	memcpy(&(target->file_name), ncp_reply_data(conn, 8),
	       NCP_MAX_FILENAME);

	target->file_attributes = ncp_reply_byte(conn, 22);
	target->file_mode       = ncp_reply_byte(conn, 23);
	target->file_length     = ntohl(ncp_reply_dword(conn, 24));
	target->creation_date   = ntohs(ncp_reply_word(conn, 28));
	target->access_date     = ntohs(ncp_reply_word(conn, 30));
	target->update_date     = ntohs(ncp_reply_word(conn, 32));
	target->update_time     = ntohs(ncp_reply_word(conn, 34));

	ncp_unlock_conn(conn);
	return 0;
}

long
ncp_close_file(struct ncp_conn *conn, const char *file_id)
{
	long result;

	ncp_init_request(conn);
	ncp_add_byte(conn, 0);
	ncp_add_mem(conn, file_id, 6);

	result = ncp_request(conn, 66);
	ncp_unlock_conn(conn);
	return result;
}

static int
ncp_do_create(struct ncp_conn *conn,
	      int dir_handle, const char *path,
	      int attr,
	      struct ncp_file_info *target,
	      int function)
{
	long result;
	
	ncp_init_request(conn);
	ncp_add_byte(conn, dir_handle);
	ncp_add_byte(conn, attr);
	ncp_add_pstring(conn, path);

	if ((result = ncp_request(conn, function)) != 0) {
		ncp_unlock_conn(conn);
		return result;
	}

	memcpy(&(target->file_id), ncp_reply_data(conn, 0),
	       NCP_FILE_ID_LEN);
	
	memzero(target->file_name);
	memcpy(&(target->file_name), ncp_reply_data(conn, 8),
	       NCP_MAX_FILENAME);

	target->file_attributes = ncp_reply_byte(conn, 22);
	target->file_mode       = ncp_reply_byte(conn, 23);
	target->file_length     = ntohl(ncp_reply_dword(conn, 24));
	target->creation_date   = ntohs(ncp_reply_word(conn, 28));
	target->access_date     = ntohs(ncp_reply_word(conn, 30));
	target->update_date     = ntohs(ncp_reply_word(conn, 32));
	target->update_time     = ntohs(ncp_reply_word(conn, 34));

	ncp_unlock_conn(conn);
	return 0;
}	

long
ncp_create_newfile(struct ncp_conn *conn,
		   int dir_handle, const char *path,
		   int attr,
		   struct ncp_file_info *target)
{
	return ncp_do_create(conn, dir_handle, path, attr, target, 77);
}

long
ncp_create_file(struct ncp_conn *conn,
		int dir_handle, const char *path,
		int attr,
		struct ncp_file_info *target)
{
	return ncp_do_create(conn, dir_handle, path, attr, target, 67);
}

long
ncp_erase_file(struct ncp_conn *conn,
	       int dir_handle, const char *path,
	       int attr)
{
	long result;

	ncp_init_request(conn);
	ncp_add_byte(conn, dir_handle);
	ncp_add_byte(conn, attr);
	ncp_add_pstring(conn, path);

	result = ncp_request(conn, 68);
	ncp_unlock_conn(conn);
	return result;
}
	
long
ncp_rename_file(struct ncp_conn *conn,
		int old_handle, const char *old_path,
		int attr,
		int new_handle, const char *new_path)
{
	long result;

	ncp_init_request(conn);
	ncp_add_byte(conn, old_handle);
	ncp_add_byte(conn, attr);
	ncp_add_pstring(conn, old_path);
	ncp_add_byte(conn, new_handle);
	ncp_add_pstring(conn, new_path);

	if ((result = ncp_request(conn, 69)) != 0) {
		ncp_unlock_conn(conn);
		return result;
	}

	ncp_unlock_conn(conn);
	return 0;
}

long
ncp_create_directory(struct ncp_conn *conn,
		     int dir_handle, const char *path,
		     int inherit_mask)
{
	long result;

	ncp_init_request_s(conn, 10);
	ncp_add_byte(conn, dir_handle);
	ncp_add_byte(conn, inherit_mask);
	ncp_add_pstring(conn, path);

	result = ncp_request(conn, 22);
	ncp_unlock_conn(conn);
	return result;
}

long
ncp_delete_directory(struct ncp_conn *conn,
		     int dir_handle, const char *path)
{
	long result;

	ncp_init_request_s(conn, 11);
	ncp_add_byte(conn, dir_handle);
	ncp_add_byte(conn, 0);	/* reserved */
	ncp_add_pstring(conn, path);

	result = ncp_request(conn, 22);
	ncp_unlock_conn(conn);
	return result;
}
	
long
ncp_rename_directory(struct ncp_conn *conn,
		     int dir_handle,
		     const char *old_path, const char *new_path)
{
	long result;

	ncp_init_request_s(conn, 15);
	ncp_add_byte(conn, dir_handle);
	ncp_add_pstring(conn, old_path);
	ncp_add_pstring(conn, new_path);

	result = ncp_request(conn, 22);
	ncp_unlock_conn(conn);
	return result;
}

static void
ncp_add_handle_path(struct ncp_conn *conn,
		    __u8 vol_num,
		    __u32 dir_base, int have_dir_base,
		    char *path)
{
	ncp_add_byte(conn, vol_num);
	ncp_add_dword(conn, dir_base);
	if (have_dir_base != 0) {
		ncp_add_byte(conn, 1); /* dir_base */
	} else {
		ncp_add_byte(conn, 0xff); /* no handle */
	}
	if (path != NULL) {
		ncp_add_byte(conn, 1); /* 1 component */
		ncp_add_pstring(conn, path);
	}
	else
	{
		ncp_add_byte(conn, 0);
	}
}

static void
ncp_extract_file_info(void *structure, struct nw_info_struct *target)
{
	__u8 *name_len;
	const int info_struct_size = sizeof(struct nw_info_struct) - 257;

	memcpy(target, structure, info_struct_size);
	name_len = structure + info_struct_size;
	target->nameLen = *name_len;
	strncpy(target->entryName, name_len+1, *name_len);
	target->entryName[*name_len] = '\0';
	return;
}

long
ncp_do_lookup(struct ncp_conn *conn,
	      struct nw_info_struct *dir,
	      char *path,	/* may only be one component */
	      struct nw_info_struct *target)
{
	__u8  vol_num;
	__u32 dir_base;
	long result;
	char *volname = NULL;

	if (target == NULL) {
		return -EINVAL;
	}
 
	if (dir == NULL) {

		/* Access a volume's root directory */
		ncp_init_request(conn);
		ncp_add_byte(conn, 22); /* subfunction */
		ncp_add_byte(conn, 0); /* dos name space */
		ncp_add_byte(conn, 0); /* reserved */
		ncp_add_byte(conn, 0); /* reserved */
		ncp_add_byte(conn, 0); /* reserved */
		ncp_add_handle_path(conn, 0, 0, 0, /* no handle */
				    path);

		if ((result = ncp_request(conn, 87)) != 0) {
			ncp_unlock_conn(conn);
			return result;
		}

		dir_base = ncp_reply_dword(conn, 4);
		vol_num  = ncp_reply_byte (conn, 8);
		ncp_unlock_conn(conn);
		volname = path;
		path = NULL;
	}
	else
	{
		vol_num = dir->volNumber;
		dir_base = dir->DosDirNum;
	}

	ncp_init_request(conn);
	ncp_add_byte(conn, 6); /* subfunction */
	ncp_add_byte(conn, 0); /* dos name space */
	ncp_add_byte(conn, 0); /* dos name space as dest */
	ncp_add_word(conn, 0xff); /* get all */
	ncp_add_dword(conn, RIM_ALL);
	ncp_add_handle_path(conn, vol_num, dir_base, 1,
			    path);

	if ((result = ncp_request(conn, 87)) != 0) {
		ncp_unlock_conn(conn);
		return result;
	}

	ncp_extract_file_info(ncp_reply_data(conn, 0), target);

	if (volname != NULL) {
		target->nameLen = strlen(volname);
		strcpy(target->entryName, volname);
	}

	ncp_unlock_conn(conn);
	return 0;
}

long
ncp_modify_file_or_subdir_dos_info(struct ncp_conn *conn,
				   struct nw_info_struct *file,
				   __u32 info_mask,
				   struct nw_modify_dos_info *info)
{
	long result;

	ncp_init_request(conn);
	ncp_add_byte(conn, 7);	/* subfunction */
	ncp_add_byte(conn, 0);	/* dos name space */
	ncp_add_byte(conn, 0);	/* reserved */
	ncp_add_word(conn, 0x8006); /* search attribs: all */

	ncp_add_dword(conn, info_mask);
	ncp_add_mem(conn, info, sizeof(*info));
	ncp_add_handle_path(conn, file->volNumber,
			    file->DosDirNum, 1, NULL);

	result = ncp_request(conn, 87);
	ncp_unlock_conn(conn);
	return result;
}
	
long
ncp_del_file_or_subdir(struct ncp_conn *conn,
		       struct nw_info_struct *dir, char *name)
{
	long result;

	ncp_init_request(conn);
	ncp_add_byte(conn, 8);	/* subfunction */
	ncp_add_byte(conn, 0);	/* dos name space */
	ncp_add_byte(conn, 0);	/* reserved */
	ncp_add_word(conn, 0x8006); /* search attribs: all */
	ncp_add_handle_path(conn, dir->volNumber,
			    dir->DosDirNum, 1, name);
	
	result = ncp_request(conn, 87);
	ncp_unlock_conn(conn);
	return result;
}

static inline void
ConvertToNWfromDWORD ( __u32 sfd , __u8 ret[6] )
{
    __u16 *dest = (__u16 *) ret;
    memcpy(&(dest[1]), &sfd, 4);
    dest[0] = dest[1] + 1;
    return;
}

long
ncp_open_create_file_or_subdir(struct ncp_conn *conn,
			       struct nw_info_struct *dir, char *name,
			       int open_create_mode,
			       __u32 create_attributes,
			       int desired_acc_rights,
			       struct nw_file_info *target)
{
	long result;

	target->opened = 0;

	ncp_init_request(conn);
	ncp_add_byte(conn, 1); /* subfunction */
	ncp_add_byte(conn, 0); /* dos name space */
	ncp_add_byte(conn, open_create_mode);
	ncp_add_word(conn, 0x8006);
	ncp_add_dword(conn, RIM_ALL);
	ncp_add_dword(conn, create_attributes);
	/* The desired acc rights seem to be the inherited rights mask
	   for directories */
	ncp_add_word(conn, desired_acc_rights);
	ncp_add_handle_path(conn, dir->volNumber,
			    dir->DosDirNum, 1, name);

	if ((result = ncp_request(conn, 87)) != 0) {
		ncp_unlock_conn(conn);
		return result;
	}

	target->opened = 1;
	target->server_file_handle = ncp_reply_dword(conn, 0);
	target->open_create_action = ncp_reply_byte(conn, 4);
	ncp_extract_file_info(ncp_reply_data(conn, 5), &(target->i));
	ConvertToNWfromDWORD(target->server_file_handle, target->file_handle);

	ncp_unlock_conn(conn);
	return 0;
}

long
ncp_initialize_search(struct ncp_conn *conn,
		      const struct nw_info_struct *dir,
		      int namespace,
		      struct ncp_search_seq *target)
{
	long result;

	if ((namespace < 0) || (namespace > 255))
	{
		return EINVAL;
	}

	memzero(*target);

	ncp_init_request(conn);
	ncp_add_byte(conn, 2); /* subfunction */
	ncp_add_byte(conn, namespace);
	ncp_add_byte(conn, 0); /* reserved */
	ncp_add_handle_path(conn, dir->volNumber,
			    dir->DosDirNum, 1, NULL);
	
	if ((result = ncp_request(conn, 87)) != 0) {
		ncp_unlock_conn(conn);
		return result;
	}

	memcpy(&(target->s), ncp_reply_data(conn, 0), sizeof(target->s));
	target->namespace = namespace;

	ncp_unlock_conn(conn);
	return 0;
}
	
/* Search for everything */
long
ncp_search_for_file_or_subdir(struct ncp_conn *conn,
			      struct ncp_search_seq *seq,
			      struct nw_info_struct *target)
{
	long result;

	ncp_init_request(conn);
	ncp_add_byte(conn, 3); /* subfunction */
	ncp_add_byte(conn, seq->namespace); 
	ncp_add_byte(conn, 0); /* data stream (???) */
	ncp_add_word(conn, 0xffff); /* Search attribs */
	ncp_add_dword(conn, RIM_ALL);	/* return info mask */
	ncp_add_mem(conn, &(seq->s), 9);
	ncp_add_byte(conn, 2); /* 2 byte pattern */
	ncp_add_byte(conn, 0xff); /* following is a wildcard */
	ncp_add_byte(conn, '*');
	
	if ((result = ncp_request(conn, 87)) != 0) {
		ncp_unlock_conn(conn);
		return result;
	}

	memcpy(seq, ncp_reply_data(conn, 0), sizeof(*seq));
	ncp_extract_file_info(ncp_reply_data(conn, 10), target);

	ncp_unlock_conn(conn);
	return 0;
}

long
ncp_ren_or_mov_file_or_subdir(struct ncp_conn *conn,
			      struct nw_info_struct *old_dir, char *old_name,
			      struct nw_info_struct *new_dir, char *new_name)
{
	long result;

	if (   (old_dir == NULL) || (old_name == NULL)
	    || (new_dir == NULL) || (new_name == NULL))
		return -EINVAL;
	
	ncp_init_request(conn);
	ncp_add_byte(conn, 4); /* subfunction */
	ncp_add_byte(conn, 0); /* dos name space */
	ncp_add_byte(conn, 1); /* rename flag */
	ncp_add_word(conn, 0x8006); /* search attributes */

	/* source Handle Path */
	ncp_add_byte(conn, old_dir->volNumber);
	ncp_add_dword(conn, old_dir->DosDirNum);
	ncp_add_byte(conn, 1);
	ncp_add_byte(conn, 1); /* 1 source component */

	/* dest Handle Path */
	ncp_add_byte(conn, new_dir->volNumber);
	ncp_add_dword(conn, new_dir->DosDirNum);
	ncp_add_byte(conn, 1);
	ncp_add_byte(conn, 1); /* 1 destination component */

	/* source path string */
	ncp_add_pstring(conn, old_name);
	/* dest path string */
	ncp_add_pstring(conn, new_name);

	result = ncp_request(conn, 87);
	ncp_unlock_conn(conn);
	return result;
}
	

/* Create a new job entry */
long
ncp_create_queue_job_and_file(struct ncp_conn *conn,
			      __u32 queue_id,
			      struct queue_job *job)
{
	long result;

	ncp_init_request_s(conn, 121);
	ncp_add_dword(conn, htonl(queue_id));
	ncp_add_mem(conn, &(job->j), sizeof(job->j));

	if ((result = ncp_request(conn, 23)) != 0) {
		ncp_unlock_conn(conn);
		return result;
	}

	memcpy(&(job->j), ncp_reply_data(conn, 0), 78);
	ConvertToNWfromDWORD(job->j.JobFileHandle, job->file_handle);

	ncp_unlock_conn(conn);
	return 0;
}

long
ncp_close_file_and_start_job(struct ncp_conn *conn,
			     __u32 queue_id,
			     struct queue_job *job)
{
	long result;

	ncp_init_request_s(conn, 127);
	ncp_add_dword(conn, htonl(queue_id));
	ncp_add_dword(conn, job->j.JobNumber);

	result = ncp_request(conn, 23);
	ncp_unlock_conn(conn);
	return result;
}

long
ncp_attach_to_queue(struct ncp_conn *conn,
		    __u32 queue_id)
{
	long result;

	ncp_init_request_s(conn, 111);
	ncp_add_dword(conn, htonl(queue_id));

	result = ncp_request(conn, 23);
	ncp_unlock_conn(conn);
	return result;
}

long
ncp_detach_from_queue(struct ncp_conn *conn,
		      __u32 queue_id)
{
	long result;

	ncp_init_request_s(conn, 112);
	ncp_add_dword(conn, htonl(queue_id));

	result = ncp_request(conn, 23);
	ncp_unlock_conn(conn);
	return result;
}

long
ncp_service_queue_job(struct ncp_conn *conn, __u32 queue_id, __u16 job_type,
		      struct queue_job *job)
{
	long result;

	ncp_init_request_s(conn, 124);
	ncp_add_dword(conn, htonl(queue_id));
	ncp_add_word(conn, htons(job_type));

	if ((result = ncp_request(conn, 23)) != 0) {
		ncp_unlock_conn(conn);
		return result;
	}

	memcpy(&(job->j), ncp_reply_data(conn, 0), 78);
	ConvertToNWfromDWORD(job->j.JobFileHandle, job->file_handle);

	ncp_unlock_conn(conn);
	return 0;
}

long
ncp_finish_servicing_job(struct ncp_conn *conn, __u32 queue_id,
			 __u32 job_number, __u32 charge_info)
{
	long result;

	ncp_init_request_s(conn, 131);
	ncp_add_dword(conn, htonl(queue_id));
	ncp_add_dword(conn, job_number);
	ncp_add_dword(conn, htonl(charge_info));

	result = ncp_request(conn, 23);
	ncp_unlock_conn(conn);
	return result;
}

long
ncp_abort_servicing_job(struct ncp_conn *conn, __u32 queue_id,
			__u32 job_number)
{
	long result;

	ncp_init_request_s(conn, 132);
	ncp_add_dword(conn, htonl(queue_id));
	ncp_add_dword(conn, job_number);

	result = ncp_request(conn, 23);
	ncp_unlock_conn(conn);
	return result;
}

static int
ncp_do_read(struct ncp_conn *conn, const char *file_id,
	    __u32 offset, __u16 to_read,
	    char *target, int *bytes_read)
{
	long result;

	ncp_init_request(conn);
	ncp_add_byte(conn, 0);
	ncp_add_mem(conn, file_id, 6);
	ncp_add_dword(conn, htonl(offset));
	ncp_add_word(conn, htons(to_read));

	if ((result = ncp_request(conn, 72)) != 0) {
		ncp_unlock_conn(conn);
		return result;
	}

	*bytes_read = ntohs(ncp_reply_word(conn, 0));

	memcpy(target, ncp_reply_data(conn, 2), *bytes_read);

	ncp_unlock_conn(conn);
	return 0;
}
	
long
ncp_read(struct ncp_conn *conn, const char *file_id,
	 off_t offset, size_t count, char *target)
{
	const int bufsize = conn->i.buffer_size;
	int already_read = 0;

	while (already_read < count)
	{
		int read_this_time;
		int to_read = min(bufsize - (offset % bufsize),
				  count - already_read);

		if (ncp_do_read(conn, file_id, offset, to_read,
				target, &read_this_time) != 0)
		{
			return -1;
		}

		offset += read_this_time;
		target += read_this_time;
		already_read += read_this_time;

		if (read_this_time < to_read)
		{
			break;
		}
	}
	return already_read;
}

static int
ncp_do_write(struct ncp_conn *conn, const char *file_id,
	     __u32 offset, __u16 to_write,
	     const char *source, int *bytes_written)
{
	long result;

	ncp_init_request(conn);
	ncp_add_byte(conn, 0);
	ncp_add_mem(conn, file_id, 6);
	ncp_add_dword(conn, htonl(offset));
	ncp_add_word(conn, htons(to_write));
	ncp_add_mem(conn, source, to_write);

	if ((result = ncp_request(conn, 73)) != 0) {
		ncp_unlock_conn(conn);
		return result;
	}

	*bytes_written = to_write;

	ncp_unlock_conn(conn);
	return 0;
}
	
long
ncp_write(struct ncp_conn *conn, const char *file_id,
	  off_t offset, size_t count, const char *source)
{
	const int bufsize = conn->i.buffer_size;
	int already_written = 0;

	while (already_written < count)
	{
		int written_this_time;
		int to_write = min(bufsize - (offset % bufsize),
				   count - already_written);

		if (ncp_do_write(conn, file_id, offset, to_write,
				source, &written_this_time) != 0)
		{
			return -1;
		}

		offset += written_this_time;
		source += written_this_time;
		already_written += written_this_time;

		if (written_this_time < to_write)
		{
			break;
		}
	}
	return already_written;
}

long
ncp_copy_file(struct ncp_conn *conn,
	      const char source_file[6],
	      const char target_file[6],
	      __u32 source_offset,
	      __u32 target_offset,
	      __u32 count,
	      __u32 *copied_count)
{
	long result;

	ncp_init_request(conn);

	ncp_add_byte(conn, 0);	/* reserved */
	ncp_add_mem(conn, source_file, 6);
	ncp_add_mem(conn, target_file, 6);
	ncp_add_dword(conn, htonl(source_offset));
	ncp_add_dword(conn, htonl(target_offset));
	ncp_add_dword(conn, htonl(count));

	if ((result = ncp_request(conn, 74)) != 0)
	{
		ncp_unlock_conn(conn);
		return result;
	}

	*copied_count = ntohl(ncp_reply_dword(conn, 0));
	ncp_unlock_conn(conn);
	return 0;
}

long
ncp_get_broadcast_message(struct ncp_conn *conn, char message[256])
{
	long result;
	int length;

	ncp_init_request_s(conn, 1);

	if ((result = ncp_request(conn, 21)) != 0)
	{
		ncp_unlock_conn(conn);
		return result;
	}

	length = ncp_reply_byte(conn, 0);
	message[length] = 0;
	memcpy(message, ncp_reply_data(conn, 1), length);
	ncp_unlock_conn(conn);
	return 0;
}

long
ncp_dealloc_dir_handle(struct ncp_conn *conn, __u8 dir_handle)
{
	long result;

	ncp_init_request_s(conn, 20);
	ncp_add_byte(conn, dir_handle);

	result = ncp_request(conn, 22);
	ncp_unlock_conn(conn);
	return result;
}

long
ncp_alloc_short_dir_handle(struct ncp_conn *conn,
			   struct nw_info_struct *dir,
			   word   alloc_mode,
			   byte  *target)
{
	long result;

	ncp_init_request(conn);
	ncp_add_byte(conn, 12);	/* subfunction */
	ncp_add_byte(conn, 0);	/* dos name space */
	ncp_add_byte(conn, 0);	/* reserved */
	ncp_add_word(conn, htons(alloc_mode));
	ncp_add_handle_path(conn, dir->volNumber, dir->DosDirNum,
			    1, NULL);

	if ((result = ncp_request(conn, 87)) != 0)
	{
		ncp_unlock_conn(conn);
		return result;
	}

	*target = ncp_reply_byte(conn, 0);
	ncp_unlock_conn(conn);
	return result;
}
