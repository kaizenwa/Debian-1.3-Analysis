/*
 ********************************************************************************
 *
 * Copyright (c) 1993 Daniel Boulet
 *
 * Redistribution and use in source forms, with and without modification,
 * are permitted provided that this entire comment appears intact.
 *
 * Redistribution in binary form may occur without any restrictions.
 * Obviously, it would be nice if you gave credit where credit is due
 * but requiring it would be too onerous.
 *
 * This software is provided ``AS IS'' without any warranties of any kind.
 *
 ********************************************************************************
 *
 *
 *  Linux port (c) 1994 Bob Beck
 *
 * Redistribution and use in source forms, with and without modification,
 * are permitted provided that this entire comment appears intact.
 *
 * This software is provided ``AS IS'' without any warranties of any kind.
 *
 ********************************************************************************
 *
 *	Drastically cleaned up: Alan Cox <Alan.Cox@linux.org>
 *	More (major) cleanups and bug fixes by Salvador Abreu <spa@fct.unl.pt>
 *	Additional options Lutz Pre"sler <Lutz.Pressler@med-stat.gwdg.de>
 *	Masquerade client support added <Alan.Cox@linux.org>
 *	Made to match 1.1.91, and Urgen's newer ipfw kernel code.
 */

#include <sys/types.h>
#ifdef _LINUX_TYPES_H
/* Yep. it's Linux */
#ifndef LINUX
#define LINUX
#endif
#endif
#include <sys/socket.h>
#include <fcntl.h>
#include <unistd.h>
#define IPFIREWALL
#include <netinet/in.h>
#include <netinet/ip.h>
#include <netinet/tcp.h>
#include <netinet/udp.h>
#ifndef LINUX
#include <netinet/ip_fw.h>
#else
#include <linux/ip_fw.h>	/* Until it gets into stdinc (I've submitted a netinet/ip_fw.h) */
#endif
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#ifdef LINUX
#include <ctype.h>
#define IPVERSION 4
#endif

#ifdef IP_FW_F_MASQ
#define DO_MASQUERADE
#endif
#include "config.h"
#include "net-locale.h"

typedef enum
{
	IPF_BLOCKING = 0,
	IPF_FORWARDING = 1,
	IPF_ACCOUNTING = 2,
	IPF_MASQUERADE = 3
} ipf_kind;

static char *ipf_names[4] = 	{"blocking", "forwarding", "accounting", "maquerading"};
static char *nls_ipf_names[4];
static long ipf_addfunc[4] = 	{IP_FW_ADD_BLK, IP_FW_ADD_FWD, IP_ACCT_ADD, IP_FW_ADD_FWD};
static long ipf_delfunc[4] = 	{IP_FW_DEL_BLK, IP_FW_DEL_FWD, IP_ACCT_DEL, IP_FW_DEL_FWD};
static int lookup = 1;

void ipf_names_init ()
{
	/* no free for this, should be fixed */
	nls_ipf_names[0] = NLS_CATSAVE (catfd, ipfwSet, ipfw_ipf_blocking, "blocking");
	nls_ipf_names[1] = NLS_CATSAVE (catfd, ipfwSet, ipfw_ipf_fwding, "forwarding");
	nls_ipf_names[2] = NLS_CATSAVE (catfd, ipfwSet, ipfw_ipf_accnting, "accounting");
	nls_ipf_names[3] = NLS_CATSAVE (catfd, ipfwSet, ipfw_ipf_msqrading, "masquerading");
}

void show_usage()
{
	fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_usage1, "usage: ipfirewall [-n]\n\t\t  l[ist]  a[ccounting]\n"));
	fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_usage2, "\t\t| l[ist]  b[locking]\n"));
	fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_usage3, "\t\t| l[ist]  f[irewall]\n"));
	fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_usage4, "\t\t| f[lush] a[ccounting]\n"));
	fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_usage5, "\t\t| f[lush] b[locking]\n"));
	fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_usage6, "\t\t| f[lush] f[irewall]\n"));
	fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_usage7, "\t\t| c[heck] b[locking] <type> from <src> to <dst>\n"));
	fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_usage8, "\t\t| c[heck] f[orwarding] <type> from <src> to <dst>\n"));
	fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_usage9, "\t\t| p[olicy] b[locking] <accept|deny|reject>\n"));
	fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_usage10, "\t\t| p[olicy] f[orwarding] <accept|deny|reject>\n"));
	fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_usage11, "\t\t| a[dd]   a[ccounting] <type> [iface <addr>] from <src> to <dst>\n"));
	fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_usage12, "\t\t| a[dd]   b[locking]   <type> [iface <addr>] from <src> to <dst>\n"));
	fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_usage13, "\t\t| a[dd]   f[orwarding] <type> [iface <addr>] from <src> to <dst>\n"));
#ifdef DO_MASQUERADE	
	fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_usage14, "\t\t| a[dd]   m[asquerade] <type> from <src> to <dst>\n"));
#endif	
	fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_usage15, "\t\t| d[el]   a[ccounting] <type> [iface <addr>] from <src> to <dst>\n"));
	fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_usage16, "\t\t| d[el]   b[locking]   <type> [iface <addr>] from <src> to <dst>\n"));
	fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_usage17, "\t\t| d[el]   f[orwarding] <type> [iface <addr>] from <src> to <dst>\n"));
#ifdef DO_MASQUERADE	
	fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_usage18, "\t\t| d[el]   m[asquerade] <type> from <src> to <dst>\n"));
#endif
	fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_usage19, "\t\t| zero[accounting]\n"));
}

/*
 * I'm not sure that this is practical ...
 */

void show_help()
{
	show_usage();
	fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_help1, "where:\n"));
	fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_help2, "       <src> ::= <host> <port> /* for TCP or UDP */\n"));
	fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_help3, "       <src> ::= <host>        /* for ICMP */\n"));
	fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_help4, "      <host> ::= <byte>.<byte>.<byte>.<byte>[/<width>]\n"));
	fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_help5, "               | <hostname>\n"));
	fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_help6, "      <port> ::= <short> | <servicename>\n"));
	fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_help7, "     <short> ::= an integer in the range 1-65535\n"));
	fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_help8, "      <byte> ::= an integer in the range 0-255\n"));
	fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_help9, "     <width> ::= an integer in the range 0-32\n"));
	NLS_CATCLOSE(catfd)
	exit(1);
}

static char *fmtip(u_long uaddr)
{
	static char tbuf[100];

	sprintf(tbuf, "%d.%d.%d.%d",
	    ((char *) &uaddr)[0] & 0xff,
	    ((char *) &uaddr)[1] & 0xff,
	    ((char *) &uaddr)[2] & 0xff,
	    ((char *) &uaddr)[3] & 0xff);

	return (&tbuf[0]);
}

static void print_ports(int cnt, int range, u_short * ports)
{
	int ix;
	char *pad;

	if (range)
	{
		if (cnt < 2)
		{
			fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_range_set,
			    "ipfw: range flag set but only %d ports\n"), cnt);
			NLS_CATCLOSE(catfd)
			exit(1);
		}
		printf("%d:%d", ports[0], ports[1]);
		ix = 2;
		pad = " ";
	}
	else
	{
		ix = 0;
		pad = "";
	}

	while (ix < cnt)
	{
		printf("%s%d", pad, ports[ix]);
		pad = " ";
		ix += 1;
	}
}

static int do_setsockopt(char *what, int fd, int proto, int cmd, void *data, int datalen, int ok_errno)
{
	char *cmdname;

#define CASE(NAME) case IP_##NAME: cmdname = "IP_" #NAME; break

	switch (cmd)
	{
		CASE(FW_FLUSH_BLK);
		CASE(FW_FLUSH_FWD);
		CASE(FW_CHK_BLK);
		CASE(FW_CHK_FWD);
		CASE(FW_ADD_BLK);
		CASE(FW_ADD_FWD);
		CASE(FW_DEL_BLK);
		CASE(FW_DEL_FWD);
		CASE(FW_POLICY_FWD);
		CASE(FW_POLICY_BLK);
		CASE(ACCT_ADD);
		CASE(ACCT_DEL);
		CASE(ACCT_FLUSH);
		CASE(ACCT_ZERO);
	default:
		fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_unkn_cmd, "ipfw: "
		    "unknown command (%d) passed to do_setsockopt - bye!\n"), cmd);
		NLS_CATCLOSE(catfd)
		exit(1);
	}

#undef CASE

	if (fd < 0)
	{
		printf("setsockopt(%d, %d, %s, 0x%x, 0x%x)\n",
		    fd, proto, cmdname, (int) data, datalen);
		if (cmd == IP_FW_CHK_BLK || cmd == IP_FW_CHK_FWD)
		{
			struct iphdr *ip = (struct iphdr *) data;
			struct tcphdr *tcp = (struct tcphdr *) &(((int *) ip)[ip->ihl]);
			if (ip->ihl != sizeof(struct iphdr) / sizeof(int))
			{
				fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_ip, "ip header length %d, should be %d\n"),
				    ip->ihl, sizeof(struct iphdr) / sizeof(int));
			}
			if (ip->protocol != IPPROTO_TCP && ip->protocol != IPPROTO_UDP)
			  {
			    NLS_CATCLOSE(catfd)
			      exit(1);
			  }
			printf(NLS_CATGETS(catfd, ipfwSet, ipfw_data_ip, "data = struct iphdr : struct %shdr {\n"),
			    ip->protocol == IPPROTO_TCP ? "tcp" : "udp");
			printf("\tsrc=%s ", fmtip(ip->saddr));
			printf("%d\n", ntohs(tcp->th_sport));
			printf("\tdst=%s  ", fmtip(ip->daddr));
			printf("%d\n", ntohs(tcp->th_dport));
			printf("}\n");
		}
		else if (cmd == IP_FW_ADD_BLK ||
			    cmd == IP_FW_ADD_FWD ||
		    cmd == IP_ACCT_ADD)
		{
			struct ip_fw *fp = (struct ip_fw *) data;
			int fmt_ports = 0;
			printf(NLS_CATGETS(catfd, ipfwSet, ipfw_data_ipfw, "data = struct ip_fw {\n"));
			if (fp->fw_flg & IP_FW_F_ACCEPT)
			{
				printf(NLS_CATGETS(catfd, ipfwSet, ipfw_accept, "\taccept "));
			}
			else
			{
				printf(NLS_CATGETS(catfd, ipfwSet, ipfw_deny, "\tdeny "));
			}
			switch (fp->fw_flg & IP_FW_F_KIND)
			{
			case IP_FW_F_ALL:
				printf(NLS_CATGETS(catfd, ipfwSet, ipfw_univ, "\tuniversal\n"));
				fmt_ports = 0;
				break;
			case IP_FW_F_TCP:
				printf("tcp\n");
				fmt_ports = 1;
				break;
			case IP_FW_F_UDP:
				printf("udp\n");
				fmt_ports = 1;
				break;
			case IP_FW_F_ICMP:
				printf("icmp\n");
				fmt_ports = 0;
				break;
			}
			printf("\tsrc=%s:", fmtip(fp->fw_src.s_addr));
			printf("%s ", fmtip(fp->fw_smsk.s_addr));
			if (fmt_ports)
			{
				print_ports(fp->fw_nsp, fp->fw_flg & IP_FW_F_SRNG, &fp->fw_pts[0]);
			}
			else if (fp->fw_flg & (IP_FW_F_SRNG | IP_FW_F_DRNG))
			{
			  NLS_CATCLOSE(catfd)
				exit(1);
			}
			else if (fp->fw_nsp > 0 || fp->fw_ndp > 0)
			{
			    NLS_CATCLOSE(catfd)
				exit(1);
			}
			printf("\n");
			printf("\tdst=%s:", fmtip(fp->fw_dst.s_addr));
			printf("%s ", fmtip(fp->fw_dmsk.s_addr));
			if (fmt_ports)
			{
				print_ports(fp->fw_ndp,
				    fp->fw_flg & IP_FW_F_DRNG,
				    &fp->fw_pts[fp->fw_nsp]);
			}
			printf("\n");
			printf("}\n");
		}
	}
	else
	{
		if (setsockopt(fd, proto, cmd, data, datalen) < 0)
		{
			char msg[128];

			if (errno == ok_errno)
			{
				return (errno);
			}
			sprintf(msg, "ipfw: setsockopt(%s)", cmdname);
			perror(msg);
			NLS_CATCLOSE(catfd)
			exit(1);
		}
	}
	return (0);
}

void show_parms(char **argv)
{
	while (*argv)
	{
		printf("%s ", *argv++);
	}
}

int get_protocol(char *arg, void (*cmd_usage) (ipf_kind), ipf_kind kind)
{
	if (arg == NULL)
	{
		fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_missing, "ipfw: missing protocol name\n"));
	}
	else if (strcmp(arg, "tcp") == 0)
	{
		return (IP_FW_F_TCP);
	}
	else if (strcmp(arg, "udp") == 0)
	{
		return (IP_FW_F_UDP);
	}
	else if (strcmp(arg, "icmp") == 0)
	{
		return (IP_FW_F_ICMP);
	}
	else if (strcmp(arg, "all") == 0)
	{
		return (IP_FW_F_ALL);
	}
	else
	{
		fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_illegal, "illegal protocol name \"%s\"\n"), arg);
	}
	(*cmd_usage) (kind);
	NLS_CATCLOSE(catfd)
	exit(1);
	return (0);
}

void get_ipaddr(char *arg, struct in_addr *addr, struct in_addr *mask, void (*usage) (ipf_kind), ipf_kind kind)
{
	char *p, *tbuf;
	int period_cnt, non_digit;
	struct hostent *hptr;

	if (arg == NULL)
	{
		fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_missing_ip, "ipfw: missing ip address\n"));
		NLS_CATCLOSE(catfd)
		exit(1);
	}
	period_cnt = 0;
	non_digit = 0;
	for (p = arg; *p != '\0' && *p != '/' && *p != ':'; p += 1)
	{
		if (*p == '.')
		{
			if (p > arg && *(p - 1) == '.')
			{
				fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_periods,
							    "ipfw: two periods in a row in ip address (%s)\n"), arg);
				NLS_CATCLOSE(catfd)
				exit(1);
			}
			period_cnt += 1;
		}
		else if (!isdigit(*p))
		{
			non_digit = 1;
		}
	}

	tbuf = malloc(p - arg + 1);
	strncpy(tbuf, arg, p - arg);
	tbuf[p - arg] = '\0';

	if (non_digit)
	{
		hptr = gethostbyname(tbuf);
		if (hptr == NULL)
		{
			fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_unkn_host, "ipfw: unknown host \"%s\"\n"), tbuf);
			NLS_CATCLOSE(catfd)
			exit(1);
		}
		if (hptr->h_length != sizeof(struct in_addr))
		{
			fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_addr_length,
			    "ipfw: hostentry addr length = %d, expected %d"
			    "(i.e. sizeof(struct in_addr))\n"),
			    hptr->h_length, sizeof(struct in_addr));
			NLS_CATCLOSE(catfd)
			exit(1);
		}
		bcopy(hptr->h_addr, addr, sizeof(struct in_addr));
	}
	else
	{
		if (period_cnt == 3)
		{

			int a1, a2, a3, a4, matched;

			if ((matched = sscanf(tbuf, "%d.%d.%d.%d", &a1, &a2, &a3, &a4))
			    != 4)
			{
				fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_matched,
				    "ipfw: Only %d fields matched in IP address!\n"),
				    matched);
				/* should this exit here? or catch it later? -BB */
			}
			if (a1 > 255 || a2 > 255 || a3 > 255 || a4 > 255)
			{
				fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_too_large,
							    "ipfw: number too large in ip address (%s)\n"), arg);
				NLS_CATCLOSE(catfd)
				exit(1);
			}
			((char *) addr)[0] = a1;
			((char *) addr)[1] = a2;
			((char *) addr)[2] = a3;
			((char *) addr)[3] = a4;

		}
		else if (strcmp(tbuf, "0") == 0)
		{

			((char *) addr)[0] = 0;
			((char *) addr)[1] = 0;
			((char *) addr)[2] = 0;
			((char *) addr)[3] = 0;

		}
		else
		{

			fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_inc_format,
						    "ipfw: incorrect ip address format \"%s\" (expected 3 periods)\n"), tbuf);
			NLS_CATCLOSE(catfd)
			exit(1);
		}
	}

	free(tbuf);

	if (mask == NULL)
	{
		if (*p != '\0')
		{
			fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_not_allowed,
						    "ipfw: ip netmask not allowed here (%s)\n"), (char *) addr);
			NLS_CATCLOSE(catfd)
			exit(1);
		}
	}
	else
	{
		if (*p == ':')
		{
			get_ipaddr(p + 1, mask, NULL, usage, kind);
		}
		else if (*p == '/')
		{
			int bits;
			char *end;

			p += 1;
			if (*p == '\0')
			{
				fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_missing_mask,
							    "ipfw: missing mask value (%s)\n"), arg);
				NLS_CATCLOSE(catfd)
				exit(1);
			}
			else if (!isdigit(*p))
			{
				fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_non_num,
							    "ipfw: non-numeric mask value (%s)\n"), arg);
				NLS_CATCLOSE(catfd)
				exit(1);
			}
			bits = strtol(p, &end, 10);
			if (*end != '\0')
			{
				fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_junk_mask,
							    "ipfw: junk after mask (%s)\n"), arg);
				NLS_CATCLOSE(catfd)
				exit(1);
			}
			if (bits < 0 || bits > sizeof(u_long) * 8)
			{
				fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_out_range,
							    "ipfw: mask length value out of range (%s)\n"), arg);
				NLS_CATCLOSE(catfd)
				exit(1);
			}
			if (bits == 0)
			{	/* left shifts of 32 aren't defined */
				mask->s_addr = 0;
			}
			else
			{
				((char *) mask)[0] = (-1 << (32 - bits)) >> 24;
				((char *) mask)[1] = (-1 << (32 - bits)) >> 16;
				((char *) mask)[2] = (-1 << (32 - bits)) >> 8;
				((char *) mask)[3] = (-1 << (32 - bits)) >> 0;
			}

		}
		else if (*p == '\0')
		{
			mask->s_addr = 0xffffffff;
		}
		else
		{
			fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_junk_ip,
						    "ipfw: junk after ip address (%s)\n"), arg);
			NLS_CATCLOSE(catfd)
			exit(1);
		}

		/*
         * Mask off any bits in the address that are zero in the mask.
         * This allows the user to describe a network by specifying
         * any host on the network masked with the network's netmask.
         */
		addr->s_addr &= mask->s_addr;

	}

}

u_short get_one_port(char *arg, void (*usage) (ipf_kind), ipf_kind kind, const char *proto_name)
{
	int slen = strlen(arg);

	if (slen > 0 && strspn(arg, "0123456789") == slen)
	{
		int port;
		char *end;

		port = strtol(arg, &end, 10);
		if (*end != '\0')
		{
			fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_illegal_port,
						    "ipfw: illegal port number (%s)\n"), arg);
			NLS_CATCLOSE(catfd)
			exit(1);
		}
		if (port <= 0 || port > 65535)
		{
			fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_portnum_out,
						    "ipfw: port number out of range (%d)\n"), port);
			NLS_CATCLOSE(catfd)
			exit(1);
		}
		return (port);
	}
	else
	{
		struct servent *sptr;

		sptr = getservbyname(arg, proto_name);

		if (sptr == NULL)
		{
			fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_unkn_service,
						    "ipfw: unknown %s service \"%s\"\n"), proto_name, arg);
			NLS_CATCLOSE(catfd)
			exit(1);
		}
		return (ntohs(sptr->s_port));
	}
}

int get_ports(char ***argv_ptr, u_short * ports, int min_ports, int max_ports, void (*usage) (ipf_kind), ipf_kind kind, const char *proto_name)
{
	int ix;
	char *arg;
	int sign;

	ix = 0;
	sign = 1;
	while ((arg = **argv_ptr) != NULL &&
	    strcmp(arg, "from") != 0 &&
	    strcmp(arg, "to") != 0)
	{
		char *p;

		/*
         * Check that we havn't found too many port numbers.
         * We do this here instead of with another condition on the while loop
         * so that the caller can assume that the next parameter is NOT a
         * port number.
         */

		if (ix >= max_ports)
		{
			fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_too_port,
						    "ipfw: too many port numbers "
						    "(max %d, got at least %d, next parm=\"%s\")\n"),
				max_ports, max_ports + 1, arg);
			NLS_CATCLOSE(catfd)
			exit(1);
		}
		if ((p = strchr(arg, ':')) == NULL)
		{
			ports[ix++] = get_one_port(arg, usage, kind, proto_name);
		}
		else
		{
			if (ix > 0)
			{
				fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_port_ranges,
							    "ipfw: "
							    "port ranges are only allowed for "
							    "the first port value pair (%s)\n"), arg);
				NLS_CATCLOSE(catfd)
				exit(1);
			}
			if (max_ports > 1)
			{
				char *tbuf;

				tbuf = malloc((p - arg) + 1);
				strncpy(tbuf, arg, p - arg);
				tbuf[p - arg] = '\0';

				ports[ix++] = get_one_port(tbuf, usage, kind, proto_name);
				ports[ix++] = get_one_port(p + 1, usage, kind, proto_name);
				sign = -1;
			}
			else
			{
				fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_no_range,
				    "ipfw: port range not allowed here (%s)\n"), arg);
				NLS_CATCLOSE(catfd)
				exit(1);
			}
		}

		*argv_ptr += 1;
	}

	if (ix < min_ports)
	{
		if (min_ports == 1)
		{
			fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_missing_port,
						    "ipfw: missing port number%s\n"),
			    max_ports == 1 ? "" : "(s)");
		}
		else
		{
			fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_nomore_port,
			    "ipfw: not enough port numbers (expected %d, got %d)\n"),
			    min_ports, ix);
		}
		NLS_CATCLOSE(catfd)
		exit(1);
	}
	return (sign * ix);
}

void check_usage(ipf_kind kind)
{
	fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_usage20,
				    "usage: ipfirewall check %s ...\n"), ipf_names[kind]);
}

void check(ipf_kind kind, int socket_fd, char **argv)
{
	char tbuff[64];
	int protocol;
	struct iphdr *packet;
	char *proto_name;

	if (kind == IPF_BLOCKING)
	  NLS_CATBUFF (catfd, ipfwSet, ipfw_check_blocking, "blocking", tbuff, 64);
	else
	  NLS_CATBUFF (catfd, ipfwSet, ipfw_check_forwarding, "forwarding", tbuff, 64);
	
	packet = (struct iphdr *) malloc
	    (sizeof(struct iphdr) + sizeof(struct tcphdr));
	packet->version = IPVERSION;
	packet->ihl = sizeof(struct iphdr) / sizeof(int);
	printf(NLS_CATGETS(catfd, ipfwSet, ipfw_check, "check %s "), tbuff);

	show_parms(argv);
	printf("\n");

	proto_name = *argv++;
	protocol = get_protocol(proto_name, check_usage, kind);
	switch (protocol)
	{
	case IP_FW_F_TCP:
		packet->protocol = IPPROTO_TCP;
		break;
	case IP_FW_F_UDP:
		packet->protocol = IPPROTO_UDP;
		break;
	default:
		fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_only_check,
					    "ipfw: can only check TCP or UDP packets\n"));
		break;
	}

	if (*argv == NULL)
	{
		fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_missing_from,"ipfw: missing \"from\" keyword\n"));
		NLS_CATCLOSE(catfd)
		exit(1);
	}
	if (strcmp(*argv, "from") == 0)
	{
		argv += 1;
		get_ipaddr(*argv++, (struct in_addr *) &packet->saddr,
		    NULL, check_usage, kind);
		if (protocol == IP_FW_F_TCP || protocol == IP_FW_F_UDP)
		{
			get_ports(&argv, &((struct tcphdr *) (&packet[1]))->th_sport,
			    1, 1, check_usage, kind, proto_name);
			((struct tcphdr *) (&packet[1]))->th_sport = htons(
			    ((struct tcphdr *) (&packet[1]))->th_sport);
		}
	}
	else
	{
		fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_expect_from,
		    "ipfw: expected \"from\" keyword, got \"%s\"\n"), *argv);
		NLS_CATCLOSE(catfd)
		exit(1);
	}

	if (*argv == NULL)
	{
		fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_missing_to, "ipfw: missing \"to\" keyword\n"));
		NLS_CATCLOSE(catfd)
		exit(1);
	}
	if (strcmp(*argv, "to") == 0)
	{
		argv += 1;
		get_ipaddr(*argv++, (struct in_addr *) &packet->daddr,
		    NULL, check_usage, kind);
		if (protocol == IP_FW_F_TCP || protocol == IP_FW_F_UDP)
		{
			get_ports(&argv, &((struct tcphdr *) (&packet[1]))->th_dport,
			    1, 1, check_usage, kind, proto_name);
			((struct tcphdr *) (&packet[1]))->th_dport = htons(
			    ((struct tcphdr *) (&packet[1]))->th_dport);
		}
	}
	else
	{
		fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_expect_to,
		    "ipfw: expected \"to\" keyword, got \"%s\"\n"), *argv);
		NLS_CATCLOSE(catfd)
		exit(1);
	}

	if (*argv == NULL)
	{
		if (do_setsockopt(kind == IPF_BLOCKING ? "checkblocking" : "checkforwarding",
			socket_fd, IPPROTO_IP,
			kind == IPF_BLOCKING ? IP_FW_CHK_BLK : IP_FW_CHK_FWD,
			packet,
			sizeof(struct iphdr) + sizeof(struct tcphdr),
			EACCES) == 0)
		{
			printf(NLS_CATGETS(catfd, ipfwSet, ipfw_paq_accept, "packet accepted by %s firewall\n"),
			    tbuff);
		}
		else
		{
			printf(NLS_CATGETS(catfd, ipfwSet, ipfw_paq_reject, "packet rejected by %s firewall\n"),
			    tbuff);
		}

		return;
	}
	else
	{
		fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_extra,
					    "ipfw: extra parameters at end of command ("));
		show_parms(argv);
		fprintf(stderr, ")\n");
		NLS_CATCLOSE(catfd)
		exit(1);
	}
}

void add_usage(ipf_kind kind)
{
	fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_usage21, "usage: ipfirewall add %s ...\n"),
		ipf_names[kind]);
}

void add(ipf_kind kind, int socket_fd, char **argv)
{
	int protocol, accept_firewall, src_range, dst_range;
	struct ip_fw firewall;
	char *proto_name;

	printf(NLS_CATGETS(catfd, ipfwSet, ipfw_add, "add %s "), nls_ipf_names[kind]);
	show_parms(argv);
	printf("\n");

	if (kind != IPF_ACCOUNTING && kind != IPF_MASQUERADE)
	{
		if (*argv == NULL)
		{
			fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_missing_acc,
						    "ipfw: missing \"accept\" or \"deny\" keyword\n"));
			NLS_CATCLOSE(catfd)
			exit(1);
		}
		if (strcmp(*argv, "deny") == 0)
		{
			accept_firewall = 0;
		}
		else if (strcmp(*argv, "accept") == 0)
		{
			accept_firewall = IP_FW_F_ACCEPT;
		}
		else if (strcmp(*argv, "reject") == 0)
			accept_firewall = IP_FW_F_ICMPRPL;
		else
		{
			fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_expect_acc,
			    "ipfw: expected \"accept\", \"deny\" or \"reject\", got \"%s\"\n"),
			    *argv);
			NLS_CATCLOSE(catfd)
			exit(1);
		}

		argv += 1;
	}
	else
		accept_firewall = IP_FW_F_ACCEPT;
	if(*argv==NULL)
	{
		fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_missing_proto, "ipfw: missing protocol name.\n"));
		NLS_CATCLOSE(catfd)
		exit(1);
	}
	proto_name = *argv++;
	protocol = get_protocol(proto_name, add_usage, kind);
	
	firewall.fw_via.s_addr = 0;
	
	if(*argv && strcmp(*argv,"iface")==0)
	{
		argv++;
		if(*argv==NULL)
		{
			fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_missing_iface,
						    "ipfw: missing interface address.\n"));
			NLS_CATCLOSE(catfd)
			exit(1);
		}
		firewall.fw_via.s_addr=inet_addr(*argv);
		if(firewall.fw_via.s_addr==-1)
		{
			fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_invalid_iface,
						    "Invalid interface address.\n"));
			NLS_CATCLOSE(catfd)
			exit(1);
		}
		argv++;
	}
	
	if (*argv == NULL)
	{
		fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_missing_from2,
					    "ipfw: missing \"from\" keyword\n"));
		NLS_CATCLOSE(catfd)
		exit(1);
	}
	if (strcmp(*argv, "from") == 0)
	{
		argv++;
		get_ipaddr(*argv++, &firewall.fw_src, &firewall.fw_smsk, add_usage, kind);
		if (protocol == IP_FW_F_TCP || protocol == IP_FW_F_UDP)
		{
			int cnt;
			cnt = get_ports(&argv, &firewall.fw_pts[0], 0, IP_FW_MAX_PORTS,
			    add_usage, kind, proto_name);
			if (cnt < 0)
			{
				src_range = IP_FW_F_SRNG;
				cnt = -cnt;
			}
			else
			{
				src_range = 0;
			}
			firewall.fw_nsp = cnt;
		}
		else
		{
			firewall.fw_nsp = 0;
			src_range = 0;
		}
	}
	else
	{
		fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_expect_from2,
					    "ipfw: expected \"from\", got \"%s\"\n"), *argv);
		NLS_CATCLOSE(catfd)
		exit(1);
	}

	if (*argv == NULL)
	{
		fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_missing_to2, "ipfw: missing \"to\" keyword\n"));
		NLS_CATCLOSE(catfd)
		exit(1);
	}
	if (strcmp(*argv, "to") == 0)
	{
		argv++;
		get_ipaddr(*argv++, &firewall.fw_dst, &firewall.fw_dmsk, add_usage, kind);
		if (protocol == IP_FW_F_TCP || protocol == IP_FW_F_UDP)
		{
			int cnt;
			cnt = get_ports(&argv, &firewall.fw_pts[firewall.fw_nsp], 0,
			    IP_FW_MAX_PORTS - firewall.fw_nsp,
			    add_usage, kind, proto_name);
			if (cnt < 0)
			{
				dst_range = IP_FW_F_DRNG;
				cnt = -cnt;
			}
			else
			{
				dst_range = 0;
			}
			firewall.fw_ndp = cnt;
		}
		else
		{
			firewall.fw_ndp = 0;
			dst_range = 0;
		}
	}
	else
	{
		fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_expect_to2,
					    "ipfw: expected \"to\", got \"%s\"\n"), *argv);
		NLS_CATCLOSE(catfd)
		exit(1);
	}

	if (*argv == NULL)
	{
		firewall.fw_flg = protocol | accept_firewall | src_range | dst_range;
#ifdef DO_MASQUERADE		
		if (kind == IPF_MASQUERADE)
			firewall.fw_flg |= IP_FW_F_MASQ;
#endif
		(void) do_setsockopt(ipf_names[kind],
		    socket_fd, IPPROTO_IP,
		    ipf_addfunc[kind],
		    &firewall,
		    sizeof(firewall),
		    0);
	}
	else
	{
		fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_extra2,
					    "ipfw: extra parameters at end of command ("));
		show_parms(argv);
		fprintf(stderr, ")\n");
		NLS_CATCLOSE(catfd)
		exit(1);
	}
}

void del_usage(ipf_kind kind)
{
	fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_usage22,
				    "usage: ipfirewall delete %s ...\n"), ipf_names[kind]);
}

void del(ipf_kind kind, int socket_fd, char **argv)
{
	int protocol, accept_firewall, src_range, dst_range;
	struct ip_fw firewall;
	char *proto_name;

	printf(NLS_CATGETS(catfd, ipfwSet, ipfw_delete, "delete %s "), nls_ipf_names[kind]);
	show_parms(argv);
	printf("\n");

	if (kind != IPF_ACCOUNTING && kind != IPF_MASQUERADE)
	{
		if (*argv == NULL)
		{
			fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_missing_acc2,
						    "ipfw: missing \"accept\" or \"deny\" keyword\n"));
			NLS_CATCLOSE(catfd)
			exit(1);
		}
		if (strcmp(*argv, "deny") == 0)
		{
			accept_firewall = 0;
		}
		else if (strcmp(*argv, "accept") == 0)
		{
			accept_firewall = IP_FW_F_ACCEPT;
		}
		else if (strcmp(*argv, "reject") == 0)
			accept_firewall = IP_FW_F_ICMPRPL;
		else
		{
			fprintf(stderr, NLS_CATGETS (catfd, ipfwSet, ipfw_expect_acc2,
			    "ipfw: expected \"accept\" or \"deny\", got \"%s\"\n"),
			    *argv);
			NLS_CATCLOSE(catfd)
			exit(1);
		}

		argv += 1;
	}
	else
		accept_firewall = IP_FW_F_ACCEPT;
	if(*argv==NULL)
	{
		fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_missing_proto2,
					    "ipfw: missing protocol name.\n"));
		NLS_CATCLOSE(catfd)
		exit(1);
	}
		
	proto_name = *argv++;
	protocol = get_protocol(proto_name, del_usage, kind);

	firewall.fw_via.s_addr = 0;
	
	if(*argv && strcmp(*argv,"iface")==0)
	{
		argv++;
		if(*argv==NULL)
		{
			fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_missing_iface2,
						    "ipfw: missing interface address.\n"));
			NLS_CATCLOSE(catfd)
			exit(1);
		}
		firewall.fw_via.s_addr=inet_addr(*argv);
		if(firewall.fw_via.s_addr==-1)
		{
			fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_invalid_iface2,
						    "Invalid interface address.\n"));
			NLS_CATCLOSE(catfd)
			exit(1);
		}
		argv++;
	}

	if (*argv == NULL)
	{
		fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_missing_from3,
					    "ipfw: missing \"from\" keyword\n"));
		NLS_CATCLOSE(catfd)
		exit(1);
	}
	if (strcmp(*argv, "from") == 0)
	{
		argv++;
		get_ipaddr(*argv++, &firewall.fw_src, &firewall.fw_smsk, del_usage, kind);
		if (protocol == IP_FW_F_TCP || protocol == IP_FW_F_UDP)
		{
			int cnt;
			cnt = get_ports(&argv, &firewall.fw_pts[0], 0, IP_FW_MAX_PORTS,
			    del_usage, kind, proto_name);
			if (cnt < 0)
			{
				src_range = IP_FW_F_SRNG;
				cnt = -cnt;
			}
			else
			{
				src_range = 0;
			}
			firewall.fw_nsp = cnt;
		}
		else
		{
			firewall.fw_nsp = 0;
			src_range = 0;
		}
	}
	else
	{
		fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_expect_from3,
					    "ipfw: expected \"from\", got \"%s\"\n"), *argv);
		NLS_CATCLOSE(catfd)
		exit(1);
	}

	if (*argv == NULL)
	{
		fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_missing_to3, "ipfw: missing \"to\" keyword\n"));
		NLS_CATCLOSE(catfd)
		exit(1);
	}
	if (strcmp(*argv, "to") == 0)
	{
		argv++;
		get_ipaddr(*argv++, &firewall.fw_dst, &firewall.fw_dmsk, del_usage, kind);
		if (protocol == IP_FW_F_TCP || protocol == IP_FW_F_UDP)
		{
			int cnt;
			cnt = get_ports(&argv, &firewall.fw_pts[firewall.fw_nsp], 0,
			    IP_FW_MAX_PORTS - firewall.fw_nsp,
			    del_usage, kind, proto_name);
			if (cnt < 0)
			{
				dst_range = IP_FW_F_DRNG;
				cnt = -cnt;
			}
			else
			{
				dst_range = 0;
			}
			firewall.fw_ndp = cnt;
		}
		else
		{
			firewall.fw_ndp = 0;
			dst_range = 0;
		}
	}
	else
	{
		fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_expect_to3,
					    "ipfw: expected \"to\", got \"%s\"\n"), *argv);
		NLS_CATCLOSE(catfd)
		exit(1);
	}

	if (*argv == NULL)
	{
		firewall.fw_flg = protocol | accept_firewall | src_range | dst_range;
#ifdef DO_MASQUERADE
		if (kind == IPF_MASQUERADE)
			firewall.fw_flg |= IP_FW_F_MASQ;
#endif			
		(void) do_setsockopt(ipf_names[kind],
		    socket_fd, IPPROTO_IP,
		    ipf_delfunc[kind],
		    &firewall,
		    sizeof(firewall),
		    0);
	}
	else
	{
		fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_extra3,
					    "ipfw: extra parameters at end of command ("));
		show_parms(argv);
		fprintf(stderr, ")\n");
		NLS_CATCLOSE(catfd)
		exit(1);
	}
}

static int count_mask(unsigned long mask)
{
	int ct;
	for (ct = 0; (mask & 0x80000000); ct++)
		mask <<= 1;
	return ct;
}

typedef struct
{
	int acct;
	unsigned long sa, da, sm, dm, iface;
	unsigned int nsp, ndp;
	unsigned long npkt, nbyt;
	unsigned int fw_pts[10];
	int fw_flg;
} fw_rec;

#define MIN(a,b) ((a)<(b)? (a): (b))
#define SRC(x)   ((x)->sa & (x)->sm)
#define DST(x)   ((x)->da & (x)->dm)

static int int_order(const void *L, const void *R)
{
	return (*(unsigned int *) R - *(unsigned int *) L);
}

static int list_order(const void *L, const void *R)
{
	register const fw_rec *l = L;
	register const fw_rec *r = R;

	register int result =
	((r->fw_flg & IP_FW_F_KIND) - (l->fw_flg & IP_FW_F_KIND)) ? :
	(MIN(SRC(r), DST(r)) - MIN(SRC(l), DST(l)));

	if (result == 0 && (l->fw_flg & (IP_FW_F_TCP | IP_FW_F_UDP)))
	{
		unsigned int nlp, lp[10], nrp, rp[10];
		unsigned int i;

		bzero(lp, 10 * sizeof(unsigned int));
		bzero(rp, 10 * sizeof(unsigned int));

		bcopy(l->fw_pts, lp, (nlp = l->nsp + l->ndp) * sizeof(unsigned int));
		bcopy(r->fw_pts, rp, (nrp = r->nsp + r->ndp) * sizeof(unsigned int));

		qsort(lp, nlp, sizeof(unsigned int), int_order);
		qsort(rp, nrp, sizeof(unsigned int), int_order);

		for (i = 0; i < 10; ++i)
			if (lp[i] != rp[i])
				return (lp[i] - rp[i]);
	}
	return result;
}

static char *addr_to_name(unsigned int a, unsigned int m)
{
	static char tbuf[128];
	struct hostent *hptr;
	struct netent *nptr;

	if (m == 0)
		NLS_CATBUFF (catfd, ipfwSet, ipfw_anywhere, "anywhere", tbuf, 128);
	else
	{
		int mask_len = count_mask(m);
		struct in_addr ia =
		{htonl(a)};

		if (lookup && (hptr = gethostbyaddr((char *) &ia, sizeof ia, AF_INET)))
			strcpy(tbuf, hptr->h_name);
		else if (lookup && (nptr = getnetbyaddr(a, AF_INET)))
			sprintf(tbuf, "=%s", nptr->n_name);
		else
			sprintf(tbuf, "%s/%d", fmtip(htonl(a)), mask_len);
	}

	return tbuf;
}

void list_file(char *path, int acct)
{
	FILE *f = fopen(path, "r");
	int nrecs = 8;
	int nused = 0;
	fw_rec *recs = (void *) malloc(sizeof(fw_rec) * nrecs);
	fw_rec *rec;
	char buf[256];
	struct servent *sptr;

	if (f == NULL)
	{
		perror(path);
		NLS_CATCLOSE(catfd)
		exit(1);
	}
	fgets(buf, 255, f);	/* skip title */
	while (fgets(buf, 255, f))
	{			/* read in the data */
		if (nused >= nrecs)
		{
			nrecs <<= 1;
			recs = (void *) realloc(recs, sizeof(fw_rec) * nrecs);
		}
		rec = &recs[nused++];

		rec->acct = acct;
		sscanf(buf,
		    "%lX/%lX->%lX/%lX %lX %X %u %u %lu %lu %u %u %u %u %u %u %u %u %u %u",
		    &rec->sa, &rec->sm, &rec->da, &rec->dm, &rec->iface,
		    &rec->fw_flg, &rec->nsp, &rec->ndp, &rec->npkt, &rec->nbyt,
		    &rec->fw_pts[0], &rec->fw_pts[1], &rec->fw_pts[2], &rec->fw_pts[3],
		    &rec->fw_pts[4], &rec->fw_pts[5], &rec->fw_pts[6], &rec->fw_pts[7],
		    &rec->fw_pts[8], &rec->fw_pts[9]);
	}
	fclose(f);

	qsort(recs, nused, sizeof(fw_rec), list_order);

	if (acct)
		printf(NLS_CATGETS(catfd, ipfwSet, ipfw_bytes, "Packets\t Bytes\t"));
	else
		printf(NLS_CATGETS(catfd, ipfwSet, ipfw_type, "Type\t"));
	{
	  char *proto, *from, *to;
	  proto = NLS_CATSAVE (catfd, ipfwSet, ipfw_proto, "Proto %19.19s %19.19s    Ports\n");
	  printf(proto,
		 (from=NLS_CATSAVE (catfd, ipfwSet, ipfw_print_from, "From        ")),
		 (to=  NLS_CATSAVE (catfd, ipfwSet, ipfw_print_to,   "To         ")));
	  free (proto);
	  free (from);
	  free (to);
	}
	for (rec = recs; nused-- > 0; ++rec)
	{
		unsigned int *pp = &rec->fw_pts[0];

		if (!rec->acct)
		{
#ifdef DO_MASQUERADE		
			if (rec->fw_flg & IP_FW_F_MASQ)
				printf(NLS_CATGETS(catfd, ipfwSet, ipfw_masquerade, "(masquerade"));
			else
#endif
			{
				if (rec->fw_flg & IP_FW_F_ACCEPT)
					printf(NLS_CATGETS(catfd, ipfwSet, ipfw_list_accept, "accept\t"));
				else
					printf(NLS_CATGETS(catfd, ipfwSet, ipfw_list_deny, "deny\t"));
			}
		}
		else
		{
			printf("%7lu\t", rec->npkt);
			if (rec->nbyt > 100 * 1024)
			{
				unsigned long kbyt = (rec->nbyt + 1023) / 1024;
				if (kbyt > 100 * 1024)
				{
					unsigned long mbyt = (kbyt + 1023) / 1024;
					printf("%6luM\t", mbyt);
				}
				else
					printf("%6luK\t", kbyt);
			}
			else
				printf("%6lu \t", rec->nbyt);
		}

		switch (rec->fw_flg & IP_FW_F_KIND)
		{
		case IP_FW_F_ALL:
			printf(NLS_CATGETS(catfd, ipfwSet, ipfw_list_all, "all   "));
			break;
		case IP_FW_F_TCP:
			printf("tcp   ");
			break;
		case IP_FW_F_UDP:
			printf("udp   ");
			break;
		case IP_FW_F_ICMP:
			printf("icmp  ");
			break;
		}

		printf("%-19.19s ", addr_to_name(rec->sa, rec->sm));
		printf("%-19.19s ", addr_to_name(rec->da, rec->dm));

		if (rec->fw_flg & (IP_FW_F_TCP | IP_FW_F_UDP))
		{
			char *sep = "";
			char *proto = (rec->fw_flg & IP_FW_F_TCP) ? "tcp" : "udp";

			if (rec->nsp == 0)
				printf(NLS_CATGETS(catfd, ipfwSet, ipfw_list_any, "any"));
			else
			{
				if (rec->fw_flg & IP_FW_F_SRNG)
				{
					printf("%u-%u", pp[0], pp[1]);
					sep = ",";
					pp += 2;
					rec->nsp -= 2;
				}
				while (rec->nsp-- > 0)
				{
					sptr = getservbyport(htons(*pp), proto);
					if (sptr)
						printf("%s%s", sep, sptr->s_name);
					else
						printf("%s%u", sep, *pp);
					++pp;
					sep = ",";
				}
			}

			printf(" -> ");

			sep = "";
			if (rec->ndp == 0)
				printf(NLS_CATGETS(catfd, ipfwSet, ipfw_list_any, "any"));
			else
			{
				if (rec->fw_flg & IP_FW_F_DRNG)
				{
					printf("%u-%u", pp[0], pp[1]);
					sep = ",";
					pp += 2;
				}
				while (rec->ndp-- > 0)
				{
					sptr = getservbyport(htons(*pp), proto);
					if (sptr)
						printf("%s%s", sep, sptr->s_name);
					else
						printf("%s%u", sep, *pp);
					++pp;
					sep = ",";
				}
			}
		}
		printf("\n");
	}
	endservent();
}

void list(int socket_fd, char **argv)
{
	if (*argv == NULL || !**argv)
	{
		fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_expect_kwds,
					    "blocking, forwarding or accounting keyword expected.\n"));
		NLS_CATCLOSE(catfd)
		exit(1);
	}
	if (strncmp(*argv, "blocking", strlen(*argv)) == 0)
	{
		list_file("/proc/net/ip_block", 0);
		return;
	}
	if (strncmp(*argv, "forwarding", strlen(*argv)) == 0)
	{
		list_file("/proc/net/ip_forward", 0);
		return;
	}
	if (strncmp(*argv, "accounting", strlen(*argv)) == 0)
	{
		list_file("/proc/net/ip_acct", 1);
		return;
	}
	fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_found_kwds,
				    "Found '%s': 'blocking', 'forwarding' or 'accounting' keyword expected.\n"), *argv);
	NLS_CATCLOSE(catfd)
	exit(1);
}

#define MATCH(in,pat) ( in && in[0] && (strncmp(in, pat, strlen(in)) == 0) )

void main(argc, argv)
int argc;
char **argv;
{
	int socket_fd;
	char *type = NULL;

#if NLS
	setlocale (LC_MESSAGES, "");
	catfd = catopen ("nettools", MCLoadBySet);
#endif
	ipf_names_init();
	socket_fd = socket(AF_INET, SOCK_RAW, IPPROTO_RAW);

	if (socket_fd < 0)
	{
		perror(NLS_CATGETS(catfd, ipfwSet, ipfw_raw_socket, "ipfw: raw socket creation"));
		NLS_CATCLOSE(catfd)
		exit(1);
	}

	/* -d disables actions and debugs */
	
	if (argv[1] && strcmp(argv[1], "-d")==0)	
	{
		argv++;
		argc--;
		close(socket_fd);
		socket_fd = -1;
	}
	
	/* -n disables nameserver lookups */

	if (argv[1] && strcmp(argv[1], "-n")==0)
	{
		argc--;
		argv++;
		lookup = 0;
	}
	if (argc == 1)
	{
		show_usage();
		NLS_CATCLOSE(catfd)
		exit(1);
	}
	if (MATCH(argv[1], "list"))
	{
		type = "list";
		list(socket_fd, &argv[2]);
	}
	else if(MATCH(argv[1], "policy"))
	{
		int type;
		int mode;
		if(MATCH(argv[2],"blocking"))
			type=IP_FW_POLICY_BLK;
		else if(MATCH(argv[2], "forwarding"))
			type=IP_FW_POLICY_FWD;
		else
		{
			fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_expect_main_blocking,
						    "ipfw: expected \"blocking\" or \"forwarding\".\n"));
			NLS_CATCLOSE(catfd)
			exit(1);
		}
		if(MATCH(argv[3], "reject"))
			mode=IP_FW_F_ICMPRPL;
		else if(MATCH(argv[3], "accept"))
			mode=IP_FW_F_ACCEPT;
		else if(MATCH(argv[3], "deny"))
			mode=0;
		else
		{	
			fprintf(stderr,NLS_CATGETS(catfd, ipfwSet, ipfw_expect_main_accept,
						   "ipfw: expected \"accept\", \"deny\" or \"reject\".\n"));
			NLS_CATCLOSE(catfd)
			exit(1);
		}
		do_setsockopt(argv[2], socket_fd, IPPROTO_IP, type, &mode, sizeof(mode),0);
		NLS_CATCLOSE(catfd)
		exit(0);
	}
	else if (MATCH(argv[1], "flush"))
	{
		if (MATCH(argv[2], "accounting"))
		{
			/* Same kludge as above, see above ranting and griping -BB */
			unsigned long fred = 1;
			(void) do_setsockopt(argv[1], socket_fd, IPPROTO_IP, IP_ACCT_FLUSH, &fred, sizeof(unsigned long), 0);

		}
		else if (MATCH(argv[2], "firewall"))
		{
			/* Same kludge as above, see above ranting and griping -BB */
			unsigned long fred = 1;
			(void) do_setsockopt(argv[1], socket_fd, IPPROTO_IP, IP_FW_FLUSH_FWD, &fred, sizeof(unsigned long), 0);
		}
		else if (MATCH(argv[2], "blocking"))
		{
			/* Same kludge as above, see above ranting and griping -BB */
			unsigned long fred = 1;
			(void) do_setsockopt(argv[1], socket_fd, IPPROTO_IP, IP_FW_FLUSH_BLK, &fred, sizeof(unsigned long), 0);
		}
		else
		{
			fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_expect_main_accounting,
			    "ipfw: expected \"accounting\", \"blocking\" or \"firewall\".\n"));
			NLS_CATCLOSE(catfd)
			exit(1);


		}
	}
	else if (MATCH(argv[1], "check"))
	{
		if (MATCH(argv[2], "blocking"))
			check(IPF_BLOCKING, socket_fd, &argv[3]);

		else if (MATCH(argv[2], "forwarding"))
			check(IPF_FORWARDING, socket_fd, &argv[3]);

		else
		{
			fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_illegal_check,
						    "ipfw: illegal `check' keyword: %s\n"),
				argv[2]);
			show_usage();
			NLS_CATCLOSE(catfd)
			exit(1);
		}
	}
	else
	{
		int is_add = MATCH(argv[1], "add");
		int is_del = MATCH(argv[1], "delete");
		char *op = NULL;

		if (is_add)
		{
			type = "add";
			if (MATCH(argv[2], "blocking"))
				add(IPF_BLOCKING, socket_fd, &argv[3]);
			else if (MATCH(argv[2], "forwarding"))
				add(IPF_FORWARDING, socket_fd, &argv[3]);
			else if (MATCH(argv[2], "accounting"))
				add(IPF_ACCOUNTING, socket_fd, &argv[3]);
#ifdef DO_MASQUERADE				
			else if (MATCH(argv[2], "masquerade"))
				add(IPF_MASQUERADE, socket_fd, &argv[3]);
#endif
			else
				op = argv[2] ? : NLS_CATSAVE (catfd, ipfwSet, ipfw_main_missing, "(missing)");
		}
		else if (is_del)
		{
			type = "del";
			if (MATCH(argv[2], "blocking"))
				del(IPF_BLOCKING, socket_fd, &argv[3]);
			else if (MATCH(argv[2], "forwarding"))
				del(IPF_FORWARDING, socket_fd, &argv[3]);
			else if (MATCH(argv[2], "accounting"))
				del(IPF_ACCOUNTING, socket_fd, &argv[3]);
#ifdef DO_MASQUERADE
			else if (MATCH(argv[2], "masquerade"))
				del(IPF_MASQUERADE, socket_fd, &argv[3]);
#endif		
			else
				op = argv[2] ? : NLS_CATSAVE (catfd, ipfwSet, ipfw_main_missing, "(missing)");
		}
		else if (MATCH(argv[1], "zeroaccounting"))
		{
			unsigned long fred = 1;
			type = "zero";
			/* Same kludge as above, see above ranting and griping -BB */
			(void) do_setsockopt(argv[1], socket_fd, IPPROTO_IP, IP_ACCT_ZERO, &fred, sizeof(unsigned long), 0);

		}
		if (!type)
		{
			fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_unkn_cmd,
						    "ipfw: unknown command `%s'\n\n"), argv[1]);
			show_help();
			NLS_CATCLOSE(catfd)
			exit(1);
		}
		else if (op)
		{
			fprintf(stderr, NLS_CATGETS(catfd, ipfwSet, ipfw_unkn_kwd,
						    "ipfw: unknown `%s' keyword: `%s'\n"),
			    type, op);
			show_usage();
			NLS_CATCLOSE(catfd)
			exit(1);
		}
	}

	NLS_CATCLOSE(catfd)
	exit(0);
}
