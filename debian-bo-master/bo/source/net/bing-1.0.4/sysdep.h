/*
 * System dependencies
 *
 * sysdep.h,v 1.1 1995/07/17 22:36:44 pb Exp
 *
 * sysdep.h,v
 * Revision 1.1  1995/07/17  22:36:44  pb
 * Linux port.
 *
 */

/*
 * Copyright (c) 1995 Pierre Beyssac.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by Pierre Beyssac,
 *	Mike Muss, the University of California, Berkely and its contributors.
 * 4. Neither the name of the author nor the names of any co-contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY PIERRE BEYSSAC AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 */

#ifndef _SYSDEP_H_
#define _SYSDEP_H_

#ifdef linux
#define icmp		icmphdr
#define ip		iphdr
#define icmp_type	type
#define icmp_code	code
#define icmp_cksum	checksum
#define icmp_id		un.echo.id
#define icmp_seq	un.echo.sequence
#define icmp_gwaddr	un.gateway
#define ip_hl		ihl
#define ip_v		version
#define ip_tos		tos
#define ip_len		tot_len
#define ip_id		id
#define ip_off		frag_off
#define ip_ttl		ttl
#define ip_p		protocol
#define ip_sum		check
#define ip_src		saddr
#define ip_dst		daddr
#ifndef MAX_IPOPTLEN
#define MAX_IPOPTLEN	4096
#endif
#define ICMP_MINLEN	16

#define IPOPT_MINOFF	4

#define IPOPT_EOL	IPOPT_END
#define IPOPT_NOP	IPOPT_NOOP

#define ICMP_UNREACH		ICMP_DEST_UNREACH
#define ICMP_UNREACH_NET	ICMP_NET_UNREACH
#define ICMP_UNREACH_HOST	ICMP_HOST_UNREACH
#define ICMP_UNREACH_PROTOCOL	ICMP_PROT_UNREACH
#define ICMP_UNREACH_PORT	ICMP_PORT_UNREACH
#define ICMP_UNREACH_NEEDFRAG	ICMP_FRAG_NEEDED
#define ICMP_UNREACH_SRCFAIL	ICMP_SR_FAILED
#define ICMP_SOURCEQUENCH	ICMP_SOURCE_QUENCH
#define ICMP_REDIRECT_NET	ICMP_REDIR_NET
#define ICMP_REDIRECT_HOST	ICMP_REDIR_HOST
#define ICMP_REDIRECT_TOSNET	ICMP_REDIR_NETTOS
#define ICMP_REDIRECT_TOSHOST	ICMP_REDIR_HOSTTOS
#define ICMP_TIMXCEED		ICMP_TIME_EXCEEDED
#define ICMP_TIMXCEED_INTRANS	ICMP_EXC_TTL
#define ICMP_TIMXCEED_REASS	ICMP_EXC_FRAGTIME
#define ICMP_PARAMPROB		ICMP_PARAMETERPROB
#define ICMP_TSTAMP		ICMP_TIMESTAMP
#define ICMP_TSTAMPREPLY	ICMP_TIMESTAMPREPLY
#define ICMP_IREQ		ICMP_INFO_REQUEST
#define ICMP_IREQREPLY		ICMP_INFO_REPLY

#define	ICMP_TO_DATA(icp)	((u_char *)(icp + 1))
#endif /* linux */

/* Defaults (should work on most systems) */

#ifndef ICMP_TO_DATA
#ifndef icmp_data
#define	ICMP_TO_DATA(icp)	((u_char *)(&(icp)->icmp_ip))
#else
#define	ICMP_TO_DATA(icp)	((u_char *)((icp)->icmp_data))
#endif
#endif

/* Define the following if you lack sterror() */
/* #define NO_STRERROR 1 */

#endif /* _SYSDEP_H_ */
