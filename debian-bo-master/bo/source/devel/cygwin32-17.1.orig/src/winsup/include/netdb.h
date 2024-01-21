
/* Original linux netdb.h merged with winsock.h types */

/*-
 * Copyright (c) 1980, 1983, 1988, 1993
 *     The Regents of the University of California.  All rights reserved.
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
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
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
 *	@(#)netdb.h	8.1 (Berkeley) 6/2/93
 *      netdb.h,v 1.1.1.1 1995/02/18 05:34:07 hjl Exp
 * -
 * Portions Copyright (c) 1993 by Digital Equipment Corporation.
 *
 * Permission to use, copy, modify and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies, and that
 * the name of Digital Equipment Corporation not be used in advertising or
 * publicity pertaining to distribution of the document or software without
 * specific, written prior permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND DIGITAL EQUIPMENT CORP. DISCLAIMS ALL
 * WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS.   IN NO EVENT SHALL DIGITAL EQUIPMENT
 * CORPORATION BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
 * PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 * -
 * --Copyright--
 */

#ifndef _NETDB_H_
#define _NETDB_H_

#ifdef __cplusplus
extern "C" {
#endif 


/*
 * Structures returned by network data base library.  All addresses are
 * supplied in host order, and returned in network order (suitable for
 * use in system calls).
 */

  /* Different from the linux versions - note the shorts.. */
struct	hostent {
	const char	*h_name;	/* official name of host */
	char	**h_aliases;	/* alias list */
	short	h_addrtype;	/* host address type */
	short	h_length;	/* length of address */
	char	**h_addr_list;	/* list of addresses from name server */
#define	h_addr	h_addr_list[0]	/* address, for backward compatiblity */
};

/*
 * Assumption here is that a network number
 * fits in an unsigned long -- probably a poor one.
 */

struct	netent {
	char		*n_name;	/* official name of net */
	char		**n_aliases;	/* alias list */
	short		n_addrtype;	/* net address type */
	unsigned long	n_net;		/* network # */
};

struct	servent {
	char	*s_name;	/* official service name */
	char	**s_aliases;	/* alias list */
	short   s_port;		/* port # */
	char	*s_proto;	/* protocol to use */
};

struct	protoent 
{
  char	*p_name;	/* official protocol name */
  char	**p_aliases;	/* alias list */
  short	p_proto;	/* protocol # */
};

struct rpcent {
	char	*r_name;	/* name of server for this rpc program */
	char	**r_aliases;	/* alias list */
	int	r_number;	/* rpc program number */
};

/*
 * Error return codes from gethostbyname() and gethostbyaddr()
 * (left in extern int h_errno).
 */


extern int h_errno;

#define	NETDB_INTERNAL -1 /* see errno */
#define	NETDB_SUCCESS   0 /* no problem */
#define	HOST_NOT_FOUND	1 /* Authoritative Answer Host not found */
#define	TRY_AGAIN	2 /* Non-Authoritive Host not found, or SERVERFAIL */
#define	NO_RECOVERY	3 /* Non recoverable errors, FORMERR, REFUSED, NOTIMP */
#define	NO_DATA		4 /* Valid name, no data record of requested type */
#define	NO_ADDRESS	NO_DATA		/* no address, look for MX record */

void		cygwin32_endhostent (void);
void		cygwin32_endnetent (void);
void		cygwin32_endprotoent (void);
void		cygwin32_endservent (void);
void		cygwin32_endrpcent  (void);
struct hostent	*cygwin32_gethostbyaddr (const char *, int, int);
struct hostent	*cygwin32_gethostbyname (const char *);
struct hostent	*cygwin32_gethostent (void);
struct netent	*cygwin32_getnetbyaddr (long, int); /* u_long? */
struct netent	*cygwin32_getnetbyname (const char *);
struct netent	*cygwin32_getnetent (void);
struct protoent	*cygwin32_getprotobyname (const char *);
struct protoent	*cygwin32_getprotobynumber (int);
struct protoent	*cygwin32_getprotoent (void);
struct servent	*cygwin32_getservbyname (const char *, const char *);
struct servent	*cygwin32_getservbyport (int, const char *);
struct servent	*cygwin32_getservent (void);
struct rpcent	*cygwin32_getrpcent (void);
struct rpcent	*cygwin32_getrpcbyname (const char *);
struct rpcent	*cygwin32_getrpcbynumber (int);
void		cygwin32_herror (const char *);
void		cygwin32_sethostent (int);
void		cygwin32_setnetent (int);
void		cygwin32_setprotoent (int);
void		cygwin32_setservent (int);
void		cygwin32_setrpcent (int);



#ifndef __INSIDE_CYGWIN_NET__
#define endhostent cygwin32_endhostent
#define endnetent cygwin32_endnetent 
#define endprotoent cygwin32_endprotoent 
#define endservent cygwin32_endservent 
#define endrpcent  cygwin32_endrpcent  
#define gethostbyaddr cygwin32_gethostbyaddr 
#define gethostbyname cygwin32_gethostbyname 
#define gethostent cygwin32_gethostent 
#define getnetbyaddr cygwin32_getnetbyaddr 
#define getnetbyname cygwin32_getnetbyname 
#define getnetent cygwin32_getnetent 
#define getprotobyname cygwin32_getprotobyname 
#define getprotobynumber cygwin32_getprotobynumber 
#define getprotoent cygwin32_getprotoent 
#define getservbyname cygwin32_getservbyname 
#define getservbyport cygwin32_getservbyport 
#define getservent cygwin32_getservent 
#define getrpcent cygwin32_getrpcent 
#define getrpcbyname cygwin32_getrpcbyname 
#define getrpcbynumber cygwin32_getrpcbynumber 
#define herror cygwin32_herror 
#define sethostent cygwin32_sethostent 
#define setnetent cygwin32_setnetent 
#define setprotoent cygwin32_setprotoent 
#define setservent cygwin32_setservent 
#define setrpcent cygwin32_setrpcent 
#endif

typedef void (*sig_t)(int);
#ifdef __cplusplus
};
#endif 

#endif /* !_NETDB_H_ */
