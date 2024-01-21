/*
 * Extend a tcl command interpreter with a dns command to query 
 * the Internet domain name service.
 *
 * Copyright (c) 1993, 1994, 1995
 *
 * E. Schoenfelder, J. Schoenwaelder
 * TU Braunschweig, Germany
 * Institute for Operating Systems and Computer Networks
 *
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that this copyright
 * notice appears in all copies.  The University of Braunschweig
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 *
 */

/*
 *	defaults are:
 *		retry-timeout: 2 secs
 *		retries: 2
 *		server: libresolv.a default
 *		query: a (for hostname to ip lookup)
 *
 * 	bugs:  (*) only one query flag is used.
 *	       (*) other default servers are asked too (feature ?).
 *
 */


#include "scotty.h"

#include <arpa/nameser.h>
#include <resolv.h>


/* debug switch: */
static int do_debug = 0;
#define dprintf		if (do_debug) printf

/*
 * max # of return hostnames/ip-addrs:
 */

#define MAXRESULT	10

/*
 * Query and reply structure.
 */

typedef struct {
    HEADER qb1;
    char qb2[PACKETSZ];
} querybuf;

/*
 * Selfmade reply structure (private use only).
 */

typedef struct {
    int type;			/* T_A, T_SOA, T_HINFO, T_MX */
    int n;			/* # of results stored */
    union {
	struct in_addr addr [MAXRESULT]; 
	char str [MAXRESULT][256];
    } u;
} a_res;

/*
 * Forward declarations for procedures defined later in this file:
 */

static int
DnsIsNumAddr		_ANSI_ARGS_((char *s));

static void
DnsInit			_ANSI_ARGS_((int timeout, int retries,
				     struct in_addr *server));
static int
DnsConvertPtr		_ANSI_ARGS_((Tcl_Interp *interp, char *hname));

static void
DnsDoQuery		_ANSI_ARGS_((char *query_string, int query_type, 
				     a_res *query_result));
static void
DnsHaveQuery		_ANSI_ARGS_((char *query_string, int query_type,
				     a_res *query_result, int depth));
static int 
DnsA			_ANSI_ARGS_((Tcl_Interp *interp, char *hname));

static int
DnsPtr			_ANSI_ARGS_((Tcl_Interp *interp, char *ip));

static void
DnsCleanHinfo		_ANSI_ARGS_((char *str));

static int
DnsHinfo		_ANSI_ARGS_((Tcl_Interp *interp, char *hname));

static int 
DnsMx			_ANSI_ARGS_((Tcl_Interp *interp, char *hname));

static int 
DnsSoa			_ANSI_ARGS_((Tcl_Interp *interp, char *hname));


/*
 * DnsIsNumAddr() checks if s contains a numerical address in dotted
 * format. It returns 1 if the test was succesful, or 0 otherwise.
 */

static int
DnsIsNumAddr (s)
     char *s;
{
    int i;

    for (i = 0; i < 4; i++) {
        if (! isdigit (*s)) {
            return 0;
	}
	while (*s && isdigit (*s)) s++;
	if ((i == 3) && (*s == '\0')) {
	    return 1;
	}
	if ((*s == '\0') || (*s != '.')) {
            return 0;
        }
	s++;
    }

    return 0;
}


/*
 * Initialize the resolver. Set default retries/timeout and get the
 * default server from the resolver library.
 */

static void
DnsInit (timeout, retries, server)
     int timeout;
     int retries;
     struct in_addr *server;
{
    static int initialized = 0;

    if (! initialized) {
	res_init ();
	_res.options |= RES_RECURSE | RES_DNSRCH | RES_DEFNAMES | RES_AAONLY;
	initialized = 1;
	if (_res.nscount > 0) {
	    *server = _res.nsaddr.sin_addr;
	}
    }

    _res.retrans = timeout;
    _res.retry = retries + 1;
    _res.nsaddr.sin_addr = *server;
}


/*
 * try to make ptr from hname (assumed to be numerical).
 */

static int
DnsConvertPtr (interp, hname)
     Tcl_Interp *interp;
     char *hname;
{
    int rc;
    
    rc = DnsPtr (interp, hname);
    if (rc != TCL_OK) {
        Tcl_AppendResult (interp, "cannot reverse lookup ", 
			  hname, (char *) NULL);
	return TCL_ERROR;
    }
    
    return TCL_OK;
}


/*
 * do a single query; mainly called by DnsHaveQuery.
 */

static void
DnsDoQuery (query_string, query_type, query_result)
     char *query_string;
     int query_type;
     a_res *query_result;
{
	querybuf query, answer;
	char buf [512], lbuf [512], auth_buf [512];
	int err, i, qlen, alen, llen, nscount, len, nsents;
	short type, class, rdlen;
	long ttl;
	querybuf *q;
	u_char *ptr;
	char *eom;

	dprintf ("*** DnsDoQuery() called:  query_string: `%s'  type: %d\n",
		query_string, query_type);

	query_result->type = -1;
	query_result->n = 0;

	/* if err is set, query_result->u.str [0] contains an 
	 * error-description: 
	 */
	err = 0;

	for (i = 0; i < sizeof (querybuf); i++)
		((char *) &query) [i] = ((char *) &answer) [i] = 0;

 /**
  ** res_mkquery(op, dname, class, type, data, datalen, newrr, buf, buflen) 
  **/
	
	qlen = res_mkquery (QUERY, query_string, C_IN, query_type, 
			    (char *) 0, 0, 0,
			    (char *) &query, sizeof(query));
	dprintf ("** res_mkquery() = %d\n", qlen);

	if (qlen <= 0)
	{
		query_result->n = -1;
		strcpy (query_result->u.str [0], "cannot make query");
		return;
	}

 /**
  ** res_send(msg, msglen, answer, anslen)
  **/	

	alen = res_send ((char *) &query, qlen, 
			 (char *) &answer, sizeof (answer));

	dprintf ("** res_send() = %d\n", alen);

	if (alen <= 0) {
	    query_result->n = -1;
	    strcpy (query_result->u.str [0], "cannot send query");
	    return;
	}

	q = &answer;

	/* if there are nameserver entries, only these are for authorative
	   answers of interest: */
	nsents = ntohs (q->qb1.nscount);

dprintf ("** got: 	qents: %d  aents: %d,  nsents: %d  rrents: %d\n",
	 ntohs (q->qb1.qdcount), ntohs (q->qb1.ancount), 
	 nsents, ntohs (q->qb1.arcount));

dprintf ("** got rcode: %d   is response: %d  opcode: %d\n", q->qb1.rcode,
	 q->qb1.qr, q->qb1.opcode);

	if (q->qb1.rcode != 0)
	{
		err = 1;
		if (q->qb1.rcode == 1)
			strcpy (query_result->u.str [0], "format error");
		else if (q->qb1.rcode == 2)
			strcpy (query_result->u.str [0], "server failure");
		else if (q->qb1.rcode == 3)
			strcpy (query_result->u.str [0], "domainname error");
		else if (q->qb1.rcode == 4)
			strcpy (query_result->u.str [0], "not implemented");
		else if (q->qb1.rcode == 5)
			strcpy (query_result->u.str [0], "query refused");
		else
			strcpy (query_result->u.str [0], "unknown error");
		dprintf ("* got error: %s\n", query_result->u.str [0]);

		query_result->type = query_type;
		query_result->n = -1;
		return;
	}

	nscount = ntohs (q->qb1.ancount);
	if (! nscount)
		nscount = ntohs (q->qb1.nscount);
	if (! nscount)
		nscount = ntohs (q->qb1.arcount);

	/* give some help (seems to be needed for very ole sun-code... */
	eom = (char *) &answer + alen;
	*eom++ = 0;
	
	ptr = (u_char *) q->qb2;

	/*
	 * skip over question section: 
	 * [ QNAME , QTYPE , QCLASS ]
	 */
	if (q->qb1.qdcount > 0)
	{
		int rc = dn_skipname (ptr, eom);
		dprintf ("** dn_skipname () = %d\n", rc);
		ptr += rc + QFIXEDSZ;
	}

	/*
	 * now we have left a section with:
	 *	Answering RR's
	 *	Authority RR's
	 *	Additional RR's
	 */

	for ( ; nscount; nscount--) 
	{

		/*
		 * every RR looks like:
		 * [ NAME , TYPE , CLASS , TTL , RDLANGTH , RDATA ]
		 */

		/**
		 ** dn_expand(msg, msglen, comp_dn, exp_dn, length)
		 **/

		llen = dn_expand ((char *) q, eom, ptr, lbuf, sizeof (lbuf));

		dprintf ("** dn_expand() = %d\n", llen);

		if (llen < 0)
		{
			/* XXX: what to do ? */
			return;
		}

		if (llen > 0)
			dprintf ("** got `%s'\n", lbuf);

		ptr += llen;

		/* fetch TYPE, CLASS, TTL: */
		GETSHORT (type, ptr);
		GETSHORT (class, ptr);
		GETLONG (ttl, ptr);
		/* fetch RDLENGTH: */
		GETSHORT (rdlen, ptr);

		dprintf ("** type: %d  class: %d  ttl: %ld  rdlen: %d\n", 
			 type, class, ttl, (int) rdlen);

		if (type == T_NS)
		{
			dprintf ("** type T_NS\n");

			len = dn_expand ((char *) q, eom, ptr, buf,
					 sizeof (buf));
			dprintf ("** ns: dn_expand() = %d\n", len);
			if (len < 0)
				return;
			if (len > 0)
				dprintf ("** ns: name got `%s'\n", buf);
			ptr += len;
		}
		else if (type == T_A) 
		{
			unsigned long x;

			dprintf ("** type T_A\n");
			GETLONG (x, ptr);
			dprintf ("** addr: 0x%08lx\n", x);
			
			/* save this address: */
			if (! strcmp (query_string, lbuf)
			    || query_result->type == T_A 
			    || query_result->type == -1)
			{
				query_result->type = T_A;
				query_result->u.addr [query_result->n++].s_addr
					= ntohl (x);
			}
		}
		else if (type == T_SOA) 
		{
			/*
			 * SOA rdata format is:
			 * [ MNAME , RNAME , SERIAL , REFRESH , RETRY
			 *   EXPIRE , MINIMUM ]
			 */
			dprintf ("** type T_SOA\n");

			/* fetch authorative site-name: */
			len = dn_expand ((char *) q, eom, ptr, auth_buf,
					 sizeof (auth_buf));
			dprintf ("** soa: dn_expand() = %d\n", len);
			if (len < 0)
				return;
			if (len > 0)
				dprintf ("** soa: name got `%s'\n", auth_buf);
			ptr += len;
			
			/* fetch/skip over maintainer-name: */
			len = dn_expand ((char *) q, eom, ptr, buf, 
					 sizeof (buf));
			dprintf ("** soa: dn_expand() = %d\n", len);
			if (len < 0)
				return;
			if (len > 0)
				dprintf ("** soa: mname got `%s'\n", buf);
			ptr += len;
			
			/* skip til end of this rr: */
			ptr += 5 * 4;

			if (query_result->type == T_SOA
			    || query_result->type == -1)
			{
				query_result->type = T_SOA;
				strcpy (query_result->u.str[query_result->n++],
					auth_buf);
			}
		}
		else if (type == T_HINFO)
		{
			int i;

			for (i = 0; i < 1; i++)		/* XXX: ??? */
			{
			len = dn_expand ((char *) q, eom, ptr, buf,
					 sizeof (buf));
			dprintf ("** hinfo: dn_expand() = %d\n", len);
			if (len < 0)
				return;
			if (len > 0)
				dprintf ("** hinfo: got `%s'\n", buf);

			ptr += rdlen;

                        if (query_result->type == T_HINFO
                            || query_result->type == -1)
                        {
                                query_result->type = T_HINFO;
                                strcpy (query_result->u.str[query_result->n++],
                                        buf);
			}
			}
		}
		else if (type == T_PTR)
		{
			len = dn_expand ((char *) q, eom, ptr, buf,
					 sizeof (buf));
			dprintf ("** ptr: dn_expand() = %d\n", len);
			if (len < 0)
				return;
			if (len > 0)
				dprintf ("** ptr: got `%s'\n", buf);

			ptr += rdlen;

                        if (query_result->type == T_PTR
                            || query_result->type == -1)
                        {
                                query_result->type = T_PTR;
                                strcpy (query_result->u.str[query_result->n++],
                                        buf);
			}
		}
		else if (type == T_MX)
		{
			unsigned prio;
			GETSHORT (prio, ptr);

			len = dn_expand ((char *) q, eom, ptr, buf,
					 sizeof (buf));
			dprintf ("** mx: dn_expand() = %d\n", len);
			if (len < 0)
				return;
			if (len > 0)
				dprintf ("** mx: got `%s'\n", buf);

			ptr += len;

                        if (query_result->type == T_MX
                            || query_result->type == -1)
                        {
                                query_result->type = T_MX;
                                sprintf (
				query_result->u.str [query_result->n++],
					 "%s %d", buf, prio);
			}
		}
		else {
			dprintf ("** unhandled type: %d\n", type);

			ptr += rdlen;
		}
	}
}


/*
 * query_result will contain the result.
 * if query_result->n < 0, the first string contains the error.
 *
 * (depth was experimental, please ignore...)
 */

static void
DnsHaveQuery (query_string, query_type, query_result, depth)
     char *query_string;
     int query_type;
     a_res *query_result;
     int depth;
{
    int i;
    a_res res;
    char tmp [256];
    
    dprintf (
	     "*** DnsHaveQuery():  string: `%s'  type: %d  server: 0x%08lx\n",
	     query_string, query_type, (long) _res.nsaddr.sin_addr.s_addr);
    
    query_result->type = -1;
    query_result->n = 0;
    
    if (depth > 1) {
        dprintf ("** DnsHaveQuery(): too deep - aborted.\n");
	return;
    }

    /*
     * loop through every domain suffix: 
     */

    for (i = -1; i < MAXDNSRCH + 1; i++) {
        if (i == -1)
	  strcpy (tmp, query_string);
	else if (! _res.dnsrch [i])
	  break;
	else
	  sprintf (tmp, "%s.%s", query_string, _res.dnsrch [i]);
	
	dprintf ("** trying: `%s'...\n", tmp);
	DnsDoQuery (tmp, query_type, &res);
	if (res.type == query_type && res.n > 0)
	  {
	    *query_result = res;
	    return;
	  }
	/* check ptr and soa's not recursive: */
	if (query_type == T_SOA || query_type == T_PTR)
	  {
	    *query_result = res;
	    return;
	  }
      }
    
    /*
     * seems to be unsuccessful: look for any answer:
     */
    for (i = -1; i < MAXDNSRCH + 1; i++)
      {
	if (i == -1)
	  strcpy (tmp, query_string);
	else if (! _res.dnsrch [i])
	  break;
	else
	  sprintf (tmp, "%s.%s", query_string, _res.dnsrch [i]);
	
	dprintf ("** trying: `%s'...\n", tmp);
	DnsDoQuery (tmp, query_type, &res);
	if (res.n > 0)
	  {
#if 1
	    /* return first answer found: */
	    *query_result = res;
	    return;
#else
	    query_string = tmp;
	    break;
#endif
	  }
      }
    
    if (res.n <= 0)
      {
	dprintf ("*** rabaeh: cannot find any answer.\n");
	*query_result = res;
	return;
      }
    
#if 1
    dprintf ("*** rabaeh: will return anyway.\n");
    return;
#else
    /*
     * here we could ask recursive other hosts ...
     * (but still wrong and not used)
     */
    
    if (res.type == T_SOA) {
      a_res tmpres;
      DnsDoQuery (res.u.str [0], T_A, &tmpres);
      if (tmpres.type != T_A || tmpres.n <= 0)
	{
	  dprintf ("** DnsHaveQuery(): no T_A for T_SOA\n");
	  return;
	}
      _res.nsaddr.sin_addr = tmpres.u.addr [0];
      _res.nscount = 1;
      DnsHaveQuery (query_string, query_type, query_result, 
		    depth + 1);
      return;
    }
    else 
      dprintf ("** DnsHaveQuery(): type is %d ???\n", res.type);
#endif
}


static int 
DnsA (interp, hname)
     Tcl_Interp *interp;
     char *hname;
{
    a_res res;
    int i;

    if (DnsIsNumAddr (hname)) {
        if (DnsPtr (interp, hname) == TCL_OK) {
	    Tcl_SetResult (interp, hname, TCL_VOLATILE);
	    return TCL_OK;
	} else {
	    return TCL_ERROR;
	}
    }
  
    DnsHaveQuery (hname, T_A, &res, 0);
    if (res.n < 0) {
        Tcl_SetResult (interp, res.u.str [0], TCL_VOLATILE);
        return TCL_ERROR;
    }
  
    if (res.type != T_A) {
        return TCL_OK;
    }
  
    for (i = 0; i < res.n; i++) {
	Tcl_AppendElement (interp, inet_ntoa (res.u.addr [i]));
    }

    return TCL_OK;
}


static int
DnsPtr (interp, ip)
     Tcl_Interp *interp;
     char *ip;
{
    a_res res;
    int i, a, b, c, d;
    char tmp [128];

    if (DnsIsNumAddr (ip)) {
        if (4 != sscanf (ip, "%d.%d.%d.%d", &a, &b, &c, &d)) {
	    Tcl_SetResult (interp, "invalid IP address", TCL_STATIC);
	    return TCL_ERROR;
	}
	sprintf (tmp, "%d.%d.%d.%d.in-addr.arpa", d, c, b, a);
    } else {
#if 0
        if (DnsA (interp, ip) == TCL_OK) {
	    Tcl_SetResult (interp, ip, TCL_VOLATILE);
	    return TCL_OK;
	} else {
	    return TCL_ERROR;
	}
#else
	Tcl_SetResult (interp, "not a valid ip address", TCL_STATIC);
	return TCL_ERROR;
#endif 
    }

    DnsHaveQuery (tmp, T_PTR, &res, 0);
    if (res.n < 0) {
        Tcl_SetResult (interp, res.u.str [0], TCL_VOLATILE);
        return TCL_ERROR;
    }

    if (res.type != T_PTR) {
        return TCL_OK;
    }

    for (i = 0; i < res.n; i++) {
        Tcl_AppendElement (interp, res.u.str[i]);
    }

    return TCL_OK;
}


/*
 * remove '\' from the given string:
 */

static void
DnsCleanHinfo (str)
     char *str;
{
     char *ptr;

     while (str && *str) {
         if (*str == '\\') {
	     for (ptr = str; *ptr; ptr++)
	       *ptr = *(ptr + 1);
	 }
	 str++;
     }
}


static int
DnsHinfo (interp, hname)
     Tcl_Interp *interp;
     char *hname;
{
    a_res res;
    char *beg, *ptr;
    char *hinfo;

    /*
     * be a friend: if we get a numerical address, convert to ptr first.
     */

    if (DnsIsNumAddr (hname)) {

	if (DnsConvertPtr (interp, hname) != TCL_OK) {
	    return TCL_ERROR;
	}
	hname = interp->result;
    }

    DnsHaveQuery (hname, T_HINFO, &res, 0);

    Tcl_ResetResult (interp);

    if (res.n < 0) {
        Tcl_SetResult (interp, res.u.str [0], TCL_VOLATILE);
	return TCL_ERROR;
    }

    if (res.type != T_HINFO) {
        return TCL_OK;
    }

    beg = ptr = res.u.str [0];
    while (*ptr && *ptr != '.') {
        if (*ptr == '\\' && *(ptr+1)) ptr++;
	ptr++;
    }
  
    if (*ptr == '.') *ptr++ = ' ';
    
    while (*ptr && *ptr != '.') {
        if (*ptr == '\\' && *(ptr+1))  ptr++;
	ptr++;
    }
  
    hinfo = ckalloc (ptr - beg + 1);
    strncpy (hinfo, beg, ptr - beg);
    hinfo[ptr - beg] = 0;
    DnsCleanHinfo (hinfo);
    Tcl_SetResult (interp, hinfo, TCL_DYNAMIC);
    
    return TCL_OK;
}


static int 
DnsMx (interp, hname)
     Tcl_Interp *interp;
     char *hname;
{
    a_res res;
    int i;

    /*
     * be a friend: if we get a numerical address, convert to ptr first.
     */

    if (DnsIsNumAddr (hname)) {

	if (DnsConvertPtr (interp, hname) != TCL_OK) {
	    return TCL_ERROR;
	}
	hname = interp->result;
    }

    DnsHaveQuery (hname, T_MX, &res, 0);

    Tcl_ResetResult (interp);

    if (res.n < 0) {
        Tcl_SetResult (interp, res.u.str [0], TCL_VOLATILE);
	return TCL_ERROR;
    }

    if (res.type != T_MX) {
        return TCL_OK;
    }

    for (i = 0; i < res.n; i++) {
        Tcl_AppendElement (interp, res.u.str[i]);
    }

    return TCL_OK;
}


static int 
DnsSoa (interp, hname)
     Tcl_Interp *interp;
     char *hname;
{
    a_res res;
    int i;

    /*
     * be a friend: if we get a numerical address, convert to ptr first. 
     * i guess, this makes absolut no sense for a soa :-)
     */

    if (DnsIsNumAddr (hname)) {
	    
	if (DnsConvertPtr (interp, hname) != TCL_OK) {
	    return TCL_ERROR;
	}	
	hname = interp->result;
    }

    DnsHaveQuery (hname, T_SOA, &res, 0);

    Tcl_ResetResult (interp);

    if (res.n < 0) {
        Tcl_SetResult (interp, res.u.str [0], TCL_VOLATILE);
	return TCL_ERROR;
    }

    if (res.type != T_SOA) {
        return TCL_OK;
    }

    for (i = 0; i < res.n; i++) {
        Tcl_AppendElement (interp, res.u.str[i]);
    }

    return TCL_OK;
}


/*
 * This procedure is invoked to process the "dns" command.
 * See the user documentation for details on what it does.
 */

int
Scotty_DnsCmd (clientData, interp, argc, argv)
     ClientData clientData;
     Tcl_Interp *interp;
     int argc;
     char *argv[];
{
    char *cmd = argv[0];
    int length;

    static int retries = 2;		/* default # of retries */
    static int timeout = 2;		/* default timeout in s */
    static struct in_addr server;	/* default DNS server */

    int actRetries = -1;		/* actually used # of retries */
    int actTimeout = -1;		/* actually used timeout in s */
    struct in_addr actServer;		/* actually used DNS server */
    char *serverName = NULL;		/* name of new DNS server */

    argc--; argv++;

    if (argc == 0) {
      wrongArgs:
        Tcl_AppendResult (interp, "wrong # args: should be \"", cmd, 
			  " ?-timeout t? ?-retries r? ?-server host? ",
			  "option arg\"", (char *) NULL);
	return TCL_ERROR;
    }
    
    /* 
     * parse optional parameters: 
     */

    while ((argc > 0) && (*argv[0] == '-')) {
        length = strlen (argv[0]);
	if (strncmp(argv[0], "-timeout", length) == 0) {
	    argc--, argv++;
	    if (argc <= 0) {
	        sprintf (interp->result, "%d", timeout);
		return TCL_OK;
	    }
	    if (Tcl_GetInt (interp, argv[0], &actTimeout) != TCL_OK) {
	        return TCL_ERROR;
	    }
	    if (actTimeout < 1) {
		Tcl_SetResult (interp, "timeout must be positive", TCL_STATIC);
                return TCL_ERROR;
	    }
	    argc--, argv++;
	} else if (strncmp (argv [0], "-retries", length) == 0) {
	    argc--, argv++;
	    if (argc <= 0) {
	        sprintf (interp->result, "%d", retries);
		return TCL_OK;
	    }
	    if (Tcl_GetInt (interp, argv[0], &actRetries) != TCL_OK) {
	        return TCL_ERROR;
	    }
	    if (actRetries < 0) {
		Tcl_SetResult (interp, "negative retries", TCL_STATIC);
		return TCL_ERROR;
	    }
	    argc--, argv++;
	} else if (strncmp (argv [0], "-server", length) == 0) {
	    struct hostent *he;
	    argc--, argv++;
	    if (argc <= 0) {
	        Tcl_SetResult (interp, inet_ntoa (server), TCL_STATIC);
		return TCL_OK;
	    }
	    serverName = argv[0];
	    if ((he = gethostbyname (serverName))) {
		actServer = * (struct in_addr *) he->h_addr;
	    } else {
		int hostaddr = inet_addr (serverName);
		if (hostaddr == -1) {
		    Tcl_AppendResult (interp, "can not lookup server \"",
				      serverName, "\"", (char *) NULL);
		    return TCL_ERROR;
		}
		memcpy ((char *) &actServer, (char *) &hostaddr, 4);
	    }
	    argc--, argv++;
	} else {
	    Tcl_AppendResult (interp, "unknown option \"", argv [0], "\"", 
			      (char *) NULL);
	    return TCL_ERROR;
	}
    }

    if (argc == 0) {
	if (actRetries >= 0) {
            retries = actRetries;
        }
        if (actTimeout > 0) {
            timeout = actTimeout;
        }
	if (serverName) {
	    server = actServer;
	}
        return TCL_OK;
    }

    if (argc != 2) {
        goto wrongArgs;
    }

    DnsInit (actTimeout < 0 ? timeout : actTimeout, 
	     actRetries <= 0 ? retries : actRetries,
	     serverName  ? &actServer : &server);

    /*
     * Get the query type.
     */

    length = strlen (argv[0]);
    
    if (strncmp (argv [0], "address", length) == 0) {
	return DnsA (interp, argv[1]);
    } else if (strncmp (argv [0], "ptr", length) == 0) {
	return DnsPtr (interp, argv[1]);
    } else if (strncmp (argv [0], "hinfo", length) == 0) {
	return DnsHinfo (interp, argv[1]);
    } else if (strncmp (argv [0], "mx", length) == 0) {
	return DnsMx (interp, argv[1]);
    } else if (strncmp (argv [0], "soa",length) == 0) {
	return DnsSoa (interp, argv[1]);
    }

    Tcl_AppendResult (interp, "bad option \"", argv[0], "\": should be ",
		      "address, ptr, hinfo, mx, or soa",
		      (char *) NULL);
    return TCL_ERROR;
}

/* end of dnsfun.c */
