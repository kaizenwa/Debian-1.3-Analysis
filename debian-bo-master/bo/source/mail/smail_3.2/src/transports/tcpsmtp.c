/*
#ident	"@(#)smail/src/transports:RELEASE-3_2:tcpsmtp.c,v 1.28 1996/02/28 14:27:48 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 *
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * tcpsmtp.c:
 *      Send mail using the SMTP protocol.
 */

#define NEED_SOCKETS
#include <stdio.h>
#include <signal.h>
#include <ctype.h>
#include <errno.h>
#include <sys/types.h>
#include "defs.h"
#include "../smail.h"
#include "../dys.h"
#include "../addr.h"
#include "../bindsmtpth.h"
#include "../transport.h"
#include "../route.h"
#include "../spool.h"
#include "../exitcodes.h"
#include "../smailconf.h"
#include "../parse.h"
#include "../lookup.h"
#include "tcpsmtp.h"
#include "smtplib.h"
#ifndef DEPEND
# include "../extern.h"
# include "../error.h"
# include "../debug.h"
#endif

/* variables imported from libc */
extern int errno;

/* functions local to this file */
#ifdef HAVE_BIND
static int tcpsmtp_bind();
#endif
static int tcpsmtp_internal();
static char *set_short_timeout();
static char *set_long_timeout();
static char *set_timeout();
static int tcpsmtp_connect();
static struct error *connect_failure();
static int ip_address();

static struct attr_table tcpsmtp_attributes[] = {
    { "short_timeout", t_proc, NULL, (tup *)set_short_timeout, 0 },
    { "long_timeout", t_proc, NULL, (tup *)set_long_timeout, 0 },
    { "service", t_string, NULL, NULL, OFFSET(tcpsmtp_private, service) },
#ifdef HAVE_BIND
    { "use_bind", t_boolean, NULL, NULL, TCPSMTP_USE_BIND },
    BIND_ATTRIBUTES(tcpsmtp_private, bindlib_attr),
#endif
};
static struct attr_table *end_tcpsmtp_attributes = ENDTABLE(tcpsmtp_attributes);


/*
 * tpd_tcpsmtp - transport to remote machine using SMTP on top of TCP/IP.
 */
void
tpd_tcpsmtp(addr, succeed, defer, fail)
    struct addr *addr;                  /* recipient addresses for transport */
    struct addr **succeed;              /* successful deliveries */
    struct addr **defer;                /* defer until a later queue run */
    struct addr **fail;                 /* failed deliveries */
{
#ifdef HAVE_BIND
    struct transport_hints *hints;
    struct transport_hints *mx_hints;
    struct ipaddr_hint *ip_addr;
#endif
    char *service;
    char *error_text;
    struct error *error;                /* error structure */
    struct tcpsmtp_private *priv;
    time_t started;
    int defer_failure, ret;

    priv = (struct tcpsmtp_private *)addr->transport->private;
    service = expand_string(priv->service, addr, (char *)0, (char *)0);
    if (service == NULL) {
        error_text = xprintf("failed to expand service, %s", priv->service);
        insert_addr_list(addr, defer,connect_failure(addr->transport, error_text));
        return;
    }

#ifdef HAVE_BIND

    for (hints = addr->tphint_list;
         hints && !EQ(hints->hint_name,"mx");
         hints = hints->succ)
            ;

    if (!hints && (addr->transport->flags & TCPSMTP_USE_BIND)) {
        if (tcpsmtp_bind(addr, defer, fail) != SUCCEED)
            return;

        for (hints = addr->tphint_list;
             hints && !EQ(hints->hint_name,"mx");
             hints = hints->succ)
                ;
    }

    /* assume the worst */
    defer_failure = FALSE;

    /* but we don't know why the worst, yet */
    error = NULL;

    if (hints) {
        int already_locked = 0;

        for (mx_hints = hints; mx_hints; mx_hints = mx_hints->succ) {
            if (!EQ(mx_hints->hint_name,"mx")) {
                continue;
            }

#define mx_hint ((struct mx_transport_hint *)(mx_hints->private))
            for (ip_addr = mx_hint->ipaddrs;
                 ip_addr;
                 ip_addr = ip_addr->succ)
            {
                if (already_locked ||
                    retry_host_lock(addr->transport, ip_addr->hostname,
                                    &defer_failure, &error) == SUCCEED)
                {
                    time(&started);
                    error = NULL;
                    ret = tcpsmtp_internal(addr, succeed, defer, fail,
                                           ip_addr->hostname, &ip_addr->addr,
                                           AF_INET, service, &error);
                    if (ret != SMTP_AGAIN) {
                        retry_host_unlock(started, (struct error *) NULL);
                        return;
                    } else {
                        /*
                         * if the connect failed because of timeout, or
                         * connection refused, then it should be retried
                         * at a later date...the machine might be down for
                         * a while.
                         */
                        defer_failure = TRUE;
                    }

                    if (ip_addr->succ &&
                           strcmpic(ip_addr->hostname,
                           (ip_addr->succ)->hostname) == 0) {
                        already_locked = 1;
                    } else {
                        retry_host_unlock(started, error);
                        already_locked = 0;
                    }
                }
            }
#undef mx_hint
        }
    }
    else

#endif /* HAVE_BIND */

    {
        struct in_addr ipaddr;
        short family;

        if (ip_address(addr->next_host,&ipaddr,&family,&error_text) < 0) {
            error_text = xprintf("IP address for %s not found",
                                 addr->next_host);
            insert_addr_list(addr, defer,
                             connect_failure(addr->transport, error_text));
            return;
        }

        error = NULL;
        if (retry_host_lock(addr->transport, addr->next_host,
                            &defer_failure, &error) == SUCCEED)
        {
            time(&started);
            ret = tcpsmtp_internal(addr, succeed, defer, fail,
                                   addr->next_host, &ipaddr,
                                   family, service, &error);
            retry_host_unlock(started, error);
            if (ret != SMTP_AGAIN) {
                return;
            } else {
                /*
                 * if the connect failed because of timeout, or
                 * connection refused, then it should be retried
                 * at a later date...the machine might be down for
                 * a while.
                 */
                defer_failure = TRUE;
            }
        }
    }

    if (!defer_failure) {
        if (!error) {
            /* should never happen */
#if 0						/* what did this mean??? */
	    error = connect_failure(ERR_CONFERR|ERR_174, "missing error");
#else
	    error = connect_failure(addr->transport, "missing error");
#endif
        }
        error->info |= ERR_NSOWNER;
    }
    insert_addr_list(addr, defer_failure ? defer : fail, error);
}

#ifdef HAVE_BIND

static int
tcpsmtp_bind(addr, defer, fail)
    struct addr *addr;
    struct addr **defer;
    struct addr **fail;
{
    struct tcpsmtp_private *priv;
    struct rt_info rt_info;
    struct error *error;
    struct addr **notnow;
    struct transport_hints **h;
    char *what;
    int result;

    priv = (struct tcpsmtp_private *)addr->transport->private;

    rt_info.next_host = NULL;
    rt_info.route = NULL;
    rt_info.transport = NULL;
    rt_info.tphint_list = NULL;

    what = xprintf("transport %s", addr->transport->name);
    result = bind_addr(addr->next_host, addr->transport->flags,
                       &priv->bindlib_attr, what, &rt_info, &error);
    xfree(what);
    if (rt_info.next_host) {
        xfree(rt_info.next_host);
    }
    if (rt_info.route) {
        xfree(rt_info.route);
    }

    notnow = NULL;
    switch (result)
    {
      case DB_SUCCEED:
        /* Found a successful match. */
        /* Append hints to address's list. */
        for (h = &addr->tphint_list; *h; h = &(*h)->succ)
            continue;
        *h = rt_info.tphint_list;
        break;

      case DB_NOMATCH:
        /* No match was found. */
        break;

      case DB_FAIL:
        /* The address should be failed, with an error of some kind. */
        notnow = fail;
        break;

      case DB_AGAIN:
        /* Routing for this address should be reattempted later. */
        notnow = defer;
	break;

      case FILE_NOMATCH:
        /* The file was not found, don't match any addresses. */
        break;

      case FILE_FAIL:
        /* Permanent router error, this is a configuration error. */
        error->info |= ERR_CONFERR;
        notnow = fail;
        break;

      case FILE_AGAIN:
        /* Temporary router database error, retry all addresses. */
        notnow = defer;
        break;
    }

    if (notnow) {
        insert_addr_list(addr, notnow, error);
        return FAIL;
    }

    return SUCCEED;
}

#endif  /* HAVE_BIND */

static int
tcpsmtp_internal(addr,succeed,defer,fail,hostname,ipaddr,family,service,ep)
    struct addr *addr;                  /* recipient addresses for transport */
    struct addr **succeed;              /* successful deliveries */
    struct addr **defer;                /* defer until a later queue run */
    struct addr **fail;                 /* failed deliveries */
    char *hostname;             /* name of the host to connect to */
    struct in_addr *ipaddr;     /* IP address of the host to connect to */
    short family;               /* address family */
    char *service;              /* service to use */
    struct error **ep;          /* error structure */
{
    struct transport *tp = addr->transport;
    struct tcpsmtp_private *priv;
    int s;                              /* socket */
    int s2;                             /* dup of s */
    struct smtp smtpbuf;                /* SMTP description buffer */
    char *error_text;
    int success;
    struct addr *ap;
    int try_ehlo;

    priv = (struct tcpsmtp_private *)tp->private;

    DEBUG4(DBG_DRIVER_LO, "transport %s: connect to host %s [%s]/%s...",
           addr->transport->name, hostname, inet_ntoa(*ipaddr), service);

    /*
     * adjust the next_host for the address, so that log entries will
     * reflect the last MX host to be tried.
     */

    for (ap = addr; ap; ap = ap->succ) {
        if (ap->next_host == NULL || !EQIC(ap->next_host, hostname)) {
            if (ap->next_host) {
                xfree(ap->next_host);
            }
            ap->next_host = COPY_STRING(hostname);
        }
    }

    /* reach out and touch someone */

    for (try_ehlo = 1, success = 0; !success && try_ehlo >= 0; --try_ehlo) {
	s = tcpsmtp_connect(hostname, ipaddr, family, service, &error_text);
	if (s >= 0) {
	    s2 = dup(s);
	    if (s2 < 0) {
		(void) close(s);
		s = -1;
	    }
	}
	if (s < 0) {
	    *ep = connect_failure(tp, error_text);
	    return SMTP_AGAIN;
	}

	smtpbuf.in = fdopen(s, "r");
	smtpbuf.out = fdopen(s2, "w");
	smtpbuf.short_timeout = priv->short_timeout;
	smtpbuf.long_timeout = priv->long_timeout;
	smtpbuf.nl = "\r\n";
	tp->flags |= PUT_CRLF;
	smtpbuf.tp = tp;
	smtpbuf.smtp_flags = ESMTP_none;
	smtpbuf.max_size = 0;

	DEBUG(DBG_DRIVER_LO, "connected\n");

	switch (smtp_startup(&smtpbuf, ep, try_ehlo)) {

	  case SMTP_FAIL:
	    insert_addr_list(addr, fail, *ep);
	    (void) fclose(smtpbuf.in);
	    (void) fclose(smtpbuf.out);
	    return SMTP_FAIL;

	  case SMTP_AGAIN:
	    (void) fclose(smtpbuf.in);
	    (void) fclose(smtpbuf.out);
	    return SMTP_AGAIN;

	  case SMTP_EHLO_FAIL:
	    (void) fclose(smtpbuf.in);
	    (void) fclose(smtpbuf.out);
	    break;

	  default:
	    success = 1;
	}
    }

    if (dont_deliver) {
        insert_addr_list(addr, succeed, (struct error *)NULL);
        smtp_shutdown(&smtpbuf);
    } else {
        success = smtp_send(&smtpbuf, addr, succeed, defer, fail, ep);
        if (success == SUCCEED) {
            smtp_shutdown(&smtpbuf);
        }
    }

    /* all done */
    (void) fclose(smtpbuf.in);
    (void) fclose(smtpbuf.out);
    return SUCCEED;
}

/*
 * tpd_smtp - obsolescent name for tpd_tcpsmtp driver
 *
 * This routine exists for backward compatibility with Smail alpha
 * releases prior to version 3.1.12.
 */
void
tpd_smtp(addr, succeed, defer, fail)
    struct addr *addr;                  /* recipient addresses for transport */
    struct addr **succeed;              /* successful deliveries */
    struct addr **defer;                /* defer until a later queue run */
    struct addr **fail;                 /* failed deliveries */
{
    tpd_tcpsmtp(addr, succeed, defer, fail);
}


/*
 * tpb_tcpsmtp - read the configuration file attributes
 */
char *
tpb_tcpsmtp(tp, attrs)
    struct transport *tp;               /* transport entry being defined */
    struct attribute *attrs;            /* list of per-driver attributes */
{
    char *error;
    static struct tcpsmtp_private tcpsmtp_template = {
        5 * 60,                         /* short timeout, 5 minutes */
        2 * 60 * 60,                    /* long timeout, 2 hours */
        "smtp",                         /* use the "smtp" service */
#ifdef HAVE_BIND
        BIND_TEMPLATE_ATTRIBUTES,
#endif
    };
    struct tcpsmtp_private *priv;       /* new tcpsmtp_private structure */

    /* copy the template private data */
    priv = (struct tcpsmtp_private *)xmalloc(sizeof(*priv));
    (void) memcpy((char *)priv, (char *)&tcpsmtp_template, sizeof(*priv));

    tp->private = (char *)priv;
    /* fill in the attributes of the private data */
    error = fill_attributes((char *)priv,
                            attrs,
                            &tp->flags,
                            tcpsmtp_attributes,
                            end_tcpsmtp_attributes);

    if (error) {
        return error;
    }
    return NULL;
}



/*
 * tpp_tcpsmtp - dump the configuration attributes
 */
void
tpp_tcpsmtp(f, tp)
     FILE * f;
     struct transport *tp;
{
    struct tcpsmtp_private *priv;

    (void) dump_standard_config(f,
				tp->private,
				tp->name,
				tp->flags,
				tcpsmtp_attributes,
				end_tcpsmtp_attributes);
    /* Deal with the proc config attributes */
    priv = (struct tcpsmtp_private *) tp->private;
    fprintf(f, "\tshort_timeout=%s,\n", ltoival((long) priv->short_timeout));
    fprintf(f, "\tlong_timeout=%s,\n", ltoival((long) priv->long_timeout));
}



/*
 * tpb_smtp - obsolescent name for tcpsmtp driver
 *
 * This routine exists for backward compatibility with Smail alpha
 * releases prior to version 3.1.12.
 */
char *
tpb_smtp(tp, attrs)
    struct transport *tp;               /* transport entry being defined */
    struct attribute *attrs;            /* list of per-driver attributes */
{
    return tpb_tcpsmtp(tp, attrs);
}


/*
 * tpp_smtp - dump the configuration attributes
 *
 * This routine exists for backward compatibility with Smail alpha
 * releases prior to version 3.1.12.
 */
void
tpp_smtp(f, tp)
     FILE * f;
     struct transport *tp;
{
    tpp_tcpsmtp(f, tp);
}


static char *
set_short_timeout(struct_p, attr)
    char *struct_p;                     /* passed private structure */
    struct attribute *attr;             /* parsed attribute */
{
    struct tcpsmtp_private *priv = (struct tcpsmtp_private *)struct_p;

    return set_timeout(&priv->short_timeout, attr);
}

static char *
set_long_timeout(struct_p, attr)
    char *struct_p;                     /* passed private structure */
    struct attribute *attr;             /* parsed attribute */
{
    struct tcpsmtp_private *priv = (struct tcpsmtp_private *)struct_p;

    return set_timeout(&priv->long_timeout, attr);
}

static char *
set_timeout(timeout, attr)
    unsigned *timeout;                  /* set this timeout variable */
    struct attribute *attr;             /* parsed attribute */
{
    long l;

    if (attr->value == on) {
        return xprintf("%s: boolean form for non-boolean attribute",
                       attr->name);
    }
    l = ivaltol(attr->value);
    if (l < 0) {
        return xprintf("%s: %s: malformed interval",
                       attr->name, attr->value);
    }
    *timeout = (unsigned)l;
    if (*timeout != l) {
        return xprintf("%s: %s: interval too large", attr->name, attr->value);
    }
    return NULL;
}


/*
 * tcpsmtp_connect - return a socket connected to the remote host
 *
 * if the remote host is of the form [192.2.12.3] then use an explicit
 * inet address.
 */
/* ARGSUSED */
static int
tcpsmtp_connect(remote_host, ip_addr, family, service, error)
    char *remote_host;				/* UNUSED */
    struct in_addr *ip_addr;
    short family;
    char *service;
    char **error;
{
    static int port = 0;                /* port to connect to */
    struct servent *smtp_service;       /* service entry */
    struct sockaddr_in sin;             /* inet socket address */
    static char *save_error = NULL;     /* keep handle to free error msgs */
    int s;                              /* socket */
    char *error_text;

    if (isdigit(*service)) {
        error_text = NULL;
        port = c_atol(service, &error_text);
        if (error_text) {
            *error = xprintf("invalid port: %s", service, error_text);
            return -1;
        }
    } else if (port == NULL) {
        smtp_service = getservbyname(service, "tcp");
        if (smtp_service == NULL) {
            *error = xprintf("service name %s not found", service);
            return -1;
        }
        port = smtp_service->s_port;
    }

    bzero((char *)&sin, sizeof(sin));
    sin.sin_addr = *ip_addr;
    sin.sin_family = family;
    sin.sin_port = port;
    s = socket(AF_INET, SOCK_STREAM, 0);
    if (s < 0) {
        if (save_error) {
            xfree(save_error);
        }
        *error = save_error = xprintf("socket: %s", strerror(errno));
        return -1;
    }

    if (connect(s, (struct sockaddr *)&sin, sizeof(sin)) < 0) {
        if (save_error) {
            xfree(save_error);
        }
        *error = save_error = xprintf("connect: %s", strerror(errno));
        (void) close(s);
        return -1;
    }

    return s;
}

static struct error *
connect_failure(tp, connect_error_text)
    struct transport *tp;
    char *connect_error_text;
{
    char *error_text;

    /*
     * ERR_148 - smtp connection failure
     *
     * DESCRIPTION
     *      We failed to connect to the smtp service of the remote
     *      host.  The reason is stored in `error'.
     *
     * ACTIONS
     *      The input addresses are deferred.
     *
     * RESOLUTION
     *      Hopefully we will connect on a retry.
     */
    error_text = xprintf("transport %s: %s", tp->name, connect_error_text);
    DEBUG1(DBG_DRIVER_LO, "%s\n", error_text);
    return note_error(ERR_148, error_text);
}


static int
ip_address(remote_host,ipaddr,family,error)
    char *remote_host;
    struct in_addr *ipaddr;
    short *family;
    char **error;
{
    if (remote_host[0] == '[') {
        /* INET addr literal address */
        char *p = index(remote_host, ']');
        unsigned long inet_number;

        if (p == NULL || p[1] != '\0') {
            *error = "Invalid host address";
            return -1;
        } else {
            *p = '\0';
            inet_number = inet_addr(remote_host + 1);
            *p = ']';
        }
        ipaddr->s_addr = inet_number;
        *family = AF_INET;
    } else {
        struct hostent *hostentp;               /* addr for remote host */

        hostentp = gethostbyname(remote_host);
        if (hostentp == NULL) {
            *error = "Unknown hostname";
            return -1;
        }
        bcopy(hostentp->h_addr, (char *)ipaddr, (size_t) hostentp->h_length);
        *family = hostentp->h_addrtype;
    }
    return 0;
}

