/*
#ident	"@(#)smail/src:RELEASE-3_2:bindlib.c,v 1.29 1996/02/26 17:18:28 woods Exp"
 */

/*
 *    Copyright (C) 1992  Ronald S. Karr
 *
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * bindlib.c
 *      common code for the BIND router and the "tcpsmtp" transport,
 *      both of which can the Domain Name Service as implemented
 *      (typically) by the a Berkeley Internet Name Domain (BIND)
 *      server.
 *
 *      bind code converted to general library by Chip Salzenberg.
 *      Rearranged, recoded and extended by Nigel Metheringham
 *      One new feature added by Philip Hazel 
 *      
 *      See next page for Specs......
 *      
 * Updates:
 *      ** 25-Oct-94 Nigel Metheringham <nigelm@ohm.york.ac.uk>
 *         Modified match_full_or_end_domain to allow negative
 *         matches if domain is preceded by a !.
 *         Affects mx_domains & uk_suffix.
 *         Code previously put into 3.1.28 release and now bought across.
 *
 *      ** 08-Dec-93 Nigel Metheringham <nigelm@ohm.york.ac.uk>
 *         Integrated into proposed smail 3.1.29 release
 *
 *      ** 19-Nov-93 Nigel Metheringham <nigelm@ohm.york.ac.uk>
 *         **** Second Beta Release to world *******************
 *         Added a fallback_gateway function.  Puts in a minimum
 *         value MX record to this gateway which allows people on
 *         poorly connected networks to fudge their routing.
 *         BEWARE - this interacts with the UK routing!!!!! (think)
 *
 *      ** 19-Nov-93 Nigel Metheringham <nigelm@ohm.york.ac.uk>
 *         Fixed bug in config struct - thanks to Max Caines for spotting it.
 *         Made DNS error codes less serious when looking for A
 *         records.  Well, if you can get to one place its all OK!
 *
 *      ** 19-Oct-93 Nigel Metheringham <nigelm@ohm.york.ac.uk>
 *         **** First Beta Release to world *******************
 *         Put in negative match code so that when you *know*
 *         you can't route, you short circuit things.
 *         Problem with previous change is that the mx_domains check
 *         is made against the DN that matched in the DNS, which if
 *         dns_search is enabled, may not be a FQDN.  Caveat-postmaster!
 *
 *      ** 19-Oct-93 Nigel Metheringham <nigelm@ohm.york.ac.uk>
 *         Re-integrated Philip's changes.  Now use new function
 *         match_full_or_end_domain to match domain lists - this
 *         means that the full domain name is matched as well as
 *         any subdomains.  Made uk_suffix a *list* of suffixes.
 *
 *      ** 13-Oct-93 Philip Hazel <ph10@cus.cam.ac.uk>
 *         Added the mx_domains attribute to permit domain-specific
 *         MX-only routing.
 *    
 *      ** 28-Jun-93 Nigel Metheringham <nigelm@ohm.york.ac.uk>
 *         Fixed bug in handling of gatewayed addresses that also
 *         must go via uk_greybook_relay - all the MX hints were
 *         lost due to a typo!
 *         
 *      ** 03-Jun-93 Nigel Metheringham <nigelm@ohm.york.ac.uk>
 *         Added new function thats been waiting for ages.  Now
 *         picks up CNAME loops and produces an error.  CNAME 
 *         loop defined as more CNAMEs than cname_limit config.
 *         Fixed a couple of error handler bugs.  Also changed
 *         so that if there is no list or file of greybook hosts
 *         then it is assumed that we *can* route to them, and
 *         corrected error handling if grey transport is duff.
 *         
 *      ** 28-May-93 Nigel Metheringham <nigelm@ohm.york.ac.uk>
 *         Added new attribute - rewrite_headers - which controls
 *         how gung-ho the system is about rewriting mail headers.
 *         Possible values are 0 (never rewrite), 1 (rewrite if
 *         required - basically if the address has been inverted)
 *         and 2 (always rewrite - tally ho etc...!).
 *         
 *      ** 27-May-93 Nigel Metheringham <nigelm@ohm.york.ac.uk>
 *         Bug fixes prompted by Philip Hazel <ph10@cus.cam.ac.uk>
 *         uk_suffixes handling made more robust.  Defaults fixed.
 *         Will not invert if address already ends in uk_suffix.
 *         And same fix for domain widening
 *
 *      ** 18-Dec-92 Nigel Metheringham <nigelm@ohm.york.ac.uk>
 *         Reindentation of code to Smail specs (ie 4 space tabs).
 *         Renumbered ERR_175 in code to ERR_176 - smail has overtaken me!
 *         
 *      ** 30-Oct-92 Nigel Metheringham <nigelm@ohm.york.ac.uk>
 *         Slight change to handle_null_mx code which stops it core dumping
 *         when the host is a domain with no A & no MX records (ie just NS?)
 *         Also fix to filter_mx_hints applying when all MXs had been filtered
 *         out.  How come I have run this code for 3 months with no problems!!
 *         
 *      ** 30-Oct-92 Nigel Metheringham <nigelm@ohm.york.ac.uk>
 *         Tweak to inversion code - now only inverts domains with dots in!
 *         
 */


/*
 * Specifications for the bind router and/or tcpsmtp transport:
 *
 *      associated transports:
 *          Generally used with an smtp transport.
 *
 *      private attribute data (see also UK specifics below):
 *	    match_domains: domains to match.  Names under any of these
 *		domains will only be matched.  All other domains will
 *              never be matched. This allows having a private DNS
 *              domain, and still utilizating a smart host.
 *          ignore_domains: domains to ignore.  Names under any of these
 *              domains will never be matched.  This prevents expensive
 *              lookups for domains that are known not to be in the DNS.
 *          required: the domain that the host is required to be in to
 *              be matched - this is used for networks behind firewalls.
 *          widen_domains: colon separated list of domains to use to
 *              extend an unmatched bare name.
 *          mx_domains: domains for which the effect of mx_only (see below)
 *              will always be applied.  Domain list separated by colons.
 *              Negative matches can be forced using a preceding ! - this
 *              is useful for excluding a subdomain of a wider domain.
 *              Ordering matters here....
 *          gateways: List of top domains and their relay gateways.  These
 *              are in the form <gateway>:<domain>...:+:
 *          rewrite_headers: Controls the rewriting of the header addresses.
 *              Integer, the higher the more likely it is to rewrite.
 *              Possible values are 0 (never rewrite), 1 (rewrite if
 *              required - basically if the address has been inverted)
 *              and 2 (always rewrite - tally ho etc...!).
 *          cname_limit: Sets a limit of the number of CNAMEs you can
 *              encounter while looking up an address.  This stops someone
 *              making a loop out of a couple of CNAMEs.  Default value 10.
 *
 *      private attribute flags:
 *          defer_no_connect:  if set and we cannot connect to the
 *              name server, try again later.  This is set by default.
 *          local_mx_okay: if not set, an MX record which points to the
 *              local host is considered to be an error which will
 *              will cause mail to be returned to the sender.
 *          defnames:  append a default domain to an unqualified hostname,
 *              using the RES_DEFNAME flag to the resolver library.
 *              This is set by default.
 *          domain_required:  at least two name components are required
 *              in the hostname.  Setting this prevents lookups of
 *              single-component names which are unlikely to be valid
 *              hosts in the DNS.
 *          mx_only:  use only MX records in matching an address.  If a
 *              host doesn't have a MX records, then don't use the A
 *              or WKS records for routing. See also mx_domains above,
 *              which does this for named domains only. 
 *          dns_search:  allow the resolver to search through its domain
 *              list for matches.  Experimental - possibly not good!
 *
 *      UK Specifics.  The following private_attribute data is intended for
 *      use with UK greybook/SMTP selection.  However it could be used to 
 *      make other decisions between transports based on gateways and MX
 *      preference values - possibly even more likely in combination with the
 *      "required" attribute
 *          uk_ignore_gateways: Do not route SMTP through these MX gateways
 *              but set the UK_MX_rejected flag.
 *          uk_greybook_transport: Name of the greybook transport. Also flag
 *              to say that it exists.
 *          uk_max_precedence: Maximum MX value used for SMTP routing within the UK.
 *          uk_greybook_hosts_list: Colon separated list of greybook reachable hosts
 *              given as FQDN in DNS order.
 *          uk_greybook_hosts_file_name:
 *          uk_greybook_hosts_file_proto:
 *          uk_greybook_hosts_file_retries:
 *          uk_greybook_hosts_file_interval: Access bits of a DB of greybook hosts
 *              given in DNS order FQDN.  This file could be generated by the
 *              cnrs package (for example).
 *          uk_greybook_relay: A relay host used for any addresses that exist in
 *              the UK but were not matched by the greybook transport stuff.
 *              This has an advantage over the smarthost in that the host part
 *              of the address is known correct.
 *          uk_suffix:  The domain suffix of a UK destination.  Basically the rules
 *              below, and the factors above apply to all hosts with this suffix
 *              to the FQDN.  Defaults to ".uk" like we're told it should, but most
 *              people are pragmatic and realise the its the academic community with
 *              the odd hangups, and so set it to ".ac.uk" :-)
 *              ** Now changed to be a list of suffixes - any FQDN matching one of
 *              of these suffixes will count as a "uk" destination.
 *              A domain may be preceded by a ! to force a negative match, allowing
 *              a subdomain to be excluded.
 *
 *      If uk_greybook_transport is specified (and an MX maximum is given), but no
 *      list of greybook hosts (either as a list or file) are given, then all
 *      traffic rejected for UK reasons will be sent to the uk_greybook_transport.
 *      If there is a match in the greybook host list or file then the uk_greybook_transport
 *      must be given and be valid.
 *
 * Algorithm
 *     1.   Check that the domain is not being ignored
 *     2.   Check that the domain is not gatewayed.
 *     3.   Look up domain in DNS - get MX records
 *     4.   Select out MX records
 *     5.   Find a transport
 *     6.   If there is no match, and we can flip, redo 1-5 with flipped name.
 *              
 *     DNS lookup:-
 *      A   Try bare name in DNS
 *      B   Try extending name with widen_domains
 *      C   If CNAME is found, redo lookup with CNAME
 *      D   If MX records, put them into mx_hints
 *      E   If no MX records make an implicit MX record (unless mx_only)
 *      F   Remove all MX records with lower precedence than local host
 *          
 *     MX record selection
 *      A   Remove all records with higher precedence than local host
 *      B   For targets with Uk suffix:-
 *          Remove MX records pointing to gateways in the ignored list
 *          Remove MX records with precedence greater than max allowed
 *          If you have removed anything under these rules set the UK_MX_rejected flag
 *          
 *     Transport Matching
 *          If there are valid MX record(s) left, then you're all done.
 *          If MX records were rejected for UK reasons
 *             If full_name is found in list of greybook hosts
 *                route using greybook
 *             else if greybook_relay is defined
 *                get MX records for relay and route by that
 *              
 * **** This is NOT YET IMPLEMENTED!!!!
 * NOTE:  Use of WKS records is enabled if the USE_WKS_RECORDS macro
 *        is defined.  This can be set from the EDITME file with the
 *        MISC_H_DEFINES variable.  If this macro is not defined, then
 *        WKS records are not retrieved and are assumed to exist and
 *        to contain the SMTP service.
 * **** Extra note.  The bind 4.9 distribution says that since no one 
 *      uses WKS records, depending on them is a *bad* idea, so we may
 *      as well drop the whole idea!  [ie I'm not going to implement them!]
 */

#define NEED_SOCKETS
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>
#include "defs.h"

/*
 * Compilation of this entire file depends on "HAVE_BIND".
 */

#ifdef HAVE_BIND

#include "smail.h"
#include "addr.h"
#include "route.h"
#include "transport.h"
#include "bindlib.h"
#include "bindsmtpth.h"
#include "lookup.h"
#include "dys.h"
#ifndef DEPEND
# include "extern.h"
# include "debug.h"
# include "error.h"
#endif

/* Additional return code
 * I need a negative match, which is a stronger statement than DB_NOMATCH
 * so I have hacked one in here, with a remap where it is used so it
 * returns a DB_NOMATCH out to the caller
 */
#ifndef DB_NEGMATCH
#define DB_NEGMATCH (-8)
#endif

#if PACKETSZ > 1024
# define MAXPACKET      PACKETSZ
#else
# define MAXPACKET      1024
#endif

#ifndef GETSHORT
/*
 * earlier versions of bind don't seem to define these useful macros,
 * so roll our own.
 */
# define GETSHORT(i, p) \
        ((i)  = ((unsigned)(*(p)++ & 0xff) << 8),       \
         (i) |= ((unsigned)(*(p)++ & 0xff)))
# define GETLONG(l, p)  \
        ((l)  = ((unsigned long)(*(p)++ & 0xff) << 24), \
         (l) |= ((unsigned long)(*(p)++ & 0xff) << 16), \
         (l) |= ((unsigned long)(*(p)++ & 0xff) << 8),  \
         (l) |= ((unsigned long)(*(p)++ & 0xff)))
# define PUTSHORT(i, p) \
        ((*(p)++ = (unsigned)(i) >> 8),                 \
         (*(p)++ = (unsigned)(i)))
# define PUTLONG(l, p)  \
        ((*(p)++ = (unsigned long)(l) >> 24),           \
         (*(p)++ = (unsigned long)(l) >> 16),           \
         (*(p)++ = (unsigned long)(l) >> 8),            \
         (*(p)++ = (unsigned long)(l)))
#endif

/*
 * The standard rrec structure doesn't have a space for the domain
 * name, so define our own.
 */
enum rr_sect { SECT_AN, SECT_NS, SECT_AR };
typedef struct rr {
    enum rr_sect rr_sect;               /* resource record section */
    char  *rr_dname;                    /* domain name */
    short  rr_class;                    /* class number */
    short  rr_type;                     /* type number */
    int    rr_size;                     /* size of data area */
    char  *rr_data;                     /* pointer to data */
} RR;

/* structure for iterating over RR's with getnextrr() */
struct rr_iterator {
    RR rr;                              /* space for storing RR */
    char dname[MAXDNAME];               /* space for storing domain name */
    char *dp;                           /* pointer within packet */
    HEADER *hp;                         /* saved header pointer */
    char *eom;                          /* end of packet */
    int ancount;                        /* count of answer records */
    int nscount;                        /* count of ns records */
    int arcount;                        /* count of additional records */
};

/* Workspace area - makes the thing more re-enterant than globals! */
struct mx_work_area {
    char * what;                        /* Name of this router */
    char * matched_target;              /* name that matched in DNS */
    char * full_target;                 /* name returned from DNS */
    int inverted;                       /* did we need to invert */
    int UK_MX_rejected;                 /* did we reject for UK reasons */
    HEADER * mx_rrs;                    /* MX RRs */
    int mx_size;                        /* Size of MX record */
    struct transport_hints * mx_hints;  /* MX hints for transport */
    struct bindlib_private * priv;      /* Pointer to router private attributes */
    struct error ** error_p;            /* Pointer to return error messages */
    long flags;                         /* Router flags */
    int local_precedence;               /* Precedence of local host */
    int * found_server;                 /* Have we used the server */
    int * no_server;                    /* Is the server dead? */
};


/*
 * import h_errno; many systems don't define it in <netdb.h>
 */

#ifndef OBSOLETE_RESOLVER
extern int h_errno;
#endif

/* functions local to this file */

#ifdef ANSI_C
# define P_(x) x
#else
# define P_(x) ()
#endif

static int bind_addr_work P_((char*,long,struct bindlib_private*,char*,
                       struct rt_info*,struct error **));
static int full_mx_lookup P_((char *, struct mx_work_area *, int,
                       struct bindlib_private *));
static int find_mx_records P_((char *, struct mx_work_area *, int));
static int decode_cnames P_((struct mx_work_area *));
static int handle_null_mxs P_((struct mx_work_area *, struct bindlib_private *));
static int make_mx_hints P_((struct mx_work_area *));
static int filter_mx_hints P_((struct mx_work_area *));
static int get_addr_hints P_((struct mx_work_area *));
static void make_rt_entry P_((struct rt_info *, char *, char *, struct transport_hints *));
static int route_by_greybook P_((struct mx_work_area *));
static int do_straight_lookup P_((char *, struct mx_work_area *));
static struct mx_work_area * mx_work_area_constructor 
    P_((struct bindlib_private *, struct error **, char *, long, int *, int *));
static void mx_work_area_destructor P_((struct mx_work_area *, int));
static char * check_if_gatewayed P_((char *, char *));
static char *strip_dots P_((char*));
static void flip P_((char *));   /* Flip address between UK/world order */
static char *rewrite_header P_((char *, char *, char *));
static int get_records P_((char*,int,HEADER*,int*,char **));
static RR *getnextrr P_((struct rr_iterator*,int));
static void rewindrr P_((struct rr_iterator*,HEADER*,int));
static void do_header_rewrites P_((char *, char *));
static int find_a_records P_((struct mx_transport_hint*,char*,char **));
static struct error *server_failure P_((char*,char*));
static struct error *packet_error P_((char*,char*,char*));
static struct error *matched_local_host P_((char*,char*));
static struct error *no_transport_error P_((char*,char*));
static struct error *open_failed P_((char*, char*, char*));
static struct error *cname_loop_error P_((char*,char*));
static struct error *res_init_error P_((char*));
static int decode_mx_rr P_((RR*,struct rr_iterator*,char **,int*));
static struct transport_hints * new_mx_hint P_((int,char*,int));
static void add_mx_hint P_((struct transport_hints **,int,char*,int));
static void free_mx_hint P_((struct transport_hints **));
static void add_a_hint P_((struct mx_transport_hint*,char*,char*));
static char * match_full_or_end_domain P_((char * domains, char * target));



/*
 * bind_compute_domain
 *
 * Return the domain that we're in, if we can find it...
 *
 */

char *
bind_compute_domain ()
{
    
#ifdef RES_INIT
    /*  This initialises the resolver.  If the resolver is so old that RES_INIT
     *  isn't defined then the routines will initialise themselves - that is
     *  if the thing will compile at all!
     */
    if ((_res.options & RES_INIT)  == 0) {
	if (res_init() == EOF) {
	    DEBUG(DBG_DRIVER_LO, "ERROR: res_init() failed - unable to get domain\n");
	    return (NULL);
	}
    }

    return(COPY_STRING(_res.defdname));
#else
    /* Not sure whats likely to be possible in a system this old! */
    return(NULL);
#endif
}



/*
 * bind_addr - lookup a host through the domain system
 *
 * Use the algorithm described at the top of this source file for
 * finding a match for a target.
 *
 * Return one of the following values:
 *
 * These return codes apply only to the specific address:
 *      DB_SUCCEED      Matched the target host.
 *      DB_NOMATCH      Did not match the target host.
 *      DB_FAIL         Fail the address with the given error.
 *      DB_AGAIN        Try to route with this address again at a
 *                      later time.
 *
 * These return codes apply to this router in general:
 *      FILE_NOMATCH    There is no server running on this machine.
 *      FILE_AGAIN      Lost contact with server, or server is
 *                      required to exist.  Try again later.
 *      FILE_FAIL       A major error has been caught in router,
 *                      notify postmaster.
 */

int
bind_addr(raw_target, flags, priv, what, rt_info, error_p)
    char *raw_target;                   /* raw target address */
    long flags;                         /* bind-specific flags */
    struct bindlib_private *priv;       /* bind-specific data */
    char *what;                         /* who called, for messages */
    struct rt_info *rt_info;            /* return route info here */
    struct error **error_p;             /* return lookup error here */
{
    long save_res_options;              /* like _res.options in <resolv.h> */
    int ret;

#ifdef RES_INIT
    /*  This initialises the resolver.  If the resolver is so old that RES_INIT
     *  isn't defined then the routines will initialise themselves - that is
     *  if the thing will compile at all!
     */
    if ((_res.options & RES_INIT)  == 0) {
	if (res_init() == EOF) {
	    DEBUG(DBG_DRIVER_LO, "ERROR: res_init() failed - defering addresses\n");
	    *error_p = res_init_error(what);
	    return (FILE_FAIL);
	}
    }
#endif
    save_res_options = _res.options;
    ret = bind_addr_work(raw_target, flags, priv, what, rt_info, error_p);
    _res.options = save_res_options;

    return ret;
}

static int
bind_addr_work(raw_target, flags, priv, what, rt_info, error_p)
    char *raw_target;                   /* raw target address */
    long flags;                         /* bind-specific flags */
    struct bindlib_private *priv;       /* bind-specific data */
    char *what;                         /* who called, for messages */
    struct rt_info *rt_info;            /* return route info here */
    struct error **error_p;             /* return lookup error here */
{
    char *target;                       /* raw_target stripped of extra dots */
    static char *orig_target = NULL;    /* Original copy of target */
    static int no_server = FALSE;       /* TRUE if no server process */
    static int found_server = FALSE;    /* TRUE if server process found */
    int success;                        /* value to return */
    int inverting = FALSE;              /* trying inverted address */
    struct mx_work_area * mxdat;        /* Work area for MX operations */
    char * gateway = NULL;

    if (no_server) {
        return FILE_AGAIN;              /* FILE_AGAIN more appropriate than DB_NOMATCH */
    }

    /*
     * strip extra dots from the target to ensure sane lookups.
     * If the target contained any dots before being stripped,
     * then don't allow the resolver to add the default domain.
     * This allows mail to high-level domains (e.g., "com.")
     * while also allowing use of default domain suffixes.
     */

#ifdef RES_DEFNAMES
# ifndef RES_DNSRCH
# define RES_DNSRCH     0
# endif
    if (flags & BIND_DEFNAMES && strchr(raw_target, '.') == NULL) {
        _res.options |= RES_DEFNAMES|RES_DNSRCH;
    } else {
        if (flags & BIND_DNS_SEARCH) {
            _res.options &= ~(RES_DEFNAMES);
            _res.options |= RES_DNSRCH;
        } else {
            _res.options &= ~(RES_DEFNAMES|RES_DNSRCH);
        }
    }
#endif

    /*
     * Omit needless dots!
     */

    target = strip_dots(raw_target);
    if (orig_target)
        xfree(orig_target);
    orig_target = COPY_STRING(target);

    do {
        target = orig_target;

        /*
         * If the target is not in one of the "match" domains, then don't
         * match it.  This prevents lookups of domains that are known not 
         * to exist in the DNS.
         *
         * Note: If the target isn't a fully qualified domain name 
         *       (i.e. doesn't contain a '.'), then assume it to be
         *       inside the default domain.
         */

        if (priv->match_domains) {
	    if (strchr(target, '.') != NULL) {
    	        if (match_end_domain(priv->match_domains, target) == NULL) {
    		    DEBUG1(DBG_DRIVER_LO, "Target %s not in match_domains\n", target);
    	            return DB_NOMATCH;
    	        }
    	        else {
    		    DEBUG1(DBG_DRIVER_LO, "Target %s in match_domains\n", target);
    	        }
            }
    	    else {
    	        DEBUG1(DBG_DRIVER_LO, "Target %s assumed to be in the default domain\n", target);
    	    }
        }
        
        /*
         * if the target is in one of the "ignore" domains, then don't
         * match it.  This prevents expensive lookups of domains that are
         * known not to exist in the DNS, such as .uucp or .bitnet.
         */

        if (priv->ignore_domains) {
            if (match_full_or_end_domain(priv->ignore_domains, target) != NULL) {
                return DB_NOMATCH;
            }
        }

        if (debug >= DBG_DRIVER_HI) {
            _res.options |= RES_DEBUG;  /* turn on resolver debugging */
        }

        /*
         * first see if the address matches one of the list of special
         * gateways.  if so, change the target to the gateway, and set
         * the full_target to the original string so it will be passed
         * on to the gateway.  the flag prevents subsequent updating of
         * full_target from the DNS records.
         */

        if ((gateway = check_if_gatewayed(target, priv->gateways)) != NULL) {
            target = strip_dots(gateway);
        }

        rt_info->matchlen = strlen(raw_target);

        /*
         * if the domain_required flag is set, then the target hostname
         * is required to have at least one "."
         */

        if (flags & BIND_DOMAIN_REQUIRED && strchr(target, '.') == NULL) {
            return DB_NOMATCH;
        }

        mxdat = mx_work_area_constructor(priv, error_p, what, flags, &found_server, &no_server);
        success = full_mx_lookup(target, mxdat, (gateway == NULL), priv);

        if (success == DB_SUCCEED) {
            /*
             * if the target is not in one of the required domains, then don't
             * match it. For systems behind firewalls who "see" systems on the
             * Internet in DNS, but which can't actually reach them.  If a host
             * is outside a local domain, this will fail and presumably fall
             * through to smart_path.
             */
            if (priv->required) {
                if (match_full_or_end_domain(priv->required, mxdat->full_target) == NULL) {
                    mx_work_area_destructor(mxdat, FALSE);
                    return DB_NOMATCH;
                }
            }

            if (gateway == NULL) {
                if (priv->rewrite_headers >= (inverting ? BIND_REWRITE_IFREQUIRED : BIND_REWRITE_ALWAYS))
                    do_header_rewrites(raw_target, mxdat->full_target);
                make_rt_entry(rt_info, mxdat->full_target, (char *) NULL, mxdat->mx_hints);
            } else {
                if (priv->rewrite_headers >= (inverting ? BIND_REWRITE_IFREQUIRED : BIND_REWRITE_ALWAYS))
                    do_header_rewrites(raw_target, orig_target);
                make_rt_entry(rt_info, mxdat->full_target, orig_target, mxdat->mx_hints);
            }
            mx_work_area_destructor(mxdat, TRUE);
            return DB_SUCCEED;
        }
    
        if ((success == DB_NOMATCH) && (mxdat->UK_MX_rejected)) {
            if ((priv->uk_greybook_transport  != NULL) ||
                (priv->uk_greybook_hosts_list != NULL) ||
                (priv->uk_greybook_hosts_file != NULL)) {
                success = route_by_greybook(mxdat); /* Try to send by greybook */
                if (success == DB_SUCCEED) {
                    DEBUG(DBG_ROUTE_LO, "Matched by greybook lookups\n");
                    if ((priv->uk_greybook_transport == NULL) ||
                        ((rt_info->transport = find_transport(priv->uk_greybook_transport)) == NULL)) {
                        *(mxdat->error_p) = no_transport_error(mxdat->what, 
                                                               priv->uk_greybook_transport ? 
                                                               priv->uk_greybook_transport :
                                                               "(not specified)");
                        success = DB_FAIL;
                    } else {
                        if (priv->rewrite_headers >= (inverting ? BIND_REWRITE_IFREQUIRED : BIND_REWRITE_ALWAYS))
			    do_header_rewrites(raw_target, 
                                               ((!gateway) ?  mxdat->full_target : orig_target));
                        make_rt_entry(rt_info, mxdat->full_target, 
				      (!gateway) ? (char *) NULL : orig_target,
				      (struct transport_hints *) NULL /* No transport hints */);
                        /* Ensure that the target and the route are in UK order,
                           unless the option is set to suppress this (future hope!) */

                        if (!(flags & BIND_UK_GREY_WORLD)) {
                            if (rt_info->route)
                                flip(rt_info->route);
                            if (rt_info->next_host)
                                flip(rt_info->next_host);
                        }
                        mx_work_area_destructor(mxdat, FALSE);
                        return DB_SUCCEED;
                    }
                }
            }
            if (success == DB_NOMATCH) {
                if (priv->uk_greybook_relay != NULL) {
                    struct mx_work_area * mxdat2;
        
                    mxdat2 = mx_work_area_constructor(priv, error_p, what, flags, &found_server, &no_server);
                       success = full_mx_lookup(priv->uk_greybook_relay, mxdat2, FALSE, priv);
                    if (success == DB_SUCCEED) {
                        if (gateway == NULL) {
                            if (priv->rewrite_headers >= (inverting ? BIND_REWRITE_IFREQUIRED : BIND_REWRITE_ALWAYS))
                                do_header_rewrites(raw_target, mxdat->full_target);
                            make_rt_entry(rt_info, mxdat2->full_target, 
                                          mxdat->full_target, mxdat2->mx_hints);
                        } else {
                            if (priv->rewrite_headers >= (inverting ? BIND_REWRITE_IFREQUIRED : BIND_REWRITE_ALWAYS))
                                do_header_rewrites(raw_target, orig_target);
                            make_rt_entry(rt_info, mxdat2->full_target, orig_target, 
                                          mxdat2->mx_hints);
                        }
                        mx_work_area_destructor(mxdat2, TRUE);
                        mx_work_area_destructor(mxdat, FALSE);
                        return DB_SUCCEED;
                    }   
                    mx_work_area_destructor(mxdat2, FALSE);
                } else {
                    /* We have a positive match, but can't route to it, so there is no
                     * point looking further
                     */
                    success = DB_NEGMATCH;
                }
            }
        }
        if ((success != DB_NOMATCH) || (mxdat->UK_MX_rejected)) {
            /* We return if an address was not matched for UK reasons because
             * we know that we have actually matched the address - we just are
             * unable to route to it.
             */
            mx_work_area_destructor(mxdat, FALSE);
            if (success == DB_NEGMATCH)
                return DB_NOMATCH;
            else
                return success;
        }
        mx_work_area_destructor(mxdat, FALSE);

        /* If we can invert then set flag - do not invert single hostnames */
        inverting = (flags & BIND_UK_TRY_INVERT) && 
            !inverting && 
                strchr(target, '.') &&
                    ((priv->uk_suffix != NULL) ? 
                     (match_full_or_end_domain(priv->uk_suffix, target) == NULL) : 1);
        if (inverting) {
            flip(target);
        }
    } while (inverting);
    return DB_NOMATCH;
}



static void make_rt_entry(rt_info, next_host, route, mx_hints)
     struct rt_info * rt_info;
     char * next_host;
     char * route;
     struct transport_hints * mx_hints;
{
    rt_info->next_host = COPY_STRING(next_host);
    if (route != NULL)
        rt_info->route = COPY_STRING(route);
    else 
        rt_info->route = NULL;

    rt_info->tphint_list = mx_hints;
}



static int route_by_greybook(mxdat)
     struct mx_work_area * mxdat;
{
    /* This may look odd, but if neither the list or the file
     * is set, then we assume that *all* greybook hosts can be
     * contacted
     */
    if ((mxdat->priv->uk_greybook_hosts_list == NULL) &&
        (mxdat->priv->uk_greybook_hosts_file == NULL))
        return DB_SUCCEED;

    /* Check to see if the host is in the grey book host list
     * This list is intended for sites that have very limited grey book
     * connectivity OR for additional special cases
     */
    if (mxdat->priv->uk_greybook_hosts_list) {
        if (is_string_in_list(mxdat->full_target, mxdat->priv->uk_greybook_hosts_list)) {
            return DB_SUCCEED;
        }
    }
    if (mxdat->priv->uk_greybook_hosts_file != NULL) {
        int success;
        char * db_ptr;
        char *error_text;
        char *return_value;

        DEBUG1(DBG_ROUTE_HI, "Opening greybook hosts file %s\n", 
               mxdat->priv->uk_greybook_hosts_file);
        success = open_database(mxdat->priv->uk_greybook_hosts_file,
				mxdat->priv->uk_greybook_hosts_proto,
				(int) mxdat->priv->uk_greybook_host_retries,
				(int) mxdat->priv->uk_greybook_host_interval,
				(struct stat *)NULL, 
				&db_ptr,
				&error_text);
        if (success != FILE_SUCCEED) {  /* If DB open fails... */
            *mxdat->error_p = open_failed(mxdat->what, 
                                          mxdat->priv->uk_greybook_hosts_file,
                                          error_text);
            return success;
        }
        DEBUG1(DBG_ROUTE_MID, "Looking up greybook host %s\n", mxdat->full_target);
        success = lookup_database(db_ptr,
                                  mxdat->full_target,
                                  &return_value, 
                                  &error_text);
        DEBUG(DBG_ROUTE_HI, "Closing greybook hosts file.\n");
        close_database(db_ptr);
        if (success == DB_SUCCEED) 
            return DB_SUCCEED;          /* Pushing it but OK here! */
    }
    return DB_NOMATCH;
}


static void do_header_rewrites (from, to)
     char * from;
     char * to;
{
    struct list *q = header;
    if EQIC(from, to) return;

    DEBUG2(DBG_ROUTE_MID, "Header rewrite \"%s\" -> \"%s\"\n", from, to);
    while (q != NULL) {
        if (strncmpic(q->text, "to:", 3) == 0 ||
            strncmpic(q->text, "cc:", 3) == 0)
            {
                DEBUG1(DBG_ROUTE_HI, "Rewriting header -- %s", q->text)
                    q->text = rewrite_header(q->text, from, to);
                DEBUG1(DBG_ROUTE_HI,  "              to -- %s", q->text)
                }
        q = q->succ;
    }
}

/*
 * full_mx_lookup       - Performs all the sections of the MX lookup
 *                        including filtering and IP address lookup
 *
 * This co-ordinates all the subsections of an MX lookup.
 * It also carries out the UK specific filtering - ie ensuring
 * that unwanted relays are not used etc...
 * This means that a DB_NOMATCH can be returned *but* that its
 * (relatively) OK because mxdat->UK_MX_rejected is set, so greybook
 * routing can be done elsewhere.
 */
static int full_mx_lookup (target, mxdat, allow_mangling, priv)
     char * target;
     struct mx_work_area * mxdat;
     int allow_mangling;
     struct bindlib_private *priv; 
{
    int success;                        /* how have we done? */

    success = find_mx_records(target, mxdat, allow_mangling);
    if (success != DB_SUCCEED) {
        return success;                 /* Actually this is failure! */
    }

    success = decode_cnames(mxdat);
    if (success != DB_SUCCEED) {
        return success;
    }

    success = make_mx_hints(mxdat);
    if (success != DB_SUCCEED) {
        return success;
    }

    if (mxdat->mx_hints == NULL) {
        success = handle_null_mxs(mxdat, priv);
        if (success != DB_SUCCEED) {
            return success;
        }
        if (mxdat->priv->fallback_gateway) 
            add_mx_hint(&(mxdat->mx_hints), BIND_FALLBACK_PREFERENCE, 
                        mxdat->priv->fallback_gateway, 1);
    } else {
        if (mxdat->priv->fallback_gateway) 
            add_mx_hint(&(mxdat->mx_hints), BIND_FALLBACK_PREFERENCE, 
                        mxdat->priv->fallback_gateway, 1);
        /* Can only filter MX records if there are some! */
        success = filter_mx_hints(mxdat);
        if (success != DB_SUCCEED) {
            return success;
        }
    }

    success = get_addr_hints(mxdat);
    if (success != DB_SUCCEED) {
        return success;
    }

    if (mxdat->mx_hints == NULL) {
        return DB_NOMATCH;              /* Not sure if you really can get here! */
    } else {
        return DB_SUCCEED;
    }
}



/*
 * find_mx_records      - look up the target in the DNS
 *
 * High level query function - looks up name in DNS.  Also performs
 * various name extensions as requested.  Returns a stack of
 * MX records in the mxdat struct as well as the name that actually
 * matched in the DNS.
 * The return value is the standard DB lookup status.
 */
static int find_mx_records (target, mxdat, allow_mangling)
     char * target;
     struct mx_work_area * mxdat;
     int allow_mangling;
{
    char * cur_target = target;
    int success;

    success = do_straight_lookup(cur_target, mxdat);

    if ((success == DB_NOMATCH) && allow_mangling) {
        char * w;                       /* Working string */
        static struct str p;            /* region for building new string */
        static int initialised = FALSE; /* Have I initialised p yet? */
        if (!initialised) {
            STR_INIT(&p);
            initialised++;
        }
        if ((mxdat->priv->widen_domains != NULL) &&
            ((mxdat->priv->uk_suffix != NULL) ? 
             (match_full_or_end_domain(mxdat->priv->uk_suffix, target) == NULL) : 1)) {
            for (w = strcolon(mxdat->priv->widen_domains); (w != NULL); (w = strcolon((char *) NULL))) {
                p.i = 0;                /* Zero string length */
                str_cat(&p, target);
                str_cat(&p, ".");       /* Add a dot */
                str_cat(&p, w);         /* Append domain */
                cur_target = p.p;
                success = do_straight_lookup(cur_target, mxdat);
                if (success != DB_NOMATCH)
			break;			/* Fall out end if not not found! */
            }
        }
    }
    if (mxdat->matched_target) {
        xfree(mxdat->matched_target);
        mxdat->matched_target = NULL;
    }
    if (success == DB_SUCCEED) {
        mxdat->matched_target = COPY_STRING(cur_target);
    }
    return success;
}


/*
 * do_straight_lookup   - relatively simple straight lookup routine!
 *
 */
static int do_straight_lookup(target, mxdat)
     char * target;
     struct mx_work_area * mxdat;
{
    int success;
    char * error_msg;

    DEBUG1(DBG_DRIVER_MID, "bindlib: MX lookup for %s\n", target);
    success =  get_records(target, 
                           T_MX, 
                           mxdat->mx_rrs, 
                           &(mxdat->mx_size), 
                           &error_msg);
    switch (success) {

      case DB_SUCCEED:
        *mxdat->found_server = TRUE;
        break;

      case FILE_NOMATCH:
      case DB_AGAIN:
	*mxdat->error_p = server_failure(mxdat->what, error_msg);
	if (!(mxdat->flags & BIND_DEFER_NO_CONN)) {
	    /* We should defer all addresses... hopefully the server will return! */
	    /* NB If this error is address related - ie it always happens to one
	     *    address we are in real trouble here - nothing will get done by
	     *    the router 
	     */
	    success = FILE_NOMATCH;
	} else {
	    success = DB_AGAIN;
	}
	break;

      case DB_FAIL:
      case FILE_AGAIN:
        *mxdat->error_p = server_failure(mxdat->what, error_msg);
        break;
  
    }
    return success;
}



/*
 * decode_cnames        - Decode any cnames returned by earlier
 *                        lookups.
 *
 * If there are CNAME records in the set returned, then we look
 * up on the domain name pointed to by those records.  We iterate
 * this until we get a proper match (maybe we should have a
 * CNAME limit here - someone could make a loop in their DNS
 * entry??)
 */
static int decode_cnames(mxdat)
     struct mx_work_area * mxdat;
{
    struct rr_iterator mx_it;
    RR *mx_rr;
    int success;
    int num_iterations = 0;

    do {                                /* Iterate through any CNAMEs */
        rewindrr(&mx_it, mxdat->mx_rrs, mxdat->mx_size);
        while ((mx_rr = getnextrr(&mx_it, FALSE)) && mx_rr->rr_type != T_CNAME)
            ;
        if (mx_rr != NULL) {
            static char nambuf[MAXDNAME];
            int dlen;

            if ((mxdat->priv->cname_limit > 0) && (num_iterations++ >= mxdat->priv->cname_limit)) {
                *(mxdat->error_p) = cname_loop_error(mxdat->what, mxdat->matched_target);
                return DB_FAIL;
            }
            dlen = dn_expand((unsigned char *)mx_it.hp, (unsigned char *)mx_it.eom, 
                             (unsigned char *)mx_rr->rr_data, (unsigned char *)nambuf, 
                             MAXDNAME);
            if (dlen < 0) {
                /* format error in response packet */
                *(mxdat->error_p) = packet_error(mxdat->what, "CNAME", mxdat->matched_target);
                return DB_AGAIN;
            }
            /*
             * OK, we have a CNAME - lets do another lookup as told:-
             * RFC 974:
             * There is one other special case.  If the response contains
             * an answer which is a CNAME RR, it indicates that REMOTE is
             * actually an alias for some other domain name. The query
             * should be repeated with the canonical domain name.
             */
            success =  find_mx_records(nambuf, mxdat, FALSE);
            if (success != DB_SUCCEED) {
                /* Cleanup code - deallocate storage */
                return success;         /* Actually this is failure! */
            }
        }
    } while (mx_rr != NULL);
    return DB_SUCCEED;
}


/*
 * handle_null_mxs      - Handle situation where no MX records are found
 *
 * This generally means putting in a zero precendence MX for the
 * host itself.  However a config option is to insist that MX
 * records exist - in this case we bomb out! Another config option
 * insists on MX records for specific domains. 
 */

static int handle_null_mxs(mxdat, priv)
     struct mx_work_area * mxdat;
     struct bindlib_private *priv;  
{
    int success;

    if (mxdat->flags & BIND_MX_ONLY) {
        /* We need MX records.  We have none - hence failed! */
        return DB_NEGMATCH;
    } else if (match_full_or_end_domain(priv->mx_domains, mxdat->matched_target) != NULL) {
        return DB_NEGMATCH;
    } else {
        /*
         * from RFC 974:
         * It is possible that the list of MXs in the response to
         * the query will be empty.  This is a special case.  If the
         * list is empty, mailers should treat it as if it contained
         * one RR, an MX RR with a preference value of 0, and a host
         * name of REMOTE.      (I.e., REMOTE is its only MX).  In
         * addition, the mailer should do no further processing on
         * the list, but should attempt to deliver the message to
         * REMOTE.      The idea here is that if a domain fails to
         * advertise any information about a particular name we will
         * give it the benefit of the doubt and attempt delivery.
         */


        add_mx_hint(&(mxdat->mx_hints), 0,
                    mxdat->matched_target,
                    TRUE);
        success = get_addr_hints(mxdat);
        if (success == DB_SUCCEED) {
            /* OK we have an implicit MX with address records (we hope!)
             * but we may not have the FQDN as yet.  So lets get the
             * FQDN out of the address hint.  Just remember here that
             * there is *only* one MX record - I put it in just above!
             */
#define mx_hint ((struct mx_transport_hint *)(mxdat->mx_hints->private))
            xfree(mx_hint->exchanger);
            mx_hint->exchanger = COPY_STRING(mx_hint->ipaddrs->hostname);
            if (mxdat->full_target == NULL) {
                mxdat->full_target = COPY_STRING(mx_hint->exchanger);
            }

            /* Check if local host */
            if (islocalhost(mxdat->full_target)) {
                mxdat->local_precedence = 0;
                if ((mxdat->flags & BIND_LOCAL_MX_OKAY) == 0) {
                    *(mxdat->error_p) = matched_local_host(mxdat->what, mxdat->matched_target);
                    return DB_FAIL;
                }
            }
        }

        return success;                 /* A touch optomistic! */
#undef mx_hint
    }
}



/*
 * make_mx_hints        - Build MX hints from the received
 *                        MX RRs
 *
 *
 */

static int make_mx_hints(mxdat)
     struct mx_work_area * mxdat;
{
    struct rr_iterator mx_it;
    RR * mx_rr;
    int success;

    rewindrr(&mx_it, mxdat->mx_rrs, mxdat->mx_size);
    while ((mx_rr = getnextrr(&mx_it, FALSE))) {
        if (mx_rr->rr_type == T_MX) {
            char * name;
            int precedence;

            success = decode_mx_rr(mx_rr, &mx_it, &name, &precedence);
            if (success != DB_SUCCEED) {
                *(mxdat->error_p) = packet_error(mxdat->what, "MX", mxdat->matched_target);
                return DB_AGAIN;
            }

            add_mx_hint(&(mxdat->mx_hints), precedence, name, FALSE);

            /* Extract the full name of the target if poss */
            if (mxdat->full_target == NULL) {
                mxdat->full_target = COPY_STRING(mx_it.dname);
            }

            /* Pick up the local host's precedence */
            if (islocalhost(name) && 
                (mxdat->local_precedence < 0 || precedence < mxdat->local_precedence))
                mxdat->local_precedence = precedence;
        }
    }
    return DB_SUCCEED;
}


/*
 * filter_mx_hints      - Apply the various filtering operations to
 *                        the received MX records
 *                        Basically this means making sure that there
 *                        no MX records with precedence higher than that
 *                        of the local host.
 *                        If you want UK behaviour (poor thing), then tossing
 *                        of records using "banned" gateways or exccessive
 *                        preference values are also tossed.
 */

static int filter_mx_hints(mxdat)
     struct mx_work_area * mxdat;
{
    struct transport_hints ** mx_a;
    int low_precedence = -1;


#define mx_hint ((struct mx_transport_hint *)((*mx_a)->private))
    for(mx_a = &(mxdat->mx_hints); (*mx_a);) {
        char *name = mx_hint->exchanger;
        int precedence = mx_hint->preference;

        if ((mxdat->priv->uk_suffix != NULL) &&
           (match_full_or_end_domain(mxdat->priv->uk_suffix, mxdat->full_target))) {
            if ((mxdat->priv->uk_max_precedence) &&
                (mx_hint->preference > mxdat->priv->uk_max_precedence)) {
                DEBUG2(DBG_ROUTE_HI, "Removed MX hint %s preference %d [UK transport]\n", 
                       name, precedence);
                free_mx_hint(mx_a);
                mxdat->UK_MX_rejected = TRUE;
                continue;
            }
      
            if ((mxdat->priv->uk_ignore_gateways != NULL) &&
                (is_string_in_list(name, mxdat->priv->uk_ignore_gateways))) {
                DEBUG2(DBG_ROUTE_HI, "Removed MX hint %s preference %d [UK gateways]\n", 
                       name, precedence);
                free_mx_hint(mx_a);
                mxdat->UK_MX_rejected = TRUE;
                continue;
            }
        }
        if (mxdat->local_precedence >= 0 && precedence >= mxdat->local_precedence) {
            /*
             * RFC 974:
             * If the domain name LOCAL is listed as an MX RR, all MX
             * RRs with a preference value greater than or equal to that
             * of LOCAL's must be discarded.
             */
            DEBUG2(DBG_ROUTE_HI, "Removed MX hint %s preference %d [Local precedence]\n", 
                   name, precedence);
            free_mx_hint(mx_a);
            continue;
        } else if (low_precedence < 0 || precedence < low_precedence) {
            low_precedence = precedence;
        }
#undef mx_hint
        mx_a = &(*mx_a)->succ;
    }
    if (! mxdat->mx_hints) {
        if (mxdat->local_precedence >= 0) {
            /* This means we rejected because the local host is the prefered MX */
            if (mxdat->flags & BIND_LOCAL_MX_OKAY) {
                return DB_SUCCEED;
            } else {
                *(mxdat->error_p) = matched_local_host(mxdat->what, mxdat->matched_target);
                return DB_FAIL;
            }
        }
        return DB_NOMATCH;
    }
    return DB_SUCCEED;
}


/*
 * get_addr_hints       - Find all the address hints for the MX records
 *
 * This both picks out the A records in the additional section of the
 * MX records, and also goes off and finds A records for any hosts
 * for which they have not been supplied.
 * As a hack, you may not have got the full_target name for a host
 * which was implicity MXed, so we cheat and pull it out here, if
 * needed!
 */
static int get_addr_hints(mxdat)
     struct mx_work_area * mxdat;
{
    struct rr_iterator a_it;
    RR * a_rr;
    struct transport_hints ** mx_a;
    int success;
    char * error_msg;
    int dns_timeout = 0;

    /* look for relevant A records in the additional section of the MX answer */
    rewindrr(&a_it, mxdat->mx_rrs, mxdat->mx_size);
    while ((a_rr = getnextrr(&a_it, TRUE))!=0) {
        struct transport_hints * hint;

        if (a_rr->rr_type==T_A) {
            for ((hint = mxdat->mx_hints); (hint != NULL); (hint = hint->succ)) {
#define mx_hint ((struct mx_transport_hint *)(hint->private))
                if (EQIC(mx_hint->exchanger,a_it.dname)) {
                    add_a_hint(mx_hint, a_it.dname, a_rr->rr_data);
                }
            }
#undef mx_hint
        }
    }


    mx_a = &(mxdat->mx_hints);
    while (*mx_a) {
#define mx_hint ((struct mx_transport_hint *)((*mx_a)->private))
        if (! mx_hint->ipaddrs) {
            /* Go out and get an A record */
            success = find_a_records(mx_hint, mx_hint->exchanger, &error_msg);
            switch (success) {
        
              case DB_SUCCEED:
                mx_a = &(*mx_a)->succ;
                break;

              case DB_NOMATCH:
                if (! mx_hint->implicit)
                    /* This is an error! */;
                free_mx_hint(mx_a);
                break;

              case FILE_NOMATCH:
                success = FILE_AGAIN;
                /* FALLTHRU */

              case DB_AGAIN:
                free_mx_hint(mx_a);
                *(mxdat->error_p) = server_failure(mxdat->what, error_msg);
                dns_timeout++;
                break;

              case DB_FAIL:
              case FILE_AGAIN:
                /* Ignore this - it shouldn't be a problem.... */
                *(mxdat->error_p) = server_failure(mxdat->what, error_msg);
                free_mx_hint(mx_a);
                break;
            }
        } else {
            mx_a = &(*mx_a)->succ;
        }
#undef mx_hint
    }

    if (mxdat->mx_hints)
        return DB_SUCCEED;
    else {
        if (dns_timeout)
            return DB_AGAIN;
        else
            return DB_NOMATCH;
    }
}


/*
 * strip_dots - remove extra dots from a hostname string
 *
 * Remove all dots from the beginning and end of a string.  Also, any
 * sequence of more than one dot is replaced by a single dot.  For
 * example, the string:
 *
 *      .att..com.
 *
 * will result in the string:
 *
 *      att.com
 *
 * The operation is non-destructive on the passed string.  The
 * resulting value points to a region which may be reused on
 * subsequent calls to strip_dots().
 */
static char *
strip_dots(s)
    register char *s;
{
    static struct str new;              /* region for building new string */
    static int inited = FALSE;          /* true if target initialized */

    /* initialize or clear the new string */
    if (! inited) {
        STR_INIT(&new);
    } else {
        new.i = 0;
    }

    /*
     * copy target, removing extra dots.
     */
    while (*s == '.') s++;
    do {
        if (*s == '.') {
            while (*s == '.') s++;
            if (*s) --s;
        }
        STR_NEXT(&new, *s);
    } while (*s++);

    return new.p;
}

/*
 * mx_work_area_constructor     - A constructor for mx_work_area
 *
 * Vaguely C++, but done so that I was getting the intialisation
 * and allocation correct.  The destructor also means I can make
 * sure the memory is deallocated correctly!
 */

static struct mx_work_area * mx_work_area_constructor (private, error_p, what, flags, 
                                                       found_server, no_server)
     struct bindlib_private * private;  /* Link to the router private info */
     struct error ** error_p;           /* Pointer to return error messages */
     char * what;                       /* Name of this router */
     long flags;                        /* Router specific flags */
     int * found_server;                /* Pointer to server status */
     int * no_server;                   /* Pointer to server down status */
{
    struct mx_work_area * mxdat;

    /* Allocate storage */
    mxdat = (struct mx_work_area *) xmalloc(sizeof(struct mx_work_area));

    /* Initialise data structure */
    mxdat->what = what;                 /* Set name of router */
    mxdat->matched_target = NULL;       /* Not yet set */
    mxdat->full_target = NULL;          /* Not yet set */
    mxdat->inverted = FALSE;            /* Not inverted (yet) */
    mxdat->UK_MX_rejected = FALSE;      /* have not rejected (yet) */
    mxdat->mx_rrs = (HEADER *)xmalloc(MAXPACKET);
    /* Make buffer for MX replies */
    mxdat->mx_size = 0;                 /* More for completeness than correctness */
    mxdat->mx_hints = NULL;             /* No MX hints yet */
    mxdat->flags = flags;               /* Copy flag values */
    mxdat->priv = private;              /* Set pointer to private attributes */
    mxdat->local_precedence = -1;       /* Mark local_precedence as not seen */
    mxdat->error_p = error_p;
    mxdat->found_server = found_server;
    mxdat->no_server = no_server;
    return mxdat;
}


/*
 * mx_work_area_destructor      - A destructor for mx_work_area
 *
 * Vaguely C++, but done so that I was getting the intialisation
 * and allocation correct.  The destructor also means I can make
 * sure the memory is deallocated correctly!
 */

static void mx_work_area_destructor (mxdat, do_not_deallocate_mxhints)
     struct mx_work_area * mxdat;
     int do_not_deallocate_mxhints;
{
    /* Deallocate storage */
    if (mxdat->matched_target != NULL) xfree(mxdat->matched_target);
    if (mxdat->full_target != NULL)    xfree(mxdat->full_target);
    if (mxdat->mx_rrs != NULL)         xfree(mxdat->mx_rrs);

    if (!do_not_deallocate_mxhints) {
        while(mxdat->mx_hints) {
            free_mx_hint(&(mxdat->mx_hints));
        }
    }
    xfree(mxdat);
}


  /* First see if the address matches one of the list of special
   * gateways. If so, change the target to the gateway, and set
   * the full_target to the original string so it will be passed
   * on to the gateway. 
   */

static char * check_if_gatewayed (target, gateways)
     char * target;
     char * gateways;
{
    register char * cur;
    static char gatebuf[80];
    int len = strlen(target);           /* length of target */
    int gated = FALSE;

    if (gateways == NULL)
        return NULL;

    cur = strcolon(gateways);
    while (cur && !gated) {
        strcpy(gatebuf, cur);
        cur = strcolon((char *)NULL);
        while (cur) {
            register int domlen = strlen(cur);
      
            if (strcmp(cur, "+") == 0) {
                cur = strcolon((char *)NULL);
                break;
            } else {
                if ((len > domlen && target[len - domlen - 1] == '.' &&
                     EQIC(target + len - domlen, cur)) ||
                    (len == domlen && EQIC(target, cur))) {
                    gated = TRUE; 
                    break;
                } else { 
                    cur = strcolon((char *)NULL);
                }
            }
        }
    }
    return gated ? gatebuf : NULL;
}


/*
 * flip - flip address between UK/world order
 *
 * operates on the string in situ
 */
static void
flip(s)
    char *s;
{
    static struct str new;              /* region for building new string */
    static int inited = FALSE;          /* true if target initialized */
    char *p;

    /* initialize or clear the new string */
    if (! inited) {
        STR_INIT(&new);
    } else {
        new.i = 0;
    }

    p = s + strlen(s);
    while (p > s) {
        if (*--p == '.') {
            *p = '\0';
            if (*(p + 1)) {
                STR_CAT(&new, p + 1);
                if (p > s) {
                    STR_NEXT(&new, '.');
                }
            }
        }
    }
    STR_CAT(&new, s);
    strcpy(s, new.p);
}

/*
 * rewrite-header - re-write an address in a header to be the
 * correct UK address, possibly widened and/or re-ordered from
 * what the user typed. Used for To: and Cc: fields.
 *
 * Under some circumstances smail seems to go round the loop
 * twice, so be careful not to do the job twice.
 */

static char *
rewrite_header(text, oldtarget, newtarget)
    char *text;
    char *oldtarget;
    char *newtarget;
{
    char *p = text;
    int oldlen = strlen(oldtarget);
    int newlen = strlen(newtarget);

    while (*p)
        {
            while (*p != 0 && *p != '@')
                p++;
            if (*p == 0)
                break;

            if (strncmpic(++p, oldtarget, oldlen) == 0)
                {
                    int c = p[oldlen];
                    if (c == 0 || c == ',' || c == '>' ||
                        c == ' ' || c == '\n' || c == '\t')
                        {
                            if (oldlen == newlen)
                                memcpy(p, newtarget, (size_t) newlen);
                            else
                                {
                                    int baselen = p - text;
                                    int oldtextlen = strlen(text);
                                    char *newtext = xmalloc(oldtextlen+1+(newlen-oldlen));
                                    memcpy(newtext, text, (size_t) baselen);
                                    memcpy(newtext+baselen, newtarget, (size_t) newlen);
                                    memcpy(newtext+baselen+newlen, p + oldlen,
                                           (size_t) (oldtextlen - baselen - oldlen + 1));
                                    xfree(text);
                                    text = newtext;
                                    p = text + baselen + newlen;
                                }
                        }
                }
        }

    return text;
}


/*
 * get_records - query the domain system for resource records of the given
 *               name.
 *
 * Send out a query and return the response packet in a passed buffer.
 * Resource records are in the bytes following the header for that
 * packet.  The actual size of the response packet is stored in pack_size.
 *
 * The passed answer buffer must have space for at least MAXPACKET
 * bytes of data.
 *
 * Return one of the following response codes:
 *
 *      DB_SUCCEED      We received an affirmative packet from the
 *                      server.
 *      DB_NOMATCH      We received a negative response from the server
 *                      indicating that the name does not exist.
 *      DB_FAIL         We received a negative response from the
 *                      server indicating some problem with the
 *                      packet.
 *      DB_AGAIN        We received a negative response from the
 *                      server indicating a temporary failure while
 *                      processing the name.
 *      FILE_NOMATCH    We could not connect to the server.
 *      FILE_AGAIN      There was a failure in the server, try again
 *                      later.
 */
static int
get_records(qname, qtype, answer, pack_size, error)
    char *qname;                        /* search for this name */
    int qtype;                          /* and for records of this type */
    register HEADER *answer;            /* buffer for storing answer */
    int *pack_size;                     /* store answer packet size here */
    char **error;                       /* store error message here */
{
#ifdef OBSOLETE_RESOLVER
    char msgbuf[MAXPACKET];
    int msglen;
#endif
    int anslen;

    *pack_size = 0;                     /* Ensure packet size is zero unless set */
#ifdef OBSOLETE_RESOLVER
    msglen = res_mkquery(QUERY, qname, C_IN, qtype, (char *)NULL, 0,
                         (struct rrec *)NULL, msgbuf, MAXPACKET);

    anslen = res_send(msgbuf, msglen,  (char *)answer, MAXPACKET);
    if (anslen < 0) {
        return FILE_NOMATCH;
    }
#else                                   /* not OBSOLETE_RESOLVER */
    anslen = res_search(qname, C_IN, qtype, (u_char *)answer, MAXPACKET);
    if (anslen < 0) {
        switch (h_errno) {
        case NO_DATA:
            return DB_SUCCEED;

        case HOST_NOT_FOUND:
            return DB_NOMATCH;

        case TRY_AGAIN:
            *error = "Nameserver: Server failure";
            return DB_AGAIN;

        case NO_RECOVERY:
            *error = "Irrecoverable nameserver error";
            return FILE_NOMATCH;

        default:
            *error = "Unknown nameserver error";
            return FILE_NOMATCH;
        }
    }
#endif                                  /* not OBSOLETE_RESOLVER */
    *pack_size = anslen;

    answer->qdcount = ntohs(answer->qdcount);
    answer->ancount = ntohs(answer->ancount);
    answer->nscount = ntohs(answer->nscount);
    answer->arcount = ntohs(answer->arcount);

    switch (answer->rcode) {

      case NOERROR:
        return DB_SUCCEED;
#ifdef OBSOLETE_RESOLVER
      case FORMERR:
        *error = "Nameserver: Format error in packet";
        return DB_FAIL;

      case SERVFAIL:
        *error = "Nameserver: Server failure";
        return DB_AGAIN;

      case NXDOMAIN:
        return DB_NOMATCH;

      case NOTIMP:
        *error = "Nameserver: Unimplemented request";
        return DB_FAIL;

      case REFUSED:
        *error = "Nameserver: Query refused";
        return FILE_AGAIN;
#endif                                  /* OBSOLETE_RESOLVER */
      default:
        *error = "Nameserver: Unknown response code";
        return DB_FAIL;
    }
}

/*
 * getnextrr - get a sequence of resource records from a name server
 *             response packet.
 *
 * The first time getnextrr() is called to process a packet, pass it
 * the header address of the packet.  For subsequent calls pass NULL.
 * When no more records remain, getnexrr() returns NULL.
 *
 * To process a specific response section, pass the section in sect.
 */
static RR *
getnextrr(it, additional)
     register struct rr_iterator *it;   /* iteration variables */
     int additional;                    /* interested in additional section RRs */
{
    int dnamelen;
    register unsigned char *dp;

    dp = (unsigned char *)it->dp;

    /* return NULL if no rr's remain */
    if (it->ancount != 0) {
        --it->ancount;
        it->rr.rr_sect = SECT_AN;
    } else if (additional && it->nscount != 0) {
        --it->nscount;
        it->rr.rr_sect = SECT_NS;
    } else if (additional && it->arcount != 0) {
        --it->arcount;
        it->rr.rr_sect = SECT_AR;
    } else {
        return NULL;
    }

    dnamelen = dn_expand((unsigned char *)it->hp, (unsigned char *)it->eom, 
                         (unsigned char *)dp, (unsigned char *)it->dname, MAXDNAME);
    if (dnamelen < 0) {
        return NULL;
    }
    dp += dnamelen;
    GETSHORT(it->rr.rr_type, dp);       /* extract type from record */
    GETSHORT(it->rr.rr_class, dp);      /* extract class */
    dp += 4;                            /* skip time to live */
    GETSHORT(it->rr.rr_size, dp);       /* extract length of data */
    it->rr.rr_data = (char *)dp;        /* there is the data */
    it->dp = (char *)(dp + it->rr.rr_size); /* skip to next resource record */

    return &it->rr;
}

static void
rewindrr(it, hp, packsize)
    register struct rr_iterator *it;
    HEADER *hp;
    int packsize;
{
    int dnamelen;
    int qdcount;

    it->dp = (char *)(hp + 1);
    it->hp = hp;
    it->eom = (char *)hp + packsize;
    it->rr.rr_dname = it->dname;
    it->ancount = it->hp->ancount;
    it->nscount = it->hp->nscount;
    it->arcount = it->hp->arcount;
    qdcount = it->hp->qdcount;
    /* skip over questions */
    while (qdcount > 0) {
        dnamelen = dn_expand((unsigned char *)it->hp, (unsigned char *)it->eom, 
                             (unsigned char *)it->dp, (unsigned char *)it->dname, 
                             MAXDNAME);
        if (dnamelen < 0) {
            it->ancount = it->nscount = it->arcount = 0;
        }
        it->dp += dnamelen;
        it->dp += 4;                    /* skip over class and type */
        --qdcount;
    }
}


/*
 * find_a_records - look for an A record for the target
 *
 * Look for an A record for the target, and return an appropriate
 * response code:
 *
 *      DB_SUCCEED      An A record was found for the target.
 *      DB_NOMATCH      There was not an A record.
 *      DB_FAIL         There was a server error for this query.
 *      DB_AGAIN        There was a server error for this query, try
 *                      again later.
 *      FILE_AGAIN      Server error, try again later.
 *      FILE_NOMATCH    Could not connect to server.
 *
 * For response codes other than DB_SUCCEED and DB_NOMATCH, store an
 * error message.
 */
static int
find_a_records(mx_hint, target, error)
    struct mx_transport_hint * mx_hint;
    char *target;
    char **error;
{
    int success;
    HEADER *a_rrs;
    int a_size;
    struct rr_iterator a_it;
    RR *a_rr;
    int result = DB_NOMATCH;

    a_rrs = (HEADER *)xmalloc(MAXPACKET);
    success = get_records(target, T_A, a_rrs, &a_size, error);

    if (success != DB_SUCCEED) {
        xfree((char *)a_rrs);
        return success;
    }

    rewindrr(&a_it, a_rrs, a_size);
    while ((a_rr = getnextrr(&a_it, FALSE))!=0) {
        if (a_rr->rr_type == T_A) {
            result = DB_SUCCEED;
            add_a_hint(mx_hint, a_it.dname, a_rr->rr_data);
        }
    }
    xfree((char *)a_rrs);
    return result;
}

static int
decode_mx_rr(rr, it, name, precedence)
    RR *rr;
    struct rr_iterator *it;
    char **name;
    int *precedence;
{
    static char nambuf[MAXDNAME];
    unsigned char *s = (unsigned char *)rr->rr_data;
    int dlen;

    GETSHORT(*precedence, s);
    dlen = dn_expand((unsigned char *)it->hp, (unsigned char *)it->eom, 
                     (unsigned char *)s, (unsigned char *)nambuf, MAXDNAME);
    if (dlen < 0) {
        return DB_FAIL;
    }
    *name = nambuf;
    return DB_SUCCEED;
}


static struct error *
server_failure(what, error)
    char *what;
    char *error;                        /* additional error text */
{
    char *error_text;

    extern int errno;

    /*
     * ERR_164 - failure talking to BIND server
     *
     * DESCRIPTION
     *      An error occured when sending or receiving packets from
     *      the BIND server, or the server registered an error in the
     *      response packet.
     *
     * ACTIONS
     *      Actions depend upon the specific error.  Usually, a retry
     *      is attempted later.
     *
     * RESOLUTION
     *      Resolution depends upon the specific error.
     */
    error_text = xprintf("%s: BIND server failure: %s: %s",
                         what, error, strerror(errno));
    DEBUG1(DBG_DRIVER_LO, "%s\n", error_text);

    return note_error(ERR_NPOSTMAST | ERR_164, error_text);
}

static struct error *
packet_error(what, type, host)
    char *what;
    char *type;                         /* type name of packet */
    char *host;                         /* target host */
{
    char *error_text;

    /*
     * ERR_165 - response packet format error
     *
     * DESCRIPTION
     *      A format error was found in a packet returned by the BIND
     *      name server.
     *
     * ACTIONS
     *      Retry again later, in the hope that the problem will go
     *      away.
     *
     * RESOLUTION
     *      This is probably a bug in the nameserver.
     */
    error_text =
        xprintf("%s: BIND server format error in %s packet for %s",
                what, type, host);
    DEBUG1(DBG_DRIVER_LO, "%s\n", error_text);

    return note_error(ERR_165, error_text);
}

static struct error *
matched_local_host(what, host)
    char *what;
    char *host;                         /* target hostname */
{
    char *error_text;

    /*
     * ERR_169 - MX record points to local host
     *
     * DESCRIPTION
     *      The MX record for the target host points to the local
     *      host, but the local host is not prepared to handle this
     *      case.
     *
     * ACTIONS
     *      The domain database should probably be looked at.
     *
     * RESOLUTION
     *      The postmaster should probably look into the problem.
     */
    error_text = xprintf("%s: MX record for %s points to local host",
                         what, host);
    DEBUG1(DBG_DRIVER_LO, "%s\n", error_text);

    return note_error(ERR_NSOWNER | ERR_169, error_text);
}

static struct error *
no_transport_error(what, transport)
    char *what;
    char *transport;                    /* name of missing transport */
{
    char *error_text;

    /*
     * ERR_110 - no transport for router
     *
     * DESCRIPTION
     *      A format error was found in a packet returned by the BIND
     *      name server.
     *
     * ACTIONS
     *      Defer delivery.
     *
     * RESOLUTION
     *      This is a configuration bug.
     */
    error_text =
        xprintf("%s: no such transport %s", what, transport);
    DEBUG1(DBG_DRIVER_LO, "%s\n", error_text);

    return note_error(ERR_CONFERR | ERR_110, error_text);
}

static struct error *
open_failed(what, file, open_error)
    char *what;
    char *file;
    char *open_error;
{
    char *error_text;

    /*
     * ERR_176 - failed to open greymail hosts database
     *
     * DESCRIPTION
     *      open_database() failed to open a greymail hosts database.  The
     *      error encountered should be stored in errno.
     *
     * ACTIONS
     *      Defer all of the input addresses as configuration errors.
     *
     * RESOLUTION
     *      The postmaster should check the director entry against the
     *      database he wishes to use.
     */
    error_text = xprintf("%s: greymail hosts database %s, open failed: %s",
                         what, file, open_error);
    DEBUG1(DBG_DRIVER_LO, "%s\n", error_text);

    return note_error(ERR_CONFERR | ERR_176, error_text);
}

static struct error *
cname_loop_error(what, host)
    char *what;
    char *host;                         /* target hostname */
{
    char *error_text;

    /*
     * ERR_177 - CNAME loop detected
     *
     * DESCRIPTION
     *      One CNAME points to another etc up to the maximum limit
     *      that this system is allowed to take (configurable).
     *      
     *
     * ACTIONS
     *      The domain database should probably be looked at.
     *
     * RESOLUTION
     *      The postmaster should probably look into the problem.
     */
    error_text = xprintf("%s: CNAME loop detected in DNS for %s.",
                         what, host);
    DEBUG1(DBG_DRIVER_LO, "%s\n", error_text);

    return note_error(ERR_NPOSTMAST | ERR_177, error_text);
}


static struct error *
res_init_error(what)
    char *what;
{
    char *error_text;

    /*
     * ERR_180 - DNS/BIND res_init() failed
     *
     * DESCRIPTION
     *      An attempt to use res_init() to setup the BIND resolver
     *      failed.
     *      
     *
     * ACTIONS
     *      There is something badly wrong with the resolver
     *
     * RESOLUTION
     *      The postmaster should probably look into the problem.
     */
    error_text = xprintf("%s: DNS/BIND res_init() failed - unable to use resolver",
                         what);
    DEBUG1(DBG_DRIVER_LO, "%s\n", error_text);

    return note_error(ERR_NPOSTMAST | ERR_180, error_text);
}



static struct transport_hints *
new_mx_hint(pref, dname, implicit)
    int pref;
    char * dname;
    int implicit;
{
    struct transport_hints * new_hint;
    struct mx_transport_hint * mx_hint;

    new_hint = (struct transport_hints *) xmalloc(sizeof(*new_hint));
    mx_hint = (struct mx_transport_hint *) xmalloc(sizeof(*mx_hint));
    new_hint->succ = (struct transport_hints *) 0;
    new_hint->hint_name = "mx";
    new_hint->private = (char *) mx_hint;
    mx_hint->preference = pref;
    mx_hint->exchanger = dname;
    mx_hint->implicit = implicit;
    mx_hint->ipaddrs = (struct ipaddr_hint *) 0;
    return new_hint;
}



static void
add_mx_hint(addr, precedence, name, implicit)
    struct transport_hints ** addr;
    int precedence;
    char * name;
    int implicit;
{
    struct transport_hints * new_hint;

    new_hint = new_mx_hint(precedence, COPY_STRING(name), implicit);
#define mxhint(hint) ((struct mx_transport_hint *)((hint)->private))
    while (*addr && (! EQ("mx",(*addr)->hint_name)
                     || mxhint(*addr)->preference < precedence))
    {
        addr = &(*addr)->succ;
    }
#undef mxhint
    new_hint->succ = *addr;
    *addr = new_hint;
}

static void
add_a_hint(mx_hint, hostname, address)
    struct mx_transport_hint *mx_hint;
    char *hostname;
    char *address;
{
    struct ipaddr_hint * a_hint, ** ah;

    a_hint = (struct ipaddr_hint *) xmalloc(sizeof(*a_hint));
    a_hint->succ = NULL;
    a_hint->hostname = COPY_STRING(hostname);
    memcpy((char *) &(a_hint->addr), address, sizeof(a_hint->addr));

    ah = &mx_hint->ipaddrs;
    while (*ah) {
        ah = &(*ah)->succ;
    }
    (*ah) = a_hint;
}



static void
free_mx_hint(hint_p)
    struct transport_hints ** hint_p;
{
    struct transport_hints * next = (*hint_p)->succ;
    struct ipaddr_hint * addr, * next_addr;

#define mxhint ((struct mx_transport_hint *)((*hint_p)->private))
    for (addr = mxhint->ipaddrs; addr; addr = next_addr) {
        next_addr = addr->succ;
        xfree(addr->hostname);
        xfree((char *)addr);
    }
    xfree(mxhint->exchanger);
    xfree((char *)mxhint);
#undef mxhint
    xfree((char *)*hint_p);
    *hint_p = next;
}


/*
 * match_full_or_end_domain - try to match one of a list of domains against a target
 *
 * given a list of domains separated by colons, determine if the given
 * target ends in, or is equal to one of those domains.  If so, return a 
 * pointer to the first character of the matched domain, else return NULL.
 * The list is scanned from left to right, with the first match returned.
 ****
 * Added functionality.  A domain spec *may* start with a ! character, in
 * which case NULL is returned if it *does* match.  This allows a specific
 * subdomain to be excluded from a more general match
 */
static char *
match_full_or_end_domain(domains, target)
    char *domains;                      /* colon separated list of domains */
    char *target;                       /* target to test against */
{
    register char *cur;                 /* current domain being checked */
    register int negmatch;	        /* are we doing negative matching */
    char * match;                       /* the actual match */

    if (!domains)
        return NULL;

    for (cur = strcolon(domains); cur; cur = strcolon((char *)NULL)) {
	negmatch = (*cur == '!') && cur++;
        match = is_suffix(cur, target, FALSE);
        if (match != NULL) {
            return (negmatch ? NULL : match);
        }
    }

    /* did not end in one of the domains */
    return NULL;
}

#endif /* HAVE_BIND */
