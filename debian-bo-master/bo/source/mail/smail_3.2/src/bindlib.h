/* @(#)src/bindlib.h bindlib.h,v 1.6 1995/02/24 18:26:00 nm4 Exp */

/*
 *    Copyright (C) 1992  Ronald S. Karr
 *
 * BIND support common to the "bind" router and the "tcpsmtp" transport.
 * Created by Simon Leinen (simon@liasun6) on 9 July 1991.
 *
 * Converted to library form usable by tcpsmtp by Chip Salzenberg.
 * Modifications to work with rewritten bindlib.c by Nigel Metheringham.
 * Added mx_domains option - Philip Hazel, October 1993 
 *
 */

#ifndef BINDLIB_H
#define BINDLIB_H

/* configuration items shared by bind router and tcpip transport */

/* flag attributes */
#define BIND_LOCAL_MX_OKAY   0x01000000 /* MX RR can point to local host */
#define BIND_DEFER_NO_CONN   0x02000000 /* defer if cannot connect to server */
#define BIND_DEFNAMES        0x04000000 /* append default domain name */
#define BIND_DOMAIN_REQUIRED 0x08000000 /* require two hostname parts */
#define BIND_MX_ONLY         0x10000000 /* require MX records */
#define BIND_UK_TRY_INVERT   0x20000000 /* try inverting UK address */
#define BIND_UK_GREY_WORLD   0x40000000 /* UK GreyBook addrs in world order */
#define BIND_DNS_SEARCH      0x80000000 /* search DNS names */

/* Levels of header rewriting */
#define BIND_REWRITE_NEVER      0       /* Never rewrite headers */
#define BIND_REWRITE_IFREQUIRED 1       /* Rewrite headers if necessary (inverted address) */
#define BIND_REWRITE_ALWAYS     2       /* Always rewrite headers */

#ifndef BIND_FALLBACK_PREFERENCE
#define BIND_FALLBACK_PREFERENCE 65535  /* Max 16 bit number */
#endif 

struct bindlib_private {
    char *match_domains;		/* domains to match */
    char *ignore_domains;               /* domains to ignore */
    char *required;                     /* domains to limit search to */
    char *widen_domains;                /* domains to try widening with */
    char *mx_domains;                   /* domains to use MX only with */ 
    char *gateways;                     /* known gateways and their domains */
    char *uk_ignore_gateways;           /* UK: gateways to ignore */
    char *uk_greybook_transport;        /* UK: null string => SMTP only */
    int  uk_max_precedence;             /* UK: max MX precedence to accept */
    char *uk_greybook_relay;            /* UK: Relay host if cannot greybook */
    char *uk_greybook_hosts_list;       /* UK: list of hosts I can gb to */
    char *uk_greybook_hosts_file;       /* UK: file of hosts I can gb to */
    char *uk_greybook_hosts_proto;      /* UK: proto for above */
    char *uk_greybook_host_retries;     /* UK: retries for above */
    char *uk_greybook_host_interval;    /* UK: interval for above */
    char *uk_suffix;                    /* UK: Suffix for UK addresses */
    int  rewrite_headers;               /* Amount of header rewriting to do */
    int  cname_limit;                   /* Depth of CNAMES allowed */
    char *fallback_gateway;             /* Fallback gateway if physical connect fails */
};

/* configuration description */
#define BIND_ATTRIBUTES(STRUCT,BL_PRIV_MEM) \
    { "local_mx_okay", t_boolean, NULL, NULL, BIND_LOCAL_MX_OKAY },     \
    { "defer_no_connect", t_boolean, NULL, NULL, BIND_DEFER_NO_CONN },  \
    { "defnames", t_boolean, NULL, NULL, BIND_DEFNAMES },               \
    { "dns_search", t_boolean, NULL, NULL, BIND_DNS_SEARCH },           \
    { "domain_required", t_boolean, NULL, NULL, BIND_DOMAIN_REQUIRED }, \
    { "mx_only", t_boolean, NULL, NULL, BIND_MX_ONLY },                 \
    { "uk_try_inverting", t_boolean, NULL, NULL, BIND_UK_TRY_INVERT },  \
    { "uk_greybook_worldorder", t_boolean, NULL, NULL, BIND_UK_GREY_WORLD }, \
    { "match_domains", t_string, NULL, NULL,				\
	  OFFSET(STRUCT, BL_PRIV_MEM.match_domains) },			\
    { "ignore_domains", t_string, NULL, NULL,                           \
          OFFSET(STRUCT, BL_PRIV_MEM.ignore_domains) },                 \
    { "required", t_string, NULL, NULL,                                 \
          OFFSET(STRUCT, BL_PRIV_MEM.required) },                       \
    { "widen_domains", t_string, NULL, NULL,                            \
          OFFSET(STRUCT, BL_PRIV_MEM.widen_domains) },                  \
    { "mx_domains", t_string, NULL, NULL,                               \
          OFFSET(STRUCT, BL_PRIV_MEM.mx_domains) },                     \
    { "gateways", t_string, NULL, NULL,                                 \
          OFFSET(STRUCT, BL_PRIV_MEM.gateways) },                       \
    { "uk_ignore_gateways", t_string, NULL, NULL,                       \
          OFFSET(STRUCT, BL_PRIV_MEM.uk_ignore_gateways) },             \
    { "uk_greybook_transport", t_string, NULL, NULL,                    \
          OFFSET(STRUCT, BL_PRIV_MEM.uk_greybook_transport) },          \
    { "uk_max_precedence", t_int, NULL, NULL,                           \
          OFFSET(STRUCT, BL_PRIV_MEM.uk_max_precedence) },              \
    { "uk_greybook_relay", t_string, NULL, NULL,                        \
          OFFSET(STRUCT, BL_PRIV_MEM.uk_greybook_relay) },              \
    { "uk_greybook_hosts_list", t_string, NULL, NULL,                   \
          OFFSET(STRUCT, BL_PRIV_MEM.uk_greybook_hosts_list) },         \
    { "uk_greybook_hosts_file", t_string, NULL, NULL,                   \
          OFFSET(STRUCT, BL_PRIV_MEM.uk_greybook_hosts_file) },         \
    { "uk_greybook_hosts_proto", t_string, NULL, NULL,                  \
          OFFSET(STRUCT, BL_PRIV_MEM.uk_greybook_hosts_proto) },        \
    { "uk_greybook_host_retries", t_string, NULL, NULL,                 \
          OFFSET(STRUCT, BL_PRIV_MEM.uk_greybook_host_retries) },       \
    { "uk_greybook_host_interval", t_string, NULL, NULL,                \
          OFFSET(STRUCT, BL_PRIV_MEM.uk_greybook_host_interval) },      \
    { "uk_suffix", t_string, NULL, NULL,                                \
          OFFSET(STRUCT, BL_PRIV_MEM.uk_suffix) },                      \
    { "rewrite_headers", t_int, NULL, NULL,                             \
          OFFSET(STRUCT, BL_PRIV_MEM.rewrite_headers) },                \
    { "cname_limit", t_int, NULL, NULL,                                 \
          OFFSET(STRUCT, BL_PRIV_MEM.cname_limit) },                    \
    { "fallback_gateway", t_string, NULL, NULL,                         \
          OFFSET(STRUCT, BL_PRIV_MEM.fallback_gateway) }                \

#define BIND_TEMPLATE_FLAGS 0

#define BIND_TEMPLATE_ATTRIBUTES \
   { NULL,                              /* char *match_domains */              \
     NULL,                              /* char *ignore_domains */              \
     NULL,                              /* char *required */                    \
     NULL,                              /* char *widen_domains */               \
     NULL,                              /* char *mx_domains */                  \
     NULL,                              /* char *gateways */                    \
     NULL,                              /* char *uk_ignore_gateways */          \
     NULL,                              /* char *uk_greybook_transport */       \
     0,                                 /* int  uk_max_precedence */            \
     NULL,                              /* char *uk_greybook_relay */           \
     NULL,                              /* char *uk_greybook_hosts_list */      \
     NULL,                              /* char *uk_greybook_hosts_file */      \
     NULL,                              /* char *uk_greybook_hosts_proto */     \
     NULL,                              /* char *uk_greybook_host_retries */    \
     NULL,                              /* char *uk_greybook_host_interval */   \
     ".uk",                             /* char *uk_suffix */                   \
     0,                                 /* int  rewrite_headers */              \
     10,                                /* int  cname_limit */                  \
     NULL                               /* char *fallback_gateway */            \
   }
#endif /* not BINDLIB_H */
