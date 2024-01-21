/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */

/* Header for the domainlist router */

/* Structure for the private options. */

typedef struct {
  int   search_type;
  int   partial_match;
  int   modemask;
  int  *owners;
  int  *owngroups;
  char *route_file;
  char *route_query;
  char *route_list;
  re_block *re_list;
} domainlist_router_options_block;

/* Data for reading the private options. */

extern optionlist domainlist_router_options[];
extern int domainlist_router_options_count;

/* Block containing default values. */

extern domainlist_router_options_block domainlist_router_option_defaults;

/* The main and initialization entry points for the router */

extern int domainlist_router_entry(router_instance *, address_item *,
  address_item **, address_item **);
extern void domainlist_router_init(router_instance *);

/* End of routers/domainlist.h */
