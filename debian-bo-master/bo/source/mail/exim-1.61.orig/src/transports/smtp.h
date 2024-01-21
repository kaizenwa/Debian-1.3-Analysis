/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */

/* Private structure for the private options and other private data. */

typedef struct {
  char *hosts;
  char *fallback_hosts;
  host_item *hostlist;
  host_item *fallback_hostlist;
  char *service;
  char *mx_domains;
  char *non_mx_domains;
  re_block *re_mx_domains;
  re_block *re_non_mx_domains;
  char *serialize_hosts;
  re_block *re_serialize_hosts;
  char *serialize_nets;
  ip_net_item *serialize_netlist;
  int   batch_max;
  int   command_timeout;
  int   connect_timeout;
  int   data_timeout;
  int   final_timeout;
  BOOL  gethostbyname;
  BOOL  dns_qualify_single;
  BOOL  dns_search_parents;
  BOOL  delay_after_cutoff;
} smtp_transport_options_block;

/* Data for reading the private options. */

extern optionlist smtp_transport_options[];
extern int smtp_transport_options_count;

/* Block containing default values. */

extern smtp_transport_options_block smtp_transport_option_defaults;

/* The main, init, and closedown entry points for the transport */

extern void smtp_transport_entry(transport_instance *, address_item *);
extern void smtp_transport_init(transport_instance *);
extern void smtp_transport_closedown(transport_instance *);

/* End of transports/smtp.h */
