/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */

/* All the global variables are defined together in this one module, so that
they are easy to find. */

#include "exim.h"


/* The OSF1 linker puts out a worrying warning if any sections contain no
executable code. It says

Warning: Linking some objects which contain exception information sections
        and some which do not. This may cause fatal runtime exception handling
        problems.

As this may cause people to worry needlessly, include a dummy function here
to stop the message from appearing. Make it reference itself to stop picky
compilers complaining that it is unused. */

static void dummy(void) { dummy(); }


/* For comments on these variables, see globals.h. I'm too idle to
duplicate them here... */

BOOL   accept_8bitmime        = FALSE;
int    accept_timeout         = 0;
char  *address_file_transport = "address_file";
char  *address_pipe_transport = "address_pipe";
char  *address_reply_transport = "address_reply";
BOOL   address_test_mode      = FALSE;
BOOL   admin_user             = FALSE;
BOOL   always_bcc             = FALSE;
int    auto_thaw              = 0;

int    batch_max              = -1;
char  *bi_command             = NULL;
char  *big_buffer             = NULL;
int    big_buffer_size        = BIG_BUFFER_SIZE;

BOOL   check_relay            = FALSE;
BOOL   collapse_source_routes = FALSE;
BOOL   config_changed         = FALSE;
FILE  *config_file            = NULL;
char  *config_filename        = CONFIGURE_FILE
			"\0<-----------Space to patch configure_filename->";
int    config_lineno          = 0;
char  *continue_hostname      = NULL;
int    continue_sequence      = 1;
char  *continue_transport     = NULL;

BOOL   daemon_listen          = FALSE;
off_t  data_start_offset      = MESSAGE_ID_LENGTH + 3;
FILE  *debug_file             = NULL;
int    debug_level            = 0;
pid_t  debug_pid              = 0;
BOOL   debug_trace_memory     = FALSE;
transport_info *debug_transport = NULL;
char  *debug_transport_file   = NULL;
int    delay_warning          = 24*60*60;
BOOL   delivery_date_remove   = TRUE;
char  *deliver_in_buffer      = NULL;
char  *deliver_out_buffer     = NULL;
int    deliver_datafile       = -1;
char  *deliver_domain         = NULL;
char  *deliver_domain_orig    = NULL;
BOOL   deliver_force          = FALSE;
BOOL   deliver_freeze         = FALSE;
int    deliver_frozen_at      = 0;
char  *deliver_home           = NULL;
char  *deliver_host           = NULL;
int    deliver_load_max       = -1;
char  *deliver_localpart      = NULL;
char  *deliver_localpart_orig = NULL;
char  *deliver_localpart_prefix = NULL;
char  *deliver_localpart_suffix = NULL;
BOOL   deliver_manual_thaw    = FALSE;
address_item *deliver_recipients = NULL;
char  *deliver_selectstring   = NULL;
director_instance *directors  = NULL;

director_instance director_defaults = {
   NULL,                      /* chain pointer */
   NULL,                      /* name */
   NULL,                      /* info */
   NULL,                      /* private options block pointer */
   NULL,                      /* driver name */
   NULL,                      /* prefix */
   NULL,                      /* suffix */
   NULL,                      /* domains */
   NULL,                      /* except_domains */
   NULL,                      /* local_parts */
   NULL,                      /* except_local_parts */
   NULL,                      /* require_files */
   NULL,                      /* re_domains */
   NULL,                      /* re_except_domains */
   NULL,                      /* re_local_parts */
   NULL,                      /* re_except_local_parts */
   NULL,                      /* transport */
   TRUE,                      /* more */
   FALSE,                     /* unseen */
   FALSE,                     /* prefix_optional */
   FALSE,                     /* suffix_optional */
   TRUE,                      /* verify_sender */
   TRUE,                      /* verify_recipient */
   FALSE,                     /* fail_verify_sender */
   FALSE,                     /* fail_verify_recipient */
   FALSE                      /* verify_only */
};

int    dns_retrans            = 0;
int    dns_retry              = 0;
BOOL   dont_deliver           = FALSE;
BOOL   dot_ends               = TRUE;

BOOL   envelope_to_remove     = TRUE;
int    errno_quota            = ERRNO_QUOTA;
char  *errors_copy            = NULL;
int    error_handling         = ERRORS_SENDER;
char  *errors_address         = "postmaster";
char  *errors_reply_to        = NULL;

#ifdef EXIM_GID
gid_t  exim_gid               = EXIM_GID;
BOOL   exim_gid_set           = TRUE;
#else
gid_t  exim_gid               = 0;
BOOL   exim_gid_set           = FALSE;
#endif

char  *exim_path              = BIN_DIRECTORY "/exim"
			"\0<---------------Space to patch exim_path->";

#ifdef EXIM_UID
uid_t  exim_uid               = EXIM_UID;
BOOL   exim_uid_set           = TRUE;
#else
uid_t  exim_uid               = 0;
BOOL   exim_uid_set           = FALSE;
#endif

int    expand_nlength[EXPAND_MAXN+1];
int    expand_nmax            = -1;
char  *expand_nstring[EXPAND_MAXN+1];
BOOL   expand_string_forcedfail = FALSE;
char  *expand_string_message;

char  *filter_test            = NULL;
int    finduser_retries       = 0;
BOOL   forbid_domain_literals = FALSE;
BOOL   freeze_tell_mailmaster = FALSE;

char  *gecos_name             = NULL;
char  *gecos_pattern          = NULL;

BOOL   have_nis               = HAVE_NIS;
BOOL   have_nisplus           = HAVE_NISPLUS;
BOOL   have_seteuid           = HAVE_SETEUID || HAVE_SETRESUID;
BOOL   header_changed         = FALSE;
header_line *header_last      = NULL;
header_line *header_list      = NULL;
header_name *header_names     = NULL;

header_name header_names_normal[] = {
  { "Bcc",         3 },       /* This data must be in the correct */
  { "Cc",          2 },       /* order as defined by the enum for */
  { "Date",        4 },       /* the hn_xxx offsets. */
  { "From",        4 },
  { "Message-Id", 10 },
  { "Sender",      6 },
  { "To",          2 },
  { "Reply-To",    8 },
  { "Subject",     7 }
};

header_name header_names_resent[] = {
  { "Resent-Bcc",        10 }, /* This data must be in the correct */
  { "Resent-Cc",          9 }, /* order as defined by the enum for */
  { "Resent-Date",       11 }, /* the hn_xxx offsets. */
  { "Resent-From",       11 },
  { "Resent-Message-Id", 17 },
  { "Resent-Sender",     13 },
  { "Resent-To",          9 },
  { "Resent-Reply-To",   15 },
  { "Resent-Subject",    14 }
};

char  *helo_verify_nets       = NULL;
ip_net_item *helo_verify_netlist = NULL;
char  *hex_digits             = "0123456789abcdef";

BOOL   ignore_errmsg_errors   = FALSE;
int    journal_fd             = -1;

int    keep_malformed         = 4*24*60*60;    /* 4 days */
BOOL   kill_ip_options        = TRUE;

int    load_average           = -2;
char  *local_domains          = NULL;
BOOL   local_domains_include_host = FALSE;
BOOL   local_domains_include_host_literals = FALSE;
BOOL   local_error_message    = FALSE;
char  *local_interfaces       = NULL;
ip_address_item *local_interface_data = NULL;
BOOL   locally_caseless       = TRUE;
char  *log_buffer             = NULL;
char  *log_file_path          = LOG_FILE_PATH
			  "\0<--------------Space to patch log_file_path->";
BOOL   log_ip_options         = TRUE;
int    log_level              = 5;
BOOL   log_received_recipients = FALSE;
BOOL   log_smtp_confirmation = FALSE;
BOOL   log_subject = FALSE;
char  *lookup_key = NULL;

/* The first argument in this vector is set to exim_path after the
configuration file has been read and exim_path is fixed. This argument
list is used for generating error messages. */

char  *mailer_argv[]          = { NULL, "-t", "-oem", "-oi", "-f", "<>",
                                  (char *)0,    /* -E<id> is added here */
                                  (char *)0,    /* -d<n> may be added */
                                  (char *)0,    /* -N may be added */
                                  (char *)0,    /* -C and its argument may */
                                  (char *)0,    /*    be added */
                                  (char *)0 };  /* the terminating zero */
char  *message_body           = NULL;
int    message_body_visible   = 500;
char  *message_filter         = NULL;
gid_t  message_filter_gid     = 0;
BOOL   message_filter_gid_set = FALSE;
uid_t  message_filter_uid     = 0;
BOOL   message_filter_uid_set = FALSE;
char  *message_id;
char  *message_id_text        = NULL;
char   message_id_option[MESSAGE_ID_LENGTH + 3];
char  *message_id_external;
FILE  *message_log            = NULL;
char  *message_precedence     = NULL;
int    message_size           = 0;
int    message_size_limit     = 0;
char  *message_reference      = NULL;

regexp *net_regexp;
uid_t *never_users            = NULL;
gid_t  nobody_gid             = 0;
BOOL   nobody_gid_set         = FALSE;
uid_t  nobody_uid             = 0;
BOOL   nobody_uid_set         = FALSE;

gid_t  originator_gid;
char  *originator_login;
uid_t  originator_uid;

BOOL   parse_allow_group      = FALSE;
BOOL   parse_found_group      = FALSE;
char  *percent_hack_domains   = NULL;
char  *pid_file_path          = PID_FILE_PATH
			  "\0<--------------Space to patch pid_file_path->";
BOOL   preserve_message_logs  = FALSE;
char  *primary_hostname       = NULL;
char   process_info[256];

char  *qualify_domain_recipient = NULL;
char  *qualify_domain_sender  = NULL;
BOOL   queue_run_force        = FALSE;
BOOL   queue_run_local        = FALSE;
int    queue_interval         = -1;
BOOL   queue_only             = FALSE;
int    queue_only_load        = -1;
BOOL   queue_remote           = FALSE;
int    queue_run_max          = 5;
BOOL   queue_smtp             = FALSE;

char **raw_recipients         = NULL;

re_block *re_local_domains    = NULL;
re_block *re_percent_hack_domains = NULL;
re_block *re_relay_domains    = NULL;
re_block *re_remote_sort      = NULL;
re_block *re_sender_accept    = NULL;
re_block *re_sender_accept_recipients = NULL;
re_block *re_sender_address_relay = NULL;
re_block *re_sender_reject    = NULL;
re_block *re_sender_reject_recipients = NULL;

gid_t  real_gid;
uid_t  real_uid;
BOOL   really_exim            = FALSE;
int    received_count         = 0;

/* This is the default text for Received headers generated by Exim. The
date will be automatically added on the end. */

char  *received_header_text   =
             "Received: "
             "${if def:sender_fullhost {from ${sender_fullhost} "
             "${if def:sender_ident {(${sender_ident})}}\n\t}"
             "{${if def:sender_ident {from ${sender_ident} }}}}"
             "by ${primary_hostname} "
             "${if def:received_protocol {with ${received_protocol}}} "
             "(Exim ${version_number} #${compile_number})\n\t"
             "id ${message_id}"
	     "\0<---------------Space to patch received_header_text->";

int    received_headers_max   = 30;
char  *received_protocol      = NULL;
int    received_time          = 0;
BOOL   receiver_try_verify    = FALSE;
char  *receiver_unqualified_hosts = NULL;
host_item *receiver_unqualified_hostlist = NULL;
char  *receiver_unqualified_nets = NULL;
ip_net_item *receiver_unqualified_netlist = NULL;
BOOL   receiver_verify        = FALSE;
char  *receiver_verify_except_hosts = NULL;
host_item *receiver_verify_except_hostlist = NULL;
char  *receiver_verify_except_nets = NULL;
ip_net_item *receiver_verify_except_netlist = NULL;
int    recipients_count       = 0;
char **recipients_list        = NULL;
int    recipients_list_max    = 0;
int    recipients_max         = 0;
BOOL   refuse_all_rcpts       = FALSE;
BOOL   refuse_ip_options      = TRUE;
char **regcomp_error_pointer  = NULL;
char  *regexp_compiling       = NULL;
regexp *regexp_From           = NULL;
regexp *regexp_ip_address;
regexp *regexp_spoolfile;
char   *relay_domains         = NULL;
int    remote_max_parallel    = 1;
char   *remote_sort           = NULL;
int    retry_interval_max     = 24*60*60;
int    retry_maximum_timeout  = 0;        /* set from retry config */
retry_config *retries         = NULL;
char  *return_path            = NULL;
BOOL   return_path_remove     = TRUE;
int    return_size_limit      = 100*1024;
int    rewrite_existflags     = 0;
rewrite_rule *rewrite_rules   = NULL;
char  *rfc1413_except_hosts   = NULL;
host_item *rfc1413_except_hostlist = NULL;
char  *rfc1413_except_nets    = NULL;
ip_net_item *rfc1413_except_netlist = NULL;
int    rfc1413_query_timeout  = 60;
uid_t  root_uid               = ROOT_UID;
char  *route_option           = NULL;

router_instance *routers  = NULL;
router_instance router_defaults = {
   NULL,                      /* chain pointer */
   NULL,                      /* name */
   NULL,                      /* info */
   NULL,                      /* private options block pointer */
   NULL,                      /* driver name */
   NULL,                      /* transport instance */
   NULL,                      /* domains */
   NULL,                      /* except_domains */
   NULL,                      /* local_parts */
   NULL,                      /* except_local_parts */
   NULL,                      /* require_files */
   NULL,                      /* re_domains */
   NULL,                      /* re_except_domains */
   NULL,                      /* re_local_parts */
   NULL,                      /* re_except_local_parts */
   TRUE,                      /* more */
   FALSE,                     /* unseen */
   TRUE,                      /* verify_sender */
   TRUE,                      /* verify_recipient */
   FALSE,                     /* fail_verify_sender */
   FALSE,                     /* fail_verify_recipient */
   FALSE,                     /* verify_only */
   FALSE,                     /* pass_on_timeout */
   NULL,                      /* self */
   self_freeze,               /* self_code */
   FALSE                      /* self_rewrite */
};

int    security_level         = 0;
char  *security_type          = NULL;
char  *sender_accept          = NULL;
char  *sender_accept_recipients = NULL;
char  *sender_address         = NULL;
char  *sender_address_relay   = NULL;
char  *sender_fullhost        = NULL;
char  *sender_host_accept     = NULL;
host_item *sender_host_accept_hosts = NULL;
char  *sender_host_accept_relay = NULL;
host_item *sender_host_accept_relay_hosts = NULL;
char  *sender_host_address    = NULL;
char  *sender_host_name       = NULL;
char  *sender_host_reject     = NULL;
host_item *sender_host_reject_hosts = NULL;
char  *sender_host_reject_recipients = NULL;
host_item *sender_host_reject_recipients_hosts = NULL;
char  *sender_host_reject_relay = NULL;
host_item *sender_host_reject_relay_hosts = NULL;
BOOL   sender_host_unknown    = FALSE;
char  *sender_ident           = NULL;
BOOL   sender_local           = FALSE;
char  *sender_net_accept      = NULL;
ip_net_item *sender_net_accept_nets = NULL;
char  *sender_net_accept_relay = NULL;
ip_net_item *sender_net_accept_relay_nets = NULL;
char  *sender_net_reject      = NULL;
ip_net_item *sender_net_reject_nets = NULL;
char  *sender_net_reject_recipients = NULL;
ip_net_item *sender_net_reject_recipients_nets = NULL;
char  *sender_net_reject_relay = NULL;
ip_net_item *sender_net_reject_relay_nets = NULL;
char  *sender_reject          = NULL;
char  *sender_reject_recipients = NULL;
BOOL   sender_try_verify      = FALSE;
char  *sender_unqualified_hosts = NULL;
host_item *sender_unqualified_hostlist = NULL;
char  *sender_unqualified_nets = NULL;
ip_net_item *sender_unqualified_netlist = NULL;
BOOL   sender_verify          = FALSE;
char  *sender_verify_except_hosts = NULL;
host_item *sender_verify_except_hostlist = NULL;
char  *sender_verify_except_nets = NULL;
ip_net_item *sender_verify_except_netlist = NULL;
BOOL   sender_verify_fixup    = FALSE;
BOOL   sender_verify_log_details = FALSE;
BOOL   sender_verify_reject   = TRUE;
char **sighup_argv            = NULL;
int    smtp_accept_count      = 0;
int    smtp_accept_max        = 20;
int    smtp_accept_queue      = 0;
int    smtp_accept_reserve    = 0;
char  *smtp_banner            = "${primary_hostname} Exim ${version_number} "
                            "#${compile_number} ready at ${tod_full}"
			    "\0<---------------Space to patch smtp_banner->";
BOOL   smtp_batched_input     = FALSE;
int    smtp_connect_backlog   = 5;
BOOL   smtp_input             = FALSE;
int    smtp_load_reserve      = -1;
int    smtp_port              = -1;
int    smtp_receive_timeout   = 5*60;
host_item *smtp_etrn_hostlist = NULL;
char  *smtp_etrn_hosts        = NULL;
ip_net_item *smtp_etrn_netlist = NULL;
char  *smtp_etrn_nets         = NULL;
host_item *smtp_reserve_hostlist = NULL;
char  *smtp_reserve_hosts     = NULL;
ip_net_item *smtp_reserve_netlist = NULL;
char  *smtp_reserve_nets      = NULL;
BOOL   smtp_verify            = FALSE;
char  *spool_directory        = SPOOL_DIRECTORY
			  "\0<--------------Space to patch spool_directory->";
char  *stderr_filename        =
#ifdef STDERR_FILE
  STDERR_FILE             "\0<--------------Space to patch stderr_filename->";
#else
  NULL;
#endif

char   string_return_buffer[STRING_RETURN_SIZE];
BOOL   strip_excess_angle_brackets = FALSE;
BOOL   strip_trailing_dot     = FALSE;

transport_instance *transport_address_file = NULL;
transport_instance *transport_address_pipe = NULL;
transport_instance *transport_address_reply = NULL;
transport_instance *transports = NULL;

transport_instance transport_defaults = {
   NULL,                     /* chain pointer */
   NULL,                     /* name */
   NULL,                     /* info */
   NULL,                     /* private options block pointer */
   NULL,                     /* driver name */
   NULL,                     /* setup entry point */
   local_batch_off,          /* local_batch */
   local_smtp_off,           /* local_smtp */
   100,                      /* batch_max */
   FALSE,                    /* uid_set */
   FALSE,                    /* gid_set */
   -1,                       /* uid */
   -1,                       /* gid */
   NULL,                     /* expand_uid */
   NULL,                     /* expand_gid */
   NULL,                     /* home_dir */
   NULL,                     /* current_dir */
   FALSE,                    /* deliver_as_creator */
   FALSE,                    /* initgroups */
   TRUE,                     /* multi-domain */
   FALSE,                    /* return_output */
   FALSE,                    /* return_fail_output */
   FALSE,                    /* log_output */
   FALSE,                    /* log_fail_output */
   2,                        /* BOOL, but set neither 1 nor 0 so can detect */
   100                       /* max_addresses */
};

int    transport_chunk_timeout= 0;
volatile BOOL transport_sigalrm_seen = FALSE;

tree_node *tree_duplicates    = NULL;
tree_node *tree_nonrecipients = NULL;
tree_node *tree_unusable      = NULL;

BOOL   trusted_caller         = FALSE;
gid_t *trusted_groups         = NULL;
uid_t *trusted_users          = NULL;

char  *unknown_login          = NULL;
char  *unknown_username       = NULL;
char  *user_login             = NULL;
char  *user_name              = NULL;
BOOL   user_null_sender       = FALSE;

BOOL   verify_only            = FALSE;
char  *version_copyright      = "Copyright (c) University of Cambridge 1997";
char  *version_date           = "?";
char  *version_cnumber        = "????";
char  *version_string         = "?";

int    warning_count          = 0;

/* End of globals.c */
