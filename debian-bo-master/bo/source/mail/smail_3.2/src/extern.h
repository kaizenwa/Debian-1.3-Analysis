/*
#ident	"@(#)smail/src:RELEASE-3_2:extern.h,v 1.47 1996/05/29 14:50:17 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * extern.h:
 *	externals used by the smail program
 */
extern int errno;			/* previous system error value */

/*
 * External variables used in the smail program
 */
/* external variables defined in alloc.c */
extern struct block *perm;		/* permanent allocation block */
extern int x_dont_panic;                /* dont panic in X_CHECK() */

/* external variables defined in config.c */
extern char *listen_name;		/* DNS name of interface to listen on */
extern char *visible_name;		/* hostname used in outgoing addrs */
extern char *visible_domains;		/* domain used in outgoing headers */
extern char *uucp_name;			/* hostname used in !-routes */
extern char *hostnames;			/* list of other local host names */
extern char *more_hostnames;		/* additional list of hostnames */
extern long max_message_size;		/* max size of message body */
extern char *grades;			/* mapping of precedence to grade */
extern int nobody_uid;			/* user id for access permission */
extern int nobody_gid;			/* group id for access permission */
extern char *nobody;			/* name of nobody user */
extern char *log_fn;			/* name of information log file */
extern char *panic_fn;			/* name of panic log file */
extern char *cons_fn;			/* name of console device file */
extern char *spool_dirs;		/* names of spooling directories */
extern int spool_mode;			/* mode for spool files */
extern int lock_mode;			/* mode for lock files */
extern int log_mode;			/* mode for system log files */
extern int message_log_mode;		/* mode for per-message log files */
extern int spool_grade;			/* default spool grade character */
extern int open_retries;		/* max open retries on startup file */
extern int open_interval;		/* sleep between open retries */
extern int min_runq_grade;		/* minimum grade to process in queue run */
extern int max_runq_grade;		/* maximum grade to process in queue run */
extern int min_delivery_grade;		/* minimum grade to deliver */
extern int max_delivery_grade;		/* maximum grade to deliver */
extern char *config_file;		/* config file name */
extern char *director_file;		/* directors file name */
extern char *router_file;		/* routers file name */
extern char *method_dir;		/* directory for non-/ method files */
extern char *transport_file;		/* transports file name */
extern char *qualify_file;              /* domain qualification file name */
extern char *retry_file;                /* address retry control file name */
extern char *smail_lib_dir;		/* default config file directory */
extern char *smail_util_dir;		/* default smail utility directory */
extern char *received_field;		/* Received: field string */
extern char *message_id_field;		/* Message_Id: field string */
extern char *date_field;		/* Date: field string */
extern char *from_field;		/* From: field string */
extern char *return_path_field;		/* Return-Path: field string */
extern char *smail;			/* location of the smail program */
extern double max_load_ave;		/* spool mail > this load agerage */
extern char *trusted;			/* : list of trusted users */
extern char *trusted_groups;		/* : list of trusted groups */
extern unsigned message_bufsiz;		/* size of message buffers */
extern int hit_table_len;		/* #entries in address hit table */
extern int flock_mailbox;		/* TRUE to use lock_fd_wait() macro */
extern int fnlock_retries;		/* retries for lock_file() creat */
extern int fnlock_interval;		/* retry intervals for lock_file() */
extern int fnlock_mode;			/* mode for lock_file() lockfiles */
extern int lock_by_name;		/* TRUE to use spool lockfiles */
extern int queue_only;			/* true to queue but not deliver */
extern int max_hop_count;		/* fail if hop_count exceeds this */
extern char *delivery_mode_string;	/* string naming delivery mode */
extern char *delivery_grades;		/* the msg grade range to be delivered */
extern char *runq_grades;		/* the msg grade range in runq */
extern char *smart_user;		/* default user for smartuser driver */
extern char *smart_path;		/* default path for smarthost driver */
extern char *smart_transport;		/* transport for smarthost driver */
extern char *second_config_file;	/* secondary configuration file */
extern char *copying_file;		/* pathname to COPYING file */
extern int auto_mkdir;			/* TRUE to auto create directories */
extern int auto_mkdir_mode;		/* the mode for auto directories */
extern int require_configs;		/* TRUE to require config files */
extern char *postmaster_address;	/* default addr of postmaster */
extern char *smtp_banner;		/* smtp startup banner message */
extern int smtp_accept_max;		/* max simultaneous SMTPs to accept */
extern int smtp_accept_queue;		/* simultaneous SMTPs until queueonly */
extern int smtp_debug;			/* allow DEBUG command in SMTP */
extern int smtp_info;			/* allow EXPN/VRFY commands in SMTP */
extern char *sender_env_variable;	/* env variable naming user */
extern int switch_percent_and_bang;	/* switch precedence of % and ! */
extern int error_copy_postmaster;	/* copy postmaster on errors */
extern long retry_interval;		/* default delivery retry interval */
extern long retry_duration;		/* default delivery retry duration */
extern long host_lock_timeout;		/* timeout for host exclusivity lock */
extern long smtp_receive_command_timeout; /* timeout for receiving SMTP cmds */
extern long smtp_receive_message_timeout; /* timeout for receiving SMTP msg */
extern char *auth_domains;		/* authoritative domain list */
extern long rfc1413_query_timeout;	/* Timeout on RFC1413 queries - initially disabled */
extern long resolve_timeout;		/* timeout on directors/routers */

/* external variables defined in default.c */
extern struct director *directors;	/* configured directors */
extern struct router *routers;		/* configured routers */
extern struct transport *transports;	/* configured transports */
extern struct transport *builtin_transports; /* builtin transports */

/* external variables defined in direct.c */
extern int cached_directors;		/* TRUE if cache_directors() called */

/* external variables defined in drivertab.c */
#ifdef DIRECT_H
extern struct direct_driver direct_drivers[]; /* compiled director drivers */
#endif
#ifdef ROUTE_H
extern struct route_driver route_drivers[]; /* compiled router drivers */
#endif
#ifdef TRANSPORT_H
extern struct transport_driver transport_drivers[]; /* transport drivers */
#endif

/* external variables defined in header.c */
extern struct list *header;		/* list of header fields */

/* external variables defined in ldinfo.c */
extern int compile_num;
extern char *compile_date;

/* external variables defined in log.c */
extern FILE *msg_logfile;		/* open stream to per-message log */

/* external variables defined in main.c */
extern int islocal;			/* TRUE if mail originated locally */
extern int exitvalue;			/* call exit with this value */
extern char *program;			/* argv[0] from main */
extern char *sender;			/* sender of message */
extern char *local_sender;		/* local sender of message */
extern int error_sender;		/* TRUE if special sender <> given */
extern char *sender_name;		/* full name of sender */
extern int debug;			/* debugging level, 0 is off */
extern int dont_deliver;		/* if TRUE, don't actually deliver */
extern int process_queue;		/* process spooled files */
extern unsigned queue_interval;		/* process queues at this interval */
extern int hop_count;			/* hop count so far for message */
extern int do_aliasing;			/* do aliasing for local addresses */
extern int extract_addresses;		/* get recipients from header */
#ifdef MAIN_H
extern enum op_mode operation_mode;	/* mode of operation */
extern enum dot_usage dot_usage;	/* how do we treat . on input */
extern enum deliver_mode deliver_mode;	/* foreground, background or queued */
extern enum er_proc error_processing;	/* method of displaying errors */
extern enum prog_type prog_type;	/* type of program we are running as */
#endif	/* MAIN_H */
extern int me_too;			/* sender allowed in aliases */
extern struct addr *recipients;		/* list of recipient addresses */
extern char *primary_name;		/* primary local name from hostnames */
extern FILE *errfile;			/* file to write debug messages to */
extern int real_uid;			/* saved real uid before ruid setup */
extern char **save_argv;		/* saved pointer to arguments */
extern int some_deferred_addrs;		/* don't unlink spool file */
					/* as some addrs were deferred */
extern int prog_euid;			/* effective uid of program */
extern int prog_egid;			/* effective gid of program */
extern int force_zero_exitvalue;	/* if TRUE always exit with status 0 */
extern int call_defer_message;		/* if TRUE must call defer_message() */
extern int sender_is_trusted;		/* TRUE if sender is a trusted user */
extern char *sender_host;		/* name of sender's host */
extern char *sender_host_addr;		/* inet address of sender's host */
extern char *sender_proto;		/* name of sender's sending protocol */
extern char *sender_program;		/* name of program that spooled msg */
extern char * ident_sender;		/* The calculated identity of the sender */
extern char * ident_method;		/* Method used to determine ident_sender */

/* external variables defined in modes.c */
extern int daemon_pid;			/* pid of daemon process */

/* external variables defined in notify.c */
extern int send_to_postmaster;		/* set TRUE to mail to postmaster */
extern int return_to_sender;		/* set TRUE to mail log to sender */

/* external variables defined in parse.c */
extern char *off;			/* boolean off attribute value */
extern char *on;			/* boolean on attribute value */

/* external variables defined in queue.c */
extern int msg_grade;			/* grade level for this message */
extern long msg_body_size;		/* size of message body */

/* external variables defined in resolve.c */
extern struct hash_table *hit_table;	/* table to recognize address hits */

/* external variables defined in route.c */
extern int cached_routers;		/* TRUE if cache_routers() called */

/* external variables defined in spool.c */
extern char *message_id;		/* unique string ID for message */
extern char *spool_dir;			/* directory used to spool message */
extern char *spool_fn;			/* basename of spool file */
extern char *input_spool_fn;		/* name in input/ directory */
extern int spoolfile;			/* open spool file */
extern char *lock_fn;			/* name of lock file for spool_fn */
extern char *msg_buf;			/* i/o buffer for spool file */
extern char *msg_ptr;			/* read placeholder in msg_buf */
extern char *msg_max;			/* last valid char in msg_buf */
extern char *end_msg_buf;		/* end of msg_buf */
extern off_t msg_foffset;		/* file offset for msg_buf contents */
extern off_t msg_size;			/* size of spool file */

/* external variables defined in version.c */
extern char *version_number;		/* string defining version number */
extern char *release_date;		/* the date for this release */
extern char *patch_number;		/* most recent patch number */
extern char *patch_date;		/* patch date */
extern char *bat;			/* the proper bat for this release */

/* external variables defined in transports.c */
extern char *path_to_sender;		/* uucp-style route to sender */
extern int cached_transports;		/* TRUE if cache_transports() called */

/* external variables defined in route.c */
extern int cached_routers;		/* TRUE if cache_routers() called */

/* external variables defined in smtprecv.c */


/*
 * External functions used in the smail program
 */
/* external functions defined in addr.c */
extern char *preparse_address();
extern char *preparse_address_1();
extern int parse_address();
extern char *address_token();
extern char *back_address_token();
extern int mixed_address();
extern char *build_uucp_route();
extern char *build_partial_uucp_route();
extern void split_addr_list();
extern void strip_rfc822_comments();
extern struct addr *alloc_addr();
extern void insert_addr_list();
extern struct addr *addr_sort();
extern struct error *note_error();

/* external functions defined in alloc.c */
extern char *xmalloc();
extern char *xrealloc();
extern void xfree();
extern char *bmalloc();
extern char *brealloc();
extern void bfree();
extern struct block *alloc_block();
extern void realloc_block();
extern void free_block();

#ifdef HAVE_BIND
/* external functions defined in bindlib.c */
extern int bind_addr();
extern char * bind_compute_domain();
#endif /* HAVE_BIND */

/* external functions defined in direct.c */
extern void direct_local_addrs();
extern void verify_local_addrs();
extern void director_user_info();
extern struct director *find_director();
extern struct direct_driver *find_direct_driver();
extern void cache_directors();
extern void finish_directors();
extern char *read_director_file();

/* external functions defined in expand.c */
extern char *expand_string();
extern char **build_cmd_line();

/* external functions defined in field.c */
extern char *tokenize();
extern char *detokenize();
extern char *process_field();
extern void dump_tokens();

/* external functions defined in hash.c */
extern int add_to_hash();
extern int lookup_in_hash();
extern struct hash_table *new_hash_table();
extern void write_hash_table();
#ifdef ETHEREAL_HASHDATA
extern struct hash *delete_from_hash();
extern struct hash *replace_in_hash();
extern struct hash *store_in_hash();
extern struct hash *walk_hash();
extern void free_hash_element();
extern void free_hash_table();
#endif	/* ETHEREAL_HASHDATA */

/* external functions defined in header.c */
extern char *process_header();
extern int read_header();
extern int write_header();
extern int parse_precedence();

/* external functions defined in log.c */
extern void open_system_logs();
extern void open_msg_log();
extern void close_msg_log();
extern void unlink_msg_log();
#ifdef ANSI_C
extern void panic(int, char *, ...);
extern void write_log(int, char *, ...);
#else
extern void panic();
extern void write_log();
#endif
extern void send_log();
extern char *scan_msg_log();

/* external functions defined in lookup.c */
extern int open_database();
extern void close_database();
extern int lookup_database();

/* external functions defined in main.c */
extern void main();
extern void initialize_state();
extern void process_args();

/* external functions defined in modes.c */
extern void build_host_strings();
extern void compute_nobody();
extern void input_signals();
extern void processing_signals();
extern void deliver_signals();
extern void test_addresses();
extern void perform_deliver_mail();
extern void deliver_mail();
extern void daemon_mode();
extern void noop_mode();
extern void verify_addresses();
extern void print_version();
extern void print_copying_file();
extern void print_variables();
extern void print_queue();
extern void smtp_mode();
extern int process_spool_file();
extern int fork_wait();

/* external functions defined in notify.c */
extern void build_host_strings();
extern void compute_nobody();
extern void input_signals();
extern void process_signals();
extern void delivery_signals();
extern void test_addresses();
extern void perform_deliver_mail();
extern void deliver_mail();
extern void verify_addresses();
extern void run_queue();
extern void print_version();
extern void print_queue();
extern void smtp_mode();
extern void batch_smtp_mode();

/* external functions defined in notify.c */
extern void fail_delivery();
extern void defer_delivery();
extern void succeed_delivery();
extern void error_delivery();
extern struct addr *process_msg_log();
extern void notify();

/* external functions defined in parse.c */
extern char *parse_entry();
extern struct attribute *parse_config();
extern struct attribute *parse_table();
extern char *read_entry();

/* external functions defined in pathto.c */
extern void pathto();
extern void optto();
extern void uupath();

/* external functions defined in pwcache.c */
extern struct passwd *getpwbyname();
extern struct group *getgrbyname();
extern struct passwd *getpwbyuid();
extern struct group *getgrbygid();

/* external functions defined in qualify.c */
extern char *read_qualify_file();
extern char *qualify_domain();

/* external functions defined in queue.c */
extern int queue_message();
extern char **read_message();
extern int write_body();
extern void check_grade();
extern void log_incoming();
extern char **scan_spool_dirs();
extern int swallow_smtp();

/* external functions defined in resolve.c */
extern void resolve_addr_list();
extern int islocalhost();

/* external functions defined in qualify.c */
extern char *read_qualify_file();
extern char *qualify_domain();

/* external functions defined in retry.c */
extern char *read_retry_file();
extern struct addr *retry_addr_before();
extern struct addr *retry_addr_after();
extern void retry_addr_finished();
extern int retry_host_lock();
extern void retry_host_unlock();

/* external functions defined in route.c */
extern void route_remote_addrs();
extern void verify_remote_addrs();
extern char *match_end_domain();
extern char *match_end();
extern char *is_suffix();
extern void route_driver_finish();
extern struct router *find_router();
extern struct route_driver *find_route_driver();
extern void cache_routers();
extern void finish_routers();
extern char *read_router_file();
extern struct method *read_method_file();

/* external functions defined in smailconf.c */
extern char *read_config_file();
extern void print_config_variable();
extern char *read_standard_file();
extern void dump_standard_config();
extern char *fill_attributes();
extern struct attr_table *find_attribute();
extern void add_config_stat();
extern int is_newconf();
extern char *make_lib_fn();

/* external functions defined in smtprecv.c */
extern char **receive_smtp();

/* external functions defined in spool.c */
extern int creat_spool();
extern int write_spool();
extern int open_spool();
extern void close_spool();
extern void unlink_spool();
extern int seek_spool();
extern off_t tell_spool();
extern int send_spool();
extern int read_spool();
extern void log_spool_errors();
extern int new_grade();
extern void defer_message();
extern long message_date();
extern long spool_max_free_space();

/* external functions defined in string.c */
extern char *str2lower();
extern int strcmpic();
extern int strncmpic();
extern int strip();
extern char *strcolon();
extern int is_string_in_list();
#ifndef HAVE_STRERROR
extern char *strerror();
#endif
extern char *strsysexit();
extern void str_printf();
extern char *xprintf();
extern int dprintf();
extern long c_atol();
extern char *base62();
extern void str_cat();
extern void str_ncat();
#ifndef HAVE_VFPRINTF
extern int vfprintf();
#endif
extern long ivaltol();
extern char *ltoival();
extern char *copy();
extern char *rcopy();

/* external functions defined in sysdep.c */
extern char *time_stamp();
extern char *get_arpa_date();
extern int get_local_year();
extern char *unix_date();
extern void compute_local_sender();
extern void getfullname();
extern FILE *fopen_as_user();
extern int lock_file();
extern void unlock_file();
extern char *compute_hostname();
extern char *compute_domain();
extern int open_child();
extern int close_child();
extern void close_all();
extern char *scan_dir();
extern int touch();
extern int fsetsize();

/* external functions defined in transport.c */
extern struct assign_transport *assign_transports();
extern void call_transports();
extern int write_message();
extern char *get_sender_addr();
extern char *remote_from_line();
extern char *local_from_line();
extern struct transport *find_transport();
extern struct transport_driver *find_transport_driver();
extern void cache_transports();
extern void finish_transports();
extern char *read_transport_file();

/* external functions defined in verify.c */
extern void verify_addr_list();

/* external functions defined in version.c */
extern char *version();
