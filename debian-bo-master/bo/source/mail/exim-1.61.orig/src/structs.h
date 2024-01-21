/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */


/* Definitions of various structures. We have to pre-declare some because of
mutually recursive definitions in the directors, routers, and transports
blocks. */

struct address_item;
struct driver_info;
struct director_info;
struct transport_info;
struct router_info;


/* Entries in lists of main and driver options are in this form. */

typedef struct {
  char *name;
  int   type;
  void *value;
} optionlist;


/* Chain of compiled regular expressions for handling lists */

typedef struct re_block {
  struct re_block *next;
  regexp *re;
} re_block;


/* Directors, routers, and transports have similar data blocks. Each driver
that is compiled into the code is represented by a xxx_info block; the active
drivers are represented by a chain of xxx_instance blocks. To make it possible
to use the same code for reading the configuration files for all three, the
layout of the start of the blocks is kept the same, and represented by the
generic structures driver_info and driver_instance. The specific structures
have the right types for the main entry point functions; we use void * in the
generic structure to match all types. */

typedef struct driver_instance {
  struct driver_instance *next;
  char *name;                    /* Instance name */
  struct driver_info *info;      /* Points to info for this driver */
  void *options_block;           /* Pointer to private options */
  char *driver_name;             /* All start with this generic option */
} driver_instance;

typedef struct driver_info {
  char *driver_name;             /* Name of driver */
  void *code;                    /* Main entry point of driver */
  void (*init)(                  /* Initialization entry point */
    struct driver_instance *);
  void (*tidyup)(                /* Tidyup entry point */
    struct driver_instance *);
  optionlist *options;           /* Table of private options names */
  int  *options_count;           /* -> Number of entries in table */
  void *options_block;           /* Points to default private block */
  int   options_len;             /* Length of same in bytes */
} driver_info;


/* Structure for holding information about the configured transports. Some
of the generally accessible options are set from the configuration file; others
are set by transport initialization, since they can only be set for certain
transports. They need to be generally accessible, however, as they are used by
the main transport code. */

typedef struct transport_instance {
  struct transport_instance *next;
  char *name;                    /* Instance name */
  struct transport_info *info;   /* Info for this driver */
  void *options_block;           /* Pointer to private options */
  char *driver_name;             /* Must be first */
  char *(*setup)(                /* Setup entry point for local transports */
    struct transport_instance *);
  int   local_batch;             /* )                                  */
  int   local_smtp;              /* )                                  */
  int   batch_max;               /* )                                  */
  BOOL  uid_set;                 /* )                                  */
  BOOL  gid_set;                 /* )                                  */
  uid_t uid;                     /* )                                  */
  gid_t gid;                     /* ) For those transports that change */
  char *expand_uid;              /* )   state - local transports.      */
  char *expand_gid;              /* )                                  */
  char *home_dir;                /* )                                  */
  char *current_dir;             /* )                                  */
  BOOL  deliver_as_creator;      /* )                                  */
  BOOL  initgroups;              /* )                                  */
  BOOL  multi_domain;            /* Applies to remote transports only */
  BOOL  return_output;           /* TRUE if output should always be returned */
  BOOL  return_fail_output;      /* ditto, but only on failure */
  BOOL  log_output;              /* Similarly for logging */
  BOOL  log_fail_output;
  BOOL  retry_use_local_part;    /* Defaults true for local, false for remote */
  int   max_addresses;           /* Max amalgamation for remote delivery */
} transport_instance;


/* Structure for holding information about a type of transport. */

typedef struct transport_info {
  char *driver_name;             /* Driver name */
  void (*code)(
    transport_instance *,
    struct address_item *);
  void (*init)(
    struct transport_instance *);
  void (*tidyup)(
    struct transport_instance *);
  optionlist *options;           /* Table of private options names */
  int  *options_count;           /* -> Number of entries in table */
  void *options_block;           /* Points to default private block */
  int   options_len;             /* Length of same in bytes */
  BOOL  local;                   /* TRUE for local transports */
  void (*closedown)(             /* For closing down a passed channel */
    struct transport_instance *);
} transport_info;


/* Structure for holding information about the configured directors. */

typedef struct director_instance {
  struct director_instance *next;
  char *name;
  struct director_info *info;
  void *options_block;           /* Pointer to private options */
  char *driver_name;             /* Must be first */
  char *prefix;                  /* Address prefix */
  char *suffix;                  /* Address suffix */
  char *domains;                 /* Specific domains */
  char *except_domains;          /* Specific not domains */
  char *local_parts;             /* Specific local parts */
  char *except_local_parts;      /* Specific not local parts */
  char *require_files;           /* Required (or not) for director to run */
  re_block *re_domains;          /* Used when domains are wildcarded */
  re_block *re_except_domains;
  re_block *re_local_parts;
  re_block *re_except_local_parts;
  transport_instance *transport; /* Assigned transport */
  BOOL  more;                    /* If FALSE, do no more if this one fails */
  BOOL  unseen;                  /* If TRUE, carry on even after success */
  BOOL  prefix_optional;         /* Just what it says */
  BOOL  suffix_optional;         /* Ditto */
  BOOL  verify_sender;           /* Use this director when verifying a sender */
  BOOL  verify_recipient;        /* Use this director when verifying a recipient*/
  BOOL  fail_verify_sender;      /* Fail verify if sender match this director */
  BOOL  fail_verify_recipient;   /* Fail verify if recipient match this director */
  BOOL  verify_only;             /* Skip this director if not verifying */
} director_instance;


/* Structure for holding information about a type of director. */

typedef struct director_info {
  char *driver_name;
  int (*code)(
    director_instance *,
    struct address_item *,
    struct address_item **,
    struct address_item **,
    struct address_item **,
    struct address_item **,
    BOOL);
  void (*init)(
    struct director_instance *);
  void (*tidyup)(
    struct director_instance *);
  optionlist *options;           /* Table of private options names */
  int  *options_count;           /* -> Number of entries in table */
  void *options_block;           /* Points to default private block */
  int   options_len;             /* Length of same in bytes */
} director_info;


/* Structure for holding information about the configured routers. */

typedef struct router_instance {
  struct router_instance *next;
  char *name;
  struct router_info *info;
  void *options_block;           /* Pointer to private options */
  char *driver_name;             /* Must be first */
  transport_instance *transport; /* Assigned transport */
  char *domains;                 /* Domains to match for this router to run */
  char *except_domains;          /* Specific not domains */
  char *local_parts;             /* Specific local parts */
  char *except_local_parts;      /* Specific not local parts */
  char *require_files;           /* Files required (or not) for router to run */
  re_block *re_domains;          /* Used when domains are wildcarded */
  re_block *re_except_domains;
  re_block *re_local_parts;
  re_block *re_except_local_parts;
  BOOL  more;                    /* If FALSE, do no more if this one fails */
  BOOL  unseen;                  /* If TRUE carry on, even after success */
  BOOL  verify_sender;           /* Use this router when verifying a sender */
  BOOL  verify_recipient;        /* Use this router when verifying a recipient */
  BOOL  fail_verify_sender;      /* Fail verify if sender match this router */
  BOOL  fail_verify_recipient;   /* Fail verify if recipient match this router */
  BOOL  verify_only;             /* Skip this router if not verifying */
  BOOL  pass_on_timeout;         /* Treat timeout DEFERs as fails */
  char *self;                    /* Text option for handling self reference */
  int   self_code;               /* Encoded version of same */
  BOOL  self_rewrite;            /* TRUE to rewrite headers if making local */
} router_instance;


/* Structure for holding information about a type of router. */

typedef struct router_info {
  char *driver_name;
  int (*code)(
    router_instance *,
    struct address_item *,
    struct address_item **,
    struct address_item **);
  void (*init)(
    struct router_instance *);
  void (*tidyup)(
    struct router_instance *);
  optionlist *options;           /* Table of private options names */
  int  *options_count;           /* -> Number of entries in table */
  void *options_block;           /* Points to default private block */
  int   options_len;             /* Length of same in bytes */
} router_info;


/* Structure for holding information about a host for use mainly by
routers, but also used when checking lists of hosts. Looking up host
addresses is done in both cases, using this structure. */

typedef struct host_item {
  struct host_item *next;
  char *name;
  char *address;
  /* Used when routing */
  int   mx;                      /* MX value if found via MX records */
  int   sort_key;                /* MX*100 plus random "fraction" */
  int   status;                  /* Usable, unusable, or unknown */
  int   why;                     /* Why host is unusable */
  int   last_try;                /* Time of last try if known */
  /* Used when checking */
  char *ident_string;            /* RFC 1413 ident string */
  re_block *compiled_name;       /* When name is wildcarded */
} host_item;

/* Structure for holding a single IP address; used for the chain of
addresses and ports for the local host. */

typedef struct ip_address_item {
  struct ip_address_item *next;
  char address[16];
} ip_address_item;

/* Structure for holding information about an IP net; used for controlling
incoming SMTP calls. */

typedef struct ip_net_item {
  struct ip_net_item *next;
  IP_ADDRESS address;
  IP_ADDRESS mask;
} ip_net_item;


/* Information about a soft delivery failure, for use when calculating
retry information. It's separate from the address block, because there
can be a chain of them for SMTP deliveries where multiple IP addresses
can be tried. */

typedef struct retry_item {
  struct retry_item *next;       /* for chaining */
  char *destination;             /* string identifying destination */
  int   basic_errno;             /* error code for this destination */
  int   more_errno;              /* additional error information */
  char *message;                 /* local error message */
  BOOL  delete;                  /* TRUE when retry info is to be deleted */
} retry_item;


/* Information about a constructed message that is to be sent using the
autoreply transport. This is pointed to from the address block. */

typedef struct reply_item {
  char *from;                    /* ) */
  char *to;                      /* ) */
  char *cc;                      /* ) specific header fields */
  char *bcc;                     /* ) */
  char *subject;                 /* ) */
  char *headers;                 /* misc other headers, concatenated */
  char *text;                    /* text string body */
  char *file;                    /* file body */
  BOOL  file_expand;             /* expand the body */
  char *logfile;                 /* file to keep a log in */
  char *oncelog;                 /* file to keep records in for once only */
} reply_item;


/* Chain of header lines; the basic structure can contain an empty
string; a bigger block is obtained for longer strings. */

typedef struct header_line {
  struct header_line *next;
  struct header_line *prev;
  int type;
  int slen;
  char text[1];
} header_line;

/* The address_item structure contains many fields which are used at various
times while delivering a message. Some are used only for remote deliveries;
some only for local. */

typedef struct address_item {
  struct address_item *next;     /* for chaining addresses */
  struct address_item *parent;   /* parent address */
  char *orig;                    /* as read from the spool */
  char *unique;                  /* used for disambiguating .forwarded */
  char *local_part;              /* local part of envelope address */
  char *prefix;                  /* stripped prefix of local part */
  char *suffix;                  /* stripped suffix of local part */
  char *domain;                  /* domain of envelope address */
  char *route_domain;            /* domain to be used by routers */
  char *errors_address;          /* where to send errors (NULL => sender) */
  BOOL pfr;                      /* pipe or file or reply delivery */
  BOOL rewrite_headers;          /* set TRUE by routers that want it done */
  BOOL local;                    /* local/remote flag */
  BOOL delivered;                /* already delivered */
  BOOL routed_by_domain;         /* routing did not depend on local part */
  BOOL ignore_error;             /* ignore delivery error */
  header_line *extra_headers;    /* additional headers */
  director_instance *director;   /* the director that directed */
  router_instance *router;       /* OR the router that routed */
  transport_instance *transport; /* transport to use */
  host_item *host_list;          /* host data for the transport */
  host_item *transported;        /* host that took delivery or failed hard */
  int  child_count;              /* number of child addresses */
  uid_t uid;                     /* uid for local transporting */
  gid_t gid;                     /* gid for local transporting */
  BOOL uid_set;                  /* Flags for setting */
  BOOL gid_set;                  /* Flags for setting */
  int  mode;                     /* mode for local transporting to a file */
  char *home_dir;                /* home directory for local transporting */
  char *current_dir;             /* current directory for local transporting */
  char *route_option;            /* undefined routing option string */
  BOOL initgroups;               /* use initgroups() for local transporting */
  BOOL expand_pipe;              /* expand pipe arguments */
  BOOL allow_pipe;               /* allow pipe in generated address */
  BOOL allow_file;               /* allow file in generated address */
  BOOL allow_reply;              /* allow autoreply in generated address */
  int  transport_return;         /* result of delivery attempt */
  int  basic_errno;              /* status after failure */
  int  more_errno;               /* additional error information */
  char *message;                 /* local error message */
  int  special_action;           /* used when when deferred or failed; also */
                                 /* contains = or - when successful SMTP delivered */
  BOOL dr_retry_exists;          /* director or router retry record exists */
  BOOL retry_skipped;            /* true if retry caused some skipping */
  BOOL retry_timedout;           /* true if retry timed out */
  retry_item *retries;           /* chain of retry information */
  char *return_filename;         /* ) some transports write text into a file */
  int   return_file;             /* ) for return to the sender */
  reply_item *reply;             /* and some generate reply messages */
} address_item;


/* Tables of normal and resent- header names consist of items of this type */

typedef struct {
  char *name;
  int   len;
} header_name;

/* Chain of information about errors (e.g. bad addresses) */

typedef struct error_block {
  struct error_block *next;
  char *text1;
  char *text2;
} error_block;

/* Chain of file names when processing the queue */

typedef struct queue_filename {
  struct queue_filename *next;
  char text[1];
} queue_filename;

/* Chain of items of retry information, read from the retry config. */

typedef struct retry_rule {
  struct retry_rule *next;
  int rule;
  int timeout;
  int p1;
  int p2;
} retry_rule;

typedef struct retry_config {
  struct retry_config *next;
  char *destination;
  re_block *re;
  int  basic_errno;
  int  more_errno;
  retry_rule *rules;
} retry_config;

/* Chain of rewrite rules, read from the rewrite config. */

typedef struct rewrite_rule {
  struct rewrite_rule *next;
  int flags;
  char *key;
  re_block *re;
  char *replacement;
} rewrite_rule;

/* Structure for each node in a tree, of which there are various kinds */

typedef struct tree_node {
  struct tree_node *left;         /* pointer to left child */
  struct tree_node *right;        /* pointer to right child */
  union
    {
    void  *ptr;                   /* pointer to data */
    int val;                      /* or integer data */
    } data;
  char balance;                   /* balancing factor */
  char name[1];                   /* node name - variable length */
} tree_node;

/* End of structs.h */
