/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */


/* These aren't macros, but they are the sort of general definition
that fits in this file and we need to have them defined early. */

typedef int BOOL;
typedef int IP_ADDRESS;

/* Define which ends of pipes are for reading and writing, as some systems
don't make both fd's two-way. */

#define pipe_read  0
#define pipe_write 1

/* The EDQUOT error code isn't universally available, though it is widespread.
There is a particular shambles in SunOS5, where it did not exist originally,
but got installed with a particular patch for Solaris 2.4. There is a
configuration variable for specifying what the system's "over quota" error is,
but if it is not set, default to EDQUOT if it exists, otherwise ENOSPC. */

#ifndef ERRNO_QUOTA
#ifdef  EDQUOT
#define ERRNO_QUOTA EDQUOT
#else
#define ERRNO_QUOTA ENOSPC
#endif
#endif

/* The way of finding out the maximum file descriptor various between OS.
Most have sysconf(), but a few don't. */

#ifdef _SC_OPEN_MAX
  #define mac_maxfd (sysconf(_SC_OPEN_MAX) - 1)
#elif defined OPEN_MAX
  #define mac_maxfd (OPEN_MAX - 1)
#elif defined NOFILE
  #define mac_maxfd (NOFILE - 1)
#else
  #define mac_maxfd 255;    /* just in case */
#endif

/* Some generalities */

#define DEBUG(x)      if ((x) <= debug_level)

#define FALSE         0
#define TRUE          1

/* The size of buffer to get for constructing log entries. Make it big
enough to hold all the headers from a normal kind of message. */

#define LOG_BUFFER_SIZE 8192

/* The size of a big buffer for use in various places - usually for
sprinting into before getting the correct store. */

#define BIG_BUFFER_SIZE 1024

/* The size of a buffer in which string_nextinlist returns string values;
keep separate from big_buffer. */

#define STRING_RETURN_SIZE 1024

/* A limit to the length of a sender address */

#define SENDER_ADDRESS_MAXLENGTH 512

/* The length of message identification strings. This is the id used internally
by exim. The external version for use in Received: strings has a leading 'E'
added to ensure it starts with a letter. */

#define MESSAGE_ID_LENGTH 16

/* The length of the base names of spool files, which consist of an internal
message id with a trailing "-H" or "-D" added. */

#define SPOOL_NAME_LENGTH (MESSAGE_ID_LENGTH+2)

/* The maximum number of message ids to store in a waiting database
record. */

#define WAIT_NAME_MAX 50

/* Macros for trivial functions */

#define mac_ismsgid(s) regexec(regexp_spoolfile, (s))

/* Macros for calling the memory allocation routines with
tracing information for debugging. */

#define store_free(addr)   store_free_3(addr, __FILE__, __LINE__)
#define store_malloc(size) store_malloc_3(size, __FILE__, __LINE__)

/* Argument values for the time-of-day function */

enum { tod_log, tod_full, tod_bsdin };

/* For identifying which kind of driver deferred */

enum {
  DTYPE_DIRECTOR,
  DTYPE_ROUTER,
  DTYPE_TRANSPORT
};

/* Error numbers for generating error messages */

enum {
  ERRMESS_BADARGADDRESS,    /* Bad address via argument list */
  ERRMESS_BADADDRESS,       /* Bad address read via -t */
  ERRMESS_NOADDRESS,        /* Message has no addresses */
  ERRMESS_BADNOADDRESS,     /* Bad address via -t, leaving none */
  ERRMESS_SPOOLWRITE,       /* Write to spool failed */
  ERRMESS_MESSAGEREAD,      /* Read from input file failed */
  ERRMESS_VLONGHEADER,      /* Excessively long header */
  ERRMESS_TOOBIG,           /* Message too big */
  ERRMESS_TOOMANYRECIP      /* Too many recipients */
};

/* Error handling styles - set by option, and apply only when receiving
a local message. */

enum {
  ERRORS_SENDER,            /* Return to sender (default) */
  ERRORS_STDERR             /* Write on stderr */
};

/* Returns from the directing, routing, and transport functions (not all
apply to all of them) */

enum {
  OK,                       /* Success */
  DEFER,                    /* Failed soft */
  FAIL,                     /* Failed hard */
  FORCEFAIL,                /* Failed, and don't pass on */
  ISLOCAL,                  /* Remote address turned out to be local */
  PASS,                     /* Pass to next - routers only */
  ERROR,                    /* Soft failed with internal or config error */
  PANIC                     /* Hard failed with internal error */
};

/* Private error numbers for delivery failures, set negative so as not
to conflict with system errno values. */

#define ERRNO_UNKNOWNERROR  (-1)
#define ERRNO_USERSLASH     (-2)
#define ERRNO_EXISTRACE     (-3)
#define ERRNO_NOTREGULAR    (-4)
#define ERRNO_BADUGID       (-5)
#define ERRNO_BADMODE       (-6)
#define ERRNO_INODECHANGED  (-7)
#define ERRNO_LOCKFAILED    (-8)
#define ERRNO_BADADDRESS2   (-9)
#define ERRNO_BADFORWARD   (-10)
#define ERRNO_FORBIDPIPE   (-11)
#define ERRNO_FORBIDFILE   (-12)
#define ERRNO_FORBIDREPLY  (-13)
#define ERRNO_MISSINGPIPE  (-14)
#define ERRNO_MISSINGFILE  (-15)
#define ERRNO_MISSINGREPLY (-16)
#define ERRNO_BADALIAS     (-17)
#define ERRNO_SMTPCLOSED   (-18)
#define ERRNO_SMTPFORMAT   (-19)
#define ERRNO_SPOOLFORMAT  (-20)
#define ERRNO_NOTABSOLUTE  (-21)
#define ERRNO_EXIMQUOTA    (-22)   /* Exim-imposed quota */

/* These must be last, so all retry deferments can easily be identified */

#define ERRNO_RETRY_BASE   (-22)   /* Base to test against */
#define ERRNO_DRETRY       (-22)   /* Not time for directing */
#define ERRNO_RRETRY       (-23)   /* Not time for routing */
#define ERRNO_LRETRY       (-24)   /* Not time for local delivery */
#define ERRNO_HRETRY       (-25)   /* Not time for any remote host */
#define ERRNO_LOCAL_ONLY   (-26)   /* Local-only delivery */

/* Special actions to take after failure or deferment. */

enum {
  SPECIAL_NONE,             /* No special action */
  SPECIAL_FREEZE,           /* Freeze message */
  SPECIAL_FAIL              /* Fail the delivery */
};


/* Values for identifying particular headers; printing characters are
used so they can easily be seen in the spool file. */

#define htype_other    ' '   /* Unspecified header */
#define htype_from     'F'
#define htype_to       'T'
#define htype_cc       'C'
#define htype_bcc      'B'
#define htype_id       'I'   /* for message-id */
#define htype_replyto  'R'
#define htype_received 'P'   /* P for Postmark */
#define htype_sender   'S'
#define htype_old      '*'   /* Replaced header */

/* Offsets into the tables of names for headers (normal & resent) */

enum { hn_bcc, hn_cc, hn_date, hn_from, hn_msgid, hn_sender, hn_to,
  hn_replyto, hn_subject };

/* Types of item in options lists */

enum { opt_stringptr, opt_lcstringptr, opt_transportptr, opt_int,
  opt_octint, opt_mkint, opt_fixed, opt_time, opt_bool, opt_bool_verify,
  opt_uid, opt_gid, opt_uidlist, opt_gidlist, opt_expand_uid, opt_expand_gid,
  opt_searchtype, opt_local_batch };

/* There's a high-ish bit which is used to flag duplicate options, kept
for compatibility, which shouldn't be output. Also used for hidden options
that are automatically maintained from others. Another high bit is used to
flag driver options that although private (so as to be settable only on some
drivers), are stored in the instance block so as to be accessible from outside.
*/

#define opt_hidden  0x100
#define opt_public  0x200
#define opt_mask    0x0ff

/* Verify types when directing and routing */

enum { v_none, v_sender, v_recipient };

/* Search types for alias files etc. The single-key+file types must
come first, with all the "query" types last. Start the values at a printing
characters, as they are concatenated with file names in the caching tree. */

enum {
  stype_lsearch='0', stype_dbm, stype_nis, stype_nis0,   /* key+file types */
  stype_querystyle, stype_nisplus };                     /* query types */

/* Status values for host_item blocks. Require hstatus_unusable and
hstatus_unusable_expired to be last. */

enum { hstatus_unknown, hstatus_usable, hstatus_unusable,
       hstatus_unusable_expired };

/* Reasons why a host is unusable (for clearer log messages) */

enum { hwhy_unknown, hwhy_retry, hwhy_failed, hwhy_deferred };

/* Domain lookup types for routers */

enum { lk_pass, lk_byname, lk_bydns, lk_bydns_a, lk_bydns_mx };

/* Values for the self_code fields */

enum { self_freeze, self_defer, self_send, self_local,
  self_fail, self_forcefail };

/* Flags for rewrite rules */

#define rewrite_sender       0x001
#define rewrite_from         0x002
#define rewrite_to           0x004
#define rewrite_cc           0x008
#define rewrite_bcc          0x010
#define rewrite_replyto      0x020
#define rewrite_all_headers  0x03F  /* all header flags */

#define rewrite_envfrom      0x040
#define rewrite_envto        0x080
#define rewrite_all_envelope 0x0C0  /* all envelope flags */

#define rewrite_all      (rewrite_all_headers | rewrite_all_envelope)

#define rewrite_whole   0x100   /* option bit for headers */

/* Flags for log_write() */

#define LOG_MAIN        1    /* Write to the main log */
#define LOG_PANIC       2    /* Write to the panic log */
#define LOG_PANIC_DIE   6    /* Write to the panic log and then die */
#define LOG_PROCESS     8    /* Write to the process log */
#define LOG_REJECT     16    /* Write to the reject log, with headers */
#define LOG_RECIPIENTS 32    /* Add recipients to the message */
#define LOG_CONFIG     64    /* Add "Exim configuration error:\n" */
#define LOG_CONFIG2   128    /* Add "Exim configuration error for" */

/* Returns from host_find_by{name,dns}() */

enum {
  HOST_FIND_FAILED,     /* failed to find the host */
  HOST_FIND_AGAIN,      /* could not resolve at this time */
  HOST_FOUND,           /* found host */
  HOST_FOUND_LOCAL      /* found, but MX points to local host */
};

/* Actions applied to specific messages. */

enum { MSG_DELIVER, MSG_FREEZE, MSG_REMOVE, MSG_THAW, MSG_ADD_RECIPIENT,
       MSG_MARK_ALL_DELIVERED, MSG_MARK_DELIVERED, MSG_EDIT_SENDER,
       MSG_EDIT_BODY };

/* Options for local batched SMTP deliveries. Must be in the same order
as for non-SMTP local batching below. */

enum {
  local_smtp_off,       /* not doing batched SMTP */
  local_smtp_one,       /* each address separate */
  local_smtp_domain,    /* batch identical domains */
  local_smtp_all        /* batch all addresses */
};

/* Options for non-SMTP local batched deliveries - must be in the same
order as for SMTP local batching above. */

enum {
  local_batch_off,       /* not doing batched delivery */
  local_batch_one,       /* each address separate */
  local_batch_domain,    /* batch identical domains */
  local_batch_all        /* batch all addresses */
};

/* Returns from the spool_read_header() function */

enum {
  spool_read_OK,        /* success */
  spool_read_notopen,   /* open failed */
  spool_read_enverror,  /* error in the envelope */
  spool_read_hdrerror   /* error in the headers */
};

/* Options for transport_write_message */

#define topt_add_return_path    0x01
#define topt_add_delivery_date  0x02
#define topt_add_envelope_to    0x04
#define topt_from_hack          0x08
#define topt_use_crlf           0x10
#define topt_smtp_dots          0x20

/* End of macros.h */
