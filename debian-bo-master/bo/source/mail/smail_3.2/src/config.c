/*
#ident	"@(#)smail/src:RELEASE-3_2:config.c,v 1.23 1996/05/29 14:50:15 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * config.c:
 *	global variables which depend upon configuration
 *
 *	most of these values can be changed by the config file.
 *
 *	no external functions.
 */
#include <sys/types.h>
#include <stdio.h>
#include "defs.h"
#include "smail.h"
#include "config.h"
#ifndef DEPEND
# include "extern.h"
#endif

char *listen_name = NULL; 		/* DNS name of interface to listen on */
char *visible_name = VISIBLE_NAME;	/* hostname used in outgoing addrs */
char *visible_domains = VISIBLE_DOMAINS; /* domain used in outgoing headers */
char *uucp_name = UUCP_NAME;		/* hostname used in !-routes */
char *hostnames = HOSTNAMES;		/* list of other local host names */
char *more_hostnames = MORE_HOSTNAMES;	/* additional list of hostnames */
long max_message_size = MAX_MESSAGE_SIZE; /* max size of message body */
char *grades = GRADES;			/* mapping of precedence to grade */
int nobody_uid = NOBODY_UID;		/* user id for access permission */
int nobody_gid = NOBODY_GID;		/* group id for access permission */
char *nobody = NOBODY;			/* name of nobody user */
char *log_fn = LOGFILE;			/* name of information log file */
char *panic_fn = PANIC_LOG;		/* name of panic log file */
char *cons_fn = CONSOLE;		/* name of console device file */
char *spool_dirs = SPOOL_DIRS;		/* names of spooling directories */
int spool_mode = SPOOL_MODE;		/* mode for spool files */
int lock_mode = LOCK_MODE;		/* mode for lock files */
int log_mode = LOG_MODE;		/* mode for system log files */
int message_log_mode = MESSAGE_LOG_MODE; /* mode for per-message log files */
int spool_grade = SPOOL_GRADE;		/* default spool grade character */
int open_retries = OPEN_RETRIES;	/* max open retries on config files */
int open_interval = OPEN_INTERVAL;	/* sleep between open retries  */
int min_runq_grade;			/* minimum grade to process in queue run */
int max_runq_grade;			/* maximum grade to process in queue run */
int min_delivery_grade;			/* minimum grade to deliver */
int max_delivery_grade;			/* maximum grade to deliver */
char *config_file = CONFIG_FILE;	/* config file name */
char *director_file = DIRECTOR_FILE;	/* directors file name */
char *router_file = ROUTER_FILE;	/* routers file name */
char *method_dir = METHOD_DIR;		/* directory for non-/ method files */
char *transport_file = TRANSPORT_FILE;	/* transports file name */
char *qualify_file = QUALIFY_FILE;      /* domain qualification file name */
char *retry_file = RETRY_FILE;          /* address retry control file name */
char *smail_lib_dir = SMAIL_LIB_DIR;	/* default config file directory */
char *smail_util_dir = SMAIL_UTIL_DIR;	/* default smail utility directory */
char *received_field = RECEIVED_FIELD;	/* Received: field string */
char *message_id_field = MESSAGE_ID_FIELD; /* Message-Id: field string */
char *date_field = DATE_FIELD;		/* Date: field string */
char *from_field = FROM_FIELD;		/* From: field string */
char *return_path_field = RETURN_PATH_FIELD; /* Return-Path: field string */
char *smail = SMAIL;			/* location of the smail program */
double max_load_ave = MAX_LOAD_AVE;	/* spool mail > this load agerage */
char *trusted = TRUSTED;		/* : list of trusted users */
char *trusted_groups = TRUSTED_GROUPS;	/* : list of trusted groups */
unsigned message_bufsiz = MESSAGE_BUF_SIZE; /* size of message buffers */
int hit_table_len = HIT_TABLE_LEN;	/* #entries in address hit table */
int flock_mailbox = FLOCK_MAILBOX;	/* TRUE to use lock_fd_wait() macro */
int fnlock_retries = FNLOCK_RETRIES;	/* retries for lock_file() creat */
int fnlock_interval = FNLOCK_INTERVAL;	/* retry intervals for lock_file() */
int fnlock_mode = FNLOCK_MODE;		/* mode for lock_file() lockfiles */
int lock_by_name = LOCK_BY_NAME;	/* TRUE to use spool lockfiles */
int queue_only = FALSE;			/* TRUE to default to -Q flag */
int max_hop_count = MAX_HOP_COUNT;	/* fail if hop_count exceeds this */
char *delivery_mode_string = DELIVERY_MODE; /* string naming delivery mode */
char *delivery_grades = DELIVERY_GRADES;/* the msg grade range to be delivered */
char *runq_grades = RUNQ_GRADES;	/* the msg grade range in runq */
char *smart_user = CONFIG_SMART_USER;	/* default user for smartuser */
char *smart_path = CONFIG_SMART_PATH;	/* default path for smarthost */
char *smart_transport = CONFIG_SMART_TRANSPORT;	/* transport for smarthost */
char *second_config_file = SECOND_CONFIG_FILE; /* secondary config file */
char *copying_file = COPYING_FILE;	/* pathname to COPYING file */
int auto_mkdir = AUTO_MKDIR;		/* TRUE to auto create directories */
int auto_mkdir_mode = AUTO_MKDIR_MODE;	/* the mode for auto directories */
int require_configs = REQUIRE_CONFIGS;	/* TRUE to require config files */
char *postmaster_address = POSTMASTER_ADDRESS; /* default addr of postmaster */
int smtp_accept_max = SMTP_ACCEPT_MAX;	/* max simultaneous SMTPs to accept */
int smtp_accept_queue = SMTP_ACCEPT_QUEUE; /* simultaneous SMTPs to queueonly */
char *smtp_banner = SMTP_BANNER;	/* smtp startup banner message */
int smtp_debug = SMTP_DEBUG;		/* allow DEBUG command in SMTP */
int smtp_info = SMTP_INFO;		/* allow EXPN/VRFY commanda in SMTP */
char *sender_env_variable = SENDER_ENV_VARIABLE; /* env variable naming user */
int switch_percent_and_bang = SWITCH_PERCENT_AND_BANG;
					/* switch precedence of % and ! */
int error_copy_postmaster = ERROR_COPY_POSTMASTER;
					/* copy postmaster on errors */
long retry_interval = RETRY_INTERVAL;	/* default delivery retry interval */
long retry_duration = RETRY_DURATION;	/* default delivery retry duration */
long host_lock_timeout = HOST_LOCK_TIMEOUT; /* timeout for host lock */
long smtp_receive_command_timeout = SMTP_RECEIVE_COMMAND_TIMEOUT;
					/* timeout for smtp command reads */
long smtp_receive_message_timeout = SMTP_RECEIVE_MESSAGE_TIMEOUT;
					/* timeout for smtp message */
char *auth_domains = AUTH_DOMAINS;	/* authoritative domain list */
long rfc1413_query_timeout = -1;	/* Timeout on RFC1413 queries - initially disabled */
long resolve_timeout = RESOLVE_TIMEOUT;	/* timeout on directors/routers */
