/*
#ident	"@(#)smail/src:RELEASE-3_2:config.h,v 1.38 1996/03/10 15:45:04 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * NOTE:
 *	This file contains default values for a large number of
 *	configurable values in smail.  THIS FILE IS NOT INTENDED TO
 *	BE EDITED DIRECTLY.  Rather, users wishing to override
 *	default values should use the conf/EDITME file.  Either
 *	the MISC_DEFINES or the MISC_C_DEFINES list variable can
 *	be used to override values in this file.
 *
 *	This file contains two primary types of variables: variables
 *	which can be left undefined, and variables which must have a
 *	value.  For variables of the former type, two macros can be
 *	defined in the EDITME file: a value can be specified, or the
 *	variable can be disabled by using a macro prefix of NO_.  For
 *	example, to set LIST_FILENAME to /usr/local/lists/${lc:user},
 *	use:
 *
 *	   MISC_C_DEFINES='#define LIST_FILENAME "/usr/local/lists/${lc:user}'
 *
 *	To disable the use of a list director:
 *
 *	   MISC_DEFINES=NO_LIST_FILENAME
 *
 *	The other type of variable (variables that must have values)
 *	cannot be turned off with a NO_ macro.
 */


/*
 * SITENAME CONFIGURATION
 *
 * The following section defines the various possible names for the
 * local host which are recognized and produced implicitly by smail
 */

/*
 * Define the full domain name for the local site used in outgoing
 * addresses within header fields.  If VISIBLE_NAME is set to NULL, then
 * the the address used will be the first name in the list HOSTNAMES.  If
 * HOSTNAMES is also NULL, then a hostname will be computed in a system-
 * dependent fashion and VISIBLE_DOMAINS used to build HOSTNAMES and
 * VISIBLE_NAME in the form <hostname>.VISIBLE_DOMAINS.  VISIBLE_NAME
 * should be a domain name which which remote sites will understand.
 *
 * VISIBLE_NAME does not need to specifically specify the local host.  It
 * can be the domain name for a set of hosts which maintain a consistent
 * user database or automatically forward to the homes for all users
 * within the domain.
 *
 * NOTE: This can be set with the config file value "visible_name"
 *
 * # define VISIBLE_NAME	"uts.amdahl.com" (* Sample *)
 */
#ifndef	VISIBLE_NAME
# define VISIBLE_NAME NULL		/* Normal setting */
#endif

/*
 * The value of VISIBLE_DOMAINS may be used to create the HOSTNAMES and
 * VISIBLE_NAMES values.  VISIBLE_DOMAINS can be a list of colon-separated
 * values.  The first value will be used in generating VISIBLE_DOMAINS,
 * the first and remaining values will be used in generating HOSTNAMES.
 * For a host in the UUCP zone which is in the maps distributed over
 * USENET in comp.mail.maps, one of these values should be "uucp".
 * For a host in a registered domain, this should contain the domain in
 * which the host is registered.
 *
 * CAUTION:  for newly registered domains "uucp" should be first in this
 *	     list for a month or two, to allow for propogation of the
 *	     new domain name around the various networks.
 *
 * NOTE: This can be set with the config file value "visible_domains"
 *
 * # define VISIBLE_DOMAINS "amdahl.com:uucp" (* sample *)
 * # define VISIBLE_DOMAINS "Berkeley.EDU" (* sample site not in UUCP zone *)
 */
#ifndef	VISIBLE_DOMAINS
# define VISIBLE_DOMAINS "uucp"		/* for a host in the UUCP zone */
#endif

/*
 * A colon-separated list of names for the local host.  This list,
 * together with VISIBLE_NAME, UUCP_HOST and MORE_HOSTNAMES should
 * represent all possible names for the local host.  For a host that
 * is in more than one domain or that can gateway to more than one
 * level of domains, this should represent those names.  For a host in
 * a registered domain in the UUCP zone, which is also in the maps
 * distributed by USENET, <hostname>.uucp should be in this list.
 *
 * If HOSTNAMES is non-NULL then VISIBLE_DOMAINS is ignored.  If
 * HOSTNAMES is non-NULL and VISIBLE_NAME is NULL, then the first
 * value in HOSTNAMES is used to create the VISIBLE_NAME.
 *
 * Also, the first value in HOSTNAMES is used to specify the primary
 * domain name for the local host.  This name should uniquely
 * specify the local host and is used in tracing headers and in
 * forming the Message-Id.
 *
 * NOTE: This can be set with the config file value "hostnames"
 *
 * # define HOSTNAMES	"amdahl.uts.amdahl.com:amdahl.uucp" (* sample *)
 */
#ifndef	HOSTNAMES
# define HOSTNAMES	NULL		/* normal setting */
#endif

/*
 * A colon-separated list of yet more names for the local host.  This
 * list can be used to specify names in addition to those names computed
 * from the VISIBLE_DOMAINS values crossed with the real hostname.  Thus,
 * if a site (especially a domain gateway) wishes to have one set of
 * names computed automatically from the machines real hostname, while
 * also recognizing an additional set of names, HOSTNAMES can be set to
 * NULL (causing it to be computed) and MORE_HOSTNAMES can be set to the
 * list of additional hostnames.
 *
 * NOTE: This can be set with the config file value "more_hostnames"
 *
 * #define MORE_HOSTNAMES	"uts.amdahl.com:amdahl.com" (* sample *)
 */
#ifndef MORE_HOSTNAMES
# define MORE_HOSTNAMES	NULL		/* normal setting */
#endif

/*
 * The hostname used in !-routes in the `From_' lines before the header.
 * Set to NULL if this should be computed in a system-dependent fashion.
 *
 * NOTE: This can be set with the config file value "uucp_name"
 *
 * # define UUCP_NAME	"amdahl"	(* Example *)
 */
#ifndef	UUCP_NAME
# define UUCP_NAME NULL			/* Normal setting */
#endif

/*
 * A list of authoritative domains for this host can be configured
 * by setting this to a colon-separated list of domains.
 *
 * NOTE: This can be set with the config file value "auth_domains".
 */

#ifndef AUTH_DOMAINS
# define AUTH_DOMAINS NULL		/* Normal setting */
#endif


/*
 * STRONG SUGGESTIONS
 *
 * The following suggested values should probably not be changed,
 * except to make them less restrictive.
 */

/*
 * maximum size for a message.  Messages longer than this are truncated.
 * If you do not wish messages to be truncated at all, set this to zero.
 *
 * NOTE: This can be set with the config file value "max_message_size"
 */
#ifndef	MAX_MESSAGE_SIZE
# define MAX_MESSAGE_SIZE	(100*1024)
#endif

/*
 * This string defines the grade values which correspond to particular
 * names of the Precedence: field in the message header.  The parts of
 * the string are separated by `:' with alternating precedence name and
 * grade character.  Numbers are higher than upper case letters which are
 * higher than lower case letters in their grade value.  Grades in the
 * range [a-m] will only have an error message and header returned to the
 * sender on errors.  Grades in the range [n-z] will not have anything
 * returned to the sender on errors.
 *
 * The values recognized by many sendmail.cf files are: special-delivery,
 * first-class and junk.  Others are useful mainly for getting mail out
 * of the local machine or for communication with other hosts running
 * smail in a similar configuration.
 *
 * NOTE: This can be set with the config file value "grades"
 */
#ifndef	GRADES
# define GRADES "special-delivery:9:air-mail:A:first-class:C:bulk:a:junk:n"
#endif

/*
 * The maximum hop count allowed for remote delivery.  If a remote
 * transport is called and the current hop_count is equal to or greater
 * than this number, delivery fails.  This number does not affect local
 * delivery (i.e., transports with the `local' attribute turned on).
 *
 * NOTE: This can be set with the config file value "max_hop_count"
 *
 * WARNING: this number is not standardized across all networks, so
 * 	    return messages to senders will fail if an intermediate
 * 	    site in the return path has a lower maximum hop count
 * 	    then the sight initiating the "hop_count exeeded" error
 * 	    message.
 */
#ifndef MAX_HOP_COUNT
# define MAX_HOP_COUNT	20
#endif


/*
 * PREFERENCES
 *
 * The following values can be changed at the preference of the local
 * site administrator without affecting any remote sites, unless the
 * administrator gets them wrong and causes mail to fail.
 *
 * NOTE: many suggested pathnames are in /usr/lib/smail and
 *	 /usr/spool/smail for compatibility with current naming
 *	 schemes used by other mailers and by netnews.  An
 *	 alternate suggestion is to group these files under one
 *	 directory /usr/smail.
 */

/*
 * This pathname is where the file COPYING from the source directory
 * will be installed for reference by the -bc option to smail.  If
 * this does not begin with `/', it will be referenced relative to
 * smail_lib_dir.
 *
 * NOTE: This can be set with the config file value "copying_file"
 */
#ifndef COPYING_FILE
# define COPYING_FILE "COPYING"
#endif

/*
 * Set this to the name of the system-wide log file to which potentially
 * interesting, non-panic, messages are written.  This file should be
 * truncated periodically.
 *
 * NOTE: This can be set with the config file value "logfile"
 *
 * # define LOGFILE "/usr/smail/log/logfile"  (* Alternate suggestion *)
 */
#ifndef	LOGFILE
# define LOGFILE "/usr/spool/smail/log/logfile" /* Suggestion */
#endif

/*
 * Set this to the name of the system-wide smail panic log file.
 * This file should be periodically reviewed by the system
 * administrator to ensure that there are no major problems with
 * the mail software, files or configuration.
 *
 * NOTE: This can be set with the config file value "panic_log"
 *
 * # define PANIC_LOG	"/usr/smail/log/panic" (* alternate suggestion *)
 */
#ifndef	PANIC_LOG
# define PANIC_LOG	"/usr/spool/smail/log/paniclog" /* Suggestion */
#endif

/*
 * Set this to the name of the console device.  This is used as a last
 * resort in attempting to write panic messages.
 *
 * NOTE: This can be set with the config file value "console"
 */
#ifndef	CONSOLE
# define CONSOLE	"/dev/console"
#endif

/*
 * define the permission modes for the logfile and panic log file.
 * If smail is not going to be run as set uid or set gid, this will
 * have to be 0666.
 *
 * NOTE: This can be set with the config file value "log_mode"
 */
#ifndef LOG_MODE
# define LOG_MODE 0644
#endif

/*
 * Set this to the spool directories for mail messages, more than one
 * spool directory can be specified by separating names with the colon
 * ( `:' ) character.  Spooling directories are tried in sequence until
 * spooling succeeds completely in one of them.  Thus, alternative
 * directories are useful to prevent loss of mail when filesystems fill
 * up, or run out of inodes, or even when somebody accidentally changes
 * permissions on one spool directory or its associated locking directory.
 * In general, alternative directories should be on separate filesystems,
 * perhaps even separate disks, if you are really paranoid.
 *
 * Of particular use is having more than one spool directory after
 * an extended period of down-time to handle the suddenly increased
 * influx of mail traffic.
 *
 * NOTE: This can be set with the config file value "spool_dirs"
 *
 * # define SPOOL_DIRS	"/usr/smail/spool" (* alternate suggestion *)
 * # define SPOOL_DIRS	"/usr/smail/spool:/alt/smail/spool" (* Example *)
 */
#ifndef	SPOOL_DIRS
# define SPOOL_DIRS	"/usr/spool/smail" /* Suggestion */
#endif

/*
 * Define the permission mode for spool files.  If a file descriptor
 * locking protocol is used which requires that a file be writable to
 * lock it, then this mode must allow at least write by owner (or group
 * if the mailer is run as set group ID).  These should not allow for
 * global read, as the messages stored in spool files should be considered
 * private.
 *
 * NOTE: This can be set with the config file value "spool_mode"
 */
#ifndef	SPOOL_MODE
# ifdef	UNIX_SYS5
#  define SPOOL_MODE	0600			/* why must this be? */
# else
#  define SPOOL_MODE	0400
# endif
#endif

/*
 * define the permission modes for lock files.  If the LOCK_BY_NAME
 * locking protocol is not used, then this is not of any value.
 *
 * NOTE: This can be set with the config file value "lock_mode" 
 */
#ifndef	LOCK_MODE
# define LOCK_MODE 0444
#endif

/*
 * define AUTO_MKDIR to be TRUE if spool and log directories should
 * be created automatically when smail fails to access them.  This
 * behavior allows smail to essentially automatically install itself
 * as long as the binaries are in place.  AUTO_MKDIR_MODE should be
 * set to the mode used for directory creation.
 */
#ifndef AUTO_MKDIR
# define AUTO_MKDIR  TRUE
#endif

#ifndef	AUTO_MKDIR_MODE
# define AUTO_MKDIR_MODE  0755
#endif

/*
 * define the permission modes for the per-message log files.
 *
 * NOTE: This can be set with the config file value "message_log_mode" 
 */
#ifndef	MESSAGE_LOG_MODE
# define MESSAGE_LOG_MODE 0644
#endif

/*
 * Name the user that is to receive mail to the Postmaster, by default.
 * This address is used, if no director matches Postmaster, to ensure
 * that this address will always reach somebody.
 */
#ifndef POSTMASTER_ADDRESS
# define POSTMASTER_ADDRESS "root"
#endif

/*
 * Defining ERROR_COPY_POSTMASTER to TRUE will cause all error message
 * generated by smail to be copied to the postmaster.  Normally, mail
 * will be copied to the postmaster only on selective errors that
 * appear to be related to local configuration errors.
 */

#if !defined(ERROR_COPY_POSTMASTER)
# define ERROR_COPY_POSTMASTER	FALSE	/* don't copy the postmaster */
#endif

/*
 * This defines the default grade for spool files. These grade
 * characters are used in a form similar to 4.3BSD UUCP grade
 * values.  Typically, this is `C'.  See the definition of the GRADES
 * attribute.
 *
 * NOTE: This can be set with the config file value "spool_grade"
 */
#ifndef	SPOOL_GRADE
# define SPOOL_GRADE	'C'
#endif

/*
 * On systems without an atomic rename system call, configuration files
 * are not guarranteed to always exist.  Thus, we must sleep and retry
 * open operations several times before determining that a file really
 * does not exist.  OPEN_INTERVAL defines the sleep interval between
 * retries (in seconds) and OPEN_RETRIES defines the number of retries
 * to be made in opening a file.
 *
 * For files that are optional, and which don't exist this is somewhat
 * expensive and introduces delays into the process, but there is nothing
 * that can be done.
 *
 * NOTE: These can be set with the config file values "open_interval" and
 *	 "open_retries"
 *
 * NOTES: for systems with an atomic rename, OPEN_RETRIES can be 0.
 *	  On systems without higher resolution timing, a sleep of
 *	  OPEN_INTERVAL is guarranteed to sleep at least
 *	  OPEN_INTERVAL-1 seconds.  Thus, this value should be at
 *	  least 2.
 */
#ifndef	OPEN_INTERVAL
# define OPEN_INTERVAL	2
#endif
#ifndef	OPEN_RETRIES
# ifdef HAVE_RENAME
#  define OPEN_RETRIES 0
# else
#  define OPEN_RETRIES	2
# endif
#endif

/*
 * The config file can be used to override some compiled in defaults of
 * smail.  If this does not begin with `/', it will be referenced
 * relative to the smail_lib_dir directory.
 *
 * NOTE: This cannot be set with the config file, for hopefully obvious
 *	 reasons.  This can be set on invocation of smail, however.
 *
 * # define CONFIG_FILE	"/usr/smail/lib/config" (* Alternate suggestion *)
 */
#ifndef	CONFIG_FILE
# define CONFIG_FILE	"config"	/* Suggestion */
#endif

/*
 * A secondary configuration file can be used which overrides both
 * internal configuration and configuration set in the primary configuration
 * file.  This is useful, for example, in an environment using shared
 * filesystems which may have a shared primary configuration and an
 * optional secondary configuration.  If this is NULL, then no secondary
 * configuration file used.
 *
 * NOTE: This can be set with the config file value "second_config_file"
 */
#ifndef SECOND_CONFIG_FILE
# define SECOND_CONFIG_FILE  NULL
#endif

/*
 * The director file contains the complete configuration for the director
 * subsystem.  It defines which director drivers are used, parameters
 * affecting driver operation and use, and the order in which directors
 * are called upon.  If this does not begin with `/', it will be
 * referenced relative to the smail_lib_dir directory.
 *
 * If this file does not exist, or DIRECTOR_FILE is NULL, the compiled
 * in defaults, defined in default.c, are used.
 *
 * NOTE: This can be set with the config file value "director_file"
 *
 * # define DIRECTOR_FILE "/usr/smail/lib/directors" (* alternate suggestion *)
 */
#ifndef	DIRECTOR_FILE
# define DIRECTOR_FILE	"directors"	/* Suggestion */
#endif

/*
 * The router file contains the complete configuration for the routing
 * subsystem.  It defines which route drivers are used, parameters
 * affecting driver operation and use, and the order in which routers
 * are called upon.  If this does not begin with `/', it will be
 * referenced relative to the smail_lib_dir directory.
 *
 * If this file does not exist, or ROUTER_FILE is NULL, the compiled
 * in defaults, defined in default.c, are used.
 *
 * NOTE: This can be set with the config file value "router_file"
 *
 * # define ROUTER_FILE	"/usr/smail/lib/routers" (* alternate suggestion *)
 */
#ifndef	ROUTER_FILE
# define ROUTER_FILE	"routers"	/* Suggestion */
#endif

/*
 * Method files indicate the transport method by which mail is delivered
 * to the next_hop site.  Method files can be indicated two ways in the
 * ROUTER_FILE: by explicit `/' or `~user' based paths, and by non-rooted
 * paths.  To find non-rooted method files, METHOD_DIR is pre-pended to
 * the path.  Set to NULL if you do not wish to have a method directory.
 * If this does not begin with `/', it will be referenced relative to
 * the smail_lib_dir directory.
 *
 * NOTE: This can be set with the config file value "method_dir"
 *
 * # define METHOD_DIR	"/usr/smail/lib/methods" (* alternate suggestion *)
 */
#ifndef	METHOD_DIR
# define METHOD_DIR	"methods"	/* Suggestion */
#endif

/*
 * The transport file contains the configuration for the transport
 * subsystem.  It defines which transport drivers are used, and parameters
 * affecting driver operation and use.  They are referenced from routers
 * and some directors.  Also, two transports are referenced implicitly be
 * smail's internal algorithms: "pipe" and "file" for delivery to
 * shell-command and file addresses.  If this does not begin with `/',
 * it will be referenced relative to the smail_lib_dir directory.
 *
 * If this file does not exist, or TRANSPORT_FILE is NULL, the compiled
 * in defaults, defined in default.c, are used.
 *
 * NOTE: This can be set with the config file value "transport_file"
 *
 * #define TRANSPORT_FILE "/usr/smail/lib/transports" (* alternate *)
 */
#ifndef	TRANSPORT_FILE
# define TRANSPORT_FILE   "transports"	/* Suggestion */
#endif

/*
 * The qualify file contains a list of hosts and the domains that they
 * live in.  It is searched sequentially.
 *
 * NOTE: This can be set with the config file value "qualify_file"
 *
 * #define QUALIFY_FILE "/usr/smail/lib/qualify" (* alternate *)
 */
#ifndef QUALIFY_FILE
# define QUALIFY_FILE   "qualify"  /* Suggestion */
#endif

/*
 * The retry file contains a list of hostname patterns, the time
 * to wait between retries, and the total time to attempt retries
 * before the address is considered undeliverable.
 *
 * NOTE: This can be set with the config file value "retry_file"
 *
 * #define RETRY_FILE "/usr/smail/lib/retry" (* alternate *)
 */
#ifndef RETRY_FILE
# define RETRY_FILE   "retry"  /* Suggestion */
#endif

/*
 * The smail library directory is the default directory for various
 * configuration files.  Also, files accessed through the routines in
 * lookup.c may be referenced relative to this directory by default.
 *
 * NOTE: This can be set with the config file value "smail_lib_dir",
 *	 though CONFIG_FILE is always referenced relative to the compiled
 *	 in lib directory, for hopefully obvious reasons.
 *
 * #define SMAIL_LIB_DIR "/usr/smail/lib"	(* Alternate Suggestion *)
 */
#ifndef SMAIL_LIB_DIR
# define SMAIL_LIB_DIR	"/usr/lib/smail"	/* Suggestion */
#endif

/*
 * The smail utility directory, which contains, among other programs,
 * mkdbm and mkaliases.
 *
 * This can be changed with the EDITME variable UTIL_BIN_DIR.
 */

#ifndef SMAIL_UTIL_DIR
# define SMAIL_UTIL_DIR	SMAIL_LIB_DIR
#endif

/*
 * Require that specified configuration files exist.  If this is
 * defined as TRUE then non-existent configuration files are generally
 * ignored.  If this defined as FALSE, non-existent configuration
 * files will generate a panic.
 */
#ifndef REQUIRE_CONFIGS
# define REQUIRE_CONFIGS FALSE
#endif

/*
 * Allow debugging to be used in SMTP dialogs.  This allows the
 * remote end to see the debugging output from smail, but does not
 * have any other affects.  If this is TRUE, the DEBUG command in
 * SMTP turns on debugging and sets its level.  If this is FALSE,
 * the DEBUG command only produces a cute message.  This is only
 * useful if NODEBUG is not defined.
 */
#ifndef SMTP_DEBUG
# define SMTP_DEBUG	TRUE		/* probably reasonable */
#endif

/*
 * Allow the information commands EXPN & VRFY to be used in SMTP dialogs.  
 * This allows the remote end to get information about your site
 * so the paranoid may wish to disable this.
 */
#ifndef SMTP_INFO
# define SMTP_INFO	TRUE		/* probably reasonable */
#endif

/*
 * Set this to a string which will be expanded to form the
 * Received: header field.  This version is about as complete
 * and correct as it can get....  Strictly speaking the "id"
 * portion must include the "@$primary_name", but if you read
 * the verbose description of RECEIVED in RFC 822, it is stated
 * "id" may be "the internal message identifier."  Also, the
 * "via" was intended to "indicate what physical mechanism the
 * message was sent over," but we replace this with the virtual
 * mechanism, i.e. the program that received the message.  We
 * include the "ident" comment since this is the easiest and
 * most obvious place to log this information inside the msg.
 *
 * NOTE: This can be set with the config file value "received_field"
 */
#ifndef	RECEIVED_FIELD
# define RECEIVED_FIELD	\
"Received: \
${if def:sender_host\
	{from $sender_host${if def:sender_host_addr\
		{(really [$sender_host_addr])}} }\
 else {${if def:sender_host_addr\
	{from [$sender_host_addr] }}}}\
by $primary_name\n\t\
${if def:sender_program\
	{via $sender_program }}\
${if def:sender_proto\
	{with $sender_proto}\
 else {with stdio}}\
${if def:ident_sender\
	{ (ident $ident_sender using $ident_method)}}\n\t\
id <$message_id@$primary_name>\n\t\
${if def:input_addr\
	{for ${top:input_addr}}\
 else {for <unknown>}}\
; $spool_date\n\t\
($version_string built $compile_date)"
#endif

/*
 * Set this to a string which will be expanded to form the
 * Message-Id: header field.
 *
 * NOTE: This can be set with the config file value "message_id_field"
 */
#ifndef	MESSAGE_ID_FIELD
# define MESSAGE_ID_FIELD "Message-Id: <$message_id@$primary_name>"
#endif

/*
 * Set this to a string which will be expanded to form the
 * Date: header field.
 *
 * NOTE: This can be set with the config file value "date_field"
 */
#ifndef	DATE_FIELD
# define DATE_FIELD	"Date: $spool_date"
#endif

/*
 * Set this to a string which will be expanded to form the
 * From: header field.
 *
 * NOTE: This can be set with the config file value "from_field"
 */
#ifndef	FROM_FIELD
# define FROM_FIELD	"From: $sender${if def:sender_name: ($sender_name)}"
#endif

/*
 * Set this to a string which will be expanded to form the
 * Return-Path: header field.
 *
 * NOTE: This can be set with the config file value "return_path_field"
 */
#ifndef	RETURN_PATH_FIELD
# define RETURN_PATH_FIELD "Return-Path: <$sender>"
#endif

/*
 * SMTP startup banner message
 */
#ifndef SMTP_BANNER
# ifdef HAVE_EHLO
#  define SMTP_BANNER "$primary_name Smail-$version (#$compile_num $compile_date) ready at $date\nESMTP supported"
# else
#  define SMTP_BANNER "$primary_name Smail-$version (#$compile_num $compile_date) ready at $date"
# endif
#endif

/*
 * Maximum number of allowed smtp connection processes forked by the
 * smtp daemon.
 *
 * NOTE: This can be set with the config file value "smtp_accept_max".
 */
#ifndef SMTP_ACCEPT_MAX
# define SMTP_ACCEPT_MAX	0	/* allow an infinite number */
#endif

/*
 * Maximum number of allowed smtp connection processes forked by the
 * smtp daemon, until only message queuing is allowed.
 *
 * NOTE: This can be set with the config file value "smtp_accept_queue".
 */
#ifndef SMTP_ACCEPT_QUEUE
# define SMTP_ACCEPT_QUEUE	0	/* allow an infinite number */
#endif

/*
 * Number of seconds to wait for receipt of SMTP commands for interactive
 * SMTP receiver processes.  The default value is in accordance with the
 * recommendations of RFC1123.
 */

#ifndef SMTP_RECEIVE_COMMAND_TIMEOUT
# define SMTP_RECEIVE_COMMAND_TIMEOUT	(5*60)	/* five minutes */
#endif

/*
 * Number of seconds to wait for receipt of a message from a data
 * command within an SMTP receiver process.
 *
 * NOTE:  RFC1123 appears to be silent on a recommendation for this
 * value.
 */

#ifndef SMTP_RECEIVE_MESSAGE_TIMEOUT
# define SMTP_RECEIVE_MESSAGE_TIMEOUT	(2*60*60)	/* 2 hours */
#endif

/*
 * Set this to a string defining the mode to use for delivery of new
 * incoming mail messages.  It should be one of:
 *
 * "foreground"		- deliver without forking a new process
 * "background"		- fork a child to do delivery and don't wait for it
 * "queue"		- do not perform delivery, just write the spool file
 *
 * NOTE: This can be set with the config file value "delivery_mode"
 */
#ifndef DELIVERY_MODE
# define DELIVERY_MODE "background"
#endif

/*
 * Define the required effective user id for mail that is to be
 * delivered after being queued.  If the effective user ID when
 * smail is executed does not equal this number, then mail messages
 * can be queued, but will not be delivered until a later queue
 * run.
 *
 * This should normally be 0 (root), since smail is not likely to
 * be able to deliver mail messages successfully unless it is run
 * as root.
 *
 * If REQUIRED_EUID is not defined, then no check will be performed,
 * and any delivery mode will be allowed.
 */
#if !defined(REQUIRED_EUID) && !defined(NO_REQUIRED_UID)
# define REQUIRED_EUID		0	/* default, must be root to deliver */
#endif

/*
 * the mailbox file locking protocol requires a retry count and interval
 * for creating lock files.  This is used by the file driver called by
 * the "file" transport and, possibly, the "local" transport
 *
 * NOTE:  These can be changed in config file by setting "fnlock_retries"
 *	  and "fnlock_interval"
 */
#ifndef	FNLOCK_RETRIES
# define FNLOCK_RETRIES		5	/* up to five retries */
#endif

#ifndef	FNLOCK_INTERVAL
# define FNLOCK_INTERVAL	3	/* at three second intervals */
#endif

/*
 * This octal permission mode is used in creating mailbox lock files
 *
 * NOTE: This can be set with the config file value "fnlock_mode"
 */
#ifndef	FNLOCK_MODE
# define FNLOCK_MODE	0666		/* everybody can read and write */
#endif

/*
 * Define a nobody user ID and group ID for defining default access
 * permissions.  This is often used by directors and routers and
 * transports that call external programs where no other uid/gid has
 * been given.
 *
 * if NOBODY_UID and NOBODY_GID are < 0, but NOBODY is non-NULL,
 * then getpwnam(3) is called to obtain the uid and gid.
 *
 * NOTE: This can be set with the config file value "nobody"
 *
 * #define NOBODY_UID	11		(* uid "nobody" on namei *)
 * #define NOBODY_GID	90		(* gid "guest" on namei *)
 */
#ifndef	NOBODY_UID
# define NOBODY_UID	(-1)		/* compute the nobody_uid */
#endif

#ifndef	NOBODY_GID
# define NOBODY_GID	(-1)		/* compute the nobody_gid */
#endif

#ifndef	NOBODY
# if	defined(UNIX_BSD4_3) || defined(sun)
#  define NOBODY	"nobody"	/* 4.3BSD has a standard for this */
# else
#  define NOBODY	"guest"	  /* patch this with something reasonable */
# endif
#endif

/*
 * Set SENDER_ENV_VARIABLE to name an environment variable that can
 * be used to name the calling user.  For System V systems this can be
 * set to LOGNAME.  For BSD systems, this can be set to USER.
 */

#ifndef SENDER_ENV_VARIABLE
/* # define SENDER_ENV_VARIABLE		"LOGNAME" /* for System V */
/* # define SENDER_ENV_VARIABLE		"USER" /* for BSD */
#define SENDER_ENV_VARIABLE		NULL	/* no default defined */
#endif

/*
 * Set RETRY_INTERVAL to the default minimum interval, in seconds,
 * between a deferred delivery and the subsequent retry.
 * Set RETRY_DURATION to the default maximum duration of retries for
 * a given address.
 */

#ifndef RETRY_INTERVAL
# define RETRY_INTERVAL (10 * 60L)	 /* ten minutes */
#endif
#ifndef RETRY_DURATION
# define RETRY_DURATION (5 * (24 * 60L * 60L))	/* five days */
#endif

/*
 * Set HOST_LOCK_TIMEOUT to the default timeout, in seconds, for
 * use when trying to lock a retry file for exclusive delivery
 * to a given host.
 */

#ifndef HOST_LOCK_TIMEOUT
# define HOST_LOCK_TIMEOUT 30L
#endif

/*
 * Set SMAIL to the location of the `smail' program.  This filename is
 * used when smail wants to re-exec itself to completely reinitialize
 * its state.
 *
 * NOTE: This can be set with the config file value "smail"
 *
 * #define SMAIL "/usr/lib/smail/smail"	(* potentially useful setting *)
 * #define SMAIL "/usr/smail/lib/smail"	(* alternate suggestion *)
 */
#ifndef	SMAIL
# define SMAIL "/usr/lib/sendmail"	/* Suggestion */
#endif

/*
 * We can directly deliver a mail message by forking a new smail process
 * each time we receive one when the load average is <= MAX_LOAD_AVE.
 * When the load average is > MAX_LOAD_AVE, all incoming messages are spooled
 * for later processing.  Spooling mail messages helps reduce the load on the
 * system while delaying mail delivery.
 *
 * The load average is taken from the 5 minute load average.  On systems
 * that do not compute load average, a value of 0 is always assumed.
 *
 * Set this value to 0 if you always wish to always with directly deliver
 * messages.  The MAX_LOAD_AVE on systems that compute the load average
 * should be set to a value where the response time becomes poor for
 * interactive users.
 *
 * 0 is a magic value which cause the load average not to be computed at
 * all.
 *
 * NOTE: This can be set with the config file value "max_load_ave"
 *
 * NOTE: Load-average computation is not currently supported.
 */
#ifndef	MAX_LOAD_AVE
# define MAX_LOAD_AVE	0
#endif

/*
 * Normal users cannot set the sender for mail.  However, daemons need
 * to be able to do this.  The TRUSTED string defines which users are
 * allowed to supply a sender explicitly.  If it is NULL, then anybody
 * can supply a sender string.
 *
 * Trusted users should include the real-user-id under which programs
 * like rmail (via uux/uuxqt) or sendmail (via inetd) execute.  Often
 * this means root, uucp and daemon.
 *
 * The symbol TRUSTED is a colon-separated list of trusted users.
 *
 * NOTE: This can be set with the config file value "trusted"
 *
 * EXTREME CAUTION:
 *	Currently, the UUCP subsystem does not change real uids when
 *	executing programs.  Since smail can only use the real uid
 *	to verify users (assuming it runs setuid), it has no way of
 *	verifying absolutely for certain that UUCP submitted mail
 *	received from a remote site as opposed to a user that simply
 *	initiated a UUCP transaction.
 *	   Unless you are certain that your UUCP (and other transport
 *	agents) will always set the real uid to something smail will
 *	recognize as trusted, you should turn off the TRUSTED attribute
 *	by setting it to NULL.
 *
 *	   Depending upon your situation, trusted groups may be
 *	sufficient for your needs.  See below.
 *
 * #define TRUSTED "root:uucp:daemon"	(* The nominal setting *)
 */
#ifndef	TRUSTED
# define TRUSTED NULL			/* Disable use of trusted */
#endif

/*
 * Smail can use the effective gid under which the mailer was invoked
 * to perform trusted user verification.  This is a better solution,
 * in general for programs such as uucp that do not change their real
 * uid (in many implementations).  By ensuring that uucp is always
 * invoked with a reasonable group, in this colon-separated list,
 * verification can be performed.  Be sure that all trusted pathways
 * into the mailer have an entry in either TRUSTED or TRUSTED_GROUPS,
 * or the results are worse than setting TRUSTED and TRUSTED_GROUPS to
 * NULL.  After all, these only affect the ability to forge sender
 * lines, which is pretty easy to do anyway.  (just connect to an SMTP
 * socket on another machine and you can forge all the mail you want).
 *
 * #define TRUSTED "uucp:wheel"		(* a potentially nominal setting *)
 */
#ifndef TRUSTED_GROUPS
# define TRUSTED_GROUPS NULL
#endif

/*
 * if your machine handles large data areas efficiently, then making
 * MESSAGE_BUF_SIZE the maximum size of a message will minimize i/o
 * overhead for reading and writing the message.  Systems with a
 * small amount of memory or poor VM systems cannot afford large buffers.
 * Larger memory systems can set the buffer size to less than
 * MAX_MESSAGE_SIZE at the expense of more i/o on large mail messages.
 *
 * NOTE: This can be set with the config file value "message_buf_size"
 */
#ifndef	MESSAGE_BUF_SIZE
# ifdef	SMALL_MEMORY
#  define MESSAGE_BUF_SIZE	BUFSIZ
# else
#  if MAX_MESSAGE_SIZE == 0
#   define MESSAGE_BUF_SIZE	(256*1024)
#  else
#   define MESSAGE_BUF_SIZE	MAX_MESSAGE_SIZE
#  endif
# endif
#endif

/*
 * Number of entries in address hit table.  The larger the number the
 * greater the space but the greater the efficiency.
 *
 * NOTE: This can be set with the config file value "hit_table_len"
 */
#ifndef HIT_TABLE_LEN
# define HIT_TABLE_LEN	241		/* suitable for large mailing lists */
#endif

/*
 * If you wish to use the % operator in preference to the ! operator,
 * define SWITCH_PERCENT_AND_BANG to be TRUE.  This switch happens
 * only for addresses that have % and ! operators in the local-part of
 * a local-part@domain address.
 *
 * See the parse_address() function in addr.c for a complete description
 * of how this affects smail.
 *
 * SWITCH_PERCENT_AND_BANG should be defined as either TRUE or FALSE.
 */

#ifndef SWITCH_PERCENT_AND_BANG
#define SWITCH_PERCENT_AND_BANG	FALSE
#endif


/*
 * DEFAULT DIRECTOR CONFIGURATION
 *
 * This section adjusts the default director configuration compiled into
 * smail.  See default.c for the complete default configuration source.
 *
 * NOTE:  The existence of a direcor configuration file replaces all of
 *	  this configuration.
 */

/*
 * configuration for the default aliases director
 *
 * If you wish to have an aliases director by default, define
 * ALIASES_FILE to be the name of a sorted file, and ALIASES_PROTO
 * to be the database access protocol to use in searching this file
 * See the file lookup.c for a complete list of these protocols.  If
 * this does not begin with `/', it will be referenced relative to the
 * smail_lib_dir directory.
 *
 * A partial list is:
 *
 * lsearch - perform linear searches on raw alias files.  This is slow
 *	     unless the aliases file is small.
 * bsearch - perform binary searches on sorted alias files with one
 *	     alias rule per line.
 * dbm	   - use the Berkeley dbm(3X) or ndbm(3X) libraries to search
 *	     for aliases.
 * yp	   - use Sun's YP service to search for aliases.  This requires
 *	     the existence of the YP library routines.
 * aliasyp - use Sun's YP service in a manner compatible with standard
 *	     Sun mail.aliases YP databases.  These do not exactly match
 *	     the semantics of other YP databases in that they count an
 *	     the ending nul byte in the length of keys.  There is a
 *	     tool distributed with smail (mkline) that can be used to
 *	     create regular YP databases, for use with the `yp' proto.
 * nialias - use NeXTs netinfo mail alias table
 * nisplus - use Sun's NIS+ (the follow-on to YP) for remote table access.
 *
 * #define ALIASES_FILE	"/usr/lib/smail/aliases" (* Suggestion *)
 * #define ALIASES_FILE	"/usr/smail/lib/aliases" (* alternate *)
 */
#if	!defined(ALIASES_FILE) && !defined(NO_ALIASES_FILE)
# define ALIASES_FILE "/usr/lib/aliases" /* sendmail compatible name */
#endif
/* NOTE: this is derrived from ALIASES_TYPE in conf/EDITME by conf/lib/mkdefs.sh */
#ifndef	ALIASES_PROTO
# define ALIASES_PROTO "lsearch"	/* This should work on any UN*X os */
#endif

/*
 * In some environments where smail is being integrated into new systems
 * it may be convenient to make the aliases file optional, so that if
 * the file does not exist, it is assumed to be empty.  To enable this
 * behavior define the name below.
 */
#if	!defined(ALIASES_OPTIONAL) && !defined(NO_ALIASES_OPTIONAL)
# ifndef HAVE_YP
#  define ALIASES_OPTIONAL		/*  */
# endif
#endif

/*
 * In some cases, failure to open an aliases database can be considered
 * a temporary failure which can be recovered from by retrying the open
 * at a later time.  One example would be use of YP where the server
 * host may be down.  To enable this behavior, define the name below.
 */
#if	!defined(ALIASES_TRYAGAIN) && !defined(NO_ALIASES_TRYAGAIN)
# if defined(HAVE_YP) || defined(HAVE_NISPLUS)
#  define ALIASES_TRYAGAIN		/*  */
# endif
#endif

/*
 * configuration for the default dotforward director
 *
 * If you do not wish to support ~/.forward files, for some strange
 * reason, define DISABLE_DOTFORWARD.  forward files in smail are handled
 * in a reasonable manner which should prevent use of .forward files for
 * trojan horse attacks.  For example, if correctly configured home
 * directories accessible by remote hosts can't be used to write files or
 * exec programs.
 */
#if	!defined(DISABLE_DOTFORWARD) && !defined(NO_DISABLE_DOTFORWARD)
/*#define DISABLE_DOTFORWARD			/* you probably don't want this! */
#endif

/*
 * The following list should define all home directories known to be
 * accessible from remote hosts, so that care can be taken for
 * .forward files found in these places.
 */
#ifndef REMOTE_HOMES
# define REMOTE_HOMES "~nuucp:~uucp:~ftp:/tmp:/usr/tmp"
#endif

/*
 * configuration for the default forwardto director
 *
 * If you wish to support users putting "Forward to " lines at the
 * beginning of their mailbox files to indicate forwarding, then
 * define how to find these mailbox files.  System V mailers currently
 * do not have any other means of specifying forwarding information
 * for users.  This maintains compatibility with this system.
 */
#if	!defined(FORWARDTO_FILE) && !defined(NO_FORWARDTO_FILE)
# ifdef LOCAL_MAIL_FILE
#  define FORWARDTO_FILE LOCAL_MAIL_FILE
# else
#  ifdef UNIX_SYS5
#   define FORWARDTO_FILE "/usr/mail/${lc:user}"
#  else
#   define FORWARDTO_FILE "/usr/spool/mail/${lc:user}"
#  endif
# endif
#endif

/*
 * configuration for the default mailing list director
 *
 * A variation on the forwardfile driver can be used to define a
 * directory which contains mailing list files.  By simply creating
 * a file in a directory a mailing list will have been defined.
 * aliases, forward files and local usernames have precedence over
 * these mailing list files, so it is safe to allow general users
 * access to this directory (e.g., users cannot use this directory
 * to steal mail from other users on the local host).
 *
 * In the spirit of allowing general users access to this file, the
 * caution and nobody attributes are turned on.  This prevents worries
 * about users being able to put shell command and file addresses in
 * these files and accessing things that they shouldn't.  However, it
 * still allows users the convenience of specifying files and shell
 * commands as destinations.
 *
 * Under a 4.3BSD system or a Sun running SunOS3.0 or higher, it is
 * reasonable to set the sticky bit on this directory.  In this case a
 * user will be able to create a file here with assurance that other
 * users will not be able to remove rename it.  However, any system
 * that is comfortable with normal UN*X /tmp and /usr/tmp semantics,
 * should also be comfortable with a globally writable mailing list
 * directory.
 *
 * If these definitions do not begin with `/', they will be referenced
 * relative to the smail_lib_dir directory.
 *
 * #define LIST_FILENAME "/usr/smail/lists/${lc:user}" (* alternative *)
 * #define LIST_OWNER_FILENAME "/usr/smail/lists/owner-${lc:user}" (* alternative *)
 */
#if	!defined(LIST_FILENAME) && !defined(NO_LIST_FILENAME)
# define LIST_FILENAME "lists/${lc:user}"	/* suggested pathname */
# define LIST_OWNER_FILENAME "owner-${lc:user}"	/* suggested pathname */
#endif
#if defined(LIST_FILENAME) && !defined(LIST_OWNER_FILENAME)
# include "CONFIG_ERROR: LIST_OWENR_FILENAME must be defined with LIST_FILENAME."
#endif

/*
 * configuration for the default smartuser director
 *
 * Smail can be configured to send mail destined to an unknown user to
 * a remote site that supposedly understands more usernames than does
 * the local host.  By defining an address to which mail to unknown
 * users should be sent, this can be accomplished.  This address should
 * contain a $user where the local address is inserted into the address.
 *
 * If SMART_USER is set to NULL, then a smartuser director is configured
 * which will read the variable smart_user which can be set in the
 * config file.  The default value of the smart_user variable can be
 * specified as CONFIG_SMART_USER.  This enables a smart_user director
 * to be compiled in while the address for the smart_user director is
 * still modifiable through configuration.
 *
 * #define SMART_USER "$user@amdahl.uts.amdahl.com" (* sample setting *)
 */
#if	!defined(SMART_USER) && !defined(NO_SMART_USER)
# define SMART_USER NULL		/* normal setting */
#endif

#ifndef	CONFIG_SMART_USER
# define CONFIG_SMART_USER  NULL
#endif


/*
 * DEFAULT ROUTER CONFIGURATION
 *
 * This section adjusts the default router configuration compiled into
 * smail.  See default.c for the complete default configuration source.
 *
 * NOTE:  The existence of a router configuration file replaces all of
 *	  this configuration.
 */

/*
 * There is currently no support for method files from the compiled in
 * smail configuration.  However, a hardcoded method table can be used
 * by defining USE_METHOD_TABLE.  If defined, all of the routers which
 * use UUCP in one form or another as a transport will use this table
 * in addition to their default associated transport.
 * Routers which use a direct SMTP transport won't use this table.
 * In the future, smail may support method files from the compiled in
 * configuration.
 */
#if	!defined(USE_METHOD_TABLE) && !defined(NO_USE_METHOD_TABLE)
/*# define USE_METHOD_TABLE		/* you probably don't want this! */
#endif


/*
 * Default gethostbyaddr router setup
 *
 * If defined, a gethostbyaddr router will be configured which recognizes
 * hostnames such as [192.2.12.142] and delivers via SMTP to that IP
 * address.  This requires that the gethostbyaddr driver be configured
 * in by the conf/driver.cf configuration file.  This in turn requires
 * the a BSD-compatible networking library.
 */
#ifdef HAVE_BSD_NETWORKING
# if	!defined(USE_GETHOSTBYADDR) && !defined(NO_USE_GETHOSTBYADDR)
#  define USE_GETHOSTBYADDR		/*  */
# endif
#endif

/*
 * GETHOSTBYADDR_TRANSPORT defines the default transport used for hosts
 * matched with the pathalias router.
 *
 * #define GETHOSTBYADDR_TRANSPORT  "inet_zone_smtp" (* alternate suggestion *)
 */
#ifndef GETHOSTBYADDR_TRANSPORT
# define GETHOSTBYADDR_TRANSPORT "smtp"
#endif


/*
 * Default bind router setup
 *
 * If defined, a bind router will be configured which calls the
 * resolve(3N) to match DNS hosts accessible over TCP/IP.  This requires
 * that the gethostbyname driver be configured in by the conf/driver.cf
 * configuration file.  This in turn requires the a BSD-compatible
 * networking library, and BIND/DNS support.
 */
#if defined(HAVE_BSD_NETWORKING) && defined(HAVE_BIND)
# if	!defined(USE_BIND) && !defined(NO_USEBIND)
#  define USE_BIND		/*  */
# endif
#endif

/*
 * BIND_TRANSPORT defines the default transport used for hosts
 * matched with the pathalias router.
 *
 * NOTE:  You probably want to ensure use of a *_bind_smtp transport...
 */
#ifndef BIND_TRANSPORT
# define BIND_TRANSPORT "inet_zone_bind_smtp"
#endif


/*
 * Default gethostbyname router setup
 *
 * If defined, a gethostbyname router will be configured which calls
 * gethostbyname(3N) to match hosts accessible over TCP/IP.  This requires
 * that the gethostbyaddr driver be configured in by the conf/driver.cf
 * configuration file.  This in turn requires the a BSD-compatible
 * networking library.
 */
#ifdef HAVE_BSD_NETWORKING
# if	!defined(USE_GETHOSTBYNAME) && !defined(NO_USEGETHOSTBYNAME)
#  define USE_GETHOSTBYNAME		/*  */
# endif
#endif

/*
 * GETHOSTBYNAME_TRANSPORT defines the default transport used for hosts
 * matched with the pathalias router.
 *
 * #define GETHOSTBYNAME_TRANSPORT  "inet_zone_smtp" (* alternate suggestion *)
 */
#ifndef GETHOSTBYNAME_TRANSPORT
# define GETHOSTBYNAME_TRANSPORT "smtp"
#endif


/*
 * Default pathalias router setup
 *
 * If you wish to have a pathalias router by default, define PATHS_FILE
 * to be the name of a sorted paths file, and PATHS_PROTO to be the
 * database access protocol used for lookups.  See the file lookup.c
 * for a complete list of these protocols.    If this does not begin
 * with `/', it will be referenced relative to the smail_lib_dir
 * directory.  A current list is:
 *
 * lsearch - perform linear searches on raw path files.  This is slow
 *	     unless the path file is small.
 * bsearch - perform binary searches on sorted path files with one
 *	     path per line.
 * dbm	   - use the Berkeley dbm(3X) or ndbm(3X) libraries to search
 *	     for paths.
 * yp	   - use Sun's YP service to search for paths.  This requires
 *	     the existence of the YP library routines.
 *
 * #define PATHS_FILE	"/usr/smail/lib/paths" (* alternate suggestion *)
 */
#if	!defined(PATHS_FILE) && !defined(NO_PATHS_FILE)
# define PATHS_FILE	"paths"		/* Suggestion */
#endif
/* NOTE: this is derrived from PATHS_TYPE in conf/EDITME by conf/lib/mkdefs.sh */
#ifndef	PATHS_PROTO
# define PATHS_PROTO	"bsearch"	/* Compatible with smail1.0 and 2.0 */
#endif

/*
 * In some environments where smail is being integrated into new systems
 * it may be convenient to make the paths file optional, so that if
 * the file does not exist, it is assumed to be empty.  To enable this
 * behavior define the name below.
 */
#if	!defined(PATHS_OPTIONAL) && !defined(NO_PATHS_OPTIONAL)
# define PATHS_OPTIONAL			/*  */
#endif

/*
 * In some cases, failure to open an paths database can be considered
 * a temporary failure which can be recovered from by retrying the open
 * at a later time.  One example would be use of YP where the server
 * host may be down.  To enable this behavior, define the name below.
 */
#if	!defined(PATHS_TRYAGAIN) && !defined(NO_PATHS_TRYAGAIN)
/*# define PATHS_TRYAGAIN		/* you probably don't want this! */
#endif

/*
 * PATHS_TRANSPORT defines the default transport used for hosts matched
 * with the pathalias router.
 *
 * #define PATHS_TRANSPORT  "demand"	(* alternate suggestion *)
 */
#ifndef	PATHS_TRANSPORT
# define PATHS_TRANSPORT  "uux"		/* queue remote rmail requests */
#endif


/*
 * Default uuname router setup
 *
 * If you wish to obtain a list of neighbor sites from the UUCP programs,
 * define the command to extract the list of UUCP neighbor sites.
 */
#if	!defined(UUNAME_COMMAND) && !defined(NO_UUNAME_COMMAND)
# define UUNAME_COMMAND	"/usr/bin/uuname" /* should work almost everywhere */
#endif

/*
 * UUNAME_TRANSPORT defines the default transport used for hosts matched
 * with the uuname router.
 *
 * #define UUNAME_TRANSPORT  "demand"	(* alternate suggestion *)
 */
#ifndef UUNAME_TRANSPORT
# define UUNAME_TRANSPORT  "uux"	/* queue remote rmail requests */
#endif


/*
 * Default setup for the smarthost router
 *
 * If you wish to send unknown addresses to a remote host for routing,
 * define the path used to send mail to that host.  Ask the remote
 * site in question before setting this up.
 *
 * There is a hook in the config file that allows the path and perhaps
 * the transport used to be set in the configuration file.  This hook
 * can be enabled by setting SMART_PATH to NULL and CONFIG_SMART_PATH
 * to the initial value for the smart_path variable.  As well,
 * CONFIG_SMART_TRANSPORT can be set to the default value for the
 * smart_transport variable, though SMART_TRANSPORT can be used instead
 * in any case.
 *
 * Using CONFIG_SMART_PATH is the prefered method of setting up the
 * smarthost router as it does not rely on hard-coded information in
 * default.c.
 *
 * if SMART_PATH is undefined, no smarthost router will be configured.
 *
 * #define SMART_PATH  "namei!amdahl"	(* example path *)
 */
#if	!defined(SMART_PATH) && !defined(NO_SMART_PATH)
# define SMART_PATH NULL		/*  */
#endif

#if	!defined(CONFIG_SMART_PATH)
# define CONFIG_SMART_PATH NULL		/*  */
#endif

#if	!defined(CONFIG_SMART_TRANSPORT)
# define CONFIG_SMART_TRANSPORT NULL	/*  */
#endif

/*
 * SMART_TRANSPORT defines the default transport used for hosts matched
 * with the uuname router.
 *
 * #define SMART_TRANSPORT "demand"	(* alternate suggestion *)
 */
#if	!defined(SMART_TRANSPORT)
# define SMART_TRANSPORT "uux"		/* Suggestion */
#endif


/*
 * Default force_paths router setup
 *
 * If you wish to have a pathalias router by default, define FORCE_PATHS_FILE
 * to be the name of a sorted paths file, and FORCE_PATHS_PROTO to be the
 * database access protocol used for lookups.  See the file lookup.c
 * for a complete list of these protocols.    If this does not begin
 * with `/', it will be referenced relative to the smail_lib_dir
 * directory.  A current list is:
 *
 * lsearch - perform linear searches on raw path files.  This is slow
 *	     unless the path file is small.
 * bsearch - perform binary searches on sorted path files with one
 *	     path per line.
 * dbm	   - use the Berkeley dbm(3X) or ndbm(3X) libraries to search
 *	     for paths.
 * yp	   - use Sun's YP service to search for paths.  This requires
 *	     the existence of the YP library routines.
 *
 * #define FORCE_PATHS_FILE	"/usr/smail/lib/forcepaths" (* alternate suggestion *)
 */
#if	!defined(FORCE_PATHS_FILE) && !defined(NO_FORCE_PATHS_FILE)
# define FORCE_PATHS_FILE	"forcepaths"	/* Suggestion */
#endif
/* NOTE: this is derrived from FORCE_PATHS_TYPE in conf/EDITME by conf/lib/mkdefs.sh */
#ifndef	FORCE_PATHS_PROTO
# define FORCE_PATHS_PROTO	"bsearch"	/* Suggestion */
#endif

/*
 * The force_paths file is always considered optional, so that if
 * the file does not exist, it is assumed to be empty.
 */
#if	!defined(FORCE_PATHS_OPTIONAL) && !defined(NO_FORCE_PATHS_OPTIONAL)
# define FORCE_PATHS_OPTIONAL			/*  */
#endif

/*
 * In some cases, failure to open an paths database can be considered
 * a temporary failure which can be recovered from by retrying the open
 * at a later time.  One example would be use of YP where the server
 * host may be down.  To enable this behavior, define the name below.
 */
#if	!defined(FORCE_PATHS_TRYAGAIN) && !defined(NO_FORCE_PATHS_TRYAGAIN)
/*# define FORCE_PATHS_TRYAGAIN			/* you probably don't want this! */
#endif

/*
 * FORCE_PATHS_TRANSPORT defines the default transport used for hosts matched
 * with the pathalias driver for the force_paths router.
 *
 * #define FORCE_PATHS_TRANSPORT  "demand"	(* alternate suggestion *)
 */
#ifndef	FORCE_PATHS_TRANSPORT
# define FORCE_PATHS_TRANSPORT	"uux"		/* queue remote rmail requests */
#endif


/*
 * Default force_smtp router setup
 *
 * If you wish to have a pathalias router by default, define FORCE_SMTP_FILE
 * to be the name of a sorted paths file, and FORCE_SMTP_PROTO to be the
 * database access protocol used for lookups.  See the file lookup.c
 * for a complete list of these protocols.    If this does not begin
 * with `/', it will be referenced relative to the smail_lib_dir
 * directory.  A current list is:
 *
 * lsearch - perform linear searches on raw path files.  This is slow
 *	     unless the path file is small.
 * bsearch - perform binary searches on sorted path files with one
 *	     path per line.
 * dbm	   - use the Berkeley dbm(3X) or ndbm(3X) libraries to search
 *	     for paths.
 * yp	   - use Sun's YP service to search for paths.  This requires
 *	     the existence of the YP library routines.
 *
 * #define FORCE_SMTP_FILE	"/usr/smail/lib/forcesmtp" (* alternate suggestion *)
 */
#if	!defined(FORCE_SMTP_FILE) && !defined(NO_FORCE_SMTP_FILE)
# define FORCE_SMTP_FILE	"forcesmtp"	/* Suggestion */
#endif
/* NOTE: this is derrived from FORCE_SMTP_TYPE in conf/EDITME by conf/lib/mkdefs.sh */
#ifndef	FORCE_SMTP_PROTO
# define FORCE_SMTP_PROTO	"bsearch"	/* Suggestion */
#endif

/*
 * The force_smtp file is always considered optional, so that if
 * the file does not exist, it is assumed to be empty.
 */
#if	!defined(FORCE_SMTP_OPTIONAL) && !defined(NO_FORCE_SMTP_OPTIONAL)
# define FORCE_SMTP_OPTIONAL			/*  */
#endif

/*
 * In some cases, failure to open an paths database can be considered
 * a temporary failure which can be recovered from by retrying the open
 * at a later time.  One example would be use of YP where the server
 * host may be down.  To enable this behavior, define the name below.
 */
#if	!defined(FORCE_SMTP_TRYAGAIN) && !defined(NO_FORCE_SMTP_TRYAGAIN)
/*# define FORCE_SMTP_TRYAGAIN			/* you probably don't want this! */
#endif

/*
 * FORCE_SMTP_TRANSPORT defines the default transport used for hosts matched
 * with the pathalias driver for the force_smtp router.
 *
 * #define FORCE_SMTP_TRANSPORT  "inet_zone_smtp" (* alternate suggestion *)
 */
#ifndef	FORCE_SMTP_TRANSPORT
# define FORCE_SMTP_TRANSPORT	"smtp"		/* deliver directly with smtp */
#endif



/*
 * DEFAULT TRANSPORT CONFIGURATION
 *
 * This section adjusts the default transport configuration compiled into
 * smail.  See default.c for the complete default configuration source.
 *
 * NOTE:  The existence of a transport configuration file replaces all of
 *	  this configuration.
 */

/*
 * Default setup for the local transport
 *
 * Smail can perform local delivery either by appending to mailboxes
 * by itself (only useful if it is setuid or setgid to something that
 * can do this, or if all mail is queued and smail performs all delivery
 * as a daemon).  Alternately, it can send messages to another program,
 * such as /bin/mail, or /usr/libexec/mail.local and have that program
 * perform delivery.
 *
 * The method used depends upon what is set.  If LOCAL_MAIL_COMMAND
 * is set, it should be a command to be executed which will deliver
 * a message given on its standard input.  Otherwise LOCAL_MAIL_FILE
 * should be set to define which file should be appended to for
 * local mail delivery.  See the installation and operators guide
 * for information on what the command and filename strings look
 * like.
 *
 * Note that the program called as LOCAL_MAIL_COMMAND must enforce all
 * of the local requirements of a ``unix'' mailbox format, which are
 * normally as follows (from the 4.4bsd mail.local(8) manual page):
 *
 *    Individual mail messages in the mailbox are delimited by an empty line
 *    followed by a line beginning with the string ``From ''.  A line contain-
 *    ing the string ``From '', the sender's name and a time stamp is prepended
 *    to each delivered mail message.  A blank line is appended to each mes-
 *    sage.  A greater-than character (``>'') is prepended to any line in the
 *     message which could be mistaken for a ``From '' delimiter line.
 *
 * #define LOCAL_MAIL_COMMAND "/path/prog $($user$)" (* custom program *)
 * #define LOCAL_MAIL_FILE "~/mbox"	(* Example: file in home directories *)
 */
#if defined(USE_LOCAL_MAIL_COMMAND) && !defined(NO_USE_LOCAL_MAIL_COMMAND)
# if !defined(LOCAL_MAIL_COMMAND)
#  ifdef UNIX_BSD4_4
#   define LOCAL_MAIL_COMMAND "/usr/libexec/mail.local -f ${shquote:sender} $(${lc:user}$)"
#  else
#   define LOCAL_MAIL_COMMAND "/bin/mail -d $(${lc:user}$)"
#  endif
# endif
#endif

#if !defined(LOCAL_MAIL_COMMAND) && !defined(LOCAL_MAIL_FILE)
# ifndef LOCAL_MAIL_COMMAND
#  if defined(UNIX_SYS5) && !defined(UNIX_SYS5_4)
#   define LOCAL_MAIL_FILE "/usr/mail/${lc:user}" /* traditional */
#  else
#   if defined(UNIX_BSD4_4) || defined(UNIX_BSD_4_3) || defined(UNIX_SYS5_4)
#    define LOCAL_MAIL_FILE "/var/mail/${lc:user}" /* modern */
#   else
#    if defined(UNIX_SUN_OS_4_1)
#     define LOCAL_MAIL_FILE "/usr/spool/mail/${lc:user}" /* traditional */
#    endif
#   endif
#  endif
# endif
#endif

/*
 * if a program is used for local delivery, set LOCAL_MAIL_ADDRS to
 * the maximum number of addrs that the program can take.  Generally,
 * a program can either take one or can take many.
 */
#ifndef	LOCAL_MAIL_ADDRS
# define LOCAL_MAIL_ADDRS	100	/* pick any large number */
#endif

/*
 * if smail is doing delivery to mailbox files directly, these files will
 * be created with this mode.
 */
#ifndef LOCAL_MAIL_MODE
# ifdef UNIX_BSD
#  define LOCAL_MAIL_MODE	0600	/* only user can read/write mail */
# else
#  define LOCAL_MAIL_MODE	0660	/* mailboxes in restricted group */
# endif
#endif

/*
 * Setup for the queued uux transport
 *
 * The following command should call uux and queue a mail message
 * for transmission to a remote host.  Check your man page to see
 * if uux supports the -amailpath option, and also check to see
 * if you have the -ggrade option.  Smail can support both of these
 * quite nicely.
 */
#ifndef	QUEUED_UUX_COMMAND
# ifdef HAVE_HDB_UUCP
#  define QUEUED_UUX_COMMAND \
	"/usr/bin/uux - -r -a$sender -g$grade $host!rmail $(($user)$)"
# else
#  define QUEUED_UUX_COMMAND \
	"/usr/bin/uux - -r $host!rmail $(($user)$)"
# endif
#endif

/*
 * Setup for the demand uux transport
 *
 * The following command should call uux and queue a mail message
 * for transmission to a remote host and immediately attempt delivery.
 * Check your man page to see if uux supports the -amailpath option, and
 * also check to see if you have the -ggrade option.  Smail can support
 * both of these quite nicely.
 */
#ifndef	DEMAND_UUX_COMMAND
# ifdef HAVE_HDB_UUCP
#  define DEMAND_UUX_COMMAND \
	"/usr/bin/uux - -a$sender -g$grade $host!rmail $(($user)$)"
# else
#  define DEMAND_UUX_COMMAND \
	"/usr/bin/uux - $host!rmail $(($user)$)"
# endif
#endif

/*
 * Setup for the queued uusmtp transport
 *
 * The following command should call uux and queue a mail message
 * for transmission to a remote host.  It should invoke a program
 * on the remote host that reads batched SMTP requests from its
 * standard input.
 */
#ifndef	QUEUED_UUSMTP_COMMAND
# ifdef HAVE_HDB_UUCP
#  define QUEUED_UUSMTP_COMMAND \
	"/usr/bin/uux - -r -a$sender -g$grade $host!rsmtp"
# else
#  define QUEUED_UUSMTP_COMMAND \
	"/usr/bin/uux - -r $host!rsmtp"
# endif
#endif

/*
 * Setup for the demand uusmtp transport
 *
 * The following command should call uux and queue a mail message
 * for immediate transmission to a remote host.  It should invoke
 * a program on the remote host that reads batched SMTP requests
 * from its standard input.
 */
#ifndef	DEMAND_UUSMTP_COMMAND
# ifdef HAVE_HDB_UUCP
#  define DEMAND_UUSMTP_COMMAND \
	"/usr/bin/uux - -a$sender -g$grade $host!rsmtp"
# else
#  define DEMAND_UUSMTP_COMMAND \
	"/usr/bin/uux - $host!rsmtp"
# endif
#endif

/*
 * Setup for the smtp transport
 *
 * If you wish to configure the SMTP over TCP/IP transports
 * then define this.  If defined, four transports will be configured:
 *
 *	smtp		- default smtp transport (either INET or UUCP zone)
 *	uucp_zone_smtp	- smtp transport for UUCP zone
 *	inet_zone_smtp	- smtp transport for the Internet
 *	local_smtp	- smtp transport for coordinated local networks
 *	uucp_zone_bind_smtp	- smtp transport for UUCP zone w/TCPSMTP_USE_BIND
 *	inet_zone_bind_smtp	- smtp transport for the Internet w/TCPSMTP_USE_BIND
 *	local_bind_smtp	- smtp transport for coordinated local networks w/TCPSMTP_USE_BIND
 */
#ifdef HAVE_BSD_NETWORKING
# if	!defined(USE_SMTP_TRANSPORT) && !defined(NO_USE_SMTP_TRANSPORT)
#  define USE_SMTP_TRANSPORT
# endif
#endif

/*
 * Delivery control by message grade
 *
 * runq_grades variable defines which messages are processed
 * during a queue run (selected by grades).  Only the grades in the
 * simple grade range (ie "a-z", no commas etc....)  will be processed.
 * The default value for this is NULL (or "-") - meaning all grades.
 *
 * The delivery_grades variable similarly defines a range of grades
 * which will be delivered immediately, rather than queued up for
 * later transmission.
 */
#ifndef RUNQ_GRADES
# define RUNQ_GRADES NULL		/*  */
#endif

#ifndef DELIVERY_GRADES
# define DELIVERY_GRADES NULL		/*  */
#endif

/*
 * Timeout of directors and routers for multiple defers
 *
 * This determines at what point a message that has been defered
 * in a router or director is bounced, otherwise can stick in a
 * defered state for ever
 */
#ifndef RESOLVE_TIMEOUT
#  define RESOLVE_TIMEOUT (3 * 86400)	/* 3 days */
#endif
