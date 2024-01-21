/*
 * CFINGERD - Configurable Finger-Query Daemon
 * Header File
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 1, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 */

#ifndef _CFINGERD_H_
#define _CFINGERD_H_

#include <stdio.h>
#include <stdlib.h>
#include <syslog.h>
#include <string.h>
#include <signal.h>
#include <fcntl.h>
#include <unistd.h>
#include <netdb.h>
#include <errno.h>
#include <utmp.h>
#include <ctype.h>
#include <time.h>
#include <pwd.h>

#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <arpa/inet.h>

#include "config.h"

#define	BOOL	int

#define	TRUE	1
#define	FALSE	0

/* First configuration option bit pair */
#define	SHOW_SYSLOG	0x0001	/* Log all errors to the syslog daemon	*/
#define	SHOW_TOP	0x0002	/* Show the top file on a finger req.	*/
#define	SHOW_BOTTOM	0x0004	/* Show the bottom file on a finger req.*/
#define	SHOW_UNAME	0x0008	/* Show the user's login name		*/
#define	SHOW_REALNAME	0x0010	/* Show the user's real name		*/
#define	SHOW_DIR	0x0020	/* Show the user's home directory	*/
#define	SHOW_SHELL	0x0040	/* Show the user's shell program	*/
#define	SHOW_ROOM	0x0080	/* Show the user's room number		*/
#define	SHOW_WPHONE	0x0100	/* Show the user's work phone number	*/
#define	SHOW_HPHONE	0x0200	/* Show the user's home phone number	*/
#define	SHOW_OTHER	0x0400	/* Show the user's other information	*/
#define	SHOW_LTON	0x0800	/* Show the last time this user was on	*/
#define	SHOW_IFON	0x1000	/* Show if this user is currently on	*/
#define	SHOW_LRMAIL	0x2000	/* Show the last time mail was read	*/
#define	SHOW_MRDATE	0x4000	/* Show the last day that mail was read	*/
#define	SHOW_FROM	0x8000	/* Show where the last login originated	*/

/* Second configuration option bit pair -- ALWAYS room for improvement */
#define	SHOW_PLAN	0x0001	/* Show the user's plan file		*/
#define	SHOW_PROJECT	0x0002	/* Show the user's project file		*/
#define	SHOW_PGPKEY	0x0004	/* Show the user's PGP key file		*/
#define	SHOW_NN_BANNER	0x0008	/* Show banner if no name specified	*/
#define SHOW_REJECTED	0x0010	/* Show the rejected file if wanted	*/
#define	SHOW_SYSTEMLIST	0x0020	/* Show the user listing if wanted	*/
#define	SHOW_IP_MATCH	0x0040	/* Show if Hostname not found from IP	*/
#define	SHOW_NOUSER	0x0080	/* Show no-user matched banner		*/
#define	SHOW_MULTFING	0x0100	/* Allow for multiple finger displays	*/
#define	SHOW_SEARCHFING	0x0200	/* Allow for searchable finger queries	*/
#define	SHOW_OVERRIDE	0x0400	/* Allow for user-overridable options	*/
#define	SHOW_STRICTFMT	0x0800	/* Allow for strict formatting output	*/
#define	SHOW_TIMESTAMP	0x1000	/* Allow for verbose timestamps		*/
#define	SHOW_FINGERFWD	0x2000	/* Allow for forwardable fingers	*/
#define	SHOW_ULISTONLY	0x4000	/* Allow for "userlist-only" fingers	*/
#define	SHOW_NOBODY1413	0x8000	/* Allow for non-ident users to finger  */

/* Third row of configuration bits */
#define	SHOW_LOG	0x0001	/* Allow for logging of all fingers	*/
#define	SHOW_PARSING	0x0002	/* Allow for text line data parsing	*/
#define	SHOW_USERLOG	0x0004	/* Allow for userlogging of all fingers */
#define	SHOW_EXEC	0x0008	/* Allow for users to execute programs	*/
#define	SHOW_FAKEUSER	0x0010	/* Allow fakeusers to be fingered	*/
#define	SHOW_XFACE	0x0020	/* Allow display of xface files		*/
#define	SHOW_HEADERS_FE	0x0040	/* Show headers if file exists		*/
#define	SHOW_CREATE_FLG	0x0080	/* Create fingerlog file if file exists	*/

/* Program exit states */
#define	PROGRAM_OKAY	0	/* Program exited fine			*/
#define	PROGRAM_SYSLOG	1	/* Program exited after a syslog report	*/
#define	PROGRAM_BUG	2	/* Program exited because of a bug	*/

/* Display strings (This could increase shortly) */
#define	D_USERNAME	0	/* Username string			*/
#define	D_REALNAME	1	/* Realname string			*/
#define	D_DIRECTORY	2	/* Directory string			*/
#define	D_SHELL		3	/* Shell string				*/
#define	D_ROOM		4	/* Room number string			*/
#define	D_WORK_PHONE	5	/* Work phone number string		*/
#define	D_HOME_PHONE	6	/* Home phone number string		*/
#define	D_OTHER		7	/* Other string				*/
#define	D_IP_NO_MATCH	8	/* No hostname matches IP string	*/
#define	D_NICE_FATAL	9	/* Nice-routine fatality string		*/
#define	D_STDIN_EMPTY	10	/* STDIN empty string			*/
#define	D_TRUST_HOST	11	/* Trusted host display string		*/
#define	D_REJECT_HOST	12	/* Rejected host display string		*/
#define	D_ROOT_FINGER	13	/* Root finger display string		*/
#define	D_SVC_FINGER	14	/* Service finger display string	*/
#define	D_ULIST_FINGER	15	/* Userlist finger display string	*/
#define	D_FAKE_USER	16	/* Fake user display string		*/
#define	D_WHOIS_USER	17	/* Whois user display string		*/
#define	D_PLAN		18	/* Plan display string			*/
#define	D_PROJECT	19	/* Project display string		*/
#define	D_PGPKEY	20	/* PGP key display string		*/
#define	D_NO_PLAN	21	/* No plan file display string		*/
#define	D_NO_PROJECT	22	/* No project file display string	*/
#define	D_NO_PGPKEY	23	/* No PGP key display string		*/
#define	D_WAIT		24	/* "Wait while I do something" string	*/
#define	D_XFACE		25	/* XFace display string			*/
#define	D_NO_XFACE	26	/* No XFace file display string		*/
#define D_FORWARD_DENY	27	/* No Forwarding string			*/
#define	D_IDENT_CONREF	28	/* Connection refused for RFC1413 report*/
#define	D_IDENT_ILLEGAL	29	/* Illegal data in RFC1413 report	*/
#define	D_IDENT_TIMEOUT	30	/* RFC1413 report timed out		*/

/* General program errors */
#define	E_STANDALONE	1	/* Cannot run in standalone mode	*/
#define	E_NONROOT	2	/* Must be root to run			*/
#define	E_INETD		3	/* Must be run in inetd			*/
#define	E_CONFIG	4	/* Configuration file error		*/
#define	E_SIGSEGV	5	/* SIGSEGV detected			*/
#define	E_SUIDROOT	6	/* Cannot be suid root			*/
#define	E_COMMANDLINE	7	/* Wrong number of command line options	*/
#define	E_FINGEROFF	8	/* Turn off all incoming fingers	*/
#define	E_WILDCARD	9	/* Wildcard settings contradict		*/
#define	E_SEARCH	10	/* Search settings turned off		*/
#define	E_NOIP		11	/* No DNS entry found for IP address	*/

/* Processed username return values */
#define	U_ILLEGAL	-1	/* Illegal username was encountered	*/
#define	U_STANDARD	1	/* User is not fake and not internal	*/
#define	U_FAKEUSER	2	/* User is a fake user (user-defined)	*/
#define	U_INTERNAL	3	/* User is an internal-defined username	*/
#define	U_USERLIST	4	/* User is requesting a userlist	*/
#define	U_FORWARD	8	/* User is a forward request		*/

/* Standard signals for use within the program */
#define	S_SIGHUP	1	/* Hangup signal			*/
#define	S_SIGINT	2	/* Interruption signal			*/
#define	S_SIGQUIT	3	/* Quit signal				*/
#define	S_SIGILL	4	/* Illegal exception signal		*/
#define	S_SIGTRAP	5	/* Trap/Breakpoint signal		*/
#define	S_SIGABRT	6	/* Abort signal				*/
#define	S_SIGFPE	7	/* Floating point exception signal	*/
#define	S_SIGUSR1	8	/* User defined signal #1		*/
#define	S_SIGSEGV	9	/* Segmentation violation signal	*/
#define	S_SIGUSR2	10	/* User defined signal #2		*/
#define	S_SIGPIPE	11	/* Broken pipe signal			*/
#define	S_SIGALRM	12	/* Alarm timeout signal			*/
#define	S_SIGTERM	13	/* Termination signal			*/
#define	S_SIGCONT	14	/* Continue signal			*/
#define	S_SIGTSTP	15	/* TTY Stopped signal			*/
#define	S_SIGTTIN	16	/* TTY Input stopped signal		*/
#define	S_SIGTTOU	17	/* TTY Output stopped signal		*/
#define	S_SIGIO		18	/* Input/output stopped signal		*/
#define	S_SIGXCPU	19	/* CPU Exception signal			*/
#define	S_SIGXFSZ	20	/* Exceeded file size signal		*/
#define	S_SIGVTALRM	21	/* Virtual Alarm signal			*/
#define	S_SIGPROF	22	/* Prof (??) signal			*/
#define	S_SIGWINCH	23	/* Window size change signal		*/

/* This was defined in syslog ... I'm using it for my own use. */
#undef	LOG_USER

/* Logging defines */
#define	LOG_IDENT	1
#define	LOG_REJECTED	2
#define	LOG_WARN	3
#define	LOG_USER	4
#define	LOG_ERROR	5

typedef struct {
    char *user;			/* Fake username */
    char *script;		/* Fake user's corresponding script */
    char *description;		/* Script description */
    BOOL searchable;		/* Searchable boolean ... are we searchable? */
} FAKEUSERS;

typedef struct {
    char *header[20];		/* Five headers for the top display */
    char *display_string;	/* Display string in printf format */
    int name_pos;		/* Position in the string to display name */
    int service_pos;		/* Position in the string to display service */
    int search_pos;		/* Position in the string to display search */
} SERVICES;

typedef struct {
    char *top_display_file;	/* Displayed at the top of the listing */
    char *bottom_display_file;	/* Displayed at the close of the listing */
    char *plan_file;		/* The generic plan file to use */
    char *project_file;		/* The generic project file to use */
    char *pgpkey_file;		/* The generic PGP file to use */
    char *xface_file;		/* The generic XFACE file to use */
    char *log_file;		/* The generic log file to use */
    char *userlog_file;		/* The generic user log file to use */
    char *mailbox_file;		/* The mail box that is used for checking */
    char *no_name_banner_file;	/* Banner file used if no name specified */
    char *no_user_banner_file;	/* Banner file used if no user is matched */
    char *no_finger_file;	/* File used if no finger is wanted */
    char *identd_banner_file;	/* Banner file used if identd is unknown */
    char *rejected_file;	/* Rejected host text file display */
    char *syslog_file;		/* Syslog output file */
    char *finger_program;	/* Program to run when no name is queried */
    char *whois_program;	/* Program to run when /W name is queried */
    char *rejected[80];		/* Rejected hosts */
    char *trusted[80];		/* Trusted hosts */
    char *forward[80];		/* Forwarded hosts */
    char *p_strings[80];	/* Program-related context print strings */
    char *finger_sites[80];	/* Finger sites that are queried */
    char *siglist[24];		/* Signal names and their errors */
    FAKEUSERS fusers[40];	/* Fake user name listings */
    SERVICES services;		/* Services configuration display */
    int  config_bits1;		/* First row of configuration bits */
    int  config_bits2;		/* Second row of configuration bits */
    int  config_bits3;		/* Third row of configuration bits */
    int  local_config_bits1;	/* Local first row of config bits */
    int  local_config_bits2;	/* Local second row of config bits */
    int  local_config_bits3;	/* Local third row of config bits */
    int	 override_bits1;	/* Configuration bits pair #1 */
    int  override_bits2;	/* Configuration bits pair #2 */
    int  override_bits3;	/* Configuration bits pair #3 */
} CONFIG;

typedef struct ecruft {
    char *error;		/* Error text */
    int dummy;			/* Some dummy cruft to make it all work */
} ECRUFT;

extern CONFIG prog_config;
extern ECRUFT errors[];
extern char *remote_addr, *localhost, *ident_user, *ip_address;
extern int trusted_host_num, rejected_host_num, forward_host_num,
	   fakeuser_num, num_finger_sites, num_headers, local_port,
	   remote_port, can_log;
extern FILE *top_display, *bottom_display, *noname_display, *nouser_display,
	    *identd_display;
extern BOOL local_finger, emulated;
extern FILE *logfile;

#include "defines.h"

#endif _CFINGERD_H_
