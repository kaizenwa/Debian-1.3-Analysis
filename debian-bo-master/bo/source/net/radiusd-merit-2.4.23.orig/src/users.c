 /*
  * RADIUS -- Remote Authentication Dial In User Service
  * 
  * 
  * Livingston Enterprises, Inc. 6920 Koll Center Parkway Pleasanton, CA   94566
  * 
  * Copyright 1992 Livingston Enterprises, Inc.
  * 
  * Permission to use, copy, modify, and distribute this software for any
  * purpose and without fee is hereby granted, provided that this copyright
  * and permission notice appear on all copies and supporting documentation,
  * the name of Livingston Enterprises, Inc. not be used in advertising or
  * publicity pertaining to distribution of the program without specific
  * prior permission, and notice be given in supporting documentation that
  * copying and distribution is by permission of Livingston Enterprises, Inc.
  * 
  * Livingston Enterprises, Inc. makes no representations about the suitability
  * of this software for any purpose.  It is provided "as is" without express
  * or implied warranty.
  *
  * [C] The Regents of the University of Michigan and Merit Network, Inc. 1992,
  * 1993, 1994, 1995, 1996 All Rights Reserved
  *
  * Permission to use, copy, modify, and distribute this software and its
  * documentation for any purpose and without fee is hereby granted, provided
  * that the above copyright notice and this permission notice appear in all
  * copies of the software and derivative works or modified versions thereof,
  * and that both the copyright notice and this permission and disclaimer
  * notice appear in supporting documentation.
  *
  * THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER
  * EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION WARRANTIES OF
  * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  THE REGENTS OF THE
  * UNIVERSITY OF MICHIGAN AND MERIT NETWORK, INC. DO NOT WARRANT THAT THE
  * FUNCTIONS CONTAINED IN THE SOFTWARE WILL MEET LICENSEE'S REQUIREMENTS OR
  * THAT OPERATION WILL BE UNINTERRUPTED OR ERROR FREE.  The Regents of the
  * University of Michigan and Merit Network, Inc. shall not be liable for
  * any special, indirect, incidental or consequential damages with respect
  * to any claim by Licensee or any third party arising from use of the
  * software.
  *
  * Public entry points in this file:
  * 
  * add_file_list
  * config_init
  * config_files
  * config_fini
  * dns_recv
  * find_auth_type
  * find_auth_ent
  * find_client
  * find_client_by_name
  * find_host_by_name
  * free_user_ent
  * get_our_addr
  * ip_hostname
  * list_cat
  * list_copy
  * pair_parse
  * return_file_list
  * user_find
  * user_gettime
  * user_update
  * 
  */

static char     sccsid[] =
		"@(#)users.c 1.3 Copyright 1992 Livingston Enterprises Inc";

static char     rcsid[] = "$Id: users.c,v 1.89 1996/06/11 21:10:39 web Exp $";

#include	<sys/types.h>
#include	<sys/param.h>
#include	<sys/socket.h>
#include	<sys/time.h>
#include	<netinet/in.h>
#include	<arpa/inet.h>

#include	<stdio.h>
#include	<stdlib.h>
#include	<netdb.h>
#include	<time.h>
#include	<ctype.h>
#include	<dirent.h>
#include	<syslog.h>

#include	"radius.h"

static void          fieldcpy PROTO((char *, char **));
static FILE_LIST    *find_file_ent PROTO((char *));
static void          free_clients PROTO((CLIENT_ENTRY *));
static void          free_file_lists PROTO((void));
static int           host_is_us PROTO((char *));
static int           insert_client PROTO((char *, char *, char *));
static int           read_auth PROTO((FILE_LIST *, int));
static int           read_users PROTO((FILE_LIST *, int));

#if defined(ultrix) || defined(__hpux) || defined(__bsdi__) || defined(linux) || defined(SCO)
extern int           h_errno;
#endif	/* ultrix */

extern char          send_buffer[4096];
extern char          recv_buffer[4096];
extern char          ourhostname[MAXHOSTNAMELEN];
extern AATV         *authtype_tv[]; /* AATVs by authentication types */
extern int           debug_flag;
extern int           dumpcore;
extern char         *radius_dir;
extern FILE         *ddt;
extern int           authfile_cnt;
extern int           clients_cnt;
extern int           users_cnt;
extern int           file_logging; /*  0 => syslog, 1 => logfile, 2 => stderr */
extern AATV         *rad_ipc_aatv;
extern FILE         *msgfd;

int                  spawn_flag = 1;
int                  dnspid = 0;   /* PID of current DNS resolver process */
char                 default_radius_server[128] = DEFAULT_RADIUS_SERVER;
char                 default_tacacs_server[128] = DEFAULT_TACACS_SERVER;
int                  rad_ipc_port = 0;

static FILE_LIST    *file_list = (FILE_LIST *) NULL;
static CLIENT_ENTRY *client_list = (CLIENT_ENTRY *) NULL;

static UINT4         self_ip[11];	/* Used with multi-homed servers */
static int           is_engine = 0;	/* rlmadmin will not change this */
static CLIENT_ENTRY *old_clients;

/*************************************************************************
 *
 *	Function: add_file_list
 *
 *	Purpose: Find an existing FILE_LIST entry on file_list with the
 *		 specified prefix or add and init a new one if the
 *		 entry doesn't already exist.
 *
 *************************************************************************/

int
add_file_list (prefix)

char           *prefix;

{
	FILE_LIST      *file_ent;
	FILE_LIST     **fl_prev;
	static char    *func = "add_file_list";

	dprintf(4, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

	for (fl_prev = &file_list, file_ent = file_list;
		file_ent;
		fl_prev = &file_ent->next, file_ent = *fl_prev)
	{
		if (strcmp (file_ent->prefix, prefix) == 0)
		{
			return 0;
		}
	}

	if ((file_ent = (FILE_LIST *) malloc (sizeof (FILE_LIST)))
							== (FILE_LIST *) NULL)
	{
		logit (LOG_DAEMON, LOG_ALERT,
			"%s: Couldn't allocate FILE_ENTRY storage", func);
		return (-1);
	}
	file_ent->prefix = add_string (prefix, ASIS);
	file_ent->user_list = (USER_ENTRY *) NULL;
	file_ent->auth_list = (AUTH_ENTRY *) NULL;
	file_ent->next = (FILE_LIST *) NULL;
	*fl_prev = file_ent;

	return 0;
} /* end of add_file_list () */

/*************************************************************************
 *
 *	Function: config_init
 *
 *	Purpose: Setup environment for config_files() to run in.
 *
 *************************************************************************/

void
config_init ()

{
	is_engine = 1; /* flag set when engine calls us */

	/*
	 *	Set dnspid to defer the call to update_clients()
	 *	until the end of config_files().
	 */
	if (dnspid == 0)
	{
		dnspid = -1;
	}

	/*
	 *	Save the old clients list so we can pick up
	 *	the DNS addresses for the new list.
	 */
	old_clients = client_list;
	client_list = (CLIENT_ENTRY *) NULL;
	return;
} /* end of config_init () */

/*************************************************************************
 *
 *	Function: config_files
 *
 *	Purpose: Read database files into memory data structures.  Reads
 *		 "RADIUS_CLIENTS" and "RADIUS_AUTH" files unconditionally
 *		 and "RADIUS_USERS" file if users_flag is not zero.  Will
 * 		 read multiple users and authfiles if the "file_pfx" is
 *		 specified in a client entry (allowing use of different
 *		 files for different client NASes).
 *
 *		 If clear_flag is greater than zero, remove existing entries.
 *
 *		 A new CLIENT_ENTRY is added to the client_list for each
 *		 client appearing in the "RADIUS_CLIENTS" file.  A new
 *		 FILE_LIST entry is added for each unique file_pfx found
 *		 in the "RADIUS_CLIENTS" file.  Each FILE_LIST entry points
 *		 to a list of USER_ENTRYs containing the information read
 *		 from the "RADIUS_USERS" file with the corresponding
 *		 file_pfx.  Also each FILE_LIST entry contains a pointer to
 *		 the list of AUTH_ENTRYs containing realm information read
 *		 from the "RADIUS_AUTH" file with the corresponding file_pfx.
 *		 If either the "RADIUS_USERS" file or the "RADIUS_AUTH" file
 *		 with the given file_pfx did not exist, the default
 *		 (non-prefixed) file name entries are used instead.
 *
 *************************************************************************/

int
config_files (users_flag, clear_flag, dolog)

int             users_flag;
int             clear_flag;
int             dolog;

{
	int             i;
	int             ent_cnt = 0;
	int             line_nbr = 0;
	int             result;
	FILE_LIST      *file_ent;
	FILE           *clientfd;
	struct hostent *hp;
	char           *file_pfx;
	char           *host;
	char           *hostnm;
	char           *secret;
	char          **paddr;
	char            buffer[128];
	char            fname[MAXPATHLEN];
	static char    *func = "config_files";

	dprintf(2, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

	if (clear_flag > 0) /* Free current list, if already set up */
	{
		free_file_lists ();
	}

	/*
	 * Add default file_list entry - the entry for the "users" and
	 * "authfile" with no optional prefix
	 */
	add_file_list ("");

	/*
	 * Determine the IP address(es) of this machine
	 */
	if ((hp = gethostbyname (ourhostname)) == (struct hostent *) NULL)
	{
		logit (LOG_DAEMON, LOG_CRIT,
			"%s: Couldn't get our own IP address(es)", func);
		return (-1);
	}

	/*
	 *	First clear, then save our IP address(es)
	 *	leaving a zero entry at the end.
	 */
	memset ((char *) self_ip, '\0', sizeof (self_ip));

	if (hp->h_addr_list != (char **) NULL)
	{
		for (i = 0, paddr = hp->h_addr_list;
			(*paddr != (char *) NULL) &&
				(i < sizeof (self_ip) / 4 - 1);
			i++, paddr++)
		{
			memcpy ((char *) &self_ip[i], (char *) *paddr,
				hp->h_length);
			self_ip[i] = ntohl(self_ip[i]);
		}
	}

	/*
	 * Now read in all client file entries, adding a new "CLIENT_ENTRY"
	 * to client_list for each valid file entry found.  If there is more
	 * than one IP address for a given client DNS name, add an entry for
	 * each address.
	 */
	sprintf (fname, "%s/%s", radius_dir, RADIUS_CLIENTS);
	if ((clientfd = fopen (fname, "r")) == (FILE *) NULL)
	{
		logit (LOG_DAEMON, LOG_ERR, "%s: Couldn't open %s for reading",
			func, fname);
		return (-1);
	}

	clients_cnt = 0;
	result = -1;    /* Init result code */

	while (fgets (buffer, sizeof (buffer), clientfd) != (char *) NULL)
	{
		line_nbr++;

		if (*buffer == COMMENT)
		{
			continue;
		}

		/* first hostname */
		if ((hostnm = strtok (buffer, " \t\n\r")) == (char *) NULL)
		{
			continue;
		}

		/* and secret field */
		if ((secret = strtok (NULL, " \t\n\r")) == (char *) NULL)
		{
			logit (LOG_DAEMON, LOG_ERR,
				"%s: missing shared secret: %s line %d",
				func, fname, line_nbr);
			continue;
		}

		file_pfx = strtok (NULL, " \t\n\r"); /* optional prefix */

		/*
		 * Look for "paired" <name1>/<name2> entries.  This type of
		 * entry allows core RADIUS servers to share a common "clients"
		 * file.  If we're name1, name2 is the target name; if we are
		 * name2, then name1 is the target.  If we're neither name,
		 * then this entry isn't of interest to us and we ignore it.
		 */
		host = hostnm;  /* Assume just one name */

		if (strchr (hostnm, '/'))	/* If <name1>/<name2> form */
		{
			strtok (hostnm, "/");
			host = strtok (NULL, " ");
			if (host_is_us (hostnm))
			{
				/* We are 1st name - client is 2nd */
			}
			else
			{
				if (host_is_us (host))
				{
					/* We are 2nd - client is 1st */
					host = hostnm;
				}
				else /* We are neither - no match */
				{
					continue;
				}
			}
		}

		if ((result = insert_client (host, secret, file_pfx)) < 0)
		{
			break;
		}
		ent_cnt++;
	} /* end of while () */
	fclose (clientfd);
	if (dolog)
	{
		logit (LOG_DAEMON, LOG_INFO,
			"%s: %s (%u entries) read to memory",
			func, fname, ent_cnt);
	}
	clients_cnt = ent_cnt;

	/*
	 * Finally, go through all the file_list entries just added, reading
	 * in the "users" and "authfile" for each prefix found.
	 */
	for (file_ent = file_list; file_ent; file_ent = file_ent->next)
	{

#if !(defined(USE_DBM) || defined(USE_NDBM))
		if (users_flag)
			if (read_users (file_ent, dolog) != 0)
			{
				return (-1);
			}
#endif	/* USE_DBM || USE_NDBM */

		if (read_auth (file_ent, dolog) != 0)
		{
			return (-1);
		}

	}

	if (result < 0)
	{
		return (-1);
	}

	return 0;
} /* end of config_files () */

/*************************************************************************
 *
 *	Function: config_fini
 *
 *	Purpose: Cleanup environment after config_files() has run.
 *
 *************************************************************************/

void
config_fini ()

{
	free_clients (old_clients);
	old_clients = (CLIENT_ENTRY *) NULL;

	if ((file_logging == 1) && (msgfd != (FILE *) NULL))
	{
		fflush (msgfd);
	}

	if (dnspid == -1)
	{
		dnspid = 0;
	}

	update_clients ();       /* Start up the DNS resolver. */

	return;
} /* end of config_fini () */

/*************************************************************************
 *
 *      Function: dns_recv
 *
 *      Purpose: Process received DNS updates for clients database.
 *
 *************************************************************************/

void
dns_recv (sin, from_ipaddr, rcvlen)

struct sockaddr_in *sin;
UINT4               from_ipaddr;
int                 rcvlen;

{
	int              cnt;
	u_char           aliascnt;
	UINT4            temp;
	UINT4           *ourad;
	char            *ptr;
	IP_ADDRESS      *an_address;
	DNS_NAME        *a_name;
	CLIENT_ENTRY    *client_ent;
	static int       notify_count = 0;
	static char     *func = "dns_recv";

	notify_count ++;
	dprintf(4, (LOG_AUTH, LOG_DEBUG, "%s: entered (%d)",
		func, notify_count));
	 
	ptr = recv_buffer + 1;

	for (ourad = self_ip;
		(*ourad != (UINT4) 0) && (*ourad != from_ipaddr);
		ourad++)
	{
		continue;
	}

	if (*ourad == (UINT4) 0)
	{
		logit (LOG_DAEMON, LOG_INFO, "%s: from %s - Security Breach",
			func, ip_hostname (from_ipaddr));
		return;
	}

	for (client_ent = client_list;
		client_ent != (CLIENT_ENTRY *) NULL; 
		client_ent = client_ent->next)
	{
		if (strcmp (ptr, client_ent->hostname) == 0)
		{
			break;
		}
	}

	if (client_ent == (CLIENT_ENTRY *) NULL)
	{
		return;
	}		

	ptr += strlen (ptr) + 1;
	aliascnt = *ptr++;

	if (*ptr != '\0') /* If alias or IP address present, clear old ones */
	{
		/*
		 * Reset expire_time with some randomness (0 - 60 minutes)
		 * to avoid a burst of calls to gethostbyname().
		 */
		client_ent->expire_time = (time (0) + ADDRESS_AGING +
						(rand () & 3) * 60 * 20) & ~3;
		for (an_address = client_ent->addrs;
			an_address != (IP_ADDRESS *) NULL;
			an_address = client_ent->addrs)
		{
			client_ent->addrs = an_address->next;
			free (an_address);
		}
 
		for (a_name = client_ent->names;
			a_name != (DNS_NAME *) NULL;
			a_name = client_ent->names)
		{
			client_ent->names = a_name->next;
			free (a_name);
		}
	}
	else /* no alias or IP address present */
	{
		memcpy ((char *) &temp, ptr, sizeof (struct in_addr));
		if (temp == TRY_AGAIN) /* TRY_AGAIN is in netdb.h */
		{
			client_ent->expire_time = (time (0) + ADDRESS_AGING) 
									& ~3;
			logit (LOG_DAEMON, LOG_ALERT,
				"%s: DNS timeout on client or host '%s'",
				func, client_ent->hostname);
			return;
		}
		else
		{
			/*
			 *	Name couldn't be resolved -- log it and retry
			 *	shortly.  Cleverly (or foolishly) use the low
			 *	two bits of expire_time to control the logging
			 *	frequency.
			 */
			if ((cnt = client_ent->expire_time & 3) == 0)
			{
				/* Log every fourth time (or once per hour) */
				logit (LOG_DAEMON, LOG_ALERT,
				     "%s: DNS couldn't resolve name '%s' (%ld)",
					func, client_ent->hostname, temp);
			}

			client_ent->expire_time = ((time (0) + ADDRESS_AGING/4)
						 	& ~3) | (++cnt & 3);
			if (client_ent->addrs != (IP_ADDRESS *) NULL)
			{
				return;
			}

			/* Add invalid address to client_ent */
			memset (ptr, 255, sizeof (struct in_addr));
		}
	}

	/* Add alias names to client_ent structure */
	while (aliascnt-- > 0)
	{
		/* Note that DNS_NAME structure reserves one extra character. */
		if ((a_name =
			(DNS_NAME *) malloc (sizeof (DNS_NAME) + strlen (ptr)))
							== (DNS_NAME *) NULL)
		{
			logit (LOG_DAEMON, LOG_ALERT,
				"%s: FATAL Couldn't allocate DNS_NAME storage",
				func);
			abort ();
		}

		/* Note that type zero will always be last one. */
		strcpy (a_name->name, ptr); 
		ptr += strlen (ptr) + 1;
		a_name->type =  (u_char) *ptr++;
		a_name->next = client_ent->names;
		client_ent->names = a_name;
	}

	/*
	 * For each address in the list, add the address to the client_ent.
	 */
	while (*ptr != '\0')
	{
		if ((an_address = (IP_ADDRESS *) malloc (sizeof (IP_ADDRESS)))
							== (IP_ADDRESS *) NULL)
		{
			logit (LOG_DAEMON, LOG_ALERT,
				"%s: FATAL Couldn't allocate IP_ADDRESS storage",
				func);
			abort ();
		}

		an_address->next = client_ent->addrs;
		memcpy ((char *) &temp, ptr, sizeof (struct in_addr));
		an_address->ipaddr.s_addr = ntohl(temp);
		client_ent->addrs = an_address;
		ptr += sizeof (struct in_addr);
	}

	if (notify_count % DNS_SLEEP == DNS_SLEEP - 1)
	{
		logit (LOG_DAEMON, LOG_INFO, "%s: Notified of (%d) DNS changes",
			func, notify_count);
	}

	return;
} /* end of dns_recv () */

/*************************************************************************
 *
 *	Function: fieldcpy
 *
 *	Purpose: Copy a data field from the buffer.  Advance the buffer
 *		 past the data field.
 *
 *************************************************************************/

static void
fieldcpy (string, uptr)

char           *string;
char          **uptr;

{
	char           *ptr;

	ptr = *uptr;
	if (*ptr == '"')
	{
		ptr++;
		while (*ptr != '"' && *ptr != '\0' && *ptr != '\n')
		{
			*string++ = *ptr++;
		}
		*string = '\0';
		if (*ptr == '"')
		{
			ptr++;
		}
		*uptr = ptr;
		return;
	}

	while (*ptr != ' ' && *ptr != '\t' && *ptr != '\0' && *ptr != '\n' &&
			*ptr != '=' && *ptr != ',')
	{
		*string++ = *ptr++;
	}
	*string = '\0';
	*uptr = ptr;
	return;
} /* end of fieldcpy () */

/*************************************************************************
 *
 *	Function: find_auth_ent
 *
 *	Purpose: Gives access to the private AUTH_ENT for the given realm.
 *
 *	Returns: pointer to the AUTH_ENT for the given realm,
 *		 or, NULL, if error.
 *
 *************************************************************************/

AUTH_ENTRY *
find_auth_ent (u_realm, prot, pfx)

char           *u_realm;
int             prot;
char           *pfx;

{
	int             head;
	int             pat_len;
	FILE_LIST      *file_ent;
	AUTH_ENTRY     *auth_ent;
	AUTH_ENTRY     *entry;
	char           *p;
	char           *realm_name;
	static char     temp[AUTH_ID_LEN + 1];
	static char    *func = "find_auth_ent";

	dprintf(2, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

	if (u_realm[0] == 0)	/* A null realm would match every line. */
	{
		dprintf(2, (LOG_AUTH, LOG_DEBUG, "%s: NULL realm", func));
		return (AUTH_ENTRY *) NULL;
	}

	if ((file_ent = find_file_ent (pfx)) == (FILE_LIST *) NULL)
	{
		dprintf(2, (LOG_AUTH, LOG_DEBUG, "%s: NULL file_ent", func));
		return (AUTH_ENTRY *) NULL;
	}

	if ((auth_ent = file_ent->auth_list) == (AUTH_ENTRY *) NULL)
	{
		/* If no auth_list for this prefix */
		file_ent = file_list;
		/* Default file_ent is first in file_list */
		if ((auth_ent = file_ent->auth_list) == (AUTH_ENTRY *) NULL)
		{
			logit (LOG_DAEMON, LOG_ERR,
				"%s: no default authfile data structure",
				func);
			return (AUTH_ENTRY *) NULL;
		}
	}

	/*
	 *	Match realm name (either exact match or substring match
	 *	based on *.realm syntax) with user supplied string.
	 */
	for ( ; auth_ent ; auth_ent = auth_ent->next )
	{
		realm_name = (char *) NULL;
		if (auth_ent->parent == (AUTH_ENTRY *) NULL) /* parent realm */
		{
			entry = auth_ent;
			/* Look for name match. */
			if (entry->name[0] == '*') /* this is wildcard realm */
			{
				p = &entry->name[1];
				pat_len = strlen (p);
				head = strlen (u_realm) - pat_len;
				if (strncmp ((char *) &u_realm[head],
					(char *) &entry->name[1], pat_len) == 0)
				{
					realm_name = u_realm;
				}
				else
				{
					realm_name = (char *) NULL;
				}
			}
			else /* not a wildcard realm */
			{
				if (strcasecmp (entry->name, u_realm) == 0)
				{
					realm_name = entry->name;
				}
			}
		}
		else /* this entry is an alias name for some real realm */
		{
			entry = auth_ent->parent;
			/* Look for name match. */
			if (entry->name[0] == '*') /* alias in wildcard realm */
			{
				p = &entry->name[1];
				pat_len = strlen (p);
				head = strlen (u_realm) - pat_len;
				if (strncmp ((char *) &u_realm[head],
					(char *) &entry->name[1], pat_len) == 0)
				{
					/* combine real prefix, parent suffix */
					strcpy (temp, u_realm);
					if (strtok (temp, ".") != (char *) NULL)
					{
						realm_name = strcat (temp,
							       &entry->name[1]);
					}
				}
				else
				{
					realm_name = (char *) NULL;
				}
			}
			else /* regular alias */
			{
				if (strcasecmp (auth_ent->name, u_realm) == 0)
				{
					realm_name = entry->name;
				}
			}
		}

		if (realm_name != (char *) NULL) /* then we have a name match */
		{
			if (!entry->prot || (entry->prot == prot))
			{
				break;
			}
		}
	}

	if (auth_ent == (AUTH_ENTRY *) NULL)
	{
		dprintf(2, (LOG_AUTH, LOG_DEBUG, "%s: realm not found", func));
		return (AUTH_ENTRY *) NULL;
	}

	return entry;
} /* end of find_auth_ent () */

/*************************************************************************
 *
 *	Function: find_auth_type
 *
 *	Purpose: Find the proper AUTH_ENTRY to use for the given authentication
 *		 realm name from the FILE_LIST entry with the given file_pfx.
 *
 *	Returns: The authentication type, name of the authentication agent to
 *		 use, the primary realm name and any optional packet filter
 *		 to be applied are returned.
 *
 *	Returns:  0 = normal return,
 *		 -1 = error return
 *
 *************************************************************************/

int
find_auth_type (u_realm, prot, pfx, type, agent, realm, filter)

char           *u_realm;
int             prot;
char           *pfx;
int            *type;	/* receives resultant authentication type value */
char          **agent;	/* receives resultant authentication agent name */
char          **realm;	/* receives resultant primary realm name */
char          **filter;	/* receives resultant authentication filter name */

{
	int             head;
	int             pat_len;
	FILE_LIST      *file_ent;
	AUTH_ENTRY     *auth_ent;
	AUTH_ENTRY     *entry;
	char           *p;
	char           *realm_name;
	static char     temp[AUTH_ID_LEN + 1];
	static char    *func = "find_auth_type";

	dprintf(2, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

	if (u_realm[0] == 0)	/* A null realm would match every line. */
	{
		dprintf(2, (LOG_AUTH, LOG_DEBUG, "%s: NULL realm", func));
		return (-1);
	}

	if ((file_ent = find_file_ent (pfx)) == (FILE_LIST *) NULL)
	{
		dprintf(2, (LOG_AUTH, LOG_DEBUG, "%s: NULL file_ent", func));
		return (-1);
	}

	if ((auth_ent = file_ent->auth_list) == (AUTH_ENTRY *) NULL)
	{
		/* If no auth_list for this prefix */
		file_ent = file_list;
		/* Default file_ent is first in file_list */
		if ((auth_ent = file_ent->auth_list) == (AUTH_ENTRY *) NULL)
		{
			logit (LOG_DAEMON, LOG_ERR,
				"%s: no default authfile data structure",
				func);
			return (-1);
		}
	}

	/*
	 *	Match realm name (either exact match or substring match
	 *	based on *.realm syntax) with user supplied string.
	 */
	for ( ; auth_ent ; auth_ent = auth_ent->next )
	{
		realm_name = (char *) NULL;
		if (auth_ent->parent == (AUTH_ENTRY *) NULL) /* parent realm */
		{
			entry = auth_ent;
			/* Look for name match. */
			if (entry->name[0] == '*') /* this is wildcard realm */
			{
				p = &entry->name[1];
				pat_len = strlen (p);
				head = strlen (u_realm) - pat_len;
				if (strncmp ((char *) &u_realm[head],
					(char *) &entry->name[1], pat_len) == 0)
				{
					realm_name = u_realm;
				}
				else
				{
					realm_name = (char *) NULL;
				}
			}
			else /* not a wildcard realm */
			{
				if (strcasecmp (entry->name, u_realm) == 0)
				{
					realm_name = entry->name;
				}
			}
		}
		else /* this entry is an alias name for some real realm */
		{
			entry = auth_ent->parent;
			/* Look for name match. */
			if (entry->name[0] == '*') /* alias in wildcard realm */
			{
				p = &entry->name[1];
				pat_len = strlen (p);
				head = strlen (u_realm) - pat_len;
				if (strncmp ((char *) &u_realm[head],
					(char *) &entry->name[1], pat_len) == 0)
				{
					/* combine real prefix, parent suffix */
					strcpy (temp, u_realm);
					if (strtok (temp, ".") != (char *) NULL)
					{
						realm_name = strcat (temp,
							       &entry->name[1]);
					}
				}
				else
				{
					realm_name = (char *) NULL;
				}
			}
			else /* regular alias */
			{
				if (strcasecmp (auth_ent->name, u_realm) == 0)
				{
					realm_name = entry->name;
				}
			}
		}

		if (realm_name != (char *) NULL) /* then we have a name match */
		{
			if (!entry->prot || (entry->prot == prot))
			{
				break;
			}
		}
	}

	if (auth_ent == (AUTH_ENTRY *) NULL)
	{
		dprintf(2, (LOG_AUTH, LOG_DEBUG, "%s: realm not found", func));
		return (-1);
	}

	*type = entry->type;
	*agent = entry->host;
	*realm = realm_name;
	*filter = entry->filter;
	dprintf(2, (LOG_AUTH, LOG_DEBUG,
		"%s: type %d, agent '%s', realm '%s' and filter '%s'",
		func, entry->type, entry->host, realm_name, entry->filter));
	return 0;
} /* end of find_auth_type () */

/*************************************************************************
 *
 *	Function: find_client
 *
 *	Purpose: Find the CLIENT_ENTRY in client_list for the client with
 *		 the given IP address.  If the entry is found, the secret
 *		 shared with this client, any configured file_pfx to be
 *		 used for this client and the client's name are returned.
 *
 *	Returns: 0 = found client entry,
 *		-1 = client not found.
 *
 *************************************************************************/

int
find_client (ipaddr, hostname, secret, pfx)

UINT4           ipaddr;
char          **hostname;	/* Receives resultant hostname, if found */
char          **secret;		/* Receives resultant secret, if found */
char          **pfx;		/* Receives resultant prefix, if found */

{
	int             ud = 0;
	CLIENT_ENTRY   *client_ent;
	IP_ADDRESS     *an_address;
	time_t          cur_time;
	static char    *func = "find_client";

	dprintf(4, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

	cur_time = time (0);
	for (client_ent = client_list;
		client_ent;
		client_ent = client_ent->next)
	{
		if (cur_time > client_ent->expire_time)
		{
			ud = 1;
		}

		for (an_address = client_ent->addrs;
			an_address != (IP_ADDRESS *) NULL;
			an_address = an_address->next)
		{
			if (an_address->ipaddr.s_addr == ipaddr)
			{
				break;
			}
		}

		if (an_address)
		{
			break;
		}
	}

	if (ud > 0)
	{
		update_clients ();
	}

	/* Don't match host-only entries (those with a null secret) */
	if (client_ent == (CLIENT_ENTRY *) NULL || *client_ent->secret == '\0')
	{
		return (-1);
	}

	*hostname = client_ent->hostname;
	*secret = client_ent->secret;
	*pfx = client_ent->prefix;

	return (0);
} /* end of find_client () */

/*************************************************************************
 *
 *	Function: find_client_by_name
 *
 *	Purpose: Find the CLIENT_ENTRY in client_list for the client with
 *		 the given hostname.  If the entry is found, the secret
 *		 shared with this client, any configured file_pfx to be
 *		 used for this client and the client's name are returned.
 *
 *	Returns: 0 = found client entry and resolved IP address,
 *		 1 = found client entry but no IP address,	
 *		 2 = found host entry but IP address not obtained 
 *		     (unresolvable DNS name),	
 *		-1 = client not found.
 *
 *************************************************************************/

int
find_client_by_name (ipaddr, hostname, secret, pfx)

UINT4          *ipaddr;		/* Receives resultant address, if found */
char           *hostname;	/* Match this name */
char          **secret;		/* Receives resultant secret, if found */
char          **pfx;		/* Receives resultant prefix, if found */

{
	int             ud = 0;
	CLIENT_ENTRY   *client_ent;
	DNS_NAME       *name_ent;
	char           *name;
	time_t          cur_time;
	static char    *func = "find_client_by_name";

	dprintf(4, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

	if (good_ipaddr (hostname) == 0)
	{
		/* name = address same as find_client() call */
		*ipaddr = ntohl(inet_addr (hostname));
		return find_client (*ipaddr, &name, secret, pfx);
	}

	if (strcmp (hostname, RADIUS_LOCALSERVER) == 0)
	{
		hostname = ourhostname;
	}

	cur_time = time (0);
	for (client_ent = client_list;
		client_ent != (CLIENT_ENTRY *) NULL;
		client_ent = client_ent->next)
	{
		if (cur_time > client_ent->expire_time)
		{
			ud = 1;
		}

		if (strcmp (client_ent->hostname, hostname) == 0)
		{
			break;
		}

		for (name_ent = client_ent->names;
			name_ent != (DNS_NAME *) NULL;
			name_ent = name_ent->next)
		{
			if (strcmp (name_ent->name, hostname) == 0)
			{
				break;
			}
		}

		if (name_ent != (DNS_NAME *) NULL)
		{
			break;
		}
	}

	if (ud > 0)
	{
		update_clients ();
	}

	/* Don't match host-only entries (those with a null secret) */
	if (client_ent == (CLIENT_ENTRY *) NULL || *client_ent->secret == '\0')
	{
		return (-1);
	}

	*secret = client_ent->secret;
	*pfx = client_ent->prefix;

	if (client_ent->addrs == (IP_ADDRESS *) NULL)
	{
		*ipaddr = 0;
		return (1);
	}

	if ((*ipaddr = client_ent->addrs->ipaddr.s_addr) == -1)
	{
		return (2);
	}

	return (0);
} /* end of find_client_by_name () */

/*************************************************************************
 *
 *	Function: find_host_by_name
 *
 *	Purpose: Resolve the host address by looking in the client list.
 *		 Non-clients (those with a null secret) in this list
 *		 are matched as well as normal clients.
 *
 *	Returns: 0 = found host entry and resolved IP address,
 *		 1 = found host entry but unresolved IP address,	
 *		 2 = found host entry but IP address not obtained 
 *		     (unresolvable DNS name - uses address 255.255.255.255),	
 *		-1 = host not found.
 *
 *************************************************************************/

int
find_host_by_name (ipaddr, hostname)

UINT4          *ipaddr;		/* receives resultant address if found */
char           *hostname;	/* Match this name */

{
	int             ud = 0;
	char           *p;
	char           *q;
	CLIENT_ENTRY   *client_ent;
	DNS_NAME       *name_ent;
	time_t          cur_time;
	static char    *func = "find_host_by_name";

	dprintf(4, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

	if (good_ipaddr (hostname) == 0)
	{
		*ipaddr = ntohl(inet_addr (hostname));
		return 0;
	}

	if (strcmp (hostname, RADIUS_LOCALSERVER) == 0)
	{
		*ipaddr = self_ip[0];
		return 0;
	}

	/* See if it's us.  Match full name or up to "." of our name */
	for (p = hostname, q = ourhostname; *p == *q; p++, q++)
	{
		if (*p == '\0')
		{
			break;
		}
	}

	if (*p == '\0' && (*q == '\0' || *q == '.'))
	{
		*ipaddr = self_ip[0];
		return 0;
	}

	cur_time = time (0);
	for (client_ent = client_list;
		client_ent != (CLIENT_ENTRY *) NULL;
		client_ent = client_ent->next)
	{
		if (cur_time > client_ent->expire_time)
		{
			ud = 1;
		}

		if (strcmp (client_ent->hostname, hostname) == 0)
		{
			break;
		}

		for (name_ent = client_ent->names;
			name_ent != (DNS_NAME *) NULL;
			name_ent = name_ent->next)
		{
			if (strcmp (name_ent->name, hostname) == 0)
			{
				break;
			}
		}

		if (name_ent != (DNS_NAME *) NULL)
		{
			break;
		}
	}

	if (ud > 0)
	{
		update_clients ();
	}

	if (client_ent == (CLIENT_ENTRY *) NULL)
	{
		*ipaddr = 0;
		return (-1);
	}

	if (client_ent->addrs == (struct ip_address *) NULL)
	{
		*ipaddr = 0;
		return (1);
	}

	if ((*ipaddr = client_ent->addrs->ipaddr.s_addr) == -1)
	{
		return (2);
	}
	return (0);
} /* end of find_host_by_name () */

/*************************************************************************
 *
 *	Function: find_file_ent
 *
 *	Purpose: Find a FILE_LIST entry on file_list with the specified
 *		 file_pfx.  The entry should be found as find_file_ent is
 *		 only called for file_pfx's that were found in the "clients"
 *		 file at initialization time.
 *
 *************************************************************************/

static FILE_LIST *
find_file_ent (file_pfx)

char           *file_pfx;

{
	FILE_LIST      *file_ent;
	static char    *func = "find_file_ent";

	dprintf(4, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

	if ((file_ent = file_list) == (FILE_LIST *) NULL)
	{
		logit (LOG_DAEMON, LOG_ERR,
			"%s: No users/authfile data structure", func);
		return (FILE_LIST *) NULL;
	}
	if (file_pfx && file_pfx[0])
	{
		while (strcmp (file_ent->prefix, file_pfx) != 0)
		{
			if ((file_ent = file_ent->next) == (FILE_LIST *) NULL)
			{
				logit (LOG_DAEMON, LOG_ERR,
					"%s: Couldn't match %s in FILE_LIST",
					func, file_pfx);
				return (FILE_LIST *) NULL;
			}
		}
	}
	return file_ent;
} /* end of find_file_ent () */

/*************************************************************************
 *
 *	Function: free_clients
 *
 *	Purpose: Toss client list entries and associated address structure.
 *
 *	Remark: Zap storage blocks to avoid leaving any secrets around.
 *
 *************************************************************************/

static void
free_clients (client_list)

CLIENT_ENTRY   *client_list;

{
	CLIENT_ENTRY      *client_ent;
	IP_ADDRESS        *an_address;	
	DNS_NAME          *a_name;
	static char       *func = "free_clients";

	dprintf(4, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

	for (client_ent = client_list;
		client_ent != (CLIENT_ENTRY *) NULL;
		client_ent = client_list)
	{
		client_list = client_ent->next;

		for (an_address = client_ent->addrs;
			an_address != (IP_ADDRESS *) NULL;
			an_address = client_ent->addrs)
		{
			client_ent->addrs = an_address->next;
			free (an_address);
		}

		for (a_name = client_ent->names;
			a_name != (DNS_NAME *) NULL;
			a_name = client_ent->names)
		{
			client_ent->names = a_name->next;
			free (a_name);
		}

		free (client_ent);
	}
	return;
} /* end of free_clients () */

/*************************************************************************
 *
 *	Function: free_file_lists
 *
 *	Purpose: Free all the storage for the "users" and "authfile"
 *		 memory resident data structures allocated by calling
 *		 config_files().
 *
 *************************************************************************/

static void
free_file_lists ()

{
	FILE_LIST      *file_ent;
	USER_ENTRY     *user_ent;
	AUTH_ENTRY     *auth_ent;
	static char    *func = "free_file_lists";

	dprintf(4, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

	authfile_cnt = 0;
	users_cnt = 0;

	for (file_ent = file_list; file_ent; file_ent = file_list)
	{
		for (user_ent = file_ent->user_list;
			user_ent;
			user_ent = file_ent->user_list)
		{
			file_ent->user_list = user_ent->next;
			free_user_ent (user_ent);
		}
		for (auth_ent = file_ent->auth_list;
			auth_ent;
			auth_ent = file_ent->auth_list)
		{
			file_ent->auth_list = auth_ent->next;
			free (auth_ent);
		}
		file_list = file_ent->next;
		free (file_ent);
	}

	return;
} /* end of free_file_lists () */

/*************************************************************************
 *
 *	Function: free_user_ent
 *
 *	Purpose: Free all components of a USER_ENTRY structure.  Zap
 *		 the USER_ENTRY storage.
 *
 *************************************************************************/

void
free_user_ent (user_ent)

USER_ENTRY     *user_ent;

{
	list_free (user_ent->check);
	list_free (user_ent->reply);
	memset ((char *) user_ent, '\0', sizeof (USER_ENTRY));
	free (user_ent);
	return;
} /* end of free_user_ent () */

/*************************************************************************
 *
 *	Function: get_our_addr
 *
 *	Purpose: A global function to return a local variable (?)
 *
 *	Returns: (an) IP address of this machine.
 *
 *************************************************************************/

UINT4
get_our_addr ()

{
	return self_ip[0];
} /* end of get_our_addr () */

/*************************************************************************
 *
 *	Function: host_is_us
 *
 *	Purpose: Determine if we are the given host.
 *
 *	Returns: 1 if the given hostname is the name of this host,
 *		 0 otherwise.
 *
 *************************************************************************/

static int
host_is_us (hostname)

char	       *hostname;

{
	UINT4	        addr;
	UINT4	       *adptr;

	if (find_host_by_name (&addr, hostname) == 0)
	{
		for (adptr = self_ip; *adptr > 0; adptr++)
		{
			if (*adptr == addr)
			{
				return 1;
			}
		}
	}
	return 0;
} /* end of host_is_us () */

/*************************************************************************
 *
 *	Function: insert_client
 *
 *	Purpose: Inserts a CLIENT_ENTRY node into client_list for the
 *		 given hostname.
 *
 *	Returns: 0 - inserted ok
 *		-1 - bad news
 *
 *************************************************************************/

static int
insert_client (hostname, secret, prefix)

char           *hostname;
char           *secret;
char           *prefix;

{
	CLIENT_ENTRY   *client_ent = (CLIENT_ENTRY *) NULL;
	CLIENT_ENTRY   *oldent;
	CLIENT_ENTRY  **prev;
	IP_ADDRESS     *ip_address;
	struct in_addr  addr;
	static char    *func = "insert_client";

	dprintf(4, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

	/* Convert generic name for us to our real name */
	if (strcmp (hostname, RADIUS_LOCALSERVER) == 0)
	{
		hostname = ourhostname;
	}

	/* Look for entry from previous list (before HUP) */

	if (old_clients != (CLIENT_ENTRY *) NULL)
	{
		for (prev = &old_clients;
			(oldent = *prev) != (CLIENT_ENTRY *) NULL;
			prev = &oldent->next)
		{
			if (strcmp (hostname, oldent->hostname) == 0)
			{
				/* Matched - Remove from old list */
				*prev = oldent->next;
				client_ent = oldent;
				break;
			}
		}
	}

	if (client_ent == (CLIENT_ENTRY *) NULL)
	{
		if ((client_ent =
			(CLIENT_ENTRY *) malloc ( sizeof (CLIENT_ENTRY)))
						== (CLIENT_ENTRY *) NULL)
		{
			logit (LOG_DAEMON, LOG_ALERT,
			     "%s: FATAL Couldn't allocate CLIENT_ENTRY storage",
				 func);
			abort ();
		}
		client_ent->hostname = add_string (hostname, ASIS);
		client_ent->names = (DNS_NAME *) NULL;
		client_ent->addrs = (IP_ADDRESS *) NULL;
		client_ent->type = CE_DNS;

		/* Set constant addrs now so we don't have to wait for DNS */
		if (good_ipaddr (hostname) == 0)
		{
			client_ent->type = CE_NUMERIC;
			addr.s_addr = ntohl(inet_addr (hostname));
		}
		else
		{
			if (strcmp (hostname, ourhostname) == 0)
			{
				client_ent->type = CE_OURADDR;
				addr.s_addr = self_ip[0];
			}
		}

		if (client_ent->type != CE_DNS)
		{
			if ((ip_address =
				(IP_ADDRESS *) malloc (sizeof (CLIENT_ENTRY)))
							== (IP_ADDRESS *) NULL)
			{
				logit (LOG_DAEMON, LOG_ALERT,
			       "%s: FATAL Couldn't allocate IP_ADDRESS storage",
					func);
				abort ();
			}

			ip_address->ipaddr = addr;
			ip_address->next = (IP_ADDRESS *) NULL;
			client_ent->addrs = ip_address;
		}
	}

	client_ent->secret = add_string (secret, ASIS);
	client_ent->prefix = add_string (prefix, ASIS);
	client_ent->expire_time = (time_t) 0;
	client_ent->next = client_list;
	client_list = client_ent;

	/*
	 *	If the entry had an optional file prefix, add a new FILE_ENTRY
	 *	to the file_list to handle this prefix.  Add_file_list() will
	 *	not add duplicate entries.
	 */
	if (client_ent->prefix[0] != '\0')
	{
		add_file_list (client_ent->prefix);
	}
	return 0;
} /* end of insert_client () */

#define	MAX_HOSTNAME_BUFFERS	20

/*************************************************************************
 *
 *	Function: ip_hostname
 *
 *	Purpose: Return a printable host name (or IP address in dotted quad
 *		 notation) for the supplied IP address.
 *
 *************************************************************************/

char *
ip_hostname (h_ipaddr)

UINT4           h_ipaddr;

{
	UINT4          *ourad;
	CLIENT_ENTRY   *client_ent;
	IP_ADDRESS     *an_address;
	DNS_NAME       *a_name;
	struct hostent *hp;
	struct in_addr  inad;
	static char     buffers[MAX_HOSTNAME_BUFFERS][128];
	static int      ndx = 0;
	char           *hstname = buffers[ndx];

	for (client_ent = client_list;
		client_ent != (CLIENT_ENTRY *) NULL;
		client_ent = client_ent->next)
	{
		for (an_address = client_ent->addrs;
			an_address != (IP_ADDRESS *) NULL;
			an_address = an_address->next)
		{
			if (an_address->ipaddr.s_addr == h_ipaddr)
			{
				break;
			}
		}

		if (an_address != (IP_ADDRESS *) NULL)
		{
			break;
		}
	}

	if (client_ent != (CLIENT_ENTRY *) NULL)
	{
		/*
		 *	We found something in our tables.
		 *	Return a pointer to that instead of
		 *	copying it to our own static area.
		 */
		if ((a_name = client_ent->names) == (DNS_NAME *) NULL ||
			(a_name->type != 0))
		{
			return (client_ent->hostname);
		}
		else /* return official name if it's not in the main entry */
		{
			return (a_name->name);
		}
	}

	/* Didn't find it in our tables.  Keep looking... */
	for (ourad = self_ip;
		(*ourad > (UINT4) 0) && (*ourad != h_ipaddr);
		ourad++)
	{
		continue;
	}

	/* If it was our own address, return our hostname. */
	if (*ourad > (UINT4) 0)
	{
		return (ourhostname);
	}

	/* It wasn't us, so make something up. */
	inad.s_addr = htonl(h_ipaddr);
	strcpy (hstname, inet_ntoa (inad));	/* xxx.yyy.zzz.qqq */

	/*
	 *	Special check for non-server use.
	 *	Note: a server always will have at
	 *	least one client.
	 */
	if (client_list == (CLIENT_ENTRY *) NULL)
	{
		if ((hp = gethostbyaddr ((char *) &inad.s_addr,
					sizeof (struct in_addr),
					AF_INET)) 
				!= (struct hostent *) NULL)
		{
			strcpy (hstname, hp->h_name);
		}
	}

	/* Circulate through the buffers... */
	ndx++;
	if (ndx >= MAX_HOSTNAME_BUFFERS)
	{
		ndx = 0;
	}

	return (hstname);

} /* end of ip_hostname () */

/*************************************************************************
 *
 *	Function: list_cat
 *
 *	Purpose: Given two lists, "a" and "b", place "b" at the end of "a"
 *
 *************************************************************************/

void
list_cat (a, b)

VALUE_PAIR    **a;
VALUE_PAIR     *b;

{
	VALUE_PAIR    **last;
	FILE           *debugout = stdout;
	static char    *func = "list_cat";

	dprintf(4, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

	if (debug_flag >= 4)
	{
		if (ddt)
		{
			debugout = ddt;
		}
		fprintf (debugout, "First list:\n");
	}

	for (last = a; *last != (VALUE_PAIR *) NULL; last = &((*last)->next))
	{
		if (debug_flag >= 4)
		{
			debug_pair (debugout, *last);
		}
	}

	*last = b;

	if (debug_flag >= 4)
	{
		fprintf (debugout, "and Second list:\n");
		debug_list (debugout, b);
	}

	return;
} /* end of list_cat () */

#define LIST_COPY_LIMIT 256   /* Limit the number of items we will copy. */

/*************************************************************************
 *
 *	Function: list_copy
 *
 *	Purpose: Make a copy of the entire list of value_pairs pointed to by
 *		 from_list.  It is necessary to copy the check_items and
 *		 reply_items from a USER_ENTRY before processing a request
 *		 because they may be modified or freed.
 *
 *************************************************************************/

void
list_copy (to_list, from_list)

VALUE_PAIR    **to_list;
VALUE_PAIR     *from_list;

{
	int             count = 0;      /* Count items we copy. */
	VALUE_PAIR     *copy_item;
	VALUE_PAIR     *new_item;
	VALUE_PAIR    **last;
	VALUE_PAIR    **old_end;
	static char    *func = "list_copy";

	dprintf(2, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

	if (to_list == (VALUE_PAIR **) NULL)
	{
		logit (LOG_DAEMON, LOG_ERR, "%s: NULL parameter", func);
		exit (-13);
	}

	copy_item = (VALUE_PAIR *) NULL;

	/* run to end of destination list */
	for (last = to_list ;
		*last != (VALUE_PAIR *) NULL ;
		last = &((*last)->next))
	{
		if (*last == from_list)
		{
			logit (LOG_DAEMON, LOG_ALERT,
			"%s: FATAL (0x%p->0x%p,0x%p) crosslinked at 0x%p->0x%p",
			       func, to_list, *to_list, from_list, last, *last);
			dumpcore = 1;
			abort ();
		}
	}
	old_end = last; /* Save old end-ptr for sanity checking. */

	new_item = (VALUE_PAIR *) NULL;

	dprintf(5, (LOG_AUTH, LOG_DEBUG,
			"%s: copy from list 0x%p to end of list at 0x%p->0x%p",
			func, from_list, to_list, *to_list));

	for (copy_item = from_list ;
		copy_item != (VALUE_PAIR *) NULL ;
		copy_item = copy_item->next)
	{
		if (count > LIST_COPY_LIMIT)
		{
			logit (LOG_DAEMON, LOG_ALERT,
			 "%s: FATAL (0x%p->0x%p, 0x%p), count=%d, limit exceed",
				func, to_list, *to_list, from_list, copy_item,
				count);
			dumpcore = 1;
			abort ();
		}

		if (copy_item == *old_end)
		{
			logit (LOG_DAEMON, LOG_ALERT,
		 "%s: FATAL (0x%X->0x%X, 0x%X) list appended to itself at 0x%X",
				func, to_list, *to_list, from_list, copy_item);
			dumpcore = 1;
			abort ();
		}

		if ((new_item = (VALUE_PAIR *) malloc (sizeof (VALUE_PAIR)))
							== (VALUE_PAIR *) NULL)
		{
			logit (LOG_DAEMON, LOG_ALERT, "%s: FATAL out of memory",
				func);
			abort ();
		}
		memcpy ((char *) new_item, (char *) copy_item,
			sizeof (VALUE_PAIR));
		new_item->next = (VALUE_PAIR *) NULL;

		*last = new_item; /* always put copy at end of to_list */
		last = &(new_item->next); /* new end of to_list */
		count++;
	}
	dprintf(2, (LOG_AUTH, LOG_DEBUG, "%s: copied %d items", func, count));
	return;
} /* end of list_copy () */

#define PARSE_MODE_NAME		0
#define PARSE_MODE_EQUAL	1
#define PARSE_MODE_VALUE	2
#define PARSE_MODE_INVALID	3

/*************************************************************************
 *
 *	Function: pair_parse
 *
 *	Purpose: Parses the buffer to extract the attribute-value pairs.
 *
 *	Returns: 0 = successful parse of attribute-value pair,
 *		-1 = syntax (or other) error detected.
 *
 *************************************************************************/

int
pair_parse (buffer, first_pair)

char           *buffer;
VALUE_PAIR    **first_pair;

{
	int             mode;
	int             rc;
	char            attrstr[AUTH_ID_LEN];
	char            valstr[AUTH_ID_LEN];
	DICT_ATTR      *attr;
	DICT_VALUE     *dval;
	VALUE_PAIR     *pair;
	VALUE_PAIR     *link;
	struct tm      *tm;
	time_t          timeval;
	static char    *func = "pair_parse";

	dprintf(4, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

	mode = PARSE_MODE_NAME;
	while (*buffer != '\n' && *buffer != '\0')
	{
		if (*buffer == ' ' || *buffer == '\t' || *buffer == ',')
		{
			buffer++;
			continue;
		}

		switch (mode)
		{
		    case PARSE_MODE_NAME:		/* Attribute Name */
			fieldcpy (attrstr, &buffer);
			if ((attr =
				dict_attrfind (attrstr)) == (DICT_ATTR *) NULL)
			{
				return (-1);
			}
			mode = PARSE_MODE_EQUAL;
			break;

		    case PARSE_MODE_EQUAL:		/* Equal sign */
			if (*buffer == '=')
			{
				mode = PARSE_MODE_VALUE;
				buffer++;
			}
			else
			{
				return (-1);
			}
			break;

		    case PARSE_MODE_VALUE:		/* Value */
			fieldcpy (valstr, &buffer);

			if ((pair =
				(VALUE_PAIR *) malloc (sizeof (VALUE_PAIR)))
							== (VALUE_PAIR *) NULL)
			{
				logit (LOG_DAEMON, LOG_ALERT,
					"%s: FATAL out of memory", func);
				abort ();
			}
			strcpy (pair->name, attr->name);
			pair->attribute = attr->value;
			pair->type = attr->type;

			switch (pair->type)
			{

			    case PW_TYPE_STRING:
				strcpy (pair->strvalue, valstr);
				break;

			    case PW_TYPE_INTEGER:
				if (isdigit (*valstr))
				{
					pair->lvalue = atoi (valstr);
				}
				else
				{
					if ((dval = dict_valfind (valstr))
							== (DICT_VALUE *) NULL)
					{
						free (pair);
						return (-1);
					}
					else
					{
						pair->lvalue = dval->value;
					}
				}
				break;

			    case PW_TYPE_IPADDR:
				rc = find_host_by_name (&pair->lvalue, valstr);
				if (rc == -1)   /* Name not in our list yet */
				{
					insert_client (valstr, "", "");
				}
				else
				{
					if (rc == 2) /* Unresolvable DNS name */
					{
						free (pair);
						return (-1);
					}
				}

				/* If DNS name given, store it in strvalue */
				if (good_ipaddr (valstr) == 0)
				{
					*pair->strvalue = '\0';
				}
				else /* Now we can re-resolve these names */
				{
					strcpy (pair->strvalue, valstr);
				}
				break;

			    case PW_TYPE_DATE:
				timeval = time (0);
				tm = localtime (&timeval);
				tm->tm_hour = 0;
				tm->tm_min = 0;
				tm->tm_sec = 0;
				user_gettime (valstr, tm);
#ifdef TIMELOCAL
				pair->lvalue = (UINT4) timelocal (tm);
#else	/* TIMELOCAL */
				pair->lvalue = (UINT4) mktime (tm);
#endif	/* TIMELOCAL */
				break;

			    default:
				free (pair);
				return (-1);
			}
			pair->next = (VALUE_PAIR *) NULL;

			if (*first_pair == (VALUE_PAIR *) NULL)
			{
				*first_pair = pair;
			}
			else
			{
				link = *first_pair;
				while (link->next != (VALUE_PAIR *) NULL)
				{
					link = link->next;
				}
				link->next = pair;
			}

			mode = PARSE_MODE_NAME;
			break;

		    default:
			mode = PARSE_MODE_NAME;
			break;
		}
	}
	return (0);
} /* end of pair_parse () */

/*************************************************************************
 *
 *	Function: read_auth
 *
 *	Purpose: Reads in the realm information from the "authfile" with
 *	         file_pfx corresponding to the given file_ent.  The information
 *		 read is copied to a data structure that is linked to the
 *		 given file_ent.
 *
 *************************************************************************/

static int
read_auth (file_ent, dolog)

FILE_LIST      *file_ent;
int             dolog;

{
	FILE             *authfd;
	char             *name;
	char             *prot;
	char             *type;
	char             *host;
	char             *filter;
	char             *alias_list;
	int               line_no = 0;
	int               ent_cnt = 0;
	int               type_inx;
	int               prot_inx;
	int               error = 0;
	AUTH_ENTRY       *auth_ent;
	AUTH_ENTRY      **end_ent;
	AUTH_ALIAS_ENTRY *alias_ent;
	DICT_VALUE       *type_val;
	static char      *auth_prots[] =
	{
		PW_PROTTYPES_DFLT,
		PW_PROTTYPES_CHAP,
		PW_PROTTYPES_PW,
		NULL
	};
	char              buffer[128];
	char              fname[MAXPATHLEN];
	char             *func = "read_auth";

	dprintf(2, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

	sprintf (fname, "%s/%s%s", radius_dir, file_ent->prefix, RADIUS_AUTH);
	if ((authfd = fopen (fname, "r")) == (FILE *) NULL)
	{
		/*
		 * It's okay if a non-prefixed RADIUS_AUTH file doesn't exist,
		 * as long as no CI_AUTHENTICATION_TYPE = REALM entries are
		 * configured in the RADIUS_USERS file.  If <pfx>RADIUS_AUTH
		 * doesn't exist, then the non-prefixed RADIUS_AUTH file will
		 * be used instead.
		 */
		return (0);
	}
	end_ent = &file_ent->auth_list;
	while (fgets (buffer, sizeof (buffer), authfd) != (char *) NULL)
	{
		line_no++;
		if (isspace (*buffer) || (*buffer == COMMENT))
		{
			continue;
		}
		name = strtok (buffer, " ,\t\n\r");

		if (strcmp ("DEFAULT_RADIUS_SERVER", name) == 0)
		{
			if ((host = strtok (NULL, " \"\n\r")) != (char *) NULL)
			{
				strcpy (default_radius_server, host);
			}
			else
			{
				default_radius_server[0] = '\0';
			}		
			continue;
		}
		else
		{
			if (strcmp ("DEFAULT_TACACS_SERVER", name) == 0)
			{
				if ((host = strtok (NULL, " \"\n\r"))
							!= (char *) NULL)
				{
					strcpy (default_tacacs_server, host);
				}
				else
				{
					default_tacacs_server[0] = '\0';
				}		
				continue;
			}
		}

		/* Scan for optional alias list or protocol indicator */

		prot_inx = 0;
		alias_list = NULL;
		type = NULL;

		while (type == NULL && !error)
		{
			prot = strtok (NULL, " ,\t\n\r");
			if (prot == NULL)
			{
				error++;
				continue;
			}


			switch (*prot)
			{
			    case '(': 	/* "(<aliases>)" */
				alias_list = prot;
				if (prot[strlen (prot) - 1] != ')')
				{
					if (strtok (NULL, ")") == NULL)
					{
						error++;
					}
				/* We don't want to break up alias list yet */
					prot[strlen (prot)] = ' ';
				}
				break;

			    case ('-'):	/* "-<protocol>" */
				if (*++prot == '\0')
				{
					error++;
					break;
				}

				for (prot_inx = 0;
				     auth_prots[prot_inx] &&
				       strcmp (prot, auth_prots[prot_inx]) != 0;
				     prot_inx++)
				{
					;
				}

				if (!auth_prots[prot_inx])
				{
					error++;
				}
				break;

			    default:
				type = prot;
				break;
			}
		}

		if (error)
		{
			logit (LOG_DAEMON, LOG_ERR,
				"%s: Invalid entry in %s at line %d",
				func, fname, line_no);
			continue;
		}

		type_val = dict_valfind (type);
		if (type_val == (DICT_VALUE *) NULL)
		{
			logit (LOG_DAEMON, LOG_ERR,
				"%s: Invalid TYPE '%s' in %s at line %d",
				func, type, fname, line_no);
			continue;
		}
		type_inx = type_val->value;

		host = strtok (NULL, " \t\n\r");
		filter = strtok (NULL, " \t\n\r");

		if (host == NULL && type_inx != AA_UNIX)
		{
			logit (LOG_DAEMON, LOG_ERR,
				"%s: Invalid entry in %s at line %d",
				func, fname, line_no);
			continue;
		}

		if ((auth_ent = (AUTH_ENTRY *) malloc (sizeof (AUTH_ENTRY)))
				== (AUTH_ENTRY *) NULL)
		{
			logit (LOG_DAEMON, LOG_ALERT,
				"%s: No memory for auth_ent", func);
			fclose (authfd);
			return (-1);
		}
		auth_ent->parent = (AUTH_ENTRY *) NULL;
		auth_ent->type = type_inx;

		if (is_engine == 1) /* then we are being called by the engine */
		{
			if (authtype_tv[type_inx] == (AATV *) NULL)
			{
				logit (LOG_DAEMON, LOG_ERR,
				  "%s: Missing AATV for entry on line %d of %s",
					func, line_no, fname);
				fclose (authfd);
				return (-1);
			}
		}

		auth_ent->prot = prot_inx;
		auth_ent->name = add_string (name, ASIS);
		auth_ent->host = add_string (host, ASIS);
		auth_ent->filter = add_string (filter, ASIS);
		auth_ent->next = (AUTH_ENTRY *) NULL;
		*end_ent = auth_ent;
		end_ent = &auth_ent->next;
		ent_cnt++;

		/* Add alias entries if aliases given */
		if (alias_list)
		{
			alias_list++;
			name = strtok (alias_list, " ,)");
			while (name)
			{
				if ((alias_ent = 
					(AUTH_ALIAS_ENTRY *) malloc 
						(sizeof(AUTH_ALIAS_ENTRY)) )
						   == (AUTH_ALIAS_ENTRY *) NULL)
				{
					logit (LOG_DAEMON, LOG_ALERT,
						"%s: No memory for auth_ent",
						func);
					fclose (authfd);
					return (-1);
				}
				alias_ent->name = add_string (name, ASIS);
				alias_ent->parent = auth_ent;
				alias_ent->next = (AUTH_ENTRY *) NULL;
				*end_ent = (AUTH_ENTRY *) alias_ent;
				end_ent = &alias_ent->next;
				name = strtok (NULL, " ,)");
			}
		}
	}
	fclose (authfd);
	if (dolog)
	{
		logit (LOG_DAEMON, LOG_INFO,
			"%s: %s (%u entries) read to memory",
			func, fname, ent_cnt);
	}
	authfile_cnt += ent_cnt;

	return 0;
} /* end of read_auth () */

#define FIND_MODE_NAME	0
#define FIND_MODE_REPLY	1
#define FIND_MODE_SKIP	2
#define FIND_MODE_FLUSH	3

/*************************************************************************
 *
 *	Function: read_users
 *
 *	Purpose: For each entry in a "users" file (with the file_pfx
 *		 corresponding to the given file_ent), read all check_items
 *		 and reply_items into a data structure.  Each such
 *		 data structure is linked to the given file_ent.
 *
 *	Returns:  0 = normal (successful) return,
 *		 -1 = error return.
 *
 *************************************************************************/

static int
read_users (file_ent, dolog)

FILE_LIST      *file_ent;
int             dolog;

{
	FILE           *userfd;
	char           *ptr;
	int             mode;
	int             line_nbr = 0;
	int             count = 0;
	USER_ENTRY     *user_ent = (USER_ENTRY *) NULL;
	USER_ENTRY    **end_ent;
	char            buffer[256];
	char            fname[MAXPATHLEN];
	static char    *func = "read_users";

	dprintf(2, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

	/*
	 * Open the user table
	 */
	sprintf (fname, "%s/%s%s", radius_dir, file_ent->prefix, RADIUS_USERS);
	if ((userfd = fopen (fname, "r")) == (FILE *) NULL)
	{
		/*
		 *  It's ok for prefixed user files to not exist, but
		 *  non-prefixed user file has to exist.
		 */
		if (file_ent->prefix[0] == 0)
		{
			logit (LOG_DAEMON, LOG_ERR,
				"%s: Couldn't open %s for reading",
			 	func, fname);
			return (-1);
		}
		return (0);
	}

	end_ent = &file_ent->user_list;

	mode = FIND_MODE_NAME;
	while (fgets (buffer, sizeof (buffer), userfd) != (char *) NULL)
	{
		line_nbr++;
		if (*buffer == COMMENT)
		{
			continue;
		}

		for (;;)
		{
			if (mode == FIND_MODE_NAME)
			{
				if (isspace (*buffer))
				{
					break;  /* to read another line */
				}
				if ((user_ent = (USER_ENTRY *)
						malloc (sizeof (USER_ENTRY)))
							== (USER_ENTRY *) NULL)
				{
					logit (LOG_DAEMON, LOG_ALERT,
				     "%s: Couldn't allocate USER_ENTRY storage",
						func);
					return (-1);
				}
				ptr = strtok (buffer, " \t\n\r");
				user_ent->name = add_string (ptr, ASIS);
				user_ent->check = (VALUE_PAIR *) NULL;
				user_ent->reply = (VALUE_PAIR *) NULL;
				user_ent->next = (USER_ENTRY *) NULL;

				/*
				 * Parse the check values
				 */
				ptr += strlen (ptr) + 1;
				if (pair_parse (ptr, &user_ent->check) != 0)
				{
					logit (LOG_DAEMON, LOG_ERR,
				      "%s: %d: Parse error for user %s (check)",
					 	func, line_nbr, user_ent->name);
					free_user_ent (user_ent);
					user_ent = (USER_ENTRY *) NULL;
				}
				else
				{
					mode = FIND_MODE_REPLY;
				}
				break;  /* to read another line */
			}

			/* Reading reply items */
			if (isspace (*buffer))
			{
				/*
				 * Parse the reply values
				 */
				if (pair_parse (buffer, &user_ent->reply) != 0)
				{
					logit (LOG_DAEMON, LOG_ERR,
				      "%s: %d: Parse error for user %s (reply)",
						func, line_nbr, user_ent->name);
					free_user_ent (user_ent);
					user_ent = (USER_ENTRY *) NULL;
					mode = FIND_MODE_NAME;
				}
				break;
			}

			/* Start of next entry */
			*end_ent = user_ent;
			end_ent = &user_ent->next;
			user_ent = (USER_ENTRY *) NULL;
			mode = FIND_MODE_NAME;
			count++;
			continue;       /* with same input */
		}
	}
	fclose (userfd);
	if (user_ent != (USER_ENTRY *) NULL)
	{
		*end_ent = user_ent;
		count++;
	}

	if (dolog)
	{
		logit (LOG_DAEMON, LOG_INFO,
			"%s: %s (%u entries) read to memory",
			func, fname, count);
	}
	users_cnt += count;

	return (0);
} /* end of read_users () */

/*************************************************************************
 *
 *	Function: return_file_list
 *
 *	Purpose: Returns the private file_list pointer for clients in need.
 *
 *************************************************************************/

FILE_LIST *
return_file_list ()

{
	return (file_list);
} /* end of return_file_list () */

/*************************************************************************
 *
 *	Function: update_clients
 *
 *	Purpose: Updates IP address(es) and expire_time in given
 *		 CLIENT_ENTRY node.
 *
 *************************************************************************/

int
update_clients ()

{
	int                len;
	int                notify_count = 0;
	struct hostent    *hp;
	struct in_addr   **addrlist;
	char              *aliascnt;
	char              *ptr;
	char             **name;
	CLIENT_ENTRY      *client_ent;
	struct sockaddr_in send_sin;
	time_t             cur_time;
	static char       *func = "update_clients";

	dprintf(4, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

	if (dnspid != 0)	/* Already resolving addrs */
	{
		return 0;
	}

	if (spawn_flag > 0)
	{
		if ((dnspid = (int) fork ()) < 0)	
		{
			dnspid = 0;
			logit (LOG_DAEMON, LOG_ALERT, "%s: fork: %s",
				func, get_errmsg ());
			return (-1);
		}

		if (dnspid != 0)	/* Parent */
		{
			return 0;
		}
	}
	
	/* ======= Child process code ======= */

	memset ((char *) &send_sin, '\0', sizeof (send_sin));
	send_sin.sin_family = AF_INET;
	send_sin.sin_addr.s_addr = htonl(self_ip[0]);
	send_sin.sin_port = htons(rad_ipc_port);

	cur_time = time (0);
	for (client_ent = client_list;
		client_ent != (CLIENT_ENTRY *) NULL;
		client_ent = client_ent->next)
	{
		if (cur_time < client_ent->expire_time)
		{
			continue;
		}

		hp = gethostbyname (client_ent->hostname);
		ptr = send_buffer;
		*ptr++ = '\0';		/* Only one code for now */
		strcpy (ptr, client_ent->hostname);
		ptr += strlen (ptr) + 1;
		*ptr = '\0';
		aliascnt = ptr++;

		if (hp != (struct hostent *) NULL)
		{
			if (hp->h_aliases != (char **) NULL)
			{
				for (name = hp->h_aliases;
					*name != (char *) NULL;
					name++)  
				{
					if (strcmp (client_ent->hostname,
						    *name) != 0)
					{
						(*aliascnt)++; 
						strcpy (ptr, *name);
						ptr += strlen (ptr) + 1; 

						/* Indicate just an alias */
						*ptr++ = 1;
					}
				}
			}

			/* Pass official name last */
			if (strcmp (client_ent->hostname, hp->h_name) != 0)
			{
				(*aliascnt)++; 
				strcpy (ptr, hp->h_name);
				ptr += strlen (ptr) + 1; 
				*ptr++ = 0;	/* Indicate official name */
			}

			if (hp->h_addr_list != (char **) NULL)
			{
				addrlist = (struct in_addr **) hp->h_addr_list;
				while (*addrlist)
				{
					memcpy (ptr, *(char **) addrlist,
						sizeof (struct in_addr));
					ptr += sizeof (struct in_addr);
					addrlist++;
				}
			}
			memset (ptr, '\0', sizeof (struct in_addr)); 
		}
		else /* Extra check for brain-dead gethostbyname() calls */
		{
			if (client_ent->type == CE_NUMERIC)
			{
				struct in_addr temp;

				temp.s_addr = ntohl(inet_addr (client_ent->hostname));
				memcpy (ptr, (char *) &temp,
					sizeof (struct in_addr));
				ptr += sizeof (struct in_addr);
				memset (ptr, '\0', sizeof (struct in_addr));
			}
			else
			{
				memset (ptr, '\0', sizeof (struct in_addr)); 
				/* Pass error code in packet */
				*(ptr + sizeof (struct in_addr) - 1) = h_errno;
			}
		}

		len = ptr - send_buffer + sizeof (struct in_addr);
		notify_count++;

		dprintf (2, (LOG_DAEMON, LOG_DEBUG,
			"%s: Sendto call number (%d) for client '%s'",
			func, notify_count, client_ent->hostname));

		if (notify_count % DNS_SLEEP == 0)
		{
			sleep (1);
		}

		/* Send it to main process */
		sendto (rad_ipc_aatv->sockfd, send_buffer, len, (int) 0,
			(struct sockaddr *) & send_sin, sizeof (send_sin));

	}

	if (spawn_flag > 0)
	{
		_exit (0);
	}

	return 0;
} /* end of update_clients () */

/*************************************************************************
 *
 *	Function: user_find
 *
 *	Purpose: Find the named user in the users database.  Create the
 *		 set of attribute-value pairs to check and reply with
 *		 for this user from the database.
 *
 *		 Note that the users database can be either in the file
 *		 "RADIUS_USERS" (-u option), in memory (default), or in
 *		 a dbm(3) database.
 *
 *************************************************************************/

int
user_find (file_pfx, name, protocol, check_pairs, reply_pairs, not_user_file)

char           *file_pfx;        /* Selects which "users" file to use */
char           *name;
int             protocol;
VALUE_PAIR    **check_pairs;
VALUE_PAIR    **reply_pairs;
int             not_user_file;   /* Look up user in a domain specific file */

{
	int             mode;
	int             namelen;
	FILE           *userfd;
	VALUE_PAIR     *check_first;
	VALUE_PAIR     *reply_first;
	VALUE_PAIR     *prot_ent;
	FILE_LIST      *file_ent;
	USER_ENTRY     *user_ent;
	USER_ENTRY     *dflt_ent;
	char           *ptr;
	FILE           *debugout = stdout;
	char            buffer[256];
	char            fname[MAXPATHLEN];
	static char    *func = "user_find";

#ifdef USE_NDBM
	datum           named;
	datum           contentd;
	DBM            *db;
#endif	/* USE_NDBM */

#ifdef USE_DBM
	datum           named;
	datum           contentd;
#endif	/* USE_DBM */

	dprintf(2, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

	*check_pairs = check_first = (VALUE_PAIR *) NULL;
	*reply_pairs = reply_first = (VALUE_PAIR *) NULL;

	if ((namelen = strlen (name)) == 0)
	{
		return 1;	/* A null name would match every line. */
	}

	/*
	 *	First check what type of lookup to do.
	 *	See if user file(s) have been cached in memory.
	 */

	if (not_user_file == 0 &&
		(dflt_ent = file_list->user_list) != (USER_ENTRY *) NULL)
	{
		if ((file_ent = find_file_ent (file_pfx)) == (FILE_LIST *) NULL)
		{
			return (-1);
		}
		if ((user_ent = file_ent->user_list) == (USER_ENTRY *) NULL)
		{
			/* If no user file for this prefix, ...  */
			/* ... then use first entry in file_list as ... */
			/* ... default file_ent. */
			user_ent = dflt_ent;
		}
		for (; user_ent; user_ent = user_ent->next)
		{
			/* Allow match for entry specifying framed protocol
			 * type specified in "protocol".  An entry with a 
			 * matching name but no framed-protocol type check 
			 * item matches unconditionally.  An entry with a 
			 * matching name and a framed-protocol type check 
			 * item must match value in "protocol".
			 */

			if (strcmp (user_ent->name, name) == 0)
			{
				if ((prot_ent =
					get_vp (user_ent->check,
						PW_FRAMED_PROTOCOL))
							== (VALUE_PAIR *) NULL)
				{
					break;
				}
				if (prot_ent->lvalue == protocol)
				{
					break;
				}
			}
		}
		if (!user_ent)	/* rc 1 => User not found and no error */
		{
			return 1;
		}
		list_copy (&check_first, user_ent->check);
		list_copy (&reply_first, user_ent->reply);
		if (debug_flag >= 2)
		{
			if (ddt)
			{
				debugout = ddt;
			}

			fprintf (debugout, "Check items:\n");
			debug_list (debugout, user_ent->check);
			fprintf (debugout, "Reply items:\n");
			debug_list (debugout, user_ent->reply);
		}
		*check_pairs = check_first;
		*reply_pairs = reply_first;
		return (0);	/* rc 0 => User found */
	} /* End of cached users file(s) lookup */

	/*
	 * Open the user table.
	 * If prefixed file doesn't exist, use default (non-prefixed) file.
	 */
	for (;;)
	{
		sprintf (fname, "%s/%s%s", radius_dir,
			file_pfx ? file_pfx : "", RADIUS_USERS);

#if defined(USE_DBM) || defined(USE_NDBM)
#ifdef USE_NDBM
		if ((db = dbm_open (fname, O_RDONLY, 0)) == 0)
		{
#endif	/* USE_NDBM */
#ifdef USE_DBM
		if (dbminit (fname) != 0)
		{
#endif	/* USE_DBM */
#else	/* USE_DBM || USE_NDBM */
		if ((userfd = fopen (fname, "r")) == (FILE *) NULL)
		{
#endif	/* USE_DBM || USE_NDBM */

			if (not_user_file == 0 &&
				file_pfx != NULL &&
				file_pfx[0] != 0)
			{
				file_pfx = NULL;
				continue;
			}

			logit (LOG_DAEMON, LOG_ERR,
				"%s: Couldn't open %s for reading",
			 	func, fname);
			return (-1);
		}
		break;
	}

#if defined(USE_DBM) || defined(USE_NDBM)
/*
 * Note that the DBM feature currently does not support protocol specific user
 * entry matching.  It could be done by appending a protocol type to the
 * name when building the database and looking for that modified name here.
 * This seems like a non-trivial task and we don't use the DBM feature
 * here so I'll leave this to someone else to implement.
 */
	named.dptr = name;
	named.dsize = namelen;

#ifdef	USE_NDBM
	contentd = dbm_fetch (db, named);
#else	/* USE_NDBM */
	contentd = fetch (named);
#endif	/* USE_NDBM */

	if (contentd.dsize == 0)
	{
		named.dptr = "DEFAULT";
		named.dsize = strlen ("DEFAULT");

#ifdef	USE_NDBM
		contentd = dbm_fetch (db, named);
#else	/* USE_NDBM */
		contentd = fetch (named);
#endif	/* USE_NDBM */

		if (contentd.dsize == 0)
		{
#ifdef	USE_NDBM
			dbm_close (db);
#else	/* USE_NDBM */
			dbmclose ();
#endif	/* USE_NDBM */
			return (-1);
		}
	}

	/*
	 * Parse the check values
	 */
	ptr = contentd.dptr;
	contentd.dptr[contentd.dsize] = '\0';

	if (pair_parse (ptr, &check_first) != 0)
	{
		logit (LOG_DAEMON, LOG_ERR,
			"%s: Parse error for user %s in %s (check)",
			func, name, fname);
		list_free (check_first);
#ifdef	USE_NDBM
		dbm_close (db);
#else	/* USE_NDBM */
		dbmclose ();
#endif	/* USE_NDBM */
		return (-1);
	}
	while (*ptr != '\n' && *ptr != '\0')
	{
		ptr++;
	}
	if (*ptr != '\n')
	{
		list_free (check_first);
#ifdef	USE_NDBM
		dbm_close (db);
#else	/* USE_NDBM */
		dbmclose ();
#endif	/* USE_NDBM */
		return (-1);
	}
	ptr++;

	/*
	 * Parse the reply values
	 */
	if (pair_parse (ptr, &reply_first) != 0)
	{
		logit (LOG_DAEMON, LOG_ERR,
			"%s: Parse error for user %s in %s (reply)",
			func, name, fname);
		list_free (check_first);
		list_free (reply_first);
#ifdef	USE_NDBM
		dbm_close (db);
#else	/* USE_NDBM */
		dbmclose ();
#endif	/* USE_NDBM */
		return (-1);
	}
#ifdef	USE_NDBM
	dbm_close (db);
#else	/* USE_NDBM */
	dbmclose ();
#endif	/* USE_NDBM */
	*check_pairs = check_first;
	*reply_pairs = reply_first;
	return (0);

#else /* USE_DBM || USE_NBDM */

	mode = FIND_MODE_NAME;

	while (fgets (buffer, sizeof (buffer), userfd) != (char *) NULL)
	{
		if (*buffer == COMMENT)
		{
			continue;
		}

		if (mode == FIND_MODE_NAME)
		{
			/*
			 * Find the entry starting with the users name.
			 */
			if (strncmp (buffer, name, namelen) == 0 &&
			    (buffer[namelen] == ' ' || buffer[namelen] == '\t'))
			{
				ptr = &buffer[namelen];

				/*
				 * Parse the check values
				 */
				if (pair_parse (ptr, &check_first) != 0)
				{
					logit (LOG_DAEMON, LOG_ERR,
				"%s: Parse error for user %s in %s (DBM check)",
						 func, name, fname);
					list_free (check_first);
					fclose (userfd);
					return (-1);
				}
				/* Allow match for entry specifying framed
				 * protocol type specified in "protocol".  An
				 * entry with matching name but no
				 * framed-protocol type check item matches
				 * unconditionally.  An entry with matching
				 * name and a framed-protocol type check item
				 * must match value in "protocol".
				 */
				if ((prot_ent =
					get_vp (check_first,
						PW_FRAMED_PROTOCOL))
							!= (VALUE_PAIR *) NULL)
				{
					if (prot_ent->lvalue != protocol)
					{
						list_free (check_first);
						continue;
					}
				}
				mode = FIND_MODE_REPLY;
			}
		}
		else
		{
			if (*buffer == ' ' || *buffer == '\t')
			{
				/*
				 * Parse the reply values
				 */
				if (pair_parse (buffer, &reply_first) != 0)
				{
					logit (LOG_DAEMON, LOG_ERR,
				"%s: Parse error for user %s in %s (DBM reply)",
						 func, name, fname);
					list_free (check_first);
					list_free (reply_first);
					fclose (userfd);
					return (-1);
				}
			}
			else /* We are done */
			{
				break;
			}
		}
	}
	fclose (userfd);

	/* Update the callers pointers */
	if (mode == FIND_MODE_NAME)
	{
		return 1;		/* No error and no match */
	}
	*check_pairs = check_first;
	*reply_pairs = reply_first;
	return (0);
#endif	/* USE_DBM || USE_NBDM */
} /* end of user_find () */

static char * months[] =
		{
			"Jan", "Feb", "Mar", "Apr", "May", "Jun",
			"Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
		};

/*************************************************************************
 *
 *	Function: user_gettime
 *
 *	Purpose: Turns printable string into correct tm struct entries.
 *
 *************************************************************************/

void
user_gettime (valstr, tm)

char           *valstr;
struct tm      *tm;

{
	int             i;
	static char    *func = "user_gettime";

	dprintf(2, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

	/* Get the month */
	for (i = 0; i < 12; i++)
	{
		if (strncmp (months[i], valstr, 3) == 0)
		{
			tm->tm_mon = i;
			i = 13;
		}
	}

	/* Get the Day */
	tm->tm_mday = atoi (&valstr[4]);

	/* Now the year */
	tm->tm_year = atoi (&valstr[7]) - 1900;
} /* end of user_gettime () */

/*************************************************************************
 *
 *	Function: user_update
 *
 *	Purpose: Updates a user in the database.  Replaces the original
 *		 entry with the name, the list of check items, and the
 *		 list of reply items which are supplied.
 *
 *************************************************************************/

int
user_update (name, user_check, user_reply)

char           *name;
VALUE_PAIR     *user_check;
VALUE_PAIR     *user_reply;

{
	FILE           *oldfd;
	FILE           *userfd;
	char            buffer[256];
	char            buffer1[256];
	int             namelen;
	int             mode;
	static char    *func = "user_update";

	dprintf(2, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

	sprintf (buffer, "%s/%s", radius_dir, RADIUS_USERS);
	sprintf (buffer1, "%s/%s", radius_dir, RADIUS_HOLD);

	/* Move the user table to a temporary location */
	if (rename (buffer, buffer1) != 0)
	{
		logit (LOG_DAEMON, LOG_ERR, "%s: Couldn't rename %s",
			func, buffer);
		return (-1);
	}

	/* Open the old user file (using the temporary name */
	if ((oldfd = fopen (buffer1, "r")) == (FILE *) NULL)
	{
		logit (LOG_DAEMON, LOG_ERR, "%s: Couldn't open %s for reading",
			func, buffer1);
		exit (-9);
	}

	/* Open the new user file */
	if ((userfd = fopen (buffer, "w")) == (FILE *) NULL)
	{
		logit (LOG_DAEMON, LOG_ERR, "%s: Couldn't open %s for writing",
			func, buffer);
		exit (-9);
	}

	mode = FIND_MODE_NAME;
	namelen = strlen (name);

	/* Copy the old to the new, only recreating the changed user */
	while (fgets (buffer, sizeof (buffer), oldfd) != (char *) NULL)
	{
		if (mode == FIND_MODE_NAME)
		{
			if ((strncmp (buffer, name, namelen) == 0 &&
			   (buffer[namelen] == ' ' || buffer[namelen] == '\t')))
			{

				/* Write our new information */
				fprintf (userfd, "%s\t", name);
				while (user_check != (VALUE_PAIR *) NULL)
				{
					fprint_attr_val (userfd, user_check);
					if (user_check->next !=
							(VALUE_PAIR *) NULL)
					{
						fprintf (userfd, ", ");
					}
					user_check = user_check->next;
				}
				fprintf (userfd, "\n\t");
				while (user_reply != (VALUE_PAIR *) NULL)
				{
					fprint_attr_val (userfd, user_reply);
					if (user_reply->next !=
							(VALUE_PAIR *) NULL)
					{
						fprintf (userfd, ",\n\t");
					}
					user_reply = user_reply->next;
				}
				fprintf (userfd, "\n");
				mode = FIND_MODE_SKIP;
			}
			else
			{
				fputs (buffer, userfd);
			}
		}
		else if (mode == FIND_MODE_SKIP)
		{
			if (*buffer != ' ' && *buffer != '\t')
			{
				fputs (buffer, userfd);
				mode = FIND_MODE_FLUSH;
			}
		}
		else
		{
			fputs (buffer, userfd);
		}
	}
	fclose (oldfd);
	fclose (userfd);
	return (0);
} /* end of user_update () */
