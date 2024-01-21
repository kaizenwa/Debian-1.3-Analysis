/*
 * dnscheck -- Check to see if the host's hostname/DNS configuration works.
 *
 *
 * COPYRIGHT  (c)  1992, 1993, 1994, 1995, 1996
 * THE REGENTS OF THE UNIVERSITY OF MICHIGAN AND MERIT NETWORK, INCORPORATED
 * ALL RIGHTS RESERVED
 * 
 * PERMISSION IS GRANTED TO USE, COPY, CREATE DERIVATIVE WORKS AND REDISTRIBUTE
 * THIS SOFTWARE AND SUCH DERIVATIVE WORKS IN BINARY FORM ONLY FOR ANY PURPOSE,
 * SO LONG AS NO FEE IS CHARGED, AND SO LONG AS THE COPYRIGHT NOTICE ABOVE, THIS
 * GRANT OF PERMISSION, AND THE DISCLAIMER BELOW APPEAR IN ALL COPIES MADE; AND
 * SO LONG AS THE NAME OF THE UNIVERSITY OF MICHIGAN IS NOT USED IN ANY
 * ADVERTISING OR PUBLICITY PERTAINING TO THE USE OR DISTRIBUTION OF THIS
 * SOFTWARE WITHOUT SPECIFIC, WRITTEN PRIOR AUTHORIZATION.
 * 
 * THIS SOFTWARE IS PROVIDED AS IS, WITHOUT REPRESENTATION FROM THE UNIVERSITY
 * OF MICHIGAN AS TO ITS FITNESS FOR ANY PURPOSE, AND WITHOUT WARRANTY BY THE
 * UNIVERSITY OF MICHIGAN OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING
 * WITHOUT LIMITATION THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE.  THE REGENTS OF THE UNIVERSITY OF MICHIGAN SHALL NOT BE
 * LIABLE FOR ANY DAMAGES, INCLUDING SPECIAL, INDIRECT, INCIDENTAL, OR
 * CONSEQUENTIAL DAMAGES, WITH RESPECT TO ANY CLAIM ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OF THE SOFTWARE, EVEN IF IT HAS BEEN OR IS HEREAFTER
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
 * 
 * For a License to distribute source code or to charge a fee for the program
 * or a product containing the program, contact MERIT at the University of
 * Michigan:
 * 
 * aaa-license@merit.edu
 * 
 * [This version puts NO LIMITS on the use.  It grants the right to create
 * DERIVATIVE WORKS.  The user may copy and distribute the code in the form
 * received AND DERIVATIVE WORKS, so long as no fee is charged.  If copies are
 * made, our copyright notice and the disclaimer must be included on them.  USE
 * THIS VERSION WITH CARE.  THIS VERSION VERY LIKELY WILL KILL ANY POTENTIAL
 * FOR LATER COMMERCIALIZATION OF THE SOFTWARE.]
 *
 *
 * No public entry points in this file.  This is a standalone program.
 *
 */

static char     rcsid[] =
		"$Id: dnscheck.c,v 1.5 1996/06/06 21:54:45 web Exp $";

#include	<netdb.h>
#include	<stdio.h>
#include	<time.h>
#include	<memory.h>
#include	<string.h>
#include	<sys/param.h>
#include	<sys/types.h>
#include	<sys/socket.h>
#include	<netinet/in.h>
#include	<arpa/inet.h>

#include	"radius.h"

#if defined(ultrix) || defined(__hpux) || defined(__bsdi__) || defined(linux) || defined(SCO)
extern int           h_errno;
#endif	/* ultrix */


/* Some private globals */

static char    *progname = "?";
static int      debug = 0;	/* To help the developer out. */
static int      terse = 0;	/* Turn this on to make it quieter. */
static int      check = 0;	/* Turn this on for RADIUS checking. */

typedef struct named_list_struct
{
	struct named_list_struct *next;
	struct my_hostent_struct *mp;
} named_list_t;

static named_list_t *cached_name = NULL;
static named_list_t *cached_addr = NULL;

typedef struct my_hostent_struct
{
	char           *h_name;		/* just like struct hostent */
	char          **h_aliases;	/* just like struct hostent */
	int             h_addrtype;	/* just like struct hostent */
	int             h_length;	/* just like struct hostent */
	char          **h_addr_list;	/* just like struct hostent */

	/* Our special stuff. */

	struct my_hostent_struct *m_name; /* For gethostbyaddr() thingy. */
	struct my_hostent_struct **m_addr_list;	/* Quick reference to other. */
} my_hostent_t;

static my_hostent_t *report_addr PROTO((int, int, char *));

/*****************************************************************************
 *
 *	Function: make_my_hostent
 *
 *	Purpose: ??? XXX ???
 *
 ****************************************************************************/

my_hostent_t *
make_my_hostent (hp)

struct hostent *hp;

{
	my_hostent_t   *new;
	char          **tab;
	int             i;

	if (!hp)
	{
		return NULL;
	}

	if (!(new = (my_hostent_t *) calloc (1, sizeof (my_hostent_t))))
	{
		fprintf (stderr, "%s: calloc(1, sizeof(my_hostent_t)) failed\n",
			 progname);
		abort ();
	}

	new->h_name = strdup (hp->h_name);
	new->h_addrtype = hp->h_addrtype;
	new->h_length = hp->h_length;

	i = 1;
	for (tab = hp->h_aliases; *tab; tab++)
	{
		i++;
	}

	new->h_aliases = (char **) calloc (i, sizeof (char *));
	for (i = 0; hp->h_aliases[i]; i++)
	{
		new->h_aliases[i] = strdup (hp->h_aliases[i]);
	}

	i = 1;
	for (tab = hp->h_addr_list; *tab; tab++)
	{
		i++;
	}

	new->h_addr_list = (char **) calloc (i, sizeof (char *));
	new->m_addr_list = (my_hostent_t **) calloc (i, sizeof (my_hostent_t *));

	for (i = 0; hp->h_addr_list[i]; i++)
	{
		new->h_addr_list[i] = (char *) calloc (1, new->h_length);
		memcpy (new->h_addr_list[i], hp->h_addr_list[i], new->h_length);
	}

	return new;
} /* end of make_my_hostent () */

/*****************************************************************************
 *
 *	Function: free_my_hostent
 *
 *	Purpose: ??? XXX ???
 *
 ****************************************************************************/

static void
free_my_hostent (mp)

my_hostent_t   *mp;

{
	char          **p;

	if (mp)
	{
		if (mp->h_name)
		{
			free (mp->h_name);
		}

		for (p = mp->h_aliases; *p; p++)
		{
			free (*p);
		}
		free (mp->h_aliases);

		for (p = mp->h_addr_list; *p; p++)
		{
			free (*p);
		}
		free (mp->h_addr_list);

		/*
		 * Assume others are still NULL
		 */
		free (mp->m_addr_list);

		free (mp);
	}
	return;
} /* end of free_my_hostent () */

/*****************************************************************************
 *
 *	Function: already_named
 *
 *	Purpose: ??? XXX ???
 *
 ****************************************************************************/

static my_hostent_t *
already_named (name)

char           *name;

{
	named_list_t   *each;
	char          **p;

	for (each = cached_name; each; each = each->next)
	{
		if (strcasecmp (name, each->mp->h_name) == 0)
		{
			return each->mp;
		}

		for (p = each->mp->h_aliases; *p; p++)
		{
			if (strcasecmp (name, *p) == 0)
			{
				return each->mp;
			}
		}
	}
	return NULL;
} /* end of already_named () */

/*****************************************************************************
 *
 *	Function: cache_hostent_name
 *
 *	Purpose: ??? XXX ???
 *
 ****************************************************************************/

static my_hostent_t *
cache_hostent_name (mp)

my_hostent_t   *mp;

{
	named_list_t   *each;
	char          **p;
	char          **q;

	for (each = cached_name; each; each = each->next)
	{
		if (strcasecmp (mp->h_name, each->mp->h_name) == 0)
		{
			return each->mp;
		}

		for (p = each->mp->h_aliases; *p; p++)
		{
			if (strcasecmp (*p, mp->h_name) == 0)
			{
				return each->mp;
			}

			for (q = mp->h_aliases; *q; q++)
			{
				if (strcasecmp (*p, *q) == 0)
				{
					return each->mp;
				}
			}
		}
	}

	each = (named_list_t *) calloc (1, sizeof (named_list_t));
	each->mp = mp;
	each->next = cached_name;
	cached_name = each;
	return each->mp;
} /* end of cache_hostent_name () */

/*****************************************************************************
 *
 *	Function: already_addrd
 *
 *	Purpose: ??? XXX ???
 *
 ****************************************************************************/

static my_hostent_t *
already_addrd (addr)

char           *addr;

{
	named_list_t   *each;
	char          **p;

	for (each = cached_addr; each; each = each->next)
	{
		for (p = each->mp->h_addr_list; *p; p++)
		{
			if (memcmp (addr, *p, 4) == 0)
			{
				return each->mp;
			}
		}
	}
	return NULL;
} /* end of already_addrd () */

/*****************************************************************************
 *
 *	Function: cache_hostent_addr
 *
 *	Purpose: ??? XXX ???
 *
 ****************************************************************************/

static my_hostent_t *
cache_hostent_addr (mp)

my_hostent_t   *mp;

{
	named_list_t   *each;
	char          **p;
	char          **q;

	for (each = cached_addr; each; each = each->next)
	{
		for (p = each->mp->h_addr_list; *p; p++)
		{
			for (q = mp->h_addr_list; *q; q++)
			{
				if (memcmp (*p, *q, 4) == 0)
				{
					return each->mp;
				}
			}
		}
	}

	each = (named_list_t *) calloc (1, sizeof (named_list_t));
	each->mp = mp;
	each->next = cached_addr;
	cached_addr = each;
	return each->mp;
} /* end of cache_hostent_addr () */

/*****************************************************************************
 *
 *	Function: report_name
 *
 *	Purpose: ??? XXX ???
 *
 ****************************************************************************/

my_hostent_t *
report_name (lvl, sib, hostname)

int             lvl;
int             sib;
char           *hostname;

{
	int             i;
	my_hostent_t   *np;
	my_hostent_t   *mp = NULL;
	char          **p;
	char            etc[200];

	if (terse == 0)
	{
		if (lvl > 0)
		{
			printf ("#%d.%d", lvl, sib);
		}

		for (i = 0; i < lvl; i++)
		{
			putchar ('\t');
		}
	}

	sib = 1;

	if ((mp = already_named (hostname)))
	{
		if (terse == 0)
		{
			printf ("(%s)\t\t# Shown earlier (by name)\n", hostname);
		}

		return mp;
	}

	if ((mp = make_my_hostent (gethostbyname (hostname))))
	{
		if ((np = cache_hostent_name (mp)) != mp)
		{
			free_my_hostent (mp);
			mp = np;
		}

		if (terse == 0)
		{
			printf ("%s\t", mp->h_name);

			etc[0] = '\0';
			if (strcasecmp (hostname, mp->h_name) != 0)
			{
				strcat (etc, "\t <--- Warning, not canonical");
				printf ("( != %s)", hostname);
			}

			printf (" => [");
			for (p = mp->h_addr_list; *p; p++)
			{
				printf ("%s",
					inet_ntoa (*(struct in_addr *) * p));
				if (p[1])
				{
					printf (", ");
				}
			}

			printf ("]%s\n", etc);
		} /* if (terse == 0) */

		for (p = mp->h_aliases, i = 0; p[i]; i++)
		{
			np = report_name (lvl + 1, sib++, p[i]);
		}

		for (p = mp->h_addr_list, i = 0; p[i]; i++)
		{
			np = report_addr (lvl + 1, sib++, p[i]);
			if (np)
			{
				mp->m_addr_list[i] = np;
			}
		}
	}
	else /* make_my_hostent() found errors */
	{
		printf ("%s: %s\t", progname, hostname);
		switch (h_errno)
		{
		    case HOST_NOT_FOUND:
			printf ("*** Not in DNS!\n");
			break;

		    case TRY_AGAIN:
			printf ("Try again later.\n");
			break;

		    case NO_DATA:
			printf ("Not a host name.\n");
			break;

		    case NO_RECOVERY:
			printf ("*** DNS server error\n");
			break;

		    default:
			printf ("*** Unknown h_errno=%d\n", h_errno);
			break;
		} /* switch (h_errno) */
	} /* if ((mp == make_my_hostent (gethostbyname (hostname)))) */

	return mp;

} /* end of report_name () */

/*****************************************************************************
 *
 *	Function: report_addr
 *
 *	Purpose: ??? XXX ???
 *
 ****************************************************************************/

static my_hostent_t *
report_addr (lvl, sib, hostaddr)

int             lvl;
int             sib;
char           *hostaddr;

{
	my_hostent_t   *np;
	my_hostent_t   *mp = NULL;
	char          **p;
	int             i;

	if (terse == 0)
	{
		if (lvl > 0)
		{
			printf ("#%d.%d", lvl, sib);
		}

		for (i = 0; i < lvl; i++)
		{
			putchar ('\t');
		}
	}
	sib = 0;

	if ((mp = already_addrd (hostaddr)))
	{
		if (terse == 0)
		{
			printf ("([%s])\t\t# Shown earlier (by addr)\n",
				inet_ntoa (*(struct in_addr *) hostaddr));
		}

		return mp;
	}

	if ((mp = make_my_hostent (gethostbyaddr (hostaddr, 4, AF_INET))))
	{
		if ((np = cache_hostent_addr (mp)) != mp)
		{
			free_my_hostent (mp);
			mp = np;
		}

		if (terse == 0)
		{
			printf ("[%s] => %s",
				inet_ntoa (*(struct in_addr *) mp->h_addr),
				mp->h_name);

			printf (" [");
			for (p = mp->h_addr_list; *p; p++)
			{
				if (memcmp (*p, hostaddr, 4) != 0)
				{
					printf ("%s",
					   inet_ntoa (*(struct in_addr *) * p));
				}
				else
				{
					printf ("==");
				}

				if (p[1])
				{
					printf (", ");
				}
			}

			printf ("]\n");
		} /* if (terse == 0) */

		np = report_name (lvl + 1, sib++, mp->h_name);
		if (np)
		{
			mp->m_name = np;
		}

		for (p = mp->h_aliases, i = 0; p[i]; i++)
		{
			np = report_name (lvl + 1, sib++, p[i]);
		}

		for (p = mp->h_addr_list, i = 0; p[i]; i++)
		{
			np = report_addr (lvl + 1, sib++, p[i]);
		}
	}
	else /* make_my_hostent() found errors */
	{
		printf ("%s: [%s]\t",
			progname, inet_ntoa (*(struct in_addr *) hostaddr));
		switch (h_errno)
		{
		    case HOST_NOT_FOUND:
			printf ("*** Not in DNS!\n");
			break;

		    case TRY_AGAIN:
			printf ("Try again later.\n");
			break;

		    case NO_DATA:
			printf ("Not a host addr.\n");
			break;

		    case NO_RECOVERY:
			printf ("*** DNS server error\n");
			break;

		    default:
			printf ("*** Unknown h_errno=%d\n", h_errno);
			break;
		}
	}
	return mp;
} /* end of report_addr () */

/*****************************************************************************
 *
 *	Function: main
 *
 *	Purpose: ??? XXX ???
 *
 ****************************************************************************/

int
main (argc, argv)

int             argc;
char          **argv;

{
	int             c;
	int             problems;
	char           *p;
	char          **tabs;
	my_hostent_t   *root;
	my_hostent_t  **subs;
	char            our_hostname[MAXHOSTNAMELEN + 1];
	extern char    *optarg;
	extern int      optind;

	gethostname (our_hostname, sizeof (our_hostname) - 1);
	our_hostname[sizeof (our_hostname) - 1] = '\0';	/* Safety. */

	progname = *argv;
	for (p = progname; *p; p++)
	{
		if (*p == '/')
		{
			progname = p + 1;
		}
	}

	while ((c = getopt (argc, argv, "ctvx")) != -1)
	{
		switch (c)
		{
		    case 'c':
			check++;
			break;

		    case 't':
			terse++;
			break;

		    case 'v':
			fprintf (stderr, "version %s\n", rcsid);
			exit (0);

		    case 'x':
			debug++;
			break;

		    default:
			fprintf (stderr, "unsupported option\n");
			exit (20);
		}
	}

	if ((terse > 0) && (check == 0))
	{
		fprintf (stderr,
	   "%s: use of -c without -t (or vice-versa) may result in no output\n",
			 progname);
	}

	if (optind < argc)
	{
		for (; optind < argc; optind++)
		{
			problems = 0;
			root = report_name (0, 0, argv[optind]);
			if (check > 0)
			{
				if (root)
				{
					/* Analyze this. */
					if (strcmp (argv[optind], root->h_name)
									!= 0)
					{
						printf ("The name '%s' isn't",
							argv[optind]);
						printf (" the CANONICAL DNS");
						printf (" name '%s'\n",
							root->h_name);
						problems++;
					}

					for (subs = root->m_addr_list,
						tabs = root->h_addr_list, c = 0;
					     tabs[c];
					     c++)
					{
						if (!subs[c])
						{
		  printf ("REVERSE MAPPING of [%s] back to '%s' not present\n",
					inet_ntoa (*(struct in_addr *) tabs[c]),
					root->h_name);
							problems++;
						}
						else
						{
							if (subs[c]->m_name != root)
							{
		printf ("REVERSE MAPPING of [%s] back to '%s' incorrect\n",
					inet_ntoa (*(struct in_addr *) tabs[c]),
					root->h_name);
				printf ("\treverse map points to \'%s\'\n",
						subs[c]->m_name->h_name);
						problems++;
							}
						}
					}
				}
				else
				{
					problems++;
				}

				if (terse == 0)
				{
					if (problems == 0)
					{
					      printf ("No problems with '%s'\n",
							argv[optind]);
					}
					else
					{
					printf ("%d problems found with '%s'\n",
							problems, argv[optind]);
					}
				}
			} /* if (check) */
			if ((optind < argc - 1) && (terse == 0))
			{
				printf ("\n---------------------------\n");
			}
		} /* For each remaining arg... */
	}
	else
	{
		problems = 0;
		if (!(root = report_name (0, 0, our_hostname)))
		{
		   printf ("YOUR SYSTEM'S HOSTNAME ('%s') IS NOT IN THE DNS!\n",
				our_hostname);
			exit (1);
		}
		else
		{
			if (check > 0)
			{
				if (strcmp (our_hostname, root->h_name) != 0)
				{
			       printf ("WARNING, your system's hostname '%s'\n",
						our_hostname);
				printf ("\tisn't the CANONICAL DNS name '%s'\n",
						root->h_name);
					problems++;
				}

				for (subs = root->m_addr_list,
					tabs = root->h_addr_list, c = 0;
				     tabs[c];
				     c++)
				{
					if (!subs[c])
					{
		   printf ("REVERSE MAPPING of [%s] back to '%s' not present\n",
					inet_ntoa (*(struct in_addr *) tabs[c]),
					root->h_name);
						problems++;
					}
					else
					{
						if (subs[c]->m_name != root)
						{
		     printf ("REVERSE MAPPING of [%s] back to '%s' incorrect\n",
					inet_ntoa (*(struct in_addr *) tabs[c]),
					root->h_name);
				       printf ("\treverse map points to '%s'\n",
						subs[c]->m_name->h_name);
							problems++;
						}
					}
				}	/* for each address... */

				if (terse == 0)
				{
					if (problems == 0)
					{
					printf ("No problems found with '%s'\n",
						our_hostname);
					}
					else
					{
					printf ("%d problems found with '%s'\n",
						problems, our_hostname);
					}
				} /* if terse == 0 */
			} /* if (check > 0) */
		}
	}

	exit (0);

} /* end of main () */
