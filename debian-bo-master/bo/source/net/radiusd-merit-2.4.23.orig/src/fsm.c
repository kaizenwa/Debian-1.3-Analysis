/* 
 * RADIUS -- Remote Authentication Dial In User Service
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
 * Public entry points in this file:
 *
 * find_aatv
 * init_fsm
 *
 */

static char     rcsid[] = "$Id: fsm.c,v 1.39 1996/06/13 19:23:42 web Exp $";

#include	<sys/types.h>
#include	<sys/param.h>
#include	<sys/stat.h>
#include	<netinet/in.h>

#include	<stdio.h>
#include	<syslog.h>
#include	<time.h>
#include	<ctype.h>

#include	"radius.h"

extern int      debug_flag;    /* values > 0 allow various debug output */
extern FILE    *ddt;
extern char    *radius_dir;
extern char    *fsm_id;

static FSM_ENT **fsm;          /* pointer to FSM table */
static FSM_ENT **default_fsm;  /* pointer to default action table */
static int      max_aatv;      /* count of known AATVs */
static AATVPTR **aatv_ptrs;  /* pointer to array of pointers to AATV pointers */

static int      add_state PROTO((STATELIST *, char *, u_int, int));
static int      enum_event PROTO((char *));
static int      extra_stuff PROTO((char *, int *, char **));
static NAME_LIST *find_state PROTO((STATELIST *, char *));
static void     rad_fsmdelete PROTO((int));
static int      rad_fsminit PROTO((char*, FSM_ENT ***));

/*
 *	Default Action Table
 */

static char    *default_actions =		/* MUST NOT HAVE FSMID */
{
"Start:\n\
	*.*.DUP			REDO		Same\n\
	*.*.FATAL		FATAL		End\n\
	*.*.TIMEOUT		TIMEOUT		End\n\
	*.*.ABORT		NULL		End\n\
End:"
};

#ifdef	MERIT_LAS
/*
 *	Standard State Machine Table when there is no FSM configuration file
 */

static char    *standard_fsm =
{
"%FSMID STD_LAS\n\
Start:\n\
	*.+AUTHEN.ACK		AUTHENTICATE	AUTHwait\n\
	*.+AUTH_ONLY.ACK	AUTHENTICATE	AUTHwait\n\
	*.+AUTHENTICATE.ACK	AUTHENTICATE	AUTHwait\n\
	*.+ACCT.ACK		ACCT		ACCTwait\n\
	*.+MGT_POLL.ACK		SRV_STATUS	MGTwait\n\
	*.*.NAK			REPLY		End\n\
\n\
AUTHwait:\n\
	Start.AUTHENTICATE.ACK	REPLY		Hold\n\
	Start.AUTHENTICATE.NAK  REPLY		Hold\n\
\n\
ACCTwait:\n\
	Start.ACCT.ACK		REPLY		Hold\n\
\n\
MGTwait:\n\
	Start.SRV_STATUS.ACK	REPLY		Hold\n\
\n\
Hold:\n\
	*.*.TIMEOUT		NULL		End\n\
\n\
End:"
};

#else	/* MERIT_LAS */
/*
 *	Standard State Machine Table when there is no FSM configuration file
 */

static char    *standard_fsm =
{
"%FSMID STD\n\
Start:\n\
	*.+AUTHEN.ACK		AUTHENTICATE	AUTHwait\n\
	*.+AUTH_ONLY.ACK	AUTHENTICATE	AUTHwait\n\
	*.+ACCT.ACK		ACCT		ACCTwait\n\
	*.+MGT_POLL.ACK		SRV_STATUS	MGTwait\n\
	*.*.NAK			REPLY		End\n\
\n\
AUTHwait:\n\
	Start.AUTHENTICATE.ACK	REPLY		Hold\n\
	Start.AUTHENTICATE.NAK  REPLY		Hold\n\
\n\
ACCTwait:\n\
	Start.ACCT.ACK		REPLY		Hold\n\
\n\
MGTwait:\n\
	Start.SRV_STATUS.ACK	REPLY		Hold\n\
\n\
Hold:\n\
	*.*.TIMEOUT		NULL		End\n\
\n\
End:"
};
#endif	/* MERIT_LAS */

/*************************************************************************
 *
 *	Function: add_state
 *
 *	Purpose: Add a state name and flag to the list.
 *		 Assign the next available state number to it.
 *
 *	Returns: The (new) next available state number to use.
 *
 *************************************************************************/

static int
add_state (list, name, flag, num)

STATELIST      *list;		/* list of states names */
char           *name;		/* name of the new state */
u_int           flag;		/* flag (seen/defined) for the new state */
int             num;		/* next available state number to use */

{
	NAME_LIST      *node;
	static char    *func = "add_state";

	dprintf(4, (LOG_DAEMON, LOG_DEBUG, "%s: entered", func));

	if (num >= ST_RESERVED)
	{
		logit (LOG_DAEMON, LOG_DEBUG, "%s: FSM size (%d) too large\n",
			func, num);
		exit (-14);
	}

	/* make sure we'll have enough space in the FSM array */
	if (list->nst >= list->maxst)
	{
		dprintf(1, (LOG_DAEMON, LOG_DEBUG,
			"%s: growing FSM size by 8", func));

		list->maxst += 8;
		fsm = (FSM_ENT **) realloc (fsm,
					    (list->maxst) * sizeof (FSM_ENT *));
	}

	/* append new state to states list */
	if ((node = (NAME_LIST *) malloc (sizeof (NAME_LIST)))
							== (NAME_LIST *) NULL)
	{
		logit (LOG_DAEMON, LOG_ALERT, "%s: FATAL out of memory", func);
		abort ();
	}

	node->name = add_string (name, ASIS);
	node->num = num;
	node->flag = (u_char) flag;
	node->next = list->states;
	list->states = node;
	list->nst++;
	dprintf(4, (LOG_DAEMON, LOG_DEBUG,
		"added %s as state %d with flag %s", name, num,
		((u_char) flag == ST_SEEN) ? "ST_SEEN" : "ST_DEFINED"));
	num++;
	return num;
} /* end of add_state () */

/*************************************************************************
 *
 *	Function: enum_event
 *
 *	Purpose: Enumerate a event (from string to number).
 *		 Note: These numbers are taken from #defines in radius.h
 *
 *	Returns: <number> of the event,
 *		 -2 if the event is not valid.
 *
 *************************************************************************/

static int
enum_event (pevent)

char           *pevent;

{
	int             i;
	int             max;
	static char    *events[] =
			{
				EN_NAK,			/* -1 */
				EN_ACK,			/* 0 */
				EN_ERROR,		/* 1 */
				EN_WAIT,		/* 2 */
				EN_FATAL,		/* 3 */
				EN_DUP_REQ,		/* 4 */
				EN_TIMER,		/* 5 */
				EN_TIMEOUT,		/* 6 */
				EN_ABORT,		/* 7 */
				EN_NEW_AUTHEN,		/* 8 */
				EN_NEW_ACCT,		/* 9 */
				EN_NEW_PASSWD,		/* 10 */
				EN_RE_ACCESS,		/* 11 */
				EN_ACC_CHAL,		/* 12 */
				EN_MGT_POLL,		/* 13 */
				EN_AUTH_ONLY,		/* 14 */
				EN_ACCT_START,		/* 19 */
				EN_ACCT_STOP,		/* 20 */
				EN_ACCT_ALIVE,		/* 21 */
				EN_ACCT_MODEM_START,	/* 22 */
				EN_ACCT_MODEM_STOP,	/* 23 */
				EN_ACCT_CANCEL,		/* 24 */
				EN_RC1,			/* 8 */
				EN_RC2,			/* 9 */
				EN_RC3,			/* 10 */
				EN_RC4,			/* 11 */
				EN_RC5,			/* 12 */
				EN_RC6,			/* 13 */
				EN_RC7,			/* 14 */
				EN_RC8,			/* 15 */
				EN_RC9,			/* 16 */
				EN_RC10,		/* 17 */
				EN_RC11,		/* 18 */
				EN_RC12,		/* 19 */
				EN_RC13,		/* 20 */
				EN_RC14,		/* 21 */
				EN_RC15,		/* 22 */
				EN_RC16,		/* 23 */
				EN_RC17,		/* 24 */
				EN_RC18,		/* 25 */
				EN_RC19,		/* 26 */
				EN_RC20,		/* 27 */
				EN_RC21,		/* 28 */
				EN_HGAS1,		/* 15 */
				EN_HGAS2,		/* 16 */
				EN_HGAS3,		/* 17 */
				EN_BACCT,		/* 18 */
				EN_OAS,			/* 25 */
				EN_OAS_ACCT,		/* 26 */
				EN_LAS,			/* 27 */
				EN_LAS_ACCT		/* 28 */
			};
	static int      event_number[] =
			{
				-1,	/* EN_NAK */
				0,	/* EN_ACK */
				1,	/* EN_ERROR */
				2,	/* EN_WAIT */
				3,	/* EN_FATAL */
				4,	/* EN_DUP_REQ */
				5,	/* EN_TIMER */
				6,	/* EN_TIMEOUT */
				7,	/* EN_ABORT */
				8,	/* EN_NEW_AUTHEN */
				9,	/* EN_NEW_ACCT */
				10,	/* EN_NEW_PASSWD */
				11,	/* EN_RE_ACCESS */
				12,	/* EN_ACC_CHAL */
				13,	/* EN_MGT_POLL */
				14,	/* EN_AUTH_ONLY */
				19,	/* EN_ACCT_START */
				20,	/* EN_ACCT_STOP */
				21,	/* EN_ACCT_ALIVE */
				22,	/* EN_ACCT_MODEM_START */
				23,	/* EN_ACCT_MODEM_STOP */
				24,	/* EN_ACCT_CANCEL */
				8,	/* EN_RC1 */
				9,	/* EN_RC2 */
				10,	/* EN_RC3 */
				11,	/* EN_RC4 */
				12,	/* EN_RC5 */
				13,	/* EN_RC6 */
				14,	/* EN_RC7 */
				15,	/* EN_RC8 */
				16,	/* EN_RC9 */
				17,	/* EN_RC10 */
				18,	/* EN_RC11 */
				19,	/* EN_RC12 */
				20,	/* EN_RC13 */
				21,	/* EN_RC14 */
				22,	/* EN_RC15 */
				23,	/* EN_RC16 */
				24,	/* EN_RC17 */
				25,	/* EN_RC18 */
				26,	/* EN_RC19 */
				27,	/* EN_RC20 */
				28,	/* EN_RC21 */
				15,	/* EN_HGAS1 */
				16,	/* EN_HGAS2 */
				17,	/* EN_HGAS3 */
				18,	/* EN_BACCT */
				25,	/* EN_OAS */
				26,	/* EN_OAS_ACCT */
				27,	/* EN_LAS */
				28	/* EN_LAS_ACCT */
			};
	static char    *func = "enum_event";

	dprintf(4, (LOG_DAEMON, LOG_DEBUG, "%s: entered", func));

	max = (sizeof (events) / sizeof (events[0]));
	for (i = 0; i < max; i++)
	{
		if (strcmp (events[i], pevent) == 0)
		{
			return event_number[i];
		}
	}
	return (-2);
} /* end of enum_event () */

/*************************************************************************
 *
 *	Function: extra_stuff
 *
 *	Purpose: Evaluate an extra value pair (or two) for this FSM entry.
 *
 *	Returns: 0, if successful,
 *		 -1, if there were errors.
 *
 *************************************************************************/

static int
extra_stuff (buf, value, string)

char           *buf;
int            *value;
char          **string;

{
	int             result;
	VALUE_PAIR     *list;
	VALUE_PAIR     *vp;
	static char    *func = "extra_stuff";

	dprintf(4, (LOG_DAEMON, LOG_DEBUG, "%s: entered", func));

	result = 0;
	*value = 0;
	*string = '\0';
	vp = (VALUE_PAIR *) NULL;
	list = (VALUE_PAIR *) NULL;

	if (pair_parse (buf, &list) != 0)
	{
		dprintf(4, (LOG_DAEMON, LOG_DEBUG, "%s: error parsing '%s'",
			func, buf));
		result = -1;
	}
	else
	{
		vp = list;
		while (vp != (VALUE_PAIR *) NULL)
		{
			switch (vp->type)
			{
			    case PW_TYPE_INTEGER:
				*value = vp->lvalue;
				break;

			    case PW_TYPE_STRING:
				if (strlen (vp->strvalue) < AUTH_ID_LEN)
				{
				    *string = add_string (vp->strvalue, ASIS);
				}
				break;

			    default:
				dprintf(4, (LOG_DAEMON, LOG_DEBUG,
					"%s: unsupported type %d", func,
					vp->type));
				result = -1;
				break;
			}
			vp = vp->next;
		}
	}
	list_free (list);

	return result;
} /* end of extra_stuff () */

/*************************************************************************
 *
 *	Function: find_aatv
 *
 *	Purpose: Find the AATV with the given id in the AATV array.
 *
 *	Returns: pointer to the given AATV,
 *		 NULL pointer if no match found.
 *
 *************************************************************************/

AATV *
find_aatv (paction)

char           *paction;		/* the AATV id to search for */

{
	int             i;
	AATV           *aatv;
	AATV           *result;
	static char    *func = "find_aatv";

	dprintf(4, (LOG_DAEMON, LOG_DEBUG, "%s: entered, looking for '%s'",
		func, paction));

	result = (AATV *) NULL;

	for (i = 0; i < max_aatv; i++)
	{
		if ((aatv = *aatv_ptrs[i]) != NULL)
		{
			/* dprintf(4, (LOG_DAEMON, LOG_DEBUG,
			      "%s: aatv[%d]->id = '%s'", func, i, aatv->id)); */
			if (strcmp (paction, (char *) aatv->id) == 0)
			{
				result = aatv;
				break;
			}
		}
	}

	return result;
} /* end of find_aatv () */

/*************************************************************************
 *
 *	Function: find_state
 *
 *	Purpose: Find the numer of the given state in the list of states.
 *
 *	Returns: pointer to the NAME_LIST node for this state,
 *		 (NAME_LIST *) NULL, if the name is not found in the list.
 *
 *************************************************************************/

static NAME_LIST *
find_state (list, name)

STATELIST      *list;		/* list of states names already seen */
char           *name;		/* the state name to search for */

{
	NAME_LIST      *state;
	static char    *func = "find_state";

	dprintf(4, (LOG_DAEMON, LOG_DEBUG, "%s: entered", func));

	for (state = list->states ;
		state != (NAME_LIST *) NULL ;
		state = state->next)
	{
		if (strcmp (state->name, name) == 0)
		{
			break;
		}
	}

	return state;
} /* end of find_state () */

/*************************************************************************
 *
 *	Function: init_fsm
 *
 *	Purpose: Does all the initialization for FSM.
 *
 *	Returns: <number of states>, if the (new) FSM is initialized properly,
 *		 -1, if any errors are detected.
 *
 *************************************************************************/

int
init_fsm (n, aatvs, cnt, fsm_file, main_fsm, def_fsm)

int             n;                /* INPUT: current number of FSM states */
AATVPTR       **aatvs;            /* INPUT: pointer to array of AATV pointers */
int             cnt;              /* INPUT: number of elements in above array */
char           *fsm_file;         /* INPUT: pointer to FSM config file name */
FSM_ENT      ***main_fsm;         /* OUTPUT: pointer to FSM table */
FSM_ENT      ***def_fsm;          /* OUTPUT: pointer to default action table */

{
	int             len;
	int             nfsm;
	FILE           *fp;
	char           *fsmfile;
	struct stat     statbuf;
	char            buf[MAXPATHLEN];
	static char    *func = "init_fsm";

	dprintf(2, (LOG_DAEMON, LOG_DEBUG, "%s: entered", func));

	if (n > 0)
	{
		rad_fsmdelete (n);
	}

	sprintf (buf, "%s/%s", radius_dir, fsm_file);
	if ((fp = fopen (buf, "r")) == (FILE *) NULL)
	{
		dprintf(2, (LOG_DAEMON, LOG_DEBUG,
			"using built-in standard FSM table"));
		fsmfile = standard_fsm;
		strcpy (buf, "built-in standard FSM table");
	}
	else
	{
		stat (buf, &statbuf);
		len = (long) statbuf.st_size;
		if ((fsmfile = (char *) malloc (len)) == (char *) NULL)
		{
			logit (LOG_DAEMON, LOG_ALERT,
				"%s: FATAL out of memory", func);
			abort ();
		}
		fread (fsmfile, len, 1, fp);
	}

	aatv_ptrs = aatvs;
	max_aatv = cnt;
	nfsm = rad_fsminit (fsmfile, &fsm);

	if (fsmfile != standard_fsm)
	{
		free (fsmfile);
	}

	dprintf(1, (LOG_DAEMON, LOG_DEBUG,
		"%s: FSM defined with %d states from %s", func, nfsm, buf));

	if (nfsm == -2)
	{
		return (-1);
	}

	/* ... and the default action table */

	if (default_fsm == (FSM_ENT **) NULL)
	{
		len = rad_fsminit (default_actions, &default_fsm);

		if (len == -2)
		{
			dprintf(1, (LOG_DAEMON, LOG_DEBUG,
				"%s: return code %d from rad_fsminit",
				func, len));
			return (-1);
		}
	}

	*main_fsm = fsm;
	*def_fsm = default_fsm;
	return nfsm;
} /* end of init_fsm () */

/*************************************************************************
 *
 *	Function: rad_fsmdelete
 *
 *	Purpose: Destroy the finite state machine (FSM) data structure.
 *
 *************************************************************************/

static void
rad_fsmdelete (nfsm)

int             nfsm;

{
	FSM_ENT        *pe;
	FSM_ENT        *pen;
	static char    *func = "rad_fsmdelete";

	dprintf(2, (LOG_DAEMON, LOG_DEBUG, "%s: entered", func));

	while (nfsm > 0) /* free the individual FSM state entries */
	{
		nfsm--;
		for (pe = fsm[nfsm]; pe != (FSM_ENT *) NULL; pe = pen)
		{
			pen = pe->next;
			free (pe);
		}
	}

	free (fsm); /* free the FSM array itself */

	return;
} /* end of rad_fsmdelete () */

/*************************************************************************
 *
 *	Function: rad_fsminit
 *
 *	Purpose: Initialize the FSM table in memory from the given file.
 *
 *	Returns:  number of states parsed, if successful,
 *		 -2, if errors occurred.
 *
 *************************************************************************/

static int
rad_fsminit (fsmfile, fsm)

char           *fsmfile;
FSM_ENT      ***fsm;

{
	int             i;		/* loop variable */
	int             more;		/* while loop control variable */
	int             nfsm;		/* state counter */
	int             line_cnt;	/* config file line count */
	int             err_cnt;	/* used in FSM verification */
	int             ref_cnt;	/* used in FSM verification */
	int             cst;		/* current state number */
	int             xvalue;
	FILE           *debugout = stdout;
	char           *ptr;		/* pointer into given file */
	char           *newline;	/* pointer into given file */
	char           *p;		/* pointer into line buffer */
	char           *s;		/* pointer into line buffer */
	char           *b;		/* pointer into line buffer */
	char           *e;		/* pointer into line buffer */
	char           *a;		/* pointer into line buffer */
	char           *n;		/* pointer into line buffer */
	char           *xtra;		/* pointer into line buffer */
	FSM_ENT        *pe;		/* pointer to an entry */
	FSM_ENT        *pn;		/* temporary pointer */
	FSM_ENT       **pfsm;		/* new FSM */
	NAME_LIST      *node;		/* pointer to a state NAME_LIST node */
	NAME_LIST     **prev;		/* used to free "sts" states below */
	STATELIST       sts;		/* state list data structure */
	char           *xstring;
	char            buf[256];	/* line buffer for file reading */
	static char    *func = "rad_fsminit";

	dprintf(2, (LOG_DAEMON, LOG_DEBUG, "%s: entered", func));

	/* allocate new FSM */
	sts.nst = 0;
	sts.maxst = NUMSTATES;
	sts.states = (NAME_LIST *) NULL;
	if ((pfsm = (FSM_ENT **) calloc (sts.maxst, sizeof (FSM_ENT *)))
							== (FSM_ENT **) NULL)
	{
		logit (LOG_DAEMON, LOG_ALERT, "%s: FATAL out of memory for FSM",
			func);
		abort ();
	}

	line_cnt = 0;		/* line number for debugging */
	nfsm = 0;		/* have seen no state yet */
	more = 1;
	ptr = fsmfile;

	while (more)
	{
		if ((newline = strchr (ptr, '\n')) == (char *) NULL)
		{
			more = 0;
			break;
		}
		else
		{
			i = newline - ptr;
			strncpy (buf, ptr, i);
			buf[i] = '\0';
			ptr = ++newline;
		}

		line_cnt++;

		if (*buf == COMMENT || *buf == '\n' || *buf == '\0')
		{
			continue;
		}
		else
		{
			if (*buf == '%')
			{
				if ((p = strtok (buf, " \t")) != (char *) NULL)
				{
					/* Have found a keyword */
					if (strcasecmp (p, "%FSMID") == 0)
					{
						/* Find end of line. */
						if ((p = strtok (NULL, "\n"))
							       != (char *) NULL)
						{
							int j;

							if (fsm_id
							       != (char *) NULL)
							{
								free (fsm_id);
							}

							j = strlen (p);
							if (j > MAX_FSMID_LEN)
							{
							   /* Truncate. */
							   p[MAX_FSMID_LEN]
									= '\0';
							}
							fsm_id = strdup (p);
						}
					}
				}
				/* Just treat FSMID as a comment. */
				continue;
			}
		}

		while (i > 0) /* capitalize all tokens on line */
		{
			i--;
			buf[i] = toupper (buf[i]);
		}

		xvalue = 0;
		xstring = (char *) NULL;

		if (isalpha (*buf)) /* states start in column one */
		{
			if ((p = strtok (buf, ":")) != NULL) /* have state */
			{
				if (strcmp (p, "END") == 0)
				{
					dprintf(4, (LOG_DAEMON, LOG_DEBUG,
						"%s state detected", p));
					more = 0;
				}
				else		/* insert new state */
				{
					dprintf(4, (LOG_DAEMON, LOG_DEBUG,
					       "about to insert new state %s",
						p));
					if ((node = find_state (&sts, p))
							== (NAME_LIST *) NULL)
					{
						cst = nfsm;
						nfsm = add_state (&sts, p,
						       (u_int) ST_DEFINED, cst);
					}
					else /* have seen this state already */
					{
						if (node->flag == ST_SEEN)
						{
							cst = node->num;
							node->flag = ST_DEFINED;
						}
						else /* oops, was defined */
						{
							logit (LOG_DAEMON,
								LOG_ERR,
						"%s: duplicate state: line %3d",
								func, line_cnt);
							logit (LOG_DAEMON,
								LOG_ERR,
								"'%s'", buf);
							return (-2);
						}
					}
				}
			}
			else
			{
				logit (LOG_DAEMON, LOG_ERR,
					"%s: bad state: line %3d",
					func, line_cnt);
				logit (LOG_DAEMON, LOG_ERR,
					"'%s'", buf); /* Log the parse error. */
				return (-2);
			}
		}
		else /* grab (state.action.event, action, nextstate) 3-tuple */
		{
			if (nfsm == 0)	/* do we have some/any state(s)? */
			{
				logit (LOG_DAEMON, LOG_ERR,
					"%s: no states: line %3d",
					func, line_cnt);
				logit (LOG_DAEMON, LOG_ERR,
					"'%s'", buf); /* Log the parse error. */
				return (-2);
			}

			if ((s = strtok (buf, " \t.")) == NULL) /* state ? */
			{
				logit (LOG_DAEMON, LOG_ERR,
					"%s: error parse state: line %3d",
					func, line_cnt);
				logit (LOG_DAEMON, LOG_ERR,
					"'%s'", buf); /* Log the parse error. */
				return (-2);
			}

			if ((b = strtok (NULL, " \t.")) == NULL) /* action ? */
			{
				logit (LOG_DAEMON, LOG_ERR,
					"%s: error parse old action: line %3d",
					func, line_cnt);
				logit (LOG_DAEMON, LOG_ERR,
					"'%s'", buf); /* Log the parse error. */
				return (-2);
			}

			if ((e = strtok (NULL, " \t\n\r")) == NULL) /* event? */
			{
				logit (LOG_DAEMON, LOG_ERR,
					"%s: error parse event: line %3d",
					func, line_cnt);
				logit (LOG_DAEMON, LOG_ERR,
					"'%s'", buf); /* Log the parse error. */
				return (-2);
			}

			if ((a = strtok (NULL, " \t\n\r")) == NULL) /* action?*/
			{
				logit (LOG_DAEMON, LOG_ERR,
					"%s: error parse action: line %3d",
					func, line_cnt);
				logit (LOG_DAEMON, LOG_ERR,
					"'%s'", buf); /* Log the parse error. */
				return (-2);
			}

			if ((n = strtok (NULL, " \t\n\r")) == NULL) /* next ? */
			{
				logit (LOG_DAEMON, LOG_ERR,
					"%s: error parse next state: line %3d",
					func, line_cnt);
				logit (LOG_DAEMON, LOG_ERR,
					"'%s'", buf); /* Log the parse error. */
				return (-2);
			}

			if ((xtra = strtok (NULL, " \t\n\r")) != NULL) /*extra*/
			{
				if (extra_stuff (xtra, &xvalue, &xstring) < 0)
				{
					logit (LOG_DAEMON, LOG_ERR,
					 "%s: error with extra stuff: line %3d",
						func, line_cnt);
					logit (LOG_DAEMON, LOG_ERR,
						"'%s'", buf); /* Log stuff. */
					return (-2);
				}
			}

			if ((pe = (FSM_ENT *) malloc (sizeof (FSM_ENT)))
							== (FSM_ENT *) NULL)
			{
				logit (LOG_DAEMON, LOG_ALERT,
					"%s: FATAL out of memory for FSM",
					func);
				abort ();
			}

			dprintf(4, (LOG_DAEMON, LOG_DEBUG,
				"have 3-tuple '%s.%s.%s'", s, b, e));
			dprintf(4, (LOG_DAEMON, LOG_DEBUG,
				"   action '%s', next state '%s'", a, n));
			dprintf(4, (LOG_DAEMON, LOG_DEBUG,
				"   value %d, and string '%s'", xvalue,
				(xstring == (char *) NULL) ? xstring : ""));

			pe->xvalue = xvalue;
			pe->xstring = xstring;

			/* handle state part of (state.action.event) 3-tuple */
			dprintf(4, (LOG_DAEMON, LOG_DEBUG,
				"finding event state '%s' with number:", s));
			if (strcmp (s, "*") == 0)
			{
				pe->event.state = ST_ANY;
				dprintf(4, (LOG_DAEMON, LOG_DEBUG,
					"     %u", ST_ANY));
			}
			else
			{
				if ((node = find_state (&sts, s))
							== (NAME_LIST *) NULL)
				{
					i = nfsm; /* pick next state */
					nfsm = add_state (&sts, s,
							  (u_int) ST_SEEN, i);
					pe->event.state = (u_char) i;
					dprintf(4, (LOG_DAEMON, LOG_DEBUG,
						"     %u and flag %s", i,
						"ST_SEEN"));
				}
				else /* have seen already */
				{
					pe->event.state = (u_char) node->num;
					dprintf(4, (LOG_DAEMON, LOG_DEBUG,
						"     %u and flag %s",
						node->num,
						(node->flag == ST_SEEN)
						? "ST_SEEN" : "ST_DEFINED"));
				}
			}

			/* handle action part of (state.action.event) 3-tuple */
			if (b[0] == '+')
			{
				pe->event.a.proxy = add_string (&b[1], ASIS);
				pe->event.isproxy = 1;
			}
			else
			{
				pe->event.isproxy = 0;
				if ((pe->event.a.aatv = find_aatv (b))
							       == (AATV *) NULL)
				{
					/* Log invalid previous action. */
					logit (LOG_DAEMON, LOG_ERR,
				     "%s: invalid old AATV name: '%s' line %3d",
						func, b, line_cnt);
					return (-2);
				}
			}

			/* handle event part of (state.action.event) 3-tuple */
			if ((pe->event.value = enum_event (e)) == -2)
			{
				/* Log invalid event. */
				logit (LOG_DAEMON, LOG_ERR,
					"%s: invalid event name: '%s' line %3d",
					func, e, line_cnt);
				return (-2);
			}

			if ((pe->action = find_aatv (a)) == (AATV *) NULL)
			{
				/* Log invalid action. */
				logit (LOG_DAEMON, LOG_ERR,
				       "%s: invalid action name: '%s' line %3d",
					func, a, line_cnt);
				return (-2);
			}

			dprintf(4, (LOG_DAEMON, LOG_DEBUG,
				"looking for next_state %s with number:", n));
			if (strcmp (n, "END") == 0)
			{
				pe->next_state = ST_END;
				dprintf(4, (LOG_DAEMON, LOG_DEBUG,
					"     %d", ST_END));
			}
			else
			{
				if (strcmp (n, "SAME") == 0)
				{
					pe->next_state = ST_SAME;
					dprintf(4, (LOG_DAEMON, LOG_DEBUG,
						"     %d", ST_SAME));
				}
				else
				{
					if ((node = find_state (&sts, n))
							== (NAME_LIST *) NULL)
					{
						i = nfsm; /* pick next state */
						nfsm = add_state (&sts, n,
							    (u_int) ST_SEEN, i);
						pe->next_state = (u_char) i;
						dprintf(4, (LOG_DAEMON,
							LOG_DEBUG,
							"     %u and flag %s",
							i, "ST_SEEN"));
					}
					else /* have seen already */
					{
						pe->next_state =
							(u_char) node->num;
						dprintf(4, (LOG_DAEMON,
							LOG_DEBUG,
							"     %u and flag %s",
							node->num,
							(node->flag == ST_SEEN)
							? "ST_SEEN"
							: "ST_DEFINED"));
					}
				}
			}

			pe->next = (FSM_ENT *) NULL;

			/* link *pe to the FSM table */
			if ((pn = pfsm[cst]) == (FSM_ENT *) NULL)
			{
				pfsm[cst] = pe;
			}
			else
			{
				while (pn->next)
				{
					pn = pn->next;
				}
				pn->next = pe;
			}
			dprintf(4, (LOG_DAEMON, LOG_DEBUG,
				"entry added to state %d:", cst));
			dprintf(4, (LOG_DAEMON, LOG_DEBUG,
				"     event [%d,%s,%d]", pe->event.state,
				(pe->event.isproxy == 1) ? pe->event.a.proxy :
					(char *) pe->event.a.aatv->id,
				pe->event.value));
			dprintf(4, (LOG_DAEMON, LOG_DEBUG,
				"     action %s", pe->action->id));
			dprintf(4, (LOG_DAEMON, LOG_DEBUG,
				"     next_state %d", pe->next_state));
			dprintf(4, (LOG_DAEMON, LOG_DEBUG,
				"     value %d", pe->xvalue));
			dprintf(4, (LOG_DAEMON, LOG_DEBUG,
				"     string '%s'",
					pe->xstring ? pe->xstring : ""));
		} /* end of else not isalpha() */
	} /* end of while (more) */

	/*
	 * now check if the state machine is valid
	 * 	-	See if there is any state (except Start)
	 *		not-reachable from any other states;
	 * 	-	See if there is any state which is
	 *		not defined.
	 *	-	Print out the whole state machine in debug
	 *		mode for inspection.
	 */
	err_cnt = 0; /* indicate assumption about clean FSM table */
	for (i = 1; i < nfsm; i++) /* for each state */
	{
		ref_cnt = 0; /* reference counter for state "i" */
		for (cst = 0; cst < nfsm; cst++)
		{
			for (pe = pfsm[cst];
				pe != (FSM_ENT *) NULL;
				pe = pe->next)
			{
				if (i == pe->next_state)
				{
					ref_cnt++;
				}
			}
		}

		/* error if no FSM entry refers to state "i" */
		if (ref_cnt == 0)
		{
			logit (LOG_DAEMON, LOG_ERR,
				"%s: non-reachable state %d\n", func, i);
			err_cnt = i;
		}
	} /* end of for each state */

	if (debug_flag >= 2)
	{
		if (ddt)
		{
			debugout = ddt;
		}

		fprintf (debugout, "\nState  Event                 Action        Next State Value String\n");
		fprintf (debugout, "-----  --------------------  ------------  ---------- ----- ------\n\n");
		fprintf (debugout, "\n");
	}

	for (i = 0; i < nfsm; i++) /* for each state (again) */
	{
		for (pe = pfsm[i]; pe != (FSM_ENT *) NULL; pe = pe->next)
		{
			if ((pe->next_state >= nfsm) &&
				(pe->next_state < ST_RESERVED))
			{
				logit (LOG_DAEMON, LOG_ERR,
					"%s: non-existant state %d\n", func,
					pe->next_state);
				err_cnt++;
			}

			if (debug_flag >= 2)
			{
				fprintf (debugout,
			  "%3d   [%3d,%-13s,%2d] %-12s  %5d       %3d  '%s'\n",
					i, pe->event.state,
					(pe->event.isproxy == 1) ?
						pe->event.a.proxy :
						(char *) pe->event.a.aatv->id,
					pe->event.value, pe->action->id,
					pe->next_state, pe->xvalue,
					pe->xstring ? pe->xstring : "");
			}
		}

		if (debug_flag >= 2)
		{
			fprintf (debugout, "\n");
		}
	} /* end of for each state (again) */

	if (debug_flag >= 2)
	{
		fprintf (debugout, "\n");
	}

	*fsm = pfsm;

	/* free the states list */
	if (debug_flag >= 2)
	{
		fprintf (debugout, "State  State Name     Number Flag\n");
		fprintf (debugout, "-----  -------------- ------ ----\n\n");
	}

	i = 0;
	while (sts.states != (NAME_LIST *) NULL)
	{
		prev = &sts.states;
		for (node = sts.states ;
			node != (NAME_LIST *) NULL ;
			node = node->next)
		{
			if (node->next == (NAME_LIST *) NULL)
			{
				break;
			}
			prev = &(node->next);
		}

		if (debug_flag >= 2)
		{
			fprintf (debugout, " %2d    %-14s %3d    %s", i, node->name,
			    node->num,
			    (node->flag == ST_SEEN) ? "ST_SEEN" : "ST_DEFINED");
		}

		if (node->flag == ST_SEEN)
		{
			if (debug_flag >= 2)
			{
				fprintf (debugout, " <--- this is an error\n");
			}
			logit (LOG_DAEMON, LOG_ERR,
				"%s: state %s seen but not defined",
				func, node->name);
			nfsm = -2;
		}
		else
		{
			if (debug_flag >= 2)
			{
				fprintf (debugout, "\n");
			}
		}

		free (node);
		*prev = (NAME_LIST *) NULL;
		i++;
	}

	if (err_cnt > 0)
	{
		return (-2);
	}

	if (debug_flag >= 2)
	{
		fprintf (debugout, "\n");
	}

	return nfsm;
} /* end of rad_fsminit () */
