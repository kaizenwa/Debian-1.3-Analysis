/************************************************************************
 *   IRC - Internet Relay Chat, ircd/s_user.c (formerly ircd/s_msg.c)
 *   Copyright (C) 1990 Jarkko Oikarinen and
 *                      University of Oulu, Computing Center
 *
 *   See file AUTHORS in IRC package for additional names of
 *   the programmers. 
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 1, or (at your option)
 *   any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifndef lint
static  char sccsid[] = "@(#)s_user.c	2.74 2/8/94 (C) 1988 University of Oulu, \
Computing Center and Jarkko Oikarinen";
#endif

#include "struct.h"
#include "common.h"
#include "sys.h"
#include "numeric.h"
#include "msg.h"
#include "channel.h"
#include "userload.h"
#include <sys/stat.h>
#include <utmp.h>
#include <fcntl.h>
#include "h.h"

void	send_umode_out PROTO((aClient*, aClient *, int));
void	send_umode PROTO((aClient *, aClient *, int, int, char *));
int	is_silenced PROTO((aClient *, aClient *));

static char buf[BUFSIZE], buf2[BUFSIZE];

/*
** m_functions execute protocol messages on this server:
**
**	cptr	is always NON-NULL, pointing to a *LOCAL* client
**		structure (with an open socket connected!). This
**		identifies the physical socket where the message
**		originated (or which caused the m_function to be
**		executed--some m_functions may call others...).
**
**	sptr	is the source of the message, defined by the
**		prefix part of the message if present. If not
**		or prefix not found, then sptr==cptr.
**
**		(!IsServer(cptr)) => (cptr == sptr), because
**		prefixes are taken *only* from servers...
**
**		(IsServer(cptr))
**			(sptr == cptr) => the message didn't
**			have the prefix.
**
**			(sptr != cptr && IsServer(sptr) means
**			the prefix specified servername. (?)
**
**			(sptr != cptr && !IsServer(sptr) means
**			that message originated from a remote
**			user (not local).
**
**		combining
**
**		(!IsServer(sptr)) means that, sptr can safely
**		taken as defining the target structure of the
**		message in this server.
**
**	*Always* true (if 'parse' and others are working correct):
**
**	1)	sptr->from == cptr  (note: cptr->from == cptr)
**
**	2)	MyConnect(sptr) <=> sptr == cptr (e.g. sptr
**		*cannot* be a local connection, unless it's
**		actually cptr!). [MyConnect(x) should probably
**		be defined as (x == x->from) --msa ]
**
**	parc	number of variable parameter strings (if zero,
**		parv is allowed to be NULL)
**
**	parv	a NULL terminated list of parameter pointers,
**
**			parv[0], sender (prefix string), if not present
**				this points to an empty string.
**			parv[1]...parv[parc-1]
**				pointers to additional parameters
**			parv[parc] == NULL, *always*
**
**		note:	it is guaranteed that parv[0]..parv[parc-1] are all
**			non-NULL pointers.
*/

/*
** next_client
**	Local function to find the next matching client. The search
**	can be continued from the specified client entry. Normal
**	usage loop is:
**
**	for (x = client; x = next_client(x,mask); x = x->next)
**		HandleMatchingClient;
**	      
*/
aClient *next_client(next, ch)
Reg1	aClient *next;	/* First client to check */
Reg2	char	*ch;	/* search string (may include wilds) */
{
	Reg3	aClient	*tmp = next;

	next = find_client(ch, tmp);
	if (tmp && tmp->prev == next)
		return NULL;
	if (next != tmp)
		return next;
	for ( ; next; next = next->next)
	    {
		if (IsService(next))
			continue;
		if (!match(ch, next->name) || !matches(next->name, ch))
			break;
	    }
	return next;
}

/*
** hunt_server
**
**	Do the basic thing in delivering the message (command)
**	across the relays to the specific server (server) for
**	actions.
**
**	Note:	The command is a format string and *MUST* be
**		of prefixed style (e.g. ":%s COMMAND %s ...").
**		Command can have only max 8 parameters.
**
**	server	parv[server] is the parameter identifying the
**		target server.
**
**	*WARNING*
**		parv[server] is replaced with the pointer to the
**		real servername from the matched client (I'm lazy
**		now --msa).
**
**	returns: (see #defines)
*/
int	hunt_server(cptr, sptr, command, server, parc, parv)
aClient	*cptr, *sptr;
char	*command, *parv[];
int	server, parc;
    {
	aClient *acptr;

	/*
	** Assume it's me, if no server
	*/
	if (parc <= server || BadPtr(parv[server]) ||
	    matches(me.name, parv[server]) == 0 ||
	    matches(parv[server], me.name) == 0)
		return (HUNTED_ISME);
	/*
	** These are to pickup matches that would cause the following
	** message to go in the wrong direction while doing quick fast
	** non-matching lookups.
	*/
	if ((acptr = find_client(parv[server], NULL)))
		if (acptr->from == sptr->from && !MyConnect(acptr))
			acptr = NULL;
	if (!acptr && (acptr = find_server(parv[server], NULL)))
		if (acptr->from == sptr->from && !MyConnect(acptr))
			acptr = NULL;
	if (!acptr)
		for (acptr = client, (void)collapse(parv[server]);
		     (acptr = next_client(acptr, parv[server]));
		     acptr = acptr->next)
		    {
			if (acptr->from == sptr->from && !MyConnect(acptr))
				continue;
			/*
			 * Fix to prevent looping in case the parameter for
			 * some reason happens to match someone from the from
			 * link --jto
			 */
			if (IsRegistered(acptr) && (acptr != cptr))
				break;
		    }
	 if (acptr)
	    {
		if (IsMe(acptr) || MyClient(acptr))
			return HUNTED_ISME;
		if (matches(acptr->name, parv[server]))
			parv[server] = acptr->name;
		sendto_one(acptr, command, parv[0],
			   parv[1], parv[2], parv[3], parv[4],
			   parv[5], parv[6], parv[7], parv[8]);
		return(HUNTED_PASS);
	    } 
	sendto_one(sptr, err_str(ERR_NOSUCHSERVER), me.name,
		   parv[0], parv[server]);
	return(HUNTED_NOSUCH);
    }

/*
** 'do_nick_name' ensures that the given parameter (nick) is
** really a proper string for a nickname (note, the 'nick'
** may be modified in the process...)
**
**	RETURNS the length of the final NICKNAME (0, if
**	nickname is illegal)
**
**  Nickname characters are in range
**	'A'..'}', '_', '-', '0'..'9'
**  anything outside the above set will terminate nickname.
**  In addition, the first character cannot be '-'
**  or a Digit.
**
**  Note:
**	'~'-character should be allowed, but
**	a change should be global, some confusion would
**	result if only few servers allowed it...
*/

static	int do_nick_name(nick)
char	*nick;
{
	Reg1 char *ch;

	if (*nick == '-' || isdigit(*nick)) /* first character in [0..9-] */
		return 0;

	for (ch = nick; *ch && (ch - nick) < NICKLEN; ch++)
		if (!isvalid(*ch) || isspace(*ch))
			break;

	*ch = '\0';

	return (ch - nick);
}


/*
** canonize
**
** reduce a string of duplicate list entries to contain only the unique
** items.  Unavoidably O(n^2).
*/
char	*canonize(buffer)
char	*buffer;
{
	static	char	cbuf[BUFSIZ];
	register char	*s, *t, *cp = cbuf;
	register int	l = 0;
	char	*p = NULL, *p2;

	*cp = '\0';

	for (s = strtoken(&p, buffer, ","); s; s = strtoken(&p, NULL, ","))
	    {
		if (l)
		    {
			for (p2 = NULL, t = strtoken(&p2, cbuf, ","); t;
			     t = strtoken(&p2, NULL, ","))
				if (!mycmp(s, t))
					break;
				else if (p2)
					p2[-1] = ',';
		    }
		else
			t = NULL;
		if (!t)
		    {
			if (l)
				*(cp-1) = ',';
			else
				l = 1;
			(void)strcpy(cp, s);
			if (p)
				cp += (p - s);
		    }
		else if (p2)
			p2[-1] = ',';
	    }
	return cbuf;
}


/*
** check_clones
**	This function counts the number of clients with the
**	same host as cptr that connected less than 20 seconds ago.
** return value
**	number of clients with same hosts (including self)
** -SeKs
*/
#ifdef CHECK_CLONE
static 	int	check_clones(cptr)
aClient *cptr;
{
	struct abacklog {
		struct	in_addr ip;
		time_t	TS;
		struct abacklog *next;
	};
	static  struct abacklog *backlog=NULL;
	register struct abacklog **blscn=&backlog,*blptr;
	register int count=0;

	/* First, ditch old entries */
	while(*blscn!=NULL)
	    {
		if((*blscn)->TS+CHECK_CLONE_PERIOD < now)
		    {
			blptr=*blscn;
			*blscn=blptr->next;
			MyFree((char *)blptr);
		    }
		else
			blscn=&(*blscn)->next;
	    }
	/* Now add new item to the list */
	blptr=(struct abacklog *)MyMalloc(sizeof(struct abacklog));
	blptr->ip.s_addr=cptr->ip.s_addr;
	blptr->TS=now;
	blptr->next=backlog;
	backlog=blptr;

	/* Count the number of entries from the same host */
	blptr=backlog;
	while(blptr!=NULL)
	    {
		if(blptr->ip.s_addr==cptr->ip.s_addr)
			count++;
		blptr=blptr->next;
	    }

	return (count);
}
#endif

/*
** register_user
**	This function is called when both NICK and USER messages
**	have been accepted for the client, in whatever order. Only
**	after this the USER message is propagated.
**
**	NICK's must be propagated at once when received, although
**	it would be better to delay them too until full info is
**	available. Doing it is not so simple though, would have
**	to implement the following:
**
**	1) user telnets in and gives only "NICK foobar" and waits
**	2) another user far away logs in normally with the nick
**	   "foobar" (quite legal, as this server didn't propagate
**	   it).
**	3) now this server gets nick "foobar" from outside, but
**	   has already the same defined locally. Current server
**	   would just issue "KILL foobar" to clean out dups. But,
**	   this is not fair. It should actually request another
**	   nick from local user or kill him/her...
*/

static	int	register_user(cptr, sptr, nick, username)
aClient	*cptr;
aClient	*sptr;
char	*nick, *username;
{
	Reg1	aConfItem *aconf;
        char    *parv[3], *tmpstr, *tmpstr2, c, d;
        short   oldstatus = sptr->status, upper = 0, lower = 0;
        short   pos = 0, leadcaps = 0, other = 0, digits = 0, badid = 0;
        short   digitgroups = 0;
	anUser	*user = sptr->user;
	int	i;
	Dlink	*lp;

	user->last = now;
	parv[0] = sptr->name;
	parv[1] = parv[2] = NULL;

	if (MyConnect(sptr))
	    {
		if ((i = check_client(sptr)))
		    {
			sendto_ops("%s from %s.", i == -3 ?
						  "Too many connections" :
			 			  "Unauthorized connection",
				   get_client_host(sptr));
			ircstp->is_ref++;
			return exit_client(cptr, sptr, &me, i == -3 ?
					     "No more connections" :
					     "No Authorization");
		      } 
		if (IsUnixSocket(sptr))
			strncpyzt(user->host, me.sockhost, HOSTLEN+1);
		else
			strncpyzt(user->host, sptr->sockhost, HOSTLEN+1);
		aconf = sptr->confs->value.aconf;
		if (sptr->flags & FLAGS_DOID && !(sptr->flags & FLAGS_GOTID))
		    {
			/* because username may point to user->username */
			char	temp[USERLEN+1];

			strncpyzt(temp, username, USERLEN+1);
			*user->username = '~';
			(void)strncpy(&user->username[1], temp, USERLEN);
			user->username[USERLEN] = '\0';
			
		    }
		else if (sptr->flags & FLAGS_GOTID)
			strncpyzt(user->username, sptr->username, USERLEN+1);
		else
			strncpyzt(user->username, username, USERLEN+1);

		if (!BadPtr(aconf->passwd) &&
		    !StrEq("ONE", aconf->passwd) &&
		    !StrEq(sptr->passwd, aconf->passwd))
		    {
			ircstp->is_ref++;
			sendto_one(sptr, err_str(ERR_PASSWDMISMATCH),
				   me.name, parv[0]);
			return exit_client(cptr, sptr, &me, "Bad Password");
		    }
		bzero(sptr->passwd, sizeof(sptr->passwd));
		/*
		 * following block for the benefit of time-dependent K:-lines
		 */
		if (find_kill(sptr))
		    {
			ircstp->is_ref++;
			return exit_client(cptr, sptr, &me, "K-lined");
		    }
#ifdef R_LINES
		if (find_restrict(sptr))
		    {
			ircstp->is_ref++;
			return exit_client(cptr, sptr, &me , "R-lined");
		    }
#endif

#ifdef CHECK_CLONE
		/*
		** Ensure that no more than 5 clients from the
		** same host connect within 20 seconds. -SeKs
		*/
		if (check_clones(sptr) > CHECK_CLONE_LIMIT
#ifdef CHECK_CLONE_DELAY
		    && me.since + CHECK_CLONE_DELAY < now
#endif
		){
			sendto_ops("Possible clonebot attack from %s.",
			           get_client_host(sptr));
			ircstp->is_ref++;
			return exit_client(cptr, sptr, &me,
				"Too many connections from same host");
		}
#endif

#ifdef DISALLOW_MIXED_CASE
 /* check for mixed case usernames, meaning probably hacked   Jon2 3-94
    Summary of rules now implemented in this patch:         Ensor 11-94
      In a mixed-case name, if first char is upper, one more upper may
        appear anywhere.  (A mixed-case name *must* have an upper first
        char, and may have one other upper.)
      A third upper may appear if all 3 appear at the beginning of the
        name, separated only by "others" (-/_/.).
      A single group of digits is allowed anywhere.
      Two groups of digits are allowed if at least one of the groups is
        at the beginning or the end.
      Only one '-', '_', or '.' is allowed (or two, if not consecutive).
        But not as the first or last char.
      No other special characters are allowed.
      Name must contain at least one letter.
*/
            tmpstr2 = tmpstr = (username[0] == '~' ? &username[1] : username);
            while (*tmpstr && !badid) {
              pos++;
              c = *tmpstr;
              tmpstr++;
              if (islower(c)) {
                lower++;
              }
              else if (isupper(c)) {
                upper++;
                if ((leadcaps || pos == 1) && !lower && !digits)
                  leadcaps++;
              }
              else if (isdigit(c)) {
                digits++;
                if (pos == 1 || !isdigit(d)) {
                  digitgroups++;
                  if (digitgroups > 2)
                    badid = 1;
                }
              }
              else if (c == '-' || c == '_' || c == '.') {
                other++;
                if (pos == 1)
                  badid = 1;
                else if (d == '-' || d == '_' || d == '.' || other > 2)
                  badid = 1;
              }
              else badid = 1;
              d = c;
            }
            if (!badid) {
              if (lower && upper && (!leadcaps || leadcaps > 3 ||
                 (upper > 2 && upper > leadcaps)))
                badid = 1;
              else if (digitgroups == 2 &&
                 !(isdigit(tmpstr2[0]) || isdigit(c)))
                badid = 1;
              else if ((!lower && !upper) || !isalnum(c))
                badid = 1;
            }
            if (badid && (!(sptr->flags & FLAGS_GOTID) ||
                strcmp(sptr->username, username) != 0)) {
#ifdef NOTIFY_OPS_INVALID_UID
                  sendto_ops("Invalid username: \"%s\" [%s!%s@%s]",
                             username, sptr->name, (sptr->flags & FLAGS_GOTID) ?
                             sptr->username : username, sptr->sockhost);
#endif
                  ircstp->is_ref++;
                  return exit_client(cptr, sptr, &me, "USER: Bad username");
            }
#endif                /* DISALLOW_MIXED_CASE */

		if (oldstatus == STAT_MASTER && MyConnect(sptr))
			(void)m_oper(&me, sptr, 1, parv);
	    }
	else
		strncpyzt(user->username, username, USERLEN+1);
	SetClient(sptr);
	user->clink = add_dlink(&sptr->user->server->serv->client, sptr);
	if (MyConnect(sptr))
	    {
		sendto_one(sptr, rpl_str(RPL_WELCOME), me.name, nick, nick);
		/* This is a duplicate of the NOTICE but see below...*/
		sendto_one(sptr, rpl_str(RPL_YOURHOST), me.name, nick,
			   get_client_name(&me, FALSE), version);
#ifdef	IRCII_KLUDGE
		/*
		** Don't mess with this one - IRCII needs it! -Avalon
		*/
		sendto_one(sptr,
			"NOTICE %s :*** Your host is %s, running version %s",
			nick, get_client_name(&me, FALSE), version);
#endif
		sendto_one(sptr, rpl_str(RPL_CREATED),me.name,nick,creation);
		sendto_one(sptr, rpl_str(RPL_MYINFO), me.name, parv[0],
			   me.name, version);
		(void)m_lusers(sptr, sptr, 1, parv);
		update_load();
		(void)m_motd(sptr, sptr, 1, parv);
		nextping = now;
		sendto_realops("Client connecting: %s (%s@%s)", nick,
			   user->username, user->host);
	    }
	else if (IsServer(cptr))
	    {
		aClient	*acptr;

		acptr = user->server;
		if (acptr->from != sptr->from)
		   {
			sendto_one(cptr, ":%s KILL %s :%s (%s != %s[%s])",
				   me.name, sptr->name, me.name,
				   user->server->name, acptr->from->name,
				   acptr->from->sockhost);
			sptr->flags |= FLAGS_KILLED;
			return exit_client(cptr, sptr, &me,
					   "NICK server wrong direction");
		   }
		else sptr->flags|=(acptr->flags & FLAGS_TS8);
	    }

	sendto_serv_butone(cptr, ":%s NICK %s %d %d %s %s %s :%s",
	  user->server->name,
	  nick, sptr->hopcount+1, sptr->lastnick, user->username, user->host,
	  user->server->name, sptr->info);
	if (MyConnect(sptr))
		send_umode_out(cptr, sptr, 0);
#ifdef	USE_SERVICES
	check_services_butone(SERVICE_WANT_NICK, sptr,
	  ":%s NICK %s %d %d %s %s %s :%s",
	  user->server->name,
	  nick, sptr->hopcount, sptr->lastnick,
	  user->username, user->host, user->server->name, sptr->info);
#endif
#ifdef NPATH
          note_signon(sptr);
#endif

	return 0;
    }

/*
** m_nick
**	parv[0] = sender prefix
**	parv[1] = nickname
**  if from server:
**      parv[2] = hopcount
**      parv[3] = timestamp
**  new nick:
**      parv[4] = username
**      parv[5] = hostname
**      parv[6] = info
**      parv[0] = server
*/
int	m_nick(cptr, sptr, parc, parv)
aClient *cptr, *sptr;
int	parc;
char	*parv[];
{
	aClient *acptr, *server = NULL;
	char	nick[NICKLEN+2], *s;
	Link	*lp;
	time_t	lastnick = (time_t)0;
	int	differ = 1;
	
	if (parc < 2)
	    {
		sendto_one(sptr, err_str(ERR_NONICKNAMEGIVEN),
			   me.name, parv[0]);
		return 0;
	    }
	else if (IsServer(sptr) && parc < 7 ||
	         IsServer(cptr) && parc < 3)
	    {
		sendto_one(sptr, err_str(ERR_NEEDMOREPARAMS),
	    		   me.name, parv[0], "NICK");
		sendto_ops("bad NICK param count for %s from %s",
			   parv[1], get_client_name(cptr, FALSE));
	    	return 0;
	    }
	if (MyConnect(sptr) && (s = (char *)index(parv[1], '~')))
		*s = '\0';
	strncpyzt(nick, parv[1], NICKLEN+1);
	/*
	 * if do_nick_name() returns a null name OR if the server sent a nick
	 * name and do_nick_name() changed it in some way (due to rules of nick
	 * creation) then reject it. If from a server and we reject it,
	 * and KILL it. -avalon 4/4/92
	 */
	if (do_nick_name(nick) == 0 ||
	    (IsServer(cptr) && strcmp(nick, parv[1])))
	    {
		sendto_one(sptr, err_str(ERR_ERRONEUSNICKNAME),
			   me.name, parv[0], parv[1]);

		if (IsServer(cptr))
		    {
			ircstp->is_kill++;
			sendto_ops("Bad Nick: %s From: %s %s",
				   parv[1], parv[0],
				   get_client_name(cptr, FALSE));
			sendto_one(cptr, ":%s KILL %s :%s (%s <- %s[%s])",
				   me.name, parv[1], me.name, parv[1],
				   nick, cptr->name);
			if (sptr != cptr) /* bad nick change */
			    {
				sendto_serv_butone(cptr,
					":%s KILL %s :%s (%s <- %s!%s@%s)",
					me.name, parv[0], me.name,
					get_client_name(cptr, FALSE),
					parv[0],
					sptr->user ? sptr->username : "",
					sptr->user ? sptr->user->server->name :
						     cptr->name);
				sptr->flags |= FLAGS_KILLED;
				return exit_client(cptr, sptr, &me,"BadNick");
			    }
		    }
		return 0;
	    }

	/*
	** Protocol 4 doesn't send the server as prefix, so it is possible
	** the server doesn't exist (a lagged net.burst), in which case
	** we simply need to ignore the NICK. Also when we got that server
	** name (again) but from another direction. --Run
	** This can be removed after all upgraded to 2.9.
	*/
	if (parc > 7 && (!(server = find_server(parv[6], NULL)) ||
	    server->from != cptr->from))
	  return 0;

	/*
	** Check against nick name collisions.
	**
	** Put this 'if' here so that the nesting goes nicely on the screen :)
	** We check against server name list before determining if the nickname
	** is present in the nicklist (due to the way the below for loop is
	** constructed). -avalon
	*/
	if ((acptr = find_server(nick, NULL)))
		if (MyConnect(sptr))
		    {
			sendto_one(sptr, err_str(ERR_NICKNAMEINUSE), me.name,
				   BadPtr(parv[0]) ? "*" : parv[0], nick);
			return 0; /* NICK message ignored */
		    }
	/*
	** acptr already has result from previous find_server()
	*/
	if (acptr)
	    {
		/*
		** We have a nickname trying to use the same name as
		** a server. Send out a nick collision KILL to remove
		** the nickname. As long as only a KILL is sent out,
		** there is no danger of the server being disconnected.
		** Ultimate way to jupiter a nick ? >;-). -avalon
		*/
		sendto_ops("Nick collision on %s(%s <- %s)",
			   sptr->name, acptr->from->name,
			   get_client_name(cptr, FALSE));
		ircstp->is_kill++;
		sendto_one(cptr, ":%s KILL %s :%s (%s <- %s)",
			   me.name, sptr->name, me.name, acptr->from->name,
			   /* NOTE: Cannot use get_client_name
			   ** twice here, it returns static
			   ** string pointer--the other info
			   ** would be lost
			   */
			   get_client_name(cptr, FALSE));
		sptr->flags |= FLAGS_KILLED;
		return exit_client(cptr, sptr, &me, "Nick/Server collision");
	    }

	/*
	** Refuse nick change if the last nick change was less
	** then 30 seconds ago. This is intended to get rid of
	** clone bots doing NICK FLOOD. -SeKs
	** If someone didn't change their nick for more then 60 seconds
	** however, allow to do two nick changes immedately after another
	** before limiting the nick flood. -Run
	*/
#ifdef NICK_DELAY
	if (MyClient(cptr))
	{
	    if(now < cptr->nextnick)
	    {
		cptr->nextnick += 2;
		sendto_one(cptr, err_str(ERR_NICKTOOFAST),
			me.name, parv[0], parv[1], cptr->nextnick - now);  /* Send error message */
		sendto_prefix_one(cptr, cptr, ":%s NICK %s",
			parv[0], parv[0]);  /* bounce NICK to user */
		return 0;  /* ignore nick change! */
	    }
	    else
	    {
	        /* Limit total to 1 change per NICK_DELAY seconds: */
	    	cptr->nextnick += NICK_DELAY;
	    	/* However allow _maximal_ 1 extra consecutive nick change: */
	    	if (cptr->nextnick < now)
	    	  cptr->nextnick = now;
	    }
        }
#endif

	if (!(acptr = find_client(nick, NULL)))
		goto nickkilldone;  /* No collisions, all clear... */
	/*
	** If acptr == sptr, then we have a client doing a nick
	** change between *equivalent* nicknames as far as server
	** is concerned (user is changing the case of his/her
	** nickname or somesuch)
	*/
	if (acptr == sptr)
		if (strcmp(acptr->name, nick) != 0)
			/*
			** Allows change of case in his/her nick
			*/
			goto nickkilldone; /* -- go and process change */
		else
			/*
			** This is just ':old NICK old' type thing.
			** Just forget the whole thing here. There is
			** no point forwarding it to anywhere,
			** especially since servers prior to this
			** version would treat it as nick collision.
			*/
			return 0; /* NICK Message ignored */
	/*
	** Note: From this point forward it can be assumed that
	** acptr != sptr (point to different client structures).
	*/
	/*
	** If the older one is "non-person", the new entry is just
	** allowed to overwrite it. Just silently drop non-person,
	** and proceed with the nick. This should take care of the
	** "dormant nick" way of generating collisions...
	*/
	if (IsUnknown(acptr) && MyConnect(acptr))
	    {
		(void)exit_client(cptr, acptr, &me,
		    "Overridden by other sign on");
		goto nickkilldone;
	    }
	/*
	** Decide, we really have a nick collision and deal with it
	*/
	if (!IsServer(cptr))
	    {
		/*
		** NICK is coming from local client connection. Just
		** send error reply and ignore the command.
		*/
		sendto_one(sptr, err_str(ERR_NICKNAMEINUSE),
			   /* parv[0] is empty when connecting */
			   me.name, BadPtr(parv[0]) ? "*" : parv[0], nick);
		return 0; /* NICK message ignored */
	    }
	/*
	** NICK was coming from a server connection.
	** This means we have a race condition (two users signing on
	** at the same time), or two net fragments reconnecting with
	** the same nick.
	** The latter can happen because two different users connected
	** or because one and the same user switched server during a
	** net break.
	** If the TimeStamps are equal, we kill both (or only 'new'
	** if it was a ":server NICK new ...").
	** Otherwise we kill the youngest when user@host differ,
	** or the oldest when they are the same.
	** We treat user and ~user as different, because if it wasn't
	** a faked ~user the AUTH wouldn't have added the '~'.
	** --Run
	** 
	*/
	if (IsServer(sptr))
	{
	    /*
	    ** A new NICK being introduced by a neighbouring
	    ** server (e.g. message type ":server NICK new ..." received)
	    */
	    lastnick = atoi(parv[3]);
	    differ = (mycmp(acptr->user->username, parv[4]) ||
	        mycmp(acptr->user->host, parv[5]));
	    sendto_ops("Nick collision on %s (%s %d <- %s %d (%s user@host))",
		    acptr->name, acptr->from->name, acptr->lastnick, 
		    get_client_name(cptr, FALSE), lastnick,
		    differ?"Different":"Same");
	}
	else
	{
	    /*
	    ** A NICK change has collided (e.g. message type ":old NICK new").
	    */
	    lastnick = atoi(parv[2]);
	    differ = (mycmp(acptr->user->username, sptr->user->username) ||
		mycmp(acptr->user->host, sptr->user->host));
	    sendto_ops("Nick change collision from %s to %s (%s %d <- %s %d)",
		    sptr->name, acptr->name, acptr->from->name,
		    acptr->lastnick, get_client_name(cptr, FALSE), lastnick);
	}
	/*
	** Now remove (kill) the nick on our side if it is the youngest.
	** If no timestamp was received, we ignore the incoming nick
	** (and expect a KILL for our legit nick soon ):
	** When the timestamps are equal we kill both nicks. --Run
	** acptr->from != cptr should *always* be true (?).
	*/
	if (acptr->from != cptr)
	{ if (differ && lastnick >= acptr->lastnick ||
	      !differ && lastnick <= acptr->lastnick)
	  { if (!IsServer(sptr))
	    { ircstp->is_kill++;
	      sendto_serv_butone(cptr, /* Kill old from outgoing servers */
		":%s KILL %s :%s (%s <- %s (Nick collision))",
		me.name, sptr->name, me.name,
		acptr->from->name, get_client_name(cptr, FALSE));
	      sptr->flags |= FLAGS_KILLED;
	      (void)exit_client(cptr, sptr, &me,
	          "Nick collision (you're a ghost)");
	    }
	    if (lastnick != acptr->lastnick)
	      return 0; /* Ignore the NICK */
	  }
	  sendto_one(acptr, err_str(ERR_NICKCOLLISION),
	      me.name, acptr->name, nick); }
	ircstp->is_kill++;
	acptr->flags |= FLAGS_KILLED;
	if (differ)
	{
	  sendto_serv_butone(cptr, /* Kill our old from outgoing servers */
		  ":%s KILL %s :%s (%s <- %s (older nick overruled))",
		  me.name, acptr->name, me.name,
		  acptr->from->name, get_client_name(cptr, FALSE));
	  (void)exit_client(cptr, acptr, &me,
	      "Nick collision (older nick overruled)");
	}
	else
	{
	  sendto_serv_butone(cptr, /* Kill our old from outgoing servers */
		  ":%s KILL %s :%s "
		  "(%s <- %s (nick collision from same user@host))",
		  me.name, acptr->name, me.name,
		  acptr->from->name, get_client_name(cptr, FALSE));
	  (void)exit_client(cptr, acptr, &me,
	      "Nick collision (You collided yourself)");
	}
	if (lastnick == acptr->lastnick) return 0;

nickkilldone:
	if (IsServer(sptr))
	    {
		/* A server introducing a new client, change source */

		if (!server) /* Test not necessary when all upgraded to 2.9 */
		  server = sptr;
		sptr = make_client(cptr);
		add_client_to_list(sptr);
		sptr->hopcount = atoi(parv[2]);
		sptr->lastnick = atoi(parv[3]);
	    }
	else if (sptr->name[0])
	    {
		/*
		** If the client belongs to me, then check to see
		** if client is currently on any channels where it
		** is currently banned.  If so, do not allow the nick
		** change to occur.
		** Also set 'lastnick' to current time, if changed.
		*/
		if (MyClient(sptr))
			for (lp = cptr->user->channel; lp; lp = lp->next)
				if (can_send(cptr, lp->value.chptr) ==
				    MODE_BAN)
				    {
					sendto_one(cptr,
						   err_str(ERR_BANNICKCHANGE),
						   me.name, parv[0],
						   lp->value.chptr->chname);
					return 0;
				    }

		/*
		** Client just changing his/her nick. If he/she is
		** on a channel, send note of change to all clients
		** on that channel. Propagate notice to other servers.
		*/
		if (mycmp(parv[0], nick))
		  sptr->lastnick = (sptr == cptr) ? TStime(): atoi(parv[2]);
		if (sptr->user)
		{
		  sendto_common_channels(sptr, ":%s NICK :%s", parv[0], nick);
		  add_history(sptr);
		  sendto_serv_butone(cptr, ":%s NICK %s :%d",
		      parv[0], nick, sptr->lastnick);
#ifdef	USE_SERVICES
		  check_services_butone(SERVICE_WANT_NICK, sptr,
		      ":%s NICK %s :%d", parv[0], nick, sptr->lastnick);
#endif
#ifdef NPATH
                  note_nickchange(sptr, nick);
#endif
	        }
	    }
	else
	    {
		/* Client setting NICK the first time */

		/* This had to be copied here to avoid problems.. */
		(void)strcpy(sptr->name, nick);

		/* If the client hasn't gotten a cookie-ping yet,
		   choose a cookie and send it. -record!jegelhof@cloud9.net */

		if(!sptr->cookie)
		{
			while((!sptr->cookie) || (sptr->cookie==-1))
			  sptr->cookie=(ircrandom());
			sendto_one(cptr, "PING :%lu", sptr->cookie);
		}

		if ((sptr->user) && (sptr->cookie==-1))
		{	/*
			** USER and PONG already received, now we have NICK.
			** *NOTE* For servers "NICK" *must* precede the
			** user message (giving USER before NICK is possible
			** only for local client connection!). register_user
			** may reject the client and call exit_client for it
			** --must test this and exit m_nick too!!!
			*/
			sptr->lastnick = TStime(); /* Always local client */
			if (register_user(cptr, sptr, nick,
					  sptr->user->username)
			    == CPTR_KILLED)
				return CPTR_KILLED; }
	    }
	/*
	**  Finally set new nick name.
	*/
	if (sptr->name[0])
		(void)del_from_client_hash_table(sptr->name, sptr);
	(void)strcpy(sptr->name, nick);
	(void)add_to_client_hash_table(nick, sptr);
	if (!IsServer(cptr) || sptr->user)
	  return 0;
	sptr->user = make_user(sptr);
	sptr->user->server = server;
	server->serv->ghost = 0; /* :server NICK means end of net.burst */
	if (parc>7)
	  /* This for compatibility with .U4 only */
	  strncpyzt(sptr->info, parv[7], sizeof(sptr->info));
	else
	  strncpyzt(sptr->info, parv[6], sizeof(sptr->info));
	strncpyzt(sptr->user->host, parv[5], sizeof(sptr->user->host));
	return register_user(cptr, sptr, sptr->name, parv[4]);
}

/*
** m_message (used in m_private() and m_notice())
** the general function to deliver MSG's between users/channels
**
**	parv[0] = sender prefix
**	parv[1] = receiver list
**	parv[2] = message text
**
** massive cleanup
** rev argv 6/91
**
*/

static	int	m_message(cptr, sptr, parc, parv, notice)
aClient *cptr, *sptr;
int	parc;
char	*parv[];
int	notice;
{
	Reg1	aClient	*acptr;
	Reg2	char	*s;
	aChannel *chptr;
	char	*nick, *server, *p, *cmd, *host;

	if (notice)
	    {
		if (check_registered(sptr))
			return 0;
	    }
	else if (check_registered_user(sptr))
		return 0;

	sptr->flags&=~FLAGS_TS8;

	cmd = notice ? MSG_NOTICE : MSG_PRIVATE;

	if (parc < 2 || *parv[1] == '\0')
	    {
		sendto_one(sptr, err_str(ERR_NORECIPIENT),
			   me.name, parv[0], cmd);
		return -1;
	    }

	if (parc < 3 || *parv[2] == '\0')
	    {
		sendto_one(sptr, err_str(ERR_NOTEXTTOSEND), me.name, parv[0]);
		return -1;
	    }

	if (MyConnect(sptr))
		parv[1] = canonize(parv[1]);
	for (p = NULL, nick = strtoken(&p, parv[1], ","); nick;
	     nick = strtoken(&p, NULL, ","))
	    {
		/*
		** nickname addressed?
		*/
		if ((acptr = find_person(nick, NULL)))
		{
		    /*
		    ** This part is almost literally copied from is_silenced(),
		    ** it does the silence checking for NOTE notices.
		    */
                    if (notice && IsServer(sptr) &&
                        !strncmp(parv[2], "Note from", 9))
		    {
		      Reg1 Link *lp;
		      char *sender, *p2;

		      if (acptr->user && (lp = acptr->user->silence) &&
			  (sender = strtoken(&p2, &parv[2][10], " ")))
		      {
			for (; lp; lp = lp->next)
			{
			  if (!matches(lp->value.cp, sender))
			  {
			    if (!IsMe(sptr))
			    {
			      sendto_one(sptr, ":%s SILENCE %s :%s",
			          acptr->name, sptr->name, lp->value.cp);
			      lp->flags=1;
			    }
			    return 0;
			  }
		        }
			*p2 = ' ';
		      }
		    }
		    if (!is_silenced(sptr, acptr))
		    {
			if (!notice && MyConnect(sptr) &&
			    acptr->user && acptr->user->away)
				sendto_one(sptr, rpl_str(RPL_AWAY), me.name,
					   parv[0], acptr->name,
					   acptr->user->away);
			sendto_prefix_one(acptr, sptr, ":%s %s %s :%s",
					  parv[0], cmd, nick, parv[2]);
		    }
		    continue;
                }
		/*
		** channel msg?
		*/
		if ((chptr = find_channel(nick, NullChn)))
		    {
			if (can_send(sptr, chptr) == 0)
				sendto_channel_butone(cptr, sptr, chptr,
						      ":%s %s %s :%s",
						      parv[0], cmd, nick,
						      parv[2]);
			else if (!notice)
				sendto_one(sptr, err_str(ERR_CANNOTSENDTOCHAN),
					   me.name, parv[0], nick);
			continue;
		    }
	
		/*
		** the following two cases allow masks in NOTICEs
		** (for OPERs only)
		**
		** Armin, 8Jun90 (gruner@informatik.tu-muenchen.de)
		*/
		if ((*nick == '$' || *nick == '#') && IsAnOper(sptr))
		    {
			if (!(s = (char *)rindex(nick, '.')))
			    {
				sendto_one(sptr, err_str(ERR_NOTOPLEVEL),
					   me.name, parv[0], nick);
				continue;
			    }
			while (*++s)
				if (*s == '.' || *s == '*' || *s == '?')
					break;
			if (*s == '*' || *s == '?')
			    {
				sendto_one(sptr, err_str(ERR_WILDTOPLEVEL),
					   me.name, parv[0], nick);
				continue;
			    }
			sendto_match_butone(IsServer(cptr) ? cptr : NULL, 
					    sptr, nick + 1,
					    (*nick == '#') ? MATCH_HOST :
							     MATCH_SERVER,
					    ":%s %s %s :%s", parv[0],
					    cmd, nick, parv[2]);
			continue;
		    }
	
		/*
		** user[%host]@server addressed?
		*/
		if ((server = (char *)index(nick, '@')) &&
		    (acptr = find_server(server + 1, NULL)))
		    {
			int count = 0;

			/*
			** Not destined for a user on me :-(
			*/
			if (!IsMe(acptr))
			    {
				sendto_one(acptr,":%s %s %s :%s", parv[0],
					   cmd, nick, parv[2]);
				continue;
			    }
			*server = '\0';

			if ((host = (char *)index(nick, '%')))
				*host++ = '\0';

			/*
			** Look for users which match the destination host
			** (no host == wildcard) and if one and one only is
			** found connected to me, deliver message!
			*/
			acptr = find_userhost(nick, host, NULL, &count);
			if (server)
				*server = '@';
			if (host)
				*--host = '%';
			if (acptr)
			    {
				if (count == 1)
				{
					if (is_silenced(sptr, acptr))
					  continue;
					sendto_prefix_one(acptr, sptr,
							  ":%s %s %s :%s",
					 		  parv[0], cmd,
							  nick, parv[2]);
				}
				else if (!notice)
					sendto_one(sptr,
						   err_str(ERR_TOOMANYTARGETS),
						   me.name, parv[0], nick);
			    }
			if (acptr)
				continue;
		    }
		sendto_one(sptr, err_str(ERR_NOSUCHNICK), me.name,
			   parv[0], nick);
            }
    return 0;
}

/*
** m_private
**	parv[0] = sender prefix
**	parv[1] = receiver list
**	parv[2] = message text
*/

int	m_private(cptr, sptr, parc, parv)
aClient *cptr, *sptr;
int	parc;
char	*parv[];
{
	return m_message(cptr, sptr, parc, parv, 0);
}

/*
** m_notice
**	parv[0] = sender prefix
**	parv[1] = receiver list
**	parv[2] = notice text
*/

int	m_notice(cptr, sptr, parc, parv)
aClient *cptr, *sptr;
int	parc;
char	*parv[];
{
	return m_message(cptr, sptr, parc, parv, 1);
}

static	void	do_who(sptr, acptr, repchan)
aClient *sptr, *acptr;
aChannel *repchan;
{
	char	status[7];
	int	i = 0;

	if (acptr->user->away)
		status[i++] = 'G';
	else
		status[i++] = 'H';
	if (IsAnOper(acptr))
		status[i++] = '*';
	if (repchan && is_chan_op(acptr, repchan))
		status[i++] = '@';
	else if (repchan && has_voice(acptr, repchan))
		status[i++] = '+';
	else if (repchan && is_zombie(acptr, repchan))
		status[i++] = '!';
        if (IsDeaf(acptr))
        	status[i++] = 'd';
	if (IsAnOper(sptr))
	{
	  if (IsInvisible(acptr))
	    status[i++] = 'i';
	  if (SendWallops(acptr))
	    status[i++] = 'w';
        }
	status[i] = '\0';
	sendto_one(sptr, rpl_str(RPL_WHOREPLY), me.name, sptr->name,
		   (repchan) ? (repchan->chname) : "*", acptr->user->username,
		   acptr->user->host, acptr->user->server->name, acptr->name,
		   status, acptr->hopcount, acptr->info);
}


/*
** m_who
**	parv[0] = sender prefix
**	parv[1] = nickname mask list
**	parv[2] = additional selection flag, only 'o' for now.
*/
int	m_who(cptr, sptr, parc, parv)
aClient *cptr, *sptr;
int	parc;
char	*parv[];
{
	Reg1	aClient *acptr;
	Reg2	char	*mask = parc > 1 ? parv[1] : NULL;
	Reg3	Link	*lp;
	aChannel *chptr;
	aChannel *mychannel;
	char	*channame = NULL, *s;
	int	oper = parc > 2 ? (*parv[2] == 'o' ): 0; /* Show OPERS only */
	int	member;

	if (!BadPtr(mask))
	    {
		if ((s = (char *)index(mask, ',')))
		    {
			parv[1] = ++s;
			(void)m_who(cptr, sptr, parc, parv);
		    }
		clean_channelname(mask);
	    }

	mychannel = NullChn;
	if (sptr->user)
		if ((lp = sptr->user->channel))
			mychannel = lp->value.chptr;

	/* Allow use of m_who without registering */
	
	/*
	**  Following code is some ugly hacking to preserve the
	**  functions of the old implementation. (Also, people
	**  will complain when they try to use masks like "12tes*"
	**  and get people on channel 12 ;) --msa
	*/
	if (!mask || *mask == '\0')
		mask = NULL;
	else if (mask[1] == '\0' && mask[0] == '*')
	    {
		mask = NULL;
		if (mychannel)
			channame = mychannel->chname;
	    }
	else if (mask[1] == '\0' && mask[0] == '0') /* "WHO 0" for irc.el */
		mask = NULL;
	else
		channame = mask;
	(void)collapse(mask);

	if (IsChannelName(channame))
	    {
		/*
		 * List all users on a given channel
		 */
		chptr = find_channel(channame, NULL);
		if (chptr)
		  {
		    member = IsMember(sptr, chptr);
		    if (member || !SecretChannel(chptr))
			for (lp = chptr->members; lp; lp = lp->next)
			    {
				if (oper && !IsAnOper(lp->value.cptr))
					continue;
				if (lp->value.cptr!=sptr &&
				    (lp->flags & CHFL_ZOMBIE))
					continue;
				if (lp->value.cptr!=sptr && IsInvisible(lp->value.cptr) && !member)
					continue;
				do_who(sptr, lp->value.cptr, chptr);
			    }
		  }
	    }
	else for (acptr = client; acptr; acptr = acptr->next)
	    {
		aChannel *ch2ptr = NULL;
		int	showperson, isinvis;

		if (!IsPerson(acptr))
			continue;
		if (oper && !IsAnOper(acptr))
			continue;
		showperson = 0;
		/* Allow opers to see local invisible users. --Ensor */
		if (IsAnOper(sptr) && MyConnect(acptr))
			showperson = 1;
#ifdef SHOW_ALL_INVISIBLE_USERS
		else
			/* Allow opers to see ALL invisible users. -- Niels */
			if (IsOper(sptr))
				showperson = 1;
#endif

		/*
		 * Show user if they are on the same channel, or not
		 * invisible and on a non secret channel (if any).
		 * Do this before brute force match on all relevant fields
		 * since these are less cpu intensive (I hope :-) and should
		 * provide better/more shortcuts - avalon
		 */
		isinvis = acptr!=sptr && IsInvisible(acptr);
		for (lp = acptr->user->channel; lp; lp = lp->next)
		    {
			chptr = lp->value.chptr;
			member = IsMember(sptr, chptr);
			if (isinvis && !member)
				continue;
			if (is_zombie(acptr, chptr))
				continue;
			if (member || (!isinvis && ShowChannel(sptr, chptr)))
			    {
				ch2ptr = chptr;
				showperson = 1;
				break;
			    }
			if (HiddenChannel(chptr) && !SecretChannel(chptr) &&
			    !isinvis)
				showperson = 1;
		    }
		if (!acptr->user->channel && !isinvis)
			showperson = 1;
		/*
		** This is brute force solution, not efficient...? ;( 
		** Show entry, if no mask or any of the fields match
		** the mask. --msa
		*/
		if (showperson &&
		    (!mask ||
		     match(mask, acptr->name) == 0 ||
		     match(mask, acptr->user->username) == 0 ||
		     match(mask, acptr->user->host) == 0 ||
		     match(mask, acptr->user->server->name) == 0 ||
		     match(mask, acptr->info) == 0))
			do_who(sptr, acptr, ch2ptr);
	    }
	sendto_one(sptr, rpl_str(RPL_ENDOFWHO), me.name, parv[0],
		   BadPtr(mask) ?  "*" : mask);
	return 0;
}

/*
** m_whois
**	parv[0] = sender prefix
**	parv[1] = nickname masklist
*/
int	m_whois(cptr, sptr, parc, parv)
aClient *cptr, *sptr;
int	parc;
char	*parv[];
{
	Reg2	Link	*lp;
	Reg3	anUser	*user;
	aClient *acptr, *a2cptr;
	aChannel *chptr;
	char	*nick, *tmp, *name;
	char	*p = NULL;
	int	found, len, mlen;

	if (check_registered_user(sptr))
		return 0;

    	if (parc < 2)
	    {
		sendto_one(sptr, err_str(ERR_NONICKNAMEGIVEN),
			   me.name, parv[0]);
		return 0;
	    }

	if (parc > 2)
	    {
		if (hunt_server(cptr,sptr,":%s WHOIS %s :%s", 1,parc,parv) !=
		    HUNTED_ISME)
			return 0;
		parv[1] = parv[2];
	    }

	for (tmp = parv[1]; (nick = strtoken(&p, tmp, ",")); tmp = NULL)
	    {
		int	invis, showperson, member, wilds;

		found = 0;
		(void)collapse(nick);
		wilds = (index(nick, '?') || index(nick, '*'));
                /*
                ** We're no longer allowing remote users to generate
                ** requests with wildcards.
                */
                if (!MyConnect(sptr) && wilds)
				continue;
		for (acptr = client; (acptr = next_client(acptr, nick));
		     acptr = acptr->next)
		    {
			if (IsServer(acptr) || IsPing(acptr))
				continue;
			/*
			 * I'm always last :-) and acptr->next == NULL!!
			 */
			if (IsMe(acptr))
				break;
			/*
			 * 'Rules' established for sending a WHOIS reply:
			 *
			 * - if wildcards are being used dont send a reply if
			 *   the querier isnt any common channels and the
			 *   client in question is invisible and wildcards are
			 *   in use (allow exact matches only);
			 *
			 * - only send replies about common or public channels
			 *   the target user(s) are on;
			 */
			user = acptr->user;
			name = (!*acptr->name) ? "?" : acptr->name;

			invis = acptr!=sptr && IsInvisible(acptr);
			member = (user && user->channel) ? 1 : 0;
			showperson = (wilds && !invis && !member) || !wilds;
			if (user)
			  for (lp = user->channel; lp; lp = lp->next)
			    {
				chptr = lp->value.chptr;
				member = IsMember(sptr, chptr);
				if (invis && !member)
					continue;
				if (is_zombie(acptr, chptr))
					continue;
				if (member || (!invis && PubChannel(chptr)))
				    {
					showperson = 1;
					break;
				    }
				if (!invis && HiddenChannel(chptr) &&
				    !SecretChannel(chptr))
					showperson = 1;
			    }
			if (!showperson)
				continue;

			if (user)
			{
			  a2cptr = user->server;
			  sendto_one(sptr, rpl_str(RPL_WHOISUSER), me.name,
				   parv[0], name,
				   user->username, user->host, acptr->info);
			}
			else
			{
			  a2cptr = &me;
			  sendto_one(sptr, rpl_str(RPL_WHOISUSER), me.name,
			      parv[0], name, "<unknown>", "<unknown>",
			      "<unknown>");
			}

			found = 1;

			if (user)
			{
			  mlen = strlen(me.name) + strlen(parv[0]) + 12 +
			      strlen(name);
			  for (len = 0, *buf = '\0', lp = user->channel; lp;
			     lp = lp->next)
			  {
			      chptr = lp->value.chptr;
			      if (ShowChannel(sptr, chptr) &&
				  (acptr==sptr || !is_zombie(acptr, chptr)))
				  {
				      if (len + strlen(chptr->chname) + mlen
					  > BUFSIZE - 5)
					  {
					      sendto_one(sptr,
							 ":%s %d %s %s :%s",
							 me.name,
							 RPL_WHOISCHANNELS,
							 parv[0], name, buf);
					      *buf = '\0';
					      len = 0;
					  }
				      if (IsDeaf(acptr))
					      *(buf + len++) = '-';
				      if (is_chan_op(acptr, chptr))
					      *(buf + len++) = '@';
				      else if (has_voice(acptr, chptr))
					      *(buf + len++) = '+';
				      else if (is_zombie(acptr, chptr))
					      *(buf + len++) = '!';
				      if (len)
					      *(buf + len) = '\0';
				      (void)strcpy(buf + len, chptr->chname);
				      len += strlen(chptr->chname);
				      (void)strcat(buf + len, " ");
				      len++;
				  }
			  }
			  if (buf[0] != '\0')
			    sendto_one(sptr, rpl_str(RPL_WHOISCHANNELS),
			        me.name, parv[0], name, buf);
                        }

			sendto_one(sptr, rpl_str(RPL_WHOISSERVER), me.name,
			    parv[0], name, a2cptr->name, a2cptr->info);

                        if (user)
                        {
			  if (user->away)
			    sendto_one(sptr, rpl_str(RPL_AWAY), me.name,
			        parv[0], name, user->away);

			  if (IsAnOper(acptr))
			    sendto_one(sptr, rpl_str(RPL_WHOISOPERATOR),
			        me.name, parv[0], name);

			  if (MyConnect(acptr))
			    sendto_one(sptr, rpl_str(RPL_WHOISIDLE), me.name,
			        parv[0], name, now - user->last,
			        acptr->firsttime);
                        }
		    }
		if (!found)
			sendto_one(sptr, err_str(ERR_NOSUCHNICK),
				   me.name, parv[0], nick);
		if (p)
			p[-1] = ',';
	    }
	sendto_one(sptr, rpl_str(RPL_ENDOFWHOIS), me.name, parv[0], parv[1]);

	return 0;
}

/*
** m_user
**	parv[0] = sender prefix
**	parv[1] = username (login name, account)
**	parv[2] = unused
**	parv[3] = unused
**	parv[4] = users real name info
*/
int	m_user(cptr, sptr, parc, parv)
aClient	*cptr, *sptr;
int	parc;
char	*parv[];
{
#define	UFLAGS	(FLAGS_INVISIBLE|FLAGS_WALLOP|FLAGS_SERVNOTICE)
	char	*username, *host, *server, *realname;
	anUser	*user;
 
	if (IsServer(cptr))
		return 0;

	if (parc > 2 && (username = (char *)index(parv[1],'@')))
	      *username = '\0'; 
	if (parc < 5 || *parv[1] == '\0' || *parv[2] == '\0' ||
	    *parv[3] == '\0' || *parv[4] == '\0')
	    {
		sendto_one(sptr, err_str(ERR_NEEDMOREPARAMS),
			   me.name, parv[0], "USER");
		return 0;
	    }

	/* Copy parameters into better documenting variables */

	username = (parc < 2 || BadPtr(parv[1])) ? "<bad-boy>" : parv[1];
	host     = (parc < 3 || BadPtr(parv[2])) ? "<nohost>" : parv[2];
	server   = (parc < 4 || BadPtr(parv[3])) ? "<noserver>" : parv[3];
	realname = (parc < 5 || BadPtr(parv[4])) ? "<bad-realname>" : parv[4];

 	user = make_user(sptr);

	if (!IsUnknown(sptr))
	    {
		sendto_one(sptr, err_str(ERR_ALREADYREGISTRED),
			   me.name, parv[0]);
		return 0;
	    }

	sptr->flags |= (UFLAGS & atoi(host));
	user->server = &me;
	strncpyzt(sptr->info, realname, sizeof(sptr->info));
	if ((sptr->name[0]) && (sptr->cookie==-1))
		/* NICK and PONG already received, now we have USER... */
		return register_user(cptr, sptr, sptr->name, username);
	else
	{
	  strncpyzt(sptr->user->username, username, USERLEN+1);
	  strncpyzt(user->host, host, sizeof(user->host));
	}
	return 0;
}

/*
** m_quit
**	parv[0] = sender prefix
**	parv[1] = comment
*/
int	m_quit(cptr, sptr, parc, parv)
aClient *cptr, *sptr;
int	parc;
char	*parv[];
    {
	register char *comment = (parc > 1 && parv[1]) ? parv[1] : cptr->name;

	if (MyClient(sptr))
		if (!strncmp("Local Kill", comment, 10) ||
		    !strncmp(comment, "Killed", 6))
			comment = parv[0];
	if (strlen(comment) > (size_t) TOPICLEN)
		comment[TOPICLEN] = '\0';
	return IsServer(sptr) ? 0 : exit_client(cptr, sptr, sptr, comment);
    }

/*
** m_kill
**	parv[0] = sender prefix
**	parv[1] = kill victim
**	parv[2] = kill path
*/
int	m_kill(cptr, sptr, parc, parv)
aClient *cptr, *sptr;
int	parc;
char	*parv[];
{
	aClient *acptr;
	char	*inpath = get_client_name(cptr,FALSE);
	char	*user, *path, *killer;
	int	chasing = 0;

	if (parc < 2 || *parv[1] == '\0')
	    {
		sendto_one(sptr, err_str(ERR_NEEDMOREPARAMS),
			   me.name, parv[0], "KILL");
		return 0;
	    }

	user = parv[1];
	path = parv[2]; /* Either defined or NULL (parc >= 2!!) */

#ifdef	OPER_KILL
	if (!IsPrivileged(cptr))
	    {
		sendto_one(sptr, err_str(ERR_NOPRIVILEGES), me.name, parv[0]);
		return 0;
	    }
#else
	if (!IsServer(cptr))
	    {
		sendto_one(sptr, err_str(ERR_NOPRIVILEGES), me.name, parv[0]);
		return 0;
	    }
#endif
	if (IsAnOper(cptr))
	    {
		if (BadPtr(path))
		    {
			sendto_one(sptr, err_str(ERR_NEEDMOREPARAMS),
				   me.name, parv[0], "KILL");
			return 0;
		    }
		if (strlen(path) > (size_t) TOPICLEN)
			path[TOPICLEN] = '\0';
	    }

	if (!(acptr = find_client(user, NULL)))
	    {
		/*
		** If the user has recently changed nick, we automaticly
		** rewrite the KILL for this new nickname--this keeps
		** servers in synch when nick change and kill collide
		*/
		if (!(acptr = get_history(user, (long)KILLCHASETIMELIMIT)))
		    {
			sendto_one(sptr, err_str(ERR_NOSUCHNICK),
				   me.name, parv[0], user);
			return 0;
		    }
		sendto_one(sptr,":%s NOTICE %s :KILL changed from %s to %s",
			   me.name, parv[0], user, acptr->name);
		chasing = 1;
	    }
	if (!MyConnect(acptr) && IsLocOp(cptr))
	    {
		sendto_one(sptr, err_str(ERR_NOPRIVILEGES), me.name, parv[0]);
		return 0;
	    }
	if (IsServer(acptr) || IsMe(acptr))
	    {
		sendto_one(sptr, err_str(ERR_CANTKILLSERVER),
			   me.name, parv[0]);
		return 0;
	    }

#ifdef	LOCAL_KILL_ONLY
	if (MyConnect(sptr) && !MyConnect(acptr))
	    {
		sendto_one(sptr, ":%s NOTICE %s :Nick %s isnt on your server",
			   me.name, parv[0], acptr->name);
		return 0;
	    }
#endif
	if (!IsServer(cptr))
	    {
		/*
		** The kill originates from this server, initialize path.
		** (In which case the 'path' may contain user suplied
		** explanation ...or some nasty comment, sigh... >;-)
		**
		**	...!operhost!oper
		**	...!operhost!oper (comment)
		*/
		if (IsUnixSocket(cptr)) /* Don't use get_client_name syntax */
			inpath = me.sockhost;
		else
			inpath = cptr->sockhost;
		if (!BadPtr(path))
		    {
			(void)sprintf(buf, "%s%s (%s)",
				cptr->name, IsOper(sptr) ? "" : "(L)", path);
			path = buf;
		    }
		else
			path = cptr->name;
	    }
	else if (BadPtr(path))
		 path = "*no-path*"; /* Bogus server sending??? */
	/*
	** Notify all *local* opers about the KILL (this includes the one
	** originating the kill, if from this server--the special numeric
	** reply message is not generated anymore).
	**
	** Note: "acptr->name" is used instead of "user" because we may
	**	 have changed the target because of the nickname change.
	*/
	if (IsLocOp(sptr) && !MyConnect(acptr))
	    {
		sendto_one(sptr, err_str(ERR_NOPRIVILEGES), me.name, parv[0]);
		return 0;
	    }
	sendto_ops("Received KILL message for %s. From %s Path: %s!%s",
		   acptr->name, parv[0], inpath, path);
#if defined(USE_SYSLOG) && defined(SYSLOG_KILL)
	if (MyClient(acptr))
	  {/* get more infos when your local clients are killed -- _dl*/
	    if (IsServer(sptr))
	      syslog(LOG_DEBUG,
		    "A local client %s!%s@%s KILLED from %s [%s] Path: %s!%s)",
		     acptr->name, acptr->user->username, acptr->user->host,
		     parv[0],sptr->name,inpath,path);
	    else
	      syslog(LOG_DEBUG,
		     "A local client %s!%s@%s KILLED by %s [%s!%s@%s] (%s!%s)",
		     acptr->name,acptr->user->username,acptr->user->host,
		     parv[0],sptr->name,sptr->user->username,sptr->user->host,
		     inpath,path);
	  }
	else if (IsOper(sptr))
	  syslog(LOG_DEBUG,"KILL From %s For %s Path %s!%s",
		 parv[0], acptr->name, inpath, path);
#endif
	/*
	** And pass on the message to other servers. Note, that if KILL
	** was changed, the message has to be sent to all links, also
	** back.
	** Suicide kills are NOT passed on --SRB
	*/
	if (!MyConnect(acptr) || !MyConnect(sptr) || !IsAnOper(sptr))
	    {
		sendto_serv_butone(cptr, ":%s KILL %s :%s!%s",
				   parv[0], acptr->name, inpath, path);
		if (chasing && IsServer(cptr))
			sendto_one(cptr, ":%s KILL %s :%s!%s",
				   me.name, acptr->name, inpath, path);
		acptr->flags |= FLAGS_KILLED;
	    }
#ifdef	USE_SERVICES
	check_services_butone(SERVICE_WANT_KILL, sptr, ":%s KILL %s :%s!%s",
				parv[0], acptr->name, inpath, path);
#endif

	/*
	** Tell the victim she/he has been zapped, but *only* if
	** the victim is on current server--no sense in sending the
	** notification chasing the above kill, it won't get far
	** anyway (as this user don't exist there any more either)
	*/
	if (MyConnect(acptr))
		sendto_prefix_one(acptr, sptr,":%s KILL %s :%s!%s",
				  parv[0], acptr->name, inpath, path);
	/*
	** Set FLAGS_KILLED. This prevents exit_one_client from sending
	** the unnecessary QUIT for this. (This flag should never be
	** set in any other place)
	*/
	if (MyConnect(acptr) && MyConnect(sptr) && IsAnOper(sptr))
		(void)sprintf(buf2, "Local kill by %s (%s)", sptr->name,
			BadPtr(parv[2]) ? sptr->name : parv[2]);
	else
	    {
		if ((killer = index(path, ' ')))
		    {
			while (*killer && *killer != '!')
				killer--;
			if (!*killer)
				killer = path;
			else
				killer++;
		    }
		else
			killer = path;
		(void)sprintf(buf2, "Killed (%s)", killer);
	    }
	return exit_client(cptr, acptr, sptr, buf2);
}

/***********************************************************************
 * m_away() - Added 14 Dec 1988 by jto. 
 *            Not currently really working, I don't like this
 *            call at all...
 *
 *            ...trying to make it work. I don't like it either,
 *	      but perhaps it's worth the load it causes to net.
 *	      This requires flooding of the whole net like NICK,
 *	      USER, MODE, etc messages...  --msa
 ***********************************************************************/

/*
** m_away
**	parv[0] = sender prefix
**	parv[1] = away message
*/
int	m_away(cptr, sptr, parc, parv)
aClient *cptr, *sptr;
int	parc;
char	*parv[];
{
	Reg1	char	*away, *awy2 = parv[1];

	if (check_registered_user(sptr))
		return 0;

	away = sptr->user->away;

	if (parc < 2 || !*awy2)
	    {
		/* Marking as not away */

		if (away)
		    {
			MyFree(away);
			sptr->user->away = NULL;
		    }
		sendto_serv_butone(cptr, ":%s AWAY", parv[0]);
		if (MyConnect(sptr))
			sendto_one(sptr, rpl_str(RPL_UNAWAY),
				   me.name, parv[0]);
#ifdef	USE_SERVICES
		check_services_butonee(SERVICE_WANT_AWAY, ":%s AWAY", parv[0]);
#endif
		return 0;
	    }

	/* Marking as away */

	if (strlen(awy2) > (size_t) TOPICLEN)
		awy2[TOPICLEN] = '\0';
	sendto_serv_butone(cptr, ":%s AWAY :%s", parv[0], awy2);
#ifdef	USE_SERVICES
	check_services_butonee(SERVICE_WANT_AWAY, ":%s AWAY :%s",
				parv[0], parv[1]);
#endif

	if (away)
		away = (char *)MyRealloc(away, strlen(awy2)+1);
	else
		away = (char *)MyMalloc(strlen(awy2)+1);

	sptr->user->away = away;
	(void)strcpy(away, awy2);
	if (MyConnect(sptr))
		sendto_one(sptr, rpl_str(RPL_NOWAWAY), me.name, parv[0]);
	return 0;
}

/*
** m_ping
**	parv[0] = sender prefix
**	parv[1] = origin
**	parv[2] = destination
*/
int	m_ping(cptr, sptr, parc, parv)
aClient *cptr, *sptr;
int	parc;
char	*parv[];
{
	aClient *acptr;
	char	*origin, *destination;

	if (parc < 2 || *parv[1] == '\0')
	    {
		sendto_one(sptr, err_str(ERR_NOORIGIN), me.name, parv[0]);
		return 0;
	    }
	origin = parv[1];
	destination = parv[2]; /* Will get NULL or pointer (parc >= 2!!) */

	acptr = find_client(origin, NULL);
	if (!acptr)
		acptr = find_server(origin, NULL);
	if (acptr && acptr != sptr)
		origin = cptr->name;
	if (!BadPtr(destination) && mycmp(destination, me.name) != 0)
	    {
		if ((acptr = find_server(destination, NULL)))
			sendto_one(acptr,":%s PING %s :%s", parv[0],
				   origin, destination);
	    	else
		    {
			sendto_one(sptr, err_str(ERR_NOSUCHSERVER),
				   me.name, parv[0], destination);
			return 0;
		    }
	    }
	else
		sendto_one(sptr,":%s PONG %s :%s", me.name,
			   (destination) ? destination : me.name, origin);
	return 0;
    }

/*
** m_pong
**	parv[0] = sender prefix
**	parv[1] = origin
**	parv[2] = destination
*/
int	m_pong(cptr, sptr, parc, parv)
aClient *cptr, *sptr;
int	parc;
char	*parv[];
{
	aClient *acptr;
	char	*origin, *destination;

	if (MyClient(sptr))
	  return 0;

	/* Check to see if this is a PONG :cookie reply from an
	   unregistered user.  If so, process it. -record       */

	  if((!IsRegistered(sptr)) && (sptr->cookie!=0) &&
		(sptr->cookie!=-1) && (parc>1))
	  {
	      if(strtoul(parv[parc-1],NULL,10)==sptr->cookie)
	      {
                 sptr->cookie=-1;
                 if((sptr->user) && (sptr->name[0])) /* NICK and USER OK */
		   return register_user(cptr, sptr, sptr->name, 
		     sptr->user->username);
	       }
	       else
		   sendto_one(sptr,
		     ":%s %d %s :To connect, type /QUOTE PONG %lu",
                     me.name, ERR_BADPING, sptr->name,
		     sptr->cookie);

	       return 0;
	  }

	if (parc < 2 || *parv[1] == '\0')
	    {
		sendto_one(sptr, err_str(ERR_NOORIGIN), me.name, parv[0]);
		return 0;
	    }

	origin = parv[1];
	destination = parv[2];
	cptr->flags &= ~FLAGS_PINGSENT;
	sptr->flags &= ~FLAGS_PINGSENT;

	if (!BadPtr(destination) && mycmp(destination, me.name) != 0)
	    {
		if ((acptr = find_client(destination, NULL)) ||
		    (acptr = find_server(destination, NULL)))
			sendto_one(acptr,":%s PONG %s %s",
				   parv[0], origin, destination);
		else
		    {
			sendto_one(sptr, err_str(ERR_NOSUCHSERVER),
				   me.name, parv[0], destination);
			return 0;
		    }
	    }
#ifdef	DEBUGMODE
	else
		Debug((DEBUG_NOTICE, "PONG: %s %s", origin,
		      destination ? destination : "*"));
#endif
	return 0;
    }


/*
** m_oper
**	parv[0] = sender prefix
**	parv[1] = oper name
**	parv[2] = oper password
*/
int	m_oper(cptr, sptr, parc, parv)
aClient *cptr, *sptr;
int	parc;
char	*parv[];
    {
	aConfItem *aconf;
	char	*name, *password, *encr;
#ifdef CRYPT_OPER_PASSWORD
	char	salt[3];
	extern	char *crypt();
#endif /* CRYPT_OPER_PASSWORD */

	if (check_registered_user(sptr))
		return 0;

	name = parc > 1 ? parv[1] : NULL;
	password = parc > 2 ? parv[2] : NULL;

	if (!IsServer(cptr) && (BadPtr(name) || BadPtr(password)))
	    {
		sendto_one(sptr, err_str(ERR_NEEDMOREPARAMS),
			   me.name, parv[0], "OPER");
		return 0;
	    }
	
	/* if message arrived from server, trust it, and set to oper */
	    
	if ((IsServer(cptr) || IsMe(cptr)) && !IsOper(sptr))
	    {
		sptr->flags |= FLAGS_OPER;
		sendto_serv_butone(cptr, ":%s MODE %s :+o", parv[0], parv[0]);
		if (IsMe(cptr))
			sendto_one(sptr, rpl_str(RPL_YOUREOPER),
				   me.name, parv[0]);
#ifdef	USE_SERVICES
		check_services_butone(SERVICE_WANT_OPER, sptr,
				      ":%s MODE %s :+o", parv[0], parv[0]);
#endif
#ifdef NPATH
                note_oper(sptr);
#endif
		return 0;
	    }
	else if (IsOper(sptr))
	    {
		if (MyConnect(sptr))
			sendto_one(sptr, rpl_str(RPL_YOUREOPER),
				   me.name, parv[0]);
		return 0;
	    }
	if (!(aconf = find_conf_exact(name, sptr->username, sptr->sockhost,
				      CONF_OPS)) &&
	    !(aconf = find_conf_exact(name, sptr->username,
				      inetntoa((char *)&cptr->ip), CONF_OPS)))
	    {
		sendto_one(sptr, err_str(ERR_NOOPERHOST), me.name, parv[0]);
                sendto_realops("Failed OPER attempt by %s (%s@%s)",
                  parv[0], sptr->user->username, sptr->sockhost);
		return 0;
	    }
#ifdef CRYPT_OPER_PASSWORD
        /* use first two chars of the password they send in as salt */

        /* passwd may be NULL. Head it off at the pass... */
        salt[0] = '\0';
        if (password && aconf->passwd)
	    {
        	salt[0] = aconf->passwd[0];
		salt[1] = aconf->passwd[1];
		salt[2] = '\0';
		encr = crypt(password, salt);
	    }
	else
		encr = "";
#else
	encr = password;
#endif  /* CRYPT_OPER_PASSWORD */

	if ((aconf->status & CONF_OPS) &&
	    StrEq(encr, aconf->passwd) && !attach_conf(sptr, aconf))
	    {
		int old = (sptr->flags & ALL_UMODES);
		char *s;

		s = index(aconf->host, '@');
		*s++ = '\0';
#ifdef	OPER_REMOTE
		if (aconf->status == CONF_LOCOP) {
#else
		if ((matches(s,me.sockhost) && !IsLocal(sptr)) ||
		    aconf->status == CONF_LOCOP) {
#endif
			SetLocOp(sptr);
			ClearOper(sptr);
		} else { /* prevent someone from being both oper and local oper */
			SetOper(sptr);
			sptr->flags &= ~FLAGS_LOCOP;
		}
		*--s =  '@';
		sendto_ops("%s (%s@%s) is now operator (%c)", parv[0],
			   sptr->user->username, sptr->sockhost,
			   IsOper(sptr) ? 'O' : 'o');
		sptr->flags |= (FLAGS_SERVNOTICE|FLAGS_WALLOP);
		send_umode_out(cptr, sptr, old);
 		sendto_one(sptr, rpl_str(RPL_YOUREOPER), me.name, parv[0]);
#if !defined(CRYPT_OPER_PASSWORD) && (defined(FNAME_OPERLOG) ||\
    (defined(USE_SYSLOG) && defined(SYSLOG_OPER)))
		encr = "";
#endif
#if defined(USE_SYSLOG) && defined(SYSLOG_OPER)
		syslog(LOG_INFO, "OPER (%s) (%s) by (%s!%s@%s)",
			name, encr,
			parv[0], sptr->user->username, sptr->sockhost);
#endif
#ifdef FNAME_OPERLOG
	      {
                int     logfile;

                /*
                 * This conditional makes the logfile active only after
                 * it's been created - thus logging can be turned off by
                 * removing the file.
                 *
                 * stop NFS hangs...most systems should be able to open a
                 * file in 3 seconds. -avalon (curtesy of wumpus)
                 */
                (void)alarm(3);
                if (IsPerson(sptr) &&
                    (logfile = open(FNAME_OPERLOG, O_WRONLY|O_APPEND)) != -1)
		{
		  (void)alarm(0);
                        (void)sprintf(buf, "%s OPER (%s) (%s) by (%s!%s@%s)\n",
				      myctime(now), name, encr,
				      parv[0], sptr->user->username,
				      sptr->sockhost);
		  (void)alarm(3);
		  (void)write(logfile, buf, strlen(buf));
		  (void)alarm(0);
		  (void)close(logfile);
		}
                (void)alarm(0);
                /* Modification by pjg */
	      }
#endif
#ifdef	USE_SERVICES
		check_services_butone(SERVICE_WANT_OPER, sptr,
				      ":%s MODE %s :+o", parv[0], parv[0]);
#endif
#ifdef NPATH
                note_oper(sptr);
#endif
	    }
	else
	    {
		(void)detach_conf(sptr, aconf);
		sendto_one(sptr,err_str(ERR_PASSWDMISMATCH),me.name, parv[0]);
                  sendto_realops("Failed OPER attempt by %s (%s@%s)",
                   parv[0], sptr->user->username, sptr->sockhost);
	    }
	return 0;
    }

/***************************************************************************
 * m_pass() - Added Sat, 4 March 1989
 ***************************************************************************/

/*
** m_pass
**	parv[0] = sender prefix
**	parv[1] = password
*/
int	m_pass(cptr, sptr, parc, parv)
aClient *cptr, *sptr;
int	parc;
char	*parv[];
    {
	char *password = parc > 1 ? parv[1] : NULL;

	if (BadPtr(password))
	    {
		sendto_one(cptr, err_str(ERR_NEEDMOREPARAMS),
			   me.name, parv[0], "PASS");
		return 0;
	    }
	if (!MyConnect(sptr) || (!IsUnknown(cptr) && !IsHandshake(cptr)))
	    {
		sendto_one(cptr, err_str(ERR_ALREADYREGISTRED),
			   me.name, parv[0]);
		return 0;
	    }
	strncpyzt(cptr->passwd, password, sizeof(cptr->passwd));
	return 0;
    }

/*
 * m_userhost added by Darren Reed 13/8/91 to aid clients and reduce
 * the need for complicated requests like WHOIS. It returns user/host
 * information only (no spurious AWAY labels or channels).
 */
int	m_userhost(cptr, sptr, parc, parv)
aClient *cptr, *sptr;
int	parc;
char	*parv[];
{
	char	*p = NULL;
	aClient	*acptr;
	Reg1	char	*s;
	Reg2	int	i, len;

	if (check_registered(sptr))
		return 0;

	if (parc > 2)
		(void)m_userhost(cptr, sptr, parc-1, parv+1);

	if (parc < 2)
	    {
		sendto_one(sptr, err_str(ERR_NEEDMOREPARAMS),
			   me.name, parv[0], "USERHOST");
		return 0;
	    }

	(void)sprintf(buf, rpl_str(RPL_USERHOST), me.name, parv[0]);
	len = strlen(buf);
	*buf2 = '\0';

	for (i = 5, s = strtoken(&p, parv[1], " "); i && s;
	     s = strtoken(&p, (char *)NULL, " "), i--)
		if ((acptr = find_person(s, NULL)))
		    {
			if (*buf2)
				(void)strcat(buf, " ");
			(void)sprintf(buf2, "%s%s=%c%s@%s",
				acptr->name,
				IsAnOper(acptr) ? "*" : "",
				(acptr->user->away) ? '-' : '+',
				acptr->user->username,
				acptr->user->host);
			(void)strncat(buf, buf2, sizeof(buf) - len);
			len += strlen(buf2);
		    }
	sendto_one(sptr, "%s", buf);
	return 0;
}

/*
 * m_ison added by Darren Reed 13/8/91 to act as an efficent user indicator
 * with respect to cpu/bandwidth used. Implemented for NOTIFY feature in
 * clients. Designed to reduce number of whois requests. Can process
 * nicknames in batches as long as the maximum buffer length.
 *
 * format:
 * ISON :nicklist
 */

int	m_ison(cptr, sptr, parc, parv)
aClient *cptr, *sptr;
int	parc;
char	*parv[];
{
	Reg1	aClient *acptr;
	Reg2	char	*s, **pav = parv;
	Reg3	int	len;
	char	*p = NULL;

	if (check_registered(sptr))
		return 0;

	if (parc < 2)
	    {
		sendto_one(sptr, err_str(ERR_NEEDMOREPARAMS),
			   me.name, parv[0], "ISON");
		return 0;
	    }

	(void)sprintf(buf, rpl_str(RPL_ISON), me.name, *parv);
	len = strlen(buf);

	for (s = strtoken(&p, *++pav, " "); s; s = strtoken(&p, NULL, " "))
		if ((acptr = find_person(s, NULL)))
		    {
			(void)strncat(buf, acptr->name, sizeof(buf) - len);
			len += strlen(acptr->name);
			(void)strncat(buf, " ", sizeof(buf) - len);
			len++;
		    }
	sendto_one(sptr, "%s", buf);
	return 0;
}

#if !defined(NPATH)
int	m_note(cptr, sptr, parc, parv)
aClient	*cptr, *sptr;
int	parc;
char	*parv[];
{
	Reg1	aClient *acptr;
	Reg2	int	i = 0;
	int	wilds = 0;
	char	*c, nbuf[50];

	if (parc < 2)
		return 0;

	c = parv[1];

	while (*c && *c != ' ' && i < 49)
	    {
        	if (*c == '*' || *c == '?')
			wilds = 1;
	  	nbuf[i++] = *c++;
	    }

	nbuf[i] = 0;

	if (wilds && (IsOper(sptr) || IsServer(sptr)))
	  sendto_serv_butone(cptr, ":%s NOTE :%s", parv[0], parv[1]);
	else
		for (acptr = client; acptr; acptr = acptr->next)
			if (IsServer(acptr) && acptr != cptr
			    && !mycmp(nbuf, acptr->name))
			    {
				sendto_one(acptr, ":%s NOTE :%s",
					   parv[0], parv[1]);
				break;
			    }
	return 0;
}
#endif

static int user_modes[]	     = { FLAGS_OPER, 'o',
				 FLAGS_LOCOP, 'O',
				 FLAGS_INVISIBLE, 'i',
				 FLAGS_WALLOP, 'w',
				 FLAGS_SERVNOTICE, 's',
				 FLAGS_DEAF, 'd',
				 FLAGS_NOKICK, 'k',
				 0, 0 };

/*
 * m_umode() added 15/10/91 By Darren Reed.
 * parv[0] - sender
 * parv[1] - username to change mode for
 * parv[2] - modes to change
 */
int	m_umode(cptr, sptr, parc, parv)
aClient *cptr, *sptr;
int	parc;
char	*parv[];
{
	Reg1	int	flag;
	Reg2	int	*s;
	Reg3	char	**p, *m;
	aClient	*acptr;
	int	what, setflags;

	if (check_registered_user(sptr))
		return 0;

	what = MODE_ADD;

	if (parc < 2)
	    {
		sendto_one(sptr, err_str(ERR_NEEDMOREPARAMS),
			   me.name, parv[0], "MODE");
		return 0;
	    }

	if (!(acptr = find_person(parv[1], NULL)))
	    {
		if (MyConnect(sptr))
			sendto_one(sptr, err_str(ERR_NOSUCHCHANNEL),
				   me.name, parv[0], parv[1]);
		return 0;
	    }

	if (IsServer(sptr) || sptr != acptr)
	    {
		if (IsServer(cptr))
			sendto_ops_butone(NULL, &me,
				  ":%s WALLOPS :MODE for User %s From %s!%s",
				  me.name, parv[1],
				  get_client_name(cptr, FALSE), sptr->name);
		else
			sendto_one(sptr, err_str(ERR_USERSDONTMATCH),
				   me.name, parv[0]);
			return 0;
	    }
 
	if (parc < 3)
	    {
		m = buf;
		*m++ = '+';
		for (s = user_modes; (flag = *s) && (m - buf < BUFSIZE - 4);
		     s += 2)
			if (sptr->flags & flag)
				*m++ = (char)(*(s+1));
		*m = '\0';
		sendto_one(sptr, rpl_str(RPL_UMODEIS),
			   me.name, parv[0], buf);
		return 0;
	    }

	/* find flags already set for user */
	setflags = 0;
	for (s = user_modes; (flag = *s); s += 2)
		if (sptr->flags & flag)
			setflags |= flag;

	/*
	 * parse mode change string(s)
	 */
	for (p = &parv[2]; p && *p; p++ )
		for (m = *p; *m; m++)
			switch(*m)
			{
			case '+' :
				what = MODE_ADD;
				break;
			case '-' :
				what = MODE_DEL;
				break;	
			/* we may not get these,
			 * but they shouldnt be in default
			 */
			case ' ' :
			case '\n' :
			case '\r' :
			case '\t' :
				break;
			default :
				for (s = user_modes; (flag = *s); s += 2)
					if (*m == (char)(*(s+1)))
				    {
					if (what == MODE_ADD)
						sptr->flags |= flag;
					else if ((flag&(FLAGS_OPER|FLAGS_LOCOP)))
					  sptr->flags &= ~(FLAGS_OPER|FLAGS_LOCOP);
					/* allow either -o or -O to reset all
					   operator status's... */
					else
					  sptr->flags &= ~flag;	
					break;
				    }
				if (flag == 0 && MyConnect(sptr))
					sendto_one(sptr,
						err_str(ERR_UMODEUNKNOWNFLAG),
						me.name, parv[0]);
				break;
			}
	/*
	 * stop users making themselves operators too easily
	 */
	if (!(setflags & FLAGS_OPER) && IsOper(sptr) && !IsServer(cptr))
		ClearOper(sptr);
	if (!(setflags & FLAGS_LOCOP) && IsLocOp(sptr) && !IsServer(cptr))
		sptr->flags &= ~FLAGS_LOCOP;
	if ((setflags & (FLAGS_OPER|FLAGS_LOCOP)) && !IsAnOper(sptr) &&
	    MyConnect(sptr))
		det_confs_butmask(sptr, CONF_CLIENT & ~CONF_OPS);
	/* new umode; servers can set it, local users cannot;
	   prevents users from /kick'ing or /mode -o'ing */
	if (!(setflags & FLAGS_NOKICK) && !IsServer(cptr))
	  sptr->flags &= ~FLAGS_NOKICK;
#ifdef	USE_SERVICES
	if (IsOper(sptr) && !(setflags & FLAGS_OPER))
		check_services_butone(SERVICE_WANT_OPER, sptr,
				      ":%s MODE %s :+o", parv[0], parv[0]);
	else if (!IsOper(sptr) && (setflags & FLAGS_OPER))
		check_services_butone(SERVICE_WANT_OPER, sptr,
				      ":%s MODE %s :-o", parv[0], parv[0]);
#endif
#ifdef NPATH
        if (IsOper(sptr) && !(setflags & FLAGS_OPER)) note_oper(sptr);
#endif
	/*
	 * compare new flags with old flags and send string which
	 * will cause servers to update correctly.
	 */
	send_umode_out(cptr, sptr, setflags);

	return 0;
}
	
/*
 * send the MODE string for user (user) to connection cptr
 * -avalon
 */
void	send_umode(cptr, sptr, old, sendmask, umode_buf)
aClient *cptr, *sptr;
int	old, sendmask;
char	*umode_buf;
{
	Reg1	int	*s, flag;
	Reg2	char	*m;
	int	what = MODE_NULL;

	/*
	 * build a string in umode_buf to represent the change in the user's
	 * mode between the new (sptr->flag) and 'old'.
	 */
	m = umode_buf;
	*m = '\0';
	for (s = user_modes; (flag = *s); s += 2)
	    {
		if (MyClient(sptr) && !(flag & sendmask))
			continue;
		if ((flag & old) && !(sptr->flags & flag))
		    {
			if (what == MODE_DEL)
				*m++ = *(s+1);
			else
			    {
				what = MODE_DEL;
				*m++ = '-';
				*m++ = *(s+1);
			    }
		    }
		else if (!(flag & old) && (sptr->flags & flag))
		    {
			if (what == MODE_ADD)
				*m++ = *(s+1);
			else
			    {
				what = MODE_ADD;
				*m++ = '+';
				*m++ = *(s+1);
			    }
		    }
	    }
	*m = '\0';
	if (*umode_buf && cptr)
		sendto_one(cptr, ":%s MODE %s :%s",
			   sptr->name, sptr->name, umode_buf);
}

/*
 * added Sat Jul 25 07:30:42 EST 1992
 */
void	send_umode_out(cptr, sptr, old)
aClient *cptr, *sptr;
int	old;
{
	Reg1    int     i;
	Reg2    aClient *acptr;

	send_umode(NULL, sptr, old, SEND_UMODES, buf);

	for (i = highest_fd; i >= 0; i--)
		if ((acptr = local[i]) && IsServer(acptr) &&
		    (acptr != cptr) && (acptr != sptr) && *buf)
			sendto_one(acptr, ":%s MODE %s :%s",
				   sptr->name, sptr->name, buf);

	if (cptr && MyClient(cptr))
		send_umode(cptr, sptr, old, ALL_UMODES, buf);
}

/***********************************************************************
 * m_silence() - Added 19 May 1994 by Run. 
 *
 ***********************************************************************/

/*
 * is_silenced : Does the actual check wether sptr is allowed
 *               to send a message to acptr.
 *               Both must be registered persons.
 * If sptr is silenced by acptr, his message should not be propagated,
 * but more over, if this is detected on a server not local to sptr
 * the SILENCE mask is sent upstream.
 */
int is_silenced(sptr, acptr)
aClient *sptr;
aClient *acptr;
{ Reg1 Link *lp;
  Reg2 anUser *user;
  static char sender[HOSTLEN+NICKLEN+USERLEN+5];

  if (!(acptr->user) || !(lp = acptr->user->silence) ||
      !(user = sptr->user)) return 0;
  sprintf(sender,"%s!%s@%s",sptr->name,user->username,user->host);
  for (; lp; lp = lp->next)
  { if (!matches(lp->value.cp, sender))
    { if (!MyConnect(sptr))
      { sendto_one(sptr->from, ":%s SILENCE %s :%s",acptr->name,
            sptr->name, lp->value.cp);
        lp->flags=1; }
      return 1; } }
  return 0;
}

int del_silence(sptr, mask)
aClient *sptr;
char *mask;
{ Reg1 Link **lp;
  Reg2 Link *tmp;

  for (lp = &(sptr->user->silence); *lp; lp = &((*lp)->next))
    if (mycmp(mask, (*lp)->value.cp)==0)
    { tmp = *lp;
      *lp = tmp->next;
      MyFree(tmp->value.cp);
      free_link(tmp);
      return 0; }
  return -1;
}

static int add_silence(sptr, mask)
aClient *sptr;
char *mask;
{ Reg1 Link *lp;
  Reg2 int cnt = 0, len = 0;

  for (lp = sptr->user->silence; lp; lp = lp->next)
  { len += strlen(lp->value.cp);
    if (MyClient(sptr))
      if ((len > MAXSILELENGTH) || (++cnt >= MAXSILES))
      { sendto_one(sptr, err_str(ERR_SILELISTFULL), me.name, sptr->name, mask);
	return -1; }
      else
      { if (!matches(lp->value.cp, mask))
	  return -1; }
    else if (!mycmp(lp->value.cp, mask))
      return -1;
  }
  lp = make_link();
  bzero((char *)lp, sizeof(Link));
  lp->next = sptr->user->silence;
  lp->value.cp = (char *)MyMalloc(strlen(mask)+1);
  (void)strcpy(lp->value.cp, mask);
  sptr->user->silence = lp;
  return 0;
}

/*
** m_silence
**	parv[0] = sender prefix
** From local client:
**	parv[1] = mask (NULL sends the list)
** From remote client:
**	parv[1] = nick that must be silenced
**      parv[2] = mask
*/
int m_silence(cptr, sptr, parc, parv)
aClient	*cptr, *sptr;
int	parc;
char	*parv[];
{
  Link *lp;
  aClient *acptr;
  char c, *cp, *user, *host;

  if (check_registered_user(sptr)) return 0;

  if (MyClient(sptr))
  {
    acptr = sptr;
    if (parc < 2 || *parv[1]=='\0' || (acptr = find_person(parv[1], NULL)))
    { if (!(acptr->user)) return 0;
      for (lp = acptr->user->silence; lp; lp = lp->next)
	sendto_one(sptr, rpl_str(RPL_SILELIST), me.name,
	    sptr->name, acptr->name, lp->value.cp);
      sendto_one(sptr, rpl_str(RPL_ENDOFSILELIST), me.name, acptr->name);
      return 0; }
    cp = parv[1];
    c = *cp;
    if (c=='-' || c=='+') cp++;
    else if (!(index(cp, '@') || index(cp, '.') ||
	index(cp, '!') || index(cp, '*')))
    { sendto_one(sptr, err_str(ERR_NOSUCHNICK), me.name, parv[0], parv[1]);
      return -1; }
    else c = '+';
    cp = pretty_mask(cp);
    if ((c=='-' && !del_silence(sptr,cp)) ||
        (c!='-' && !add_silence(sptr,cp)))
    { sendto_prefix_one(sptr, sptr, ":%s SILENCE %c%s", parv[0], c, cp);
      if (c=='-')
	sendto_serv_butone(NULL, ":%s SILENCE * -%s", sptr->name, cp);
    }
  }
  else if (parc < 3 || *parv[2]=='\0')
  {
    sendto_one(sptr, err_str(ERR_NEEDMOREPARAMS), me.name, parv[0], "SILENCE");
    return -1;
  }
  else
  {
    acptr = find_person(parv[1], NULL); /* In case of NOTE notice, parv[1]
    					   can be a server */
    if (*parv[2]=='-')
    { if (!del_silence(sptr,parv[2]+1))
	sendto_serv_butone(cptr, ":%s SILENCE %s :%s",
	    parv[0], parv[1], parv[2]); }
    else
    { (void)add_silence(sptr,parv[2]);
      if (acptr && !MyClient(acptr))
        sendto_one(acptr, ":%s SILENCE %s :%s",
            parv[0], parv[1], parv[2]); }
  }
  return 0;
}
