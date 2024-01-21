/************************************************************************
 *   IRC - Internet Relay Chat, ircd/s_misc.c (formerly ircd/date.c)
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
static  char sccsid[] = "@(#)s_misc.c	2.42 3/1/94 (C) 1988 University of Oulu, \
Computing Center and Jarkko Oikarinen";
#endif

#include <sys/time.h>
#include "struct.h"
#include "common.h"
#include "sys.h"
#include "numeric.h"
#include "userload.h"
#include <sys/stat.h>
#include <fcntl.h>
#if !defined(ULTRIX) && !defined(SGI) && !defined(sequent) && \
    !defined(__convex__)
# include <sys/param.h>
#endif
#if defined(PCS) || defined(AIX) || defined(SVR3)
# include <time.h>
#endif
#ifdef HPUX
#include <unistd.h>
#endif
#ifdef DYNIXPTX
#include <sys/types.h>
#include <time.h>
#endif
#include "h.h"

static	void	exit_one_client PROTO((aClient *,aClient *,char *,int));

static	char	*months[] = {
	"January",	"February",	"March",	"April",
	"May",	        "June",	        "July",	        "August",
	"September",	"October",	"November",	"December"
};

static	char	*weekdays[] = {
	"Sunday",	"Monday",	"Tuesday",	"Wednesday",
	"Thursday",	"Friday",	"Saturday"
};

/*
 * stats stuff
 */
struct	stats	ircst, *ircstp = &ircst;

char	*date(clock) 
time_t	clock;
{
	static	char	buf[80], plus;
	Reg1	struct	tm *lt, *gm;
	struct	tm	gmbuf;
	int	minswest;

	if (!clock) 
		clock = now;
	gm = gmtime(&clock);
	bcopy((char *)gm, (char *)&gmbuf, sizeof(gmbuf));
	gm = &gmbuf;
	lt = localtime(&clock);

	if (lt->tm_yday == gm->tm_yday)
		minswest = (gm->tm_hour - lt->tm_hour) * 60 +
			   (gm->tm_min - lt->tm_min);
	else if (lt->tm_yday > gm->tm_yday)
		minswest = (gm->tm_hour - (lt->tm_hour + 24)) * 60;
	else
		minswest = ((gm->tm_hour + 24) - lt->tm_hour) * 60;

	plus = (minswest > 0) ? '-' : '+';
	if (minswest < 0)
		minswest = -minswest;

	(void)sprintf(buf, "%s %s %d 19%02d -- %02d:%02d %c%02d:%02d",
		weekdays[lt->tm_wday], months[lt->tm_mon],lt->tm_mday,
		lt->tm_year, lt->tm_hour, lt->tm_min,
		plus, minswest/60, minswest%60);

	return buf;
}

/**
 ** myctime()
 **   This is like standard ctime()-function, but it zaps away
 **   the newline from the end of that string. Also, it takes
 **   the time value as parameter, instead of pointer to it.
 **   Note that it is necessary to copy the string to alternate
 **   buffer (who knows how ctime() implements it, maybe it statically
 **   has newline there and never 'refreshes' it -- zapping that
 **   might break things in other places...)
 **
 **/

char	*myctime(value)
time_t	value;
{
	static	char	buf[28];
	Reg1	char	*p;

	(void)strcpy(buf, ctime(&value));
	if ((p = (char *)index(buf, '\n')) != NULL)
		*p = '\0';

	return buf;
}

/*
** check_registered_user is used to cancel message, if the
** originator is a server or not registered yet. In other
** words, passing this test, *MUST* guarantee that the
** sptr->user exists (not checked after this--let there
** be coredumps to catch bugs... this is intentional --msa ;)
**
** There is this nagging feeling... should this NOT_REGISTERED
** error really be sent to remote users? This happening means
** that remote servers have this user registered, althout this
** one has it not... Not really users fault... Perhaps this
** error message should be restricted to local clients and some
** other thing generated for remotes...
*/
int	check_registered_user(sptr)
aClient	*sptr;
{
	if (!IsRegisteredUser(sptr))
	    {
		sendto_one(sptr, err_str(ERR_NOTREGISTERED), me.name, "*");
		return -1;
	    }
	return 0;
}

/*
** check_registered user cancels message, if 'x' is not
** registered (e.g. we don't know yet whether a server
** or user)
*/
int	check_registered(sptr)
aClient	*sptr;
{
	if (!IsRegistered(sptr))
	    {
		sendto_one(sptr, err_str(ERR_NOTREGISTERED), me.name, "*");
		return -1;
	    }
	return 0;
}

/*
** get_client_name
**      Return the name of the client for various tracking and
**      admin purposes. The main purpose of this function is to
**      return the "socket host" name of the client, if that
**	differs from the advertised name (other than case).
**	But, this can be used to any client structure.
**
**	Returns:
**	  "name[user@ip#.port]" if 'showip' is true;
**	  "name[sockethost]", if name and sockhost are different and
**	  showip is false; else
**	  "name".
**
** NOTE 1:
**	Watch out the allocation of "nbuf", if either sptr->name
**	or sptr->sockhost gets changed into pointers instead of
**	directly allocated within the structure...
**
** NOTE 2:
**	Function return either a pointer to the structure (sptr) or
**	to internal buffer (nbuf). *NEVER* use the returned pointer
**	to modify what it points!!!
*/
char	*get_client_name(sptr, showip)
aClient *sptr;
int	showip;
{
	static char nbuf[HOSTLEN * 2 + USERLEN + 5];

	if (MyConnect(sptr))
	    {
		if (IsUnixSocket(sptr))
		    {
			if (showip)
				(void) sprintf(nbuf, "%s[%s]",
					sptr->name, sptr->sockhost);
			else
				(void) sprintf(nbuf, "%s[%s]",
					sptr->name, me.sockhost);
		    }
		else
		    {
			if (showip)
				(void)sprintf(nbuf, "%s[%s@%s]",
					sptr->name,
					(!(sptr->flags & FLAGS_GOTID)) ? "" :
					sptr->username,
					inetntoa((char *)&sptr->ip));
			else
			    {
				if (mycmp(sptr->name, sptr->sockhost))
					(void)sprintf(nbuf, "%s[%s]",
						sptr->name, sptr->sockhost);
				else
					return sptr->name;
			    }
		    }
		return nbuf;
	    }
	return sptr->name;
}

char	*get_client_host(cptr)
aClient	*cptr;
{
	static char nbuf[HOSTLEN * 2 + USERLEN + 5];

	if (!MyConnect(cptr))
		return cptr->name;
	if (!cptr->hostp)
		return get_client_name(cptr, FALSE);
	if (IsUnixSocket(cptr))
		(void) sprintf(nbuf, "%s[%s]", cptr->name, me.name);
	else
		(void)sprintf(nbuf, "%s[%-.*s@%-.*s]",
			cptr->name, USERLEN,
			(!(cptr->flags & FLAGS_GOTID)) ? "" : cptr->username,
			HOSTLEN, cptr->hostp->h_name);
	return nbuf;
}

/*
 * Form sockhost such that if the host is of form user@host, only the host
 * portion is copied.
 */
void	get_sockhost(cptr, host)
Reg1	aClient	*cptr;
Reg2	char	*host;
{
	Reg3	char	*s;
	if ((s = (char *)index(host, '@')))
		s++;
	else
		s = host;
	strncpyzt(cptr->sockhost, s, sizeof(cptr->sockhost));
}

/*
 * Return wildcard name of my server name according to given config entry
 * --Jto
 */
char	*my_name_for_link(name, aconf)
char	*name;
aConfItem *aconf;
{
	static	char	namebuf[HOSTLEN];
	register int	count = aconf->port;
	register char	*start = name;

	if (count <= 0 || count > 5)
		return start;

	while (count-- && name)
	    {
		name++;
		name = (char *)index(name, '.');
	    }
	if (!name)
		return start;

	namebuf[0] = '*';
	(void)strncpy(&namebuf[1], name, HOSTLEN - 1);
	namebuf[HOSTLEN - 1] = '\0';

	return namebuf;
}

/*
** exit_downlinks - added by Run 25-9-94
**
** Removes all clients and downlinks (+clients) of any server
** QUITs are generated and sent to local users.
**
** cptr    : server that must have all dependents removed
** sptr    : source who thought that this was a good idea
** comment : comment sent as sign off message to local clients
*/
void exit_downlinks(cptr, sptr, comment)
aClient *cptr, *sptr;
char *comment;
{
  Reg1 aClient *acptr;
  Reg2 Dlink *next;
  Reg3 Dlink *lp;

  /* Run over all its downlinks */
  for (lp=cptr->serv->down; lp; lp=next)
  {
    next=lp->next;
    acptr=lp->value.cptr;
    /* Remove the downlinks and client of the downlink */
    exit_downlinks(acptr, sptr, comment);
    /* Remove the downlink itself */
    exit_one_client(acptr, sptr, me.name, 1);
  }
  /* Remove all clients of this server */
  for (lp=cptr->serv->client; lp; lp=next)
  {
    next=lp->next;
    exit_one_client(lp->value.cptr, sptr, comment, 1);
  }
}

/*
** exit_client, rewritten 25-9-94 by Run
**
** This function exits a client of *any* type (user, server, etc)
** from this server. Also, this generates all necessary prototol
** messages that this exit may cause.
**
** This function implicitly exits all other clients depending on
** this connection.
**
** For convenience, this function returns a suitable value for
** m_funtion return value:
**
**	CPTR_KILLED	if (cptr == bcptr)
**	0		if (cptr != bcptr)
**
** This function can be called in two ways:
** 1) From before or in parse(), exitting the 'cptr', in which case it was
**    invoked as exit_client(cptr, cptr, &me,...), causing it to always
**    return CPTR_KILLED.
** 2) Via parse from a m_function call, in which case it was invoked as
**    exit_client(cptr, acptr, sptr, ...). Here 'sptr' is known; the client
**    that generated the message in a way that we can assume he already
**    did remove acptr from memory himself (or in other cases we don't mind
**    because he will be delinked.) Or invoked as:
**    exit_client(cptr, acptr/sptr, &me, ...) when WE decide this one should
**    be removed.
** In general: No generated SQUIT or QUIT should be sent to source link
** sptr->from. And CPTR_KILLED should be returned if cptr got removed (too).
**
** --Run
*/
int	exit_client(cptr, bcptr, sptr, comment)
aClient *cptr;		/* Connection being handled by read_message right now */
aClient *bcptr;		/* Client being killed */
aClient *sptr;		/* The client that made the decision to remove this
			   one, never NULL */
char	*comment;	/* Reason for the exit */
{
  Reg1	aClient	*acptr;
  Reg2	aClient	*next;
  Reg3	Dlink	*dlp;
#ifdef	FNAME_USERLOG
  time_t	on_for;
#endif
  char	comment1[HOSTLEN + HOSTLEN + 2];

  if (MyConnect(bcptr))
  {
    bcptr->flags |= FLAGS_CLOSING;
    current_load_data.conn_count--;
    if (IsPerson(bcptr))
    {
      char mydom_mask[HOSTLEN + 1];
      mydom_mask[0] = '*';
      sendto_realops ("Client exiting: %s (%s@%s) [%s]", 
          bcptr->name, bcptr->user->username, bcptr->user->host, comment);
      strncpy(&mydom_mask[1], DOMAINNAME, HOSTLEN - 1);
      current_load_data.client_count--;
      if (matches(mydom_mask, bcptr->sockhost) == 0)
        current_load_data.local_count--;
    }
    update_load();
#ifdef FNAME_USERLOG
    on_for = now - bcptr->firsttime;
# if defined(USE_SYSLOG) && defined(SYSLOG_USERS)
    if (IsPerson(bcptr))
      syslog(LOG_NOTICE, "%s (%3d:%02d:%02d): %s@%s (%s)\n",
          myctime(bcptr->firsttime), on_for / 3600, (on_for % 3600)/60,
          on_for % 60, bcptr->user->username, bcptr->sockhost, bcptr->name);
# else
    {
      char	linebuf[160];
      int	logfile;

      /*
       * This conditional makes the logfile active only after
       * it's been created - thus logging can be turned off by
       * removing the file.
       *
       * stop NFS hangs...most systems should be able to open a
       * file in 3 seconds. -avalon (curtesy of wumpus)
       */
      (void)alarm(3);
      if (IsPerson(bcptr) &&
	  (logfile = open(FNAME_USERLOG, O_WRONLY|O_APPEND)) != -1)
      {
	  (void)alarm(0);
	  (void)sprintf(linebuf,
		  "%s (%3d:%02d:%02d): %s@%s [%s]\n",
		  myctime(bcptr->firsttime),
		  on_for / 3600, (on_for % 3600)/60,
		  on_for % 60,
		  bcptr->user->username, bcptr->user->host,
		  bcptr->username);
	  (void)alarm(3);
	  (void)write(logfile, linebuf, strlen(linebuf));
	  (void)alarm(0);
	  (void)close(logfile);
      }
      (void)alarm(0);
      /* Modification by stealth@caen.engin.umich.edu */
    }
# endif
#endif
    if (bcptr != sptr->from && bcptr->status >= STAT_MASTER)
 					 /* the source knows already --Run */
    {
      if (IsServer(bcptr) || IsHandshake(bcptr))
      {
	if (IsServer(bcptr) && Protocol(bcptr->from)<9)
	  sendto_one(bcptr, ":%s SQUIT %s :%s", sptr->name,
	      me.name, comment);
	else /* If we are handshaking, use the newest protocol :/ */
	  sendto_one(bcptr, ":%s SQUIT %s 0 :%s", sptr->name,
	      me.name, comment);
      }
      else if (!IsConnecting(bcptr))
	sendto_one(bcptr, "ERROR :Closing Link: %s by %s (%s)",
	    get_client_name(bcptr,FALSE), sptr->name, comment);
      if ((IsServer(bcptr) || IsHandshake(bcptr) || IsConnecting(bcptr)) &&
          (sptr == &me || (IsServer(sptr) &&
          (strncmp(comment, "Leaf-only link", 14) ||
           strncmp(comment, "Non-Hub link", 12)))))
      {
        if (bcptr->serv->user && (acptr=find_client(bcptr->serv->by, NULL)) &&
            acptr->user == bcptr->serv->user)
          sendto_one(acptr,
              ":%s NOTICE %s :Link with %s cancelled: %s",
              me.name, acptr->name, bcptr->name, comment);
        else
          acptr = NULL;
        if (sptr == &me)
	  sendto_lops_butone(acptr, "Link with %s cancelled: %s",
	      bcptr->name, comment);
      }
    }
    /*
    ** Close the Client connection first.
    */
    close_connection(bcptr);
  }

  if (IsServer(bcptr))
  {
    (void)strcpy(comment1, bcptr->serv->up->name);
    (void)strcat(comment1," ");
    (void)strcat(comment1, bcptr->name);
    if (!IsServer(sptr) || Protocol(sptr)>4)
    {
      if (IsPerson(sptr))
        sendto_lops_butone(sptr, "%s SQUIT by %s [%s]:",
            (sptr->user->server == bcptr ||
            sptr->user->server == bcptr->serv->up) ? "Local" : "Remote",
            get_client_name(sptr,TRUE), sptr->user->server->name);
      else if (sptr != &me && bcptr->serv->up != sptr)
        sendto_ops("Received SQUIT %s from %s :", bcptr->name,
            IsServer(sptr) ? sptr->name : get_client_name(sptr,TRUE));
      sendto_ops("Net break: %s (%s)", comment1, comment);
    }
    exit_downlinks(bcptr, sptr, comment1);
  }

  /*
  ** Now generate the needed protocol for the other server links
  ** except the source:				USE_SERVICES not supported
  */
  for (dlp = me.serv->down; dlp; dlp = dlp->next)
    if (dlp->value.cptr != sptr->from &&
        dlp->value.cptr != bcptr)
    {
      if (IsServer(bcptr))
	sendto_one(dlp->value.cptr, ":%s SQUIT %s %lu :%s",
	    sptr->name, bcptr->name, bcptr->serv->timestamp, comment);
      else if (IsClient(bcptr) && (bcptr->flags & FLAGS_KILLED) == 0)
	sendto_one(dlp->value.cptr, ":%s QUIT :%s", bcptr->name, comment);
    }

  exit_one_client(bcptr, sptr, comment, 0);

  /*
  ** cptr can only have been killed if it was cptr itself that got killed here,
  ** because cptr can never have been a dependant of bcptr    --Run
  */
  return (cptr == bcptr) ? CPTR_KILLED : 0;
}

/*
** Exit client with formatted message, added 25-9-94 by Run
*/
#ifndef USE_VARARGS
/*VARARGS*/
int exit_client_msg(cptr, bcptr, sptr, pattern, p1, p2, p3, p4, p5, p6)
aClient *cptr, *bcptr, *sptr;
char    *pattern, *p1, *p2, *p3, *p4, *p5, *p6;
{
#else
int exit_client_msg(cptr, bcptr, sptr, pattern, va_alist)
aClient *cptr, *bcptr, *sptr;
char    *pattern;
va_dcl
{
  va_list vl;
#endif
  char msgbuf[1024];

#ifdef  USE_VARARGS
  va_start(vl);
  (void)vsprintf(msgbuf, pattern, vl);
  va_end(vl);
#else
  (void)sprintf(msgbuf, pattern, p1, p2, p3, p4, p5, p6);
#endif
  return exit_client(cptr, bcptr, sptr, msgbuf);
}

/*
** Exit one client, local or remote. Assuming for local client that
** all dependants already have been removed, and socket is closed.
**
** Rewritten by Run - 24 sept 94
**
** bcptr : client being (s)quitted
** sptr : The source (prefix) of the QUIT or SQUIT
**
** --Run
*/
static	void	exit_one_client(bcptr, sptr, comment, OldProtocolFlag)
aClient *bcptr, *sptr;
char *comment;
int OldProtocolFlag;
{
  Reg1	aClient *acptr;
  Reg2	int	i;
  Reg3	Link	*lp;
  Reg4	Link	*prev;
  Reg5	Dlink	*dlp;

  if (IsClient(bcptr))
  {
    /* Stop a running /LIST clean */
    if (MyClient(bcptr) && bcptr->listing)
    {
      bcptr->listing->mode.mode &= ~MODE_LISTED;
      bcptr->listing = NULL;
    }

    if (AskedPing(bcptr))
	cancel_ping(bcptr, NULL);
    /*
    ** If this exit is generated from "m_kill", then there
    ** is no sense in sending the QUIT--KILL's have been
    ** sent instead.
    */
/* Old Protocol : */
    if ((bcptr->flags & FLAGS_KILLED) == 0)
    {
      for (dlp = me.serv->down; dlp; dlp = dlp->next)
	if (Protocol(dlp->value.cptr)<9 && bcptr->from != dlp->value.cptr)
          sendto_one(dlp->value.cptr,":%s QUIT :%s", bcptr->name, comment);
    }
    /*
    ** If a person is on a channel, send a QUIT notice
    ** to every client (person) on the same channel (so
    ** that the client can show the "**signoff" message).
    ** (Note: The notice is to the local clients *only*)
    */
    sendto_common_channels(bcptr, ":%s QUIT :%s", bcptr->name, comment);

#ifdef NPATH
    note_signoff(bcptr);
#endif

    while ((lp = bcptr->user->channel))
      remove_user_from_channel(bcptr,lp->value.chptr);

    /* Clean up invitefield */
    while ((lp = bcptr->user->invited))
      del_invite(bcptr, lp->value.chptr);

    /* Clean up silencefield */
    while ((lp = bcptr->user->silence))
      (void)del_silence(bcptr, lp->value.cp);

    /* Remove from serv->client list */
    remove_dlink(&bcptr->user->server->serv->client, bcptr->user->clink);
  }
  else if (IsServer(bcptr))
  {
/* Old Protocol : */
    for (dlp = me.serv->down; dlp; dlp = dlp->next)
      if (Protocol(dlp->value.cptr)<9 && dlp->value.cptr->fd >= 0)
      {
        if (bcptr->from != dlp->value.cptr &&
            (sptr->from != dlp->value.cptr->from || OldProtocolFlag))
          sendto_one(dlp->value.cptr, "SQUIT %s :%s", bcptr->name, comment);
        else if (sptr->from != dlp->value.cptr->from && !OldProtocolFlag)
          sendto_one(dlp->value.cptr, ":%s SQUIT %s :%s", sptr->name,
              bcptr->name, comment);
      }

    /* Remove downlink list node of uplink */
    remove_dlink(&bcptr->serv->up->serv->down, bcptr->serv->updown);
  }
  else if (IsPing(bcptr)) /* Apperently, we are closing ALL links */
  {
    del_queries((char *)bcptr);
    end_ping(bcptr);
    return;
  }
  else if (IsService(bcptr))
  {
    /* Lame, lamer, Avalon. */
    sendto_ops("ERROR: exit_one_client: Service? Whaisda?");
  }
  else if (IsMe(bcptr))
  {
    sendto_ops("ERROR: tried to exit me! : %s", comment);
    return; /* ...must *never* exit self! */
  }

  /* Remove bcptr from the client list */
  if (del_from_client_hash_table(bcptr->name, bcptr) != 1)
    Debug((DEBUG_ERROR, "%#x !in tab %s[%s] %#x %#x %#x %d %d %#x",
	bcptr, bcptr->name, bcptr->from ? bcptr->from->sockhost : "??host",
	bcptr->from, bcptr->next, bcptr->prev, bcptr->fd,
	bcptr->status, bcptr->user));
  remove_client_from_list(bcptr);
  return;
}

void	checklist()
{
	Reg1	aClient	*acptr;
	Reg2	int	i,j;

	if (!(bootopt & BOOT_AUTODIE))
		return;
	for (j = i = 0; i <= highest_fd; i++)
		if (!(acptr = local[i]))
			continue;
		else if (IsClient(acptr))
			j++;
	if (!j)
	    {
#ifdef	USE_SYSLOG
		syslog(LOG_WARNING,"ircd exiting: autodie");
#endif
		exit(0);
	    }
	return;
}

void	initstats()
{
	bzero((char *)&ircst, sizeof(ircst));
}

void	tstats(cptr, name)
aClient	*cptr;
char	*name;
{
	Reg1	aClient	*acptr;
	Reg2	int	i;
	Reg3	struct stats	*sp;
	struct	stats	tmp;

	sp = &tmp;
	bcopy((char *)ircstp, (char *)sp, sizeof(*sp));
	for (i = 0; i < MAXCONNECTIONS; i++)
	    {
		if (!(acptr = local[i]))
			continue;
		if (IsServer(acptr))
		    {
			sp->is_sbs += acptr->sendB;
			sp->is_sbr += acptr->receiveB;
			sp->is_sks += acptr->sendK;
			sp->is_skr += acptr->receiveK;
			sp->is_sti += now - acptr->firsttime;
			sp->is_sv++;
			if (sp->is_sbs > 1023)
			    {
				sp->is_sks += (sp->is_sbs >> 10);
				sp->is_sbs &= 0x3ff;
			    }
			if (sp->is_sbr > 1023)
			    {
				sp->is_skr += (sp->is_sbr >> 10);
				sp->is_sbr &= 0x3ff;
			    }
		    }
		else if (IsClient(acptr))
		    {
			sp->is_cbs += acptr->sendB;
			sp->is_cbr += acptr->receiveB;
			sp->is_cks += acptr->sendK;
			sp->is_ckr += acptr->receiveK;
			sp->is_cti += now - acptr->firsttime;
			sp->is_cl++;
			if (sp->is_cbs > 1023)
			    {
				sp->is_cks += (sp->is_cbs >> 10);
				sp->is_cbs &= 0x3ff;
			    }
			if (sp->is_cbr > 1023)
			    {
				sp->is_ckr += (sp->is_cbr >> 10);
				sp->is_cbr &= 0x3ff;
			    }
		    }
		else if (IsUnknown(acptr))
			sp->is_ni++;
	    }

	sendto_one(cptr, ":%s %d %s :accepts %u refused %u",
		   me.name, RPL_STATSDEBUG, name, sp->is_ac, sp->is_ref);
	sendto_one(cptr, ":%s %d %s :unknown commands %u prefixes %u",
		   me.name, RPL_STATSDEBUG, name, sp->is_unco, sp->is_unpf);
	sendto_one(cptr, ":%s %d %s :nick collisions %u unknown closes %u",
		   me.name, RPL_STATSDEBUG, name, sp->is_kill, sp->is_ni);
	sendto_one(cptr, ":%s %d %s :wrong direction %u empty %u",
		   me.name, RPL_STATSDEBUG, name, sp->is_wrdi, sp->is_empt);
	sendto_one(cptr, ":%s %d %s :numerics seen %u mode fakes %u",
		   me.name, RPL_STATSDEBUG, name, sp->is_num, sp->is_fake);
	sendto_one(cptr, ":%s %d %s :auth successes %u fails %u",
		   me.name, RPL_STATSDEBUG, name, sp->is_asuc, sp->is_abad);
	sendto_one(cptr, ":%s %d %s :local connections %u udp packets %u",
		   me.name, RPL_STATSDEBUG, name, sp->is_loc, sp->is_udp);
	sendto_one(cptr, ":%s %d %s :Client Server",
		   me.name, RPL_STATSDEBUG, name);
	sendto_one(cptr, ":%s %d %s :connected %u %u",
		   me.name, RPL_STATSDEBUG, name, sp->is_cl, sp->is_sv);
	sendto_one(cptr, ":%s %d %s :bytes sent %u.%uK %u.%uK",
		   me.name, RPL_STATSDEBUG, name,
		   sp->is_cks, sp->is_cbs, sp->is_sks, sp->is_sbs);
	sendto_one(cptr, ":%s %d %s :bytes recv %u.%uK %u.%uK",
		   me.name, RPL_STATSDEBUG, name,
		   sp->is_ckr, sp->is_cbr, sp->is_skr, sp->is_sbr);
	sendto_one(cptr, ":%s %d %s :time connected %u %u",
		   me.name, RPL_STATSDEBUG, name, sp->is_cti, sp->is_sti);
}
