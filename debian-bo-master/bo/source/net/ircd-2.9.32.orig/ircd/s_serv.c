/************************************************************************
 *   IRC - Internet Relay Chat, ircd/s_serv.c (formerly ircd/s_msg.c)
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
static  char sccsid[] = "@(#)s_serv.c	2.55 2/7/94 (C) 1988 University of Oulu, \
Computing Center and Jarkko Oikarinen";
#endif

#include "struct.h"
#include "common.h"
#include "sys.h"
#include "numeric.h"
#include "msg.h"
#include "channel.h"
#include "userload.h"
#if defined(PCS) || defined(AIX) || defined(DYNIXPTX) || defined(SVR3)
#include <time.h>
#endif
#include <sys/stat.h>
#include <fcntl.h>
#include <utmp.h>
#include "h.h"

static	char	buf[BUFSIZE];

int     max_connection_count = 1, max_client_count = 1;

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
** m_version
**	parv[0] = sender prefix
**	parv[1] = remote server
*/
int	m_version(cptr, sptr, parc, parv)
aClient *sptr, *cptr;
int	parc;
char	*parv[];
{
	extern	char	serveropts[];
	Reg1	aClient	*acptr;
/* version should also be always available imo --dl
	if (check_registered(sptr))
		return 0;
*/
	if (MyConnect(sptr) && parc > 1)
	{
	  if (!(acptr = find_match_server(parv[1])))
	  {
	      sendto_one(sptr, err_str(ERR_NOSUCHSERVER),
		  me.name, parv[0], parv[1]);
	      return 0;
	  }
	  parv[1] = acptr->name;
        }

	if (hunt_server(cptr,sptr,":%s VERSION :%s",1,parc,parv)==HUNTED_ISME)
		sendto_one(sptr, rpl_str(RPL_VERSION), me.name,
			   parv[0], version, debugmode, me.name, serveropts);
	return 0;
}

/*
** m_squit
**	parv[0] = sender prefix
**	parv[1] = server name
**      parv[2] = timestamp
**	parv[parc-1] = comment
*/
int	m_squit(cptr, sptr, parc, parv)
aClient *cptr, *sptr;
int	parc;
char	*parv[];
    {
	Reg1	aConfItem *aconf;
	char	*server;
	Reg2	aClient	*acptr;
	char	*comment = (parc > ((!IsServer(cptr) || Protocol(cptr) < 9) ?
			   2 : 3) && !BadPtr(parv[parc-1])) ?
			   parv[parc-1] : cptr->name;

	if (!IsPrivileged(sptr))
	    {
		sendto_one(sptr, err_str(ERR_NOPRIVILEGES), me.name, parv[0]);
		return 0;
	    }

	if (parc > (IsServer(cptr) ? ((Protocol(cptr) < 9) ? 1 : 2) : 1))
	    {
		server = parv[1];
		/*
		** To accomodate host masking, a squit for a masked server
		** name is expanded if the incoming mask is the same as
		** the server name for that link to the name of link.
		*/
		if ((*server == '*') && IsServer(cptr) &&
		    (aconf = cptr->serv->nline) &&
		    !mycmp(server, my_name_for_link(me.name, aconf)))
		{
		  server = cptr->name;
		  acptr = cptr;
		}
		else
		{
		  /*
		  ** The following allows wild cards in SQUIT. Only usefull
		  ** when the command is issued by an oper.
		  */
		  for (acptr = client; (acptr = next_client(acptr, server));
		      acptr = acptr->next)
		    if (IsServer(acptr) || IsMe(acptr))
		      break;

		  if (acptr)
		  {
		    if (IsMe(acptr))
		      {
			  if (IsServer(cptr))
			  {
			    acptr = cptr;
			    server = cptr->sockhost;
			  }
			  else
			    acptr = NULL;
		      }
		    else
		    {
		      /*
		      ** Look for a matching server that is closer,
		      ** that way we won't accidently squit two close
		      ** servers like davis.* and davis-r.* when typing
		      ** /SQUIT davis*
		      */
		      aClient *acptr2;
		      for (acptr2 = acptr->serv->up; acptr2 != &me;
		          acptr2 = acptr2->serv->up)
		        if (!match(server, acptr2->name))
		          acptr = acptr2;
		    }
		  }
		}
		/* If atoi(parv[2]) == 0 we must indeed squit !
		** It wil be our neighbour.
		*/
		if (acptr && IsServer(sptr) && Protocol(sptr) > 4 &&
		    atoi(parv[2]) && atoi(parv[2]) != acptr->serv->timestamp)
		{
		  Debug((DEBUG_NOTICE, "Ignoring SQUIT with wrong timestamp"));
		  return 0;
		}
	    }
	else
	    {
		sendto_one(cptr, err_str(ERR_NEEDMOREPARAMS),
		     me.name, parv[0], "SQUIT");
		 if (IsServer(cptr))
		 {
		    /*
		    ** This is actually protocol error. But, well, closing
		    ** the link is very proper answer to that...
		    */
		    server = cptr->sockhost;
		    acptr = cptr;
		  }
	          else
		    return 0;
	     }
	if (!acptr)
	    {
		if (IsPerson(sptr))
		  sendto_one(sptr, err_str(ERR_NOSUCHSERVER),
			   me.name, parv[0], server);
		return 0;
	    }
	if (IsLocOp(sptr) && !MyConnect(acptr))
	    {
		sendto_one(sptr, err_str(ERR_NOPRIVILEGES), me.name, parv[0]);
		return 0;
	    }

	return exit_client(cptr, acptr, sptr, comment);
    }

int exit_new_server(cptr, sptr, host, prot, timestamp, fmt, p1, p2, p3)
aClient *cptr, *sptr;
u_short prot;
time_t timestamp;
char *fmt, *p1, *p2, *p3, *host;
{
  char buf[128];
  if (!IsServer(sptr))
  {
    if (prot < 9)
    {
      strcpy(buf, ":%s SQUIT %s :");
      strcat(buf, fmt);
      sendto_one(cptr, buf, me.name, host, p1, p2, p3);
    }
    return exit_client_msg(cptr, cptr, &me, fmt, p1, p2, p3);
  }
  if (Protocol(cptr) < 9)
  {
    strcpy(buf, ":%s SQUIT %s :");
    strcat(buf, fmt);
    sendto_one(cptr, buf, me.name, host, p1, p2, p3);
  }
  else
  {
    strcpy(buf, ":%s SQUIT %s %lu :");
    strcat(buf, fmt);
    sendto_one(cptr, buf, me.name, host, timestamp, p1, p2, p3);
  }
  return 0;
}

static int a_kills_b_too(a, b)
register aClient *a, *b;
{
  for (; b != a && b != &me; b = b->serv->up)
  return (a == b ? 1 : 0);
}

/*
** m_server
**	parv[0] = sender prefix
**	parv[1] = servername
**	parv[2] = hopcount
** P09: parv[3] = start timestamp
**      parv[4] = link timestamp
**      parv[5] = major protocol version: P09
**      parv[parc-1] = serverinfo
*/
int	m_server(cptr, sptr, parc, parv)
aClient *cptr, *sptr;
int	parc;
char	*parv[];
{
	Reg1	char	*ch;
	Reg2	int	i;
	Reg3	Link	*lp;
	char	info[REALLEN+1], *inpath, *host, *s;
	aClient *acptr, *bcptr, *LHcptr;
	aConfItem *aconf, *bconf, *cconf, *lhconf;
	int	hop, ret, active_lh_line = 0;
	u_short	prot;
	time_t	start_timestamp, timestamp=0, recv_time, ghost=0;

	recv_time = TStime();
	info[0] = '\0';
	inpath = get_client_name(cptr, TRUE);
	if (parc < 2 || *parv[1] == '\0')
	{
	  sendto_one(cptr,"ERROR :No servername");
	  return exit_client(cptr, cptr, &me, "No servername");
	}
	/* Since .U5 a hopcount is required --Run */
	if (parc < 3 || (hop=atoi(parv[2])) == 0 ||
	    (IsServer(cptr) && parc < ((Protocol(cptr) > 4) ? 7 : 4)))
	{
	  sendto_one(sptr, err_str(ERR_NEEDMOREPARAMS),
	       me.name, parv[0], "SERVER");
	  return exit_client(cptr, cptr, &me, "Need more parameters");
	}
	host = parv[1];
	/* Detect Protocol(cptr) */
	if (parc > 6)
	{
	  if (strlen(parv[5])!=3 || (parv[5][0] != 'P' && parv[5][0] != 'J'))
	    return exit_client_msg(cptr, sptr, &me,
	        "Bogus protocol (%s)", parv[5]);
	  prot=atoi(parv[5]+1);
	  start_timestamp = atoi(parv[3]);
	  timestamp = atoi(parv[4]);
	  Debug((DEBUG_INFO, "Got SERVER %s with timestamp [%s] age %lu (%lu)",
	      host, parv[4], start_timestamp, me.serv->timestamp));
	  if (prot>4 &&
	      (timestamp<780000000 || hop==1 && start_timestamp<780000000))
	  {
	    return exit_client_msg(cptr, sptr, &me,
	        "Bogus timestamps (%s %s)", parv[3], parv[4]);
	  }
	}
	else if (!IsServer(cptr)) /* New local server ? */
	/* Protocol 4 didn't support protocol versions yet */
	  prot=4;
	else /* Remote server */
	  prot=0; /* Set to 'unknown' */
	if (parc > 3) (void)strncpy(info, parv[parc-1], REALLEN);
	if (prot && (prot < atoi(MINOR_PROTOCOL)))
	{
	    sendto_ops("Got incompatible protocol version (%s) from %s",
	        parv[5], get_client_name(cptr, TRUE));
            return exit_new_server(cptr, sptr, host, prot, timestamp,
		"Incompatible protocol: %s", parv[5]);
	}
	/*
	** Check for "FRENCH " infection ;-) (actually this should
	** be replaced with routine to check the hostname syntax in
	** general). [ This check is still needed, even after the parse
	** is fixed, because someone can send "SERVER :foo bar " ].
	** Also, changed to check other "difficult" characters, now
	** that parse lets all through... --msa
	*/
	if (strlen(host) > HOSTLEN)
		host[HOSTLEN] = '\0';
	for (ch = host; *ch; ch++)
		if (*ch <= ' ' || *ch > '~')
			break;
	if (*ch || !index(host, '.'))
	    {
		sendto_ops("Bogus server name (%s) from %s", host,
			   get_client_name(cptr, TRUE));
		return exit_client_msg(cptr, cptr, &me,
		    "Bogus server name (%s)", host);
	    }

	if (IsPerson(cptr))
	    {
		/*
		** A local link that has been identified as a USER
		** tries something fishy... ;-)
		*/
		sendto_one(cptr, err_str(ERR_ALREADYREGISTRED),
			   me.name, parv[0]);
		sendto_ops("User %s trying to become a server %s",
			   get_client_name(cptr, TRUE),host);
		return 0;
	    }
	
	if (IsServer(cptr))
	{
		/*
		** A local server introduces a new server behind this link.
		** Check if this is allowed according L:, H: and Q: lines.
		*/
		if (info[0] == '\0')
		  return exit_client_msg(cptr, cptr, &me,
		      "No server info specified for %s", host);

		/*
		** See if the newly found server is behind a guaranteed
		** leaf (L-line). If so, close the link.
		*/
		if ((lhconf = find_conf_host(cptr->confs, host, CONF_LEAF)) &&
		    (!lhconf->port || (hop > lhconf->port)))
	 	{	
	 	  /*
	 	  ** L: lines normally come in pairs, here we try to
	 	  ** make sure that the oldest link is squitted, not
	 	  ** both.
	 	  */
	 	  active_lh_line = 1;
		  if (timestamp <= cptr->serv->timestamp)
		    LHcptr = NULL; /* Kill incoming server */
		  else
		    LHcptr = cptr; /* Squit ourselfs */
		}
		else if (!(lhconf = find_conf_host(cptr->confs, host, CONF_HUB)) ||
		    (lhconf->port && (hop > lhconf->port)) )
		{
		  aClient *ac3ptr;
		  active_lh_line = 2;
		  /* Look for net junction causing this: */
		  LHcptr = (Protocol(sptr)>4) ? NULL : /* incoming server */
		  				cptr;  /* Or local if prot==4 */
		  if (Protocol(sptr)>4 && *parv[5]!='J')
		    for (ac3ptr = sptr; ac3ptr != &me;
		        ac3ptr = ac3ptr->serv->up)
                      if (IsJunction(ac3ptr))
                      {
                        LHcptr=ac3ptr;
                        break;
                      }
		}
		/*
		** See if the newly found server has a Q line for it in
		** our conf. If it does, lose the link that brought it
		** into our network. Format:
		**
		** Q:<unused>:<reason>:<servername>
		**
		** Example:  Q:*:for the hell of it:eris.Berkeley.EDU
		*/
		if ((aconf = find_conf_name(host, CONF_QUARANTINED_SERVER)))
		    {
			sendto_ops_butone(NULL, &me,
				":%s WALLOPS * :%s brought in %s, %s %s",
				me.name, me.name, get_client_name(cptr,FALSE),
				host, "closing link because",
				BadPtr(aconf->passwd) ? "reason unspecified" :
				aconf->passwd);

			return exit_client_msg(cptr, cptr, &me,
			    "%s is not welcome: %s. %s", host,
			    BadPtr(aconf->passwd) ? "reason unspecified" :
			    aconf->passwd, "Q-lined.");
		    }
	}

	if (IsUnknown(cptr) || IsHandshake(cptr))
	{
	  char *encr;

	  /*
	  ** A local link that is still in undefined state wants
	  ** to be a SERVER. Check if this is allowed and change
	  ** status accordingly...
	  */
	  /*
	  ** If there is more then one server on the same machine
	  ** that we try to connect to, it could be that the /CONNECT
	  ** <mask> caused this connect to be put at the wrong place
	  ** in the hashtable.
	  ** --Run
	  */
	  if (IsHandshake(cptr) &&
	      hash_nick_name(cptr->name) != hash_nick_name(host))
          {
            del_from_client_hash_table(cptr->name, cptr);
            add_to_client_hash_table(host, cptr);
          }
	  strncpyzt(cptr->name, host, sizeof(cptr->name));
	  strncpyzt(cptr->info, info[0] ? info:me.name, sizeof(cptr->info));
	  cptr->hopcount = hop;

	  /* check connection rules */
	  for (cconf = conf; cconf; cconf = cconf->next)
	    if ((cconf->status == CONF_CRULEALL) &&
		(matches(cconf->host, host) == 0))
	      if (crule_eval (cconf->passwd))
		{
		  ircstp->is_ref++;
		  sendto_ops("Refused connection from %s.",
			     get_client_host(cptr));
		  return exit_client(cptr, cptr, &me,
		      "Disallowed by connection rule");
		}

	  if (check_server_init(cptr))
	  {
	    ircstp->is_ref++;
	    sendto_ops("Received unauthorized connection from %s.",
	      get_client_host(cptr));
	    return exit_client(cptr, cptr, &me, "No C/N conf lines");
	  }

	  host = cptr->name;

          current_load_data.conn_count++;
          update_load();

	  if (!(aconf = find_conf(cptr->confs, host, CONF_NOCONNECT_SERVER)))
	      {
		  ircstp->is_ref++;
		  sendto_ops("Access denied. No N line for server %s", inpath);
		  return exit_client_msg(cptr, cptr, &me,
		      "Access denied. No N line for server %s", inpath);
	      }
	  if (!(bconf = find_conf(cptr->confs, host, CONF_CONNECT_SERVER)))
	      {
		  ircstp->is_ref++;
		  sendto_ops("Only N (no C) field for server %s",inpath);
		  return exit_client_msg(cptr, cptr, &me,
		      "Only N (no C) field for server %s", inpath);
	      }

#ifdef CRYPT_LINK_PASSWORD
	  /* passwd may be NULL. Head it off at the pass... */
	  if(*cptr->passwd)
	      {
		  char    salt[3];
		  extern  char *crypt();

		  salt[0]=aconf->passwd[0];
		  salt[1]=aconf->passwd[1];
		  salt[2]='\0';
		  encr = crypt(cptr->passwd, salt);
	      }
	  else
		  encr = "";
#else
	  encr = cptr->passwd;
#endif  /* CRYPT_LINK_PASSWORD */
	  if (*aconf->passwd && !StrEq(aconf->passwd, encr))
	      {
		  ircstp->is_ref++;
		  sendto_ops("Access denied (passwd mismatch) %s", inpath);
		  return exit_client_msg(cptr, cptr, &me, 
		      "No Access (passwd mismatch) %s", inpath);
	      }
	  bzero(cptr->passwd, sizeof(cptr->passwd));

#ifndef	HUB
	  for (i = 0; i <= highest_fd; i++)
		  if (local[i] && IsServer(local[i]))
		      {
			  active_lh_line = 3;
			  LHcptr = cptr;
			  break;
		      }
#endif
	  if (!IsUnknown(cptr))	
	    {
		s = (char *)index(aconf->host, '@');
		*s = '\0'; /* should never be NULL */
		Debug((DEBUG_INFO, "Check Usernames [%s]vs[%s]",
			aconf->host, cptr->username));
		if (matches(aconf->host, cptr->username))
		    {
			*s = '@';
			ircstp->is_ref++;
			sendto_ops("Username mismatch [%s]v[%s] : %s",
				   aconf->host, cptr->username,
				   get_client_name(cptr, TRUE));
			return exit_client(cptr, cptr, &me, "Bad Username");
		    }
		*s = '@';
	    }
	}

	/*
	** We want to find IsConnecting() and IsHandshake() too,
	** use find_client(). The second try is for hostmasking, although
	** I doubt if that is still fully supported at all --Run.
	*/
	while ((acptr = find_client(host, NULL)) ||
	    (acptr = find_name(host, NULL)))
	    {
		/*
		** This link is trying feed me a server that I already have
		** access through another path
		**
		** Do not allow Uworld to do this.
		** Do not allow servers that are juped.
		** Do not allow servers that have older link timestamps
		**   then this try.
		**
		** If my ircd.conf sucks, I can try to connect to myself:
		*/   
		if (acptr == &me)
		  return exit_client_msg(cptr, cptr, &me,
		    "nick collision with me (%s)", host);
		/*
		** Kill our try, if we had one.
		*/
		if (IsConnecting(acptr))
		{
		  if (!active_lh_line && exit_client(cptr, acptr, &me,
		      "Just connected via another link") == CPTR_KILLED)
		    return CPTR_KILLED;
		  /*
		  **  We can have only ONE 'IsConnecting', 'IsHandshake' or
		  ** 'IsServer', because new 'IsConnecting's are refused to
		  ** the same server if we already had it.
		  */
		  break;
		}
		/*
		** Avoid other nick collisions...
		** This is a doubtfull test though, what else would it be
		** when it has a server.name ?
		*/
		else if (!IsServer(acptr) && !IsHandshake(acptr))
		  return exit_client_msg(cptr, cptr, &me,
		      "Nickname %s already exists!", host);
		/*
		** Our new server might be a juped server,
		** or someone trying abuse a second Uworld:
		*/
	        else if (IsServer(acptr) &&
	            (strncasecmp(acptr->info,"JUPE",4)==0 ||
	            find_conf_host(cptr->confs, sptr->name, CONF_UWORLD)))
		{
		  if (!IsServer(sptr))
		    return exit_client(cptr, sptr, &me, acptr->info);
		  sendto_one(cptr,
		      ":%s WALLOPS :Received :%s SERVER %s from %s !?!",
		      me.name, parv[0], parv[1], cptr->name);
		  return exit_new_server(cptr, sptr, host, prot, timestamp,
		        "%s", acptr->info);
		}
		/*
		** Of course we find the handshake this link was before :)
		*/
		else if (IsHandshake(acptr) && acptr == cptr)
		  break;
		/*
		** This is the old protocol, can be removed when all upgraded
		** to u2.9.
		*/
		if (prot<9 || (IsServer(acptr) && Protocol(acptr)<9))
		  return exit_client_msg(cptr, cptr, &me,
	              "server %s already exists (protocol 4 failure)", host);
		/*
		** Here we have a server nick collision...
		** We don't want to kill the link that was last /connected,
		** but we neither want to kill a good (old) link.
		** Therefor we kill the second youngest link.
		*/
		else
		{
		  aClient *c2ptr = NULL, *c3ptr = acptr;
		  aClient *ac2ptr, *ac3ptr;

		  /* Search youngest link: */
		  for (ac3ptr = acptr; ac3ptr != &me; ac3ptr = ac3ptr->serv->up)
		    if (ac3ptr->serv->timestamp > c3ptr->serv->timestamp)
		      c3ptr = ac3ptr;
		  if (IsServer(sptr))
		  {
		    for (ac3ptr = sptr; ac3ptr != &me;
		        ac3ptr = ac3ptr->serv->up)
		      if (ac3ptr->serv->timestamp > c3ptr->serv->timestamp)
		        c3ptr = ac3ptr;
		  }
		  if (timestamp > c3ptr->serv->timestamp)
		  {
		    c3ptr = NULL;
		    c2ptr = acptr; /* Make sure they differ */
		  }
		  /* Search second youngest link: */
		  for (ac2ptr = acptr; ac2ptr != &me; ac2ptr = ac2ptr->serv->up)
		    if (ac2ptr != c3ptr &&
		        ac2ptr->serv->timestamp > 
		        (c2ptr ? c2ptr->serv->timestamp : timestamp))
		      c2ptr = ac2ptr;
		  if (IsServer(sptr))
		  {
		    for (ac2ptr = sptr; ac2ptr != &me;
		        ac2ptr = ac2ptr->serv->up)
		      if (ac2ptr != c3ptr &&
		          ac2ptr->serv->timestamp >
		          (c2ptr ? c2ptr->serv->timestamp : timestamp))
		        c2ptr = ac2ptr;
		  }
		  if (c3ptr && timestamp >
		      (c2ptr ? c2ptr->serv->timestamp : timestamp))
		    c2ptr = NULL;
		  /* If timestamps are equal, decide which link to break
		  ** by name.
		  */
		  if ((c2ptr ? c2ptr->serv->timestamp : timestamp) ==
		      (c3ptr ? c3ptr->serv->timestamp : timestamp))
		  {
		    char *n2, *n2up;
		    char *n3, *n3up;
		    if (c2ptr)
		    { n2=c2ptr->name;
		      n2up=MyConnect(c2ptr)?me.name:c2ptr->serv->up->name; }
		    else
		    { n2=host;
		      n2up=IsServer(sptr)?sptr->name:me.name; }
		    if (c3ptr)
		    { n3=c3ptr->name;
		      n3up=MyConnect(c3ptr)?me.name:c3ptr->serv->up->name; }
		    else
		    { n3=host;
		      n3up=IsServer(sptr)?sptr->name:me.name; }
		    if (strcmp(n2, n2up) > 0)
		      n2=n2up;
		    if (strcmp(n3, n3up) > 0)
		      n3=n3up;
		    if (strcmp(n3, n2) > 0)
                    {
		       ac2ptr = c2ptr; c2ptr = c3ptr; c3ptr = ac2ptr;
                    }
		  }
		  /* Now squit the second youngest link: */
		  if (!c2ptr)
		    return exit_new_server(cptr, sptr, host, prot, timestamp,
			"server %s already exists and is %ld seconds younger.",
			host, (long)acptr->serv->timestamp - (long)timestamp);
		  else if (c2ptr->from == cptr || IsServer(sptr))
		  {
		    if (active_lh_line)
		    {
		      /*
		      ** If the L: or H: line also gets rid of this link,
		      ** we sent just one squit.
		      */
		      if (LHcptr && a_kills_b_too(LHcptr, c2ptr))
			break;
		      /*
		      ** If breaking the loop here solves the L: or H:
		      ** line problem, we don't squit that.
		      */
		      if (c2ptr->from == cptr ||
		          (LHcptr && a_kills_b_too(c2ptr, LHcptr)))
			active_lh_line = 0;
		      else
		      {
		      /*
		      ** If we still have a L: or H: line problem,
		      ** we prefer to squit the new server, solving
		      ** loop and L:/H: line problem with only one squit.
		      */
		        LHcptr = NULL;
		        break;
		      }
		    }
		    /*
		    ** If the new server was introduced by a server that caused a
		    ** Ghost less then 20 seconds ago, this is probably also
		    ** a Ghost... (20 seconds is more then enough because all
		    ** SERVER messages are at the beginning of a net.burst). --Run
		    */
		    if (now - cptr->serv->ghost < 20)
                    {
		      if (exit_client(cptr, acptr, &me, "Ghost loop") == CPTR_KILLED)
			return CPTR_KILLED;
		    }
		    else if (exit_client_msg(cptr, c2ptr, &me,
			"Loop <-- %s (new link is %d seconds younger)", host,
			(c3ptr ? (long)c3ptr->serv->timestamp : timestamp) -
			(long)c2ptr->serv->timestamp) == CPTR_KILLED)
		      return CPTR_KILLED;
		  }
		  else
		  {
		    if (active_lh_line)
		    {
		      if (LHcptr && a_kills_b_too(LHcptr, acptr))
			break;
		      if (acptr->from == cptr ||
		          (LHcptr && a_kills_b_too(acptr, LHcptr)))
			active_lh_line = 0;
		      else
		      {
		        LHcptr = NULL;
		        break;
		      }
		    }
		    /*
		    ** We can't believe it is a lagged server message
		    ** when it directly connects to us...
		    ** kill the older link at the ghost, rather then
		    ** at the second youngest link, assuming it isn't
		    ** a REAL loop.
		    */
		    ghost = now; /* Mark that it caused a ghost */
		    if (exit_client(cptr, acptr, &me, "Ghost") == CPTR_KILLED)
		      return CPTR_KILLED;
		    break;
		  }
		  /*
		  ** Did we kill the incoming server off already ?
		  */
		  if (c2ptr->from == cptr)
		    return 0;
		}
	    }

	if (active_lh_line)
	    {
	      if (LHcptr == NULL)
		return exit_new_server(cptr, sptr, host, prot, timestamp,
		    (active_lh_line == 2) ?
		    "Non-Hub link %s <- %s(%s)" :
		    "Leaf-only link %s <- %s(%s)",
		    get_client_name(cptr,  TRUE), host,
		    lhconf ? (lhconf->host ? lhconf->host : "*") : "!");
	      else
	      {
	        register int killed = a_kills_b_too(LHcptr, sptr);
	        if (active_lh_line < 3)
		{
		  if (exit_client_msg(cptr, LHcptr, &me,
		      (active_lh_line == 2) ?
		      "Non-Hub link %s <- %s(%s)" :
		      "Leaf-only link %s <- %s(%s)",
		      get_client_name(cptr,  TRUE), host,
		      lhconf ? (lhconf->host ? lhconf->host : "*") : "!")
		      == CPTR_KILLED)
		    return CPTR_KILLED;
		}
	        else
		{
		  ircstp->is_ref++;
		  if (exit_client(cptr, LHcptr, &me, "I'm a leaf") ==
		      CPTR_KILLED)
		    return CPTR_KILLED;
		}
		/*
		** Did we kill the incoming server off already ?
		*/
		if (killed)
		  return 0;
	      }
	    }

	if (IsServer(cptr))
	    {
		/*
		** Server is informing about a new server behind
		** this link. Create REMOTE server structure,
		** add it to list and propagate word to my other
		** server links...
		*/

		acptr = make_client(cptr);
		(void)make_server(acptr);
		acptr->serv->prot = prot;
	  	acptr->serv->timestamp = timestamp;
		acptr->hopcount = hop;
		strncpyzt(acptr->name, host, sizeof(acptr->name));
		strncpyzt(acptr->info, info, sizeof(acptr->info));
		acptr->serv->up = sptr;
		acptr->serv->updown = add_dlink(&sptr->serv->down, acptr);
		SetServer(acptr);
		acptr->flags|=FLAGS_TS8;
		add_client_to_list(acptr);
		(void)add_to_client_hash_table(acptr->name, acptr);
		if (Protocol(cptr)>4 && *parv[5] == 'J')
		{
		  sendto_ops("Net junction: %s %s", sptr->name, acptr->name);
		  SetJunction(acptr);
		}
		/*
		** Old sendto_serv_but_one() call removed because we now
		** need to send different names to different servers
		** (domain name matching)
		*/
		for (i = 0; i <= highest_fd; i++)
		    {
			if (!(bcptr = local[i]) || !IsServer(bcptr) ||
			    bcptr == cptr || IsMe(bcptr))
				continue;
			if (!(cconf = bcptr->serv->nline))
			    {
				sendto_ops("Lost N-line for %s on %s. Closing",
					   get_client_name(cptr, TRUE), host);
				return exit_client(cptr, cptr, &me,
				    "Lost N line");
			    }
			if (matches(my_name_for_link(me.name, cconf),
				    acptr->name) == 0)
				continue;
			if (Protocol(bcptr) > 4)
			{
			  if (Protocol(cptr) > 4)
			    sendto_one(bcptr, ":%s SERVER %s %d 0 %s %s :%s",
				parv[0], acptr->name, hop+1,
				parv[4], parv[5], acptr->info);
			  else
			    sendto_one(bcptr, ":%s SERVER %s %d 0 0 P0%u :%s",
				parv[0], acptr->name, hop+1,
				Protocol(acptr), acptr->info);
			}
			else
			  sendto_one(bcptr, ":%s SERVER %s %d :%s",
			      parv[0], acptr->name, hop+1, acptr->info);
		    }
#ifdef	USE_SERVICES
		check_services_butone(SERVICE_WANT_SERVER, sptr,
		    ":%s SERVER %s %d %s %s :%s", parv[0],
		    acptr->name, hop+1, parv[3], parv[4], acptr->info);
#endif
		return 0;
	    }

	if (IsUnknown(cptr) || IsHandshake(cptr))
	{
	  (void)make_server(cptr);
	  cptr->serv->timestamp = timestamp;
	  cptr->serv->prot = prot;
	  cptr->serv->ghost = ghost;

	  ret = m_server_estab(cptr, aconf, bconf);
	}
	else ret = 0;

	if (IsServer(cptr) && prot > 4 && start_timestamp > 780000000 &&
	    start_timestamp < me.serv->timestamp)
	{
#ifndef RELIABLE_CLOCK
#ifdef TESTNET
	  sendto_ops("Debug: my start time: %lu ; others start time: %lu",
	      me.serv->timestamp, start_timestamp);
	  sendto_ops("Debug: receive time: %lu ; received timestamp: %lu"
	      " ; difference %ld", recv_time, timestamp, timestamp - recv_time);
#endif
	  me.serv->timestamp += timestamp - recv_time;
	  TSoffset += timestamp - recv_time;
#else
	  if (abs(timestamp - recv_time)>30)
	  { sendto_ops("Connected to a net with a timestamp-clock"
		" difference of %ld seconds! Used SETTIME to correct"
		" this.", timestamp - recv_time);
	    sendto_one(cptr, ":%s SETTIME %lu :%s", me.name, TStime(),
		me.name); }
#endif
	}

	return ret;
}

/*
 * m_server_estab, may only be called after a SERVER was received from cptr,
 * and thus make_server was called, and serv->prot set. --Run
 */
int	m_server_estab(cptr, aconf, bconf)
Reg1	aClient	*cptr;
Reg2	aConfItem *aconf, *bconf;
{
	Reg3	aClient	*acptr;
	Reg4	Link *lp;
	char	*inpath, *host, *s;
	int	split, i;

	split = (mycmp(cptr->name, cptr->sockhost) &&
		strncasecmp(cptr->info, "JUPE", 4));
	inpath = get_client_name(cptr,TRUE);
	host = cptr->name;

	if (IsUnknown(cptr))
	    {
		if (bconf->passwd[0])
			sendto_one(cptr,"PASS :%s",bconf->passwd);
		/*
		** Pass my info to the new server
		*/
		if (Protocol(cptr)==4)
		  sendto_one(cptr, "SERVER %s 1 :%s",
			   my_name_for_link(me.name, aconf),
			   (me.info[0]) ? (me.info) : "IRCers United");
		else
		  sendto_one(cptr, "SERVER %s 1 %lu %lu J%s :%s",
			   my_name_for_link(me.name, aconf),
			   me.serv->timestamp, cptr->serv->timestamp,
			   MAJOR_PROTOCOL,
			   (me.info[0]) ? (me.info) : "IRCers United");
	    }

	det_confs_butmask(cptr,
	    CONF_LEAF|CONF_HUB|CONF_NOCONNECT_SERVER|CONF_UWORLD);

	if (!IsHandshake(cptr))
	  (void)add_to_client_hash_table(cptr->name, cptr);
	SetServer(cptr);
	cptr->flags|=FLAGS_TS8;
	nextping = now;
	if (cptr->serv->user && (acptr=find_client(cptr->serv->by, NULL)) &&
	    acptr->user == cptr->serv->user)
          sendto_one(acptr, ":%s NOTICE %s :Link with %s established.",
              me.name, acptr->name, inpath);
        else
          acptr = NULL;
	sendto_lops_butone(acptr, "Link with %s established.", inpath);
	cptr->serv->up = &me;
	cptr->serv->updown = add_dlink(&me.serv->down, cptr);
	cptr->serv->nline = aconf;
	sendto_ops("Net junction: %s %s", me.name, cptr->name);
	SetJunction(cptr);
#ifdef	USE_SERVICES
	check_services_butone(SERVICE_WANT_SERVER, sptr,
				":%s SERVER %s %d :%s", parv[0],
				cptr->name, hop+1, cptr->info);
#endif
	/*
	** Old sendto_serv_but_one() call removed because we now
	** need to send different names to different servers
	** (domain name matching) Send new server to other servers.
	*/
	for (i = 0; i <= highest_fd; i++) 
	    {
		if (!(acptr = local[i]) || !IsServer(acptr) ||
		    acptr == cptr || IsMe(acptr))
			continue;
		if ((aconf = acptr->serv->nline) &&
		    !matches(my_name_for_link(me.name, aconf), cptr->name))
			continue;
		if (split && Protocol(acptr)<9)
		{
		  sendto_one(acptr,":%s SERVER %s 2 :[%s] %s", me.name,
		      cptr->name, cptr->sockhost, cptr->info);
		}
		else if (split)
		{
		  sendto_one(acptr, ":%s SERVER %s 2 0 %lu %s%u :[%s] %s",
		      me.name, cptr->name, cptr->serv->timestamp,
		      (Protocol(cptr)>9)?"J":"J0", Protocol(cptr),
		      cptr->sockhost, cptr->info);
		}
		else if (!split && Protocol(acptr)<9)
		{
		  sendto_one(acptr,":%s SERVER %s 2 :%s",
		      me.name, cptr->name, cptr->info);
		}
		else
		{
		  sendto_one(acptr, ":%s SERVER %s 2 0 %lu %s%u :%s",
		      me.name, cptr->name, cptr->serv->timestamp,
		      (Protocol(cptr)>9)?"J":"J0", Protocol(cptr),
		      cptr->info);
		}
	    }

	/*
	** Pass on my client information to the new server
	**
	** First, pass only servers (idea is that if the link gets
	** cancelled beacause the server was already there,
	** there are no NICK's to be cancelled...). Of course,
	** if cancellation occurs, all this info is sent anyway,
	** and I guess the link dies when a read is attempted...? --msa
	** 
	** Note: Link cancellation to occur at this point means
	** that at least two servers from my fragment are building
	** up connection this other fragment at the same time, it's
	** a race condition, not the normal way of operation...
	**
	*/

	aconf = cptr->serv->nline;
	for (acptr = &me; acptr; acptr = acptr->prev)
	    {
		/* acptr->from == acptr for acptr == cptr */
		if (acptr->from == cptr)
			continue;
		if (IsServer(acptr))
		    {
			if (matches(my_name_for_link(me.name, aconf),
				    acptr->name) == 0)
				continue;
			split = (MyConnect(acptr) &&
				 mycmp(acptr->name, acptr->sockhost) &&
				 strncasecmp(acptr->info, "JUPE", 4));
			if (split && Protocol(cptr)<9)
			{
			  sendto_one(cptr, ":%s SERVER %s %d :[%s] %s",
		   	      acptr->serv->up->name, acptr->name,
			      acptr->hopcount+1,
		   	      acptr->sockhost, acptr->info);
			}
			else if (split)
			{
			  sendto_one(cptr,
			      ":%s SERVER %s %d 0 %lu %s%u :[%s] %s",
		   	      acptr->serv->up->name, acptr->name,
			      acptr->hopcount+1, acptr->serv->timestamp,
			      (Protocol(acptr)>9)?"P":"P0", Protocol(acptr),
		   	      acptr->sockhost, acptr->info);
			}
			else if (!split && Protocol(cptr)<9)
			{
			  sendto_one(cptr, ":%s SERVER %s %d :%s",
		   	      acptr->serv->up->name, acptr->name,
			      acptr->hopcount+1, acptr->info);
			}
			else
			{
			  sendto_one(cptr,
			      ":%s SERVER %s %d 0 %lu %s%u :%s",
		   	      acptr->serv->up->name, acptr->name,
			      acptr->hopcount+1, acptr->serv->timestamp,
			      (Protocol(acptr)>9)?"P":"P0", Protocol(acptr),
		   	      acptr->info);
			}
		    }
	    }

	for (acptr = &me; acptr; acptr = acptr->prev)
	    {
		/* acptr->from == acptr for acptr == cptr */
		if (acptr->from == cptr)
			continue;
		if (IsPerson(acptr))
		    {
			/*
			** IsPerson(x) is true only when IsClient(x) is true.
			** These are only true when *BOTH* NICK and USER have
			** been received. -avalon
			** Or only NICK in new format. --Run
			*/
			sendto_one(cptr,":%s NICK %s %d %d %s %s %s :%s",
			  acptr->user->server->name,
			  acptr->name, acptr->hopcount + 1, acptr->lastnick,
			  acptr->user->username, acptr->user->host,
			  acptr->user->server->name, acptr->info);
			send_umode(cptr, acptr, 0, SEND_UMODES, buf);
			send_user_joins(cptr, acptr);
		    }
		else if (IsService(acptr))
		    {
			sendto_one(cptr,"NICK %s :%d",
				   acptr->name, acptr->hopcount + 1);
			sendto_one(cptr,":%s SERVICE * * :%s",
				   acptr->name, acptr->info);
		    }
	    }
	/*
	** Last, pass all channels plus statuses
	*/
	{
		Reg1 aChannel *chptr;
		for (chptr = channel; chptr; chptr = chptr->nextch)
			send_channel_modes(cptr, chptr);
	}
	return 0;
}

/*
** m_info
**	parv[0] = sender prefix
**	parv[1] = servername
*/
int	m_info(cptr, sptr, parc, parv)
aClient *cptr, *sptr;
int	parc;
char	*parv[];
{
	char **text = infotext;

	if (check_registered(sptr))
		return 0;

	if (hunt_server(cptr,sptr,":%s INFO :%s",1,parc,parv) == HUNTED_ISME)
	    {
		while (*text)
			sendto_one(sptr, rpl_str(RPL_INFO),
				   me.name, parv[0], *text++);

		sendto_one(sptr, rpl_str(RPL_INFO), me.name, parv[0], "");
		sendto_one(sptr,
			   ":%s %d %s :Birth Date: %s, compile # %s",
			   me.name, RPL_INFO, parv[0], creation, generation);
		sendto_one(sptr, ":%s %d %s :On-line since %s",
			   me.name, RPL_INFO, parv[0],
			   myctime(me.firsttime));
		sendto_one(sptr, rpl_str(RPL_ENDOFINFO), me.name, parv[0]);
	    }

    return 0;
}

/*
** m_links
**	parv[0] = sender prefix
**	parv[1] = servername mask
** or
**	parv[0] = sender prefix
**	parv[1] = server to query 
**      parv[2] = servername mask
*/
int	m_links(cptr, sptr, parc, parv)
aClient *cptr, *sptr;
int	parc;
char	*parv[];
{
	char *mask;
	aClient *acptr;

	if (check_registered_user(sptr))
		return 0;
    
	if (parc > 2)
	    {
		if (hunt_server(cptr, sptr, ":%s LINKS %s :%s", 1, parc, parv)
				!= HUNTED_ISME)
			return 0;
		mask = parv[2];
	    }
	else
		mask = parc < 2 ? NULL : parv[1];

	for (acptr = client, (void)collapse(mask); acptr; acptr = acptr->next) 
	    {
		if (!IsServer(acptr) && !IsMe(acptr))
			continue;
		if (!BadPtr(mask) && match(mask, acptr->name))
			continue;
		sendto_one(sptr, rpl_str(RPL_LINKS),
			   me.name, parv[0], acptr->name, acptr->serv->up->name,
			   acptr->hopcount, acptr->serv->prot,
			   (acptr->info[0] ? acptr->info :
			   "(Unknown Location)"));
	    }

	sendto_one(sptr, rpl_str(RPL_ENDOFLINKS), me.name, parv[0],
		   BadPtr(mask) ? "*" : mask);
	return 0;
}

/*
** m_summon should be redefined to ":prefix SUMMON host user" so
** that "hunt_server"-function could be used for this too!!! --msa
** As of 2.7.1e, this was the case. -avalon
**
**	parv[0] = sender prefix
**	parv[1] = user
**	parv[2] = server
**	parv[3] = channel (optional)
*/
int	m_summon(cptr, sptr, parc, parv)
aClient *sptr, *cptr;
int	parc;
char	*parv[];
{
	char	*host, *user, *chname;
#ifdef	ENABLE_SUMMON
	char	hostbuf[17], namebuf[10], linebuf[10];
#  ifdef LEAST_IDLE
        char	linetmp[10], ttyname[15]; /* Ack */
        struct	stat stb;
        time_t	ltime = (time_t)0;
#  endif
	int	fd, flag = 0;
#endif

	if (check_registered_user(sptr))
		return 0;
	if (parc < 2 || *parv[1] == '\0')
	    {
		sendto_one(sptr, err_str(ERR_NORECIPIENT),
			   me.name, parv[0], "SUMMON");
		return 0;
	    }
	user = parv[1];
	host = (parc < 3 || BadPtr(parv[2])) ? me.name : parv[2];
	chname = (parc > 3) ? parv[3] : "*";
	/*
	** Summoning someone on remote server, find out which link to
	** use and pass the message there...
	*/
	parv[1] = user;
	parv[2] = host;
	parv[3] = chname;
	parv[4] = NULL;
	if (hunt_server(cptr, sptr, ":%s SUMMON %s %s %s", 2, parc, parv) ==
	    HUNTED_ISME)
	    {
#ifdef ENABLE_SUMMON
		if ((fd = utmp_open()) == -1)
		    {
			sendto_one(sptr, err_str(ERR_FILEERROR),
				   me.name, parv[0], "open", UTMP);
			return 0;
		    }
#  ifndef LEAST_IDLE
		while ((flag = utmp_read(fd, namebuf, linebuf, hostbuf,
					 sizeof(hostbuf))) == 0) 
			if (StrEq(namebuf,user))
				break;
#  else
                /* use least-idle tty, not the first
                 * one we find in utmp. 10/9/90 Spike@world.std.com
                 * (loosely based on Jim Frost jimf@saber.com code)
                 */
		
                while ((flag = utmp_read(fd, namebuf, linetmp, hostbuf,
					 sizeof(hostbuf))) == 0)
		    {
			if (StrEq(namebuf,user))
			    {
				(void)sprintf(ttyname,"/dev/%s",linetmp);
				if (stat(ttyname,&stb) == -1)
				    {
					sendto_one(sptr,
						   err_str(ERR_FILEERROR),
						   me.name, sptr->name,
						   "stat", ttyname);
					return 0;
				    }
				if (!ltime)
				    {
					ltime= stb.st_mtime;
					(void)strcpy(linebuf,linetmp);
				    }
				else if (stb.st_mtime > ltime) /* less idle */
				    {
					ltime= stb.st_mtime;
					(void)strcpy(linebuf,linetmp);
				    }
			    }
		    }
#  endif
		(void)utmp_close(fd);
#  ifdef LEAST_IDLE
                if (ltime == 0)
#  else
		if (flag == -1)
#  endif
			sendto_one(sptr, err_str(ERR_NOLOGIN),
				   me.name, parv[0], user);
		else
			summon(sptr, user, linebuf, chname);
#else
		sendto_one(sptr, err_str(ERR_SUMMONDISABLED),
			   me.name, parv[0]);
#endif /* ENABLE_SUMMON */
	    }
	return 0;
}


/*
** m_stats
**	parv[0] = sender prefix
**	parv[1] = statistics selector (defaults to Message frequency)
**	parv[2] = server name (current server defaulted, if omitted)
**
**	Currently supported are:
**		M = Message frequency (the old stat behaviour)
**		L = Local Link statistics
**              C = Report C and N configuration lines
*/
/*
** m_stats/stats_conf
**    Report N/C-configuration lines from this server. This could
**    report other configuration lines too, but converting the
**    status back to "char" is a bit akward--not worth the code
**    it needs...
**
**    Note:   The info is reported in the order the server uses
**            it--not reversed as in ircd.conf!
*/

static int report_array[15][3] = {
		{ CONF_CONNECT_SERVER,    RPL_STATSCLINE, 'C'},
		{ CONF_NOCONNECT_SERVER,  RPL_STATSNLINE, 'N'},
		{ CONF_CLIENT,            RPL_STATSILINE, 'I'},
		{ CONF_KILL,              RPL_STATSKLINE, 'K'},
		{ CONF_QUARANTINED_SERVER,RPL_STATSQLINE, 'Q'},
		{ CONF_LEAF,		  RPL_STATSLLINE, 'L'},
		{ CONF_OPERATOR,	  RPL_STATSOLINE, 'O'},
		{ CONF_HUB,		  RPL_STATSHLINE, 'H'},
		{ CONF_LOCOP,		  RPL_STATSOLINE, 'o'},
		{ CONF_CRULEALL,	  RPL_STATSDLINE, 'D'},
		{ CONF_CRULEAUTO,	  RPL_STATSDLINE, 'd'},
		{ CONF_SERVICE,		  RPL_STATSSLINE, 'S'},
		{ CONF_UWORLD,		  RPL_STATSULINE, 'U'},
		{ CONF_TLINES,            RPL_STATSTLINE, 'T'},
		{ 0, 0}
				};

static	void	report_configured_links(sptr, mask)
aClient *sptr;
int	mask;
{
	static	char	null[] = "<NULL>";
	aConfItem *tmp;
	int	*p, port;
	char	c, *host, *pass, *name;
	
	for (tmp = conf; tmp; tmp = tmp->next)
		if (tmp->status & mask)
		    {
			for (p = &report_array[0][0]; *p; p += 3)
				if (*p == tmp->status)
					break;
			if (!*p)
				continue;
			c = (char)*(p+2);
			host = BadPtr(tmp->host) ? null : tmp->host;
			pass = BadPtr(tmp->passwd) ? null : tmp->passwd;
			name = BadPtr(tmp->name) ? null : tmp->name;
			port = (int)tmp->port;
			/*
			 * On K line the passwd contents can be
			/* displayed on STATS reply. 	-Vesa
			 */
			if (tmp->status == CONF_KILL)
				sendto_one(sptr, rpl_str(p[1]), me.name,
					   sptr->name, c, host, pass,
					   name, port, get_conf_class(tmp));
			/* connect rules are classless */
			else if (tmp->status & CONF_CRULE)
				sendto_one(sptr, rpl_str(p[1]), me.name,
					   sptr->name, c, host, name);
			else if (tmp->status & CONF_TLINES)
			        sendto_one(sptr, rpl_str(p[1]), me.name,
					   sptr->name, c, host, pass);
			else
				sendto_one(sptr, rpl_str(p[1]), me.name,
					   sptr->name, c, host, name, port,
					   get_conf_class(tmp));
		    }
	return;
}

int	m_stats(cptr, sptr, parc, parv)
aClient *cptr, *sptr;
int	parc;
char	*parv[];
    {
      	static	char	Sformat[]  = ":%s %d %s Connection SendQ SendM SendKBytes RcveM RcveKBytes :Open since";
	static	char	Lformat[]  = ":%s %d %s %s %u %u %u %u %u :%u";
	struct	Message	*mptr;
	aClient	*acptr;
	aGline	*agline, *a2gline;
	char	stat = parc > 1 ? parv[1][0] : '\0';
	Reg1	int	i;
	int	doall = 0, wilds = 0;
	char	*name;

	if (check_registered(sptr))
		return 0;

	if (hunt_server(cptr,sptr,":%s STATS %s :%s",2,parc,parv)!=HUNTED_ISME)
		return 0;

	if (parc > 2)
	    {
		name = parv[2];
		if (!mycmp(name, me.name))
			doall = 2;
		else if (matches(name, me.name) == 0)
			doall = 1;
		if (index(name, '*') || index(name, '?'))
			wilds = 1;
	    }
	else
		name = me.name;

	switch (stat)
	{
	case 'L' : case 'l' :
		/*
		 * send info about connections which match, or all if the
		 * mask matches me.name.  Only restrictions are on those who
		 * are invisible not being visible to 'foreigners' who use
		 * a wild card based search to list it.
		 */
	  	sendto_one(sptr, Sformat, me.name, RPL_STATSLINKINFO, parv[0]);
		for (i = 0; i <= highest_fd; i++)
		    {
			if (!(acptr = local[i]))
				continue;
			if (IsInvisible(acptr) && (doall || wilds) &&
			    !(MyConnect(sptr) && IsOper(sptr)) &&
			    !IsAnOper(acptr) && (acptr != sptr))
				continue;
			if (!doall && wilds && matches(name, acptr->name))
				continue;
			if (!(doall || wilds) && mycmp(name, acptr->name))
				continue;
			sendto_one(sptr, Lformat, me.name,
				   RPL_STATSLINKINFO, parv[0],
				   (isupper(stat)) ?
				   get_client_name(acptr, TRUE) :
				   get_client_name(acptr, FALSE),
				   (int)DBufLength(&acptr->sendQ),
				   (int)acptr->sendM, (int)acptr->sendK,
				   (int)acptr->receiveM, (int)acptr->receiveK,
				   time(NULL) - acptr->firsttime);
		    }
		break;
	case 'C' : case 'c' :
                report_configured_links(sptr, CONF_CONNECT_SERVER|
					CONF_NOCONNECT_SERVER);
		break;
	case 'G' : case 'g' :
		/* send glines */
		for (agline = gline, a2gline = NULL; agline;
		     agline = agline->next) {
		  if (agline->expire <= TStime()) { /* handle expired glines */
		    free_gline(agline, a2gline);
		    agline = a2gline ? a2gline : gline; /* make sure to splice
							   list together */
		    if (!agline) break; /* last gline; break out of loop */
		    continue; /* continue! */
		  }
		  sendto_one(sptr, rpl_str(RPL_STATSGLINE), me.name,
			     sptr->name, 'G', agline->name, agline->host,
			     agline->expire, agline->reason);
		  a2gline = agline;
		}
		break;
	case 'H' : case 'h' :
                report_configured_links(sptr, CONF_HUB|CONF_LEAF);
		break;
	case 'I' : case 'i' :
		report_configured_links(sptr, CONF_CLIENT);
		break;
	case 'K' : case 'k' :
		report_configured_links(sptr, CONF_KILL);
		break;
	case 'M' : case 'm' :
		for (mptr = msgtab; mptr->cmd; mptr++)
			if (mptr->count)
				sendto_one(sptr, rpl_str(RPL_STATSCOMMANDS),
					   me.name, parv[0], mptr->cmd,
					   mptr->count, mptr->bytes);
		break;
	case 'o' : case 'O' :
		report_configured_links(sptr, CONF_OPS);
		break;
	case 'Q' : case 'q' :
		report_configured_links(sptr, CONF_QUARANTINED_SERVER);
		break;
	case 'R' : case 'r' :
#ifdef DEBUGMODE
		send_usage(sptr,parv[0]);
#endif
		break;
	case 'D' :
		report_configured_links(sptr, CONF_CRULEALL);
		break;
	case 'd' :
		report_configured_links(sptr, CONF_CRULE);
		break;
	case 'S' : case 's' :
		report_configured_links(sptr, CONF_SERVICE);
		break;
	case 't' : 
		tstats(sptr, parv[0]);
		break;
	case 'T' :
	        report_configured_links(sptr, CONF_TLINES);
		break;
	case 'U' :
                report_configured_links(sptr, CONF_UWORLD);
		break;
	case 'u' :
	    {
		register time_t nowr;

		nowr = now - me.since;
		sendto_one(sptr, rpl_str(RPL_STATSUPTIME), me.name, parv[0],
			   nowr/86400, (nowr/3600)%24, (nowr/60)%60, nowr%60);
                sendto_one(sptr, rpl_str(RPL_STATSCONN), me.name, parv[0],
			   max_connection_count, max_client_count);
		break;
	    }
        case 'W' : case 'w' :
                calc_load(sptr, parv[0]);
                break;
	case 'X' : case 'x' :
#ifdef	DEBUGMODE
		send_listinfo(sptr, parv[0]);
#endif
		break;
	case 'Y' : case 'y' :
		report_classes(sptr);
		break;
	case 'Z' : case 'z' :
		count_memory(sptr, parv[0]);
		break;
	default :
		stat = '*';
		break;
	}
	sendto_one(sptr, rpl_str(RPL_ENDOFSTATS), me.name, parv[0], stat);
	return 0;
    }

/*
** m_users
**	parv[0] = sender prefix
**	parv[1] = servername
*/
int	m_users(cptr, sptr, parc, parv)
aClient *cptr, *sptr;
int	parc;
char	*parv[];
{
#ifdef ENABLE_USERS
	char	namebuf[10],linebuf[10],hostbuf[17];
	int	fd, flag = 0;
#endif

	if (check_registered_user(sptr))
		return 0;

	if (hunt_server(cptr,sptr,":%s USERS :%s",1,parc,parv) == HUNTED_ISME)
	    {
#ifdef ENABLE_USERS
		if ((fd = utmp_open()) == -1)
		    {
			sendto_one(sptr, err_str(ERR_FILEERROR),
				   me.name, parv[0], "open", UTMP);
			return 0;
		    }

		sendto_one(sptr, rpl_str(RPL_USERSSTART), me.name, parv[0]);
		while (utmp_read(fd, namebuf, linebuf,
				 hostbuf, sizeof(hostbuf)) == 0)
		    {
			flag = 1;
			sendto_one(sptr, rpl_str(RPL_USERS), me.name, parv[0],
				   namebuf, linebuf, hostbuf);
		    }
		if (flag == 0) 
			sendto_one(sptr, rpl_str(RPL_NOUSERS),
				   me.name, parv[0]);

		sendto_one(sptr, rpl_str(RPL_ENDOFUSERS), me.name, parv[0]);
		(void)utmp_close(fd);
#else
		sendto_one(sptr, err_str(ERR_USERSDISABLED), me.name, parv[0]);
#endif
	    }
	return 0;
}

/*
** Note: At least at protocol level ERROR has only one parameter,
** although this is called internally from other functions
** --msa
**
**	parv[0] = sender prefix
**	parv[*] = parameters
*/
int	m_error(cptr, sptr, parc, parv)
aClient *cptr, *sptr;
int	parc;
char	*parv[];
    {
	Reg1	char	*para;

	para = (parc > 1 && *parv[1] != '\0') ? parv[1] : "<>";

	Debug((DEBUG_ERROR,"Received ERROR message from %s: %s",
	      sptr->name, para));
	/*
	** Ignore error messages generated by normal user clients
	** (because ill-behaving user clients would flood opers
	** screen otherwise). Pass ERROR's from other sources to
	** the local operator...
	*/
	if (IsPerson(cptr) || IsService(cptr))
		return 0;
	if (cptr == sptr)
		sendto_ops("ERROR :from %s -- %s",
			   get_client_name(cptr, FALSE), para);
	else
		sendto_ops("ERROR :from %s via %s -- %s", sptr->name,
			   get_client_name(cptr,FALSE), para);
	if (IsUnknown(cptr))
		exit_client_msg(cptr, cptr, &me,
		    ":%s %d %s ERROR :Register first.", me.name,
		    ERR_NOTREGISTERED, sptr->name);

	return 0;
    }

/*
** m_help
**	parv[0] = sender prefix
*/
int	m_help(cptr, sptr, parc, parv)
aClient *cptr, *sptr;
int	parc;
char	*parv[];
    {
	int i;

	for (i = 0; msgtab[i].cmd; i++)
		sendto_one(sptr,":%s NOTICE %s :%s",
			   me.name, parv[0], msgtab[i].cmd);
	return 0;
    }

/*
 * parv[0] = sender
 * parv[1] = host/server mask.
 * parv[2] = server to query
 */
int	 m_lusers(cptr, sptr, parc, parv)
aClient *cptr, *sptr;
int	parc;
char	*parv[];
    {
	int	s_count = 0, c_count = 0, u_count = 0, i_count = 0;
	int	o_count = 0, m_client = 0, m_client_local = 0, m_server = 0;
        char mydom_mask[HOSTLEN + 1];
	aClient *acptr;

	if (check_registered_user(sptr))
		return 0;

	if (parc > 2)
		if(hunt_server(cptr, sptr, ":%s LUSERS %s :%s", 2, parc, parv)
				!= HUNTED_ISME)
			return 0;

        mydom_mask[0] = '*';
        strncpy(&mydom_mask[1], DOMAINNAME, HOSTLEN - 1);

	(void)collapse(parv[1]);
	for (acptr = client; acptr; acptr = acptr->next)
	    {
		if (parc>1)
			if (!IsServer(acptr) && acptr->user)
			    {
				if (match(parv[1], acptr->user->server->name))
					continue;
			    }
			else
	      			if (match(parv[1], acptr->name))
					continue;

		switch (acptr->status)
		{
		case STAT_SERVER:
			if (MyConnect(acptr))
				m_server++;
		case STAT_ME:
			s_count++;
			break;
		case STAT_CLIENT:
			if (IsOper(acptr))
	        		o_count++;
#ifdef	SHOW_INVISIBLE_LUSERS
			if (MyConnect(acptr)) {
		  		m_client++;
				if (matches(mydom_mask, acptr->sockhost) == 0)
				  m_client_local++;
			      }
			if (!IsInvisible(acptr))
				c_count++;
			else
				i_count++;
#else
			if (MyConnect(acptr))
			    {
				if (IsInvisible(acptr))
				    {
					if (IsAnOper(sptr))
						m_client++;
				    }
				else
					m_client++;
			    }
	 		if (!IsInvisible(acptr))
				c_count++;
			else
				i_count++;
#endif
			break;
		case STAT_PING:
			break;
		default:
			u_count++;
			break;
	 	}
	     }
#ifndef	SHOW_INVISIBLE_LUSERS
	if (IsAnOper(sptr) && i_count)
#endif
	sendto_one(sptr, rpl_str(RPL_LUSERCLIENT), me.name, parv[0],
		   c_count, i_count, s_count);
#ifndef	SHOW_INVISIBLE_LUSERS
	else
		sendto_one(sptr,
			":%s %d %s :There are %d users on %d servers", me.name,
			    RPL_LUSERCLIENT, parv[0], c_count, s_count);
#endif
	if (o_count)
		sendto_one(sptr, rpl_str(RPL_LUSEROP),
			   me.name, parv[0], o_count);
	if (u_count > 0)
		sendto_one(sptr, rpl_str(RPL_LUSERUNKNOWN),
			   me.name, parv[0], u_count);
	if ((c_count = count_channels(sptr))>0)
		sendto_one(sptr, rpl_str(RPL_LUSERCHANNELS),
			   me.name, parv[0], count_channels(sptr));
	sendto_one(sptr, rpl_str(RPL_LUSERME),
		   me.name, parv[0], m_client, m_server);
        sendto_one(sptr,
	       ":%s NOTICE %s :Highest connection count: %d (%d clients)",
               me.name, parv[0], max_connection_count, max_client_count);
        if (m_client > max_client_count)
          max_client_count = m_client;
        if ((m_client + m_server) > max_connection_count) {
          max_connection_count = m_client + m_server;
          if (max_connection_count % 10 == 0)  /* only send on even tens */
            sendto_ops("Maximum connections: %d (%d clients)",
                     max_connection_count, max_client_count);
        }
        current_load_data.local_count = m_client_local;
        current_load_data.client_count = m_client;
        current_load_data.conn_count = m_client + m_server;
	return 0;
    }

  
/***********************************************************************
 * m_connect() - Added by Jto 11 Feb 1989
 ***********************************************************************/

/*
** m_connect
**	parv[0] = sender prefix
**	parv[1] = servername
**	parv[2] = port number
**	parv[3] = remote server
*/
int	m_connect(cptr, sptr, parc, parv)
aClient *cptr, *sptr;
int	parc;
char	*parv[];
    {
	int	port, tmpport, retval;
	aConfItem *aconf, *cconf;
	aClient *acptr;

	if (!IsPrivileged(sptr))
	    {
		sendto_one(sptr, err_str(ERR_NOPRIVILEGES), me.name, parv[0]);
		return -1;
	    }

	if (IsLocOp(sptr) && parc > 3)	/* Only allow LocOps to make */
		return 0;		/* local CONNECTS --SRB      */

	if (parc > 3 && MyClient(sptr))
	{
	  aClient *acptr2, *acptr3;
	  if (!(acptr3 = find_match_server(parv[3])))
	  {
	      sendto_one(sptr, err_str(ERR_NOSUCHSERVER),
		  me.name, parv[0], parv[3]);
	      return 0;
	  }
	  
	  /* Look for closest matching server */
	  for (acptr2 = acptr3; acptr2 != &me; acptr2 = acptr2->serv->up)
	    if (!match(parv[3], acptr2->name))
	      acptr3 = acptr2;

	  parv[3] = acptr3->name;
	}

	if (hunt_server(cptr,sptr,":%s CONNECT %s %s :%s",
		       3,parc,parv) != HUNTED_ISME)
		return 0;

	if (parc < 2 || *parv[1] == '\0')
	    {
		sendto_one(sptr, err_str(ERR_NEEDMOREPARAMS),
			   me.name, parv[0], "CONNECT");
		return -1;
	    }

	if ((acptr = find_server(parv[1], NULL)))
	    {
		sendto_one(sptr, ":%s NOTICE %s :Connect: Server %s %s %s.",
			   me.name, parv[0], parv[1], "already exists from",
			   acptr->from->name);
		return 0;
	    }

	for (aconf = conf; aconf; aconf = aconf->next)
		if (aconf->status == CONF_CONNECT_SERVER &&
		    matches(parv[1], aconf->name) == 0)
		  break;
	/* Checked first servernames, then try hostnames. */
	if (!aconf)
        	for (aconf = conf; aconf; aconf = aconf->next)
                	if (aconf->status == CONF_CONNECT_SERVER &&
                            (matches(parv[1], aconf->host) == 0 ||
                             matches(parv[1], index(aconf->host, '@')+1) == 0))
                  		break;

	if (!aconf)
	    {
	      sendto_one(sptr,
			 "NOTICE %s :Connect: Host %s not listed in ircd.conf",
			 parv[0], parv[1]);
	      return 0;
	    }
	/*
	** Get port number from user, if given. If not specified,
	** use the default form configuration structure. If missing
	** from there, then use the precompiled default.
	*/
	tmpport = port = aconf->port;
	if (parc > 2 && !BadPtr(parv[2]))
	    {
		if ((port = atoi(parv[2])) <= 0)
		    {
			sendto_one(sptr,
				   "NOTICE %s :Connect: Illegal port number",
				   parv[0]);
			return 0;
		    }
	    }
	else if (port <= 0 && (port = PORTNUM) <= 0)
	    {
		sendto_one(sptr, ":%s NOTICE %s :Connect: missing port number",
			   me.name, parv[0]);
		return 0;
	    }

        /*
	** Evaluate connection rules...  If no rules found, allow the
        ** connect.  Otherwise stop with the first true rule (ie: rules
        ** are ored together.  Oper connects are effected only by D
        ** lines (CRULEALL) not d lines (CRULEAUTO).
        */
	for (cconf = conf; cconf; cconf = cconf->next)
	  if ((cconf->status == CONF_CRULEALL) &&
	      (matches(cconf->host, aconf->name) == 0))
	    if (crule_eval (cconf->passwd))
	      {
		sendto_one(sptr,
			   "NOTICE %s :Connect: Disallowed by rule: %s",
			   parv[0], cconf->name);
		return 0;
	      }

	/*
	** Notify all operators about remote connect requests
	*/
	if (!IsAnOper(cptr))
	    {
		sendto_ops_butone(NULL, &me,
				  ":%s WALLOPS :Remote CONNECT %s %s from %s",
				   me.name, parv[1], parv[2] ? parv[2] : "",
				   get_client_name(sptr,FALSE));
#if defined(USE_SYSLOG) && defined(SYSLOG_CONNECT)
		syslog(LOG_DEBUG, "CONNECT From %s : %s %d", parv[0], parv[1], parv[2] ? parv[2] : "");
#endif
	    }
	aconf->port = port;
	switch (retval = connect_server(aconf, sptr, NULL))
	{
	case 0:
		sendto_one(sptr,
			   ":%s NOTICE %s :*** Connecting to %s[%s].",
			   me.name, parv[0], aconf->host, aconf->name);
		break;
	case -1:
		/* Comments already sent */
		break;
	case -2:
		sendto_one(sptr, ":%s NOTICE %s :*** Host %s is unknown.",
			   me.name, parv[0], aconf->host);
		break;
	default:
		sendto_one(sptr,
			   ":%s NOTICE %s :*** Connection to %s failed: %s",
			   me.name, parv[0], aconf->host, strerror(retval));
	}
	aconf->port = tmpport;
	return 0;
    }

/*
** m_wallops (write to *all* opers currently online)
**	parv[0] = sender prefix
**	parv[1] = message text
*/
int	m_wallops(cptr, sptr, parc, parv)
aClient *cptr, *sptr;
int	parc;
char	*parv[];
    {
	char	*message;

	message = parc > 1 ? parv[1] : NULL;

	if (BadPtr(message))
	    {
		sendto_one(sptr, err_str(ERR_NEEDMOREPARAMS),
			   me.name, parv[0], "WALLOPS");
		return 0;
	    }

	if (!IsServer(sptr) && MyConnect(sptr) && !IsAnOper(sptr))
	    {
	      sendto_one(sptr, err_str(ERR_NOPRIVILEGES), me.name, parv[0]);
	      return 0;
	    }
	sendto_ops_butone(IsServer(cptr) ? cptr : NULL, sptr,
			":%s WALLOPS :%s", parv[0], message);
#ifdef	USE_SERVICES
	check_services_butone(SERVICE_WANT_WALLOP, sptr, ":%s WALLOP :%s",
				parv[0], message);
#endif
	return 0;
    }

time_t TSoffset=0; /* Global variable; Offset of timestamps to system clock */

/*
** m_time
**	parv[0] = sender prefix
**	parv[1] = servername
*/

int	m_time(cptr, sptr, parc, parv)
aClient *cptr, *sptr;
int	parc;
char	*parv[];
    {
	if (check_registered_user(sptr))
		return 0;
	if (hunt_server(cptr,sptr,":%s TIME :%s",1,parc,parv) == HUNTED_ISME)
		sendto_one(sptr, rpl_str(RPL_TIME), me.name,
			   parv[0], me.name, TStime(), TSoffset, date((long)0));
	return 0;
    }

/*
** m_settime
**	parv[0] = sender prefix
**	parv[1] = new time
**      parv[2] = servername
*/
m_settime(cptr, sptr, parc, parv)
aClient	*cptr, *sptr;
int	parc;
char	*parv[];
{
  time_t t;
  long int dt;
  static char tbuf[11];

  if (!IsPrivileged(sptr)) return 0;

  if (parc == 2 && MyClient(sptr)) parv[parc++]=me.name;

  if (parc < 3)
  {
    sendto_one(sptr, err_str(ERR_NEEDMOREPARAMS),
        me.name, parv[0], "SETTIME");
    return 0;
  }

  t=atoi(parv[1]);
  dt=TStime()-t;

  if (t < 779557906 || dt < -9000000)
  {
      sendto_one(sptr, ":%s NOTICE %s :SETTIME: Bad value", me.name, parv[0]);
      return 0;
  }

  if (IsServer(sptr))
    sendto_serv_butone(cptr, ":%s SETTIME %s :%s",parv[0],parv[1],parv[2]);
  else
  {
    sprintf(tbuf,"%lu",TStime());
    parv[1]=tbuf;
    if (hunt_server(cptr,sptr,":%s SETTIME %s :%s",2,parc,parv) != HUNTED_ISME)
      return 0;
  }

  sendto_ops("SETTIME from %s, clock is set %ld seconds %s",
      get_client_name(sptr,FALSE), (dt < 0) ? -dt : dt,
      (dt < 0) ? "forwards" : "backwards");
  TSoffset-=dt;
  if (IsPerson(sptr))
    sendto_one(sptr, ":%s NOTICE %s :clock is set %ld seconds %s", me.name,
        parv[0], (dt < 0) ? -dt : dt, (dt < 0) ? "forwards" : "backwards");
  return 0;
}

char *militime(sec, usec)
char *sec, *usec;
{
  struct timeval tv;
  static char timebuf[18];

  (void)gettimeofday(&tv,NULL);
  if (sec && usec)
    sprintf(timebuf,"%ld",
        (tv.tv_sec-atoi(sec))*1000+(tv.tv_usec-atoi(usec))/1000);
  else
    sprintf(timebuf,"%ld %ld",tv.tv_sec,tv.tv_usec);
  return timebuf;
}

/*
** m_rping  -- by Run
**
**	parv[0] = sender prefix
**	parv[1] = pinged server
**	parv[2] = from person: start server ; from server: sender
**	parv[3] = start time in s  ;from person: Optional remark
**      parv[4] = start time in us
*/
int m_rping(cptr, sptr, parc, parv)
aClient *cptr, *sptr;
int     parc;
char    *parv[];
{
  aClient *acptr;

  if (!IsPrivileged(sptr)) return 0;

  if (parc < (IsAnOper(sptr) ? (MyConnect(sptr) ? 2 : 3) : 6))
  {
    sendto_one(sptr, err_str(ERR_NEEDMOREPARAMS),
        me.name, parv[0], "RPING");
    return 0;
  }
  if (MyClient(sptr))
  {
    if (parc == 2)
      parv[parc++]=me.name;
    else if (!(acptr=find_match_server(parv[2])))
    {
      parv[3]=parv[2];
      parv[2]=me.name;
      parc++;
    }
    else
      parv[2]=acptr->name;
    if (parc == 3) parv[parc++]="<No client start time>";
  }

  if (IsAnOper(sptr))
  {
    if (hunt_server(cptr,sptr,":%s RPING %s %s :%s",2,parc,parv) != HUNTED_ISME)
      return 0;
    if (!(acptr=find_match_server(parv[1])) || !IsServer(acptr))
    {
      sendto_one(sptr, err_str(ERR_NOSUCHSERVER), me.name, parv[0], parv[1]);
      return 0;
    }
    sendto_one(acptr,":%s RPING %s %s %s :%s",me.name,acptr->name,sptr->name,
        militime(NULL, NULL), parv[3]);
  }
  else
  {
    if (hunt_server(cptr,sptr,":%s RPING %s %s %s %s :%s",1,parc,parv)
        != HUNTED_ISME)
      return 0;
    sendto_one(cptr,":%s RPONG %s %s %s %s :%s",me.name,parv[0],
        parv[2],parv[3],parv[4],parv[5]);
  }
  return 0;
}

/*
** m_rpong  -- by Run too :)
**
**	parv[0] = sender prefix
**	parv[1] = from pinged server: start server; from start server: sender
**	parv[2] = from pinged server: sender; from start server: pinged server
**	parv[3] = pingtime in ms
**      parv[4] = client info (for instance start time)
*/
int m_rpong(cptr, sptr, parc, parv)
aClient *cptr, *sptr;
int     parc;
char    *parv[];
{
  aClient *acptr;

  if (!IsServer(sptr)) return 0;

  if (parc < 5)
  {
    sendto_one(sptr, err_str(ERR_NEEDMOREPARAMS),
        me.name, parv[0], "RPING");
    return 0;
  }

  if (!(acptr=find_client(parv[1],(aClient *)NULL)))
    return 0; 

  if (!IsMe(acptr))
  {
    if (IsServer(acptr) && parc > 5)
    {
      sendto_one(acptr,":%s RPONG %s %s %s %s :%s",
	  parv[0],parv[1],parv[2],parv[3],parv[4],parv[5]);
      return 0;
    }
  }
  else
  {
    parv[1]=parv[2];
    parv[2]=sptr->name;
    parv[0]=me.name;
    parv[3]=militime(parv[3],parv[4]);
    parv[4]=parv[5];
    if (!(acptr=find_person(parv[1],(aClient *)NULL)))
      return 0; /* No bouncing between servers ! */
  }

  sendto_one(acptr,":%s RPONG %s %s %s :%s",
      parv[0],parv[1],parv[2],parv[3],parv[4]);
  return 0;
}

/*
** m_admin
**	parv[0] = sender prefix
**	parv[1] = servername
*/
int	m_admin(cptr, sptr, parc, parv)
aClient *cptr, *sptr;
int	parc;
char	*parv[];
    {
	aConfItem *aconf;

/* admin should be available to all (to get contact point for I lines,...) --dl
	if (check_registered(sptr))
		return 0; 
*/
	if (MyConnect(sptr) && parc > 1)
	{
	  aClient *acptr;
	  if (!(acptr = find_match_server(parv[1])))
	  {
	      sendto_one(sptr, err_str(ERR_NOSUCHSERVER),
		  me.name, parv[0], parv[1]);
	      return 0;
	  }
	  parv[1] = acptr->name;
        }
	if (hunt_server(cptr,sptr,":%s ADMIN :%s",1,parc,parv) != HUNTED_ISME)
		return 0;
	if ((aconf = find_admin()))
	    {
		sendto_one(sptr, rpl_str(RPL_ADMINME),
			   me.name, parv[0], me.name);
		sendto_one(sptr, rpl_str(RPL_ADMINLOC1),
			   me.name, parv[0], aconf->host);
		sendto_one(sptr, rpl_str(RPL_ADMINLOC2),
			   me.name, parv[0], aconf->passwd);
		sendto_one(sptr, rpl_str(RPL_ADMINEMAIL),
			   me.name, parv[0], aconf->name);
	    }
	else
		sendto_one(sptr, err_str(ERR_NOADMININFO),
			   me.name, parv[0], me.name);
	return 0;
    }

#if defined(OPER_REHASH) || defined(LOCOP_REHASH)
/*
** m_rehash
**
*/
int	m_rehash(cptr, sptr, parc, parv)
aClient	*cptr, *sptr;
int	parc;
char	*parv[];
{
#ifndef	LOCOP_REHASH
	if (!MyClient(sptr) || !IsOper(sptr))
#else
# ifdef	OPER_REHASH
	if (!MyClient(sptr) || !IsAnOper(sptr))
# else
	if (!MyClient(sptr) || !IsLocOp(sptr))
# endif
#endif
	    {
		sendto_one(sptr, err_str(ERR_NOPRIVILEGES), me.name, parv[0]);
		return 0;
	    }
	sendto_one(sptr, rpl_str(RPL_REHASHING), me.name, parv[0], configfile);
	sendto_ops("%s is rehashing Server config file", parv[0]);
#ifdef USE_SYSLOG
	syslog(LOG_INFO, "REHASH From %s\n", get_client_name(sptr, FALSE));
#endif
	return rehash(cptr, sptr, (parc > 1) ? ((*parv[1] == 'q')?2:0) : 0);
}
#endif

#if defined(OPER_RESTART) || defined(LOCOP_RESTART)
/*
** m_restart
**
*/
int	m_restart(cptr, sptr, parc, parv)
aClient *cptr, *sptr;
int	parc;
char	*parv[];
{
#ifndef	LOCOP_RESTART
	if (!MyClient(sptr) || !IsOper(sptr))
#else
# ifdef	OPER_RESTART
	if (!MyClient(sptr) || !IsAnOper(sptr))
# else
	if (!MyClient(sptr) || !IsLocOp(sptr))
# endif
#endif
	    {
		sendto_one(sptr, err_str(ERR_NOPRIVILEGES), me.name, parv[0]);
		return 0;
	    }
#ifdef USE_SYSLOG
	syslog(LOG_WARNING, "Server RESTART by %s\n",
		get_client_name(sptr,FALSE));
#endif
	server_reboot();
	return 0;
}
#endif

/*
** m_trace
**	parv[0] = sender prefix
**	parv[1] = servername
*/
int	m_trace(cptr, sptr, parc, parv)
aClient *cptr, *sptr;
int	parc;
char	*parv[];
{
	Reg1	int	i;
	Reg2	aClient	*acptr;
	aClass	*cltmp;
	char	*tname;
	int	doall, link_s[MAXCONNECTIONS], link_u[MAXCONNECTIONS];
	int	cnt = 0, wilds, dow;

	if (check_registered(sptr))
		return 0;

	if (parc > 2)
		if (hunt_server(cptr, sptr, ":%s TRACE %s :%s",
				2, parc, parv))
			return 0;

	if (parc > 1)
		tname = parv[1];
	else
		tname = me.name;

	switch (hunt_server(cptr, sptr, ":%s TRACE :%s", 1, parc, parv))
	{
	case HUNTED_PASS: /* note: gets here only if parv[1] exists */
	    {
		aClient	*ac2ptr;

		ac2ptr = next_client(client, tname);
		sendto_one(sptr, rpl_str(RPL_TRACELINK), me.name, parv[0],
			   version, debugmode, tname, ac2ptr->from->name);
		return 0;
	    }
	case HUNTED_ISME:
		break;
	default:
		return 0;
	}

	doall = (parv[1] && (parc > 1)) ? !matches(tname, me.name): TRUE;
	wilds = !parv[1] || index(tname, '*') || index(tname, '?');
	dow = wilds || doall;

	for (i = 0; i < MAXCONNECTIONS; i++)
		link_s[i] = 0, link_u[i] = 0;

	if (doall)
		for (acptr = client; acptr; acptr = acptr->next)
#ifdef	SHOW_INVISIBLE_LUSERS
			if (IsPerson(acptr))
				link_u[acptr->from->fd]++;
#else
			if (IsPerson(acptr) &&
			    (!IsInvisible(acptr) || IsOper(sptr)))
				link_u[acptr->from->fd]++;
#endif
			else if (IsServer(acptr))
				link_s[acptr->from->fd]++;

	/* report all direct connections */
	
	for (i = 0; i <= highest_fd; i++)
	    {
		char	*name;
		int	class;

		if (!(acptr = local[i])) /* Local Connection? */
			continue;
		if (IsInvisible(acptr) && dow &&
		    !(MyConnect(sptr) && IsOper(sptr)) &&
		    !IsAnOper(acptr) && (acptr != sptr))
			continue;
		if (!doall && wilds && matches(tname, acptr->name))
			continue;
		if (!dow && mycmp(tname, acptr->name))
			continue;
		name = get_client_name(acptr,FALSE);
		class = get_client_class(acptr);

		switch(acptr->status)
		{
		case STAT_CONNECTING:
			sendto_one(sptr, rpl_str(RPL_TRACECONNECTING), me.name,
				   parv[0], class, name);
			cnt++;
			break;
		case STAT_HANDSHAKE:
			sendto_one(sptr, rpl_str(RPL_TRACEHANDSHAKE), me.name,
				   parv[0], class, name);
			cnt++;
			break;
		case STAT_ME:
			break;
		case STAT_UNKNOWN:
			sendto_one(sptr, rpl_str(RPL_TRACEUNKNOWN),
				   me.name, parv[0], class, name);
			cnt++;
			break;
		case STAT_CLIENT:
			/* Only opers see users if there is a wildcard
			 * but anyone can see all the opers.
			 */
			if (IsOper(sptr)  &&
			    (MyClient(sptr) || !(dow && IsInvisible(acptr)))
			    || !dow || IsAnOper(acptr))
			    {
				if (IsAnOper(acptr))
					sendto_one(sptr,
						   rpl_str(RPL_TRACEOPERATOR),
						   me.name,
						   parv[0], class, name,
						   now - acptr->lasttime);
				else
					sendto_one(sptr,rpl_str(RPL_TRACEUSER),
						   me.name, parv[0],
						   class, name,
						   now - acptr->lasttime);
				cnt++;
			    }
			break;
		case STAT_SERVER:
			if (acptr->serv->user)
				sendto_one(sptr, rpl_str(RPL_TRACESERVER),
					   me.name, parv[0], class, link_s[i],
					   link_u[i], name, acptr->serv->by,
					   acptr->serv->user->username,
					   acptr->serv->user->host,
   					   now - acptr->lasttime);
			else
				sendto_one(sptr, rpl_str(RPL_TRACESERVER),
					   me.name, parv[0], class, link_s[i],
					   link_u[i], name, *(acptr->serv->by) ?
					   acptr->serv->by : "*", "*", me.name,
					   now - acptr->lasttime);
			cnt++;
			break;
		case STAT_SERVICE:
			sendto_one(sptr, rpl_str(RPL_TRACESERVICE),
				   me.name, parv[0], class, name);
			cnt++;
			break;
		case STAT_LOG:
			sendto_one(sptr, rpl_str(RPL_TRACELOG), me.name,
				   parv[0], LOGFILE, acptr->port);
			cnt++;
			break;
		case STAT_PING:
			sendto_one(sptr, rpl_str(RPL_TRACEPING), me.name,
				   parv[0], name, (acptr->acpt) ?
				   acptr->acpt->name : "<null>");
			break;	
		default: /* ...we actually shouldn't come here... --msa */
			sendto_one(sptr, rpl_str(RPL_TRACENEWTYPE), me.name,
				   parv[0], name);
			cnt++;
			break;
		}
	    }
	/*
	 * Add these lines to summarize the above which can get rather long
         * and messy when done remotely - Avalon
         */
       	if (!IsAnOper(sptr) || !cnt)
	    {
		if (cnt)
			return 0;
		/* let the user have some idea that its at the end of the
		 * trace
		 */
		sendto_one(sptr, rpl_str(RPL_TRACESERVER),
			   me.name, parv[0], 0, link_s[me.fd],
			   link_u[me.fd], me.name, "*", "*", me.name);
		return 0;
	    }
	for (cltmp = FirstClass(); doall && cltmp; cltmp = NextClass(cltmp))
		if (Links(cltmp) > 0)
			sendto_one(sptr, rpl_str(RPL_TRACECLASS), me.name,
				   parv[0], Class(cltmp), Links(cltmp));
	return 0;
    }

extern aConfItem *find_tline PROTO((aClient *));

/*
 * m_motd
 * parv[0] - sender prefix
 * parv[1] - servername
 *
 * modified 30 mar 1995 by flux (cmlambertus@ucdavis.edu) 
 * T line patch - display motd based on hostmask 
 */
int	m_motd(cptr, sptr, parc, parv)
aClient *cptr, *sptr;
int	parc;
char	*parv[];
{
        int     fd, nr, tflag;
	char	line[80];
	Reg1	char	 *tmp;
	struct	stat	sb;
	struct	tm	*tm;
        aConfItem       *ftmp;

	if (check_registered(sptr))
		return 0;

	if (hunt_server(cptr, sptr, ":%s MOTD :%s", 1,parc,parv)!=HUNTED_ISME)
		return 0;

	/*
	 * find out if we have a T line for our hostname 
	 */
        ftmp = find_tline(cptr);

	/*
	 * stop NFS hangs...most systems should be able to open a file in
	 * 3 seconds. -avalon (curtesy of wumpus)
	 */
	(void)alarm(3);
	/* if we returned 0 in find_tline, there is no T line for this host
	 * so display the stock ircd.motd   
	 */
	if (!ftmp || (fd = open(ftmp->passwd, O_RDONLY)) == -1)
		fd = open(MOTD, O_RDONLY);
	(void)alarm(0);
	if (fd == -1)
	    {
		sendto_one(sptr, err_str(ERR_NOMOTD), me.name, parv[0]);
		return 0;
	    }
	(void)fstat(fd, &sb);
	sendto_one(sptr, rpl_str(RPL_MOTDSTART), me.name, parv[0], me.name);
	tm = localtime((time_t *)&sb.st_mtime); /* NetBSD needs cast */
	sendto_one(sptr, ":%s %d %s :- %d/%d/%d %d:%02d", me.name, RPL_MOTD,
		   parv[0], tm->tm_mday, tm->tm_mon + 1, 1900 + tm->tm_year,
		   tm->tm_hour, tm->tm_min);
	(void)dgets(-1, NULL, 0); /* make sure buffer is at empty pos */
	while ((nr=dgets(fd, line, sizeof(line)-1)) > 0)
	    {
	    	line[nr]='\0';
		if ((tmp = (char *)index(line,'\n')))
			*tmp = '\0';
		if ((tmp = (char *)index(line,'\r')))
			*tmp = '\0';
		sendto_one(sptr, rpl_str(RPL_MOTD), me.name, parv[0], line);
	    }
	(void)dgets(-1, NULL, 0); /* make sure buffer is at empty pos */
	sendto_one(sptr, rpl_str(RPL_ENDOFMOTD), me.name, parv[0]);
	(void)close(fd);
	return 0;
    }

/*
** m_close - added by Darren Reed Jul 13 1992.
*/
int	m_close(cptr, sptr, parc, parv)
aClient	*cptr, *sptr;
int	parc;
char	*parv[];
{
	Reg1	aClient	*acptr;
	Reg2	int	i;
	int	closed = 0;

	if (!MyOper(sptr))
	    {
		sendto_one(sptr, err_str(ERR_NOPRIVILEGES), me.name, parv[0]);
		return 0;
	    }

	for (i = highest_fd; i; i--)
	    {
		if (!(acptr = local[i]))
			continue;
		if (!IsUnknown(acptr) && !IsConnecting(acptr) &&
		    !IsHandshake(acptr))
			continue;
		sendto_one(sptr, rpl_str(RPL_CLOSING), me.name, parv[0],
			   get_client_name(acptr, TRUE), acptr->status);
		(void)exit_client(cptr, acptr, &me, "Oper Closing");
		closed++;
	    }
	sendto_one(sptr, rpl_str(RPL_CLOSEEND), me.name, parv[0], closed);
	return 0;
}

#if defined(OPER_DIE) || defined(LOCOP_DIE)
int	m_die(cptr, sptr, parc, parv)
aClient	*cptr, *sptr;
int	parc;
char	*parv[];
{
	Reg1	aClient	*acptr;
	Reg2	int	i;

#ifndef	LOCOP_DIE
	if (!MyClient(sptr) || !IsOper(sptr))
#else
# ifdef	OPER_DIE
	if (!MyClient(sptr) || !IsAnOper(sptr))
# else
	if (!MyClient(sptr) || !IsLocOp(sptr))
# endif
#endif
	    {
		sendto_one(sptr, err_str(ERR_NOPRIVILEGES), me.name, parv[0]);
		return 0;
	    }

	for (i = 0; i <= highest_fd; i++)
	    {
		if (!(acptr = local[i]))
			continue;
		if (IsClient(acptr))
			sendto_one(acptr,
				   ":%s NOTICE %s :Server Terminating. %s",
				   me.name, acptr->name,
				   get_client_name(sptr, TRUE));
		else if (IsServer(acptr))
			sendto_one(acptr, ":%s ERROR :Terminated by %s",
				   me.name, get_client_name(sptr, TRUE));
	    }
	(void)s_die();
	return 0;
}
#endif

int	m_gline(cptr, sptr, parc, parv)
aClient	*cptr, *sptr;
int	parc;
char	*parv[];
{
  aGline *agline, *a2gline = NULL;
  aConfItem *tmp;
  aClient *acptr;
  char *user, *host;
  int active = -1;
#ifdef GPATH
  int logfile;
#endif /* GPATH */
  time_t expire = 0;

  if (IsServer(cptr)) {
    if (find_conf_host(cptr->confs, sptr->name, CONF_UWORLD)) {
      if (parc < 3 || (*parv[2] != '-' && (parc < 5 || *parv[4] == '\0'))) {
	sendto_one(sptr, err_str(ERR_NEEDMOREPARAMS), me.name, parv[0],
		   "GLINE");
	return 0;
      }

      if (*parv[2] == '-') /* add mode or delete mode? */
	active = GLINE_INACTIVE;
      else active = GLINE_ACTIVE;

      if (*parv[2] == '+' || *parv[2] == '-')
	parv[2]++; /* step past mode indicator */

      /* forward the message appropriately */
      if (!(acptr = find_person(parv[1], NULL)))
	sendto_serv_butone(cptr, active == GLINE_INACTIVE ? ":%s GLINE %s -%s"
			   : ":%s GLINE %s +%s %s :%s", parv[0], parv[1],
			   parv[2], parv[3], parv[4]); /* global! */
      else if (IsServer(acptr) || !MyConnect(acptr))
	sendto_one(acptr, active == GLINE_INACTIVE ? ":%s GLINE %s -%s" :
		   ":%s GLINE %s +%s %s :%s", parv[0], parv[1], parv[2],
		   parv[3], parv[4]); /* single destination */

      if (!(host = strchr(parv[2], '@'))) { /* convert user@host */
	user = "*"; /* no @'s; assume username is '*' */
	host = parv[2];
      } else {
	user = parv[2];
	*(host++) = '\0'; /* break up string at the '@' */
      }

      for (agline = gline, a2gline = NULL; agline; agline = agline->next) {
	if (agline->expire <= TStime()) { /* handle expired glines */
	  free_gline(agline, a2gline);
	  agline = a2gline ? a2gline : gline;
	  if (!agline) break; /* end of list due to expire */
	  continue;
	}

	if (!mycmp(agline->name, user) && !mycmp(agline->host, host))
	  break;

	a2gline = agline;
      }

      if (active == GLINE_INACTIVE && agline) { /* removing the gline */
	sendto_ops("%s removing GLINE for %s@%s", parv[0], agline->name,
		   agline->host); /* notify opers */

#ifdef GPATH
	(void)alarm(3); /* make a log entry */
	logfile = open(GPATH, O_WRONLY|O_APPEND|O_CREAT, S_IRUSR|S_IWUSR);
	(void)alarm(0);

	(void)sprintf(buf, "# %lu %s removing GLINE for %s@%s\n", TStime(),
		      parv[0], agline->name, agline->host);

	(void)alarm(3);
	(void)write(logfile, buf, strlen(buf));
	(void)alarm(0);

	(void)close(logfile);
#endif /* GPATH */

	free_gline(agline, a2gline); /* remove the gline */

      } else if (active != GLINE_INACTIVE) { /* must be adding a gline */
	expire = atoi(parv[3]) + TStime(); /* expire time? */
	if (agline && agline->expire < expire) { /* new expire time? */
	  /* yes, notify the opers */
	  sendto_ops("%s resetting expiration time on GLINE for %s@%s to "
		     "%lu", parv[0], agline->name, agline->host, expire);

#ifdef GPATH
	  (void)alarm(3); /* make a log entry */
	  logfile = open(GPATH, O_WRONLY|O_APPEND|O_CREAT, S_IRUSR|S_IWUSR);
	  (void)alarm(0);

	  (void)sprintf(buf, "# %lu %s resetting expiration time on GLINE "
			"for %s@%s to %lu\n", TStime(), parv[0], agline->name,
			agline->host, expire);

	  (void)alarm(3);
	  (void)write(logfile, buf, strlen(buf));
	  (void)alarm(0);

	  (void)close(logfile);
#endif /* GPATH */

	  agline->expire = expire; /* reset the expire time */
	} else if (!agline) { /* create gline */
	  /* find any pre-existing K-lines */
	  for (tmp = conf; tmp; tmp = tmp->next)
	    if ((tmp->status == CONF_KILL) && tmp->host && tmp->name) {
	      if ((!match(tmp->name,user)) && (!match(tmp->host,host)))
	        return 0; /* found an existing K-line that matches */
             }

	  for (agline = gline, a2gline = NULL; agline; agline = agline->next) {
	    if (agline->expire <= TStime()) { /* handle expired glines */
	      free_gline(agline, a2gline);
	      agline = agline ? a2gline : gline;
	      if (!agline) break; /* end of list due to expire */
	      continue;
	    }
              if ((!match(agline->name,user)) && (!match(agline->host,host)))
                return 0; /* found an existing G-line that matches */

	    a2gline = agline; /* keep track of back pointer */
	  }

	  sendto_ops("%s adding GLINE for %s@%s, expiring at %lu: %s", parv[0],
		     user, host, expire, parv[4]); /* inform ops */
#ifdef GPATH
	  (void)alarm(3); /* make a log entry */
	  logfile = open(GPATH, O_WRONLY|O_APPEND|O_CREAT, S_IRUSR|S_IWUSR);
	  (void)alarm(0);

	  (void)sprintf(buf, "# %lu %s adding GLINE for %s@%s, expiring at "
			"%lu: %s\n", TStime(), parv[0], user, host, expire,
			parv[4]);

	  (void)alarm(3);
	  (void)write(logfile, buf, strlen(buf));
	  (void)alarm(0);

	  /* this can be inserted into the conf */
	  (void)sprintf(buf, "K:%s:%s:%s\n", host, parv[4], user);

	  (void)alarm(3);
	  (void)write(logfile, buf, strlen(buf));
	  (void)alarm(0);

	  (void)close(logfile);
#endif /* GPATH */

	  agline = make_gline(host, parv[4], user, expire); /* add the line */

	  for (active = 0; active <= highest_fd; active++) /* get the users! */
	    if ((acptr = local[active]) && !IsMe(acptr)) {

	      if (!acptr->user || strlen(acptr->sockhost) > (size_t) HOSTLEN ||
		  (acptr->user->username ? strlen(acptr->user->username) : 0) >
		  (size_t) HOSTLEN) continue; /* these tests right out of
						 find_kill for safety's sake */

	      if (match(agline->host, acptr->sockhost) == 0 &&
		  (!acptr->user->username || 
                    match(agline->name, acptr->user->username) == 0)) {

		/* ok, he was the one that got G-lined */
		sendto_one(acptr, ":%s %d %s :*** %s.", me.name,
			   ERR_YOUREBANNEDCREEP, acptr->name, agline->reason);

		/* let the ops know about my first kill */
		sendto_ops("G-line active for %s",
			   get_client_name(acptr, FALSE));

		/* and get rid of him */
		exit_client(cptr, acptr, &me, "G-lined");
	      }
	    }
	}
      }
    }
    else return 0; /* server not permitted to add a GLINE anyway */
  }
  else if (parc < 2) { /* not enough args and a user...list glines */

    for (agline = gline, a2gline = NULL; agline; agline = agline->next) {

      if (agline->expire <= TStime()) { /* handle expired glines */
	free_gline(agline, a2gline);
	agline = a2gline ? a2gline : gline;
	if (!agline) break; /* agline->next is illegal if agline == NULL! */
	continue; /* don't need to display this gline; next iteration will */
      }

      /* display the GLINE */
      sendto_one(cptr, rpl_str(RPL_GLIST), me.name, parv[0],
		 agline->name, agline->host, agline->expire, agline->reason,
		 (agline->active == GLINE_INACTIVE) ? " (Inactive)" : "");

      a2gline = agline; /* keep the back pointer */
    }
    sendto_one(cptr, rpl_str(RPL_ENDOFGLIST), me.name, parv[0]);

  } else {

    if (IsOper(cptr)) { /* non-oper not permitted to change things */
      if (*parv[1] == '-') { /* oper wants to deactivate the gline */
	active = GLINE_INACTIVE;
	parv[1]++;
      } else if (*parv[1] == '+') { /* oper wants to activate inactive gline */
	active = GLINE_ACTIVE;
	parv[1]++;
      }

      if (parc > 2)
	expire = atoi(parv[2]) + TStime(); /* oper wants to reset expire TS */
    }

    if (!(host = strchr(parv[1], '@'))) {
      user = "*"; /* no @'s; assume username is '*' */
      host = parv[1];
    } else {
      user = parv[1];
      *(host++) = '\0'; /* break up string at the '@' */
    }

    for (agline = gline, a2gline = NULL; agline; agline = agline->next) {

      if (agline->expire <= TStime()) { /* handle expired glines */
	free_gline(agline, a2gline);
	agline = a2gline ? a2gline : gline;
	if (!agline) break; /* break out of loop */
	continue;
      }

      if ((!match(agline->name, user) || !mycmp(agline->name, user)) &&
	  (!match(agline->host, host) || !mycmp(agline->host, host)))
	break;

      a2gline = agline;
    }

    if (!agline) {
      sendto_one(cptr, err_str(ERR_NOSUCHGLINE), me.name, parv[0], user, host);
      return 0;
    }

    if (expire <= agline->expire)
      expire = 0;

    if ((active == -1 || active == agline->active) && expire == 0) {
      /* oper wants a list of one gline only */
      sendto_one(cptr, rpl_str(RPL_GLIST), me.name, parv[0], agline->name,
		 agline->host, agline->expire, agline->reason,
		 (agline->active == GLINE_INACTIVE) ? " (Inactive)" : "");
      sendto_one(cptr, rpl_str(RPL_ENDOFGLIST), me.name, parv[0]);
      return 0;
    }

    if (active != -1 && active != agline->active)
      agline->active = active; /* reset activation on gline */
    else
      active = -1; /* for later sendto_ops and logging functions */

    if (expire)
      agline->expire = expire; /* reset expiration time */

    /* inform the operators what's up */
    if (active != -1) { /* changing the activation */
      sendto_ops(!expire ? "%s %sactivating GLINE for %s@%s":"%s %sactivating "
		 "GLINE for %s@%s and resetting expiration time to %lu",
		 parv[0], active == GLINE_INACTIVE ? "de" : "re", agline->name,
		 agline->host, agline->expire);
#ifdef GPATH
      (void)alarm(3); /* make a log entry */
      logfile = open(GPATH, O_WRONLY|O_APPEND|O_CREAT, S_IRUSR|S_IWUSR);
      (void)alarm(0);

      (void)sprintf(buf, !expire ? "# %lu %s!%s@%s %sactivating GLINE for "
		    "%s@%s\n" : "# %lu %s!%s@%s %sactivating GLINE for %s@%s "
		    "and resetting expiration time to %lu\n", TStime(),
		    parv[0], cptr->user->username, cptr->user->host,
		    active == GLINE_INACTIVE ? "de" : "re", agline->name,
		    agline->host, agline->expire);

      (void)alarm(3);
      (void)write(logfile, buf, strlen(buf));
      (void)alarm(0);

      (void)close(logfile);
#endif /* GPATH */

    } else { /* changing only the expiration */
      sendto_ops("%s resetting expiration time on GLINE for %s@%s to %lu",
		 parv[0], agline->name, agline->host, agline->expire);
#ifdef GPATH
      (void)alarm(3); /* make a log entry */
      logfile = open(GPATH, O_WRONLY|O_APPEND|O_CREAT, S_IRUSR|S_IWUSR);
      (void)alarm(0);

      (void)sprintf(buf, "# %lu %s!%s@%s resetting expiration time on GLINE "
		    "for %s@%s to %lu\n", TStime(), parv[0],
		    cptr->user->username, cptr->user->host, agline->name,
		    agline->host, agline->expire);

      (void)alarm(3);
      (void)write(logfile, buf, strlen(buf));
      (void)alarm(0);

      (void)close(logfile);
#endif /* GPATH */
    }
  }

  return 0;
}
