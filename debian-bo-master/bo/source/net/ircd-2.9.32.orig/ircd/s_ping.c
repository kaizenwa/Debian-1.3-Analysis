/************************************************************************
 *   IRC - Internet Relay Chat, ircd/s_ping.c
 *   Copyright (C) 1994 Carlo K ( Run @ undernet.org )
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
static  char sccsid[] = "@(#)s_ping.c	1.0 9/21/94 (C) 1994 Carlo Kid";
#endif

#include "struct.h"
#include "common.h"
#include "sys.h"
#include "numeric.h"
#include "patchlevel.h"
#include <sys/socket.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#ifdef	UNIXPORT
# include <sys/un.h>
#endif
#if defined(__hpux)
# include "inet.h"
#endif
#include <fcntl.h>
#include "sock.h"	/* If FD_ZERO isn't defined up to this point,  */
			/* define it (BSD4.2 needs this) */
#include "h.h"

#define UPINGBUFSIZE 2000 /* Lot bigger then 1024, bit smaller then 2048 */
#define UPINGTIMEOUT 120 /* Timeout waitting for first ping response */

extern u_long inet_addr();

/*
 * start_ping
 *
 * As for now, I am abusing the client structure for a ping connection.
 * Used members are:
 * These are used by existing routines as well, and do have their own meaning:
 *   fd       : The socket file descriptor.
 *   status   : To flag that this IS one of these abused ping structures
 *   sockhost : Name of requested server to ping (aconf->host).
 *   name     : aconf->name
 *   ip       : ip#
 * These have more or less their own meaning,
 * but are not used by existing routines:
 *   flags    : To flag that a next ping is requested.
 *   port     : Requested remote port.
 * These are only used by the 'uping' routines
 * and have totally different meanings:
 *   buffer   : buffer hold pingtimes of received packets
 *   confs    : recv/send (char *) buffer.
 *   hopcount : Total number of requested pings
 *   count    : Number of pings left to send.
 *   hashv    : Number of pings left to be received.
 *   acpt     : client asking for this ping
 *   lasttime : last time a ping was sent
 *   firsttime: recvfrom timeout
 *   since    : timeout in seconds to next recvfrom
 *   receiveK : minimum in ms
 *   sendM    : average in ms
 *   receiveM : maximum in ms
 */
int	start_ping(cptr)
Reg1	aClient	*cptr;
{
  struct sockaddr_in remote_addr;

  Debug((DEBUG_NOTICE,"start_ping(%x) status %d", cptr, cptr->status));

  if (!(cptr->acpt)) return -1;

  bcopy((char *)&cptr->ip, (char *)&remote_addr.sin_addr,
      sizeof(struct in_addr));
#ifdef TESTNET
  remote_addr.sin_port = htons(cptr->port + 10000);
#else
  remote_addr.sin_port = htons(cptr->port);
#endif
  remote_addr.sin_family = AF_INET;

  sendto_one(cptr->acpt,
      ":%s NOTICE %s :Sending %d ping%s to %s[%s] port %d",
      me.name, cptr->acpt->name, cptr->hopcount,
      (cptr->hopcount == 1) ? "" : "s", cptr->name,
#ifdef TESTNET
      inetntoa((char *)&remote_addr.sin_addr), ntohs(remote_addr.sin_port)
	  - 10000);
#else
      inetntoa((char *)&remote_addr.sin_addr), ntohs(remote_addr.sin_port));
#endif

  cptr->firsttime = now + UPINGTIMEOUT;
  cptr->since = UPINGTIMEOUT;
  cptr->flags |= (FLAGS_PING);

  return 0;
}

/*
 * send_ping
 *
 */
void	send_ping(cptr)
aClient	*cptr;
{
  struct sockaddr_in remote_addr;
  struct timeval tv;

  bcopy((char *)&cptr->ip, (char *)&remote_addr.sin_addr,
      sizeof(struct in_addr));
#ifdef TESTNET
  remote_addr.sin_port = htons(cptr->port + 10000);
#else
  remote_addr.sin_port = htons(cptr->port);
#endif
  remote_addr.sin_family = AF_INET;

  (void) gettimeofday(&tv, NULL);
  (void)sprintf((char *)cptr->confs, " %10lu%c%6lu",
      tv.tv_sec, '\0', tv.tv_usec);

  Debug((DEBUG_SEND, "send_ping: sending [%s %s] to %s.%d on %d",
      (char *)cptr->confs, (char *)cptr->confs + 12,
      inetntoa((char *)&remote_addr.sin_addr), ntohs(remote_addr.sin_port),
      cptr->fd));

  if (sendto(cptr->fd, (char *)cptr->confs, 1024, 0,
      (struct sockaddr *)&remote_addr, sizeof(struct sockaddr_in)) != 1024)
  {
    int err=errno;
    if (cptr->acpt)
      sendto_one(cptr->acpt, ":%s NOTICE %s :UPING: sendto() failed: %s",
          me.name, cptr->acpt->name, strerror(get_sockerr(cptr)));
    Debug((DEBUG_SEND, "send_ping: sendto failed on %d (%d)", cptr->fd, err));
    end_ping(cptr);
  }
  else if (--(cptr->count) <= 0 )
  {
    ClearPing(cptr);
    if (cptr->hashv <= 0) end_ping(cptr);
  }

  return;
}

/*
 * read_ping
 *
 */
void	read_ping(cptr)
Reg1	aClient	*cptr;
{
  int addr_len = sizeof(struct sockaddr_in);
  struct sockaddr_in remote_addr;
  struct timeval tv;
  int len;
  unsigned long int pingtime;
  char *s;

  bcopy((char *)&cptr->ip, (char *)&remote_addr.sin_addr,
      sizeof(struct in_addr));
#ifdef TESTNET
  remote_addr.sin_port = htons(cptr->port + 10000);
#else
  remote_addr.sin_port = htons(cptr->port);
#endif
  remote_addr.sin_family = AF_INET;

  (void)gettimeofday(&tv, NULL);

  if ((len=recvfrom(cptr->fd, (char *)cptr->confs, UPINGBUFSIZE, 0,
       (struct sockaddr *)&remote_addr, &addr_len)) == -1)
  {
    int err = errno;
    sendto_one(cptr->acpt,
        ":%s NOTICE %s :UPING: recvfrom: %s",
        me.name, cptr->acpt->name, strerror(get_sockerr(cptr)));
    Debug((DEBUG_SEND, "read_ping: recvfrom: %d", err));
    if (err != EAGAIN) end_ping(cptr);
    return;
  }

  if (len<19) return; /* Broken packet */

  pingtime = (tv.tv_sec - atoi((char *)cptr->confs + 1)) * 1000 +
      (tv.tv_usec - atoi((char *)cptr->confs + strlen((char *)cptr->confs) +
      1)) / 1000;
  cptr->sendM += pingtime;
  if (!(cptr->receiveK) || (cptr->receiveK > pingtime))
    cptr->receiveK = pingtime;
  if (pingtime > cptr->receiveM)
    cptr->receiveM = pingtime;
  /* Wait at most 10 times the average pingtime for the next one: */
  if ((cptr->since =
      cptr->sendM / ( 100 * (cptr->hopcount - cptr->hashv + 1))) < 2)
    cptr->since = 2;
  cptr->firsttime = tv.tv_sec + cptr->since;
  
  Debug(("read_ping: %d bytes, ti %lu: [%s %s] %u ms",
        len, cptr->since, (char *)cptr->confs,
	(char *)cptr->confs + strlen((char *)cptr->confs) + 1, pingtime));

  s = cptr->buffer + strlen(cptr->buffer);
  sprintf(s, " %lu", pingtime);

  if ((--(cptr->hashv) <= 0 && !DoPing(cptr)) || !(cptr->acpt))
      end_ping(cptr);

  return;
}

int ping_server(cptr, hp)
aClient *cptr;
struct hostent *hp;
{
  if ( ( !cptr->ip.s_addr )
#ifdef UNIXPORT
      && ( ( cptr->sockhost[2] ) != '/' )
#endif
      )
  {
    struct hostent *hp;
    char *s;
    Link    lin;

    if (!(cptr->acpt)) return -1; /* Oper left already */

    lin.flags = ASYNC_PING;
    lin.value.cptr = cptr;
    nextdnscheck = 1;
    s = (char *)index(cptr->sockhost, '@');
    s++; /* should never be NULL; cptr->sockhost is actually a conf->host */
    if ((cptr->ip.s_addr = inet_addr(s)) == -1)
    {
      cptr->ip.s_addr = 0;
      hp = gethost_byname(s, &lin);
      Debug((DEBUG_NOTICE, "ping_sv: hp %x ac %x ho %s", hp, cptr, s));
      if (!hp) return 0;
      bcopy(hp->h_addr, (char *)&cptr->ip, sizeof(struct in_addr));
    }
  }

  return start_ping(cptr);
}

/*
** m_uping  -- by Run
**
**	parv[0] = sender prefix
**	parv[1] = pinged server
**      parv[2] = port
**	parv[3] = hunted server
**      parv[4] = number of requested pings
*/
int m_uping(cptr, sptr, parc, parv)
aClient *cptr, *sptr;
int     parc;
char    *parv[];
{
  aConfItem *aconf;
  int port, fd, opt;

  if (!IsPrivileged(sptr))
  {
    sendto_one(sptr, err_str(ERR_NOPRIVILEGES), me.name, parv[0]);
    return -1;
  }

  if (parc < 2)
  {
    sendto_one(sptr, err_str(ERR_NEEDMOREPARAMS),
        me.name, parv[0], "UPING");
    return 0;
  }

  if (MyClient(sptr))
  {
    if (parc == 2)
    {
      parv[parc++]=UDP_PORT;
      parv[parc++]=me.name;
      parv[parc++]="5";
    }
    else if (parc == 3)
    {
      if (isdigit(*parv[2]))
      {
        parv[parc++]=me.name;
      }
      else
      {
	parv[parc++]=parv[2];
	parv[2]=UDP_PORT;
      }
      parv[parc++]="5";
    }
    else if (parc == 4)
    {
      if (isdigit(*parv[2]))
      {
	if (isdigit(*parv[3]))
	{
	  parv[parc++]=parv[3];
	  parv[3]=me.name;
	}
	else
	  parv[parc++]="5";
      }
      else
      {
        parv[parc++]=parv[3];
	parv[3]=parv[2];
	parv[2]=UDP_PORT;
      }
    }
  }
  if (hunt_server(cptr,sptr,":%s UPING %s %s %s %s",3,parc,parv) != HUNTED_ISME)
    return 0;

  if (BadPtr(parv[4]) || atoi(parv[4])<=0)
  {
    sendto_one(sptr,":%s NOTICE %s :UPING: Illegal number of packets: %s",
      me.name, parv[0], parv[4]);
    return 0;
  }

  /* Check if a CONNECT would be possible at all (adapted from m_connect) */
  for (aconf = conf; aconf; aconf = aconf->next)
    if (aconf->status == CONF_CONNECT_SERVER &&
        matches(parv[1], aconf->name) == 0)
      break;
  if (!aconf)
    for (aconf = conf; aconf; aconf = aconf->next)
      if (aconf->status == CONF_CONNECT_SERVER &&
          (matches(parv[1], aconf->host) == 0 ||
          matches(parv[1], index(aconf->host, '@')+1) == 0))
        break;
  if (!aconf)
  {
    sendto_one(sptr, "NOTICE %s :UPING: Host %s not listed in ircd.conf",
        parv[0], parv[1]);
    return 0;
  }

  if (AskedPing(sptr))
    cancel_ping(sptr, sptr); /* Cancel previous ping request */

  /*
   * Determine port: First user supplied, then default : 7007
   */
  if (!BadPtr(parv[2]) && (port = atoi(parv[2])) <= 0) port=atoi(UDP_PORT);

  (void)alarm(2);
  if ((fd = socket(AF_INET, SOCK_DGRAM, 0)) == -1)
  {
    int err = errno;
    (void)alarm(0);
    sendto_ops("m_uping: socket: %s", (err != EAGAIN) ?
        strerror(err) : "No more sockets");
    sendto_one(sptr, ":%s NOTICE %s :UPING: Unable to create udp ping socket",
	me.name, parv[0]);
#ifdef	USE_SYSLOG
    syslog(LOG_ERR, "Unable to create udp ping socket");
#endif
    return 0;
  }
  (void)alarm(0);

  if (fcntl(fd, F_SETFL, FNDELAY)==-1)
  {
    sendto_ops("m_uping: fcntl FNDELAY: %s", strerror(errno));
    sendto_one(sptr, ":%s NOTICE %s :UPING: Can't set fd non-blocking",
	me.name, parv[0]);
    close(fd);
    return 0;
  }
  /*
  ** On some systems, receive and send buffers must be equal in size.
  ** Others block select() when the buffers are too small
  ** (Linux 1.1.50 blocks when < 2048) --Run
  */
  opt = 2048;
  if (setsockopt(fd, SOL_SOCKET, SO_SNDBUF, (OPT_TYPE *)&opt, sizeof(opt)) < 0 ||
      setsockopt(fd, SOL_SOCKET, SO_RCVBUF, (OPT_TYPE *)&opt, sizeof(opt)) < 0)
  {
    int err=errno;
    sendto_ops("m_uping: setsockopt SO_SNDBUF|SO_RCVBUF: %s",
	strerror(err));
    sendto_one(sptr, ":%s NOTICE %s :UPING: error in setsockopt: %s",
        me.name, parv[0], strerror(err));
    close(fd);
    return 0;
  }

  if (fd >= MAXCONNECTIONS)
  {
    sendto_ops("Can't allocate fd for uping (all connections in use)");
    sendto_one(sptr, ":%s NOTICE %s :UPING: All connections in use",
        me.name, parv[0]);
    close(fd);
    return 0;
  }

  if (fd > highest_fd)
    highest_fd = fd;
  local[fd] = cptr = make_client(NULL);
  cptr->confs = (Link *)MyMalloc(UPINGBUFSIZE); /* Really a (char *) */
  cptr->fd = fd;
  cptr->port = port;
  cptr->hopcount = cptr->hashv = cptr->count = MIN(20, atoi(parv[4]));
  strcpy(cptr->sockhost, aconf->host);
  cptr->acpt = sptr;
  SetAskedPing(sptr);
  bcopy((void *)&aconf->ipnum, (void *)&cptr->ip, sizeof(struct in_addr));
  strcpy(cptr->name, aconf->name);
  cptr->firsttime = 0;
  SetPing(cptr);

  switch (ping_server(cptr, NULL))
  {
    case 0:
      break;
    case -1:
      del_queries((char *)cptr);
      end_ping(cptr);
      break;
  }
  return 0;
}

void end_ping(cptr)
register aClient *cptr;
{
  Debug((DEBUG_DEBUG,"end_ping: %x", cptr));
  if (cptr->acpt)
  {
    if (cptr->firsttime) /* Started at all ? */
    {
      if (cptr->hashv != cptr->hopcount) /* Received any pings at all ? */
      {
	sendto_one(cptr->acpt, ":%s NOTICE %s :UPING %s%s",
	    me.name, cptr->acpt->name, cptr->name, cptr->buffer);
	sendto_one(cptr->acpt,
":%s NOTICE %s :UPING Stats: sent %d recvd %d ; min/avg/max = %lu/%lu/%lu ms",
	    me.name, cptr->acpt->name, cptr->hopcount - cptr->count,
	    cptr->hopcount - cptr->hashv, cptr->receiveK,
	    (2 * cptr->sendM + cptr->hopcount - cptr->hashv) /
	    (2 * ( cptr->hopcount - cptr->hashv)),
	    cptr->receiveM);
      }
      else
        sendto_one(cptr->acpt,
            ":%s NOTICE %s :UPING: no response from %s within %d seconds",
            me.name, cptr->acpt->name, cptr->name,
            now + cptr->since - cptr->firsttime);
    }
    else
      sendto_one(cptr->acpt,
          ":%s NOTICE %s :UPING: Could not start ping to %s %d",
          me.name, cptr->acpt->name, cptr->name, cptr->port);
  }
  (void)close(cptr->fd);
  local[cptr->fd] = NULL;
  if (cptr->acpt)
    ClearAskedPing(cptr->acpt);
  MyFree((char *)cptr->confs);
  free_client(cptr);
}

void cancel_ping(sptr, acptr)
aClient *sptr, *acptr;
{
  int i;
  aClient *cptr;

  Debug((DEBUG_DEBUG, "Cancelling uping for %x (%s)", sptr, sptr->name));
  for (i = highest_fd; i >= 0; i--)
    if ((cptr = local[i]) && IsPing(cptr) && cptr->acpt == sptr)
    { cptr->acpt = acptr;
      del_queries((char *)cptr);
      end_ping(cptr);
      break; }

  ClearAskedPing(sptr);
}
