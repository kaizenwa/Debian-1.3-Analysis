/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

/*
 * From: @(#)process.c	5.10 (Berkeley) 2/26/91
 */
char rcsid[] = 
  "$Id: process.c,v 1.5 1996/12/29 17:13:53 dholland Exp $";

/*
 * process.c handles the requests, which can be of three types:
 *	ANNOUNCE - announce to a user that a talk is wanted
 *	LEAVE_INVITE - insert the request into the table
 *	LOOK_UP - look up to see if a request is waiting in
 *		  in the table for the local user
 *	DELETE - delete invitation
 */
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <netdb.h>
#include <syslog.h>
#include <stdio.h>
#include <string.h>
#include <paths.h>
#include <utmp.h>
#include "mytalkd.h"
#include "proto.h"

static void do_announce(CTL_MSG *mp, CTL_RESPONSE *rp);
static int find_user(char *name, char *tty);
extern int debug;

void
process_request(CTL_MSG *mp, CTL_RESPONSE *rp)
{
	CTL_MSG *ptr;

	rp->vers = TALK_VERSION;
	rp->type = mp->type;
	rp->id_num = htonl(0);
	if (mp->vers != TALK_VERSION) {
		syslog(LOG_WARNING, "Bad protocol version %d", mp->vers);
		rp->answer = BADVERSION;
		return;
	}
	mp->id_num = ntohl(mp->id_num);
	mp->addr.sa_family = ntohs(mp->addr.sa_family);
	if (mp->addr.sa_family != AF_INET) {
		syslog(LOG_WARNING, "Bad address, family %d",
		    mp->addr.sa_family);
		rp->answer = BADADDR;
		return;
	}
	mp->ctl_addr.sa_family = ntohs(mp->ctl_addr.sa_family);
	if (mp->ctl_addr.sa_family != AF_INET) {
		syslog(LOG_WARNING, "Bad control address, family %d",
		    mp->ctl_addr.sa_family);
		rp->answer = BADCTLADDR;
		return;
	}
	mp->pid = ntohl(mp->pid);
	if (debug) print_request("process_request", mp);

	switch (mp->type) {
	  case ANNOUNCE:
		do_announce(mp, rp);
		break;
	  case LEAVE_INVITE:
		ptr = find_request(mp);
		if (ptr != (CTL_MSG *)0) {
			rp->id_num = htonl(ptr->id_num);
			rp->answer = SUCCESS;
		} 
		else insert_table(mp, rp);
		break;
	  case LOOK_UP:
		ptr = find_match(mp);
		if (ptr != (CTL_MSG *)0) {
			rp->id_num = htonl(ptr->id_num);
			rp->addr = ptr->addr;
			rp->addr.sa_family = htons(ptr->addr.sa_family);
			rp->answer = SUCCESS;
		} 
		else rp->answer = NOT_HERE;
		break;
	  case DELETE:
		rp->answer = delete_invite(mp->id_num);
		break;
	  default:
		rp->answer = UNKNOWN_REQUEST;
		break;
	}
	if (debug) print_response("process_request", rp);
}

static void
do_announce(CTL_MSG *mp, CTL_RESPONSE *rp)
{
	struct hostent *hp;
	CTL_MSG *ptr;
	int result;
	struct in_addr the_addr;

	/* see if the user is logged */
	result = find_user(mp->r_name, mp->r_tty);
	if (result != SUCCESS) {
		rp->answer = result;
		return;
	}
	the_addr = ((struct sockaddr_in *)(&mp->ctl_addr))->sin_addr;
	hp = gethostbyaddr((char *)&the_addr, sizeof(the_addr), AF_INET);
	if (hp == NULL) {
		rp->answer = MACHINE_UNKNOWN;
		return;
	}
	ptr = find_request(mp);
	if (ptr == NULL) {
		insert_table(mp, rp);
		rp->answer = announce(mp, hp->h_name);
		return;
	}
	if (mp->id_num > ptr->id_num) {
		/*
		 * This is an explicit re-announce, so update the id_num
		 * field to avoid duplicates and re-announce the talk.
		 */
		ptr->id_num = new_id();
		rp->id_num = htonl(ptr->id_num);
		rp->answer = announce(mp, hp->h_name);
	} 
	else {
		/* a duplicated request, so ignore it */
		rp->id_num = htonl(ptr->id_num);
		rp->answer = SUCCESS;
	}
}

/*
 * Search utmp for the local user
 */
static int
find_user(char *name, char *tty)
{
	struct utmp ubuf;
	int status;
	FILE *fd;
	struct stat statb;
	char ftty[20];
	time_t last_time = 0;
	int notty;

	notty = (*tty == '\0');

	if ((fd = fopen(_PATH_UTMP, "r")) == NULL) {
		fprintf(stderr, "talkd: can't read %s.\n", _PATH_UTMP);
		return (FAILED);
	}
	status = NOT_HERE;
	strcpy(ftty, _PATH_DEV);
	status = PERMISSION_DENIED;
	while (fread((char *) &ubuf, sizeof ubuf, 1, fd) == 1) {
#ifdef USER_PROCESS
		if (ubuf.ut_type!=USER_PROCESS) continue;
#endif
		if (!strncmp(ubuf.ut_name, name, sizeof(ubuf.ut_name))) {
			if (notty) {
				/* no particular tty was requested */
				strncpy(ftty+5, ubuf.ut_line, sizeof(ftty)-5);
				ftty[sizeof(ftty)-1] = 0;

				if (stat(ftty,&statb) == 0) {
					if (!(statb.st_mode & 020))
						continue;
					if (statb.st_atime > last_time) {
						last_time = statb.st_atime;
						strcpy(tty, ubuf.ut_line);
						status = SUCCESS;
					}
					continue;
				}
			}
			if (!strcmp(ubuf.ut_line, tty)) {
				status = SUCCESS;
				break;
			}
		}
	}
	fclose(fd);
	return status;
}
