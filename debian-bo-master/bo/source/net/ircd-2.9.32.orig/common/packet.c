/************************************************************************
 *   IRC - Internet Relay Chat, common/packet.c
 *   Copyright (C) 1990  Jarkko Oikarinen and
 *                       University of Oulu, Computing Center
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
static  char sccsid[] = "@(#)packet.c	2.12 1/30/94 (C) 1988 University of Oulu, \
Computing Center and Jarkko Oikarinen";
#endif
 
#include "struct.h"
#include "common.h"
#include "sys.h"
#include "msg.h"
#include "h.h"

extern char *last_dead_comment;

/*
** dopacket
**	cptr - pointer to client structure for which the buffer data
**	       applies.
**	buffer - pointr to the buffer containing the newly read data
**	length - number of valid bytes of data in the buffer
**
** Note:
**	It is implicitly assumed that dopacket is called only
**	with cptr of "local" variation, which contains all the
**	necessary fields (buffer etc..)
*/
int	dopacket(cptr, buffer, length)
Reg3	aClient *cptr;
char	*buffer;
Reg4	int	length;
{
	Reg1	char	*ch1;
	Reg2	char	*ch2;
	register char *cptrbuf;
	aClient	*acpt = cptr->acpt;
 
	cptrbuf = cptr->buffer;
	me.receiveB += length; /* Update bytes received */
	cptr->receiveB += length;
	if (cptr->receiveB > 1023)
	    {
		cptr->receiveK += (cptr->receiveB >> 10);
		cptr->receiveB &= 0x03ff;	/* 2^10 = 1024, 3ff = 1023 */
	    }
	if (acpt != &me)
	    {
		acpt->receiveB += length;
		if (acpt->receiveB > 1023)
		    {
			acpt->receiveK += (acpt->receiveB >> 10);
			acpt->receiveB &= 0x03ff;
		    }
	    }
	else if (me.receiveB > 1023)
	    {
		me.receiveK += (me.receiveB >> 10);
		me.receiveB &= 0x03ff;
	    }
	ch1 = cptrbuf + cptr->count;
	ch2 = buffer;
	while (--length >= 0)
	    {
		register char g;
		g = (*ch1 = *ch2++);
		/*
		 * Yuck.  Stuck.  To make sure we stay backward compatible,
		 * we must assume that either CR or LF terminates the message
		 * and not CR-LF.  By allowing CR or LF (alone) into the body
		 * of messages, backward compatibility is lost and major
		 * problems will arise. - Avalon
		 */
		if ( g < '\16' && (g== '\n' || g == '\r'))
		    {
			if (ch1 == cptrbuf)
				continue; /* Skip extra LF/CR's */
			*ch1 = '\0';
			me.receiveM += 1; /* Update messages received */
			cptr->receiveM += 1;
			if (cptr->acpt != &me)
				cptr->acpt->receiveM += 1;
			if (parse(cptr, cptr->buffer, ch1, msgtab) ==
			    CPTR_KILLED)
				return CPTR_KILLED;
			/*
			** Socket is dead so exit
			*/
			if (IsDead(cptr))
			  return exit_client(cptr, cptr, &me, last_dead_comment);
			ch1 = cptrbuf;
		    }
		else if (ch1 < cptrbuf + (sizeof(cptr->buffer)-1))
			ch1++; /* There is always room for the null */
	    }
	cptr->count = ch1 - cptr->buffer;
	return 0;
}
