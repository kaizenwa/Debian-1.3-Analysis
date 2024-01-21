/*
** hosts_misc.c
**
** Copyright (c) 1993 Signum Support AB, Sweden
**
** This file is part of the NYS Library.
**
** The NYS Library is free software; you can redistribute it and/or
** modify it under the terms of the GNU Library General Public License as
** published by the Free Software Foundation; either version 2 of the
** License, or (at your option) any later version.
**
** The NYS Library is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
** Library General Public License for more details.
** 
** You should have received a copy of the GNU Library General Public
** License along with the NYS Library; see the file COPYING.LIB.  If
** not, write to the Free Software Foundation, Inc., 675 Mass Ave,
** Cambridge, MA 02139, USA.
**
** Authors: Peter Eriksson <pen@signum.se>
**          Michael A. Griffith <grif@cs.ucr.edu>
*/

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>


void print_hosts(struct hostent *host)
{
   int member_index;
      
   printf("'%s' %s ", 
	  host->h_name,
	  host->h_addrtype == AF_INET ? "AF_INET" : "UNKNOWN");

   for (member_index = 0 ;
	host->h_addr_list && host->h_addr_list[member_index] ;
	++member_index)
       if (member_index == 0)
	   fprintf(stdout, "'%s'", inet_ntoa( *(struct in_addr *)
					     host->h_addr_list[member_index]));
       else
	   fprintf(stdout, ", '%s'", inet_ntoa( *(struct in_addr *)
					       host->h_addr_list[member_index]));
   printf(" (");
   for (member_index = 0 ;
	host->h_aliases && host->h_aliases[member_index] ;
	++member_index)
      if (member_index == 0)
	 fprintf(stdout, "'%s'", host->h_aliases[member_index]);
      else
         fprintf(stdout, ", '%s'", host->h_aliases[member_index]);
   printf(")\n");
}













