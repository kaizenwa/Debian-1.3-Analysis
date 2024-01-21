/*
** services_port.c
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
#include <stdlib.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>


extern void print_service(struct servent *);


int
main(int argc, char *argv[])
{
    struct servent *sp;
   
    if (argc < 2)
	exit(1);
   
    sp = getservbyport(htons(atoi(argv[1])), argv[2]);

    if (sp)
	print_service(sp);
    else
	printf("No entry found\n");
    
    exit(0);
}
