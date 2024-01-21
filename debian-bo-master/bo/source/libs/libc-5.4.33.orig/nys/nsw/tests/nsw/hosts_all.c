/*
** hosts_all.c
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
#include <netdb.h>

extern void print_hosts(struct hostent *);

int
main(int argc, char *argv[])
{
    struct hostent *host;

    
    sethostent(0);

    while ((host = gethostent()))
	print_hosts(host);

    endhostent();
    
    exit(0);
}
