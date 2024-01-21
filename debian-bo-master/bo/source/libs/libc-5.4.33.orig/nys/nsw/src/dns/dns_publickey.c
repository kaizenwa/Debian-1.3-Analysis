/*
** publickey.c                      dummy publickey DNS access functions
**
** Copyright (C) 1993 Signum Support AB
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
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <rpc/rpc.h>
#include <rpc/key_prot.h>

extern int xdecrypt(char *secret, char *passwd);

int _dns_getpublickey (const char *netname, char *publickey)
{
	publickey = NULL;
	return (0);
}

int _dns_getsecretkey (const char *netname, char *secretkey, char *passwd)
{
	secretkey = NULL;
	return (0);
}
