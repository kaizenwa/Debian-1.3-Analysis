/*
 *		IPX protocol output functions.
 *		[Not yet input]
 *
 *			Alan Cox  <Alan.Cox@linux.org>
 *
 *		This program is free software; you can redistribute it
 *		and/or  modify it under  the terms of  the GNU General
 *		Public  License as  published  by  the  Free  Software
 *		Foundation;  either  version 2 of the License, or  (at
 *		your option) any later version.
 */
#include "config.h"

#if HAVE_AFIPX
#include <sys/types.h>
#include <sys/socket.h>
#include <linux/ipx.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <netinet/in.h>
#include "support.h"
#include "pathnames.h"
#define  EXTERN
#include "net-locale.h"


/* Display a ipx domain address. */
static char *
ipx_print(unsigned char *ptr)
{
  static char buff[64];
  struct sockaddr_ipx *sipx=(struct sockaddr_ipx *)(ptr-2);
  sprintf(buff,"%08lX:%02X:%02X:%02X:%02X:%02X:%02X",
  	ntohl(sipx->sipx_network),
  	(int)sipx->sipx_node[0],(int)sipx->sipx_node[1],
  	(int)sipx->sipx_node[2],(int)sipx->sipx_node[3],
  	(int)sipx->sipx_node[4],(int)sipx->sipx_node[5]);
  return(buff);
}


/* Display a ipx domain address. */
static char *
ipx_sprint(struct sockaddr *sap, int numeric)
{
  static char buf[64];

  if (sap->sa_family != AF_IPX)
    return(NLS_CATBUFF (catfd, ipxSet, ipx_none, "[NONE SET]", buf, 64));
  return(ipx_print(sap->sa_data));
}


struct aftype ipx_aftype = {
  "ipx",	NULL, /*"IPX",*/		AF_IPX,	0,
  ipx_print,	ipx_sprint,		NULL,		NULL
};

#endif
