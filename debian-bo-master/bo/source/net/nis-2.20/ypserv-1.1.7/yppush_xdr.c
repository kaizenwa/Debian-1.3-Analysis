/*
** Copyright (c) 1996 Thorsten Kukuk 
**
** This file is part of the NYS YP Server.
**
** The NYS YP Server is free software; you can redistribute it and/or
** modify it under the terms of the GNU General Public License as
** published by the Free Software Foundation; either version 2 of the
** License, or (at your option) any later version.
**
** The NYS YP Server is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
** General Public License for more details.
** 
** You should have received a copy of the GNU General Public
** License along with the NYS YP Server; see the file COPYING.  If
** not, write to the Free Software Foundation, Inc., 675 Mass Ave,
** Cambridge, MA 02139, USA.
**
** Author: Thorsten Kukuk <kukuk@uni-paderborn.de>
*/

static char rcsid[] = "$Id: yppush_xdr.c,v 1.3 1996/08/18 13:41:00 kukuk Exp $";

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "system.h"

#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <memory.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "yp.h"

#include "yp_msg.h"
#include "yppush.h"

/*
** If we use our own version under SunOS 4.x, we will get 
** problems with gethostname, etc.
** But under Linux, we need this since the xdr_* stuff under
** libc 4.x, 5.x is really broken. GNU libc 6 does not have
** the xdr_* functions
*/
#if !defined(__sun__) && !defined(sun)

bool_t xdr_domainname(XDR *xdrs, domainname *objp)
{
  if (!xdr_string(xdrs, objp, YPMAXDOMAIN))
    return (FALSE);
  return (TRUE);
}

bool_t xdr_mapname(XDR *xdrs, mapname *objp)
{  
  if (!xdr_string(xdrs, objp, YPMAXMAP))
    return (FALSE);
  return (TRUE);
}

bool_t xdr_peername(XDR *xdrs, peername *objp)
{  
  if (!xdr_string(xdrs, objp, YPMAXPEER))
    return (FALSE);
  return (TRUE);
}

bool_t xdr_ypmap_parms(XDR *xdrs, ypmap_parms *objp)
{
  if (!xdr_domainname(xdrs, &objp->domain))
    return (FALSE);
  if (!xdr_mapname(xdrs, &objp->map))
    return (FALSE);
  if (!xdr_u_int(xdrs, &objp->ordernum))
    return (FALSE);
  if (!xdr_peername(xdrs, &objp->peer))
    return (FALSE);
  return (TRUE);
}

bool_t xdr_ypreq_xfr(XDR *xdrs, ypreq_xfr *objp)
{ 
  if (!xdr_ypmap_parms(xdrs, &objp->map_parms))
    return (FALSE);
  if (!xdr_u_int(xdrs, &objp->transid))
    return (FALSE);
  if (!xdr_u_int(xdrs, &objp->prog))
    return (FALSE);
  if (!xdr_u_int(xdrs, &objp->port))
    return (FALSE);
  return (TRUE);
}

bool_t xdr_yppush_status(XDR *xdrs, yppush_status *objp)
{
  if (!xdr_enum(xdrs, (enum_t *)objp))
    return (FALSE);
  return (TRUE);
}

bool_t xdr_yppushresp_xfr(XDR *xdrs, yppushresp_xfr *objp)
{
  if (!xdr_u_int(xdrs, &objp->transid))
    return (FALSE);
  if (!xdr_yppush_status(xdrs, &objp->status))
    return (FALSE);
  return (TRUE);
}

#endif
