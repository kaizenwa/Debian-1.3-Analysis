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

static char rcsid[] = "$Id: ypxfrd_getmap.c,v 1.5 1997/03/08 07:56:46 kukuk Exp $";

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "system.h"

#include <sys/types.h>
#include <time.h>
#include "ypxfrd.h"
#include "yp.h"
#include <rpc/rpc.h>
#include <sys/uio.h>
#if defined (HAVE_FCNTL_H)
#include <fcntl.h>
#else
#include <sys/fcntl.h>
#endif
#include <sys/stat.h>
#include <errno.h>
#include <memory.h>
#include <unistd.h>

#include "yp_msg.h"

#if !defined (HAVE_GETRPCPORT)
#include <compat/getrpcport.c>
#endif

static int file = 0;

static bool_t xdr_ypxfr_xfr(register XDR *xdrs, xfr *objp)
{
  while(1) {
    if (!xdr_xfr(xdrs, objp))
      return(FALSE);
    if (objp->ok == TRUE) 
      {
	if (write(file, objp->xfr_u.xfrblock_buf.xfrblock_buf_val,
		  objp->xfr_u.xfrblock_buf.xfrblock_buf_len) == -1) 
	  {
	    yp_msg("write failed: %s", strerror(errno));
	    return(FALSE);
	  }
      }
    xdr_free((xdrproc_t)xdr_xfr, (char *)objp);
    if (objp->ok == FALSE) 
      {
	switch(objp->xfr_u.xfrstat) 
	  {
	  case(XFR_DONE):
	    return(TRUE);
	    break;
	  case(XFR_DENIED):
	    yp_msg("access to map denied by rpc.ypxfrd");
	    return(FALSE);
	    break;
	  case(XFR_NOFILE):
	    yp_msg("reqested map does not exist");
	    return(FALSE);
	    break;
	  case(XFR_ACCESS):
	    yp_msg("rpc.ypxfrd couldn't access the map");
	    return(FALSE);
	    break;
	  case(XFR_BADDB):
	    yp_msg("file is not a database");
	    return(FALSE);
	    break;
	  case(XFR_READ_OK):
	    yp_msg("block read successfully");
	    return(TRUE);
	    break;
	  case(XFR_READ_ERR):
	    yp_msg("got read error from rpc.ypxfrd");
	    return(FALSE);
	    break;
	  case(XFR_DB_ENDIAN_MISMATCH):
	    yp_msg("rpc.ypxfrd databases have the wrong endian");
	    return(FALSE);
	    break;
	  case(XFR_DB_TYPE_MISMATCH):
	    yp_msg("rpc.ypxfrd doesn't support the needed database type");
	    return(FALSE);
	    break;
	  default:
	    yp_msg("got unknown status from rpc.ypxfrd");
	    return(FALSE);
	    break;
	  }
      }
  }
}

int ypxfrd_transfer(char *host, char *map, char *domain, char *tmpname)
{
  CLIENT *clnt;
  struct ypxfr_mapname req;
  struct xfr resp;
  struct timeval timeout = { 0, 25 };

  if (debug_flag) 
    yp_msg("Trying ypxfrd ...");

  if (!getrpcport(host, YPXFRD_FREEBSD_PROG, YPXFRD_FREEBSD_VERS, IPPROTO_TCP))
    {
      if(debug_flag)
	yp_msg(" not running\n");
      return 1;
    }
  
  req.xfrmap = map;
  req.xfrdomain = domain;
  req.xfrmap_filename = map;
  req.xfr_db_type = XFR_DB_GNU_GDBM;
  req.xfr_byte_order = XFR_ENDIAN_LITTLE;
  memset((char *)&resp, 0, sizeof(resp));
  
  if ((clnt = clnt_create(host, YPXFRD_FREEBSD_PROG,
			  YPXFRD_FREEBSD_VERS, "tcp")) == NULL)
    goto error;

  if ((file = open(tmpname, O_RDWR|O_CREAT, S_IRUSR|S_IWUSR)) == -1) 
    {
      clnt_destroy(clnt);
      yp_msg("couldn't open %s: %s", tmpname, strerror(errno));
      goto error;
    }
  
  if (clnt_call(clnt,YPXFRD_GETMAP, (xdrproc_t)xdr_ypxfr_mapname,
		(char *)&req, (xdrproc_t)xdr_ypxfr_xfr, 
		(char *)&resp, timeout) != RPC_SUCCESS) 
    {
      yp_msg("%s", clnt_sperror(clnt,"call to rpc.ypxfrd failed"));
      unlink(tmpname);
      clnt_destroy(clnt);
      close(file);
      goto error;
  }
  
  clnt_destroy(clnt);
  close(file);

  if (debug_flag) 
    yp_msg(" success\n");
  
  return 0;

error:
  if (debug_flag) 
    yp_msg(" failed\n");
  return 1;
}
