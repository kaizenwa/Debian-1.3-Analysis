/*
 * dip		A program for handling dialup IP connecions.
 *		This module handles the PPP protocol.
 *
 * Version:	@(#)ppp.c	3.3.3	08/16/93
 * Modified:	@(#)ppp.c	3.3.4	01/09/95
 *
 * Author:      Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
 *		Copyright 1988-1993 MicroWalt Corporation
 *
 * Modified:	Uri Blumenthal <uri@watson.ibm.com>
 *
 *		This program is free software; you can redistribute it
 *		and/or  modify it under  the terms of  the GNU General
 *		Public  License as  published  by  the  Free  Software
 *		Foundation;  either  version 2 of the License, or  (at
 *		your option) any later version.
 *
 * 		Peter Wassenaar Oct 1 '94:
 *    		Turned `not implemented yet' into a working DIP/PPPD 
 *		interface ppp-daemon takes over the terminal 
 *		control from `dip'
 * 
 *              Dirk Melchers   dirk@merlin.nbg.sub.org Jan 3, 95:
 *              Added support for different IP addresses via $local
 *	        and $remote.
 */
#include "dip.h"


void
do_ppp(struct dip *dip)
{
  int stat;
  char string[255];

  (void) strcpy((char *) dip->protocol, "PPP");

  dup2(tty_askfd(),0);
  dup2(tty_askfd(),1);    /* re-direct cua? to stdin/stdout */

  if (dip->loc_ip.s_addr == INADDR_ANY) { /* loc IP addr not set */
      /* PPP will negotiate everything by itself, automatic!     */
      stat=execlp(_PATH_BIN_PPP, "pppd", "-detach", "defaultroute",
		  "noipdefault", "crtscts", "modem", NULL);
  }  else  {
    if (dip->rmt_ip.s_addr == INADDR_ANY) { /*rem IP addr not set*/
      /* DIP found the addresses necessary and we are */
      /* passing them to the PPP to use.              */
      sprintf(string,"%s:", dip->local);
    } else { 
      /* DIP found the addresses necessary and we are */
      /* passing them to the PPP to use.              */
      sprintf(string,"%s:%s", dip->local, dip->remote);
    }
    stat=execlp(_PATH_BIN_PPP, "pppd", "-detach", "defaultroute", 
		"crtscts", "modem", string, NULL);
  }

  perror("DIP: do_ppp(pppd)");    /* point of no return */
  return;
}
