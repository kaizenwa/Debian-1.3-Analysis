/*
 * dip		A program for handling dialup IP connecions.
 *		This module handles the SLIP protocol.
 *
 * Version:	@(#)slip.c	3.3.3	08/16/93
 *
 * Author:      Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
 *		Copyright 1988-1993 MicroWalt Corporation
 *
 *		This program is free software; you can redistribute it
 *		and/or  modify it under  the terms of  the GNU General
 *		Public  License as  published  by  the  Free  Software
 *		Foundation;  either  version 2 of the License, or  (at
 *		your option) any later version.
 */
#include "dip.h"


/* SLIP protocol constants. */
#define	END		0xC0		/* end-of-datagram marker	*/
#define	ESC		0xDB		/* Binary Escape marker		*/
#define	ESC_END		0xDC		/* Escaped END character	*/
#define	ESC_ESC		0xDD		/* Escaped ESCAPE character	*/


void
do_slip(struct dip *dip)
{
  (void) strcpy((char *) dip->protocol, "SLIP");
  
  /* Put line in SLIP discipline. */
  if (tty_set_disc(N_SLIP) < 0) {
    syslog(LOG_ERR, "SETD(N_SLIP): %m");
    return;
  }
  
  if (opt_v == 1)
    syslog(LOG_INFO, "slip: set_disc N_SLIP (%d)", N_SLIP);

  /* Disable VJ Header Compression. */
  if (tty_set_encap(0x0000) < 0) {
    syslog(LOG_ERR, "SET_ENCAP(VJ_COMP=0): %m");
    return;
  }
  
  if (opt_v == 1)
    syslog(LOG_INFO, "slip: set_encap(0x0000)");
  
  /* Ask the kernel for the name of our interface. */
  if (tty_get_name(dip->ifname) < 0) {
	syslog(LOG_ERR, "GIFNAME: %m");
	(void) tty_set_disc(-1);
	return;
  }
  
  if (opt_v == 1) {
      printf("slip: interface %s\n", dip->ifname);
  }

  /* Add the route to that host. */
  (void) attach(dip);
}


void
do_cslip(struct dip *dip)
{
  (void) strcpy((char *) dip->protocol, "CSLIP");

  /* Put line in SLIP discipline. */
  if (tty_set_disc(N_SLIP) < 0) {
    syslog(LOG_ERR, "SETD(N_SLIP): %m");
    return;
  } 
    
  if (opt_v == 1)
    syslog(LOG_INFO, "cslip: set_disc N_SLIP (%d)", N_SLIP);

  /* Enable VJ Header Compression. */
  if (tty_set_encap(0x0001) < 0) {
    syslog(LOG_ERR, "SET_ENCAP(VJ_COMP=1): %m");
    return;
  }
  
  if (opt_v == 1)
    syslog(LOG_INFO, "cslip: set_encap(0x0001)");
  
  /* Ask the kernel for the name of our interface. */
  if (tty_get_name(dip->ifname) < 0) {
    syslog(LOG_ERR, "GIFNAME: %m");
    (void) tty_set_disc(-1);
    return;
  }
  
  if (opt_v == 1) {
      printf("cslip: interface %s\n", dip->ifname);
  }
  /* Add the route to that host. */
  (void) attach(dip);
}
