/*
 * NET-2	This file contains the top-level part of the hardware
 *		support functions module for the NET-2 base distribution.
 *
 * Version:	@(#)hw.c	1.10	10/07/93
 *
 * Author:	Fred N. van Kempen, <waltje@uwalt.nl.mugnet.org>
 *		Copyright 1993 MicroWalt Corporation
 *
 *		This program is free software; you can redistribute it
 *		and/or  modify it under  the terms of  the GNU General
 *		Public  License as  published  by  the  Free  Software
 *		Foundation;  either  version 2 of the License, or  (at
 *		your option) any later version.
 */
#include <sys/types.h>
#include <sys/socket.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include "config.h"
#include "support.h"
#include "pathnames.h"
#define  EXTERN
#include "net-locale.h"


extern	struct hwtype	unspec_hwtype;
extern	struct hwtype	loop_hwtype;

extern	struct hwtype	slip_hwtype;
extern	struct hwtype	cslip_hwtype;
extern	struct hwtype	slip6_hwtype;
extern	struct hwtype	cslip6_hwtype;
extern	struct hwtype	adaptive_hwtype;

extern	struct hwtype	ether_hwtype;

extern	struct hwtype	ax25_hwtype;
extern  struct hwtype   kiss_hwtype;

extern struct hwtype	ppp_hwtype;

extern struct hwtype	arcnet_hwtype;

static struct hwtype *hwtypes[] = {

  &loop_hwtype,

#if HAVE_HWSLIP
  &slip_hwtype,
  &cslip_hwtype,
  &slip6_hwtype,
  &cslip6_hwtype,
  &adaptive_hwtype,
#endif
  &unspec_hwtype,
#if HAVE_HWETHER
  &ether_hwtype,
#endif
#if HAVE_HWAX25
  &ax25_hwtype,
#endif
#if HAVE_HWPPP
  &ppp_hwtype,
#endif  
#if HAVE_HWARC
  &arcnet_hwtype,
#endif  
  NULL
};

static short sVhwinit = 0;

void hwinit ()
{
  loop_hwtype.title = NLS_CATSAVE (catfd, loopbackSet, loopback_loop, "Local Loopback");
#if HAVE_HWSLIP
  slip_hwtype.title = NLS_CATSAVE (catfd, slipSet, slip_slip, "Serial Line IP");
  cslip_hwtype.title = NLS_CATSAVE (catfd, slipSet, slip_cslip, "VJ Serial Line IP");
  slip6_hwtype.title = NLS_CATSAVE (catfd, slipSet, slip_slip6, "6-bit Serial Line IP");
  cslip6_hwtype.title = NLS_CATSAVE (catfd, slipSet, slip_cslip6, "VJ 6-bit Serial Line IP");
  adaptive_hwtype.title = NLS_CATSAVE (catfd, slipSet, slip_adaptive, "Adaptive Serial Line IP");
#endif
  unspec_hwtype.title = NLS_CATSAVE (catfd, loopbackSet, loopback_unspec, "UNSPEC");
#if HAVE_HWETHER
  ether_hwtype.title = NLS_CATSAVE (catfd, etherSet, ether_ether, "10Mbps Ethernet");
#endif
#if HAVE_HWAX25
  ax25_hwtype.title = NLS_CATSAVE (catfd, ax25Set, ax25_hw, "AMPR AX.25");
#endif
#if HAVE_HWPPP
  ppp_hwtype.title = NLS_CATSAVE (catfd, pppSet, ppp_ppp, "Point-Point Protocol");
#endif  
#if HAVE_HWARC
  arcnet_hwtype.title = NLS_CATSAVE (catfd, arcnetSet, arcnet_arcnet, "1.5Mbps ARCnet");
#endif
  sVhwinit = 1;
}

/* Check our hardware type table for this type. */
struct hwtype *
get_hwtype(char *name)
{
  struct hwtype **hwp;

  if (!sVhwinit)
    hwinit();
  
  hwp = hwtypes;
  while (*hwp != NULL) {
	if (!strcmp((*hwp)->name, name)) return(*hwp);
	hwp++;
  }
  return(NULL);
}


/* Check our hardware type table for this type. */
struct hwtype *
get_hwntype(int type)
{
  struct hwtype **hwp;

  if (!sVhwinit)
    hwinit();

  hwp = hwtypes;
  while (*hwp != NULL) {
	if ((*hwp)->type == type) return(*hwp);
	hwp++;
  }
  return(NULL);
}
