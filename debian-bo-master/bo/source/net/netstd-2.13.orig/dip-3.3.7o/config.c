/*
 * dip		A program for handling dialup IP connecions.
 *		This program handles the connections needed for dialup
 *		IP links, like SLIP or PPP.  It can handle both incoming
 *		and outgoing connections, using password security for
 *		incoming connections.  The outgoing connections use the
 *		system's dial(3) library if possible.
 *
 * Version:	@(#)config.c	3.3.3	08/16/93
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
#define GLOBAL
#include "dip.h"


#if DIP_SLIP
#define DIP_HAVE_SLIP	1		/* select "SLIP"		*/
#define DIP_HAVE_CSLIP	1		/* select "CompressedSLIP"	*/
#else
#define DIP_HAVE_SLIP	0		/* no  "SLIP"			*/
#define DIP_HAVE_CSLIP	0		/* no "CompressedSLIP"		*/
#endif


#if (DIP_HAVE_SLIP || DIP_SLIP)
extern void do_slip(struct dip *dip);
#endif
#if (DIP_HAVE_CSLIP || DIP_CSLIP)
extern void do_cslip(struct dip *dip);
#endif
#if (DIP_PPP || DIP_HAVE_PPP)
extern void do_ppp(struct dip *dip);
#endif
#if (DIP_TERM || DIP_HAVE_TERM)
extern void do_termp(struct dip *dip);
#endif


struct protosw protosw[] = {
#if (DIP_HAVE_SLIP || DIP_SLIP)
  { "SLIP",		1,	do_slip		},
#endif
#if (DIP_HAVE_CSLIP || DIP_CSLIP)
  { "CSLIP",		2,	do_cslip	},
#endif
#if (DIP_PPP || DIP_HAVE_PPP)
  { "PPP",		3,	do_ppp		},
#endif
#if (DIP_TERM || DIP_HAVE_TERM)
  { "TERM",		4,	do_termp	},
#endif
  { (char *)NULL,	0,	NULL		}
};


int
get_prot(char *name)
{
  register int i=0, j=0;

  if (isdigit(*name))
    j = atoi(name);

  while(protosw[i].name != (char *)NULL) {
	if (!strcasecmp(protosw[i].name, name)) return(protosw[i].type);
	if (protosw[i].type == j) return(protosw[i].type);
	i++;
  }
  return(0);
}
