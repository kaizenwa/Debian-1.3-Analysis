/*
 * This is taken from ifconfig.c:
 * Version:	@(#)ifconfig.c	2.30	10/07/93
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
#include <linux/if_ether.h>
#include <stdio.h>
#include <string.h>


/* Modified to use "struct enet_statistics" instead of "struct interface".*/
/* sewilco@fieldday.mn.org 95/12/27 */

void if_getstats(char *ifname, struct enet_statistics *stats)
{
  FILE *f=fopen("/proc/net/dev","r");
  char buf[256];
  char *bp;
  if(f==NULL)
  	return;
  while(fgets(buf,255,f))
  {
  	bp=buf;
  	while(*bp&&isspace(*bp))
  		bp++;
  	if(strncmp(bp,ifname,strlen(ifname))==0 && bp[strlen(ifname)]==':')
  	{
 		bp=strchr(bp,':');
 		bp++;
 		sscanf(bp,"%d %d %d %d %d %d %d %d %d %d %d",
 			&stats->rx_packets,
 			&stats->rx_errors,
 			&stats->rx_dropped,
 			&stats->rx_fifo_errors,
 			&stats->rx_frame_errors,
 			
 			&stats->tx_packets,
 			&stats->tx_errors,
 			&stats->tx_dropped,
 			&stats->tx_fifo_errors,
 			&stats->collisions,
 			
 			&stats->tx_carrier_errors
 		);
 		fclose(f);
 		return;
  	}
  }
  fclose(f);
}
