/*
 *	This module:
 *		This module is free software; you can redistribute it and/or
 *		modify it under the terms of the GNU General Public License
 *		as published by the Free Software Foundation; either version
 *		2 of the License, or (at your option) any later version.
 *
 *
 *
 */
#include <sys/socket.h>
#include <linux/ax25.h>
#include <stdlib.h>

#include <stdio.h> 
#include "rspfax25.h"

/* ax25 -> ascii conversion */

char *ax25_ntoa(ax25_address *a)
{
	static char buf[11];
	char c, *s;
	int n;

	for (n = 0, s = buf; n < 6; n++)
	{
		c = (a->ax25_call[n] >> 1) & 0x7F;

		if (c != ' ') *s++ = c;
	}
	
	*s++ = '-';

	if ((n = ((a->ax25_call[6] >> 1) & 0x0F)) > 9)
	{
		*s++ = '1';
		n -= 10;
	}
	
	*s++ = n + '0';
	*s++ = '\0';

	return(buf);

}

/* Convert from ascii to ax25 internal */
ax25_address ax25_addr(char *ptr)
{
	ax25_address axaddr;
	char *s = (char*)&(axaddr.ax25_call);
	unsigned int n;

	
	while( (*ptr != '-') &&	( (int)(s - (char*)&(axaddr.ax25_call)) < 6) )
		*s++ = (*ptr++) << 1;
	ptr++;	/* Skip over dash */
	
	n = atoi(ptr);
	*s = n << 1;
	return axaddr;
}


/* Compare two ax.25 addresses */

int ax25cmp(ax25_address *a, ax25_address *b)
{
	int ct=0;
	while(ct<6)
	{
		if((a->ax25_call[ct]&0xFE)!=(b->ax25_call[ct]&0xFE))	/* Clean off repeater bits */
			return 1;
		ct++;
	}
 	if((a->ax25_call[ct]&0x1E)==(b->ax25_call[ct]&0x1E))	/* SSID without control bit */
 		return 0;
 	return 2;			/* Partial match */
}

int recv_ax25(unsigned char *buf, ax25_address *daddr, struct full_sockaddr_ax25 *saddr, int *pid, char *data, int *datalen)
{

	return 1;
}
	
