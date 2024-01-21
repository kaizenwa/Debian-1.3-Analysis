#ifndef _RSPFAX25_H_
#define _RSPFAX25_H_
/*
 * rspfax25.h
 *
 * Functions required to wander through the ax.25 maze.  We have 
 * type-conversions here too.
 * Some of this is from /usr/src/linux/net/inet/ax25.c which is GPL
 */ 
char *ax25_ntoa(ax25_address *a);
ax25_address ax25_addr(char *ptr);
int ax25cmp(ax25_address *a, ax25_address *b);
int recv_ax25(unsigned char *buf, ax25_address *daddr, struct full_sockaddr_ax25 *saddr, int *pid, char *data, int *datalen);
#endif
