#ifndef _RSPFIF_H_
#define _RSPFIF_H_
/*
 * rspf_if.h
 *
 * These are all the routines for the interface handling
 *
 * Changes:
 * 13/03/95 cs  Created
 * 12/05/95 cs  Shifted address and tx_pkts into here
 */

void iface_setup(void) ;
void add_iface(char *name, u_char cost);
void clear_all_ifaces(void);
int is_rspfif(char *name);
int rspf_ifaces(char *buf, int buflen);
u_char get_iface_cost(char *port);

u_long get_bcast_addr(char *ifname);
u_long get_iface_addr(char *ifname);
int get_tx_pkts(char *iface);

struct rspf_if{
	char name[IFNAMSIZ];
	u_char cost;
};

#endif /* RSPFIF_H */