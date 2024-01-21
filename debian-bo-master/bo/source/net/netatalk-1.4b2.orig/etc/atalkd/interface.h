/*
 * Copyright (c) 1990,1992 Regents of The University of Michigan.
 * All Rights Reserved. See COPYRIGHT.
 */

struct interface {
    struct interface	*i_next;
    char		i_name[ IFNAMSIZ ];
    int			i_flags;
    int			i_time;
    struct sockaddr_at	i_addr;
    struct sockaddr_at	i_caddr;
    struct ziptab	*i_czt;
    struct rtmptab	*i_rt;
    struct gate		*i_gate;
    struct atport	*i_ports;
};

#define IFACE_PHASE1	0x01
#define IFACE_PHASE2	0x02
#define IFACE_LOOPBACK	0x04		/* is the loopback interface */
#define IFACE_SEED	0x08		/* act as seed */
#define IFACE_ADDR	0x10		/* has an address set */
#define IFACE_CONFIG	0x20		/* has been configured */
#define IFACE_NOROUTER	0x40		/* no router on interface */
#define IFACE_LOOP	0x80		/* has a loopback route */

#define UNSTABLE	2
#define STABLE		0
#define STABLEANYWAY	-2

#define IFBASE		2	/* base number of interfaces (non-routing) */

#ifdef linux
#define LOOPIFACE	"lo"
#else linux
#define LOOPIFACE	"lo0"
#endif linux

extern struct interface	*interfaces;
extern struct interface	*ciface;
extern int		ninterfaces;
struct interface	*newiface();
