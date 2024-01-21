/*
 * Copyright (c) 1990,1991 Regents of The University of Michigan.
 * All Rights Reserved.
 *
 * Permission to use, copy, modify, and distribute this software and
 * its documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appears in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation, and that the name of The University
 * of Michigan not be used in advertising or publicity pertaining to
 * distribution of the software without specific, written prior
 * permission. This software is supplied as is without expressed or
 * implied warranties of any kind.
 *
 *	Research Systems Unix Group
 *	The University of Michigan
 *	c/o Mike Clark
 *	535 W. William Street
 *	Ann Arbor, Michigan
 *	+1-313-763-0525
 *	netatalk@itd.umich.edu
 */

/***************************************************************************** 
* Copyright (C) 1996 by Sun Microsystems Computer Co.
* 
* 
* Permission to use, copy, modify, and distribute this software and its
* documentation for any purpose and without fee is hereby granted, provided
* that the above copyright notice appear in all copies and that both that
* copyright notice and this permission notice appear in supporting
* documentation.  This software is provided "as is" without express or
* implied warranty.
*
******************************************************************************/

/*
 * <-1byte(8bits) ->
 * +---------------+
 * | 0 | hopc  |len|
 * +---------------+
 * | len (cont)    |
 * +---------------+
 * |               |
 * +- DDP csum    -+
 * |               |
 * +---------------+
 * |               |
 * +- Dest NET    -+
 * |               |
 * +---------------+
 * |               |
 * +- Src NET     -+
 * |               |
 * +---------------+
 * | Dest NODE     |
 * +---------------+
 * | Src NODE      |
 * +---------------+
 * | Dest PORT     |
 * +---------------+
 * | Src PORT      |
 * +---------------+
 *
 * On Apples, there is also a ddp_type field, after src_port. However,
 * under this unix implementation, user level processes need to be able
 * to set the ddp_type. In later revisions, the ddp_type may only be
 * available in a raw_appletalk interface.
 */

#ifndef DDP_H
#define DDP_H

#include <sys/socket.h>
#include <net/route.h>
#include <net/if.h>

#include "at.h"
#include "at_var.h"

struct elaphdr {
    u_char	el_dnode;
    u_char	el_snode;
    u_char	el_type;
};

#define	SZ_ELAPHDR	3

#define ELAP_DDPSHORT	0x01
#define ELAP_DDPEXTEND	0x02

/*
 * Extended DDP header. Includes sickness for dealing with arbitrary
 * bitfields on a little-endian arch.
 */
struct ddpehdr {
    union {
	struct {
#if BYTE_ORDER == BIG_ENDIAN
    unsigned		dub_pad:2;
    unsigned		dub_hops:4;
    unsigned		dub_len:10;
    unsigned		dub_sum:16;
#endif
#if 0 /* BYTE_ORDER == LITTLE_ENDIAN */
    unsigned		dub_sum:16;
    unsigned		dub_len:10;
    unsigned		dub_hops:4;
    unsigned		dub_pad:2;
#endif
	} du_bits;
	unsigned	du_bytes;
    } deh_u;
#define deh_pad		deh_u.du_bits.dub_pad
#define deh_hops	deh_u.du_bits.dub_hops
#define deh_len		deh_u.du_bits.dub_len
#define deh_sum		deh_u.du_bits.dub_sum
#define deh_bytes	deh_u.du_bytes
    u_short		deh_dnet;
    u_short		deh_snet;
    u_char		deh_dnode;
    u_char		deh_snode;
    u_char		deh_dport;
    u_char		deh_sport;
};

#define DDP_MAXHOPS	15

struct ddpshdr {
    union {
	struct {
#if BYTE_ORDER == BIG_ENDIAN
    unsigned		dub_pad:6;
    unsigned		dub_len:10;
    unsigned		dub_dport:8;
    unsigned		dub_sport:8;
#endif
#if 0 /* BYTE_ORDER == LITTLE_ENDIAN */
    unsigned		dub_sport:8;
    unsigned		dub_dport:8;
    unsigned		dub_len:10;
    unsigned		dub_pad:6;
#endif
	} du_bits;
	unsigned	du_bytes;
    } dsh_u;
#define dsh_pad		dsh_u.du_bits.dub_pad
#define dsh_len		dsh_u.du_bits.dub_len
#define dsh_dport	dsh_u.du_bits.dub_dport
#define dsh_sport	dsh_u.du_bits.dub_sport
#define dsh_bytes	dsh_u.du_bytes
};

typedef struct timer_data
{
    int			id;	       		/* timer id */
    int			type;	       		/* timer type */
    int			interval;      	       	/* timeout value */
    int			times;	       		/* total no.of timeout events*/
    queue_t		*replyq;       		/* queue for sending msg. */
    kmutex_t		mu;	       		/* mutex varaible */
    struct timer_data	*tp;   			/* pointer to itself */
} TIMER_DATA;

#define DLPI_ADDR_LEN	10			/* just for testing */
typedef struct {		/* network definition details */
    ulong		service_mode;		/* service mode */
    ulong		addr_length;		/* address length */
    long		sap_length;		/* sap addres */
    u_char		hwaddr[DLPI_ADDR_LEN]; 	/* hardware address + sap */
    long		brdcst_addr_length;	/* broadcast addr. length */
    u_char		brdcst_addr [DLPI_ADDR_LEN];
    ulong		ppa;			/* physical of attachment */
    ulong		sap;			/* service access point */
    TIMER_DATA	       	timer;       		/* DLPI timer */
    int			state;			/* DLPI state */
    kmutex_t		mu_dlpi;		/* mutex varaible */
} DLPI_DATA;

typedef struct {		/* Interface's aarp related data */
    struct at_addr	addr;			/* address in AppleTalk frmt.*/
    struct netrange	nr;			/* net range */
    int			flags;			/* flag */
    TIMER_DATA		timer;			/* aarp timer */
    kmutex_t		mu_aarp;       		/* mutex lock */
    int			probe_cnt;		/* probe count */
    int			probe_net;		/* probe net */
    int			net_range;
    int			netct;
    int			nodect;
    int			nodeoff;
    queue_t		*q;			/* probe reply q */
    mblk_t		*mp;			/* probe reply message */
} IF_DATA;

#define	ATNAMSIZ	16
typedef struct atalk_iface {	/* maintain a list of network interfaces */
    char		ifr_name[ATNAMSIZ];	/* device name */
    int			flags;			/* interface flag */
    int                 status;			/* status of the interface */
    queue_t		*ReadQ;			/* read queue */
    queue_t		*WriteQ;		/* write queue */
    mblk_t		*UnitDataReq;		/* DL_UNITDATA_REQ template */
    DLPI_DATA		dlpi;			/* dlpi data */
    IF_DATA		aa;			/* Aarp data */
    u_char		testaddr[10];		/* test address */
    struct atalk_iface	*next;			/* points to next struct */
} atalk_iface;

/* this structure is for ioctl ( IF_UNITSEL ) */
typedef struct {
    /* ioctl data structure to configure interface */
    unsigned long ppa;		/* physic point of attachment */
    unsigned long sap;		/* service access point */
    u_char  testaddr[6];	/* test hardware address, for test only */
} DDP_SELECTIF;

struct atalk_sock {		/*  */
  unsigned short dst_net;
  unsigned short src_net;
  unsigned short dst_node;
  unsigned short src_node;
  unsigned short dst_port;
  unsigned short src_port;
};

struct atalk_socket {		/* maintain a linked list of application socket connections */
  char name[16];
  struct atalk_socket *next;
  int minor;
  int mtu;
  struct atalk_sock at;
  int zapped;
  int state;
  int broadcast;
  queue_t *rdq, *wrq;
  char pad[16];
};
typedef struct atalk_socket atalk_socket;

struct atalk_route {		/* maintain a linked list of valid ddp routes */
  atalk_iface *dev;
  struct at_addr target;
  struct at_addr gateway;
  int flags;
  struct atalk_route *next;	/* all are kept in a global linked list */
};

struct userArg {		/* needed to do transparent COPYIN and COPYOUT STREAMS crap */
  int copy;			/* M_COPYIN is user=>kernel, M_COPYOUT is kernel=>user */
  int size;			/* handler is the only one who knows - thats the problem */
  void *addr;			/* address user of user arg - arg3 of ioctl() */
  int cnt;			/* must count multiple M_COPYIN or M_COPYOUT passes */
};

/* Constants */
#define	YES		1
#define NO		0

#define	NO_ERROR	0


/* DEBUG macro definition */
#ifdef DEBUG
#define DO_DEBUG 1
#define	dcmn_err(X)	if (ddp_debug) cmn_err X
#else
#define DO_DEBUG 0
#define dcmn_err(X)	/* No debug message */
#endif

/* GLOBAL PROTOTYPES */
void ioctlReply				( queue_t	*q,
					  mblk_t	*mp,
					  int		error);
void dlpiSendData			( atalk_iface	*atp,
					  mblk_t	*mp,
					  u_char	*EtherAddr);
void transparentCopy			( queue_t	*q,
					  mblk_t	*mp,
					  struct userArg *ua);
atalk_iface *findNamedInterface		(char		*device_name);

struct at_addr *findPrimary		(void);

/* Timer library functions */
int	timerInit		( TIMER_DATA	*tp,
				  int		type,
				  int		interval,
				  int		times,
				  queue_t	*readq );

int	timerStart		( TIMER_DATA	*tp );

int	timerReset		( TIMER_DATA	*tp,
				  int		interval,
				  int		times );

void	timerStop		( TIMER_DATA	*tp );

void	timerDestroy		( TIMER_DATA	*tp );

/* Tpi utility function */
atalk_socket	*locateSocket 	( atalk_socket		*s,
				  struct sockaddr_at	*sat );

/* GLOBAL VARIABLES */
int ddp_debug;
struct atalk_socket *atalk_socket_list;
struct atalk_route *atalk_router_list;
struct atalk_route DefaultRoute;
atalk_iface *atalk_iface_list;
kmutex_t AtalkSocketMutex;
kmutex_t AtalkIfaceMutex;
kmutex_t AtalkRouteMutex;
int ddp_nlocalrts;
int ddp_ndevs;
char *ddp_mindevs;

#endif /* !DDP_H */
