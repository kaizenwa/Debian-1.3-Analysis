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

#ifndef AARP_H
#define AARP_H

#include <sys/types.h>
#include "at.h"
#include <sys/stream.h>
#include <sys/ethernet.h>

#include "ddp.h"

/* Interface flags */
#define	ATIF_PROBE		1	  /* probing */
#define ATIF_PROBE_FAIL		2	  /* probe fail */
#define ATIF_PROBE_COMPLETE	3	  /* probe complete */

/* Timeout flags */
#define	TIMEOUT_AARP_PROBE	1000
#define TIMEOUT_AARP_TICK	1001
#define TIMEOUT_AARP_EXPIRY	1002

#define AARP_PROBE_TIMEOUT	1
#define	AARP_PROBE_REPLY	2

/* Timer data */
#define AARP_EXPIRY_SECOND     	(5*60)	  /* 5 min */
#define AARP_EXPIRY_TICK	(AARP_EXPIRY_SECOND*hz)
#define AARP_TICK_TIME		(hz/5)	  /* 5 per sec */
#define	AARP_RETRANSMIT_LIMIT	10	  /* probe 10 times, total of 2 sec.*/
#define AARP_RESOLVE_SECOND	10	  /* 10 seconds : some value bigger
					     than total retransmit time +
					     a bit for last reply to appear
					     and to stop continual requests */
#define AARP_RESOLVE_TIME	(AARP_RESOLVE_SEC*hz)

#define AARP_HW_TYPE_ETHERNET	1
#define AARP_HW_TYPE_TOKENRING	2

#define AARP_HASH_SIZE		16

#define AARPHRD_ETHER	0x0001

#define AARPOP_REQUEST	0x01
#define AARPOP_RESPONSE	0x02
#define AARPOP_PROBE	0x03

/*
 * This structure is used for both phase 1 and 2. Under phase 1
 * the net is not filled in. It is in phase 2. In both cases, the
 * hardware address length is (for some unknown reason) 4. If
 * anyone at Apple could program their way out of paper bag, it
 * would be 1 and 3 respectively for phase 1 and 2.
 */
union aapa {
    u_char		ap_pa[4];
    struct ap_node {
	u_char		an_zero;
	u_char		an_net[2];
	u_char		an_node;
    } ap_node;
};

/* Appletalk AARP header */
struct elapaarp
{
    u_short	hw_type;		/* hardware type: ethernet ... */
    u_short	pa_type;		/* protocol type: apple talk */
    u_char	hw_len;			/* hardware address len. */
    u_char	pa_len;			/* protocal address len. */
    u_short	function;		/* packet function */
    u_char	hw_src [ ETHERADDRL ]; 	/* source hardware address */
    union aapa	pa_src;			/* source protocol address */
    u_char	hw_dst [ ETHERADDRL ]; 	/* dest. hardware address */
    union aapa	pa_dst;			/* dest. protocol address */
};

/* Lists of aarp entries */
struct aarp_entry
{
    /* These first two are only used for unresolved entries */
    unsigned long	last_sent;  /* Last time we xmitted the aarp request*/
    unsigned long	expires_at;   /* Entry expiry time*/
    struct at_addr	target_addr;  /* DDP Address */
    u_char		hwaddr[ETHERADDRL];
				     /*Physical i/f address of target/router*/
    unsigned short	xmit_count;  /* When this hits 10 we give up */
    mblk_t		*mp;	     /* Message wait for resolution */
    atalk_iface		*dev;	     /* Interface structure */
    struct aarp_entry	*next;	     /* Next entry in chain */
};

/* Function prototypes */
int	aarp_send_ddp		( atalk_iface		*dev,
				  mblk_t		*mp,
				  struct at_addr	*sa,
				  u_char		*hwaddr );

void	aarp_expire_timeout	( void );


int	aarp_rput		( queue_t      		*q,
				  mblk_t		*mp );

int	aarp_probe_setup	( atalk_iface		*atif );

int	aarp_probe_process	( queue_t		*q,
				  int			callby );

void	aarp_startup		( void );

void	aarp_cleanup		( void );
/****************************************************************************/
#endif	/* !AARP_H */


