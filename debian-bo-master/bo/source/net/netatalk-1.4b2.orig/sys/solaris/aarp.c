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
*******************************************************************************
*
* Module	aarp.c
*
* Description	Appletalk aarp protocol for Sun Solaris 2.x.
*		This file is based from Linux version of aarp.c.
*		Some parts have been rewritten specific for stream.
*
* Change log
*	Simon Hung	April 11, 96
*	Change sizeof( struct aapa ) to sizeof ( union aapa )
*
*	Simon Hung	March 12,96
*	1st version
*
*****************************************************************************/
 
#include <sys/types.h>
#include "at.h"
#include <sys/stream.h>
#include <sys/ddi.h>
#include <sys/kmem.h>
#include <sys/cmn_err.h>
/* #include <sys/time.h> */
#include <sys/byteorder.h>
#include <sys/debug.h>

#include <errno.h>

#include "ddp.h"
#include "aarp.h"
#include "phase2.h"

/* Contants */
#define	RESOLVED_TABLE		0
#define UNRESOLVED_TABLE	1

/* Local variables */
static u_char	at_org_code[3] = { 0x08, 0x00, 0x07 };
static u_char	aarp_org_code[3] = { 0x00, 0x00, 0x00 };

/* lock varaible */
static kmutex_t	mu_aarp_table;		/* aarp table access lock */
static kmutex_t mu_aarp;		/* aarp global lock */

/* Hashed list of resolved and unresolved entries*/
static struct aarp_entry	*resolved[AARP_HASH_SIZE];
static struct aarp_entry	*unresolved[AARP_HASH_SIZE];
static int			unresolved_count=0;
static int			aarp_timerid = 0;
static u_char			aarp_eth_multicast[]=
					{ 0x09, 0x00, 0x07, 0xFF, 0xFF, 0xFF };
static u_char			*ddp_eth_multicast = aarp_eth_multicast;

/* Local function prototypes */
static mblk_t	*aarp_msg_allocb     	( u_short	function );

static void	aarp_send_query		( struct aarp_entry	*a );

static void	aarp_send_reply		( atalk_iface		*dev,
					  struct at_addr	*us,
					  struct at_addr	*them,
					  u_char		*sha );

static void	aarp_send_probe		( atalk_iface		*dev,
					  struct at_addr	*us );

static void	aarp_delete_entry      	( struct aarp_entry	*a );

static void	aarp_expire_timer	( struct aarp_entry	**n,
					  int			flag );

static void	aarp_kick		( struct aarp_entry	**n );

static struct aarp_entry *aarp_find_entry ( struct aarp_entry	*list,
					    atalk_iface		*dev,
					    struct at_addr	*sat);

static void	aarp_resolved		( struct aarp_entry	**list,
					  struct aarp_entry	*a,
					  int			hash );

static void	aarp_cleanup_entry	( struct aarp_entry	**list );

/******************************************************************************
*
* Function	aarp_msg_allocb
*
* Description	This function is used to create an aarp message block.
*
* Return	new message
*		NULL - fail
*
******************************************************************************/

static mblk_t	* aarp_msg_allocb (
u_short	function
)
{
    mblk_t		*new;
    struct elapaarp	*eah;
    int			size;

    dcmn_err((CE_CONT, "aarp_msg_allocb\n"));

    size = sizeof(struct llc) + sizeof(struct elapaarp);
    if ( (new = allocb ( size, BPRI_HI ))==NULL )
    {
	cmn_err ( CE_WARN, "aarp: failed allocb(elapaarp)" );
	return ( (mblk_t *) NULL );
    }
	
    /* Set up the ARP. */
    eah = ( struct elapaarp *) (new->b_rptr + sizeof(struct llc));
    eah->hw_type = htons(AARP_HW_TYPE_ETHERNET);
    eah->pa_type = htons(ETHERTYPE_AT);
    eah->hw_len	= ETHERADDRL;	
    eah->pa_len	= sizeof( union aapa );
    eah->function = htons( function );

    /* Update b_wptr */
    new->b_wptr = new->b_rptr + size;

    return ( new );
}
/*****************************************************************************/

/****************************************************************************
*
* Function	aarp_send_query
*
* Description	Send an aarp queue entry request
*
* Return	None
*
******************************************************************************/
 
static void	aarp_send_query (
struct aarp_entry *a
)
{
    mblk_t		*queryp;
    atalk_iface		*dev = a->dev;
    struct elapaarp	*eah;
    struct llc		*llc;

    /* Allocate query message block */
    if ( (queryp = aarp_msg_allocb (AARPOP_REQUEST)) == NULL )
    {
	cmn_err ( CE_WARN, "aarp: failed allocb(elapaarp)" );
	return;
    }

    /* Setup 802.3 aarp packet */
    llc = ( struct llc *) queryp->b_rptr;
    llc->llc_dsap = llc->llc_ssap = LLC_SNAP_LSAP;
    llc->llc_control = LLC_UI;
    bcopy ( (caddr_t) aarp_org_code, (caddr_t) llc->llc_org_code,
	    sizeof( aarp_org_code ) );
    llc->llc_ether_type = htons ( ETHERTYPE_AARP );

    eah = ( struct elapaarp *) (queryp->b_rptr + sizeof(struct llc));

    /* copy source address from the network device */
    bcopy ( (caddr_t) dev->dlpi.hwaddr, (caddr_t) eah->hw_src, ETHERADDRL );
    eah->pa_src.ap_node.an_zero  = 0;
    bcopy ( (caddr_t)&dev->aa.addr.s_net,(caddr_t)eah->pa_src.ap_node.an_net,
	    sizeof(eah->pa_src.ap_node.an_net));
    eah->pa_src.ap_node.an_node = dev->aa.addr.s_node;

    /* Set up dest. address */
    bzero ( (caddr_t) eah->hw_dst, ETHERADDRL );
    eah->pa_dst.ap_node.an_zero = 0;
    bcopy ((caddr_t)&a->target_addr.s_net,(caddr_t)eah->pa_dst.ap_node.an_net,
	    sizeof(eah->pa_dst.ap_node.an_net));
    eah->pa_dst.ap_node.an_node = a->target_addr.s_node;
	
    /* set target to the AARP multicast, and the send */
    dlpiSendData ( dev, queryp, aarp_eth_multicast );

    /* Update the sending count */
    a->xmit_count++;
}
/*****************************************************************************/

/******************************************************************************
*
* Function	aarp_send_reply
*
* Description	This function is used to reply an aarp message.
*
* Return	None
*
******************************************************************************/

static  void	aarp_send_reply (
atalk_iface	*dev,		/* interface data */
struct at_addr	*us,		/* our appletalk address */
struct at_addr	*them,		/* dest. appletalk address */
u_char		*sha		/* dest. hardware address */
)
{
    mblk_t		*replyp;
    struct elapaarp	*eah;
    struct llc		*llc;

    dcmn_err (( CE_CONT, "aarp_send_reply\n" ));

    /* Allocate query message block */
    if ( (replyp = aarp_msg_allocb (AARPOP_RESPONSE)) == NULL )
    {
	cmn_err ( CE_WARN, "aarp: failed allocb(elapaarp)" );
	return;
    }

    /* Fill in SNAP header information. */
    llc = ( struct llc *) replyp->b_rptr;
    llc->llc_dsap = llc->llc_ssap = LLC_SNAP_LSAP;
    llc->llc_control = LLC_UI;
    bcopy ( (caddr_t) aarp_org_code, (caddr_t) llc->llc_org_code,
	    sizeof(llc->llc_org_code ));
    llc->llc_ether_type = htons ( ETHERTYPE_AARP );

    /* Fill in aarp information */
    eah = ( struct elapaarp *) (replyp->b_rptr + sizeof( struct llc ));

    /* copy source address from the network device */
    bcopy ( (caddr_t) dev->dlpi.hwaddr, (caddr_t) eah->hw_src, ETHERADDRL );
    eah->pa_src.ap_node.an_zero  = 0;
    bcopy ( (caddr_t)&us->s_net,(caddr_t)eah->pa_src.ap_node.an_net,
	    sizeof(eah->pa_src.ap_node.an_net));
    eah->pa_src.ap_node.an_node = us->s_node;

    /* copy dest. address */
    if(sha==NULL)
	bzero ( (caddr_t) eah->hw_dst, ETHERADDRL );
    else
	bcopy ( (caddr_t)sha, (caddr_t) eah->hw_dst, ETHERADDRL );
	
    eah->pa_dst.ap_node.an_zero = 0;
    bcopy ( (caddr_t)&them->s_net,(caddr_t)eah->pa_dst.ap_node.an_net,
	    sizeof(eah->pa_dst.ap_node.an_net));
    eah->pa_dst.ap_node.an_node = them->s_node;
	
    /* set target to the AARP multicast, and send.*/
    dlpiSendData ( dev, replyp, sha );
}
/*****************************************************************************/

/******************************************************************************
*
* Function	aarp_send_probe
*
* Description	Send probe frames. Called from atif_probe_device.
*
* Return	None
*
******************************************************************************/
 
static void	aarp_send_probe (
atalk_iface	*dev,
struct at_addr	*us
)
{
    mblk_t		*probep;
    struct elapaarp	*eah;
    struct llc		*llc;

    dcmn_err(( CE_CONT, "aarp_send_probe,net=%d node=%d\n",
	       us->s_net, us->s_node ));

    /* Allocate query message block */
    if ( (probep = aarp_msg_allocb (AARPOP_PROBE))==NULL )
    {
	cmn_err ( CE_WARN, "aarp: failed allocb(elapaarp)" );
	return;
    }

    /* Fill in SNAP header information */
    llc = ( struct llc *) probep->b_rptr;
    llc->llc_dsap = llc->llc_ssap = LLC_SNAP_LSAP;
    llc->llc_control = LLC_UI;
    bcopy ( (caddr_t) aarp_org_code, (caddr_t) llc->llc_org_code,
	    sizeof(llc->llc_org_code) );
    llc->llc_ether_type = htons ( ETHERTYPE_AARP );

    /* Fill in probe data */
    eah = ( struct elapaarp *) ( probep->b_rptr + sizeof ( struct llc ));

    /* copy source address from the network device */
    bcopy ( (caddr_t) dev->dlpi.hwaddr, (caddr_t) eah->hw_src, ETHERADDRL );

    eah->pa_src.ap_node.an_zero  = 0;
    bcopy ( (caddr_t)&us->s_net,(caddr_t)eah->pa_src.ap_node.an_net,
	    sizeof(eah->pa_src.ap_node.an_net));
    eah->pa_src.ap_node.an_node = us->s_node;

    /* copy dest. address, for probing source address == dest. addres */
    bzero ( (caddr_t) eah->hw_dst, ETHERADDRL );
    eah->pa_dst.ap_node.an_zero = 0;
    bcopy ( (caddr_t) &us->s_net, (caddr_t) eah->pa_dst.ap_node.an_net,
	    sizeof( eah->pa_dst.ap_node.an_net) );
    eah->pa_dst.ap_node.an_node = us->s_node;
	
    /* set target to the AARP multicast, and the send */
    dlpiSendData ( dev, probep, aarp_eth_multicast );
}
/*****************************************************************************/

/*****************************************************************************
*
* Function	aarp_send_ddp
*
* Description	Send a DDP frame.
*		This function assumes user's message has already formatted
*		to comply IEEE 802.3 (LLC).
*
* Return	
*
******************************************************************************/
 
int aarp_send_ddp(
atalk_iface    *dev,
mblk_t	       *mp,
struct at_addr *sa,
u_char	       *hwaddr
)
{
    static	u_char macaddr[] = { 0x02, 0x60, 0x8c, 0x02, 0xea, 0x72 };

    int			hash;
    struct aarp_entry	*a;
    ulong		current_time;

    dcmn_err((CE_CONT, "aarp_send_ddp*******\n" ));

    /* if broadcast */
    if(sa->s_node==ATADDR_BCAST)
    {
	dlpiSendData ( dev, mp, ddp_eth_multicast );
	return 1;
    }

    /* get current timer in sec */
    if ( drv_getparm ( TIME, &current_time ) < 0 )
	cmn_err ( CE_WARN, "fail to get current time\n" );

    /* Work out hash */
    hash=((unsigned int)(sa->s_node)%(AARP_HASH_SIZE-1));
    dcmn_err((CE_CONT, "hash=%d\n", hash ));

    /* Do we have a resolved entry ? */
    if ( (a = aarp_find_entry ( resolved[hash], dev, sa)) )
    {
	/* Found entry */
	/* Reset expire time = current + expiry time*/
	dcmn_err((CE_CONT, "find entry\n"));
	a->expires_at= current_time + AARP_EXPIRY_SECOND;
	dlpiSendData ( dev, mp, a->hwaddr );
	return 1;
    }

    /* Do we have an unresolved entry: This is the less common path */
    if ( (a=aarp_find_entry(unresolved[hash],dev,sa)) )
    {
	/* Found in unresolve table */
	/* Queue onto the unresolved queue, return fail */
	return 0;
    }

    /* 1st time for this dest. */
    /* Allocate a new entry */
    if ( ( a = kmem_zalloc ( sizeof( struct aarp_entry ),KM_NOSLEEP)) == NULL )
    {
	/* Whoops slipped... good job it's an unreliable protocol 8) */
	cmn_err (CE_WARN, "aarp: aarp_alloc() failed" );
	return 0;
    }
    dcmn_err((CE_CONT, "new entry=%x\n", a ));

    /* save data into new entry */
    a->expires_at = current_time + AARP_RESOLVE_SECOND;
    a->target_addr= *sa;	/* save target address */
    a->xmit_count=0;		/* update by aarp_send_query */
    a->mp = mp;			/* buffer current data */
    a->dev = dev;		/* save interface */

    /* get lock */
    mutex_enter ( &mu_aarp_table );

    a->next=unresolved[hash];	/* the current entry becomes 2nd entry */

    /* place entry into unresolved table */
    unresolved[hash]=a;		/* the new entry becomes the 1st one */
    unresolved_count++;

    /* release locks */
    mutex_exit ( &mu_aarp_table );

    /* Send an initial request for the address	 */
    aarp_send_query(a);

    /* Switch to fast timer if needed (That is if this is the
     *	first unresolved entry to get added)	 */
    if(unresolved_count==1)
    {
	/* deletle current timer */
	if ( aarp_timerid )
	    untimeout ( aarp_timerid );

	/* start a new timer with new interval */
	aarp_timerid = timeout ( aarp_expire_timeout , (caddr_t) NULL,
				 AARP_TICK_TIME );
    }

    /* Tell the ddp layer we have taken over for this frame.*/
    return 1;
}
/*****************************************************************************/

/******************************************************************************
*
* Function	aarp_delete_entry
*
* Description	Delete pending message if exists, and free entry.
*
* Return	None
*
* Note		The function is a part of timer's callback function, therefore
*	        it doesn't require lock. If the function is called other than
*		callback, then the calling function must provides it own
*		mutex lock mechanism.
*
*****************************************************************************/

static void	aarp_delete_entry (
struct aarp_entry *a
)
{
    dcmn_err((CE_CONT, "delete entry for net=%d node=%d\n",
	      a->target_addr.s_net, a->target_addr.s_node ));

    /* free pending message */
    if ( a->mp )
	freemsg ( a->mp );

    /* free entry */
    kmem_free ( a, sizeof ( struct aarp_entry ) );
}
/****************************************************************************/

/******************************************************************************
*
* Function	aarp_expire_timer
*
* Decsription	Handle an aarp timer expire
*
* Return	None
*
* Note		The function is part of timer's callback function, therefore
*	        it doesn't require lock. If the function is called other than
*		callback, then the calling function must provides it own
*		mutex lock mechanism.
*		The 'flag' is really for unresolved table, but the entry
*		will always be removed by aarp_kick() anyway, it is there
*		just for consistence when an entry is removed from unresolved
*		table, the unresolved count will reflect the change. It might
*		be a better way, but need to look at it again.
*
******************************************************************************/

static void aarp_expire_timer (
struct aarp_entry **n,
int		  flag
)
{
    struct aarp_entry *t;
    ulong      	      current_time;

    if ( drv_getparm ( TIME, &current_time ) < 0 )
	cmn_err( CE_WARN, "drv_getparm() fail, current time is incorrect" );

    while((*n)!=NULL)
    {
	/* Expired ? */
	dcmn_err((CE_CONT, "expires_at=%d current=%d\n", (*n)->expires_at,
		  current_time ));
	if((*n)->expires_at < current_time )
	{
	    /* save entry */
	    t= *n;
	    *n=(*n)->next;	/* link previous to next */
	    aarp_delete_entry(t);
	    if ( (flag==UNRESOLVED_TABLE) && unresolved_count )
		unresolved_count--;
	}
	else
	    n=&((*n)->next);
    }
}
/*****************************************************************************/

/******************************************************************************
*
* Function	aarp_kick
*
* Description	Kick all pending requests 5 times a second.
*
* Return	None
*
* Note		The function is part of timer's callback function, therefore
*	        it doesn't require lock. If the function is called other than
*		callback, then the calling function must provides it own
*		mutex lock mechanism.
*
******************************************************************************/
 
static void aarp_kick (struct aarp_entry **n)
{
    struct aarp_entry *t;

    /* search through aarp_entry list */
    while((*n)!=NULL)
    {
	/* Expired - if this will be the 11th transmit, we delete instead */
	if((*n)->xmit_count>=AARP_RETRANSMIT_LIMIT)
	{
	    /* this entry has expired */
	    t= *n;
	    *n=(*n)->next;
	    aarp_delete_entry(t);
	    if ( unresolved_count )
		unresolved_count--;
	}
	else
	{
	    /* resend query again */
	    aarp_send_query(*n);
	    n=&((*n)->next);
	}
    }
}
/*****************************************************************************/

/******************************************************************************
*
* Function	aarp_expire_timeout
*
* Description	Handle the timer event
*
* Return	None
*
******************************************************************************/
 
void aarp_expire_timeout (
void
)
{
    int ct=0;

    dcmn_err((CE_CONT, "aarp_expire_timeout\n"));

    for(ct=0;ct<AARP_HASH_SIZE;ct++)
    {
	aarp_expire_timer(&resolved[ct], RESOLVED_TABLE);
	aarp_kick(&unresolved[ct]);
	aarp_expire_timer(&unresolved[ct], UNRESOLVED_TABLE );
    }

    aarp_timerid = timeout ( aarp_expire_timeout, (caddr_t) NULL, 
			     (unresolved_count)? AARP_TICK_TIME:
						 AARP_EXPIRY_TICK );
}
/*****************************************************************************/

/******************************************************************************
*
* Function	aarp_find_entry
*
* Description	Find an entry. We might return an expired but not yet purged
*		entry. We don't care as it will do no harm.
*
* Return	aarp entry
*
******************************************************************************/
 
static struct aarp_entry *aarp_find_entry (
struct aarp_entry *list,
atalk_iface	   *dev,
struct at_addr	   *sat
)
{
    mutex_enter ( &mu_aarp_table );

    /* sequential search */
    while(list)
    {
	if( list->target_addr.s_net==sat->s_net
	    && list->target_addr.s_node==sat->s_node
	    && list->dev==dev)
	    break;
	list=list->next;
    }

    mutex_exit ( &mu_aarp_table );

    dcmn_err((CE_CONT, "aarp_find_entry=%x\n", list ));

    return list;
}
/*****************************************************************************/

/*****************************************************************************
*
* Function	aarp_resolved
*
* Description	This function is called when an aarp entry is resolved.
*
* Return	None
*
******************************************************************************/

static void	aarp_resolved (
struct aarp_entry **list,
struct aarp_entry *a,
int		  hash
)
{
    dcmn_err((CE_CONT, "aarp_resolved: net=%d node=%d\n",
	      a->target_addr.s_net, a->target_addr.s_node ));

    mutex_enter ( &mu_aarp_table );

    /* note : list contains address of pointer that points to the block */
    /*
     * There's a bug here, or in the code that adds stuff to this list.
     * Under some circumstances, we get *many* packets sent, just after
     * we get a AAROP_RESPONSE.
     */
    while(*list!=NULL)
    {
	if(*list==a)
	{
	    /* match entry */
	    unresolved_count--;
	    *list=a->next;	/* link previous and next block */

	    /* Move into the resolved list */
	    a->next=resolved[hash];	/* put 1st to 2nd */
	    resolved[hash]=a;		/* put a to 1st */

	    /* Kick frames off */
	    if ( a->mp )
	    {
		dlpiSendData ( a->dev, a->mp, a->hwaddr );
		a->mp = NULL;
	    }
	    mutex_exit ( &mu_aarp_table );
	    return;
	}
	else
	    list=&((*list)->next);
    }
    mutex_exit ( &mu_aarp_table );
}
/*****************************************************************************/

/******************************************************************************
*
* Function	aarp_rput
*
* Description	This function is called when driver receives an aarp packet
*
* Return
*
******************************************************************************/

int aarp_rput (
queue_t		*q,
mblk_t		*mp
)
{
    atalk_iface		*dev = ( atalk_iface *) q->q_ptr;
    struct elapaarp	*ea;
    struct aarp_entry	*a;
    struct at_addr	sa;
    struct at_addr	*ma;
    unsigned long	flags;
    int			hash;
    struct atalk_iface	*ifa;

    dcmn_err((CE_CONT, "aarp_rput\n" ));

    /* Frame size ok ? */
    if( (mp->b_wptr-mp->b_rptr)<(sizeof(struct llc)+ sizeof(struct elapaarp)))
    {
	cmn_err(CE_WARN, "aarp_cv: packet is incorrect size" );
	freemsg ( mp );
	return 0;
    }

    ea = (struct elapaarp *) ( mp->b_rptr + sizeof(struct llc) );
    ea->function=ntohs(ea->function);

    /* Sanity check fields. */
    if( ea->function<AARPOP_REQUEST
        || ea->function > AARPOP_PROBE
        || ea->hw_len != ETHERADDRL
        || (ea->pa_len != sizeof( union aapa ))
        || (ea->pa_src.ap_node.an_zero != 0)
        || (ea->pa_dst.ap_node.an_zero != 0) )
    {
	freemsg ( mp );
	return 0;
    }
	
    /* Looks good */
    /* Work out hash value */
    hash=((unsigned int )(ea->pa_src.ap_node.an_node))%(AARP_HASH_SIZE-1);
    dcmn_err(( CE_CONT,"hash=%d\n", hash ));

    /* Build an address */
    /* sender address */
    sa.s_node=ea->pa_src.ap_node.an_node;
    bcopy ( (caddr_t) ea->pa_src.ap_node.an_net, (caddr_t) &sa.s_net,
	    sizeof( sa.s_net) );
	
    /* Process the packet */
	 
    /* Check for replies of me */
    if( dev->aa.flags==ATIF_PROBE)
    {	
	u_short dstnet;

	/* we should also check anyone probing our address,
	   if yes, we should abort too.- code need to be added */

	bcopy ( (caddr_t) ea->pa_dst.ap_node.an_net,
	        &dstnet, sizeof( u_short ) );

	dcmn_err((CE_CONT, "dst_net=%d dst_node=%d\n", dstnet,
		  ea->pa_dst.ap_node.an_node ));

	if( (dev->aa.addr.s_node==ea->pa_dst.ap_node.an_node)
	    && (dev->aa.addr.s_net==dstnet) )
	{
	    dcmn_err((CE_CONT, "Addres is already used\n" ));

	    /* someone is using the address that we want to use, so
	       try next one */
	    dcmn_err ((CE_CONT, "Response to our probe\n" ));
	    aarp_probe_process ( q, AARP_PROBE_REPLY );
	}

	freemsg ( mp );
	return 0;
    }
    else if ( dev->aa.flags == ATIF_PROBE_FAIL )
    {
	cmn_err(CE_WARN, "Interface - probe fail, so is not operational\n" );
	freemsg ( mp );
	return ( 0 );
    }

    switch(ea->function)
    {
    case AARPOP_RESPONSE:	
	dcmn_err((CE_CONT, "AARPOP_RESPONSE\n"));

	/* Speed up */
	if(unresolved_count==0)
	    break;

	/* Find the entry */
	if( (a=aarp_find_entry(unresolved[hash],dev,&sa))==NULL
	    || dev != a->dev)
	{
	    /* entry has removed from unresolved, so it ignore it and
	       throw it away */
	    break;
	}

	/* correct reply to our request, update entry */
	/* save hardware address */
	bcopy ( (caddr_t) ea->hw_src, (caddr_t) a->hwaddr, ETHERADDRL );

	/* move entry from unresolved table to resovled table */
	aarp_resolved(&unresolved[hash],a,hash);

	if( unresolved_count==0 )
	{
	    /* stop current timer */
	    if ( aarp_timerid )
		untimeout ( aarp_timerid );

	    /* start a new one */
	    aarp_timerid = timeout(aarp_expire_timeout, (caddr_t) NULL,
				   AARP_EXPIRY_TICK );
	}

	break;
			
    case AARPOP_REQUEST:
    case AARPOP_PROBE:
	dcmn_err((CE_CONT, "AARPOP_REQUEST, AARPOP_PROBE\n" ));

	/* If it is my address set ma to my address and reply. We can treat
	 * probe and request the same. Probe simply means we shouldn't cache
	 * the querying host,as in a probe they are proposing an address not
	 * using one. */
			 
	ma = &dev->aa.addr;
	sa.s_node=ea->pa_dst.ap_node.an_node;
	bcopy ( (caddr_t) ea->pa_dst.ap_node.an_net, (caddr_t) &sa.s_net,
	        sizeof(sa.s_net));

	dcmn_err((CE_CONT,"ma->net=%d node=%d, sa->net=%d node=%d\n",
		  ma->s_net, ma->s_node, sa.s_net, sa.s_node ));

	if( sa.s_node!=ma->s_node )
	    break;

	if( sa.s_net && ma->s_net && (sa.s_net!=ma->s_net) )
	    break;

	sa.s_node=ea->pa_src.ap_node.an_node;
	bcopy( (caddr_t) ea->pa_src.ap_node.an_net, (caddr_t) &sa.s_net,
	       sizeof ( sa.s_net) );
			
	/* send out response to either request or probe */
	aarp_send_reply(dev, ma, &sa, ea->hw_src);
	break;
    }

    freemsg ( mp );
    return 1;		
}
/*****************************************************************************/

/******************************************************************************
*
* Function	aarp_probe_setup
*
* Description	Initialize aarp probe process
*
* Return
*
******************************************************************************/

int	aarp_probe_setup (
atalk_iface	*atif
)
{
    IF_DATA	*ifp = &atif->aa;
    ulong	current_time;

    dcmn_err((CE_CONT, "aarp_probe_setup\n"));

    ASSERT ( atif != NULL );

    if ( ( ifp->flags==ATIF_PROBE )
	|| ( ifp->flags == ATIF_PROBE_FAIL)
	|| ( ifp->flags == ATIF_PROBE_COMPLETE))
    {
	/* It has already done the probe */
	dcmn_err ((CE_CONT, "flags=%d\n", ifp->flags ));
	ioctlReply ( ifp->q, ifp->mp, 0 );
	return ( 0 );
    }

    /* get current time value as radom generator seed */
    drv_getparm ( TIME, &current_time );

    /* get a lock */
    mutex_enter ( &atif->aa.mu_aarp );

    ifp->flags = ATIF_PROBE;
    ifp->net_range = ntohs( ifp->nr.nr_lastnet - ifp->nr.nr_firstnet) + 1;
    dcmn_err((CE_CONT, "net_range=%d\n", ifp->net_range ));

    if ( (ifp->probe_net = ntohs ( atif->aa.addr.s_net )) == ATADDR_ANYNET )
    {
	ifp->probe_net = ntohs ( ifp->nr.nr_firstnet );
	if ( ifp->net_range )
	    ifp->probe_net += ( current_time % ifp->net_range );

	atif->aa.addr.s_net = htons ( ifp->probe_net );
    }
    dcmn_err((CE_CONT, "probe_net=%d\n", ifp->probe_net ));

    if ( atif->aa.addr.s_node == ATADDR_ANYNODE )
	atif->aa.addr.s_node = current_time &0xff;
    dcmn_err((CE_CONT, "probe_node=%d\n", atif->aa.addr.s_node ));

    ifp->netct = 0;
    ifp->nodect = 0;

    /* just increment by 1 a time */
    ifp->nodeoff = 1;

    /* release lock */
    mutex_exit ( &atif->aa.mu_aarp );

    /* initialise timer */
    timerInit ( &atif->aa.timer, TIMEOUT_AARP_PROBE, hz/5, 
	       AARP_RETRANSMIT_LIMIT, atif->ReadQ );
    timerReset ( &atif->aa.timer, hz/5 , AARP_RETRANSMIT_LIMIT );

    aarp_send_probe ( atif, &ifp->addr );
}  
/*****************************************************************************/

/******************************************************************************
*
* Function	aarp_probe_process
*
* Description	The function is called when a timeout or probe response
*		message is received.
*
* Return	There are numerous exit points in this function, make sure
*		mutex_exit() is called before return.	
*
******************************************************************************/

int	aarp_probe_process (
queue_t	*q,
int	callby
)
{
    atalk_iface	*atif = ( atalk_iface *) q->q_ptr;
    IF_DATA	*ifp  = ( IF_DATA *) &atif->aa;

    ASSERT ( atif != NULL );

    dcmn_err((CE_CONT, "aarp_probe_process\n"));

    mutex_enter ( &atif->aa.mu_aarp );

    if ( atif->aa.flags != ATIF_PROBE )
    {
	cmn_err (CE_NOTE, "Interface is not in probe mode `\n" );
	mutex_exit ( &atif->aa.mu_aarp );
	return ( 0 );
    }

    if ( callby==AARP_PROBE_TIMEOUT )
    {
	/* Time out */
	if ( ifp->timer.times  )
	    aarp_send_probe ( atif, &ifp->addr );
	else
	{
	    /* no body reply to our probe,greate use current address */
	    dcmn_err((CE_CONT, "ATIF_PROBE_COMPLETE\n"));
	    dcmn_err((CE_CONT, "net=%d node=%d\n", ifp->addr.s_net,
		      ifp->addr.s_node ));
	    ifp->flags = ATIF_PROBE_COMPLETE;

	    /* reply ioctl (SETCSIFADDR) */
	    dcmn_err((CE_CONT, "aarp_probe_process: reply ioctl- OK" ));
	    ioctlReply ( atif->aa.q, atif->aa.mp, 0 );
	}
	mutex_exit ( &atif->aa.mu_aarp );
	return ( 1 );
    }

    /* get a response from our probe packet, someone is already using
       the appletalk address */

    /* disable current timer */
    timerStop ( &atif->aa.timer );

    /* emulate the old double 'for' loop */
    if ( ++ifp->nodect > 256 )
    {
	if ( (++ifp->netct) > ifp->net_range )
	{
	    /* The network is full */
	    dcmn_err((CE_CONT, "ATIF_PROBE_FAIL\n"));
	    ifp->flags = ATIF_PROBE_FAIL;
	    mutex_exit ( &atif->aa.mu_aarp );

	    /* reply ioctl ( SIOCSIFADDR ) with ENPSPC */
	    dcmn_err((CE_CONT, "aarp_probe_process: reply ioctl- FAIL" ));
	    ioctlReply ( atif->aa.q, atif->aa.mp, ENOSPC ); 
	    return ( 1 );
	}

	/* probe next net */
	ifp->probe_net++;
	if ( ifp->probe_net > (int )ntohs ( ifp->nr.nr_lastnet ) )
	    ifp->probe_net = ntohs ( ifp->nr.nr_firstnet );

	/* reset node for new net */
	ifp->nodect=0;
	ifp->nodeoff = 1;

    }

    /* next try */
    ifp->addr.s_net = htons ( ifp->probe_net );
    ifp->addr.s_node = ( ifp->nodect + ifp->nodeoff ) & 0xff;

    mutex_exit ( &atif->aa.mu_aarp );

    /* start timer */
    timerReset ( &atif->aa.timer, hz/5, AARP_RETRANSMIT_LIMIT );

    aarp_send_probe ( atif, &ifp->addr );
}
/*****************************************************************************/

/******************************************************************************
*
* Function	aarp_startup
*
* Description	This function is called to initialize aarp operation
*
* Return	None
*
******************************************************************************/

void	aarp_startup (
void
)
{
    int		i;

    dcmn_err((CE_CONT, "aarp_starup\n"));

    /* Initilize mutex */
    mutex_init ( &mu_aarp_table, "aarp mu table", MUTEX_DRIVER, NULL );
    mutex_init ( &mu_aarp, "aarp mu global", MUTEX_DRIVER, NULL );

    for ( i=0; i<AARP_HASH_SIZE; i++ )
    {
	resolved [i] = ( struct aarp_entry *) NULL;
	unresolved [i] = ( struct aarp_entry *) NULL;
    }
}
/*****************************************************************************/

/******************************************************************************
*
* Function	aarp_cleanup_entry
*
* Description	This function is used to clean up a aarp hash table.
*
* Return	None
*
******************************************************************************/

static	void aarp_cleanup_entry (
struct aarp_entry **list
)
{
    struct aarp_entry	*e;

    /* dcmn_err((CE_CONT, "aarp_cleanup_entry")); */

    /* loop for each entry in the list */
    while ( *list )
    {
	e = (*list)->next;
	dcmn_err(( CE_CONT, "%x ", *list ));
	kmem_free ( *list, sizeof(struct aarp_entry) );
	*list = e;
    }
    dcmn_err((CE_CONT, "\n" ));
}
/*****************************************************************************/

/******************************************************************************
*
* Function	aarp_cleanup
*
* Description	This function is called by driver close routine to do
*		cleanup aarp operation. 
*
* Return	None.
*
******************************************************************************/

void	aarp_cleanup (
void
)
{
    int	i;

    dcmn_err((CE_CONT,"aarp_cleanup\n"));

    /* clean up aarp's hash tables */
    for ( i=0; i<AARP_HASH_SIZE; i++ )
    {
	aarp_cleanup_entry ( &resolved[i] );
	aarp_cleanup_entry ( &unresolved[i] );
    }

    /* delete timer if it exists */
    if ( aarp_timerid )
	untimeout ( aarp_timerid );

    /* destroy mutex lock */
    mutex_destroy ( &mu_aarp_table );
    mutex_destroy ( &mu_aarp );

}
/*****************************************************************************/


