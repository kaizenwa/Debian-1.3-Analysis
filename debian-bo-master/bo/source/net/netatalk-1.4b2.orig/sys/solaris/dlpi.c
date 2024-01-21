/******************************************************************************
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
* Module	dlpi.c
*
* Description	This module provides all the facilities required by Netatalk
*		for accessing DLPI layer.
*
* Written by	Simon Hung	Sun, OPCOM.
*		Feb, 96
*
* Change log
*	Simon Hung	April 11, 96
*	Removed test section for aarp_probe operation, since we can
*	use socket to initiate the test.
*
*	Simon Hung	Feb 12,96
*	Initial version
*
******************************************************************************/

/* Include files */
#include <sys/conf.h>
#include <sys/stream.h>
#include <sys/stropts.h>
#include <sys/errno.h>
#include <sys/dlpi.h>
#include <sys/ethernet.h>
#include <sys/cmn_err.h>
#include <sys/socket.h>
#include <sys/sockio.h>
#include <sys/kmem.h>
#include <sys/debug.h>
#include <sys/byteorder.h>

#include <net/if.h>	
#include <net/route.h>
#include <netinet/arp.h>

#include <inet/common.h>
#include <inet/mi.h>
#include <inet/mib2.h>

#include "ddp.h"
#include "aarp.h"
#include "phase2.h"

#include <sys/ddi.h>		/* always include ddi's last */
#include <sys/sunddi.h>

/* DLPI State Constants */
enum {
    ATALK_DLPI_OPEN=1,
    ATALK_DLPI_WAITINFO,
    ATALK_DLPI_TIMEOUT,
    ATALK_DLPI_ATTACH,
    ATALK_DLPI_XV1,
    ATALK_DLPI_OPERATE,
};

extern void ddptp_rput(queue_t *, mblk_t *);

/* Local Prototypes */
static void	dlpiDisplayInfo		(mblk_t		*mp);

static void	dlpiInfoTimer		(DLPI_DATA	*dp);

static void	dlpiIoctl		(queue_t	*q,
					 mblk_t		*mp);


static void	dlpiIoctlData		(queue_t	*q,
					 mblk_t		*mp);

static int	dlpiAttachReq		(atalk_iface	*atp);

static int	dlpiBindReq		(atalk_iface	*atp);

static int	dlpiInit		(queue_t	*q);

static void	dlpiRead		(queue_t	*q,
					 mblk_t		*mp);

static int	dlpiRecvBindAck		(queue_t	*q,
					 mblk_t		*mp);

static void	dlpiRecvInfo		(queue_t	*q,
					 mblk_t		*mp);

static void	dlpiRecvUnitdata	(queue_t	*q,
					 mblk_t		*mp);

static void	dlpiSendMdata		(queue_t	*q,
					 mblk_t		*mp);

static mblk_t	*dlpiMessage		(ulong		Primitive,
					 int		Size);

static int	unitSelect		(atalk_iface	*atp,
					 DDP_SELECTIF	*dp);

int	dlpiEnableMulticast	( atalk_iface	*atp, u_char *multicast );

int dlpi_startup(void)
{
    atalk_iface *iface;

    if ((iface = kmem_alloc(sizeof *iface, KM_NOSLEEP)) == NULL) {
	cmn_err(CE_WARN, "No memory available for interface structure");
	return ENOMEM;
    }
    bzero((void *)iface, sizeof *iface);

    /* insert interface into ddp global list */
    mutex_enter(&AtalkIfaceMutex);
    iface->next = atalk_iface_list;
    atalk_iface_list = iface;
    mutex_exit(&AtalkIfaceMutex);

    strcpy (iface->ifr_name, "lo0");
    iface->flags |= IFF_LOOPBACK;
    iface->aa.flags = ATIF_PROBE_COMPLETE;
    iface->dlpi.state = ATALK_DLPI_OPEN;
    dcmn_err((CE_CONT, "Opened loopback interface for <%s>\n",
	      iface->ifr_name));
    return 0;
}

/******************************************************************************

/******************************************************************************
 * Description	This function is called when the driver is push into
 *		a stream, it is also responsible for initializing
 *		global data structure if it is called for the 1st time
 *
 * Return	0 - SUCCESS
 *		EFAULT - No gloabl data struct.
 *		ENOSPC - no more device left
 *
 ******************************************************************************/
int dlpi_open(queue_t *q, dev_t *devp, int flag, int sflag, cred_t *credp)
{
    atalk_iface *iface;

    if ((iface = kmem_alloc(sizeof *iface, KM_NOSLEEP)) == NULL) {
	cmn_err(CE_WARN, "No memory available for interface structure");
	return ENOMEM;
    }
    bzero((void *)iface, sizeof *iface);

    /* insert interface into ddp global list */
    mutex_enter(&AtalkIfaceMutex);
    iface->next = atalk_iface_list;
    atalk_iface_list = iface;
    mutex_exit(&AtalkIfaceMutex);

    ASSERT(sflag == MODOPEN);	/* STREAMS module above a real network device */
    ASSERT(q->q_ptr == NULL); /* only one instance per stream */

    /* save reference pointer in both read and write queues */
    q->q_ptr = OTHERQ(q)->q_ptr = iface;

    /* save read/write queues to global structure */
    iface->ReadQ = q;
    iface->WriteQ = WR(q);
    iface->dlpi.state = ATALK_DLPI_OPEN;

    qprocson (q);		/* enable put/srv routines */
    
    /* initialize aarp interface lock */
    mutex_init ( &iface->aa.mu_aarp, "aarp mu", MUTEX_DRIVER, NULL );

    /* after all set up, init dlpi device below */
    dlpiInit(q);

    return 0;			/* Succesful */
}
/******************************************************************************

/******************************************************************************
* Description	This function is called when the driver is being I_POP'd or
*		closed.
*
* Return	0 - SUCCESSFUL
*		EBUSY - device is still busy
*
******************************************************************************/

int dlpi_close(queue_t *q)
{
    atalk_iface *iface;
    int device;
    
    dcmn_err((CE_CONT, "DDP:dlpi_close()\n"));
    ASSERT(q->q_ptr);
    
    /* Stop put/srv */
    qprocsoff (q);
    
    iface = (atalk_iface *)q->q_ptr;
    
    /* remove 'iface' from the global interface list */
    mutex_enter(&AtalkIfaceMutex);
    if (iface == atalk_iface_list)
    {
	atalk_iface_list = iface->next;
    }
    else
    {
	atalk_iface *p;
	for (p = atalk_iface_list; p->next; p = p->next)
	{
	    if (iface == p->next)
	    {
		p->next = iface->next;
		break;
	    }
	}
    }
    mutex_exit(&AtalkIfaceMutex);

    /* destroy aarp interface lock */
    mutex_destroy ( &iface->aa.mu_aarp );

    dcmn_err((CE_CONT, "freeing interface %s\n", iface->ifr_name));
    kmem_free(iface, sizeof *iface);
    
    dcmn_err((CE_CONT, "leaving dlpi_close\n"));
    return 0;
}
/*****************************************************************************/

/******************************************************************************
*
* Function	dlpi_rput
*
* Description	As a STREAMS module that sits above the Network Driver
*		this routine handles all DLPI messages coming up
*
* Return	None
*
******************************************************************************/

void dlpi_rput(queue_t *q, mblk_t *mp)
{
    union DL_primitives	*dlp;
    mblk_t		*mdp;
    int			mlen, mtype;
    atalk_iface		*atp;
    TIMER_DATA		*tp;

    atp = (atalk_iface *)q->q_ptr;
    mtype = mp->b_datap->db_type;

    switch (mtype)
    {
    case M_PCPROTO:
    case M_PROTO:
	dlp = (union DL_primitives *)mp->b_rptr;

	if (mp->b_wptr - mp->b_rptr < sizeof(dl_unitdata_ind_t)
	    || dlp->dl_primitive != DL_UNITDATA_IND)
	{
	    /* Handle everything except DL_UNITDATA_IND */
	    dlpiRead(q, mp);
	}
	else
	{
	    /* DL_UNITDATA_IND message, */
	    dlpiRecvUnitdata ( q, mp );
	}
	break;

    case M_PCSIG:
	/* This message type is sent by the timer */
	tp = ( TIMER_DATA *) mp->b_rptr;
	switch ( tp->type )
	{
	case TIMEOUT_AARP_PROBE:
	    aarp_probe_process ( q, AARP_PROBE_TIMEOUT );
	    freemsg ( mp );
	    break;

	default:
	    freemsg ( mp );
	    break;
	}
	break;

    default:
	/* DLPI should only pass up PROTO & PCPROTO */
	cmn_err(CE_WARN, "DDP:dlpi_rput - unknown message <%d>", mtype);
	freemsg(mp);
	break;
    }
}
/*****************************************************************************/

/******************************************************************************
*
* Function	dlpi_wput
*
* Decsription	As a STREAMS module that sits above the Network Driver,
*		convert any incoming requests into dlpi messages and send
*		them down to network device.
*		Should only be ioctl based requests at initialization time
*		coming from atalkd - nobody else.
*
* Return	None
*
******************************************************************************/


void dlpi_wput(queue_t *q, mblk_t *mp)
{
    union DL_primitives	*dlp;
    dl_attach_req_t	*dlar;
    int			mtype, RetCode;

    mtype  = mp->b_datap->db_type;

    switch (mtype) {
    case M_DATA:
    { 
	/* test only case */
	atalk_iface	*atif = ( atalk_iface *) q->q_ptr;
	struct at_addr	sa;
	
	/* dlpiSendData ( atif, mp, atif->testaddr ); */
	sa.s_net = 18;
	sa.s_node = 1;

	aarp_send_ddp ( atif, mp, &sa, (u_char *) 1 );

    }
	break;

    case M_IOCTL:		/* ioctl calls to configure the device */
	dlpiIoctl(q, mp);
	break;

    case M_IOCDATA:		/* transparent ioctl followups */
	dlpiIoctlData  ( q, mp );
	break;

    case M_FLUSH:
	cmn_err(CE_WARN, "DDP:dlpi_wput - flow control not yet implemented");
	freemsg(mp);
	break;

    default:
	cmn_err(CE_WARN, "DDP:dlpi_wput - unknown message type <%d>",mtype);
	freemsg(mp);
	break;
    }
}
/*****************************************************************************/

/******************************************************************************
*
* Function	dlpiBindReq
*
* Description	Issue DL_BIND_REQ message to DLPI provider
*
* Return
*
******************************************************************************/

static int dlpiBindReq(atalk_iface *atp)
{
    mblk_t	*mp;
    dl_bind_req_t	*dlbr;

    if ((mp = dlpiMessage(DL_BIND_REQ, sizeof *dlbr)) == NULL)
	return ENOMEM;

    dlbr = (dl_bind_req_t *)mp->b_rptr;
    ASSERT(dlbr->dl_primitive == DL_BIND_REQ);

    dlbr->dl_sap = atp->dlpi.sap;
    dlbr->dl_max_conind = 0;
    dlbr->dl_service_mode = DL_CLDLS;	/* connectionless data link*/
    dlbr->dl_conn_mgmt = 0;		/* non con-mgmt stream */
    dlbr->dl_xidtest_flg = 0;		/* no auto init. of test and xid */
    dcmn_err((CE_CONT, "dl->sap=%x\n", dlbr->dl_sap ));

    /* Send it to DLPI provider */
    putnext (atp->WriteQ, mp);

    return 0;
}
/*****************************************************************************/

/*****************************************************************************
*
* Function	dlpiMessage
*
* Description	This function is used to allocate a DLPI message block.
*
* Return	mblk_t - Message buffer
*
******************************************************************************/

static mblk_t *dlpiMessage (ulong Primitive, int Size)
{
    mblk_t *pM;
    char	*pC;

    /* Alloc new message */
    if ( (pM = allocb ( Size, BPRI_HI )) )
    {
	/* alloc ok */
	pC = (char *) pM->b_rptr;

	/* Reset buffer */
	bzero ( pC, Size );

	/*    ((dl_bind_req_t *)ALIGN32(pC))->dl_primitive = Primitive; */
	((dl_bind_req_t *)pC)->dl_primitive = Primitive;
	pM->b_wptr = (u_char *) &pC[Size];

	/* Set message type to M_PROTO */
	pM->b_datap->db_type = M_PROTO;
    }
    return pM;
}
/*****************************************************************************/

/*****************************************************************************
*
* Function	dlpiInit
*
* Description	This function is called during device open to initiate
*		DLPI interface.
*
* Return
*
******************************************************************************/

static int dlpiInit(queue_t *q)
{
    atalk_iface *atp;
    queue_t *wq;
    mblk_t *mp;
    char *pInterfaceName;

    atp = (atalk_iface *)q->q_ptr;
    wq = WR(q);

    /* Ensure there is a device linked below and we're ready to be
       initialized */
    ASSERT(WR(q)->q_next);
    ASSERT(atp && atp->dlpi.state == ATALK_DLPI_OPEN);
     
    pInterfaceName = wq->q_next->q_qinfo->qi_minfo->mi_idname;
    if ( strlen(pInterfaceName) < ATNAMSIZ-1 )
    {
	strcpy ( atp->ifr_name, pInterfaceName );
	dcmn_err((CE_CONT, "downstream = (%s)\n", atp->ifr_name ));
    }
    else
	cmn_err(CE_WARN, "Interface name is longer than %d", ATNAMSIZ);

    if ( !(mp = dlpiMessage(DL_INFO_REQ, sizeof(dl_info_req_t))))
    {
	cmn_err (CE_WARN, "dlpiInit() failed");
	return ENOMEM;
    }
  
    /* set get DLPI_INFO state */
    atp->dlpi.state = ATALK_DLPI_WAITINFO;
  
    /* put out DLPI_INFO_REQ */
    putnext (atp->WriteQ, mp);

    /* Wait until message arrived */
    while (atp->dlpi.state == ATALK_DLPI_WAITINFO)
    {
	if (! qwait_sig(q))
	{
	    qprocsoff (q);
	    return EINTR;
	}
    }

    /* Validate that the DLPI_INFO was successful - now ready for attach */
    if (atp->dlpi.state != ATALK_DLPI_ATTACH)
    {
	dcmn_err((CE_CONT,"can't attach because DL_INFO failed\n"));
	return EINVAL;		/* fail the open request */
    }

    return 0;
}
/*****************************************************************************/

/******************************************************************************
*
* Function	dlpiRead
*
* Description	called from dlpi_rput function to handle DLPI message from
*		downstream.
*
* Return	None
*
******************************************************************************/

static void dlpiRead (queue_t *q, mblk_t *mp)
{
    static u_char	atalk_multicast[] = { 9,0,7,0xff,0xff,0xff };
    dl_ok_ack_t		*dloa;
    dl_error_ack_t	*dlea;
    int			sts;
    struct iocblk	*iocbp;

    dloa = (dl_ok_ack_t *) ALIGN32(mp->b_rptr);
    dlea = (dl_error_ack_t *)dloa;

    switch ( dloa->dl_primitive )
    {
    case DL_INFO_ACK:
	dlpiRecvInfo(q, mp);	/* Store DLPI operating information */
	break;

    case DL_BIND_ACK:
	sts = dlpiRecvBindAck(q, mp);

	/* FINALLY, reply with result of the UNITSELECT ioctl */
	ioctlReply(WR(q), mp, sts); 

	/* enable multicast*/
	dlpiEnableMulticast ( ( atalk_iface *) q->q_ptr, atalk_multicast );

	/* since reusing mp, don;t fall through and free it */
	return;			
	
    case DL_OK_ACK:
	/* acknowledgement from DLPI below */
	switch (dloa->dl_correct_primitive)
	{
	case DL_ATTACH_REQ:
	    /* attach successful */
	    dcmn_err((CE_CONT, "DL_ATTACH_REQ: DL_OK_ACK\n"));
	    sts = dlpiBindReq ((atalk_iface *)q->q_ptr);
	    if (sts != 0)
	    {
		/* bind request not even issued */
		ioctlReply(WR(q), mp, sts);

		/* since reusing mp, don;t fall through and free it */
		return;
	    }
	    break;
      
	case DL_UNBIND_REQ:
	    dcmn_err((CE_CONT, "DL_UNBIND_REQ: DL_OK_ACK\n"));
	    break;

	case DL_ENABMULTI_REQ:
	    dcmn_err((CE_CONT, "DL_ENABMULTI_REQ: DL_OK_ACK\n"));
	    break;

	}
	break;
    
    case DL_ERROR_ACK:
	dcmn_err((CE_CONT,"DL_ERRROR_ACK <%d><%d>\n",dlea->dl_errno,
		  dlea->dl_unix_errno));
    
	switch ( dlea->dl_error_primitive)
	{
	case DL_ATTACH_REQ:
	    dcmn_err(( CE_CONT, "DL_ATTACH_REQ\n" ));
	    /* reply failed status to the UNITSELECT ioctl */
	    ioctlReply(WR(q), mp, dlea->dl_unix_errno);
	    return;		/* since reusing mp, don;t fall through and free it */

	case DL_BIND_REQ:
	    dcmn_err((CE_CONT, "DL_BIND_REQ - should release ioctl\n"));
	    ioctlReply(WR(q), mp, dlea->dl_unix_errno);
	    return;		/* since reusing mp, don;t fall through and free it */

	case DL_UNBIND_REQ:
	    dcmn_err((CE_CONT, "DL_UNBIND_REQ\n"));
	    break;

	case DL_DETACH_REQ:
	    dcmn_err((CE_CONT, "DL_DETACH_REQ\n"));
	    break;

	case DL_ENABMULTI_REQ:
	    dcmn_err((CE_CONT, "DL_ENABMULTI_REQ - error\n" ));
	    break;

	default:
	    dcmn_err(( CE_CONT, "UNKNOW operation %d\n",
		      dlea->dl_error_primitive ));
	    break;
	}
	break;

    case DL_UDERROR_IND:
	dcmn_err((CE_CONT, "DL_UDERROR_IND\n"));
	break;
	
    default:
	dcmn_err((CE_CONT, "default: putnext()\n"));
	putnext ( q, mp );
	return;			/* don't free the message */
    }
    dcmn_err(( CE_CONT, "freemsg()\n" ));
    freemsg (mp);		/* free the message */
}
/*****************************************************************************/

/******************************************************************************
*
* Function	dlpiRecvBindAck
*
* Description	The function is called to handle bind acknowlegement
*
* Return
*
******************************************************************************/

static int dlpiRecvBindAck(queue_t *q, mblk_t *mp)
{
    atalk_iface		*atp;
    dl_bind_ack_t		*dlba;
    dl_unitdata_req_t	*dlur;
    int			i;
    char			*p;

    dcmn_err((CE_CONT, "dlpiRecvBindAck\n" ));

    atp = (atalk_iface *)q->q_ptr;
    dlba = (dl_bind_ack_t *) mp->b_rptr;
    dcmn_err((CE_CONT, "dl_sap=%x\n", dlba->dl_sap ));
  
    atp->dlpi.addr_length = dlba->dl_addr_length;
    dcmn_err((CE_CONT, "dl_addr_length=%d dl_addr_offset=%d\n",
	      atp->dlpi.addr_length, dlba->dl_addr_offset ));

    p=(char *)mp->b_rptr;
    p += dlba->dl_addr_offset;

    bcopy ( p, (char *)atp->dlpi.hwaddr, atp->dlpi.addr_length );

    dcmn_err((CE_CONT, "addr: "));
    for (i = 0; i < (int)atp->dlpi.addr_length; i++ )
	dcmn_err((CE_CONT, "%02x ", atp->dlpi.hwaddr[i]));
    dcmn_err((CE_CONT,"\n"));

    /* Create DL_UNITDATA_REQ template */
    dcmn_err((CE_CONT, "Create DL_UNITDATA_REQ template\n"));

    atp->UnitDataReq = dlpiMessage(DL_UNITDATA_REQ, sizeof(dl_unitdata_req_t)
				   + atp->dlpi.addr_length);
    if ( !atp->UnitDataReq ) {
	dcmn_err((CE_CONT, "Problem create template\n" ));
	return ENOMEM;
    }

    dlur = ( dl_unitdata_req_t *)ALIGN32(atp->UnitDataReq->b_rptr);
    dlur->dl_priority.dl_min = 0;
    dlur->dl_priority.dl_max = 0;
    dlur->dl_dest_addr_length = atp->dlpi.addr_length;
    dlur->dl_dest_addr_offset = sizeof *dlur;

    /* store interface's hardware address */
    bcopy ((char *)atp->dlpi.hwaddr, (char *)dlur + dlur->dl_dest_addr_offset,
	   atp->dlpi.addr_length );

    return 0;
}
/*****************************************************************************/

/******************************************************************************
*
* Function	dlpiRecvInfo
*
* Description	The function is called when driver receives dlpi info message
*
* Return	None
*
******************************************************************************/

static void dlpiRecvInfo(queue_t *q, mblk_t *mp)
{
    dl_info_ack_t	*dlia;
    atalk_iface	*atp;

    dlia = (dl_info_ack_t *)ALIGN32(mp->b_rptr);
    atp = (atalk_iface *)q->q_ptr;

    /* obtain lock */
    /* do sanity check */
    if ( dlia->dl_version == DL_VERSION_2 )
    {
	u_char	*p;
	int	i;

	dlpiDisplayInfo ( mp );

	atp->dlpi.service_mode = dlia->dl_service_mode;
	atp->dlpi.addr_length = dlia->dl_addr_length;
	atp->dlpi.sap_length = dlia->dl_sap_length;

	/* store broadcast address */
	atp->dlpi.brdcst_addr_length = dlia->dl_brdcst_addr_length;
	bcopy ( (char *) (mp->b_rptr + dlia->dl_brdcst_addr_offset ),
	       (char *) atp->dlpi.brdcst_addr,
	       atp->dlpi.brdcst_addr_length );

	dcmn_err((CE_CONT,"brdcst_addr_len=%d\n",
		  atp->dlpi.brdcst_addr_length));
	p = atp->dlpi.brdcst_addr;
	dcmn_err((CE_CONT,"dl_brdcst_addr: "));
	for (i = 0; i < atp->dlpi.brdcst_addr_length; ++i)
	    dcmn_err((CE_CONT,"%02x ", *p++ ));
	dcmn_err((CE_CONT,"\n" ));
    
	atp->dlpi.state = ATALK_DLPI_ATTACH;
    }
    else
    {
	atp->dlpi.state = ATALK_DLPI_XV1;
    }

    /* release lock */
}
/*****************************************************************************/

/******************************************************************************
*
* Function	dlpiRecvUnitdata
*
* Description	Receive a data packet from a network interface
*
* Return	None
*
******************************************************************************/

static void dlpiRecvUnitdata(queue_t *q, mblk_t *mp)
{
    atalk_iface		*atp = ( atalk_iface *) q->q_ptr;
    dl_unitdata_ind_t	*dlui = (dl_unitdata_ind_t *) mp->b_rptr;
    mblk_t		*Dp;
    struct llc		*llcp;

    u_char		SourceAddr [20];
    int			i;
    u_char		*p;
    atalk_socket          *s;

    dcmn_err((CE_CONT, "dlpiRecvUnitdata\n"));

    /*
    dcmn_err((CE_CONT, "Dest: " ));
    for (i = 0, p=(u_char *)mp->b_rptr + dlui->dl_dest_addr_offset;
	 i < dlui->dl_dest_addr_length; i++ )
	dcmn_err((CE_CONT, "[%02x] ", *p++ ));
    dcmn_err((CE_CONT, "\n" ));

    dcmn_err((CE_CONT, "Src: " ));
    for ( i=0, p=(u_char *)mp->b_rptr+dlui->dl_src_addr_offset;
	 i < dlui->dl_dest_addr_length; i++ )
    {
	SourceAddr[i] = *p;
	dcmn_err((CE_CONT, "[%02x] ", *p++ ));
    }
    dcmn_err((CE_CONT,"\n"));
    */

    if ( !mp->b_cont )
    {
	dcmn_err((CE_CONT, "mp->b_cont is NULL, no data\n" ));
	freemsg(mp);
	return;
    }

    /* Save message data */
    Dp = mp->b_cont;
    llcp = (struct llc *) Dp->b_rptr;

    /*
    dcmn_err((CE_CONT, "llc_dsap=%02x llc_ssap=%02x\n", llcp->llc_dsap,
	      llcp->llc_ssap ));
    dcmn_err((CE_CONT, "control=%02x org_code[%02x][%02x][%02x]\n",
	      llcp->llc_control, llcp->llc_org_code[0], llcp->llc_org_code[1],
	      llcp->llc_org_code[2] ));
    dcmn_err((CE_CONT, "llc_ether_type=%04x\n", llcp->llc_ether_type ));

    if ( canputnext ( atp->ReadQ ))
    {
	putnext ( atp->ReadQ, mp );
	return;
    }
    */

    /* detach data from original header */
    mp->b_cont=NULL;

    /* Throw the header away */
    freemsg(mp);

    /* Process packet */
    switch ( llcp->llc_ether_type ) {
    case ETHERTYPE_AT:
	dcmn_err((CE_NOTE,"Receive ETHERTYPE_AT packet"));
/*  For initial testing of write side don't try to do read stuff too. */
#if 1
	ddptp_rput(q, Dp);
	return;
#else
	freemsg ( Dp );
	break;
#endif

    case ETHERTYPE_AARP:
	aarp_rput ( q, Dp );
	break;

    default:
	dcmn_err((CE_CONT, "Receive UNKNOWN type packet\n"));
	freemsg( (mblk_t *) Dp );
	break;
    }
}
/*****************************************************************************/

/*****************************************************************************
*
* Function	dlpiSendData
*
* Description	send data out to the network.
*
* Return	None
*
* Note		This function isn't taken care of loop back connection at
*		present.
*
******************************************************************************/

void dlpiSendData( atalk_iface *atp, mblk_t *mp, u_char *EtherAddr)
{
    mblk_t		*mp1;
    dl_unitdata_req_t	*dlur;

    int i, Length;
    u_char	*up;
    char	*p;
    char		*addrp;
  
    p = (char *)mp->b_rptr;
    Length = mp->b_wptr - mp->b_rptr;
    dcmn_err((CE_CONT,"Length=%d\n", Length ));

    /* find a device to send data to *
    if (! (atp->flags & IFF_LOOPBACK))
    {
	* we have to send it back up *
	return;
    }
    */

    mp1 = atp->UnitDataReq;

    /* duplicate DL_UNITDATA_REQ template */
    if (mp1 && (mp1 = copyb(mp1)))
    {
	/* attach data */
	mp1->b_cont = mp;

	bcopy((char *)EtherAddr, (char *)mp1->b_rptr + sizeof *dlur, 6);

	up = (u_char *) mp1->b_rptr;
	up += sizeof(dl_unitdata_req_t );

	dcmn_err((CE_CONT,"Dest:[%02x][%02x][%02x][%02x][%02x][%02x]\n",
		  *up, *(up+1), *(up+2), *(up+3), *(up+4), *(up+5) ));

	/* Send data to DLPI layer */
	if (canputnext(atp->WriteQ))
	    putnext(atp->WriteQ, mp1);
	else
	{
	    cmn_err ( CE_WARN, "DLPI busy, couldn't send\n" );
	    freemsg ( mp1 );
	}
    }
}
/*****************************************************************************/

/******************************************************************************
*
* Function	dlpiSendMdata
*
* Description	The function is designed for testing purposes, the destination
*		ethernet address is stored at atalk_iface.testaddr, which is
*		set during IF_UNITSEL ioctl call.
*		Please note, there is no facility to alter the testaddr once
*		it is set.
*
* Return	None
*
******************************************************************************/

static void dlpiSendMdata (queue_t *q, mblk_t *mp)
{
    atalk_iface		*atp = (atalk_iface *)q->q_ptr;
    mblk_t		*mp1 = atp->UnitDataReq;
    dl_unitdata_req_t	*dlur;
    u_char		*up;
    char	       	*addrp;

    dcmn_err((CE_CONT,"dlpiSendMdata\n" ));
    dcmn_err((CE_CONT,"Length=%d\n", mp->b_wptr - mp->b_rptr ));

    /* duplicate DL_UNITDATA_REQ template */
    if (mp1 && (mp1 = copyb(mp1)))
    {
	mp1->b_cont = mp;

	/* copy dest ether address for global structure*/
	bcopy ( (char *)atp->testaddr,
	       (char *)mp1->b_rptr + sizeof(dl_unitdata_req_t),
	       (atp->dlpi.addr_length+atp->dlpi.sap_length) );

	up = (u_char *) mp1->b_rptr;
	up += sizeof(dl_unitdata_req_t );

	dcmn_err((CE_CONT,"Dest:[%02x][%02x][%02x][%02x][%02x][%02x]\n",
		  *up, *(up+1), *(up+2), *(up+3), *(up+4), *(up+5) ));

	if ( canputnext ( atp->WriteQ) )
	{
	    dcmn_err ((CE_CONT, "putnext(mp1)" ));
	    putnext ( atp->WriteQ, mp1 );
	    return;
	}
    }

    /* Problem with template, or can't duplicate it, so free orig.message */
    dcmn_err((CE_CONT, "problem template, freemsg(mp)"));
    freemsg (mp);
}
/*****************************************************************************/

/******************************************************************************
*
* Function	dlpiIoctl
*
* Description	This function is called to handle ioctl calls.
*
* Return	None
*
* Note		It only handles transparent ioctl command.
*
******************************************************************************/

static void dlpiIoctl(queue_t *q, mblk_t *mp)
{
    atalk_iface *atp;
    struct iocblk	*ioc;
    struct copyreq *cqp;
    struct userArg *ua;
    
    atp = (atalk_iface *)q->q_ptr;
    ioc = (struct iocblk *)mp->b_rptr;
    
    switch (ioc->ioc_cmd)
    {
    case IF_UNITSEL:
	/* specific which device to use */
	ASSERT(ioc->ioc_count == TRANSPARENT);
	ASSERT(mp->b_cont != NULL); /*required to contain addr of user struct*/

	/* must be in the proper state to allow unitsel be attempted */
	if (atp->dlpi.state != ATALK_DLPI_ATTACH)
	{
	    dcmn_err((CE_CONT,"can't attach because DL_INFO failed\n"));
	    ioctlReply(q, mp, EINVAL);
	    break;
	}

	/* need help doing COPYIN/COPYOUT STREAMS crap */
	if ((ua = kmem_alloc(sizeof *ua, KM_NOSLEEP)) == NULL)
	{
	    cmn_err(CE_WARN, "DDP:tpiIoctl - No kernel memory available");
	    ioctlReply(q, mp, ENOMEM);
	    break;
	}

	ua->addr  = (caddr_t)*((long *)mp->b_cont->b_rptr);
	ua->size  = sizeof(DDP_SELECTIF);
	ua->copy  = M_COPYIN;
	transparentCopy(q, mp, ua);
	break;

    default:
	cmn_err(CE_WARN, "DDP:tpiIoctl - Unknown M_IOCTL <%d>", ioc->ioc_cmd);
	ioctlReply(q, mp, EINVAL);
	break;
    }
}

/*****************************************************************************/

/******************************************************************************
*
* Function	dlpiIoctlData
*
* Description	This function handles responses for COPYIN and COPYOUT
*		ioctl data.
*
* Return	None
*
******************************************************************************/

static void dlpiIoctlData(queue_t *q, mblk_t *mp)
{
    struct copyresp	*csp;
    struct userArg	*ua;
    int			sts;

    csp = (struct copyresp *)mp->b_rptr;
    sts = (int)csp->cp_rval;
    ua  = (struct userArg *)csp->cp_private;

    switch (csp->cp_cmd)
    {
    case IF_UNITSEL:
	/* specify which device to use */
	if (sts == 0)
	{
	    /* must be a COPYIN success from STREAM head */
	    struct atalk_iface *ai;
	    DDP_SELECTIF *dp;

	    ASSERT(mp->b_cont->b_rptr != NULL);
	    dp = (DDP_SELECTIF *)mp->b_cont->b_rptr;
	    ai = (struct atalk_iface *)q->q_ptr;
	    sts = unitSelect(ai, dp);
	    if (sts == 0)
	    {
		/* attach request was successfully issued */
		/* I really want this after the BIND_ACK is received but
		   I can't make it work there ??? */
		/* FINALLY, reply with result of the UNITSELECT ioctl */
		ioctlReply(q, mp, 0);
	    }
	}
	else 
	    cmn_err(CE_WARN, "DDP:dlpiIoctl IF_UNITSEL COPY_IN failed");
	break;

    default:
	cmn_err(CE_WARN, "DDP:dlpiIoctlData - unknown cmd <%d>", csp->cp_cmd);
	sts = EINVAL;
	break;
    }

    if (sts != 0)
    {
	/* we're finished because of failure */
	kmem_free(ua, sizeof *ua); /* so won't be needing this any more */
	ioctlReply(q, mp, sts);	/* pass sts back to users ioctl */
    }
}
/*****************************************************************************/

/******************************************************************************
*
*
* Function	unitSeclect
*
* Description	This function is called when the IF_UNITSEL ioctl command
*		is received.
*
* Return	result from dlipAttachReq().
*
******************************************************************************/

static int unitSelect(atalk_iface *atp, DDP_SELECTIF *dp)
{
    int sts;

    atp->dlpi.ppa = dp->ppa;
    atp->dlpi.sap = dp->sap;
    bcopy((char *)&dp->testaddr, (char *)&atp->testaddr, 6);

    dcmn_err((CE_CONT,"ppa=%x sap=%x addln=%d\n",
	      atp->dlpi.ppa,atp->dlpi.sap,6));
    dcmn_err((CE_CONT, "testaddr:[%02x][%02x][%02x][%02x][%02x][%02x]\n",
	      atp->testaddr[0],atp->testaddr[1],
	      atp->testaddr[2],atp->testaddr[3],
	      atp->testaddr[4],atp->testaddr[5] ));


    /* Attach/Bind to device */
    return dlpiAttachReq(atp);
}
/****************************************************************************/

/*****************************************************************************
*
* Function	dlpiAttachReq
*
* Description	issue DL_ATTACH_REQ message to DLPI provider
*
* Return
*
******************************************************************************/

static int dlpiAttachReq(atalk_iface *atp)
{
    mblk_t *mp;
    dl_attach_req_t *dlar;
    char	buffer[20];

    if ((mp = dlpiMessage(DL_ATTACH_REQ, sizeof *dlar)) == NULL)
	return ENOMEM;

    dlar = (dl_attach_req_t *)mp->b_rptr;

    /* set ppa */
    dlar->dl_ppa = atp->dlpi.ppa;
    dcmn_err((CE_CONT, "DL_ATTACH_REQ, ppa=%d\n", dlar->dl_ppa ));

    /* Send it to DLPI provider */
    putnext(atp->WriteQ, mp);

    /* complete the device name, so "le" -> "le0" */
    numtos((unsigned long)atp->dlpi.ppa, buffer);

    if ( (strlen(buffer)+strlen(atp->ifr_name))< ATNAMSIZ-1 )
    {
	strcat ( atp->ifr_name, buffer );
	dcmn_err((CE_CONT,"device <%s>", atp->ifr_name));
    }
    else
	cmn_err(CE_WARN, "device name is longer than %d", ATNAMSIZ-1);

    return 0;
}
/*****************************************************************************/

/******************************************************************************
*
* Function	dlpiEnableMulticast
*
* Description	This function is called to enable the multicast mode, and
*		then joins a specified multicast address.
*		For Netatalk, we just hardcode the multicast address, it
*		should be changed to set by users.
*
* Return	None
*
******************************************************************************/

int	dlpiEnableMulticast (
atalk_iface	*atp,
u_char		*multicast
)
{
    mblk_t		*mp;
    dl_enabmulti_req_t	*demr;

    if ( (mp= dlpiMessage(DL_ENABMULTI_REQ,
			  sizeof(dl_enabmulti_req_t)+ETHERADDRL)) ==NULL )
	return ( ENOMEM );

    demr = ( dl_enabmulti_req_t *) mp->b_rptr;
    demr->dl_addr_length = ETHERADDRL;
    demr->dl_addr_offset = sizeof ( dl_enabmulti_req_t );
    bcopy ( (caddr_t) multicast,
	    (caddr_t) mp->b_rptr+sizeof(dl_enabmulti_req_t),
	    ETHERADDRL );

    /* Send down to DLPI provider */
    putnext ( atp->WriteQ, mp );

    return (0);
}
/*****************************************************************************/

/*****************************************************************************
 * Description	This function is written for part of initial testing. It
 *		logs the DLPI_INFO structure.
 *
 * Return	None
 *
 *****************************************************************************/

static void dlpiDisplayInfo(mblk_t *mp)
{
    dl_info_ack_t	*dlia = ( dl_info_ack_t *)ALIGN32(mp->b_rptr);
    u_char		*p;
    int			i;

    dcmn_err((CE_CONT, "DL_INFO_ACK\n"));

    dcmn_err((CE_CONT, "dl_max_sdu=%d dl_min_sdu=%d\n",
	      dlia->dl_max_sdu, dlia->dl_min_sdu));
    dcmn_err((CE_CONT, "dl_addr_length=%d\n", dlia->dl_addr_length ));
    dcmn_err((CE_CONT, "dl_mac_type=%d\n", dlia->dl_mac_type));
    dcmn_err((CE_CONT, "dl_current_state=%d\n", dlia->dl_current_state));
    dcmn_err((CE_CONT, "dl_sap_length=%d\n", dlia->dl_sap_length));
    dcmn_err((CE_CONT, "dl_service_mode=%d\n", dlia->dl_service_mode));
    dcmn_err((CE_CONT, "dl_provider_style=%d\n", dlia->dl_provider_style));
    dcmn_err((CE_CONT, "dl_addr_offset=%d\n", dlia->dl_addr_offset));
    dcmn_err((CE_CONT, "dl_version=%d\n", dlia->dl_version));
}
/*****************************************************************************/







