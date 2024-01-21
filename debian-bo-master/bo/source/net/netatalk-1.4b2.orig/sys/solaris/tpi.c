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

#include <sys/types.h>
#include <sys/stream.h>
#include <sys/stropts.h>
#include <sys/tihdr.h>
#include <sys/tiuser.h>
#include <sys/cmn_err.h>
#include <sys/socket.h>
#include <sys/debug.h>
#include <sys/kmem.h>
#include <sys/errno.h>
#include <sys/sockio.h>
#include <sys/byteorder.h>
#include <sys/timod.h>

#include <net/if.h>
#include <net/route.h>
#include <inet/common.h>

#include "at.h"
#include "ddp.h"
#include "phase2.h"
#include "aarp.h"

#include <sys/ddi.h>		/* ddi's need to be included last */
#include <sys/sunddi.h>

/* Local Prototypes */
static void tpiInfo(queue_t *q, mblk_t *mp);
static void tpiBind(queue_t *q, mblk_t *mp);
static void tpiUnbind(queue_t *q, mblk_t *mp);
static void tpiOptMgt(queue_t *q, mblk_t *mp);
static void tpiFlush(queue_t *q, mblk_t *mp);
static void tpiSend (queue_t *q, mblk_t *mp);
static void tpiIoctl(queue_t *q, mblk_t *mp);
static void tpiIoctlData(queue_t *q, mblk_t *mp);

static mblk_t *tpiAckAlloc(mblk_t *mp, int size, int tpiType);
static atalk_iface *findInterface(int net, int node);
static struct atalk_route *findRoute(struct at_addr *target);
static u_short checkSum(mblk_t *mblk, int skip);
static int setIfreqAddr(queue_t *q, mblk_t *mp, struct ifreq *ifr);
static int getIfreqAddr(struct ifreq *ifr);
static void setDefaultRoute(atalk_iface *dev);
static int createRoute(struct rtentry *r, struct atalk_iface *devhint);
static atalk_socket *findSocket(struct sockaddr_at *sat);

static int	matchSocket	( struct sockaddr_at	*s1,
				  struct sockaddr_at	*s2 );
static int	doSocketmatch	( atalk_socket		*s,
				  struct sockaddr_at	*sat );

static int getInterfaces(struct ifreq *ifrs, int size);

int tpi_open (queue_t *q, dev_t *dev, int flag, int sflag, cred_t *credp)
{
  atalk_socket *s;
  minor_t i;
  dcmn_err((CE_CONT, "ddp: tpi_open()\n"));

  ASSERT(sflag == CLONEOPEN);	/* As a driver, we are ALWAYS a clone device */
  ASSERT(q->q_ptr == NULL);	/* clone devices shouldn't get opened more than once */

  mutex_enter(&AtalkSocketMutex);
  for (i = 0; i < ddp_ndevs; ++i) {
      if (ddp_mindevs[i] == 0)		/* free minor device */
	  break;
  }

  mutex_exit(&AtalkSocketMutex);

  if (i == ddp_ndevs) {			/* must find a unique number */
      cmn_err(CE_WARN, "Out of ddp minor devices\n");
      return -1;
  }

  *dev = makedevice(getmajor(*dev), i);

  s = (atalk_socket *)kmem_alloc(sizeof *s, KM_NOSLEEP);
  if (s == NULL) {
    cmn_err(CE_WARN, "No memory available");
    return -1;
  }
  bzero((void *)s, sizeof *s);

  /* Add 's' to the global list of socket structures */
  mutex_enter(&AtalkSocketMutex);
  s->next = atalk_socket_list;
  atalk_socket_list = s;
  mutex_exit(&AtalkSocketMutex);

  strcpy(s->name, "socket------");
  numtos(i, s->name+7);
  dcmn_err((CE_CONT, "adding <%s> %x to socket list\n", s->name,s ));

  s->minor = i;
  s->broadcast = 0;
  s->zapped = 1;
  s->state = TS_UNBND;		/* TCP_CLOSE */
  
  s->mtu = DDP_MAXSZ;
  s->at.src_net  = 0;
  s->at.src_node = 0;
  s->at.src_port = 0;
  s->at.dst_net  = 0;
  s->at.dst_node = 0;
  s->at.dst_port = 0;
  s->rdq = q;
  s->wrq = WR(q);

  q->q_ptr = WR(q)->q_ptr = s;
  
  ddp_mindevs[i] = 1;			/* mark as in use */

  /* enable put/srv routines */
  qprocson(q);

  dcmn_err((CE_CONT,"leaving tpiopen\n"));
  return 0;
}

int tpi_close (queue_t *q)
{
  atalk_socket *s;
  dcmn_err((CE_CONT, "tpi_close()\n"));
  ASSERT(q->q_ptr);

  /* Test */
  for ( s=(atalk_socket *)q->q_ptr; s ; s=s->next )
      dcmn_err((CE_CONT, "%x (%s)\n", s, s->name ));

  qprocsoff (q);
  s = (atalk_socket *)q->q_ptr;
  q->q_ptr = NULL;

  ddp_mindevs[s->minor] = 0;		/* mark as free */

  /* remove 's' from the global socket list */
  mutex_enter(&AtalkSocketMutex);
  if (s == atalk_socket_list) {
    atalk_socket_list = s->next;
  } else {
    atalk_socket *p;
    for (p = atalk_socket_list; p->next; p = p->next) {
      if (s == p->next) {
	p->next = s->next;
	break;
      }
    }
  }
  mutex_exit(&AtalkSocketMutex);
  
  dcmn_err((CE_CONT, "kmem_free socket %s <%x>\n", s->name, s ));
  kmem_free(s, sizeof *s);
  return 0;
}

/* No STREAMS messages should ever flow through this code path because there is no-one below us */
void tpi_rput (queue_t *q, mblk_t *mp)
{
  cmn_err(CE_WARN,"Nobody messages should be sent to tpi_rput ");
  dcmn_err((CE_CONT, "freemsg(mp)\n"));
  freemsg(mp);
}

/* Receive a message from above */
void tpi_wput (queue_t *q, mblk_t *mp)
{
  atalk_socket *s;
  struct datab *db;
  union T_primitives *tprim;

  dcmn_err((CE_CONT, "Entering tpi_wput routine\n"));
  ASSERT(q->q_ptr);

  s = (atalk_socket *)q->q_ptr;
  db = mp->b_datap;
  dcmn_err((CE_CONT, "received %d\n", db->db_type));

  switch(db->db_type) {
  case M_PCPROTO:
    dcmn_err((CE_CONT,"M_PCPROTO\n"));
    tprim = (union T_primitives *)ALIGN32(mp->b_rptr);
    switch(tprim->type) {
    case T_INFO_REQ:
      tpiInfo(q, mp);
      break;
    default:
      cmn_err(CE_WARN, "unknown M_PCPROTO type %d", tprim->type);
      break;
    }
    break;
	
  case M_PROTO:
    dcmn_err((CE_CONT,"M_PROTO\n"));
    tprim = (union T_primitives *)ALIGN32(mp->b_rptr);

    switch(tprim->type){
    case T_BIND_REQ:
      tpiBind(q, mp);
      break;
    case T_UNBIND_REQ:
      tpiUnbind(q, mp);
      break;
    case T_OPTMGMT_REQ:
      tpiOptMgt(q, mp);
      break;
    case T_UNITDATA_REQ:
      tpiSend(q, mp);
      break;
    default:
      dcmn_err((CE_CONT, "unknown M_PROTO type %d\n", tprim->type));
      break;
    }
    break;

  case M_FLUSH:
    dcmn_err((CE_CONT,"M_FLUSH\n"));
    tpiFlush(q, mp);
    break;

  case M_IOCTL:			/* ioctl calls */
    dcmn_err((CE_CONT,"M_IOCTL\n"));
    tpiIoctl(q, mp);
    break;

  case M_IOCDATA:		/* transparent ioctl followups */
    dcmn_err((CE_CONT,"M_IOCTLDATA\n"));
    tpiIoctlData(q, mp);
    break;

  default:			/* drivers toss unrecognized messages */
    cmn_err(CE_WARN, "unknown msg received %d", db->db_type);
    dcmn_err((CE_CONT, "freemsg(mp)\n"));
    freemsg(mp);
    break;
  }
}

static void tpiInfo(queue_t *q, mblk_t *mp)
{
  struct T_info_req *req;
  static struct T_info_ack ack = {
    T_INFO_ACK,
    DDP_MAXSZ,		/*TSDU_size : ??? max appletalk packet size */
    -2,			/*ETSU_size:ddp does not support expedited data. */
    -2,			/*CDATA_size: ddp does not support connect data. */
    -2,			/*DDATA_size:ddp doesn't support disconnect data.*/
    sizeof(struct sockaddr_at), /* ADDR_size : */
    64,			/* OPT_size  : ??? */
    1024,		/* TIDU_size : ??? */
    T_CLTS,		/* SERV_type : ddp supports connection-less. */
    TS_UNBND,		/* CURRENT_state:  This is set from ddp_state. */
    0			/* PROVIDER_flag */
  };
  dcmn_err((CE_CONT, "T_INFO_REQ\n"));
  req = (struct T_info_req *)mp->b_rptr;
  ASSERT(req->PRIM_type == T_INFO_REQ);
    
  /* Create a T_INFO_ACK message. */
  bcopy((char *)&ack, (char *)mp->b_rptr, sizeof ack);
  mp->b_wptr = mp->b_rptr + sizeof ack;
  qreply(q, mp);
}

/* Select an unused port number */
static int selectPort(struct sockaddr_at *sa)
{
    dcmn_err((CE_CONT, "selectPort\n"));
    for(sa->sat_port=ATPORT_RESERVED; sa->sat_port<ATPORT_LAST; ++sa->sat_port)
    {
	if (findSocket(sa) == NULL )
	    return sa->sat_port;
    }

    return EBUSY;
}

static int autobind(atalk_socket *s)
{
  struct at_addr *ap = findPrimary();
  struct sockaddr_at sa;
  int n;

  dcmn_err((CE_CONT, "autobind\n"));
  if (ap == NULL || ap->s_net == htons(ATADDR_ANYNET))
  {
      dcmn_err((CE_CONT, "fail to find primary ap=%x\n", ap ));
      return EADDRNOTAVAIL;
  }
  s->at.src_net = sa.sat_addr.s_net = ap->s_net;
  s->at.src_node = sa.sat_addr.s_node = ap->s_node;

  if ((n = selectPort(&sa)) < 0)
    return n;

  s->at.src_port = sa.sat_port = n;
  s->zapped = 0;

  return 0;
}

/* Set the address 'our end' of the connection */
static void tpiBind(queue_t *q, mblk_t *mp)
{
  atalk_socket *s;
  struct sockaddr_at *sa, tmp;
  struct T_bind_req *req;
  struct T_bind_ack *ack;
  struct T_error_ack *nak;
  int sts, tpists;

  dcmn_err((CE_CONT, "received T_BIND_REQ\n"));
  ASSERT(q->q_ptr);

  s = (atalk_socket *)q->q_ptr;
  ASSERT(s->state == TS_UNBND);

  req = (struct T_bind_req *)ALIGN32(mp->b_rptr);
  ASSERT(req->PRIM_type == T_BIND_REQ);

  /* send by sockmod for autobinding */
  if (req->ADDR_length != sizeof *sa) {
      if ( autobind ( s ) < 0 ) {
	  dcmn_err ((CE_CONT," autobind fail\n"));
	  goto fail;
      }
      tmp.sat_family = AF_APPLETALK;
      tmp.sat_addr.s_net = s->at.src_net;
      tmp.sat_addr.s_node = s->at.src_node;
      tmp.sat_port = s->at.src_port;
      sa = &tmp;
      goto bind_done;
  }

  sa = (struct sockaddr_at *)(mp->b_rptr + req->ADDR_offset);
  ASSERT(sa->sat_family == AF_APPLETALK);
  tmp = *sa;

  if (tmp.sat_addr.s_net == htons(ATADDR_ANYNET)) {
    struct at_addr *ap;

    if ((ap = findPrimary()) == NULL) { /* find an interface address */
      sts = EADDRNOTAVAIL;
      tpists = TNOADDR;
      goto fail;
    }
    dcmn_err((CE_CONT,"ap->s_net=%d ap->s_node=%d\n", ap->s_net, ap->s_node ));
    tmp.sat_addr.s_net = ap->s_net;
    tmp.sat_addr.s_node = ap->s_node;

  } else {			/* Find interface to handle this address */
    if (findInterface(tmp.sat_addr.s_net, tmp.sat_addr.s_node) == NULL) {
      sts = EADDRNOTAVAIL;
      tpists = TNOADDR;
      goto fail; 
    }
  }

  /* Assign a port number to this address */
  if (tmp.sat_port == ATADDR_ANYPORT) {
    int n; 
    if ((n = selectPort(&tmp)) < 0) {
      sts = n;
      tpists = TNOADDR;
      goto fail;
    }
  }

  /* ensure that this address is not already in use */
  if (findSocket(&tmp) != NULL) {
    sts = EADDRINUSE;
    tpists = TNOADDR; /* TADDRBUSY */
    goto fail;
  }
  s->at.src_net  = tmp.sat_addr.s_net;
  s->at.src_node = tmp.sat_addr.s_node;
  s->at.src_port = tmp.sat_port;

  s->zapped = 0;
  s->state = TS_IDLE;

bind_done:
  dcmn_err((CE_CONT,"Acknowledge successful bind net=%d node=%d port=%d\n",
	    s->at.src_net, s->at.src_node, s->at.src_port));

  mp->b_datap->db_type = M_PCPROTO;
  ack = (struct T_bind_ack *)mp->b_rptr;
  ack->PRIM_type   = T_BIND_ACK;
  ack->ADDR_length = sizeof *sa;
  ack->ADDR_offset = sizeof *ack;
  ack->CONIND_number = 0;

  mp->b_wptr = mp->b_rptr + sizeof *ack;
  bcopy((void *)sa, (void *)mp->b_wptr, sizeof *sa);
  mp->b_wptr += sizeof *sa;

  qreply(q, mp);
  return;

fail:	/* nacknowledge that the attempted bind failed */
  dcmn_err((CE_CONT, "Bind Failed\n"));
  /* build a T_error_ack failure message packet */
  mp->b_datap->db_type = M_PCPROTO;
  nak = (struct T_error_ack *)mp->b_rptr;
  nak->PRIM_type   = T_ERROR_ACK;
  nak->ERROR_prim  = T_BIND_REQ;
  nak->TLI_error   = tpists;
  nak->UNIX_error  = sts;
  mp->b_wptr = mp->b_rptr + sizeof *nak;
  qreply(q, mp);		/* qreply with a bind failed message */
}

/* Clear the address of 'our end' of the connection */
static void tpiUnbind(queue_t *q, mblk_t *mp)
{
  static struct T_ok_ack ack = {
    T_OK_ACK,	/* PRIM_type : */
    0		/* CORRECT_prim: what we're ack'ing */
  };
  struct T_unbind_req *req;
  atalk_socket *s;

  dcmn_err((CE_CONT, "received T_UNBIND_REQ\n"));

  ASSERT(q != NULL && q->q_ptr != NULL);
  s = (atalk_socket *)q->q_ptr;

  req = (struct T_unbind_req *)ALIGN32(mp->b_rptr);
  ASSERT(req->PRIM_type == T_UNBIND_REQ);
    
  /* Create a T_OK_ACK message. */
  ack.CORRECT_prim = req->PRIM_type;
  bcopy((char *)&ack, (char *)mp->b_rptr, sizeof ack);	      
  qreply(q, mp);

  s->at.src_net  = 0;
  s->at.src_node = 0;
  s->at.src_port = 0;
  s->at.dst_net  = 0;
  s->at.dst_node = 0;
  s->at.dst_port = 0;
  s->zapped      = 1;
  s->state       = TS_UNBND;		/* TCP_CLOSE */
}


/* Really don't know what this is for yet, until then, its a stub */
static void tpiOptMgt(queue_t *q, mblk_t *mp)
{
  struct T_optmgmt_req *req;
  
  dcmn_err((CE_CONT, "received T_OPTMGT_REQ\n"));
  req = (struct T_optmgmt_req *)ALIGN32(mp->b_rptr);
  ASSERT(req->PRIM_type == T_OPTMGMT_REQ);
}


/* Generic stream flushing */
static void tpiFlush(queue_t *q, mblk_t *mp)
{
  if (*mp->b_rptr & FLUSHW) {
    flushq(q, FLUSHDATA);
    *mp->b_rptr &= ~ FLUSHW;
  }
  if (*mp->b_rptr & FLUSHR) {
    flushq(RD(q), FLUSHDATA);
    qreply(q, mp);
  } else {
    dcmn_err((CE_CONT, "freemsg(mp)\n"));
    freemsg(mp);
  }
}


/* Send the datagram to the destination - requires no acknowledgement */
static void tpiSend (queue_t *q, mblk_t *mp)
{
  static u_char	at_org_code [] = { 8, 0, 7 };
  struct T_unitdata_req *req;
  struct sockaddr_at *sa;
  struct ddpehdr *hdr;
  struct atalk_route *rt;
  atalk_socket *s;
  atalk_iface *dev;
  int i, loopback = 0;
  struct llc	*llcp;
  mblk_t	*new;
  int		data_size;

  dcmn_err((CE_CONT, "received T_UNITDATA_REQ\n"));

  ASSERT(q != NULL && q->q_ptr != NULL);
  s = (atalk_socket *)q->q_ptr;

  req = (struct T_unitdata_req *)ALIGN32(mp->b_rptr);
  ASSERT(req->PRIM_type == T_UNITDATA_REQ);

  ASSERT( mp->b_cont != NULL );
  data_size = mp->b_cont->b_wptr - mp->b_cont->b_rptr;

  dcmn_err((CE_CONT,"DEST_length=%d\n", req->DEST_length));
  dcmn_err((CE_CONT,"DEST_offset=%d\n", req->DEST_offset));
  dcmn_err((CE_CONT,"OPT_length=%d\n",  req->OPT_length));
  dcmn_err((CE_CONT,"OPT_offset=%d\n",  req->OPT_offset));
  dcmn_err((CE_CONT,"data length=%d\n", data_size ));

  if (req->DEST_length == sizeof *sa) {
    if (s->zapped)
      if ( autobind(s) <0 )
	return;			/* Can't bind so fail */

    ASSERT(req->DEST_offset != 0);

    /* Real data stored in M_DATA block */
    sa = (struct sockaddr_at *)(mp->b_rptr + req->DEST_offset);
    dcmn_err((CE_CONT,"family=%d net=%d node=%d port=%d\n", sa->sat_family,
	   sa->sat_addr.s_net, sa->sat_addr.s_node, sa->sat_port));

    ASSERT(sa->sat_family == AF_APPLETALK);

  } else {			/* not address provided */
    static struct sockaddr_at default_sa;
    sa = &default_sa;
    sa->sat_family = AF_APPLETALK;
    sa->sat_port   = s->at.dst_port;
    sa->sat_addr.s_node = s->at.dst_node;
    sa->sat_addr.s_net  = s->at.dst_net;
  }

  if (sa->sat_addr.s_net != 0 || sa->sat_addr.s_node == ATADDR_ANYNODE) {
    rt = findRoute(&sa->sat_addr);
  } else {
    struct at_addr hint_sa;
    hint_sa.s_node = 0;
    hint_sa.s_net  = s->at.src_net;
    rt = findRoute(&hint_sa);
  }
  if (rt == NULL){
    dcmn_err((CE_CONT, "no route\n"));
    return;			/* ENETUNREACH */
  }
  dev = rt->dev;

  /* allocate link logic control block */
  if ( ( new = allocb ( sizeof(struct llc), BPRI_HI))
       == NULL )
  {
      cmn_err ( CE_WARN, "allocb() fail\n" );
      freemsg ( mp );
      return;
  }
  new->b_wptr = new->b_rptr + sizeof(struct llc);

  /* assign llc pointer */
  llcp = ( struct llc *) new->b_rptr;
  llcp->llc_dsap= llcp->llc_ssap = LLC_SNAP_LSAP;
  llcp->llc_control = LLC_UI;
  bcopy ( (caddr_t) at_org_code, (caddr_t) llcp->llc_org_code,
	 sizeof( at_org_code ) );
  llcp->llc_ether_type = htons ( ETHERTYPE_AT );

  /* Attach original data to new message block */
  new->b_cont = mp;

  /* prepend ddpehdr - reuse mp from above */
  mp->b_datap->db_type = M_DATA;
  mp->b_rptr = mp->b_datap->db_base;
  mp->b_wptr = mp->b_rptr + sizeof(*hdr);

  /* initialize the ddpehdr */
  hdr = (struct ddpehdr *) (mp->b_rptr);
  hdr->deh_pad   = 0;
  hdr->deh_hops  = 0;
  hdr->deh_dnet	 = sa->sat_addr.s_net;
  hdr->deh_dnode = sa->sat_addr.s_node;
  hdr->deh_dport = sa->sat_port;
  hdr->deh_snet	 = s->at.src_net;
  hdr->deh_snode = s->at.src_node;
  hdr->deh_sport = s->at.src_port;

  hdr->deh_sum	= checkSum(mp, 4); /* skip first 4 bytes of bit stuff */
  /* hdr->deh_len   = sizeof *sa + sizeof *hdr;*/
  hdr->deh_len   =  sizeof *hdr + data_size;
  
  /* gross hack to get length into machine independent form
   * without using unions of bit fields
   */
  *(unsigned short *)&hdr = ntohs(*(unsigned short *)&hdr);

  /* special case */
  if ( (hdr->deh_dnode==0) && (hdr->deh_dnet==0) )
  {
      mblk_t *loopmsg;
      loopmsg = copymsg(new);
      ddptp_rput (NULL, loopmsg);	/* loopback has no queues */
      dcmn_err((CE_CONT, "special case loop back send\n"));
      return;
  }

#if 0
  /* Loopback broadcast packets to non gateway targets
     (ie routes to group we are in) */
  if (hdr->deh_dnode == ATADDR_BCAST) {
      if ((! (rt->flags & RTF_GATEWAY)) && (! (dev->flags & IFF_LOOPBACK))) {
	mblk_t *loopmsg;
	  dcmn_err((CE_CONT,"broadcast packets to non-gateway targets\n"));
	  /* loopback = 1; */
	  loopmsg = copymsg(new);
	  aarp_send_ddp(dev, loopmsg, &sa->sat_addr, (u_char *) 1);
      }
  }
#endif

  if ((dev->flags & IFF_LOOPBACK) || loopback) {
    dcmn_err((CE_CONT, "Doing loopback\n"));
    ddptp_rput (NULL, new);	/* loopback has no queues */

  } else {
      dcmn_err((CE_CONT, "Doing output\n"));
      if (rt->flags & RTF_GATEWAY) {
	  static struct sockaddr_at gsat;
	  gsat.sat_addr = rt->gateway;
	  sa = &gsat;
      }
      aarp_send_ddp(dev, new, &sa->sat_addr, (u_char *) 1 );
  }
  dcmn_err((CE_CONT, "Data send completed\n"));
}

/* Transparent ioctl command processing */
static void tpiIoctl(queue_t *q, mblk_t *mp)
{
  struct iocblk	*iocp;
  struct copyreq *cqp;
  struct userArg *ua;

  dcmn_err((CE_CONT, "tpiIoctl\n"));
  iocp = (struct iocblk *)mp->b_rptr;

  switch (iocp->ioc_cmd) {
  case SIOCADDRT:		/* add a new route */
  case SIOCDELRT:		/* delete an existing route */
    ASSERT(iocp->ioc_count == TRANSPARENT);
    ASSERT(mp->b_cont != NULL);	/* required to contain addr of user struct */

    /* need help doing COPYIN/COPYOUT STREAMS crap */
    if ((ua = kmem_alloc(sizeof *ua, KM_NOSLEEP)) == NULL) {
      cmn_err(CE_WARN, "DDP:tpiIoctl - No kernel memory available");
      ioctlReply(q, mp, ENOMEM);
    }
    ua->addr  = (caddr_t)*((long *)mp->b_cont->b_rptr);
    ua->size  = sizeof(struct rtentry);
    ua->copy  = M_COPYIN;
    transparentCopy(q, mp, ua);
    break;

    /* all of these start same way - copying in user's request buffer */
  case SIOCSIFADDR:		/* set the named interface address */
  case SIOCGIFADDR:		/* get the named interface address */
  case SIOCGIFFLAGS:		/* get the named interface flags */
  case SIOCSIFFLAGS:		/* set the named interface flags */
  case SIOCADDMULTI:		/* enable multicasting */
    ASSERT(iocp->ioc_count == TRANSPARENT);
    ASSERT(mp->b_cont != NULL);	/* required to contain addr of user struct */

    /* need help doing COPYIN/COPYOUT STREAMS crap */
    if ((ua = kmem_alloc(sizeof *ua, KM_NOSLEEP)) == NULL) {
      cmn_err(CE_WARN, "DDP:tpiIoctl - No kernel memory available");
      ioctlReply(q, mp, ENOMEM);
    }
    ua->addr  = (caddr_t)*((long *)mp->b_cont->b_rptr);
    ua->size  = sizeof(struct ifreq);
    ua->copy  = M_COPYIN;
    transparentCopy(q, mp, ua);
    break;

  case SIOCGIFCONF:
    ASSERT(iocp->ioc_count == TRANSPARENT);
    ASSERT(mp->b_cont != NULL);	/* required to contain addr of user struct */

  case TI_GETMYNAME:		/* get socket name */
      dcmn_err((CE_CONT,"received TI_GETMYNAME request\n"));
      ASSERT(iocp->ioc_count == TRANSPARENT);
      ASSERT(mp->b_cont != NULL); /* required to contain addr of user struct */

      /* need help doing COPYIN/COPYOUT STREAMS crap */
      if ((ua = kmem_alloc(sizeof *ua, KM_NOSLEEP)) == NULL) {
	  cmn_err(CE_WARN, "DDP:tpiIoctl - No kernel memory available");
	  ioctlReply(q, mp, ENOMEM);
      }
      ua->addr  = (caddr_t)*((long *)mp->b_cont->b_rptr);
      ua->size  = sizeof(struct netbuf);
      ua->copy  = M_COPYIN;
      transparentCopy(q, mp, ua);
      break;

  case TI_GETPEERNAME:
      dcmn_err((CE_CONT,"received TI_GETPEERNAME request\n"));
      break;

  default:
    cmn_err(CE_WARN, "DDP:tpiIoctl - Unknown M_IOCTL <%d>", iocp->ioc_cmd);
    ioctlReply(q, mp, EINVAL);
    break;
  }
}

/* Delete a route - Find it and discard it */
static int deleteRoute(struct rtentry *rt)
{
  struct atalk_route **r;
  struct atalk_route *tmp;
  struct at_addr *addr = (struct at_addr *)&rt->rt_dst;

  mutex_enter(&AtalkRouteMutex);
  r = &atalk_router_list;
  while ((tmp = *r) != NULL) {
    if (tmp->target.s_net == addr->s_net
	&& (! (tmp->flags & RTF_HOST)
	    || tmp->target.s_node == addr->s_node)) {
      *r = tmp->next;
      kmem_free(tmp, sizeof *tmp);
      break;
    }
    r = &tmp->next;
  }
  mutex_exit(&AtalkRouteMutex);

  if (tmp != NULL)		/* found the route and deleted it */
    return 0;
  else
    return ENOENT;		/* couldn't find it */
}

/* report flags of named interface */
static int getIfreqFlags(struct ifreq *ifr)
{
    atalk_iface *iface;
    struct sockaddr_at *sat;

    dcmn_err((CE_CONT,"!entering getIfreqAddr for <%s>\n", ifr->ifr_name));

    /* find the interface */
    iface = findNamedInterface(ifr->ifr_name);
    if (iface == NULL)
	return ENODEV;

    /* populate the ifreq structure */
    ifr->ifr_flags = iface->flags;

    return 0;
}

/* configure a named interface */
static int setIfreqFlags(struct ifreq *ifr)
{
    atalk_iface *iface;

    /* find the interface */
    iface = findNamedInterface(ifr->ifr_name);
    if (iface == NULL)
	return ENODEV;

    /* populate the ifreq structure */
    iface->flags = ifr->ifr_flags;
    return 0;
}

/* Response handler for COPY_IN and COPY_OUT STREAMS requests */
static void tpiIoctlData(queue_t *q, mblk_t *mp)
{
    struct copyresp *csp;
    struct ifreq *ifr;
    struct userArg *ua;
    int sts, done = 0;
    struct sockaddr *sa;

    dcmn_err((CE_CONT, "tpiIoctlData\n"));
    csp = (struct copyresp *)mp->b_rptr;
    sts = (int)csp->cp_rval;
    ua  = (struct userArg *)csp->cp_private;

    switch (csp->cp_cmd) {

    case SIOCADDRT:		/* add a new route */
	if (sts == 0) {		/* must be a COPYIN success from STREAM head */
	    struct rtentry *rt;
	    ASSERT(mp->b_cont->b_rptr != NULL);
	    rt = (struct rtentry *)mp->b_cont->b_rptr;
	    if ((sts = createRoute(rt, NULL)) == 0) {
		done = 1;
	    }
	} else {
	    cmn_err(CE_WARN,"DDP:tpiIoctl SIOCADDRT on <%s> COPYIN failed",
		    ifr->ifr_name);
	}
	break;

    case SIOCDELRT:		/* delete a route */
	if (sts == 0) {		/* must be a COPYIN success from STREAM head */
	    struct rtentry *rt;
	    ASSERT(mp->b_cont->b_rptr != NULL);
	    rt = (struct rtentry *)mp->b_cont->b_rptr;
	    if ((sts = deleteRoute(rt)) == 0) {
		done = 1;
	    }
	} else {
	    cmn_err(CE_WARN,"DDP:tpiIoctl SIOCDELRT on <%s> COPYIN failed",
		    ifr->ifr_name);
	}
	break;

    case SIOCADDMULTI:		/* enable multicasting */
	if (sts == 0) {		/* must be a COPYIN success from STREAM head */
	    ASSERT(mp->b_cont->b_rptr != NULL);
	    ifr = (struct ifreq *)mp->b_cont->b_rptr;
	    if ((sts = addMulticast(ifr)) == 0) {
		done = 1;
		/* request has been sent - DLPI will acknowledge the users
		   request when completed */
	    } else {
		cmn_err(CE_WARN,"DDP:tpiIoctlData - Can't add multicast <%s>",
			ifr->ifr_name);
	    }
	} else {
	    cmn_err(CE_WARN,"DDP:tpiIoctl SIOCADDMULTI on <%s> COPY_IN failed",
		    ifr->ifr_name);
	}
	break;

    case SIOCSIFADDR:		/* set the named ifreq address */
	if (sts == 0) {		/* must be a COPYIN success from STREAM head */
	    ASSERT(mp->b_cont->b_rptr != NULL);
	    ifr = (struct ifreq *)mp->b_cont->b_rptr;

	    /* we can't return ok, until aarp_probe done */
	    sts = setIfreqAddr( q, mp, ifr);
	    if ( sts < 0 )
	    {
		/* probing */
		sts = 0;
		kmem_free ( ua, sizeof *ua );
	    }
	    else if ( sts == 0 )
		/* loop back */
		done = 1;
	    else
		cmn_err(CE_WARN,"DDP:tpiIoctlData - Can't SetAddr for <%s>",
			ifr->ifr_name);
	} else {
	    cmn_err(CE_WARN,"DDP:tpiIoctl SIOCSIFADDR on <%s> COPY_IN failed",
		    ifr->ifr_name);
	}
	break;
	
    case SIOCSIFFLAGS:		/* set the named ifreq flags */
	if (sts == 0) {		/* must be a COPYIN success from STREAM head */
	    ASSERT(mp->b_cont->b_rptr != NULL);
	    ifr = (struct ifreq *)mp->b_cont->b_rptr;
	    if ((sts = setIfreqFlags(ifr)) == 0)
		done = 1;		/* transparent copy is done AOK */
	    else
		cmn_err(CE_WARN,"DDP:tpiIoctlData:Can't setIfreqFlags <%s>",
			ifr->ifr_name);
	} else {
	    cmn_err(CE_WARN,"DDP:tpiIoctl SIOCSIFFLAGS on <%s> COPYIN failed",
		    ifr->ifr_name);
	}
	break;
	
    case SIOCGIFADDR:		/* get the named ifreq address */
	if (sts == 0) {		/* [copyin | copyout] was aok at STREAM head */
	    if (ua->copy == M_COPYIN) { /* its the copyin acknowledgement */
		ASSERT(mp->b_cont->b_rptr != NULL); /* this is users data */
		ifr = (struct ifreq *)mp->b_cont->b_rptr;
		if ((sts = getIfreqAddr(ifr)) == 0) {
		    ua->copy = M_COPYOUT;
		    transparentCopy(q, mp, ua);
		} else {
		    cmn_err(CE_WARN,"DDP:tpiIoctl - can't getIfreqFlags <%s>",
			    ifr->ifr_name);
		}
	    } else {		/* must be the copyout acknowledgement */
		ASSERT(ua->copy == M_COPYOUT);
		done = 1;
	    }
	}
	break;
	
    case SIOCGIFFLAGS:		/* get the named ifreq flags */
	if (sts == 0) {		/* [copyin | copyout] was aok at STREAM head */
	    if (ua->copy == M_COPYIN) { /* its the copyin acknowledgement */
		ASSERT(mp->b_cont->b_rptr != NULL); /* this is users data */
		ifr = (struct ifreq *)mp->b_cont->b_rptr;
		if ((sts = getIfreqFlags(ifr)) == 0) {
		    ua->copy = M_COPYOUT;
		    transparentCopy(q, mp, ua);
		} else {
		    cmn_err(CE_WARN,"DDP:tpiIoctl - can't getIfreqAddr <%s>",
			    ifr->ifr_name);
		}
	    } else {		/* must be the copyout acknowledgement */
		ASSERT(ua->copy == M_COPYOUT);
		done = 1;
	    }
	}
	break;
	
    case SIOCGIFCONF:
	if (sts == 0) {
	    static void *usrAddr;
	    struct ifconf *ifc;
	    
	    if (ua->copy == M_COPYIN) {
		struct ifreq *ifrs;
		void *bufAddr;
		int maxSize, size;
		
		ASSERT(mp->b_cont->b_rptr != NULL); /* this is users data */
		usrAddr = ua->addr;
		
		/* get the attributes of the users request */
		ifc = (struct ifconf *)mp->b_cont->b_rptr;
		bufAddr = ifc->ifc_buf;
		maxSize = ifc->ifc_len;
		
		/* re use the same b_cont blk */
		ifrs = (struct ifreq *)mp->b_cont->b_rptr;
		
		/* build a table of all available appletalk interfacess */
		size = getInterfaces(ifrs, maxSize);
		
		/* copy the data back out to waiting user */
		mp->b_cont->b_wptr = mp->b_cont->b_rptr + size;
		ua->cnt  = 1;
		ua->copy = M_COPYOUT;
		ua->addr = bufAddr;
		ua->size = size;
		transparentCopy(q, mp, ua);
		
	    } else {			/* it was a copy out */
		ASSERT(ua->copy == M_COPYOUT);
		if (ua->cnt == 1) {
		    /* need a b_cont for copyout */
		    if (mp->b_cont == NULL) {
			mp->b_cont = allocb(ua->size, BPRI_HI);
			ASSERT(mp->b_cont != NULL);
		    }
		    mp->b_cont->b_datap->db_type = M_DATA;
		    mp->b_cont->b_wptr = mp->b_cont->b_rptr + sizeof *ifc;
		    
		    ifc = (struct ifconf *)mp->b_cont->b_rptr;
		    ifc->ifc_len = ua->size;
		    ifc->ifc_req = ua->addr;
		    
		    /* issue part 2 of the copyout sequence */
		    ua->addr = usrAddr;
		    ua->size = sizeof *ifc;
		    ua->cnt  = 2;	/* part 2 of 2 part copyout sequence */
		    ua->copy = M_COPYOUT; /* addr & size set in phase0 above */
		    transparentCopy(q, mp, ua);
		    dcmn_err((CE_CONT, "done transparent copyout part 2"));
		    
		} else {
		    ASSERT(ua->cnt == 2);
		    done = 1;
		}
	    }
	}
	break;
	
    case TI_GETPEERNAME:
	/* don't know what to do so do what GETMYNAME does for now */
	/* FALL THROUGH */
	sts = EINVAL;
	break;

    case TI_GETMYNAME:
	dcmn_err((CE_CONT, "*** doing TI_GETMYNAME ***"));
	if (sts == 0) {		/* no problems with transparent copy */
	    if ( ua->copy == M_COPYIN ) { /* copyin has completed AOK */
		void *userBufAddr;
		struct netbuf	*np;
		
		/* its the copyin acknowledgement */
		ASSERT(mp->b_cont != NULL);/* this is where the user data is */
		
		/* get user buffer address that we copied in */
		np = (struct netbuf *)mp->b_cont->b_rptr;
		
		/* set the length of the data message */
		np->len = sizeof(struct sockaddr_at);
		userBufAddr = np->buf;
		
		/* start copy out part 1 - the netbuf structure */
		ua->cnt  = 1;		/* part 1 of 2 part copyout sequence */
		ua->copy = M_COPYOUT;
		transparentCopy(q, mp, ua);
		dcmn_err((CE_CONT, "done transparent copyout part 1"));
		
		/* prepare for part 2 of copyout sequence - netbuf->buf data */
		ua->size = sizeof(struct sockaddr_at);
		ua->addr = userBufAddr;
		
	    } else {			/* its an M_COPYOUT */
		if (ua->cnt == 1) {	/* 1st part of two part copyout done */
		    struct sockaddr_at  *sat;
		    struct atalk_socket *ats;
		    
		    ASSERT(ua->copy == M_COPYOUT);
		    
		    /* need a b_cont for copyout */
		    if (mp->b_cont == NULL) {
			mp->b_cont = allocb(ua->size, BPRI_HI);
			ASSERT(mp->b_cont != NULL);
		    }
		    mp->b_cont->b_datap->db_type = M_DATA;
		    mp->b_cont->b_wptr = mp->b_cont->b_rptr + sizeof *sat;
		    
		    /* write the data here to be copied out to user space */
		    sat = (struct sockaddr_at *)mp->b_cont->b_rptr;
		    ats = (struct atalk_socket *)q->q_ptr;
		    
		    /* gather the information needed to answer this request */
		    sat->sat_family      = AF_APPLETALK;
		    sat->sat_port        = ats->at.src_port;
		    sat->sat_addr.s_net  = ats->at.src_net;
		    sat->sat_addr.s_node = ats->at.src_node;
		    
		    dcmn_err((CE_CONT,"copyout %d bytes <%d,%d,%d> %x-%d",
			    sizeof *sat, sat->sat_port,sat->sat_addr.s_net,
			    sat->sat_addr.s_node, ua->addr, ua->size));
		    
		    /* issue part 2 of the copyout sequence */
		    ua->cnt  = 2;/* part 2 of 2 part copyout sequence */
		    ua->copy = M_COPYOUT; /* addr+size set in phase0 above */
		    transparentCopy(q, mp, ua);
		    dcmn_err((CE_CONT, "done transparent copyout part 2"));
		    
		} else {		/* done part 2 of a 2 part sequence */
		    ASSERT(ua->cnt == 2);
		    ASSERT(ua->copy == M_COPYOUT);
		    done = 1;
		}
	    }
	}
	break;
	
    default:
	cmn_err(CE_WARN, "DDP:tpiIoctlData - unknown cmd <%d>", csp->cp_cmd);
	sts = EINVAL;
	break;
    }

    if (sts != 0 || done) {	/* we're finished here */
	kmem_free(ua, sizeof *ua);	/* so won't be needing this any more */
	ioctlReply(q, mp, sts);	/* pass sts back to users ioctl */
    }
}

/* Find a match for a specific network:node pair */
static atalk_iface *findInterface(int net, int node)
{
  atalk_iface *iface;
  mutex_enter(&AtalkIfaceMutex);
  for (iface = atalk_iface_list; iface; iface = iface->next) {
    dcmn_err((CE_CONT, "net[%d,%d],node[%d,%d],status=%d\n",
	      iface->aa.addr.s_net, net, iface->aa.addr.s_node, node,
	      iface->aa.flags ));
    if (( (node == ATADDR_BCAST)
	  || (node == ATADDR_ANYNODE)
	  || (iface->aa.addr.s_node == node)) 
	&& (iface->aa.addr.s_net == net)
	&& (iface->aa.flags==ATIF_PROBE_COMPLETE) )
      break;
  }
  mutex_exit(&AtalkIfaceMutex);
  return iface;
}


/*
 * Routing Routines
 */

/*
 *	Find a route for an appletalk packet. This ought to get cached in
 *	the socket (later on...). We know about host routes and the fact
 *	that a route must be direct to broadcast.
 */
static struct atalk_route *
findRoute(struct at_addr *target)
{
  struct atalk_route *r;
  mutex_enter(&AtalkRouteMutex);
  for (r = atalk_router_list; r != NULL; r = r->next) {
    if (! (r->flags & RTF_UP))
      continue;

    if (r->target.s_net == target->s_net)
      if (! (r->flags & RTF_HOST) || r->target.s_node == target->s_node)
	break;
  }
  mutex_exit(&AtalkRouteMutex);

  if (r == NULL && DefaultRoute.dev != NULL)
    return &DefaultRoute;

  return r;
}

void scrubRoutes( atalk_iface *ifp )
{
    struct sockaddr_at	*sat;
    struct rtentry	rt;

    bzero( (void *)&rt, sizeof( rt ));
    sat = (struct sockaddr_at *)&rt.rt_dst;
    sat->sat_family = AF_APPLETALK;
    sat->sat_addr.s_node = ATADDR_ANYNODE;

    /*
     * If the range is the full 0-fffe range, just use
     * the default route.
     */
    if ( ifp->aa.nr.nr_firstnet == htons( 0x0000 ) &&
	    ifp->aa.nr.nr_lastnet == htons( 0xfffe )) {
	sat->sat_addr.s_net = 0;
	deleteRoute( &rt );
    } else {
	u_short	i;

	for ( i = ntohs( ifp->aa.nr.nr_firstnet );
		i <= ntohs( ifp->aa.nr.nr_lastnet ); i++ ) {
	    sat->sat_addr.s_net = htons( i );
	    deleteRoute( &rt );
	}
    }
}

static u_short checkSum(mblk_t *mblk, int skip)
{
    mblk_t *p;
    u_char *c;
    u_long cksum = 0;

    /* process every data block in the entire message */
    for (p = mblk; p; p = p->b_cont) {
	for (c = p->b_rptr; c < p->b_wptr; ++c) {
	    if (skip > 0) {	/* caller advised to skip so many bytes */
		--skip;
		continue;
	    }
	    cksum = (cksum + *c) << 1;
	    if (cksum & 0x00010000)
		++cksum;
	    cksum &= 0x0000ffff;
	}
    }
    if (cksum == 0)
	cksum = 0x0000ffff;
    
    return (u_short)cksum;
}


addMulticast(struct ifreq *ifr)
{
    extern int dlpiEnableMulticast (atalk_iface *atp, u_char *multicast);
    atalk_iface *iface;

    /* find the interface */
    iface = findNamedInterface(ifr->ifr_name);
    if (iface == NULL)
	return ENODEV;

    return dlpiEnableMulticast (iface, (u_char *)ifr->ifr_addr.sa_data );
}

/* report configuration of named interface */
static int getIfreqAddr(struct ifreq *ifr)
{
    atalk_iface *iface;
    struct sockaddr_at *sat;

    dcmn_err((CE_CONT,"entering getIfreqAddr for <%s>\n", ifr->ifr_name));

    /* find the interface */
    iface = findNamedInterface(ifr->ifr_name);
    if (iface == NULL)
	return ENODEV;

    sat = (struct sockaddr_at *)&ifr->ifr_addr;
    sat->sat_family = AF_APPLETALK;
    sat->sat_port = 0;
    sat->sat_addr = iface->aa.addr;
    *(struct netrange *)sat->sat_zero = iface->aa.nr;
 
    return 0;			/* success */
}

/* configure a named interface */
static int setIfreqAddr( queue_t *q, mblk_t *mp, struct ifreq *ifr)
{
  atalk_iface *atp;
  struct sockaddr_at *sat;
  struct netrange *nr;
  struct rtentry rtdef;
  int sts;

  sat = (struct sockaddr_at *)&ifr->ifr_addr;
  if (sat->sat_family != AF_APPLETALK) {
    cmn_err(CE_WARN, "DDP:setIfreqAddr - Wrong socket family");
    return EINVAL;
  }

  if (sat->sat_addr.s_node == ATADDR_BCAST || sat->sat_addr.s_node == 254) {
    cmn_err(CE_WARN, "DDP:setIfreqAddr - Can't set a broadcast address");
    return EINVAL;
  }

  nr = (struct netrange *)sat->sat_zero;
  if (nr->nr_phase != 2 ) {
    cmn_err(CE_WARN,"DDP:setIfreqAddr - Phase %d NOT supported",nr->nr_phase);
    return EPROTONOSUPPORT;
  }

  /* search for correct interface */
  if (! (atp = findNamedInterface(ifr->ifr_name))) {
    cmn_err(CE_WARN,"DDP:tpisetIfreqAddr - No such device %s", ifr->ifr_name);
    return EINVAL;
  }
  if (atp != NULL) {		/* found an existing device */
    if (atp->aa.flags == ATIF_PROBE ) {
      cmn_err(CE_WARN,"DDP:setIfreqAddr - Already setting address (%s)",
	      ifr->ifr_name);
      return EBUSY;
    }
  } else {			/* describing a new device */
    cmn_err(CE_WARN, "device <%s> not found", ifr->ifr_name);
  }

  /*
   * Really, we should do a more checks, but...
   */
  if (( atp->flags & IFF_LOOPBACK ) == 0 &&
	sat->sat_addr.s_net == ATADDR_ANYNET ) {
    sat->sat_addr.s_net = nr->nr_firstnet;
  }

  if ( atp->flags & IFF_UP ) {
    atp->flags &= ~IFF_UP;
    scrubRoutes( atp );
  }

  atp->aa.addr = sat->sat_addr;
  atp->aa.nr   = *nr;

  /* Everything worked, so add the direct routes */
  sat = (struct sockaddr_at *)&rtdef.rt_gateway;
  sat->sat_family      = AF_APPLETALK;
  sat->sat_addr.s_net  = atp->aa.addr.s_net;
  sat->sat_addr.s_node = atp->aa.addr.s_node;

  sat = (struct sockaddr_at *)&rtdef.rt_dst;
  rtdef.rt_flags = RTF_UP;
  sat->sat_family = AF_APPLETALK;
  sat->sat_addr.s_node = ATADDR_ANYNODE;
  if (atp->flags & IFF_LOOPBACK)
    rtdef.rt_flags |= RTF_HOST;

  /* Routerless initial state */
  if (nr->nr_firstnet == htons(0) && nr->nr_lastnet == htons(0xFFFE)) {
    sat->sat_addr.s_net = atp->aa.addr.s_net;
    sts = createRoute(&rtdef, atp);
    if (sts != 0)
      return sts;
    setDefaultRoute(atp);
  } else {
    int		limit;
    u_short	i;

    limit = ntohs(nr->nr_lastnet);
    if ((int)(limit - ntohs(nr->nr_firstnet)) > ddp_nlocalrts) {
      cmn_err(CE_WARN, "DDP:tpiSetAddr - Too many routes/iface (%s), %d max %d",
	      ifr->ifr_name, limit - ntohs( nr->nr_firstnet ), ddp_nlocalrts );
      return EINVAL;
    }
    for(i = ntohs(nr->nr_firstnet); i <= limit; ++i) {
      sat->sat_addr.s_net = htons(i);
      sts = createRoute(&rtdef, atp);
      if (sts != 0)
	return sts;
    }
  }
  atp->flags |= IFF_UP;

  if (atp->flags & IFF_LOOPBACK) {
    atp->aa.flags = ATIF_PROBE_COMPLETE;
    return 0;
  }

  /* if not loopback, we reply after aarp_probe*/
  atp->aa.q  = q;
  atp->aa.mp = mp;

  /*
   * We should check if we need to probe, but...
   */
  atp->aa.flags = 0;

  /* tell aarp to probe our address */
  aarp_probe_setup ( atp );

  return -1;
}


/*
 * Add a router. Basically make sure it looks valid and stuff the
 * entry in the list. While it uses netranges we always set them to one
 * entry to work like netatalk.
 */
static int createRoute(struct rtentry *r, struct atalk_iface *devhint)
{
  struct sockaddr_at *ta, *ga;
  struct atalk_route *rt;
  unsigned long flags;

  ta = (struct sockaddr_at *)&r->rt_dst;
  ga = (struct sockaddr_at *)&r->rt_gateway;

  dcmn_err((CE_CONT, "Entering createRoute for %d,%d\n",
	    ta->sat_addr.s_net, ta->sat_addr.s_node));

  if (ta->sat_family != AF_APPLETALK)
    return EINVAL;

  if (devhint == NULL && ga->sat_family != AF_APPLETALK)
    return EINVAL;
	
  /* Now walk the routing table and make our decisions */
  mutex_enter(&AtalkRouteMutex);
  for (rt = atalk_router_list; rt != NULL; rt = rt->next) {
    if (r->rt_flags != rt->flags)
      continue;

    if (ta->sat_addr.s_net == rt->target.s_net) {
      if (! (rt->flags & RTF_HOST))
	break;

      if (ta->sat_addr.s_node == rt->target.s_node)
	break;
    }
  }
  mutex_exit(&AtalkRouteMutex);

  if (devhint == NULL) {
    struct atalk_iface *iface, *riface;

    riface = NULL;
    mutex_enter( &AtalkIfaceMutex );
    for (iface = atalk_iface_list; iface != NULL; iface = iface->next) {
      if (riface == NULL 
       && ntohs(ga->sat_addr.s_net) >= ntohs(iface->aa.nr.nr_firstnet)
       && ntohs(ga->sat_addr.s_net) <= ntohs(iface->aa.nr.nr_lastnet))
	riface = iface;

      if (ga->sat_addr.s_net == iface->aa.addr.s_net
       && ga->sat_addr.s_node == iface->aa.addr.s_node)
	riface = iface;
    }
    mutex_exit( &AtalkIfaceMutex );
    if (riface == NULL)
      return ENETUNREACH;
    devhint = riface;
  }

  if (rt == NULL) {
    /* create a new route structure and add it into our global list */
    rt = kmem_alloc(sizeof *rt, KM_NOSLEEP);
    if (rt == NULL)
      return ENOBUFS;

    mutex_enter(&AtalkRouteMutex);
    rt->next = atalk_router_list;
    atalk_router_list = rt;
    mutex_exit(&AtalkRouteMutex);
  }

  /* Fill in the entry */
  rt->target = ta->sat_addr;			
  rt->dev    = (void *)devhint;
  rt->flags  = r->rt_flags;
  rt->gateway=ga->sat_addr;

  return 0;
}

/* Set up a default router */
static void setDefaultRoute(atalk_iface *dev)
{
  DefaultRoute.dev = dev;
  DefaultRoute.flags = RTF_UP;
  DefaultRoute.gateway.s_net = htons(0);
  DefaultRoute.gateway.s_node = 0;
}

/* Find matching socket(s) to send the received message up to */
static int matchSocket (struct sockaddr_at *s1, struct sockaddr_at *s2)
{
    struct at_addr *sa1 = &s1->sat_addr; /* the socket being considered */
    struct at_addr *sa2 = &s2->sat_addr; /* address from message received */

    if (s1->sat_port == s2->sat_port) {	/* port number must always match */
	if (sa2->s_net == 0) {		/* msg won't leave this machine */
	    if (sa2->s_node == 0) {	/* what does this mean ??? */
		if (sa1->s_net == 0 && sa1->s_node == 0) {
		    dcmn_err((CE_CONT, "loop back special\n"));
		    return ( YES );
		}
	    } else if (sa1->s_node > 0
		       && (sa2->s_node == 0xff || sa1->s_node == sa2->s_node)){
		dcmn_err((CE_CONT, "net=0, non loop back\n" ));
		return ( YES );
	    }
	} else if (sa1->s_net == sa2->s_net) { /* we are intended machine */
	    dcmn_err(( CE_CONT, "specific net\n" ));
	    if (sa2->s_node == 0xff || sa1->s_node == sa2->s_node) {
		return YES;	/* broadcast match or real match */
	    }
	} else {		/* not intended for this machine */
	    dcmn_err((CE_WARN, "packet not meant for this machine"));
	}
    }
    return NO;
}

/* assemble match properties, and do a match */
static	int doSocketMatch ( atalk_socket *s, struct sockaddr_at *sat )
{
    struct sockaddr_at	s1;

    s1.sat_port = s->at.src_port;
    s1.sat_addr.s_net = s->at.src_net;
    s1.sat_addr.s_node = s->at.src_node;
/*    dcmn_err((CE_CONT,"port={%d,%d},net={%d,%d},node={%d,%d}\n",
	      s1.sat_port,sat->sat_port,
	      s1.sat_addr.s_net, sat->sat_addr.s_net,
	      s1.sat_addr.s_node, sat->sat_addr.s_node ));
*/
    return ( matchSocket ( &s1, sat ));
}
    
/* Find socket in the list, if socket_list is NULL, it will reset to
   atalk_sock_list. The intention here is to enable continue search
   through the socket list, since there are possible multiple socket
   satisfy the criteria */
atalk_socket *locateSocket ( atalk_socket      	*s,
			     struct sockaddr_at	*sat )
{
    if ( !s )
	s = atalk_socket_list;
    else if ( s->next )
	s = s->next;
    else
	return ( (atalk_socket *) NULL );

    /* lock socket list */
    mutex_enter(&AtalkSocketMutex);

    for ( ; s ; s=s->next )
    {
	if ( doSocketMatch ( s, sat ) == YES )
	    break;
    }

    /* release lock */
    mutex_exit(&AtalkSocketMutex);
    return ( s );
}

/* Find a socket in the list */
atalk_socket *findSocket(struct sockaddr_at *sat)
{
    atalk_socket *s;
    dcmn_err((CE_CONT, "findSocket\n"));
    mutex_enter(&AtalkSocketMutex);
    for (s = atalk_socket_list; s != NULL; s = s->next)
    {
	if (s->at.src_net == sat->sat_addr.s_net
	    && s->at.src_node == sat->sat_addr.s_node
	    && s->at.src_port == sat->sat_port)
	    break;
    }
    mutex_exit(&AtalkSocketMutex);
    return s;
}

/* build a table of all available appletalk handling network interfaces */
static int getInterfaces(struct ifreq *ifrs, int maxsize)
{
  atalk_iface *iface;
  int size;

  mutex_enter(&AtalkIfaceMutex);
  for (size = 0, iface = atalk_iface_list; 
       iface != NULL && size + sizeof *ifrs <= maxsize;
       iface = iface->next, ++ifrs, size += sizeof *ifrs) {
    /* populate the ifrs table */
    dcmn_err((CE_CONT, "adding <%s> interface to %x", iface->ifr_name, ifrs));
    strcpy(ifrs->ifr_name, iface->ifr_name);
  }
  mutex_exit(&AtalkIfaceMutex);
  return size;
}
