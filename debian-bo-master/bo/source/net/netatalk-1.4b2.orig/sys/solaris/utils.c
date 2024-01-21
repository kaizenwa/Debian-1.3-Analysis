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

#include <net/if.h>
#include <net/route.h>
#include <inet/common.h>

#include "ddp.h"

#include <sys/ddi.h>		/* ddi's need to be included last */
#include <sys/sunddi.h>

#include <sys/time.h>

/* Local function prototype */
static void	timerService	( TIMER_DATA	*tp );

/* 
 * Routines to be shared by all
 * Prototypes in ddp.h for everone to use
 */

/* For STREAMS, copying data between user and kernel is a major production
 * the user has done an ioctl(socket, SIOC?, &ifreq) and we need to copy into
 * kernel the user's struct ifreq contents
 * Transparent STREAMS operation is assumed
 */
void transparentCopy(queue_t *q, mblk_t *mp, struct userArg *ua)
{
  struct copyreq *cqp;

  /* reuse the mp message to request a COPYIN */
  cqp = (struct copyreq *)mp->b_rptr;
  cqp->cq_flag = 0;
  cqp->cq_private = (void *)ua;
  cqp->cq_addr = ua->addr;
  cqp->cq_size = ua->size;

  mp->b_datap->db_type = ua->copy;
  mp->b_wptr = mp->b_rptr + sizeof(struct copyreq);
  qreply(q, mp);		/* expect an M_IOCDATA when this is done */
}


/* Shared by ioctl handling in both dlpi layer and tpi layer */
void ioctlReply(queue_t *q, mblk_t *mp, int error)
{
  struct iocblk	*iocbp = ( struct iocblk *)mp->b_rptr;

  if (error != 0) {		/* should be errno values */
    dcmn_err((CE_CONT,"M_IOCNAK: error=%d\n", error));
    mp->b_datap->db_type = M_IOCNAK;
    iocbp->ioc_error = error;
    iocbp->ioc_count = 0;
    iocbp->ioc_rval  = -1;

  } else {
    dcmn_err((CE_CONT, "M_IOCACK, ok\n"));
    mp->b_datap->db_type = M_IOCACK;
    iocbp->ioc_error = 0;
    iocbp->ioc_count = 0;
    iocbp->ioc_rval  = 0;
  }
  mp->b_wptr = mp->b_rptr + sizeof(struct iocblk);

  /* don't need any continuation messages */
  if (mp->b_cont) {
    freemsg(mp->b_cont);
    mp->b_cont = NULL;
  }

  qreply(q, mp);
}

/* Find and return a pointer to the primary interface structure */
struct at_addr *findPrimary(void)
{
  atalk_iface *iface;
  dcmn_err((CE_NOTE, "entering findPrimary"));

  /* find a matching non-loopback interface */
  for (iface = atalk_iface_list; iface != NULL; iface = iface->next) {
    dcmn_err((CE_CONT, "name=%s\n", iface->ifr_name ));
    if (! (iface->flags & IFF_LOOPBACK))
      return &iface->aa.addr;
  }

  /* nothing found - use the default if there is one */
  if (atalk_iface_list != NULL) {
    dcmn_err((CE_CONT, "loopback name=%s\n", atalk_iface_list->ifr_name));
    return &atalk_iface_list->aa.addr;
  }

  /* no available interface */
  return NULL;
}

/* Given an interface name, produce a matching atalk_iface structure */
atalk_iface *findNamedInterface(char *name)
{
  atalk_iface *iface;
  mutex_enter(&AtalkIfaceMutex);
  for (iface = atalk_iface_list; iface != NULL; iface = iface->next)
    if (strcmp(name, iface->ifr_name) == 0)
      break;
  mutex_exit(&AtalkIfaceMutex);
  return iface;
}

/*****************************************************************************
* TIMER LIBRARY
*****************************************************************************/
/*****************************************************************************
*
* Function	timer_init
*
* Description	This function is called to prepare a timer. This function
*		must be called in prior to any other functions in the timer
*		library.
*
* Return	SUCCESS
*		FAIL
*
*****************************************************************************/

int	timerInit (
TIMER_DATA *tp,
int	   type,
int	   interval,
int	   times,
queue_t	   *readq
)
{
    dcmn_err((CE_CONT, "timerInit\n"));

    /* sanity check */
    ASSERT ( tp != NULL );

    /* check input parameters */
    if ( tp->type || tp->interval || tp->times || tp->replyq )
    {
	dcmn_err((CE_CONT,"Timer structure is in use\n"));
	return ( -1 );
    }

    tp->type = type;
    tp->interval = interval;
    tp->times = times;
    tp->replyq = readq;
    tp->tp = tp;

    /* Initialise mutex lock */
    mutex_init ( &tp->mu, "atalk mu", MUTEX_DRIVER, NULL );

    return ( 0 );
}
/****************************************************************************/

/*****************************************************************************
*
* Function	timerStart
*
* Description	This function is called to start a timer, and timerInit()
*		must called in prior to this function.
*
* Return	0 - success
*		-1 - fail
*
******************************************************************************/

int	timerStart (
TIMER_DATA *tp
)
{
    dcmn_err((CE_CONT, "timerStart\n"));

    /* sanity check */
    if ( !tp || !tp->type || !tp->interval || !tp->replyq )
    {
	dcmn_err((CE_CONT, "Incorrect values in tp\n"));
	return ( -1 );
    }

    /* obtain lock */
    mutex_enter ( &tp->mu );

    /* check whether the timer has already started */
    if ( !tp->id )
	tp->id = qtimeout ( tp->replyq, timerService,
			    (caddr_t)tp, tp->interval );

    /* release lock */
    mutex_exit ( &tp->mu );

    return ( 0 );
}
/****************************************************************************/

/*****************************************************************************
*
* Function	timerService
*
* Description	This function is timer service routine, it is responsible for
*		sending out a time out message, and start next timeout if
*		is required.
*
* Return	None
*
******************************************************************************/

static void timerService (
TIMER_DATA *tp
)
{
    mblk_t	*mp;

    /* sanity check */
    ASSERT ( tp!=NULL );
    ASSERT ( tp->replyq != NULL );

    /* create message block */
    if ( (mp = allocb ( sizeof(TIMER_DATA *), BPRI_HI)) )
    {
	TIMER_DATA	*timep;

	/* copy data to message */
	timep = ( TIMER_DATA *) mp->b_rptr;
	*timep = *tp;
	
	/* set stream message type */
	mp->b_datap->db_type = M_PCSIG;
	mp->b_wptr = mp->b_rptr + sizeof ( TIMER_DATA *);
    }
    else
	cmn_err ( CE_WARN, "timerService: can't allocb()" );

    /* update no. of timeleft */
    tp->times--;

    /* if timer is still required, start off next one */
    if ( tp->times>0 )
	tp->id = qtimeout ( tp->replyq, timerService, (caddr_t) tp,
			    tp->interval );
    else
    {
	/* timer is no longer required */
	tp->id = 0;
    }

    /* acknownlege time out */
    if ( mp )
	put ( tp->replyq, mp );
}
/****************************************************************************/

/*****************************************************************************
*
* Function	timerReset
*
* Description	This function is called to reset timer value, and start
*		timer.
*
* Return
*
*****************************************************************************/

int	timerReset (
TIMER_DATA *tp,
int	   interval,
int	   times
)
{
    dcmn_err((CE_CONT,"timerReset\n"));

    /* sanity check */
    ASSERT ( tp != NULL );

    tp->interval = interval;
    tp->times = times;

    return ( timerStart ( tp ) );
}
/****************************************************************************/

/*****************************************************************************
*
* Function	timerStop
*
* Description	This function is called to stop a timer temporary, it can be
*		restarted by calling timerStart() again.
*
* Return	None
*
******************************************************************************/

void timerStop (
TIMER_DATA *tp
)
{
    dcmn_err((CE_CONT, "stopTimer\n"));

    /* sanity check */
    ASSERT ( tp != NULL );

    /* obtain lock */
    mutex_enter ( &tp->mu );

    /* stop timer if it is running */
    if ( tp->id )
    {
	quntimeout ( tp->replyq, tp->id );
	tp->id = 0;
    }

    /* release lock */
    mutex_exit ( &tp->mu );
}
/****************************************************************************/

/*****************************************************************************
*
* Function	destroyTimer
*
* Description	This function is called when initiated timer is no longer
*		needed. This function must be called after timerInit().
*
* Return	None
*
*****************************************************************************/

void timerDestroy (
TIMER_DATA *tp
)
{
    dcmn_err((CE_CONT, "timerDestroy\n"));

    ASSERT ( tp != NULL );

    /* obtain lock */
    mutex_enter ( &tp->mu );

    /* delete timer if it is running */
    if ( tp->id )
    {
	quntimeout ( tp->replyq, tp->id );
	tp->id = 0;
    }

    /* reset structure */
    tp->type = 0;
    tp->interval = 0;
    tp->times = 0;
    tp->replyq = NULL;

    /* release lock */
    mutex_exit ( &tp->mu );

    /* destroy lock */
    mutex_destroy ( &tp->mu );
}
/****************************************************************************/
