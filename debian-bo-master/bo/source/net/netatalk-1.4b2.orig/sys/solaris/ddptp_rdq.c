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

/* -----------------------------------------------------------------------------
| DDP Transport Provider ReaD Queue
|
|   This module contains the read queue processing routines for the DDP
| Transport Provider.
|
| Revision History:
| 08-Mar-96  Steve Gopaulsingh  First writing, based on ddptp.c from Maurie
|                               and Simon.
|
----------------------------------------------------------------------------- */
#include <sys/types.h>
#include <sys/stream.h>
#include <sys/cmn_err.h>

#include <sys/ddi.h>

#include <sys/socket.h>
#include <net/if.h>

#include <sys/tihdr.h>

#include <sys/debug.h>
#include <sys/errno.h>

#include "at.h"
#include "ddp.h"
#include "ddptp_utls.h"

extern
int     canput(queue_t *),
        putnext(queue_t *, mblk_t *),
        putq(queue_t *, mblk_t *);

extern
mblk_t *copymsg(mblk_t *);

extern
void    bzero(caddr_t, size_t),
        freemsg(mblk_t *);

#define PROTOTYPE extern

PROTOTYPE
void ddptp_rput(queue_t *, mblk_t *);


#undef L_TEST


/* -----------------------------------------------------------------------------
| DDP Transport Provider Read queue PUT
|
|   Above us are all the streams that have opened /dev/ddp. Process the messsage
| appropriately.
|
|   NOTE: This routine should be "collapsed" once it has been verified that
|         we only see regular M_DATA messages at this point.
|
| Revision History:
| 08-Mar-96  Steve Gopaulsingh  First writing, expanded from original stub.
|
----------------------------------------------------------------------------- */
void ddptp_rput(
queue_t *q,
mblk_t  *mp
)
{
   queue_t *uql[ATPORT_LAST],               /* Re. at.h header. */
           **uq = uql;
   mblk_t  *dm,
           *tpih;
   sat_t    from,
            to;
   u_short  i = (u_short) 0;

   dcmn_err((CE_NOTE, "DDP: ddptp_rput()"));

   bzero((caddr_t) &from, (size_t) sizeof(struct sockaddr_at));
   bzero((caddr_t) &to,   (size_t) sizeof(struct sockaddr_at));

   if (!ddptp_verify_msg(q, mp, &from, &to)) {
      dcmn_err((CE_CONT, "ddptp_rput() message verification failed\n"));
      freemsg(mp);
      return;
   }

   dcmn_err((CE_CONT, "from(%d,%d,%d), to(%d,%d,%d)\n",
	     from.sat_addr.s_net, from.sat_addr.s_node, from.sat_port,
	     to.sat_addr.s_net, to.sat_addr.s_node, to.sat_port ));

   if (!(i = get_upper_qs(&from, &to, uql))) {          /* Terminated list. */
      dcmn_err((CE_CONT, "ddptp_rput() can't find upper queue, freed\n"));
      freemsg(mp);
#ifndef L_TEST
      return;
#endif
   }

   if (!(tpih = get_tpi_hdr(T_UNITDATA_IND, &from))) { /* PRIORITY msg? */
      dcmn_err((CE_CONT, "ddptp_rput() can't get t_unitdata_ind header\n"));
      freemsg(mp);
      return;
   }

   tpih->b_cont = mp;                                  /* Prepend tpi hdr. */

#ifdef L_TEST
   msg_print(tpih);
   return;
   /* NOTREACHED */
#endif

   /* Process priority message */
   /* IS THIS POSSIBLE HERE ? */
   if (mp->b_datap->db_type & QPCTL) {
      dcmn_err((CE_CONT, "ddptp_rput(pri.) # Qs = %d\n", (int) i));
      tpih->b_cont = (mblk_t *)0;                      /* Do we need this? */
      freemsg(tpih);                                   /* No, for now...   */
      for ( ; *uq ; uq++)
         if ((dm = copymsg(mp)))                       /* Give each upper a   */
            (void) putnext(*uq, dm);                   /* distinct copy.      */
         else {
            /* EMPTY */
            dcmn_err((CE_CONT, "ddptp_rput(pri.) can't copymsg()\n"));
         }
      freemsg(mp);
      return;
   }

   /* Process regular message */
   switch (mp->b_datap->db_type) {
      case M_DATA:
         dcmn_err((CE_CONT, "ddptp_rput(M_DATA) # Qs = %d\n", (int) i));
         for ( ; *uq ; uq++)
            if (canput(*uq))
               if ((dm = copymsg(tpih)))               /* Give each upper a   */
                  (void) putnext(*uq, dm);             /* distinct copy.      */
               else {
                  /* EMPTY */
                  dcmn_err((CE_CONT, "ddptp_rput(M_DATA) can't copymsg()\n"));
               }
            else {
               /* EMPTY */
               dcmn_err((CE_CONT, "ddptp_rput(M_DATA) can't put upper Q\n"));
            }
         freemsg(tpih);
         break;
      default:
         dcmn_err((CE_CONT, "ddptp_rput(def.) # Qs = %d\n", (int) i));
         for ( ; *uq ; uq++)
            if (canput(*uq))
               if ((dm = copymsg(tpih)))               /* Give each upper a   */
                  (void) putnext(*uq, dm);             /* distinct copy.      */
               else {
                  /* EMPTY */
                  dcmn_err((CE_CONT, "ddptp_rput(def.) can't copymsg()\n"));
               }
            else {
               /* EMPTY */
               dcmn_err((CE_CONT, "ddptp_rput(def.) can't put upper Q\n"));
            }
         freemsg(tpih);
   }

   return;
}
#undef L_TEST
