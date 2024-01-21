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
|   DDP Transport Provider UTiLitieS
|
|      This module is meant to contain utility functions necessary for the
| support of the DDP Transport Provider
|
----------------------------------------------------------------------------- */
#include <string.h>
#include <sys/types.h>
#include <sys/stream.h>
#include <sys/cmn_err.h>

#include <sys/ddi.h>

#include <sys/socket.h>
#include <net/if.h>

#include <sys/tihdr.h>

#include <inet/common.h>

#include <sys/debug.h>
#include <sys/errno.h>

#include "at.h"
#include "endian.h"
#include "ddp.h"
#include "ddptp_utls.h"
#include "phase2.h"

extern
atalk_socket *findSocket(struct sockaddr_at *);             /* From tpi.c */

extern
int  adjmsg(mblk_t *, int),
     msgdsize(mblk_t *);

extern
mblk_t *allocb(int, uint),
       *msgpullup(mblk_t *, int);

extern
void bcopy(caddr_t, caddr_t, size_t),
     bzero(caddr_t, size_t),
     freemsg(mblk_t *);

/* Adaptations for mblk_t */
#define MTOD(x,t)        ((t) ALIGN32(x->b_rptr))
#define MBLEN(m)         (m->b_wptr - m->b_rptr)
#define MBEXT(m)         (m->b_datap->db_lim - m->b_datap->db_base)
#define MBTYP(m)         (m->b_datap->db_type)
#define MBPRI(m)         (m->b_band)


#undef L_TEST


/* -----------------------------------------------------------------------------
|
|   Do some basic verification and extract source and destination info.
|
----------------------------------------------------------------------------- */
int ddptp_verify_msg(
queue_t *q,
mblk_t  *mp,
sat_t   *from,
sat_t   *to
)
{
   mblk_t         *pm  = (mblk_t *) 0;
   struct ddpehdr *deh,
                   ddpe;
   int             dlen,
                   mlen;

   ASSERT(mp->b_rptr <= mp->b_wptr);
 
#if 0
   msg_print(mp);
   dcmn_err((CE_CONT, "ddptp_verify_msg(): ifr_name = %s\n", atp->ifr_name));
#endif

   /* Toss LLC and SNAP from 802.3 E-frame. */
   (void) adjmsg(mp, sizeof(struct llc));                        /* Hmmm ... */
#if 0
   dcmn_err((CE_CONT, "ddptp_verify_msg(): LLC & SNAP removed\n"));
   hex_print((int) MBLEN(mp), mp->b_rptr);
   dcmn_err((CE_CONT, "\n"));
#endif

   dlen = (int) sizeof(struct ddpehdr);
   if (MBLEN(mp) < dlen) {
      if ((pm = msgpullup(mp, dlen))) {
         dcmn_err((CE_CONT, "ddptp_verify_msg(): ddpehdr from pulled up\n"));
         deh = MTOD(pm, struct ddpehdr *);
      }
      else {
        dcmn_err((CE_CONT, "ddptp_verify_msg(): can't fill struct ddpehdr\n"));
        return(0);
      }
   }
   else {
      dcmn_err((CE_CONT, "ddptp_verify_msg(): ddpehdr from original\n"));
      deh = MTOD(mp, struct ddpehdr *);
   }

   bcopy((caddr_t) deh, (caddr_t) &ddpe, (size_t) sizeof(struct ddpehdr));

   ddpe.deh_bytes = ntohl(ddpe.deh_bytes);
   dlen           = (int) ALIGN32(ddpe.deh_len);
#if 0
   dcmn_err((CE_CONT, "ddptp_verify_msg(): deh_len = %d\n", dlen));
#endif

   if ((int) pm) {
      dcmn_err((CE_CONT, "ddptp_verify_msg(): freeing pulled up\n"));
      freemsg(pm);
   }

   from->sat_family      = AF_APPLETALK;
   from->sat_port        = ddpe.deh_sport;
   from->sat_addr.s_net  = ddpe.deh_snet;
   from->sat_addr.s_node = ddpe.deh_snode;
   to->sat_family        = AF_APPLETALK;
   to->sat_port          = ddpe.deh_dport;
   to->sat_addr.s_net    = ddpe.deh_dnet;
   to->sat_addr.s_node   = ddpe.deh_dnode;

   /*
    * Adjust the length, removing any padding that may have been added
    * at a link layer.  We do this before we attempt to forward a packet,
    * possibly on a different media.
    *
    * For now assume same on Solaris w/DLPI.
    */
   if ((mlen = msgdsize(mp)) < dlen) {
      dcmn_err((CE_CONT, "ddptp_verify_msg(): a-length(%d) header, ignored\n",
                mlen));
      freemsg(mp);
      return(0);
   }

   if (dlen < mlen) {
      dcmn_err((CE_CONT, "ddptp_verify_msg(): a-length > header, adjusting\n"));
      (void) adjmsg(mp, dlen - mlen);
   }

#if 0
   dcmn_err((CE_CONT, "ddptp_verify_msg(): adjusting for ddpehdr\n"));
#endif
   (void) adjmsg(mp, sizeof(struct ddpehdr));        /* Header read. */
#if 0
   hex_print((int) MBLEN(mp), mp->b_rptr);
   dcmn_err((CE_CONT, "\n"));
   dcmn_err((CE_CONT, "ddptp_verify_msg(): aa->flags = %x\n", aa->flags));
#endif

#if 1
   return(1);
#else
   return((int) (aa->flags & AFA_PHASE2));           /* PHASE 2 */
#endif
}


/* -----------------------------------------------------------------------------
|
|   Setup list of matching upper queues and return count.
|
|   keyed to ATPORT_LAST of at.h
|
----------------------------------------------------------------------------- */
/* ARGSUSED */
u_short get_upper_qs(
sat_t    *from,
sat_t    *to,
queue_t **ql
)
{
   atalk_socket *s = (atalk_socket *) 0;
   u_short       i = 0;

   dcmn_err((CE_CONT, "get_upper_qs() entered\n"));

   for (s = locateSocket(s, to) ; s ; i++, s = locateSocket(s, to))
      *(ql + i) = s->rdq;
   *(ql + i) = (queue_t *) 0;

   dcmn_err((CE_CONT, "get_upper_qs() returns %d\n", (int) i));

   return(i);
}


/* -----------------------------------------------------------------------------
|
|   Create appropriate tpi header block.
|
----------------------------------------------------------------------------- */
mblk_t *get_tpi_hdr(
long   tpi_prim_type,
sat_t *adr
)
{
   mblk_t *tpih = (mblk_t *) 0;
   char   *rptr;
   int     size;

   dcmn_err((CE_CONT, "get_tpi_hdr(): entered\n"));
   switch (tpi_prim_type) {
      case T_UNITDATA_IND:
         size = (int) (sizeof(struct T_unitdata_ind) + sizeof(sat_t));
         if ((tpih = allocb(size, BPRI_HI))) {
            rptr = (char *) tpih->b_rptr;
            bzero((caddr_t) rptr, (size_t) size);
            size         = sizeof(struct T_unitdata_ind);
            tpih->b_wptr = (u_char *) &rptr[size];
            tpih->b_datap->db_type = M_PROTO;
            {
               struct T_unitdata_ind *Tui;
               Tui = MTOD(tpih, struct T_unitdata_ind *);
               
               Tui->PRIM_type  = tpi_prim_type;
               Tui->SRC_length = (long) sizeof(sat_t);
               Tui->SRC_offset = (long) size;
               Tui->OPT_length = (long) 0;
               Tui->OPT_offset = Tui->SRC_offset + Tui->SRC_length;
               bcopy((caddr_t) adr,
                     (caddr_t) (tpih->b_rptr + Tui->SRC_offset),
                     (size_t) sizeof(sat_t));
               tpih->b_wptr = (u_char *) &rptr[(int) Tui->OPT_offset];
#if 0
   hex_print((int) MBLEN(tpih), tpih->b_rptr);
   dcmn_err((CE_CONT, "\n"));
#endif
            }
         }
         else {
            /* EMPTY */
            dcmn_err((CE_CONT, "get_tpi_hdr(): can't get header\n"));
         }
         break;
      case T_UDERROR_IND:                      /* Do we get any of these? */
         dcmn_err((CE_CONT, "get_tpi_hdr(): T_UDERROR_IND\n"));
         size = (int) (sizeof(struct T_uderror_ind) + sizeof(sat_t));
         if ((tpih = allocb(size, BPRI_HI))) {
            rptr = (char *) tpih->b_rptr;
            bzero((caddr_t) rptr, (size_t) size);
            size         = sizeof(struct T_uderror_ind);
            tpih->b_wptr = (u_char *) &rptr[size];
            tpih->b_datap->db_type = M_PROTO;
            {
               struct T_uderror_ind *Tui;
               dcmn_err((CE_CONT, "get_tpi_hdr(): setting header\n"));
               Tui = MTOD(tpih, struct T_uderror_ind *);
               
               Tui->PRIM_type   = tpi_prim_type;
               Tui->DEST_length = (long) sizeof(sat_t);
               Tui->DEST_offset = (long) size;
               Tui->OPT_length  = (long) 0;
               Tui->OPT_offset  = Tui->DEST_offset + Tui->DEST_length;
               Tui->ERROR_type  = (long) 0;    /* 'Till we get the real one. */
               bcopy((caddr_t) adr,
                     (caddr_t) (tpih->b_rptr + Tui->DEST_offset),
                     (size_t) sizeof(sat_t));
               tpih->b_wptr = (u_char *) &rptr[(int) Tui->OPT_offset];
            }
         }
         else {
            /* EMPTY */
            dcmn_err((CE_CONT, "get_tpi_hdr(): can't get header\n"));
         }
         break;
      default:
            dcmn_err((CE_CONT, "get_tpi_hdr(): DEFAULT?\n"));
         break;
   }

   return(tpih);
}


/* -----------------------------------------------------------------------------
|
|   Print out a buffer in hex.
|
----------------------------------------------------------------------------- */
/* ARGSUSED */
void msg_print(
mblk_t *m
)
{
   dcmn_err((CE_CONT, "msg_print(%x) START\n", m));
   for ( ; m ; m = m->b_cont) {
      dcmn_err((CE_CONT, "typ=%x\n", MBTYP(m)));
      dcmn_err((CE_CONT, "ext=%d pri=%x\n", MBEXT(m), MBPRI(m)));
      hex_print(MBLEN(m), m->b_rptr);
      dcmn_err((CE_CONT, "\n"));
   }
   dcmn_err((CE_CONT, "msg_print(%x) END\n", m));

   return;
}


/* -----------------------------------------------------------------------------
|
|   Print out a buffer in hex.
|
----------------------------------------------------------------------------- */
/* ARGSUSED */
void hex_print(
int     len,
u_char *buf
)
{
   dcmn_err((CE_CONT, "hex_print(%d, %x)\n", len, buf));
   while (len--)
      dcmn_err((CE_CONT, "%02x ", *buf++));

   return;
}
