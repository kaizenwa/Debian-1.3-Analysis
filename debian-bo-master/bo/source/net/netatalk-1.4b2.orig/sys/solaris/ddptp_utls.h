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

#ifndef _DDPTP_UTLS
#  define _DDPTP_UTLS
#  define PROTOTYPE extern

   typedef struct sockaddr_at sat_t;
   typedef IF_DATA            ifd_t;

   PROTOTYPE
   int     ddptp_verify_msg(queue_t *, mblk_t *, sat_t *, sat_t *);

   PROTOTYPE
   u_short get_upper_qs(sat_t *, sat_t *, queue_t **);

   PROTOTYPE
   mblk_t *get_tpi_hdr(long, sat_t *);

   PROTOTYPE
   void    msg_print(mblk_t *),
           hex_print(int, u_char *);

#endif
