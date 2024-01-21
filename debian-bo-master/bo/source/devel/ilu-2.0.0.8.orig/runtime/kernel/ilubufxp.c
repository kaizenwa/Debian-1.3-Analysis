/*
 * Copyright (c) 1991-1995 Xerox Corporation.  All Rights Reserved.
 * 
 * Unlimited use, reproduction, and distribution of this software is
 * permitted.  Any copy of this software must include both the above
 * copyright notice of Xerox Corporation and this paragraph.  Any
 * distribution of this software must comply with all applicable
 * United States export control laws.  This software is made
 * available AS IS, and XEROX CORPORATION DISCLAIMS ALL WARRANTIES,
 * EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, AND NOTWITHSTANDING ANY OTHER PROVISION CONTAINED
 * HEREIN, ANY LIABILITY FOR DAMAGES RESULTING FROM THE SOFTWARE OR
 * ITS USE IS EXPRESSLY DISCLAIMED, WHETHER ARISING IN CONTRACT,
 * TORT (INCLUDING NEGLIGENCE) OR STRICT LIABILITY, EVEN IF XEROX
 * CORPORATION IS ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
 */
/*
 * $Id: ilubufxp.c,v 1.41 1995/08/24 23:25:56 spreitze Exp spreitze
 * $
 */
/* Last edited by Mike Spreitzer November 1, 1995 3:11 pm PST */

#include "iluntrnl.h"
#include "transprt.h"

/* L1, L2 unconstrained */

typedef struct buffCons BuffCons, *BuffList;

#define CHUNKSIZE 4096

struct buffCons {
  BuffList        next;
  ilu_cardinal    start, len;
  ilu_byte        bytes[CHUNKSIZE];
};

typedef struct {
  BuffList        buffs;
}              *TransportParms;
/*
 * First buffer is in exposed buffer while processing the message.
 * tr_inBuff != NIL <=> processing the message.  Elements are freed
 * as they are passed.  Between close and read of last byte,
 * buffs==NIL and tr_inBuff points to freed buffer.
 */

static          ilu_integer
CloseDFd(ilu_Transport t)
{
  return 0;
}

static          ilu_boolean
SetInputHandler(ilu_Transport t,
		ilu_TransportInputHandler tih,
		ilu_refany tih_rock,
		ILU_ERRS((no_memory, internal,
			  no_resources)) * err)
{
  /*
   * tc_set_input_handler is used only for incoming transports. This
   * is only an outgoing transport.
   */
  return ILU_ERR_CONS1(internal, err, minor, ilu_im_tportRole, FALSE);
}

static          ilu_boolean
WaitForInput(ilu_Transport t,
	     ilu_FineTime * limit,
	     ILU_ERRS((interrupted)) * err)
{
  return ILU_CLER(*err);
}

static ilu_boolean 
Interrupt(ilu_Transport t, ILU_ERRS((bad_param)) * err)
{
  return ILU_CLER(*err);
}

static          ilu_ReadHeaderResultCode
BeginMessage(ilu_Transport t,
	     ilu_boolean input,
	     ILU_ERRS((IoErrs)) * err)
{
  /* Creator promised this won't happen. */
  return ILU_ERR_CONS1(internal, err, minor, ilu_im_bufxpMisuse,
		       ilu_rhrc_error);
}

static          ilu_boolean
EndMessage(ilu_Transport t,
	   ilu_boolean flush,
	   ilu_Message * msg,
	   ILU_ERRS((IoErrs)) * err)
{
  if (t->tr_inBuff == NIL)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_endMessage, FALSE);
  t->tr_inBuff = NIL;
  return ILU_CLER(*err);
}

static          ilu_boolean
SendWholeMessage(ilu_Transport t,
		 ilu_Message * msg,
		 ILU_ERRS((IoErrs)) * err)
{
  /* Creator promised I won't be used for output. */
  return ILU_ERR_CONS1(internal, err, minor, ilu_im_bufxpMisuse, FALSE);
}

static          ilu_boolean
WriteBytes(ilu_Transport t, ilu_bytes buf, ilu_cardinal bufLen,
	   ilu_boolean flush, ILU_ERRS((IoErrs)) * err)
{
  /* Creator promised I won't be used for output. */
  return ILU_ERR_CONS1(internal, err, minor, ilu_im_bufxpMisuse, FALSE);
}

static          ilu_cardinal
ReadBytes(ilu_Transport t, ilu_bytes buf, ilu_cardinal bufLen,
	  ilu_TransportReport * rpt, ILU_ERRS((IoErrs)) * err)
{
  TransportParms  tp = (TransportParms) t->tr_data;
  BuffList        head = tp->buffs, next;
  if (t->tr_inBuff == NIL)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_bytesWithoutMsg,
			 0);
  if (t->tr_inNext < t->tr_inLimit)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcInputSkipsBuff,
			 0);
  tp->buffs = next = head->next;
  ilu_free(head);
  if (next != NIL) {
    DEBUG(CONNECTION_DEBUG,
	  (stderr,
	   "ilubufxp.ReadBytes(%p): next chunk has %lu bytes\n",
	   t, (long unsigned) next->len));
    t->tr_inBuff = next->bytes;
    t->tr_inNext = next->start;
    t->tr_inLimit = next->start + next->len;
    ILU_CLER(*err);
    if (buf != NIL) {
      ilu_cardinal    l1 = MIN(next->len, bufLen);
      memcpy((void *) buf, (void *) (t->tr_inBuff + t->tr_inNext),
	     (SIZE_T) l1);
      t->tr_inNext += l1;
      return l1;
    }
    return (next->len);
  }
  DEBUG(CONNECTION_DEBUG,
	(stderr, "ilubufxp.ReadBytes(%p): at EOM&F.\n", t));
  rpt->tr_eom = rpt->tr_eof = TRUE;
  ILU_CLER(*err);
  return 0;
}

static ilu_boolean 
Close(ilu_Transport t, ilu_integer * dfd,
      ILU_ERRS((internal)) * err)
{
  TransportParms  tp = (TransportParms) t->tr_data;
  BuffList        bl = tp->buffs;
  DEBUG(CONNECTION_DEBUG, (stderr, "ilubufxp.Close(%p)\n", t));
  *dfd = 0;
  while (bl != NIL) {
    BuffList        tmp = bl;
    bl = bl->next;
    ilu_free(tmp);
  }
  t->tr_inBuff = NIL;
  tp->buffs = NIL;
  ilu_free(tp);
  ilu_free(t);
  return ILU_CLER(*err);
}

static struct _ilu_TransportClass_s myClass = {
  TRUE,				/* boundaried */
  TRUE,				/* reliable */
  CloseDFd,
  SetInputHandler,
  WaitForInput,
  Interrupt,
  BeginMessage,
  EndMessage,
  SendWholeMessage,
  WriteBytes,
  ReadBytes,
  Close
};

/* Main Invariant holds; L2 >= {t's conn's iomu} */
ilu_Transport
_ilu_BufferInputMessage(ilu_Transport t,
			ILU_ERRS((IoErrs)) * err)
{
  ilu_Transport   ans = (ilu_Transport) ilu_malloc(sizeof(*ans));
  TransportParms  tp = (TransportParms) ilu_malloc(sizeof(tp));
  BuffList       *tailp;
  ilu_cardinal    nchunks = 0, nbytes = 0;
  ilu_TransportReport rpt = {FALSE, FALSE};
  if (ans == NIL || tp == NIL) {
    (void) ILU_ERR_CONS1(no_memory, err, nbytes,
			 sizeof(*ans) + sizeof(*tp),
			 6);
    goto faild;
  }
  tp->buffs = NIL;
  tailp = &tp->buffs;
  while (!rpt.tr_eom) {
    BuffList        c = (BuffList) ilu_malloc(sizeof(*c));
    if (c == NIL) {
      (void) ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(*c), 6);
      goto faild;
    }
    if (t->tr_inBuff == NIL || t->tr_inNext >= t->tr_inLimit)
      if (!(*t->tr_class->tc_wait_for_input) (t, NIL, err))
	goto faild;
    c->start = 0;
    c->next = NIL;
    *tailp = c;
    tailp = &c->next;
    c->len = transport_read_upto_bytes(t, c->bytes, CHUNKSIZE, &rpt,
				       err);
    if (ILU_ERRNOK(*err))
      goto faild;
    nchunks += 1;
    nbytes += c->len;
  }
  ans->tr_inBuff = tp->buffs->bytes;
  ans->tr_inNext = tp->buffs->start;
  ans->tr_inLimit = tp->buffs->start + tp->buffs->len;
  ans->tr_outBuff = NIL;
  ans->tr_outNext = ans->tr_outLimit = 0;
  ans->tr_class = &myClass;
  ans->tr_data = tp;
  if (!transport_end_message(t, FALSE, NIL, err))
    goto faild;
  DEBUG(CONNECTION_DEBUG,
	(stderr,
	 "_ilu_BufferInputMessage(%p) makes %p from %lu bytes in %lu chunks.\n",
	 t, ans, (long unsigned) nbytes, (long unsigned) nchunks));
  return ans;
faild:
  if (tp != NIL) {
    while (tp->buffs != NIL) {
      BuffList        tmp = tp->buffs;
      tp->buffs = tp->buffs->next;
      ilu_free(tmp);
    }
    ilu_free(tp);
  }
  if (ans != NIL)
    ilu_free(ans);
  return NIL;
}
