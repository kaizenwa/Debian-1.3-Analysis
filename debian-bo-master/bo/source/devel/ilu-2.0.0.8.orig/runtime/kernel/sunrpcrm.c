/*
Copyright (c) 1991-1995 Xerox Corporation.  All Rights Reserved.  

Unlimited use, reproduction, and distribution of this software is
permitted.  Any copy of this software must include both the above
copyright notice of Xerox Corporation and this paragraph.  Any
distribution of this software must comply with all applicable United
States export control laws.  This software is made available AS IS,
and XEROX CORPORATION DISCLAIMS ALL WARRANTIES, EXPRESS OR IMPLIED,
INCLUDING WITHOUT LIMITATION THE IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS FOR A PARTICULAR PURPOSE, AND NOTWITHSTANDING ANY OTHER
PROVISION CONTAINED HEREIN, ANY LIABILITY FOR DAMAGES RESULTING FROM
THE SOFTWARE OR ITS USE IS EXPRESSLY DISCLAIMED, WHETHER ARISING IN
CONTRACT, TORT (INCLUDING NEGLIGENCE) OR STRICT LIABILITY, EVEN IF
XEROX CORPORATION IS ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
*/
/* $Id: sunrpcrm.c,v 1.28 1996/06/06 19:18:23 spreitze Exp $ */
/* Last edited by Mike Spreitzer June 6, 1996 10:37 am PDT */

#include "iluntrnl.h"

#include "transprt.h"
#include "mooring.h"

#include "oscalls.h"

typedef unsigned short ilusunrpcrmport_t;
/* WIN port needs arg of htons() to be an unsigned short */

typedef struct {
  ilu_TransportCreator lower;
}              *CreatorParms;

typedef struct sunrpcrmparms {
  /* L1, L2, Main unconstrained */

  /* For input: */
  ilu_boolean     inWedged;	/* further input msgs forbidden? */
  ilu_boolean     busyIn;	/* currently processing input msg? */
  ilu_boolean     lastIn;	/* current chunk is last? */
  ilu_boolean     firstIn;	/* next chunk is first? */
  ilu_cardinal    inLength;	/* of lower-level data in buffer */
  ilu_cardinal    inChunkRem;	/* how much more after inLength? */
  ilu_cardinal    inSize;	/* size of input buffer */
  /*
   * Unconsumed lower-level input bytes are at indices [tr_inNext,
   * inLength) in tr_inBuff.  When busyIn, firstIn and lastIn are
   * meaningful, and the unconsumed bytes of current chunk are: (a)
   * at indices [tr_inNext, tr_inLimit), and then (b) inChunkRem
   * bytes yet to be read from lower level.  tr_inLimit==inLength
   * when inChunkRem>0.  inChunkRem==0 and tr_inNext==tr_inLimit
   * when !busyIn && !inWedged.  inSize is the allocated size of
   * tr_inBuff.  inWedged and busyIn are mutually exclusive.  When
   * inWedged, no more messages may be input (we've lost synch with
   * lower layer).
   */

  /* For output: */
  ilu_boolean     busyOut;	/* currently processing output msg? */
  ilu_cardinal    outStart;	/* loc o 1st payload byte of cur msg */
  ilu_cardinal    outSize;	/* size of output buffer */
  /*
   * Unwritten lower-level data are at indices [0, tr_outNext) of
   * tr_outBuff.  When !busyOut, tr_outLimit==tr_outNext.  When
   * busyOut, payload so far of current message is at indices
   * [outStart, tr_outNext); header needs to be written at
   * [outStart-4, outStart).  The allocated size of tr_outBuff is
   * outSize, which is always >= tr_outLimit + (busyOut?4:8).
   */

  ilu_Transport   lower;
  ilu_boolean     dirIsIn;
  /*
   * At most one of busyIn, busyOut is true; when one is, dirIsIn
   * indicates which one.
   */
}              *SUNRPCRMParms;
/* What goes in the data field of a SUNRPCRM ilu_Transport. */

/*L1, L2, Main unconstrained*/

#define BUFFERSIZE		(8192 + 8)
#define DIRECT_THRESHOLD	1024

/*L1 >= {connection's iomu}*/
#define SUNRPCRMPARMS(a) ((SUNRPCRMParms)(a))

/*L1_sup < trmu; L2 unconstrained*/
static          CreatorParms
_sunrpcrm_InterpretInfo(ilu_TransportInfo info,
			ILU_ERRS((no_memory, inv_objref)) * err)
{
  CreatorParms    cp;
  ilu_TransportCreator lower;

  if (strncmp(info[0], "sunrpcrm", 8) != 0 ||
      info[1] == NIL)
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ts, NIL);
  lower = _ilu_GetTransportCreator(info + 1, err);
  if (ILU_ERRNOK(*err))
    return NIL;
  if (lower->tcr_boundaried || !lower->tcr_reliable)
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ts, NIL);
  cp = (CreatorParms) ilu_malloc(sizeof(*cp));
  if (cp == NIL)
    return ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(*cp), NIL);
  cp->lower = lower;
  return cp;
}

/*Main Invariant holds; L2 disjoint {conn's iomu, callmu}*/
static          ilu_boolean
SetInputHandler(ilu_Transport self, ilu_TransportInputHandler tih,
		ilu_refany tih_rock,
		ILU_ERRS((no_memory, internal, no_resources)) * err)
{
  SUNRPCRMParms   p = SUNRPCRMPARMS(transport_data(self));
  if (tih != NULLFN && self->tr_inLimit < p->inLength) {
    (*tih) (tih_rock);
    ILU_CLER(*err);
    return FALSE;
  }
  return ilu_SetTransportInputHandler(p->lower, tih, tih_rock, err);
}

/*Main Invariant holds; L2 not further constrained*/

static          ilu_boolean
_sunrpcrm_WaitForInput(ilu_Transport self, ilu_FineTime * limit,
		       ILU_ERRS((interrupted)) * err)
{
  SUNRPCRMParms   p = SUNRPCRMPARMS(transport_data(self));
  if (self->tr_inNext + 4 <= p->inLength
      || p->busyIn && (self->tr_inNext < self->tr_inLimit
		       || p->lastIn && p->inChunkRem == 0))
    return ILU_CLER(*err);
  return (transport_wait_for_input(p->lower, limit, err));
}

static          ilu_boolean
_sunrpcrm_Interrupt(ilu_Transport self, ILU_ERRS((bad_param)) * err)
{
  SUNRPCRMParms   p = SUNRPCRMPARMS(transport_data(self));
  return (transport_interrupt(p->lower, err));
}

/* L2, Main, unconstrained; L1_sup < trmu */

static          ilu_integer
_sunrpcrm_FdUsage(ilu_TransportCreator self, ilu_boolean mooring)
{
  CreatorParms    cp = (CreatorParms) self->tcr_data;
  return (*cp->lower->tcr_dfd) (cp->lower, mooring);
}

static          ilu_integer
_sunrpcrm_CloseDFd(ilu_Transport self)
{
  SUNRPCRMParms   p = SUNRPCRMPARMS(transport_data(self));
  return ((*p->lower->tr_class->tc_closeDFd) (p->lower));
}

/*Main Invariant holds*/
/*L2 >= {conn's iomu}*/

static          ilu_cardinal
ReadWork(ilu_Transport self,
	 ilu_bytes buffer,
	 ilu_cardinal len,
	 ilu_TransportReport * rpt,
	 ILU_ERRS((IoErrs)) * err);

static          ilu_ReadHeaderResultCode
_sunrpcrm_BeginMessage(ilu_Transport self,
		       ilu_boolean input_p,
		       ILU_ERRS((IoErrs)) * err)
{
  SUNRPCRMParms   p = SUNRPCRMPARMS(transport_data(self));
  DEBUG(SUNRPCRM_DEBUG,
	(stderr, "sunrpcrm(%p): BeginMessage(%s)\n",
	 self, input_p ? "input" : "output"));
  if (p->busyIn || p->busyOut)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_beginMessage,
			 ilu_rhrc_error);
  p->dirIsIn = input_p;
  if (input_p) {
    if (p->inWedged)
      return ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_conn_lost,
			   ilu_rhrc_error);
    p->busyIn = p->firstIn = TRUE;
    p->lastIn = FALSE;
    if (self->tr_inNext == p->inLength) {
      ilu_TransportReport rpt;
      (void) ReadWork(self, NIL, 0, &rpt, err);
      if (ILU_ERRNOK(*err)) {
	p->inWedged = TRUE;
	p->busyIn = FALSE;
	return ilu_rhrc_error;
      }
      if (p->inLength < 4 && rpt.tr_eof) {
	p->inWedged = TRUE;
	p->busyIn = FALSE;
	return ilu_rhrc_eof;
      }
      if (p->inLength == 0) {
	_ilu_Assert(p->inChunkRem == 0 && self->tr_inLimit == p->inLength,
		    "sunrpcrm.c:BeginMessage");
	p->busyIn = FALSE; 
	return ilu_rhrc_nothing;
      }
    }
  } else {
    p->busyOut = TRUE;
    p->outStart = self->tr_outNext += 4;
    self->tr_outLimit = p->outSize - 4;
  }
  ILU_CLER(*err);
  return ilu_rhrc_ok;
}

#define FORMAT_HEADER(where,size,eom)		\
(						\
 (where)[0] = (((size) >> 24) & 0xFF) | ((eom) ? 0x80 : 0),	\
 (where)[1] = (((size) >> 16) & 0xFF),		\
 (where)[2] = (((size) >> 8) & 0xFF),		\
 (where)[3] = ((size) & 0xFF)			\
)

static          ilu_boolean
_sunrpcrm_EndMessage(ilu_Transport self,
		     ilu_boolean flush,
		     ilu_Message * msgh,
		     ILU_ERRS((IoErrs)) * err)
{
  register SUNRPCRMParms p = SUNRPCRMPARMS(transport_data(self));
  if (p->busyOut) {
    ilu_cardinal    n1 = self->tr_outNext - p->outStart;
    ilu_boolean     ans = TRUE;
    DEBUG(SUNRPCRM_DEBUG,
	  (stderr,
	   "sunrpcrm(%p): EndMessage(output, flush=%d, last chunkSize=%lu)\n",
	   self, (flush != 0), (long unsigned) (n1)));
    FORMAT_HEADER(self->tr_outBuff + p->outStart - 4, n1, TRUE);
    p->busyOut = FALSE;
    if (flush || self->tr_outNext + 8 > p->outSize) {
      ans = ((*p->lower->tr_class->tc_write_bytes)
	     (p->lower, self->tr_outBuff, self->tr_outNext, flush,
	      err));
      self->tr_outNext = 0;
    }
    self->tr_outLimit = self->tr_outNext;
    return ans;
  } else if (p->busyIn) {
    ilu_boolean     first = TRUE, drops = FALSE;
    ilu_TransportReport rpt = {FALSE, FALSE};
    DEBUG(SUNRPCRM_DEBUG,
	  (stderr, "sunrpcrm(%p): EndMessage(input)\n", self));
    while (!((p->lastIn && p->inChunkRem == 0) || rpt.tr_eof)) {
      ilu_cardinal    lread;
      drops = (drops || p->inChunkRem != 0 ||
	       self->tr_inLimit != self->tr_inNext);
      self->tr_inNext = self->tr_inLimit;
      if (p->inChunkRem > 0 || p->inLength < self->tr_inNext + 4) {
	if (!(first || transport_wait_for_input(p->lower, NIL, err)))
	  goto wedge;
	first = FALSE;
      }
      lread = ReadWork(self, NIL, 0, &rpt, err);
      if (ILU_ERRNOK(*err))
	goto wedge;
    }
    drops = drops || self->tr_inLimit != self->tr_inNext;
    self->tr_inNext = self->tr_inLimit;
    p->busyIn = FALSE;
    if (drops)
      return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcBytesDropped,
			   FALSE);
    return ILU_CLER(*err);
wedge:
    p->inWedged = TRUE;
    return (p->busyIn = FALSE);
  } else
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_endMessage,
			 FALSE);
}

static          ilu_boolean
_sunrpcrm_WriteBytes(ilu_Transport self, ilu_bytes b,
		     ilu_cardinal bufferSize,
		     ilu_boolean flush,
		     ILU_ERRS((IoErrs)) * err)
{
  SUNRPCRMParms   p = SUNRPCRMPARMS(transport_data(self));
  ilu_Transport   lt = p->lower;
  ilu_cardinal    rem = self->tr_outLimit - self->tr_outNext;
  ilu_boolean     direct;
  if (!p->busyOut)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_bytesWithoutMsg,
			 FALSE);
  direct = (bufferSize >= DIRECT_THRESHOLD);
  if (direct) {
    ilu_cardinal    l1 = self->tr_outNext - p->outStart;
    if (l1 > 0) {
      FORMAT_HEADER(self->tr_outBuff + p->outStart - 4, l1, FALSE);
      FORMAT_HEADER(self->tr_outBuff + self->tr_outNext, bufferSize,
		    FALSE);
      self->tr_outNext += 4;
      DEBUG(SUNRPCRM_DEBUG,
	    (stderr,
	     "sunrpcrm(%p): Writing length %lu chunk, plus header for length %lu.\n",
	     self, l1, bufferSize));
    } else {
      /* current chunk empty --- hijack its header */
      FORMAT_HEADER(self->tr_outBuff + p->outStart - 4, bufferSize,
		    FALSE);
      DEBUG(SUNRPCRM_DEBUG,
	    (stderr,
	     "sunrpcrm(%p): Writing header for length %lu chunk.\n",
	     self, bufferSize));
    }
    if (!transport_write_bytes(lt, self->tr_outBuff,
			       self->tr_outNext, err))
      return FALSE;
    DEBUG(SUNRPCRM_DEBUG,
	  (stderr,
	   "sunrpcrm(%p): Writing length %lu chunk body.\n",
	   self, bufferSize));
    if (!transport_write_bytes(lt, b, bufferSize, err))
      return FALSE;
    self->tr_outNext = p->outStart = 4;
  } else {
    ilu_cardinal    l1 = MIN(rem, bufferSize);
    if (l1 > 0)
      memcpy((void *) (self->tr_outBuff + self->tr_outNext),
	     (void *) b, l1);
    self->tr_outNext += l1;
    b += l1;
    bufferSize -= l1;
    if (bufferSize > 0 || (self->tr_outLimit - self->tr_outNext) < 16) {
      /* Gotta write buffer to make room for more, or to make sure we
	 always have room for 16 more bytes in the buffer. */
      /* Possible improvement: avoid writing 0-length chunk here. */
      FORMAT_HEADER(self->tr_outBuff + p->outStart - 4,
		    self->tr_outNext - p->outStart, FALSE);
      DEBUG(SUNRPCRM_DEBUG,
	    (stderr,
	     "sunrpcrm(%p): Writing length %lu chunk.\n",
	     self, self->tr_outNext - p->outStart));
      if (!transport_write_bytes(lt, self->tr_outBuff,
				 self->tr_outNext, err))
	return FALSE;
      p->outStart = 4;
      memcpy((void *) (self->tr_outBuff + 4), (void *) b, bufferSize);
      self->tr_outNext = 4 + bufferSize;
    }
  }
  _ilu_Assert((self->tr_outLimit - self->tr_outNext)>=16,
	      "_sunrpcrm_WriteBytes failed to provide at least 16 bytes in output buffer");
  return ILU_CLER(*err);
}

static          ilu_boolean
_sunrpcrm_SendWholeMessage(ilu_Transport self, ilu_Message * msgh,
			   ILU_ERRS((IoErrs)) * err)
{
  return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcReliable, FALSE);
}

#define PARSE_HEADER(ptr,last,size)				\
	(last = ((ptr)[0] & 0x80) != 0,				\
	 size = ((((ptr)[0] & 0x7F) << 24) + ((ptr)[1] << 16)	\
		 + ((ptr)[2] << 8) + (ptr)[3]))

static          ilu_cardinal
_sunrpcrm_ReadBytes(ilu_Transport self,
		    ilu_bytes buffer,
		    ilu_cardinal len,
		    ilu_TransportReport * rpt,
		    ILU_ERRS((IoErrs)) * err)
{
  return ReadWork(self, buffer, len, rpt, err);
}

static          ilu_cardinal
ReadWork(ilu_Transport self,
	 ilu_bytes buffer,
	 ilu_cardinal len,
	 ilu_TransportReport * rpt,
	 ILU_ERRS((IoErrs)) * err)
{
  SUNRPCRMParms   p = SUNRPCRMPARMS(transport_data(self));
  ilu_cardinal    rem = self->tr_inLimit - self->tr_inNext;
  if (!p->busyIn)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_bytesWithoutMsg,
			 0);
  if (rem > 0)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcInputSkipsBuff,
			 0);
  rpt->tr_eom = rpt->tr_eof = FALSE;
  if (p->lastIn && p->inChunkRem == 0) {
    rpt->tr_eom = TRUE;
    return 0;
  }
  if (p->inChunkRem > 0) {
    /* Chunk not done; read more of it. */
    ilu_cardinal    lread;
    self->tr_inNext = 0;
    lread = transport_read_upto_bytes(p->lower, self->tr_inBuff,
				      p->inSize, rpt, err);
    DEBUG(SUNRPCRM_DEBUG,
	  (stderr,
      "sunrpcrm(%p): Read %lu more bytes, out of %lu remaining.\n",
	   self, lread, p->inChunkRem));
    p->inLength = lread;
    p->inChunkRem -= (self->tr_inLimit = MIN(lread, p->inChunkRem));
    if (ILU_ERRNOK(*err))
      return (buffer ? 0 : lread);
    if (rpt->tr_eof && self->tr_inLimit == p->inLength) {
      rpt->tr_eom = TRUE;
      DEBUG(SUNRPCRM_DEBUG,
	    (stderr,
	     (p->inChunkRem == 0 && p->lastIn)
	     ? "sunrpcrm(%p): chunk ends cleanly after remainder.\n"
	     : "sunrpcrm(%p): unexpected EOF in chunk remainder!\n",
	     self));
      return (buffer ? 0 : lread);
    } else
      rpt->tr_eof = FALSE;
  } else {
    /* !p->lastIn; read another chunk. */
    ilu_cardinal    csize, cbsize;
    if (self->tr_inNext + 4 <= p->inLength) {
      /* Chunk's header already in buffer. */
      csize = 1;		/* a place to set breakpoints */
    } else {
      /* Shift header fragment to buffer start, then read more */
      ilu_cardinal    l1 = p->inLength - self->tr_inNext;
      ilu_cardinal    lread, l2 = p->inSize - l1;
      memcpy((void *) self->tr_inBuff,
	     (void *) (self->tr_inBuff + self->tr_inNext), l1);
      self->tr_inNext = self->tr_inLimit = 0;
      p->inLength = l1;
      lread = transport_read_upto_bytes(p->lower,
				   (void *) (self->tr_inBuff + l1),
					l2, rpt, err);
      DEBUG(SUNRPCRM_DEBUG,
	    (stderr,
	     "sunrpcrm(%p): Read %lu more bytes, in addition to %lu previous, of next chunk (incl header).\n",
	     self, lread, l1));
      p->inLength = l1 + lread;
      if (ILU_ERRNOK(*err))
	return 0;
      if (l1 + lread == 0 && rpt->tr_eof && p->firstIn) {
	/* clean EOF */
	DEBUG(SUNRPCRM_DEBUG,
	      (stderr,
	       "sunrpcrm(%p): No more messages incoming.\n", self));
	rpt->tr_eom = TRUE;
	return 0;
      }
      if (l1 + lread < 4 && rpt->tr_eof) {	/* EOF */
	rpt->tr_eom = TRUE;
	DEBUG(SUNRPCRM_DEBUG,
	      (stderr,
	       "sunrpcrm(%p): EOF while reading header.\n", self));
	return 0;
      }
      rpt->tr_eof = FALSE;
      /* If the EOF is real, it'll happen again. */
    }
    if (self->tr_inNext + 4 <= p->inLength) {
      PARSE_HEADER(self->tr_inBuff + self->tr_inNext, p->lastIn, csize);
      p->firstIn = FALSE;
      self->tr_inNext += 4;
      cbsize = MIN(p->inLength - self->tr_inNext, csize);
      self->tr_inLimit = self->tr_inNext + cbsize;
      p->inChunkRem = csize - cbsize;
      DEBUG(SUNRPCRM_DEBUG,
	    (stderr,
	     "sunrpcrm(%p): Parse header of length %lu chunk (last=%d), %lu in buffer.\n",
	     self, csize, p->lastIn != 0, cbsize));
    }
  }
  /* Now use what's in tr_inBuff */
  rem = self->tr_inLimit - self->tr_inNext;
  if (buffer == NIL)
    return rem;
  if (rem > 0) {
    ilu_cardinal    l1 = MIN(rem, len);
    memcpy((void *) buffer,
	   (void *) (self->tr_inBuff + self->tr_inNext),
	   l1);
    self->tr_inNext += l1;
    return l1;
  }
  return 0;
}

/*L1, L2, Main unconstrained*/

/*L2 >= {conn's iomu}*/

static          ilu_boolean
_sunrpcrm_Close(ilu_Transport self, ilu_integer * dfd,
		ILU_ERRS((internal)) * err)
{
  SUNRPCRMParms   p = SUNRPCRMPARMS(transport_data(self));
  ilu_Transport   lt = p->lower;
  DEBUG(SUNRPCRM_DEBUG,
	(stderr, "sunrpcrm(%p): Close(lower=%p)\n", self, lt));
  ilu_free(self->tr_inBuff);
  ilu_free(self->tr_outBuff);
  ilu_free(p);
  ilu_free(self);
  return transport_close(lt, dfd, err);
}

static struct _ilu_TransportClass_s myclass = {
  ilu_TRUE,			/* boundaried */
  ilu_TRUE,			/* reliable */
  _sunrpcrm_CloseDFd,
  SetInputHandler,
  _sunrpcrm_WaitForInput,
  _sunrpcrm_Interrupt,
  _sunrpcrm_BeginMessage,
  _sunrpcrm_EndMessage,
  _sunrpcrm_SendWholeMessage,
  _sunrpcrm_WriteBytes,
  _sunrpcrm_ReadBytes,
  _sunrpcrm_Close
};

static          ilu_Transport
NewTrans(ilu_Transport lower, ILU_ERRS((no_memory)) * err)
{
  ilu_Transport   ans;
  SUNRPCRMParms   parms = (SUNRPCRMParms) ilu_malloc(sizeof(*parms));
  if (parms == NIL)
    return ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(*parms), NIL);
  parms->busyIn = parms->busyOut = parms->inWedged = FALSE;
  parms->inLength = parms->inChunkRem = 0;
  parms->inSize = parms->outSize = BUFFERSIZE;
  parms->lower = lower;
  ans = (ilu_Transport) ilu_malloc(sizeof(*ans));
  if (ans == NIL)
    return ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(*ans), NIL);
  ans->tr_inBuff = ilu_malloc(parms->inSize);
  ans->tr_outBuff = ilu_malloc(parms->outSize);
  if (ans->tr_inBuff == NIL || ans->tr_outBuff == NIL)
    return ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(*ans), NIL);
  ans->tr_inNext = ans->tr_inLimit = 0;
  ans->tr_outNext = ans->tr_outLimit = 0;
  ans->tr_class = &myclass;
  ans->tr_data = parms;
  DEBUG(SUNRPCRM_DEBUG,
	(stderr, "sunrpcrm:NewTrans(lower=%p) = %p\n",
	 lower, ans));
  ILU_CLER(*err);
  return ans;
}

static          ilu_Transport
_sunrpcrm_CreateTransport(ilu_TransportCreator self, ilu_boolean buffer,
			  ilu_integer *dfd, ilu_Passport pp,
			  ILU_ERRS((IoErrs)) * err)
{
  CreatorParms    cp = (CreatorParms) self->tcr_data;
  ilu_Transport   lower;
  lower = (*cp->lower->tcr_createTransport) (cp->lower, FALSE, dfd, pp, err);
  if (ILU_ERRNOK(*err))
    return NIL;
  if (lower->tr_class->tc_boundaried || !transport_reliable(lower))
    /* He promised! (We checked earlier) */
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcBug, NIL);
  return NewTrans(lower, err);
}

/*L1.sup < trmu; L2 unconstrained*/
static ilu_integer MooringDFd(ilu_Mooring self, ilu_boolean add)
{
  ilu_Mooring     lm = (ilu_Mooring) self->mo_data;
  return ((*lm->mo_dfd) (lm, add));
}

/*L1, L2 unconstrained*/
static          ilu_boolean
SetReqHandler(ilu_Mooring self, ilu_Server s,
	      ilu_TransportInputHandler tih, ilu_refany tih_rock,
	      ILU_ERRS((no_memory, imp_limit, no_resources,
			broken_locks, internal)) * err)
{
  ilu_Mooring     lm = (ilu_Mooring) self->mo_data;
  return ((*lm->mo_set_req_handler) (lm, s, tih, tih_rock, err));
}

/*Before: L1 = {s},
          forall conn: (L2 >= {conn.iomu}) => (L2 >= {conn.callmu});
  After: Main Invariant holds*/
static          ilu_boolean
WaitForReq(ilu_Mooring self, ilu_Server s,
	   ILU_ERRS((broken_locks)) * err)
{
  ilu_Mooring     lm = (ilu_Mooring) self->mo_data;
  return ((*lm->mo_wait_for_req) (lm, s, err));
}

/*L1, L2, Main unconstrained*/

/*L1_sup < trmu*/
static          ilu_Transport
_sunrpcrm_AcceptClient(ilu_Mooring self, ilu_string * tinfo_out,
		       ilu_integer *dfd, ilu_Passport pp,
		       ILU_ERRS((IoErrs)) * err)
{
  ilu_Mooring     lm = (ilu_Mooring) self->mo_data;
  ilu_Transport   lower;
  ilu_string      subtinfo;
  lower = ((*lm->mo_accept_connection)
	   (lm, tinfo_out ? &subtinfo : NIL, dfd, pp, err));
  if (ILU_ERRNOK(*err) || lower == NIL)
    return NIL;
  if (tinfo_out) {
    *tinfo_out = _ilu_Strcat3("sunrpcrm", " over ", subtinfo);
    if (*tinfo_out == NIL)
      return ILU_ERR_CONS1(no_memory, err, nbytes,
			   10 + strlen(subtinfo), NIL);
  }
  return NewTrans(lower, err);
}

static          ilu_boolean
_sunrpcrm_CloseMooring(ilu_Mooring self, ilu_integer *dfd,
		       ILU_ERRS((internal)) * err)
{
  ilu_Mooring     lower = (ilu_Mooring) self->mo_data;
  DEBUG(SUNRPCRM_DEBUG,
	(stderr, "sunrpcrm(%p): CloseMooring(lower=%p)\n",
	 self, lower));
  if (!(*lower->mo_close) (lower, dfd, err))
    return FALSE;
  ilu_free(self);
  return TRUE;
}

/*L1, L2 unconstrained*/
static struct _ilu_Mooring_s mooringProto = {
  MooringDFd,
  SetReqHandler,
  WaitForReq,
  _sunrpcrm_AcceptClient,
  _sunrpcrm_CloseMooring,
  NIL				/* data */
};

/*L1_sup < trmu*/
static          ilu_Mooring
_sunrpcrm_CreateMooring(ilu_TransportCreator self,
			ilu_TransportInfo * tinfo_out,
			ilu_boolean buffer,
			ilu_integer *dfd,
			ilu_Passport pp,	/* unused in this transport */
			ILU_ERRS((no_memory)) * err)
{
  CreatorParms    cp = (CreatorParms) self->tcr_data;
  ilu_Mooring     lower, ans;
  ilu_TransportInfo      subtinfo = NIL;
  lower = ((*cp->lower->tcr_createMooring)
	   (cp->lower, tinfo_out ? &subtinfo : NIL, FALSE, dfd, pp, err));
  if (ILU_ERRNOK(*err))
    return NIL;
  ans = (ilu_Mooring) ilu_malloc(sizeof(*ans));
  if (ans == NIL)
    return (ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(*ans), NIL));
  if (tinfo_out) {
    *tinfo_out = _ilu_ConcatTinfo ("sunrpcrm", subtinfo, err);
    if (ILU_ERRNOK(*err))
      return NIL;
    else
      ilu_free(subtinfo);
  }
  DEBUG(SUNRPCRM_DEBUG,
	(stderr, "sunrpcrm(%p): CreateMooring(lower=%p)\n",
	 ans, lower));
  *ans = mooringProto;
  ans->mo_data = lower;
  return (ans);
}

/*L1, L2 unconstrained*/

static void _sunrpcrm_CloseCreator(ilu_TransportCreator self)
{
  CreatorParms    cp = (CreatorParms) self->tcr_data;
  (*cp->lower->tcr_close) (cp->lower);
  ilu_free(cp);
  ilu_free(self);
  return;
}

static struct _ilu_TransportCreator_s myCreatorProto = {
  TRUE,				/* boundaried */
  TRUE,				/* reliable */
  _sunrpcrm_FdUsage,
  _sunrpcrm_CreateTransport,
  _sunrpcrm_CreateMooring,
  _sunrpcrm_CloseCreator,
  NIL				/* data */
};

/*L1_sup < trmu*/
ilu_TransportCreator
_ilu_sunrpcrm_TransportCreator(ilu_TransportInfo tinfo,
			       ILU_ERRS((no_memory,
					 inv_objref)) * err)
{
  ilu_TransportCreator ans;
  CreatorParms    cp;
  cp = _sunrpcrm_InterpretInfo(tinfo, err);
  if (ILU_ERRNOK(*err))
    return NIL;
  ans = (ilu_TransportCreator) ilu_malloc(sizeof(*ans));
  if (ans == NIL)
    return ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(*ans), NIL);
  *ans = myCreatorProto;
  ans->tcr_data = cp;
  ILU_CLER(*err);
  return ans;
}

