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
/* $Id: inmem.c,v 1.27 1996/05/29 03:34:30 janssen Exp $ */
/* Last edited by Mike Spreitzer March 12, 1996 9:38 am PST */

/* In-memory Transport
   The in-memory transport is a boundaried, reliable transport
   which buffers a request or reply.
   Its transport info begins "inmem_".
 */

/* place includes here */
#include "iluxport.h"
#include "iluntrnl.h"
#include "mooring.h"
#include "transprt.h"

/* place #defines here */
#define BUFSIZE 4096 /* Stab in the dark.  Doubled if too small.  */

typedef struct BufferList_s {
  ilu_bytes buffer;
  ilu_cardinal offset; /* valid data at [0, offset),
			  unused space at [offset, size). */
  ilu_cardinal size; /* change if buffer redimmed. */
  struct BufferList_s * next;
} * BufferList;

/* Shared between the two transports at the "ends" of the connection */
typedef struct SharedData_s {
  /* L2 >= {callmu, iomu of outgoing transport's conn} */
  
  BufferList sd_request; /* next is NIL */

  /* There may be multiple outstanding replies. */
  /* -Tail is NIL exactly when -Head is NIL. */
  BufferList sd_repliesHead; /* Take off from */
  BufferList sd_repliesTail; /* Put on to */

  /* Messages that have been read. */
  BufferList sd_reuseHead; 
  BufferList sd_reuseTail;

  /* Input handler of incoming side Transport */
  ilu_TIH        *sd_tih;    /* tihSet => tih != NIL */
  ilu_boolean     sd_tihSet; /* (if FALSE, sd_tih not meaningful.) */

  ilu_cardinal sd_bufferSize; /* Used to allocate buffers.
			Possibly read from creator's tinfo.  */
  
} *SharedData;


typedef struct {
  /* Connection request handler. */
  ilu_TIH        *md_tih;		/* !=NIL when req handler reg'd */
  SharedData      md_sharedData;   /* NIL outside mutex. */
  ilu_boolean     md_locked; /* stand-in for a mutex */
}              *MooringParms;


typedef struct {
  MooringParms cd_mooringData; /* NIL until mooring is created */
  ilu_TransportInfo cd_tinfo;   /* the tinfo passed at creator creation time */
}              *CreatorParms;



typedef struct {

  ilu_boolean     td_working; /* between Begin- and EndMessage */
  ilu_boolean     td_input; /* vs. output.  Meaningful if working. */

  ilu_boolean     td_outgoing; /* vs. incoming. */

#if 0
  MooringParms    td_mooringData;  /* Address of mooring parms */
#endif

  SharedData      td_sharedData; /* shared with "other end" transport */
  BufferList      td_current; /* NIL if !working */

}              *TransportParms;

/*********************************************
  Methods of the in-memory transport class

  tc_closeDFd
  tc_set_input_handler
  tc_wait_for_input
  tc_interrupt
  tc_begin_message
  tc_end_message
  tc_send_whole_message
  tc_write_bytes
  tc_read_bytes
  tc_close

  *********************************************/

/*L1, L2 unconstrained*/

/*Main Invariant holds; L2 no further constrained*/
static          ilu_boolean
_inmem_Interrupt(ilu_Transport self, ILU_ERRS((bad_param)) * err)
{
  return ILU_CLER(*err);
}

static          ilu_integer
_inmem_CloseDFd(ilu_Transport self)
{
  return (0);
}


/*Main Invariant holds; L2 disjoint {conn's iomu, callmu}*/
static          ilu_boolean
_inmem_SetInputHandler(ilu_Transport self,
		     ilu_TransportInputHandler tih_proc,
		     ilu_refany tih_rock,
		     ILU_ERRS((no_memory, no_resources)) * err)
{
  TransportParms  tp = (TransportParms) self->tr_data;
  SharedData      sd = tp->td_sharedData;
  ilu_TIH        *tih = sd->sd_tih;

  if (tp->td_outgoing)
    /* Don't bother, it's just ReadExtraMsg */
    return ILU_CLER(*err);
  sd->sd_tihSet = FALSE;
  if (tih_proc != NULLFN) {
    if (tih == NIL) {
      tih = (ilu_TIH *) ilu_malloc(sizeof(*tih));
      if (tih == NIL)
	return ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(*tih), FALSE);
      sd->sd_tih = tih;
    }
    tih->tih_proc = tih_proc;
    tih->tih_rock = tih_rock;
    sd->sd_tihSet = TRUE;
  }
  return ILU_CLER(*err);
}

/*Main Invariant holds*/
/*L2 >= {conn's iomu}*/

/*
 * Should only be called when message is finished and waiting in
 * shared data space, and control has passed from message writer to
 * message reader.
 */
static          ilu_boolean
_inmem_WaitForInput(ilu_Transport t, ilu_FineTime * limit,
		  ILU_ERRS((interrupted)) * err)
{
  return ILU_CLER(*err);
}


static BufferList
  NewBufferList (ilu_cardinal size, ilu_bytes buffer, ILU_ERRS((IoErrs)) * err)
{
 BufferList b = (BufferList) ilu_malloc(sizeof(struct BufferList_s));
 if (b == NIL)
   return ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(struct BufferList_s), NIL);
 if (buffer == NIL)
   {
     b->buffer = (ilu_bytes) ilu_malloc( size );
     if (b->buffer == NIL)
       return ILU_ERR_CONS1(no_memory, err, nbytes, size, NIL);
   }
 else
   b->buffer = buffer;
 b->offset = 0;
 b->size = size;
 b->next = NIL;
 ILU_CLER(*err);
 return b;
}

  /*Main Invariant holds; L2 >= {conn's iomu}*/

/*
 * The inmemory transport is boundaried.
 * Begin- and EndMessage should be used.
 */

static          ilu_ReadHeaderResultCode
_inmem_BeginMessage(ilu_Transport self,
		  ilu_boolean input_p,
		  ILU_ERRS((IoErrs)) * err)
{
  TransportParms tp = (TransportParms) self->tr_data;
  SharedData sd = tp->td_sharedData;

  DEBUG(INMEM_DEBUG,
	(stderr, "inmem(%p,%p): BeginMessage(%s).\n",
	 self, sd, input_p ? "input" : "output"));
  
  /* Already began a message? */
  if (tp->td_working)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_beginMessage,
			 ilu_rhrc_error);
  tp->td_working = TRUE;
  
  if (input_p)  {
    tp->td_input = TRUE;
    if (tp->td_outgoing) {
      if (sd->sd_repliesHead == NIL) {
	DEBUG(INMEM_DEBUG,
	      (stderr,
	       "inmem(%p,%p): BeginMessage(outgoing input) found nothing.\n",
	       self, sd));
	tp->td_working = FALSE;
	return ilu_rhrc_nothing;
      }
      tp->td_current = sd->sd_repliesHead; /* read a reply */
      sd->sd_repliesHead = tp->td_current->next;
      if (sd->sd_repliesHead == NIL)
	sd->sd_repliesTail = NIL;
    } else {
      if (sd->sd_request == NIL) {
	DEBUG(INMEM_DEBUG,
	      (stderr,
	       "inmem(%p,%p): BeginMessage(incoming input) found nothing.\n",
	       self, sd));
	tp->td_working = FALSE;
	return ilu_rhrc_nothing;
      }
      tp->td_current = sd->sd_request;   /* read the request */
      sd->sd_request = NIL;
    }
    tp->td_current->next = NIL;
    /* Make read buffer visible */
    self->tr_inBuff = tp->td_current->buffer;
    self->tr_inNext = 0;
    self->tr_inLimit = tp->td_current->offset;
  } else {
    tp->td_input = FALSE;
    /* Recycle a buffer, if possible.
       Else allocate a new one. */
    if (sd->sd_reuseHead != NIL) {
      tp->td_current = sd->sd_reuseHead;
      sd->sd_reuseHead = tp->td_current->next;
      if (sd->sd_reuseHead == NIL)
	sd->sd_reuseTail = NIL;
    } else {
      tp->td_current = NewBufferList(sd->sd_bufferSize, NIL, err);
      if (ILU_ERRNOK(*err))
	return ilu_rhrc_error;
    }
    tp->td_current->next = NIL;
    /* Make write buffer visible */
    self->tr_outBuff = tp->td_current->buffer;
    self->tr_outNext = 0;
    self->tr_outLimit = tp->td_current->size;
  }
  ILU_CLER(*err);
  return ilu_rhrc_ok;
}

/* Before: (does not hold mooring mutex)
   After: if flush, holds mooring mutex */
static          ilu_boolean
_inmem_EndMessage(ilu_Transport self,
		ilu_boolean flush, /* ignore if inputting? */
		ilu_Message *msg, /* ignore: reliable. */
		ILU_ERRS((IoErrs)) * err)
{
  TransportParms tp = (TransportParms) self->tr_data;
  SharedData sd = tp->td_sharedData;

  /* Already ended message? */
  if (!tp->td_working)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_endMessage, FALSE);
  tp->td_working = FALSE;
  
  if (tp->td_input) {
    DEBUG(INMEM_DEBUG,
	  (stderr, "inmem(%p,%p): EndMessage(input)\n",
	   self, sd));
    /* Finished reading a message, "throw it away" */
    if (sd->sd_reuseHead == NIL)
      sd->sd_reuseHead = sd->sd_reuseTail = tp->td_current;
    else {
      sd->sd_reuseTail->next = tp->td_current;
      sd->sd_reuseTail = sd->sd_reuseTail->next;
    }
    sd->sd_reuseTail->next = NIL;
    sd->sd_reuseTail->offset = 0;	/* don't mess with ->size */
    self->tr_inBuff = NIL;
    tp->td_current = NIL;
    self->tr_inNext = self->tr_inLimit = 0;
  } else {
    /* Finished writing a message. */
    /* Remember how much was written */
    DEBUG(INMEM_DEBUG,
      (stderr, "inmem(%p,%p): EndMessage(%s, flush=%d, len=%lu)\n",
       self, sd, "output", flush != FALSE,
       (long unsigned) self->tr_outNext));
    tp->td_current->offset = self->tr_outNext;
    if (tp->td_outgoing) {
      /* Store mesage as request */
      _ilu_Assert(sd->sd_request == NIL,
		  "_inmem_EndMessage: Outstanding request");
      /* signal ilu_im_broken? */
      sd->sd_request = tp->td_current;
      /* Call the input handler! */
      if (sd->sd_tih->tih_proc != NULLFN)
	(*sd->sd_tih->tih_proc) (sd->sd_tih->tih_rock);
      /* No return value. */
    } else {
      /* Store message as reply */
      if (sd->sd_repliesHead == NIL)
	sd->sd_repliesHead = sd->sd_repliesTail = tp->td_current;
      else {
	sd->sd_repliesTail->next = tp->td_current;
	sd->sd_repliesTail = sd->sd_repliesTail->next;
      }
    }
  }
  return ILU_CLER(*err);
}

/*
 * The inmemory transport is reliable.  Should not resend.
 */
static          ilu_boolean
_inmem_SendWholeMessage(ilu_Transport self, ilu_Message * msgh,
		      ilu_Error * err)
{
  return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcReliable,
		       FALSE);
}


/*
 * will need some work 
 */
static          ilu_boolean
_inmem_WriteBytes(ilu_Transport self, ilu_bytes buf,
		ilu_cardinal bufferSize,
		ilu_boolean flush, /* ignore */
		ILU_ERRS((IoErrs)) * err)
     /* If reallocating is not desirable, add a continue bit to
	BufferList, so that multiple BL's may represent one message.
	Affects Begin/EndMessage, Read/WriteBytes, others.  */
{

  TransportParms  tp = (TransportParms) self->tr_data;
  ilu_bytes       b;
  ilu_cardinal    more = bufferSize + 16;

  /* Error if not working on a message */
  if (!tp->td_working)
    return ILU_ERR_CONS1(internal, err, minor,
			 ilu_im_bytesWithoutMsg, FALSE);

  if ((self->tr_outNext + bufferSize) >= self->tr_outLimit) {
    tp->td_current->size += MAX(tp->td_current->size, more);
    self->tr_outLimit = tp->td_current->size;
    b = (ilu_bytes) ilu_realloc(self->tr_outBuff, tp->td_current->size);
    if (b == NIL)
      return ILU_ERR_CONS1(no_memory, err, nbytes,
			   tp->td_current->size, FALSE);
    self->tr_outBuff = tp->td_current->buffer = b;
  }
  if (buf != NIL) {
    /* Copy it into the exposed buffer */
    memcpy((void *) (self->tr_outBuff + self->tr_outNext),
	   (void *) buf, (SIZE_T) bufferSize);
    self->tr_outNext += bufferSize;
  }
  return ILU_CLER(*err);
}


static          ilu_cardinal
_inmem_ReadBytes(ilu_Transport self,
	       ilu_bytes buffer,
	       ilu_cardinal len,
	       ilu_TransportReport * rpt,
	       ILU_ERRS((IoErrs)) * err)
{
  
  TransportParms tp = (TransportParms) self->tr_data;

  rpt->tr_eom = rpt->tr_eof = FALSE;
  
  /* Error if not working on a message */
  if (!tp->td_working)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_bytesWithoutMsg, 0);

  /* Error if exposed buffer holds more input. */
  if (self->tr_inNext != self->tr_inLimit)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcInputSkipsBuff,
			 0);

  /* The only place to read from is the exposed buffer itself,
     and since it does not hold more input, this must be EOM. */
  rpt->tr_eom = TRUE;
  ILU_CLER(*err);
  return 0;

  /*
   * "Does not read past message boundary (if boundaried), EOF, or end
   * of given buffer."
   */
  /* if there's an EOF in the buffer, spec breaks.
     (would there ever be?) */

}


/*L1, L2 unconstrained*/
static void CloseSharedData(SharedData sd)
{
  BufferList bl, next;

  if (sd->sd_tih != NIL)
    ilu_free(sd->sd_tih);
  if (sd->sd_request != NIL) {
    ilu_free(sd->sd_request->buffer);
    ilu_free(sd->sd_request);
  }
  if (sd->sd_repliesHead != NIL) {
    for (bl = sd->sd_repliesHead ; bl != NIL ; bl = next)  {
      next = bl->next;
      ilu_free(bl->buffer);
      ilu_free(bl);
    }
  }
  if (sd->sd_reuseHead != NIL)  {
    for (bl = sd->sd_reuseHead ; bl != NIL ; bl = next)  {
      next = bl->next;
      ilu_free(bl->buffer);
      ilu_free(bl);
    }
  }
  ilu_free(sd);
}

/*L1, L2 unconstrained*/
static          ilu_boolean
_inmem_CloseTransport(ilu_Transport self, ilu_integer * dfd,
		    ILU_ERRS((internal)) * err)
{
  /*
   * Free anything that belongs to self. Outgoing transport
   * responsible for shared data.
   */
  TransportParms  tp = (TransportParms) self->tr_data;
  DEBUG(INMEM_DEBUG,
	(stderr, "inmem(%p,%p): Close\n",
	 tp, tp->td_sharedData));
  *dfd = 0;
  if (tp->td_outgoing)
    CloseSharedData(tp->td_sharedData);
  if (tp->td_current != NIL) {
    ilu_free(tp->td_current->buffer);
    ilu_free(tp->td_current);
  }
  ilu_free(tp);
  self->tr_data = NIL;
  ilu_free(self);
  return ILU_CLER(*err);
}

/***********************************
  Methods of the in-memory mooring

  mo_set_req_handler
  mo_wait_for_req
  mo_accept_connection
  mo_close
  
  ***********************************/

/*
 * Set the transport class methods...
 * there is only one memClass.
 */

static struct _ilu_TransportClass_s memClass = {
  ilu_TRUE,			/* boundaried */
  ilu_TRUE,			/* reliable */
  _inmem_CloseDFd,
  _inmem_SetInputHandler,
  _inmem_WaitForInput,
  _inmem_Interrupt,
  _inmem_BeginMessage,
  _inmem_EndMessage,
  _inmem_SendWholeMessage,
  _inmem_WriteBytes,
  _inmem_ReadBytes,
  _inmem_CloseTransport
};

/*L1, L2, Main unconstrained*/

/* L1, L2 unconstrained */
static          ilu_Transport
_inmem_NewT(ILU_ERRS((IoErrs)) * err)
     /* Used for outgoing and incoming transports
	If buffer, allocate a SharedData. */
{
  ilu_Transport   ans;
  TransportParms parms;

  /* Allocate. */
  ans = (ilu_Transport) ilu_malloc(sizeof(*ans));
  if (ans == NIL)
    return ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(*ans), NIL);
  parms = (TransportParms) ilu_malloc(sizeof(*parms));
  if (parms == NIL)
    return ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(*parms), NIL);

  /* Initialize. */
  parms->td_working = FALSE; /* set in Begin/EndMsg */
  parms->td_input = FALSE; /* set in BeginMsg */
  parms->td_outgoing = FALSE; /* set by caller */
  parms->td_sharedData = NIL; /* set by caller */
  parms->td_current = NIL;	/* set in Begin/EndMsg */

  ans->tr_class = &memClass;
  ans->tr_data = parms;

  /* set by _inmem_BeginMessage */
  ans->tr_inBuff = ans->tr_outBuff = NIL;
  ans->tr_inLimit = ans->tr_outLimit = 0;
  ans->tr_inNext = ans->tr_outNext = 0;

  ILU_CLER(*err);
  return ans;
}

/*L1_sup < trmu*/
static          ilu_Transport
_inmem_AcceptClient(ilu_Mooring self, ilu_string * tinfo_out,
		  ilu_integer *dfd, ilu_Passport pp, ILU_ERRS((IoErrs)) * err)
{
  /* Create incoming transport.
     Called by connection request handler. */

  MooringParms    mp = (MooringParms) self->mo_data;
  TransportParms  tp;
  ilu_Transport   ans;
  char buf[32];

  *dfd = 0;
  _ilu_Assert((int) mp->md_locked, "_inmem_AcceptClient");
  /* signal ilu_im_brokenLocks? */
  
  if (tinfo_out != NIL) {
    sprintf(buf, "inmem(sd=%p)", mp->md_sharedData);
    *tinfo_out = _ilu_Strdup(buf);
    if (*tinfo_out == NIL)
      return ILU_ERR_CONS1(no_memory, err, nbytes, 1 + strlen(buf), NIL);
  }
  
  ans = _inmem_NewT(err);
  if (ILU_ERRNOK(*err))
    return NIL;
  
  tp = (TransportParms) ans->tr_data;
  tp->td_outgoing = FALSE;

  /* Connect to outgoing transport */
  tp->td_sharedData = mp->md_sharedData;
  mp->md_sharedData = NIL;
  mp->md_locked = FALSE;
  
  DEBUG(INMEM_DEBUG,
	(stderr,
	 "inmem(%p): AcceptClient => transport %p shared data %p\n",
	 self, ans, tp->td_sharedData));
  
  return ans;
}

static          ilu_boolean
_inmem_CloseMooring(ilu_Mooring self, ilu_integer * dfd,
		  ILU_ERRS((internal)) * err)
{
  MooringParms    mp = (MooringParms) self->mo_data;

  DEBUG(INMEM_DEBUG, (stderr, "inmem(%p): CloseMooring\n", self));

  *dfd = 0;
  if (mp->md_tih != NIL)
    ilu_free(mp->md_tih);
  ilu_free(mp);
  self->mo_data = NIL;
  return ILU_CLER(*err);
}

/*L1.sup < trmu; L2 unconstrained*/
static ilu_integer _inmem_MooringDFd(ilu_Mooring self, ilu_boolean add)
{
  return 0;
}

/*L1, L2 unconstrained*/
static          ilu_boolean
_inmem_SetReqHandler(ilu_Mooring self, ilu_Server s,
		   ilu_TransportInputHandler tih_proc,
		   ilu_refany tih_rock,
		   ILU_ERRS((no_memory, imp_limit, no_resources,
			     broken_locks, internal)) * err)
{
  /* Called only by single threaded RT? ***********************************/

  MooringParms    mp = (MooringParms) self->mo_data;
  ilu_TIH         *tih;
  tih = (ilu_TIH *) ilu_malloc(sizeof(*tih));
  _ilu_Assert(mp->md_tih == NIL, "_inmem_SetReqHandler");      /* signal ilu_im_ ??? */
  if (tih == NIL)
    return ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(*tih), FALSE);
  tih->tih_proc = tih_proc;
  tih->tih_rock = tih_rock;
  mp->md_tih = tih;
  return ILU_CLER(*err);
  
}

/*Before: L1 = {s},
          forall conn: (L2 >= {conn.iomu}) => (L2 >= {conn.callmu});
  After: Main Invariant holds*/
static          ilu_boolean
_inmem_WaitForReq(ilu_Mooring self, ilu_Server s,
		ILU_ERRS((interrupted, broken_locks)) * err)
{
  /*  (Instead of waiting on FD directly.)
   * In a multi-threaded runtime, a thread calls this to wait for
   * the next connection request.  The server mutex is released
   * while this thread is blocked.  Because the server mutex is held
   * at entry, another thread cannot close the Mooring until after
   * the impl releases it.  Returns TRUE after a connection request
   * arrives; may also return TRUE after the Mooring's been closed.
   */

  /* Should not be used for inmem.
     (would be called when new port created?)
   */

  _ilu_Assert(0, "_inmem_WaitForReq");
  /* signal something */
  return FALSE;
  
}



/****************************************************
  Methods of the in-memory transport/mooring creator

  tcr_dfd
  tcr_createTransport
  tcr_createMooring
  tcr_close

  ***************************************************/


/* L1_sup < trmu; L2 unconstrained */
static          ilu_integer
_inmem_FdUsage(ilu_TransportCreator self, ilu_boolean mooring)
{
  return (0);
}

/* Main Invariant holds */
static ilu_Transport
  _inmem_CreateTransport(ilu_TransportCreator self, ilu_boolean buffer,
			 ilu_integer *dfd, ilu_Passport pp, ILU_ERRS((IoErrs)) * err)
{
  /* Creates outgoing transport, makes call to create incoming transport
     and link the two.  (Will allocate buffers even if !buffer.) */
  
  CreatorParms    cp = (CreatorParms) self->tcr_data;
  MooringParms    mp;
  TransportParms  tp;
  ilu_Transport   ans;
  SharedData      sd;
  ilu_Error       lerr;

  *dfd = 0;
  ans = _inmem_NewT(err);
  if (ILU_ERRNOK(*err))
    return NIL;
  /* Exposed buffers are NIL until message begun. */
  
  /* Create the shared data space. */
    
  sd = (SharedData) ilu_malloc(sizeof(struct SharedData_s));
  if (sd == NIL)  {
    _inmem_CloseTransport(ans, dfd, &lerr); /* clean up */
    ILU_HANDLED(lerr);
    return ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(*sd), NIL);
  }
  sd->sd_bufferSize = BUFSIZE; /* could also be contained in tinfo? */
  sd->sd_tih = NIL;
  sd->sd_tihSet = FALSE;
  sd->sd_reuseHead = sd->sd_reuseTail = NIL;

#if 0
/* MJS August 23, 1995.  Don't we want these initialized to NIL,
   to represent the absence of a request and any replies? */

  sd->sd_request = NewBufferList(sd->sd_bufferSize, NIL err);
  if (ILU_ERRNOK(*err)) {
    _inmem_CloseTransport(ans, &lerr); /* clean up */
    return NIL;
  }
  sd->sd_repliesHead = NewBufferList(sd->sd_bufferSize, NIL, err);
  if (ILU_ERRNOK(*err)) {
    _inmem_CloseTransport(ans, &lerr); /* clean up */
    return NIL;
  }
  sd->sd_repliesTail = sd->sd_repliesHead;
#else
  sd->sd_repliesTail = sd->sd_repliesHead = sd->sd_request = NIL;
#endif
  
  DEBUG(INMEM_DEBUG,
	(stderr,
	 "inmem: Create outgoing transport %p sharedData %p\n",
	 ans, sd));
  
  tp = (TransportParms) ans->tr_data;
  tp->td_sharedData = sd;
  tp->td_outgoing = TRUE;
  
  if (cp != NIL)
    {
      mp = cp->cd_mooringData;
      if (mp != NIL)
	{
	  /* Get the mooring mutex.
	     Put pointer to the shared data space in mooring.
	     Call the mooring's registered conn-request handler. */
	  mp->md_locked = TRUE;
	  mp->md_sharedData = sd;
	  (*mp->md_tih->tih_proc) (mp->md_tih->tih_rock);
	  /* No return value.  Must call mo_accept_connection. */
	}
    }
  /* I think that's all. */
  return ans;
}

/*L1, L2 unconstrained*/
static struct _ilu_Mooring_s mooringProto = {
  _inmem_MooringDFd,
  _inmem_SetReqHandler,
  _inmem_WaitForReq,
  _inmem_AcceptClient,
  _inmem_CloseMooring,
  NIL				/* data */
};

/* L1_sup < trmu; L2 unconstrained */
static ilu_Mooring 
_inmem_CreateMooring(ilu_TransportCreator self,
		     ilu_TransportInfo * tinfo_out,
		     ilu_boolean buffer, /* ignored */
		     ilu_integer *dfd,
		     ilu_Passport pp,	/* unused */
		     ILU_ERRS((IoErrs)) * err)
{
  CreatorParms    cp = (CreatorParms) self->tcr_data;
  MooringParms    mp;
  ilu_Mooring     ans;

  ILU_CLER(*err);
  *dfd = 0;

  _ilu_Assert(cp->cd_mooringData == NIL, "_inmem_CreateMooring");
  /* signal something? */
  
  ans = (ilu_Mooring) ilu_malloc(sizeof(*ans));
  if (ans == NIL) {
    return ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(*ans), NIL);
  }
  mp = (MooringParms) ilu_malloc(sizeof(*mp));
  if (mp == NIL) {
    return ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(*mp), NIL);
  }
  mp->md_tih = NIL;
  mp->md_sharedData = NIL;
  mp->md_locked = FALSE;

  *ans = mooringProto;
  ans->mo_data = mp;

  /* create transport info */
  if (tinfo_out != NIL)
    {
      ilu_string t[1] = { NIL };
      *tinfo_out = _ilu_ConcatTinfo("inmem_", t, err);
      if (ILU_ERRNOK(*err))
	{
	  ilu_free(mp);
	  ilu_free(ans);
	  return NIL;
	}
    }

  /* Convey to outgoing transports. */
  cp->cd_mooringData = mp;
  
  DEBUG(INMEM_DEBUG, (stderr, "inmem: CreateMooring %p\n", ans));

  return ans;
  
}

/* L1, L2 unconstrained */
static void 
_inmem_CloseCreator(ilu_TransportCreator self)
{
  CreatorParms    cp = (CreatorParms) self->tcr_data;
  ilu_free(cp);
  ilu_free(self);
  return;
}


/*
 * Set the transport creator methods...
 * There should be one TCr for every inmem port.
 */

static struct _ilu_TransportCreator_s creatorProto = {
  TRUE,				/* boundaried */
  TRUE,				/* reliable */
  _inmem_FdUsage,
  _inmem_CreateTransport,
  _inmem_CreateMooring,
  _inmem_CloseCreator,
  NIL				/* data */
};

/*L1_sup < trmu*/
ilu_TransportCreator
_ilu_inmem_TransportCreator(ilu_TransportInfo tinfo,
			  ILU_ERRS((no_memory, inv_objref)) * err)
{
  ilu_TransportCreator ans;
  CreatorParms    cp;
  cp = (CreatorParms) ilu_malloc(sizeof(*cp));
  if (cp == NIL)
    return ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(*cp), NIL);
  ans = (ilu_TransportCreator) ilu_malloc(sizeof(*ans));
  if (ans == NIL)
    return ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(*ans), NIL);
  *ans = creatorProto;
  ans->tcr_data = cp;
  /* This will be set when the tCr creates a mooring and will not
     be changed after that:  */
  cp->cd_mooringData = NIL;
  /* might as well save it */
  cp->cd_tinfo = _ilu_CopyTinfo(tinfo, err);
  if (ILU_ERRNOK(*err))
    {
      ilu_free(ans);
      ilu_free(cp);
      return NIL;
    }
  ILU_CLER(*err);
  return (ans);
}

/* Main Invariant holds */
ilu_Transport
_ilu_BufferTransport_Create (ilu_cardinal size, ilu_bytes buffer,
			     ILU_ERRS((IoErrs)) * err)
{
  TransportParms  tp;
  ilu_Transport   t;
  SharedData      sd;
  ilu_Error       lerr;
  ilu_integer	  dfd;
  ilu_ReadHeaderResultCode rhrc;

  t = _inmem_NewT(err);
  if (ILU_ERRNOK(*err))
    return NIL;
  /* Exposed buffers are NIL until message begun. */
  
  /* Create the shared data space. */
    
  sd = (SharedData) ilu_malloc(sizeof(struct SharedData_s));
  if (sd == NIL)  {
    _inmem_CloseTransport(t, &dfd, &lerr); /* clean up */
    ILU_HANDLED(lerr);
    return ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(*sd), NIL);
  }
  sd->sd_bufferSize = size; /* could also be contained in tinfo? */
  sd->sd_tih = NIL;
  sd->sd_tihSet = FALSE;
  sd->sd_reuseHead = sd->sd_reuseTail = NIL;
  sd->sd_repliesTail = sd->sd_repliesHead = sd->sd_request = NIL;
  
  DEBUG(INMEM_DEBUG,
	(stderr,
	 "_ilu_BufferTransport: Create outgoing inmem transport %p sharedData %p\n",
	 t, sd));
  
  tp = (TransportParms) t->tr_data;
  tp->td_sharedData = sd;
  tp->td_outgoing = FALSE;
  tp->td_working = FALSE;
  tp->td_current = NIL;

  if (buffer != NIL)
    {
      sd->sd_request = NewBufferList (size, buffer, err);
      if (ILU_ERRNOK(*err))
	{
	  _inmem_CloseTransport (t, &dfd, &lerr);
	  return (NIL);
	}
      sd->sd_request->offset = size;
    }

  rhrc = _inmem_BeginMessage (t, (buffer != NIL), err);
  
  switch (rhrc) {
  case ilu_rhrc_ok:
    break;
  case ilu_rhrc_error:
    _inmem_CloseTransport(t, &dfd, &lerr);	/* clean up */
    ILU_HANDLED(lerr);
    return NIL;
  default:
    _ilu_Assert(FALSE, "inmem.c:BufferTransport");
  }

  return t;
}

/*L1, l2?*/
void
_ilu_BufferTransport_Destroy (ilu_Transport self, ilu_cardinal * size,
			      ilu_bytes * buffer, ILU_ERRS((IoErrs)) * err)
{
  TransportParms  tp;
  ilu_boolean	input;
  BufferList	current;
  ilu_integer	dfd;

  tp = (TransportParms) self->tr_data;
  input = tp->td_input;
  current = tp->td_current;

  _inmem_EndMessage(self, TRUE, NIL, err);
  if (ILU_ERRNOK(*err))
    return;

  if (current != NIL)
    {
      if (size != NIL)
	*size = current->offset;
      if (buffer != NIL) {
	*buffer = current->buffer;
	current->buffer = NIL;
      }
    }

  /* we need to close this, as this transport is always td_outgoing == FALSE */
  CloseSharedData(tp->td_sharedData);
  tp->td_sharedData = NIL;
  tp->td_current = NIL;

  transport_close(self, &dfd, err);
}
