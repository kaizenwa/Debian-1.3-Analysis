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
/* $Id: transprt.c,v 1.60 1996/05/08 00:34:43 mdavidso Exp $ */
/* Last edited by Mike Spreitzer December 7, 1995 9:26 am PST */

#define _POSIX_SOURCE


#include "iluntrnl.h"
#include "transprt.h"

struct transports_s {
  /* L1, L2, Main unconstrained */

  ilu_string      name;
  ilu_TransportInstantiator instantiator;
};

/* L1_sup < trmu; L2 unconstrained */

/*L1, L2, Main unconstrained*/

static struct transports_s transports[MAX_TRANSPORTS] = {
#ifdef UDPSOCKET_TRANSPORT
  { "udp", _ilu_udp_TransportCreator },
#endif /* UDPSOCKET_TRANSPORT */
#ifdef TCPIP_TRANSPORT
  { "tcp", _ilu_tcp_TransportCreator },
#endif /* TCPIP_TRANSPORT */
#ifdef SUNRPCRM_TRANSPORT
  { "sunrpcrm", _ilu_sunrpcrm_TransportCreator },
#endif /* SUNRPCRM_TRANSPORT */
#ifdef W3MUX_TRANSPORT
  { "w3mux", _ilu_w3mux_TransportCreator },
#endif /* W3MUX_TRANSPORT */
#ifdef SECURE_TRANSPORT
  { "security", _ilu_security_TransportCreator },
#endif /* SECURE_TRANSPORT */
  { "inmem", _ilu_inmem_TransportCreator },
  { NIL, NULLFN } };

ILU_ERRS((TransportAlreadyRegistered, MaxCountExceeded))
ilu_RegisterTransport(char *name,
		      ilu_TransportInstantiator new_transport,
		      ilu_boolean override)
{
  int             i;
  ilu_Error       e;

  DEBUG(EXPORT_DEBUG, (stderr, "ilu_RegisterTransport (%s)\n", name));
  for (i = 0; i < MAX_TRANSPORTS && transports[i].name != NIL; i++) {
    if (strcmp(transports[i].name, name) == 0) {
      if (override)
	transports[i].instantiator = new_transport;
      else
	{
	  DEBUG(EXPORT_DEBUG,
		    (stderr, "ilu_RegisterTransport:  \"%s\" already registered.\n",
		     name));
	  return ILU_ERR_CONS3(TransportAlreadyRegistered, &e,
			       name, name,
			       old_transport, transports[i].instantiator,
			       new_transport, new_transport, e);
	}
    }
  }
  if (i < MAX_TRANSPORTS && transports[i].name == NIL) {
    transports[i].name = name;
    transports[i].instantiator = new_transport;
    if ((i + 1) < MAX_TRANSPORTS)
      transports[i + 1].name = NIL;
    return ILU_NO_ERR;
  } else
    {
      DEBUG(EXPORT_DEBUG,
		(stderr, "ilu_RegisterTransport:  too many transports"
		 "(%d) to register \"%s\"\n", MAX_TRANSPORTS, name));
      return ILU_ERR_CONS1(MaxCountExceeded, &e,
			   max_count, MAX_TRANSPORTS, e);
    }
}

static struct transports_s *
  FindTransport(ilu_string tinfo)
{
  ilu_integer     i, l1;
  char           *p;
  if (tinfo == NIL)
    return NIL;
  p = strchr(tinfo, '_');
  if (p == NIL)
    l1 = strlen(tinfo);
  else
    l1 = p - tinfo;
  for (i = 0; transports[i].name != NIL; i += 1)
    if (_ilu_casefree_ncmp(tinfo, transports[i].name, l1) == 0
	&& transports[i].name[l1] == 0)
      {
	DEBUG(CONNECTION_DEBUG,
		  (stderr, "(FindTransport)(%s) => %p\n",
		   tinfo, &transports[i]));
	return (&transports[i]);
      }
  DEBUG(CONNECTION_DEBUG,
	    (stderr, "(FindTransport)(%s) => NIL\n", tinfo));
  return (NIL);
}

/*L1_sup < trmu*/
ilu_TransportCreator
_ilu_GetTransportCreator(ilu_TransportInfo tinfo,
			 ILU_ERRS((no_memory, inv_objref)) * err)
{
  struct transports_s *p;
  ilu_TransportCreator tcr;

  if ((p = FindTransport(tinfo[0])) == NIL) {
    DEBUG(CONNECTION_DEBUG,
	  (stderr,
	   "transprt.c: Unable to find registered transport creator for info \"%s\".\n",
	   tinfo[0]));
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_tc, NIL);
  }
  tcr = (*(p->instantiator)) (tinfo, err);
  DEBUG(CONNECTION_DEBUG, (stderr, "_ilu_GetTransportCreator (%s...) => %p\n",
			   tinfo[0], tcr));
  return tcr;
}

/*Main Invariant holds; L2 disjoint {conn's iomu, callmu}*/
ilu_boolean 
ilu_SetTransportInputHandler(ilu_Transport trans,
			     ilu_TransportInputHandler tih,
			     ilu_refany tih_rock,
			     ILU_ERRS((no_memory, internal,
				       no_resources)) * err)
{
  if (tih != NULLFN && trans->tr_inBuff != NIL &&
      trans->tr_inNext < trans->tr_inLimit) {
    (*tih) (tih_rock);
    return FALSE;
  } else
    return ((*trans->tr_class->tc_set_input_handler)
	    (trans, tih, tih_rock, err));
}

/*Main Invariant holds; L2 >= {conn's I/O mutex}*/
ilu_boolean
_ilu_transportWriteBytes(ilu_Transport self,
			 ilu_bytes buf,
			 ilu_cardinal bufLen,
			 ILU_ERRS((IoErrs)) * err)
{
  ilu_TransportClass tc = self->tr_class;
  return ((*tc->tc_write_bytes) (self, buf, bufLen, FALSE, err));
}

/*Main Invariant holds; L2 >= {conn's I/O mutex}*/
ilu_bytes
_ilu_transportGetOutputBuffer(ilu_Transport self,
			      ilu_cardinal len,
			      ILU_ERRS((IoErrs)) * err)
{
  if (len > 16)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_trBufSize, NIL);
  if (!(*self->tr_class->tc_write_bytes) (self, NIL, 0, FALSE, err))
    return FALSE;
  if (self->tr_outBuff == NIL || self->tr_outNext >= self->tr_outLimit
      || 16 > self->tr_outLimit - self->tr_outNext)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_tcBug, NIL);
  return (self->tr_outBuff + (self->tr_outNext += len) - len);
}


/* ------------------------------------------------------------
transport_read_bytes attempts to read the number of bytes desired
from the specified transport into the specified buffer.
It returns the number of bytes actually read.  The number actually
read may be less than requested if some error occurred during the read,
e.g. end of file, in which case p_error will be set appropriately and
the number of bytes actually read into the buffer will be returned
immediately.

Main Invariant holds; L2 >= {conn's I/O mutex} */

ilu_cardinal
_ilu_transportReadBytes(ilu_Transport p_transport, ilu_bytes pc_buffer,
			ilu_cardinal card_num_desired, ILU_ERRS((IoErrs)) * p_error) {

	ilu_cardinal card_total_read = 0;
	ilu_cardinal card_num_read;
	ilu_TransportReport transport_report;

	ILU_CLER(*p_error);

	/* get what we can out of our buffer */
	if (p_transport->tr_inBuff != NIL) {
		card_total_read = MIN(card_num_desired, p_transport->tr_inLimit - p_transport->tr_inNext);
		memcpy((void *) pc_buffer, (void *) (p_transport->tr_inBuff + p_transport->tr_inNext),
			card_total_read);
		p_transport->tr_inNext += card_total_read;
		pc_buffer += card_total_read;
	}

	/* if we got all we needed from the buffer, just give em back */
	if (card_total_read == card_num_desired)
		return card_total_read;

	while (1) {

	/* read some */
	card_num_read = (*p_transport->tr_class->tc_read_bytes) (p_transport, 
		pc_buffer, 
		card_num_desired - card_total_read, 
		&transport_report, 
		p_error);

	/* adjust to account for what we read */
    pc_buffer += card_num_read;
    card_total_read += card_num_read;

	/* had a problem, just return count of what we got */
    if (ILU_ERRNOK(*p_error)) 
      return card_total_read;

	/* got all that was desired, just return the count */
    if (card_total_read == card_num_desired)
      return card_total_read;

	/* if we hit end of file or end of message, set the error accordingly, and
	   and return how many we got before the error happened */
    if (transport_report.tr_eof)
      return ILU_ERR_CONS1(comm_failure, p_error, minor, ilu_cfm_eof,
			   card_total_read);
    else if (transport_report.tr_eom)
      return ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_eom, card_total_read);

	/* else wait until there's some more available to read, then loop */
    if (!(*p_transport->tr_class->tc_wait_for_input) (p_transport, NIL, p_error))
      return card_total_read;
  }
}


/*Main Invariant holds; L2 >= {conn's I/O mutex}*/
ilu_bytes
_ilu_transportGetInputBuffer(ilu_Transport self,
			     ilu_cardinal len,
			     ILU_ERRS((IoErrs)) * err)
{
  if (len > 16)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_trBufSize, NIL);
  if ((void) transport_read_bytes(self, self->tr_tinBuff, len, err), ILU_ERRNOK(*err))
    return NIL;
  ILU_CLER(*err);
  return self->tr_tinBuff;
}

/*Main Invariant holds; L2 >= {conn's I/O mutex}*/
ilu_cardinal
_ilu_transportReadUpToBytes(ilu_Transport self,
			    ilu_bytes buf,
			    ilu_cardinal len,
			    ilu_TransportReport * rpt,
			    ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    l1 = 0, l2;
  ILU_CLER(*err);
  rpt->tr_eom = rpt->tr_eof = FALSE;
  if (self->tr_inBuff != NIL) {
    l1 = MIN(len, self->tr_inLimit - self->tr_inNext);
    memcpy((void *) buf, (void *) (self->tr_inBuff + self->tr_inNext),
	   l1);
    self->tr_inNext += l1;
    buf += l1;
  }
  if (l1 < len) {
    ilu_cardinal    lreq = len - l1;
    l2 = (*self->tr_class->tc_read_bytes) (self, buf, lreq, rpt, err);
    l1 += l2;
    buf += l2;
    if (ILU_ERRNOK(*err))
      return l1;
  }
  return l1;
}

/*Main Invariant holds; L2 >= {conn's I/O mutex}*/
extern          ilu_boolean
_ilu_transportReadMessage(ilu_Transport self,
			  ilu_bytes * msg,
			  ilu_cardinal * msgLen,
			  ilu_TransportReport * rpt,
			  ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    nread, pos = 0, len = 4096;
  ilu_bytes       buf = NIL;
  rpt->tr_eom = rpt->tr_eof = FALSE;
  DEBUG(CONNECTION_DEBUG, (stderr, "_ilu_transportReadMessage:  reading message from transport <%lx>\n",
			   self));
  while (!(rpt->tr_eom || rpt->tr_eof)) {
    if (buf) {
      len *= 2;
      buf = ilu_realloc(buf, len);
    } else
      buf = ilu_malloc(len);
    if (buf == NIL)
      return ILU_ERR_CONS1(no_memory, err, nbytes, len, FALSE);
    if ((*self->tr_class->tc_wait_for_input) (self, NIL, err), ILU_ERRNOK(*err))
      return FALSE;
    nread = transport_read_upto_bytes(self, buf + pos, len - pos,
				      rpt, err);
    DEBUG(CONNECTION_DEBUG, (stderr, "_ilu_transportReadMessage:  read %lu bytes, eof=%s, eom=%s\n",
			     (unsigned long) nread, rpt->tr_eof ? "true" : "false", rpt->tr_eom ? "true" : "false"));
    if (ILU_ERRNOK(*err))
      {
	ilu_free(buf);
	return FALSE;
      }
    pos += nread;
  }

  DEBUG(CONNECTION_DEBUG, (stderr, "_ilu_transportReadMessage:  returning buf %p of %lu bytes\n",
			   buf, (unsigned long) pos));
  *msg = buf;
  *msgLen = pos;
  return TRUE;
}

ilu_boolean
  _ilu_CompareTinfo (ilu_TransportInfo t1, ilu_TransportInfo t2)
{
  int i;
  for (i = 0;  t1[i] != NIL && t2[i] != NIL;  i++)
    if (strcmp(t1[i], t2[i]) != 0)
      return ilu_FALSE;
  if (t1[i] != NIL || t2[i] != NIL)
    return ilu_FALSE;
  return ilu_TRUE;
}

ilu_TransportInfo
_ilu_CopyTinfo(ilu_TransportInfo tinfo, ILU_ERRS((no_memory)) * err)
{
  int             count;
  int             i, sz = 0;
  ilu_string      s;
  ilu_TransportInfo ans;

  for (s = tinfo[count = 0]; s != NIL; s = tinfo[++count])
    sz += strlen(s) + 1;
  sz += (sizeof(ilu_string) * (count + 1));
  if ((ans = (ilu_TransportInfo) ilu_MallocE(sz, err)) == NIL)
    return NIL;
  s = (char *) (ans + count + 1);
  for (i = 0; i < count; i++) {
    ans[i] = s;
    strcpy(ans[i], tinfo[i]);
    s += strlen(tinfo[i]) + 1;
  }
  ans[count] = NIL;
  ILU_CLER(*err);
  return (ans);
}

ilu_TransportInfo
_ilu_ConcatTinfo(ilu_string info, ilu_TransportInfo tinfo, ILU_ERRS((no_memory)) * err)
{
  ilu_cardinal    len, count;
  ilu_TransportInfo ans;
  ilu_string      p;

  for (len = strlen(info) + 1, count = 0; tinfo[count] != NIL; count++)
    len += (strlen(tinfo[count]) + 1);
  len += (sizeof(ilu_string) * (count + 2));
  if ((ans = (ilu_TransportInfo) ilu_MallocE(len, err)) == NIL)
    return NIL;
  ans[0] = p = (char *) (ans + count + 2);
  strcpy(ans[0], info);
  p += strlen(info) + 1;
  for (len = 0; tinfo[len] != NIL; len++) {
    ans[len + 1] = p;
    strcpy(p, tinfo[len]);
    p += strlen(tinfo[len]) + 1;
  }
  ans[len + 1] = NIL;
  ILU_CLER(*err);
  return ans;
}

ilu_string 
_ilu_StringifyTinfo(ilu_TransportInfo tinfo, ILU_ERRS((no_memory)) * err)
{
  int             i, len = 0;
  ilu_string      ans, p;

  for (i = 0; tinfo[i] != NIL; i++)
    len += strlen(tinfo[i]) + 1;
  if ((ans = (char *) ilu_MallocE(len, err)) == NIL)
    return NIL;
  for (i = 0, p = ans; tinfo[i] != NIL; i++) {
    strcpy(p, tinfo[i]);
    p += strlen(p);
    if (tinfo[i + 1] != NIL)
      *p++ = ILU_TINFO_DIVIDER;
  }
  *p = 0;
  return (ans);
}

#ifdef ENABLE_DEBUGGING
void
  _ilu_PrintTinfo (ilu_TransportInfo tinfo)
{
  int i;

  for (i = 0;  tinfo[i] != NIL;  i++)
    {
      ilu_DebugPrintf ("<%s>", tinfo[i]);
    }
}
#endif
