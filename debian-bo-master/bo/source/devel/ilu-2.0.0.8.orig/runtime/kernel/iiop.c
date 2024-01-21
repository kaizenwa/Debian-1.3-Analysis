/*
Copyright (c) 1991-1996 Xerox Corporation.  All Rights Reserved.  

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
/* $Id: iiop.c,v 1.90 1996/06/27 23:39:26 spreitze Exp $ */
/* Last edited by Mike Spreitzer June 27, 1996 4:31 pm PDT */

#ifdef MACOS
#pragma segment ilu
#endif

#include <string.h>

#include "iluntrnl.h"
#include "iiop.h"

#include "call.h"
#include "protocol.h"
#include "connect.h"
#include "transprt.h"
#include "port.h"
#include "object.h"
#include "type.h"
#include "method.h"
#include "mooring.h"
#include "server.h"

static const ilu_byte IIOPDefaultMajorVersion = 1;
static const ilu_byte IIOPDefaultMinorVersion = 0;

#define IDLAttributePrefix "ilu--prefix-idlAttribute-"
#define IDLAttributePrefixLen (sizeof(IDLAttributePrefix)-1)

static ilu_boolean _IIOP_SendPacket(ilu_Call, ilu_boolean);

static ilu_boolean Initialized = ilu_FALSE;

#ifdef ENABLE_DEBUGGING
static ilu_string encode (ilu_bytes key, ilu_cardinal len)
{
  ilu_cardinal i;
  ilu_bytes copy;

  copy = ilu_must_malloc(len + 1);
  memcpy ((char *) copy, (char *) key, len);
  copy[len] = 0;
  for (i = 0;  i < len;  i++)
    if ((copy[i] < ((ilu_byte) 0x20)) || (copy[i] > ((ilu_byte) 0x7E)))
      copy[i] = (ilu_byte) '.';
  return (ilu_string) copy;
}
#endif /* ENABLE_DEBUGGING */

/*************************************************************/
/*		Implement CDR PACKET			     */
/*************************************************************/

/*L1, L2 unconstrained for sizing, end*/
/*Main holds, L2 >= {call's connection's callmu, iomu} for output*/
/*L1 unconstrained, L2 >= {call's connection's callmu, iomu} for input*/

/* ==================== cardinal ==================== */

#define SWAP_WORD(a) ( ((a) << 24) | \
                      (((a) << 8) & 0x00ff0000) | \
                      (((a) >> 8) & 0x0000ff00) | \
		      ((ilu_cardinal)(a) >>24) )

static void
 _cdr_put_u8 (PACKET p, ilu_byte val, ilu_Error *err)
{
  register ilu_byte *buf = PACKET_ALLOC(p, 1, 1, err);

  if (ILU_ERROK(*err))
    {
      *buf++ = val;
      PACKET_INCR(p, 1, 1);
    }
}

static void
 _cdr_put_u16 (PACKET p, ilu_shortcardinal val, ilu_Error *err)
{
  register ilu_byte *buf = PACKET_ALLOC(p, 2, 2, err);

  if (ILU_ERROK(*err))
    {
      PTR_ADJUST(buf,p, 2);
      if (ENDIAN_MATCH(p))
	MEMCPY(buf, &val, 2);
      else
	{
	  buf[0] = ((ilu_byte *) &val)[1];
	  buf[1] = ((ilu_byte *) &val)[0];
	}
      PACKET_INCR(p, 2, 2);
    }
}

static void
     _cdr_put_u32 (PACKET p, ilu_cardinal val, ilu_Error *err)
{
  register ilu_byte *buf = PACKET_ALLOC(p, 4, 4, err);
  ilu_cardinal tmp;

  if (ILU_ERROK(*err))
    {
      PTR_ADJUST(buf,p, 4);
      if (ENDIAN_MATCH(p))
	MEMCPY(buf, &val, 4);
      else
	{
	  tmp = SWAP_WORD(val);
	  MEMCPY(buf, &tmp, 4);
	}
      PACKET_INCR(p, 4, 4);
    }
}

static void
     _cdr_put_u64 (PACKET p, ilu_longcardinal *val, ilu_Error *err)
{
  register ilu_byte *buf = PACKET_ALLOC(p, 8, 8, err);

  if (ILU_ERROK(*err))
    {
      PTR_ADJUST(buf,p, 8);
      if (ENDIAN_MATCH(p))
	MEMCPY(buf, val, 8);
      else
	{
	  buf[0] = ((ilu_byte *) val)[7];
	  buf[1] = ((ilu_byte *) val)[6];
	  buf[2] = ((ilu_byte *) val)[5];
	  buf[3] = ((ilu_byte *) val)[4];
	  buf[4] = ((ilu_byte *) val)[3];
	  buf[5] = ((ilu_byte *) val)[2];
	  buf[6] = ((ilu_byte *) val)[1];
	  buf[7] = ((ilu_byte *) val)[0];
	}
      PACKET_INCR(p, 8, 8);
    }
}

static void
 _cdr_put_s8 (PACKET p, signed char val, ilu_Error *err)
{
  _cdr_put_u8(p, (ilu_byte) val, err);
}

static void
 _cdr_put_s16 (PACKET p, ilu_shortinteger val, ilu_Error *err)
{
  _cdr_put_u16(p, (ilu_shortcardinal) val, err);
}

static void
 _cdr_put_s32 (PACKET p, ilu_integer val, ilu_Error *err)
{
  _cdr_put_u32(p, (ilu_cardinal) val, err);
}

static void
  _cdr_put_s64 (PACKET p, ilu_longinteger *val, ilu_Error *err)
{
  register ilu_byte *buf = PACKET_ALLOC(p, 8, 8, err);

  if (ILU_ERROK(*err))
    {
      PTR_ADJUST(buf,p, 8);
      if (ENDIAN_MATCH(p))
	MEMCPY(buf, val, 8);
      else
	{
	  buf[0] = ((ilu_byte *) val)[7];
	  buf[1] = ((ilu_byte *) val)[6];
	  buf[2] = ((ilu_byte *) val)[5];
	  buf[3] = ((ilu_byte *) val)[4];
	  buf[4] = ((ilu_byte *) val)[3];
	  buf[5] = ((ilu_byte *) val)[2];
	  buf[6] = ((ilu_byte *) val)[1];
	  buf[7] = ((ilu_byte *) val)[0];
	}
      PACKET_INCR(p, 8, 8);
    }
}

static void
 _cdr_put_r32 (PACKET p, ilu_shortreal val, ilu_Error *err)
{
  _cdr_put_u32(p, *((ilu_cardinal *)(&val)), err);
}

static void
 _cdr_put_r64 (PACKET p, ilu_real val, ilu_Error *err)
{
  _cdr_put_u64(p, ((ilu_longcardinal *) (&val)), err);
}

static void
  _cdr_put_r128 (PACKET p, ilu_longreal val, ilu_Error *err)
{
  register ilu_byte *buf = PACKET_ALLOC(p, 16, 8, err);

  if (ILU_ERROK(*err))
    {
      PTR_ADJUST(buf,p, 8);
      if (ENDIAN_MATCH(p))
	MEMCPY(buf, &val, 16);
      else
	{
	  buf[0] = ((ilu_byte *) &val)[15];
	  buf[1] = ((ilu_byte *) &val)[14];
	  buf[2] = ((ilu_byte *) &val)[13];
	  buf[3] = ((ilu_byte *) &val)[12];
	  buf[4] = ((ilu_byte *) &val)[11];
	  buf[5] = ((ilu_byte *) &val)[10];
	  buf[6] = ((ilu_byte *) &val)[9];
	  buf[7] = ((ilu_byte *) &val)[8];
	  buf[8] = ((ilu_byte *) &val)[7];
	  buf[9] = ((ilu_byte *) &val)[6];
	  buf[10] = ((ilu_byte *) &val)[5];
	  buf[11] = ((ilu_byte *) &val)[4];
	  buf[12] = ((ilu_byte *) &val)[3];
	  buf[13] = ((ilu_byte *) &val)[2];
	  buf[14] = ((ilu_byte *) &val)[1];
	  buf[15] = ((ilu_byte *) &val)[0];
	}
      PACKET_INCR(p, 16, 8);
    }
}

static void
 _cdr_put_bytes (PACKET p, ilu_bytes ptr, ilu_cardinal len, ilu_Error *err)
{
  if (_cdr_put_u32 (p, len, err), ILU_ERRNOK(*err))
    return;
  PACKET_WRITE(p, ptr, len, err);
  PACKET_INCR(p, len, 1);
}

static void
 _cdr_put_opaque (PACKET p, ilu_bytes ptr, ilu_cardinal len, ilu_Error *err)
{
  PACKET_WRITE(p, ptr, len, err);
  PACKET_INCR(p, len, 1);
}

static void
     _cdr_get_u8 (PACKET p, ilu_byte *l, ilu_Error *err)
{
  register ilu_byte *buf = PACKET_OBTAIN(p, 1, 1, err);

  if (ILU_ERROK(*err))
    {
      *l = buf[0];
      PACKET_INCR(p, 1, 1);
    }
}

static void
  _cdr_get_u16 (PACKET p, ilu_shortcardinal *l, ilu_Error *err)
{
  register ilu_byte *buf = PACKET_OBTAIN(p, 2, 2, err);

  if (ILU_ERROK(*err))
    {
      PTR_ADJUST(buf,p,2);
      if (ENDIAN_MATCH(p))
	MEMCPY(l, buf, 2);
      else
	{
	  ((ilu_byte *) l)[0] = buf[1];
	  ((ilu_byte *) l)[1] = buf[0];
	}
      PACKET_INCR(p, 2, 2);
    }
}

static void
     _cdr_get_u32 (PACKET p, ilu_cardinal *l, ilu_Error *err)
{
  register ilu_byte *buf = PACKET_OBTAIN(p, 4, 4, err);

  if (ILU_ERROK(*err))
    {
      PTR_ADJUST(buf,p,4);
      if (ENDIAN_MATCH(p))
	MEMCPY(l, buf, 4);
      else
	{
	  ((ilu_byte *) l)[0] = buf[3];
	  ((ilu_byte *) l)[1] = buf[2];
	  ((ilu_byte *) l)[2] = buf[1];
	  ((ilu_byte *) l)[3] = buf[0];
	}
      PACKET_INCR(p, 4, 4);
    }
}

static void
     _cdr_get_u64 (PACKET p, ilu_longcardinal *l, ilu_Error *err)
{
  register ilu_byte *buf = PACKET_OBTAIN(p, 8, 8, err);

  if (ILU_ERROK(*err))
    {
      PTR_ADJUST(buf,p, 8);
      if (ENDIAN_MATCH(p))
	MEMCPY (l, buf, 8);
      else
	{
	  ((ilu_byte *) l)[0] = buf[7];
	  ((ilu_byte *) l)[1] = buf[6];
	  ((ilu_byte *) l)[2] = buf[5];
	  ((ilu_byte *) l)[3] = buf[4];
	  ((ilu_byte *) l)[4] = buf[3];
	  ((ilu_byte *) l)[5] = buf[2];
	  ((ilu_byte *) l)[6] = buf[1];
	  ((ilu_byte *) l)[7] = buf[0];
	}
      PACKET_INCR(p, 8, 8);
    }
}

static void
     _cdr_get_s64 (PACKET p, ilu_longinteger *l, ilu_Error *err)
{
  _cdr_get_u64 (p, (ilu_longcardinal *) l, err);
}

static void
     _cdr_get_s32 (PACKET p, ilu_integer *l, ilu_Error *err)
{
  _cdr_get_u32 (p, (ilu_cardinal *) l, err);
}

static void
     _cdr_get_s16 (PACKET p, short *l, ilu_Error *err)
{
  _cdr_get_u16(p, (ilu_shortcardinal *) l, err);
}

static void
     _cdr_get_s8 (PACKET p, signed char * l, ilu_Error *err)
{
  _cdr_get_u8(p, (unsigned char *) l, err);
}

static void
  _cdr_get_r32(PACKET p, float *l, ilu_Error *err)
{
  _cdr_get_u32 (p, (ilu_cardinal *) l, err);
}

static void
  _cdr_get_r64 (PACKET p, ilu_real *l, ilu_Error *err)
{
  _cdr_get_u64 (p, (ilu_longcardinal *) l, err);
}

static void
  _cdr_get_r128 (PACKET p, ilu_longreal *l, ilu_Error *err)
{
  register ilu_byte *buf = PACKET_OBTAIN(p, 16, 8, err);

  if (ILU_ERROK(*err))
    {
      PTR_ADJUST(buf,p, 8);
      if (ENDIAN_MATCH(p))
	MEMCPY(l, buf, 16);
      else
	{
	  ((ilu_byte *) l)[0] = buf[15];
	  ((ilu_byte *) l)[1] = buf[14];
	  ((ilu_byte *) l)[2] = buf[13];
	  ((ilu_byte *) l)[3] = buf[12];
	  ((ilu_byte *) l)[4] = buf[11];
	  ((ilu_byte *) l)[5] = buf[10];
	  ((ilu_byte *) l)[6] = buf[9];
	  ((ilu_byte *) l)[7] = buf[8];
	  ((ilu_byte *) l)[8] = buf[7];
	  ((ilu_byte *) l)[9] = buf[6];
	  ((ilu_byte *) l)[10] = buf[5];
	  ((ilu_byte *) l)[11] = buf[4];
	  ((ilu_byte *) l)[12] = buf[3];
	  ((ilu_byte *) l)[13] = buf[2];
	  ((ilu_byte *) l)[14] = buf[1];
	  ((ilu_byte *) l)[15] = buf[0];
	}
      PACKET_INCR(p, 16, 8);
    }
}

static ilu_cardinal IIOPMaxStringSize = 100000;

static void
  _cdr_get_bytes(PACKET p, ilu_byte **b, ilu_cardinal *l, ilu_cardinal limit, ILU_ERRS((marshal, bad_param)) *err)
{
  ilu_cardinal l2;
  ilu_boolean was_malloced = FALSE;

  if (_cdr_get_u32 (p, &l2, err), ILU_ERRNOK(*err))
    return;

  *l = 0;
  if (l2 > IIOPMaxStringSize OR (limit > 0 AND l2 > limit))
    {
      DEBUG(IIOP_DEBUG, (stderr,
			 "%s %lu, which exceeds IIOPMaxStringSize value of %lu or call limit of %lu.\n",
			 "Attempt to read byte sequence of length", l2,
			 IIOPMaxStringSize, limit));
      ILU_ERR_CONS1(marshal, err, minor, ilu_mm_sequenceLimit, 0);
    }
  else if (l2 == 0)
    {
      *l = l2;
      ILU_CLER(*err);
    }
  else
    {
      if (*b == NIL)
	{
	  *b = (ilu_byte *) ilu_malloc(l2 + 1);
	  if (*b == NIL)
	    {
	      ILU_ERR_CONS1(no_memory, err, nbytes, l2 + 1, 0);
	      return;
	    }
	  was_malloced = TRUE;
	}
      PACKET_READ(p, *b, l2, err);
      if (ILU_ERRNOK(*err) && was_malloced)
	ilu_free(*b);
      else
	{
	  PACKET_INCR(p, l2, 1);
	  if (was_malloced)
	    (*b)[l2] = 0;
	  *l = l2;
	}
    }
}

static void
  _cdr_get_opaque (PACKET p, ilu_byte **b, ilu_cardinal len, ilu_Error *err)
{
  ilu_boolean was_malloced = FALSE;

  if (*b == NIL)
    {
      *b = (ilu_byte *) ilu_malloc(len);
      if (*b == NIL)
	{
	  ILU_ERR_CONS1(no_memory, err, nbytes, len, 0);
	  return;
	}
      else
	was_malloced = TRUE;
    }
  PACKET_READ(p, *b, len, err);
  if (ILU_ERRNOK(*err) && was_malloced)
    ilu_free(*b);
  else
    PACKET_INCR(p, len, 1);
}

static void _cdr_destroy (PACKET p, ilu_Error *err)
{
  FREETOKEN(p->objKey);
  FREETOKEN(p->principal);
  ILU_CLER(*err);
}

static struct ilu_packet_methods_s CDR_methods = {

  _cdr_put_s8, _cdr_put_s16, _cdr_put_s32, _cdr_put_s64,
  _cdr_put_u8, _cdr_put_u16, _cdr_put_u32, _cdr_put_u64,
  _cdr_put_r32, _cdr_put_r64, _cdr_put_r128,
  _cdr_put_bytes, _cdr_put_opaque,

  _cdr_get_s8, _cdr_get_s16, _cdr_get_s32, _cdr_get_s64,
  _cdr_get_u8, _cdr_get_u16, _cdr_get_u32, _cdr_get_u64,
  _cdr_get_r32, _cdr_get_r64, _cdr_get_r128,
  _cdr_get_bytes, _cdr_get_opaque,

  _cdr_destroy
  };

static PACKET _cdr_InitPacket (PACKET p, ilu_Transport bs, enum byte_order bo, ilu_bytes offset)
{
  p->methods = &CDR_methods;
  p->bs = bs;
  p->vop = offset;
  p->byteorder = bo;
  p->size = 0;
  p->objKey = NIL;
  p->objKeyLen = 0;
  p->principal = NIL;
  p->principalLen = 0;
  return (p);
}

static PACKET _cdr_CreatePacket (ilu_Transport bs, enum byte_order bo, ilu_bytes offset, ilu_Error *err)
{
  PACKET p = (PACKET) ilu_malloc (sizeof(struct ilu_packet_s));
  if (p == NIL)
    ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(struct ilu_packet_s), 0);
  else
    {
      (void) _cdr_InitPacket (p, bs, bo, offset);
      ILU_CLER(*err);
    }
  return p;
}

static PACKET _cdr_InmemPacket (ilu_cardinal size, ilu_bytes data, enum byte_order bo, ilu_bytes offset, ilu_Error *err)
{
  ilu_Transport t;

  t = _ilu_BufferTransport_Create (size, data, err);
  if (ILU_ERROK(*err))
    return (_cdr_CreatePacket (t, bo, offset, err));
  else
    return NIL;
}

static void _cdr_InmemFree (PACKET pk, ilu_cardinal *bufferlen, ilu_bytes *buffer)
{
  ilu_Error err;

  ilu_Transport t = pk->bs;
  packet_destroy(pk, &err);
  ILU_HANDLED(err);
  FREETOKEN(pk);
  _ilu_BufferTransport_Destroy (t, bufferlen, buffer, &err);
  ILU_HANDLED(err);
}

/**********************************************************************
  End of packet implementation
***********************************************************************/

/*L1, L2, Main unconstrained*/

static void Initialize (void)
{
  if (Initialized)
    return;

  Initialized = ilu_TRUE;
}

static ilu_refany _IIOP_CreateDataBlock_1_0 (ilu_Error *err)
{
  struct IIOP_DataBlock *s;

  if ((s = ilu_malloc(sizeof(*s))) == NIL)
    return (ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(*s), NIL));
  s->major = 1;
  s->minor = 0;
  ILU_CLER(*err);
  return (s);
}

static ilu_refany _IIOP_CreateDataBlock_1_1 (ilu_Error *err)
{
  struct IIOP_DataBlock *s;

  if ((s = ilu_malloc(sizeof(*s))) == NIL)
    return (ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(*s), NIL));
  s->major = 1;
  s->minor = 1;
  ILU_CLER(*err);
  return (s);
}

static void _IIOP_FreeDataBlock (ilu_refany d /* , ilu_Error *err */)
{
  ilu_free(d);
}

static ilu_boolean _IIOP_InitCall (ilu_Call call , ilu_Error *err)
{
  PACKET p = _cdr_CreatePacket (connection_transport(call->ca_connection),
				NATIVE_BYTE_ORDER,
				0,
				err);

  if (ILU_ERRNOK(*err))
    return FALSE;
  iiop_set_packet(call, p);
  call->ca_prTrans = connection_transport(call_connection(call));
  /* figure header size */

  iiop_vop(call) = 0;
  return TRUE;
}

static ilu_boolean _IIOP_FinishCall (ilu_Call call, ilu_Error *err)
{
  if (!call->ca_incoming)
    iiop_objKey(call) = NIL;
  _cdr_destroy (iiop_packet(call), err);
  FREETOKEN(iiop_packet(call));
  iiop_set_packet(call, NIL);
  return ILU_ERROK(*err);
}

/*======================================================================*/
/*=========== Mapping to and from IIOP classes and methods =============*/
/*======================================================================*/

static ilu_Class FindClassFromObjectKey (ilu_bytes key, ilu_cardinal key_len)
{
  /* treat "key" as URL of an object, return its class */
  /* For an ILU object key marshalled over UNO, we have a full URL */
  ilu_Class	cl = NIL;
  ilu_string	mstid = NIL;

  if (strcmp((ilu_string) key, "ilu") != 0)
    {
      DEBUG(IIOP_DEBUG,
	    (stderr, "_IIOP_InterpretRequest:  object key not an ILU object key\n"));
    }
  else
    cl = ilu_FindClassFromID((ilu_string)(key + 4));
  return cl;
}

static ilu_Method FindMethodOnClass2 (ilu_Class c, ilu_string mname, ilu_Class *realclass)
{
  ilu_cardinal i;

  for (i = 0;  i < c->cl_method_count;  i++)
    {
      if (strcmp (mname, c->cl_methods[i].me_name) == 0)
	{
	  *realclass = c;
	  return (c->cl_methods + i);
	}
    }
  for (i = 0;  i < c->cl_scls_count;  i++)
    {
      ilu_Method m;
	  
      if ((m = FindMethodOnClass2 (c->cl_sclses[i], mname, realclass)) != NIL)
	return (m);
    }
  return NIL;
}

static ilu_Method FindMethodOnClass (ilu_Class c, ilu_string mname, ilu_Class *realclass)
{
  ilu_Method m = NIL;

  if ((m = FindMethodOnClass2 (c, mname, realclass)) == NIL)
    {
      ilu_Class root = ilu_rootClass;
      ilu_cardinal i;

      for (i = 0;  i < root->cl_method_count;  i++)
	{
	  if (strcmp (mname, root->cl_methods[i].me_name) == 0)
	    {
	      m = root->cl_methods + i;
	      *realclass = ilu_rootClass;
	      break;
	    }
	}
    }

  return m;
}

static void
  FindClassAndMethodFromIDLMethodName (ilu_Call call, ilu_Class ptype, ilu_string idl_name)
{
  char buf[1024];
  char *b;
  char *p1, *p2;
  ilu_Method m;
  ilu_Class realclass;

  if ((strlen(idl_name) + IDLAttributePrefixLen + 1) < sizeof(buf))
    b = buf;
  else
    b = ilu_must_malloc(strlen(idl_name) + 1);

  if (idl_name[0] == '_')
    { strcpy (b, IDLAttributePrefix);  p2 = b + IDLAttributePrefixLen; }
  else
    { p2 = b; }
  for (p1 = idl_name;  *p1 != 0;  p1++, p2++)
    {
      if (*p1 == '_')
	*p2 = '-';
      else
	*p2 = *p1;
    }
  *p2 = 0;
  m = FindMethodOnClass (ptype, b, &realclass);
  if (b != buf)
    ilu_free(b);
  call_intro_type(call) = realclass;
  call_method(call) = m;
  return;
}

static void FormMethodName (char *buf, ilu_Method m)
{
  char *p1, *p2;

  if (strncmp(m->me_name, IDLAttributePrefix, IDLAttributePrefixLen) == 0)
    p2 = m->me_name + IDLAttributePrefixLen;
  else
    p2 = m->me_name;
  for (p1 = buf;  *p2 != 0;  p1++, p2++)
    {
      if (*p2 == '-')
	*p1 = '_';
      else
	*p1 = *p2;
    }
  *p1 = 0;
}

static struct CORBA_exception_s {
  char *name;
  ilu_ProtocolException val;
} CORBA_exceptions[] = {
  { "IDL:omg.org/CORBA/UNKNOWN:1.0", ilu_ProtocolException_Unknown},
  { "IDL:omg.org/CORBA/BAD_PARAM:1.0", ilu_ProtocolException_GarbageArguments},
  { "IDL:omg.org/CORBA/NO_MEMORY:1.0", ilu_ProtocolException_Unknown},
  { "IDL:omg.org/CORBA/IMP_LIMIT:1.0", ilu_ProtocolException_Unknown},
  { "IDL:omg.org/CORBA/COMM_FAILURE:1.0", ilu_ProtocolException_LostConnection},
  { "IDL:omg.org/CORBA/INV_OBJREF:1.0", ilu_ProtocolException_GarbageArguments},
  { "IDL:omg.org/CORBA/NO_PERMISSION:1.0", ilu_ProtocolException_RequestRejected},
  { "IDL:omg.org/CORBA/INTERNAL:1.0", ilu_ProtocolException_Unknown},
  { "IDL:omg.org/CORBA/MARSHALL:1.0", ilu_ProtocolException_GarbageArguments},
  { "IDL:omg.org/CORBA/INITIALIZE:1.0", ilu_ProtocolException_Unknown},
  { "IDL:omg.org/CORBA/NO_IMPLEMENT:1.0", ilu_ProtocolException_NoSuchMethodOnClass},
  { "IDL:omg.org/CORBA/BAD_TYPECODE:1.0", ilu_ProtocolException_NoSuchClassAtServer},
  { "IDL:omg.org/CORBA/BAD_OPERATION:1.0", ilu_ProtocolException_NoSuchMethodOnClass},
  { "IDL:omg.org/CORBA/NO_RESOURCES:1.0", ilu_ProtocolException_Unknown},
  { "IDL:omg.org/CORBA/NO_RESPONSE:1.0", ilu_ProtocolException_RequestTimeout},
  { "IDL:omg.org/CORBA/PERSIST_STORE:1.0", ilu_ProtocolException_Unknown},
  { "IDL:omg.org/CORBA/BAD_INV_ORDER:1.0", ilu_ProtocolException_Unknown},
  { "IDL:omg.org/CORBA/TRANSIENT:1.0", ilu_ProtocolException_Unknown},
  { "IDL:omg.org/CORBA/FREE_MEM:1.0", ilu_ProtocolException_Unknown},
  { "IDL:omg.org/CORBA/INV_IDENT:1.0", ilu_ProtocolException_GarbageArguments},
  { "IDL:omg.org/CORBA/INV_FLAG:1.0", ilu_ProtocolException_GarbageArguments},
  { "IDL:omg.org/CORBA/INTF_REPOS:1.0", ilu_ProtocolException_Unknown},
  { "IDL:omg.org/CORBA/BAD_CONTEXT:1.0", ilu_ProtocolException_GarbageArguments},
  { "IDL:omg.org/CORBA/OBJ_ADAPTER:1.0", ilu_ProtocolException_Unknown},
  { "IDL:omg.org/CORBA/DATA_CONVERSION:1.0", ilu_ProtocolException_GarbageArguments}
};

static ilu_cardinal FigureExceptionIndexFromIDLName (ilu_Class c, ilu_Method m, char *rep_id)
{
  ilu_cardinal i;

  DEBUG(IIOP_DEBUG, (stderr, "(FigureExceptionIndexFromIDLName):  exception <%s> received.\n",
		     rep_id));
  if (m != NIL)
    {
      for (i = 0;  i < m->me_exceptionCount;  i++)
	if (strcmp(m->me_exceptionVector[i], rep_id) == 0)
	  return (i + 1);
      DEBUG(IIOP_DEBUG, (stderr, "(FigureExceptionIndexFromIDLName):  no exception on method '%s' with ID '%s'.\n",
			 m->me_name, rep_id));
      return 0;
    }
  else
    {
      for (i = 0;  i < (sizeof(CORBA_exceptions)/sizeof(struct CORBA_exception_s));  i++)
	{
	  if (strcmp(CORBA_exceptions[i].name, rep_id) == 0)
	    return (CORBA_exceptions[i].val);
	}
      DEBUG(IIOP_DEBUG, (stderr, "(FigureExceptionIndexFromIDLName):  no standard exception '%s' known.\n",
			 rep_id));
      return 0;
    }
}

static char * system_exceptions[] = {
  /* ilu_ProtocolException_Success = 0, */
  "IDL:omg.org/CORBA/BAD_TYPECODE:1.0",		/* ilu_ProtocolException_NoSuchClassAtServer = 1 */
  "IDL:omg.org/CORBA/UNKNOWN:1.0",		/* ilu_ProtocolException_ClassVersionMismatch = 2 */
  "IDL:omg.org/CORBA/BAD_OPERATION:1.0",	/* ilu_ProtocolException_NoSuchMethodOnClass = 3 */
  "IDL:omg.org/CORBA/BAD_PARAM:1.0",		/* ilu_ProtocolException_GarbageArguments = 4 */
  "IDL:omg.org/CORBA/UNKNOWN:1.0",		/* ilu_ProtocolException_Unknown = 5 */
  "IDL:omg.org/CORBA/COMM_FAILURE:1.0",		/* ilu_ProtocolException_LostConnection = 6 */
  "IDL:omg.org/CORBA/UNKNOWN:1.0",		/* ilu_ProtocolException_RequestRejected = 7 */
  "IDL:omg.org/CORBA/NO_RESPONSE:1.0"		/* ilu_ProtocolException_RequestTimeout = 8 */
  };

static ilu_string FigureNameOfException (ilu_Class c, ilu_Method m, ilu_integer index, ilu_ProtocolException sysExcn)
{
  if (index == 0)
    {
      ilu_cardinal val = ((ilu_cardinal) sysExcn) - 1;

      if (val >= (sizeof(system_exceptions)/sizeof(char *)))
	{
	  DEBUG(IIOP_DEBUG, (stderr, "(iiop.c:FigureNameOfException):  Unknown ilu_ProtocolException value passed:  %lu.\n", (long unsigned) val));
	  return (NIL);
	}
      else
	return system_exceptions[val];
    }
  else
    {
      if ((ilu_cardinal) index > m->me_exceptionCount)
	{
	  DEBUG(IIOP_DEBUG, (stderr, "Invalid exception index %u specified for method %s.\n",
			     index, m->me_name));
	  return (NIL);
	}
      return (m->me_exceptionVector[index - 1]);
    }
}

static void FreeIORData (struct IIOP_IOR_IOR *ior)
{
  ilu_cardinal i;

  FREETOKEN(ior->type_id);
  for (i = 0;  i < ior->nProfiles;  i++)
    {
      FREETOKEN(ior->Profile[i].profileData);
    }
}

#define isalphanum(x)	(((x)<='z'&&(x)>='a')||((x)<='Z'&&(x)>='A')||((x)<='9'&&(x)>='0'))
static const char hextable[] = "0123456789ABCDEF";
#define hexval(x)	(((x)>='0'&&(x)<='9')?((x)-'0'):(((x)>='A'&&(x)<='F')?((x)-'A'+10):(((x)>='a'&&(x)<='f')?((x)-'a'+10):0)))

static unsigned char *bdup (unsigned char *p, ilu_cardinal size)
{
  unsigned char *p2 = (unsigned char *) ilu_must_malloc(size);
  MEMCPY(p2, p, size);
  return (p2);
}

static ilu_bytes
  decode_foreign_object_key (ilu_string key, ilu_cardinal *len, ilu_Error *err)
{
  ilu_bytes n;
  ilu_cardinal i, j;

  if (strncmp(key, "CORBA:", 6) != 0)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, NIL);
  *len = (ilu_cardinal) (strlen(key) - 6) / 2;
  n = (ilu_bytes) ilu_MallocE(*len, err);
  if (ILU_ERRNOK(*err)) return NIL;
  for (j = 6, i = 0;  i < *len;  i++, j += 2)
    n[i] = (hexval(key[j]) << 4) | hexval(key[j+1]);
  return n;
}

static ilu_string
  encode_foreign_object_key (ilu_bytes key, ilu_cardinal keylen, ilu_Error *err)
{
  ilu_string n;
  ilu_cardinal i, j;

  n = (ilu_string) ilu_MallocE(keylen * 2 + 7, err);
  if (ILU_ERRNOK(*err)) return NIL;
  strcpy (n, "CORBA:");
  for (i = 0, j = 6;  i < keylen;  i++, j += 2)
    {
      n[j] = hextable[(key[i] >> 4) & 0xF];
      n[j+1] = hextable[key[i] & 0xF];
    }
  n[j] = 0;
  return n;
}

static ilu_boolean _iiop_AddIIOPProfile (char *sbh, ilu_Object obj,
					 struct IIOP_IOR_IOR *ior,
					 ilu_Error *err)
{
  ilu_TransportInfo	tinfo;
  ilu_byte	hostname[1000];
  ilu_byte *	object_key;
  ilu_cardinal object_key_len = 0;
  char *ih, *sid, *pinfo, *mstid;
  ilu_string   cinfo;
  ilu_cardinal cinfolen = 0;
  unsigned long port;
  int i;
  PACKET pk;

  if (!ilu_ParseSBH(sbh, &ih, &sid, &mstid, &cinfo, &cinfolen, err))
    return ilu_FALSE;
  if (cinfo == NIL)
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ci, ilu_FALSE);
  if (!_ilu_ParseConnectInfo(cinfo, cinfolen, &pinfo, &tinfo, err))
    return ilu_FALSE;
  if (strcmp(pinfo, "iiop_1_0_1") != 0)
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ps, ilu_FALSE);
  ilu_free(pinfo);
  /* Find host and port */
  for (i = 0;  tinfo[i] != NIL;  i++)
    ;
  if ((i < 1) || (sscanf (tinfo[i-1], "tcp_%[^_]_%lu", hostname, &port) != 2))
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ti, ilu_FALSE);
  ilu_free(tinfo);

  if (strcmp(ih, "$") == 0)
    /* non-ILU object.  Recover the original object key and send it. */
    {
      object_key = decode_foreign_object_key(sid, &object_key_len, err);
      ilu_free(sid);
      if (ILU_ERRNOK(*err)) return ilu_FALSE;
    }
  else
    /* ILU object.  Send mstid, sid, and ih as object key.
       Unfortunately, we need to put the type ID in the object key,
       because the IIOP protocol has left it out of its header. */
    {
      int sidl = strlen(sid), ihl = strlen(ih), mstidl = strlen(mstid);
      object_key_len = sidl + ihl + mstidl + 7;
      object_key = (ilu_bytes) ilu_malloc(object_key_len);
      if (object_key == NIL)
	return ILU_ERR_CONS1(no_memory, err, nbytes, object_key_len, ilu_FALSE);
      memcpy ((void *) object_key, "ilu", 4);
      memcpy ((void *) (object_key + 4), mstid, mstidl + 1);
      memcpy ((void *) (object_key + mstidl + 5), sid, sidl + 1);
      memcpy ((void *) (object_key + mstidl + sidl + 6), ih, ihl + 1);
      ilu_free(sid);
    }
  ilu_free(mstid);
  ilu_free(ih);

  /* Build IIOP profile.

     Figure max size.  should be
     1 for byte order,
     2 for IIOP_Version,
     (1 padding),
     max of 4+15=19 for hostname,
     (padding),
     2 for port,
     (padding),
     n + 4 for object_key
     */

  pk = _cdr_InmemPacket (object_key_len + 4 + 20 + 4 + 4, NIL, NATIVE_BYTE_ORDER, 0, err);
  if (ILU_ERRNOK(*err))
    {
      ilu_free(object_key);
      return ilu_FALSE;
    }
  if ((_cdr_put_u8 (pk, (NATIVE_BYTE_ORDER == LittleEndian), err), ILU_ERRNOK(*err)) OR
      (_cdr_put_u8 (pk, IIOPDefaultMajorVersion, err), ILU_ERRNOK(*err)) OR
      (_cdr_put_u8 (pk, IIOPDefaultMinorVersion, err), ILU_ERRNOK(*err)) OR
      (_cdr_put_bytes (pk, hostname, (ilu_cardinal) (strlen((ilu_string) hostname) + 1), err), ILU_ERRNOK(*err)) OR
      (_cdr_put_u16 (pk, (ilu_shortcardinal) port, err), ILU_ERRNOK(*err)) OR
      (_cdr_put_bytes (pk, object_key, object_key_len, err), ILU_ERRNOK(*err)))
    {
      _cdr_InmemFree(pk, NIL, NIL);
      ilu_free(object_key);
      return (ilu_FALSE);
    }
  ior->Profile[ior->nProfiles].tag = IIOP_TAG_INTERNET_IOP;
  _cdr_InmemFree (pk, &ior->Profile[ior->nProfiles].profileDataLen,
		  &ior->Profile[ior->nProfiles].profileData);
  ior->nProfiles += 1;
  ilu_free(object_key);
  return ilu_TRUE;
}

static ilu_boolean _iiop_AddILUProfile (char *sbh,
					ilu_Object obj,
					struct IIOP_IOR_IOR *ior,
					ilu_Error *err)
{
  PACKET pk;

  /* ILU object.  Send SBH as object key. */
  
  /* Build IIOP ILU profile.

     Figure max size.  should be
     1 for byte order,
     2 for IIOP_Version,
     1 for padding,
     4 for length of SBH,
     n + 1 for SBH
     */

  pk = _cdr_InmemPacket (9 + strlen(sbh), NIL, NATIVE_BYTE_ORDER, 0, err);
  if (ILU_ERRNOK(*err))
    return ilu_FALSE;
  if ((_cdr_put_u8 (pk, (NATIVE_BYTE_ORDER == LittleEndian), err), ILU_ERRNOK(*err)) OR
      (_cdr_put_u8 (pk, IIOPDefaultMajorVersion, err), ILU_ERRNOK(*err)) OR
      (_cdr_put_u8 (pk, IIOPDefaultMinorVersion, err), ILU_ERRNOK(*err)) OR
      (_cdr_put_bytes (pk, (ilu_bytes) sbh, strlen(sbh) + 1, err), ILU_ERRNOK(*err)))
    return (ILU_ERROK(*err));
  ior->Profile[ior->nProfiles].tag = IIOP_TAG_ILU_IOP;
  _cdr_InmemFree (pk, &ior->Profile[ior->nProfiles].profileDataLen,
		  &ior->Profile[ior->nProfiles].profileData);
  ior->nProfiles += 1;
  return (ILU_ERROK(*err));
}

/* obj != NIL => Inside(server(obj), class(obj)); */
static ilu_Error IOROfObject (ilu_Object obj, struct IIOP_IOR_IOR **ior)
{
  ilu_Error err = ILU_INIT_NO_ERR;
  struct {
    struct IIOP_IOR_IOR ior;
    struct IIOP_IOR_TaggedProfile profiles[10];
  } myior;

  myior.ior.type_id = NIL;
  myior.ior.nProfiles = 0;

  if (obj == NIL)	/* NIL object reference */
    {
      myior.ior.type_id = "";
    }
  else
    {
      char	*sbh = ilu_SBHOfObject(obj);
      ilu_string cinfo = NIL;
      ilu_cardinal cinfolen = 0;

      if (sbh == NIL OR object_class(obj) == NIL)
	return ILU_ERR_CONS1(internal, &err, minor, ilu_im_broken, err);
      if (!ilu_ParseSBH(sbh, NIL, NIL, NIL, &cinfo, &cinfolen, &err))
	return err;
      myior.ior.type_id = _ilu_Strdup(class_unique_id(object_class(obj)));
      if (strncmp(object_ih(obj), "$", 2) != 0)
	{
	  if (!_iiop_AddILUProfile (sbh, obj, (struct IIOP_IOR_IOR *) &myior, &err))
	    return err;
	}
      if (strncmp(cinfo, "iiop_1_0_1", 10) == 0)
	{
	  if (!_iiop_AddIIOPProfile (sbh, obj, (struct IIOP_IOR_IOR *) &myior, &err))
	    return err;
	}
    }
  if (ILU_ERROK(err))
    {
      ilu_cardinal i;

      if (*ior == NIL)
	{
	  *ior = ilu_MallocE(sizeof(struct IIOP_IOR_IOR) + ((myior.ior.nProfiles - 1) * sizeof(struct IIOP_IOR_TaggedProfile)), &err);
	}
      if (*ior != NIL)
	{
	  (*ior)->type_id = myior.ior.type_id;
	  (*ior)->nProfiles = myior.ior.nProfiles;
	  if (myior.ior.nProfiles > 0)
	    {
	      (*ior)->Profile[0].tag = myior.ior.Profile[0].tag;
	      (*ior)->Profile[0].profileDataLen = myior.ior.Profile[0].profileDataLen;
	      (*ior)->Profile[0].profileData = myior.ior.Profile[0].profileData;
	    }
	  for (i = 1;  i < myior.ior.nProfiles;  i++)
	    {
	      (*ior)->Profile[i].tag = myior.profiles[i-1].tag;
	      (*ior)->Profile[i].profileDataLen = myior.profiles[i-1].profileDataLen;
	      (*ior)->Profile[i].profileData = myior.profiles[i-1].profileData;
	    }
	}
    }
      
  return err;
}

static ilu_boolean parse_IIOP_Profile (struct IIOP_IOR_TaggedProfile *prof,
				       ilu_string *ih,
				       ilu_string *sid,
				       ilu_string *mstid,
				       ilu_string *cinfo,
				       ilu_cardinal *cinfolen,
				       ilu_Error *err)
{
  PACKET pk;
  ilu_shortcardinal port;
  ilu_byte major_version;
  ilu_byte minor_version;
  ilu_string	hostname = NIL;
  ilu_cardinal	hostname_len;
  ilu_bytes	object_key = NIL;
  ilu_cardinal	object_key_len;
  ilu_byte	byte_order_flag;
  enum byte_order bo;
  ilu_bytes	junk;
  ilu_cardinal	junklen;

  bo = (prof->profileData[0] == 0) ? BigEndian : LittleEndian;
  pk = _cdr_InmemPacket (prof->profileDataLen, prof->profileData, bo, 0, err);
  if (ILU_ERRNOK(*err))
    return ilu_FALSE;
  if ((_cdr_get_u8 (pk, &byte_order_flag, err), ILU_ERRNOK(*err)) OR
      (_cdr_get_u8 (pk, &major_version, err), ILU_ERRNOK(*err)) OR
      (_cdr_get_u8 (pk, &minor_version, err), ILU_ERRNOK(*err)) OR
      (_cdr_get_bytes (pk, (ilu_bytes *) &hostname, &hostname_len, 0xFFFF, err), ILU_ERRNOK(*err)) OR
      (_cdr_get_u16 (pk, &port, err), ILU_ERRNOK(*err)) OR
      (_cdr_get_bytes (pk, &object_key, &object_key_len, 0xFFFF, err), ILU_ERRNOK(*err)))
    {
#ifdef ENABLE_DEBUGGING
      DEBUG(IIOP_DEBUG,
	    (stderr, "(iiop.c:parse_IIOP_Profile):  Invalid encapsulated profile detected:\n"));
      _ilu_debug_DumpPacket(prof->profileData, prof->profileDataLen, "encapsulated profile");
#endif
      _cdr_InmemFree(pk, &junklen, &junk);
      return (ilu_FALSE);
    }
  _cdr_InmemFree(pk, &junklen, &junk);

#ifdef ENABLE_DEBUGGING
  if ((_ilu_DebugLevel & IIOP_DEBUG) != 0)
    {
      ilu_string ok2 = encode(object_key, object_key_len);
      ilu_DebugPrintf ("parse_IIOP_Profile:  bo=%s, version=%d.%d, hostname=%s, port=%u, object_key=<%s>\n",
		       (bo == BigEndian) ? "BigEndian" : "LittleEndian", major_version, minor_version, hostname,
		       (unsigned int) port, ok2);
      ilu_free(ok2);
    }
#endif /* ENABLE_DEBUGGING */

  if (strcmp((ilu_string) object_key, "ilu") == 0)
    {
      /* We have an ILU object, go ahead and transform it */

      if (mstid != NIL)
	*mstid = _ilu_Strdup((ilu_string) object_key + 4);
      if (sid != NIL)
	*sid = _ilu_Strdup((ilu_string) object_key + 4 + strlen((ilu_string) object_key + 4) + 1);
      if (ih != NIL)
	*ih = _ilu_Strdup((ilu_string) object_key + 4 + strlen((ilu_string) object_key + 4) + 1 + strlen((ilu_string) object_key + 4 + strlen((ilu_string) object_key + 4) + 1) + 1);
      if (cinfo != NIL || cinfolen != NIL)
	{
	  char cinfobuf[1024];
	  sprintf (cinfobuf, "iiop_1_0_1%ctcp_%s_%lu", ILU_CINFO_DIVIDER, hostname, ((unsigned long) port) & 0xFFFF);
	  if (cinfo != NIL)
	    *cinfo = _ilu_Strdup(cinfobuf);
	  if (cinfolen != NIL)
	    *cinfolen = strlen(cinfobuf);
	}
      ilu_free(object_key);
      ilu_free(hostname);
      return ilu_TRUE;
    }
  else
    {
      /* Non-ILU object from some other ORB:
	 - Use object_key for serverID
	 - Use ih of "$" */

      if (ih != NIL)
	*ih = _ilu_Strdup("$");
      if (sid != NIL)
	{
	  *sid = encode_foreign_object_key(object_key, object_key_len, err);
	  if (ILU_ERRNOK(*err))
	    return ilu_FALSE;
	}
      if (mstid != NIL)	/* we don't know anything about the type at this level */
	*mstid = NIL; /* or perhaps _ilu_Strdup(class_unique_id(ilu_rootClass)) */
      if (cinfo != NIL)
	{
	  char buf[2048];

	  sprintf (buf, "iiop_%u_%u_1@tcp_%s_%lu",
		   major_version, minor_version, hostname, ((unsigned long) port) & 0xFFFF);
	  *cinfo = _ilu_Strdup(buf);
	  if (cinfolen != NIL)
	    *cinfolen = strlen(buf);
	}
      ILU_CLER(*err);
      return ilu_TRUE;
    }
}

static ilu_boolean parse_ILU_Profile (struct IIOP_IOR_TaggedProfile *prof,
				      ilu_string *ih,
				      ilu_string *sid,
				      ilu_string *mstid,
				      ilu_string *cinfo,
				      ilu_cardinal *cinfolen,
				      ilu_Error *err)
{
  PACKET pk;
  ilu_byte major_version;
  ilu_byte minor_version;
  ilu_string	object_key = NIL;
  ilu_cardinal	object_key_len;
  ilu_byte	byte_order_flag;
  enum byte_order bo;
  ilu_bytes	junk;
  ilu_cardinal	junklen;

  bo = (prof->profileData[0] == 0) ? BigEndian : LittleEndian;
  pk = _cdr_InmemPacket (prof->profileDataLen, prof->profileData, bo, 0, err);
  if (ILU_ERRNOK(*err))
    return ilu_FALSE;
  if ((_cdr_get_u8 (pk, &byte_order_flag, err), ILU_ERRNOK(*err)) OR
      (_cdr_get_u8 (pk, &major_version, err), ILU_ERRNOK(*err)) OR
      (_cdr_get_u8 (pk, &minor_version, err), ILU_ERRNOK(*err)) OR
      (_cdr_get_bytes (pk, (ilu_bytes *) &object_key, &object_key_len, 0xFFFF, err), ILU_ERRNOK(*err)))
    {
#ifdef ENABLE_DEBUGGING
      DEBUG(IIOP_DEBUG,
	    (stderr, "(iiop.c:parse_ILU_Profile):  Invalid encapsulated profile detected:\n"));
      _ilu_debug_DumpPacket(prof->profileData, prof->profileDataLen, "encapsulated profile");
#endif /* ENABLE_DEBUGGING */
      _cdr_InmemFree(pk, &junklen, &junk);
      return (ilu_FALSE);
    }
  _cdr_InmemFree(pk, &junklen, &junk);

  if (strncmp(object_key, "ilu:", 4) == 0)
    {
      /* We have an ILU object, go ahead and transform it */
      ilu_string lcinfo;
      ilu_cardinal lcinfolen;

      if (!ilu_ParseSBH ((ilu_string) object_key, ih, sid, mstid, &lcinfo, &lcinfolen, err))
	{
	  ilu_free(object_key);
	  return FALSE;
	}
      if (cinfo != NIL)
	{
	  *cinfo = ilu_MallocE(lcinfolen+1, err);
	  if (ILU_ERRNOK(*err))
	    {
	      ilu_free(object_key);
	      return FALSE;
	    }
	  memcpy (*cinfo, lcinfo, lcinfolen);
	  (*cinfo)[lcinfolen] = 0;
	}
      if (cinfolen != NIL)
	*cinfolen = lcinfolen;
      ilu_free(object_key);
      return ilu_TRUE;
    }
  else
    {
      DEBUG(IIOP_DEBUG,
	    (stderr, "parse_ILU_Profile:  non-ILU URL <%s>.\n", object_key));
      ilu_free(object_key);
      return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_sbh, ilu_FALSE);
    }
}

static          ilu_boolean
  IsOfTypeViaRPC(ilu_Object o, ilu_Class type,
		 ILU_ERRS((bad_locks, inv_objref,
			   no_resources, IoErrs)) * err)
{
  ilu_Call_s      call_s;
  ilu_Call        call = &call_s;
  ilu_cardinal    reqSize;
  ilu_cardinal    estatus = 0;
  ilu_ProtocolException internal;
  ilu_Class       pclass = object_class(o);
  ilu_string	  id = class_unique_id(type);
  ilu_cardinal idlen = strlen(class_unique_id(type));
  ilu_Server      s = object_server(o);
  ilu_boolean	result = ilu_FALSE;
  ilu_Connection  newconn;
  extern ilu_Method	_ilu_IsAMethod;
  DEBUG(OBJECT_DEBUG,
	(stderr,
	 "(iiop.c:IsOfTypeViaRPC):  object %p, type \"%s\"...\n",
	 o, class_name(type)));
  (void) ilu_StartCall(call, s, _ilu_rootClass, _ilu_IsAMethod, 0, NIL,
		       &newconn, err);
  if (newconn != NIL)
    (void) _ilu_HandOffNewConnection(newconn, err);
  if (ILU_ERRNOK(*err))
    return FALSE;
  _ilu_AcquireMutex(server_lock(s));
  reqSize = ilu_SizeOfObjectID(call, o, TRUE, _ilu_rootClass, err);
  _ilu_ReleaseMutex(server_lock(s));
  if (ILU_ERRNOK(*err))
    goto faild;
  reqSize += ilu_SizeOfString (call, id, idlen, 0xFFFF, err);
  if (ILU_ERRNOK(*err))
    goto faild;
  if (!ilu_StartRequest(call, reqSize, err))
    goto faild;
  ilu_EnterServer(s, object_class(o));
  ilu_OutputObjectID(call, o, TRUE, _ilu_rootClass, err);
  if (ILU_ERRNOK(*err))
    goto faild;
  ilu_OutputString(call, id, idlen, 0xFFFF, err);
  if (ILU_ERRNOK(*err))
    goto faild;
  if (!ilu_FinishRequest(call, err))
    goto faild;
  internal = ilu_GetReply(call, &estatus, err);
  if (internal == ilu_ProtocolException_Not)
    goto faild;
  if (internal != ilu_ProtocolException_Success) {
    (void) ILU_ERR_CONS1(internal, err, minor, ilu_im_callFail, 0);
    goto faild;
  }
  if (estatus != 0) {
    (void) ILU_ERR_CONS1(internal, err, minor, ilu_im_callFail, 0);
    goto faild;
  }
  ilu_InputBoolean(call, &result, err);
  if (ILU_ERRNOK(*err))
    goto faild;
  ilu_ReplyRead(call, err);
  if (ILU_ERRNOK(*err))
    goto faild;
faild:
  ilu_FinishCall(call, err);
  if (ILU_ERRNOK(*err)) {
    ILU_HANDLED(*err);
  }
  return result;
}

typedef struct {
  ilu_Object obj;
  ilu_Class mstid_candidate;
  ilu_boolean abort;
} CheckTypeData;

/* L1 > otmu */
static void CheckTypeViaIsA (ilu_Class type, ilu_refany rock)
{
  ilu_Error err;
  CheckTypeData *d = (CheckTypeData *) rock;
  extern ilu_boolean _ilu_IsSubObjectType (ilu_Class, ilu_Class);

  if (d->abort ||
      (d->mstid_candidate == type) ||
      !_ilu_IsSubObjectType (type, d->mstid_candidate))
    return;
  if (IsOfTypeViaRPC(d->obj, type, &err) && ILU_ERROK(err))
    d->mstid_candidate = type;
  else
    d->abort = ILU_ERRNOK(err);
  ILU_HANDLED(err);
}

ilu_Class 
_ilu_IIOP_FindClassViaRPC(ilu_Object o)
{
  ilu_string      types = NIL;
  ilu_Class       c = NIL;
  ilu_Class       pclass = object_class(o);
  ILU_ERRS((bad_locks, inv_objref, no_resources, IoErrs)) lerr;
  CheckTypeData	d;

  if (class_singleton(pclass)) {
    DEBUG(IIOP_DEBUG | OBJECT_DEBUG,
	  (stderr,
	 "%s %s is singleton, not attempting to figure the real types via an RPC call.\n",
	   "_ilu_IIOP_FindClassViaRPC:  pclass", class_name(pclass)));
    return (NIL);
  }
  else
    {
      DEBUG(IIOP_DEBUG | OBJECT_DEBUG,
	    (stderr, "_ilu_IIOP_FindClassViaRPC(%p \"%s\" \"%s\")\n",
	     o, server_id(object_server(o)), object_ih(o)));
    }
  _ilu_Assert(!server_is_true(object_server(o)),
	      "_ilu_IIOP_FindClassViaRPC: called on true object");

  /* Make sure object is of at least the putative type */
  if (!IsOfTypeViaRPC(o, pclass, &lerr) || ILU_ERRNOK(lerr))
    {
      ILU_HANDLED(lerr);
      return NIL;
    }

  /* Now check other types to see if the object is some subtype of the putative type */
  d.obj = o;
  d.mstid_candidate = pclass;
  d.abort = ilu_FALSE;
  _ilu_EnumerateClasses (CheckTypeViaIsA, (ilu_refany) &d);
  if (d.abort)
    return NIL;
  else
    return d.mstid_candidate;
}

static ilu_boolean ParseIOR (struct IIOP_IOR_IOR *ior,
			     ilu_string *ih,
			     ilu_string *sid,
			     ilu_string *mstid,
			     ilu_string *cinfo,
			     ilu_cardinal *cinfolen,
			     ilu_Error *err)
{
  ilu_cardinal i;

  ILU_CLER(*err);

  if (ior->nProfiles == 0)
    return (ilu_FALSE);
  else
    {
      for (i = 0;  i < ior->nProfiles;  i++)
	{
	  if (ior->Profile[i].tag == IIOP_TAG_ILU_IOP)
	    {
	      _ilu_Assert((ior->type_id != NIL && ((int) strlen(ior->type_id) > 0)),
			  "Bad type_id in IOR with ILU profile");
	      /* Note that, due to lack of rigor by the writers of the IIOP,
		 and lack of agreement among ORB implementors, we can't trust
		 type ID values unless they come from an ILU orb.  Sigh...
	       */
	      if (mstid != NIL)
		*mstid = _ilu_Strdup(ior->type_id);
	      return parse_ILU_Profile (&ior->Profile[i],
					ih, sid, NIL, cinfo, cinfolen, err);
	    }
	}
      for (i = 0;  i < ior->nProfiles;  i++)
	{
	  if (ior->Profile[i].tag == IIOP_TAG_INTERNET_IOP)
	    {
	      return parse_IIOP_Profile (&ior->Profile[i],
					 ih, sid, mstid, cinfo, cinfolen, err);
	    }
	}
    }
  return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ior, ilu_FALSE);
}

static ilu_Object ObjectOfIOR (struct IIOP_IOR_IOR *ior,
			       ilu_Class static_type,
			       ilu_Error *err)
{
  ilu_string ih = NIL, sid = NIL, mstid = NIL, cinfo = NIL;
  ilu_cardinal cinfolen;
  ilu_Error lerr;
  ilu_Server s = NIL;
  ilu_Object h = NIL;
  ilu_Class foundclass = NIL;

  if (ior->nProfiles == 0)
    {
      ILU_CLER(*err);
      return NIL;
    }
  else if (ParseIOR(ior, &ih, &sid, &mstid, &cinfo, &cinfolen, err))
    {
      s = _ilu_FindServer (sid, TRUE, cinfo, cinfolen, &lerr);
      ilu_free (sid);
      ilu_free (cinfo);
      if (ILU_ERRNOK(lerr))
	{
	  ILU_HANDLED(lerr);
	  ilu_free(ih);
	  ilu_free(mstid);
	  return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_sid, NIL);
	}
      else if (s != NIL)
	{
	  ilu_EnterServer (s, static_type);
	  h = _ilu_FindObjectInServer (ih, s);
	  if (mstid != NIL)
	    foundclass = ilu_FindClassFromID(mstid);
	  if (h == NIL)
	    h = _ilu_FindOrCreateObject (ih, s, foundclass,
					 static_type, mstid, NIL, &lerr);
	  ilu_free(ih);
	  ilu_free(mstid);
	  if (ILU_ERRNOK(lerr))
	    return NIL;
	}
      if (h == NIL)
	{
	  return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ih, NIL);
	}
      else
	return (h);
    }
  else
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ior, NIL);
}

ilu_boolean _ilu_IIOP_ParseIIOP (ilu_string iiop, ilu_string *ih, ilu_string *sid,
				ilu_string *mstid, ilu_string *cinfo, ilu_cardinal *cinfolen,
				ilu_Error *err)
{
  /* Parse Sun IIOP style URL:
   *
   * iiop1.0://<host>:<port>/<key>
   */
  
  char hostname[1024];
  unsigned long port;
  char key[1024];

  if (sscanf (iiop, "iiop:1.0//%1023[^:]:%lu/%1023s", hostname, &port, key) != 3)
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ior, ilu_FALSE);
  else if (strcmp(key, "ilu") == 0)   /* ILU object */
    {
      /* We have an ILU object, go ahead and transform it */

      if (mstid != NIL)
	*mstid = _ilu_Strdup(key + 4);
      if (sid != NIL)
	*sid = _ilu_Strdup(key + 4 + strlen(key + 4) + 1);
      if (ih != NIL)
	*ih = _ilu_Strdup(key + 4 + strlen(key + 4) + 1 + strlen(key + 4 + strlen(key + 4) + 1) + 1);
      if (cinfo != NIL || cinfolen != NIL)
	{
	  char cinfobuf[1024];
	  sprintf (cinfobuf, "iiop_1_0_1%ctcp_%s_%lu", ILU_CINFO_DIVIDER, hostname,
		   ((unsigned long) port) & 0xFFFF);
	  if (cinfo != NIL)
	    *cinfo = _ilu_Strdup(cinfobuf);
	  if (cinfolen != NIL)
	    *cinfolen = strlen(cinfobuf);
	}
      ilu_free(hostname);
      return ilu_TRUE;
    }
  else
    {
      if (ih != NIL)
	*ih = _ilu_Strdup("$");
      if (sid != NIL)
	{
	  *sid = encode_foreign_object_key((ilu_bytes) key, strlen(key) + 1, err);
	  if (ILU_ERRNOK(*err))
	    return ilu_FALSE;
	}
      if (mstid != NIL)
	*mstid = NIL;
      if (cinfo != NIL)
	{
	  char cinfobuf[1024];
	  sprintf (cinfobuf, "iiop_1_0_1%ctcp_%s_%lu",
		   ILU_CINFO_DIVIDER, hostname, ((unsigned long) port) & 0xFFFF);
	  *cinfo = _ilu_Strdup(cinfobuf);
	  if (cinfolen != NIL)
	    *cinfolen = strlen(cinfobuf);
	}
      return ilu_TRUE;
    }
}

ilu_boolean _ilu_IIOP_ParseIOR (ilu_string ior, ilu_string *ih, ilu_string *sid,
				ilu_string *mstid, ilu_string *cinfo, ilu_cardinal *cinfo_len,
				ilu_Error *err)
{
  /* Parse OMG IOR: style URL:
   *
   * IOR:<hex-digits>
   */

  ilu_cardinal nbytes;
  PACKET pk;
  struct IIOP_IOR_IOR *p;
  ilu_bytes iorbytes;
  ilu_cardinal i;
  ilu_string repository_id = NIL;
  ilu_cardinal repository_id_len = 0;
  ilu_cardinal nprofiles = 0;
  enum byte_order bo;
  ilu_byte junk;
  int ptr;

  if (ior == NIL)
    {
      DEBUG(IIOP_DEBUG,
	    (stderr, "_ilu_IIOP_ParseIOR:  NIL IOR string passed\n"));
      return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_nil, ilu_FALSE);
    }
  else if ((strncmp(ior, "IOR:", 4) != 0) && (strncmp(ior, "ior:", 4) != 0))
    {
      DEBUG(IIOP_DEBUG,
	    (stderr, "_ilu_IIOP_ParseIOR:  IOR string doesn't begin with \"IOR:\"\n"));
      return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ior, ilu_FALSE);
    }
  else if ((nbytes = strlen(ior+4)) < 2)
    {
      DEBUG(IIOP_DEBUG,
	    (stderr, "_ilu_IIOP_ParseIOR:  IOR string is too short to contain obj ref\n"));
      return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ior, ilu_FALSE);
    }
  else if ((nbytes % 2) != 0)
    {
      DEBUG(IIOP_DEBUG,
	    (stderr, "_ilu_IIOP_ParseIOR:  IOR string contains odd number of hex digits\n"));
      return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ior, ilu_FALSE);
    }
  nbytes = (nbytes / 2);
  iorbytes = ilu_malloc(nbytes);
  for (i = 0, ptr = 4;  i < nbytes;  i += 1, ptr += 2)
    iorbytes[i] = (hexval(ior[ptr]) << 4) | hexval(ior[ptr+1]);
  bo = (iorbytes[0] == 0) ? BigEndian : LittleEndian;
  pk = _cdr_InmemPacket (nbytes, iorbytes, bo, 0, err);
  if (ILU_ERRNOK(*err))
    return ilu_FALSE;
  
  if ((_cdr_get_u8 (pk, &junk, err), ILU_ERRNOK(*err)) OR	/* throw away byte order */
      (_cdr_get_bytes (pk, (ilu_bytes *) &repository_id, &repository_id_len, 0xFFFF, err), ILU_ERRNOK(*err)) OR
      (_cdr_get_u32 (pk, &nprofiles, err), ILU_ERRNOK(*err)))
    {
      _cdr_InmemFree(pk, NIL, NIL);
      return ilu_FALSE;
    }
  DEBUG(IIOP_DEBUG,
	(stderr, "_ilu_IIOP_ParseIOR:  byte order %s, repository id <%s>, %lu profile%s\n",
	 (bo == LittleEndian) ? "LittleEndian" : "BigEndian", repository_id, (unsigned long) nprofiles,
	 (nprofiles == 1) ? "" : "s"));
  if (nprofiles == 0)
    {
      DEBUG(IIOP_DEBUG,
	    (stderr, "_ilu_IIOP_ParseIOR:  no profiles, so returning the NIL object"));
      /* NIL object ref */
      ILU_CLER(*err);
      ilu_free(repository_id);
      _cdr_InmemFree(pk, NIL, NIL);
      return ilu_TRUE;
    }
  nbytes = sizeof(struct IIOP_IOR_IOR) + nprofiles*(sizeof(struct IIOP_IOR_TaggedProfile));
  p = (struct IIOP_IOR_IOR *) ilu_malloc(nbytes);
  if (p == NIL)
    {
      ilu_free(repository_id);
      _cdr_InmemFree(pk, NIL, NIL);
      return ILU_ERR_CONS1(no_memory, err, nbytes, nbytes, ilu_FALSE);
    }
  p->type_id = repository_id;
  p->nProfiles = nprofiles;
  for (i = 0;  i < nprofiles;  i++)
    {
      p->Profile[i].profileData = NIL;
      if ((_cdr_get_u32 (pk, &p->Profile[i].tag, err), ILU_ERRNOK(*err)) ||
	  (_cdr_get_bytes (pk, &p->Profile[i].profileData,
			   &p->Profile[i].profileDataLen, 0xFFFF, err), ILU_ERRNOK(*err)))
	{
	  DEBUG(IIOP_DEBUG,
		(stderr, "_ilu_IIOP_ParseIOR:  Error reading profile %lu\n", (unsigned long) i+1));
	  _cdr_InmemFree(pk, NIL, NIL);
	  FreeIORData(p);
	  ilu_free(p);
	  return FALSE;
	}
      DEBUG(IIOP_DEBUG,
	    (stderr, "_ilu_IIOP_ParseIOR:  profile %lu is %lu bytes, tag %lu%s, %s byte order\n",
	     (unsigned long) i+1, (unsigned long) p->Profile[i].profileDataLen,
	     p->Profile[i].tag, ((p->Profile[i].tag == IIOP_TAG_INTERNET_IOP) ? " (INTERNET IOP)" :
				 ((p->Profile[i].tag == IIOP_TAG_ILU_IOP) ? " (ILU IOP)" : "")),
	     (p->Profile[i].profileData[0] == 0) ? "BigEndian" : "LittleEndian"));
    }
  _cdr_InmemFree(pk, NIL, NIL);	/* note this also frees iorbytes */
  (void) ParseIOR(p, ih, sid, mstid, cinfo, cinfo_len, err);
  FreeIORData(p);
  ilu_free(p);
  if (ILU_ERRNOK(*err))
    {
      DEBUG(IIOP_DEBUG,
	    (stderr, "_ilu_IIOP_ParseSBH:  error:  No object for IOR.\n"));
      return FALSE;
    }
  else
    return TRUE;
}

static HashTable ObjectIors = ILU_NIL;

static struct IIOP_IOR_IOR NilObjectIOR = { "", 0, { 0, 0, ILU_NIL } };

typedef struct iorhash {
  ilu_Server s;
  ilu_Class c;
  struct IIOP_IOR_IOR *ior;
  char ih[1];
} *iorhash;

static struct IIOP_IOR_IOR *FindIORForObject(ilu_Object h)
{
  iorhash ior;

  if (ObjectIors == NIL)
    return NIL;
  if (h == NIL)
    return &NilObjectIOR;
  if ((ior = (iorhash) _ilu_hash_FindInTable(ObjectIors, h)) == NIL)
    return NIL;
  if ((h->ob_server != ior->s) OR
      (strcmp(h->ob_ih, ior->ih) != 0) OR
      (h->ob_class != ior->c))
    {
      _ilu_hash_RemoveFromTable(ObjectIors, h);
      ilu_free(ior->ior);
      ilu_free(ior);
      return NIL;
    }
  else
    return ior->ior;
}

static void RegisterIORForObject (ilu_Object h, struct IIOP_IOR_IOR *ior)
{
  iorhash n;

  if (h == NIL)
    return;

  if (ObjectIors == NIL)
    ObjectIors = _ilu_hash_MakeNewTable(137, _ilu_hash_HashPointer,
					_ilu_hash_PointerCompare);

  _ilu_Assert(ObjectIors!=NIL,"NIL IOR hash table");
  if (ior == NIL)
    _ilu_hash_RemoveFromTable (ObjectIors, h);
  else
    {
      n = (iorhash) ilu_malloc(sizeof(struct iorhash) + strlen(h->ob_ih));
      if (n == NIL)
	return;
      n->c = h->ob_class;
      n->s = h->ob_server;
      strcpy (n->ih, h->ob_ih);
      n->ior = ior;
      _ilu_hash_AddToTable(ObjectIors, h, n);
    }
}

static void PossiblyRegisterIORForObject (ilu_Object h, struct IIOP_IOR_IOR *ior)
{
  struct IIOP_IOR_IOR *v = FindIORForObject (h);
  if (v == NIL)
    RegisterIORForObject (h, ior);
  else
    {
      FreeIORData(ior);
      ilu_free(ior);
    }
}

/* (obj!=NIL) => Inside(object_server(obj), object_class(obj) */
ilu_string
  ilu_IOROfObject (ilu_Object obj, ilu_Error *err)
{
  PACKET pk;
  static ilu_string NIL_IOR = "";
  struct IIOP_IOR_IOR	*iorp = NIL;
  ilu_bytes marshalled_ior;
  ilu_cardinal marshalled_ior_len;
  ilu_string new_ior_string;

  if (obj == NIL)
    {
      ILU_CLER(*err);
      return _ilu_Strdup(NIL_IOR);
    }
  else
    {
      ilu_cardinal size;
      ilu_cardinal i;
      if ((iorp = FindIORForObject(obj)) == NIL)
	{
	  if ((*err = IOROfObject(obj, &iorp)), ILU_ERRNOK(*err))
	    return (NIL);
	  RegisterIORForObject(obj, iorp);
	}
      size = 4 + 4 + strlen(iorp->type_id) + PADDING_NEC(strlen(iorp->type_id),4);	/* type ID */
      size += 4;								/* nProfiles */
      for (i = 0;  i < iorp->nProfiles;  i++)
	size += (4 + 4 + iorp->Profile[i].profileDataLen + PADDING_NEC(iorp->Profile[i].profileDataLen,4));
      pk = _cdr_InmemPacket (size, NIL, NATIVE_BYTE_ORDER, 0, err);
      if ((_cdr_put_u8 (pk, (NATIVE_BYTE_ORDER == LittleEndian), err), ILU_ERRNOK(*err)) ||
	  (_cdr_put_bytes (pk, (ilu_bytes) iorp->type_id, strlen(iorp->type_id) + 1, err), ILU_ERRNOK(*err)) ||
	  (_cdr_put_u32 (pk, iorp->nProfiles, err), ILU_ERRNOK(*err)))
	{
	  _cdr_InmemFree(pk, NIL, NIL);
	  return NIL;
	}
      for (i = 0;  i < iorp->nProfiles;  i++)
	{
	  if ((_cdr_put_u32 (pk, iorp->Profile[i].tag, err), ILU_ERRNOK(*err)) ||
	      (_cdr_put_bytes (pk, iorp->Profile[i].profileData,
			       iorp->Profile[i].profileDataLen, err), ILU_ERRNOK(*err)))
	    {
	      _cdr_InmemFree(pk, NIL, NIL);
	      return NIL;
	    }
	}
      _cdr_InmemFree (pk, &marshalled_ior_len, &marshalled_ior);
      if ((new_ior_string = ilu_malloc(5 + (marshalled_ior_len * 2))) == NIL)
	{
	  ilu_free (marshalled_ior);
	  return (ILU_ERR_CONS1(no_memory, err, nbytes, 5+(marshalled_ior_len*2), NIL));
	}
      strcpy (new_ior_string, "IOR:");
      for (i = 0;  i < marshalled_ior_len;  i++)
	{
	  new_ior_string[4+(2*i)] = hextable[(marshalled_ior[i] >> 4) & 0xF];
	  new_ior_string[5+(2*i)] = hextable[marshalled_ior[i] & 0xF];
	}
      new_ior_string[4+(marshalled_ior_len*2)] = 0;
      ilu_free(marshalled_ior);
      ILU_CLER(*err);
      return new_ior_string;
    }
}

/*======================================================================*/
/*======================== Basic I/O code ==============================*/
/*======================================================================*/

#define INPUT_ERROR		1
#define OUTPUT_ERROR		2

/*L1, L2, Main unconstrained (this is only for calling from debugger)*/
ilu_cardinal _ilu_IIOP_SetMaxStringSize (ilu_cardinal size)
{
  ilu_cardinal old_size = IIOPMaxStringSize;
  if (size > 0)
    IIOPMaxStringSize = size;
  return (old_size);  
}

/*L2 >= {call's connection's iomu}*/
/*L1, Main unconstrained*/

/* ==================== cardinal ==================== */

static void
  _IIOP_OutputCardinal (ilu_Call call, ilu_cardinal l, ILU_ERRS((IoErrs)) *err)
{
  packet_put_u32(iiop_packet(call), l, err);
}

static void
  _IIOP_InputCardinal (ilu_Call call, ilu_cardinal *i, ILU_ERRS((IoErrs)) *err)
{
  packet_get_u32(iiop_packet(call), i, err);
}

/*ARGSUSED*/
static ilu_cardinal
  _IIOP_SizeOfCardinal (ilu_Call call, ilu_cardinal i, ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal n = PADDING_NEC(iiop_vop(call), 4) + 4;

  iiop_incr_vop(call, n);
  ILU_CLER(*err);
  return (n);
}

/* ==================== short cardinal ==================== */

static void
  _IIOP_OutputShortCardinal (ilu_Call call, ilu_shortcardinal i, ILU_ERRS((IoErrs)) *err)
{
  packet_put_u16(iiop_packet(call), i, err);
}

static void
  _IIOP_InputShortCardinal (ilu_Call call, ilu_shortcardinal *i, ILU_ERRS((IoErrs)) *err)
{
  packet_get_u16(iiop_packet(call), i, err);
}

/*ARGSUSED*/
static ilu_cardinal
  _IIOP_SizeOfShortCardinal (ilu_Call call, ilu_shortcardinal i, ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal n = PADDING_NEC(iiop_vop(call), 2) + 2;

  iiop_incr_vop(call, n);
  ILU_CLER(*err);
  return (n);
}

/* ==================== integer ==================== */

static void
  _IIOP_OutputInteger (ilu_Call call, ilu_integer i, ILU_ERRS((IoErrs)) *err)
{
  packet_put_s32 (iiop_packet(call), i, err);
}

static void
  _IIOP_InputInteger (ilu_Call call, ilu_integer *i, ILU_ERRS((IoErrs)) *err)
{
  packet_get_s32 (iiop_packet(call), i, err);
}

/*ARGSUSED*/
static ilu_cardinal
  _IIOP_SizeOfInteger (ilu_Call call, ilu_integer i, ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal n = PADDING_NEC(iiop_vop(call), 4) + 4;

  iiop_incr_vop(call, n);
  ILU_CLER(*err);
  return (n);
}

/* ==================== short integer ==================== */

static void
  _IIOP_OutputShortInteger (ilu_Call call, ilu_shortinteger i, ILU_ERRS((IoErrs)) *err)
{
  packet_put_s16(iiop_packet(call), i, err);
}

static void
  _IIOP_InputShortInteger (ilu_Call call, ilu_shortinteger *i, ILU_ERRS((IoErrs)) *err)
{
  packet_get_s16 (iiop_packet(call), i, err);
}

/*ARGSUSED*/
static ilu_cardinal
  _IIOP_SizeOfShortInteger (ilu_Call call, ilu_shortinteger i, ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal n = PADDING_NEC(iiop_vop(call), 2) + 2;

  iiop_incr_vop(call, n);
  ILU_CLER(*err);
  return (n);
}

/* ==================== long integer ==================== */

static void
  _IIOP_OutputLongInteger (ilu_Call call, ilu_longinteger i, ILU_ERRS((IoErrs)) *err)
{
  packet_put_s64 (iiop_packet(call), &i, err);
}

static void
  _IIOP_InputLongInteger (ilu_Call call, ilu_longinteger *i, ILU_ERRS((IoErrs)) *err)
{
  packet_get_s64 (iiop_packet(call), i, err);
}

/*ARGSUSED*/
static ilu_cardinal
  _IIOP_SizeOfLongInteger (ilu_Call call, ilu_longinteger i, ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal n = PADDING_NEC(iiop_vop(call), 8) + 8;

  iiop_incr_vop(call, n);
  ILU_CLER(*err);
  return (n);
}

/* ==================== long cardinal ==================== */

static void
  _IIOP_OutputLongCardinal (ilu_Call call, ilu_longcardinal i, ILU_ERRS((IoErrs)) *err)
{
  _cdr_put_u64 (iiop_packet(call), &i, err);
}

static void
  _IIOP_InputLongCardinal (ilu_Call call, ilu_longcardinal *i, ILU_ERRS((IoErrs)) *err)
{
  _cdr_get_u64 (iiop_packet(call), i, err);
}

/*ARGSUSED*/
static ilu_cardinal
  _IIOP_SizeOfLongCardinal (ilu_Call call, ilu_longcardinal i, ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal n = PADDING_NEC(iiop_vop(call), 8) + 8;

  iiop_incr_vop(call, n);
  ILU_CLER(*err);
  return (n);
}

/* ==================== enumeration ==================== */

static void
  _IIOP_OutputEnumeration (ilu_Call call, ilu_shortcardinal i, ILU_ERRS((IoErrs)) *err)
{
  _cdr_put_u32 (iiop_packet(call), (ilu_cardinal) i, err);
}

static void
  _IIOP_InputEnumeration (ilu_Call call, ilu_shortcardinal *i, ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal i2;

  _cdr_get_u32 (iiop_packet(call), &i2, err);
  if (ILU_ERROK(*err))
    *i = i2;
}

/*ARGSUSED*/
static ilu_cardinal
  _IIOP_SizeOfEnumeration (ilu_Call call, ilu_shortcardinal i, ILU_ERRS((IoErrs)) *err)
{
  return (_IIOP_SizeOfCardinal(call, (ilu_cardinal) i, err));
}

/* ==================== real ==================== */

static void
  _IIOP_OutputReal (ilu_Call call, double d, ILU_ERRS((IoErrs)) *err)
{
  _cdr_put_r64 (iiop_packet(call), d, err);
}

static void
  _IIOP_InputReal (ilu_Call call, double *d, ILU_ERRS((IoErrs)) *err)
{
  _cdr_get_r64 (iiop_packet(call), d, err);
}

/*ARGSUSED*/
static ilu_cardinal
  _IIOP_SizeOfReal (ilu_Call call, double d, ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal n = PADDING_NEC(iiop_vop(call), 8) + 8;

  iiop_incr_vop(call, n);
  ILU_CLER(*err);
  return n;
}

/* ==================== long real ==================== */

static void
  _IIOP_OutputLongReal (ilu_Call call, ilu_longreal d, ILU_ERRS((IoErrs)) *err)
{
  _cdr_put_r128 (iiop_packet(call), d, err);
}

static void
  _IIOP_InputLongReal (ilu_Call call, ilu_longreal *d, ILU_ERRS((IoErrs)) *err)
{
  _cdr_get_r128 (iiop_packet(call), d, err);
}

/*ARGSUSED*/
static ilu_cardinal
  _IIOP_SizeOfLongReal (ilu_Call call, ilu_longreal d, ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal n = PADDING_NEC(iiop_vop(call), 8) + 16;

  iiop_incr_vop(call, n);
  ILU_CLER(*err);
  return (n);
}

/* ==================== short real ==================== */

static void
  _IIOP_OutputShortReal (ilu_Call call, float f, ILU_ERRS((IoErrs)) *err)
{
  _cdr_put_r32 (iiop_packet(call), f, err);
}

static void
  _IIOP_InputShortReal (ilu_Call call, float *f, ILU_ERRS((IoErrs)) *err)
{
  _cdr_get_r32 (iiop_packet(call), f, err);
}

/*ARGSUSED*/
static ilu_cardinal
  _IIOP_SizeOfShortReal (ilu_Call call, float d, ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal n = PADDING_NEC(iiop_vop(call), 4) + 4;

  iiop_incr_vop(call, n);
  ILU_CLER(*err);
  return (n);
}

/* ==================== bytes ==================== */

static void OutputBytes (ilu_Call call, ilu_bytes s, ilu_cardinal len,
			 ilu_cardinal limit, ilu_boolean nulterm, ilu_Error *err)
{
  ilu_cardinal size;

  if (limit > 0)
    size = MIN(len, limit);
  else
    size = len;

  size = size + (nulterm ? 1 : 0);

  _cdr_put_u32 (iiop_packet(call), size, err);
  if (ILU_ERROK(*err))
    {
      _cdr_put_opaque (iiop_packet(call), s, size - (nulterm ? 1 : 0), err);
      if (ILU_ERROK(*err) && nulterm)
	{
	  _cdr_put_u8 (iiop_packet(call), 0, err);
	}
    }
}

static void
  _IIOP_OutputBytes (ilu_Call call, ilu_bytes s, ilu_cardinal len, ilu_cardinal limit, ILU_ERRS((IoErrs)) *err)
{
  OutputBytes (call, s, len, limit, ilu_FALSE, err);
}

static void
  _IIOP_InputBytes (ilu_Call call, ilu_bytes *s, ilu_cardinal *len, ilu_cardinal limit, ILU_ERRS((IoErrs)) *err)
{
  *s = NIL;
  _cdr_get_bytes (iiop_packet(call), s, len, limit, err);
}

  /*ARGSUSED*/
static ilu_cardinal SizeOfBytes (ilu_Call call, ilu_bytes i, ilu_cardinal l, ilu_cardinal limit, ilu_boolean nulterm, ilu_Error *err)
{
  ilu_cardinal n = _IIOP_SizeOfCardinal (call, l, err);
  ilu_cardinal n2;

  if (ILU_ERRNOK(*err))
    return 0;
  if (((limit > 0) && (l > limit)) || l > IIOPMaxStringSize)
    {
      DEBUG(IIOP_DEBUG,
	    (stderr, "(iiop.c:SizeOfBytes):  Attempt to pass bytestring of length %lu, which exceeds either IIOPMaxStringSize of %lu, or limit on this sequence type of %lu.\n",
	     l, IIOPMaxStringSize, limit));
      return (0);
    }

  n2 = l + (nulterm ? 1 : 0);
  iiop_incr_vop(call, n2);
  ILU_CLER(*err);
  return (n + n2);
}

  /*ARGSUSED*/
static ilu_cardinal
  _IIOP_SizeOfBytes (ilu_Call call, ilu_bytes i, ilu_cardinal l, ilu_cardinal limit, ILU_ERRS((IoErrs)) *err)
{
  return (SizeOfBytes (call, i, l, limit, ilu_FALSE, err));
}

/* ==================== string ==================== */

static void
  _IIOP_OutputString (ilu_Call call, ilu_string s, ilu_cardinal len, ilu_cardinal limit, ILU_ERRS((IoErrs)) *err)
{
  OutputBytes (call, (ilu_bytes) s, len, limit, ilu_TRUE, err);
}

static void
  _IIOP_InputString (ilu_Call call, ilu_string *s, ilu_cardinal *len, ilu_cardinal limit, ILU_ERRS((IoErrs)) *err)
{
  *s = NIL;
  _cdr_get_bytes (iiop_packet(call), (ilu_byte **) s, len, limit, err);
  if (ILU_ERROK(*err))
    *len -= 1;
}

  /*ARGSUSED*/
static ilu_cardinal
  _IIOP_SizeOfString (ilu_Call call, ilu_string i, ilu_cardinal l, ilu_cardinal limit, ILU_ERRS((IoErrs)) *err)
{
  return (SizeOfBytes (call, (ilu_bytes) i, l, limit, ilu_TRUE, err));
}

/* ==================== byte ==================== */

static void
  _IIOP_OutputByte (ilu_Call call, ilu_byte b, ILU_ERRS((IoErrs)) *err)
{
  _cdr_put_u8 (iiop_packet(call), b, err);
}

static void
  _IIOP_InputByte (ilu_Call call, ilu_byte *b, ILU_ERRS((IoErrs)) *err)
{
  _cdr_get_u8 (iiop_packet(call), b, err);
}

/*ARGSUSED*/
static ilu_cardinal
  _IIOP_SizeOfByte (ilu_Call call, ilu_byte i, ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal n = PADDING_NEC(iiop_vop(call), 1) + 1;

  iiop_incr_vop(call, n);
  ILU_CLER(*err);
  return n;
}

/* ==================== short char ==================== */

static void
_IIOP_OutputShortChar(ilu_Call call, ilu_shortcharacter b,
		      ILU_ERRS((IoErrs)) * err)
{
  _IIOP_OutputByte(call, (ilu_byte) b, err);
}

static void
_IIOP_InputShortChar(ilu_Call call, ilu_shortcharacter * b,
		     ILU_ERRS((IoErrs)) * err)
{
  _IIOP_InputByte(call, (ilu_byte *) b, err);
}

/* ARGSUSED */
static          ilu_cardinal
_IIOP_SizeOfShortChar(ilu_Call call, ilu_shortcharacter i,
		      ILU_ERRS((IoErrs)) * err)
{
  return _IIOP_SizeOfByte(call, (ilu_byte) i, err);
}

/* ==================== boolean ==================== */

static void
  _IIOP_OutputBoolean (ilu_Call call, ilu_boolean b, ILU_ERRS((IoErrs)) *err)
{
  _cdr_put_u8 (iiop_packet(call), (ilu_byte) (b ? 1 : 0), err);
}

static void
  _IIOP_InputBoolean (ilu_Call call, ilu_boolean *b, ILU_ERRS((IoErrs)) *err)
{
  ilu_byte b2;

  _cdr_get_u8 (iiop_packet(call), &b2, err);
  if (ILU_ERROK(*err))
    *b = (b2 == 0) ? FALSE : TRUE;
}

/*ARGSUSED*/
static ilu_cardinal
  _IIOP_SizeOfBoolean (ilu_Call call, ilu_boolean i, ILU_ERRS((IoErrs)) *err)
{
  return (_IIOP_SizeOfByte(call, (ilu_byte) (i ? 1 : 0), err));
}

/* ==================== opaque ==================== */

static void
  _IIOP_OutputOpaque (ilu_Call call, ilu_bytes o, ilu_cardinal len, ILU_ERRS((IoErrs)) *err)
{
  _cdr_put_opaque (iiop_packet(call), o, len, err);
}

static void
  _IIOP_InputOpaque (ilu_Call call, ilu_bytes *o, ilu_cardinal len, ILU_ERRS((IoErrs)) *err)
{
  _cdr_get_opaque (iiop_packet(call), o, len, err);
}

static ilu_cardinal
  _IIOP_SizeOfOpaque (ilu_Call call, ilu_bytes o, ilu_cardinal len, ILU_ERRS((IoErrs)) *err)
{
  iiop_incr_vop(call, len);
  ILU_CLER(*err);
  return (len);
}

/* ==================== sequence ==================== */

static void
  _IIOP_OutputSequence (ilu_Call c, ilu_cardinal sequenceLength, ilu_cardinal limit, ILU_ERRS((IoErrs)) *err)
{
  _IIOP_OutputCardinal (c, sequenceLength, err);
}

static void
  _IIOP_OutputSequenceMark (ilu_Call c, ilu_cardinal extent, ILU_ERRS((IoErrs)) *err)
{
  ILU_CLER(*err);
}

static void
  _IIOP_InputSequenceMark (ilu_Call c, ilu_cardinal extent, ILU_ERRS((IoErrs)) *err)
{
  ILU_CLER(*err);
}

static void
  _IIOP_InputSequence (ilu_Call c, ilu_cardinal *sequenceLength, ilu_cardinal limit, ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal len;

  _IIOP_InputCardinal (c, &len, err);
  if (ILU_ERROK(*err))
    *sequenceLength = len;
}

static void
  _IIOP_EndSequence (ilu_Call c, ILU_ERRS((IoErrs)) *err)
{
  ILU_CLER(*err);
}

static ilu_cardinal
  _IIOP_SizeOfSequence (ilu_Call c, ilu_cardinal length, ilu_cardinal limit, ILU_ERRS((IoErrs)) *err)
{
  return (_IIOP_SizeOfCardinal(c, length, err));
}

/* ==================== union ==================== */

static void
  _IIOP_OutputUnion (ilu_Call c, ilu_cardinal typeIndex,
		     ilu_cardinal dsize,
		     ILU_ERRS((IoErrs)) *err)
{
  switch (dsize) {
  case 1:
    _IIOP_OutputByte (c, (ilu_byte) typeIndex, err);
    break;
  case 2:
    _IIOP_OutputShortCardinal (c, (ilu_shortcardinal) typeIndex, err);
    break;
  case 4:
    _IIOP_OutputCardinal (c, (ilu_cardinal) typeIndex, err);
    break;
  default:
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_unionDiscSize, 0);
  };
}

static void
  _IIOP_InputUnion (ilu_Call c, ilu_cardinal *typeIndex,
		    ilu_cardinal dsize,
		    ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal i;
  ilu_byte b;
  ilu_shortcardinal s;

  switch (dsize)
    {
    case 1:
      _IIOP_InputByte (c, &b, err);
      break;
    case 2:
      _IIOP_InputShortCardinal (c, &s, err);
      break;
    case 4:
      _IIOP_InputCardinal (c, &i, err);
      break;
    default:
      ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_unionDiscSize, 0);
    };
  if (ILU_ERROK(*err))
    {
      switch (dsize)
	{
	case 1:
	  *typeIndex = b;
	  break;
	case 2:
	  *typeIndex = s;
	  break;
	case 4:
	  *typeIndex = i;
	  break;
	default:
	  ;
	};
    }
}

static void
  _IIOP_EndUnion (ilu_Call c, ILU_ERRS((IoErrs)) *err)
{
  ILU_CLER(*err);
}

static ilu_cardinal
  _IIOP_SizeOfUnion (ilu_Call c, ilu_cardinal typeIndex,
		     ilu_cardinal dsize,
		     ILU_ERRS((IoErrs)) *err)
{
  switch (dsize)
    {
    case 1:
      return (_IIOP_SizeOfByte(c, (ilu_byte) typeIndex, err));
    case 2:
      return (_IIOP_SizeOfShortCardinal(c, (ilu_shortcardinal) typeIndex, err));
    case 4:
      return (_IIOP_SizeOfCardinal(c, typeIndex, err));
    default:
      ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_unionDiscSize, 0);
    }
  return 0;
}

/* ==================== array ==================== */

static void
  _IIOP_OutputArray (ilu_Call c, ilu_cardinal len, ILU_ERRS((IoErrs)) *err)
{
  ILU_CLER(*err);
}

static void
  _IIOP_InputArray (ilu_Call c, ILU_ERRS((IoErrs)) *err)
{
  ILU_CLER(*err);
}

static void
  _IIOP_EndArray (ilu_Call c, ILU_ERRS((IoErrs)) *err)
{
  ILU_CLER(*err);
}

static ilu_cardinal
  _IIOP_SizeOfArray (ilu_Call c, ilu_cardinal len, ILU_ERRS((IoErrs)) *err)
{
  ILU_CLER(*err);
  return(0);
}

/* ==================== record ==================== */

static void
  _IIOP_OutputRecord (ilu_Call c, ILU_ERRS((IoErrs)) *err)
{
  ILU_CLER(*err);
}

static void
  _IIOP_InputRecord (ilu_Call c, ILU_ERRS((IoErrs)) *err)
{
  ILU_CLER(*err);
}

static void
  _IIOP_EndRecord (ilu_Call c, ILU_ERRS((IoErrs)) *err)
{
  ILU_CLER(*err);
}

static ilu_cardinal
  _IIOP_SizeOfRecord (ilu_Call c, ILU_ERRS((IoErrs)) *err)
{
  ILU_CLER(*err);
  return(0);
}

/* ==================== object ==================== */

/*L2 >= {call's connection's callmu, iomu}
  h != NIL => all the following:
  before: Inside(s, cl);
  after:				 L1 disjoint {cmu, s};
  after: cl collectible		      => L1  not >=  {gcmu};
  after: cl collectible & s surrogate => Main Invariant holds;
  where s = h's server and cl = h's type.
  (We don't really need to hold cmu for surrogate or non-collectible
   objects, but this is convenient because ilu_Enter/ExitServer can
   be used.)*/
static ilu_boolean
  _IIOP_OutputObjectID (ilu_Call call, ilu_Object h,
			ilu_boolean discriminator_p,
			ilu_Class static_type, ILU_ERRS((IoErrs)) *err)
{
  ilu_bytes	ostr = NIL;	/* discriminator_p ? OID : SBH */
  ilu_cardinal	ostr_len = 0;	/* length of ostr buffer */
  struct IIOP_IOR_IOR	*iorp;
  ilu_string	tstr = NIL;	/* most specific type string */
  ilu_boolean	is_nil = (h == NIL);

  tstr = (h != NIL) ? ((object_mstid(h) != NIL) ? object_mstid(h) : class_unique_id(object_class(h))) : "";
  DEBUG(OBJECT_DEBUG,
	(stderr,
	 "(ilu:_IIOP_OutputObjectID:  m-s type id for %s/%s is <%s>\n",
	 server_id(object_server(h)), object_ih(h), tstr));

  if (discriminator_p)
    {
      ostr = iiop_objKey(call);
      ostr_len = iiop_objKeyLen(call);
      iiop_objKey(call) = NIL;
    }
  else {
    if (h == NIL && (discriminator_p || !static_type->cl_optional))
      return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_nil, 0);
    iorp = FindIORForObject(h);
    /* We just sized this object; there'd better be an IOR for it! */
    _ilu_Assert(iorp!=NIL,"unexpected NIL IOR pointer");
  }

  if (ostr == NIL && iorp->type_id == NIL) {
    ilu_ExitServer(object_server(h), object_class(h));
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_broken, FALSE);
  }
  if (!is_nil) {
    if (object_is_true(h) && object_collectible(h)) {
      object_lastRemote(h) = ilu_CoarseTime_Now();
      *err = _ilu_TouchedObj(h);
      ILU_MUST_BE_SUCCESS(*err);
    }
    ilu_ExitServer(object_server(h), object_class(h));
  }
  if (discriminator_p)
    {
      char buf[2048];

      protocol_output_bytes(call_proto(call), call, ostr, ostr_len, 0xFFFF, err);
      ilu_free(ostr);
      if (ILU_ERRNOK(*err))
	return FALSE;
      FormMethodName (buf, call_method(call));
      /* method name */
      if (protocol_output_string(call_proto(call), call, buf, strlen(buf), 0xFFFF, err), ILU_ERRNOK(*err))
	return FALSE;
      /* principal */
      if (protocol_output_bytes(call_proto(call), call, (ilu_bytes) buf, 0, 0xFFFF, err), ILU_ERRNOK(*err))
	return FALSE;
    }
  else
    {
      ilu_cardinal i;

      if (protocol_output_string (call_proto(call), call, iorp->type_id, strlen(iorp->type_id), 0xFFFF, err),
	  ILU_ERRNOK(*err))
	return FALSE;
      if (protocol_output_cardinal (call_proto(call), call, iorp->nProfiles, err), ILU_ERRNOK(*err))
	return FALSE;
      for (i = 0;  i < iorp->nProfiles;  i++)
	{
	  if (protocol_output_cardinal (call_proto(call), call, iorp->Profile[i].tag, err), ILU_ERRNOK(*err))
	    return FALSE;
	  if (protocol_output_bytes(call_proto(call), call, iorp->Profile[i].profileData,
				    iorp->Profile[i].profileDataLen, 0xFFFF, err), ILU_ERRNOK(*err))
	    return FALSE;
	}
    }
  return (ILU_CLER(*err));
}

/*before: L1 = {},
  after:  *h!=NIL => Inside(*h's server, static_type);
  after:  *h==NIL => L1 = {};
  forall conn: (L2 >= {conn.iomu}) => (L2 >= {conn.callmu});
  L2 >= {call's connection's callmu, iomu};
  Main otherwise unconstrained*/
static ilu_boolean
  _IIOP_InputObjectID (ilu_Call call, ilu_Object *h,
		       ilu_boolean discriminator_p,
		       ilu_Class static_type,
		       ILU_ERRS((IoErrs)) *err)
{
  ilu_Server	server = connection_server(call_connection(call));

  *h = NIL;
  if (static_type == NIL)
    {
      DEBUG(INCOMING_DEBUG | OBJECT_DEBUG,
	    (stderr, "_IIOP_InputObjectID(disc=%ld, static_type=NIL)\n",
	     (long) discriminator_p));
      return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_nil, FALSE);
    };

  if (discriminator_p)
    {
      ilu_string	ih = NIL, sid = NIL, mstid = NIL;
      ilu_string	cinfo = NIL;
      ilu_cardinal	cinfolen = 0;
      ilu_bytes		key;
      ilu_cardinal	keylen;

      /* with the IIOP, the discriminator (or "object key") is input during the
	 InterpretRequest call, and stored in iiop_objKey(call), with the length
	 in iiop_objKeyLen(call). */

      key = iiop_objKey(call);
      keylen = iiop_objKeyLen(call);
      iiop_objKey(call) = NIL;
      iiop_objKeyLen(call) = 0;

      if (key == NIL || strncmp((ilu_string) key, "ilu", 4) != 0)
	{
	  DEBUG(INCOMING_DEBUG,
		(stderr,
		 "_IIOP_InputObjectID:  incoming oid not an ILU object-key"));
	  ILU_ERR_CONS1(marshal, err, minor, ilu_mm_alien_disc, 6);
	}
      else
	{
	  mstid = ((char *) key) + 4;
	  sid = mstid + strlen(mstid) + 1;
	  ih = sid + strlen(sid) + 1;

	  ilu_EnterServer(server, static_type);
	  if (strcmp(sid, server_id(server)) != 0) {
	    DEBUG(INCOMING_DEBUG,
		  (stderr,
		   "%s %s is for wrong server (not %s).\n",
		   "_IIOP_InputObjectID:  incoming oid sid", sid,
		   server_id(server)));
	    ILU_ERR_CONS1(marshal, err, minor, ilu_mm_alien_disc, 6);
	    ilu_ExitServer(server, static_type);
	  } else if (server_objs(server) == NIL) {
	    DEBUG(INCOMING_DEBUG,
		  (stderr, "%s %s is in closed server %s.\n",
		   "_IIOP_InputObjectID:  instance", ih, sid));
	    ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_svr_closed, NIL);
	    ilu_ExitServer(server, static_type);
	  } else if ((*h = _ilu_FindObjectInServer(ih, server)) == NIL) {
	    DEBUG(INCOMING_DEBUG,
		  (stderr, "%s %s not found in server %s.\n",
		   "_IIOP_InputObjectID:  instance", ih, sid));
	    ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_inst_nf, NIL);
	    ilu_ExitServer(server, static_type);
	  }
	}
      ilu_free(key);
      if (ILU_ERRNOK(*err)) {
	return FALSE;
      }
    }
  else
    {
      /* not discriminant */

      ilu_string typeid;
      ilu_cardinal nprofiles;
      struct IIOP_IOR_IOR *p;
      ilu_cardinal len;

      if (protocol_input_string(call_proto(call), call, &typeid, &len, 0xFFFF, err), ILU_ERRNOK(*err))
	return FALSE;
      if (protocol_input_cardinal(call_proto(call), call, &nprofiles, err), ILU_ERRNOK(*err))
	return FALSE;
      if (p = ilu_MallocE(sizeof(struct IIOP_IOR_IOR) + ((nprofiles - 1) * sizeof(struct IIOP_IOR_TaggedProfile)), err),
	  ILU_ERRNOK(*err))
	return FALSE;
      p->type_id = typeid;
      p->nProfiles = 0;
      if (nprofiles == 0)
	{
	  ilu_free(typeid);
	  ilu_free(p);
	  *h = NIL;
	}	  
      else
	{
	  ilu_cardinal i;

	  for (i = 0;  i < nprofiles;  i++)
	    {
	      if (protocol_input_cardinal(call_proto(call), call, &p->Profile[i].tag, err), ILU_ERRNOK(*err))
		{
		  FreeIORData(p);
		  ilu_free(p);
		  return FALSE;
		}
	      p->Profile[i].profileData = NIL;
	      if (protocol_input_bytes(call_proto(call), call, &p->Profile[i].profileData,
				       &p->Profile[i].profileDataLen, 0xFFFF, err), ILU_ERRNOK(*err))
		{
		  FreeIORData(p);
		  ilu_free(p);
		  return FALSE;
		}
	      p->nProfiles++;
	    }
	  *h = ObjectOfIOR (p, static_type, err);
	  if (ILU_ERRNOK(*err))
	    {
	      DEBUG(INCOMING_DEBUG,
		    (stderr, "_IIOP_InputObjectID:  error:  No object for IOR.\n"));
	      return FALSE;
	    }
	  else if (*h != NIL)
	    PossiblyRegisterIORForObject (*h, p);
	  else
	    {
	      FreeIORData(p);
	      ilu_free(p);
	      return FALSE;
	    }	    
	}
      
      if (*h == NIL) {
	if (static_type->cl_optional) {
	  return TRUE;
	} else {
	  DEBUG(INCOMING_DEBUG,
		(stderr, "_IIOP_InputObjectID:  bad NIL obj.\n"));
	  return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_nil, FALSE);
	}
      }
    }
  return TRUE;
}

/*h!=NIL => L1 >= {obj's server};
  h==NIL => L1 unconstrained.
  L2, Main unconstrained*/
static ilu_cardinal _IIOP_SizeOfObjectID(ilu_Call call, ilu_Object h,
					ilu_boolean discriminator_p,
					ilu_Class static_type,
					ilu_Error *err)
{
  ilu_bytes	ostr = NIL;	/* discriminator_p ? OID : SBH */
  ilu_cardinal	ostr_len = 0;	/* length of ostr buffer */
  struct IIOP_IOR_IOR	*iorp;
  ilu_string	tstr = NIL;	/* most specific type string */
  ilu_cardinal	size = 0, size2 = 0;
  ilu_boolean	is_nil = (h == NIL);

  if (call_connection(call) == NIL) {
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_broken, 0);
  }

  tstr = (h != NIL) ? ((object_mstid(h) != NIL) ? object_mstid(h) : class_unique_id(object_class(h))) : "";
  DEBUG(OBJECT_DEBUG,
	(stderr,
	 "(ilu:_IIOP_SizeOfObjectID:  m-s type id for %s/%s is <%s>\n",
	 server_id(object_server(h)), object_ih(h), tstr));

  if (discriminator_p)
    {
      if (strcmp(h->ob_ih, "$") == 0)			/* non-ILU object */
	{
	  ostr = decode_foreign_object_key(h->ob_server->sr_id, &ostr_len, err);
	  if (ILU_ERRNOK(*err)) return 0;
	}
      else
	{
	  int sidl = strlen(server_id(object_server(h))), ihl = strlen(object_ih(h)), mstidl = strlen(tstr);

	  ostr_len = sidl + ihl + mstidl + 7;
	  ostr = (ilu_bytes) ilu_malloc (ostr_len);
	  if (ostr == NIL)
	    return ILU_ERR_CONS1(no_memory, err, nbytes, ostr_len, 0);
	  memcpy ((void *) ostr, "ilu", 4);
	  memcpy ((void *) (ostr + 4), tstr, mstidl + 1);
	  memcpy ((void *) (ostr + mstidl + 5), server_id(object_server(h)), sidl + 1);
	  memcpy ((void *) (ostr + mstidl + sidl + 6), object_ih(h), ihl + 1);
	}
    }
  else {
    if (h == NIL && (discriminator_p || !static_type->cl_optional))
      return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_nil, 0);
    if ((iorp = FindIORForObject(h)) == NIL)
      {
	if ((*err = IOROfObject(h, &iorp)), ILU_ERRNOK(*err))
	  return (0);
	RegisterIORForObject(h, iorp);
      }
  }

  if (ostr == NIL && iorp == NIL)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_broken, 0);

  if (!is_nil) {
    if (object_is_true(h) && object_collectible(h)) {
      object_lastRemote(h) = ilu_CoarseTime_Now();
      *err = _ilu_TouchedObj(h);
      ILU_MUST_BE_SUCCESS(*err);
    }
  }
  if (discriminator_p)
    {
      char buf[2048];
      ilu_cardinal size3;

      iiop_vop(call) = 0;
      size3 = 4 + 1 + 1 + 1 + 1 + 4;	/* magic + major version + minor version + msg type + byte order + size */
      size3 += 4;			/* service context */
      size3 += 4;			/* serial number ("request id") */
      size3 += 1;			/* response expected? */
      iiop_vop(call) = (ilu_bytes) size3;
      size3 = _IIOP_SizeOfBytes (call, (ilu_bytes) ostr, ostr_len, 0xFFFF, err);
      if (ILU_ERRNOK(*err))
	return 0;
      size += size3;
      FormMethodName (buf, call_method(call));
      size3 = _IIOP_SizeOfString (call, buf, strlen(buf), 0xFFFF, err);
      if (ILU_ERRNOK(*err))
	return 0;
      size += size3;
      size3 = _IIOP_SizeOfBytes (call, (ilu_bytes) buf, 0, 0xFFFF, err);
      if (ILU_ERRNOK(*err))
	return 0;
      size += size3;
      iiop_objKey(call) = ostr;
      iiop_objKeyLen(call) = ostr_len;
    }
  else
    {
      ilu_cardinal i;
      ilu_cardinal size3;

      size3 = _IIOP_SizeOfString (call, iorp->type_id, strlen(iorp->type_id), 0xFFFF, err);
      if (ILU_ERRNOK(*err)) return 0;
      size += size3;
      size3 = _IIOP_SizeOfCardinal (call, iorp->nProfiles, err);
      if (ILU_ERRNOK(*err)) return 0;
      size += size3;
      for (i = 0;  i < iorp->nProfiles;  i++)
	{
	  size3 = _IIOP_SizeOfCardinal (call, iorp->Profile[i].tag, err);
	  if (ILU_ERRNOK(*err)) return 0;
	  size += size3;
	  size3 = _IIOP_SizeOfBytes (call, iorp->Profile[i].profileData,
				     iorp->Profile[i].profileDataLen, 0xFFFF, err);
	  if (ILU_ERRNOK(*err)) return 0;
	  size += size3;
	}
    }
  DEBUG(IIOP_DEBUG, (stderr, "_IIOP_SizeOfObjectID (%p, %p, %s, %s) => %lu\n",
		     call, h, discriminator_p ? "is disc" : "not disc",
		     class_name(static_type), size));
  ILU_CLER(*err);
  return (size);
}

/*======================================================================
**======================================================================
**====================  Non-I/O code ===================================
**======================================================================
**====================================================================*/

/* L1, L2 unconstrained */
static char *
  _IIOP_MessageTypeName (int message_type)
{
  char *message_type_names[] = {
    "Request", "Reply", "CancelRequest",
    "LocateRequest", "LocateReply",
    "CloseConnection", "MessageError" };

  if (message_type < 0 OR message_type > (sizeof(message_type_names)/sizeof(char *)))
    return NIL;
  else
    return message_type_names[message_type];
}

/*L2, Main unconstrained*/
/*L1_sup < prmu*/
static ilu_string _IIOP_FormProtocolHandle (ilu_Object obj)
{
  char buf[100];

  sprintf (buf, "iiop_%u_%u_1", IIOPDefaultMajorVersion, IIOPDefaultMinorVersion);
  return (_ilu_Strdup(buf));
}
     
static void _IIOP_EndMessage (ilu_Transport bs,
			      ilu_boolean input_p,
			      ilu_Error *err)
{
  ILU_CLER(*err);
  if (bs->tr_class->tc_boundaried)
    (void) transport_end_message(bs, ilu_TRUE, NIL, err);
  else if (! input_p)
    bs->tr_class->tc_write_bytes (bs, NIL, 0, TRUE, err);
}

static ilu_boolean
  _IIOP_SendErrorMessage (ilu_Transport bs,
			  ilu_Error *err)
{
  struct {
    unsigned char magic[4];
    unsigned char major_version;
    unsigned char minor_version;
    unsigned char byte_order;
    unsigned char msg_type;
    ilu_cardinal  msg_size;
  } msgheader;
  PACKET p = NIL;

  p = _cdr_CreatePacket (bs, NATIVE_BYTE_ORDER, 0, err);
  if (ILU_ERRNOK(*err))
    return FALSE;

  if (bs->tr_class->tc_boundaried)
    {
      ilu_ReadHeaderResultCode ans;
      ans = transport_begin_message(bs, FALSE, err);
      switch (ans) {
      case ilu_rhrc_ok:
      case ilu_rhrc_eof:
      case ilu_rhrc_nothing:
      case ilu_rhrc_handled:
	break;
      case ilu_rhrc_error:
	DEBUG((INCOMING_DEBUG | IIOP_DEBUG),
	      (stderr, "_IIOP_SendErrorMessage:  error %s "
	       "on transport_begin_message (output)\n",
	       ILU_ERR_NAME(*err)));
	goto errcase;
      default:
	_ilu_Assert(FALSE, "_IIOP_SendErrorMessage: bad ilu_ReadHeaderResultCode value");
      }
    }

  msgheader.magic[0] = 'G';
  msgheader.magic[1] = 'I';
  msgheader.magic[2] = 'O';
  msgheader.magic[3] = 'P';
  msgheader.major_version = 1;
  msgheader.minor_version = 0;
  msgheader.byte_order = (NATIVE_BYTE_ORDER == LittleEndian);
  msgheader.msg_type = GIOP_PacketType_MessageError;
  msgheader.msg_size = 0;

  if (transport_write_bytes(bs, (ilu_bytes) &msgheader, sizeof(msgheader), err), ILU_ERRNOK(*err))
    goto errcase;
  _IIOP_EndMessage (bs, FALSE, err);
  if (ILU_ERRNOK(*err))
    goto errcase;

  if (p != NIL)
    { _cdr_destroy (p, err); ilu_free(p); }

  return ILU_ERROK(*err);

 errcase:
  DEBUG(IIOP_DEBUG,
	(stderr, "_IIOP_SendErrorMessage:  error %s on attempt to reply.\n",
	 ILU_ERR_NAME(*err)));
  if (p != NIL)
    { ilu_Error lerr; _cdr_destroy (p, &lerr); ILU_HANDLED(lerr); ilu_free(p); }
  return FALSE;
}

static ilu_boolean
  _IIOP_SendLocateReply (ilu_Transport bs,
			 ilu_cardinal serialNumber,
			 ilu_bytes object_key,
			 ilu_cardinal object_key_len,
			 ilu_Error *err)
{
  ilu_Server server = NIL;
  struct IIOP_IOR_IOR *ior = ILU_NIL;
  enum IIOP_LocateStatus status = IIOP_UNKNOWN_OBJECT;
  PACKET p = NIL;
  struct {
    unsigned char magic[4];
    unsigned char major_version;
    unsigned char minor_version;
    unsigned char byte_order;
    unsigned char msg_type;
    ilu_cardinal  msg_size;
    ilu_cardinal  request_id;
    ilu_cardinal  locate_status;
  } msgheader;

  DEBUG(IIOP_DEBUG,
	(stderr, "(ilu_IIOP:SendLocateReply): %lu, 0x%p (%ld bytes)\n",
	 serialNumber, object_key, (long) object_key_len));

  if (strncmp((ilu_string) object_key, "ilu", 4) == 0)
    {
      ilu_string mstid;
      ilu_string sid;
      ilu_string ih;
      ilu_Object h;

      /* We have an ILU object, go ahead and transform it */
      
      mstid = ((char *) object_key) + 4;
      sid = mstid + strlen(mstid) + 1;
      ih = sid + strlen(sid) + 1;

      server = _ilu_FindServer(sid, ilu_FALSE, NIL, 0, err);
      if (ILU_ERRNOK(*err))
	{
	  DEBUG(IIOP_DEBUG,
		(stderr, "_IIOP_SendLocateReply:  "
		 "No local server with sid \"%s\".\n", sid));
	  status = IIOP_UNKNOWN_OBJECT;
	}
      else
	{
	  ilu_EnterServer (server, _ilu_rootClass);
	  if (server_objs(server) == NIL)
	    {
	      DEBUG(IIOP_DEBUG,
		    (stderr, "%s %s is in closed server %s.\n",
		     "_IIOP_SendLocateReply:  instance", ih, sid));
	      status = IIOP_UNKNOWN_OBJECT;
	    }
	  else if ((h = _ilu_FindObjectInServer(ih, server)) == NIL)
	    {
	      DEBUG(IIOP_DEBUG,
		    (stderr, "%s %s not found in server %s.\n",
		     "_IIOP_SendLocateReply:  instance", ih, sid));
	      status = IIOP_UNKNOWN_OBJECT;
	    }
	  else if (ilu_TrueInstanceP(h))
	    status = IIOP_OBJECT_HERE;
	  else
	    {
	      *err = IOROfObject (h, &ior);
	      if (ILU_ERRNOK(*err))
		status = IIOP_UNKNOWN_OBJECT;
	      else
		status = IIOP_OBJECT_FORWARD;
	      ILU_HANDLED(*err);
	    }
	  ilu_ExitServer(server, _ilu_rootClass);
	}
    }

#ifdef ENABLE_DEBUGGING
  if ((_ilu_DebugLevel & IIOP_DEBUG) != 0)
    {
      ilu_string encoded_obj_key = encode (object_key, object_key_len);
      ilu_DebugPrintf ("_IIOP_SendLocateReply:  replying with %s to LocateRequest for <%s>\n",
		       (status == IIOP_UNKNOWN_OBJECT) ? "UNKNOWN_OBJECT" :
		       (status == IIOP_OBJECT_FORWARD) ? "OBJECT_FORWARD" :
		       (status == IIOP_OBJECT_HERE) ? "OBJECT_HERE" : "???",
		       encoded_obj_key);
      ilu_free(encoded_obj_key);
    }
#endif

  p = _cdr_CreatePacket (bs, NATIVE_BYTE_ORDER, 0, err);
  if (ILU_ERRNOK(*err))
    return FALSE;

  if (bs->tr_class->tc_boundaried)
    {
      ilu_ReadHeaderResultCode ans;
      ans = transport_begin_message(bs, FALSE, err);
      switch (ans) {
      case ilu_rhrc_ok:
      case ilu_rhrc_eof:
      case ilu_rhrc_nothing:
      case ilu_rhrc_handled:
	break;
      case ilu_rhrc_error:
	DEBUG((INCOMING_DEBUG | IIOP_DEBUG),
	      (stderr, "_IIOP_SendLocateReply:  error %s "
	       "on transport_begin_message (output)\n",
	       ILU_ERR_NAME(*err)));
	goto errcase;
      default:
	_ilu_Assert(FALSE, "_IIOP_SendLocateReply: bad ilu_ReadHeaderResultCode value");
      }
    }

  msgheader.magic[0] = 'G';
  msgheader.magic[1] = 'I';
  msgheader.magic[2] = 'O';
  msgheader.magic[3] = 'P';
  msgheader.major_version = 1;
  msgheader.minor_version = 0;
  msgheader.byte_order = (NATIVE_BYTE_ORDER == LittleEndian);
  msgheader.msg_type = GIOP_PacketType_LocateReply;
  msgheader.msg_size = sizeof(msgheader) - 12;
  msgheader.request_id = serialNumber;
  msgheader.locate_status = status;

  if (status == IIOP_OBJECT_FORWARD)
    {
      /* adjust size to allow for marshalled IOR */

      int size, i;
      size = strlen(ior->type_id) + 1;
      size = 4 + size + PAD4(size);	/* type id */
      size += 4;			/* nprofiles */
      for (i = 0;  i < ior->nProfiles;  i++)
	{
	  size += 4;	/* tag */
	  size += 4 + ior->Profile[i].profileDataLen;
	  if ((i + 1) < ior->nProfiles)
	    size += PAD4(ior->Profile[i].profileDataLen);
	}
      msgheader.msg_size += size;
    }

  if (transport_write_bytes(bs, (ilu_bytes) &msgheader, 20, err), ILU_ERRNOK(*err))
    goto errcase;
  if (status == IIOP_OBJECT_FORWARD)
    {
      int i;
      
      if ((_cdr_put_bytes(p, (ilu_bytes)(ior->type_id), strlen(ior->type_id) + 1, err), ILU_ERRNOK(*err)) OR
	  (_cdr_put_u32 (p, ior->nProfiles, err), ILU_ERRNOK(*err)))
	goto errcase;
      for (i = 0;  i < ior->nProfiles;  i++)
	{
	  if ((_cdr_put_u32 (p, ior->Profile[i].tag, err), ILU_ERRNOK(*err)) OR
	      (_cdr_put_bytes (p, ior->Profile[i].profileData, ior->Profile[i].profileDataLen, err), ILU_ERRNOK(*err)))
	    goto errcase;
	}
    }
  _IIOP_EndMessage (bs, FALSE, err);
  if (ILU_ERRNOK(*err))
    goto errcase;

  if (p != NIL)
    { _cdr_destroy (p, err); ilu_free(p); }

  return ILU_ERROK(*err);

 errcase:
  DEBUG(IIOP_DEBUG,
	(stderr, "_IIOP_SendLocateReply:  error %s on attempt to reply.\n",
	 ILU_ERR_NAME(*err)));
  if (p != NIL)
    { ilu_Error lerr; _cdr_destroy (p, &lerr); ILU_HANDLED(lerr); ilu_free(p); }
  return FALSE;
}

/*Main Invariant holds*/
/*L2 >= {conn's iomu}*/

static ilu_boolean
  _IIOP_CheckBoundaries (ilu_Call call, int msgType, ilu_Error *err)
{
  ilu_Transport bs = iiop_transport(call);
  PACKET p = iiop_packet(call);
  int extra = (iiop_size(call) + 12) - (iiop_vop(call) - (ilu_byte *) 0);
  if (extra != 0)
    {
      DEBUG(IIOP_DEBUG,
	    (stderr, "_IIOP_CheckBoundaries:  Warning:  "
	     "Received IIOP \"%s\" message on connection \"%s\" "
	     "with %d bytes of trailing garbage.\n",
	     _IIOP_MessageTypeName(msgType),
	     call_connection_id(call), extra));
      if (extra > 0)
	{
	  /* read in the extra bytes and discard them */
	  ilu_bytes foo = NIL;
	  _cdr_get_opaque (p, &foo, extra, err);
	  ilu_free(foo);
	}
      else if (extra < 0)
	{ /* ???? */ };
    }
  if (ILU_ERROK(*err))
    {
      _IIOP_EndMessage (bs, TRUE, err);

      /* XXX -- should send error message at this point,
	 but will break some current implementations, such
	 as Black Widow 2.0beta. */
#if 0
      if (ILU_ERROK(*err) && extra != 0)
	_IIOP_SendErrorMessage (bs, err);
#endif
    }
  return ILU_ERROK(*err);
}


static ilu_ReadHeaderResultCode
  _IIOP_ReadHeader (ilu_Call call,
		    ilu_PacketType *type,
		    ilu_cardinal *sn,
		    ILU_ERRS((IoErrs)) *err)
{
  ilu_Transport bs = iiop_transport(call);
  ilu_byte header[8];
  ilu_byte ptype;
  ilu_TransportReport report = { ilu_FALSE, ilu_FALSE };
  ilu_cardinal serialNumber = 0xFFFFFFFF, size;
  ilu_ReadHeaderResultCode ans;
  PACKET p = iiop_packet(call);

  if (bs->tr_class->tc_boundaried)
    {
      ans = transport_begin_message(bs, TRUE, err);
      switch (ans) {
      case ilu_rhrc_ok:
	break;
      case ilu_rhrc_error:
	DEBUG((INCOMING_DEBUG | IIOP_DEBUG),
	      (stderr, "%s:  error %s on transport_begin_message (input)\n",
	       "_IIOP_ReadHeader", ILU_ERR_NAME(*err)));
	ILU_HANDLED(*err);
	return ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_protocol_sync_lost, ilu_rhrc_error);
      case ilu_rhrc_eof:
      case ilu_rhrc_nothing:
      case ilu_rhrc_handled:
	return (ans);
      default:
	_ilu_Assert(FALSE, "iiop.c:ReadHeader -- bad ilu_ReadHeaderResultCode value");
      }
    }

  iiop_vop(call) = 0;

  /* Read bytes specially here, because we need to distinguish between EOF and
     no-data-available-without-blocking. */

  if ((size = transport_read_upto_bytes (bs, header, 8, &report, err)), ILU_ERRNOK(*err))
    return ilu_rhrc_error;
  else if (report.tr_eof)
    return ilu_rhrc_eof;
  else if ((size == 0) || report.tr_eom)
    return ilu_rhrc_nothing;
  else if (size != 8)
    {
      DEBUG(IIOP_DEBUG,
	    (stderr, "_IIOP_ReadHeader:  partial message header encountered.  %ld bytes.\n",
	     (long) size));
      return ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_protocol_sync_lost, ilu_rhrc_error);
    };

  iiop_vop(call) += 8;

  if (header[0] != 'G' ||
      header[1] != 'I' ||
      header[2] != 'O' ||
      header[3] != 'P')
    goto marshalError;

  if (header[4] != iiop_major_version(call) ||
      header[5] != iiop_minor_version(call))
    goto marshalError;

  ptype = header[7];
  if (ptype == GIOP_PacketType_Request)
    *type = ilu_PacketType_Request;
  else if (ptype == GIOP_PacketType_Reply)
    *type = ilu_PacketType_Reply;

  iiop_byte_order(call) = (header[6] == 1) ? LittleEndian : BigEndian;
  iiop_packetType(call) = ptype;

  _cdr_get_u32 (p, &iiop_size(call), err);
  if (ILU_ERRNOK(*err))
    return (ilu_rhrc_error);

  /* read the serial number... */
  switch (ptype)
    {
    case GIOP_PacketType_Reply:
    case GIOP_PacketType_Request:
      {
	/* skip any service context */
	ilu_cardinal i, count, id, datalen;
	ilu_bytes data = NIL;

	if (!(_cdr_get_u32 (p, &count, err), ILU_ERROK(*err))) goto marshalError;
	for (i = 0;  i < count;  i++)
	  {
	    data = NIL;
	    if (!(_cdr_get_u32 (p, &id, err), ILU_ERROK(*err)) OR
		!(_cdr_get_bytes (p, &data, &datalen, 0, err), ILU_ERROK(*err)))
	      goto marshalError;
	    ilu_free(data);
	  }

	/* now read the serial number */
	if (_cdr_get_u32 (p, &serialNumber, err), ILU_ERRNOK(*err))
	  goto marshalError;
      }
      break;

    case GIOP_PacketType_CancelRequest:
      {
	ilu_cardinal req_to_cancel;
	if (_cdr_get_u32 (p, &req_to_cancel, err), ILU_ERRNOK(*err))
	  goto marshalError;
	_IIOP_CheckBoundaries(call, GIOP_PacketType_CancelRequest, err);
	DEBUG(IIOP_DEBUG,
	      (stderr, "_IIOP_ReadHeader:  Cancel for request %lu received and discarded.\n",
	       (unsigned long) req_to_cancel));
      }
      break;

    case GIOP_PacketType_LocateRequest:
      {
	ilu_bytes object_key = NIL;
	ilu_cardinal object_key_len = 0;

	if ((_cdr_get_u32 (p, &serialNumber, err), ILU_ERRNOK(*err)) OR
	    (_cdr_get_bytes (p, &object_key, &object_key_len, 0, err), ILU_ERRNOK(*err)))
	  goto marshalError;
	_IIOP_CheckBoundaries (call, GIOP_PacketType_LocateRequest, err);
#ifdef ENABLE_DEBUGGING
	if ((_ilu_DebugLevel & IIOP_DEBUG) != 0)
	  {
	    ilu_string encoded_object_key;
	    encoded_object_key = encode(object_key, object_key_len);
	    ilu_DebugPrintf ("_IIOP_ReadHeader:  LocateRequest for object <%s> received.\n",
			     encoded_object_key);
	    ilu_free(encoded_object_key);
	  }
#endif /* ENABLE_DEBUGGING */
	_IIOP_SendLocateReply (bs, serialNumber, object_key, object_key_len, err);
	ilu_free(object_key);
      }
      break;

    case GIOP_PacketType_LocateReply:
      {
	ilu_cardinal locateStatus;

	if ((_cdr_get_u32 (p, &serialNumber, err), ILU_ERRNOK(*err)) OR
	    (_cdr_get_u32 (p, &locateStatus, err), ILU_ERRNOK(*err)))
	  goto marshalError;
	if (locateStatus == IIOP_OBJECT_FORWARD)
	  {
	    ilu_string type_id = NIL;
	    ilu_cardinal nProfiles, i, profile_tag, profile_len;
	    ilu_bytes profile_data;

	    if ((_cdr_get_bytes (p, (ilu_bytes *) &type_id, &nProfiles, 0xFFFF, err), ILU_ERRNOK(*err)) OR
		(ilu_free(type_id), _cdr_get_u32 (p, &nProfiles, err), ILU_ERRNOK(*err)))
	      goto marshalError;
	    for (i = 0;  i < nProfiles;  i++)
	      {
		profile_data = NIL;
		if ((_cdr_get_u32 (p, &profile_tag, err), ILU_ERRNOK(*err)) OR
		    (_cdr_get_bytes (p, &profile_data, &profile_len, 0xFFFF, err), ILU_ERRNOK(*err)))
		  goto marshalError;
		ilu_free(profile_data);
	      }
	  }
	_IIOP_CheckBoundaries (call, GIOP_PacketType_LocateReply, err);
	DEBUG(IIOP_DEBUG,
	      (stderr, "_IIOP_ReadHeader:  LocateReply for request %lu received, status %lu.\n",
	       (unsigned long) serialNumber, (unsigned long) locateStatus));
      }
      break;

    case GIOP_PacketType_MessageError:
      {
	serialNumber = 0;
	_IIOP_CheckBoundaries (call, GIOP_PacketType_MessageError, err);
	DEBUG(IIOP_DEBUG,
	      (stderr, "_IIOP_ReadHeader:  MessageError advisory received.\n"));
      }
      break;

    case GIOP_PacketType_CloseConnection:
      {
	serialNumber = 0;
	_IIOP_CheckBoundaries (call, GIOP_PacketType_CloseConnection, err);
	DEBUG(IIOP_DEBUG,
	      (stderr, "_IIOP_ReadHeader:  CloseConnection advisory received.\n"));
      }
      break;
    };

 marshalError:

#ifdef ENABLE_DEBUGGING
  if ((_ilu_DebugLevel & IIOP_DEBUG) != 0)
    {
      char *mtype = _IIOP_MessageTypeName(ptype);
      ilu_DebugPrintf ("%s %s #%lu, %u bytes, byte order %s.\n",
		       "_IIOP_ReadHeader:  ",
		       (mtype == NIL) ? "(unrecognized message)" : mtype,
		       serialNumber, iiop_size(call),
		       (p->byteorder == BigEndian) ? "big-endian" : "little-endian");
    }
#endif
	 
  if (ILU_ERRNOK(*err))
    return ilu_rhrc_error;
  else if ((ptype == GIOP_PacketType_Request) OR (ptype == GIOP_PacketType_Reply))
    {
      *sn = serialNumber;
      return ilu_rhrc_ok;
    }
  else
    return ilu_rhrc_handled;
}

static ilu_refany 
  _IIOP_DelayInterp(ilu_Call call,
		    ILU_ERRS((IoErrs)) * err)
{
  ilu_Transport   t = iiop_transport(call);
  return _ilu_BufferInputMessage(t, err);
}

static void
  _IIOP_ResumeInterp(ilu_Call call, ilu_refany x)
{
  ilu_Transport   d = (ilu_Transport) x;
  call->ca_prTrans = d;
  return;
}

static ilu_boolean 
  _IIOP_DiscardMessage(ilu_Call call, ILU_ERRS((internal)) * err)
{
  ILU_CLER(*err);
  return (ilu_TRUE);
}

static ilu_boolean
  _IIOP_AbandonDelayedInterp(ilu_refany x, ILU_ERRS((internal)) * err)
{
  ilu_Transport   d = (ilu_Transport) x;
  ilu_boolean     ans;
  ilu_integer     cdfd;
  ans = transport_close(d, &cdfd, err);
  _ilu_Assert(cdfd == 0, "IIOP AbandonDelayedInterp");
  return ans;
}

static void
  _IIOP_RequestRead (ilu_Call call, ILU_ERRS((IoErrs)) *err)
{
  ilu_Transport bs = iiop_transport(call);
  iiop_vop(call) = 0;
  _IIOP_EndMessage (bs, TRUE, err);
}

static void
  _IIOP_ReplyRead (ilu_Call call, ILU_ERRS((IoErrs)) *err)
{
  ilu_Transport bs = iiop_transport(call);
  iiop_vop(call) = 0;
  _IIOP_EndMessage (bs, TRUE, err);
}

static ilu_boolean
  _IIOP_InterpretRequest (ilu_Call call, ILU_ERRS((IoErrs)) *err)
{
  ilu_boolean responseExpected;
  ilu_cardinal method_name_len = 0;
  ilu_string method_name;
  ilu_Class putative_class;

  if (iiop_packetType(call) == GIOP_PacketType_Request)
    {
      _IIOP_InputBoolean (call, &responseExpected, err);
      if (ILU_ERRNOK(*err))
	return FALSE;
      else
	{
	  if (!(_IIOP_InputBytes(call, &iiop_objKey(call), &iiop_objKeyLen(call), 0, err), ILU_ERROK(*err)))
	    {
	      call->ca_pe = ilu_ProtocolException_GarbageArguments;
	      return ilu_FALSE;
	    }
	  if (!(_IIOP_InputString(call, &method_name, &method_name_len, 0, err), ILU_ERROK(*err)))
	    {
	      call->ca_pe = ilu_ProtocolException_GarbageArguments;
	      return ilu_FALSE;
	    }
	}
      _ilu_AcquireMutex(ilu_prmu);
      putative_class = FindClassFromObjectKey (iiop_objKey(call), iiop_objKeyLen(call));
      _ilu_ReleaseMutex(ilu_prmu);
      if (putative_class == NIL)
	{
	  DEBUG(IIOP_DEBUG,
		(stderr, "%s  (call %lu) %s %p (%ul)\n",
		 "_IIOP_InterpretRequest:", call->ca_SN,
		 "Can't find ilu_Class for object_key", iiop_objKey(call), iiop_objKeyLen(call)));
	  call->ca_pe = ilu_ProtocolException_NoSuchClassAtServer;
	  return ilu_FALSE;      
	}

      FindClassAndMethodFromIDLMethodName (call, putative_class, method_name);

      if (call->ca_method == NIL)
	{
	  DEBUG(IIOP_DEBUG,
		(stderr,
		 "%s  (call %lu) %s \"%s\" with methodID = <%s>.\n",
		 "_IIOP_InterpretRequest:", call->ca_SN,
		 "Can't find method on class", class_name(putative_class),
		 method_name));
	  ilu_free(method_name);
	  call->ca_pe = ilu_ProtocolException_NoSuchMethodOnClass;
	  return ILU_ERR_CONS1(bad_operation, err, minor, ilu_bom_noSuchOperationOnType, ilu_FALSE);
	}
      else
	{
	  DEBUG(IIOP_DEBUG,
		(stderr, "%sclass %s, method %s is %p (%s).\n",
		 "_IIOP_InterpretRequest:  ", class_name(putative_class),
		 method_name, call->ca_method, call->ca_method->me_name));
	}
      ilu_free(method_name);
      /* Now read and remember the info about the Principal */
      _IIOP_InputBytes (call, &iiop_principal(call), &iiop_principalLen(call), 0xFFFF, err);
      if (ILU_ERRNOK(*err))
	{
	  DEBUG((IIOP_DEBUG | SECURITY_DEBUG),
		(stderr, "%s:  error <%s> fetching principal of call %lu (%s)\n",
		 ILU_ERR_NAME(*err), (long unsigned) call_serial_number(call),
		 method_name(call_method(call))));
	  call->ca_pe = ilu_ProtocolException_RequestRejected;
	  return ilu_FALSE;
	}
      else
	{
	  DEBUG((IIOP_DEBUG | SECURITY_DEBUG),
		(stderr, "%s:  Principal of call %lu (%s) is %lu bytes at %p\n",
		 "_IIOP_InterpretRequest",  (long unsigned) call_serial_number(call),
		 method_name(call_method(call)), (long unsigned) iiop_principalLen(call),
		 iiop_principal(call)));
	  FREETOKEN(iiop_principal(call));
	  iiop_principal(call) = NIL;
	  iiop_principalLen(call) = 0;
	}
      DEBUG(IIOP_DEBUG,
	    (stderr, "_IIOP_InterpretRequest:  returning TRUE\n"));
      return (ilu_TRUE);
    }
  else
    return (ILU_ERR_CONS1(marshal, err, minor, ilu_mm_msgTypeUnknown, ilu_FALSE));
}

/*L1, Main unconstrained*/
/*L2 >= {conn's iomu}*/

static ilu_ProtocolException _IIOP_InterpretReply (ilu_Call call,
						   ilu_cardinal *exception_code,
						   ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal replyStatus;

  _IIOP_InputCardinal (call, &replyStatus, err);

  DEBUG(IIOP_DEBUG,
	(stderr, "_IIOP_InterpretReply:  replyStatus on reply %lu is %lu\n",
	 call_serial_number(call), replyStatus));

  if (ILU_ERRNOK(*err))
    return ilu_ProtocolException_Not;

  if (replyStatus == GIOP_ReplyStatusType_NO_EXCEPTION)
    {
      *exception_code = 0;
      return (ilu_ProtocolException_Success);
    }
  else if (replyStatus == GIOP_ReplyStatusType_USER_EXCEPTION)
    {
      ilu_string exception_name = NIL;
      ilu_cardinal exception_name_len = 0;
      if (_IIOP_InputString (call, &exception_name, &exception_name_len, 0, err), ILU_ERRNOK(*err))
	return ilu_ProtocolException_Not;      
      *exception_code = FigureExceptionIndexFromIDLName (call_intro_type(call), call_method(call), exception_name);
      ilu_free(exception_name);
      return (ilu_ProtocolException_Success);
    }
  else if (replyStatus == GIOP_ReplyStatusType_SYSTEM_EXCEPTION)
    {
      ilu_string exception_name = NIL;
      ilu_cardinal exception_name_len = 0;
      ilu_cardinal i;
      ilu_cardinal minor;
      ilu_cardinal completed;

      if (_IIOP_InputString (call, &exception_name, &exception_name_len, 0, err), ILU_ERRNOK(*err))
	return ilu_ProtocolException_Not;
      i = FigureExceptionIndexFromIDLName (NIL, NIL, exception_name);
      ilu_free(exception_name);
      if (_IIOP_InputCardinal (call, &minor, err), ILU_ERRNOK(*err))
	return ilu_ProtocolException_Not;
      if (_IIOP_InputCardinal (call, &completed, err), ILU_ERRNOK(*err))
	return ilu_ProtocolException_Not;
      *exception_code = minor;
      DEBUG(IIOP_DEBUG,
	    (stderr, "_IIOP_InterpretReply:  system exception <%s> received, minor code %lu,"
	     " completed %s\n", ilu_PEName(i), (unsigned long) minor,
	     (completed == 0) ? "YES" : ((completed == 1) ? "NO" : ((completed == 2) ? "MAYBE" : "INVALID"))));	     
      return (i);
    }
  else if (replyStatus == GIOP_ReplyStatusType_LOCATION_FORWARD)
    {
      ilu_DebugPrintf ("_IIOP_InterpretReply:  IIOP LOCATION_FORWARD reply received!\n");
      ilu_DebugPrintf ("_IIOP_InterpretReply:  ILU does not yet handle LOCATION_FORWARD replies.\n");
      return ilu_ProtocolException_RequestRejected;
    }
  else
    {
      DEBUG(IIOP_DEBUG,
	    (stderr, "_IIOP_InterpretReply:  unexpected reply status %lu.\n", (unsigned long) replyStatus));
      return ILU_ERR_CONS1(marshal, err, minor, ilu_mm_msgTypeUnknown, ilu_ProtocolException_Not);
    }
}

/*L1_sup < prmu*/

static ilu_boolean
  _IIOP_StartRequest (ilu_Call call,
		      ilu_cardinal argSize,
		      ILU_ERRS((IoErrs)) *err)
{
  static ilu_byte magic[] = "GIOP";
  ilu_cardinal packetSize;
  ilu_Class pclass = call_intro_type(call);
  ilu_Method method = call_method(call);
  ilu_Transport bs = iiop_transport(call);

  DEBUG(IIOP_DEBUG,
	(stderr,
	 "%s %p (sn %lu), argSize %lu, class %s (%s), meth %s (%lu)\n",
	 "_IIOP_BeginRequest:  call", call,
	 call_serial_number(call), argSize, class_name(pclass),
	 class_unique_id(pclass), method_name(method),
	 method_id(method)));

  Initialize();

  packetSize = argSize + 21;

  if (bs->tr_class->tc_boundaried)
    {
      if (transport_begin_message(bs, FALSE, err)
	  != ilu_rhrc_ok)
	return ilu_FALSE;
    }

  iiop_vop(call) = 0;
  iiop_size(call) = packetSize;

  if ((_IIOP_OutputOpaque (call, magic, 4, err), ILU_ERRNOK(*err)) ||
      (_IIOP_OutputByte (call, iiop_major_version(call) & 0xFF, err), ILU_ERRNOK(*err)) ||
      (_IIOP_OutputByte (call, iiop_minor_version(call) & 0xFF, err), ILU_ERRNOK(*err)) ||
      (_IIOP_OutputByte (call,
			 (NATIVE_BYTE_ORDER == LittleEndian),
			 err), ILU_ERRNOK(*err)) ||
      (_IIOP_OutputByte (call, GIOP_PacketType_Request, err), ILU_ERRNOK(*err)) ||
      (_IIOP_OutputCardinal (call, packetSize - 12, err), ILU_ERRNOK(*err)) ||
      /* zero-length context */
      (_IIOP_OutputCardinal (call, 0, err), ILU_ERRNOK(*err)) ||
      /* serial number */
      (_IIOP_OutputCardinal (call, call_serial_number(call), err), ILU_ERRNOK(*err)) ||
      /* response expected? */
      (_IIOP_OutputByte (call, (ilu_byte) (! (method_asynchronous(method))), err), ILU_ERRNOK(*err)))
    return (ilu_FALSE);

  /* The object ID and method ID will be output during the call to 
     marshall the discriminant */

  DEBUG(IIOP_DEBUG,
	(stderr, "_IIOP_StartRequest:  request %lu begun (argsize %lu).\n",
	 call_serial_number(call), argSize));
  return (ilu_TRUE);
}

static ilu_boolean
  _IIOP_FinishRequest (ilu_Call call, ilu_Message *msg, ILU_ERRS((IoErrs)) *err)
{
  ilu_Transport t = iiop_transport(call);

  if (((ilu_cardinal) iiop_vop(call)) != iiop_size(call))
    {
      DEBUG(IIOP_DEBUG,
	    (stderr, "IIOP bad message size %lu in \"%s\" request %ld.  %lu bytes actually output.\n",
	     (long unsigned) iiop_size(call), method_name(call_method(call)), (long) call_serial_number(call), (long) iiop_vop(call)));
    }
  _IIOP_EndMessage (t, FALSE, err);
  iiop_vop(call) = 0;
  return ILU_ERROK(*err);
}

static ilu_boolean _IIOP_FinishReply (ilu_Call call, ILU_ERRS((IoErrs)) *err)
{
  ilu_Transport t = iiop_transport(call);

  if (((ilu_cardinal) iiop_vop(call)) != iiop_size(call))
    {
      DEBUG(IIOP_DEBUG,
	    (stderr, "IIOP bad message size %lu in \"%s\" reply %ld.  %lu bytes actually output.\n",
	     (long unsigned) iiop_size(call), method_name(call_method(call)), (long) call_serial_number(call), (long unsigned) iiop_vop(call)));
    }
  _IIOP_EndMessage (t, FALSE, err);
  return ILU_ERROK(*err);
}

static ilu_boolean _IIOP_FinishException (ilu_Call call, ILU_ERRS((IoErrs)) *err)
{
  ilu_Transport t = iiop_transport(call);

  if (((ilu_cardinal) iiop_vop(call)) != iiop_size(call))
    {
      DEBUG(IIOP_DEBUG,
	    (stderr, "IIOP bad message size %lu in \"%s\" reply %ld.  %lu bytes actually output.\n",
	     (long unsigned) iiop_size(call), method_name(call_method(call)), (long) call_serial_number(call), (long unsigned) iiop_vop(call)));
    }
  _IIOP_EndMessage (t, FALSE, err);
  return ILU_ERROK(*err);
}

/*L1, Main unconstrained*/
/*L2 >= {call's conn's iomu}*/

static ilu_cardinal
  _IIOP_BeginSizingReply (ilu_Call call,
			  ilu_boolean exceptions_possible,
			  ILU_ERRS((IoErrs)) *err)
{
  iiop_byte_order(call) = NATIVE_BYTE_ORDER;
  ILU_CLER(*err);
  return 24;
}

static ilu_boolean
  _IIOP_BeginReply (ilu_Call call,
		    ilu_boolean exceptions,
		    ilu_cardinal argSize,
		    ILU_ERRS((IoErrs)) *err)
{
  static ilu_byte magic[] = "GIOP";
  ilu_Transport bs = iiop_transport(call);

  DEBUG(IIOP_DEBUG,
	(stderr, "%s %lu, argSize %lu, exceptions %s.\n",
	 "_IIOP_BeginReply:  SN", call->ca_SN, argSize,
	 exceptions ? "TRUE" : "FALSE"));

  Initialize();

  iiop_vop(call) = 0;
  iiop_size(call) = argSize;

  if (bs->tr_class->tc_boundaried)
    {
      if (transport_begin_message(bs, FALSE, err)
	  != ilu_rhrc_ok)
	return ilu_FALSE;
    }

  if ((_IIOP_OutputOpaque (call, magic, 4, err), ILU_ERRNOK(*err)) ||
      (_IIOP_OutputByte (call, iiop_major_version(call) & 0xFF, err), ILU_ERRNOK(*err)) ||
      (_IIOP_OutputByte (call, iiop_minor_version(call) & 0xFF, err), ILU_ERRNOK(*err)) ||
      (_IIOP_OutputByte (call,
			 (NATIVE_BYTE_ORDER == LittleEndian),
			 err), ILU_ERRNOK(*err)) ||	/* always use big-endian */
      (_IIOP_OutputByte (call, GIOP_PacketType_Reply, err), ILU_ERRNOK(*err)) ||
      (_IIOP_OutputCardinal (call, argSize - 12, err), ILU_ERRNOK(*err)) ||
      /* zero-length context */
      (_IIOP_OutputCardinal (call, 0, err), ILU_ERRNOK(*err)) ||
      /* serial number */
      (_IIOP_OutputCardinal (call, call->ca_SN, err), ILU_ERRNOK(*err)) ||
      /* reply status */
      (_IIOP_OutputCardinal (call, GIOP_ReplyStatusType_NO_EXCEPTION, err), ILU_ERRNOK(*err)))
    {
      DEBUG(IIOP_DEBUG,
	    (stderr, "_IIOP_BeginReply:  err <%s> starting reply %lu (size %lu).\n",
	     ILU_ERR_NAME(*err), call_serial_number(call), argSize));
      return (ilu_FALSE);
    }
  else
    {
      DEBUG(IIOP_DEBUG,
	    (stderr, "_IIOP_BeginReply:  started reply %lu (size %lu).\n",
	     call_serial_number(call), argSize));
      return (ilu_TRUE);
    }
}

static ilu_cardinal
  _IIOP_BeginSizingException (ilu_Call call,
			      ilu_cardinal eindex,
			      ilu_ProtocolException sysExnIdx,
			      ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal packetsize = 0;
  ilu_string ename;

  Initialize();

  ILU_CLER(*err);
  
  iiop_byte_order(call) = NATIVE_BYTE_ORDER;
  iiop_vop(call) = 0;

  ename = FigureNameOfException (call_intro_type(call), call_method(call), eindex, sysExnIdx);
  DEBUG(IIOP_DEBUG,
	(stderr, "_IIOP_BeginSizingException:  exception string is <%s>\n", ename));
  iiop_objKey(call) = (ilu_bytes) ename;
  iiop_objKeyLen(call) = strlen(ename);
  
  packetsize += (4	/* magic "GIOP" */
		 + 2	/* version */
		 + 1	/* byte order */
		 + 1	/* message type -- exception */
		 + 4	/* message size */
		 + 4	/* empty context */
		 + 4	/* serial number */
		 + 4	/* exception kind -- user or system */
		 );

  iiop_vop(call) = (ilu_bytes) packetsize;
  packetsize += _IIOP_SizeOfBytes(call, iiop_objKey(call), iiop_objKeyLen(call) + 1, 0xFFFF, err);
  if (eindex == 0)	/* system exception */
    {
      packetsize += _IIOP_SizeOfCardinal (call, 1, err);	/* for minor code */
      packetsize += _IIOP_SizeOfCardinal (call, 1, err);	/* for completion status */
    }
  return packetsize;
}

static ilu_boolean
  _IIOP_BeginException (ilu_Call call,
			ilu_cardinal evalue,
			ilu_ProtocolException sysExnIndex,
			ilu_cardinal argSize,
			ILU_ERRS((IoErrs)) *err)
{
  static ilu_byte magic[] = "GIOP";
  ilu_string ename;
  ilu_Transport bs = iiop_transport(call);

  Initialize();

  if (evalue == 0 && ((int) sysExnIndex) > 0 && iiop_objKey(call) == NIL)
    argSize = _IIOP_BeginSizingException (call, evalue, sysExnIndex, err);

  ename = (char *) iiop_objKey(call);
  
  iiop_vop(call) = 0;
  iiop_size(call) = argSize;
  iiop_objKey(call) = NIL;
  iiop_objKeyLen(call) = 0;

  if (bs->tr_class->tc_boundaried)
    {
      if (transport_begin_message(bs, FALSE, err)
	  != ilu_rhrc_ok)
	return ilu_FALSE;
    }

  if ((_IIOP_OutputOpaque (call, magic, 4, err), ILU_ERRNOK(*err)) ||
      (_IIOP_OutputByte (call, iiop_major_version(call), err), ILU_ERRNOK(*err)) ||
      (_IIOP_OutputByte (call, iiop_minor_version(call), err), ILU_ERRNOK(*err)) ||
      (_IIOP_OutputByte (call,
			 (NATIVE_BYTE_ORDER == LittleEndian),
			 err), ILU_ERRNOK(*err)) ||	/* always use big-endian */
      (_IIOP_OutputByte (call, GIOP_PacketType_Reply, err), ILU_ERRNOK(*err)) ||
      (_IIOP_OutputCardinal (call, iiop_size(call) - 12, err), ILU_ERRNOK(*err)) ||
      /* zero-length context */
      (_IIOP_OutputCardinal (call, 0, err), ILU_ERRNOK(*err)) ||
      /* serial number */
      (_IIOP_OutputCardinal (call, call->ca_SN, err), ILU_ERRNOK(*err)) ||
      ((evalue == 0) ?
       ((_IIOP_OutputCardinal (call, GIOP_ReplyStatusType_SYSTEM_EXCEPTION, err), ILU_ERRNOK(*err)) ||
	(_IIOP_OutputString (call, ename, strlen(ename), 0, err), ILU_ERRNOK(*err)) ||
	(_IIOP_OutputCardinal (call, sysExnIndex, err), ILU_ERRNOK(*err)) ||
	(_IIOP_OutputCardinal (call, ((ilu_cardinal)2) /* CORBA::COMPLETED_MAYBE */, err), ILU_ERRNOK(*err))) :
       ((_IIOP_OutputCardinal (call, GIOP_ReplyStatusType_USER_EXCEPTION, err), ILU_ERRNOK(*err)) ||
	(_IIOP_OutputString (call, ename, strlen(ename), 0, err), ILU_ERRNOK(*err)))))
    {
      DEBUG(IIOP_DEBUG,
	    (stderr, "_IIOP_BeginException:  err <%s> starting reply %lu (size %lu).\n",
	     ILU_ERR_NAME(*err), call_serial_number(call), iiop_size(call)));
      return (ilu_FALSE);
    }
  else
    {
      DEBUG(IIOP_DEBUG,
	    (stderr, "_IIOP_BeginException:  started reply %lu (size %lu).\n",
	     call_serial_number(call), iiop_size(call)));
      return (ilu_TRUE);
    }
}

/*L2, Main unconstrained*/
/*L1 >= {prmu}*/

static ilu_Protocol _IIOP_NewIIOP (void)
{
  ilu_Protocol new = (ilu_Protocol) ilu_must_malloc(sizeof(struct _ilu_Protocol_s));

  new->pr_concurrent_requests = ilu_TRUE;
  new->pr_sizing_required = ilu_TRUE;

  new->pr_init_call = _IIOP_InitCall;
  new->pr_start_request = _IIOP_StartRequest;
  new->pr_finish_request = _IIOP_FinishRequest;
  new->pr_begin_sizing_reply = _IIOP_BeginSizingReply;
  new->pr_begin_reply = _IIOP_BeginReply;
  new->pr_finish_reply = _IIOP_FinishReply;
  new->pr_begin_sizing_exn = _IIOP_BeginSizingException;
  new->pr_begin_exception = _IIOP_BeginException;
  new->pr_finish_exception = _IIOP_FinishException;
  new->pr_finish_call = _IIOP_FinishCall;
  new->pr_prefinish_call = NULLFN;

  new->pr_read_header = _IIOP_ReadHeader;
  new->pr_delay_interp = _IIOP_DelayInterp;
  new->pr_resume_interp = _IIOP_ResumeInterp;
  new->pr_abandon_delayed_interp = _IIOP_AbandonDelayedInterp;
  new->pr_discard_input = _IIOP_DiscardMessage;
  new->pr_discard_output = _IIOP_DiscardMessage;
  
  new->pr_interpret_request = _IIOP_InterpretRequest;
  new->pr_request_read = _IIOP_RequestRead;
  new->pr_interpret_reply = _IIOP_InterpretReply;
  new->pr_reply_read = _IIOP_ReplyRead;

  new->pr_output_integer = _IIOP_OutputInteger;
  new->pr_input_integer = _IIOP_InputInteger;
  new->pr_size_of_integer = _IIOP_SizeOfInteger;

  new->pr_output_shortinteger = _IIOP_OutputShortInteger;
  new->pr_input_shortinteger = _IIOP_InputShortInteger;
  new->pr_size_of_shortinteger = _IIOP_SizeOfShortInteger;

  new->pr_output_longinteger = _IIOP_OutputLongInteger;
  new->pr_input_longinteger = _IIOP_InputLongInteger;
  new->pr_size_of_longinteger = _IIOP_SizeOfLongInteger;

  new->pr_output_cardinal = _IIOP_OutputCardinal;
  new->pr_input_cardinal = _IIOP_InputCardinal;
  new->pr_size_of_cardinal = _IIOP_SizeOfCardinal;

  new->pr_output_shortcardinal = _IIOP_OutputShortCardinal;
  new->pr_input_shortcardinal = _IIOP_InputShortCardinal;
  new->pr_size_of_shortcardinal = _IIOP_SizeOfShortCardinal;

  new->pr_output_longcardinal = _IIOP_OutputLongCardinal;
  new->pr_input_longcardinal = _IIOP_InputLongCardinal;
  new->pr_size_of_longcardinal = _IIOP_SizeOfLongCardinal;

  new->pr_output_real = _IIOP_OutputReal;
  new->pr_input_real = _IIOP_InputReal;
  new->pr_size_of_real = _IIOP_SizeOfReal;

  new->pr_output_shortreal = _IIOP_OutputShortReal;
  new->pr_input_shortreal = _IIOP_InputShortReal;
  new->pr_size_of_shortreal = _IIOP_SizeOfShortReal;

  new->pr_output_longreal = _IIOP_OutputLongReal;
  new->pr_input_longreal = _IIOP_InputLongReal;
  new->pr_size_of_longreal = _IIOP_SizeOfLongReal;

  new->pr_output_optional = (void (*)(ilu_Call, ilu_boolean, ilu_Error *)) _IIOP_OutputCardinal;
  new->pr_input_optional = (void (*)(ilu_Call, ilu_boolean *, ilu_Error *)) _IIOP_InputCardinal;
  new->pr_size_of_optional = (ilu_cardinal (*)(ilu_Call, ilu_boolean, ilu_Error *)) _IIOP_SizeOfCardinal;

  new->pr_output_enum_code = _IIOP_OutputEnumeration;
  new->pr_input_enum_code = _IIOP_InputEnumeration;
  new->pr_size_of_enum_code = _IIOP_SizeOfEnumeration;

  new->pr_output_byte = _IIOP_OutputByte;
  new->pr_input_byte = _IIOP_InputByte;
  new->pr_size_of_byte = _IIOP_SizeOfByte;

  new->pr_output_character = _IIOP_OutputShortCardinal;
  new->pr_input_character = _IIOP_InputShortCardinal;
  new->pr_size_of_character = _IIOP_SizeOfShortCardinal;

  new->pr_output_boolean = _IIOP_OutputBoolean;
  new->pr_input_boolean = _IIOP_InputBoolean;
  new->pr_size_of_boolean = _IIOP_SizeOfBoolean;

  new->pr_output_shortchar = _IIOP_OutputShortChar;
  new->pr_input_shortchar = _IIOP_InputShortChar;
  new->pr_size_of_shortchar = _IIOP_SizeOfShortChar;

  new->pr_output_string = _IIOP_OutputString;
  new->pr_input_string = _IIOP_InputString;
  new->pr_size_of_string = _IIOP_SizeOfString;

  new->pr_output_wstring = _ilu_OutputWString;
  new->pr_input_wstring = _ilu_InputWString;
  new->pr_size_of_wstring = _ilu_SizeOfWString;

  new->pr_output_bytes = _IIOP_OutputBytes;
  new->pr_input_bytes = _IIOP_InputBytes;
  new->pr_size_of_bytes = _IIOP_SizeOfBytes;

  new->pr_output_opaque = _IIOP_OutputOpaque;
  new->pr_input_opaque = _IIOP_InputOpaque;
  new->pr_size_of_opaque = _IIOP_SizeOfOpaque;

  new->pr_output_object_id = _IIOP_OutputObjectID;
  new->pr_input_object_id = _IIOP_InputObjectID;
  new->pr_size_of_object_id = _IIOP_SizeOfObjectID;

  new->pr_output_stringvec = (void (*)(ilu_Call, ilu_string, ilu_cardinal, ilu_Error *)) _IIOP_OutputOpaque;
  new->pr_input_stringvec = (void (*)(ilu_Call, ilu_string *, ilu_cardinal, ilu_Error *)) _IIOP_InputOpaque;
  new->pr_size_of_stringvec = (ilu_cardinal (*)(ilu_Call, ilu_string, ilu_cardinal, ilu_Error *)) _IIOP_SizeOfOpaque;

  new->pr_output_wstringvec = _ilu_OutputWStringVec;
  new->pr_input_wstringvec = _ilu_InputWStringVec;
  new->pr_size_of_wstringvec = _ilu_SizeOfWStringVec;

  new->pr_output_sequence = _IIOP_OutputSequence;
  new->pr_output_sequence_mark = _IIOP_OutputSequenceMark;
  new->pr_input_sequence = _IIOP_InputSequence;
  new->pr_input_sequence_mark = _IIOP_InputSequenceMark;
  new->pr_end_sequence = _IIOP_EndSequence;
  new->pr_size_of_sequence = _IIOP_SizeOfSequence;

  new->pr_output_record = _IIOP_OutputRecord;
  new->pr_input_record = _IIOP_InputRecord;
  new->pr_end_record = _IIOP_EndRecord;
  new->pr_size_of_record = _IIOP_SizeOfRecord;

  new->pr_output_array = _IIOP_OutputArray;
  new->pr_input_array = _IIOP_InputArray;
  new->pr_end_array = _IIOP_EndArray;
  new->pr_size_of_array = _IIOP_SizeOfArray;

  new->pr_output_union = _IIOP_OutputUnion;
  new->pr_input_union = _IIOP_InputUnion;
  new->pr_end_union = _IIOP_EndUnion;
  new->pr_size_of_union = _IIOP_SizeOfUnion;

  new->pr_form_handle = _IIOP_FormProtocolHandle;

  new->pr_create_data_block = _IIOP_CreateDataBlock_1_0;
  new->pr_free_data_block = (void (*)(void *)) _IIOP_FreeDataBlock;

  return (new);
}

/*L1_sup < prmu*/

ilu_Protocol _ilu_IIOP_Protocol(void)
{
  /*L1 >= {prmu}*/
  static ilu_Protocol StandardIIOP = NULL;
  _ilu_AcquireMutex(ilu_prmu);
  if (StandardIIOP == NULL)
    StandardIIOP = _IIOP_NewIIOP();
  _ilu_ReleaseMutex(ilu_prmu);
  return (StandardIIOP);
}

