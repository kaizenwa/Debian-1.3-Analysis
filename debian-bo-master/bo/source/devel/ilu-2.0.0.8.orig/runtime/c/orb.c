/*
** Copyright (c) 1993-1995 Xerox Corporation.  All Rights Reserved.
**
** Unlimited use, reproduction, and distribution of this software is
** permitted.  Any copy of this software must include both the above
** copyright notice of Xerox Corporation and this paragraph.  Any
** distribution of this software must comply with all applicable United
** States export control laws.  This software is made available AS IS,
** and XEROX CORPORATION DISCLAIMS ALL WARRANTIES, EXPRESS OR IMPLIED,
** INCLUDING WITHOUT LIMITATION THE IMPLIED WARRANTIES OF MERCHANTABILITY
** AND FITNESS FOR A PARTICULAR PURPOSE, AND NOTWITHSTANDING ANY OTHER
** PROVISION CONTAINED HEREIN, ANY LIABILITY FOR DAMAGES RESULTING FROM
** THE SOFTWARE OR ITS USE IS EXPRESSLY DISCLAIMED, WHETHER ARISING IN
** CONTRACT, TORT (INCLUDING NEGLIGENCE) OR STRICT LIABILITY, EVEN IF
** XEROX CORPORATION IS ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
*/
/* $Id: orb.c,v 1.32 1996/03/15 23:03:05 mdavidso Exp $ */
/* Last edited by Mike Spreitzer December 15, 1995 10:05 am PST */

#include "iluchdrs.h"

static ILU_C_Object	_ILU_C_ORB[1];
CORBA_Object		ILU_C_ORB = _ILU_C_ORB;

void _ILU_C_SetProtocolError (CORBA_Environment *status, ilu_ProtocolException perror)
{
  status->_major = CORBA_SYSTEM_EXCEPTION;
  status->freeRoutine = ((void (*)(void *)) 0);
  status->ptr = (void *) ilu_must_malloc(sizeof(CORBA_ex_body));
  ((CORBA_ex_body *)(status->ptr))->minor = 0;
  ((CORBA_ex_body *)(status->ptr))->completed = CORBA_COMPLETED_NO;
  switch (perror)
    {
    case ilu_ProtocolException_NoSuchClassAtServer:
      status->returnCode = ex_CORBA_BAD_TYPECODE;
      break;

    case ilu_ProtocolException_ClassVersionMismatch:
      status->returnCode = ex_CORBA_BAD_TYPECODE;
      break;

    case ilu_ProtocolException_NoSuchMethodOnClass:
      status->returnCode = ex_CORBA_BAD_OPERATION;
      break;

    case ilu_ProtocolException_GarbageArguments:
      status->returnCode = ex_CORBA_BAD_PARAM;
      break;

    case ilu_ProtocolException_Unknown:
      status->returnCode = ex_CORBA_UNKNOWN;
      ((CORBA_ex_body *)(status->ptr))->completed = CORBA_COMPLETED_MAYBE;
      break;

    case ilu_ProtocolException_LostConnection:
      status->returnCode = ex_CORBA_COMM_FAILURE;
      ((CORBA_ex_body *)(status->ptr))->completed = CORBA_COMPLETED_MAYBE;
      break;

    case ilu_ProtocolException_RequestRejected:
      status->returnCode = ex_CORBA_BAD_OPERATION;
      break;

    case ilu_ProtocolException_RequestTimeout:
      status->returnCode = ex_CORBA_NO_RESPONSE;
      ((CORBA_ex_body *)(status->ptr))->completed = CORBA_COMPLETED_MAYBE;
      break;

    default:
      break;

    }
}

ILU_C_ExceptionCode	ex_CORBA_UNKNOWN = "CORBA: an unknown exception was encountered";
ILU_C_ExceptionCode	ex_CORBA_BAD_PARAM = "CORBA: a bad parameter was passed";
ILU_C_ExceptionCode	ex_CORBA_NO_MEMORY = "CORBA: dynamic memory allocation failure";
ILU_C_ExceptionCode	ex_CORBA_IMP_LIMIT = "CORBA: some implementation limit exceeded";
ILU_C_ExceptionCode	ex_CORBA_COMM_FAILURE = "CORBA: communication failure";
ILU_C_ExceptionCode	ex_CORBA_INV_OBJREF = "CORBA: invalid object reference";
ILU_C_ExceptionCode	ex_CORBA_NO_PERMISSION = "CORBA: no permission for attempted operation";
ILU_C_ExceptionCode	ex_CORBA_INTERNAL = "CORBA: ORB internal error";
ILU_C_ExceptionCode	ex_CORBA_MARSHAL = "CORBA: error marshalling parameter or result";
ILU_C_ExceptionCode	ex_CORBA_INITIALIZE = "CORBA: ORB initialization failure";
ILU_C_ExceptionCode	ex_CORBA_NO_IMPLEMENT = "CORBA: operation implementation unavailable";
ILU_C_ExceptionCode	ex_CORBA_BAD_TYPECODE = "CORBA: bad typecode";
ILU_C_ExceptionCode	ex_CORBA_BAD_OPERATION = "CORBA: invalid operation";
ILU_C_ExceptionCode	ex_CORBA_NO_RESOURCES = "CORBA: insufficient resources for operation";
ILU_C_ExceptionCode	ex_CORBA_NO_RESPONSE = "CORBA: response to request not yet available";
ILU_C_ExceptionCode	ex_CORBA_PERSIST_STORE = "CORBA: persistent storage failure";
ILU_C_ExceptionCode	ex_CORBA_BAD_INV_ORDER = "CORBA: routine invocations out of order";
ILU_C_ExceptionCode	ex_CORBA_TRANSIENT = "CORBA: transient failure -- reissue request";
ILU_C_ExceptionCode	ex_CORBA_FREE_MEM = "CORBA: cannot free memory";
ILU_C_ExceptionCode	ex_CORBA_INV_IDENT = "CORBA: invalid identifier syntax";
ILU_C_ExceptionCode	ex_CORBA_INV_FLAG = "CORBA: invalid modifier or option was specified";
ILU_C_ExceptionCode	ex_CORBA_INTF_REPOS = "CORBA: error accessing interface repository";
ILU_C_ExceptionCode	ex_CORBA_BAD_CONTEXT = "CORBA: error processing context object";
ILU_C_ExceptionCode	ex_CORBA_OBJ_ADAPTER = "CORBA: error detected by object adapter";
ILU_C_ExceptionCode	ex_CORBA_DATA_CONVERSION = "CORBA: data conversion error";

#define SYS_EX(type,completion)	{s->_major=CORBA_SYSTEM_EXCEPTION;s->returnCode=(type);s->ptr=(void *)ilu_must_malloc(sizeof(CORBA_ex_body));s->freeRoutine=((void (*)(void *)) 0);((CORBA_ex_body *)(s->ptr))->minor=0;((CORBA_ex_body *)(s->ptr))->completed=(completion);}

/*
** corba defined exception handling functions
*/

char *CORBA_exception_id (CORBA_Environment *ev)
{
  return(( char * ) ev->returnCode );
}

void *CORBA_exception_value (CORBA_Environment *ev)
{
  return(( void * ) ev->ptr );
}

void CORBA_exception_free (CORBA_Environment *ev)
{
  if (ev->freeRoutine != ((void (*)(void *)) 0))
    (*ev->freeRoutine)(ev->ptr);
  ilu_free(ev->ptr);
  ev->ptr = ILU_NIL;
  ev->freeRoutine = ((void (*)(void *)) 0);
  return;
}

char *CORBA_string_alloc(CORBA_unsigned_long len)
{
  return ilu_malloc(len + 1);
}

void CORBA_free (void *ptr)
{
  if ( ptr )
    ilu_free( ptr );
  return;
}

CORBA_ORBStatus CORBA_ORB_get_default_context (CORBA_Object h, CORBA_Context *ctx, CORBA_Environment *s)
{
  if (h != ILU_C_ORB)
    SYS_EX(ex_CORBA_INV_OBJREF,CORBA_COMPLETED_NO)
  else
    SYS_EX(ex_CORBA_NO_IMPLEMENT,CORBA_COMPLETED_NO)
}
    
char *CORBA_ORB_object_to_string (CORBA_Object h, CORBA_Object o, CORBA_Environment *s)
{
  char *sbh;

#ifdef IIOP_PROTOCOL
  if (h != ILU_C_ORB || ((sbh = ILU_C_IOROfObject(o)) == ILU_NIL))
#else
  if (h != ILU_C_ORB || ((sbh = ILU_C_SBHOfObject(o)) == ILU_NIL))
#endif
    {
      SYS_EX(ex_CORBA_INV_OBJREF,CORBA_COMPLETED_NO)
      return (ILU_NIL);
    }
  else
    {
      s->_major = CORBA_NO_EXCEPTION;
      return (ILU_C_Strdup(sbh));
    }
}

CORBA_Object CORBA_ORB_string_to_object (CORBA_Object o, char *sbh, CORBA_Environment *s)
{
  CORBA_Object h;

  if (o != ILU_C_ORB || (h = ILU_C_SBHToObject (sbh, ILU_NIL, ILU_NIL)) == ILU_NIL)
    {
      SYS_EX(ex_CORBA_INV_OBJREF,CORBA_COMPLETED_NO)
      return (ILU_NIL);
    }
  else
    {
      s->_major = CORBA_NO_EXCEPTION;
      return (h);
    }
}

CORBA_boolean CORBA_Object_is_nil (CORBA_Object h, CORBA_Environment *s)
{
  s->_major = CORBA_NO_EXCEPTION;
  return (h == ILU_NIL);
}

CORBA_Object CORBA_Object_duplicate (CORBA_Object h, CORBA_Environment *s)
{
  s->_major = CORBA_NO_EXCEPTION;
  return (h);
}

void CORBA_Object_release (CORBA_Object h, CORBA_Environment *s)
{
  s->_major = CORBA_NO_EXCEPTION;
  /* ilu_DestroyObject( h ); */
}

#define NSysExns 25
static ILU_C_ExceptionCode SysExnsByIndex[NSysExns] = {0,};
#define ILU_SET_ELT(x,y) SysExnsByIndex[ILU_ERRTYP(x)-1] = ILU_C_STDEX(y)

/*L1, L2 unconstrained*/
void
_ILU_C_ConvertError(ILU_C_ENVIRONMENT * env,
		    ilu_Error * err,
		    ILU_C_COMPLETIONSTATUS cstat)
{
  unsigned long   minor;
  ilu_integer     major;
  if (SysExnsByIndex[0] == 0) {
    ILU_SET_ELT(unknown, UNKNOWN);
    ILU_SET_ELT(bad_param, BAD_PARAM);
    ILU_SET_ELT(no_memory, NO_MEMORY);
    ILU_SET_ELT(imp_limit, IMP_LIMIT);
    ILU_SET_ELT(comm_failure, COMM_FAILURE);
    ILU_SET_ELT(inv_objref, INV_OBJREF);
    ILU_SET_ELT(no_permission, NO_PERMISSION);
    ILU_SET_ELT(internal, INTERNAL);
    ILU_SET_ELT(marshal, MARSHAL);
    ILU_SET_ELT(initialize, INITIALIZE);
    ILU_SET_ELT(no_implement, NO_IMPLEMENT);
    ILU_SET_ELT(bad_typecode, BAD_TYPECODE);
    ILU_SET_ELT(bad_operation, BAD_OPERATION);
    ILU_SET_ELT(no_resources, NO_RESOURCES);
    ILU_SET_ELT(no_response, NO_RESPONSE);
    ILU_SET_ELT(persist_store, PERSIST_STORE);
    ILU_SET_ELT(bad_inv_order, BAD_INV_ORDER);
    ILU_SET_ELT(transient, TRANSIENT);
    ILU_SET_ELT(free_mem, FREE_MEM);
    ILU_SET_ELT(inv_ident, INV_IDENT);
    ILU_SET_ELT(inv_flag, INV_FLAG);
    ILU_SET_ELT(intf_repos, INTF_REPOS);
    ILU_SET_ELT(bad_context, BAD_CONTEXT);
    ILU_SET_ELT(obj_adapter, OBJ_ADAPTER);
    ILU_SET_ELT(data_conversion, DATA_CONVERSION);
  }
  minor = ilu_CORBAizeSystemErr(err, &major);
  if (0 <= major && major < NSysExns)
    ILU_C_RAISE_SYS_EXPR(env, SysExnsByIndex[major], minor,
			 cstat);
  else
    ILU_C_RAISE_SYS_EXPR(env, ILU_C_STDEX(INTERNAL), 1000, cstat);
  return;
}

CORBA_octet *
  CORBA_sequence_octet_bufalloc(CORBA_unsigned_long l)
{
  CORBA_octet *v = ilu_malloc(sizeof(CORBA_octet) * l);
  if (v == ILU_NIL)
    return _ILU_C_MallocFailure(sizeof(CORBA_octet) * l);
  else
    return v;
}

CORBA_boolean *
  CORBA_sequence_boolean_bufalloc(CORBA_unsigned_long l)
{
  CORBA_boolean *v = ilu_malloc(sizeof(CORBA_boolean) * l);
  if (v == ILU_NIL)
    return _ILU_C_MallocFailure(sizeof(CORBA_boolean) * l);
  else
    return v;
}

CORBA_char *
  CORBA_sequence_char_bufalloc(CORBA_unsigned_long l)
{
  CORBA_char *v = ilu_malloc(sizeof(CORBA_char) * l);
  if (v == ILU_NIL)
    return _ILU_C_MallocFailure(sizeof(CORBA_char) * l);
  else
    return v;
}

CORBA_unsigned_short *
  CORBA_sequence_unsigned_short_bufalloc(CORBA_unsigned_long l)
{
  CORBA_unsigned_short *v = ilu_malloc(sizeof(CORBA_unsigned_short) * l);
  if (v == ILU_NIL)
    return _ILU_C_MallocFailure(sizeof(CORBA_unsigned_short) * l);
  else
    return v;
}

CORBA_unsigned_long *
  CORBA_sequence_unsigned_long_bufalloc(CORBA_unsigned_long l)
{
  CORBA_unsigned_long *v = ilu_malloc(sizeof(CORBA_unsigned_long) * l);
  if (v == ILU_NIL)
    return _ILU_C_MallocFailure(sizeof(CORBA_unsigned_long) * l);
  else
    return v;
}

CORBA_short *
  CORBA_sequence_short_bufalloc(CORBA_unsigned_long l)
{
  CORBA_short *v = ilu_malloc(sizeof(CORBA_short) * l);
  if (v == ILU_NIL)
    return _ILU_C_MallocFailure(sizeof(CORBA_short) * l);
  else
    return v;
}

CORBA_long *
  CORBA_sequence_long_bufalloc(CORBA_unsigned_long l)
{
  CORBA_long *v = ilu_malloc(sizeof(CORBA_long) * l);
  if (v == ILU_NIL)
    return _ILU_C_MallocFailure(sizeof(CORBA_long) * l);
  else
    return v;
}

CORBA_float *
  CORBA_sequence_float_bufalloc(CORBA_unsigned_long l)
{
  CORBA_float *v = ilu_malloc(sizeof(CORBA_float) * l);
  if (v == ILU_NIL)
    return _ILU_C_MallocFailure(sizeof(CORBA_float) * l);
  else
    return v;
}

CORBA_double *
  CORBA_sequence_double_bufalloc(CORBA_unsigned_long l)
{
  CORBA_double *v = ilu_malloc(sizeof(CORBA_double) * l);
  if (v == ILU_NIL)
    return _ILU_C_MallocFailure(sizeof(CORBA_double) * l);
  else
    return v;
}

CORBA_string *
  CORBA_sequence_string_bufalloc(CORBA_unsigned_long l)
{
  CORBA_string *v = ilu_malloc(sizeof(CORBA_string) * l);
  if (v == ILU_NIL)
    return _ILU_C_MallocFailure(sizeof(CORBA_string) * l);
  else
    return v;
}

