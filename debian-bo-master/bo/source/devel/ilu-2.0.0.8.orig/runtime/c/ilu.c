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
/* $Id: ilu.c,v 1.126 1996/07/12 18:01:57 larner Exp $ */
/* Last tweaked by Mike Spreitzer July 10, 1996 11:42 pm PDT */

#include <stdio.h>

#include <stdarg.h>

#if ((defined(WIN32) || defined(WIN16)) && defined(_WINIO))
#include <winiodef.h>
#endif /* ((defined(WIN32) || defined(WIN16)) && defined(_WINIO)) */

#include <string.h>
#include <stdlib.h>		/* for exit */

#include "iluchdrs.h"

#include <oscalls.h>		/* for OS_SLEEP */
#include <iludebug.h>		/* for _ilu_DebugLevel and LSR_DEBUG */
 
#define  TRUE  1
#define  FALSE 0
#define  AND   &&
#define  OR    ||
#define  NOT   !

#define OPTIONAL(x)	x
#define PASS(x)		x
#define RETAIN(x)	x
#define GLOBAL(x)	x
 
#define SEQUENCE_INCREMENT 5

#define MOST_SPECIFIC_ILU_CLASS(cobj)	((cobj)->type)[0]->c
#define ILU_CLASS_OF_C_CLASS(cclass)	(cclass)[0]->c

/* L1, L2, Main unconstrained */

#define ILU_C_Logging (_ilu_DebugLevel & LSR_DEBUG)

static ilu_boolean c_registered = FALSE;
static ilu_cardinal c_lang = 47;

static ilu_Server defaultServer = ILU_NIL;

static ilu_Server GetDefaultServer(void);

static ilu_boolean threaded = ilu_FALSE;
static ilu_boolean threadedSet = ilu_FALSE;

static void     (*Fork) (void (*proc) (void *arg), void *arg) = 0;

static void MonitorOutgoingConnection(void *rock);

static ilu_cardinal MyLangIdx(void)
{
  if (!c_registered) {
    c_lang = ilu_RegisterLanguage("ANSI-C");
    c_registered = TRUE;
  }
  return c_lang;
}

void ilu_CString__Free (ilu_CString *s)
{
  if (*s != ILU_NIL)
    ilu_free(*s);
}

struct typeRecord_s {
  ilu_Class iluclass;
  ILU_C_Type cclass;
  struct typeRecord_s *next;
};

typedef struct typeRecord_s *typeRecord;

static typeRecord TypeRegistry = ILU_NIL;

void _ILU_C_RegisterSurrogateCType (ilu_Class c, ILU_C_Type t)
{
  typeRecord      r;

  r = (typeRecord) ilu_must_malloc(sizeof(*r));
  r->iluclass = c;
  r->cclass = t;
  r->next = TypeRegistry;
  TypeRegistry = r;
}

/* Inside(obj->server, obj->class) */
static ILU_C_Object *
_ILU_C_CreateSurrogateFromRegistry(ilu_Class c, ilu_Object obj)
{
  typeRecord      p;
  ILU_C_Object   *lspo = ILU_NIL;

  for (p = TypeRegistry; p != ILU_NIL; p = p->next)
    if (c == p->iluclass)
      break;
  if (p != ILU_NIL) {
    lspo = (ILU_C_Object *) ilu_must_malloc(sizeof(ILU_C_Object));
    lspo->server = ilu_ServerOfObject(obj);
    lspo->instanceId = (ilu_Object) obj;
    lspo->data = 0;
    lspo->type = p->cclass;
    lspo->interruptH = ILU_NIL;
    ilu_RegisterLanguageSpecificObject(obj, lspo, MyLangIdx());
  } else {
    ilu_DebugPrintf (
	    "ILU_C:  attempt to create surrogate instance of class \"%s\", but surrogate code is not loaded.\n",
	    c->cl_name);
  }
  return (lspo);
}
 
ilu_CString ILU_C_ClassName (ILU_C_Object *o)
{
  ilu_Class c = ILU_C_ClassRecordOfInstance (o);
  if (o == ILU_NIL)
    return (ILU_NIL);
  else
    return (c->cl_name);
}

ilu_CString ILU_C_ClassID (ILU_C_Object *o)
{
  ilu_Class c = ILU_C_ClassRecordOfInstance (o);
  if (o == ILU_NIL)
    return (ILU_NIL);
  else
    return (c->cl_unique_id);
}

ilu_Class ILU_C_FindILUClassByTypeName (ilu_CString typename)
{
  return (ilu_FindClassFromName(typename));
}

ilu_Class ILU_C_FindILUClassByTypeID (ilu_CString typeID)
{
  return (ilu_FindClassFromID(typeID));
}

/* L1, L2, Main unconstrained */
ilu_Class ILU_C_ClassRecordOfInstance(ILU_C_Object *o)
{
  if (o == ILU_NIL OR o->type == ILU_NIL)
    return (ILU_NIL);
  return ((o->type)[0]->c);
}

/* L1, L2, Main unconstrained */
void *_ILU_C_FindMethod (ILU_C_Object *h, ilu_Class cls, int ndx)
{
  ILU_C_Type c;
  _ILU_C_MethodBlock *d;
 
  c = h->type;
  while ((d = *c++) != ILU_NIL) {
    if (d->c == cls)
      {
	return(( void * ) d->methods[ ndx ]);
      }
  }
  return(( void * ) ILU_NIL );
}
 
/* Main Invariant holds */
ilu_CString ILU_C_SBHOfObject (ILU_C_Object *obj)
{
  ilu_CString sbh = ILU_NIL;

  ilu_Object kobj = _ILU_C_KernelObjOfObj (obj);
  if (kobj != ILU_NIL)
    {
      sbh = ilu_SBHOfObject (kobj);
      ilu_ExitServer (obj->server, MOST_SPECIFIC_ILU_CLASS(obj));
    }
  return (sbh);
}

#ifdef IIOP_PROTOCOL

/* Main Invariant holds */
ilu_CString ILU_C_IOROfObject (ILU_C_Object *obj)
{
  ilu_Error       err;
  ilu_CString     sbh = ILU_NIL;

  ilu_Object      kobj = _ILU_C_KernelObjOfObj(obj);
  sbh = ilu_IOROfObject(kobj, &err);
  if (ILU_ERRNOK(err)) {
    ilu_DebugPrintf("ILU_C_IOROfObject:  Error <%s> in creating IOR.\n",
		    ILU_ERR_NAME(err));
    ILU_HANDLED(err);
    sbh = ILU_NIL;
  }
  if (kobj != ILU_NIL)
    ilu_ExitServer(obj->server, MOST_SPECIFIC_ILU_CLASS(obj));
  return (sbh);
}

#endif

/*Main Invariant holds*/
ILU_C_Object   *
ILU_C_SBHToObject(ilu_CString sbh,
		  ilu_Class static_type,
		  ILU_C_ENVIRONMENT * env)
{
  ILU_C_Object   *h = ILU_NIL;
  ilu_Object      obj;
  ILU_ERRS((bad_locks, broken_locks, inv_objref,
	    no_memory, internal)) lerr;
  env->_major = ILU_C_NO_EXCEPTION;
  obj = ilu_ObjectOfSBH(sbh, static_type, &lerr);
  if (ILU_ERRNOK(lerr))
    _ILU_C_ConvertError(env, &lerr, CORBA_COMPLETED_NO);
  if (obj == ILU_NIL)
    return (ILU_NIL);
  h = ilu_GetLanguageSpecificObject(obj, MyLangIdx());
  if (h == ILU_NIL)
    h = _ILU_C_CreateSurrogateFromRegistry(ilu_ClassOfObject(obj), obj);
  ilu_ExitServer(ilu_ServerOfObject(obj), static_type);
  return (h);
}

/* Main Invariant holds */
void
ILU_C_PingObject(ILU_C_Object * obj, ILU_C_ENVIRONMENT * env)
{
  ilu_Object      kobj = _ILU_C_KernelObjOfObj(obj);
  ilu_Class       mst;
  ilu_Error       err = ILU_INIT_NO_ERR;
  ilu_boolean     ok;
  ilu_Connection  newconn;
  if (kobj == ILU_NIL) {
    ILU_C_RAISE_SYSTEM(env, BAD_PARAM, 0, NO);
    return;
  }
  mst = ilu_ClassOfObject(kobj);
  err = ilu_DeltaHolds(kobj, 1);
  ilu_ExitServer(obj->server, mst);
  ILU_ERR_SWITCH(err) {
    ILU_SUCCESS_CASE;
    ILU_ERR_CASE(GcRegFailed, v) {
      ILU_C_RAISE_SYSTEM(env, COMM_FAILURE, 0, NO);
      return;
    }
    ILU_ERR_ELSE {
      ILU_C_RAISE_SYSTEM(env, INTERNAL, 0, NO);
      return;
    }
  } ILU_ERR_ENDSWITCH;
  ok = ilu_PingObject(kobj, &newconn);
  if (newconn != ILU_NIL)
    (*Fork) (MonitorOutgoingConnection, newconn);
  ilu_EnterServer(obj->server, mst);
  err = ilu_DeltaHolds(kobj, -1);
  ilu_ExitServer(obj->server, mst);
  ILU_ERR_SWITCH(err) {
    ILU_SUCCESS_CASE;
    ILU_ERR_CASE(GcRegFailed, v) {
      ILU_C_RAISE_SYSTEM(env, COMM_FAILURE, 0, MAYBE);
      return;
    }
    ILU_ERR_ELSE {
      ILU_C_RAISE_SYSTEM(env, INTERNAL, 0, MAYBE);
      return;
    }
  } ILU_ERR_ENDSWITCH;
  if (ok)
    env->_major = ILU_C_NO_EXCEPTION;
  else
    ILU_C_RAISE_SYSTEM(env, COMM_FAILURE, 0, MAYBE);
  return;
}

/* Main Invariant holds */
void ILU_C_DestroyObject(ILU_C_OBJECT obj)
{
  ilu_Object      kobj = _ILU_C_KernelObjOfObj(obj);
  ilu_Class       mst;
  if (kobj == ILU_NIL)
    return;
  mst = ilu_ClassOfObject(kobj);
  ilu_RegisterLanguageSpecificObject(kobj, ILU_NIL, MyLangIdx());
  obj->instanceId = ILU_NIL;
  ilu_ExitServer(obj->server, mst);
  return;
}

typedef struct {
  ilu_cardinal    ncobjs, nviobjs;
}               DestructCounters;

/* Inside(kobj's server, ilu_rootClass) */
static int DestroyIt(ilu_Object kobj, ilu_refany rock)
{
  DestructCounters *dc = (DestructCounters *) rock;
  ilu_cardinal    mli = MyLangIdx();
  ILU_C_OBJECT    cobj = ilu_GetLanguageSpecificObject(kobj, mli);
  if (cobj != ILU_NIL) {
    dc->ncobjs++;
    ilu_RegisterLanguageSpecificObject(kobj, ILU_NIL, mli);
    cobj->instanceId = ILU_NIL;
  }
  if (ilu_VeryInterested(kobj))
    dc->nviobjs++;
  return 0;
}

/* Main Invariant holds */
void 
ILU_C_CloseServer(ilu_Server s,
		  ilu_cardinal * ncobjs, ilu_cardinal * nviobjs,
		  ilu_cardinal * nconns, ILU_C_ENVIRONMENT * env)
{
  int             res;
  DestructCounters dc = {0, 0};
  res = ilu_BankAndScanServer(s, DestroyIt, &dc, nconns);
  if (ncobjs != ILU_NIL)
    *ncobjs = dc.ncobjs;
  if (nviobjs != ILU_NIL)
    *nviobjs = dc.nviobjs;
  env->_major = ILU_C_NO_EXCEPTION;
  return;
}

/* Main Invariant holds */
void
ILU_C_DestroyObjectAndServer(ILU_C_OBJECT obj,
			     ILU_C_ENVIRONMENT * env)
{
  ilu_Server      s = obj->server;
  ILU_C_DestroyObject(obj);
  ILU_C_CloseServer(s, ILU_NIL, ILU_NIL, ILU_NIL, env);
  return;
}

/* Main Invariant holds */
ilu_boolean
ILU_C_ValidateOrDestroyObjSvr(ILU_C_OBJECT obj,
			      ILU_C_ENVIRONMENT * env)
{
  ILU_C_PingObject(obj, env);
  switch (env->_major) {
  case ILU_C_NO_EXCEPTION:
    return ilu_TRUE;
  case ILU_C_SYSTEM_EXCEPTION:
    if (ILU_C_EXCEPTION_ID(env) != ILU_C_STDEX(COMM_FAILURE))
      return ilu_FALSE;
    break;
  default:			/* can't happen */
    ILU_C_RAISE_SYSTEM(env, INTERNAL, 0, MAYBE);
    return ilu_FALSE;
  }
  ILU_C_EXCEPTION_FREE(env);
  ILU_C_DestroyObjectAndServer(obj, env);
  return ilu_FALSE;
}

typedef struct CallCons *CallList;
struct CallCons {
  /* L1, L2 unconstrained */

  ilu_Call        head;
  CallList        tail;
};

struct ILU_C_InterruptHandle_s {
  /* L1, L2 unconstrained */
  CallList        calls;
};

/* L1, L2 unconstrained */
static ilu_boolean AddCallToIH(ilu_Call call, ILU_C_InterruptHandle h)
{
  CallList        this = (CallList) ilu_malloc(sizeof(*this));
  if (this == ILU_NIL)
    return FALSE;
  this->head = call;
  this->tail = h->calls;
  h->calls = this;
  return TRUE;
}

/* L1, L2 unconstrained */
static void RemCallFromIH(ilu_Call call, ILU_C_InterruptHandle h)
{
  CallList       *pcur;
  for (pcur = &(h->calls); *pcur != ILU_NIL; pcur = &((*pcur)->tail)) {
    if ((*pcur)->head == call) {
      CallList        doomed = *pcur;
      *pcur = doomed->tail;
      ilu_free(doomed);
      return;
    }
  }
}

/* Main Invariant holds */
ILU_C_InterruptHandle ILU_C_NewInterruptHandle(void)
{
  ILU_C_InterruptHandle h;
  h = (ILU_C_InterruptHandle) ilu_malloc(sizeof(*h));
  if (h != ILU_NIL)
    h->calls = ILU_NIL;
  return h;
}

/* Main Invariant holds */
void
ILU_C_SetObjectInterruptHandle(ILU_C_Object * obj,
			       ILU_C_InterruptHandle h)
{
  if (obj != ILU_NIL)
    obj->interruptH = h;
}

/* Main Invariant holds */
void ILU_C_InterruptHandleCalls(ILU_C_InterruptHandle h)
{
  CallList        cl;
  ilu_Error       lerr;
  for (cl = h->calls; cl != ILU_NIL; cl = cl->tail) {
    ilu_InterruptCall(cl->head, &lerr);
    ILU_HANDLED(lerr);
  }
}

/* Main Invariant holds */
ilu_CString ILU_C_PublishObject (ILU_C_Object *obj)
{
  ilu_Object kobj = _ILU_C_KernelObjOfObj (obj);
  if (kobj != ILU_NIL)
    return(ilu_PublishObject (kobj));
  else
    return (ILU_NIL);
}

/* Main Invariant holds */
ilu_boolean ILU_C_WithdrawObject (ILU_C_Object *obj, ilu_CString proof)
{
  ilu_Object kobj = _ILU_C_KernelObjOfObj (obj);
  if (kobj != ILU_NIL)
    return (ilu_WithdrawObject (kobj, proof));
  else
    return (ilu_FALSE);
}

/* Main Invariant holds; L2 otherwise unrestricted */
OPTIONAL(GLOBAL(ILU_C_Object *))
ILU_C_LookupObject(RETAIN(char *) sid, RETAIN(char *) ih,
		   ilu_Class static_type)
{
  int             change;
  return ILU_C_ReLookupObject(sid, ih, static_type, &change);
}

/* Main Invariant holds; L2 otherwise unrestricted */
OPTIONAL(GLOBAL(ILU_C_Object *))
ILU_C_ReLookupObject(RETAIN(char *) sid, RETAIN(char *) ih,
		     ilu_Class static_type, int *change)
{
  ILU_C_Object   *h = ILU_NIL;
  ilu_Object      obj;

  *change = ilu_ReLookupObject(sid, ih, static_type, &obj);
  if (obj == ILU_NIL)
    return (ILU_NIL);
  h = ilu_GetLanguageSpecificObject(obj, MyLangIdx());
  if (h == ILU_NIL)
    h = _ILU_C_CreateSurrogateFromRegistry(ilu_ClassOfObject(obj), obj);
  ilu_ExitServer(ilu_ServerOfObject(obj), static_type);
  return (h);
}

/*L1, L2, Main unconstrained*/

PASS(ilu_Passport)
ILU_C_CreatePassport (OPTIONAL(PASS(ilu_IdentityInfo)) info,
		      ILU_ERRS((no_memory)) *err)
{
  return ilu_CreatePassport(info, err);
}

PASS(ilu_IdentityInfo)
ILU_C_CopyIdentity (RETAIN(const struct _ilu_IdentityInfo_s *) info,
		    ILU_ERRS((no_memory)) *err)
{
  return ilu_CopyIdentity(info, err);
}

ilu_boolean
ILU_C_AddIdentity (RETAIN(ilu_Passport) pp,
		   PASS(const struct _ilu_IdentityInfo_s *) info,
		   ilu_Error *err)
{
  return ilu_AddIdentity (pp, info, err);
}

OPTIONAL(RETAIN(ilu_IdentityInfo))
ILU_C_FindIdentity (RETAIN(ilu_Passport) pp,
		    ilu_IdentityType type)
{
  return ilu_FindIdentity (pp, type);
}

ilu_boolean
ILU_C_DestroyPassport (PASS(ilu_Passport) pp,
		       ilu_Error *err)
{
  return ilu_DestroyPassport(pp, err);
}

#ifdef SECURE_TRANSPORT

ilu_IdentityInfo
ILU_C_AcquireGSSIdentity (gss_cred_id_t cred, ilu_Error *err)
{
  return ilu_AcquireGSSIdentity (cred, err);
}

ilu_boolean
ILU_C_DecodeGSSIdentity (ilu_IdentityInfo info,	/* input; retain; info to decode */
			 gss_name_t * name,	/* output; name in identity */
			 ilu_FineTime *till,	/* output; good-till; seconds past Unix epoch */
			 gss_OID mech,		/* input; actual mechanism desired; optional */
			 ilu_boolean *local,	/* if TRUE, local; otherwise remote */
			 ilu_cardinal *flags,	/* connection flags, as in gss_inquire_context */
			 ilu_Error *err)
{
  return ilu_DecodeGSSIdentity (info, name, till, mech, local, flags, err);
}

gss_cred_id_t
ILU_C_AcquireGSSCredForName (char *name,		/* name */
			     ilu_cardinal lifetime,	/* lifetime */
			     gss_OID mech,		/* secmech */
			     ilu_boolean accept_only,	/* accept_only */
			     ilu_Error *err		/* err */)
{
  return ilu_AcquireGSSCredForName (name, lifetime, mech, accept_only, err);
}

PASS(ilu_string)
ILU_C_GSSNameToString (gss_name_t name,
		       ilu_Error *err)
{
  return ilu_GSSNameToString (name, err);
}

#endif /* def SECURE_TRANSPORT */

/**before: Main Invariant, Call-Hi(call);
    after: Call-Invariant(call, err) && call->ca_ms == ilu_cmsHi*/
ILU_C_Object   *
_ILU_C_GetServerSingleton(ilu_Call call, ILU_ERRS((bad_param)) * err)
{
  ILU_C_Object   *h;
  ilu_Object      kobj;

  if ((kobj = ilu_GetCallSingleton(call, err)) != ILU_NIL) {
    h = (ILU_C_Object *) ilu_GetLanguageSpecificObject(kobj, MyLangIdx());
    ilu_ExitServer(ilu_ServerOfObject(kobj),
		   ilu_IntroTypeOfCall(call));
    return (h);
  }
  return (ILU_NIL);
}

/*before: not Inside (cobj->server, cobj->type->c) */
/*after:  return != ILU_NIL => Inside(cobj->server, cobj->type->c) */
ilu_Object _ILU_C_KernelObjOfObj (ILU_C_Object *cobj)
{
  ilu_EnterServer (cobj->server, MOST_SPECIFIC_ILU_CLASS(cobj));
  if (cobj->instanceId != ILU_NIL)
    return (cobj->instanceId);
  else
    {
      ilu_ExitServer (cobj->server, MOST_SPECIFIC_ILU_CLASS(cobj));
      return (ILU_NIL);
    }
}

struct _ILU_C_ObjectTable_struct {
  ILU_C_Object * (*ot_object_of_ih) (ilu_string, ilu_private);
  void (*ot_free_ot) (ilu_private);
  ilu_private ot_user_data;
  ilu_ObjectTable ot_kernel_ot;
};

static ilu_Object
  _ILU_C_OT_ObjectOfIh (ilu_ObjectTable self,
			ilu_string ih)
{
  ILU_C_ObjectTable cot = (ILU_C_ObjectTable)(self->ot_rock);
  ILU_C_Object *h;
  h = (cot->ot_object_of_ih) (ih, cot->ot_user_data);
  if (h == ILU_NIL)
    return (ILU_NIL);
  else
    return (h->instanceId);
}

static void _ILU_C_OT_FreeSelf (ilu_ObjectTable self)
{
  (*((ILU_C_ObjectTable)(self->ot_rock))->ot_free_ot)
  (((ILU_C_ObjectTable)(self->ot_rock))->ot_user_data);
  ilu_free(self->ot_rock);
}

ILU_C_ObjectTable
  ILU_C_CreateObjectTable (
			   /*L1 >= {server}; L1 >= {gcmu} if result is true and collectible*/
			   CORBA_Object (*object_of_ih)(ilu_string /* ih */,
							ilu_private /* user_data */),
			   void (*free_ot) (ilu_private /* user_data */),
			   ilu_private user_data)
{
  ILU_C_ObjectTable cot = ilu_malloc(sizeof(*cot));
  ilu_ObjectTable ot = ilu_malloc(sizeof(*ot));
  cot->ot_object_of_ih = object_of_ih;
  cot->ot_free_ot = free_ot;
  cot->ot_user_data = user_data;
  cot->ot_kernel_ot = ot;
  ot->ot_object_of_ih = _ILU_C_OT_ObjectOfIh;
  ot->ot_free_self = _ILU_C_OT_FreeSelf;
  ot->ot_rock = cot;
  return (cot);
}

/* locking unconstrained */
void *_ILU_C_MallocFailure (ilu_cardinal nbytes)
{
  ilu_DebugPrintf("*** ilu_malloc(%u) failed.  Returning NIL.\n",
		  nbytes);
  return ILU_NIL;
}

/* locking unconstrained */
ilu_CString ILU_C_Strdup (ilu_CString s)
{
  if (s == ILU_NIL)
    return (ILU_NIL);
  else {
    ilu_CString     s2 = ilu_must_malloc(strlen(s) + 1);
    strcpy(s2, s);
    return (s2);
  }
}

/* L1, L2, Main unconstrained */
ilu_boolean _ILU_C_IsSingleton (ilu_Class c)
{
  return (c->cl_singleton != ILU_NIL);
}

/* Main invariant holds */
ILU_C_Object *
  ILU_C_CreateSurrogateObject (ilu_Class type,
			       RETAIN(ilu_string) ih,
			       ilu_Server server,
			       ILU_C_ENVIRONMENT *env)
/* Create and return an instance of the specified type,
   with the specified ih, on the specified server */
{
  ILU_C_Object   *h = ILU_NIL;
  ilu_Object      obj;
  ilu_Server s;
  ILU_ERRS((bad_locks, broken_locks, inv_objref,
	    no_memory, internal)) lerr;
  env->_major = ILU_C_NO_EXCEPTION;
  obj = ilu_FindOrCreateSurrogate ((server == ILU_NIL) ? GetDefaultServer() : server,
				   ih, type, &lerr);
  if (ILU_ERRNOK(lerr))
    _ILU_C_ConvertError(env, &lerr, CORBA_COMPLETED_NO);
  if (obj == ILU_NIL)
    return (ILU_NIL);
  h = ilu_GetLanguageSpecificObject(obj, MyLangIdx());
  if (h == ILU_NIL)
    h = _ILU_C_CreateSurrogateFromRegistry(ilu_ClassOfObject(obj), obj);
  ilu_ExitServer(ilu_ServerOfObject(obj), type);
  return (h);
}

/* insideServer => Inside(server, class) */
/* otherwise, Main invariant holds */
ILU_C_Object   *
_ILU_C_CreateTrueObject(ILU_C_Type class,
			OPTIONAL(ilu_CString) instance_handle,
			OPTIONAL(ilu_Server) server,
			void *client_data,
			ilu_boolean insideServer)
{
  ILU_C_Object   *lspo;
  ilu_Class       iluclass;
  ilu_CString     id;
  char            idbuf[10];
  static ilu_cardinal idcounter = 0;

  if (server == ILU_NIL)
    server = GetDefaultServer();

  iluclass = (*class)->c;

  lspo = (ILU_C_Object *) ilu_must_malloc(sizeof(ILU_C_Object));
  lspo->type = class;
  lspo->server = server;
  lspo->data = client_data;

  if (instance_handle != ILU_NIL)
    id = ILU_C_Strdup(instance_handle);
  else {
    sprintf(idbuf, "%lu", (unsigned long) ++idcounter);
    id = ILU_C_Strdup(idbuf);
  }

  if (!insideServer)
    ilu_EnterServer(lspo->server, iluclass);
  lspo->instanceId = ilu_FindOrCreateTrueObject(id, lspo->server,
						iluclass, lspo);
  if (!insideServer)
    ilu_ExitServer(lspo->server, iluclass);
  if (lspo->instanceId == ILU_NIL) {
    ilu_free(lspo);
    ilu_DebugPrintf (
	    "ILU_C:  Can't create kernel object for true object of type \"%s\".\n",
	    iluclass->cl_name);
    return (ILU_NIL);
  } else
    return (lspo);
}

/*Main invariant, Call-Hi(call)*/
ILU_C_Object   *
_ILU_C_InputObject(ilu_Call call, ilu_Class putative_class,
		   ilu_boolean discriminator,
		   ILU_ERRS((IoErrs)) * err)
{
  ilu_Object      obj = ILU_NIL;
  ILU_C_Object   *o = ILU_NIL;
  ilu_Class       c;

  ilu_InputObjectID(call, &obj, discriminator,
		    putative_class, err);
  if (ILU_ERRNOK(*err))
    return ILU_NIL;
  if (obj == ILU_NIL)
    return (ILU_NIL);
  /* now Inside(obj->server, putative_class) */
  o = (ILU_C_Object *) ilu_GetLanguageSpecificObject(obj, MyLangIdx());
  if (o == ILU_NIL) {
    if ((c = ilu_ClassOfObject(obj)) != ILU_NIL)
      o = _ILU_C_CreateSurrogateFromRegistry(c, obj);
  }
  ilu_ExitServer(ilu_ServerOfObject(obj), putative_class);
  return (o);
}
 
/*Main invariant, Call-Hi(call)*/
ilu_boolean 
_ILU_C_OutputObject(ilu_Call call, ILU_C_Object * obj,
		    ilu_Class putative_class,
		    ilu_boolean discriminator,
		    ILU_ERRS((IoErrs)) * err)
{
  ILU_CLER(*err);
  if (obj != ILU_NIL)
    ilu_EnterServer(obj->server, MOST_SPECIFIC_ILU_CLASS(obj));
  ilu_OutputObjectID(call,
		     (obj == ILU_NIL) ? ILU_NIL : obj->instanceId,
		     discriminator, putative_class, err);
  return ILU_ERROK(*err);
}
 
/* Main invariant holds */
ilu_cardinal 
_ILU_C_SizeOfObject(ilu_Call call, ILU_C_Object * obj,
		    ilu_Class putative_class,
		    ilu_boolean discriminator,
		    ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal    sz;

  if (obj != ILU_NIL)
    ilu_EnterServer(obj->server, MOST_SPECIFIC_ILU_CLASS(obj));
  sz = ilu_SizeOfObjectID(call,
		      (obj == ILU_NIL) ? ILU_NIL : obj->instanceId,
			  discriminator, putative_class, err);
  if (ILU_ERROK(*err) && (obj != ILU_NIL))
    ilu_ExitServer(obj->server, MOST_SPECIFIC_ILU_CLASS(obj));
  return (sz);
}
 
/***********************************************************************/
/***********************************************************************/
/***********************************************************************/
/***********************************************************************/

/*		Server code					       */

/***********************************************************************/
/***********************************************************************/
/***********************************************************************/
/***********************************************************************/

#include <stdio.h>      /* I/O defs (including popen and pclose) */

#if ((defined(WIN32) || defined(WIN16)) && defined(_WINIO))
#include <winiodef.h>
#endif /* ((defined(WIN32) || defined(WIN16)) && defined(_WINIO)) */

#include <sys/types.h>
#include <errno.h>
#if !(defined(WIN32) || defined(WIN16))
/* dll no sys/errno in VC++2 */
#include <sys/errno.h>
#endif /* not WIN32 or WIN16 */

#if (defined(WIN32) || defined(WIN16))
/* for WSAGetLastError */
#include <winsock.h>
#endif /* WIN32 or WIN16 */

static void     _ILU_C_ReadConnectionRequests(void *arg);
static void     _ILU_C_ReadConnectionRequest(ilu_private);
static void     _ILU_C_ReadServiceRequest(ilu_private);
static ilu_boolean 
_ILU_C_FinalServiceRequest(ilu_private arg,
			   ilu_boolean single_threaded);
static void PassNewConnections(void *rock);

extern ilu_Exception	ex_ilu_ProtocolError;

static char *DefaultProtocol = "sunrpc";
static ilu_string DefaultTransport[] = { "sunrpcrm", "tcp_0_0", ILU_NIL };
static ilu_string inmemTinfo[] = { "inmem_", ILU_NIL };

/* Main invariant holds */
ilu_boolean     ILU_C_SetFork(void (*fork) (void (*proc) (void *arg),
				            void *arg))
{
  ILU_ERRS((internal)) lerr;
  if (threadedSet)
    return ilu_FALSE;
  threadedSet = ilu_TRUE;
  Fork = fork;
  threaded = ilu_TRUE;
  (void) MyLangIdx();
  (void) GetDefaultServer();
  (*fork)(PassNewConnections, ILU_NIL);
  (void) ilu_NewConnectionGetterForked(&lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  return (ilu_TRUE);
}

/*
 * We don't want to mention ilu_OSForkNewThread() or
 * ilu_InitializeOSThreading explicitly, in case the application
 * isn't using them, to avoid linking that object module
 * unnecessarily.  So we indirect through KernelForkProc. If the
 * application code uses ILU_C_USE_OS_THREADS, the right things are
 * automatically invoked.
 */

static ILU_C_ErrableForkProc errableFork = 0;

static void ErrlessFork(ILU_C_WorkProc threadproc, void *threadarg)
{
  ILU_ERRS((no_memory, no_resources, internal)) err;
  (void) ((*errableFork) (threadproc, threadarg, &err));
  ILU_MUST_BE_SUCCESS(err);
}

/*Main invariant holds*/
ilu_boolean
ILU_C_EnableThreads(ILU_C_ThreadSetupProc s,
		    ILU_C_ErrableForkProc f)
{
  ILU_ERRS((bad_param, no_memory, no_resources,
	    internal)) err;

  if (s == 0 || f == 0) {
    ilu_DebugPrintf("ilu.c:ILU_C_EnableThreads given silly arguments!\n");
    return ilu_FALSE;
  }
  if (!(*s) (&err)) {
    ilu_DebugPrintf("ILU_C_EnableThreads:  error %s (raised at line %d of %s) attempting to set ILU kernel multi-threaded!\n",
		    ILU_ERR_NAME(err), ilu_ErrorLine(&err),
		    ilu_ErrorFile(&err));
    ILU_HANDLED(err);
    return ilu_FALSE;
  }
  errableFork = f;
  ILU_C_SetFork(ErrlessFork);
  return ilu_TRUE;
}

/*Main invariant holds */
static void MonitorOutgoingConnection(void *rock)
{
  ilu_Connection  conn = (ilu_Connection) rock;
  ILU_ERRS((IoErrs)) lerr;
  (void) ilu_OutgoingConnectionThreadProc(conn, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
}

/* Main invariant holds */
static void PassNewConnections(void *rock)
{
  ilu_Connection  nu;
  ILU_ERRS((internal)) lerr;
  while (1) {
    nu = ilu_OtherNewConnection(&lerr);
    if (nu != ILU_NIL)
      (*Fork) (MonitorOutgoingConnection, nu);
    ILU_MUST_BE_SUCCESS(lerr);
  }
}

/*
 * Create a true server then open an ILU port so we've got
 * something to export objects on. This procedure should be called
 * only once.
 */
/* Main invariant holds */
ilu_Server
ILU_C_InitializeServer(char *serverID, ILU_C_ObjectTable obj_tab,
		       char *protocol, ilu_TransportInfo transport,
		       ilu_Passport pp, ilu_boolean setdefaultport)
{
  ilu_Server      server;
  ilu_Port        port;
  ilu_boolean     closed = 0;
  ilu_Error	 err;
  char           *sid;
  char           *pinfo = ((protocol == ILU_NIL) ? DefaultProtocol
			   : protocol);
  ilu_TransportInfo tinfo = ((transport == ILU_NIL) ? DefaultTransport
			     : transport);
  threadedSet = ilu_TRUE;
  if (serverID == ILU_NIL)
    sid = ilu_InventID();
  else
    sid = ILU_C_Strdup(serverID);
  if (sid == ILU_NIL) {
    ilu_DebugPrintf (
       "ilu.c:  Unable to allocate for copy of server ID \"%s\"\n",
	    serverID ? serverID : "(unknown gensymed ID)");
    exit(1);
  }
  server = ilu_CreateTrueServer(sid,
	    (obj_tab == ILU_NIL) ? ILU_NIL : obj_tab->ot_kernel_ot,
				MyLangIdx());
  if (server == ILU_NIL) {
    ilu_DebugPrintf (
	"ilu.c:  Couldn't create true server from (\"%s\", %p).\n",
	    sid, obj_tab);
    exit(1);
  }
  if (protocol != ILU_NIL OR transport != ILU_NIL OR setdefaultport) {
    port = ilu_CreatePort(server, pinfo, tinfo, pp, &err);
    if (port == ILU_NIL) {
      char **ti;
      ilu_DebugPrintf (
	      "ilu.c:  Error <%s> opening port with protocol = \"%s\" and transport =",
	      ILU_ERR_NAME(err), pinfo);
      ILU_HANDLED(err);
      for (ti = tinfo;  *ti != ILU_NIL;  ti++)
	ilu_DebugPrintf (" \"%s\"", *ti);
      ilu_DebugPrintf (".\n");
      ilu_BankServer(server);
      return (ILU_NIL);
    }
    if (setdefaultport)
      ilu_SetServerDefaultPort(server, port);
    if (threaded) {
      Fork(_ILU_C_ReadConnectionRequests, port);
    } else {
      ILU_ERRS((no_memory, imp_limit,
		no_resources, bad_param,
		bad_locks, internal,
		broken_locks)) lerr;
      if (!ilu_SetConnectionRequestHandler(port,
				      _ILU_C_ReadConnectionRequest,
					   port, &lerr)) {
	ilu_DebugPrintf (
	  "ilu.c:  Can't register connection request handler for new server;\n");
	ilu_DebugPrintf ("sid=%s, k-err=%s\n", sid, ILU_ERR_NAME(lerr));
	ILU_HANDLED(lerr);
	ilu_BankServer(server);
	return (ILU_NIL);
      }
    }
  }
  port = ilu_CreatePort(server, DefaultProtocol, inmemTinfo, pp, &err);
  if (port == ILU_NIL) {
    ilu_DebugPrintf (
	    "ilu.c:  Error <%s> creating local port (protocol=\"%s\", tinfo=\"%s\")\n",
	    ILU_ERR_NAME(err), DefaultProtocol, inmemTinfo[0]);
    ILU_HANDLED(err);
  } else {
    ILU_ERRS((no_memory, imp_limit,
	      no_resources, bad_param,
	      bad_locks, internal,
	      broken_locks)) lerr;
    if (!ilu_SetConnectionRequestHandler(port,
				      _ILU_C_ReadConnectionRequest,
					 port, &lerr)) {
      ilu_DebugPrintf (
	      "ilu.c:  Can't register connection request handler for local port of new server;\n");
      ilu_DebugPrintf ("sid=%s, k-err=%s\n", sid, ILU_ERR_NAME(lerr));
      ILU_HANDLED(lerr);
    }
  }
  return (server);
}

/*
 * Create a true server then open an ILU port so we've got
 * something to export objects on.  This function should be called
 * at most once.
 */
/* Main invariant holds */
static ilu_Server GetDefaultServer(void)
{
  if (defaultServer != ILU_NIL)
    return defaultServer;
  defaultServer = ILU_C_InitializeServer(ILU_NIL, ILU_NIL, ILU_NIL,
					 ILU_NIL, ILU_NIL, TRUE);
  if (defaultServer == ILU_NIL) {
    ilu_DebugPrintf("ilu.c:  Error:  Couldn't create default server.\n");
    exit(1);
  }
  return defaultServer;
}

/* Main invariant holds */
static void _ILU_C_RunConnection(void *arg)
{
  ilu_Connection  conn = (ilu_Connection) arg;
  ilu_boolean     closed = ilu_FALSE;
  while (NOT closed) {
    closed = NOT ilu_BlockingWaitForInputOnConnection(conn, ILU_NIL);
    if (closed)
      break;
    closed = _ILU_C_FinalServiceRequest(conn, ilu_FALSE);
  }
  if (ILU_C_Logging)
    ilu_DebugPrintf(
		 "Connection %p no longer waiting for requests.\n",
		    conn);
  ilu_CloseConnection(conn);
}

extern void _ilu_WaitForInputOnFD (int, ilu_boolean *, void *);

/* Main invariant holds */
static void _ILU_C_ReadConnectionRequests(void *arg)
{
  ilu_Port        p = (ilu_Port) arg;
  ilu_boolean     closed = 0;
  ilu_Connection  conn;

  while (1) {
    if (ILU_C_Logging)
      ilu_DebugPrintf("Port %p waiting for connection request.\n", p);
    ilu_WaitForPortConnectionRequest(p);
    conn = ilu_HandleNewConnection(p, &closed);
    if (closed)
      break;
    if (conn == ILU_NIL) {
      if (ILU_C_Logging)
	ilu_DebugPrintf("Port %p connection request declined.\n", p);
    } else {
      /* fork thread to run new connection */
      Fork(_ILU_C_RunConnection, conn);
    }
  }
  if (ILU_C_Logging)
    ilu_DebugPrintf(
	    "Port %p no longer waiting for connection requests.\n",
		    p);
}

/*L1, L2, Main unconstrained*/
static ilu_boolean EnableRequests(ilu_Connection conn, ilu_Call call)
{
  ilu_Server      s = ilu_ServerOfConnection(conn);
  ilu_string      sid = ilu_IDOfServer(s);
  ilu_boolean     ans;
  ILU_ERRS((no_memory, no_resources)) lerr;
  call->ca_reqs_enabled = ilu_TRUE;
  ans = ilu_SetConnectionInputHandler(conn, _ILU_C_ReadServiceRequest,
				      conn, &lerr);
  if (!ans) {
    if (ILU_C_Logging)
      printf("ilu.c:  Can't register request handler for conn %p server %s!\nError = %s\n",
	     conn, sid, ILU_ERR_NAME(lerr));
    else
      ilu_DebugPrintf(
	    "ilu.c:  Can't register request handler for conn %p server %s!\nError = %s\n",
	    conn, sid, ILU_ERR_NAME(lerr));
    ILU_HANDLED(lerr);
  } else if (ILU_C_Logging)
    printf("Connection %p server %s awaiting requests.\n",
	   conn, sid);
  return ans;
}

/*L1, L2, Main unconstrained*/
static ilu_boolean DisableRequests(ilu_Connection conn, ilu_Call call)
{
  ilu_Server      s = ilu_ServerOfConnection(conn);
  ilu_string      sid = ilu_IDOfServer(s);
  ilu_boolean     ans;
  ILU_ERRS((no_memory, no_resources)) lerr;
  call->ca_reqs_enabled = ilu_FALSE;
  ans = ilu_SetConnectionInputHandler(conn, NULL, ILU_NIL, &lerr);
  if (!ans) {
    if (ILU_C_Logging)
      printf("ilu.c:  failure (%s) detected when unregistering request handler for conn %p server %s!\n",
	     ILU_ERR_NAME(lerr), conn, sid);
    else
      ilu_DebugPrintf("ilu.c:  failure (%s) detected when unregistering request handler for conn %p server %s!\n",
		      ILU_ERR_NAME(lerr), conn, sid);
    ILU_HANDLED(lerr);
  } else if (ILU_C_Logging)
    printf("Connection %p server %s not awaiting requests.\n",
	   conn, sid);
  return ans;
}

/* Main invariant holds */
static void
_ILU_C_ReadConnectionRequest(ilu_private arg)
{
  ilu_Port        p = (ilu_Port) arg;
  ilu_boolean     closed = 0;
  ilu_Connection  conn;
  ilu_Call_s      nocall;
  if (ILU_C_Logging)
    printf("Port %p handling connection request.\n", p);
  conn = ilu_HandleNewConnection(p, &closed);
  if (closed) {
    ilu_DebugPrintf("Port %p closed.\n", p);
  } else if (conn == ILU_NIL) {
    if (ILU_C_Logging)
      ilu_DebugPrintf("Port %p connection request declined.\n", p);
  } else
    EnableRequests(conn, &nocall);
}

typedef void (*cstubproc) (ilu_Call, ilu_Error *);

typedef struct {
  ilu_Call        call;
  ilu_Method      method;
}               C_Invokn;

/** Main;
    before: L2    >=    {call's conn's callmu, iomu};
    after:  L2 disjoint {call's conn's callmu, iomu} */
static void DoInvocation(void *arg)
{
  C_Invokn       *ci = (C_Invokn *) arg;
  ILU_ERRS((bad_locks, IoErrs)) lerr;
  (*((cstubproc) ci->method->me_stubproc)) (ci->call, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);	/* should hand off to app */
}

/* Returns TRUE iff connection closed. */
/*Main Invariant holds
  before: L2 disjoint {arg's callmu, iomu}
  after:  L2     >=   {conn's callmu, iomu} if result==ilu_good_request,
  after:  L2 disjoint {conn's callmu, iomu} if result!=ilu_good_request */
/*If fd_significant: _ILU_C_ReadServiceRequest registered for fd*/
static          ilu_boolean
_ILU_C_FinalServiceRequest(ilu_private arg,
			   ilu_boolean single_threaded)
{
  ilu_Call_s      call_s, *call;
  ilu_boolean     initted;
  ilu_Class       class;
  ilu_Connection  conn = (ilu_Connection) arg;
  ilu_Method      method;
  ilu_cardinal    SN;
  ilu_RcvReqStat  stat;
  ILU_ERRS((bad_locks, IoErrs)) lerr = ILU_INIT_NO_ERR;
  C_Invokn       *ci = ILU_NIL;

  if (ILU_C_Logging)
    printf("Activity on connection %p:  ", conn);
  if (single_threaded) {
    call = &call_s;
    DisableRequests(conn, call);
  } else if (ilu_ThreadPerRequest(conn)) {
    call = ilu_malloc(sizeof(*call));
    ci = ilu_malloc(sizeof(*ci));
    if (call == ILU_NIL || ci == ILU_NIL) {	/* out of memory */
      ilu_CloseConnection(conn);
      return ilu_TRUE;
    }
  } else
    call = &call_s;
  stat = ilu_ReceiveRequest(call, &initted, conn, &class, &method,
			    &SN, &lerr);
  if (ILU_C_Logging) {
    if (stat == ilu_RcvReqStat_request)
      printf("Received request SN %lu, method \"%s\" of class \"%s\"\n",
	     (unsigned long) SN, method->me_name, class->cl_name);
    else
      printf("ReceiveRequest => %s, *initted = %s, *err = %s\n",
	     ((stat == ilu_RcvReqStat_noop) ? "noop" :
	      ((stat == ilu_RcvReqStat_quit) ? "quit" :
	       "invalid result code")),
	     ((initted) ? "T" : "F"),
	     (ILU_ERROK(lerr) ? "SUCCESS" : ILU_ERR_NAME(lerr)));
  }
  if (stat == ilu_RcvReqStat_request) {
    /* L2 >= {conn's callmu, iomu} */
    if (ci) {
      ci->call = call;
      ci->method = method;
      (*Fork) (DoInvocation, ci);
      /* L2 disjoint {conn's callmu, iomu} (handed off to new thd) */
    } else {
      (*((cstubproc) method->me_stubproc)) (call, &lerr);
      /* L2 disjoint {conn's callmu, iomu} */
      /* single_threaded != threaded */
      /* !threaded <=> _ILU_C_ReadServiceRequest registered */
    }
    return (ilu_FALSE);
  } else if (initted) {
    ilu_FinishCall(call, &lerr);
  }

  ILU_ERR_SWITCH(lerr) {

    ILU_SUCCESS_CASE ;

    ILU_ERR_ELSE
      ILU_ERRPRINTF("_ILU_C_FinalServiceRequest Warning: error %s from %s line %d\n",
		ILU_ERR_NAME(lerr), ilu_ErrorFile(&lerr), ilu_ErrorLine(&lerr));

  } ILU_ERR_ENDSWITCH;	/* should hand error off to app */

  if (stat == ilu_RcvReqStat_quit)
    return ilu_TRUE;
  if (single_threaded)
    EnableRequests(conn, call);
  return ilu_FALSE;
}

/* Returns TRUE iff connection closed. */
/*Main Invariant holds
  before: L2 disjoint {arg's callmu, iomu}
  after:  L2     >=   {conn's callmu, iomu} if result==ilu_good_request,
  after:  L2 disjoint {conn's callmu, iomu} if result!=ilu_good_request */
static void 
_ILU_C_ReadServiceRequest(ilu_private arg)
{
  (void) _ILU_C_FinalServiceRequest(arg, ilu_TRUE);
  return;
}


/**before: Main Invariant, Call-Hi(call);
    after: Call-Invariant(call, err),
	   success => call->ca_ms == ilu_cmsLo*/
/*If not threaded:
  before: _ILU_C_ReadServiceRequest not registered;
  after:  _ILU_C_ReadServiceRequest registered iff protocol concurrent.*/
ilu_boolean
_ILU_C_FinishParameters(ilu_Call call, ILU_C_Object * obj,
			ILU_ERRS((bad_locks, broken_locks)) * err)
{
  ilu_Connection  conn;
  ilu_boolean     ans;
  conn = ilu_ConnectionOfCall(call);
  ans = ilu_RequestRead(call, err);
  if ((!threaded) && ilu_ThreadPerRequest(conn))
    EnableRequests(conn, call);
  return ans;
}

/**before: Main Invariant && Call-Lo(call);
    after: Call-Invariant(call, err),
	   success => call->ca_ms == ilu_cmsHi. */
/**If not threaded:
  before: _ILU_C_ReadServiceRequest registered iff protocol concurrent;
  after:  _ILU_C_ReadServiceRequest not registered.*/
ilu_boolean
_ILU_C_BeginReply(ilu_Call call, ilu_boolean exceptions,
		  ilu_cardinal argSize,
		  ILU_ERRS((bad_locks, IoErrs)) * err)
{
  ilu_Connection  conn = ilu_ConnectionOfCall(call);
  ilu_boolean     ans;
  if ((!threaded) && ilu_ThreadPerRequest(conn))
    DisableRequests(conn, call);
  ans = ilu_BeginReply(call, exceptions, argSize, err);
  return ans;
}

/**before: Main Invariant && Call-Lo(call);
    after: Call-Invariant(call, err),
	   success => call->ca_ms == ilu_cmsHi. */
/*If not threaded:
  before: _ILU_C_ReadServiceRequest registered iff protocol concurrent;
  after:  _ILU_C_ReadServiceRequest not registered.*/
ilu_boolean
_ILU_C_BeginException(ilu_Call call, ilu_cardinal evalue,
		      ilu_cardinal argSize,
		      ILU_ERRS((bad_locks, IoErrs)) * err)
{
  ilu_Connection  conn = ilu_ConnectionOfCall(call);
  ilu_boolean     ans;
  if ((!threaded) && ilu_ThreadPerRequest(conn))
    DisableRequests(conn, call);
  ans = ilu_BeginException(call, evalue, argSize, err);
  return ans;
}

/**before: Main Invariant, Call-Hi(call);
    after: Call-Invariant(call, err) && Call-Hi(call)*/
/**If not threaded:
  before: _ILU_C_ReadServiceRequest not registered;
  after:  _ILU_C_ReadServiceRequest registered.*/
ilu_boolean
_ILU_C_FinishReply(ilu_Call call, ILU_ERRS((bad_locks, IoErrs)) * err)
{
  ilu_Connection  conn = ilu_ConnectionOfCall(call);
  ilu_boolean     ans;
  ans = ilu_FinishReply(call, err);
  return ans;
}

/**Before: Call-Invariant(call, err);
    After: Main Invariant*/
void 
_ILU_C_FinishServingCall(ilu_Call call, ilu_Error * err)
{
  ilu_Connection  conn = ilu_ConnectionOfCall(call);
  ilu_FinishCall(call, err);
  ILU_ERR_SWITCH(*err) {
    ILU_ERR_CASE(comm_failure, v)
      0;
    ILU_ERR_ELSE {
      if (!threaded && !call->ca_reqs_enabled)
	EnableRequests(conn, call);
    }
  } ILU_ERR_ENDSWITCH;
  ILU_HANDLED(*err);
}

/**before: Main Invariant, Call-Hi(call);
    after: Call-Invariant(call, err) && Call-Hi(call)*/
/*If* not threaded:
  before: _ILU_C_ReadServiceRequest not registered;
  after:  _ILU_C_ReadServiceRequest registered.*/
ilu_boolean
_ILU_C_FinishException(ilu_Call call,
		       ILU_ERRS((bad_locks, IoErrs)) * err)
{
  ilu_Connection  conn = ilu_ConnectionOfCall(call);
  ilu_boolean     ans;
  ans = ilu_FinishException(call, err);
  return ans;
}

/**before: Main Invariant, Call-Lo(call);
    after: Call-Invariant(call, err),
	   success => call->ca_ms == ilu_cmsNo*/
/**If not threaded:
  before: _ILU_C_ReadServiceRequest registered iff protocol concurrent;
  after:  _ILU_C_ReadServiceRequest registered.*/
ilu_boolean
_ILU_C_NoReply(ilu_Call call,
	       ILU_ERRS((bad_param, bad_locks, broken_locks)) * err)
{
  ilu_Connection  conn = ilu_ConnectionOfCall(call);
  ilu_boolean     ans;

  ans = ilu_NoReply(call, err);
  return ans;
}

void ILU_C_Stoppable_Run(int* stop) {

    threadedSet = ilu_TRUE;
    ilu_RunMainLoop( stop );
}

/* Main Invariant holds */
void ILU_C_Run( )
{
    int		stop = 0;

    threadedSet = ilu_TRUE;
    if (threaded)
      while (1) OS_SLEEP(30);
    else
      ilu_RunMainLoop( &stop );
}

/* unrestricted */
unsigned int _ILU_C_SafeStrlen (ilu_CString str)
{
  if (str == ILU_NIL)
    return 0;
  else
    return (strlen((char *) str));
}

/* unrestricted */
void _ILU_C_ExtendString (CORBA_char **str, CORBA_char item, CORBA_boolean atend)
{
  CORBA_char *s2;
  ilu_cardinal size;

  if (*str == ILU_NIL)
    size = 0;
  else
    size = strlen((char *) (*str));
  s2 = ilu_must_malloc(size + 2);
  if (!atend)
    {
      s2[0] = item;
      if (*str != ILU_NIL)
	strncpy ((char *) (s2 + 1), (char *) (*str), size);
      s2[size + 1] = 0;
    }
  else
    {
      if (*str != ILU_NIL)
	strncpy ((char *) s2, (char *) (*str), size);
      s2[size] = item;
      s2[size + 1] = 0;
    }
  *str = s2;
}

/* unrestricted */
void _ILU_C_ExtendWString (ilu_character **str, ilu_character item, CORBA_boolean atend)
{
  ilu_character *s2;
  ilu_cardinal size;

  size = _ILU_C_SafeWStrlen(*str);
  s2 = ilu_must_malloc((size + 2) * sizeof(ilu_character));
  if (!atend)
    {
      s2[0] = item;
      if (*str != ILU_NIL)
	{
	  ilu_character *p, *q;
	  ilu_cardinal i;

	  for (p = s2 + 1, q = *str, i = 0;  i < size;  i++)
	    *p++ = *q++;
	}
      s2[size + 1] = 0;
    }
  else
    {
      if (*str != ILU_NIL)
	{
	  ilu_character *p, *q;
	  ilu_cardinal i;

	  for (p = s2, q = *str, i = 0;  i < size;  i++)
	    *p++ = *q++;
	}
      s2[size] = item;
      s2[size + 1] = 0;
    }
  *str = s2;
}

/*unrestricted*/
void _ILU_C_PopString (CORBA_char **s, CORBA_char *ret)
{
  if (s != ILU_NIL && *s != ILU_NIL && **s != 0)
    {
      *ret = **s;
      *s = *s + 1;      
    }
}

/*unrestricted*/
void _ILU_C_PopWString (ilu_character **s, ilu_character *ret)
{
  if (s != ILU_NIL && *s != ILU_NIL && **s != 0)
    {
      *ret = **s;
      *s = *s + 1;
    }
}

/*unrestricted*/
unsigned int _ILU_C_SafeWStrlen (ilu_character *str)
{
  register ilu_character *p = str;

  if (str == ILU_NIL)
    return 0;

  while (*p++ != 0)
    ;
  return (p-str);
}

static          ilu_boolean
_ILU_C_CatchException(ilu_Call call, ilu_Method method,
		      _ILU_C_ExceptionDescription evec,
		      ILU_C_ENVIRONMENT * status,
		      ilu_cardinal exceptionIndex,
		      ILU_ERRS((IoErrs)) * err)
{

  if ((exceptionIndex > method->me_exceptionCount) ||
      exceptionIndex == 0) {
    status->_major = ILU_C_SYSTEM_EXCEPTION;
    status->returnCode = ILU_C_STDEX(UNKNOWN);
    status->ptr = (void *) ilu_MallocE(sizeof(ilu_cardinal), err);
    if (ILU_ERRNOK(*err))
      return ilu_FALSE;
    *((ilu_cardinal *) (status->ptr)) = 0;
    status->freeRoutine = 0;
  } else {
    unsigned int    valsize;
    ilu_Class       valclass;
    ILU_C_InputFn   valfn;

    status->_major = ILU_C_USER_EXCEPTION;
    status->returnCode = ((ILU_C_ExceptionCode)
		   method->me_exceptionVector[exceptionIndex - 1]);
    valsize = evec[exceptionIndex - 1].size;
    if (valsize > 0) {
      status->ptr = (void *) ilu_MallocE(valsize, err);
      if (ILU_ERRNOK(*err))
	return ilu_FALSE;
      if (evec[exceptionIndex - 1].class != ILU_NIL) {
	valclass = evec[exceptionIndex - 1].class;
	(status->ptr) = (void *) _ILU_C_InputObject(call, valclass,
						    ilu_FALSE, err);
      } else {
	valfn = evec[exceptionIndex - 1].inFn;
	(void) (*valfn) (call, status->ptr, err);
      }
      if (ILU_ERRNOK(*err))
	return ilu_FALSE;
      status->freeRoutine = evec[exceptionIndex - 1].freeFn;
    }
  }
  return ilu_TRUE;
}

ilu_cardinal ILU_C_SizeOfBoolean (ilu_Call call, CORBA_boolean b, ilu_Error *err)
{
  return (ilu_SizeOfBoolean(call, (ilu_boolean) b, err));
}

void ILU_C_OutputBoolean (ilu_Call call, CORBA_boolean b, ilu_Error *err)
{
  ilu_OutputBoolean (call, (ilu_boolean) b, err);
}

void ILU_C_InputBoolean (ilu_Call call, CORBA_boolean *b, ilu_Error *err)
{
  ilu_boolean b2;

  if (ilu_InputBoolean (call, &b2, err), ILU_ERROK(*err))
    *b = (CORBA_boolean) b2;
}

static ilu_cardinal _ILU_C_WStrlen(ilu_wstring s)
{
  register int i;
  if (s == ILU_NIL)
    return 0;
  for (i = 0;  s[i] != 0;  i++)
    ;
  return (ilu_cardinal) i;
}

/*
  _ILU_C_GenericCall

  void _ILU_C_GenericCall (ilu_Class class, ilu_Method method,
                           _ILU_C_ExceptionDescription *evec,
                           ILU_C_Object *discriminant,
                           ILU_C_ENVIRONMENT *status,
                           char *argdesc, ...)

  Operates in varargs mode, off "argdesc".  "argdesc" provides a description
  of the arguments to the method, as follows:

  "argdesc" contains 2 fields, separated by ':'.

  The first field provides a list
  of the types of all the arguments which are either In or InOut arguments,
  each as a one-character code.  The codes are capital letters if the argument
  is an InOut argument, lower-case if an In argument.  Specific types are
  encoded as follows:

  a & A:  short integer
  b & B:  integer
  c & C:  long integer
  d & D:  short cardinal
  e & E:  cardinal
  f & F:  long cardinal
  g & G:  short real
  h & H:  real
  i & I:  long real
  j & J:  short character
  k & K:  character
  l & L:  long character
  m & m:  byte
  n & N:  boolean
  o & O:  enumeration
  p & P:  object
  q & Q:  C string
  r & R:  Unicode string
  y    :  Out variable-length type
  z & Z:  other

  s-x & S-Y:  reserved for future use

  Values of primitive ISL types are presented on the varargs
  list as appropriate for a value of this type, given the CORBA ANSI C calling
  conventions.  Values of the meta-type "other" are presented as void *
  pointers to the real values, followed by a function pointer to one of the
  three routines for sizing, output, and input of the constructed type,
  depending on its position in the varargs part of the arglist.

  The sequence of value specified by the first field of "argdesc" actually
  appears twice in the varargs part of the _ILU_C_GenericCall arglist, as
  there is no assurance that "va_start" can be called twice in the same
  function.

  The second field of "argdesc" contains a list 
  of the types of all the arguments which are either the return type of the
  method, or Out or InOut arguments to the method, again as one-character
  codes, using the same coding scheme, but this time with lower-case
  characters representing Out arguments.  For the purposes of this routine,
  the return value of the method is passed and coded as another Out argument.
  The return value argument appears before the actual Out and InOut arguments
  in the arglist.  The code 'y' is added to represent Out parameters of
  variable-length types.

  Example:

  INTERFACE FOO;
  TYPE R = RECORD a : INTEGER, b : REAL END;
  EXCEPTION E1;
  EXCEPTION E2 : R;
  TYPE O4 = OBJECT
    METHODS
     Real-R-to-Real-R (a1 : REAL, INOUT a2 : R): REAL RAISES E1, E2 END
    END;

  The marshalling code for FOO.R and the surrogate C stub for FOO.O4.R-to-R
  would look like:

void _FOO_Output_R (ilu_Call _call, FOO_R * _val, ilu_Error *err)
{
  if (_val == ILU_NIL)
    return;

  ilu_OutputRecord (_call, err);
  if (ILU_ERRNOK(*err)) return;

  ilu_OutputInteger (_call, (_val->a), err);
  if (ILU_ERRNOK(*err)) return;

  ilu_OutputReal (_call, (_val->b), err);
  if (ILU_ERRNOK(*err)) return;

  ilu_EndRecord (_call);
}

ilu_cardinal _FOO_SizeOf_R (ilu_Call _call, FOO_R * _val, ilu_Error *err)
{
  ilu_cardinal size = 0;
  ilu_cardinal s;

  size = ilu_SizeOfRecord (_call);

  s = ilu_SizeOfInteger(_call, (_val->a));
  if (s == 0) return 0;
  size += s;

  s = ilu_SizeOfReal(_call, (_val->b));
  if (s == 0) return 0;
  size += s;

  ilu_EndRecord (_call);
  return size;
}

FOO_R * _FOO_Input_R (ilu_Call _call, FOO_R * _ref, ilu_Error *err)
{
  FOO_R * _val;

  ilu_InputRecord (_call, err);
  if (ILU_ERRNOK(*err)) return _val;

  if (_ref != ILU_NIL)
    _val = _ref;
  else
    _val = (FOO_R *) ilu_must_malloc (sizeof (FOO_R));

  ilu_InputInteger (_call, &_val->a, err);
  if (ILU_ERRNOK(*err)) return _val;

  ilu_InputReal (_call, &_val->b, err);
  if (ILU_ERRNOK(*err)) return _val;

  ilu_EndRecord (_call);

  return (_val);
}

void FOO_R__Free (FOO_R * _val)
{
}

ilu_real _FOO_O4_Real_R_to_Real_R
  (FOO_O4 _handle, ILU_C_ENVIRONMENT *_status, ilu_real a1, FOO_R *a2)
{
  ilu_real _retvalue;
  static _ILU_C_ExceptionDescription _evec[] = {
    { 0, ILU_NIL },
    { sizeof(FOO_R), &_FOO_R__Input }};

  _ILU_C_GenericCall (&_FOO_O4__ILUClassRecord,
                      &_FOO_O4__ILUClassRecord.cl_methods[0],
		      _evec, _handle, _status, "hZ:hZ",
                      a1,
                      (void *) a2,
                      (void)(*)() _Foo_SizeOf_R,
                      a1,
                      (void *) a2,
                      (void)(*)() _Foo_Output_R,
                      &_retvalue,
                      (void *) a2,
                      (void)(*)() _Foo_Input_R,
                      );
  return _retvalue;
}

*/

void
  _ILU_C_GenericCall (ilu_Class class, ilu_Method method,
		      _ILU_C_ExceptionDescription evec,
		      ILU_C_Object *discriminant,
		      ILU_C_ENVIRONMENT *status,
		      char *argdesc, ...)
{
  ilu_Call_s      call_s;
  ilu_Call        call = &call_s;
  ilu_Error       err = ILU_INIT_NO_ERR;
  ilu_cardinal    size = 0, s;
  ilu_boolean     vaing = ilu_FALSE, initted = FALSE;
  ilu_boolean	needs_sizing;
  ilu_Object      kobj;
  va_list         ap;
  char           *p;
  ilu_cardinal    exceptionIndex;
  ILU_C_COMPLETIONSTATUS completed = CORBA_COMPLETED_NO;
  ilu_ProtocolException protocolErr;
  ILU_C_InterruptHandle intH = ILU_NIL;
  ilu_Connection  newconn;
  enum foo {a, b, c};

  ILU_CLER(err);

  (void) ilu_StartCall(call, discriminant->server, class, method,
		     MyLangIdx(), status->callerPassport,
		     &newconn, &err);
  if (newconn != ILU_NIL)
    (*Fork) (MonitorOutgoingConnection, newconn);
  if (ILU_ERRNOK(err))
    goto kerrOhneCall;
  intH = discriminant->interruptH;
  if (intH != ILU_NIL)
    if (!AddCallToIH(call, intH)) {
      ILU_C_RAISE_SYSTEM(status, NO_MEMORY, 0, NO);
      goto cerr;
    }

  needs_sizing = ilu_CallNeedsSizing(call);

  /* figure size of discriminant */
  if ((!needs_sizing) || _ILU_C_IsSingleton(class))
    size = 0;
  else {
    if ((kobj = _ILU_C_KernelObjOfObj(discriminant)) == ILU_NIL) {
      ILU_C_RAISE_SYSTEM(status, INV_OBJREF, 0, NO);
      goto cerr;
    }
    size = ilu_SizeOfObjectID(call, kobj, ilu_TRUE, class, &err);
    ilu_ExitServer(discriminant->server, ilu_ClassOfObject(kobj));
    if (ILU_ERRNOK(err))
      goto kerr;
  }
  
  va_start (ap, argdesc);
  vaing = ilu_TRUE;

  /* add in sizes of other args */

#define GENERIC_SIZEI(type,fn) { \
                                 type *v = va_arg(ap, type *); \
				 if (needs_sizing) {s = fn (call, *v, &err); \
				 if (ILU_ERRNOK(err)) goto kerr; \
				 else size += s; }\
			       } \
			       break
#define GENERIC_SIZES(type,fn) { \
                                 type v = (type) va_arg(ap, int); \
				 if (needs_sizing) { s = fn (call, v, &err); \
				 if (ILU_ERRNOK(err)) goto kerr; \
				 else size += s; }\
			       } \
			       break
#define GENERIC_SIZER(type,fn) { \
                                 type v = (type) va_arg(ap, double); \
				 if (needs_sizing) { s = fn (call, v, &err); \
				 if (ILU_ERRNOK(err)) goto kerr; \
				 else size += s; }\
			       } \
			       break
#define GENERIC_SIZE(type,fn) { \
                                 type v = va_arg(ap, type); \
				 if (needs_sizing) { s = fn (call, v, &err); \
				 if (ILU_ERRNOK(err)) goto kerr; \
				 else size += s; }\
			       } \
			       break


  for (p = argdesc;  p != ILU_NIL && *p != 0 && *p != ':';  p++)
    {
      switch (*p)
	{

	  /* Integers */

	case 'a':
	  GENERIC_SIZES(ilu_shortinteger, ilu_SizeOfShortInteger);
	case 'A':
	  GENERIC_SIZEI(ilu_shortinteger, ilu_SizeOfShortInteger);
	case 'b':
	  GENERIC_SIZE(ilu_integer, ilu_SizeOfInteger);
	case 'B':
	  GENERIC_SIZEI(ilu_integer, ilu_SizeOfInteger);
	case 'c':
	  GENERIC_SIZE(ilu_longinteger, ilu_SizeOfLongInteger);
	case 'C':
	  GENERIC_SIZEI(ilu_longinteger, ilu_SizeOfLongInteger);

	  /* Cardinals */

	case 'd':
	  GENERIC_SIZES(ilu_shortcardinal, ilu_SizeOfShortCardinal);
	case 'D':
	  GENERIC_SIZEI(ilu_shortcardinal, ilu_SizeOfShortCardinal);
	case 'e':
	  GENERIC_SIZE(ilu_cardinal, ilu_SizeOfCardinal);
	case 'E':
	  GENERIC_SIZEI(ilu_cardinal, ilu_SizeOfCardinal);
	case 'f':
	  GENERIC_SIZE(ilu_longcardinal, ilu_SizeOfLongCardinal);
	case 'F':
	  GENERIC_SIZEI(ilu_longcardinal, ilu_SizeOfLongCardinal);

	  /* Reals */

	case 'g':
	  GENERIC_SIZER(ilu_shortreal, ilu_SizeOfShortReal);
	case 'G':
	  GENERIC_SIZEI(ilu_shortreal, ilu_SizeOfShortReal);
	case 'h':
	  GENERIC_SIZE(ilu_real, ilu_SizeOfReal);
	case 'H':
	  GENERIC_SIZEI(ilu_real, ilu_SizeOfReal);
	case 'i':
	  GENERIC_SIZE(ilu_longreal, ilu_SizeOfLongReal);
	case 'I':
	  GENERIC_SIZEI(ilu_longreal, ilu_SizeOfLongReal);

	  /* Characters */

	case 'j':
	  GENERIC_SIZES(ilu_shortcharacter, ilu_SizeOfShortCharacter);
	case 'J':
	  GENERIC_SIZEI(ilu_shortcharacter, ilu_SizeOfShortCharacter);
	case 'k':
	  GENERIC_SIZES(ilu_character, ilu_SizeOfCharacter);
	case 'K':
	  GENERIC_SIZEI(ilu_character, ilu_SizeOfCharacter);
/*
	case 'l':
	  GENERIC_SIZE(ilu_longcharacter, ilu_SizeOfLongCharacter);
	case 'L':
	  GENERIC_SIZEI(ilu_longcharacter, ilu_SizeOfLongCharacter);
*/
	  /* Byte */

	case 'm':
	  GENERIC_SIZES(ilu_byte, ilu_SizeOfByte);
	case 'M':
	  GENERIC_SIZEI(ilu_byte, ilu_SizeOfByte);


	  /* Boolean */

	case 'n':
	  GENERIC_SIZES(CORBA_boolean, ILU_C_SizeOfBoolean);
	case 'N':
	  GENERIC_SIZEI(CORBA_boolean, ILU_C_SizeOfBoolean);

	  /* Enumeration */
	case 'o':
	  {
	    enum foo v;
	    v = va_arg(ap, enum foo);
	    if (needs_sizing) {
	      size += ilu_SizeOfEnum (call, (ilu_shortcardinal) v, &err);
	      if (ILU_ERRNOK(err)) goto kerr; };
	    break;
	  }
	case 'O':
	  {
	    enum foo *v;
	    v = va_arg(ap, enum foo *);
	    if (needs_sizing) {
	      size += ilu_SizeOfEnum (call, (ilu_shortcardinal) (*v), &err);
	      if (ILU_ERRNOK(err)) goto kerr; };
	    break;
	  }
	  /* Objects */

	case 'p':
	  {
	    ILU_C_Object *v;
	    ilu_Class pclass;

	    v = va_arg(ap, ILU_C_Object *);
	    pclass = va_arg(ap, ilu_Class);
	    if (needs_sizing) {
	      s = _ILU_C_SizeOfObject (call, v, pclass, ilu_FALSE, &err);
	      if (ILU_ERRNOK(err)) goto kerr;
	      size += s; };
	    break;
	  }
	case 'P':
	  {
	    ILU_C_Object **v;
	    ilu_Class pclass;

	    v = va_arg(ap, ILU_C_Object **);
	    pclass = va_arg(ap, ilu_Class);
	    if (needs_sizing) {
	      s = _ILU_C_SizeOfObject (call, *v, pclass, ilu_FALSE, &err);
	      if (ILU_ERRNOK(err)) goto kerr;
	      size += s;
	    };
	    break;
	  }

	  /* C String */

	case 'q':
	  {
	    ilu_string str;
	    ilu_cardinal limit;

	    str = va_arg(ap, ilu_string);
	    limit = va_arg(ap, ilu_cardinal);
	    if (needs_sizing) {
	      s = ilu_SizeOfString (call, str, strlen(str), limit, &err);
	      if (ILU_ERRNOK(err)) goto kerr;
	      size += s;
	    };
	    break;
	  }
	case 'Q':
	  {
	    ilu_string *str;
	    ilu_cardinal limit;

	    str = va_arg(ap, ilu_string *);
	    limit = va_arg(ap, ilu_cardinal);
	    if (needs_sizing) {
	      s = ilu_SizeOfString (call, *str, strlen(*str), limit, &err);
	      if (ILU_ERRNOK(err)) goto kerr;
	      size += s;
	    };
	    break;
	  }

	  /* Unicode String */

	case 'r':
	  {
	    ilu_wstring str;
	    ilu_cardinal limit;

	    str = va_arg(ap, ilu_wstring);
	    limit = va_arg(ap, ilu_cardinal);
	    if (needs_sizing) {
	      s = ilu_SizeOfWString (call, str, _ILU_C_WStrlen(str), limit, &err);
	      if (ILU_ERRNOK(err)) goto kerr;
	      size += s;
	    };
	    break;
	  }
	case 'R':
	  {
	    ilu_wstring *str;
	    ilu_cardinal limit;

	    str = va_arg(ap, ilu_wstring *);
	    limit = va_arg(ap, ilu_cardinal);
	    if (needs_sizing) {
	      s = ilu_SizeOfWString (call, *str, _ILU_C_WStrlen(*str), limit, &err);
	      if (ILU_ERRNOK(err)) goto kerr;
	      size += s;
	    }
	    break;
	  }

	case 'z':
	case 'Z':
	  {
	    void           *v;
	    ILU_C_SizeFn    fp;

	    v = va_arg(ap, void *);
	    fp = va_arg(ap, ILU_C_SizeFn);
	    if (needs_sizing) {
	      s = (*fp) (call, v, &err);
	      if (ILU_ERRNOK(err)) goto kerr; else size += s;
	    };
	    break;
	  }

	default:
	  ILU_C_RAISE_SYSTEM(status, INTERNAL, 0, NO);
	  goto cerr;
	}
    }

  if (!ilu_StartRequest(call, size, &err))
    goto kerr;

  /* marshall discriminant */

  if (!_ILU_C_IsSingleton(class))
    {
      if ((kobj = _ILU_C_KernelObjOfObj (discriminant)) == ILU_NIL)
	{
	  ILU_C_RAISE_SYSTEM(status, INV_OBJREF, 0, NO);
	  goto cerr;
	}
      ilu_OutputObjectID(call, kobj, ilu_TRUE, class, &err);
      if (ILU_ERRNOK(err))
	goto kerr;
    }
  
  /* marshall other arguments */

#define GENERIC_OUTPUT(type,fn) { \
                                  type v = va_arg(ap, type); \
				  fn (call, v, &err); \
				  if (ILU_ERRNOK(err)) goto kerr; \
				} \
				break

#define GENERIC_OUTPUTS(type,fn) { \
                                  type v = va_arg(ap, int); \
				  fn (call, v, &err); \
				  if (ILU_ERRNOK(err)) goto kerr; \
				} \
				break

#define GENERIC_OUTPUTR(type,fn) { \
                                  type v = va_arg(ap, double); \
				  fn (call, v, &err); \
				  if (ILU_ERRNOK(err)) goto kerr; \
				} \
				break

#define GENERIC_OUTPUTI(type,fn) { \
                                   type *v = va_arg(ap, type *); \
				   fn (call, *v, &err); \
				   if (ILU_ERRNOK(err)) goto kerr; \
				 } \
				 break

  for (p = argdesc;  p != ILU_NIL && *p != 0 && *p != ':';  p++)
    {
      switch (*p)
	{

	  /* Integers */

	case 'a':
	  GENERIC_OUTPUTS(ilu_shortinteger, ilu_OutputShortInteger);
	case 'A':
	  GENERIC_OUTPUTI(ilu_shortinteger, ilu_OutputShortInteger);
	case 'b':
	  GENERIC_OUTPUT(ilu_integer, ilu_OutputInteger);
	case 'B':
	  GENERIC_OUTPUTI(ilu_integer, ilu_OutputInteger);
	case 'c':
	  GENERIC_OUTPUT(ilu_longinteger, ilu_OutputLongInteger);
	case 'C':
	  GENERIC_OUTPUTI(ilu_longinteger, ilu_OutputLongInteger);

	  /* Cardinals */

	case 'd':
	  GENERIC_OUTPUTS(ilu_shortcardinal, ilu_OutputShortCardinal);
	case 'D':
	  GENERIC_OUTPUTI(ilu_shortcardinal, ilu_OutputShortCardinal);
	case 'e':
	  GENERIC_OUTPUT(ilu_cardinal, ilu_OutputCardinal);
	case 'E':
	  GENERIC_OUTPUTI(ilu_cardinal, ilu_OutputCardinal);
	case 'f':
	  GENERIC_OUTPUT(ilu_longcardinal, ilu_OutputLongCardinal);
	case 'F':
	  GENERIC_OUTPUTI(ilu_longcardinal, ilu_OutputLongCardinal);

	  /* Reals */

	case 'g':
	  GENERIC_OUTPUTR(ilu_shortreal, ilu_OutputShortReal);
	case 'G':
	  GENERIC_OUTPUTI(ilu_shortreal, ilu_OutputShortReal);
	case 'h':
	  GENERIC_OUTPUT(ilu_real, ilu_OutputReal);
	case 'H':
	  GENERIC_OUTPUTI(ilu_real, ilu_OutputReal);
	case 'i':
	  GENERIC_OUTPUT(ilu_longreal, ilu_OutputLongReal);
	case 'I':
	  GENERIC_OUTPUTI(ilu_longreal, ilu_OutputLongReal);

	  /* Characters */

	case 'j':
	  GENERIC_OUTPUTS(ilu_shortcharacter, ilu_OutputShortCharacter);
	case 'J':
	  GENERIC_OUTPUTI(ilu_shortcharacter, ilu_OutputShortCharacter);
	case 'k':
	  GENERIC_OUTPUTS(ilu_character, ilu_OutputCharacter);
	case 'K':
	  GENERIC_OUTPUTI(ilu_character, ilu_OutputCharacter);
/*
	case 'l':
	  GENERIC_OUTPUT(ilu_longcharacter, ilu_OutputLongCharacter);
	case 'L':
	  GENERIC_OUTPUTI(ilu_longcharacter, ilu_OutputLongCharacter);
*/

	  /* Byte */

	case 'm':
	  GENERIC_OUTPUTS(ilu_byte, ilu_OutputByte);
	case 'M':
	  GENERIC_OUTPUTI(ilu_byte, ilu_OutputByte);


	  /* Boolean */

	case 'n':
	  GENERIC_OUTPUTS(CORBA_boolean, ILU_C_OutputBoolean);
	case 'N':
	  GENERIC_OUTPUTI(CORBA_boolean, ILU_C_OutputBoolean);

	  /* Enumeration */

	case 'o':
	  {
	    enum foo v;
	    v = va_arg(ap, enum foo);
	    ilu_OutputEnum (call, (ilu_shortcardinal) v, &err);
	    if (ILU_ERRNOK(err)) goto kerr;
	    break;
	  }
	case 'O':
	  {
	    enum foo *v;
	    v = va_arg(ap, enum foo *);
	    ilu_OutputEnum (call, (ilu_shortcardinal) (*v), &err);
	    if (ILU_ERRNOK(err)) goto kerr;
	    break;
	  }

	case 'p':
	  {
	    ILU_C_Object *v;
	    ilu_Class pclass;

	    v = va_arg(ap, ILU_C_Object *);
	    pclass = va_arg(ap, ilu_Class);
	    _ILU_C_OutputObject (call, v, pclass, ilu_FALSE, &err);
	    if (ILU_ERRNOK(err)) goto kerr;
	    break;
	  }
	case 'P':
	  {
	    ILU_C_Object **v;
	    ilu_Class pclass;

	    v = va_arg(ap, ILU_C_Object **);
	    pclass = va_arg(ap, ilu_Class);
	    _ILU_C_OutputObject (call, *v, pclass, ilu_FALSE, &err);
	    if (ILU_ERRNOK(err)) goto kerr;
	    break;
	  }

	  /* C String */

	case 'q':
	  {
	    ilu_string str;
	    ilu_cardinal limit;

	    str = va_arg(ap, ilu_string);
	    limit = va_arg(ap, ilu_cardinal);
	    ilu_OutputString (call, str, strlen(str), limit, &err);
	    if (ILU_ERRNOK(err)) goto kerr;
	    break;
	  }
	case 'Q':
	  {
	    ilu_string *str;
	    ilu_cardinal limit;

	    str = va_arg(ap, ilu_string *);
	    limit = va_arg(ap, ilu_cardinal);
	    ilu_OutputString (call, *str, strlen(*str), limit, &err);
	    if (ILU_ERRNOK(err)) goto kerr;
	    break;
	  }

	  /* Unicode String */

	case 'r':
	  {
	    ilu_wstring str;
	    ilu_cardinal limit;

	    str = va_arg(ap, ilu_wstring);
	    limit = va_arg(ap, ilu_cardinal);
	    ilu_OutputWString (call, str, _ILU_C_WStrlen(str), limit, &err);
	    if (ILU_ERRNOK(err)) goto kerr;
	    break;
	  }
	case 'R':
	  {
	    ilu_wstring *str;
	    ilu_cardinal limit;

	    str = va_arg(ap, ilu_wstring *);
	    limit = va_arg(ap, ilu_cardinal);
	    ilu_OutputWString (call, *str, _ILU_C_WStrlen(*str), limit, &err);
	    if (ILU_ERRNOK(err)) goto kerr;
	    break;
	  }

	case 'z':
	case 'Z':
	  {
	    void           *v;
	    ILU_C_OutputFn  fp;

	    v = va_arg(ap, void *);
	    fp = va_arg(ap, ILU_C_OutputFn);

	    (*fp) (call, v, &err);
	    if (ILU_ERRNOK(err)) goto kerr;
	    break;
	  }

	default:
	  ILU_C_RAISE_SYSTEM(status, INTERNAL, 0, NO);
	  goto cerr;
	}
    }

  /* finish the request */

  completed = CORBA_COMPLETED_MAYBE;
  if (!ilu_FinishRequest (call, &err)) goto kerr;

  /* is there a reply? */

  if (! method->me_asynchronous)
    {
      /* yes */

      protocolErr = ilu_GetReply (call, &exceptionIndex, &err);
      if (ILU_ERRNOK(err)) goto kerr;

      if (protocolErr == ilu_ProtocolException_Success)
	{
	  completed = CORBA_COMPLETED_YES;
	  /* check to see if the user signalled Success (exceptionIndex == 0) */
	  if (exceptionIndex == 0)
	    {
	      status->returnCode = ILU_NIL;
	      status->_major = ILU_C_NO_EXCEPTION;

	      /* read in any return results */

#define GENERIC_INPUT(type,fn) { \
                                 type * v = va_arg(ap, type *); \
                                 fn (call, v, &err); \
				 if (ILU_ERRNOK(err)) goto kerr; \
			       } \
			       break

	      /* we left "p" pointing at the separating colon char */
	      for (++p;  p != ILU_NIL && *p != 0 && *p != ':';  p++)
		{
		  switch (*p)
		    {

		      /* Integers */

		    case 'a':
		    case 'A':
		      GENERIC_INPUT(ilu_shortinteger, ilu_InputShortInteger);
		    case 'b':
		    case 'B':
		      GENERIC_INPUT(ilu_integer, ilu_InputInteger);
		    case 'c':
		    case 'C':
		      GENERIC_INPUT(ilu_longinteger, ilu_InputLongInteger);

		      /* Cardinals */

		    case 'd':
		    case 'D':
		      GENERIC_INPUT(ilu_shortcardinal, ilu_InputShortCardinal);
		    case 'e':
		    case 'E':
		      GENERIC_INPUT(ilu_cardinal, ilu_InputCardinal);
		    case 'f':
		    case 'F':
		      GENERIC_INPUT(ilu_longcardinal, ilu_InputLongCardinal);

		      /* Reals */

		    case 'g':
		    case 'G':
		      GENERIC_INPUT(ilu_shortreal, ilu_InputShortReal);
		    case 'h':
		    case 'H':
		      GENERIC_INPUT(ilu_real, ilu_InputReal);
		    case 'i':
		    case 'I':
		      GENERIC_INPUT(ilu_longreal, ilu_InputLongReal);

		      /* Characters */

		    case 'j':
		    case 'J':
		      GENERIC_INPUT(ilu_shortcharacter, ilu_InputShortCharacter);
		    case 'k':
		    case 'K':
		      GENERIC_INPUT(ilu_character, ilu_InputCharacter);
/*
		    case 'l':
		    case 'L':
		      GENERIC_INPUT(ilu_longcharacter, ilu_InputLongCharacter);
*/

		      /* Byte */

		    case 'm':
		    case 'M':
		      GENERIC_INPUT(ilu_byte, ilu_InputByte);


		      /* Boolean */

		    case 'n':
		    case 'N':
		      GENERIC_INPUT(CORBA_boolean, ILU_C_InputBoolean);

		      /* Enum */

		    case 'o':
		    case 'O':
		      {
			ilu_shortcardinal val;
			enum foo *v;
			v = (enum foo *) va_arg(ap, enum foo *);
			ilu_InputEnum (call, &val, &err);
			if (ILU_ERRNOK(err)) goto kerr;
			*v = (enum foo) val;
			break;
		      }

		      /* Object */

		    case 'p':
		    case 'P':
		      {
			ILU_C_Object **v;
			ilu_Class pclass;

			v = va_arg(ap, ILU_C_Object **);
			pclass = va_arg(ap, ilu_Class);
			*v = _ILU_C_InputObject (call, pclass, ilu_FALSE, &err);
			if (ILU_ERRNOK(err)) goto kerr;
			break;
		      }

		      /* C String */

		    case 'q':
		      {
			ilu_string *str;
			ilu_cardinal limit;
			ilu_cardinal len;

			str = va_arg(ap, ilu_string *);
			limit = va_arg(ap, ilu_cardinal);
			*str = ILU_NIL;
			ilu_InputString (call, str, &len, limit, &err);
			if (ILU_ERRNOK(err)) goto kerr;
			break;
		      }
		    case 'Q':
		      {
			ilu_string *str;
			ilu_cardinal limit;
			ilu_cardinal len;

			str = va_arg(ap, ilu_string *);
			limit = va_arg(ap, ilu_cardinal);
			ilu_free(*str);
			*str = ILU_NIL;
			ilu_InputString (call, str, &len, limit, &err);
			if (ILU_ERRNOK(err)) goto kerr;
			size += s;
			break;
		      }

		      /* Unicode String */

		    case 'r':
		      {
			ilu_wstring *str;
			ilu_cardinal limit;
			ilu_cardinal len;

			str = va_arg(ap, ilu_wstring *);
			limit = va_arg(ap, ilu_cardinal);
			*str = ILU_NIL;
			ilu_InputWString (call, str, &len, limit, &err);
			if (ILU_ERRNOK(err)) goto kerr;
			break;
		      }
		    case 'R':
		      {
			ilu_wstring *str;
			ilu_cardinal limit;
			ilu_cardinal len;

			str = va_arg(ap, ilu_wstring *);
			limit = va_arg(ap, ilu_cardinal);
			ilu_free(*str);
			*str = ILU_NIL;
			ilu_InputWString (call, str, &len, limit, &err);
			if (ILU_ERRNOK(err)) goto kerr;
			size += s;
			break;
		      }

		      /* other constructed type */

		    case 'y':
		      {
			void ** v;
			ILU_C_InputFn fp;

			v = va_arg(ap, void **);
			fp = va_arg(ap, ILU_C_InputFn);

			*v = (*fp) (call, ILU_NIL, &err);
			if (ILU_ERRNOK(err)) goto kerr;
			break;
		      }

		    case 'z':
		    case 'Z':
		      {
			void * v;
			ILU_C_InputFn fp;

			v = va_arg(ap, void *);
			fp = va_arg(ap, ILU_C_InputFn);

			(void) (*fp) (call, v, &err);
			if (ILU_ERRNOK(err)) goto kerr;
			break;
		      }

		    case '*':
		      break;

		    default:
		      ILU_C_RAISE_SYSTEM(status, INTERNAL, 0, MAYBE);
		      goto cerr;
		    }
		}
	    }
	  else
	    /* indicates user signalled an expected exception */
	    {
	      if (!_ILU_C_CatchException(call, method, evec, status,
					 exceptionIndex, &err))
		goto kerr;
	    }
	  if (!ilu_ReplyRead (call, &err))
	    goto kerr;
	}
      else
	{
	  _ILU_C_SetProtocolError (status, protocolErr);
	  goto cerr;
	}
    }
  else
    {
      /* no return values or exceptions allowed, just return */
    }

  va_end(ap);
  vaing = ilu_FALSE;

kerr:		/* used for errors signalled via "err" */

  ilu_FinishCall (call, &err);
  if (ILU_ERROK(err))
    goto the_end;
  goto kerrOhneCall;

cerr:	/* used for errors signalled via "status" */

  ilu_FinishCall(call, &err);
  goto abend2;

 kerrOhneCall:

  _ILU_C_ConvertError (status, &err, completed);

 abend2:

  if (vaing)
    va_end(ap);
  
 the_end:
  
  if (initted && intH != ILU_NIL)
    RemCallFromIH(call, intH);
  
  return;
}

void
_ILU_C_SendException(ilu_Call call, _ILU_C_ExceptionDescription evec,
		     ILU_C_ENVIRONMENT * status, ilu_Error * err)
{
  ilu_cardinal argSize = 0;
  ilu_cardinal eindex = 0, limit;
  ilu_Method method = ilu_MethodOfCall(call);

  ILU_CLER(*err);

  if (status->returnCode == ILU_NIL)
    return;

  limit = method->me_exceptionCount;
  for (eindex = 0; eindex < limit;  eindex ++)
    if (method->me_exceptionVector[eindex] == status->returnCode) 
      goto found;
  ilu_DebugPrintf("ilu.c: %s raises unexpected exception %p!\n",
		  ilu_NameOfMethod(method), status->returnCode);
  (void) ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_bogus_raise, 6);
  return;
  
found:

#define GENERIC_ESIZE(type,fn) argSize = fn (call, *((type *) status->ptr), err); break

  if (ilu_CallNeedsSizing(call)) {
    argSize = ilu_BeginSizingException(call, eindex+1, err);
    if (ILU_ERRNOK(*err))
      return;
  
    switch (evec[eindex].size)
      {
	/* Integers */

      case 'a':
	GENERIC_ESIZE(ilu_shortinteger, ilu_SizeOfShortInteger);
      case 'b':
	GENERIC_ESIZE(ilu_integer, ilu_SizeOfInteger);
      case 'c':
	GENERIC_ESIZE(ilu_longinteger, ilu_SizeOfLongInteger);

	/* Cardinals */

      case 'd':
	GENERIC_ESIZE(ilu_shortcardinal, ilu_SizeOfShortCardinal);
      case 'e':
	GENERIC_ESIZE(ilu_cardinal, ilu_SizeOfCardinal);
      case 'f':
	GENERIC_ESIZE(ilu_longcardinal, ilu_SizeOfLongCardinal);

	/* Reals */

      case 'g':
	GENERIC_ESIZE(ilu_shortreal, ilu_SizeOfShortReal);
      case 'h':
	GENERIC_ESIZE(ilu_real, ilu_SizeOfReal);
      case 'i':
	GENERIC_ESIZE(ilu_longreal, ilu_SizeOfLongReal);

	/* Characters */

      case 'j':
	GENERIC_ESIZE(ilu_shortcharacter, ilu_SizeOfShortCharacter);
      case 'k':
	GENERIC_ESIZE(ilu_character, ilu_SizeOfCharacter);
	/*
	  case 'l':
	  GENERIC_ESIZE(ilu_longcharacter, ilu_SizeOfLongCharacter);
	  */
	/* Byte */

      case 'm':
	GENERIC_ESIZE(ilu_byte, ilu_SizeOfByte);

	/* Boolean */

      case 'n':
	GENERIC_ESIZE(CORBA_boolean, ILU_C_SizeOfBoolean);

	/* Enumeration */
      case 'o':
	GENERIC_ESIZE(ilu_shortcardinal, ilu_SizeOfEnum);

	/* Objects */

      case 'p':
	argSize = _ILU_C_SizeOfObject (call, (ILU_C_Object *) status->ptr, evec[eindex].class, ilu_FALSE, err);
	break;

	/* C String */
      case 'q':
	argSize = ilu_SizeOfString (call, *((ilu_string *)(status->ptr)),
				    strlen(*((ilu_string *)(status->ptr))),
				    0xFFFF, err);

      case 'z':
	argSize = (*evec[eindex].sizeFn)(call, status->ptr, err);
	break;

      default:
	argSize = 0;
      }
    if (ILU_ERRNOK(*err)) return;
  }

  _ILU_C_BeginException (call, eindex+1, argSize, err);
  if (ILU_ERRNOK(*err)) return;

#define GENERIC_EOUT(type,fn) fn (call, *((type *) status->ptr), err); break

  switch (evec[eindex].size)
    {
      /* Integers */

    case 'a':
      GENERIC_EOUT(ilu_shortinteger, ilu_OutputShortInteger);
    case 'b':
      GENERIC_EOUT(ilu_integer, ilu_OutputInteger);
    case 'c':
      GENERIC_EOUT(ilu_longinteger, ilu_OutputLongInteger);

      /* Cardinals */

    case 'd':
      GENERIC_EOUT(ilu_shortcardinal, ilu_OutputShortCardinal);
    case 'e':
      GENERIC_EOUT(ilu_cardinal, ilu_OutputCardinal);
    case 'f':
      GENERIC_EOUT(ilu_longcardinal, ilu_OutputLongCardinal);

      /* Reals */

    case 'g':
      GENERIC_EOUT(ilu_shortreal, ilu_OutputShortReal);
    case 'h':
      GENERIC_EOUT(ilu_real, ilu_OutputReal);
    case 'i':
      GENERIC_EOUT(ilu_longreal, ilu_OutputLongReal);

      /* Characters */

    case 'j':
      GENERIC_EOUT(ilu_shortcharacter, ilu_OutputShortCharacter);
    case 'k':
      GENERIC_EOUT(ilu_character, ilu_OutputCharacter);
      /*
	case 'l':
	GENERIC_EOUT(ilu_longcharacter, ilu_OutputLongCharacter);
	*/
      /* Byte */

    case 'm':
      GENERIC_EOUT(ilu_byte, ilu_OutputByte);

      /* Boolean */

    case 'n':
      GENERIC_EOUT(CORBA_boolean, ILU_C_OutputBoolean);

      /* Enumeration */
    case 'o':
      GENERIC_EOUT(ilu_shortcardinal, ilu_OutputEnum);

      /* Objects */

    case 'p':
      _ILU_C_OutputObject (call, (ILU_C_Object *) status->ptr, evec[eindex].class, ilu_FALSE, err);
      break;

      /* C string */
    case 'q':
      ilu_OutputString (call, *((ilu_string *) (status->ptr)),
			strlen(*((ilu_string *) (status->ptr))),
			0xFFFF, err);

    case 'z':
      (*evec[eindex].outFn)(call, status->ptr, err);
      break;

    default:
      break;
    }
  if (ILU_ERRNOK(*err)) return;

  _ILU_C_FinishException (call, err);
  if (ILU_ERRNOK(*err)) return;

  return;
}

ilu_cardinal _ilu_CString__SizeOf (ilu_Call call, ilu_string str, ilu_Error *err)
{
  return (ilu_SizeOfString(call, str, _ILU_C_SafeStrlen(str), 0, err));
}

void _ilu_CString__Output (ilu_Call call, ilu_string str, ilu_Error *err)
{
  ilu_OutputString(call, str, _ILU_C_SafeStrlen(str), 0, err);
}

ilu_CString _ilu_CString__Input (ilu_Call call, ilu_CString *strp, ilu_Error *err)
{
  ilu_cardinal len = 0;
  ilu_string p = ILU_NIL;

  ilu_InputString(call, &p, &len, 0, err);
  if (ILU_ERROK(*err))
    {
      if (strp != ILU_NIL)
	*strp = p;
      return (p);
    }
  else
    return (ILU_NIL);
}

/*============================================================*/
/*============================================================*/
/*                 Sequence operations                        */
/*============================================================*/
/*============================================================*/

#define Alloc( n, t )   ( t * ) ilu_must_malloc( n * sizeof( t ))
#define Realloc( p, n, t ) ( t * ) ilu_realloc( p, n * sizeof( t ))

void _ILU_C_AppendGeneric (ILU_C_Sequence h, char *p, int sz)
{
  char *ptr;

  /*
   ** place the item pointed to by p
   ** at the end of the sequence
   */

  if (h->_length >= h->_maximum)
    {
      h->_maximum = (h->_maximum + SEQUENCE_INCREMENT);
      if (h->_buffer != ILU_NIL)
	h->_buffer = ilu_realloc(h->_buffer, h->_maximum * sz);
      else
	h->_buffer = ilu_must_malloc(h->_maximum*sz);
    }
  ptr = h->_buffer + (h->_length * sz);
  memcpy (ptr, p, sz);
  h->_length += 1;
}

void _ILU_C_EveryElement(ILU_C_Sequence h, void (*proc)(void *,void *), int sz, void *data)
{
  int i;
  char *p;

  if (!h || h->_length <= 0)
    return;
  for(p = h->_buffer, i = 0; ((unsigned)i) < h->_length; i++, p += sz)
    (*proc)((void *) p, data);
}

void _ILU_C_PopGeneric (ILU_C_Sequence h, char *p, int sz)
{
    char	*ptr;

    /*
    ** return the top element
    ** in the sequence in p then
    ** remove it from the list.
    */

    if ( !h || h->_length <= 0 )
	return;
    memcpy( p, h->_buffer, sz );
    h->_length--;
    ptr = h->_buffer + sz;
    memmove (h->_buffer, ptr, h->_length * sz);
}

void _ILU_C_PushGeneric (ILU_C_Sequence h, char *p, int sz)
{
    int		l = h->_length;
    int		n;
    char	*ptr;

    /*
    ** place the item pointed to by p
    ** at the beginning of the sequence
    */

    h->_length++;
    n = h->_length * sz;
    if ( h->_length > h->_maximum ) {
        if ( h->_buffer )
    	    h->_buffer = Realloc( h->_buffer, n, char );
        else
    	    h->_buffer = Alloc( n, char );
	h->_maximum = h->_length;
    }
    ptr = h->_buffer + sz;

    memmove (ptr, h->_buffer, l * sz);
    memcpy (h->_buffer, p, sz);
}


/* L1, L2, Main unconstrained (rely on correct calls) */

static _ILU_C_MethodBlock GcCallbackMethods = { ILU_NIL, { 0 } };

static _ILU_C_MethodBlock *GcCallback__TrueTypeVector[] = {
	&GcCallbackMethods,
	ILU_NIL
};

static int CallbackInited = 0;
static ilu_Server cbs = ILU_NIL;
static ILU_C_OBJECT cbo = ILU_NIL;

/*Main invariant holds*/
void _ILU_C_EnsureGcClient(void)
{
  if (CallbackInited)
    return;
  GcCallbackMethods.c = ilu_GetGcCallbackClass();
  cbs = ILU_C_InitializeServer(ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL, TRUE);
  cbo = _ILU_C_CreateTrueObject(GcCallback__TrueTypeVector,
				"the-gc-callback", cbs, ILU_NIL, ilu_FALSE);
  if (cbo != ILU_NIL) {
    ilu_EnterServer(cbs, MOST_SPECIFIC_ILU_CLASS(cbo));
    ilu_SetGcClient(cbo->instanceId);
    ilu_ExitServer(cbs, MOST_SPECIFIC_ILU_CLASS(cbo));
  }
  CallbackInited = 1;
  return;
}
