/*
Copyright (c) 1991, 1992, 1993 Xerox Corporation.  All Rights Reserved.  

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
/* $Id: iluobj.cpp,v 1.50 1996/07/11 18:02:31 spreitze Exp $ */
/* Last tweaked by Mike Spreitzer July 11, 1996 11:02 am PDT */

#include "ilu.hh"

extern "C" {
#include <stdio.h>

#if ((defined(WIN32) || defined(WIN16)) && defined(_WINIO))
#include <winiodef.h>
#endif /* ((defined(WIN32) || defined(WIN16)) && defined(_WINIO)) */

extern char *_ilu_Strdup(const char *);
}

iluObject::iluObject()
{
  this->ILURPCObject = NULL;
  this->ILURPCServer = NULL;
  this->ILUPublishProof = NULL;
  this->ILUInstanceHandle = NULL;
  this->ILUMostSpecificObj = NULL;
}

iluObject::iluObject(char *instance_handle)
{
  this->ILURPCObject = NULL;
  this->ILURPCServer = NULL;
  this->ILUPublishProof = NULL;
  this->ILUInstanceHandle = instance_handle;
  this->ILUMostSpecificObj = NULL;
}

iluObject::iluObject(ilu_Server s)
{
  this->ILURPCObject = NULL;
  this->ILURPCServer = s;
  this->ILUPublishProof = NULL;
  this->ILUInstanceHandle = NULL;
  this->ILUMostSpecificObj = NULL;
}

iluObject::iluObject(ilu_Server s, char *instance_handle)
{
  this->ILURPCObject = NULL;
  this->ILURPCServer = s;
  this->ILUPublishProof = NULL;
  this->ILUInstanceHandle = instance_handle;
  this->ILUMostSpecificObj = NULL;
}

iluObject::~iluObject()
{
  if (this->ILUPublishProof != NULL)
    this->ILUWithdraw();
  if (this->ILURPCServer != NULL)
    {
      ilu::EnterServer(this->ILURPCServer, this->ILUInstanceClassRecord);
      if (this->ILURPCObject != NULL)
        {
          ilu::SetLanguageSpecificObject (this->ILURPCObject, NULL);
          this->ILURPCObject = NULL;
        }
      ilu::ExitServer(this->ILURPCServer, this->ILUInstanceClassRecord);
    }
  if (this->ILUInstanceHandle != NULL)
    free(this->ILUInstanceHandle);
}

struct registry_record {
  ilu_Class c;
  class iluObject * (*proc)(ilu_KernelObject);
  struct registry_record *next;
};

static struct registry_record *RegisteredClasses = NULL;

static struct registry_record * findRegistryRecordByClass (ilu_Class c)
{
  struct registry_record *p;

  for (p = RegisteredClasses;  p != NULL;  p = p->next)
    if (p->c == c)
      return (p);
  return (NULL);
}

void iluObject::RegisterSurrogateCreator (ilu_Class c, class iluObject * (*proc)(ilu_KernelObject))
{
  struct registry_record *p;

  if ((p = findRegistryRecordByClass(c)) == NULL)
    {
      p = (struct registry_record *) malloc(sizeof(struct registry_record));
      p->c = c;
      p->proc = proc;
      p->next = RegisteredClasses;
      RegisteredClasses = p;
    }
}

/* Inside(obj->server, c) */
class iluObject * iluObject::CreateFromRegistry (ilu_Class c, ilu_KernelObject obj)
{
  class iluObject *lspo;
  struct registry_record *p;

  if ((p = findRegistryRecordByClass(c)) == NULL)
    return (NULL);
  else
    {
      lspo = (*(p->proc))(obj);
      return(lspo);
    }
}

void *iluObject::ILUCastDown (ilu_Class cast_to)
{
  if (cast_to == NULL)
    return ((void *) this);
  else	/* don't know how to cast to anything in this method */
    return (NULL);
}

ilu_Server iluObject::ILUGetKernelServer (void)
{
  return (this->ILURPCServer);
}

ilu_Server iluObject::ILUEnsureKernelServer ()
{
  if (this->ILURPCServer == NULL)
    {
      iluServer *server = this->ILUGetServer();

      if (server != NULL)
        this->ILURPCServer = server->KernelServer();
    }
  return this->ILURPCServer;
}

/* after: return obj != 0 implies Inside(obj->server, obj->class) */
ilu_KernelObject iluObject::ILUGetRPCObject ()
{
  if (this->ILUEnsureKernelServer() == NULL)
    return 0;
  ilu_EnterServer(this->ILURPCServer, this->ILUInstanceClassRecord);
  (void) this->ILUEnsureKernelObject();
  if (this->ILURPCObject == NULL)
    ilu_ExitServer(this->ILURPCServer, this->ILUInstanceClassRecord);
  return this->ILURPCObject;
}

/* Inside(obj->server, obj->class) */
void iluObject::ILUSetRPCObject (ilu_KernelObject obj)
{
  this->ILURPCServer = ilu_ServerOfObject(obj);
  this->ILURPCObject = obj;
}

ilu_CString iluObject::ILUStringBindingHandle ()
{
  return (ilu::SBHOfObject(this->ILUGetRPCObject()));
}

char *iluObject::ILUGetInstanceHandle ()
{
  return (this->ILUInstanceHandle);
}

/* Inside(obj->server, obj->class) */
ilu_KernelObject iluObject::ILUEnsureKernelObject()
{
  if (this->ILUEnsureKernelServer() == NULL)
    return 0;
  if (this->ILURPCObject == NULL) {
    /* Should be a true object. */
    static ilu_cardinal      idcounter = 0;
    char           *id;
    ilu_Error       lerr;

    if ((id = this->ILUGetInstanceHandle()) != NULL) {
      id = ilu_StrdupE(id, &lerr);
      if (ILU_ERRNOK(lerr)) {
	ILU_HANDLED(lerr);
	return 0;
      }
    } else {
      char            idbuf[10];

      sprintf(idbuf, "%lu", (unsigned long) (++idcounter));
      id = ilu_StrdupE(idbuf, &lerr);
      if (ILU_ERRNOK(lerr)) {
	ILU_HANDLED(lerr);
	return 0;
      }
      this->ILUInstanceHandle = ilu_StrdupE(idbuf, &lerr);
      if (ILU_ERRNOK(lerr)) {
	ilu_free(id);
	ILU_HANDLED(lerr);
	return 0;
      }
    }

  this->ILURPCObject = ilu::CreateTrueKernelObject
      (id, this->ILURPCServer,
      this->ILUInstanceClassRecord, this);
  }
  return this->ILURPCObject;
}

class iluServer * iluObject::ILUGetServer ()
{
  return ilu::GetDefaultServer();
}

void * iluObject::InputObject (iluCall call, ilu_Boolean discriminator_p, ilu_Class putative_class)
{
  ilu_KernelObject obj;
  class iluObject *o;
  ilu_Class       c;

  if (discriminator_p && putative_class->cl_singleton) {
    if ((obj = ilu_GetCallSingleton(&call->call, &call->err)) == NULL) {
      ilu_PreferSuccess(&call->err);
      return (NULL);
    }
  } else {
    obj = ilu::InputObjectID(call, discriminator_p, putative_class);
    if (obj == NULL)
      return (NULL);
  }

  /* now Inside(obj->server, putative_class) */
  o = (class iluObject *) ilu::GetLanguageSpecificObject(obj);
  if (o == NULL) {
    if ((c = ilu::GetObjectClass(obj)) != NULL)
      o = iluObject::CreateFromRegistry(c, obj);
  }
  ilu::ExitServer(ilu::GetObjectServer(obj), putative_class);
  if (o == NULL)
    return (o);
  else
    return (o->ILUCastDown(putative_class));
}

ilu_Boolean iluObject::OutputObject (iluCall call, class iluObject *obj, ilu_Class putative_class)
{
  ilu_KernelObject kobj;
  kobj = (obj == NULL) ? ((ilu_KernelObject) 0) : obj->ILUGetRPCObject();

  /* now kobj != NULL implies Inside(kobj->server, kobj->class) */
  return (ilu::OutputObjectID(call, kobj, ilu_FALSE, putative_class));
}

ilu_Cardinal iluObject::SizeOfObject (iluCall call, class iluObject *obj, ilu_Class putative_class)
{
  ilu_KernelObject kobj;
  kobj = (obj == NULL) ? ((ilu_KernelObject) 0) : obj->ILUGetRPCObject();

  /* now kobj != NULL implies Inside(kobj->server, kobj->class) */
  return (ilu::SizeOfObjectID(call, kobj, ilu_FALSE, putative_class));
}

ilu_Boolean iluObject::ILUPublish ()
{
  ilu_KernelObject kobj = this->ILUGetRPCObject();

  /* now Inside(kobj->server, kobj->class) */
  if ((this->ILUPublishProof = ilu_PublishObject (kobj)) == NULL)
    return ilu_FALSE;
  else
    return ilu_TRUE;
}

ilu_Boolean iluObject::ILUWithdraw ()
{
  ilu_Boolean status;
  ilu_KernelObject kobj = this->ILUGetRPCObject();

  /* now Inside(kobj->server, kobj->class) */
  status = ilu_WithdrawObject (kobj, this->ILUPublishProof);
  free(this->ILUPublishProof);
  this->ILUPublishProof = NULL;
  return (status);
}

void * iluObject::Lookup (char *sid, char *ih, ilu_Class pclass)
{
  ilu_KernelObject kobj;
  class iluObject *o;

  if ((kobj = ilu_LookupObject (sid, ih, pclass)) == NULL)
    return (NULL);
  /* now Inside the server of the kobj */
  if ((o = (class iluObject *) ilu::GetLanguageSpecificObject(kobj)) == NULL)
    {
      o = iluObject::CreateFromRegistry (pclass, kobj);
    }
  ilu::ExitServer(ilu::GetObjectServer(kobj), pclass);
  if (o == NULL)
    return (o);
  else
    return o->ILUCastDown(pclass);
}

char * iluObject::ILUClassName (void)
{
  return (this->ILUInstanceClassRecord
	  ? this->ILUInstanceClassRecord->cl_name
	  : (char *) 0);
}

char * iluObject::ILUClassId (void)
{
  return (this->ILUInstanceClassRecord
	  ? this->ILUInstanceClassRecord->cl_unique_id
	  : (char *) 0);
}

void iluObject::_ILU_RegisterAsGCCallback (class iluObject *c)
{
  ilu_KernelObject kobj;

  if ((kobj = c->ILUGetRPCObject()) != NULL)
    {
      ilu_SetGcClient (kobj);	// register ilu_Object with GC handler in kernel
      ilu_ExitServer(c->ILURPCServer, c->ILUInstanceClassRecord);
    }
}

//////////////////////////////////////////////////////////////////////
// code to do the GC keepalive acknowedgements
//////////////////////////////////////////////////////////////////////

class _ilu_GCCallback : public virtual iluObject {
  public:
    _ilu_GCCallback(class iluServer *server);
    virtual ~_ilu_GCCallback();

  static class _ilu_GCCallback * ILUCreateFromSBH(ilu_CString sbh);
  static class _ilu_GCCallback * ILUQuaT (class iluObject *from);
  static ilu_Class ILUClassRecord;

  virtual void * ILUCastDown (ilu_Class cast_to);

  virtual iluServer * ILUGetServer();

  private:
   class iluServer *ourServer;
};

ilu_Class _ilu_GCCallback::ILUClassRecord = NULL;

class _ilu_GCCallback * _ilu_GCCallback::ILUCreateFromSBH (ilu_CString sbh)
{
  return (_ilu_GCCallback *) ilu::SBHToObject(sbh, _ilu_GCCallback::ILUClassRecord);
}

class _ilu_GCCallback * _ilu_GCCallback::ILUQuaT (class iluObject *from)
{
  return((class _ilu_GCCallback *) (from->ILUCastDown (_ilu_GCCallback::ILUClassRecord)));
}

void * _ilu_GCCallback::ILUCastDown (ilu_Class cast_to)
{
  if (cast_to == NULL)
    return((void *)((class iluObject *) this));
  else if (cast_to == _ilu_GCCallback::ILUClassRecord)
    return ((void *) this);
  else return (NULL);
}

_ilu_GCCallback::_ilu_GCCallback (class iluServer *server)
{
  this->ILUInstanceClassRecord = ilu_GetGcCallbackClass();
  this->ILUSetMostSpecificObject((void *) this);
  this->ourServer = server;
}

_ilu_GCCallback::~_ilu_GCCallback ()
{
}

class iluServer *_ilu_GCCallback::ILUGetServer ()
{
  return (this->ourServer);
}

static class iluObject * Create__ilu_GCCallback(ilu_KernelObject obj)
{
  class _ilu_GCCallback *nobj = new _ilu_GCCallback(NULL);
  nobj->ILUSetRPCObject(obj);
  ilu::SetLanguageSpecificObject(obj, (class iluObject *) nobj);
  return ((class iluObject *) nobj);
}

class _ilu_GCCallbackInit {
  public:
    _ilu_GCCallbackInit();
};

static _ilu_GCCallbackInit do_init;
static _ilu_GCCallback * GCCallbackObject = NULL;

_ilu_GCCallbackInit::_ilu_GCCallbackInit ()
{
  class iluServer *server;

  // Initialize GCCallback class

  _ilu_GCCallback::ILUClassRecord = ilu_GetGcCallbackClass();
  iluObject::RegisterSurrogateCreator (_ilu_GCCallback::ILUClassRecord, Create__ilu_GCCallback);

  // create instance of GCCallback on server

  server = new iluServer(NULL, NULL);
  server->AddPort (NULL, NULL, ilu_TRUE);
  GCCallbackObject = new _ilu_GCCallback(server);

  iluObject::_ILU_RegisterAsGCCallback(GCCallbackObject);
}
