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
/* $Id: ilusrvr.cpp,v 1.42 1996/06/17 13:59:27 spreitze Exp $ */
/* Last edited by Mike Spreitzer June 17, 1996 6:51 am PDT */

#include <stdio.h>	/* I/O defs (including popen and pclose) */

#if ((defined(WIN32) || defined(WIN16)) && defined(_WINIO))
#include <winiodef.h>
#endif /* ((defined(WIN32) || defined(WIN16)) && defined(_WINIO)) */

#include <sys/types.h>
#include <errno.h>
#if !(defined(WIN32) || defined(WIN16))
// dll no sys/errno in VC++2
#include <sys/errno.h>
#endif /* not WIN32 */

#include "ilu.hh"

/* portability hack since we don't have access to kernel headers */
#ifndef ANSI_STRERROR
#ifdef SUNOS
#define ANSI_STRERROR(errnum)	(sys_errlist[errnum])
#else
#define ANSI_STRERROR	strerror
#endif
#endif

iluObjectTable::~iluObjectTable()
{
  fprintf(stderr, "default destructor for iluObjectTable was called!\n");
}

/* First, a simple input dispatch loop */

static iluMainLoop *theML = NULL;

static void CallRun(int *stop)
{
  theML->Run(stop);
  return;
}

static void CallExit (int *stop)
{
  theML->Exit(stop);
}

static ilu_boolean CallRegisterInput(int fd,
				     void (*proc)(int fd, void * rock),
				     void * rock)
{
  return ((ilu_boolean) theML->RegisterInputHandler(fd, proc, rock));
}

static ilu_boolean CallUnregisterInput(int fd)
{
  return ((ilu_boolean) theML->UnregisterInputHandler(fd));
}

static ilu_boolean CallRegisterOutput(int fd,
				     void (*proc)(int fd,
						  void * rock),
				     void * rock)
{
  return ((ilu_boolean) theML->RegisterOutputHandler(fd, proc, rock));
}

static ilu_boolean CallUnregisterOutput(int fd)
{
  return ((ilu_boolean) theML->UnregisterOutputHandler(fd));
}

static ilu_Alarm CallCreateAlarm (void)
{
  return (theML->CreateAlarm());
}

static void CallSetAlarm (ilu_Alarm alarm, ilu_FineTime t, void (*proc)(void * rock), void * rock)
{
  theML->SetAlarm (alarm, t, proc, rock);
}

static void CallUnsetAlarm (ilu_Alarm alarm)
{
  theML->ClearAlarm (alarm);
}

static ilu_MainLoop kml = {
  CallRun, CallExit,
  CallRegisterInput, CallUnregisterInput,
  CallRegisterOutput, CallUnregisterOutput,
  CallCreateAlarm, CallSetAlarm, CallUnsetAlarm };

void iluServer::iluSetMainLoop(iluMainLoop *ml)
{
  theML = ml;
  ilu_SetMainLoop(&kml);
  return;
}

static ilu_Boolean threaded    = ilu_FALSE;
static ilu_Boolean threadedSet = ilu_FALSE;

static void (*Fork)(void (*proc)(void *arg), void *arg) = 0;

/*Main invariant holds */
void iluServer::MonitorOutgoingConnection(void *rock)
{
  ilu_Connection  conn = (ilu_Connection) rock;
  ILU_ERRS((IoErrs)) lerr;
  (void) ilu_OutgoingConnectionThreadProc(conn, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
}

/* Main invariant holds */
void iluServer::PassNewConnections(void *rock)
{
  ilu_Connection  nu;
  ILU_ERRS((internal)) lerr;
  while (1) {
    nu = ilu_OtherNewConnection(&lerr);
    if (nu != NULL)
      (*Fork) (MonitorOutgoingConnection, nu);
    ILU_MUST_BE_SUCCESS(lerr);
  }
}

// This method is used when someone outside of this module needs to use the Fork routine.
// Since it's declared static, it isn't visible (ilu.cpp needs this)

void iluServer::MonitorConn(ilu_Connection conn)
{
    (*Fork) (MonitorOutgoingConnection, conn);
}

ilu_Boolean iluServer::SetFork(void (*fork)(void (*proc)(void *arg), void *arg))
{
  ILU_ERRS((internal)) lerr;

  if (threadedSet)
    return ilu_FALSE;
  threadedSet = ilu_TRUE;
  Fork = fork;
  threaded = ilu_TRUE;
  (void) ilu::CppLangIdx();
  (void) ilu::GetDefaultServer();
  (*fork)(PassNewConnections, NULL);
  (void) ilu_NewConnectionGetterForked(&lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  return (ilu_TRUE);
}

/* Main Invariant holds for invocations */
static ilu_ForkProc *theForkProc = 0;

/* Main Invariant holds */
static void     ErrlessFork(void (*proc) (void *arg), void *arg)
{
  ILU_ERRS((no_memory, no_resources, internal)) lerr;
  if (theForkProc == ILU_NIL)
    fprintf(stderr,
       "ilusrvr.cpp::ErrlessFork invoked with theForkProc=NIL!\n");
  else {
    (*theForkProc) (proc, arg, &lerr);
    ILU_MUST_BE_SUCCESS(lerr);
  }
}

/* Main Invariant holds */
ilu_Boolean iluServer::StartThreading(ilu_ThreadSetupProc *s,
                                       ilu_ForkProc *f)
{
  ILU_ERRS((bad_param, no_memory, no_resources, internal)) lerr;
  if (!(*s) (&lerr)) {
    ilu_DebugPrintf("iluServer::StartThreading(%p, %p) got"
		    " kernel error %s (raised at line %d of %s)"
		    " during setup!\n",
		    s, f, ILU_ERR_NAME(lerr), ilu_ErrorLine(&lerr),
		    ilu_ErrorFile(&lerr));
    ILU_HANDLED(lerr);
    return ilu_FALSE;
  }
  theForkProc = f;
  iluServer::SetFork(ErrlessFork);
  return ilu_TRUE;
}
  

/* Returns TRUE iff connection closed. */
/*Main Invariant holds
  before: L2 disjoint {arg's callmu, iomu}
  after:  L2     >=   {conn's callmu, iomu} if result==ilu_good_request,
  after:  L2 disjoint {conn's callmu, iomu} if result!=ilu_good_request */
/*If !threaded: ReadServiceRequest registered for fd of conn */
static ilu_boolean FinalServiceRequest (ilu_private arg)
{
  ilu_Connection  conn = (ilu_Connection) arg;
  ilu_RcvReqStat  s;
  iluCall_s	  call;
  ilu_Class       ptype;
  ilu_Method      method;
  ilu_Boolean	  initted;
  ilu_Cardinal    serial_number;

  iluServer::DisableRequests((iluCall) 0, conn);
  call.call.ca_reqs_enabled = ilu_FALSE;
  s = ilu::ReceiveRequest(conn, &call, &initted, &ptype, &method,
                          &serial_number);
  if (s == ilu_RcvReqStat_request) {
    ilu_refany      stubproc = ilu_GetMethodStubProc(method);
    _ilu_Assert(stubproc != NULL, "ilusrvr.cpp:FinalServiceRequest");
    (*(void (*) (ilu_Call)) stubproc) (&call.call);
  }
  if (initted) {
    ilu_FinishCall(&call.call, &call.err);
    ilu_PreferSuccess(&call.err);
    ILU_HANDLED(call.err);
  }
  if (s == ilu_RcvReqStat_quit)
    return ilu_TRUE;
  if (!call.call.ca_reqs_enabled) {
    iluServer::EnableRequests(&call, conn);
  }
  return ilu_FALSE;
}

/* This routine handles the dispatching of an incoming request */

static void ReadServiceRequest (void *arg)
{
  (void) FinalServiceRequest(arg);
}

/* When a request for a new connection to the server is received,
   we have to handle it. */

/*ARGSUSED*/
void ReadConnectionRequest (void *arg)
{
  ilu_Port        p = (ilu_Port) arg;
  ilu_Error       lerr;
  ilu_Connection  conn;

  if ((conn = ilu::HandleNewConnection(p)) == NULL) {
    fprintf(stderr, "iluServer::ReadConnectionRequest:  Error:  Unable to accept incoming connection.\n");
    return;
  }
  ilu_SetConnectionInputHandler(conn, ReadServiceRequest, conn, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
}


/* Main invariant holds */
static void RunConnection (void *arg)
{
  ilu_Connection conn   = (ilu_Connection) arg;

  for (;;) {
    if (!ilu_BlockingWaitForInputOnConnection(conn, (ilu_FineTime *) 0))
      break;
    if (FinalServiceRequest(conn))
      break;
  }
  ilu::CloseConnection(conn);
}

/* Main invariant holds */
static void ReadConnectionRequests (void *arg)
{
  ilu_Port        p = (ilu_Port) arg;
  ilu_Connection  conn;
  while (ilu::WaitForPortConnectionRequest(p)) {
    conn = ilu::HandleNewConnection(p);
    if (conn != ILU_NIL)
      Fork(RunConnection, conn);
  }
}

/* Inside(obj->server, obj->class) */
static ilu_KernelObject Call_object_of_ih(ilu_ObjectTable self,
					  ilu_string ih)
{
  iluObjectTable *objtab = (iluObjectTable *) self->ot_rock;
  iluObject * obj = (iluObject *) objtab->ObjectOfIH(ih);
  if (obj == NULL)
    return (NULL);
  else
    return obj->ILUEnsureKernelObject();
}

static void Call_free_self(ilu_ObjectTable self)
{
  iluObjectTable *objtab = (iluObjectTable *) self->ot_rock;
  delete objtab;
  free((char *) self);
  return;
}

static ilu_string inmemTinfo[] = { "inmem_", 0 };

iluServer::iluServer (	char *serviceID /* = NULL */,
			iluObjectTable *objtab /* = NULL */)
{
  ilu_ObjectTable kot;
  threadedSet = ilu_TRUE;
  if (objtab == NULL)
    kot = NULL;
  else {
    kot = (ilu_ObjectTable) ilu_must_malloc(sizeof(ilu_ObjectTable_s));
    kot->ot_object_of_ih = Call_object_of_ih;
    kot->ot_free_self = Call_free_self;
    kot->ot_rock = (void *) objtab;
  }
  if ((serviceID == NULL) || (strlen(serviceID) == 0))
    serviceID = ilu_InventID();
  ks = ilu_CreateTrueServer(serviceID, kot, ilu::CppLangIdx());
  (void) AddPort("sunrpc", inmemTinfo, 0);
  /* What to do if this fails?? */
  return;
}

ilu_Boolean iluServer::AddPort(	char *protocolType,
				ilu_TransportInfo transportType,
				ilu_Boolean be_default)
{
  ilu_Error       lerr;
  ilu_Port        nu;
  nu = ilu::CreatePort(ks, protocolType, transportType);
  if (nu == NULL) {
    fprintf(stderr, "iluServer::Start:  Error:  Couldn't open port.\n");
    return (ilu_FALSE);
  }
  if (be_default) ilu::SetServerDefaultPort(ks, nu);
  if (threaded && transportType != inmemTinfo) {
    Fork(ReadConnectionRequests, nu);
  } else {
    ilu_SetConnectionRequestHandler(nu, ReadConnectionRequest, nu, &lerr);
    ILU_MUST_BE_SUCCESS(lerr);
  }
  return (ilu_TRUE);
}

void iluServer::Stoppable_Run(int *stop)
{
  ilu_RunMainLoop(stop);
}

void iluServer::Run()
{
  int stop = 0;

  threadedSet = ilu_TRUE;
  ilu_RunMainLoop(&stop);
}

ilu_Server iluServer::KernelServer()
{
  return (this->ks);
}


/*L1, L2, Main unconstrained*/
ilu_Boolean iluServer::EnableRequests(iluCall call, ilu_Connection conn)
{
  if (threaded)
    return ilu_TRUE;

  ilu_Server      s = ilu_ServerOfConnection(conn);
  ilu_string      sid = ilu_IDOfServer(s);
  ilu_Error       lerr;
  ilu_kernelBoolean ans;
  ilu_Error	*perr;

  if (call == ILU_NIL)
    perr = &lerr;
  else
    perr = &call->err;
  ans = ilu_SetConnectionInputHandler(conn, ReadServiceRequest, conn, perr);
  ilu_PreferSuccess(perr);
  if (!ans)
    fprintf(stderr,
	    "iluServer.cc:  Can't register ReadServiceRequest on conn %p server %s!\n",
	    conn, sid);
  else if (call != ILU_NIL)
    call->call.ca_reqs_enabled = ilu_TRUE;
  return ans;
}

/*L1, L2, Main unconstrained*/
ilu_Boolean iluServer::DisableRequests(iluCall call, ilu_Connection conn)
{
  if (threaded)
    return ilu_TRUE;

  ilu_Server      s = ilu_ServerOfConnection(conn);
  ilu_string      sid = ilu_IDOfServer(s);
  ilu_Error       lerr;
  ilu_kernelBoolean ans;
  ilu_Error	*perr;

  if (call == ILU_NIL)
    perr = &lerr;
  else
    perr = &call->err;
  ans = ilu_SetConnectionInputHandler(conn, 0, NULL, perr);
  ilu_PreferSuccess(perr);
  if (!ans)
    fprintf(stderr, "iluServer.cc:  bug detected when unregistering input source for conn %p server %s!\n", conn, sid);
  else if (call != ILU_NIL)
    call->call.ca_reqs_enabled = ilu_FALSE;
  return ans;
}
