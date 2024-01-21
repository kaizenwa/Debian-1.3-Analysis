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

  $Id: iluPrmodule.c,v 1.119 1996/07/17 17:53:48 janssen Exp $
  */

#include <stdio.h>
#include <sys/types.h>	/* for gid_t */

/* from ILU */
#include <iluxport.h>
/* local */
#include "python.h"
#include "ilualobject.h"
#include "ilucaobject.h"
#include "iluclobject.h"
#include "iluftobject.h"
#include "ilulrobject.h"
#include "ilusvobject.h"
#include "ilulpobject.h"
#include "ilugiobject.h"
#include "iohcobject.h"
#include "ivobject.h"
#include "thcobject.h"
#include "pythonthreads.h"

/* from Python */
#include "sysmodule.h"
#include "intrcheck.h"	/* for PyOS_InterruptOccurred */

#define Py_COUNT(x)	(((PyObject *)(x))->ob_refcnt)

static ilu_boolean readServiceRequest(ilu_refany, ilu_boolean);
static void runConnection(void *arg);
static void readConnectionRequests(void *arg);
static ilu_TransportInfo getInmemTransport(void);
static void cleanupUninterestingObjects (ilu_private junk);
static void singleThreadedReadServiceRequest (ilu_refany);

#ifdef ILU_PYTHON_THREADS

/* 
   Python threading is achieved by adding the global interpreter lock
   to the partial order of mutexes as follows:
   X < global interpreter lock, for all X

   That is, there are no restrictions on what mutexes a thread may be holding
   when it attempts to enter the Python interpreter, but a thread may not enter
   any mutexes while holding the global interpter lock.  

   There can be no deadlocks on waiting for the global interpter lock
   because whichever thread is holding the global interpreter lock
   must relinquish it before entering any other mutexes.

   There can be no deadlocks waiting for ILU mutexes because a thread
   cannot hold the global interpter lock while trying to enter into a
   mutex, so the thread cannot prevent the thread holding these
   mutexes from proceeding.

   In addition to avoiding deadlock, we attempt to avoid long delays
   in waiting for the global interpter lock by releasing the lock
   around calls into the ILU kernel that might potentially block or
   otherwise take a long time.
   */

#include "oscalls.h" /* for OS_SLEEP */
static void bootstrapAlarm(ilu_private);
static PyObject *ilupython_thread_push_call(PyObject *, PyObject *);
static void ilupython_thread_pop_call(PyObject *);
static PyObject *ilupython_thread_current_call();
static PyObject *ilupython_thread_init_stack(PyObject *tid);
static void ilupython_collect_outgoing_connections(void *arg);
static void ilupython_watch_outgoing_connection(void *arg);

/* start_new_thread is defined by Python but has no "Py_" synonym */
#define ilupython_fork_thread(proc, arg) (start_new_thread(proc, arg)) 
static PyObject *thread_call_stack_map;

/* This flag is also accessed by ivobject.c */
ilu_boolean ilupython_threaded_operation = ilu_FALSE;

typedef struct
{
  void (*real_proc)(ilu_private arg);
  ilu_private arg;
} bootstrap_rock;

static bootstrap_rock gc_alarm_rock = { cleanupUninterestingObjects, ILU_NIL };

#define current_thread_id() PyInt_FromLong(get_thread_ident())
#endif /* ILU_PYTHON_THREADS */

#if (ILUPYTHON_MAJOR_VERSION == 1 && ILUPYTHON_MINOR_VERSION >= 3)
#define PYINSTANCE_NEW(type,arg)		PyInstance_New((type),(arg),ILU_NIL)
#else
#define PYINSTANCE_NEW(type,arg)		PyInstance_New((type),(arg))
#endif	/* (ILUPYTHON_MAJOR_VERSION == 1 && ILUPYTHON_MINOR_VERSION == 3) */

#if (ILUPYTHON_MAJOR_VERSION == 1 && ILUPYTHON_MINOR_VERSION >= 2)
#define PYTHON_LONG_FROM_STRING(str,base)	PyLong_FromString (str, ILU_NIL, base)
#else
#define PYTHON_LONG_FROM_STRING(str,base)	PyLong_FromString (str, base)
#endif /* (ILUPYTHON_MAJOR_VERSION == 1 && ILUPYTHON_MINOR_VERSION == 2) */


  /* exceptions defined by this module */
       PyObject *		_ilupython_GeneralError;
static PyObject *		ProtocolError;
static PyObject *		UnimplementedMethodError;

/* strings used in exceptions */
static char strArg1ShouldBeIluCall[] = "arg1 should be ilu_Call";
static char strArg2ShouldBeIntOrLongInt[] = "arg2 should be int or long int";
static char strArg2OutOfRange[] = "arg2 is out of range";
static char strListTooLong[] = "list length exceeds limit";
static char strListDoesntMatch[] = "list length doesn't match array bound";

/* time interval used in GC cleanups */
static const ilu_FineTime plus1Minute = { 60, 0 };

/* names */
static char nameVarClass[]		= "_IluClass";
static char nameVarInstVars[]		= "_IluInstVars";
static char nameVarInstHandle[]		= "IluInstHandle";
static char nameVarServer[]		= "IluServer";

/* names used in Sun RPC UNIX authentication */
static char nameSunRPCAuth[]		= "sunrpc-unix";
static char nameSunRPCAuthUID[]		= "uid";
static char nameSunRPCAuthGID[]		= "gid";
static char nameSunRPCAuthHostname[]	= "hostname";
static char nameSunRPCAuthGroups[]	= "groups";

/* name used for default peer identification */
static char nameConnectionIdentityInfo[]= "connection";

/* name used for GSS identity */
static char nameGSSIdentityInfo[]	= "GSS";

/* global state */
static PyObject *	defaultTrueServer;
static int		stopMainLoop;
static ilu_refany	gcAlarm = ILU_NIL;
static ilu_boolean	gcAlarmSet = ilu_FALSE;
static PyObject *	gcList = ILU_NIL;

/* global mapping table from class ID to IluclObject * */
static PyObject *	classMap = ILU_NIL;

/* Global var used by ivobject.c */
       ilu_cardinal	_ilupython_LangIndex = 0;	/* obtained by initialization call to ilu_RegisterLanguage("Python"); */

/********************************/

static char *
  stringDup(char *src)
{
  char *	result;

  if ((result = PyMem_NEW(char, strlen(src) + 1)) == 0)
    {
      (void) PyErr_NoMemory();
      return 0;
    }
  strcpy(result, src);
  return result;
}

char *
  _ilupython_formErrDescription (char *buf, ilu_Error *err)
{
  char *p;

  sprintf (buf, "%s, %lu:  <%s> %s", ilu_ErrorFile(err), (unsigned long) ilu_ErrorLine(err),
	   ILU_ERR_NAME(*err), ILU_ERR_DESCRIPTION(*err));
  p = buf + strlen(buf);
  ILU_ERR_SWITCH(*err) {
    ILU_ERR_CASE(bad_param, errp)
      { sprintf (p, " %lu", (unsigned long) errp->minor); }
    ILU_ERR_CASE(imp_limit, errp)
      { sprintf (p, " %lu (%s)",
		 (unsigned long) errp->minor,
		 (errp->minor == ilu_ilm_strlen) ? "maximum string length safety param exceeded" :
		 ((errp->minor == ilu_ilm_nomst) ? "no most specific type known" : "unknown")); }
    ILU_ERR_CASE(marshal, errp)
      { sprintf (p, " %lu", (unsigned long) errp->minor); }
    ILU_ERR_CASE(internal, errp)
      { sprintf (p, " %lu", (unsigned long) errp->minor); }
    ILU_ERR_CASE(inv_objref, errp)
      { sprintf (p, " %lu", (unsigned long) errp->minor); }
    ILU_ERR_CASE(no_resources, errp)
      { sprintf (p, " %lu", (unsigned long) errp->minor); }
    ILU_ERR_CASE(no_memory, errp)
      { sprintf (p, " %lu bytes requested", (unsigned long) errp->nbytes); }
    ILU_ERR_ELSE
      {}
  } ILU_ERR_ENDSWITCH;
  return buf;
}

static void
  handleCalloutException(char *culprit, ilu_Error *err)
{
  PyObject *	except;
  PyObject *	val;
  PyObject *	f;

#if (ILUPYTHON_MAJOR_VERSION == 1 && ILUPYTHON_MINOR_VERSION >= 2)

  PyObject *	traceback;

  PyErr_Fetch (&except, &val, &traceback);
  Py_XDECREF (traceback);
  PyErr_Clear ();

#else

  PyErr_GetAndClear(&except, &val);

#endif /* (ILUPYTHON_MAJOR_VERSION == 1 && ILUPYTHON_MINOR_VERSION >= 2) */

  if (except == PyExc_SystemExit)
    {
      if (val == 0 || val == Py_None)
	Py_Exit(0);
      if (PyInt_Check(val))
	Py_Exit(PyInt_AsLong(val));
    }
#if (ILUPYTHON_MAJOR_VERSION == 1 && ILUPYTHON_MINOR_VERSION >= 2)

  if ((f = PySys_GetObject("stderr")) != 0)
    {
      PyObject *trace = PyTraceBack_Fetch();
      if (trace != 0)
	{
	  PyTraceBack_Print (trace, f);
	  Py_DECREF (trace);
	}
    }

#else

  if ((f = PySys_GetObject("stderr")) != 0)
    PyErr_PrintTraceBack(f);

#endif /* (ILUPYTHON_MAJOR_VERSION == 1 && ILUPYTHON_MINOR_VERSION >= 2) */

  ilu_DebugPrintf ("%s raised exception ", culprit);
  PyObject_Print(except, stderr, Py_PRINT_RAW);
  if (val && val != Py_None)
    {
      ilu_DebugPrintf (": ");
      PyObject_Print(val, stderr, Py_PRINT_RAW);
    }
  ilu_DebugPrintf ("\n");

  if (err != ILU_NIL && ILU_ERROK(*err))
    ILU_ERR_CONS0(unknown, err, 0);

  PyErr_Clear();
}

/********************************/

static PyObject *
  ilumod_SetDebugLevel(PyObject *self, PyObject *args)
{
  long	bits;
  char *spec;
  ilu_cardinal old_level;

  if (PyArg_Parse(args, "l", &bits))
    old_level = ilu_SetDebugLevel(bits); /* non-blocking */
  else if (PyArg_Parse(args, "z", &spec))
    old_level = ilu_SetDebugLevelViaString (spec); /* non-blocking */
  else return 0;
  return PyInt_FromLong(old_level);
}

/********************************/

static ilu_Class
  getKernelClass(PyClassObject *pycl)
{
  PyObject *	cl;
  char		msg[256];

  if ((cl = PyObject_GetAttrString((PyObject *) pycl, nameVarClass)) == 0)
    {
      sprintf(msg, "class arg should have a variable named %s",
	      nameVarClass);
      PyErr_SetString(PyExc_TypeError, msg);
      return 0;
    }
  if (!ilucl_Check(cl))
    {
      sprintf(msg, "class arg's %s variable should be ilu_Class",
	      nameVarClass);
      PyErr_SetString(PyExc_TypeError, msg);
      return 0;
    }
  return ((IluclObject *) cl)->c;
}

static IluclObject *
  getPythonClass (PyClassObject *pycl)
{
  PyObject *	cl;
  char		msg[256];

  if ((cl = PyObject_GetAttrString((PyObject *) pycl, nameVarClass)) == 0)
    {
      sprintf(msg, "class arg should have a variable named %s",
	      nameVarClass);
      PyErr_SetString(PyExc_TypeError, msg);
      return 0;
    }
  if (!ilucl_Check(cl))
    {
      sprintf(msg, "class arg's %s variable should be ilu_Class",
	      nameVarClass);
      PyErr_SetString(PyExc_TypeError, msg);
      return 0;
    }
  return ((IluclObject *) cl);
}

/********************************/

typedef struct CrNode CrNode;
struct CrNode
{
  PyClassObject *	pycl;
  ilu_Class	kclass;
  CrNode *	next;
};

static CrNode *	crHead;

static int
  addToClassRegistry(PyClassObject *pycl)
{
  CrNode *	n	= PyMem_NEW(CrNode, 1);
  ilu_Class	kclass;

  if ((kclass = getKernelClass(pycl)) == 0)
    return -1;
  if (n == 0)
    {
      (void) PyErr_NoMemory();
      return -1;
    }
  Py_INCREF(pycl);
  n->pycl = pycl;
  n->kclass = kclass;
  n->next = crHead;
  crHead = n;
  return 0;
}

static PyObject *
  findInClassRegistry(ilu_Class kclass)
{
  CrNode *	n;

  for (n = crHead; n != 0; n = n->next)
    {
      if (n->kclass == kclass)
	return (PyObject *) n->pycl;
    }
  return 0;
}

/********************************/

static PyObject *
  ilumod_FormClassRecord(PyObject *self, PyObject *args)
{
  char *		name;
  char *		brand;
  char *		uniqueId;
  char *		singleton;
  char		collectible;
  char		optional;
  char *		authentication;
  PyObject *	methodTuple;
  PyObject *	superclassTuple;
  PyObject *	result;

  if (!PyArg_Parse(args, "(ssszbbzOO)", &name, &brand, &uniqueId,
		   &singleton, &collectible, &optional, &authentication,
		   &methodTuple, &superclassTuple))
    return 0;
  if (!PyTuple_Check(methodTuple))
    {
      PyErr_SetString(PyExc_TypeError, "arg7 should be tuple");
      return 0;
    }
  if (!PyTuple_Check(superclassTuple))
    {
      PyErr_SetString(PyExc_TypeError, "arg8 should be tuple");
      return 0;
    }
  result = ilucl_New(name, brand, uniqueId, singleton, collectible,
		     optional, authentication, methodTuple, superclassTuple);
  return result;
}

/* Forward declarations. */
static int		ensureGcClient(void);

static PyObject *
  ilumod_RegisterClass(PyObject *self, PyObject *args)
{
  PyClassObject *	pycl;
  IluclObject *		icl;
  ilu_Class	kclass;

  if (!PyArg_Parse(args, "O", &pycl))
    return 0;
  if (!PyClass_Check(pycl))
    {
      PyErr_SetString(PyExc_TypeError, "arg1 should be class");
      return 0;
    }
  if ((kclass = getKernelClass(pycl)) == 0)
    return 0;
  if (addToClassRegistry(pycl) < 0)
    return 0;
  icl = getPythonClass(pycl);
  PyDict_SetItemString (classMap, icl->id, (PyObject *) icl);
  if (icl->collectible && ensureGcClient() < 0)
    return 0;
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_RegisterSkeletons(PyObject *self, PyObject *args)
{
  IluclObject *	cl;
  PyObject *	skelTuple;

  if (!PyArg_Parse(args, "(OO)", &cl, &skelTuple))
    return 0;
  if (!ilucl_Check(cl))
    {
      PyErr_SetString(PyExc_TypeError, "arg1 should be ilu_Class");
      return 0;
    }
  if (!PyTuple_Check(skelTuple))
    {
      PyErr_SetString(PyExc_TypeError, "arg2 should be tuple");
      return 0;
    }
  if (ilucl_RegisterSkeletons(cl, skelTuple) < 0)
    return 0;
  Py_INCREF(Py_None);
  return Py_None;
}

/********************************/

#define NullClObjectPtr			((IluclObject *) 0)

static ilu_boolean
  ConcurrentConn(IlucaObject *ca)
{
  if (!iluca_Check(ca))
    {
      ilu_DebugPrintf ("ConcurrentConn:  called with non-IlucaObject * obj\n");
      return ilu_FALSE;
    }
  else
    {
      return ilu_ThreadPerRequest(ilu_ConnectionOfCall(ca->call)); /* both non-blocking */
    }
}

static void
  enableRequestsOnConn(ilu_Connection conn)
{
  ilu_Error err = ILU_INIT_NO_ERR;

  ILUPY_ILLEGAL_IN_THREADED(ilu_ThreadPerRequest(conn));

  CALL_KERNEL(ilupython_threaded_operation, ilu_SetConnectionInputHandler (conn,
				  singleThreadedReadServiceRequest,
				  (ilu_refany) conn,
				  &err));
  if (ILU_ERRNOK(err))
    {
      char buf[1000];
      _ilupython_formErrDescription (buf, &err);
      ilu_DebugPrintf ("ilu: ilu_SetConnectionInputHandler() signals <%s>.\n",
	      buf);
      ILU_HANDLED(err);
    }
}

static void
  disableRequestsOnConn(ilu_Connection conn)
{
  ilu_Error err = ILU_INIT_NO_ERR;

  ILUPY_ILLEGAL_IN_THREADED(ilu_ThreadPerRequest(conn));

  CALL_KERNEL(ilupython_threaded_operation, ilu_SetConnectionInputHandler (conn,
				  (ilu_TransportInputHandler) 0,
				  ILU_NIL,
				  &err));

  if (ILU_ERRNOK(err))
    {
      char buf[1000];
      _ilupython_formErrDescription (buf, &err);
      ilu_DebugPrintf ("ilu: ilu_SetConnectionInputHandler() signals <%s>.\n",
	      buf);
      ILU_HANDLED(err);
    }
}

static ilu_boolean
  enableRequestsOnCallConn(IlucaObject *ca)
{
  ilu_Connection conn = ilu_ConnectionOfCall(ca->call);

  ILUPY_ILLEGAL_IN_THREADED(ilu_ThreadPerRequest(conn));

  CALL_KERNEL(ilupython_threaded_operation, ilu_SetConnectionInputHandler (conn,
				 singleThreadedReadServiceRequest,
				 (ilu_refany) conn,
				 &ca->err));

  if (ILU_ERRNOK(ca->err))
    {
      char buf[1000];
      _ilupython_formErrDescription (buf, &ca->err);
      ilu_DebugPrintf ("ilu: ilu_SetConnectionInputHandler() signals <%s>.\n",
	      buf);
      return ilu_FALSE;
    }
  else
    {
      ca->conn_disabled = ilu_FALSE;
      return ilu_TRUE;
    }
}

static ilu_boolean
  disableRequestsOnCallConn(IlucaObject *ca)
{
  ilu_Connection conn = ilu_ConnectionOfCall(ca->call);

  ILUPY_ILLEGAL_IN_THREADED(ilu_ThreadPerRequest(conn));

  CALL_KERNEL(ilupython_threaded_operation, ilu_SetConnectionInputHandler (conn,
				 (ilu_TransportInputHandler) 0,
				 ILU_NIL,
				 &ca->err));
  if (ILU_ERRNOK(ca->err))
    {
      char buf[1000];
      _ilupython_formErrDescription (buf, &ca->err);
      ilu_DebugPrintf ("ilu: ilu_SetConnectionInputHandler() signals <%s>.\n",
	      buf);
      return ilu_FALSE;
    }
  else
    {
      ca->conn_disabled = ilu_TRUE;
      return ilu_TRUE;
    }
}

static void
  callSkeleton(ilu_Call call, ilu_Class kclass, ilu_Method meth)
{
  IluclObject *	cl;
  int		methodIndex;
  PyObject *	skelFunc;
  PyObject *	skelArgs;
  IlucaObject *	ca;
  PyObject *	result;
  ilu_Method    methods;
  ilu_cardinal	method_count;
  ilu_string	uid;
  ilu_Error	lerr = ILU_INIT_NO_ERR;
  ilu_Connection conn;

  conn = ilu_ConnectionOfCall(call); /* non-blocking */
  if (!ilu_DataOfClass (kclass, ILU_NIL, ILU_NIL, &uid, ILU_NIL,
			ILU_NIL, &method_count, ILU_NIL, ILU_NIL,
			ILU_NIL, &methods)) /* non-blocking */
    {
      ilu_DebugPrintf ("callSkeleton:  invalid kclass, can't get DataOfClass\n");
      ILU_ERR_CONS1(internal, &lerr, minor, ilu_im_check, ILU_NIL);
    }
  else
    {
      cl = (IluclObject *) PyDict_GetItemString (classMap, uid);

      if (!ilucl_Check(cl))
	{
	  ilu_DebugPrintf ("callSkeleton:  invalid kclass, yields bad IluclObject *\n");
	  ILU_ERR_CONS1(internal, &lerr, minor, ilu_im_check, ILU_NIL);
	}
      else
	{
	  methodIndex = meth - methods;
	  if (methodIndex < 0 || method_count <= methodIndex)
	    {
	      ilu_DebugPrintf ("callSkeleton: bad method index (%d)\n",
			       methodIndex);
	      ILU_ERR_CONS1(internal, &lerr, minor, ilu_im_check, ILU_NIL);
	    }
	  else if (cl->skeletons == 0 ||
		   ((skelFunc = cl->skeletons[methodIndex]) == 0))
	    {
	      ilu_DebugPrintf (
			       "callSkeleton: no skeleton for method index (%d)\n",
			       methodIndex);
	      ILU_ERR_CONS1(internal, &lerr, minor, ilu_im_check, ILU_NIL);
	    }
	  else if (!PyCallable_Check(skelFunc))
	    {
	      ilu_DebugPrintf (
			       "callSkeleton: bad skeleton for method index (%d)\n",
			       methodIndex);
	      ILU_ERR_CONS1(internal, &lerr, minor, ilu_im_check, ILU_NIL);
	    }
	  else if ((ca = (IlucaObject *) iluca_FromCall(call)) == 0)
	    {
	      ilu_DebugPrintf ("callSkeleton: couldn't create ilu_Call\n");
	      ILU_ERR_CONS1(internal, &lerr, minor, ilu_im_check, ILU_NIL);
	    }
	  else
	    {
	      if ((skelArgs = Py_BuildValue("(O)", ca)) == 0)
		{
		  ilu_DebugPrintf ("callSkeleton: couldn't build args\n");
#if defined(WIN32)
		  ilu_Error *e;
		  e = &ca->err;
		  ILU_ERR_CONS1(internal, e, minor, ilu_im_check, ILU_NIL);
#else
		  ILU_ERR_CONS1(internal, &ca->err, minor, ilu_im_check, ILU_NIL);
#endif
		}
	      else
		{
#ifdef ILU_PYTHON_THREADS
		  if (ilupython_threaded_operation)
		    {
		      PyObject *tid = current_thread_id();

		      if (!tid)
			{
			  /* it's probably somehow possible to figure out how many bytes we need */
#if defined(WIN32)
			  ilu_Error *e;
			  e = &ca->err;
			  ILU_ERR_CONS1(no_memory, e, nbytes, 0, ILU_NIL);
#else
			  ILU_ERR_CONS1(no_memory, &ca->err, nbytes, 0, ILU_NIL);
#endif
			}
		      else
			{
			  PyObject *call_stack;

			  if (!(call_stack = ilupython_thread_push_call(tid, (PyObject *)ca)))
			    {
#if defined(WIN32)
			      ilu_Error *e;
			      e = &ca->err;
			      ILU_ERR_CONS1(no_memory, e, nbytes, 0, ILU_NIL);
#else
			      ILU_ERR_CONS1(no_memory, &ca->err, nbytes, 0, ILU_NIL);
#endif
			    }
			  else
			    {
			      result = PyEval_CallObject(skelFunc, skelArgs);

			      if (result == 0)
				handleCalloutException("skeleton", &ca->err);
			      else
				Py_DECREF(result);

			      ilupython_thread_pop_call(call_stack);
			    }
			  Py_DECREF(tid);
			}
		    }
		  else
		    {
		      result = PyEval_CallObject(skelFunc, skelArgs);
			  
		      if (result == 0)
			handleCalloutException("skeleton", &ca->err);
		      else
			Py_DECREF(result);
		    }
#else /* !ILU_PYTHON_THREADS */
		  result = PyEval_CallObject(skelFunc, skelArgs);
			  
		  if (result == 0)
		    handleCalloutException("skeleton", &ca->err);
		  else
		    Py_DECREF(result);
#endif /* ILU_PYTHON_THREADS */
		  Py_DECREF(skelArgs);
		}
	      CALL_KERNEL(ilupython_threaded_operation, ilu_FinishCall (ca->call, &ca->err));
	      ILU_HANDLED(ca->err);
	      if (!ilupython_threaded_operation && ca->conn_disabled)
		enableRequestsOnConn(conn);
	      
	      Py_DECREF(ca);
	      return;
	    }
	}
    }

  if (!ilupython_threaded_operation)
    enableRequestsOnConn(conn);
  CALL_KERNEL(ilupython_threaded_operation, ilu_FinishCall (call, &lerr));
  ilu_free(call);
  ILU_HANDLED(lerr);
}

/* before: conn registered
 **         (meaning readServiceRequest is registered for conn's fd)
 ** after: conn registered iff conn not closed
 */
static ilu_boolean
  readServiceRequest(ilu_refany rock, ilu_boolean single_threaded)
{
  ilu_Connection		conn	= (ilu_Connection) rock;
  ilu_RcvReqStat		stat;
  ilu_Class			kclass;
  ilu_Method			meth;
  ilu_cardinal			serialNo;
  ilu_Call_s			call;
  ilu_Error			err;
  ilu_boolean			initted;

  if (single_threaded)
    disableRequestsOnConn(conn);

  CALL_KERNEL(ilupython_threaded_operation, stat = ilu_ReceiveRequest(&call, &initted, conn, &kclass, &meth, &serialNo, &err));

  if (stat == ilu_RcvReqStat_request)
    {
      callSkeleton(&call, kclass, meth);
    }
  else if (initted)
    {
      CALL_KERNEL(ilupython_threaded_operation, ilu_FinishCall (&call, &err));
      ILU_HANDLED(err);
    }
  if (ILU_ERRNOK(err))
    {
      fprintf (stderr, "Error parsing or serving request on connection %p:  %s",
	       conn, ILU_ERR_NAME(err));
      if (stat == ilu_RcvReqStat_request)
	fprintf (stderr, ", serial number %ul, method \"%s.%s\"\n",
		 serialNo, kclass->cl_name, meth->me_name);
      else
	fprintf (stderr, " (no reliable info about the request is available)\n");
    }
  if (single_threaded && (stat == ilu_RcvReqStat_noop))
    enableRequestsOnConn(conn);
  return (stat == ilu_RcvReqStat_quit ? ilu_TRUE : ilu_FALSE);
}

/* after: new connection registered */
static void
  readConnectionRequest(ilu_private rock)
{
  ilu_Port		port	= (ilu_Port) rock;
  ilu_Connection	conn;
  ilu_boolean		closed;

  CALL_KERNEL(ilupython_threaded_operation, conn = ilu_HandleNewConnection(port, &closed));
    
    if (conn == 0)
    {
      ilu_DebugPrintf ("ilu_HandleNewConnection failed\n");
      return;
    }

    if (!ilupython_threaded_operation)
      enableRequestsOnConn(conn);
}

static ilu_Port
  createPort(ilu_Server kserver, ilu_TransportInfo transport, char *protocol, ilu_Passport pp)
{
  ilu_Port	port;
  int		fd;
  ilu_boolean	closed;
  ilu_Error	err = ILU_INIT_NO_ERR;
  ilu_boolean result;

  CALL_KERNEL(ilupython_threaded_operation, (port = ilu_CreatePort(kserver, protocol, transport, pp, &err)));

  if (port == 0)
    {
      char buf[1000];
      _ilupython_formErrDescription (buf, &err);
      ilu_DebugPrintf ("ilu: ilu_CreatePort signals <%s>.\n",
	       buf);
      ILU_HANDLED(err);
      return 0;
    }

#ifdef ILU_PYTHON_THREADS
  if (ilupython_threaded_operation && (transport != getInmemTransport()))
    {
      if (!ilupython_fork_thread(readConnectionRequests, (void *)port))
	{
	  /* should I clean up the port somehow? */
	  return 0;
	}
      return port;
    }
#endif /* ILU_PYTHON_THREADS */

  CALL_KERNEL(ilupython_threaded_operation, result = ilu_SetConnectionRequestHandler(port, readConnectionRequest,
				       (ilu_refany) port, &err));
     
  if (!result)
    {
      char buf[1000];
      _ilupython_formErrDescription (buf, &err);
      ilu_DebugPrintf ("ilu: ilu_SetConnectionRequestHandler signals <%s>.\n",
	       buf);
      ILU_HANDLED(err);
      return 0;
    }
  return port;
}

static ilu_TransportInfo
  getInmemTransport(void)
{
  static ilu_TransportInfo tinfo = ILU_NIL;
  /* this should always be called inside the global interpreter lock, so the static variable
     trick, while ugly :-), should be safe -- robh */

  if (tinfo == ILU_NIL)
    {
      tinfo = (ilu_TransportInfo) ilu_malloc (sizeof(ilu_string) * 2 + /* sizeof "inmem" */ 6);
      if (tinfo != ILU_NIL)
	{
	  tinfo[0] = ((char *)tinfo) + (2 * sizeof(ilu_string));
	  tinfo[1] = ILU_NIL;
	  strcpy (tinfo[0], "inmem");
	}
    }
  return tinfo;
}

static ilu_Server
  createTrueServer(char *serverId, ilu_ObjectTable objTab, ilu_TransportInfo transport,
		   char *protocol, ilu_Passport pp)
{
  ilu_Server	kserver;

  CALL_KERNEL(ilupython_threaded_operation, (kserver = ilu_CreateTrueServer(serverId, objTab, _ilupython_LangIndex)));

  if (kserver == 0)
    {
      PyErr_SetString(_ilupython_GeneralError, "unable to create true server");
      return 0;
    }

  if (createPort(kserver, getInmemTransport(), "sunrpc", pp) == 0)
    {
      PyErr_SetString(_ilupython_GeneralError, "unable to create local port for new true server");
      return 0;
    }

  if (createPort(kserver, transport, protocol, pp) == 0)
    {
      PyErr_SetString(_ilupython_GeneralError, "unable to create specified port on new server");
      return 0;
    }
  return kserver;
}

static ilu_TransportInfo
  getDefaultTransport(void)
{
  static ilu_TransportInfo tinfo = ILU_NIL;

  /* again, the static trick is okay because this should only be called from within the global interpreter
     lock */

  if (tinfo == ILU_NIL)
    {
      tinfo = (ilu_TransportInfo) ilu_malloc (sizeof(ilu_string) * 3 + /* sizeof "sunrpcrm" */ 9 + /* sizeof "tcp_0_0" */ + 8);
      if (tinfo != ILU_NIL)
	{
	  tinfo[0] = ((char *)tinfo) + (3 * sizeof(ilu_string));
	  tinfo[1] = ((char *)tinfo) + (3 * sizeof(ilu_string)) + 9;
	  tinfo[2] = ILU_NIL;
	  strcpy (tinfo[0], "sunrpcrm");
	  strcpy (tinfo[1], "tcp_0_0");
	}
    }
  return tinfo;
}

static char *
  getDefaultProtocol(const char **transport)
{
#if 0
  if (ilupython_threaded_operation)
    return "sunrpc";
  else
    return "csunrpc";
#endif
  return "csunrpc";
}

static int
  convTinfo (PyObject *o, char ***tinfo)
{
  char **n = ILU_NIL;
  int count = 0, i;

  if (PyTuple_Check(o))
    {
      count = PyTuple_Size(o);
      for (i = 0;  i < count;  i++)
	{
	  if (!PyString_Check(PyTuple_GET_ITEM(o, i)))
	    goto err;
	}
      if ((n = (char **) ilu_malloc(sizeof(ilu_string) * (count + 1))) == ILU_NIL)
	goto err2;
      memset ((void *) n, 0, sizeof(ilu_string) * (count + 1));
      for (i = 0;  i < count;  i++)
	if ((n[i] = PyString_AsString (PyTuple_GET_ITEM(o, i))) == ILU_NIL)
	  goto err2;
      *tinfo = n;
      return 1;
    }
  else if (PyList_Check(o))
    {
      count = PyList_Size(o);
      for (i = 0;  i < count;  i++)
	{
	  if (!PyString_Check(PyList_GET_ITEM((PyListObject *) o, i)))
	    goto err;
	}
      if ((n = (char **) ilu_malloc(sizeof(ilu_string) * (count + 1))) == ILU_NIL)
	goto err2;
      memset ((void *) n, 0, sizeof(ilu_string) * (count + 1));
      for (i = 0;  i < count;  i++)
	if ((n[i] = PyString_AsString (PyList_GET_ITEM((PyListObject *) o, i))) == ILU_NIL)
	  goto err2;
      *tinfo = n;
      return 1;
    }

 err:
  PyErr_SetString(PyExc_TypeError,
		  "arg2 should be sequence of transport-info strings");
  goto errexit;

 err2:
  PyErr_NoMemory();
  goto errexit;

 errexit:
  if (n != ILU_NIL)
    {
      for (i = 0;  i < count;  i++)
	{
	  if (n[i] != ILU_NIL)
	    ilu_free(n[i]);
	}
      ilu_free(n);
    }
  return 0;
}

/* Forward declarations. */
static ilu_ObjectTable	createObjectTable(PyObject *objectOfIh);
static void		setObjectTableServer(ilu_ObjectTable self,
					     ilu_Server kserver);

static PyObject *
  ilumod_CreateServer(PyObject *self, PyObject *args)
{
  char *		serverId;
  ilu_ObjectTable	objTab;
  char **		trans;
  char *		proto;
  PyObject *	objectOfIh;
  ilu_Server	kserver;
  PyObject *	result;

  if (PyArg_Parse(args, ""))
    {
      serverId = 0;
      trans = 0;
      proto = 0;
      objectOfIh = Py_None;
    }
  else if (PyErr_Clear(), PyArg_Parse(args, "z", &serverId))
    {
      trans = 0;
      proto = 0;
      objectOfIh = Py_None;
    }
  else if (PyErr_Clear(), PyArg_ParseTuple(args, "zO&", &serverId, convTinfo, &trans))
    {
      proto = 0;
      objectOfIh = Py_None;
    }
  else if (PyErr_Clear(), PyArg_ParseTuple(args, "zO&z", &serverId, convTinfo, &trans, &proto))
    {
      objectOfIh = Py_None;
    }
  else
    {
      PyErr_Clear();
      if (!PyArg_ParseTuple(args, "zO&zO", &serverId, convTinfo, &trans, &proto,
		       &objectOfIh))
	return 0;
    }
  if (objectOfIh == Py_None)
    objTab = 0;
  else
    {
      if (!PyCallable_Check(objectOfIh))
	{
	  PyErr_SetString(PyExc_TypeError,
			  "arg4 should be callable");
	  return 0;
	}
      if ((objTab = createObjectTable(objectOfIh)) == 0)
	return 0;
    }

  if (serverId == 0)
    {
      CALL_KERNEL(ilupython_threaded_operation, serverId = ilu_InventID());
    }
  else if ((serverId = stringDup(serverId)) == 0)
    return 0;
  if (trans == 0)
    trans = getDefaultTransport();
  if (proto == 0)
    proto = getDefaultProtocol((const char **) trans);

  if ((kserver = createTrueServer(serverId, objTab, trans, proto, ILU_NIL)) == 0)
    {
      if (objTab)
	(*objTab->ot_free_self)(objTab);
      return 0;
    }
  if (objTab)
    setObjectTableServer(objTab, kserver);
  if ((result = ilusv_FromServer(kserver)) == 0)
    {
      CALL_KERNEL(ilupython_threaded_operation, ilu_BankServer(kserver));
    }
  else if (defaultTrueServer == 0)
    {
      defaultTrueServer = result;
      Py_INCREF(result);
    }
  return result;
}

static PyObject *
  createDefaultTrueServer(void)
{
  char *		serverId	= ilu_InventID();
  ilu_ObjectTable	objTab		= NULL;
  char **		transport	= getDefaultTransport();
  char *		protocol	= getDefaultProtocol((const char **) transport);
  ilu_Server	kserver;

  /*
    ilu_DebugPrintf ("creating default true server:");
    ilu_DebugPrintf (" id='%s', protocol='%s'\n",
    serverId, protocol);
    */
  if ((kserver = createTrueServer(serverId, objTab, transport,
				  protocol, ILU_NIL)) == 0)
    return 0;
  return ilusv_FromServer(kserver);
}

static PyObject *
  getDefaultTrueServer(void)
{
  /* should be safe because it takes place entirely within the global interpreter lock */
  if (defaultTrueServer == 0)
    defaultTrueServer = createDefaultTrueServer();
  return defaultTrueServer;
}

static PyObject *
  ilumod_DefaultServer(PyObject *self, PyObject *args)
{
  PyObject *	result;

  if (!PyArg_Parse(args, ""))
    return 0;
  if ((result = getDefaultTrueServer()) == 0)
    return 0;
  Py_INCREF(result);
  return result;
}

/********************************/

static IvObject *
  getInstVars(PyInstanceObject *inst)
{
  PyObject *	iv;

  if ((iv = PyDict_GetItemString(inst->in_dict, nameVarInstVars)) == 0)
    {
      ilu_Class	kclass;

      if ((iv = iv_New()) == 0)
	return 0;
      if ((kclass = getKernelClass(inst->in_class)) == 0)
	return 0;
      ((IvObject *) iv)->kclass = kclass;
      if (PyDict_SetItemString(inst->in_dict,
			       nameVarInstVars, iv) < 0)
	{
	  Py_DECREF(iv);
	  return 0;
	}
      Py_DECREF(iv); /* let dictionary have only reference */
    }
  else if (!iv_Check(iv))
    {
      char	msg[256];

      sprintf(msg, "instance's %s should be instvars",
	      nameVarInstVars);
      PyErr_SetString(PyExc_TypeError, msg);
      return 0;
    }
  return (IvObject *) iv;
}

static char *
  inventInstanceId(void)
{
  static ilu_cardinal	nextId;
  char		buffer[32];

  sprintf(buffer, "%lu", (unsigned long) (++nextId));
  return stringDup(buffer);
}

static char *
  getInstanceId(ilu_Class kclass, PyInstanceObject *inst)
{
  PyObject *ih;

  if (((ih = PyDict_GetItemString(inst->in_dict,
				  nameVarInstHandle)) == 0) || !PyString_Check(ih))
    {
      ih = PyString_FromString(inventInstanceId());
      if (PyDict_SetItemString(inst->in_dict, nameVarInstHandle, ih) < 0)
	{
	  Py_DECREF(ih);
	  return 0;
	}
    }
  return (PyString_AS_STRING((PyStringObject *) ih));
}

static void
  cleanupUninterestingObjects (ilu_private junk)
{
  if (gcList != ILU_NIL && PyList_Check(gcList))
    {
      Py_DECREF(gcList);
      gcList = ILU_NIL;
    }
  gcAlarmSet = ilu_FALSE;
}

static void
  setupGCCleanupAlarm (void)
{
  if (gcAlarmSet)
    return;

  if (gcAlarm == ILU_NIL)
    gcAlarm = ilu_CreateAlarm(); /* non-blocking (hopefully)! */
  
  _ilu_Assert(gcAlarm != ILU_NIL, "failed to create GC alarm");

#ifdef ILU_PYTHON_THREADS
  if (ilupython_threaded_operation)
    {
      CALL_KERNEL(ilupython_threaded_operation, ilu_SetAlarm(gcAlarm, ilu_FineTime_Add(ilu_FineTime_Now(), plus1Minute), bootstrapAlarm, (void *)&gc_alarm_rock));
    }
  else
#endif /* ILU_PYTHON_THREADS */
    {
      ilu_SetAlarm (gcAlarm,
		    ilu_FineTime_Add (ilu_FineTime_Now(), plus1Minute),
		    cleanupUninterestingObjects,
		    ILU_NIL);
    }
  gcAlarmSet = ilu_TRUE;
}

static void
  removeObjFromGCList (PyObject *list, PyObject *inst)
{
  int len = PyList_Size(list);
  int i;
  PyObject *emptyList = PyList_New(0);

  for (i = 0;  i < len;  i++)
    if (PyList_GetItem(list, i) == inst)
      {
	PyList_SetSlice (list, i, i+1, emptyList);
	len = PyList_Size(list);
	i -= 1;
      }

  Py_DECREF(emptyList);
}

static void
  addObjToGCList (PyObject *list, PyObject *inst)
{
  int len = PyList_Size(list);
  int i;

  for (i = 0;  i < len;  i++)
    if (PyList_GetItem(list, i) == inst)
      return;	/* already there */
  PyList_Append (list, inst);
  /*
    printf ("*** Added 0x%x to gc list\n", inst);
    */
}

static void
  trackKernelInterest (ilu_Object kobj, int vi)
{
  /* called when ILU kernel gains or loses interest in a
     collectible object */
  PyInstanceObject *inst = (PyInstanceObject *) ilu_GetLanguageSpecificObject(kobj, _ilupython_LangIndex); /* non-blocking */
  ilu_Class cl = ilu_ClassOfObject(kobj); /* non-blocking */

  if (inst != ILU_NIL && ((!ilu_TrueInstanceP(kobj)) || ilu_CollectibleP(cl))) /* non-blocking */
    {
      /*
	printf ("*** ILU kernel is %s in <%s> (0x%x, %s, %d)\n", vi ? "interested" : "not interested",
	ilu_SBHOfObject(kobj), inst, ilu_TrueInstanceP(kobj) ? "T" : "S", Py_COUNT(inst));
	*/
      if (vi)
	{
	  Py_INCREF(inst);
	  if (gcList != ILU_NIL && PyList_Check(gcList))
	    removeObjFromGCList (gcList, (PyObject *) inst);
	}
      else
	{
	  if (Py_COUNT(inst) < 2)
	    {
	      if (gcList == ILU_NIL)
		gcList = PyList_New(1);

	      addObjToGCList (gcList, (PyObject *) inst);
	      setupGCCleanupAlarm();
	    }
	  Py_DECREF(inst);
	}
    }
}

static ilu_Object
  createKernelObject(IvObject *iv, PyInstanceObject *inst)
{
  char *		instId;
  char *		kih;
  ilu_Object		kobj;

  if ((instId = getInstanceId(iv->kclass, inst)) == 0)
    return 0;
  kih = stringDup(instId);
  kobj = ilu_FindOrCreateTrueObject(kih, iv->kserver, iv->kclass,
				    (void *) inst); /* non-blocking */
  if (kobj == 0)
    PyErr_SetString(_ilupython_GeneralError, "FindOrCreateTrueObject failed");
  if (!ilu_CollectibleP(iv->kclass)) /* non-blocking */
    Py_INCREF(inst);	/* hold onto Python true objects */
  return kobj;
}

static PyObject *
  getInstanceSpecificServer(PyInstanceObject *inst)
{
  PyObject *	sv;

  if ((sv = PyObject_GetAttrString((PyObject *)inst, nameVarServer)) != 0)
    {
      if (ilusv_Check(sv))
	return sv;
    }
  else
    PyErr_Clear();
  return 0;
}

static IvObject *
  getInstVarsWithKobj(PyInstanceObject *inst)
{
  IvObject *	iv;

  if ((iv = getInstVars(inst)) == 0)
    return 0;
  if (iv->kserver == 0)
    {
      PyObject *	sv;

      if ((sv = getInstanceSpecificServer(inst)) == 0 &&
	  (sv = getDefaultTrueServer()) == 0)
	return 0;
      iv->kserver = ((IlusvObject *) sv)->kserver;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_EnterServer(iv->kserver, iv->kclass));
  if (iv->kobj == 0)
    {
      if ((iv->kobj = createKernelObject(iv, inst)) == 0)
	{
	  ilu_ExitServer(iv->kserver, iv->kclass);
	  return 0;
	}
    }
  return iv;
}

/********************************/

typedef struct
{
  PyObject *	objectOfIh;
  ilu_Server	kserver;
} OtRock;

static ilu_Object
  getKobjFromOtObject(PyInstanceObject *inst, ilu_Server kserver)
{
  IvObject *	iv;

  if (!PyInstance_Check(inst))
    {
      ilu_DebugPrintf ("object table: instance not returned\n");
      return 0;
    }

  if ((iv = getInstVars(inst)) == 0)
    {
      ilu_DebugPrintf ("object table: returned instance is bad");
      return 0;
    }
  if (iv->kserver == 0)
    iv->kserver = kserver;
  else if (iv->kserver != kserver)
    {
      ilu_DebugPrintf (
	      "object table: returned instance has different server");
      return 0;
    }
  if (iv->kobj == 0)
    iv->kobj = createKernelObject(iv, inst);
  return iv->kobj;
}

static ilu_Object
  callObjectOfIh(ilu_ObjectTable self, ilu_string ih)
{
  OtRock *	otr	= (OtRock *) self->ot_rock;
  PyObject *	args;
  PyObject *	inst;
  ilu_Object	kobj;

  if ((args = Py_BuildValue("(s)", ih)) == 0)
    {
      ilu_DebugPrintf ("object table: couldn't build args\n");
      return 0;
    }
  inst = PyEval_CallObject(otr->objectOfIh, args);
  Py_DECREF(args);
  if (inst == 0)
    {
      handleCalloutException("object table: function", ILU_NIL);
      return 0;
    }
  kobj = getKobjFromOtObject((PyInstanceObject *) inst, otr->kserver);
  Py_DECREF(inst);
  return kobj;
}

static void
  freeObjectTable(ilu_ObjectTable self)
{
  OtRock *	otr	= (OtRock *) self->ot_rock;

  Py_DECREF(otr->objectOfIh);
  PyMem_DEL(otr);
  PyMem_DEL(self);
}

static ilu_ObjectTable
  createObjectTable(PyObject *objectOfIh)
{
  ilu_ObjectTable	objTab;
  OtRock *	otr;

  if ((objTab = PyMem_NEW(ilu_ObjectTable_s, 1)) == 0)
    {
      (void) PyErr_NoMemory();
      return 0;
    }
  if ((otr = PyMem_NEW(OtRock, 1)) == 0)
    {
      PyMem_DEL(objTab);
      (void) PyErr_NoMemory();
      return 0;
    }

  Py_INCREF(objectOfIh);
  otr->objectOfIh = objectOfIh;
  otr->kserver = 0;

  objTab->ot_object_of_ih = callObjectOfIh;
  objTab->ot_free_self = freeObjectTable;
  objTab->ot_rock = (ilu_private) otr;
  return objTab;
}

static void
  setObjectTableServer(ilu_ObjectTable self, ilu_Server kserver)
{
  ((OtRock *) self->ot_rock)->kserver = kserver;
}

/********************************/

static PyObject *
  createSurrogateInstance(ilu_Object kobj)
{
  ilu_Class		kclass	= ilu_ClassOfObject(kobj);
  PyObject *		pycl;
  PyInstanceObject *	inst;
  IvObject *		iv;

  if ((pycl = findInClassRegistry(kclass)) == 0)
    {
      PyErr_SetString(_ilupython_GeneralError, "type not in registry");
      return 0;
    }
  if ((inst = (PyInstanceObject *) PYINSTANCE_NEW(pycl, ILU_NIL)) == 0)
    return 0;
  if ((iv = getInstVars(inst)) == 0)
    return 0;
  iv->kserver = ilu_ServerOfObject(kobj);
  iv->kobj = kobj;
  return (PyObject *) inst;
}

static PyObject *
  getPythonObject(ilu_Object kobj, ilu_Class kclass)
{
  ilu_Server	kserver	= ilu_ServerOfObject(kobj);
  PyObject *	pyobj;

  if ((pyobj = (PyObject *) ilu_GetLanguageSpecificObject(kobj, _ilupython_LangIndex)) == 0)
    {
      if ((pyobj = createSurrogateInstance(kobj)) == 0)
	return 0;
      CALL_KERNEL(ilupython_threaded_operation, ilu_RegisterLanguageSpecificObject(kobj, (void *) pyobj, _ilupython_LangIndex));
    }
  else
    Py_INCREF(pyobj);
  ilu_ExitServer(kserver, kclass); /* non-blocking */
  return pyobj;
}

static PyObject *
  getPythonTrueObject(ilu_Object kobj, ilu_Class kclass)
{
  ilu_Server	kserver	= ilu_ServerOfObject(kobj);
  PyObject *	pyobj;

  if ((pyobj = (PyObject *) ilu_GetLanguageSpecificObject(kobj, _ilupython_LangIndex)) == 0) /* non-blocking */
    PyErr_SetString(_ilupython_GeneralError,
		    "kernel object without true instance");
  else
    Py_INCREF(pyobj);
  ilu_ExitServer(kserver, kclass); /* non-blocking */
  return pyobj;
}

/********************************/

static int
  ensureGcClient(void)
{
  static ilu_Server	gcClientKserver;
  static ilu_Object	gcClientKobj;
  char *			gcServerId;

  if (gcClientKserver == 0)
    {
      char **	trans	= getDefaultTransport();
      char *	proto	= getDefaultProtocol((const char **) trans);
      char *	sid;

      CALL_KERNEL(ilupython_threaded_operation, sid = ilu_InventID());

      if ((gcClientKserver = createTrueServer(sid, ILU_NIL,
					      trans, proto, ILU_NIL)) == 0)
	return -1;
    }
  if (gcClientKobj == 0)
    {
      ilu_Class	kclass;
      char *		kih;

      EXIT_INTERPRETER(ilupython_threaded_operation);
      kclass = ilu_GetGcCallbackClass();
      kih = ilu_InventID();

      ilu_EnterServer(gcClientKserver, kclass);

      /*
       ** We're cheating here in giving something other than a
       ** Python instance as the languageSpecificObject (4th arg).
       ** But this should be okay as long as this gc object remains
       ** inaccessible (its SBH is not public knowledge).
       */
      gcClientKobj = ilu_FindOrCreateTrueObject(kih,
						gcClientKserver, kclass, (void *) &gcClientKobj);

      if (gcClientKobj != 0)
	ilu_SetGcClient(gcClientKobj);
      ilu_ExitServer(gcClientKserver, kclass);

      ENTER_INTERPRETER(ilupython_threaded_operation);

      if (gcClientKobj == 0)
	{
	  PyErr_SetString(_ilupython_GeneralError,
			  "failed creating gc client object");
	  return -1;
	}
    }
  return 0;
}

/********************************/

static PyObject *
  ilumod_ObjectOfSBH(PyObject *self, PyObject *args)
{
  PyClassObject *	pycl;
  char *		sbh;
  ilu_Class		kclass;
  ilu_Object		kobj;
  ilu_Server		server;
  ilu_Error		err = ILU_INIT_NO_ERR;
  ilu_ConsiderSbhResult result;

  PyErr_Clear();
  if (!PyArg_Parse(args, "(Os)", &pycl, &sbh))
    return 0;
  if (!PyClass_Check(pycl))
    {
      PyErr_SetString(PyExc_TypeError, "arg1 should be class");
      return 0;
    }
  if ((kclass = getKernelClass(pycl)) == 0)
    return 0;
  CALL_KERNEL(ilupython_threaded_operation, result = ilu_ConsiderSBH(sbh, &server, &err));
  if (result == ilucsr_err)
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &err));
      ILU_HANDLED(err);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, kobj = ilu_ObjectOfSBH(sbh, kclass, &err));

  if (kobj == 0)
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &err));
      ILU_HANDLED(err);
      return 0;
    }
  return getPythonObject(kobj, kclass);
}

static PyObject *
  ilumod_SBHOfObject(PyObject *self, PyObject *args)
{
  PyInstanceObject *	inst;
  char *		sbh;
  IvObject *		iv;

  if (!PyArg_Parse(args, "O", &inst))
    return 0;
  if (!PyInstance_Check(inst))
    {
      PyErr_SetString(PyExc_TypeError, "arg1 should be instance");
      return 0;
    }
  if ((iv = getInstVarsWithKobj(inst)) == 0)
    return 0;
  sbh = ilu_SBHOfObject(iv->kobj); /* non-blocking */
  ilu_ExitServer(iv->kserver, iv->kclass);
  if (sbh == 0)
    {
      PyErr_SetString(_ilupython_GeneralError, "ilu_SBHOfObject failed");
      return 0;
    }
  return PyString_FromString(sbh);
}

#ifdef IIOP_PROTOCOL

static PyObject *
  ilumod_IOROfObject(PyObject *self, PyObject *args)
{
  PyInstanceObject *	inst;
  char *		ior;
  IvObject *		iv;
  ilu_Error		err;
  PyObject *		ret;

  if (!PyArg_Parse(args, "O", &inst))
    return 0;
  if (!PyInstance_Check(inst))
    {
      PyErr_SetString(PyExc_TypeError, "arg1 should be instance");
      return 0;
    }
  if ((iv = getInstVarsWithKobj(inst)) == 0)
    return 0;
  ior = ilu_IOROfObject(iv->kobj, &err); /* non-blocking */
  if (iv->kobj != ILU_NIL)
    ilu_ExitServer(iv->kserver, iv->kclass); /* non-blocking */
  if (ILU_ERRNOK(err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &err));
      ILU_HANDLED(err);
      return 0;
    }
  ret = PyString_FromString(ior);
  ilu_free (ior);
  return ret;
}

#else

static PyObject *
  ilumod_IOROfObject(PyObject *self, PyObject *args)
{
  PyErr_SetString(_ilupython_GeneralError, "The CORBA IIOP protocol is not configured in.  No IOR support.");
  return 0;
}

#endif

static PyObject *
  ilumod_Delete (PyObject *self, PyObject *args)
{
  PyInstanceObject *	inst;
  IvObject *	iv;

  if (!PyArg_Parse(args, "O", &inst))
    return 0;
  if (!PyInstance_Check(inst))
    {
      PyErr_SetString(PyExc_TypeError, "arg1 should be instance");
      return 0;
    }
  if ((iv = getInstVars(inst)) == 0)
    return 0;
  if (Py_COUNT(inst) < 2)
    {
      if (gcList == ILU_NIL)
	gcList = PyList_New(1);

      addObjToGCList (gcList, (PyObject *) inst);
      setupGCCleanupAlarm();
    }
  Py_DECREF(inst);
  Py_INCREF(Py_None);
  return (Py_None);  
}

/********************************/

static PyObject *
  ilumod_PublishObject (PyObject *self, PyObject *args)
{
  PyInstanceObject *inst;
  IvObject *iv;

  if (!PyArg_Parse(args, "O", &inst))
    return 0;
  if (!PyInstance_Check(inst))
    {
      PyErr_SetString(PyExc_TypeError, "arg1 should be instance");
      return 0;
    }
  if ((iv = getInstVarsWithKobj(inst)) == 0)
    return 0;
  CALL_KERNEL(ilupython_threaded_operation, iv->publish_proof = ilu_PublishObject(iv->kobj));
  if (iv->publish_proof != NULL)
    {
      Py_INCREF(Py_None);
      return Py_None;
    }
  else
    {
      PyErr_SetString(_ilupython_GeneralError, "ilu_PublishObject failed");
      return 0;
    }
}

static PyObject *
  ilumod_WithdrawObject (PyObject *self, PyObject *args)
{
  PyInstanceObject *inst;
  IvObject *iv;
  ilu_boolean result;

  if (!PyArg_Parse(args, "O", &inst))
    return 0;

  if (!PyInstance_Check(inst))
    {
      PyErr_SetString(PyExc_TypeError, "arg1 should be instance");
      return 0;
    }
  if ((iv = getInstVars(inst)) == 0)
    return 0;
  if (iv->kserver == 0 || iv->kclass == 0)
    {
      PyErr_SetString(_ilupython_GeneralError, "null class or kernel server for object");
      return 0;
    }
  if (iv->publish_proof == NULL)
    {
      PyErr_SetString(_ilupython_GeneralError, "not published");
      return 0;
    }
  EXIT_INTERPRETER(ilupython_threaded_operation);
  ilu_EnterServer(iv->kserver, iv->kclass);
  result = ilu_WithdrawObject (iv->kobj, iv->publish_proof);
  ENTER_INTERPRETER(ilupython_threaded_operation);

  if (result)
    {
      free(iv->publish_proof);
      iv->publish_proof = NULL;
      Py_INCREF(Py_None);
      return Py_None;
    }
  else
    {
      PyErr_SetString(_ilupython_GeneralError, "ilu_WithdrawObject failed");
      return 0;
    }
}

static PyObject *
  ilumod_LookupObject (PyObject *self, PyObject *args)
{
  char *sid;
  char *ih;
  PyClassObject *pycl;
  ilu_Class kclass;
  ilu_Object kobj;
  IvObject *iv;
  PyObject *pobj = NULL;

  if (!PyArg_Parse(args, "(ssO)", &sid, &ih, &pycl))
    return 0;
  if (!PyClass_Check(pycl))
    {
      PyErr_SetString(PyExc_TypeError, "arg3 should be class");
      return 0;
    }
  if ((kclass = getKernelClass(pycl)) == 0)
    return 0;
  CALL_KERNEL(ilupython_threaded_operation, (void) ilu_ReLookupObject (sid, ih, kclass, &kobj));
  if (kobj == ILU_NIL)
    {
      Py_INCREF(Py_None);
      return Py_None;
    }
  else
    {
      pobj = getPythonObject (kobj, kclass);
      Py_INCREF(pobj);
      return pobj;
    }
}

static PyObject *
  ilumod_ParseSBH(PyObject *self, PyObject *args)
{
  char *	sbh;
  char *	ih		= 0;
  char *	sid		= 0;
  char *	mstid		= 0;
  char *	cinfo		= ILU_NIL;
  ilu_cardinal	cinfolen	= 0;
  ilu_Error	err		= ILU_INIT_NO_ERR;
  PyObject *	result;
  PyObject	*oih, *osid, *omstid, *ocinfo;

  if (!PyArg_Parse(args, "s", &sbh))
    return 0;
  if (!ilu_ParseSBH(sbh, &ih, &sid, &mstid, &cinfo, &cinfolen, &err)) /* non-blocking */
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &err));
      ILU_HANDLED(err);
      return 0;
    }
  ocinfo = PyString_FromStringAndSize (cinfo, cinfolen);
  result = Py_BuildValue("(sssO)", ih, sid, mstid, ocinfo);
  PyMem_XDEL(ih);
  PyMem_XDEL(sid);
  PyMem_XDEL(mstid);
  Py_DECREF(ocinfo);
  return result;
}

static PyObject *
  ilumod_PingObject (PyObject *self, PyObject *args)
{
  PyInstanceObject *inst;
  ilu_Error err = ILU_INIT_NO_ERR;
  ilu_boolean ok;
  IvObject *iv;
  ilu_Connection newconn;

  if (!PyArg_Parse(args, "O", &inst))
    return 0;
  if (!PyInstance_Check(inst))
    {
      PyErr_SetString(PyExc_TypeError, "arg1 should be instance");
      return 0;
    }
  if ((iv = getInstVarsWithKobj(inst)) == 0)
    return 0;

  /* now Inside(kobj->server, kobj->class) */

  CALL_KERNEL(ilupython_threaded_operation, err = ilu_DeltaHolds (iv->kobj, 1));

  ilu_ExitServer (iv->kserver, iv->kclass); /* non-blocking */
  ILU_ERR_SWITCH(err) {
    ILU_SUCCESS_CASE;
    ILU_ERR_CASE(GcRegFailed, v) {
      PyErr_SetString (_ilupython_GeneralError, "GC registration failed in ilu_DeltaHolds() call");
      return 0;
    }
    ILU_ERR_ELSE {
      PyErr_SetString (_ilupython_GeneralError, "Unknown error in ilu_DeltaHolds() call");
      return 0;
    }
  } ILU_ERR_ENDSWITCH;

  EXIT_INTERPRETER(ilupython_threaded_operation);
  ok = ilu_PingObject(iv->kobj, &newconn);
  _ilu_Assert(newconn == ILU_NIL || ilupython_threaded_operation, "non-NIL connection returned from ilu_PingObject in non-threaded runtime");

#ifdef ILU_PYTHON_THREADS
  if (newconn != ILU_NIL)
    ilupython_fork_thread(ilupython_watch_outgoing_connection, newconn);
#endif /* ILU_PYTHON_THREADS */

  ilu_EnterServer(iv->kserver, iv->kclass);
  err = ilu_DeltaHolds (iv->kobj, -1);
  ilu_ExitServer (iv->kserver, iv->kclass);
  ENTER_INTERPRETER(ilupython_threaded_operation);

  ILU_ERR_SWITCH(err) {
    ILU_SUCCESS_CASE;
    ILU_ERR_CASE(GcRegFailed, v) {
      PyErr_SetString (_ilupython_GeneralError, "GC registration failed in ilu_DeltaHolds() call");
      return 0;
    }
    ILU_ERR_ELSE {
      PyErr_SetString (_ilupython_GeneralError, "Unknown error in ilu_DeltaHolds() call");
      return 0;
    }
  } ILU_ERR_ENDSWITCH;
  if (ok)
    {
      Py_INCREF(Py_True);
      return Py_True;
    }
  else
    {
      Py_INCREF(Py_False);
      return Py_False;
    }
}

/********************************/

static PyObject *
  ilumod_CreateLoopHandle (PyObject *self, PyObject *args)
{
  PyObject *obj;

  if (!PyArg_Parse(args, ""))
    return 0;
  if ((obj = ilulp_Create()) == 0)
    return 0;
  Py_INCREF(obj);
  return obj;
}

static PyObject *
  ilumod_RunMainLoop(PyObject *self, PyObject *args)
{
  IlulpObject *indicator;

  if (!PyArg_Parse(args, "O", &indicator))
    return 0;
  if (!ilulp_Check(indicator))
    {
      PyErr_SetString(PyExc_TypeError, "arg1 should be ILU loop handle");
      return 0;
    }
  Py_INCREF(indicator);

#ifdef ILU_PYTHON_THREADS
  if (ilupython_threaded_operation)
    {
      while (!indicator->val)
	{
	  CALL_KERNEL(ilupython_threaded_operation, OS_SLEEP(30));
	}
    }
  else
#endif /* ILU_PYTHON_THREADS */
    ilu_RunMainLoop(&indicator->val);
  Py_DECREF(indicator);
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_ExitMainLoop(PyObject *self, PyObject *args)
{
  IlulpObject *indicator;

  if (!PyArg_Parse(args, "O", &indicator))
    return 0;
  if (!ilulp_Check(indicator))
    {
      PyErr_SetString(PyExc_TypeError, "arg1 should be ILU loop handle");
      return 0;
    }
  ilu_ExitMainLoop(&indicator->val);
  Py_INCREF(Py_None);
  return Py_None;
}

/********************************/

/* Hmm... should we restrict switching to threaded operation after a new mainloop has been
   set? */
static PyObject *
  callbackWithOneArg(PyObject *callback, PyObject *arg, char *what)
{
  PyObject *	argsTuple;
  PyObject *	result;

  if ((argsTuple = Py_BuildValue("(O)", arg)) == 0)
    {
      Py_DECREF(arg);
      return 0;
    }
  result = PyEval_CallObject(callback, argsTuple);
  Py_DECREF(argsTuple);
  if (result == 0)
    {
      char	culprit[100];

      sprintf(culprit, "%s callback", what);
      handleCalloutException(culprit, ILU_NIL);
    }
  return result;
}

static PyObject *
  callbackWithTwoArgs(PyObject *callback, PyObject *arg1, PyObject *arg2, char *what)
{
  PyObject *	argsTuple;
  PyObject *	result;

  if ((argsTuple = Py_BuildValue("(OO)", arg1, arg2)) == 0)
    {
      Py_DECREF(arg1);
      Py_DECREF(arg2);
      return 0;
    }
  result = PyEval_CallObject(callback, argsTuple);
  Py_DECREF(argsTuple);
  if (result == 0)
    {
      char	culprit[100];

      sprintf(culprit, "%s callback", what);
      handleCalloutException(culprit, ILU_NIL);
    }
  return result;
}

static ilu_boolean
  registerHandler(PyObject *callback, int fd, ilu_IOHandler handler,
		  ilu_private rock, char *what)
{
  PyObject *	arg;
  PyObject *	result;
  ilu_boolean	stat;

  if ((arg = iohc_New(fd, handler, rock)) == 0 ||
      (result = callbackWithOneArg(callback, arg, what)) == 0)
    return ilu_FALSE;
  stat = BOOLEAN((result != Py_None));
  Py_DECREF(result);
  return stat;
}

static ilu_boolean
  cancelHandler(PyObject *callback, int fd, char *what)
{
  PyObject *	arg;
  PyObject *	result;
  ilu_boolean	stat;

  if ((arg = PyInt_FromLong(fd)) == 0 ||
      (result = callbackWithOneArg(callback, arg, what)) == 0)
    return ilu_FALSE;
  stat = BOOLEAN((result != Py_None));
  Py_DECREF(result);
  return stat;
}

static PyObject *cbDoEvent;
static PyObject *cbRegInp;
static PyObject *cbCanInp;
static PyObject *cbRegOut;
static PyObject *cbCanOut;
static PyObject *cbCreateAlarm;
static PyObject *cbSetAlarm;
static PyObject *cbCanAlarm;

static void
  Run(int *stop)
{
  PyObject *	argsTuple;
  PyObject *	result;

  if ((argsTuple = PyTuple_New(0)) == 0)
    {
      /* error */
      return;
    }

  *stop = 0;

  while (!(*stop))
    {
      if (PyOS_InterruptOccurred ())
	{
	  PyErr_SetNone (PyExc_KeyboardInterrupt);
	  *stop = 1;
	}

      result = PyEval_CallObject(cbDoEvent, argsTuple);
      if (result == 0)
	handleCalloutException("run main loop", ILU_NIL);
      else
	Py_DECREF(result);
    }
  Py_DECREF(argsTuple);
  return;
}

static void
  Exit(int *stop)
{
  *stop = 1;
}

struct ihcallback_struct {
  int fd;
  PyObject *callback;
  struct ihcallback_struct *next;
};

struct ihcallback_struct *ihandlers = ILU_NIL;

static void
  ActualInputHandler (int fd, ilu_private rock)
{
  PyObject *argsTuple, *result;
  struct ihcallback_struct *ihs = (struct ihcallback_struct *) rock;

  if ((argsTuple = PyTuple_New(0)) == 0)
    {
      /* error */
      return;
    }
  result = PyEval_CallObject(ihs->callback, argsTuple);
  Py_DECREF(argsTuple);
  if (result == 0)
    handleCalloutException("input handler callback", ILU_NIL);
  else
    Py_DECREF(result);
}

static PyObject *
  ilumod_RegisterInputHandler (PyObject *self, PyObject *args)
{
  PyObject *handler;
  PyObject *fileobj;
  int fd;
  struct ihcallback_struct *ihs, *last, *next;

  if (!PyArg_Parse(args, "(OO)", &fileobj, &handler))
    return 0;
  if (PyFile_Check(fileobj))
    {
      FILE *file;
      file = PyFile_AsFile (fileobj);
      fd = fileno(file);	/* fileno is POSIX 8.2.1.1 -- should be declared in stdio.h */
    }
  else if (PyInt_Check(fileobj))
    {
      fd = (int) PyInt_AsLong(fileobj);
    }
  else
    {
      PyErr_SetString(PyExc_TypeError, "arg1 should be open readable file or file descriptor (small int)");
      return 0;
    }
  if ((handler != Py_None) && !PyCallable_Check(handler))
    {
      PyErr_SetString(PyExc_TypeError, "arg2 should be callback function or None");
      return 0;
    }
  for (last = ILU_NIL, ihs = ihandlers;  ihs != ILU_NIL;  ihs = next)
    {
      if (ihs->fd == fd)
	{
	  if (handler == Py_None)	/* doing an UNregister */
	    {
	      if (last == ILU_NIL)	/* take `ihs' off the list */
		ihandlers = ihs->next;
	      else
		last->next = ihs->next;
	      next = ihs->next;		/* advance `next' pointer */
	      /* don't advance last */
	      ilu_UnregisterInputSource (fd);
	      Py_DECREF(ihs->callback);	/* this should del the callback */
	      ihs->next = ILU_NIL;	/* just for any conservative GC */
	      ilu_free(ihs);		/* ok, now get rid of this struct */
	    }
	  else
	    {
	      Py_DECREF(ihs->callback);	/* remove old callback proc */
	      ihs->callback = handler;	/* add new one */
	      Py_INCREF(ihs->callback);	/* and hold onto it */
	      last = ihs;		/* advance past this link */
	      next = ihs->next;
	    }
	}
      else
	{
	  last = ihs;
	  next = ihs->next;
	}
    }
  if (ihs == ILU_NIL && handler != Py_None)
    {
      if ((ihs = (struct ihcallback_struct *) ilu_malloc(sizeof(struct ihcallback_struct))) == ILU_NIL)
	{
	  (void) PyErr_NoMemory();
	  return 0;
	}
      ihs->fd = fd;
      ihs->callback = handler;
      Py_INCREF(ihs->callback);
      ihs->next = ihandlers;
      ihandlers = ihs;
      ilu_RegisterInputSource (fd, ActualInputHandler, ihs);
    }  
  Py_INCREF(Py_None);
  return (Py_None);
}

static ilu_boolean
  registerInputHandler(int fd, ilu_IOHandler handler, ilu_private rock)
{
  return registerHandler(cbRegInp, fd, handler, rock, "reg input");
}

static ilu_boolean
  cancelInputHandler(int fd)
{
  return cancelHandler(cbCanInp, fd, "cancel input");
}

static ilu_boolean
  registerOutputHandler(int fd, ilu_IOHandler handler, ilu_private rock)
{
  return registerHandler(cbRegOut, fd, handler, rock, "reg output");
}

static ilu_boolean
  cancelOutputHandler(int fd)
{
  return cancelHandler(cbCanOut, fd, "cancel output");
}

/* Main Invariant holds; L2 otherwise unconstrained */
typedef void    (*AlarmProc) (ilu_private rock);

static ilu_refany
  CreateAlarm (void)
{
  PyObject *	argsTuple;
  PyObject *	result;

  if ((argsTuple = PyTuple_New(0)) == 0)
    {
      /* error */
      return ILU_NIL;
    }
  result = PyEval_CallObject(cbCreateAlarm, argsTuple);
  Py_DECREF(argsTuple);
  if (result == 0)
    {
      handleCalloutException("create alarm callback", ILU_NIL);
      return 0;
    }
  else
    return ((ilu_refany) result);
}

static void
SetAlarm(ilu_refany alarm, ilu_FineTime t,
	 AlarmProc proc, ilu_private rock)
{
  PyObject *	arg;
  PyObject *	result;

  if ((arg = thc_New(t, proc, rock)) == 0 ||
      (result = callbackWithTwoArgs(cbSetAlarm, (struct _object *) alarm, arg, "set alarm")) == 0)
    return;
  Py_DECREF(result);
}

static void
UnsetAlarm(ilu_refany alarm)
{
  PyObject *	result;

  if ((result = callbackWithOneArg(cbCanAlarm, (struct _object *) alarm, "cancel alarm")) == 0)
    return;
  Py_DECREF(result);
}

static ilu_MainLoop synth = {
  Run, Exit,
  registerInputHandler, cancelInputHandler,
  registerOutputHandler, cancelOutputHandler,
  CreateAlarm, SetAlarm, UnsetAlarm};

static PyObject *
  ilumod_SetMainLoop(PyObject *self, PyObject *args)
{
  PyObject *	DoEvent;
  PyObject *	regInp;
  PyObject *	canInp;
  PyObject *	regOut;
  PyObject *	canOut;
  PyObject *	createAlarm;
  PyObject *	setAlarm;
  PyObject *	cancelAlarm;
  static ilu_boolean initialized = ilu_FALSE;

  if (!PyArg_Parse(args, "(OOOOOOOO)", &DoEvent, &regInp, &canInp, &regOut, &canOut,
		   &createAlarm, &setAlarm, &cancelAlarm))
    return 0;
  if (!PyCallable_Check(DoEvent) ||
      !PyCallable_Check(regInp) || !PyCallable_Check(canInp) ||
      !PyCallable_Check(regOut) || !PyCallable_Check(canOut) ||
      !PyCallable_Check(createAlarm) || !PyCallable_Check(setAlarm) || !PyCallable_Check(cancelAlarm))
    {
      PyErr_SetString(PyExc_TypeError,
		      "all args should be callable");
      return 0;
    }
  cbDoEvent = DoEvent;
  cbRegInp = regInp;
  cbCanInp = canInp;
  cbRegOut = regOut;
  cbCanOut = canOut;
  cbCreateAlarm = createAlarm;
  cbSetAlarm = setAlarm;
  cbCanAlarm = cancelAlarm;
  if (!initialized)
    {
      ilu_SetMainLoop (&synth);
      initialized = ilu_TRUE;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

/********************************/

static PyObject *
  longRealFromVector(
		     int (*size)(PyObject *),
		     PyObject *(*getItem)(PyObject *, int),
		     PyObject *seq
		     )
{
  ilu_longreal	lr;
  int		i;

  if ((*size)(seq) != ilulr_NBytesInLongReal)
    {
      PyErr_SetString(PyExc_TypeError, "arg1 has improper length");
      return 0;
    }
  for (i = 0; i < ilulr_NBytesInLongReal; i++)
    {
      PyObject *	item	= (*getItem)(seq, i);
      long		itemVal;

      if (!PyInt_Check(item))
	{
	  PyErr_SetString(PyExc_TypeError,
			  "item should be integer");
	  return 0;
	}
      itemVal = PyInt_AsLong(item);
      if ((unsigned char) itemVal != itemVal)
	{
	  PyErr_SetString(PyExc_ValueError,
			  "item exceeds range of byte");
	  return 0;
	}
      ((unsigned char *)&lr)[i] = itemVal;
    }
  return ilulr_FromLongReal(lr);
}

static PyObject *
  ilumod_LongReal(PyObject *self, PyObject *args)
{
  PyObject *	v;

  if (!PyArg_Parse(args, "O", &v))
    return 0;
  if (PyFloat_Check(v))
    return ilulr_FromDouble(PyFloat_AsDouble(v));
  else if (PyInt_Check(v))
    return ilulr_FromDouble((double) PyInt_AsLong(v));
  else if (PyList_Check(v))
    return longRealFromVector(PyList_Size, PyList_GetItem, v);
  else if (PyTuple_Check(v))
    return longRealFromVector(PyTuple_Size, PyTuple_GetItem, v);
  PyErr_SetString(PyExc_TypeError,
		  "arg1 should be int, float, or sequence of bytes");
  return 0;
}

/********************************/

static PyObject *
  ilumod_FineTime(PyObject *self, PyObject *args)
{
  PyObject *	value;
  ilu_FineTime	ft;

  if (!PyArg_Parse(args, "O", &value))
    return 0;
  if (PyFloat_Check(value))
    ft = ilu_FineTime_FromDouble(PyFloat_AsDouble(value)); /* non-blocking */
  else if (PyInt_Check(value))
    {
      ft.ft_s = PyInt_AsLong(value);
      ft.ft_t = 0;
    }
  else
    {
      PyErr_SetString(PyExc_TypeError, "arg1 should be int or float");
      return 0;
    }
  return iluft_FromFineTime(ft);
}

static PyObject *
  ilumod_FineTime_Now(PyObject *self, PyObject *args)
{
  ilu_FineTime	ft;

  if (!PyArg_Parse(args, ""))
    return 0;
  ft = ilu_FineTime_Now(); /* non-blocking */
  return iluft_FromFineTime(ft);
}

static PyObject *
  ilumod_CreateAlarm(PyObject *self, PyObject *args)
{
  if (!PyArg_Parse(args, ""))
    return 0;
  return ilual_New();
}

/********************************/

static char *
  protoExceptionImage(ilu_ProtocolException pe)
{
  switch (pe)
    {
    case ilu_ProtocolException_Success:
      return "success";
    case ilu_ProtocolException_NoSuchClassAtServer:
      return "no such class at server";
    case ilu_ProtocolException_ClassVersionMismatch:
      return "class version mismatch";
    case ilu_ProtocolException_NoSuchMethodOnClass:
      return "no such method on class";
    case ilu_ProtocolException_GarbageArguments:
      return "garbage arguments";
    case ilu_ProtocolException_Unknown:
      return "unknown";
    case ilu_ProtocolException_LostConnection:
      return "lost connection";
    case ilu_ProtocolException_RequestRejected:
      return "request rejected";
    case ilu_ProtocolException_RequestTimeout:
      return "request timeout";
    default:
      return "?";
    }
}

static void
  protoErr(ilu_ProtocolException pe)
{
  PyErr_SetString(ProtocolError, protoExceptionImage(pe));
}

/********************************/

static PyObject *
  ilumod_BeginCall(PyObject *self, PyObject *args)
{
  PyInstanceObject *	inst;
  ilu_Method		meth;
  IvObject *		iv;
  ilu_Call		call;
  ilu_cardinal		methodIndex = 0;
  IluclObject *		cl;
  ilu_Error		err = ILU_INIT_NO_ERR;
  ilu_Connection	newconn;
  ilu_boolean           result;

  if (!PyArg_Parse(args, "(OOi)", &inst, &cl, &methodIndex))
    return 0;
  if (!PyInstance_Check(inst))
    {
      PyErr_SetString(PyExc_TypeError, "arg1 should be instance");
      return 0;
    }
  if ((iv = getInstVars(inst)) == 0)
    return 0;
  if (iv->kserver == 0)
    {
      PyErr_SetString(_ilupython_GeneralError, "object has no server");
      return 0;
    }
  if (!ilucl_Check(cl))
    {
      PyErr_SetString(PyExc_TypeError, "arg2 should be ilu_Class");
      return 0;
    }
  meth = ilu_MethodNOfClass (cl->c, methodIndex - 1); /* non-blocking */
  if (meth == ILU_NIL)
    {
      PyErr_SetString(PyExc_TypeError,
		      "arg3 (method index) out of bounds");
      return 0;
    }
  if ((call = (ilu_Call) ilu_malloc(sizeof(*call))) == ILU_NIL)
    {
      PyErr_NoMemory();
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, result = ilu_StartCall(call, iv->kserver, cl->c, meth, _ilupython_LangIndex, ILU_NIL, &newconn, &err));

  if (!result)
    {
      char errbuf[1000];

      ilu_free(call);
      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &err));
      ILU_HANDLED(err);
      return 0;
    }
  _ilu_Assert(newconn == ILU_NIL || ilupython_threaded_operation, "non-NIL connection returned from ilu_StartCall in non-threaded runtime");
#ifdef ILU_PYTHON_THREADS
  if (newconn != ILU_NIL)
    ilupython_fork_thread(ilupython_watch_outgoing_connection, newconn);
#endif /* ILU_PYTHON_THREADS */
  return iluca_FromCall(call);
}

static PyObject *
  ilumod_FinishCall(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_FinishCall(ca->call, &ca->err));
  ilu_free(ca->call);
  if (! ILU_ERROK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_BeginRequest(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  IluclObject *	cl;
  int		methodIndex;
  long		argSize;
  ilu_Method	meth;
  ilu_boolean   result;

  if (!PyArg_Parse(args, "(Ol)", &ca, &argSize))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, result = ilu_StartRequest(ca->call, argSize, &ca->err));

  if (result == ilu_FALSE)
    {
      char errbuf[1000];
      
      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_FinishRequest(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  ilu_boolean result;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, result = ilu_FinishRequest(ca->call, &ca->err));

  if (result == ilu_FALSE)
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));

      return 0;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_GetReply(PyObject *self, PyObject *args)
{
  IlucaObject *		ca;
  ilu_ProtocolException	pe;
  ilu_cardinal		scode;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, pe = ilu_GetReply(ca->call, &scode, &ca->err));

  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));

      return 0;
    }
  if (pe != ilu_ProtocolException_Success)
    {
      protoErr(pe);
      return 0;
    }
  return PyInt_FromLong(scode);
}

static PyObject *
  ilumod_ReplyRead(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  ilu_boolean result;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, result = ilu_ReplyRead(ca->call, &ca->err));

  if (result == ilu_FALSE)
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));

      return 0;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_ExceptionName(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  IluclObject *	cl;
  int		evalue;
  ilu_Method	meth;
  PyObject *	result;
  ilu_cardinal	ecount;
  ilu_Exception*evec;

  if (!PyArg_Parse(args, "(OOi)", &ca, &cl, &evalue))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (!ilucl_Check(cl))
    {
      PyErr_SetString(PyExc_TypeError, "arg2 should be ilu_Class");
      return 0;
    }
  meth = ilu_MethodOfCall(ca->call); /* non-blocking */
  ilu_DataOfMethod (meth, ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL,
		    &ecount, &evec, ILU_NIL); /* non-blocking */
  if (evalue < 0 || ecount < evalue)
    {
      PyErr_SetString(_ilupython_GeneralError, "unknown exception");
      return 0;
    }
  result = PyString_FromString(evec[evalue - 1]);
  Py_INCREF(result);
  return result;
}

/********************************/

static PyObject *
  ilumod_GetSingleton(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  ilu_Object	kobj;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, kobj = ilu_GetCallSingleton(ca->call, &ca->err));
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));

      return 0;
    }
  else if (kobj == 0)
    {
      PyErr_SetString(_ilupython_GeneralError, "couldn't get singleton");
      return 0;
    }
  return getPythonTrueObject(kobj, ilu_ClassOfObject(kobj));
}

struct passport_list {
  ilu_Passport 		passport;
  PyObject *		python_version;
  struct passport_list *next;
};

static struct passport_list *all_passports = ILU_NIL;

/* before: conn not registered */
/* after:  conn not registered iff protocol concurrent */
static PyObject *
  ilumod_RequestRead(PyObject *self, PyObject *args)
{
  IlucaObject *		ca;
  ilu_Passport		pp;
  struct passport_list *	ppe;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  if (!ilupython_threaded_operation)
    {
      /* assumes single threading */
      ppe = (struct passport_list *) ilu_must_malloc(sizeof(struct passport_list));
      ppe->passport = ilu_CallerPassportOfCall(ca->call);
      ppe->python_version = Py_None;
      ppe->next = all_passports;
      all_passports = ppe;
    }
  else
    { /* in a multi-threaded runtime, this is handled in callSkeleton */ }

  CALL_KERNEL(ilupython_threaded_operation, ilu_RequestRead(ca->call, &ca->err));

  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }

  if (!ilupython_threaded_operation && ConcurrentConn(ca))
    enableRequestsOnCallConn(ca);

  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
ilumod_CallerIdentity (PyObject *self, PyObject *args)
{
  PyObject *local_passport = Py_None;
  PyObject **py_passport;
  ilu_Passport passport;

#ifdef ILU_PYTHON_THREADS
  if (ilupython_threaded_operation)
    {
      IlucaObject *ca = (IlucaObject *)ilupython_thread_current_call();
      
      /*py_passport = ((PyObject **)&ca->call->ca_private);*/
      /* since the ilu_Call object is disposed of independently of the
	 iluca object, we can't stash it in the private field */
      py_passport = &local_passport;
      passport = ilu_CallerPassportOfCall(ca->call);
    }
  else
#endif /* ILU_PYTHON_THREADS */
    {
      if (all_passports == ILU_NIL)
	{
	  PyErr_SetString (_ilupython_GeneralError, "no outstanding ILU calls");
	  return 0;
	}
      else
	{
	  py_passport = &all_passports->python_version;
	  passport = all_passports->passport;
	}
    }
    
  if (*py_passport == Py_None)
    {
      ilu_IdentityInfo ident;

      if ((ident = ilu_FindIdentity (passport, ilu_ConnectionIdentity)) != ILU_NIL) /* non-blocking */
	{
	  PyObject *pp;

	  /* now create passport, and put default identity in it */

	  if ((pp = *py_passport) == Py_None)
	    {
	      pp = PyDict_New();
	      if (pp == ILU_NIL)
		{
		  PyErr_SetString (_ilupython_GeneralError, "Can't create passport object");
		  return (0);
		}
	      *py_passport = pp;
	    }
	  PyDict_SetItemString (pp, nameConnectionIdentityInfo, PyString_FromString((ilu_string) (ident->ii_info)));
	};

#ifdef SUNRPC_PROTOCOL
      if ((ident = ilu_FindIdentity (passport, ilu_SunRPCAuthUnixIdentity)) != ILU_NIL) /* non-blocking */
	{
	  PyObject *pp;
	  PyObject *auth;
	  PyObject *groups;
	  int groupcount = 0;
	  int i;

	  /* create SunRPC identity record */

	  auth = PyDict_New ();
	  if (auth == ILU_NIL)
	    {
	      PyErr_SetString (_ilupython_GeneralError, "Can't create dictionary object for SunRPCAuthUnix identity info");
	      return 0;
	    }
	  PyDict_SetItemString (auth, nameSunRPCAuthUID,
				PyInt_FromLong((long) ((ilu_SunRPCAuthUnixIdentityInfo)(ident->ii_info))->ii_UID));
	  PyDict_SetItemString (auth, nameSunRPCAuthGID,
				PyInt_FromLong((long) ((ilu_SunRPCAuthUnixIdentityInfo)(ident->ii_info))->ii_GID));
	  PyDict_SetItemString (auth, nameSunRPCAuthHostname,
				PyString_FromString(((ilu_SunRPCAuthUnixIdentityInfo)(ident->ii_info))->ii_hostname));
	  for (i = 0, groupcount = 0;  i < (int)((ilu_SunRPCAuthUnixIdentityInfo)(ident->ii_info))->ii_ngids;  i += 1)
	    if (((ilu_SunRPCAuthUnixIdentityInfo)(ident->ii_info))->ii_gids[i] != 0)
	      groupcount += 1;
	  if (groupcount == 0)
	    groups = Py_None;
	  else
	    {
	      groups = PyTuple_New(groupcount);
	      if (groups == 0)
		{
		  PyErr_SetString (_ilupython_GeneralError, "Couldn't create tuple of user's groups");
		  Py_DECREF(auth);
		  return 0;
		}
	      for (i = 0, groupcount = 0;  i < (int) ((ilu_SunRPCAuthUnixIdentityInfo)(ident->ii_info))->ii_ngids;  i++)
		{
		  if (((ilu_SunRPCAuthUnixIdentityInfo)(ident->ii_info))->ii_gids[i] != 0)
		    {
		      PyTuple_SetItem (groups, groupcount,
				       PyInt_FromLong((long) ((ilu_SunRPCAuthUnixIdentityInfo)(ident->ii_info))->ii_gids[i]));
		      groupcount += 1;
		    }
		}
	    }
	  PyDict_SetItemString (auth, nameSunRPCAuthGroups, groups);

	  /* now create passport, and put Sun RPC identity into it */

	  if ((pp = *py_passport) == Py_None)
	    {
	      pp = PyDict_New();
	      if (pp == ILU_NIL)
		{
		  PyErr_SetString (_ilupython_GeneralError, "Can't create passport object");
		  Py_DECREF(auth);
		  return (0);
		}
	      *py_passport = pp;
	    }
	  PyDict_SetItemString (pp, nameSunRPCAuth, auth);
	};
#endif /* SUNRPC_PROTOCOL */

#ifdef SECURE_TRANSPORT
      if ((ident = ilu_FindIdentity (passport, ilu_GSSIdentity)) != ILU_NIL) /* non-blocking */
	{
	  PyObject *pp;
	  PyObject *gssid;

	  /* create GSS identity record */

	  gssid = ilugi_FromIdentityInfo (ident);
	  if (gssid == ILU_NIL)
	    {
	      PyErr_SetString (_ilupython_GeneralError, "Can't create GSSIdentityInfo object");
	      return 0;
	    }

	  /* now create passport, and put GSS identity into it */

	  if ((pp = *py_passport) == Py_None)
	    {
	      pp = PyDict_New();
	      if (pp == ILU_NIL)
		{
		  PyErr_SetString (_ilupython_GeneralError, "Can't create passport object");
		  Py_DECREF(gssid);
		  return (0);
		}
	      *py_passport = pp;
	    }
	  PyDict_SetItemString (pp, nameGSSIdentityInfo, gssid);
	};
#endif /* SECURE_TRANSPORT */

    }

  Py_INCREF(*py_passport);
  return (*py_passport);
}

static void PopPassport (ilu_Call call)
{
  struct passport_list *e = all_passports;
  if (e != NULL)
    {
      all_passports = e->next;
      if (e->python_version != Py_None)
	Py_DECREF(e->python_version);
      e->python_version = 0;
      ilu_free(e);
    }
}

/* before: conn registered iff protocol concurrent */
/* after:  conn registered */
static PyObject *
  ilumod_NoReply(PyObject *self, PyObject *args)
{
  IlucaObject *		ca;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (!ilupython_threaded_operation)
    PopPassport (ca->call);

  CALL_KERNEL(ilupython_threaded_operation, ilu_NoReply(ca->call, &ca->err));

  if (!ilupython_threaded_operation && !ConcurrentConn(ca))
    enableRequestsOnCallConn(ca);

  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

/* before: conn registered iff protocol concurrent
 ** after:  normal return   -> conn not registered,
 **	   _ilupython_GeneralError    -> conn registered, or
 **	   other exception -> conn registered iff protocol concurrent
 */
static PyObject *
  ilumod_BeginSizingReply(PyObject *self, PyObject *args)
{
  IlucaObject *		ca;
  int			raises;
  long			argSize;

  if (!PyArg_Parse(args, "(Oi)", &ca, &raises))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (!ilupython_threaded_operation && ConcurrentConn(ca))
    disableRequestsOnCallConn(ca);

  CALL_KERNEL(ilupython_threaded_operation, argSize = ilu_BeginSizingReply(ca->call, BOOLEAN(raises), &ca->err)); 

  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      if (!ilupython_threaded_operation)
	enableRequestsOnCallConn(ca);
      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  return PyInt_FromLong(argSize);
}

/* before: conn not registered
 ** after:  normal return   -> conn not registered,
 **	   _ilupython_GeneralError    -> conn registered, or
 **	   other exception -> conn registered iff protocol concurrent
 */
static PyObject *
  ilumod_BeginReply(PyObject *self, PyObject *args)
{
  IlucaObject *		ca;
  int			raises;
  long			argSize;
  ilu_boolean           result;

  if (!PyArg_Parse(args, "(Oil)", &ca, &raises, &argSize))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (!ilupython_threaded_operation)
    PopPassport (ca->call);
  CALL_KERNEL(ilupython_threaded_operation, result = ilu_BeginReply(ca->call, BOOLEAN(raises), argSize, &ca->err));
  if (result == ilu_FALSE)
    {
      char errbuf[1000];

      if (!ilupython_threaded_operation)
	enableRequestsOnCallConn(ca);
      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

/* before: conn not registered
 ** after:  normal return -> conn registered
 **	   exception     -> conn not registered
 */
static PyObject *
  ilumod_FinishReply(PyObject *self, PyObject *args)
{
  IlucaObject *		ca;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_FinishReply(ca->call, &ca->err));
  if (!ilupython_threaded_operation)
    enableRequestsOnCallConn(ca);
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

/* before: conn registered iff protocol concurrent
 ** after:  normal return   -> conn not registered
 **	   _ilupython_GeneralError    -> conn registered
 **	   other exception -> conn registered iff protocol concurrent
 */
static PyObject *
  ilumod_BeginSizingException(PyObject *self, PyObject *args)
{
  IlucaObject *		ca;
  int			ecode;
  long			argSize;

  if (!PyArg_Parse(args, "(Oi)", &ca, &ecode))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (!ilupython_threaded_operation && ConcurrentConn(ca))
    disableRequestsOnCallConn(ca);

  CALL_KERNEL(ilupython_threaded_operation, argSize = ilu_BeginSizingException(ca->call, ecode, &ca->err));

  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      if (!ilupython_threaded_operation && !ConcurrentConn(ca))
	enableRequestsOnCallConn(ca);
      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }

  return PyInt_FromLong(argSize);
}

/* before: conn not registered
 ** after:  normal return   -> conn not registered
 **	   _ilupython_GeneralError    -> conn registered
 **	   other exception -> conn registered iff protocol concurrent
 */
static PyObject *
  ilumod_BeginException(PyObject *self, PyObject *args)
{
  IlucaObject *		ca;
  int			ecode;
  long			argSize;
  ilu_boolean           result;

  if (!PyArg_Parse(args, "(Oil)", &ca, &ecode, &argSize))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (!ilupython_threaded_operation)
    PopPassport (ca->call);

  CALL_KERNEL(ilupython_threaded_operation, result = ilu_BeginException(ca->call, ecode, argSize, &ca->err));

  if (result == ilu_FALSE)
    {
      char errbuf[1000];

      if (!ilupython_threaded_operation && !ConcurrentConn(ca))
	enableRequestsOnCallConn(ca);
      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

/* before: conn not registered
 ** after:  normal return -> conn registered
 **	   exception     -> conn not registered
 */
static PyObject *
  ilumod_FinishException(PyObject *self, PyObject *args)
{
  IlucaObject *		ca;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (!ilupython_threaded_operation)
    enableRequestsOnCallConn(ca);
  if (!ilu_FinishException(ca->call, &ca->err)) /* non-blocking */
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

/* before: conn not registered iff protocol concurrent */
/* after:  conn registered (except for bad ca TypeError exception) */
static PyObject *
  ilumod_UnexpectedException(PyObject *self, PyObject *args)
{
  PyObject *		excType	= PySys_GetObject("exc_type");
  PyObject *		excVal	= PySys_GetObject("exc_value");
  IlucaObject *		ca;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  ILU_ERR_CONS0(unknown, &ca->err, ILU_NIL);
  PyErr_SetObject(excType, excVal);
  return 0;
}


/************* begin marshalling/unmarshalling routines *************/

static PyObject *
  ilumod_SizeOfObjectID(PyObject *self, PyObject *args)
{
  IluclObject *		icl;
  IlucaObject *		ca;
  PyObject *		inst;
  int			isDiscrim;
  PyClassObject *	pycl;
  IvObject *		iv;
  ilu_Object		kobj;
  ilu_Class		kclass;
  long			size;

  if (!PyArg_Parse(args, "(OOiO)", &ca, &inst, &isDiscrim, &pycl))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (!PyClass_Check(pycl))
    {
      PyErr_SetString(PyExc_TypeError, "arg4 should be class");
      return 0;
    }
  if ((kclass = getKernelClass(pycl)) == 0)
    return 0;
  if ((icl = getPythonClass(pycl)) == 0)
    return 0;
  if (!(PyInstance_Check(inst) ||
	(inst == Py_None && !isDiscrim && icl->optional)))
    {
      PyErr_SetString(PyExc_TypeError, "arg2 should be instance");
      return 0;
    }
  if (inst != Py_None)
    {
      if ((iv = getInstVarsWithKobj((PyInstanceObject *) inst)) == 0)
	return 0;
      kobj = iv->kobj;
    }
  else
    kobj = 0;
  size = ilu_SizeOfObjectID(ca->call, kobj, BOOLEAN(isDiscrim), kclass, &ca->err); /* non-blocking */
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  if ((PyObject *) inst != Py_None)
    ilu_ExitServer(iv->kserver, iv->kclass); /* non-blocking */
  return PyInt_FromLong(size);
}

static PyObject *
  ilumod_OutputObjectID(PyObject *self, PyObject *args)
{
  IluclObject *		icl;
  IlucaObject *		ca;
  PyObject *		inst;
  int			isDiscrim;
  PyClassObject *		pycl;
  IvObject *		iv;
  ilu_Object		kobj;
  ilu_Class		kclass;

  if (!PyArg_Parse(args, "(OOiO)", &ca, &inst, &isDiscrim, &pycl))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  if (!PyClass_Check(pycl))
    {
      PyErr_SetString(PyExc_TypeError, "arg4 should be class");
      return 0;
    }

  if ((kclass = getKernelClass(pycl)) == 0)
    return 0;

  if ((icl = getPythonClass(pycl)) == 0)
    return 0;

  if (!(PyInstance_Check(inst) ||
	(inst == Py_None && !isDiscrim && icl->optional)))
    {
      PyErr_SetString(PyExc_TypeError, "arg2 should be instance");
      return 0;
    }

  if (inst != Py_None)
    {
      if ((iv = getInstVarsWithKobj((PyInstanceObject *) inst)) == 0)
	return 0;
      kobj = iv->kobj;
    }
  else
    kobj = 0;

  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputObjectID(ca->call, kobj, BOOLEAN(isDiscrim), kclass, &ca->err));

  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }

  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputObjectID(PyObject *self, PyObject *args)
{
  IluclObject * icl;
  IlucaObject *	ca;
  int		isDiscrim;
  PyClassObject *	pycl;
  ilu_Class	kclass;
  ilu_Object	kobj;

  if (!PyArg_Parse(args, "(OiO)", &ca, &isDiscrim, &pycl))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (!PyClass_Check(pycl))
    {
      PyErr_SetString(PyExc_TypeError, "arg3 should be class");
      return 0;
    }
  if ((kclass = getKernelClass(pycl)) == 0)
    return 0;
  if ((icl = getPythonClass(pycl)) == 0)
    return 0;

 CALL_KERNEL(ilupython_threaded_operation, ilu_InputObjectID(ca->call, &kobj, BOOLEAN(isDiscrim), kclass, &ca->err));

  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  if (kobj == 0)
    {
      if (isDiscrim || !icl->optional)
	{
	  PyErr_SetString(_ilupython_GeneralError,
			  "ilu_InputObjectID failed");
	  return 0;
	}
      Py_INCREF(Py_None);
      return Py_None;
    }
  if (isDiscrim)
    return getPythonTrueObject(kobj, kclass);
  return getPythonObject(kobj, kclass);
}

/********************************/

static PyObject *
  ilumod_SizeOfShortInteger(PyObject *self, PyObject *args)
{
  IlucaObject *		ca;
  long			value, s;

  if (!PyArg_Parse(args, "(Ol)", &ca, &value))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if ((ilu_shortinteger) value != value)
    {
#if defined(WIN32)
	  ilu_Error *e;
	  e = &ca->err;
#endif
      PyErr_SetString(PyExc_ValueError, strArg2OutOfRange);
#if defined(WIN32)
      ILU_ERR_CONS1(marshal, e, minor, ilu_mm_badInteger, 0);
#else
      ILU_ERR_CONS1(marshal, &ca->err, minor, ilu_mm_badInteger, 0);
#endif
      return 0;
    }
  s = ilu_SizeOfShortInteger(ca->call, value, &ca->err); /* non-blocking */
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  return PyInt_FromLong(s);
}

static PyObject *
  ilumod_OutputShortInteger(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  long		value;

  if (!PyArg_Parse(args, "(Ol)", &ca, &value))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if ((ilu_shortinteger) value != value)
    {
#if defined(WIN32)
	  ilu_Error *e;
	  e = &ca->err;
#endif
      PyErr_SetString(PyExc_ValueError, strArg2OutOfRange);
#if defined(WIN32)
      ILU_ERR_CONS1(marshal, e, minor, ilu_mm_badInteger, 0);
#else
      ILU_ERR_CONS1(marshal, &ca->err, minor, ilu_mm_badInteger, 0);
#endif
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputShortInteger(ca->call, value, &ca->err));

  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputShortInteger(PyObject *self, PyObject *args)
{
  IlucaObject *		ca;
  ilu_shortinteger	value;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, ilu_InputShortInteger(ca->call, &value, &ca->err));

  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  return PyInt_FromLong(value);
}

/********************************/

static PyObject *
  ilumod_SizeOfInteger(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  long		value, s;

  if (!PyArg_Parse(args, "(Ol)", &ca, &value))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  s = ilu_SizeOfInteger(ca->call, value, &ca->err); /* non-blocking */

  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  return (PyInt_FromLong(s));
}

static PyObject *
  ilumod_OutputInteger(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  long		value;

  if (!PyArg_Parse(args, "(Ol)", &ca, &value))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputInteger(ca->call, value, &ca->err));

  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputInteger(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  ilu_integer	value;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, ilu_InputInteger(ca->call, &value, &ca->err));
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  return PyInt_FromLong(value);
}

/********************************/

/* Returns divmod(v, 2**32), after checking that v is in [lo, hi]. */
static PyObject *
  decomposeLongInt(PyObject *v, char *hexLoBound, char *hexHiBound)
{
  int		isTooSmall;
  int		isTooLarge;
  PyObject *	liLo;
  PyObject *	liHi;
  PyObject *	li2_32;
  PyObject *	divmodTuple;

  if ((liLo = PYTHON_LONG_FROM_STRING(hexLoBound, 16)) == 0)
    return 0;
  isTooSmall = PyObject_Compare(v, liLo) < 0;
  Py_DECREF(liLo);
  if (isTooSmall)
    {
      PyErr_SetString(PyExc_ValueError, strArg2OutOfRange);
      return 0;
    }

  if ((liHi = PYTHON_LONG_FROM_STRING(hexHiBound, 16)) == 0)
    return 0;
  isTooLarge = PyObject_Compare(v, liHi) > 0;
  Py_DECREF(liHi);
  if (isTooLarge)
    {
      PyErr_SetString(PyExc_ValueError, strArg2OutOfRange);
      return 0;
    }

  if ((li2_32 = PYTHON_LONG_FROM_STRING("100000000", 16)) == 0)
    return 0;
  divmodTuple = (*v->ob_type->tp_as_number->nb_divmod)(v, li2_32);
  Py_DECREF(li2_32);
  return divmodTuple;
}

static int
  getIluLongInteger(PyObject *v, ilu_longinteger *pValue)
{
  if (PyInt_Check(v))
    {
      long	value	= PyInt_AsLong(v);

      ILU_LONGINT_LOW_WORD(pValue) = value & 0xffffffff;

      /* Behavior of a single 32-bit shift is not always defined. */
      ILU_LONGINT_HIGH_WORD(pValue) = value >> 16;
      ILU_LONGINT_HIGH_WORD(pValue) >>= 16;
      return 1;
    }
  if (PyLong_Check(v))
    {
      PyObject *	divmodTuple;

      if ((divmodTuple = decomposeLongInt(v,
					  "-8000000000000000", "7fffffffffffffff")) == 0)
	return 0;
      ILU_LONGINT_LOW_WORD(pValue) = PyLong_AsDouble(PyTuple_GetItem(divmodTuple, 1));
      ILU_LONGINT_HIGH_WORD(pValue) = PyLong_AsDouble(PyTuple_GetItem(divmodTuple, 0));
      Py_DECREF(divmodTuple);
      return 1;
    }
  PyErr_SetString(PyExc_TypeError, strArg2ShouldBeIntOrLongInt);
  return 0;
}

static PyObject *
  ilumod_SizeOfLongInteger(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  PyObject *		v;
  ilu_longinteger	value;
  ilu_cardinal		s;

  if (!PyArg_Parse(args, "(OO)", &ca, &v))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (!getIluLongInteger(v, &value))
    {
#if defined(WIN32)
	  ilu_Error *e;
	  e = &ca->err;
      ILU_ERR_CONS1(marshal, e, minor, ilu_mm_badInteger, 0);
#else
      ILU_ERR_CONS1(marshal, &ca->err, minor, ilu_mm_badInteger, 0);
#endif
      return 0;
    }
  s = ilu_SizeOfLongInteger(ca->call, value, &ca->err); /* non-blocking */
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  return PyInt_FromLong(s);
}

static PyObject *
  ilumod_OutputLongInteger(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  PyObject *	v;
  ilu_longinteger	value;

  if (!PyArg_Parse(args, "(OO)", &ca, &v))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (!getIluLongInteger(v, &value))
    {
#if defined(WIN32)
	  ilu_Error *e;
	  e = &ca->err;
      ILU_ERR_CONS1(marshal, e, minor, ilu_mm_badInteger, 0);
#else
      ILU_ERR_CONS1(marshal, &ca->err, minor, ilu_mm_badInteger, 0);
#endif
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputLongInteger(ca->call, value, &ca->err));

  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputLongInteger(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  ilu_longinteger	value;
  int		sign;
  char		image[18];

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, ilu_InputLongInteger(ca->call, &value, &ca->err));

  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }

  if (ILU_LONGINT_HIGH_WORD(&value) < 0)
    {
      sign = '-';
      ILU_LONGINT_LOW_WORD(&value) = 0xffffffff & - ILU_LONGINT_LOW_WORD(&value);
      ILU_LONGINT_HIGH_WORD(&value) = ~ ILU_LONGINT_HIGH_WORD(&value);
      if (ILU_LONGINT_LOW_WORD(&value) == 0)
	ILU_LONGINT_HIGH_WORD(&value) += 1;
    }
  else
    sign = '+';
  sprintf(image, "%c%08lx%08lx", sign, (unsigned long) ILU_LONGINT_HIGH_WORD(&value),
	  (unsigned long) ILU_LONGINT_LOW_WORD(&value));
  return PYTHON_LONG_FROM_STRING(image, 16);
}

/********************************/

static PyObject *
  ilumod_SizeOfShortCardinal(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  long		value;
  ilu_cardinal	s;

  if (!PyArg_Parse(args, "(Ol)", &ca, &value))
    return 0;

  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if ((ilu_shortcardinal) value != value)
    {
#if defined(WIN32)
	  ilu_Error *e;
	  e = &ca->err;
#endif
      PyErr_SetString(PyExc_ValueError, strArg2OutOfRange);
#if defined(WIN32)
      ILU_ERR_CONS1(marshal, e, minor, ilu_mm_badInteger, 0);
#else
      ILU_ERR_CONS1(marshal, &ca->err, minor, ilu_mm_badInteger, 0);
#endif
      return 0;
    }
  s = ilu_SizeOfShortCardinal(ca->call, value, &ca->err); /* non-blocking */
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  return PyInt_FromLong(s);
}

static PyObject *
  ilumod_OutputShortCardinal(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  long		value;

  if (!PyArg_Parse(args, "(Ol)", &ca, &value))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if ((ilu_shortcardinal) value != value)
    {
#if defined(WIN32)
	  ilu_Error *e;
	  e = &ca->err;
#endif
      PyErr_SetString(PyExc_ValueError, strArg2OutOfRange);
#if defined(WIN32)
      ILU_ERR_CONS1(marshal, e, minor, ilu_mm_badInteger, 0);
#else
      ILU_ERR_CONS1(marshal, &ca->err, minor, ilu_mm_badInteger, 0);
#endif
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputShortCardinal(ca->call, value, &ca->err));

  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputShortCardinal(PyObject *self, PyObject *args)
{
  IlucaObject *		ca;
  ilu_shortcardinal	value;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, ilu_InputShortCardinal(ca->call, &value, &ca->err));

  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  return PyInt_FromLong(value);
}

/********************************/

static int
  getIluCardinal(PyObject *v, ilu_cardinal *pValue)
{
  if (PyInt_Check(v))
    {
      long	value	= PyInt_AsLong(v);

      if (value < 0)
	{
	  PyErr_SetString(PyExc_ValueError, strArg2OutOfRange);
	  return 0;
	}
      *pValue = value;
      return 1;
    }
  if (PyLong_Check(v))
    {
      double	value	= PyLong_AsDouble(v);

      if (value < 0 || value > (unsigned) 0xffffffff)
	{
	  PyErr_SetString(PyExc_ValueError, strArg2OutOfRange);
	  return 0;
	}
      *pValue = value;
      return 1;
    }
  PyErr_SetString(PyExc_TypeError, strArg2ShouldBeIntOrLongInt);
  return 0;
}

static PyObject *
  ilumod_SizeOfCardinal(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  PyObject *	v;
  ilu_cardinal	value;
  ilu_cardinal	s;

  if (!PyArg_Parse(args, "(OO)", &ca, &v))
    return 0;

  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  if (!getIluCardinal(v, &value))
#if defined(WIN32)
  {
	  ilu_Error *e;
	  e = &ca->err;
	  return ILU_ERR_CONS1(marshal, e, minor, ilu_mm_badInteger, (PyObject *) 0);
  }
#else
    return ILU_ERR_CONS1(marshal, &ca->err, minor, ilu_mm_badInteger, ILU_NIL);
#endif

  s = ilu_SizeOfCardinal(ca->call, value, &ca->err); /* non-blocking */

  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  return PyInt_FromLong(s);
}

static PyObject *
  ilumod_OutputCardinal(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  PyObject *	v;
  ilu_cardinal	value;

  if (!PyArg_Parse(args, "(OO)", &ca, &v))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (!getIluCardinal(v, &value))
#if defined(WIN32)
  {
	  ilu_Error *e;
	  e = &ca->err;
	  return ILU_ERR_CONS1(marshal, e, minor, ilu_mm_badInteger, (PyObject *) 0);
  }
#else
    return ILU_ERR_CONS1(marshal, &ca->err, minor, ilu_mm_badInteger, (PyObject *) 0);
#endif

  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputCardinal(ca->call, value, &ca->err));

  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputCardinal(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  ilu_cardinal	value;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, ilu_InputCardinal(ca->call, &value, &ca->err));

  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  if ((long) value >= 0)
    return PyLong_FromLong(value); /* presumably faster */
  return PyLong_FromDouble((double) value);
}

/********************************/

static int
  getIluLongCardinal(PyObject *v, ilu_longcardinal *pValue)
{
  if (PyInt_Check(v))
    {
      long	value	= PyInt_AsLong(v);

      if (value < 0)
	{
	  PyErr_SetString(PyExc_ValueError, strArg2OutOfRange);
	  return 0;
	}
      ILU_LONGCARD_LOW_WORD(pValue) = value;
      ILU_LONGCARD_HIGH_WORD(pValue) = 0;
      return 1;
    }
  if (PyLong_Check(v))
    {
      PyObject *	divmodTuple;

      if ((divmodTuple = decomposeLongInt(v,
					  "0", "ffffffffffffffff")) == 0)
	return 0;
      ILU_LONGCARD_LOW_WORD(pValue) = PyLong_AsDouble(PyTuple_GetItem(divmodTuple, 1));
      ILU_LONGCARD_HIGH_WORD(pValue) = PyLong_AsDouble(PyTuple_GetItem(divmodTuple, 0));
      Py_DECREF(divmodTuple);
      return 1;
    }
  PyErr_SetString(PyExc_TypeError, strArg2ShouldBeIntOrLongInt);
  return 0;
}

static PyObject *
  ilumod_SizeOfLongCardinal(PyObject *self, PyObject *args)
{
  IlucaObject *		ca;
  PyObject *		v;
  ilu_longcardinal	value;
  ilu_cardinal	s;

  if (!PyArg_Parse(args, "(OO)", &ca, &v))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (!getIluLongCardinal(v, &value))
#if defined(WIN32)
  {
	  ilu_Error *e;
	  e = &ca->err;
	  return ILU_ERR_CONS1(marshal, e, minor, ilu_mm_badInteger, (PyObject *) 0);
  }
#else
    return ILU_ERR_CONS1(marshal, &ca->err, minor, ilu_mm_badInteger, (PyObject *) 0);
#endif

  s = ilu_SizeOfLongCardinal(ca->call, value, &ca->err); /* non-blocking */
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  return PyInt_FromLong(s);
}

static PyObject *
  ilumod_OutputLongCardinal(PyObject *self, PyObject *args)
{
  IlucaObject *		ca;
  PyObject *		v;
  ilu_longcardinal	value;

  if (!PyArg_Parse(args, "(OO)", &ca, &v))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (!getIluLongCardinal(v, &value))
#if defined(WIN32)
  {
	  ilu_Error *e;
	  e = &ca->err;
	  return ILU_ERR_CONS1(marshal, e, minor, ilu_mm_badInteger, (PyObject *) 0);
  }
#else
    return ILU_ERR_CONS1(marshal, &ca->err, minor, ilu_mm_badInteger, (PyObject *) 0);
#endif

  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputLongCardinal(ca->call, value, &ca->err));
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputLongCardinal(PyObject *self, PyObject *args)
{
  IlucaObject *		ca;
  ilu_longcardinal	value;
  char			image[17];

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_InputLongCardinal(ca->call, &value, &ca->err));
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  sprintf(image, "%08lx%08lx", (unsigned long) ILU_LONGCARD_HIGH_WORD(&value),
	  (unsigned long) ILU_LONGCARD_LOW_WORD(&value));
  return PYTHON_LONG_FROM_STRING(image, 16);
}

/********************************/

static PyObject *
  ilumod_SizeOfShortReal(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  float		value;
  ilu_cardinal	s;

  if (!PyArg_Parse(args, "(Of)", &ca, &value))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  s = ilu_SizeOfShortReal(ca->call, value, &ca->err); /* non-blocking */
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  return PyInt_FromLong(s);
}

static PyObject *
  ilumod_OutputShortReal(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  float		value;

  if (!PyArg_Parse(args, "(Of)", &ca, &value))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputShortReal(ca->call, value, &ca->err));
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputShortReal(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  float		value;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_InputShortReal(ca->call, &value, &ca->err));
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  return PyFloat_FromDouble(value);
}

/********************************/

static PyObject *
  ilumod_SizeOfReal(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  double		value;
  ilu_cardinal	s;

  if (!PyArg_Parse(args, "(Od)", &ca, &value))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  s = ilu_SizeOfReal(ca->call, value, &ca->err); /* non-blocking */
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  return PyInt_FromLong(s);
}

static PyObject *
  ilumod_OutputReal(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  double		value;

  if (!PyArg_Parse(args, "(Od)", &ca, &value))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputReal(ca->call, value, &ca->err));
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputReal(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  double		value;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_InputReal(ca->call, &value, &ca->err));
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  return PyFloat_FromDouble(value);
}

/********************************/

static PyObject *
  ilumod_SizeOfLongReal(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  IlulrObject *	v;
  ilu_cardinal	s;

  if (!PyArg_Parse(args, "(OO)", &ca, &v))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (!ilulr_Check(v))
    {
      PyErr_SetString(PyExc_TypeError, "arg2 should be ilu_longreal");
      return 0;
    }
  s = ilu_SizeOfLongReal(ca->call, ilulr_AS_LONGREAL(v), &ca->err); /* non-blocking */
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  return PyInt_FromLong(s);
}

static PyObject *
  ilumod_OutputLongReal(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  IlulrObject *	v;

  if (!PyArg_Parse(args, "(OO)", &ca, &v))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (!ilulr_Check(v))
    {
      PyErr_SetString(PyExc_TypeError, "arg2 should be ilu_longreal");
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputLongReal(ca->call, ilulr_AS_LONGREAL(v), &ca->err));
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputLongReal(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  ilu_longreal	value;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_InputLongReal(ca->call, &value, &ca->err));
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  return ilulr_FromLongReal(value);
}

/********************************/

static PyObject *
  ilumod_SizeOfEnum(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  int		value;
  ilu_cardinal	s;

  if (!PyArg_Parse(args, "(Oi)", &ca, &value))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  s = ilu_SizeOfEnum(ca->call, value, &ca->err); /* non-blocking */
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  return PyInt_FromLong(s);
}

static PyObject *
  ilumod_OutputEnum(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  int		value;

  if (!PyArg_Parse(args, "(Oi)", &ca, &value))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputEnum(ca->call, value, &ca->err));
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputEnum(PyObject *self, PyObject *args)
{
  IlucaObject *		ca;
  ilu_shortcardinal	value;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_InputEnum(ca->call, &value, &ca->err));
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  return PyInt_FromLong(value);
}

/********************************/

static PyObject *
  ilumod_SizeOfCharacter(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  long		value;
  ilu_cardinal	s;

  if (!PyArg_Parse(args, "(Ol)", &ca, &value))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if ((ilu_character) value != value)
    {
#if defined(WIN32)
	  ilu_Error *e;
	  e = &ca->err;
#endif
      PyErr_SetString(PyExc_ValueError, strArg2OutOfRange);
#if defined(WIN32)
      return ILU_ERR_CONS1(marshal, e, minor, ilu_mm_badInteger, (PyObject *) 0);
#else
      return ILU_ERR_CONS1(marshal, &ca->err, minor, ilu_mm_badInteger, (PyObject *) 0);
#endif
    }
  s = ilu_SizeOfCharacter(ca->call, value, &ca->err); /* non-blocking */
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  return PyInt_FromLong(s);
}

static PyObject *
  ilumod_OutputCharacter(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  long		value;

  if (!PyArg_Parse(args, "(Ol)", &ca, &value))
    return 0;
  if (!iluca_Check(ca))
    {
#if defined(WIN32)
	  ilu_Error *e;
	  e = &ca->err;
#endif
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
#if defined(WIN32)
      return ILU_ERR_CONS1(marshal, e, minor, ilu_mm_badInteger, (PyObject *) 0);
#else
      return ILU_ERR_CONS1(marshal, &ca->err, minor, ilu_mm_badInteger, (PyObject *) 0);
#endif
    }
  if ((ilu_character) value != value)
    {
      PyErr_SetString(PyExc_ValueError, strArg2OutOfRange);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputCharacter(ca->call, value, &ca->err));

  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }

  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputCharacter(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  ilu_character	value;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, ilu_InputCharacter(ca->call, &value, &ca->err));

  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  return PyInt_FromLong(value);
}

/********************************/

static PyObject *
  ilumod_SizeOfByte(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  long		value;
  ilu_cardinal	s;

  if (!PyArg_Parse(args, "(Ol)", &ca, &value))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if ((ilu_byte) value != value)
    {
#if defined(WIN32)
	  ilu_Error *e;
	  e = &ca->err;
#endif
      PyErr_SetString(PyExc_ValueError, strArg2OutOfRange);
#if defined(WIN32)
      return ILU_ERR_CONS1(marshal, e, minor, ilu_mm_badInteger, (PyObject *) 0);
#else
      return ILU_ERR_CONS1(marshal, &ca->err, minor, ilu_mm_badInteger, (PyObject *) 0);
#endif
    }
  s = ilu_SizeOfByte(ca->call, value, &ca->err); /* non-blocking */
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  return PyInt_FromLong(s);
}

static PyObject *
  ilumod_OutputByte(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  long		value;

  if (!PyArg_Parse(args, "(Ol)", &ca, &value))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if ((ilu_byte) value != value)
    {
#if defined(WIN32)
	  ilu_Error *e;
	  e = &ca->err;
#endif
      PyErr_SetString(PyExc_ValueError, strArg2OutOfRange);
#if defined(WIN32)
      return ILU_ERR_CONS1(marshal, e, minor, ilu_mm_badInteger, (PyObject *) 0);
#else
      return ILU_ERR_CONS1(marshal, &ca->err, minor, ilu_mm_badInteger, (PyObject *) 0);
#endif
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputByte(ca->call, value, &ca->err));
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputByte(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  ilu_byte	value;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_InputByte(ca->call, &value, &ca->err));
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  return PyInt_FromLong(value);
}

/********************************/

static PyObject *
  ilumod_SizeOfBoolean(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  long		value;
  ilu_cardinal	s;

  if (!PyArg_Parse(args, "(Ol)", &ca, &value))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (value != 0 && value != 1)
    {
#if defined(WIN32)
	  ilu_Error *e;
	  e = &ca->err;
#endif
      PyErr_SetString(PyExc_ValueError, strArg2OutOfRange);
#if defined(WIN32)
      return ILU_ERR_CONS1(marshal, e, minor, ilu_mm_badInteger, (PyObject *) 0);
#else
      return ILU_ERR_CONS1(marshal, &ca->err, minor, ilu_mm_badInteger, (PyObject *) 0);
#endif
    }
  s = ilu_SizeOfBoolean(ca->call, BOOLEAN(value), &ca->err); /* non-blocking */
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  return PyInt_FromLong(s);
}

static PyObject *
  ilumod_OutputBoolean(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  long		value;

  if (!PyArg_Parse(args, "(Ol)", &ca, &value))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (value != 0 && value != 1)
    {
#if defined(WIN32)
	  ilu_Error *e;
	  e = &ca->err;
#endif
      PyErr_SetString(PyExc_ValueError, strArg2OutOfRange);
#if defined(WIN32)
      return ILU_ERR_CONS1(marshal, e, minor, ilu_mm_badInteger, (PyObject *) 0);
#else
      return ILU_ERR_CONS1(marshal, &ca->err, minor, ilu_mm_badInteger, (PyObject *) 0);
#endif
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputBoolean(ca->call, BOOLEAN(value), &ca->err));
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputBoolean(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  ilu_boolean	value;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_InputBoolean(ca->call, &value, &ca->err));
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  return PyInt_FromLong(value);
}

/********************************/

static ilu_bytes
  vectorToBytes(PyObject *vec, long *pLen, int *pAlloced)
{
  PyObject *	(*getItem)(PyObject *, int);
  int		len;
  ilu_bytes	value;
  int		i;

  if (PyString_Check(vec))
    {
      *pLen = PyString_Size(vec);
      *pAlloced = 0;
      return (ilu_bytes) PyString_AsString(vec);
    }
  else if (PyList_Check(vec))
    {
      getItem = PyList_GetItem;
      len = PyList_Size(vec);
    }
  else if (PyTuple_Check(vec))
    {
      getItem = PyTuple_GetItem;
      len = PyTuple_Size(vec);
    }
  else
    {
      PyErr_SetString(PyExc_TypeError,
		      "arg2 should be string, list, or tuple");
      return 0;
    }
  if ((value = PyMem_NEW(ilu_byte, len)) == 0)
    {
      (void) PyErr_NoMemory();
      return 0;
    }
  for (i = 0; i < len; i++)
    {
      PyObject *	item	= (*getItem)(vec, i);
      long		itemVal;

      if (!PyInt_Check(item))
	{
	  PyErr_SetString(PyExc_TypeError,
			  "item should be integer");
	  PyMem_DEL(value);
	  return 0;
	}
      itemVal = PyInt_AsLong(item);
      if ((ilu_byte) itemVal != itemVal)
	{
	  PyErr_SetString(PyExc_ValueError,
			  "item exceeds range of byte");
	  PyMem_DEL(value);
	  return 0;
	}
      value[i] = itemVal;
    }
  *pLen = len;
  *pAlloced = 1;
  return value;
}

static PyObject *
  stringFromBytes(ilu_bytes value, int len)
{
  return PyString_FromStringAndSize((char *) value, len);
}

static PyObject *
  listFromBytes(ilu_bytes value, int len)
{
  PyObject *	list;
  int		i;

  if ((list = PyList_New(len)) == 0)
    return 0;
  for (i = 0; i < len; i++)
    {
      PyObject *	item;
      
      if ((item = PyInt_FromLong(value[i])) == 0)
	{
	  Py_DECREF(list);
	  return 0;
	}
      if (PyList_SetItem(list, i, item) < 0)
	{
	  Py_DECREF(list);
	  return 0;
	}
    }
  return list;
}

static PyObject *
  ilumod_SizeOfBytes(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  PyObject *	vec;
  long		limit;
  long		len;
  int		alloced;
  ilu_bytes	value;
  long		size;

  if (!PyArg_Parse(args, "(OOl)", &ca, &vec, &limit))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if ((value = vectorToBytes(vec, &len, &alloced)) == 0)
    return 0;
  if (limit > 0 && len > limit)
    {
      PyErr_SetString(PyExc_TypeError, strListTooLong);
      if (alloced)
	PyMem_DEL(value);
      return 0;
    }
  size = ilu_SizeOfBytes(ca->call, value, len, limit, &ca->err); /* non-blocking */
  if (alloced)
    PyMem_DEL(value);
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  return PyInt_FromLong(size);
}

static PyObject *
  ilumod_OutputBytes(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  PyObject *	vec;
  long		limit;
  long		len;
  int		alloced;
  ilu_bytes	value;

  if (!PyArg_Parse(args, "(OOl)", &ca, &vec, &limit))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if ((value = vectorToBytes(vec, &len, &alloced)) == 0)
    return 0;
  if (limit > 0 && len > limit)
    {
      PyErr_SetString(PyExc_TypeError, strListTooLong);
      if (alloced)
	PyMem_DEL(value);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputBytes(ca->call, value, len, limit, &ca->err));

  if (alloced)
    PyMem_DEL(value);

  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputBytes(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  long		limit;
  ilu_bytes	value;
  ilu_cardinal	len;
  PyObject *	list;

  if (!PyArg_Parse(args, "(Ol)", &ca, &limit))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  value = 0;
  CALL_KERNEL(ilupython_threaded_operation, ilu_InputBytes(ca->call, &value, &len, limit, &ca->err));
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  list = stringFromBytes(value, len);
  PyMem_XDEL(value);
  return list;
}

/********************************/

static PyObject *
  ilumod_SizeOfString(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  PyObject *	str;
  long		limit;
  ilu_string	value;
  ilu_cardinal	len;
  ilu_cardinal	s;

  if (!PyArg_Parse(args, "(OOl)", &ca, &str, &limit))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (!PyString_Check(str))
    {
      PyErr_SetString(PyExc_TypeError, "arg2 should be string");
      return 0;
    }
  value = PyString_AsString(str);
  len = PyString_Size(str);
  s = ilu_SizeOfString(ca->call, value, len, limit, &ca->err); /* non-blocking */
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  return PyInt_FromLong(s);
}

static PyObject *
  ilumod_OutputString(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  PyObject *	str;
  long		limit;
  ilu_string	value;
  ilu_cardinal	len;

  if (!PyArg_Parse(args, "(OOl)", &ca, &str, &limit))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if (!PyString_Check(str))
    {
      PyErr_SetString(PyExc_TypeError, "arg2 should be string");
      return 0;
    }
  value = PyString_AsString(str);
  len = PyString_Size(str);

  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputString(ca->call, value, len, limit, &ca->err));

  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputString(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  long		limit;
  ilu_string	value;
  ilu_cardinal	len;
  PyObject *	str;

  if (!PyArg_Parse(args, "(Ol)", &ca, &limit))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  value = 0;

  CALL_KERNEL(ilupython_threaded_operation, ilu_InputString(ca->call, &value, &len, limit, &ca->err));

  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  str = PyString_FromStringAndSize(value, len);
  if (value)
    free(value);
  return str;
}

/********************************/

static ilu_wstring
  vectorToWString(PyObject *vec, long *pLen)
{
  PyObject *	(*getItem)(PyObject *, int);
  int		len;
  ilu_wstring	value;
  int		i;

  if (PyString_Check(vec))
    {
      char *	string	= PyString_AsString(vec);

      len = PyString_Size(vec);
      if ((value = PyMem_NEW(ilu_character, len)) == 0)
	{
	  (void) PyErr_NoMemory();
	  return 0;
	}
      for (i = 0; i < len; i++)
	value[i] = string[i];
      *pLen = len;
      return value;
    }
  else if (PyList_Check(vec))
    {
      getItem = PyList_GetItem;
      len = PyList_Size(vec);
    }
  else if (PyTuple_Check(vec))
    {
      getItem = PyTuple_GetItem;
      len = PyTuple_Size(vec);
    }
  else
    {
      PyErr_SetString(PyExc_TypeError,
		      "arg2 should be string, list, or tuple");
      return 0;
    }
  if ((value = PyMem_NEW(ilu_character, len)) == 0)
    {
      (void) PyErr_NoMemory();
      return 0;
    }
  for (i = 0; i < len; i++)
    {
      PyObject *	item	= (*getItem)(vec, i);
      long		itemVal;

      if (!PyInt_Check(item))
	{
	  PyErr_SetString(PyExc_TypeError,
			  "item should be integer");
	  PyMem_DEL(value);
	  return 0;
	}
      itemVal = PyInt_AsLong(item);
      if ((ilu_character) itemVal != itemVal)
	{
	  PyErr_SetString(PyExc_ValueError,
			  "item exceeds range of ilu character");
	  PyMem_DEL(value);
	  return 0;
	}
      value[i] = itemVal;
    }
  *pLen = len;
  return value;
}

static PyObject *
  listFromWString(ilu_wstring value, int len)
{
  PyObject *	list;
  int		i;

  if ((list = PyList_New(len)) == 0)
    return 0;
  for (i = 0; i < len; i++)
    {
      PyObject *	item;
      
      if ((item = PyInt_FromLong(value[i])) == 0)
	{
	  Py_DECREF(list);
	  return 0;
	}
      if (PyList_SetItem(list, i, item) < 0)
	{
	  Py_DECREF(list);
	  return 0;
	}
    }
  return list;
}

static PyObject *
  ilumod_SizeOfWString(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  PyObject *	vec;
  long		limit;
  long		len;
  ilu_wstring	value;
  long		size;

  if (!PyArg_Parse(args, "(OOl)", &ca, &vec, &limit))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if ((value = vectorToWString(vec, &len)) == 0)
    return 0;
  if (limit > 0 && len > limit)
    {
      PyErr_SetString(PyExc_TypeError, strListTooLong);
      PyMem_DEL(value);
      return 0;
    }
  size = ilu_SizeOfWString(ca->call, value, len, limit,
			   &ca->err); /* non-blocking */
  PyMem_DEL(value);
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  return PyInt_FromLong(size);
}

static PyObject *
  ilumod_OutputWString(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  PyObject *	vec;
  long		limit;
  long		len;
  ilu_wstring	value;

  if (!PyArg_Parse(args, "(OOl)", &ca, &vec, &limit))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if ((value = vectorToWString(vec, &len)) == 0)
    return 0;
  if (limit > 0 && len > limit)
    {
      PyErr_SetString(PyExc_TypeError, strListTooLong);
      PyMem_DEL(value);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputWString(ca->call, value, len, limit,
	      &ca->err));
  PyMem_DEL(value);
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputWString(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  long		limit;
  ilu_wstring	value;
  ilu_cardinal	len;
  PyObject *	list;

  if (!PyArg_Parse(args, "(Ol)", &ca, &limit))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  value = 0;

  CALL_KERNEL(ilupython_threaded_operation, ilu_InputWString(ca->call, &value, &len, limit, &ca->err));

  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  list = listFromWString(value, len);
  PyMem_XDEL(value);
  return list;
}

/********************************/

static PyObject *
  ilumod_SizeOfOpaque(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  PyObject *	vec;
  long		bound;
  long		len;
  int		alloced;
  ilu_bytes	value;
  long		size;

  if (!PyArg_Parse(args, "(OOl)", &ca, &vec, &bound))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if ((value = vectorToBytes(vec, &len, &alloced)) == 0)
    return 0;
  if (len != bound)
    {
      PyErr_SetString(PyExc_TypeError, strListDoesntMatch);
      if (alloced)
	PyMem_DEL(value);
      return 0;
    }

  size = ilu_SizeOfOpaque(ca->call, value, bound, &ca->err); /* non-blocking */

  if (alloced)
    PyMem_DEL(value);
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  return PyInt_FromLong(size);
}

static PyObject *
  ilumod_OutputOpaque(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  PyObject *	vec;
  long		bound;
  long		len;
  int		alloced;
  ilu_bytes	value;

  if (!PyArg_Parse(args, "(OOl)", &ca, &vec, &bound))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if ((value = vectorToBytes(vec, &len, &alloced)) == 0)
    return 0;
  if (len != bound)
    {
      PyErr_SetString(PyExc_TypeError, strListDoesntMatch);
      if (alloced)
	PyMem_DEL(value);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputOpaque(ca->call, value, bound, &ca->err));
  if (alloced)
    PyMem_DEL(value);
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputOpaque(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  long		bound;
  ilu_bytes	value;
  PyObject *	list;

  if (!PyArg_Parse(args, "(Ol)", &ca, &bound))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  value = 0;
  CALL_KERNEL(ilupython_threaded_operation, ilu_InputOpaque(ca->call, &value, bound, &ca->err));
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  list = stringFromBytes(value, bound);
  PyMem_XDEL(value);
  return list;
}

/********************************/

static PyObject *
  ilumod_SizeOfStringVec(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  PyObject *	vec;
  long		bound;
  long		len;
  int		alloced;
  ilu_string	value;
  long		size;

  if (!PyArg_Parse(args, "(OOl)", &ca, &vec, &bound))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if ((value = (ilu_string) vectorToBytes(vec, &len, &alloced)) == 0)
    return 0;
  if (len != bound)
    {
      PyErr_SetString(PyExc_TypeError, strListDoesntMatch);
      if (alloced)
	PyMem_DEL(value);
      return 0;
    }
  size = ilu_SizeOfStringVec(ca->call, value, bound, &ca->err); /* non-blocking */
  if (alloced)
    PyMem_DEL(value);
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  return PyInt_FromLong(size);
}

static PyObject *
  ilumod_OutputStringVec(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  PyObject *	vec;
  long		bound;
  long		len;
  int		alloced;
  ilu_string	value;

  if (!PyArg_Parse(args, "(OOl)", &ca, &vec, &bound))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if ((value = (ilu_string) vectorToBytes(vec, &len, &alloced)) == 0)
    return 0;
  if (len != bound)
    {
      PyErr_SetString(PyExc_TypeError, strListDoesntMatch);
      if (alloced)
	PyMem_DEL(value);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputStringVec(ca->call, value, bound, &ca->err));
  if (alloced)
    PyMem_DEL(value);
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputStringVec(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  long		bound;
  ilu_string	value;
  PyObject *	list;

  if (!PyArg_Parse(args, "(Ol)", &ca, &bound))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  value = 0;
  CALL_KERNEL(ilupython_threaded_operation, ilu_InputStringVec(ca->call, &value, bound, &ca->err));
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  list = stringFromBytes((ilu_bytes) value, bound);
  PyMem_XDEL(value);
  return list;
}

/********************************/

static PyObject *
  ilumod_SizeOfWStringVec(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  PyObject *	vec;
  long		bound;
  long		len;
  ilu_wstring	value;
  long		size;

  if (!PyArg_Parse(args, "(OOl)", &ca, &vec, &bound))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if ((value = vectorToWString(vec, &len)) == 0)
    return 0;
  if (len != bound)
    {
      PyErr_SetString(PyExc_TypeError, strListDoesntMatch);
      PyMem_DEL(value);
      return 0;
    }
  size = ilu_SizeOfWStringVec(ca->call, value, bound,
			      &ca->err); /* non-blocking */
  PyMem_DEL(value);
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  return PyInt_FromLong(size);
}

static PyObject *
  ilumod_OutputWStringVec(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  PyObject *	vec;
  long		bound;
  long		len;
  ilu_wstring	value;

  if (!PyArg_Parse(args, "(OOl)", &ca, &vec, &bound))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  if ((value = vectorToWString(vec, &len)) == 0)
    return 0;
  if (len != bound)
    {
      PyErr_SetString(PyExc_TypeError, strListDoesntMatch);
      PyMem_DEL(value);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputWStringVec(ca->call, value, bound,
	      &ca->err));
  PyMem_DEL(value);
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputWStringVec(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  long		bound;
  ilu_wstring	value;
  PyObject *	list;

  if (!PyArg_Parse(args, "(Ol)", &ca, &bound))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  value = 0;
  CALL_KERNEL(ilupython_threaded_operation, ilu_InputWStringVec(ca->call, &value, bound, &ca->err));
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyMem_XDEL(value);
      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  list = listFromWString(value, bound);
  PyMem_XDEL(value);
  return list;
}

/********************************/

static PyObject *
  ilumod_SizeOfOptional(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  int		present;
  ilu_cardinal	s;

  if (!PyArg_Parse(args, "(Oi)", &ca, &present))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  s = ilu_SizeOfOptional(ca->call, BOOLEAN(present), &ca->err); /* non-blocking */
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  return PyInt_FromLong(s);
}

static PyObject *
  ilumod_OutputOptional(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  int		present;

  if (!PyArg_Parse(args, "(Oi)", &ca, &present))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputOptional(ca->call, BOOLEAN(present), &ca->err));
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputOptional(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  ilu_boolean	present;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_InputOptional(ca->call, &present, &ca->err));
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  if (present)
    return PyInt_FromLong(present);
  Py_INCREF(Py_None);
  return Py_None;
}

/********************************/

static PyObject *
  ilumod_SizeOfUnion(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  long		discrim, discrim_size;
  ilu_cardinal	s;

  if (!PyArg_Parse(args, "(Oll)", &ca, &discrim, &discrim_size))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  s = ilu_SizeOfUnion(ca->call, discrim, discrim_size, &ca->err); /* non-blocking */
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  return PyInt_FromLong(s);
}

static PyObject *
  ilumod_OutputUnion(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  long		discrim, discrim_size;

  if (!PyArg_Parse(args, "(Oll)", &ca, &discrim, &discrim_size))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputUnion(ca->call, discrim, discrim_size, &ca->err));
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputUnion(PyObject *self, PyObject *args)
{
  IlucaObject *		ca;
  ilu_cardinal		discrim;
  ilu_cardinal		discrim_size;

  if (!PyArg_Parse(args, "(Ol)", &ca, &discrim_size))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_InputUnion(ca->call, &discrim, discrim_size, &ca->err));
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  return PyInt_FromLong(discrim);
}

static PyObject *
  ilumod_EndUnion(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_EndUnion(ca->call, &ca->err));
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

/********************************/

static PyObject *
  ilumod_SizeOfArray(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  long		length;
  ilu_cardinal	s;

  if (!PyArg_Parse(args, "(Ol)", &ca, &length))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  s = ilu_SizeOfArray(ca->call, length, &ca->err); /* non-blocking */
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  return PyInt_FromLong(s);
}

static PyObject *
  ilumod_OutputArray(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  long		length;

  if (!PyArg_Parse(args, "(Ol)", &ca, &length))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputArray(ca->call, length, &ca->err));
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputArray(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_InputArray(ca->call, &ca->err));
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_EndArray(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  CALL_KERNEL(ilupython_threaded_operation, ilu_EndArray(ca->call, &ca->err));
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

/********************************/

static PyObject *
  ilumod_SizeOfRecord(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  ilu_cardinal	s;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  s = ilu_SizeOfRecord(ca->call, &ca->err); /* non-blocking */
  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  return PyInt_FromLong(s);
}

static PyObject *
  ilumod_OutputRecord(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputRecord(ca->call, &ca->err));

  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputRecord(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, ilu_InputRecord(ca->call, &ca->err));

  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_EndRecord(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, ilu_EndRecord(ca->call, &ca->err));

  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

/********************************/

static PyObject *
  ilumod_SizeOfSequence(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  long		len;
  long		limit;
  ilu_cardinal	s;

  if (!PyArg_Parse(args, "(Oll)", &ca, &len, &limit))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }
  s = ilu_SizeOfSequence(ca->call, len, limit, &ca->err); /* non-blocking */

  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  return PyInt_FromLong(s);
}

static PyObject *
  ilumod_OutputSequence(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  long		len;
  long		limit;

  if (!PyArg_Parse(args, "(Oll)", &ca, &len, &limit))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, ilu_OutputSequence(ca->call, len, limit, &ca->err));

  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
  ilumod_InputSequence(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;
  ilu_cardinal	len;
  long		limit;

  if (!PyArg_Parse(args, "(Ol)", &ca, &limit))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, ilu_InputSequence(ca->call, &len, limit, &ca->err));

  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  return PyInt_FromLong((long) len);
}

static PyObject *
  ilumod_EndSequence(PyObject *self, PyObject *args)
{
  IlucaObject *	ca;

  if (!PyArg_Parse(args, "O", &ca))
    return 0;
  if (!iluca_Check(ca))
    {
      PyErr_SetString(PyExc_TypeError, strArg1ShouldBeIluCall);
      return 0;
    }

  CALL_KERNEL(ilupython_threaded_operation, ilu_EndSequence(ca->call, &ca->err));

  if (ILU_ERRNOK(ca->err))
    {
      char errbuf[1000];

      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &ca->err));
      return 0;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
ilumod_ThreadedOperation(PyObject *self, PyObject *args)
{
#ifdef ILU_PYTHON_THREADS
  ilu_Error err;

  if ((thread_call_stack_map = PyDict_New()) == NULL)
    {
      PyErr_SetString(_ilupython_GeneralError, "couldn't initialize thread call stack map");
      return 0;
    }

  if (!ilu_InitializeOSThreading(&err))
    {
      char msg[256];

      sprintf(msg, "failed to initialize kernel threads: %s", ILU_ERR_NAME(err));
      ILU_HANDLED(err);
      
      PyErr_SetString(_ilupython_GeneralError, msg);
      return 0;
    }

  ilu_NewConnectionGetterForked(&err);
  ilupython_fork_thread(ilupython_collect_outgoing_connections, NULL);

  if (!ILU_ERROK(err))
    {
      char buf[256];

      _ilupython_formErrDescription(buf, &err);

      PyErr_SetString(_ilupython_GeneralError, buf);
      ILU_HANDLED(err);
      return 0;
    }

  ilupython_threaded_operation = ilu_TRUE;

  PyEval_InitThreads();
  init_thread();

  Py_INCREF(Py_None);
  return Py_None;
#else
  PyErr_SetString(_ilupython_GeneralError, "iluPrmodule was compiled without thread support");
  return 0;
#endif
}

static PyObject *
ilumod_SetFDBudget(PyObject *self, PyObject *args)
{
  int	budget;
  ilu_cardinal new_budget;

  if (PyArg_Parse(args, "i", &budget))
    {
      if (budget < 0)
	{
	  PyErr_SetString(_ilupython_GeneralError, "can't set FD budget to negative amount");
	  return 0;
	}
	else
	  {
	    CALL_KERNEL(ilupython_threaded_operation, new_budget = ilu_SetFDBudget(budget)); 
	  }
    }
  else return 0;
  return PyInt_FromLong(new_budget);
}
  
/************* end marshalling/unmarshalling routines *************/


static PyMethodDef ilumod_methods[] =
{
  {	"SetDebugLevel",		ilumod_SetDebugLevel		},
  {	"SetDebugLevelViaString",	ilumod_SetDebugLevel		},

  {	"FormClassRecord",		ilumod_FormClassRecord		},
  {	"RegisterClass",		ilumod_RegisterClass		},
  {	"RegisterSkeletons",		ilumod_RegisterSkeletons	},
  {	"CreateServer",			ilumod_CreateServer		},
  {	"DefaultServer",		ilumod_DefaultServer		},
  {	"ObjectOfSBH",			ilumod_ObjectOfSBH		},
  {	"SBHOfObject",			ilumod_SBHOfObject		},
  {	"IOROfObject",			ilumod_IOROfObject		},
  {	"Delete",			ilumod_Delete			},

  {	"PublishObject",		ilumod_PublishObject		},
  {	"WithdrawObject",		ilumod_WithdrawObject		},
  {	"LookupObject",			ilumod_LookupObject		},
  {	"PingObject",			ilumod_PingObject		},
  {	"ParseSBH",			ilumod_ParseSBH			},

  {	"RegisterInputHandler",		ilumod_RegisterInputHandler	},
  {	"CreateLoopHandle",		ilumod_CreateLoopHandle		},
  {	"RunMainLoop",			ilumod_RunMainLoop		},
  {	"ExitMainLoop",			ilumod_ExitMainLoop		},

  {	"SetMainLoop",			ilumod_SetMainLoop		},

  {	"LongReal",			ilumod_LongReal			},

  {	"FineTime",			ilumod_FineTime			},
  {	"FineTime_Now",			ilumod_FineTime_Now		},
  {	"CreateAlarm",			ilumod_CreateAlarm		},

  {	"BeginCall",			ilumod_BeginCall		},
  {	"FinishCall",			ilumod_FinishCall		},
  {	"BeginRequest",			ilumod_BeginRequest		},
  {	"FinishRequest",		ilumod_FinishRequest		},
  {	"GetReply",			ilumod_GetReply			},
  {	"ReplyRead",			ilumod_ReplyRead		},
  {	"ExceptionName",		ilumod_ExceptionName		},

  {	"GetSingleton",			ilumod_GetSingleton		},
  {	"RequestRead",			ilumod_RequestRead		},
  {	"CallerIdentity",		ilumod_CallerIdentity		},
  {	"NoReply",			ilumod_NoReply			},
  {	"BeginSizingReply",		ilumod_BeginSizingReply		},
  {	"BeginReply",			ilumod_BeginReply		},
  {	"FinishReply",			ilumod_FinishReply		},
  {	"BeginSizingException",		ilumod_BeginSizingException	},
  {	"BeginException",		ilumod_BeginException		},
  {	"FinishException",		ilumod_FinishException		},
  {	"UnexpectedException",		ilumod_UnexpectedException	},

  {	"SizeOfObjectID",		ilumod_SizeOfObjectID		},
  {	"OutputObjectID",		ilumod_OutputObjectID		},
  {	"InputObjectID",		ilumod_InputObjectID		},

  {	"SizeOfShortInteger",		ilumod_SizeOfShortInteger	},
  {	"OutputShortInteger",		ilumod_OutputShortInteger	},
  {	"InputShortInteger",		ilumod_InputShortInteger	},

  {	"SizeOfInteger",		ilumod_SizeOfInteger		},
  {	"OutputInteger",		ilumod_OutputInteger		},
  {	"InputInteger",			ilumod_InputInteger		},

  {	"SizeOfLongInteger",		ilumod_SizeOfLongInteger	},
  {	"OutputLongInteger",		ilumod_OutputLongInteger	},
  {	"InputLongInteger",		ilumod_InputLongInteger		},

  {	"SizeOfShortCardinal",		ilumod_SizeOfShortCardinal	},
  {	"OutputShortCardinal",		ilumod_OutputShortCardinal	},
  {	"InputShortCardinal",		ilumod_InputShortCardinal	},

  {	"SizeOfCardinal",		ilumod_SizeOfCardinal		},
  {	"OutputCardinal",		ilumod_OutputCardinal		},
  {	"InputCardinal",		ilumod_InputCardinal		},

  {	"SizeOfLongCardinal",		ilumod_SizeOfLongCardinal	},
  {	"OutputLongCardinal",		ilumod_OutputLongCardinal	},
  {	"InputLongCardinal",		ilumod_InputLongCardinal	},

  {	"SizeOfShortReal",		ilumod_SizeOfShortReal		},
  {	"OutputShortReal",		ilumod_OutputShortReal		},
  {	"InputShortReal",		ilumod_InputShortReal		},

  {	"SizeOfReal",			ilumod_SizeOfReal		},
  {	"OutputReal",			ilumod_OutputReal		},
  {	"InputReal",			ilumod_InputReal		},

  {	"SizeOfLongReal",		ilumod_SizeOfLongReal		},
  {	"OutputLongReal",		ilumod_OutputLongReal		},
  {	"InputLongReal",		ilumod_InputLongReal		},

  {	"SizeOfEnum",			ilumod_SizeOfEnum		},
  {	"OutputEnum",			ilumod_OutputEnum		},
  {	"InputEnum",			ilumod_InputEnum		},

  {	"SizeOfCharacter",		ilumod_SizeOfCharacter		},
  {	"OutputCharacter",		ilumod_OutputCharacter		},
  {	"InputCharacter",		ilumod_InputCharacter		},

  {	"SizeOfByte",			ilumod_SizeOfByte		},
  {	"OutputByte",			ilumod_OutputByte		},
  {	"InputByte",			ilumod_InputByte		},

  {	"SizeOfBoolean",		ilumod_SizeOfBoolean		},
  {	"OutputBoolean",		ilumod_OutputBoolean		},
  {	"InputBoolean",			ilumod_InputBoolean		},

  {	"SizeOfBytes",			ilumod_SizeOfBytes		},
  {	"OutputBytes",			ilumod_OutputBytes		},
  {	"InputBytes",			ilumod_InputBytes		},

  {	"SizeOfString",			ilumod_SizeOfString		},
  {	"OutputString",			ilumod_OutputString		},
  {	"InputString",			ilumod_InputString		},

  {	"SizeOfWString",		ilumod_SizeOfWString		},
  {	"OutputWString",		ilumod_OutputWString		},
  {	"InputWString",			ilumod_InputWString		},

  {	"SizeOfOpaque",			ilumod_SizeOfOpaque		},
  {	"OutputOpaque",			ilumod_OutputOpaque		},
  {	"InputOpaque",			ilumod_InputOpaque		},

  {	"SizeOfStringVec",		ilumod_SizeOfStringVec		},
  {	"OutputStringVec",		ilumod_OutputStringVec		},
  {	"InputStringVec",		ilumod_InputStringVec		},

  {	"SizeOfWStringVec",		ilumod_SizeOfWStringVec		},
  {	"OutputWStringVec",		ilumod_OutputWStringVec		},
  {	"InputWStringVec",		ilumod_InputWStringVec		},

  {	"SizeOfOptional",		ilumod_SizeOfOptional		},
  {	"OutputOptional",		ilumod_OutputOptional		},
  {	"InputOptional",		ilumod_InputOptional		},

  {	"SizeOfUnion",			ilumod_SizeOfUnion		},
  {	"OutputUnion",			ilumod_OutputUnion		},
  {	"InputUnion",			ilumod_InputUnion		},
  {	"EndUnion",			ilumod_EndUnion			},

  {	"SizeOfArray",			ilumod_SizeOfArray		},
  {	"OutputArray",			ilumod_OutputArray		},
  {	"InputArray",			ilumod_InputArray		},
  {	"EndArray",			ilumod_EndArray			},

  {	"SizeOfRecord",			ilumod_SizeOfRecord		},
  {	"OutputRecord",			ilumod_OutputRecord		},
  {	"InputRecord",			ilumod_InputRecord		},
  {	"EndRecord",			ilumod_EndRecord		},

  {	"SizeOfSequence",		ilumod_SizeOfSequence		},
  {	"OutputSequence",		ilumod_OutputSequence		},
  {	"InputSequence",		ilumod_InputSequence		},
  {	"EndSequence",			ilumod_EndSequence		},

  {     "ThreadedOperation",            ilumod_ThreadedOperation        },
  {     "SetFDBudget",                  ilumod_SetFDBudget              },

  {	0								}
};

/********************************/

static PyObject *
  newException(PyObject *dict, char *name)
{
  PyObject *	e	= PyString_FromString(name);

  if (e == 0 || PyDict_SetItemString(dict, name, e) < 0)
    Py_FatalError("ilu can't define exception");
  return e;
}

static void
  createExceptions(PyObject *modDict)
{
  _ilupython_GeneralError = newException(modDict, "IluGeneralError");
  ProtocolError = newException(modDict, "IluProtocolError");
  UnimplementedMethodError = newException(modDict,
					  "IluUnimplementedMethodError");
}

/********************************/

static void
  createConstants(PyObject *modDict)
{
  PyObject *	FineTimeRate;
  PyObject *	Version;

  if (PyDict_SetItemString(modDict, "FALSE", Py_False) < 0 ||
      PyDict_SetItemString(modDict, "TRUE", Py_True) < 0)
    Py_FatalError("ilu can't define FALSE and TRUE");

  if ((FineTimeRate = PyInt_FromLong(ilu_FineTimeRate)) == 0 ||
      PyDict_SetItemString(modDict, "FineTimeRate", FineTimeRate) < 0)
    Py_FatalError("ilu can't define FineTimeRate");

  if ((Version = PyString_FromString(ilu_GetILUVersion())) == 0 ||
      PyDict_SetItemString(modDict, "Version", Version) < 0)
    Py_FatalError("ilu can't define Version");
}

/********************************/

void initiluPr(void);	/* added to keep gcc happy */

void
  initiluPr(void)
{
  PyObject *	mod	= Py_InitModule("iluPr", ilumod_methods);
  PyObject *	dict	= PyModule_GetDict(mod);

  _ilupython_LangIndex = ilu_RegisterLanguage ("Python");

  createExceptions(dict);
  createConstants(dict);
  classMap = PyDict_New();
  if (PyDict_SetItemString(dict, "ClassMappings", classMap) < 0)
    Py_FatalError ("ilu can't define ClassMappings");

  ilu_SetNoter (trackKernelInterest, _ilupython_LangIndex);
}

static void 
singleThreadedReadServiceRequest(ilu_refany rock)
{
  (void)readServiceRequest(rock, ilu_TRUE);
}

#ifdef ILU_PYTHON_THREADS
static void
readConnectionRequests(void *arg)
{
  ilu_Port port = (ilu_Port) arg;
  ilu_boolean sure, closed = ilu_FALSE;
  ilu_Connection conn;
  PyObject *_save = ILU_NIL;

  ILUPY_ILLEGAL_IN_UNTHREADED;

  /* I'm not sure this thread ever really needs to be in the interpreter at all. */
  NEW_THREAD_ENTER;

  while (!closed)
    {
      EXIT_INTERPRETER(ilupython_threaded_operation);
      ilu_WaitForPortConnectionRequest(port);

      conn = ilu_HandleNewConnection(port, &closed);
      ENTER_INTERPRETER(ilupython_threaded_operation);

      if (conn == ILU_NIL)
	{
	  closed = ilu_TRUE;
	}
      else
	{
	  ilupython_fork_thread(runConnection, (void *)conn);
	}
    }

  FINISHED_THREAD_EXIT;

  exit_thread();
}

static void
runConnection(void *arg)
{
  ilu_Connection conn = (ilu_Connection) arg;
  ilu_boolean closed = ilu_FALSE;
  PyObject *tid;
  PyObject *call_stack = ILU_NIL;

  ILUPY_ILLEGAL_IN_UNTHREADED;

  NEW_THREAD_ENTER;

  tid = current_thread_id();

  if (!tid)
    return;

  if (PyDict_GetItem(thread_call_stack_map, tid) == 0)
    {
      /* top level of the stack */

      if (!(call_stack = ilupython_thread_init_stack(tid)))
	{
	  Py_DECREF(tid);
	  return;
	}
      /* note that call_stack will be NIL in every level except
	 the top one */
    }
  
  while (!closed)
    {
      CALL_KERNEL(ilupython_threaded_operation, BOOLEAN(closed = !(ilu_BlockingWaitForInputOnConnection(conn, ILU_NIL))));

      if (!closed)
	{
	  closed = readServiceRequest(conn, ilu_FALSE);
	}

    }

  if (call_stack != ILU_NIL)
    {
      _ilu_Assert(PyDict_DelItem(thread_call_stack_map, tid) == 0, "PyDict_DelItem failed");
      Py_DECREF(call_stack);
    }

  Py_DECREF(tid);

  FINISHED_THREAD_EXIT;

  ilu_CloseConnection(conn);

  exit_thread();
}

static PyObject *
ilupython_thread_push_call(PyObject *tid, PyObject *call)
{
  PyObject *call_list;

  ILUPY_ILLEGAL_IN_UNTHREADED;

  call_list = PyDict_GetItem(thread_call_stack_map, tid);

  _ilu_Assert(call_list != ILU_NIL, "call_list is NIL");

  return (PyList_Append(call_list, call) != 0 ? 0 : call_list);
}

static void
ilupython_thread_pop_call(PyObject *stack)
{
  int list_size = PyList_Size(stack);

  ILUPY_ILLEGAL_IN_UNTHREADED;

  if (list_size > 0)
    PyList_SetSlice(stack, list_size - 1, list_size, NULL);
}

static PyObject *
ilupython_thread_current_call()
{
  PyObject *tid, *stack, *obj;
  int size;

  ILUPY_ILLEGAL_IN_UNTHREADED;

  if (!(tid = current_thread_id()))
    return 0;

  stack = PyDict_GetItem(thread_call_stack_map, tid);
  
  Py_DECREF(tid);

  if (!stack)
    return 0;

  obj = PyList_GetItem(stack, PyList_Size(stack) - 1);

  return obj;
}

static PyObject *
ilupython_thread_init_stack(PyObject *tid)
{
  PyObject *new_stack;

  ILUPY_ILLEGAL_IN_UNTHREADED;

  if (!(new_stack = PyList_New(0)))
    return 0;

  if (PyDict_SetItem(thread_call_stack_map, tid, new_stack) != 0)
    return 0;

  return new_stack;
}

static void
bootstrapAlarm(ilu_private rock)
{
  bootstrap_rock *bootstrap = (bootstrap_rock *)rock;

  NEW_THREAD_ENTER; 
  bootstrap->real_proc(bootstrap->arg);

  FINISHED_THREAD_EXIT; 
}

static void
ilupython_watch_outgoing_connection(void *arg)
{
  ilu_Connection conn = (ilu_Connection)arg;
  ilu_Error err = ILU_INIT_NO_ERR;

  ILUPY_ILLEGAL_IN_UNTHREADED;

  ilu_OutgoingConnectionThreadProc(conn, &err);
  ILU_MUST_BE_SUCCESS(err);

  exit_thread();
}

static void
ilupython_collect_outgoing_connections(void *arg)
{
  ilu_Error err = ILU_INIT_NO_ERR;
  ilu_Connection conn;

  ILUPY_ILLEGAL_IN_UNTHREADED;

  while (1)
    {
      conn = ilu_OtherNewConnection(&err);
      if (ILU_ERROK(err))
	{
	  ilupython_fork_thread(ilupython_watch_outgoing_connection, conn);
	}
      else
	{
	  ilu_DebugPrintf("ilu_OtherNewConnection raises error %s!\n", ILU_ERR_NAME(err));
	  ILU_MUST_BE_SUCCESS(err);
	}
    }

  /* exit_thread(); */
}
#endif /* ILU_PYTHON_THREADS */

static void printPyObject(PyObject *o)
{
  PyObject *str = PyObject_Str(o);
  if (str != ILU_NIL)
    {
      ILU_ERRPRINTF("%s\n", PyString_AsString(str));
      Py_DECREF(str);
    }
  else
    ILU_ERRPRINTF("Error obtaining string form of object %p (refcount=%d, type=%p)\n",
		  o, o->ob_refcnt, o->ob_type);
}
