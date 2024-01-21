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
/* $Id: iluxport.h,v 1.227 1996/07/09 21:54:50 mdavidso Exp $ */
/* Last edited by Mike Spreitzer June 27, 1996 4:39 pm PDT */

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _ILU_EXPORTS_
#define _ILU_EXPORTS_

/* define dllexport to support building DLLs on Win32 */
#if defined(WIN32)
#if defined(ILU_BUILDING_KERNEL)
#define ILU_PUBLIC __declspec(dllexport) extern
#define ILU_PUBLIC_CLASS  class __declspec(dllexport)
#else
#define ILU_PUBLIC __declspec(dllimport) extern
#define ILU_PUBLIC_CLASS class __declspec(dllimport)
#endif /* defined(ILU_BUILDING_KERNEL) */
#else
#define ILU_PUBLIC extern
#define ILU_PUBLIC_CLASS class
#endif /* defined(WIN32) */

#if (defined(WIN32)||defined(WIN16))
#include <iluwin.h>
#else
#include <iluconf.h>
#endif

#include <iluerror.h>

#if (defined(ILU_SOLARIS2_THREADS) || defined(ILU_POSIX_THREADS) || defined(ILU_WIN32_THREADS))
#define ILU_OS_THREADED
#endif

/*
 * Unless specified otherwise, a result type of ilu_boolean
 * indicates TRUE is returned on normal completion, and FALSE
 * indicates something went wrong.
 * 
 * An argument of type "ilu_Error *" is never NIL, is written through
 * but not read, and, unless specified otherwise, is always given a
 * meaningful value; a proc whose result type has a
 * failure-indicating value returns this value exactly when raising
 * an error; when broken_locks is raised, the locking post-condition
 * doesn't necessarily hold.
 */


/* ================ Basic typedefs ================ */

#define ILU_ERRPRINTF	ilu_DebugPrintf

#include <ilubasic.h>
/*
 * Which #defines ILU_NIL, which we render as simply "NIL" in
 * comments in this file.
 */

typedef ilu_cardinal ilu_LanguageIndex;
        /* represents a registered language */

typedef char * ilu_Exception;
	/* address of exception description */

typedef struct _ilu_Server_s * ilu_Server;
	/* handle held by client on server */

typedef struct _ilu_Client_s * ilu_Client;
	/* handle held by server on client */

typedef struct _ilu_Connection_s * ilu_Connection;
	/* wrapper for bytestream connection */

typedef struct _ilu_Object_s * ilu_Object;
	/* wrapper around server for instances */

typedef struct _ilu_Pipe_s * ilu_Pipe;
	/* pipe for connection to other module */

typedef struct _ilu_TransportCreator_s *ilu_TransportCreator;
	/* creates transports and moorings */

typedef struct _ilu_Transport_s *ilu_Transport;
	/* abstraction of a TCP/IP or XNS Transport */

typedef struct _ilu_TransportClass_s *ilu_TransportClass;
	/* methods for Transport */

typedef struct _ilu_Mooring_s * ilu_Mooring;
	/* abstraction of a listen socket */

typedef struct _ilu_Protocol_s * ilu_Protocol;
	/* protocol binding */

typedef struct _ilu_PortClass_s * ilu_PortClass;
	/* methods for a Port */

typedef struct _ilu_Port_s * ilu_Port;
	/* handle held by server on bytestream */

typedef struct _ilu_Class_s * ilu_Class;
	/* class description for server */

typedef struct _ilu_Method_s * ilu_Method;
	/* method description for server */

typedef struct _ilu_Call_s ilu_Call_s, *ilu_Call;
	/* call description */

typedef ilu_refany ilu_Lock;
	/* slot for lock implementation's use */

typedef struct _ilu_IdentityInfo_s * ilu_IdentityInfo;
typedef struct _ilu_IdentityType_s * ilu_IdentityType;

typedef struct _ilu_Passport_s * ilu_Passport;

typedef ilu_string *ilu_TransportInfo;
/*
 * NIL-terminated vector of plain tinfo strings.  Both the vector
 * and the strings are in the same dynamic memory "object", and the
 * vector starts at the start of that object.  In other words, to
 * free one of these things, just free the vector.  And construct
 * them so that doesn't leak memory.
 */

/*Main Invariant holds;
  L2    >=    {call's conn's callmu, iomu} before,
  L2 disjoint {call's conn's callmu, iomu} after*/
typedef void    (*ilu_StubProc) (ilu_Call);

/* perhaps these two structs should be in iluntrnl.h, but the C, C++, and Lisp
   runtimes (and possibly others) use their definition. */

struct _ilu_Method_s {
  /* read-only; no locks needed */
  
  ilu_string      me_name;
  ilu_cardinal    me_id;
  ilu_boolean     me_cacheable;		/* functional? */
  ilu_boolean     me_asynchronous;	/* wait after calling? */
  ilu_Exception  *me_exceptionVector;	/* list o possible exns */
  ilu_cardinal    me_exceptionCount;	/* num exns in list */
  ilu_StubProc    me_stubproc;
};

struct _ilu_Class_s {
  /*L1, L2, Main unconstrained*/
  
  ilu_string cl_name;		/* ILU name of class */
  ilu_string cl_brand;		/* brand on class */
  ilu_string cl_unique_id;	/* unique id for type graph
				   of class */
  ilu_string  cl_singleton;	/* pinfo if class is a singleton */
  ilu_boolean cl_collectible;   /* class is collectible? */

  ilu_string cl_authentication;	/* non-NIL with name of
				   authentication type, if any */
  ilu_Method cl_methods;
  ilu_cardinal cl_method_count;
  ilu_cardinal cl_scls_count;	/* number of superclasses */
  ilu_string *cl_scls_ids;	/* address of vector of strings */

  /*L1 >= {otmu}*/
  
  ilu_Class *cl_sclses;		/* address o vector o ptrs to supercls */

  unsigned cl_shown : 1;	/* temp bit for use in type.c */
  unsigned cl_optional : 1;	/* may be NIL? -- IDL support */
};


/* ================ Errors ================ */
/* Some commonly used errors */

#include <iluerrs.h>


/* ================ Locking ================ */
/*
We use mutual exclusion.  A "mutex" is an object that can be held by
one thread at a time.  We say a thread "enters" and "exits" a mutex;
we used to say "acquires" and "releases".  A thread is either "inside"
or "outside" a mutex.  The operation of "entering" a mutex blocks the
calling thread until no thread is inside the mutex, then enters.

Associated with each mutex is a set of variables, and an invariant
that involves these variables.  A thread may inspect and modify those
variables only while inside the mutex.  The thread assumes the
invariant to hold when entering the mutex; may violate the invariant
while inside the mutex; and must restore the invariant before exiting
the mutex.  Among other things, this means it would be an error for a
thread to try to enter a mutex while the thread is already inside the
mutex.

For each variable, we have "locking comments" that indicate which
mutex is associated with the variable.  We haven't been rigorous about
writing down the mutex invariants.  This may be symptommatic of the
fact that we sometimes think of a mutex as simply an object lock on
its associated variables.

ILU has two classes of mutexes: connection mutexes and non-connection
mutexes.

There is a call mutex per connection and an I/O mutex per connection.
The call and I/O mutexes are held during I/O of a request or reply
message on the connection; the I/O mutex is also held while closing a
connection.  For a connection using a concurrent protocol, the call
mutex is released while waiting for a reply; for a connection using a
non-concurrent protocol, the call mutex is held from the start of the
request to the end of the reply.  When starting a call, one of the
server's available connections is used, or a new one is created and
used if all the existing ones have their call mutexes held.

Here are the non-connection mutexes:
smu:	global mutex for the server table;
otmu:	global mutex for object type data structures;
cmu:	global LRU list of connections
prmu:	global mutex for protocol registry
trmu:	global mutex for transport registry
(lamu:   global mutex for language registry)
gcmu:	global mutex for GC data structures
timu:	global mutex for alarm implementation.
server:	one mutex per server.

Our main technique for avoiding deadlocks is to put a partial order on
mutexes, and acquire mutexes in an order consistent with the partial
order.  That is, a thread may enter mutex B while holding mutex A only
if A < B (we have a few carefully managed exceptions to this rule;
more on this later).  The partial order is the transitive closure of
the following relationships.

cmu < server
smu < server
server < prmu
server < trmu
gcmu < server
gcmu < timu
cmu < smu
gcmu < cmu		(for gc.c:gcaInvoke)
cmu < timu		(for call.c:GR{Set,Cancel})
prmu < otmu		(for Protocol->interpret_request)
conn.iomu < X, for all non-connection X
conn.callmu < conn.iomu

The exceptions to the entering order rule are this: a thread may enter
a connection mutex while it holds any collection of non-connection
mutexes and connection mutexes of other connections --- this rule
declines to allow only mutexes of the same connection (and one of the
two cases of that is allowed by the partial order rule).

We use the symbols L2 and L1 to stand for the sets of connection and
non-connection mutexes held by a thread, respectively.  We write ">="
for the set inclusion relation.  We write "L1.sup < X" to mean that
either (a) L1 is empty, or (b) the maximum elment of L1 (the partial
order rule says there must be exactly one maximal element whenever L1
isn't empty) precedes X in the partial order.  We write "L1.sup = X"
to mean that L1 is not empty and its maximum member is X.  We don't
speak of "L2.sup" because a thread is allowed to violate the partial
order rule with respect to L2 mutexes.

Lemma: There can be no deadlocks that involve only non-connection
mutexes.  This is so because threads acquire non-connection mutexes in
an order consistent with a fixed partial order.

Deadlocks involving connection mutexes are avoided by more complicated
reasoning.

There is a locking invariant called the "Main Remnant":

  forall conn: (L2 >= {conn.iomu}) => (L2 >= {conn.callmu});

it holds at all times for all threads, except inside certain
procedures involved with closing a connection (and these procedures
don't enter the call mutex for the connection).  This means that a
thread that will hold both the call and I/O mutexes of a connection
acquires the call mutex first.  There is a common locking invariant,
called the "Main Invariant":

  L1 = {} and Main Remnant.

It holds in many places.  The Main Invariant is exactly what's guaranteed
to hold while an application's service routines are called.  The Main
Invariant is among the things guaranteed to hold while a stub is
marshalling or unmarshalling.  The Main Invariant is exactly what's
guaranteed to hold while waiting for I/O on a File Descriptor to be
enabled.  The Main Invariant is among the things guaranteed to hold while
doing I/O on a File Descriptor.

While waiting for a reply message to arrive, a thread may hold only:
(a) the call mutex of the connection, and (b) call and I/O mutexes of
other connections.

Lemma: there can be no deadlocks involving both L1 and L2 mutexes.
This is so because a thread holds no L1 mutexes while blocked waiting
to acquire an L2 mutex (see the procedures for entering L2 mutexes).

Lemma: there can be no deadlocks involving only L2 mutexes.  A thread
that will hold both the call and I/O mutexes of a connection acquires
the call mutex first.  Thus, we need consider only deadlocks involving
multiple connections.  There is just one occasion when a thread blocks
waiting to acquire one L2 mutex while holding another (of a different
connection): when initiating reading of a reply message from
connection C2 to type or GC callback Y made while unmarshalling a
message of call X from connection C1 (X and Y, C1 and C2 are
necessarily distinct).  Can a C2 mutex be held by something waiting to
acquire a C1 mutex?  That something would have to be in the midst of
reading a reply message W from C2, waiting to read a reply message
from C1 to type or GC callback Z initiated during the processing of W.
Because the reading of W started after the call message of Y was sent
(both connection mutexes are held for the entire processing of a
message), which in turn was after both C1 mutexes were acquired for X,
a connection other than C1 would have been chosen for Z (see the logic
in ilu_StartCall in call.c).  This shows that there cannot be a cycle
of just two arcs.  Longer cycles are also impossible, because this
reasoning can be chained.

Theorem: There can be no deadlocks.  In a single-threaded runtime,
this isn't an issue.  In a multi-threaded runtime, the above three
lemmas cover the three possible cases.

There is another common requirement, which is with respect to some
server s and object type cl, is called Inside(s, cl), and is this:

  ~GC(cl)	      => L1 >= {      cmu, s};
   GC(cl) &&  true(s) => L1 >= {gcmu, cmu, s};
   GC(cl) && ~true(s) => L1  = {      cmu, s};
   GC(cl) && ~true(s) => Main Remnant;
   L2 not otherwise constrained.

Note that this invariant has the property that if C2 is a subtype of
C1, then Inside(s, C1) => Inside(s, C2).  This property is used in
situations where we know some static type of an object, but not
necessarily the most specific type (yet).

There is a common refinement of the Main Invariant, known as the
Call Invariant.  It holds during most of the processing of a call,
on both the client and server sides.  The Call Invariant involves
a member of the call data structure, as well as an ilu_Error*,
which is understood to be the one last passed to a kernel interface
procedure.  Here is the definition of the Call Invariant:

Call-Invariant(call, err) ==
ILU_ERR_SWITCH(*err) {
  ILU_ERR_CASE2(bad_locks, broken_locks)
    TRUE;
  ILU_ERR_ELSE
    Main Invariant &&
    switch (call->ca_ms) {
    case ilu_cmsHi: Call-Hi(call);
    case ilu_cmsLo: Call-Lo(call);
    case ilu_cmsNo: Call-No(call);
    }
} ILU_ERR_ENDSWITCH;

This uses three other locking abstractions:

Call-Hi(call)==  L2    >=    {call's conn's callmu, iomu};
Call-Lo(call)== (L2 disjoint {call's conn's         iomu}) &&
		(L2 disjoint {call's conn's callmu} iff proto concurrent);
Call-No(call)==  L2 disjoint {call's conn's callmu, iomu};

Similarly, there is a Call-Remnant(call, err), defined as

ILU_ERR_SWITCH(*err) {
  ILU_ERR_CASE2(bad_locks, broken_locks)
    TRUE;
  ILU_ERR_ELSE
    Main Remnant &&
    switch (call->ca_ms) {
    case ilu_cmsHi: Call-Hi(call);
    case ilu_cmsLo: Call-Lo(call);
    case ilu_cmsNo: Call-No(call);
    }
} ILU_ERR_ENDSWITCH;

Throughout most of the kernel interface, an ilu_Call is an opaque
data structure.  In fact, as far as C is concerned, it *is* an
opaque data structure.  But one member, ca_ms, is revealed in the
locking comments.  This is mainly so we can write an accurate locking
comment for ilu_FinishCall, which is called in a great variety of
circumstances that are distinguishable (with respect to locking) by
no state directly accessible through this interface.

For variables, the locking comments say what mutexes must be held to
access the variable.  For procedure values, the locking comments say
what mutexes must be held to call the procedure, and, if the procedure
changes the set of held mutexes, how.  Both sorts of comment are
applicable to procedure-valued variables; we prefer to document the
locking pre- and post-conditions in a typedef of the procedure type,
and describe the variable/mutex association in the usual way.

We have three sorts of locking comments: those about L1, those about
L2, and those about whether the Main Invariant applies.  Locking
comments come in blocks.  There are two kinds of blocks of locking
comments: a "sticky" block is followed by a blank line; a "one-shot"
is not.  A locking comment is also called "sticky" or "one-shot",
depending on the kind of the comment block in which the comment is
contained.  A one-shot comment applies only to the immediately
following item.  A sticky comment of a certain sort applies to all
items between it and the next sticky comment of the same sort, except
those items to which a one-shot comment of the same sort applies.
Another exception is this: where the Main Invariant holds, we needn't
explicitly override comments of the L1 sort.

When a procedure raises INTERNAL/broken_locks, its locking
post-condition does not necessarily hold.


Sadly, we need condition variables to get high-performance
multithreaded operation.  A thread can wait on a condition variable.
Another thread can "notify" that condition variable.  This causes all
threads currently waiting on the condition variable to return from the
wait operation.  To prevent timing splinters, decisions about waiting
and notifying should be made inside a mutex.  This means the mutex
must be released while waiting on a condition variable, and there must
be no possibilty of a thread switch between the release of the mutex
and the start of the wait; the wait operation takes the mutex as an
argument, because in a pre-emptive threads environment (eg, PCR) the
release and the wait must be an atomic thread operation.

Some runtimes (eg, a single-threaded one) cannot support condition
variables; these runtimes supply NIL for all the condition variable
operations.  */

/*L1, L2, Main unconstrained*/

typedef ilu_private ilu_Mutex, ilu_Condition;

ILU_PUBLIC ilu_Mutex ilu_smu;	/* Global mutex for server table */
ILU_PUBLIC ilu_Mutex ilu_otmu;	/* ..for object type data struct.s */
ILU_PUBLIC ilu_Mutex ilu_cmu;	/* Global mutex for conn mgmt */
ILU_PUBLIC ilu_Mutex ilu_prmu;	/* Global mutex for transp. reg'y */
ILU_PUBLIC ilu_Mutex ilu_trmu;	/* Global mutex for proto. reg'y */
ILU_PUBLIC ilu_Mutex ilu_gcmu;
ILU_PUBLIC ilu_Mutex ilu_daimu;	/* For default alarm impl */

/* We don't declare here what timu is; each implementation of alarms
   has the freedom and responsibility to choose an implementation of
   timu.  ilu_daimu is available for use by one implementation per
   program. */

/* L1_sup < m before, L1_sup = m after */
#define ilu_EnterMutex(m,err) \
	ilu_EnterMutexWork(m,ilu_FALSE,err,__FILE__,__LINE__)
/*
 * Blocks until acquisition succeeds or an error is raised.  Returns
 * TRUE on success, FALSE on failure.  Sets *err appropriately for a
 * kernel interface call (bad_param => internal/inv_mutex, bad_locks
 * stet, others => internal/unhandled).
 */

/* L1_sup < m before, L1_sup = m after */
#define ilu_ReEnterMutex(m,err) \
	ilu_EnterMutexWork(m,ilu_TRUE,err,__FILE__,__LINE__)
/*
 * Blocks until acquisition succeeds or an error is raised.  On
 * success, returns TRUE without setting *err.  On failure returns
 * FALSE and raises broken_locks.
 */

/* L1_sup < m before, L1_sup = m after */
ILU_PUBLIC          ilu_boolean
ilu_EnterMutexWork(ilu_Mutex m, ilu_boolean hard,
	       ILU_ERRS((bad_locks, internal, broken_locks)) * err,
		   const char *file, int line);

/* L1 >= {m} before, L1 not >= {m} after */
#define ilu_ExitMutex(m,hard,err) \
	ilu_ExitMutexWork(m,hard,err,__FILE__,__LINE__)
/*
 * Releases held lock.  On success, returns TRUE without modifying
 * *err.  On failure examines hard.  If true, raises broken_locks;
 * otherwise, maps error like ilu_EnterMutex.
 */

/* L1 >= {m} before, L1 not >= {m} after */
ILU_PUBLIC          ilu_boolean
ilu_ExitMutexWork(ilu_Mutex m, ilu_boolean hard,
	       ILU_ERRS((bad_locks, internal, broken_locks)) * err,
		  const char *file, int line);

ILU_PUBLIC ilu_boolean 
ilu_CondNotify(ilu_Condition c,
	       ILU_ERRS((broken_locks)) * err);
/*
 * Returns true iff success.  Sets *err appropriately for a kernel
 * interface call: broken_locks if c invalid or the LockTech doesn't
 * do conditions.
 */

/* L1_sup = m */
#define ilu_CMWait1(c,m,err)	ilu_CMWait2(c,m,m,err)
/*
 * Atomically exit m and commence waiting on c.  After c is
 * notified, enter m. Returns true iff success.  Sets *err
 * appropriately for a kernel interface call: broken_locks if c or m
 * invalid, if m not held, or if the LockTech doesn't do conditions
 * (as if there were a hard==TRUE argument).
 */

/* L1_sup = m */
ILU_PUBLIC          ilu_boolean
ilu_CMWait2(ilu_Condition c, ilu_Mutex m,
	   ilu_Mutex m2, ILU_ERRS((broken_locks)) * err);
/*
 * If m2 != m, exit m2.  Then atomically exit m and commence waiting
 * on c.  After c is notified, enter m2 if m2 != m, then enter m.
 * Returns true iff success.  Sets *err appropriately for a kernel
 * interface call: broken_locks if c, m, or m2 invalid, if m or m2
 * not held, or if the LockTech doesn't do conditions (as if there
 * were a hard==TRUE argument).
 */

ILU_PUBLIC ilu_Mutex ilu_CreateMutex( ilu_string d1, ilu_string d2 );
/* The concatenation of d1 & d2 describes the mutex;
 * storage for them owned by caller. */

/*L1_sup < m before, L1_sup = m after*/
ILU_PUBLIC void ilu_AcquireMutex( ilu_Mutex m );
/* Blocks until acquisition succeeds. */

/* L1 >= {m} */
ILU_PUBLIC void     ilu_HoldMutex(ilu_Mutex m);
/* Checks that the caller holds the given mutex. */

/*L1 >= {m} before, L1 not >= {m} after*/
ILU_PUBLIC void ilu_ReleaseMutex( ilu_Mutex m );
/* Releases held lock. */

/* Locking unconstrained */
ILU_PUBLIC ilu_Mutex ilu_GetOTMutex(void);

ILU_PUBLIC ilu_boolean ilu_CanCondition(void);

ILU_PUBLIC ilu_Condition
ilu_CreateCondition(ilu_string d1, ilu_string d2,
		     ILU_ERRS((CantCondition)) * err);

ILU_PUBLIC ILU_ERRS((CantCondition)) ilu_NotifyCondition(ilu_Condition c);

ILU_PUBLIC ILU_ERRS((CantCondition)) ilu_DestroyCondition(ilu_Condition c);

/* L1_sup = m */
ILU_PUBLIC ILU_ERRS((CantCondition)) ilu_WaitCondition(ilu_Condition c,
						   ilu_Mutex m);

/*before: 				       L1_sup < cmu;
  before: cl collectible		    => L1_sup < gcmu;
  before: cl collectible & server surrogate => Main Invariant holds;
  after:  Inside(server, cl)*/
ILU_PUBLIC void ilu_EnterServer(ilu_Server server, ilu_Class cl);
/* Needed by LS runtime to call GetLanguageSpecificObject or
 * RegisterLanguageSpecificObject */

/*before: Inside(server, cl);
  after:				      L1 disjoint {cmu, server};
  after: cl collectible			   => L1 disjoint {gcmu};
  after: cl collectible & server surrogate => Main Invariant holds*/
ILU_PUBLIC void ilu_ExitServer(ilu_Server server, ilu_Class cl);

/******** Supplying the impl of mutexes ********/

typedef void Void;

typedef struct {
  /* These fields are readonly. */
  /* L2, Main unconstrained for calling */

  /* L1 unconstrained */
  ilu_Mutex(*lt_mcreate) (ilu_string d1, ilu_string d2);
  /*
   * The pair (d1, d2) describes the mutex; storage for strings
   * owned by caller.  Returns NIL on mem. alloc. failure.
   */

  /* L1 unconstrained */
  Void(*lt_muncons) (ilu_Mutex m, ilu_string * d1,
		     ilu_string * d2, ILU_ERRS((bad_param)) * err);
  /*
   * Reveals strings given to lt_mcreate.  Returned strings owned by
   * mutex.  Raises bad_param iff m is not valid or if d1 or d2 is
   * null.  m is valid when it's non-null and the result of a
   * previous call on lt_mcreate (false positives allowed here).
   */

  /* L1_sup < m before, L1_sup = m after */
  Void(*lt_acquire) (ilu_Mutex m,
		     ILU_ERRS((bad_param, bad_locks)) * err);
  /*
   * Raises bad_param if m not valid; raises bad_locks if m held at
   * entry (should we allow deadlock?).
   */

  /* L1 >= {m} */
  Void(*lt_hold) (ilu_Mutex m,
		  ILU_ERRS((bad_param, bad_locks)) * err);
  /* Raises bad_param if m not valid, bad_locks if not held */

  /* L1 >= {m} before, L1 not >= {m} after */
  Void(*lt_release) (ilu_Mutex m,
		     ILU_ERRS((bad_param, bad_locks)) * err);
  /* Raises bad_param if m not valid, bad_locks if not held */

  /* L1 unconstrained */

  ilu_Condition(*lt_ccreate) (ilu_string d1, ilu_string d2);
  /* Returns ILU_NIL on memory alloc failure. */

  Void(*lt_cuncons) (ilu_Condition c, ilu_string * d1,
		     ilu_string * d2, ILU_ERRS((bad_param)) * err);
  /* Reveals strings given to lt_ccreate. */
  /* Returned strings owned by c. */

  Void(*lt_notify) (ilu_Condition c,
		    ILU_ERRS((bad_param)) * err);
  Void(*lt_cdestroy) (ilu_Condition c,
		      ILU_ERRS((bad_param)) * err);

  /* L1_sup = m */
  Void(*lt_wait) (ilu_Condition c, ilu_Mutex m, ilu_Mutex m2,
		  ILU_ERRS((bad_param, bad_locks)) * err);
  /*
   * If m2 != m, exit m2.  Then atomically exit m and commence
   * waiting on c.  After c is notified, enter m2 if m2 != m, then
   * enter m.  Of course, the caller can't know or care whether any
   * of m and m2 are entered & exited multiple times after c is
   * notified; this freedom is needed to implement this procedure in
   * terms of a simpler primitive that has no m2 parameter and
   * re-acquires m after c is notified.  Returns true iff success.
   * Raises bad_param if c, m, or m2 not valid, bad_locks if m not
   * held, or if m2 different from m and not held.
   */

}               ilu_LockTech;

ILU_PUBLIC void
ilu_SetLockTech(ilu_LockTech * lt,
		ILU_ERRS((bad_param, no_memory)) * err);
/*
 * Call this procedure once at startup time to tell the kernel how
 * to manage mutexes.  You want to call this as soon as possible,
 * because mutexes are used to coorindate access to everything
 * else.  The default implementation will be used, if necessary,
 * before this proc is called; some languages, like C++, provide
 * very poor control over the order in which modules' start codes
 * are executed.  This proc must be called at a time when no
 * mutexes are held, and _ilu_AcquireMutex and _ilu_CreateCondition
 * have yet been created; violations are sometimes detected.  The
 * procs are like their iluntrnl.h counterparts.  A single-threaded
 * runtime can give NIL as the argument.  A single-threaded
 * runtime can give NIL condition var procs; a multi-threaded
 * runtime *must* provide non-NIL ones.
 */

/* ================ Memory Management ================ */

/*L1, L2, Main unconstrained*/

ILU_PUBLIC void *ilu_malloc(ilu_cardinal size);
ILU_PUBLIC void *ilu_realloc(void *p, ilu_cardinal size);
ILU_PUBLIC void ilu_free(void *p);
#define ilu_malloc(s)		ilu_full_malloc(s,__FILE__,__LINE__)
#define ilu_realloc(p,s)	ilu_full_realloc(p,s,__FILE__,__LINE__)
#define ilu_free(p)		ilu_full_free(p,__FILE__,__LINE__)
/* The ILU runtime (kernel and LS (if appropriate)) use these procedures
   to manage dynamic memory.  ilu_malloc and ilu_realloc call malloc
   and realloc, respectively.  If the basic procedure (malloc or
   realloc) fails, the ILU version then tries to free up some memory,
   perhaps even calling application-specified procs to free memory,
   and then tries again.  These procedures return NIL if the
   requested amount of memory still can't be allocated. */

#if 0
ILU_PUBLIC void    *
ilu_MallocE(ilu_cardinal size,
	    ILU_ERRS((no_memory)) * err);
ILU_PUBLIC void    *
ilu_ReallocE(void *p, ilu_cardinal size,
	     ILU_ERRS((no_memory)) * err);
#else
#define ilu_MallocE(s,e)	ilu_full_MallocE(s,e,__FILE__,__LINE__)
#define ilu_ReallocE(p,s,e)	ilu_full_ReallocE(p,s,e,__FILE__,__LINE__)
#endif
/*
 * Convenient wrappers around ilu_malloc and ilu_realloc that set
 * *err appropriately for a kernel interface call.
 */

ILU_PUBLIC void    *
ilu_full_malloc(ilu_cardinal size,
		const char *file, int line);
ILU_PUBLIC void    *
ilu_full_realloc(void *p, ilu_cardinal size,
		 const char *file, int line);
ILU_PUBLIC void     ilu_full_free(void *p, const char *file, int line);
ILU_PUBLIC void    *
ilu_full_MallocE(ilu_cardinal size, ILU_ERRS((no_memory)) * err,
		 const char *file, int line);
ILU_PUBLIC void    *
ilu_full_ReallocE(void *p, ilu_cardinal size, ILU_ERRS((no_memory)) * err,
		  const char *file, int line);
/* The actual procedures used by the above macros */

ILU_PUBLIC ilu_string 
  ilu_StrdupE(const ilu_string,
	      ILU_ERRS((no_memory)) * err);
#define ilu_StrdupE(s,e)	ilu_full_StrdupE(s,e,__FILE__,__LINE__)
ILU_PUBLIC ilu_string
  ilu_full_StrdupE(const ilu_string /* str */,
		   ILU_ERRS((no_memory)) * /* err */,
		   const char * /* filename */,
		   int /* lineno */);

ILU_PUBLIC ilu_string 
ilu_Strcat3E(const ilu_string s1, const ilu_string s2,
	     const ilu_string s3, ILU_ERRS((no_memory)) * err);

/* More conveniences */

ILU_PUBLIC ilu_boolean ilu_AddFreer(void (*free)(ilu_cardinal size));
/* Apps (and LS runtimes) can call this proc to register ways
   to free up memory.  Returns FALSE iff unable to register.
   Concurrent calls not allowed, not detected.
   The /size/ argument to /free/ indicates how big a
   request is prompting this freeing. */

#define ilu_must_malloc(s) ilu_full_must_malloc((s),__FILE__,__LINE__)
/*
 * Like ilu_malloc, but returns only if the memory can be allocated;
 * otherwise, something drastic is done to indicate an unrecoverable
 * fault.  This is used only in the error-reporting system; other
 * allocation failures should be reported in the more controlled
 * way.
 */

ILU_PUBLIC void    *
ilu_full_must_malloc(ilu_cardinal size,
		     const char *file, int line);
/* Used to implement the macro above. */

typedef void    (*ilu_FailureConsumer) (const char *file, int line);
/* A procedure that never returns. */

ILU_PUBLIC void     ilu_SetMemFailureAction(int mfa);
/*
 * Calling this tells the runtime which drastic action is to be
 * performed when ilu_must_malloc fails.  -2 means to print an
 * explanatory message on stderr and coredump; -1 means to print and
 * then loop forever; positive numbers mean to print and then
 * exit(mfa); others number reserved. The default is -1.
 */

ILU_PUBLIC void     ilu_SetMemFailureConsumer(ilu_FailureConsumer mfc);
/*
 * An alternative to ilu_SetMemFailureAction: this causes mfc to be
 * called when ilu_must_malloc fails.
 */

/* ================ Internal Consistency Checking ================ */

/*L1, L2, Main unconstrained*/

/*
 * The first two macros and procedures here are for use only inside the
 * kernel, and elsewhere only in the expansions of the macros in
 * iluerror.h for the kernel exception system.
 */

#define _ilu_Assert(pred,clue) _ilu_FullAssert(pred,clue,__FILE__,__LINE__)
/*
 * Code in the kernel calls this at internal consistency checks from
 * which it is *not* prepared to return an error (there will be none
 * of these once the error system is fully deployed).  The first
 * argument should be a C boolean that's true (i.e., an int that's
 * not 0).  The second argument is some string that distinguishes
 * the call point from every other point that calls _ilu_Assert;
 * storage is owned by the caller.  This procedure returns iff t.
 */

ILU_PUBLIC void
_ilu_FullAssert(int t, ilu_string id,
		const char *file, int line);
/* Used in implementing the above macro. */

#define ilu_Check(pred,err) (((pred) && ILU_CLER(*err)) || ilu_FullCheckFailed((err),__FILE__,__LINE__))
/*
 * Code in the kernel calls this at internal consistency checks from
 * which it *is* prepared to return an error.  An invocation of this
 * macro expands to an expression.  When the check succeeds (pred is
 * a true value), the expression evaluates to a true value and sets
 * *err to indicate success.  When the check fails, the check
 * failure action/consumer is consulted; the macro expansion will
 * either (a) not return, or (b) evaluate to a false value after
 * setting *err to internal/check.
 */

ILU_PUBLIC          ilu_boolean
ilu_FullCheckFailed(ILU_ERRS((internal)) * err,
		    const char *file, int line);
/* Used in implementing the above macro. */


/* The following two procedures are generally available. */

ILU_PUBLIC void     ilu_SetAssertionFailureAction(int afa);
/*
 * Calling this tells the runtime which drastic action is to be
 * performed when a run-time assertion fails.  -2 means to print an
 * explanatory message to stderr and then coredump; -1 means to
 * print and then loop forever; non-negative numbers mean to print
 * and then exit(afa); others number reserved. The default is -1.
 */

ILU_PUBLIC void     ilu_SetAssertionFailConsumer(ilu_FailureConsumer afc);
/*
 * An alternative to ilu_SetAssertionFailureAction: this causes afc
 * to be called (and no printing) when a run-time assertion fails.
 */

typedef void    (*ilu_CheckFailureConsumer) (const char *file, int line);
/*
 * A procedure for handling an internal consistency check failure.
 * If this procedure returns, the consistency check failure will be
 * raised as an error from the kernel.
 */

ILU_PUBLIC void     ilu_SetCheckFailureAction(int cfa);
/*
 * Calling this tells the runtime which action is to be performed
 * when an internal consistency check fails.  -3 means to raise an
 * error from the kernel (without necessarily printing anything); -2
 * means to print an explanatory message to stderr and then
 * coredump; -1 means to print and then loop forever; non-negative
 * numbers mean to print and then exit(afa); others number reserved.
 * The default is -1.
 */

ILU_PUBLIC void     ilu_SetCheckFailureConsumer(ilu_CheckFailureConsumer cfc);
/*
 * An alternative to ilu_SetCheckFailureAction: this causes cfc to
 * be called (and no printing); if cfc returns, an error will be
 * raised from the kernel.
 */

typedef void    (*ilu_RaiseDebugHook) (ilu_ErrorType et,
		                       const char *file, int line);
/*
 * A procedure that's called when an error is being raised in the
 * kernel.  Should return without doing anything (visible to the
 * kernel).  Intended for debugging use by ILU maintainers only.
 */
	       
ILU_PUBLIC void     ilu_SetRaiseDebugHook(ilu_RaiseDebugHook rdh);
/*
 * Call this to make rdh the one called as part of every error raise
 * in the kernel.  A language-specific runtime might call this,
 * passing a procedure that calls into the language in question, so
 * that breakpoints can be set in that language.  Intended for
 * debugging use by ILU maintainers only.
 */


/* ================ Adding Protocols and Transports ================ */
/*
New protocols and transports can be added dynamically to the kernel
by calls on the following routines:
*/

/*L1, L2, Main unconstrained*/

ILU_PUBLIC 
ILU_ERRS((ProtocolAlreadyRegistered, MaxCountExceeded))
ilu_RegisterProtocol(char *pname,
		     ilu_Protocol(*instantiator) (void),
		     ilu_boolean override_existing_registration);

typedef				/* L1_sup < trmu; L2 unconstrained */
ilu_TransportCreator(*ilu_TransportInstantiator) (ilu_TransportInfo /* tinfo */,
						  ILU_ERRS((no_memory,
							    inv_objref)) * /* err */);

ILU_PUBLIC
ILU_ERRS((TransportAlreadyRegistered, MaxCountExceeded))
ilu_RegisterTransport(char *tname,
		      ilu_TransportInstantiator instantiator,
		      ilu_boolean override_existing_registration);

/* ================ Deleting Objects ================ */
/*
 * A kernel object (ilu_Object) is accessible only inside its
 * server's mutex.  That mutex's invariant includes this: either (1)
 * the kernel object and its language-specific object (LSO) point to
 * each other, or (2) neither the kernel object nor the LSO points
 * to the other.  Sadly, this means application-specific code for
 * introducing and finalizing an object must run inside the server's
 * mutex.
 * 
 * The kernel may be "interested" in a kernel object for one of a few
 * reasons:
 * 
 * (1) It is a collectible true object with remote surrogates extant or
 * still possible (the timeout hasn't expired).
 * 
 * (2) It is a true object on which one of the built-in methods is
 * working.
 * 
 * (3) It is a collectible surrogate and the kernel is notifying the true
 * server of the positive existance of the surrogate.
 * 
 * (4) It is a collectible surrogate and the kernel is notifying the true
 * server that the surrogate is being deleted.
 * 
 * (5) It has an associated LSO.
 * 
 * We say the kernel is "very interested" in a kernel object if it is
 * interested for any but the last two of the above reasons.  The
 * kernel keeps the language runtime appraised of whether the kernel
 * is currently very interested in each object.
 * 
 * The language runtime may, at any time, choose to disassociate the
 * kernel object and the LSO.  A language with a garbage collector
 * might choose to keep the object in a collector-visible global
 * data structure during those times when the kernel is very
 * interested in the object, and choose to disassociate the KO and
 * LSO when the LSO is finalized.  A language with or without a
 * garbage collector might choose to give the application the
 * opportunity to explicitly disassociate the KO and LSO.
 * 
 * When the language runtime finds itself holding an LSO with no
 * associated KO, it can choose either to declare the LSO "broken",
 * or try to find or create a KO for it.  In the latter case, a
 * disassociated true LSO will need to be holding the object's OID
 * (ie, server plus server-relative ID); a disassociated surrogate
 * LSO will need to be holding the full SBH of the object.  When the
 * language runtime finds itself holding a KO with no associated LSO
 * it may try to find or create an LSO, or --- if the object is true
 * --- complain that the object is "closed".
 * 
 * As long as the kernel is interested, the object stays in the kernel
 * server's hash table of objects.  When the kernel is not
 * interested in the object, the kernel will un-table, destroy, and
 * free the object.
 * 
 * The LS runtime, stubs, and application --- as well as the kernel ---
 * promise to not hold onto a kernel object (directly --- indirect
 * through an LSO or server is OK, because of the server's
 * invariant) while outside its server's mutex.  Pay close attention
 * to the locking dance done during marshalling and unmarshalling.
 * If an LS runtime promises to never disassociate a certain KO and
 * LSO, that runtime may hold onto that KO outside its server's
 * mutex.  This could be done for the true GC callback object
 * exported by a client of collectible objects.  Similarly, where
 * the kernel has indicated its "interest" in an object, the kernel
 * may hold onto that object outside its server's mutex.
 */


/* ==================================== from debug.c */

/*L1, L2, Main unconstrained*/

ILU_PUBLIC ilu_cardinal ilu_SetDebugLevel(ilu_cardinal /* bits */);
/* ILU allows debugging messages for a number of internal features to
   be enabled or disabled by setting or clearing bits in the argument
   to ilu_SetDebugLevel().  See iludebug.h for a listing of the
   specific features which can be selected.   Returns the previous
   setting.  */

ILU_PUBLIC ilu_cardinal ilu_SetDebugLevelViaString(char *spec);
/* The features for which debugging messages are to be displayed
   may be specified as a string consisting of colon-separated names,
   as well as via bits, by using ilu_SetDebugLevelViaString() instead
   of ilu_SetDebugLevel().  See iludebug.h for a listing of the
   allowable features.  This can also be set with the environment
   variable ILU_DEBUG.  Returns the previous setting. */

#ifndef va_start
#include <stdarg.h>
#endif

ILU_PUBLIC void ilu_DebugPrintf (char *formatSpec, ...);
/* All debugging messages are displayed via calls on this printf-alike
   routine. */  

ILU_PUBLIC void ilu_SetDebugMessageHandler (void (*) (char * /* formatSpec */, va_list /* parms to output */));
/* Sets the output function which ilu_DebugPrintf() uses to display
   the messages printed through it.  By default, ilu_DebugPrintf()
   uses vfprintf(stderr, ...).  Two special values for the argument to
   this function are accepted:  ILU_DEFAULT_DEBUG_MESSAGE_HANDLER, which
   causes the default vfprintf(stderr, ...) message handler to be
   re-instated, and ILU_NIL_DEBUG_MESSAGE_HANDLER, which causes debug
   messages to be discarded. */

#define ILU_DEFAULT_DEBUG_MESSAGE_HANDLER ((void(*)(char *,va_list))1)
#define ILU_NIL_DEBUG_MESSAGE_HANDLER ((void(*)(char *,va_list))0)

ILU_PUBLIC void ilu_SendDebugOutputToFile (ilu_string	/* filename */);
/* Sets the debug message handler to write the debugging messages
   to the file named by the argument.  This can also be invoked by
   setting the environment variable ILU_DEBUG_FILE to the filename
   before running the program. */

/* ================ Time ================ */
/*L1, L2, Main unconstrained*/

typedef struct ilu_FineTime_s ilu_FineTime;

struct ilu_FineTime_s {
  ilu_integer ft_s;	/* seconds since some origin */
  ilu_cardinal ft_t;	/* fraction of a second */
};
/* Represents s + t/N seconds since some origin.  0 <= t < N.
   If ilu_FineTimeRate is 0, N is one greater than the largest
   ilu_cardinal; otherwise, N is ilu_FineTimeRate. */

ILU_PUBLIC const ilu_cardinal ilu_FineTimeRate;

ILU_PUBLIC ilu_FineTime ilu_FineTime_Now(void);

ILU_PUBLIC ilu_integer ilu_CoarseTime_Now(void);
/* Like ilu_FineTime_Now, but returns just the seconds component. */
/* Do we also need a way to get a "very old" or "invalid" time? */

ILU_PUBLIC ilu_FineTime ilu_FineTime_Add(ilu_FineTime a, ilu_FineTime b);

ILU_PUBLIC ilu_FineTime ilu_FineTime_Sub(ilu_FineTime a, ilu_FineTime b);

ILU_PUBLIC ilu_FineTime ilu_FineTime_Mul(ilu_FineTime a, float b);

ILU_PUBLIC ilu_integer ilu_FineTime_Cmp(ilu_FineTime a, ilu_FineTime b);
/* sgn(result) == sgn(a-b) */

#define ilu_FineTime_Eq(a, b) (((a).ft_s==(b).ft_s) && ((a).ft_t==(b).ft_t))

ILU_PUBLIC ilu_cardinal ilu_rescale(ilu_cardinal n, ilu_cardinal dfrom,
					 ilu_cardinal dto);
/* Returns floor(X(dto)*n/X(dfrom)), where
   X(c) = (double) (one more than the biggest ilu_cardinal) if c==0,
   X(c) = (double) c					    if c!=0.
   Caller guarantees 0 <= n < X(dfrom).*/

ILU_PUBLIC ilu_FineTime ilu_FineTime_FromDouble(double seconds);


/* ================ FD & Connection Management ================ */
/*
 * Because we may open multiple connections to a server, we need
 * some policy for when to close them.  That policy is this: the
 * application gives the ILU kernel a "File Descriptor Budget".  The
 * ILU kernel promises to use no more than this many File
 * Descriptors at once.  [How much sense does this make for the
 * Macintosh?  Other non-UNIX-like OSes?]  Off the top of this
 * budget we take FDs needed for serving (one per listening socket
 * and one per accept).  The remainder is allocated to outgoing
 * connections (over transports that use FDs --- ie, not inmemory).
 * When we want to consume a new FD, and there's no room left in the
 * budget, we go looking for an idle outgoing connection (one with
 * no outstanding calls) to close.  All idle outgoing connections
 * are kept in a doubly-linked list, ordered by when the connection
 * went idle (most recently at the front).
 */

/*L1_sup < cmu; L2 unconstrained*/
ILU_PUBLIC ilu_cardinal ilu_GetFDBudget(void);

/*Main Invariant holds; L2 otherwise unconstrained*/
ILU_PUBLIC ilu_cardinal ilu_SetFDBudget(ilu_cardinal n);
/* Sets the FD budget to n, if possible.  This is impossible when n is smaller than the previous budget, and the kernel can't close enough idle outgoing connections to reach n; in this case the kernel sets the budget as low as it can.  In all cases the new budget is returned. */


/* ================ Client side routines ================ */

/*Main Invariant holds; L1, L2 otherwise unconstrained*/

/*before: L1 = {};
  after:  result!=NIL => Inside(result's server, static_type);
  after:  result==NIL => L1 = {};
  Main Remnant holds*/
ILU_PUBLIC          ilu_Object
ilu_ObjectOfSBH(ilu_string sbh,
		ilu_Class static_type,
		ILU_ERRS((bad_locks, broken_locks, inv_objref,
			  no_memory, internal)) * err);
/*
 * Importing an object begins with calling this procedure, which
 * returns the kernel representation of an object (which may be
 * true or a surrogate).  mstid is the unique_id of the true object;
 * in general, it's a subtype of static_type; mstid may be NIL,
 * which means we don't know (and will never find out!) any type
 * more specific than the type statically associated with "this
 * position".  Storage of sbh and mstid is owned by the caller.
 * Neither sbh nor static_type may be NIL.  If result!=NIL &&
 * ilu_GetLanguageSpecificObject(result)==NIL, the caller must
 * invoke ilu_RegisterLanguageSpecificObject or ilu_DeltaHolds on
 * the result before unlocking the server.
 */

/*L1 >= {obj's server}; L2, Main unconstrained*/
ILU_PUBLIC      ilu_refany
ilu_GetLanguageSpecificObject(ilu_Object obj,
			      ilu_LanguageIndex language);
/* Returns the language-specific object, if any, associated with
 * the given kernel object and the given language. */

/*Inside(obj's server, obj's type);
  L1 no higher than required by that*/
ILU_PUBLIC void 
ilu_RegisterLanguageSpecificObject(ilu_Object obj,
				   ilu_refany lso,
				   ilu_LanguageIndex language);
/*
 * Makes a link from the given kernel object to the given
 * language-specific object; removes such a link if given lso==NIL.
 * This clues the kernel into whether the application is using the
 * object.  If lso==NIL, this may provoke the kernel to destroy and
 * free the kernel object; see "Deleting Objects" above.  L1
 * mutexes are exited and re-entered inside this procedure!
 */

/*Inside(obj's server, t); L1 no higher than required by that*/
ILU_PUBLIC void ilu_SetLSO(ilu_Object obj, ilu_Class t, ilu_refany lso,
		       ilu_LanguageIndex language);
/*
 * Like ilu_RegisterLanguageSpecificObject, but with better control
 * over locking.  obj has type t (among others).  Calling this
 * satisfies requirement to call ilu_RegisterLanguageSpecificObject.
 */

/*Inside(obj's server, obj's type)*/
ILU_PUBLIC 
ILU_ERRS((BadDataStructure, KernelBroken, GcRegFailed,
	  bad_locks, broken_locks))
ilu_DeltaHolds(ilu_Object obj, ilu_integer dholds);
/*
 * The holds on an ilu_Object count the number of places the object
 * is being used outside its server's mutex.  A LS runtime will
 * need to have a hold while calling ilu_PingObject, for example.
 * Pass +1 to add a hold, then -1 to remove it.  When the count goes
 * to 0, the kernel may destroy and free the kernel object; see
 * "Deleting Objects" above. L1 mutexes are exited and re-entered
 * inside this procedure!
 */

/*Inside(obj's server, obj's type)*/
ILU_PUBLIC void
ilu_DHolds(ilu_Object obj, ilu_integer dholds);
/*
 * Like ilu_DeltaHolds, but doesn't chase down consequences (and
 * doesn't exit L1 mutexes).  ilu_DeltaHolds or
 * ilu_RegisterLanguageSpecificObject must later be called before
 * exiting server mutex.
 */

/**Main Invariant holds;
   after: success => Call-Invariant(call, err) && Call-Hi(call)*/

ILU_PUBLIC      ilu_boolean
ilu_StartCall(ilu_Call_s * call, ilu_Server server, ilu_Class intro_type,
	      ilu_Method method, ilu_LanguageIndex caller_language,
	      ilu_Passport pp, ilu_Connection * new_conn,
	      ILU_ERRS((IoErrs, bad_locks,
			inv_objref, no_resources)) * err);
/*
 * Client stub calls this to initiate a call.  (call) is a pointer
 * to uninitialized memory owned by the stub.  (intro_type) is the
 * object type that introduced the method, and (method) is the rep'n
 * in that object type.  (new_conn) is an OUT parameter, through
 * which ilu_StartCall will pass either NIL or a new outgoing
 * connection that must be monitored with
 * ilu_OutgoingConnectionThreadProc.  New connections will only be
 * returned in a multi-threaded runtime; in a single-threaded one,
 * the kernel can monitor the outgoing connections without any help
 * from the LSR.  On success, initializes *call and returns TRUE;
 * ilu_FinishCall must eventually be called.  Otherwise, returns
 * FALSE and ilu_FinishCall should not be called.  A new connection
 * may be or not be returned through (new_conn) independently of
 * result and (*err).
 */

/*Main Invariant holds; L2 not further constrained*/
ILU_PUBLIC      ilu_boolean
ilu_OutgoingConnectionThreadProc(ilu_Connection conn,
				 ILU_ERRS((IoErrs)) * err);
/*
 * In a multi-threaded LSR, for each ilu_Connection produced by
 * ilu_StartCall or ilu_OtherNewConnection, a thread is forked to
 * call this procedure, which normally returns when the connection
 * is closed.
 */

/*Main Invariant holds; L2 not further constrained*/
ILU_PUBLIC      ilu_Connection
                ilu_OtherNewConnection(ILU_ERRS((internal)) * err);
/*
 * The kernel also produces new outgoing connections in
 * circumstances from which it is difficult to return the new
 * connection to the LSR.  To get these, a multi-threaded LSR forks,
 * at startup time, a special thread that repeatedly calls
 * ilu_OtherNewConnection, and forks a thread to run
 * ilu_OutgoingConnectionThreadProc for each returned connection.
 * The special thread must be forked AFTER the call on
 * ilu_SetLockTech.
 */

/*Main Invariant holds; L2 not further constrained*/
ILU_PUBLIC      ilu_boolean
                ilu_NewConnectionGetterForked(ILU_ERRS((internal)) * err);
/*
 * A multi-threaded runtime calls this at startup time, after
 * forking the thread that will call ilu_OtherNewConnection.
 */

typedef enum {
  ilucsr_err,			/* see *err */
  ilucsr_notReified,		/* sbh identifies a non-reified
				 * server */
  ilucsr_noProblem,		/* no problem has been detected
				 * with the server's current
				 * contact info */
  ilucsr_isTrue,		/* the server is true for some language */
  ilucsr_noNews,		/* sbh doesn't contain new contact
				 * info */
  ilucsr_changed		/* the identified surrogate server
				 * has been switched to the contact
				 * info in sbh */
}               ilu_ConsiderSbhResult;

/* L1_sup < smu; L2, Main unconstrained */
ILU_PUBLIC          ilu_ConsiderSbhResult
ilu_ConsiderSBH(ilu_string sbh,
		ilu_Server * s,
		ILU_ERRS((BadProtocolInfo,
			  no_memory, inv_objref, internal)) * err);
/*
 * If ilu_StartCall raised inv_objref, it might be because the
 * contact info held for the server is no longer valid.  If you
 * think sbh might hold newer, valid contact info, call this
 * procedure. Stores the identified ilu_Server, if reified, into *s.
 */

/**Before: Main Invariant, Call-Hi(call);
    After: Call-Invariant(call, err),
	   success => Call-Hi(call)*/

ILU_PUBLIC          ilu_boolean
ilu_StartRequest(ilu_Call call, ilu_cardinal argSize,
		 ILU_ERRS((IoErrs)) * err);
/*
 * Client calls this to introduce the arguments.  The size includes
 * that of the discriminator, if any.  Returns FALSE iff raising an
 * error, in which case the call is over: the LSR next calls
 * ilu_FinishCall, then raises a language-specific exception.
 * Otherwise, the arguments are mashalled next, using the
 * marshalling routines introduced later.
 */

ILU_PUBLIC          ilu_boolean
ilu_FinishRequest(ilu_Call call,
		  ILU_ERRS((IoErrs)) * err);
/*
 * End bracket for arguments.  If the call is of an ASYNCHRONOUS
 * method, the client proceeds to ilu_FinishCall.  Otherwise, the
 * client continues with ilu_GetReply.  Returns FALSE iff raising an
 * error, in which case the call is over.
 */

typedef enum ilu_ProtocolExceptions {
  ilu_ProtocolException_Success = 0,
  ilu_ProtocolException_NoSuchClassAtServer = 1,
  ilu_ProtocolException_ClassVersionMismatch = 2,
  ilu_ProtocolException_NoSuchMethodOnClass = 3,
  ilu_ProtocolException_GarbageArguments = 4,
  ilu_ProtocolException_Unknown = 5,
  ilu_ProtocolException_LostConnection = 6,
  ilu_ProtocolException_RequestRejected = 7,
  ilu_ProtocolException_RequestTimeout = 8,
  ilu_ProtocolException_Not = 1000	/* non-protocol failure; see
					 * *err */
} ilu_ProtocolException;

#define ilu_PEName(pe) \
(((pe) <= ilu_ProtocolException_Not) \
 ? ilu_PENames[pe] \
 : "(invalid ProtoExn!)")

ILU_PUBLIC const char *ilu_PENames[ilu_ProtocolException_Not + 1];

ILU_PUBLIC          ilu_ProtocolException
ilu_GetReply(ilu_Call call, ilu_cardinal * errorStatus,
	     ILU_ERRS((bad_locks, IoErrs)) * err);
/*
 * The client calls this to wait for the reply message and begin
 * processing it.  The result is success or a protocol-level error.
 * Iff a protocol error is being reported, caller next calls
 * ilu_FinishCall.  ilu_GetReply also decodes whether the marshalled
 * results are normal results or an exception parameter;
 * `errorStatus` gets 0 if success is being reported, otherwise 1 +
 * (index into method's exception vector).  The client next calls
 * the appropriate unmarshalling routine, then ilu_ReplyRead, then
 * ilu_FinishCall, and finally returns to the client code.
 */

ILU_PUBLIC          ilu_boolean
ilu_ReplyRead(ilu_Call call,
	      ILU_ERRS((IoErrs)) * err);
/*
 * A client stub that handles a reply in any way calls this after
 * succesfully unmarshalling any results or exception parameter,
 * before calling ilu_FinishCall().  Next call is ilu_FinishCall,
 * regardless of success or failure.
 */

/**Before: Call-Invariant(call, err).
   After:  Main Invariant holds, and
	   L2 disjoint {call's conn's callmu, iomu},
	   if possible (*err indicates bad_locks or broken_locks
	   when not possible).*/
ILU_PUBLIC void
ilu_FinishCall(ilu_Call call,
	       ILU_ERRS((bad_locks, IoErrs)) * err);
/*
 * This is the last procedure called for an ilu_Call, on both the
 * client and server sides.  This procedure does not free the
 * ilu_Call, because the caller owns it.  The ilu_Error is an INOUT
 * parameter; it's a comm_failure upon entry when the connection
 * needs to be closed.  On the client side, upon exit it is the
 * error to raise from the stub.  On the server side,
 * ILU_ERRNOK(*err) upon return indicates an error not returned
 * across the wire.
 */

/*L1, L2, Main unconstrained*/

ILU_PUBLIC ilu_Exception 
ilu_ExceptionOfMethod(ilu_Method method,
		      ilu_cardinal index);
/* This maps the exception index into a method-independent
 * (indeed, even object-type-independent) representation for
 * the exception.  This is useful for writing one exception
 * unmarshalling routine to share among all the methods of
 * an object type.  The index is 1+ the subscript into the
 * method's exceptionVector. */

/* Main holds, L2 no further constrained */
ILU_PUBLIC ilu_boolean 
ilu_InterruptCall(ilu_Call call,
		  ILU_ERRS((bad_locks, broken_locks)) * err);
/*
 * In a single-threaded runtime, the LSR makes this available to the
 * application, to call if and when it is tired of waiting for a
 * call to return.  Do not use in a multi-threaded runtime; instead
 * use runtime-specific methods to interrupt a wait on I/O or CV.
 */

/*
 * The following procedures are applicable to both true and
 * surrogate servers.
 */

/* L1 < cmu */
ILU_PUBLIC void ilu_BankServer(ilu_Server s);
/*
 * Begin shutting down the given server.  Henceforth no more
 * objects may be added to the server (for a surrogate server, this
 * means unmarshalling currently unknown surrogates will fail).
 * Closes all the server's ports (if it's a true server), and each
 * open connection as soon as its I/O mutex is not held. An
 * application or LS runtime can free damn near all a server's
 * resources by calling this procedure and then unlinking every
 * kernel and LS object in the server.
 */

typedef int     (*ilu_objectCallback) (ilu_Object /* obj */ ,
			                  ilu_refany /* rock */ );

/* L1 >= {s} */
ILU_PUBLIC int
ilu_ScanServerObjs(ilu_Server /* s */ ,
		   ilu_objectCallback /* cb */ ,
		   ilu_refany /* rock */ );
/*
 * Calls cb(obj, rock) for each obj currently extant in server s,
 * unless and until cb returns a non-zero value, at which point
 * that value is returned.  0 is returned iff cb never returns a
 * non-zero value.  cb is called under the same mutexes as
 * ilu_ScanServerObjs.  cb can release and re-acquire mutexes; cb
 * should return under the same mutexes as called.  Objects added
 * or removed during the enumeration might or might not be
 * enumerated. If an object is removed, others might be enumerated
 * twice.  An enumerated object is in the server when enumerated.
 */

/* L1 < gcmu */
ILU_PUBLIC int
ilu_BankAndScanServer(ilu_Server s,
		      ilu_objectCallback cb,
		      ilu_refany rock,
		      ilu_cardinal * nconns);
/*
 * Bank the server, then (if nconns != NIL) *nconns =
 * ilu_NumIoingConnsOfServer(s), and then enumerate its objects (as
 * in ilu_ScanServerObjs) while Inside(s, ilu_rootClass).  E.g., cb
 * could disassociate the given object and its LSO (if any).
 */

/* L1 >= {cmu, smu, s} */
ILU_PUBLIC void ilu_InnerBankServer(ilu_Server s);
/*
 * Like BankServer, but for calling within the specified mutexes.
 */

/* L1 >= {cmu, s} */
ILU_PUBLIC void ilu_PreBankServer(ilu_Server s);
/*
 * Like InnerBankServer, but requires fewer mutexes.  BankServer or
 * InnerBankServer should be called later (PreBankServer
 * functionally finishes the job, but doesn't free quite as much
 * memory).
 */

/* L1 >= {s} */
ILU_PUBLIC ilu_cardinal ilu_NumObjsInServer(ilu_Server s);
/*
 * Returns the number of objects currently reified in the given
 * server.  Objects are counted regardless of whether they
 * currently have an associated LSO.
 */

/* L1 >= {s} */
ILU_PUBLIC ilu_cardinal ilu_NumIoingConnsOfServer(ilu_Server s);
/*
 * Returns the number of connections of the given server whose I/O
 * mutex is held.
 */


/* ================ Language Registry ================ */

/* L2, Main unconstrained
   L1 unconstrained at present.
   (Threaded runtimes may require language table mutex?) */
ILU_PUBLIC ilu_LanguageIndex ilu_RegisterLanguage(ilu_string name);
/*
 * Pass in name of language and get an index.  Wherever the kernel
 * wants to store/compare a language, it will store/compare the index
 * of the language.  Every language runtime must register its presence
 * at least once, ideally before doing anything else.  Reregistering a
 * name will not change the table (behaves as a lookup.)
 * name will never be freed.
 *
 * The indices returned by ilu_RegisterLanguage are contiguous and
 * begin at a low number.  It is reasonable to use them as indices
 * into an array of language-specific things.
 */
     
/* ================ Object Type Registry ================ */

/*L1, L2, Main unconstrained*/

ILU_PUBLIC const ilu_Class ilu_rootClass;
/*
 * Every object type is implicitly a subtype of this one; this
 * relation is not explicitly mentioned in the supertype list.
 */

/*L1 >= {otmu}*/

ILU_PUBLIC          ilu_Class
ilu_DefineObjectType(ilu_string cl_name,
		     ilu_string cl_brand,
		     ilu_string cl_unique_id,
		     ilu_string cl_singleton,
		     ilu_boolean cl_optional,
		     ilu_boolean cl_collectible,
		     ilu_string cl_authentication,
		     ilu_cardinal cl_method_count,
		     ilu_cardinal cl_scls_count,
		     ilu_string cl_scls_ids[],
		     ILU_ERRS((internal, no_memory)) *err);
/*
 * The following sequence beings the kernel and a stub to a mutual
 * understanding of an object type.  Note that this must work when
 * there are multiple stubs (in different languages) that know about
 * the same object type.  First, the stub enters the Object Type
 * Mutex (otmu).  Then it calls this procedure, which either adds a
 * (partially constructed) new object type to the kernel or checks
 * the arguments against an object type already known to the kernel.
 * Then the stub makes a similar call for each method, and each
 * exception of each method.  This completes the construction of the
 * new object type, if it wasn't already known.  Finally, the stub
 * calls ilu_ObjectTypeDefined, and then exits the Object Type Mutex
 * (otmu).   Caller owns string arguments, and the array thereof;
 * result will never be freed.
 */

ILU_PUBLIC          ilu_Exception
ilu_DefineException(char *i, char *e,
		    ILU_ERRS((internal, no_memory)) * err);
/*
 * Returns the representation of an exception.  When i != NIL, args
 * are interface and exception names; when i==NIL, e is the CORBA
 * GIOP representation.  Caller owns args; result is never freed.
 */

ILU_PUBLIC ilu_Method 
ilu_DefineMethod(ilu_Class c,
		 ilu_cardinal i,
		 ilu_string me_name,
		 ilu_cardinal me_id,
		 ilu_boolean me_cacheable,
		 ilu_boolean me_asynchronous,
		 ilu_cardinal me_exceptionCount,
		 ilu_Exception *me_exceptionVector,
		 ILU_ERRS((internal, no_memory)) *err);
/*
 * Defines the i'th method of class c.  Caller owns me_name,
 * me_exceptionVector.
 */

ILU_PUBLIC ilu_boolean 
ilu_ObjectTypeDefined(ilu_Class t,
		      ILU_ERRS((internal/typeIncomplete)) * err);
/*
 * Called by stub when it thinks it has completely described the
 * given object type.
 */

/*L1, L2, Main unconstrained*/

ILU_PUBLIC ilu_boolean ilu_CollectibleP (ilu_Class);

ILU_PUBLIC ilu_Method ilu_MethodNOfClass (ilu_Class, ilu_cardinal  /* method index */);

ILU_PUBLIC ilu_boolean
  ilu_DataOfClass (ilu_Class		/* c */,
		   /* all the rest are out parameters, which
		      may be NIL if that info is not needed */
		   char **		/* name */,
		   char **		/* brand */,
		   char **		/* id */,
		   char **		/* singleton */,
		   ilu_boolean *	/* collectible */,
		   ilu_cardinal *	/* method_count */,
		   ilu_cardinal *	/* superclass_count */,
		   ilu_Class **		/* superclasses */,
		   ilu_boolean *	/* optional */,
		   ilu_Method *		/* methods */);
/* returns data of the class.  Any out parameter may be NIL to
   not receive that data.  Data is still owned by the callee. */
     
ILU_PUBLIC ilu_boolean
  ilu_DataOfMethod (ilu_Method		/* m */,
		    /* all the rest are out parameters, which
		       may be NIL if that info is not needed */
		    ilu_string *	/* name */,
		    ilu_cardinal *	/* id */,
		    ilu_boolean *	/* cacheable */,
		    ilu_boolean *	/* asynchronous */,
		    ilu_cardinal *	/* ecount */,
		    ilu_Exception **	/* evec */,
		    ilu_StubProc *	/* stubproc */);
/* returns data of the method.  Any out parm may be NIL to not
   receive that data.  Ownership of out data is retained by callee. */

/*L1_sup < otmu*/
/*L2, Main unconstrained*/

ILU_PUBLIC ilu_Class ilu_GetGcCallbackClass(void);
/* This also registers the class, if necessary. */

ILU_PUBLIC ilu_Class ilu_FindClassFromID( char *unique_id );
ILU_PUBLIC ilu_Class ilu_FindClassFromName( char *classname );
    /* Ways to look up registered object types. */

ILU_PUBLIC ilu_boolean ilu_IsSubObjectType( ilu_Class a, ilu_Class b );
/* Returns TRUE iff a is a subtype of b
   (including the degenerate case of a=b). */


/* ================ Server side ================ */

typedef struct ilu_ObjectTable_struct ilu_ObjectTable_s, *ilu_ObjectTable;

/*L1_sup < smu*/
/*L2, Main unconstrained*/

ILU_PUBLIC ilu_Server ilu_CreateTrueServer( ilu_string id,
				        ilu_ObjectTable objtab,
				        ilu_LanguageIndex language);
/*
 * A server module starts by declaring its existence, with a call on
 * ilu_CreateTrueServer.  id may not be NIL.  If a non-NIL objtab is
 * given, the kernel will call its ot_object_of_ih when
 * unmarshalling a reference to an object not currently in the
 * server's hash table of objects; otherwise, only tabled objects
 * may be unmarshalled.  Ownership of the arguments is associated
 * with the result.
 * The server is true for the specified language.
 */

ILU_PUBLIC ilu_string ilu_InventID(void);
/* Generates a string that's unique over space and time.  A server
   with nothing better to use might call this to get an ID.  The
   malloc'ed return value is owned by the caller.  */

struct ilu_ObjectTable_struct {
  /* Fields are readonly.  Before and after calls:
     L1 >= {server}; L2, Main unconstrained.
   */
  
  /*L1 >= {server}; L1 >= {gcmu} if result is true and collectible*/
  ilu_Object (*ot_object_of_ih)(ilu_ObjectTable self,
					   ilu_string ih);
  /* Returns the object associated with the given instance handle,
     or NIL if no such object.  Caller owns ih.  The object
     returned is obtained by calling ilu_FindOrCreateTrueObject. */
  
  void (*ot_free_self)(ilu_ObjectTable self);
	/* The server using this object table is being closed,
	   ot_object_of_ih will not be called again.
	   Release appropriate resources and ilu_free(this struct*). */
  
  ilu_private ot_rock;
};
/*
 * An object table gives the application the ability to create true
 * objects upon presentation of an instance handle.  The object
 * table is (ultimately) implemented by the application, and passed
 * to the kernel through ilu_CreateTrueServer.  For those
 * applications that don't need this, NIL can be passed.
 */

/*L1, L2, Main unconstrained*/


/*Main Invariant holds*/
ILU_PUBLIC ilu_Port 
ilu_CreatePort(ilu_Server,
	       ilu_string, /* protocol, retain */
	       ilu_TransportInfo, /* tinfo, retain */
	       ilu_Passport, /* optional, retain */
	       ilu_Error *);
/* A server will use this to create a Port on which to listen for connection
 * requests.  The protocolInfo may be a prefix of a real
 * protocolInfo string; it must at least identify the protocol.
 * The transportInfo must syntactically be a full transportInfo;
 * it may have fields that mean "unspecified".
 * Ports that have some notion of server identity will use identity info
 * in the passport to some end; it may be an error not to pass a passport,
 * depending on the transport info.  Caller owns the string arguments.
 */

/*L1_sup < s*/
ILU_PUBLIC void ilu_SetServerDefaultPort( ilu_Server s, ilu_Port p );
/* If more than one port is created for a server, this operation specifies which of those ports is used to create contact info for objects in that server.  A no-op if the port is closed. */

/*L1 >= {the object's server};
  L1 >= {gcmu} if cl collectible*/
ILU_PUBLIC ilu_Object 
  ilu_FindOrCreateTrueObject(ilu_string /* ih, REQUIRED, RETAINED */,
			     ilu_Server server, ilu_Class cl,
			     ilu_refany languageSpecificObject);
/*
 * This procedure is used for creating true objects.  The object is
 * true for the language for which the server is true.  This
 * procedure is called with non-NIL ih, server, cl, and
 * languageSpecificObject.  The LS runtime lets the application
 * choose ih, and/or provides a default way of choosing ih.
 * Ownership of ih is retained by caller.  If the kernel object
 * already exists, its object type must be exactly cl, and
 * languageSpecificObject is ignored.  Otherwise,
 * languageSpecificObject is the one for the language for which the
 * server is true.
 */

/*before: L1 = {};
  after:  result!=NIL => Inside(result's server, cl);
  after:  result==NIL => L1 = {};
  Main Remnant holds; L2 otherwise unconstrained*/
ILU_PUBLIC ilu_Object 
  ilu_FindOrCreateSurrogate (ilu_Server server,
			     ilu_string ih,
			     ilu_Class type,
			     ILU_ERRS((bad_locks, broken_locks, inv_objref,
				       internal)) * err);
/* Create and return an instance of the specified type,
   with the specified ih, on the specified server */

/*L2, Main unconstrained*/
/*L1 < port's server*/

/* Main Invariant holds; L2 disjoint {conn's iomu, callmu} */
typedef void    (*ilu_TransportInputHandler) (ilu_refany rock);
/* See ilu_SetConnectionRequestHandler */

/* Main Invariant holds; no more constraints on L2*/

ILU_PUBLIC          ilu_boolean
ilu_SetConnectionRequestHandler(ilu_Port port,
				ilu_TransportInputHandler tih,
				ilu_refany tih_rock,
				ILU_ERRS((no_memory, imp_limit,
					  no_resources, bad_param,
					  bad_locks, internal,
					  broken_locks)) * err);
/*
 * The server then waits for connection requests to show up on the
 * port.  A multi-threaded runtime does this by forking a thread per
 * port; a single-threaded one registers a connection request
 * handler with the main loop.  Passing tih = a null function
 * pointer cancels the registration.  Raises bad_param/closed if the
 * port is closed.
 */

ILU_PUBLIC ilu_boolean ilu_WaitForPortConnectionRequest(ilu_Port port);
/* A multi-threaded runtime uses this procedure to wait for a connection
 * request to arrive at the given port.  Returns false when port is
 * closed, true when a connection request is (probably ---
 * ilu_HandleNewConnection should cope with the uncertainty) waiting. */

/*L1_sup < cmu*/

/*Main Invariant holds*/
ILU_PUBLIC ilu_Connection 
  ilu_HandleNewConnection(ilu_Port port,
			  ilu_boolean * closed);
/*
 * When input shows up on the FD for a port, the server calls this
 * procedure to create a connection.  This proc returns a new
 * "incoming" connection (the other kind, "outgoing", don't appear
 * in the interface to the kernel) to the port's server.  This proc
 * sets *closed; result is meaningful only if *closed is false.  The
 * result will be NIL if opening the connection now would exceed the
 * kernel's FD budget, or if there wasn't really a connection
 * request waiting.  If the port is closed, the server may
 * eventually call ilu_DestroyPort.
 */

/*L1_sup < conn's server*/

/*Main Invariant holds*/
ILU_PUBLIC          ilu_boolean
  ilu_BlockingWaitForInputOnConnection(ilu_Connection conn,
				       ilu_FineTime * limit);
/*
 * The server then waits for input to show up on that connection.
 * Again, a multi-threaded runtime forks a thread that calls this
 * procedure (passing NIL for limit means +infinity), a
 * single-threaded runtime sets an input handler.  If
 * ilu_BlockingWaitForInputOnConnection returns FALSE, the
 * connection should be abandoned; ilu_CloseConnection should be
 * called.  After input allegedly shows up, the server begins
 * processing the request by calling ilu_ReceiveRequest.
 */

/*Main Invariant holds; L2 disjoint {conn's iomu, callmu}*/
ILU_PUBLIC          ilu_boolean
ilu_SetConnectionInputHandler(ilu_Connection conn,
			      ilu_TransportInputHandler tih,
			      ilu_refany tih_rock,
			      ILU_ERRS((no_memory, internal,
					no_resources)) * err);
/*
 * A single-threaded runtime calls this to set the input handler for
 * the connection; tih==0 means don't handle input for a while.
 */

/*L1 >= {conn's server}, L2 unconstrained*/
ILU_PUBLIC      ilu_boolean
ilu_ClearConnectionInputHandler(ilu_Connection conn,
				ILU_ERRS((no_memory, internal,
					  no_resources)) * err);
/*
 * Like ilu_SetConnectionInputHandler(conn, NIL, NIL, err), but with
 * different locking requirements.
 */

typedef enum {
  ilu_RcvReqStat_noop,		/* nothing to do for this request */
  ilu_RcvReqStat_quit,		/* stop working on connection */
  ilu_RcvReqStat_request	/* decent message received */
}               ilu_RcvReqStat;

/*Main Invariant holds*/
/*before: L2 disjoint {conn's callmu, iomu},
 *after:  Call-Invariant(*call, err)        if *call != NIL,
 *after:  L2 disjoint {conn's callmu, iomu} if *call == NIL*/
 
ILU_PUBLIC          ilu_RcvReqStat
  ilu_ReceiveRequest(ilu_Call_s * call, ilu_boolean * initted,
		     ilu_Connection conn, ilu_Class * intro_type,
		     ilu_Method * meth, ilu_cardinal * sn,
		     ILU_ERRS((bad_locks, IoErrs)) * err);
/*
 * A server runtime calls this to start processing a request.  (call)
 * points to uninitialized memory owned by the stub; (initted) is an
 * OUT ilu_boolean parameter.
 * 
 * If the result is ilu_RcvReqStat_request: *initted is TRUE; *call has
 * been initialized; meaningful values have been stored through
 * intro_type, meth, and sn (provided so that the LSR can include
 * some interesting details in debugging printouts); *err indicates
 * success; call->ca_ms == ilu_cmsHi.  Next arguments are
 * unmarshalled, then ilu_RequestRead and then one of
 * ilu_BeginReply, ilu_BeginException, or ilu_NoReply must later be
 * called (unless an error causes a jump to ilu_FinishCall); these
 * things are done by the stub, which the LSR next invokes like
 * this: (*meth->me_stubproc)(call).  The language-specific runtime
 * can use the "private" field of the ilu_Call to pass other
 * information to the stub.
 * 
 * For other results, *initted may or may not be TRUE, *call is
 * uninitialized iff *initted is FALSE, *err may or may not indicate
 * an error, and intro_type, methd, and sn may or may not have been
 * stored through.  Whenever this procedure sets *initted to a true
 * value, ilu_FinishCall must eventually be called; pass err to
 * ilu_FinishCall, then pass *err to server module if ilu_FinishCall
 * doesn't consume it.  When *initted is set to FALSE, notify the
 * server module of *err.
 * 
 * When this procedure returns ilu_RcvReqStat_quit, the LSR should stop
 * processing requests on this connection (which is now closed (but
 * not yet freed)).
 * 
 * When this procedure returns ilu_RcvReqStat_noop, the stub should not
 * be called, but the connection should continue to be served; the
 * LSR goes back to waiting for the next request on this connection.
 */
/****
Here's an outline of how a multi-threaded LSR uses ilu_ReceiveRequest,
assuming the stub calls ilu_FinishCall:

ilu_boolean     going = TRUE;
while (going) {
  ilu_RcvReqStat  rrs;
  ilu_Call_s      call;
  ilu_boolean     initted;
  ilu_Class       intro;
  ilu_Method      meth;
  ilu_cardinal    sn;
  ilu_Error       err;
  if (!ilu_BlockingWaitForInputOnConnection(conn, NULL))
    break;
  rrs = ilu_ReceiveRequest(&call, &initted, conn, &intro, &meth, &sn,
			   &err);
  going = rrs != ilu_RcvReqStat_quit;
  if (rrs == ilu_RcvReqStat_request)
    (*meth->me_stubproc) (&call);
  else if (initted)
    ilu_FinishCall(&call, err);
}
****/

/*L1, L2, Main unconstrained*/
ILU_PUBLIC ilu_boolean ilu_ThreadPerRequest(ilu_Connection conn);
/* After calling ilu_ReceiveRequest, and before invoking the stub,
 * a multithreaded runtime consults this procedure to decide whether
 * to fork a thread to process this request. */

/**before: Main Invariant, Call-Hi(call);
    after: Call-Remnant(call, err) && call->ca_ms == ilu_cmsHi;
    after: result!=NIL => Inside(call->ca_server, call->ca_intro_type);
    after: result==NIL => L1 = {}*/
ILU_PUBLIC ilu_Object 
ilu_GetCallSingleton(ilu_Call call,
		     ILU_ERRS((bad_param)) * err);
/*
 * The stub unmarshalls the arguments, beginning with the
 * discriminator.  If call->ca_intro_type is a singleton,
 * ilu_GetServerSingleton is called to get the discriminator;
 * otherwise, the discriminator is unmarshalled by a call on
 * ilu_InputObjectID.  This procedure returns NIL iff raising an
 * error, in which case the caller should jump to the call on
 * ilu_FinishCall.
 */

/**before: Main Invariant, Call-Hi(call);
    after: Call-Invariant(call, err),
	   success => call->ca_ms == ilu_cmsLo*/
ILU_PUBLIC          ilu_boolean
ilu_RequestRead(ilu_Call call,
		ILU_ERRS((IoErrs)) * err);
/*
 * A server stub calls this after unmarshalling the arguments,
 * before executing the procedure.  If err raised, caller then jumps
 * to the call on ilu_FinishCall.
 */

/*Main Invariant holds, L2 otherwise unconstrained*/

ILU_PUBLIC          ilu_cardinal
ilu_BeginSizingReply(ilu_Call call,
		     ilu_boolean exns_possible,
		     ILU_ERRS((IoErrs)) * err);
/*
 * After successful procedure execution, the server stub calls this
 * procedure to set up the reply size computation.  The sum of this
 * procedure's result and results' sizes is passed as argSize to
 * ilu_BeginReply.
 */

ILU_PUBLIC          ilu_cardinal
ilu_BeginSizingException(ilu_Call call,
			 ilu_integer eindex,
			 ILU_ERRS((IoErrs)) * err);
/*
 * If the call raises a programmer-defined exception, the server
 * stub calls this procedure to start computation of the reply size.
 * The sum of this procedure's result and size of exn's parm (if
 * any) is passed as argSize to ilu_BeginException.  This routine
 * is used to signal both system exceptions and user exceptions.
 * In the first case, the eindex value should be the inverse of the
 * integer value of the ilu_ProtocolException being signalled; in
 * the second case, it should be 1 + the zero-based index of the
 * exception in the list of exceptions for the method.
 */

/**before: Main Invariant && Call-Lo(call);
    after: Call-Invariant(call, err),
	   success => call->ca_ms == ilu_cmsHi. */

ILU_PUBLIC          ilu_boolean
ilu_BeginReply(ilu_Call call,
	       ilu_boolean exns_possible, ilu_cardinal argSize,
	       ILU_ERRS((bad_locks, IoErrs)) * err);
/*
 * Server stub calls this to introduce successful results.
 * `exns_possible` indicates whether this call might raise exceptions.
 * If result is TRUE, the results are marshalled; if result is
 * FALSE, proceed to ilu_FinishCall.
 */

ILU_PUBLIC          ilu_boolean
ilu_BeginException(ilu_Call call,
		   ilu_integer evalue, ilu_cardinal argSize,
		   ILU_ERRS((bad_locks, IoErrs)) * err);
/* If the call should raise an exception instead of return some
 * results, the server stub calls this (instead of ilu_BeginReply)
 * to introduce the exception&parameter.  For protocol exceptions, evalue
 * is the inverse of the integer value of the ilu_ProtocolException value
 * of the exception; for programmer-defined
 * exceptions, evalue is 1 + the subscript into the method's
 * exceptionVector.  argSize is the marshalled size of the
 * exeption parameter.  If result is TRUE, then the exception parameter
 * is marshalled next; otherwise, proceed to ilu_FinishCall. */

/**before: Main Invariant, Call-Hi(call);
    after: Call-Invariant(call, err) && Call-Hi(call)*/

ILU_PUBLIC ilu_boolean 
ilu_FinishReply(ilu_Call call,
		ILU_ERRS((bad_locks, IoErrs)) * err);
/*
 * End bracket for success results; call ilu_FinishCall next,
 * regardless of err.
 */

ILU_PUBLIC ilu_boolean 
ilu_FinishException(ilu_Call call,
		    ILU_ERRS((bad_locks, IoErrs)) * err);
/*
 * End bracket for exn result; call ilu_FinishCall next, regardless
 * of err.
 */

/**before: Main Invariant, Call-Lo(call);
    after: Call-Invariant(call, err),
	   success => call->ca_ms == ilu_cmsNo*/

ILU_PUBLIC ilu_boolean 
ilu_NoReply(ilu_Call call,
	    ILU_ERRS((bad_param, bad_locks, broken_locks)) * err);
/*
 * The server stub calls this for asynchronous methods.  Next call
 * ilu_FinishCall.
 */

/*L2 = {}*/
/*L1_sup < cmu*/

ILU_PUBLIC void ilu_CloseConnection( ilu_Connection conn );
/* A true server was serving on conn, and calls this to cease serving.
 * This procedure can *ONLY* be called on "incoming" connections.
 */

ILU_PUBLIC void ilu_DestroyConnection( ilu_Connection conn );
/* This is called on an incoming connection after ilu_CloseConnection
 * has been called and when the true server is not processing (and will
 * never again process) any calls from this connection.
 * This procedure frees conn.
 *
 * !!! MISTAKES WILL CAUSE MEMORY SMASHES !!!
 *
 * This proc can be called correctly because incoming connections
 * appear only in an ilu_Server or an incoming ilu_Call or in the args
 * of certain kernel interface procedures.  The connection can be
 * reached from an ilu_Server only while holding the server lock.
 * An ilu_Call appears only in kernel interface procs that the true server
 * is guaranteeing never again to call with an ilu_Call from conn,
 * and in an ilu_Pipe --- whose locking isn't designed yet.
 * The true server is guaranteeing to never again make any kernel calls
 * that explicitly mention this connection.
 *
 */

/*L2, Main unconstrained*/
ILU_PUBLIC void ilu_ClosePort(ilu_Port port);
/* A true server can call this to cease exporting itself through
 * the given port.  If the port was the server's default port,
 * some other port (if there are any) is chosen to be the default.
 * Since the contact info in an SBH mentions only one port, woe
 * unto a client that uses an SBH for a closed port.
 */

ILU_PUBLIC void ilu_DestroyPort( ilu_Port port );
/* This is called on a port after ilu_ClosePort
 * has been called and when the true server is not processing (and will
 * never again process) any connections to this port.
 * This procedure frees port.
 *
 * !!! MISTAKES WILL CAUSE MEMORY SMASHES !!!
 *
 * This proc can be called correctly because a port appears only in
 * an ilu_Connection, an ilu_Server, or a call on a kernel interface
 * procedure.  The server is guaranteeing there are no open connections
 * from this port, and never again to call a kernel interface proc with
 * this port; the server lock is held while accessing ports of the server.
 *
 */

/* ================ Concurrent I/O routines ================ */

/*Main Invariant holds; L1, L2 otherwise unconstrained*/
ILU_PUBLIC void ilu_RunMainLoop(int *stop);
/*
 * A single-threaded runtime calls this to animate all true servers,
 * handle the alarm, and do any other registered input processing.
 * A multi-threaded runtime never calls this procedure, instead
 * forking threads to animate servers and pass the time.  This
 * procedure may be invoked recursively, so long as no two
 * cuncurrent invocations are given the address for "stop".  This
 * procedure processes input and time until ilu_ExitMainLoop is
 * invoked on stop.
 */

/*L1, L2, Main unconstrained; synch provided by single-threadedness*/

ILU_PUBLIC void ilu_ExitMainLoop(int *stop);
/* This causes the current invocation of ilu_RunMainLoop on stop to return once it's done with the input handler or alarm it's currently executing. */

ILU_PUBLIC ilu_boolean ilu_RegisterInputSource(int fd,
	/*Main Invariant holds; L2 otherwise unconstrained*/
	void (*proc)(int fd, ilu_private rock),
	ilu_private rock);
/*
 * A single-threaded runtime calls this procedure to declare how to
 * handle input on a given FD.  It returns FALSE if it can't do its
 * job due to some resource limitation.  ilu_UnregisterInputSource
 * must be called on this FD before ilu_RegisterInputSource can be
 * called on it again.
 */

ILU_PUBLIC ilu_boolean ilu_UnregisterInputSource(int fd);
/* A single-threaded runtime calls this procedure to cease handling input on an FD.  It returns FALSE if input on the FD wasn't being handled. */

ILU_PUBLIC ilu_boolean ilu_RegisterOutputSource(int fd,
	/*Main Invariant holds; L1, L2 otherwise unconstrained*/
	void (*proc)(int fd, ilu_private rock),
	ilu_private rock);
/*
 * A single-threaded runtime calls this procedure to queue output
 * for an FD.  It returns FALSE if it can't do its job due to some
 * resource limitation.  ilu_UnregisterOutputSource must be called
 * on this FD before ilu_RegisterOutputSource can be called on it
 * again.
 */

ILU_PUBLIC ilu_boolean ilu_UnregisterOutputSource(int fd);
/* A single-threaded runtime calls this procedure to cease queueing output on an FD.  It returns FALSE if output wasn't being queued on the FD. */

/*L1_sup < timu; L2, Main unconstrained*/

ILU_PUBLIC ilu_refany ilu_CreateAlarm(void);
/* Available in both single-threaded and multi-threaded environments.
   Creates a (re)settable alarm. */

ILU_PUBLIC void ilu_SetAlarm(ilu_refany alarm, ilu_FineTime t,
			     /*for invoking: Main Invariant holds*/
			     void (*proc)(ilu_private rock),
			     ilu_private rock);
/* An alarm has a
 * trigger time and a closure.  The closure is invoked once, as soon after
 * the trigger time as the runtime is able.  ilu_SetAlarm overwrites the
 * previous setting of the alarm.
 */

ILU_PUBLIC void ilu_UnsetAlarm(ilu_refany alarm);
/* Effectively sets the trigger time to infinity. */

/*Main Invariant holds; L2 otherwise unconstrained*/
typedef void (*ilu_IOHandler)(int fd, ilu_private rock);

typedef struct {
	/* These fields are readonly*/
	
	/*Main Invariant holds; L1, L2 otherwise unconstrained*/
	void (*ml_run)(int *stop);
	
	/*L1, L2, Main unconstrained*/

	void (*ml_exit)(int *stop);
	ilu_boolean (*ml_register_input)(int fd,
		ilu_IOHandler handler,
		ilu_private rock);
	ilu_boolean (*ml_unregister_input)(int fd);
	ilu_boolean (*ml_register_output)(int fd,
		ilu_IOHandler handler,
		ilu_private rock);
	ilu_boolean (*ml_unregister_output)(int fd);
	
	/*L1_sup < timu*/
	
	ilu_refany (*ml_create_alarm)(void);
	void (*ml_set_alarm)(ilu_refany alarm,
					ilu_FineTime t,
					/*Main Invariant holds;
					  L2 otherwise unconstrained*/
					void (*proc)(ilu_private rock),
					ilu_private rock);
	void (*ml_unset_alarm)(ilu_refany alarm);
} ilu_MainLoop;

/*L1, L2, Main unconstrained; synch provided by single-threadedness*/

ILU_PUBLIC void ilu_SetMainLoop(ilu_MainLoop *ml);
/*
 * A single-threaded runtime, or an application running thereon, can
 * call this procedure to supply a non-standard implementation of
 * the main loop (eg, the main loop of another toolkit).  A
 * multi-threaded runtime calls this to supply the implementation of
 * alarms; the other procedure slots are NIL.  This procedure should
 * be called before any calls on ilu_RunMainLoop, ilu_ExitMainLoop,
 * ilu_RegisterInputSource, ilu_UnregisterInputSource,
 * ilu_RegisterOutputSource, ilu_UnregisterOutputSource,
 * ilu_CreateAlarm, ilu_SetAlarm, or ilu_UnsetAlarm; when we get our
 * error system we can report violations.  The storage for the
 * argument is never freed.  The kernel makes few enough calls on
 * create_alarm that each could fork a new thread (in a
 * multi-threaded runtime); the application's demands are not
 * constrained.
 */

ILU_PUBLIC ilu_boolean     ilu_AddRegisterersToDefault
                (
		 ilu_boolean(*reg_inp) (int fd,
					ilu_IOHandler handler,
					ilu_private rock),
		 ilu_boolean(*can_inp) (int fd),
		 ilu_boolean(*reg_out) (int fd,
					ilu_IOHandler handler,
					ilu_private rock),
		 ilu_boolean(*can_out) (int fd),
		 void (*set_alarm) (ilu_FineTime t,
/* Main Invariant holds; L2 otherwise unconstrained */
				    void (*proc) (ilu_FineTime t)),
		 void (*can_alarm) (void)
);
/*
 * ILU's default main loop for UNIX is willing to notify another
 * main loop of input and output handler registrations and
 * de-registrations.  This is useful for integrating ILU's main
 * loop with some other, relatively uncooperative, main loop.  The
 * integrator calls ilu_AddRegisterersToDefault with procs that
 * notify the other main loop.  ilu_TRUE is returned on successful
 * extension; ilu_FALSE otherwise.  The default main loop
 * multiplexes its alarms into one fundamental alarm; set_alarm and
 * can_alarm are given the scheduling of this fundamental alarm.
 * When ILU calls (*set_alarm)(t, proc), the non-ILU main loop
 * should arrange to call proc(u) soon after time t arrives, where
 * t <= u <= the time of the call on proc(u); larger (valid) values
 * of u are better, but not a lot better.  If set_alarm is called
 * again before proc is called, this changes the time at which proc
 * should be called.  When set_alarm is called after proc, this
 * schedules a new call on proc.  can_alarm cancels the scheduled
 * call on proc, if any.
 */

#ifdef ILU_OS_THREADED

/*L1, L2, Main unconstrained; synch provided by single-threadedness*/
ILU_PUBLIC ilu_boolean
  ilu_InitializeOSThreading(ILU_ERRS((bad_param, no_memory,
				      no_resources, internal)) * err);
/*
 * This routine will initialize the ILU runtime kernel to use either
 * Solaris2 or Win32 or Posix threads (only one kind is allowed at a
 * time).  It amounts to calling ilu_SetWaitTech, ilu_SetMainLoop,
 * and ilu_SetLockTech with metaobjects constructed from the
 * OS-supplied facilities.  It is intended to be used by the C and
 * C++ runtimes, immediately previous to calling ILU_C_SetFork() or
 * its C++ equivalent.  May raise bad_param if the kernel has
 * already been set threaded.
 */

/* the Main invariant holds; L2 otherwise unconstrained */
ILU_PUBLIC ilu_boolean
  ilu_OSForkNewThread (void (*proc)(void *arg), void *arg,
		       ILU_ERRS((no_memory, no_resources,
				 internal)) *err);
/* A thin veneer over the OS-supplied fork function. */

#endif /* (defined(ILU_OS_THREADED)) */

/* Main Invariant holds, L2 otherwise unconstrained */
typedef void
ilu_FDWaitProc(int fd, ilu_boolean * sure,
	       ilu_FineTime * limit,
	       ILU_ERRS((interrupt)) * err);

typedef struct {
  /* These fields are readonly */

  ilu_FDWaitProc *wt_read_wait;
  ilu_FDWaitProc *wt_write_wait;

}               ilu_WaitTech;
/*
 * These two procedures return ASAP after any of the following four
 * conditions becomes true:
 * 
 * (1) the appropriate kind of I/O can be done on the given file
 * descriptor without blocking (this includes detecting EOF,
 * detecting closure of the FD),
 * 
 * (2) when an exceptional condition exists on the FD,
 * 
 * (3) when *limit (+infinity if limit==NIL) is exceeded, or
 * 
 * (4) the thread has been asked to interrupt its current call.
 * 
 * These procedures set *sure.  When *sure is set true, one of the
 * first two conditions held; when *sure is set false, the third or
 * fourth held .  interrupt is raised in the fourth case, and the
 * ilu_interruptSet error member is significant.  This data
 * structure is only for use in multi-threaded programs, and these
 * procedures block only the calling thread.
 * 
 * Caller will remember how few guarantees he has about whether any
 * given ilu_Transport or ilu_Connection will still be open when one
 * of these procedures returns.
 */

/*L1, L2, Main unconstrained; called only from start code*/

ILU_PUBLIC void ilu_SetWaitTech(ilu_WaitTech *wt);
/* A multi-threaded runtime (even one using a kernel threads package, because the default implementation calls ilu_RunMainLoop) calls this procedure to supply the means to block a thread until it can do I/O on a given file descriptor. */

/*L1_sup < trmu*/
ILU_PUBLIC ilu_boolean ilu_SIGPIPE_Handled(void);
/*
 * SIGPIPE is occasionally raised as ILU TCP connections are torn
 * down.  A runtime or app that cares about SIGPIPE signals does the
 * following two things before creating the first TCP connection or
 * port: (1) installs its SIGPIPE handler, and (2) calls
 * ilu_SIGPIPE_Handled to prevent ILU from installing its own
 * handler (which simply ignores the signal).  Such an app or
 * runtime must be prepared to cope with SIGPIPEs arising due to
 * ILU.  ilu_SIGPIPE_Handled returns TRUE if called early enough;
 * otherwise returns FALSE.
 */

/* ================ Alarm Multiplexing ================ */
/*
 * These data structures and procedures are useful for multiplexing
 * multiple alarms on top of one alarm.  The client constructs
 * procedures with the proper alarming signatures by calling the
 * procedures below to do most of the work.  The data structures and
 * procedures below operate within some mutex provided by the
 * client; call that mutex mxamu.
 */

typedef struct _ilu_Alarmette_s ilu_Alarmette_s, *ilu_Alarmette;

/*L2 unconstrained*/

struct _ilu_Alarmette_s {
  /*L1 >= {mxamu}*/
  
  ilu_Alarmette al_next, al_prev;	/* list links */
  ilu_boolean  al_set;			/* in queue? */
  ilu_FineTime al_trigger;		/* when to execute */
};
/* A data structure common to all alarms multiplexed into other alarms.
   When set==TRUE, next and prev are non-NIL; when set==FALSE,
   they are NIL.  Initialize set to FALSE and next and prev to NIL. */

typedef struct {
  /* L1 >= {mxamu} for access and invocations */

  ilu_Alarmette   ar_head;	/* of queue of alarmettes yet to
				 * trigger */
  /*
   * for calling: L1_sup = mxamu, & other things true of
   * ilu_MXAProc(..)
   */
  void            (*ar_invoke) (ilu_Alarmette a); /* invoke one now */
  void            (*ar_set) (ilu_FineTime t);	/* schedule a call on
						 * ilu_MXAProc */
  void            (*ar_cancel) (void);	/* cancel that */
}               ilu_AlarmRep;
/*
 * Data and procedures provided by the client for multiplexing a
 * set of Alarmettes onto one alarm.  ar_head->al_next and
 * ar_head->al_prev are initialized to ar_head.  ar_invoke invokes
 * the given Alarmette.  ar_set and ar_cancel manipulate the one
 * alarm into which Alarmettes are multiplexed by the procedures
 * below.  (*ar->ar_set)(t) means the client should call
 * ilu_MXAProc(u, ar), once, soon after time t arrives; t <= u <=
 * the time of the call on ilu_MXAProc.  Larger (valid) values of u
 * are better than smaller ones, but not a lot better.  If ar_set
 * is called again before ilu_MXAProc, this changes the time at
 * which ilu_MXAProc should be called.  A call on ar_set after
 * ilu_MXAProc schedules a new call on ilu_MXAProc.  ar_cancel
 * cancels the pending call to ilu_MXAProc, if any.
 */

/*L1 >= {mxamu}*/

ILU_PUBLIC void ilu_MXASet(ilu_AlarmRep *ar, ilu_Alarmette a, ilu_FineTime t);
/* Schedule (*ar->invoke)(a) to happen ASAP after t;
   this replaces any other scheduled invocation of a. */

ILU_PUBLIC void ilu_MXAClear(ilu_AlarmRep *ar, ilu_Alarmette a);
/* Cancel the scheduled invocation of a, if any. */

/*L1_sup = mxamu*/
ILU_PUBLIC void ilu_MXAProc(ilu_FineTime u, ilu_AlarmRep *ar);
/*
 * The client arranges to call ilu_MXAProc(t, ar), once, soon after
 * time t, in response to a call (*ar->ar_set)(t), as described
 * above.  ilu_MXAProc(t, ar) calls (*ar->ar_invoke)(a) for the
 * appropriate Alarmettes a.
 */


/* ================ (Un)Marshalling routines ================ */

/* End brackets for [un]marshalling and size routines;
 * call these after the contents introduced by a call on
 * (Output|Input|SizeOf)(Sequence|Union|Array|Record).
 */
/*L1, L2 unconstrained*/

ILU_PUBLIC ilu_boolean ilu_EndSequence(ilu_Call call, ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_boolean ilu_EndUnion(ilu_Call call, ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_boolean ilu_EndArray(ilu_Call call, ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_boolean ilu_EndRecord(ilu_Call call, ILU_ERRS((IoErrs)) *err);

/* Marshalling routines */
/**before: Main Invariant, Call-Hi(call);
    after: Call-Invariant(call, err),
	   success => Call-Hi(call)*/

ILU_PUBLIC void
ilu_OutputShortInteger(ilu_Call call, ilu_shortinteger i,
		       ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_OutputInteger(ilu_Call call, ilu_integer i,
		  ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_OutputLongInteger(ilu_Call call, ilu_longinteger i,
		      ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_OutputShortCardinal(ilu_Call call, ilu_shortcardinal i,
			ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_OutputCardinal(ilu_Call call, ilu_cardinal i,
		   ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_OutputLongCardinal(ilu_Call call, ilu_longcardinal i,
		       ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_OutputShortReal(ilu_Call call, float f,
		    ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_OutputReal(ilu_Call call, double d,
	       ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_OutputLongReal(ilu_Call call, ilu_longreal f,
		   ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_OutputEnum(ilu_Call call, ilu_shortcardinal i,
	       ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_OutputCharacter(ilu_Call call, ilu_character i,
		    ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_OutputByte(ilu_Call call, ilu_byte b,
	       ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_OutputShortCharacter (ilu_Call call, ilu_shortcharacter b,
			  ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_OutputBoolean(ilu_Call call, ilu_boolean b,
		  ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_OutputOptional(ilu_Call call, ilu_boolean optionalStatus,
		   ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void
ilu_OutputSequence(ilu_Call call, ilu_cardinal len,
		   ilu_cardinal limit, ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_OutputSequenceMark(ilu_Call call,
		       ilu_cardinal extent,
		       ILU_ERRS((IoErrs)) * err);
/* Call this every 2^16-1 elements.  ??? What's extent ??? */

ILU_PUBLIC void 
ilu_OutputUnion(ilu_Call call, ilu_cardinal discriminator,
		ilu_cardinal discriminator_size,
		ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_OutputArray(ilu_Call call, ilu_cardinal length,
		ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_OutputRecord(ilu_Call call, ILU_ERRS((IoErrs)) * err);

/* Output an OPTIONAL X or an X; `optional` indicates
 * which.  If `optional`, the value may be NIL. */
ILU_PUBLIC void 
ilu_OutputString(ilu_Call call, ilu_string s,
		 ilu_cardinal len, ilu_cardinal limit,
		 ILU_ERRS((IoErrs)) * err);
    /* Variable-length array of short character. */

ILU_PUBLIC void 
ilu_OutputStringVec(ilu_Call call, ilu_string s,
		    ilu_cardinal len,
		    ILU_ERRS((IoErrs)) * err);
    /* Fixed-length array of short character. */

ILU_PUBLIC void
ilu_OutputWString(ilu_Call,
		  ilu_wstring,		/* the string */
		  ilu_cardinal len,	/* len of the string */
		  ilu_cardinal limit,	/* limit for the parm */
		  ILU_ERRS((IoErrs)) * err);
    /* Variable-length array of character. */

ILU_PUBLIC void
ilu_OutputWStringVec(ilu_Call,
		     ilu_wstring,	/* the vector of wchars */
		     ilu_cardinal,	/* len */
		     ILU_ERRS((IoErrs)) * err);
    /* Fixed-length array of character. */

ILU_PUBLIC void 
ilu_OutputBytes(ilu_Call call, ilu_bytes o,
		ilu_cardinal len, ilu_cardinal limit,
		ILU_ERRS((IoErrs)) * err);
    /* Variable-length array of byte. */

ILU_PUBLIC void 
ilu_OutputOpaque(ilu_Call call, ilu_opaque o,
		 ilu_cardinal len,
		 ILU_ERRS((IoErrs)) * err);
    /* Fixed-length array of byte. */

/**L2 >= {call's conn's callmu, iomu}.
  obj == NIL => Main Invariant holds.
  obj != NIL => all the following:
  before: Inside(s, cl);
  after:				 L1 disjoint {cmu, s};
  after: cl collectible		      => L1  not >=  {gcmu};
  after: cl collectible & s surrogate => Main Invariant holds;
  where s = obj's server and cl = obj's type.
  (We don't really need to hold cmu for surrogate or non-collectible
   objects, but this is convenient because ilu_Enter/ExitServer can
   be used.)*/
ILU_PUBLIC void
ilu_OutputObjectID(ilu_Call call, ilu_Object obj,
		   ilu_boolean discriminator_p,
		   ilu_Class static_type,
		   ILU_ERRS((IoErrs)) * err);
/* Output a object; `discriminator_p` iff in discriminator position. */

/* Un-marshalling routines */
/**before: Main Invariant, Call-Hi(call);
    after: Call-Invariant(call, err),
	   success => Call-Hi(call)*/

ILU_PUBLIC void 
ilu_InputShortInteger(ilu_Call call,
		      ilu_shortinteger * i,
		      ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_InputInteger(ilu_Call call, ilu_integer * i,
		 ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_InputLongInteger(ilu_Call call,
		     ilu_longinteger * i,
		     ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_InputShortCardinal(ilu_Call call,
		       ilu_shortcardinal * i,
		       ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_InputCardinal(ilu_Call call, ilu_cardinal * i,
		  ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_InputLongCardinal(ilu_Call call,
		      ilu_longcardinal * i,
		      ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_InputShortReal(ilu_Call call, float *f,
		   ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_InputReal(ilu_Call call, double *d,
	      ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_InputLongReal(ilu_Call call, ilu_longreal * f,
		  ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_InputEnum(ilu_Call call,
	      ilu_shortcardinal * i,
	      ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_InputCharacter(ilu_Call call,
		   ilu_character * i,
		   ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_InputByte(ilu_Call call, ilu_byte * b,
	      ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_InputShortCharacter (ilu_Call call, ilu_shortcharacter * b,
			 ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_InputBoolean(ilu_Call call, ilu_boolean * b,
		 ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_InputOptional(ilu_Call call,
		  ilu_boolean * optionalStatus,
		  ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_InputSequence(ilu_Call call,
		  ilu_cardinal * len, ilu_cardinal limit,
		  ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_InputSequenceMark(ilu_Call call,
		      ilu_cardinal extent,
		      ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void
ilu_InputUnion(ilu_Call call, ilu_cardinal * discriminator,
	       ilu_cardinal discriminator_size,
	       ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_InputArray(ilu_Call call,
	       ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_InputRecord(ilu_Call call, ILU_ERRS((IoErrs)) * err);

ILU_PUBLIC void 
ilu_InputString(ilu_Call call, ilu_string * s,
		ilu_cardinal * len, ilu_cardinal limit,
		ILU_ERRS((IoErrs)) * err);
/*
 * Input a (variable-length, by definition) sequence of short
 * characters; a terminating NUL is appended.  (s) and (len) are OUT
 * parameters, through which the length and base pointer will be
 * returned.  When length is 0, a non-null base pointer is returned.
 */

ILU_PUBLIC void 
ilu_InputStringVec(ilu_Call call, ilu_string * s, ilu_cardinal len,
		   ILU_ERRS((IoErrs)) * err);
/*
 * Input a (fixed-length, by definition) array of short characters.
 * Caller may pass non-null (*s), in which case the characters will
 * be stored there; otherwise, callee will allocate the necessary
 * memory, and store a pointer to it in (*s).
 */

ILU_PUBLIC void 
ilu_InputWString(ilu_Call call, ilu_wstring * s,
		 ilu_cardinal * len, ilu_cardinal limit,
		 ILU_ERRS((IoErrs)) * err);
/*
 * Input a (variable-length, by definition) sequence of characters;
 * a terminating NUL is appended.  (s) and (len) are OUT parameters,
 * through which the length and base pointer will be returned.  When
 * length is 0, a non-null base pointer is returned.
 */

ILU_PUBLIC void 
ilu_InputWStringVec(ilu_Call call, ilu_wstring * s,
		    ilu_cardinal len, ILU_ERRS((IoErrs)) * err);
/*
 * Input a (fixed-length, by definition) array of characters.
 * Caller may pass non-null (*s), in which case the characters will
 * be stored there; otherwise, callee will allocate the necessary
 * memory, and store a pointer to it in (*s).
 */

ILU_PUBLIC void 
ilu_InputOpaque(ilu_Call call, ilu_opaque * o,
		ilu_cardinal len, ILU_ERRS((IoErrs)) * err);
/*
 * Input a (fixed-length, by definition) array of bytes.  Caller may
 * pass non-null (*s), in which case the bytes will be stored there;
 * otherwise, callee will allocate the necessary memory, and store a
 * pointer to it in (*s).
 */

ILU_PUBLIC void 
ilu_InputBytes(ilu_Call call, ilu_bytes * o,
	       ilu_cardinal * len, ilu_cardinal limit,
	       ILU_ERRS((IoErrs)) * err);
/*
 * Input a (variable-length, by definition) sequence of bytes; a
 * terminating NUL is appended.  (o) and (len) are OUT parameters,
 * through which the length and base pointer will be returned.  When
 * the length is 0, NULL might be returned as the base poiner.
 */


/**Main Remnant holds, L2 >= {call's connection's callmu, iomu};
  before: L1 = {},
  after:  *o!=NIL => Inside(*o's server, static_type);
  after:  *o==NIL => L1 = {};
  after:  ILU_ERRNOK(*err) => *o==NIL*/
ILU_PUBLIC void
ilu_InputObjectID(ilu_Call call, ilu_Object * o,
		  ilu_boolean discriminator_p, ilu_Class static_type,
		  ILU_ERRS((IoErrs)) * err);
/* static_type is not NIL.
   Afterward, if *o!=NIL && ilu_GetLanguageSpecificObject(*o)==NIL,
   the caller will invoke ilu_RegisterLanguageSpecificObject
   on *o before unlocking the server. */


/* Size-computing routines */
/*Main Invariant holds; L2 otherwise unconstrained*/

ILU_PUBLIC ilu_cardinal ilu_SizeOfShortInteger(ilu_Call call,
					   ilu_shortinteger i,
					   ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfInteger(ilu_Call call,
				      ilu_integer i,
				      ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfLongInteger(ilu_Call call,
					  ilu_longinteger i,
					  ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfShortCardinal(ilu_Call call,
					    ilu_shortcardinal i,
					    ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfCardinal(ilu_Call call,
				       ilu_cardinal i,
				       ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfLongCardinal(ilu_Call call,
					   ilu_longcardinal i,
					   ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfShortReal(ilu_Call call,
					float d, ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfReal(ilu_Call call, double d,
				   ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfLongReal(ilu_Call call,
				       ilu_longreal d,
				       ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfEnum(ilu_Call call,
				   ilu_shortcardinal i,
				   ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfCharacter(ilu_Call call,
					ilu_character i,
					ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfByte(ilu_Call call, ilu_byte i,
				   ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfShortCharacter(ilu_Call call, ilu_shortcharacter i,
					     ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfBoolean(ilu_Call call, ilu_boolean i,
				      ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfOptional(ilu_Call call,
				       ilu_boolean optionalStatus,
				       ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfSequence(ilu_Call call, ilu_cardinal len,
				       ilu_cardinal limit,
				       ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfUnion(ilu_Call call,
				    ilu_cardinal discriminator,
				    ilu_cardinal discriminator_size,
				    ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfArray(ilu_Call call,ilu_cardinal length,
				    ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfRecord(ilu_Call call,
				     ILU_ERRS((IoErrs)) *err);

ILU_PUBLIC ilu_cardinal ilu_SizeOfString(ilu_Call call, ilu_string i,
				    ilu_cardinal l, ilu_cardinal limit,
				    ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfStringVec(ilu_Call call, ilu_string i,
				        ilu_cardinal l,
				        ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfWString(ilu_Call call, ilu_wstring i,
				      ilu_cardinal l, ilu_cardinal limit,
				      ILU_ERRS((IoErrs)) *err);
ILU_PUBLIC ilu_cardinal ilu_SizeOfWStringVec(ilu_Call call, ilu_wstring i,
					 ilu_cardinal l,
					 ILU_ERRS((IoErrs)) *err);

ILU_PUBLIC ilu_cardinal ilu_SizeOfOpaque(ilu_Call call, ilu_opaque o,
				     ilu_cardinal l,
				     ILU_ERRS((IoErrs)) *err);

ILU_PUBLIC ilu_cardinal ilu_SizeOfBytes(ilu_Call call, ilu_bytes o,
				    ilu_cardinal l, ilu_cardinal limit,
				    ILU_ERRS((IoErrs)) *err);

/*Main Remnant holds.
  before: obj!=NIL => L1 = {obj's server};
	  obj==NIL => L1 = {}.
  after:  exn      => L1 = {} if possible; else
	  obj!=NIL => L1 = {obj's server}; else
	  obj==NIL => L1 = {}.*/
ILU_PUBLIC ilu_cardinal ilu_SizeOfObjectID(ilu_Call call, ilu_Object obj,
				       ilu_boolean discriminator_p,
				       ilu_Class static_type,
				       ILU_ERRS((IoErrs)) *err);


/* ================ Simple Binding ================ */

/*before: Inside(s, cl)
  after:				 L1 disjoint {cmu, s};
  after: cl collectible		      => L1  not >=  {gcmu};
  after: cl collectible & s surrogate => Main Invariant holds;
  where s = obj's server and cl = obj's type.
  (We don't really need to hold cmu for surrogate or non-collectible
   objects, but this is convenient because ilu_Enter/ExitServer can
   be used.)*/
ILU_PUBLIC /* OPTIONAL */ char *ilu_PublishObject(ilu_Object obj);
/*
 * Publishes the SBH of the object in the local object domain.
 * Returns an "ownership proof", a string which must be supplied to
 * withdraw the object
 */

/*before: Inside(s, cl)
  after:				 L1 disjoint {cmu, s};
  after: cl collectible		      => L1  not >=  {gcmu};
  after: cl collectible & s surrogate => Main Invariant holds;
  where s = obj's server and cl = obj's type.
  (We don't really need to hold cmu for surrogate or non-collectible
   objects, but this is convenient because ilu_Enter/ExitServer can
   be used.)*/
ILU_PUBLIC          ilu_boolean
ilu_WithdrawObject(ilu_Object obj,
		    /* PASS */ char *ownership_proof);
/*
 * Withdraws the object "obj", if "ownership_proof" is that
 * returned when the object was registered.
 */

/*before: L1 = {};
  after:  result!=NIL => Inside(result's server, pclass);
  after:  result==NIL => L1 = {};
  forall conn: (L2 >= {conn.iomu}) => (L2 >= {conn.callmu});
  Main otherwise unconstrained */
ILU_PUBLIC /* OPTIONAL */ ilu_Object
ilu_LookupObject(char *sid, char *ih,
		 ilu_Class pclass);
/*
 * Attempts to find in the local domain the object identified by the
 * given server ID and server-relative Instance Handle.  "pclass" is
 * a type the caller knows the object to have.  Returns NIL on
 * failure.  Causes the kernel to reconsider which
 * contact info it wants to use for the identified server.
 */

/*before: L1 = {};
  after:  result!=NIL => Inside(result's server, pclass);
  after:  result==NIL => L1 = {};
  forall conn: (L2 >= {conn.iomu}) => (L2 >= {conn.callmu});
  Main otherwise unconstrained */
ILU_PUBLIC          ilu_boolean
ilu_ReLookupObject(char *sid, char *ih,
		   ilu_Class pclass,
		    /* OPTIONAL */ ilu_Object * po);
/*
 * Like ilu_LookupObject, but result indicates whether the kernel
 * changed its choice of contact info.  The object (or NIL) is
 * returned at *po; po may not be NIL, but *po need not be
 * initialized in any way.
 */

/* ================ Identities and Passports ================ */

struct _ilu_IdentityInfo_s {
  ilu_IdentityType ii_type;
  ilu_boolean ii_owned_by_passport;
  ilu_refany ii_info;
};

struct _ilu_IdentityType_s {

  char *it_name;	/* constant string */

  ilu_cardinal				/* size of return string */
    (*it_string_form) (ilu_IdentityInfo,/* instance, retain */
		       char *,		/* address of caller buffer */
		       ilu_cardinal,	/* size of caller buffer */
		       ILU_ERRS((internal, bad_param)) *);
  /* formats a string form of the IdentityInfo into the caller-supplied
     buffer.  If the buffer is too small for the identity, it may either
     truncate the identity and return success, or signal an error */

  ilu_refany
    (*it_duplicate_data) (ilu_IdentityInfo,	/* instance, retain */
			  ILU_ERRS((no_memory, internal)) *);
  /* returns a deep copy of the argument. */

  void
    (*it_free_data) (ilu_IdentityInfo,
		     ILU_ERRS((internal)) *);
  /* frees any associated data structure */
      

  /*-------------------*/
  /* the following two methods may be either NULLFN, or defined.
     If not defined, this identity type can not be transported
     arbitrarily across the wire.  Otherwise, it can be, by protocols
     which do that sort of thing. */

  ilu_cardinal				/* size of pickled info */
    (*it_pickle) (ilu_IdentityInfo,	/* instance, retain */
		  ilu_bytes *,		/* caller buffer, retain */
		  ilu_Error *);
  /* writes identity info into buffer in way that can be recovered
     by a call on "it_unpickle", but is otherwise unconstrained.
     if the buffer is insufficient for pickling, should raise no_memory
     and return the suggested number of bytes to call this with.
     The pickled form should always take fewer than 0x10000 bytes. */

  ilu_IdentityInfo
    (*it_unpickle) (ilu_bytes,		/* buffer to unpickle, retain */
		    ilu_cardinal,	/* len of pickled form */
		    ilu_Error *);
  /* creates an identity info from the data in the buffer, and returns
     it. */
};

ILU_PUBLIC ilu_boolean
  ilu_RegisterIdentityType (ilu_IdentityType,
			    ilu_Error *);
/* make the identity type known to the ILU runtime, so that
   the generic identity operators (below) will work properly on it. */

/* 'standard' identity types */

ILU_PUBLIC struct _ilu_IdentityType_s	ilu_NoIdentity_s;
#define ilu_NoIdentity (&ilu_NoIdentity_s)
typedef void * ilu_NoIdentityInfo;

ILU_PUBLIC struct _ilu_IdentityType_s	ilu_ConnectionIdentity_s;
#define ilu_ConnectionIdentity (&ilu_ConnectionIdentity_s)
typedef ilu_string ilu_ConnectionIdentityInfo;

#ifdef SECURE_TRANSPORT

#include <gssapi.h>

ILU_PUBLIC struct _ilu_IdentityType_s	ilu_GSSIdentity_s;
#define ilu_GSSIdentity (&ilu_GSSIdentity_s)

typedef struct _ilu_GSSIdentityInfo_s * ilu_GSSIdentityInfo;

ILU_PUBLIC ilu_IdentityInfo
  ilu_AcquireGSSIdentity (gss_cred_id_t, ilu_Error *);

ILU_PUBLIC ilu_boolean
  ilu_DecodeGSSIdentity (ilu_IdentityInfo,	/* input; retain; info to decode */
			 gss_name_t *,		/* output; name in identity */
			 ilu_FineTime *,	/* output; good-till; seconds past Unix epoch */
			 gss_OID,		/* input; actual mechanism desired; optional */
			 ilu_boolean *,		/* if TRUE, local; otherwise remote */
			 ilu_cardinal *,	/* connection flags, as in gss_inquire_context */
			 ilu_Error *);

ILU_PUBLIC gss_cred_id_t
  ilu_AcquireGSSCredForName (char *,		/* name */
			     ilu_cardinal,	/* lifetime */
			     gss_OID,		/* secmech */
			     ilu_boolean,	/* accept_only */
			     ilu_Error *	/* err */);
ILU_PUBLIC ilu_string
  ilu_GSSNameToString (gss_name_t,
		       ilu_Error *err);

#endif /* def SECURE_TRANSPORT */

#ifdef SUNRPC_PROTOCOL

ILU_PUBLIC struct _ilu_IdentityType_s	ilu_SunRPCAuthUnixIdentity_s;
#define ilu_SunRPCAuthUnixIdentity (&ilu_SunRPCAuthUnixIdentity_s)

typedef struct {
  ilu_shortcardinal ii_UID;
  ilu_shortcardinal ii_GID;
  ilu_string      ii_hostname;
  ilu_shortcardinal ii_ngids;
  ilu_shortcardinal *ii_gids;
} *ilu_SunRPCAuthUnixIdentityInfo;

ILU_PUBLIC ilu_IdentityInfo
  ilu_GetSunRPCAuthUnixIdentityInfo (ilu_Error *);
/* returns identity info for the current user */

#endif				/* SUNRPC_PROTOCOL */

/*L1, L2, Main unconstrained*/

ILU_PUBLIC ilu_boolean
  ilu_RegisterIdentityType (struct _ilu_IdentityType_s *,	/* pass */
			    ilu_Error *);

ILU_PUBLIC ilu_IdentityType
  ilu_FindIdentityTypeByName (char *,
			      ilu_Error *);
/* returns the identity type specified by the first parm,
   or NIL if there is no identity type by that name */

ILU_PUBLIC ilu_Passport /* pass, optional */
  ilu_CreatePassport (const struct _ilu_IdentityInfo_s *,	/* optional, pass */
		      ILU_ERRS((no_memory)) *);
/* creates and returns a passport, optionally containing the specified identity */

ILU_PUBLIC ilu_IdentityInfo
  ilu_CopyIdentity (const struct _ilu_IdentityInfo_s *,
		    ILU_ERRS((no_memory)) *);
/* allocates and returns a copy of the ilu_IdentityInfo parameter */

ILU_PUBLIC ilu_boolean
  ilu_AddIdentity (ilu_Passport /* retain */,
		   const struct _ilu_IdentityInfo_s *,
		   ilu_Error *);
/* added identity to Passport.  Only one identity of each type is allowed.
   Returns ILU_ERROK() of the error parameter. */

ILU_PUBLIC ilu_IdentityInfo /* optional, retain */
  ilu_FindIdentity (ilu_Passport /* retain */,
		    ilu_IdentityType);
/* return identity of specified type, if present.  Returns NIL if not present. */

ILU_PUBLIC ilu_boolean
  ilu_PickleIdentity (ilu_IdentityInfo,	/* in; retain */
		      ilu_bytes *,	/* out; pass; pickled data */
		      ilu_cardinal *,	/* out; pickled data len */
		      ilu_Error *);
/* returns true if pickling succeeds; false otherwise.
   may return false without signalling an error if the identity
   type of the ilu_IdentityInfo doesn't define a pickling
   method */

ILU_PUBLIC ilu_IdentityInfo
  ilu_UnpickleIdentity (ilu_IdentityType,
			ilu_bytes,	/* in; retain; pickled data */
			ilu_cardinal,	/* in; pickled data len */
			ilu_Error *);
/* returns an ilu_IdentityInfo if the pickling succeeds,
   returns NIL if ilu_IdentityType doesn't defined an unpickling
   method, or if an error occurs. */

ILU_PUBLIC ilu_boolean
  ilu_DestroyPassport (ilu_Passport /* pass */,
		       ilu_Error * /* retain */);
/* frees any associated identities, and if free_passport is specified,
   calls ilu_free() on the ilu_Passport arg. */

/* ================ Other routines ================ */

/* ================ URL Syntax ============ */
/*
  Basically, an ILU URL has the form

  `ilu:'<sid>`/'<ih>ILU_TYPE_MARKER<mstid>(ILU_CINFO_MARKER<cinfo>)+

  where <cinfo> is of the form

  <pinfo>ILU_CINFO_DIVIDER<tinfo>

  and <tinfo> is of the form

  <tfilter>(ILU_TINFO_DIVIDER<tfilter>)*

  But we have to define what these characters are... */

#define ILU_TYPE_MARKER		';'
#define ILU_CINFO_MARKER	';'
#define ILU_CINFO_DIVIDER	'@'
#define ILU_TINFO_DIVIDER	'='

/* ======================================== */

/*L1, L2 unconstrained*/

typedef ilu_boolean (*ilu_SBHParser)(ilu_string,	/* encoded SBH */
				     ilu_string *,	/* plain instance handle (opt) */
				     ilu_string *,	/* plain server ID (opt) */
				     ilu_string *,	/* plain MSTID (opt) */
				     ilu_string *,	/* encoded contact info (opt) */
				     ilu_cardinal *,	/* encoded contact info len (opt) */
				     ILU_ERRS((no_memory, internal, inv_objref)) *);

ILU_PUBLIC void
  ilu_RegisterSBHParser (ilu_string,		/* scheme name */
			 ilu_SBHParser);	/* parser for that scheme */

ILU_PUBLIC          ilu_boolean
  ilu_ParseSBH(ilu_string /* URL (encoded, of course) */ ,
	       ilu_string * /* plainInstH (opt) */ ,
	       ilu_string * /* plainServerID (opt) */ ,
	       ilu_string * /* plainMstid (opt) */ ,
	       ilu_string * /* encodedContactInfo (opt) */ ,
	       ilu_cardinal * /* encodedContactInfoLen (opt) */ ,
	       ILU_ERRS((no_memory, internal, inv_objref)) *);
/*
 * Parse an SBH (==ILU URL), returning whichever elements are
 * specified by passing in non-NIL pointers.  The whole sequence of
 * contact info.s is returned in *encodedContactInfo.  Caller
 * retains ownership of URL argument.  If ih != NIL, ownership of
 * *ih is passed to caller iff successful.  Similarly for
 * plainServerID and plainMstid. *encodedContactInfo is set to point
 * into the given URL, and *encodedContactInfoLen is set to the
 * length of the contact info.s substring; the next character is
 * left unmolested.
 */

/*Main Invariant holds; L2 otherwise unconstrained*/
ILU_PUBLIC ilu_boolean 
ilu_PingObject(ilu_Object o,
	       ilu_Connection * new_conn);
/*
 * Returns ilu_TRUE if the true object exists, and the process
 * serving it can be contacted; ilu_FALSE otherwise.  May return a
 * new outgoing connection to monitor (a la ilu_StartCall).
 */

/*L1 >= {obj's server}; L1_sup < prmu*/
ILU_PUBLIC ilu_string ilu_SBHOfObject( ilu_Object obj );
/*
 * Ownership of result is retained by callee, which guarantees the
 * string to be valid only until the server mutex is exited.  May
 * return NIL if the object's server isn't exported through any
 * port; may return an invalid SBH if the cached one references a
 * closed port.
 */

#ifdef IIOP_PROTOCOL

/* (obj!=NIL) => Inside(object_server(obj), object_class(obj) */
ILU_PUBLIC ilu_string ilu_IOROfObject (ilu_Object /* obj */,
				       ilu_Error * /* errp */);
/* String result owned by caller.
 * Returns OMG IIOP-specified IOR string for object.  May return
 * NIL if object is not exported through an IIOP ilu_Port.
 */

#endif /* IIOP_PROTOCOL */

ILU_PUBLIC ilu_string ilu_MstidOfObject( ilu_Object obj );
/*
 * Returns the ID of the most specific type of the given object.
 * Storage for result owned by the object
 * (i.e., freed when the object is freed).  Caller should thus hold
 * obj's server in order to know the object won't be freed upon
 * return.
 */

ILU_PUBLIC ilu_string ilu_GetILUVersion(void);

ILU_PUBLIC ilu_cardinal ilu_GetILUMajorVersion(void);

ILU_PUBLIC ilu_cardinal ilu_GetILUMinorVersion(void);

ILU_PUBLIC ilu_cardinal ilu_IDOfMethod(ilu_Method method);

ILU_PUBLIC ilu_cardinal ilu_ExceptionCountOfMethod(ilu_Method method);

ILU_PUBLIC ilu_string ilu_NameOfMethod(ilu_Method method);

ILU_PUBLIC void ilu_SetMethodStubProc(ilu_Method method, ilu_StubProc proc);

ILU_PUBLIC ilu_refany ilu_GetMethodStubProc(ilu_Method method);

ILU_PUBLIC ilu_Method ilu_FindMethodByID( ilu_Class /* intro_type */,
					  ilu_cardinal /* ID */);

ILU_PUBLIC ilu_Method ilu_MethodOfCall(ilu_Call);

ILU_PUBLIC ilu_Connection ilu_ConnectionOfCall(ilu_Call);

ILU_PUBLIC ilu_boolean ilu_CallNeedsSizing(ilu_Call);
/* returns TRUE if this call requires accurate argument sizes
   to be furnished for ilu_StartCall, ilu_BeginReply, or
   ilu_BeginException.  If FALSE, 0 may be passed as an
   argument size. */

ILU_PUBLIC ilu_Class ilu_IntroTypeOfCall(ilu_Call);

ILU_PUBLIC /* OPTIONAL */ ilu_Passport ilu_CallerPassportOfCall(ilu_Call call);
/* in true method stub, returns the caller's passport, if any.
   The reference is valid throughout the call */

ILU_PUBLIC void ilu_SetCallerPassportOfCall(ilu_Call, ilu_Passport);
/* Sets the passport of the call to the specified Passport.
   For use by client stubs. */

ILU_PUBLIC ilu_Server ilu_ServerOfConnection(ilu_Connection);

ILU_PUBLIC ilu_string ilu_IDOfServer( ilu_Server );

ILU_PUBLIC ilu_cardinal ilu_CRC32OfIDOfServer ( ilu_Server );

ILU_PUBLIC ilu_boolean ilu_TrueServerP( ilu_Server );
/* Is this server true for some language? */


/*L1, L2, Main unconstrained*/
/*(But be careful about holding directly onto an ilu_Object)*/

ILU_PUBLIC ilu_Class ilu_ClassOfObject( ilu_Object obj );

ILU_PUBLIC ilu_boolean ilu_TrueInstanceP( ilu_Object obj );
/* Is this object true for some language? */

ILU_PUBLIC ilu_Server ilu_ServerOfObject( ilu_Object obj );

ILU_PUBLIC ilu_string ilu_IhOfObject( ilu_Object o );
/* Return the ih of the given object; result is owned by the object.
   This could be useful when mapping a disassociated true KO
   to a LSO (ih is what's needed to consult the objtab). */

#ifdef ILU_BINDING_HOST

/****************************** from sbilu.c ********************/

/* These are used by the sbilu server code, so we export them,
   but they should still be considered private to the ILU system! */

/*L1, L2, Main unconstrained */

ILU_PUBLIC ilu_boolean ilu_GetSimpleBindingSBH
  (char *,		/* buffer to receive SBH */
   ilu_cardinal		/* size of the buffer */
   );

ILU_PUBLIC ilu_boolean ilu_GetSBServiceParms
  (char *,		/* buffer to receive realm name (min 1100 bytes) */
   char *,		/* buffer to receive host name (min 1100 bytes) */
   ilu_shortcardinal *	/* 16-bit int pointer to receive port */
   );

#endif /* def ILU_BINDING_HOST */

/****************************** from crc32.c ********************/

/* These are used by the sbilu server code, so we export them,
   but they should still be considered private to the ILU system! */

/*L1, L2, Main unconstrained */

ILU_PUBLIC ilu_cardinal _ilu_CRC32
  (ilu_bytes,		/* buffer to take the CRC-32 of, retain */
   ilu_cardinal);	/* length of buffer */

ILU_PUBLIC ilu_cardinal _ilu_CRC32WithAccum
  (ilu_bytes,		/* buffer to take the CRC-32 of, retain */
   ilu_cardinal,	/* length of buffer */
   ilu_cardinal);	/* accumulated CRC returned from previous call to
			   _ilu_CRC32 or _ilu_CRC32WithAccum */

/*============ from pipe.c =========================*/

/*L1, L2 not designed yet*/

ILU_PUBLIC ilu_string ilu_Pipe_Type( ilu_Pipe pipe );

ILU_PUBLIC ilu_boolean ilu_Pipe_OtherEndGone( ilu_Pipe pipe );

ILU_PUBLIC ilu_boolean ilu_Pipe_WillBlock( ilu_Pipe pipe );

ILU_PUBLIC void ilu_Pipe_Destroy( ilu_Pipe pipe );

ILU_PUBLIC ilu_Pipe ilu_Pipe_Create( ilu_string contact_info,
				     ilu_string iluTypeName,
				     ilu_boolean sink_p );

ILU_PUBLIC ilu_boolean ilu_Pipe_Open( ilu_Pipe pipe );

ILU_PUBLIC ilu_Call ilu_Pipe_Call( ilu_Pipe pipe );

/* ==================================== from gc.c */

/* ======== Server Side ======== */

/*L2, Main unconstrained*/

/*Inside(obj's server, obj's type)*/
typedef void (*ilu_ObjectNoter)(ilu_Object obj,
				int vi);
/*
 * The LS runtime provides this procedure.  The kernel calls this
 * procedure when the kernel becomes, or ceases being, very
 * interested in an object.  The LS runtime uses this opportunity to
 * keep a LS object around as long as there are surrogates.  This
 * procedure should not call anything [eg, RegLSO(obj, NIL)] that
 * might free the object.
 */

/*L1_sup < cmu*/
ILU_PUBLIC void ilu_SetNoter(ilu_ObjectNoter n, ilu_LanguageIndex language);
/* Each LS runtime in the address space calls this at most once,
 * before any objects are created. */

/*L1 >= {obj's server};
  obj true && collectible => L1 >= {gcmu}*/
ILU_PUBLIC ilu_boolean ilu_VeryInterested(ilu_Object obj);
/* Tests whether the kernel is very interested in this object. */

/* ======== Client Side ======== */

/*L1, L2, Main unconstrained*/

ILU_PUBLIC void ilu_SetGcClient(ilu_Object interest);
/* A client of GCed objects calls this --- once, before
   RegisterLanguageSpecificObject on any GCed object. */

#ifdef WIN16
int ilu_StartupWinsock ();
#endif


/* ================ For stubs ================ */
/*
 * The following definitions are part of the contract between (C,
 * C++) stubs and kernel; they're not for application consumption.
 */

typedef struct {
  /* L1, L2 unconstrained */

  ilu_bytes       msg_base;
  ilu_cardinal    msg_len;
}               ilu_Message;
/* A contiguous copy of a whole message. */

typedef enum {
  ilu_ciosNone,			/* neither inputting nor outputting */
  ilu_ciosIn,			/* inputting */
  ilu_ciosOut			/* outputting */
}               ilu_CallIOState;
/*
 * Indicates where a call is in relation to the message framing
 * methods of its ilu_Protocol: tells whether pr_discard_output or
 * pr_discard_input should be called upon abort.
 */

typedef enum {
  ilu_cmsNo,		/*L2 disjoint {call's conn's callmu, iomu}*/
  ilu_cmsLo,		/*L2 disjoint {call's conn's iomu},
			  L2 >= {callmu} iff proto not concurrent*/
  ilu_cmsHi		/*L2 >= {call's conn's callmu, iomu}*/
}               ilu_CallMutexState;
/* Which of its connection's L2 mutexes are held by a call? */

struct _ilu_Call_s {
  /* L1, L2, Main unconstrained */

  ilu_cardinal    ca_SN;	/* serial number of request */
  ilu_Server      ca_server;	/* of call */
  ilu_Class       ca_intro_type;/* type having ca_method */
  ilu_Method      ca_method;	/* ID number of method */
  ilu_Connection  ca_connection;/* connection which points to
				 * (server or client) */
  ilu_refany      ca_private;	/* slot for lang-specific
				 * implementation's use */
  ilu_Passport    ca_caller;	/* slot for caller's identity
				 * passport */
  ilu_Passport    ca_callee;	/* slot for callee's identity (on
				 * reply) */
  ilu_boolean     ca_irq;	/* interrupt requested */
  ilu_cardinal    ca_prdata1;	/* for protocol's use */
  ilu_refany      ca_prdata2;	/* for protocol's use */
  ilu_Message     ca_msg;
  /*
   * When the transport is unreliable, a copy of the call message is
   * kept here for retransmissions.
   */
  ilu_Transport   ca_prTrans;	/* for protocol's use */
  ilu_boolean     ca_incoming;	/* true on server side */
  ilu_CallIOState ca_ios;	/* phase in pr_ calls */
  ilu_CallMutexState ca_ms;	/* which L2 mutexes held */
  ilu_ProtocolException ca_pe;	/* to return from server side */
  ilu_boolean     ca_reqs_enabled;	/* for LSR use */
};



#endif /* _ILU_EXPORTS_ */
#ifdef __cplusplus
}
#endif

