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
/* $Id: error.c,v 1.35 1996/03/29 18:44:28 janssen Exp $ */
/* $Log: error.c,v $
 * Revision 1.35  1996/03/29  18:44:28  janssen
 * Fixed typo
 *
 * Revision 1.34  1996/02/27  02:08:53  janssen
 * Removed redundant #include of iluerror.h.
 *
 * Revision 1.33  1995/12/20  23:05:48  mdavidso
 * Error macro at end of file was too much for Visual C++ 1.5.
 * Simplified in order to compile, but this is not the best solution.
 *
 * Revision 1.32  1995/12/07  17:45:13  spreitze
 * Eliminated IODs.
 * Also removed a few old unused error types.
 *
 * Revision 1.31  1995/11/22  00:10:49  spreitze
 * Changed parameters of internal/check-to-CORBA/minor conversion
 * from binary-oriented ones to decimal-oriented ones.
 *
 * Revision 1.30  1995/11/17  17:18:28  spreitze
 * Changed formula for combining hash(filename) and lineno
 * for internal/check-to-CORBA conversion, by using a
 * prime modulus to eliminate collisions.
 *
 * Revision 1.29  1995/11/16  23:30:45  spreitze
 * Added ilu_Check and internal/check.
 *
 * Revision 1.28  1995/09/05  08:38:15  janssen
 * Convert gss_security properly to CORBA error.
 *
 * Revision 1.27  1995/09/05  08:32:40  janssen
 * Added gss_security.
 *
 * Revision 1.26  1995/08/25  03:08:10  janssen
 * Added minor fetch for bad_operation and bad_typecode.
 *
 * Revision 1.25  1995/08/06  04:51:35  janssen
 * Updated copyright.
 *
 * Revision 1.24  1995/07/28  23:09:00  spreitze
 * Added the ability to interrupt calls.
 *
 * Revision 1.23  1995/07/20  15:58:39  spreitze
 * Added nbytes parameter to "no_memory" error type
 *
 * Revision 1.22  1995/07/17  14:39:52  spreitze
 * Tied up some loose ends in the introduction of the current error system.
 *
 * Revision 1.21  1995/07/14  14:58:06  spreitze
 * Made SizeOfXXX, EndXXX routines use error system.
 * Eliminated CommunicationsError.
 *
 * Revision 1.20  1995/07/07  16:29:49  spreitze
 * Fixed some locking comments.
 * Fixed speling of marshall.
 *
 * Revision 1.19  1995/07/06  23:22:39  spreitze
 * First draft with latest error system.
 *
 * Revision 1.18  1995/05/28  00:23:56  janssen
 * Added ReadError.
 *
 * Revision 1.17  1995/05/24  23:21:31  janssen
 * added MallocFailure.
 *
 * Revision 1.16  1994/12/07  03:21:25  janssen
 * Remove "unused statement" complained about by gcc.
 *
 * Revision 1.15  1994/12/06  07:34:17  janssen
 * Added BadSBH and BadURL.
 *
 * Revision 1.14  1994/12/05  21:43:13  spreitze
 * Replaced "NULL" with "NIL";
 * "Last tweaked by" with "Last edited by".
 *
 * Revision 1.13  1994/11/17  08:29:08  janssen
 * Updated for new OS partitioning
 *
 * Revision 1.12  1994/11/04  05:16:56  spreitze
 * Eliminated the one method of the GC callback class.
 * Other minor improvements, mainly in error reporting.
 *
 * Revision 1.11  1994/10/13  20:01:36  spreitze
 * "free" -> "ilu_free";
 * ilu_ParseOID==0 doesn't imply non-null parts;
 * moved co_protocol_data into the variable part of a connection,
 * to be accessed only under the connection's iomu ---
 * so we can NULL it out when we free the protocol data block.
 *
 * Revision 1.10  1994/10/06  14:37:16  spreitze
 * Added #include <string.h> for memset.
 *
 * Revision 1.9  1994/09/20  02:13:57  janssen
 * Added new errors for use by marshalling code.
 *
 * Revision 1.8  1994/09/05  21:19:19  spreitze
 * Changed malloc calls to ilu_malloc calls.
 * Liberalized SetLockTech.
 * Generalize AddRegisterersToDefault
 * Renamed RegisterLockTech to SetLockTech.
 *
 * Revision 1.7  1994/08/01  17:18:03  spreitze
 * Added some errors internal to the kernel.
 * Small changes and additions to error system.
 *
 * Revision 1.6  1994/07/25  15:38:09  spreitze
 * Error system revision; header file rationalization.
 *
 * Revision 1.5  1994/07/06  07:10:14  janssen
 * Changed to allow dynamic registration of protocols and transports.
 * Removed ilu_TransportType and ilu_ProtocolType.
 *
 * Revision 1.4  1994/06/23  06:59:28  janssen
 * Output buffers now responsibility of transport, rather than protocol.
 * Modifications to TransportClass to allow this.
 * Note that tr_flush_buffer now serves as end-of-message marker.
 * */
/* Last edited by Mike Spreitzer December 7, 1995 9:31 am PST */

/* Standard error machinery, and defs of standard errors */

#define _POSIX_SOURCE

#ifdef MACOS
#pragma segment ilu
#endif

#include "iluntrnl.h"

ilu_Error       ilu_success_err = {NIL, 0, 0};

static ilu_ErrorTypeDetails typeDetails[ILU_ERRTYP(ErrListLen)];

ilu_ErrorTypeDetails ilu_GetErrorTypeDetails(int et)
{
  _ilu_Assert(0 <= et && et < ILU_ERRTYP(ErrListLen),
	      "uninitialized ilu_Error?");
  if (typeDetails[et] == NIL) {
#define ILU_ERRLISTELT(id) typeDetails[ILU_ERRTYP(id)]= ILU_ERRTYPDET(id);
    ILU_ERRLIST;
#undef ILU_ERRLISTELT
    _ilu_Assert(typeDetails[et] != NIL,
		"construction of error.c:typeDetails failed");
  }
  return typeDetails[et];
}

unsigned long
ilu_CORBAizeSystemErr(ilu_Error * err,
		      ilu_integer * major)
{
  unsigned long   minor = 0;
  *major = err->ilu_type - ILU_ERRTYP(unknown);
  ILU_ERR_SWITCH(*err) {
    ILU_ERR_CASE(unknown, v)		minor = v->ignoreme*0;
    ILU_ERR_CASE(bad_param, v)		minor = v->minor;
    ILU_ERR_CASE(no_memory, v)		minor = v->nbytes;
    ILU_ERR_CASE(imp_limit, v)		minor = v->minor;
    ILU_ERR_CASE(comm_failure, v)	minor = v->minor;
    ILU_ERR_CASE(inv_objref, v)		minor = v->minor;
    ILU_ERR_CASE(no_permission, v)	minor = v->ignoreme*0;
    ILU_ERR_CASE(marshal, v)		minor = v->minor;
    ILU_ERR_CASE(initialize, v)		minor = v->ignoreme*0;
    ILU_ERR_CASE(no_implement, v)	minor = v->ignoreme*0;
    ILU_ERR_CASE(bad_typecode, v)	minor = v->minor;
    ILU_ERR_CASE(bad_operation, v)	minor = v->minor;
    ILU_ERR_CASE(no_resources, v)	minor = v->minor;
    ILU_ERR_CASE(no_response, v)	minor = v->ignoreme*0;
    ILU_ERR_CASE(persist_store, v)	minor = v->ignoreme*0;
    ILU_ERR_CASE(bad_inv_order, v)	minor = v->ignoreme*0;
    ILU_ERR_CASE(transient, v)		minor = v->ignoreme*0;
    ILU_ERR_CASE(free_mem, v)		minor = v->ignoreme*0;
    ILU_ERR_CASE(inv_ident, v)		minor = v->ignoreme*0;
    ILU_ERR_CASE(inv_flag, v)		minor = v->ignoreme*0;
    ILU_ERR_CASE(intf_repos, v)		minor = v->ignoreme*0;
    ILU_ERR_CASE(bad_context, v)	minor = v->ignoreme*0;
    ILU_ERR_CASE(obj_adapter, v)	minor = v->ignoreme*0;
    ILU_ERR_CASE(data_conversion, v)	minor = v->ignoreme*0;
    ILU_ERR_CASE(internal, v) {
      if (v->minor == ilu_im_check) {
	const char     *cf = err->ilu_file;
	char           *f = (char *) cf;
	minor = (_ilu_hash_HashString(f, 32771) * 10000
		 + err->ilu_line + 1000);
      } else
	minor = v->minor;
    }
    ILU_ERR_CASE(bad_locks, v) {
      *major = ILU_ERRTYP(internal) - ILU_ERRTYP(unknown);
      minor = ilu_im_badLocks + v->ignoreme * 0;
    }
    ILU_ERR_CASE(broken_locks, v) {
      *major = ILU_ERRTYP(internal) - ILU_ERRTYP(unknown);
      minor = ilu_im_broken + v->ignoreme * 0;
    }
    ILU_ERR_CASE(interrupted, v) {
      *major = ILU_ERRTYP(no_response) - ILU_ERRTYP(unknown);
      minor = 65536 + v->ilu_interruptSet;
    }
    ILU_ERR_CASE(gss_security, v) {
      *major = ILU_ERRTYP(no_permission) - ILU_ERRTYP(unknown);
      minor = v->major;
    }
    ILU_ERR_ELSE {
      ILU_HANDLED(*err);
      *major = -1;
      return 0;
    }
  }
  ILU_ERR_ENDSWITCH;
  ILU_HANDLED(*err);
  return minor;
}

void ilu_FreeErrp(ilu_Error *e)
{
  ilu_ErrorTypeDetails td;
  _ilu_Assert(e != NIL, "ilu_FreeErrp(NIL)");
  if (ILU_ERROK(*e))
    return;
  td = ilu_GetErrorTypeDetails(e->ilu_type);
  (*td->freeproc) (e);
  return;
}

void ilu_FreeNestedErr(ilu_Error *e)
{
  if (e != NIL) {
    ilu_FreeErrp(e);
    ilu_free(e);
  }
  return;
}

ilu_Error      *
ilu_ErrDup(ilu_Error * e)
{
  ilu_Error      *e2 = (ilu_Error *) ilu_malloc(sizeof(ilu_Error));
  if (e2 != NIL)
    *e2 = *e;
  return e2;

}

const char     *
ilu_ErrorFile(ilu_Error * e)
{
  _ilu_Assert(e != NIL, "ilu_ErrorFile(NIL)");
  return e->ilu_file;
}

int 
ilu_ErrorLine(ilu_Error * e)
{
  _ilu_Assert(e != NIL, "ilu_ErrorLine(NIL)");
  return e->ilu_line;
}

static ilu_RaiseDebugHook the_rdh = NULLFN;

void 
ilu_SetRaiseDebugHook(ilu_RaiseDebugHook rdh)
{
  the_rdh = rdh;
  return;
}

void
_ilu_NoteRaise(int et, const char *file,
	       int line)
{
  ilu_RaiseDebugHook rdh = the_rdh;
  if (rdh != NULLFN)
    (*rdh) (et, file, line);
  return;
}


ILU_DEF_ERR(success, "(the error code that signals success)") {}
ILU_DEF_ERR(unknown, "(a standard CORBA system exception)") {}
ILU_DEF_ERR(bad_param, "(a standard CORBA system exception)") {}
ILU_DEF_ERR(no_memory, "(a standard CORBA system exception)") {}
ILU_DEF_ERR(imp_limit, "(a standard CORBA system exception)") {}
ILU_DEF_ERR(comm_failure, "(a standard CORBA system exception)") {}
ILU_DEF_ERR(inv_objref, "(a standard CORBA system exception)") {}
ILU_DEF_ERR(no_permission, "(a standard CORBA system exception)") {}
ILU_DEF_ERR(internal, "(a standard CORBA system exception)") {}
ILU_DEF_ERR(marshal, "(a standard CORBA system exception)") {}
ILU_DEF_ERR(initialize, "(a standard CORBA system exception)") {}
ILU_DEF_ERR(no_implement, "(a standard CORBA system exception)") {}
ILU_DEF_ERR(bad_typecode, "(a standard CORBA system exception)") {}
ILU_DEF_ERR(bad_operation, "(a standard CORBA system exception)") {}
ILU_DEF_ERR(no_resources, "(a standard CORBA system exception)") {}
ILU_DEF_ERR(no_response, "(a standard CORBA system exception)") {}
ILU_DEF_ERR(persist_store, "(a standard CORBA system exception)") {}
ILU_DEF_ERR(bad_inv_order, "(a standard CORBA system exception)") {}
ILU_DEF_ERR(transient, "(a standard CORBA system exception)") {}
ILU_DEF_ERR(free_mem, "(a standard CORBA system exception)") {}
ILU_DEF_ERR(inv_ident, "(a standard CORBA system exception)") {}
ILU_DEF_ERR(inv_flag, "(a standard CORBA system exception)") {}
ILU_DEF_ERR(intf_repos, "(a standard CORBA system exception)") {}
ILU_DEF_ERR(bad_context, "(a standard CORBA system exception)") {}
ILU_DEF_ERR(obj_adapter, "(a standard CORBA system exception)") {}
ILU_DEF_ERR(data_conversion, "(a standard CORBA system exception)") {}
ILU_DEF_ERR(bad_locks, "caller violated locking precondition") {}
ILU_DEF_ERR(broken_locks, "locking meta-object broken") {}
ILU_DEF_ERR(interrupted, "caller asked for RPC interruption") {}
ILU_DEF_ERR(gss_security, "a standard GSS security error") {}


ILU_DEF_ERR(KernelBroken, "Internal bug in kernel") {
  /* nothing to free */
}

ILU_DEF_ERR(BadDataStructure, "Pointer to invalid data structure encountered") {
  /* assume that all strings are constants */
}

ILU_DEF_ERR(MaxCountExceeded, "An attempt was made to fill an array or structure beyond its fixed limit") {
  /* assume that all strings are constants */
}

ILU_DEF_ERR(ProtocolAlreadyRegistered, "Specified protocol was already in registry of protocols") {
  /* assume that all strings are constants */
}

ILU_DEF_ERR(TransportAlreadyRegistered, "Specified transport was already in registry of transports") {
  /* assume that all strings are constants */
}

ILU_DEF_ERR(WrongLocks, "Locking precondition not met") {
  /* Nothing to free */
}

ILU_DEF_ERR(BadProtocolInfo, "Bogus protocol info") {
  ilu_free(e->x);
}

ILU_DEF_ERR(MalformedKernelObject, "Kernel ilu_Object that has bad fields") {
  ilu_free(e->reason);
}

ILU_DEF_ERR(NoConnection, "Attempt to marshal or unmarshal to/from a non-existent connection") {
}

ILU_DEF_ERR(ClosedServer, "Received discriminant for closed server") {
  ilu_free(e->oid);
}

ILU_DEF_ERR(ObjectNotFoundInServer, "Object can't be located on specified server") {
  ilu_free(e->oid);
}

ILU_DEF_ERR(NoObjectForSBH, "Can't determine object") {
  ilu_free(e->sbh);
}

ILU_DEF_ERR(BadArguments, "Invalid arguments for function") {
  ilu_free(e->reason);
}

ILU_DEF_ERR(GcRegFailed, "[Un]RegisterGCInterest failed") {
}

ILU_DEF_ERR(MallocFailure, "Couldn't allocated dynamic memory") {
  /* nothing to free */
}

#define ILU_ERRLISTELT(id) ILU_QUADEF(id);
#if defined(WIN16)

    ILU_QUADEF(success);
    ILU_QUADEF(unknown);
    ILU_QUADEF(bad_param);
    ILU_QUADEF(no_memory);
    ILU_QUADEF(imp_limit);
    ILU_QUADEF(comm_failure);
    ILU_QUADEF(inv_objref);
    ILU_QUADEF(no_permission);
    ILU_QUADEF(internal);
    ILU_QUADEF(marshal);
    ILU_QUADEF(initialize);
    ILU_QUADEF(no_implement);
    ILU_QUADEF(bad_typecode);
    ILU_QUADEF(bad_operation);
    ILU_QUADEF(no_resources);
    ILU_QUADEF(no_response);
    ILU_QUADEF(persist_store);
    ILU_QUADEF(bad_inv_order);
    ILU_QUADEF(transient);
    ILU_QUADEF(free_mem);
    ILU_QUADEF(inv_ident);
    ILU_QUADEF(inv_flag);
    ILU_QUADEF(intf_repos);
    ILU_QUADEF(bad_context);
    ILU_QUADEF(obj_adapter);
    ILU_QUADEF(data_conversion);
    ILU_QUADEF(bad_locks);
    ILU_QUADEF(broken_locks);
    ILU_QUADEF(interrupted);
    ILU_QUADEF(gss_security);
    ILU_QUADEF(KernelBroken);
    ILU_QUADEF(BadDataStructure);
    ILU_QUADEF(MallocFailure);
    ILU_QUADEF(MalformedKernelObject);
    ILU_QUADEF(MaxCountExceeded);
    ILU_QUADEF(ProtocolAlreadyRegistered);
    ILU_QUADEF(TransportAlreadyRegistered);
    ILU_QUADEF(WrongLocks);
    ILU_QUADEF(BadProtocolInfo);
    ILU_QUADEF(BadArguments);
    ILU_QUADEF(GcRegFailed);
    ILU_QUADEF(NoConnection);
    ILU_QUADEF(ClosedServer);
    ILU_QUADEF(ObjectNotFoundInServer);
    ILU_QUADEF(NoObjectForSBH);
    ILU_QUADEF(CantCondition);
#else

ILU_ERRLIST

#endif
#undef ILU_ERRLISTELT
