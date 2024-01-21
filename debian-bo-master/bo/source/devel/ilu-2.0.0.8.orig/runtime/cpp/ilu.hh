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
/* $Id: ilu.hh,v 1.73 1996/07/01 20:29:49 larner Exp $ */
/* Last edited by Mike Spreitzer June 20, 1996 11:06 am PDT */

#ifndef __ilu_H_
#define __ilu_H_ 1

#include <stdlib.h>
#include <string.h>

extern "C" {
#include <iluxport.h>
}

/* define dllexport to support building DLLs on Win32 */
#if defined(WIN32)
#if defined(ILU_BUILDING_RUNTIME)
#define ILU_RUNTIME_PUBLIC __declspec(dllexport) extern
#define ILU_RUNTIME_PUBLIC_CLASS  class __declspec(dllexport)
#else
#define ILU_RUNTIME_PUBLIC __declspec(dllimport) extern
#define ILU_RUNTIME_PUBLIC_CLASS class __declspec(dllimport)
#endif /* defined(ILU_BUILDING_RUNTIME) */
#else
#define ILU_RUNTIME_PUBLIC extern
#define ILU_RUNTIME_PUBLIC_CLASS class
#endif /* defined(WIN32) */


  typedef unsigned char ilu_Boolean;

  typedef ilu_boolean ilu_kernelBoolean;
#define ilu_kernelTRUE ilu_TRUE
#define ilu_kernelFALSE ilu_FALSE

  typedef long int ilu_Integer;
  typedef short int ilu_ShortInteger;
  typedef ilu_longinteger ilu_LongInteger;

  typedef unsigned long int ilu_Cardinal;
  typedef unsigned short int ilu_ShortCardinal;
  typedef ilu_longcardinal ilu_LongCardinal;

  typedef double ilu_Real;
  typedef float ilu_ShortReal;
  typedef ilu_longreal ilu_LongReal;

  typedef unsigned short ilu_Character;
  typedef char ilu_ShortCharacter;

  typedef ilu_ShortCharacter * ilu_CString;
  typedef ilu_ShortCharacter * ilu_T_CString;
  typedef ilu_Character *ilu_WString;

  typedef unsigned char ilu_Byte;

  typedef char *ilu_Exception;		/* address of exception description */
  typedef ilu_Object ilu_KernelObject;

struct _ilu_LongInteger_s {
  ilu_Integer high;
  unsigned long int low;
};

struct _ilu_LongCardinal_s {
  ilu_Cardinal high;
  ilu_Cardinal low;
};

struct iluCall_s {
 public:
  ilu_Call_s call;
  ilu_Error  err;
};

typedef struct iluCall_s *iluCall;

ILU_RUNTIME_PUBLIC_CLASS ilu {
public:
  static ilu_Exception	Success;
  static ilu_Exception	ProtocolError;

  static          ilu_Boolean
                  ParseSBH(ilu_CString sbh, ilu_CString * plainInstH,
			   ilu_CString * plainServerID,
			   ilu_CString * plainMstid,
			   ilu_CString * encodedContactInfo,
			   ilu_Cardinal * cinfolen);
  /*
   * Caller owns sbh, *plainInstH, *plainServerID, and *plainMstid;
   * *encodedContactInfo points at substring, length *cinfolen, of
   * sbh.  See iluxport.h for more details.
   */

  static ilu_Class	FindClassFromTypeName	(char *name);
  static ilu_Class	FindClassFromID		(char *id);



//private:  (the rest is private to ILU)

  static ilu_Boolean    StartCall(iluCall call, ilu_Server s, ilu_Class intro_type, ilu_Method method);
  static ilu_Cardinal   BeginSizingException(iluCall call, ilu_Cardinal eIndex);
  static ilu_Boolean    BeginException(iluCall call, ilu_Cardinal eCode, ilu_Cardinal argSize);
  static ilu_Boolean	StartRequest		(iluCall call, ilu_Cardinal argSize);
  static ilu_Cardinal	BeginSizingReply		(iluCall call, ilu_Boolean exns_possible);
  static ilu_Boolean	BeginReply		(iluCall call, ilu_Boolean exns_possible, ilu_Cardinal argSize);
  static ilu_Boolean	ReplyRead		(iluCall call);
  static ilu_Boolean	FinishCall		(iluCall call);
  static ilu_Boolean	FinishException 	(iluCall call);
  static ilu_Boolean	FinishRequest		(iluCall call);
  static ilu_Boolean	FinishReply		(iluCall call);
  static ilu_Boolean	FinishParameters	(iluCall call, void *obj);
  static void		NoReply			(iluCall call);
  
  static ilu_Exception	ExceptionOfMethod	(ilu_Method method, ilu_Cardinal index);
  static void		EnterServer		(ilu_Server ks, ilu_Class c);
  static void		ExitServer		(ilu_Server ks, ilu_Class c);
  static void *		SBHToObject		(ilu_CString sbh, ilu_Class putative_class);
  static ilu_CString	SBHOfObject		(ilu_KernelObject obj);
  static ilu_Class	GetObjectClass		(ilu_KernelObject obj);
  static ilu_Server	GetObjectServer		(ilu_KernelObject obj);
  static ilu_KernelObject	CreateTrueKernelObject	(char *ih, ilu_Server server, ilu_Class c, void * lspo);
  static class iluObject *	GetLanguageSpecificObject	(ilu_KernelObject obj);
  static void		SetLanguageSpecificObject	(ilu_KernelObject obj, class iluObject * lspo);

  static void	EnterOTMu();
  static void	ExitOTMu();
  
  static ilu_Class	DefineObjectType(ilu_CString cl_name,
		     ilu_CString cl_brand,
		     ilu_CString cl_unique_id,
		     ilu_CString cl_singleton,
		     ilu_Boolean cl_optional,
		     ilu_Boolean cl_collectible,
		     ilu_CString cl_authentication,
		     ilu_Cardinal cl_method_count,
		     ilu_Cardinal cl_scls_count,
		     ilu_CString *cl_scls_ids);
  
  static ilu_Exception DefineException(ilu_CString i, ilu_CString e);
  
  static void	DefineMethod(ilu_Class c,
		 ilu_Cardinal i,
		 ilu_CString me_name,
		 ilu_Cardinal me_id,
		 ilu_Boolean me_cacheable,
		 ilu_Boolean me_asynchronous,
		 ilu_Cardinal me_exceptionCount,
		 ilu_Exception *me_exceptionVector);
  
  static void	ObjectTypeDefined(ilu_Class t);
  
  static void		EndSequence		(iluCall call);
  static void		EndUnion		(iluCall call);
  static void		EndArray		(iluCall call);
  static void		EndRecord		(iluCall call);
  
  static unsigned char	InputByte		(iluCall call, ilu_Byte *b);
  static ilu_Boolean	InputBoolean		(iluCall call, ilu_Boolean *b);
  static ilu_Boolean	InputOptional		(iluCall call, ilu_Boolean *b);
  static ilu_Cardinal	InputCardinal		(iluCall call, ilu_Cardinal *b);
  static ilu_Character	InputCharacter		(iluCall call, ilu_Character *c);
  static ilu_ShortCharacter	InputShortCharacter	(iluCall call, ilu_ShortCharacter *c);
  static ilu_ShortCardinal	InputEnum	(iluCall call, ilu_ShortCardinal *c);
  static ilu_Integer	InputInteger		(iluCall call, ilu_Integer *c);
  static ilu_Real		InputReal	(iluCall call, ilu_Real *);
  static ilu_ShortCardinal	InputShortCardinal	(iluCall call, ilu_ShortCardinal *);
  static ilu_ShortInteger	InputShortInteger	(iluCall call, ilu_ShortInteger *);
  static ilu_ShortReal	InputShortReal	(iluCall call, ilu_ShortReal *);
  static ilu_LongCardinal	InputLongCardinal	(iluCall call, ilu_LongCardinal *);
  static ilu_LongInteger	InputLongInteger	(iluCall call, ilu_LongInteger *);
  static ilu_LongReal	InputLongReal	(iluCall call, ilu_LongReal *);
  static ilu_CString	InputString	(iluCall call, ilu_CString buf, ilu_Cardinal *len, ilu_Cardinal limit);
  static ilu_CString	InputStringVec	(iluCall call, ilu_CString buf, ilu_Cardinal len);
  static ilu_WString	InputWString	(iluCall call, ilu_WString buf, ilu_Cardinal *len, ilu_Cardinal limit);
  static ilu_WString	InputWStringVec	(iluCall call, ilu_WString buf, ilu_Cardinal len);
  static unsigned char*	InputBytes	(iluCall call, unsigned char *buf, ilu_Cardinal *len, ilu_Cardinal limit);
  static unsigned char*	InputOpaque	(iluCall call, unsigned char *buf, ilu_Cardinal len);
  
  static ilu_KernelObject	InputObjectID	(iluCall call, ilu_Boolean discriminator_p, ilu_Class putative_class);
  
  static ilu_Cardinal	InputSequence	(iluCall call, ilu_Cardinal *count, ilu_Cardinal limit);
  static ilu_Cardinal	InputUnion	(iluCall call, ilu_Cardinal *discriminator, ilu_Cardinal discriminator_size);
  static ilu_Boolean	InputArray	(iluCall call);
  static ilu_Boolean	InputRecord	(iluCall call);
  
  static ilu_Boolean	OutputByte	(iluCall call, unsigned char byte);
  static ilu_Boolean	OutputBoolean	(iluCall call, ilu_Boolean b);
  static ilu_Boolean	OutputOptional	(iluCall call, ilu_Boolean present);
  static ilu_Boolean	OutputCardinal	(iluCall call, ilu_Cardinal val);
  static ilu_Boolean	OutputCharacter	(iluCall call, ilu_Character val);
  static ilu_Boolean	OutputEnum	(iluCall call, ilu_ShortCardinal val);
  static ilu_Boolean	OutputInteger	(iluCall call, ilu_Integer val);
  static ilu_Boolean	OutputReal	(iluCall call, double val);
  static ilu_Boolean	OutputShortCardinal	(iluCall call, ilu_ShortCardinal val);
  static ilu_Boolean	OutputShortInteger	(iluCall call, ilu_ShortInteger val);
  static ilu_Boolean	OutputShortReal	(iluCall call, float val);
  static ilu_Boolean	OutputLongCardinal	(iluCall call, ilu_LongCardinal val);
  static ilu_Boolean	OutputLongInteger	(iluCall call, ilu_LongInteger val);
  static ilu_Boolean	OutputLongReal	(iluCall call, ilu_LongReal val);
  static ilu_Boolean	OutputString	(iluCall call, ilu_CString buf, ilu_Cardinal len, ilu_Cardinal limit);
  static ilu_Boolean	OutputStringVec	(iluCall call, ilu_CString buf, ilu_Cardinal len);
  static ilu_Boolean	OutputWString	(iluCall call, ilu_WString buf, ilu_Cardinal len, ilu_Cardinal limit);
  static ilu_Boolean	OutputWStringVec(iluCall call, ilu_WString buf, ilu_Cardinal len);
  static ilu_Boolean	OutputBytes	(iluCall call, unsigned char * bytes, ilu_Cardinal len, ilu_Cardinal limit);
  static ilu_Boolean	OutputOpaque	(iluCall call, unsigned char * buf, ilu_Cardinal len);
  
  static ilu_Boolean	OutputObjectID	(iluCall call, ilu_KernelObject obj, ilu_Boolean discriminator_p, ilu_Class c);
  
  static ilu_Boolean	OutputSequence	(iluCall call, ilu_Cardinal length, ilu_Cardinal limit);
  static ilu_Boolean	OutputUnion	(iluCall call, ilu_Cardinal discriminator, ilu_Cardinal discriminator_size);
  static ilu_Boolean	OutputArray	(iluCall call);
  static ilu_Boolean	OutputRecord	(iluCall call);
  
  static ilu_Cardinal	SizeOfByte	(iluCall call, unsigned char byte);
  static ilu_Cardinal	SizeOfBoolean	(iluCall call, ilu_Boolean b);
  static ilu_Cardinal	SizeOfOptional	(iluCall call, ilu_Boolean present);
  static ilu_Cardinal	SizeOfCardinal	(iluCall call, ilu_Cardinal val);
  static ilu_Cardinal	SizeOfCharacter	(iluCall call, ilu_Character val);
  static ilu_Cardinal	SizeOfEnum	(iluCall call, ilu_ShortCardinal val);
  static ilu_Cardinal	SizeOfInteger	(iluCall call, ilu_Integer val);
  static ilu_Cardinal	SizeOfReal	(iluCall call, double val);
  static ilu_Cardinal	SizeOfShortCardinal	(iluCall call, ilu_ShortCardinal val);
  static ilu_Cardinal	SizeOfShortInteger	(iluCall call, ilu_ShortInteger val);
  static ilu_Cardinal	SizeOfShortReal	(iluCall call, float val);
  static ilu_Cardinal	SizeOfLongCardinal	(iluCall call, ilu_LongCardinal val);
  static ilu_Cardinal	SizeOfLongInteger	(iluCall call, ilu_LongInteger val);
  static ilu_Cardinal	SizeOfLongReal	(iluCall call, ilu_LongReal val);
  static ilu_Cardinal	SizeOfString	(iluCall call, ilu_CString buf, ilu_Cardinal len, ilu_Cardinal limit);
  static ilu_Cardinal	SizeOfStringVec	(iluCall call, ilu_CString buf, ilu_Cardinal len);
  static ilu_Cardinal	SizeOfWString	(iluCall call, ilu_WString buf, ilu_Cardinal len, ilu_Cardinal limit);
  static ilu_Cardinal	SizeOfWStringVec(iluCall call, ilu_WString buf, ilu_Cardinal len);
  static ilu_Cardinal	SizeOfBytes	(iluCall call, unsigned char * bytes, ilu_Cardinal len, ilu_Cardinal limit);
  static ilu_Cardinal	SizeOfOpaque	(iluCall call, unsigned char * buf, ilu_Cardinal len);
  
  static ilu_Cardinal	SizeOfObjectID	(iluCall call, ilu_KernelObject obj, ilu_Boolean discriminator_p, ilu_Class c);
  
  static ilu_Cardinal	SizeOfSequence	(iluCall call, ilu_Cardinal length, ilu_Cardinal limit);
  static ilu_Cardinal	SizeOfUnion	(iluCall call, ilu_Cardinal discriminator, ilu_Cardinal discriminator_size);
  static ilu_Cardinal	SizeOfArray	(iluCall call);
  static ilu_Cardinal	SizeOfRecord	(iluCall call);
  
  static void		SetDefaultServer (class iluServer * server);
  static class iluServer * GetDefaultServer ();
  
  static ilu_Port	CreatePort	(ilu_Server server,
					 char *protocolType,
					 ilu_TransportInfo transportType);
  static void	SetServerDefaultPort	(ilu_Server s, ilu_Port p);

  static ilu_ProtocolException	WaitForReply	(iluCall call, ilu_Cardinal *successCode);
  static ilu_RcvReqStat ReceiveRequest(ilu_Connection conn,
				       iluCall pcall, ilu_Boolean *initted,
				       ilu_Class * pclass,
				       ilu_Method * method,
				       ilu_Cardinal * serial_number);
  
  static ilu_Connection	HandleNewConnection	(ilu_Port port);
  static ilu_Boolean	WaitForPortConnectionRequest	(ilu_Port port);
  static void		CloseConnection		(ilu_Connection conn);

  static void 		RunMainLoop		(int *stop);
  static void		ExitMainLoop		(int *stop);
  static ilu_Boolean 	RegisterInputHandler	(int fd, void (*handlerProc)(int fd, void *rock), void *rock);
  static ilu_Boolean	UnregisterInputHandler	(int fd);
  static ilu_Boolean 	RegisterOutputHandler	(int fd,void (*handlerProc)(int fd, void *rock),void *rock);
  static ilu_Boolean	UnregisterOutputHandler	(int fd);
  static void     FullPreferSuccess(ilu_Error *e, const char *atf, int atl);
  static ilu_cardinal CppLangIdx();
};

#define ilu_PreferSuccess(e) ilu::FullPreferSuccess(e,__FILE__,__LINE__)
/* Private to ILU */

class iluServer;

ILU_RUNTIME_PUBLIC_CLASS iluObject {
public:
			iluObject();
			iluObject(ilu_Server);
			iluObject(ilu_Server, char *instance_handle);
			iluObject(char *instance_handle);
  virtual 		~iluObject();

  virtual ilu_CString	ILUStringBindingHandle	();
  virtual ilu_Boolean	ILUPublish		();
  virtual ilu_Boolean	ILUWithdraw		();
  static  void *	Lookup			(char *sid, char *ih, ilu_Class pclass);
  char *		ILUClassName		(void);
  char *		ILUClassId		(void);

  // An implementer of true objects is responsible for either
  // overriding this method or calling SetDefaultServer.
  virtual iluServer *	ILUGetServer		(void);

  ilu_Class		ILUInstanceClassRecord;

  static  void		_ILU_RegisterAsGCCallback	(class iluObject *);  // ugly hack to get the RPCObject

//private:  (the rest is private to ILU)

  static  iluObject *	CreateFromRegistry	(ilu_Class c, ilu_KernelObject);
  static  void		RegisterSurrogateCreator(ilu_Class c,
						 class iluObject * (*proc)(ilu_KernelObject));
  static  void *	InputObject		(iluCall call, ilu_Boolean discriminator_p, ilu_Class putative_class);
  static  ilu_Boolean	OutputObject		(iluCall call, iluObject *obj, ilu_Class putative_class);
  static  ilu_Cardinal	SizeOfObject		(iluCall call, iluObject *obj, ilu_Class putative_class);

  virtual char *	ILUGetInstanceHandle	();
  virtual ilu_KernelObject ILUGetRPCObject	();
  virtual void		ILUSetRPCObject		(ilu_KernelObject obj);
  
  virtual ilu_Server	ILUGetKernelServer	(void);

  virtual void *	ILUCastDown		(ilu_Class cast_to);
  ilu_KernelObject	ILUEnsureKernelObject	();
  ilu_Server		ILUEnsureKernelServer	();
  void *		ILUMostSpecificObject	() { return (this->ILUMostSpecificObj); };
  void			ILUSetMostSpecificObject (void *o) { this->ILUMostSpecificObj = o; };

private:

  ilu_KernelObject	ILURPCObject;
  ilu_Server		ILURPCServer;
  char *		ILUPublishProof;
  char *		ILUInstanceHandle;
  void *		ILUMostSpecificObj;

};

typedef void * ilu_Alarm;
/* an alarm is an active object which can be set to invoke a procedure
   with an argument "rock" at a specified time, asynchronously. */

ILU_RUNTIME_PUBLIC_CLASS iluObjectTable {
 public:

  virtual ~iluObjectTable();

  virtual iluObject *	ObjectOfIH (ilu_CString ih) = 0;

};

ILU_RUNTIME_PUBLIC_CLASS iluMainLoop {
 public:

  virtual void		Run (int *stop) = 0;
  virtual void		Exit (int *stop) = 0;
  virtual ilu_Boolean	RegisterInputHandler (int fd,
					void (*handlerProc)(int,void *),
					void *handlerArg) = 0;
  virtual ilu_Boolean	UnregisterInputHandler (int fd) = 0;
  virtual ilu_Boolean	RegisterOutputHandler (int fd,
					void (*handlerProc)(int,void *),
					void *handlerArg) = 0;
  virtual ilu_Boolean	UnregisterOutputHandler (int fd) = 0;
  virtual ilu_Alarm	CreateAlarm() = 0;
  virtual void		SetAlarm(ilu_Alarm alarm, ilu_FineTime t, void (*proc)(void *rock), void *rock) = 0;
  virtual void		ClearAlarm (ilu_Alarm alarm) = 0;
};

#ifdef ILU_OS_THREADED

/*Main invariant holds*/
#define ILU_CPP_USE_OS_THREADS  \
  iluServer::StartThreading(ilu_InitializeOSThreading, \
			    ilu_OSForkNewThread)
/*
 * If you have configured ILU be able to use OS-supplied threads,
 * this will tell the C++ runtime to do so.  If
 * ILU_CPP_USE_OS_THREADS is used, you do not need to call
 * iluServer::SetFork(), described below, nor the ilu_SetWaitTech,
 * ilu_SetMainLoop, and ilu_SetLockTech procedures of ILU's runtime
 * kernel.  This routine should be called before any other ILU calls
 * are made, or any interface initialization calls are made.  It
 * returns FALSE (and prints out an error message) if anything goes
 * wrong in setting up the threads support.
 */

#endif	/* defined(ILU_OS_THREADED) */

extern          "C" {
  typedef         ilu_boolean
                  ilu_ThreadSetupProc(ILU_ERRS((bad_param, no_memory,
				                no_resources,
				                internal)) * err);
  /*
   * A procedure that calls the ilu_SetWaitTech, ilu_SetMainLoop,
   * and ilu_SetLockTech procedures of ILU's runtime kernel.
   */

  typedef         ilu_boolean
                  ilu_ForkProc(void (*proc) (void *arg),
			       void *arg,
		               ILU_ERRS((no_memory, no_resources,
				         internal)) * err);
  /* A procedure that forks a thread. */
}

ILU_RUNTIME_PUBLIC_CLASS iluServer {
 public:

			/* AC: changed to not have defaults, but CFront
			 * can't grok them.
			 */
			iluServer(char *serviceID /* = NULL */,
				  iluObjectTable *objtab /* = NULL */);
			  /* Initializes the server, with 0 ports.
			     You'll need to call AddPort to make the
			     server reachable.  If a NULL object table
			     is given, a new one with a default
			     implementation is used. Caller owns serviceID. */

  virtual ilu_Boolean	AddPort (char *protocolType,
				 ilu_TransportInfo transportType,
				 ilu_Boolean be_default);
			  /* Adds another port to an existing server.
			     If "be_default" or this is the first port
			     added to the server, the new port will become
			     the default port for this server. */

  virtual ilu_Server	KernelServer(void);

  static void Stoppable_Run(int *stop);
  			  /* This is the main, outer loop for a server
			     program.  It is what animates all the
			     true servers. It returns when *stop is non zero */

  static  void		Run(void);
			  /* This is the main, outer loop for a server
			     program.  It is what animates all the
			     true servers. It never returns. */

  static  ilu_Boolean	RegisterInputHandler (int fd,
					void (*handlerProc)(int,void *),
					void *handlerArg);
			  /* Call this to extend the dispatch in the
			     main loop implemented by Run. */

  static  ilu_Boolean	UnregisterInputHandler (int fd);
			  /* Call this to cancel an extension installed
			     by a call on RegisterInputHandler. */

  static  void		iluSetMainLoop(iluMainLoop *ml);
			  /* If your application needs to alter the main
			     loop used by ILU, call this.  Call it, if
			     at all, before using the previous three
			     members. */

  static  ilu_Boolean	SetFork(void (*fork)(void (*proc)(void *arg),
					     void *arg));

  static  ilu_Boolean	StartThreading(ilu_ThreadSetupProc *s,
                                       ilu_ForkProc *f);
			/* Calls /s/, and if successful, SetFork(f) */

  static  void          MonitorOutgoingConnection(void *rock);

  static  void          PassNewConnections(void *rock);

  static  void          MonitorConn(ilu_Connection conn);

//private:  (the rest is private to ILU)

  static ilu_Boolean EnableRequests(iluCall call, ilu_Connection conn);
  static ilu_Boolean DisableRequests(iluCall call, ilu_Connection conn);

private:

  ilu_Server ks;	  /* The kernel's rep. of this server. */

};

#endif				/* ifndef __ilu_H_ */
