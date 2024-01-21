#ifndef __DDEML_H
#define __DDEML_H

/* These names exported in user32.def, but the only app that I have
 that uses them is wxwindows, wich looks in ddeml.h for them.
*/


#ifdef UNICODE
#define DdeQueryString DdeQueryStringW
#define DdeInitialize DdeInitializeW
#define DdeCreateStringHandle DdeCreateStringHandleW
#else
#define DdeQueryString DdeQueryStringA
#define DdeInitialize DdeInitializeA
#define DdeCreateStringHandle DdeCreateStringHandleA
#endif


typedef BOOL SECURITY_CONTEXT_TRACKING_MODE;

typedef enum
  {
    SecurityAnonymous,
    SecurityIdentification,
    SecurityImpersonation,
    SecurityDelegation
  }
SECURITY_IMPERSONATION_LEVEL;

typedef struct
  {
    DWORD Length;
    SECURITY_IMPERSONATION_LEVEL ImpersonationLevel;
    SECURITY_CONTEXT_TRACKING_MODE ContextTrackingMode;
    BOOL EffectiveOnly;
  }
SECURITY_QUALITY_OF_SERVICE;


typedef struct 
  {
    UINT cb;
    UINT wFlags;
    UINT wCountryID;
    int iCodePage;
    DWORD dwLangID;
    DWORD dwSecurity;
    SECURITY_QUALITY_OF_SERVICE qos;
  }
CONVCONTEXT;

DECLARE_HANDLE (HCONV);

#define XCLASS_BOOL 		0x1000
#define XCLASS_DATA 		0x2000
#define XCLASS_FLAGS		0x4000
#define XCLASS_MASK 		0xfc00
#define XCLASS_NOTIFICATION 	0x8000
#define XTYPF_NOBLOCK 		0x2
#define XTYP_ADVDATA   		0x4010
#define XTYP_ADVREQ  		0x2022
#define XTYP_ADVSTART  		0x1030
#define XTYP_ADVSTOP  		0x8040
#define XTYP_CONNECT 		0x1062
#define XTYP_CONNECT_CONFIRM 	0x8072
#define XTYP_DISCONNECT  	0x80c2
#define XTYP_EXECUTE  		0x4050
#define XTYP_POKE   		0x4090
#define XTYP_REQUEST 		0x20b0


#define DMLERR_DLL_USAGE   	0x4004
#define DMLERR_INVALIDPARAMETER 0x4006
#define DMLERR_NOTPROCESSED  	0x4009
#define DMLERR_POSTMSG_FAILED  	0x400c
#define DMLERR_SERVER_DIED  	0x400e
#define DMLERR_SYS_ERROR   	0x400f
#define DMLERR_BUSY   		0x4001
#define DMLERR_DATAACKTIMEOUT  	0x4002
#define DMLERR_ADVACKTIMEOUT  	0x4000
#define DMLERR_DLL_NOT_INITIALIZED  0x4003
#define DMLERR_LOW_MEMORY   	0x4007
#define DMLERR_MEMORY_ERROR  	0x4008
#define DMLERR_POKEACKTIMEOUT  	0x400b
#define DMLERR_NO_CONV_ESTABLISHED  0x400a
#define DMLERR_REENTRANCY   	0x400d
#define DMLERR_UNFOUND_QUEUE_ID 0x4011
#define DMLERR_UNADVACKTIMEOUT  0x4010
#define DMLERR_EXECACKTIMEOUT   0x4005
#define DDE_FACK 		0x8000
#define DDE_FNOTPROCESSED	0x0000

#define DNS_REGISTER 0x0001
#define DNS_UNREGISTER 0x0002
#define CP_WINANSI 1004
#define CP_WINUNICODE 1200
#define EXPENTRY CALLBACK
#define APPCLASS_STANDARD  0x00000000

DECLARE_HANDLE (HDDEDATA)
DECLARE_HANDLE (HSZ)
DECLARE_HANDLE (HCONVLIST)


typedef CALLB PFNCALLBACK;

typedef CONVCONTEXT *PCONVCONTEXT;
typedef void (*CALLB) ();

HCONV WINAPI 	DdeConnect (DWORD, HSZ, HSZ, CONVCONTEXT *);
HSZ WINAPI 	DdeCreateStringHandleA (DWORD, char *, int);
HSZ WINAPI 	DdeCreateStringHandleW (DWORD, wchar_t *, int);
BOOL WINAPI 	DdeDisconnect (HCONV);
BOOL WINAPI 	DdeFreeDataHandle (HDDEDATA);
DWORD WINAPI 	DdeGetData (HDDEDATA, BYTE *, DWORD, DWORD);
UINT WINAPI 	DdeGetLastError (DWORD);
UINT WINAPI 	DdeInitializeA (DWORD *, CALLB, DWORD, DWORD);
UINT WINAPI 	DdeInitializeW (DWORD *, CALLB, DWORD, DWORD);
HDDEDATA WINAPI DdeNameService (DWORD, HSZ, HSZ, UINT);
BOOL WINAPI 	DdePostAdvise (DWORD, HSZ, HSZ);
DWORD WINAPI 	DdeQueryStringA (DWORD, HSZ, char *, DWORD, int);
DWORD WINAPI 	DdeQueryStringW (DWORD, HSZ, wchar_t *, DWORD, int);
HCONV WINAPI 	DdeReconnect (HCONV);
BOOL WINAPI 	DdeUninitialize (DWORD);

#endif


