/* oops, this file doesn't exist in a real win32 world.. 
   some of it should go into windef.h */

#ifndef _WINTYPES_H
#define _WINTYPES_H

#ifdef i386
#define WINAPI __attribute__ ((stdcall))
#else
#define WINAPI
#endif

#define PACKED __attribute__((packed))
#define WINAPIINSIDE WINAPI
#define PASCAL WINAPI
#define FAR
#define far

#define CALLBACK WINAPIINSIDE

typedef WINAPIINSIDE int ( *FARPROC)();


#define INFINITE            0xFFFFFFFF  
#define LOWORD(l)           ((l) & 0xffff)
#define HIWORD(l)           (((l) >> 16) & 0xffff)
#define SEXT_HIWORD(l)     ((((int)l) >> 16))
#define ZEXT_HIWORD(l)     ((((unsigned int)l) >> 16))
#define SEXT_LOWORD(l)     ((int)(short)l)

#define FALSE 0
#define TRUE 1


#define GENERIC_READ                     0x80000000
#define GENERIC_WRITE                    0x40000000
#define GENERIC_EXECUTE                  0x20000000
#define GENERIC_ALL                      0x10000000


#define NORMAL_PRIORITY_CLASS       0x00000020
#define IDLE_PRIORITY_CLASS         0x00000040
#define HIGH_PRIORITY_CLASS         0x00000080
#define REALTIME_PRIORITY_CLASS     0x00000100

#define OF_READ             0x00000000
#define OF_WRITE            0x00000001
#define OF_READWRITE        0x00000002
#define OF_SHARE_COMPAT     0x00000000
#define OF_SHARE_EXCLUSIVE  0x00000010
#define OF_SHARE_DENY_WRITE 0x00000020
#define OF_SHARE_DENY_READ  0x00000030
#define OF_SHARE_DENY_NONE  0x00000040
#define OF_PARSE            0x00000100
#define OF_DELETE           0x00000200
#define OF_VERIFY           0x00000400
#define OF_CANCEL           0x00000800
#define OF_CREATE           0x00001000
#define OF_PROMPT           0x00002000
#define OF_EXIST            0x00004000
#define OF_REOPEN           0x00008000
#if 0
typedef struct { int foo;} HANDLE; /* So we don't mix up fds with handles*/
#define BAD_HANDLE_P(x) (x.foo == -1)
#define OK_HANDLE_P(x) (x.foo !=  -1)
#define SET_BAD_HANDLE(x) (x.foo =  -1)
#define process_handle_to_int(x) (x.foo)
#define SET_HANDLE(x,y) (x.foo = y)


#define BAD_HANDLE_P(x) (x == 0xdead)
#define OK_HANDLE_P(x) (x !=  0xdead)
#define SET_BAD_HANDLE(x) (x = 0xdead)
#define process_handle_to_int(x) (x+0x100)
#define SET_HANDLE(x,y) (x = y-0x100)
#endif

#define STARTF_USESHOWWINDOW    0x00000001
#define STARTF_USESIZE          0x00000002
#define STARTF_USEPOSITION      0x00000004
#define STARTF_USECOUNTCHARS    0x00000008
#define STARTF_USEFILLATTRIBUTE 0x00000010
#define STARTF_RUNFULLSCREEN    0x00000020  
#define STARTF_FORCEONFEEDBACK  0x00000040
#define STARTF_FORCEOFFFEEDBACK 0x00000080
#define STARTF_USESTDHANDLES    0x00000100
#define STARTF_USEHOTKEY        0x00000200


typedef unsigned int WPARAM;
typedef unsigned long LPARAM;
typedef unsigned long LRESULT;
typedef unsigned int DWORD;
typedef long LONG;
typedef int *LPINT;
typedef char *LPSTR;
typedef char *LPBYTE;
typedef unsigned short WORD;
typedef short SHORT;
typedef int BOOL;
typedef short WCHAR;
typedef unsigned int UINT;
#define CONST const
typedef char CHAR;
#define VOID void
typedef void *LPVOID ;
typedef DWORD *LPDWORD;
#define LMEM_FIXED          0x0000
#define LMEM_MOVEABLE       0x0002
#define LMEM_NOCOMPACT      0x0010
#define LMEM_NODISCARD      0x0020
#define LMEM_ZEROINIT       0x0040
#define GMEM_FIXED          0x0000

#define STD_INPUT_HANDLE    (-10)
#define STD_OUTPUT_HANDLE   (-11)
#define STD_ERROR_HANDLE    (-12)

#ifdef UNICODE
typedef WCHAR TCHAR;
#else
typedef char TCHAR;
#endif

DECLARE_HANDLE(HWND);
DECLARE_HANDLE(HDBC);
DECLARE_HANDLE(HSTMT);
DECLARE_HANDLE(HINSTANCE);
typedef struct
{
  SHORT X;
  SHORT Y;
} COORD;

typedef struct 
{
  SHORT Left;
  SHORT Top;
  SHORT Right;
  SHORT Bottom;
} SMALL_RECT;

typedef struct
{
  DWORD Internal;
  DWORD InternalHigh;
  DWORD Offset;
  DWORD OffsetHigh;
  HANDLE hEvent;
} OVERLAPPED;

typedef struct 
{
  int dwLowDateTime;
  int dwHighDateTime;
} FILETIME;


typedef struct 
{
  int dwFileAttributes;
  FILETIME ftCreationTime;
  FILETIME ftLastAccessTime;
  FILETIME ftLastWriteTime;
  int dwVolumeSerialNumber;
  int nFileSizeHigh;
  int nFileSizeLow;
  int nNumberOfLinks;
  int nFileIndexHigh;
  int nFileIndexLow;
} BY_HANDLE_FILE_INFORMATION ;

typedef unsigned char BYTE;
#ifndef NULL
#define NULL 0
#endif

typedef struct  
    {
    long x;
    long y;
    }   POINT;

typedef POINT *LPPOINT;
typedef LRESULT CALLBACK (* WNDPROC)(HWND, UINT, WPARAM, LPARAM);

DECLARE_HANDLE(HICON);
DECLARE_HANDLE(HCURSOR);
DECLARE_HANDLE(HBRUSH);

typedef short  RETCODE;
DECLARE_HANDLE(HENV);

#define PASCAL WINAPI
#define _export

typedef int INT;

typedef INT *PINT;
typedef BYTE *PBYTE;

#ifdef UNICODE
typedef wchar_t *LPTSTR;
typedef const wchar_t *LPCTSTR;
#else
typedef char *LPTSTR;
#endif

typedef const char *LPCSTR;
DECLARE_HANDLE(HBITMAP);

#define HFILE INT


typedef struct
{
  LONG    left;
  LONG    top;
  LONG    right;
  LONG    bottom;
} RECT;

typedef RECT *LPRECT;

typedef struct
{
  LONG        cx;
  LONG        cy;
} SIZE;


#endif /* _WINTYPES_H */
