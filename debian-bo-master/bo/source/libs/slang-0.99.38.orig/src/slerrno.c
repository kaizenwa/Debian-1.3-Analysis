/* The point of this file is to handle errno values in a system independent
 * way so that they may be used in slang scripts.
 */
#include <stdio.h>
#include <errno.h>
#include "slang.h"
#include "_slang.h"

int _SLerrno_Return_Status;

typedef struct
{
   char *msg;
   int sys_errno;
}
Errno_Map_Type;

static Errno_Map_Type Errno_Map [] = 
{
#ifndef EPERM
# define EPERM	-1
#endif
   {"Not owner",			EPERM},
#ifndef ENOENT
# define ENOENT	-1
#endif
   {"No such file or directory",	ENOENT},
#ifndef ESRCH
# define ESRCH	-1
#endif
   {"No such process",			ESRCH},
#ifndef ENXIO
# define ENXIO	-1
#endif
   {"No such device or address",	ENXIO},
#ifndef ENOEXEC
# define ENOEXEC	-1
#endif
   {"Exec format error",		ENOEXEC},
#ifndef EBADF
# define EBADF	-1
#endif
   {"Bad file number",			EBADF},
#ifndef ECHILD
# define ECHILD	-1
#endif
   {"No children",			ECHILD},
#ifndef ENOMEM
# define ENOMEM	-1
#endif
   {"Not enough core",			ENOMEM},
#ifndef EACCES
# define EACCES	-1
#endif
   {"Permission denied",		EACCES},
#ifndef EFAULT
# define EFAULT	-1
#endif
   {"Bad address",			EFAULT},
#ifndef ENOTBLK
# define ENOTBLK	-1
#endif
   {"Block device required",		ENOTBLK},
#ifndef EBUSY
# define EBUSY	-1
#endif
   {"Mount device busy",		EBUSY},
#ifndef EEXIST
# define EEXIST	-1
#endif
   {"File exists",			EEXIST},
#ifndef EXDEV
# define EXDEV	-1
#endif
   {"Cross-device link",		EXDEV},
#ifndef ENODEV
# define ENODEV	-1
#endif
   {"No such device",			ENODEV},
#ifndef ENOTDIR
# define ENOTDIR	-1
#endif
   {"Not a directory",			ENOTDIR},
#ifndef EISDIR
# define EISDIR	-1
#endif
   {"Is a directory",			EISDIR},
#ifndef EINVAL
# define EINVAL	-1
#endif
   {"Invalid argument",			EINVAL},
#ifndef ENFILE
# define ENFILE	-1
#endif
   {"File table overflow",		ENFILE},
#ifndef EMFILE
# define EMFILE	-1
#endif
   {"Too many open files",		EMFILE},
#ifndef ENOTTY
# define ENOTTY	-1
#endif
   {"Not a typewriter",			ENOTTY},
#ifndef ETXTBSY
# define ETXTBSY	-1
#endif
   {"Text file busy",			ETXTBSY},
#ifndef EFBIG
# define EFBIG	-1
#endif
   {"File too large",			EFBIG},
#ifndef ENOSPC
# define ENOSPC	-1
#endif
   {"No space left on device",		ENOSPC},
#ifndef ESPIPE
# define ESPIPE	-1
#endif
   {"Illegal seek",			ESPIPE},
#ifndef EROFS
# define EROFS	-1
#endif
   {"Read-only file system",		EROFS},
#ifndef EMLINK
# define EMLINK	-1
#endif
   {"Too many links",			EMLINK},
#ifndef EPIPE
# define EPIPE	-1
#endif
   {"Broken pipe",			EPIPE},
#ifndef ELOOP
# define ELOOP	-1
#endif
   {"Too many levels of symbolic links",ELOOP},
#ifndef ENAMETOOLONG
# define ENAMETOOLONG	-1
#endif
   {"File name too long",		ENAMETOOLONG},
   {NULL, 0}
};


void _SLerrno_set_return_status (void)
{
   int i;
   Errno_Map_Type *e;
   
   e = Errno_Map;
   
   i = 2;
   while (e->msg != NULL)
     {
	if (e->sys_errno == errno)
	  {
	     _SLerrno_Return_Status = i;
	     return;
	  }
	
	i++;
	e++;
     }
   
   _SLerrno_Return_Status = 1;
}

char *_SLerrno_strerror (void)
{
   int i = _SLerrno_Return_Status - 2;
   
   if (i < 0)
     return "Unknown Error";
   
   return (Errno_Map[i].msg);
}
