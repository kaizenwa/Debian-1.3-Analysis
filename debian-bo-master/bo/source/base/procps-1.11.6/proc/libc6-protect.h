#ifndef _LIBC6_PROTECT_H  /* we have to protect the libc6 headers from the */
#define _LIBC6_PROTECT_H  /* Linux headers which harm them */

#if defined (__GLIBC__) && __GLIBC__ >= 2

#define _LINUX_TIME_H
#define _LINUX_IPC_H
#define _LINUX_RESOURCE_H
#define _LINUX_SIGNAL_H
#define _LINUX_DIRENT_H

#endif

#endif
