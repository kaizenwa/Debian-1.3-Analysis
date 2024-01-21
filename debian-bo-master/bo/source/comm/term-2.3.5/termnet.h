/* Replace normal networking commands */

#ifdef __cplusplus
extern "C" {
#endif
#ifndef TERMWRAP
#ifdef BUILD_LIBC
#ifdef SHAREDIR
/* These are used for including termnet.c functions in libt.so.4 */
/* I don't use __* directly, because __ has special meaning to some compilers... */
#define term_accept     accept
#define x__accept       __accept
#define term_bind       bind
#define x__bind         __bind
#define term_chroot     chroot
#define x__chroot       __chroot
#define term_close      close
#define x__close        __close
#define term_connect    connect
#define x__connect      __connect
#define term_dup        dup
#define x__dup          __dup
#define term_dup2       dup2
#define x__dup2         __dup2
#define term_fcntl      fcntl
#define x__fcntl        __fcntl
#define term_fork       fork
#define x__fork         __fork
#define term_gethostbyname    gethostbyname
#define x__gethostbyname      __gethostbyname
#define term_gethostbyaddr    gethostbyaddr
#define x__gethostbyaddr      __gethostbyaddr
#define term_gethostname      gethostname
#define x__gethostname        __gethostname
#define term_getpeername      getpeername
#define x__getpeername        __getpeername
#define term_getsockname      getsockname
#define x__getsockname        __getsockname
#define term_listen   listen
#define x__listen     __listen
#define term_perror   _IO_perror
#define __perror      ___IO_perror
#define x__perror     ___IO_perror
#define term_rcmd     rcmd
#define x__rcmd       __rcmd
#define term_recv     recv
#define x__recv       __recv
#define term_recvfrom recvfrom
#define x__recvfrom   __recvfrom
#define term_send     send
#define x__send       __send
#define term_sendto   sendto
#define x__sendto     __sendto
#define term_shutdown shutdown
#define x__shutdown   __shutdown
#define term_socket   socket
#define x__socket     __socket
#define term_strerror strerror
#define x__strerror   __strerror
#define term_vfork    vfork
#define x__vfork      __vfork
#else /* Not SHAREDIR */
/* These are to rename the original libc.so.4 functions when including them in */
/* libt.so.4 */
#define accept          __accept
#define bind            __bind
#define chroot(x)       __chroot(x)
#define connect         __connect
#define close(x)        __close(x)
#define dup(x)          __dup(x)
#define dup2(x,y)       __dup2(x,y)
#ifndef linux
#define fcntl           __fcntl
#endif
#define fork()          __fork()
#define gethostbyname   __gethostbyname
#define gethostbyaddr   __gethostbyaddr
#define getpeername     __getpeername
#define getsockname     __getsockname
#define gethostname(x,y) __gethostname(x,y)
#define listen          __listen
#define perror  	___IO_perror
#define _IO_perror  	___IO_perror
#define rcmd            __rcmd
#define recv            __recv
#define recvfrom        __recvfrom
#define send            __send
#define sendto          __sendto
#define shutdown        __shutdown
#define socket          __socket
#define strerror	__strerror
#define vfork()         __vfork()
#endif
#else /* Not BUILD_LIBC */
#ifdef BUILD_LIBTERMNET
/* This is for compiling libtermnet.so.2 */
#define x__accept          accept
#define x__bind            bind
#define x__chroot          chroot
#define x__close           close
#define x__connect         connect
#define x__dup             dup
#define x__dup2            dup2
#define x__fcntl           fcntl
#define x__fork            fork
#define x__gethostbyname   gethostbyname
#define x__gethostbyaddr   gethostbyaddr
#define x__gethostname     gethostname
#define x__getpeername     getpeername
#define x__getsockname     getsockname
#define x__listen          listen
#define x__perror  	   perror
#define x__rcmd            rcmd
#define x__recv            recv
#define x__recvfrom        recvfrom
#define x__send            send
#define x__sendto          sendto
#define x__shutdown        shutdown
#define x__socket          socket
#define x__strerror	   strerror
#define x__vfork           vfork
#else /* Not BUILD_LIBTERMNET */
/* This is for when the user ports a program to use libtermnet.so.4. */
#define accept          term_accept
#define bind            term_bind
#define chroot          term_chroot
#define close           term_close
#define connect         term_connect
#define dup             term_dup
#define dup2            term_dup2
#define fcntl           term_fcntl
#define fork            term_fork
#define gethostbyname   term_gethostbyname
#define gethostbyaddr   term_gethostbyaddr
#define gethostname     term_gethostname
#define getpeername     term_getpeername
#define getsockname     term_getsockname
#define listen          term_listen
#define perror  	term_perror
#define rcmd            term_rcmd
#define recv            term_recv
#define recvfrom        term_recvfrom
#define send            term_send
#define sendto          term_sendto
#define shutdown        term_shutdown
#define socket          term_socket
#define strerror	term_strerror
#ifndef vfork
#define vfork           term_vfork
#endif /* vfork */
#endif /* BUILD_LIBTERMNET */
#endif /* BUILD_LIBC */
#endif /* TERMWRAP */
#ifdef __cplusplus
}
#endif
