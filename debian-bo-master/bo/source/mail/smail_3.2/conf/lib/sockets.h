/* @(#) sockets.h,v 1.2 1992/09/20 18:51:11 tron Exp -  */

/*
 * sockets.h:
 *   Inclusions required to use BSD-style sockets.
 *
 * These includes can be overridden by the SOCKET_INCLUDES variable
 * in the EDITME or conf/os files.
 */

#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#ifdef HAVE_BIND
# undef NOERROR		/* remove conflict in SVR4 header files */
# include <arpa/nameser.h>
# include <resolv.h>
#endif
