#ifdef SVR3
/*
 * This file defines the socketpair code for ISC unix.
 */

#define I_ERRNO
#define I_TYPES
#define I_IOCTL
#include "includes.h"

int term_socketpair(int domain,
	   int type,
	   int protocol,
	   int fds[2])
{
  int d1, d2, nd1;
  int status;
  int flags;
  struct sockaddr_in master;
  struct sockaddr_in slave;
  int mode;
  int slavesz;

  d1 = x__socket(domain, type, protocol);
  if (d1 < 0)
      return -1;
  /*
   * Now bind a name to the master socket.
   */
  master.sin_port = htons(9693);
  master.sin_family = AF_INET;
  master.sin_addr.s_addr = htonl(0x7f000001);

  if (x__bind(d1, &master, sizeof(master)) < 0) {
    x__perror("Bind failed for master socket to port 9693");
    exit(1);
  }

  /*
   * I now have both sockets open.  I need to set the O_NDELAY flag
   * for the master so I can do a listen and then the connect from the
   * slave.
   */
#ifdef DONTDELAY
  flags = x__fcntl(d1, F_GETFL, 0);
  if (flags < 0) {
    status = errno;
    x__close(d1);
    errno = status;
    return flags;
  }

  flags |= O_NDELAY;

  if (x__fcntl(d1, F_SETFL, flags) < 0) {
    x__perror("fcntl set flags failed");
    return -1;
  }
#endif

  status = x__listen(d1, 1);
    
  if (status == -1 && (errno != EAGAIN && errno != EINTR)) {
    x__perror("listen failed");
  }

#ifdef DONTDELAY
  flags &= ~O_NDELAY;
  if (x__fcntl(d1, F_SETFL, flags) < 0) {
    x__perror("fcntl set flags failed");
    exit(1);
  }
#endif

  d2 = x__socket(domain, type, protocol);
  if (d2 < 0) {
    status = errno;
    x__close(d1);
    errno = status;
    return d2;
  }

  status = x__connect(d2, &master, sizeof(struct sockaddr_in));

  if (status == -1) {
    x__perror("Error connecting from slave to master");
    return -1;
  } 

  slave = master;

  nd1 = x__accept(d1, &slave, &slavesz);
  if (nd1 == -1) {
    x__perror("Error accepting connection from slave");
    return -1;
  }
  /*
   * At this point I should be connected on the two sockets.
   */
  x__close(d1);
  fds[0] = nd1;
  fds[1] = d2;
  return 0;
}

#else /* keep ranlib from complaining */
int term_socketpair(int domain,
	   int type,
	   int protocol,
	   int fds[2]) {return -1;}
#endif
