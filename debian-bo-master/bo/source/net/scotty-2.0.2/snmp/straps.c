/*
 * straps.c:						Sept 1994
 *
 * A simple SNMP trap-sink. Mainly for scotty's snmp code, but
 * commonly useable by other clients.
 *
 * The trap demon listens to the snmp-trap port 162/udp and forwards
 * the received event to connected clients (like scotty).  
 *
 * Because the port 162 needs root access and the port can be opend
 * only one time, the use of a simple forwarding demon is a good choice.
 *
 * The client can connect to the AF_UNIX domain stream socket
 * /tmp/.straps and will get the trap-packets in raw binary form:
 *	4 bytes ip-address (in network-byte-order) of the sender
 *	2 bytes port-number (in network-byte-order) of the sender
 *	4 bytes data-length (in host-byte-order) followed by the 
 *	n data-bytes of the packet.
 */

#include <config.h>

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/un.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

#ifndef FD_SETSIZE
#define FD_SETSIZE 32
#endif


/* default trap service name: */
static char *snmp_trap_service_name = "snmp-trap";

/* default trap service port (if service_name failed): */
static short snmp_trap_service_port = 162;

/* default path to the unix-stream service socket: */
static char *serv_path = "/tmp/.straps";

/* default multicast IP address: */
static char *mcast_address = "234.0.0.1";


#ifdef SIGPIPE
/* ignore broken pipes */
static void
ign_pipe ()
{
    /* restart signalhandler for all these bozo's outside: */
    signal (SIGPIPE, ign_pipe);
}
#endif


int
main (argc, argv)
int argc;
char *argv[];
{
    struct servent *se;
    struct sockaddr_in taddr, laddr;
    struct sockaddr_un saddr, daddr;
    int trap_s, serv_s, slen, dlen, llen, rc, i;
    fd_set fds;
    static int cl_addr [FD_SETSIZE];
    char buf [2048];
    int go_on;
    int mcast_s = -1;
    
    /*
     * check args:
     */

    if (argc > 1) {
	fprintf (stderr, "use:  straps\nto run the snmp-trap demon.\n");
	exit (1);
    }
    
    /*
     * fetch trap port:
     */
    
    if ((se = getservbyname (snmp_trap_service_name, "udp"))) {
	snmp_trap_service_port = se->s_port;
    } else {
	snmp_trap_service_port = htons (snmp_trap_service_port);
    }      

    /*
     * open trap port:
     */

    if ((trap_s = socket (AF_INET, SOCK_DGRAM, 0)) < 0) {
	perror ("straps: unable to get trap socket");
	exit (1);
    }
    
    taddr.sin_family = AF_INET;
    taddr.sin_port = snmp_trap_service_port;
    taddr.sin_addr.s_addr = INADDR_ANY;

    if (bind (trap_s, (struct sockaddr *) &taddr, sizeof (taddr)) < 0) {
	perror ("straps: unable to bind trap socket");
	exit (1);
    }

#ifdef HAVE_MULTICAST
    if ((mcast_s = socket (AF_INET, SOCK_DGRAM, 0)) < 0) {
	perror ("straps: unable to get multicast trap socket");
    }

    if (mcast_s > 0) {
        struct ip_mreq mreq;
        mreq.imr_multiaddr.s_addr = inet_addr(mcast_address);
	mreq.imr_interface.s_addr = htonl(INADDR_ANY);
	if (setsockopt(mcast_s, IPPROTO_IP, IP_ADD_MEMBERSHIP, (char*) &mreq,
		       sizeof(mreq)) == -1) {
	    perror ("straps: unable to join multicast group");
	    close(mcast_s);
	    mcast_s = -1;
	}
    }

#ifdef SO_REUSEADDR
    /* Allow others to bind to the same UDP port. */
    if (mcast_s > 0) {
        int on = 1;
	setsockopt(mcast_s, SOL_SOCKET, SO_REUSEADDR, 
		   (char *) &on, sizeof(on));
    }
#endif

    if (mcast_s > 0) {
        struct sockaddr_in maddr;
        maddr.sin_family = AF_INET;
	maddr.sin_port = snmp_trap_service_port;
	maddr.sin_addr.s_addr = htonl(INADDR_ANY);
	if (bind(mcast_s, (struct sockaddr*) &maddr, sizeof(maddr)) == -1) {
	    perror ("straps: unable to bind multicast trap socket");
	    close(mcast_s);
	    mcast_s = -1;
	}
    }
#endif

    /*
     * open client socket:
     */

    /* remove old socket: */
    unlink (serv_path);

    if ((serv_s = socket (AF_UNIX, SOCK_STREAM, 0)) < 0) {
	perror ("straps: unable to get server socket");
	exit (1);
    }

    memset ((char *) &saddr, 0, sizeof(saddr));
    
    saddr.sun_family = AF_UNIX;
    strcpy (saddr.sun_path, serv_path);
    slen = sizeof(saddr) - sizeof(saddr.sun_path) + strlen(saddr.sun_path);

    if (bind (serv_s, (struct sockaddr *) &saddr, slen) < 0) {
	perror ("straps: unable to bind server socket");
	exit (1);
    }
    
    if (listen (serv_s, 5) < 0) {
	perror ("straps: unable to listen on server socket");
	exit (1);
    }

#ifdef SIGPIPE
    /* ignore broken pipes */
    signal (SIGPIPE, ign_pipe);
#endif
    
    /*
     * fine anything is ready; lets listen for events: 
     * the for(;;) loop aborts, if the last client went away.
     */

    for (go_on = 1; go_on; ) {

	  FD_ZERO (&fds);
	  FD_SET (trap_s, &fds);
	  FD_SET (serv_s, &fds);
	  if (mcast_s > 0) {
	      FD_SET (mcast_s, &fds);
	  }

	  /* fd's connected from clients. listen for EOF's: */
	  for (i = 0; i < FD_SETSIZE; i++) {
	      if (cl_addr [i] > 0) FD_SET(i, &fds);
	  }

	  rc = select (FD_SETSIZE, &fds, (fd_set *) 0, (fd_set *) 0, 
		       (struct timeval *) 0);
	  if (rc < 0) {
	      if (errno == EINTR || errno == EAGAIN) continue;
	      perror ("straps: select failed");
	  }
	  
	  if (FD_ISSET (trap_s, &fds)) {
	      /* read trap message and forward to clients: */
	      llen = sizeof(laddr);
	      if ((rc = recvfrom(trap_s, buf, sizeof (buf), 0, 
				 (struct sockaddr *) &laddr, &llen)) < 0) {
		  perror ("straps: recvfrom failed");
		  continue;
	      }
	      
	      for (i = 0; i < FD_SETSIZE; i++) {
		  if (cl_addr [i] > 0) {
		      /* XXX: check writeability */
		      if (write (i, (char *) 
				 &laddr.sin_addr.s_addr, 4) != 4
			  || write (i, (char *) &laddr.sin_port, 2) != 2
			  || write (i, (char *) &rc, 4) != 4
			  || write (i, buf, rc) != rc)
			{
			    cl_addr [i] = 0;
				close (i);
			}
		  }
	      }
	      
	      /* should we go on ? */
	      for (go_on = 0, i = 0; i < FD_SETSIZE; i++) {
		  go_on += cl_addr [i] > 0;
	      }

	  } else if (mcast_s > 0 && FD_ISSET (mcast_s, &fds)) {
	      /* read trap message and forward to clients: */
	      llen = sizeof(laddr);
	      if ((rc = recvfrom(mcast_s, buf, sizeof (buf), 0, 
				 (struct sockaddr *) &laddr, &llen)) < 0) {
		  perror ("straps: recvfrom failed");
		  continue;
	      }
	      
	      for (i = 0; i < FD_SETSIZE; i++) {
		  if (cl_addr [i] > 0) {
		      /* XXX: check writeability */
		      if (write (i, (char *) 
				 &laddr.sin_addr.s_addr, 4) != 4
			  || write (i, (char *) &laddr.sin_port, 2) != 2
			  || write (i, (char *) &rc, 4) != 4
			  || write (i, buf, rc) != rc)
			{
			    cl_addr [i] = 0;
				close (i);
			}
		  }
	      }
	      
	      /* should we go on ? */
	      for (go_on = 0, i = 0; i < FD_SETSIZE; i++) {
		  go_on += cl_addr [i] > 0;
	      }

	  } else if (FD_ISSET (serv_s, &fds)) {
	      /* accept a new client: */
	      memset ((char *) &daddr, 0, sizeof(daddr));
	      dlen = sizeof (daddr);
	      
	      rc = accept (serv_s, (struct sockaddr *) &daddr, &dlen);
	      if (rc < 0) {
		  perror ("straps: accept failed");
		  continue;
	      }
	      cl_addr [rc] = 1;

	  } else {
	      /* fd's connected from clients. (XXX: should check for EOF): */
	      for (i = 0; i < FD_SETSIZE; i++) {
		  if (cl_addr [i] > 0 && FD_ISSET (i, &fds)) {
		      cl_addr [i] = 0;
		      close (i);
		  }
	      }
	      
	      /* should we go on ? */
	      for (go_on = 0, i = 0; i < FD_SETSIZE; i++) {
		  go_on += cl_addr [i] > 0;
	      }
	  }
	  
      } /* end for (;;) */

    unlink (serv_path);

    return 0;
}

/* end of straps.c */
