/*
 *
 * $Id: gwm.shar,v 1.115 1995/12/08 07:51:55 colas Exp $
 */

#include "spy.h"


#ifdef __STDC__
int
KoalaSpy_SendPacket(char *host, /* 0 -> SPY_HOST */
		    int port,   /* 0 -> SPY_PORT */
		    char *progName,   /* defines the log file */
		    char *origin,    /* 0 -> hostname */
		    char *msg)
#else /* !__STDC__ */
KoalaSpy_SendPacket(host, port, progName, origin, msg)
    char *host;
    int port;  
    char *progName;
    char *origin;  
    char *msg;
#endif /* !__STDC__ */
{
#ifndef NO_KOALA_SPY
/*
  char *host = SPY_HOST;
  int port = SPY_PORT;
  */
    int rc;

    struct servent *sp;
    struct sockaddr_in sin;
    struct hostent *h;
    int sock = 0;
    int flags = 0;

    if (getenv("NO_KOALA_SPY")) {
	return 0;
    }

    /* getting hostname */
    if (!(h = gethostbyname(host ? host : SPY_HOST))) {
	return 1;
    }

    /* creating a socket */
    if ((sock = socket (AF_INET, SOCK_DGRAM, 0)) < 0) {
	return 2;
    }

    memset(&sin, 0, sizeof (sin));
    sin.sin_family = AF_INET;

    /* providing host server identity */
    memcpy(&sin.sin_addr, h->h_addr, h->h_length);

    /* affecting the port number of the server to the sin structure */
    sin.sin_port = htons(port ? port : SPY_PORT);

    /* connect to the server */
#if defined(linux) || defined(SVR4)
    rc = connect (sock, (struct sockaddr *) &sin, sizeof (sin));
#else
    rc = connect (sock, &sin, sizeof (sin));
#endif
    if (rc < 0) {
	return 3;
    }


    /* Make all sockets blocking */
    fcntl(sock, F_GETFL, &flags);
    flags &= ~O_NDELAY;
    fcntl(sock, F_SETFL, flags);

    /*
    ** Sending the string
    */
    {
	unsigned char buf[SPY_PACKET_SIZE], h[256];
	int index;
	int msgLen;    /* left = msgLen + 2 */

	if (! origin)
	    gethostname(h, 256);
	else
	    strncpy(h, origin, 256);
	h[255] = '\0';

	/* create message */
	sprintf(buf, "XXX%s %s : ", progName, h);
	msgLen = strlen(buf);
	strncpy(buf + msgLen, msg, SPY_PACKET_SIZE - msgLen - 1);
	buf[SPY_PACKET_SIZE - 1] ='\0';

	msgLen = strlen(buf) + 1;
	if (msgLen > SPY_PACKET_SIZE - 3)
	    msgLen = SPY_PACKET_SIZE - 3;

	{   /* write magic number and length of message */
	    int i = 0;
	    buf[i++] = SPY_MAGIC_NUMBER & 0xff;
	    buf[i++] = msgLen / 256;
	    buf[i++] = msgLen % 256;
	}

	/* then message itself */
	{
	    int written;
	    int index = 0;
	    while (index < SPY_PACKET_SIZE) {
		written = write(sock, & buf[index], SPY_PACKET_SIZE - index);
		if (written <= 0) {
		    return(6);
		}
		index += written;
	    }
	}

	close(sock);
    }
#endif /* !NO_KOALA_SPY */

    return 0;

}
