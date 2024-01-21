#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <sys/time.h>
#include <fcntl.h>

extern int errno;

#ifdef SOLARIS
#define bzero(s, n) (memset((s), 0, (n)))
#endif

int doConnect(host, port)
char *host;
int port;
{
    struct hostent *hostent, mkHost;
    struct sockaddr_in sockaddr;
    int sock, status;
    int hostaddr, hostaddrPtr[2];
    extern int errno;

    if ((hostent = gethostbyname(host)) == NULL)
    {
	if ((hostaddr = inet_addr(host)) == -1) { return -1; }
	mkHost.h_addr_list = (char **) hostaddrPtr;
	mkHost.h_addr_list[0] = (char *) &hostaddr;
	mkHost.h_addr_list[1] = NULL;
	mkHost.h_length = sizeof(hostaddr);
	mkHost.h_addrtype = AF_INET;
	hostent = &mkHost;
    }
    if ((sock = socket(PF_INET, SOCK_STREAM, 0)) < 0) { return -1; }
    bzero((char *) &sockaddr, sizeof(sockaddr));
    sockaddr.sin_family = AF_INET;
    memcpy((char *) &(sockaddr.sin_addr.s_addr),
	   (char *) hostent->h_addr_list[0],
	   (size_t) hostent->h_length);
    sockaddr.sin_port = htons(port);

    if (connect(sock, (struct sockaddr *) &sockaddr, sizeof(sockaddr)) < 0)
    {
	close(sock);
	return -1;
    }
    return sock;
}

int main(argc, argv)
int argc;
char *argv[];
{
    int fd, port;
    char buffer[4096], host[128];
    int l, tl = 0, ntl, pid, out;
    time_t st;

    pid = getpid();
    printf("%d\n", pid);
    fflush(stdout);
    gethostname(host, 128);
    port = atoi(argv[2]);
    if ((fd = doConnect(argv[1], port)) < 0 )
    {
	printf("%d DCCError Get %s from %s connection failed %d.\n", pid, argv[3], 
	  argv[5], errno);
	fclose(stdout);
	exit(1);
    }
    printf("%d DCC Get connection to %s established.\n", pid, argv[5]);
    fflush(stdout);
    st = time((time_t *) 0);
    l = -1;
    if ((out = open(argv[3], O_WRONLY | O_CREAT | O_TRUNC, 0600)) >= 0)
    {
	while ((l = read(fd, buffer, 4096)) > 0) 
	{
	    tl += l;
	    printf("%d DCC Get progress to %s %d\n", fd, argv[5], tl);
	    fflush(stdout);
	    if (write(out, buffer, l) != l) 
	    {
		sprintf(host,
		  "%d DCCError Get: %s from %s Write error on output.\n", pid,
		  argv[3], argv[5]);
		l = -1;
		break;
	    }
	    ntl = htonl(tl);
	    if (write(fd, (char *) &ntl, sizeof(int)) != sizeof(int))
	    {
		sprintf(host,
		  "%d DCCError Get: %s from %s Write error on transfer.\n", pid,
		  argv[3], argv[5]);
		l = -1;
		break;
	    }
	}
    }
    else
    {
	sprintf(host,
	  "%d DCCError Get: %s from %s Cannot create output file.\n", pid,
	  argv[3], argv[5]);
    }
    (void) close(out);
    (void) shutdown(fd, 2);
    (void) close(fd);    
    if (l < 0)
    {
	(void) unlink(argv[3]);
    }
    else 
    {
	if ((st = time((time_t *) 0) - st) == 0)
	{
	    st = 1;
	}
	sprintf(host, "%d DCC Get: %s from %s completed. %f Kbytes/sec\n", pid,
	    argv[3], argv[5], ((float) tl /1000.0) / (float) st);
    }
    printf("%s", host);
    fclose(stdout);
    exit(0);
}
