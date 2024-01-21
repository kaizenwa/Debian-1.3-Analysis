/*
 * $Source: /home/nlfm/Working/Zircon/Released/RCS/dccsend.c,v $
 * $Date: 1996/06/04 08:34:07 $
 * $Revision: 1.17.1.1 $
 */
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <sys/time.h>
#include <fcntl.h>

#ifdef SOLARIS
#define bzero(s, n) (memset((s), 0, (n)))
#endif

#ifdef AIX
#include <sys/select.h>
#endif

int doServer(port)
int *port;
{
    struct hostent *hostent, mkHost;
    struct sockaddr_in sockaddr;
    int sock, status;
    int hostaddr, hostaddrPtr[2];

    if ((sock = socket(PF_INET, SOCK_STREAM, 0)) < 0)
    {
	return -1;
    }
    hostaddr = INADDR_ANY;
    mkHost.h_addr_list = (char **) hostaddrPtr;
    mkHost.h_addr_list[0] = (char *) &hostaddr;
    mkHost.h_addr_list[1] = NULL;
    mkHost.h_length = sizeof(hostaddr);
    mkHost.h_addrtype = AF_INET;
    hostent = &mkHost;

    bzero((char *) &sockaddr, sizeof(sockaddr));
    sockaddr.sin_family = AF_INET;
    memcpy((char *) &(sockaddr.sin_addr.s_addr),
	   (char *) hostent->h_addr_list[0],
	   (size_t) hostent->h_length);
    sockaddr.sin_port = htons(0);

    if (bind(sock, (struct sockaddr *) &sockaddr, sizeof(sockaddr)) < 0)
    {
	(void) close(sock);
	return -1;
    }
    status = sizeof(sockaddr);
    getsockname (sock, (struct sockaddr *) &sockaddr, &status);
    *port = ntohs(sockaddr.sin_port);
    return sock;
}

int doaccept(fdi)
int fdi;
{
    struct sockaddr_in sockaddr;
    int len = sizeof sockaddr;
    int fd;

    fd = accept (fdi, (struct sockaddr *) &sockaddr, &len);
    (void) shutdown(fdi, 2);
    (void) close(fdi);
    return (fd < 0) ? -1 : fd;
}

int main(argc, argv)
int argc;
char *argv[];
{
    int fd, g, blks, snt, prog, port;
    char buffer[4096], host[128];
    int l, tl = 0, i, pid;
    struct timeval timeout;
    fd_set rdset;
    struct stat fs;
    time_t st;
    double size;
    extern int errno;

    pid = getpid();
    if ((fd = doServer(&port)) < 0 || port == 0)
    {
	exit(1);
    }
    printf("%d %d\n", port, pid);
    fflush(stdout);
    (void) close(2);
    listen(fd, 5);
    printf("%d DCC Send accept to %s established.\n", pid, argv[3]);
    fflush(stdout);
    if ((fd = doaccept(fd)) < 0) 
    {
        printf("%d DCCError Send %s to %s: Accept error %d.\n", pid,
	   argv[1], argv[3], errno);
	fclose(stdout);
	exit(1);
    }
    printf("%d DCC Send connection to %s established.\n", fd, argv[3]);
    fflush(stdout);
    if ((g = open(argv[1], O_RDONLY)) < 0)
    {
	printf("%d DCCError Send %s to %s: Cannot read file.\n", pid, 
	        argv[1], argv[3]);
	fflush(stdout);
    }
    else
    {
	fstat(g, &fs);
	blks = (fs.st_size/2048 + 1) / 10;
	prog = 0;
	snt = 0;
	st = time((time_t *) 0);
	while ((l = read(g, buffer, 2048)) > 0)
	{
	    tl += l;
	    if (write(fd, buffer, l) != l) 
	    {
		sprintf(host,
		  "%d DCCError Send %s to %s: Write error.\n", pid, 
		  argv[1], argv[3]);
		l = -1;
		break;
	    }
	    snt += 1;
	    if (snt > blks)
	    {
		snt = 0;
		prog += 10;
		printf("%d DCC Send progress to %s %d\n", fd, argv[3],prog);
		fflush(stdout);
	    }
	    do
	    {
		FD_ZERO(&rdset);
		FD_SET(fd, &rdset);
		timeout.tv_sec = 10;
		timeout.tv_usec = 0;
#ifdef __hpux   
                if (select(fd + 1, (int *)&rdset, NULL, NULL, &timeout) != 1)
#else           
                if (select(fd + 1, &rdset, NULL, NULL, &timeout) != 1)
#endif /* __hpux */
		{
		    l = -1;
		    break;
		}
		else
		{
		    if (read(fd, (char *) &i, sizeof(int)) != sizeof(int)) 
		    {
			sprintf(host,
			  "%d DCC Send %s to %s: Read error.\n", pid, 
			  argv[1], argv[3]);
			l = -1;
			break;
		    }
		}
	    }
	    while (ntohl(i) != tl);
	
	}
    }
    (void) shutdown(fd, 2);
    (void) close(fd);
    (void) close(g);
    if (l >= 0)
    {
	if ((st = time((time_t *) 0) - st) == 0)
	{
	    st = 1;
        }
	sprintf(host, "%d DCC Send %s to %s completed. %f Kbytes/sec", pid, 
		argv[1], argv[3], (((float) tl / 1000.0) / (float) st));
    }
    printf("%s", host);
    fclose(stdout);
    exit((l >=0 ) ? 0 : 1);
}
