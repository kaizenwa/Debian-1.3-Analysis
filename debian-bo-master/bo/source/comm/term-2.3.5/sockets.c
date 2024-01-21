#define I_SYS
#define I_ERRNO
#define I_STRING
#define I_INET
#define I_IOCTL
#define I_IDENT
#include "includes.h"

#define NOBODY "daemon"

extern char com_result[1024];

struct sockaddr *make_sockaddr(unsigned int port, char *host,
  unsigned long defaultaddr);

char *sockaddr_to_str(struct sockaddr *,int trans);

/* This binds to the specified port, and optionally listens for connections.
 */

#ifdef NO_UNIX_DOMAIN
int unix_domain = 0;
#else
int unix_domain = 1;
#endif

unsigned int getportbyname(char *name,char *type) {
  struct servent *service;
  if (! *name || *name == '/') return 0;
  return (service = getservbyname(name,type)) 
    ? (unsigned int) service->s_port : 0;
}

int bind_tcp_listen(unsigned int port, int queue)
{
  struct sockaddr_in *addr_in;
  int s,tmperrno;

  addr_in = (struct sockaddr_in *)make_sockaddr(port,NULL,INADDR_ANY);

  if (term_localaddr == INADDR_ANY) 
    get_term_localaddr(inet_addr("127.0.0.1"));

  s = x__socket(AF_INET, SOCK_STREAM, 0); 

  if (s == -1) {
    tmperrno = errno;
    sprintf(com_result,"XXXsocket:%s",x__strerror(tmperrno)); 
    errno = tmperrno;
    return -1;
  }

  if (x__bind(s, (struct sockaddr * ) addr_in, sizeof(struct sockaddr)) < 0) {
    tmperrno = errno;
    sprintf(com_result,"XXXbind:%s",x__strerror(tmperrno)); 
    x__close(s);
    errno = tmperrno;
    if (errno != EADDRINUSE) {	/* if it wasn't in use */
      return -1; 		/* then we can't handle it, so abort. */
    }else {
      return -2;		/* handled specially by some clients. */
    }
  }

  if (! queue) return s;

  if (x__listen(s, queue) == -1) {	/* If we can't listen... */
    tmperrno = errno;
    sprintf(com_result,"XXXlisten:%s",x__strerror(tmperrno));
    				/* then just dump. We can't handle */
				/* errors here. */
    x__close(s);
    errno = tmperrno;
    return -1;
  }
  return s;
}

int bind_tcp(unsigned int s) {
  return bind_tcp_listen(s,5);
}

/*	! unix_domain
 *
 *	We bind the socket, and then write the address to the requested
 *	file.  We are the server.
 */

/*	if STREAMS_PIPE
 *
 *	This is a bit of a hack.  If STREAMS_PIPE is defined, we look for the
 *	environment variable TERM_BORROWED_DISPLAY_NUMBER, and interpret the
 *	value as an X-server display number, and use that streams pipe as
 *	our client/server IPC channel.
 *
 *	This is for SCO.  It would probably work on other systems if you
 *	became root and set up the appropriate /dev/X?R and /dev/X?S entries
 */

int bind_unix(char *path) 
{
#ifdef STREAMS_PIPE
  char *borrowed_display_number;
#endif

  int s;

#ifdef STREAMS_PIPE
  borrowed_display_number = getenv("TERM_BORROWED_DISPLAY_NUMBER");
  if (borrowed_display_number != 0) {
    s = open_stream_pipe(atoi(borrowed_display_number));
    if (s < 0) {
      sprintf(com_result, "XXXcannot borrow X display channel %s", borrowed_display_number);
      return -1;
    } else {
      sprintf(com_result, "XXXBorrowed X display channel %d\n", atoi(borrowed_display_number));
      return s;
    }
  }

#endif

  if (! unix_domain) {
    struct sockaddr_in sock_in;
    int size_in;
    int fd;

    if ((s = x__socket(AF_INET, SOCK_STREAM, 0 )) < 0) {
      sprintf(com_result,"XXXsocket:%s",x__strerror(errno));
      return -1;
    }

    sock_in.sin_family = AF_INET;
    sock_in.sin_addr.s_addr = htonl(INADDR_ANY);
    if (! (sock_in.sin_port=getportbyname(path,"tcp"))) {
      sock_in.sin_port = htons(0);
      if (x__bind(s, (struct sockaddr *) &sock_in, sizeof(sock_in)) < 0) {
        sprintf(com_result,"XXXbind:%s",x__strerror(errno));
        x__close(s);
        return -1;
      }
      size_in = sizeof(sock_in);
      if (x__getsockname(s, (struct sockaddr *) &sock_in, &size_in) < 0) {
        sprintf(com_result,"XXXgetsockname:%s",x__strerror(errno));
        x__close(s);
        return -1;
      }
      if ((fd = open(path, O_WRONLY|O_CREAT, 0666)) < 0) {
        sprintf(com_result,"XXXopen:%s",x__strerror(errno));
        x__close(s);
        return -1;
      }else {
        char port[10];

        sprintf(port,"%u\n",(unsigned int) ntohs(sock_in.sin_port));
        if (write(fd, port, sizeof(port)) < 0) {
          sprintf(com_result,"XXXwrite:%s",x__strerror(errno));
          x__close(s);
          return -1;
        }
        x__close(fd);
      }
    }else {
      if (x__bind(s, (struct sockaddr *) &sock_in, sizeof(sock_in)) < 0) {
        sprintf(com_result,"XXXbind:%s",x__strerror(errno));
        x__close(s);
        return -1;
      }
    }

    sprintf(com_result, "XXXOK, Port is %d", ntohs(sock_in.sin_port));
  }
#ifdef NO_UNIX_DOMAIN
  else return -1;
#else
  else{
    struct sockaddr_un sock_un;
    if ((s = x__socket(AF_UNIX, SOCK_STREAM, 0 )) < 0) {
      sprintf(com_result,"XXXsocket:%s",x__strerror(errno));
      return -1;
    }

    sock_un.sun_family = AF_UNIX;
  
    strcpy(sock_un.sun_path, path);
    unlink(sock_un.sun_path);
    if (x__bind(s, (struct sockaddr *) &sock_un, strlen(sock_un.sun_path) + 2) < 0) {
      sprintf(com_result,"XXXbind:%s\n%s %s\n",x__strerror(errno),
        "Can't bind a socket at %s",path);
      x__close(s);
      return -1;
    }
  }
#endif

  /* ok. Start looking for connections. */
  if (x__listen(s, 5) < 0) {
    sprintf(com_result,"XXXlisten%s",x__strerror(errno));
    x__close(s);
    return -1;
  }
  return s;
}

/* Connect to the specified port/address */

int connect_tcp(int s, char *hostname, unsigned int port) {
  struct sockaddr *addr;
  int on=1;

  if (! port) return -1;

  if(term_localaddr == INADDR_ANY)
    get_term_localaddr(inet_addr("127.0.0.1"));

  if (!(addr=make_sockaddr(port,hostname,htonl(term_localaddr)))) {
    sprintf(com_result,"XXXmake_sockaddr:%s",x__strerror(errno));
    if (s >= 0) x__close(s);
    return -1;
  }

  if (s < 0) if ((s = x__socket(AF_INET, SOCK_STREAM, 0 )) < 0) {
    sprintf(com_result,"XXXsocket:%s",x__strerror(errno));
    return -1;
  }

  if (! getuid() || ! geteuid()) {
    struct sockaddr_in sock_in;
    int port = 20;

    port = ntohs(getportbyname("ftp","tcp"));

    sock_in.sin_addr.s_addr = htonl(term_localaddr);
    sock_in.sin_family = AF_INET;
    sock_in.sin_port =
      htons((! port || port == 1 || port > 1024) ? 20 : port - 1);
    setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (char *)&on, sizeof(on));
    if (x__bind(s, (struct sockaddr *) &sock_in, sizeof(sock_in))< 0) {
      sprintf(com_result,"XXXbind %d %lx: %s", port,ntohl(sock_in.sin_addr.s_addr), x__strerror(errno));
      x__close(s);
      return  -1; 
    }
  }
  setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (char *)&on, sizeof(on));
  if (x__connect(s,addr,sizeof(struct sockaddr))<0) {
    sprintf(com_result,"XXXconnect: %s", x__strerror(errno));
    x__close(s);
    return  -1;
  }
  return s;
}

/* This routine allows one to duplicate a term connection without accessing
 * the unix socket.
 */

int duplicate_connect(int s, int Sock)
{
  char port[10];

  if (s < 0) if ((s = x__socket(AF_INET, SOCK_STREAM, 0 )) < 0) {
    sprintf(com_result,"XXXsocket:%s",x__strerror(errno));
    return -1;
  }

  if(send_command(Sock,C_BINDS,1,"%d",0)<0){
    sprintf(com_result,"XXXC_BINDS:%s",command_result);
    x__close(s);
    return -1;
  }
  if(read(Sock,port,sizeof(port))<0){
    sprintf(com_result,"XXXread:%s",x__strerror(errno));
    x__close(s);
    return -1;
  }
  if(port[2]!= I_OK){
    sprintf(com_result,"XXXC_BINDS: port not ok");
    x__close(s);
    return -1;
  }
  return connect_tcp(s,NULL,(unsigned int)atoi(&port[3]));
}

/* This routine connects to a unix socket, intended to be term...
 */

/* if STREAMS_PIPE
 *
 *	This is a bit of a hack.  If STREAMS_PIPE is defined, we look for the
 *	environment variable TERM_BORROWED_DISPLAY_NUMBER, and interpret the
 *	value as an X-server display number, and use that streams pipe as
 *	our client/server IPC channel.
 *
 *	This is for SCO.  It would probably work on other systems if you
 *	became root and set up the appropriate /dev/X?R and /dev/X?S entries
 */


int open_unix(char *p){
  return connect_unix(-1,p);
}

/*      To hack things even more, I now have tried to make it possible to
 *      connect non-unix type sockets to term with the command C_BINDS. (bcr)
 */

int connect_unix(int S, char *p)
{
  struct sockaddr_in sock_in;
#ifdef STREAMS_PIPE
  char *borrowed_display_number;
#endif
  unsigned int port;
  int s = -1;

#ifdef STREAMS_PIPE
  borrowed_display_number = getenv("TERM_BORROWED_DISPLAY_NUMBER");
  if (borrowed_display_number != 0) {
    if(S<0)
      s = MakeStreamPipeConnection(atoi(borrowed_display_number));
    else
      s = S;
    if (s < 0) {
      sprintf(com_result, "XXXcannot borrow X display channel %s", borrowed_display_number);
      return -1;
    } else {
      return s;
    }
  }
#endif

#ifndef NO_UNIX_DOMAIN
  if (unix_domain) {
    struct sockaddr_un sock_un;
	/* First we find out if S is a unix domain socket. */
	/* If it is, we can connect it directly to the term socket. */
    if(S>=0){
      int size_in;

      size_in = sizeof(sock_in);
      if (x__getsockname(S, (struct sockaddr *) &sock_in, &size_in) < 0) {
        sprintf(com_result,"XXXgetsockname:%s",x__strerror(errno));
        x__close(s);
        return -1;
      }
      if(sock_in.sin_family==AF_UNIX) s=S;
    }
    if (s<0) if ((s = x__socket(AF_UNIX, SOCK_STREAM, 0 )) < 0) {
      sprintf(com_result,"XXXsocket:%s",x__strerror(errno));
      return -1;
    }

    sock_un.sun_family =  AF_UNIX;
    strcpy(sock_un.sun_path,p);
 
    if (x__connect(s, (struct sockaddr *) &sock_un, 
	      strlen(sock_un.sun_path) + 2) < 0) {
      sprintf(com_result,"XXXconnect 2:%s",x__strerror(errno));
      if (S != s || S < 0) {
        x__close(s);
      }else
        return -1;
    }else {
	/* If S is a tcp socket, then we duplicate the connection as tcp */
	/* and then close the unix socket we just created. */
      if(S>=0 && S!=s){
        S = duplicate_connect(S, s);
        x__close(s);
        s = S;
      }
      return s;
    }
  }
#endif
	/* Instead of a unix socket, we just have a file with the port # to */
	/* connect to. */
  if (eaccess(p,R_OK|W_OK) >= 0) {
    char sport[10];

    s = open(p, O_RDONLY);
    if (s < 0) {
      sprintf(com_result,"XXXopen:%s",x__strerror(errno));
      return -1;
    }

    if (read(s, sport, sizeof(sport)) < 0) {
      sprintf(com_result,"XXXread:%s",x__strerror(errno));
      x__close(s);
      return -1;
    }
    x__close(s);
    port = (unsigned int)atoi(sport);
  }else {
    port = ntohs(getportbyname(p,"tcp"));
  }

	/* If needed open a socket. */

  return connect_tcp(S,NULL,port);
}

/* This is a short routine that uses ident to determine who the user is. */

char *getconnname(int fd) {
#ifdef USE_IDENT
  char path[PATH_MAX], *p = path, *whoami = NULL;
  struct sockaddr_in laddr, raddr;
  struct linger linger;
  int laddr_len = sizeof(laddr), raddr_len = sizeof(raddr);
  unsigned int rport, lport;
  int i,j=1,s = -1, on = 1;

  while(j) {
    linger.l_onoff = 1;
    linger.l_linger = 0;
    j = 0;
    p = path;
    if (fd < 0) {
      s = x__socket(AF_INET, SOCK_STREAM, 0);
      fd = s;
      memset(&raddr,0,sizeof(raddr));
      raddr.sin_family = AF_INET;
      if (term_localaddr == INADDR_ANY)
        get_term_localaddr(inet_addr("127.0.0.1"));
      raddr.sin_addr.s_addr = htonl(term_localaddr);
      if (! (raddr.sin_port = getportbyname("ident","tcp")))
        raddr.sin_port = htons(113);
      setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (char *)&on, sizeof(on));
      setsockopt(s, SOL_SOCKET, SO_LINGER, (char *)&linger, sizeof(linger));
      if (x__connect(s,(struct sockaddr *)&raddr,raddr_len) < 0) break;
    }
    if (x__getpeername(fd,(struct sockaddr *)&raddr,&raddr_len) < 0) break;
    if (raddr.sin_family != AF_INET) break;
    rport = ntohs(raddr.sin_port);

    if (rport < 1024) {
      strcpy(path,"root");
      whoami = path;
      break;
    }

    if (x__getsockname(fd,(struct sockaddr *)&laddr,&laddr_len) < 0) break;
    if (laddr.sin_family != AF_INET) break;
    lport = ntohs(laddr.sin_port);

    if (! (raddr.sin_port = getportbyname("ident","tcp"))) 
      raddr.sin_port = htons(113);

    if (s < 0) {
      s = x__socket(AF_INET, SOCK_STREAM, 0);
      setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (char *)&on, sizeof(on));
      setsockopt(s, SOL_SOCKET, SO_LINGER, (char *)&linger, sizeof(linger));
      if (x__connect(s,(struct sockaddr *)&raddr,raddr_len) < 0) break;
    }
    if (s < 0) break;

    sprintf(path,"%u , %u\n",rport,lport);
    write(s,path,PATH_MAX);
    *path = 0;
    read(s,path,PATH_MAX);
    linger.l_onoff = 0;
    linger.l_linger = 60;
    setsockopt(s, SOL_SOCKET, SO_LINGER, (char *)&linger, sizeof(linger));

    for (i=0; i<3;i++) {
      p = strchr(p,':');
      if (*p) for(p++; *p && isspace(*p); p++);
      if (! *p) break;
      if (! i && (strncmp(p,"USERID",6) || ! isspace(p[6]))) break;
    }
    for(whoami = p; ! isspace(*p); p++);
    *p = 0;
  }
  if (s >= 0) {
    set_nonblock(s);
    x__close(s);
  }
  return whoami;
#else
  return NULL;
#endif
}

int getuserid(char *whoami) {
  struct passwd *pass;
  if (! whoami || ! (pass = getpwnam(whoami))) return -1;
  return pass->pw_uid;
} 

int getconnid(int fd) {
  return getuserid(getconnname(fd));
}

