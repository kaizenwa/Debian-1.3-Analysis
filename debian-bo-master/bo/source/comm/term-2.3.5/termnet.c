/* Most of the routines in this file are just quick hacks to get things
 * running. The general idea is that term is only used when the local
 * gethostbyname fails to find anything.  Many of these routines are just
 * quick hacks and can be greatly improved.
 */

/* Everything that follows is an attempt to allow term to easier porting
 * of software to term.  The functions term_connect, term_rcmd, and 
 * term_gethostbyname are written such that ported routines will work
 * even with ordinary network connections.  Unfortunately, I haven't
 * figured out how to make term_bind network invisible yet.
 */

#define I_ARGS
#define I_IOCTL
#define I_SYS
#define I_GETOPT
#define I_STAT
#define I_SIGNAL
#define I_STRING
#define I_ARGS
#define I_INET
#define I_ERRNO
#define I_CTYPE
#include "includes.h"
#include "client.h"

#ifndef EOPNOTSUPP
#define EOPNOTSUPP ~0
#endif
#ifndef ENOTCONN
#define ENOTCONN EOPNOTSUPP
#endif
#ifndef EBADF
#define EBADF ENOTCONN
#endif

extern int use_term;

void get_term_localaddr(unsigned long);
void term_do_exit(void);
struct sockaddr *str_to_sockaddr(char *, unsigned long);
char *sockaddr_to_str(struct sockaddr *, int trans);
struct hostent *host_lookup(char *,int,int,int,struct hostent *);

int term_debug = 0;

#ifdef NO_LOOPBACK
int use_term = 1;
#else
int use_term = -1;
#endif

static int tmp_errno = 0;
static int Sock = -1;

/* This is a way to disable termnet commands that aren't smart enough */
/* to automatically determine if term commands should be used. */
static int remote_connect = -1;

/* This is needed for term_getsockname. */
struct sockinfo_type {
  int sock;
  int client;
  int sockport;
  int how;
  struct sockaddr *udp_addr;
};

static struct sockinfo_type sockinfo[MAX_CLIENTS];
static int udp_pending[MAX_CLIENTS];
static int pending_count=0;

static char term_error[1024];

#ifndef USE_PROGNAME
#define progname ""
#define get_command() ""
#else
static char *progname = NULL;

static char *get_command(void) {
  int i=0;
  char *cmdline = "";
  extern char **environ;

  progname = "";
  if(environ) {
    for (cmdline = *environ; cmdline && (i<2);)
      i = (! *(--cmdline)) ? i + 1 : 0;
    if (cmdline) 
      for (progname = (cmdline += 2); cmdline = strchr(cmdline,'/');
        progname = cmdline);
  }
  return progname;
}
#endif

static void print_err(int level) {
  if ((term_debug & level) && term_error[0]) {
    fprintf(stderr,"%s\n",term_error);
    term_error[0] = 0;
  }
  if (level == 32768)
    term_error[0] = 0;
  errno = tmp_errno;
  return;
}

static struct hostent *str_to_hostent(char *addr){
  static unsigned long int laddr=0;
  static struct hostent hs;
  static char host[258];
  static char *caddr[2]={NULL,NULL};
  static char *host_aliases[2]={NULL,NULL};

  host_aliases[0]=(char *)hs.h_name;

  hs.h_name=host;

  sscanf(addr,"%lx %258s",&laddr,hs.h_name);
  if(! laddr) return NULL;

  laddr = htonl(laddr);
  caddr[0]=(char *) &laddr;

/* I'm using 0 as remote host. */

  hs.h_aliases = host_aliases;
  hs.h_addr_list = caddr;
  hs.h_length = sizeof(unsigned long int);
  hs.h_addrtype = AF_INET; 
  return &hs;
}


/* To keep the lookups accurate, I need this.
 */

static void close_fd(int fd,int how){
  int i, s, j;
#if 1
#endif

  for(i=0;i<pending_count;i++) if(udp_pending[i] == fd) {
    udp_pending[i] = -1;
    if (i == pending_count + 1)
      --pending_count;
    else
      break;
  }

  if (remote_term_version > 11862) {
    if (Sock < 0)
      Sock = socket_connect_server(-1,term_server);
    s = Sock;
  }else s = -1;


  if(fd>=0)
    for(i=0;i<MAX_CLIENTS;i++)
      if (fd == sockinfo[i].sock) {

#if STUPID_SHUTDOWN
    if ((sockinfo[i].how  |= 3|(how + 1) ) == 3) {
#endif
      sockinfo[i].sock = -1;

      if (s >= 0 && sockinfo[i].client >= 0) {
        if (sockinfo[i].udp_addr != NULL) {
          for(j=0;j<MAX_CLIENTS;j++)
            if (sockinfo[j].udp_addr != NULL && sockinfo[j].sock >= 0)
              if (!memcmp(sockinfo[i].udp_addr,
                sockinfo[j].udp_addr,sizeof(struct sockaddr))) break;
          if (j == MAX_CLIENTS) { 
            if ((j = send_command(s, C_CLOSE, 1, "%d", sockinfo[i].client)) < 0) {
              tmp_errno = -1;
              sprintf(term_error, "Term: C_CLOSE 1 %d failed: %s",sockinfo[i].client,
                command_result);
              print_err(1);
              if (j < -1) {
                x__close(s);
                s = -1;
              }
            }
          }
        }
      }
      if (sockinfo[i].udp_addr != NULL)
        free(sockinfo[i].udp_addr);
      sockinfo[i].udp_addr = NULL;
      sockinfo[i].sockport = 0;
      sockinfo[i].how = 0;
#if STUPID_SHUTDOWN
    }
#endif
  }
}

/* This is a client lookup for sockets necessary for term_accept 
 * and term_sendto
 */

static int store_sockinfo(int s, int client, struct sockaddr *udp_addr) {
  static char term_clients[MAX_CLIENTS*4];
  static int count = -1;
  char *ptr,*nptr;
  int i,k,port;

  if (count < 0) {
#if 0	/* This isn't necessary any more */
    atexit(term_do_exit); 
#endif 
	/* Zero everything */
    count=0;
    for(i=0;i<MAX_CLIENTS;i++) {
      sockinfo[i].sock = -1;
      sockinfo[i].udp_addr = NULL;
      sockinfo[i].sockport = 0;
      sockinfo[i].how = 0;
    }

    /* Restore any client information
	*
	* TERMCLIENTS is of the form 
	* 	"TERMCLIENTS=-m f0 c0 p0 s0 ... fN cN pN sN"
	* where f0 is clients[].sock, c0 is clients[].client,
	* p0 is port in clients[].udp_addr (0 if none)
        * and s0 is socket name port in clients[].sockport
	* m is the number of clients per file descriptor.
	*/

    if ((ptr = getenv("TERMCLIENTS"))) {
      int n = 0, m = 3;
      while(*ptr && n<MAX_CLIENTS) {
        if (remote_connect < 0) {
          use_term_command(PUBLIC);
          remote_connect=1;
        }
        if (Sock < 0) 
          if ((Sock=socket_connect_server(-1,term_server)) < 0) {
            tmp_errno = -1;
            sprintf(term_error, "Term 3: %s", command_result);
            print_err(1);
            remote_connect=0;
            return MAX_CLIENTS;
        }
        pending_count=0;
        sockinfo[n].sock = strtol(ptr,&nptr,10);
	if (n == 1 && !(*nptr) && sockinfo[n].sock < 0) {
          m = -sockinfo[n].sock;
          sockinfo[n].sock = strtol(ptr,&nptr,10);
        }
	sockinfo[n].how  = 0;
        if(!(*nptr) || m < 2) break;
        sockinfo[n].client = strtol(nptr,&ptr,10);
        nptr = ptr;
        if(!(*nptr) || m < 3) break;
        port = strtol(nptr,&ptr,10);
        if(port) {
          struct sockaddr_in addr_udp;

          if (term_localaddr == INADDR_ANY)
            get_term_localaddr(inet_addr("127.0.0.1"));
          addr_udp.sin_addr.s_addr = htonl(term_localaddr);
          addr_udp.sin_port = htons(port);
          memcpy(sockinfo[n].udp_addr,&addr_udp,sizeof(udp_addr));
        }
        if (!(*nptr) || m < 4) break;
        sockinfo[n].sockport = strtol(nptr,&ptr,10);
        n++;
      }
    }
  }
  if (s < 0) return MAX_CLIENTS;

  for(k=0;k<MAX_CLIENTS;k++)
    if(sockinfo[k].sock<0||s == sockinfo[k].sock) break;
  if(k==MAX_CLIENTS){
    k = ++count % MAX_CLIENTS;
    count = k;
    close_fd(sockinfo[k].sock,2);
    tmp_errno = -1;
    sprintf(term_error,"Too many sockets open.  Killing one.");
    print_err(1);
  }
  sockinfo[k].sock = s;
  sockinfo[k].client = client;
  sockinfo[k].how  = 0;
  if (! udp_addr) {
    sockinfo[k].udp_addr = NULL;
  }else {
    sockinfo[k].udp_addr = (struct sockaddr *)malloc(sizeof(struct sockaddr));
    memcpy(sockinfo[k].udp_addr,udp_addr,sizeof(struct sockaddr));
  }
	/* Store sockinfo for the children */
  strcpy(term_clients,"TERMCLIENTS= -4");
  for(i=0;i<MAX_CLIENTS;i++) {
    if(sockinfo[i].sock < 0) continue;
    if (sockinfo[i].udp_addr != NULL) {
      struct sockaddr_in *addr_in = (struct sockaddr_in *)&sockinfo[i].udp_addr;
      sprintf(&term_clients[strlen(term_clients)]," %d %d %u %u",
        sockinfo[i].sock,sockinfo[i].client,
        ntohs(addr_in->sin_port), sockinfo[i].sockport);
    }else {
      sprintf(&term_clients[strlen(term_clients)]," %d %d 0 %u",
        sockinfo[i].sock,sockinfo[i].client, sockinfo[i].sockport);
    }
  }
  term_putenv(term_clients);

  return k;
}

static int term_udpsocket(int S) {
  struct sockaddr udp_addr;
  int s,i,client,len;
  struct sockaddr_in *addr_in=NULL;

  if (remote_connect < 0) {
    remote_connect=use_term_command(PUBLIC);
    store_sockinfo(-1,-1,NULL);
  }

  if (term_debug & 32768) {
    tmp_errno = errno;
    sprintf(term_error,"test term_udpsocket");
    print_err(32768);
  }

  for(i=0;i<pending_count;i++) if(udp_pending[i] == S) {
    udp_pending[i] = -1;
    if (i == pending_count + 1)
      --pending_count;
    else
      break;
  }

  if (i == pending_count) return MAX_CLIENTS;

  if (Sock < 0) 
    if ((Sock=socket_connect_server(-1,term_server)) < 0) {
    tmp_errno = ~0;
    sprintf(term_error, "Term 3: %s", command_result);
    print_err(1);
    return MAX_CLIENTS;
  }

  if ((s=duplicate_connect(-1,Sock)) < 0) {
    tmp_errno = ~0;
    sprintf(term_error,"Term: connection failed: %s",command_result);
    print_err(1);
    return MAX_CLIENTS;
  }

  if ((i=send_command(s, C_STATS, 1, "%d", -6)) < 0) {
    tmp_errno = ~0;
    sprintf(term_error,"Term: C_STATS -6 failed: %s",command_result);
    print_err(1);
    x__close(s);
    if (i < -1) {
      x__close(s);
      s = -1;
    }
    return MAX_CLIENTS;
  }
  client = atoi(command_result);

  if ((i=send_command(Sock, C_USOCK, 0, "%d %d", client,
          (UDP_T_SENDSTRIPHDR | UDP_T_RECADDHDR))) < 0) {
    tmp_errno = ~0;
    sprintf(term_error,"Term: C_USOCK remote %d failed: %s",
      client,command_result);
    print_err(1);
  }else if ((i=send_command(Sock, C_USOCK, 1, "%d %d", client,
      (UDP_T_SENDIGNOREHDR))) < 0) {
    tmp_errno = ~0;
    sprintf(term_error,"Term: C_USOCK local %d failed: %s",
      client,command_result);
    print_err(1);
  }

  if (i < -1) term_do_exit();
  x__close(s);

  if (i < 0) return MAX_CLIENTS;

  if (term_localaddr == INADDR_ANY) get_term_localaddr(inet_addr("127.0.0.1"));
  if ((i=send_command(Sock, C_UBIND, 1, "%d %d", client, 0)) < 0) {
    tmp_errno = ~0;
    sprintf(term_error,"Term: C_UBIND failed: %s",command_result);
    print_err(1);
  }else if ((i=send_command(Sock, C_GETSOCKNAME, 1, "%d", client)) < 0) {
    tmp_errno = ~0;
    sprintf(term_error,"Term: C_GETSOCKNAME failed: %s",command_result);
    print_err(1);
  }else if (!(addr_in=(struct sockaddr_in *)str_to_sockaddr(command_result,
    htonl(term_localaddr)))) {
    tmp_errno = ~0;
    sprintf(term_error,"Can't translate socket address: %s",command_result);
    print_err(1);
    i = -1;
  }

  if (i < 0) {
    if (i < -1) term_do_exit();
    if (S >= 0) 
      term_close(S);
    return MAX_CLIENTS;
  }

  len = sizeof(udp_addr);
  memcpy(&udp_addr, addr_in, len);
  return store_sockinfo(S,client,&udp_addr);
}

void term_do_exit(void) {
  int k;
  for(k=0;k<MAX_CLIENTS;k++)
    if (sockinfo[k].sock > 0) term_close(k);
  if (Sock >= 0) x__close(Sock);
  Sock = -1;
  return;
}

/* This handles printing termnet errors after term returns. */

char *term_strerror(int errnum) {
  char *s;
  s=x__strerror(errnum);
  if (! use_term || errnum != ~0) return s;
  if (remote_connect < 0) {
    remote_connect=use_term_command(PUBLIC);
    store_sockinfo(-1,-1,NULL);
  }
  if (term_error[0]) return term_error;
  return s;
}    

/* This handles printing termnet errors after term returns. */

void term_perror(char *message) {
  if (! use_term || errno != ~0) {
    x__perror(message);
    return;
  }
  if (remote_connect < 0) {
    remote_connect=use_term_command(PUBLIC);
    store_sockinfo(-1,-1,NULL);
  }
  errno = ~0;
  if (! term_error[0]) {
    x__perror(message);
    return;
  }
  fprintf(stderr,"%s: %s\n",message,term_error);
}

/* This is a lookup routine so I can return the remote peer name instead
 * of the local name.
 */

int term_getpeername(int S, struct sockaddr *name, int *namelen){
  int i,j;

  if(! use_term) return x__getpeername(S,name,namelen);

  if(S<0) return -1;

  if (remote_connect < 0) {
    remote_connect=use_term_command(PUBLIC);
    store_sockinfo(-1,-1,NULL);
  }

  if (term_debug & 32768) {
    tmp_errno = errno;
    sprintf(term_error,"test term_getpeername");
    print_err(32768);
  }

  for(i=0;i<MAX_CLIENTS;i++)
    if(S==sockinfo[i].sock) break;

  if (i < MAX_CLIENTS) {
    if(namelen != NULL)
      *namelen=(*namelen>sizeof(struct sockaddr_in)) ? sizeof(struct sockaddr_in) : *namelen;
    /* Only open the connection once for a client! */
    if (Sock < 0)
      if((Sock=socket_connect_server(-1,term_server)) < 0) {
      tmp_errno = ~0;
      sprintf(term_error, "Term 4: %s", command_result);
      print_err(1);
      return x__getpeername(S,name,namelen);
    }
    if((j=send_command(Sock, C_GETPEERNAME, 0, "%d", sockinfo[i].client)) < 0) {
      tmp_errno = ~0;
      sprintf(term_error, "C_GETPEERNAME: %s", command_result);
      print_err(1);
      if (j < -1) term_do_exit();
      return x__getpeername(S,name,namelen);
    }

    if(name != NULL && namelen != NULL){
      *namelen = ( *namelen < sizeof(struct sockaddr) ) ? *namelen : sizeof(struct sockaddr);
      memcpy(name,str_to_sockaddr(command_result,htonl(term_remoteaddr)),
        *namelen);
      remote_connect=1;
      return 0; 
    }
  }
  if (use_term < 1)
    return x__getpeername(S,name,namelen);
  errno = ENOTCONN;
  return -1;
}    


/* This is a lookup routine so I can return the remote socket name instead
 * of the local name.
 */

int term_getsockname(int S, struct sockaddr *name, int *namelen){
  int i,j = -1, k;
  struct hostent *hp;
  struct sockaddr_in *name_in;

  if(! use_term) return x__getsockname(S,name,namelen);

  if (remote_connect < 0) {
    remote_connect=use_term_command(PUBLIC);
    store_sockinfo(-1,-1,NULL);
  }

  if (term_debug & 32768) {
    tmp_errno = errno;
    sprintf(term_error,"test term_getsockname");
    print_err(32768);
  }

  if (use_term < 1) {
    if ((j=x__getsockname(S,name,namelen)) < 0) return j;
  }
  if (S < 0) {
    errno = EBADF;
    return j;
  }

  for(i=0;i<MAX_CLIENTS;i++)
    if (S == sockinfo[i].sock) break;

  name_in = (struct sockaddr_in *) name;
  if (use_term < 1 && i == MAX_CLIENTS) {
    if (name_in->sin_family == AF_INET 
        && name_in->sin_addr.s_addr == INADDR_ANY ) { 
      char hostname[259];

#if defined(SYSV) && !defined(DYNIXPTX)
      { struct utsname unam;
        uname(&unam);
        strcpy(hostname, unam.nodename); }
#else
      x__gethostname (hostname, sizeof(hostname));
#endif
      if (isdigit(hostname[0])) {
        name_in->sin_addr.s_addr = inet_addr(hostname);
      }else if ((hp=host_lookup(hostname,0,AF_INET,0,NULL))) {
        memcpy(&name_in->sin_addr, hp->h_addr, hp->h_length);
      }
    }
    return j;
  }
  if (i == MAX_CLIENTS) return -1;

  if (sockinfo[i].sockport) {
    if ((! name) || (! namelen)) return 0;
    if (*namelen >= sizeof(struct sockaddr_in)) {
      name_in->sin_family = AF_INET;
      name_in->sin_addr.s_addr = htonl(term_remoteaddr);
      name_in->sin_port = htons(sockinfo[i].sockport);
    }
  }

  /* Only open the connection once for a client! */
  if (Sock < 0)
    if ((Sock=socket_connect_server(-1,term_server)) < 0) {
    tmp_errno = ~0;
    sprintf(term_error, "Term 1: %s", command_result);
    print_err(1);
    return j;
  }
  if ((k=send_command(Sock, C_GETSOCKNAME, 0, "%d", sockinfo[i].client)) < 0) {
    tmp_errno = ~0;
    sprintf(term_error, "C_GETSOCKNAME: %d %d : %s", S, sockinfo[i].client, command_result);
    print_err(1);
    if (k < -1) term_do_exit();
    return j;
  }

    
  *namelen = (*namelen>sizeof(struct sockaddr)) ? 
    sizeof(struct sockaddr) : *namelen;
  memcpy(name,str_to_sockaddr(command_result,htonl(term_remoteaddr)),
    *namelen);
  if(*namelen >= sizeof(struct sockaddr_in)) {
    sockinfo[i].sockport = ntohs(name_in->sin_port);
  }

  remote_connect=1;
  return j;
}    


/* For term gethostbyname() is executed on the remote machine when
 * the connection is established.  So here, I just list the hostname
 * in a table.  I list the index as 0.0.0.index.  Since I doubt that
 * 0.0.0.index is used by any host in the world as an IP # this should
 * allow routines to work with and without term.  (NOTE: This limits
 * use to 255 term hosts listed in each program.  If you access more
 * than that, the original hostnames will be replaced.)
 */

#ifndef linux
struct hostent *term_gethostbyname(char *host){
#else
struct hostent *term_gethostbyname(const char *host){
#endif
  int i;
  static char hostname[259];
  static struct hostent *hp;

  if(! use_term) return host_lookup((char *)host,0,AF_INET,0,NULL);

  if (remote_connect < 0) {
    remote_connect=use_term_command(PUBLIC);
    store_sockinfo(-1,-1,NULL);
  }

  if (term_debug & 32768) {
    tmp_errno = errno;
    sprintf(term_error,"test term_gethostbyname");
    print_err(32768);
  }

  if (host != NULL)
    if (use_term < 1 && (hp=host_lookup((char *)host,0,AF_INET,0,NULL)))
      return hp;


  /* Copy the passed-in name, to make sure it doesn't get munged */
  if (host != NULL){
    if ((hp=host_lookup((char *)host,0,AF_INET,1,NULL))) return hp;
    if(!strcmp(host,"127.0.0.254"))
      hostname[0] = '\0';
    else
      strncpy(hostname,host,256);
  }else
    hostname[0] = '\0';
  
  /* Only open the connection once for a client! */
  if (Sock < 0)
    if ((Sock = socket_connect_server(-1,term_server)) < 0) {
    tmp_errno = ~0;
    sprintf(term_error, "Term: %s %s", host ? host : "NULL", command_result);
    print_err(1);
    remote_connect = 0;
    return NULL;
  }
  if ((i=send_command(Sock, C_GETHOSTNAME, 0, "%s", hostname)) < 0) {
    tmp_errno = ~0;
    sprintf(term_error, "C_GETHOSTNAME: %s", command_result);
    print_err(1);
    if (i < -1) term_do_exit();
    return NULL;
  }
  if (host) 
    return host_lookup((char *)host,0,AF_INET,1,str_to_hostent(command_result));
  else
    return str_to_hostent(command_result);
}

#ifndef linux
struct hostent *term_gethostbyaddr(char *addr, int len, int type){
#else
struct hostent *term_gethostbyaddr(const char *addr, int len, int type){
#endif
  struct hostent *hs;
  static char host[16];

  if(! use_term || type != AF_INET) {
    return host_lookup((char *)addr,len,type,0,NULL);
  }

  if (remote_connect < 0) {
    remote_connect=use_term_command(PUBLIC);
    store_sockinfo(-1,-1,NULL);
  }

  if (term_debug & 32768) {
    tmp_errno = errno;
    sprintf(term_error,"test term_gethostbyaddr");
    print_err(32768);
  }

  if(use_term < 1 && (hs=host_lookup((char *)addr, len, AF_INET,0,NULL)))
    return hs;

  {
    unsigned long int *j;
    struct in_addr tmp;
    memset(&tmp,0,sizeof(tmp));
    j = (unsigned long *)addr; 
    tmp.s_addr = *j;
    strcpy(host,inet_ntoa(tmp));
  };

  return term_gethostbyname(host);
}

int term_shutdown(int fd,int how){

  if(! use_term) return x__shutdown(fd,how);

  if (remote_connect < 0) {
    remote_connect=use_term_command(PUBLIC);
    store_sockinfo(-1,-1,NULL);
  }

  if (term_debug & 32768) {
    tmp_errno = errno;
    sprintf(term_error,"test term_shutdown");
    print_err(32768);
  }

  close_fd(fd,how);
  return x__shutdown(fd,how);
}

int term_close(int fd){
  if (remote_connect < 0) {
    remote_connect=use_term_command(PUBLIC);
    store_sockinfo(-1,-1,NULL);
  }

  if (term_debug & 32768) {
    tmp_errno = errno;
    sprintf(term_error,"test term_close");
    print_err(32768);
  }

  close_fd(fd,2);
  return x__close(fd);
}   


/* For term connections, this is needed because the program may either
 * listen() or connect() after a bind().
 */

int term_listen(int fd, int backlog){
  int i,k;

  if(! use_term) return x__listen(fd,backlog);

  if (remote_connect < 0) {
    remote_connect=use_term_command(PUBLIC);
    store_sockinfo(-1,-1,NULL);
  }

  if (term_debug & 32768) {
    tmp_errno = errno;
    sprintf(term_error,"test term_listen");
    print_err(32768);
  }

  if(fd < 0) return x__listen(fd,backlog);

  for(k=0;k<MAX_CLIENTS;k++)
    if(fd==sockinfo[k].sock) break;

  if (k == MAX_CLIENTS) {
    if (use_term < 1) return x__listen(fd,backlog);
    errno = EOPNOTSUPP;
    return -1;
  }

  if (Sock < 0) 
    if ((Sock=socket_connect_server(-1,term_server)) < 0) {
    tmp_errno = ~0;
    sprintf(term_error, "Term 9: %s", command_result);
    print_err(1);
    return -1;
  }
  if ((i=send_command(Sock,C_LISTEN,0,"%d %d",sockinfo[k].client,backlog))< 0) {
    tmp_errno = ~0;
    sprintf(term_error,"Term: C_LISTEN '%d %d': %s", sockinfo[k].client, backlog,
      command_result);
    print_err(1);
    if (i < -1) term_do_exit();
    return -1;
  }
  return 0;
}


/* OK now lets try redirecting socket binding.  I'm at a lost in decided
 * how to make this work for both term and non-term connections.  Here is
 * my current algorithm to redirect AF_INET ports:
 *
 * - If a non-zero port # the service must be listed as both "tcp"
 *   and "term" in /etc/service.
 * - If it is a zero port #, then the port is only redirected if the same
 *   program has already opened a term connection to a remote host.
 *
 * For UDP this is even simpler.  If we are connected to term, we use term.
 */

int term_bind(int S, struct sockaddr *my_addr, int addrlen) {
  int s, i, iport, k;
  struct sockaddr_in *name_in;

  if(! use_term) return x__bind(S,my_addr,addrlen);

  if (remote_connect < 0) {
    remote_connect=use_term_command(PUBLIC);
    store_sockinfo(-1,-1,NULL);
  }

  if (term_debug & 32768) {
    tmp_errno = errno;
    sprintf(term_error,"test term_bind");
    print_err(32768);
  }

  for(k=0;k<MAX_CLIENTS;k++)
    if (S == sockinfo[k].sock) break;

  if (k == MAX_CLIENTS) for(i=0;i<pending_count;i++) if(udp_pending[i] == S) {
    if ((k=term_udpsocket(S)) < 0)
      if(use_term < 1) {
        return x__bind(S, my_addr, addrlen);
      }else {
        errno = EBADF;
        return -1;
      }
    break;
  }

  if (k != MAX_CLIENTS && sockinfo[k].udp_addr != NULL) {
    struct sockaddr addr;
    int j = -1, len = sizeof(addr), tmperrno = 0;

    name_in = (struct sockaddr_in *) my_addr;

    if (name_in->sin_addr.s_addr != inet_addr("127.0.0.254") 
	&& name_in->sin_addr.s_addr != htonl(term_remoteaddr)) {         
      j = x__bind(S, my_addr, addrlen);	/* Try the address requested */
      tmperrno = errno;
      errno = tmperrno;
      if (j == -1  && tmperrno != EBADF) return j;
    }

	/* Next check if this socket is also usable by term */

    if (k == MAX_CLIENTS) return j;  /* Since it is not available just return */
 
	/* Now we redirect a remote port, so that the local socket is */
	/* bound both locally and remotely. */

    iport=ntohs(name_in->sin_port);

    if ((s=Sock) < 0) {
      tmp_errno = ~0;
      sprintf(term_error,"Term: Not connect to server");
      print_err(1);
      errno = tmperrno;
      return j;
    }

    if ((i=send_command(s, C_UBIND, 0, "%d %d",
          sockinfo[k].client, iport)) < 0) {
      tmp_errno = ~0;
      sprintf(term_error,"Term: C_UBIND failed: %s %d %d",command_result,
        sockinfo[k].client, iport);
      print_err(1);
    }else if ((i=x__getsockname(S,&addr,&len)) < 0) {
      x__perror("getsockname()");
    }else if ((i=send_command(s, C_UDPSET, 1, "%d :%s",
      sockinfo[k].client, sockaddr_to_str(&addr,0))) < 0) {
      tmp_errno = ~0;
      sprintf(term_error, "Term: C_UDPSET failed: %s",command_result);
      print_err(1);
    }

    if (i < 0) {
      errno = EACCES;
      return -1;
    }else return 0;
  }else {  
    int iport,client = -1;
    struct servent *service=NULL;

    name_in=(struct sockaddr_in *) my_addr;
    if (S < 0 || name_in->sin_family != AF_INET
        || (!remote_connect && !name_in->sin_port
            && name_in->sin_addr.s_addr != htonl(term_remoteaddr)
            && name_in->sin_addr.s_addr != inet_addr("127.0.0.254")))
      return x__bind(S,my_addr,addrlen);

    if ((iport=ntohs(name_in->sin_port))) {
      service=getservbyport(htons(iport),"tcp");
      if (service) 
        if ((service=getservbyname(service->s_name,"term")))
          iport=ntohs(service->s_port);
      if (!service && name_in->sin_addr.s_addr != inet_addr("127.0.0.254")) {
        if (!remote_connect
           || ( geteuid() && term_remoteuser && iport < 1024)
           || ((service=getservbyport(htons(iport),"noterm"))) ){
          return x__bind(S,my_addr,addrlen);
        }
      } 
    }

    if (Sock < 0) 
      if ((Sock=socket_connect_server(-1,term_server)) < 0) {
      return x__bind(S,my_addr,addrlen);
    }

    if ((s = duplicate_connect(S,Sock)) < 0) {
      tmp_errno = ~0;
      sprintf(term_error, "Term: %s", command_result);
      print_err(1);
      i = -1;
    }else if ((i=send_command(s, C_STATS, 1, "%d", -6)) < 0) {
      tmp_errno = ~0;
      sprintf(term_error, "Term: C_STATS -6 failed: %s",command_result);
      print_err(1);
    }else{
      client=atoi(command_result);
      if ((i=send_command(s,C_BIND,0,"%d %d",iport,0)) < 0) { 
        tmp_errno = ~0;
        sprintf(term_error,"Term: C_BIND failed: %s",command_result);
        print_err(1);
      }else {
        int port;
        port = atoi(command_result);
        if ((k=store_sockinfo(s, client, NULL)) < MAX_CLIENTS) {
          sockinfo[k].sockport = port;
          remote_connect=1;
        }
      }
    }

    if(i<0){
      if(s >= 0) x__close(s);
      return i;
    }
    return 0;
  }
}


/* Finally for term connections, accept simply continues where
 * bind left off.
 */

int term_accept(int pending, struct sockaddr *addr, int *addrlen){
  int j=MAX_CLIENTS,s = -1,i = -1;
  char port[34];

  if(! use_term) return x__accept(pending,addr,addrlen);

  if (remote_connect < 0) {
    remote_connect=use_term_command(PUBLIC);
    store_sockinfo(-1,-1,NULL);
  }

  if (term_debug & 32768) {
    tmp_errno = errno;
    sprintf(term_error,"test term_accept");
    print_err(32768);
  }

  if(pending>=0)
    for(j=0;j<MAX_CLIENTS;j++)
      if(pending == sockinfo[j].sock) break;

  if (j == MAX_CLIENTS || addr == NULL || addrlen == NULL )
    return x__accept(pending, addr, addrlen);

  if (read(pending,port,10) < 0) {
    tmp_errno = errno;
    sprintf(term_error,"Term: read port: %s",x__strerror(j));
    print_err(1);
    return -1;
  }
 
  if (Sock < 0) 
    if ((Sock=socket_connect_server(-1,term_server)) < 0) {
    return -1;
  }

  if (sockinfo[j].client != atoi(port)) {
    tmp_errno = ~0;
    sprintf(term_error,"Term: Accept mis-match %d != %s",
      sockinfo[j].client,port);
    print_err(1);
    i = -1;
  }else if ((i = s = duplicate_connect(-1,Sock)) < 0) {
    tmp_errno = ~0;
    sprintf(term_error, "Term 7: %s", command_result);
    print_err(1);
  }else if ((i=send_command(s, C_STATS, 1, "%d", -6)) < 0) {
    tmp_errno = ~0;
    sprintf(term_error, "Term: C_STATS -6 failed: %s",command_result);
    print_err(1);
  }else{
    store_sockinfo(s, atoi(command_result),NULL);
    if((i=send_command(s, C_ACCEPT, 0, "%d", sockinfo[j].client))<0){
      tmp_errno = ~0;
      sprintf(term_error,"Term: C_ACCEPT: %d %s",sockinfo[j].client,command_result);
      print_err(1);
    }else{
      *addrlen = (*addrlen>sizeof(struct sockaddr)) ? sizeof(struct sockaddr) : *addrlen;
      memcpy(addr,str_to_sockaddr(command_result,htonl(term_remoteaddr)),
        *addrlen); 
      send_command(s, C_DUMB, 1, 0);
      return s;
    }
  }
  if (s >= 0) {
    term_close(s);
  }
  return -1; 
}


int term_connect(int S, struct sockaddr *addr, int addrlen){
  int i = -1, k, s, client = -1, old=0;
  struct sockaddr_in *addr_in;
  char host[59],*ihost;
  unsigned long j;
 
  if(! use_term) return x__connect(S,addr,addrlen);

  if (remote_connect < 0) {
    remote_connect=use_term_command(PUBLIC);
    store_sockinfo(-1,-1,NULL);
  }

  if (term_debug & 32768) {
    tmp_errno = errno;
    sprintf(term_error,"test term_connect");
    print_err(32768);
  }

  for(k=0;k<MAX_CLIENTS;k++)
    if (S == sockinfo[k].sock) break;



  addr_in=(struct sockaddr_in *) addr;

  if(addr_in->sin_family != AF_INET) {
    close_fd(S,2);
    return x__connect(S,addr,addrlen);
  }

  j = addr_in->sin_addr.s_addr;
  ihost=(char *) &j;

  if (j == INADDR_ANY || j == inet_addr("127.0.0.254")) {
    ihost = "\0";
  }else {
    if (host_lookup(ihost,sizeof(unsigned long),addr_in->sin_family,0,NULL)) {
      close_fd(S,2);
      return x__connect(S,addr,addrlen);
    } 
#define UC(a) (unsigned char) ((a) & 0xff)
    sprintf(host,"%u.%u.%u.%u",UC(ihost[0]),UC(ihost[1]),UC(ihost[2]),UC(ihost[3]));
    ihost=host;
  }

  if (k == MAX_CLIENTS) for(i=0;i<pending_count;i++) if(udp_pending[i] == S) {
    if ((k=term_udpsocket(S)) < 0) 
      return x__connect(S, addr, addrlen);
    break;
  }

  if (k < MAX_CLIENTS && sockinfo[k].udp_addr != NULL) {
    client = sockinfo[k].client;
    x__connect(S, sockinfo[k].udp_addr, sizeof(sockinfo[k].udp_addr));
    if ((s = Sock) < 0) {
      if(term_debug >= 0) {
        tmp_errno = ~0;
        sprintf(term_error,"Term: Not connected to term");
        print_err(1);
      }
    }else if ((i=send_command(s, C_UDPSET, 0, "%d :%s",
        client, sockaddr_to_str(addr,0))) < 0) {
      tmp_errno = ~0;
      sprintf(term_error, "Term: C_UDPSET failed: %s",command_result);
      if (i < -1) {
        x__close(s);
        s = -1;
      }
      print_err(1);
    }
  }else {
    i = 0;
    if (k == MAX_CLIENTS) {
      if (Sock < 0) 
        if((Sock=socket_connect_server(-1,term_server)) < 0) 
          i = -1;
      if (i >= 0) if ((i = duplicate_connect(S,Sock)) < 0) {
        tmp_errno = ~0;
        sprintf(term_error, "Term 8: %s", command_result);
        print_err(1);
      }else if ((i=send_command(S, C_STATS, 1, "%d", -6)) < 0) {
        tmp_errno = ~0;
        sprintf(term_error, "Term: C_STATS -6 failed: %s",command_result);
        print_err(1);
      }else 
        client = atoi(command_result);
    } else{
      client = sockinfo[k].client;
      old = 1;
    }
    if (i >= 0) {
      if (! *ihost) {
        if ((i=send_command(S,C_PORT,0,"%d %d",ntohs(addr_in->sin_port),
            old))< 0) {
          tmp_errno = ~0;
          sprintf(term_error,"Term: C_PORT '%d' : %s",ntohs(addr_in->sin_port),
            command_result);
          print_err(1);
        }
      }else {
#if 1
        if (isdigit(ihost[0]) && inet_addr(ihost) == htonl(term_remoteaddr) && ntohs(addr_in->sin_port) == 1080) {
          addr_in->sin_port = 80;
          fprintf(stderr,"Redirecting %s:1080 to %s:80\n",ihost,ihost);
        }else if (ntohs(addr_in->sin_port) == 1080) {
          fprintf(stderr,"Not Redirecting %s:1080 to %lx:80\n",ihost,term_remoteaddr);
        }
#endif
        if ((i=send_command(S,C_PORT,0,"%s:%d",ihost,ntohs(addr_in->sin_port),
            old))< 0) {
          tmp_errno = ~0;
          sprintf(term_error,"Term: C_PORT '%s:%d' : %s",ihost,ntohs(addr_in->sin_port),
            command_result);
          print_err(1);
        }
      }
    }
    if (i < 0) {
      term_close(S);
      return -1;
    }else if (i >= 0) {
      send_command(S, C_DUMB, 1, 0);
      store_sockinfo(S, client, NULL);
    }
  }
  return ((i<0) ? x__connect(S,addr,addrlen) : 0);
}


/* term_gethostname() - get hostname of remote system, not local
 * ytalk sends gethostbyname of the local hostname to talkd,
 * so we give it the remote hostname
 */

int term_gethostname(char *name, size_t len) {
  int len2;
  struct hostent *hs=NULL;

  if(! use_term) return x__gethostname(name, len);

  if (remote_connect < 0) {
    remote_connect=use_term_command(PUBLIC);
    store_sockinfo(-1,-1,NULL);
  }

  if (term_debug & 32768) {
    tmp_errno = errno;
    sprintf(term_error,"test term_gethostname");
    print_err(32768);
  }

  if (! remote_connect) return x__gethostname(name, len);

  if ( remote_term_version < 20054) {
    if (!(hs=term_gethostbyname(NULL))) {
       return x__gethostname(name, len);
    }
    hs = str_to_hostent(command_result);
    len2 = strlen(hs->h_name)+1;
  }else {
    len2 = strlen(term_remotehost) + 1;
  }

  if (len2 > len) {
    errno = EINVAL;
    return -1;
  }
  
  if (hs) 
    strncpy(name,hs->h_name,len2);
  else
    strncpy(name,term_remotehost,len2);

  return 0;
}


/* term_socket()
 * Basically, we create a new client, C_USOCK both ends, C_UBIND the local end
 * (to listen to the local program's sendto's) and C_GETPEERNAME the local UDP
 * socket so term_sendto sends to the right place
 */

int term_socket( int domain, int type, int protocol) {
  int s,i;

  if(! use_term) return x__socket(domain, type, protocol);

  if (remote_connect < 0) {
    remote_connect=use_term_command(PUBLIC);
    store_sockinfo(-1,-1,NULL);
  }

  if (term_debug & 32768) {
    tmp_errno = errno;
    sprintf(term_error,"test term_socket");
    print_err(32768);
  }

	/* If the same file descriptor was closed with fclose, then term */
	/* didn't know about it, so we take care of that now. */

  if ((s = x__socket(domain,type,protocol)) < 0) return s;

  for(i=0;i<MAX_CLIENTS;i++)
    if (s == sockinfo[i].sock) close_fd(s,2);

	/* We can return if this isn't a udp socket. */

  if (domain != AF_INET || type != SOCK_DGRAM || !remote_connect)
    return s;

  for (i=0;i<pending_count;i++)
    if(udp_pending[i] == s || udp_pending[i] == -1) break;

  if (i == MAX_CLIENTS) return s;
  udp_pending[i] = s;

  if (i == pending_count) pending_count++;
  return s;
}

#ifndef linux
int term_sendto(int s, void *msg, int len, unsigned int flags, 
    struct sockaddr *to, int tolen) {
#else
int term_sendto(int s, const void *msg, int len, unsigned int flags, 
    const struct sockaddr *to, int tolen) {
#endif

  int i,k;
  static un_char *buff=NULL;
  static int alloced=0;
  unsigned long j;
  char *ihost=NULL;
  struct sockaddr_in *to_addr = (struct sockaddr_in *)to;

  if (! use_term) return x__sendto(s, (char *)msg, len, flags,
    (struct sockaddr *)to, tolen);

  if (remote_connect < 0) {
    remote_connect=use_term_command(PUBLIC);
    store_sockinfo(-1,-1,NULL);
  }


  if (term_debug & 32768) {
    tmp_errno = errno;
    sprintf(term_error,"test term_sendto");
    print_err(32768);
  }

  for(k=0;k<MAX_CLIENTS;k++)
    if (s == sockinfo[k].sock && sockinfo[k].udp_addr != NULL) break;


  if (to && tolen) {
    j = to_addr->sin_addr.s_addr;
    ihost=(char *) &j;
  }

  if (k == MAX_CLIENTS) {
    for(i=0;i<pending_count;i++) if(udp_pending[i] == s) {
      if (ihost && host_lookup(ihost,sizeof(long unsigned),
        to_addr->sin_family,0,NULL)) break;
      ihost = NULL;
      k=term_udpsocket(s);
      break;
    }
    if (k == MAX_CLIENTS || k < 0) {
      if (to && tolen) 
        return x__sendto(s, (char *)msg, len, flags,
          (struct sockaddr *)to, tolen);
      else 
        return x__send(s, (char *)msg, len, flags);
    }
  }

  if (ihost && host_lookup(ihost,sizeof(long unsigned),AF_INET,0,NULL))
    return x__sendto(s, (char *)msg, len, flags,
      (struct sockaddr *)to, tolen);
    

  if (alloced < (len+HEADER_SIZE)) {	/* malloc it */
    if (! buff) 
      buff = (un_char *)malloc(sizeof(char)*(len+HEADER_SIZE+1)); /* +1 just to make sure... */
    else
      buff = (un_char *)realloc(buff,sizeof(char)*(len+HEADER_SIZE+1));
    alloced = len + HEADER_SIZE + 1;
  }

  memset(buff, 0, alloced);

  {
    unsigned long int lhost;
    unsigned short int lport;
    if (to_addr != NULL && tolen) if (to_addr->sin_family == AF_INET) {
      lhost = ntohl(to_addr->sin_addr.s_addr);
      lport = ntohs(to_addr->sin_port);
      buff[0] = (lhost>>24)&255;
      buff[1] = (lhost>>16)&255;
      buff[2] = (lhost>>8)&255;
      buff[3] = lhost&255;
      buff[4] = (lport>>8)&255;
      buff[5] = lport&255;
    }
    to_addr = (struct sockaddr_in *)sockinfo[k].udp_addr;
  }
  memcpy(buff+HEADER_SIZE,msg,len);

  i = x__sendto(s, buff, len+HEADER_SIZE, flags, sockinfo[k].udp_addr,
	sizeof(struct sockaddr));
  
  return (i - HEADER_SIZE);
}

/* This simulates rcmd().  locuser is ignored!  Also term_error
 * is piped with stdout.  For term_error I send a closed pipe descriptor.  This
 * seems to keep "rsh" happy.  (Otherwise you'll create lots of zombies.)
 * So far I've only defined "shell", "cmd", "login", and "who".
 *
 * Programs that use rcmd() should be SUID root, so I take extra security
 * measures here.
 */

#ifndef linux
int term_rcmd(char **ahost,unsigned short inport, char *locuser,
    char *remuser, char *cmd, int *fd2p){
#else
int term_rcmd(char **ahost,unsigned short inport, const char *locuser,
    const char *remuser, const char *cmd, int *fd2p){
#endif
  int i = 0,s = -1;
  char *rcommand, *term;
  struct servent *sp;

  if(! use_term) return x__rcmd(ahost,inport,(char *)locuser,
    (char *)remuser,(char *)cmd,fd2p);

  if (remote_connect < 0) {
    remote_connect=use_term_command(PUBLIC);
    store_sockinfo(-1,-1,NULL);
  }

  if (term_debug & 32768) {
    tmp_errno = errno;
    sprintf(term_error,"test term_rcmd");
    print_err(32768);
  }

/* If the host is listed by gethostbyname(), don't use term */

  if (! geteuid() && *ahost)
    if (strcmp(*ahost,"remotehost") && strcmp(*ahost,"127.0.0.254"))
      if (use_term < 1 && host_lookup(*ahost,0,AF_INET,0,NULL))
        return x__rcmd(ahost,inport,(char *)locuser,
          (char *)remuser,(char *)cmd,fd2p);

/* These values will need to be passed to the rd_exec function */

  if (fd2p!=NULL)
    *fd2p = -1;

  sp = getservbyport(inport,"tcp");
  if(!strcmp(sp->s_name,"shell")||!strcmp(sp->s_name,"cmd")){
    use_term_command(PRIVILEGED);
    setuid(getuid());
    rcommand=(char *)cmd;
  }else if(!strcmp(sp->s_name,"login")){
    use_term_command(PRIVILEGED);
    setuid(getuid());
    rcommand=NULL;
  }else if(!strcmp(sp->s_name,"who")){
    rcommand="rwho";
  }else{
    tmp_errno = ~0;
    sprintf(term_error,"%s is not understood by term yet.",sp->s_name);
    print_err(1);
    return -1;
  }

  s = x__socket(AF_INET, SOCK_STREAM, 0);
  if ((s = socket_connect_server(s,term_server)) <0) {
    tmp_errno = ~0;
    sprintf(term_error, "Term 6: %s", command_result);
    print_err(1);
    i = -1;
  }else{
	/* Convert the "127.0.0.254 remotehost" alias */
    if (! *ahost || ! strcmp(*ahost,"remotehost")
         || !strcmp(*ahost,"127.0.0.254") ) {
      struct hostent *hs;
      if (remote_term_version < 11714 ) {
        *ahost = "127.0.0.1";
      }else if (send_command(s, C_GETHOSTNAME, 0, "%s","\0") < 0) {
        tmp_errno = ~0;
        sprintf(term_error, "C_GETHOSTNAME failed: %s", command_result);
        print_err(1);
        *ahost = "127.0.0.1";
      }else if ((hs = str_to_hostent(command_result))){
        *ahost = (char *) hs->h_name;
      };
	/* If after all that work we still have "remotehost", just change it */
	/* We don't do this to begin with, because not everyone has listed */
	/* "localhost" in their .rhosts file & hosts.equiv file. */
      if ( !strcmp(*ahost,"127.0.0.254") 
          || !strcmp(*ahost,"remotehost") ) 
        *ahost = "127.0.0.1";
    }
    term = getenv("TERM");
    if ( i >= 0 ) {
      if ((i=send_command(s, C_STATS, 1, "%d", -6)) < 0) {
        tmp_errno = ~0;
        sprintf(term_error, "Term: C_STATS -6 failed: %s",command_result);
        print_err(1);
      }else {
        store_sockinfo(s, atoi(command_result),NULL);
        if (! rcommand) {
          if (! term) 
            i=send_command(s,C_EXEC,0,"rlogin %s -l %s",
              *ahost,remuser);
          else
            i=send_command(s,C_EXEC,0,"-DTERM=%s%crlogin %s -l %s",
              term,'\377',*ahost,remuser);
        }else {
          if (! term) 
            i=send_command(s,C_EXEC,0,"rsh %s -l %s %s",
              *ahost,remuser,rcommand);
          else
            i=send_command(s,C_EXEC,0,"-DTERM=%s%crsh %s -l %s %s",
              term,'\377',*ahost,remuser,rcommand);
        };
        if (i < 0) {
          tmp_errno = ~0;
          sprintf(term_error,"Term: C_EXEC %s",command_result);
          print_err(1);
          term_close(s);
        }else {
          send_command(s, C_DUMB, 1, 0);
        }
      }
    }
  }

  if (i<0) {
    if (s>=0) {
      x__close(s);
      s = -1;
    }
  }else if (fd2p!=NULL) {
    int stat_pipe[2];
    if((pipe(stat_pipe))>=0){ /* open a pipe for passing the connect status */
        *fd2p=stat_pipe[0];
        x__close(stat_pipe[1]);
    };
  };
  return s;
}

/* Replacing exec calls is very tricky.  But normally fork() is called before */
/* exec, so this should patch most of the security problems. */

int term_fork(void) {
  if(! use_term) return x__fork();

  if (remote_connect < 0) {
    remote_connect=use_term_command(PUBLIC);
    store_sockinfo(-1,-1,NULL);
  }

  if (term_debug & 32768) {
    tmp_errno = errno;
    sprintf(term_error,"test term_fork");
    print_err(32768);
  }

	/* This is to prevent problems with race conditions... */
	/* The cost is we will be using an extra client after */
	/* the next exec().   Fortunately this isn't neccissary */
	/* with term_vfork(). */

  if (Sock < 0) {
    return x__fork();
  }else {
    int s,pid;
    s=duplicate_connect(-1,Sock);
    pid = x__fork();
    if (pid) {
      if (s >= 0) close(s);
      return pid;
    }else {
      Sock = s;
    }
  }
  return 0;
}

#ifndef linux
int term_chroot(char *path) {
#else
int term_chroot(const char *path) {
#endif

  if(! use_term) return x__chroot((char *)path);

  if (remote_connect < 0) {
    remote_connect=use_term_command(PUBLIC);
    store_sockinfo(-1,-1,NULL);
  }

  if (term_debug & 32768) {
    tmp_errno = errno;
    sprintf(term_error,"test term_chroot");
    print_err(32768);
  }

  if (Sock < 0) 
    if ((Sock=socket_connect_server(-1,term_server)) < 0) {
    tmp_errno = ~0;
    sprintf(term_error, "Term A: %s", command_result);
    print_err(1);
  }
  return x__chroot((char *)path);
}

int term_vfork(void) {

  if(! use_term) return x__vfork();

  if (remote_connect < 0) {
    remote_connect=use_term_command(PUBLIC);
    store_sockinfo(-1,-1,NULL);
  }

  if (term_debug & 32768) {
    tmp_errno = errno;
    sprintf(term_error,"test term_vfork");
    print_err(32768);
  }

  return x__vfork();
}

/* term_recvfrom()
 *
 * recvfrom() replacement which correctly assigns the from-address
 * to the remote site instead of localhost, and strip header. -danjo
 */

#ifndef linux
int term_recvfrom( int s, char *buff, int len, unsigned int flags,
		  struct sockaddr *from, int *fromlen)
#else
int term_recvfrom( int s, void *buff, int len, unsigned int flags,
		  struct sockaddr *from, int *fromlen)
#endif
{
  static un_char *mybuff = NULL;
  static int alloced;
  int avail, myfromlen, k, h;
  struct sockaddr_in myfrom;

  if(! use_term) return x__recvfrom(s,(char *)buff,len,flags,from,fromlen);

  if (remote_connect < 0) {
    remote_connect=use_term_command(PUBLIC);
    store_sockinfo(-1,-1,NULL);
  }

  if (term_debug & 32768) {
    tmp_errno = errno;
    sprintf(term_error,"test term_connect");
    print_err(32768);
  }

  for(k=0;k<MAX_CLIENTS;k++)
    if (s == sockinfo[k].sock && sockinfo[k].udp_addr != NULL) break;
  if (k == MAX_CLIENTS) return x__recvfrom(s,(char *)buff,len,flags,from,fromlen);

  avail = len + HEADER_SIZE;

#ifdef FIONREAD
  ioctl(s,FIONREAD,&avail);
  if (avail > len + HEADER_SIZE) 
    avail = len + HEADER_SIZE;
#endif

  if( alloced < avail) { /* not enough room, let's alloc more */
    if (! mybuff) {
       mybuff = (un_char *)malloc(sizeof(char)*(avail+1)); /* +1 needed? */
       alloced = avail+1;
    }else {
       mybuff = (un_char *)realloc(mybuff,sizeof(char)*(avail+1));
       alloced = avail+1;
    }
  }

  h = HEADER_SIZE;

  avail=x__recvfrom(s,mybuff,avail,flags,(struct sockaddr *)&myfrom,
      &myfromlen);
  k=errno;

  if (avail < h)
    h=0;
  else if (fromlen != NULL && from != NULL && sockinfo[k].udp_addr != NULL &&
     myfromlen) {
    struct sockaddr_in *udp_in;
    udp_in = (struct sockaddr_in *) sockinfo[k].udp_addr;
    if (myfrom.sin_family != udp_in->sin_family
      && myfrom.sin_addr.s_addr != udp_in->sin_addr.s_addr
      && myfrom.sin_port != udp_in->sin_port) h=0;
  }

  if (! h) {
    if (avail >= 0)
      memcpy((char *)buff, mybuff,avail*sizeof(char));
  }else {
    avail -= h;
    if (fromlen != NULL && from != NULL) {
      myfrom.sin_family = AF_INET;
      myfrom.sin_addr.s_addr =
        htonl((mybuff[0]<<24)+(mybuff[1]<<16)+(mybuff[2]<<8)+mybuff[3]);
      myfrom.sin_port = htons((mybuff[4]<<8)+mybuff[5]);

      if (myfrom.sin_addr.s_addr == inet_addr("127.0.0.1") ||
          myfrom.sin_addr.s_addr == inet_addr("127.0.0.254") ||
          myfrom.sin_addr.s_addr == INADDR_ANY ) 
        myfrom.sin_addr.s_addr = htonl(term_remoteaddr);
    }
    memcpy((char *)buff,mybuff+HEADER_SIZE,avail*sizeof(char));
  }     
  if (avail >= 0 && fromlen != NULL && from != NULL) {
    *fromlen = (*fromlen > myfromlen) ? myfromlen : *fromlen;
    memcpy(from,&myfrom,*fromlen * sizeof(char));
  }

  errno=k;
  return avail;
}


/* term_recv()
 *
 * just call term_recvfrom ignoring the garbage -danjo
 */

#ifndef linux
int term_recv(int s, char *buf, int len, unsigned int flags)
#else
int term_recv(int s, void *buf, int len, unsigned int flags)
#endif
{
  if(! use_term) return x__recv(s,(char *)buf,len,flags);

  if (remote_connect < 0) {
    remote_connect=use_term_command(PUBLIC);
    store_sockinfo(-1,-1,NULL);
  }

  if (term_debug & 32768) {
    tmp_errno = errno;
    sprintf(term_error,"test term_recvfrom");
    print_err(32768);
  }

  return term_recvfrom(s,(char *)buf,len,flags,NULL,NULL);
}

/* The following 3 functions all do the same thing, duplicate the 
 * sockinfo when the file descriptor gets duplicated.
 */

int term_dup(int oldfd) {
  int k, newfd;

  if(! use_term) return x__dup(oldfd);

  if ((newfd = x__dup(oldfd)) < 0) return newfd;

  if (term_debug & 32768) {
    tmp_errno = errno;
    sprintf(term_error,"test term_dup");
    print_err(32768);
  }

  if (remote_connect < 0) {
    remote_connect=use_term_command(PUBLIC);
    store_sockinfo(-1,-1,NULL);
  }else {
    for(k=0;k<MAX_CLIENTS;k++)
      if (oldfd == sockinfo[k].sock) break;
    if (k == MAX_CLIENTS) return newfd;

    store_sockinfo(newfd, sockinfo[k].client, sockinfo[k].udp_addr);
  }
  return newfd;
}


int term_dup2(int oldfd, int newfd) {
  int k, fd;

  if(! use_term) return x__dup2(oldfd,newfd);

  if ((fd = x__dup2(oldfd,newfd)) < 0) return fd;

  if (term_debug & 32768) {
    tmp_errno = errno;
    sprintf(term_error,"test term_dup2");
    print_err(32768);
  }

  if (remote_connect < 0) {
    remote_connect=use_term_command(PUBLIC);
    store_sockinfo(-1,-1,NULL);
  }else {
    for(k=0;k<MAX_CLIENTS;k++)
      if (oldfd == sockinfo[k].sock) break;
    if (k == MAX_CLIENTS) return fd;

    store_sockinfo(newfd, sockinfo[k].client, sockinfo[k].udp_addr);
  }
  return fd;
}


int term_fcntl(int fd, int cmd, ...) {
  int k, newfd;

  if(! use_term) return x__fcntl(fd,cmd,*(long *)(sizeof(int *)+&cmd));

  if ((newfd=x__fcntl(fd, cmd, *(long *)(sizeof(int *)+&cmd))) < 0) return newfd;

  if (cmd != F_DUPFD) return newfd;

  if (term_debug & 32768) {
    tmp_errno = errno;
    sprintf(term_error,"test term_fcntl");
    print_err(32768);
  }

  if (remote_connect < 0) {
    remote_connect=use_term_command(PUBLIC);
    store_sockinfo(-1,-1,NULL);
  }else {
    for(k=0;k<MAX_CLIENTS;k++)
      if (fd == sockinfo[k].sock) break;
    if (k == MAX_CLIENTS) return newfd;

    store_sockinfo(newfd, sockinfo[k].client, sockinfo[k].udp_addr);
  }
  return newfd;
}

#ifndef linux
int term_send(int s, void *msg, int len, unsigned int flags) {
#else
int term_send(int s, const void *msg, int len, unsigned int flags) {
#endif

  if(! use_term) return x__send(s,(char *)msg,len,flags);

  if (remote_connect < 0) {
    remote_connect=use_term_command(PUBLIC);
    store_sockinfo(-1,-1,NULL);
  }

  if (term_debug & 32768) {
    tmp_errno = errno;
    sprintf(term_error,"test term_send");
    print_err(32768);
  }

  return term_sendto(s, msg, len, flags, NULL, 0);
}

