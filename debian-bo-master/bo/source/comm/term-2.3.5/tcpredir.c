#define I_ERRNO
#define I_SYS
#define I_SIGNAL
#define I_CTYPE
#include "includes.h"

#include "client.h"

#define MAXREDIR 32
/* improved version - does multiple redirs  -ot */

#ifdef ONE_CLIENT
# define main tredir
#else
int term_debug = 0;
#endif

char *sockaddr_to_str(struct sockaddr *,int trans);
char *remote_port[MAXREDIR];

int rd_connect_server(int n, struct sockaddr *addr) {
  int s;
  
  if ((s = socket_connect_server(-1,term_server)) <0) {
    fprintf(stderr,"Term: %s\n",command_result);
    x__close(s);
    return -1;
  }

  send_command(s, C_SETPEERNAME, 0, "%s", sockaddr_to_str(addr,0));

  if (send_command(s, C_PORT, 0, "%s", remote_port[n])< 0) {
    fprintf(stderr,"Term C_PORT: %s\n",command_result);
    return -1;
  }

  send_command(s, C_DUMB, 1, 0);
  return s;
}

void check_usage(int n) {
  if ( n < 2 )
    {
      fprintf ( stderr, "Usage: redir [options] {<localport> <[host:]remoteport>}\n" );
      exit(1);
    }
}

int main(int argc, char *argv[]) {
  int s;
  int first, i;
  unsigned int port;
  int svs[MAXREDIR];

#ifdef SOCKS
  SOCKSinit(argv[0]);
#endif

  signal(SIGPIPE, SIG_IGN);

  if ((first = client_options(argc, argv,"",NULL)) < 0) exit(1);

  setbuf(stderr, 0);
	check_usage(argc-first);

  for (i = 0; i < MAXREDIR && first < argc;first += 2) {
		check_usage(argc - first);
		fprintf(stderr, "Redirecting %s to %s\n", argv[first], argv[first+1] );
    remote_port[i] = argv[first+1];
    
    /* Bind the local socket we are going */
    /* to listen on.  */
    if ( isdigit(*argv[first]) ) 
      sscanf(argv[first],"%u",&port);
    else {
      struct servent *service;
      service = getservbyname(argv[first],"tcp");
      port = service ? ntohs(service->s_port) : 0;
    }

    if (! port) {
      fprintf(stderr, "Unknown service %s\n", argv[first]);
      continue;
    }else {
      s = bind_tcp_listen(port, 5);
    }

    if (s == -2) {
      fprintf(stderr, "Port is already bound.\n");
    } else if (s < 0) {
      fprintf(stderr, "Can't bind to %s.\n", argv[first]);
    } else {
      svs[i++] = s;
    }
  }
  if(!i){
    fprintf(stderr,"Nothing to do.\n");
    exit(1);
  };
  do_connect(i, svs, rd_connect_server);
  exit(0);
}
