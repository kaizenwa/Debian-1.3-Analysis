#define I_ERRNO
#define I_SYS
#define I_IOCTL
#define I_STRING
#define I_SIGNAL
#define I_GETOPT
#define I_CTYPE

#include "includes.h"

#ifndef X_DEFAULT_DISPLAY_NUM
#define X_DEFAULT_DISPLAY_NUM 9
#endif

int display_num = X_DEFAULT_DISPLAY_NUM;

#ifdef ONE_CLIENT
# define main txconn
#else
int term_debug = 0;
#endif

int xc_connect_server(int dummy, struct sockaddr *addr) {
  int s;
  signal(SIGPIPE, SIG_IGN);
  if ((s = connect_server(term_server)) < 0)
    return -1;

  if (send_command(s, C_X_SERVER, 0, REMOTE_X) < 0)
    return -1;

  send_command(s, C_DUMB, 1, 0);

  return s;
}


int main(int argc, char *argv[])
{
  int s2, svs[4], s = 0;
  char *p, host[1024];
#ifdef X_UNIX_SOCKET
  char unix_sock[110];
#endif
#ifdef SOCKS
  SOCKSinit(argv[0]);
#endif

  (void) client_options(argc, argv,"",NULL);
  setbuf(stderr, 0);

/* We begin by opening the socket so we can tell if this is unix or tcp. */


  if ((s2 = socket_connect_server(-1,term_server)) < 0) {
    fprintf(stderr, "Term: %s\n", command_result);
    exit(1);
  }

/* Figure out the host:display_num to use */
  p = (term_optind < argc) ? argv[term_optind] : getenv("DISPLAY");
  if (p && strchr(p,':')) {
    strncpy(host, p, (strlen(p) >= sizeof(host)) ? sizeof(host)-1 : strlen(p));
  } else {
    host[0] = '\0';
  }
  if ((p = strchr(host,':')) == NULL) {
    display_num = X_DEFAULT_DISPLAY_NUM;
  } else {
    display_num = isdigit(*p) ? atoi(p) : X_DEFAULT_DISPLAY_NUM;
    *p = '\0';
  }

/* At this point, host:display_num is our new DISPLAY */

#ifdef X_TCP_SOCKET
  while ((svs[s] = bind_tcp_listen(6000+(unsigned int)display_num, 5)) == -2) {
    if (++display_num > 99) {
      fprintf(stderr, "Unable to bind tcp socket\n");
      break;
    }
  } 
  if (display_num < 100) ++s;
#endif /* X_TCP_SOCKET */

#ifdef X_UNIX_SOCKET
  sprintf(unix_sock,LOCAL_X_DIR);
  mkdir(unix_sock, 0777);
  sprintf(unix_sock,"%s/%s%d", LOCAL_X_DIR, LOCAL_X_NAME, display_num);
  if ((svs[s] = bind_unix(unix_sock)) < 0) 
    fprintf(stderr, "Unable to bind unix socket\n");
  else
    ++s;
#endif /* X_UNIX_SOCKET */

#ifdef X_STREAMS_PIPE
  if ((svs[s] = open_stream_pipe(display_num)) < 0) 
    fprintf(stderr, "Unable to open streams pipe\n");
  else
    ++s;
#endif /* X_STREAMS_PIPE */

  if (!s) {
    fprintf(stderr,"Xconn: no connections\n");
    exit(1);
  } else {
    send_command(s2, C_PUTENV, 1, "DISPLAY=%s:%d", host, display_num); 
    x__close(s2);
    fprintf(stderr,"Xconn bound to DISPLAY=%s:%d\n", host, display_num);
    printf("%s:%d\n", host, display_num);
    do_connect(s, svs, xc_connect_server); /* Forks a child */
  }
  exit(0);
}
