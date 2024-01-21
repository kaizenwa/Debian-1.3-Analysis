/* udpredir.c - redirect udp sockets over term
 * Danny Gasparovski u923168@student.canberra.edu.au
 *
 * C_USOCK both ends, C_UBIND local end,
 * then hang around forever (until we get killed)
 * so that the UDP clients don't die.
 */

#define I_STRING
#define I_INET
#define I_SYS
#define I_IOCTL
#define I_SIGNAL

#include "includes.h"

#define MAXREDIR 32

int S = -1;
char *sockaddr_to_str(struct sockaddr *,int trans);

static void sig_ignore(int dummy) {
  signal(SIGPIPE, sig_ignore);
}

/* do the redirecting */

static int do_udp_redir(unsigned short localport, struct sockaddr *r_addr) {
  int s,client;

  if (S < 0) return -1;

  /* create new client */
  if( (s = socket_connect_server(-1,term_server)) <0) {
    fprintf(stderr,"Term: %s\n", command_result);
    return -1;
  }
  else if( send_command(s, C_STATS, 1, "%d", -6) < 0) {
    fprintf(stderr,"Term C_STATS: %s\n", command_result);
    term_close(s);
    return -1;
  }
  client = atoi(command_result);

  /* C_USOCK it */
  if(send_command(S,C_USOCK,0,"%d %d",client,
                    (UDP_T_SENDSTRIPHDR|UDP_T_SENDIGNOREHDR))<0) {
    fprintf(stderr,"Term C_USOCK: %s\n", command_result);
    term_close(s);
    return -1;
  }
  else if(send_command(S,C_USOCK,1,"%d %d",client,(UDP_T_RECADDHDR))<0) {
    fprintf(stderr,"Term C_USOCK: %s\n", command_result);
    term_close(s);
    return -1;
  }
  term_close(s);

  /* bind local USOCK to read the "from" port */
  if( send_command(S, C_UBIND, 1, "%d %d", client, localport) <0) {
    fprintf(stderr,"Term C_UBIND: %s\n", command_result);
    return -1;
  }

  /* set up UDP parameters on remote to reflect the "to" port */
  if( send_command(S, C_UDPSET, 0, "%d :%s", client, sockaddr_to_str(r_addr,0)) <0) {
    fprintf(stderr,"Term C_UDPSET: %s\n", command_result);
    return -1;
  }

  /* all seemed to succeed */
  return 0;
}


static void check_usage(int n) {

  if ( n < 2 ) {
    fprintf ( stderr, "Usage: udpredir [options] {<localport> <[host:]remoteport>}\n" );
    exit(1);
  }
}

#ifdef ONE_CLIENT
# define main tudpredir
#endif

int main(int argc, char **argv) {

struct sockaddr_in r_addr;
int first,i;
char *ptr=NULL;
struct hostent *hh;
fd_set in, out, except;

#ifdef SOCKS
  SOCKSinit(argv[0]);
#endif
  signal(SIGPIPE, sig_ignore);

  if( (first = client_options(argc,argv,"",NULL)) <0)
    exit(1);
  check_usage(argc - first);

  if( (S=socket_connect_server(-1,term_server)) <0) {
    fprintf(stderr,"Term: %s\n", command_result);
    exit(1);
  }

  /* parse args */
  for (i = 0; i < MAXREDIR && first < argc;first += 2) {
    check_usage(argc - first);
    fprintf(stderr, "Redirecting %s to %s\n", argv[first], argv[first+1] );

    r_addr.sin_family = AF_INET;

    ptr = strchr(argv[first+1],':');
    if(ptr == NULL) {
      r_addr.sin_addr.s_addr = inet_addr("127.0.0.254");
      r_addr.sin_port = atoi(argv[first+1]);
    } else {
      char *host = NULL;
      r_addr.sin_port = atoi(++ptr); /* get port */
      if( r_addr.sin_port <= 0) {
        fprintf(stderr,"Bad port number %u.\n", r_addr.sin_port);
        exit(1);
      }
      host = (char *) malloc( sizeof(char) *  ( strlen(argv[first+1]) + 1) );
      strcpy(host,argv[first+1]);
      ptr = strchr(host,':');
      *ptr = '\0'; 
      if ((hh = term_gethostbyname(host)) == NULL) exit(1);
      if (host) free(host);
      memcpy(&r_addr.sin_addr, hh->h_addr, hh->h_length);
    }

    /* ok, got everything, redirect it */
    if( do_udp_redir((unsigned short)atoi(argv[first]),
          (struct sockaddr *)&r_addr) <0) exit(1);
  }

  FD_ZERO(&in);
  FD_ZERO(&out);
  FD_ZERO(&except);

 if (S != 0) term_close(0);
 if (S != 1) term_close(1);
 if (S != 2) term_close(2);

	/* We should wait until term exits. */  
	/* This doesn't seem to work, I'm not sure why... */
  do {
    FD_SET(S,&in);
    FD_SET(S,&except);
    select(1, &in, &out, &except, 0);
  } while ( ! (FD_ISSET(S,&except) || FD_ISSET(S,&in)) );

  term_close(S);
  exit(0);
}

