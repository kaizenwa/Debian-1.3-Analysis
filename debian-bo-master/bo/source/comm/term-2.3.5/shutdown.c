#include "includes.h"
#include "client.h"

#ifdef ONE_CLIENT
# define main tshutdown
#else
int term_debug = 0;
#endif

int arg = -1;

static int local_options ( char opt, char *term_optarg )
{
  switch(opt)
  {
  case 'h' :
    arg = 2; 
    break;
  case 'n' :
    arg = 1; 
    break;
  default:
    return -1;
  }
  return 0;
}

int main(int argc, char *argv[]) {
  int s;

#ifdef SOCKS
  SOCKSinit(argv[0]);
#endif
  (void) client_options(argc, argv,"vhn",local_options);

  use_term_command(PRIVILEGED);

  if ((s = socket_connect_server(-1,term_server)) <0) {
    fprintf(stderr,"Term: %s\n", command_result);
    exit(1);
  }

  if (verbose) 
    printf ("Shutting down remote...\n");
  if (send_command(s, C_QUIT, 0, "%d", arg) < 0) {
    fprintf(stderr,"Couldn't shutdown remote: %u\n", termerrno);
    exit(1);
  }
  if (verbose)
    printf ("Shutting down local...\n");
  if (send_command(s, C_QUIT, 1, "%d", arg) < 0) {
    fprintf(stderr,"Couldn't shutdown local: %u\n", termerrno);
    exit(1);
  }
  exit(0);
}
