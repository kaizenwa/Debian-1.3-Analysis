#include "includes.h"
#include "client.h"

int main(int argc, char *argv[]) {
  int first, s, new, port;
  char buff[10];

#ifdef SOCKS
  SOCKSinit(argv[0]);
#endif
  first = client_options(argc, argv);
  printf("first is %d\n", first);


  s = connect_server(term_server);
  if ((send_command(s, C_BIND, 0, "%d", 4040)) < 0) {
    printf("Couldn't run C_BIND.\n");
    printf("reason given: %s\n", command_result);
  }

  read(s, buff, 2);
  printf("buf == %s\n", buff);
  port = atoi(buff);

  new = connect_server(term_server);
  if ((send_command(new, C_ACCEPT, 0, "%d", port)) < 0) {
    printf("C_accept failed.\n");
    printf("Reason given: %s\n", command_result);
  }
  write(new, "Hello.\n", 8);
  sleep(2);
  x__close(new);
  x__close(s);
}
