#include <sys/param.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <arpa/nameser.h>

#include <stdio.h>
#include <ctype.h>
#include <resolv.h>

#pragma weak _res = __res

#undef _res;
extern struct __res_state _res;
struct __res_state __res = {0};

#pragma weak __res_status_location = __normal__res_status_location

/* The one in libpthread will override __res_status_location () */
struct __res_state *__normal__res_status_location ( void );

struct __res_state *
__normal__res_status_location ( void )
{
  return &_res;
}
