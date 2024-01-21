#include <netinet/in.h>

#ifdef __cplusplus
extern "C"
{
#endif

unsigned long	 cygwin32_inet_addr (const char *);
int		 cygwin32_inet_aton (const char *, struct in_addr *);
unsigned long	 cygwin32_inet_lnaof (struct in_addr);
struct in_addr	 cygwin32_inet_makeaddr (unsigned long , unsigned long);
unsigned int	 cygwin32_inet_netof (struct in_addr);
unsigned int	 cygwin32_inet_network (const char *);
char		*cygwin32_inet_ntoa (struct in_addr);

#ifdef __cplusplus
};
#endif


#ifndef __INSIDE_CYGWIN_NET__

#define inet_addr cygwin32_inet_addr 
#define inet_aton cygwin32_inet_aton 
#define inet_lnaof cygwin32_inet_lnaof 
#define inet_makeaddr cygwin32_inet_makeaddr 
#define inet_netof cygwin32_inet_netof 
#define inet_network cygwin32_inet_network 
#define inet_ntoa cygwin32_inet_ntoa 
 
#endif
