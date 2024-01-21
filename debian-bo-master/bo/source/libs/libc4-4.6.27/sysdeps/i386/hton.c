#include <ansidecl.h>
#include <netinet/in.h>

#undef ntohl
#undef ntohs
#undef htonl
#undef htons

unsigned long int
htonl(unsigned long int x)
{
  return __htonl (x);
}

unsigned short int
htons(unsigned short int x)
{
  return __htons (x);
}

#include <gnu-stabs.h>
#ifdef elf_alias
elf_alias (htonl, ntohl);
elf_alias (htons, ntohs);
#endif

