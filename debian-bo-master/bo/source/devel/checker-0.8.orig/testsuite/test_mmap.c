#include <sys/mman.h>
#include <errno.h>

main()
{
 char *addr;
 addr = mmap(0x900000, 4096, PROT_READ | PROT_WRITE, MAP_FIXED | MAP_PRIVATE,
             0,0);
 if (addr != 0x900000)
    perror("mmap:");

}    