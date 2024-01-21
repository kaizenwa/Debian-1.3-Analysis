#include <sys/mman.h>
#include <sys/fcntl.h>
#include <errno.h>

main()
{
 unsigned char *addr;
 char *dum;
 int fd;
 int i,j;
 
 dum = malloc(10);
 dum[4] = 'c';
 dum[6] = 'e';
 i = dum[4];

#if 0 
 fd = open("/etc/locale/C/libc.cat", O_RDONLY);
#else
 fd = open("/dev/zero", O_RDWR);
#endif
 addr = 0;
 addr = mmap(addr, 4096, PROT_READ|PROT_WRITE, MAP_PRIVATE, fd,0);
 close(fd);
 printf ("mmap addr: %p\n", addr);
 if (addr == (char*)-1)
    perror("mmap:");
 for (j = 0; j < 4096; j += 16)
   {
     printf("%04x:  ", (unsigned int)addr + j);
     for (i = 0; i < 16; i++)
       printf("%02x ", addr[j+i]);
     printf("   ");
     for (i = 0; i < 16; i++)
       printf("%c", addr[j+i] >= 32 ? addr[j+i] : '.');
     printf("\n");
   }
 munmap(addr, 4096);
 printf("%d\n", addr[2]);
}
