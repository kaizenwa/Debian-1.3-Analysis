#ifndef _SYS_MMAN_H
#define _SYS_MMAN_H

#define MAP_ANON	MAP_ANONYMOUS	   
#define MAP_FILE	0x00	 

#define PROT_NONE 0
#define PROT_READ 1
#define PROT_WRITE 2
#define PROT_EXEC 4

#define MAP_SHARED 1
#define MAP_FIXED 0x10

extern caddr_t mmap (caddr_t addr, size_t len, int prot, int flags, int fd, off_t off);
extern int munmap (caddr_t addr, size_t len);
extern int mprotect (caddr_t addr, size_t len, int prot);

#endif 
