#ifndef _SYSCALL_H_
#define _SYSCALL_H_

#include <sys/syscall.h>

extern inline volatile void sim_exit(int status);
extern inline volatile void sim_close(int fd);
extern inline int sim_mmap(void * addr, unsigned int size,
				    unsigned int prot,
				    unsigned int flags, int fd,
				    unsigned int f_offset);
extern inline void sim_munmap(void * addr, unsigned int size);
extern inline int sim_open(char * addr, unsigned int flags);
extern inline int sim_write(int fd, const char * buf, int len);
extern inline int sim_read(int fd, const char * buf, int len);
extern inline int sim_mprotect(const char * addr, int size, int prot);

/* Here are the definitions for a bunch of syscalls that are required
   by the dynamic linker.  The idea is that we want to be able
   to call these before the errno symbol is dynamicly linked, so
   we use our own version here.  Note that we cannot assume any
   dynamic linking at all, so we cannot return any error codes.
   We just punt if there is an error. */

extern inline volatile void sim_exit(int status)
{
#if 0
  int __res;
#ifdef IBCS_COMPATIBLE
  __asm__ volatile ("pushl %0\n\tpushl $0\n\tmovl %1,%%eax\n\t" \
		    "lcall $7,$0" : : "r" (status), "a" (__IBCS_exit));
#else
  __asm__ volatile ("movl %%ecx,%%ebx\n"\
		    "int $0x80" \
		    :  "=a" (__res) : "0" (__NR_exit),"c" ((long)(status)));
#endif
#else
  __asm__ volatile ("mov %1, %%o0\n\t"
  		    "mov %0, %%g1\n\t"
  		    "ta 8\n\t"
  		    "nop\n\t"
  		    : /* no output */ : "I" (SYS_exit), "r" (status)
  		    : "%o0", "%g1");
#endif
}

extern inline volatile void sim_close(int fd)
{
    int status;
#if 0
#ifdef IBCS_COMPATIBLE
    __asm__ volatile ("pushl %1\n\t" \
	    "pushl $0\n\t" \
	    "movl %2,%%eax\n\t" \
	    "lcall $7,$0\n\t" \
	    "jnb .+4\n\t" \
	    "xor %%eax, %%eax\n\t" \
	    "addl $8,%%esp\n\t" \
	    : "=a" (status) : "r" (fd), "a" (__IBCS_close));
#else
  __asm__ volatile ("pushl %%ebx\n"\
		    "movl %%ecx,%%ebx\n"\
		    "int $0x80\n" \
		    "popl %%ebx\n"\
		    : "=a" (status) \
		    : "0" (__NR_close),"c" ((long)(fd)));
    
#endif
#else
  __asm__ volatile ("mov %1, %%g1\n\t"
  		    "mov %2, %%o0\n\t"
  		    "ta 8\n\t"
  		    "nop\n\t"
  		    "mov %%o0, %0\n\t"
  		    : "=r" (status) : "I" (SYS_close), "r" (fd)
  		    : "%o0", "%g1");
#endif
/*  return status; */
}

extern inline int sim_mmap(void * addr, unsigned int size,
				    unsigned int prot,
				    unsigned int flags, int fd,
				    unsigned int f_offset)
{
  int malloc_buffer;
#if 0
#ifdef IBCS_COMPATIBLE
  __asm__ volatile ("pushl %7\n\t" \
		    "pushl %6\n\t" \
		    "pushl %5\n\t" \
		    "pushl %4\n\t" \
		    "pushl %3\n\t" \
		    "pushl %2\n\t" \
		    "pushl $0\n\t" \
		    "movl %1,%%eax\n\t" \
		    "lcall $7,$0\n\t" \
		    "jnb .+4\n\t" \
		    "xor %%eax, %%eax\n\t" \
		    "addl $28,%%esp\n\t" \
		    : "=a" (malloc_buffer) : "a" (__IBCS_mmap),
		    "rm" (addr), "rm" (size), "rm" (prot), "rm" (flags), 
		    "rm" (fd), "rm" (f_offset));
#else
  __asm__ volatile ("pushl %%ebx\n\t" \
		    "pushl %7\n\t" \
		    "pushl %6\n\t" \
		    "pushl %5\n\t" \
		    "pushl %4\n\t" \
		    "pushl %3\n\t" \
		    "pushl %2\n\t" \
		    "movl %%esp,%%ebx\n\t" \
		    "int $0x80\n\t" \
		    "addl $24,%%esp\n\t" \
		    "popl %%ebx\n" \
		    : "=a" (malloc_buffer) : "a" (__NR_mmap),
		    "rm" (addr), "rm" (size), "rm" (prot), "rm" (flags), 
		    "rm" (fd), "rm" (f_offset));
#endif
#else
  __asm__ volatile ("mov %2, %%o0\n\t"
  		    "mov %3, %%o1\n\t"
  		    "mov %4, %%o2\n\t"
  		    "sethi %%hi(0x80000000), %%g1\n\t"
  		    "or %%g1, %5, %%o3\n\t"
  		    "mov %6, %%o4\n\t"
  		    "mov %7, %%o5\n\t"
  		    "mov %1, %%g1\n\t"
  		    "ta 8\n\t"
  		    "bcc,a 1f\n\t"
  		    "mov %%o0, %0\n\t"
  		    "mov -1, %0\n\t"
  		    "1: nop\n\t"
  		    : "=r" (malloc_buffer) : "I" (SYS_mmap), "r" (addr), "r" (size), "r" (prot), "r" (flags), "r" (fd), "r" (f_offset) 
  		    : "%o0", "%o1", "%o2", "%o3", "%o4", "%o5", "%g1");
#endif
  return malloc_buffer;
}

extern inline void sim_munmap(void * addr, unsigned int size)
{
  __asm__ volatile ("mov %0, %%g1\n\t"
  		    "mov %1, %%o0\n\t"
  		    "mov %2, %%o1\n\t"
  		    "ta 8\n\t"
  		    "nop\n\t"
  		    : /* no output */ : "I" (SYS_munmap), "r" (addr), "r" (size)
  		    : "%o0", "%o1", "%g1");
}

#if 0
#include <sys/mman.h>

extern inline int sim_mmap(void * addr, unsigned int size,
				    unsigned int prot,
				    unsigned int flags, int fd,
				    unsigned int f_offset)
{
  if (flags & MAP_SHARED)
    return sim_mmap_real(addr, size, prot, flags, fd, f_offset);
  else
    {
      int res;
      flags &= ~MAP_PRIVATE;
      flags |= MAP_SHARED;
      res = sim_mmap_real(addr, size, prot, flags, fd, f_offset);
      flags &= ~MAP_SHARED;
      flags |= MAP_PRIVATE;
      if (res != -1)
        sim_munmap((void*)res, size);
      else
        res = (int)addr;
      return sim_mmap_real((void*)res, size, prot, flags, fd, f_offset);
    }
}
#endif

extern inline int sim_open(char * addr, unsigned int flags)
{
  int zfileno;
#if 0
#ifdef IBCS_COMPATIBLE
  __asm__ volatile ("pushl %3\n\t" \
	    "pushl %2\n\t" \
	    "pushl $0\n\t" \
	    "movl %1,%%eax\n\t" \
	    "lcall $7,$0\n\t" \
	    "jnb .+7\n\t" \
	    "movl $-1, %%eax\n\t" \
	    "addl $12,%%esp\n\t" \
	    :"=a" (zfileno) : "i" (__IBCS_open), "rm" (addr), "rm" (flags));
#else
  __asm__ volatile ("pushl %%ebx\n"\
		    "movl %%esi,%%ebx\n"\
		    "int $0x80\n" \
		    "popl %%ebx\n"\
		    : "=a" (zfileno) \
		    : "0" (__NR_open),"S" ((long)(addr)),"c" ((long)(flags)));
#endif
#else
  __asm__ volatile ("mov %1, %%g1\n\t"
  		    "mov %2, %%o0\n\t"
  		    "mov %3, %%o1\n\t"
  		    "ta 8\n\t"
  		    "bcc,a 1f\n\t"
  		    "mov %%o0, %0\n\t"
  		    "mov -1, %0\n\t"
  		    "1: nop\n\t"
  		    : "=r" (zfileno) : "I" (SYS_open), "r" (addr), "r" (flags)
  		    : "%o0", "%o1", "%g1");
#endif

  return zfileno;
}

extern inline int sim_write(int fd, const char * buf, int len)
{
  int status;
#if 0
#ifdef IBCS_COMPATIBLE
  __asm__ volatile ("pushl %4\n\t" \
	  "pushl %3\n\t" \
	  "pushl %2\n\t" \
	  "pushl $0\n\t" \
	  "movl %1,%%eax\n\t" \
	  "lcall $7,$0\n\t" \
	  "jnb .+4\n\t" \
	  "xor %%eax, %%eax\n\t" \
	  "addl $12,%%esp\n\t" \
	  :"=a" (status) : "i" (__IBCS_write), "rm" (fd), "rm" (buf), "rm" (len));
#else
  __asm__ volatile ("pushl %%ebx\n"\
		    "movl %%esi,%%ebx\n"\
		    "int $0x80\n" \
		    "popl %%ebx\n"\
		    : "=a" (status) \
		    : "0" (__NR_write),"S" ((long)(fd)),"c" ((long)(buf)),"d" ((long)(len)));
#endif
#else
  __asm__ volatile ("mov %1, %%g1\n\t"
  		    "mov %2, %%o0\n\t"
  		    "mov %3, %%o1\n\t"
  		    "mov %4, %%o2\n\t"
  		    "ta 8\n\t"
  		    "nop\n\t"
  		    "mov %%o0, %0\n\t"
  		    : "=r" (status) : "I" (SYS_write), "r" (fd), "r" (buf), "r" (len)
  		    : "%o0", "%o1", "%o2", "%g1");
#endif
  return status;
}


extern inline int sim_read(int fd, const char * buf, int len)
{
  int status;
#if 0
#ifdef IBCS_COMPATIBLE
  __asm__ volatile ("pushl %4\n\t" \
	  "pushl %3\n\t" \
	  "pushl %2\n\t" \
	  "pushl $0\n\t" \
	  "movl %1,%%eax\n\t" \
	  "lcall $7,$0\n\t" \
	  "jnb .+4\n\t" \
	  "xor %%eax, %%eax\n\t" \
	  "addl $12,%%esp\n\t" \
	  : "=a" (status) : "i" (__IBCS_read), "rm" (fd), "rm" (buf), "rm" (len));
#else
  __asm__ volatile ("pushl %%ebx\n"\
		    "movl %%esi,%%ebx\n"\
		    "int $0x80\n" \
		    "popl %%ebx\n"\
		    : "=a" (status) \
		    : "0" (__NR_read),"S" ((long)(fd)),"c" ((long)(buf)),"d" ((long)(len)));
#endif
#else
  __asm__ volatile ("mov %1, %%g1\n\t"
  		    "mov %2, %%o0\n\t"
  		    "mov %3, %%o1\n\t"
  		    "mov %4, %%o2\n\t"
  		    "ta 8\n\t"
  		    "nop\n\t"
  		    "mov %%o0, %0\n\t"
  		    : "=r" (status) : "I" (SYS_read), "r" (fd), "r" (buf), "r" (len)
  		    : "%o0", "%o1", "%o2", "%g1");
#endif
  return status;
}

extern inline int sim_mprotect(const char * addr, int size, int prot)
{
  int status;
#if 0
#ifdef IBCS_COMPATIBLE
  __asm__ volatile ("pushl %4\n\t" \
	  "pushl %3\n\t" \
	  "pushl %2\n\t" \
	  "pushl $0\n\t" \
	  "movl %1,%%eax\n\t" \
	  "lcall $7,$0\n\t" \
	  "jnb .+7\n\t" \
	  "movl $-1, %%eax\n\t" \
	  "addl $16,%%esp\n\t" \
	  :"=a" (status) : "i" (__IBCS_mprotect), "rm" (addr), "rm" (size), "rm" (prot));
#else
  __asm__ volatile ("pushl %%ebx\n"\
		    "movl %%esi,%%ebx\n"\
		    "int $0x80\n" \
		    "popl %%ebx\n"\
		    : "=a" (status) \
		    : "0" (__NR_mprotect),"S" ((long)(addr)),"c" ((long)(size)),"d" ((long)(prot)));
#endif
#else
  __asm__ volatile ("mov %1, %%g1\n\t"
  		    "mov %2, %%o0\n\t"
  		    "mov %3, %%o1\n\t"
  		    "mov %4, %%o2\n\t"
  		    "ta 8\n\t"
  		    "nop\n\t"
  		    "mov %%o0, %0\n\t"
  		    : "=r" (status) : "I" (SYS_mprotect), "r" (addr), "r" (size), "r" (prot)
  		    : "%o0", "%o1", "%o2", "%g1");
#endif
  return status;
}

#if 0
extern inline int sim_fstat(int fd, void *stat)
{
  int status;
  __asm__ volatile ("mov %1, %%g1\n\t"
  		    "mov %2, %%o0\n\t"
  		    "mov %3, %%o1\n\t"
  		    "ta 8\n\t"
  		    "nop\n\t"
  		    "mov %%o0, %0\n\t"
  		    : "=r" (status) : "I" (SYS_fstat), "r" (fd), "r" (stat)
  		    : "%o0", "%o1", "%g1");
  return status;
}
#endif

#endif /* _SYSCALL_H_ */
