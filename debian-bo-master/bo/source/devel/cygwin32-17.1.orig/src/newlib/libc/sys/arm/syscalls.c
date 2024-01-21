#include <_ansi.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "swi.h"
int errno;

int
_swiread (int file,
       char *ptr,
       int len)
{
  asm ("swi %a0" :: "i" (SWI_Read ));
}

int
_read (int file,
       char *ptr,
       int len)
{
  return len - _swiread (file, ptr, len);
}


int
_lseek (int file,
	int ptr,
	int dir)
{
  asm ("swi %a0" :: "i" (SWI_Seek));
}

static
writechar (char c)
{
  asm ("swi %a0" :: "i" (SWI_WriteC));
}

/* Returns #chars not! written */

int
_swiwrite (
	 int file,
	 char *ptr,
	 int len)
{
  asm ("swi %a0" :: "i" (SWI_Write));
}

int
_write (
	 int file,
	 char *ptr,
	 int len)
{
  return len - _swiwrite (file, ptr,len); 
}



int
_close (int file)
{
  asm ("swi %a0" :: "i" (SWI_Close));
}



register char *stack_ptr asm ("sp");

caddr_t
_sbrk (int incr)
{
  extern char end asm ("__end__");	/* Defined by the linker */
  static char *heap_end;
  char *prev_heap_end;

  if (heap_end == 0)
    {
      heap_end = &end;
    }
  prev_heap_end = heap_end;
  if (heap_end > stack_ptr)
    {
      _write (1, "Heap and stack collision\n", 25);
      abort ();
    }
  heap_end += incr;
  return (caddr_t) prev_heap_end;
}




int
_fstat (int file,
	struct stat *st)
{
  st->st_mode = S_IFCHR;
  return 0;
}


int
_open (
	const char *path,
	int flags)
{
  asm ("swi %a0" :: "i" (SWI_Open));
}

int
_unlink ()
{
  return -1;
}

isatty (fd)
     int fd;
{
  return 1;
}



_exit (n)
{
  asm ("swi %a0" :: "i" (SWI_Exit));
}

abort ()
{
 asm ("mov r0,#17\nswi %a0" :: "i" (SWI_Exit));
}


_kill (n, m)
{
  asm ("swi %a0" :: "i" (SWI_Exit));
}


_getpid (n)
{
  return 1;
}




_raise ()
{

}

#if 0
int
_stat (const char *path, struct stat *st)

{

  asm ("swi %a0" :: "i" (SWI_Stat));
}

int
_chmod (const char *path, short mode)
{
  asm ("swi %a0" :: "i" (SWI_Chmod));
}

int
_chown (const char *path, short owner, short group)
{
  asm ("swi %a0" :: "i" (SWI_Chown));
}

int
_utime (path, times)
     const char *path;
     char *times;
{
  asm ("swi %a0" :: "i" (SWI_Utime));
}

int
_fork ()
{
  asm ("swi %a0" :: "i" (SWI_Fork));
}

int
_wait (statusp)
     int *statusp;
{
  asm ("swi %a0" :: "i" (SWI_Wait));
}

int
_execve (const char *path, char *const argv[], char *const envp[])
{
  return _trap3 (SYS_execve, path, argv, envp);
}

int
_execv (const char *path, char *const argv[])
{
  return _trap3 (SYS_execv, path, argv);
}

int
_pipe (int *fd)
{
  return _trap3 (SYS_pipe, fd);
}
#endif

signal()
{
}
alarm()
{
}
