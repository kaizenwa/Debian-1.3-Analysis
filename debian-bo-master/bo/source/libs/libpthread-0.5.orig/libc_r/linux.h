#ifndef _CONFIG_LINUX_H
#define  _CONFIG_LINUX_H

#undef _STDIO_USES_IOSTREAM
#define _STDIO_USES_IOSTREAM   1

#undef _IO_HAVE_ST_BLKSIZE
#define _IO_HAVE_ST_BLKSIZE    1

#undef _IO_DEBUG
#define _IO_DEBUG

#define _IO_open       __open
#define _IO_close      __close
#define        _IO_fork        __fork
#define        _IO_fcntl       __fcntl
#define _IO__exit      _exit
#define _IO_read       __read
#define _IO_write      __write
#define _IO_lseek      __lseek
#define        _IO_getdtablesize       __getdtablesize
#define _IO_pipe       __pipe
#define _IO_dup2       __dup2
#define _IO_execl      execl
#define _IO_waitpid    __waitpid
#define _IO_stat        __stat
#define _IO_getpid      __getpid
#define _IO_geteuid     __geteuid
#define _IO_getegid     __getegid
#define _IO_fstat      __fstat

#endif /* _CONFIG_LINUX_H */
