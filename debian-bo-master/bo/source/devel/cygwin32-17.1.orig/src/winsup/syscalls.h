/* syscalls for WIN32.

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
*/

#ifdef __cplusplus
extern "C" {
#endif

#include <sys/stat.h>
#include <sys/param.h>
#include <time.h>
#include <sys/time.h>
#include <sys/times.h>
#include <errno.h>

#define MIN(a,b) ((a)<(b) ? (a): (b))

/* FIXME: errno handling needs cleaning up.  */
void seterrno (const char *,int line);
#define __seterrno() seterrno (__FILE__, __LINE__)
#undef errno
#define errno dont_use_this_since_were_in_a_shared library
#define set_errno(val) (_impure_ptr->_errno = (val))
#define get_errno()  (_impure_ptr->_errno)

#define NOT_OPEN_FD(fd) u->self->hmap.not_open (fd)

#define alloca(x) __builtin_alloca (x)


#define DEFAULT_GID 100
#define DEFAULT_UID 500

/* FIXME: We wouldn't need most of these, since we include the headers above.
   Find out why.  Perhaps to keep them all in one place.  */
/* FIXME: This list should be sorted.  */

clock_t times (struct tms * buf);
clock_t _times (struct tms * buf);
int utimes (const char *path, struct timeval *tvp);
int settimeofday (const struct timeval * ,  const  struct timezone *);
int gettimeofday (struct timeval *p, struct timezone *z);
int _gettimeofday (struct timeval *p, struct timezone *z);
char * timezone ();
char *getwd (char *);
int _open (const char *path, int flags,...);
int _close (int id);
off_t _lseek (int fd, off_t pos, int dir);
int ioctl (int id, int cmd , void *);
int _read (int fd, void *ptr, size_t len);
int _write (int fd, const void *ptr, size_t len);
void * _sbrk (size_t incr);
int _stat (const char *name, struct stat *buf);
int _fstat (int fd, struct stat *buf);
struct passwd *getpwent (void);
void endpwent (void);
struct passwd * getpwuid (uid_t uid);
size_t getpagesize ();
int _setmode (int, int);
int setmode (int, int);
int ftruncate (int, size_t);
int symlink (const char *oldpath, const char *newpath);
int readlink (const char *, char *, int);
int gettimeofday (struct timeval *p, struct timezone *z);
char *cuserid (char *src);
struct tm * localtime (const time_t * tim_p);
void export_free (void *p);
void *export_malloc (int size);
void *export_realloc (void *p, int size);

#define STD_RBITS S_IRUSR | S_IRGRP | S_IROTH
#define STD_WBITS S_IWUSR
#define STD_XBITS S_IXUSR | S_IXGRP | S_IXOTH

#define O_NOSYMLINK 0x80000


#ifdef __cplusplus
}
#endif
