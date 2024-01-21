#ifndef _CHKRSYSCALL_H_
#define _CHKRSYSCALL_H_

#include <signal.h>

#define read(a,b,c) chkr_read(a,b,c)
#define write(a,b,c) chkr_write(a,b,c)
#define close(a) chkr_close(a)
#define lseek(a,b,c) chkr_lseek(a,b,c)
#define unlink(a) chkr_unlink(a)
#define getpid chkr_getpid
#define kill(a,b) chkr_kill(a,b)
#define open chkr_open
#define fcntl chkr_fcntl
#define access(a,b) chkr_access(a,b)
#define stat(a,b) chkr_stat(a,b)
#define lstat(a,b) chkr_lstat(a,b)
#define fstat(a,b) chkr_fstat(a,b)
#define _exit(a) chkr__exit(a)
#define link(a,b) chkr_link(a,b)
#define dup2(a,b) chkr_dup2(a,b)
#define sigprocmask(a,b,c) chkr_sigprocmask(a,b,c)
#define sigpending(a) chkr_sigpending(a)
#define sigreturn(a) chkr_sigreturn(a)
#define ftruncate(a,b) chkr_ftruncate(a,b)
#define gettimeofday(a,b) chkr_gettimeofday(a,b)
#define sysconf(a) chkr_sysconf(a)
#define mmap(a,b,c,d,e,f) chkr_mmap(a,b,c,d,e,f)
#define munmap(a,b) chkr_munmap(a,b)
#define shmctl(a,b,c) chkr_shmctl(a,b,c)

struct shmid_ds;
struct stat;
struct sigaction;
struct timeval;
struct timezone;

extern int read (int fd, PTR buf, size_t nbytes);
extern int write (int fd, const PTR buf, size_t n);
extern int close (int fd);
extern long lseek (int fd, long offset, int whence);
extern int unlink (const char *name);
extern pid_t getpid (void);
extern int kill (pid_t pid, int sig);
extern int open (const char *filename, int flags, ...);
extern int fcntl (int filedes, int cmd, ...);
extern int access (const char *name, int type);
extern int stat (const char *filename, struct stat *stat_buf);
extern int lstat (const char *filename, struct stat *stat_buf);
extern int fstat (int fd, struct stat *stat_buf);
extern int chkr_sigaction(int sig, const struct sigaction*, struct sigaction*);
extern void _exit(int status);
extern void* chkr_sbrk (int increment);
extern int link (const char *filenamne, const char *to);
extern int dup2 (int fd1, int fd2);
extern int sigprocmask(int how, const sigset_t *set, sigset_t *oldset);
extern int sigpending (sigset_t *set);
extern int ftruncate(int fd, off_t length);
extern int gettimeofday (struct timeval *tp, struct timezone *tz);
extern long sysconf(int arg);
extern caddr_t mmap(caddr_t addr, size_t len, int prot, int flags, int fd, off_t off);
extern int munmap(caddr_t addr, size_t len);
extern int shmctl(int shmid, int cmd, struct shmid_ds *buf);

#endif /* _CHKRSYSCALL_H_ */
