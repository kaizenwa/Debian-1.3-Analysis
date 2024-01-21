/*
 * This is from "Advanced Programming in the Unix Environment"
 * by W.R. Stevens
 */

/* Our own header, to be included *after* all standard system headers */

#ifndef	__ourhdr_h
#define	__ourhdr_h

#include	<sys/types.h>	/* required for some of our prototypes */
#include	<stdio.h>		/* for convenience */
#include	<stdlib.h>		/* for convenience */
#include	<string.h>		/* for convenience */
#include	<unistd.h>		/* for convenience */

#define	MAXLINE	4096			/* max line length */

#define	FILE_MODE	(S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH)
					/* default file access permissions for new files */
#define	DIR_MODE	(FILE_MODE | S_IXUSR | S_IXGRP | S_IXOTH)
					/* default permissions for new directories */

typedef	void	Sigfunc(int);	/* for signal handlers */

					/* 4.3BSD Reno <signal.h> doesn't define SIG_ERR */
#if	defined(SIG_IGN) && !defined(SIG_ERR)
#define	SIG_ERR	((Sigfunc *)-1)
#endif

#define	min(a,b)	((a) < (b) ? (a) : (b))
#define	max(a,b)	((a) > (b) ? (a) : (b))

					/* prototypes for our own functions */
char	*path_alloc(int *);			/* {Prog pathalloc} */
int		 open_max(void);			/* {Prog openmax} */
void	 clr_fl(int, int);			/* {Prog setfl} */
void	 set_fl(int, int);			/* {Prog setfl} */
void	 pr_exit(int);				/* {Prog prexit} */
void	 pr_mask(const char *);		/* {Prog prmask} */

ssize_t	 readn(int, void *, size_t);/* {Prog readn} */
ssize_t	 writen(int, const void *, size_t);/* {Prog writen} */

int		lock_reg(int, int, int, off_t, int, off_t);
									/* {Prog lockreg} */
#define	read_lock(fd, offset, whence, len) \
			lock_reg(fd, F_SETLK, F_RDLCK, offset, whence, len)
#define	readw_lock(fd, offset, whence, len) \
			lock_reg(fd, F_SETLKW, F_RDLCK, offset, whence, len)
#define	write_lock(fd, offset, whence, len) \
			lock_reg(fd, F_SETLK, F_WRLCK, offset, whence, len)
#define	writew_lock(fd, offset, whence, len) \
			lock_reg(fd, F_SETLKW, F_WRLCK, offset, whence, len)
#define	un_lock(fd, offset, whence, len) \
			lock_reg(fd, F_SETLK, F_UNLCK, offset, whence, len)

pid_t	lock_test(int, int, off_t, int, off_t);
									/* {Prog locktest} */

#define	is_readlock(fd, offset, whence, len) \
			lock_test(fd, F_RDLCK, offset, whence, len)
#define	is_writelock(fd, offset, whence, len) \
			lock_test(fd, F_WRLCK, offset, whence, len)

void	log_msg(const char *, ...);		/* {App misc_source} */
void	log_open(const char *, int, int);
void	log_quit(const char *, ...);
void	log_ret(const char *, ...);
void	log_sys(const char *, ...);

void    daemon_start(void);

#endif	/* __ourhdr_h */
