/* ==== fd.c ============================================================
 * Copyright (c) 1993, 1994 by Chris Provenzano, proven@mit.edu
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *  This product includes software developed by Chris Provenzano.
 * 4. The name of Chris Provenzano may not be used to endorse or promote 
 *	  products derived from this software without specific prior written
 *	  permission.
 *
 * THIS SOFTWARE IS PROVIDED BY CHRIS PROVENZANO ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL CHRIS PROVENZANO BE LIABLE FOR ANY 
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR 
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT 
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF 
 * SUCH DAMAGE.
 *
 * Description : All the syscalls dealing with fds.
 *
 *  1.00 93/08/14 proven
 *      -Started coding this file.
 *
 *	1.01 93/11/13 proven
 *		-The functions readv() and writev() added.
 */

#ifndef lint
static const char rcsid[] = "fd.c,v 1.4 1995/09/11 02:26:36 hjl Exp";
#endif

#include <pthread.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/uio.h>
#include <stdarg.h>
#include <fcntl.h>
#include <errno.h>
#include <memory.h>

/*
 * These first functions really should not be called by the user.
 *
 * I really should dynamically figure out what the table size is.
 */
static pthread_mutex_t fd_table_mutex = PTHREAD_MUTEX_INITIALIZER;
static const int dtablecount = 4096/sizeof(struct fd_table_entry);
int dtablesize;

extern pthread_sched_other_resume (struct pthread * );
extern void pthread_sched_resume (void);
extern void fd_kern_init(int);
extern void fd_kern_reset(int);
extern void pthread_resched_resume (enum pthread_state);

/*
 * These don't seem to be implemented. vch
 */
extern machdep_sys_close(int);
extern int machdep_sys_ioctl(int, unsigned long, caddr_t);
extern int sleep_cancel (struct pthread * );

/*
 *  Prototypes.
 */
int                  fd_init_entry          (int);
int                  fd_check_entry         (unsigned int); 
void                 fd_init                (void);
int                  fd_allocate            (void);
void                 fd_basic_basic_unlock  (struct fd_table_entry *, int);
void                 fd_basic_unlock        (int, int);
void                 fd_unlock              (int, int);
int                  fd_basic_lock          (unsigned int, int,
                                             pthread_mutex_t *,
                                             struct timespec *);
int                  fd_lock                (unsigned int, int,
                                             struct timespec *);
struct fd_table_entry *fd_free              (int);
ssize_t              read_timedwait         (int, void *, size_t,
                                             struct timespec *);
ssize_t              read                   (int, void *, size_t);
int                  readv_timedwait        (int, const struct iovec *, int,
                                             struct timespec *);
int                  readv                  (int, const struct iovec *,
                                             size_t);
ssize_t              write_timedwait        (int, const void *, size_t,
                                             struct timespec *);
ssize_t              write                  (int, const void *, size_t);
int                  writev_timedwait       (int, const struct iovec *, int,
                                             struct timespec *);
int                  writev                 (int, const struct iovec *,
                                             size_t);
off_t                lseek                  (int, off_t, int);
int                  close                  (int);
static inline void   fd_basic_dup           (int, int);
int                  dup2                   (int, int);
int                  dup                    (int);
int                  fcntl                  (int, int, ...);
#ifdef __linux__
int                 _fxstat                 (int, int, struct stat *);
#else
int                  fstat                  (int, struct stat *);
#endif
int                  getdtablesize          (void);
int                  ioctl                  (int, unsigned long, caddr_t);

/* ==========================================================================
 * Allocate dtablecount entries at once and populate the fd_table.
 *
 * fd_init_entry()
 */
int fd_init_entry(int entry)
{
	struct fd_table_entry *fd_entry;
	int i, round;

	if (fd_table[entry] == NULL) {
		round = entry - entry % dtablecount;

		if ((fd_entry = (struct fd_table_entry *)malloc(
		  sizeof(struct fd_table_entry) * dtablecount)) == NULL) {
			return(NOTOK);
		}
		
		for (i = 0; i < dtablecount; i++) {
			fd_table[round + i] = &fd_entry[i];

			fd_table[round + i]->ops 	= NULL;
			fd_table[round + i]->type 	= FD_NT;
			fd_table[round + i]->fd.i 	= NOTOK;
			fd_table[round + i]->flags 	= 0;
			fd_table[round + i]->count 	= 0;

			pthread_mutex_init(&(fd_table[round + i]->mutex), NULL);
			pthread_queue_init(&(fd_table[round + i]->r_queue));
			pthread_queue_init(&(fd_table[round + i]->w_queue));
			fd_table[round + i]->r_owner 	= NULL;
			fd_table[round + i]->w_owner 	= NULL;
			fd_table[round + i]->r_lockcount= 0;
			fd_table[round + i]->w_lockcount= 0;

			fd_table[round + i]->next 		= NULL;
		}
	}
	return(OK);
}

/* ==========================================================================
 * fd_check_entry()
 */
int fd_check_entry(unsigned int entry) 
{
	int ret = OK;

	pthread_mutex_lock(&fd_table_mutex);

	if (entry < dtablesize) { 
		if (fd_table[entry] == NULL) {
			if (fd_init_entry(entry)) {
				SET_ERRNO(EBADF);
				ret = -EBADF;
			}
		}
	} else {
		SET_ERRNO(EBADF);
		ret = -EBADF;
	}

	pthread_mutex_unlock(&fd_table_mutex);
	return(ret);
}

/* ==========================================================================
 * fd_init()
 */
void fd_init(void)
{
  if ((dtablesize = machdep_sys_getdtablesize()) < 0) {
    /* Can't figure out the table size. */
    PANIC();
  }

  /* This is again temporary and can be bumped up if necessary. */
  if (dtablesize > 1024) {
    dtablesize = 1024;
  }

  if ((fd_table = (struct fd_table_entry **)
       malloc(sizeof(struct fd_table_entry) * dtablesize))) {
    memset(fd_table, 0, sizeof(struct fd_table_entry) * dtablesize);
    if (fd_check_entry(0) == OK) {
      return;
    }
  }

  /*
   * There isn't enough memory to allocate a fd table at init time.
   * This is a problem.
   */
  PANIC();
}

/* ==========================================================================
 * fd_allocate()
 */
int fd_allocate(void)
{
	pthread_mutex_t * mutex;
	int i;

	for (i = 0; i < dtablesize; i++) {
		if (fd_check_entry(i) == OK) {
			mutex = &(fd_table[i]->mutex);
			if (pthread_mutex_trylock(mutex)) {
				continue;
			}
			if (fd_table[i]->count || fd_table[i]->r_owner
			  || fd_table[i]->w_owner) {
				pthread_mutex_unlock(mutex);
				continue;
			}
			if (fd_table[i]->type == FD_NT) {
				/* Test to see if the kernel version is in use */
				if ((machdep_sys_fcntl(i, F_GETFL, (int)NULL)) >= OK) {
					/* If so continue; */
					pthread_mutex_unlock(mutex);
					continue;
				}
			}
			fd_table[i]->count++;
			pthread_mutex_unlock(mutex);
			return(i);
		}
	}
	SET_ERRNO(ENFILE);
	return(NOTOK);
}

/* ==========================================================================
 * fd_basic_basic_unlock()
 *
 * The real work of unlock without the locking of fd_table[fd].lock.
 */
void fd_basic_basic_unlock(struct fd_table_entry * entry, int lock_type)
{
	struct pthread *pthread;

	if (entry->r_owner == pthread_run) {
		if ((entry->type == FD_HALF_DUPLEX) ||
	      (entry->type == FD_TEST_HALF_DUPLEX) ||
		  (lock_type == FD_READ) || (lock_type == FD_RDWR)) {
			if (entry->r_lockcount == 0) {
				if ((pthread = pthread_queue_deq(&entry->r_queue))) {
					pthread_sched_prevent();
					entry->r_owner = pthread;
 					if ((SET_PF_DONE_EVENT(pthread)) == OK) {
						pthread_sched_other_resume(pthread);
					} else {
						pthread_sched_resume();
					}
				} else {
					entry->r_owner = NULL;
   	    		}
			} else {
				entry->r_lockcount--;
			}
		}
	}

	if (entry->w_owner == pthread_run) {
		if ((entry->type != FD_HALF_DUPLEX) &&
	      (entry->type != FD_TEST_HALF_DUPLEX) &&
		  ((lock_type == FD_WRITE) || (lock_type == FD_RDWR))) {
			if (entry->w_lockcount == 0) {
				if ((pthread = pthread_queue_deq(&entry->w_queue))) {
					pthread_sched_prevent();
					entry->w_owner = pthread;
 					if ((SET_PF_DONE_EVENT(pthread)) == OK) {
						pthread_sched_other_resume(pthread);
					} else {
						pthread_sched_resume();
					}
				} else {
					entry->w_owner = NULL;
        		}
			} else {
				entry->w_lockcount--;
			}
		}
	}
}

/* ==========================================================================
 * fd_basic_unlock()
 */
void fd_basic_unlock(int fd, int lock_type)
{
	fd_basic_basic_unlock(fd_table[fd], lock_type);
}

/* ==========================================================================
 * fd_unlock()
 */
void fd_unlock(int fd, int lock_type)
{
	pthread_mutex_t *mutex;

	mutex = &(fd_table[fd]->mutex);
	pthread_mutex_lock(mutex);
	fd_basic_basic_unlock(fd_table[fd], lock_type);
	pthread_mutex_unlock(mutex);
}

/* ==========================================================================
 * fd_basic_lock()
 * 
 * The real work of lock without the locking of fd_table[fd].lock.
 * Be sure to leave the lock the same way you found it. i.e. locked.
 */
int fd_basic_lock(unsigned int fd, int lock_type, pthread_mutex_t * mutex, 
  struct timespec * timeout)
{
  switch (fd_table[fd]->type) {
  case FD_NIU:
    /* If not in use return EBADF error */
    return(NOTOK);
    break;
  case FD_NT:
    /*
     * If not tested, test it and see if it is valid 
     * If not ok return EBADF error 
     */
    fd_kern_init(fd);
    if (fd_table[fd]->type == FD_NIU) {
      return(NOTOK);
    }
    break;
  case FD_TEST_HALF_DUPLEX:
  case FD_TEST_FULL_DUPLEX:
    /* If a parent process reset the fd to its proper state */
    if (!fork_lock) {
      /* It had better be a kernel fd */
      fd_kern_reset(fd);
    }
    break;
  default:
    break;
  }

  if ((fd_table[fd]->type == FD_HALF_DUPLEX) ||
      (fd_table[fd]->type == FD_TEST_HALF_DUPLEX) ||
      (lock_type == FD_READ) || (lock_type == FD_RDWR)) {
    if (fd_table[fd]->r_owner) {
      if (fd_table[fd]->r_owner != pthread_run) {
        pthread_sched_prevent();
        pthread_queue_enq(&fd_table[fd]->r_queue, pthread_run);
        SET_PF_WAIT_EVENT(pthread_run);
        pthread_mutex_unlock(mutex);

        if (timeout) {
          /* get current time */
          struct timespec current_time;
          machdep_gettimeofday(&current_time);
          sleep_schedule(&current_time, timeout);

          /* Reschedule will unlock pthread_run */
          pthread_resched_resume(PS_FDLR_WAIT);
          pthread_mutex_lock(mutex);

          /* If we're the owner then we have to cancel the sleep */
          if (fd_table[fd]->r_owner != pthread_run) {
            CLEAR_PF_DONE_EVENT(pthread_run);
            SET_ERRNO(ETIMEDOUT);
            return(NOTOK);
          }
          sleep_cancel(pthread_run);
        } else {
          /* Reschedule will unlock pthread_run */
          pthread_resched_resume(PS_FDLR_WAIT);
          pthread_mutex_lock(mutex);
        }
        CLEAR_PF_DONE_EVENT(pthread_run);
      } else {
        fd_table[fd]->r_lockcount++;
      }
    }
    fd_table[fd]->r_owner = pthread_run;
  }
  if ((fd_table[fd]->type != FD_HALF_DUPLEX) &&
      (fd_table[fd]->type != FD_TEST_HALF_DUPLEX) &&
      ((lock_type == FD_WRITE) || (lock_type == FD_RDWR))) {
    if (fd_table[fd]->w_owner) {
      if (fd_table[fd]->w_owner != pthread_run) {
        pthread_sched_prevent();
        pthread_queue_enq(&fd_table[fd]->w_queue, pthread_run);
        SET_PF_WAIT_EVENT(pthread_run);
        pthread_mutex_unlock(mutex);

        if (timeout) {
          /* get current time */
          struct timespec current_time;
          machdep_gettimeofday(&current_time);
          sleep_schedule(&current_time, timeout);

          /* Reschedule will unlock pthread_run */
          pthread_resched_resume(PS_FDLR_WAIT);
          pthread_mutex_lock(mutex);

          /* If we're the owner then we have to cancel the sleep */
          if (fd_table[fd]->w_owner != pthread_run) {
            if (lock_type == FD_RDWR) {
              /* Unlock current thread */
              fd_basic_unlock(fd, FD_READ);
            }
            CLEAR_PF_DONE_EVENT(pthread_run);
            SET_ERRNO(ETIMEDOUT);
            return(NOTOK);
          }
          sleep_cancel(pthread_run);
        } else {
          /* Reschedule will unlock pthread_run */
          pthread_resched_resume(PS_FDLR_WAIT);
          pthread_mutex_lock(mutex);
        }
        CLEAR_PF_DONE_EVENT(pthread_run);
      } else {
        fd_table[fd]->w_lockcount++;
      }
    }
    fd_table[fd]->w_owner = pthread_run;
  }
  if (!fd_table[fd]->count) {
    fd_basic_unlock(fd, lock_type);
    return(NOTOK);
  }
  return(OK);
}

/* ==========================================================================
 * fd_lock()
 */
#define pthread_mutex_lock_timedwait(a, b) pthread_mutex_lock(a)

int fd_lock(unsigned int fd, int lock_type, struct timespec * timeout)
{
	pthread_mutex_t *mutex;
	int error;

	if ((error = fd_check_entry(fd)) == OK) {
		mutex = &(fd_table[fd]->mutex);
		if (pthread_mutex_lock_timedwait(mutex, timeout)) {
			SET_ERRNO(ETIMEDOUT);
			return(-ETIMEDOUT);
		}
		error = fd_basic_lock(fd, lock_type, mutex, timeout);
		pthread_mutex_unlock(mutex);
	}
	return(error);
}

/* ==========================================================================
 * fd_free()
 *
 * Assumes fd is locked and owner by pthread_run
 * Don't clear the queues, fd_unlock will do that.
 */
struct fd_table_entry * fd_free(int fd)
{
	struct fd_table_entry *fd_valid;

    fd_valid = NULL;
	fd_table[fd]->r_lockcount = 0;
	fd_table[fd]->w_lockcount = 0;
	if (--fd_table[fd]->count) {
		fd_valid = fd_table[fd];
		fd_table[fd] = fd_table[fd]->next;
		fd_valid->next = fd_table[fd]->next;
		/* Don't touch queues of fd_valid */
	}

	fd_table[fd]->type 	= FD_NIU;
	fd_table[fd]->fd.i 	= NOTOK;
	fd_table[fd]->next 	= NULL;
	fd_table[fd]->flags = 0;
	fd_table[fd]->count = 0;
	return(fd_valid);
}


/* ==========================================================================
 * ======================================================================= */

/* ==========================================================================
 * read_timedwait()
 */
ssize_t read_timedwait(int fd, void *buf, size_t nbytes, 
  struct timespec * timeout)
{
	int ret;

	if ((ret = fd_lock(fd, FD_READ, NULL)) == OK) {
     	ret = fd_table[fd]->ops->read(fd_table[fd]->fd,
		  fd_table[fd]->flags, buf, nbytes, timeout); 
		fd_unlock(fd, FD_READ);
	} 
	return(ret);
}

/* ==========================================================================
 * read()
 */
ssize_t read(int fd, void *buf, size_t nbytes)
{
	return(read_timedwait(fd, buf, nbytes, NULL));
}

/* ==========================================================================
 * readv_timedwait()
 */
int readv_timedwait(int fd, const struct iovec *iov, int iovcnt,
  struct timespec * timeout)
{
	int ret;

	if ((ret = fd_lock(fd, FD_READ, NULL)) == OK) {
     	ret = fd_table[fd]->ops->readv(fd_table[fd]->fd,
		  fd_table[fd]->flags, iov, iovcnt, timeout); 
		fd_unlock(fd, FD_READ);
	} 
	return(ret);
}

/* ==========================================================================
 * readv()
 */
int readv(int fd, const struct iovec *iov, size_t iovcnt)
{
	return(readv_timedwait(fd, iov, iovcnt, NULL));
}

/* ==========================================================================
 * write()
 */
ssize_t write_timedwait(int fd, const void *buf, size_t nbytes, 
  struct timespec * timeout)
{
	int ret;

	 if ((ret = fd_lock(fd, FD_WRITE, NULL)) == OK) {
     	ret = fd_table[fd]->ops->write(fd_table[fd]->fd,
		  fd_table[fd]->flags, buf, nbytes, timeout); 
        fd_unlock(fd, FD_WRITE);
    }
    return(ret);
}

/* ==========================================================================
 * write()
 */
ssize_t write(int fd, const void * buf, size_t nbytes)
{
	return(write_timedwait(fd,  buf, nbytes, NULL));
}

/* ==========================================================================
 * writev_timedwait()
 */
int writev_timedwait(int fd, const struct iovec *iov, int iovcnt,
  struct timespec * timeout)
{
	int ret;

	 if ((ret = fd_lock(fd, FD_WRITE, NULL)) == OK) {
     	ret = fd_table[fd]->ops->writev(fd_table[fd]->fd,
		  fd_table[fd]->flags, iov, iovcnt, timeout); 
        fd_unlock(fd, FD_WRITE);
    }
    return(ret);
}

/* ==========================================================================
 * writev()
 */
int writev(int fd, const struct iovec *iov, size_t iovcnt)
{
	return(writev_timedwait(fd, iov, iovcnt, NULL));
}

/* ==========================================================================
 * lseek()
 */
off_t lseek(int fd, off_t offset, int whence)
{
  int ret;

  if ((ret = fd_lock(fd, FD_RDWR, NULL)) == OK) {
    ret = fd_table[fd]->ops->seek(fd_table[fd]->fd,
                                  fd_table[fd]->flags, offset, whence); 
    fd_unlock(fd, FD_RDWR);
  }
  return(ret);
}

/* ==========================================================================
 * close()
 *
 * The whole close procedure is a bit odd and needs a bit of a rethink.
 * For now close() locks the fd, calls fd_free() which checks to see if
 * there are any other fd values poinging to the same real fd. If so
 * It breaks the wait queue into two sections those that are waiting on fd
 * and those waiting on other fd's. Those that are waiting on fd are connected
 * to the fd_table[fd] queue, and the count is set to zero, (BUT THE LOCK IS
 * NOT RELEASED). close() then calls fd_unlock which give the fd to the next
 * queued element which determins that the fd is closed and then calls
 * fd_unlock etc...
 *
 * XXX close() is even uglier now. You may assume that the kernel fd is the
 * same as fd if fd_table[fd] == NULL or if fd_table[fd]->type == FD_NT.
 * This is true because before any fd_table[fd] is allocated the corresponding
 * kernel fd must be checks to see if it's valid.
 */
int close(int fd)
{
	struct fd_table_entry * entry;
	pthread_mutex_t *mutex;
	union fd_data realfd;
	int ret, flags;

	/* Need to lock the newfd by hand */
	if (fd < dtablesize) { 
		pthread_mutex_lock(&fd_table_mutex);
		if (fd_table[fd]) {
			pthread_mutex_unlock(&fd_table_mutex);
			mutex = &(fd_table[fd]->mutex);
			pthread_mutex_lock(mutex);

			/*
 			 * XXX Gross hack ... because of fork(), any fd closed by the
			 * parent should not change the fd of the child, unless it owns it.
			 */
			switch(fd_table[fd]->type) {
			case FD_NIU:
				pthread_mutex_unlock(mutex);
				ret = -EINVAL;
				break;
			case FD_NT:	
				/* 
				 * If it's not tested then the only valid possibility is it's
				 * kernel fd.
				 */
				ret = machdep_sys_close(fd);
				fd_table[fd]->type = FD_NIU;
				pthread_mutex_unlock(mutex);
				break;
			case FD_TEST_FULL_DUPLEX:
			case FD_TEST_HALF_DUPLEX:
				realfd = fd_table[fd]->fd;
				flags = fd_table[fd]->flags;
				if ((entry = fd_free(fd)) == NULL) {
     				ret = fd_table[fd]->ops->close(realfd, flags);
				} else {
					/* There can't be any others waiting for fd. */
					pthread_mutex_unlock(&entry->mutex);
					/* Note: entry->mutex = mutex */
					mutex = &(fd_table[fd]->mutex);
				}
				pthread_mutex_unlock(mutex);
				break;
			default:
				ret = fd_basic_lock(fd, FD_RDWR, mutex, NULL);
				if (ret == OK) {
					realfd = fd_table[fd]->fd;
					flags = fd_table[fd]->flags;
					pthread_mutex_unlock(mutex);
					if ((entry = fd_free(fd)) == NULL) {
     					ret = fd_table[fd]->ops->close(realfd, flags);
					} else {
						fd_basic_basic_unlock(entry, FD_RDWR);
						pthread_mutex_unlock(&entry->mutex);
						/* Note: entry->mutex = mutex */
					}
					fd_unlock(fd, FD_RDWR);
				} else {
					pthread_mutex_unlock(mutex);
				}
				break;
			}
		} else {
			/* Don't bother creating a table entry */
			pthread_mutex_unlock(&fd_table_mutex);
			ret = machdep_sys_close(fd);
		}
		return(ret);
	}
	return(-EINVAL);
}

/* ==========================================================================
 * fd_basic_dup()
 *
 * Might need to do more than just what's below.
 */
static inline void fd_basic_dup(int fd, int newfd)
{
	fd_table[newfd]->next = fd_table[fd]->next;
	fd_table[fd]->next = fd_table[newfd];
	fd_table[newfd] = fd_table[fd];
	fd_table[fd]->count++;
}

/* ==========================================================================
 * dup2()
 *
 * Note: Always lock the lower number fd first to avoid deadlocks.
 * Note: Leave the newfd locked. It will be unlocked at close() time.
 * Note: newfd must be locked by hand so it can be closed if it is open,
 * 		 or it won't be opened while dup is in progress.
 */
int dup2( int fd, int newfd)
{
	struct fd_table_entry * entry;
	pthread_mutex_t *mutex;
	union fd_data realfd;
	int ret, flags;

	if (newfd < dtablesize) {
		if (fd < newfd) {
			if ((ret = fd_lock(fd, FD_RDWR, NULL)) == OK) {
				/* Need to lock the newfd by hand */
				mutex = &(fd_table[newfd]->mutex);
				pthread_mutex_lock(mutex);

				/* Is it inuse */
				if (fd_basic_lock(newfd, FD_RDWR, mutex, NULL) == OK) {
					realfd = fd_table[newfd]->fd;
					flags = fd_table[newfd]->flags;
					/* free it and check close status */
					if ((entry = fd_free(newfd)) == NULL) {
						entry = fd_table[newfd];
     					entry->ops->close(realfd, flags);
						if (entry->r_queue.q_next) {
							if (fd_table[fd]->next) {
						  		fd_table[fd]->r_queue.q_last->next = 
							      entry->r_queue.q_next;
							} else {
						  		fd_table[fd]->r_queue.q_next = 
							      entry->r_queue.q_next;
							}
							fd_table[fd]->r_queue.q_last = 
							  entry->r_queue.q_last;
						}
						if (entry->w_queue.q_next) {
							if (fd_table[fd]->next) {
						  		fd_table[fd]->w_queue.q_last->next = 
							      entry->w_queue.q_next;
							} else {
						  		fd_table[fd]->w_queue.q_next = 
							      entry->w_queue.q_next;
							}
							fd_table[fd]->w_queue.q_last = 
							  entry->w_queue.q_last;
						}
						entry->r_queue.q_next = NULL;
						entry->w_queue.q_next = NULL;
						entry->r_queue.q_last = NULL;
						entry->w_queue.q_last = NULL;
						entry->r_owner = NULL;
						entry->w_owner = NULL;
						ret = OK;
					} else {
						fd_basic_basic_unlock(entry, FD_RDWR);
						pthread_mutex_unlock(&entry->mutex);
						/* Note: entry->mutex = mutex */
					}
				}
				fd_basic_dup(fd, newfd);
			}
			fd_unlock(fd, FD_RDWR);
		} else {
			/* Need to lock the newfd by hand */
			mutex = &(fd_table[newfd]->mutex);
			pthread_mutex_lock(mutex);

			if ((ret = fd_lock(fd, FD_RDWR, NULL)) == OK) {
				/* Is newfd inuse */
				if ((ret = fd_basic_lock(newfd, FD_RDWR, mutex, NULL)) == OK) {
					realfd = fd_table[newfd]->fd;
					flags = fd_table[newfd]->flags;
					/* free it and check close status */
					if ((entry = fd_free(newfd)) == NULL) {
						entry = fd_table[newfd];
   		  				entry->ops->close(realfd, flags);
						if (entry->r_queue.q_next) {
							if (fd_table[fd]->next) {
						  		fd_table[fd]->r_queue.q_last->next = 
							      entry->r_queue.q_next;
							} else {
						  		fd_table[fd]->r_queue.q_next = 
							      entry->r_queue.q_next;
							}
							fd_table[fd]->r_queue.q_last = 
							  entry->r_queue.q_last;
						}
						if (entry->w_queue.q_next) {
							if (fd_table[fd]->next) {
						  		fd_table[fd]->w_queue.q_last->next = 
							      entry->w_queue.q_next;
							} else {
						  		fd_table[fd]->w_queue.q_next = 
							      entry->w_queue.q_next;
							}
							fd_table[fd]->w_queue.q_last = 
							  entry->w_queue.q_last;
						}
						entry->r_queue.q_next = NULL;
						entry->w_queue.q_next = NULL;
						entry->r_queue.q_last = NULL;
						entry->w_queue.q_last = NULL;
						entry->r_owner = NULL;
						entry->w_owner = NULL;
						ret = OK;
					} else {
						fd_basic_basic_unlock(entry, FD_RDWR);
						pthread_mutex_unlock(&entry->mutex);
						/* Note: entry->mutex = mutex */
					}
					fd_basic_dup(fd, newfd);
				}
				fd_unlock(fd, FD_RDWR);
			}
		}
	} else {
		ret = NOTOK;
	}
	return(ret);
			
}

/* ==========================================================================
 * dup()
 */
int dup(int fd)
{
	int ret;

	if ((ret = fd_lock(fd, FD_RDWR, NULL)) == OK) {
		ret = fd_allocate();
		fd_basic_dup(fd, ret);
		fd_unlock(fd, FD_RDWR);
	}
	return(ret);
}

/* ==========================================================================
 * fcntl()
 */
int fcntl(int fd, int cmd, ...)
{
	int ret, flags;
	va_list ap;

	flags = 0;
	if ((ret = fd_lock(fd, FD_RDWR, NULL)) == OK) {
		va_start(ap, cmd);
		switch(cmd) {
		case F_DUPFD:
			ret = fd_allocate();
			fd_basic_dup(va_arg(ap, int), ret);
			break;
		case F_SETFD:
			break;
		case F_GETFD:
			break;
		case F_GETFL:
			ret = fd_table[fd]->flags;
			break;
		case F_SETFL:
			flags = va_arg(ap, int);
     		if ((ret = fd_table[fd]->ops->fcntl(fd_table[fd]->fd,
			  fd_table[fd]->flags, cmd, flags | __FD_NONBLOCK)) == OK) {
				fd_table[fd]->flags = flags;
			}
			break;
/*		case F_SETLKW: */
			/*
			 * Do the same as SETLK but if it fails with EACCES or EAGAIN
			 * block the thread and try again later, not implemented yet
			 */
/*		case F_SETLK: */
/*		case F_GETLK: 
			flock = va_arg(ap, struct flock*);
     		ret = fd_table[fd]->ops->fcntl(fd_table[fd]->fd,
			  fd_table[fd]->flags, cmd, flock);
			break; */
		default:
			/* Might want to make va_arg use a union */
     		ret = fd_table[fd]->ops->fcntl(fd_table[fd]->fd,
			  fd_table[fd]->flags, cmd, va_arg(ap, void*));
			break;
		}
		va_end(ap);
		fd_unlock(fd, FD_RDWR);
	}
	return(ret);
}

/* ==========================================================================
 * fstat()
 *
 * Might want to indirect this.
 */
#ifdef __linux__
int _fxstat (int version, int fd, struct stat *buf)
{
	int ret;

	if ((ret = fd_lock(fd, FD_READ, NULL)) == OK) {
		ret = __machdep_sys__fxstat (version, 
			fd_table[fd]->fd.i, buf);
        fd_unlock(fd, FD_READ);
    }
    return(ret);
}

#else
int fstat(int fd, struct stat *buf)
{
	int ret;

	if ((ret = fd_lock(fd, FD_READ, NULL)) == OK) {
		ret = machdep_sys_fstat(fd_table[fd]->fd.i, buf);
        fd_unlock(fd, FD_READ);
    }
    return(ret);
}
#endif

/* ==========================================================================
 * getdtablesize()
 */
int getdtablesize()
{
	return dtablesize;
}

/* ==========================================================================
 * ioctl()
 *
 * Really want to do a real implementation of this that parses the args ala
 * fcntl(), above, but it will have to be a totally platform-specific,
 * nightmare-on-elm-st-style sort of thing.  Might even deserve its own file
 * ala select()... --SNL
 */
int
ioctl(int fd, unsigned long request, caddr_t arg)
{
    int ret;

	if (fd < 0 || fd >= dtablesize)
	    ret = NOTOK;
	else if (fd_table[fd]->fd.i == NOTOK)
	    ret = machdep_sys_ioctl(fd, request, arg);
	else if ((ret = fd_lock(fd, FD_RDWR, NULL)) == OK) {
	    ret = machdep_sys_ioctl(fd_table[fd]->fd.i, request, arg);
		fd_unlock(fd, FD_RDWR);
	}
	return ret;
}

#if defined(__ELF__) || defined(__GNU_LIBRARY__)
#include <gnu-stabs.h>
#ifdef elf_alias
elf_alias (close, __close);
elf_alias (dup, __dup);
elf_alias (dup2, __dup2);
elf_alias (fcntl, __fcntl);
elf_alias (ioctl, __ioctl);
elf_alias (lseek, __lseek);
elf_alias (read, __read);
elf_alias (readv, __readv);
elf_alias (write, __write);
elf_alias (writev, __writev);
#endif
#endif
