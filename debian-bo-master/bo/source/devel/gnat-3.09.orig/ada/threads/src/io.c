/* Copyright (C) 1992, the Florida State University
   Distributed by the Florida State University under the terms of the
   GNU Library General Public License.

This file is part of Pthreads.

Pthreads is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation (version 2).

Pthreads is distributed "AS IS" in the hope that it will be
useful, but WITHOUT ANY WARRANTY; without even the implied
warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with Pthreads; see the file COPYING.  If not, write
to the Free Software Foundation, 675 Mass Ave, Cambridge,
MA 02139, USA.

Report problems and direct all questions to:

  pthreads-bugs@ada.cs.fsu.edu

  @(#)io.c	2.5 4/12/95
  
*/

#ifdef IO

#include "pthread_internals.h"
#include <sys/types.h>
#include <sys/uio.h>
#include <fcntl.h>

#if defined(STACK_CHECK) && defined(SIGNAL_STACK)
/*
 * aioread/write may cause a stack overflow in the UNIX kernel which cannot
 * be caught by sighandler. This seems to be a bug in SunOS. We get
 * around this problem by deliberately trying to access a storage
 * location on stack about a page ahead of where we are. This will
 * cause a premature stack overflow (SIGBUS) which *can* be caught
 * by sighandler.
 */
  extern int pthread_page_size;
#define ACCESS_STACK \
  MACRO_BEGIN \
    if (*(int *) (pthread_get_sp() - pthread_page_size)) \
      ; \
  MACRO_END

#else !STACK_CHECK || !SIGNAL_STACK
#define ACCESS_STACK
#endif !STACK_CHECK || !SIGNAL_STACK

/*------------------------------------------------------------*/
/*
 * read - Same as POSIX.1 read except that it blocks only the current thread
 * rather than entire process.
 */
int READ(fd, buf, nbytes)
     int fd;
     void *buf;
     IO_SIZE_T nbytes;
{
  int mode;
  struct iovec iov[1];
  pthread_t p;
  struct timeval timeout;


  /*
   * If the mode is O_NDELAY perform a Non Blocking read & return immediately.
   */
  if ((mode = fcntl(fd, F_GETFL, 0)) & (O_NDELAY|O_NONBLOCK)) {
    iov[0].iov_base = buf;
    iov[0].iov_len = nbytes;
    return(readv(fd, iov, 1));
  }

  ACCESS_STACK;

  /*
   * Else issue an asynchronous request for nbytes.
   */
  timeout.tv_sec        = 0;
  timeout.tv_usec       = 0;
  p = mac_pthread_self();
  SET_KERNEL_FLAG;
  p->resultp.aio_return = AIO_INPROGRESS;

  if (aioread(fd, buf, nbytes, 0l, SEEK_CUR,
	      (struct aio_result_t *) &p->resultp) < 0) {
    CLEAR_KERNEL_FLAG;
    return(-1);
  }

  if (p->resultp.aio_return != AIO_INPROGRESS) {
    if (p->resultp.aio_return != -1)
      lseek(fd, p->resultp.aio_return, SEEK_CUR);
    else
      set_errno (p->resultp.aio_errno);
    p->state |= T_IO_OVER;
    CLEAR_KERNEL_FLAG;
    aiowait(&timeout);
    return(p->resultp.aio_return);
  }
  sigaddset(&p->sigwaitset, AIO_SIG);
  p->state &= ~T_RUNNING;
  p->state |= T_BLOCKED | T_INTR_POINT;
  if (sigismember(&p->pending, SIGCANCEL) && !sigismember(&p->mask, SIGCANCEL))
    SIG_CLEAR_KERNEL_FLAG(TRUE);
  else {
    pthread_q_deq_head(&ready, PRIMARY_QUEUE);
    SIM_SYSCALL(TRUE);
    CLEAR_KERNEL_FLAG;
  }

  /*
   * The case when the read() is interrupted (when the thread receives a
   * signal other than SIGIO), read() returns -1 and errno is set to EINTR
   * (this is done in signal.c when the thread is woken up).
   */
  switch (p->resultp.aio_return) {
  case AIO_INPROGRESS:
    aiocancel((struct aio_result_t *) &p->resultp);
    return (-1);
  case -1:
    aiowait(&timeout);
    set_errno (p->resultp.aio_errno);
    return (-1);
  default:
    aiowait(&timeout);
    lseek(fd, p->resultp.aio_return, SEEK_CUR);
    return(p->resultp.aio_return);
  }
}

/*------------------------------------------------------------*/
/*
 * write - Same as POSIX.1 write except that it blocks only the current
 * thread rather than entire process.
 */
int WRITE(fd, buf, nbytes)
     int fd;
     const void *buf;
     IO_SIZE_T nbytes;
{

  int mode;
  struct iovec iov[1];
  pthread_t p;
  struct timeval timeout;

  /*
   * If the mode is O_NDELAY perform a Non Blocking write & return immediately.
   */
  if ((mode = fcntl(fd, F_GETFL, 0)) & (O_NDELAY|O_NONBLOCK)) {
    iov[0].iov_base = (caddr_t) buf;
    iov[0].iov_len = nbytes;
    return (writev(fd, iov, 1));
  }

  ACCESS_STACK;

  /*
   * Else issue an asynchronous request for nbytes.
   */
  timeout.tv_sec        = 0;
  timeout.tv_usec       = 0;
  p = mac_pthread_self();
  SET_KERNEL_FLAG;
  p->resultp.aio_return = AIO_INPROGRESS;

  if (aiowrite(fd, (char *) buf, nbytes, 0l, SEEK_CUR,
	       (struct aio_result_t *) &p->resultp) < 0) {
    CLEAR_KERNEL_FLAG;
    return(-1);
  }

  if (p->resultp.aio_return != AIO_INPROGRESS) {
    if (p->resultp.aio_return != -1)
      lseek(fd, p->resultp.aio_return, SEEK_CUR);
    else
      set_errno(p->resultp.aio_errno);
    p->state |= T_IO_OVER;
    CLEAR_KERNEL_FLAG;
    aiowait(&timeout);
    return(p->resultp.aio_return);
  }
  sigaddset(&p->sigwaitset, AIO_SIG);
  p->state &= ~T_RUNNING;
  p->state |= T_BLOCKED | T_INTR_POINT; 
  if (sigismember(&p->pending, SIGCANCEL) &&
      !sigismember(&p->mask, SIGCANCEL))
    SIG_CLEAR_KERNEL_FLAG(TRUE);
  else {
    pthread_q_deq_head(&ready, PRIMARY_QUEUE);
    SIM_SYSCALL(TRUE);
    CLEAR_KERNEL_FLAG;
  }
  switch (p->resultp.aio_return) {
  case AIO_INPROGRESS:
    aiocancel((struct aio_result_t *) &p->resultp);
    return(-1);
  case -1:
    aiowait(&timeout);
    set_errno (p->resultp.aio_errno);
    return(-1);
  default:
    aiowait(&timeout);
    lseek(fd, p->resultp.aio_return, SEEK_CUR);
    return(p->resultp.aio_return);
  }
}

#ifndef SVR4
/*------------------------------------------------------------*/
/*
 * accept - Same as BSD accept except that it blocks only the current thread
 * rather than entire process.
 */
int accept(s, addr, addrlen)
     int s;
     struct sockaddr *addr;
     int *addrlen;
{
  int mode;
  struct iovec iov[1];
  pthread_t p;
  struct timeval timeout;
  int result;

  /*
   * If the mode is O_NDELAY perform a Non Blocking accept & return 
   * immediately.
   */
  if ((mode = fcntl(s, F_GETFL, 0)) & (O_NDELAY|O_NONBLOCK)) {
    return (ACCEPT(s, addr, addrlen));
  }

  ACCESS_STACK;

  /*
   * Else issue an asynchronous request
   */

  mode = fcntl(s, F_GETFL, 0);
  if (fcntl(s, F_SETFL, (mode|FNDELAY|FASYNC)) < 0)
    return (-1);
  if (fcntl(s, F_SETOWN, getpid()) < 0) {
    fcntl (s, F_SETFL, mode);
    return(-1);
  }

  p = mac_pthread_self();
  SET_KERNEL_FLAG; /* No preemption */

  while (TRUE) {
#ifdef DEBUG
    printf("Try to accept\n");
#endif
    result = ACCEPT(s, addr, addrlen);
    if (result != -1) {
      CLEAR_KERNEL_FLAG;
      if ((fcntl(result, F_SETFL, mode) != -1) &&
          (fcntl(s, F_SETFL, mode) != -1))
        return(result);
      return (-1);
    }
    if (errno != EWOULDBLOCK) {
      CLEAR_KERNEL_FLAG;
      fcntl(s, F_SETFL, mode);
      return(-1);
    };
#ifdef DEBUG
    printf("Unsuccessfull\n");
#endif DEBUG
    sigaddset(&p->sigwaitset, AIO_SIG);
    p->state &= ~T_RUNNING;
    p->state |= T_BLOCKED | T_INTR_POINT;
    if (sigismember(&p->pending, SIGCANCEL) &&
	!sigismember(&p->mask, SIGCANCEL))
      SIG_CLEAR_KERNEL_FLAG(TRUE);
    else {
      pthread_q_deq_head(&ready, PRIMARY_QUEUE);
      SIM_SYSCALL(TRUE);
      CLEAR_KERNEL_FLAG;
    }
  }
}

/*------------------------------------------------------------*/
/*
 * pthread_fds_set - initialization of file descriptor
 */
void pthread_fds_set(l, r)
     fd_set *l, *r;
{
  if (l)
    if (r)
      *l = *r;
    else 
      FD_ZERO(l);
}

/*------------------------------------------------------------*/
/*
 * pthread_fds_union - bit-wise union of file descriptor fields
 */
void pthread_fds_union(l, r, width)
     fd_set *l,*r;
     int width;
{
  int i;

  for (i = 0; i < howmany(width, NFDBITS); i++)
    (*l).fds_bits [i] = (*l).fds_bits [i] | (*r).fds_bits [i];
}

/*------------------------------------------------------------*/
/*
 * select - Same as BSD select except that it blocks only the current thread
 * rather than entire process.
 */
int select(width, readfds, writefds, exceptfds, timeout)
     int width;
     fd_set *readfds, *writefds, *exceptfds;
     struct timeval *timeout;
{
  static struct timeval mintimeout = {0, 0};
  pthread_t p;
  int result;
  struct timespec p_timeout;
  
  ACCESS_STACK;
  
  p = mac_pthread_self();
  SET_KERNEL_FLAG;
  
  pthread_fds_set(&(p->readfds), readfds);
  pthread_fds_set(&(p->writefds), writefds);
  pthread_fds_set(&(p->exceptfds), exceptfds); 
  
  result = SELECT(width, readfds, writefds, exceptfds, &mintimeout);
  if (result != 0) {
    CLEAR_KERNEL_FLAG;
    return result;
  }
  
  p->width = width;
  gwidth = MAX(gwidth, width);
  pthread_fds_union(&greadfds, &(p->readfds), width);
  pthread_fds_union(&gwritefds, &(p->writefds), width);
  pthread_fds_union(&gexceptfds, &(p->exceptfds), width);
  p->wait_on_select = TRUE;
  
  if (timeout && timerisset(timeout)) {
    U2P_TIME(p_timeout, *timeout);
    if (pthread_timed_sigwait(p, p_timeout, REL_TIME, NULL, NULL) == -1) {
      CLEAR_KERNEL_FLAG;
      return(-1);
    }
  }
  sigaddset(&p->sigwaitset, AIO_SIG);
  p->state &= ~T_RUNNING;
  p->state |= T_BLOCKED | T_INTR_POINT;
  if (sigismember(&p->pending, SIGCANCEL) &&
      !sigismember(&p->mask, SIGCANCEL))
    SIG_CLEAR_KERNEL_FLAG(TRUE);
  else {
    pthread_q_deq_head(&ready, PRIMARY_QUEUE);
    SIM_SYSCALL(TRUE);
    CLEAR_KERNEL_FLAG;
  }

  if (p->wait_on_select)
    return(0);

  pthread_fds_set(readfds, &(p->readfds));  
  pthread_fds_set(writefds, &(p->writefds)); 
  pthread_fds_set(exceptfds, &(p->exceptfds));
  return(p->how_many);
}
#endif !SVR4

/*------------------------------------------------------------*/
#if defined(STACK_CHECK) && defined(SIGNAL_STACK)
/*
 * pthread_io_end - dummy function used to do pc mapping to check
 *                  stack_overflow.
 */
void pthread_io_end()
{
  return;
}
/*------------------------------------------------------------*/
#endif STACK_CHECK && SIGNAL_STACK
#endif IO
