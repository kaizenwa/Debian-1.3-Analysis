/* ==== machdep.c ============================================================
 * Copyright (c) 1993, 1994 Chris Provenzano, proven@athena.mit.edu
 *
 * Description : Machine dependent functions for Linux-1.0 on m68k
 *
 *	1.00 93/08/04 proven
 *      -Started coding this file.
 *	Ported to m68k by Andreas Schwab <schwab@issan.informatik.uni-dortmund.de>
 */

#ifndef lint
static const char rcsid[] = "machdep.c,v 1.1 1995/08/09 02:59:24 hjl Exp";
#endif

#include <errno.h>
#include "pthread.h"
#include <sys/types.h>
#include <sys/param.h> /* for OPEN_MAX */
#include <sys/socketcall.h>

extern void context_switch_done(void);
extern void pthread_sched_resume(void);
extern int machdep_sys_socketcall(int, int *);


/* ==========================================================================
 * machdep_save_state()
 */
int machdep_save_state(void)
{
    return(_setjmp(pthread_run->machdep_data.machdep_state));
}

/* ==========================================================================
 * machdep_restore_state()
 */
void machdep_restore_state(void)
{
    longjmp(pthread_run->machdep_data.machdep_state, 1);
}

/* ==========================================================================
 * machdep_save_float_state()
 */
void machdep_save_float_state(struct pthread * pthread)
{
	char * fdata = (char *)pthread->machdep_data.machdep_float_state;

	__asm__ ("fmovemx %/fp0-%/fp7,%0" :: "m" (*fdata));
}

/* ==========================================================================
 * machdep_restore_float_state()
 */
void machdep_restore_float_state(void)
{
	char * fdata = (char *)pthread_run->machdep_data.machdep_float_state;

	__asm__ ("fmovemx %0,%/fp0-%/fp7" :: "m" (*fdata));
}

/* ==========================================================================
 * machdep_set_thread_timer()
 */
void machdep_set_thread_timer(struct machdep_pthread *machdep_pthread)
{
    if (setitimer(ITIMER_VIRTUAL, &(machdep_pthread->machdep_timer), NULL)) {
        PANIC();
    }
}

/* ==========================================================================
 * machdep_unset_thread_timer()
 */
void machdep_unset_thread_timer(struct machdep_pthread *machdep_pthread)
{
    struct itimerval zeroval = { { 0, 0 }, { 0, 0} };

    if (setitimer(ITIMER_VIRTUAL, &zeroval, NULL)) {
        PANIC();
    }
}

/* ==========================================================================
 * machdep_pthread_cleanup()
 */
void *machdep_pthread_cleanup(struct machdep_pthread *machdep_pthread)
{
    return(machdep_pthread->machdep_stack);
}

/* ==========================================================================
 * machdep_pthread_start()
 */
void machdep_pthread_start(void)
{
    context_switch_done();
    pthread_sched_resume();

    /* Run current threads start routine with argument */
    pthread_exit(pthread_run->machdep_data.start_routine
		 (pthread_run->machdep_data.start_argument));

    /* should never reach here */
    PANIC();
}

/* ==========================================================================
 * machdep_pthread_create()
 */
void machdep_pthread_create(struct machdep_pthread *machdep_pthread,
			    void *(* start_routine)(void *),
			    void *start_argument, long stack_size,
			    void *stack_start, long nsec)
{
    machdep_pthread->machdep_stack = stack_start;

    machdep_pthread->start_routine = start_routine;
    machdep_pthread->start_argument = start_argument;

    machdep_pthread->machdep_timer.it_value.tv_sec = 0;
    machdep_pthread->machdep_timer.it_interval.tv_sec = 0;
    machdep_pthread->machdep_timer.it_interval.tv_usec = 0;
    machdep_pthread->machdep_timer.it_value.tv_usec = nsec / 1000;

    setjmp(machdep_pthread->machdep_state);
    /*
     * Set up new stack frame so that it looks like it
     * returned from a longjmp() to the beginning of
     * machdep_pthread_start().
     */
    machdep_pthread->machdep_state->__aregs[0] = (void *)machdep_pthread_start;

    /* Stack starts high and builds down. */
    machdep_pthread->machdep_state->__sp =
      (void *) ((char *)machdep_pthread->machdep_stack + stack_size);
}

/* ==========================================================================
 * Linux Socket calls are a bit different
 * ==========================================================================
 * machdep_sys_socket()
 */
int machdep_sys_socket(int a, int b, int c)
{
	int array[3];

	array[0] = (int)a;
	array[1] = (int)b;
	array[2] = (int)c;

	return(machdep_sys_socketcall(SYS_SOCKET, array));
}

/* ==========================================================================
 * machdep_sys_accept()
 */
int machdep_sys_accept(int a, struct sockaddr * b, int * c)
{
	int array[3];

	array[0] = (int)a;
	array[1] = (int)b;
	array[2] = (int)c;

	return(machdep_sys_socketcall(SYS_ACCEPT, array));
}

/* ==========================================================================
 * machdep_sys_bind()
 */
int machdep_sys_bind(int a, const struct sockaddr * b, int c)
{
	int array[3];

	array[0] = (int)a;
	array[1] = (int)b;
	array[2] = (int)c;

	return(machdep_sys_socketcall(SYS_BIND, array));
}

/* ==========================================================================
 * machdep_sys_connect()
 */
int machdep_sys_connect(int a, const struct sockaddr * b, int c)
{
	int array[3];

	array[0] = (int)a;
	array[1] = (int)b;
	array[2] = (int)c;

	return(machdep_sys_socketcall(SYS_CONNECT, array));
}

/* ==========================================================================
 * machdep_sys_listen()
 */
int machdep_sys_listen(int a, int b)
{
	int array[2];

	array[0] = (int)a;
	array[1] = (int)b;

	return(machdep_sys_socketcall(SYS_LISTEN, array));
}

/* ==========================================================================
 * machdep_sys_shutdown()
 */
int machdep_sys_shutdown(int a, int b)
{
    int array[2];

    array[0] = (int)a;
    array[1] = (int)b;

    return(machdep_sys_socketcall(SYS_GETSOCKOPT, array));
}

/* ==========================================================================
 * machdep_sys_getsockopt()
 */
int machdep_sys_getsockopt(int a, int b, int c, char *d, int *e)
{
	int array[5];

	array[0] = (int)a;
	array[1] = (int)b;
	array[2] = (int)c;
	array[3] = (int)d;
	array[4] = (int)e;

	return(machdep_sys_socketcall(SYS_GETSOCKOPT, array));
}

/* ==========================================================================
 * machdep_sys_setsockopt()
 */
int machdep_sys_setsockopt(int a, int b, int c, char *d, int e)
{
	int array[5];

	array[0] = (int)a;
	array[1] = (int)b;
	array[2] = (int)c;
	array[3] = (int)d;
	array[4] = (int)e;

	return(machdep_sys_socketcall(SYS_SETSOCKOPT, array));
}

/* ==========================================================================
 * machdep_sys_getpeername()
 */
int machdep_sys_getpeername(int a, struct sockaddr *b, int *c)
{
	int array[3];

	array[0] = (int)a;
	array[1] = (int)b;
	array[2] = (int)c;

	return(machdep_sys_socketcall(SYS_GETPEERNAME, array));
}

/* ==========================================================================
 * machdep_sys_send()
 */
int machdep_sys_send(int a, char *b, int c, int d)
{
	int array[4];

	array[0] = (int)a;
	array[1] = (int)b;
	array[2] = (int)c;
	array[3] = (int)d;

	return(machdep_sys_socketcall(SYS_SEND, array));
}

/* ==========================================================================
 * machdep_sys_sendto()
 */
int machdep_sys_sendto(int a, char *b, int c, int d, struct sockaddr *e, int f)
{
	int array[6];

	array[0] = (int)a;
	array[1] = (int)b;
	array[2] = (int)c;
	array[3] = (int)d;
	array[4] = (int)e;
	array[5] = (int)f;

	return(machdep_sys_socketcall(SYS_SENDTO, array));
}

/* ==========================================================================
 * machdep_sys_recv()
 */
int machdep_sys_recv(int a, char *b, int c, int d)
{
	int array[4];

	array[0] = (int)a;
	array[1] = (int)b;
	array[2] = (int)c;
	array[3] = (int)d;

	return(machdep_sys_socketcall(SYS_RECV, array));
}

/* ==========================================================================
 * machdep_sys_recvfrom()
 */
int machdep_sys_recvfrom(int a, char *b, int c, int d,
			 struct sockaddr *e, int *f)
{
	int array[6];

	array[0] = (int)a;
	array[1] = (int)b;
	array[2] = (int)c;
	array[3] = (int)d;
	array[4] = (int)e;
	array[5] = (int)f;

	return(machdep_sys_socketcall(SYS_RECVFROM, array));
}

/* ==========================================================================
 * machdep_sys_readv()
 */
int machdep_sys_readv(int a, struct iovec * b, int c)
{
	return(-ENOSYS);
}

/* ==========================================================================
 * machdep_sys_sendmsg()
 */
int machdep_sys_sendmsg(int a, char * b, int c)
{
	return(-ENOSYS);
}

/* ==========================================================================
 * machdep_sys_writev()
 */
int machdep_sys_writev(int a, struct iovec * b, int c)
{
	return(-ENOSYS);
}

/* ==========================================================================
 * machdep_sys_recvmsg()
 */
int machdep_sys_recvmsg(int a, char * b, int c)
{
	return(-ENOSYS);
}

#if 0
/* ==========================================================================
 * machdep_sys_getdirentries()
 */
int machdep_sys_getdirentries(int fd, char * buf, int len, int * seek)
{
	int ret;

	if ((ret = machdep_sys_readdir(fd, buf, 1)) >= OK) {
		return 1;
	}
	return(ret);
}
#endif

/* ==========================================================================
 * machdep_sys_wait3()
 */
int machdep_sys_wait3(int * b, int c, int * d)
{
        return(machdep_sys_wait4(0, b, c, d));
}
 
/* ==========================================================================
 * machdep_sys_waitpid()
 */
int machdep_sys_waitpid(int a, int * b, int c)
{
        return(machdep_sys_wait4(a, b, c, NULL));
}  

/* getdtablesize */
int machdep_sys_getdtablesize ()
{
	return OPEN_MAX;
}

 
