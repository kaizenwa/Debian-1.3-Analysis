#include <libc-lock.h>

#pragma weak pthread_cond_broadcast = __pthread_0
#pragma weak pthread_cond_destroy = __pthread_0
#pragma weak pthread_cond_init = __pthread_0
#pragma weak pthread_cond_signal = __pthread_0
#pragma weak pthread_cond_wait = __pthread_0
#pragma weak pthread_equal = __pthread_1
#pragma weak pthread_mutex_destroy =__pthread_0
#pragma weak pthread_mutex_init =__pthread_0
#pragma weak pthread_mutex_lock = __pthread_0
#pragma weak pthread_mutex_unlock =__pthread_0
#pragma weak pthread_self = __pthread_0
#pragma weak ftrylockfile = __pthread_0

int __pthread_1 ( void );
int __pthread_0 ( void );
void __pthread_void ( void );

int
__pthread_1 ()
{
  return 1;
}

int
__pthread_0 ()
{
  return 0;
}

#pragma weak flockfile = __pthread_void
#pragma weak funlockfile = __pthread_void
#pragma weak pthread_yield = __pthread_void
#pragma weak pthread_once = __pthread_void

void __pthread_void ( void );

void
__pthread_void ()
{
  return;
}

/* It should never be changed. */
const int __pthreaded = 0;

#ifdef __GNUC__
void __pthread_void ( void ) __attribute__ ((weak));
#else
#pragma weak __pthreaded
#endif

/* libio lock. */
__libc_lock_define_initialized(, __libc_libio_lock);

#if 0
/* malloc lock. */
__libc_lock_define_initialized(, __libc_malloc_lock);
#endif

/* localtime lock. */
__libc_lock_define_initialized(, __libc_localtime_lock);

/* gmtime lock. */
__libc_lock_define_initialized(, __libc_gmtime_lock);

#if 0
#pragma weak __pthread_mutex_malloc = __pthread_void_ptr

/* It should return a malloced mutex. */
void *
__pthread_void_ptr ()
{
  return (void *)&__pthreaded;
}

#pragma weak __pthread_mutex_free = __pthread_void
#endif
