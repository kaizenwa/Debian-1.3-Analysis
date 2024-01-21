#ifndef _INTERNAL_LIBC_LOCK_H
#define _INTERNAL_LIBC_LOCK_H

#ifdef _POSIX_THREADS

#include "../pthread.h"
#include <gnu-stabs.h>

#define __libc_lock_define(CLASS,NAME) \
       CLASS pthread_mutex_t *NAME

#define __libc_lock_define_initialized(CLASS,NAME) \
       static pthread_mutex_t libc_##NAME \
               = PTHREAD_MUTEX_INITIALIZER; \
       CLASS pthread_mutex_t *NAME = &libc_##NAME

#define __libc_lock_lock(NAME) \
        pthread_mutex_lock (NAME)

#define __libc_lock_unlock(NAME) \
        pthread_mutex_unlock (NAME)

#endif

#endif /*  _INTERNAL_LIBC_LOCK_H */
