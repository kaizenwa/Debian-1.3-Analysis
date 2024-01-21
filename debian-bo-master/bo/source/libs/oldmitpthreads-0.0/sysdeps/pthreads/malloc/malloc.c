#include <malloc.h>
#include <libc-lock.h>

__libc_lock_define (extern, __libc_malloc_lock);

extern void __libc_free (void *);
extern void * __libc_malloc (size_t);
extern void * __libc_calloc (size_t, size_t);
extern void * __libc_valloc (size_t);
extern void * __libc_memalign (size_t, size_t);
extern void * __libc_realloc (void *, size_t);

void
free (void *ptr)
{
  if (ptr == NULL) return;

  __libc_lock_lock(__libc_malloc_lock);
  __libc_free (ptr);
  __libc_lock_unlock(__libc_malloc_lock);
}

void *
malloc (size_t size)
{
  void * ret;
  __libc_lock_lock(__libc_malloc_lock);
  ret = __libc_malloc (size);
  __libc_lock_unlock(__libc_malloc_lock);
  return ret;
}

void *
valloc (size_t size)
{
  void * ret;
  __libc_lock_lock(__libc_malloc_lock);
  ret = __libc_valloc (size);
  __libc_lock_unlock(__libc_malloc_lock);
  return ret;
}

void *
calloc (size_t nmemb, size_t size)
{
  void * ret;
  __libc_lock_lock(__libc_malloc_lock);
  ret = __libc_calloc (nmemb, size);
  __libc_lock_unlock(__libc_malloc_lock);
  return ret;
}

void *
memalign (size_t alignment, size_t size)
{
  void * ret;
  __libc_lock_lock(__libc_malloc_lock);
  ret = __libc_memalign (alignment, size);
  __libc_lock_unlock(__libc_malloc_lock);
  return ret;
}

void *
realloc (void *ptr, size_t size)
{
  void * ret;
  __libc_lock_lock(__libc_malloc_lock);
  ret = __libc_realloc (ptr, size);
  __libc_lock_unlock(__libc_malloc_lock);
  return ret;
}
