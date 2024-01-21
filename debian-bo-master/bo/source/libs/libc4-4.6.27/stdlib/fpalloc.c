#include <stdlib.h>
#include <memory.h>
#include <endian.h>

#ifdef IEEE_754

#ifdef __BIG_ENDIAN
#define snan_bytes             { }
#else
#define snan_bytes             { }
#endif
		     
void *
fpalloc (size_t n)
{
  void * ptr;

  ptr = malloc (n);
  if (!ptr) return ptr;
  return memtile (ptr, n, snan_bytes, sizeof (snan_bytes));
}

#elese

void *
fpalloc (size_t n)
{
  return malloc (n);
}

#endif
