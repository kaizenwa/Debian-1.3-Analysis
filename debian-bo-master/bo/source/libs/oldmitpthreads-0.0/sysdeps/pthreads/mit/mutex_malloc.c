#include <pthread.h>
#include <malloc.h>

pthread_mutex_t *
__pthread_mutex_malloc (const pthread_mutexattr_t *attr)
{
  pthread_mutex_t *mutex =
	(pthread_mutex_t*) malloc (sizeof (pthread_mutex_t));

  if (mutex)
  {
    pthread_mutex_init (mutex, attr);
  }
  return mutex;
}

void
__pthread_mutex_free (pthread_mutex_t *mutex)
{
  pthread_mutex_destroy(mutex);
  free (mutex);
}
