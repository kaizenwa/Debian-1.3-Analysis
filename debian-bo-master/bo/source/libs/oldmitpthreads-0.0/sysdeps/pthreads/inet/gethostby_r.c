#include <pthread.h>
#include <netdb.h>
#include <string.h>

static pthread_mutex_t gethostby_mutex = PTHREAD_MUTEX_INITIALIZER;

static int
convert (struct hostent *host, struct hostent *result,
	char *buf, int buflen, int *h_errnop)
{
  int len, i;

  if (!buf || !h_errnop) return -1;
  *h_errnop = h_errno;

  *result = *host;
  result->h_name = (char *) buf;
  /* This is the size. */
  len = strlen (host->h_name) + 1;
  if (len > buflen) return -1;
  buflen -= len;
  buf += len;
  strcpy (result->h_name, host->h_name);

  /* How many aliases and how big the buffer should be? There
     is always a NULL pointer. */
  for (len = sizeof (char *), i = 0; host->h_aliases [i]; i++)
  {
    /* It should be size of (char *) and the length of string
       plus 1. */
    len += strlen (host->h_aliases [i]) + 1 + sizeof (char *);
  }
  if (len > buflen) return -1;
  buflen -= len;

  /* This is an array of char * for h_aliases. */
  result->h_aliases = (char **) buf;
  buf += (i + 1) * sizeof (char *);

  /* We copy the aliases now. */
  for (i = 0; host->h_aliases [i]; i++)
  {
    result->h_aliases [i] = (char *) buf;
    strcpy (result->h_aliases [i], host->h_aliases [i]);
    buf += strlen (host->h_aliases [i]) + 1;
  }
  /* This is the last one */
  result->h_aliases [i] = NULL;

#if BSD >= 43 || defined(h_addr)
  for (len = sizeof (char *), i = 0; host->h_addr_list [i]; i++)
  {
    /* It should be size of (char *) and the length of string
       plus 1. */
    len += strlen (host->h_addr_list [i]) + 1 + sizeof (char *);
  }
  if (len > buflen) return -1;

  /* This is an array of char * for h_addr_list. */
  result->h_addr_list = (char **) buf;
  buf += (i + 1) * sizeof (char *);

  /* We copy the h_addr_list now. */
  for (i = 0; host->h_addr_list [i]; i++)
  {
    result->h_addr_list [i] = (char *) buf;
    strcpy (result->h_addr_list [i], host->h_addr_list [i]);
    len = strlen (host->h_addr_list [i]);
    buf += len + 1;
  }
  /* This is the last one */
  result->h_addr_list [i] = NULL;
#else
  len = strlen (host->h_addr) + 1 + sizeof (char *);
  if (len > buflen) return -1;

  result->h_addr = (char *) buf;
  strcpy (result->h_addr, host->h_addr);
#endif
  return 0;
}

struct hostent *
gethostbyaddr_r (const char *addr, int length, int type,
	struct hostent *result, char *buffer, int buflen,
	int *h_errnop)
{
  struct hostent *host;

  pthread_mutex_lock (&gethostby_mutex);

  host = gethostbyaddr (addr, length, type);
  if (!host ||
	convert (host, result, buffer, buflen, h_errnop) != 0)
  {
    result = NULL;
  }

  pthread_mutex_unlock (&gethostby_mutex);
  return result;
}

struct hostent *
gethostbyname_r (const char *name,
	struct hostent *result, char *buffer, int buflen,
	int *h_errnop)
{
  struct hostent *host;

  pthread_mutex_lock (&gethostby_mutex);

  host = gethostbyname (name);
  if (!host ||
	convert (host, result, buffer, buflen, h_errnop) != 0)
  {
    result = NULL;
  }

  pthread_mutex_unlock (&gethostby_mutex);
  return result;
}

struct hostent *
gethostent_r (struct hostent *result, char *buffer, int buflen,
	int *h_errnop)
{
  struct hostent *host;

  pthread_mutex_lock (&gethostby_mutex);

  host = gethostent ();
  if (!host ||
	convert (host, result, buffer, buflen, h_errnop) != 0)
  {
    result = NULL;
  }

  pthread_mutex_unlock (&gethostby_mutex);
  return result;
}
