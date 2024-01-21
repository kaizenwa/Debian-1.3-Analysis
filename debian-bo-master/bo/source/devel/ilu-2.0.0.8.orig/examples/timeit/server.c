/* $Id: server.c,v 1.12 1996/05/04 05:12:45 janssen Exp $ */
/* Last edited by Mike Spreitzer April 2, 1996 9:46 pm PST */

#include <stdio.h>
#ifdef WIN32
/* dll - to pick up gethostname */
#include <winsock.h>
#include "timeit.h"
#else
#include <unistd.h>	/* for gethostname */
#include "timeit.h"
#endif /* WIN32 */

#define ILU_TEST_DOMAIN "parc.xerox.com"
ilu_boolean verbose = ilu_FALSE;

int main (int ac, char **av)
{
  extern void Test1__InitializeServer(void);
  ilu_Server s;
  timeit_p  uc;
  char *pinfo = "sunrpc";
  char *tinfo[10] = { "sunrpcrm", "tcp_0_0", ILU_NIL };
  char hostname[1000], serverid[1000];

  timeit__InitializeServer();

  if (ac > 1)
    strcpy (hostname, av[1]);
  else
    gethostname (hostname, sizeof(hostname));

  if (ac > 2)
    verbose = ilu_TRUE;

  sprintf (serverid, "TimingTest.%s.%s", hostname, ILU_TEST_DOMAIN);
  s = ILU_C_InitializeServer (serverid, ILU_NIL, pinfo, tinfo, ILU_NIL, ilu_TRUE);
  uc = timeit_p__CreateTrue ( "0", s, ILU_NIL );
  ILU_C_PublishObject (uc);

  if (uc != ILU_NIL)
    {
      printf ("exported %s\n", ILU_C_SBHOfObject(uc));
      ILU_C_Run( );
    }
  else
    {
      printf ("couldn't create object\n");
      exit(1);
    }
  return 1;
}

ilu_cardinal
  server_timeit_p_ping1 (timeit_p h, CORBA_unsigned_long p1, ILU_C_ENVIRONMENT *e)
{
  if (verbose)
    printf ("timeit_p_ping1(%lu) => %lu\n", p1, p1);
  return (p1);
}

ilu_real
  server_timeit_p_ping2 (timeit_p h, CORBA_double p1, ILU_C_ENVIRONMENT *e)
{
  if (verbose)
    printf ("timeit_p_ping2(%f) => %f\n", p1, p1);
  return (p1);
}

ilu_CString
  server_timeit_p_ping3 (timeit_p h, ilu_CString p1, ILU_C_ENVIRONMENT *e)
{
  ilu_cardinal    len;
  ilu_CString     ans;
  if (verbose)
    printf("timeit_p_ping3(\"%s\") => \"%s\"\n", p1, p1);
  len = strlen(p1);
  ans = (ilu_CString) ilu_malloc(len + 1);
  if (ans != ILU_NIL)
    strcpy(ans, p1);
  return (ans);
}

static timeit_pageimg pageimg_Create (ilu_cardinal size)
{
  timeit_pageimg *p = timeit_pageimg__alloc();
  p->_maximum = size;
  p->_length = size;
  p->_buffer = CORBA_sequence_octet_bufalloc(size);
  return *p;
}

timeit_pageSeq *
server_timeit_p_doctest(timeit_p h, ilu_CString docname,
			CORBA_unsigned_long pagecount,
			CORBA_unsigned_long pagesize,
			ILU_C_ENVIRONMENT * e)
{
  timeit_pageSeq *p = timeit_pageSeq__alloc();
  ilu_cardinal    i;

  timeit_pageSeq_Init(p, pagecount, ILU_NIL);

  for (i = 0; i < pagecount; i++)
    p->_buffer[i] = pageimg_Create(pagesize);
  p->_length = pagecount;

  if (verbose)
    printf("timeit_p_doctest(\"%s\", %u, %u) => %p\n",
	   docname, pagecount, pagesize, p);

  return (p);
}

