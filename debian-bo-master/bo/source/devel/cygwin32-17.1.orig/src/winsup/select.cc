/* select for WIN32.

   Written by Steve Chamberlain of Cygnus Support.
   sac@cygnus.com

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#define  __INSIDE_CYGWIN_NET__
#include "winsup.h"
#include <sys/socket.h>
#include <netdb.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

#define __INSIDE_CYGWIN32__
#include <mywinsock.h>

static HANDLE select_thread_lock;
static HANDLE select_main_lock;
static HANDLE thread_handle;
static  struct tothread *args;
static int select_res;

class winsock_fds
{
public:
  unsigned short fd_count;
  SOCKET fd_array[FD_SETSIZE];
  int mapping[FD_SETSIZE];
  const char *name;
  winsock_fds (const char *name);
  void add (SOCKET x, int map);
  int translate_and_count (fd_set *dst);
  void dump ();
};

class tothread
{
public:
  winsock_fds r;
  winsock_fds w;
  winsock_fds e;
  timeval *timeout;
  void dump ();
  tothread ();
};

tothread::tothread (): r ("read socket"), w ("write socket"), e ("exception socket")
{
}

void tothread::dump ()
{
  r.dump ();
  w.dump ();
  e.dump ();
}

winsock_fds::winsock_fds (const char *n) 
{
  fd_count = 0;
  name = n;
}

void winsock_fds::add (SOCKET x, int map)
{
  mapping[fd_count] = map;
  fd_array[fd_count] = x;
  fd_count++;
}

void winsock_fds::dump ()
{
  for (int i = 0; i < fd_count; i++)
    {
      select_printf ("%s %d %d->%d\n", name, i, mapping[i], fd_array[i]);
    }
}

int winsock_fds::translate_and_count (fd_set *dst)
{
  int total = 0;

  select_printf (" translate_and_count %s:\n", name);

  for (int i = 0; i < fd_count; i++)
    {
      if (fd_array[i]) {
	FD_SET (mapping[i], dst);
	select_printf (" t_and_c: setting %d because of %d\n", mapping[i], i);
	total ++;
      }
    }
  select_printf (" translate_and_count %s gives %d\n", name, total);
  return total;
}

void selectthread (void *)
{
  select_printf ("selectthread started\n");
  /* Wait for the args to be set up */
  while (1) 
    {
      WaitForSingleObject (select_thread_lock, INFINITE);
      select_printf ("about to select, %x %x %x\n",
		   &args->r, &args->w, &args->e);

      timeval to;
      
      to.tv_sec = 2;
      to.tv_usec = 2;
      
      select_res =   select (0,
			     (fd_set *)(&args->r),
			     (fd_set *)(&args->w),
			     (fd_set *)(&args->e),
			     &to);
      select_printf ("select real res %d %x %x %d\n",select_res,
		   to.tv_sec,
		   to.tv_usec, sizeof (to));
      ReleaseSemaphore (select_thread_lock, 1,0);
      WaitForSingleObject (select_main_lock, INFINITE);
    }
}

class bmap_class
{
public:
  HANDLE handles[FD_SETSIZE];
  const char *names[FD_SETSIZE];
  char map[FD_SETSIZE];
  char rmap[FD_SETSIZE];
  char wmap[FD_SETSIZE];
  char emap[FD_SETSIZE];
  
  int size;
  int always_size;

  void add (int, int, int, int, HANDLE, const char *);
  void add_always_ready (int, int, int, int,  const char *);
  void add_socket (HANDLE, const char *);

  
  bmap_class ();
  void dump ();
  int waithit (int, fd_set *readfds,
	       fd_set *writefds,
	       fd_set *exceptfds);
  int alwayshit (fd_set *readfds,
	       fd_set *writefds,
	       fd_set *exceptfds);
  
  
};

int bmap_class::alwayshit (fd_set *readfds,
			 fd_set *writefds,
			 fd_set *exceptfds)
{
  int i;
  int retval = 0;
  select_printf ("always hit: size %d\n", always_size);
  for (i= 0; i < always_size; i++)
    {
      int ai = FD_SETSIZE - i - 1;
      select_printf ("always hit ai %d (i %d)\n", ai, i);
      retval +=      waithit (ai, readfds, writefds, exceptfds);
      select_printf ("always hit waithit %d\n", retval);
    }
  select_printf ("always hit: returned %d\n", retval);
  return retval;
}

int bmap_class::waithit (int idx,
			 fd_set *readfds,
			 fd_set *writefds,
			 fd_set *exceptfds)
{
  int res = 0;
  int fd = map[idx];
  
  if (rmap[idx]) {
    res++;
    FD_SET (fd, readfds);
select_printf ("waithit: %d [mapped %d] set read\n", idx, fd);
  }
  

  if (wmap[idx]) {
    res++;
    FD_SET (fd, writefds);
select_printf ("waithit: %d [mapped %d] set write\n", idx, fd);
  }
  

  if (emap[idx]) {
    res++;    
    FD_SET (fd, exceptfds);
select_printf ("waithit: %d [mapped %d] set except\n", idx, fd);
  }
  return res;
}

void bmap_class::dump ()
{
  select_printf ("dump of mappings of %d file handles to select bitmaps (in=%d out=%d)\n",
		 size,
		 GetStdHandle (STD_INPUT_HANDLE),
		 GetStdHandle (STD_OUTPUT_HANDLE));
  
  for (int i = 0; i < size; i++)
    {
      select_printf (" index %d: %d %s back to fd %d, readbit=%d writebit=%d ebit=%d\n",
		     i, handles[i],names[i], map[i], rmap[i], wmap[i], emap[i]);
      
    }
  
}

bmap_class::bmap_class ()
{
  size = 0;
  always_size = 0;  
}

void bmap_class::add_socket (HANDLE so, const char *name)
{
  handles[size] = so;
  names[size] = name;
  map[size] = 99;
  rmap[size] = 9;
  wmap[size] = 9;
  emap[size] = 9;
  size++;
}

void bmap_class::add (int r, int w, int e, int idx, HANDLE h, const char *name)
{
  handles[size] = h;
  names[size] = name;
  map[size] = idx;
  
  rmap[size] = r;
  wmap[size] = w;
  emap[size] = e;
  size++;
}

void bmap_class::add_always_ready (int r, int w, int e, int idx, const char *name)
{
  int ai = FD_SETSIZE - always_size - 1;
  names[ai] = name;
  map[ai] = idx;
  rmap[ai] = r;
  wmap[ai] = w;
  emap[ai] = e;
  always_size++;
}


static void dbg_input_event (INPUT_RECORD*input_rec) {
select_printf ("polled: PeekConsoleInput, type 0x%x key: dn %d, st 0x%x\n",
	      input_rec->EventType, 
	      input_rec->Event.KeyEvent.bKeyDown,
	      input_rec->Event.KeyEvent.dwControlKeyState);
select_printf ("polled: ... rpt %d vkc 0x%x vsc 0x%x ascii 0x%x\n",
	      input_rec->Event.KeyEvent.wRepeatCount,
	      input_rec->Event.KeyEvent.wVirtualKeyCode,
	      input_rec->Event.KeyEvent.wVirtualScanCode,
	      input_rec->Event.KeyEvent.AsciiChar);
}

/* Used to cope when a polling select is requested */

static int polled (int socket_count,
		  bmap_class *mp,
		  struct tothread *t,
		  fd_set *readfds, 
		  fd_set *writefds,  
		  fd_set *exceptfds)
{
  int retval = 0;
  
  /* Polling.. This is simple */
  if (socket_count) 
    {
      struct timeval timeout = {0,0 };

      int hadone =    select (0,    
			      (fd_set *)(&t->r),
			      (fd_set *)(&t->w),
			      (fd_set *)(&t->e),
			      &timeout);
      if (hadone)
	{
	  retval += t->r.translate_and_count (readfds);
	  retval += t->w.translate_and_count (writefds);
	  retval += t->e.translate_and_count (exceptfds);
 select_printf ("polled: (%d) select on socket gives %d\n", hadone, retval);
	}
    }
      
/*  Sleep (10); */
  int code = WaitForMultipleObjects (mp->size, mp->handles, 0, 10 /* 0 */);
/*  Sleep (10); */
select_printf ("polled: WaitForMultipleObjects (%d)->%d\n", mp->size, code);


if ((code == WAIT_OBJECT_0) && (mp->handles[0] == GetStdHandle (STD_INPUT_HANDLE))) {
    int num_events = 0, ne2;
    if (GetNumberOfConsoleInputEvents (GetStdHandle (STD_INPUT_HANDLE), 
				     &num_events)) {
	if (num_events == 0) {
	    code = WAIT_TIMEOUT;
	    select_printf ("polled: gnocie found no events, forcing TIMEOUT\n");
	} else {
	    INPUT_RECORD input_rec[128];
	    select_printf ("polled: getnumberofconsoleinputevents->%d\n", num_events);

	    if (PeekConsoleInput (GetStdHandle (STD_INPUT_HANDLE), 
				input_rec, 128, &ne2)) {
		int i;
		select_printf ("polled: PeekConsoleInput got %d records\n", ne2);
		for (i = 0; i < ne2; i++)
		    dbg_input_event (input_rec+i);
		if ((input_rec[0].Event.KeyEvent.AsciiChar == 0)
		    || (input_rec[0].Event.KeyEvent.bKeyDown == 0)) {
		    ReadConsoleInput (GetStdHandle (STD_INPUT_HANDLE), 
				     input_rec, 1, &ne2);
		    select_printf ("polled: stole one with ReadConsoleInput (forcing TIMEOUT)\n");
		    /* grabbed at least one */
		    code = WAIT_TIMEOUT;
		} else {
		    /* at least one is real so leave it */
		    num_events = 0;
		    code = WAIT_OBJECT_0;
		    select_printf ("polled: one good record (forcing OBJECT_0)\n");
		}
	    } else {
		/* peek error */ 
		code = WAIT_TIMEOUT; 
		select_printf ("polled: peek failed (forcing TIMEOUT)\n");
	    }
	}
    } else {
	/* gnocie failed */
	code = WAIT_TIMEOUT;
	select_printf ("polled: gnocie failed (forcing TIMEOUT)\n");
    }
}
      
  
  if (code >= WAIT_OBJECT_0 
      && code < WAIT_OBJECT_0 + mp->size)
    {
      retval +=  mp->waithit (code - WAIT_OBJECT_0,
			      readfds,
			      writefds,
			      exceptfds);
      select_printf ("polled: Waithit return %d\n", retval);
    }


  retval += mp->alwayshit (readfds, writefds, exceptfds);
  select_printf ("polled: alwayshit return %d\n", retval);

  return retval;
}

static int waiting (int socket_count,
		   bmap_class *mp,
		   struct tothread *t,
		   fd_set *readfds, 
		   fd_set *writefds,  
		   fd_set *exceptfds,
		   int millisec)
{
  /* We fake by polling... */


  int retval =0;
  

  /* Try a quick poll */
  retval = polled (socket_count, mp, t, readfds, writefds, exceptfds);
  if (retval)
    return retval;

  int socket_entry  = -10;
  int todo = 20;
  
  if (socket_count)
    {
      t->dump ();
      struct timeval mytimeout;
      mytimeout.tv_sec = 1;
      mytimeout.tv_usec = 0;
      t->timeout = &mytimeout;
      args = t;
      /* Start up the socket thread  - so the select thread
	 will start up and look for the things we need. */
      socket_entry = mp->size;
      mp->add_socket (select_thread_lock,"socket thread");
      ReleaseSemaphore (select_thread_lock, 1,0);
    }

  int code;

  while (1) 
    {
      int i;
      select_printf ("Waiting for a socket thread or a filehandle.. the list is\n");
      for (i= 0; i <mp->size; i++)
	{
	  select_printf ("%x,", mp->handles[i]);
	}
      select_printf ("\n");

      code = WaitForMultipleObjects (mp->size, mp->handles, 0, millisec);

      select_printf ("res code %d, code %d\n", socket_entry, code);
      if (code == socket_entry && select_res == 0)
	{
	  /* We've found an event - but it was just the select timing out, 
	     retry it */
	  select_printf ("just a timeout\n");
	  ReleaseSemaphore (select_thread_lock, 1, 0);
	  ReleaseSemaphore (select_main_lock, 1, 0);
	}
      else
	break;
    }
  

  select_printf ("Waitfor said (todo %d) %d %d\n", todo, code, GetLastError ());

  
  int idx = code - WAIT_OBJECT_0;
  if (idx == socket_entry )
    {
      /* One of the socket things we were waiting for happened  - work out
	 which socket it was */
      select_printf ("code said it was a select\n");
      t->dump ();      
      retval+=  t->r.translate_and_count (readfds);
      retval += t->w.translate_and_count (writefds);
      retval += t->e.translate_and_count (exceptfds);

      /* The select has finished, make sure we get back to the top of the
	 thread, and make it wait for us */
      ReleaseSemaphore (select_main_lock,1,0);
    }
  else if (idx >= 0 && idx < todo)
    {
      /* We've hit something which isn't a socket */
      retval +=  mp->waithit (idx, readfds,  writefds, exceptfds);
	  
      if (socket_entry >= 0)
	{
	  /* This is nasty - there are two race conditions here. 

	     either the WaitForMultipleObjects above finished before
	     our select thread started the select call,

	     or select finished after WaitForMultipleObjects exited */

	  /* We guess that select finished after WaitForMultiple object,s
	     and hope to recover if that is not so */
	top:
	  int res = WaitForSingleObject (select_thread_lock, 0);
	  select_printf ("WAIT SAYS %d\n", res);
	  if (res == WAIT_OBJECT_0)
	    {
	      /* Ha we got the lock, eveything is cool. - add the bits
		 we may have found */
	      if (select_res) {
		retval += t->r.translate_and_count (readfds);
		retval += t->w.translate_and_count (writefds);
		retval += t->e.translate_and_count (exceptfds);
		t->dump ();
	      }
	    }
	  else if (res == WAIT_TIMEOUT)
	    {
	      /* it wasn't ready.. try and cancel the select */
	      int t = WSACancelBlockingCall ();
	      select_printf ("Cancel failed, because %d\n", t);
	      goto top;
	    }
	  else
	    {
	      select_printf ("res is %d\n", res);
	    }
	  select_printf ("*** CANCELED %d\n", res);
	  ReleaseSemaphore (select_main_lock,1,0);
	}
    }

  return retval;
}

int  cygwin32_select (int  n,
		     fd_set *readfds, 
		     fd_set *writefds,  
		     fd_set *exceptfds,
		     struct timeval *timeout)
{
  hinfo_vec *hvec = &u->self->hmap;
  int socket_count= 0;

  int retval = 0;

  fd_set dummy;
  

  long millisec;
  /* Mapping between the index into the h vector and the fd number */
  bmap_class mp;
  

  struct tothread t;

select_printf ("cygwin32_select (%d,%x,%x,%x,%x (%d.%d))\n",
	      n, 
	      readfds?readfds->fds_bits[0]:0,
	      writefds?writefds->fds_bits[0]:0,
	      exceptfds?exceptfds->fds_bits[0]:0,
	      timeout,
	      timeout ? timeout->tv_sec : 0,
	      timeout ? timeout->tv_usec : 0);


  FD_ZERO (&dummy);
  
  if (!readfds)
    readfds=&dummy;
  if (!writefds)
    writefds=&dummy;
  if (!exceptfds)
    exceptfds=&dummy;
  
  millisec =  timeout ? timeout->tv_sec * 1000 + timeout->tv_usec / 1000 : INFINITE;


  syscall_printf ("select (%d, %x, %x, %x, %x (%x:%x));\n",
		 n,
		 readfds,
		 writefds,
		 exceptfds,
		 timeout,
		 timeout ? timeout->tv_sec : 0,
		 timeout ? timeout->tv_usec : 0);

  for (int i = n -1; i >= 0; i--)
    {
      int wantread = FD_ISSET (i, readfds);
      int wantwrite = FD_ISSET (i, writefds);
      int wantexcept = FD_ISSET (i, exceptfds);


      if (wantread|wantwrite|wantexcept)
	{
select_printf ("select: [%d] r %d w %d e %d\n", 
	      i, wantread, wantwrite, wantexcept);
	  if (!NOT_OPEN_FD (i)) 
	    {
	      fhandler_socket *s = (*hvec)[i].h->is_socket ();
	      if (s)
		{
		  if (wantexcept)
		    t.e.add (s->get_socket (), i);
		  if (wantread)
		    t.r.add (s->get_socket (), i);
		  if (wantwrite)
		    t.w.add (s->get_socket (), i);
		  socket_count ++;
		}
	      else if (u->self->hmap[i].h->always_ready ())
		{
		 select_printf ("adding always %s\n", u->self->hmap[i].h->get_name ());
		  mp.add_always_ready (wantread,
			  wantwrite,
			  wantexcept,
  		       i,
			  u->self->hmap[i].h->get_name ());
		}
	      else
		{
		  select_printf ("adding normal %s\n", u->self->hmap[i].h->get_name ());
		  mp.add (wantread,
			  wantwrite,
			  wantexcept,
			  i,u->self->hmap[i].h->get_handle (),
			  u->self->hmap[i].h->get_name ());
		}
	    }
	}
    }


  FD_ZERO (readfds);
  FD_ZERO (writefds);
  FD_ZERO (exceptfds);
  int i;

  mp.dump ();

  
  if (millisec == 0)
    {
      retval = polled (socket_count, &mp, &t,readfds, writefds, exceptfds);
    }
  
  else 
    {
#if 0
      retval = waiting (socket_count, &mp, &t,readfds, writefds, exceptfds, millisec);
#else
      for (i= 0; i < millisec; i+=100)
	{
	  retval = polled (socket_count, &mp, &t,readfds, writefds, exceptfds);      
	  if (retval)
	    break;
	  Sleep (100);
	}
#endif
    }
  
  for (i = 0; i < n; i++)
    {
      if (FD_ISSET (i, readfds))
	select_printf ("->read %d\n", i);
      if (FD_ISSET (i, writefds))
	select_printf ("->write %d\n", i);
      if (FD_ISSET (i, exceptfds))
	select_printf ("->except %d\n", i);
    }

  syscall_printf ("%d = select (%d, %x, %x, %x, %x (%x:%x));\n",
		 retval,
		 n,
		 readfds,
		 writefds,
		 exceptfds,
		 timeout,
		 timeout ? timeout->tv_sec : 0,
		 timeout ? timeout->tv_usec : 0);

select_printf ("%d = cygwin32_select (%d,%x,%x,%x,%x (%d.%d))\n",
	      retval,
	      n, 
	      readfds?readfds->fds_bits[0]:0,
	      writefds?writefds->fds_bits[0]:0,
	      exceptfds?exceptfds->fds_bits[0]:0,
	      timeout,
	      timeout ? timeout->tv_sec : 0,
	      timeout ? timeout->tv_usec : 0);

  return retval;
      
}


void select_init ()
{
  DWORD thread_id;


  select_thread_lock = CreateSemaphoreA (0, 0, 1, 0);
  select_main_lock = CreateSemaphoreA (0, 0, 1, 0);
  thread_handle = CreateThread (0, 0x1000, &selectthread, 0, 0, &thread_id);
  select_printf ("select_thread_lock %d\n", select_thread_lock);
  select_printf ("select_main_lock %d\n", select_main_lock);
  
}
