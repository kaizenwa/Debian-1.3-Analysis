/* shared data area support.

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#include <unistd.h>
#include "winsup.h"
#include "registry.h"

shared_info *s;

/* The handle of the shared data area.  */
static HANDLE h;

/* Open the shared memory map.  */

static void
open_shared_file_map ()
{
  HANDLE h = OpenFileMappingA (FILE_MAP_READ|FILE_MAP_WRITE, FALSE,
			       "cygwin.shared");

  if (!h)
    {
      SECURITY_ATTRIBUTES sa;
      sa.nLength = sizeof (SECURITY_ATTRIBUTES);
      sa.bInheritHandle = 0;
      sa.lpSecurityDescriptor = 0;

      h = CreateFileMappingA ((HANDLE) 0xffffffff,
			      NULL,
			      PAGE_READWRITE,
			      0,
			      sizeof (*s),
			      "cygwin.shared");

    }

  if (!h)
    {
      system_printf ("shared_init: CreateFileMappingA\n");
      api_fatal ("terminating");
    }

  s = (shared_info *) MapViewOfFileEx (h, FILE_MAP_READ|FILE_MAP_WRITE,
				       0, 0, 0, 0xa000000);

  if (!s)
    { 
      /* Probably win95, so try without specifying the address.  */
      s = (shared_info *) MapViewOfFileEx (h, FILE_MAP_READ|FILE_MAP_WRITE,
					   0,0,0,0);
    }

  if (!s)
    {
      system_printf ("shared_init: MapViewOfFileEx\n");
      api_fatal ("terminating");
    }

  debug_printf ("open_shared_file_map: s = %p, h = %d\n", s, h);

  /* FIXME: I couldn't find anywhere in the documentation a note about
     whether the memory is initialized to zero.  The code assumes it does
     and since this part seems to be working, we'll leave it as is.  */
}

void
shared_info::initialize ()
{
  /* Ya, Win32 provides a way for a dll to watch when it's first loaded.
     We may eventually want to use it but for now we have this.  */
  if (inited)
    return;

  /* Initialize the mount table.  */
  mount.init ();

  /* Initialize the process table.  */
  p.init ();

  /* Initialize the queue of deleted files.  */
  delqueue.init ();

  /* Fetch misc. registry entries.  */

  reg_session reg;

  heap_chunk_in_mb = reg.get_key ().get_int ("heap_chunk_in_mb", 8);
  if (heap_chunk_in_mb < 4)
    {
      heap_chunk_in_mb = 4;
      reg.get_key ().set_int ("heap_chunk_in_mb", heap_chunk_in_mb);
    }

  inited = 1;
}

void
shared_init ()
{
  open_shared_file_map ();

  s->initialize ();
}

void
shared_terminate ()
{
  CloseHandle (h);
}

int
shared_info::heap_chunk_size ()
{
  return heap_chunk_in_mb << 20;
}

/* For apps that wish to access the shared data.  */

shared_info *
cygwin32_getshared ()
{
  return s;
}
