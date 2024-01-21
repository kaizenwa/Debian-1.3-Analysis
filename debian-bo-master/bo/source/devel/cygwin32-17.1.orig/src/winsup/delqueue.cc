

#include "winsup.h"

void delqueue_item::process ()
{
  if (inuse)
    {
      if (DeleteFileA (name))
	{
	  syscall_printf ("Deleted %s\n", name);
	  inuse = 0;
	}
      else
	{
	  int res = GetLastError ();
	  if (res == ERROR_SHARING_VIOLATION)
	    {
	      /* File still inuse, that's ok */
	      syscall_printf ("Still using %s\n", name);
	    }

	  else
	    {
	      syscall_printf ("Hmm, don't know what to do with  %s\n", name);

	    }
	}
    }
}

void delqueue_list::init ()
{
  for (int i = 0; i < MAX_DELQUEUES_PENDING; i++)
    {
      v[i].init ();
    }
}

void delqueue_item::init ()
{
  inuse = 0;
}

int delqueue_item::add (const char *dosname)
{
  char *end;
  if (inuse)
    return 0;

  GetFullPathName (dosname, sizeof (name), name, &end);
  inuse = 1;
  return 1;
}

void delqueue_list::queue_file (const char *dosname)
{
  for (int i = 0; i < MAX_DELQUEUES_PENDING; i++)
    {
      if (v[i].add (dosname))
	return;
    }

  small_printf ("Out of queue slots!!\n");
}

void  delqueue_list::process_queue ()
{
  syscall_printf ("Running delqueue\n");
  for (int i = 0; i < MAX_DELQUEUES_PENDING; i++)
    {
      v[i].process ();
    }
}
