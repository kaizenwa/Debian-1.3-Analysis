/* List the currently loaded modules.
   Copyright 1996, 1997 Linux International.

   New implementation contributed by Richard Henderson <rth@tamu.edu>
   Based on original work by Bjorn Eckwall <bj0rn@blox.se>

   This file is part of the Linux modutils.

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2 of the License, or (at your
   option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */


#include <sys/types.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>

#include "module.h"
#include "util.h"

#include "logger.h"

/*======================================================================*/

/* If we don't have query_module ... */

static int
old_lsmod(void)
{
  int fd, len, bufsize;
  char *buffer;

  /* We've got no other option but to read /proc.  */

  if ((fd = open("/proc/modules", O_RDONLY)) < 0)
    {
      error("/proc/modules: %m");
      return 1;
    }

  buffer = xmalloc(bufsize = 8*1024);
retry_read:
  len = read(fd, buffer, bufsize);
  if (len < 0)
    {
      error("/proc/modules: %m");
      return 1;
    }
  else if (len == sizeof(buffer))
    {
      lseek(fd, 0, SEEK_SET);
      buffer = xrealloc(buffer, bufsize *= 2);
      goto retry_read;
    }

  close(fd);

  /* Write it out.  */

  puts("Module         Pages    Used by");
  write(1, buffer, len);

  return 0;
}

/* If we do have query_module ... */

static int
new_lsmod(void)
{
  char *module_names, *m, *refs;
  size_t bufsize, ret, nmod, i;

  /* A header.  */

  puts("Module                  Size  Used by");

  /* Fetch the list of modules.  */

  module_names = xmalloc(bufsize = 1024);
retry_mod_load:
  if (query_module(NULL, QM_MODULES, module_names, bufsize, &ret))
    {
      if (errno == ENOSPC)
	{
	  module_names = xrealloc(module_names, bufsize = ret);
	  goto retry_mod_load;
	}
      error("QM_MODULES: %m");
      return 1;
    }
  nmod = ret;

  refs = xmalloc(bufsize = 1024);
  for (i = 0, m = module_names; i < nmod; ++i, m += strlen(m)+1)
    {
      struct new_module_info info;
      size_t j;
      char *r;

      if (query_module(m, QM_INFO, &info, sizeof(info), &ret))
	{
	  error("QM_INFO: %m");
	  return 1;
	}

      printf("%-20s%8lu%4ld ", m, info.size, info.usecount);

      if (info.flags & NEW_MOD_DELETED)
	fputs(" (deleted)", stdout);
      else if (!(info.flags & NEW_MOD_RUNNING))
	fputs(" (uninitialized)", stdout);
      else 
	{
	  if (info.flags & NEW_MOD_AUTOCLEAN)
	    fputs(" (autoclean)", stdout);
	  if (!(info.flags & NEW_MOD_USED_ONCE))
	    fputs(" (unused)", stdout);
	}

    retry_ref_load:
      if (query_module(m, QM_REFS, refs, bufsize, &ret))
	{
	  if (errno == ENOSPC)
	    {
	      refs = xrealloc(refs, bufsize = ret);
	      goto retry_ref_load;
	    }
	  error("QM_REFS: %m");
	  return 1;
	}

      if (ret > 0)
	{
	  putchar(' ');
	  putchar('[');
	  j = 0, r = refs;
	  fputs(r, stdout);
	  while (r += strlen(r)+1, ++j < ret)
	    {
	      putchar(' ');
	      fputs(r, stdout);
	    }
	  putchar(']');
	}

      putchar('\n');
    }

  return 0;
}

int
main(int argc, char **argv)
{
  error_file = "lsmod";
  if (query_module(NULL, 0, NULL, 0, NULL) == 0)
    return new_lsmod();
  else
    return old_lsmod();
}
