/* Display the symbols exported from the running kernel.
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
#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>

#include "module.h"
#include "util.h"
#include "version.h"

#include "logger.h"


/*======================================================================*/

int flag_allsyms = 0;
int flag_mod_info = 0;
int flag_print_header = 1;


/*======================================================================*/

static inline void
print_symbol(const char *name, unsigned long value, const char *module)
{
  if (module)
    printf("%0*lx  %-32s  [%s]\n", (int)(2*sizeof(void*)),
	   value, name, module);
  else
    printf("%0*lx  %-32s\n", (int)(2*sizeof(void*)), value, name);
}

static inline void
print_mod_info(const char *module, unsigned long addr, unsigned long size)
{
  int o;

  o = printf("%0*lx  (%luk)", (int)(2*sizeof(void*)), addr,
	     (size / 1024) + ((size % 1024) != 0));

  o = 2*sizeof(void*)+2+32+2 - o;

  printf("%*s[%s]\n", o, "", module);
}


/* If we don't have query_module()... */

static int
old_ksyms(void)
{
  struct old_kernel_sym *ksyms, *k;
  int nsyms, i;
  int kmem_fd = -1;
  const char *module;

  nsyms = get_kernel_syms(NULL);
  if (nsyms < 0)
    {
      perror("get_kernel_syms");
      return 1;
    }

  ksyms = xmalloc(nsyms * sizeof(*ksyms));

  if (get_kernel_syms(ksyms) != nsyms)
    {
      fprintf(stderr, "Inconsistency reading kernel symbols -- "
	      "is someone else playing with modules?\n");
      return 1;
    }

  /* If requested, open kmem so we can get at module information.  */

  if (flag_mod_info)
    {
      kmem_fd = open("/dev/kmem", O_RDONLY);
      if (kmem_fd < 0)
	{
	  perror("ksyms: open /dev/kmem");
	  return 1;
	}
    }

  module = NULL;
  for (k = ksyms, i = 0; i < nsyms; ++i, ++k)
    if (k->name[0] == '#')
      {
	if (k->name[1])
	  {
	    module = &k->name[1];
	    if (flag_mod_info)
	      {
		struct old_module info;

		if (llseek(kmem_fd, (off_t)k->value, SEEK_SET) == -1
		    || read(kmem_fd, &info, sizeof(info)) != sizeof(info))
		  {
		    perror("ksyms: /dev/kmem");
		    return 1;
		  }
		print_mod_info(module, info.addr, info.size * getpagesize());
	      }
	  }
	else
	  {
	    if (!flag_allsyms)
	      break;
	    module = NULL;
	  }
      }
    else
      print_symbol(k->name, k->value, module);

  free(ksyms);
  if (flag_mod_info)
    close(kmem_fd);

  return 0;
}


/* If we do have query_module()...  */

static int
new_ksyms(void)
{
  char *modules;
  size_t nmodules;
  struct new_module_symbol *syms;
  size_t nsymbols;

  ssize_t ret;
  size_t bufsize;
  size_t i, j;
  char *m;
  struct new_module_symbol *s;

  /* Query for the module names.  */

  modules = xmalloc(bufsize = 256);
retry_modules_load:
  if (query_module(NULL, QM_MODULES, modules, bufsize, &ret))
    {
      if (errno == ENOSPC)
	{
	  modules = xrealloc(modules, bufsize = ret);
	  goto retry_modules_load;
	}
      perror("ksyms: QM_MODULES");
      return 1;
    }
  nmodules = ret;

  /* Iterate over the modules, in order, and care for their symbols.  */

  syms = xmalloc(bufsize = 16*1024);
  for (i = 0, m = modules; i < nmodules; ++i, m += strlen(m)+1)
    {
      if (flag_mod_info)
	{
	  struct new_module_info info;
	  if (query_module(m, QM_INFO, &info, sizeof(info), &ret))
	    {
	      if (errno == ENOENT)
		{
		  /* The module must have been removed in the interim: skip */
		  continue;
		}
	      perror("ksyms: QM_INFO");
	      return 1;
	    }
	  print_mod_info(m, info.addr, info.size);
	}

    retry_mod_symbol_load:
      if (query_module(m, QM_SYMBOLS, syms, bufsize, &ret))
	switch (errno)
	  {
	  case ENOSPC:
	    syms = xrealloc(syms, bufsize = ret);
	    goto retry_mod_symbol_load;
	  case ENOENT:
	    continue;
	  default:
	    perror("ksyms: QM_SYMBOLS");
	    return 1;
	  }
      nsymbols = ret;

      for (j = 0, s = syms; j < nsymbols; ++j, ++s)
	print_symbol((char *)syms + s->name, s->value, m);
    }

  /* Now do the kernel itself.  */

  if (flag_allsyms)
    {
    retry_kern_symbol_load:
      if (query_module(NULL, QM_SYMBOLS, syms, bufsize, &ret))
	{
	  if (errno == ENOSPC)
	    {
	      syms = xrealloc(syms, bufsize = ret);
	      goto retry_kern_symbol_load;
	    }
	  perror("ksyms: QM_SYMBOLS");
	  return 1;
	}
      nsymbols = ret;

      for (j = 0, s = syms; j < nsymbols; ++j, ++s)
	print_symbol((char *)syms + s->name, s->value, NULL); 
    }

  free(modules);
  free(syms);

  return 0;
}


/*======================================================================*/

int main(int argc, char **argv)
{
  int i;

  error_file = "ksyms";

  while ((i = getopt(argc, argv, "amhV")) != EOF)
    switch (i)
      {
      case 'a':
	flag_allsyms = 1;
	break;
      case 'm':
	flag_mod_info = 1;
	break;
      case 'h':
	flag_print_header = 0;
	break;
      case 'V':
        fputs("ksyms version " MODUTILS_VERSION "\n", stderr);
        break;
      }

  if (flag_print_header)
    printf("%-*s  %-32s  Defined by\n", (int)(2*sizeof(void*)),
	   "Address", "Symbol");

  if (query_module(NULL, 0, NULL, 0, NULL) == 0)
    return new_ksyms();
  else
    return old_ksyms();
}
