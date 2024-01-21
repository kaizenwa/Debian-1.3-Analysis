/* Copyright 1997 Free Software Foundation, Inc.
   Contributed by Marcin Dalecki <dalecki@sub994.sub.uni-goettingen.de>

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

#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>
#include <errno.h>
#include <elf.h>
#include ELF_MACHINE_H

#include "util.h"
#include "obj.h"
#include "misc.h"
#include "conf_file.h"

int n_objs = 0;
int max_objs = 0;
struct obj_file **objs;
char **names;

/*======================================================================*/

static int flag_show_error = 0;
static int flag_verbose = 0;

#ifndef NO_COMPAT
static int flag_new_syscalls = 0;
#endif

struct new_module_symbol *ksyms;
size_t nksyms;

/*======================================================================*/

static void
new_read_kernel_syms (void)
{
  struct new_module_symbol *syms, *s;
  size_t ret, bufsize, nsyms, j;

  /* Collect the kernel's symbols.  */

  syms = xmalloc (bufsize = 16 * 1024);
retry_kern_sym_load:
  if (query_module (NULL, QM_SYMBOLS, syms, bufsize, &ret))
    {
      if (errno == ENOSPC)
	{
	  syms = xrealloc (syms, bufsize = ret);
	  goto retry_kern_sym_load;
	}
      error ("kernel: QM_SYMBOLS: %m");
      exit (1);
    }
  nksyms = nsyms = ret;
  ksyms = syms;

  for (j = 0, s = syms; j < nsyms; ++j, ++s)
    s->name += (unsigned long) syms;
}

#ifndef NO_COMPAT

static void
old_read_kernel_syms (void)
{
  struct old_kernel_sym *ks, *k;
  struct new_module_symbol *s;
  int nks, nms, i;

  nks = get_kernel_syms(NULL);
  if (nks < 0)
    {
      error("get_kernel_syms: %m");
      return;
    }

  ks = k = xmalloc(nks * sizeof(*ks));
  if (get_kernel_syms(ks) != nks)
    {
      error("inconsistency with get_kernel_syms -- is someone else "
	    "playing with modules?");
      free(ks);
      return;
    }

  /* Collect the module information.  */

  while (k->name[0] != '#' || k->name[1])
    ++k;
  ++k;

  nksyms = nms = nks - (k - ks);
  ksyms = s = (nms ? xmalloc(nms * sizeof(*s)) : NULL);

  for (i = 0; i < nms; ++i, ++s, ++k)
    {
      s->name = (unsigned long)k->name;
      s->value = k->value;
    }
}

#endif

/*
 * Format the dependency list of a module into a simple makefile
 */
static void
print_deps_file (const char *depfile)
{
  FILE *fout = stdout;
  char **deps;
  int max_deps = 64;
  int o;

  deps = (char **) xmalloc (max_deps * sizeof (char *));

  if (depfile && !(fout = fopen (depfile, "w")))
    {
      lprintf ("can't open %s", depfile);
      exit (1);
    }

  /*
   * Loop through all modules we visited and construct the dependencies.
   */
  for (o = 0; o < n_objs; ++o)
    {
      int errs = 0;
      int n_deps = 0;
      int i;

      /*
       * Loop throught all undefined symbols of this object
       */
      for (i = 0; i < HASH_BUCKETS; ++i)
	{
	  struct obj_symbol *sym;
	  for (sym = objs[o]->symtab[i]; sym; sym = sym->next)
	    {
	      int j;
	      int k;

	      if (sym->secidx != SHN_UNDEF
		  || ELFW (ST_BIND) (sym->info) == STB_WEAK)
		continue;

	      /*
	       * Traverse all modules and search for one, where it may be
	       * defined.
	       */
	      for (j = 0; j < n_objs; ++j)
		{
		  struct obj_symbol *tmp;
		  if (j == o)
		    continue;

		  tmp = obj_find_symbol (objs[j], sym->name);
		  if (tmp && tmp->secidx != SHN_UNDEF)
		    break;
		}
	      if (j == n_objs)
		{
		  if (errs == 0)
		    lprintf ("%s: unresolved symbol(s)"
			     ,names[o]);
		  if (flag_show_error)
		    lprintf ("\t%s", sym->name);
		  errs++;
		  continue;
		}

	      /*
	       * Look if it's allready there
	       */
	      for (k = 0; k < n_deps; ++k)
		if (deps[k] == names[j])
		  break;
	      /*
	       * And add the dep info if neccessary.
	       */
	      if (k != n_deps)
		continue;

	      if (k == max_deps)
		{
		  max_deps <<= 1;
		  deps = (char **)
		    xrealloc (deps, max_deps * sizeof (char *));
		}
	      deps[k] = names[j];
	      ++n_deps;
	    }
	}

      if (errs == 0 && flag_verbose)
	printf ("%s\n", names[o]);
      fprintf (fout, "%s:", names[o]);

      /*
       * Print the dependencies for this module.
       */
#if 1
      while (n_deps--)
	fprintf (fout, " %s", deps[n_deps]);
#else
      for (i = 0; i < n_deps; ++i)
	fprintf (fout, " %s", deps[i]);
#endif
      fprintf (fout, "\n\n");
    }
  if (fout != stdout)
    fclose (fout);
  free (deps);
}

/*
 * Load an object file and resolve all the symbols contained therein.
 */
static int
load_obj_file (const char *module)
{
  FILE *fp;
  struct obj_file *file;
  struct obj_symbol *sym;
  int i;

  if (!(fp = fopen (module, "r")))
    return 1;

  if (!(file = obj_load (fp)))
    return 1;

  if (n_objs == max_objs)
    {
      max_objs <<= 2;
      objs = (struct obj_file **)
	xrealloc (objs, max_objs * sizeof (struct obj_file *));
      names = (char **) xrealloc (names, max_objs * sizeof (char *));
    }
  objs[n_objs] = file;
  names[n_objs] = xstrdup (module);

  /*
   * Hide the kernel symbols...
   */
  for (i = 0; i < nksyms; ++i)
    if ((sym = obj_find_symbol (file, (char *) ksyms[i].name)) != NULL)
      sym->secidx = SHN_HIRESERVE + 1;

  /*
   * Hide them they are used by insmod internally.
   */
  if ((sym = obj_find_symbol (file, "__this_module")) != NULL)
    sym->secidx = SHN_HIRESERVE + 1;
  if ((sym = obj_find_symbol (file, "mod_use_count_")) != NULL)
    sym->secidx = SHN_HIRESERVE + 1;
  ++n_objs;

  return 0;
}

/*
 * Print usage information ans exit.
 */
static void
usage (void)
{
  puts(
    "Usage: depmod [-e -s -v ] -a [FORCED_KERNEL_VER]\n"
    "       depmod [-e -s -v ] MODULE_1.o MODULE_2.o ...\n"
    "Create module-dependency information for modprobe.\n\n"
    "  -a, --all                  visit all modules\n"
    "  -d, --debug                run in debug mode\n"
    "  -e                         output unresolved symbols\n"
    "  -s, --system-log           use the system log for error reporting\n"
    "      --help                 display this help and exit\n"
    "  -v, --verbose              run in verbose mode\n"
    "  -V, --version              output version information and exit"
    );
}

int
main (int argc, char *argv[])
{
  int ret = 1;
  int flag_stdmode = 0;

  int opt_tag;

  if (argc == 1)
    {
      usage ();
      return 1;
    }

#ifndef NO_COMPAT
  flag_new_syscalls = !query_module (NULL, 0, NULL, 0, NULL);
  if (!flag_new_syscalls)
    old_read_kernel_syms ();
  else
#endif
    new_read_kernel_syms ();

  /*
   * Yes we are using getopts!
   */
  while (1)
    {
      static struct option long_opts[] =
      {
	{"all", 0, 0, 'a'},
	{"debug", 0, 0, 'd'},
	{"system-log", 0, 0, 's'},
	{"verbose", 0, 0, 'v'},
	{"version", 0, 0, 'V'},
	{"help", 0, 0, 'h'},
	{0, 0, 0, 0}		/* Table end tag */
      };
      int opt_ind = 0;

      opt_tag = getopt_long (argc, argv, "adesvV", long_opts,
			     &opt_ind);
      if (opt_tag == -1)
	break;

      switch (opt_tag)
	{
	case 'a':
	  flag_stdmode = 1;
	  break;

	case 'd':
	  flag_debug = 1;
	  break;

	case 'e':
	  flag_show_error = 1;
	  break;

	case 's':
	  setsyslog ("depmod");	/* use the syslog for reporting */
	  break;

	case 'v':
	  flag_verbose = 1;	/* give terse informations during run */
	  break;

	case '?':
	case 'h':
	  usage ();
	  exit (opt_tag == 'h' ? 0 : 1);
	  break;

	case 'V':
	  puts ("depmod (Linux modutils) " MODUTILS_VERSION);
	  if (argc != 2)
	    putchar ('\n');
	  break;

	default:
	  abort ();
	}
    }

  /*
   * Skip all automatically processed options.
   */
  argc -= optind;
  argv += optind;

  /*
   * argv now points to the first non-option argument
   * argc is the remaining argument count
   */

  n_objs = 0;
  max_objs = 64;
  objs = (struct obj_file **) xmalloc (max_objs * sizeof (struct obj_file *));
  names = (char **) xmalloc (max_objs * sizeof (char *));

  if (flag_stdmode)
    {
      struct mod_path *mods = NULL;
      struct mod_path *tmp;

      if (argc > 0)
	{
	  if (read_config_file (*argv))
	    {
	      lprintf ("%s does not exist", depfile);
	      return 1;
	    }
	}
      else if (read_config_file (NULL))		/* read the default config file */
	return 1;

      mods = find_matching_mods ("*", NULL, 1);
      ret = 0;
      while ((tmp = mods) && !ret)
	{
	  ret = load_obj_file (mods->path);
	  mods = mods->next;
	  free (tmp->path);
	  free (tmp);
	}
      while ((tmp = mods))
	{
	  mods = mods->next;
	  free (tmp->path);
	  free (tmp);
	}

      if (!ret)
	print_deps_file (depfile);
    }
  else
    {
      /*
       * Process all modules which are explicitly specified on the command line
       */

      for (ret = 0; argc > 0 && !ret; ++argv, --argc)
	ret = load_obj_file (*argv);
      if (!ret)
	print_deps_file (NULL);
    }

  return ret;
}
