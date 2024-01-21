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
#include <stdarg.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/param.h>
#include <getopt.h>
#include <sys/stat.h>

#include "module.h"
#include "util.h"

#include "misc.h"
#include "conf_file.h"

/*
 * This is the actual modprobe specific part.
 *
 * The general convention throughout this file is that functions ALWAYS
 * return 0 on success and some different value otherwise.
 */

struct dep_node
  {
    struct dep_node *next;	/* modules */
    struct dep_node *deps;	/* dependences */
    char *name;
  };

/*
 * Command line flags
 */
static char flag_verbose = 0;
static int flag_by_kerneld = 0;

#ifndef NO_COMPAT
static int flag_new_syscalls = 0;
#endif

/*
 * Dependence information from the configuration file.
 */
static struct dep_node *file_deps = NULL;

static void
verbose (const char *ctl,...)
{
  if (flag_verbose)
    {
      va_list list;
      va_start (list, ctl);
      vprintf (ctl, list);
      va_end (list);
      fflush (stdout);
    }
}

/*
 * Free the memory associated with an dependency list
 */
static void
deps_free (struct dep_node *node)
{
  if (!node)
    return;

  free (node->name);
  deps_free (node->next);
  deps_free (node->deps);
  free (node);
}

static struct dep_node *
dep_lookup (struct dep_node *a, const char *name)
{
  if (!a)
    return NULL;

  if (strcmp (name, a->name) == 0)
    return a;

  return dep_lookup (a->next, name);
}

static struct dep_node *
dep_add (struct dep_node *node, const char *name)
{
  struct dep_node *tmp = (struct dep_node *)
  xmalloc (sizeof (struct dep_node));

  tmp->name = xstrdup (name);
  tmp->deps = NULL;
  tmp->next = node;

  return tmp;
}

/*
 *  Check if a given module is already in the kernel space.
 */

#ifndef NO_COMPAT

static int
old_in_kernel (char *mod)
{
  FILE *f;
  char linebuf[256], *c;

  if (!(f = fopen ("/proc/modules", "r")))
    error ("/proc/modules: %m");

  while (fgets (linebuf, 256, f))
    {
      c = strchr (linebuf, ' ');
      if (!c)
	error ("/proc/modules: Syntax error");
      *c = 0;
      if (!strcmp (mod, linebuf))
	{
	  fclose (f);
	  c = strchr (c + 1, '\t');
	  if (!c || c[1] != '[')
	    return 1;
	  else
	    return 2;
	}
    }

  fclose (f);
  return 0;
}

#endif

static int
new_in_kernel (char *mod)
{
  char *module_names, *m;
  size_t bufsize, ret, nmod, i;

  /* Fetch the list of modules.  */

  module_names = xmalloc (bufsize = 1024);
retry_mod_load:
  if (query_module (NULL, QM_MODULES, module_names, bufsize, &ret))
    {
      if (errno == ENOSPC)
	{
	  module_names = xrealloc (module_names, bufsize = ret);
	  goto retry_mod_load;
	}
      error ("QM_MODULES: %m");
      return 0;
    }
  nmod = ret;

  for (i = 0, m = module_names; i < nmod; ++i, m += strlen (m) + 1)
    if (!strcmp (m, mod))
      {
	free (mod);
	free (module_names);
	return 1;
      }

  free (mod);
  free (module_names);
  return 0;
}

static int
in_kernel (char *mod)
{
  mod = strip_o (mod);
#ifndef NO_COMPAT
  if (!flag_new_syscalls)
    return old_in_kernel (mod);
  else
#endif
    return new_in_kernel (mod);
}

/*
 * Read the dependancy file.
 */
static int
read_deps_file (const char *cfgfile)
{
  int line = 0;
  char *buf;
  char *end;
  char *tmp;
  char linebuf [8192];

  if (!(buf = read_and_preprocess_file (cfgfile)))
    return 1;

  /*
   * And now parse the buffer.
   */
  for (tmp = buf; (end = get_concat_line (tmp, &line)); tmp = end)
    {
      char *cp;

      if (!*tmp)		/* ignore blank lines */
	continue;

      tmp = resolve_string (tmp, linebuf, sizeof (linebuf));

      if (!(cp = strchr (tmp, ':')))
	{
	  free (buf);
	  error (":%d:parsing error in dependeny file\n", line);
	  return 1;		/* parsing error */
	}
      if ((cp > tmp) && *(cp - 1) == ' ')
	*(cp - 1) = '\0';
      *cp = '\0';

      /* name found */
      file_deps = dep_add (file_deps, tmp);

      tmp = cp + 1;
      while ((cp = strrchr (tmp, ' ')))
	{
	  *cp = '\0';
	  file_deps->deps = dep_add (file_deps->deps, cp + 1);
	}
    }
  free (buf);
  return 0;
}

static int
exec_rmmod_cmd (char *mod)
{
  int ret = 0;
  char *ex;
  mod = strip_o (mod);

  if ((ex = find_assoc_cmd (PRE_REMOVE, mod)) != NULL)
    if (system (ex))
      lprintf ("pre-remove %s failed\n", mod);

  if ((ex = find_assoc_cmd (REMOVE, mod)) != NULL)
    {
      if ((ret = system (ex)))
	lprintf ("remove %s failed\n", mod);
    }
  else if ((ret = delete_module (mod)) < 0)
    {
      ret = 1;
      perror (mod);
    }

  if (!ret && (ex = find_assoc_cmd (POST_REMOVE, mod)) != NULL)
    {
      if (system (ex) != 0)
	lprintf ("post-remove %s failed\n", mod);
    }

  free (mod);
  return ret;
}

/*
 * Unload all sub-modules in reverse order.
 */
static int
rm_sub_mods (struct dep_node *nod)
{
  int ret = 0;
  if (nod != NULL)
    if (!(ret = rm_sub_mods (nod->next)))
      ret = exec_rmmod_cmd (nod->name);

  return ret;
}

/*
 * Return the options associated with a module.
 * Return NULL if there are none.
 */
static char *
any_options (char *mod)
{
  char *modname = strip_o (mod);
  struct mod_option *opts;

  for (opts = mod_options; opts; opts = opts->next)
    if (!strcmp (opts->module, modname))
      return opts->args;

  return NULL;
}

/*
 * Try to load a module and the sub-modules it needs.
 */
static int
insmod (char *mod, struct dep_node **newin_kernel, char *options[])
{
  int err = 0;
  struct dep_node *dep;
  struct dep_node *nod;
  char *load_cmd;
  int cmd_len;
  char *op;
  char *ex;
  int i;

  if (!mod)
    return 0;

  if (in_kernel (mod))
    return 0;

  if (!(nod = dep_lookup (file_deps, mod)))
    {
      lprintf ("no dependency information for module: \"%s\"", mod);
      return 1;
    }

  dep = nod->deps;
  while (dep && !err)
    {
      err = insmod (dep->name, newin_kernel, NULL);
      dep = dep->next;
    }

  if (err)
    {
      rm_sub_mods (nod->deps);	/* revert everything */
      return 1;
    }

  /*
   * First determine the length of the command we will use.
   */
  cmd_len = 64 + strlen (mod);
  if (options && options[1] && strchr (options[1], '='))
    for (i = 1; options[i]; ++i)
      if (strchr (options[i], '='))
	cmd_len += strlen (options[i]) + 2;
      else
	break;
  else
    {
      if ((op = any_options (mod)))
	cmd_len += strlen (op) + 2;
      if (options && options[0] && (op = any_options (options[0])))
	cmd_len += strlen (op) + 2;
    }

  /*
   * And now actually compose the command!
   */
  load_cmd = (char *) xmalloc (cmd_len);

  strcpy (load_cmd, "/sbin/insmod ");
  if (flag_by_kerneld)
    strcat (load_cmd, "-k ");
  if (log)
    strcat (load_cmd, "-s ");
  if (insmod_opts)
    {
      strcat (load_cmd, insmod_opts);
      strcat (load_cmd, " ");
    }
  strcat (load_cmd, mod);

  if (options && options[1] && strchr (options[1], '='))
    for (i = 1; options[i]; ++i)
      {
	if (strchr (options[i], '='))
	  {
	    strcat (load_cmd, " ");
	    strcat (load_cmd, options[i]);
	  }
	else
	  break;
      }
  else
    {
      if ((op = any_options (mod)))
	{
	  strcat (load_cmd, " ");
	  strcat (load_cmd, op);
	}
      if (options && options[0] &&
	  (op = any_options (options[0])))
	{
	  strcat (load_cmd, " ");
	  strcat (load_cmd, op);
	}
    }
  verbose ("\r%s\n\t\t", load_cmd);

  if ((ex = find_assoc_cmd (PRE_INSTALL, mod)) != NULL)
    if ((err = system (ex)) != 0)
      lprintf ("pre-install %s failed\n", mod);

  if (!err)
    {
      if ((ex = find_assoc_cmd (INSTALL, mod)) != NULL)
	err = system (ex);
      else
	err = system (load_cmd);
    }

  if (!err && (ex = find_assoc_cmd (POST_INSTALL, mod)) != NULL)
    if ((err = system (ex)) != 0)
      lprintf ("post-install %s failed\n", mod);

  free (load_cmd);

  if (err)
    {
      rm_sub_mods (nod->deps);
      return 1;
    }

  *newin_kernel = dep_add (*newin_kernel, mod);

  return 0;
}

/*
 * Check if a module is referenced by something else
 */

static int
is_removable (char *mod)
{
  mod = strip_o (mod);

#ifndef NO_COMPAT
  if (!flag_new_syscalls)
    return (old_in_kernel (mod) == 1);
  else
#endif
    {
      size_t ret;

      if (!in_kernel (mod))
	return 0;
      query_module (mod, QM_REFS, NULL, 0, &ret);
      return ret == 0;
    }
}

/*
 * Unload a module and whichever modules where required by this module.
 */
static int
unload (char *mod)
{
  int ret = 0;
  struct mod_path *objs = NULL;
  struct mod_path *tmp;

  /*
   * Ignore if the module doesn't exist or it's used by someone else.
   */

  if (!mod || !is_removable (mod))
    return 0;

  /*
   * If there is no information about a module in
   * the dependancy file, we remove it without further checking.
   */
  if (!(objs = locate_mod_obj (mod, NULL)) || objs == (void *) (-1))
    return delete_module (strip_o (mod));

  /*
   * Otherwise we try to kill all instantations of it.
   */
  ret = 0;
  for (tmp = objs; tmp; tmp = tmp->next)
    if (in_kernel (tmp->path))
      {
	struct dep_node *nod;

	if (!(nod = dep_lookup (file_deps, tmp->path)))
	  lprintf ("no dependency information for module %s", mod);
	else
	  {
	    struct dep_node *deps;

	    ret = exec_rmmod_cmd (tmp->path);

	    for (deps = nod->deps; deps; deps = deps->next)
	      unload (deps->name);
	  }
      }

  while ((tmp = objs))
    {
      objs = objs->next;
      free (tmp->path);
      free (tmp);
    }
  return 0;
}

/*
 * Load modules specified on the command line
 */
static int
load_from_list (char *list[], int n, char *type, int loadall)
{
  int ret = 1;
  int i;

  for (i = 0; i < n; i++)
    {
      struct dep_node *kernel_deps = NULL;

      /*
       * We can pass options to the module via modprobe's command line.
       * It goes like this:
       * /sbin/modprobe module opt1=value opt2=value [ othermodule ...]
       * An option is a keyword followed by an equal sign and a value.
       * No spaces are allowed in the sequence, unless it is quoted.
       *
       * The option list ends at the end of the list or at the
       * first non-option argument (a module).
       */
      if (strchr (list[i], '=') == NULL)
	{
	  struct mod_path *objs = NULL;
	  struct mod_path *tmp;

	  objs = locate_mod_obj (list[i], type);
	  if (!objs)
	    lprintf ("can't locate module %s", list[i]);
	  else if (objs != (void *) (-1))
	    {
	      for (tmp = objs; tmp; tmp = tmp->next)
		{
		  if (insmod (tmp->path, &kernel_deps, &list[i]) == 0)
		    {
		      ret = 0;
		      if (!loadall)	/* stop after fist success */
			break;
		    }
		}
	      while ((tmp = objs))
		{
		  objs = objs->next;
		  free (tmp->path);
		  free (tmp);
		}
	    }
	}

      deps_free (kernel_deps);
      kernel_deps = NULL;

      if (ret == 0 && !loadall)
	break;
    }

  return ret;
}

/*
 * Print all available modules matching "pattern" and of a certain type.
 */
static void
print_list (const char *pattern, const char *type)
{
  struct mod_path *mods = NULL;
  struct mod_path *tmp;

  mods = find_matching_mods (pattern, type, 1);
  while ((tmp = mods))
    {
      printf ("%s\n", mods->path);
      mods = mods->next;
      free (tmp->path);
      free (tmp);
    }
}

/*
 * Print usage information and exit.
 */
static void
usage (void)
{
  printf ("Usage: modprobe [-a] [ -t TYPE ] MODULE [opt=val ...] ...\n"
	  "       modprobe -c\n"
	  "Load MODULE_1 MODULE_2, and modules needed by them\n"
	  "with the specified options.\n\n"
	  "  -a, --all                  load all modules\n"
	  "  -c, --show-conf            show the current modules configuration and exit\n"
	  "  -d, --debug                run in debug mode\n"
	  "  -k, --kernel-daemon        only used by the kernel daemon\n"
	  "  -l, --list                 list currently available modules\n"
	  "  -r, --remove               unload modules from the kernel\n"
  "  -s, --system-log           use the system logging for error reporting\n"
	  "  -t TYPE,\n"
    "  -type TYPE                 restrict actions to modules of the TYPE\n"
	  "      --help                 display this help and exit\n"
	  "  -v, --verbose              run in verbose mode\n"
	"  -V, --version              output version information and exit\n"
	  "\n"
    );
}

static void
nothing (const char *str)
{
  lprintf ("argument missing for %s\n"
	 "Please specify at least one module or a wildcard like \\*.", str);
}


int
main (int argc, char *argv[])
{
  int ret = 0;
  char *type = NULL;		/* Search in all path[] */

  int flag_remove = 0;
  int flag_list = 0;
  int flag_load_all = 0;	/* Load only one module out of a list */

  int opt_tag;

  if (argc == 1)
    {
      usage ();
      return 1;
    }

  if (read_config_file (NULL))
    return 1;

  if (read_deps_file (depfile))
    return 1;

  /*
   * Yes we are using getopts!
   */
  while (1)
    {
      static struct option long_opts[] =
      {
	{"all", 0, 0, 'a'},
	{"show-conf", 0, 0, 'c'},
	{"debug", 0, 0, 'd'},
	{"kernel-daemon", 0, 0, 'k'},
	{"list", 0, 0, 'l'},
	{"remove", 0, 0, 'r'},
	{"system-log", 0, 0, 's'},
	{"type", 1, 0, 't'},
	{"verbose", 0, 0, 'v'},
	{"version", 0, 0, 'V'},
	{"help", 0, 0, 'h'},
	{0, 0, 0, 0}		/* Table end tag */
      };
      int opt_ind = 0;

      opt_tag = getopt_long (argc, argv, "acklrst:vV", long_opts,
			     &opt_ind);
      if (opt_tag == -1)
	break;

      switch (opt_tag)
	{
	case 'a':
	  flag_load_all = 1;
	  break;

	case 'c':
	  if (argc != 2)
	    {			/* allow it only beeing used exclusive */
	      usage ();
	      exit (1);
	    }
	  print_active_config ();	/* show configuration */
	  exit (0);

	case 'd':
	  flag_debug = 1;
	  break;

	case 'k':
	  flag_by_kerneld = 1;	/* called by kernel daemon */
	  break;

	case 'l':
	  flag_list = 1;	/* list modules of certain kind */
	  break;

	case 'r':
	  flag_remove = 1;
	  break;

	case 's':
	  setsyslog ("modprobe");	/* use the syslog for reporting */
	  break;

	case 't':
	  type = xstrdup (optarg);
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
	  puts ("modprobe (Linux modutils) " MODUTILS_VERSION);
	  if (argc != 2)
	    putchar ('\n');
	  break;

	default:
	  abort ();
	}
    }

  if (ret == -1)
    return 1;

  /*
   * Skip all automatically processed options.
   */
  argc -= optind;
  argv += optind;

  /*
   * argv now points to the first non-option argument
   * argc is the remaining argument count
   */

#ifndef NO_COMPAT
  flag_new_syscalls = !query_module (NULL, 0, NULL, 0, NULL);
#endif

  if (flag_remove)
    {
      if (argc > 0)
	for (; argc > 0 && ret == 0; ++argv, --argc)
	  ret = unload (*argv);
      else
	nothing ("remove");
    }
  else if (flag_list)
    {
      if (argc > 0)
	for (; argc > 0 && ret == 0; ++argv, --argc)
	  print_list (*argv, type);
      else
	print_list ("*", type);
    }
  else
    {
      if (argc > 0)
	ret = load_from_list (argv, argc, type, flag_load_all);
      else
	nothing ("load");
    }

  verbose ("\r");
  return ret;
}
