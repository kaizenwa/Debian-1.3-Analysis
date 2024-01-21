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

#include <stddef.h>
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/utsname.h>
#include <signal.h>
#include <limits.h>
#include <dirent.h>
#include <fnmatch.h>

#include "conf_file.h"
#include "util.h"
#include "misc.h"

/*
 * List of different paths for different sets
 * used like this: "/lib/module/SET/TYPE/bar.o
 */

struct mod_type
  {
    struct mod_type *next;
    char *type;
  };

static struct mod_set
  {
    struct mod_set *next;
    struct mod_type *types;
    char *set;
  }
 *mod_set = NULL;

char *default_types[] =
{
  "fs",
  "misc",
  "net",
  "scsi",
  "block",
  "cdrom",
  "ipv4",
  "ipv6",
  NULL				/* marks the end of the list! */
};

char *default_sets[] =
{
  "/lib/modules",
  "/lib/modules/default",
  NULL
};

/*
 * Operations concearned with the search paths.
 */

static void
add_set (char *set)
{
  struct mod_set *new_set;

  new_set = (struct mod_set *) xmalloc (sizeof (struct mod_set));
  new_set->next = mod_set;
  new_set->types = NULL;
  new_set->set = xstrdup (set);
  mod_set = new_set;
}

static void
add_type (struct mod_set *set, char *type)
{
  struct mod_type *new_type;

  new_type = (struct mod_type *) xmalloc (sizeof (struct mod_type));
  new_type->next = set->types;
  new_type->type = xstrdup (type);
  set->types = new_type;
}

static void
add_option (char *module, char *args)
{
  struct mod_option *new_opt;
  new_opt = (struct mod_option *) xmalloc (sizeof (struct mod_option));
  new_opt->next = mod_options;
  new_opt->module = xstrdup (module);
  new_opt->args = xstrdup (args);
  mod_options = new_opt;
}

static struct action
{
  struct action *next;
  enum command when;
  char *module;
  char *cmd;
}
 *mod_action = NULL;

char *depfile = NULL;
char *insmod_opts = NULL;
struct mod_option *mod_options = NULL;

static struct mod_alias
  {
    struct mod_alias *next;
    char *name;
    char *alias;
  }
alias_list[] =
{
#include "alias.h"
  {
    NULL, NULL, NULL
  }
};

static struct mod_alias *mod_alias = NULL;

static void
add_alias (char *module, char *alias)
{
  struct mod_alias *tmp;

  tmp = (struct mod_alias *) xmalloc (sizeof (struct mod_alias));
  tmp->next = mod_alias;
  tmp->name = xstrdup (module);
  tmp->alias = xstrdup (alias);
  mod_alias = tmp;
}

static void
add_action (enum command when, char *module, char *action)
{
  struct action *tmp;

  tmp = (struct action *) xmalloc (sizeof (struct action));
  tmp->next = mod_action;
  tmp->when = when;
  tmp->module = xstrdup (module);
  tmp->cmd = xstrdup (action);
  mod_action = tmp;
}

static void
release_all_sets (void)
{
  struct mod_set *set;
  struct mod_set *tmp;

  set = mod_set;
  while (set)
    {
      struct mod_type *type;
      struct mod_type *ttmp;
      tmp = set;
      free (set->set);

      type = set->types;
      while (type)
	{
	  ttmp = type;
	  free (type->type);
	  type = type->next;
	  free (ttmp);
	}
      set = set->next;
      free (tmp);
    }
  mod_set = NULL;
}

/*
 * Read the modules configuration file.
 *
 * Error messages are generated.
 */
static int
error_missing_argument (int line)
{
  lprintf ("conf:%d: missing argument\n", line);
  return 1;
}

static int
expect_action (int line, int assign,
	       enum command com,
	       char *cp, char **tmp)
{
  if (assign)
    {
      lprintf ("conf:%d: %s used in assignment\n", line, *tmp);
      return 1;
    }
  if (!cp)
    return error_missing_argument (line);
  *tmp = cp + 1;
  cp = strchr (*tmp, ' ');
  if (!cp)
    return error_missing_argument (line);
  *cp = '\0';

  add_action (com, *tmp, cp + 1);

  return 0;
}

int
read_config_file (char *kernel_ver)
{
  struct utsname uts_info;
  int drop_default_paths = 1;
  char *depfile_tmp;
  char **list;
  struct mod_set *set;
  char *def_set;
  char *config;
  char *tmp = NULL;
  char *end;
  int line = 0;			/* current line of the configuration file */
  char linebuf [8192];

  /*
   * Initialize the list of predefined aliases.
   */
  mod_alias = alias_list;
  while (mod_alias[1].name && mod_alias[1].alias)
    {
      mod_alias->next = mod_alias + 1;
      ++mod_alias;
    }
  mod_alias = alias_list;

  /*
   * Create the full name of the output file.
   */
  if (kernel_ver)
    {
      depfile_tmp = (char *) xmalloc (strlen ("/lib/modules//modules.dep") +
				      strlen (kernel_ver) + 1);
      strcpy (depfile_tmp, "/lib/modules/");
      strcat (depfile_tmp, kernel_ver);
      strcat (depfile_tmp, "/modules.dep");
    }
  else
    depfile_tmp = xstrdup ("");

  depfile = depfile_tmp;

  /*
   * Create the primary path for modules:
   */

  list = default_sets;
  while (*list)
    {
      add_set (*list);
      ++list;
    }

  if (kernel_ver)
    tmp = kernel_ver;
  else
    {
      uname (&uts_info);
      tmp = uts_info.release;
    }
  def_set = (char *) xmalloc (strlen ("/lib/modules/") + strlen (tmp) + 1);
  strcpy (def_set, "/lib/modules/");
  strcat (def_set, tmp);
  add_set (def_set);

  free (def_set);

  /*
   * Add the default types to the just created global list.
   */
  for (set = mod_set; set; set = set->next)
    {
      char **type;

      for (type = default_types; *type; ++type)
	add_type (set, *type);
    }

  /*
   * Now read the configuration file.
   */

  config = NULL;
  if (!access ("/etc/modules.conf", R_OK))
    config = read_and_preprocess_file ("/etc/modules.conf");
  if (!config)
    config = read_and_preprocess_file ("/etc/conf.modules");

  /*
   * Parse the preprocessed file.
   */
  for (tmp = config; (end = get_concat_line (tmp, &line)); tmp = end)
    {
      char *cp;
      int assign = 0;

      if (!*tmp)		/* ignore blank lines */
	continue;

      tmp = resolve_string (tmp, linebuf, sizeof (linebuf));

      cp = strpbrk (tmp, " =");
      if (cp)
	{
	  if (*cp == '=')
	    assign = 1;
	  else
	    {

	      if (cp[1] == '=')
		{
		  assign = 1;
		  *cp = '\0';
		  ++cp;
		}
	      else
		assign = 0;
	    }
	  *cp = '\0';
	}

      /*
       * Now interpret the configuration lines
       */
      if (!strcmp (tmp, "keep"))
	{
	  if (cp)
	    {
	      lprintf ("conf:%d: \"keep\" used with argument\n", line);
	      return 1;
	    }
	  drop_default_paths = 0;
	}
      else if (!strcmp (tmp, "insmod_opt"))
	{
	  if (!assign)
	    {
	      lprintf ("conf:%d: \"insmod_opt\" not within assignment\n", line);
	      return 1;
	    }
	  if (!cp)
	    return error_missing_argument (line);
	  tmp = cp + 1;
	  insmod_opts = xstrdup (tmp);
	}
      else if (!strcmp (tmp, "depfile"))
	{
	  if (!assign)
	    {
	      lprintf ("conf:%d: depfile not within assignment\n", line);
	      return 1;
	    }
	  if (!cp)
	    return error_missing_argument (line);
	  tmp = cp + 1;
	  free (depfile_tmp);
	  depfile_tmp = xstrdup (tmp);
	}
      else if (!strcmp (tmp, "options"))
	{
	  if (assign)
	    {
	      lprintf ("conf:%d: options used in assignment\n", line);
	      return 1;
	    }
	  if (!cp)
	    return error_missing_argument (line);
	  tmp = cp + 1;
	  cp = strchr (tmp, ' ');
	  if (!cp)
	    {
	      lprintf ("conf:%d: missing module argument\n", line);
	      return 1;
	    }
	  *cp = '\0';

	  add_option (tmp, cp + 1);
	}
      else if (!strcmp (tmp, "alias"))
	{
	  if (assign)
	    {
	      lprintf ("conf:%d: alias used in assignment\n", line);
	      return 1;
	    }
	  if (!cp)
	    return error_missing_argument (line);
	  tmp = cp + 1;
	  cp = strchr (tmp, ' ');
	  if (!cp)
	    return error_missing_argument (line);
	  *cp = '\0';

	  if (strchr (cp, ' '))
	    {
	      lprintf ("conf:%d: ambigious alias requested\n", line);
	      return 1;
	    }
	  add_alias (tmp, cp + 1);
	}
      else if (!strcmp (tmp, "pre-install"))
	{
	  if (expect_action (line, assign, PRE_INSTALL, cp, &tmp))
	    return 1;
	}
      else if (!strcmp (tmp, "install"))
	{
	  if (expect_action (line, assign, INSTALL, cp, &tmp))
	    return 1;
	}
      else if (!strcmp (tmp, "post-install"))
	{
	  if (expect_action (line, assign, POST_INSTALL, cp, &tmp))
	    return 1;
	}
      else if (!strcmp (tmp, "pre-remove"))
	{
	  if (expect_action (line, assign, PRE_REMOVE, cp, &tmp))
	    return 1;
	}
      else if (!strcmp (tmp, "remove"))
	{
	  if (expect_action (line, assign, REMOVE, cp, &tmp))
	    return 1;
	}
      else if (!strcmp (tmp, "post-remove"))
	{
	  if (expect_action (line, assign, POST_REMOVE, cp, &tmp))
	    return 1;
	}
      else if (strstr (tmp, "path") == tmp)
	{
	  if (!assign)
	    {
	      lprintf ("conf:%d: path not within assignment\n", line);
	      return 1;
	    }
	  if (!cp)
	    return error_missing_argument (line);
	  if (strchr (tmp, '['))
	    {
	      char *type;

	      if (tmp != strstr (tmp, "path["))
		{
		  lprintf ("conf:%d: syntax error \"%s\"\n", line, tmp);
		  return 1;
		}
	      type = strchr (tmp, '[') + 1;
	      if (!(tmp = strchr (tmp, ']')))
		{
		  lprintf ("conf:%d: missing \"]\"\n", line);
		  return 1;
		}
	      *tmp = '\0';
	      if (tmp == type)
		{
		  lprintf ("conf:%d: empty cathegory in path[...]\n", line);
		  return 1;
		}
	      if (drop_default_paths)
	      {
		/* We only do it once. */
		drop_default_paths = 0;
		release_all_sets ();
	      }
	      add_set (cp + 1);
	      printf ("TYPE %s/%s\n", cp + 1, type);
	      add_type (mod_set, type);
	    }
	  else
	    {
	      char **type;

	      if (strcmp (tmp, "path"))
		{
		  lprintf ("conf:%d: unknown keyword \"%s\"\n", line, tmp);
		  return 1;
		}
	      if (drop_default_paths)
	      {
		/* We only do it once. */
		drop_default_paths = 0;
		release_all_sets ();
	      }
	      add_set (cp + 1);
	      /*
	       * And now add all default types to this set.
	       */
	      for (type = default_types; *type; ++type)
		add_type (mod_set, *type);
	    }
	}
      else
	{
	  lprintf ("conf:%d: unknown keyword \"%s\"\n", line, tmp);
	  return 1;
	}
    }
  free (config);

  /*
   * If the dependancy file wasn't specified by the configuration file and
   * we where not forced to use one special, then give a default:
   */
  if (!*depfile_tmp)
    {
      if (kernel_ver)
	tmp = kernel_ver;
      else
	{
	  uname (&uts_info);
	  tmp = uts_info.release;
	}
      free (depfile_tmp);
      depfile_tmp = (char *) xmalloc (strlen ("/lib/modules//modules.dep") +
				      strlen (tmp) + 1);
      strcpy (depfile_tmp, "/lib/modules/");
      strcat (depfile_tmp, tmp);
      strcat (depfile_tmp, "/modules.dep");
    }
  depfile = depfile_tmp;

  return 0;
}

/*
 * Find all modules matching the globbing pattern "match" in directoryies of
 * type "type". "many" specifies if we are looking for all possible matches
 * or only the first one.
 */
struct mod_path *
find_matching_mods (const char *match, const char *type, int many)
{
  struct mod_set *set;
  struct mod_path *it = NULL;

  /*
   * Go through all classes looking for the type.
   */
  for (set = mod_set; set; set = set->next)
    {
      struct mod_type *t;
      for (t = set->types; t; t = t->next)
	if (!type || !strcmp (type, t->type))
	  {
	    struct stat statb;
	    DIR *dp;
	    struct dirent *ep;
	    char *dir = (char *) xmalloc (strlen (set->set)
					  + strlen (t->type) + 2);
	    strcpy (dir, set->set);
	    strcat (dir, "/");
	    strcat (dir, t->type);

	    /*
	     * OK. Now we know where to search. Go through the contents of this
	     * directory and search for matches.
	     */

	    dp = opendir (dir);
	    if (dp)
	      while ((ep = readdir (dp)))
		{
		  /*
		   * Look if it's a regular file!
		   */
		  char *file = (char *) xmalloc (strlen (dir)
						 + strlen (ep->d_name) + 2);
		  strcpy (file, dir);
		  strcat (file, "/");
		  strcat (file, ep->d_name);

		  /*
		   * Check if it's a regular file and we have the permissions
		   * to access it.
		   */
		  if (stat (file, &statb) == -1
		      || (statb.st_mode & S_IFMT) != S_IFREG
		      || access (file, R_OK))
		    {
		      free (file);
		      continue;
		    }

		  /*
		   * And now check if it is matching our search pattern
		   */

		  if (!fnmatch (match, ep->d_name, FNM_PERIOD))
		    {
		      struct mod_path *tmp =
		      (struct mod_path *) xmalloc (sizeof (struct mod_path));
		      tmp->next = it;
		      tmp->path = file;
		      it = tmp;

		      if (!many)	/* search only for the first match */
			{
			  free (dir);
			  return it;
			}
		    }
		  else
		    free (file);
		}
	    free (dir);
	  }
    }

  return it;
}

/*
 * Print out the currently active configuration.
 */
void
print_active_config (void)
{
  struct mod_set *set;
  struct mod_option *opt;
  struct mod_alias *alias;
  struct action *action;

  printf ("# This file was generated by: modprobe -c (" MODUTILS_VERSION ")\n");

  /*
   * Go through all classes looking for the type.
   */

  for (set = mod_set; set; set = set->next)
    {
      struct mod_type *t;
      if ((t = set->types))
	while (t)
	  {
	    printf ("path[%s]=%s\n", t->type, set->set);
	    t = t->next;
	  }
      else
	printf ("path=%s\n", set->set);
    }
  puts ("# Aliases");
  for (alias = mod_alias; alias; alias = alias->next)
    printf ("alias %s %s\n", alias->name, alias->alias);
  puts ("# Options");
  for (opt = mod_options; opt; opt = opt->next)
    printf ("options %s %s\n", opt->module, opt->args);

  if (mod_action)
    {
      puts ("# Commands");
      for (action = mod_action; action; action = action->next)
	{
	  switch (action->when)
	    {
	    case PRE_INSTALL:
	      printf ("pre-install ");
	      break;
	    case INSTALL:
	      printf ("install ");
	      break;
	    case POST_INSTALL:
	      printf ("post-install ");
	      break;
	    case PRE_REMOVE:
	      printf ("pre-remove ");
	      break;
	    case REMOVE:
	      printf ("remove ");
	      break;
	    case POST_REMOVE:
	      printf ("post-remove ");
	      break;
	    }
	  printf ("%s %s\n", action->module, action->cmd);
	}
    }
}

/*
 * Look for a command associated with this action.
 */
char *
find_assoc_cmd (enum command when, char *mod)
{
  char *modname = strip_o (mod);
  struct action *action;

  for (action = mod_action; action; action = action->next)
    if ((action->when == when) && !strcmp (action->module, modname))
      {
	free (modname);
	return action->cmd;
      }

  free (modname);
  return NULL;
}

/*
 * Check if a module name is indeed an alias for another module.
 * Return the real module or this one (mod) if it is not an alias
 */
static char *
translate_alias (char *mod)
{
  struct mod_alias *alias;
  char *modname = strip_o (mod);

  for (alias = mod_alias; alias; alias = alias->next)
    if (!strcmp (alias->name, modname))
      {
	free (modname);
	return alias->alias;
      }

  free (modname);
  return mod;
}


/*
 *  Locate all modules matching the "match".
 */
struct mod_path *
locate_mod_obj (char *match, char *type)
{
  struct mod_path *it = NULL;
  char *match_o;

  if (strchr (match, '/') != NULL)
    {
      /*
       * Absolute path. Don't search any further!
       */

      it = (struct mod_path *) xmalloc (sizeof (struct mod_path));
      it->next = NULL;
      it->path = xstrdup (match);
      return it;
    }

  match = translate_alias (match);
  if (!match || !(*match))
    return NULL;
  if (!strcmp (match, "off"))
    return (void *) (-1);

  match_o = (char *) xmalloc (strlen (match) + 3);
  strcpy (match_o, match);
  strcat (match_o, ".o");

  it = find_matching_mods (match_o, type, 0);
  free (match_o);

  if (!it)
    if (!(it = find_matching_mods (match, type, 0)))
      return NULL;

  return it;
}
