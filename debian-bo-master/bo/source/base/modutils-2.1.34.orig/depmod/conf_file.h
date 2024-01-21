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

#ifndef CONFIG_H
#define CONFIG_H

#include "version.h"
#define ETC_CONF_MODULES	"/etc/conf.modules"
#define SHELL_WILD "\"\'\140$*?[]{}|\\"

enum command
  {
    PRE_INSTALL, POST_INSTALL, PRE_REMOVE, POST_REMOVE,
    INSTALL, REMOVE
  };

/*
 * This struct is used for reporting path lists of matching files.
 */

struct mod_path
  {
    struct mod_path *next;
    char *path;
  };

/*
 * This is the  list of additional options supplied in /etc/modules.conf
 */
extern struct mod_option
  {
    struct mod_option *next;
    char *module;		/* the module this options will be appended */
    char *args;			/* the options to pass to insmod when loading this module */
  }
 *mod_options;

/*
 * Options for insmod found in the configuration file.
 */
extern char *insmod_opts;

/*
 * The actually active full path name of the dependency file.
 */
extern char *depfile;

extern int read_config_file (char *);
extern struct mod_path *find_matching_mods (const char *match, const char *type, int many);
extern void print_active_config (void);
extern char *find_assoc_cmd (enum command when, char *mod);
extern struct mod_path *locate_mod_obj (char *match, char *type);
#endif
