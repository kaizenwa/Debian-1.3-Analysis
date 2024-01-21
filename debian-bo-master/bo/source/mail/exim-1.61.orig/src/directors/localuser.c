/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */


#include "../exim.h"
#include "localuser.h"



/* Options specific to the localuser director. */

optionlist localuser_director_options[] = {
  { "current_directory", opt_stringptr,
      (void *)(offsetof(localuser_director_options_block, current_dir)) },
  { "directory",         opt_stringptr | opt_hidden,
      (void *)(offsetof(localuser_director_options_block, pw_dir)) },
  { "home_directory",    opt_stringptr,
      (void *)(offsetof(localuser_director_options_block, home_dir)) },
  { "initgroups",        opt_bool,
      (void *)(offsetof(localuser_director_options_block, initgroups)) },
  { "match_directory",   opt_stringptr,
      (void *)(offsetof(localuser_director_options_block, pw_dir)) }
};

/* Size of the options list. An extern variable has to be used so that its
address can appear in the tables drtables.c. */

int localuser_director_options_count =
  sizeof(localuser_director_options)/sizeof(optionlist);

/* Default private options block for the localuser director. */

localuser_director_options_block localuser_director_option_defaults = {
  NULL,        /* pw_dir */
  NULL,        /* home_dir */
  NULL,        /* current_dir */
  NULL,        /* re_pw_dir */
  FALSE        /* initgroups */
};



/*************************************************
*          Initialization entry point            *
*************************************************/

/* Called for each instance, after its options have been read, to
enable consistency checks to be done, or anything else that needs
to be set up. */

void localuser_director_init(director_instance *dblock)
{
/*
localuser_director_options_block *ob =
  (localuser_director_options_block *)(dblock->options_block);
*/
if (dblock->transport == NULL)
  log_write(0, LOG_PANIC_DIE|LOG_CONFIG2, "%s director:\n  "
  "a transport is required by the localuser director driver", dblock->name);
}



/*************************************************
*              Main entry point                  *
*************************************************/

/* See local README for interface description. */

int localuser_director_entry(
  director_instance *dblock,      /* data for this instantiation */
  address_item *addr,             /* address we are working on */
  address_item **addr_local,      /* add it to this if it's local */
  address_item **addr_remote,     /* add it to this if it's remote */
  address_item **addr_new,        /* put new addresses on here */
  address_item **addr_succeed,    /* put finished with addresses here */
  BOOL verify)                    /* TRUE when verifying */
{
localuser_director_options_block *ob =
  (localuser_director_options_block *)(dblock->options_block);
struct passwd *pw;

/* The third argument to direct_finduser() must be NULL here, to prevent a
digit string being taken as a numeric uid. */

if (!direct_finduser(addr->local_part, &pw, NULL))
  {
  DEBUG(2) debug_printf("%s director failed for %s: no such user\n",
    dblock->name, addr->local_part);
  return FAIL;
  }

/* If there is a setting for pw_dir, check for a match. Expand the string
if necessary, allowing the home directory to appear in the expansion,
for exotic cases. The outer level resets deliver_home. */

deliver_home = pw->pw_dir;

if (ob->pw_dir != NULL)
  {
  BOOL match;
  char *dir = ob->pw_dir;
  if (strchr(dir, '$') != NULL)
    {
    dir = expand_string(dir);
    if (dir == NULL)
      {
      if (expand_string_forcedfail)
        {
        DEBUG(2) debug_printf("%s director expansion of \"%s\" forced "
          "failure\n", dblock->name, ob->pw_dir);
        return FAIL;
        }
      log_write(0, LOG_MAIN|LOG_PANIC_DIE, "%s director: failed to expand "
        "directory string \"%s\"", dblock->name, ob->pw_dir);
      }
    }
  match = match_check_string(pw->pw_dir, dir, &(ob->re_pw_dir), -1, TRUE);
  if (dir != ob->pw_dir) store_free(dir);
  if (!match)
    {
    DEBUG(2)
      {
      debug_printf("%s director failed to match pw_dir with \"%s\" "
        "for %s\n", dblock->name, ob->pw_dir, addr->local_part);
      if (strcmp(dir, ob->pw_dir) != 0)
        debug_printf("expanded string was \"%s\"\n", dir);
      }
    return FAIL;
    }
  }

/* Do file existence tests */

switch (match_exists(dblock->require_files))
  {
  case FAIL:
  DEBUG(9) debug_printf("%s director failed: file existence failure\n",
    dblock->name);
  return FAIL;

  case DEFER:
  addr->message = string_sprintf("file existence defer in %s director: %s",
    dblock->name, strerror(errno));
  return DEFER;
  }

/* This local user is OK; accept the address */

addr->transport = dblock->transport;
addr->uid = pw->pw_uid;
addr->uid_set = TRUE;
addr->gid = pw->pw_gid;
addr->gid_set = TRUE;
addr->initgroups = ob->initgroups;
if (ob->home_dir != NULL) addr->home_dir = ob->home_dir;
  else addr->home_dir = string_copy(pw->pw_dir);
addr->current_dir = ob->current_dir;

if (addr->transport->info->local)
  {
  addr->next = *addr_local;
  *addr_local = addr;
  }
else
  {
  addr->next = *addr_remote;
  *addr_remote = addr;
  }

return OK;
}

/* End of director/localuser.c */
