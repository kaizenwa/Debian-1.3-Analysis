/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */


#include "../exim.h"
#include "aliasfile.h"



/* Options specific to the aliasfile director. */

optionlist aliasfile_director_options[] = {
  { "*expand_group",      opt_stringptr | opt_hidden,
      (void *)(offsetof(aliasfile_director_options_block, expand_gid)) },
  { "*expand_user",       opt_stringptr | opt_hidden,
      (void *)(offsetof(aliasfile_director_options_block, expand_uid)) },
  { "*set_group",         opt_bool | opt_hidden,
      (void *)(offsetof(aliasfile_director_options_block, gid_set)) },
  { "*set_user",          opt_bool | opt_hidden,
      (void *)(offsetof(aliasfile_director_options_block, uid_set)) },
  { "current_directory",     opt_stringptr,
      (void *)(offsetof(aliasfile_director_options_block, current_dir)) },
  { "directory",          opt_stringptr | opt_hidden,
      (void *)(offsetof(aliasfile_director_options_block, home_dir)) },
  { "errors_to",          opt_stringptr,
      (void *)(offsetof(aliasfile_director_options_block, errors_to)) },
  { "expand",             opt_bool,
      (void *)(offsetof(aliasfile_director_options_block, expand)) },
  { "file",               opt_stringptr,
      (void *)(offsetof(aliasfile_director_options_block, file)) },
  { "forbid_file",        opt_bool,
      (void *)(offsetof(aliasfile_director_options_block, forbid_file)) },
  { "forbid_pipe",        opt_bool,
      (void *)(offsetof(aliasfile_director_options_block, forbid_pipe)) },
  { "freeze_missing_include", opt_bool,
      (void *)(offsetof(aliasfile_director_options_block, freeze_missing_include)) },
  { "group",              opt_expand_gid,
      (void *)(offsetof(aliasfile_director_options_block, gid)) },
  { "home_directory",     opt_stringptr,
      (void *)(offsetof(aliasfile_director_options_block, home_dir)) },
  { "initgroups",         opt_bool,
      (void *)(offsetof(aliasfile_director_options_block, initgroups)) },
  { "modemask",           opt_octint,
      (void *)(offsetof(aliasfile_director_options_block, modemask)) },
  { "optional",           opt_bool,
      (void *)(offsetof(aliasfile_director_options_block, optional)) },
  { "owners",             opt_uidlist,
      (void *)(offsetof(aliasfile_director_options_block, owners)) },
  { "owngroups",          opt_gidlist,
      (void *)(offsetof(aliasfile_director_options_block, owngroups)) },
  { "query",              opt_stringptr,
      (void *)(offsetof(aliasfile_director_options_block, query)) },
  { "rewrite",            opt_bool,
      (void *)(offsetof(aliasfile_director_options_block, rewrite)) },
  { "search_type",        opt_searchtype,
      (void *)(offsetof(aliasfile_director_options_block, search_type)) },
  { "skip_syntax_errors", opt_bool,
      (void *)(offsetof(aliasfile_director_options_block, skip_syntax_errors)) },
  { "user",               opt_expand_uid,
      (void *)(offsetof(aliasfile_director_options_block, uid)) }
};

/* Size of the options list. An extern variable has to be used so that its
address can appear in the tables drtables.c. */

int aliasfile_director_options_count =
  sizeof(aliasfile_director_options)/sizeof(optionlist);

/* Default private options block for the aliasfile director. */

aliasfile_director_options_block aliasfile_director_option_defaults = {
  NULL,     /* file */
  NULL,     /* query */
  NULL,     /* home_dir */
  NULL,     /* current_dir */
  NULL,     /* errors_to */
  NULL,     /* expand_uid */
  NULL,     /* expand_gid */
  -1,       /* uid */
  -1,       /* gid */
  022,      /* modemask */
  -1,       /* search_type */
  NULL,     /* owners */
  NULL,     /* owngroups */
  FALSE,    /* uid_set */
  FALSE,    /* gid_set */
  FALSE,    /* initgroups */
  FALSE,    /* expand */
  FALSE,    /* optional */
  FALSE,    /* forbid_file */
  FALSE,    /* forbid_pipe */
  TRUE,     /* freeze_missing_include */
  TRUE,     /* rewrite */
  FALSE     /* skip_syntax_errors */
};



/*************************************************
*          Initialization entry point            *
*************************************************/

/* Called for each instance, after its options have been read, to
enable consistency checks to be done, or anything else that needs
to be set up.

Argument:
  dblock       a pointer to the director instance block

Returns:       nothing
*/

void
aliasfile_director_init(director_instance *dblock)
{
aliasfile_director_options_block *ob =
  (aliasfile_director_options_block *)(dblock->options_block);

/* If a fixed uid field is set, then a gid field must also be set. */

if (ob->uid_set && !ob->gid_set && ob->expand_gid == NULL)
  log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
    "user set without group for the %s director", dblock->name);

/* A search type is mandatory */

if (ob->search_type < 0)
  log_write(0, LOG_PANIC_DIE|LOG_CONFIG2, "%s director:\n  "
    "a search type option is required", dblock->name);

/* For single-key+file search types, a file name is mandatory; for other types
a query is mandatory. An absolute file name is mandatory for lsearch and dbm;
but can't check for absoluteness if the name is being looked up. */

if (ob->search_type < stype_querystyle)
  {
  if (ob->query != NULL)
    log_write(0, LOG_PANIC_DIE|LOG_CONFIG2, "%s director:\n  "
      "\"query\" specified for a single-key search type", dblock->name);

  if (ob->file == NULL)
    log_write(0, LOG_PANIC_DIE|LOG_CONFIG2, "%s director:\n  "
      "no file name specified", dblock->name);

  if ((ob->search_type == stype_lsearch || ob->search_type == stype_dbm) &&
       ob->file[0] != '/' && ob->file[0] != '$')
    log_write(0, LOG_PANIC_DIE|LOG_CONFIG2, "%s director:\n  "
      "an absolute file path name is required for lsearch or dbm", dblock->name);
  }

/* Non-single-key search type */

else
  {
  if (ob->file != NULL)
    log_write(0, LOG_PANIC_DIE|LOG_CONFIG2, "%s director:\n  "
      "\"file\" specified for a non-single-key search type", dblock->name);

  if (ob->query == NULL)
    log_write(0, LOG_PANIC_DIE|LOG_CONFIG2, "%s director:\n  "
      "no query specified", dblock->name);
  }
}




/*************************************************
*              Main entry point                  *
*************************************************/

/* See local README for interface description */

int
aliasfile_director_entry(
  director_instance *dblock,      /* data for this instantiation */
  address_item *addr,             /* address we are working on */
  address_item **addr_local,      /* add it to this if it's local */
  address_item **addr_remote,     /* add it to this if it's remote */
  address_item **addr_new,        /* put new addresses on here */
  address_item **addr_succeed,    /* put the old one here on success */
  BOOL verify)                    /* true if verifying */
{
aliasfile_director_options_block *ob =
  (aliasfile_director_options_block *)(dblock->options_block);
address_item *generated = NULL;
char *errors_to = addr->errors_address;
char *filename;
char *query;
char *aliastext;
char *error;
void *handle;
error_block *eblock = NULL;
uid_t uid = ob->uid;
gid_t gid = ob->gid;
struct passwd *upw = NULL;
BOOL uid_set = ob->uid_set;
BOOL gid_set = ob->gid_set;
int  extracted;

/* Do file existence tests */

switch (match_exists(dblock->require_files))
  {
  case FAIL:
  DEBUG(9) debug_printf("%s director skipped: file existence failure\n",
    dblock->name);
  return FAIL;

  case DEFER:
  addr->message = string_sprintf("file existence defer in %s director: %s",
    dblock->name, strerror(errno));
  return DEFER;
  }

/* For single-key+file search types, set the required file name and expand it.
If the expansion fails, log the incident and indicate an internal error. The
file name has already been checked for absoluteness, at initialization time,
but only if it did not start with an expansion, so we double check here. */

if (ob->search_type < stype_querystyle)
  {
  query = addr->local_part;
  filename = expand_string(ob->file);
  if (filename == NULL)
    {
    log_write(0, LOG_MAIN|LOG_PANIC, "%s director failed to expand %s: %s",
      dblock->name, ob->file, expand_string_message);
    addr->message = string_sprintf("%s director failed to expand %s: %s",
      dblock->name, ob->file, expand_string_message);
    addr->special_action = SPECIAL_FREEZE;
    return ERROR;
    }
  else if ((ob->search_type == stype_lsearch || ob->search_type == stype_dbm) &&
           filename[0] != '/')
    {
    log_write(0, LOG_MAIN|LOG_PANIC, "%s director requires absolute file name "
      "for lsearch or dbm: "
      "%s generated from expanding %s", dblock->name, filename, ob->file);
    addr->special_action = SPECIAL_FREEZE;
    return ERROR;
    }

  DEBUG(2) debug_printf("%s director: file = %s search type = %d\n",
    dblock->name, filename, ob->search_type);
  }

/* For query-style lookups, expand the query and set the filename NULL */

else
  {
  filename = NULL;
  query = expand_string(ob->query);
  if (query == NULL)
    {
    log_write(0, LOG_MAIN|LOG_PANIC, "%s director failed to expand %s: %s",
      dblock->name, ob->query, expand_string_message);
    addr->message = string_sprintf("%s director failed to expand %s: %s",
      dblock->name, ob->query, expand_string_message);
    addr->special_action = SPECIAL_FREEZE;
    return ERROR;
    }
  }

/* Open the file (or whatever) for searching, according to the search type that
is set. The search functions return a handle identifying the search. For files
this is a FILE * or a DBM *; for other things is is < 0. If the optional flag
is set, failure to open is not an error; we just fail to direct. */

DEBUG(9) debug_printf("file=%s query=%s\n",
  (filename == NULL)? "NULL" : filename, query);

handle = search_open(filename, ob->search_type, ob->modemask, ob->owners,
  ob->owngroups, &error);

if (handle == NULL)
  {
  if (ob->optional && errno == ENOENT)
    {
    DEBUG(2) debug_printf("%s director skipped: file failed to open and "
      "optional flag set\n", dblock->name);
    return FAIL;
    }
  addr->basic_errno = ERRNO_BADALIAS;
  addr->message = error;
  log_write(0, LOG_MAIN|LOG_PANIC, "%s director: %s", dblock->name, error);
  return ERROR;
  }

/* Now search the file (or whatever) for the entry we are interested in.
The text is returned in dynamic store. */

aliastext = search_find(handle, filename, query, ob->search_type, 0, NULL,
  &error);

if (aliastext == NULL)
  {
  DEBUG(2) debug_printf("%s director failed for %s: %s\n", dblock->name,
    addr->local_part, error);
  return FAIL;
  }

/* If the expand option is set, pass the text through the string expander. */

if (ob->expand)
  {
  char *newtext = expand_string(aliastext);
  if (newtext == NULL)
    {
    log_write(0, LOG_MAIN, "%s director failed to expand %s (generated from "
      "local part %s)", dblock->name, aliastext, addr->local_part);
    addr->special_action = SPECIAL_FREEZE;
    return ERROR;
    }
  store_free(aliastext);
  aliastext = newtext;
  }

/* If there is no fixed uid set, see if there's a dynamic one that can
be expanded and possibly looked up. Exim panics if such a lookup fails. */

if (!uid_set && ob->expand_uid != NULL)
  {
  direct_find_expanded_user(ob->expand_uid, dblock->name, "director",
    &upw, &uid);
  uid_set = TRUE;
  }

/* Likewise for the gid */

if (!gid_set && ob->expand_gid != NULL)
  {
  direct_find_expanded_group(ob->expand_gid, dblock->name, "director", &gid);
  gid_set = TRUE;
  }

/* If a uid is set, then a gid must also be available; use one from the passwd
lookup if it happened. */

if (uid_set && !gid_set)
  {
  if (upw != NULL)
    {
    gid = upw->pw_gid;
    gid_set = TRUE;
    }
  else
    log_write(0, LOG_PANIC_DIE, "User set without group for %s director",
      dblock->name);
  }

/* If this director has a local errors_to setting for where to send error
messages for its children, expand it, and then check that it is a valid
address before using it, except when just verifying an address. Otherwise
there could be directing loops if someone sets up a silly configuration. */

if (ob->errors_to != NULL)
  {
  char *s = expand_string(ob->errors_to);
  if (s == NULL)
    {
    log_write(0, LOG_MAIN, "%s director failed to expand %s", dblock->name,
      ob->errors_to);
    addr->special_action = SPECIAL_FREEZE;
    return ERROR;
    }

  if (verify) errors_to = s; else
    {
    char *snew;
    if (verify_address(s, TRUE, TRUE, NULL, NULL, &snew, FALSE, FALSE) == OK)
      errors_to = snew;
    }
  }

/* If there is a transport specified for the director, then set up this
address to use that transport. Ignore the alias text. */

if (dblock->transport != NULL)
  {
  addr->transport = dblock->transport;

  if (errors_to != NULL) addr->errors_address = errors_to;
  addr->director = dblock;
  addr->home_dir = ob->home_dir;
  addr->current_dir = ob->current_dir;
  if (uid_set)
    {
    addr->uid = uid;
    addr->uid_set = TRUE;
    }
  if (gid_set)
    {
    addr->gid = gid;
    addr->gid_set = TRUE;
    addr->initgroups = ob->initgroups;
    }

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

  DEBUG(2)
    {
    debug_printf("  queued for %s transport ", dblock->transport->name);
    if (addr->uid_set) debug_printf("uid=%d ", (int)(addr->uid));
      else debug_printf("uid=unset ");
    if (addr->gid_set) debug_printf("gid=%d\n", (int)(addr->gid));
      else debug_printf("gid=unset\n");
    }

  return OK;
  }

/* There is a common function for use by aliasing and aliasing directors that
extracts a list of addresses from a text string. Setting the fourth-last
argument FALSE makes generating no addresses an error. However, setting the
third-last TRUE allows an address that is ":blackhole:" in the alias file
to generate nothing without causing an error. Setting the last argument NULL
causes no checks to be made on :include: files. */

extracted = parse_extract_addresses(aliastext, &generated, &error, FALSE,
  TRUE, ob->rewrite, NULL, ob->skip_syntax_errors? &eblock : NULL);
store_free(aliastext);

/* If extraction failed, return error and freeze, unless it was a missing
include file and no_freeze_missing_include is set. */

if (extracted != 0)
  {
  addr->basic_errno = ERRNO_BADALIAS;
  addr->message =
    string_sprintf("<%s> - error in alias file: %s", addr->orig, error);
  if (extracted > 0 && !ob->freeze_missing_include) return DEFER;
  addr->special_action = SPECIAL_FREEZE;
  return ERROR;
  }

/* If skip_syntax_errors was set and there were syntax errors in the list,
error messages will be present in eblock. Log them. */

while (eblock != NULL)
  {
  log_write(0, LOG_MAIN, "%s director: syntax error in alias file %s: "
    "%s in \"%s\"", dblock->name, filename, eblock->text1, eblock->text2);
  eblock = eblock->next;
  }

/* Add the new addresses to the list of new addresses, copying in the
uid, gid and permission flags for use by pipes and files, setting
the parent, and or-ing its ignore_error flag. */

while (generated != NULL)
  {
  address_item *next = generated;
  generated = next->next;
  next->parent = addr;
  next->ignore_error |= addr->ignore_error;
  addr->child_count++;

  next->next = *addr_new;
  *addr_new = next;

  if (errors_to != NULL) next->errors_address = errors_to;

  if (next->pfr)
    {
    next->director = dblock;
    if (uid_set)
      {
      next->uid = uid;
      next->uid_set = TRUE;
      }
    if (gid_set)
      {
      next->gid = gid;
      next->gid_set = TRUE;
      next->initgroups = ob->initgroups;
      }
    next->home_dir = ob->home_dir;
    next->home_dir = ob->current_dir;
    next->allow_pipe = !ob->forbid_pipe;
    next->allow_file = !ob->forbid_file;
    }

  DEBUG(2)
    {
    debug_printf("%s director generated %s%s%s%s%s\n",
      dblock->name,
      next->orig,
      next->pfr? " (pipe, file, or autoreply)" : "",
      (errors_to != NULL)? " (errors to " : "",
      (errors_to != NULL)? errors_to : "",
      (errors_to != NULL)? ")" : "");

    if (next->uid_set)
      debug_printf("   uid=%d ", (int)(next->uid));
    else
      debug_printf("   uid=unset ");

    if (next->gid_set)
      debug_printf("gid=%d ", (int)(next->gid));
    else
      debug_printf("gid=unset ");

    debug_printf("home=%s\n", (next->home_dir == NULL)? "null" :
      next->home_dir);
    }
  }

/* If no children were generated for this address, but no error was given,
it means that there was just a :blackhole: entry in the alias file. Log
something. */

if (addr->child_count <= 0 && !verify && !address_test_mode)
  log_write(0, LOG_MAIN, "=> :blackhole: <%s> D=%s", addr->orig, dblock->name);

/* Put the original address onto the succeed queue. This ensures that any
retry item that it acquires gets processed. */

addr->next = *addr_succeed;
*addr_succeed = addr;

return OK;
}

/* End of director/aliasfile.c */
