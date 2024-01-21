/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */


#include "../exim.h"
#include "forwardfile.h"



/* Options specific to the forwardfile director. */

optionlist forwardfile_director_options[] = {
  { "*expand_group",      opt_stringptr | opt_hidden,
      (void *)(offsetof(forwardfile_director_options_block, expand_gid)) },
  { "*expand_user",       opt_stringptr | opt_hidden,
      (void *)(offsetof(forwardfile_director_options_block, expand_uid)) },
  { "*set_group",         opt_bool | opt_hidden,
      (void *)(offsetof(forwardfile_director_options_block, gid_set)) },
  { "*set_user",          opt_bool | opt_hidden,
      (void *)(offsetof(forwardfile_director_options_block, uid_set)) },
  { "check_group",        opt_bool,
      (void *)(offsetof(forwardfile_director_options_block, check_group)) },
  { "check_local_user",   opt_bool,
      (void *)(offsetof(forwardfile_director_options_block, check_local_user)) },
  { "current_directory",  opt_stringptr,
      (void *)(offsetof(forwardfile_director_options_block, current_dir)) },
  { "directory",          opt_stringptr | opt_hidden,
      (void *)(offsetof(forwardfile_director_options_block, file_dir)) },
  { "errors_to",          opt_stringptr,
      (void *)(offsetof(forwardfile_director_options_block, errors_to)) },
  { "file",               opt_stringptr,
      (void *)(offsetof(forwardfile_director_options_block, file)) },
  { "file_directory",     opt_stringptr,
      (void *)(offsetof(forwardfile_director_options_block, file_dir)) },
  { "filter",             opt_bool,
      (void *)(offsetof(forwardfile_director_options_block, filter)) },
  { "forbid_file",        opt_bool,
      (void *)(offsetof(forwardfile_director_options_block, forbid_file)) },
  { "forbid_filter_log",  opt_bool,
      (void *)(offsetof(forwardfile_director_options_block, forbid_filter_log)) },
  { "forbid_include",     opt_bool,
      (void *)(offsetof(forwardfile_director_options_block, forbid_include)) },
  { "forbid_pipe",        opt_bool,
      (void *)(offsetof(forwardfile_director_options_block, forbid_pipe)) },
  { "forbid_reply",        opt_bool,
      (void *)(offsetof(forwardfile_director_options_block, forbid_reply)) },
  { "freeze_missing_include", opt_bool,
      (void *)(offsetof(forwardfile_director_options_block, freeze_missing_include)) },
  { "group",              opt_expand_gid,
      (void *)(offsetof(forwardfile_director_options_block, gid)) },
  { "home_directory",     opt_stringptr,
      (void *)(offsetof(forwardfile_director_options_block, home_dir)) },
  { "initgroups",         opt_bool,
      (void *)(offsetof(forwardfile_director_options_block, initgroups)) },
  { "modemask",           opt_octint,
      (void *)(offsetof(forwardfile_director_options_block, modemask)) },
  { "owners",             opt_uidlist,
      (void *)(offsetof(forwardfile_director_options_block, owners)) },
  { "owngroups",          opt_gidlist,
      (void *)(offsetof(forwardfile_director_options_block, owngroups)) },
  { "rewrite",            opt_bool,
      (void *)(offsetof(forwardfile_director_options_block, rewrite)) },
  { "seteuid",            opt_bool,
      (void *)(offsetof(forwardfile_director_options_block, use_seteuid)) },
  { "skip_syntax_errors", opt_bool,
      (void *)(offsetof(forwardfile_director_options_block, skip_syntax_errors)) },
  { "user",               opt_expand_uid,
      (void *)(offsetof(forwardfile_director_options_block, uid)) }
};

/* Size of the options list. An extern variable has to be used so that its
address can appear in the tables drtables.c. */

int forwardfile_director_options_count =
  sizeof(forwardfile_director_options)/sizeof(optionlist);

/* Default private options block for the forwardfile director. */

forwardfile_director_options_block forwardfile_director_option_defaults = {
  NULL,     /* file_dir */
  NULL,     /* home_dir */
  NULL,     /* current_dir */
  NULL,     /* file */
  NULL,     /* errors_to */
  NULL,     /* expand_uid */
  NULL,     /* expand_gid */
  -1,       /* uid */
  -1,       /* gid */
  022,      /* modemask */
  NULL,     /* owners */
  NULL,     /* owngroups */
  FALSE,    /* uid_set */
  FALSE,    /* gid_set */
  TRUE,     /* check_local_user */
  FALSE,    /* check_group */
  FALSE,    /* filter */
  FALSE,    /* forbid_filter_log */
  FALSE,    /* forbid_file */
  FALSE,    /* forbid_include */
  FALSE,    /* forbid_pipe */
  FALSE,    /* forbid_reply */
  FALSE,    /* initgroups */
  FALSE,    /* use_seteuid */
  TRUE,     /* freeze_missing_include */
  TRUE,     /* rewrite */
  FALSE     /* skip_syntax_errors */
};



/*************************************************
*          Initialization entry point            *
*************************************************/

/* Called for each instance, after its options have been read, to
enable consistency checks to be done, or anything else that needs
to be set up. */

void forwardfile_director_init(director_instance *dblock)
{
forwardfile_director_options_block *ob =
  (forwardfile_director_options_block *)(dblock->options_block);

/* If a fixed uid field is set, then a gid field must also be set. */

if (ob->uid_set && !ob->gid_set && ob->expand_gid == NULL)
  log_write(0, LOG_PANIC_DIE|LOG_CONFIG,
    "user set without group for the %s director", dblock->name);

/* A file name is mandatory */

if (ob->file == NULL)
  log_write(0, LOG_PANIC_DIE|LOG_CONFIG2, "%s director:\n  "
    "no file name specified", dblock->name);

/* A directory setting is optional, but if it exists it must be absolute,
though we can't check for certain until it has been expanded. */

if (ob->file_dir != NULL && ob->file_dir[0] != '/' &&
    ob->file_dir[0] != '$')
  log_write(0, LOG_PANIC_DIE|LOG_CONFIG2, "%s director:\n  "
    "absolute path name required for the 'directory' option", dblock->name);

/* Permit relative paths only if local user checking is set, or if the
directory option (which must be absolute) is set. */

if (!ob->check_local_user && ob->file[0] != '/' && ob->file[0] != '$' &&
     ob->file_dir == NULL)
  log_write(0, LOG_PANIC_DIE|LOG_CONFIG2, "%s director:\n  "
    "absolute file path required when check_local_user and directory are not set",
    dblock->name);

/* Permit seteuid only if it is configured to be available, and either local
user checking is set, or uid/gid are supplied. */

if (ob->use_seteuid && !have_seteuid)
  log_write(0, LOG_PANIC_DIE|LOG_CONFIG2, "%s director:\n  "
    "seteuid requested but not configured as available",
    dblock->name);

if (ob->use_seteuid && !ob->check_local_user && !ob->uid_set &&
    ob->expand_uid == NULL)
  log_write(0, LOG_PANIC_DIE|LOG_CONFIG2, "%s director:\n  "
    "seteuid requested without check_local_user or explicit uid", dblock->name);

/* A transport must *not* be specified */

if (dblock->transport != NULL)
  log_write(0, LOG_PANIC_DIE|LOG_CONFIG2, "%s director:\n  "
    "a transport is not allowed for this director", dblock->name);
}



/*************************************************
*              Main entry point                  *
*************************************************/

/* See local README for interface description. */

int forwardfile_director_entry(
  director_instance *dblock,      /* data for this instantiation */
  address_item *addr,             /* address we are working on */
  address_item **addr_local,      /* add it to this if it's local */
  address_item **addr_remote,     /* add it to this if it's remote */
  address_item **addr_new,        /* put new addresses on here */
  address_item **addr_succeed,    /* put old address here on success */
  BOOL verify)                    /* TRUE when verifying */
{
forwardfile_director_options_block *ob =
  (forwardfile_director_options_block *)(dblock->options_block);
address_item *generated = NULL;
char *directory = NULL;
char *errors_to = addr->errors_address;
char *filename;
char *filebuf;
char *error;
struct stat statbuf;
struct passwd *pw;
struct passwd *upw = NULL;
error_block *eblock = NULL;
FILE *fwd;
uid_t uid = ob->uid;
gid_t gid = ob->gid;
int   yield = OK;
int   saved_euid = -1;
int   saved_egid = -1;
int   extracted = 0;
BOOL  uid_set = ob->uid_set;
BOOL  gid_set = ob->gid_set;
BOOL  uid_ok = FALSE;
BOOL  gid_ok = !ob->check_group;
BOOL  restore_uid = FALSE;
BOOL  stat_directory = TRUE;
BOOL  delivered = FALSE;
BOOL  is_filter = FALSE;

/* If the check_local_user option is set, check that the local_part is
the login of a local user, and fail if not. Note: the third argument to
direct_finduser() must be NULL here, to prevent a numeric string being
taken as a numeric uid. If the user is found, set directory to the home
directory, and the home expansion variable as well, so that it can be
used while expanding ob->file_dir! */

if (ob->check_local_user)
  {
  if (!direct_finduser(addr->local_part, &pw, NULL))
    {
    DEBUG(2) debug_printf("%s director failed for %s (not a user)\n",
      dblock->name, addr->local_part);
    return FAIL;
    }
  else
    {
    directory = pw->pw_dir;
    deliver_home = directory;
    }
  }

/* If the file_directory option is set expand the string, and set it as the
"home" directory. The expansion can contain $home if check_local_user
is set. */

if (ob->file_dir != NULL)
  {
  directory = expand_string(ob->file_dir);
  if (directory == NULL)
    {
    log_write(0, LOG_MAIN, "%s director failed to expand %s: %s", dblock->name,
      ob->file_dir, expand_string_message);
    addr->special_action = SPECIAL_FREEZE;
    return ERROR;
    }
  deliver_home = directory;
  }

/* Do file existence tests; these can include references to $home. */

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

/* Get the required file name and expand it. If the expansion fails, log the
incident and indicate an internal error. */

filename = expand_string(ob->file);

if (filename == NULL)
  {
  log_write(0, LOG_MAIN, "%s director failed to expand %s: %s", dblock->name,
    ob->file, expand_string_message);
  addr->special_action = SPECIAL_FREEZE;
  return ERROR;
  }

DEBUG(2) debug_printf("%s director: file = %s\n", dblock->name,
  filename);

/* If a directory is set and the file name is not absolute, construct the
complete file name. Otherwise set a flag to prevent an attempt at statting the
directory below. */

if (directory != NULL && filename[0] != '/')
  filename = string_sprintf("%s/%s", directory, filename);
else stat_directory = FALSE;

/* Check that the file name is absolute. Simple checks are done in the
init function, but expansions mean that we have to do a final check here. */

if (filename[0] != '/')
  {
  log_write(0, LOG_MAIN, "%s is not an absolute path for the %s director",
    filename, dblock->name);
  addr->special_action = SPECIAL_FREEZE;
  return ERROR;
  }

/* If there is no fixed explicit uid set, see if there's a dynamic one that can
be expanded and possibly looked up. This uid is used (a) for reading the file
if seteuid is on and (b) for running the transports for generated file and pipe
addresses. It is not (necessarily) the same as the uids that may own the file.
Exim panics if an expanded string is not a number and can't be found in the
password file. */

if (!uid_set && ob->expand_uid != NULL)
  {
  direct_find_expanded_user(ob->expand_uid, dblock->name, "director",
    &upw, &uid);
  uid_set = TRUE;
  }

/* Likewise, set up an expanded gid if necessary. */

if (!gid_set && ob->expand_gid != NULL)
  {
  direct_find_expanded_group(ob->expand_gid, dblock->name, "director", &gid);
  gid_set = TRUE;
  }

/* If a uid was specified, a gid to go with it must also be available. Take it
from the passwd entry if one was looked up for the uid; otherwise it is a
disaster. */

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

/* If the seteuid option is on and a uid is available (either explicit or as a
result of check_local_user), use seteuid() to become the local user. This is
necessary in order to read .forward files that are in NFS-mounted home
directories. The macros expand to -1 on systems without setuid() configured,
but in those cases this code is never obeyed, as the option is locked out in
the init check.

Even if the seteuid option is not on, we need to do this when the security
level is 1 or 3, because in those cases this code is running seteuid to exim.
But only when a uid is available. */

if ((uid_set || ob->check_local_user) &&
    (ob->use_seteuid || security_level == 1 || security_level == 3))
  {
  saved_euid = geteuid();
  saved_egid = getegid();
  if (saved_euid != root_uid) mac_seteuid(root_uid);

  if (uid_set)
    {
    if (ob->initgroups)
      {
      struct passwd *ppw = getpwuid(uid);
      if (ppw != NULL) initgroups(ppw->pw_name, gid);
      }
    mac_setegid(gid);
    mac_seteuid(uid);
    }
  else
    {
    if (ob->initgroups) initgroups(pw->pw_name, pw->pw_gid);
    mac_setegid(pw->pw_gid);
    mac_seteuid(pw->pw_uid);
    }

  DEBUG(2) debug_printf("%s director euid=%d egid=%d\n", dblock->name,
    (int)geteuid(), (int)getegid());
  restore_uid = TRUE;
  }

/* You might think we could just test for the existence of the required file by
attempting to open it, but life isn't that simple. In many large systems,
.forward files in users' home directories are used, with the home directories
possibly NFS-mounted from some remote place. It doesn't seem possible to detect
the state of "NFS mount inaccessible" just by trying to open a file.

The common case is specified with a relative path name (relative to the home
directory or to a specified directory), and in that case we try to do a bit
better by statting the directory first. If it cannot be statted, assume there
is some mounting problem, and defer the delivery. */

if (directory != NULL && stat_directory)
  {
  if (stat(directory, &statbuf) != 0)
    {
    DEBUG(2) debug_printf("%s director failed to stat %s: deferred\n",
      dblock->name, directory);
    addr->message = string_sprintf("%s director failed to stat %s",
      dblock->name, directory);
    yield = DEFER;
    goto RESTORE_UID;             /* skip forward */
    }
  DEBUG(2) debug_printf("successful stat of %s\n", directory);
  }

/* Now try to open the file for reading. If this fails with a non-existence
error, we have no option but to believe that the file does not exist, so the
director gives up on this address. */

fwd = fopen(filename, "r");
if (fwd == NULL)
  {
  if (errno != ENOENT)
    {
    DEBUG(2) debug_printf("%s director failed to open %s: %s: deferred\n",
      dblock->name, filename, strerror(errno));
    addr->message = string_sprintf("%s director failed to open %s: %s",
      dblock->name, filename, strerror(errno));
    addr->special_action = SPECIAL_FREEZE;
    yield = ERROR;
    goto RESTORE_UID;             /* skip forward */
    }

  DEBUG(2) debug_printf("%s director: no file found\n", dblock->name);
  yield = FAIL;
  goto RESTORE_UID;               /* skip forward */
  }


/* Now check up on the mode of the file. It is tempting to do this stat before
opening the file, and use it as an existence check. However, doing that opens a
small security loophole in that the status could be changed before the file is
opened. Can't quite see what problems this might lead to, but you can't be too
careful where security is concerned. Fstat() on an open file can normally be
expected to succeed, but there are some NFS states where it does not. */

if (fstat(fileno(fwd), &statbuf) != 0)
  {
  yield = DEFER;
  goto CLOSE_RESTORE_UID;         /* skip forward */
  }

if ((statbuf.st_mode & S_IFMT) != S_IFREG ||
    (statbuf.st_mode & ob->modemask) != 0)
  {
  addr->basic_errno = ERRNO_BADFORWARD;
  addr->message =
    string_sprintf("<%s> - bad mode (%o) for %s (%s director)",
      addr->orig, statbuf.st_mode, filename, dblock->name);
  yield = DEFER;
  goto CLOSE_RESTORE_UID;         /* skip forward */
  }

/* Check the owner and group, including the current user if check_local_user
was set. If there is nothing to check, let it go. */

if (ob->check_local_user)
  { if (statbuf.st_uid == pw->pw_uid) uid_ok = TRUE; }
else if (ob->owners == NULL) uid_ok = TRUE;

if (!uid_ok && ob->owners != NULL)
  {
  int i;
  for (i = 1; i <= (int)(ob->owners[0]); i++)
    if (ob->owners[i] == statbuf.st_uid) { uid_ok = TRUE; break; }
  }

/* If gid is to be checked, the local user's gid is always permissible. */

if (!gid_ok)
  {
  if (ob->check_local_user)
    { if (statbuf.st_gid == pw->pw_gid) gid_ok = TRUE; }
  if (!gid_ok && ob->owngroups != NULL)
    {
    int i;
    for (i = 1; i <= (int)(ob->owngroups[0]); i++)
      if (ob->owngroups[i] == statbuf.st_gid) { gid_ok = TRUE; break; }
    }
  }

if (!uid_ok || !gid_ok)
  {
  addr->basic_errno = ERRNO_BADFORWARD;
  addr->message =
    string_sprintf("<%s> - bad %s for %s (%s director)",
      addr->orig, uid_ok? "group" : "owner", filename, dblock->name);
  yield = DEFER;
  goto CLOSE_RESTORE_UID;         /* skip forward */
  }

/* Read the .forward file and generate new addresses for each entry therein.
We read the file in one go in order to minimize the time we have it open. */

filebuf = store_malloc(statbuf.st_size + 1);
if (fread(filebuf, 1, statbuf.st_size, fwd) != statbuf.st_size)
  {
  addr->basic_errno = errno;
  addr->message =
    string_sprintf("<%s> - error while reading forward file (%s director)\n",
    addr->orig, dblock->name);
  yield = DEFER;
  goto CLOSE_RESTORE_UID;         /* skip forward */
  }
filebuf[statbuf.st_size] = 0;


/* If the filter option is set, the file is to be interpreted as a filter
file instead of a straight list of addresses, if it starts with
"# Exim filter ..." (any capitilization, spaces optional). */

if (ob->filter)
  {
  char *s = filebuf;
  char *tag = "# exim filter";
  while (isspace(*s)) s++;           /* Skips initial blank lines */
  for (; *tag != 0; s++, tag++)
    {
    if (*tag == ' ')
      {
      while (*s == ' ' || *s == '\t') s++;
      s--;
      }
    else if (tolower(*s) != tolower(*tag)) break;
    }
  if (*tag == 0) is_filter = TRUE;
  }


/* Filter interpretation is done by a general function that is also called from
the filter testing option (-bf). The final argument specifies whether the log
command can be used; this is OK if we have set the uid to a local user, but not
otherwise. Set up the value of extracted to be the same as it is from
parse_extract_addresses(). */

if (is_filter)
  {
  extracted = filter_interpret(filebuf, &generated, &delivered, NULL, &error,
    restore_uid, ob->forbid_filter_log, ob->rewrite)? 0 : -1;
  }

/* Otherwise it's a vanilla .forward file; call parse_extract_addresses()
to get the values. The yield is 0=>OK, -1=>error, +1=> failed to open an
:include: file. */

else
  {
  /* There is a common function for use by forwarding and aliasing
  directors that extracts a list of addresses from a text string.
  Setting the fourth argument TRUE says that generating no addresses
  (from valid syntax) is no error.

  The forward file may include :include: items, and we have to be
  careful about permissions for reading them. The extracting function
  will check that include files begin with a specified string, unless
  NULL is supplied. Supplying "*" locks out :include: files, since they
  must be absolute paths. We lock them out if (a) requested to do so or
  (b) we haven't used seteuid and there's no directory to check. If
  seteuid has been used, just try to read anything; otherwise restrict
  to the directory or lock out if none. */

  extracted = parse_extract_addresses(filebuf, &generated, &error,
    TRUE,                                      /* no addresses => no error */
    FALSE,                                     /* don't recognize :blackhole: */
    ob->rewrite,                               /* rewrite if configured */
    ob->forbid_include? "*" :                  /* includes forbidden */
    restore_uid? NULL :                        /* if seteuid, try all */
    (directory == NULL)? "*" :                 /* if no directory, lock out */
    directory,                                 /* else restrain to directory */
    ob->skip_syntax_errors? &eblock : NULL);
  }

/* The store for holding the forward file is now finished with. */

store_free(filebuf);

/* At this point we are finished with the .forward file. Close it, and, if
seteuid was used above, restore the previous effective uid and gid. The dreaded
goto is used above to skip to this code when errors are detected. */

CLOSE_RESTORE_UID:

fclose(fwd);

RESTORE_UID:

if (restore_uid)
  {
  mac_seteuid(root_uid);
  if (ob->initgroups) setgroups(0, NULL);
  mac_setegid(saved_egid);
  mac_seteuid(saved_euid);
  DEBUG(2) debug_printf("%s director restored euid=%d egid=%d\n", dblock->name,
    (int)geteuid(), (int)getegid());
  }

/* If there has been an error, return the error value now. Subsequently we
can just return directly on error, since there is no further need to mess with
the uid or close the file. */

if (yield != OK) return yield;

/* Extraction failed */

if (extracted != 0)
  {
  /* If extraction from a filter file failed, it is a "probably user error", to
  use a good old IBM term. Just defer delivery and let the user clean things
  up. */

  if (is_filter)
    {
    addr->basic_errno = ERRNO_BADFORWARD;
    addr->message =
      string_sprintf("<%s> - error in filter file: %s", addr->orig,
        error);
    return DEFER;
    }

  /* If extraction from a .forward file failed, freeze and yield ERROR if
  it was a missing :include: file and freeze_missing_include is TRUE. Other-
  wise just DEFER and hope things get fixed eventually. */

  else
    {
    addr->basic_errno = ERRNO_BADFORWARD;
    addr->message =
      string_sprintf("<%s> - error in forward file: %s", addr->orig,
        error);
    if (extracted > 0 && ob->freeze_missing_include)
      {
      addr->special_action = SPECIAL_FREEZE;
      return ERROR;
      }
    else return DEFER;
    }
  }

/* If skip_syntax_errors was set and there were syntax errors in the list,
error messages will be present in eblock. Log them. */

while (eblock != NULL)
  {
  log_write(0, LOG_MAIN, "%s director: syntax error in forward file %s: "
    "%s in \"%s\"", dblock->name, filename, eblock->text1, eblock->text2);
  eblock = eblock->next;
  }

/* If this director has a local errors_to setting for where to send error
messages for its children, expand it, and then check that it is a valid
address before using it, except when verifying. Otherwise there could be
directing loops if a silly config is set. */

if (ob->errors_to != NULL)
  {
  char *s = expand_string(ob->errors_to);
  if (s == NULL)
    {
    log_write(0, LOG_MAIN, "%s director failed to expand %s: %s", dblock->name,
      ob->errors_to, expand_string_message);
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

/* Add the new addresses to the list of new addresses, copying in the
uid, gid and permission flags for use by pipes and files and autoreplies,
setting the parent, and or-ing its ignore_error flag. */

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
    else if (ob->check_local_user)
      {
      next->uid = pw->pw_uid;
      next->uid_set = TRUE;
      }
    if (gid_set)
      {
      next->gid = gid;
      next->gid_set = TRUE;
      }
    else if (ob->check_local_user)
      {
      next->gid = pw->pw_gid;
      next->gid_set = TRUE;
      }
    next->initgroups = ob->initgroups;
    if (ob->home_dir != NULL) next->home_dir = ob->home_dir;
      else if (directory != NULL) next->home_dir = string_copy(directory);
    next->current_dir = ob->current_dir;
    next->allow_pipe = !ob->forbid_pipe;
    next->allow_file = !ob->forbid_file;
    next->allow_reply = !ob->forbid_reply;
    }

  DEBUG(2) debug_printf("%s director generated %s%s%s%s%s\n",
    dblock->name,
    next->orig,
    next->pfr? " (pipe, file, or autoreply)" : "",
    (errors_to != NULL)? " (errors to " : "",
    (errors_to != NULL)? errors_to : "",
    (errors_to != NULL)? ")" : "");
  }

/* If the filter interpreter returned "delivered" then we have succeeded
in completely handling this address. Otherwise, we have to arrange for this
address to be passed on to subsequent directors. Returning FAIL would appear to
be the answer, but it isn't, because successful delivery of the base address
gets it marked "done", so deferred generated addresses never get tried again.
We have to generate a new version of the base address, as if there were a
"deliver" command in the filter file, with the original address as parent.
However, we don't need to do this if there were no generated addresses. */

if (is_filter)
  {
  address_item *next;

  if (!delivered)
    {
    if (addr->child_count <= 0) return FAIL;

    next = deliver_make_addr(addr->orig);
    next->parent = addr;
    next->ignore_error |= addr->ignore_error;
    next->pfr = addr->pfr;
    addr->child_count++;
    next->next = *addr_new;
    *addr_new = next;

    if (errors_to != NULL) next->errors_address = errors_to;

    DEBUG(2) debug_printf("%s director generated %s%s%s%s%s\n",
      dblock->name,
      next->orig,
      next->pfr? " (pipe, file, or autoreply)" : "",
      (errors_to != NULL)? " (errors to " : "",
      (errors_to != NULL)? errors_to : "",
      (errors_to != NULL)? ")" : "");
    }

  yield = OK;
  }

/* If the forward file generated no addresses, it is not an error. The
director just fails. Compare aliasfile, which is different. */

else yield = (addr->child_count <= 0)? FAIL : OK;

/* If the yield is OK, put the original address onto the succeed queue so
that any retry items that get attached to it get processed. */

if (yield == OK)
  {
  addr->next = *addr_succeed;
  *addr_succeed = addr;
  }

return yield;
}

/* End of directors/forwardfile.c */
