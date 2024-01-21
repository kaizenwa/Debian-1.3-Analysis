/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */

/* A set of functions to search databases in various formats. An open database
is represented by a void * value. This set of functions has no private state. */

#include "exim.h"

#if HAVE_NIS
#include <rpcsvc/ypclnt.h>
#endif

#if HAVE_NISPLUS
#include <rpcsvc/nis.h>
#endif


/* Returns from the file credential check function. */

enum { check_ok, check_mode, check_user, check_group, check_fstat };

/* Tree in which to cache open files until tidyup called. */

tree_node *search_tree = NULL;

/* Place to cache lookups for efficiency. */

static char cached_filename[256] = "";
static char cached_keystring[256] = "";
static int  cached_type = -1;
static char *cached_data = NULL;



/*************************************************
*         Check a file's credentials             *
*************************************************/

/* fstat can normally be expected to work on an open file, but there are some
NFS states where it may not.

Arguments:
  fd         an open file descriptor
  modemask   a mask specifying mode bits that must *not* be set
  owners     NULL or a list of of allowable uids, count in the first item
  owngroups  NULL or a list of allowable gids, count in the first item

Returns:     check_fstat if fstat() fails
             check_mode if not a regular file or bad mode
             check_user if wrong owner
             check_group if wrong group
             check_ok if all well

Side effect: sets errno to ERRNO_BADUGID or ERRNO_BADMODE for bad uid/gid
             or bad mode; otherwise leaves it to what fstat set it to.
*/

static int
check_file(int fd, int modemask, int *owners, int *owngroups)
{
int i;
struct stat statbuf;

if (fstat(fd, &statbuf) != 0) return check_fstat;

if ((statbuf.st_mode & S_IFMT) != S_IFREG ||
    (statbuf.st_mode & modemask) != 0)
  {
  errno = ERRNO_BADMODE;
  return check_mode;
  }

if (owners != NULL)
  {
  BOOL uid_ok = FALSE;
  for (i = 1; i <= (int)owners[0]; i++)
    if (owners[i] == statbuf.st_uid) { uid_ok = TRUE; break; }
  if (!uid_ok)
    {
    errno = ERRNO_BADUGID;
    return check_user;
    }
  }

if (owngroups != NULL)
  {
  BOOL gid_ok = FALSE;
  for (i = 1; i <= (int)owngroups[0]; i++)
    if (owngroups[i] == statbuf.st_gid) { gid_ok = TRUE; break; }
  if (!gid_ok)
    {
    errno = ERRNO_BADUGID;
    return check_group;
    }
  }

return check_ok;
}



/*************************************************
*               Release cached resources         *
*************************************************/

/* When search_open is called, it caches the file that it opens in search_tree.
The name of the tree node is a concatenation of the search type with the file
name. These files are closed only when this tidyup routine is called, typically
at the end of sections of code where a number of lookups might occur.

First, there is an internal, recursive subroutine.

Argument:   a pointer to a search_openfile tree node
Returns:    nothing
*/

static void
tidyup_subtree(tree_node *t)
{
if (t->left != NULL) tidyup_subtree(t->left);
if (t->right != NULL) tidyup_subtree(t->right);
if (t->name[0] == stype_lsearch) fclose((FILE *)(t->data.ptr));
  else if (t->name[0] == stype_dbm) EXIM_DBCLOSE((EXIM_DB *)(t->data.ptr));
store_free(t);
}

/* The external entry point

Argument: none
Returns:  nothing
*/

void
search_tidyup(void)
{
DEBUG(9) debug_printf("search_tidyup called\n");
if (search_tree != NULL)
  {
  tidyup_subtree(search_tree);
  search_tree = NULL;
  }
}




/*************************************************
*             Open search database               *
*************************************************/

/* A mode, and lists of owners and groups are passed over for checking in
the cases where the database is one or more files. Return NULL, with a message
pointed to by message, in cases of error.

For search types that use a file or files, check up on the mode after opening.
It is tempting to do a stat before opening the file, and use it as an existence
check. However, doing that opens a small security loophole in that the status
could be changed before the file is opened. Can't quite see what problems this
might lead to, but you can't be too careful where security is concerned.
Fstat() on an open file can normally be expected to succeed, but there are some
NFS states where it does not.

There are two styles of query: (1) in the "single-key+file" style, a single key
string and a file name are given, for example, for linear searches, DBM files,
or for NIS. (2) In the "query" style, no "filename" is given; instead just a
single query string is passed. This applies to multiple-key lookup types such
as NIS+.

Before opening, scan the tree of cached files to see if this "file" is already
open for the correct search type. If so, return the saved handle. If not, put
the handle in the tree for possible subsequent use. See search_tidyup above for
closing the cached files.

Arguments:
  filename       the name of the file for single-key+file style lookups
                 NULL for query-style lookups
  search_type    the type of search required
  modemask       if a real single file is used, this specifies mode bits that
                 must not be set; otherwise it is ignored
  owners         if a real single file is used, this specifies the possible
                 owners of the file; otherwise it is ignored
  owngroups      if a real single file is used, this specifies the possible
                 group owners of the file; otherwise it is ignored
  message        points to a char * which can be set to point to an error
                 message when something goes wrong

Returns:         an identifying handle for the open database
*/

void *
search_open(char *filename, int search_type, int modemask, int *owners,
  int *owngroups, char **message)
{
int rc;
FILE *f;
EXIM_DB *d;
char *nis_domain;
void *handle;
tree_node *t;
char keybuffer[256];

/* See if we already have this file open for this type of search, and if so,
pass back the previous handle. The key for the tree node is the search type
concatenated with the file name. */

if (filename != NULL)
  {
  sprintf(keybuffer, "%c%s", search_type, filename);
  if ((t = tree_search(search_tree, keybuffer)) != NULL)
    {
    DEBUG(9) debug_printf("search_open found (%c) %s cached\n",
      search_type, filename);
    return t->data.ptr;
    }
  }

/* Otherwise, open the file - each search type has its own code. */

DEBUG(9) debug_printf("search_open (%c) %s\n", search_type,
  (filename == NULL)? "NULL" : filename);

errno = 0;

switch (search_type)
  {
  /* Linear search */

  case stype_lsearch:
  f = fopen(filename, "r");
  if (f == NULL)
    {
    int save_errno = errno;
    *message = string_sprintf("failed to open %s for linear search: %s",
      filename, strerror(errno));
    errno = save_errno;
    return NULL;
    }

  if ((rc = check_file(fileno(f), modemask, owners, owngroups)) != check_ok)
    {
    int save_errno = errno;
    *message = (rc == check_fstat)?
      string_sprintf("%s: failed to fstat open file", filename) :
      string_sprintf("%s (linear search): wrong %s", filename,
        (rc == check_mode)? "mode" :
        (rc == check_user)? "owner" : "group");
    errno = save_errno;
    return NULL;
    }
  handle = f;
  break;


  /* DBM search */

  case stype_dbm:
  d = EXIM_DBOPEN(filename, O_RDONLY, 0);
  if (d == NULL)
    {
    int save_errno = errno;
    *message = string_sprintf("failed to open %s as a %s file: %s", filename,
      EXIM_DBTYPE, strerror(errno));
    errno = save_errno;
    return NULL;
    }

  /* This needs to know more about the underlying files than is good for it! */

  #ifdef USE_DB
  if ((rc = check_file(EXIM_DBFD(d), modemask, owners, owngroups)) != check_ok)
  #else
  if (((rc = check_file(dbm_pagfno(d), modemask, owners, owngroups)) != check_ok)
      &&
      ((rc = check_file(dbm_dirfno(d), modemask, owners, owngroups)) != check_ok))
  #endif
    {
    int save_errno = errno;
    *message = (rc == check_fstat)?
      string_sprintf("%s: failed to fstat open file", filename) :
      string_sprintf("%s (dbm search): wrong %s", filename,
        (rc == check_mode)? "mode" :
        (rc == check_user)? "user" : "group");
    errno = save_errno;
    return NULL;
    }
  handle = d;
  break;

  /* NIS search */

  #if HAVE_NIS
  case stype_nis:
  case stype_nis0:
  if (yp_get_default_domain(&nis_domain) != 0)
    {
    *message = string_sprintf("failed to get default NIS domain");
    return NULL;
    }
  handle = nis_domain;
  break;
  #endif

  /* NIS+ search */

  #if HAVE_NISPLUS
  case stype_nisplus:
  handle = (void *)(1);    /* Just return something non-null */
  break;
  #endif

  /* Oops */

  default:
  *message = string_sprintf("unknown search type (%d) in search_open%s%s",
    search_type,
    (filename == NULL)? "" : " for file ",
    (filename == NULL)? "" : filename);
  return NULL;
  break;
  }

/* Get here only if the file has been successfully opened. If there is
a filename enter the file (with type concatenated) into the tree. */

if (filename != NULL)
  {
  t = store_malloc(sizeof(tree_node) + (int)strlen(keybuffer));
  strcpy(t->name, keybuffer);
  t->data.ptr = handle;
  tree_insertnode(&search_tree, t);
  }

return handle;
}





/*************************************************
*  Internal function: Find one item in database  *
*************************************************/

/*The answer is always put into dynamic store. If the key contains a colon,
then it is treated as a double key: the first part is the key for the record in
the file, and the remainder is a subkey that is used to extract a subfield from
the main data. Subfields are specified as subkey=value in the records.

The last lookup is cached by file name and key - using the handle is no good as
it isn't unique enough.

Arguments:
  handle       the handle from search_open
  filename     the filename that was handed to search_open, or
               NULL for query-style searches
  keystring    the keystring for single-key+file lookups, or
               the querystring for query-style lookups
  type         the type of lookup
  errmsg       somewhere to point to an error message

Returns:       NULL if the query failed, or
               a pointer to a string containing the answer
*/

static char *
internal_search_find(void *handle, char *filename, char *keystring, int type,
  char **errmsg)
{
int length;
int nis_data_length;
FILE *f;
EXIM_DB *d;
EXIM_DATUM key, data;
char *subkey = NULL;
char *colon = NULL;
char *nis_data;
char buffer[4096];

*errmsg = "";    /* for safety */

/* If there's a colon in a key for a single-key+file lookup, temporarily
terminate the main key, and set up the subkey. */

if (filename != NULL)
  {
  colon = strchr(keystring, ':');
  if (colon != NULL)
    {
    subkey = colon + 1;
    *colon = 0;
    }
  }

/* Turn a NULL filename into an empty string so that it gets copied and
compared in the cache without special code. */

else filename = "";

/* Insurance. If the keystring is empty, just fail. */

if (keystring[0] == 0) return NULL;

/* If there is no cached record of the right type, or if this lookup is for a
new key, or in a different file, we must search the file and set up new cached
data. */

if (type != cached_type ||
    strcmp(keystring, cached_keystring) != 0 ||
    strcmp(filename, cached_filename) != 0)
  {
  DEBUG(9)
    {
    if (filename[0] != 0)
      debug_printf("file lookup required for %s%s%s in %s\n",
        keystring,
        (subkey == NULL)? "" : ":",
        (subkey == NULL)? "" : subkey,
        filename);
    else
      debug_printf("database lookup required for %s\n", keystring);
    }

  if (cached_data != NULL)
    {
    store_free(cached_data);    /* free previous */
    cached_data = NULL;
    }

  /* Length of key to match */

  length = (int)strlen(keystring);

  /* Code for the different kinds of search. The answer should be put in
  dynamic store as a zero-terminated string, and pointed to by cached_data.
  Leave cached_data = NULL on failure. */

  switch(type)
    {
    /* Linear search */

    case stype_lsearch:
    f = (FILE *)handle;
    rewind(f);

    while (fgets(buffer, sizeof(buffer), f) != NULL)
      {
      int ptr, size;
      int p = (int)strlen(buffer);
      char *s = buffer;

      while (p > 0 && isspace(buffer[p-1])) p--;
      buffer[p] = 0;
      if (buffer[0] == 0 || buffer[0] == '#' || isspace(buffer[0])) continue;
      while (*s != 0 && *s != ':' && !isspace(*s)) s++;
      if (s-buffer != length || strncmpic(buffer, keystring, length) != 0)
        continue;

      if (*s == ':') s++;
      while (isspace(*s)) s++;

      size = 100;
      ptr = 0;
      cached_data = store_malloc(size);
      if (*s != 0)
        cached_data = string_cat(cached_data, &size, &ptr, s, (int)strlen(s));

      while (fgets(buffer, sizeof(buffer), f) != NULL)
        {
        p = (int)strlen(buffer);
        while (p > 0 && isspace(buffer[p-1])) p--;
        buffer[p] = 0;
        if (buffer[0] == 0 || buffer[0] == '#') continue;
        if (!isspace(buffer[0])) break;
        cached_data = string_cat(cached_data, &size, &ptr, buffer, (int)strlen(buffer));
        }

      cached_data[ptr] = 0;
      break;
      }
    break;


    /* DBM search. */

    case stype_dbm:
    d = (EXIM_DB *)handle;
    EXIM_DATUM_DATA(key) = keystring;
    EXIM_DATUM_SIZE(key) = length + 1;
    if (EXIM_DBGET(d, key, data))
      cached_data = string_copy(EXIM_DATUM_DATA(data));
    break;


    /* NIS search */

    #if HAVE_NIS
    case stype_nis:
    case stype_nis0:
    if (yp_match((char *)handle, filename, keystring,
        length + ((type == stype_nis)? 0 : 1),
        &nis_data, &nis_data_length) == 0)
      {
      cached_data = string_copy(nis_data);
      cached_data[nis_data_length] = 0;    /* remove final '\n' */
      }
    break;
    #endif


    /* NIS+ search. The format of queries for a NIS+ search is

      [field=value,...],table-name
    or
      [field=value,...],table-name:result-field-name

    in other words, a normal NIS+ "indexed name", with an optional
    result field name tagged on the end after a colon. If there is
    no result-field name, the yield is the concatenation of all the
    fields, preceded by their names and an equals sign. */

    #if HAVE_NISPLUS
    case stype_nisplus:
      {
      int i;
      int ssize = 0;
      int offset = 0;
      char *field_name = NULL;
      nis_result *nrt = NULL;
      nis_result *nre = NULL;
      nis_object *tno, *eno;
      struct entry_obj *eo;
      struct table_obj *ta;
      char *p = keystring + (int)strlen(keystring);

      /* Search backwards for a colon to see if a result field name
      has been given. */

      while (p > keystring && p[-1] != ':') p--;

      if (p > keystring)
        {
        field_name = p;
        p[-1] = 0;
        }
      else p = keystring + (int)strlen(keystring);

      /* Now search backwards to find the comma that starts the
      table name. */

      while (p > keystring && p[-1] != ',') p--;
      if (p <= keystring)
        {
        *errmsg = "NIS+ query malformed";
        goto NISPLUS_EXIT;
        }

      /* Look up the data for the table, in order to get the field names,
      check that we got back a table, and set up pointers so the field
      names can be scanned. */

      nrt = nis_lookup(p, EXPAND_NAME);
      if (nrt->status != 0)
        {
        *errmsg = string_sprintf("NIS+ error accessing %s table: %s", p,
          nis_sperrno(nrt->status));
        goto NISPLUS_EXIT;
        }
      tno = nrt->objects.objects_val;
      if (tno->zo_data.zo_type != TABLE_OBJ)
        {
        *errmsg = string_sprintf("NIS+ error: %s is not a table", p);
        goto NISPLUS_EXIT;
        }
      ta = &(tno->zo_data.objdata_u.ta_data);

      /* Now look up the entry in the table, check that we got precisely one
      object and that it is a table entry. */

      nre = nis_list(keystring, EXPAND_NAME, NULL, NULL);
      if (nre->status != 0)
        {
        *errmsg = string_sprintf("NIS+ error accessing entry %s: %s",
          keystring, nis_sperrno(nre->status));
        goto NISPLUS_EXIT;
        }
      if (nre->objects.objects_len > 1)
        {
        *errmsg = string_sprintf("NIS+ returned more than one object for %s",
          keystring);
        goto NISPLUS_EXIT;
        }
      else if (nre->objects.objects_len < 1)
        {
        *errmsg = string_sprintf("NIS+ returned no data for %s", keystring);
        goto NISPLUS_EXIT;
        }
      eno = nre->objects.objects_val;
      if (eno->zo_data.zo_type != ENTRY_OBJ)
        {
        *errmsg = string_sprintf("NIS+ error: %s is not an entry", keystring);
        goto NISPLUS_EXIT;
        }

      /* Scan the columns in the entry and in the table. If a result field
      was given, look for that field; otherwise concatenate all the fields
      with their names. */

      eo = &(eno->zo_data.objdata_u.en_data);
      for (i = 0; i < eo->en_cols.en_cols_len; i++)
        {
        table_col *tc = ta->ta_cols.ta_cols_val + i;
        entry_col *ec = eo->en_cols.en_cols_val + i;
        int len = ec->ec_value.ec_value_len;
        char *value = ec->ec_value.ec_value_val;

        /* The value may be NULL for a zero-length field. Turn this into an
        empty string for consistency. Remove trailing whitespace and zero
        bytes. */

        if (value == NULL) value = ""; else
          while (len > 0 && (value[len-1] == 0 || isspace(value[len-1]))) len--;

        /* Concatenate all fields if no specific one selected */

        if (field_name == NULL)
          {
          cached_data = string_cat(cached_data, &ssize, &offset, tc->tc_name,
            (int)strlen(tc->tc_name));
          cached_data = string_cat(cached_data, &ssize, &offset, "=", 1);

          /* Quote the value if it contains spaces or is empty */

          if (value[0] == 0 || strchr(value, ' ') != NULL)
            {
            int j;
            cached_data = string_cat(cached_data, &ssize, &offset, "\"", 1);
            for (j = 0; j < len; j++)
              {
              if (value[j] == '\"' || value[j] == '\\')
                cached_data = string_cat(cached_data, &ssize, &offset,
                  "\\", 1);
              cached_data = string_cat(cached_data, &ssize, &offset,
                  value+j, 1);
              }
            cached_data = string_cat(cached_data, &ssize, &offset, "\"", 1);
            }
          else cached_data = string_cat(cached_data, &ssize, &offset,
            value, len);

          cached_data = string_cat(cached_data, &ssize, &offset, " ", 1);
          }

        /* When the specified field is found, grab its data and finish */

        else if (strcmp(field_name, tc->tc_name) == 0)
          {
          cached_data = string_copyn(value, len);
          goto NISPLUS_EXIT;
          }
        }

      /* Error if a field name was specified and we didn't find it; if no
      field name, ensure the concatenated data is zero-terminated. */

      if (field_name != NULL)
        *errmsg = string_sprintf("NIS+ field %s not found for %s", field_name,
          keystring);
      else cached_data[offset] = 0;

      /* Restore the colon in the keystring, and free result store before
      finishing. */

      NISPLUS_EXIT:
      if (field_name != NULL) field_name[-1] = ':';
      if (nrt != NULL) nis_freeresult(nrt);
      if (nre != NULL) nis_freeresult(nre);
      }
    break;
    #endif
    }

  /* A record that has been found is now in cached_data, which is either NULL
  or points to a bit of dynamic store. Remember the file name, main key, and
  lookup type, but only if the file name and main key are < 256 characters
  long (the size of the cache slots). Longer keys are presumably exceedingly
  rare... */

  if ((int)strlen(filename) < 256 && length < 256)
    {
    strcpy(cached_filename, filename);
    strcpy(cached_keystring, keystring);
    cached_type = type;
    }
  else cached_type = -1;    /* Force lookup next time */
  }

else DEBUG(9) debug_printf("cached data used for lookup of %s%s%s%s%s\n",
  keystring,
  (subkey == NULL)? "" : ":",
  (subkey == NULL)? "" : subkey,
  (filename[0] == 0)? "" : " in ",
  (filename[0] == 0)? "" : filename);

/* Put back the colon if it was overwritten */

if (colon != NULL) *colon = ':';

/* If we have found data, pick out the subfield if required. Otherwise
make a fresh copy of the whole cached string. */

if (subkey != NULL && cached_data != NULL)
  return expand_getkeyed(subkey, cached_data);
    else if (cached_data != NULL) return string_copy(cached_data);
      else return NULL;
}




/*************************************************
* Find one item in database, possibly wildcarded *
*************************************************/

/* This function calls the internal function above; once only if there
is no partial matching, but repeatedly when partial matching is requested.

Arguments:
  handle       the handle from search_open
  filename     the filename that was handed to search_open, or
               NULL for query-style searches
  keystring    the keystring for single-key+file lookups, or
               the querystring for query-style lookups
  type         the type of lookup
  partial      -1 means no partial matching
               otherwise it's the minimum number of components
  expand_setup pointer to offset for setting up expansion strings;
               don't do any if < 0
  errmsg       somewhere to point to an error message

Returns:       NULL if the query failed, or
               a pointer to a string containing the answer
*/

char *
search_find(void *handle, char *filename, char *keystring, int type,
  int partial, int *expand_setup, char **errmsg)
{
char *yield = internal_search_find(handle, filename, keystring, type,
  errmsg);

if (yield == NULL && partial > 0 && filename != NULL)
  {
  char *keystring2 = string_sprintf("*.%s", keystring);

  DEBUG(9) debug_printf("trying partial match %s\n", keystring2);
  yield = internal_search_find(handle, filename, keystring2, type, errmsg);

  if (yield == NULL)
    {
    int dotcount = 0;
    char *keystring3 = keystring2 + 2;
    char *s = keystring3;
    while (*s != 0) if (*s++ == '.') dotcount++;

    while (dotcount-- >= partial)
      {
      while (keystring3[1] != '.') keystring3++;
      *keystring3 = '*';
      DEBUG(9) debug_printf("trying partial match %s\n", keystring3);
      yield = internal_search_find(handle, filename, keystring3, type, errmsg);
      if (yield != NULL)
        {
        if (expand_setup != NULL && *expand_setup >= 0)
          {
          *expand_setup += 1;
          expand_nstring[*expand_setup] = keystring;
          expand_nlength[*expand_setup] = (int)strlen(keystring) -
            (int)strlen(keystring3) + 1;
          }
        break;
        }
      keystring3 += 2;
      }
    }

  store_free(keystring2);
  }

return yield;
}

/* End of search.c */
