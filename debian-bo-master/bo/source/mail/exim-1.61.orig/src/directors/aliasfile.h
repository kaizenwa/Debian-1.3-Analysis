/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */

/* Header for the aliasfile director */

/* Structure for the private options. */

typedef struct {
  char *file;
  char *query;
  char *home_dir;
  char *current_dir;
  char *errors_to;
  char *expand_uid;
  char *expand_gid;
  uid_t uid;
  gid_t gid;
  int   modemask;
  int   search_type;
  int  *owners;
  int  *owngroups;
  BOOL  uid_set;
  BOOL  gid_set;
  BOOL  initgroups;
  BOOL  expand;
  BOOL  optional;
  BOOL  forbid_file;
  BOOL  forbid_pipe;
  BOOL  freeze_missing_include;
  BOOL  rewrite;
  BOOL  skip_syntax_errors;
} aliasfile_director_options_block;

/* Data for reading the private options. */

extern optionlist aliasfile_director_options[];
extern int aliasfile_director_options_count;

/* Block containing default values. */

extern aliasfile_director_options_block aliasfile_director_option_defaults;

/* The main and initialization entry points */

extern int aliasfile_director_entry(director_instance *, address_item *,
  address_item **, address_item **, address_item **, address_item **, BOOL);
extern void aliasfile_director_init(director_instance *);

/* End of directors/aliasfile.h */
