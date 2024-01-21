/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */

/* Private structure for the private options. */

typedef struct {
  char *file_dir;
  char *home_dir;
  char *current_dir;
  char *file;
  char *errors_to;
  char *expand_uid;
  char *expand_gid;
  uid_t uid;
  gid_t gid;
  int   modemask;
  uid_t *owners;
  gid_t *owngroups;
  BOOL  uid_set;
  BOOL  gid_set;
  BOOL  check_local_user;
  BOOL  check_group;
  BOOL  filter;
  BOOL  forbid_filter_log;
  BOOL  forbid_file;
  BOOL  forbid_include;
  BOOL  forbid_pipe;
  BOOL  forbid_reply;
  BOOL  initgroups;
  BOOL  use_seteuid;
  BOOL  freeze_missing_include;
  BOOL  rewrite;
  BOOL  skip_syntax_errors;
} forwardfile_director_options_block;

/* Data for reading the private options. */

extern optionlist forwardfile_director_options[];
extern int forwardfile_director_options_count;

/* Block containing default values. */

extern forwardfile_director_options_block forwardfile_director_option_defaults;

/* The main and initialization entry points for the director */

extern int forwardfile_director_entry(director_instance *, address_item *,
  address_item **, address_item **, address_item **, address_item **, BOOL);

extern void forwardfile_director_init(director_instance *);

/* End of directors/forwardfile.h */
