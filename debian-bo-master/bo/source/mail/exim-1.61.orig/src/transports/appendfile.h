/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */

/* Private structure for the private options. */

typedef struct {
  char *filename;
  char *dirname;
  char *prefix;
  char *suffix;
  char *create_file_string;
  char *quota;
  int   quota_value;
  int   mode;
  int   dirmode;
  int   lockfile_mode;
  int   lockfile_timeout;
  int   lock_retries;
  int   lock_interval;
  int   create_file;
  BOOL  allow_symlink;
  BOOL  check_group;
  BOOL  create_directory;
  BOOL  notify_comsat;
  BOOL  require_lockfile;
  BOOL  use_lockfile;
  BOOL  from_hack;
  BOOL  return_path_add;
  BOOL  delivery_date_add;
  BOOL  envelope_to_add;
  BOOL  file_must_exist;
} appendfile_transport_options_block;

/* Restricted creation options */

enum { create_anywhere, create_belowhome, create_inhome };

/* Data for reading the private options. */

extern optionlist appendfile_transport_options[];
extern int appendfile_transport_options_count;

/* Block containing default values. */

extern appendfile_transport_options_block appendfile_transport_option_defaults;

/* The main and init entry points for the transport */

extern void appendfile_transport_entry(transport_instance *, address_item *);

extern void appendfile_transport_init(transport_instance *);

/* End of transports/appendfile.h */
