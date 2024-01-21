/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 199h */
/* See the file NOTICE for conditions of use and distribution. */

/* Private structure for the private options. */

typedef struct {
  char *cmd;
  char *path;
  char *prefix;
  char *suffix;
  int   umask;
  int   max_output;
  int   timeout;
  BOOL  from_hack;
  BOOL  ignore_status;
  BOOL  return_path_add;
  BOOL  delivery_date_add;
  BOOL  envelope_to_add;
  BOOL  restrict_to_path;
} pipe_transport_options_block;

/* Data for reading the private options. */

extern optionlist pipe_transport_options[];
extern int pipe_transport_options_count;

/* Block containing default values. */

extern pipe_transport_options_block pipe_transport_option_defaults;

/* The main and init entry points for the transport */

extern void pipe_transport_entry(transport_instance *, address_item *);

extern void pipe_transport_init(transport_instance *);

/* End of transports/pipe.h */
