/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */

/* Private structure for the private options. */

typedef struct {
  char *new_address;
  BOOL panic_expansion_fail;
} smartuser_director_options_block;

/* Data for reading the private options. */

extern optionlist smartuser_director_options[];
extern int smartuser_director_options_count;

/* Block containing default values. */

extern smartuser_director_options_block smartuser_director_option_defaults;

/* The main and initialization entry points for the director */

extern int smartuser_director_entry(director_instance *, address_item *,
  address_item **, address_item **, address_item **, address_item **, BOOL);

extern void smartuser_director_init(director_instance *);

/* End of directors/smartuser.h */
