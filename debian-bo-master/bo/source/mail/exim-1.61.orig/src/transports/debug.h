/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */

/* Private structure for the private options. This is not used for
the debug transport and so must always be empty. However, some compilers do
not like empty structures - the Standard, alas, says "undefined behaviour" for
an empty structure - so we have to put in a dummy value. */

typedef struct {
  int dummy;
} debug_transport_options_block;

/* Data for reading the private options. */

extern optionlist debug_transport_options[];
extern int debug_transport_options_count;

/* Block containing default values. */

extern debug_transport_options_block debug_transport_option_defaults;

/* The main and init entry points for the transport */

extern void debug_transport_entry(transport_instance *, address_item *);

extern void debug_transport_init(transport_instance *);

/* End of transports/debug.h */
