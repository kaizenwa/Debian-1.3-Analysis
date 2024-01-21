/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */


#include "../exim.h"
#include "debug.h"



/* Options specific to the debug transport. There are none. This transport
is not normally referred to from the configuration file; it is set up by
means of the debug_transport option only. Because some compilers do not like
empty declarations ("undefined" in the Standard) we put in a dummy value. */

optionlist debug_transport_options[] = {
  { "", 0, NULL }
};

/* Size of the options list. An extern variable has to be used so that its
address can appear in the tables drtables.c. */

int debug_transport_options_count =
  sizeof(debug_transport_options)/sizeof(optionlist);

/* Default private options block for the debug transport. Again, a dummy
value is present to keep some compilers happy. */

debug_transport_options_block debug_transport_option_defaults = { 0 };



/*************************************************
*          Initialization entry point            *
*************************************************/

/* Called for each instance, after its options have been read, to
enable consistency checks to be done, or anything else that needs
to be set up. */

void
debug_transport_init(transport_instance *tblock)
{
/*
debug_transport_options_block *ob =
  (debug_transport_options_block *)(tblock->options_block);
*/
}




/*************************************************
*              Main entry point                  *
*************************************************/

/* See local README for interface details. This is a special transport, in
that it doesn't really transport the message. Instead, it copies it to the file
defined by the debug_transport option, preceded by information about the data
handed to it. This transport is substituted for a real transport when Exim is
run with the debug_transport option. The transport_instance argument is that
for the transport that would otherwise have been called. Please take real
care with this dangerous debugging tool. */

void
debug_transport_entry(
  transport_instance *tblock,      /* data for this instantiation */
  address_item *addrlist)          /* address we are working on */
{
FILE *f;
address_item *addr;
host_item *host;
int yield = DEFER;
/*
debug_transport_options_block *ob =
  (debug_transport_options_block *)(tblock->options_block);
*/

f = fopen(debug_transport_file, "a");

if (f == NULL)
  {
  for (addr = addrlist; addr != NULL; addr = addr->next)
    {
    addr->basic_errno = errno;
    addr->message = string_sprintf("while opening %s", debug_transport);
    addr->special_action = SPECIAL_FREEZE;
    }
  }

else
  {
  fprintf(f, "===================================================================\n");
  fprintf(f, "message_id  = %s\n", message_id);
  fprintf(f, "sender      = %s\n", sender_address);
  fprintf(f, "return_path = %s\n", return_path);
  fprintf(f, "transport   = %s\n", tblock->name);
  for (host = addrlist->host_list; host != NULL; host = host->next)
    {
    fprintf(f, "host        = %s [%s] MX=%d\n",
      (host->name == NULL)? "unset" : host->name,
      (host->address == NULL)? "unset" : host->address,
      host->mx);
    }
  for (addr = addrlist; addr != NULL; addr = addr->next)
    {
    fprintf(f, "local_part  = %s\n", addr->local_part);
    fprintf(f, "domain      = %s\n", addr->domain);
    addr->transport_return = OK;
    }
  fprintf(f, "-------------------------------------------------------------------\n");
  fflush(f);

  transport_write_message(addrlist, fileno(f), 0, NULL, 0);

  fclose(f);
  yield = OK;
  }

/* Set all addresses to the same return code */

for (addr = addrlist; addr != NULL; addr = addr->next)
  addr->transport_return = yield;

DEBUG(2) debug_printf("%s transport yielded %d\n", tblock->name,
  addrlist->transport_return);
}

/* End of transport/debug.c */
