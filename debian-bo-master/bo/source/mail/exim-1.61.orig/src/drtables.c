/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */


#include "exim.h"


/* The OSF1 linker puts out a worrying warning if any sections contain no
executable code. It says

Warning: Linking some objects which contain exception information sections
        and some which do not. This may cause fatal runtime exception handling
        problems.

As this may cause people to worry needlessly, include a dummy function here
to stop the message from appearing. Make it call itself to stop picky compilers
complaining that it isn't used. */

static void dummy(void) { dummy(); }


/* Tables of information about which directors, routers and transports are
included in the exim binary. This is controlled by macros defined in the
config.h file. */


/* Pull in the necessary header files */

#ifdef DIRECTOR_ALIASFILE
#include "directors/aliasfile.h"
#endif

#ifdef DIRECTOR_FORWARDFILE
#include "directors/forwardfile.h"
#endif

#ifdef DIRECTOR_LOCALUSER
#include "directors/localuser.h"
#endif

#ifdef DIRECTOR_SMARTUSER
#include "directors/smartuser.h"
#endif

#ifdef ROUTER_DOMAINLIST
#include "routers/domainlist.h"
#endif

#ifdef ROUTER_IPLITERAL
#include "routers/ipliteral.h"
#endif

#ifdef ROUTER_IPLOOKUP
#include "routers/iplookup.h"
#endif

#ifdef ROUTER_LOOKUPHOST
#include "routers/lookuphost.h"
#endif

#ifdef ROUTER_QUERYPROGRAM
#include "routers/queryprogram.h"
#endif

#ifdef TRANSPORT_APPENDFILE
#include "transports/appendfile.h"
#endif

#ifdef TRANSPORT_AUTOREPLY
#include "transports/autoreply.h"
#endif

#ifdef TRANSPORT_DEBUG
#include "transports/debug.h"
#endif

#ifdef TRANSPORT_PIPE
#include "transports/pipe.h"
#endif

#ifdef TRANSPORT_SMTP
#include "transports/smtp.h"
#endif


/* Now set up the structures, terminated by an entry with a null name. */

director_info directors_available[] = {
#ifdef DIRECTOR_ALIASFILE
  {
  "aliasfile",
  aliasfile_director_entry,
  aliasfile_director_init,
  NULL,    /* no tidyup entry */
  aliasfile_director_options,
  &aliasfile_director_options_count,
  &aliasfile_director_option_defaults,
  sizeof(aliasfile_director_options_block)
  },
#endif
#ifdef DIRECTOR_FORWARDFILE
  {
  "forwardfile",
  forwardfile_director_entry,
  forwardfile_director_init,
  NULL,    /* no tidyup entry */
  forwardfile_director_options,
  &forwardfile_director_options_count,
  &forwardfile_director_option_defaults,
  sizeof(forwardfile_director_options_block)
  },
#endif
#ifdef DIRECTOR_LOCALUSER
  {
  "localuser",
  localuser_director_entry,
  localuser_director_init,
  NULL,    /* no tidyup entry */
  localuser_director_options,
  &localuser_director_options_count,
  &localuser_director_option_defaults,
  sizeof(localuser_director_options_block)
  },
#endif
#ifdef DIRECTOR_SMARTUSER
  {
  "smartuser",
  smartuser_director_entry,
  smartuser_director_init,
  NULL,    /* no tidyup entry */
  smartuser_director_options,
  &smartuser_director_options_count,
  &smartuser_director_option_defaults,
  sizeof(smartuser_director_options_block)
  },
#endif
{ "", NULL, NULL, NULL, NULL, 0, NULL, 0 }
};



router_info routers_available[] = {
#ifdef ROUTER_DOMAINLIST
  {
  "domainlist",
  domainlist_router_entry,
  domainlist_router_init,
  NULL,    /* no tidyup entry */
  domainlist_router_options,
  &domainlist_router_options_count,
  &domainlist_router_option_defaults,
  sizeof(domainlist_router_options_block)
  },
#endif
#ifdef ROUTER_IPLITERAL
  {
  "ipliteral",
  ipliteral_router_entry,
  ipliteral_router_init,
  NULL,    /* no tidyup entry */
  ipliteral_router_options,
  &ipliteral_router_options_count,
  &ipliteral_router_option_defaults,
  sizeof(ipliteral_router_options_block)
  },
#endif
#ifdef ROUTER_IPLOOKUP
  {
  "iplookup",
  iplookup_router_entry,
  iplookup_router_init,
  NULL,    /* no tidyup entry */
  iplookup_router_options,
  &iplookup_router_options_count,
  &iplookup_router_option_defaults,
  sizeof(iplookup_router_options_block)
  },
#endif
#ifdef ROUTER_LOOKUPHOST
  {
  "lookuphost",
  lookuphost_router_entry,
  lookuphost_router_init,
  NULL,    /* no tidyup entry */
  lookuphost_router_options,
  &lookuphost_router_options_count,
  &lookuphost_router_option_defaults,
  sizeof(lookuphost_router_options_block)
  },
#endif
#ifdef ROUTER_QUERYPROGRAM
  {
  "queryprogram",
  queryprogram_router_entry,
  queryprogram_router_init,
  NULL,    /* no tidyup entry */
  queryprogram_router_options,
  &queryprogram_router_options_count,
  &queryprogram_router_option_defaults,
  sizeof(queryprogram_router_options_block)
  },
#endif
{ "", NULL, NULL, NULL, NULL, 0, NULL, 0 }
};



transport_info transports_available[] = {
#ifdef TRANSPORT_APPENDFILE
  {
  "appendfile",                                /* driver name */
  appendfile_transport_entry,                  /* main entry point */
  appendfile_transport_init,                   /* init entry point */
  NULL,                                        /* no tidyup entry */
  appendfile_transport_options,                /* local options table */
  &appendfile_transport_options_count,         /* number of entries */
  &appendfile_transport_option_defaults,       /* private options defaults */
  sizeof(appendfile_transport_options_block),  /* size of private block */
  TRUE,                                        /* local flag */
  NULL                                         /* no closedown entry */
  },
#endif
#ifdef TRANSPORT_AUTOREPLY
  {
  "autoreply",                                 /* driver name */
  autoreply_transport_entry,                   /* main entry point */
  autoreply_transport_init,                    /* init entry point */
  NULL,                                        /* no tidyup entry */
  autoreply_transport_options,                 /* local options table */
  &autoreply_transport_options_count,          /* number of entries */
  &autoreply_transport_option_defaults,        /* private options defaults */
  sizeof(autoreply_transport_options_block),   /* size of private block */
  TRUE,                                        /* local flag */
  NULL                                         /* no closedown entry */
  },
#endif
#ifdef TRANSPORT_DEBUG
  {
  "debug",                                     /* driver name */
  debug_transport_entry,                       /* main entry point */
  debug_transport_init,                        /* init entry point */
  NULL,                                        /* no tidyup entry */
  debug_transport_options,                     /* local options table */
  &debug_transport_options_count,              /* number of entries */
  &debug_transport_option_defaults,            /* private options defaults */
  sizeof(debug_transport_options_block),       /* size of private block */
  TRUE,                                        /* local flag */
  NULL                                         /* no closedown entry */
  },
#endif
#ifdef TRANSPORT_PIPE
  {
  "pipe",                                      /* driver name */
  pipe_transport_entry,                        /* main entry point */
  pipe_transport_init,                         /* init entry point */
  NULL,                                        /* no tidyup entry */
  pipe_transport_options,                      /* local options table */
  &pipe_transport_options_count,               /* number of entries */
  &pipe_transport_option_defaults,             /* private options defaults */
  sizeof(pipe_transport_options_block),        /* size of private block */
  TRUE,                                        /* local flag */
  NULL                                         /* no closedown entry */
  },
#endif
#ifdef TRANSPORT_SMTP
  {
  "smtp",                                      /* driver name */
  smtp_transport_entry,                        /* main entry point */
  smtp_transport_init,                         /* init entry point */
  NULL,                                        /* no tidyup entry */
  smtp_transport_options,                      /* local options table */
  &smtp_transport_options_count,               /* number of entries */
  &smtp_transport_option_defaults,             /* private options defaults */
  sizeof(smtp_transport_options_block),        /* size of private block */
  FALSE,                                       /* local flag */
  smtp_transport_closedown                     /* close down passed channel */
  },
#endif
{ "", NULL, NULL, NULL, NULL, 0, NULL, 0, FALSE }
};

/* End of drtables.c */
