/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

static char RCSid[] = "$Id: nvlists.c,v 1.1 1996/01/24 19:29:19 chuck Exp $" ;

#include <sys/types.h>
#include <sys/socket.h>
#include <syslog.h>

#include "defs.h"
#include "sconf.h"

/*
 * A NULL value for the name field marks the end of the table
 */

struct name_value service_types[] =
	{
#ifndef NO_RPC
		{ "RPC",					ST_RPC			},
#endif
		{ "INTERNAL",			ST_INTERNAL		},
		{ "UNLISTED",			ST_UNLISTED		},
		{ "SPECIAL",			ST_SPECIAL		},
		{ CHAR_NULL,			0					}
	} ;


struct name_value service_flags[] =
	{
		{ "REUSE",				SF_REUSE			},
		{ "INTERCEPT",			SF_INTERCEPT	},
		{ "NORETRY",			SF_NORETRY		},
		{ "IDONLY",				SF_IDONLY		},
		{ CHAR_NULL,			0					}
	} ;


struct name_value socket_types[] =
	{
		{ "stream",				SOCK_STREAM			},
		{ "dgram",				SOCK_DGRAM			},
		{ "raw",					SOCK_RAW				},
		{ "seqpacket",			SOCK_SEQPACKET		},
		{ CHAR_NULL,			1						},
		{ "BAD SOCKET TYPE",	0						}
	} ;


struct name_value success_log_options[] =
	{
		{ "HOST",      		LO_HOST			},
		{ "DURATION",  		LO_DURATION		},
		{ "EXIT",      		LO_EXIT			},
		{ "PID",					LO_PID			},
		{ "USERID",				LO_USERID		},
		{ CHAR_NULL,			0					}
	} ;


struct name_value failure_log_options[] = 
	{
		{ "HOST",      		LO_HOST			},
		{ "ATTEMPT",			LO_ATTEMPT		},
		{ "RECORD",				LO_RECORD		},
		{ "USERID",				LO_USERID		},
		{ CHAR_NULL,			0					}
	} ;



struct name_value syslog_facilities[] =
	{
		{ "daemon",				LOG_DAEMON	},
		{ "auth",				LOG_AUTH		},
		{ "user",				LOG_USER		},
		{ "local0",				LOG_LOCAL0	},
		{ "local1",				LOG_LOCAL1	},
		{ "local2",				LOG_LOCAL2	},
		{ "local3",				LOG_LOCAL3	},
		{ "local4",				LOG_LOCAL4	},
		{ "local5",				LOG_LOCAL5	},
		{ "local6",				LOG_LOCAL6	},
		{ "local7",				LOG_LOCAL7	},
		{ CHAR_NULL, 			1				},
		{ "BAD FACILITY",		0				}
	} ;


struct name_value syslog_levels[] = 
	{
		{ "emerg",				LOG_EMERG	},
		{ "alert",				LOG_ALERT	},
		{ "crit",				LOG_CRIT		},
		{ "err",					LOG_ERR		},
		{ "warning",			LOG_WARNING	},
		{ "notice",				LOG_NOTICE	},
		{ "info",				LOG_INFO		},
		{ "debug",				LOG_DEBUG	},
		{ CHAR_NULL,			1				},
		{ "BAD LEVEL",			0				}
	} ;

