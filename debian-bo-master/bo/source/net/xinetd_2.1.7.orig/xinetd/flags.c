/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

static char RCSid[] = "$Id: flags.c,v 1.1 1996/01/24 19:29:19 chuck Exp $" ;

#include "flags.h"
#include "state.h"
#include "defs.h"

/*
 * Control flags
 *
 *		NOENV means that the setjmp buffer should be disabled while the
 *		flag action is running (so if something goes wrong in the flag
 *		action the program will be terminated).
 */
#define CF_NONE						0x0
#define CF_NOENV						0x1

struct flag_action
{
	int 			fa_value ;
	voidfunc 	fa_action ;
	int 			fa_control_flags ;
} ;


void soft_reconfig() ;
void hard_reconfig() ;
void dump_internal_state() ;
void periodic_check() ;
void user_requested_check() ;
void child_exit() ;
void server_retry() ;
void quit_program() ;
void terminate_program() ;


static struct flag_action flag_actions[] =
{
	{ SOFT_RECONFIG_FLAG,		soft_reconfig,				CF_NOENV		},
	{ HARD_RECONFIG_FLAG,		hard_reconfig,				CF_NOENV		},
	{ RETRY_FLAG,					server_retry,				CF_NONE		},
	{ DUMP_FLAG,					dump_internal_state,		CF_NONE	 	},
	{ TERMINATE_FLAG,				terminate_program,		CF_NONE		},
	{ CONSISTENCY_FLAG,			user_requested_check,	CF_NONE		},
	{ CHILD_FLAG,					child_exit,					CF_NONE		},
	{ QUIT_FLAG,					quit_program,				CF_NONE		},
	{ PERIODIC_CHECK_FLAG,		periodic_check,			CF_NONE		},
	{ 0,								0,								0				}
} ;


/*
 * Returns after all flags are clear
 */
void check_flags()
{
	bool_int valid_env = ps.rws.env_is_valid ;

	while ( ! M_ARE_ALL_CLEAR( ps.flags ) )
	{
		register struct flag_action *fap ;

		for ( fap = flag_actions ; fap->fa_value != 0 ; fap++ )
			if ( M_IS_SET( ps.flags, fap->fa_value ) )
			{
				M_CLEAR( ps.flags, fap->fa_value ) ;

				if ( fap->fa_control_flags & CF_NOENV )
					ps.rws.env_is_valid = FALSE ;
				else
					ps.rws.env_is_valid = valid_env ;

				(*fap->fa_action)() ;

				break ;
			}
	}

	ps.rws.env_is_valid = valid_env ;
}


