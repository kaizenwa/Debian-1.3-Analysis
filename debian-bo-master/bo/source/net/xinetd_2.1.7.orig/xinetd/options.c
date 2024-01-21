/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

#include "options.h"

/*
 * $Id: options.c,v 1.1 1996/01/24 19:29:19 chuck Exp $
 */


#define NULL         0

void exit() ;

int d_option ;
int f_option ;
char * f_option_arg_1 ;
int filelog_option ;
char * filelog_option_arg_1 ;
int syslog_option ;
char * syslog_option_arg_1 ;
int reuse_option ;
int limit_option ;
unsigned limit_option_arg_1 ;
int loop_option ;
unsigned loop_option_arg_1 ;
int pid_option ;
int logprocs_option ;
unsigned logprocs_option_arg_1 ;
int shutdownprocs_option ;
unsigned shutdownprocs_option_arg_1 ;
int cc_option ;
unsigned cc_option_arg_1 ;

char *program_name ;

int opt_recognize( argc, argv )
	int argc ;
	char *argv[] ;
{
	int arg ;
	char *strrchr() ;

	program_name = strrchr( argv[ 0 ], '/' ) ;
	program_name = ( program_name == NULL ) ? argv[ 0 ] : program_name + 1 ;

	for ( arg = 1 ; arg < argc ; arg++ )
		if ( argv[ arg ][ 0 ] == '-' && argv[ arg ][ 1 ] != 0 )
		{
			if ( strcmp( &argv[ arg ][ 1 ], "d" ) == 0 ) 
				d_option = 1 ;
			else if ( strcmp( &argv[ arg ][ 1 ], "f" ) == 0 ) 
			{
				if ( ++arg == argc )
					usage() ;
				f_option_arg_1 = ( argv[ arg ] ) ;
				f_option = 1 ;
			}
			else if ( strcmp( &argv[ arg ][ 1 ], "filelog" ) == 0 ) 
			{
				if ( ++arg == argc )
					usage() ;
				filelog_option_arg_1 = ( argv[ arg ] ) ;
				filelog_option = 1 ;
			}
			else if ( strcmp( &argv[ arg ][ 1 ], "syslog" ) == 0 ) 
			{
				if ( ++arg == argc )
					usage() ;
				syslog_option_arg_1 = ( argv[ arg ] ) ;
				syslog_option = 1 ;
			}
			else if ( strcmp( &argv[ arg ][ 1 ], "reuse" ) == 0 ) 
				reuse_option = 1 ;
			else if ( strcmp( &argv[ arg ][ 1 ], "limit" ) == 0 ) 
			{
				int atoi() ;

				if ( ++arg == argc )
					usage() ;
				limit_option_arg_1 = atoi( argv[ arg ] ) ;
				limit_option = 1 ;
			}
			else if ( strcmp( &argv[ arg ][ 1 ], "loop" ) == 0 ) 
			{
				int atoi() ;

				if ( ++arg == argc )
					usage() ;
				loop_option_arg_1 = atoi( argv[ arg ] ) ;
				loop_option = 1 ;
			}
			else if ( strcmp( &argv[ arg ][ 1 ], "pid" ) == 0 ) 
				pid_option = 1 ;
			else if ( strcmp( &argv[ arg ][ 1 ], "logprocs" ) == 0 ) 
			{
				int atoi() ;

				if ( ++arg == argc )
					usage() ;
				logprocs_option_arg_1 = atoi( argv[ arg ] ) ;
				logprocs_option = 1 ;
			}
			else if ( strcmp( &argv[ arg ][ 1 ], "shutdownprocs" ) == 0 ) 
			{
				int atoi() ;

				if ( ++arg == argc )
					usage() ;
				shutdownprocs_option_arg_1 = atoi( argv[ arg ] ) ;
				shutdownprocs_option = 1 ;
			}
			else if ( strcmp( &argv[ arg ][ 1 ], "cc" ) == 0 ) 
			{
				int atoi() ;

				if ( ++arg == argc )
					usage() ;
				cc_option_arg_1 = atoi( argv[ arg ] ) ;
				cc_option = 1 ;
			}
		}
		else
			break ;

	if ( filelog_option + syslog_option > 1 )
		usage() ;


	if ( argc - arg != 0 )
		usage() ;
	return( arg ) ;
}

void usage()
{
	Sprint( 2, "Usage: %s [-d] [-f config_file] [-filelog filename] [-syslog facility] [-reuse] [-limit proc_limit] [-loop loop_rate] [-pid] [-logprocs limit] [-shutdownprocs limit] [-cc interval]\n", program_name ) ;
	exit( 1 ) ;
}
