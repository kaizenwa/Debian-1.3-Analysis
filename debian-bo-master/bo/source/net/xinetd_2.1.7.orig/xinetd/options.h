/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

extern int d_option ;
extern int f_option ;
extern char * f_option_arg_1 ;
extern int filelog_option ;
extern char * filelog_option_arg_1 ;
extern int syslog_option ;
extern char * syslog_option_arg_1 ;
extern int reuse_option ;
extern int limit_option ;
extern unsigned limit_option_arg_1 ;
extern int loop_option ;
extern unsigned loop_option_arg_1 ;
extern int pid_option ;
extern int logprocs_option ;
extern unsigned logprocs_option_arg_1 ;
extern int shutdownprocs_option ;
extern unsigned shutdownprocs_option_arg_1 ;
extern int cc_option ;
extern unsigned cc_option_arg_1 ;

#define f_option_arg                   f_option_arg_1
#define filelog_option_arg             filelog_option_arg_1
#define syslog_option_arg              syslog_option_arg_1
#define limit_option_arg               limit_option_arg_1
#define loop_option_arg                loop_option_arg_1
#define logprocs_option_arg            logprocs_option_arg_1
#define shutdownprocs_option_arg       shutdownprocs_option_arg_1
#define cc_option_arg                  cc_option_arg_1

extern char *program_name ;

void usage() ;

