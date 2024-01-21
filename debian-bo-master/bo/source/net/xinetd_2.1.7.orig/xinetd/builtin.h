/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

#ifndef BUILTIN_H
#define BUILTIN_H

/*
 * $Id: builtin.h,v 1.1 1996/01/24 19:29:19 chuck Exp $
 */

#include "defs.h"

#define FORK				YES
#define NO_FORK			NO

struct builtin
{
   voidfunc		b_handler ;					/* builtin service handler 			*/
   boolean_e	b_fork_server ;			/* whether a server must be forked	*/
} ;

typedef struct builtin builtin_s ;

/*
 * All builtins are invoked with a struct server argument
 */
#define builtin_invoke( bp, serp )		(*(bp)->b_handler)( serp )
#define builtin_forks( bp )				( (bp)->b_fork_server == YES )


struct builtin_service
{
   char			*bs_name ;					/* for identification purposes		*/
   int			bs_socket_type ;			/* for identification purposes		*/
	builtin_s	bs_handle ;
} ;

builtin_s *builtin_find() ;
builtin_s *builtin_lookup() ;

#endif	/* BUILTIN_H */

