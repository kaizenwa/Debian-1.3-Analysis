/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

#ifndef PARSE_H
#define PARSE_H

#include "defs.h"

/*
 * $Id: parse.h,v 1.1 1996/01/24 19:29:19 chuck Exp $
 */

typedef enum { NO_ENTRY, BAD_ENTRY, SERVICE_ENTRY, DEFAULTS_ENTRY } entry_e ;

enum assign_op { SET_EQ, PLUS_EQ, MINUS_EQ } ;

struct attribute
{
   char		*a_name ;            /* name of attribute							*/
   unsigned a_id ;               /* attribute id								*/
   int		a_nvalues ;				/* number of values							*/
   status_e (*a_parser)() ;		/* function that parses the attribute	*/
} ;


#define ENTRY_BEGIN              '{'
#define ENTRY_END                '}'
#define COMMENT_BEGIN            '#'
#define KW_SERVICE					"service"
#define KW_DEFAULTS					"defaults"

extern int line_count ;

#endif	/* PARSE_H */

