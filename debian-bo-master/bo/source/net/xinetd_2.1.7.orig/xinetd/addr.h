/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

#ifndef ADDR_H
#define ADDR_H

/*
 * $Id: addr.h,v 1.1 1996/01/24 19:29:19 chuck Exp $
 */

#include "defs.h"

bool_int		addrlist_match() ;
void			addrlist_dump() ;
void			addrlist_free() ;
status_e		addrlist_add() ;
status_e		addrlist_remove() ;
status_e		addrlist_copy() ;

#endif	/* ADDR_H */
