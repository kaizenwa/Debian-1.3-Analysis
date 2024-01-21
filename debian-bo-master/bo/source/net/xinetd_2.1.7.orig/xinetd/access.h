/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

#ifndef ACCESS_H
#define ACCESS_H

/*
 * $Id: access.h,v 1.1 1996/01/24 19:29:19 chuck Exp $
 */

#include "defs.h"

/*
 * These flags are used to form a mask for access_control.
 * The mask determines which checks will be performed.
 */
#define CF_ADDRESS				1
#define CF_TIME					2
#define CF_SERVICE_LIMIT		3
#define CF_PROCESS_LIMIT		4

typedef enum
   {
      AC_OK,               /* ok to start a server                         */
      AC_FORK,             /* tried to start a server but fork failed      */
      AC_ADDRESS,          /* we do not accept requests from that address  */
      AC_TIME,             /* we do not accept requests at this time       */
      AC_SERVICE_LIMIT,    /* server limit would be exceeded for this      */
                           /* service                                      */
      AC_PROCESS_LIMIT     /* total process limit would be exceeded        */
   } access_e ;

extern struct name_value access_code_names[] ;

#define access_explain( code )	nv_get_name( access_code_names, (int) (code) )

access_e access_control() ;

#endif	/* ACCESS_H */
