/*
** compat_misc.c              COMPAT support routines
**
** Copyright (c) 1993 Signum Support AB, Sweden
**
** This file is part of the NYS Library.
**
** The NYS Library is free software; you can redistribute it and/or
** modify it under the terms of the GNU Library General Public License as
** published by the Free Software Foundation; either version 2 of the
** License, or (at your option) any later version.
**
** The NYS Library is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
** Library General Public License for more details.
** 
** You should have received a copy of the GNU Library General Public
** License along with the NYS Library; see the file COPYING.LIB.  If
** not, write to the Free Software Foundation, Inc., 675 Mass Ave,
** Cambridge, MA 02139, USA.
**
** Author: Peter Eriksson <pen@signum.se>
*/

#include "config.h"

#ifdef ENABLE_COMPAT


#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <rpcsvc/ypclnt.h>
#include <rpcsvc/yp_prot.h>

extern int
__yp_check (char **dom)
{
  static char *__yp_domain = NULL;
  
  if (NULL == __yp_domain)
      __yp_domain = calloc(1, YPMAXDOMAIN+1);
  
  if ('\0' == __yp_domain[0])
    if (yp_get_default_domain (&__yp_domain))
      return 0;
#ifdef __linux__
  /* If you didn't specify the domain name while building the Linux
   * kernel, your domain name will be set to "(none)".
   */
    else
      if (strcmp(__yp_domain,"(none)") == 0)
	return 0;
#endif
  
  if (yp_bind (__yp_domain) == 0)
    return 1;
  return 0;
}
#endif /* ENABLE_COMPAT */

